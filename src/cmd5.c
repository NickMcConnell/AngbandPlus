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
static int get_spell(int *sn, cptr prompt, int sval, bool known, bool realm_2)
{
	int i;
	int spell;
	int num = 0;
	int ask;
	byte spells[PY_MAX_SPELLS];
	bool flag, okay;
	char choice;
	const magic_type *s_ptr;
	char out_val[160];
	int use_realm = p_ptr->spell.r[realm_2 ? 1 : 0].realm;
	cptr p = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell");

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (spell_okay(*sn, known, use_realm - 1))
		{
			/* Success */
			return (TRUE);
		}
		else
		{
			/* Invalid repeat - reset it */
			repeat_clear();
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
		if (spell_okay(spells[i], known, use_realm - 1)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* Save the screen */
	screen_save();

	/* Display a list of spells */
	print_spells(spells, num, 20, 1, use_realm - 1);

	/* Show choices */
	/* Update */
	p_ptr->window |= (PW_SPELL);

	/* Window stuff */
	window_stuff();

	/* Build a prompt (accept all spells) */
	(void)strnfmt(out_val, 78, "(%^ss, ESC=exit) %^s which %s? ", p, prompt, p);

	/* Get a spell from the user */
	while (get_com(out_val, &choice))
	{
		/* Note verify */
		ask = (isupper(choice));

		/* Lowercase */
		if (ask) choice = tolower(choice);

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
		if (!spell_okay(spell, known, use_realm - 1))
		{
			bell("Illegal spell choice!");
			msgf("You may not %s that %s.", prompt, p);
			continue;
		}

		/* Verify it */
		if (ask)
		{
			/* Access the spell */
			s_ptr = &mp_ptr->info[use_realm - 1][spell % 32];

			/* Belay that order */
			if (!get_check("%^s %s (%d mana, %d%% fail)? ",
						  prompt, spell_names[use_realm - 1][spell % 32],
						  spell_mana(spell, use_realm - 1), 
						  spell_chance(spell, use_realm - 1))) continue;
		}

		/* Stop the loop */
		flag = TRUE;
		break;
	}


	/* Restore the screen */
	screen_load();


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

	byte spells[PY_MAX_SPELLS];


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
	print_spells(spells, num, 20, 1, (o_ptr->tval - TV_BOOKS_MIN));

	/* Clear the top line */
	clear_msg();

	/* Prompt user */
    pause_line(0);

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
	object_type *o_ptr;
	cptr q, s;

	/* Warriors are illiterate */
	if (!(p_ptr->spell.r[0].realm || p_ptr->spell.r[1].realm))
	{
		msgf("You cannot read books!");
		return;
	}

	/* Restrict choices to books */
	item_tester_hook = item_tester_hook_is_book;
	
	/* Get an item */
	q = "Browse which book? ";
	s = "You have no books that you can read.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Print out the spells */
	do_cmd_browse_aux(o_ptr);
}


/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int i, sval;
	int increment = 0;

	/* Spells of r[1].realm will have an increment of +32 */
	int spell = -1;

	cptr p = ((mp_ptr->spell_book == TV_SORCERY_BOOK) ? "spell" : "prayer");

	object_type *o_ptr;

	cptr q, s;

	if (!p_ptr->spell.r[0].realm)
	{
		msgf("You cannot read books!");
		return;
	}

	if (p_ptr->tim.blind || no_lite())
	{
		msgf("You cannot see!");
		return;
	}

	if (p_ptr->tim.confused)
	{
		msgf("You are too confused!");
		return;
	}

	if (!(p_ptr->new_spells))
	{
		msgf("You cannot learn any new %ss!", p);
		return;
	}

	msgf("You can learn %d new %s%s.", p_ptr->new_spells, p,
			   (p_ptr->new_spells == 1 ? "" : "s"));
	message_flush();

	/* Restrict choices to "useful" books */
	item_tester_tval = mp_ptr->spell_book;

	/* Get an item */
	q = "Study which book? ";
	s = "You have no books that you can read.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Access the item's sval */
	sval = o_ptr->sval;

	if (o_ptr->tval == REALM2_BOOK) increment = 32;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Mage -- Learn a selected spell */
	if (mp_ptr->spell_book != TV_LIFE_BOOK)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", sval, FALSE,
					   (bool)(increment ? TRUE : FALSE))
			&& (spell == -1)) return;
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
								p_ptr->spell.r[increment / 32].realm - 1))
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
		msgf("You cannot learn any %ss in that book.", p);

		/* Abort */
		return;
	}


	/* Take a turn */
	p_ptr->state.energy_use = 100;

	if (increment) spell += increment;

	/* Learn the spell */
	p_ptr->spell.r[spell / 32].learned |= (1L << (spell % 32));

	/* Find the next open entry in "spell.order[]" */
	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
		/* Stop at the first empty space */
		if (p_ptr->spell.order[i] == 99) break;
	}

	/* Add the spell to the known list */
	p_ptr->spell.order[i++] = spell;

	/* Mention the result */
	msgf(MSGT_STUDY, "You have learned the %s of %s.",
				   p, spell_names
				   [p_ptr->spell.r[increment / 32].realm - 1][spell % 32]);

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
		msgf("You can learn %d more %s%s.",
				   p_ptr->new_spells, p, (p_ptr->new_spells != 1) ? "s" : "");
	}

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);

	/* Talking to yourself... */
	make_noise(1);
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
			(void)destroy_doors_touch();
			break;
		}
		case 16:  case 17:
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
			(void)earthquake(px, py, 5);
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
				(void)summon_specific(0, px, py, (p_ptr->depth * 3) / 2, type,
									  TRUE, FALSE, FALSE);
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
			(void)summon_cyber(-1, px, py);
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


static bool cast_life_spell(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int dir;
	int plev = p_ptr->lev;

	switch (spell)
	{
		case 0:				/* Detect Evil */
			(void)detect_monsters_evil();
			break;
		case 1:				/* Cure Light Wounds */
			(void)hp_player(damroll(2, 10));
			(void)inc_cut(-10);
			break;
		case 2:				/* Bless */
			(void)inc_blessed(rand_range(12, 24));
			break;
		case 3:				/* Remove Fear */
			(void)clear_afraid();
			break;
		case 4:				/* Call Light */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
		case 5:				/* Detect Traps + Secret Doors */
			(void)detect_traps(TRUE);
			(void)detect_doors();
			(void)detect_stairs();
			break;
		case 6:				/* Cure Medium Wounds */
			(void)hp_player(damroll(4, 10));
			(void)inc_cut(-40);
			break;
		case 7:				/* Satisfy Hunger */
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		case 8:				/* Remove Curse */
			(void)remove_curse();
			break;
		case 9:				/* Cure Poison */
			(void)clear_poisoned();
			break;
		case 10:				/* Cure Critical Wounds */
			(void)hp_player(damroll(8, 10));
			(void)clear_stun();
			(void)clear_cut();
			break;
		case 11:				/* Sense Unseen */
			(void)inc_tim_invis(rand_range(24, 48));
			break;
		case 12:				/* Orb or Draining */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_HOLY_FIRE, dir,
							(damroll(3, 6) + plev +
							 (plev / ((p_ptr->rp.pclass == CLASS_PRIEST ||
									   p_ptr->rp.pclass ==
									   CLASS_HIGH_MAGE) ? 2 : 4))),
							((plev < 30) ? 2 : 3));

			break;
		case 13:				/* Protection from Evil */
			(void)inc_protevil(randint1(25) + 3 * p_ptr->lev);
			break;
		case 14:				/* Healing */
			(void)hp_player(300);
			(void)clear_stun();
			(void)clear_cut();
			break;
		case 15:				/* Glyph of Warding */
			(void)warding_glyph();
			break;
		case 16:				/* Exorcism */
			(void)dispel_undead(plev);
			(void)dispel_demons(plev);
			(void)turn_evil(plev);
			break;
		case 17:				/* Dispel Curse */
			(void)remove_all_curse();
			break;
		case 18:				/* Dispel Undead + Demons */
			(void)dispel_undead(plev * 3);
			(void)dispel_demons(plev * 3);
			break;
		case 19:				/* 'Day of the Dove' */
			(void)charm_monsters(plev * 2);
			break;
		case 20:				/* Dispel Evil */
			(void)dispel_evil(plev * 4);
			break;
		case 21:				/* Banishment */
			if (banish_evil(100))
			{
				msgf("The power of your god banishes evil!");
			}
			break;
		case 22:				/* Holy Word */
			(void)dispel_evil(plev * 4);
			(void)hp_player(1000);
			(void)clear_afraid();
			(void)clear_poisoned();
			(void)clear_stun();
			(void)clear_cut();
			break;
		case 23:				/* Warding True */
			(void)warding_glyph();
			(void)glyph_creation();
			break;
		case 24:				/* Heroism */
			(void)inc_hero(rand_range(25, 50));
			(void)hp_player(10);
			(void)clear_afraid();
			break;
		case 25:				/* Prayer */
			(void)inc_blessed(rand_range(50, 100));
			break;
		case 26:
			return bless_weapon();
		case 27:				/* Restoration */
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_CHR);
			(void)restore_level();
			break;
		case 28:				/* Healing True */
			(void)hp_player(2000);
			(void)clear_stun();
			(void)clear_cut();
			break;
		case 29:				/* Holy Vision */
			return identify_fully();
		case 30:				/* Divine Intervention */
			(void)project(0, 1, px, py, 777, GF_HOLY_FIRE, PROJECT_KILL);
			(void)dispel_monsters(plev * 4);
			(void)slow_monsters();
			(void)stun_monsters(plev * 4);
			(void)confuse_monsters(plev * 4);
			(void)turn_monsters(plev * 4);
			(void)stasis_monsters(plev * 4);
			(void)summon_specific(-1, px, py, plev, SUMMON_ANGEL, TRUE, TRUE,
								  TRUE);
			(void)inc_shero(rand_range(25, 50));
			(void)hp_player(300);

			/* Haste */
			(void)inc_fast(randint1(20 + plev) + plev);

			(void)clear_afraid();
			break;
		case 31:				/* Holy Invulnerability */
			(void)inc_invuln(rand_range(7, 14));
			break;
		default:
			msgf("You cast an unknown Life spell: %d.", spell);
			message_flush();
	}

	make_noise(2);

	return TRUE;
}



static bool cast_sorcery_spell(int spell)
{
	int dir;
	int plev = p_ptr->lev;

	switch (spell)
	{
		case 0:				/* Detect Monsters */
			(void)detect_monsters_normal();
			break;
		case 1:				/* Phase Door */
			teleport_player(10);
			break;
		case 2:				/* Detect Doors and Traps */
			(void)detect_traps(TRUE);
			(void)detect_doors();
			(void)detect_stairs();
			break;
		case 3:				/* Light Area */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
		case 4:				/* Confuse Monster */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)confuse_monster(dir, (plev * 3) / 2);
			break;
		case 5:				/* Teleport Self */
			teleport_player(plev * 5);
			break;
		case 6:				/* Sleep Monster */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)sleep_monster(dir);
			break;
		case 7:				/* Recharging */
			return recharge(plev * 4);
		case 8:				/* Magic Mapping */
			map_area();
			break;
		case 9:				/* Identify */
			return ident_spell();
		case 10:				/* Slow Monster */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)slow_monster(dir);
			break;
		case 11:				/* Mass Sleep */
			(void)sleep_monsters();
			break;
		case 12:				/* Teleport Away */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_beam(GF_AWAY_ALL, dir, plev);
			break;
		case 13:				/* Haste Self */
			(void)inc_fast(randint1(20 + plev) + plev);
			break;
		case 14:				/* Detection True */
			(void)detect_all();
			break;
		case 15:				/* Identify True */
			return identify_fully();
		case 16:				/* Detect Objects and Treasure */
			(void)detect_objects_normal();
			(void)detect_treasure();
			(void)detect_objects_gold();
			break;
		case 17:				/* Detect Enchantment */
			(void)detect_objects_magic();
			break;
		case 18:				/* Charm Monster */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)charm_monster(dir, plev);
			break;
		case 19:				/* Dimension Door */
			msgf("You open a dimensional gate. Choose a destination.");
			return dimension_door();
		case 20:				/* Sense Minds */
			(void)inc_tim_esp(rand_range(25, 55));
			break;
		case 21:				/* Self knowledge */
			(void)self_knowledge();
			break;
		case 22:				/* Teleport Level */
			(void)teleport_player_level();
			break;
		case 23:				/* Word of Recall */
			word_of_recall();
			break;
		case 24:				/* Stasis */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)stasis_monster(dir);
			break;
		case 25:				/* Telekinesis */
			if (!get_aim_dir(&dir)) return FALSE;

			fetch(dir, plev * 15, FALSE);
			break;
		case 26:				/* Explosive Rune */
			(void)explosive_rune();
			break;
		case 27:				/* Clairvoyance */
			wiz_lite();
			if (!(FLAG(p_ptr, TR_TELEPATHY)))
			{
				(void)inc_tim_esp(rand_range(25, 55));
			}
			break;
		case 28:				/* Enchant Weapon */
			return enchant_spell(randint1(4), randint1(4), 0);
		case 29:				/* Enchant Armour */
			return enchant_spell(0, 0, rand_range(2, 5));
		case 30:				/* Alchemy */
			return alchemy();
		case 31:				/* Globe of Invulnerability */
			(void)inc_invuln(rand_range(8, 16));
			break;
		default:
			msgf("You cast an unknown Sorcery spell: %d.", spell);
			message_flush();
	}

	make_noise(2);

	return TRUE;
}


static bool cast_nature_spell(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int dir;
	int beam;
	int plev = p_ptr->lev;
	bool no_trump = FALSE;

	if (p_ptr->rp.pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->rp.pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else
		beam = plev / 2;

	switch (spell)
	{
		case 0:				/* Detect Creatures */
			(void)detect_monsters_normal();
			break;
		case 1:				/* First Aid */
			(void)hp_player(damroll(2, 8));
			(void)inc_cut(-15);
			break;
		case 2:				/* Detect Doors + Traps */
			(void)detect_traps(TRUE);
			(void)detect_doors();
			(void)detect_stairs();
			break;
		case 3:				/* Produce Food */
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		case 4:				/* Daylight */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			if ((FLAG(p_ptr, TR_HURT_LITE)) &&
				!(FLAG(p_ptr, TR_RES_LITE)) &&
				!(FLAG(p_ptr, TR_IM_LITE)))
			{
				msgf("The daylight scorches your flesh!");
				take_hit(damroll(2, 2), "daylight");
			}
			break;
		case 5:				/* Animal Taming */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)charm_animal(dir, plev);
			break;
		case 6:				/* Resist Environment */
			(void)inc_oppose_cold(rand_range(20, 40));
			(void)inc_oppose_fire(rand_range(20, 40));
			(void)inc_oppose_elec(rand_range(20, 40));
			break;
		case 7:				/* Cure Wounds + Poison */
			(void)clear_cut();
			(void)clear_poisoned();
			break;
		case 8:				/* Stone to Mud */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)wall_to_mud(dir);
			break;
		case 9:				/* Lightning Bolt */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
									damroll(3 + ((plev - 5) / 4), 8));
			break;
		case 10:				/* Nature Awareness -- downgraded */
			map_area();
			(void)detect_traps(TRUE);
			(void)detect_doors();
			(void)detect_stairs();
			(void)detect_monsters_normal();
			break;
		case 11:				/* Frost Bolt */
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt_or_beam(beam - 10, GF_COLD, dir,
									damroll(5 + ((plev - 5) / 4), 8));
			break;
		case 12:				/* Ray of Sunlight */
			if (!get_aim_dir(&dir)) return FALSE;
			msgf("A line of sunlight appears.");
			(void)lite_line(dir, damroll(6, 8));
			break;
		case 13:				/* Entangle */
			(void)slow_monsters();
			break;
		case 14:				/* Summon Animals */
			if (!
				(summon_specific
				 (-1, px, py, plev, SUMMON_ANIMAL_RANGER, TRUE, TRUE, TRUE)))
				no_trump = TRUE;
			break;
		case 15:				/* Herbal Healing */
			(void)hp_player(1000);
			(void)clear_stun();
			(void)clear_cut();
			(void)clear_poisoned();
			break;
		case 16:				/* Door Building */
			(void)door_creation();
			break;
		case 17:				/* Stair Building */
			(void)stair_creation();
			break;
		case 18:				/* Stone Skin */
			(void)inc_shield(rand_range(30, 50));
			break;
		case 19:				/* Resistance True */
			(void)inc_oppose_acid(rand_range(20, 40));
			(void)inc_oppose_elec(rand_range(20, 40));
			(void)inc_oppose_fire(rand_range(20, 40));
			(void)inc_oppose_cold(rand_range(20, 40));
			(void)inc_oppose_pois(rand_range(20, 40));
			break;
		case 20:				/* Animal Friendship */
			(void)charm_animals(plev * 2);
			break;
		case 21:				/* Stone Tell */
			return identify_fully();
		case 22:				/* Wall of Stone */
			(void)wall_stone();
			break;
		case 23:				/* Protection from Corrosion */
			return rustproof();
		case 24:				/* Earthquake */
			(void)earthquake(px, py, 10);
			break;
		case 25:				/* Whirlwind Attack */
			whirlwind_attack();
			break;
		case 26:				/* Blizzard */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_COLD, dir, 70 + plev, (plev / 12) + 1);
			break;
		case 27:				/* Lightning Storm */
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_ELEC, dir, 90 + plev, (plev / 12) + 1);
			break;
		case 28:				/* Whirlpool */
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_WATER, dir, 100 + plev, (plev / 12) + 1);
			break;
		case 29:				/* Call Sunlight */
			(void)fire_ball(GF_LITE, 0, 150, 8);
			wiz_lite();
			if ((FLAG(p_ptr, TR_HURT_LITE)) &&
				!(FLAG(p_ptr, TR_RES_LITE)) &&
				!(FLAG(p_ptr, TR_IM_LITE)))
			{
				msgf("The sunlight scorches your flesh!");
				take_hit(50, "sunlight");
			}
			break;
		case 30:				/* Elemental Brand */
			brand_weapon(0);
			break;
		case 31:				/* Nature's Wrath */
			(void)dispel_monsters(plev * 4);
			(void)earthquake(px, py, 20 + (plev / 2));
			(void)project(0, 1 + plev / 12, px, py,
						  100 + plev, GF_DISINTEGRATE,
						  PROJECT_KILL | PROJECT_ITEM);
			break;
		default:
			msgf("You cast an unknown Nature spell: %d.", spell);
			message_flush();
	}

	if (no_trump)
		msgf("No animals arrive.");

	make_noise(2);

	return TRUE;
}


static bool cast_chaos_spell(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int dir, i, beam;
	int plev = p_ptr->lev;

	if (p_ptr->rp.pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->rp.pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else
		beam = plev / 2;

	switch (spell)
	{
		case 0:				/* Magic Missile */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
									damroll(3 + ((plev - 1) / 5), 4));
			break;
		case 1:				/* Trap / Door destruction */
			(void)destroy_doors_touch();
			break;
		case 2:				/* Flash of Light == Light Area */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
		case 3:				/* Touch of Confusion */
			if (!p_ptr->state.confusing)
			{
				msgf("Your hands start glowing.");
				p_ptr->state.confusing = TRUE;
				p_ptr->redraw |= (PR_STATUS);
			}
			break;
		case 4:				/* Manaburst */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_MISSILE, dir,
							(damroll(3, 5) + plev +
							 (plev / (((p_ptr->rp.pclass == CLASS_MAGE) ||
									   (p_ptr->rp.pclass ==
										CLASS_HIGH_MAGE)) ? 2 : 4))),
							((plev < 30) ? 2 : 3));
			/* Shouldn't actually use GF_MANA, as it will destroy all
			 * items on the floor */
			break;
		case 5:				/* Fire Bolt */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_bolt_or_beam(beam, GF_FIRE, dir,
									damroll(8 + ((plev - 5) / 4), 8));
			break;
		case 6:				/* Fist of Force ("Fist of Fun") */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_DISINTEGRATE, dir,
							damroll(8 + ((plev - 5) / 4), 8), 0);
			break;
		case 7:				/* Teleport Self */
			teleport_player(plev * 5);
			break;
		case 8:				/* Wonder */
		{
			/*
			 * This spell should become more useful (more
			 * controlled) as the player gains experience levels.
			 * Thus, add 1/5 of the player's level to the die roll.
			 * This eliminates the worst effects later on, while
			 * keeping the results quite random.  It also allows
			 * some potent effects only at high level.
			 */
			int die = randint1(100) + plev / 5;

			if (die < 26)
				chg_virtue(V_CHANCE, 1);

			if (!get_aim_dir(&dir)) return FALSE;
			if (die > 100)
				msgf("You feel a surge of power!");
			if (die < 8) (void)clone_monster(dir);
			else if (die < 14) (void)speed_monster(dir);
			else if (die < 26) (void)heal_monster(dir);
			else if (die < 31) (void)poly_monster(dir);
			else if (die < 36)
				(void)fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
										damroll(3 + ((plev - 1) / 5), 4));
			else if (die < 41) (void)confuse_monster(dir, plev);
			else if (die < 46) (void)fire_ball(GF_POIS, dir, 20 + (plev / 2),
											   3);
			else if (die < 51) (void)lite_line(dir, damroll(6, 8));
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
			else if (die < 81) (void)fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
			else if (die < 86) (void)fire_ball(GF_ACID, dir, 40 + plev, 2);
			else if (die < 91) (void)fire_ball(GF_ICE, dir, 70 + plev, 3);
			else if (die < 96) (void)fire_ball(GF_FIRE, dir, 80 + plev, 3);
			else if (die < 101) (void)drain_life(dir, 100 + plev);
			else if (die < 104)
			{
				(void)earthquake(px, py, 12);
			}
			else if (die < 106)
			{
				(void)destroy_area(px, py, 15);
			}
			else if (die < 108)
			{
				(void)genocide(TRUE);
			}
			else if (die < 110) (void)dispel_monsters(120);
			else				/* RARE */
			{
				(void)dispel_monsters(150);
				(void)slow_monsters();
				(void)sleep_monsters();
				(void)hp_player(300);
			}
			break;
		}
		case 9:				/* Chaos Bolt */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_bolt_or_beam(beam, GF_CHAOS, dir,
									damroll(10 + ((plev - 5) / 4), 8));
			break;
		case 10:				/* Sonic Boom */
			msgf("BOOM! Shake the room!");
			(void)project(0, plev / 10 + 2, px, py,
						  45 + plev, GF_SOUND, PROJECT_KILL | PROJECT_ITEM);
			break;
		case 11:				/* Doom Bolt -- always beam in 2.0.7 or later */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_beam(GF_MANA, dir, damroll(11 + ((plev - 5) / 4), 8));
			break;
		case 12:				/* Fire Ball */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_FIRE, dir, plev + 55, 2);
			break;
		case 13:				/* Teleport Other */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_beam(GF_AWAY_ALL, dir, plev);
			break;
		case 14:				/* Word of Destruction */
			(void)destroy_area(px, py, 15);
			break;
		case 15:				/* Invoke Logrus */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_CHAOS, dir, plev + 66, plev / 5);
			break;
		case 16:				/* Polymorph Other */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)poly_monster(dir);
			break;
		case 17:				/* Chain Lightning */
			for (dir = 0; dir <= 9; dir++)
				(void)fire_beam(GF_ELEC, dir, damroll(5 + (plev / 10), 8));
			break;
		case 18:				/* Arcane Binding == Charging */
			return recharge(90);
		case 19:				/* Disintegration */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_DISINTEGRATE, dir, plev + 80, 3 + plev / 40);
			break;
		case 20:				/* Alter Reality */
			alter_reality();
			break;
		case 21:				/* Polymorph Self */
			do_poly_self();
			break;
		case 22:				/* Chaos Branding */
			brand_weapon(1);
			break;
		case 23:				/* Summon monster, demon */
		{
			bool pet = (one_in_(3));
			bool group = !(pet && (plev < 50));

			if (summon_specific
				((pet ? -1 : 0), px, py, (plev * 3) / 2, SUMMON_DEMON, group,
				 FALSE, pet))
			{
				msgf
					("The area fills with a stench of sulphur and brimstone.");

				if (pet)
					msgf("'What is thy bidding... Master?'");
				else
					msgf
						("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
			}
			break;
		}
		case 24:				/* Beam of Gravity */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_beam(GF_GRAVITY, dir, damroll(9 + ((plev - 5) / 4), 8));
			break;
		case 25:				/* Meteor Swarm */
		{
			int x, y;
			int b = rand_range(10, 20);
			for (i = 0; i < b; i++)
			{
				int count = 0;

				while (count < 1000)
				{
					count++;

					x = px - 5 + randint1(10);
					y = py - 5 + randint1(10);

					/* paranoia */
					if (!in_boundsp(x, y)) continue;

					/* keep going if not in LOS */
					if (!player_has_los_grid(parea(x, y))) continue;

					/* if close enough - exit */
					if (distance(px, py, x, y) < 6) break;
				}

				if (count >= 1000) break;

				(void)project(0, 2, x, y, (plev * 3) / 2, GF_METEOR,
							  PROJECT_KILL | PROJECT_JUMP | PROJECT_ITEM);
			}
		}
			break;
		case 26:				/* Flame Strike */
			(void)fire_ball(GF_FIRE, 0, 150 + (2 * plev), 8);
			break;
		case 27:				/* Call Chaos */
			call_chaos();
			break;
		case 28:				/* Magic Rocket */
			if (!get_aim_dir(&dir)) return FALSE;

			msgf("You launch a rocket!");
			(void)fire_ball(GF_ROCKET, dir, 120 + plev, 2);
			break;
		case 29:				/* Mana Storm */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_MANA, dir, 300 + (plev * 2), 4);
			break;
		case 30:				/* Breathe Logrus */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_CHAOS, dir, p_ptr->chp, 2);
			break;
		case 31:				/* Call the Void */
			call_the_();
			break;
		default:
			msgf("You cast an unknown Chaos spell: %d.", spell);
			message_flush();
	}

	make_noise(2);

	return TRUE;
}


static bool cast_death_spell(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int dir;
	int beam;
	int plev = p_ptr->lev;
	int dummy = 0;
	int i;

	if (p_ptr->rp.pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->rp.pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else
		beam = plev / 2;

	switch (spell)
	{
		case 0:				/* Detect Undead & Demons -> Unlife */
			(void)detect_monsters_nonliving();
			break;
		case 1:				/* Malediction */
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
		case 2:				/* Detect Evil */
			(void)detect_monsters_evil();
			break;
		case 3:				/* Stinking Cloud */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_POIS, dir, 10 + (plev / 2), 2);
			break;
		case 4:				/* Black Sleep */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)sleep_monster(dir);
			break;
		case 5:				/* Resist Poison */
			(void)inc_oppose_pois(rand_range(20, 40));
			break;
		case 6:				/* Horrify */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fear_monster(dir, plev);
			(void)stun_monster(dir, plev);
			break;
		case 7:				/* Enslave the Undead */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)control_one_undead(dir, plev);
			break;
		case 8:				/* Orb of Entropy */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_OLD_DRAIN, dir,
							(damroll(3, 6) + plev +
							 (plev / (((p_ptr->rp.pclass == CLASS_MAGE) ||
									   (p_ptr->rp.pclass ==
										CLASS_HIGH_MAGE)) ? 2 : 4))),
							((plev < 30) ? 2 : 3));
			break;
		case 9:				/* Nether Bolt */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_bolt_or_beam(beam, GF_NETHER, dir,
									damroll(6 + ((plev - 5) / 4), 8));
			break;
		case 10:				/* Terror */
			(void)turn_monsters(30 + plev);
			break;
		case 11:				/* Vampiric Drain */
			if (!get_aim_dir(&dir)) return FALSE;

			dummy = plev + randint1(plev) * MAX(1, plev / 10);	/* Dmg */
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
				if (p_ptr->food < PY_FOOD_MAX)	/* Not gorged already */
					(void)set_food(dummy >=
								   PY_FOOD_MAX ? PY_FOOD_MAX - 1 : dummy);
			}
			break;
		case 12:				/* Poison Branding */
			brand_weapon(2);
			break;
		case 13:				/* Dispel Good */
			(void)dispel_good(plev * 4);
			break;
		case 14:				/* Genocide */
			(void)genocide(TRUE);
			break;
		case 15:				/* Restore Life */
			(void)restore_level();
			break;
		case 16:				/* Berserk */
			(void)inc_shero(rand_range(25, 50));
			(void)hp_player(30);
			(void)clear_afraid();
			break;
		case 17:				/* Invoke Spirits */
		{
			int die = randint1(100) + plev / 5;
			if (!get_aim_dir(&dir)) return FALSE;

			msgf("You call on the power of the dead...");

			if (die < 26)
				chg_virtue(V_CHANCE, 1);

			if (die > 100)
				msgf("You feel a surge of eldritch force!");

			if (die < 8)
			{
				msgf
					("Oh no! Mouldering forms rise from the earth around you!");
				(void)summon_specific(0, px, py, p_ptr->depth, SUMMON_UNDEAD,
									  TRUE, FALSE, FALSE);

				chg_virtue(V_UNLIFE, 1);
			}
			else if (die < 14)
			{
				msgf("An unnamable evil brushes against your mind...");
				(void)inc_afraid(rand_range(4, 8));
			}
			else if (die < 26)
			{
				msgf
					("Your head is invaded by a horde of gibbering spectral voices...");
				(void)inc_confused(rand_range(4, 8));
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
				(void)confuse_monster(dir, plev);
			}
			else if (die < 46)
			{
				(void)fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
			}
			else if (die < 51)
			{
				(void)lite_line(dir, damroll(6, 8));
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
				(void)earthquake(px, py, 12);
			}
			else if (die < 106)
			{
				(void)destroy_area(px, py, 15);
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
			{					/* RARE */
				(void)dispel_monsters(150);
				(void)slow_monsters();
				(void)sleep_monsters();
				(void)hp_player(300);
			}

			if (die < 31)
				msgf
					("Sepulchral voices chuckle. 'Soon you will join us, mortal.'");
			break;
		}
		case 18:				/* Dark Bolt */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_bolt_or_beam(beam, GF_DARK, dir,
									damroll(4 + ((plev - 5) / 4), 8));
			break;
		case 19:				/* Battle Frenzy */
			(void)inc_shero(rand_range(25, 50));
			(void)hp_player(30);
			(void)clear_afraid();
			(void)inc_fast(rand_range(plev / 2, 20 + plev));
			break;
		case 20:				/* Vampirism True */
			if (!get_aim_dir(&dir)) return FALSE;

			chg_virtue(V_SACRIFICE, -1);
			chg_virtue(V_VITALITY, -1);

			for (dummy = 0; dummy < 3; dummy++)
			{
				(void)drain_gain_life(dir, 100);
			}
			break;
		case 21:				/* Vampiric Branding */
			brand_weapon(3);
			break;
		case 22:				/* Darkness Storm */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_DARK, dir, 120, 4);
			break;
		case 23:				/* Mass Genocide */
			(void)mass_genocide(TRUE);
			break;
		case 24:				/* Death Ray */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)death_ray(dir, plev);
			break;
		case 25:				/* Raise the Dead */
		{
			if (raise_dead(px, py, (bool)(!one_in_(3))))
			{
				msgf
					("Cold winds begin to blow around you, carrying with them the stench of decay...");
				chg_virtue(V_UNLIFE, 1);
			}
			else
			{
				msgf("Nothing happens.");
			}
			break;
		}
		case 26:				/* Esoteria */
			if (randint1(50) > plev)
				return ident_spell();
			else
				return identify_fully();
			break;
		case 27:				/* Word of Death */
			(void)dispel_living(plev * 3);
			break;
		case 28:				/* Evocation */
			(void)dispel_monsters(plev * 4);
			(void)turn_monsters(plev * 4);
			(void)banish_monsters(plev * 4);
			break;
		case 29:				/* Hellfire */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_HELL_FIRE, dir, 666, 3);
			take_hit(rand_range(50, 100), "the strain of casting Hellfire");
			break;
		case 30:				/* Omnicide */
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
				if (FLAG(r_ptr, RF_UNIQUE)) continue;

				/* Hack -- Skip Quest Monsters */
				if (FLAG(r_ptr, RF_QUESTOR)) continue;

				/* Notice changes in view */
				if (FLAG(r_ptr, RF_LITE_1) || FLAG(r_ptr, RF_LITE_2))
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
				move_cursor_relative(px, py);

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
		case 31:				/* Wraithform */
			(void)inc_wraith_form(rand_range(plev / 2, plev));
			break;
		default:
			msgf("You cast an unknown Death spell: %d.", spell);
			message_flush();
	}

	make_noise(2);

	return TRUE;
}


static bool cast_trump_spell(int spell, bool success)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int dir;
	int beam;
	int plev = p_ptr->lev;
	int dummy = 0;
	bool no_trump = FALSE;
	char tmp_val[160];


	if (p_ptr->rp.pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->rp.pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else
		beam = plev / 2;

	switch (spell)
	{
		case 0:				/* Phase Door */
			if (success)
			{
				teleport_player(10);
			}
			break;
		case 1:				/* Mind Blast */
			if (success)
			{
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt_or_beam(beam - 10, GF_PSI, dir,
										damroll(3 + ((plev - 1) / 5), 3));
			}
			break;
		case 2:				/* Shuffle */
			if (success)
			{
				/* A limited power 'wonder' spell */
				int die = randint1(120);

				if ((p_ptr->rp.pclass == CLASS_ROGUE) ||
					(p_ptr->rp.pclass == CLASS_HIGH_MAGE))
					die = (randint1(110)) + plev / 5;
				/* Card sharks and high mages get a level bonus */

				msgf("You shuffle the deck and draw a card...");

				if (die < 30)
					chg_virtue(V_CHANCE, 1);

				if (die < 7)
				{
					msgf("Oh no! It's Death!");
					for (dummy = 0; dummy < randint1(3); dummy++)
						(void)activate_hi_summon();
				}
				else if (die < 14)
				{
					msgf("Oh no! It's the Devil!");
					(void)summon_specific(0, px, py, p_ptr->depth,
										  SUMMON_DEMON, TRUE, FALSE, FALSE);
				}
				else if (die < 18)
				{
					int count = 0;

					msgf("Oh no! It's the Hanged Man.");
					(void)activate_ty_curse(FALSE, &count);
				}
				else if (die < 22)
				{
					msgf("It's the swords of discord.");
					aggravate_monsters(0);
				}
				else if (die < 26)
				{
					msgf("It's the Fool.");
					(void)do_dec_stat(A_INT);
					(void)do_dec_stat(A_WIS);
				}
				else if (die < 30)
				{
					msgf("It's the picture of a strange monster.");
					if (!
						(summon_specific
						 (0, px, py, (p_ptr->depth * 3) / 2, rand_range(33, 38),
						  TRUE, FALSE, FALSE)))
						no_trump = TRUE;
				}
				else if (die < 33)
				{
					msgf("It's the Moon.");
					(void)unlite_area(10, 3);
				}
				else if (die < 38)
				{
					msgf("It's the Wheel of Fortune.");
					wild_magic(randint0(32));
				}
				else if (die < 40)
				{
					msgf("It's a teleport trump card.");
					teleport_player(10);
				}
				else if (die < 42)
				{
					msgf("It's Justice.");
					(void)inc_blessed(p_ptr->lev);
				}
				else if (die < 47)
				{
					msgf("It's a teleport trump card.");
					teleport_player(100);
				}
				else if (die < 52)
				{
					msgf("It's a teleport trump card.");
					teleport_player(200);
				}
				else if (die < 60)
				{
					msgf("It's the Tower.");
					wall_breaker();
				}
				else if (die < 72)
				{
					msgf("It's Temperance.");
					(void)sleep_monsters_touch();
				}
				else if (die < 80)
				{
					msgf("It's the Tower.");
					(void)earthquake(px, py, 5);
				}
				else if (die < 82)
				{
					msgf("It's the picture of a friendly monster.");
					if (!
						(summon_specific
						 (-1, px, py, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE1,
						  FALSE, TRUE, TRUE)))
						no_trump = TRUE;
				}
				else if (die < 84)
				{
					msgf("It's the picture of a friendly monster.");
					if (!
						(summon_specific
						 (-1, px, py, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE2,
						  FALSE, TRUE, TRUE)))
						no_trump = TRUE;
				}
				else if (die < 86)
				{
					msgf("It's the picture of a friendly monster.");
					if (!
						(summon_specific
						 (-1, px, py, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE4,
						  FALSE, TRUE, TRUE)))
						no_trump = TRUE;
				}
				else if (die < 88)
				{
					msgf("It's the picture of a friendly monster.");
					if (!
						(summon_specific
						 (-1, px, py, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE5,
						  FALSE, TRUE, TRUE)))
						no_trump = TRUE;
				}
				else if (die < 96)
				{
					msgf("It's the Lovers.");
					if (get_aim_dir(&dir))
						(void)charm_monster(dir, MIN(p_ptr->lev, 20));
				}
				else if (die < 101)
				{
					msgf("It's the Hermit.");
					(void)wall_stone();
				}
				else if (die < 111)
				{
					msgf("It's the Judgement.");
					do_cmd_rerate();
					if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
					{
						msgf("You are cured of all mutations.");
						p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
						p_ptr->update |= PU_BONUS;
						handle_stuff();
					}
				}
				else if (die < 120)
				{
					msgf("It's the Sun.");
					wiz_lite();
				}
				else
				{
					msgf("It's the World.");
					if (p_ptr->exp < PY_MAX_EXP)
					{
						s32b ee = (p_ptr->exp / 25) + 1;
						if (ee > 5000) ee = 5000;
						msgf("You feel more experienced.");
						gain_exp(ee);
					}
				}
			}
			break;
		case 3:				/* Reset Recall */
			if (p_ptr->depth && success)
			{
				dun_type *d_ptr = dungeon();
				
				s16b max_depth = MAX(p_ptr->depth, d_ptr->recall_depth);
				s16b min_depth = d_ptr->min_level;
			
				/* Default */
				strnfmt(tmp_val, 160, "%d", MAX(max_depth, min_depth));

				/* Ask for a level */
				if (get_string(tmp_val, 11, "Reset to which level (%d-%d): ",
								 min_depth, max_depth))
				{
					/* Extract request */
					dummy = atoi(tmp_val);

					/* Paranoia */
					if (dummy < min_depth) dummy = min_depth;

					/* Paranoia */
					if (dummy > max_depth) dummy = max_depth;

					d_ptr->recall_depth = dummy;

					/* Accept request */
					msgf("Recall depth set to level %d (%d').", dummy,
							   dummy * 50);
				}
				else
				{
					return FALSE;
				}
			}
			break;
		case 4:				/* Teleport Self */
			if (success)
			{
				teleport_player(plev * 4);
			}
			break;
		case 5:				/* Dimension Door */
			if (success)
			{
				msgf("You open a dimensional gate. Choose a destination.");
				return dimension_door();
			}
			break;
		case 6:				/* Trump Spying */
			if (success)
			{
				(void)inc_tim_esp(rand_range(25, 55));
			}
			break;
		case 7:				/* Teleport Away */
			if (success)
			{
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_beam(GF_AWAY_ALL, dir, plev);
			}
			break;
		case 8:				/* Trump Object */
			if (success)
			{
				if (!get_aim_dir(&dir)) return FALSE;
				fetch(dir, plev * 15, TRUE);
			}
			break;
		case 9:				/* Trump Animal */
		{
			bool pet = success;	/* was (randint1(5) > 2) */
			int type = (pet ? SUMMON_ANIMAL_RANGER : SUMMON_ANIMAL);
			bool group = (pet ? FALSE : TRUE);

			if (success)
				msgf("You concentrate on the trump of an animal...");

			if (summon_specific
				((pet ? -1 : 0), px, py, plev, type, group, FALSE, pet))
			{
				if (!pet)
					msgf("An angry animal appears!");
			}
			else
			{
				no_trump = TRUE;
			}

			break;
		}
		case 10:				/* Phantasmal Servant */
			if (success)
			{
				if (summon_specific
					(-1, px, py, (plev * 3) / 2, SUMMON_PHANTOM, FALSE, TRUE,
					 TRUE))
				{
					msgf("'Your wish, master?'");
				}
				else
				{
					no_trump = TRUE;
				}
			}
			break;
		case 11:				/* Trump Monster */
		{
			bool pet = success;	/* was (randint1(5) > 2) */
			int type = (pet ? SUMMON_NO_UNIQUES : 0);
			bool group = (pet ? FALSE : TRUE);

			if (success)
				msgf("You concentrate on the trump of a monster...");

			if (summon_specific
				((pet ? -1 : 0), px, py, (plev * 3) / 2, type, group, FALSE,
				 pet))
			{
				if (!pet)
					msgf("An angry monster appears!");
			}
			else
			{
				no_trump = TRUE;
			}

			break;
		}
		case 12:				/* Conjure Elemental */
		{
			bool pet = success;	/* was (randint1(6) > 3) */
			bool group = (pet ? FALSE : TRUE);

			if (success)
				msgf("You concentrate on the trump of a monster...");

			if (summon_specific
				((pet ? -1 : 0), px, py, (plev * 3) / 2, SUMMON_ELEMENTAL,
				 group, FALSE, pet))
			{
				if (!pet)
					msgf("An angry elemental appears!");
			}
			else
			{
				no_trump = TRUE;
			}

			break;
		}
		case 13:				/* Teleport Level */
			if (success)
			{
				(void)teleport_player_level();
			}
			break;
		case 14:				/* Word of Recall */
			if (success)
			{
				word_of_recall();
			}
			break;
		case 15:				/* Banish */
			if (success)
			{
				(void)banish_monsters(plev * 4);
			}
			break;
		case 16:				/* Joker Card */
		{
			bool pet = success;	/* was one_in_(2) */
			bool group = (pet ? FALSE : TRUE);

			if (success)
				msgf("You concentrate on a joker card...");

			switch (randint1(4))
			{
				case 1: dummy = SUMMON_BIZARRE1;
					break;
				case 2: dummy = SUMMON_BIZARRE2;
					break;
				case 3: dummy = SUMMON_BIZARRE4;
					break;
				case 4: dummy = SUMMON_BIZARRE5;
					break;
			}

			if (summon_specific
				((pet ? -1 : 0), px, py, (plev * 3) / 2, dummy, group, FALSE,
				 pet))
			{
				if (!pet)
					msgf("An angry creature appears!");
			}
			else
			{
				no_trump = TRUE;
			}
			break;
		}
		case 17:				/* Trump Spiders */
		{
			bool pet = success;	/* (randint1(5) > 2) */
			bool group = (pet ? FALSE : TRUE);

			if (success)
				msgf("You concentrate on the trump of a spider...");

			if (summon_specific
				((pet ? -1 : 0), px, py, plev, SUMMON_SPIDER, group, FALSE,
				 pet))
			{
				if (!pet)
					msgf("An angry spiders appears!");
			}
			else
			{
				no_trump = TRUE;
			}

			break;
		}
		case 18:				/* Trump Reptiles */
		{
			bool pet = success;	/* was (randint1(5) > 2) */
			bool group = (pet ? FALSE : TRUE);

			if (success)
				msgf("You concentrate on the trump of a reptile...");

			if (summon_specific
				((pet ? -1 : 0), px, py, (plev * 3) / 2, SUMMON_HYDRA, group,
				 FALSE, pet))
			{
				if (!pet)
					msgf("An angry reptile appears!");
			}
			else
			{
				no_trump = TRUE;
			}

			break;
		}
		case 19:				/* Trump Hounds */
		{
			bool pet = success;	/* was (randint1(5) > 2) */

			if (success)
				msgf("You concentrate on the trump of a hound...");

			if (summon_specific
				((pet ? -1 : 0), px, py, plev, SUMMON_HOUND, TRUE, FALSE, pet))
			{
				if (!pet)
					msgf("Angry barking surrounds you!");
			}
			else
			{
				no_trump = TRUE;
			}

			break;
		}
		case 20:				/* Trump Branding */
			if (success)
			{
				brand_weapon(4);
			}
			break;
		case 21:				/* Living Trump */
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
					msgf("You have turned into a Living Trump.");
			}
			break;
		case 22:				/* Death Dealing */
			if (success)
			{
				(void)dispel_living(plev * 3);
			}
			break;
		case 23:				/* Trump Cyberdemon */
		{
			bool pet = success;	/* was (randint1(10) > 3) */

			if (success)
				msgf("You concentrate on the trump of a Cyberdemon...");

			if (summon_specific
				((pet ? -1 : 0), px, py, plev * 2, SUMMON_CYBER, FALSE, FALSE,
				 pet))
			{
				if (!pet)
					msgf("An angry Cyberdemon appears!");
			}
			else
			{
				no_trump = TRUE;
			}

			break;
		}
		case 24:				/* Trump Divination */
			if (success)
			{
				(void)detect_all();
			}
			break;
		case 25:				/* Trump Lore */
			if (success)
			{
				return identify_fully();
			}
			break;
		case 26:				/* Trump Undead */
		{
			bool pet = success;	/* (randint1(10) > 3) */
			bool group = (pet ? FALSE : TRUE);

			if (success)
				msgf
					("You concentrate on the trump of an undead creature...");

			if (summon_specific
				((pet ? -1 : 0), px, py, plev * 2, SUMMON_UNDEAD, group, FALSE,
				 pet))
			{
				if (!pet)
					msgf("An angry undead creature appears!");
			}
			else
			{
				no_trump = TRUE;
			}

			break;
		}
		case 27:				/* Trump Dragon */
		{
			bool pet = success;	/* was (randint1(10) > 3) */
			bool group = (pet ? FALSE : TRUE);

			if (success)
				msgf("You concentrate on the trump of a dragon...");

			if (summon_specific
				((pet ? -1 : 0), px, py, plev * 2, SUMMON_DRAGON, group, FALSE,
				 pet))
			{
				if (!pet)
					msgf("An angry dragon appears!");
			}
			else
			{
				no_trump = TRUE;
			}

			break;
		}
		case 28:				/* Mass Trump */
		{
			no_trump = TRUE;

			if (success)
				msgf("You concentrate on several trumps at once...");

			for (dummy = 0; dummy < 3 + (plev / 10); dummy++)
			{
				bool pet = success;	/* was (randint1(10) > 3) */
				bool group = (pet ? FALSE : TRUE);
				int type = (pet ? SUMMON_NO_UNIQUES : 0);

				if (summon_specific
					((pet ? -1 : 0), px, py, (plev * 3) / 2, type, group, FALSE,
					 pet))
				{
					if (!pet)
						msgf("An angry creature appears!");
					no_trump = FALSE;
				}
			}
			break;
		}
		case 29:				/* Trump Demon */
		{
			bool pet = success;	/* was (randint1(10) > 3) */
			bool group = (pet ? FALSE : TRUE);

			if (success)
				msgf("You concentrate on the trump of a demon...");

			if (summon_specific
				((pet ? -1 : 0), px, py, plev * 2, SUMMON_DEMON, group, FALSE,
				 pet))
			{
				if (!pet)
					msgf("An angry demon appears!");
			}
			else
			{
				no_trump = TRUE;
			}

			break;
		}
		case 30:				/* Trump Ancient Dragon */
		{
			bool pet = success;	/* was (randint1(10) > 3) */
			bool group = (pet ? FALSE : TRUE);
			int type = (pet ? SUMMON_HI_DRAGON_NO_UNIQUES : SUMMON_HI_DRAGON);

			if (success)
				msgf
					("You concentrate on the trump of an ancient dragon...");

			if (summon_specific
				((pet ? -1 : 0), px, py, plev * 2, type, group, FALSE, pet))
			{
				if (!pet)
					msgf("An angry ancient dragon appears!");
			}
			else
			{
				no_trump = TRUE;
			}

			break;
		}
		case 31:				/* Trump Greater Undead */
		{
			bool pet = success;	/* was (randint1(10) > 3) */
			bool group = (pet ? FALSE : TRUE);
			int type = (pet ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_HI_UNDEAD);

			if (success)
				msgf
					("You concentrate on the trump of a greater undead being...");

			if (summon_specific
				((pet ? -1 : 0), px, py, plev * 2, type, group, FALSE, pet))
			{
				if (!pet)
					msgf("An angry greater undead creature appears!");
			}
			else
			{
				no_trump = TRUE;
			}

			break;
		}
		default:
			msgf("You cast an unknown Trump spell: %d.", spell);
			message_flush();
	}

	if (no_trump)
	{
		msgf("Nobody answers to your Trump call.");
	}

	make_noise(2);

	return TRUE;
}


static bool cast_arcane_spell(int spell)
{
	int dir;
	int beam;
	int plev = p_ptr->lev;
	int dummy = 0;

	if (p_ptr->rp.pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->rp.pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else
		beam = plev / 2;

	switch (spell)
	{
		case 0:				/* Zap */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
									damroll(3 + ((plev - 1) / 5), 3));
			break;
		case 1:				/* Wizard Lock */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)wizard_lock(dir);
			break;
		case 2:				/* Detect Invisibility */
			(void)detect_monsters_invis();
			break;
		case 3:				/* Detect Monsters */
			(void)detect_monsters_normal();
			break;
		case 4:				/* Blink */
			teleport_player(10);
			break;
		case 5:				/* Light Area */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
		case 6:				/* Trap & Door Destruction */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)destroy_door(dir);
			break;
		case 7:				/* Cure Light Wounds */
			(void)hp_player(damroll(2, 8));
			(void)inc_cut(-10);
			break;
		case 8:				/* Detect Doors & Traps */
			(void)detect_traps(TRUE);
			(void)detect_doors();
			(void)detect_stairs();
			break;
		case 9:				/* Phlogiston */
			phlogiston();
			break;
		case 10:				/* Detect Treasure */
			(void)detect_treasure();
			(void)detect_objects_gold();
			break;
		case 11:				/* Detect Enchantment */
			(void)detect_objects_magic();
			break;
		case 12:				/* Detect Object */
			(void)detect_objects_normal();
			break;
		case 13:				/* Cure Poison */
			(void)clear_poisoned();
			break;
		case 14:				/* Resist Cold */
			(void)inc_oppose_cold(rand_range(20, 40));
			break;
		case 15:				/* Resist Fire */
			(void)inc_oppose_fire(rand_range(20, 40));
			break;
		case 16:				/* Resist Lightning */
			(void)inc_oppose_elec(rand_range(20, 40));
			break;
		case 17:				/* Resist Acid */
			(void)inc_oppose_acid(rand_range(20, 40));
			break;
		case 18:				/* Cure Medium Wounds */
			(void)hp_player(damroll(4, 8));
			(void)inc_cut(-50);
			break;
		case 19:				/* Teleport */
			teleport_player(plev * 5);
			break;
		case 20:				/* Stone to Mud */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)wall_to_mud(dir);
			break;
		case 21:				/* Ray of Light */
			if (!get_aim_dir(&dir)) return FALSE;

			msgf("A line of light appears.");
			(void)lite_line(dir, damroll(6, 8));
			break;
		case 22:				/* Satisfy Hunger */
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		case 23:				/* See Invisible */
			(void)inc_tim_invis(rand_range(24, 48));
			break;
		case 24:				/* Recharging */
			return recharge(plev * 4);
		case 25:				/* Teleport Level */
			(void)teleport_player_level();
			break;
		case 26:				/* Identify */
			return ident_spell();
		case 27:				/* Teleport Away */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_beam(GF_AWAY_ALL, dir, plev);
			break;
		case 28:				/* Elemental Ball */
			if (!get_aim_dir(&dir)) return FALSE;

			switch (randint1(4))
			{
				case 1: dummy = GF_FIRE;
					break;
				case 2: dummy = GF_ELEC;
					break;
				case 3: dummy = GF_COLD;
					break;
				default: dummy = GF_ACID;
					break;
			}
			(void)fire_ball(dummy, dir, 75 + (plev), 2);
			break;
		case 29:				/* Detection */
			(void)detect_all();
			break;
		case 30:				/* Word of Recall */
			word_of_recall();
			break;
		case 31:				/* Clairvoyance */
			wiz_lite();
			if (!(FLAG(p_ptr, TR_TELEPATHY)))
			{
				(void)inc_tim_esp(rand_range(25, 55));
			}
			break;
		default:
			msgf("You cast an unknown Arcane spell: %d.", spell);
			message_flush();
	}

	make_noise(2);

	return TRUE;
}


/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int sval, spell, realm;
	int chance, smana;
	int increment = 0;
	int use_realm;
	bool cast;

	const cptr prayer =
		((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell");

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;

	/* Require spell ability */
	if (!p_ptr->spell.r[0].realm)
	{
		msgf("You cannot cast spells!");
		return;
	}

	/* Require lite */
	if (p_ptr->tim.blind || no_lite())
	{
		msgf("You cannot see!");
		return;
	}

	/* Not when confused */
	if (p_ptr->tim.confused)
	{
		msgf("You are too confused!");
		return;
	}

	/* Restrict choices to spell books */
	item_tester_tval = mp_ptr->spell_book;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no spell books!";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Access the item's sval */
	sval = o_ptr->sval;

	if (o_ptr->tval == REALM2_BOOK) increment = 32;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	realm = p_ptr->spell.r[increment / 32].realm;

	/* Ask for a spell */
	if (!get_spell
		(&spell, ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
		 sval, TRUE, (bool)(increment ? TRUE : FALSE)))
	{
		if (spell == -2)
			msgf("You don't know any %ss in that book.", prayer);
		return;
	}


	/* Access the spell */
	use_realm = p_ptr->spell.r[increment / 32].realm;

	s_ptr = &mp_ptr->info[use_realm - 1][spell];

	/* Get mana cost */
	smana = spell_mana(spell, use_realm - 1);

	/* Verify "dangerous" spells */
	if (smana > p_ptr->csp)
	{
		/* Warning */
		msgf("You do not have enough mana to %s this %s.",
				   ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
				   prayer);

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
	chance = spell_chance(spell, use_realm - 1);

	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (flush_failure) flush();

		msgf("You failed to get the %s off!", prayer);
		sound(SOUND_FAIL);

		if ((randint1(100) < chance) && (mp_ptr->spell_book == TV_LIFE_BOOK))
			chg_virtue(V_FAITH, -1);
		else if (randint1(100) < chance)
			chg_virtue(V_KNOWLEDGE, -1);


		if (realm == REALM_TRUMP)
		{
			(void)cast_trump_spell(spell, FALSE);
		}
		else if ((o_ptr->tval == TV_CHAOS_BOOK) && (randint1(100) < spell))
		{
			msgf("You produce a chaotic effect!");
			wild_magic(spell);
		}
		else if ((o_ptr->tval == TV_DEATH_BOOK) && (randint1(100) < spell))
		{
			if ((sval == 3) && one_in_(2))
			{
				msgf("Your sanity is shaken by reading the Necronomicon!");

				/* Mind blast */
				if (!player_save(100))
				{
					if (!(FLAG(p_ptr, TR_RES_CONF)))
					{
						(void)inc_confused(rand_range(4, 8));
					}
					if (!(FLAG(p_ptr, TR_RES_CHAOS)) && one_in_(3))
					{
						(void)inc_image(rand_range(150, 400));
					}
				}

				/* Lose int & wis */
				else if (!player_save(100))
				{
					(void)do_dec_stat(A_INT);
					(void)do_dec_stat(A_WIS);
				}
			}
			else
			{
				msgf("It hurts!");
				take_hit(damroll(o_ptr->sval + 1, 6), "a miscast Death spell");
				if ((spell > 15) && one_in_(6) &&
					 !(FLAG(p_ptr, TR_HOLD_LIFE)))
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
			case REALM_LIFE:	/* * LIFE * */
				cast = cast_life_spell(spell);
				break;
			case REALM_SORCERY:	/* * SORCERY * */
				cast = cast_sorcery_spell(spell);
				break;
			case REALM_NATURE:	/* * NATURE * */
				cast = cast_nature_spell(spell);
				break;
			case REALM_CHAOS:	/* * CHAOS * */
				cast = cast_chaos_spell(spell);
				break;
			case REALM_DEATH:	/* * DEATH * */
				cast = cast_death_spell(spell);
				break;
			case REALM_TRUMP:	/* TRUMP */
				cast = cast_trump_spell(spell, TRUE);
				break;
			case REALM_ARCANE:	/* ARCANE */
				cast = cast_arcane_spell(spell);
				break;
			default:
				cast = FALSE;
				msgf("You cast a spell from an unknown realm: realm %d, spell %d.",
					 realm, spell);
				message_flush();
		}

		/* Canceled spells cost neither a turn nor mana */
		if (!cast) return;

		/* A spell was cast */
		if (!(p_ptr->spell.r[increment / 32].worked & (1L << spell)))
		{
			/* Experience: 5, 20, 45, or 80 * spell level */
			int book = 1 + (spell / 8);
			int exp = 5 * book * book * s_ptr->slevel;

			/* The spell worked */
			if (realm == p_ptr->spell.r[0].realm)
			{
				p_ptr->spell.r[0].worked |= (1L << spell);
			}
			else
			{
				p_ptr->spell.r[1].worked |= (1L << spell);
			}

			/* Gain experience */
			gain_exp(exp);

			if (mp_ptr->spell_book == TV_LIFE_BOOK)
				chg_virtue(V_FAITH, 1);
			else
				chg_virtue(V_KNOWLEDGE, 1);
		}
	}

	/* Take a turn */
	p_ptr->state.energy_use = 100;

	/* Sufficient mana */
	if (smana <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= smana;
	}

	/* Over-exert the player */
	else
	{
		int oops = smana - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		msgf("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)inc_paralyzed(randint1(5 * oops + 1));

		if (mp_ptr->spell_book == TV_LIFE_BOOK)
			chg_virtue(V_FAITH, -10);
		else
			chg_virtue(V_KNOWLEDGE, -10);

		/* Damage CON (possibly permanently) */
		if (one_in_(2))
		{
			bool perm = one_in_(4);

			/* Message */
			msgf("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, rand_range(15, 25), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}


/*
 * Pray a prayer -- Unused in Zangband
 */
void do_cmd_pray(void)
{
	msgf("Praying is not used in %s. Use magic spell casting instead.",
			   VERSION_NAME);
}

/* Forward declare */
extern menu_type pet_menu[PET_CHOICE_MAX + 1];


/*
 * Dismiss some pets
 */
static bool cmd_pets_dismiss(int dummy)
{
	int dismissed = 0;
	
	monster_type *m_ptr;
	
	int pet_ctr;
	bool pets = FALSE, all_pets = FALSE;

	/* Ignore parameter */
	(void) dummy;

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
				if (get_check("Dismiss %v? ", MONSTER_FMT(m_ptr, 0x80)))
					delete_this = TRUE;
			}

			if (delete_this)
			{
				/* Notice changes in view */
				if (r_info[m_ptr->r_idx].flags[6] &
					(RF6_LITE_1 | RF6_LITE_2))
				{
					/* Update some things */
					p_ptr->update |= (PU_MON_LITE);
				}

				delete_monster_idx(pet_ctr);
				dismissed++;
			}
		}
	}

	msgf("You have dismissed %d pet%s.", dismissed,
			   (dismissed == 1 ? "" : "s"));
	
	/* Calculate pets */
	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		/* Access the monster */
		m_ptr = &m_list[pet_ctr];

		if (is_pet(m_ptr))
		{
			/* Is it a pet? */
			pets = TRUE;
			break;
		}
	}

	/* Can only dismiss pets if actually have some */
	if (pets)
	{
		pet_menu[PET_DISMISS].flags |= MN_ACTIVE;
	}
	else
	{
		pet_menu[PET_DISMISS].flags &= ~(MN_ACTIVE);
	}

	/* Stay at menu */
	return (FALSE);
}


static bool cmd_pets_close(int dummy)
{
	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Change follow distance */
	p_ptr->pet_follow_distance = PET_CLOSE_DIST;
	
	/* Stay at menu */
	return (FALSE);
}

static bool cmd_pets_follow(int dummy)
{
	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Change follow distance */
	p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
	
	/* Stay at menu */
	return (FALSE);
}

static bool cmd_pets_destroy(int dummy)
{
	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Change follow distance */
	p_ptr->pet_follow_distance = PET_DESTROY_DIST;
	
	/* Stay at menu */
	return (FALSE);
}

static bool cmd_pets_space(int dummy)
{
	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Change follow distance */
	p_ptr->pet_follow_distance = PET_SPACE_DIST;
	
	/* Stay at menu */
	return (FALSE);
}

static bool cmd_pets_away(int dummy)
{
	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Change follow distance */
	p_ptr->pet_follow_distance = PET_AWAY_DIST;
	
	/* Stay at menu */
	return (FALSE);
}

static bool cmd_pets_doors(int dummy)
{
	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Toggle the open doors flag */
	p_ptr->pet_open_doors = !p_ptr->pet_open_doors;
	
	if (p_ptr->pet_open_doors)
	{
		pet_menu[PET_OPEN_DOORS].text = "pets may open doors";
	}
	else
	{
		pet_menu[PET_OPEN_DOORS].text = "pets may not open doors";
	}

		
	/* Stay at menu */
	return (FALSE);
}

static bool cmd_pets_items(int dummy)
{
	int pet_ctr;
	
	monster_type *m_ptr;
	
		/* Hack - ignore parameter */
	(void) dummy;


	/* Toggle pet pickup flag */
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
				drop_object_list(&m_ptr->hold_o_idx,
								 m_ptr->fx, m_ptr->fy);
			}
		}
	}
	
	if (p_ptr->pet_pickup_items)
	{
		pet_menu[PET_TAKE_ITEMS].text = "pets may pick up items";
	}
	else
	{
		pet_menu[PET_TAKE_ITEMS].text = "pets may not pick up items";
	}
	
	/* Stay at menu */
	return (FALSE);
}


/* The menu used to interact with pets */
menu_type pet_menu[PET_CHOICE_MAX + 1] =
{
	{"Stay close", NULL, cmd_pets_close, MN_ACTIVE | MN_SELECT},
	{"Follow me", NULL, cmd_pets_follow, MN_ACTIVE | MN_SELECT},
	{"Seek and destroy", NULL, cmd_pets_destroy, MN_ACTIVE | MN_SELECT},
	{"Give me space", NULL, cmd_pets_space, MN_ACTIVE | MN_SELECT},
	{"Stay away", NULL, cmd_pets_away, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, cmd_pets_doors, MN_ACTIVE},
	{NULL, NULL, cmd_pets_items, MN_ACTIVE},
	{"Display current pets", NULL, do_cmd_knowledge_pets, MN_ACTIVE | MN_CLEAR},
	{"Dismiss pets", NULL, cmd_pets_dismiss, MN_ACTIVE | MN_CLEAR},
	MENU_END
};


/*
 * Issue a pet command
 */
void do_cmd_pet(void)
{
	bool pets = FALSE;
	int pet_ctr;
	monster_type *m_ptr;
	int pet_select = -1;

	/* Calculate pets */
	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		/* Access the monster */
		m_ptr = &m_list[pet_ctr];

		if (is_pet(m_ptr))
		{
			/* Is it a pet? */
			pets = TRUE;
			break;
		}
	}

	/* Can only dismiss pets if actually have some */
	if (pets)
	{
		pet_menu[PET_DISMISS].flags |= MN_ACTIVE;
	}
	else
	{
		pet_menu[PET_DISMISS].flags &= ~(MN_ACTIVE);
	}

	/* Get current option */
	if (p_ptr->pet_follow_distance == PET_CLOSE_DIST)
	{
		pet_select = PET_STAY_CLOSE;
	}
	
	if (p_ptr->pet_follow_distance == PET_FOLLOW_DIST)
	{
		pet_select = PET_FOLLOW_ME;
	}
	
	if (p_ptr->pet_follow_distance == PET_DESTROY_DIST)
	{
		pet_select = PET_SEEK_AND_DESTROY;
	}
	
	if (p_ptr->pet_follow_distance == PET_SPACE_DIST)
	{
		pet_select = PET_ALLOW_SPACE;
	}
	
	if (p_ptr->pet_follow_distance == PET_AWAY_DIST)
	{
		pet_select = PET_STAY_AWAY;
	}
	
	/* Change option text depending on flag */
	if (p_ptr->pet_open_doors)
	{
		pet_menu[PET_OPEN_DOORS].text = "pets may open doors";
	}
	else
	{
		pet_menu[PET_OPEN_DOORS].text = "pets may not open doors";
	}

	/* Change option text depending on flag */
	if (p_ptr->pet_pickup_items)
	{
		pet_menu[PET_TAKE_ITEMS].text = "pets may pick up items";
	}
	else
	{
		pet_menu[PET_TAKE_ITEMS].text = "pets may not pick up items";
	}

	/* Interact with menu */
	display_menu(pet_menu, pet_select, FALSE, NULL, NULL);
}
