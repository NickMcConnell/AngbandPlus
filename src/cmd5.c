/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, restoreearch,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
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
static int get_spell(int *sn, cptr prompt, int sval, bool known)
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

	int priest = ((mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book == TV_PRAYER_BOOK || mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book == TV_DEATH_BOOK) ? 1 : 0);

	cptr p = ((priest) ? "prayer" : "spell");

#ifdef ALLOW_REPEAT

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (spell_okay(*sn, known))
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT */

	/* Extract spells */
	for (spell = 0; spell < PY_MAX_SPELLS; spell++)
	{
		/* Check for this spell */
		if ((spell < 32) ?
		    (spell_flags[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][sval][0] & (1L << spell)) :
		    (spell_flags[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][sval][1] & (1L << (spell - 32))))
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
		if (spell_okay(spells[i], known)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);


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
		verify = (isupper(choice) ? TRUE : FALSE);

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
		if (verify)
		{
			char tmp_val[160];

			/* Get the spell */
			s_ptr = &mp_ptr[p_ptr->pclass[p_ptr->current_class]]->info[spell];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
			        prompt, spell_names[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][spell],
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

		/* Hack -- forget redraw */
		/* redraw = FALSE; */
	}


	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = spell;

#ifdef ALLOW_REPEAT

	repeat_push(*sn);

#endif /* ALLOW_REPEAT */

	/* Success */
	return (TRUE);
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
	int item, sval;

	int spell, i;
	int num = 0;

	byte spells[PY_MAX_SPELLS];

	object_type *o_ptr;

	cptr q, s;

	char choice;

	char out_val[160];

	int priest = ((mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book == TV_PRAYER_BOOK || mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book == TV_DEATH_BOOK) ? 1 : 0);

	cptr p = ((priest) ? "prayer" : "spell");

	/* Warriors are illiterate */
	if (!mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book)
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
	item_tester_tval = mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book;

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

	/* Get the item's sval */
	sval = o_ptr->sval;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Extract spells */
	for (spell = 0; spell < PY_MAX_SPELLS; spell++)
	{
		/* Check for this spell */
		if ((spell < 32) ?
		    (spell_flags[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][sval][0] & (1L << spell)) :
		    (spell_flags[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][sval][1] & (1L << (spell - 32))))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(%^ss %c-%c, ESC=exit) Browse which %s? ",
	p, I2A(0), I2A(num - 1), p);

	/* Save screen */
	screen_save();

	/* Display a list of spells */
	print_spells(spells, num, 1, 20);

	/* Get a spell from the user */
	while (get_com(out_val, &choice))
	{
		/* Display a list of spells */
		print_spells(spells, num, 1, 20);

		/* Stop browsing */
		if (choice == ESCAPE) continue;

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

		/* Display spell description */
		c_prt(TERM_L_BLUE, spell_descs[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][spell], num+2, 22);
	}

	/* Restore the screen */
	screen_load();
}

/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int i, item, sval;

	int spell = -1;

	cptr q, s;

	object_type *o_ptr;

	int priest = ((mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book == TV_PRAYER_BOOK || mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book == TV_DEATH_BOOK) ? 1 : 0);

	cptr p = ((priest) ? "prayer" : "spell");

	if (!mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book)
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
	item_tester_tval = mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book;

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

	/* Get the item's sval */
	sval = o_ptr->sval;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Mage -- Learn a selected spell */
	if (!priest)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", sval, FALSE) && (spell == -1)) return;
	}

	/* Priest -- Learn a random prayer */
	if (priest)
	{
		int k = 0;

		int gift = -1;

		/* Extract spells */
		for (spell = 0; spell < PY_MAX_SPELLS; spell++)
		{
			/* Check spells in the book */
			if ((spell < 32) ?
			    (spell_flags[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][sval][0] & (1L << spell)) :
			    (spell_flags[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][sval][1] & (1L << (spell - 32))))
			{
				/* Skip non "okay" prayers */
				if (!spell_okay(spell, FALSE)) continue;

				/* Apply the randomizer */
				if ((++k > 1) && (rand_int(k) != 0)) continue;

				/* Track it */
				gift = spell;
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

	/* Learn the spell */
	if (spell < 32)
	{
	  p_ptr->spell_learned1[(priest)] |= (1L << spell);
	}
	else
	{
	  p_ptr->spell_learned2[(priest)] |= (1L << (spell - 32));
	}

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
	  /* Stop at the first empty space */
	  if (p_ptr->spell_order[(priest)][i] == 99) break;
	}

	/* Add the spell to the known list */
	p_ptr->spell_order[(priest)][i] = spell;

	/* Mention the result */
	message_format(MSG_STUDY, 0, "You have learned the %s of %s.",
	           p, spell_names[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][spell]);

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

	/* Redraw object recall */
	p_ptr->window |= (PW_OBJECT);
}



/*
 * Brand the current weapon
 */
static void brand_weapon(void)
{
	object_type *o_ptr;

	o_ptr = &inventory[INVEN_WIELD];

	/* you can never modify artifacts / ego-items */
	/* you can never modify broken / cursed items */
	if ((o_ptr->k_idx) &&
	    (!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
	    (!broken_p(o_ptr)) && (!cursed_p(o_ptr)))
	{
		cptr act;

		char o_name[80];

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

		object_desc(o_name, o_ptr, FALSE, 0);

		msg_format("Your %s %s", o_name, act);

		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}

	else
	{
		if (flush_failure) flush();
		msg_print("The Branding failed.");
	}
}

/*
* Brand some ammunition.  Used by an illusion spell. 
 */
void brand_ammo(int brand_type)
{
	int a;
	int allowable;

	allowable = TV_BOLT | TV_ARROW | TV_SHOT;

	for (a = 0; a < INVEN_PACK; a++)
	{
		object_type *o_ptr = &inventory[a];

		if ((o_ptr->tval != TV_BOLT) &&
		    (o_ptr->tval != TV_ARROW) && 
		    (o_ptr->tval != TV_SHOT))
		  continue;
		if ((!artifact_p(o_ptr)) && 
		    (!ego_item_p(o_ptr)) &&
		    (!cursed_p(o_ptr)))
		  break;
	}

	/* Enchant the ammo (or fail) */
	if ((a < INVEN_PACK) && (rand_int(100) < 50))
	{
		object_type *o_ptr = &inventory[a];
		char *ammo_name, *aura_name, msg[48];
		int aura_type;

		switch (brand_type == 0 ? randint(3) : brand_type)
		{
		case 1:
			aura_name = "fiery";
			aura_type = EGO_BRAND_FIRE;
			break;
		case 2:
			aura_name = "frosty";
			aura_type = EGO_BRAND_COLD;
			break;
		case 3:
			aura_name = "sparkling";
			aura_type = EGO_BRAND_ELEC;
			break;
		case 4:
			aura_name = "acidic";
			aura_type = EGO_BRAND_ACID;
			break;
		case 5:
		default:
			aura_name = "sickly green";
			aura_type = EGO_BRAND_POIS;
			break;
		}
		
		if (o_ptr->tval == TV_BOLT)
			ammo_name = "bolts";
		else if (o_ptr->tval == TV_ARROW)
			ammo_name = "arrows";
		else
			ammo_name = "shots";

		sprintf (msg, "Your %s are covered in a %s aura!",
			ammo_name, aura_name);
		msg_print (msg);
		o_ptr->name2 = aura_type;
		enchant (o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}
	else
	{
		if (flush_failure) flush();
		msg_print ("The enchantment failed.");
	}
}

/*
 * Poison the current weapon
 */
static void poison_weapon(void)
{
	object_type *o_ptr;

	o_ptr = &inventory[INVEN_WIELD];

	/* you can never modify artifacts / ego-items */
	/* you can never modify broken / cursed items */
	if ((o_ptr->k_idx) &&
	    (!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
	    (!broken_p(o_ptr)) && (!cursed_p(o_ptr)))
	{
		cptr act;

		char o_name[80];

		act = "is covered in a sickly green aura!";
		o_ptr->name2 = EGO_BRAND_POIS;

		object_desc(o_name, o_ptr, FALSE, 0);

		msg_format("Your %s %s", o_name, act);

		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}

	else
	{
		if (flush_failure) flush();
		msg_print("The Branding failed.");
	}
}

/* Cast a spell */
bool cast_spell(int realm, int spell)
{
  int beam, dir;
  int py = p_ptr->py;
  int px = p_ptr->px;
  /* Runecasters and sorcerors cast spells at their class level */
  int plev = 1;
  if (player_has_class(CLASS_RUNECASTER, 0)) plev = level_of_class(CLASS_RUNECASTER);
  if (player_has_class(CLASS_SORCEROR, 0)) plev = level_of_class(CLASS_SORCEROR);

  switch (realm)
  {
      case REALM_MAGIC:
      {
	/* Only one of these classes can be chosen */
	if (player_has_class(CLASS_MAGE, 0)) 
	  plev = level_of_class(CLASS_MAGE);

	/* Hack -- chance of "beam" instead of "bolt" */
	beam = ((player_has_class(CLASS_MAGE, 0)) ? plev : (plev / 2));

	/* Spells.  */
	switch (spell)
	  {
	  case SPELL_MAGIC_MISSILE:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
				damroll(3 + ((plev - 1) / 5), 4));
	      effects[EFFECT_BBOLT] += 1;
	      effects[EFFECT_ARROW] += 1;
	      break;
	    }

	  case SPELL_DETECT_MONSTERS:
	    {
	      (void)detect_monsters_normal();
	      effects[EFFECT_DETECT_MONSTERS]++;
	      break;
	    }

	  case SPELL_PHASE_DOOR:
	    {
	      teleport_player(10);
	      effects[EFFECT_BLINK]++;
	      break;
	    }

	  case SPELL_LIGHT_AREA:
	    {
	      (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
	      effects[EFFECT_LIGHT_AREA]++;
	      break;
	    }

	  case SPELL_TREASURE_DETECTION:
	    {
	      (void)detect_treasure();
	      (void)detect_objects_gold();
	      effects[EFFECT_DETECT_ITEM]++;
	      break;
	    }

	  case SPELL_CURE_LIGHT_WOUNDS:
	    {
	      (void)hp_player(damroll(2, 8));
	      (void)set_cut(p_ptr->cut - 15);
	      effects[EFFECT_CURE_LIGHT_SERIOUS]++;
	      break;
	    }

	  case SPELL_OBJECT_DETECTION:
	    {
	      (void)detect_objects_normal();
	      effects[EFFECT_DETECT_ITEM]++;
	      break;
	    }

	  case SPELL_FIND_TRAPS_DOORS:
	    {
	      (void)detect_traps();
	      (void)detect_doors();
	      (void)detect_stairs();
	      effects[EFFECT_DETECT_DOOR_TRAP]++;
	      break;
	    }

	  case SPELL_STINKING_CLOUD:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_POIS, dir,
			10 + (plev / 2), 2);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_POIS]++;
	      break;
	    }

	  case SPELL_CONFUSE_MONSTER:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      (void)confuse_monster(dir, plev);
	      effects[EFFECT_HINDER]++;
	      break;
	    }

	  case SPELL_LIGHTNING_BOLT:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_bolt_or_beam(beam-10, GF_ELEC, dir,
				damroll(3+((plev-5)/4), 8));
	      effects[EFFECT_BBOLT]++;
	      effects[EFFECT_LIGHTNING]++;
	      break;
	    }

	  case SPELL_TRAP_DOOR_DESTRUCTION:
	    {
	      (void)destroy_doors_touch();
	      effects[EFFECT_DOOR_DESTRUCT]++;
	      break;
	    }

	  case SPELL_SLEEP_I:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      (void)sleep_monster(dir, p_ptr->current_class);
	      effects[EFFECT_HINDER]++;
	      break;
	    }

	  case SPELL_CURE_POISON:
	    {
	      (void)set_poisoned(0);
	      effects[EFFECT_CURE_POISON]++;
	      break;
	    }

	  case SPELL_TELEPORT_SELF:
	    {
	      teleport_player(plev * 5);
	      effects[EFFECT_TELEPORT]++;
	      break;
	    }

	  case SPELL_SPEAR_OF_LIGHT:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      msg_print("A line of blue shimmering light appears.");
	      lite_line(dir);
	      effects[EFFECT_BEAM]++;
	      effects[EFFECT_LIGHT]++;
	      break;
	    }

	  case SPELL_FROST_BOLT:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_bolt_or_beam(beam-10, GF_COLD, dir,
				damroll(5+((plev-5)/4), 8));
	      effects[EFFECT_BBOLT]++;
	      effects[EFFECT_COLD]++;
	      break;
	    }

	  case SPELL_TURN_STONE_TO_MUD:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      (void)wall_to_mud(dir);
	      effects[EFFECT_STONE_TO_MUD]++;
	      break;
	    }

	  case SPELL_SATISFY_HUNGER:
	    {
	      (void)set_food(PY_FOOD_MAX - 1);
	      effects[EFFECT_SATISFY_HUNGER]++;
	      break;
	    }

	  case SPELL_RECHARGE_ITEM_I:
	    {
	      if (!recharge(5)) return FALSE;
	      effects[EFFECT_RECHARGE]++;
	      break;
	    }

	  case SPELL_SLEEP_II:
	    {
	      (void)sleep_monsters_touch(p_ptr->current_class);
	      effects[EFFECT_MASS_HINDER]++;
	      break;
	    }

	  case SPELL_POLYMORPH_OTHER:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      (void)poly_monster(dir, p_ptr->current_class);
	      effects[EFFECT_POLY_OTHER]++;
	      break;
	    }
	
	  case SPELL_IDENTIFY:
	    {
	      if (!ident_spell()) return FALSE;
	      effects[EFFECT_IDENTIFY]++;
	      break;
	    }

	  case SPELL_SLEEP_III:
	    {
	      (void)sleep_monsters(p_ptr->current_class);
	      effects[EFFECT_MASS_HINDER]++;
	      break;
	    }

	  case SPELL_FIRE_BOLT:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_bolt_or_beam(beam, GF_FIRE, dir,
				damroll(8+((plev-5)/4), 8));
	      effects[EFFECT_BBOLT]++;
	      effects[EFFECT_FIRE]++;
	      break;
	    }

	  case SPELL_SLOW_MONSTER:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      (void)slow_monster(dir, p_ptr->current_class);
	      effects[EFFECT_HINDER]++;
	      break;
	    }

	  case SPELL_FROST_BALL:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_COLD, dir,
			30 + (plev), 2);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_COLD]++;
	      break;
	    }

	  case SPELL_RECHARGE_ITEM_II:
	    {
	      if (!recharge(40)) return FALSE;
	      effects[EFFECT_RECHARGE]++;
	      break;
	    }

	  case SPELL_TELEPORT_OTHER:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      (void)teleport_monster(dir);
	      effects[EFFECT_TELEPORT_OTHER]++;
	      break;
	    }

	  case SPELL_HASTE_SELF:
	    {
	      if (!p_ptr->fast)
		{
		  (void)set_fast(randint(20) + plev);
		}
	      else
		{
		  (void)set_fast(p_ptr->fast + randint(5));
		}
	      effects[EFFECT_HASTE]++;
	      break;
	    }

	  case SPELL_FIRE_BALL:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_FIRE, dir,
			55 + (plev), 2);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_FIRE]++;
	      break;
	    }

	  case SPELL_WORD_OF_DESTRUCTION:
	    {
	      destroy_area(py, px, 15, TRUE);
	      effects[EFFECT_DESTRUCTION]++;
	      break;
	    }

	  case SPELL_GENOCIDE:
	    {
	      (void)genocide();
	      effects[EFFECT_GENOCIDE]++;
	      break;
	    }

	  case SPELL_DOOR_CREATION:
	    {
	      (void)door_creation();
	      effects[EFFECT_DOOR_CREATION]++;
	      break;
	    }

	  case SPELL_STAIR_CREATION:
	    {
	      (void)stair_creation();
	      effects[EFFECT_STAIR_CREATION]++;
	      break;
	    }

	  case SPELL_TELEPORT_LEVEL:
	    {
	      (void)teleport_player_level();
	      effects[EFFECT_ALTER_REALITY]++;
	      break;
	    }

	  case SPELL_EARTHQUAKE:
	    {
	      earthquake(py, px, 10);
	      effects[EFFECT_EARTHQUAKE]++;
	      break;
	    }

	  case SPELL_WORD_OF_RECALL:
	    {
	      if (p_ptr->astral)
		{
		  msg_print("You feel a terrible sense of loss.");
		  break;
		}
	      set_recall();
	      effects[EFFECT_RECALL]++;
	      break;
	    }

	  case SPELL_ACID_BOLT:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_bolt_or_beam(beam, GF_ACID, dir,
				damroll(6+((plev-5)/4), 8));
	      effects[EFFECT_BBOLT]++;
	      effects[EFFECT_ACID]++;
	      break;
	    }

	  case SPELL_CLOUD_KILL:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_POIS, dir,
			20 + (plev / 2), 3);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_POIS]++;
	      break;
	    }

	  case SPELL_ACID_BALL:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_ACID, dir,
			40 + (plev), 2);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_ACID]++;
	      break;
	    }

	  case SPELL_ICE_STORM:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_COLD, dir,
			70 + (plev), 3);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_COLD]++;
	      break;
	    }

	  case SPELL_METEOR_SWARM:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_METEOR, dir,
			65 + (plev), 3);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_METEOR]++;
	      break;
	    }

	  case SPELL_MANA_STORM:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_MANA, dir,
			300 + (plev * 2), 3);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_MANA]++;
	      break;
	    }

	  case SPELL_DETECT_EVIL:
	    {
	      (void)detect_monsters_evil();
	      effects[EFFECT_DETECT_SEMI]++;
	      break;
	    }

	  case SPELL_DETECT_ENCHANTMENT:
	    {
	      (void)detect_objects_magic();
	      effects[EFFECT_DETECT_ITEM]++;
	      break;
	    }

	  case SPELL_RECHARGE_ITEM_III:
	    {
	      if (!recharge(100)) return FALSE;
	      effects[EFFECT_RECHARGE]++;
	      break;
	    }

	  case SPELL_GENOCIDE2:
	    {
	      (void)genocide();
	      effects[EFFECT_GENOCIDE]++;
	      break;
	    }

	  case SPELL_MASS_GENOCIDE:
	    {
	      (void)mass_genocide();
	      effects[EFFECT_MASS_GENOCIDE]++;
	      break;
	    }

	  case SPELL_RESIST_FIRE:
	    {
	      (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
	      effects[EFFECT_RES_ELEMENT]++;
	      break;
	    }

	  case SPELL_RESIST_COLD:
	    {
	      (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
	      effects[EFFECT_RES_ELEMENT]++;
	      break;
	    }

	  case SPELL_RESIST_ACID:
	    {
	      (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
	      effects[EFFECT_RES_ELEMENT]++;
	      break;
	    }

	  case SPELL_RESIST_POISON:
	    {
	      (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
	      effects[EFFECT_RES_POIS]++;
	      break;
	    }

	  case SPELL_RESISTANCE:
	    {
	      int time = randint(20) + 20;
	      (void)set_oppose_acid(p_ptr->oppose_acid + time);
	      (void)set_oppose_elec(p_ptr->oppose_elec + time);
	      (void)set_oppose_fire(p_ptr->oppose_fire + time);
	      (void)set_oppose_cold(p_ptr->oppose_cold + time);
	      (void)set_oppose_pois(p_ptr->oppose_pois + time);
	      effects[EFFECT_RES_ELEMENT]++;
	      effects[EFFECT_RES_POIS]++;
	      break;
	    }

	  case SPELL_HEROISM:
	    {
	      (void)hp_player(10);
	      (void)set_hero(p_ptr->hero + 
			     randint(25) + 25);
	      (void)set_afraid(0);
	      effects[EFFECT_HEROISM]++;
	      effects[EFFECT_REMOVE_FEAR]++;
	      break;
	    }

	  case SPELL_SHIELD:
	    {
	      (void)set_shield(p_ptr->shield + randint(20) + 30);
	      effects[EFFECT_SHIELD]++;
	      break;
	    }

	  case SPELL_BERSERKER:
	    {
	      (void)hp_player(30);
	      (void)set_shero(p_ptr->shero + 
			      randint(25) + 25);
	      (void)set_afraid(0);
	      effects[EFFECT_BERSERK]++;
	      effects[EFFECT_REMOVE_FEAR]++;
	      break;
	    }

	  case SPELL_ESSENCE_OF_SPEED:
	    {
	      if (!p_ptr->fast)
		{
		  (void)set_fast(randint(30) + 30 + plev);
		}
	      else
		{
		  (void)set_fast(p_ptr->fast + randint(10));
		}
	      effects[EFFECT_HASTE]++;
	      break;
	    }

	  case SPELL_GLOBE_OF_INVULNERABILITY:
	    {
	      (void)set_invuln(p_ptr->invuln + randint(8) + 8);
	      effects[EFFECT_GLOBE_OF_INVULN]++;
	      break;
	    }
	  }
	break;
      }
      case REALM_PRAYER:
      {
	/* Only one of these classes can be chosen */
	if (player_has_class(CLASS_PRIEST, 0))
	  plev = level_of_class(CLASS_PRIEST);
	if (player_has_class(CLASS_CRUSADER, 0))
	  plev = level_of_class(CLASS_CRUSADER);
	if (p_ptr->power_passive == POWER_MEDITATION) plev *= 2;

	/* Spells. */
	switch (spell)
	  {
	  case PRAYER_DETECT_EVIL:
	    {
	      (void)detect_monsters_evil();
	      effects[EFFECT_DETECT_SEMI]++;
	      break;
	    }
	
	  case PRAYER_CURE_LIGHT_WOUNDS:
	    {
	      (void)hp_player(damroll(2, 10));
	      (void)set_cut(p_ptr->cut - 10);
	      effects[EFFECT_CURE_LIGHT_SERIOUS]++;
	      break;
	    }
      
	  case PRAYER_BLESS:
	    {
	      (void)set_blessed(p_ptr->blessed + randint(12) + 12);
	      effects[EFFECT_BLESS]++;
	      break;
	    }

	  case PRAYER_REMOVE_FEAR:
	    {
	      (void)set_afraid(0);
	      effects[EFFECT_REMOVE_FEAR]++;
	      break;
	    }

	  case PRAYER_CALL_LIGHT:
	    {
	      (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
	      effects[EFFECT_LIGHT_AREA]++;
	      break;
	    }

	  case PRAYER_FIND_TRAPS:
	    {
	      (void)detect_traps();
	      effects[EFFECT_DETECT_DOOR_TRAP]++;
	      break;
	    }

	  case PRAYER_DETECT_DOORS_STAIRS:
	    {
	      (void)detect_doors();
	      (void)detect_stairs();
	      effects[EFFECT_DETECT_DOOR_TRAP]++;
	      break;
	    }

	  case PRAYER_SLOW_POISON:
	    {
	      (void)set_poisoned(p_ptr->poisoned / 2);
	      effects[EFFECT_CURE_POISON]++;
	      break;
	    }

	  case PRAYER_SCARE_MONSTER:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      (void)fear_monster(dir, plev);
	      effects[EFFECT_HINDER]++;
	      break;
	    }

	  case PRAYER_PORTAL:
	    {
	      teleport_player(plev * 3);
	      effects[EFFECT_TELEPORT]++;
	      break;
	    }

	  case PRAYER_CURE_SERIOUS_WOUNDS:
	    {
	      (void)hp_player(damroll(4, 10));
	      (void)set_cut((p_ptr->cut / 2) - 20);
	      effects[EFFECT_CURE_LIGHT_SERIOUS]++;
	      break;
	    }

	  case PRAYER_CHANT:
	    {
	      (void)set_blessed(p_ptr->blessed + randint(24) + 24);
	      effects[EFFECT_BLESS]++;
	      break;
	    }

	  case PRAYER_SANCTUARY:
	    {
	      (void)sleep_monsters_touch(p_ptr->current_class);
	      effects[EFFECT_MASS_HINDER]++;
	      break;
	    }

	  case PRAYER_SATISFY_HUNGER:
	    {
	      (void)set_food(PY_FOOD_MAX - 1);
	      effects[EFFECT_SATISFY_HUNGER]++;
	      break;
	    }

	  case PRAYER_REMOVE_CURSE:
	    {
	      remove_curse();
	      effects[EFFECT_DISPEL_REMOVE_CURSE]++;
	      break;
	    }

	  case PRAYER_RESIST_HEAT_COLD:
	    {
	      (void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
	      (void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
	      effects[EFFECT_RES_ELEMENT]++;
	      break;
	    }

	  case PRAYER_NEUTRALIZE_POISON:
	    {
	      (void)set_poisoned(0);
	      effects[EFFECT_CURE_POISON]++;
	      break;
	    }

	  case PRAYER_ORB_OF_DRAINING:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_HOLY_ORB, dir,
			(damroll(3, 6) + plev +
			 (plev / ((p_ptr->pclass[p_ptr->current_class] == CLASS_PRIEST) ? 2 : 4))),
			((plev < 30) ? 2 : 3));
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_HOLY_ORB]++;
	      break;
	    }

	  case PRAYER_CURE_CRITICAL_WOUNDS:
	    {
	      (void)hp_player(damroll(6, 10));
	      (void)set_cut(0);
	      effects[EFFECT_CURE_CRIT_MORTAL]++;
	      break;
	    }

	  case PRAYER_SENSE_INVISIBLE:
	    {
	      (void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
	      effects[EFFECT_DETECT_INVIS]++;
	      break;
	    }

	  case PRAYER_PROTECTION_FROM_EVIL:
	    {
	      (void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev[p_ptr->current_class]);
	      effects[EFFECT_PROTECT_FROM]++;
	      break;
	    }

	  case PRAYER_EARTHQUAKE:
	    {
	      earthquake(py, px, 10);
	      effects[EFFECT_EARTHQUAKE]++;
	      break;
	    }

	  case PRAYER_SENSE_SURROUNDINGS:
	    {
	      map_area();
	      effects[EFFECT_MAPPING]++;
	      break;
	    }

	  case PRAYER_CURE_MORTAL_WOUNDS:
	    {
	      (void)hp_player(damroll(8, 10));
	      (void)set_stun(0);
	      (void)set_cut(0);
	      effects[EFFECT_CURE_CRIT_MORTAL]++;
	      break;
	    }

	  case PRAYER_TURN_UNDEAD:
	    {
	      (void)turn_undead(p_ptr->current_class);
	      effects[EFFECT_MASS_HINDER]++;
	      break;
	    }

	  case PRAYER_PRAYER:
	    {
	      (void)set_blessed(p_ptr->blessed + randint(48) + 48);
	      effects[EFFECT_BLESS]++;
	      break;
	    }

	  case PRAYER_DISPEL_UNDEAD:
	    {
	      (void)dispel_undead(randint(plev * 3));
	      effects[EFFECT_DISPEL_EVIL_UNDEAD]++;
	      break;
	    }

	  case PRAYER_HEAL:
	    {
	      (void)hp_player(300);
	      (void)set_stun(0);
	      (void)set_cut(0);
	      effects[EFFECT_HEAL]++;
	      break;
	    }

	  case PRAYER_DISPEL_EVIL:
	    {
	      (void)dispel_evil(randint(plev * 3));
	      effects[EFFECT_DISPEL_EVIL_UNDEAD]++;
	      break;
	    }

	  case PRAYER_GLYPH_OF_WARDING:
	    {
	      warding_glyph();
	      effects[EFFECT_GLYPH_WARDING]++;
	      break;
	    }

	  case PRAYER_HOLY_WORD:
	    {
	      (void)dispel_evil(randint(plev * 4));
	      (void)hp_player(1000);
	      (void)set_afraid(0);
	      (void)set_poisoned(0);
	      (void)set_stun(0);
	      (void)set_cut(0);
	      effects[EFFECT_DISPEL_EVIL_UNDEAD]++;
	      effects[EFFECT_HEAL]++;
	      effects[EFFECT_REMOVE_FEAR]++;
	      effects[EFFECT_CURE_POISON]++;
	      break;
	    }

	  case PRAYER_DETECT_MONSTERS:
	    {
	      (void)detect_monsters_normal();
	      effects[EFFECT_DETECT_MONSTERS]++;
	      break;
	    }

	  case PRAYER_DETECTION:
	    {
	      (void)detect_all();
	      effects[EFFECT_DETECT_ALL]++;
	      break;
	    }

	  case PRAYER_PERCEPTION:
	    {
	      if (!ident_spell()) return FALSE;
	      effects[EFFECT_IDENTIFY]++;
	      break;
	    }

	  case PRAYER_PROBING:
	    {
	      (void)probing();
	      effects[EFFECT_PROBING]++;
	      break;
	    }

	  case PRAYER_CLAIRVOYANCE:
	    {
	      wiz_lite();
	      effects[EFFECT_WIZ_LITE]++;
	      break;
	    }

	  case PRAYER_CURE_SERIOUS_WOUNDS2:
	    {
	      (void)hp_player(damroll(4, 10));
	      (void)set_cut(0);
	      effects[EFFECT_CURE_LIGHT_SERIOUS]++;
	      break;
	    }

	  case PRAYER_CURE_MORTAL_WOUNDS2:
	    {
	      (void)hp_player(damroll(8, 10));
	      (void)set_stun(0);
	      (void)set_cut(0);
	      effects[EFFECT_CURE_CRIT_MORTAL]++;
	      break;
	    }

	  case PRAYER_HEALING:
	    {
	      (void)hp_player(2000);
	      (void)set_stun(0);
	      (void)set_cut(0);
	      effects[EFFECT_HEAL]++;
	      break;
	    }

	  case PRAYER_RESTORATION:
	    {
	      (void)do_res_stat(A_STR);
	      (void)do_res_stat(A_INT);
	      (void)do_res_stat(A_WIS);
	      (void)do_res_stat(A_DEX);
	      (void)do_res_stat(A_CON);
	      (void)do_res_stat(A_CHR);
	      effects[EFFECT_RESTORE_ATT]++;
	      break;
	    }

	  case PRAYER_REMEMBRANCE:
	    {
	      (void)restore_level();
	      effects[EFFECT_RESTORE_EXP]++;
	      break;
	    }

	  case PRAYER_DISPEL_UNDEAD2:
	    {
	      (void)dispel_undead(randint(plev * 4));
	      effects[EFFECT_DISPEL_EVIL_UNDEAD]++;
	      break;
	    }

	  case PRAYER_DISPEL_EVIL2:
	    {
	      (void)dispel_evil(randint(plev * 4));
	      effects[EFFECT_DISPEL_EVIL_UNDEAD]++;
	      break;
	    }

	  case PRAYER_BANISHMENT:
	    {
	      if (banish_evil(100))
		{
		  msg_print("The power of your god banishes evil!");
		}
	      effects[EFFECT_BANISH]++;
	      break;
	    }

	  case PRAYER_WORD_OF_DESTRUCTION:
	    {
	      destroy_area(py, px, 15, TRUE);
	      effects[EFFECT_DESTRUCTION]++;
	      break;
	    }

	  case PRAYER_ANNIHILATION:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      drain_life(dir, 200);
	      effects[EFFECT_DRAIN_LIFE]++;
	      break;
	    }

	  case PRAYER_UNBARRING_WAYS:
	    {
	      (void)destroy_doors_touch();
	      effects[EFFECT_DOOR_DESTRUCT]++;
	      break;
	    }

	  case PRAYER_RECHARGING:
	    {
	      if (!recharge(15)) return FALSE;
	      effects[EFFECT_RECHARGE]++;
	      break;
	    }

	  case PRAYER_DISPEL_CURSE:
	    {
	      (void)remove_all_curse();
	      effects[EFFECT_DISPEL_REMOVE_CURSE]++;
	      break;
	    }

	  case PRAYER_ENCHANT_WEAPON:
	    {
	      if (!enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0)) return FALSE;
	      effects[EFFECT_ENCHANT_WEAPON]++;
	      break;
	    }

	  case PRAYER_ENCHANT_ARMOUR:
	    {
	      if (!enchant_spell(0, 0, rand_int(3) + 2)) return FALSE;
	      effects[EFFECT_ENCHANT_ARMOUR]++;
	      break;
	    }

	  case PRAYER_ELEMENTAL_BRAND:
	    {
	      brand_weapon();
	      effects[EFFECT_ELEMENTAL_BRAND]++;
	      break;
	    }

	  case PRAYER_BLINK:
	    {
	      teleport_player(10);
	      effects[EFFECT_BLINK]++;
	      break;
	    }

	  case PRAYER_TELEPORT_SELF:
	    {
	      teleport_player(plev * 8);
	      effects[EFFECT_TELEPORT]++;
	      break;
	    }

	  case PRAYER_TELEPORT_OTHER:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      (void)teleport_monster(dir);
	      effects[EFFECT_TELEPORT_OTHER]++;
	      break;
	    }

	  case PRAYER_TELEPORT_LEVEL:
	    {
	      (void)teleport_player_level();
	      effects[EFFECT_ALTER_REALITY]++;
	      break;
	    }

	  case PRAYER_WORD_OF_RECALL:
	    {
	      if (p_ptr->astral)
		{
		  msg_print("You feel a terrible sense of loss.");
		  break;
		}
	      set_recall();
	      effects[EFFECT_RECALL]++;
	      break;
	    }

	  case PRAYER_ALTER_REALITY:
	    {
	      msg_print("The world changes!");

				/* Leaving */
	      p_ptr->leaving = TRUE;

	      effects[EFFECT_ALTER_REALITY]++;
	      break;
	    }
	  }
	break;
      }
      case REALM_ILLUSION:
      {
	/* Only one of these classes can be chosen */
	if (player_has_class(CLASS_ILLUSIONIST, 0))
	  plev = level_of_class(CLASS_ILLUSIONIST);
	
	/* Hack -- chance of "beam" instead of "bolt" */
	beam = ((player_has_class(CLASS_ILLUSIONIST, 0)) ? plev : (plev / 2));

	/* Spells.  */
	switch (spell)
	  {
	  case 0:
	    {
	      /* confusion bolt -KMW- */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_bolt_or_beam(beam-10, GF_CONFUSION, dir,
				damroll(2 + ((plev - 1) / 5), 4));
	      effects[EFFECT_BBOLT]++;
	      effects[EFFECT_CONF]++;
	      break;
	    }

	  case 1: /* detect monsters */
	    {
	      (void)detect_monsters_normal();	
	      effects[EFFECT_DETECT_MONSTERS]++;
	      break;
	    }

	  case 2: /* phase door */
	    {
	      teleport_player(10);
	      effects[EFFECT_BLINK]++;
	      break;
	    }

	  case 3: /* light area */
	    {
	      (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
	      effects[EFFECT_LIGHT_AREA]++;
	      break;
	    }

	  case 4: /* treasure detection */
	    {
	      (void)detect_treasure();
	      (void)detect_objects_gold();
	      effects[EFFECT_DETECT_ITEM]++;
	      break;
	    }

	  case 5:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      (void)fear_monster(dir, plev);
	      effects[EFFECT_HINDER]++;
	      break;
	    }

	  case 6: /* object detection */
	    {
	      (void)detect_objects_normal();
	      effects[EFFECT_DETECT_ITEM]++;
	      break;
	    }

	  case 7: /* find hidden traps/doors */
	    {
	      (void)detect_traps();
	      (void)detect_doors();
	      (void)detect_stairs();
	      effects[EFFECT_DETECT_DOOR_TRAP]++;
	      break;
	    }

	  case 8: /* stinking cloud */
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_POIS, dir,
			10 + (plev / 2), 2);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_POIS]++;
	      break;
	    }

	  case 9:
	    {
	      /* infravision */
	      if (p_ptr->tim_infra == 0)
		set_tim_infra(p_ptr->tim_infra + 200 + randint(100));
	      effects[EFFECT_INFRA]++;
	      break;
	    }

	  case 10:
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      (void)sleep_monster(dir, p_ptr->current_class);
	      effects[EFFECT_HINDER]++;
	      break;
	    }

	  case 11: /* trap/door destruction */
	    {
	      (void)destroy_doors_touch();
	      effects[EFFECT_DOOR_DESTRUCT]++;
	      break;
	    }

	  case 12:
	    {
	      /* fog cloud -KMW- */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_POIS, dir,
			10 + (plev / 2), 3);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_POIS]++;
	      break;
	    }

	  case 13: /* cure poison */
	    {
	      (void)set_poisoned(0);
	      effects[EFFECT_CURE_POISON]++;
	      break;
	    }

	  case 14:
	    {
	      /* satisfy hunger */
	      (void)set_food(PY_FOOD_MAX - 1);
	      effects[EFFECT_SATISFY_HUNGER]++;
	      break;
	    }

	  case 15:
	    {
	      /* shadow door */
	      (void)door_creation();
	      effects[EFFECT_DOOR_CREATION]++;
	      break;
	    }

	  case 16:
	    {
	      /* shadow monster */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_bolt(GF_FORCE, dir,
			damroll(5+((plev-6)/4), 8));
	      effects[EFFECT_BOLT]++;
	      effects[EFFECT_FORCE]++;
	      break;
	    }

	  case 17: /* turn stone to mud */
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      (void)wall_to_mud(dir);
	      effects[EFFECT_STONE_TO_MUD]++;
	      break;
	    }

	  case 18:
	    {
	      /* detect invisible */
	      (void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
	      effects[EFFECT_DETECT_INVIS]++;
	      break;
	    }

	  case 19: /* recharge item */
	    {
	      if (!recharge((plev * 2))) return FALSE;
	      effects[EFFECT_RECHARGE]++;
	      break;
	    }

	  case 20: /* brand ammo */
	    {
	      /* Random enchantment */
	      (void)brand_ammo(0);
	      effects[EFFECT_ELEMENTAL_BRAND]++;
	      break;
	    }

	  case 21:
	    {
	      /* spear of light */
	      if (!get_aim_dir(&dir)) return FALSE;
	      msg_print("A line of blue shimmering light appears.");
	      fire_beam(GF_LITE, dir,
			damroll(2+((plev-5)/4), 6));
	      lite_line(dir);
	      effects[EFFECT_BEAM]++;
	      effects[EFFECT_LIGHT]++;
	      break;
	    }

	  case 22:
	    {
	      /* chaos */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_CHAOS, dir,
			25 + plev, 2);	
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_CHAOS]++;
	      break;
	    }

	  case 23:
	    {
	      /* mental barrier */
	      set_mental_barrier(p_ptr->mental_barrier + 100);
	      msg_print("Your wisdom and intelligence cannot be changed!");
	      effects[EFFECT_MENTAL_BARRIER]++;
	      break;
	    }

	  case 24:
	    {
	      /* true sight */
	      map_area();
	      effects[EFFECT_MAPPING]++;
	      break;
	    }

	  case 25: /* slow monster */
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      (void)slow_monster(dir, p_ptr->current_class);
	      effects[EFFECT_HINDER]++;
	      break;
	    }

	  case 26:
	    {
	      /* shadow ball */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_DARK, dir, 35 + (plev), 2);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_DARK]++;
	    }

	  case 27:
	    {
	      /* bolt of darkness */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_bolt_or_beam(beam, GF_DARK, dir,
				damroll(8+((plev-5)/4), 8));
	      effects[EFFECT_BBOLT]++;
	      effects[EFFECT_DARK]++;
	      break;
	    }

	  case 28:
	    {
	      /* shadow form */
	      /* (void)set_shadow(p_ptr->wraith_form + plev + randint(24)); */
	      msg_print("This spell not currently available");
	      break;
	    }

	  case 29: /* haste self */
	    {
	      if (!p_ptr->fast)
		(void)set_fast(randint(20) + plev);
	      else
		(void)set_fast(p_ptr->fast + randint(5));
	      effects[EFFECT_HASTE]++;
	      break;
	    }

	  case 30:
	    {
	      /* prismatic wall */
	      warding_glyph();
	      effects[EFFECT_GLYPH_WARDING]++;
	      break;
	    }

	  case 31:
	    {
	      /* prismatic spray */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_LITE, 5, 40 + (plev), 2);
	      fire_beam(GF_LITE, dir,
			damroll(8+((plev-5)/4), 8));
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_BEAM]++;
	      effects[EFFECT_LIGHT]++;
	      break;
	    }

	  case 32:
	    {
	      /* chromatic shield */
	      (void)set_shield(p_ptr->shield + randint(30) + 30);
	      effects[EFFECT_SHIELD]++;
	      break;
	    }

	  case 33:
	    {
	      /* wizard lock */
	      /* wizard_lock(); */
	      /* effects[EFFECT_LOCK]++; */
	      msg_print("This spell not currently available");
	      break;
	    }

	  case 34:
	    {
	      /* bedlam */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_CHAOS, dir,
			50 + plev, 10);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_CHAOS]++;
	      break;
	    }

	  case 35:
	    {
	      /* word of recall */
	      if (p_ptr->astral)
		{
		  msg_print("You feel a terrible sense of loss.");
		  break;
		}
	      set_recall();
	      effects[EFFECT_RECALL]++;
	      break;
	    }

	  case 36:
	    {
	      /* detect enchantment */
	      (void)detect_objects_magic();
	      effects[EFFECT_DETECT_ITEM]++;
	      break;
	    }

	  case 37:
	    {
	      /* probing */
	      (void)probing();
	      effects[EFFECT_PROBING]++;
	      break;
	    }

	  case 38:
	    {
	      /* sunfire */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_LITE, dir,
			50 + plev, 10);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_LIGHT]++;
	      break;
	    }

	  case 39: /* the bigbys that are duplicates - warding, slow etc. shoudl act all aroundplayer */
	    {
	      /* Bigby's Interposing Hand */
	      warding_glyph();
	      effects[EFFECT_GLYPH_WARDING]++;
	      break;
	    }

	  case 40:
	    {
	      /* Bigby's Phantom Hand */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_bolt_or_beam(beam, GF_CONFUSION, dir,
				damroll(2+((plev-5)/4), 8));
	      effects[EFFECT_BBOLT]++;
	      effects[EFFECT_CONF]++;
	      break;
	    }

	  case 41:
	    {
	      /* Bigby's Forceful Hand */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_bolt_or_beam(beam, GF_FORCE, dir,
				damroll(6+((plev-5)/4), 8));
	      effects[EFFECT_BBOLT]++;
	      effects[EFFECT_FORCE]++;
	      break;
	    }

	  case 42:
	    {
	      /* Bigby's Grasping Hand */
	      if (!get_aim_dir(&dir)) return FALSE;
	      (void)slow_monster(dir, p_ptr->current_class);
	      effects[EFFECT_HINDER]++;
	      break;
	    }

	  case 43:
	    {
	      /* Bigby's Clenched Fist */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_beam(GF_FORCE, dir,
			damroll(10+((plev-5)/4), 8));
	      effects[EFFECT_BEAM]++;
	      effects[EFFECT_FORCE]++;
	      break;
	    }

	  case 44:
	    {
	      /* Bigby's Crushing Hand */
	      /* want to have this last two turns */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_bolt(GF_GRAVITY, dir,
			damroll(12+((plev-5)/4), 8));
	      effects[EFFECT_BOLT]++;
	      effects[EFFECT_GRAVITY]++;
	      break;
	    }

	  case 45:
	    {
	      /* force blast */
	      fire_ball(GF_FORCE, 5,
			300 + (plev * 2), 3);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_FORCE]++;
	      break;
	    }

	  case 46:
	    {
	      /* Sphere of Light */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_LITE, dir,
			30 + (plev), 2);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_LIGHT]++;
	      break;
	    }

	  case 47:
	    {
	      /* sphere of darkness */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_DARK, dir,
			35 + (plev), 2);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_DARK]++;
	      break;
	    }

	  case 48:
	    {
	      /* sphere of confusion */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_CONFUSION, dir,
			40 + (plev), 2);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_CONF]++;
	      break;
	    }

	  case 49:
	    {
	      /* sphere of chaos */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_CHAOS, dir,
			45 + (plev), 2);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_CHAOS]++;
	      break;
	    }

	  case 50:
	    {
	      /* sphere of sound */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_SOUND, dir,
			50 + (plev), 2);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_SOUND]++;
	      break;
	    }

	  case 51:
	    {
	      /* explosion */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_SHARD, dir,
			80 + (plev), 2);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_SHARD]++;
	      break;
	    }

	  case 52:
	    {
	      /* remove fear */
	      (void)set_afraid(0);
	      effects[EFFECT_REMOVE_FEAR]++;
	      break;
	    }

	  case 53:
	    {
	      /* resist light & dark */
	      (void)set_oppose_ld(p_ptr->oppose_ld + randint(20) + 20);
	      effects[EFFECT_OPPOSE_LD]++;
	      break;	
	    }

	  case 54:
	    {
	      (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
	      effects[EFFECT_RES_POIS]++;
	      break;
	    }

	  case 55:
	    {
	      /* resist chaos & confusion */
	      (void)set_oppose_cc(p_ptr->oppose_cc + randint(20) + 20);
	      effects[EFFECT_OPPOSE_CC]++;
	      break;
	    }

	  case 56:
	    {
	      /* resist sound & shards */
	      (void)set_oppose_ss(p_ptr->oppose_ss + randint(20) + 20);
	      effects[EFFECT_OPPOSE_SS]++;
	      break;
	    }

	  case 57:
	    {
	      /* resist nexus */
	      (void)set_oppose_nex(p_ptr->oppose_nex + randint(20) + 20);
	      effects[EFFECT_OPPOSE_NEX]++;
	      break;
	    }

	  case 58:
	    {
	      /* invisibility */
	      /* (void)set_invis(p_ptr->tim_invis + randint(24) + 24, 30); */
	      msg_print("This spell not currently available");
	      break;	
	    }

	  case 59:
	    {
	      /* shadow monsters */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_DARK, 5, 30 + (plev), 2);
	      fire_beam(GF_FORCE, dir,
			damroll(6+((plev-5)/4), 8));
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_BEAM]++;
	      effects[EFFECT_DARK]++;
	      effects[EFFECT_FORCE]++;
	      break;
	    }

	  case 60:
	    {
	      /* shadow ball */
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_DARK, dir,
			40 + (plev), 2);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_DARK]++;
	      break;
	    }

	  case 61:
	    {
	      /* life for mana */
	      int m = 0;
	      m = get_quantity("How much mana?",999);
	      if (!m) return FALSE;
	      if (m<0) return FALSE;
	      if (m > p_ptr->chp) {
		msg_print("You don't have that much life!");
		return FALSE;
	      }
	      if ((p_ptr->csp + m) > p_ptr->msp)
		m = p_ptr->msp - p_ptr->csp;
	      p_ptr->csp += m;
	      take_hit(m,"spellcasting");
	      msg_print("You convert life into mana.");
	      effects[EFFECT_LIFE_FOR_MANA]++;
	      break;
	    }

	  case 62:
	    {
	      /* shadow gate */
	      teleport_player(plev * 7);
	      effects[EFFECT_TELEPORT]++;
	      break;
	    }

	  case 63:
	    {
	      /* summon shadows */
	      /* summon_monster(SUMMON_SHADOWS); */
	      msg_print("This spell not currently available");
	      break;
	    }

	  }
	break;
      }
      case REALM_DEATH:
      {
	/* Only one of these classes can be chosen */
	if (player_has_class(CLASS_DEATH_PRIEST, 0))
	  plev = level_of_class(CLASS_DEATH_PRIEST);
	if (player_has_class(CLASS_SLAYER, 0))
	  plev = level_of_class(CLASS_SLAYER);
	if (p_ptr->power_passive == POWER_MEDITATION) plev *= 2;

	/* Spells.  */
	switch (spell)
	  {		       
	  case 0: /* Malediction */
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_bolt(GF_NETHER, dir,
			damroll(3 + ((plev - 1) / 5), 4));
	      effects[EFFECT_BOLT]++;
	      effects[EFFECT_NETHER] += 1;
	      break;
	    }
		  
	  case 1: /* Detect Undead */
	    {
	      (void)detect_monsters_undead();
	      effects[EFFECT_DETECT_SEMI]++;
	      break;
	    }

	  case 2: /* Cure Light Wounds */
	    {
	      (void)hp_player(damroll(2, 8));
	      (void)set_cut(p_ptr->cut - 15);
	      effects[EFFECT_CURE_LIGHT_SERIOUS]++;
	      break;
	    }

	  case 3: /* Bless */
	    {
	      (void)set_blessed(p_ptr->blessed + randint(12) + 12);
	      effects[EFFECT_BLESS]++;
	      break;
	    }

	  case 4: /* Remove Fear */
	    {
	      (void)set_afraid(0);
	      effects[EFFECT_REMOVE_FEAR]++;
	      break;
	    }

	  case 5: /* Infravision */
	    {
	      if (p_ptr->tim_infra == 0)
		set_tim_infra(p_ptr->tim_infra + 200 + 
			      randint(100));
	      effects[EFFECT_INFRA]++;
	      break;
	    }

	  case 6: /* Horrify */
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      (void)fear_monster(dir, plev);
	      effects[EFFECT_HINDER]++;
	      break;
	    }

	  case 7: /* Slow Poison */
	    {
	      (void)set_poisoned(p_ptr->poisoned / 2);
	      effects[EFFECT_CURE_POISON]++;
	      break;
	    }

	  case 8: /* Stealth */
	    {
	      if (p_ptr->tim_stealth == 0)
		set_tim_stealth(p_ptr->tim_stealth + 200 + 
				randint(100));
	      effects[EFFECT_STEALTH]++;
	      break;
	    }

	  case 9: /* Call Light */
	    {
	      (void)lite_area(damroll(2, (plev / 2)), 
			      (plev / 10) + 1);
	      effects[EFFECT_LIGHT_AREA]++;
	      break;
	    }

	  case 10: /* Find Hidden Traps/Doors */
	    {
	      (void)detect_traps();
	      (void)detect_doors();
	      (void)detect_stairs();
	      effects[EFFECT_DETECT_DOOR_TRAP]++;
	      break;
	    }

	  case 11: /* Cure Serious Wounds */
	    {
	      (void)hp_player(damroll(4, 10));
	      (void)set_cut((p_ptr->cut / 2) - 20);
	      effects[EFFECT_CURE_LIGHT_SERIOUS]++;
	      break;
	    }

	  case 12: /* Heroism */
	    {
	      (void)hp_player(10);
	      (void)set_hero(p_ptr->hero + randint(25) + 25);
	      (void)set_afraid(0);
	      effects[EFFECT_HEROISM]++;
	      effects[EFFECT_REMOVE_FEAR]++;
	      break;
	    }

	  case 13: /* Sanctuary */
	    {
	      (void)sleep_monsters_touch(p_ptr->current_class);
	      effects[EFFECT_HINDER]++;
	      break;
	    }

	  case 14: /* Res Cold */
	    {
	      (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
	      effects[EFFECT_RES_ELEMENT]++;
	      break;
	    }

	  case 15: /* Noxious Fumes */
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_POIS, dir,
			10 + (plev / 2), 3);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_POIS]++;
	      break;
	    }

	  case 16: /* Break Curse */
	    {
	      remove_curse();
	      effects[EFFECT_DISPEL_REMOVE_CURSE]++;
	      break;
	    }

	  case 17: /* Sustenance */
	    {
	      (void)set_food(PY_FOOD_MAX - 1);
	      effects[EFFECT_SATISFY_HUNGER]++;
	      break;
	    }

	  case 18: /* Neutralize Poison */
	    {
	      (void)set_poisoned(0);
	      effects[EFFECT_CURE_POISON]++;
	      break;
	    }

	  case 19: /* Shield */
	    {
	      (void)set_shield(p_ptr->shield + randint(20) + 30);
	      effects[EFFECT_SHIELD]++;
	      break;
	    }

	  case 20: /* Orb of Enthropy */
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_NETHER, dir,
			(damroll(3, 6) + plev +
			 (plev / 2)),
			((plev < 30) ? 2 : 3));
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_NETHER]++;
	      break;
	    }

	  case 21: /* Cure Critical Wounds */
	    {
	      (void)hp_player(damroll(6, 10));
	      (void)set_cut(0);
	      effects[EFFECT_CURE_CRIT_MORTAL]++;
	      break;
	    }

	  case 22: /* Protection from Undead */
	    {
	      (void)set_prot_undead(p_ptr->prot_undead + 
				    randint(25) + 
				    3 * p_ptr->lev[p_ptr->current_class]);
	      effects[EFFECT_PROTECT_FROM]++;
	      break;
	    }

	  case 23: /* Sense Invisible */
	    {
	      (void)set_tim_invis(p_ptr->tim_invis + 
				  randint(24) + 24);
	      effects[EFFECT_DETECT_INVIS]++;
	      break;
	    }

	  case 24: /* Turn Undead */
	    {
	      (void)turn_undead(p_ptr->current_class);
	      effects[EFFECT_MASS_HINDER]++;
	      break;
	    }

	  case 25: /* Drain Life */
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      drain_life(dir, 50);
	      effects[EFFECT_DRAIN_LIFE]++;
	      break;
	    }

	  case 26: /* Sense Shadows */
	    {
	      map_area();
	      effects[EFFECT_MAPPING]++;
	      break;
	    }

	  case 27: /* Cure Mortal Wounds */
	    {
	      (void)hp_player(damroll(8, 10));
	      (void)set_stun(0);
	      (void)set_cut(0);
	      effects[EFFECT_CURE_CRIT_MORTAL]++;
	      break;
	    }

	  case 28: /* Terror */
	    {
	      (void)scare_monsters(p_ptr->current_class);
	      effects[EFFECT_MASS_HINDER]++;
	      break;
	    }

	  case 29: /* Dispel Undead */
	    {
	      (void)dispel_undead(randint(plev * 3));
	      effects[EFFECT_DISPEL_EVIL_UNDEAD]++;
	      break;
	    }

	  case 30: /* Heal */
	    {
	      (void)hp_player(300);
	      (void)set_stun(0);
	      (void)set_cut(0);
	      effects[EFFECT_HEAL]++;
	      break;
	    }

	  case 31: /* Darkness Storm */
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      fire_ball(GF_DARK, dir, 35 + (plev), 2);
	      effects[EFFECT_BALL]++;
	      effects[EFFECT_DARK]++;
	      break;
	    }

	  case 32: /* Glyph of Warding */
	    {
	      warding_glyph();
	      effects[EFFECT_GLYPH_WARDING]++;
	      break;
	    }

	  case 33: /* Cure Serious Wounds II */
	    {
	      (void)hp_player(damroll(4, 10));
	      (void)set_cut(0);
	      effects[EFFECT_CURE_LIGHT_SERIOUS]++;
	      break;
	    }

	  case 34: /* Cure Mortal Wounds II */
	    {
	      (void)hp_player(damroll(8, 10));
	      (void)set_stun(0);
	      (void)set_cut(0);
	      effects[EFFECT_CURE_CRIT_MORTAL]++;
	      break;
	    }

	  case 35: /* Healing */
	    {
	      (void)hp_player(2000);
	      (void)set_stun(0);
	      (void)set_cut(0);
	      effects[EFFECT_HEAL]++;
	      break;
	    }

	  case 36: /* Restore Life */
	    {
	      (void)restore_level();
	      effects[EFFECT_RESTORE_EXP]++;
	      break;
	    }

	  case 37: /* Restoration */
	    {
	      (void)do_res_stat(A_STR);
	      (void)do_res_stat(A_INT);
	      (void)do_res_stat(A_WIS);
	      (void)do_res_stat(A_DEX);
	      (void)do_res_stat(A_CON);
	      (void)do_res_stat(A_CHR);
	      effects[EFFECT_RESTORE_ATT]++;
	      break;
	    }

	  case 38: /* Dispel Undead II */
	    {
	      (void)dispel_undead(randint(plev * 4));
	      effects[EFFECT_DISPEL_EVIL_UNDEAD]++;
	      break;
	    }

	  case 39: /* Banish Undead */
	    {
	      if (banish_undead(100))
		{
		  msg_print("You banish the undead!");
		}
	      effects[EFFECT_BANISH]++;
	      break;
	    }

	  case 40: /* Genocide */
	    {
	      (void)genocide();
	      effects[EFFECT_GENOCIDE]++;
	      break;
	    }

	  case 41: /* Mass Genocide */
	    {
	      (void)mass_genocide();
	      effects[EFFECT_MASS_GENOCIDE]++;
	      break;
	    }

	  case 42: /* Annihilation */
	    {
	      if (!get_aim_dir(&dir)) return FALSE;
	      drain_life(dir, 200);
	      effects[EFFECT_DRAIN_LIFE]++;
	      break;
	    }

	  case 43: /* Res Lite/Dark */
	    {
	      (void)set_oppose_ld(p_ptr->oppose_ld + 
				  randint(20) + 20);
	      effects[EFFECT_OPPOSE_LD]++;
	      break;	
	    }

	  case 44: /* Invisibility */
	    {
	      /* (void)set_invis(p_ptr->tim_invis + randint(24) + 24, 30); */
	      /* effects[EFFECT_INVIS]++; */
	      msg_print("This prayer not currently available");
	      break;	
	    }

	  case 45: /* Regeneration */
	    {
	      (void)set_tim_regen(p_ptr->tim_regen + 
				  randint(20) + 20);
	      effects[EFFECT_REGEN]++;
	      break;			  
	    }

	  case 46: /* Res Nether */
	    {
	      (void)set_oppose_nether(p_ptr->oppose_nether + 
				      randint(20) + 20);
	      effects[EFFECT_OPPOSE_NETHER]++;
	      break;	
	    }

	  case 47: /* Wraithform */
	    {
	      /* (void)set_shadow(p_ptr->wraith_form + plev + randint(24)); */
	      /* effects[EFFECT_SHADOWFORM]++; */
	      msg_print("This prayer not currently available");
	      break;
	    }

	  case 48: /* Berserk Strength */
	    {
	      (void)hp_player(30);
	      (void)set_shero(p_ptr->shero + randint(25) + 25);
	      (void)set_afraid(0);
	      effects[EFFECT_BERSERK]++;
	      effects[EFFECT_REMOVE_FEAR]++;
	      break;
	    }

	  case 49: /* Invulnerability */
	    {
	      (void)set_invuln(p_ptr->invuln + randint(8) + 8);
	      effects[EFFECT_GLOBE_OF_INVULN]++;
	      break;
	    }

	  case 50: /* Recharging */
	    {
	      if (!recharge(15)) return FALSE;
	      effects[EFFECT_RECHARGE]++;
	      break;
	    }

	  case 51: /* Dispel Curse */
	    {
	      (void)remove_all_curse();
	      effects[EFFECT_DISPEL_REMOVE_CURSE]++;
	      break;
	    }

	  case 52: /* Enchant Weapon */
	    {
	      if (!enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0)) return FALSE;
	      effects[EFFECT_ENCHANT_WEAPON]++;
	      break;
	    }

	  case 53: /* Enchant Armour */
	    {
	      if (!enchant_spell(0, 0, rand_int(3) + 2)) return FALSE;
	      effects[EFFECT_ENCHANT_ARMOUR]++;
	      break;
	    }

	  case 54: /* Poison Weapon */
	    {
	      (void)poison_weapon();
	      effects[EFFECT_POISON_WEAPON]++;
	      break;
	    }

	  case 55: /* Shadow Door */
	    {
	      teleport_player(10);
	      effects[EFFECT_BLINK]++;
	      break;
	    }

	  case 56: /* Unbarring Ways */
	    {
	      (void)destroy_doors_touch();
	      effects[EFFECT_DOOR_DESTRUCT]++;
	      break;
	    }

	  case 57: /* Perception */
	    {
	      if (!ident_spell()) return FALSE;
	      effects[EFFECT_IDENTIFY]++;
	      break;
	    }

	  case 58: /* Probing */
	    {
	      (void)probing();
	      effects[EFFECT_PROBING]++;
	      break;
	    }

	  case 59: /* Word of Recall */
	    {
	      if (p_ptr->astral)
		{
		  msg_print("You feel a terrible sense of loss.");
		  break;
		}
	      set_recall();
	      effects[EFFECT_RECALL]++;
	      break;
	    }
	  }
	     
	break;
      }
  }

  return TRUE;
}

/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int item, sval, spell;
	int chance;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;


	/* Require spell ability */
	if (mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book != TV_MAGIC_BOOK)
	{
		msg_print("You cannot cast spells!");
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
	item_tester_tval = mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book;

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

	/* Get the item's sval */
	sval = o_ptr->sval;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Ask for a spell */
	if (!get_spell(&spell, "cast", sval, TRUE))
	{
		if (spell == -2) msg_print("You don't know any spells in that book.");
		return;
	}


	/* Get the spell */
	s_ptr = &mp_ptr[p_ptr->pclass[p_ptr->current_class]]->info[spell];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

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
	        cast_spell(REALM_MAGIC, spell);

		/* A spell was cast */
		if (!((spell < 32) ?
		      (p_ptr->spell_worked1[0] & (1L << spell)) :
		      (p_ptr->spell_worked2[0] & (1L << (spell - 32)))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (spell < 32)
			{
				p_ptr->spell_worked1[0] |= (1L << spell);
			}
			else
			{
				p_ptr->spell_worked2[0] |= (1L << (spell - 32));
			}

			/* Gain experience */
			if (player_has_class(CLASS_MAGE, 0))
			  gain_exp(e * s_ptr->slevel, index_of_class(CLASS_MAGE));

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
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




/*
 * Pray a prayer
 */
void do_cmd_pray(void)
{
	int item, sval, spell, chance;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;

	/* Must use prayer books */
	if (mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book != TV_PRAYER_BOOK)
	{
		msg_print("Pray hard enough and your prayers may be answered.");
		return;
	}

	/* Must have lite */
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	/* Must not be confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}


	/* Restrict choices */
	item_tester_tval = mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no prayer books!";
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

	/* Get the item's sval */
	sval = o_ptr->sval;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Choose a spell */
	if (!get_spell(&spell, "recite", sval, TRUE))
	{
		if (spell == -2) msg_print("You don't know any prayers in that book.");
		return;
	}


	/* Get the spell */
	s_ptr = &mp_ptr[p_ptr->pclass[p_ptr->current_class]]->info[spell];


	/* Verify "dangerous" prayers */
	if (player_has_class(CLASS_CRUSADER, 0) && (s_ptr->smana > p_ptr->mpp))
	{
		/* Warning */
		msg_print("You do not have enough piety to recite this prayer.");

		/* Flush input */
		flush();

		/* No chance */
		return;
	}
	/* Verify "dangerous" prayers */
	else if (s_ptr->smana > p_ptr->cpp)
	{
		/* Warning */
		msg_print("You do not have enough piety to recite this prayer.");

		/* Flush input */
		flush();

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
	chance = spell_chance(spell);

	/* Check for failure */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print("You failed to concentrate hard enough!");
	}

	/* Success */
	else
	{
	        /* Cast the spell */
	        cast_spell(REALM_PRAYER, spell);

		/* A prayer was prayed */
		if (!((spell < 32) ?
		      (p_ptr->spell_worked1[1] & (1L << spell)) :
		      (p_ptr->spell_worked2[1] & (1L << (spell - 32)))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (spell < 32)
			{
				p_ptr->spell_worked1[1] |= (1L << spell);
			}
			else
			{
				p_ptr->spell_worked2[1] |= (1L << (spell - 32));
			}

			/* Gain experience */
			if (player_has_class(CLASS_PRIEST, 0))
			  gain_exp(e * s_ptr->slevel, index_of_class(CLASS_PRIEST));
			if (player_has_class(CLASS_CRUSADER, 0))
			  gain_exp(e * s_ptr->slevel, index_of_class(CLASS_CRUSADER));

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Sufficient mana */
	if (player_has_class(CLASS_CRUSADER, 0))
	{
	     p_ptr->mpp -= s_ptr->smana;
	     p_ptr->cpp = p_ptr->mpp;
	}
	else if (s_ptr->smana <= p_ptr->cpp)
	{
		/* Use some mana */
		p_ptr->cpp -= s_ptr->smana;
	}

	/* Over-exert the player */
	else
	{
		int oops = s_ptr->smana - p_ptr->cpp;

		/* No mana left */
		p_ptr->cpp = 0;
		p_ptr->cpp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage WIS (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You feel less wise!");

			/* Reduce wisdom */
			(void)dec_stat(A_WIS, 15 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

/*
 * Cast an illusion spell
 */
void do_cmd_cast_illusion(void)
{
	int item, sval, spell;
	int chance;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;

	/* Require spell ability */
	if (mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book != TV_ILLUSION_BOOK)
	{
		msg_print("You cannot cast spells!");
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
	item_tester_tval = mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book;

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

	/* Get the item's sval */
	sval = o_ptr->sval;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Ask for a spell */
	if (!get_spell(&spell, "cast", sval, TRUE))
	{
		if (spell == -2) msg_print("You don't know any spells in that book.");
		return;
	}


	/* Get the spell */
	s_ptr = &mp_ptr[p_ptr->pclass[p_ptr->current_class]]->info[spell];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

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
	        cast_spell(REALM_ILLUSION, spell);

		/* A spell was cast */
		if (!((spell < 32) ?
		      (p_ptr->spell_worked1[0] & (1L << spell)) :
		      (p_ptr->spell_worked2[0] & (1L << (spell - 32)))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (spell < 32)
			{
				p_ptr->spell_worked1[0] |= (1L << spell);
			}
			else
			{
			        p_ptr->spell_worked2[0] |= (1L << (spell - 32));
			}

			/* Gain experience */
			if (player_has_class(CLASS_ILLUSIONIST, 0))
			  gain_exp(e * s_ptr->slevel, 
				   index_of_class(CLASS_ILLUSIONIST));

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
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


/*
 * Cast a death spell
 */
void do_cmd_cast_death(void)
{
	int item, sval, spell;
	int chance;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;

	/* Require spell ability */
	if (mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book != TV_DEATH_BOOK)
	{
		msg_print("Pray hard enough and your prayers may be answered.");
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
	item_tester_tval = mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no prayer books!";
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

	/* Get the item's sval */
	sval = o_ptr->sval;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Ask for a spell */
	if (!get_spell(&spell, "cast", sval, TRUE))
	{
		if (spell == -2) msg_print("You don't know any prayers in that book.");
		return;
	}


	/* Get the spell */
	s_ptr = &mp_ptr[p_ptr->pclass[p_ptr->current_class]]->info[spell];


	/* Verify "dangerous" spells */
	if (player_has_class(CLASS_SLAYER, 0) && (s_ptr->smana > p_ptr->mpp))
	{
		/* Warning */
		msg_print("You do not have enough piety to recite this prayer.");

		/* Flush input */
		flush();

		/* No chance */
		return;
	}
	else if (s_ptr->smana > p_ptr->cpp)
	{
		/* Warning */
		msg_print("You do not have enough piety to recite this prayer.");

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
		msg_print("You failed to concentrate hard enough!");
	}

	/* Process spell */
	else
	{
	     /* Cast the spell */
	     cast_spell(REALM_DEATH, spell);

	     /* A spell was cast */
	     if (!((spell < 32) ?
		   (p_ptr->spell_worked1[1] & (1L << spell)) :
		   (p_ptr->spell_worked2[1] & (1L << (spell - 32)))))
	     {
			int e = s_ptr->sexp;

			/* The spell worked */
			if (spell < 32)
			{
				p_ptr->spell_worked1[1] |= (1L << spell);
			}
			else
			{
				p_ptr->spell_worked2[1] |= (1L << (spell - 32));
			}

			/* Gain experience */
			if (player_has_class(CLASS_DEATH_PRIEST, 0))
			  gain_exp(e * s_ptr->slevel, 
				   index_of_class(CLASS_DEATH_PRIEST));
			if (player_has_class(CLASS_SLAYER, 0))
			  gain_exp(e * s_ptr->slevel, 
				   index_of_class(CLASS_SLAYER));

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
	     }
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Sufficient mana */
	if (player_has_class(CLASS_SLAYER, 0))
	{
	     p_ptr->mpp -= s_ptr->smana;
	     p_ptr->cpp = p_ptr->mpp;
	}
	else if (s_ptr->smana <= p_ptr->cpp)
	{
		/* Use some mana */
		p_ptr->cpp -= s_ptr->smana;
	}

	/* Over-exert the player */
	else
	{
		int oops = s_ptr->smana - p_ptr->cpp;

		/* No mana left */
		p_ptr->cpp = 0;
		p_ptr->cpp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage WIS (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You feel less wise!");

			/* Reduce constitution */
			(void)dec_stat(A_WIS, 15 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

/* Crusaders */
struct power {
     int min_lev;
     cptr name;
     bool active;
     bool crusader;
     bool slayer;
};

struct power powers[MAX_POWERS] =
{
     /* Level, name, is it active rather than passive, can crusaders use it, can slayers use it */
     {  1, "Blessing",             FALSE, TRUE,  TRUE },
     {  2, "Stealth",              FALSE, FALSE, TRUE },
     {  3, "Heroism",              FALSE, TRUE,  TRUE },
     {  5, "Dark Vision",          FALSE, FALSE, TRUE },
     {  5, "Weapon : Light",       TRUE,  TRUE,  FALSE },
     {  7, "Berserk Strength",     FALSE, TRUE,  TRUE },
     {  9, "Shield",               FALSE, TRUE,  TRUE },
     { 12, "Regeneration",         FALSE, TRUE,  TRUE },
     { 15, "Weapon : Cold",        TRUE,  FALSE, TRUE },
     { 15, "Weapon : Flame",       TRUE,  TRUE,  FALSE },
     { 18, "Meditation",           FALSE, TRUE,  TRUE },
     { 21, "Resistance",           FALSE, TRUE,  FALSE },
     { 21, "Weapon : Dark",        TRUE,  FALSE, TRUE },
     { 24, "Resistance",           FALSE, FALSE, TRUE },
     { 27, "Haste Self",           FALSE, TRUE,  TRUE },
     { 30, "Weapon : Slay Undead", TRUE,  TRUE,  FALSE },
     { 30, "Weapon : Poison",      TRUE,  FALSE, TRUE },
     { 35, "Protection from Evil", FALSE, TRUE,  FALSE },
     { 35, "Protection from Animals", FALSE, FALSE, TRUE },
     { 40, "Weapon : Slay Evil",   TRUE,  TRUE,  FALSE },
     { 40, "Weapon : Slay Animal", TRUE,  FALSE, TRUE },
};

static int get_power(int *sn)
{
        int             i, ask, num = 0, y = 1, x = 20;
        int             plev = level_of_class(CLASS_CRUSADER);
	int             temp[MAX_POWERS];
	char		choice;
	char            out_val[160];
	byte            line_attr;

	/* Nothing chosen yet */
	bool flag = FALSE;

	/* No redraw yet */
	bool redraw = FALSE;

#ifdef ALLOW_REPEAT

	/* Get the spell, if available */
	if (repeat_pull(sn))
	     return (TRUE);

#endif /* ALLOW_REPEAT */

	/* Assume cancelled */
	(*sn) = -1;

	/* Only one of these clases can be chosen */
	if (player_has_class(CLASS_CRUSADER, 0)) plev = level_of_class(CLASS_CRUSADER);
	if (player_has_class(CLASS_SLAYER, 0)) plev = level_of_class(CLASS_SLAYER);

	for (i = 0; i < MAX_POWERS; i++)
	{
	     if (powers[i].min_lev <= plev)
	     {
		  if (powers[i].crusader && player_has_class(CLASS_CRUSADER, 0))
		  {
		       temp[num] = i;
		       num++;
		  }
		  else if (powers[i].slayer && player_has_class(CLASS_SLAYER, 0))
		  {
		       temp[num] = i;
		       num++;
		  }
	     }
	}

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
						I2A(0), I2A(num - 1));
	
	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				char power_desc[80];

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				screen_save();

				/* Display a list of spells */
				prt("", y, x);
				put_str("Name", y, x + 5);
				put_str("Lv", y, x + 35);

				/* Dump the spells */
				for (i = 0; i < num; i++)
				{
					/* Access the spell */
					if (powers[temp[i]].min_lev > plev) break;
					
					/* Color */
					line_attr = TERM_WHITE;

					if (powers[temp[i]].active)
					{
					     if (temp[i] == p_ptr->power_active) 
						  line_attr = TERM_L_BLUE;
					     else line_attr = TERM_BLUE;
					}
					else
					{
					     if (temp[i] == p_ptr->power_passive) 
						  line_attr = TERM_L_GREEN;
					     else line_attr = TERM_GREEN;
					}

					/* Dump the spell */
					sprintf(power_desc, "  %c) %-30s%2d",
						I2A(i), powers[temp[i]].name, 
						powers[temp[i]].min_lev);
					c_prt(line_attr, power_desc, y + i + 1, x);
				}

				/* Clear the bottom line */
				prt("", y + i + 1, x);
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
			bell("Illegal power choice!");
			continue;
		}

	        /* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", powers[temp[i]].name);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) screen_load();

	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = temp[i];
	
#ifdef ALLOW_REPEAT

	repeat_push(*sn);

#endif /* ALLOW_REPEAT */

	/* Success */
	return (TRUE);	
}

/* Use a power */
void do_cmd_power(void)
{
     int power;
     /* Save last power */
     byte last_active = p_ptr->power_active;
     byte last_passive = p_ptr->power_passive;
     /* Notice changes? */
     bool notice = FALSE;

     /* Ask for a power */
     if (!get_power(&power)) return;

     /* Use the power */
     switch (power)
     {
     case POWER_BLESSING:
	  p_ptr->power_passive = POWER_BLESSING;
	  effects[EFFECT_BLESS]++;
	  break;
     case POWER_STEALTH:
	  p_ptr->power_passive = POWER_STEALTH;
	  effects[EFFECT_STEALTH]++;
	  break;
     case POWER_HEROISM:
	  p_ptr->power_passive = POWER_HEROISM;
	  if (p_ptr->afraid) {
	       (void)set_afraid(0);
	       effects[EFFECT_REMOVE_FEAR]++;
	  }
	  effects[EFFECT_HEROISM]++;
	  break;
     case POWER_VISION:
	  p_ptr->power_passive = POWER_VISION;
	  effects[EFFECT_INFRA]++;
	  effects[EFFECT_DETECT_INVIS]++;
	  break;
     case POWER_WPN_LIGHT:
	  p_ptr->power_active = POWER_WPN_LIGHT;
	  break;
     case POWER_BERSERK:
	  p_ptr->power_passive = POWER_BERSERK;
	  if (p_ptr->afraid) {
	       (void)set_afraid(0);
	       effects[EFFECT_REMOVE_FEAR]++;
	  }
	  effects[EFFECT_BERSERK]++;
	  break;
     case POWER_SHIELD:
	  p_ptr->power_passive = POWER_SHIELD;
	  effects[EFFECT_SHIELD]++;
	  break;
     case POWER_REGEN:
	  p_ptr->power_passive = POWER_REGEN;
	  effects[EFFECT_REGEN]++;
	  break;
     case POWER_WPN_COLD:
	  p_ptr->power_active = POWER_WPN_COLD;
	  break;
     case POWER_WPN_FLAME:
	  p_ptr->power_active = POWER_WPN_FLAME;
	  break;
     case POWER_HASTE:
	  p_ptr->power_passive = POWER_HASTE;
	  effects[EFFECT_HASTE]++;
	  break;
     case POWER_MEDITATION:
	  p_ptr->power_passive = POWER_MEDITATION;
	  break;
     case POWER_RESISTANCE1:
	  /* The crusader version gives res elements and poison */
	  p_ptr->power_passive = POWER_RESISTANCE1;
	  effects[EFFECT_RES_ELEMENT]++;
	  if (level_of_class(CLASS_CRUSADER) >= 24) effects[EFFECT_RES_POIS]++;
	  break;
     case POWER_WPN_DARK:
	  p_ptr->power_active = POWER_WPN_DARK;
	  effects[EFFECT_OPPOSE_LD]++;
	  break;
     case POWER_RESISTANCE2:
	  /* The slayer version gives res dark, cold, fear, poison and nether */
	  p_ptr->power_passive = POWER_RESISTANCE2;
	  (void)set_afraid(0);
	  effects[EFFECT_REMOVE_FEAR]++;
	  effects[EFFECT_OPPOSE_LD]++;
	  effects[EFFECT_RES_ELEMENT]++;
	  effects[EFFECT_RES_POIS]++;
	  if (level_of_class(CLASS_SLAYER) >= 29) effects[EFFECT_OPPOSE_NETHER]++;
	  break;
     case POWER_SLAY_UNDEAD:
	  p_ptr->power_active = POWER_SLAY_UNDEAD;
	  break;
     case POWER_WPN_POISON:
	  p_ptr->power_active = POWER_WPN_POISON;
	  break;
     case POWER_PROT_EVIL:
	  p_ptr->power_passive = POWER_PROT_EVIL;
	  effects[EFFECT_PROTECT_FROM]++;
	  break;
     case POWER_PROT_ANIMAL:
	  p_ptr->power_passive = POWER_PROT_ANIMAL;
	  effects[EFFECT_PROTECT_FROM]++;
	  break;
     case POWER_SLAY_EVIL:
	  p_ptr->power_active = POWER_SLAY_EVIL;
	  break;
     case POWER_SLAY_ANIMAL:
	  p_ptr->power_active = POWER_SLAY_ANIMAL;
	  break;
     }

     /* Notice if active power changes */
     if (p_ptr->power_active != last_active)
     {
	  cptr open = ((player_has_class(CLASS_MONK, 0)) ? "hands start" : "weapon starts");
	  cptr close = ((player_has_class(CLASS_MONK, 0)) ? "hands stop" : "weapon stops");

	  /* Notice ending of last power */
	  switch (last_active)
	  {
	  case POWER_WPN_LIGHT:
	       msg_format("Your %s shining.", close);
	       break;
	  case POWER_WPN_COLD:
	       msg_format("Your %s radiating cold.", close);
	       break;
	  case POWER_WPN_FLAME:
	       msg_format("Your %s burning.", close);
	       break;
	  case POWER_WPN_DARK:
	       msg_format("You %s absorbing light.", close);
	       break;
	  case POWER_SLAY_UNDEAD:
	       msg_format("Your %s being more effective against undead.", close);
	       break;
	  case POWER_WPN_POISON:
	       msg_format("Your %s dripping with poison.", close);
	       break;
	  case POWER_SLAY_EVIL:
	       msg_format("Your %s being more effective against evil.", close);
	       break;
	  case POWER_SLAY_ANIMAL:
	       msg_format("Your %s being more effective against animals.", close);
	       break;
	  }
	  /* Notice beginning of new power */
	  switch (p_ptr->power_active)
	  {
	  case POWER_WPN_LIGHT:
	       msg_format("Your %s shining!", open);
	       break;
	  case POWER_WPN_COLD:
	       msg_format("Your %s radiating cold!", open);
	       break;
	  case POWER_WPN_FLAME:
	       msg_format("Your %s burning!", open);
	       break;
	  case POWER_WPN_DARK:
	       msg_format("You %s absorbing light.", open);
	       break;
	  case POWER_SLAY_UNDEAD:
	       msg_format("Your %s being more effective against undead!", open);
	       break;
	  case POWER_WPN_POISON:
	       msg_format("Your %s dripping with poison!", open);
	       break;
	  case POWER_SLAY_EVIL:
	       msg_format("Your %s being more effective against evil!", open);
	       break;
	  case POWER_SLAY_ANIMAL:
	       msg_format("Your %s being more effective against animals!", open);
	       break;
	  }

	  notice = TRUE;
     }

     /* Notice if passive power changes */
     if (p_ptr->power_passive != last_passive)
     {
	  /* Notice ending of last power */
	  switch (last_passive)
	  {
	  case POWER_BLESSING:
	       msg_print("The prayer has expired.");
	       break;
	  case POWER_STEALTH:
	       msg_print("You become less stealthy.");
	       break;
	  case POWER_HEROISM:
	       msg_print("The heroism wears off.");
	       break;
	  case POWER_VISION:
	       msg_print("Your vision dims.");
	       break;
	  case POWER_BERSERK:
	       msg_print("You feel less Berserk.");
	       break;
	  case POWER_SHIELD:
	       msg_print("Your mystic shield crumbles away.");
	       break;
	  case POWER_REGEN:
	       msg_print("You stop regenerating.");
	       break;
	  case POWER_HASTE:
	       msg_print("You feel yourself slow down.");
	       break;
	  case POWER_MEDITATION:
	       msg_print("You stop meditating.");
	       break;
	  case POWER_RESISTANCE1:
	       msg_print("You feel less resistant to the elements.");
	       if (level_of_class(CLASS_CRUSADER) >= 24)
		    msg_print("You feel less resistant to poison.");
	       break;
	  case POWER_RESISTANCE2:
	       if (level_of_class(CLASS_SLAYER) >= 29)
		    msg_print("You feel less resistant to fear, darkness, cold, poison and nether.");
	       else
		    msg_print("You feel less resistant to fear, darkness, cold and poison.");
	       break;
	  case POWER_PROT_EVIL:
	       msg_print("You no longer feel safe from evil.");
	       break;
	  case POWER_PROT_ANIMAL:
	       msg_print("You no longer feel safe from animals.");
	       break;
	  }
	  /* Notice beginning of new power */
	  switch (p_ptr->power_passive)
	  {
	  case POWER_BLESSING:
	       msg_print("You feel righteous!");
	       break;
	  case POWER_STEALTH:
	       msg_print("You become more stealthy.");
	       break;
	  case POWER_HEROISM:
	       msg_print("You feel like a hero!");
	       break;
	  case POWER_VISION:
	       msg_print("Your eyes pierce the darkness!");
	       break;
	  case POWER_BERSERK:
	       msg_print("You feel like a killing machine!");
	       break;
	  case POWER_SHIELD:
	       msg_print("A mystic shield forms around your body!");
	       break;
	  case POWER_REGEN:
	       msg_print("You start regenerating!");
	       break;
	  case POWER_HASTE:
	       msg_print("You feel yourself moving faster!");
	       break;
	  case POWER_MEDITATION:
	       msg_print("You start meditating.");
	       break;
	  case POWER_RESISTANCE1:
	       msg_print("You feel resistant to the elements!");
	       if (level_of_class(CLASS_CRUSADER) >= 24)
		    msg_print("You feel resistant to poison!");
	       break;
	  case POWER_RESISTANCE2:
	       if (level_of_class(CLASS_SLAYER) >= 29)
		    msg_print("You feel resistant to fear, darkness, cold, poison and nether!");
	       else
		    msg_print("You feel resistant to fear, darkness, cold and poison!");
	       break;
	  case POWER_PROT_EVIL:
	       msg_print("You feel safe from evil!");
	       break;
	  case POWER_PROT_ANIMAL:
	       msg_print("You feel safe from animals!");
	       break;
	  }

	  notice = TRUE;
     }

     if (notice) 
     {
	  /* Disturb */
	  if (disturb_state) disturb(0, 0);

	  /* Recalculate bonuses */
	  p_ptr->update |= (PU_BONUS);

	  /* Handle stuff */
	  handle_stuff();

	  /* Take a turn */
	  p_ptr->energy_use = 100;
     }

     /* Window stuff */
     p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}



/*** Shifters ***/

struct shifter_power {
     int min_lev;
     cptr name;
     bool type; /* Is it a shift (TRUE) or a power (FALSE) */
     int form;  /* If a power, which form is required? If a shift, which is it? */
     int cost;  /* If a power, then mana cost. If a shift, then chance of failure */
};

cptr shifter_forms[MAX_SHIFTER_FORMS-1] = {
  "Giant Spider", "Tengu", "Arctic Bear", "Wyvern", "Ent", "Umber Hulk", "Gorgon", "Phase Spider",
  "Mind Flayer", "Colbran", "Ice Troll", "Beholder", "Vampire", "Chaos Drake"
};

struct shifter_power shifter_powers[MAX_SHIFTER_POWERS] =
{
     {  1,     "Unshift",       TRUE,  0,                  0 },
     /* Giant Spider : res pois, dex +1, stealth +3 */     
     {  1,     "Giant Spider",  TRUE,  FORM_GIANT_SPIDER, 10 },
     {  1, "Sting",            FALSE,  FORM_GIANT_SPIDER,  1 },
     {  1, "Stinking Cloud",   FALSE,  FORM_GIANT_SPIDER,  3 },
     {  1, "Resist Poison",    FALSE,  FORM_GIANT_SPIDER, 12 }, 
     /* Tengu : speed +5, res conf, dex +2, teleportitus */
     {  4,            "Tengu",  TRUE,  FORM_TENGU,        15 },
     {  4, "Phase Door",       FALSE,  FORM_TENGU,         2 },
     {  4, "Teleport Self",    FALSE,  FORM_TENGU,         9 },
     /* Arctic Bear : str +2, res cold, res fear, claws +2 */
     {  7,      "Arctic Bear",  TRUE,  FORM_ARCTIC_BEAR,  20 },
     {  7, "Heroism",          FALSE,  FORM_ARCTIC_BEAR,   3 },
     {  7, "Resist Cold",      FALSE,  FORM_ARCTIC_BEAR,   6 }, 
     {  7, "Berserk Strength", FALSE,  FORM_ARCTIC_BEAR,  12 },
     /* Wyvern : int +2, feather fall, res fire, claws +3 */
     { 10,           "Wyvern",  TRUE,  FORM_WYVERN,       30 },
     { 10, "Light Area",       FALSE,  FORM_WYVERN,        2 },
     { 10, "Resist Fire",      FALSE,  FORM_WYVERN,        6 }, 
     { 10, "Breathe Fire",     FALSE,  FORM_WYVERN,       15 },
     /* Ent : str +2, slow -5, slow digestion, susteptible to fire */
     { 13,              "Ent",  TRUE,  FORM_ENT,          40 },
     { 13, "First Aid",        FALSE,  FORM_ENT,           3 },
     { 13, "Satisfy Hunger",   FALSE,  FORM_ENT,           6 },
     { 13, "Herbal Healing",   FALSE,  FORM_ENT,           9 },
     { 13, "Entangle",         FALSE,  FORM_ENT,          25 },
     /* Umber Hulk : free act, con +3, claws +6 */
     { 16,       "Umber Hulk",  TRUE,  FORM_UMBER_HULK,   50 },
     { 16, "Confuse Monster",  FALSE,  FORM_UMBER_HULK,    2 },
     { 16, "Stone to Mud",     FALSE,  FORM_UMBER_HULK,    9 },
     { 16, "Earthquake",       FALSE,  FORM_UMBER_HULK,   20 },
     /* Gorgon : res acid, int +3, sust int */
     { 19,           "Gorgon",  TRUE,  FORM_GORGON,       60 },
     { 19, "Resist Acid",      FALSE,  FORM_GORGON,        6 }, 
     { 19, "Sleep Monster",    FALSE,  FORM_GORGON,        9 },
     { 19, "Breathe Acid",     FALSE,  FORM_GORGON,       15 },
     /* Phase Spider : dex +3, sust dex, free act, speed +5, res pois, stealth +2 */
     { 22,     "Phase Spider",  TRUE,  FORM_PHASE_SPIDER, 65 },
     { 22, "Sting",            FALSE,  FORM_PHASE_SPIDER,  1 },
     { 22, "Phase Door",       FALSE,  FORM_PHASE_SPIDER,  2 },
     { 22, "Haste Self",       FALSE,  FORM_PHASE_SPIDER, 15 },
     /* Mind Flayer : int +5, sust int, res conf, telepathy, str -2, con -2 */
     { 25,      "Mind Flayer",  TRUE,  FORM_MINDFLAYER,   70 },
     { 25, "Confuse Monster",  FALSE,  FORM_MINDFLAYER,    2 },
     { 25, "Scare",            FALSE,  FORM_MINDFLAYER,    6 },
     { 25, "Teleport Self",    FALSE,  FORM_MINDFLAYER,    9 },
     { 25, "Mind Blast",       FALSE,  FORM_MINDFLAYER,   15 },
     /* Colbran : res elec, str +3, res shards, monks get brand elec */
     { 28,          "Colbran",  TRUE,  FORM_COLBRAN,      75 },
     { 28, "Lightning Bolt",   FALSE,  FORM_COLBRAN,       3 },
     { 28, "Resist Electricity",FALSE, FORM_COLBRAN,       6 }, 
     { 28, "Lightning Ball",   FALSE,  FORM_COLBRAN,      12 },
     { 28, "Haste Self",       FALSE,  FORM_COLBRAN,      15 },
     /* Ice Troll : str +5, sust str, regen, res fear, im cold, monks get brand cold */
     { 31,        "Ice Troll",  TRUE,  FORM_ICE_TROLL,    80 },
     { 31, "Slow Monster",     FALSE,  FORM_ICE_TROLL,     9 },
     { 31, "Shield",           FALSE,  FORM_ICE_TROLL,    12 }, 
     { 31, "Ice Bolt",         FALSE,  FORM_ICE_TROLL,    15 },
     /* Beholder : res blind, searching +5, see invis, slow -10 */
     { 34,         "Beholder",  TRUE,  FORM_BEHOLDER,     85 },
     { 34, "Detect Monsters",  FALSE,  FORM_BEHOLDER,      1 }, 
     { 34, "Magic Mapping",    FALSE,  FORM_BEHOLDER,      9 }, 
     { 34, "Teleport Other",   FALSE,  FORM_BEHOLDER,     12 },
     { 34, "Confusion Ball",   FALSE,  FORM_BEHOLDER,     20 },
     { 34, "Terror",           FALSE,  FORM_BEHOLDER,     25 },
     { 34, "Mass Sleep",       FALSE,  FORM_BEHOLDER,     30 },
     /* Vampire : res cold, res dark, res pois, res nether, hold life, regen, see invis */
     { 37,          "Vampire",  TRUE,  FORM_VAMPIRE,      90 },
     { 37, "Scare Monster",    FALSE,  FORM_VAMPIRE,       6 },
     { 37, "Drain Life",       FALSE,  FORM_VAMPIRE,      15 },
     { 37, "Nether Bolt",      FALSE,  FORM_VAMPIRE,      25 },
     { 37, "Life for Mana",    FALSE,  FORM_VAMPIRE,      30 }, 
     /* Chaos Drake : res conf, res chaos, res nexus, im fire, feather fall */
     { 40,      "Chaos Drake",  TRUE,  FORM_CHAOS_DRAKE,  95 },
     { 40, "Polymorph Monster",FALSE,  FORM_CHAOS_DRAKE,  20 },
     { 40, "Chaos Bolt",       FALSE,  FORM_CHAOS_DRAKE,  30 },
     { 40, "Nexus Bolt",       FALSE,  FORM_CHAOS_DRAKE,  40 },
};

static void shifter_info(char *p, int power)
{
     int plev = level_of_class(CLASS_SHIFTER);

     /* Default */
     strcpy(p, "");

     /* Hide spell information */
     if (adult_hidden) return;

     switch (power)
     {
     case SHIFTER_STING: case SHIFTER_STING2: 
	  sprintf(p, " dam %dd4", 3 + ((plev - 1) / 5)); break;
     case SHIFTER_STINKING_CLOUD: 
	  sprintf(p, " dam %d", 10 + (plev / 2)); break;
     case SHIFTER_RES_POIS: case SHIFTER_RES_FIRE: case SHIFTER_RES_COLD: 
     case SHIFTER_RES_ACID: case SHIFTER_RES_ELEC: strcpy(p, " dur 20+d20"); break;	  
     case SHIFTER_TELE_SELF: case SHIFTER_TELE_SELF2:  printf(p, " range %d", plev * 5); break;
     case SHIFTER_HEROISM: case SHIFTER_BERSERK: strcpy(p, " dur 25+d25"); break;
     case SHIFTER_BR_FIRE: case SHIFTER_BR_ACID: case SHIFTER_L_BALL:
	  sprintf(p, " dam %d, r2", 55 + plev); break;
     case SHIFTER_HASTE: case SHIFTER_HASTE2:
	  sprintf(p, " dur %d+d20", plev); break;
     case SHIFTER_FIRST_AID: sprintf(p, " cure 2d%d", plev+5); break;
     case SHIFTER_HERBAL_HEALING: sprintf(p, " cure %d", plev*2); break;
     case SHIFTER_L_BOLT: sprintf(p, " dam %dd6", 3 + ((plev - 5) / 4)); break;
     case SHIFTER_SHIELD: strcpy(p, " dur 30+d30"); break;
     case SHIFTER_MIND_BLAST: case SHIFTER_ICE_BOLT: 
	  sprintf(p, " dam %dd8", 6 + ((plev - 5) / 4)); break;
     case SHIFTER_CONF_BALL: sprintf(p, " dam %d, r3", 40 + plev); break;
     case SHIFTER_NETHER_BOLT: sprintf(p, " dam %dd8", 9 + ((plev - 5) / 4)); break;
     case SHIFTER_CHAOS_BOLT: sprintf(p, " dam %dd8", 6 + ((plev - 5) / 3)); break;
     case SHIFTER_NEXUS_BOLT: sprintf(p, " dam %dd8", 6 + ((plev - 5) / 2)); break;
     }
}

static int get_shifter_power(int *sn)
{
        int             i = -1, ask, y = 1, x = 20;
        int             plev = level_of_class(CLASS_SHIFTER);
        int             temp[MAX_SHIFTER_POWERS], num = 0;
	char		choice;
	char            out_val[160];
	byte            line_attr;

	/* Nothing chosen yet */
	bool flag = FALSE;

	/* No redraw yet */
	bool redraw = FALSE;

#ifdef ALLOW_REPEAT

	/* Get the spell, if available */
	if (repeat_pull(sn))
	     return (TRUE);

#endif /* ALLOW_REPEAT */

	/* Assume cancelled */
	(*sn) = -1;

	for (i = 0; i < MAX_SHIFTER_POWERS; i++)
	{
	     if (shifter_powers[i].min_lev <= plev)
	     {
		  /* Shapeshift */
		  if (shifter_powers[i].type)
		  {
		       /* If not current form */
		      if (shifter_powers[i].form != p_ptr->shapeshift)
		      {
			   temp[num] = i;
			   num++;
		      }
		  }
		  else /* Power */
		  {
		       /* If related to current form */
		       if (shifter_powers[i].form == p_ptr->shapeshift)
		       {
			    temp[num] = i;
			    num++;
		       }
		  }
	     }
	}

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
						I2A(0), I2A(num - 1));
	
	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				char power_desc[80];
				char info[40];
				cptr comment;

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				screen_save();

				/* Display a list of spells */
				prt("", y, x);
				put_str("Name", y, x + 5);
				put_str("Cost/SFail", y, x + 29);
				put_str("Info", y, x + 40);

				/* Dump the spells */
				for (i = 0; i < num; i++)
				{
					/* Color */
					line_attr = TERM_WHITE;

					/* Green for forms, and darken unusbale powers */
					if (shifter_powers[temp[i]].type)
					     line_attr = TERM_GREEN;
					else if (shifter_powers[temp[i]].cost > p_ptr->csp)
					     line_attr = TERM_L_DARK;

					/* Get information */
					shifter_info(info, temp[i]);
					comment = info;

					/* Dump the spell */
					if (shifter_powers[temp[i]].type)
					{
					     /* Get base chance of failure */
					     int chance = shifter_powers[temp[i]].cost;
					     /* Code for calculating spell chance from object2.c */
					     chance -= 3 * (level_of_class(CLASS_SHIFTER) - 
							    shifter_powers[temp[i]].min_lev);
					     chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[A_DEX]] - 1);
					     /* Minimum failure rate */
					     if (chance < adj_mag_fail[p_ptr->stat_ind[A_DEX]]) 
						  chance = adj_mag_fail[p_ptr->stat_ind[A_DEX]];
					     /* Always a 5 percent chance of working */
					     if (chance > 95) chance = 95;
					     /* Returning to default form always succeeds */
					     if (temp[i] == SHIFTER_UNSHIFT) chance = 0;
					     sprintf(power_desc, "  %c) %-30s%3d%% %s",
						     I2A(i), shifter_powers[temp[i]].name, 
						     chance, comment);
					}
					else
					     sprintf(power_desc, "  %c) %-30s%3d %s",
						     I2A(i), shifter_powers[temp[i]].name, 
						     shifter_powers[temp[i]].cost, comment);
					c_prt(line_attr, power_desc, y + i + 1, x);
			        }

				/* Clear the bottom line */
				prt("", y + i + 1, x);
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
			bell("Illegal power choice!");
			continue;
		}
		
	        /* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", shifter_powers[temp[i]].name);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}
		
		/* Stop the loop */
		flag = TRUE;
        } 

	/* Restore the screen */
	if (redraw) screen_load();

	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
        (*sn) = temp[i];
	
#ifdef ALLOW_REPEAT

	repeat_push(*sn);

#endif /* ALLOW_REPEAT */

	/* Success */
        return (TRUE);	
}

void do_cmd_shifter()
{
     int power;
     int plev = level_of_class(CLASS_SHIFTER);
     int dir;
     int py = p_ptr->py;
     int px = p_ptr->px;
     bool notice = FALSE;

     /* Ask for a power */
     if (!get_shifter_power(&power)) return;

     /* If shift, check for failure. If power, check mana */
     if (shifter_powers[power].type)
     {
	  /* Get base chance of failure */
	  int chance = shifter_powers[power].cost;
	  /* Code for calculating spell chance from object2.c */
	  chance -= 3 * (level_of_class(CLASS_SHIFTER) - shifter_powers[power].min_lev);
	  chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[A_DEX]] - 1);
	  /* Minimum failure rate */
	  if (chance < adj_mag_fail[p_ptr->stat_ind[A_DEX]]) 
	       chance = adj_mag_fail[p_ptr->stat_ind[A_DEX]];
	  /* Always a 5 percent chance of working */
	  if (chance > 95) chance = 95;
	  /* Returning to default form always succeeds */
	  if (power == SHIFTER_UNSHIFT) chance = 0;

	  if (rand_int(100) < chance)
	  {
		if (flush_failure) flush();
		msg_print("You failed to concentrate hard enough!");

		/* Take a turn */
		p_ptr->energy_use = 100;

		return;
	  }
     }
     else
     {
	  /* Not enough mana */
	  if (shifter_powers[power].cost > p_ptr->csp)
	  {
	       /* Warning */
	       msg_print("You do not have enough mana to use this power.");
	       
	       /* Flush input */
	       flush();
	       
	       return;
	  }
     }

     /* Use the power */
     switch (power)
     {
     case SHIFTER_UNSHIFT:
          msg_format("You return to your originial form.");
	  p_ptr->shapeshift = 0;
	  notice = TRUE;
	  break;
     case SHIFTER_GIANT_SPIDER:
     case SHIFTER_TENGU:
     case SHIFTER_ARCTIC_BEAR:
     case SHIFTER_WYVERN:
     case SHIFTER_UMBER_HULK:
     case SHIFTER_GORGON:
     case SHIFTER_PHASE_SPIDER:
     case SHIFTER_ENT:
     case SHIFTER_COLBRAN:
     case SHIFTER_ICE_TROLL:
     case SHIFTER_MINDFLAYER:
     case SHIFTER_BEHOLDER:
     case SHIFTER_VAMPIRE:
     case SHIFTER_CHAOS_DRAKE:
	  msg_format("You become a %s!", shifter_powers[power].name);
	  p_ptr->shapeshift = shifter_powers[power].form;
	  notice = TRUE;
	  break;
     case SHIFTER_STING:
     case SHIFTER_STING2:
	  if (!get_aim_dir(&dir)) return;
	  fire_bolt(GF_POIS, dir, damroll(3 + ((plev - 1) / 5), 4));
	  effects[EFFECT_BOLT] += 1;
	  effects[EFFECT_POIS] += 1;
	  break;
     case SHIFTER_STINKING_CLOUD:
	  if (!get_aim_dir(&dir)) return;
	  fire_ball(GF_POIS, dir,
		    10 + (plev / 2), 2);
	  effects[EFFECT_BALL]++;
	  effects[EFFECT_POIS] += 1;
	  break;
     case SHIFTER_RES_POIS:
	  (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
	  effects[EFFECT_RES_POIS]++;
	  break;
     case SHIFTER_PHASE_DOOR:
     case SHIFTER_PHASE_DOOR2:
	  teleport_player(10);
	  effects[EFFECT_BLINK]++;
	  break;
     case SHIFTER_TELE_SELF:
     case SHIFTER_TELE_SELF2:
	  teleport_player(plev * 5);
	  effects[EFFECT_TELEPORT]++;
	  break;
     case SHIFTER_HEROISM:
	  (void)hp_player(10);
	  (void)set_hero(p_ptr->hero + 
			 randint(25) + 25);
	  if (set_afraid(0))
	       effects[EFFECT_REMOVE_FEAR]++;
	  effects[EFFECT_HEROISM]++;
	  break;
     case SHIFTER_RES_COLD:
	  (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
	  effects[EFFECT_RES_ELEMENT]++;
	  break;
     case SHIFTER_BERSERK:
	  (void)hp_player(30);
	  (void)set_shero(p_ptr->shero + 
			  randint(25) + 25);
	  if (set_afraid(0))
	       effects[EFFECT_REMOVE_FEAR]++;
	  effects[EFFECT_BERSERK]++;
	  break;
     case SHIFTER_LIGHT_AREA:
	  (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
	  effects[EFFECT_LIGHT_AREA]++;
	  break;
     case SHIFTER_RES_FIRE:
	  (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
	  effects[EFFECT_RES_ELEMENT]++;
	  break;
     case SHIFTER_BR_FIRE:
	  if (!get_aim_dir(&dir)) return;
	  fire_ball(GF_FIRE, dir,
		    55 + (plev), 2);
	  effects[EFFECT_BALL]++;
	  effects[EFFECT_FIRE]++;
	  break;
     case SHIFTER_CONFUSE:
     case SHIFTER_CONFUSE2:
	  if (!get_aim_dir(&dir)) return;
	  (void)confuse_monster(dir, plev);
	  effects[EFFECT_HINDER]++;
	  break;
     case SHIFTER_STONE_TO_MUD:
	  if (!get_aim_dir(&dir)) return;
	  (void)wall_to_mud(dir);
	  effects[EFFECT_STONE_TO_MUD]++;
	  break;
     case SHIFTER_EARTHQUAKE:
	  earthquake(py, px, 10);
	  effects[EFFECT_EARTHQUAKE]++;
	  break;
     case SHIFTER_HASTE:
     case SHIFTER_HASTE2:
	  if (!p_ptr->fast)
	  {
	       (void)set_fast(randint(20) + plev);
	  }
	  else
	  {
	       (void)set_fast(p_ptr->fast + randint(5));
	  }
	  effects[EFFECT_HASTE]++;
	  break;
     case SHIFTER_RES_ACID:
	  (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
	  effects[EFFECT_RES_ELEMENT]++;
	  break;
     case SHIFTER_SLEEP:
	  if (!get_aim_dir(&dir)) return;
	  (void)sleep_monster(dir, p_ptr->current_class);
	  effects[EFFECT_HINDER]++;
	  break;
     case SHIFTER_BR_ACID:
	  if (!get_aim_dir(&dir)) return;
	  fire_ball(GF_ACID, dir,
		    55 + (plev), 2);
	  effects[EFFECT_BALL]++;
	  effects[EFFECT_ACID]++;
	  break;
     case SHIFTER_FIRST_AID:
	  (void)hp_player(damroll(2, plev+5));
	  (void)set_cut(p_ptr->cut - (plev+10));
	  (void)set_poisoned(p_ptr->poisoned / 2);
	  effects[EFFECT_CURE_LIGHT_SERIOUS]++;
	  effects[EFFECT_CURE_POISON]++;
	  break;
     case SHIFTER_HERBAL_HEALING:
	  (void)hp_player(plev * 2);
	  (void)set_stun(0);
	  (void)set_cut(0);
	  (void)set_poisoned(0);
	  effects[EFFECT_HEAL]++;
	  effects[EFFECT_CURE_POISON]++;
	  break;
     case SHIFTER_SAT_HUNGER:
	  (void)set_food(PY_FOOD_MAX - 1);
	  effects[EFFECT_SATISFY_HUNGER]++;
	  break;
     case SHIFTER_ENTANGLE:
	  (void)slow_monsters(p_ptr->current_class);
	  effects[EFFECT_MASS_HINDER]++;
	  break;
     case SHIFTER_MIND_BLAST:
	  if (!get_aim_dir(&dir)) return;
	  fire_bolt_or_beam(plev, GF_CONFUSION, dir,
			    damroll(6+((plev-5)/4), 8));
	  effects[EFFECT_BBOLT]++;
	  effects[EFFECT_CONF]++;
	  break;
     case SHIFTER_L_BOLT:
	  if (!get_aim_dir(&dir)) return;
	  fire_bolt_or_beam(plev, GF_ELEC, dir,
			    damroll(3+((plev-5)/4), 6));
	  effects[EFFECT_BBOLT]++;
	  effects[EFFECT_LIGHTNING]++;
	  break;
     case SHIFTER_RES_ELEC:
	  (void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
	  effects[EFFECT_RES_ELEMENT]++;
	  break;
     case SHIFTER_L_BALL:
	  if (!get_aim_dir(&dir)) return;
	  fire_ball(GF_ELEC, dir,
		    55 + (plev), 2);
	  effects[EFFECT_BALL]++;
	  effects[EFFECT_LIGHTNING]++;
	  break;
     case SHIFTER_SLOW:
	  if (!get_aim_dir(&dir)) return;
	  (void)slow_monster(dir, p_ptr->current_class);
	  effects[EFFECT_HINDER]++;
	  break;
     case SHIFTER_SHIELD:
	  (void)set_shield(p_ptr->shield + randint(30) + 30);
	  effects[EFFECT_SHIELD]++;
	  break;
     case SHIFTER_ICE_BOLT:
	  if (!get_aim_dir(&dir)) return;
	  fire_bolt_or_beam(plev, GF_ICE, dir,
			    damroll(6+((plev-5)/4), 8));
	  effects[EFFECT_BBOLT]++;
	  effects[EFFECT_COLD]++;
	  break;
     case SHIFTER_DETECT_MONS:
	  (void)detect_monsters_normal();
	  effects[EFFECT_DETECT_MONSTERS]++;
	  break;
     case SHIFTER_MAGIC_MAP:
	  map_area();
	  effects[EFFECT_MAPPING]++;
	  break;
     case SHIFTER_TELE_OTHER:
	  if (!get_aim_dir(&dir)) return;
	  (void)teleport_monster(dir);
	  effects[EFFECT_TELEPORT_OTHER]++;
	  break;
     case SHIFTER_CONF_BALL:
	  if (!get_aim_dir(&dir)) return;
	  fire_ball(GF_CONFUSION, dir,
		    40 + (plev), 3);
	  effects[EFFECT_BALL]++;
	  effects[EFFECT_CONF]++;
	  break;
     case SHIFTER_TERROR:
	  (void)scare_monsters(p_ptr->current_class);
	  effects[EFFECT_MASS_HINDER]++;
	  break;
     case SHIFTER_MASS_SLEEP:
	  (void)sleep_monsters(p_ptr->current_class);
	  effects[EFFECT_MASS_HINDER]++;
	  break;
     case SHIFTER_SCARE:
     case SHIFTER_SCARE2:
	  if (!get_aim_dir(&dir)) return;
	  (void)fear_monster(dir, plev);
	  effects[EFFECT_HINDER]++;
	  break;
     case SHIFTER_DRAIN_LIFE:
	  if (!get_aim_dir(&dir)) return;
	  drain_life(dir, 200);
	  effects[EFFECT_DRAIN_LIFE]++;
	  break;
     case SHIFTER_NETHER_BOLT:
	  if (!get_aim_dir(&dir)) return;
	  fire_bolt_or_beam(plev-10, GF_NETHER, dir,
		    damroll(9 + ((plev-5)/4), 8));
	  effects[EFFECT_BBOLT] += 1;
	  effects[EFFECT_NETHER] += 1;
	  break;
     case SHIFTER_LIFE_MANA:
     {
	  /* life for mana */
	  int m = 0;
	  m = get_quantity("How much mana?",999);
	  if (!m) return;
	  if (m<0) return;
	  if (m > p_ptr->chp) {
	       msg_print("You don't have that much life!");
	       return;
	  }
	  if ((p_ptr->csp + m) > p_ptr->msp)
	       m = p_ptr->msp - p_ptr->csp;
	  p_ptr->csp += m;
	  take_hit(m,"spellcasting");
	  msg_print("You convert life into mana.");
	  effects[EFFECT_LIFE_FOR_MANA]++;
	  break;
     }
     case SHIFTER_POLY_OTHER:
	  if (!get_aim_dir(&dir)) return;
	  (void)poly_monster(dir, p_ptr->current_class);
	  effects[EFFECT_POLY_OTHER]++;
	  break;
     case SHIFTER_CHAOS_BOLT:
	  if (!get_aim_dir(&dir)) return;
	  fire_bolt(GF_CHAOS, dir,
		    damroll(6 + ((plev-5)/3), 8));
	  effects[EFFECT_BOLT] += 1;
	  effects[EFFECT_CHAOS] += 1;
	  break;
     case SHIFTER_NEXUS_BOLT:
	  if (!get_aim_dir(&dir)) return;
	  fire_bolt(GF_NEXUS, dir,
		    damroll(6 + ((plev-5)/2), 8));
	  effects[EFFECT_BOLT] += 1;
	  effects[EFFECT_NEXUS] += 1;
	  break;
     }

     /* Take a turn */
     p_ptr->energy_use = 100;

     /* Powers use mana */
     if (!shifter_powers[power].type)
	  p_ptr->csp -= shifter_powers[power].cost;

     /* Redraw mana */
     p_ptr->redraw |= (PR_MANA);

     /* Changed form */
     if (notice) 
     {
	  /* Disturb */
	  if (disturb_state) disturb(0, 0);

	  /* Recalculate bonuses */
	  p_ptr->update |= (PU_BONUS);

	  /* Handle stuff */
	  handle_stuff();
     }

     /* Window stuff */
     p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

int get_sorceror_spell(int *sn, cptr prompt, bool any)
{
        int             ask, y = 1, x = 20, i;
	char		choice;
	char            out_val[160];
	byte            line_attr;

	/* Nothing chosen yet */
	bool flag = FALSE;

	/* No redraw yet */
	bool redraw = FALSE;

#ifdef ALLOW_REPEAT

	/* Get the spell, if available */
	if (repeat_pull(sn))
	     return (TRUE);

#endif /* ALLOW_REPEAT */

	/* Assume cancelled */
	(*sn) = -1;

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Spells %c-%c, *=List, ESC=exit) %^s which spell? ",
						I2A(0), I2A(MAX_SORCEROR_SPELL - 1), prompt);
	
	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
	     /* Request redraw */
	     if ((choice == ' ') || (choice == '*') || (choice == '?'))
	     {
		  /* Show the list */
		  if (!redraw)
		  {
		       int j;
		       const magic_type *s_ptr;
		       
		       /* Show list */
		       redraw = TRUE;

		       /* Save the screen */
		       screen_save();

		       /* Display a list of spells */
		       prt("", y, x);
		       put_str("Name", y, x + 5);
		       if (adult_hidden) /* Hide spell information */
			    put_str("                 ", y, x + 35);
		       else /* Show spell information */
			    put_str("Lv Mana  Info", y, x + 35);
		       
		       /* Dump the spells */
		       for (j = 0; j < MAX_SORCEROR_SPELL; j++)
		       {
			    cptr comment;
			    char info[80] = "";

			    int class;
			    switch (sorceror_spell[j] / 64)
			    {
			    case REALM_MAGIC: class = CLASS_MAGE;
				 break;
			    case REALM_PRAYER: class = CLASS_PRIEST;
				 break;
			    case REALM_ILLUSION: class = CLASS_ILLUSIONIST;
				 break;
			    case REALM_DEATH: class = CLASS_DEATH_PRIEST;
				 break;
			    }

			    /* Get the spell */
			    s_ptr = &magic_info[class].info[sorceror_spell[j] % 64];
			    
			    /* Skip unknown spells */
			    if (sorceror_spell[j] == -1)
			    {
				 char temp_val[160];
				 sprintf(temp_val, "  %c) %-30s", I2A(j), "(unknown)");
				 c_prt(TERM_L_DARK, "", y + j + 1, x);
				 break;
			    }

			    spell_info_aux(info, sorceror_spell[j] / 64, 
					   sorceror_spell[j] % 64);
			    comment = info;

			    /* Assume spell is known and tried */
			    line_attr = TERM_WHITE;

			    /* Darken if not enough mana or level */
			    if (s_ptr->smana > p_ptr->csp)
			      line_attr = TERM_L_DARK;
			    if (s_ptr->slevel > level_of_class(CLASS_SORCEROR))
			      line_attr = TERM_RED;

			    if (adult_hidden)
			    {
				 /* Dump the spell --(-- */
				 char temp_val[160];
				 sprintf(temp_val, "  %c) %-30s",
					 I2A(j), spell_names[sorceror_spell[j] / 64][sorceror_spell[j] % 64]);
				 c_prt(line_attr, temp_val, y + j + 1, x);
			    }
			    else
			    {
				 /* Dump the spell --(-- */
				 char temp_val[160];
				 sprintf(temp_val, "  %c) %-30s%2d %4d %s",
					 I2A(j), spell_names[sorceror_spell[j] / 64][sorceror_spell[j] % 64],
					 s_ptr->slevel, s_ptr->smana, 
					 comment);
				 c_prt(line_attr, temp_val, y + j + 1, x);
			    }
		       }
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
	     if (!any && ((i < 0) || 
			  (i >= MAX_SORCEROR_SPELL) || 
			  (sorceror_spell[i] == -1)))
	     {
		  bell("Illegal spell choice!");
		  msg_format("You may not %s that spell.", prompt);
		  continue;
	     }

	     /* Check the level/mana */
	     if (!any)
	     {
		  const magic_type *s_ptr;
		  int class;
		  switch (sorceror_spell[i] / 64)
		  {
		  case REALM_MAGIC: class = CLASS_MAGE;
		       break;
		  case REALM_PRAYER: class = CLASS_PRIEST;
		       break;
		  case REALM_ILLUSION: class = CLASS_ILLUSIONIST;
		       break;
		  case REALM_DEATH: class = CLASS_DEATH_PRIEST;
		       break;
		  }
		  
		  /* Get the spell */
		  s_ptr = &magic_info[class].info[sorceror_spell[i] % 64];
		  
		  /* Level or mana too high */
		  if (s_ptr->slevel >  level_of_class(CLASS_SORCEROR) ||
		      s_ptr->smana > p_ptr->csp)
		  {
		       bell("This spell is too hard for you!");
		       continue;
		  }
	     }

	     /* Verify it */
	     if (ask)
	     {
		  char tmp_val[160];
		  
		  /* Prompt */
		  strnfmt(tmp_val, 78, "%s %s? ", prompt, spell_names[sorceror_spell[i] / 64][sorceror_spell[i] % 64]);
		  
		  /* Belay that order */
		  if (!get_check(tmp_val)) continue;
	     }

	     /* Stop the loop */
	     flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) screen_load();

	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = i;
	
#ifdef ALLOW_REPEAT

	repeat_push(*sn);

#endif /* ALLOW_REPEAT */

	/* Success */
	return (TRUE);	
}

void do_cmd_sorceror()
{
	int spell, class;

	const magic_type *s_ptr;

	/* Not when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* Hack -- Handle stuff */
	handle_stuff();

	if (sorceror_spell[0] == -1)
	{
	     msg_print("You don't know any spells.");
	     return;
	}

	/* Ask for a spell */
	if (!get_sorceror_spell(&spell, "cast", FALSE))
	{
		return;
	}

	switch (sorceror_spell[spell] / 64)
	{
	case REALM_MAGIC: class = CLASS_MAGE;
	     break;
	case REALM_PRAYER: class = CLASS_PRIEST;
	     break;
	case REALM_ILLUSION: class = CLASS_ILLUSIONIST;
	     break;
	case REALM_DEATH: class = CLASS_DEATH_PRIEST;
	     break;
	}

	/* Get the spell */
	s_ptr = &magic_info[class].info[sorceror_spell[spell] % 64];
			    
	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

		/* Flush input */
		flush();

		return;
	}

	/* Cast the spell */
	cast_spell(sorceror_spell[spell] / 64, 
		   sorceror_spell[spell] % 64);

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Use some mana */
	p_ptr->csp -= s_ptr->smana;

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);     
}

static int get_action(int *sn, bool m)
{
        int             i = -1, ask, y = 1, x = 20;
        int             temp[MAX_CLASS], num = 0;
	char		choice;
	char            out_val[160];
	byte            line_attr;

	/* Nothing chosen yet */
	bool flag = FALSE;

	/* No redraw yet */
	bool redraw = FALSE;

#ifdef ALLOW_REPEAT

	/* Get the spell, if available */
	if (repeat_pull(sn))
	     return (TRUE);

#endif /* ALLOW_REPEAT */

	/* Assume cancelled */
	(*sn) = -1;

	for (i = 0; i < MAX_CLASS; i++)
	{
	     if ((p_ptr->pclass[i] == magery_class(TRUE) && m) ||
		 (p_ptr->pclass[i] == priest_class(TRUE) && !m) ||
		 (p_ptr->pclass[i] == CLASS_CRUSADER && m) ||
		 (p_ptr->pclass[i] == CLASS_SLAYER && m) ||
		 (p_ptr->pclass[i] == CLASS_SHIFTER && m) ||
		 (p_ptr->pclass[i] == CLASS_SORCEROR && m))
	     {
		  temp[num] = p_ptr->pclass[i];
		  num ++;
	     }
	}

	/* no classes can do this */
	if (num == 0) {
	     if (m) msg_print("You cannot cast spells!");
	     else
		  msg_print("Pray hard enough and your prayers may be answered.");
	     return 0;
	}

	/* more than one choice */
	if (num > 1)
	{

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Class %c-%c, *=List, ESC=exit) Use which class? ",
						I2A(0), I2A(num - 1));

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				char power_desc[80];

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				screen_save();

				/* Display a list of spells */
				prt("", y, x);

				/* Dump the spells */
				for (i = 0; i < num; i++)
				{
					/* Color */
					line_attr = TERM_WHITE;

					/* Dump the spell */
					sprintf(power_desc, "  %c) %-30s",
						I2A(i), class_info[temp[i]].title);
					c_prt(line_attr, power_desc, y + i + 1, x);
				}

				/* Clear the bottom line */
				prt("", y + i + 1, x);
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
			bell("Illegal class choice!");
			continue;
		}

	        /* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", class_info[temp[i]].title);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	} /* only one choice */
	else { i = 0; flag = TRUE; }

	/* Restore the screen */
	if (redraw) screen_load();

	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = temp[i];
	
#ifdef ALLOW_REPEAT

	repeat_push(*sn);

#endif /* ALLOW_REPEAT */

	/* Success */
	return (TRUE);	
}

void do_cmd_action(bool m)
{
     int action;

     /* Ask for a power */
     if (!get_action(&action, m)) return;

     switch_until(action);

     /* 'm' */
     if (m)
     {
	  switch (action)
	  {
	  case CLASS_SORCEROR:
	       do_cmd_sorceror(); break;
	  case CLASS_CRUSADER:
	  case CLASS_SLAYER:
	       do_cmd_power(); break;
	  case CLASS_SHIFTER:
	       do_cmd_shifter(); break;
	  case CLASS_ILLUSIONIST:
	       do_cmd_cast_illusion(); break;
	  case CLASS_MAGE:
	       do_cmd_cast(); break;
	  }
     }
     /* 'p' */
     else
     {
	  switch (action)
	  {
	  case CLASS_DEATH_PRIEST:
	  case CLASS_SLAYER:
	       do_cmd_cast_death(); break;
	  case CLASS_PRIEST:
	  case CLASS_CRUSADER:
	       do_cmd_pray(); break;
	  }
     }
}
