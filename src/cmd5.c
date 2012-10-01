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

	int spell = -1;
	int num = 0;

	byte spells[64];

	int ver;

	bool flag, redraw, okay;
	char choice;

	magic_type *s_ptr;

	char out_val[160];

	cptr p = ((mp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");

#ifdef ALLOW_REPEAT /* TNB */

    /* Get the spell, if available */
    if (repeat_pull(sn)) {

        /* Verify the spell */
        if (spell_okay(*sn, known)) {

            /* Success */
            return (TRUE);
        }
    }

#endif

	/* Extract spells */
	for (spell = 0; spell < 64; spell++)
	{
		/* Check for this spell */
		if ((spell < 32) ?
		    (spell_flags[mp_ptr->spell_type][sval][0] & (1L << spell)) :
		    (spell_flags[mp_ptr->spell_type][sval][1] & (1L << (spell - 32))))
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
			        prompt, spell_names[mp_ptr->spell_type][spell],
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

#ifdef ALLOW_REPEAT /* TNB */

    repeat_push(*sn);

#endif

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

	int spell = -1;
	int num = 0;

	byte spells[64];

	object_type *o_ptr;

	cptr q, s;


	/* Warriors are illiterate */
	if (!mp_ptr->spell_book)
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
		    (spell_flags[mp_ptr->spell_type][sval][0] & (1L << spell)) :
		    (spell_flags[mp_ptr->spell_type][sval][1] & (1L << (spell - 32))))
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
 *
 * Added building flag -KMW-
 */
void do_cmd_study(bool in_bldg)
{
	int i, item, sval;

	int spell = -1;

	cptr p = ((mp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");

	cptr q, s;

	object_type *o_ptr;


	if (!mp_ptr->spell_book)
	{
		msg_print("You cannot read books!");
		return;
	}

	/* If in building - can see spellbooks -KMW- */
	if ((p_ptr->blind || no_lite()) && (!in_bldg))	{
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


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Mage& Illusionist -- Learn a selected spell */
	if((mp_ptr->spell_book == TV_MAGIC_BOOK) ||
	    (mp_ptr->spell_book == TV_ILLUSION_BOOK))  /* added -KMW- */
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", sval, FALSE) && (spell == -1)) return;
	}

	/* Priest& Druid -- Learn a random prayer */
	if((mp_ptr->spell_book == TV_PRAYER_BOOK) ||
	    (mp_ptr->spell_book == TV_NATURE_BOOK))
	{
		int k = 0;

		int gift = -1;

		/* Extract spells */
		for (spell = 0; spell < 64; spell++)
		{
			/* Check spells in the book */
			if ((spell < 32) ?
			    (spell_flags[mp_ptr->spell_type][sval][0] & (1L << spell)) :
			    (spell_flags[mp_ptr->spell_type][sval][1] & (1L << (spell - 32))))
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
	           p, spell_names[mp_ptr->spell_type][spell]);

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
* Brand some ammunition.  Used by Cubragol and a mage spell.  The spell was
* moved here from cmd6.c where it used to be for Cubragol only.  I've also
 * expanded it to do either frost, fire or venom, at random. -GJW	-KMW-
 */
void brand_ammo (int brand_type, int bolts_only)
{
	int a;
	int allowable;

	if (bolts_only)
		allowable = TV_BOLT;
	else
		allowable = TV_BOLT | TV_ARROW | TV_SHOT;

	for (a = 0; a < INVEN_PACK; a++)
	{
		object_type *o_ptr = &inventory[a];

		if ((bolts_only) && (o_ptr->tval != TV_BOLT)) continue;
		if ((!bolts_only) && (o_ptr->tval != TV_BOLT) &&
		    (o_ptr->tval != TV_ARROW) && (o_ptr->tval != TV_SHOT))
		    	continue;
		if ((!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
		   (!cursed_p(o_ptr) && !broken_p(o_ptr)))
		   	break;
	}

	/* Enchant the ammo (or fail) */
	if ((a < INVEN_PACK) && (rand_int(100) < 50))
	{
		object_type *o_ptr = &inventory[a];
		char *ammo_name, *aura_name, msg[48];
		int aura_type, r;

		if (brand_type == 1) r = 0;		/* fire only */
		else if (brand_type == 2) r = 99;	/* poison only */
		else r = rand_int (100);

		if (r < 33)
		{
			aura_name = "fiery";
			aura_type = EGO_FLAME;
		}
		else if (r < 67)
		{
			aura_name = "frosty";
			aura_type = EGO_FROST;
		}
		else
		{
			aura_name = "sickly";
			aura_type = EGO_VENOM;
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


/* Fetch an item (teleport it right underneath the caster) */
static void fetch_item(int dir, int wgt)
{
	int ty, tx, i;
	bool flag;
	object_type *o_ptr;
	int py, px;

	py = p_ptr->py; px = p_ptr->px;
	/* Check to see if an object is already there */
	if(cave_o_idx[py][px])
	{
		msg_print("You can't fetch when you're already standing on something.");
		return;
	}

	/* Use a target */
	if(dir==5 && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
		if(distance(py, px, ty, tx)>MAX_RANGE)
		{
			msg_print("You can't fetch something that far away!");
			return;
		}
	}
	else
	{
		/* Use a direction */
		ty = py; /* Where to drop the item */
		tx = px;
		flag = FALSE;
		do
		{
			ty += ddy[dir];
			tx += ddx[dir];
			if ((distance(py, px, ty, tx)> MAX_RANGE)
				|| !cave_floor_bold(ty, tx)) return;
		} while(!cave_o_idx[ty][tx]);
	}
	o_ptr = &o_list[cave_o_idx[ty][tx]];
	if (o_ptr->weight > wgt)
	{	/* Too heavy to 'fetch' */
		msg_print("The object is too heavy.");
		return;
	}
	i = cave_o_idx[ty][tx];
	cave_o_idx[ty][tx] = 0;
	cave_o_idx[py][px] = i; /* 'move' it */
	o_ptr->iy = py;
	o_ptr->ix = px;

	p_ptr->redraw |= PR_MAP;
}


/* incremental sleep spell */
/* -KMW- */
static void do_sleep_monster(void)
{
	int dir;

	if (p_ptr->lev < 15) {
		if (!get_aim_dir(&dir)) return;
		sleep_monster(dir);
	} else if (p_ptr->lev < 30)
		sleep_monsters_touch();
	else
		sleep_monsters();
}

/* incremental fear spell */
/* -KMW- */
static void do_fear_monster(void)
{
	int dir;

	if (p_ptr->lev < 15) {
		if (!get_aim_dir(&dir)) return;
		fear_monster(dir,p_ptr->lev);
	} else if (p_ptr->lev < 30)
		fear_monsters_touch();
	else
		fear_monsters();
}

/* incremental cure wounds spell */
/* -KMW- */
static void do_cure_wounds(void)
{
	if (p_ptr->lev < 15)
		(void)hp_player(damroll(4, 10));
	else if (p_ptr->lev < 30) {
		(void)hp_player(damroll(6, 10));
		(void)set_cut(0);
	} else {
		(void)hp_player(damroll(8, 10));
		(void)set_stun(0);
		(void)set_cut(0);
	}
}


/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int item, sval, spell, dir;
	int chance, beam;

	int plev = p_ptr->lev;

	object_type *o_ptr;

	magic_type *s_ptr;

	cptr q, s;


	/* Require spell ability */
	if((mp_ptr->spell_book != TV_MAGIC_BOOK) &&
		 (mp_ptr->spell_book != TV_ILLUSION_BOOK)) /* Added -KMW- */
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

	/* Access the item's sval */
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


	/* Access the spell */
	s_ptr = &mp_ptr->info[spell];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

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
		/* Hack -- chance of "beam" instead of "bolt" */
		beam =(((p_ptr->pclass == CLASS_MAGE) || /* Added Illusionist -KMW- */
 		    (p_ptr->pclass == CLASS_ILLUSIONIST)) ? plev : (plev / 2));

		/* Spells.  */
		switch (spell)
		{
			case 0:
			{
				if (mp_ptr->spell_type == 2) {
					/* confusion bolt -KMW- */
	 				if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam-10, GF_CONFUSION, dir,
					    damroll(2 + ((plev - 1) / 5), 4));
					break;
				} else { /* magic missile */
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
					    damroll(3 + ((plev - 1) / 5), 4));
					break;
				}
			}

			case 1: /* detect monsters */
			{
				(void)detect_monsters_normal();
				break;
			}

			case 2: /* phase door */
			{
				teleport_player(10);
				break;
			}

			case 3: /* light area */
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 4: /* treasure detection */
			{
				(void)detect_treasure();
				(void)detect_objects_gold();
				break;
			}

			case 5:
			{
				if (mp_ptr->spell_type == 2) {
					/* fear -KMW- */
					(void)do_fear_monster();
					break;
				} else { /* cure wounds */
	 				(void)do_cure_wounds();
					break;
				}
			}

			case 6: /* object detection */
			{
				(void)detect_objects_normal();
				break;
			}

			case 7: /* find hidden traps/doors */
			{
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 8: /* stinking cloud */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          10 + (plev / 2), 2);
				break;
			}

			case 9:
			{
				if(mp_ptr->spell_type == 2) {
					/* infravision */
					if (p_ptr->tim_infra == 0)
					set_tim_infra(p_ptr->tim_infra + 200 + randint(100));
					break;
				} else { /* confuse monster */
					if (!get_aim_dir(&dir)) return;
					(void)confuse_monster(dir, plev);
					break;
				}
			}

			case 10:
			{
				if(mp_ptr->spell_type == 2) {
					/* sleep */
					(void)do_sleep_monster();
					break;
				} else { /* lightning bolt */
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam-10, GF_ELEC, dir,
					    damroll(3+((plev-5)/4), 8));
					break;
				}
			}

			case 11: /* trap/door destruction */
			{
				(void)destroy_doors_touch();
				break;
			}

			case 12:
			{
				if (mp_ptr->spell_type == 2) {
					/* fog cloud -KMW- */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_FOG, dir,
					    10 + (plev / 2), 3);
					break;
				} else { /* sleep */
					if (!get_aim_dir(&dir)) return;
					(void)do_sleep_monster();
					break;
					break;
				}
			}

			case 13: /* cure poison */
			{
				(void)set_poisoned(0);
				break;
			}

			case 14:
			{
				if (mp_ptr->spell_type == 2) {
					/* satisfy hunger */
					(void)set_food(PY_FOOD_MAX - 1);
					break;
				} else { /* teleport self */
					teleport_player(plev * 5);
					break;
				}
			}

			case 15:
			{
				if(mp_ptr->spell_type == 2) {
					/* shadow door */
					(void)door_creation();
					break;
				} else { /* spear of light */
					if (!get_aim_dir(&dir)) return;
					msg_print("A line of blue shimmering light appears.");
					fire_beam(GF_LITE, dir,
					    damroll(2+((plev-5)/4), 6));
					lite_line(dir);
					break;
				}
			}

			case 16:
			{
				if (mp_ptr->spell_type == 2) {
					/* shadow monster */
					if (!get_aim_dir(&dir)) return;
					fire_bolt(GF_FORCE, dir,
					    damroll(5+((plev-6)/4), 8));
					break;
				} else { /* frost bolt */
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam-10, GF_COLD, dir,
					    damroll(5+((plev-5)/4), 8));
					break;
				}
			}

			case 17: /* turn stone to mud */
			{
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			}

			case 18:
			{
				if (mp_ptr->spell_type == 2) {
					/* detect invisible */
					(void)set_tim_s_invis(p_ptr->tim_s_invis + randint(24) + 24);
					break;
				} else { /* satisfy hunger */
					(void)set_food(PY_FOOD_MAX - 1);
					break;
				}
			}

			case 19: /* recharge item */
			{
				(void)recharge((plev * 2));
				break;
			}

			case 20: /* brand ammo */
			{
				if (mp_ptr->spell_type == 2) {
					(void)brand_ammo(1,0);
					break;
				} else { /* fetch item */
					if (!get_aim_dir(&dir)) return;
					fetch_item(dir, plev*15);
					break;
				}
			}

			case 21:
			{
				if (mp_ptr->spell_type == 2) {
					/* spear of light */
					if (!get_aim_dir(&dir)) return;
					msg_print("A line of blue shimmering light appears.");
					fire_beam(GF_LITE, dir,
					    damroll(2+((plev-5)/4), 6));
					lite_line(dir);
					break;
				} else { /* polymorph other */
					if (!get_aim_dir(&dir)) return;
					(void)poly_monster(dir);
					break;
				}
			}

			case 22:
			{
				if (mp_ptr->spell_type == 2) {
					/* chaos */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_CHAOS, dir,
					    25 + plev, 2);
					break;
				} else { /* identify */
					(void)ident_spell();
					break;
				}
			}

			case 23:
			{
				if (mp_ptr->spell_type == 2) {
					/* mental barrier */
					p_ptr->tim_sus_int = p_ptr->tim_sus_int + 100;
					p_ptr->tim_sus_wis = p_ptr->tim_sus_wis + 100;
					msg_print("Your wisdom and intelligence cannot be changed!");
					break;
				} else {
					/* lower water */
					alter_terrain(py,px,plev,2);
					break;
				}
			}

			case 24:
			{
				if(mp_ptr->spell_type == 2) {
					/* true sight */
					map_area();
					break;
				} else { /* fire bolt */
					if (!get_aim_dir(&dir)) return;

					fire_bolt_or_beam(beam, GF_FIRE, dir,
					    damroll(8+((plev-5)/4), 8));
					break;
				}
			}

			case 25: /* slow monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir);
				break;
			}

			case 26:
			{
				if (mp_ptr->spell_type == 2) {
					/* shadow ball */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_DARK, dir, 35 + (plev), 2);
					break;
				} else { /* frost ball */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_COLD, dir, 30 + (plev), 2);
					break;
				}
			}

			case 27:
			{
				if (mp_ptr->spell_type == 2) {
					/* bolt of darkness */
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam, GF_DARK, dir,
					    damroll(8+((plev-5)/4), 8));
					break;
				} else { /* summon elemental -KMW- */
					summon_monster(SUMMON_ELEMENTAL);
					break;
				}
			}

			case 28:
			{
				if(mp_ptr->spell_type == 2) {
					/* shadowform */
					(void)set_tim_ghost(p_ptr->tim_ghostly + plev + randint(24));
					break;
				} else { /* teleport other */
					if (p_ptr->ts_anchor) {
						msg_print("Your time/space anchor prevents it from leaving.");
						break;
					}
					if (!get_aim_dir(&dir)) return;
					(void)teleport_monster(dir);
					break;
				}
			}

			case 29: /* haste self */
			{
				if (!p_ptr->fast)
					(void)set_fast(randint(20) + plev);
				else
					(void)set_fast(p_ptr->fast + randint(5));
				break;
			}

			case 30:
			{
				if(mp_ptr->spell_type == 2) {
					/* prismatic wall */
					warding_glyph();
					break;
				} else { /* fire ball */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_FIRE, dir,
					    55 + (plev), 2);
					break;
				}
			}

			case 31:
			{
				if (mp_ptr->spell_type == 2) {
					/* prismatic spray */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_LITE, 5, 40 + (plev), 2);
					fire_beam(GF_LITE, dir,
					    damroll(8+((plev-5)/4), 8));
					break;
				} else { /* word of destruction */
					destroy_area(py, px, 15, TRUE);
					break;
				}
			}

			case 32:
			{
				if (mp_ptr->spell_type == 2) {
					/* chromatic shield */
					(void)set_shield(p_ptr->shield + randint(30) + 30);
					break;
				} else { /* genocide */
					(void)genocide();
					break;
				}
			}

			case 33:
			{
				if (mp_ptr->spell_type == 2) {
					/* wizard lock */
					do_cmd_spike(TRUE);
					break;
				} else { /* door creation */
					(void)door_creation();
					break;
				}
			}

			case 34:
			{
				if (mp_ptr->spell_type == 2) {
					/* bedlam */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_CHAOS, dir,
					    50 + plev, 10);
					break;
				} else { /* stair creation */
					(void)stair_creation();
					break;
				}
			}

			case 35:
			{
				if (mp_ptr->spell_type == 2) {
					/* word of recall */
					if (!p_ptr->word_recall)
					{
						p_ptr->word_recall = rand_int(20) + 15;
						msg_print("The air about you becomes charged...");
					}
					else
					{
						p_ptr->word_recall = 0;
						msg_print("A tension leaves the air around you...");
					}
					break;
				} else { /* teleport level */
					if (p_ptr->ts_anchor) {
						msg_print("Your time/space anchor prevents you from leaving.");
						break;
					}
					(void)teleport_player_level();
					break;
				}
			}

			case 36:
			{
				if (mp_ptr->spell_type == 2) {
					/* detect enchantment */
					(void)detect_objects_magic();
					break;
				} else { /* earthquake */
					earthquake(py, px, 10);
					break;
				}
			}

			case 37:
			{
				if(mp_ptr->spell_type == 2) {
					/* probing */
					(void)probing();
					break;
				} else { /* word of recall */
					if (!p_ptr->word_recall)
					{
						if (p_ptr->ts_anchor) {
							msg_print("Your time/space anchor prevents you from recalling.");
							break;
						}

						p_ptr->word_recall = rand_int(20) + 15;
						msg_print("The air about you becomes charged...");
					}
					else
					{
						p_ptr->word_recall = 0;
						msg_print("A tension leaves the air around you...");
					}
					break;
				}
			}

			case 38:
			{
				if (mp_ptr->spell_type == 2) {
					/* sunfire */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_LITE, dir,
					    50 + plev, 10);
					break;
				} else { /* dimension door */
					int i,j;

					msg_print("You open a dimensional gate. Choose a destination.");
					if (!tgt_pt(&i,&j)) return;
					if (!cave_empty_bold(j,i) || (cave_info[j][i] & CAVE_ICKY) ||
					     (distance(j,i,p_ptr->py,p_ptr->px) > plev + 2) || (!rand_int(plev * plev / 2))) {
						msg_print("You fail to exit the astral plane correctly!");
						teleport_player(10);
					} else teleport_player_to(j,i);
					break;
				}
			}

			case 39: /* the bigbys that are duplicates - warding, slow etc. shoudl act all aroundplayer */
			{
				if (mp_ptr->spell_type == 2) {
					/* Bigby's Interposing Hand */
					warding_glyph();
					break;
				} else { /* acid bolt */
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam, GF_ACID, dir,
					    damroll(6+((plev-5)/4), 8));
					break;
				}
			}

			case 40:
			{
				if (mp_ptr->spell_type == 2) {
					/* Bigby's Phantom Hand */
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam, GF_CONFUSION, dir,
					    damroll(2+((plev-5)/4), 8));
					break;
				} else { /* cloud kill */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_POIS, dir,
					    20 + (plev / 2), 3);
					break;
				}
			}

			case 41:
			{
				if (mp_ptr->spell_type == 2) {
					/* Bigby's Forceful Hand */
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam, GF_FORCE, dir,
					    damroll(6+((plev-5)/4), 8));
					break;
				} else { /* acid ball */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_ACID, dir,
					    40 + (plev), 2);
					break;
				}
			}

			case 42:
			{
				if (mp_ptr->spell_type == 2) {
					/* Bigby's Grasping Hand */
					if (!get_aim_dir(&dir)) return;
					(void)slow_monster(dir);
					break;
				} else { /* ice storm */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_COLD, dir,
					    70 + (plev), 3);
					break;
				}
			}

			case 43:
			{
				if (mp_ptr->spell_type == 2) {
					/* Bigby's Clenched Fist */
					if (!get_aim_dir(&dir)) return;
					fire_beam(GF_FORCE, dir,
					    damroll(10+((plev-5)/4), 8));
					break;
				} else { /* meteor swarm */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_METEOR, dir,
					    65 + (plev), 3);
					break;
				}
			}

			case 44:
			{
				if (mp_ptr->spell_type == 2) {
					/* Bigby's Crushing Hand */
					/* want to have this last two turns */
					if (!get_aim_dir(&dir)) return;
					fire_bolt(GF_GRAVITY, dir,
					    damroll(12+((plev-5)/4), 8));
					break;
				} else { /* plasma bolt */
					if (!get_aim_dir(&dir)) return;
					fire_bolt(GF_PLASMA, dir,
					    damroll(10+((plev-5)/4), 8));
					break;
				}
			}

			case 45:
			{
				if (mp_ptr->spell_type == 2) {
					/* force blast */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_FORCE, 5,
					    300 + (plev * 2), 3);
					break;
				} else { /* mana storm */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_MANA, dir,
					    300 + (plev * 2), 3);
					break;
				}
			}

			case 46:
			{
				if (mp_ptr->spell_type == 2) {
					/* Sphere of Light */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_LITE, dir,
					    30 + (plev), 2);
					break;
				} else { /* wizard light */
					wiz_lite();
					break;
				}
			}

			case 47:
			{
				if (mp_ptr->spell_type == 2) {
					/* sphere of darkness */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_DARK, dir,
					    35 + (plev), 2);
					break;
				} else { /* detect enchantment */
					(void)detect_objects_magic();
					break;
				}
			}

			case 48:
			{
				if (mp_ptr->spell_type == 2) {
					/* sphere of confusion */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_CONFUSION, dir,
					    40 + (plev), 2);
					break;
				} else { /* time/space anchor */
					(void)set_tim_tsanchor(p_ptr->ts_anchor + randint(100) + p_ptr->lev);
					break;
				}
			}

			case 49:
			{
				if (mp_ptr->spell_type == 2) {
					/* sphere of chaos */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_CHAOS, dir,
					    45 + (plev), 2);
					break;
				} else { /* genocide */
					(void)genocide();
					break;
				}
			}

			case 50:
			{
				if (mp_ptr->spell_type == 2) {
					/* sphere of sound */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_SOUND, dir,
					    50 + (plev), 2);
					break;
				} else { /* mass genocide */
					(void)mass_genocide();
					break;
				}
			}

			case 51:
			{
				if (mp_ptr->spell_type == 2) {
					/* explosion */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_SHARD, dir,
					    80 + (plev), 2);
					break;
				} else { /* probing */
					(void)probing();
					break;
				}
			}

			case 52:
			{
				if (mp_ptr->spell_type == 2) {
					/* remove fear */
					(void)set_afraid(0);
					break;
				} else { /* resist fire */
					(void)set_oppose_fire(p_ptr->oppose_cold + randint(20) + 20);
					break;
				}
			}

			case 53:
			{
				if (mp_ptr->spell_type == 2) {
					/* resist light & dark */
					(void)set_oppose_ld(p_ptr->oppose_ld + randint(20) + 20);
					break;
				} else { /* resist cold */
					(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
					break;
				}
			}

			case 54:
			{
				if (mp_ptr->spell_type == 2) {
					/* resist poison */
					(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
					break;
				} else { /* resist acid */
					(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
					break;
				}
			}

			case 55:
			{
				if (mp_ptr->spell_type == 2) {
					/* resist chaos & confusion */
					(void)set_oppose_cc(p_ptr->oppose_cc + randint(20) + 20);
					break;
				} else { /* resist poison */
					(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
					break;
				}
			}

			case 56:
			{
				if (mp_ptr->spell_type == 2) {
					/* resist sound & shards */
					(void)set_oppose_ss(p_ptr->oppose_ss + randint(20) + 20);
					break;
				} else { /* protection from corrosion */
					rustproof();
					break;
				}
			}

			case 57:
			{
				if (mp_ptr->spell_type == 2) {
					/* resist nexus */
					(void)set_oppose_nex(p_ptr->oppose_nex + randint(20) + 20);
					break;
				} else { /* resistance */
					(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) +
					    20);
					(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) +
					    20);
					(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
					(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) +
					    20);
					(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) +
					    20);
					break;
				}
			}

			case 58:
			{
				if (mp_ptr->spell_type == 2) {
					/* Invisibility */
					(void)set_tim_invis(p_ptr->tim_invis + randint(24));
					break;
				} else { /* heroism */
					(void)hp_player(10);
					(void)set_hero(p_ptr->hero + randint(25) + 25);
					(void)set_afraid(0);
					break;
				}
			}

			case 59:
			{
				if (mp_ptr->spell_type == 2) {
					/* shadow monsters */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_DARK, 5, 30 + (plev), 2);
					fire_beam(GF_FORCE, dir,
					    damroll(6+((plev-5)/4), 8));
					break;
				} else { /* shield */
					(void)set_shield(p_ptr->shield + randint(20) + 30);
					break;
				}
			}

			case 60:
			{
				if (mp_ptr->spell_type == 2) {
					/* shadow ball */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_DARK, dir,
					    40 + (plev), 2);
					break;
				} else { /* berserker */
					(void)hp_player(30);
					(void)set_shero(p_ptr->shero + randint(25) + 25);
					(void)set_afraid(0);
					break;
				}
			}

			case 61:
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
				break;
			}

			case 62:
			{
				if (mp_ptr->spell_type == 2) {
					/* shadow gate */
					teleport_player(plev * 7);
					break;
				} else { /* essence of speed */
					if (!p_ptr->fast)
						(void)set_fast(randint(30) + 30 + plev);
					else
						(void)set_fast(p_ptr->fast + randint(10));
					break;
					break;
				}
			}

			case 63:
			{
				if (mp_ptr->spell_type == 2) {
					/* summon shadows */
					summon_monster(SUMMON_SHADOWS);
					break;
				} else {
					/* globe of invulnerability */
					(void)set_invuln(p_ptr->invuln + randint(4) + 4);
					break;
				}
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
			gain_exp(e * s_ptr->slevel);
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
		cptr act = NULL;

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
 * Pray a prayer
 */
void do_cmd_pray(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int y, x;

	int item, sval, spell, dir, chance;
	int beam;

	int plev = p_ptr->lev;

	object_type *o_ptr;

	magic_type *s_ptr;

	cptr q, s;


	/* Must use prayer books */
	if((mp_ptr->spell_book != TV_PRAYER_BOOK) &&
	    (mp_ptr->spell_book != TV_NATURE_BOOK))
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
	item_tester_tval = mp_ptr->spell_book;

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

	/* Access the item's sval */
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


	/* Access the spell */
	s_ptr = &mp_ptr->info[spell];


	/* Verify "dangerous" prayers */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to recite this prayer.");

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
		/* Hack -- chance of "beam" instead of "bolt" */
		beam = (((p_ptr->pclass == CLASS_PRIEST) ||
		    (p_ptr->pclass == CLASS_DRUID)) ? plev : (plev / 2));

		switch (spell)
		{
			case 0: /* detect evil */
			{
				(void)detect_monsters_evil();
				break;
			}

			case 1: /* cure wounds */
			{
				(void)do_cure_wounds();
				break;
			}

			case 2:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* poison bolt */
	 				if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam-10, GF_POIS, dir,
					    damroll(3 + ((plev - 1) / 5), 4));
					break;
				} else { /* bless */
					(void)set_blessed(p_ptr->blessed + randint(12) + 12);
					break;
				}
			}

			case 3: /* remove fear */
			{
				(void)set_afraid(0);
				break;
			}

			case 4: /* call light */
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 5: /* find traps */
			{
				(void)detect_traps();
				break;
			}

			case 6: /* detect doors/stairs */
			{
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 7: /* slow poison */
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				break;
			}

			case 8:
			{
				if(p_ptr->pclass == CLASS_DRUID) {
					/* sleep */
					(void)do_sleep_monster();
					break;
				} else { /* scare monster */
					if (!get_aim_dir(&dir)) return;
					(void)do_fear_monster();
					break;
				}
			}

			case 9: /* portal */
			{
				teleport_player(plev * 3);
				break;
			}

			case 10: /* duststorm */
			{
				fire_ball(GF_FORCE, 5, 3 + (plev / 4), 6);
				break;
			}

			case 11:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* cool lava */
					alter_terrain(py,px,plev,1);
					break;
				} else { /* chant */
					(void)set_blessed(p_ptr->blessed + randint(24) + 24);
					break;
				}
			}

			case 12:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* poison cloud */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_POIS, dir, 9 + (plev / 2), 3);
					break;
				} else { /* sanctuary */
					(void)do_sleep_monster();
					break;
				}
			}

			case 13: /* satisfy hunger */
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 14: /* remove curse */
			{
				remove_curse();
				break;
			}

			case 15: /* resist heat and cold */
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
				break;
			}

			case 16: /* levitate */
			{
				if (p_ptr->levitate == 0)
					p_ptr->levitate = p_ptr->levitate + randint(100);
				break;
			}

			case 17:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* charm monster */
					if (!get_aim_dir(&dir)) return;
					charm_monster(dir, plev);
					break;
				} else { /* orb of draining */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_HOLY_ORB, dir, (damroll(3, 6) + plev +
					     (plev / ((p_ptr->pclass == 2) ? 2 : 4))),
					     ((plev < 30) ? 2 : 3));
					break;
				}
			}

			case 18: /* disintegrate */
			{
				alter_terrain(py,px,plev,3);
				break;
			}

			case 19:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* grow trees */
					alter_terrain(py,px,1,4);
					break;
				} else { /* sense invisible */
					(void)set_tim_s_invis(p_ptr->tim_s_invis + randint(24) + 24);
					break;
				}
			}

			case 20: /* protection from evil */
			{
				(void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
				break;
			}

			case 21:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* sense invisible */
					(void)set_tim_s_invis(p_ptr->tim_s_invis + randint(24) + 24);
					break;
				} else { /* earthquake */
					earthquake(py, px, 10);
					break;
				}
			}

			case 22: /* sense surroundings */
			{
				map_area();
				break;
			}

			case 23:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* lower water */
					alter_terrain(py,px,plev,2);
					break;
				} else { /* cure mortal wounds */
					(void)hp_player(damroll(8, 10));
					(void)set_stun(0);
					(void)set_cut(0);
					break;
				}
			}

			case 24:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* turn animals */
					(void)turn_animals();
					break;
				} else { /* turn undead */
					(void)turn_undead();
					break;
				}
			}

			case 25:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* fear */
					(void)do_fear_monster();
					break;
				} else { /* prayer */
					(void)set_blessed(p_ptr->blessed + randint(48) + 48);
					break;
				}
			}

			case 26:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* call wolves */
					summon_monster(SUMMON_WOLVES);
					break;
				} else { /* dispel undead */
					(void)dispel_undead(plev * 3);
					break;
				}
			}

			case 27: /* heal */
			{
				(void)hp_player(300);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 28:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* analyze monster */
					(void) probing();
					break;
				} else { /* dispel evil */
					(void)dispel_evil(plev * 3);
					break;
				}
			}

			case 29: /* glyph of warding */
			{
				warding_glyph();
				break;
			}

			case 30:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* detection */
					(void)detect_all();
					break;
				} else { /* holy word */
					(void)dispel_evil(plev * 4);
					(void)hp_player(1000);
					(void)set_afraid(0);
					(void)set_poisoned(0);
					(void)set_stun(0);
					(void)set_cut(0);
					break;
				}
			}

			case 31:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* nexus bolt */
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam, GF_NEXUS, dir,
					    damroll(8+((plev-5)/4), 8));
					break;
				} else { /* summon angel */
					summon_monster(SUMMON_ANGEL);
					break;
				}
			}

			case 32: /* detect monsters */
			{
				(void)detect_monsters_normal();
				break;
			}

			case 33:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* map terrain */
					for(y=0;y < DUNGEON_HGT;y++) {
						for(x=0;x < DUNGEON_WID;x++) {
							if ((cave_feat[y][x] >= FEAT_DEEP_WATER) &&
							    (cave_feat[y][x] <= FEAT_MOUNTAIN)) {
								cave_info[y][x] |= (CAVE_GLOW);
								cave_info[y][x] |= (CAVE_MARK);
							}
						}
					}
					/* Redraw map */
					p_ptr->redraw |= (PR_MAP);

					break;
				} else { /* detection */
					(void)detect_all();
					break;
				}
			}

			case 34: /* perception */
			{
				(void)ident_spell();
				break;
			}

			case 35: /* probing */
			{
				(void)probing();
				break;
			}

			case 36: /* clairvoyance */
			{
				wiz_lite();
				break;
			}

			case 37:
			{
				/* resist nether */
				(void)set_oppose_neth(p_ptr->oppose_neth + randint(20) + 20);
				break;
			}

			case 38:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* fear */
					(void)do_fear_monster();
					break;
				} else { /* cure wounds */
	 				(void)do_cure_wounds();
					break;
				}
			}

			case 39: /* cure mortal wounds */
			{
				(void)hp_player(damroll(8, 10));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 40:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* forest */
					flood(py, px, 20,2);
					break;
				} else { /* *healing* */
					(void)hp_player(2000);
					(void)set_stun(0);
					(void)set_cut(0);
					break;
				}
			}

			case 41: /* restoration */
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				break;
			}

			case 42:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* faithfulness */
					if (!get_aim_dir(&dir)) return;
					pet_monster(dir, plev);
					break;
				} else { /* remembrance */
					(void)restore_level();
					break;
				}
			}

			case 43:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* firestorm */
					fire_ball(GF_FIRE, 5,
					    35 + (plev * 2), 10);
					break;
				} else { /* dispel demons */
					(void)dispel_undead(plev * 6);
					break;
				}
			}

			case 44:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* fissure */
					(void) fissure(FEAT_WALL_EXTRA);
					(void) fissure(FEAT_WALL_EXTRA);
					break;
				} else { /* dispel undead */
					(void)dispel_undead(plev * 4);
					break;
				}
			}

			case 45:
			{
				if(p_ptr->pclass == CLASS_DRUID) {
					/* tornado */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_FORCE, dir,
					    60 + (plev), 2);
					break;
				} else { /* dispel evil */
					(void)dispel_evil(plev * 4);
					break;
				}
			}

			case 46:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* earthquake */
					earthquake(py, px, 10);
					break;
				} else { /* banishment */
					if (banish_evil(100))
					{
						msg_print("The power of your god banishes evil!");
					}
					break;
				}
			}

			case 47:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* flood */
					flood(py, px, 20,1);
					break;
				} else { /* word of destruction */
					destroy_area(py, px, 15, TRUE);
					break;
				}
			}

			case 48:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* nexus ball */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_NEXUS, dir,
					    70 + (plev), 2);
					break;
				} else { /* annihilation */
					if (!get_aim_dir(&dir)) return;
					drain_life(dir, 200);
					break;
				}
			}

			case 49: /* balefire */
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_BALEFIRE, dir,
				    damroll(15+((plev-5)/3), 10));
				break;
			}

			case 50: /* unbarring ways */
			{
				(void)destroy_doors_touch();
				break;
			}

			case 51: /* recharging */
			{
				(void)recharge((plev * 2));
				break;
			}

			case 52:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* Eruption */
					alter_terrain(py,px,plev,5);
					break;
				} else { /* dispel curse */
					(void)remove_all_curse();
					break;
				}
			}

			case 53:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* sustain self */
					p_ptr->tim_sus_str = p_ptr->tim_sus_str + 100;
					p_ptr->tim_sus_int = p_ptr->tim_sus_int + 100;
					p_ptr->tim_sus_wis = p_ptr->tim_sus_wis + 100;
					p_ptr->tim_sus_dex = p_ptr->tim_sus_dex + 100;
					p_ptr->tim_sus_con = p_ptr->tim_sus_con + 100;
					p_ptr->tim_sus_chr = p_ptr->tim_sus_chr + 100;
					msg_print("You feel complete and unchangeable!");
					break;
				} else { /* enchant weapon */
					(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
					break;
				}
			}

			case 54:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* brand ammo */
					(void)brand_ammo(2,0);
					break;
				} else { /* enchant armor */
					(void)enchant_spell(0, 0, rand_int(3) + 2);
					break;
				}
			}

			case 55: /* elemental brand */
			{
				brand_weapon();
				break;
			}

			case 56: /* imprision */
			{
				if (!get_aim_dir(&dir)) return;
				imprision(FEAT_WALL_EXTRA,dir,p_ptr->lev/10);
				break;
			}

			case 57: /* blink */
			{
				teleport_player(10);
				break;
			}

			case 58: /* teleport self */
			{
				teleport_player(plev * 8);
				break;
			}

			case 59: /* teleport other */
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 60:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* summon animals */
					summon_monster(SUMMON_ANIMALS);
					summon_monster(SUMMON_ANIMALS);
					break;
				} else { /* teleport level */
					(void)teleport_player_level();
					break;
				}
			}

			case 61: /* word of recall */
			{
				if (p_ptr->word_recall == 0)
				{
					p_ptr->word_recall = rand_int(20) + 15;
					msg_print("The air about you becomes charged...");
				}
				else
				{
					p_ptr->word_recall = 0;
					msg_print("A tension leaves the air around you...");
				}
				break;
			}

			case 62: /* alter reality */
			{
				msg_print("The world changes!");

				/* Leaving */
				p_ptr->leaving = TRUE;

				break;
			}

			case 63:
			{
				if (p_ptr->pclass == CLASS_DRUID) {
					/* drown */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_WATER, dir,
					    100 + (plev * 2), 3);
					break;
				} else { /* immolation */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_FIRE, dir,
					    100 + (plev * 2), 3);
					break;
				}
			}

		}

		/* A prayer was prayed */
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
			gain_exp(e * s_ptr->slevel);
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
