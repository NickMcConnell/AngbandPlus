/* File: cmd5.c */

/* Purpose: Realmed Class commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */


#include "angband.h"


/* Maximum number of tries for teleporting */
#define MAX_TRIES 300

/* Does it contains a schooled spell ? */
static bool hook_school_spellable(object_type *o_ptr)
{
        if (o_ptr->tval == TV_BOOK)
                return TRUE;
        else
        {
                u32b f1, f2, f3, f4, f5, esp;
                
                /* Extract object flags */
                object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

                if ((f5 & TR5_SPELL_CONTAIN) && (o_ptr->pval2 != -1))
                        return TRUE;
        }
        return FALSE;
}

/* Is it a book */
bool item_tester_hook_browsable(object_type *o_ptr)
{
        if (hook_school_spellable(o_ptr)) return TRUE;
	if (o_ptr->tval >= TV_BOOK) return TRUE;
	return FALSE;
}

bool item_tester_hook_realmable(object_type *o_ptr)
{
	if (o_ptr->tval > TV_BOOK) return TRUE;
	return FALSE;
}

/*
 * Increase a spell power
 * Adds spell power * 10%
 */
s32b apply_power_dam(s32b dam, int lvl, byte power)
{
	if (lvl < 0) lvl = 0;
	if (lvl > 50) lvl = 50;

	lvl *= power;

	dam += (lvl * dam) / 10;

	return (dam);
}


/*
 * Adds spell power dice
 */
s32b apply_power_dice(s32b dice, int lvl, byte power)
{
	if (lvl < 0) lvl = 0;
	if (lvl > 50) lvl = 50;

	lvl *= power;

	dice += lvl;

	return (dice);
}


/*
 * Adds spell power parts
 */
s32b apply_power_dur(s32b dur, int lvl, byte power)
{
	if (lvl < 0) lvl = 0;
	if (lvl > 50) lvl = 50;

	dur += (dur / power) * lvl;

	return (dur);
}


/*
 * Get the spell level of a spell
 */
byte get_spell_level(int realm, int spell)
{
	magic_type *s_ptr = &realm_info[realm][spell];


	if (!((get_skill(SKILL_SORCERY) >= s_ptr->slevel) &&
	      (is_sorcery_realm(realm))))
	{
		return (spell_level[realm][spell]);
	}
	else
	{
		return (1);
	}
}


#if 0

/*
 * Hook to determine if an object is drainable
 */
static bool item_tester_hook_scroll_amulet(object_type *o_ptr)
{
	if ((o_ptr->tval == TV_AMULET) || (o_ptr->tval == TV_SCROLL)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

#endif


/*
 * Are we using a mage staff
 */
bool is_magestaff()
{
	int i;


	i = 0;

	while (p_ptr->body_parts[i] == INVEN_WIELD)
	{
		object_type *o_ptr = &inventory[INVEN_WIELD + i];

		/* Wielding a mage staff */
		if ((o_ptr->k_idx) && (o_ptr->tval == TV_MSTAFF)) return (TRUE);

		/* Next slot */
		i++;

		/* Paranoia */
		if (i >= (INVEN_TOTAL - INVEN_WIELD)) break;
	}

	/* Not wielding a mage staff */
	return (FALSE);
}

int get_realm_skill(int realm)
{
	int skill = -1;

	/* Analyse the realm */
	switch (realm)
	{
		case REALM_SYMBIOTIC:
		{
			skill = SKILL_SYMBIOTIC;
			break;
		}

		case REALM_MUSIC:
		{
			skill = SKILL_MUSIC;
			break;
		}

		case REALM_DRUID:
		{
			skill = SKILL_DRUID;
			break;
		}

		case REALM_DAEMON:
		{
			skill = SKILL_DAEMON;
			break;
		}

		/* No realms */
		default:
		{
			return (0);
		}
	}
	return (skill);
}

int get_max_spell_realm(int realm)
{
	int skill, sp = 0, i;

	magic_type *s_ptr;

	skill = get_realm_skill(realm);
	if (skill == -1) return (0);

	sp = get_skill_scale(skill, 64);
	for (i = 0; i < 64; i++)
	{
		s_ptr = &realm_info[realm][i];

		if (spell_learned[realm][i / 32] & BIT(i % 32)) sp -= get_spell_level(realm, i);
	}

        /* Less than 0 ? */
	if (sp < 0)
	{
		msg_format("Warning %d spells in realm %d. -- please report the error to the maintainer", sp, realm);
		sp = 0;
	}

	/* Return the number */
	return (sp);
}


/*
 * Return principal stat for the given realm
 */
int get_realm_stat(int realm)
{
	/* Analyse the realm */
	switch (realm)
	{
		case REALM_SYMBIOTIC:
		{
			return(A_INT);
		}

		case REALM_MUSIC:
		{
			return(A_CHR);
		}
		case REALM_DRUID:
		{
			return(A_WIS);
		}

		case REALM_DAEMON:
		{
			return(A_INT);
		}

		default:
		{
			return (0);
		}
	}
}

/* Skills that induce spell learning */
bool must_learn_spells()
{
        int i;

        for (i = 0; i < max_s_idx; i++)
        {
                if (s_info[i].value)
                        switch (i)
                        {
                                case SKILL_SYMBIOTIC:
                                case SKILL_MUSIC:
                                case SKILL_DRUID:
                                case SKILL_DAEMON:
                                        return TRUE;
                        }
        }
        return FALSE;
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

bool get_spell_all_hack = FALSE;

static int get_spell(int *sn, cptr prompt, int sval, bool known,
	object_type *o_ptr, bool allow_list)
{
	int realm = o_ptr->tval - TV_SYMBIOTIC_BOOK + 1;

	int i;

	u32b spell = -1;

	int num = 0;

	int ask;

	byte spells[64];

	bool flag, redraw, okay;

	char choice;

	magic_type *s_ptr;

	char out_val[160];

	cptr p;


	p = "spell";

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (get_spell_all_hack || spell_okay(*sn, known, realm))
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT -- TNB */

	/* Extract spells */
	for (spell = 0; spell < 64; spell++)
	{
		/* Check for this spell */
		if (fake_spell_flags[realm][sval][spell < 32] & BIT(spell % 32))
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
		if (get_spell_all_hack || spell_okay(spells[i], known, realm))
		{
			okay = TRUE;
		}
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
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
	}


	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
	        p, I2A(0), I2A(num - 1), prompt, p);

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if (((choice == ' ') || (choice == '*') || (choice == '?')) &&
		    allow_list)
		{
			/* Show the list */
			if (!redraw)
			{
				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				character_icky = TRUE;
				Term_save();

				/* Display a list of spells */
				print_spells(spells, num, 1, 13, realm);
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
				character_icky = FALSE;
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

		/* Require "okay" spells */
		if (!spell_okay(spell, known, realm) && !get_spell_all_hack)
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
			s_ptr = &realm_info[realm][spell % 64];

			/* Prompt */
			strnfmt(tmp_val, 78, "Level %d %s %s (%d mana, %d%% fail)? ",
			        prompt, get_spell_level(realm, spell),
			        spell_names[realm][spell % 64][0],
			        s_ptr->smana, spell_chance(spell, realm));

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}


	/* Restore the screen */
	if (redraw)
	{
		Term_load();
		character_icky = FALSE;
	}


	/* Show choices */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
	}


	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = spell;

#ifdef ALLOW_REPEAT /* TNB */

	repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}


/*
 * Peruse the spells/prayers in a book
 *
 * Note that *all* spells in the book are listed
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */

/*
 * Helper function for browsing
 */
void do_cmd_browse_aux(object_type *o_ptr)
{
	int sval, line, col = 13;

	int realm = o_ptr->tval - TV_SYMBIOTIC_BOOK + 1;

	int spell = -1;

	int num = 0;

	cptr desc;

	byte spells[64];

        u32b f1, f2, f3, f4, f5, esp;


        if (o_ptr->tval == TV_BOOK)
	{
		browse_school_spell(o_ptr->sval, o_ptr->pval);
		return;
	}
                
        /* Extract object flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);
        if (f5 & TR5_SPELL_CONTAIN)
	{
                browse_school_spell(255, o_ptr->pval2);
		return;
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
		if (fake_spell_flags[realm][sval][spell < 32] & BIT(spell % 32))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}


	/* Save the screen */
	character_icky = TRUE;
	Term_save();

	/* Display the spells */
	line = print_spells(spells, num, 1, 13, realm);

	/* Clear the top line */
	prt("", 0, 0);

	get_spell_all_hack = TRUE;

	while (TRUE)
	{
		col = 13;

		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "browse", o_ptr->sval, TRUE, o_ptr, FALSE))
		{
			/* If cancelled, leave immediately. */
			if (spell != -1)
			{
				/* Notify that there's nothing to see, and wait. */
				c_put_str(TERM_SLATE, "No spells to browse     ", 0, 11);

				/* Any key cancels if no spells are available. */
				inkey();
			}

			/* Restore the screen */
			Term_load();
			character_icky = FALSE;

			get_spell_all_hack = FALSE;

			return;
		}

		/* Clear lines, position cursor  (really should use strlen here) */
		Term_erase(0, line, 255);

		/* Display that spell's information. */
		desc = spell_names[realm][spell][1];

		if (strlen(desc) > 80 - col)
		{
			col = 80 - strlen(desc);
		}
		c_prt(TERM_L_BLUE, desc, line, col);
	}
}


void do_cmd_browse(void)
{
	int item;

	cptr q, s;

	object_type *o_ptr;

	/* Restrict choices to "useful" books */
	item_tester_hook = item_tester_hook_browsable;

	/* Get an item */
	q = "Browse which book? ";
	s = "You have no books that you can read.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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

	do_cmd_browse_aux(o_ptr);
}


/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int item, sval;

	int realm;

	int spell = -1;

	int i;

	cptr p;

	object_type *o_ptr;

	cptr q, s;


	/* Must be able to read */
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	/* Too confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	p = "spell";

	/* Restrict choices to "useful" books */
	item_tester_hook = item_tester_hook_realmable;

	/* Get an item */
	q = "Study which book? ";
	s = "You have no books that you can read.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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

	/* Access the realm of the spellbook */
	realm = o_ptr->tval - TV_SYMBIOTIC_BOOK + 1;

	if ((i = get_max_spell_realm(realm)) == 0)
	{
		cmsg_format(TERM_L_RED,
		            "You cannot learn any %s levels in this realm.", p);
		return;
	}
	else
	{
		cmsg_format(TERM_L_BLUE,
		            "You can learn %d %s levels in this realm.", i, p);
	}

	/* Access the item's sval */
	sval = o_ptr->sval;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Ask for a spell, allow cancel */
	if (!get_spell(&spell, "study", sval, FALSE, o_ptr, TRUE) && (spell == -1))
	{
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

	if ((get_skill(SKILL_SORCERY) >= realm_info[realm][spell].slevel) &&
	    (is_sorcery_realm(realm)))
	{
		msg_format("This %s is under the control of the Sorcery skill, "
		           "you cannot increase it.", p);
		return;
	}

	if (realm_info[realm][spell].slevel > p_ptr->lev)
	{
		msg_format("This %s is too difficult for you.", p);
		return;
	}

	/* Take a turn */
	energy_use = 100;

	/* Increase spell power */
	if (spell_learned[realm][spell / 32] & BIT(spell % 32))
	{
		spell_level[realm][spell]++;

		msg_format("You have increased your knowledge of the %s of %s.",
		           p, spell_names[realm][spell % 64][0]);
	}
	/* Learn the spell */
	else
	{
		spell_learned[realm][spell / 32] |= BIT(spell % 32);
		spell_level[realm][spell] = 1;

		/* Mention the result */
		msg_format("You have learned the %s of %s.",
		           p, spell_names[realm][spell % 64][0]);
	}

	/* Sound */
	sound(SOUND_STUDY);
}


void do_poly_wounds(void)
{
	/* Changed to always provide at least _some_ healing */
	s16b wounds = p_ptr->cut;

	s16b hit_p = (p_ptr->mhp - p_ptr->chp);

	s16b change = damroll(p_ptr->lev, 5);

	bool Nasty_effect = (randint(5)==1);


	if (!(wounds || hit_p || Nasty_effect)) return;

	msg_print("Your wounds are polymorphed into less serious ones.");
	hp_player(change);
	if (Nasty_effect)
	{
		msg_print("A new wound was created!");
		take_hit(change/2, "a polymorphed wound");
		set_cut(change);
	}
	else
	{
		set_cut((p_ptr->cut)-(change/2));
	}
}

void do_poly_self(void)
{
	int power = p_ptr->lev;
	int poly_power;

	msg_print("You feel a change coming over you...");

	if ((power > rand_int(20)) && (rand_int(3) == 0))
	{
		char effect_msg[80] = "";
		int new_race, expfact, goalexpfact;

		/* Some form of racial polymorph... */
		power -= 10;

		if ((power > rand_int(5)) && (rand_int(4) == 0))
		{
			/* sex change */
			power -= 2;

			if (p_ptr->psex == SEX_MALE)
			{
				p_ptr->psex = SEX_FEMALE;
				sp_ptr = &sex_info[p_ptr->psex];
				strcpy(effect_msg, "female");
			}
			else
			{
				p_ptr->psex = SEX_MALE;
				sp_ptr = &sex_info[p_ptr->psex];
				strcpy(effect_msg, "male");
			}
		}

		if ((power > rand_int(30)) && (rand_int(5) == 0))
		{
			int tmp = 0;

			/* Harmful deformity */
			power -= 15;

			while (tmp < 6)
			{
				if ( rand_int(2) == 0)
				{
					(void)dec_stat(tmp, randint(6) + 6, (rand_int(3) == 0));
					power -= 1;
				}
				tmp++;
			}

			/* Deformities are discriminated against! */
			(void)dec_stat(A_CHR, randint(6), TRUE);

			if (effect_msg[0])
			{
				char tmp_msg[10];
				strnfmt(tmp_msg, 10, "%s", effect_msg);
				strnfmt(effect_msg, 80, "deformed %s", tmp_msg);
			}
			else
			{
				strcpy(effect_msg, "deformed");
			}
		}

		while ((power > rand_int(20)) && (rand_int(10) == 0))
		{
			/* Polymorph into a less corrupted form */
			power -= 10;

			lose_corruption(0);
		}

		/*
		 * I'm not sure 'power' is always positive, with *so* many minuses.
		 * Also, passing zero / negative numbers to randint/rand_int can
		 * cause a zero divide exception, IIRC, not to speak of its absurdity
		 * -- pelpel
		 */
		poly_power = (power > 1) ? power : 1;

		/*
		 * Restrict the race choices by exp penalty so weak polymorph
		 * always means weak race
		 */
		goalexpfact = 100 + 3 * rand_int(poly_power);

		/* Roll until an appropriate selection is made */
		while (1)
		{
			new_race = rand_int(max_rp_idx);
			expfact = race_info[new_race].r_exp;

			if ((new_race != p_ptr->prace) && (expfact <= goalexpfact)) break;
		}

		if (effect_msg[0])
		{
			msg_format("You turn into a%s %s!",
			           ((is_a_vowel(rp_name[race_info[new_race].title]))?"n":""),
			           race_info[new_race].title + rp_name);
		}
		else
		{
			msg_format("You turn into a %s %s!", effect_msg,
			           race_info[new_race].title);
		}

		p_ptr->prace = new_race;
		rp_ptr = &race_info[p_ptr->prace];

		/* Experience factor */
		p_ptr->expfact = rp_ptr->r_exp + rmp_ptr->r_exp + cp_ptr->c_exp;

		/* Calculate the height/weight */
		get_height_weight();


		check_experience();
		p_ptr->max_plv = p_ptr->lev;

		p_ptr->redraw |= (PR_BASIC);

		p_ptr->update |= (PU_BONUS);

		handle_stuff();
		lite_spot(py, px);
	}

	if ((power > rand_int(30)) && (rand_int(6) == 0))
	{
		int tmp = 0;

		/* Abomination! */
		power -= 20;

		msg_print("Your internal organs are rearranged!");
		while (tmp < 6)
		{
			(void)dec_stat(tmp, randint(6) + 6, (rand_int(3) == 0));
			tmp++;
		}
		if (rand_int(6) == 0)
		{
			msg_print("You find living difficult in your present form!");
			take_hit(damroll(randint(10),p_ptr->lev), "a lethal corruption");
			power -= 10;
		}
	}

	if ((power > rand_int(20)) && (rand_int(4) == 0))
	{
		power -= 10;

		do_cmd_rerate();
	}

	while ((power > rand_int(15)) && (rand_int(3) == 0))
	{
		power -= 7;
		(void) gain_random_corruption(0);
	}

	if (power > rand_int(5))
	{
		power -= 5;
		do_poly_wounds();
	}

	/* Note: earlier deductions may have left power < 0 already. */
	while (power > 0)
	{
		corrupt_player();
		power--;
	}
}


/*
 * Brand the current weapon
 */
void brand_weapon(int brand_type)
{
	object_type *o_ptr;

	cptr act = NULL;

	char o_name[80];


	o_ptr = &inventory[INVEN_WIELD];

	/*
	 * You can never modify artifacts / ego-items
	 * You can never modify cursed items
	 *
	 * TY: You _can_ modify broken items (if you're silly enough)
	 */
	if (!o_ptr->k_idx || artifact_p(o_ptr) || ego_item_p(o_ptr) ||
		o_ptr->art_name || cursed_p(o_ptr))
	{
		if (flush_failure) flush();

		msg_print("The Branding failed.");
	}


	/* Save the old name */
	object_desc(o_name, o_ptr, FALSE, 0);

	switch (brand_type)
	{
		case 6:
		{
			act = "glows with godly power.";
			o_ptr->name2 = EGO_BLESS_BLADE;
			o_ptr->pval = randint(4);

			break;
		}
		case 5:
		{
			act = "seems very powerful.";
			o_ptr->name2 = EGO_EARTHQUAKES;
			o_ptr->pval = randint(3);

			break;
		}
		case 4:
		{
			act = "seems very unstable now.";
			o_ptr->name2 = EGO_DRAGON;
			o_ptr->pval = randint(2);

			break;
		}
		case 3:
		{
			act = "thirsts for blood!";
			o_ptr->name2 = EGO_VAMPIRIC;

			break;
		}
		case 2:
		{
			act = "is coated with poison.";
			o_ptr->name2 = EGO_BRAND_POIS;

			break;
		}
		case 1:
		{
			act = "is engulfed in raw chaos!";
			o_ptr->name2 = EGO_CHAOTIC;

			break;
		}
		default:
		{
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
		}
	}

	/* Apply the ego */
	apply_magic(o_ptr, dun_level, FALSE, FALSE, FALSE);
	o_ptr->discount =  100;

	msg_format("Your %s %s", o_name, act);

	enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
}


#if 0

static void call_the_void(void)
{
	int i;

	cptr verb;

	cptr obj;


	verb = "cast";
	obj = "spell";

	/* XXX XXX XXX */
	if (cave_floor_bold(py-1,px-1) && cave_floor_bold(py-1, px) &&
		cave_floor_bold(py-1,px+1) && cave_floor_bold(py,px-1) &&
		cave_floor_bold(py,px+1) && cave_floor_bold(py+1,px-1) &&
		cave_floor_bold(py+1,px) && cave_floor_bold(py+1,px+1))
	{
		for (i = 1; i < 10; i++)
		{
			if (i-5) fire_ball(GF_ROCKET, i, 175, 2);
		}

		for (i = 1; i < 10; i++)
		{
			if (i-5) fire_ball(GF_MANA, i, 175, 3);
		}

		for (i = 1; i < 10; i++)
		{
			if (i-5) fire_ball(GF_NUKE, i, 175, 4);
		}
	}
	else
	{
		msg_format("You %s the %s too close to a wall!", verb, obj);
		msg_print("There is a loud explosion!");

		/* Prevent destruction of quest levels and town */
		if (!is_quest(dun_level) && dun_level)
		{
			destroy_area(py, px, 20+(p_ptr->lev), TRUE, FALSE);
			msg_print("The dungeon collapses...");
		}
		else
		{
			msg_print("The dungeon trembles...");
		}
		take_hit(100 + (randint(150)), "a suicidal Call the Void");
	}
}

#endif


/*
 * Fetch an item (teleport it right underneath the caster)
 */
void fetch(int dir, int wgt, bool require_los)
{
	int ty, tx, i;

	bool flag;

	cave_type *c_ptr;

	object_type *o_ptr;

	char o_name[80];


	/* Check to see if an object is already there */
	if (cave[py][px].o_idx)
	{
		msg_print("You can't fetch when you're already standing on something.");
		return;
	}

	/* Use a target */
	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;

		if (distance(py, px, ty, tx) > MAX_RANGE)
		{
			msg_print("You can't fetch something that far away!");
			return;
		}

		c_ptr = &cave[ty][tx];

		if (!c_ptr->o_idx)
		{
			msg_print("There is no object at this place.");
			return;
		}

		if (require_los && (!player_has_los_bold(ty, tx)))
		{
			msg_print("You have no direct line of sight to that location.");
			return;
		}
	}
	else
	{
		/* Use a direction */
		ty = py; /* Where to drop the item */
		tx = px;
		flag = FALSE;

		while (1)
		{
			ty += ddy[dir];
			tx += ddx[dir];
			c_ptr = &cave[ty][tx];

			if ((distance(py, px, ty, tx) > MAX_RANGE) ||
			    !cave_floor_bold(ty, tx)) return;

			if (c_ptr->o_idx) break;
		}
	}

	o_ptr = &o_list[c_ptr->o_idx];

	if (o_ptr->weight > wgt)
	{
		/* Too heavy to 'fetch' */
		msg_print("The object is too heavy.");
		return;
	}

	i = c_ptr->o_idx;
	c_ptr->o_idx = o_ptr->next_o_idx;
	cave[py][px].o_idx = i; /* 'move' it */
	o_ptr->next_o_idx = 0;
	o_ptr->iy = py;
	o_ptr->ix = px;

	object_desc(o_name, o_ptr, TRUE, 0);
	msg_format("%^s flies through the air to your feet.", o_name);

	note_spot(py,px);
	p_ptr->redraw |= PR_MAP;
}


/*
 * Handle random effects of player shrieking
 */
void shriek_effect()
{
	switch(randint(9))
	{
		case 1: case 5: case 8: case 9:
		{
			msg_print("You made a high pitched shriek!");
			aggravate_monsters(1);

			break;
		}
		case 2: case 6:
		{
			msg_print("Oups! You call a monster.");
			summon_specific(py, px, max_dlv[dungeon_type], 0);

			break;
		}
		case 3: case 7:
		{
			msg_print("The dungeon collapses!");
			earthquake(py, px, 5);

			break;
		}
		case 4:
		{
			msg_print("Your shriek is so horrible that you damage your health!");
			take_hit(damroll(p_ptr->lev / 5, 8),"inner hemorrhaging");

			break;
		}
	}
}


/*
 * Like all the random effect codes, this is *ugly*,
 * and there is not a single line of comment, so I can't tell
 * some fall throughs are really intended. Well, I know it's
 * intended to be bizarre :) -- pelpel
 */
void wild_magic(int spell)
{
	int counter = 0;
	int type = SUMMON_BIZARRE1 - 1 + randint(6);

	if (type < SUMMON_BIZARRE1) type = SUMMON_BIZARRE1;
	else if (type > SUMMON_BIZARRE6) type = SUMMON_BIZARRE6;

	switch (randint(spell) + randint(8) + 1)
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
			unlite_area(10, 3);

			break;
		}

		case 12:
		case 13:
		case 14:
		{
			lite_area(damroll(2, 3), 2);

			break;
		}

		case 15:
		{
			destroy_doors_touch();

			break;
		}

		case 16: case 17:
		{
			wall_breaker();

			/* I don't think this is a fall through -- pelpel */
			break;
		}

		case 18:
		{
			sleep_monsters_touch();

			break;
		}

		case 19:
		case 20:
		{
			trap_creation();

			break;
		}

		case 21:
		case 22:
		{
			door_creation();

			break;
		}

		case 23:
		case 24:
		case 25:
		{
			aggravate_monsters(1);

			break;
		}

		case 26:
		{
			/* Prevent destruction of quest levels and town */
			if (!is_quest(dun_level) && dun_level)
				earthquake(py, px, 5);

			break;
		}

		case 27:
		case 28:
		{
			(void) gain_random_corruption(0);

			break;
		}

		case 29:
		case 30:
		{
			apply_disenchant(0);

			break;
		}

		case 31:
		{
			lose_all_info();

			break;
		}

		case 32:
		{
			fire_ball(GF_CHAOS, 0, spell + 5, 1 + (spell / 10));

			break;
		}

		case 33:
		{
			wall_stone(py, px);

			break;
		}

		case 34:
		case 35:
		{
			while (counter++ < 8)
			{
				(void) summon_specific(py, px, (dun_level * 3) / 2, type);
			}

			break;
		}

		case 36:
		case 37:
		{
			activate_hi_summon();

			break;
		}

		case 38:
		{
			summon_cyber();

			/* I don't think this is a fall through -- pelpel */
			break;
		}

		default:
		{
			activate_ty_curse();
		}
	}

	return;
}


/*
 * Hack -- Determine if the player is wearing an artefact ring
 * specified by art_type, that should be an index into a_info
 */
bool check_ring(int art_type)
{
	int i;


	/* We are only interested in ring slots */
	i = INVEN_RING;

	/* Scan the list of rings until we reach the end */
	while (p_ptr->body_parts[i - INVEN_WIELD] == INVEN_RING)
	{
		/* Found the ring we were looking for */
		if (inventory[i].k_idx && (inventory[i].name1 == art_type))
		{
			return (TRUE);
		}

		/* Next item */
		i++;
	}

	/* Found nothing */
	return (FALSE);
}

/*
 * Use a power of the monster in symbiosis
 */
int use_symbiotic_power(int r_idx, bool great, bool only_number, bool no_cost)
{
	int power = -1;

	int num = 0, dir = 0 , i;

	int powers[96];

	bool flag, redraw;

	int ask, plev = p_ptr->lev;

	char choice;

	char out_val[160];

	monster_race *r_ptr = &r_info[r_idx];

	int rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	int x = px, y = py, k;

	int rad;

	int label;


	/* List the monster powers -- RF4_* */
	for (i = 0; i < 32; i++)
	{
		if (r_ptr->flags4 & BIT(i))
		{
			if (monster_powers[i].great && (!great)) continue;
			if (!monster_powers[i].power) continue;
			powers[num++] = i;
		}
	}

	/* List the monster powers -- RF5_* */
	for (i = 0; i < 32; i++)
	{
		if (r_ptr->flags5 & BIT(i))
		{
			if (monster_powers[i + 32].great && (!great)) continue;
			if (!monster_powers[i + 32].power) continue;
			powers[num++] = i + 32;
		}
	}

	/* List the monster powers -- RF6_* */
	for (i = 0; i < 32; i++)
	{
		if (r_ptr->flags6 & BIT(i))
		{
			if (monster_powers[i + 64].great && (!great)) continue;
			if (!monster_powers[i + 64].power) continue;
			powers[num++] = i + 64;
		}
	}

	if (!num)
	{
		msg_print("You have no powers you can use.");
		return (0);
	}

	if (only_number) return (num);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Get the last label */
	label = (num <= 26) ? I2A(num - 1) : I2D(num - 1 - 26);

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78,
	        "(Powers a-%c, *=List, ESC=exit) Use which power of your monster? ",
	        label);

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				character_icky = TRUE;
				Term_save();

				prt ("", y++, x);

				while (ctr < num)
				{
					monster_power *mp_ptr = &monster_powers[powers[ctr]];
					int mana = mp_ptr->mana / 10;

					if (!mana) mana = 1;

					if (mana > p_ptr->msp) mana = p_ptr->msp;

					label = (ctr < 26) ? I2A(ctr) : I2D(ctr - 26);

					if (!no_cost)
					{
						strnfmt(dummy, 80, " %c) %2d %s",
						        label, mana, mp_ptr->name);
					}
					else
					{
						strnfmt(dummy, 80, " %c) %s",
						        label, mp_ptr->name);
					}

					if (ctr < 17)
					{
						prt(dummy, y + ctr, x);
					}
					else
					{
						prt(dummy, y + ctr - 17, x + 40);
					}

					ctr++;
				}

				if (ctr < 17)
				{
					prt ("", y + ctr, x);
				}
				else
				{
					prt ("", y + 17, x);
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
				character_icky = FALSE;
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
			/* Can't uppercase digits XXX XXX XXX */
			ask = FALSE;

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", monster_powers[power].name);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw)
	{
		Term_load();
		character_icky = FALSE;
	}

	/* Abort if needed */
	if (!flag)
	{
		energy_use = 0;
		return num;
	}

	/* 'Powerful' monsters have wider radii */
	if (r_ptr->flags2 & RF2_POWERFUL)
	{
		rad = 1 + (p_ptr->lev/15);
	}
	else
	{
		rad = 1 + (p_ptr->lev/20);
	}


	/* Analyse power */
	switch (power)
	{
		/**** RF4 (bit position) ****/

		/* SHRIEK */
		case 0:
		{
			aggravate_monsters(-1);

			break;
		}

		/* MULTIPLY */
		case 1:
		{
			do_cmd_wiz_named_friendly(p_ptr->body_monster, FALSE);

			break;
		}

		/* S_ANIMAL */
		case 2:
		{
			summon_specific_friendly(y, x, rlev, SUMMON_ANIMAL, TRUE);

			break;
		}

		/* ROCKET */
		case 3:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_ROCKET, dir, p_ptr->lev * 12, 1 + (p_ptr->lev/20));

			break;
		}

		/* ARROW_1 */
		case 4:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_ARROW, dir, damroll(1,6));

			break;
		}

		/* ARROW_2 */
		case 5:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_ARROW, dir, damroll(3,6));

			break;
		}

		/* ARROW_3 */
		case 6:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_ARROW, dir, damroll(5,6));

			break;
		}

		/* ARROW_4 */
		case 7:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_ARROW, dir, damroll(7,6));

			break;
		}

		/* BR_ACID */
		case 8:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_ACID, dir, p_ptr->lev * 5, rad);

			break;
		}

		/* BR_ELEC */
		case 9:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_ELEC, dir, p_ptr->lev * 5, rad);

			break;
		}

		/* BR_FIRE */
		case 10:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_FIRE, dir, p_ptr->lev * 5, rad);

			break;
		}

		/* BR_COLD */
		case 11:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_COLD, dir, p_ptr->lev * 5, rad);

			break;
		}

		/* BR_POIS */
		case 12:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_POIS, dir, p_ptr->lev * 5, rad);

			break;
		}

		/* BR_NETH */
		case 13:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_NETHER, dir, p_ptr->lev * 5, rad);

			break;
		}

		/* BR_LITE */
		case 14:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_LITE, dir, p_ptr->lev * 8, rad);

			break;
		}

		/* BR_DARK */
		case 15:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_DARK, dir, p_ptr->lev * 8, rad);

			break;
		}

		/* BR_CONF */
		case 16:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_CONFUSION, dir, p_ptr->lev * 8, rad);

			break;
		}

		/* BR_SOUN */
		case 17:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_SOUND, dir, p_ptr->lev * 8, rad);

			break;
		}

		/* BR_CHAO */
		case 18:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_CHAOS, dir, p_ptr->lev * 7, rad);

			break;
		}

		/* BR_DISE */
		case 19:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_DISENCHANT, dir, p_ptr->lev * 7, rad);

			break;
		}

		/* BR_NEXU */
		case 20:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_NEXUS, dir, p_ptr->lev * 5, rad);

			break;
		}

		/* BR_TIME */
		case 21:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_TIME, dir, p_ptr->lev * 3, rad);

			break;
		}

		/* BR_INER */
		case 22:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_INERTIA, dir, p_ptr->lev * 4, rad);

			break;
		}

		/* BR_GRAV */
		case 23:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_GRAVITY, dir, p_ptr->lev * 4, rad);

			break;
		}

		/* BR_SHAR */
		case 24:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_SHARDS, dir, p_ptr->lev * 8, rad);

			break;
		}

		/* BR_PLAS */
		case 25:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_PLASMA, dir, p_ptr->lev * 3, rad);

			break;
		}

		/* BR_WALL */
		case 26:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_FORCE, dir, p_ptr->lev * 4, rad);

			break;
		}

		/* BR_MANA */
		case 27:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_MANA, dir, p_ptr->lev * 5, rad);

			break;
		}

		/* BA_NUKE */
		case 28:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_NUKE, dir, p_ptr->lev * 8, 1 + (p_ptr->lev/20));

			break;
		}

		/* BR_NUKE */
		case 29:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_NUKE, dir, p_ptr->lev * 8, 1 + (p_ptr->lev/20));

			break;
		}

		/* BA_CHAO */
		case 30:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_CHAOS, dir, p_ptr->lev * 4, 2);

			break;
		}

		/* BR_DISI */
		case 31:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_DISINTEGRATE, dir, p_ptr->lev * 5, 1 + (p_ptr->lev/20));

			break;
		}


		/**** RF5 (bit position + 32) ****/

		/* BA_ACID */
		case 32:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_ACID, dir, randint(p_ptr->lev * 6)+20, 2);

			break;
		}

		/* BA_ELEC */
		case 33:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_ELEC, dir, randint(p_ptr->lev * 3)+20, 2);

			break;
		}

		/* BA_FIRE */
		case 34:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_FIRE, dir, randint(p_ptr->lev * 7)+20, 2);

			break;
		}

		/* BA_COLD */
		case 35:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_COLD, dir, randint(p_ptr->lev * 3)+20, 2);

			break;
		}

		/* BA_POIS */
		case 36:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_POIS, dir, damroll(12,2), 2);

			break;
		}

		/* BA_NETH */
		case 37:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_NETHER, dir, randint(p_ptr->lev * 4)+20, 2);

			break;
		}

		/* BA_WATE */
		case 38:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_WATER, dir, randint(p_ptr->lev * 4)+20, 2);

			break;
		}

		/* BA_MANA */
		case 39:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_MANA, dir, randint(p_ptr->lev * 3)+20, 2);

			break;
		}

		/* BA_DARK */
		case 40:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_DARK, dir, randint(p_ptr->lev * 3)+20, 2);

			break;
		}

		/* 41 DRAIN_MANA -- Not available */

		/* 42 MIND_BLAST -- Not available */

		/* 43 BRAIN_SMASH -- Not available */

		/* CAUSE_1 */
		case 44:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_MANA, dir, damroll(3, 8));

			break;
		}

		/* CAUSE_2 */
		case 45:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_MANA, dir, damroll(8, 8));

			break;
		}

		/* CAUSE_3 */
		case 46:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_MANA, dir, damroll(10, 15));

			break;
		}

		/* CAUSE_4 */
		case 47:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_MANA, dir, damroll(15, 15));

			break;
		}

		/* BO_ACID */
		case 48:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_ACID, dir, damroll(7, 8) + (p_ptr->lev/3));

			break;
		}

		/* BO_ELEC */
		case 49:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_ELEC, dir, damroll(4, 8) + (p_ptr->lev/3));

			break;
		}

		/* BO_FIRE */
		case 50:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_FIRE, dir, damroll(9, 8) + (p_ptr->lev/3));

			break;
		}

		/* BO_COLD */
		case 51:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_COLD, dir, damroll(6, 8) + (p_ptr->lev/3));

			break;
		}

		/* BO_POIS */
		case 52:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_POIS, dir, damroll(7, 8) + (p_ptr->lev/3));

			break;
		}

		/* BO_NETH */
		case 53:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_NETHER, dir, damroll(5, 5) + (p_ptr->lev/3));

			break;
		}

		/* BO_WATE */
		case 54:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_WATER, dir, damroll(10, 10) + (p_ptr->lev/3));

			break;
		}

		/* BO_MANA */
		case 55:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_MANA, dir, damroll(3, 8) + (p_ptr->lev/3));

			break;
		}

		/* BO_PLAS */
		case 56:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_PLASMA, dir, damroll(8, 8) + (p_ptr->lev/3));

			break;
		}

		/* BO_ICEE */
		case 57:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_ICE, dir, damroll(6, 6) + (p_ptr->lev/3));

			break;
		}

		/* MISSILE */
		case 58:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_MISSILE, dir, damroll(2, 6) + (p_ptr->lev/3));

			break;
		}

		/* SCARE */
		case 59:
		{
			if (!get_aim_dir(&dir)) break;

			fear_monster(dir, plev);

			break;
		}

		/* BLIND */
		case 60:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_CONFUSION, dir, damroll(1, 8) + (p_ptr->lev/3));

			break;
		}

		/* CONF */
		case 61:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_CONFUSION, dir, damroll(7, 8) + (p_ptr->lev/3));

			break;
		}

		/* SLOW */
		case 62:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_OLD_SLOW, dir, damroll(6, 8) + (p_ptr->lev/3));

			break;
		}

		/* HOLD */
		case 63:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_OLD_SLEEP, dir, damroll(5, 8) + (p_ptr->lev/3));

			break;
		}


		/**** RF6 (bit position + 64) ****/

		/* HASTE */
		case 64:
		{
			if (!p_ptr->fast)
			{
				(void)set_fast(randint(20 + (plev) ) + plev, 10);
			}
			else
			{
				(void)set_fast(p_ptr->fast + randint(5), 10);
			}

			break;
		}

		/* HAND_DOOM */
		case 65:
		{
			if (!get_aim_dir(&dir)) break;

			fire_bolt(GF_MANA, dir, damroll(10, 8) + (p_ptr->lev));

			break;
		}

		/* HEAL */
		case 66:
		{
			hp_player(damroll(8,5));

			break;
		}

		/* S_ANIMALS */
		case 67:
		{
			for (k = 0; k < 4; k++)
			{
				summon_specific_friendly(y, x, rlev, SUMMON_ANIMAL, TRUE);
			}

			break;
		}

		/* BLINK */
		case 68:
		{
			if (dungeon_flags1 & LF1_NO_TELEPORT)
			{
				msg_print("No teleport on special levels...");
				break;
			}

			teleport_player(10);

			break;
		}

		/* TPORT */
		case 69:
		{
			if (dungeon_flags1 & LF1_NO_TELEPORT)
			{
				msg_print("No teleport on special levels...");
				break;
			}

			teleport_player(plev * 5);

			break;
		}

		/* TELE_TO */
		case 70:
		{
			int ii,ij;

			if (dungeon_flags1 & LF1_NO_TELEPORT)
			{
				msg_print("No teleport on special levels...");
				break;
			}

			msg_print("You go between.");

			if (!tgt_pt(&ii, &ij)) break;

			p_ptr->energy -= 60 - plev;

			if (!cave_empty_bold(ij, ii) ||
			    (cave[ij][ii].info & CAVE_ICKY) ||
			    (distance(ij, ii, py, px) > plev * 20 + 2))
			{
				msg_print("You fail to show the destination correctly!");
				p_ptr->energy -= 100;
				teleport_player(10);
			}
			else teleport_player_to(ij, ii);

			break;
		}

		/* TELE_AWAY */
		case 71:
		{
			if (dungeon_flags1 & LF1_NO_TELEPORT)
			{
				msg_print("No teleport on special levels...");
				break;
			}

			if (!get_aim_dir(&dir)) break;

			(void)fire_beam(GF_AWAY_ALL, dir, plev);

			break;
		}

		/* TELE_LEVEL */
		case 72:
		{
			if (dungeon_flags1 & LF1_NO_TELEPORT)
			{
				msg_print("No teleport on special levels...");
				break;
			}

			teleport_player_level();

			break;
		}

		/* DARKNESS */
		case 73:
		{
			(void)project(-1, 3, py, px, 0, GF_DARK_WEAK,
			              PROJECT_GRID | PROJECT_KILL);

			/* Unlite the room */
			unlite_room(py, px);

			break;
		}

		/* TRAPS */
		case 74:
		{
			trap_creation();

			break;
		}

		/* 75 FORGET -- Not available */

		/* ANIM_DEAD -- Use the same code as the nether spell */
		case 76:
		{
			if (!get_aim_dir(&dir)) break;

			fire_ball(GF_RAISE, dir, 1, 0);

			break;
		}

		/* 77 S_BUG -- Not available, well we do that anyway ;) */

		/* 78 S_RNG -- Not available, who dares? */

		/* S_DRAGONRIDER */
		case 79:
		{
			for (k = 0; k < 1; k++)
			{
				summon_specific_friendly(y, x, rlev, SUMMON_DRAGONRIDER, TRUE);
			}

			break;
		}

		/* S_KIN -- Summon Kin, because we code bugs :) */
		case 80:
		{
			/* Big hack */
			summon_kin_type = r_ptr->d_char;

			for (k = 0; k < 6; k++)
			{
				summon_specific_friendly(y, x, rlev, SUMMON_KIN, TRUE);
			}

			break;
		}

		/* S_HI_DEMON */
		case 81:
		{
			for (k = 0; k < 1; k++)
			{
				summon_specific_friendly(y, x, rlev, SUMMON_HI_DEMON, TRUE);
			}

			break;
		}

		/* S_MONSTER */
		case 82:
		{
			for (k = 0; k < 1; k++)
			{
				summon_specific_friendly(y, x, rlev, 0, TRUE);
			}

			break;
		}

		/* S_MONSTERS */
		case 83:
		{
			for (k = 0; k < 6; k++)
			{
				summon_specific_friendly(y, x, rlev, 0, TRUE);
			}

			break;
		}

		/* S_ANT */
		case 84:
		{
			for (k = 0; k < 6; k++)
			{
				summon_specific_friendly(y, x, rlev, SUMMON_ANT, TRUE);
			}

			break;
		}

		/* S_SPIDER */
		case 85:
		{
			for (k = 0; k < 6; k++)
			{
				summon_specific_friendly(y, x, rlev, SUMMON_SPIDER, TRUE);
			}

			break;
		}

		/* S_HOUND */
		case 86:
		{
			for (k = 0; k < 6; k++)
			{
				summon_specific_friendly(y, x, rlev, SUMMON_HOUND, TRUE);
			}

			break;
		}

		/* S_HYDRA */
		case 87:
		{
			for (k = 0; k < 6; k++)
			{
				summon_specific_friendly(y, x, rlev, SUMMON_HYDRA, TRUE);
			}

			break;
		}

		/* S_ANGEL */
		case 88:
		{
			for (k = 0; k < 1; k++)
			{
				summon_specific_friendly(y, x, rlev, SUMMON_ANGEL, TRUE);
			}

			break;
		}

		/* S_DEMON */
		case 89:
		{
			for (k = 0; k < 1; k++)
			{
				summon_specific_friendly(y, x, rlev, SUMMON_DEMON, TRUE);
			}

			break;
		}

		/* S_UNDEAD */
		case 90:
		{
			for (k = 0; k < 1; k++)
			{
				summon_specific_friendly(y, x, rlev, SUMMON_UNDEAD, TRUE);
			}

			break;
		}

		/* S_DRAGON */
		case 91:
		{
			for (k = 0; k < 1; k++)
			{
				summon_specific_friendly(y, x, rlev, SUMMON_DRAGON, TRUE);
			}

			break;
		}

		/* S_HI_UNDEAD */
		case 92:
		{
			for (k = 0; k < 8; k++)
			{
				summon_specific_friendly(y, x, rlev, SUMMON_HI_UNDEAD_NO_UNIQUES, TRUE);
			}

			break;
		}

		/* S_HI_DRAGON */
		case 93:
		{
			for (k = 0; k < 8; k++)
			{
				summon_specific_friendly(y, x, rlev, SUMMON_HI_DRAGON_NO_UNIQUES, TRUE);
			}

			break;
		}

		/* S_WRAITH */
		case 94:
		{
			for (k = 0; k < 8; k++)
			{
				summon_specific_friendly(y, x, rlev, SUMMON_WRAITH, TRUE);
			}

			break;
		}

		/* 95 S_UNIQUE -- Not available */
	}

	/* Take some SP */
	if (!no_cost)
	{
		int chance, pchance;

		chance = (monster_powers[power].mana + r_ptr->level);
		pchance = adj_str_wgt[cp_ptr->spell_stat]/2 + get_skill(SKILL_POSSESSION);

		if (rand_int(chance) >= pchance)
		{
			int m = monster_powers[power].mana / 10;

			if (!m) m = 1;

			if (m > p_ptr->msp) m = p_ptr->msp;

			p_ptr->csp -= m;
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);

	return (num);
}


/*
 * Cast a spell in the Symbiotic books
 */
void cast_symbiotic_spell(int spell, byte level)
{
	object_type *o_ptr;
	monster_race *r_ptr;
	int to_s = level + p_ptr->to_s;
	s32b dam;

	/* Get the carried monster */
	o_ptr = &inventory[INVEN_CARRY];

	/* Analyse spell */
	switch (spell)
	{
		/* Minor Symbiotic Healing */
		case 0:
		{
			int max;

			dam = apply_power_dice(2, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " heal monster %ldd6", dam);
				return;
			}

			if (!o_ptr->k_idx)
			{
				msg_print("You are not in symbiosis.");
				break;
			}

			r_ptr = &r_info[o_ptr->pval];
			max = maxroll(r_ptr->hdice, r_ptr->hside);
			o_ptr->pval2 += damroll(dam, 6);
			if (o_ptr->pval2 > max) o_ptr->pval2 = max;

			msg_print("Your monster is healed");

			/* Display the monster hitpoints */
			p_ptr->redraw |= (PR_MH);

			break;
		}

		/* Tangled Creepers */
		case 1:
		{
			if (info_spell) return;

			slow_monsters();

			break;
		}

		/* Vamiric healing */
		case 2:
		{
			int dummy,plev=p_ptr->lev,dir;
			int max;

			dam = apply_power_dur(plev, to_s, 2);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " heal monster %ld+d%ld", dam, dam);
				return;
			}

			r_ptr = &r_info[o_ptr->pval];
			max = maxroll(r_ptr->hdice, r_ptr->hside);

			if (!get_aim_dir(&dir)) return;

			/* Damage */
			dummy = dam + randint(dam);

			if (drain_life(dir, dummy))
			{
				o_ptr->pval2 += dummy;
				if(o_ptr->pval2 > max)o_ptr->pval2 = max;
				p_ptr->redraw |= (PR_MH);
			}

			break;
		}

		/* Life Transfer */
		case 3:
		{
			int max, hp;

			dam = apply_power_dice(6, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " hp->hp monster %ldd15", dam);
				return;
			}

			if (!o_ptr->k_idx)
			{
				msg_print("You are not in symbiosis.");
				break;
			}

			r_ptr = &r_info[o_ptr->pval];
			max = maxroll(r_ptr->hdice, r_ptr->hside);

			hp = damroll(dam, 15);

			/* Not enough hit points */
			if (p_ptr->chp <= hp)
			{
				msg_print("You can't do this in your weakened state.");
				break;
			}

			/* Heal monster */
			o_ptr->pval2 += hp;
			if (o_ptr->pval2 > max)o_ptr->pval2 = max;

			/* Sacrifice HP */
			p_ptr->chp -= hp;

			/* Message */
			msg_print("Your monster is healed");

			/* Redraw */
			p_ptr->redraw |= (PR_HP);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);

			/* Display the monster hitpoints */
			p_ptr->redraw |= (PR_MH);

			break;
		}

		/* Satisfy Hunger */
		case 4:
		{
			if (info_spell) return;

			(void)set_food(PY_FOOD_MAX - 1);

			break;
		}

		/* Minor Symbiotic Powers */
		case 5:
		{
			if (info_spell) return;

			if (!o_ptr->k_idx)
			{
				msg_print("You are not in symbiosis.");
				break;
			}

			use_symbiotic_power(o_ptr->pval, FALSE, FALSE, TRUE);

			break;
		}

		case 6: /* Summon Never-moving Pet */
		{
			if (info_spell) return;

			summon_specific_friendly(py, px, dun_level, SUMMON_MINE,FALSE);

			break;
		}


		/* No Symbiotic book in the second book slot */


		/* Mana Healing */
		case 16:
		{
			int max, hp;

			dam = apply_power_dice(6, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " sp->hp monster %ldd15", dam);
				return;
			}

			if (!o_ptr->k_idx)
			{
				msg_print("You are not in symbiosis.");
				break;
			}

			r_ptr = &r_info[o_ptr->pval];
			max = maxroll(r_ptr->hdice, r_ptr->hside);

			hp = damroll(dam, 15);

			/* Not enough mana */
			if (p_ptr->csp <= hp)
			{
				msg_print("You can't do this in your weaken state.");
				break;
			}

			/* Restore mana of the monster */
			o_ptr->pval2 += hp;
			if (o_ptr->pval2 > max) o_ptr->pval2 = max;

			/* Sacrifice mana */
			p_ptr->csp -= hp;

			/* Message */
			msg_print("Your monster is healed");

			/* Redraw */
			p_ptr->redraw |= (PR_MANA);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);

			/* Display the monster hitpoints */
			p_ptr->redraw |= (PR_MH);

			break;
		}

		/* Summon Never-moving Pets */
		case 17:
		{
			int k;

			if (info_spell) return;

			for (k = 0; k < 6; k++)
			{
				summon_specific_friendly(py, px, dun_level, SUMMON_MINE,FALSE);
			}

			break;
		}

		/* Major Symbiotic Healing */
		case 18:
		{
			int max;

			dam = apply_power_dice(3, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " heal monster %ldd60", dam);
				return;
			}

			if (!o_ptr->k_idx)
			{
				msg_print("You are not in symbiosis.");
				break;
			}

			r_ptr = &r_info[o_ptr->pval];
			max = maxroll(r_ptr->hdice, r_ptr->hside);
			o_ptr->pval2 += damroll(dam, 60);
			if (o_ptr->pval2 > max) o_ptr->pval2 = max;

			msg_print("Your monster is healed");

			/* Display the monster hitpoints */
			p_ptr->redraw |= (PR_MH);

			break;
		}

		/* Healing */
		case 19:
		{
			dam = apply_power_dice(2, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " heal %ldd40", dam);
				return;
			}

			hp_player(damroll(dam, 40));

			break;
		}

		/* Major Symbiotic Powers */
		case 20:
		{
			if (info_spell) return;

			if (!o_ptr->k_idx)
			{
				msg_print("You are not in symbiosis.");
				break;
			}

			use_symbiotic_power(o_ptr->pval, TRUE, FALSE, TRUE);

			break;
		}

		/* Use Ennemy's Powers */
		case 21:
		{
			int y,x;
			cave_type *c_ptr;
			monster_type *m_ptr;

			if (info_spell) return;

			if (!tgt_pt(&x, &y)) return;

			c_ptr = &cave[y][x];

			if (!c_ptr->m_idx) break;

			m_ptr = &m_list[c_ptr->m_idx];
			use_symbiotic_power(m_ptr->r_idx, TRUE, FALSE, TRUE);

			break;
		}

		default:
		{
			msg_format("You cast an unknown Symbiotic spell: %d.", spell);
			msg_print(NULL);

			break;
		}
	}
}


/*
 * Sing a song in the Harper books
 */
void cast_music_spell(int spell, byte level)
{
	int dir, dummy;

	int plev = p_ptr->lev;

	int to_s2 = (level + p_ptr->to_s) / 2;


	/* Ensure minimal value */
	if (to_s2 == 0) to_s2 = 1;

	if (p_ptr->music_extra)
	{
		msg_print("You stop singing.");
		p_ptr->music_extra = MUSIC_NONE;
	}

	p_ptr->music_extra2 = spell;


	/* Analyse song */
	switch (spell)
	{
		/* Song of slowness */
		case 0:
		{
			if (info_spell) return;

			msg_print("You start humming a slow, steady melody...");
			p_ptr->music_extra = MUSIC_SLOW;

			break;
		}

		/* Stop singing */
		case 1:
		{
			if (info_spell) return;

			p_ptr->music_extra = MUSIC_NONE;

			break;
		}

		/* The note which kill */
		case 2:
		{
			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %dd4", 4 + ((plev - 1) / 5));
				return;
			}

			msg_print("You cry out in an ear-wracking voice...");

			if (!get_aim_dir(&dir)) return;

			fire_beam(GF_SOUND, dir,
			          damroll(4 + ((plev - 1) / 5), 4));

			break;
		}

		/* Stunning song */
		case 3:
		{
			if (info_spell) return;

			msg_print("You weave a pattern of sounds to bewilder and daze...");
			p_ptr->music_extra = MUSIC_STUN;

			break;
		}

		/* Song of life */
		case 4:
		{
			if (info_spell) return;
			msg_print("Life flows through you as you sing a song of healing...");
			p_ptr->music_extra = MUSIC_LIFE;

			break;
		}

		/* Song of mind */
		case 5:
		{
			if (info_spell) return;

			msg_print("Your quiet music sharpens your sense of hearing...");
			p_ptr->music_extra = MUSIC_MIND;

			break;
		}

		/* Song of lite */
		case 6:
		{
			if (info_spell) return;
			msg_print("Your uplifting song brings brightness to dark places...");
			p_ptr->music_extra = MUSIC_LITE;

			break;
		}

		/* Song of fury */
		case 7:
		{
			if (info_spell) return;

			msg_print("Your sing of the heroic Elder Days...");
			p_ptr->music_extra = MUSIC_FURY;

			break;
		}

		/* Awareness song */
		case 8:
		{
			if (info_spell) return;

			msg_print("As you start singing, "
			          "you become more aware of the world around you.");
			p_ptr->music_extra = MUSIC_AWARE;

			break;
		}

		/* Song of knowledge */
		case 9:
		{
			if (info_spell)
			{
				strnfmt(spell_txt, 50, " range 1");
				return;
			}

			msg_print("You recall the rich lore of the world...");
			p_ptr->music_extra = MUSIC_ID;

			break;
		}

		/* Between note */
		case 10:
		{
			if (info_spell)
			{
				strnfmt(spell_txt, 50, " range 100");
				return;
			}

			msg_print("Your voice twists through space...");
			teleport_player(100);

			break;
		}

		/* The song which kill */
		case 11:
		{
			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %dd6", 10 + ((plev - 1) / 5));
				return;
			}

			msg_print("You call out with a terrible curse...");

			if (!get_aim_dir(&dir)) return;

			fire_beam(GF_SOUND, dir,
			          damroll(10 + ((plev - 1) / 5), 6));

			break;
		}

		/* song of illusion */
		case 12:
		{
			if (info_spell) return;

			msg_print("You weave a pattern of sounds to beguile and confuse...");
			p_ptr->music_extra = MUSIC_ILLUSION;

			break;
		}

		/* wall breaking song */
		case 13:
		{
			if (info_spell)
			{
				strnfmt(spell_txt, 50, " rad 1");
				return;
			}

			msg_print("You weave a pattern of sounds to contort and shatter...");
			p_ptr->music_extra = MUSIC_WALL;

			break;
		}

		/* song of resistance */
		case 14:
		{
			if (info_spell) return;

			msg_print("You sing a song of perserverance against powers...");
			p_ptr->music_extra = MUSIC_RESIST;

			break;
		}

		/* song of time */
		case 15:
		{
			if (info_spell) return;

			msg_print("You start singing swift and light folk-tunes...");
			p_ptr->music_extra = MUSIC_TIME;

			break;
		}

		/* Education song */
		case 16:
		{
			monster_type *m_ptr;
			monster_race *r_ptr;
			int m;

			if (info_spell) return;

			msg_print("You recall legends of the fair "
			          "and foul creatures of the world...");

			if (!get_rep_dir(&dir)) break;

			if (!cave[py + ddy[dir]][px + ddx[dir]].m_idx) break;

			m_ptr = &m_list[cave[py + ddy[dir]][px + ddx[dir]].m_idx];
			r_ptr = race_inf(m_ptr);

			msg_print("Now you have a better knowledge of this monster.");

			r_ptr->r_wake = r_ptr->r_ignore = MAX_UCHAR;

			/* Observe "maximal" attacks */
			for (m = 0; m < 4; m++)
			{
				/* Examine "actual" blows */
				if (r_ptr->blow[m].effect || r_ptr->blow[m].method)
				{
					/* Hack -- maximal observations */
					r_ptr->r_blows[m] = MAX_UCHAR;
				}
			}

			/* Hack -- maximal drops */
			r_ptr->r_drop_gold = r_ptr->r_drop_item =
				(((r_ptr->flags1 & (RF1_DROP_4D2)) ? 8 : 0) +
				 ((r_ptr->flags1 & (RF1_DROP_3D2)) ? 6 : 0) +
				 ((r_ptr->flags1 & (RF1_DROP_2D2)) ? 4 : 0) +
				 ((r_ptr->flags1 & (RF1_DROP_1D2)) ? 2 : 0) +
				 ((r_ptr->flags1 & (RF1_DROP_90))  ? 1 : 0) +
				 ((r_ptr->flags1 & (RF1_DROP_60))  ? 1 : 0));

			/* Hack -- but only "valid" drops */
			if (r_ptr->flags1 & (RF1_ONLY_GOLD)) r_ptr->r_drop_item = 0;
			if (r_ptr->flags1 & (RF1_ONLY_ITEM)) r_ptr->r_drop_gold = 0;

			/* Hack -- observe many spells */
			r_ptr->r_cast_inate = MAX_UCHAR;
			r_ptr->r_cast_spell = MAX_UCHAR;

			/* Hack -- know all the flags */
			r_ptr->r_flags1 = r_ptr->flags1;
			r_ptr->r_flags2 = r_ptr->flags2;
			r_ptr->r_flags3 = r_ptr->flags3;
			r_ptr->r_flags4 = r_ptr->flags4;
			r_ptr->r_flags5 = r_ptr->flags5;
			r_ptr->r_flags6 = r_ptr->flags6;

			break;
		}

		/* World Contortion */
		case 17:
		{
			if (info_spell) return;

			msg_print("Reality whirls wildly as you sing a dizzying melody...");
			fire_explosion(py, px, GF_AWAY_ALL,
			               p_ptr->lev/15 + 1, 1 + (p_ptr->lev * 3));

			break;
		}

		/* charming note */
		case 18:
		{
			if (info_spell)
			{
				strnfmt(spell_txt, 50, " power %dd5",(10+((plev-1)/5)));
				return;
			}

			msg_print("You chant a song of friendship...");

			if (!get_aim_dir(&dir)) return;

			fire_beam(GF_CHARM, dir,
			          damroll(10 + ((plev - 1) / 5), 5));

			break;
		}

		/* song of death */
		case 19:
		{
			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %dd10", 20 + ((plev - 1) / 5));
				return;
			}

			msg_print("You unleash a furious barrage of sounds...");

			if (!get_aim_dir(&dir)) return;

			fire_beam(GF_SOUND, dir,
			          damroll(20 + ((plev - 1) / 5), 10));

			break;
		}

		/* vibration note */
		case 20:
		{
			if (info_spell) return;

			msg_print("You sing an old song of the dwarven-smiths...");
			brand_weapon(5); /* brand earthquake */

			break;
		}

		/* vibration song */
		case 21:
		{
			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %dd6/turn",10+plev/15);
				return;
			}

			msg_print("The fury of the Downfall of Numenor lashes out...");
			p_ptr->music_extra = MUSIC_VIBRA;

			break;
		}

		/* song of disruption */
		case 22:
		{
			if (info_spell) return;

			msg_print("You sing of the primeval shaping of Middle-earth...");
			alter_reality();

			break;
		}

		/* Lay of Gil-Galad */
		case 23:
		{
			if (info_spell) return;

			msg_print("You chant a powerful, heroic call to arms...");
			for (dummy = 0; dummy < 3 + (plev / 10); dummy++)
			{
				if (randint(10) > 3)
				{
					summon_specific_friendly(py, px, plev*to_s2,
					                         SUMMON_NO_UNIQUES, FALSE);
				}
				else
				{
					if (summon_specific(py, px, plev, 0))
					{
						msg_print("A summoned creature gets angry!");
					}
				}
			}

			break;
		}

		/* hidding song */
		case 24:
		{
			if (info_spell)
			{
				strnfmt(spell_txt, 50, " power 40");
				return;
			}

			msg_print("Your song carries you beyond the sight of mortal eyes...");
			p_ptr->music_extra = MUSIC_HIDE;

			break;
		}

		/* shriek of death */
		case 25:
		{
			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %dd20", 100 + ((plev - 1) / 5));
				return;
			}

			msg_print("A chanting dark and fell rises, "
			          "bearing death to the world...");

			if (!get_aim_dir(&dir)) return;

			fire_bolt(GF_SOUND, dir,
			          damroll(100 + ((plev - 1) / 5), 20));

			break;
		}

		/* charming song */
		case 26:
		{
			if (info_spell)
			{
				strnfmt(spell_txt, 50, " rad 1");
				return;
			}

			msg_print("You weave a slow, soothing melody of imploration...");
			p_ptr->music_extra = MUSIC_CHARME;

			break;
		}

		/* between shriek */
		case 27:
		{
			if (info_spell) return;

			msg_print("The holy power of the Music of the Ainur enters you...");
			p_ptr->music_extra = MUSIC_HOLY;

			break;
		}

		/* destruction shriek */
		case 28:
		{
			if (info_spell)
			{
				strnfmt(spell_txt, 50, " rad 10");
				return;
			}

			msg_print("The unmeasurable destructive power of the Oceans "
			          "crashes around you...");
			destroy_area(py, px, 10, TRUE, FALSE);

			break;
		}

		/* song of liberty */
		case 29:
		{
			if (info_spell) return;

			msg_print("You recall the valor of Fingolfin's challenge "
			          "to the Dark Lord...");
			p_ptr->music_extra = MUSIC_LIBERTY;

			break;
		}

		/* song of the Undeads */
		case 30:
		{
			if (info_spell)
			{
				strnfmt(spell_txt, 50, " rad 1");
				return;
			}

			msg_print("The themes of life and revival are "
					  "woven into your song...");
			p_ptr->music_extra = MUSIC_RAISE;

			break;
		}

		/* Immaterial song */
		case 31:
		{
			if (info_spell) return;

			msg_print("You chant a deep and moving hymn of "
			          "the shadowy Halls of Mandos...");
			p_ptr->music_extra = MUSIC_SHADOW;

			break;
		}

		default:
		{
			msg_format("You sing an unknown song: %d.", spell);
			msg_print(NULL);

			break;
		}
	}
}


/*
 * Cast a spell in the Druidic books
 *
 * Druids were, IIRC, Celtic *priests*, but the list looks more like
 * spells than prayers...
 */
void cast_druid_spell(int spell, byte level)
{
	int dir, beam;
	int plev = p_ptr->lev;
	int to_s = level + p_ptr->to_s;
	s32b dam, rad;

	int amt = 0, att = 0;


	if (get_skill(SKILL_SPELL)) beam = plev + get_skill_scale(SKILL_SPELL, 15);
	else beam = plev / 2;


	switch (spell)
	{
		/* Tunnel */
		case 0:
		{
			magic_type *s_ptr = &realm_info[REALM_DRUID][0];
			int d, i, min, ox, oy, x=0, y=0;
			int tries = 0, dis;
			int xx = -1, yy = -1;
			bool look = TRUE;

			dam = apply_power_dice(10, to_s, 2);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " range %ld", dam);
				return;
			}

			dis = dam;

			if (p_ptr->druid_extra3)
			{
				msg_print("This level has already been drained!");
				break;
			}

			amt = get_quantity("What is the minimal amount of mana "
			                   "the tunnel end must have?", 255);

			att = get_check("Does the mana have to match exactly "
			                "the specified amount?");

			if (p_ptr->resist_continuum)
			{
				msg_print("The space-time continuum can't be disrupted.");
				return;
			}

			if (p_ptr->anti_tele)
			{
				msg_print("A mysterious force prevents you from teleporting!");
				return;
			}

			/* To be on the safe side... */
			if (dis > 200) dis = 200;

			/* Minimum distance */
			min = dis / 2;

			/* Look until done */
			while (look)
			{
				tries++;

				/* Verify max distance */
				if (dis > 200) dis = 200;

				/* Try several locations */
				for (i = 0; i < 500; i++)
				{
					/* Pick a (possibly illegal) location */
					while (1)
					{
						y = rand_spread(py, dis);
						x = rand_spread(px, dis);
						d = distance(py, px, y, x);
						if ((d >= min) && (d <= dis)) break;
					}

					/* Ignore illegal locations */
					if (!in_bounds(y, x)) continue;

					/* Require "naked" floor space */
					if (!cave_naked_bold(y, x)) continue;

					/* Require a certain amount of mana */
					if (cave[y][x].mana < amt) continue;
					if ((att == TRUE) && (cave[y][x].mana > amt)) continue;

					/* No teleporting into vaults and such */
					if (cave[y][x].info & (CAVE_ICKY)) continue;

					/* This grid looks good */
					look = FALSE;

					/* Stop looking */
					break;
				}

				/* Increase the maximum distance */
				dis = dis * 2;

				/* Decrease the minimum distance */
				min = min / 2;

				/* Stop after MAX_TRIES tries */
				if (tries > MAX_TRIES) return;
			}

			/* Sound */
			sound(SOUND_TELEPORT);

			/* Save the old location */
			oy = py;
			ox = px;

			/* Move the player */
			py = y;
			px = x;

			/* Absorb some mana */
			if (s_ptr->smana <= amt)
			{
				cave[y][x].mana -= s_ptr->smana;
				p_ptr->csp += s_ptr->smana;
			}
			else
			{
				cave[y][x].mana -= amt;
				p_ptr->csp += amt;
			}

			/* Redraw the old spot */
			lite_spot(oy, ox);

			while (xx < 2)
			{
				yy = -1;

				while (yy < 2)
				{
					if (xx == 0 && yy == 0)
					{
						/* Do nothing */
					}
					else
					{
						if (cave[oy+yy][ox+xx].m_idx)
						{
							monster_race *r_ptr = race_inf(&m_list[cave[oy+yy][ox+xx].m_idx]);

							/*
							 * The latter limitation is to avoid
							 * totally unkillable suckers...
							 */
							if ((r_ptr->flags6 & RF6_TPORT) &&
							    !(r_ptr->flags3 & RF3_RES_TELE))
							{
								if (!(m_list[cave[oy+yy][ox+xx].m_idx].csleep))
								{
									teleport_to_player(cave[oy+yy][ox+xx].m_idx);
								}
							}
						}
					}
					yy++;
				}
				xx++;
			}

			/* Redraw the new spot */
			lite_spot(py, px);

			/* Check for new panel (redraw map) */
			verify_panel();

			/* Update stuff */
			p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MON_LITE);

			/* Update the monsters */
			p_ptr->update |= (PU_DISTANCE);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD);

			/* Handle stuff XXX XXX XXX */
			handle_stuff();

			break;
		}

		/* Canalize Mana */
		case 1:
		{
			if (info_spell) return;

			if (p_ptr->druid_extra2 == CLASS_CANALIZE_MANA)
			{
				p_ptr->druid_extra2 = CLASS_NONE;
				msg_print("You stop canalizing the earth's mana.");
			}
			else
			{
				msg_print("You begin canalizing in yourself the earth's mana.");
				p_ptr->druid_extra2 = CLASS_CANALIZE_MANA;
			}

			break;
		}

		/* Acid Bolt */
		case 2:
		{
			dam = apply_power_dice(4, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %dd%ld", 3+((plev-5)/4), dam);
				return;
			}

			if (!get_aim_dir(&dir)) return;

			fire_bolt(GF_ACID, dir,
			          damroll(3+((plev-5)/4), dam));

			break;
		}

		/* Mana Path */
		case 3:
		{
			if (info_spell) return;

			if (p_ptr->druid_extra3)
			{
				msg_print("This level has already been drained!");
				break;
			}

			if (p_ptr->druid_extra2 == CLASS_MANA_PATH)
			{
				p_ptr->druid_extra2 = CLASS_NONE;
				msg_print("You stop laying a mana path.");

				p_ptr->update |= PU_BONUS;
			}
			else
			{
				msg_print("You begin laying a mana path.");

				/* Ask for the amount of mana to lay down */
				amt = get_quantity("What amount of mana do you want "
				                   "to lay on each step?", 255);
				if (!amt) break;

				/* Ask for laying type */
				if (get_check("Do you want to absorb the old mana before laying?"))
				{
					att |= (CLASS_MANA_PATH_ERASE);
				}

				p_ptr->druid_extra2 = CLASS_MANA_PATH;
				p_ptr->druid_extra = amt + (att << 8) ;

				p_ptr->update |= (PU_BONUS);
			}

			break;
		}

		/* Forest Generation */
		case 4:
		{
			int i,j;
			byte rad = 2 + (plev / 25) + to_s;

			if (info_spell) return;

			for (j = py - rad; j < py + rad; j++)
			{
				for (i = px - rad; i < px + rad; i++)
				{
					if ((distance(py, px, j, i) <= rad) && in_bounds(j,i))
					{
						if ((cave[j][i].mana >= 20) &&
						    cave_clean_bold(j, i) &&
						    cave_empty_bold(j, i))
						{
							cave[j][i].mana -= 20;
							cave_set_feat(j, i, FEAT_TREES);
						}
					}
				}
			}

			break;
		}

		/* Druidistic Acid Beam */
		case 5:
		{
			dam = apply_power_dice(4, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %dd%ld", 5+((plev-5)/4), dam);
				return;
			}

			if (!get_aim_dir(&dir)) return;

			fire_druid_beam(GF_ACID, dir,
			                damroll(5+((plev-5)/4), dam));

			break;
		}

		/* Raise Mountains */
		case 6:
		{
			int i, j;
			byte rad = 2 + (plev / 25) + to_s;

			if (info_spell) return;

			for (j = py - rad; j < py + rad; j++)
			{
				for (i = px - rad; i < px + rad; i++)
				{
					if ((distance(py, px, j, i) <= rad) && in_bounds(j, i))
					{
						if ((cave[j][i].mana >= 20) &&
						    cave_clean_bold(j, i) &&
						    cave_empty_bold(j, i))
						{
							cave[j][i].mana -= 20;
							cave_set_feat(j, i, FEAT_MOUNTAIN);
						}
					}
				}
			}

			break;
		}

		/* Stone Skin */
		case 7:
		{
			dam = apply_power_dur(10, to_s, 3);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dur %ld+d10", dam);
				return;
			}

			set_shield(p_ptr->shield + dam + randint(10), 30 + (to_s * 2), 0, 0, 0);

			break;
		}

		/* Infra */
		case 8:
		{
			dam = apply_power_dur(10, to_s, 2);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dur %ld+d15", dam);
				return;
			}

			set_tim_infra(p_ptr->tim_infra + dam + randint(15));

			break;
		}

		/* Fire Bolt */
		case 9:
		{
			dam = apply_power_dice(7, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %dd%ld", 6+((plev-5)/4), dam);
				return;
			}

			if (!get_aim_dir(&dir)) return;

			fire_bolt(GF_FIRE, dir,
			          damroll(6 + ((plev-5)/4), dam));

			break;
		}

		/* Fire Ball */
		case 10:
		{
			rad = apply_power_dice(2, to_s, 1);
			dam = apply_power_dam(40, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %ld", dam);
				return;
			}

			if (!get_aim_dir(&dir)) return;

			fire_ball(GF_FIRE, dir,
			          dam, rad);

			break;
		}

		/* Enlight Traps */
		case 11:
		{
			if (info_spell) return;

			detect_traps(DEFAULT_RADIUS);

			break;
		}

		/* Fire Beam */
		case 12:
		{
			dam = apply_power_dice(8, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %dd%ld", 7+((plev-5)/4), dam);
				return;
			}

			if (!get_aim_dir(&dir)) return;

			fire_beam(GF_FIRE, dir,
			          damroll(7 + ((plev-5)/4), dam));

			break;
		}

		/* Druidistic Fire Bolt */
		case 13:
		{
			dam = apply_power_dice(8, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %dd%ld", 7+((plev-5)/4), dam);
				return;
			}

			if (p_ptr->druid_extra3)
			{
				msg_print("This level has already been drained!");
				break;
			}

			if (!get_aim_dir(&dir)) return;

			fire_druid_bolt(GF_FIRE, dir,
			                damroll(7 + ((plev-5)/4), dam));

			break;
		}

		/* Druidistic Fire Beam */
		case 14:
		{
			dam = apply_power_dice(8, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %dd%ld", 7+((plev-5)/4), dam);
				return;
			}

			if (p_ptr->druid_extra3)
			{
				msg_print("This level has already been drained!");
				break;
			}

			if (!get_aim_dir(&dir)) return;

			fire_druid_beam(GF_ACID, dir,
			                damroll(7 + ((plev-5)/4), dam));

			break;
		}

		/* Create Lava */
		case 15:
		{
			int i, j;
			byte rad = 2 + (plev / 25) + to_s;

			if (info_spell) return;

			for (j = py - rad; j < py + rad; j++)
			{
				for (i = px - rad; i < px + rad; i++)
				{
					if ((distance(py, px, j, i) <= rad) && in_bounds(j, i))
					{
						if ((cave[j][i].mana >= 20) &&
						    cave_clean_bold(j, i) &&
						    cave_empty_bold(j, i))
						{
							cave[j][i].mana -= 20;
							cave_set_feat(j, i, FEAT_DEEP_LAVA);
						}
					}
				}
			}

			break;
		}

		/* Winds of Mana */
		case 16:
		{
			if (info_spell) return;

			if (p_ptr->druid_extra3)
			{
				msg_print("This level has already been drained!");
				break;
			}

			if (p_ptr->druid_extra3 == CLASS_WINDS_MANA)
			{
				p_ptr->druid_extra3 = CLASS_NONE;
				msg_print("You stop expulsing mana winds.");
			}
			else
			{
				msg_print("You begin expulsing mana winds.");

				/* Ask for the amount of mana to lay down */
				amt = get_quantity("What amount of mana do you want "
				                   "to lay on each step?", 255);
				if (!amt) break;

				/* Ask for laying type */
				if (get_check("Do you want to absorb the old mana before laying?"))
				{
					att |= (CLASS_MANA_PATH_ERASE);
				}

				p_ptr->druid_extra2 = CLASS_WINDS_MANA;
				p_ptr->druid_extra = amt + (att << 8) ;
			}

			break;
		}

		/* Summon Air Elem */
		case 17:
		{
			int xx = px,yy = py;

			if (info_spell) return;

			msg_format("You magically summon an Air Elemental.");
			scatter(&yy, &xx, py, px, 6, 0);
			place_monster_aux(yy, xx, test_monster_name("Air elemental"),
			                  FALSE, FALSE, TRUE);

			break;
		}

		/* Wispers from Afar */
		case 18:
		{
			if (info_spell) return;

			ident_spell();

			break;
		}

		/* The Winds of Manwe */
		case 19:
		{
			rad = apply_power_dice(4, to_s, 1);
			dam = apply_power_dur(10, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %ld", dam);
				return;
			}

			fire_ball(GF_GRAVITY, 0, dam, rad);

			break;
		}

		/* Bird View */
		case 20:
		{
			if (info_spell) return;

			wiz_lite_extra();

			break;
		}

		/* *Wispers from Afar* */
		case 21:
		{
			if (info_spell) return;

			identify_fully();

			break;
		}

		/* Windy Speed */
		case 22:
		{
			dam = apply_power_dam(5, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dur %ld+d5", dam);
				return;
			}

			set_fast(p_ptr->fast + dam + randint(5), 10);

			break;
		}

		/* The Thunders of Manwe */
		case 23:
		{
			dam = apply_power_dam(8, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %dd%ld", 10+((plev-5)/4), dam);
				return;
			}

			if (get_check("Do you want it to be a druidistic beam? "))
			{
				if (p_ptr->druid_extra3)
				{
					msg_print("This level has already been drained!");
					break;
				}

				if (!get_aim_dir(&dir)) return;

				fire_druid_beam(GF_SOUND, dir,
				                damroll(10 + ((plev-5)/4), dam));
			}
			else
			{
				if (!get_aim_dir(&dir)) return;

				fire_beam(GF_ELEC, dir,
				          damroll(10 + ((plev-5)/4), dam));
			}

			break;
		}

		/* Summon Aqua Golems */
		case 24:
		{
			int xx = px, yy = py;

			if (info_spell) return;

			msg_format("You magically summon Aquatic Golems.");
			scatter(&yy, &xx, py, px, 6, 0);
			place_monster_aux(yy, xx, test_monster_name("Aquatic golem"),
			                  FALSE, TRUE, TRUE);

			break;
		}

		/* Walk over the Water */
		case 25:
		{
			dam = apply_power_dur(50, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dur %ld+d40", dam);
				return;
			}

			set_walk_water(p_ptr->walk_water + dam + randint(40));

			break;
		}

		/* Flood */
		case 26:
		{
			int i, j;
			byte rad = 3 + (plev / 25) + to_s;

			if (info_spell) return;

			for (j = py - rad - 1; j < py + rad + 1; j++)
			{
				for (i = px - rad - 1; i < px + rad + 1; i++)
				{
					s32b dis;
					int feat;

					/* Distance from the player */
					dis = distance(py, px, j, i);

					if ((dis <= rad + 1) && in_bounds(j, i))
					{
						if ((cave[j][i].mana >= 20) &&
						    cave_clean_bold(j, i) &&
						    cave_empty_bold(j, i))
						{
							/* Choose depth of water */
							if (dis <= rad) feat = FEAT_DEEP_WATER;
							else feat = FEAT_SHAL_WATER;

							cave[j][i].mana -= 20;
							cave_set_feat(j, i, feat);
						}
					}
				}
			}

			break;
		}

		/* Summon Water Elems */
		case 27:
		{
			int xx = px, yy = py;

			if (info_spell) return;

			msg_format("You magically summon Water Elementals.");
			scatter(&yy, &xx, py, px, 6, 0);
			place_monster_aux(yy, xx, test_monster_name("Water elemental"),
			                  FALSE, TRUE, TRUE);

			break;
		}

		/* Purification */
		case 28:
		{
			dam = apply_power_dur(300, to_s, 5);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " heal %ld", dam);
				return;
			}

			hp_player(dam);
			set_blind(0);
			set_confused(0);
			set_poisoned(0);
			set_stun(0);
			set_cut(0);

			break;
		}

		/* Go Underwater */
		case 29:
		{
			int ij, ii;

			if (info_spell) return;

			if ((cave[py][px].feat != FEAT_DEEP_WATER) &&
			    (cave[py][px].feat != FEAT_SHAL_WATER))
			{
				msg_print("You must be on a water square.");
				break;
			}

			msg_print("You go underwater, choose a destination.");

			if (!tgt_pt(&ii, &ij)) return;

			p_ptr->energy -= 60 - plev;

			/* XXX XXX XXX */
			if (!cave_empty_bold(ij,ii) ||
			    (cave[ij][ii].info & CAVE_ICKY) ||
		        (distance(ij,ii,py,px) > plev*10 + 2) ||
			    !(cave[ij][ii].info & CAVE_MARK) ||
			    ((cave[ij][ii].feat != FEAT_DEEP_WATER) &&
			     (cave[ij][ii].feat != FEAT_SHAL_WATER)))
			{
				msg_print("You fail to dive correctly!");
				p_ptr->energy -= 100;
				teleport_player(10);
			}
			else teleport_player_to(ij, ii);

			break;
		}

		/* Tidal Wave */
		case 30:
		{
			rad = apply_power_dice(4, to_s, 1);
			dam = apply_power_dam(400, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %ld", dam);
				return;
			}

			if (!get_aim_dir(&dir)) return;

			(void)fire_ball(GF_WATER, dir, dam, rad);

			break;
		}

		/* Flood Level */
		case 31:
		{
			if (info_spell) return;

			msg_print("The world is being flooded!");
			p_ptr->druid_extra |= (CLASS_FLOOD_LEVEL);
			alter_reality();

			break;
		}

		/* Glyph of Warding */
		case 32:
		{
			if (info_spell) return;

			warding_glyph();

			break;
		}

		/* Orb of Mana */
		case 33:
		{
			rad = apply_power_dice(2, to_s, 1);
			dam = apply_power_dice(10, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam 45d%ld+%d", dam, plev * 2);
				return;
			}

			if (!get_aim_dir(&dir)) return;

			(void)fire_ball(GF_MANA, dir, damroll(45, dam) + (plev * 2), rad);

			break;
		}

		/* Gather mana */
		case 34:
		{
			if (info_spell) return;

			if (p_ptr->druid_extra3)
			{
				msg_print("This level has already been drained!");
			}
			else
			{
				msg_print("This level hasn't been drained yet.");
			}

			break;
		}

		/* Mirror of Mana */
		case 35:
		{
			dam = apply_power_dice(10, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dur %ld+d10", dam);
				return;
			}

			set_tim_reflect(p_ptr->tim_reflect + dam + randint(10));

			break;
		}

		/* Activate Rune of Mana */
		case 36:
		{
			if (info_spell) return;

			if (p_ptr->druid_extra3)
			{
				msg_print("This level has already been drained!");
				break;
			}

			if(p_ptr->druid_extra2 == CLASS_CANALIZE_MANA)
			{
				p_ptr->druid_extra2 = CLASS_NONE;
				msg_print("You stop canalizing the earth's mana.");
			}
			else
			{
				msg_print("You begin canalizing in yourself the earth's mana.");
				p_ptr->druid_extra |= (CLASS_CANALIZE_MANA_EXTRA);
				p_ptr->druid_extra2 = CLASS_CANALIZE_MANA;
			}

			break;
		}

		/* Combine the 5 Elements */
		case 37:
		{
			dam = apply_power_dice(8, to_s, 1);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam 5*%dd%ld", 10+((plev-5)/4), dam);
				return;
			}

			if (!get_aim_dir(&dir)) return;

			fire_bolt_or_beam(beam, GF_FIRE, dir,
			                  damroll(10+((plev-5)/4), dam));
			fire_bolt_or_beam(beam, GF_COLD, dir,
			                  damroll(10+((plev-5)/4), dam));
			fire_bolt_or_beam(beam, GF_ACID, dir,
			                  damroll(10+((plev-5)/4), dam));
			fire_bolt_or_beam(beam, GF_ELEC, dir,
			                  damroll(10+((plev-5)/4), dam));
			fire_bolt_or_beam(beam, GF_MANA, dir,
			                  damroll(10+((plev-5)/4), dam));

			break;
		}

		/* Shield of Mana */
		case 38:
		{
			dam = apply_power_dur(20, to_s, 3);

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dur %ld+d10", dam);
				return;
			}

			set_shield(p_ptr->shield + dam + randint(10), 100 + (to_s * 2), 0, 0, 0);

			break;
		}

		/* Drain Level's Mana */
		case 39:
		{
			int i, j;
			long amt = 0;

			for (i = 0; i < cur_wid; i++)
			{
				for (j = 0; j < cur_hgt; j++)
				{
					amt += cave[j][i].mana / 60;
				}
			}

			dam = apply_power_dam(amt, to_s, 1);
			if (dam > 5000) dam = 5000;

			if (info_spell)
			{
				strnfmt(spell_txt, 50, " dam %ld", dam);
				return;
			}

			if (p_ptr->druid_extra3)
			{
				msg_print("This level has already been drained!");
				break;
			}

			p_ptr->druid_extra3 = TRUE;

			if (!get_aim_dir(&dir)) return;

			(void)fire_bolt(GF_MANA, dir, dam);

			break;
		}

		default:
		{
			msg_format("You cast an unknown Druid spell: %d.", spell);
			msg_print(NULL);

			break;
		}
	}
}



/*
 * Cast the given spell of the given realm
 */
void cast_spell(int realm, int spell, byte level)
{
	switch (realm)
	{
		/* SYMBIOTIC */
		case REALM_SYMBIOTIC:
		{
			cast_symbiotic_spell(spell, level);
			break;
		}

		/* MUSIC */
		case REALM_MUSIC:
		{
			cast_music_spell(spell, level);
			break;
		}

		/* DRUIDISTIC */
		case REALM_DRUID:
		{
			cast_druid_spell(spell, level);
			break;
		}

		/* DAEMON -SC- */
		case REALM_DAEMON:
		{
			cast_daemon_spell(spell, level);
			break;
		}

		default:
		{
			msg_format("You cast a spell from an unknown realm: "
			           "realm %d, spell %d, level %d.",
			           realm, spell, level);
			msg_print(NULL);

			break;
		}
	}
}


/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int item, sval, spell, realm;
	int chance, perc;

	cptr verb;
	cptr obj;

	object_type *o_ptr;

	magic_type *s_ptr;

	cptr q, s;


	verb = "cast";
	obj = "spell";

	/* No magic */
	if (p_ptr->antimagic)
	{
		msg_print("Your anti-magic field disrupts any magic attempts.");
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
	item_tester_hook = item_tester_hook_realmable;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no spell books!";
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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
	if (!get_spell(&spell, verb, sval, TRUE, o_ptr, TRUE))
	{
		if (spell == -2)
		{
			msg_format("You don't know any %ss in that book.", obj);
		}

		return;
	}


	/* Access the realm of the spellbook */
	realm = o_ptr->tval - TV_SYMBIOTIC_BOOK + 1;

	/* Access the spell */
	s_ptr = &realm_info[realm][spell];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_format("You do not have enough mana to %s this %s.", verb, obj);

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}

	if (s_ptr->slevel > p_ptr->lev)
	{
		msg_format("This %s is too difficult for you.", obj);
		return;
	}


	/* Spell failure chance */
	chance = spell_chance(spell, realm);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		int insanity = (p_ptr->msane - p_ptr->csane) * 100 / p_ptr->msane;
		char sfail[80];

		/* Flush input if told so */
		if (flush_failure) flush();

		/* Insane players can see something strange */
		if (rand_int(100) < insanity)
		{
			get_rnd_line("sfail.txt",sfail);
			msg_format("A cloud of %s appears above you.", sfail);
		}

		/* Normal failure messages */
		else
		{
			msg_print("You failed to get the spell off!");
		}

		sound(SOUND_FAIL);

		if ((o_ptr->tval == TV_MUSIC_BOOK) && (randint(100) < spell))
		{
			msg_print("You produce a horrible shriek!");
			shriek_effect();
		}
	}

	/* Process spell */
	else
	{
		/* Spells */
		cast_spell(realm, spell, get_spell_level(realm, spell) - 1);

		/* A spell was cast */
		if (!(spell_worked[realm][spell / 32] & BIT(spell % 32)) &&
		    (!(get_skill(SKILL_SORCERY) >= s_ptr->slevel) ||
		     !is_sorcery_realm(realm)))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			spell_worked[realm][spell / 32] |= BIT(spell % 32);

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);
		}
	}

	/* Take a turn */
	/* UGLY hack */
	if (USE_REALM(REALM_DAEMON))
	{
		if ((o_ptr->tval == TV_DAEMON_BOOK) && (item == INVEN_WIELD))
		{
			if (is_magestaff()) energy_use = 26;
			else energy_use = 33;
		}
		else if ((o_ptr->tval == TV_DAEMON_BOOK) && (item != INVEN_WIELD))
		{
			if (is_magestaff()) energy_use = 400;
			else energy_use = 500;
		}
		else
		{
			if (is_magestaff()) energy_use = 80;
			else energy_use = 100;
		}
	}
	else
	{
		if (is_magestaff()) energy_use = 80;
		else energy_use = 100;
	}

	/* Reduce time ? */
	perc = 100;
	perc -= 3 * (get_spell_level(realm, spell) - 1);
	energy_use = perc * energy_use / 100;

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
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}


/*
 * Schooled magic
 */

/*
 * Find a spell in any books/objects
 */
static int hack_force_spell = -1;
bool get_item_hook_find_spell(int *item)
{
        int i, spell;
        char buf[80];
        char buf2[100];

        strcpy(buf, "Manathrust");
        if (!get_string("Spell name? ", buf, 79))
                return FALSE;
        sprintf(buf2, "return find_spell(\"%s\")", buf);
        spell = exec_lua(buf2);
        if (spell == -1) return FALSE;
        for (i = 0; i < INVEN_TOTAL; i++)
        {
                object_type *o_ptr = &inventory[i];

                /* Must we wield it ? */
                if ((wield_slot(o_ptr) != -1) && (i < INVEN_WIELD)) continue;

                /* Is it a non-book? */
                if ((o_ptr->tval != TV_BOOK))
                {
                        u32b f1, f2, f3, f4, f5, esp;
                
                        /* Extract object flags */
                        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

                        if ((f5 & TR5_SPELL_CONTAIN) && (o_ptr->pval2 == spell))
                        {
                                *item = i;
                                hack_force_spell = spell;
                                return TRUE;
                        }
                }
                /* A random book ? */
                else if ((o_ptr->sval == 255) && (o_ptr->pval == spell))
                {
                        *item = i;
                        hack_force_spell = spell;
                        return TRUE;
                }
                /* A normal book */
                else if (o_ptr->sval != 255)
                {
                        sprintf(buf2, "return spell_in_book(%d, %d)", o_ptr->sval, spell);
                        if (exec_lua(buf2))
                        {
                                *item = i;
                                hack_force_spell = spell;
                                return TRUE;
                        }
                }
        }
        return FALSE;
}

/*
 * Get a spell from a book
 */
u32b get_school_spell(cptr do_what)
{
	int i, item;
	u32b spell = -1;
	int num = 0, where = 1;
	int ask;
	bool flag, redraw;
	char choice;
	char out_val[160];
	char buf2[40];
        object_type *o_ptr;
        int tmp;
        int sval, pval;

        hack_force_spell = -1;
        get_item_extra_hook = get_item_hook_find_spell;
        item_tester_hook = hook_school_spellable;
	sprintf(buf2, "You have no book to %s from", do_what);
        if (!get_item(&item, format("%^s from which book?", do_what), buf2, USE_INVEN | USE_EQUIP | USE_EXTRA )) return -1;

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

        /* If it can be wielded, it must */
        if ((wield_slot(o_ptr) != -1) && (item < INVEN_WIELD))
        {
                msg_format("You cannot %s from that object, it must be wielded first.", do_what);
                return -1;
        }

        if (repeat_pull(&tmp))
        {
                return tmp;
        }

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Show choices */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
	}

	/* No spell to cast by default */
	spell = -1;

        /* Is it a random book, or something else ? */
        if (o_ptr->tval == TV_BOOK)
        {
                sval = o_ptr->sval;
                pval = o_ptr->pval;
        }
        else
        {
                sval = 255;
                pval = o_ptr->pval2;
        }

        if (hack_force_spell == -1)
        {
                num = exec_lua(format("return book_spells_num(%d)", sval));

                /* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Spells %c-%c, Descs %c-%c, *=List, ESC=exit) %^s which spell? ",
                        I2A(0), I2A(num - 1), I2A(0) - 'a' + 'A', I2A(num - 1) - 'a' + 'A',  do_what);

                /* Get a spell from the user */
                while (!flag && get_com(out_val, &choice))
                {
                        /* Request redraw */
                        if (((choice == ' ') || (choice == '*') || (choice == '?')))
                        {
                                /* Show the list */
                                if (!redraw)
                                {
                                        /* Show list */
                                        redraw = TRUE;

                                        /* Save the screen */
                                        character_icky = TRUE;
                                        Term_save();

                                        /* Display a list of spells */
                                        where = exec_lua(format("return print_book(%d, %d)", sval, pval));
                                }

                                /* Hide the list */
                                else
                                {
                                        /* Hide list */
                                        redraw = FALSE;
                                        where = 1;

                                        /* Restore the screen */
                                        Term_load();
                                        character_icky = FALSE;
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

                        /* Verify it */
                        if (ask)
                        {
                                /* Show the list */
                                if (!redraw)
                                {
                                        /* Show list */
                                        redraw = TRUE;

                                        /* Save the screen */
                                        character_icky = TRUE;
                                        Term_load();
                                        Term_save();

                                }
                                /* Rstore the screen */
                                else
                                {
                                        /* Restore the screen */
                                        Term_load();
                                }

                                /* Display a list of spells */
                                where = exec_lua(format("return print_book(%d, %d)", sval, pval));
                                exec_lua(format("print_spell_desc(spell_x(%d, %d, %d), %d)", sval, pval, i, where));
                        }
                        else
                        {
                                /* Save the spell index */
                                spell = exec_lua(format("return spell_x(%d, %d, %d)", sval, pval, i));

                                /* Require "okay" spells */
                                if (!exec_lua(format("return is_ok_spell(%d)", spell)))
                                {
                                        bell();
                                        msg_format("You may not %s that spell.", do_what);
                                        spell = -1;
                                        continue;
                                }

                                /* Stop the loop */
                                flag = TRUE;
                        }
                }
        }
        else
        {
                /* Require "okay" spells */
                if (exec_lua(format("return is_ok_spell(%d)", hack_force_spell)))
                {
                        flag = TRUE;
                        spell = hack_force_spell;
                }
                else
                {
                        bell();
                        msg_format("You may not %s that spell.", do_what);
                        spell = -1;
                }
        }


	/* Restore the screen */
	if (redraw)
	{
		Term_load();
		character_icky = FALSE;
	}


	/* Show choices */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
	}


	/* Abort if needed */
	if (!flag) return -1;

        tmp = spell;
        repeat_push(tmp);
	return spell;
}

void cast_school_spell()
{
	int spell;

	/* No magic */
	if (p_ptr->antimagic)
	{
		msg_print("Your anti-magic field disrupts any magic attempts.");
		return;
	}

	spell = get_school_spell("cast");

	/* Actualy cast the choice */
	if (spell != -1)
	{
		exec_lua(format("cast_school_spell(%d, spell(%d))", spell, spell));
	}
}

void browse_school_spell(int book, int pval)
{
	int i;
	int num = 0, where = 1;
	int ask;
	char choice;
	char out_val[160];

	/* Show choices */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
	}

	num = exec_lua(format("return book_spells_num(%d)", book));

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Spells %c-%c, ESC=exit) cast which spell? ",
	        I2A(0), I2A(num - 1));

	/* Save the screen */
	character_icky = TRUE;
	Term_save();

	/* Display a list of spells */
	where = exec_lua(format("return print_book(%d, %d)", book, pval));

	/* Get a spell from the user */
	while (get_com(out_val, &choice))
	{
		/* Display a list of spells */
		where = exec_lua(format("return print_book(%d, %d)", book, pval));

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

                /* Restore the screen */
                Term_load();

                /* Display a list of spells */
                where = exec_lua(format("return print_book(%d, %d)", book, pval));
                exec_lua(format("print_spell_desc(spell_x(%d, %d, %d), %d)", book, pval, i, where));
	}


	/* Restore the screen */
	Term_load();
	character_icky = FALSE;

	/* Show choices */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
	}
}

/* Can it contains a schooled spell ? */
static bool hook_school_can_spellable(object_type *o_ptr)
{
        u32b f1, f2, f3, f4, f5, esp;
                
        /* Extract object flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

        if ((f5 & TR5_SPELL_CONTAIN) && (o_ptr->pval2 == -1))
                return TRUE;
        return FALSE;
}

/*
 * Copy a spell from a bok to an object
 */
void do_cmd_copy_spell()
{
        int spell = get_school_spell("copy");
        int item;
        object_type *o_ptr;

        if (spell == -1) return;
        item_tester_hook = hook_school_can_spellable;
        if (!get_item(&item, "Copy to which object? ", "You have no object to copy to.", (USE_INVEN | USE_EQUIP))) return;
        o_ptr = get_object(item);

        msg_print("You copy the spell!");
        o_ptr->pval2 = spell;
        inven_item_describe(item);
}
