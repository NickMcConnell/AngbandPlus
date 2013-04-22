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

extern void do_cmd_rerate(void);
extern bool item_tester_hook_armour(object_type *o_ptr);

/* Maximum number of tries for teleporting */
#define MAX_TRIES 300

/*
 * Increase a spell power
 */

/* adds spell power * 10% */
long apply_power_dam(long dam, int lvl, byte power)
{
        if (lvl < 0) lvl = 0;
        if (lvl > 50) lvl = 50;

        lvl *= power;

        dam += (lvl * dam) / 10;

        return (dam);
}
/* adds spell power dice */
long apply_power_dice(long dice, int lvl, byte power)
{
        if (lvl < 0) lvl = 0;
        if (lvl > 50) lvl = 50;

        lvl *= power;

        dice += lvl;

        return (dice);
}
/* adds spell power parts */
long apply_power_dur(long dur, int lvl, byte power)
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
        if (p_ptr->pclass != CLASS_SORCERER)
        {
                return (spell_level[realm][spell]);
        }
        else
        {
                return (1);
        }
}

/*
 * Hook to determine if an object is drainable
 */
static bool item_tester_hook_scroll_amulet(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_AMULET) || (o_ptr->tval == TV_SCROLL)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/* Are we using a mage staff */
bool is_magestaff()
{
        int i;

        i = 0;
        while (p_ptr->body_parts[i] == INVEN_WIELD)
        {
                object_type *o_ptr = &inventory[INVEN_WIELD + i];

                if ((o_ptr->k_idx) && (o_ptr->tval == TV_MSTAFF)) return (TRUE);

                i++;
        }

        return (FALSE);
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
static int get_spell(int *sn, cptr prompt, int sval, bool known, object_type *o_ptr, bool allow_list)
{
        int         realm = o_ptr->tval - TV_VALARIN_BOOK + 1;
	int         i;
        u32b        spell = -1;
	int         num = 0;
	int         ask;
        byte        spells[64];
	bool        flag, redraw, okay;
	char        choice;
	magic_type  *s_ptr;
	char        out_val[160];
        cptr        p = ((mp_ptr->spell_book == TV_VALARIN_BOOK) ? "prayer" : "spell");

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
                if ((fake_spell_flags[realm][sval][(spell < 32)] & (1L << (spell % 32))))
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
                if (get_spell_all_hack || spell_okay(spells[i], known, realm)) okay = TRUE;
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
                if (((choice == ' ') || (choice == '*') || (choice == '?')) && (allow_list))
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
                if ((!spell_okay(spell, known, realm)) && (!get_spell_all_hack))
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
                        s_ptr = &realm_info[realm][spell%64];

			/* Prompt */
                        strnfmt(tmp_val, 78, "Level %d %s %s (%d mana, %d%% fail)? ",
                                prompt, get_spell_level(realm, spell), spell_names[realm][spell%64][0],
                                s_ptr->smana, spell_chance(spell,realm));

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
        int             sval, line, col = 13;
	int		spell = -1;
	int		num = 0;

	byte		spells[64];

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
                if ((fake_spell_flags[o_ptr->tval - TV_VALARIN_BOOK + 1][sval][(spell < 32)] & (1L << (spell % 32))))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}


	/* Save the screen */
        character_icky = TRUE;
	Term_save();

	/* Display the spells */
        line = print_spells(spells, num, 1, 13, o_ptr->tval - TV_VALARIN_BOOK + 1);

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
                if (strlen(spell_names[o_ptr->tval - TV_VALARIN_BOOK + 1][spell][1]) > 80 - col)
                {
                        col = 80 - strlen(spell_names[o_ptr->tval - TV_VALARIN_BOOK + 1][spell][1]);
                }
                c_prt(TERM_L_BLUE, spell_names[o_ptr->tval - TV_VALARIN_BOOK + 1][spell][1], line, col);
        }
}


void do_cmd_browse(void)
{
        int             item;

	cptr q, s;

	object_type	*o_ptr;

	/* Warriors are illiterate */
	if (!(p_ptr->realm1 || p_ptr->realm2))
	{
		msg_print("You cannot read books!");
		return;
	}

	/* No lite */
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

#if 0
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
 * Modify the realm_info array using the number of spell learned as a
 * modificator
 */
void calc_magic_bonus()
{
        int i, j, num[MAX_REALM];

        for(i = 0; i < MAX_REALM; i++)
        {
                num[i] = 0;

                for(j = 0; j < 64; j++)
                {
                        if((spell_learned[i][j < 32] & (1 << (j % 32))) &&
                           !(spell_forgotten[i][j < 32] & (1 << (j % 32))))
                                num[i]++;
                }
        }

        for(i = 0; i < MAX_REALM; i++)
                for(j = 0; j < 64; j++)
                {
                        int tmp;

                        /* Take the base info */
                        realm_info[i][j].slevel = realm_info_base[i][j].slevel;
                        realm_info[i][j].smana = realm_info_base[i][j].smana;
                        realm_info[i][j].sfail = realm_info_base[i][j].sfail;
                        realm_info[i][j].sexp = realm_info_base[i][j].sexp;

                        tmp = realm_info[i][j].smana;
                        tmp -= (tmp * num[i]) / 80;
                        tmp = (tmp < 1)?1:(tmp > 255)?255:tmp;
                        realm_info[i][j].smana = tmp;

                        tmp = realm_info[i][j].sfail;
                        tmp -= (tmp * num[i]) / 100;
                        tmp = (tmp < 5)?5:(tmp > 95)?95:tmp;
                        realm_info[i][j].sfail = tmp;
                }
}



/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int	i, item, sval;

	int	spell = -1;

        cptr p = ((mp_ptr->spell_book == TV_MAGERY_BOOK) ? "spell" : "prayer");

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

	/* Mage -- Learn a selected spell */
#if 0
        if ((mp_ptr->spell_book != TV_VALARIN_BOOK) || (p_ptr->pclass == CLASS_MONK))
	{
		/* Ask for a spell, allow cancel */
                if (!get_spell(&spell, "study", sval, FALSE, o_ptr, TRUE) && (spell == -1)) return;
	}

	/* Priest -- Learn a random prayer */
        else
	{
		int k = 0;

		int gift = -1;

		/* Extract spells */
                for (spell = 0; spell < 64; spell++)
		{
			/* Check spells in the book */
                        if ((fake_spell_flags[o_ptr->tval - TV_VALARIN_BOOK + 1][sval][(spell < 32)] & (1L << (spell % 32))))
			{
				/* Skip non "okay" prayers */
                                if (!spell_okay(spell, FALSE, o_ptr->tval - TV_VALARIN_BOOK + 1)) continue;

				/* Hack -- Prepare the randomizer */
				k++;

				/* Hack -- Apply the randomizer */
				if (rand_int(k) == 0) gift = spell;
			}
		}

		/* Accept gift */
		spell = gift;
	}
#else
        /* Ask for a spell, allow cancel */
        if (!get_spell(&spell, "study", sval, FALSE, o_ptr, TRUE) && (spell == -1)) return;
#endif

	/* Nothing to study */
	if (spell < 0)
	{
		/* Message */
		msg_format("You cannot learn any %ss in that book.", p);

		/* Abort */
		return;
	}


	/* Take a turn */
	energy_use = 100;

        /* Increase spell power */
        if (spell_learned[o_ptr->tval - TV_VALARIN_BOOK + 1][(spell < 32)] & (1L << (spell % 32)))
        {
                spell_level[o_ptr->tval - TV_VALARIN_BOOK + 1][spell]++;

                msg_format("You have increased your knowledge of the %s of %s.",
                        p, spell_names[o_ptr->tval - TV_VALARIN_BOOK + 1][spell%64][0]);
        }
	/* Learn the spell */
        else
        {
                spell_learned[o_ptr->tval - TV_VALARIN_BOOK + 1][(spell < 32)] |= (1L << (spell % 32));
                spell_level[o_ptr->tval - TV_VALARIN_BOOK + 1][spell] = 1;

                /* Find the next open entry in "spell_order[]" */
                for (i = 0; i < 64; i++)
                {
                        /* Stop at the first empty space */
                        if (spell_order[i] == 99) break;
                }

                /* Add the spell to the known list */
                realm_order[i] = o_ptr->tval - TV_VALARIN_BOOK + 1;
                spell_order[i] = spell;

                /* Mention the result */
                msg_format("You have learned the %s of %s.",
                        p, spell_names[o_ptr->tval - TV_VALARIN_BOOK + 1][spell%64][0]);
        }

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

        /* Recalculate realm_info according to the number of spell learned */
        calc_magic_bonus();

	/* Save the new_spells value */
	p_ptr->old_spells = p_ptr->new_spells;

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);
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

	msg_print("You feel a change coming over you...");

	if ((power > rand_int(20)) && (rand_int(3) == 1))
	{
		char effect_msg[80] = "";
		int new_race, expfact, goalexpfact;

		/* Some form of racial polymorph... */
		power -= 10;

		if ((power > rand_int(5)) && (rand_int(4) == 1))
		{
			/* sex change */
			power -= 2;

			if (p_ptr->psex == SEX_MALE)
			{
				p_ptr->psex = SEX_FEMALE;
				sp_ptr = &sex_info[p_ptr->psex];
				sprintf(effect_msg,"female");
			}
			else
			{
				p_ptr->psex = SEX_MALE;
				sp_ptr = &sex_info[p_ptr->psex];
				sprintf(effect_msg,"male");
			}
		}

		if ( power>rand_int(30) && rand_int(5)==1 )
		{
			int tmp = 0;

			/* Harmful deformity */
			power -= 15;

			while (tmp < 6)
			{
				if ( rand_int(2)==1 )
				{
					(void)dec_stat(tmp, randint(6)+6, (randint(3)==1));
					power -= 1;
				}
				tmp++;
			}

			/* Deformities are discriminated against! */
			(void)dec_stat(A_CHR, randint(6), TRUE);

			if (effect_msg[0])
			{
				char tmp_msg[10];
				sprintf(tmp_msg,"%s",effect_msg);
				sprintf(effect_msg,"deformed %s",tmp_msg);
			}
			else
			{
				sprintf(effect_msg,"deformed");
			}
		}

		while ( power>rand_int(20) && rand_int(10)==1 )
		{
                        /* Polymorph into a less corrupted form */
			power -= 10;

                        lose_corruption(0);
		}

		/* Restrict the race choices by exp penalty so weak polymorph
		always means weak race
		*/
		goalexpfact = 100 + 3*rand_int(power);

		do
		{
			new_race = rand_int(MAX_RACES);
			expfact = race_info[new_race].r_exp;
		}
		while ( (new_race == p_ptr->prace) && (expfact > goalexpfact) );

		if (effect_msg[0])
		{
			msg_format("You turn into a%s %s!",
                                (( new_race == RACE_ELF )?"n":""),
				race_info[new_race].title);
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

		/* Calculate the height/weight for males */
                if (p_ptr->psex == SEX_MALE)
                {
                        p_ptr->ht = randnor(rp_ptr->m_b_ht + rmp_ptr->m_b_ht, rp_ptr->m_m_ht + rmp_ptr->m_m_ht);
                        p_ptr->wt = randnor(rp_ptr->m_b_wt + rmp_ptr->m_b_wt, rp_ptr->m_m_wt + rmp_ptr->m_m_wt);
                }

                /* Calculate the height/weight for females */
                else if (p_ptr->psex == SEX_FEMALE)
                {
                        p_ptr->ht = randnor(rp_ptr->f_b_ht + rmp_ptr->f_b_ht, rp_ptr->f_m_ht + rmp_ptr->f_m_ht);
                        p_ptr->wt = randnor(rp_ptr->f_b_wt + rmp_ptr->f_b_wt, rp_ptr->f_m_wt + rmp_ptr->f_m_wt);
                }

                /* Calculate the height/weight for neuters */
                else if (p_ptr->psex == SEX_NEUTER)
                {
                        p_ptr->ht = randnor((rp_ptr->m_b_ht + rmp_ptr->m_b_ht + rp_ptr->f_b_ht + rmp_ptr->f_b_ht) / 2, (rp_ptr->m_m_ht + rmp_ptr->m_m_ht + rp_ptr->f_m_ht + rmp_ptr->f_m_ht) / 2);
                        p_ptr->wt = randnor((rp_ptr->m_b_wt + rmp_ptr->m_b_wt + rp_ptr->f_b_wt + rmp_ptr->f_b_wt) / 2, (rp_ptr->m_m_wt + rmp_ptr->m_m_wt + rp_ptr->f_m_wt + rmp_ptr->f_m_wt) / 2);
                }

		check_experience();
		p_ptr->max_plv = p_ptr->lev;

		p_ptr->redraw |= (PR_BASIC);

		p_ptr->update |= (PU_BONUS);

		handle_stuff();
		lite_spot(py, px);
	}

	if ((power > rand_int(30)) && (rand_int(6) == 1))
	{
		int tmp = 0;

		/* Abomination! */
		power -= 20;

		msg_print("Your internal organs are rearranged!");
		while (tmp < 6)
		{
			(void)dec_stat(tmp, randint(6)+6, (randint(3)==1));
			tmp++;
		}
		if (randint(6)==1)
		{
			msg_print("You find living difficult in your present form!");
                        take_hit(damroll(randint(10),p_ptr->lev), "a lethal corruption");
			power -= 10;
		}
	}

	if ((power > rand_int(20)) && (rand_int(4) == 1))
	{
		power -= 10;

		do_cmd_rerate();
	}

	while ((power > rand_int(15)) && (rand_int(3) == 1))
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

	o_ptr = &inventory[INVEN_WIELD];

	/* you can never modify artifacts / ego-items */
	/* you can never modify cursed items */
	/* TY: You _can_ modify broken items (if you're silly enough) */
	if ((o_ptr->k_idx) &&
		(!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
		(!(o_ptr->art_name)) && (!cursed_p(o_ptr)))
	{
		cptr act = NULL;

		/* Let's get the name before it is changed... */
		char o_name[80];
		object_desc(o_name, o_ptr, FALSE, 0);

		switch (brand_type)
		{
                case 6:
                        act = "glows with godly power.";
                        o_ptr->name2 = EGO_BLESS_BLADE;
                        o_ptr->pval = randint(4);
			break;
                case 5:
                        act = "seems very powerful.";
                        o_ptr->name2 = EGO_EARTHQUAKES;
                        o_ptr->pval = randint(3);
			break;
		case 4:
			act = "seems very unstable now.";
                        o_ptr->name2 = EGO_DRAGON;
			o_ptr->pval = randint(2);
			break;
		case 3:
			act = "thirsts for blood!";
                        o_ptr->name2 = EGO_VAMPIRIC;
			break;
		case 2:
			act = "is coated with poison.";
                        o_ptr->name2 = EGO_BRAND_POIS;
			break;
		case 1:
                        act = "is engulfed in raw chaos!";
                        o_ptr->name2 = EGO_CHAOTIC;
			break;
		default:
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
                /* Apply the ego */
                apply_magic(o_ptr, dun_level, FALSE, FALSE, FALSE);

		msg_format("Your %s %s", o_name, act);

		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}
	else
	{
		if (flush_failure) flush();

		msg_print("The Branding failed.");
	}
}


static void call_the_(void)
{
	int i;

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
		msg_format("You %s the %s too close to a wall!",
                        ((mp_ptr->spell_book == TV_VALARIN_BOOK) ? "recite" : "cast"),
                        ((mp_ptr->spell_book == TV_VALARIN_BOOK) ? "prayer" : "spell"));
		msg_print("There is a loud explosion!");

		/* Prevent destruction of quest levels and town */
		if (!is_quest(dun_level) && dun_level)
		{
			destroy_area(py, px, 20+(p_ptr->lev), TRUE);
			msg_print("The dungeon collapses...");
		}
		else
		{
			msg_print("The dungeon trembles...");
		}
		take_hit(100 + (randint(150)), "a suicidal Call the Void");
	}
}


/*
 * Fetch an item (teleport it right underneath the caster)
 */
void fetch(int dir, int wgt, bool require_los)
{
	int             ty, tx, i;
	bool            flag;
	cave_type       *c_ptr;
	object_type     *o_ptr;
	char            o_name[80];

	/* Check to see if an object is already there */
	if (cave[py][px].o_idx)
	{
		msg_print("You can't fetch when you're already standing on something.");
		return;
	}

	/* Use a target */
	if (dir == 5 && target_okay())
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

		do
		{
			ty += ddy[dir];
			tx += ddx[dir];
			c_ptr = &cave[ty][tx];

			if ((distance(py, px, ty, tx)> MAX_RANGE)
				|| !cave_floor_bold(ty, tx)) return;
		}
		while(!c_ptr->o_idx);
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

void shriek_effect()
{
        switch(randint(9))
        {
                case 1: case 5: case 8: case 9:
                        msg_print("You made a high pitched shriek!");
                        aggravate_monsters(1);
                        break;
                case 2: case 6:
                        msg_print("Oups! You call a monster.");
                        summon_specific(py, px, max_dlv[dungeon_type], 0);
                        break;
                case 3: case 7:
                        msg_print("The dungeon collapses!");
                        earthquake(py, px, 5);
                        break;
                case 4:
                        msg_print("Your shriek is so horrible that you damage your health!");
                        take_hit(damroll(p_ptr->lev/5,8),"inner hemorrhaging");
                        break;
        }
}

void wild_magic(int spell)
{
	int counter = 0;
	int type = SUMMON_BIZARRE1 - 1 + randint(6);

	if (type < SUMMON_BIZARRE1) type = SUMMON_BIZARRE1;
	else if (type > SUMMON_BIZARRE6) type = SUMMON_BIZARRE6;

	switch(randint(spell) + randint(8) + 1)
	{
	case 1:
	case 2:
	case 3:
		teleport_player(10);
		break;
	case 4:
	case 5:
	case 6:
		teleport_player(100);
		break;
	case 7:
	case 8:
		teleport_player(200);
		break;
	case 9:
	case 10:
	case 11:
		unlite_area(10, 3);
		break;
	case 12:
	case 13:
	case 14:
		lite_area(damroll(2, 3), 2);
		break;
	case 15:
		destroy_doors_touch();
		break;
	case 16: case 17:
		wall_breaker();
	case 18:
		sleep_monsters_touch();
		break;
	case 19:
	case 20:
		trap_creation();
		break;
	case 21:
	case 22:
		door_creation();
		break;
	case 23:
	case 24:
	case 25:
		aggravate_monsters(1);
		break;
	case 26:
		/* Prevent destruction of quest levels and town */
		if (!is_quest(dun_level) && dun_level)
			earthquake(py, px, 5);
		break;
	case 27:
	case 28:
                (void) gain_random_corruption(0);
		break;
	case 29:
	case 30:
		apply_disenchant(0);
		break;
	case 31:
		lose_all_info();
		break;
	case 32:
		fire_ball(GF_CHAOS, 0, spell + 5, 1 + (spell / 10));
		break;
	case 33:
		wall_stone();
		break;
	case 34:
	case 35:
		while (counter++ < 8)
		{
			(void) summon_specific(py, px, (dun_level * 3) / 2, type);
		}
		break;
	case 36:
	case 37:
		activate_hi_summon();
		break;
	case 38:
		summon_cyber();
	default:
		activate_ty_curse();
	}

	return;
}



void cast_valarin_spell(int spell, byte level)
{
	int	dir;
        int     plev = p_ptr->lev, i;
	bool    no_trump = FALSE;
        int     to_s = (level + p_ptr->to_s);

	switch (spell)
	{
	   case 0: /* Detect Evil */
                        if (info_spell) return;
                       (void)detect_monsters_evil();
		       break;
           case 1: /* Call Light */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " rad %ld", apply_power_dice(1 + (plev / 10), to_s, 1));
                                return;
                        }
                       (void)lite_area(damroll(2, (plev / 2)), apply_power_dice(1 + (plev / 10), to_s, 1));
		       break;
	   case 2: /* Bless */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d12", apply_power_dur(12, to_s, 3));
                                return;
                        }
                       (void)set_blessed(p_ptr->blessed + randint(12) + apply_power_dur(12, to_s, 3));
		       break;
	   case 3: /* Remove Fear */
                        if (info_spell) return;
                       (void)set_afraid(0);
		       break;
           case 4: /* Cure Light Wounds */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ldd10", apply_power_dice(2, to_s, 1));
                                return;
                        }
                        (void)hp_player(damroll(apply_power_dice(2, to_s, 1), 10));
			(void)set_cut(p_ptr->cut - 10);
		       break;
           case 5: /* Slow Poison */
                        if (info_spell) return;
                        (void)set_poisoned((p_ptr->poisoned / 2) - to_s);
		       break;
           case 6: /* Detect Secret Doors */
                        if (info_spell) return;
			(void)detect_doors();
			(void)detect_stairs();
		       break;
           case 7: /* Detect Traps */
                        if (info_spell) return;
			(void)detect_traps();
		       break;

           case 8: /* Spear of Light */
                        if (info_spell) return;
                       if (!get_aim_dir(&dir)) return;
                       msg_print("A line of blue shimmering light appears.");
                       lite_line(dir);
                       break;
           case 9: /* Remove Curse */
                        if (info_spell) return;
			remove_curse();
		       break;
           case 10: /* Cure Serious Wounds */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " heal %ldd10", apply_power_dice(6, to_s, 1));
                                return;
                        }
                        (void)hp_player(damroll(apply_power_dice(6, to_s, 1), 10));
			(void)set_cut(0);
		       break;
           case 11: /* Satisfy Hunger */
                        if (info_spell) return;
			(void)set_food(PY_FOOD_MAX - 1);
		       break;
           case 12: /* Holy Chant */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d24", apply_power_dur(12, to_s, 3));
                                return;
                        }
                       set_blessed(p_ptr->blessed + randint(24) + apply_power_dur(12, to_s, 1));
		       break;
           case 13: /* Sense Surroundings */
                        if (info_spell) return;
                       map_area();
		       break;
           case 14: /* Neutralize Poison */
                        if (info_spell) return;
                       set_poisoned(0);
		       break;
           case 15: /* resist fire&cold */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", apply_power_dur(10, to_s, 3));
                                return;
                        }
                       set_oppose_fire(p_ptr->oppose_fire + randint(10) + apply_power_dur(10, to_s, 1));
                       set_oppose_cold(p_ptr->oppose_cold + randint(10) + apply_power_dur(10, to_s, 1));
                       break;
           case 16: /* Cure critical wounds */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " heal %ldd10", apply_power_dice(8, to_s, 3));
                                return;
                        }
                        (void)hp_player(damroll(apply_power_dice(8, to_s, 1), 10));
                        (void)set_stun(0);
			(void)set_cut(0);
                       break;
           case 17: /* Orb or Draining */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam 3d6+%ld", apply_power_dur(plev + (plev / 2), to_s, 1));
                                return;
                        }
                       if (!get_aim_dir(&dir)) return;
                       fire_ball(GF_HOLY_FIRE, dir, damroll(3, 6) + apply_power_dur(plev + (plev / 2), to_s, 1),
                                        apply_power_dice(((plev < 30) ? 2 : 3), to_s, 1));
		       break;
           case 18: /* Portal */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " range %ld", apply_power_dur(200, to_s, 10));
                                return;
                        }
                        teleport_player(apply_power_dur(200, to_s, 10));
			break;
       case 19: /* Sense Unseen */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d20", apply_power_dur(20, to_s, 5));
                                return;
                        }
                       set_tim_invis(p_ptr->tim_invis + apply_power_dur(20, to_s, 5) + randint(20));
                       break;
                case 20: /* Holy Blade */
                {
                        object_type forge, *q_ptr = &forge;

                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", apply_power_dur(10, to_s, 2));
                                return;
                        }

                        object_prep(q_ptr, lookup_kind(TV_HAFTED, SV_MACE));
                        q_ptr->name2 = EGO_HA;
                        q_ptr->number = 1;
                        apply_magic(q_ptr, plev + to_s, FALSE, FALSE, FALSE);
                        q_ptr->timeout = apply_power_dur(10, to_s, 2) + randint(10);
                        q_ptr->art_flags5 |= TR5_TEMPORARY;
                        object_aware(q_ptr);
                        object_known(q_ptr);
                        (void)inven_carry(q_ptr, FALSE);
                        break;
                }
           case 21: /* Turn Undead */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ldd10", apply_power_dice(5, to_s, 1));
                                return;
                        }
			if (!get_aim_dir(&dir)) return;
                        fire_beam(GF_TURN_UNDEAD, dir, damroll(apply_power_dice(5, to_s, 1), 10));
			break;
           case 22: /* Perception */
                        if (info_spell) return;
                       ident_spell();
		       break;
           case 23: /* Holy Aura */
           {
                       int t = randint(25);

                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %d+d25", to_s + 3 * plev);
                                return;
                        }
                       (void)set_holy(p_ptr->holy + t + to_s + 3 * plev);
                       (void)set_protevil(p_ptr->protevil + t + 3 * plev + to_s);
		       break;
           }
           case 24: /* Heroism */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d20", apply_power_dur(20, to_s, 8));
                                return;
                        }
                        (void)set_hero(p_ptr->hero + randint(20) + apply_power_dur(20, to_s, 8));
		       break;
           case 25: /* Word of recall */
                        if (info_spell) return;
                       recall_player();
		       break;
           case 26: /* Dispel undead */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", apply_power_dam(60, to_s, 1));
                                return;
                        }
                       dispel_undead(apply_power_dam(60, to_s, 1));
                       break;
           case 27: /* Heal */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " heal %ld", apply_power_dam(150, to_s, 1));
                                return;
                        }
                       (void)hp_player(apply_power_dam(150, to_s, 1));
                       (void)set_stun(0);
                       (void)set_cut(0);
		       break;
           case 28: /* Banish */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", apply_power_dam(100, to_s, 1));
                                return;
                        }
                       project_hack(GF_AWAY_ALL, apply_power_dam(100, to_s, 1));
		       break;
           case 29: /* Dispel evil */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", apply_power_dam(120, to_s, 1));
                                return;
                        }
                       dispel_evil(apply_power_dur(120, to_s, 1));
		       break;
           case 30: /* Holy Word */
                        if (info_spell) return;
                        (void)dispel_evil(apply_power_dam(plev * 4, to_s, 1));
                        (void)hp_player(apply_power_dam(500, to_s, 1));
			(void)set_afraid(0);
			(void)set_poisoned(0);
			(void)set_stun(0);
			(void)set_cut(0);
		       break;
                case 31: /* animal taming */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", apply_power_dam(100, to_s, 1));
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        (void) charm_animal(dir, apply_power_dam(100, to_s, 1));
                        break;
                case 32: /* stone to mud */
                        if (info_spell) return;
			if (!get_aim_dir(&dir)) return;
			(void)wall_to_mud(dir);
                        break;
                case 33: /* sterilization */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", apply_power_dur(10, to_s, 1));
                                return;
                        }
                        set_no_breeders(no_breeds + randint(10) + apply_power_dur(10, to_s, 1));
                        break;
                case 34: /* summon animal */
                        if (info_spell) return;
                        if (!(summon_specific_friendly(py, px, apply_power_dice(plev / 2, to_s, 1), SUMMON_ANIMAL_RANGER, TRUE)))
                                no_trump = TRUE;
                        break;
                case 35: /* haste self */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %d+d%d", plev + to_s, 20 + plev);
                                return;
                        }
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev) ) + plev + to_s);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5) + to_s);
			}
                        break;
                case 36: /* call hounds */
                        if (info_spell) return;
                        msg_format("You magically summon hounds.");
                        for (i = 0; i < 6; i++)
                        {
                                summon_specific_friendly(py, px, apply_power_dice(dun_level, to_s, 1), SUMMON_HOUND, TRUE);
                        }
                case 37: /* whirlwind attack */
                        if (info_spell) return;
                         {
                         int y = 0, x = 0;
                         cave_type       *c_ptr;
                         monster_type    *m_ptr;

                         for (dir = 0; dir <= 9; dir++)
                         {
                                y = py + ddy[dir];
                                x = px + ddx[dir];
                                c_ptr = &cave[y][x];

                                /* Get the monster */
                                m_ptr = &m_list[c_ptr->m_idx];

                                /* Hack -- attack monsters */
                                if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x)))
                                py_attack(y, x, -1);
                         }
                         }
                         break;

                case 38: /* unbarring ways */
                        if (info_spell) return;
                        (void)destroy_doors_touch();
                        break;
                case 39: /* sanctuary */
                        if (info_spell) return;
                        (void)sleep_monsters_touch();
                        break;
                case 40: /* dispel curse */
                        if (info_spell) return;
                        remove_all_curse();
                        break;
                case 41: /* tread water */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %d+d%ld", plev, apply_power_dam(plev, to_s, 5));
                                return;
                        }
                        set_walk_water(p_ptr->walk_water + plev + randint(apply_power_dur(plev, to_s, 5)));
                        break;
                case 42: /* healing */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " heal %ld", apply_power_dam(300, to_s, 1));
                                return;
                        }
                        (void)hp_player(apply_power_dam(300, to_s, 1));
			(void)set_stun(0);
			(void)set_cut(0);
			(void)set_poisoned(0);
                        (void)set_image(0);
                        (void)set_afraid(0);
                        break;
                case 43: /* bless weapon */
                        if (info_spell) return;
                        brand_weapon(6);
                        break;
                case 44: /* glyph of warding */
                        if (info_spell) return;
                        warding_glyph();
                        break;
                case 45: /* invis */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d24", apply_power_dice(0, to_s, 3));
                                return;
                        }
                        (void)set_invis(p_ptr->tim_invisible + randint(24) + apply_power_dice(0, to_s, 3), 30);
                        break;
                case 46: /* CMW */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " heal %ldd10", apply_power_dice(8, to_s, 1));
                                return;
                        }
                        (void)hp_player(damroll(apply_power_dice(8, to_s, 1), 10));
                        (void)set_stun(0);
                        (void)set_cut(0);
                        break;
                case 47: /* mass sleep */
                        if (info_spell) return;
			(void)sleep_monsters();
                        break;
                case 48: /* remembrance */
                        if (info_spell) return;
                        (void)restore_level();
                        break;
                case 49: /* restoration */
                        if (info_spell) return;
                        (void)do_res_stat(A_STR);
                        (void)do_res_stat(A_INT);
                        (void)do_res_stat(A_WIS);
                        (void)do_res_stat(A_DEX);
                        (void)do_res_stat(A_CON);
                        (void)do_res_stat(A_CHR);
                        break;
                case 50: /* mass charm */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", apply_power_dam(100, to_s, 1));
                                return;
                        }
                        charm_monsters(apply_power_dam(100, to_s, 1));
                        break;
                case 51: /* wraithform */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d%d", apply_power_dice(plev / 2, to_s, 1), plev / 2);
                                return;
                        }
                        set_shadow(p_ptr->tim_wraith + randint(plev/2) + apply_power_dice(plev / 2, to_s, 1));
                        break;
                case 52: /* chain lightning */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam 8*%dd%ld", 5 + (plev / 10), apply_power_dice(8, to_s, 1));
                                return;
                        }
                        for (dir = 0; dir <= 9; dir++)
                                fire_beam(GF_ELEC, dir, damroll(5 + (plev / 10), apply_power_dice(8, to_s, 1)));
                        break;
                case 53: /* disintegration */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", 80 + apply_power_dam(plev, to_s, 4));
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_DISINTEGRATE, dir,
                                  80 + apply_power_dam(plev, to_s, 4), apply_power_dice(3, to_s, 1));
                        break;
                case 54: /* blizard */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", 70 + apply_power_dam(plev, to_s, 4));
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
			fire_ball(GF_COLD, dir,
                                70 + apply_power_dam(plev, to_s, 4), apply_power_dice(1, to_s, 1));
                        break;
                case 55: /* whirlpool */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", 100 + apply_power_dam(plev, to_s, 4));
                                return;
                        }
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_WATER, dir,
                                100 + apply_power_dam(plev, to_s, 4), apply_power_dice(1 + (plev / 12), to_s, 1));
                        break;
                case 56: /* lightning storm */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", 90 + apply_power_dam(plev, to_s, 4));
                                return;
                        }
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_ELEC, dir,
                                90 + apply_power_dam(plev, to_s, 4), apply_power_dam(1 + (plev / 12), to_s, 1));
                        break;
                case 57: /* meteor swarm */
                {
		       int x, y, dx, dy, d, count = 0;
		       int b = 10 + randint(10);

                        if (info_spell) return;

		       for (i = 0; i < b; i++) {
			   do {
			       count++;
			       if (count > 1000)  break;
			       x = px - 5 + randint(10);
			       y = py - 5 + randint(10);
			       dx = (px > x) ? (px - x) : (x - px);
			       dy = (py > y) ? (py - y) : (y - py);
			       /* Approximate distance */
                               d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));
                           } while ((d > 5) || (!(player_has_los_bold(y, x))));

			   if (count > 1000)   break;
			   count = 0;
                           project(0, 2, y, x, apply_power_dam(plev * 3, to_s, 1) / 2, GF_METEOR, PROJECT_KILL|PROJECT_JUMP|PROJECT_ITEM);
		       }
		   }
                        break;
                case 58: /* infusion */
                        if (info_spell) return;
                        (void)recharge(apply_power_dam(plev * 2, to_s, 1));
                        break;
                case 59: /* alter reality */
                        if (info_spell) return;
                        alter_reality();
                        break;
                case 60: /* restore life */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " heal %ld", apply_power_dam(3000, to_s, 1));
                                return;
                        }
			(void)restore_level();
                        hp_player(apply_power_dam(3000, to_s, 1));
                        break;
                case 61: /* call angel */
                {
                        if (info_spell) return;
                        msg_format("You magically summon an angel.");
                        summon_specific_friendly(py, px, dun_level + to_s, SUMMON_ANGEL, TRUE);
                        break;
                }
                case 62: /* earendil's star */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", apply_power_dam(150, to_s, 2));
                                return;
                        }
                        fire_ball(GF_LITE, 0, apply_power_dam(150, to_s, 2), apply_power_dice(7, to_s, 1));
                        wiz_lite();
                        if (((p_ptr->pracem == RMOD_VAMPIRE)||(p_ptr->mimic_form == MIMIC_VAMPIRE)) && !(p_ptr->resist_lite))
                        {
                               msg_print("The starlight scorches your flesh!");
                               take_hit(50, "starlight");
                        }
                        break;
                case 63: /* divinity */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d5", apply_power_dice(8, to_s, 1));
                                return;
                        }
                        if (p_ptr->mimic_form != MIMIC_VALAR)
                                set_mimic(apply_power_dice(8, to_s, 1) + randint(5), MIMIC_VALAR);
                        else
                                set_mimic(p_ptr->tim_mimic + 1 + randint(2), MIMIC_VALAR);
                        break;

	       default:
                 msg_format("You cast an unknown Valarin spell: %d.", spell);
		 msg_print(NULL);
	   }

	if (no_trump)
	{
                msg_print("Nobody answers to your call.");
	}
}

void cast_magery_spell(int spell, byte level)
{
        int     dir, beam;
        int     plev = p_ptr->lev;
        int     to_s = level + p_ptr->to_s;
        long    rad, dam;

        if (cp_ptr->flags1 & CF1_BEAM) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
                case 0: /* Magic Missile */
                        dam = apply_power_dice(4, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd%ld", 3 + ((plev - 1) / 5), dam);
                                return;
                        }
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                                                  damroll(3 + ((plev - 1) / 5), dam));
                        break;
	   case 1: /* Phase Door */
                        dam = apply_power_dam(10, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " range %ld", dam);
                                return;
                        }
                        teleport_player(dam);
		       break;
           case 2: /* Detect Monsters */
                        if (info_spell) return;
			(void)detect_monsters_normal();
		       break;
           case 3: /* Detect Traps */
                        if (info_spell) return;
			(void)detect_traps();
		       break;
           case 4: /* Light Area */
                        if (info_spell) return;
                        (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
                       break;
           case 5: /* Detect Doors/Stairs */
                        if (info_spell) return;
                        (void)detect_doors();
                        (void)detect_stairs();
		       break;
           case 6: /* Confuse Monster */
                        dam = apply_power_dam((plev * 3) / 2, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
			if (!get_aim_dir(&dir)) return;
                        (void)confuse_monster(dir, dam);
			break;
                case 7: /* Scan Object */
                        if (info_spell) return;
                        psychometry();
                        break;

                case 8: /* Noxious Cloud */
                        rad = apply_power_dice(2, to_s, 1);
                        dam = apply_power_dam(10 + (plev / 2), to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_POIS, dir,
                                dam, rad);
                        break;
                case 9: /* Teleport */
                        dam = apply_power_dur(plev * 5, to_s, 5);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " range %ld", dam);
                                return;
                        }
                        teleport_player(dam);
                        break;
                case 10: /* Beam of Light */
                        if (info_spell) return;
                        if (!get_aim_dir(&dir)) return;
                        msg_print("A line of blue shimmering light appears.");
                        lite_line(dir);
                        break;
                case 11: /* Sleep Monster */
                        if (info_spell) return;
                        if (!get_aim_dir(&dir)) return;
                        (void)sleep_monster(dir);
                        break;
                case 12: /* Lightning Bolt */
                        dam = apply_power_dice(8, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd%ld", 5+((plev-5)/4), dam);
                                return;
                        }
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                                  damroll(5+((plev-5)/4), dam));
                        break;
                case 13: /* Stone to Mud */
                        if (info_spell) return;
			if (!get_aim_dir(&dir)) return;
			(void)wall_to_mud(dir);
                        break;
                case 14: /* Frost Bolt */
                        dam = apply_power_dice(8, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd%ld", 6+((plev-5)/4), dam);
                                return;
                        }
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam-10, GF_COLD, dir,
                                damroll(6+((plev-5)/4), dam));
                        break;
                case 15: /* Recharging */
                        if (info_spell) return;
                        (void)recharge(30 + to_s);
                        break;

                case 16: /* Ethereal Eye */
                        dam = apply_power_dur(20, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d20", dam);
                                return;
                        }
                        map_area();
                        set_tim_invis(p_ptr->tim_invis + dam + randint(20));
                        break;
                case 17: /* Fire Bolt */
                        dam = apply_power_dice(8, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd%ld", 9+((plev-5)/4), dam);
                                return;
                        }
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
                                damroll(9+((plev-5)/4), dam));
                        break;
                case 18: /* Identify */
                        if (info_spell) return;
			(void)ident_spell();
                        break;
                case 19: /* Typhoon Daze */
                        rad = apply_power_dice(2, to_s, 1);
                        dam = apply_power_dam(30, to_s, 10);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                        fire_ball(GF_CONFUSION, 0, dam, rad);
                        break;
                case 20: /* Time Distortion */
                        if (info_spell) return;
                        slow_monsters();
                        break;
                case 21: /* Haste Self */
                        dam = apply_power_dur(20, to_s, 6);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d%d", dam, 20 + plev);
                                return;
                        }
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev)) + dam);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5) + 5);
			}
                        break;
                case 22: /* Elemental Blast */
                        dam = apply_power_dice(8, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd%ld", 5+((plev-5)/4), dam);
                                return;
                        }
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
                                damroll(5+((plev-5)/4), dam));
                        fire_bolt_or_beam(beam, GF_COLD, dir,
                                damroll(5+((plev-5)/4), dam));
                        fire_bolt_or_beam(beam, GF_ACID, dir,
                                damroll(5+((plev-5)/4), dam));
                        fire_bolt_or_beam(beam, GF_ELEC, dir,
                                damroll(5+((plev-5)/4), dam));
		       break;
                case 23: /* Teleport Away */
                        if (info_spell) return;
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_beam(GF_AWAY_ALL, dir, plev);
                        break;

                case 24: /* Scan monster */
                {
                        monster_type *m_ptr;
                        monster_race *r_ptr;
                        int m;

                        if (info_spell) return;

                        msg_print("You recall legends of the fair and foul creatures of the world...");

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
                case 25: /* Meditation */
                        dam = apply_power_dur(20, to_s, 6);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d20", dam);
                                return;
                        }
                        set_meditation(p_ptr->meditation + dam + randint(20));
                        break;
                case 26: /* Gravitic Distortion */
                        rad = apply_power_dice(2, to_s, 1);
                        dam = apply_power_dam(30, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        fire_ball(GF_GRAVITY, 0, dam, rad);
                        break;
                case 27: /* Dopplegangaer */
                {
                        int ii, ij;

                        if (info_spell) return;
                        msg_print("Choose a location for your doppleganger.");
                        if (!tgt_pt(&ii,&ij)) return;
                        if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY))
                        {
                                msg_print("You can't put a doppleganger here!");
                                break;
                        }
                        place_monster_aux(ij, ii, test_monster_name("Doppleganger"), FALSE, FALSE, TRUE);
                        break;
                }
                case 28: /* Firestorm */
                        rad = apply_power_dice(2, to_s, 1);
                        dam = apply_power_dam(100, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                       if (!get_aim_dir(&dir)) return;
                       (void)fire_ball(GF_FIRE, dir, dam, rad);
		       break;
                case 29: /* Force Shield */
                        dam = apply_power_dur(30, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d20", dam);
                                return;
                        }
                       (void)set_shield(p_ptr->shield + randint(20) + dam, apply_power_dur(50, to_s, 10), 0);
		       break;
                case 30: /* Crippled Gaze */
                        dam = apply_power_dam(100, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_STUN_CONF, dir, dam, 0);
                        (void)fire_ball(GF_OLD_SLOW, dir, dam, 0);
                        break;
                case 31: /* Collapse Cieling */
                {
                        int y, x;

                        rad = apply_power_dice(5, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " rad %ld", rad);
                                return;
                        }

                       if (!tgt_pt(&x, &y)) break;
                       earthquake(y, x, rad);
		       break;
                }

                case 32: /* Gravitic Beam */
                        dam = apply_power_dam(20, to_s, 4);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                       if (!get_aim_dir(&dir)) return;
                       (void)fire_beam(GF_GRAVITY, dir, dam);
		       break;
                case 33: /* Sanctuary */
                        if (info_spell) return;
                       wall_stone();
		       break;
                case 34: /* Starbust */
                {
                        int i;

                        dam = apply_power_dam(90, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam 8*%ld", dam);
                                return;
                        }
                        for (i = 1; i < 10; i++)
                        {
                                if (i - 5) fire_beam(GF_LITE, i, dam);
                        }
                        break;
                }
                case 35: /* Statis Cage */
                        dam = apply_power_dam(100, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_OLD_SLEEP, dir, dam, 0);
                        break;
                case 36: /* Elemental Shield */
                        dam = apply_power_dur(20, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d20", dam);
                                return;
                        }
                        (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + dam);
                        (void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + dam);
                        (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + dam);
                        (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + dam);
                        (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + dam);
                        break;
                case 37: /* Mirror Guard */
                        dam = apply_power_dice(10, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        set_tim_reflect(p_ptr->tim_reflect + dam + randint(10));
                        break;
                case 38: /* Sunfire */
                        rad = apply_power_dice(3, to_s, 1);
                        dam = apply_power_dam(200 + plev, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball_beam(GF_LITE, dir, dam, rad);
                        break;
                case 39: /* Force of the Elements */
                {
                        int i;

                        dam = apply_power_dice(8, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam 8*4*14d%ld", dam);
                                return;
                        }
                        for (i = 1; i < 10; i++)
                        {
                                if (i-5)
                                {
                                        if(magik(50)) fire_bolt(GF_FIRE, i, damroll(15, dam));
                                        if(magik(50)) fire_bolt(GF_COLD, i, damroll(14, dam));
                                        if(magik(50)) fire_bolt(GF_ELEC, i, damroll(12, dam));
                                        if(magik(50)) fire_bolt(GF_ACID, i, damroll(18, dam));
                                }
                        }
                        break;
                }

                case 40: /* Earthquake */
                        rad = apply_power_dice(5, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " rad %ld", rad);
                                return;
                        }
                        earthquake(py, px, rad);
                        break;
                case 41: /* Polymorph */
                        if (info_spell) return;
			if (!get_aim_dir(&dir)) return;
			(void)poly_monster(dir);
                        break;
                case 42: /* Wall of Stone */
                        if (info_spell) return;
                        if (!get_aim_dir(&dir)) return;
                        project_hook(GF_STONE_WALL, dir, 1, PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID);
                        break;
                case 43: /* Warp Space */
                        rad = apply_power_dice(4, to_s, 1);
                        dam = apply_power_dam(60, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        fire_ball(GF_GRAVITY, 0, dam, rad);
                        break;
                case 44: /* Chaos Blast */
                        rad = apply_power_dice(5, to_s, 1);
                        dam = apply_power_dam(100 + plev, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        fire_ball(GF_CHAOS, 0, dam, rad);
                        break;
                case 45: /* Lava Flow */
                        dam = apply_power_dur(5 + (plev / 7), to_s, 5);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        project_hook(GF_LAVA_FLOW, dir, dam, PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID);
                        break;
                case 46: /* Pyrrhic Blast */
                        rad = apply_power_dice(3, to_s, 1);
                        dam = apply_power_dam(600, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        fire_ball(GF_MANA, 0, dam, rad);
                        take_hit(200, "the heat of a Pyrrhic Blast");
                        break;
                case 47: /* Word of Destruction */
                        if (info_spell) return;
                        destroy_area(py, px, 15, TRUE);
                        break;

                case 48: /* Radiate Fear */
                        rad = apply_power_dice(10, to_s, 1);
                        dam = apply_power_dam(20 + plev, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                        fire_ball(GF_FEAR, 0, dam, rad);
                        break;
                case 49: /* Probing */
                        if (info_spell) return;
                        probing();
                        break;
                case 50: /* Forcefull Graze */
                        dam = apply_power_dur(50, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_bolt(GF_STUN, dir, dam);
                        break;
                case 51: /* Recharging II */
                        if (info_spell) return;
                        recharge(40 + (level + p_ptr->to_s));
                        break;
                case 52: /* Transmutation */
                        if (info_spell) return;
                        alchemy();
                        break;
                case 53: /* Self Scan */
                        if (info_spell) return;
                        self_knowledge(NULL);
                        identify_pack();
                        p_ptr->notice |= PN_COMBINE | PN_REORDER;
                        break;
                case 54: /* Id II */
                        if (info_spell) return;
                        identify_fully();
                        break;
                case 55: /* Clairvoyance */
                        if (info_spell) return;
                        wiz_lite_extra();
                        break;

                case 56: /* Volcano Flow */
                        dam = apply_power_dice(7, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam 2*15d%ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        project_hook(GF_LAVA_FLOW, dir, 9 + (plev / 7) + to_s, PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID);
                        (void)fire_beam(GF_PLASMA, dir, damroll(15, dam));
                        (void)fire_beam(GF_PLASMA, dir, damroll(15, dam));
                        break;
                case 57: /* Plasma Eruption */
                        rad = apply_power_dice(6, to_s, 1);
                        dam = apply_power_dam(400, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        fire_ball(GF_PLASMA, 0, dam, rad);
                        break;
                case 58: /* Annihilate */
                        dam = apply_power_dam(500, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_bolt(GF_MISSILE, dir, dam);
                        break;
                case 59: /* Olbivion Blast */
                        if (info_spell) return;
                        mass_genocide(TRUE);
                        break;
                case 60: /* Mana Spin */
                {
                        int i;

                        dam = apply_power_dice(10, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam 8*3*10d%ld", dam);
                                return;
                        }
                        for (i = 1; i < 10; i++)
                        {
                                if (i-5)
                                {
                                        fire_bolt(GF_MANA, i, damroll(10, dam));
                                        fire_bolt(GF_MANA, i, damroll(10, dam));
                                        fire_bolt(GF_MANA, i, damroll(10, dam));
                                }
                        }
                        break;
                }
                case 61: /* Tidal Wave */
                        rad = apply_power_dice(3, to_s, 1);
                        dam = apply_power_dam(450, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_WATER, dir, dam, rad);
                        break;
                case 62: /* Anarchy Strike */
                {
                        int i, j;

                        rad = apply_power_dice(3, to_s, 1);
                        dam = apply_power_dam(250, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam 5*%ld", dam);
                                return;
                        }
                        for(j = 0; j < 5; j++)
                        {
                                i = randint(10);
                                while(i == 5) i = randint(10);
                                (void)fire_ball(GF_CHAOS, i, dam, rad);
                        }
                        break;
                }
                case 63: /* Mana Strike */
                        dam = apply_power_dam(1200, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_MANA, dir, dam, 0);
                        break;
           default:
                   msg_format("You cast an unknown Magery spell: %d.", spell);
		   msg_print(NULL);
	   }
}

/* Such an usefull hack :) */
bool check_ring(int art_type)
{
        int i;

        i = INVEN_RING;
        /* Scan the list of rings until we reach the end */
        while (p_ptr->body_parts[i - INVEN_WIELD] == INVEN_RING)
        {
                /* Found the ring we were looking for ? GREAT ! */
                if (inventory[i].k_idx && (inventory[i].name1 == art_type)) return TRUE;

                /* Next item */
                i++;
        }

        /* Found nothing ? blah return FALSE then */
        return FALSE;
}

void cast_shadow_spell(int spell, byte level)
{
        int     dir, beam;
        int     plev = p_ptr->lev;
        int     to_s = level + p_ptr->to_s;
        long    dam = 0, rad;

        if (cp_ptr->flags1 & CF1_BEAM) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
                case 0: /* Dark Hand */
                        dam = apply_power_dice(4, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd%ld", 3 + ((plev - 1) / 5), dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_DARK, dir,
                               damroll(3 + ((plev - 1) / 5), dam));
                        break;
                case 1: /* Sense unseen */
                        if (info_spell) return;
                        detect_monsters_invis();
                        break;
                case 2: /* dark light */
                        if (info_spell) return;
                        lite_room(py, px);
                        break;
                case 3: /* armor of void */
                        dam = apply_power_dur(10, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d5", dam);
                                return;
                        }
                        set_shield(p_ptr->shield + dam + randint(5), 10 + to_s, 0);
                        break;
                case 4: /* fear of the shadows */
                        dam = apply_power_dam(plev, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fear_monster(dir, dam);
                        break;
                case 5: /* sense shadows */
                        if (info_spell) return;
                        detect_objects_normal();
                        break;
                case 6: /* shadow spikes */
                {
                        int i;

                        dam = apply_power_dam(10 + plev, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam 8*%ld", dam);
                                return;
                        }
                        for (i = 1; i < 10; i++)
                        {
                                if (i - 5) fire_beam(GF_DARK, i, dam);
                        }
                        break;
                }
                case 7: /* shadow hiding */
                        dam = apply_power_dur(5, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        set_invis(p_ptr->tim_invis + randint(10) + dam, 30);
                        break;

                case 8: /* Illusory Ball */
                        rad = apply_power_dam(2 + (plev / 20), to_s, 1);
                        dam = apply_power_dam(13, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_OLD_CONF, dir, dam, rad);
                        break;
                case 9: /* Darkness Storm */
                        rad = apply_power_dam(3 + (plev / 20), to_s, 1);
                        dam = apply_power_dam(20, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        fire_ball(GF_DARK, 0, dam, rad);
                        break;
                case 10: /* Energies Channeling */
                        if (info_spell) return;
                        map_area();
                        break;
                case 11: /* Immaterial Beings */
                        if (info_spell) return;
                        summon_specific_friendly(py, px, plev + to_s, SUMMON_GHOST, TRUE);
                        break;
                case 12: /* Shadow Enlightment */
                        if (info_spell) return;
                        ident_spell();
                        break;
                case 13: /* Disolve Matter */
                        rad = apply_power_dam(0, to_s, 1);
                        dam = apply_power_dam(50 + plev, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_FORCE, dir, dam, rad);
                        break;
                case 14: /* Invoke the Night */
                        if (info_spell) return;
                        if (!get_aim_dir(&dir)) return;
                        sleep_monster(dir);
                        break;
                case 15: /* Resist L&D */
                        dam = apply_power_dur(10, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        set_oppose_ld(p_ptr->oppose_ld + dam + randint(10));
                        break;

                case 16: /* Teleportation */
                        dam = apply_power_dam(200, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " range %ld", dam);
                                return;
                        }
                        teleport_player(dam);
                        break;
                case 17: /* Absorb Light */
                {
                        int y, x, light = 0;
                        cave_type *c_ptr;

                        if (info_spell) return;

                        for(y = py - 6; y <= py + 6; y++)
                        {
                                for(x = px - 6; x <= px + 6; x++)
                                {
                                        if(!in_bounds(y, x)) continue;

                                        c_ptr = &cave[y][x];

                                        if (distance(y, x, py, px) > 6) continue;

                                        if (c_ptr->info & CAVE_GLOW)
                                        {
                                                light++;

                                                /* No longer in the array */
                                                c_ptr->info &= ~(CAVE_TEMP);

                                                /* Darken the grid */
                                                c_ptr->info &= ~(CAVE_GLOW);

                                                /* Hack -- Forget "boring" grids */
                                                if ((f_info[c_ptr->feat].flags1 & FF1_FLOOR) && !(f_info[c_ptr->feat].flags1 & FF1_REMEMBER))
                                                {
                                                        /* Forget the grid */
                                                        c_ptr->info &= ~(CAVE_MARK);

                                                        /* Notice */
                                                        note_spot(y, x);
                                                }

                                                /* Process affected monsters */
                                                if (c_ptr->m_idx)
                                                {
                                                        /* Update the monster */
                                                        update_mon(c_ptr->m_idx, FALSE);
                                                }

                                                /* Redraw */
                                                lite_spot(y, x);
                                        }
                                }
                        }
                        msg_print("The light around you is absorbed... and transformed into pure mana!");
                        p_ptr->csp += light * (5 + to_s);
                        if (p_ptr->csp > p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);
			}
                        msg_print("Your feel your head clear.");
                        break;
                }
                case 18: /* Shadow Regeneration */
                        dam = apply_power_dice(8, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " heal %dd%ld", 5 + (plev / 5), dam);
                                return;
                        }
                        hp_player(damroll(5 + (plev / 5), dam));
                        break;
                case 19: /* Steal Shadow */
                        dam = apply_power_dam(60 + plev, to_s, 5);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        dispel_monsters(dam);
                        take_hit(60, "the stealing of your shadow");
                        break;
                case 20: /* Storm in the Shadow */
                        dam = apply_power_dice(5, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ldd20", dam);
                                return;
                        }
                        project_meteor(3, GF_DARK, damroll(dam, 20), PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_THRU);
                        break;
                case 21: /* Shadow of Life */
                {
                        if (info_spell) return;
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_RAISE, dir, 1, 0);
                        break;
                }
                case 22: /* Shadow of Dragon */
                        if (info_spell) return;
                        summon_specific_friendly(py, px, plev + to_s, SUMMON_DRAGON, TRUE);
                        break;
                case 23: /* Banish Shadows */
                        dam = apply_power_dice(180, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        dispel_monsters(dam);
                        break;

                case 24: /* Feelings of Darkness */
                        if (info_spell) return;
                        probing();
                        break;
                case 25: /* Dark Bolt */
                        dam = apply_power_dur(230, to_s, 15);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_DARK, dir, dam);
                        break;
                case 26: /* Shadow orb */
                        rad = apply_power_dice(2, to_s, 1);
                        dam = apply_power_dam(200 + plev, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_DARK, dir, dam, rad);
                        break;
                case 27: /* Shadow of Undead */
                        if (info_spell) return;
                        summon_specific_friendly(py, px, plev + to_s, SUMMON_UNDEAD, TRUE);
                        break;
                case 28: /* Shadow Rain */
                {
                        int y, x, i;

                        rad = apply_power_dam(500, to_s, 1);
                        dam = apply_power_dam(50, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld*%ld", rad, dam);
                                return;
                        }
                        for(i = 0; i < rad; i++)
                        {
                                y = randint(cur_hgt) - 1;
                                x = randint(cur_wid) - 1;

                                project(0, 1, y, x, dam, GF_DARK,
                                     PROJECT_STOP | PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_JUMP);
                        }
                        break;
                }
                case 29: /* Mirror of Shadow */
                        dam = apply_power_dur(5, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        set_tim_reflect(p_ptr->tim_reflect + dam + randint(10));
                        break;
                case 30: /* Comet from the Void */
                        rad = apply_power_dice(2, to_s, 1);
                        dam = apply_power_dur(400, to_s, 15);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_FORCE, dir, dam, rad);
                        break;
                case 31: /* Call the Void */
                        if (info_spell) return;
                        call_the_();
                        break;

                case 32: /* Examin Shadow */
                {
                        monster_type *m_ptr;
                        monster_race *r_ptr;
                        int m;

                        if (info_spell) return;

                        msg_print("You recall legends of the fair and foul creatures of the world...");

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
                case 33: /* *Shadow Enlightment* */
                        if (info_spell) return;
                        identify_fully();
                        break;
                case 34: /* Remove Curse */
                        if (info_spell) return;
                        remove_curse();
                        break;
                case 35: /* *Remove Curse* */
                        if (info_spell) return;
                        remove_all_curse();
                        break;
                case 36: /* Shadow Form */
                        dam = apply_power_dur(5, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d5", dam);
                                return;
                        }
                        set_shadow(p_ptr->tim_wraith + randint(5) + dam);
                        break;

                case 37: /* Shadow Portal */
                        dam = apply_power_dam(10, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " range %ld", dam);
                                return;
                        }
                        teleport_player(dam);
                        break;
                case 38: /* Warping Rift */
                        rad = apply_power_dice(4, to_s, 1);
                        dam = apply_power_dam(200, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        fire_ball(GF_GRAVITY, 0, dam, rad);
                        break;
                case 39: /* Void Jumping */
                        if (info_spell) return;
                        recall_player();
                        break;
                case 40: /* Shadow Distortion */
                        rad = apply_power_dice(1, to_s, 1);
                        dam = apply_power_dam(100, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_GRAVITY, dir, dam, rad);
                        break;
                case 41: /* Between Jump */
                {
                        int ij, ii;

                        if (info_spell) return;

                        if (dungeon_flags1 & LF1_NO_TELEPORT) {msg_print("Not on special levels!");break;}

                        msg_print("You open a between gate. Choose a destination.");
                        if (!tgt_pt(&ii,&ij)) break;
                        p_ptr->energy -= 60 - plev;
                        if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
                        (distance(ij,ii,py,px) > plev + 2) ||
                        (!rand_int(plev * plev / 2)))
                        {
                                msg_print("You fail to exit the between correctly!");
                                p_ptr->energy -= 100;
                                get_pos_player(10,&ij,&ii);
                        }
                        cave_set_feat(py,px,FEAT_BETWEEN);
                        cave_set_feat(ij,ii,FEAT_BETWEEN);
                        cave[py][px].special = ii + (ij << 8);
                        cave[ij][ii].special = px + (py << 8);
                        break;
                }
                case 42: /* Between Maze */
                        if (info_spell) return;
                        fire_ball(GF_BETWEEN_GATE, 0, 1, 2 + to_s);
                        break;

                case 43: /* Dispel Living */
                        dam = apply_power_dam(100, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        dispel_living(dam);
                        break;
                case 44: /* Conjure Dragons */
                {
                        int i;
                        if (info_spell) return;

                        for(i = 0; i < 5 + to_s; i++)
                                summon_specific_friendly(py, px, plev + to_s, SUMMON_DRAGON, TRUE);
                        break;
                }
                case 45: /* Dispel Undead */
                        dam = apply_power_dam(120 + plev, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        dispel_undead(dam);
                        break;
                case 46: /* Conjure Undeads */
                {
                        int i;
                        if (info_spell) return;

                        for(i = 0; i < 5 + to_s; i++)
                                summon_specific_friendly(py, px, plev + to_s, SUMMON_UNDEAD, TRUE);
                        break;
                }
                case 47: /* Dispel Demon */
                        dam = apply_power_dam(180 + plev, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        dispel_demons(dam);
                        break;
                case 48: /* Conjure Demons */
                {
                        int i;

                        if (info_spell) return;

                        for(i = 0; i < 5 + to_s; i++)
                                summon_specific_friendly(py, px, plev + to_s, SUMMON_DEMON, TRUE);
                        break;
                }

                case 49: /* Recharge I */
                        rad = apply_power_dam(20, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", rad);
                                return;
                        }
                        recharge(rad);
                        break;
                case 50: /* Sphere of Void */
                        rad = apply_power_dice(3, to_s, 1);
                        dam = apply_power_dur(320, to_s, 15);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        fire_ball(GF_DISINTEGRATE, 0, dam, rad);
                        break;
                case 51: /* Shadow Alteration */
                        if (info_spell) return;
                        alter_reality();
                        break;
                case 52: /* Recharge II */
                        dam = apply_power_dam(40, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                        recharge(dam);
                        break;
                case 53: /* Concentrate Light */
                {
                        int y, x, light = 0;
                        cave_type *c_ptr;

                        if (info_spell) return;

                        for(y = py - 6; y <= py + 6; y++)
                        {
                                for(x = px - 6; x <= px + 6; x++)
                                {
                                        if(!in_bounds(y, x)) continue;

                                        c_ptr = &cave[y][x];

                                        if (distance(y, x, py, px) > 6) continue;

                                        if (c_ptr->info & CAVE_GLOW)
                                        {
                                                light++;

                                                /* No longer in the array */
                                                c_ptr->info &= ~(CAVE_TEMP);

                                                /* Darken the grid */
                                                c_ptr->info &= ~(CAVE_GLOW);

                                                /* Hack -- Forget "boring" grids */
                                                if ((f_info[c_ptr->feat].flags1 & FF1_FLOOR) && !(f_info[c_ptr->feat].flags1 & FF1_REMEMBER))
                                                {
                                                        /* Forget the grid */
                                                        c_ptr->info &= ~(CAVE_MARK);

                                                        /* Notice */
                                                        note_spot(y, x);
                                                }

                                                /* Process affected monsters */
                                                if (c_ptr->m_idx)
                                                {
                                                        /* Update the monster */
                                                        update_mon(c_ptr->m_idx, FALSE);
                                                }

                                                /* Redraw */
                                                lite_spot(y, x);
                                        }
                                }
                        }
                        if (!get_aim_dir(&dir)) break;
                        msg_print("The light around you is absorbed... and released in a powerful bolt!");
                        fire_bolt(GF_LITE, dir, damroll(light, p_ptr->lev / 3));
                        break;
                }
                        break;
                case 54: /* Shield of Darkness */
                        rad = apply_power_dice(50, to_s, 3);
                        dam = apply_power_dur(30, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d20", dam);
                                return;
                        }
                        (void)set_shield(p_ptr->shield + randint(20) + dam, rad, 0);
                        break;
                case 55: /* Tunnel of Shadow */
                {
                        int min = ((dun_level - 5 - to_s) < d_info[dungeon_type].mindepth)?d_info[dungeon_type].mindepth:(dun_level - 5 - to_s);
                        int max = ((dun_level + 5 + to_s) < d_info[dungeon_type].maxdepth)?d_info[dungeon_type].maxdepth:(dun_level + 5 + to_s);
                        int dest = dun_level;

                        if (info_spell) return;

                        /* Ask for level */
                        {
                                char    ppp[80];

                                char    tmp_val[160];

                                /* Prompt */
                                sprintf(ppp, "Jump to level (%d-%d): ", min, max);

                                /* Default */
                                sprintf(tmp_val, "%d", dun_level);

                                /* Ask for a level */
                                if (!get_string(ppp, tmp_val, 10)) return;

                                /* Extract request */
                                dest = atoi(tmp_val);
                        }

                        /* Paranoia */
                        if (dest < 0) dest = 0;

                        /* Paranoia */
                        if (dest > d_info[dungeon_type].maxdepth) dest = d_info[dungeon_type].maxdepth;

                        /* Accept request */
                        msg_format("A tunnel of shadows is open to the level %d.", dest);

                        /* Change level */
                        dun_level = dest;

                        p_ptr->inside_arena = 0;
                        leaving_quest = p_ptr->inside_quest;

                        p_ptr->inside_quest = 0;

                        /* Leaving */
                        p_ptr->leaving = TRUE;
                        break;
                }
                case 56: /* Genocide */
                        if (info_spell) return;
                        genocide(TRUE);
                        break;
                case 57: /* Enslave Undead */
                        rad = apply_power_dice(4, to_s, 1);
                        dam = apply_power_dam(150, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                        fire_ball(GF_CONTROL_UNDEAD, 0, dam, rad);
                        break;
                case 58: /* Create Minor Ring */
        {
                int item, die = randint(100);
                object_type             *o_ptr;
                cptr q, s;

                        if (info_spell) return;

                /* Restrict choices to amulets */
                item_tester_tval = TV_RING;

                /* Get an item */
                q = "Use which ring? ";
                s = "You have no ring to use.";
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

                if(o_ptr->sval != SV_RING_NOTHING)
                {
                        msg_print("You must use a Ring of Nothing.");
                        break;
                }

                if(die <= 2)
                {
                        o_ptr->sval = SV_RING_TELEPORTATION;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 12)
                {
                        o_ptr->sval = SV_RING_FEATHER_FALL;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 22)
                {
                        o_ptr->sval = SV_RING_PROTECTION;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 32)
                {
                        o_ptr->sval = SV_RING_FREE_ACTION;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 42)
                {
                        o_ptr->sval = SV_RING_DAMAGE;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 52)
                {
                        o_ptr->sval = SV_RING_SLAYING;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 62)
                {
                        o_ptr->sval = SV_RING_SPEED;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 67)
                {
                        o_ptr->sval = SV_RING_RES_NEXUS;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 77)
                {
                        o_ptr->sval = SV_RING_RES_SHARDS;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 78)
                {
                        o_ptr->sval = SV_RING_ATTACKS;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 88)
                {
                        o_ptr->sval = SV_RING_INVIS;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 98)
                {
                        o_ptr->sval = SV_RING_FLYING;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 100)
                {
                        o_ptr->sval = SV_RING_PRECONITION;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                o_ptr->k_idx = lookup_kind(o_ptr->tval, o_ptr->sval);

                p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                p_ptr->window |= (PW_INVEN);
                break;
        }
                case 59: /* Control the Three */
                        if (info_spell) return;
                        if (check_ring(ART_NARYA) && check_ring(ART_NENYA) && check_ring(ART_VILYA))
                        {
                                msg_print("The power of the Elven Rings is released! You are now protected from the passing of the Time.");
                                set_tim_res_time(p_ptr->tim_res_time + 20 + randint(30) + ((level + p_ptr->to_s) * 2));
                                restore_level();
                                if (p_ptr->black_breath)
                                {
                                        msg_print("The hold of the Black Breath on you is broken!");
                                }
                                p_ptr->black_breath = FALSE;

                                remove_dg_curse();
                        }
                        else
                                msg_print("You must wear the Three Rings of Power(Narya, Nenya and Vilya) to use this spell.");
                        break;
                case 60: /* Protection from Undeads */
                        dam = apply_power_dur(20, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d30", dam);
                                return;
                        }
                        if(p_ptr->black_breath)
                        {
                                msg_print("You are cured from the Black Breath!");
                                p_ptr->black_breath = FALSE;
                        }
                        set_protundead(p_ptr->protundead + dam + randint(30));
                        break;
                case 61: /* Mass Genocide */
                        if (info_spell) return;
                        mass_genocide(TRUE);
                        break;
                case 62: /* Hellfire */
                        rad = apply_power_dice(3, to_s, 1);
                        dam = apply_power_dam(666, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
			if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_HELL_FIRE, dir,
                                dam, rad);
                        take_hit(100+randint(50), "the strain of casting Hellfire");
                        break;
                case 63: /* Control The Ring */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " a lot of dam");
                                return;
                        }
                        if(check_ring(ART_POWER))
                        {
                                rad = apply_power_dice(5, to_s, 1);
                                dam = apply_power_dam(1000, to_s, 1);
                                fire_ball(GF_MANA, 0, dam, rad);

                                rad = apply_power_dice(2, to_s, 1);
                                dam = apply_power_dam(750, to_s, 1);
                                if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_DARK, dir, dam, rad);

                                dam = apply_power_dam(500, to_s, 1);
                                if (!get_aim_dir(&dir)) return;
                                fire_bolt(GF_FORCE, dir, dam);
                        }
                        else
                                msg_print("You must wear The One Ring, The Ring of Power to use this spell.");
                        break;

                default:
                        msg_format("You cast an unknown Shadow spell: %d.", spell);
                        msg_print(NULL);
                        break;
        }
}


void cast_crusade_spell(int spell, byte level)
{
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
        int     to_s = level + p_ptr->to_s;
        long    dam, rad;

        if (cp_ptr->flags1 & CF1_BEAM) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
                case 0: /* Boldness */
                        if (info_spell) return;
                        set_afraid(0);
                break;
                case 1: /* Bless */
                        dam = apply_power_dur(12, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d12", dam);
                                return;
                        }
                       (void)set_blessed(p_ptr->blessed + randint(12) + dam);
                break;
                case 2: /* Light Area */
                        if (info_spell) return;
                        lite_area(damroll(2, (plev / 2)), (plev / 10) + 1 + to_s);
                break;
                case 3: /* See invis  */
                        dam = apply_power_dur(12, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        set_tim_invis(p_ptr->tim_invis + dam + randint(12));
                break;
                case 4: /* Touch of conf */
                    if (!(p_ptr->confusing))
                    {
                        msg_print("Your hands start glowing.");
                        p_ptr->confusing = TRUE;
                    }
                break;
                case 5: /* Invoke Fear*/
                        dam = apply_power_dam(5 + plev, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        (void)fear_monster(dir, dam);
                break;
                case 6: /* Resist fire */
                        dam = apply_power_dur(10, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        set_oppose_fire(p_ptr->oppose_fire + dam + rand_int(10));
                break;
                case 7: /* Resist cold */
                        dam = apply_power_dur(12, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        set_oppose_cold(p_ptr->oppose_cold + dam + rand_int(10));
                break;

                case 8: /* Spear of light */
                        if (info_spell) return;
                       if (!get_aim_dir(&dir)) return;
                       msg_print("A line of blue shimmering light appears.");
                       lite_line(dir);
                       break;
                break;
                case 9: /* Heroism */
                        dam = apply_power_dam(20, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d20", dam);
                                return;
                        }
                        (void)set_hero(p_ptr->hero + randint(20) + dam);
                break;
                case 10: /* Holy Chant */
                        dam = apply_power_dam(12, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d24", dam);
                                return;
                        }
                       set_blessed(p_ptr->blessed + randint(24) + dam);
                break;
                case 11: /* Sense foes */
                        if (info_spell) return;
                        detect_monsters_normal();
                break;
                case 12: /* Jam Doors */
                        if (info_spell) return;
                        if (!(get_aim_dir(&dir))) break;
                        (void) wizard_lock(dir);
                break;
                case 13: /* Cure wounds & poison */
                        if (info_spell) return;
                        set_poisoned(0);
                        set_cut(0);
                break;
                case 14: /* Wolvish hunger */
                        dam = apply_power_dam(10, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if(p_ptr->mimic_form != MIMIC_WOLF)
                                set_mimic(dam + randint(8), MIMIC_WOLF);
                        else
                                set_mimic(p_ptr->tim_mimic + dam + randint(8), MIMIC_WOLF);
                        break;
                break;
                case 15: /* Berserker rage */
                        dam = apply_power_dur(250, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d35", dam);
                                return;
                        }
                        (void)set_shero(p_ptr->shero + randint(35) + dam);
                        (void)set_blessed(p_ptr->blessed + randint(35) + dam);
                break;

                case 16: /* vision */
                        if (info_spell) return;
                        map_area();
                break;
                case 17: /* Fiery aura */
                        dam = apply_power_dam(15, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        set_tim_fire_aura(p_ptr->tim_fire_aura + dam + randint(10));
                break;
                case 18: /* Holy Shield */
                        dam = apply_power_dam(30, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d20", dam);
                                return;
                        }
                       (void)set_shield(p_ptr->shield + randint(20) + dam, 50 + to_s, 0);
                break;
                case 19: /* Mend wounds */
                        dam = apply_power_dur(30, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " heal %ld", dam);
                                return;
                        }
                        hp_player(dam);
                        set_cut(0);
                break;
                case 20: /* Haste Self */
                        dam = apply_power_dur(plev , to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d%d", dam, 20 + plev);
                                return;
                        }
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev)) + dam);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5));
			}
                break;
                case 21: /* Thunder's Wrath */
                        dam = apply_power_dam(30, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam 8*%ld+10d5", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_SOUND, 0, dam, 2 + (plev / 25));
                        fire_ball(GF_FORCE, 0, dam, 2 + (plev / 25));
                        fire_ball(GF_CONFUSION, dir, damroll(10, 5), 0);
                break;
                case 22: /* Counterstrike */
                        dam = apply_power_dam(10, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        set_shield(p_ptr->shield + dam + randint(10), 10, SHIELD_COUNTER);
                break;
                case 23: /* Whirlwind attack */
                         {
                         int y = 0, x = 0;
                         cave_type       *c_ptr;
                         monster_type    *m_ptr;

                        if (info_spell) return;

                         for (dir = 0; dir <= 9; dir++)
                         {
                             y = py + ddy[dir];
                             x = px + ddx[dir];
                             c_ptr = &cave[y][x];

                             /* Get the monster */
                             m_ptr = &m_list[c_ptr->m_idx];

                             /* Hack -- attack monsters */
                             if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x)))
                           py_attack(y, x, -1);
                         }
                         }
                break;

                case 24: /* call thunder */
                        rad = apply_power_dice(2 + (plev / 10), to_s, 1);
                        dam = apply_power_dam(45 + plev, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                       msg_print("BOOM! Shake the room!");
                       project(0, rad, py, px,
                           dam, GF_SOUND, PROJECT_KILL|PROJECT_ITEM);
                break;
                case 25: /* mass Charm */
                        dam = apply_power_dam(100, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                        charm_monsters(dam);
                break;
                case 26: /* True Strike */
                        dam = apply_power_dur(25, to_s, 5);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d25", dam);
                                return;
                        }
                        set_strike(p_ptr->strike + randint(25) + dam);
                break;
                case 27: /* Deadly Blow */
                        dam = apply_power_dice(1 + (plev / 15), to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " %ld blows", dam);
                                return;
                        }
                        set_tim_deadly(dam);
                break;
                case 28: /* healing */
                        dam = apply_power_dam(400, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " heal %ld", dam);
                                return;
                        }
                        (void)hp_player(dam);
			(void)set_cut(0);
                break;
                case 29: /* dragon's wrath */
                        dam = apply_power_dur(15, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        if(p_ptr->mimic_form != MIMIC_DRAGON)
                                set_mimic(dam + randint(10), MIMIC_DRAGON);
                        else
                                set_mimic(p_ptr->tim_mimic + dam + randint(10), MIMIC_DRAGON);
                        break;
                break;
                case 30: /* Summon angel */
                        if (info_spell) return;
                        msg_format("You magically summon an angel.");
                        summon_specific_friendly(py, px, dun_level + (level + p_ptr->to_s), SUMMON_ANGEL, TRUE);
                break;
                case 31: /* wave of power */
                        if (info_spell) return;
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_ATTACK, dir, 1);
                break;
	default:
                msg_format("You cast an unknown Crusade spell: %d.", spell);
		msg_print(NULL);
    }
}


void cast_sigaldry_spell(int spell, byte level)
{
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
        int     to_s = level + p_ptr->to_s;
        long    dam, rad;

        if (cp_ptr->flags1 & CF1_BEAM) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
        case 0: /* Detect Monster */
                        if (info_spell) return;
                (void)detect_monsters_normal();
                break;
        case 1: /* Wizard Lock */
                        if (info_spell) return;
            if (!(get_aim_dir(&dir))) break;
            (void) wizard_lock(dir);
        break;
        case 2: /* Illuminate */
                        if (info_spell) return;
                (void)lite_area(damroll(2 + to_s, (plev / 2)), (plev / 10) + 1 + to_s);
        break;
        case 3: /* Locate Doors/Stairs */
                        if (info_spell) return;
			(void)detect_doors();
			(void)detect_stairs();
        break;
        case 4: /* Locate Traps */
                        if (info_spell) return;
			(void)detect_traps();
        break;
        case 5: /* Disruption I */
        {
                int typ = GF_MISSILE;

                        dam = apply_power_dice(5, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ldd4+%d", dam, plev / 5);
                                return;
                        }

                if (!get_aim_dir(&dir)) return;
                /* A radius-0 ball may (1) be aimed at objects etc.,
                 * and will affect them; (2) may be aimed at ANY
                 * visible monster, unlike a 'bolt' which must travel
                 * to the monster. */

                if(randint(100) < plev)
                {
                        int die = rand_int(100);
                        if (die < 40) {
                                typ = GF_STUN_DAM;
                        } else if (die < 80) {
                                typ = GF_CONF_DAM;
                        } else {
                                typ = GF_STUN_CONF;
                        }
                }

                fire_ball(typ, dir,
                          damroll(dam, 4) + (plev / 5), 0);
        }
        break;
        case 6: /* Blink */
                        dam = apply_power_dice(10, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " range %ld", dam);
                                return;
                        }
                teleport_player(dam);
        break;
        case 7: /* Detect Treasures */
                        if (info_spell) return;
                (void)detect_treasure();
        break;
        case 8: /* Detect Objects */
                        if (info_spell) return;
			(void)detect_objects_normal();
			(void)detect_objects_gold();
        break;
        case 9: /* Warding I */
                        dam = apply_power_dur(5 + plev, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d5", dam);
                                return;
                        }
                set_shield(p_ptr->shield + rand_int(5) + dam, 50 + to_s, 0);
        break;
        case 10: /* Trap/Door Destruction */
                        if (info_spell) return;
			(void)destroy_doors_touch();
        break;
        case 11: /* Sleep I */
                        if (info_spell) return;
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir);
        break;
        case 12: /* Preservation */
        {
                int item;
                object_type *o_ptr;
                cptr q, s;

                        if (info_spell) return;

                /* Restrict choices to corpses */
                item_tester_tval = TV_CORPSE;

                /* Get an item */
                q = "Preserve which corspe? ";
                s = "You have no corpse to preserve.";
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

                o_ptr->pval = 65535;
                msg_print("The corpse seem to be fresher.");
                break;
        }
        break;
        case 13: /* Confuse */
                        dam = apply_power_dam(plev * 3 / 2, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                if (!get_aim_dir(&dir)) return;
                (void)confuse_monster(dir, dam);
        break;
        case 14: /* Magic Aura */
                        dam = apply_power_dur(10, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d5", dam);
                                return;
                        }
                (void)set_holy(p_ptr->holy + dam + randint(5));
        break;
        case 15: /* Appraise Item */
                        if (info_spell) return;
                psychometry();
        break;

        case 16: /* Resist Fire */
                        dam = apply_power_dam(10, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d20", dam);
                                return;
                        }
                set_oppose_fire(p_ptr->oppose_fire + dam + rand_int(20));
        break;
        case 17: /* Resist Cold */
                        dam = apply_power_dam(10, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d20", dam);
                                return;
                        }
                set_oppose_cold(p_ptr->oppose_cold + dam + rand_int(20));
        break;
        case 18: /* Finger of Pain */
                        dam = apply_power_dice(8, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd%ld", 5 + (plev / 5), dam);
                                return;
                        }
                /* It doesn't need to travel to the monster, it just Jump at it */
                if (!get_aim_dir(&dir)) return;
                project_hook(GF_MISSILE, dir, damroll(5 + (plev / 5), dam),
                             PROJECT_STOP | PROJECT_KILL | PROJECT_JUMP);
        break;
        case 19: /* Recharge I */
                        dam = apply_power_dam(20, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                (void)recharge(dam);
        break;
        case 20: /* Magic Map */
                        if (info_spell) return;
                map_area();
        break;
        case 21: /* Sleep II */
                        if (info_spell) return;
                       (void)sleep_monsters_touch();
        break;
        case 22: /* Teleport Self */
                        dam = apply_power_dam(200, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " range %ld", dam);
                                return;
                        }
                teleport_player(dam);
        break;
        case 23: /* Identify */
                        if (info_spell) return;
			(void)ident_spell();
        break;

        case 24: /* True Strike */
                        dam = apply_power_dam(25, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d25", dam);
                                return;
                        }
                set_strike(p_ptr->strike + randint(25) + dam);
        break;
        case 25: /* Between Gate */
       {
             int ii, ij;

                        if (info_spell) return;

             if(dungeon_flags1 & LF1_NO_TELEPORT){msg_print("Not on special levels!");break;}

             msg_print("You open a between gate. Choose a destination.");
             if (!tgt_pt(&ii,&ij)) return;
             p_ptr->energy -= 60 - plev;
             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
             (distance(ij,ii,py,px) > plev + 2 + to_s) ||
             (!rand_int(plev * plev / 2)))
             {
                 msg_print("You fail to exit the between correctly!");
                 p_ptr->energy -= 100;
                 get_pos_player(10 + to_s,&ij,&ii);
             }
             cave_set_feat(py,px,FEAT_BETWEEN);
             cave_set_feat(ij,ii,FEAT_BETWEEN);
             cave[py][px].special = ii + (ij << 8);
             cave[ij][ii].special = px + (py << 8);

             break;
       }
        case 26: /* Disruption II */
        {
                int typ = GF_MISSILE;

                        dam = apply_power_dice(10, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ldd10+%d", dam, plev / 5);
                                return;
                        }

                if (!get_aim_dir(&dir)) return;
                /* A radius-0 ball may (1) be aimed at objects etc.,
                 * and will affect them; (2) may be aimed at ANY
                 * visible monster, unlike a 'bolt' which must travel
                 * to the monster. */

                if(randint(60) < plev)
                {
                        int die = rand_int(100);
                        if (die < 40) {
                                typ = GF_STUN_DAM;
                        } else if (die < 80) {
                                typ = GF_CONF_DAM;
                        } else {
                                typ = GF_STUN_CONF;
                        }
                }

                fire_ball(typ, dir,
                          damroll(dam, 10) + (plev / 5), 0);
        }
        break;
               break;
        case 27: /* Drain Magic I */
				{
					object_type * o_ptr;
					int lev, item;
                                        cptr  q, s;

                        if (info_spell) return;

                                        item_tester_hook = item_tester_hook_scroll_amulet;

					/* Get an item */
					q = "Drain which item? ";
					s = "You have nothing to drain.";
					if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) break;

					if (item >= 0)
					{
						o_ptr = &inventory[item];
					}
					else
					{
						o_ptr = &o_list[0 - item];
					}

					lev = k_info[o_ptr->k_idx].level;

                                        if(randint(100) < 50)
                                        {
                                                if (o_ptr->tval == TV_SCROLL)
                                                {
                                                        if (o_ptr->sval == SV_SCROLL_NOTHING)
                                                        {
                                                                msg_print("You can't absorb energy from that.");
                                                        }
                                                        else
                                                        {
                                                                p_ptr->csp += lev;
                                                                o_ptr->sval = SV_SCROLL_NOTHING;
                                                        }
                                                }
                                                else
                                                {
                                                        if ((o_ptr->sval == SV_AMULET_NOTHING) || (o_ptr->sval == SV_AMULET_NO_MAGIC))
                                                        {
                                                                msg_print("You can't absorb energy from that.");
                                                        }
                                                        else
                                                        {
                                                                p_ptr->csp += 2 * lev;
                                                                o_ptr->sval = SV_AMULET_NOTHING;
                                                        }
                                                }
                                        }
                                        /* Simply destroy the object */
                                        else
                                        {
                                                msg_print("The object gets destroyed.");
                                                if (o_ptr->tval == TV_SCROLL)
                                                {
                                                        o_ptr->sval = SV_SCROLL_NOTHING;
                                                }
                                                else
                                                {
                                                        o_ptr->sval = SV_AMULET_NOTHING;
                                                }
                                        }

                                        o_ptr->k_idx = lookup_kind(o_ptr->tval, o_ptr->sval);
                                        if (item >= 0)
                                        {
                                                inven_item_describe(item);
                                        }
                                        else
                                        {
                                                floor_item_describe(0 - item);
                                        }

					if (p_ptr->csp > p_ptr->msp)
					{
						p_ptr->csp = p_ptr->msp;
					}

					p_ptr->notice |= (PN_COMBINE | PN_REORDER);
					p_ptr->window |= (PW_INVEN);

                break;
        }
        case 28: /* Scribe Scroll */
        {
                int item, realm, spel;
                object_type *o_ptr, *q_ptr;
                cptr q, s;

                        if (info_spell) return;

                /* Restrict choices to scrolls */
                item_tester_tval = TV_SCROLL;

                /* Get an item */
                q = "Use which scroll? ";
                s = "You have no scroll to use.";
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

                if(o_ptr->sval != SV_SCROLL_NOTHING)
                {
                        msg_print("You must use a Scroll of Nothing.");
                        break;
                }

                if(o_ptr->number > 1)
                {
                        msg_print("You can't inscribe more than one scroll.");
                        break;
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
                        q_ptr = &inventory[item];
                }

                /* Get the item (on the floor) */
                else
                {
                        q_ptr = &o_list[0 - item];
                }

                realm = q_ptr->tval - TV_VALARIN_BOOK + 1;

                /* Ask for a spell */
                if (!get_spell(&spel, "scribe", q_ptr->sval, TRUE, q_ptr, TRUE))
                {
                        if (spell == -2)
                                msg_print("You don't know any spells in that book.");
                        return;
                }

                if(realm_info[realm][spel].smana + realm_info[REALM_SIGALDRY][spell].smana > p_ptr->csp)
                {
                        msg_print("You don't have enough mana to do that");
                        return;
                }

                /* Create the scroll */
                o_ptr->sval = SV_SCROLL_SPELL;
                o_ptr->pval = realm;
                o_ptr->pval2 = spel;
                o_ptr->k_idx = lookup_kind(o_ptr->tval, o_ptr->sval);

                /* Use some more mana */
                p_ptr->csp -= realm_info[realm][spel].smana + realm_info[REALM_SIGALDRY][spell].smana;

                /* Identify it */
                object_aware(o_ptr);

                if (item >= 0)
                {
                        inven_item_describe(item);
                }
                else
                {
                        floor_item_describe(0 - item);
                }

                p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                p_ptr->window |= (PW_INVEN);
                break;
        }
        case 29: /* Infuse Amulet */
        {
                int item, die = randint(100);
                object_type             *o_ptr;
                cptr q, s;

                if (info_spell) return;

                /* Restrict choices to amulets */
                item_tester_tval = TV_AMULET;

                /* Get an item */
                q = "Use which amulet? ";
                s = "You have no amulet to use.";
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

                if(o_ptr->sval != SV_AMULET_NOTHING)
                {
                        msg_print("You must use an Amulet of Nothing.");
                        break;
                }

                if(die <= 2)
                {
                        o_ptr->sval = SV_AMULET_WISDOM;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 12)
                {
                        o_ptr->sval = SV_AMULET_CHARISMA;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 22)
                {
                        o_ptr->sval = SV_AMULET_SEARCHING;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 32)
                {
                        o_ptr->sval = SV_AMULET_TELEPORT;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 42)
                {
                        o_ptr->sval = SV_AMULET_SLOW_DIGEST;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 52)
                {
                        o_ptr->sval = SV_AMULET_RESIST_ACID;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 62)
                {
                        o_ptr->sval = SV_AMULET_ADORNMENT;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 67)
                {
                        o_ptr->sval = SV_AMULET_THE_MAGI;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 77)
                {
                        o_ptr->sval = SV_AMULET_DOOM;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 78)
                {
                        o_ptr->sval = SV_AMULET_REFLECTION;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 88)
                {
                        o_ptr->sval = SV_AMULET_NO_MAGIC;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 98)
                {
                        o_ptr->sval = SV_AMULET_NO_TELE;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                else if(die <= 100)
                {
                        o_ptr->sval = SV_AMULET_RESISTANCE;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE);
                }
                o_ptr->k_idx = lookup_kind(o_ptr->tval, o_ptr->sval);

                p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                p_ptr->window |= (PW_INVEN);
                break;
        }
        case 30: /* Glyph of Warding */
                        if (info_spell) return;
                        warding_glyph();
        break;
        case 31: /* Implosion */
                        rad = apply_power_dice(2, to_s, 1);
                        dam = apply_power_dam(200, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                fire_ball(GF_IMPLOSION, 0, dam, rad);
        break;
	default:
                msg_format("You cast an unknown Sigaldry spell: %d.", spell);
		msg_print(NULL);
    }
}

int use_symbiotic_power(int r_idx, bool great, bool only_number, bool no_cost)
{
        int             Power = -1;
        int             num = 0, dir = 0 , i;

        int             powers[96];

	bool            flag, redraw;
        int             ask, plev = p_ptr->lev;

	char            choice;

	char            out_val[160];
        monster_race    *r_ptr = &r_info[r_idx];
        int             rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);
        int             x = px, y = py, k;
        int             rad;

        /* List the powers */
        for (i = 0; i < 32; i++)
        {
                if (r_ptr->flags4 & BIT(i))
                {
                        if (monster_powers[i].great && (!great)) continue;
                        if (!monster_powers[i].power) continue;
                        powers[num++] = i;
                }
        }
        for (i = 0; i < 32; i++)
        {
                if (r_ptr->flags5 & BIT(i))
                {
                        if (monster_powers[i + 32].great && (!great)) continue;
                        if (!monster_powers[i + 32].power) continue;
                        powers[num++] = i + 32;
                }
        }
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
                return 0;
        }

        if (only_number) return num;

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power of your monster? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power of your monster? ",
			I2A(0), '0' + num - 27);
	}

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

				while (ctr < num && ctr < 17)
				{
                                        sprintf(dummy, "%c) %s", I2A(ctr), monster_powers[powers[ctr]].name);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
                                        int mana = monster_powers[powers[ctr]].mana / 10;

                                        if (!mana) mana = 1;

                                        if (mana > p_ptr->msp) mana = p_ptr->msp;

					if (ctr < 26)
					{
                                                if (!no_cost) sprintf(dummy, " %c) %2d %s", I2A(ctr), mana, monster_powers[powers[ctr]].name);
                                                else sprintf(dummy, " %c) %s", I2A(ctr), monster_powers[powers[ctr]].name);
					}
					else
					{
                                                if (!no_cost) sprintf(dummy, " %c) %2d %s", '0' + ctr - 26, mana, monster_powers[powers[ctr]].name);
                                                else sprintf(dummy, " %c) %s", '0' + ctr - 26, monster_powers[powers[ctr]].name);
					}
					prt(dummy, y + ctr - 17, x + 40);
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
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
                        strnfmt(tmp_val, 78, "Use %s? ", monster_powers[Power].name);

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

	if (r_ptr->flags2 & RF2_POWERFUL)
		rad = 1 + (p_ptr->lev/15);
	else
		rad = 1 + (p_ptr->lev/20);

        switch(Power)
        {
                case 0: /* Shriek */
                        aggravate_monsters(-1);
                        break;
                case 1: /* Multiply */
			do_cmd_wiz_named_friendly(p_ptr->body_monster, FALSE);
			break;
                case 2: /* Summon animal */
                        summon_specific_friendly(y, x, rlev, SUMMON_ANIMAL, TRUE);
                        break;
                case 3: /* Rocket */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ROCKET, dir, p_ptr->lev * 12, 1 + (p_ptr->lev/20));
                        break;
                case 4: /* Arrow1 */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(1,6));
                        break;
                case 5: /* Arrow2 */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(3,6));
                        break;
                case 6: /* Arrow3 */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(5,6));
                        break;
                case 7: /* Arrow4 */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(7,6));
                        break;
                case 8: /* Br acid */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ACID, dir, p_ptr->lev * 5, rad);
                        break;
                case 9: /* Br elec */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ELEC, dir, p_ptr->lev * 5, rad);
                        break;
                case 10: /* br fire */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FIRE, dir, p_ptr->lev * 5, rad);
                        break;
                case 11: /* br cold */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_COLD, dir, p_ptr->lev * 5, rad);
                        break;
                case 12: /* br pois */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_POIS, dir, p_ptr->lev * 5, rad);
                        break;
                case 13: /* br neth */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NETHER, dir, p_ptr->lev * 5, rad);
                        break;
                case 14: /* br lite */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_LITE, dir, p_ptr->lev * 8, rad);
                        break;
                case 15: /* br dark */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DARK, dir, p_ptr->lev * 8, rad);
                        break;
                case 16: /* br conf */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CONFUSION, dir, p_ptr->lev * 8, rad);
                        break;
                case 17: /* br soun */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SOUND, dir, p_ptr->lev * 8, rad);
                        break;
                case 18: /* br chao */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CHAOS, dir, p_ptr->lev * 7, rad);
                        break;
                case 19: /* br dise */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DISENCHANT, dir, p_ptr->lev * 7, rad);
                        break;
                case 20: /* br nexu */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NEXUS, dir, p_ptr->lev * 5, rad);
                        break;
                case 21: /* br time */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_TIME, dir, p_ptr->lev * 3, rad);
                        break;
                case 22: /* br iner */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_INERTIA, dir, p_ptr->lev * 4, rad);
                        break;
                case 23: /* br grav */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_GRAVITY, dir, p_ptr->lev * 4, rad);
                        break;
                case 24: /* br shar */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SHARDS, dir, p_ptr->lev * 8, rad);
                        break;
                case 25: /* br plas */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_PLASMA, dir, p_ptr->lev * 3, rad);
                        break;
                case 26: /* br wall */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FORCE, dir, p_ptr->lev * 4, rad);
                        break;
                case 27: /* br mana */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, p_ptr->lev * 5, rad);
                        break;
                case 28: /* ba nuke */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, p_ptr->lev * 8, 1 + (p_ptr->lev/20));
                        break;
                case 29: /* br nuke */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, p_ptr->lev * 8, 1 + (p_ptr->lev/20));
                        break;
                case 30: /* ba chao */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CHAOS, dir, p_ptr->lev * 4, 2);
                        break;
                case 31: /* br disi */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DISINTEGRATE, dir, p_ptr->lev * 5, 1 + (p_ptr->lev/20));
                        break;

                case 32: /* ba acid */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ACID, dir, randint(p_ptr->lev * 6)+20, 2);
                        break;
                case 33: /* ba elec */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ELEC, dir, randint(p_ptr->lev * 3)+20, 2);
                        break;
                case 34: /* ba fire */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FIRE, dir, randint(p_ptr->lev * 7)+20, 2);
                        break;
                case 35: /* ba cold */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_COLD, dir, randint(p_ptr->lev * 3)+20, 2);
                        break;
                case 36: /* ba pois */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_POIS, dir, damroll(12,2), 2);
                        break;
                case 37: /* ba neth */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NETHER, dir, randint(p_ptr->lev * 4)+20, 2);
                        break;
                case 38: /* ba wate */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_WATER, dir, randint(p_ptr->lev * 4)+20, 2);
                        break;
                case 39: /* ba mana */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, randint(p_ptr->lev * 3)+20, 2);
                        break;
                case 40: /* ba dark */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DARK, dir, randint(p_ptr->lev * 3)+20, 2);
                        break;
                case 44: /* cause1 */
		        if (get_aim_dir(&dir))
		        {
			        fire_bolt(GF_MANA, dir, damroll(3, 8));
			}
		        break;
                case 45: /* cause2 */
		        if (get_aim_dir(&dir))
		        {
			        fire_bolt(GF_MANA, dir, damroll(8, 8));
			}
		        break;
                case 46: /* cause3 */
		        if (get_aim_dir(&dir))
		        {
			        fire_bolt(GF_MANA, dir, damroll(10, 15));
			}
		        break;
                case 47: /* cause4 */
		        if (get_aim_dir(&dir))
		        {
			        fire_bolt(GF_MANA, dir, damroll(15, 15));
			}
		        break;
                case 48: /* bo acid */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ACID, dir, damroll(7, 8) + (p_ptr->lev/3));
                        break;
                case 49: /* bo elec */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ELEC, dir, damroll(4, 8) + (p_ptr->lev/3));
                        break;
                case 50: /* bo fire */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_FIRE, dir, damroll(9, 8) + (p_ptr->lev/3));
                        break;
                case 51: /* bo cold */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_COLD, dir, damroll(6, 8) + (p_ptr->lev/3));
                        break;
                case 52: /* bo pois */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_POIS, dir, damroll(7, 8) + (p_ptr->lev/3));
                        break;
                case 53: /* bo neth */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_NETHER, dir, damroll(5, 5) + (p_ptr->lev/3));
                        break;
                case 54: /* bo wate */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_WATER, dir, damroll(10, 10) + (p_ptr->lev/3));
                        break;
                case 55: /* bo mana */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_MANA, dir, damroll(3, 8) + (p_ptr->lev/3));
                        break;
                case 56: /* bo plas */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_PLASMA, dir, damroll(8, 8) + (p_ptr->lev/3));
                        break;
                case 57: /* bo ice */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ICE, dir, damroll(6, 6) + (p_ptr->lev/3));
                        break;
                case 58: /* missile */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_MISSILE, dir, damroll(2, 6) + (p_ptr->lev/3));
                        break;
                case 59: /* blind */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_CONFUSION, dir, damroll(1, 8) + (p_ptr->lev/3));
                        break;
                case 60: /* scare */
			if (get_aim_dir(&dir))
				fear_monster(dir, plev);
                        break;
                case 61: /* conf */
			if (get_aim_dir(&dir))
                               fire_bolt(GF_CONFUSION, dir, damroll(7, 8) + (p_ptr->lev/3));
                        break;
                case 62: /* slow */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_OLD_SLOW, dir, damroll(6, 8) + (p_ptr->lev/3));
                        break;
                case 63: /* hold */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_OLD_SLEEP, dir, damroll(5, 8) + (p_ptr->lev/3));
                        break;


                case 64: /* haste */
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev) ) + plev);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5));
			}
                        break;
                case 65: /* hand of doom */
                        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(10, 8) + (p_ptr->lev));
			}
                        break;
                case 66: /* heal */
                        hp_player(damroll(8,5));
                        break;
                case 67: /* Summon animals */
                        for (k = 0; k < 4; k++)
                        {
                                summon_specific_friendly(y, x, rlev, SUMMON_ANIMAL, TRUE);
                        }
                        break;
                case 68: /* Blink */
                        if(dungeon_flags1 & LF1_NO_TELEPORT) {msg_print("No teleport on special levels ...");break;}
                        teleport_player(10);
                        break;
                case 69: /* Teleport */
                        if(dungeon_flags1 & LF1_NO_TELEPORT) {msg_print("No teleport on special levels ...");break;}
                        teleport_player(plev * 5);
                        break;
                case 70: /* tele to */
                        {
                             int ii,ij;

                             if(dungeon_flags1 & LF1_NO_TELEPORT) {msg_print("No teleport on special levels ...");break;}
                             msg_print("You go between.");
                             if (!tgt_pt(&ii,&ij)) break;
                             p_ptr->energy -= 60 - plev;
                             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
                             (distance(ij,ii,py,px) > plev*20 + 2))
                             {
                                 msg_print("You fail to show the destination correctly!");
                                 p_ptr->energy -= 100;
                                 teleport_player(10);
                             }
                             else teleport_player_to(ij,ii);
                        }
                        break;
                case 71: /* tele away */
                        if(dungeon_flags1 & LF1_NO_TELEPORT) {msg_print("No teleport on special levels ...");break;}
                        if (!get_aim_dir(&dir)) break;
                        (void)fire_beam(GF_AWAY_ALL, dir, plev);
                        break;
                case 72: /* tele level */
                        if(dungeon_flags1 & LF1_NO_TELEPORT) {msg_print("No teleport on special levels ...");break;}
			teleport_player_level();
			break;
                case 73: /* darkness */
                        (void)project(-1, 3, py, px, 0, GF_DARK_WEAK, PROJECT_GRID | PROJECT_KILL);
                        /* Unlite up the room */
                        unlite_room(py, px);
                        break;
                case 74: /* create traps */
			trap_creation();
		        break;
                case 76: /* raise the dead - uses the same code as the
			    nether spell*/
		{
                        if (!get_aim_dir(&dir)) break;
                        fire_ball(GF_RAISE, dir, 1, 0);
			break;
		}
                case 79: /* Summon dragonridder */
                                for (k = 0; k < 1; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_DRAGONRIDDER, TRUE);
				}
                        break;
                case 80: /* Summon kin */
				summon_kin_type = r_ptr->d_char; /* Big hack */
                                for (k = 0; k < 6; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_KIN, TRUE);
				}
                        break;
                case 81: /* Summon cyber */
                                for (k = 0; k < 1; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_HI_DEMON, TRUE);
				}
                        break;
                case 82: /* Summon monster */
                                for (k = 0; k < 1; k++)
				{
                                                summon_specific_friendly(y, x, rlev, 0, TRUE);
				}
                        break;
                case 83: /* Summon monsters */
                                for (k = 0; k < 6; k++)
				{
                                                summon_specific_friendly(y, x, rlev, 0, TRUE);
				}
                        break;
                case 84: /* Summon ant */
                                for (k = 0; k < 6; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_ANT, TRUE);
				}
                        break;
                case 85: /* Summon spider */
                                for (k = 0; k < 6; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_SPIDER, TRUE);
				}
                        break;
                case 86: /* Summon hound */
                                for (k = 0; k < 6; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_HOUND, TRUE);
				}
                        break;
                case 87: /* Summon hydra */
                                for (k = 0; k < 6; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_HYDRA, TRUE);
				}
                        break;
                case 88: /* Summon angel */
                                for (k = 0; k < 1; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_ANGEL, TRUE);
				}
                        break;
                case 89: /* Summon demon */
                                for (k = 0; k < 1; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_DEMON, TRUE);
				}
                        break;
                case 90: /* Summon undead */
                                for (k = 0; k < 1; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_UNDEAD, TRUE);
				}
                        break;
                case 91: /* Summon dragon */
                                for (k = 0; k < 1; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_DRAGON, TRUE);
				}
                        break;
                case 92: /* Summon hiundead */
                                for (k = 0; k < 8; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_HI_UNDEAD_NO_UNIQUES, TRUE);
				}
                        break;
                case 93: /* Summon hidragon */
                                for (k = 0; k < 8; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_HI_DRAGON_NO_UNIQUES, TRUE);
				}
                        break;
                case 94: /* Summon wraith */
                                for (k = 0; k < 8; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_WRAITH, TRUE);
				}
                        break;
        }

        /* Take some SP */
        if (!no_cost)
        {
                int chance, pchance;

                chance = (monster_powers[Power].mana + r_ptr->level);
                pchance = adj_str_wgt[mp_ptr->spell_stat]/2 + p_ptr->lev;
                if (rand_int(chance) >= pchance)
                {
                        int m = monster_powers[Power].mana / 10;

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

        return num;
}

void cast_para_spell(int spell, byte level)
{
long dam = 0;
int i;
switch(spell)
  {
  /* Fungus Strike */
  case 0:
    { 
    int y, x;
    cave_type *c_ptr;
    monster_type *m_ptr;
	    
    /* Pick a target.  If none, quit. */            	    
    if (!tgt_pt(&x,&y)) return;
    c_ptr = &cave[y][x];

    if (!(c_ptr->m_idx)) break;
    m_ptr = &m_list[c_ptr->m_idx];
	    
    /* Find Damage */
    for (i = 1; i < m_max; i++)
      {
      monster_type *m_ptr = &m_list[i];

      /* Paranoia -- Skip dead monsters */
      if (!m_ptr->r_idx) continue;

      /* Require line of sight */
      if (!player_has_los_bold(m_ptr->fy, m_ptr->fx)) continue;
		
      dam += (r_info[m_ptr->r_idx].d_char == 'm' || 
              r_info[m_ptr->r_idx].d_char == ',') * r_info[m_ptr->r_idx].level;
      }
	    
    dam *= (p_ptr->lev * (100 * p_ptr->csp / p_ptr->msp)) / 100;
	    
    /* Apply Damage */
    if(r_info[m_ptr->r_idx].d_char == 'm' || r_info[m_ptr->r_idx].d_char == ',')
      m_ptr->hp += dam;
    else
      m_ptr->hp -= dam;
    break;
    }
  }
}

void cast_symbiotic_spell(int spell, byte level)
{
        object_type *o_ptr;
        monster_race *r_ptr;
        int to_s = level + p_ptr->to_s;
        long dam;

        /* Get the carried monster */
        o_ptr = &inventory[INVEN_CARRY];

	switch (spell)
	{
        case 0: /* Minor Symbiotic healing */
                dam = apply_power_dice(2, to_s, 1);
                if (info_spell)
                {
                        sprintf(spell_txt, " heal monster %ldd6", dam);
                        return;
                }
                if (o_ptr->k_idx)
                {
                        int max;

                        r_ptr = &r_info[o_ptr->pval];
                        max = maxroll(r_ptr->hdice, r_ptr->hside);
                        o_ptr->pval2 += damroll(dam, 6);
                        if(o_ptr->pval2 > max)o_ptr->pval2 = max;

                        msg_print("Your monster is healed");

                        /* Display the monster hitpoints */
                        p_ptr->redraw |= (PR_MH);
                }
                else
                        msg_print("You are not in symbiosis.");
                break;

        case 1: /* Tangled Creepers */
                if (info_spell) return;
                slow_monsters();
                break;

        case 2: /* Vampiric healing */
        {
                int dummy,plev=p_ptr->lev,dir;
                int max;

                dam = apply_power_dur(plev, to_s, 2);
                if (info_spell)
                {
                        sprintf(spell_txt, " heal monster %ld+d%ld", dam, dam);
                        return;
                }

                r_ptr = &r_info[o_ptr->pval];
                max = maxroll(r_ptr->hdice, r_ptr->hside);

                if (!get_aim_dir(&dir)) return;
                dummy = dam + randint(dam);   /* Dmg */
                if (drain_life(dir, dummy))
                {
                        o_ptr->pval2 += dummy;
                        if(o_ptr->pval2 > max)o_ptr->pval2 = max;
                        p_ptr->redraw |= (PR_MH);
                }
                break;
       }

        case 3: /* Life transfer */
                dam = apply_power_dice(6, to_s, 1);
                if (info_spell)
                {
                        sprintf(spell_txt, " hp->hp monster %ldd15", dam);
                        return;
                }
                if(o_ptr->k_idx)
                {
                        int max,hp;

                        r_ptr = &r_info[o_ptr->pval];
                        max = maxroll(r_ptr->hdice, r_ptr->hside);

                        hp = damroll(dam, 15);
                        if(p_ptr->chp-hp > 0)
                        {
                                o_ptr->pval2 += hp;
                                if(o_ptr->pval2 > max)o_ptr->pval2 = max;
                                p_ptr->chp -= hp;
                                msg_print("Your monster is healed");

                                /* Redraw */
                                p_ptr->redraw |= (PR_HP);

                                /* Window stuff */
                                p_ptr->window |= (PW_PLAYER);

                                /* Display the monster hitpoints */
                                p_ptr->redraw |= (PR_MH);
                        }
                        else
                                msg_print("You can't do this in your weakened state.");
                }
                else
                        msg_print("You are not in symbiosis.");
                break;

        case 4: /* Satisfy Hunger */
                if (info_spell) return;
                (void)set_food(PY_FOOD_MAX - 1);
                break;

        case 5: /* Symbiotic minor powers */
                if (info_spell) return;
                if(o_ptr->k_idx) use_symbiotic_power(o_ptr->pval, FALSE, FALSE, TRUE);
                else msg_print("You are not in symbiosis.");
                break;

        case 6: /* Summon a never-moving pet */
                if (info_spell) return;
                summon_specific_friendly(py, px, dun_level, SUMMON_MINE,FALSE);
                break;

        case 16: /* Mana healing */
                dam = apply_power_dice(6, to_s, 1);
                if (info_spell)
                {
                        sprintf(spell_txt, " sp->hp monster %ldd15", dam);
                        return;
                }
                if(o_ptr->k_idx)
                {
                        int max,hp;

                        r_ptr = &r_info[o_ptr->pval];
                        max = maxroll(r_ptr->hdice, r_ptr->hside);

                        hp = damroll(dam, 15);
                        if(p_ptr->csp-hp > 0)
                        {
                                o_ptr->pval2 += hp;
                                if(o_ptr->pval2 > max)o_ptr->pval2 = max;
                                p_ptr->csp -= hp;
                                msg_print("Your monster is healed");

                                /* Redraw */
                                p_ptr->redraw |= (PR_MANA);

                                /* Window stuff */
                                p_ptr->window |= (PW_PLAYER);

                                /* Display the monster hitpoints */
                                p_ptr->redraw |= (PR_MH);
                        }
                        else
                                msg_print("You can't do this in your weaken state.");
                }
                else
                        msg_print("You are not in symbiosis.");
                break;

        case 17: /* Summon some never-moving pets */
                {
                        int k;

                        if (info_spell) return;

                        for(k=0;k<6;k++)
                                summon_specific_friendly(py, px, dun_level, SUMMON_MINE,FALSE);
                }
                break;

        case 18: /* Major Symbiotic healing */
                dam = apply_power_dice(3, to_s, 1);
                if (info_spell)
                {
                        sprintf(spell_txt, " heal monster %ldd60", dam);
                        return;
                }
                if(o_ptr->k_idx)
                {
                        int max;

                        r_ptr = &r_info[o_ptr->pval];
                        max = maxroll(r_ptr->hdice, r_ptr->hside);
                        o_ptr->pval2 += damroll(dam, 60);
                        if(o_ptr->pval2 > max)o_ptr->pval2 = max;

                        msg_print("Your monster is healed");

                        /* Display the monster hitpoints */
                        p_ptr->redraw |= (PR_MH);
                }
                else
                        msg_print("You are not in symbiosis.");
                break;

        case 19: /* Healing */
                dam = apply_power_dice(2, to_s, 1);
                if (info_spell)
                {
                        sprintf(spell_txt, " heal %ldd40", dam);
                        return;
                }
                hp_player(damroll(dam, 40));
                break;

        case 20: /* Major Symbiotic powers */
                if (info_spell) return;
                if(o_ptr->k_idx) use_symbiotic_power(o_ptr->pval, TRUE, FALSE, TRUE);
                else msg_print("You are not in symbiosis.");
                break;

        case 21: /* Use Enemy's Powers */
        {
                int y,x;
                cave_type *c_ptr;
                monster_type *m_ptr;

                if (info_spell) return;

                if (!tgt_pt(&x,&y)) return;
                c_ptr = &cave[y][x];

                if (!(c_ptr->m_idx)) break;
                m_ptr = &m_list[c_ptr->m_idx];
                use_symbiotic_power(m_ptr->r_idx, TRUE, FALSE, TRUE);
                break;
        }

        default:
                msg_format("You cast an unknown Symbiotic spell: %d.", spell);
		msg_print(NULL);
    }
}

void cast_music_spell(int spell, byte level)
{
        int     dir, dummy;
	int	plev = p_ptr->lev;
        int to_s2=(level + p_ptr->to_s)/2;
        to_s2 = (to_s2==0)?1:to_s2;
	
        if(p_ptr->class_extra1)
        {
                msg_print("You stop singing.");
                p_ptr->class_extra1 = MUSIC_NONE;
        }

        p_ptr->class_extra2 = spell;

	switch (spell)
	{
                case 0: /* Song of slowness */
                        if (info_spell) return;
                       msg_print("You start humming a slow, steady melody...");
                       p_ptr->class_extra1 = MUSIC_SLOW;
		       break;
                case 1:  /* Stop singing */
                        if (info_spell) return;
                       p_ptr->class_extra1 = MUSIC_NONE;
		       break;
                case 2:  /* The note that scares */
                        if (info_spell)
                            return;
                       msg_print("You cry out in an ear-wracking voice...");
                       scare_monsters();
		       break;
                case 3:  /* Stunning song */
                        if (info_spell) return;
                       msg_print("You weave a pattern of sounds to bewilder and daze...");
                       p_ptr->class_extra1 = MUSIC_STUN;
		       break;
                case 4:  /* Song of life */
                        if (info_spell) return;
                       msg_print("Life flows through you as you sing a song of healing...");
                       p_ptr->class_extra1 = MUSIC_LIFE;
		       break;
                case 5:  /* Song of mind */
                        if (info_spell) return;
                       msg_print("Your quiet music sharpens your sense of hearing...");
                       p_ptr->class_extra1 = MUSIC_MIND;
		       break;
                case 6:  /* Song of lite */
                        if (info_spell) return;
                       msg_print("Your uplifting song brings brightness to dark places...");
                       p_ptr->class_extra1 = MUSIC_LITE;
		       break;
                case 7:  /* Song of fury */
                        if (info_spell) return;
                       msg_print("Your sing of the heroic Elder Days...");
                       p_ptr->class_extra1 = MUSIC_FURY;
		       break;
                case 8:  /* Awareness song */
                        if (info_spell) return;
                       msg_print("As you start singing, you become more aware of the world around you.");
                       p_ptr->class_extra1 = MUSIC_AWARE;
                       break;
                case 9:  /* Song of knowledge */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " range 1");
                                return;
                        }
                       msg_print("You recall the rich lore of the world...");
                       p_ptr->class_extra1 = MUSIC_ID;
                       break;
                case 10:  /* Between note */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " range 100");
                                return;
                        }
                       msg_print("Your voice twists through space...");
                       teleport_player(100);
                       break;
                case 11:  /* The song which kill */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd6", 10 + ((plev - 1) / 5));
                                return;
                        }
                       msg_print("You call out with a terrible curse...");
                       project_hack(GF_SOUND, damroll(5 + ((plev - 1) / 5), 1));
                       break;
                case 12:  /* song of illusion */
                        if (info_spell) return;
                       msg_print("You weave a pattern of sounds to beguile and confuse...");
                       p_ptr->class_extra1 = MUSIC_ILLUSION;
                       break;
                case 13:  /* wall breaking song */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " rad 1");
                                return;
                        }
                       msg_print("You weave a pattern of sounds to contort and shatter...");
                       p_ptr->class_extra1 = MUSIC_WALL;
                       break;
                case 14:  /* song of resistance */
                        if (info_spell) return;
                       msg_print("You sing a song of perserverance against powers...");
                       p_ptr->class_extra1 = MUSIC_RESIST;
                       break;
                case 15:  /* song of time */
                        if (info_spell) return;
                       msg_print("You start singing swift and light folk-tunes...");
                       p_ptr->class_extra1 = MUSIC_TIME;
                       break;
                case 16:  /* Education song */
                {
                        monster_type *m_ptr;
                        monster_race *r_ptr;
                        int m;

                        if (info_spell) return;
                        msg_print("You recall legends of the fair and foul creatures of the world...");

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
                case 17:  /* World Contortion */
                        if (info_spell) return;
                       msg_print("Reality whirls wildly as you sing a dizzying melody...");
                       fire_explosion(py, px, GF_AWAY_ALL, p_ptr->lev/15 + 1, 1 + (p_ptr->lev * 3));
                       break;
                case 18:  /* charming note */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %dd5",(10+((plev-1)/5)));
                                return;
                        }
                       msg_print("You chant a song of friendship...");
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_CHARM, dir,
                              damroll(10 + ((plev - 1) / 5), 5));
                       break;
                case 19:  /* song of death */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd10", 20 + ((plev - 1) / 5));
                                return;
                        }
                       msg_print("You unleash a furious barrage of sounds...");
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_SOUND, dir,
                              damroll(20 + ((plev - 1) / 5), 10));
                       break;
                case 20:  /* vibration note */
                        if (info_spell) return;
                       msg_print("You sing an old song of the dwarven-smiths...");
                       brand_weapon(5); /* brand earthquake */
                       break;
                case 21:  /* vibration song */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd6/turn",10+plev/15);
                                return;
                        }
                       msg_print("The fury of the Downfall of Numenor lashes out...");
                       p_ptr->class_extra1 = MUSIC_VIBRA;
                       break;
                case 22:  /* song of disruption */
                        if (info_spell) return;
                       msg_print("You sing of the primeval shaping of Middle-earth...");
                       alter_reality();
                       break;
                case 23:  /* Lay of Gil-Galad */
                        if (info_spell) return;
                       msg_print("You chant a powerful, heroic call to arms...");
            for (dummy = 0; dummy < 3 + (plev / 10); dummy++)
            {
                if (randint(10)>3)
                {
                 summon_specific_friendly(py, px, plev*to_s2, SUMMON_NO_UNIQUES, FALSE);
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
                case 24:  /* hidding song */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power 40");
                                return;
                        }
                       msg_print("Your song carries you beyond the sight of mortal eyes...");
                       p_ptr->class_extra1 = MUSIC_HIDE;
                       break;
                case 25:  /* shriek of death */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd20", 100 + ((plev - 1) / 5));
                                return;
                        }
                       msg_print("A chanting dark and fell rises, bearing death to the world...");
                       if (!get_aim_dir(&dir)) return;
                       fire_bolt(GF_SOUND, dir,
                              damroll(100 + ((plev - 1) / 5), 20));
                       break;
                case 26:  /* charming song */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " rad 1");
                                return;
                        }
                       msg_print("You weave a slow, soothing melody of imploration...");
                       p_ptr->class_extra1 = MUSIC_CHARME;
                       break;
                case 27:  /* become a god */
                        if (info_spell) return;
                       msg_print("The holy power of the Music of the Ainur enters you...");
                       p_ptr->class_extra1 = MUSIC_HOLY;
                       break;
                case 28:  /* destruction shriek */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " rad 10");
                                return;
                        }
                       msg_print("The unmeasurable destructive power of the Oceans crashes around you...");
                       destroy_area(py, px, 10, TRUE);
                       break;
                case 29:  /* song of liberty */
                        if (info_spell) return;
                       msg_print("You recall the valor of Fingolfin's challenge to the Dark Lord...");
                       p_ptr->class_extra1 = MUSIC_LIBERTY;
                       break;
                case 30:  /* dispels evil */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " rad 1");
                                return;
                        }
                       msg_print("The themes of life and revival are woven into your song...");
                       p_ptr->class_extra1 = MUSIC_BEAUTY;
                       break;
                case 31:  /* Immaterial song */
                        if (info_spell) return;
                       msg_print("You chant a deep and moving hymn of the shadowy Halls of Mandos...");
                       p_ptr->class_extra1 = MUSIC_SHADOW;
                       break;

                default:
                        msg_format("You sing an unknown song: %d.", spell);
                        msg_print(NULL);
        }
}

void cast_tribal_spell(int spell, byte level)
{
        int     dir, beam;
        int     plev = p_ptr->lev;
        int     to_s = level + p_ptr->to_s;
        long    dam, rad;

        if (cp_ptr->flags1 & CF1_BEAM) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
                case 0: /* Slumber */
                        rad = apply_power_dice(2, to_s, 1);
                        dam = apply_power_dam(5 + plev, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                        fire_ball(GF_OLD_SLEEP, 0, dam, rad);
                        break;
                case 1: /* Lightning Bolt */
                        dam = apply_power_dice(2, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ldd10", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                damroll(dam, 10));
                        break;
                case 2: /* Bewilder */
                        rad = apply_power_dice(1, to_s, 1);
                        dam = apply_power_dam(7 + plev, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_OLD_CONF, dir, dam, rad);
                        break;
                case 3: /* Song of Morning */
                        rad = apply_power_dice(5, to_s, 1);
                        dam = apply_power_dam(10 + plev, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        fire_ball(GF_LITE, 0, dam, rad);
                        break;
                case 4: /* Recuperation */
                        dam = apply_power_dice(1, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " heal %ldd20", dam);
                                return;
                        }
                        hp_player(damroll(dam, 20));
                        break;
                case 5: /* Meditate */
                        if (info_spell) return;
                        set_paralyzed(p_ptr->paralyzed + damroll(6 + to_s, 10));
                        fire_ball(GF_MAKE_GLYPH, 0, 1, 2 + to_s);
                        heal_insanity(300 + (level + p_ptr->to_s));
                        do_res_stat(A_INT);
                        project_hack(GF_AWAY_ALL, 5 + to_s);
                        break;
                case 6: /* Wolf Spirit */
                        dam = apply_power_dur(10, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        if(p_ptr->mimic_form != MIMIC_WOLF)
                                set_mimic(dam + randint(10), MIMIC_WOLF);
                        else
                                set_mimic(p_ptr->tim_mimic + dam + randint(10), MIMIC_WOLF);
                        break;
                case 7: /* Dress Wounds */
                        dam = apply_power_dice(3, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " heal %ldd15", dam);
                                return;
                        }
                        hp_player(damroll(dam, 15));
                        set_cut(0);
                        break;

                case 8: /* Grow Berries */
                        if (info_spell) return;
                        set_food(p_ptr->food + 3000);
                        break;
                case 9: /* Sense Presence */
                        if (info_spell) return;
                        detect_monsters_normal();
                        break;
                case 10: /* Punishment */
                        rad = apply_power_dice(4, to_s, 1);
                        dam = apply_power_dam(25, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ldd2", dam);
                                return;
                        }
                        fire_ball(GF_MANA, 0, damroll(dam, 2), rad);
                        break;
                case 11: /* Life Drain */
                        rad = apply_power_dice(5, to_s, 1);
                        dam = apply_power_dice(50, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ldd50", dam);
                                return;
                        }
                        dec_stat(A_STR, 5, (magik(50))?STAT_DEC_NORMAL:STAT_DEC_PERMANENT);
                        dec_stat(A_CON, 5, (magik(50))?STAT_DEC_NORMAL:STAT_DEC_PERMANENT);
                        dec_stat(A_DEX, 5, (magik(50))?STAT_DEC_NORMAL:STAT_DEC_PERMANENT);
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_MANA, dir, damroll(50, dam), rad);
                        break;
                case 12: /* Beetle Spirit */
                        dam = apply_power_dur(10, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld+d10", dam);
                                return;
                        }
                        if(p_ptr->mimic_form != MIMIC_INSECT)
                                set_mimic(dam + randint(10), MIMIC_INSECT);
                        else
                                set_mimic(p_ptr->tim_mimic + dam + randint(10), MIMIC_INSECT);
                        break;
                case 13: /* Call Nature */
                        if (info_spell) return;
                        summon_specific_friendly(py, px, dun_level, SUMMON_ANT, FALSE);
                        summon_specific_friendly(py, px, dun_level, SUMMON_SPIDER, FALSE);
                        break;
                case 14: /* Bird Spirit */
                        dam = apply_power_dur(10, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        if(p_ptr->mimic_form != MIMIC_SPARROW)
                                set_mimic(dam + randint(10), MIMIC_SPARROW);
                        else
                                set_mimic(p_ptr->tim_mimic + dam + randint(10), MIMIC_SPARROW);
                        break;
                case 15: /* Winter's Fury */
                        rad = apply_power_dice(2, to_s, 1);
                        dam = apply_power_dice(5, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ldd14", dam);
                                return;
                        }
                        fire_ball(GF_COLD, 0, damroll(dam, 10), 2 + rad);
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_COLD, dir, damroll(dam, 18), rad);
                        break;

                case 16: /* Stun */
                        rad = apply_power_dice(6, to_s, 1);
                        dam = apply_power_dice(5, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ldd5", dam);
                                return;
                        }
                        fire_ball(GF_OLD_CONF, 0, damroll(dam, 5), rad);
                        fire_ball(GF_OLD_SLEEP, 0, damroll(dam, 5), rad);
                        fire_ball(GF_OLD_SLOW, 0, damroll(dam, 5), rad);
                        break;
                case 17: /* Remove Evil */
                        if (info_spell) return;
                        remove_curse();
                        break;
                case 18: /* Destroy Traps */
                        if (info_spell) return;
                        fire_ball(GF_KILL_TRAP, 0, 1, 2 + to_s);
                        break;
                case 19: /* Lore */
                        if (info_spell) return;
                        ident_spell();
                        break;
                case 20: /* Invoked Destruction */
                        dam = apply_power_dice(2, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam 75d%ld", dam);
                                return;
                        }
                        invoke(GF_FIRE, damroll(75, dam));
                        break;
                case 21: /* Ruins */
                        rad = apply_power_dice(6, to_s, 1);
                        dam = apply_power_dam(180, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        fire_ball(GF_DESTRUCTION, 0, dam, rad);
                        break;
                case 22: /* Fright */
                        if (info_spell) return;
                        scare_monsters();
                        break;
                case 23: /* Winds of Displacement */
                        rad = apply_power_dice(1, to_s, 1);
                        dam = apply_power_dam(180, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                        fire_ball(GF_AWAY_ALL, 0, dam, rad);
                        break;

                case 24: /* Death Chant */
                        rad = apply_power_dam(200, to_s, 1);
                        dam = apply_power_dice(40, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        dec_stat(A_STR, 15, FALSE);
                        dec_stat(A_CON, 15, FALSE);
                        dec_stat(A_DEX, 15, FALSE);
                        dec_stat(A_INT, 15, FALSE);
                        take_hit(25, "casting Death Chant");
                        fire_ball(GF_MANA, 0, damroll(rad, 4), 4);
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_MANA, dir, damroll(dam, 20), 2);
                        break;
                case 25: /* Storm's Fury */
                        dam = apply_power_dice(1, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ldd150", dam);
                                return;
                        }
                        project_meteor(3, GF_ELEC, damroll(dam, 150), PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_THRU);
                        break;
                case 26: /* Call Hydras */
                        if (info_spell) return;
                        summon_specific_friendly(py, px, dun_level, SUMMON_HYDRA, TRUE);
                        break;
                case 27: /* Lifeblood */
                        hp_player(damroll(1000, 2 + to_s));
                        heal_insanity(damroll(110, 2 + to_s));
                        set_cut(0);
                        set_poisoned(0);
                        set_stun(0);
                        set_shield(p_ptr->shield + 10 + randint(10) + to_s, 50 + to_s, 0);
                        do_res_stat(A_STR);
                        do_res_stat(A_INT);
                        do_res_stat(A_WIS);
                        do_res_stat(A_DEX);
                        do_res_stat(A_CON);
                        do_res_stat(A_CHR);
                        lose_exp(p_ptr->exp / 6);
                        break;
                case 28: /* Invoked Winds */
                        if (info_spell) return;
                        invoke(GF_AWAY_ALL, damroll(1, 200));
                        break;
                case 29: /* Dispel Evil */
                        rad = apply_power_dice(10, to_s, 1);
                        dam = apply_power_dam(100, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam 3*%ld", dam);
                                return;
                        }
                        fire_ball(GF_HOLY_FIRE, 0, dam * 2, rad);
                        dispel_evil(dam);
                        break;
                case 30: /* Mystic Forces */
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam a lot");
                                return;
                        }
                        fire_ball(GF_ELEC, 0, damroll(100, 10 + to_s), 10 + to_s);
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_MANA, dir, damroll(1 + to_s, 400), 5 + to_s);
                        fire_beam(GF_ICE, dir, damroll(1 + to_s, 100));
                        break;
                case 31: /*  */
                        break;

                default:
                        msg_format("You cast an unknown Tribal spell: %d.", spell);
                        msg_print(NULL);
        }
}

void cast_druid_spell(int spell, byte level)
{
        int     dir, beam;
        int     plev = p_ptr->lev;
        int     to_s = level + p_ptr->to_s;
        long    dam, rad;

        int amt = 0, att = 0;

        if (cp_ptr->flags1 & CF1_BEAM) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
                case 0: /* Tunnel */
                {
                        magic_type *s_ptr = &realm_info[REALM_DRUID][0];
                        int d, i, min, ox, oy, x=0, y=0;
                        int tries = 0, dis;
                        int xx = -1, yy = -1;
                        bool look = TRUE;

                        dam = apply_power_dice(10, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " range %ld", dam);
                                return;
                        }
                        dis = dam;

                        if(!p_ptr->class_extra5)
                        {
                        amt = get_quantity("What is the minimal amount of mana the tunnel end must have?", 255);

                        att = get_check("Does the mana have to match exactly the specified amount?");

                        if(p_ptr->resist_continuum) {msg_print("The space-time continuum can't be disrupted."); return;}

                        if (p_ptr->anti_tele)
                        {
                                msg_print("A mysterious force prevents you from teleporting!");
                                return;
                        }

                        if (dis > 200) dis = 200; /* To be on the safe side... */

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

                        /* Absord some mana */
                        if(s_ptr->smana <= amt)
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

                                                        if ((r_ptr->flags6
                                                            & RF6_TPORT) &&
                                                            !(r_ptr->flags3
                                                            & RF3_RES_TELE))
                                                                /*
                                                                 * The latter limitation is to avoid
                                                                 * totally unkillable suckers...
                                                                 */
                                                        {
                                                                if (!(m_list[cave[oy+yy][ox+xx].m_idx].csleep))
                                                                        teleport_to_player(cave[oy+yy][ox+xx].m_idx);
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
                        p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

                        /* Update the monsters */
                        p_ptr->update |= (PU_DISTANCE);

                        /* Window stuff */
                        p_ptr->window |= (PW_OVERHEAD);

                        /* Handle stuff XXX XXX XXX */
                        handle_stuff();
                        }else msg_print("This level has already been drained!");
                        break;
                }
                case 1: /* Canalize Mana */
                        if (info_spell) return;
                        if(p_ptr->class_extra7 == CLASS_CANALIZE_MANA)
                        {
                                p_ptr->class_extra7 = CLASS_NONE;
                                msg_print("You stop canalizing the earth's mana.");
                        }
                        else
                        {
                                msg_print("You begin canalizing in yourself the earth's mana.");
                                p_ptr->class_extra7 = CLASS_CANALIZE_MANA;
                        }
                        break;
                case 2: /* Acid Bolt */
                        dam = apply_power_dice(4, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd%ld", 3+((plev-5)/4), dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_ACID, dir,
                                damroll(3+((plev-5)/4), dam));
                        break;
                case 3: /* Mana Path */
                        if (info_spell) return;
                        if(!p_ptr->class_extra5)
                        {
                        if(p_ptr->class_extra7 == CLASS_MANA_PATH)
                        {
                                p_ptr->class_extra7 = CLASS_NONE;
                                msg_print("You stop laying a mana path.");

                                p_ptr->update |= PU_BONUS;
                        }
                        else
                        {
                                msg_print("You begin laying a mana path.");

                                /* Ask for the amount of mana to lay down */
                                amt = get_quantity("What amount of mana do you want to lay on each step?", 255);
                                if(!amt) break;

                                /* Ask for laying type */
                                if(get_check("Do you want to absorb the old mana before laying?"))
                                        att |= CLASS_MANA_PATH_ERASE;

                                p_ptr->class_extra7 = CLASS_MANA_PATH;
                                p_ptr->class_extra6 = amt + (att << 8) ;

                                p_ptr->update |= PU_BONUS;
                        }
                        }else msg_print("This level has already been drained!");
                        break;
                case 4: /* Forest Generation */
                {
                        int i,j;
                        byte rad = 2 + (plev / 25) + to_s;

                        if (info_spell) return;
                        for (j = py - rad; j < py + rad; j++)
                        for (i = px - rad; i < px + rad; i++)
                        {
                                if ((distance(py, px, j, i) <= rad) && in_bounds(j,i))
                                {
                                        if ((cave[j][i].mana >= 20) && cave_clean_bold(j, i) && cave_empty_bold(j, i))
                                        {
                                                cave[j][i].mana -= 20;
                                                cave_set_feat(j, i, FEAT_TREES);
                                        }
                                }
                        }
                        break;
                }
                case 5: /* Druidistic Acid Beam */
                        dam = apply_power_dice(4, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd%ld", 5+((plev-5)/4), dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_druid_beam(GF_ACID, dir,
                                damroll(5+((plev-5)/4), dam));
                        break;
                case 6: /* Raise Mountains */
                {
                        int i,j;
                        byte rad = 2 + (plev / 25) + to_s;

                        if (info_spell) return;
                        for(j = py - rad; j < py + rad; j++)
                        for(i = px - rad; i < px + rad; i++)
                                if((distance(py, px, j, i) <= rad) && in_bounds(j,i))
                                {
                                        if((cave[j][i].mana >= 20) && cave_clean_bold(j, i) && cave_empty_bold(j, i))
                                        {
                                                cave[j][i].mana -= 20;
                                                cave_set_feat(j, i, FEAT_MOUNTAIN);
                                        }
                                }
                        break;
                }
                case 7: /* Stone Skin */
                        dam = apply_power_dur(10, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        set_shield(p_ptr->shield + dam + randint(10), 30 + (to_s * 2), 0);
                        break;

                case 8: /* Infra */
                        dam = apply_power_dur(10, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d15", dam);
                                return;
                        }
                        set_tim_infra(p_ptr->tim_infra + dam + randint(15));
                        break;
                case 9: /* Fire Bolt */
                        dam = apply_power_dice(7, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd%ld", 6+((plev-5)/4), dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_FIRE, dir,
                                damroll(6 + ((plev-5)/4), dam));
                        break;
                case 10: /* Fire Ball */
                        rad = apply_power_dice(2, to_s, 1);
                        dam = apply_power_dam(40, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_FIRE, dir,
                                dam, rad);
                        break;
                case 11: /* Enlight Traps */
                        if (info_spell) return;
                        detect_traps();
                        break;
                case 12: /* Fire Beam */
                        dam = apply_power_dice(8, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd%ld", 7+((plev-5)/4), dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_beam(GF_FIRE, dir,
                                damroll(7 + ((plev-5)/4), dam));
                        break;
                case 13: /* Druidistic Fire Bolt */
                        dam = apply_power_dice(8, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd%ld", 7+((plev-5)/4), dam);
                                return;
                        }
                        if(!p_ptr->class_extra5)
                        {
                        if (!get_aim_dir(&dir)) return;
                        fire_druid_bolt(GF_FIRE, dir,
                                damroll(7 + ((plev-5)/4), dam));
                        }else msg_print("This level has already been drained!");
                        break;
                case 14: /* Druidistic Fire Beam */
                        dam = apply_power_dice(8, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd%ld", 7+((plev-5)/4), dam);
                                return;
                        }
                        if(!p_ptr->class_extra5)
                        {
                        if (!get_aim_dir(&dir)) return;
                        fire_druid_beam(GF_ACID, dir,
                                damroll(7 + ((plev-5)/4), dam));
                        }else msg_print("This level has already been drained!");
                        break;
                case 15: /* Create Lava */
                {
                        int i,j;
                        byte rad = 2 + (plev / 25) + to_s;

                        if (info_spell) return;
                        for(j = py - rad; j < py + rad; j++)
                        for(i = px - rad; i < px + rad; i++)
                                if((distance(py, px, j, i) <= rad) && in_bounds(j,i))
                                {
                                        if((cave[j][i].mana >= 20) && cave_clean_bold(j, i) && cave_empty_bold(j, i))
                                        {
                                                cave[j][i].mana -= 20;
                                                cave_set_feat(j, i, FEAT_DEEP_LAVA);
                                        }
                                }
                        break;
                }

                case 16: /* Winds of Mana */
                        if (info_spell) return;
                        if(!p_ptr->class_extra5)
                        {
                        if(p_ptr->class_extra7 == CLASS_WINDS_MANA)
                        {
                                p_ptr->class_extra7 = CLASS_NONE;
                                msg_print("You stop expulsing mana winds.");
                        }
                        else
                        {
                                msg_print("You begin expulsing mana winds.");

                                /* Ask for the amount of mana to lay down */
                                amt = get_quantity("What amount of mana do you want to lay on each step?", 255);
                                if(!amt) break;

                                /* Ask for laying type */
                                if(get_check("Do you want to absorb the old mana before laying?"))
                                        att |= CLASS_MANA_PATH_ERASE;

                                p_ptr->class_extra7 = CLASS_WINDS_MANA;
                                p_ptr->class_extra6 = amt + (att << 8) ;
                        }
                        }else msg_print("This level has already been drained!");
                        break;
                case 17: /* Summon Air Elem */
                {
                        int xx = px,yy = py;

                        if (info_spell) return;
                        msg_format("You magically summon an Air Elemental.");
                        scatter(&yy, &xx, py, px, 6, 0);
                        place_monster_aux(yy, xx, test_monster_name("Air elemental"), FALSE, FALSE, TRUE);
                        break;
                }
                case 18: /* Wispers from Afar */
                        if (info_spell) return;
                        ident_spell();
                        break;
                case 19: /* The Winds of Manwe */
                        rad = apply_power_dice(4, to_s, 1);
                        dam = apply_power_dur(10, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        fire_ball(GF_GRAVITY, 0, dam, rad);
                        break;
                case 20: /* Bird View */
                        if (info_spell) return;
                        wiz_lite_extra();
                        break;
                case 21: /* *Wispers from Afar* */
                        if (info_spell) return;
                        identify_fully();
                        break;
                case 22: /* Windy Speed */
                        dam = apply_power_dam(5, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d5", dam);
                                return;
                        }
                        set_fast(p_ptr->fast + dam + randint(5));
                        break;
                case 23: /* The Thunders of Manwe */
                        dam = apply_power_dam(8, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %dd%ld", 10+((plev-5)/4), dam);
                                return;
                        }
                        if(get_check("Do you want it to be a druidistic beam ?"))
                        {
                                if(!p_ptr->class_extra5)
                                {
                                        if (!get_aim_dir(&dir)) return;
                                        fire_druid_beam(GF_ELEC, dir,
                                                damroll(10 + ((plev-5)/4), dam));
                                }else msg_print("This level has already been drained!");
                        }
                        else
                        {
                                if (!get_aim_dir(&dir)) return;
                                fire_beam(GF_ELEC, dir,
                                        damroll(10 + ((plev-5)/4), dam));
                        }
                        break;

                case 24: /* Summon Aqua Golems */
                {
                        int xx = px,yy = py;

                        if (info_spell) return;
                        msg_format("You magically summon Aquatic Golems.");
                        scatter(&yy, &xx, py, px, 6, 0);
                        place_monster_aux(yy, xx, test_monster_name("Aquatic golem"), FALSE, TRUE, TRUE);
                        break;
                }
                case 25: /* Walk over the Water */
                        dam = apply_power_dur(50, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d40", dam);
                                return;
                        }
                        set_walk_water(p_ptr->walk_water + dam + randint(40));                        break;
                case 26: /* Flood */
                {
                        int i,j;
                        byte rad = 3 + (plev / 25) + to_s;

                        if (info_spell) return;

                        for(j = py - rad - 1; j < py + rad + 1; j++)
                        for(i = px - rad - 1; i < px + rad + 1; i++)
                                if((distance(py, px, j, i) <= rad) && in_bounds(j,i))
                                {
                                        if((cave[j][i].mana >= 20) && cave_clean_bold(j, i) && cave_empty_bold(j, i))
                                        {
                                                cave[j][i].mana -= 20;
                                                cave_set_feat(j, i, FEAT_DEEP_WATER);
                                        }
                                }
                                else if((distance(py, px, j, i) <= rad + 1) && in_bounds(j,i))
                                {
                                        if((cave[j][i].mana >= 20) && cave_clean_bold(j, i) && cave_empty_bold(j, i))
                                        {
                                                cave[j][i].mana -= 20;
                                                cave_set_feat(j, i, FEAT_SHAL_WATER);
                                        }
                                }
                        break;
                }
                case 27: /* Summon Water Elems */
                {
                        int xx = px,yy = py;

                        if (info_spell) return;
                        msg_format("You magically summon Water Elementals.");
                        scatter(&yy, &xx, py, px, 6, 0);
                        place_monster_aux(yy, xx, test_monster_name("Water elemental"), FALSE, TRUE, TRUE);
                        break;
                }
                        break;
                case 28: /* Purification */
                        dam = apply_power_dur(300, to_s, 5);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " heal %ld", dam);
                                return;
                        }
                        hp_player(dam);
                        set_blind(0);
                        set_confused(0);
                        set_poisoned(0);
                        set_stun(0);
                        set_cut(0);
                        break;
                case 29: /* Go Underwater */
                {
                        int ij, ii;

                        if (info_spell) return;

                        if((cave[py][px].feat == FEAT_DEEP_WATER) || (cave[py][px].feat == FEAT_SHAL_WATER))
                        {

                        msg_print("You go underwater, choose a destination.");
                        if (!tgt_pt(&ii,&ij)) return;
                        p_ptr->energy -= 60 - plev;
                        if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
                           (distance(ij,ii,py,px) > plev*10 + 2) || !(cave[ij][ii].info & CAVE_MARK) ||
                           ((cave[ij][ii].feat != FEAT_DEEP_WATER) && (cave[ij][ii].feat != FEAT_SHAL_WATER)))
                        {
                                msg_print("You fail to dive correctly!");
                                p_ptr->energy -= 100;
                                teleport_player(10);
                        }
                        else teleport_player_to(ij,ii);
                        } else msg_print("You must be on a water square.");
                        break;
                }
                case 30: /* Tidal Wave */
                        rad = apply_power_dice(4, to_s, 1);
                        dam = apply_power_dam(400, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_WATER, dir, dam, rad);
                        break;
                case 31: /* Flood Level */
                        if (info_spell) return;
                        msg_print("The world is being flooded !");
                        p_ptr->class_extra6 |= CLASS_FLOOD_LEVEL;
                        alter_reality();
                        break;

                case 32: /* Glyph of Warding */
                        if (info_spell) return;
                        warding_glyph();
                        break;
                case 33: /* Orb of Mana */
                        rad = apply_power_dice(2, to_s, 1);
                        dam = apply_power_dice(10, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam 45d%ld+%d", dam, plev * 2);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_MANA, dir, damroll(45, dam) + (plev * 2), rad);
                        break;
                case 34: /* Gather mana */
                        if (info_spell) return;
                        if(!p_ptr->class_extra5)
                        {

                        }else msg_print("This level has already been drained!");
                        break;
                case 35: /* Mirror of Mana */
                        dam = apply_power_dice(10, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        set_tim_reflect(p_ptr->tim_reflect + dam + randint(10));
                        break;
                case 36: /* Activate Rune of Mana */
                        if (info_spell) return;
                        if(!p_ptr->class_extra5)
                        {
                        if(p_ptr->class_extra7 == CLASS_CANALIZE_MANA)
                        {
                                p_ptr->class_extra7 = CLASS_NONE;
                                msg_print("You stop canalizing the earth's mana.");
                        }
                        else
                        {
                                msg_print("You begin canalizing in yourself the earth's mana.");
                                p_ptr->class_extra6 |= CLASS_CANALIZE_MANA_EXTRA;
                                p_ptr->class_extra7 = CLASS_CANALIZE_MANA;
                        }
                        break;
                        }else msg_print("This level has already been drained!");
                        break;
                case 37: /* Combine the 5 Elements */
                        dam = apply_power_dice(8, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam 5*%dd%ld", 10+((plev-5)/4), dam);
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
                case 38: /* Shield of Mana */
                        dam = apply_power_dur(20, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        set_shield(p_ptr->shield + dam + randint(10), 100 + (to_s * 2), 0);
                        break;
                case 39: /* Drain Level's Mana */
                {
                        int i, j;
                        long amt = 0;

                        for(i = 0; i < cur_wid; i++)
                        {
                                for(j = 0; j < cur_hgt; j++)
                                {
                                        amt += cave[j][i].mana / 60;
                                }
                        }

                        dam = apply_power_dam(amt, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if(!p_ptr->class_extra5)
                        {
                                p_ptr->class_extra5 = TRUE;
                                if (!get_aim_dir(&dir)) return;
                                (void)fire_bolt(GF_MANA, dir, dam);
                        }else msg_print("This level has already been drained!");
                        break;

                }
                default:
                        msg_format("You cast an unknown Druid spell: %d.", spell);
                        msg_print(NULL);
        }
}

void cast_spirit_spell(int spell, byte level)
{
        int     dir, beam;
        int     plev = p_ptr->lev;
        int     to_s = level + p_ptr->to_s;
        long    dam, rad;

	/* Access the spell */
        magic_type *s_ptr = &realm_info[REALM_SPIRIT][spell];

        if (cp_ptr->flags1 & CF1_BEAM) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
                case 0: /* Phase door */
                        dam = apply_power_dice(10, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " range %ld", dam);
                                return;
                        }
                        teleport_player(dam);
                        break;
                case 1: /* telep */
                        dam = apply_power_dur(100 + plev, to_s, 15);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " range %ld", dam);
                                return;
                        }
                        teleport_player(dam);
                        break;
                case 2: /* tele level */
                        if (info_spell) return;
                        teleport_player_level();
                        break;
                case 3: /* fast recall */
                        dam = 6 - to_s;
                        if (dam < 0) dam = 0;
                        if (info_spell)
                        {
                                sprintf(spell_txt, " wait 4+d%ld", dam);
                                return;
                        }
                        if (!p_ptr->word_recall)
                        {
                                p_ptr->word_recall = rand_int(5) + 4;
                                msg_print("The air about you becomes charged...");
                        }
                        else
                        {
                                p_ptr->word_recall = 0;
                                msg_print("A tension leaves the air around you...");
                        }
                        break;
                case 4: /* prob trav */
                        dam = apply_power_dur(10, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d%d", dam, 10 + plev);
                                return;
                        }
                        set_prob_travel(p_ptr->prob_travel + dam + randint(10 + plev));
                        break;
                case 5: /* energize */
                        /* Wipe all mana */
                        dam = apply_power_dur(200, to_s, 20);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " energy %ld", dam);
                                return;
                        }
                        if (s_ptr->smana < p_ptr->csp) p_ptr->csp = s_ptr->smana;
                        p_ptr->energy += dam;
                        break;
                case 6: /* solidify continuum */
                        dam = apply_power_dam((plev / 5) + 5, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        if (!p_ptr->tim_res_time) set_tim_res_time(dam + randint(10));
                        break;
                case 7: /* between jump */
                {
                        int ii, ij;

                        if (info_spell) return;

                        msg_print("You go between.");
                        if (!tgt_pt(&ii,&ij)) return;
                        p_ptr->energy -= 60 - plev;
                        if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
                           (distance(ij,ii,py,px) > plev*20 + 2))
                        {
                                msg_print("You fail to exit the between correctly!");
                                p_ptr->energy -= 100;
                                teleport_player(10);
                        }
                        else teleport_player_to(ij,ii);
                        break;
                }

                case 8: /* detect monster */
                        if (info_spell) return;
                        detect_monsters_normal();
                        break;
                case 9: /* id */
                        if (info_spell) return;
                        ident_spell();
                        break;
                case 10: /* reveal secrets */
                        if (info_spell) return;
                        detect_traps();
                        detect_doors();
                        detect_stairs();
                        detect_treasure();
                        detect_objects_normal();
                        break;
                case 11: /* probe */
                        if (info_spell) return;
                        probing();
                        break;
                case 12: /* reveal wild */
                        dam = apply_power_dice(3 + (plev / 10), to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " rad %ld", dam);
                                return;
                        }
                        reveal_wilderness_around_player(p_ptr->wilderness_y, p_ptr->wilderness_x, 0, dam);
                        break;
                case 13: /* self know */
                        if (info_spell) return;
                        self_knowledge(NULL);
                        break;
                case 14: /* *id* */
                        if (info_spell) return;
                        identify_fully();
                        break;
                case 15: /* clairvoyance */
                        if (info_spell) return;
                        wiz_lite();
                        break;

                case 16: /* Radiate force */
                {
                        int i;

                        dam = apply_power_dam(10 + (plev * 2), to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam 8*%ld", dam);
                                return;
                        }
                        for (i = 1; i < 10; i++)
                        {
                                if (i - 5) fire_beam(GF_FORCE, i, dam);
                        }

                        break;
                }
                case 17: /* telekinesis */
                        if (info_spell) return;
                        if (!get_aim_dir(&dir)) return;
                        fetch(dir, plev, FALSE);
                        break;
                case 18: /* gravitic hole */
                        rad = apply_power_dice(3 + (plev / 10), to_s, 1);
                        dam = apply_power_dam(20 + plev, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        fire_ball(GF_GRAVITY, 0, dam, rad);
                        break;
                case 19: /* project force */
                        rad = apply_power_dice(1 + (plev / 12), to_s, 1);
                        dam = apply_power_dam(100 + (3 * plev), to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_FORCE, dir, dam, rad);
                        break;
                case 20: /* elem shield */
                        dam = apply_power_dur(20, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d20", dam);
                                return;
                        }
                        (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + dam);
                        (void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + dam);
                        (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + dam);
                        (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + dam);
                        (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + dam);
                        break;
                case 21: /* wipe */
                        if (info_spell) return;
                        wipe(py, px, 5 + (plev / 10));
                        break;
                case 22: /* inertia wave */
                        dam = apply_power_dam(100 + (plev * 2), to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        project_hack(GF_INERTIA, dam);
                        break;
                case 23: /* disrupt shield */
                        dam = apply_power_dice(4, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d6", dam);
                                return;
                        }
                        set_disrupt_shield(dam + randint(6));
                        break;

                case 24: /* charm monster */
                        dam = apply_power_dam(plev, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        (void) charm_monster(dir, dam);
                        break;
                case 25: /* disrupt mind */
                        dam = apply_power_dam(100 + (plev * 3), to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_OLD_CONF, dir, dam);
                        fire_bolt(GF_STUN, dir, dam);
                        break;
                case 26: /* sleep */
                        if (info_spell) return;
                        (void)sleep_monsters();
                        break;
                case 27: /* awareness */
                        dam = apply_power_dur(25, to_s, 5);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d30", dam);
                                return;
                        }
                        (void)set_tim_esp(p_ptr->tim_esp + randint(30) + dam);
                        break;
                case 28: /* psychic surgery */
                        if (info_spell) return;
                        restore_level();
                        set_afraid(0);
                        (void)do_res_stat(A_WIS);
                        (void)do_res_stat(A_INT);
                        break;
                case 29: /* mass charm */
                        dam = apply_power_dam(200, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                        charm_monsters(dam);
                        break;
                case 30: /* disrupts minds */
                        dam = apply_power_dam(100 + (plev * 2), to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        project_hack(GF_OLD_CONF, dam);
                        project_hack(GF_STUN, dam);
                        break;
                case 31: /* mental blast */
                        dam = apply_power_dam(200 + (plev * 2), to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        project_hack(GF_PSI, dam);
                        break;

                default:
                        msg_format("You cast an unknown Spirit spell: %d.", spell);
                        msg_print(NULL);
        }
}

/*
 * Basicaly cast the given spell of the given realm
 */
void cast_spell(int realm, int spell, byte level)
{
		switch (realm)
		{
                case REALM_VALARIN: /* * VALARIN * */
                        cast_valarin_spell(spell, level);
			break;
                case REALM_MAGERY: /* * MAGERY * */
                        cast_magery_spell(spell, level);
			break;
                case REALM_SHADOW: /* * SHADOW * */
                        cast_shadow_spell(spell, level);
			break;
                case REALM_CRUSADE: /* CRUSADE */
                        cast_crusade_spell(spell, level);
			break;
                case REALM_SIGALDRY: /* SIGALDRY */
                        cast_sigaldry_spell(spell, level);
			break;
                case REALM_SYMBIOTIC: /* SYMBIOTIC */
                        cast_symbiotic_spell(spell, level);
			break;
                case REALM_MUSIC: /* MUSIC */
                        cast_music_spell(spell, level);
			break;
                case REALM_MAGIC: /* MAGIC */
                        cast_magic_spell(spell, level);
			break;
                case REALM_PRAYER: /* PRAYER */
                        cast_prayer_spell(spell, level);
			break;
                case REALM_ILLUSION: /* ILLUSION */
                        cast_illusion_spell(spell, level);
			break;
                case REALM_TRIBAL: /* TRIBAL */
                        cast_tribal_spell(spell, level);
			break;
                case REALM_DRUID: /* DRUIDISTIC */
                        cast_druid_spell(spell, level);
			break;
                case REALM_SPIRIT: /* SPIRIT */
                        cast_spirit_spell(spell, level);
			break;
		default:
                        msg_format("You cast a spell from an unknown realm: realm %d, spell %d, level %d.", realm, spell, level);
			msg_print(NULL);
		}
}

/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int	item, sval, spell, realm;
        int     chance, perc;

        const cptr prayer = ((mp_ptr->spell_book == TV_VALARIN_BOOK) ? "prayer" : (mp_ptr->spell_book == TV_MUSIC_BOOK) ? "song" : "spell");

	object_type	*o_ptr;

	magic_type	*s_ptr;

	cptr q, s;

        /* No magic */
        if (p_ptr->antimagic)
        {
                msg_print("Your anti-magic field disrupts any magic attempts.");
                return;
        }

	/* Require spell ability */
	if (!p_ptr->realm1)
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

        realm = o_ptr->tval - TV_VALARIN_BOOK + 1;

	/* Ask for a spell */
        if (!get_spell(&spell, ((mp_ptr->spell_book == TV_VALARIN_BOOK) ? "recite" : "cast"),
                sval, TRUE, o_ptr, TRUE))
	{
		if (spell == -2)
			msg_format("You don't know any %ss in that book.", prayer);
		return;
	}


	/* Access the spell */
        s_ptr = &realm_info[realm][spell];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_format("You do not have enough mana to %s this %s.",
                        ((mp_ptr->spell_book == TV_VALARIN_BOOK) ? "recite" : "cast"),
			prayer);

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
        chance = spell_chance(spell, realm);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
                char sfail[80];

		if (flush_failure) flush();

                get_rnd_line("sfail.txt",sfail);

                msg_format("A cloud of %s appears above you.", sfail);
		sound(SOUND_FAIL);

		if (o_ptr->tval == TV_MUSIC_BOOK && (randint(100)<spell))
		{
                        msg_print("You produce a horrible shriek!");
                        shriek_effect();
		}
	}

	/* Process spell */
	else
	{
		/* Spells.  */
                cast_spell(realm, spell, get_spell_level(realm, spell) - 1);

		/* A spell was cast */
                if ((!(spell_worked[realm][(spell < 32)] & (1L << (spell % 32)))) && (p_ptr->pclass != CLASS_SORCERER))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
                        spell_worked[realm][(spell < 32)] |= (1L << (spell % 32));

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);
		}
	}

	/* Take a turn */
        /* UGLY hack */
        if (is_magestaff()) energy_use = 80;
        else energy_use = 100;
        
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

