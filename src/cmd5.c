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
 * Hook to determine if an object is drainable
 */
static bool item_tester_hook_scroll_amulet(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_AMULET) || (o_ptr->tval == TV_SCROLL)) return (TRUE);

	/* Assume not */
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
static int get_spell(int *sn, cptr prompt, int sval, bool known, object_type *o_ptr)
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
                if (spell_okay(*sn, known, realm))
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
                if (spell_okay(spells[i], known, realm)) okay = TRUE;
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
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				/* Show list */
				redraw = TRUE;

				/* Save the screen */
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
                if (!spell_okay(spell, known, realm))
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
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
                                prompt, spell_names[realm][spell%64],
                                s_ptr->smana, spell_chance(spell,realm));

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}


	/* Restore the screen */
	if (redraw) Term_load();


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
        int             sval;
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
	Term_save();
	
	/* Display the spells */
        print_spells(spells, num, 1, 13, o_ptr->tval - TV_VALARIN_BOOK + 1);
	
	/* Clear the top line */
	prt("", 0, 0);
	
	/* Prompt user */
	put_str("[Press any key to continue]", 0, 23);
	
	/* Wait for key */
	(void)inkey();
	
	/* Restore the screen */
	Term_load();
}


void do_cmd_browse(void)
{
        int             item;
	
	cptr q, s;
	
	object_type	*o_ptr;

        if(p_ptr->pclass == CLASS_POWERMAGE)
        {
                random_spell *s_ptr;
                s_ptr = select_spell(TRUE);
                if(s_ptr == NULL) return;
                msg_format("%s : %s",s_ptr->name,s_ptr->desc);
                return;
        }

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
        if (mp_ptr->spell_book != TV_VALARIN_BOOK)
	{
		/* Ask for a spell, allow cancel */
                if (!get_spell(&spell, "study", sval, FALSE, o_ptr) && (spell == -1)) return;
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
	
	/* Learn the spell */
        spell_learned[o_ptr->tval - TV_VALARIN_BOOK + 1][(spell < 32)] |= (1L << (spell % 32));
	
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
                p, spell_names[o_ptr->tval - TV_VALARIN_BOOK + 1][spell%64]);
	
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
			/* Polymorph into a less mutated form */
			power -= 10;
			
			lose_mutation(0);
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
		p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;
		
		/* Calculate the height/weight for males */
		if (p_ptr->psex == SEX_MALE)
		{
			p_ptr->ht = randnor(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
			p_ptr->wt = randnor(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
		}
		
		/* Calculate the height/weight for females */
		else if (p_ptr->psex == SEX_FEMALE)
		{
			p_ptr->ht = randnor(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
			p_ptr->wt = randnor(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
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
			take_hit(damroll(randint(10),p_ptr->lev), "a lethal mutation");
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
		(void) gain_random_mutation(0);
	}
	
	if (power > rand_int(5))
	{
		power -= 5;
		do_poly_wounds();
	}
	
	/* Note: earlier deductions may have left power < 0 already. */
	while (power > 0)
	{
		mutate_player();
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
			o_ptr->name2 = EGO_TRUMP;
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
                        take_hit(damroll(p_ptr->lev/5,8),"a too vibrating sound");
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
		(void) gain_random_mutation(0);
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



static void cast_valarin_spell(int spell)
{
	int	dir;
        int     plev = p_ptr->lev, i;
	bool    no_trump = FALSE;
        int to_s2=p_ptr->to_s/2;
        int mto_s2=p_ptr->to_s/2;

        mto_s2 = (mto_s2==0)?1:mto_s2;
	
	switch (spell)
	{
	   case 0: /* Detect Evil */
                       (void)detect_monsters_evil();
		       break;
           case 1: /* Call Light */
                       (void)lite_area(damroll(2, (plev / 2)), (plev / 10)*mto_s2 + 1);
		       break;
	   case 2: /* Bless */
                       (void)set_blessed(p_ptr->blessed + randint(12)+to_s2 + 12);
		       break; 
	   case 3: /* Remove Fear */
                       (void)set_afraid(0);
		       break;
           case 4: /* Cure Light Wounds */
                        (void)hp_player(damroll(2, 10)+to_s2);
			(void)set_cut(p_ptr->cut - 10);
		       break;
           case 5: /* Slow Poison */
                        (void)set_poisoned((p_ptr->poisoned / 2) - to_s2);
		       break;
           case 6: /* Detect Secret Doors */
			(void)detect_doors();
			(void)detect_stairs();
		       break;
           case 7: /* Detect Traps */
			(void)detect_traps();
		       break;

           case 8: /* Spear of Light */
                       if (!get_aim_dir(&dir)) return;
                       msg_print("A line of blue shimmering light appears.");
                       lite_line(dir);
                       break;
           case 9: /* Remove Curse */
			remove_curse();
		       break;
           case 10: /* Cure Serious Wounds */
                        (void)hp_player(damroll(6, 10)+to_s2);
			(void)set_cut(0);
		       break;
           case 11: /* Satisfy Hunger */
			(void)set_food(PY_FOOD_MAX - 1);
		       break;
           case 12: /* Holy Chant */
                       set_blessed(p_ptr->blessed + randint(24) + 12 + to_s2);
		       break;
           case 13: /* Sense Surroundings */
                       map_area();
		       break;
           case 14: /* Neutralize Poison */
                       set_poisoned(0);
		       break;
           case 15: /* resist fire&cold */
                       set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10 + to_s2);
                       set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10 + to_s2);
                       break;
           case 16: /* Cure critical wounds */
                        (void)hp_player(damroll(8, 10)+to_s2);
                        (void)set_stun(0);
			(void)set_cut(0);
                       break;
           case 17: /* Orb or Draining */
                       if (!get_aim_dir(&dir)) return;
                       fire_ball(GF_HOLY_FIRE, dir, (damroll(3, 6) + plev + (plev / ((p_ptr->pclass == 2 || p_ptr->pclass == CLASS_HIGH_MAGE) ? 2 : 4)))*mto_s2,
                                        ((plev < 30) ? 2 : 3)+to_s2);
		       break;
           case 18: /* Portal */
                        teleport_player(200 * mto_s2);
			break;
       case 19: /* Sense Unseen */
                       set_tim_invis(p_ptr->tim_invis + 20 + randint(20) + p_ptr->to_s);
                       break;
       case 20: /* Earthquake */
                       earthquake(py, px, 8 + to_s2);
		       break;
           case 21: /* Turn Undead */
			if (!get_aim_dir(&dir)) return;
                        fire_beam(GF_TURN_UNDEAD, dir, damroll(5, 10) * mto_s2);
			break;
           case 22: /* Perception */
                       ident_spell();
		       break;
           case 23: /* Holy Aura */
           {
                       int t = randint(25);
                       (void)set_holy(p_ptr->holy + t + to_s2 + 3 * plev);
                       (void)set_protevil(p_ptr->protevil + t + 3 * plev + to_s2);
		       break;
           }
           case 24: /* Heroism */
                        (void)set_hero(p_ptr->hero + randint(20)+ p_ptr->to_s + 20);
		       break;
           case 25: /* Word of recall */
                       recall_player();
		       break;
           case 26: /* Dispel undead */
                       dispel_undead(60 * mto_s2);
                       break;
           case 27: /* Heal */
                       (void)hp_player(150 * mto_s2);
                       (void)set_stun(0);
                       (void)set_cut(0);
		       break;
           case 28: /* Banish */
                       project_hack(GF_AWAY_ALL, 100 * mto_s2);
		       break;
           case 29: /* Dispel evil */
                       dispel_evil(120 * mto_s2);
		       break;
           case 30: /* Holy Word */
                        (void)dispel_evil(plev * 4 * mto_s2);
                        (void)hp_player(500 * mto_s2);
			(void)set_afraid(0);
			(void)set_poisoned(0);
			(void)set_stun(0);
			(void)set_cut(0);
		       break;
                case 31: /* animal taming */
                        if (!get_aim_dir(&dir)) return;
                        (void) charm_animal(dir, plev * mto_s2);
                        break;
                case 32: /* stone to mud */
			if (!get_aim_dir(&dir)) return;
			(void)wall_to_mud(dir);
                        break;
                case 33: /* sterilization */
                        set_no_breeders(no_breeds + randint(10) + 10 + p_ptr->to_s);
                        break;
                case 34: /* summon animal */
                        if (!(summon_specific_friendly(py, px, plev+to_s2, SUMMON_ANIMAL_RANGER, TRUE)))
                                no_trump = TRUE;
                        break;
                case 35: /* haste self */
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev) ) + plev+to_s2);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5)+to_s2);
			}
                        break;
                case 36: /* call hounds */
                        msg_format("You magically summon hounds.");
                        for (i = 0; i < 6; i++)
                        {
                                summon_specific_friendly(py, px, dun_level + p_ptr->to_s, SUMMON_HOUND, TRUE);
                        }
                case 37: /* whirlwind attack */
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
                        (void)destroy_doors_touch();
                        break;
                case 39: /* sanctuary */
                        (void)sleep_monsters_touch();
                        break;
                case 40: /* dispel curse */
                        remove_all_curse();
                        break;
                case 41: /* tread water */
                        set_walk_water(p_ptr->walk_water + plev + randint(plev) + p_ptr->to_s);
                        break;
                case 42: /* healing */
                        (void)hp_player(300 + (p_ptr->to_s * 3));
			(void)set_stun(0);
			(void)set_cut(0);
			(void)set_poisoned(0);
                        (void)set_image(0);
                        (void)set_afraid(0);
                        break;
                case 43: /* bless weapon */
                        brand_weapon(6);
                        break;
                case 44: /* glyph of warding */
                        warding_glyph();
                        break;
                case 45: /* invis */
                        (void)set_invis(p_ptr->tim_invisible + randint(24) + p_ptr->to_s, 30);
                        break;
                case 46: /* CMW */
                        (void)hp_player(damroll(8 * mto_s2, 10));
                        (void)set_stun(0);
                        (void)set_cut(0);
                        break;
                case 47: /* mass sleep */
			(void)sleep_monsters();
                        break;
                case 48: /* remembrance */
                        (void)restore_level();
                        break;
                case 49: /* restoration */
                        (void)do_res_stat(A_STR);
                        (void)do_res_stat(A_INT);
                        (void)do_res_stat(A_WIS);
                        (void)do_res_stat(A_DEX);
                        (void)do_res_stat(A_CON);
                        (void)do_res_stat(A_CHR);
                        break;
                case 50: /* mass charm */
                        charm_monsters(100 * mto_s2);
                        break;
                case 51: /* wraithform */
                        set_shadow(p_ptr->wraith_form + randint(plev/2) + (plev/2)+to_s2);
                        break;
                case 52: /* chain lightning */
                        for (dir = 0; dir <= 9; dir++)
                                fire_beam(GF_ELEC, dir, damroll(5+(plev/10), 8) * mto_s2);
                        break;
                case 53: /* disintegration */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_DISINTEGRATE, dir,
                                  80 + (plev) * mto_s2, 3 + (plev/40) + to_s2);
                        break;
                case 54: /* blizard */
                        if (!get_aim_dir(&dir)) return;
			fire_ball(GF_COLD, dir,
                                70 + (plev)*mto_s2, (plev/12) + to_s2+1);
                        break;
                case 55: /* whirlpool */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_WATER, dir,
                                100 + (plev)*mto_s2, (plev/12) + to_s2+1);
                        break;
                case 56: /* lightning storm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_ELEC, dir,
                                90 + (plev)*mto_s2, (plev/12) + to_s2+1);
                        break;
                case 57: /* meteor swarm */
           {
		       int x, y, dx, dy, d, count = 0;
		       int b = 10 + randint(10); 
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
               project(0, 2, y, x, (plev*3+to_s2)/2, GF_METEOR, PROJECT_KILL|PROJECT_JUMP|PROJECT_ITEM);
		       }
		   }
                        break;
                case 58: /* infusion */
                       (void)recharge(plev * 2 + (p_ptr->to_s * 2));
                        break;
                case 59: /* alter reality */
                        alter_reality();
                        break;
                case 60: /* restore life */
			(void)restore_level();
                        hp_player(3000 * mto_s2);
                        break;
                case 61: /* call angel */
                {
                        msg_format("You magically summon an angel.");
                        summon_specific_friendly(py, px, dun_level + p_ptr->to_s, SUMMON_ANGEL, TRUE);
                        break;
                }
                case 62: /* earendil's star */
                        fire_ball(GF_LITE, 0, 150 * mto_s2, 8 + to_s2);
                        wiz_lite();
                        if (((p_ptr->prace == RACE_VAMPIRE)||(p_ptr->mimic_form == MIMIC_VAMPIRE)) && !(p_ptr->resist_lite))
                        {
                               msg_print("The starlight scorches your flesh!");
                               take_hit(50, "starlight");
                        }
                        break;
                case 63: /* divinity */
                        if(p_ptr->mimic_form != MIMIC_VALAR)
                                set_mimic(8 + randint(5) + to_s2, MIMIC_VALAR);
                        else
                                set_mimic(p_ptr->tim_mimic + 8 + randint(5) + to_s2, MIMIC_VALAR);
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



static void cast_magery_spell(int spell)
{
        int     dir, beam;
        int     plev = p_ptr->lev;
        int to_s2=p_ptr->to_s/2;
        int mto_s2=p_ptr->to_s/2;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;
	
	switch (spell)
	{
                case 0: /* Magic Missile */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                                                  damroll(3 + ((plev - 1) / 5), 4 * mto_s2));
                        break;
	   case 1: /* Phase Door */
                        teleport_player(10 * mto_s2);
		       break;
           case 2: /* Detect Monsters */
			(void)detect_monsters_normal();
		       break;
           case 3: /* Detect Traps */
			(void)detect_traps();
		       break; 
           case 4: /* Light Area */
                        (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1+to_s2);
                       break;
           case 5: /* Detect Doors/Stairs */
                        (void)detect_doors();
                        (void)detect_stairs();
		       break; 
           case 6: /* Confuse Monster */
			if (!get_aim_dir(&dir)) return;
                        (void)confuse_monster(dir, ( plev * 3) / 2 * mto_s2);
			break;
                case 7: /* Scan Object */
                        psychometry();
                        break;

                case 8: /* Noxious Cloud */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_POIS, dir,
                                10 + (plev / 2)*mto_s2, 2+to_s2);
                        break;
                case 9: /* Teleport */
                        teleport_player(plev * 5 * mto_s2);
                        break;
                case 10: /* Beam of Light */
                        if (!get_aim_dir(&dir)) return;
                        msg_print("A line of blue shimmering light appears.");
                        lite_line(dir);
                        break;
                case 11: /* Sleep Monster */
                        if (!get_aim_dir(&dir)) return;
                        (void)sleep_monster(dir);
                        break;
                case 12: /* Lightning Bolt */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                                  damroll(5+((plev-5)/4), 8 * mto_s2));
                        break;
                case 13: /* Stone to Mud */
			if (!get_aim_dir(&dir)) return;
			(void)wall_to_mud(dir);
                        break;
                case 14: /* Frost Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam-10, GF_COLD, dir,
                                damroll(6+((plev-5)/4) * mto_s2, 8));
                        break;
                case 15: /* Recharging */
                        (void)recharge(30 + to_s2);
                        break;

                case 16: /* Ethereal Eye */
                        map_area();
                        set_tim_invis(p_ptr->tim_invis + 20 + randint(20) + p_ptr->to_s);
                        break;
                case 17: /* Fire Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
                                damroll(9+((plev-5)/4), 8 * mto_s2));
                        break;
                case 18: /* Identify */
			(void)ident_spell();
                        break;
                case 19: /* Typhoon Daze */
                        fire_ball(GF_CONFUSION, 0, 30 * mto_s2, 2 + to_s2);
                        break;
                case 20: /* Time Distortion */
                        slow_monsters();
                        break;
                case 21: /* Haste Self */
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev) ) + plev + to_s2);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5) + to_s2);
			}
                        break;
                case 22: /* Elemental Blast */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
                                damroll(5+((plev-5)/4), 8 * mto_s2));
                        fire_bolt_or_beam(beam, GF_COLD, dir,
                                damroll(5+((plev-5)/4), 8 * mto_s2));
                        fire_bolt_or_beam(beam, GF_ACID, dir,
                                damroll(5+((plev-5)/4), 8 * mto_s2));
                        fire_bolt_or_beam(beam, GF_ELEC, dir,
                                damroll(5+((plev-5)/4), 8 * mto_s2));
		       break;
                case 23: /* Teleport Away */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_beam(GF_AWAY_ALL, dir, plev);
                        break;

                case 24: /* Scan monster */
                {
                        monster_type *m_ptr;
                        monster_race *r_ptr;
                        int m;

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
                        set_meditation(p_ptr->meditation + 20 + randint(20) + p_ptr->to_s);
                        break;
                case 26: /* Gravitic Distortion */
                        fire_ball(GF_GRAVITY, 0, 10 * mto_s2, 2 + to_s2);
                        break;
                case 27: /* Dopplegangaer */
                {
                        int ii, ij;

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
                       if (!get_aim_dir(&dir)) return;
                       (void)fire_ball(GF_FIRE, dir, 100 * mto_s2, 2 + to_s2);
		       break;
                case 29: /* Force Shield */
                       (void)set_shield(p_ptr->shield + randint(20) + 30 + to_s2, 50 + to_s2);
		       break;
                case 30: /* Crippled Gaze */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_STUN_CONF, dir, 100 * mto_s2, 0);
                        (void)fire_ball(GF_OLD_SLOW, dir, 100 * mto_s2, 0);
                        break;
                case 31: /* Collapse Cieling */
                {
                        int y, x;

                       if (!tgt_pt(&x, &y)) break;
                       earthquake(y, x, 5 + to_s2);
		       break;
                }

                case 32: /* Gravitic Beam */
                       if (!get_aim_dir(&dir)) return;
                       (void)fire_beam(GF_GRAVITY, dir, 20 * mto_s2);
		       break;
                case 33: /* Sanctuary */
                       wall_stone();
		       break;
                case 34: /* Starbust */
                {
                        int i;

                        for (i = 1; i < 10; i++)
                        {
                                if (i - 5) fire_beam(GF_LITE, i, 90 * mto_s2);
                        }
                        break;
                }
                case 35: /* Statis Cage */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_OLD_SLEEP, dir, 100 * mto_s2, 0);
                        break;
                case 36: /* Elemental Shield */
                        (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20 + to_s2);
                        (void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20 + to_s2);
                        (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20 + to_s2);
                        (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20 + to_s2);
                        (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20 + to_s2);
                        break;
                case 37: /* Mirror Guard */
                        set_tim_reflect(p_ptr->tim_reflect + 10 + randint(10) + to_s2);
                        break;
                case 38: /* Flare Gaze */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball_beam(GF_FIRE, dir, 100 * mto_s2, 3 + to_s2);
                        break;
                case 39: /* Force of the Elements */
                {
                        int i;

                        for (i = 1; i < 10; i++)
                        {
                                if (i-5)
                                {
                                        if(magik(50)) fire_bolt(GF_FIRE, i, damroll(15, 8 * mto_s2));
                                        if(magik(50)) fire_bolt(GF_COLD, i, damroll(14, 8 * mto_s2));
                                        if(magik(50)) fire_bolt(GF_ELEC, i, damroll(12, 8 * mto_s2));
                                        if(magik(50)) fire_bolt(GF_ACID, i, damroll(18, 8 * mto_s2));
                                }
                        }
                        break;
                }

                case 40: /* Earthquake */
                        earthquake(py, px, 5 + to_s2);
                        break;
                case 41: /* Polymorph */
			if (!get_aim_dir(&dir)) return;
			(void)poly_monster(dir);
                        break;
                case 42: /* Wall of Stone */
                        if (!get_aim_dir(&dir)) return;
                        project_hook(GF_STONE_WALL, dir, 1, PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID);
                        break;
                case 43: /* Warp Space */
                        fire_ball(GF_GRAVITY, 0, 60 * mto_s2, 4 + to_s2);
                        break;
                case 44: /* Chaos Blast */
                        fire_ball(GF_CHAOS, 0, (100 + plev) * mto_s2, 5 + to_s2);
                        break;
                case 45: /* Lava Flow */
                        if (!get_aim_dir(&dir)) return;
                        project_hook(GF_LAVA_FLOW, dir, 5 + (plev / 7) + to_s2, PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID);
                        break;
                case 46: /* Pyrrhic Blast */
                        fire_ball(GF_MANA, 0, 600 * mto_s2, 3 + to_s2);
                        take_hit(200, "the heat of a Pyrrhic Blast");
                        break;
                case 47: /* Word of Destruction */
                        destroy_area(py, px, 15+(p_ptr->to_s*2), TRUE);
                        break;

                case 48: /* Radiate Fear */
                        fire_ball(GF_FEAR, 0, 20 + to_s2, 10 + to_s2);
                        break;
                case 49: /* Probing */
                        probing();
                        break;
                case 50: /* Forcefull Graze */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_bolt(GF_STUN, dir, 50 + to_s2);
                        break;
                case 51: /* Recharging II */
                        recharge(40 + p_ptr->to_s);
                        break;
                case 52: /* Transmutation */
                        alchemy();
                        break;
                case 53: /* Self Scan */
                        self_knowledge(NULL);
                        identify_pack();
                        p_ptr->notice |= PN_COMBINE | PN_REORDER;
                        break;
                case 54: /* Id II */
                        identify_fully();
                        break;
                case 55: /* Clairvoyance */
                        wiz_lite_extra();
                        break;

                case 56: /* Volcano Flow */
                        if (!get_aim_dir(&dir)) return;
                        project_hook(GF_LAVA_FLOW, dir, 9 + (plev / 7) + to_s2, PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID);
                        (void)fire_beam(GF_PLASMA, dir, damroll(15, 7 * mto_s2));
                        (void)fire_beam(GF_PLASMA, dir, damroll(15, 7 * mto_s2));
                        break;
                case 57: /* Plasma Eruption */
                        fire_ball(GF_PLASMA, 0, 400 * mto_s2, 6 + to_s2);
                        break;
                case 58: /* Annihilate */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_bolt(GF_MISSILE, dir, 500 * mto_s2);
                        break;
                case 59: /* Olbivion Blast */
                        mass_genocide(TRUE);
                        break;
                case 60: /* Mana Spin */
                {
                        int i;

                        for (i = 1; i < 10; i++)
                        {
                                if (i-5)
                                {
                                        fire_bolt(GF_MANA, i, damroll(10, 10 * mto_s2));
                                        fire_bolt(GF_MANA, i, damroll(10, 10 * mto_s2));
                                        fire_bolt(GF_MANA, i, damroll(10, 10 * mto_s2));
                                }
                        }
                        break;
                }
                case 61: /* Tidal Wave */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_WATER, dir, 450 * mto_s2, 3 + to_s2);
                        break;
                case 62: /* Anarchy Strike */
                {
                        int i, j;

                        for(j = 0; j < 5; j++)
                        {
                                i = randint(10);
                                while(i == 5) i = randint(10);
                                (void)fire_ball(GF_CHAOS, i, 250 * mto_s2, 3 + to_s2);
                        }
                        break;
                }
                case 63: /* Mana Strike */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_MANA, dir, 1200 * mto_s2, 0);
                        break;
           default:
                   msg_format("You cast an unknown Magery spell: %d.", spell);
		   msg_print(NULL);
	   }
}

/* Such an usefull hack :) */
static bool check_ring(int art_type)
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

static void cast_shadow_spell(int spell)
{
        int     dir, beam;
        int     plev = p_ptr->lev;
        int to_s2=p_ptr->to_s/2;
        int mto_s2=p_ptr->to_s/2;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
                case 0: /* Dark Hand */
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_DARK, dir,
                               damroll(3 + ((plev - 1) / 5), 4 * mto_s2));
                        break;
                case 1: /* Sense unseen */
                        detect_monsters_invis();
                        break;
                case 2: /* dark light */
                        lite_room(py, px);
                        break;
                case 3: /* armor of void */
                        set_shield(p_ptr->shield + 10 + randint(5) + to_s2, 10 + to_s2);
                        break;
                case 4: /* fear of the shadows */
                        if (!get_aim_dir(&dir)) return;
                        fear_monster(dir, plev * mto_s2);
                        break;
                case 5: /* sense shadows */
                        detect_objects_normal();
                        break;
                case 6: /* shadow spikes */
                {
                        int i;

                        for (i = 1; i < 10; i++)
                        {
                                if (i - 5) fire_beam(GF_DARK, i, (6 + plev) * mto_s2);
                        }
                        break;
                }
                case 7: /* shadow hidding */
                        set_invis(p_ptr->tim_invis + 5 + randint(10) + p_ptr->to_s, 30);
                        break;

                case 8: /* Illusory Ball */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_OLD_CONF, dir, 13 * mto_s2, 2 + (plev / 20) + to_s2);
                        break;
                case 9: /* Darkness Storm */
                        fire_ball(GF_DARK, 0, (20 + plev) * mto_s2, 3 + (plev / 20) + to_s2);
                        break;
                case 10: /* Energies Channeling */
                        map_area();
                        break;
                case 11: /* Immaterial Beings */
                        summon_specific_friendly(py, px, plev + to_s2, SUMMON_GHOST, TRUE);
                        break;
                case 12: /* Shadow Enlightment */
                        ident_spell();
                        break;
                case 13: /* Disolve Mater */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_FORCE, dir, (50 + plev) * mto_s2, 0 + to_s2);
                        break;
                case 14: /* Invoke the Night */
                        if (!get_aim_dir(&dir)) return;
                        sleep_monster(dir);
                        break;
                case 15: /* Resist L&D */
                        set_oppose_ld(p_ptr->oppose_ld + 10 + randint(10) + to_s2);
                        break;

                case 16: /* Teleportation */
                        teleport_player(200 + p_ptr->to_s);
                        break;
                case 17: /* Absorb Light */
                {
                        int y, x, light = 0;
                        cave_type *c_ptr;

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
                        p_ptr->csp += light * (5 + to_s2);
                        if (p_ptr->csp > p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);
			}
                        msg_print("Your feel your head clearer.");
                        break;
                }
                case 18: /* Shadow Regeneration */
                        hp_player(damroll(5 + (plev / 5), 8 + to_s2));
                        break;
                case 19: /* Steal Shadow */
                        dispel_monsters((60 + plev) * mto_s2);
                        take_hit(60, "the stealing of your shadow");
                        break;
                case 20: /* Storm in the Shadow */
                        project_meteor(3, GF_DARK, damroll(5 + to_s2, 20), PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_THRU);
                        break;
                case 21: /* Shadow of Life */
                {
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_RAISE, dir, 1, 0);
                        break;
                }
                case 22: /* Shadow of Dragon */
                        summon_specific_friendly(py, px, plev + to_s2, SUMMON_DRAGON, TRUE);
                        break;
                case 23: /* Banish Shadows */
                        dispel_monsters(180 * mto_s2);
                        break;

                case 24: /* Feelings of Darkness */
			probing();
                        break;
                case 25: /* Dark Bolt */
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_DARK, dir, (230 + plev) * mto_s2);
                        break;
                case 26: /* Shadow orb */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_DARK, dir, (200 + plev) * mto_s2, 2 + to_s2);
                        break;
                case 27: /* Shadow of Undead */
                        summon_specific_friendly(py, px, plev + to_s2, SUMMON_UNDEAD, TRUE);
                        break;
                case 28: /* Shadow Rain */
                {
                        int y, x, i;

                        for(i = 0; i < 500 + (20 * to_s2); i++)
                        {
                                y = randint(cur_hgt) - 1;
                                x = randint(cur_wid) - 1;

                                project(0, 1, y, x, 50 * mto_s2, GF_DARK,
                                     PROJECT_STOP | PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_JUMP);
                        }
                        break;
                }
                case 29: /* Mirror of Shadow */
                        set_tim_reflect(p_ptr->tim_reflect + 5 + randint(10) + to_s2);
                        break;
                case 30: /* Comet from the Void */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_FORCE, dir, 400 * mto_s2, 2 + to_s2);
                        break;
                case 31: /* Call the Void */
                        call_the_();
                        break;

                case 32: /* Examin Shadow */
                {
                        monster_type *m_ptr;
                        monster_race *r_ptr;
                        int m;

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
                        identify_fully();
                        break;
                case 34: /* Remove Curse */
                        remove_curse();
                        break;
                case 35: /* *Remove Curse* */
                        remove_all_curse();
                        break;
                case 36: /* Shadow Form */
                        set_shadow(p_ptr->wraith_form + randint(plev/2) + (plev/2) + to_s2);
                        break;

                case 37: /* Shadow Portal */
                        teleport_player(10 + to_s2);
                        break;
                case 38: /* Warping Rift */
                        fire_ball(GF_GRAVITY, 0, 100 * mto_s2, 4 + to_s2);
                        break;
                case 39: /* Void Jumping */
                        recall_player();
                        break;
                case 40: /* Shadow Distortion */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_GRAVITY, dir, 200 * mto_s2, 3 + to_s2);
                        break;
                case 41: /* Between Jump */
                {
                        int ij, ii;

                        if(special_flag){msg_print("Not on special levels!");break;}

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
                        fire_ball(GF_BETWEEN_GATE, 0, 1, 2 + to_s2);
                        break;

                case 43: /* Dispel Living */
                        dispel_living((60 + plev) * mto_s2);
                        break;
                case 44: /* Conjure Dragons */
                {
                        int i;

                        for(i = 0; i < 5 + to_s2; i++)
                                summon_specific_friendly(py, px, plev + to_s2, SUMMON_DRAGON, TRUE);
                        break;
                }
                case 45: /* Dispel Undead */
                        dispel_undead((120 + plev) * mto_s2);
                        break;
                case 46: /* Conjure Undeads */
                {
                        int i;

                        for(i = 0; i < 5 + to_s2; i++)
                                summon_specific_friendly(py, px, plev + to_s2, SUMMON_UNDEAD, TRUE);
                        break;
                }
                case 47: /* Dispel Demon */
                        dispel_demons((180 + plev) * mto_s2);
                        break;
                case 48: /* Conjure Demons */
                {
                        int i;

                        for(i = 0; i < 5 + to_s2; i++)
                                summon_specific_friendly(py, px, plev + to_s2, SUMMON_DEMON, TRUE);
                        break;
                }

                case 49: /* Recharge I */
                        recharge(20 + p_ptr->to_s);
                        break;
                case 50: /* Sphere of Void */
                        fire_ball(GF_DISINTEGRATE, 0, 320 * mto_s2, 3 + to_s2);
                        break;
                case 51: /* Shadow Alteration */
                        alter_reality();
                        break;
                case 52: /* Recharge II */
                        recharge(40 + p_ptr->to_s);
                        break;
                case 53: /* Concentrate Light */
                {
                        int y, x, light = 0;
                        cave_type *c_ptr;

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
                        msg_print("The light around you is absorbed... and released in a powerfull bolt!");
                        fire_bolt(GF_LITE, dir, damroll(light * mto_s2, p_ptr->lev));
                        break;
                }
                        break;
                case 54: /* Shield of Darkness */
                        (void)set_shield(p_ptr->shield + randint(20) + 30 + to_s2, 50 + to_s2);
                        break;
                case 55: /* Tunnel of Shadow */
                {
                        int min = ((dun_level - 5 - to_s2) < d_info[dungeon_type].mindepth)?d_info[dungeon_type].mindepth:(dun_level - 5 - to_s2);
                        int max = ((dun_level + 5 + to_s2) < d_info[dungeon_type].maxdepth)?d_info[dungeon_type].maxdepth:(dun_level + 5 + to_s2);
                        int dest = dun_level;

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
                        msg_format("Atunnel of shadows is open to the level %d.", dest);

                        if (autosave_l)
                        {
                                is_autosave = TRUE;
                                msg_print("Autosaving the game...");
                                do_cmd_save_game();
                                is_autosave = FALSE;
                        }

                        /* Change level */
                        dun_level = dest;

                        p_ptr->inside_arena = 0;
                        leaving_quest = p_ptr->inside_quest;

                        /* Leaving an 'only once' quest marks it as failed */
                        if (leaving_quest &&
                                (quest[leaving_quest].flags & QUEST_FLAG_ONCE) &&
                                (quest[leaving_quest].status == QUEST_STATUS_TAKEN))
                        {
                                quest[leaving_quest].status = QUEST_STATUS_FAILED;
                        }

                        p_ptr->inside_quest = 0;

                        /* Leaving */
                        p_ptr->leaving = TRUE;
                        break;
                }
                case 56: /* Genocide */
                        genocide(TRUE);
                        break;
                case 57: /* Enslave Undead */
                        fire_ball(GF_CONTROL_UNDEAD, 0, 150 * mto_s2, 4 + to_s2);
                        break;
                case 58: /* Create Minor Ring */
        {
                int item, die = randint(100);
                object_type             *o_ptr;
                cptr q, s;

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
                        if (check_ring(ART_NARYA) && check_ring(ART_NENYA) && check_ring(ART_VILYA))
                        {
                                msg_print("The power of the Elven Rings is released! You are now protected from the passing of the Time.");
                                set_tim_res_time(p_ptr->tim_res_time + 20 + randint(30) + (p_ptr->to_s * 2));
                                restore_level();
                        }
                        else
                                msg_print("You must wear two of the Three Rings of Power(Narya, Nenya and Vilya) to use this spell.");
                        break;
                case 60: /* Protection from Undeads */
                        if(p_ptr->black_breath)
                        {
                                msg_print("You are cured from the Black Breath!");
                                p_ptr->black_breath = FALSE;
                        }
                        set_protundead(p_ptr->protundead + 20 + randint(30) + to_s2);
                        break;
                case 61: /* Mass Genocide */
                        mass_genocide(TRUE);
                        break;
                case 62: /* Hellfire */
			if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_HELL_FIRE, dir,
                                666 * mto_s2, 3 + to_s2);
                                take_hit(50+randint(50), "the strain of casting Hellfire");
                        break;
                case 63: /* Control The Ring */
                        if(check_ring(ART_POWER))
                        {
                                fire_ball(GF_MANA, 0, 2000 * mto_s2, 5 + to_s2);
                                if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_DARK, dir, 1500 * mto_s2, 2 + to_s2);
                                if (!get_aim_dir(&dir)) return;
                                fire_bolt(GF_FORCE, dir, 1000 * mto_s2);
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


static void cast_chaos_spell(int spell)
{
	int	dir, i, beam;
	int	plev = p_ptr->lev;
        int to_s2=p_ptr->to_s/2;
        int mto_s2=p_ptr->to_s/2;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
		case 0: /* Magic Missile */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                                                  damroll(3 + ((plev - 1) / 5) * mto_s2, 4));
                break;
        case 1: /* Trap / Door destruction, was: Blink */
			(void)destroy_doors_touch();
			break;
        case 2: /* Flash of Light == Light Area */
                        (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1 + to_s2);
			break; 
        case 3: /* Touch of Confusion */
            if (!(p_ptr->confusing))
            {
                msg_print("Your hands start glowing.");
                p_ptr->confusing = TRUE;
            }
			break;
       case 4: /* Manaburst */
             if (!get_aim_dir(&dir)) return;
             fire_ball(GF_MISSILE, dir,
            (damroll(3, 5) + plev +
             (plev / (((p_ptr->pclass == CLASS_MAGE)
                || (p_ptr->pclass == CLASS_HIGH_MAGE)) ? 2 : 4))) * mto_s2,
            ((plev < 30) ? 2 : 3)+to_s2);
          /* Shouldn't actually use GF_MANA, as it will destroy all
       * items on the floor */
             break;
        case 5: /* Fire Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
                                damroll(8+((plev-5)/4), 8)*mto_s2);
			break;
        case 6: /* Fist of Force ("Fist of Fun") */
			if (!get_aim_dir(&dir)) return;
           fire_ball(GF_DISINTEGRATE, dir,
               damroll(8+((plev-5)/4), 8)*mto_s2, to_s2);
            break;
		case 7: /* Teleport Self */
                        teleport_player(plev * 5*mto_s2);
			break;
        case 8: /* Wonder */
           {
           /* This spell should become more useful (more
              controlled) as the player gains experience levels.
              Thus, add 1/5 of the player's level to the die roll.
              This eliminates the worst effects later on, while
              keeping the results quite random.  It also allows
              some potent effects only at high level. */

               int die = randint(100) + plev / 5 + p_ptr->to_s;

               if (!get_aim_dir(&dir)) return;
               if (die > 100)
                   msg_print ("You feel a surge of power!");
               if (die < 8) clone_monster (dir);
               else if (die < 14) speed_monster (dir);
               else if (die < 26) heal_monster (dir);
               else if (die < 31) poly_monster (dir);
               else if (die < 36)
                   fire_bolt_or_beam (beam - 10,
                   GF_MISSILE, dir,
                   damroll(3 + ((plev - 1) / 5), 4)*mto_s2);
               else if (die < 41) confuse_monster (dir, plev*mto_s2);
               else if (die < 46) fire_ball (GF_POIS, dir, 20 + (plev / 2)*mto_s2, 3+to_s2);
               else if (die < 51) lite_line (dir);
               else if (die < 56)
                   fire_bolt_or_beam (beam - 10, GF_ELEC, dir,
                   damroll(3+((plev-5)/4),8)*mto_s2);
               else if (die < 61)
                   fire_bolt_or_beam (beam - 10, GF_COLD, dir,
                   damroll(5+((plev-5)/4),8)*mto_s2);
               else if (die < 66)
                   fire_bolt_or_beam (beam, GF_ACID, dir,
                   damroll(6+((plev-5)/4),8)*mto_s2);
               else if (die < 71)
                   fire_bolt_or_beam (beam, GF_FIRE, dir,
                   damroll(8+((plev-5)/4),8)*mto_s2);
               else if (die < 76) drain_life (dir, 75+(p_ptr->to_s*2));
               else if (die < 81) fire_ball (GF_ELEC, dir, 30 + plev*mto_s2 / 2, 2+to_s2);
               else if (die < 86) fire_ball (GF_ACID, dir, 40 + plev*mto_s2, 2+to_s2);
               else if (die < 91) fire_ball (GF_ICE, dir, 70 + plev*mto_s2, 3+to_s2);
               else if (die < 96) fire_ball (GF_FIRE, dir, 80 + plev*mto_s2, 3+to_s2);
               else if (die < 101) drain_life (dir, 100 + plev+to_s2);
               else if (die < 104) earthquake (py, px, 12+to_s2);
               else if (die < 106) destroy_area (py, px, 15, TRUE);
               else if (die < 108) genocide(TRUE);
               else if (die < 110) dispel_monsters (120+(p_ptr->to_s*2));
               else /* RARE */
               {
                   dispel_monsters (150+(p_ptr->to_s*2));
                   slow_monsters();
                   sleep_monsters();
                   hp_player (300+(p_ptr->to_s*3));
               }
               break;
           }
			break;
		case 9: /* Chaos Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_CHAOS, dir,
				damroll(10+((plev-5)/4), 8));
			break;
        case 10: /* Sonic Boom */
               msg_print("BOOM! Shake the room!");
                   project(0, 2+plev/10, py, px,
               45+plev*mto_s2, GF_SOUND, PROJECT_KILL|PROJECT_ITEM);
                   break;
        case 11: /* Doom Bolt -- always beam in 2.0.7 or later */
				if (!get_aim_dir(&dir)) return;
                fire_beam(GF_MANA, dir, damroll(11+((plev-5)/4), 8)*mto_s2);
			break;
		case 12: /* Fire Ball */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_FIRE, dir,
                                        55 + (plev)*mto_s2, 2+to_s2);
			break;
		case 13: /* Teleport Other */
           if (!get_aim_dir(&dir)) return;
               (void)fire_beam(GF_AWAY_ALL, dir, plev*mto_s2);
			break;
		case 14: /* Word of Destruction */
                        destroy_area(py, px, 15+(p_ptr->to_s*2), TRUE);
			break;
                case 15: /* Invoke chaos */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_CHAOS, dir,
                                        66 + (plev)*mto_s2, (plev / 5)+to_s2);
			break;
        case 16: /* Polymorph Other */
			if (!get_aim_dir(&dir)) return;
			(void)poly_monster(dir);
			break;
        case 17: /* Chain Lightning */
          for (dir = 0; dir <= 9; dir++)
            fire_beam(GF_ELEC, dir, damroll(5+(plev/10), 8)*mto_s2);
           break;
        case 18: /* Arcane Binding == Charging */
                        (void)recharge(40+to_s2);
			break;
        case 19: /* Disintegration */
			if (!get_aim_dir(&dir)) return;
           fire_ball(GF_DISINTEGRATE, dir,
               80 + (plev)*mto_s2, 3 + (plev/40)+to_s2);
               break;
            break;
        case 20: /* Alter Reality */
			msg_print("The world changes!");
                if (autosave_l)
                {
                    is_autosave = TRUE;
                    msg_print("Autosaving the game...");
                    do_cmd_save_game();
                    is_autosave = FALSE;
                }
			/* Leaving */
			p_ptr->leaving = TRUE;

			break;
        case 21: /* Polymorph Self */
            do_poly_self();
	    break;
        case 22: /* Chaos Branding */
		brand_weapon(1);
		break;
        case 23: /* Summon monster, demon */
		if (randint(3) == 1)
		{
                        if (summon_specific(py, px, (plev*3)*mto_s2/2, SUMMON_DEMON))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");
				msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
			}
        	}
		else
		{
                        if (summon_specific_friendly(py, px, (plev*3)*mto_s2/2,
				SUMMON_DEMON, (plev == 50 ? TRUE : FALSE)))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");
				msg_print("'What is thy bidding... Master?'");
			}
		}
		break;
        case 24: /* Beam of Gravity */
			if (!get_aim_dir(&dir)) return;
                fire_beam(GF_GRAVITY, dir, damroll(9+((plev-5)/4), 8)*mto_s2);
            break;
        case 25: /* Meteor Swarm  */
#if 1
           {
		       int x, y, dx, dy, d, count = 0;
		       int b = 10 + randint(10); 
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
               project(0, 2, y, x, (plev*3+to_s2)/2, GF_METEOR, PROJECT_KILL|PROJECT_JUMP|PROJECT_ITEM);
		       }
		   }
	           break;
#else
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_METEOR, dir,
                                65 + (plev)*mto_s2, 3 + (plev/40)+to_s2);
			break;
#endif
		case 26: /* Flame Strike */
			fire_ball(GF_FIRE, 0,
                150 + (2*plev)*mto_s2, 8+to_s2);
			break;
        case 27: /* Call Chaos */
            call_chaos();
			break;
        case 28: /* Magic Rocket */
			if (!get_aim_dir(&dir)) return;
            msg_print("You launch a rocket!");
			fire_ball(GF_ROCKET, dir,
                                        120 + (plev)*mto_s2, 2+to_s2);
			break;
        case 29: /* Mana Storm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_MANA, dir,
                                300 + (plev * 2)*mto_s2, 4+to_s2);
            break;
        case 30: /* Breathe chaos */
               if (!get_aim_dir(&dir)) return;
               fire_ball(GF_CHAOS,dir,p_ptr->chp*mto_s2,
                     2+to_s2);
               break;
                case 31: /* Call the Void */
			call_the_();
			break;
	default:
		msg_format("You cast an unknown Chaos spell: %d.", spell);
		msg_print(NULL);
	}
}


static void cast_nether_spell(int spell)
{
        int     dir;
	int	beam;
	int	plev = p_ptr->lev;
        int to_s2=p_ptr->to_s/2;
        int mto_s2=p_ptr->to_s/2;
        bool no_trump = FALSE;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
        else beam = plev / 2;

	switch (spell)
	{
       case 0: /* Detect Undead & Demons -> Unlife*/
       (void) detect_monsters_nonliving();
		       break;
       case 1: /* Detect Evil */
			(void)detect_monsters_evil();
         break;
       case 2: /* Horrify */
			if (!get_aim_dir(&dir)) return;
                        (void)fear_monster(dir, 5 + plev * mto_s2);
                        (void) stun_monster(dir, 5 + plev * mto_s2);
		       break; 
           case 3: /* Sleep I */
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir);
		       break;
           case 4: /* Reveal Invisible */
                       set_tim_invis(p_ptr->tim_invis + 20 + randint(20) + p_ptr->to_s);
		       break;
           case 5: /* Lethargy */
                       if (!get_aim_dir(&dir)) return;
                       fire_bolt_or_beam(beam, GF_OLD_SLOW, dir,
                                         damroll(3+((plev-5)/4), 6) * mto_s2);
		       break;
       case 6: /* Resist Poison */
                       set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20 + to_s2);
		       break;
       case 7: /* Stinkning cloud */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_POIS, dir,
                                10 + (plev / 2)*mto_s2, 2+to_s2);
               break;

       case 8: /* Resist Cold */
               set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20 + to_s2);
               break;
       case 9: /* Black Dart */
               if (!get_aim_dir(&dir)) return;
               fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                                 damroll(4 + ((plev - 1) / 5), 6 * mto_s2));
               break;
        case 10: /* Invisibility */
        {
                int ii = p_ptr->lev/2 + randint(p_ptr->lev) + p_ptr->to_s;
                set_invis(p_ptr->tim_invisible + ii, 35);
                set_tim_invis(p_ptr->tim_invisible + ii);
                break;
        }
           case 11: /* Spear of Phantasms */
				if (!get_aim_dir(&dir)) return;
                                fire_beam(GF_TURN_ALL, dir,
                                          damroll(5 * mto_s2, 7));
         break;
       case 12: /* Levitation */
                set_tim_ffall(p_ptr->tim_ffall + 20 + randint(20) + p_ptr->to_s);
                break;
       case 13: /* Sleep II */
                       (void)sleep_monsters_touch();
		       break;
           case 14: /* Nether Bolt */
               if (!get_aim_dir(&dir)) return;
               fire_bolt_or_beam(beam, GF_NETHER, dir,
                                 damroll(5 + ((plev - 1) / 5), 8 * mto_s2));
		       break;
           case 15: /* Enslave Undead */
                        if (!get_aim_dir(&dir)) return;
                        (void)control_one_undead(dir, plev);
		       break;
           case 16: /* Icy Touch */
               if (!get_aim_dir(&dir)) return;
               fire_bolt_or_beam(beam, GF_ICE, dir,
                                 damroll(6 + (plev / 5), 8 * mto_s2));
		       break;
       case 17: /* Bloodlust */
               (void)set_shero(p_ptr->shero + randint(25) + 25);
               (void)set_blessed(p_ptr->blessed + randint(25) + 25);
       break;
       case 18: /* Werewolf Form*/
                        if(p_ptr->mimic_form != MIMIC_WEREWOLF)
                                set_mimic(25 + randint(25) + to_s2, MIMIC_WEREWOLF);
                        else
                                set_mimic(p_ptr->tim_mimic + 25 + randint(25) + to_s2, MIMIC_WEREWOLF);
       break;
       case 19: /* Draining Touch */
                   if (!get_aim_dir(&dir)) return;
                   if (drain_life(dir, (p_ptr->lev * 2 * mto_s2)))
                           hp_player(p_ptr->lev + randint(p_ptr->lev * mto_s2));
           break;
        case 20: /* Turn Undead */
				if (!get_aim_dir(&dir)) return;
                                fire_bolt_or_beam(beam, GF_TURN_UNDEAD, dir,
                                          damroll(6 * mto_s2, 8));
                break;
        case 21: /* Sleep III */
                       (void)sleep_monsters();
		       break;
       case 22: /* Call Undead*/
	{
                msg_print("You concentrate on the summoning of an undead creature...");

                if (!(summon_specific_friendly(py, px, plev+to_s2, SUMMON_UNDEAD, TRUE)))
                        no_trump = TRUE;

		break;
	}
        case 23: /* Open Chasm */
        {
                monster_type *m_ptr;
                monster_race *r_ptr;
                cave_type *c_ptr;
                int     ii = 0, ij = 0;


		if (!tgt_pt(&ii,&ij)) return;

                cave_set_feat(ij, ii, FEAT_DARK_PIT);
                msg_print("A chasm appears in the floor!");

                if(cave[ij][ii].m_idx)
                {
                        m_ptr = &m_list[cave[ij][ii].m_idx];
                        r_ptr = race_inf(m_ptr);

                        if(r_ptr->flags7 & RF7_CAN_FLY)
                        {
                                msg_print("The monster flies over the chasm.");
                        }
                        else
                        {
                                if(!(r_ptr->flags1 & RF1_UNIQUE))
                                        msg_print("The monster falls in the chasm !");

                                delete_monster(ij, ii);
                        }
                }

                if(cave[ij][ii].o_idx)
                {
                        s16b this_o_idx, next_o_idx = 0;

                        c_ptr = &cave[ij][ii];

                        /* Scan all objects in the grid */
                        for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
                        {
                                object_type *o_ptr;
                                bool plural = FALSE;

                                char o_name[80];
	
                                /* Acquire object */
                                o_ptr = &o_list[this_o_idx];

                                if(o_ptr->number > 1) plural = TRUE;

                                /* Acquire next object */
                                next_o_idx = o_ptr->next_o_idx;

                                /* Effect "observed" */
                                if (o_ptr->marked)
                                {
                                        object_desc(o_name, o_ptr, FALSE, 0);
                                }

                                /* Artifacts get to resist */
                                if (o_ptr->name1)
                                {
                                        /* Observe the resist */
                                        if (o_ptr->marked)
                                        {
                                                msg_format("The %s %s simply fly over the chasm!",
                                                           o_name, (plural ? "are" : "is"));
                                        }
                                }

                                /* Kill it */
                                else
                                {
                                        /* Delete the object */
                                        delete_object_idx(this_o_idx);

                                        /* Redraw */
                                        lite_spot(ij, ii);
                                }
                        }
               }
               break;
        }
        case 24: /* Dark Bolt */
                       if (!get_aim_dir(&dir)) return;
                       fire_bolt_or_beam(beam, GF_DARK, dir,
                                         damroll(8 * mto_s2, 8));
		       break;
       case 25: /* Vampiric Form */
                        if(p_ptr->mimic_form != MIMIC_VAMPIRE)
                                set_mimic(20 + randint(15) + to_s2, MIMIC_VAMPIRE);
                        else
                                set_mimic(p_ptr->tim_mimic + 20 + randint(15) + to_s2, MIMIC_VAMPIRE);
                        break;
        case 26: /* Raise Dead */
        {
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_RAISE, dir, 1, 0);
        }
       case 27: /* Spear of Darkness */
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_DARK, dir,
                                         damroll(8 * mto_s2, 8 + (plev / 5)));
		       break;
       case 28: /* Banish       */
                       banish_monsters(plev * 3 / 2 * mto_s2);
		       break;
        case 29: /* Dispel Good */
                (void)dispel_good(plev * 4 * mto_s2);
		break;
        case 30: /* Genocide */
		(void)genocide(TRUE);
		break;

        case 31: /* Detect Monsters */
                detect_monsters_normal();
        break;
        case 32: /* Hypnotic Gaze */
                msg_print("Your eyes look mesmerizing...");
                if (get_aim_dir(&dir))
                        (void) charm_monster(dir, p_ptr->lev * mto_s2);
        break;
        case 33: /* Piercing Gaze */
                map_area();
        break;
        case 34: /* Sense Magic */
                psychometry();
        break;
        case 35: /* Detection */
                (void)detect_all();
        break;
        case 36: /* Telepathy */
                (void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25 + to_s2);
        break;
        case 37: /* Clairvoyance */
                wiz_lite();
        break;

        case 38: /* Resist Fire */
                set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20 + p_ptr->to_s);
        break;
        case 39: /* Fiery Aura */
                set_tim_fire_aura(p_ptr->tim_fire_aura + 10 + randint(10) + p_ptr->to_s);
        break;
        case 40: /* Spear of Fire */
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_FIRE, dir,
                                         damroll(7 * mto_s2, 8 + (plev / 5)));
        break;
        case 41: /* Fireball */
                       if (!get_aim_dir(&dir)) return;
                       fire_ball(GF_FIRE, dir,
                                         80 * mto_s2, 2 + (plev / 10) + to_s2);
        break;
        case 42: /* Call Demon */
                msg_format("You magically summon a demon.");
                summon_specific_friendly(py, px, dun_level, SUMMON_DEMON, TRUE);
        break;
        case 43: /* Flame of Udun */
                        if(p_ptr->mimic_form != MIMIC_BALROG)
                                set_mimic(20 + randint(15) + to_s2, MIMIC_BALROG);
                        else
                                set_mimic(p_ptr->tim_mimic + 20 + randint(15) + to_s2, MIMIC_BALROG);
        break;
        case 44: /* Hellfire */
			if (!get_aim_dir(&dir)) return;
            fire_ball(GF_HELL_FIRE, dir,
                    666 * mto_s2, 3 + to_s2);
            take_hit(50+randint(50), "the strain of casting Hellfire");
        break;

        case 45: /* Confuse */
			if (!get_aim_dir(&dir)) return;
            (void)confuse_monster(dir, ( plev * 3) / 2 * mto_s2);
        break;
        case 46: /* Thraldom */
                msg_print("Your eyes look mesmerizing...");
                if (get_aim_dir(&dir))
                        (void) star_charm_monster(dir, p_ptr->lev / 2 * mto_s2);
        break;
        case 47: /* Polymorph Other */
			if (!get_aim_dir(&dir)) return;
			(void)poly_monster(dir);
        break;
        case 48: /* Polymorph Self */
                do_poly_self();
        break;
        case 49: /* Plague */
                project_hack(GF_POIS, 130 * mto_s2);
        break;
        case 50: /* Ravage Soul */
                if (!get_aim_dir(&dir)) return;
                /* A radius-0 ball may (1) be aimed at objects etc.,
                 * and will affect them; (2) may be aimed at ANY
                 * visible monster, unlike a 'bolt' which must travel
                 * to the monster. */

                fire_ball(GF_NETHER, dir,
                          damroll(8 + (plev / 10), 8 * mto_s2), 0 + to_s2);
        break;

        case 51: /* Absorb Light */
                unlite_area(10 * mto_s2, 5 + p_ptr->to_s);
                hp_player(damroll(plev / 5 + 1, 8 * mto_s2));
        break;
        case 52: /* Drain magic */
				{
					object_type * o_ptr;
					int lev, item;
                                        cptr  q, s;

					item_tester_hook = item_tester_hook_recharge;

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
					
                                        if (o_ptr->tval == TV_ROD_MAIN)
					{
                                                if (o_ptr->timeout > 0)
						{
							msg_print("You can't absorb energy from a discharged rod.");
						}
						else
						{
                                                        p_ptr->csp += o_ptr->timeout;
                                                        o_ptr->timeout = 0;
						}
					}
					else
					{
						if (o_ptr->pval > 0)
						{
							p_ptr->csp += o_ptr->pval * lev;
							o_ptr->pval = 0;
						}
						else
						{
							msg_print("There's no energy there to absorb!");
						}
						o_ptr->ident |= IDENT_EMPTY;
					}
					
					if (p_ptr->csp > p_ptr->msp)
					{
						p_ptr->csp = p_ptr->msp;
					}
					
					p_ptr->notice |= (PN_COMBINE | PN_REORDER);
					p_ptr->window |= (PW_INVEN);
				}
        break;
        case 53: /* Spear of Death */
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_NETHER, dir,
                                 damroll(5 * mto_s2, 7 + plev / 5));
        break;
        case 54: /* Restore Life */
                (void)restore_level();
                hp_player(300 * mto_s2);
        break;
        case 55: /* Nightfall */
                       if (!get_aim_dir(&dir)) return;
                       fire_ball(GF_DARK, dir,
                                 200 * mto_s2, 5 + to_s2);
        break;
        case 56: /* Blood Curse */
                if (!get_aim_dir(&dir)) return;
                /* A radius-0 ball may (1) be aimed at objects etc.,
                 * and will affect them; (2) may be aimed at ANY
                 * visible monster, unlike a 'bolt' which must travel
                 * to the monster. */

                fire_ball(GF_MISSILE, dir, p_ptr->chp / 2, 0);
                take_hit(p_ptr->chp / 2, "casting a Blood Curse");
        break;
        case 57: /* Wraithworld */
                set_shadow(p_ptr->wraith_form + randint(plev/2) + (plev/2)+to_s2);
        break;

        case 58: /* Invoke Spirits */
           {
               int die = randint(100) + plev / 5+to_s2;
               if (!get_aim_dir(&dir)) return;

              msg_print("You call on the power of the dead...");
               if (die > 100)
                 msg_print ("You feel a surge of eldritch force!");

               if (die < 8) {
               msg_print("Oh no! Mouldering forms rise from the earth around you!");
               (void) summon_specific(py, px, dun_level, SUMMON_UNDEAD);
               } else if (die < 14) {
               msg_print("An unnamable evil brushes against your mind...");
               set_afraid(p_ptr->afraid + randint(4) + 4);
               } else if (die < 26) {
               msg_print("Your head is invaded by a horde of gibbering spectral voices...");
               set_confused(p_ptr->confused + randint(4) + 4);
               } else if (die < 31) {
               poly_monster (dir);
               } else if (die < 36) {
               fire_bolt_or_beam (beam - 10,
                          GF_MISSILE, dir,
                          damroll(3 + ((plev - 1) / 5), 4)*to_s2);
               } else if (die < 41) {
               confuse_monster (dir, plev);
               } else if (die < 46) {
               fire_ball (GF_POIS, dir, 20 + (plev / 2)*to_s2, 3+to_s2);
               } else if (die < 51) {
               lite_line (dir);
               } else if (die < 56) {
               fire_bolt_or_beam (beam - 10, GF_ELEC, dir,
                          damroll(3+((plev-5)/4),8)*to_s2);
               } else if (die < 61) {
               fire_bolt_or_beam (beam - 10, GF_COLD, dir,
                          damroll(5+((plev-5)/4),8)*to_s2);
               } else if (die < 66) {
               fire_bolt_or_beam (beam, GF_ACID, dir,
                          damroll(6+((plev-5)/4),8)*to_s2);
               } else if (die < 71) {
               fire_bolt_or_beam (beam, GF_FIRE, dir,
                          damroll(8+((plev-5)/4),8)*to_s2);
               } else if (die < 76) {
               drain_life (dir, 75+to_s2);
               } else if (die < 81) {
               fire_ball (GF_ELEC, dir, 30 + plev*to_s2 / 2, 2+to_s2);
               } else if (die < 86) {
               fire_ball (GF_ACID, dir, 40 + plev*to_s2, 2+to_s2);
               } else if (die < 91) {
               fire_ball (GF_ICE, dir, 70 + plev*to_s2, 3+to_s2);
               } else if (die < 96) {
               fire_ball (GF_FIRE, dir, 80 + plev*to_s2, 3+to_s2);
               } else if (die < 101) {
               drain_life (dir, 100 + plev+(p_ptr->to_s*2));
               } else if (die < 104) {
               earthquake (py, px, 12+to_s2);
               } else if (die < 106) {
               destroy_area (py, px, 15+to_s2, TRUE);
               } else if (die < 108) {
               genocide(TRUE);
               } else if (die < 110) {
               dispel_monsters (120+to_s2);
               } else { /* RARE */
               dispel_monsters (150+to_s2);
               slow_monsters();
               sleep_monsters();
               hp_player (300+(p_ptr->to_s*2));
               }

               if (die < 31)
                 msg_print("Sepulchral voices chuckle. 'Soon you will join us, mortal.'");
               break;
           }
        break;
        case 59: /* Vampiric Branding */
                brand_weapon(3);
        break;
        case 60: /* Dispel Life */
                dispel_living(plev * 4 * mto_s2);
        break;
        case 61: /* Word of Destruction */
                destroy_area(py, px, 15 + to_s2, TRUE);
        break;
        case 62: /* Summon Greater Undead */
                msg_format("You magically summon greater undeads.");
                summon_specific_friendly(py, px, dun_level, SUMMON_HI_UNDEAD_NO_UNIQUES, TRUE);
        break;
        case 63: /* Mass Genocide */
                mass_genocide(TRUE);
        break;

	default:
                msg_format("You cast an unknown Nether spell: %d.", spell);
		msg_print(NULL);
	}

	if (no_trump)
                msg_print("Nobody answers your call.");
}


static void cast_crusade_spell(int spell)
{
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
        int to_s2=p_ptr->to_s/2;
        int mto_s2=p_ptr->to_s/2;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
                case 0: /* Boldness */
                        set_afraid(0);
                break;
                case 1: /* Bless */
                       (void)set_blessed(p_ptr->blessed + randint(12) + to_s2 + 12);
                break;
                case 2: /* Infra */
                        set_tim_infra(p_ptr->tim_infra + 20 + randint(20) + to_s2);
                break;
                case 3: /* Flash  */
                        lite_area(damroll(2, (plev / 2)), (plev / 10) + 1 + to_s2);
                break;
                case 4: /* See invis */
                        set_tim_invis(p_ptr->tim_invis + 12 + randint(12) + to_s2);
                break;
                case 5: /* Touch of Confusion */
                    if (!(p_ptr->confusing))
                    {
                        msg_print("Your hands start glowing.");
                        p_ptr->confusing = TRUE;
                    }
                break;
                case 6: /* Invoke fear */
                        if (!get_aim_dir(&dir)) return;
                        (void)fear_monster(dir, 5 + plev * mto_s2);
                break;
                case 7: /* Resist fire */
                        set_oppose_fire(p_ptr->oppose_fire + 10 + rand_int(10) + to_s2);
                break;

                case 8: /* Resist cold */
                        set_oppose_cold(p_ptr->oppose_cold + 12 + rand_int(10) + to_s2);
                break;
                case 9: /* Spear of light */
                       if (!get_aim_dir(&dir)) return;
                       msg_print("A line of blue shimmering light appears.");
                       lite_line(dir);
                       break;
                break;
                case 10: /* Sense foes */
                        detect_monsters_normal();
                break;
                case 11: /* Cure wounds & poison */
                        set_poisoned(0);
                        set_cut(0);
                break;
                case 12: /* Wolvish hunger */
                        if(p_ptr->mimic_form != MIMIC_WOLF)
                                set_mimic(10 + randint(8) + to_s2, MIMIC_WOLF);
                        else
                                set_mimic(p_ptr->tim_mimic + 10 + randint(8) + to_s2, MIMIC_WOLF);
                        break;
                break;
                case 13: /* Lightning lance */
                       if (!get_aim_dir(&dir)) return;
                       fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                               damroll(6 + ((plev-5)/4), 8 * mto_s2));
                break;
                case 14: /* Charm foes */
                         if (!get_aim_dir(&dir)) return;
                         (void) charm_monster(dir, plev * mto_s2);
                break;
                case 15: /* Heroism */
                        (void)set_hero(p_ptr->hero + randint(10)+ p_ptr->to_s + 10);
                break;

                case 16: /* holy chant */
                       set_blessed(p_ptr->blessed + randint(20) + 20 + to_s2);
                break;
                case 17: /* Fiery aura */
                        set_tim_fire_aura(p_ptr->tim_fire_aura + 15 + randint(10) + p_ptr->to_s);
                break;
                case 18: /* Smite */
                       if (!get_aim_dir(&dir)) return;
                       fire_bolt_or_beam(beam-10, GF_COLD, dir,
                               damroll(8 + ((plev-5)/4), 8 * mto_s2));
                break;
                case 19: /* Polymorph wounds */
			do_poly_wounds();
                break;
                case 20: /* holy lance */
                       if (!get_aim_dir(&dir)) return;
                       fire_bolt_or_beam(beam-10, GF_HOLY_FIRE, dir,
                               damroll(8 + ((plev-5)/4), 9 * mto_s2));
                break;
                case 21: /* vision */
                        map_area();
                break;
                case 22: /* exorcise */
                        dispel_demons((50 * mto_s2) + plev);
                        dispel_undead((50 * mto_s2) + plev);
                break;
                case 23: /* prayer */
                      (void)set_blessed(p_ptr->blessed + randint(48) + 48 + to_s2);
                break;

                case 24: /* call thunder */
                       msg_print("BOOM! Shake the room!");
                       project(0, 2+plev/10, py, px,
                           45 + plev * mto_s2, GF_SOUND, PROJECT_KILL|PROJECT_ITEM);
                break;
                case 25: /* berserk rage */
                        (void)set_shero(p_ptr->shero + randint(35) + 25 + to_s2);
                        (void)set_blessed(p_ptr->blessed + randint(35) + 25 + to_s2);
                break;
                case 26: /* dragon's wrath */
                        if(p_ptr->mimic_form != MIMIC_DRAGON)
                                set_mimic(15 + randint(10) + to_s2, MIMIC_DRAGON);
                        else
                                set_mimic(p_ptr->tim_mimic + 15 + randint(10) + to_s2, MIMIC_DRAGON);
                        break;
                break;
                case 27: /* mass charm */
                        charm_monsters(100 * mto_s2);
                break;
                case 28: /* haste self */
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev) ) + plev + to_s2);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5) + to_s2);
			}
                break;
                case 29: /* Wave of force */
                project(0, 3 + to_s2, py, px,
                        (120 + plev) * mto_s2,
                        GF_FORCE, PROJECT_KILL | PROJECT_ITEM | PROJECT_GRID | PROJECT_STOP);
                break;
                case 30: /* Invocation */
                       dispel_evil(150 * mto_s2);
                       if(rand_int(100) < 10) banish_evil(50 + p_ptr->to_s);
                break;
                case 31: /* Invulnerability */
                        (void)set_invuln(p_ptr->invuln + randint(3)+ to_s2 + 2);
                break;
	default:
                msg_format("You cast an unknown Crusade spell: %d.", spell);
		msg_print(NULL);
    }
}


static void cast_sigaldry_spell(int spell)
{
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
        int to_s2=p_ptr->to_s/2;
        int mto_s2=p_ptr->to_s/2;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
        case 0: /* Detect Monster */
                (void)detect_monsters_normal();
                break;
        case 1: /* Wizard Lock */
            if (!(get_aim_dir(&dir))) break;
            (void) wizard_lock(dir);
        break;
        case 2: /* Illuminate */
                (void)lite_area(damroll(2 * mto_s2, (plev / 2)), (plev / 10) + 1+to_s2);
        break;
        case 3: /* Locate Doors/Stairs */
			(void)detect_doors();
			(void)detect_stairs();
        break;
        case 4: /* Locate Traps */
			(void)detect_traps();
        break;
        case 5: /* Disruption I */
        {
                int typ = GF_MISSILE;

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
                          damroll(5 * mto_s2, 4) + (plev / 5), 0 + to_s2);
        }
        break;
        case 6: /* Blink */
                teleport_player(10 + to_s2);
        break;
        case 7: /* Detect Treasures */
                (void)detect_treasure();
        break;
        case 8: /* Detect Objects */
			(void)detect_objects_normal();
			(void)detect_objects_gold();
        break;
        case 9: /* Warding I */
                set_shield(p_ptr->shield + rand_int(5) + 5 + to_s2, 50 + to_s2);
        break;
        case 10: /* Trap/Door Destruction */
			(void)destroy_doors_touch();
        break;
        case 11: /* Sleep I */
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir);
        break;
        case 12: /* Preservation */
        {
                int item;
                object_type *o_ptr;
                cptr q, s;

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
			if (!get_aim_dir(&dir)) return;
            (void)confuse_monster(dir, ( plev * 3) / 2 * mto_s2);
        break;
        case 14: /* Magic Aura */
                (void)set_holy(p_ptr->holy + 10 + randint(5) + to_s2);
        break;
        case 15: /* Appraise Item */
                psychometry();
        break;

        case 16: /* Resist Fire */
                set_oppose_fire(p_ptr->oppose_fire + 10 + rand_int(20) + p_ptr->to_s);
        break;
        case 17: /* Resist Cold */
                set_oppose_cold(p_ptr->oppose_cold + 10 + rand_int(20) + p_ptr->to_s);
        break;
        case 18: /* Finger of Pain */
                /* It doesn't need to travel to the monster, it just Jump at it */
                if (!get_aim_dir(&dir)) return;
                project_hook(GF_MISSILE, dir, damroll(5 + (plev / 5), 8 * mto_s2),
                             PROJECT_STOP | PROJECT_KILL | PROJECT_JUMP);
        break;
        case 19: /* Recharge I */
                (void)recharge(20 * mto_s2);
        break;
        case 20: /* Magic Map */
                map_area();
        break;
        case 21: /* Sleep II */
                       (void)sleep_monsters_touch();
        break;
        case 22: /* Teleport Self */
                teleport_player(200 * mto_s2);
        break;
        case 23: /* Identify */
			(void)ident_spell();
        break;

        case 24: /* True Strike */
                set_strike(p_ptr->strike + randint(25) + 25 + to_s2);
        break;
        case 25: /* Between Gate */
       {
             int ii, ij;

             if(special_flag){msg_print("Not on special levels!");break;}

             msg_print("You open a between gate. Choose a destination.");
             if (!tgt_pt(&ii,&ij)) return;
             p_ptr->energy -= 60 - plev;
             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
             (distance(ij,ii,py,px) > plev + 2+to_s2) ||
             (!rand_int(plev * plev / 2)))
             {
                 msg_print("You fail to exit the between correctly!");
                 p_ptr->energy -= 100;
                 get_pos_player(10+to_s2,&ij,&ii);
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
                          damroll(10 * mto_s2, 10) + (plev / 5), 0 + to_s2);
        }
        break;
               break;
        case 27: /* Drain Magic I */
				{
					object_type * o_ptr;
					int lev, item;
                                        cptr  q, s;

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
                if (!get_spell(&spel, "scribe", q_ptr->sval, TRUE, q_ptr))
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
                        warding_glyph();
        break;
        case 31: /* Implosion */
                fire_ball(GF_IMPLOSION, 0, 200 * mto_s2, 2 + to_s2);
        break;
	default:
                msg_format("You cast an unknown Sigaldry spell: %d.", spell);
		msg_print(NULL);
    }
}

int use_symbiotic_power(int r_idx, bool great, bool only_number)
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask, plev = p_ptr->lev;

	char            choice;

	char            out_val[160];
        monster_race    *r_ptr = &r_info[r_idx];
        int rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);
        int x=px,y=py,k;
	int rad;

        /* List the powers */
        if(r_ptr->flags2 & RF2_MULTIPLY) {strcpy(power_desc[num],"Multiply");powers[num++]=90;}
        if(r_ptr->flags4 & RF4_SHRIEK) {strcpy(power_desc[num],"Agravate monsters");powers[num++]=0;}
        if(great) if(r_ptr->flags4 & RF4_ROCKET) {strcpy(power_desc[num],"Rocket");powers[num++]=1;}
        if(r_ptr->flags4 & RF4_ARROW_1) {strcpy(power_desc[num],"Arrow1");powers[num++]=2;}
        if(r_ptr->flags4 & RF4_ARROW_2) {strcpy(power_desc[num],"Arrow2");powers[num++]=3;}
        if(great) if(r_ptr->flags4 & RF4_ARROW_3) {strcpy(power_desc[num],"Arrow3");powers[num++]=4;}
        if(great) if(r_ptr->flags4 & RF4_ARROW_4) {strcpy(power_desc[num],"Arrow4");powers[num++]=5;}
        if(great) if(r_ptr->flags4 & RF4_BR_ACID) {strcpy(power_desc[num],"Breathe Acid");powers[num++]=6;}
        if(great) if(r_ptr->flags4 & RF4_BR_ELEC) {strcpy(power_desc[num],"Breathe Lightning");powers[num++]=7;}
        if(r_ptr->flags4 & RF4_BR_FIRE) {strcpy(power_desc[num],"Breathe Fire");powers[num++]=8;}
        if(r_ptr->flags4 & RF4_BR_COLD) {strcpy(power_desc[num],"Breathe Cold");powers[num++]=9;}
        if(great) if(r_ptr->flags4 & RF4_BR_POIS) {strcpy(power_desc[num],"Breathe Poison");powers[num++]=10;}
        if(great) if(r_ptr->flags4 & RF4_BR_NETH) {strcpy(power_desc[num],"Breathe Nether");powers[num++]=11;}
        if(r_ptr->flags4 & RF4_BR_LITE) {strcpy(power_desc[num],"Breathe Lite");powers[num++]=12;}
        if(great) if(r_ptr->flags4 & RF4_BR_DARK) {strcpy(power_desc[num],"Breathe Darkness");powers[num++]=13;}
        if(great) if(r_ptr->flags4 & RF4_BR_CONF) {strcpy(power_desc[num],"Breathe Confusion");powers[num++]=14;}
        if(great) if(r_ptr->flags4 & RF4_BR_SOUN) {strcpy(power_desc[num],"Breathe Sound");powers[num++]=15;}
        if(great) if(r_ptr->flags4 & RF4_BR_CHAO) {strcpy(power_desc[num],"Breathe Chaos");powers[num++]=16;}
        if(great) if(r_ptr->flags4 & RF4_BR_DISE) {strcpy(power_desc[num],"Breathe Disenchantment");powers[num++]=17;}
        if(great) if(r_ptr->flags4 & RF4_BR_NEXU) {strcpy(power_desc[num],"Breathe Nexus");powers[num++]=18;}
        if(great) if(r_ptr->flags4 & RF4_BR_TIME) {strcpy(power_desc[num],"Breathe Time");powers[num++]=19;}
        if(great) if(r_ptr->flags4 & RF4_BR_INER) {strcpy(power_desc[num],"Breathe Inertia");powers[num++]=20;}
        if(great) if(r_ptr->flags4 & RF4_BR_GRAV) {strcpy(power_desc[num],"Breathe Gravity");powers[num++]=21;}
        if(great) if(r_ptr->flags4 & RF4_BR_SHAR) {strcpy(power_desc[num],"Breathe Shards");powers[num++]=22;}
        if(great) if(r_ptr->flags4 & RF4_BR_PLAS) {strcpy(power_desc[num],"Breathe Plasma");powers[num++]=23;}
        if(great) if(r_ptr->flags4 & RF4_BR_WALL) {strcpy(power_desc[num],"Breathe Force");powers[num++]=24;}
        if(great) if(r_ptr->flags4 & RF4_BR_MANA) {strcpy(power_desc[num],"Breathe Mana");powers[num++]=25;}
        if(great) if(r_ptr->flags4 & RF4_BA_NUKE) {strcpy(power_desc[num],"Nuke Ball");powers[num++]=26;}
        if(great) if(r_ptr->flags4 & RF4_BR_NUKE) {strcpy(power_desc[num],"Breathe Nuke");powers[num++]=27;}
        if(great) if(r_ptr->flags4 & RF4_BA_CHAO) {strcpy(power_desc[num],"Chaos Ball");powers[num++]=28;}
        if(great) if(r_ptr->flags4 & RF4_BR_DISI) {strcpy(power_desc[num],"Breathe Disintegration");powers[num++]=29;}
        if(great) if(r_ptr->flags5 & RF5_BA_ACID) {strcpy(power_desc[num],"Acid Ball");powers[num++]=30;}
        if(great) if(r_ptr->flags5 & RF5_BA_ELEC) {strcpy(power_desc[num],"Lightning Ball");powers[num++]=31;}
        if(r_ptr->flags5 & RF5_BA_FIRE) {strcpy(power_desc[num],"Fire Ball");powers[num++]=32;}
        if(r_ptr->flags5 & RF5_BA_COLD) {strcpy(power_desc[num],"Cold Ball");powers[num++]=33;}
        if(great) if(r_ptr->flags5 & RF5_BA_POIS) {strcpy(power_desc[num],"Poison Ball");powers[num++]=34;}
        if(great) if(r_ptr->flags5 & RF5_BA_NETH) {strcpy(power_desc[num],"Nether Ball");powers[num++]=35;}
        if(r_ptr->flags5 & RF5_BA_WATE) {strcpy(power_desc[num],"Water Ball");powers[num++]=36;}
        if(great) if(r_ptr->flags5 & RF5_BA_MANA) {strcpy(power_desc[num],"Mana Ball");powers[num++]=37;}
        if(great) if(r_ptr->flags5 & RF5_BA_DARK) {strcpy(power_desc[num],"Darkness Ball");powers[num++]=38;}
        if(r_ptr->flags5 & RF5_CAUSE_1) {strcpy(power_desc[num],"Cause light wounds");powers[num++]=42;}
        if(r_ptr->flags5 & RF5_CAUSE_2) {strcpy(power_desc[num],"Cause medium wounds");powers[num++]=43;}
        if(r_ptr->flags5 & RF5_CAUSE_3) {strcpy(power_desc[num],"Cause critical wounds");powers[num++]=44;}
        if(r_ptr->flags5 & RF5_CAUSE_4) {strcpy(power_desc[num],"Cause mortal wounds");powers[num++]=45;}
        if(r_ptr->flags5 & RF5_BO_ACID) {strcpy(power_desc[num],"Acid Bolt");powers[num++]=46;}
        if(r_ptr->flags5 & RF5_BO_ELEC) {strcpy(power_desc[num],"Lightning Bolt");powers[num++]=47;}
        if(r_ptr->flags5 & RF5_BO_FIRE) {strcpy(power_desc[num],"Fire Bolt");powers[num++]=48;}
        if(r_ptr->flags5 & RF5_BO_COLD) {strcpy(power_desc[num],"Cold Bolt");powers[num++]=49;}
	if(r_ptr->flags5 & RF5_BO_POIS) {strcpy(power_desc[num],"Poison Bolt");powers[num++]=56;}
        if(r_ptr->flags5 & RF5_BO_NETH) {strcpy(power_desc[num],"Nether Bolt");powers[num++]=50;}
        if(r_ptr->flags5 & RF5_BO_WATE) {strcpy(power_desc[num],"Water Bolt");powers[num++]=51;}
        if(r_ptr->flags5 & RF5_BO_MANA) {strcpy(power_desc[num],"Mana Bolt");powers[num++]=52;}
        if(r_ptr->flags5 & RF5_BO_PLAS) {strcpy(power_desc[num],"Plasma Bolt");powers[num++]=53;}
        if(r_ptr->flags5 & RF5_BO_ICEE) {strcpy(power_desc[num],"Ice Bolt");powers[num++]=54;}
        if(r_ptr->flags5 & RF5_MISSILE) {strcpy(power_desc[num],"Missile");powers[num++]=55;}
        if(r_ptr->flags5 & RF5_SCARE) {strcpy(power_desc[num],"Scare");powers[num++]=41;}
        if(r_ptr->flags5 & RF5_BLIND) {strcpy(power_desc[num],"Blindness");powers[num++]=57;}
        if(r_ptr->flags5 & RF5_CONF) {strcpy(power_desc[num],"Confusion");powers[num++]=58;}
        if(r_ptr->flags5 & RF5_SLOW) {strcpy(power_desc[num],"Slow");powers[num++]=59;}
        if(r_ptr->flags5 & RF5_HOLD) {strcpy(power_desc[num],"Paralyse");powers[num++]=60;}
        if(r_ptr->flags6 & RF6_HASTE) {strcpy(power_desc[num],"Haste Self");powers[num++]=61;}
        if(great) if(r_ptr->flags6 & RF6_HAND_DOOM) {strcpy(power_desc[num],"Hand of Doom");powers[num++]=62;}
        if(r_ptr->flags6 & RF6_HEAL) {strcpy(power_desc[num],"Heal");powers[num++]=63;}
        if(r_ptr->flags6 & RF6_BLINK) {strcpy(power_desc[num],"Blink");powers[num++]=64;}
        if(r_ptr->flags6 & RF6_TPORT) {strcpy(power_desc[num],"Teleport");powers[num++]=65;}
        if(great) if(r_ptr->flags6 & RF6_TELE_TO) {strcpy(power_desc[num],"Teleport To");powers[num++]=66;}
        if(r_ptr->flags6 & RF6_TELE_AWAY) {strcpy(power_desc[num],"Teleport Away");powers[num++]=67;}
        if(great) if(r_ptr->flags6 & RF6_TELE_LEVEL) {strcpy(power_desc[num],"Teleport Level");powers[num++]=68;}
        if(r_ptr->flags6 & RF6_DARKNESS) {strcpy(power_desc[num],"Darkness");powers[num++]=69;}
        if(great) if(r_ptr->flags6 & RF6_TRAPS) {strcpy(power_desc[num],"Create Traps");powers[num++]=88;}
	if(great) if(r_ptr->flags6 & RF6_RAISE_DEAD) {strcpy(power_desc[num],"Raise the Dead");powers[num++]=89;}
        if(r_ptr->flags6 & RF6_S_BUG) {strcpy(power_desc[num],"Summon Sofware Bugs");powers[num++]=70;}
        if(r_ptr->flags6 & RF6_S_RNG) {strcpy(power_desc[num],"Summon RNG");powers[num++]=71;}
        if(great) if(r_ptr->flags6 & RF6_S_DRAGONRIDER) {strcpy(power_desc[num],"Summon DragonRider");powers[num++]=72;}
        if(r_ptr->flags6 & RF6_S_KIN) {strcpy(power_desc[num],"Summon Kin");powers[num++]=73;}
        if(great) if(r_ptr->flags6 & RF6_S_CYBER) {strcpy(power_desc[num],"Summon Cyberdemon");powers[num++]=74;}
        if(r_ptr->flags6 & RF6_S_MONSTER) {strcpy(power_desc[num],"Summon Monster");powers[num++]=75;}
        if(great) if(r_ptr->flags6 & RF6_S_MONSTERS) {strcpy(power_desc[num],"Summon Monsters");powers[num++]=76;}
        if(r_ptr->flags6 & RF6_S_ANT) {strcpy(power_desc[num],"Summon Ants");powers[num++]=77;}
        if(r_ptr->flags6 & RF6_S_SPIDER) {strcpy(power_desc[num],"Summon Spiders");powers[num++]=78;}
        if(great) if(r_ptr->flags6 & RF6_S_HOUND) {strcpy(power_desc[num],"Summon Hound");powers[num++]=79;}
        if(great) if(r_ptr->flags6 & RF6_S_HYDRA) {strcpy(power_desc[num],"Summon Hydras");powers[num++]=80;}
        if(great) if(r_ptr->flags6 & RF6_S_ANGEL) {strcpy(power_desc[num],"Summon Angel");powers[num++]=81;}
        if(great) if(r_ptr->flags6 & RF6_S_DEMON) {strcpy(power_desc[num],"Summon Demon");powers[num++]=82;}
        if(great) if(r_ptr->flags6 & RF6_S_UNDEAD) {strcpy(power_desc[num],"Summon Undead");powers[num++]=83;}
        if(great) if(r_ptr->flags6 & RF6_S_DRAGON) {strcpy(power_desc[num],"Summon Dragon");powers[num++]=84;}
        if(great) if(r_ptr->flags6 & RF6_S_HI_UNDEAD) {strcpy(power_desc[num],"Summon High Undead");powers[num++]=85;}
        if(great) if(r_ptr->flags6 & RF6_S_HI_DRAGON) {strcpy(power_desc[num],"Summon High Dragon");powers[num++]=86;}
        if(great) if(r_ptr->flags6 & RF6_S_WRAITH) {strcpy(power_desc[num],"Summon Wraith");powers[num++]=87;}

        if(!num) {msg_print("No powers to use.");return 0;}

        if(only_number) return num;

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
				Term_save();

				prt ("", y++, x);

				while (ctr < num && ctr < 17)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
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
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

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
		case 90: /* Multiply */
			msg_format("You multiply...");
			do_cmd_wiz_named_friendly(p_ptr->body_monster, FALSE);
			break;
                case 0: /* Shriek */
                        aggravate_monsters(-1);
                        break;
                case 1: /* Rocket */
                        msg_print("You launch a rocket...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ROCKET, dir, p_ptr->lev * 12, 1 + (p_ptr->lev/20));
                        break;
                case 2: /* Arrow1 */
                        msg_print("You fire a light arrow...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(1,6));
                        break;
                case 3: /* Arrow2 */
                        msg_print("You fire a heavy arrow...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(3,6));
                        break;
                case 4: /* Arrow3 */
                        msg_print("You fire a light missile...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(5,6));
                        break;
                case 5: /* Arrow4 */
                        msg_print("You fire a heavy missile...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(7,6));
                        break;
                case 6: /* Br acid */
                        msg_print("You breathe acid ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ACID, dir, p_ptr->lev * 5, rad);
                        break;
                case 7: /* Br elec */
                        msg_print("You breathe lightning ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ELEC, dir, p_ptr->lev * 5, rad);
                        break;
                case 8: /* br fire */
                        msg_print("You breathe fire ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FIRE, dir, p_ptr->lev * 5, rad);
                        break;
                case 9: /* br cold */
                        msg_print("You breathe cold ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_COLD, dir, p_ptr->lev * 5, rad);
                        break;
                case 10: /* br pois */
                        msg_print("You breathe poison ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_POIS, dir, p_ptr->lev * 5, rad);
                        break;
                case 11: /* br neth */
                        msg_print("You breathe nether ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NETHER, dir, p_ptr->lev * 5, rad);
                        break;
                case 12: /* br lite */
                        msg_print("You breathe lite ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_LITE, dir, p_ptr->lev * 8, rad);
                        break;
                case 13: /* br dark */
                        msg_print("You breathe dark ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DARK, dir, p_ptr->lev * 8, rad);
                        break;
                case 14: /* br conf */
                        msg_print("You breathe confusion ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CONFUSION, dir, p_ptr->lev * 8, rad);
                        break;
                case 15: /* br soun */
                        msg_print("You breathe sound ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SOUND, dir, p_ptr->lev * 8, rad);
                        break;
                case 16: /* br chao */
                        msg_print("You breathe chaos ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CHAOS, dir, p_ptr->lev * 7, rad);
                        break;
                case 17: /* br dise */
                        msg_print("You breathe disenchantment ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DISENCHANT, dir, p_ptr->lev * 7, rad);
                        break;
                case 18: /* br nexu */
                        msg_print("You breathe nexus ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NEXUS, dir, p_ptr->lev * 5, rad);
                        break;
                case 19: /* br time */
                        msg_print("You breathe time ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_TIME, dir, p_ptr->lev * 3, rad);
                        break;
                case 20: /* br iner */
                        msg_print("You breathe inertia ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_INERTIA, dir, p_ptr->lev * 4, rad);
                        break;
                case 21: /* br grav */
                        msg_print("You breathe gravity ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_GRAVITY, dir, p_ptr->lev * 4, rad);
                        break;
                case 22: /* br shar */
                        msg_print("You breathe shards ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SHARDS, dir, p_ptr->lev * 8, rad);
                        break;
                case 23: /* br plas */
                        msg_print("You breathe plasma ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_PLASMA, dir, p_ptr->lev * 3, rad);
                        break;
                case 24: /* br wall */
                        msg_print("You breathe force ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FORCE, dir, p_ptr->lev * 4, rad);
                        break;
                case 25: /* br mana */
                        msg_print("You breathe mana ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, p_ptr->lev * 5, rad);
                        break;
                case 26: /* ba nuke */
                        msg_print("You cast a ball of nuke ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, p_ptr->lev * 8, 1 + (p_ptr->lev/20));
                        break;
                case 27: /* br nuke */
                        msg_print("You breathe nuke ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, p_ptr->lev * 8, 1 + (p_ptr->lev/20));
                        break;
                case 28: /* ba chao */
                        msg_print("You cast a ball of chaos ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CHAOS, dir, p_ptr->lev * 4, 2);
                        break;
                case 29: /* br disi */
                        msg_print("You breathe disintegration ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DISINTEGRATE, dir, p_ptr->lev * 5, 1 + (p_ptr->lev/20));
                        break;
                case 30: /* ba acid */
                        msg_print("You cast a ball of acid ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ACID, dir, randint(p_ptr->lev * 6)+20, 2);
                        break;
                case 31: /* ba elec */
                        msg_print("You cast a ball of lightning ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ELEC, dir, randint(p_ptr->lev * 3)+20, 2);
                        break;
                case 32: /* ba fire */
                        msg_print("You cast a ball of fire ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FIRE, dir, randint(p_ptr->lev * 7)+20, 2);
                        break;
                case 33: /* ba cold */
                        msg_print("You cast a ball of cold ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_COLD, dir, randint(p_ptr->lev * 3)+20, 2);
                        break;
                case 34: /* ba pois */
                        msg_print("You cast a ball of poison ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_POIS, dir, damroll(12,2), 2);
                        break;
                case 35: /* ba neth */
                        msg_print("You cast a ball of nether ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NETHER, dir, randint(p_ptr->lev * 4)+20, 2);
                        break;
                case 36: /* ba wate */
                        msg_print("You cast a ball of water ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_WATER, dir, randint(p_ptr->lev * 4)+20, 2);
                        break;
                case 37: /* ba mana */
                        msg_print("You cast a ball of mana ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, randint(p_ptr->lev * 3)+20, 2);
                        break;
                case 38: /* ba dark */
                        msg_print("You cast a ball of darkness ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DARK, dir, randint(p_ptr->lev * 3)+20, 2);
                        break;
	        case 42: /* cause1 */
		        msg_print("You cause light wounds ...");
		        if (get_aim_dir(&dir))
		        {
			        fire_bolt(GF_MANA, dir, damroll(3, 8));
			}
		        break;
	        case 43: /* cause2 */
		        msg_print("You cause serious wounds ...");
		        if (get_aim_dir(&dir))
		        {
			        fire_bolt(GF_MANA, dir, damroll(8, 8));
			}
		        break;
	        case 44: /* cause3 */
		        msg_print("You cause critical wounds ...");
		        if (get_aim_dir(&dir))
		        {
			        fire_bolt(GF_MANA, dir, damroll(10, 15));
			}
		        break;
	        case 45: /* cause4 */
		        msg_print("You cause mortal wounds ...");
		        if (get_aim_dir(&dir))
		        {
			        fire_bolt(GF_MANA, dir, damroll(15, 15));
			}
		        break;
                case 46: /* bo acid */
                        msg_print("You cast a bolt of acid ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ACID, dir, damroll(7, 8) + (p_ptr->lev/3));
                        break;
                case 47: /* bo elec */
                        msg_print("You cast a bolt of lightning ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ELEC, dir, damroll(4, 8) + (p_ptr->lev/3));
                        break;
                case 48: /* bo fire */
                        msg_print("You cast a bolt of fire ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_FIRE, dir, damroll(9, 8) + (p_ptr->lev/3));
                        break;
                case 49: /* bo cold */
                        msg_print("You cast a bolt of cold ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_COLD, dir, damroll(6, 8) + (p_ptr->lev/3));
                        break;
                case 56: /* bo pois */
                        msg_print("You cast a bolt of poison ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_POIS, dir, damroll(7, 8) + (p_ptr->lev/3));
                        break;
                case 50: /* bo neth */
                        msg_print("You cast a bolt of nether ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_NETHER, dir, damroll(5, 5) + (p_ptr->lev/3));
                        break;
                case 51: /* bo wate */
                        msg_print("You cast a bolt of water ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_WATER, dir, damroll(10, 10) + (p_ptr->lev/3));
                        break;
                case 52: /* bo mana */
                        msg_print("You cast a bolt of mana ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_MANA, dir, damroll(3, 8) + (p_ptr->lev/3));
                        break;
                case 53: /* bo plas */
                        msg_print("You cast a bolt of plasma ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_PLASMA, dir, damroll(8, 8) + (p_ptr->lev/3));
                        break;
                case 54: /* bo ice */
                        msg_print("You cast a bolt of ice ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ICE, dir, damroll(6, 6) + (p_ptr->lev/3));
                        break;
                case 55: /* missile */
                        msg_print("You cast a magic missile ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_MISSILE, dir, damroll(2, 6) + (p_ptr->lev/3));
                        break;
                case 57: /* blind */
                        msg_print("You cast blindness ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_CONFUSION, dir, damroll(1, 8) + (p_ptr->lev/3));
                        break;
                case 41: /* scare */
                        msg_print("You cast scare ...");
			if (get_aim_dir(&dir))
				fear_monster(dir, plev);
                        break;
                case 58: /* conf */
                        msg_print("You cast a bolt of confusion ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_CONFUSION, dir, damroll(7, 8) + (p_ptr->lev/3));
                        break;
                case 59: /* slow */
                        msg_print("You cast a bolt of slowness ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_OLD_SLOW, dir, damroll(6, 8) + (p_ptr->lev/3));
                        break;
                case 60: /* hold */
                        msg_print("You cast a bolt of paralisation ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_OLD_SLEEP, dir, damroll(5, 8) + (p_ptr->lev/3));
                        break;
                case 61: /* haste */
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev) ) + plev);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5));
			}
                        break;
                case 62: /* hand of doom */
                        msg_print("You invoke the Hand of Doom ...");
                        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(10, 8) + (p_ptr->lev));
			}
                        break;
                case 63: /* heal */
                        hp_player(damroll(8,5));
                        break;
                case 64: /* Blink */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
                        teleport_player(10);
                        break;
                case 65: /* Teleport */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
                        teleport_player(plev * 5);
                        break;
                case 66: /* tele to */
                        {
                             int ii,ij;

                             if(special_flag) {msg_print("No teleport on special levels ...");break;}
                             msg_print("You go between.");
                             if (!tgt_pt(&ii,&ij)) return num;
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
                case 67: /* tele away */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
                        if (!get_aim_dir(&dir)) return num;
                        (void)fire_beam(GF_AWAY_ALL, dir, plev);
                        break;
		case 68: /* tele level */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
			teleport_player_level();
			break;
                case 69: /* darkness */
                        (void)project(-1, 3, py, px, 0, GF_DARK_WEAK, PROJECT_GRID | PROJECT_KILL);
                        /* Unlite up the room */
                        unlite_room(py, px);
                        break;
	        case 88: /* create traps */
		        msg_print("You create traps ...");
			trap_creation();
		        break;
		case 89: /* raise the dead - uses the same code as the
			    nether spell*/
		{
                        if (!get_aim_dir(&dir)) break;
                        fire_ball(GF_RAISE, dir, 1, 0);
			break;
		}
                case 70: /* Summon bug */
                                msg_format("You magically code some software bugs.");
				for (k = 0; k < 6; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_BUG, TRUE);
				}
                        break;
                case 71: /* Summon RNG */
                                msg_format("You magically code some RNGs.");
				for (k = 0; k < 6; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_RNG, TRUE);
				}
                        break;
                case 72: /* Summon dragonrider */
                                msg_format("You magically summon a DragonRider.");
                                for (k = 0; k < 1; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_DRAGONRIDER, TRUE);
				}
                        break;
                case 73: /* Summon kin */
                                msg_format("You magically summon some Kins.");
				summon_kin_type = r_ptr->d_char; /* Big hack */
                                for (k = 0; k < 6; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_KIN, TRUE);
				}
                        break;
                case 74: /* Summon cyber */
                                msg_format("You magically summon a Cyberdemon.");
                                for (k = 0; k < 1; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_CYBER, TRUE);
				}
                        break;
                case 75: /* Summon monster */
                                msg_format("You magically summon a monster.");
                                for (k = 0; k < 1; k++)
				{
                                                summon_specific_friendly(y, x, rlev, 0, TRUE);
				}
                        break;
                case 76: /* Summon monsters */
                                msg_format("You magically summon monsters.");
                                for (k = 0; k < 6; k++)
				{
                                                summon_specific_friendly(y, x, rlev, 0, TRUE);
				}
                        break;
                case 77: /* Summon ant */
                                msg_format("You magically summon ants.");
                                for (k = 0; k < 6; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_ANT, TRUE);
				}
                        break;
                case 78: /* Summon spider */
                                msg_format("You magically summon spiders.");
                                for (k = 0; k < 6; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_SPIDER, TRUE);
				}
                        break;
                case 79: /* Summon hound */
                                msg_format("You magically summon hounds.");
                                for (k = 0; k < 6; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_HOUND, TRUE);
				}
                        break;
                case 80: /* Summon hydra */
                                msg_format("You magically summon hydras.");
                                for (k = 0; k < 6; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_HYDRA, TRUE);
				}
                        break;
                case 81: /* Summon angel */
                                msg_format("You magically summon an angel.");
                                for (k = 0; k < 1; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_ANGEL, TRUE);
				}
                        break;
                case 82: /* Summon demon */
                                msg_format("You magically summon a demon.");
                                for (k = 0; k < 1; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_DEMON, TRUE);
				}
                        break;
                case 83: /* Summon undead */
                                msg_format("You magically summon an undead.");
                                for (k = 0; k < 1; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_UNDEAD, TRUE);
				}
                        break;
                case 84: /* Summon dragon */
                                msg_format("You magically summon a dragon.");
                                for (k = 0; k < 1; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_DRAGON, TRUE);
				}
                        break;
                case 85: /* Summon hiundead */
                                msg_format("You magically summon greater undeads.");
                                for (k = 0; k < 8; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_HI_UNDEAD_NO_UNIQUES, TRUE);
				}
                        break;
                case 86: /* Summon hidragon */
                                msg_format("You magically summon greater dragons.");
                                for (k = 0; k < 8; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_HI_DRAGON_NO_UNIQUES, TRUE);
				}
                        break;
                case 87: /* Summon wraith */
                                msg_format("You magically summon Wraith.");
                                for (k = 0; k < 8; k++)
				{
                                                summon_specific_friendly(y, x, rlev, SUMMON_WRAITH, TRUE);
				}
                        break;
        }
        return num;
}

static void cast_symbiotic_spell(int spell)
{
        object_type *o_ptr;
        monster_race *r_ptr;
        int to_s2=p_ptr->to_s/2;

        to_s2 = (to_s2==0)?1:to_s2;

        /* Get the carried monster */
        o_ptr = &inventory[INVEN_CARRY];

	switch (spell)
	{
        case 0: /* Minor Symbiotic healing */
                if(o_ptr->k_idx)
                {
                        int max;

                        r_ptr = &r_info[o_ptr->pval];
                        max = maxroll(r_ptr->hdice, r_ptr->hside);
                        o_ptr->pval2 += damroll(2, 6)+to_s2;
                        if(o_ptr->pval2 > max)o_ptr->pval2 = max;

                        msg_print("Your monster is healed");

                        /* Display the monster hitpoints */
                        p_ptr->redraw |= (PR_MH);
                }
                else
                        msg_print("You are not in symbiosis.");
                break;

        case 1: /* Tangled Creepers */
                slow_monsters();
                break;

        case 2: /* Vamiric healing */
        {
                int dummy,plev=p_ptr->lev,dir;                        
                int max;

                r_ptr = &r_info[o_ptr->pval];
                max = maxroll(r_ptr->hdice, r_ptr->hside);

                if (!get_aim_dir(&dir)) return;
                dummy = plev + randint(plev) * MAX(1, plev/10)+(p_ptr->to_s*2);   /* Dmg */
                if (drain_life(dir, dummy))
                {
                        o_ptr->pval2 += dummy;
                        if(o_ptr->pval2 > max)o_ptr->pval2 = max;
                        p_ptr->redraw |= (PR_MH);
                }
                break;
       }

        case 3: /* Life transfer */
                if(o_ptr->k_idx)
                {
                        int max,hp;

                        r_ptr = &r_info[o_ptr->pval];
                        max = maxroll(r_ptr->hdice, r_ptr->hside);

                        hp = damroll(6, 15)+to_s2;
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
                                msg_print("You can't do this in your weaken state.");
                }
                else
                        msg_print("You are not in symbiosis.");
                break;

        case 4: /* Satisfy Hunger */
                (void)set_food(PY_FOOD_MAX - 1);
                break;

        case 5: /* Symbiotic minor powers */
                if(o_ptr->k_idx) use_symbiotic_power(o_ptr->pval, FALSE, FALSE);
                else msg_print("You are not in symbiosis.");
                break;

        case 6: /* Summon a never-moving pet */
                summon_specific_friendly(py, px, dun_level, SUMMON_MINE,FALSE);
                break;

        case 16: /* Mana healing */
                if(o_ptr->k_idx)
                {
                        int max,hp;
             
                        r_ptr = &r_info[o_ptr->pval];
                        max = maxroll(r_ptr->hdice, r_ptr->hside);

                        hp = damroll(6, 15)+to_s2;
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
                for(k=0;k<6;k++)
                        summon_specific_friendly(py, px, dun_level, SUMMON_MINE,FALSE);
                }
                break;

        case 18: /* Major Symbiotic healing */
                if(o_ptr->k_idx)
                {
                        int max;

                        r_ptr = &r_info[o_ptr->pval];
                        max = maxroll(r_ptr->hdice, r_ptr->hside);
                        o_ptr->pval2 += damroll(3, 60)+to_s2;
                        if(o_ptr->pval2 > max)o_ptr->pval2 = max;

                        msg_print("Your monster is healed");

                        /* Display the monster hitpoints */
                        p_ptr->redraw |= (PR_MH);
                }
                else
                        msg_print("You are not in symbiosis.");
                break;

        case 19: /* Healing */
                hp_player(damroll(2, 40)+to_s2);
                break;

        case 20: /* Major Symbiotic powers */
                if(o_ptr->k_idx) use_symbiotic_power(o_ptr->pval, TRUE, FALSE);
                else msg_print("You are not in symbiosis.");
                break;

        case 21: /* Use Ennemy's Powers */
        {
                int y,x;
                cave_type *c_ptr;
                monster_type *m_ptr;

                if (!tgt_pt(&x,&y)) return;
                c_ptr = &cave[y][x];

                if (!(c_ptr->m_idx)) break;
                m_ptr = &m_list[c_ptr->m_idx];
                use_symbiotic_power(m_ptr->r_idx, TRUE, FALSE);
                break;
        }

        default:
                msg_format("You cast an unknown Symbiotic spell: %d.", spell);
		msg_print(NULL);
    }
}

static void cast_music_spell(int spell)
{
        int     dir, dummy;
	int	plev = p_ptr->lev;
        int to_s2=p_ptr->to_s/2;
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
                       msg_print("You start humming a slow, steady melody...");
                       p_ptr->class_extra1 = MUSIC_SLOW;
		       break;
                case 1:  /* Stop singing */
                       p_ptr->class_extra1 = MUSIC_NONE;
		       break;
                case 2:  /* The note which kill */
                       msg_print("You cry out in an ear-wracking voice...");
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_SOUND, dir,
                              damroll(4 + ((plev - 1) / 5) * p_ptr->to_s, 4));
		       break;
                case 3:  /* Stunning song */
                       msg_print("You weave a pattern of sounds to bewilder and daze...");
                       p_ptr->class_extra1 = MUSIC_STUN;
		       break;
                case 4:  /* Song of life */
                       msg_print("Life flows through you as you sing a song of healing...");
                       p_ptr->class_extra1 = MUSIC_LIFE;
		       break;
                case 5:  /* Song of mind */
                       msg_print("Your quiet music sharpens your sense of hearing...");
                       p_ptr->class_extra1 = MUSIC_MIND;
		       break;
                case 6:  /* Song of lite */
                       msg_print("Your uplifting song brings brightness to dark places...");
                       p_ptr->class_extra1 = MUSIC_LITE;
		       break;
                case 7:  /* Song of fury */
                       msg_print("Your sing of the heroic Elder Days...");
                       p_ptr->class_extra1 = MUSIC_FURY;
		       break;
                case 8:  /* Awareness song */
                       msg_print("As you start singing, you become more aware of the world around you.");
                       p_ptr->class_extra1 = MUSIC_AWARE;
                       break;
                case 9:  /* Song of knowledge */
                       msg_print("You recall the rich lore of the world...");
                       p_ptr->class_extra1 = MUSIC_ID;
                       break;
                case 10:  /* Between note */
                       msg_print("Your voice twists through space...");
                       teleport_player(100);
                       break;
                case 11:  /* The song which kill */
                       msg_print("You call out with a terrible curse...");
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_SOUND, dir,
                              damroll(10 + ((plev - 1) / 5) * p_ptr->to_s, 6));
                       break;
                case 12:  /* song of illusion */
                       msg_print("You weave a pattern of sounds to beguile and confuse...");
                       p_ptr->class_extra1 = MUSIC_ILLUSION;
                       break;
                case 13:  /* wall breaking song */
                       msg_print("You weave a pattern of sounds to contort and shatter...");
                       p_ptr->class_extra1 = MUSIC_WALL;
                       break;
                case 14:  /* song of resistance */
                       msg_print("You sing a song of perserverance against powers...");
                       p_ptr->class_extra1 = MUSIC_RESIST;
                       break;
                case 15:  /* song of time */
                       msg_print("You start singing swift and light folk-tunes...");
                       p_ptr->class_extra1 = MUSIC_TIME;
                       break;
                case 16:  /* Education song */
                {
                        monster_type *m_ptr;
                        monster_race *r_ptr;
                        int m;

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
                       msg_print("Reality whirls wildly as you sing a dizzying melody...");
                       fire_explosion(py, px, GF_AWAY_ALL, p_ptr->lev/15 + 1, 1 + (p_ptr->lev * 3));
                       break;
                case 18:  /* charming note */
                       msg_print("You chant a song of friendship...");
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_CHARM, dir,
                              damroll(10 + ((plev - 1) / 5) * p_ptr->to_s, 5));
                       break;
                case 19:  /* song of death */
                       msg_print("You unleash a furious barrage of sounds...");
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_SOUND, dir,
                              damroll(20 + ((plev - 1) / 5) * p_ptr->to_s, 10));
                       break;
                case 20:  /* vibration note */
                       msg_print("You sing an old song of the dwarven-smiths...");
                       brand_weapon(5); /* brand earthquake */
                       break;
                case 21:  /* vibration song */
                       msg_print("The fury of the Downfall of Numenor lashes out...");
                       p_ptr->class_extra1 = MUSIC_VIBRA;
                       break;
                case 22:  /* song of disruption */
                       msg_print("You sing of the primeval shaping of Middle-earth...");
                       alter_reality();
                       break;
                case 23:  /* Lay of Gil-Galad */
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
                       msg_print("Your song carries you beyond the sight of mortal eyes...");
                       p_ptr->class_extra1 = MUSIC_HIDE;
                       break;
                case 25:  /* shriek of death */
                       msg_print("A chanting dark and fell rises, bearing death to the world...");
                       if (!get_aim_dir(&dir)) return;
                       fire_bolt(GF_SOUND, dir,
                              damroll(100 + ((plev - 1) / 5) * p_ptr->to_s, 20));
                       break;
                case 26:  /* charming song */
                       msg_print("You weave a slow, soothing melody of imploration...");
                       p_ptr->class_extra1 = MUSIC_CHARME;
                       break;
                case 27:  /* between shriek */
                       msg_print("The holy power of the Music of the Ainur enters you...");
                       p_ptr->class_extra1 = MUSIC_HOLY;
                       break;
                case 28:  /* destruction shriek */
                       msg_print("The unmeasurable destructive power of the Oceans crashes around you...");
                       destroy_area(py, px, 10, TRUE);
                       break;
                case 29:  /* song of liberty */
                       msg_print("You recall the valor of Fingolfin's challenge to the Dark Lord...");
                       p_ptr->class_extra1 = MUSIC_LIBERTY;
                       break;
                case 30:  /* song of the Undeads */
                       msg_print("The themes of life and revival are woven into your song...");
                       p_ptr->class_extra1 = MUSIC_RAISE;
                       break;
                case 31:  /* Immaterial song */
                       msg_print("You chant a deep and moving hymn of the shadowy Halls of Mandos...");
                       p_ptr->class_extra1 = MUSIC_SHADOW;
                       break;

                default:
                        msg_format("You sing an unknown song: %d.", spell);
                        msg_print(NULL);
        }
}

static void cast_tribal_spell(int spell)
{
        int     dir, beam;
        int     plev = p_ptr->lev;
        int to_s2=p_ptr->to_s/2;
        int mto_s2=p_ptr->to_s/2;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
                case 0: /* Slumber */
                        fire_ball(GF_OLD_SLEEP, 0, (5 + plev) * mto_s2, 2 + to_s2);
                        break;
                case 1: /* Lightning Bolt */
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                damroll(2 * mto_s2, 10));
                        break;
                case 2: /* Bewilder */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_OLD_CONF, dir, (7 + plev) * mto_s2, 1 + to_s2);
                        break;
                case 3: /* Song of Morning */
                        fire_ball(GF_LITE, 0, (10 + plev) * mto_s2, 5 + to_s2);
                        break;
                case 4: /* Recuperation */
                        hp_player(damroll(1 + to_s2, 20));
                        break;
                case 5: /* Meditate */
                        set_paralyzed(p_ptr->paralyzed + damroll(2, 6));
                        fire_ball(GF_MAKE_GLYPH, 0, 1, 2 + to_s2);
                        heal_insanity(300 + p_ptr->to_s);
                        do_res_stat(A_INT);
                        project_hack(GF_AWAY_ALL, 5 + to_s2);
                        break;
                case 6: /* Wolf Spirit */
                        if(p_ptr->mimic_form != MIMIC_WOLF)
                                set_mimic(10 + randint(10) + to_s2, MIMIC_WOLF);
                        else
                                set_mimic(p_ptr->tim_mimic + 10 + randint(10) + to_s2, MIMIC_WOLF);
                        break;
                case 7: /* Dress Wounds */
                        hp_player(damroll(3 + to_s2, 15));
                        set_cut(0);
                        break;

                case 8: /* Grow Berries */
                        set_food(p_ptr->food + 3000);
                        break;
                case 9: /* Sense Presence */
                        detect_monsters_normal();
                        break;
                case 10: /* Punishment */
                        fire_ball(GF_MANA, 0, damroll(25 + to_s2, 2), 4 + to_s2);
                        break;
                case 11: /* Life Drain */
                        dec_stat(A_STR, 5, (magik(70))?STAT_DEC_NORMAL:STAT_DEC_PERMANENT);
                        dec_stat(A_CON, 5, (magik(70))?STAT_DEC_NORMAL:STAT_DEC_PERMANENT);
                        dec_stat(A_DEX, 5, (magik(70))?STAT_DEC_NORMAL:STAT_DEC_PERMANENT);
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_MANA, dir, damroll(50, 50), 5);
                        break;
                case 12: /* Beetle Spirit */
                        if(p_ptr->mimic_form != MIMIC_INSECT)
                                set_mimic(10 + randint(10) + to_s2, MIMIC_INSECT);
                        else
                                set_mimic(p_ptr->tim_mimic + 10 + randint(10) + to_s2, MIMIC_INSECT);
                        break;
                case 13: /* Call Nature */
                        summon_specific_friendly(py, px, dun_level, SUMMON_ANT, FALSE);
                        summon_specific_friendly(py, px, dun_level, SUMMON_SPIDER, FALSE);
                        break;
                case 14: /* Bird Spirit */
                        if(p_ptr->mimic_form != MIMIC_SPARROW)
                                set_mimic(10 + randint(10) + to_s2, MIMIC_SPARROW);
                        else
                                set_mimic(p_ptr->tim_mimic + 10 + randint(10) + to_s2, MIMIC_SPARROW);
                        break;
                case 15: /* Winter's Fury */
                        fire_ball(GF_COLD, 0, damroll(10 + to_s2, 10), 4 + to_s2);
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_COLD, dir, damroll(5 + to_s2, 18), 2 + to_s2);
                        break;

                case 16: /* Stun */
                        fire_ball(GF_OLD_CONF, 0, damroll(5 * mto_s2, 5), 6 + to_s2);
                        fire_ball(GF_OLD_SLEEP, 0, damroll(5 * mto_s2, 5), 6 + to_s2);
                        fire_ball(GF_OLD_SLOW, 0, damroll(5 * mto_s2, 5), 6 + to_s2);
                        break;
                case 17: /* Remove Evil */
                        remove_curse();
                        break;
                case 18: /* Destroy Traps */
                        fire_ball(GF_KILL_TRAP, 0, 1, 2 + to_s2);
                        break;
                case 19: /* Lore */
                        ident_spell();
                        break;
                case 20: /* Invoked Destruction */
                        invoke(GF_FIRE, damroll(75, 2));
                        break;
                case 21: /* Ruins */
                        fire_ball(GF_DESTRUCTION, 0, 180, 6 + to_s2);
                        break;
                case 22: /* Fright */
                        scare_monsters();
                        break;
                case 23: /* Winds of Displacement */
                        fire_ball(GF_AWAY_ALL, 0, 180, 1 + to_s2);
                        break;

                case 24: /* Death Chant */
                        dec_stat(A_STR, 15, FALSE);
                        dec_stat(A_CON, 15, FALSE);
                        dec_stat(A_DEX, 15, FALSE);
                        dec_stat(A_INT, 15, FALSE);
                        take_hit(25, "casting Death Chant");
                        fire_ball(GF_MANA, 0, damroll(200 + to_s2, 4), 4 + to_s2);
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_MANA, dir, damroll(40 + to_s2, 20), 2 + to_s2);
                        break;
                case 25: /* Storm's Fury */
                        project_meteor(3, GF_ELEC, damroll(1 + to_s2, 150), PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_THRU);
                        break;
                case 26: /* Call Hydras */
                        summon_specific_friendly(py, px, dun_level, SUMMON_HYDRA, TRUE);
                        break;
                case 27: /* Lifeblood */
                        hp_player(damroll(1000, 2 + to_s2));
                        heal_insanity(damroll(110, 2 + to_s2));
                        set_cut(0);
                        set_poisoned(0);
                        set_stun(0);
                        set_shield(p_ptr->shield + 10 + randint(10) + to_s2, 50 + to_s2);
                        do_res_stat(A_STR);
                        do_res_stat(A_INT);
                        do_res_stat(A_WIS);
                        do_res_stat(A_DEX);
                        do_res_stat(A_CON);
                        do_res_stat(A_CHR);
                        lose_exp(p_ptr->exp / 6);
                        break;
                case 28: /* Invoked Winds */
                        invoke(GF_AWAY_ALL, damroll(1, 200));
                        break;
                case 29: /* Dispel Evil */
                        fire_ball(GF_HOLY_FIRE, 0, 100 * mto_s2, 10 + to_s2);
                        dispel_evil(200 * mto_s2);
                        break;
                case 30: /* Mystic Forces */
                        fire_ball(GF_ELEC, 0, damroll(100, 10 + to_s2), 10 + to_s2);
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_MANA, dir, damroll(1 + to_s2, 400), 5 + to_s2);
                        fire_beam(GF_ICE, dir, damroll(1 + to_s2, 100));
                        break;
                case 31: /*  */
                        break;

                default:
                        msg_format("You cast an unknown Tribal spell: %d.", spell);
                        msg_print(NULL);
        }
}

static void cast_druid_spell(int spell)
{
        int     dir, beam;
        int     plev = p_ptr->lev;
        int to_s2=p_ptr->to_s/2;
        int mto_s2=p_ptr->to_s/2;

        int amt = 0, att = 0;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
                case 0: /* Tunnel */
                {
                        magic_type *s_ptr = &realm_info[REALM_DRUID][0];
                        int d, i, min, ox, oy, x=0, y=0;
                        int tries = 0, dis = 10 + to_s2;
                        int xx = -1, yy = -1;
                        bool look = TRUE;

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
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_ACID, dir,
                                damroll(3+((plev-5)/4), 4 * mto_s2));
                        break;
                case 3: /* Mana Path */
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
                                if(get_check("Do you want to absord the old mana before laying?"))
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
                        byte rad = 2 + (plev / 25) + to_s2;

                        for(j = py - rad; j < py + rad; j++)
                        for(i = px - rad; i < px + rad; i++)
                                if((distance(py, px, j, i) <= rad) && in_bounds(j,i))
                                {
                                        if((cave[j][i].mana >= 20) && cave_clean_bold(j, i) && cave_empty_bold(j, i))
                                        {
                                                cave[j][i].mana -= 20;
                                                cave_set_feat(j, i, FEAT_TREES);
                                        }
                                }
                        break;
                }
                case 5: /* Druidistic Acid Beam */
                        if (!get_aim_dir(&dir)) return;
                        fire_druid_beam(GF_ACID, dir,
                                damroll(5+((plev-5)/4), 4 * mto_s2));
                        break;
                case 6: /* Raise Mountains */
                {
                        int i,j;
                        byte rad = 2 + (plev / 25) + to_s2;

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
                        set_shield(p_ptr->shield + 10 + randint(10) + to_s2, 30 + (p_ptr->to_s * 2));
                        break;

                case 8: /* Infra */
                        set_tim_infra(p_ptr->tim_infra + 10 + randint(15) + to_s2);
                        break;
                case 9: /* Fire Bolt */
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_FIRE, dir,
                                damroll(6 + ((plev-5)/4), 7 * mto_s2));
                        break;
                case 10: /* Fire Ball */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_FIRE, dir,
                                (40 + ((plev-5)/4)) * mto_s2, 2 + to_s2);
                        break;
                case 11: /* Enlight Traps */
                        detect_traps();
                        break;
                case 12: /* Fire Beam */
                        if (!get_aim_dir(&dir)) return;
                        fire_beam(GF_FIRE, dir,
                                damroll(7 + ((plev-5)/4), 8 * mto_s2));
                        break;
                case 13: /* Druidistic Fire Bolt */
                        if(!p_ptr->class_extra5)
                        {
                        if (!get_aim_dir(&dir)) return;
                        fire_druid_bolt(GF_FIRE, dir,
                                damroll(7 + ((plev-5)/4), 8 * mto_s2));
                        }else msg_print("This level has already been drained!");
                        break;
                case 14: /* Druidistic Fire Beam */
                        if(!p_ptr->class_extra5)
                        {
                        if (!get_aim_dir(&dir)) return;
                        fire_druid_beam(GF_ACID, dir,
                                damroll(7 + ((plev-5)/4), 8 * mto_s2));
                        }else msg_print("This level has already been drained!");
                        break;
                case 15: /* Create Lava */
                {
                        int i,j;
                        byte rad = 2 + (plev / 25) + to_s2;

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
                                if(get_check("Do you want to absord the old mana before laying?"))
                                        att |= CLASS_MANA_PATH_ERASE;

                                p_ptr->class_extra7 = CLASS_WINDS_MANA;
                                p_ptr->class_extra6 = amt + (att << 8) ;
                        }
                        }else msg_print("This level has already been drained!");
                        break;
                case 17: /* Summon Air Elem */
                {
                        int xx = px,yy = py;
                        msg_format("You magically summon an Air Elemental.");
                        scatter(&yy, &xx, py, px, 6, 0);
                        place_monster_aux(yy, xx, test_monster_name("Air elemental"), FALSE, FALSE, TRUE);
                        break;
                }
                case 18: /* Wispers from Afar */
                        ident_spell();
                        break;
                case 19: /* The Winds of Manwe */
                        fire_ball(GF_GRAVITY, 0, 10 * mto_s2, 4 + to_s2);
                        break;
                case 20: /* Bird View */
                        wiz_lite_extra();
                        break;
                case 21: /* *Wispers from Afar* */
                        identify_fully();
                        break;
                case 22: /* Windy Speed */
                        set_fast(p_ptr->fast + 5 + randint(5) + to_s2);
                        break;
                case 23: /* The Thunders of Manwe */
                        if(get_check("Do you want it to be a druidistic beam ?"))
                        {
                                if(!p_ptr->class_extra5)
                                {
                                        if (!get_aim_dir(&dir)) return;
                                        fire_druid_beam(GF_ELEC, dir,
                                                damroll(10 + ((plev-5)/4), 10 * mto_s2));
                                }else msg_print("This level has already been drained!");
                        }
                        else
                        {
                                if (!get_aim_dir(&dir)) return;
                                fire_beam(GF_ELEC, dir,
                                        damroll(10 + ((plev-5)/4), 9 * mto_s2));
                        }
                        break;

                case 24: /* Summon Aqua Golems */
                {
                        int xx = px,yy = py;
                        msg_format("You magically summon Aquatic Golems.");
                        scatter(&yy, &xx, py, px, 6, 0);
                        place_monster_aux(yy, xx, test_monster_name("Aquatic golem"), FALSE, TRUE, TRUE);
                        break;
                }
                case 25: /* Walk over the Water */
                        set_walk_water(p_ptr->walk_water + 50 + randint(40) + to_s2);
                        break;
                case 26: /* Flood */
                {
                        int i,j;
                        byte rad = 3 + (plev / 25) + to_s2;

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
                        msg_format("You magically summon Water Elementals.");
                        scatter(&yy, &xx, py, px, 6, 0);
                        place_monster_aux(yy, xx, test_monster_name("Water elemental"), FALSE, TRUE, TRUE);
                        break;
                }
                        break;
                case 28: /* Purification */
                        hp_player(300 + (p_ptr->to_s * 20));
                        set_blind(0);
                        set_confused(0);
                        set_poisoned(0);
                        set_stun(0);
                        set_cut(0);
                        break;
                case 29: /* Go Underwater */
                {
                        int ij, ii;

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
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_WATER, dir, 400 * mto_s2, 4 + to_s2);
                        break;
                case 31: /* Flood Level */
                        msg_print("The world is being flooded !");
                        p_ptr->class_extra6 |= CLASS_FLOOD_LEVEL;
                        alter_reality();                        
                        break;

                case 32: /* Glyph of Warding */
                        warding_glyph();
                        break;
                case 33: /* Orb of Mana */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_MANA, dir, damroll(45, 10) * mto_s2 + (plev * 2), 2 + to_s2);
                        break;
                case 34: /* Gather mana */
                        if(!p_ptr->class_extra5)
                        {
                                
                        }else msg_print("This level has already been drained!");
                        break;
                case 35: /* Mirror of Mana */
                        set_tim_reflect(p_ptr->tim_reflect + 10 + randint(10) + to_s2);
                        break;
                case 36: /* Activate Rune of Mana */
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
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
                                damroll(10+((plev-5)/4), 8 * mto_s2));
                        fire_bolt_or_beam(beam, GF_COLD, dir,
                                damroll(10+((plev-5)/4), 8 * mto_s2));
                        fire_bolt_or_beam(beam, GF_ACID, dir,
                                damroll(10+((plev-5)/4), 8 * mto_s2));
                        fire_bolt_or_beam(beam, GF_ELEC, dir,
                                damroll(10+((plev-5)/4), 8 * mto_s2));
                        fire_bolt_or_beam(beam, GF_MANA, dir,
                                damroll(10+((plev-5)/4), 8 * mto_s2));
                        break;
                case 38: /* Shield of Mana */
                        set_shield(p_ptr->shield + 20 + randint(10) + to_s2, 100);
                        break;
                case 39: /* Drain Level's Mana */
                        if(!p_ptr->class_extra5)
                        {
                                int i, j;
                                long amt = 0;

                                for(i = 0; i < cur_wid; i++)
                                        for(j = 0; j < cur_hgt; j++)
                                        {
                                                amt += cave[j][i].mana / 75;
                                        }                                                

                                p_ptr->class_extra5 = TRUE;
                                if (!get_aim_dir(&dir)) return;
                                (void)fire_bolt(GF_MANA, dir, amt);
                        }else msg_print("This level has already been drained!");
                        break;

                default:
                        msg_format("You cast an unknown Druid spell: %d.", spell);
                        msg_print(NULL);
        }
}

/*
 * Basicaly cast the given spell of the given realm
 */
void cast_spell(int realm, int spell)
{
		switch (realm)
		{
                case REALM_VALARIN: /* * VALARIN * */
                        cast_valarin_spell(spell);
			break;
                case REALM_MAGERY: /* * MAGERY * */
                        cast_magery_spell(spell);
			break;
                case REALM_SHADOW: /* * SHADOW * */
                        cast_shadow_spell(spell);
			break;
		case REALM_CHAOS: /* * CHAOS * */
			cast_chaos_spell(spell);
			break;
                case REALM_NETHER: /* * NETHER * */
                        cast_nether_spell(spell);
			break;
                case REALM_CRUSADE: /* CRUSADE */
                        cast_crusade_spell(spell);
			break;
                case REALM_SIGALDRY: /* SIGALDRY */
                        cast_sigaldry_spell(spell);
			break;
                case REALM_SYMBIOTIC: /* SYMBIOTIC */
                        cast_symbiotic_spell(spell);
			break;
                case REALM_MUSIC: /* MUSIC */
                        cast_music_spell(spell);
			break;
                case REALM_MAGIC: /* MAGIC */
                        cast_magic_spell(spell);
			break;
                case REALM_PRAYER: /* PRAYER */
                        cast_prayer_spell(spell);
			break;
                case REALM_ILLUSION: /* ILLUSION */
                        cast_illusion_spell(spell);
			break;
                case REALM_TRIBAL: /* TRIBAL */
                        cast_tribal_spell(spell);
			break;
                case REALM_DRUID: /* DRUIDISTIC */
                        cast_druid_spell(spell);
			break;
		case REALM_DAEMON: /* DAEMON -SC- */
			cast_daemon_spell(spell);
			break;
		default:
			msg_format("You cast a spell from an unknown realm: realm %d, spell %d.", realm, spell);
			msg_print(NULL);
		}
}

/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int	item, sval, spell, realm;
	int	chance;
	
        const cptr prayer = ((mp_ptr->spell_book == TV_VALARIN_BOOK) ? "prayer" : (mp_ptr->spell_book == TV_MUSIC_BOOK) ? "song" : "spell");
	
	object_type	*o_ptr;
	
	magic_type	*s_ptr;
	
	cptr q, s;

        /* No magic */
        if (p_ptr->antimagic)
        {
                msg_print("Your anti-magic field disrupts any magic attemps.");
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
                sval, TRUE, o_ptr))
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
		
		if (o_ptr->tval == TV_CHAOS_BOOK && (randint(100)<spell))
		{
			msg_print("You produce a chaotic effect!");
			wild_magic(spell);
		}
                else if (o_ptr->tval == TV_MUSIC_BOOK && (randint(100)<spell))
		{
                        msg_print("You produce a horrible shriek!");
                        shriek_effect();
		}
                else if (o_ptr->tval == TV_NETHER_BOOK && (randint(100)<spell))
		{
			if ((sval == 3) && (randint(2)==1))
			{
				sanity_blast(0, TRUE);
			}
			else
			{
				msg_print("It hurts!");
                                take_hit(damroll((o_ptr->sval)+1,6), "a miscast Nether spell");
				if (spell>15 && randint(6)==1 && !(p_ptr->hold_life))
					lose_exp(spell * 250);
			}
		}
	}
	
	/* Process spell */
	else
	{
		/* Spells.  */
                cast_spell(realm, spell);

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
        if (p_ptr->pclass == CLASS_DAEMONOLOGIST)
        {
                if ((o_ptr->tval == TV_DAEMON_BOOK) && (item == INVEN_WIELD))
                {
                        energy_use = 33;
                }
                else if ((o_ptr->tval == TV_DAEMON_BOOK) && (item != INVEN_WIELD))
                {
                        energy_use = 500;
                }
                else
                {
                        energy_use = 100;
                }
        }
        else
        {
                energy_use = 100;
        }

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

