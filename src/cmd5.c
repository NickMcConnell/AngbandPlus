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
static int get_spell(int *sn, cptr prompt, int sval, bool known, bool realm_2, object_type *o_ptr)
{
	int         i;
	int         spell = -1;
	int         num = 0;
	int         ask;
	byte        spells[64];
	bool        flag, redraw, okay;
	char        choice;
	magic_type  *s_ptr;
	char        out_val[160];
	int         use_realm = (realm_2?p_ptr->realm2:p_ptr->realm1);
	cptr        p = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell");

        if (p_ptr->pclass == CLASS_SORCERER)
        {
                use_realm = o_ptr->tval - TV_LIFE_BOOK + 1;
        }

#ifdef ALLOW_REPEAT /* TNB */

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

#endif /* ALLOW_REPEAT -- TNB */

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
		if (spell_okay(spells[i], known, use_realm-1)) okay = TRUE;
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
                                print_spells(spells, num, 1, 13, use_realm-1);
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
		if (!spell_okay(spell, known, use_realm-1))
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
			s_ptr = &mp_ptr->
                        info[use_realm-1][spell%32];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
				prompt, spell_names[use_realm-1][spell%32],
				s_ptr->smana, spell_chance(spell,use_realm-1));

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


static void rustproof(void)
{
	int         item;
	object_type *o_ptr;
	char        o_name[80];
	cptr        q, s;

	/* Select a piece of armour */
	item_tester_hook = item_tester_hook_armour;
	
	/* Get an item */
	q = "Rustproof which piece of armour? ";
	s = "You have nothing to rustproof.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;
	
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
	
	
	/* Description */
	object_desc(o_name, o_ptr, FALSE, 0);
	
	o_ptr->art_flags3 |= TR3_IGNORE_ACID;
	
	if ((o_ptr->to_a < 0) && !(o_ptr->ident & IDENT_CURSED))
	{
		msg_format("%s %s look%s as good as new!",
			((item >= 0) ? "Your" : "The"), o_name,
			((o_ptr->number > 1) ? "" : "s"));
		o_ptr->to_a = 0;
	}
	
	msg_format("%s %s %s now protected against corrosion.",
		((item >= 0) ? "Your" : "The"), o_name,
		((o_ptr->number > 1) ? "are" : "is"));
	
}


/*
 * Peruse the spells/prayers in a book
 *
 * Note that *all* spells in the book are listed
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(void)
{
	int		item, sval;
	int		spell = -1;
	int		num = 0;
	
	byte		spells[64];
	
	object_type	*o_ptr;
	
	cptr q, s;

        if(p_ptr->pclass == CLASS_POWERMAGE)
        {
                random_spell *s_ptr;
                s_ptr = select_spell(TRUE);
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
	Term_save();
	
	/* Display the spells */
        print_spells(spells, num, 1, 13, (o_ptr->tval-90));
	
	/* Clear the top line */
	prt("", 0, 0);
	
	/* Prompt user */
	put_str("[Press any key to continue]", 0, 23);
	
	/* Wait for key */
	(void)inkey();
	
	/* Restore the screen */
	Term_load();
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
	
	cptr p = ((mp_ptr->spell_book == TV_SORCERY_BOOK) ? "spell" : "prayer");
	
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
	
	if (o_ptr->tval==p_ptr->realm2+89) increment=32;
	
	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);
	
	/* Hack -- Handle stuff */
	handle_stuff();
	
	/* Mage -- Learn a selected spell */
	if (mp_ptr->spell_book != TV_LIFE_BOOK)
	{
		/* Ask for a spell, allow cancel */
                if (!get_spell(&spell, "study", sval, FALSE, (bool)(increment ? TRUE : FALSE), o_ptr)
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
					(increment?(p_ptr->realm2)-1:(p_ptr->realm1)-1))) continue;
				
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
	
	if (increment) spell += increment;
	
	/* Learn the spell */
	if (spell < 32)
	{
		spell_learned1 |= (1L << spell);
	}
	else
	{
		spell_learned2 |= (1L << (spell - 32));
	}
	
	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < 64; i++)
	{
		/* Stop at the first empty space */
		if (spell_order[i] == 99) break;
	}
	
	/* Add the spell to the known list */
	spell_order[i++] = spell;
	
	/* Mention the result */
	msg_format("You have learned the %s of %s.",
		p, spell_names
		[(increment?(p_ptr->realm2)-1:(p_ptr->realm1)-1)][spell%32]);
	
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
 * Charge a lite (torch or latern)
 */
static void phlogiston(void)
{
	int max_flog = 0;
	object_type * o_ptr = &inventory[INVEN_LITE];
	
	/* It's a lamp */
	if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_LANTERN))
	{
		max_flog = FUEL_LAMP;
	}
	
	/* It's a torch */
	else if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_TORCH))
	{
		max_flog = FUEL_TORCH;
	}
	
	/* No torch to refill */
	else
	{
		msg_print("You are not wielding anything which uses phlogiston.");
		return;
	}
	
	if (o_ptr->pval >= max_flog)
	{
		msg_print("No more phlogiston can be put in this item.");
		return;
	}
	
	/* Refuel */
	o_ptr->pval += (max_flog / 2);
	
	/* Message */
	msg_print("You add phlogiston to your light item.");
	
	/* Comment */
	if (o_ptr->pval >= max_flog)
	{
		o_ptr->pval = max_flog;
		msg_print("Your light item is full.");
	}
	
	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);
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
			act = "is engulfed in raw Logrus!";
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
			((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
			((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell"));
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
                        summon_specific(py, px, p_ptr->max_dlv[dungeon_type], 0);
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



static void cast_life_spell(int spell)
{
	int	dir;
	int	plev = p_ptr->lev;
        int to_s2=p_ptr->to_s/2;
        to_s2 = (to_s2==0)?1:to_s2;
	
	switch (spell)
	{
	   case 0: /* Detect Evil */
			(void)detect_monsters_evil();
		       break;
	   case 1: /* Cure Light Wounds */
                        (void)hp_player(damroll(2, 10)+to_s2);
			(void)set_cut(p_ptr->cut - 10);
		       break;
	   case 2: /* Bless */
                        (void)set_blessed(p_ptr->blessed + randint(12)+to_s2 + 12);
		       break; 
	   case 3: /* Remove Fear */
			(void)set_afraid(0);
		       break;
	   case 4: /* Call Light */
                        (void)lite_area(damroll(2, (plev / 2)), (plev / 10)*to_s2 + 1);
		       break;
	   case 5: /* Detect Traps + Secret Doors */
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
		       break;
	   case 6: /* Cure Medium Wounds */
                        (void)hp_player(damroll(4, 10)+to_s2);
                        (void)set_cut((p_ptr->cut / 2) - 20-p_ptr->to_s);
		       break;
	   case 7: /* Satisfy Hunger */
			(void)set_food(PY_FOOD_MAX - 1);
		       break;
	   case 8: /* Remove Curse */
			remove_curse();
		       break;
	   case 9: /* Cure Poison */
			(void)set_poisoned(0);
		       break;
	   case 10: /* Cure Critical Wounds */
                        (void)hp_player(damroll(8, 10)+to_s2);
			(void)set_stun(0);
			(void)set_cut(0);
		       break;
	   case 11: /* Sense Unseen */
                        (void)set_tim_invis(p_ptr->tim_invis + randint(24)+to_s2 + 24);
		       break;
	   case 12: /* Orb or Draining */
	   if (!get_aim_dir(&dir)) return;
            fire_ball(GF_HOLY_FIRE, dir,
				(damroll(3, 6) + plev +
                    (plev / ((p_ptr->pclass == 2
                              || p_ptr->pclass == CLASS_HIGH_MAGE) ? 2 : 4)))*to_s2,
                                        ((plev < 30) ? 2 : 3)+to_s2);
		       break;
	   case 13: /* Protection from Evil */
                        (void)set_protevil(p_ptr->protevil + randint(25)+to_s2 + 3 * p_ptr->lev);
		       break;
	   case 14: /* Healing */
                        (void)hp_player(300*to_s2);
			(void)set_stun(0);
			(void)set_cut(0);
		       break;
	   case 15: /* Glyph of Warding */
			warding_glyph();
		       break;
       case 16: /* Exorcism */
         (void) dispel_undead(plev+p_ptr->to_s);
         (void) dispel_demons(plev+p_ptr->to_s);
         (void) turn_evil(plev+p_ptr->to_s);
               break;
	   case 17: /* Dispel Curse */
			(void)remove_all_curse();
		       break;
       case 18: /* Dispel Undead + Demons */
            (void)dispel_undead(plev * 3*to_s2);
        (void)dispel_demons(plev * 3*to_s2);
			break;
       case 19: /* 'Day of the Dove' */
                  charm_monsters(plev * 2*to_s2);
		       break;
       case 20: /* Dispel Evil */
                        (void)dispel_evil(plev * 4*to_s2);
		       break;
	   case 21: /* Banishment */
                        if (banish_evil(100*to_s2))
			{
				msg_print("The power of your god banishes evil!");
			}
			break;
	   case 22: /* Holy Word */
           (void)dispel_evil(plev * 4*to_s2);
                        (void)hp_player(1000*to_s2);
			(void)set_afraid(0);
			(void)set_poisoned(0);
			(void)set_stun(0);
			(void)set_cut(0);
		       break;
	   case 23: /* Warding True */
		warding_glyph();
		glyph_creation();
		       break;
	   case 24: /* Heroism */
                        (void)set_hero(p_ptr->hero + randint(25)+to_s2 + 25);
			(void)hp_player(10);
			(void)set_afraid(0);
		       break;
	   case 25: /* Prayer */
                        (void)set_blessed(p_ptr->blessed + randint(48)+to_s2 + 48);
		       break;
#if 0 /* Old version */
	   case 26: /* Healing II */
			(void)hp_player(800);
			(void)set_stun(0);
			(void)set_cut(0);
		       break;
#else
       case 26:
            bless_weapon();
            break;
#endif
	   case 27: /* Restoration */
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_CHR);
			(void)restore_level();
		       break;
       case 28: /* Healing True */
                        (void)hp_player(2000*to_s2);
			(void)set_stun(0);
			(void)set_cut(0);
		       break;
       case 29: /* Holy Vision */
		identify_fully();
		       break;
       case 30: /* Divine Intervention */
         project(0, 1, py, px, 777, GF_HOLY_FIRE,   PROJECT_KILL);
         dispel_monsters(plev * 4*to_s2);
         slow_monsters();
         stun_monsters(plev*4*to_s2);
         confuse_monsters(plev*4*to_s2);
         turn_monsters(plev*4*to_s2);
         stasis_monsters(plev*4*to_s2);
         summon_specific_friendly(py, px, plev, SUMMON_ANGEL, TRUE);
         (void)set_shero(p_ptr->shero + randint(25)+to_s2 + 25);
         (void)hp_player(300*to_s2);
         if (!p_ptr->fast) {   /* Haste */
         (void)set_fast(randint(20 + (plev) ) + plev+to_s2);
         } else {
         (void)set_fast(p_ptr->fast + randint(5)+to_s2);
         }
         (void)set_afraid(0);
         break;
	   case 31: /* Holy Invulnerability */
                        (void)set_invuln(p_ptr->invuln + randint(7)+to_s2 + 7);
		       break;
	       default:
		 msg_format("You cast an unknown Life spell: %d.", spell);
		 msg_print(NULL);
	   }
}



static void cast_sorcery_spell(int spell)
{
	int	dir;
	int	plev = p_ptr->lev;
	int	ii = 0, ij = 0;
        int to_s2=p_ptr->to_s/2;
        to_s2 = (to_s2==0)?1:to_s2;
	
	switch (spell)
	{
	   case 0: /* Detect Monsters */
			(void)detect_monsters_normal();
		       break;
	   case 1: /* Phase Door */
                        teleport_player(10*to_s2);
		       break;
	   case 2: /* Detect Doors and Traps */
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
		       break; 
       case 3: /* Light Area */
                        (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1+to_s2);
            break;
	   case 4: /* Confuse Monster */
			if (!get_aim_dir(&dir)) return;
            (void)confuse_monster(dir, ( plev * 3) / 2 *to_s2);
			break;
	   case 5: /* Teleport Self */
            teleport_player(plev * 5*to_s2);
		       break;
	   case 6: /* Sleep Monster */
			if (!get_aim_dir(&dir)) return;
			(void)sleep_monster(dir);
		       break;
	   case 7: /* Recharging */
               (void)recharge(plev * 2+(p_ptr->to_s*2));
		       break;
	   case 8: /* Magic Mapping */
			map_area();
		       break;
	   case 9: /* Identify */
			(void)ident_spell();
		       break;
           case 10: /* Invisibility */
                        ii = p_ptr->lev/2 + randint(p_ptr->lev) + p_ptr->to_s;
                        set_invis(p_ptr->tim_invis + ii, 35);
                        set_tim_invis(p_ptr->tim_invis + ii);
		       break;
	   case 11: /* Mass Sleep */
			(void)sleep_monsters();
		       break;
	   case 12: /* Teleport Away */
			if (!get_aim_dir(&dir)) return;
               (void)fire_beam(GF_AWAY_ALL, dir, plev*to_s2);
		       break;
	   case 13: /* Haste Self */
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev) ) + plev+to_s2);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5)+to_s2);
			}
		       break;
	   case 14: /* Detection True */
			(void)detect_all();
		       break;
	   case 15: /* Identify True */
			identify_fully();
		       break;
       case 16: /* Detect Objects and Treasure*/
			(void)detect_objects_normal();
			(void)detect_treasure();
			(void)detect_objects_gold();
		       break;
       case 17: /* Detect Enchantment */
			(void)detect_objects_magic();
		       break;
       case 18: /* Charm Monster */
                 if (!get_aim_dir(&dir)) return;
                 (void) charm_monster(dir, plev*to_s2);
               break;
       case 19: /* Between gate */
       {
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

       case 20: /* Sense Minds */
            (void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25+to_s2);
		       break;
       case 21: /* Self knowledge */
           (void)self_knowledge();
               break;
	   case 22: /* Teleport Level */
			(void)teleport_player_level();
		       break;
	   case 23: /* Word of Recall */
			{
                                if (special_flag)
                                {
                                msg_print("No recall on special levels..");
                                break;
                                }
				recall_player();
				break;
			}
       case 24: /* Stasis */
			if (!get_aim_dir(&dir)) return;
			(void)stasis_monster(dir);
		       break;
       case 25: /* Telekinesis */
         if (!get_aim_dir(&dir)) return;
         fetch(dir, plev*15+to_s2, FALSE);
         break;
       case 26: /* Recharging True -- replaced by Explosive Rune */
               explosive_rune();
		       break;
	   case 27: /* Clairvoyance */
			wiz_lite();
            if (!(p_ptr->telepathy))
            {
                (void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25+to_s2);
            }
		       break;
	   case 28: /* Enchant Weapon */
                        (void)enchant_spell(rand_int(4) + 1+to_s2, rand_int(4) + 1+to_s2, 0, 0);
		       break;
	   case 29: /* Enchant Armour */
                        (void)enchant_spell(0, 0, rand_int(3) + 2+to_s2, 0);
		       break;
	   case 30: /* Alchemy */
		       (void) alchemy();
		       break;
	   case 31: /* Globe of Invulnerability */
                        (void)set_invuln(p_ptr->invuln + randint(8)+to_s2 + 8);
		       break;
	   default:
		   msg_format("You cast an unknown Sorcery spell: %d.", spell);
		   msg_print(NULL);
	   }
}


static void cast_nature_spell(int spell)
{
	int	    dir;
	int	    beam;
	int	    plev = p_ptr->lev;
	bool    no_trump = FALSE;
        int to_s2=p_ptr->to_s/2;
        to_s2 = (to_s2==0)?1:to_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
	   case 0: /* Detect Creatures */
			(void)detect_monsters_normal();
		       break;
	   case 1: /* First Aid */
                        (void)hp_player(damroll(2, 8)+to_s2);
                        (void)set_cut(p_ptr->cut - 15-p_ptr->to_s);
		       break;
	   case 2: /* Detect Doors + Traps */
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
		       break; 
	   case 3: /* Produce Food */
			(void)set_food(PY_FOOD_MAX - 1);
		       break;
       case 4: /* Daylight */
               (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1+to_s2);
            if (((p_ptr->prace == RACE_VAMPIRE)||(p_ptr->mimic_form == MIMIC_VAMPIRE)) && !(p_ptr->resist_lite))
            {
                msg_print("The daylight scorches your flesh!");
                take_hit(damroll(2,2), "daylight");
                            }
               break;
       case 5: /* Animal Taming */
         if (!get_aim_dir(&dir)) return;
         (void) charm_animal(dir, plev*to_s2);
         break;
       case 6: /* Resist Environment */
                        (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20+to_s2);
                        (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20+to_s2);
                        (void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20+to_s2);
		       break;
       case 7: /* Cure Wounds + Poison */
            (void)set_cut(0);
			(void)set_poisoned(0);
		       break;
	   case 8: /* Stone to Mud */
			if (!get_aim_dir(&dir)) return;
			(void)wall_to_mud(dir);
		       break;
	   case 9: /* Lightning Bolt */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                                  damroll(3+((plev-5)/4), 8)*to_s2);
		       break;
       case 10: /* Nature Awareness -- downgraded */
			map_area();
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			(void)detect_monsters_normal();
            break;
	   case 11: /* Frost Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam-10, GF_COLD, dir,
                                damroll(5+((plev-5)/4)*to_s2, 8));
		       break;
#if 0
	   case 12: /* Ray of Sunlight */
			if (!get_aim_dir(&dir)) return;
			msg_print("A line of sunlight appears.");
			lite_line(dir);
		       break;
#else
           case 12: /* Stop breeders */
                       set_no_breeders(no_breeds + randint(10) + 10 + p_ptr->to_s);
		       break;
#endif
	   case 13: /* Entangle */
			slow_monsters();
		       break;
       case 14: /* Summon Animals */
             if (!(summon_specific_friendly(py, px, plev+to_s2, SUMMON_ANIMAL_RANGER, TRUE)))
                no_trump = TRUE;
             break;
      case 15: /* Herbal Healing */
                        (void)hp_player(1000+(p_ptr->to_s*3));
			(void)set_stun(0);
			(void)set_cut(0);
			(void)set_poisoned(0);
		       break;
       case 16: /* Door Building */
			(void)door_creation();
		       break;
       case 17: /* Stair Building */
			(void)stair_creation();
		       break;
       case 18: /* Stone Skin */
                        (void)set_shield(p_ptr->shield + randint(20) + 30+to_s2);
		       break;
       case 19: /* Resistance True */
                        (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20+to_s2);
                        (void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20+to_s2);
                        (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20+to_s2);
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
			(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
		       break;
        case 20: /* Animal Friendship */
        (void) charm_animals(plev * 2*to_s2);
         break;
	   case 21: /* Stone Tell */
		identify_fully();
		       break;
       case 22: /* Wall of Stone */
		(void)wall_stone();
		       break;
       case 23: /* Protection from Corrosion */
               rustproof();
		       break;
       case 24: /* Earthquake */
                        earthquake(py, px, 10+to_s2);
		       break;
       case 25: /* Whirlwind Attack */
         {
         int y = 0, x = 0;
         cave_type       *c_ptr;
         monster_type    *m_ptr;

         for (dir = 0; dir <= 9; dir++) {
             y = py + ddy[dir];
             x = px + ddx[dir];
             c_ptr = &cave[y][x];

             /* Get the monster */
             m_ptr = &m_list[c_ptr->m_idx];

             /* Hack -- attack monsters */
             if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x)))
           py_attack(y, x);
         }
         }
         break;
       case 26: /* Blizzard */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_COLD, dir,
                                70 + (plev)*to_s2, (plev/12)*to_s2+1);
		       break;
	   case 27: /* Lightning Storm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_ELEC, dir,
                                90 + (plev)*to_s2, (plev/12)*to_s2+1);
		       break;
	   case 28: /* Whirlpool */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_WATER, dir,
                                100 + (plev)*to_s2, (plev/12)*to_s2+1);
		       break;
	   case 29: /* Call Sunlight */

                        fire_ball(GF_LITE, 0, 150*to_s2, 8*to_s2);
			wiz_lite();
            if (((p_ptr->prace == RACE_VAMPIRE)||(p_ptr->mimic_form == MIMIC_VAMPIRE)) && !(p_ptr->resist_lite))
            {
                msg_print("The sunlight scorches your flesh!");
                take_hit(50, "sunlight");
            }
		       break;
	   case 30: /* Elemental Brand */
            brand_weapon(0);
		       break;
	   case 31: /* Nature's Wrath */
            (void)dispel_monsters(plev * 4);
            earthquake(py, px, 20 + (plev / 2)+to_s2 );
         project(0, 1+plev/12, py, px,
             100+plev, GF_DISINTEGRATE, PROJECT_KILL|PROJECT_ITEM);
		       break;
	default:
		msg_format("You cast an unknown Nature spell: %d.", spell);
		msg_print(NULL);
	}
	if (no_trump)
		msg_print("No animals arrive.");
}


static void cast_chaos_spell(int spell)
{
	int	dir, i, beam;
	int	plev = p_ptr->lev;
        int to_s2=p_ptr->to_s/2;
        to_s2 = (to_s2==0)?1:to_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
		case 0: /* Magic Missile */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                                                  damroll(3 + ((plev - 1) / 5)*to_s2, 4));
                break;
        case 1: /* Trap / Door destruction, was: Blink */
			(void)destroy_doors_touch();
			break;
        case 2: /* Flash of Light == Light Area */
                        (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1+to_s2);
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
                || (p_ptr->pclass == CLASS_HIGH_MAGE)) ? 2 : 4)))*to_s2,
            ((plev < 30) ? 2 : 3)+to_s2);
          /* Shouldn't actually use GF_MANA, as it will destroy all
       * items on the floor */
             break;
        case 5: /* Fire Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
                                damroll(8+((plev-5)/4), 8)*to_s2);
			break;
        case 6: /* Fist of Force ("Fist of Fun") */
			if (!get_aim_dir(&dir)) return;
           fire_ball(GF_DISINTEGRATE, dir,
               damroll(8+((plev-5)/4), 8)*to_s2, p_ptr->to_s);
            break;
		case 7: /* Teleport Self */
                        teleport_player(plev * 5*to_s2);
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
                   damroll(3 + ((plev - 1) / 5), 4)*to_s2);
               else if (die < 41) confuse_monster (dir, plev*to_s2);
               else if (die < 46) fire_ball (GF_POIS, dir, 20 + (plev / 2)*to_s2, 3+to_s2);
               else if (die < 51) lite_line (dir);
               else if (die < 56)
                   fire_bolt_or_beam (beam - 10, GF_ELEC, dir,
                   damroll(3+((plev-5)/4),8)*to_s2);
               else if (die < 61)
                   fire_bolt_or_beam (beam - 10, GF_COLD, dir,
                   damroll(5+((plev-5)/4),8)*to_s2);
               else if (die < 66)
                   fire_bolt_or_beam (beam, GF_ACID, dir,
                   damroll(6+((plev-5)/4),8)*to_s2);
               else if (die < 71)
                   fire_bolt_or_beam (beam, GF_FIRE, dir,
                   damroll(8+((plev-5)/4),8)*to_s2);
               else if (die < 76) drain_life (dir, 75+(p_ptr->to_s*2));
               else if (die < 81) fire_ball (GF_ELEC, dir, 30 + plev*to_s2 / 2, 2+to_s2);
               else if (die < 86) fire_ball (GF_ACID, dir, 40 + plev*to_s2, 2+to_s2);
               else if (die < 91) fire_ball (GF_ICE, dir, 70 + plev*to_s2, 3+to_s2);
               else if (die < 96) fire_ball (GF_FIRE, dir, 80 + plev*to_s2, 3+to_s2);
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
               45+plev*to_s2, GF_SOUND, PROJECT_KILL|PROJECT_ITEM);
                   break;
        case 11: /* Doom Bolt -- always beam in 2.0.7 or later */
				if (!get_aim_dir(&dir)) return;
                fire_beam(GF_MANA, dir, damroll(11+((plev-5)/4), 8)*to_s2);
			break;
		case 12: /* Fire Ball */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_FIRE, dir,
                                        55 + (plev)*to_s2, 2+to_s2);
			break;
		case 13: /* Teleport Other */
           if (!get_aim_dir(&dir)) return;
               (void)fire_beam(GF_AWAY_ALL, dir, plev*to_s2);
			break;
		case 14: /* Word of Destruction */
                        destroy_area(py, px, 15+(p_ptr->to_s*2), TRUE);
			break;
		case 15: /* Invoke Logrus */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_CHAOS, dir,
                                        66 + (plev)*to_s2, (plev / 5)+to_s2);
			break;
        case 16: /* Polymorph Other */
			if (!get_aim_dir(&dir)) return;
			(void)poly_monster(dir);
			break;
        case 17: /* Chain Lightning */
          for (dir = 0; dir <= 9; dir++)
            fire_beam(GF_ELEC, dir, damroll(5+(plev/10), 8)*to_s2);
           break;
        case 18: /* Arcane Binding == Charging */
                        (void)recharge(40+to_s2);
			break;
        case 19: /* Disintegration */
			if (!get_aim_dir(&dir)) return;
           fire_ball(GF_DISINTEGRATE, dir,
               80 + (plev)*to_s2, 3 + (plev/40)+to_s2);
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
                        if (summon_specific(py, px, (plev*3)*to_s2/2, SUMMON_DEMON))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");
				msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
			}
        	}
		else
		{
                        if (summon_specific_friendly(py, px, (plev*3)*to_s2/2,
				SUMMON_DEMON, (plev == 50 ? TRUE : FALSE)))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");
				msg_print("'What is thy bidding... Master?'");
			}
		}
		break;
        case 24: /* Beam of Gravity */
			if (!get_aim_dir(&dir)) return;
                fire_beam(GF_GRAVITY, dir, damroll(9+((plev-5)/4), 8)*to_s2);
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
                                65 + (plev)*to_s2, 3 + (plev/40)+to_s2);
			break;
#endif
		case 26: /* Flame Strike */
			fire_ball(GF_FIRE, 0,
                150 + (2*plev)*to_s2, 8+to_s2);
			break;
        case 27: /* Call Chaos */
            call_chaos();
			break;
        case 28: /* Magic Rocket */
			if (!get_aim_dir(&dir)) return;
            msg_print("You launch a rocket!");
			fire_ball(GF_ROCKET, dir,
                                        120 + (plev)*to_s2, 2+to_s2);
			break;
        case 29: /* Mana Storm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_MANA, dir,
                                300 + (plev * 2)*to_s2, 4+to_s2);
            break;
        case 30: /* Breathe Logrus */
               if (!get_aim_dir(&dir)) return;
               fire_ball(GF_CHAOS,dir,p_ptr->chp*to_s2,
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


static void cast_death_spell(int spell)
{
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
	int	dummy = 0;
	int	i;
        int to_s2=p_ptr->to_s/2;
        to_s2 = (to_s2==0)?1:to_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
       case 0: /* Detect Undead & Demons -> Unlife*/
       (void) detect_monsters_nonliving();
		       break;
       case 1: /* Malediction */
         if (!get_aim_dir(&dir)) return;
         /* A radius-0 ball may (1) be aimed at objects etc.,
          * and will affect them; (2) may be aimed at ANY
          * visible monster, unlike a 'bolt' which must travel
          * to the monster. */

         fire_ball(GF_HELL_FIRE, dir,
           damroll(3 + ((plev - 1) / 5), 3)*to_s2, 0+to_s2);
         if (randint(5)==1) {   /* Special effect first */
         dummy = randint(1000);
         if (dummy == 666)
           fire_bolt(GF_DEATH_RAY, dir, plev*to_s2);
         else if (dummy < 500)
           fire_bolt(GF_TURN_ALL, dir, plev*to_s2);
         else if (dummy < 800)
           fire_bolt(GF_OLD_CONF, dir, plev*to_s2);
         else
           fire_bolt(GF_STUN, dir, plev*to_s2);
         }
         break;
       case 2: /* Detect Evil */
			(void)detect_monsters_evil();
		       break; 
	   case 3: /* Stinking Cloud */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_POIS, dir,
                                10 + (plev / 2)*to_s2, 2+to_s2);
		       break;
	   case 4: /* Black Sleep */
			if (!get_aim_dir(&dir)) return;
			(void)sleep_monster(dir);
		       break;
	   case 5: /* Resist Poison */
                        (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20+to_s2);
		       break;
       case 6: /* Horrify */
			if (!get_aim_dir(&dir)) return;
                        (void)fear_monster(dir, plev*to_s2);
                        (void) stun_monster(dir, plev*to_s2);
		       break;
       case 7: /* Enslave the Undead */
         if (!get_aim_dir(&dir)) return;
           (void)control_one_undead(dir, plev*to_s2);
               break;
       case 8: /* Orb of Entropy */
         if (!get_aim_dir(&dir)) return;
         fire_ball(GF_OLD_DRAIN, dir,
           (damroll(3, 6) + plev +
            (plev / (((p_ptr->pclass == CLASS_MAGE)
            || (p_ptr->pclass == CLASS_HIGH_MAGE)) ? 2 : 4)))*to_s2,
           ((plev < 30) ? 2 : 3)+to_s2);
               break;
       case 9: /* Nether Bolt */
			if (!get_aim_dir(&dir)) return;
            fire_bolt_or_beam(beam, GF_NETHER, dir,
                                damroll(6+((plev-5)/4), 8)*to_s2);
		       break;
        case 10: /* Raise Death */
        {
                int item,x,y;
                object_type *o_ptr;

                cptr q, s;

                /* Restrict choices to corpses */
                item_tester_tval = TV_CORPSE;

                /* Get an item */
                q = "Use which corpse? ";
                s = "You have no corpse to use.";
                if (!get_item(&item, q, s, (USE_FLOOR))) break;;

                o_ptr = &o_list[0 - item];

                        if(randint(8)>=5-o_ptr->sval){
                                msg_print("You touch the corpse ... the monster raise from the graveyard!");

                                x=px;
                                y=py;
                                get_pos_player(100, &y, &x);
                                place_monster_one(y, x, o_ptr->pval2, FALSE, TRUE);

                                floor_item_increase(0 - item, -1);
                                floor_item_describe(0 - item);
                                floor_item_optimize(0 - item);
                        }
                break;
        }

	   case 11: /* Vampiric Drain */
       if (!get_aim_dir(&dir)) return;
       dummy = plev + randint(plev) * MAX(1, plev/10)+(p_ptr->to_s*2);   /* Dmg */
                 if (drain_life(dir, dummy)) {
           (void)hp_player(dummy);
           /* Gain nutritional sustenance: 150/hp drained */
           /* A Food ration gives 5000 food points (by contrast) */
           /* Don't ever get more than "Full" this way */
           /* But if we ARE Gorged,  it won't cure us */
           dummy = p_ptr->food + MIN(5000, 100 * dummy);
           if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
             (void)set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX-1 : dummy);
       }
         break;
       case 12: /* Poison Branding */
            brand_weapon(2);
		       break;
       case 13: /* Cloud Kill -> Dispel Good */
#if 0
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
					  60 + plev, (plev/10)+1);
		       break;
#endif
            (void)dispel_good(plev * 4*to_s2);
		       break;
	   case 14: /* Genocide */
			(void)genocide(TRUE);
		       break;
	   case 15: /* Restore Life */
			(void)restore_level();
		       break;
	   case 16: /* Berserk */
            (void)set_shero(p_ptr->shero + randint(25) + 25+to_s2);
                        (void)hp_player(30+to_s2);
			(void)set_afraid(0);
		       break;
       case 17: /* Invoke Spirits */
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
	   case 18: /* Dark Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_DARK, dir,
                                damroll(4+((plev-5)/4), 8)*to_s2);
		       break;
       case 19: /* Battle Frenzy */
                        (void)set_shero(p_ptr->shero + randint(25) + 25+to_s2);
            (void)hp_player(30);
			(void)set_afraid(0);
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev / 2) ) + (plev / 2)+to_s2);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5)+to_s2);
			}
		       break;
        case 20: /* Vampirism True */
			if (!get_aim_dir(&dir)) return;
           for (dummy = 0; dummy < 3; dummy++)
           {
               if (drain_life(dir, 100+to_s2))
                   hp_player(100+to_s2);
                }
                   break;
        case 21: /* Vampiric Branding */
            brand_weapon(3);
		       break;
       case 22: /* Darkness Storm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_DARK, dir,
                                        120*to_s2, 4+to_s2);
		       break;
        case 23: /* Mass Genocide */
			(void)mass_genocide(TRUE);
		       break;
       case 24: /* Death Ray */
			if (!get_aim_dir(&dir)) return;
                        (void)death_ray(dir, plev*to_s2);
		       break;
       case 25: /* Raise the Dead */
                   if (randint(3) == 1) {
               if (summon_specific(py, px, (plev*3)*to_s2/2,
                       (plev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD))) {
               msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
               msg_print("'The dead arise... to punish you for disturbing them!'");
               }
           } else {
               if (summon_specific_friendly(py, px, (plev*3)*to_s2/2,
                       (plev > 47 ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_UNDEAD),
                       (((plev > 24) && (randint(3) == 1)) ? TRUE : FALSE))) {
               msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
               msg_print("Ancient, long-dead forms arise from the ground to serve you!");
               }
           }
           break;
       case 26: /* Esoteria */
                if (randint(50)>plev+to_s2)
		    (void) ident_spell();
		else
		    identify_fully();
		       break;
       case 27: /* Word of Death */
            (void)dispel_living(plev * 3*to_s2);
		       break;
       case 28: /* Evocation       */
        (void)dispel_monsters(plev * 4*to_s2);
         turn_monsters(plev*4*to_s2);
         banish_monsters(plev*4*to_s2);
		       break;
       case 29: /* Hellfire */
			if (!get_aim_dir(&dir)) return;
            fire_ball(GF_HELL_FIRE, dir,
                    666*to_s2, 3+to_s2);
            take_hit(50+randint(50), "the strain of casting Hellfire");
            break;
        case 30: /* Omnicide */
         p_ptr->csp -= 100;  /* Display doesn't show mana cost (100)
       * as deleted until the spell has finished. This gives a
       * false impression of how high your mana is climbing.
       * Therefore, 'deduct' the cost temporarily before entering the
       * loop, then add it back at the end so that the rest of the
       * program can deduct it properly */
         for (i = 1; i < m_max; i++)
         {
             monster_type    *m_ptr = &m_list[i];
             monster_race    *r_ptr = &r_info[m_ptr->r_idx];

             /* Paranoia -- Skip dead monsters */
             if (!m_ptr->r_idx) continue;

             /* Hack -- Skip Unique Monsters */
             if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Hack -- Skip Quest Monsters */
		if (r_ptr->flags1 & RF1_QUESTOR) continue;


             /* Delete the monster */
             delete_monster_idx(i);

             /* Take damage */
             take_hit(randint(4), "the strain of casting Omnicide");

             /* Absorb power of dead soul */
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
         p_ptr->csp += 100;   /* Restore, ready to be deducted properly */

         break;
        case 31: /* Wraithform */
        set_shadow(p_ptr->wraith_form + randint(plev/2) + (plev/2)+to_s2);
        break;
	default:
		msg_format("You cast an unknown Death spell: %d.", spell);
		msg_print(NULL);
	}
}


static void cast_trump_spell(int spell)
{
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
	int	dummy = 0;
	int	ii = 0, ij = 0;
	bool	no_trump = FALSE;
	char	ppp[80];
	char	tmp_val[160];
        int to_s2=p_ptr->to_s/2;
        to_s2 = (to_s2==0)?1:to_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
        case 0: /* Phase Door */
                        teleport_player(10+to_s2);
        break;
        case 1: /* Mind Blast */
               if (!get_aim_dir(&dir)) return;
                 fire_bolt_or_beam(beam-10, GF_PSI, dir,
                              damroll(3 + ((plev - 1) / 5), 3)*to_s2);
        break;
        case 2: /* Shuffle */

           {
                /* A limited power 'wonder' spell */

               int die = randint(120)+to_s2;

               if ((p_ptr->pclass == CLASS_ROGUE) ||
                   (p_ptr->pclass == CLASS_HIGH_MAGE))
                    die = (randint(110)) + plev / 5;
               /* Card sharks and high mages get a level bonus */

            msg_print("You shuffle the deck and draw a card...");

            if (die < 7 )
            {
                msg_print("Oh no! It's Death!");
                for (dummy = 0; dummy < randint(3); dummy++)
                    (void)activate_hi_summon();
            }
            else if (die < 14)
            {
                msg_print("Oh no! It's the Devil!");
                (void) summon_specific(py, px, dun_level, SUMMON_DEMON);
            }
            else if (die < 18 )
            {
                msg_print("Oh no! It's the Hanged Man.");
                activate_ty_curse();
            }
            else if (die < 22 )
            {
                msg_print("It's the swords of discord.");
                aggravate_monsters(1);
            }
            else if (die < 26)
            {
                msg_print("It's the Fool.");
                (void) do_dec_stat(A_INT);
                (void) do_dec_stat(A_WIS);
            }
            else if (die < 30)
            {
                msg_print("It's the picture of a strange monster.");
                if (!(summon_specific(py, px, (dun_level * 3) / 2, 32 + randint(6))))
                    no_trump = TRUE;
            }
            else if (die < 33)
            {
                msg_print("It's the Moon.");
                unlite_area(10,3);
            }
            else if (die < 38)
            {
                msg_print("It's the Wheel of Fortune.");
                wild_magic((randint(32))-1);
            }
            else if (die < 40)
            {
                msg_print("It's a teleport trump card.");
                teleport_player(10+to_s2);
            }
            else if (die <42)
            {
                msg_print("It's Justice.");
                set_blessed(p_ptr->blessed + p_ptr->lev+to_s2);
            }
            else if (die <47)
            {
                msg_print("It's a teleport trump card.");
                teleport_player(100+to_s2);
            }
            else if (die <52)
            {
                msg_print("It's a teleport trump card.");
                teleport_player(200+to_s2);
            }
            else if (die <60)
            {
                msg_print("It's the Tower.");
                wall_breaker();
            }
            else if (die <72)
            {
                msg_print("It's Temperance.");
                sleep_monsters_touch();
            }
            else if (die <80)
            {
                msg_print("It's the Tower.");
                earthquake(py, px, 5+to_s2);
            }
            else if (die<82)
            {
                msg_print("It's the picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, (dun_level * 3)*to_s2 / 2, SUMMON_BIZARRE1, FALSE)))
                    no_trump = TRUE;
            }
            else if (die<84)
            {
                msg_print("It's the picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, (dun_level * 3)*to_s2 / 2, SUMMON_BIZARRE2, FALSE)))
                    no_trump = TRUE;
            }
            else if (die<86)
            {
                msg_print("It's the picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, (dun_level * 3)*to_s2 / 2, SUMMON_BIZARRE4, FALSE)))
                    no_trump = TRUE;
            }
            else if (die<88)
            {
                msg_print("It's the picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, (dun_level * 3)*to_s2 / 2, SUMMON_BIZARRE5, FALSE)))
                    no_trump = TRUE;
            }
            else if (die<96)
            {
                msg_print("It's the Lovers.");
                if (!get_aim_dir(&dir)) return;
                (void) charm_monster(dir, MIN(p_ptr->lev, 20)+to_s2);
            }
            else if (die<101)
            {
                msg_print("It's the Hermit.");
                wall_stone();
            }
            else if (die< 111)
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
        break;
        case 3: /* Reset Recall */
            {
                /* Prompt */
                sprintf(ppp, "Reset to which level (1-%d): ", p_ptr->max_dlv[dungeon_type]);

                /* Default */
                sprintf(tmp_val, "%d", MAX(dun_level,1));

                /* Ask for a level */
                if (!get_string(ppp, tmp_val, 10)) return;

                /* Extract request */
                dummy = atoi(tmp_val);

                /* Paranoia */
                if (dummy < 1) dummy = 1;

                /* Paranoia */
                if (dummy > p_ptr->max_dlv[dungeon_type]) dummy = p_ptr->max_dlv[dungeon_type];

                /* Accept request */
                msg_format("Recall depth set to level %d (%d').", dummy, dummy * 50 );
            }
        break;
        case 4: /* Teleport Self */
            teleport_player(plev * 4+(p_ptr->to_s*2));
        break;
        case 5: /* Between gate */
       {
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
        case 6: /* Trump Spying */
            (void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25+to_s2);
        break;
        case 7: /* Teleport Away */
			if (!get_aim_dir(&dir)) return;
               (void)fire_beam(GF_AWAY_ALL, dir, plev*to_s2);
        break;
        case 8: /* Trump Object */
             if (!get_aim_dir(&dir)) return;
                 fetch(dir, plev*15+to_s2, TRUE);
        break;
        case 9: /* Trump Animal */
        {
            msg_print ("You concentrate on the trump of an animal...");
            if (randint(5)>2)
            {
              if (!(summon_specific_friendly(py, px, plev*to_s2, SUMMON_ANIMAL_RANGER, FALSE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_ANIMAL))
                {
                    msg_print("The summoned animal gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
        case 10: /* Phantasmal Servant */
               if (summon_specific_friendly(py, px, (plev*3)*to_s2/2, SUMMON_PHANTOM, FALSE))
               {
                    msg_print ("'Your wish, master?'");
                }
                else
                {
                    no_trump = TRUE;
                }
        break;
        case 11: /* Trump Monster */
        {
            msg_print ("You concentrate on the trump of a monster...");
            if (randint(5)>2)
            {
             if (!(summon_specific_friendly(py, px, plev*to_s2, SUMMON_NO_UNIQUES, FALSE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, 0))
                {
                    msg_print("The summoned creature gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
        case 12: /* Conjure Elemental */
        {
            if (randint(6)>3)
            {
             if (!(summon_specific_friendly(py, px, plev*to_s2, SUMMON_ELEMENTAL, FALSE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_ELEMENTAL))
                {
                      msg_print("You fail to control the elemental creature!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }

        break;
        case 13: /* Teleport Level */
			(void)teleport_player_level();
        break;
        case 14: /* Word of Recall */
			{
                                if (special_flag)
                                {
                                msg_print("No recall on special levels..");
                                break;
                                }
				recall_player();
				break;
			}
        case 15: /* Banish */
             banish_monsters(plev*4*to_s2);
        break;
        case 16: /* Joker Card */
            msg_print("You concentrate on a joker card...");
            switch(randint(4))
            {
                case 1: dummy = SUMMON_BIZARRE1; break;
                case 2: dummy = SUMMON_BIZARRE2; break;
                case 3: dummy = SUMMON_BIZARRE4; break;
                case 4: dummy = SUMMON_BIZARRE5; break;

            }
            if (randint(2)==1)
            {
                if (summon_specific(py, px, plev, dummy))
                    msg_print("The summoned creature gets angry!");
                 else
                    no_trump = TRUE;
                }
            else
            {
                if (!(summon_specific_friendly(py, px, plev*to_s2, dummy, FALSE)))
                    no_trump = TRUE;
            }
        break;
        case 17: /* Trump Spiders */
        {
            msg_print ("You concentrate on the trump of a spider...");
            if (randint(5)>2)
            {
                if (!(summon_specific_friendly(py, px, plev*to_s2, SUMMON_SPIDER, TRUE)))
                    no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_SPIDER))
                {
                    msg_print("The summoned spiders get angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
        case 18: /* Trump Reptiles */
        {
            msg_print ("You concentrate on the trump of a reptile...");
            if (randint(5)>2)
            {
             if (!(summon_specific_friendly(py, px, plev*to_s2, SUMMON_HYDRA, TRUE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_HYDRA))
                {
                    msg_print("The summoned reptile gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
/* Ancient Trump 19 */
#if 0
        case 19: /* Trump Hounds */
        {
            msg_print ("You concentrate on the trump of a hound...");
            if (randint(5)>2)
            {
             if (!(summon_specific_friendly(py, px, plev*to_s2, SUMMON_HOUND, TRUE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_HOUND))
                {
                    msg_print("The summoned hounds get angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
#else
        case 19: /* Trump Home */
                do_cmd_home_trump();
                break;
#endif
        case 20: /* Trump Branding */
            brand_weapon(4);
        break;
        case 21: /* Dragon Being */
        if (randint(8)==1) dummy = 12;
        else dummy = 180;
        if (gain_random_mutation(dummy))
            msg_print("You have turned into a Dragon Being.");
        break;
        case 22: /* Death Dealing */
            (void)dispel_living(plev * 3*to_s2);
        break;
        case 23: /* Trump Cyberdemon */
        {
            msg_print ("You concentrate on the trump of a Cyberdemon...");
            if (randint(10)>3)
            {
              if (!(summon_specific_friendly(py, px, plev*to_s2, SUMMON_CYBER, FALSE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_CYBER))
                {
                    msg_print("The summoned Cyberdemon gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
        case 24: /* Trump Divination */
			(void)detect_all();
        break;
        case 25: /* Trump Lore */
            identify_fully();
        break;
        case 26: /* Trump Undead */
        {
            msg_print ("You concentrate on the trump of an undead creature...");
            if (randint(10)>3)
            {
             if (!(summon_specific_friendly(py, px, plev*to_s2, SUMMON_UNDEAD, FALSE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_UNDEAD))
                {
                    msg_print("The summoned undead creature gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
        case 27: /* Trump Dragon */
        {
            msg_print ("You concentrate on the trump of a dragon...");
            if (randint(10)>3)
            {
             if (!(summon_specific_friendly(py, px, plev*to_s2, SUMMON_DRAGON, FALSE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_DRAGON))
                {
                    msg_print("The summoned dragon gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }

        break;
        case 28: /* Mass Trump */
        {
            no_trump = TRUE;
            msg_print ("You concentrate on several trumps at once...");
            for (dummy = 0; dummy < 3 + (plev / 10); dummy++)
            {
                if (randint(10)>3)
                {
                 if (summon_specific_friendly(py, px, plev*to_s2, SUMMON_NO_UNIQUES, FALSE))
                    no_trump = FALSE;
                }
                else
                {
                    if (summon_specific(py, px, plev, 0))
                    {
                        msg_print("A summoned creature gets angry!");
                        no_trump = FALSE;
                    }
                }
            }
        }
        break;
        case 29: /* Trump Demon */
        {
            msg_print ("You concentrate on the trump of a demon...");
            if (randint(10)>3)
            {
             if (!(summon_specific_friendly(py, px, plev*to_s2, SUMMON_DEMON, FALSE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_DEMON))
                {
                    msg_print("The summoned demon gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
        case 30: /* Trump Ancient Dragon */
        {
            msg_print ("You concentrate on the trump of an ancient dragon...");
            if (randint(10)>3)
            {
             if (!(summon_specific_friendly(py, px, plev*to_s2, SUMMON_HI_DRAGON_NO_UNIQUES, FALSE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_HI_DRAGON))
                {
                    msg_print("The summoned ancient dragon gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }

        break;
        case 31: /* Trump Greater Undead */
        {
            msg_print ("You concentrate on the trump of a greater undead being...");
            if (randint(10)>3)
            {
             if (!(summon_specific_friendly(py, px, plev*to_s2, SUMMON_HI_UNDEAD_NO_UNIQUES, FALSE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_HI_UNDEAD))
                {
                    msg_print("The summoned greater undead creature gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
	default:
		msg_format("You cast an unknown Trump spell: %d.", spell);
		msg_print(NULL);
    }
	if (no_trump)
	{
		msg_print("Nobody answers to your Trump call.");
	}
}


static void cast_arcane_spell(int spell)
{
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
	int	dummy = 0;
        int to_s2=p_ptr->to_s/2;
        to_s2 = (to_s2==0)?1:to_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
        case 1: /* Wizard Lock */
            if (!(get_aim_dir(&dir))) break;
            (void) wizard_lock(dir);
        break;
        case 2: /* Detect Invisibility */
            (void)detect_monsters_invis();
        break;
        case 3: /* Detect Monsters */
			(void)detect_monsters_normal();
        break;
        case 4: /* Blink */
            teleport_player(10+to_s2);
        break;
        case 5: /* Light Area */
                        (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1+to_s2);
        break;
        case 6: /* Trap & Door Destruction */
            if (!(get_aim_dir(&dir))) return;
            (void) destroy_door(dir);
        break;
        case 7: /* Cure Light Wounds */
            (void) hp_player(damroll(2, 8)+to_s2);
            (void) set_cut(p_ptr->cut - 10 - p_ptr->to_s);
        break;
        case 8: /* Detect Doors & Traps */
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
        break;
        case 9: /* Phlogiston */
            phlogiston();
        break;
        case 10: /* Detect Treasure */
			(void)detect_treasure();
			(void)detect_objects_gold();

        break;
        case 11: /* Detect Enchantment */
			(void)detect_objects_magic();
        break;
        case 12: /* Detect Object */
			(void)detect_objects_normal();
        break;
        case 13: /* Cure Poison */
			(void)set_poisoned(0);
        break;
        case 14: /* Resist Cold */
                        (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20+to_s2);
        break;
        case 15: /* Resist Fire */
                        (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20+to_s2);
        break;
        case 16: /* Resist Lightning */
                        (void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20+to_s2);
        break;
        case 17: /* Resist Acid */
            (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20+to_s2);
        break;
        case 18: /* Cure Medium Wounds */
            (void)hp_player(damroll(4, 8)+to_s2);
            (void)set_cut((p_ptr->cut / 2) - 50-p_ptr->to_s);
        break;
        case 19: /* Teleport */
            teleport_player(plev * 5+to_s2);
        break;
        case 20: /* Stone to Mud */
			if (!get_aim_dir(&dir)) return;
			(void)wall_to_mud(dir);
        break;
        case 21: /* Ray of Light */
			if (!get_aim_dir(&dir)) return;
            msg_print("A line of light appears.");
			lite_line(dir);
        break;
        case 22: /* Satisfy Hunger */
			(void)set_food(PY_FOOD_MAX - 1);
        break;
        case 23: /* See Invisible */
                        (void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24+to_s2);
        break;
        case 24: /* Recharging */
               (void)recharge(plev * 2+to_s2);
               break;
        case 25: /* Teleport Level */
			(void)teleport_player_level();
        break;
        case 26: /* Identify */
			(void)ident_spell();
        break;
        case 27: /* Teleport Away */
			if (!get_aim_dir(&dir)) return;
               (void)fire_beam(GF_AWAY_ALL, dir, plev*to_s2);
        break;
        case 28: /* Elemental Ball */
			if (!get_aim_dir(&dir)) return;
            switch (randint(4))
            {
                case 1: dummy = GF_FIRE;
                case 2: dummy = GF_ELEC;
                case 3: dummy = GF_COLD;
                default: dummy = GF_ACID;
            }
            fire_ball(dummy, dir,
                    75 + (plev)*to_s2, 2+to_s2);
        break;
        case 29: /* Detection */
			(void)detect_all();
        break;
        case 30: /* Word of Recall */
			{
                                if (special_flag)
                                {
                                msg_print("No recall on special levels..");
                                break;
                                }
				recall_player();
				break;
            }
        case 31: /* Clairvoyance */
			wiz_lite();
            if (!(p_ptr->telepathy))
            {
                (void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25+to_s2);
            }
        break;
	default:
		msg_format("You cast an unknown Arcane spell: %d.", spell);
		msg_print(NULL);
    }
}

void use_symbiotic_power(int r_idx, bool great)
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
        int x=px,y=py,k,count;

        /* List the powers */
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
        if(r_ptr->flags5 & RF5_BO_ACID) {strcpy(power_desc[num],"Acid Bolt");powers[num++]=46;}
        if(r_ptr->flags5 & RF5_BO_ELEC) {strcpy(power_desc[num],"Lightning Bolt");powers[num++]=47;}
        if(r_ptr->flags5 & RF5_BO_FIRE) {strcpy(power_desc[num],"Fire Bolt");powers[num++]=48;}
        if(r_ptr->flags5 & RF5_BO_COLD) {strcpy(power_desc[num],"Cold Bolt");powers[num++]=49;}
        if(r_ptr->flags5 & RF5_BO_NETH) {strcpy(power_desc[num],"Nether Bolt");powers[num++]=50;}
        if(r_ptr->flags5 & RF5_BO_WATE) {strcpy(power_desc[num],"Water Bolt");powers[num++]=51;}
        if(r_ptr->flags5 & RF5_BO_MANA) {strcpy(power_desc[num],"Mana Bolt");powers[num++]=52;}
        if(r_ptr->flags5 & RF5_BO_PLAS) {strcpy(power_desc[num],"Plasma Bolt");powers[num++]=53;}
        if(r_ptr->flags5 & RF5_BO_ICEE) {strcpy(power_desc[num],"Ice Bolt");powers[num++]=54;}
        if(r_ptr->flags5 & RF5_MISSILE) {strcpy(power_desc[num],"Missile");powers[num++]=55;}
        if(r_ptr->flags5 & RF5_CONF) {strcpy(power_desc[num],"Confusion");powers[num++]=58;}
        if(r_ptr->flags5 & RF5_SLOW) {strcpy(power_desc[num],"Slow");powers[num++]=59;}
        if(r_ptr->flags5 & RF5_HOLD) {strcpy(power_desc[num],"Paralyse");powers[num++]=60;}
        if(r_ptr->flags6 & RF6_HASTE) {strcpy(power_desc[num],"Haste Self");powers[num++]=61;}
        if(r_ptr->flags6 & RF6_HEAL) {strcpy(power_desc[num],"Heal");powers[num++]=63;}
        if(r_ptr->flags6 & RF6_BLINK) {strcpy(power_desc[num],"Blink");powers[num++]=64;}
        if(r_ptr->flags6 & RF6_TPORT) {strcpy(power_desc[num],"Teleport");powers[num++]=65;}
        if(great) if(r_ptr->flags6 & RF6_TELE_TO) {strcpy(power_desc[num],"Teleport To");powers[num++]=66;}
        if(r_ptr->flags6 & RF6_TELE_AWAY) {strcpy(power_desc[num],"Teleport Away");powers[num++]=67;}
        if(r_ptr->flags6 & RF6_DARKNESS) {strcpy(power_desc[num],"Darkness");powers[num++]=69;}

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

        if(!num) {msg_print("These monster have no power to use.");return;}

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
		return;
	}

        switch(Power)
        {
                case 0: /* Shriek */
                        aggravate_monsters(-1);
                        break;
                case 1: /* Rocket */
                        msg_print("You laught a rocket...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ROCKET, dir, p_ptr->lev * 12, 1 + (p_ptr->lev/20));
                        break;
                case 2: /* Arrow1 */
                        msg_print("You fire an arrow...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(1,6));
                        break;
                case 3: /* Arrow2 */
                        msg_print("You fire an arrow...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(3,6));
                        break;
                case 4: /* Arrow3 */
                        msg_print("You fire a missile...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(5,6));
                        break;
                case 5: /* Arrow4 */
                        msg_print("You fire a missile...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(7,6));
                        break;
                case 6: /* Br acid */
                        msg_print("You breathe acid ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ACID, dir, p_ptr->lev * 16, 1 + (p_ptr->lev/20));
                        break;
                case 7: /* Br elec */
                        msg_print("You breathe lightning ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ELEC, dir, p_ptr->lev * 16, 1 + (p_ptr->lev/20));
                        break;
                case 8: /* br fire */
                        msg_print("You breathe fire ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FIRE, dir, p_ptr->lev * 16, 1 + (p_ptr->lev/20));
                        break;
                case 9: /* br cold */
                        msg_print("You breathe cold ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_COLD, dir, p_ptr->lev * 16, 1 + (p_ptr->lev/20));
                        break;
                case 10: /* br pois */
                        msg_print("You breathe poison ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_POIS, dir, p_ptr->lev * 12, 1 + (p_ptr->lev/20));
                        break;
                case 11: /* br neth */
                        msg_print("You breathe nether ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NETHER, dir, p_ptr->lev * 10, 1 + (p_ptr->lev/20));
                        break;
                case 12: /* br lite */
                        msg_print("You breathe lite ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_LITE, dir, p_ptr->lev * 8, 1 + (p_ptr->lev/20));
                        break;
                case 13: /* br dark */
                        msg_print("You breathe dark ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DARK, dir, p_ptr->lev * 8, 1 + (p_ptr->lev/20));
                        break;
                case 14: /* br conf */
                        msg_print("You breathe confusion ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CONFUSION, dir, p_ptr->lev * 8, 1 + (p_ptr->lev/20));
                        break;
                case 15: /* br soun */
                        msg_print("You breathe sound ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SOUND, dir, p_ptr->lev * 8, 1 + (p_ptr->lev/20));
                        break;
                case 16: /* br chao */
                        msg_print("You breathe chaos ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CHAOS, dir, p_ptr->lev * 11, 1 + (p_ptr->lev/20));
                        break;
                case 17: /* br dise */
                        msg_print("You breathe disenchantment ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DISENCHANT, dir, p_ptr->lev * 12, 1 + (p_ptr->lev/20));
                        break;
                case 18: /* br nexu */
                        msg_print("You breathe nexus ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NEXUS, dir, p_ptr->lev * 5, 1 + (p_ptr->lev/20));
                        break;
                case 19: /* br time */
                        msg_print("You breathe time ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_TIME, dir, p_ptr->lev * 3, 1 + (p_ptr->lev/20));
                        break;
                case 20: /* br iner */
                        msg_print("You breathe inertia ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_INERTIA, dir, p_ptr->lev * 4, 1 + (p_ptr->lev/20));
                        break;
                case 21: /* br grav */
                        msg_print("You breathe gravity ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_GRAVITY, dir, p_ptr->lev * 4, 1 + (p_ptr->lev/20));
                        break;
                case 22: /* br shar */
                        msg_print("You breathe shards ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SHARDS, dir, p_ptr->lev * 8, 1 + (p_ptr->lev/20));
                        break;
                case 23: /* br plas */
                        msg_print("You breathe plasma ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_PLASMA, dir, p_ptr->lev * 3, 1 + (p_ptr->lev/20));
                        break;
                case 24: /* br wall */
                        msg_print("You breathe force ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FORCE, dir, p_ptr->lev * 4, 1 + (p_ptr->lev/20));
                        break;
                case 25: /* br mana */
                        msg_print("You breathe mana ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, p_ptr->lev * 5, 1 + (p_ptr->lev/20));
                        break;
                case 26: /* ba nuke */
                        msg_print("You cast a ball of nuke ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, p_ptr->lev * 12, 1 + (p_ptr->lev/20));
                        break;
                case 27: /* br nuke */
                        msg_print("You breathe nuke ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, p_ptr->lev * 12, 1 + (p_ptr->lev/20));
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
                case 45: /* bo acid */
                        msg_print("You cast a bolt of acid ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ACID, dir, damroll(7, 8) + (p_ptr->lev/3));
                        break;
                case 46: /* bo elec */
                        msg_print("You cast a bolt of lightning ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ELEC, dir, damroll(4, 8) + (p_ptr->lev/3));
                        break;
                case 47: /* bo fire */
                        msg_print("You cast a bolt of fire ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_FIRE, dir, damroll(9, 8) + (p_ptr->lev/3));
                        break;
                case 48: /* bo cold */
                        msg_print("You cast a bolt of cold ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_COLD, dir, damroll(6, 8) + (p_ptr->lev/3));
                        break;
                case 49: /* bo pois */
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
                case 58: /* conf */
                        msg_print("You cast a bolt of confusion ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_CONFUSION, dir, damroll(7, 8) + (p_ptr->lev/3));
                        break;
                case 59: /* slow */
                        msg_print("You cast a bolt of unspeed ...");
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
                             msg_print("You go between. Show the destination to your Dragon.");
                             if (!tgt_pt(&ii,&ij)) return;
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
			if (!get_aim_dir(&dir)) return;
                        (void)fire_beam(GF_AWAY_ALL, dir, plev);
                        break;
                case 69: /* darkness */
                        (void)project(-1, 3, py, px, 0, GF_DARK_WEAK, PROJECT_GRID | PROJECT_KILL);
                        /* Unlite up the room */
                        unlite_room(py, px);
                        break;
                case 70: /* Summon bug */
                                msg_format("You magically code some software bugs.");
				for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_BUG, TRUE);
				}
                        break;
                case 71: /* Summon RNG */
                                msg_format("You magically code some RNGs.");
				for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_RNG, TRUE);
				}
                        break;
                case 72: /* Summon dragonrider */
                                msg_format("You magically summon a DragonRider.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_DRAGONRIDER, TRUE);
				}
                        break;
                case 73: /* Summon kin */
                                msg_format("You magically summon some Kins.");
				summon_kin_type = r_ptr->d_char; /* Big hack */
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_KIN, TRUE);
				}
                        break;
                case 74: /* Summon cyber */
                                msg_format("You magically summon a Cyberdemon.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_CYBER, TRUE);
				}
                        break;
                case 75: /* Summon monster */
                                msg_format("You magically summon a monster.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, 0, TRUE);
				}
                        break;
                case 76: /* Summon monsters */
                                msg_format("You magically summon monsters.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, 0, TRUE);
				}
                        break;
                case 77: /* Summon ant */
                                msg_format("You magically summon ants.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_ANT, TRUE);
				}
                        break;
                case 78: /* Summon spider */
                                msg_format("You magically summon spiders.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_SPIDER, TRUE);
				}
                        break;
                case 79: /* Summon hound */
                                msg_format("You magically summon hounds.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_HOUND, TRUE);
				}
                        break;
                case 80: /* Summon hydra */
                                msg_format("You magically summon hydras.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_HYDRA, TRUE);
				}
                        break;
                case 81: /* Summon angel */
                                msg_format("You magically summon an angel.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_ANGEL, TRUE);
				}
                        break;
                case 82: /* Summon demon */
                                msg_format("You magically summon a demon.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_DEMON, TRUE);
				}
                        break;
                case 83: /* Summon undead */
                                msg_format("You magically summon an undead.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_UNDEAD, TRUE);
				}
                        break;
                case 84: /* Summon dragon */
                                msg_format("You magically summon a dragon.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_DRAGON, TRUE);
				}
                        break;
                case 85: /* Summon hiundead */
                                msg_format("You magically summon greater undeads.");
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_HI_UNDEAD_NO_UNIQUES, TRUE);
				}
                        break;
                case 86: /* Summon hidragon */
                                msg_format("You magically summon greater dragons.");
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_HI_DRAGON_NO_UNIQUES, TRUE);
				}
                        break;
                case 87: /* Summon wraith */
                                msg_format("You magically summon Wraith.");
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_WRAITH, TRUE);
				}
                        break;
        }
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
                if(o_ptr->k_idx) use_symbiotic_power(o_ptr->pval,FALSE);
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
                if(o_ptr->k_idx) use_symbiotic_power(o_ptr->pval,TRUE);
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
                use_symbiotic_power(m_ptr->r_idx,TRUE);
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
                        r_ptr = &r_info[m_ptr->r_idx];

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
                        msg_format("You cast an unknown Music spell: %d.", spell);
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
	int	increment = 0;
	int	use_realm;
	
        const cptr prayer = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : (mp_ptr->spell_book == TV_MUSIC_BOOK) ? "song" : "spell");
	
	object_type	*o_ptr;
	
	magic_type	*s_ptr;
	
	cptr q, s;
	
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
	
	if (o_ptr->tval == p_ptr->realm2+89) increment = 32;
	
	
	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);
	
	/* Hack -- Handle stuff */
	handle_stuff();
	
	if (increment) realm = p_ptr->realm2;
	else realm = p_ptr->realm1;
	
	/* Ask for a spell */
	if (!get_spell(&spell, ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
                sval, TRUE, (bool)(increment?TRUE:FALSE), o_ptr))
	{
		if (spell == -2)
			msg_format("You don't know any %ss in that book.", prayer);
		return;
	}
	
	
	/* Access the spell */
	use_realm = (increment?p_ptr->realm2:p_ptr->realm1);

        if (p_ptr->pclass == CLASS_SORCERER)
        {
                use_realm = o_ptr->tval - TV_LIFE_BOOK + 1;
                realm = o_ptr->tval - TV_LIFE_BOOK + 1;
                increment = 0;
        }

	s_ptr = &mp_ptr->info[use_realm-1][spell];
	
	
	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_format("You do not have enough mana to %s this %s.",
			((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
			prayer);
		
		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}
	
	
	/* Spell failure chance */
	chance = spell_chance(spell,use_realm-1);
	
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
		else if (o_ptr->tval == TV_DEATH_BOOK && (randint(100)<spell))
		{
			if ((sval == 3) && (randint(2)==1))
			{
				sanity_blast(0, TRUE);
			}
			else
			{
				msg_print("It hurts!");
				take_hit(damroll((o_ptr->sval)+1,6), "a miscast Death spell");
				if (spell>15 && randint(6)==1 && !(p_ptr->hold_life))
					lose_exp(spell * 250);
			}
		}
	}
	
	/* Process spell */
	else
	{
		/* Spells.  */
		switch (realm)
		{
		case REALM_LIFE: /* * LIFE * */
			cast_life_spell(spell);
			break;
		case REALM_SORCERY: /* * SORCERY * */
			cast_sorcery_spell(spell);
			break;
		case REALM_NATURE: /* * NATURE * */
			cast_nature_spell(spell);
			break;
		case REALM_CHAOS: /* * CHAOS * */
			cast_chaos_spell(spell);
			break;
		case REALM_DEATH: /* * DEATH * */
			cast_death_spell(spell);
			break;
		case REALM_TRUMP: /* TRUMP */
			cast_trump_spell(spell);
			break;
		case REALM_ARCANE: /* ARCANE */
			cast_arcane_spell(spell);
			break;
                case REALM_SYMBIOTIC: /* SYMBIOTIC */
                        cast_symbiotic_spell(spell);
			break;
                case REALM_MUSIC: /* MUSIC */
                        cast_music_spell(spell);
			break;
		default:
			msg_format("You cast a spell from an unknown realm: realm %d, spell %d.", realm, spell);
			msg_print(NULL);
		}

		/* A spell was cast */
                if ((!((increment) ?
			(spell_worked2 & (1L << spell)) :
                (spell_worked1 & (1L << (spell))))) && (p_ptr->pclass != CLASS_SORCERER))
		{
			int e = s_ptr->sexp;
			
			/* The spell worked */
			if (realm == p_ptr->realm1)
			{
				spell_worked1 |= (1L << spell);
			}
			else
			{
				spell_worked2 |= (1L << spell);
			}
			
			/* Gain experience */
			gain_exp(e * s_ptr->slevel);
		}
	}
	
	/* Take a turn */
	energy_use = 100;
	
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

void mindcraft_info(char *p, int power)
{
    int plev = p_ptr->lev;
	
    strcpy(p, "");
	
    switch (power) {
	case 0:  break;
	case 1:  sprintf(p, " dam %dd%d", 3 + ((plev - 1) / 4), 3 + plev/15); break;
	case 2:  sprintf(p, " range %d", (plev < 25 ? 10 : plev + 2)); break;
	case 3:  sprintf(p, " range %d", plev * 5);  break;
	case 4:  break;
	case 5:  sprintf(p, " dam %dd8", 8+((plev-5)/4));  break;
	case 6:  sprintf(p, " dur %d", plev);  break;
	case 7:  break;
	case 8:  sprintf(p, " dam %d", plev * ((plev-5) / 10 + 1)); break;
	case 9:  sprintf(p, " dur 11-%d", plev + plev/2);  break;
	case 10: sprintf(p, " dam %dd6", plev/2);  break;
	case 11: sprintf(p, " dam %d", plev * (plev > 39 ? 4: 3)); break;
    }
}

extern void do_cmd_rerate(void);
extern bool item_tester_hook_armour(object_type *o_ptr);

random_spell* select_spell(bool quick);

/*
 * Allow user to choose a mindcrafter power.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", or "study"
 * The "known" should be TRUE for cast/pray, FALSE for study
 *
 * nb: This function has a (trivial) display bug which will be obvious
 * when you run it. It's probably easy to fix but I haven't tried,
 * sorry.
 */
static int get_mindcraft_power(int *sn)
{
	int             i;
	int             num = 0;
	int             y = 1;
	int             x = 20;
	int             minfail = 0;
	int             plev = p_ptr->lev;
	int             chance = 0;
	int             ask;
	char            choice;
	char            out_val[160];
	char            comment[80];
	cptr            p = "power";
	mindcraft_power spell;
	bool            flag, redraw;

	/* Assume cancelled */
	*sn = (-1);

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (mindcraft_powers[*sn].min_lev <= plev)
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT -- TNB */

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	for (i = 0; i < MAX_MINDCRAFT_POWERS; i++)
	{
		if (mindcraft_powers[i].min_lev <= plev)
		{
			num++;
		}
	}

	/* Build a prompt (accept all spells) */
    strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
		p, I2A(0), I2A(num - 1), p);

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
        if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
            /* Show the list */
			if (!redraw)
			{
                char psi_desc[80];

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				/* Display a list of spells */
				prt("", y, x);
				put_str("Name", y, x + 5);
				put_str("Lv Mana Fail Info", y, x + 35);

				/* Dump the spells */
				for (i = 0; i < MAX_MINDCRAFT_POWERS; i++)
				{
					/* Access the spell */
					spell = mindcraft_powers[i];
					if (spell.min_lev > plev)   break;

					chance = spell.fail;
                    /* Reduce failure rate by "effective" level adjustment */
                    chance -= 3 * (p_ptr->lev - spell.min_lev);

                    /* Reduce failure rate by INT/WIS adjustment */
                    chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);

					/* Not enough mana to cast */
					if (spell.mana_cost > p_ptr->csp)
					{
                        chance += 5 * (spell.mana_cost - p_ptr->csp);
					}

                    /* Extract the minimum failure rate */
                    minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];

					/* Minimum failure rate */
                    if (chance < minfail) chance = minfail;

					/* Stunning makes spells harder */
                    if (p_ptr->stun > 50) chance += 25;
                    else if (p_ptr->stun) chance += 15;

                    /* Always a 5 percent chance of working */
					if (chance > 95) chance = 95;

					/* Get info */
					mindcraft_info(comment, i);

					/* Dump the spell --(-- */
                    sprintf(psi_desc, "  %c) %-30s%2d %4d %3d%%%s",
                        I2A(i), spell.name,
                        spell.min_lev, spell.mana_cost, chance, comment);
                    prt(psi_desc, y + i + 1, x);
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
		spell = mindcraft_powers[i];

        /* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
            strnfmt(tmp_val, 78, "Use %s? ", mindcraft_powers[i].name);

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
	(*sn) = i;

#ifdef ALLOW_REPEAT /* TNB */

	repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}

/*
 * do_cmd_cast calls this function if the player's class
 * is 'mindcrafter'.
 */
void do_cmd_mindcraft(void)
{
	int             n = 0,  b = 0;
	int             chance;
	int             dir;
	int             minfail = 0;
	int             plev = p_ptr->lev;
	mindcraft_power spell;

	/* not if confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* get power */
	if (!get_mindcraft_power(&n))  return;
	
	spell = mindcraft_powers[n];
    
	/* Verify "dangerous" spells */
	if (spell.mana_cost > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to use this power.");
		
		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}
    
	/* Spell failure chance */
	chance = spell.fail;
	
	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - spell.min_lev);
	
	/* Reduce failure rate by INT/WIS adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);
	
	/* Not enough mana to cast */
	if (spell.mana_cost > p_ptr->csp)
	{
		chance += 5 * (spell.mana_cost - p_ptr->csp);
	}
	
	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];
	
	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;
	
	/* Stunning makes spells harder */
	if (p_ptr->stun > 50) chance += 25;
	else if (p_ptr->stun) chance += 15;
	
	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;
	
	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_format("You failed to concentrate hard enough!");
		sound(SOUND_FAIL);
		
		if (randint(100) < (chance/2))
		{
			/* Backfire */
			b = randint(100);
			if (b < 5)
			{
				msg_print("Oh, no! Your mind has gone blank!");
				lose_all_info();
			}
			else if (b < 15)
			{
				msg_print("Weird visions seem to dance before your eyes...");
				set_image(p_ptr->image + 5 + randint(10));
			}
			else if (b < 45)
			{
				msg_print("Your brain is addled!");
				set_confused(p_ptr->confused + randint(8));
			}
			else if (b < 90)
			{
				set_stun(p_ptr->stun + randint(8));
			}
			else
			{
				/* Mana storm */
				msg_print("Your mind unleashes its power in an uncontrollable storm!");
				project(1, 2+plev/10, py, px, plev * 2,
					GF_MANA,PROJECT_JUMP|PROJECT_KILL|PROJECT_GRID|PROJECT_ITEM);
				p_ptr->csp = MAX(0, p_ptr->csp - plev * MAX(1, plev/10));
			}
		}
	}
	else
	{
		sound(SOUND_ZAP);

		/* spell code */
		switch (n)
		{
		case 0:   /* Precog */
			if (plev > 44)
				wiz_lite();
			else if (plev > 19)
				map_area();
			
			if (plev < 30)
			{
				b = detect_monsters_normal();
				if (plev > 14)  b |=  detect_monsters_invis();
				if (plev > 4)   b |=  detect_traps();
			}
			else
			{
				b = detect_all();
			}
			
			if ((plev > 24) && (plev < 40))
				set_tim_esp(p_ptr->tim_esp + plev);
			
			if (!b) msg_print("You feel safe.");
			break;
		case 1:
			/* Mindblast */
			if (!get_aim_dir(&dir)) return;
			if (randint(100) < plev * 2)
				fire_beam(GF_PSI, dir, damroll(3 + ((plev - 1) / 4), (3+plev/15)));
			else
				fire_ball(GF_PSI, dir, damroll(3 + ((plev - 1) / 4), (3+plev/15)), 0);
			break;
		case 2:
			/* Minor displace */
			if (plev < 25)
			{
				teleport_player(10);
			}
			else
			{
                                int ii,ij;
             msg_print("You open a between gate. Choose a destination.");
             if (!tgt_pt(&ii,&ij)) return;
             p_ptr->energy -= 60 - plev;
             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
             (distance(ij,ii,py,px) > plev + 2+(p_ptr->to_s*3)) ||
             (!rand_int(plev * plev / 2)))
             {
                 msg_print("You fail to exit the between correctly!");
                 p_ptr->energy -= 100;
                 get_pos_player(10+p_ptr->to_s/2,&ij,&ii);
             }
             cave_set_feat(py,px,FEAT_BETWEEN);
             cave_set_feat(ij,ii,FEAT_BETWEEN);
             cave[py][px].special = ii + (ij << 8);
             cave[ij][ii].special = px + (py << 8);

             break;
			}
			break;
		case 3:
			/* Major displace */
			if (plev > 29)
				banish_monsters(plev);
			teleport_player(plev * 5);
			break;
		case 4:
			/* Domination */
			if (plev < 30)
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DOMINATION, dir, plev, 0);
			}
			else
			{
				charm_monsters(p_ptr->lev * 2);
			}
			break;
		case 5:
			/* Fist of Force  ---  not 'true' TK  */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_SOUND, dir, damroll(8+((plev-5)/4), 8),
				(plev > 20 ? (plev-20)/8 + 1 : 0));
			break;
		case 6:
			/* Character Armour */
			set_shield(p_ptr->shield + plev);
			if (plev > 14) set_oppose_acid(p_ptr->oppose_acid + plev);
			if (plev > 19) set_oppose_fire(p_ptr->oppose_fire + plev);
			if (plev > 24) set_oppose_cold(p_ptr->oppose_cold + plev);
			if (plev > 29) set_oppose_elec(p_ptr->oppose_elec + plev);
			if (plev > 34) set_oppose_pois(p_ptr->oppose_pois + plev);
			break;
		case 7:
			/* Psychometry */
			if (plev < 40)
				psychometry();
			else
				ident_spell();
			break;
		case 8:
			/* Mindwave */
			msg_print("Mind-warping forces emanate from your brain!");
			if (plev < 25)
				project(0, 2+plev/10, py, px,
				(plev*3)/2, GF_PSI, PROJECT_KILL);
			else
				(void)mindblast_monsters(plev * ((plev-5) / 10 + 1));
			break;
		case 9:
			/* Adrenaline */
			set_afraid(0);
			set_stun(0);
			hp_player(plev);
			b = 10 + randint((plev*3)/2);
			if (plev < 35)
				set_hero(p_ptr->hero + b);
			else
				set_shero(p_ptr->shero + b);
			
			if (!p_ptr->fast)
			{
				/* Haste */
				(void)set_fast(b);
			}
			else
			{
				(void)set_fast(p_ptr->fast + b);
			}
			break;
		case 10:
			/* Psychic Drain */
			if (!get_aim_dir(&dir)) return;
			b = damroll(plev/2, 6);
			if (fire_ball(GF_PSI_DRAIN, dir, b,  0 +
				(plev-25)/10))
				p_ptr->energy -= randint(150);
			break;
		case 11:
			/* Telekinesis */
			msg_print("A wave of pure physical force radiates out from your body!");
			project(0, 3+plev/10, py, px,
				plev * (plev > 39 ? 4 : 3), GF_TELEKINESIS, PROJECT_KILL|PROJECT_ITEM|PROJECT_GRID);
			break;
		default:
			msg_print("Zap?");
		}
	}
    
	/* Take a turn */
	energy_use = 100;
	
	/* Sufficient mana */
	if (spell.mana_cost <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= spell.mana_cost;
	}
	
	/* Over-exert the player */
	else
	{
		int oops = spell.mana_cost - p_ptr->csp;
		
		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;
		
		/* Message */
		msg_print("You faint from the effort!");
		
		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));
		
		/* Damage WIS (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);
			
			/* Message */
			msg_print("You have damaged your mind!");
			
			/* Reduce constitution */
			(void)dec_stat(A_WIS, 15 + randint(10), perm);
		}
	}
	
	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);
	
	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}

static int get_mimic_chance(int c)
{
  int chance=c;
  chance -= p_ptr->lev * 3;
  chance -= 3 * adj_mag_stat[p_ptr->stat_ind[A_DEX]];

  if (chance < 2) chance = 2;

  /* Stunning makes spells harder */
  if (p_ptr->stun > 50) chance += 25;
  else if (p_ptr->stun) chance += 15;

  /* Always a 5 percent chance of working */
  if (chance > 95) chance = 95;

  /* Return the chance */
  return (chance);
}

void do_cmd_mimic()
{
        int             item;
        int chance;

	object_type	*o_ptr;

	cptr q, s;

  if (p_ptr->blind || no_lite()) {
    msg_print("You cannot see!");
    return;
  }

  if (p_ptr->confused) {
    msg_print("You are too confused!");
    return;
  }

        if(!p_ptr->mimic_form){

	/* Restrict choices to potions */
        item_tester_tval = TV_MIMIC_BOOK;

	/* Get an item */
        q = "Use which book? ";
        s = "You have no book of lore.";
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

        chance = get_mimic_chance((o_ptr->sval*2)+50);

  if (chance > 75) {
    msg_print("You feel uneasy with this shape-change.");
    if (!get_check("Try it anyway? ")) {
      return;
    }
  }

  if (randint(100) < chance) {
    msg_print("Your shape-change goes horribly wrong!");

    if (randint(100) < p_ptr->skill_sav) {
      msg_print("You manage to wrest your body back under control.");
    } else {
      set_mimic(60,MIMIC_ABOMINATION);
                        /* Redraw title */
                        p_ptr->redraw |= (PR_TITLE);
                        /* Recalculate bonuses */
                        p_ptr->update |= (PU_BONUS);
    }
  } else {
      set_mimic(o_ptr->pval,o_ptr->sval);
                        /* Redraw title */
                        p_ptr->redraw |= (PR_TITLE);
                        /* Recalculate bonuses */
                        p_ptr->update |= (PU_BONUS);
  }

        }
        else
        {
                msg_print("You are already transformed !");
                if(p_ptr->mimic_form!=MIMIC_ABOMINATION){
                        if (!(get_check("Turn into an abomination to cancel ? ")))
                        {
                                return;
                        }
                        p_ptr->mimic_form=MIMIC_ABOMINATION;
                        p_ptr->tim_mimic=20;
                        /* Redraw title */
                        p_ptr->redraw |= (PR_TITLE);
                        /* Recalculate bonuses */
                        p_ptr->update |= (PU_BONUS);
                }
        }
}

/*
 * do_cmd_cast calls this function if the player's class
 * is 'beastmaster'.
 */

int Beastmaster_Summon[25]={
                SUMMON_ANT,
                SUMMON_SPIDER,
                SUMMON_HOUND,
                SUMMON_HOUND,
                SUMMON_ANIMAL,
                SUMMON_ANIMAL_RANGER,
                SUMMON_DRAGON,
                SUMMON_DRAGON,
                SUMMON_ELEMENTAL,
                SUMMON_ELEMENTAL,
                SUMMON_UNDEAD,
                SUMMON_HYDRA,
                SUMMON_ANGEL,
                SUMMON_HI_DRAGON_NO_UNIQUES,
                SUMMON_HI_DRAGON_NO_UNIQUES,
                SUMMON_WRAITH,
                SUMMON_WRAITH,
                SUMMON_BIZARRE3,
                SUMMON_HI_UNDEAD_NO_UNIQUES,
                SUMMON_HI_UNDEAD_NO_UNIQUES,
                SUMMON_CYBER,
                SUMMON_CYBER,
                SUMMON_CYBER,
                SUMMON_PHANTOM,
                SUMMON_PHANTOM
};

void do_cmd_beastmaster(void)
{
        int plev = p_ptr->lev,i;
	monster_type    *m_ptr;

	/* Process the monsters (backwards) */
        p_ptr->class_extra1=0;
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		m_ptr = &m_list[i];

		if (is_pet(m_ptr))
		{
                        p_ptr->class_extra1++;
		}
        }
        if(p_ptr->class_extra1<plev*2){
                if(rand_int(80-(plev)-p_ptr->stat_use[5]-p_ptr->to_s)<20){
                        summon_specific_friendly(py, px, plev, rand_int(plev/2), FALSE);
                }
        }
        else msg_print("You can't summon more pets");
    
	/* Take a turn */
	energy_use = 100;

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}

int alchemist_baterie = -1;
int alchemist_charge = 0;
int alchemist_num = -1;
bool alchemist_ego = FALSE;

/*
 * Hook to determine if an object is usesable with a power baterie
 */
static bool item_tester_hook_powerable(object_type *o_ptr)
{
        int i;

        for(i = 0; i < 9; i++)
                if((alchemist_recipes[alchemist_baterie].item[i].ctval==o_ptr->tval)&&(alchemist_recipes[alchemist_baterie].item[i].csval==o_ptr->sval)) return TRUE;

        for(i = 0; i < 9; i++)
                if(alchemist_recipes[alchemist_baterie].ego[i].which==o_ptr->tval) return TRUE;

	/* Assume not */
	return (FALSE);
}
/*
 * Hook to determine if an object is extractable in a power baterie
 */
static bool item_tester_hook_extractable(object_type *o_ptr)
{
        object_kind *k_ptr = &k_info[o_ptr->k_idx];

        if(((o_ptr->tval==TV_POTION)||
        (o_ptr->tval==TV_POTION2)||
        (o_ptr->tval==TV_WAND)||
        (o_ptr->tval==TV_STAFF)||
        (o_ptr->tval==TV_RING)||
        (o_ptr->tval==TV_AMULET)||
        (o_ptr->tval==TV_SCROLL)||
        (o_ptr->tval==TV_ROD))&&(!k_ptr->know)) return TRUE;

	/* Assume not */
	return (FALSE);
}

bool get_alchemist_target(int *i)
{
        int item, a;
        object_type *o_ptr;

	cptr q, s;

        item_tester_hook = item_tester_hook_powerable;

	/* Get an item */
        q = "Apply to which item? ";
        s = "You have no item to apply it.";
        if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

        *i=item;

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
        if((o_ptr->tval==TV_WAND)||(o_ptr->tval==TV_STAFF))alchemist_charge=o_ptr->pval;

        for(a = 0; a < 9; a++)
                if((alchemist_recipes[alchemist_baterie].item[a].ctval==o_ptr->tval)&&(alchemist_recipes[alchemist_baterie].item[a].csval==o_ptr->sval)) {alchemist_num = a; alchemist_ego = FALSE; return TRUE;}

        for(a = 0; a < 9; a++)
                if(alchemist_recipes[alchemist_baterie].ego[a].which==o_ptr->tval) {alchemist_num = a; alchemist_ego = TRUE; return TRUE;}

        return TRUE;
}

/*
 * do_cmd_cast calls this function if the player's class
 * is 'alchemist'.
 */
void do_cmd_alchemist(void)
{
        int item, used_up, i,ext=0, a, b = -1;
        char ch;
        byte create_q_ptr=FALSE;

	object_type	*o_ptr;
	object_type	forge;
        object_type     *q_ptr;

        cptr q, s;
        char com[80];

        q_ptr = &forge;

        alchemist_charge = 0;

        if(p_ptr->lev > 29)
                sprintf(com, "[A]dd, [E]xtract a power or [C]reate an artifact ?");
        else
                sprintf(com, "[A]dd, [E]xtract a power ?");

	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

        while (TRUE)
        {
                if (!get_com(com, &ch))
                {
                        ext = 0;
                        break;
                }
                if (ch == 'A' || ch == 'a')
                {
                        ext = 1;
                        break;
                }
                if (ch == 'E' || ch == 'e')
                {
                        ext = 2;
                        break;
                }
                if ((ch == 'C' || ch == 'c')&&(p_ptr->lev > 29))
                {
                        ext = 3;
                        break;
                }
        }

        /**********Add a power*********/
        if(ext==1){
        /* Restrict choices to bateries */
        item_tester_tval = TV_BATERIE;

	/* Get an item */
        q = "Use which baterie? ";
        s = "You have no bateries to use.";
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


	/* Take a turn */
	energy_use = 100;

        /* Assume the baterie will get used up */
	used_up = TRUE;

        for(a=0;a<=MAX_ALCHEMIST_RECIPES;a++){
                if(alchemist_recipes[a].sval_baterie==o_ptr->sval)break;
        }

        alchemist_baterie = a;

        used_up=get_alchemist_target(&i);

        if(used_up==TRUE){
                if(alchemist_ego){
                        if(alchemist_recipes[a].ego[alchemist_num].ego_num>o_ptr->number)
                        {
                               q=format("You need at least %d bateries !",alchemist_recipes[a].ego[alchemist_num].ego_num);
                               msg_print(q);
                               used_up=FALSE;
                               goto fin_alchemist;
                        }
                        if (i >= 0)
                        {
                                q_ptr = &inventory[i];
                        }        
                        else
                        {
                                q_ptr = &o_list[0 - i];
                        }
                        if(q_ptr->name2==0){
                                if(q_ptr->number>1){
                                        msg_print("Can't enchant more than one item !");
                                        used_up=FALSE;
                                        goto fin_alchemist;
                                }
                                q_ptr->name2=alchemist_recipes[a].ego[alchemist_num].ego;
                                if(alchemist_recipes[a].ego[alchemist_num].enchant==ALCHEMIST_ENCHANT_DAM){
                                        q_ptr->to_h=rand_int(4+p_ptr->lev/5)+1;
                                        q_ptr->to_d=rand_int(4+p_ptr->lev/5)+1;
                                }
                                if(alchemist_recipes[a].ego[alchemist_num].enchant==ALCHEMIST_ENCHANT_PVAL){
                                        q_ptr->pval=rand_int(4+p_ptr->lev/5)+1;
                                }
                                if(alchemist_recipes[a].ego[alchemist_num].enchant==ALCHEMIST_ENCHANT_DAM_PVAL){
                                        q_ptr->to_h=rand_int(4+p_ptr->lev/5)+1;
                                        q_ptr->to_d=rand_int(4+p_ptr->lev/5)+1;
                                        q_ptr->pval=rand_int(4+p_ptr->lev/5)+1;
                                }
                        }else{
                                msg_print("This object is already enchanted !");
                                used_up=FALSE;
                                goto fin_alchemist;
                        }
                }else{
                        if(alchemist_recipes[a].item[alchemist_num].num>o_ptr->number)
                        {
                                q=format("You need at least %d bateries !",alchemist_recipes[a].item[alchemist_num].num);
                                msg_print(q);
                                used_up=FALSE;
                                goto fin_alchemist;
                        }

                        q_ptr = &forge;
                        object_wipe(q_ptr);
                        object_prep(q_ptr, lookup_kind(alchemist_recipes[a].item[alchemist_num].etval, alchemist_recipes[a].item[alchemist_num].esval));
                        if((q_ptr->tval==TV_WAND)||(q_ptr->tval==TV_STAFF))q_ptr->pval=alchemist_charge+1;
                        if((q_ptr->tval==TV_RING)||(q_ptr->tval==TV_AMULET))
                                apply_magic(q_ptr,p_ptr->max_dlv[dungeon_type],(randint(110-(p_ptr->max_dlv[dungeon_type]))==0)?TRUE:FALSE,
                                                                         FALSE,
                                                                         FALSE);
                        object_aware(q_ptr);
                        object_known(q_ptr);

                        q_ptr->ident |= IDENT_STOREB;

                        create_q_ptr=TRUE;
                }
        }
fin_alchemist:
        
	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* The item was tried */
	object_tried(o_ptr);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

        /* Hack -- allow certain bateries to be "preserved" */
	if (!used_up) return;

        /* Destroy a baterie in the pack */
	if (item >= 0)
	{
                inven_item_increase(item, -((alchemist_ego)?alchemist_recipes[a].ego[alchemist_num].ego_num:alchemist_recipes[a].item[alchemist_num].num));
                inven_item_describe(item);
	}

        /* Destroy a baterie on the floor */
	else
	{
                floor_item_increase(0 - item, -((alchemist_ego)?alchemist_recipes[a].ego[alchemist_num].ego_num:alchemist_recipes[a].item[alchemist_num].num));
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}
        if(create_q_ptr==TRUE){
                                if (i >= 0)
                                {
                                        inven_item_increase(i, -1);
                                        inven_item_describe(i);
                                        inven_item_optimize(i);
                                }
                                else
                                {
                                        floor_item_increase(0 - i, -1);
                                        floor_item_describe(0 - i);
                                        floor_item_optimize(0 - i);
                                }
                inven_carry(q_ptr,TRUE);
        }
        for(item=0;item<36;item++)inven_item_optimize(item);


        /**********Extract a power*********/
        }else if(ext==2){

        item_tester_hook = item_tester_hook_extractable;

	/* Get an item */
        q = "Extract from which item? ";
        s = "You have no item to extract power.";
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

        for(a=0;a<=MAX_ALCHEMIST_RECIPES;a++){
                for(b = 0; b < 9; b++)
                        if((alchemist_recipes[a].item[b].etval==o_ptr->tval)&&(alchemist_recipes[a].item[b].esval==o_ptr->sval)) goto fin_alchemist_2;
        }
        b = -1;
fin_alchemist_2:
        switch(b){
                case -1:
                {
                        object_kind *k_ptr = &k_info[o_ptr->k_idx];

                        k_ptr->know = TRUE; /* Don't try this item anymore */
                        break;
                }
                default:
                        if(((o_ptr->tval==TV_WAND)||(o_ptr->tval==TV_STAFF))&&(o_ptr->pval>1)){
                                o_ptr->pval--;
                        }else{
                                q_ptr = &forge;
                                object_wipe(q_ptr);
                                object_prep(q_ptr, lookup_kind(alchemist_recipes[a].item[b].ctval,alchemist_recipes[a].item[b].csval));
                                q_ptr->number = 1;
                                object_aware(q_ptr);
                                object_known(q_ptr);
                                q_ptr->ident |= IDENT_STOREB;
                                create_q_ptr=TRUE;
                        }
                        break;
        }
        if(b!=-1){                    
                if(create_q_ptr==TRUE){
                        if (item >= 0)
                        {
                                inven_item_increase(item, -1);
                                inven_item_describe(item);
                                inven_item_optimize(item);
                        }
                        else
                        {
                                floor_item_increase(0 - item, -1);
                                floor_item_describe(0 - item);
                                floor_item_optimize(0 - item);
                        }
                        inven_carry(q_ptr,TRUE);
                }

                q_ptr = &forge;
                object_wipe(q_ptr);
                object_prep(q_ptr, lookup_kind(TV_BATERIE,alchemist_recipes[a].sval_baterie));
                q_ptr->number = alchemist_recipes[a].item[b].num;
                object_aware(q_ptr);
                object_known(q_ptr);
                q_ptr->ident |= IDENT_STOREB;
                inven_carry(q_ptr,TRUE);
        }

        /*******Create an artifact*******/
        }else if(ext == 3){
                do_cmd_create_artifact();
        }

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}

/*
 * Command to ask favors from your god.
 */
void do_cmd_pray(void) {
  int level;
  cptr name;

  if (p_ptr->pgod == 0) {
    msg_print("Pray hard enough and your prayers might be answered.");
    return;
  }

  if (!get_check("Are you sure you want to disturb your deity? ")) return;

  level = interpret_grace() - interpret_favor();
  name = deity_info[p_ptr->pgod-1].name;

  if (p_ptr->pclass == CLASS_PRIEST && magik(30)) {
    level++;
  }

  if (p_ptr->pclass == CLASS_PALADIN && magik(10)) {
    level++;
  }

  if (level < 0) level = 0;
  if (level > 10) level = 10;

  energy_use = 100;

  switch (level) {
  case 10: case 9: case 8:
    msg_format("%s thunders: ``Thou hast pleaseth me, mortal.''", name);
    great_side_effect();
    break;

  case 7:
  case 6:
    msg_format("%s hisses: ``Leave me alone now, mortal!''", name);
    if (magik(30)) set_grace(p_ptr->grace - 1000);
    break;

  case 5:
  case 4:
  case 3:
    msg_format("%s quakes in rage: ``Thou art supremely insolent, mortal!''", name);
    nasty_side_effect();
    set_grace(p_ptr->grace - 5000);
    break;

  case 2:
  case 1:
  case 0:
    msg_format("%s whispers: ``Prepare to die, mortal...''", name);
    deadly_side_effect(TRUE);
    set_grace(p_ptr->grace - 20000);
    break;
  }

  p_ptr->god_favor += 25000;
}

/* 
 * Return percentage chance of spell failure. 
 */

int spell_chance_random(random_spell* rspell) {
  int chance, minfail;


  /* Extract the base spell failure rate */
  chance = rspell->level + 25;

  /* Reduce failure rate by "effective" level adjustment */
  chance -= 3 * (p_ptr->lev - rspell->level);

  /* Reduce failure rate by INT/WIS adjustment */
  chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);

  /* Not enough mana to cast */
  if (rspell->mana > p_ptr->csp) {
    chance += 5 * (rspell->mana - p_ptr->csp);
  }

  /* Extract the minimum failure rate */
    minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];

  /* Minimum failure rate */
  if (chance < minfail) chance = minfail;
  
  /* Stunning makes spells harder */
  if (p_ptr->stun > 50) chance += 25;
  else if (p_ptr->stun) chance += 15;

  /* Always a 5 percent chance of working */
  if (chance > 95) chance = 95;

  /* Return the chance */
  return (chance);
}



/*
 * Print a batch of spells.
 */
static void print_spell_batch(int batch, int max) {
  char buff[80];
  random_spell* rspell;
  int i;

  prt(format("      %-30s Lev Fail Mana  ", "Name"), 1, 20);

  for (i = 0; i < max; i++) {
    rspell = &random_spells[batch*10+i];

    if (rspell->untried) {
      sprintf(buff, "  %c) %-30s  (Spell untried)  ", I2A(i),
	      rspell->name);

    } else {
      sprintf(buff, "  %c) %-30s %3d %4d%% %3d  ", I2A(i), rspell->name, 
              rspell->level, spell_chance_random(rspell), rspell->mana);
    }

    prt(buff, 2+i, 20);
  }
  prt("", 2+i, 20);
}



/* 
 * List ten random spells and ask to pick one. 
 */

static random_spell* select_spell_from_batch(int batch, bool quick) {
  char tmp[160];
  char which;
  int mut_max = 10;
  random_spell* ret;

  Term_save();

  if (spell_num < (batch+1)*10) {
    mut_max = spell_num - batch*10;
  }

  sprintf(tmp, "(a-%c, * to list, / to rename, - to comment) Select a power: ", 
	  I2A(mut_max-1));
  
  prt(tmp, 0, 0);

  if (quick) {
    print_spell_batch(batch, mut_max);
  }

  while (1) {
    which = inkey();

    if (which == ESCAPE) {
      ret = NULL;
      break;

    } else if (which == '*'  || which == '?' || which == ' ') {
      print_spell_batch(batch, mut_max);

    } else if (which == '\r' && mut_max == 1) {
      ret = &random_spells[batch*10];
      break;

    } else if (which == '/') {
      prt("Rename which power: ", 0, 0);
      which = tolower(inkey());

      if (islower(which) && A2I(which) <= mut_max) {
	get_string("Name this power: ", 
                   random_spells[batch*10+A2I(which)].name, 29);
	prt(tmp, 0, 0);
      } else {
	bell();
	prt(tmp, 0, 0);
      }

    } else if (which == '-') {
      prt("Comment which power: ", 0, 0);
      which = tolower(inkey());

      if (islower(which) && A2I(which) <= mut_max) {
	get_string("Comment this power: ",
                   random_spells[batch*10+A2I(which)].desc, 29);
	prt(tmp, 0, 0);
      } else {
	bell();
	prt(tmp, 0, 0);
      }

    } else {
      which = tolower(which);
      if (islower(which) && A2I(which) < mut_max) {
        ret = &random_spells[batch*10+A2I(which)];
	break;
      } else {
	bell();
      }
    }
  }

  Term_load();

  return ret;
}
  

/* 
 * Pick a random spell from a menu 
 */

random_spell* select_spell(bool quick) {
  char tmp[160];
  char which;
  int batch_max = (spell_num-1)/10;

  if (spell_num == 0) {
    msg_print("There are no spells you can cast.");
    return NULL;
  }

  if (p_ptr->confused) {
    msg_print("You can't use your powers while confused!");
    return NULL;
  }

  Term_save();

  sprintf(tmp, "(a-%c) Select batch of powers: ", I2A(batch_max));
  
  prt(tmp, 0, 0);
  
  while (1) {
    which = inkey();
    
    if (which == ESCAPE) {
      Term_load();
      return NULL;

    } else if (which == '\r' && batch_max == 0) {
      Term_load();
      return select_spell_from_batch(0, quick);

    } else {
      which = tolower(which);
      if (islower(which) && A2I(which) <= batch_max) {
	Term_load();
	return select_spell_from_batch(A2I(which), quick);
      } else {
	bell();
      }
    }
  }
}


void do_cmd_powermage(void)
{
        random_spell *s_ptr;

        int dir, chance;
        int ty = 0, tx = 0;

        s_ptr = select_spell(FALSE);
    
        if (s_ptr == NULL) return;

        if(p_ptr->csp < s_ptr->mana)
        {
                msg_print("You do not have enough mana.");
                return;
        }

	/* Spell failure chance */
        chance = spell_chance_random(s_ptr);
	
	/* Failed spell */
	if (rand_int(100) < chance)
	{
                char sfail[80];

		if (flush_failure) flush();

                get_rnd_line("sfail.txt",sfail);

                msg_format("A cloud of %s appears above you.", sfail);
		sound(SOUND_FAIL);
	}


        p_ptr->csp -= s_ptr->mana;

        s_ptr->untried = FALSE;
        
        /* Hack -- Spell needs a target */
        if (s_ptr->proj_flags & PROJECT_BEAM || s_ptr->proj_flags & PROJECT_STOP)
        {
                if (!get_aim_dir(&dir)) return;

                /* Hack -- Use an actual "target" */
                if ((dir == 5) && target_okay()) {
                        tx = target_col;
                        ty = target_row;
                } else {
                        /* Use the given direction */
                        ty = py + ddy[dir];
                        tx = px + ddx[dir];
                }
        }

        if (s_ptr->proj_flags & PROJECT_BLAST) {
                ty = py;
                tx = px;
        }

        if(s_ptr->proj_flags & PROJECT_VIEWABLE)
        {
                project_hack(s_ptr->GF,damroll(s_ptr->dam_dice, s_ptr->dam_sides));
        }
        else
        if(s_ptr->proj_flags & PROJECT_METEOR_SHOWER)
        {
                project_meteor(s_ptr->radius, s_ptr->GF, damroll(s_ptr->dam_dice, s_ptr->dam_sides), s_ptr->proj_flags);
        }
        else
        {
                project(0, s_ptr->radius, ty, tx,
                        damroll(s_ptr->dam_dice, s_ptr->dam_sides),
                        s_ptr->GF, s_ptr->proj_flags);
        }

	/* Take a turn */
	energy_use = 100;

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
        p_ptr->redraw |= (PR_MANA);
}

/* Old magic system */

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
static int nr_get_spell(int *sn, cptr prompt, int sval, bool known)
{
	int                     i;

	int                     spell = -1;
	int                     num = 0;

	byte            spells[64];

	bool            flag, redraw, okay, ask;
	char            choice;

	magic_type      *s_ptr;

	char            out_val[160];

	cptr p = ((mp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");


	/* Extract spells */
	for (spell = 0; spell < 64; spell++)
	{
		/* Check for this spell */
		if ((spell < 32) ?
                    (nr_spell_flags[mp_ptr->spell_type][sval][0] & (1L << spell)) :
                    (nr_spell_flags[mp_ptr->spell_type][sval][1] & (1L << (spell - 32))))
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
                if (nr_spell_okay(spells[i], known)) okay = TRUE;
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
                                nr_print_spells(spells, num, 1, 20);
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Ask again */
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
                if (!nr_spell_okay(spell, known))
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
                        s_ptr = &mp_ptr->info[(spell>31)][spell % 32];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
                                prompt, nr_spell_names[mp_ptr->spell_type][spell],
                                s_ptr->smana, spell_chance(spell % 32, (spell>31)));

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
void nr_do_cmd_browse(void)
{
	int                     item, sval;

	int                     spell = -1;
	int                     num = 0;

	byte            spells[64];

	object_type             *o_ptr;


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

	/* Get an item (from inven or floor) */
        if (!get_item(&item, "Browse which book? ", NULL, (USE_INVEN | USE_FLOOR)))
	{
		if (item == -2) msg_print("You have no books that you can read.");
		return;
	}

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
                    (nr_spell_flags[mp_ptr->spell_type][sval][0] & (1L << spell)) :
                    (nr_spell_flags[mp_ptr->spell_type][sval][1] & (1L << (spell - 32))))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}


	/* Save the screen */
	Term_save();

	/* Display the spells */
        nr_print_spells(spells, num, 1, 20);

	/* Clear the top line */
	prt("", 0, 0);

	/* Prompt user */
	put_str("[Press any key to continue]", 0, 23);

	/* Wait for key */
	(void)inkey();

	/* Restore the screen */
	Term_load();
}




/*
 * Study a book to gain a new spell/prayer
 */
void nr_do_cmd_study(void)
{
	int                     i, item, sval;

	int                     spell = -1;

	cptr p = ((mp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");

	object_type             *o_ptr;


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

	/* Get an item (from inven or floor) */
        if (!get_item(&item, "Study which book? ", NULL, (USE_INVEN | USE_FLOOR)))
	{
		if (item == -2) msg_print("You have no books that you can read.");
		return;
	}

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
	if (mp_ptr->spell_book == TV_MAGIC_BOOK)
	{
		/* Ask for a spell, allow cancel */
                if (!nr_get_spell(&spell, "study", sval, FALSE) && (spell == -1)) return;
	}

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
                            (nr_spell_flags[mp_ptr->spell_type][sval][0] & (1L << spell)) :
                            (nr_spell_flags[mp_ptr->spell_type][sval][1] & (1L << (spell - 32))))
			{
				/* Skip non "okay" prayers */
                                if (!nr_spell_okay(spell, FALSE)) continue;

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
	if (spell < 32)
	{
		spell_learned1 |= (1L << spell);
	}
	else
	{
		spell_learned2 |= (1L << (spell - 32));
	}

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < 64; i++)
	{
		/* Stop at the first empty space */
		if (spell_order[i] == 99) break;
	}

	/* Add the spell to the known list */
	spell_order[i++] = spell;

	/* Mention the result */
	msg_format("You have learned the %s of %s.",
                   p, nr_spell_names[mp_ptr->spell_type][spell]);

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
void nr_do_cmd_cast(void)
{
	int                     item, sval, spell, dir;
	int                     chance, beam;
	int                     plev = p_ptr->lev;
        byte to_s2 = p_ptr->to_s / 2;

	object_type             *o_ptr;

	magic_type              *s_ptr;

        to_s2 = (to_s2==0)?1:to_s2;

	/* Require spell ability */
	if (mp_ptr->spell_book != TV_MAGIC_BOOK)
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

	/* Get an item (from inven or floor) */
        if (!get_item(&item, "Use which book? ", NULL, (USE_INVEN | USE_FLOOR)))
	{
		if (item == -2) msg_print("You have no spell books!");
		return;
	}

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
        if (!nr_get_spell(&spell, "cast", sval, TRUE))
	{
		if (spell == -2) msg_print("You don't know any spells in that book.");
		return;
	}


	/* Access the spell */
        s_ptr = &mp_ptr->info[(spell>31)][spell % 32];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
        chance = spell_chance(spell % 32, (spell>31));

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
                beam = ((p_ptr->pclass == CLASS_WIZARD) ? plev : (plev / 2));

		/* Spells.  */
		switch (spell)
		{
			case 0:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                                                  damroll(3 + ((plev - 1) / 5), 4) * to_s2);
				break;
			}

			case 1:
			{
				(void)detect_monsters_normal();
				break;
			}

			case 2:
			{
                                teleport_player(10 * to_s2);
				break;
			}

			case 3:
			{
                                (void)lite_area(damroll(2, (plev / 2)) * to_s2, (plev / 10) + 1 + to_s2);
				break;
			}

			case 4:
			{
				(void)detect_treasure();
				(void)detect_objects_gold();
				break;
			}

			case 5:
			{
                                (void)hp_player(damroll(2 * p_ptr->to_s, 8));
				(void)set_cut(p_ptr->cut - 15);
				break;
			}

			case 6:
			{
				(void)detect_objects_normal();
				break;
			}

			case 7:
			{
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 8:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
                                          10 + (plev / 2) * to_s2, 2 + to_s2);
				break;
			}

			case 9:
			{
				if (!get_aim_dir(&dir)) return;
                                (void)confuse_monster(dir, plev * to_s2);
				break;
			}

			case 10:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                                  damroll(3+((plev-5)/4), 8) * to_s2);
				break;
			}

			case 11:
			{
				(void)destroy_doors_touch();
				break;
			}

			case 12:
			{
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir);
				break;
			}

			case 13:
			{
				(void)set_poisoned(0);
				break;
			}

			case 14:
			{
                                teleport_player(plev * 5 * to_s2);
				break;
			}

			case 15:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("A line of blue shimmering light appears.");
				lite_line(dir);
				break;
			}

			case 16:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_COLD, dir,
                                                  damroll(5+((plev-5)/4), 8) * to_s2);
				break;
			}

			case 17:
			{
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			}

			case 18:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 19:
			{
				(void)recharge(5);
				break;
			}

			case 20:
			{
				(void)sleep_monsters_touch();
				break;
			}

			case 21:
			{
				if (!get_aim_dir(&dir)) return;
				(void)poly_monster(dir);
				break;
			}

			case 22:
			{
				(void)ident_spell();
				break;
			}

			case 23:
			{
				(void)sleep_monsters();
				break;
			}

			case 24:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
                                                  damroll(8+((plev-5)/4), 8) * to_s2);
				break;
			}

			case 25:
			{
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir);
				break;
			}

			case 26:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
                                          30 + (plev) * to_s2, 2 + to_s2);
				break;
			}

			case 27:
			{
				(void)recharge(40);
				break;
			}

			case 28:
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 29:
			{
				if (!p_ptr->fast)
				{
                                        (void)set_fast(randint(20) + plev + to_s2);
				}
				else
				{
                                        (void)set_fast(p_ptr->fast + randint(5) + to_s2);
				}
				break;
			}

			case 30:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
                                          55 + (plev) * to_s2, 2 + to_s2);
				break;
			}

			case 31:
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case 32:
			{
                                (void)genocide(TRUE);
				break;
			}

			case 33:
			{
				(void)door_creation();
				break;
			}

			case 34:
			{
				(void)stair_creation();
				break;
			}

			case 35:
			{
				(void)teleport_player_level();
				break;
			}

			case 36:
			{
				earthquake(py, px, 10);
				break;
			}

			case 37:
			{
				if (special_flag)
				{
				msg_print("No recall on special levels..");
				break;
				}
				
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
			}

			case 38:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
                                                  damroll(6+((plev-5)/4), 8) * to_s2);
				break;
			}

			case 39:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
                                          20 + (plev / 2) * to_s2, 3 + to_s2);
				break;
			}

			case 40:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir,
                                          40 + (plev) * to_s2, 2 + to_s2);
				break;
			}

			case 41:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
                                          70 + (plev) * to_s2, 3 + to_s2);
				break;
			}

			case 42:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_METEOR, dir,
                                          65 + (plev) * to_s2, 3 + to_s2);
				break;
			}

			case 43:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir,
                                          300 + (plev * 2) * to_s2, 3 + to_s2);
				break;
			}

			case 44:
			{
				(void)detect_monsters_evil();
				break;
			}

			case 45:
			{
				(void)detect_objects_magic();
				break;
			}

			case 46:
			{
				recharge(100);
				break;
			}

			case 47:
			{
                                (void)genocide(TRUE);
				break;
			}

			case 48:
			{
                                (void)mass_genocide(TRUE);
				break;
			}

			case 49:
			{
                                (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20 + to_s2);
				break;
			}

			case 50:
			{
                                (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20 + to_s2);
				break;
			}

			case 51:
			{
                                (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20 + to_s2);
				break;
			}

			case 52:
			{
                                (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20 + to_s2);
				break;
			}

			case 53:
			{
                                (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20 + to_s2);
                                (void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20 + to_s2);
                                (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20 + to_s2);
                                (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20 + to_s2);
                                (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20 + to_s2);
				break;
			}

			case 54:
			{
                                (void)hp_player(10 * to_s2);
                                (void)set_hero(p_ptr->hero + randint(25) + 25 + to_s2);
				(void)set_afraid(0);
				break;
			}

			case 55:
			{
                                (void)set_shield(p_ptr->shield + randint(20) + 30 + to_s2);
				break;
			}

			case 56:
			{
                                (void)hp_player(30 * to_s2);
                                (void)set_shero(p_ptr->shero + randint(25) + 25 + to_s2);
				(void)set_afraid(0);
				break;
			}

			case 57:
			{
				if (!p_ptr->fast)
				{
                                        (void)set_fast(randint(30) + 30 + plev + to_s2);
				}
				else
				{
                                        (void)set_fast(p_ptr->fast + randint(10) + to_s2);
				}
				break;
			}

			case 58:
			{
                                (void)set_invuln(p_ptr->invuln + randint(8) + 8 + to_s2);
				break;
			}
		}

		/* A spell was cast */
		if (!((spell < 32) ?
		      (spell_worked1 & (1L << spell)) :
		      (spell_worked2 & (1L << (spell - 32)))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (spell < 32)
			{
				spell_worked1 |= (1L << spell);
			}
			else
			{
				spell_worked2 |= (1L << (spell - 32));
			}

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);
		}
	}

	/* Take a turn */
	energy_use = 100;

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
}

/*
 * Pray a prayer
 */
void nr_do_cmd_pray(void)
{
	int item, sval, spell, dir, chance;
	int plev = p_ptr->lev;
        int to_s2 = p_ptr->to_s /2;

	object_type     *o_ptr;

	magic_type  *s_ptr;

        to_s2 = (to_s2==0)?1:to_s2;

	/* Must use prayer books */
	if (mp_ptr->spell_book != TV_PRAYER_BOOK)
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

	/* Get an item (from inven or floor) */
        if (!get_item(&item, "Use which book? ", NULL, (USE_INVEN | USE_FLOOR)))
	{
		if (item == -2) msg_print("You have no prayer books!");
		return;
	}

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
        if (!nr_get_spell(&spell, "recite", sval, TRUE))
	{
		if (spell == -2) msg_print("You don't know any prayers in that book.");
		return;
	}


	/* Access the spell */
        s_ptr = &mp_ptr->info[(spell>31)][spell % 32];


	/* Verify "dangerous" prayers */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to recite this prayer.");

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
        chance = spell_chance(spell % 32, (spell>31));

	/* Check for failure */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print("You failed to concentrate hard enough!");
	}

	/* Success */
	else
	{
		switch (spell)
		{
			case 0:
			{
				(void)detect_monsters_evil();
				break;
			}

			case 1:
			{
                                (void)hp_player(damroll(2 * to_s2, 10));
				(void)set_cut(p_ptr->cut - 10);
				break;
			}

			case 2:
			{
                                (void)set_blessed(p_ptr->blessed + randint(12) + 12 + to_s2);
				break;
			}

			case 3:
			{
				(void)set_afraid(0);
				break;
			}

			case 4:
			{
                                (void)lite_area(damroll(2, (plev / 2)) * to_s2, (plev / 10) + 1);
				break;
			}

			case 5:
			{
				(void)detect_traps();
				break;
			}

			case 6:
			{
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 7:
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				break;
			}

			case 8:
			{
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, plev);
				break;
			}

			case 9:
			{
                                teleport_player(plev * 3 * to_s2);
				break;
			}

			case 10:
			{
                                (void)hp_player(damroll(4 * to_s2, 10));
				(void)set_cut((p_ptr->cut / 2) - 20);
				break;
			}

			case 11:
			{
                                (void)set_blessed(p_ptr->blessed + randint(24) + 24 + to_s2);
				break;
			}

			case 12:
			{
				(void)sleep_monsters_touch();
				break;
			}

			case 13:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 14:
			{
				remove_curse();
				break;
			}

			case 15:
			{
                                (void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10 + to_s2);
                                (void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10 + to_s2);
				break;
			}

			case 16:
			{
				(void)set_poisoned(0);
				break;
			}

			case 17:
			{
				if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_HOLY_FIRE, dir,
                                          (damroll(3, 6) + plev * to_s2 +
					   (plev / ((p_ptr->pclass == 2) ? 2 : 4))),
                                          ((plev < 30) ? 2 : 3) + to_s2);
				break;
			}

			case 18:
			{
                                (void)hp_player(damroll(6 * to_s2, 10));
				(void)set_cut(0);
				break;
			}

			case 19:
			{
                                (void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24 + to_s2);
				break;
			}

			case 20:
			{
                                (void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev + to_s2);
				break;
			}

			case 21:
			{
				earthquake(py, px, 10);
				break;
			}

			case 22:
			{
				map_area();
				break;
			}

			case 23:
			{
                                (void)hp_player(damroll(8 * to_s2, 10));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 24:
			{
				(void)turn_undead();
				break;
			}

			case 25:
			{
                                (void)set_blessed(p_ptr->blessed + randint(48) + 48 + to_s2);
				break;
			}

			case 26:
			{
                                (void)dispel_undead(plev * 3 * to_s2);
				break;
			}

			case 27:
			{
                                (void)hp_player(300 * to_s2);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 28:
			{
				(void)dispel_evil(plev * 3);
				break;
			}

			case 29:
			{
				warding_glyph();
				break;
			}

			case 30:
			{
                                (void)dispel_evil(plev * 4 * to_s2);
                                (void)hp_player(1000 * to_s2);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 31:
			{
				(void)detect_monsters_normal();
				break;
			}

			case 32:
			{
				(void)detect_all();
				break;
			}

			case 33:
			{
				(void)ident_spell();
				break;
			}

			case 34:
			{
				(void)probing();
				break;
			}

			case 35:
			{
				wiz_lite();
				break;
			}

			case 36:
			{
                                (void)hp_player(damroll(4 * to_s2, 10));
				(void)set_cut(0);
				break;
			}

			case 37:
			{
                                (void)hp_player(damroll(8 * to_s2, 10));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 38:
			{
                                (void)hp_player(2000 * to_s2);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 39:
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				break;
			}

			case 40:
			{
				(void)restore_level();
				break;
			}

			case 41:
			{
                                (void)dispel_undead(plev * 4 * to_s2);
				break;
			}

			case 42:
			{
                                (void)dispel_evil(plev * 4 * to_s2);
				break;
			}

			case 43:
			{
                                if (banish_evil(100 * to_s2))
				{
					msg_print("The power of your god banishes evil!");
				}
				break;
			}

			case 44:
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case 45:
			{
				if (!get_aim_dir(&dir)) return;
                                drain_life(dir, 200 * to_s2);
				break;
			}

			case 46:
			{
				(void)destroy_doors_touch();
				break;
			}

			case 47:
			{
				(void)recharge(15);
				break;
			}

			case 48:
			{
				(void)remove_all_curse();
				break;
			}

			case 49:
			{
                                (void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0, 0);
				break;
			}

			case 50:
			{
                                (void)enchant_spell(0, 0, rand_int(3) + 2, 0);
				break;
			}

			case 51:
			{
                                brand_weapon(0);
				break;
			}

			case 52:
			{
                                teleport_player(10 * to_s2);
				break;
			}

			case 53:
			{
                                teleport_player(plev * 8 * to_s2);
				break;
			}

			case 54:
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 55:
			{
				(void)teleport_player_level();
				break;
			}

			case 56:
			{
				if (special_flag)
				{
					msg_print("No recall on special levels...");
					break;
				}
				
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

			case 57:
			{
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
			}
		}

		/* A prayer was prayed */
		if (!((spell < 32) ?
		      (spell_worked1 & (1L << spell)) :
		      (spell_worked2 & (1L << (spell - 32)))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (spell < 32)
			{
				spell_worked1 |= (1L << spell);
			}
			else
			{
				spell_worked2 |= (1L << (spell - 32));
			}

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);
		}
	}

	/* Take a turn */
	energy_use = 100;

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
}

void do_cmd_possessor()
{
        char ch, ext;

        while (TRUE)
        {
                if (!get_com("[U]se your race powers or your [I]ncarnating powers ?", &ch))
                {
                        ext = 0;
                        break;
                }
                if (ch == 'U' || ch == 'u')
                {
                        ext = 1;
                        break;
                }
                if (ch == 'I' || ch == 'i')
                {
                        ext = 2;
                        break;
                }
        }
        if(ext == 1)
        {
                monster_race *r_ptr = &r_info[p_ptr->body_monster];

                if(rand_int(100) < (r_ptr->freq_inate + r_ptr->freq_spell + p_ptr->lev) * 3 / 2) use_symbiotic_power(p_ptr->body_monster, TRUE);
        }
        else if(ext == 2)
                if(p_ptr->disembodied)
                        do_cmd_integrate_body();
                else
                        do_cmd_leave_body(TRUE);

	/* Take a turn */
	energy_use = 100;
}

