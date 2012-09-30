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


extern void do_cmd_rerate(void);
extern void mutate_player(void);
extern item_tester_hook_armour(object_type *o_ptr);

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
	int                     i;

	int                     spell = -1;
	int                     num = 0;

	byte            spells[64];

	bool            flag, redraw, okay, ask;
	char            choice;

	magic_type      *s_ptr;

	char            out_val[160];

    int use_realm = (realm_2?p_ptr->realm2:p_ptr->realm1);
    cptr p = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell");

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
		print_spells(spells, num, 1, 20, use_realm-1);
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

	    s_ptr = &mp_ptr->info[use_realm-1][spell%32];

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

	/* Success */
	return (TRUE);
}


void rustproof(void)
{
    int                     item;

	object_type             *o_ptr;

	char            o_name[80];

   /* Select a piece of armour */
   item_tester_hook = item_tester_hook_armour;

	/* Get an item (from equip or inven or floor) */
    if (!get_item(&item, "Rustproof which piece of armour? ", TRUE, TRUE, TRUE))
	{
        if (item == -2) msg_print("You have nothing to rustproof.");
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
 * Peruse the spells/prayers in a Book
 *
 * Note that *all* spells in the book are listed
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(void)
{
	int                     item, sval;
    int         spell = -1;
	int                     num = 0;

	byte            spells[64];

	object_type             *o_ptr;


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

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Browse which book? ", FALSE, TRUE, TRUE))
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
    print_spells(spells, num, 1, 20, (o_ptr->tval-90));

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
	int                     i, item, sval;
    int         increment = 0;
	/* Spells of realm2 will have an increment of +32 */
	int                     spell = -1;

    cptr p = ((mp_ptr->spell_book == TV_SORCERY_BOOK) ? "spell" : "prayer");

	object_type             *o_ptr;


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

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Study which book? ", FALSE, TRUE, TRUE))
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

    if (o_ptr->tval==p_ptr->realm2+89) increment=32;

    /* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Mage -- Learn a selected spell */
    if (mp_ptr->spell_book != TV_LIFE_BOOK)
	{
		/* Ask for a spell, allow cancel */
	if (!get_spell(&spell, "study", sval, FALSE,(increment?TRUE:FALSE))
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

    s16b wounds = p_ptr->cut, hit_p = (p_ptr->mhp - p_ptr->chp);
    s16b change = damroll(p_ptr->lev, 5);
    bool Nasty_effect = (randint(5)==1);

    if (!(wounds || hit_p || Nasty_effect)) return;

    if (Nasty_effect)
        {

            msg_print("A new wound was created!");
            take_hit(change, "a polymorphed wound");
            set_cut(change);
        }
    else
        {
            msg_print("Your wounds are polymorphed into less serious ones.");
            hp_player(change);
            set_cut((p_ptr->cut)-(change/2));
        }
}

void do_poly_self(void)
{
int effects = randint(3);
int tmp = 0;
int new_race;
int more_effects = TRUE;

msg_print("You feel a change coming over you...");

while (effects-- && more_effects)
    {
        switch (randint(20))
        {
        case 1: case 2: case 3: case 4:
            do_cmd_rerate();
            break;
        case 5: case 6: case 7: case 8:
            do_poly_wounds();
            break;
        case 9: /* Racial polymorph! Uh oh... */
          {
            do { new_race = randint(MAX_RACES) -1; } while (new_race == p_ptr->prace);

            msg_format("You turn into a%s %s!",
                ((new_race == RACE_AMBERITE || new_race == RACE_ELF
                  || new_race == RACE_IMP)?"n":""),
                race_info[new_race].title);

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

            p_ptr->update |= (PU_BONUS);
          }
          lite_spot(py, px);
          more_effects = FALSE; /* Stop here! */
          break;
        case 10: /* Purposedly "leaks" into default */
            msg_print("You polymorph into an abomination!");
            while (tmp < 6)
            {
                (void)dec_stat(tmp, randint(6)+6, (randint(3)==1));
                tmp++;
            }
            if (randint(6)==1)
            {
                msg_print("You find living difficult in your present form!");
                take_hit(damroll(randint(p_ptr->lev),p_ptr->lev), "a lethal mutation");
            }
            /* No break; here! */
        default:
            mutate_player();
    }
    }

}

static void brand_weapon(int brand_type)
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

		char o_name[80];
        object_desc(o_name, o_ptr, FALSE, 0); /* Let's get the name before
                                                it is changed... */

    switch (brand_type)
    {
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
	    if (i-5) fire_ball(GF_ROCKET, i,
				175, 2);

		for (i = 1; i < 10; i++)
	    if (i-5) fire_ball(GF_MANA, i,
				175, 3);
		
		for (i = 1; i < 10; i++)
	    if (i-5) fire_ball(GF_NUKE, i,
				175, 4);
	}




    else
    {
        msg_format("You %s the %s too close to a wall!",
            ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
            ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell"));
        msg_print("There is a loud explosion!");
        destroy_area(py, px, 20+(p_ptr->lev), TRUE);
        msg_print("The dungeon collapses...");
        take_hit(100 + (randint(150)), "a suicidal Call the Void");
    }
}

void wild_magic(int spell)
{
    int counter = 0;
    int type = SUMMON_BIZARRE1 - 1 + (randint(6));
    if (type < SUMMON_BIZARRE1) type = SUMMON_BIZARRE1;
    else if (type > SUMMON_BIZARRE6) type = SUMMON_BIZARRE6;

    switch(randint(spell) + randint(6) + 1)

    {
        case 1: case 2: case 3:
            teleport_player(10);
            break;
        case 4: case 5: case 6:
            teleport_player(100);
            break;
        case 7: case 8:
            teleport_player(200);
            break;
        case 9: case 10: case 11:
            unlite_area(10,3);
            break;
        case 12: case 13: case 14:
            lite_area(damroll(2,3),2);
            break;
        case 15:
            destroy_doors_touch();
            break;
        case 16: case 17:
            wall_breaker();
        case 18:
            sleep_monsters_touch();
            break;
        case 19: case 20:
            trap_creation();
            break;
        case 21: case 22:
            door_creation();
            break;
        case 23: case 24: case 25:
            aggravate_monsters(1);
            break;
        case 26:
            earthquake(py, px, 5);
            break;
        case 27: case 28:
            apply_disenchant(0);
            break;
        case 29:
            lose_all_info();
            break;
        case 30:
            fire_ball(GF_CHAOS, 0, spell + 5, 1 + (spell/10));
            break;
        case 31:
            wall_stone();
            break;
        case 32: case 33:
            while (counter++ < 8)
            {
            (void) summon_specific(py, px, (dun_level * 3) / 2, type);
                    }
            break;
        case 34: case 35:
            activate_hi_summon();
            break;
        case 36:
            summon_cyber();
        default:
            activate_ty_curse();
    }
    return;
}


/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
    int         item, sval, spell, dir, realm;
    int                     chance, beam;
    int                     plev = p_ptr->lev;
    int         increment = 0, dummy = 0;
    int         use_realm, i;
    const cptr prayer = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell");

	object_type             *o_ptr;

	magic_type              *s_ptr;

    /* Require spell ability */
    if (p_ptr->realm1 == 0)
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
	if (!get_item(&item, "Use which book? ", FALSE, TRUE, TRUE))
	{
        if (item == -2) msg_format("You have no %s books!", prayer);
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

    if (o_ptr->tval == p_ptr->realm2+89) increment = 32;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

    if (increment) realm = p_ptr->realm2-1; else realm = p_ptr->realm1-1;

	/* Ask for a spell */
    if (!get_spell(&spell, ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast")
, sval, TRUE, (increment?TRUE:FALSE)))
	{
        if (spell == -2)
            msg_format("You don't know any %ss in that book.", prayer);
		return;
	}


	/* Access the spell */

    use_realm = (increment?p_ptr->realm2:p_ptr->realm1);

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
		if (flush_failure) flush();


        msg_format("You failed to get the %s off!", prayer);

        if (o_ptr->tval == TV_CHAOS_BOOK && (randint(100)<spell))
        {
            msg_print("You produce a chaotic effect!");
            wild_magic(spell);
        }
        else if (o_ptr->tval == TV_DEATH_BOOK && (randint(100)<spell))
        {
            msg_print("It hurts!");
            take_hit(damroll((o_ptr->sval)+1,6), "a miscast Death spell");
            if (spell>15 && randint(6)==1 && !(p_ptr->hold_life))
                lose_exp(spell * 250);
        }
	}

	/* Process spell */
	else
	{
		/* Hack -- chance of "beam" instead of "bolt" */
		beam = ((p_ptr->pclass == 1) ? plev : (plev / 2));


	/* Spells.  */
	switch (realm)
	{
	case 0: /* * LIFE * */
	  switch (spell)
	  {
	   case 0: /* Detect Evil */
			(void)detect_monsters_evil();
		       break;
	   case 1: /* Cure Light Wounds */
			(void)hp_player(damroll(2, 10));
			(void)set_cut(p_ptr->cut - 10);
		       break;
	   case 2: /* Bless */
			(void)set_blessed(p_ptr->blessed + randint(12) + 12);
		       break; 
	   case 3: /* Remove Fear */
			(void)set_afraid(0);
		       break;
	   case 4: /* Call Light */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		       break;
	   case 5: /* Detect Traps + Secret Doors */
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
		       break;
	   case 6: /* Cure Medium Wounds */
			(void)hp_player(damroll(4, 10));
			(void)set_cut((p_ptr->cut / 2) - 20);
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
			(void)hp_player(damroll(8, 10));
			(void)set_stun(0);
			(void)set_cut(0);
		       break;
	   case 11: /* Sense Unseen */
			(void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
		       break;
	   case 12: /* Orb or Draining */
	   if (!get_aim_dir(&dir)) return;
            fire_ball(GF_HOLY_FIRE, dir,
				(damroll(3, 6) + plev +
					(plev / ((p_ptr->pclass == 2) ? 2 : 4))),
					((plev < 30) ? 2 : 3));
		       break;
	   case 13: /* Protection from Evil */
			(void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
		       break;
	   case 14: /* Healing */
			(void)hp_player(300);
			(void)set_stun(0);
			(void)set_cut(0);
		       break;
	   case 15: /* Glyph of Warding */
			warding_glyph();
		       break;
	   case 16: /* Turn Undead */
			(void)turn_undead();
		       break;
	   case 17: /* Dispel Curse */
			(void)remove_all_curse();
		       break;
	   case 18: /* Dispel Undead */
			(void)dispel_undead(plev * 4);
			break;
	   case 19: /* Dispel Demons */
	    (void)dispel_demons(plev * 4);
		       break;
       case 20: /* Dispel Evil */
			(void)dispel_evil(plev * 4);
		       break;
	   case 21: /* Banishment */
			if (banish_evil(100))
			{
				msg_print("The power of your god banishes evil!");
			}
			break;
	   case 22: /* Holy Word */
	   (void)dispel_evil(plev * 4);
			(void)hp_player(1000);
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
			(void)set_hero(p_ptr->hero + randint(25) + 25);
			(void)hp_player(10);
			(void)set_afraid(0);
		       break;
	   case 25: /* Prayer */
			(void)set_blessed(p_ptr->blessed + randint(48) + 48);
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
		       break;
	   case 28: /* Remembrance */
			(void)restore_level();
		       break;
	   case 29: /* Healing True */
			(void)hp_player(2000);
			(void)set_stun(0);
			(void)set_cut(0);
		       break;
	   case 30: /* Holy Vision */
		identify_fully();
		       break;
	   case 31: /* Holy Invulnerability */
			(void)set_invuln(p_ptr->invuln + randint(7) + 7);
		       break;
	       default:
		 msg_format("You cast an unknown Life spell: %d.", spell);
		 msg_print(NULL);
	   }
	  break;
	
	case 1: /* * SORCERY * */
	  switch (spell)
	  {
	   case 0: /* Detect Monsters */
			(void)detect_monsters_normal();
		       break;
	   case 1: /* Phase Door */
			teleport_player(10);
		       break;
	   case 2: /* Detect Doors and Traps */
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
		       break; 
       case 3: /* Light Area */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
            break;
	   case 4: /* Confuse Monster */
			if (!get_aim_dir(&dir)) return;
            (void)confuse_monster(dir, ( plev * 3) / 2 );
			break;
	   case 5: /* Teleport Self */
            teleport_player(plev * 5);
		       break;
	   case 6: /* Sleep Monster */
			if (!get_aim_dir(&dir)) return;
			(void)sleep_monster(dir);
		       break;
	   case 7: /* Recharging */
               (void)recharge(plev * 2);
		       break;
	   case 8: /* Magic Mapping */
			map_area();
		       break;
	   case 9: /* Identify */
			(void)ident_spell();
		       break;
	   case 10: /* Slow Monster */
			if (!get_aim_dir(&dir)) return;
			(void)slow_monster(dir);
		       break;
	   case 11: /* Mass Sleep */
			(void)sleep_monsters();
		       break;
	   case 12: /* Teleport Away */
			if (!get_aim_dir(&dir)) return;
			(void)teleport_monster(dir);
		       break;
	   case 13: /* Haste Self */
			if (!p_ptr->fast)
			{
				(void)set_fast(randint(20 + (plev) ) + plev);
			}
			else
			{
				(void)set_fast(p_ptr->fast + randint(5));
			}
		       break;
	   case 14: /* Detection True */
			(void)detect_all();
		       break;
	   case 15: /* Identify True */
			identify_fully();
		       break;
	   case 16: /* Detect Invisibility */
			detect_monsters_invis();
		       break;
	   case 17: /* Unbarring Ways */
			(void) destroy_doors_touch();
		       break;
       case 18: /* Detect Objects and Treasure*/
			(void)detect_objects_normal();
			(void)detect_treasure();
			(void)detect_objects_gold();
		       break;
	   case 19: /* Detect Enchantment */
			(void)detect_objects_magic();
		       break;
	   case 20: /* Sense Invisible */
			(void)set_tim_invis(p_ptr->tim_invis + randint(30) + 25);
		       break;
	   case 21: /* Probing */
			(void)probing();
		       break;
	   case 22: /* Teleport Level */
			(void)teleport_player_level();
		       break;
	   case 23: /* Word of Recall */
			{
                if (dun_level && (p_ptr->max_dlv > dun_level))
                {
                    if (get_check("Reset recall depth? "))
                    p_ptr->max_dlv = dun_level;

                }
				if (!p_ptr->word_recall)
				{
					p_ptr->word_recall = rand_int(21) + 15;
					msg_print("The air about you becomes charged...");
				}
				else
				{
					p_ptr->word_recall = 0;
					msg_print("A tension leaves the air around you...");
				}
				break;
			}
		       break;
	   case 24: /* Stasis */
			if (!get_aim_dir(&dir)) return;
			(void)stasis_monster(dir);
		       break;
	   case 25: /* Slow Monsters */
			slow_monsters();
		       break;
       case 26: /* Recharging True -- replaced by Explosive Rune */
               explosive_rune();
		       break;
	   case 27: /* Clairvoyance */
			wiz_lite();
		       break;
	   case 28: /* Enchant Weapon */
			(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
		       break;
	   case 29: /* Enchant Armour */
			(void)enchant_spell(0, 0, rand_int(3) + 2);
		       break;
	   case 30: /* Alchemy */
		       (void) alchemy();
		       break;
	   case 31: /* Globe of Invulnerability */
			(void)set_invuln(p_ptr->invuln + randint(8) + 8);
		       break;
	       default:
		 msg_format("You cast an unknown Sorcery spell: %d.", spell);
		 msg_print(NULL);
	   }
	  break;
	
	case 2: /* * NATURE * */
	  switch (spell)
	  {
	   case 0: /* Detect Creatures */
			(void)detect_monsters_normal();
		       break;
	   case 1: /* First Aid */
			(void)hp_player(damroll(2, 8));
			(void)set_cut(p_ptr->cut - 15);
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
               (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		       break;
       case 5: /* Resist Lightning */
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
		       break;
       case 6: /* Resist Fire and Cold */
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
		       break;
	   case 7: /* Cure Poison */
			(void)set_poisoned(0);
		       break;
	   case 8: /* Stone to Mud */
			if (!get_aim_dir(&dir)) return;
			(void)wall_to_mud(dir);
		       break;
	   case 9: /* Lightning Bolt */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
						  damroll(3+((plev-5)/4), 8));
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
				damroll(5+((plev-5)/4), 8));
		       break;
	   case 12: /* Ray of Sunlight */
			if (!get_aim_dir(&dir)) return;
			msg_print("A line of sunlight appears.");
			lite_line(dir);
		       break;
	   case 13: /* Entangle */
			slow_monsters();
		       break;
	   case 14: /* Ball of Cold */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_COLD, dir,
				30 + (plev), 2);
		       break;
	   case 15: /* Herbal Healing */
			(void)hp_player(1000);
			(void)set_stun(0);
			(void)set_cut(0);
			(void)set_poisoned(0);
		       break;
	   case 16: /* Resist Acid */
			(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
		       break;
	   case 17: /* Door Building */
			(void)door_creation();
		       break;
	   case 18: /* Stair Building */
			(void)stair_creation();
		       break;
	   case 19: /* Stone Skin */
			(void)set_shield(p_ptr->shield + randint(20) + 30);
		       break;
	   case 20: /* Resistance True */
			(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
			(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
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
	   case 24: /* Decompose */
	    if (!get_aim_dir(&dir)) return;
			drain_life(dir, 75);
		       break;
	   case 25: /* Earthquake */
			earthquake(py, px, 10);
		       break;
	   case 26: /* Blizzard */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_COLD, dir,
				70 + (plev), (plev/12)+1);
		       break;
	   case 27: /* Lightning Storm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_ELEC, dir,
				90 + (plev), (plev/12)+1);
		       break;
	   case 28: /* Whirlpool */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_WATER, dir,
				100 + (plev), (plev/12)+1);
		       break;
	   case 29: /* Call Sunlight */
			fire_ball(GF_LITE, 0,
                150, 8);
			wiz_lite();
		       break;
	   case 30: /* Elemental Brand */
            brand_weapon(0);
		       break;
	   case 31: /* Nature's Wrath */
            (void)dispel_monsters(plev * 4);
            earthquake(py, px, 20 + (plev / 2) );
		       break;
	       default:
		 msg_format("You cast an unknown Nature spell: %d.", spell);
		 msg_print(NULL);
	   }
	  break;

	case 3: /* * CHAOS * */
	   switch (spell)
	   {
		case 0: /* Magic Missile */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
						  damroll(3 + ((plev - 1) / 5), 4));
                break;
        case 1: /* Trap / Door destruction, was: Blink */
			(void)destroy_doors_touch();
			break;
        case 2: /* Flash of Light == Light Area */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break; 
        case 3: /* Wallbreaker, was: Trap + Door Destruction */
            wall_breaker();
			break;
        case 4: /* Touch of Confusion */
            if (!(p_ptr->confusing))
            {
                msg_print("Your hands start glowing.");
                p_ptr->confusing = TRUE;
            }
			break;
		case 5: /* Acid Bolt */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
						  damroll(6+((plev-5)/4), 8));
			break;
		case 6: /* Fire Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
				damroll(8+((plev-5)/4), 8));
			break;
		case 7: /* Teleport Self */
			teleport_player(plev * 5);
			break;
		case 8: /* Plasma Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_PLASMA, dir,
				damroll(9+((plev-5)/4), 8));
			break;
		case 9: /* Chaos Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_CHAOS, dir,
				damroll(10+((plev-5)/4), 8));
			break;
		case 10: /* Acid Ball */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_ACID, dir,
				40 + (plev), 2);
			break;
		case 11: /* Doom Bolt */
				if (!get_aim_dir(&dir)) return;
                fire_bolt_or_beam((plev + 45), GF_MANA, dir,
						  damroll(11+((plev-5)/4), 8));
			break;
		case 12: /* Fire Ball */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_FIRE, dir,
					55 + (plev), 2);
			break;
		case 13: /* Teleport Other */
			if (!get_aim_dir(&dir)) return;
			(void)teleport_monster(dir);
			break;
		case 14: /* Word of Destruction */
			destroy_area(py, px, 15, TRUE);
			break;
		case 15: /* Invoke Logrus */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_CHAOS, dir,
					66 + (plev), (plev / 5));
			break;
		case 16: /* Polymorph Wounds */
            do_poly_wounds();
			break;
		case 17: /* Polymorph Other */
			if (!get_aim_dir(&dir)) return;
			(void)poly_monster(dir);
			break;
		case 18: /* Nexus Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_NEXUS, dir,
				damroll(8+((plev-5)/4), 8));
			break;
		case 19: /* Arcane Binding == Charging */
			(void)recharge(40);
			break;
		case 20: /* Nexus Ball */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_NEXUS, dir,
					50 + (plev), 2);
			break;
		case 21: /* Alter Reality */
			msg_print("The world changes!");
			new_level_flag = TRUE;
			break;
		case 22: /* Polymorph Self */
            do_poly_self();
			break;
		case 23: /* Chaos Branding */
            brand_weapon(1);
			break;
		case 24: /* Magic Grenade */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_SHARDS, dir,
					50 + (plev), 2);
			break;
		case 25: /* Meteor Swarm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_METEOR, dir,
				65 + (plev), 3 + (plev/40));
			break;
		case 26: /* Flame Strike */
			fire_ball(GF_FIRE, 0,
                150 + (2*plev), 8);
			break;
		case 27: /* Nuclear Missile */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_NUKE, dir,
					100 + (plev), 2);
			break;
		case 28: /* Call Chaos */
            call_chaos();
			break;
		case 29: /* Magic Rocket */
			if (!get_aim_dir(&dir)) return;
            msg_print("You launch a rocket!");
			fire_ball(GF_ROCKET, dir,
					120 + (plev), 2);
			break;
		case 30: /* Mana Storm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_MANA, dir,
				300 + (plev * 2), 4);
			break;
		case 31: /* Call the Void */
			call_the_();
			break;
		default:
		  msg_format("You cast an unknown Chaos spell: %d.", spell);
		  msg_print(NULL);
	    }
	   break;
	
	case 4: /* * DEATH * */
	  switch (spell)
	  {
	   case 0: /* Detect Undead */
		(void) detect_monsters_xxx(RF3_UNDEAD);
		       break;
	   case 1: /* Detect Demons */
		(void) detect_monsters_xxx(RF3_DEMON);
		       break;
	   case 2: /* Detect Evil */
			(void)detect_monsters_evil();
		       break; 
	   case 3: /* Stinking Cloud */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_POIS, dir,
				10 + (plev / 2), 2);
		       break;
	   case 4: /* Black Sleep */
			if (!get_aim_dir(&dir)) return;
			(void)sleep_monster(dir);
		       break;
	   case 5: /* Resist Poison */
			(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
		       break;
	   case 6: /* Stun */
			if (!get_aim_dir(&dir)) return;
			(void) stun_monster(dir, plev);
		       break;
	   case 7: /* Terror */
			if (!get_aim_dir(&dir)) return;
			(void)fear_monster(dir, plev);
		       break;
	   case 8: /* Turn Undead */
			(void)turn_undead();
		       break;
	   case 9: /* Poison Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_POIS, dir,
				damroll(6+((plev-5)/4), 8));
		       break;
	   case 10: /* Drain Life */
			if (!get_aim_dir(&dir)) return;
			drain_life(dir, 90);
		       break;
	   case 11: /* Vampiric Drain */
		if (!get_aim_dir(&dir)) return;
        for (i = 0; i < 2 + ((p_ptr->lev) / 15); i++)
		    {
            dummy = randint(15) + 5;
			if (drain_life(dir, dummy))
			  (void)hp_player(dummy);
			else
			   break;
		     }
		       break;
       case 12: /* Poison Branding */
            brand_weapon(2);
		       break;
	   case 13: /* Cloud Kill */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
					  60 + plev, (plev/10)+1);
		       break;
	   case 14: /* Genocide */
			(void)genocide();
		       break;
	   case 15: /* Restore Life */
			(void)restore_level();
		       break;
	   case 16: /* Berserk */
            (void)set_shero(p_ptr->shero + randint(25) + 25);
			(void)hp_player(30);
			(void)set_afraid(0);
		       break;
	   case 17: /* Remove Curse */
			remove_curse();
		       break;
	   case 18: /* Dark Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_DARK, dir,
				damroll(4+((plev-5)/4), 8));
		       break;
	   case 19: /* Nether Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_NETHER, dir,
				damroll(6+((plev-5)/4), 8));
		       break;
	   case 20: /* Ball of Darkness */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_DARK, dir,
					40 + (plev), 2);
		       break;
	   case 21: /* Nether Ball */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_NETHER, dir,
					55 + (plev), 2);
		       break;
	   case 22: /* Battle Frenzy */
			(void)set_shero(p_ptr->shero + randint(25) + 25);
            (void)hp_player(30);
			(void)set_afraid(0);
			if (!p_ptr->fast)
			{
				(void)set_fast(randint(20 + (plev / 2) ) + (plev / 2));
			}
			else
			{
				(void)set_fast(p_ptr->fast + randint(5));
			}
		       break;
	   case 23: /* Darkness Storm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_DARK, dir,
					120, 4);
		       break;
	   case 24: /* Death Ray */
			if (!get_aim_dir(&dir)) return;
			(void)death_ray(dir, plev);
		       break;
	   case 25: /* Annihilation */
			if (!get_aim_dir(&dir)) return;
			drain_life(dir, 200);
		       break;
	   case 26: /* Esoteria */
		if (randint(50)>plev)
		    (void) ident_spell();
		else
		    identify_fully();
		       break;
       case 27: /* Vampiric Branding */
            brand_weapon(3);
		       break;
	   case 28: /* Dispel Living */
	    (void)dispel_living(plev * 3);
		       break;
	   case 29: /* Dispel Creature */
	    (void)dispel_monsters(plev * 3);
		       break;
	   case 30: /* Mass Genocide */
			(void)mass_genocide();
		       break;
	   case 31: /* Hellfire */
			if (!get_aim_dir(&dir)) return;
            fire_ball(GF_HELL_FIRE, dir,
					300, 3);
            take_hit(50+randint(50), "the strain of casting Hellfire");
            break;
	       default:
		 msg_format("You cast an unknown Death spell: %d.", spell);
		 msg_print(NULL);
	   }
	  break;

	 default:
		  msg_format("You cast a spell from an unknown realm: realm %d, spell %d.", realm, spell);
		  msg_print(NULL);
	    }
	
#if 0 /* Old code */
		switch (spell)
		{
			case 0:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
						  damroll(3 + ((plev - 1) / 5), 4));
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
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
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
				(void)hp_player(damroll(2, 8));
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
					  10 + (plev / 2), 2);
				break;
			}

			case 9:
			{
				if (!get_aim_dir(&dir)) return;
				(void)confuse_monster(dir, plev);
				break;
			}

			case 10:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
						  damroll(3+((plev-5)/4), 8));
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
						  damroll(5+((plev-5)/4), 8));
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
						  damroll(8+((plev-5)/4), 8));
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
					  30 + (plev), 2);
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
					(void)set_fast(randint(20) + plev);
				}
				else
				{
					(void)set_fast(p_ptr->fast + randint(5));
				}
				break;
			}

			case 30:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
					  55 + (plev), 2);
				break;
			}

			case 31:
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case 32:
			{
				(void)genocide();
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
						  damroll(6+((plev-5)/4), 8));
				break;
			}

			case 39:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
					  20 + (plev / 2), 3);
				break;
			}

			case 40:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir,
					  40 + (plev), 2);
				break;
			}

			case 41:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
					  70 + (plev), 3);
				break;
			}

			case 42:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_METEOR, dir,
					  65 + (plev), 3);
				break;
			}

			case 43:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir,
					  300 + (plev * 2), 3);
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
				(void)genocide();
				break;
			}

			case 48:
			{
				(void)mass_genocide();
				break;
			}

			case 49:
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				break;
			}

			case 50:
			{
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				break;
			}

			case 51:
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				break;
			}

			case 52:
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}

			case 53:
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}

			case 54:
			{
				(void)hp_player(10);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case 55:
			{
				(void)set_shield(p_ptr->shield + randint(20) + 30);
				break;
			}

			case 56:
			{
				(void)hp_player(30);
				(void)set_shero(p_ptr->shero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case 57:
			{
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(30) + 30 + plev);
				}
				else
				{
					(void)set_fast(p_ptr->fast + randint(10));
				}
				break;
			}

			case 58:
			{
				(void)set_invuln(p_ptr->invuln + randint(8) + 8);
				break;
			}
		}
#endif

		/* A spell was cast */
	if (!((increment) ?
	      (spell_worked2 & (1L << spell)) :
	      (spell_worked1 & (1L << (spell)))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
	    if (realm == p_ptr->realm1-1)
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
}


/*
 * Brand the current weapon
 */

/*
 * Pray a prayer -- Unused in Zangband
 */
void do_cmd_pray(void)
{
#if 1
    msg_print
    ("Praying is not used in Zangband. Use 'm'agic spell casting instead.");
#else
	int item, sval, spell, dir, chance;
	int plev = p_ptr->lev;

	object_type     *o_ptr;

	magic_type  *s_ptr;


	/* Must use prayer books */
    if (mp_ptr->spell_book != TV_LIFE_BOOK)
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
	if (!get_item(&item, "Use which book? ", FALSE, TRUE, TRUE))
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
		switch (spell)
		{
			case 0:
			{
				(void)detect_monsters_evil();
				break;
			}

			case 1:
			{
				(void)hp_player(damroll(2, 10));
				(void)set_cut(p_ptr->cut - 10);
				break;
			}

			case 2:
			{
				(void)set_blessed(p_ptr->blessed + randint(12) + 12);
				break;
			}

			case 3:
			{
				(void)set_afraid(0);
				break;
			}

			case 4:
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
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
				teleport_player(plev * 3);
				break;
			}

			case 10:
			{
				(void)hp_player(damroll(4, 10));
				(void)set_cut((p_ptr->cut / 2) - 20);
				break;
			}

			case 11:
			{
				(void)set_blessed(p_ptr->blessed + randint(24) + 24);
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
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
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
				fire_ball(GF_HOLY_ORB, dir,
					  (damroll(3, 6) + plev +
					   (plev / ((p_ptr->pclass == 2) ? 2 : 4))),
					  ((plev < 30) ? 2 : 3));
				break;
			}

			case 18:
			{
				(void)hp_player(damroll(6, 10));
				(void)set_cut(0);
				break;
			}

			case 19:
			{
				(void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
				break;
			}

			case 20:
			{
				(void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
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
				(void)hp_player(damroll(8, 10));
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
				(void)set_blessed(p_ptr->blessed + randint(48) + 48);
				break;
			}

			case 26:
			{
				(void)dispel_undead(plev * 3);
				break;
			}

			case 27:
			{
				(void)hp_player(300);
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
				(void)dispel_evil(plev * 4);
				(void)hp_player(1000);
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
				(void)hp_player(damroll(4, 10));
				(void)set_cut(0);
				break;
			}

			case 37:
			{
				(void)hp_player(damroll(8, 10));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 38:
			{
				(void)hp_player(2000);
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
				(void)dispel_undead(plev * 4);
				break;
			}

			case 42:
			{
				(void)dispel_evil(plev * 4);
				break;
			}

			case 43:
			{
				if (banish_evil(100))
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
				drain_life(dir, 200);
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
				(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
				break;
			}

			case 50:
			{
				(void)enchant_spell(0, 0, rand_int(3) + 2);
				break;
			}

			case 51:
			{
				brand_weapon();
				break;
			}

			case 52:
			{
				teleport_player(10);
				break;
			}

			case 53:
			{
				teleport_player(plev * 8);
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
				new_level_flag = TRUE;
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
#endif
}

