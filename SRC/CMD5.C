/* File: cmd5.c */

/* Purpose: Spell/Prayer commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

/* Note - fix psi spell flags */
#include "angband.h"


extern void do_cmd_rerate(void);
extern void mutate_player(void);
extern bool item_tester_hook_armour(object_type *o_ptr);

/* from spells1.c */
extern int blast_center_x,blast_center_y;

static bool balefire_done = FALSE;
  
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
	int		i;
	int		spell = -1;
	int		num = 0;
	int		ask;

	byte		spells[64];

	bool		flag, redraw, okay;
	char		choice;

	magic_type	*s_ptr;

	char		out_val[160];

	int use_realm = (realm_2?p_ptr->realm2:p_ptr->realm1);
 	cptr p = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" :
		  ((mp_ptr->spell_book == TV_PSI_BOOK) ? "power" : "spell"));

	if (mp_ptr->spell_book == TV_PSI_BOOK)
	  {
	    use_realm = 1;
	    for (i=0 ; i<2 ; i++)
	      for (spell=0 ; spell<32 ; spell++)
		if (psi_spell_flags[sval][i] & (1L << spell))
		  spells[num++] = (i << 5) + spell;
	  }
	else
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

			/* psi: "%32"'s removed to allow psi overflow */

			/* Access the spell */
			s_ptr = &mp_ptr->info[use_realm-1][spell];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
				prompt, (mp_ptr->spell_book == TV_PSI_BOOK) ?
				psi_spell_names[spell] : spell_names[use_realm-1][spell],
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
	int		item;

	object_type	*o_ptr;

	char		o_name[80];

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
	int		item, sval;
	int		spell = -1;
	int		num = 0;

	byte		spells[64];

	object_type	*o_ptr;
 	cptr q,s;
 

	/* Warriors are illiterate */
	if (!(p_ptr->realm1 || p_ptr->realm2))
	{
		msg_print("You cannot read books!");
		return;
	}

#if 0

	/* No lite */
	if (p_ptr->blind || no_lite())
	  if (mp_ptr->spell_book != TV_PSI_BOOK)
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

	if (mp_ptr->spell_book == TV_PSI_BOOK)
	  {
	    q = "Examine which focus? ";
	    s = "You have no foci to examine. ";
	  }
	else
	  {
	    q = "Browse which book? ";
	    s = "You have no books that you can read.";
	  }
	
	/* Get an item (from inven or floor) */
	if (!get_item(&item,q, TRUE, TRUE, TRUE))
	  {
	    if (item == -2) msg_print(s);
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

	/* Hack - 3 levels of psionic foci */
	if (mp_ptr->spell_book == TV_PSI_BOOK) sval /= 3;
  
	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	if (mp_ptr->spell_book == TV_PSI_BOOK)
	  {
	    int i;
	    for (i=0 ; i<2 ; i++)
	      for (spell = 0 ; spell<32 ; spell++)
		if (psi_spell_flags[sval][i] & (1L << spell))
		  spells[num++] = spell + i * 32;
	  }
	else
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
	int	i, item, sval;
	int	increment = 0;
	/* Spells of realm2 will have an increment of +32 */
	int	spell = -1;

 	cptr q,s,name;
 	cptr p = ((mp_ptr->spell_book == TV_SORCERY_BOOK) ? "spell" :
 	((mp_ptr->spell_book == TV_PSI_BOOK) ? "power" : "prayer"));

	object_type             *o_ptr;


	if (!p_ptr->realm1)
	{
		msg_print("You cannot read books!");
		return;
	}

	if (p_ptr->blind || no_lite())
	  if (mp_ptr->spell_book != TV_PSI_BOOK)
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

 	if (mp_ptr->spell_book == TV_PSI_BOOK)
 	  {
 	    q = "Examine which focus? ";
 	    s = "You have no foci to examine. ";
 	  }
 	else
 	  {
	    q = "Study which book? ";
	    s = "You have no books that you can read.";
 	  }

	/* Get an item (from inven or floor) */
	if (!get_item(&item, q, TRUE, TRUE, TRUE))
	  {
	    if (item == -2) msg_print(s);
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

 	if (mp_ptr->spell_book == TV_PSI_BOOK)
	  {
	    if (!get_spell(&spell,"study",sval/3,FALSE,0) && (spell == -1)) 
	      return;
	  }
	else
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
 		msg_format("You cannot learn any %ss in that %s.", p,
 		 (mp_ptr->spell_book == TV_PSI_BOOK) ? "focus" : "book");

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

	name = (mp_ptr->spell_book == TV_PSI_BOOK) ? psi_spell_names[spell] :
	  spell_names[(increment?(p_ptr->realm2)-1:(p_ptr->realm1)-1)][spell%32];
  
	/* Mention the result */
	msg_format("You have learned the %s of %s.",p, name);
		
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
int effects = randint(2);
int tmp = 0;
int new_race;
int more_effects = TRUE;

msg_print("You feel a change coming over you...");

while (effects-- && more_effects)
    {
        switch (randint(14))
        {
        case 1: case 2:
            do_cmd_rerate();
            break;
        case 5: case 6:
            do_poly_wounds();
            break;
        case 7: case 8: case 9:
            (void) gain_random_mutation(0);
            break;
         case 10: case 11: /* Racial polymorph! Uh oh... */
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

            p_ptr->redraw |= (PR_BASIC);

            p_ptr->update |= (PU_BONUS);

            handle_stuff();
          }
          lite_spot(py, px);
          more_effects = FALSE; /* Stop here! */
          break;
        case 12: /* Purposedly "leaks" into default */
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

static void phlogiston (void)
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

 /* Fetch an item (teleport it right underneath the caster) */
 void fetch(int dir, int wgt, bool require_los)
 {
       int ty, tx, i;
       bool flag;
       cave_type *c_ptr;
   object_type *o_ptr;

   /* Check to see if an object is already there */
   if(cave[py][px].o_idx)
   {
       msg_print("You can't fetch when you're already standing on something.");
       return;
   }

   /* Use a target */
   if(dir==5 && target_okay())
   {
       tx = target_col;
       ty = target_row;
       if(distance(py, px, ty, tx)>MAX_RANGE)
       {
           msg_print("You can't fetch something that far away!");
           return;
       }
       c_ptr = &cave[ty][tx];

       if (require_los && (!player_has_los_bold(ty,tx)))
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
       } while(!c_ptr->o_idx);
   }
   o_ptr = &o_list[c_ptr->o_idx];
   if (o_ptr->weight > wgt)
   {   /* Too heavy to 'fetch' */
       msg_print("The object is too heavy.");
       return;
   }
   i = c_ptr->o_idx;
   c_ptr->o_idx = 0;
   cave[py][px].o_idx = i; /* 'move' it */
   o_ptr->iy = py;
   o_ptr->ix = px;


   note_spot(py,px);
   p_ptr->redraw |= PR_MAP;

 }




void wild_magic(int spell)
{
    int counter = 0;
    int type = SUMMON_BIZARRE1 - 1 + (randint(6));
    if (type < SUMMON_BIZARRE1) type = SUMMON_BIZARRE1;
    else if (type > SUMMON_BIZARRE6) type = SUMMON_BIZARRE6;

    switch(randint(spell) + randint(8) + 1)

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
            (void) gain_random_mutation(0);
            break;
        case 29: case 30:
            apply_disenchant(0);
            break;
        case 31:
            lose_all_info();
            break;
        case 32:
            fire_ball(GF_CHAOS, 0, spell + 5, 1 + (spell/10));
            break;
        case 33:
            wall_stone();
            break;
        case 34: case 35:
            while (counter++ < 8)
            {
            (void) summon_specific(py, px, (dun_level * 3) / 2, type);
                    }
            break;
        case 36: case 37:
            activate_hi_summon();
            break;
        case 38:
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
	int	item, sval, spell, dir, realm;
	int	chance, beam;
	int	plev = p_ptr->lev;
	int	increment = 0, dummy = 0;
	int	use_realm, i;
	int	ii = 0, ij = 0;

	bool	no_trump = FALSE;
	const cptr prayer = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell");

	object_type	*o_ptr;

	magic_type	*s_ptr;

        char	ppp[80];

        char	tmp_val[160];

	/* Require spell ability */
	if (p_ptr->realm1 == 1)
	{
	  if (mp_ptr->spell_book == TV_PSI_BOOK)
	    do_cmd_psi();
	  else
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

	if (increment) realm = p_ptr->realm2-1;
	else realm = p_ptr->realm1-1;

	/* Ask for a spell */
	if (!get_spell(&spell, ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
		sval, TRUE, (increment?TRUE:FALSE)))
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

    if (p_ptr->pclass == CLASS_MAGE) beam = plev;
    else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
    else beam = plev / 2;


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
                    (plev / ((p_ptr->pclass == 2
                              || p_ptr->pclass == CLASS_HIGH_MAGE) ? 2 : 4))),
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
       case 16: /* Exorcism */
         (void) dispel_undead(plev);
         (void) dispel_demons(plev);
         (void) turn_evil(plev);
               break;
	   case 17: /* Dispel Curse */
			(void)remove_all_curse();
		       break;
       case 18: /* Dispel Undead + Demons */
            (void)dispel_undead(plev * 3);
        (void)dispel_demons(plev * 3);
			break;
       case 19: /* 'Day of the Dove' */
                  charm_monsters(plev * 2);
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
			(void)restore_level();
		       break;
       case 28: /* Healing True */
			(void)hp_player(2000);
			(void)set_stun(0);
			(void)set_cut(0);
		       break;
       case 29: /* Holy Vision */
		identify_fully();
		       break;
       case 30: /* Divine Intervention */
         project(0, 1, py, px, 777, GF_HOLY_FIRE,   PROJECT_KILL);
         dispel_monsters(plev * 4);
         slow_monsters();
         stun_monsters(plev*4);
         confuse_monsters(plev*4);
         turn_monsters(plev*4);
         stasis_monsters(plev*4);
         summon_specific_friendly(py, px, plev, SUMMON_ANGEL, TRUE);
         (void)set_shero(p_ptr->shero + randint(25) + 25);
         (void)hp_player(300);
         if (!p_ptr->fast) {   /* Haste */
         (void)set_fast(randint(20 + (plev) ) + plev);
         } else {
         (void)set_fast(p_ptr->fast + randint(5));
         }
         (void)set_afraid(0);
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
               (void)fire_beam(GF_AWAY_ALL, dir, plev);
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
                 (void) charm_monster(dir, plev);
               break;
       case 19: /* Dimension Door */
       {
             msg_print("You open a dimensional gate. Choose a destination.");
             if (!tgt_pt(&ii,&ij)) return;
             p_ptr->energy -= 60 - plev;
             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
             (distance(ij,ii,py,px) > plev + 2) ||
             (!rand_int(plev * plev / 2)))
             {
                 msg_print("You fail to exit the astral plane correctly!");
                 p_ptr->energy -= 100;
                 teleport_player(10);
             }
             else teleport_player_to(ij,ii);
             break;
            }

       case 20: /* Sense Minds */
            (void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25);
		       break;
       case 21: /* Self knowledge */
           (void)self_knowledge();
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
       case 24: /* Stasis */
			if (!get_aim_dir(&dir)) return;
			(void)stasis_monster(dir);
		       break;
       case 25: /* Telekinesis */
         if (!get_aim_dir(&dir)) return;
         fetch(dir, plev*15, FALSE);
         break;
       case 26: /* Recharging True -- replaced by Explosive Rune */
               explosive_rune();
		       break;
	   case 27: /* Clairvoyance */
			wiz_lite();
            if (!(p_ptr->telepathy))
            {
                (void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25);
            }
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
            if ((p_ptr->prace == RACE_VAMPIRE) && !(p_ptr->resist_lite))
            {
                msg_print("The daylight scorches your flesh!");
                take_hit(damroll(2,2), "daylight");
                            }
               break;
       case 5: /* Animal Taming */
         if (!get_aim_dir(&dir)) return;
         (void) charm_animal(dir, plev);
         break;
       case 6: /* Resist Environment */
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
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
       case 14: /* Summon Animals */
             if (!(summon_specific_friendly(py, px, plev, SUMMON_ANIMAL_RANGER, TRUE)))
                no_trump = TRUE;
             break;
      case 15: /* Herbal Healing */
			(void)hp_player(1000);
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
			(void)set_shield(p_ptr->shield + randint(20) + 30);
		       break;
       case 19: /* Resistance True */
			(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
			(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
		       break;
        case 20: /* Animal Friendship */
        (void) charm_animals(plev * 2);
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
			earthquake(py, px, 10);
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

			fire_ball(GF_LITE, 0, 150, 8);
			wiz_lite();
            if ((p_ptr->prace == RACE_VAMPIRE) && !(p_ptr->resist_lite))
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
            earthquake(py, px, 20 + (plev / 2) );
         project(0, 1+plev/12, py, px,
             100+plev, GF_DISINTEGRATE, PROJECT_KILL|PROJECT_ITEM);
		       break;
	       default:
		 msg_format("You cast an unknown Nature spell: %d.", spell);
		 msg_print(NULL);
	   }
      if (no_trump)
        msg_print("No animals arrive.");
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
                || (p_ptr->pclass == CLASS_HIGH_MAGE)) ? 2 : 4))),
            ((plev < 30) ? 2 : 3));
          /* Shouldn't actually use GF_MANA, as it will destroy all
       * items on the floor */
             break;
        case 5: /* Fire Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
				damroll(8+((plev-5)/4), 8));
			break;
        case 6: /* Fist of Force ("Fist of Fun") */
			if (!get_aim_dir(&dir)) return;
           fire_ball(GF_DISINTEGRATE, dir,
               damroll(8+((plev-5)/4), 8), 0);
            break;
		case 7: /* Teleport Self */
			teleport_player(plev * 5);
			break;
        case 8: /* Wonder */
           {
           /* This spell should become more useful (more
              controlled) as the player gains experience levels.
              Thus, add 1/5 of the player's level to the die roll.
              This eliminates the worst effects later on, while
              keeping the results quite random.  It also allows
              some potent effects only at high level. */

               int die = randint(100) + plev / 5;

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
                   damroll(3 + ((plev - 1) / 5), 4));
               else if (die < 41) confuse_monster (dir, plev);
               else if (die < 46) fire_ball (GF_POIS, dir, 20 + (plev / 2), 3);
               else if (die < 51) lite_line (dir);
               else if (die < 56)
                   fire_bolt_or_beam (beam - 10, GF_ELEC, dir,
                   damroll(3+((plev-5)/4),8));
               else if (die < 61)
                   fire_bolt_or_beam (beam - 10, GF_COLD, dir,
                   damroll(5+((plev-5)/4),8));
               else if (die < 66)
                   fire_bolt_or_beam (beam, GF_ACID, dir,
                   damroll(6+((plev-5)/4),8));
               else if (die < 71)
                   fire_bolt_or_beam (beam, GF_FIRE, dir,
                   damroll(8+((plev-5)/4),8));
               else if (die < 76) drain_life (dir, 75);
               else if (die < 81) fire_ball (GF_ELEC, dir, 30 + plev / 2, 2);
               else if (die < 86) fire_ball (GF_ACID, dir, 40 + plev, 2);
               else if (die < 91) fire_ball (GF_ICE, dir, 70 + plev, 3);
               else if (die < 96) fire_ball (GF_FIRE, dir, 80 + plev, 3);
               else if (die < 101) drain_life (dir, 100 + plev);
               else if (die < 104) earthquake (py, px, 12);
               else if (die < 106) destroy_area (py, px, 15, TRUE);
               else if (die < 108) genocide(TRUE);
               else if (die < 110) dispel_monsters (120);
               else /* RARE */
               {
                   dispel_monsters (150);
                   slow_monsters();
                   sleep_monsters();
                   hp_player (300);
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
               45+plev, GF_SOUND, PROJECT_KILL|PROJECT_ITEM);
                   break;
        case 11: /* Doom Bolt -- always beam in 2.0.7 or later */
				if (!get_aim_dir(&dir)) return;
                fire_beam(GF_MANA, dir, damroll(11+((plev-5)/4), 8));
			break;
		case 12: /* Fire Ball */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_FIRE, dir,
					55 + (plev), 2);
			break;
		case 13: /* Teleport Other */
           if (!get_aim_dir(&dir)) return;
               (void)fire_beam(GF_AWAY_ALL, dir, plev);
			break;
		case 14: /* Word of Destruction */
			destroy_area(py, px, 15, TRUE);
			break;
		case 15: /* Invoke Logrus */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_CHAOS, dir,
					66 + (plev), (plev / 5));
			break;
        case 16: /* Polymorph Other */
			if (!get_aim_dir(&dir)) return;
			(void)poly_monster(dir);
			break;
        case 17: /* Chain Lightning */
          for (dir = 0; dir <= 9; dir++)
            fire_beam(GF_ELEC, dir, damroll(5+(plev/10), 8));
           break;
        case 18: /* Arcane Binding == Charging */
			(void)recharge(40);
			break;
        case 19: /* Disintegration */
			if (!get_aim_dir(&dir)) return;
           fire_ball(GF_DISINTEGRATE, dir,
               80 + (plev), 3 + (plev/40));
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
			new_level_flag = TRUE;
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
			if (summon_specific(py, px, (plev*3)/2, SUMMON_DEMON))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");
				msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
			}
        	}
		else
		{
			if (summon_specific_friendly(py, px, (plev*3)/2,
				SUMMON_DEMON, (plev == 50 ? TRUE : FALSE)))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");
				msg_print("'What is thy bidding... Master?'");
			}
		}
		break;
        case 24: /* Beam of Gravity */
			if (!get_aim_dir(&dir)) return;
                fire_beam(GF_GRAVITY, dir, damroll(9+((plev-5)/4), 8));
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
               project(0, 2, y, x, (plev*3)/2, GF_METEOR, PROJECT_KILL|PROJECT_JUMP|PROJECT_ITEM);
		       }
		   }
	           break;
#else
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_METEOR, dir,
				65 + (plev), 3 + (plev/40));
			break;
#endif
		case 26: /* Flame Strike */
			fire_ball(GF_FIRE, 0,
                150 + (2*plev), 8);
			break;
        case 27: /* Call Chaos */
            call_chaos();
			break;
        case 28: /* Magic Rocket */
			if (!get_aim_dir(&dir)) return;
            msg_print("You launch a rocket!");
			fire_ball(GF_ROCKET, dir,
					120 + (plev), 2);
			break;
        case 29: /* Mana Storm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_MANA, dir,
				300 + (plev * 2), 4);
            break;
        case 30: /* Breathe Logrus */
               if (!get_aim_dir(&dir)) return;
               fire_ball(GF_CHAOS,dir,p_ptr->chp,
                     2);
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
           damroll(3 + ((plev - 1) / 5), 3), 0);
         if (randint(5)==1) {   /* Special effect first */
         dummy = randint(1000);
         if (dummy == 666)
           fire_bolt(GF_DEATH_RAY, dir, plev);
         else if (dummy < 500)
           fire_bolt(GF_TURN_ALL, dir, plev);
         else if (dummy < 800)
           fire_bolt(GF_OLD_CONF, dir, plev);
         else
           fire_bolt(GF_STUN, dir, plev);
         }
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
       case 6: /* Horrify */
			if (!get_aim_dir(&dir)) return;
			(void)fear_monster(dir, plev);
            (void) stun_monster(dir, plev);
		       break;
       case 7: /* Enslave the Undead */
         if (!get_aim_dir(&dir)) return;
           (void)control_one_undead(dir, plev);
               break;
       case 8: /* Orb of Entropy */
         if (!get_aim_dir(&dir)) return;
         fire_ball(GF_OLD_DRAIN, dir,
           (damroll(3, 6) + plev +
            (plev / (((p_ptr->pclass == CLASS_MAGE)
            || (p_ptr->pclass == CLASS_HIGH_MAGE)) ? 2 : 4))),
           ((plev < 30) ? 2 : 3));
               break;
       case 9: /* Nether Bolt */
			if (!get_aim_dir(&dir)) return;
            fire_bolt_or_beam(beam, GF_NETHER, dir,
				damroll(6+((plev-5)/4), 8));
		       break;
       case 10: /* Terror */
             turn_monsters(30+plev);
             break;
	   case 11: /* Vampiric Drain */
       if (!get_aim_dir(&dir)) return;
       dummy = plev + randint(plev) * MAX(1, plev/10);   /* Dmg */
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
            (void)dispel_good(plev * 4);
		       break;
	   case 14: /* Genocide */
			(void)genocide(TRUE);
		       break;
	   case 15: /* Restore Life */
			(void)restore_level();
		       break;
	   case 16: /* Berserk */
            (void)set_shero(p_ptr->shero + randint(25) + 25);
			(void)hp_player(30);
			(void)set_afraid(0);
		       break;
       case 17: /* Invoke Spirits */
           {
               int die = randint(100) + plev / 5;
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
                          damroll(3 + ((plev - 1) / 5), 4));
               } else if (die < 41) {
               confuse_monster (dir, plev);
               } else if (die < 46) {
               fire_ball (GF_POIS, dir, 20 + (plev / 2), 3);
               } else if (die < 51) {
               lite_line (dir);
               } else if (die < 56) {
               fire_bolt_or_beam (beam - 10, GF_ELEC, dir,
                          damroll(3+((plev-5)/4),8));
               } else if (die < 61) {
               fire_bolt_or_beam (beam - 10, GF_COLD, dir,
                          damroll(5+((plev-5)/4),8));
               } else if (die < 66) {
               fire_bolt_or_beam (beam, GF_ACID, dir,
                          damroll(6+((plev-5)/4),8));
               } else if (die < 71) {
               fire_bolt_or_beam (beam, GF_FIRE, dir,
                          damroll(8+((plev-5)/4),8));
               } else if (die < 76) {
               drain_life (dir, 75);
               } else if (die < 81) {
               fire_ball (GF_ELEC, dir, 30 + plev / 2, 2);
               } else if (die < 86) {
               fire_ball (GF_ACID, dir, 40 + plev, 2);
               } else if (die < 91) {
               fire_ball (GF_ICE, dir, 70 + plev, 3);
               } else if (die < 96) {
               fire_ball (GF_FIRE, dir, 80 + plev, 3);
               } else if (die < 101) {
               drain_life (dir, 100 + plev);
               } else if (die < 104) {
               earthquake (py, px, 12);
               } else if (die < 106) {
               destroy_area (py, px, 15, TRUE);
               } else if (die < 108) {
               genocide(TRUE);
               } else if (die < 110) {
               dispel_monsters (120);
               } else { /* RARE */
               dispel_monsters (150);
               slow_monsters();
               sleep_monsters();
               hp_player (300);
               }

               if (die < 31)
                 msg_print("Sepulchral voices chuckle. 'Soon you will join us, mortal.'");
               break;
           }
	   case 18: /* Dark Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_DARK, dir,
				damroll(4+((plev-5)/4), 8));
		       break;
       case 19: /* Battle Frenzy */
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
        case 20: /* Vampirism True */
			if (!get_aim_dir(&dir)) return;
           for (dummy = 0; dummy < 3; dummy++)
           {
               if (drain_life(dir, 100))
                   hp_player(100);
                }
                   break;
        case 21: /* Vampiric Branding */
            brand_weapon(3);
		       break;
       case 22: /* Darkness Storm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_DARK, dir,
					120, 4);
		       break;
        case 23: /* Mass Genocide */
			(void)mass_genocide(TRUE);
		       break;
       case 24: /* Death Ray */
			if (!get_aim_dir(&dir)) return;
			(void)death_ray(dir, plev);
		       break;
       case 25: /* Raise the Dead */
                   if (randint(3) == 1) {
               if (summon_specific(py, px, (plev*3)/2,
                       (plev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD))) {
               msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
               msg_print("'The dead arise... to punish you for disturbing them!'");
               }
           } else {
               if (summon_specific_friendly(py, px, (plev*3)/2,
                       (plev > 47 ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_UNDEAD),
                       (((plev > 24) && (randint(3) == 1)) ? TRUE : FALSE))) {
               msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
               msg_print("Ancient, long-dead forms arise from the ground to serve you!");
               }
           }
           break;
       case 26: /* Esoteria */
		if (randint(50)>plev)
		    (void) ident_spell();
		else
		    identify_fully();
		       break;
       case 27: /* Word of Death */
	    (void)dispel_living(plev * 3);
		       break;
       case 28: /* Evocation       */
        (void)dispel_monsters(plev * 4);
         turn_monsters(plev*4);
         banish_monsters(plev*4);
		       break;
       case 29: /* Hellfire */
			if (!get_aim_dir(&dir)) return;
            fire_ball(GF_HELL_FIRE, dir,
                    666, 3);
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
        set_shadow(p_ptr->wraith_form + randint(plev/2) + (plev/2));
        break;
	       default:
		 msg_format("You cast an unknown Death spell: %d.", spell);
		 msg_print(NULL);
	   }
	  break;

    case 5: /* TRUMP */
    switch (spell)
    {
        case 0: /* Phase Door */
			teleport_player(10);
        break;
        case 1: /* Mind Blast */
               if (!get_aim_dir(&dir)) return;
                 fire_bolt_or_beam(beam-10, GF_PSI, dir,
                              damroll(3 + ((plev - 1) / 5), 3));
        break;
        case 2: /* Shuffle */

           {
                /* A limited power 'wonder' spell */

               int die = randint(120);

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
                teleport_player(10);
            }
            else if (die <42)
            {
                msg_print("It's Justice.");
                set_blessed(p_ptr->blessed + p_ptr->lev);
            }
            else if (die <47)
            {
                msg_print("It's a teleport trump card.");
                teleport_player(100);
            }
            else if (die <52)
            {
                msg_print("It's a teleport trump card.");
                teleport_player(200);
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
                earthquake(py, px, 5);
            }
            else if (die<82)
            {
                msg_print("It's the picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, (dun_level * 3) / 2, SUMMON_BIZARRE1, FALSE)))
                    no_trump = TRUE;
            }
            else if (die<84)
            {
                msg_print("It's the picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, (dun_level * 3) / 2, SUMMON_BIZARRE2, FALSE)))
                    no_trump = TRUE;
            }
            else if (die<86)
            {
                msg_print("It's the picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, (dun_level * 3) / 2, SUMMON_BIZARRE4, FALSE)))
                    no_trump = TRUE;
            }
            else if (die<88)
            {
                msg_print("It's the picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, (dun_level * 3) / 2, SUMMON_BIZARRE5, FALSE)))
                    no_trump = TRUE;
            }
            else if (die<96)
            {
                msg_print("It's the Lovers.");
                if (!get_aim_dir(&dir)) return;
                (void) charm_monster(dir, MIN(p_ptr->lev, 20));
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
                sprintf(ppp, "Reset to which level (1-%d): ", p_ptr->max_dlv);

                /* Default */
                sprintf(tmp_val, "%d", MAX(dun_level,1));

                /* Ask for a level */
                if (!get_string(ppp, tmp_val, 10)) return;

                /* Extract request */
                dummy = atoi(tmp_val);

                /* Paranoia */
                if (dummy < 1) dummy = 1;

                /* Paranoia */
                if (dummy > p_ptr->max_dlv) dummy = p_ptr->max_dlv;

                /* Accept request */
                msg_format("Recall depth set to level %d (%d').", dummy, dummy * 50 );
            }
        break;
        case 4: /* Teleport Self */
            teleport_player(plev * 4);
        break;
        case 5: /* Dimension Door */
       {
             msg_print("You open a dimensional gate. Choose a destination.");
             if (!tgt_pt(&ii,&ij)) return;
             p_ptr->energy -= 60 - plev;
             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
             (distance(ij,ii,py,px) > plev + 2) ||
             (!rand_int(plev * plev / 2)))
             {
                 msg_print("You fail to exit the astral plane correctly!");
                 p_ptr->energy -= 100;
                 teleport_player(10);
             }
             else teleport_player_to(ij,ii);
             break;
            }
        case 6: /* Trump Spying */
            (void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25);
        break;
        case 7: /* Teleport Away */
			if (!get_aim_dir(&dir)) return;
               (void)fire_beam(GF_AWAY_ALL, dir, plev);
        break;
        case 8: /* Trump Object */
             if (!get_aim_dir(&dir)) return;
                 fetch(dir, plev*15, TRUE);
        break;
        case 9: /* Trump Animal */
        {
            msg_print ("You concentrate on the trump of an animal...");
            if (randint(5)>2)
            {
              if (!(summon_specific_friendly(py, px, plev, SUMMON_ANIMAL_RANGER, FALSE)))
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
               if (summon_specific_friendly(py, px, (plev*3)/2, SUMMON_PHANTOM, FALSE))
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
             if (!(summon_specific_friendly(py, px, plev, SUMMON_NO_UNIQUES, FALSE)))
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
             if (!(summon_specific_friendly(py, px, plev, SUMMON_ELEMENTAL, FALSE)))
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
        case 15: /* Banish */
             banish_monsters(plev*4);
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
                if (!(summon_specific_friendly(py, px, plev, dummy, FALSE)))
                    no_trump = TRUE;
            }
        break;
        case 17: /* Trump Spiders */
        {
            msg_print ("You concentrate on the trump of a spider...");
            if (randint(5)>2)
            {
                if (!(summon_specific_friendly(py, px, plev, SUMMON_SPIDER, TRUE)))
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
             if (!(summon_specific_friendly(py, px, plev, SUMMON_HYDRA, TRUE)))
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
        case 19: /* Trump Hounds */
        {
            msg_print ("You concentrate on the trump of a hound...");
            if (randint(5)>2)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_HOUND, TRUE)))
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
        case 20: /* Trump Branding */
            brand_weapon(4);
        break;
        case 21: /* Living Trump */
        if (randint(8)==1) dummy = 16;
        else dummy = 8;
        if (gain_random_mutation(dummy))
            msg_print("You have turned into a Living Trump.");
        break;
        case 22: /* Death Dealing */
            (void)dispel_living(plev * 3);
        break;
        case 23: /* Trump Cyberdemon */
        {
            msg_print ("You concentrate on the trump of a Cyberdemon...");
            if (randint(10)>3)
            {
              if (!(summon_specific_friendly(py, px, plev, SUMMON_CYBER, FALSE)))
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
             if (!(summon_specific_friendly(py, px, plev, SUMMON_UNDEAD, FALSE)))
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
             if (!(summon_specific_friendly(py, px, plev, SUMMON_DRAGON, FALSE)))
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
                 if (summon_specific_friendly(py, px, plev, SUMMON_NO_UNIQUES, FALSE))
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
             if (!(summon_specific_friendly(py, px, plev, SUMMON_DEMON, FALSE)))
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
             if (!(summon_specific_friendly(py, px, plev, SUMMON_HI_DRAGON_NO_UNIQUES, FALSE)))
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
             if (!(summon_specific_friendly(py, px, plev, SUMMON_HI_UNDEAD_NO_UNIQUES, FALSE)))
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
    break;
    case 6: /* ARCANE */
    switch (spell)
    {
        case 0: /* Zap */
               if (!get_aim_dir(&dir)) return;
                 fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                              damroll(3 + ((plev - 1) / 5), 3));
        break;
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
            teleport_player(10);
        break;
        case 5: /* Light Area */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
        break;
        case 6: /* Trap & Door Destruction */
            if (!(get_aim_dir(&dir))) return;
            (void) destroy_door(dir);
        break;
        case 7: /* Cure Light Wounds */
            (void) hp_player(damroll(2, 8));
            (void) set_cut(p_ptr->cut - 10);
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
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
        break;
        case 15: /* Resist Fire */
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
        break;
        case 16: /* Resist Lightning */
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
        break;
        case 17: /* Resist Acid */
            (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
        break;
        case 18: /* Cure Medium Wounds */
            (void)hp_player(damroll(4, 8));
            (void)set_cut((p_ptr->cut / 2) - 50);
        break;
        case 19: /* Teleport */
            teleport_player(plev * 5);
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
			(void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
        break;
        case 24: /* Recharging */
               (void)recharge(plev * 2);
               break;
        case 25: /* Teleport Level */
			(void)teleport_player_level();
        break;
        case 26: /* Identify */
			(void)ident_spell();
        break;
        case 27: /* Teleport Away */
			if (!get_aim_dir(&dir)) return;
               (void)fire_beam(GF_AWAY_ALL, dir, plev);
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
                    75 + (plev), 2);
        break;
        case 29: /* Detection */
			(void)detect_all();
        break;
        case 30: /* Word of Recall */
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
        case 31: /* Clairvoyance */
			wiz_lite();
            if (!(p_ptr->telepathy))
            {
                (void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25);
            }
        break;
        default:
        msg_format("You cast an unknown Arcane spell: %d.", spell);
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
    ("Praying is not used in Zangband. Use magic spell casting instead.");
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
                if (autosave_l)
                {
                    is_autosave = TRUE;
                    msg_print("Autosaving the game...");
                    do_cmd_save_game();
                    is_autosave = FALSE;
                }
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


static void mindcraft_info(char *p, int power)
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
	int                     i;

	int                     num = 0;
    int y = 1;
    int x = 20;
    int minfail = 0;
    
        int  plev = p_ptr->lev;
    int chance = 0;
    
    bool            flag, redraw;
    int             ask;
	char            choice;

        mindcraft_power spell;
    
	char            out_val[160];
        char            comment[80];
    
    cptr p = "power";

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

       for (i = 0; i < MAX_MINDCRAFT_POWERS; i++)
	      if (mindcraft_powers[i].min_lev <= plev)
		num++;

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

	/* Success */
	return (TRUE);
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'mindcrafter'.
 */
void do_cmd_mindcraft(void)
{
    int   n = 0,  b = 0;
    int chance;
    int dir;
    int minfail = 0;
    int plev = p_ptr->lev;
    
    mindcraft_power   spell;
    
    /* not if confused */
    if (p_ptr->confused) {
	msg_print("You are too confused!");
	return;
    }
    
    /* get power */
    if (!get_mindcraft_power(&n))  return;
	
    spell = mindcraft_powers[n];
    
    /* Verify "dangerous" spells */
    if (spell.mana_cost > p_ptr->csp) {
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

	    if (randint(100) < (chance/2)) {    /* Backfire */
		b = randint(100);
		if (b < 5)   {
		    msg_print("Oh, no! Your mind has gone blank!");
		    lose_all_info();
		} else if (b < 15) {
            msg_print("Weird visions seem to dance before your eyes...");
		    set_image(p_ptr->image + 5 + randint(10));
		} else if (b < 45) {
		    msg_print("Your brain is addled!");
		    set_confused(p_ptr->confused + randint(8));
		} else if (b < 90) {
		    set_stun(p_ptr->stun + randint(8));
		} else {   /* Mana storm */
            msg_print("Your mind unleashes its power in an uncontrollable storm!");
		    project(1, 2+plev/10, py, px,
                plev * 2, GF_MANA,PROJECT_JUMP|PROJECT_KILL|PROJECT_GRID|PROJECT_ITEM);
            p_ptr->csp = MAX(0, p_ptr->csp - plev * MAX(1, plev/10));
		}
	    }
	}  else  {
	    /* spell code */
	    switch (n) {
	     case 0:   /* Precog */
		if (plev > 44)
		  wiz_lite();
		else if (plev > 19)
		  map_area();
		
		if (plev < 30) {
		    b = detect_monsters_normal();
		    if (plev > 14)  b |=  detect_monsters_invis();
		    if (plev > 4)   b |=  detect_traps();
		} else {
		    b = detect_all();
		}
		
		if ((plev > 24) && (plev < 40))
		  set_tim_esp(p_ptr->tim_esp + plev);
		
		if (!b)  msg_print("You feel safe.");
		break;
	     case 1:   /* Mindblast */
		if (!get_aim_dir(&dir)) return;
		if (randint(100) < plev * 2)
          fire_beam(GF_PSI, dir, damroll(3 + ((plev - 1) / 4), (3+plev/15)));
		else
          fire_ball(GF_PSI, dir, damroll(3 + ((plev - 1) / 4), (3+plev/15)), 0);
		break;
	     case 2:   /* Minor displace */
		if (plev < 25) {
		    teleport_player(10);
		} else {
		      int i = 0, j = 0;
		      msg_print("Choose a destination.");
		      if (!tgt_pt(&i,&j)) return;
		      p_ptr->energy -= 60 - plev;
              if (!cave_empty_bold(j,i) || (cave[j][i].info & CAVE_ICKY) ||
			  (distance(j,i,py,px) > plev + 2) ||
			  (!rand_int(plev * plev / 2)))
			  {
                  msg_print("Something disrupts your concentration!");
			      p_ptr->energy -= 100;
			      teleport_player(20);
			  }
		      else teleport_player_to(j,i);
		      break;
		  }
		break;
	     case 3:   /* Major displace */
		if (plev > 29)
		  banish_monsters(plev);
		teleport_player(plev * 5);
		break;
	     case 4:   /* Domination */
		if (plev < 30) {
		    if (!get_aim_dir(&dir)) return;
            fire_ball(GF_DOMINATION, dir, plev, 0);
		} else {
            charm_monsters(p_ptr->lev * 2);
		}
		break;
	     case 5:   /* Fist of Force  ---  not 'true' TK  */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_SOUND, dir, damroll(8+((plev-5)/4), 8),
			  (plev > 20 ? (plev-20)/8 + 1 : 0));
		break;
	     case 6:   /* Character Armour */
		set_shield(p_ptr->shield + plev);
        if (plev > 14)   set_oppose_acid(p_ptr->oppose_acid + plev);
        if (plev > 19)   set_oppose_fire(p_ptr->oppose_fire + plev);
        if (plev > 24)   set_oppose_cold(p_ptr->oppose_cold + plev);
        if (plev > 29)   set_oppose_elec(p_ptr->oppose_elec + plev);
        if (plev > 34)   set_oppose_pois(p_ptr->oppose_pois + plev);
		break;
	     case 7:   /* Psychometry */
		if (plev < 40)
		  psychometry();
		else
		  ident_spell();
		break;
	     case 8:   /* Mindwave */
        msg_print("Mind-warping forces emanate from your brain!");
		if (plev < 25)
		  project(0, 2+plev/10, py, px,
			  (plev*3)/2, GF_PSI, PROJECT_KILL);
		else
          (void)mindblast_monsters(plev * ((plev-5) / 10 + 1));
		break;
	     case 9:   /* Adrenaline */
		set_afraid(0);
		set_stun(0);
		hp_player(plev);
		b = 10 + randint((plev*3)/2);
		if (plev < 35)
		  set_hero(p_ptr->hero + b);
		else
		  set_shero(p_ptr->shero + b);
		
		if (!p_ptr->fast)	{   /* Haste */
		    (void)set_fast(b);
		} else {
		    (void)set_fast(p_ptr->fast + b);
		}
		break;
	     case 10:   /* Psychic Drain */
		if (!get_aim_dir(&dir)) return;
		b = damroll(plev/2, 6);
		if (fire_ball(GF_PSI_DRAIN, dir, b,  0 +
                (plev-25)/10))
		  p_ptr->energy -= randint(150);
		break;
	     case 11:   /* Telekinesis */
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


}

int get_psi_cost(int spell)
{
 
  int cost = mp_ptr->info[spell >> 5][spell & 31].smana;
  switch (spell)
    {
    case 14: /* teleport other */
    case 29: /* complete healing */
    case 31: /* wall of force */
    case 40: /* cannibalize */
    case 41: /* intensify */
    case 42: /* splice */
    case 43: /* receptacle */
    case 46: /* psychic surgery */
    case 49: /* detect monsters */
    case 51: /* reveal secrets */
    case 52: /* clairvoyance */
    case 53: /* m. analysis */
    case 54: case 55: /* read object/aura */
      return cost;
    }
  return (cost * (2 + p_ptr->intensify)) / 2;
}
 
int amplify_dam(int dam)
{
  return (dam * (3 + p_ptr->intensify)) / 3;
}
 
int inverse_amplify(int dam)
{
  return (dam * 3) / (3 + p_ptr->intensify);
}
 
static bool item_tester_hook_drain(object_type *o_ptr)
{
  int t = o_ptr->tval;
  if ((t != TV_STAFF) && (t != TV_WAND)) return FALSE;
  return (o_ptr->pval != 0);
}
 
#define req_focus(lvl) if (f_lev<lvl) {msg_print(\
"Your focus isn't strong enough to use that power."); return;}
 
int min(int a,int b) {return (a<b) ? a : b;}
 
int max(int a,int b) {return (a>b) ? a : b;}
 
int splice_val = 0,splice_n = 0;
 
#define A(x) amplify_dam(x)
 
/*
  * use a psionic power
  */
 
void do_cmd_psi(void)
{
  int                     item, sval, spell, dir, f_lev;
  int                     chance,cost = 0;
  int                     plev = p_ptr->lev;
 
  object_type *o_ptr;
 
  magic_type              *s_ptr;
  char temp[20];
  int i,j;
 
#if 0
  /* Require spell ability */
  if (mp_ptr->spell_book != TV_PSI_BOOK)
    {
      if (mp_ptr->spell_book == TV_MAGIC_BOOK)
 	do_cmd_cast();
      else
 	if (mp_ptr->spell_book == TV_PRAYER_BOOK)
 	  do_cmd_pray();
 	else
 	  msg_print("You cannot use psionics!");
      return;
    }
#endif
  
  /* Not when confused */
  if (p_ptr->confused)
    {
      msg_print("You are too confused!");
      return;
    }
 
  /* Note: psionics *are* possible when blind */
 
  /* Metapsionics allows use of any focus */
  if (meta_psi_lev)
    {
      /* Restrict choices to spell books */
      item_tester_tval = mp_ptr->spell_book;
 
      /* Get an item (from inven or floor) */
      if (!get_item(&item,"Use which discipline? ",TRUE,TRUE,TRUE))
	{
	  if (item == -2)
	    msg_print("You have no psionic foci!");
	  return;
	}
 
      /* Get the item (in the pack) */
      if (item >= 0)
 	o_ptr = &inventory[item];
 
      /* Get the item (on the floor) */
      else
 	o_ptr = &o_list[0 - item];
 
      /* Access the item's sval - extract power and type*/
      sval = o_ptr->sval;
      f_lev = sval % 3;
 
      /* If we are using metapsionics limit focus power */
      if (o_ptr != &inventory[INVEN_NECK]) f_lev = min(meta_psi_lev,f_lev);
      sval /= 3;
    }
  else
    {
      o_ptr = &inventory[INVEN_NECK];
       
      item = 0;
      if (o_ptr) item = (o_ptr->tval == TV_PSI_BOOK);
      if (!item)
 	{
 	  msg_print("You aren't wearing a psionic focus!");
 	  return;
	}
 
      sval = o_ptr->sval;
      f_lev = sval % 3;
      sval /= 3;
    }

  /* Track the object kind */
  object_kind_track(o_ptr->k_idx);
  /* Hack -- Handle stuff */
  handle_stuff();


  /* Ask for a spell */
  if (!get_spell(&spell, "use", sval , TRUE,0))
    {
      if (spell == -2) msg_print("You don't know any powers in that discipline.");
      return;
    }

  /* Access the spell */
  s_ptr = &mp_ptr->info[spell >> 5][spell & 31];
  cost = get_psi_cost(spell);

  /* Verify "dangerous" spells */
  if (cost > p_ptr->csp)
    {
      /* Warning */
      msg_print("You do not have enough mana to use this power.");

      /* Verify */
      if (!get_check("Attempt it anyway? ")) return;
    }


  /* Spell failure chance */
  chance = spell_chance(spell & 31,spell >> 5);

  /* Failed spell */
  if (rand_int(100) < chance)
    {
      if (flush_failure) flush();
      msg_print("You failed to concentrate hard enough!");
      p_ptr->csp += cost >> 1;
      if (splice_val)
 	{
 	  splice_val -= max(damroll(2,8),splice_val / 2);
 	  if (splice_val <= 0) splice_val = 0;
 	}
    }
 
  /* Process spell */
  else
    {
 
      /* Spells.  */
      switch (spell)
	{
 	  /* *** TELEPATHY *** */
 
 	case 0: /* Mind Thrust */
 	  {
 	    if (!get_aim_dir(&dir)) return;
 	    fire_ball(GF_PSI, dir, A(damroll(1 + (plev / 4), 4)),0);
 	    break;
 	  }
 
 	case 1: /* Empathy */
 	  {
 	    if (plev < 25)
 	      {
 		if (!get_aim_dir(&dir)) return;
 		fire_ball(GF_EMPATHY,dir,A(plev),max(4,plev / 5));                      
 	      }
 	    else
 	      project_los(GF_EMPATHY,A(plev));
 	    break;
 	  }
 
 	case 2: /* Mental Barrier */
 	  {
 	    msg_print("You shield your psyche from mental attacks.");
 	    p_ptr->mental_barrier += randint(A(20 + 2 * plev));
 	    p_ptr->resist_conf = TRUE;
 	    p_ptr->resist_fear = TRUE;
 	    p_ptr->sustain_int = TRUE;
 	    p_ptr->sustain_wis = TRUE;
 	    break;
 	  }
 
 	case 3: /* Psychic Crush */
 	  {
 	    req_focus(1);
 	    if (!get_aim_dir(&dir)) return;
 	    fire_ball(GF_PSI2, dir,A(damroll(plev , 9)),0);
 	    break;
 	  }
 
 	case 4: /* Intimidate */
 	  {
 	    if (!get_aim_dir(&dir)) return;
 	    fear_monster(dir,A(plev * 3));
 	    break;
 	  }
 
 	case 5: /* Sleep */
 	  {
 	    if (!get_aim_dir(&dir)) return;
 	    fire_ball(GF_OLD_SLEEP,dir,A(plev * 3),0);
 	    break;
 	  }
 
 	case 6: /* Psionic Blast */
 	  {
 	    if (!get_aim_dir(&dir)) return;
 	    fire_ball(GF_PSI, dir, A(plev * 5),2);
 	    break;
 	  }
 
 	case 7: /* Mind Wrack */
 	  {
 	    req_focus(1);
 	    if (!get_aim_dir(&dir)) return;
 	    fire_ball(GF_PSI3,dir,A(damroll(2 , plev)),0);
 	    break;
 	  }
 
 	case 8: /* Domination */
 	  {
 	    req_focus(2);
 	    if (!get_aim_dir(&dir)) return;
 	    fire_ball(GF_DOMINATION,dir,A(plev + randint(plev * 4)),0);
 	    break;
 	  }
 
#if 0
 	case 9: /* Mass Domination */
 	  {
 	    req_focus(2);
 	    project_los(GF_DOMINATE,A(damroll(3,plev)));
 	    break;
 	  }
 
 	case 10: /* Suicide */
 	  {
 	    msg_print("You command your followers to kill themselves!");
 	    break;
 	  }
#endif 
 
 	case 9: /* Amnesia */
 	  {
 	    req_focus(2);
 	    if (!get_aim_dir(&dir)) return;
 	    fire_ball(GF_AMNESIA,dir,A(plev + (rand_int(2) ? randint(plev * 3) : 0)),0);
 	    break;
 	  }
 	  
 	  /* *** PSYCHOPORTATION *** */
 	  
 	case 10: /* Blink */
 	  {
 	    teleport_player(A(10));
 	    break;
 	  }
 
 	case 11: /* Teleport */
 	  {
 	    teleport_player(A(plev * 5));
 	    break;
 	  }
 
 	case 12: /* Dimension Door */
 	  {
 	    msg_print("You open a dimensional gate. Choose a destination.");
 	    if (!tgt_pt(&i,&j)) return;
 	    p_ptr->energy -= inverse_amplify(60 - plev);
 	    if (!cave_empty_bold(j,i) || (cave[j][i].info & CAVE_ICKY) ||
 		(distance(j,i,py,px) > A(plev + 2)) ||
 		(!rand_int(A(plev * plev / 2))))
 	      {
 		msg_print("You fail to exit the astral plane correctly!");
 		p_ptr->energy -= inverse_amplify(100);
 		teleport_player(A(10));
 	      }
 	    else teleport_player_to(j,i);
 	    break;
 	  }
 
 	case 13: /* Probability Travel */
 	  {
 	    req_focus(1);
 	    p_ptr->prob_travel += A((plev / 8) - 3 + randint(5));
 	    if (p_ptr->prob_travel > 0)
 	      msg_print("You enter the astral plane!");
 	    else 
 	      {
 		msg_print("You fail to reach the astral plane.");
 		p_ptr->prob_travel = 0;
 	      }
 	    break;
 	  }
 
 	case 14: /* Teleport Other */
 	  {
 	    req_focus(1);
 	    if (!get_aim_dir(&dir)) return;
 	    (void)teleport_monster(dir);
 	    break;
 	  }
 
 	case 15: /* Recall */
 	  {
 	    req_focus(2);
	    if (dun_level && (p_ptr->max_dlv > dun_level))
	      if (get_check("Reset recall depth? "))
		p_ptr->max_dlv = dun_level;

 	    if (!p_ptr->word_recall)
 	      {
 		if (p_ptr->ts_anchor)
 		  {
 		    msg_print("Your time/space anchor prevents you from recalling.");
 		    break;
 		  }
 		p_ptr->word_recall = inverse_amplify(rand_int(30));
 		msg_print("The air about you becomes charged...");
 	      }
 	    else
	      {
 		p_ptr->word_recall = 0;
 		msg_print("A tension leaves the air around you...");
 	      }
 	    break;
 	  }

 	case 16: /* Time Control */
 	  {
 	    req_focus(1);
 	    if (p_ptr->ts_anchor)
 	      {
 		msg_print("You end your time/space anchor.");
 		p_ptr->ts_anchor = 0;
 	      }
 	    (void)set_fast(p_ptr->fast + A(plev + randint(20)));
 	    while (p_ptr->fast > 100 + randint(plev * 10))
 	      {
 		msg_print("You accelerate time too much!");
 		i = randint(randint(randint(p_ptr->fast * 3)));
 		_killer = "a blast from the past.";
 		project(-2,rand_int(5),py,px,i,GF_TIME,
 			PROJECT_KILL);
 		p_ptr->fast -= i;
 		if (p_ptr->fast < 0) p_ptr->fast = 0;
 	      }
 	    break;
 	  }
 			  
 	case 17: /* Steal Time */
 	  {
 	    req_focus(2);
 	    if (p_ptr->ts_anchor)
 	      if (!rand_int(3))
 		{
 		  msg_print("The time/space anchor stops your time bolt!");
 		  break;
 		}
 	      else if (!rand_int(3))
 		{
 		  msg_print("Your time/space anchor collapses!");
 		  p_ptr->ts_anchor = 0;
 		}
 	    if (!get_aim_dir(&dir)) return;
	    life_drained = 0;
 	    fire_bolt_or_beam(plev,GF_TIME,dir,A(damroll(10,plev)));
	    if (life_drained)
	      {
		msg_print("You have stolen precious seconds of time!");
		p_ptr->energy += min(life_drained,damroll(2,plev));
	      }
 	    break;
 	  }
 
 	case 18: /* Time/Space Anchor */
 	  {
 	    msg_print("With immense mental effort you temporarily stabilize local space.");
 	    p_ptr->ts_anchor += randint(randint(A(100 + plev)));
 	    break;
 	  }
 
 	case 19: /* Time Shift */
 	  {
 	    req_focus(2);
 	    if (p_ptr->ts_anchor)
 	      {
 		msg_print("You end your time/space anchor.");
 		p_ptr->ts_anchor = 0;
 	      }
 	    msg_print("Time freezes around you!");
 	    p_ptr->energy += A(damroll(5,plev));
 	    break;
 	  }
 
 	  /* *** PSYCHOMETABOLISM *** */
 
 	case 20: /* Cell Adjustment */
 	  {
 	    (void)hp_player(A(damroll(plev / 2, 6)));
 	    (void)set_cut(p_ptr->cut - A(15));
 	    break;
 	  }
 
 	case 21: /* Satisfy Hunger */
 	  {
	    msg_print("You initiate glycolysis through sheer will and start to tremble.");
	    p_ptr->energy -= inverse_amplify(300 - 5 * plev);
 	    (void)set_food(PY_FOOD_MAX - 1);
 	    break;
 	  }
 
 	case 22: /* Adrenaline Control */
 	  {
 	    msg_print("Adrenaline surges through your veins!");
 	    p_ptr->adrenaline += A(randint(20) + plev);
 	    if (p_ptr->biofeedback)
 	      {
 		msg_print("You lose control of your blood flow!");
 		i = randint(randint(p_ptr->adrenaline));
 		take_hit(damroll(2,i),"adrenaline poisoning");
 		p_ptr->adrenaline -= i;
 		p_ptr->biofeedback = 0;
 	      }
 	    while (p_ptr->adrenaline > 30 + damroll(2,plev * 2))
 	      {
 		msg_print("Your body can't handle that much adrenaline!");
 		i = randint(randint(p_ptr->adrenaline));
 		take_hit(damroll(3,i * 2),"adrenaline poisoning");
 		p_ptr->adrenaline -= i;
 	      }
 	    p_ptr->update |= PU_BONUS | PU_HP;
 	    handle_stuff();
 	    break;
 	  }
 		  
 	case 23: /* Biofeedback */
 	  {
 	    msg_print("Your pulse slows and your body prepares to resist damage.");
 	    p_ptr->biofeedback += randint(A(10 + plev));
 	    if (p_ptr->adrenaline)
 	      {
 		msg_print("The adrenaline drains out of your veins.");
 		p_ptr->adrenaline = 0;
 		if (!rand_int(8))
 		  {
 		    msg_print("You start to tremble as your blood sugar crashes.");
 		    set_slow(p_ptr->slow + rand_int(rand_int(16)));
 		    if (!rand_int(5)) set_paralyzed(p_ptr->paralyzed + 1);
 		    if (!rand_int(3)) set_stun(p_ptr->stun + rand_int(30));
 		  }
 	      }
 	    while (p_ptr->biofeedback > 25 + rand_int(rand_int(plev)))
 	      {
 		msg_print("You speed up your pulse to avoid fainting!");
 		p_ptr->biofeedback -= randint(20);
 	      }
 	    break;
 	  }
 		  
 	case 24: /* Shadowform */
 	  {
 	    req_focus(2);
 	    msg_print("You leave the physical world and become a living shadow!");
 	    p_ptr->wraith_form += randint(randint(A(plev + 12)));
 	    if (p_ptr->wraith_form > 30 + randint(plev * 2))
 	      {
 		msg_print("You begin to fade into the netherworld!");
 		i = randint(p_ptr->wraith_form);
 		_killer = "shadows";
 		project(-2,1,py,px,damroll(i,6),GF_NETHER,PROJECT_KILL);
 		p_ptr->wraith_form -= i;
 		if (p_ptr->wraith_form < 0) p_ptr->wraith_form = 0;
 	      }
 	    break;
 	  }
 
 	case 25: /* Drain Life */
 	  {
 	    req_focus(1);
 	    if (!get_aim_dir(&dir)) return;
 	    life_drained = 0;
	    fade_dam_on = TRUE;
 	    fire_bolt_or_beam(plev,GF_OLD_DRAIN,dir,A(damroll(plev,10)));
	    fade_dam_on = FALSE;
 	    if (life_drained) hp_player(life_drained / 2);
 	    break;
 	  }
 			  
 	case 26: /* Double Pain */
 	  {
 	    req_focus(2);
 	    if (!get_aim_dir(&dir)) return;
 	    fire_ball(GF_PAIN,dir,randint(A(plev * 3 + 15)),0);
 	    break;
 	  }
 
 	case 27: /* Energy Contaiment */
 	  {
 	    int n = A(10 + randint(20));
 	    (void)set_oppose_acid(p_ptr->oppose_acid + n);
 	    (void)set_oppose_elec(p_ptr->oppose_elec + n);
 	    (void)set_oppose_fire(p_ptr->oppose_fire + n);
 	    (void)set_oppose_cold(p_ptr->oppose_cold + n);
 	    (void)set_oppose_pois(p_ptr->oppose_pois + n);
 	    break;
 	  }
 
 	case 28: /* Death Field */
 	  {
 	    req_focus(1);
 	    sprintf(temp,"Intensity (1-%d): ",plev);
 	    i = get_quantity(temp,plev);
 	    i = damroll(6,i);
 	    msg_print("Waves of darkness begin emanating from your body.");
 	    take_hit(A(damroll(2,i)),"idiocy");
 	    project(0,3,py,px,A(damroll(15,i)),GF_NETHER,PROJECT_KILL);
 	    break;
 	  }
 
 	case 29: /* Complete Healing */
 	  {
 	    req_focus(1);
 	    msg_print("You enter a trance and your body begins to rapidly recover.");
 	    (void)hp_player(5000);
 	    (void)set_poisoned(0);
 	    (void)set_stun(0);
 	    (void)set_cut(0);
 	    p_ptr->energy -= 350 - plev * 2; /* intensify doesn't help */
 	    break;
 	  }
 
 	  /* *** PSYCHOKINESIS *** */
 
 	case 30: /* Light Control */
 	  {
 	    if (!p_ptr->blind) 
 	      msg_print("Your infrared transforms into a brilliant burst of visible light.");
 	    project(0,(plev / 10) + 1,py,px,A(damroll(3, plev)),
 		    GF_LITE_WEAK, PROJECT_GRID | PROJECT_KILL);
 	    lite_room(py,px);
 	    break;
 	  }
 
 	case 31: /* Wall of Force */
 	  {
 	    if (no_fw == 100)
 	      {
 		msg_print("You already have 100 force walls - that's the limit.");
 		return;
 	      }
 	    msg_print("You create a barrier of pure force.");
 	    if (!tgt_pt(&i,&j)) return;
 	    if (!cave_empty_bold(j,i))
 	      {
 		msg_print("There's something in the way!");
 		break;
 	      }
 	    fw_x[no_fw] = i;
 	    fw_y[no_fw] = j;
 	    no_fw++;
 	    cave[j][i].feat = FEAT_FORCE_WALL;
 	    lite_spot(j,i);
 	    break;
 	  }
 
 	case 32: /* Inertial Barrier */
 	  {
 	    msg_print("The air around you solidifies into a protective shield.");
 	    p_ptr->inertial_barrier += randint(A(20 + plev / 2));
 	    while (p_ptr->inertial_barrier > plev + randint(200))
 	      {
 		msg_print("The inertia makes it difficult to move!");
 		_killer = "air pressure";
 		i = randint(50);
 		project(-2,randint(3),py,px,i * 2,GF_INERTIA,
 			PROJECT_KILL);
 		if ((p_ptr->inertial_barrier -= i) < 0)
 		  p_ptr->inertial_barrier = 0;
 	      }
 	    break;
 	  }
 
 	case 33: /* Project Force */
 	  {
 	    if (!get_aim_dir(&dir)) return;
	    if (plev > 30) plev = 30 + (plev - 30) / 4;
	    fade_dam_on = TRUE;
 	    fire_beam(GF_FORCE,dir,A(damroll(3,plev)));
	    fade_dam_on = FALSE;
 	    break;
 	  }
 
 	case 34: /* Sonic Boom */
 	  {
 	    if (!get_aim_dir(&dir)) return;
	    msg_print("You hear a roar of sound.");
	    if (plev > 30) plev = 30 + (plev - 30) / 4;
 	    fire_ball(GF_SOUND, dir,A(30 + 2 * plev),5);
 	    break;
 	  }
 
 	case 35: /* Disintegrate */
 	  {
 	    req_focus(1);
 	    if (!get_aim_dir(&dir)) return;
	    if (plev > 30) plev = 30 + (plev - 30) / 2;
 	    fire_ball(GF_DISINTEGRATE,dir,A(damroll(plev,9)),2);
	    break;
 	  }
 
 	case 36: /* Sphere of Cold */
 	  {
 	    req_focus(1);
 	    if (!get_aim_dir(&dir)) return;
 	    fire_ball(GF_COLD,dir,A(plev * 9 - 36),1);
 	    break;
 	  }
 
 	case 37: /* Fire Eruption */
 	  {
 	    req_focus(1);
 	    if (!get_aim_dir(&dir)) return;
 	    fire_ball(GF_FIRE,dir,A(plev * 15 - 65),0);
 	    break;
 	  }
 
 	case 38: /* Detonate */
 	  {
	    int radius = 0;
 	    if (!get_aim_dir(&dir)) return;
	    if (!rand_int(plev))
	      {
		msg_print("Your imperfect concentration causes the power to backfire!");
		_killer = "detonate backfire";
		project(-2,2,py,px,A(damroll(15,min(plev,40))),GF_METEOR, 
			PROJECT_GRID | PROJECT_KILL);
		do {radius += randint(6);} while (!rand_int(3));
		radius = (radius < 5) ? 10 : radius * 2;
	      }
	    else
	      {
		fire_ball(GF_METEOR,dir,A(damroll(15,min(plev,40))),2);
		if (!rand_int(8))
		  {
		    msg_print("The dungeon collapses around the blast");
                    do {radius += randint(6);} while (!rand_int(3));
		  }
	      }
	    if (radius) earthquake(blast_center_y,blast_center_x,radius);
 	    break;
 	  }
 
 	case 39: /* Balefire */
 	  {
 	    req_focus(2);
 	    if (!get_aim_dir(&dir)) return;
 	    balefire_done = FALSE;
 	    fire_beam(GF_BALEFIRE,dir,A(damroll(5,p_ptr->chp)));
 	    break;
 	  }
 
 	  /* *** METAPSIONICS *** */
 
 	case 40: /* Cannibalize */
 	  {
 	    int m = 0,h;
 	    do
 	      {
 		m = get_quantity("How much mana?",999);
 		if (p_ptr->precognition)
 		  msg_print(format("That would probably leave you with %d hp.",
 				   p_ptr->chp - (m * 5) / 2));
 		if (!m) return;
 	      } while (!get_check(format("Confirm %d mana?",m)));
 	    m += rand_int(6) - 2;
 	    if (m<0) return;
 	    p_ptr->csp += m;
 	    for (h=0;m;m--) h += rand_int(6);
 	    msg_print("You begin converting your life force into psionic power.");
 	    take_hit(h,"idiocy");
 	    break;
 	  }
 
	case 41: /* Intensify */
 	  {
 	    p_ptr->intensify = get_quantity(format("Intensify level? (max %d):",
						    (plev + 7) / 8),(plev + 7) / 8);
 	    /* msg_print("x<=(L+7)/8;main x/L;power*=1+x/3;cost*=1+x/2"); */
 	    break;
 	  }
 
 	case 42: /* Splice */
 	  {
 	    req_focus(1);
 	    if (splice_val)
 	      {
 		splice_val = 0;
 		return;
 	      }
 	    splice_val = get_quantity(format( \
"Type in amount of mana to spend to begin splice (max %d).",plev),plev);
 	    splice_n = 0;
 	    p_ptr->csp -= splice_val;
 	    while (splice_val) do_cmd_psi();
 	    msg_print("Your splice unravels.");
 	    break;
 	  }
 
 	case 43: /* Receptacle */
 	  item_tester_hook = item_tester_hook_drain;
 	  if (!get_item(&item, "Drain what item?", TRUE,TRUE,TRUE))
	    {
	      msg_print("Nothing charged to drain");
	      return;
	    }
 			
 	  if (item >= 0) o_ptr = &inventory[item];
 	  else o_ptr = &o_list[0 - item];
 
 	  msg_print("Energy drains from your pack!");
 
 	  i = 0;
 	  while ((p_ptr->csp < p_ptr->msp) && (o_ptr->pval > 0) &&
 		 (i < 6 + randint(60)))
 	    {
 	      p_ptr->csp += damroll(o_ptr->number,plev);
 	      j = randint((plev > 40) ? randint(6) : 6);
 	      o_ptr->pval -= j;
 	      i += j;
 	    }
 
 	  if ((j = o_ptr->pval) < 0) o_ptr->pval = 0;
 	  else
 	    if ((i = i * i - 40) > 0)
 	      if (rand_int(2)) /* Straight up 50% save */
 		if (damroll(3,20) < i + damroll(j,6))
 		  {
 		    msg_print("There is a bright flash of light.");
              			/* Reduce and describe inventory */
 		    if (item >= 0)
 		      {
 			inven_item_increase(item, -999);
 			inven_item_describe(item);
 			inven_item_optimize(item);
 		      }
 
 		    /* Reduce and describe floor item */
 		    else
 		      {
 			floor_item_increase(0 - item, -999);
 			floor_item_describe(0 - item);
 			floor_item_optimize(0 - item);
 		      }
 
 		    i = damroll(j,(j>20 ? 16 : j));
 		    _killer = "a burst of magical energy";
 		    project(-2,i > 50 ? 3 : 2,py,px,i,GF_MANA,
 			    PROJECT_GRID | PROJECT_KILL | PROJECT_ITEM);
 		  }
 
 	  /* Combine / Reorder the pack */
 	  p_ptr->notice |= (PN_COMBINE | PN_REORDER);
 
 	  /* Window stuff */
 	  p_ptr->window |= (PW_INVEN);
 
 	  /* Done */
 	  break;
 
 	case 44: /* Empower */
 	  {
 	    recharge(rand_int(3)?100:100*randint(randint(plev)));
 	    break;
 	  }
 
 	case 45: /* Psychic Drain */
 	  {
 	    req_focus(2);
 	    if (!get_aim_dir(&dir)) return;
 	    life_drained = 0;
 	    fade_dam_on = TRUE;
 	    fire_ball(GF_PSI_DRAIN,dir,A(damroll(plev/2,6)),1 + !rand_int(6) - !rand_int(5));
 	    fade_dam_on = FALSE;
 	    p_ptr->csp += damroll(5,life_drained) >> 2;
 	    p_ptr->energy -= randint(150);
 	    break;
 	  }
 
 	case 46: /* Psychic Surgery */
 	  {
 	    restore_level();
 	    set_afraid(0);
 	    (void)do_res_stat(A_WIS);
 	    (void)do_res_stat(A_INT);
 	    break;
 	  }
                                
 	case 47: /* Ultrablast */
 	  {
 	    req_focus(1);
	    msg_print("You project waves of psychic energy with all your strength!");
 	    project_los(GF_PSI,A(damroll(40,plev)));
	    if (!rand_int(plev))
	      {
		int temp = 0;
		msg_print("You lose control over the attack and your mana drains away.");
		do {temp += cost;} while (!rand_int(3));
		cost = temp;
	      }
 	    break;
 	  }
  			       
 	case 48: /* *Ultrablast* */
 	  {
 	    req_focus(2);
	    msg_print("You project deadly psychic energy with all your strength!");	 
 	    project_los(GF_PSI2,A(damroll(100,plev)));
	    if (!rand_int(plev - 25))
	      {
		int temp = 0;
		msg_print("You lose control over the attack and your mana drains away.");
		do {temp += cost;} while (!rand_int(5));
		cost = temp;
	      }
 	    break;
 	  }
 
 	  /* *** CLAIRSENTIENCE *** */
 
 	case 49: /* Detect Monsters */
 	  {
 	    (void)detect_monsters_normal();
 	    break;
 	  }
 
 	case 50: /* Awareness */
 	  {
 	    msg_print("You become aware of nearby intelligence!");
 	    p_ptr->tim_esp += A(plev * plev / 5 + randint(80));
 	    p_ptr->telepathy = TRUE;
 	    p_ptr->update |= PU_MONSTERS;
 	    p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP);
 	    handle_stuff();
 	    break;
 	  }
 
 	case 51: /* Reveal Secrets */
 	  {
 	    detect_doors();
 	    detect_traps();
 	    detect_stairs();
 	    break;
 	  }
 
 	case 52: /* Clairvoyance */
 	  {
 	    msg_print("An image of your surroundings forms in your mind...");
 	    map_area();
 	    break;
 	  }
 
 	case 53: /* Metaphysical analysis */
 	  {
	    show_aura();
	    show_durations();
 	    break;
 	  }
 
 	case 54: /* Read Object */
 	  {
 	    req_focus(1);
 	    (void)ident_spell();
 	    break;
 	  }
 
 	case 55: /* Read Aura */
 	  {
 	    req_focus(2);
 	    (void)identify_fully();
 	    break;
 	  }
 
 	case 56: /* Precognition */
 	  {
 	    req_focus(2);
 	    msg_print("Images from the future begin to flash before your eyes.");
 	    p_ptr->precognition += A(randint(plev));
 	    break;
 	  }
 
 	case 57: /* Analyze Monster */
 	  {
 	    req_focus(2);
 	    if (!get_aim_dir(&dir)) return;
 	    fire_ball(GF_ANALYZE,dir,A(2 * plev),0);
 	    break;
 	  }
 
 	default:
 	  {
 	    msg_format("%s not implemented",
 		       spell_names[mp_ptr->spell_type][spell]);
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
      if (splice_val)
 	{
 	  command_dir = 0; /* change targets */
 	  splice_val -= cost / 3 + ++splice_n;
 	  if (splice_val < 0) splice_val = 0; else
 	    msg_format("%d splice points remaining.",splice_val);
 	}
 
    }
 
  /* Take a turn */
  energy_use = 100;
 
  /* Sufficient mana */
  if (cost <= p_ptr->csp)
    {
      /* Use some mana */
      p_ptr->csp -= cost;
    }
 
  /* Over-exert the player */
  else
    {
      int oops = cost - p_ptr->csp;
 
      /* No mana left */
      p_ptr->csp = 0;
      p_ptr->csp_frac = 0;
 
      /* Message */
      msg_print("You faint from the effort!");
 
      /* Hack -- Bypass free action */
      (void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));
 
      if (rand_int(100) < 50)
 	{
 	  bool perm = (rand_int(100) < 25);
 
 	  /* Message */
 	  msg_print("You have damaged your psyche!");
 
 	  (void)dec_stat(A_INT, 15 + randint(10), perm);
 	  (void)dec_stat(A_WIS, 15 + randint(10), perm);
 	}
    }
 
  if (p_ptr->csp >= p_ptr->msp)
    {
      p_ptr->csp = p_ptr->msp;
      p_ptr->csp_frac = 0;
    }
 
  /* Redraw mana */
  p_ptr->redraw |= (PR_MANA);
 
  /* Window stuff */
  p_ptr->window |= PW_PLAYER;
}
 
#undef A


int get_balefire_effect()
{
  int result = 0;
  if (balefire_done) return 0;
  balefire_done = TRUE;
  if (rand_int(inverse_amplify(10))) return 0;
  msg_print("The balefire tears a hole in reality!");
  if (p_ptr->ts_anchor)
    {
      msg_print("Your time/space anchor is ripped apart!");
      p_ptr->ts_anchor = 0;
      if (!rand_int(6)) return BALE_ABORT;
    }
  if (!rand_int(8)) return BALE_ABORT;
  while (!result)
    {
      if (!rand_int(12)) result |= BALE_HOLE;
      if (!rand_int(12)) result |= BALE_SUMMON;
      if (!rand_int(8)) result |= rand_int(3) ? (rand_int(2) ? BALE_KILL : BALE_CLONE) : BALE_HEAL;
      if (!rand_int(8)) result |= BALE_TIME_ATTACK;
      // if (!rand_int(20)) result |= BALE_TIME_TRAVEL;
      if (!rand_int(20)) result |= BALE_HISTORY;
      if (!rand_int(12)) result |= BALE_ALTER;
      if (!rand_int(30)) result |= BALE_ALTER | BALE_TIME_TRAVEL;
      if (!rand_int(40)) result |= BALE_HOLE | BALE_SUMMON | BALE_CLONE | BALE_TIME_ATTACK;
    }
  return result;
}

void time_travel(bool forward)
{
  msg_format("You are sent far into the %s...",forward ? "future" : "past");
  if ((town_special & ~TOWN_MESSAGE) == TOWN_DEAD_FUTURE)
    {
      if (forward)
	switch (rand_int(5))
	  {
	  case 1:
	    msg_print("You arrive in a world where Morgoth has achieved total domination.");
	    msg_print("Your quest hopeless, you are forced to surrender to his armies.");
	    msg_print("You live the rest of your life in slavery remembering your mistakes...");
	    p_ptr->chp = 0;
	    take_hit(1,"an orcish slavemaster");
	    break;
	  case 2:
	    msg_print("You arrive to find Morgoth's empire replaced by...");
	    msg_print("a republic of insects and grasses.");
	    msg_print("No enemy is in sight and the air is clean and fresh.");
	    msg_print("Unfortunately, your armor can't stop the hard radiation");
	    msg_print("suffusing the atmosphere.  You soon realize that the");
	    msg_print("price of total victory has been total war.");
	    p_ptr->chp = 0;
	    take_hit(1,"Uranium-238");
	    break;
	  default:
	    msg_print("You arrive even further in the future.");
	    msg_print("You feel vaguely relieved.");
	    /* empty_house() */
	    break;
	  }
      else
	{
	  msg_print("Hopefully, this time you're back in the present!");
	  town_special = TOWN_MESSAGE; /* optionally with TOWN_WEIRD */
	}
    }
  else if ((town_special & ~TOWN_MESSAGE) == TOWN_DEAD_PAST)
    {
      if (forward)
	{
	  msg_print("Hopefully, this time you're back in the present!");
	  town_special = TOWN_MESSAGE; /* optionally with TOWN_WEIRD */
	}
      else
	switch (rand_int(6))
	  {
	  case 1: 
	    msg_print("You arrive in an epoch without plants.");
	    msg_print("You never liked vegetables, but now, with no oxygen ");
	    msg_print("in the atmosphere, you start to appreciate them.");
	    p_ptr->chp = 0;
	    take_hit(1,"asphyxiation");
	    break;
	  case 2:
	    msg_print("You arrive in a prehistoric epoch.");
	    msg_print("You accidently step on a caterpillar...");
	    msg_print("The entire course of human history is changed and,");
	    msg_print("paradoxically, you are never born.");
	    p_ptr->chp = 0;
	    take_hit(1,"a bizarre paradox");
	    break;
	  default:
	    msg_print("You arrive even further in the past.");
	    msg_print("You feel vaguely relieved.");
	    /* empty_house() */
	    break;
	  }
    }
  else if (!(town_special & ~TOWN_MESSAGE))
    {
      town_special = TOWN_MESSAGE | (forward ?  TOWN_DEAD_FUTURE : TOWN_DEAD_PAST);
      msg_print("You feel lonely for a moment...");
      msg_print("You find yourself contemplating how little human interaction you've");
      msg_print("had during your years in the pits of Angband and can't help a feeling");
      msg_print("of regret.  You quash it and continue your quest.");
    }
}

void do_balefire_effects(int effect)
{
  if (effect & BALE_TIME_TRAVEL)
    {
      time_travel(rand_int(2));
      if (!dun_level) effect |= BALE_ALTER;
    }
  if (effect & BALE_HISTORY)
    {
      seed_town = rand_int(0x10000000);
      /* other effects ? never born ? TOWN_WEIRD */
      msg_print("You have changed the entire course of human history!");
      msg_print(NULL);
      if (dun_level == 0)
	{
	  new_level_flag = TRUE;
	  effect &= ~BALE_ALTER;
	}
    }
  if (effect & BALE_ALTER)
    {
      msg_print("The world changes!");
      msg_print(NULL);
      new_level_flag = TRUE;
    }
  if (effect & BALE_TIME_ATTACK)
    {
      msg_print("Temporal rifts erupt all around you!");
      msg_print(NULL);
      _killer = " a rupture in space/time.";
      project(-2,rand_int(5),py,px,rand_int(rand_int(400)),
	      GF_TIME,PROJECT_KILL);
    }
  if (effect & BALE_SUMMON)
    {
      int count = 0,n=0;
      msg_print("Time vortices spin into existence all around you!");
      msg_print(NULL);
      while ((count < 10) && (n < 1000))
	{
	  count += summon_specific(py,px,100,SUMMON_TIME);
	  n++;
	}
    }
  if (effect & BALE_HOLE)
    {
      int x,y,count=0;
      msg_print("The balefire has left a time/space anomaly!");
      msg_print("It seems to be expanding...");
      msg_print(NULL);
      do
	{
	  x = px + rand_int(9) - 4;
	  y = py + rand_int(9) - 4;
	  scatter(&y,&x,y,x,10,0);
	  count++;
	}
      while ((count < 1000) && (distance(x,y,px,py) < 3)); /* mercy */
      /* summon_specific(py,px,dun_level,SUMMON_HOLE); */
    }
  if (effect & (BALE_CLONE | BALE_KILL | BALE_HEAL))
    ; /* these effects aren't handled here */
}

/* this may need a "#ifdef DJGPP" */
#define print_dur(dur) \
do {if (p_ptr->dur) msg_format(#dur " %d",p_ptr->dur);} while (0)

void show_durations()
{
  print_dur(fast);
  print_dur(slow);
  print_dur(blind);
  print_dur(paralyzed);
  print_dur(confused);
  print_dur(afraid);
  print_dur(image);
  print_dur(poisoned);
  print_dur(stun);
  print_dur(cut);
  print_dur(protevil);
  print_dur(invuln);
  print_dur(hero);
  print_dur(shero);
  print_dur(shield);
  print_dur(blessed);
  print_dur(tim_invis);
  print_dur(tim_infra);
  print_dur(oppose_acid);
  print_dur(oppose_elec);
  print_dur(oppose_fire);
  print_dur(oppose_cold);
  print_dur(oppose_pois);
  print_dur(tim_esp);
  print_dur(wraith_form);
  print_dur(biofeedback);
  print_dur(adrenaline);
  print_dur(inertial_barrier);
  print_dur(prob_travel);
  print_dur(precognition);
  print_dur(mental_barrier);
  print_dur(ts_anchor);
  print_dur(intensify);
  print_dur(word_recall);
}

/* when aura changes
   -sometimes display nothing
   -sometimes print "something's changed"
   -sometimes print "something's better/worse"
   -sometimes print type of aura
   -sometimes print approximate strength

   when leaving a level,
   sometimes keep the aura
   if leaving a powerful aura increase nightmare chances
*/

void show_aura()
{
  if (dungeon_aura_type & AURA_HARD_MAGIC)
    msg_print("Magic is more difficult.");
  if (dungeon_aura_type & AURA_CORRUPT_MAGIC)
    msg_print("An evil aura attempts to corrupt your spells.");
  if (dungeon_aura_type & AURA_STRANGE_MAGIC)
    msg_print("A chaotic aura threatens to disrupt your spells.");
  if (dungeon_aura_type & AURA_WEAK_MAGIC)
    msg_print("Magical attacks are less damaging.");
  if (dungeon_aura_type & AURA_MIS_TELE)
    msg_print("Teleportation has become unreliable.");
  if (dungeon_aura_type & AURA_RADIATION)
    msg_print("Hard radiation fills the air.");
  if (dungeon_aura_type & AURA_WEAKNESS)
    msg_print("The hostile atmosphere makes you weaker.");
  if (dungeon_aura_type & AURA_SLOW)
    msg_print("Time moves faster for the rest of the level.");
  if (dungeon_aura_type & AURA_CHAOS)
    msg_print("Chaotic effects randomly occur.");

  if (dungeon_aura_level)
    msg_format("Aura strength %d",dungeon_aura_level);
  else
    msg_print("There is no level aura.");
}

