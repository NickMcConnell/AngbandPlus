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



/* Fetch an item (teleport it right underneath the caster) */
static void fetch(int dir, int wgt)
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
	{	/* Too heavy to 'fetch' */
		msg_print("The object is too heavy.");
		return;
	}
	i = c_ptr->o_idx;
	c_ptr->o_idx = 0;
	cave[py][px].o_idx = i; /* 'move' it */
	o_ptr->iy = py;
	o_ptr->ix = px;

	p_ptr->redraw |= PR_MAP;
}

/* Restore randint(max) of mana */
static void restore_mana(int max)
{
	p_ptr->csp += randint(max);
	if(p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;
	p_ptr->redraw = PR_MANA;
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

		ego_item_type *e_ptr;

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
		e_ptr = &e_info[o_ptr->name2];
		o_ptr->flags1 |= e_ptr->flags1;
		o_ptr->flags2 |= e_ptr->flags2;
		o_ptr->flags3 |= e_ptr->flags3;
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

/* Puts all creatures on a level to sleep */
static void mass_sleep()
{
	int i;
	byte lev;
	monster_type *m_ptr;

	lev = (smod(S_MAGIC) + p_ptr->lev)/2;

	for(i=1; i<m_max; i++)
	{
		m_ptr = &m_list[i];
		if(m_ptr->r_idx)
			(void)project(0, 0, m_ptr->fy, m_ptr->fx, lev, GF_MASS_SLEEP,
				PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE);
	}
}

/*
 * Shapechange code. Most of the work is done by calc_bonuses().
 * We just handle the messages and whatnot.
 */
void shapechange(s16b shape)
{
  char *shapedesc;

  p_ptr->schange = shape;
  p_ptr->update |= PU_BONUS;

  switch (shape) {
  case SHAPE_SHEEP:
    shapedesc = "sheep";
    break;
  case SHAPE_GOAT:
    shapedesc = "goat";
    break;
  case SHAPE_BEAR:
    shapedesc = "bear";
    break;
  case SHAPE_LION:
    shapedesc = "lion";
    break;
  case SHAPE_GAZELLE:
    shapedesc = "gazelle";
    break;
  case SHAPE_CHEETAH:
    shapedesc = "cheetah";
    break;
  case SHAPE_DRAGON:
    shapedesc = "dragon";
    break;
  default:
    msg_print("You return to your normal form.");
    return;
  }
  msg_format("You assume the form of a %s.", shapedesc);
  msg_print("Your equipment merges into your body!");
}

/* Mod to AC, +to_h, +to_d, Speed, Offset of Karate type */
/* static void change(s16b ac, s16b to_h, s16b to_d, s16b spd, s16b i) */
/* { */
/* 	p_ptr->ac_mod=ac; */
/* 	p_ptr->to_h_mod=to_h; */
/* 	p_ptr->to_d_mod=to_d; */
/* 	p_ptr->pspeed_mod = spd; */
/* 	p_ptr->technique = i; */
/* 	p_ptr->update |= PU_BONUS; */
/* } */

/* This will return the amount of damage this type of attack should do in
   this weather */
static int weather(int typ, int da)
{
  int w,dam;
	dam=da;
	w = p_ptr->weather;
	switch(typ)
	{
		case GF_MISSILE: /* "winds" */
		{
			if (w & W_WINDY)
			  dam=dam*4/3;
			else if (w & W_STILL)
			  dam=0;
			break;
		}
		case GF_FIRE:
		{
			if (w & W_WARM)
				dam=dam*4/3;
			else if (w & W_COOL)
				dam=0;
				break;
		}
		case GF_COLD:
		{
			if (w & W_COOL)
				dam*=4/3;
			else if (w & W_WARM)
				dam=0;
			break;
		}
		case GF_ELEC:
		{
			if (w & W_DRY)
				dam=dam*4/3;
			else if (w & W_MOIST)
				dam=dam/2;
			if (w & W_COOL)
				dam=dam*4/3;
			else if (w & W_WARM)
				dam=dam/2;
			if (dam<(da/3))
				dam=0;
			break;
		}
		case GF_POIS:
		{
			if (w & W_STILL)
				dam=dam*4/3;
			else if (w & W_WINDY)
				dam=0;
			break;
		}
	}
	return dam;
}

/* Attack a monster with a druid spell, affected by weather */
static void attack(int typ, int dam, int ball)
{
	int result,da,dir;
	result=1;
	da=weather(typ,dam);
	if (da)
	{
		if (get_aim_dir(&dir))
		{
			if (!ball) /* bolt */
				fire_bolt(typ, dir, dam);
			else
				fire_ball(typ, dir, dam, ball);
		}  
	}
}


/*
 * Get the "name" of spells (ie, "spell", "prayer", or "technique")
 */
static cptr spell_type()
{
	switch(p_ptr->realm)
	{
		case PRIEST: return("prayer"); break;
		case DRUID: return("technique"); break;
		default: return("spell"); break;
	}
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
static int get_spell(int *sn, cptr prompt, int sval, bool known)
{
	int			i, j = -1;

	byte		spell[64], num = 0;
	bool		flag, redraw, okay, ask;
	char		choice;
	magic_type		*s_ptr;
	char		out_val[160];

	cptr p = spell_type();
	/* Extract spells */
	for (i = 0; i < 64; i++)
	{
		/* Check for this spell */
		if ((i < 32) ?
		    (spell_flags[p_ptr->realm-1][sval][0] & (1L << i)) :
		    (spell_flags[p_ptr->realm-1][sval][1] & (1L << (i - 32))))
		{
			/* Collect this spell */
			spell[num++] = i;
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
		if (spell_okay(spell[i], known)) okay = TRUE;
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
				print_spells(spell, num, 1, 20);
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
		j = spell[i];
		/* Require "okay" spells */
		if (!spell_okay(j, known))
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
			s_ptr = &mp_ptr->info[j];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
			        prompt, spell_names[p_ptr->realm-1][j],
			        s_ptr->smana, spell_chance(j));
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
	(*sn) = j;

	/* Success */
	return (TRUE);
}

/*
 * Peruse the spells/prayers in a Book
 *
 * Note that *all* spells in the book are listed
 */
void do_cmd_browse(void)
{
	int			i, item, sval;
	byte		spell[64], num = 0;
	object_type		*o_ptr;

	/* Warriors are illiterate */
	if (!mp_ptr->spell_book)
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

	/* Confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}


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

	/* Extract spells */
	for (i = 0; i < 64; i++)
	{
		/* Check for this spell */
		if ((i < 32) ?
		    (spell_flags[p_ptr->realm-1][sval][0] & (1L << i)) :
		    (spell_flags[p_ptr->realm-1][sval][1] & (1L << (i - 32))))
		{
			/* Collect this spell */
			spell[num++] = i;
		}
	}


	/* Save the screen */
	Term_save();
	/* Display the spells */
	print_spells(spell, num, 1, 20);

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
	int			i, item, sval;

	int			j = -1;

	cptr p = spell_type();

	object_type		*o_ptr;

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

	/* Priest -- Learn a random prayer */
	if (mp_ptr->spell_book == TV_PRAYER_BOOK)
	{
		int k = 0;

		/* Extract spells */
		for (i = 0; i < 64; i++)
		{
			/* Check spells in the book */
			if ((i < 32) ?
			    (spell_flags[p_ptr->realm-1][sval][0] & (1L << i)) :
			    (spell_flags[p_ptr->realm-1][sval][1] & (1L << (i - 32))))
			{
				/* Skip non "okay" prayers */
				if (!spell_okay(i, FALSE)) continue;
				/* Hack -- Prepare the randomizer */
				k++;
				/* Hack -- Apply the randomizer */
				if (rand_int(k) == 0) j = i;
			}
		}
	}

	/* Other realms -- Learn a selected spell */
	else
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&j, "study", sval, FALSE) && (j == -1)) return;
	}

	/* Nothing to study */
	if (j < 0)
	{
		/* Message */
		msg_format("You cannot learn any %ss in that book.", p);

		/* Abort */
		return;
	}

	/* Take a turn */
	energy_use = 100;

	/* Learn the spell */
	if (j < 32)
	{
		spell_learned1 |= (1L << j);
	}
	else
	{
		spell_learned2 |= (1L << (j - 32));
	}

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < 64; i++)
	{
		/* Stop at the first empty space */
		if (spell_order[i] == 99) break;
	}

	/* Add the spell to the known list */
	spell_order[i++] = j;

	/* Mention the result */
	msg_format("You have learned the %s of %s.",
	           p, spell_names[p_ptr->realm-1][j]);
	/* One less spell available */
	p_ptr->new_spells--;
	/* Report on remaining prayers */
	if (p_ptr->new_spells)
	{
		msg_format("You can learn %d more %ss.", p_ptr->new_spells, p);
	}
	/* Save the new_spells value */
	p_ptr->old_spells = p_ptr->new_spells;
	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);
}
/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int item, sval, j, dir;
	int chance, beam;
	int plev;
	object_type *o_ptr;

	magic_type *s_ptr;

	/* Must have a realm */
	if (!p_ptr->realm)
	{
		msg_print("You can't cast spells!");
		return;
	}

	/* Require lite */
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	/* Must not be berserk */
	if(p_ptr->shero)
	{
		msg_print("You are too berserk!");
		return;
	}

	/* Must be able to use magic */
	if(p_ptr->nomagic)
	{
		msg_print("Your magic abilities are unusable right now.");
		return;
	}

	/* Can't be shapechanged. */
	if(p_ptr->schange)
	  {
	    msg_print("You cannot cast spells while shapechanging.");
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
	/* Ask for a spell */
	if (!get_spell(&j, "cast", sval, TRUE))
	{
		if (j == -2) msg_print("You don't know any spells in that book.");
		return;
	}

	/* Access the spell */
	s_ptr = &mp_ptr->info[j];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana.");

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
	chance = spell_chance(j);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print("You failed to get the spell off!");
	}
	/* Process spell */
	else
	{
		/* Get player level */
		plev= smod(S_MAGIC) + (p_ptr->cur_skill[mp_ptr->spell_skill]/20)-5;
		/* Hack -- chance of "beam" instead of "bolt" */
		beam = (p_ptr->cur_skill[mp_ptr->spell_skill]-52)/4;

		/* Spells.  */
		switch (j + (p_ptr->realm -1)*64)
		{
			/*** Mage spells ***/
			case 0:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir, damroll(3 + ((plev-1)/5), 4));
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
				(void)detect_objects_normal();
				(void)detect_treasure();
				(void)detect_objects_gold();
				break;
			}

			case 5:
			{
				(void)detect_traps();
				(void)detect_doors();
				break;
			}

			case 6:
			{
				if (!get_aim_dir(&dir)) return;
				fetch(dir, plev*15);
				break;
			}

			case 7:
			{
				do_cmd_spike(1);
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
				(void)fear_monster(dir, plev);
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
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir, 10+plev/4, 2);
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
				teleport_player(plev * 5);
				break;
			}
			case 15:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
					damroll(4+((plev-5)/45), 6));
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
				(void)sleep_monsters();
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
				if (!get_aim_dir(&dir)) return;
				(void)fire_ball(GF_ELEC, dir, 30+plev/4, 2);
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
				msg_print("You feel your head clear a bit.");
				restore_mana(35);
				break;
			}

			case 33:
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				break;
			}

			case 34:
			{
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				break;
			}

			case 35:
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				break;
			}

			case 36:
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}
			case 37:
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}

			case 38:
			{
				(void)door_creation();
				break;
			}

			case 39:
			{
				(void)stair_creation(FALSE);
				break;
			}

			case 40:
			{
				recall_player();
				break;
			}

 			case 41:
			{
				(void)detect_general(0, RF3_EVIL, "evil");
				break;
			}

			case 42:
			{
				(void)detect_objects_magic();
				break;
			}

			case 43:
			{
				recharge(100);
				break;
			}

			case 44:
			{
				(void)genocide();
				break;
			}

			case 45:
			{
				(void)mass_genocide();
				break;
			}
			case 46:
			{
				(void)hp_player(10);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case 47:
			{
				(void)set_shield(p_ptr->shield + randint(20) + 30);
				break;
			}

			case 48:
			{
				(void)hp_player(30);
				(void)set_shero(p_ptr->shero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case 49:
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

			case 50:
			{
				(void)set_invuln(p_ptr->invuln + randint(8) + 8);
				break;
			}

			case 51:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_SOUND, dir, 150+(plev*3)/2, 2);
				break;
			}

			case 52:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir,
				          220 + (plev * 3) / 2, 3);
				break;
			}

			case 53:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_PLASMA, dir,
				          240 + (plev * 3) / 2, 2);
				break;
			}

			case 54:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir,
				          260+(plev*3)/2, 3);
				break;
			}

			case 55:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_CHAOS, dir,
				          180 + (plev*3)/2, 3);
				break;
			}

			case 56:
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_ELEC, dir, 180+(plev*3)/2);
				break;
			}

			case 57:
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_MISSILE, dir, (plev*3)/2+190);
				break;
			}

			case 58:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLY_ORB, dir, 300, 3);
				break;
			}

			/*** Priest spells ***/
			case 64:
			{
				(void)detect_general(0, RF3_EVIL, "evil");
				break;
			}

			case 65:
			{
				(void)hp_player(damroll(3+plev/15, 3+plev/8));
				(void)set_cut(p_ptr->cut - 10);
				break;
			}

			case 66:
			{
				(void)set_blessed(p_ptr->blessed + randint(12) + 12);
				break;
			}

			case 67:
			{
				(void)set_afraid(0);
				break;
			}

			case 68:
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 69:
			{
				(void)detect_traps();
				(void)detect_doors();
				break;
			}

			case 70:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_HOLY_ORB, dir,
							damroll(3,5));
				break;
			}

			case 71:
			{
				(void)set_poisoned(0);
				break;
			}

			case 72:
			{
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir);
				break;
			}

			case 73:
			{
				teleport_player(plev * 3);
				break;
			}

			case 74:
			{
				(void)hp_player(damroll(4+plev/5, 5+plev/8));
				(void)set_cut((p_ptr->cut / 2) - 20);
				break;
			}

			case 75:
			{
				(void)set_blessed(p_ptr->blessed + randint(24) + 24);
				break;
			}

			case 76:
			{
				banishment(RF3_ANIMAL, plev*2);
				break;
			}

			case 77:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 78:
			{
				(void)remove_curse();
				break;
			}

			case 79:
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
				break;
			}

			case 80:
			{
				recall_player();
				break;
			}
			case 81:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLY_ORB, dir,
				          damroll(4, 4)+(plev*3)/2, 2);
				break;
			}

			case 82:
			{
				(void)hp_player(damroll(5+plev/5, 5+plev/6));
				(void)set_cut(0);
				break;
			}

			case 83:
			{
				if (p_ptr->tim_invis>0)
					set_tim_invis(p_ptr->tim_invis+5+randint(5));
					else set_tim_invis(15+plev +randint(24));
				(void)set_detect_inv(p_ptr->detect_inv + randint(24)+15+plev);
				break;
			}

			case 84:
			{
				(void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
				break;
			}

			case 85:
			{
				earthquake(py, px, 10);
				break;
			}

			case 86:
			{
				(void)map_area();
				break;
			}

			case 87:
			{
				(void)hp_player(damroll(10+plev/9, 4+plev/8));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}
			case 88:
			{
				(void)turn_undead();
				break;
			}

			case 89:
			{
				banishment(RF3_EVIL, plev);
				break;
			}

			case 90:
			{
				(void)dispel_undead(plev*3);
				break;
			}

			case 91:
			{
				(void)hp_player(200 + plev*3);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 92:
			{
				(void)dispel_evil(plev*3);
				break;
			}

			case 93:
			{
				warding_glyph();
				break;
			}

			case 94:
			{
 				(void)dispel_evil(plev*4);
				(void)hp_player(2000);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 95:
			{
				teleport_player(10);
				break;
			}

			case 96:
			{
				teleport_player(plev * 8);
				break;
			}

			case 97:
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 98:
			{
				(void)teleport_player_level();
				break;
			}

			case 99:
			{
				(void)set_ironwill((p_ptr->ironwill >0)? 300+plev*2:
					(p_ptr->ironwill+50+plev/4));
				break;
			}

			case 100:
			{
				msg_print("The world changes.");
				new_level_flag = TRUE;
				break;
			}

			case 101:
			{
				(void)detect_all();
				break;
			}

			case 102:
			{
				(void)ident_spell();
				break;
			}

			case 103:
			{
				(void)probing();
				break;
			}

			case 104:
			{
				wiz_lite();
				break;
			}
			case 105:
			{
				self_knowledge();
				break;
			}
			case 106:
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + 50 + plev +
									randint(plev+10));
				(void)set_oppose_elec(p_ptr->oppose_elec + 50 + plev +
									randint(plev+10));
				(void)set_oppose_fire(p_ptr->oppose_fire + 50 + plev +
									randint(plev+10));
				(void)set_oppose_cold(p_ptr->oppose_cold + 50 + plev +
									randint(plev+10));
				(void)set_oppose_pois(p_ptr->oppose_pois + 50 + plev +
									randint(plev+10));
				break;
			}

			case 107:
			{
				(void)set_shield(p_ptr->shield + 50 + plev/3 + randint(plev));
				msg_print("The essence of your god surrounds you!");
				break;
			}

			case 108:
			{
				(void)restore_stats();
				break;
			}

			case 109:
			{
				(void)restore_level();
				break;
			}

			case 110:
			{
				(void)set_invuln(p_ptr->invuln + randint(10)+plev/5 +2);
				break;
			}

			case 111:
			{
				(void)recharge(15);
				break;
			}

			case 112:
			{
				(void)remove_all_curse();
				break;
			}

			case 113:
			{
				set_blessed(p_ptr->blessed + 20 + plev);
				set_fast(!(p_ptr->fast) ? plev+20: randint(plev/2)+5);
				break;
			}

			case 114:
			{
				(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
				break;
			}

			case 115:
			{
				(void)enchant_spell(0, 0, rand_int(3) + 2);
				break;
			}

			case 116:
			{
				brand_weapon();
				break;
			}

			case 117:
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_SOUND, dir, plev+40);
				break;
			}

			case 118:
			{
				(void)dispel_evil(plev * 4);
				break;
			}

			case 119:
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_HOLY_ORB, dir, (plev*3)/2+150);
				break;
			}

			case 120:
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}
			case 121:
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 300);
				break;
			}
			/*** Druid spells ***/
			case 128:
			{
				(void)detect_general(1, RF3_UNDEAD, "life");
				break;
			}

			case 129:
			{
				predict_weather(10 + plev*2);
				break;
			}

			case 130:
			{
				(void)set_blessed(p_ptr->blessed + plev*2 + 10);
				break;
			}

			case 131:
			{
				(void)set_afraid(0);
				break;
			}

			case 132:
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 133:
			{
				(void)detect_doors();
				(void)detect_traps();
				break;
			}

			case 134:
			{
				(void)hp_player(damroll(3, 4));
				break;
			}

			case 135:
			{
				(void)set_poisoned(0);
				break;
			}

			case 136:
			{
				recall_player();
				break;
			}

			case 137:
			{
				(void)sleep_monsters();
				break;
			}

			case 138:
			{
				(void)hp_player(damroll(5, 6) + (plev*2)/3);
				break;
			}

			case 139:
			{
				banishment(RF3_ANIMAL, 20+plev);
				break;
			}

			case 140:
			{
				teleport_player(plev*2 + 10);
				break;
			}

			case 141:
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(plev)
									 + plev + 20);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(plev)
									+ plev + 20);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(plev)
									+ plev + 20);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(plev)
									+ plev + 20);
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(plev)
									+ plev + 20);
								break;
			}

			case 142:
			{
				(void)hp_player(damroll(6, 7) + plev);
				break;
			}

			case 143:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}
			case 144:
			{
				attack(GF_POIS, 20+plev/4, 0);
				break;
			}

			case 145:
			{
			  attack(GF_ELEC,25+plev/4,0);
			  break;
			}

			case 146:
			{
				(void)map_area();
				break;
			}

			case 147:
			{
			  attack(GF_COLD,30+plev/4,0);
			  break;
			}

			case 148:
			{
				(void)hp_player(damroll(10, 10) + (plev*3)/2);
				break;
			}

			case 149:
			{
				attack(GF_FIRE, 35+plev/4, 0);
				break;
			}

			case 150:
			{
				attack(GF_MISSILE, 40+plev/4, 0);
				break;
			}

			case 151:
			{
				attack(GF_COLD, 45+plev/2, 2);
				break;
			}

			case 152:
			{
				attack(GF_FIRE, 50+plev/2, 2);
				break;
			}

			case 153:
			{
				attack(GF_ELEC, 55+plev/2, 2);
				break;
			}

			case 154:
			{
				attack(GF_POIS, 60+plev/2, 2);
				break;
			}

			case 155:
			{
/* 				change(5, -6, 2, 0, 27); */
			  shapechange(SHAPE_SHEEP);
				break;
			}

			case 156:
			{
/* 				change(8, 3, -1, 0, 28); */
			  shapechange(SHAPE_GOAT);
				break;
			}

			case 157:
			{
				(void)hp_player(1000);
				break;
			}

			case 158:
			{
				(void)set_fast(p_ptr->fast + plev + randint(plev + 50));
				break;
			}

			case 159:
			{
				restore_mana(50);
				break;
			}

			case 160:
			{
/* 				change(10, -3, 10, 0, 32); */
			  shapechange(SHAPE_BEAR);
				break;
			}

			case 161:
			{
/* 				change(15, -1, 3, 0, 33); */
			  shapechange(SHAPE_LION);
				break;
			}

			case 162:
			{
/* 				change(5, 1, -4, 1, 34); */
			  shapechange(SHAPE_GAZELLE);
				break;
			}

			case 163:
			{
/* 				change(35, 4, -1, 10, 35); */
			  shapechange(SHAPE_CHEETAH);
				break;
			}

			case 164:
			{
/* 				change(20, 2, 5, 10, 36); */
			  shapechange(SHAPE_DRAGON);
				break;
			}

			case 165:
			{
				(void)ident_spell();
				break;
			}

			case 166:
			{
				(void)detect_objects_normal();
				(void)detect_treasure();
				(void)detect_objects_gold();
				(void)detect_objects_magic();
				break;
			}

			case 167:
			{
				(void)probing();
				break;
			}

			case 168:
			{
				(void)stair_creation(FALSE);
				break;
			}
			case 169:
			{
				(void)wiz_lite();
				break;
			}

			case 170:
			{
				(void)set_blessed(p_ptr->blessed + 60 + plev);
				break;
			}
			case 171:
			{
				(void)set_ironwill(p_ptr->ironwill=0?
					60 + plev : p_ptr->ironwill+plev+20);
				break;
			}

			case 172:
			{
				(void)set_shero(p_ptr->shero + plev + 50);
				break;
			}

			case 173:
			{
				set_shield(p_ptr->shield + plev + randint(plev) + 10);
				msg_print("A mystic shield forms around your body!");
				break;
			}

			case 174:
			{
				(void)mass_sleep();
				break;
			}

			case 175:
			{
				attack(GF_FIRE, (plev*3)/2+200, 3);
				break;
			}

			case 176:
			{
				attack(GF_COLD, (plev*3)/2+210, 3);
				break;
			}

			case 177:
			{
				attack(GF_POIS, 220+(plev*3)/2, 3);
				break;
			}

			case 178:
			{
				attack(GF_SOUND, (plev*3)/2+230, 3);
				break;
			}

			case 179:
			{
				attack(GF_ELEC, (plev*3)/2+240, 3);
				break;
			}
			case 180:
			{
				p_ptr->weather|=W_DRY;
				p_ptr->weather&=~W_MOIST;
				msg_print("The air seems dry.");
				break;
			}

			case 181:
			{
				p_ptr->weather|=W_MOIST;
				p_ptr->weather&=~W_DRY;
				msg_print("The air seems moist.");
				break;
			}

			case 182:
			{
				p_ptr->weather|=W_WINDY;
				p_ptr->weather&=~W_STILL;
				msg_print("You feel the wind pick up.");
				break;
			}

			case 183:
			{
				p_ptr->weather|=W_STILL;
				p_ptr->weather&=~W_WINDY;
				msg_print("You feel the wind die down.");
				break;
			}

			case 184:
			{
				p_ptr->weather|=W_COOL;
				p_ptr->weather&=~W_WARM;
				msg_print("You feel the air grow chilly.");
				break;
			}

			case 185:
			{
				p_ptr->weather|=W_WARM;
				p_ptr->weather&=~W_COOL;
				msg_print("You feel uncomfortably warm.");
				break;
			}

			/*** Necro spells ***/
			case 192:
			{
				(void)detect_general(0, RF3_UNDEAD, "undead");
				break;
			}
			case 193:
			{
				(void)teleport_player(20);
				break;
			}

			case 194:
			{
				(void)remove_curse();
				break;
			}

			case 195:
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 196:
			{
				if (!get_aim_dir(&dir)) return;
				(void)confuse_monster(dir, plev);
				break;
			}

			case 197:
			{
				(void)detect_doors();
				(void)detect_traps();
				break;
			}

			case 198:
			{
				(void)set_poisoned(p_ptr->poisoned /2);
				break;
			}

			case 199:
			{
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir);
				break;
			}

			case 200:
			{
				(void)dispel_undead(plev*4);
				break;
			}

			case 201:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 202:
			{
				(void)set_oppose_cold(p_ptr->oppose_cold>0?
									p_ptr->oppose_cold+10: 30+plev);
				(void)set_oppose_fire(p_ptr->oppose_fire>0?
									p_ptr->oppose_fire+10: 30+plev);
				break;
			}

			case 203:
			{
				(void)slow_undead();
				break;
			}

			case 204:
			{
				(void)set_ironwill(p_ptr->ironwill>0? p_ptr->ironwill+plev+5:
							plev*2+30);
				break;
			}

			case 205:
			{
				teleport_player(plev*4);
				break;
			}

			case 206:
			{
				recall_player();
				break;
			}

			case 207:
			{
				detect_general(1, RF3_UNDEAD, "life");
				break;
			}

			case 208:
			{
				set_tim_infra(p_ptr->tim_infra + 50 + plev*2);
				(void)set_detect_inv(p_ptr->detect_inv + plev*2+50);
				break;
			}

			case 209:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_SPIRIT, dir, 20+plev/3, 2);
				break;
			}

			case 210:
			{
				(void)hp_player((plev*3)/2+50);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 211:
			{
				set_protevil(p_ptr->protevil>0?
							p_ptr->protevil+plev/4+10: plev*2+50);
				break;
			}

			case 212:
			{
				(void)ident_spell();
				break;
			}

			case 213:
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, plev/3 +35);
				break;
			}

			case 214:
			{
				(void)slow_monsters();
				break;
			}
			case 215:
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}
			case 216:
			{
				o_ptr = &inventory[INVEN_WIELD];
				if (!o_ptr->k_idx) break;
				if (o_ptr->name1 || o_ptr->name2)
				{
					msg_print("Your weapon resists the spell.");
					break;
				}
 				if (o_ptr->flags3 & TR3_CURSED)
				{
					msg_print("The weapon has already been cursed.");
					break;
				}
  				o_ptr->flags3 |= TR3_CURSED;
				o_ptr->ident |= (IDENT_CURSED);
				enchant(o_ptr, 2, ENCH_TOHIT);
				enchant(o_ptr, 2, ENCH_TODAM);

				msg_print("Your weapon feels deathly cold!");
				break;
			}

			case 217:
			{
				set_shero(p_ptr->shero>0? p_ptr->shero+10: plev+30);
				break;
			}

			case 218:
			{
				set_oppose_pois(p_ptr->oppose_pois>0?
							p_ptr->oppose_pois+20: plev+50);
				break;
			}

			case 219:
			{
				set_shield(p_ptr->shield + randint(20) +30);
				break;
			}

			case 220:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_SPIRIT, dir, 80+(plev*3)/2, 2);
				break;
			}
			case 221:
			{
				o_ptr=&inventory[INVEN_WIELD];
				if (!o_ptr->k_idx) break;
				if (o_ptr->name1 || o_ptr->name2)
				{
					msg_print("Your weapon resists the spell.");
					break;
				}

				msg_print("A deep purple light surrounds your weapon.");
				o_ptr->name2 = EGO_SLAY_UNDEAD;
				o_ptr->to_h += 1+randint(plev/8);
				o_ptr->to_d += 1+randint(plev/8);
				o_ptr->flags1 |= TR1_SLAY_UNDEAD;
				o_ptr->flags2 |= TR2_HOLD_LIFE;
				o_ptr->flags3 |= TR3_LITE | TR3_SEE_INVIS;
				break;
			}

			case 222:
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case 223:
			{
				banishment(RF3_UNDEAD|RF3_DEMON, plev*2);
				break;
			}

			case 224:
			{
				(void)dispel_undead(plev*5);
				break;
			}

			case 225:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_SPIRIT, dir, (plev*3)/2+220, 3);
				break;
			}

			case 226:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLY_ORB, dir, plev+150, 3);
				break;
			}

			case 227:
			{
				(void)do_res_stat(rand_int(6));
				break;
			}

			case 228:
			{
				(void)restore_level();
				break;
			}

			case 229:
			{
				(void)restore_stats();
				break;
			}
			case 230:
			{
				if(p_ptr->exp == p_ptr->max_exp) break;
				p_ptr->exp = p_ptr->max_exp;
				msg_print("You feel your life force return.");
				p_ptr->redraw |= PR_EXP;
				break;
			}

			case 231:
			{
				restore_mana(35);
				break;
			}

			case 232:
			{
				(void)dispel_animal(plev*5);
				break;
			}

			case 233:
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, (plev*3)/2+120);
				break;
			}

			case 234:
			{
				(void)genocide();
				break;
			}

			case 235:
			{
				(void)mass_genocide();
				break;
			}

			case 236:
			{
				(void)hp_player(plev*2 +200);
				break;
			}
			case 237:
			{
				set_shero(p_ptr->shero>0? p_ptr->shero+plev+10:
							plev+50);
				set_fast(p_ptr->fast>0? p_ptr->fast+plev+10: plev+50);
				break;
			}

			case 238:
			{
				if(p_ptr->oppose_fire) break;
				(void)set_tim_invis(plev*2 +50);
				(void)set_oppose_fire(plev*2 +50);
				(void)set_oppose_cold(plev*2 +50);
				(void)set_oppose_acid(plev*2 +50);
				(void)set_oppose_pois(plev*2 +50);
				(void)set_ironwill(plev*2 +50);
				(void)set_fast(plev*2 +50);
				break;
			}

			case 239:
			{
				set_invuln(p_ptr->invuln>0? p_ptr->invuln+randint(5)+5:
							randint(plev/5)+10);
				break;
			}

			case 240:
			{
				(void)detect_all();
				break;
			}

			case 241:
			{
				(void)probing();
				break;
			}

			case 242:
			{
				(void)map_area();
				break;
			}

			case 243:
			{
				(void)wiz_lite();
				break;
			}
			default:
			{
				msg_print("You have successfully cast a bugged spell!");
				msg_print("Better tell someone, right now!");
			}
		}
		/* A spell was cast */
		if (!((j < 32) ?
		      (spell_worked1 & (1L << j)) :
		      (spell_worked2 & (1L << (j - 32)))))
		{
			int e = s_ptr->sexp;
			/* The spell worked */
			if (j < 32)
			{
				spell_worked1 |= (1L << j);
			}
			else
			{
				spell_worked2 |= (1L << (j - 32));
			}

			/* Gain experience */
			gain_exp(e << 2);
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

/* Stop doing a druid shapechange */
void do_cmd_t_stop()
{
	if(!(p_ptr->schange))
	{
		msg_print("You aren't in another form right now.");
		return;
	}
	/* Confirm */
	if(!get_check("Really return to normal? ")) return;

	shapechange(SHAPE_NORMAL);
	energy_use = 50;
/* 	p_ptr->schange = 0; */
/* 	p_ptr->ac_mod = 0; */
/* 	p_ptr->to_h_mod = 0; */
/* 	p_ptr->to_d_mod = 0; */
/* 	p_ptr->pspeed_mod=0; */
/* 	p_ptr->update |= PU_BONUS; */
}
