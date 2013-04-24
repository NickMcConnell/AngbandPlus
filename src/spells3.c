/* File: spells3.c */

/* Purpose: Spell code (part 3) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Turns an object into gold, gain some of its value in a shop
 */
bool alchemy(void)
{
	int item, amt = 1;
	int old_number;
	long price;
	bool force = FALSE;
	object_type *o_ptr;
	char o_name[256];
	char out_val[512];

	cptr q, s;

	/* Hack -- force destruction */
	if (p_ptr->command_arg > 0) force = TRUE;

	/* Get an item */
	q = "Turn which item to gold? ";
	s = "You have nothing to turn to gold.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

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


	/* See how many items */
	if (o_ptr->number > 1)
	{
		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number);

		/* Allow user abort */
		if (amt <= 0) return FALSE;
	}


	/* Describe the object */
	old_number = o_ptr->number;
	o_ptr->number = amt;
	object_desc(o_name, o_ptr, TRUE, 3);
	o_ptr->number = old_number;

	/* Verify unless quantity given */
	if (!force)
	{
		if (object_value(o_ptr) < 1)
		{
			/* Make a verification */
			sprintf(out_val, "Really turn %s to gold? ", o_name);
			if (!get_check(out_val)) return FALSE;
		}
	}
	
	/* Artifacts cannot be converted to gold */
	if (artifact_p(o_ptr))
	{
		/* Message */
		msg_format("You cannot turn %s into gold!", o_name);

		/* Don't mark id'ed objects */
		if (object_known_p(o_ptr)) return (FALSE);

		/* It has already been sensed */
		if (o_ptr->ident & (IDENT_SENSE))
		{
			/* Already sensed objects always get improved feelings */
			if (cursed_p(o_ptr) || broken_p(o_ptr))
				o_ptr->discount = INSCRIP_TERRIBLE;
			else
				o_ptr->discount = INSCRIP_SPECIAL;
		}
		else
		{
			/* Mark the object as indestructible */
			o_ptr->discount = INSCRIP_INDESTRUCTIBLE;
		}

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return (FALSE);
	}

	price = object_value(o_ptr);

	if (price <= 0)
	{
		/* Message */
		msg_format("You turn %s to fool's gold.", o_name);
	}
	else
	{
		price /= 3;

		if (amt > 1) price *= amt;

		if (price > 30000) price = 30000;
		msg_format("You turn %s to %ld coins worth of gold.", o_name, price);
		p_ptr->au += price;

		/* Redraw gold */
		p_ptr->redraw |= (PR_GOLD);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	}

	/* Eliminate the item (from the pack) */
	if (item >= 0)
	{
		inven_item_increase(item, -amt);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Eliminate the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -amt);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	return TRUE;
}

/* have your wounds change. . . Possibly for the worse */

void do_poly_wounds(void)
{
	s16b wounds = p_ptr->cut;
	s16b hit_p = (p_ptr->mhp - p_ptr->chp);
	s16b change = damroll(p_ptr->depth, 5);
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

/*
 * Fetch an item (teleport it right underneath the caster)
 * I'll have to figure this out later, at least now it's
 * not 'undeclared' Fixed. This cave info stuff sucks!
 * I can't figure out why it's different in v from _all_
 * the other variants -ccc
 */
void fetch(int dir, int wgt, bool require_los)
{
	int             ty, tx, i;
 	bool            flag;
 	object_type     *o_ptr;
 	char            o_name[80];
 	int py = 			p_ptr->py;
 	int px = 			p_ptr->px;
 
	/* Check to see if you're on an empty floor square */
	if(!cave_clean_bold(py, px))
	{
		msg_print("You need an empty space on the floor to fetch something.");
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

		if (cave_o_idx[ty][tx] == 0)
		{
			msg_print("There's nothing there to fetch!");
			return;
		}

		if (require_los && (!player_has_los_bold(ty,tx)))
		{
			msg_print("You have no direct line of sight to that location.");
			return;
		}

		if (cave_info[ty][tx] & (CAVE_ICKY))
		{
			msg_print("The object refuses to budge!");
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
		}
		while(cave_o_idx[py][px] == 0);
	}

	o_ptr = &o_list[(cave_o_idx[ty][tx])];

	if (o_ptr->weight > wgt)
	{
		/* Too heavy to 'fetch' */
		msg_print("The object is too heavy.");
		return;
	}

	i = cave_o_idx[ty][tx];
	cave_o_idx[ty][tx] = 0;
	cave_o_idx[py][px] = i; /* 'move' it */
	o_ptr->iy = py;
	o_ptr->ix = px;

	object_desc(o_name, o_ptr, TRUE, 0);
	msg_format("%^s appears at your feet.", o_name);

	note_spot(py,px);
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	p_ptr->window |= (PW_OVERHEAD);

}

void do_poly_self(void)
{
	int effects = randint(2);
	int tmp = 0;
	int more_effects = TRUE;

	msg_print("You feel a change coming over you...");

	while (effects-- && more_effects)
	{
		switch (randint(15))
		{
			case 1: case 2:
				/* GRRRRRR. No Idea why I can't decleare this 'extern'
				 *  from wizard.c  
				 *  heheheh. Guess I should know better than declare a
				 *  static as a extern   */
				do_cmd_rerate();
				break;
			case 3: /* Lose all mutations -- Gumby */
				if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3 ||
					p_ptr->muta4 || p_ptr->muta5 || p_ptr->muta6)
				{
					msg_print("All of your lovely mutations go away!");
					p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
					p_ptr->muta4 = p_ptr->muta5 = p_ptr->muta6 = 0;
					p_ptr->update |= PU_BONUS;
					handle_stuff();
				}
				else
				{
					msg_print("You feel as if you almost lost something...");
				}
				break;
			case 4: case 5:
				do_poly_wounds();
				break;
			case 6: case 7: case 8:
				(void) gain_random_mutation(0);
				break;
			case 9: case 10: case 11:
				lose_mutation(0);
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

void mutate_player(void)
{
	int max1, cur1, max2, cur2, ii, jj;

	/* Pick a pair of stats */
	ii = rand_int(6);
	for (jj = ii; jj == ii; jj = rand_int(6)) /* loop */;

	max1 = p_ptr->stat_max[ii];
	cur1 = p_ptr->stat_cur[ii];
	max2 = p_ptr->stat_max[jj];
	cur2 = p_ptr->stat_cur[jj];

	p_ptr->stat_max[ii] = max2;
	p_ptr->stat_cur[ii] = cur2;
	p_ptr->stat_max[jj] = max1;
	p_ptr->stat_cur[jj] = cur1;

	p_ptr->update |= (PU_BONUS);
}

void do_cmd_rerate(void)
{
	int min_value, max_value, i, percent;

	min_value = (PY_MAX_LEVEL * 3 * (p_ptr->hitdie - 1)) / 8;
	min_value += PY_MAX_LEVEL;

	max_value = (PY_MAX_LEVEL * 5 * (p_ptr->hitdie - 1)) / 8;
	max_value += PY_MAX_LEVEL;

	p_ptr->player_hp[0] = p_ptr->hitdie;

	/* Rerate */
	while (1)
	{
		/* Collect values */
		for (i = 1; i < PY_MAX_LEVEL; i++)
		{
			p_ptr->player_hp[i] = randint(p_ptr->hitdie);
			p_ptr->player_hp[i] += p_ptr->player_hp[i - 1];
		}

		/* Legal values */
		if ((p_ptr->player_hp[PY_MAX_LEVEL - 1] >= min_value) &&
		    (p_ptr->player_hp[PY_MAX_LEVEL - 1] <= max_value)) break;
	}

	percent = (int)(((long)p_ptr->player_hp[PY_MAX_LEVEL - 1] * 200L) /
	                (p_ptr->hitdie + ((PY_MAX_LEVEL - 1) * p_ptr->hitdie)));

	/* Update and redraw hitpoints */
	p_ptr->update |= (PU_HP);
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	/* Handle stuff */
	handle_stuff();

	/* Message */
	msg_format("You feel your life force changing!");
}

