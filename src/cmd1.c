/* File: cmd1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "animeband.h"

/* Silly Global Variable */
bool launch;


/*
 * Determine if the player "hits" a monster (normal combat).
 *
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit_fire(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Instant miss or hit */
	if (k < 10) return (k < 5);

	/* Invisible monsters are harder to hit */
	if (!vis) chance = chance / 2;

	/* Power competes against armor */
	if ((chance > 0) && (rand_int(chance) >= (ac * 3 / 4))) return (TRUE);

	/* Assume miss */
	return (FALSE);
}



/*
 * Determine if the player "hits" a monster (normal combat).
 *
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit_norm(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Instant miss or hit */
	if (k < 10) return (k < 5);

	/* Penalize invisible targets */
	if (!vis) chance = chance / 2;

	/* Power competes against armor */
	if ((chance > 0) && (rand_int(chance) >= (ac * 3 / 4))) return (TRUE);

	/* Assume miss */
	return (FALSE);
}



/*
 * Critical hits (from objects thrown by player)
 * Factor in item weight, total plusses, and player level.
 */
sint critical_shot(int weight, int plus, int dam)
{
	int i, k;

	int choosenumber;

	/* Groove Specific Bounses */
	if (p_ptr->pgroove == G_DRUNK){
		choosenumber = 5000 - 1250 * (p_ptr->c_meter * 3 / p_ptr->m_meter);
	}

	else if (p_ptr->pgroove == G_FIRE){

		choosenumber = 5000 - 1750 * (p_ptr->c_meter  * 2 / p_ptr->m_meter);
	}

	else
		choosenumber = 5000;

	/* Extract "shot" power */
	i = (weight + ((p_ptr->to_h + plus) * 4) + (p_ptr->lev * 2));

	/* Critical hit */
	if (randint(choosenumber) <= i)
	{
		k = weight + randint(500);

		if (k < 500)
		{
			msg_print("It was a good hit!");
			dam = 2 * dam + 5;
		}
		else if (k < 1000)
		{
			msg_print("It was a great hit!");
			dam = 2 * dam + 10;
		}
		else
		{
			msg_print("It was a superb hit!");
			dam = 3 * dam + 15;
		}
	}

	return (dam);
}



/*
 * Critical hits (by player)
 *
 * Factor in weapon weight, total plusses, player level.
 */
sint critical_norm(int weight, int plus, int dam)
{
	int i, k;
	int choosenumber;
	
	if (p_ptr->pgroove == G_DRUNK){
		choosenumber = 5000 - 1250 * (p_ptr->c_meter * 3 / p_ptr->m_meter);
	}

	else if (p_ptr->pgroove == G_FIRE){

		choosenumber = 5000 - 1250 * (p_ptr->c_meter * 2 / p_ptr->m_meter);
	}

	else
		choosenumber = 5000;

	/* Extract "blow" power */
	i = (weight + ((p_ptr->to_h + plus) * 5) + (p_ptr->lev * 3));

	/* Chance */
	if (randint(choosenumber) <= i)
	{
		k = weight + randint(650);
		launch = TRUE;
		

		if (k < 400)
		{
			msg_print("It was a good hit!");
			dam = 2 * dam + 5;
		}
		else if (k < 700)
		{
			msg_print("It was a great hit!");
			dam = 2 * dam + 10;
		}
		else if (k < 900)
		{
			msg_print("It was a superb hit!");
			dam = 3 * dam + 15;
		}
		else if (k < 1300)
		{
			msg_print("It was a *GREAT* hit!");
			dam = 3 * dam + 20;
		}
		else
		{
			msg_print("It was a *SUPERB* hit!");
			dam = ((7 * dam) / 2) + 25;
		}
		
	}

	return (dam);
}



/*
 * Extract the "total damage" from a given object hitting a given monster.
 *
 * Note that "flasks of oil" do NOT do fire damage, although they
 * certainly could be made to do so.  XXX XXX
 *
 * Note that most brands and slays are x3, except Slay Animal (x2),
 * Slay Evil (x2), and Kill dragon (x5).
 */
sint tot_dam_aux(const object_type *o_ptr, int tdam, const monster_type *m_ptr)
{
	int mult = 1;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Some "weapons" and "ammo" do extra damage */
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		case TV_SHURIKEN:
		{
			/* Slay Animal */
			if ((f1 & (TR1_SLAY_ANIMAL)) &&
			    (r_ptr->flags3 & (RF3_ANIMAL)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_ANIMAL);
				}

				if (mult < 2) mult = 2;
			}

			/* Slay Evil */
			if ((f1 & (TR1_SLAY_EVIL)) &&
			    (r_ptr->flags3 & (RF3_EVIL)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_EVIL);
				}

				if (mult < 2) mult = 2;
			}

			if ((p_ptr->banish_evil) &&
			    (r_ptr->flags3 & (RF3_EVIL)))
			{
				msg_print("You use your warding scroll.");

				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_EVIL);
				}

				if (mult < 5) mult = 5;
				p_ptr->banish_evil = FALSE;
				
			}

			/* Slay Undead */
			if ((f1 & (TR1_SLAY_UNDEAD)) &&
			    (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_UNDEAD);
				}

				if (mult < 3) mult = 3;
			}

			/* Slay Demon */
			if ((f1 & (TR1_SLAY_DEMON)) &&
			    (r_ptr->flags3 & (RF3_DEMON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_DEMON);
				}

				if (mult < 3) mult = 3;
			}

			/* Slay Sentai */
			if ((f1 & (TR1_SLAY_SENTAI)) &&
			    (r_ptr->flags3 & (RF3_SENTAI)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_SENTAI);
				}

				if (mult < 3) mult = 3;
			}

			/* Slay Mecha */
			if ((f1 & (TR1_SLAY_MECHA)) &&
			    (r_ptr->flags3 & (RF3_MECHA)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_MECHA);
				}

				if (mult < 3) mult = 3;
			}


			/* Slay Troll */
			if ((f1 & (TR1_SLAY_TROLL)) &&
			    (r_ptr->flags3 & (RF3_TROLL)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_TROLL);
				}

				if (mult < 3) mult = 3;
			}

			/* Slay Giant */
			if ((f1 & (TR1_SLAY_GIANT)) &&
			    (r_ptr->flags3 & (RF3_GIANT)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_GIANT);
				}

				if (mult < 3) mult = 3;
			}

			/* Slay Dragon */
			if ((f1 & (TR1_SLAY_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_DRAGON);
				}

				if (mult < 3) mult = 3;
			}

			/* Execute Dragon */
			if ((f1 & (TR1_KILL_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_DRAGON);
				}

				if (mult < 5) mult = 5;
			}


			/* Brand (Acid) */
			if (f1 & (TR1_BRAND_ACID))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ACID))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags3 |= (RF3_IM_ACID);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3) mult = 3;
				}
			}

			/* Brand (Elec) */
			if (f1 & (TR1_BRAND_ELEC))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ELEC))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags3 |= (RF3_IM_ELEC);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3) mult = 3;
				}
			}

			/* Brand (Fire) */
			if (f1 & (TR1_BRAND_FIRE))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_FIRE))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags3 |= (RF3_IM_FIRE);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3) mult = 3;
				}
			}

			/* Brand (Cold) */
			if (f1 & (TR1_BRAND_COLD))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_COLD))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags3 |= (RF3_IM_COLD);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3) mult = 3;
				}
			}

			/* Brand (Poison) */
			if (f1 & (TR1_BRAND_POIS))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_POIS))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags3 |= (RF3_IM_POIS);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3) mult = 3;
				}
			}

			break;
		}
	}


	/* Return the total damage */
	return (tdam * mult);
}


/*
 * Search for hidden things
 */
void search(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, chance;

	s16b this_o_idx, next_o_idx = 0;


	/* Start with base search ability */
	chance = p_ptr->skill_srh;

	/* Penalize various conditions */
	if (p_ptr->blind || no_lite()) chance = chance / 10;
	if (p_ptr->confused || p_ptr->image) chance = chance / 10;

	/* Search the nearby grids, which are always in bounds */
	for (y = (py - 1); y <= (py + 1); y++)
	{
		for (x = (px - 1); x <= (px + 1); x++)
		{
			/* Sometimes, notice things */
			if (rand_int(100) < chance)
			{
				/* Invisible trap */
				if (cave_feat[y][x] == FEAT_INVIS)
				{
					/* Pick a trap */
					pick_trap(y, x);

					/* Message */
					msg_print("You have found a trap.");

					/* Disturb */
					disturb(0, 0);
				}

				/* Secret door */
				if (cave_feat[y][x] == FEAT_SECRET)
				{
					/* Message */
					msg_print("You have found a secret door.");

					/* Pick a door */
					place_closed_door(y, x);

					/* Disturb */
					disturb(0, 0);
				}

				/* Scan all objects in the grid */
				for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
				{
					object_type *o_ptr;

					/* Get the object */
					o_ptr = &o_list[this_o_idx];

					/* Get the next object */
					next_o_idx = o_ptr->next_o_idx;

					/* Skip non-chests */
					if (o_ptr->tval != TV_CHEST) continue;

					/* Skip non-trapped chests */
					if (!chest_traps[o_ptr->pval]) continue;

					/* Identify once */
					if (!object_known_p(o_ptr))
					{
						/* Message */
						msg_print("You have discovered a trap on the chest!");

						/* Know the trap */
						object_known(o_ptr);

						/* Notice it */
						disturb(0, 0);
					}
				}
			}
		}
	}
}


/*
 * Determine if the object can be picked up, and has "=g" in its inscription.
 */
static bool auto_pickup_okay(const object_type *o_ptr)
{
	cptr s;

	/* It can't be carried */
	if (!inven_carry_okay(o_ptr)) return (FALSE);

	/* No inscription */
	if (!o_ptr->note) return (FALSE);

	/* Find a '=' */
	s = strchr(quark_str(o_ptr->note), '=');

	/* Process inscription */
	while (s)
	{
		/* Auto-pickup on "=g" */
		if (s[1] == 'g') return (TRUE);

		/* Find another '=' */
		s = strchr(s + 1, '=');
	}

	/* Don't auto pickup */
	return (FALSE);
}


/*
 * Helper routine for py_pickup() and py_pickup_floor().
 *
 * Add the given dungeon object to the character's inventory.
 *
 * Delete the object afterwards.
 */
static void py_pickup_aux(int o_idx)
{
	int slot;

	char o_name[80];
	object_type *o_ptr;

	o_ptr = &o_list[o_idx];

	/* Carry the object */
	slot = inven_carry(o_ptr);

	/* Get the object again */
	o_ptr = &inventory[slot];

	/* Describe the object */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Message */
	msg_format("You have %s (%c).", o_name, index_to_label(slot));

	/* Delete the object */
	delete_object_idx(o_idx);
}


/*
 * Make the player carry everything in a grid.
 *
 * If "pickup" is FALSE then only gold will be picked up.
 */
void py_pickup(int pickup)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	s16b this_o_idx, next_o_idx = 0;

	object_type *o_ptr;

	char o_name[80];

#ifdef ALLOW_EASY_FLOOR

	int last_o_idx = 0;

	int can_pickup = 0;
	int not_pickup = 0;

#endif /* ALLOW_EASY_FLOOR */


	/* Scan the pile of objects */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- disturb */
		disturb(0, 0);

		/* Pick up gold */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Message */
			msg_format("You have found %ld gold pieces worth of %s.",
			           (long)o_ptr->pval, o_name);

			/* Collect the gold */
			p_ptr->au += o_ptr->pval;

			/* Redraw gold */
			p_ptr->redraw |= (PR_GOLD);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

			/* Delete the gold */
			delete_object_idx(this_o_idx);

			/* Check the next object */
			continue;
		}

		/* Test for auto-pickup */
		if (auto_pickup_okay(o_ptr))
		{
			/* Pick up the object */
			py_pickup_aux(this_o_idx);

			/* Check the next object */
			continue;
		}

#ifdef ALLOW_EASY_FLOOR

		/* Easy Floor */
		if (easy_floor)
		{
			/* Pickup if possible */
			if (pickup && inven_carry_okay(o_ptr))
			{
				/* Pick up if allowed */
				if (!carry_query_flag)
				{
					/* Pick up the object */
					py_pickup_aux(this_o_idx);
				}

				/* Else count */
				else
				{
					/* Remember */
					last_o_idx = this_o_idx;

					/* Count */
					++can_pickup;
				}
			}

			/* Else count */
			else
			{
				/* Remember */
				last_o_idx = this_o_idx;

				/* Count */
				++not_pickup;
			}

			/* Check the next object */
			continue;
		}

#endif /* ALLOW_EASY_FLOOR */

		/* Describe the object */
		if (!pickup)
		{
			msg_format("You see %s.", o_name);

			/* Check the next object */
			continue;
		}

		/* Note that the pack is too full */
		if (!inven_carry_okay(o_ptr))
		{
			msg_format("You have no room for %s.", o_name);

			/* Check the next object */
			continue;
		}

		/* Query before picking up */
		if (carry_query_flag)
		{
			char out_val[160];
			sprintf(out_val, "Pick up %s? ", o_name);
			if (!get_check(out_val)) continue;
		}

		/* Pick up the object */
		py_pickup_aux(this_o_idx);
	}

#ifdef ALLOW_EASY_FLOOR

	/* Easy floor, objects left */
	if (easy_floor && (can_pickup + not_pickup > 0))
	{
		/* Not picking up */
		if (!pickup)
		{
			/* One object */
			if (not_pickup == 1)
			{
				/* Get the object */
				o_ptr = &o_list[last_o_idx];

				/* Describe the object */
				object_desc(o_name, o_ptr, TRUE, 3);

				/* Message */
				msg_format("You see %s.", o_name);
			}

			/* Multiple objects */
			else
			{
				/* Message */
				msg_format("You see a pile of %d objects.", not_pickup);
			}

			/* Done */
			return;
		}

		/* No room */
		if (!can_pickup)
		{
			/* One object */
			if (not_pickup == 1)
			{
				/* Get the object */
				o_ptr = &o_list[last_o_idx];

				/* Describe the object */
				object_desc(o_name, o_ptr, TRUE, 3);

				/* Message */
				msg_format("You have no room for %s.", o_name);
			}

			/* Multiple objects */
			else
			{
				/* Message */
				msg_print("You have no room for any of the objects on the floor.");
			}

			/* Done */
			return;
		}

		/* Pick up objects */
		while (1)
		{
			cptr q, s;

			int item;

			/* Restrict the choices */
			item_tester_hook = inven_carry_okay;

			/* Get an object*/
			q = "Get which item? ";
			s = NULL;
			if (!get_item(&item, q, s, (USE_FLOOR))) break;

			/* Pick up the object */
			py_pickup_aux(0 - item);
		}
	}

#endif /* ALLOW_EASY_FLOOR */

}



/*
 * Determine if a trap affects the player.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match trap power against player armor.
 */
static int check_hit(int power)
{
	int k, ac;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- 5% hit, 5% miss */
	if (k < 10) return (k < 5);

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Power competes against Armor */
	if ((power > 0) && (randint(power) >= (ac * 3 / 4))) return (TRUE);

	/* Assume miss */
	return (FALSE);
}

/* Resets Hamilton chain */
static void reset_puzzle(void){
	/* Erase crap */

	int xbox, ybox;
		int blah;
			int x = 1;
			int y= 3;
			for (ybox = 0; ybox < 4; ybox++)
			{
			for (xbox = 0; xbox < 5; xbox++){
				for (blah =0; blah < 4; blah++){
				cave_set_feat(y, x+blah, FEAT_FLOOR);
			}
				x+=5;
			}
			y+=5;
			x = 1;

			}

			cave_set_feat(1,4,FEAT_FLOOR);

}

/* Check for Victory */
static bool check_victory(void){
	/* Erase crap */

	int xbox, ybox;

			int x = 1;
			int y= 3;
			for (ybox = 0; ybox < 4; ybox++)
			{
			for (xbox = 0; xbox < 5; xbox++){

				if (cave_feat[y + (ybox*5)][x + (xbox*5)] != FEAT_FUN_TELE)
				{

					/*msg_format("%d of y and %d of x has no Tele.", (y+ybox*5), (x+xbox*5));*/
					return (FALSE);
				}
				
				
			}
			
			}
		return (TRUE);

}

/* Handle player hitting trigger in coin puzzle */
void hit_trigger2(int y, int x)
{

	int ytop = (DUNGEON_HGT - 11) / 2 + 1;
	int xtop = (DUNGEON_WID - 34) / 2 + 1;
	int leftside = 0;
	int rightside = 0;
	int scalex;
	int scaley;
	object_type *o_ptr;
	/*
		cave_set_feat(ytop + 2, xtop + 11, FEAT_TRIGGER);
		cave_set_feat(ytop + 2, xtop + 30, FEAT_TRIGGER);
		*/

	/* Which trigger? */
	if (x < xtop + 14)
	{
		/* Weigh the coins in the scales! */
		for (scalex = xtop + 7; scalex < xtop + 10; scalex++)
		{
			for (scaley = ytop; scaley < ytop + 2; scaley++)
			{

		o_ptr = &o_list[cave_o_idx[scaley][scalex]];

		/* Is this a coin? */
		if ((o_ptr->tval == TV_QUEST_ITEM) && (o_ptr->sval == SV_COIN))
		{
			leftside += o_ptr->pval;
		}

			}
		}


		/* Weigh the coins in the scales! Right side */
		for (scalex = xtop + 13; scalex < xtop + 16; scalex++)
		{
			for (scaley = ytop; scaley < ytop + 2; scaley++)
			{

		o_ptr = &o_list[cave_o_idx[scaley][scalex]];

		/* Is this a coin? */
		if ((o_ptr->tval == TV_QUEST_ITEM) && (o_ptr->sval == SV_COIN))
		{
			rightside += o_ptr->pval;
		}
			}
		}

		/*msg_format("Leftside: %d    Rightside: %d", leftside, rightside);*/


		/* Analyze scale */
		if (leftside > rightside)
		{
			/* Place Tanuki to the left */
			place_monster_aux(ytop+3, xtop+8, TANUKI, FALSE, FALSE);

		}

		else if (rightside > leftside)
		{
			/* Place Tanuki to the right */
			place_monster_aux(ytop+3, xtop+14, TANUKI, FALSE, FALSE);
		}

		else
		{
			/* Place a Flaming Bear in FRONT OF YOU!  MWAHAHAH! */
			place_monster_aux(ytop+3, xtop+11, FLAMING_BEAR, FALSE, FALSE);
		}



	}

	/* Other trigger */
	else {
		o_ptr = &o_list[cave_o_idx[ytop + 4][xtop + 31]];

		/* Is this the counterfeit coin? */
		if ((o_ptr->tval == TV_QUEST_ITEM) &&
			(o_ptr->sval == SV_COIN) &&
			(o_ptr->pval == 2)){

			cave_set_feat(ytop+1, xtop+1, FEAT_MORE);
		}

		else {
			/* FLAMING BEAR IN FRONT OF YOU!*/
			place_monster_aux(ytop+2, xtop+29, FLAMING_BEAR, FALSE, FALSE);
		}
	}



}

/*
 * Handle player hitting a trigger
 */
void hit_trigger(int y, int x)
{
	

	int ytop = (DUNGEON_HGT - 9)/ 2 + 1;
	int xtop = (DUNGEON_WID - 34)/2 - 1;
	int counter = 0;
	int xstar = xtop + 26;
	int puzzle[4] = {0};
	int puzclr[4] = {0};
	int numcorrect = 0;
	int numcolor = 0;
	int checker;

		
	

	/* Find the first available location */

	while ((cave_feat[ytop][xstar+counter] > FEAT_TRIGGER) &&
		   (counter < 4))
	{
		counter++;

	}
	if (counter <= 3)
	{
	
		cave_set_feat(ytop, xstar + counter, cave_feat[y-1][x]);
	
	}


	if (counter > 2){
		/* Count correct color, correct position */
		for (counter = 0; counter < 4; counter++)
		{
			if (cave_feat[1][1+counter] == cave_feat[ytop][xstar+counter])
			{
				numcorrect++;
				puzzle[counter] = 2;
				puzclr[counter] = 2;

			}
		}
		/* If solved, place a stairway */
		if (numcorrect == 4)
		{
			cave_set_feat(ytop, xtop + 2, FEAT_MORE);
			return;

		}

		/* If not solved yet */
		if (numcorrect < 4)
		{
			for (counter = 0; counter < 4; counter++)
			{
				if (puzzle[counter] == 0)
				{
					/* Check for right color, but wrong position */
					for (checker = 0; checker < 4; checker++)
					{
						if (cave_feat[1][1+checker] == cave_feat[ytop][xstar+counter])
						{
							if ((puzzle[counter] == 0) && (puzclr[checker] == 0))
							{
								puzzle[counter] = 1;
								puzclr[checker] = 1;
								numcolor++;
							}
						}


					}

				}
			}
			
		}
		else {
			cave_set_feat(ytop+1, xtop+1, FEAT_MORE);
			return;
		}
	
	/* Reset Puzzle */	
		for (counter = 0; counter <4; counter++)
		{
			cave_set_feat(ytop, xstar+counter, FEAT_FLOOR);
		}
		 /* msg_format("%d right, %d color", numcorrect, numcolor);		*/

		/* Place monsters for info */
		counter = 0;
		if (numcorrect == 0 && numcolor == 0){
		
			/* You didn't get any right...bad bad bad */
			place_monster_aux(ytop, xtop+2, FLAMING_BEAR, FALSE, FALSE);
		
		}

		else {
			for (checker = 0; checker < numcorrect; checker++)
			{
				/* Tanukis for Right place, right color, Kitsunes for Right color, wrong place */
			 place_monster_aux(ytop + counter, xtop + 2, TANUKI, FALSE, FALSE);
			 counter++;

			}

			for (checker = 0; checker < numcolor; checker++)
			{
			 place_monster_aux(ytop + counter, xtop + 2, KITSUNE, FALSE, FALSE);
			 counter++;

			}
		}

		return;
	}
	

	
	
	


}

void hit_train_station(void)
{
	int curStop = 0;

	
		msg_print("Welcome to the train station.");
		/*msg_print("It costs 300 AU to board the train.");*/

		/* Flush input */
		flush();

		/* Verify */
		if (!get_check("Board? ")) return;

		/* 300 should be in define.h.....meh 
		if (p_ptr->au < 300)
		{
			msg_print("You do not have enough money.");
			return;
		}
		*/

		/*else
		{*/
			msg_print("Off we go!");
			while (1)
			{
				msg_format("You arrive at %s.", train_stops[curStop]);

				/* Flush input */
				flush();

				/* Verify */
				if (get_check("Depart? "))
				{
					msg_print("You get off the train....");

				/*	p_ptr->au -= 300;*/

					/* New location */
					p_ptr->location = train_jump[curStop];

					/* Leaving */
					p_ptr->leaving = TRUE;

					return;
				}

				curStop++;

				if (curStop > MAX_TRAIN - 1)
				{
					curStop = 0;

				}

			}

		/*}*/

}


/*
 * Handle player hitting a tele
 */

void hit_tele(int y, int x)
{
	
	int blah;
	/* Disturb the player */
	disturb(0, 0);
	/* Analyze XXX XXX XXX */
	switch (cave_feat[y][x])
	{
		case FEAT_TELE_1:
			{
			
			teleport_player_to(1,1);
			/* You solved it */
			break;
			}

		case FEAT_TELE_2:
			{
			teleport_player_to(1,6);
			break;
			}
		case FEAT_TELE_3:
			{
			teleport_player_to(1,11);
			break;
			}

		case FEAT_TELE_4:
			{
			teleport_player_to(1,16);
			break;
			}

		case FEAT_TELE_5:
			{
			teleport_player_to(1,21);
			break;
			}

		case FEAT_TELE_6:
			{
			teleport_player_to(6,1);
			break;
			}

		case FEAT_TELE_7:
			{
			teleport_player_to(6,6);
			break;
			}
		case FEAT_TELE_8:
			{
			teleport_player_to(6,11);
			break;
			}

		case FEAT_TELE_9:
			{
			teleport_player_to(6,16);
			break;
			}

		case FEAT_TELE_10:
			{
			teleport_player_to(6,21);
			break;
			}

		case FEAT_TELE_11:
			{
			teleport_player_to(11,1);
			break;
			}

		case FEAT_TELE_12:
			{
			teleport_player_to(11,6);
			break;
			}
		case FEAT_TELE_13:
			{
			teleport_player_to(11,11);
			break;
			}

		case FEAT_TELE_14:
			{
			teleport_player_to(11,16);
			break;
			}

		case FEAT_TELE_15:
			{
			teleport_player_to(11,21);
			break;
			}

		case FEAT_TELE_16:
			{
			teleport_player_to(16,1);
			break;
			}
		case FEAT_TELE_17:
			{
			teleport_player_to(16,6);
			break;
			}
		case FEAT_TELE_18:
			{
			teleport_player_to(16,11);
			break;
			}

		case FEAT_TELE_19:
			{
			teleport_player_to(16,16);
			break;
			}

		case FEAT_TELE_20:
			{
			teleport_player_to(16,21);
			break;
			}

		case FEAT_FUN_TELE:
			{
			if (p_ptr->location == W_HAMILTON){
			teleport_player_to(1,1);
			reset_puzzle();
			}

			else {
				teleport_player_to((DUNGEON_HGT-5) / 2 + 1, 
					(DUNGEON_WID-34) / 2 - 1);
			}

			return;
			break;
			}

							
		}

	/* Put fun teles upon exit */
	y = y-1;
	if (x > 0 && x < 5)
		x = 1;
	else if (x > 5 && x < 10)
		x = 6;
	else if (x > 10 && x < 15)
		x = 11;
	else if (x > 15 && x < 20)
		x = 16;
	else 
		x = 21;

	for (blah =0; blah<4; blah++)
	{
		cave_set_feat(y, x+blah, FEAT_FUN_TELE);
	}

	if (check_victory()){
			
				cave_set_feat(1, 4, FEAT_MORE);
				return;
			}
			
	

}

/*
 * Handle player hitting the special entrance
 */

void hit_special(int location, int y, int x)
{

	int newlocation = location;

	switch (location)
	{

	case W_ASANO:
		{
		if (p_ptr->normal_quests[QUEST_CHUSHINGURA] == STATUS_COMPLETE)
		{
			msg_print("The pathway is closed.");
			return;
		}
		msg_print("You travel down a winding path and sneak into Kira's Mansion.");
		newlocation = W_KIRA;
		break;
		}

	case W_KIRA:
		{
		msg_print("You sneak out.");
		newlocation = W_ASANO;
		break;
		}
	case W_FUN_CITY:
		{
		/* Go to Battle Arena */
			if (p_ptr->pending_duel == FALSE)
		{
			msg_print("The doors to the battle arena are locked.");
			return;
		}
			else if (p_ptr->pending_duel == NORMAL_DUEL)
			{
			msg_print("You prepare yourself mentally.");
		newlocation = W_DUEL_ARENA;
		p_ptr->pending_duel = FALSE;
			}
			else 
			{
				msg_print("You prepare yourself mentally.");
		newlocation = W_KUMITE_ARENA;
		p_ptr->pending_duel = FALSE;
			}

		break;
		}

	case W_DUEL_ARENA:
		{
			newlocation = W_FUN_CITY;
			break;
		}

	case W_KUMITE_ARENA:
		{
			newlocation = W_FUN_CITY;
			break;
		}

	case W_PUZZLE_LAND:
		{
		if (p_ptr->normal_quests[QUEST_PUZZLE_LAND]	== STATUS_COMPLETE)
		{
			msg_print("The doors are locked.");
			return;
		}

		if (p_ptr->lev < 40)
		{
			msg_print("You must be at least level 40 to proceed.");
			return;
		}
		newlocation = W_EULER;
		break;
		}
	case W_TOKYO_TOWER:
		{
			if (p_ptr->normal_quests[QUEST_TOKYO_TOWER] == STATUS_COMPLETE)
			{
				msg_print("The doors are locked.");
				return;
			}
			if (p_ptr->lev < 40)
			{
				msg_print("You must be at least level 40 to proceed.");
				return;
			}
			newlocation = W_TOWER_1;
			break;
		}
	case W_TOWER_1:
		{
			newlocation = W_TOKYO_TOWER;
			break;
		}
	case W_TOWER_ROOF:
		{
			msg_print("You jump off the roof.");
			newlocation = W_TOKYO_TOWER;
			break;
		}
	case W_PAGODA_ROOF:
		{
			msg_print("You jump off the roof.");
			newlocation = W_MIYAZAKI_FOREST;
			break;
		}

	case W_MIYAZAKI_FOREST:
		{
			if (p_ptr->lev < 20)
			{
				msg_print("You must be at least level 20 to enter.");
				return;
			}

			if (p_ptr->normal_quests[QUEST_PAGODA] == STATUS_NOT_KNOWN)
			{
			newlocation = W_PAGODA_1;
			}


			else{
				msg_print("The doors are locked.");
				return;
			}
			break;
		}
	case W_MIYAZAKI_TOWN:
		{
			newlocation = W_KIKI_BAKERY;
			break;
		}

	case W_KIKI_BAKERY:
		{
			newlocation = W_MIYAZAKI_TOWN;
			break;
		}

	case W_TREASURE_ROOM:
		{
			newlocation = W_PUZZLE_LAND;
			break;
		}

	case W_EULER:
		{
			newlocation = W_PUZZLE_LAND;
			break;
		}

		
	
	}

	/* New location */
	p_ptr->location = newlocation;

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Handle player hitting a real trap
 */
void hit_trap(int y, int x)
{
	int i, num, dam;

	cptr name = "a trap";


	/* Disturb the player */
	disturb(0, 0);

	/* Analyze XXX XXX XXX */
	switch (cave_feat[y][x])
	{
		case FEAT_TRAP_HEAD + 0x00:
		{
			msg_print("You fall through a trap door!");
			if (p_ptr->ffall)
			{
				msg_print("You float gently down to the next level.");
			}
			else
			{
				dam = damroll(2, 8);
				take_hit(dam, name);
			}
			
			if (p_ptr->location == W_TOWN)
			/* New depth */
			p_ptr->depth++;

			else
				p_ptr->location++;

			/* Leaving */
			p_ptr->leaving = TRUE;

			break;
		}

		case FEAT_TRAP_HEAD + 0x01:
		{
			msg_print("You fall into a pit!");
			if (p_ptr->ffall)
			{
				msg_print("You float gently to the bottom of the pit.");
			}
			else
			{
				dam = damroll(2, 6);
				take_hit(dam, name);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x02:
		{
			msg_print("You fall into a spiked pit!");

			if (p_ptr->ffall)
			{
				msg_print("You float gently to the floor of the pit.");
				msg_print("You carefully avoid touching the spikes.");
			}

			else
			{
				/* Base damage */
				dam = damroll(2, 6);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled!");

					dam = dam * 2;
					(void)set_cut(p_ptr->cut + randint(dam));
				}

				/* Take the damage */
				take_hit(dam, name);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x03:
		{
			msg_print("You fall into a spiked pit!");

			if (p_ptr->ffall)
			{
				msg_print("You float gently to the floor of the pit.");
				msg_print("You carefully avoid touching the spikes.");
			}

			else
			{
				/* Base damage */
				dam = damroll(2, 6);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled on poisonous spikes!");

					dam = dam * 2;
					(void)set_cut(p_ptr->cut + randint(dam));

					if (p_ptr->resist_pois || p_ptr->oppose_pois)
					{
						msg_print("The poison does not affect you!");
					}
					else
					{
						dam = dam * 2;
						(void)set_poisoned(p_ptr->poisoned + randint(dam));
					}
				}

				/* Take the damage */
				take_hit(dam, name);
			}

			break;
		}

		case FEAT_TRAP_HEAD + 0x04:
		{
			msg_print("You are enveloped in a cloud of smoke!");
			cave_info[y][x] &= ~(CAVE_MARK);
			cave_set_feat(y, x, FEAT_FLOOR);
			num = 2 + randint(3);
			for (i = 0; i < num; i++)
			{
				(void)summon_specific(y, x, p_ptr->depth, 0);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x05:
		{
			msg_print("You hit a teleport trap!");
			teleport_player(100);
			break;
		}

		case FEAT_TRAP_HEAD + 0x06:
		{
			msg_print("You are enveloped in flames!");
			dam = damroll(4, 6);
			fire_dam(dam, "a fire trap");
			break;
		}

		case FEAT_TRAP_HEAD + 0x07:
		{
			msg_print("You are splashed with acid!");
			dam = damroll(4, 6);
			acid_dam(dam, "an acid trap");
			break;
		}

		case FEAT_TRAP_HEAD + 0x08:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name);
				(void)set_slow(p_ptr->slow + rand_int(20) + 20);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x09:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name);
				(void)do_dec_stat(A_STR);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0A:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name);
				(void)do_dec_stat(A_DEX);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0B:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name);
				(void)do_dec_stat(A_CON);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0C:
		{
			msg_print("You are surrounded by a black gas!");
			if (!p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + rand_int(50) + 25);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0D:
		{
			msg_print("You are surrounded by a gas of scintillating colors!");
			if (!p_ptr->resist_confu)
			{
				(void)set_confused(p_ptr->confused + rand_int(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0E:
		{
			msg_print("You are surrounded by a pungent green gas!");
			if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
			{
				(void)set_poisoned(p_ptr->poisoned + rand_int(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0F:
		{
			msg_print("You are surrounded by a strange white mist!");
			if (!p_ptr->free_act)
			{
				(void)set_paralyzed(p_ptr->paralyzed + rand_int(10) + 5);
			}
			break;
		}
	}
}



/*
 * Attack the monster at the given location
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
void py_attack(int y, int x)
{
	int num = 0, k, bonus, chance, amt;
	

	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;

	object_type *o_ptr;

	char m_name[80];

	bool fear = FALSE;

	bool do_quake = FALSE;
	int dir;
	int m_idx = cave_m_idx[y][x];
	u32b f1, f2, f3;

	

	/* Get the monster */
	m_ptr = &m_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];
	l_ptr = &l_list[m_ptr->r_idx];

	/* Set launch */
	launch = FALSE;


	/* Disturb the player */
	disturb(0, 0);


	/* Disturb the monster */
	m_ptr->csleep = 0;


	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);


	/* Auto-Recall if possible and visible */
	if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

	/* Track a new monster */
	if (m_ptr->ml) health_track(cave_m_idx[y][x]);


	/* Handle player fear */
	if (p_ptr->afraid)
	{
		/* Message */
		msg_format("You are too afraid to attack %s!", m_name);

		/* Done */
		return;
	}

	/* Friendly */

	if (r_ptr->flags3 & (RF3_FRIENDLY)){

		/* Message */
		msg_format("You bump into %s.", m_name);

		return;

	}


	/* Get the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Calculate the "attack quality" */
	bonus = p_ptr->to_h + o_ptr->to_h;
	chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));

	

	/* Attack once for each legal blow */
	while (num++ < p_ptr->num_blow)
	{

			
		/* Test for hit */
		if (test_hit_norm(chance, r_ptr->ac, m_ptr->ml))
		{
			/* Message */
			if (p_ptr->geneijin){
			message_format(MSG_HIT, m_ptr->r_idx, "You hit %s.  Hey!", m_name);
			}

			else {
				message_format(MSG_HIT, m_ptr->r_idx, "You hit %s.", m_name);
			}

			/* Hack -- bare hands do one damage */
			k = 1;

			/* Hack -- Chi Warriors bare hands do much more */
			if (p_ptr->pclass == C_CHI_WARRIOR)
			{
				k = rand_int(p_ptr->lev + 5);
			}

			/* Handle normal weapon */
			if (o_ptr->k_idx)
			{
				k = damroll(o_ptr->dd, o_ptr->ds);
				k = tot_dam_aux(o_ptr, k, m_ptr);
				if (p_ptr->impact && (k > 50)) do_quake = TRUE;
				k = critical_norm(o_ptr->weight, o_ptr->to_h, k);
				k += o_ptr->to_d;
			}

			/* Hack -- Bakusai Tengetsu */
			if (p_ptr->bakusai_tengetsu)
			{
				p_ptr->bakusai_tengetsu = FALSE;
				do_quake = TRUE;
			}


			/* Apply the player damage bonuses */
			k += p_ptr->to_d;

			/* Apply groove specific bonuses */

			if (p_ptr->pgroove == G_FIRE)
			{
				/* 33% extra damage */
				k += (k / 6) * (p_ptr->c_meter * 2 / p_ptr->m_meter);
			}

			if (p_ptr->pgroove == G_DRUNK)
			{
				/* 50% extra damage */
				k += (k / 6) * (p_ptr->c_meter * 3 / p_ptr->m_meter);
			}

			/* Sand does less damage */
			if (p_ptr->pgroove == G_SAND)
			{
				/* 33% less damage */
				k -= (k/3);

				/* Gain Meter */
				if (k > 0)
				(void)set_meter(p_ptr->c_meter + k);

			}
		
			/* No negative damage */
			if (k < 0) k = 0;

			/* Hack -- Amaguri ken is a one time damage multiplier */
			if (p_ptr->amaguri_ken)
			{
				/* Hack -- Revert Amaguri Ken */
				p_ptr->amaguri_ken = FALSE;
				msg_print("You use the Amaguri Ken technique");
				k *= (p_ptr->lev / 10 + 1);
			}

			
			if (p_ptr->backstab){
				p_ptr->backstab = FALSE;
				message_format(MSG_HIT, m_ptr->r_idx, "You backstab %s!", m_name);
				k += p_ptr->lev;
			}

			/* Kancho */
			if (p_ptr->kancho)
			{
				msg_format("Kancho successful!");
				k += p_ptr->lev * 4;
				p_ptr->kancho = FALSE;
			}

			/* Rasengan */
			/*if (p_ptr->rasengan)
			{
				msg_format("%^s gets hit by your Rasengan!");
				k += p_ptr->lev * 3; 
				p_ptr->rasengan = FALSE;
			}*/

			/* Hakke */
			if (p_ptr->jyuken)
			{
				k *= 2;
				msg_print("Hakke!  Two strikes!");
				msg_print("Four strikes!  Eight strikes!");
				msg_print("Sixteen strikes!  Thirty-two strikes!");
				msg_print("Sixty-four strikes!");
				p_ptr->jyuken = FALSE;
				
			
				m_ptr->silenced += 10 + rand_int(p_ptr->lev) / 5;
				
			}

			/* Hakke2 */
			if (p_ptr->jyuken2)
			{
				p_ptr->jyuken2 = FALSE;
				k *= 2;
				msg_print("Your hands start moving incredibly fast!");
				
				
				fire_ball(GF_HAKKE, 0, p_ptr->lev, 1);
								
			}

			/* Chidori */
			if (p_ptr->chidori)
			{
				p_ptr->chidori = FALSE;
				k += p_ptr->lev * 5;
				msg_format("You plunge a chidori into %^s!!!", m_name);
			}


			/* Complex message */
			if (p_ptr->wizard)
			{
				msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
			}

			
			/* Damage, check for fear and death */
			if (mon_take_hit(cave_m_idx[y][x], k, &fear, NULL)) break; 
						
			/* Warrior attacks can randomly stun */
			if ((cp_ptr->flags & CF_STUN_HANDS) || (p_ptr->chakra_gate_level > 5))
			{
				if (launch)
				{
					/* Stun monster */
					if (r_ptr->flags3 & (RF3_NO_STUN))
					{
						if (m_ptr->ml)
						{
							l_ptr->r_flags3 |= (RF3_NO_STUN);
						}
						msg_format("%^s is unaffected.", m_name);
					}

					else if (rand_int(100) < r_ptr->level)
					{
						msg_format("%^s is unaffected.", m_name);
					}

					else
					{
						msg_format("%^s appears stunned.", m_name);
						m_ptr->stunned += 10 + rand_int(p_ptr->lev) / 5;
					}

				}

			}

			/* Confusion attack */
			if (p_ptr->confusing)
			{
				/* Cancel glowing hands */
				p_ptr->confusing = FALSE;

				/* Message */
				msg_print("Your hands stop glowing.");

				/* Confuse the monster */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags3 |= (RF3_NO_CONF);
					}

					msg_format("%^s is unaffected.", m_name);
				}
				else if (rand_int(100) < r_ptr->level)
				{
					msg_format("%^s is unaffected.", m_name);
				}
				else
				{
					msg_format("%^s appears confused.", m_name);
					m_ptr->confused += 10 + rand_int(p_ptr->lev) / 5;
				}
			}

			/* Super Punch */
			if (p_ptr->super_punch)
			{
				p_ptr->super_punch = FALSE;
				/* Message */
				msg_print("...from rows one to seven and eleven and tweleve there is open seating!  SHINKANSEN!");
				msg_print("A high speed bullet train rushes out of your fist!");

				/* Stun Monster */
				m_ptr->stunned += 5 + rand_int(p_ptr->lev) / 5;

				/* Launch monster into the air */
				m_ptr->mondust += rand_int(5) + 3;

				/* Phase Monster */
				teleport_away(m_idx, 13);
			}

			if (p_ptr->rasengan)
			{
				p_ptr->rasengan = FALSE;
				msg_format("%^s gets hit by your Rasengan!", m_name);

				/* Stun Monster */
				if (!(m_ptr->stunned)){
				m_ptr->stunned += 10 + rand_int(p_ptr->lev) / 5;
				}

				
				/* Phase Monster */
				teleport_away(m_idx, 15);

			}

			/* Dust */
			if ((f3 & TR3_DUST) || (p_ptr->chakra_gate_level > 3))
			{

				if ((!(m_ptr->mondust)) && (launch))
				{
					/* Launch the monster */
				if (r_ptr->flags3 & (RF3_NO_DUST))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags3 |= (RF3_NO_DUST);
					}

					msg_format("%^s blocks your dust attempt!", m_name);
				}

				else {
					msg_format("%^s gets launched into the air!", m_name);
					m_ptr->mondust += rand_int(5) + 3;
				}

				}


			}

		}

		/* Player misses */
		else
		{
			/* Hack -- a terrible miss will result in a blade catch by smart monsters */
			if ((o_ptr->k_idx) && ((r_ptr->flags2) & (RF2_MUTODORI)))
			{
			
				if ((!(test_hit_norm(chance, r_ptr->ac, m_ptr->ml))) && (rand_int(100) < 50))
				{
					msg_format("%^s catches your weapon and deflects it away!", m_name);

					/* Get a quantity */
					amt = get_quantity(NULL, o_ptr->number);

					/* Apply Knowledge */
					l_ptr->r_flags2 |= (RF2_MUTODORI); 

					/* Hack -- Cannot remove cursed items */
					if (cursed_p(o_ptr))
					{
							/* Oops */
							msg_print("Your cursed weapon stays in your hand!");

							/* Nope */
							break;
					}

					if (!(test_hit_norm(chance, r_ptr->ac, m_ptr->ml)))
					{
					msg_format("You lose your grip on your weapon!");
					/* Deflect weapon onto floor */
					inven_drop(INVEN_WIELD, amt);
					}
					break;

				}

			}
			/* Message */
			message_format(MSG_MISS, m_ptr->r_idx, "You miss %s.", m_name);

			
			
		}
	}


	/* Hack -- delay fear messages */
	if (fear && m_ptr->ml)
	{
		/* Message */
		message_format(MSG_FLEE, m_ptr->r_idx, "%^s flees in terror!", m_name);
	}


	/* Mega-Hack -- apply earthquake brand */
	if (do_quake) earthquake(p_ptr->py, p_ptr->px, 10);
}





/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should only be called when energy has been expended.
 *
 * Note that this routine handles monsters in the destination grid,
 * and also handles attempting to move into walls/doors/rubble/etc.
 */
void move_player(int dir, int jumping)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;


	/* Find the result of moving */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Hack -- attack monsters */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack */
		py_attack(y, x);
	}

#ifdef ALLOW_EASY_ALTER

	/* Optionally alter known traps/doors on (non-jumping) movement */
	else if (easy_alter && !jumping &&
	         (cave_info[y][x] & (CAVE_MARK)) &&
	         (cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
	         (cave_feat[y][x] <= FEAT_DOOR_TAIL))
	{
		/* Not already repeating */
		if (!p_ptr->command_rep)
		{
			/* Hack -- Optional auto-repeat */
			if (always_repeat && (p_ptr->command_arg <= 0))
			{
				/* Repeat 99 times */
				p_ptr->command_rep = 99;

				/* Reset the command count */
				p_ptr->command_arg = 0;
			}
		}

		/* Alter */
		do_cmd_alter();
	}

#endif /* ALLOW_EASY_ALTER */

	/* Player can not walk through "walls" */
	else if (!cave_floor_bold(y, x))
	{
		/* Disturb the player */
		disturb(0, 0);

		/* Notice unknown obstacles */
		if (!(cave_info[y][x] & (CAVE_MARK)))
		{
			/* Rubble */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				message(MSG_HITWALL, 0, "You feel a pile of rubble blocking your way.");
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
			}

			/* Closed door */
			else if (cave_feat[y][x] < FEAT_SECRET)
			{
				message(MSG_HITWALL, 0, "You feel a door blocking your way.");
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
			}

			/* Wall (or secret door) */
			else
			{
				message(MSG_HITWALL, 0, "You feel a wall blocking your way.");
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
			}
		}

		/* Mention known obstacles */
		else
		{
			/* Rubble */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				message(MSG_HITWALL, 0, "There is a pile of rubble blocking your way.");
			}

			/* Closed door */
			else if (cave_feat[y][x] < FEAT_SECRET)
			{
				message(MSG_HITWALL, 0, "There is a door blocking your way.");
			}

			/* Wall (or secret door) */
			else
			{
				message(MSG_HITWALL, 0, "There is a wall blocking your way.");
			}
		}
	}

	/* Normal movement */
	else
	{
		/* Sound XXX XXX XXX */
		/* sound(MSG_WALK); */

		/* Move player */
		monster_swap(py, px, y, x);

		/* New location */
		y = py = p_ptr->py;
		x = px = p_ptr->px;


		/* Spontaneous Searching */
		if ((p_ptr->skill_fos >= 50) ||
		    (0 == rand_int(50 - p_ptr->skill_fos)))
		{
			search();
		}

		/* Continuous Searching */
		if (p_ptr->searching)
		{
			search();
		}

		/* Handle "objects" */
		py_pickup(jumping != always_pickup);

		/* Handle "store doors" */
		if (((cave_feat[y][x] >= FEAT_SHOP_HEAD) &&
		    (cave_feat[y][x] <= FEAT_SHOP_TAIL)) || 
			((cave_feat[y][x] >= FEAT_VENDING_MACHINE)
			&& (cave_feat[y][x] <=FEAT_GROCERY_STORE))
			|| (cave_feat[p_ptr->py][p_ptr->px] == FEAT_DUELING_GUILD))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter store */
			p_ptr->command_new = '_';

			/* Free turn XXX XXX XXX */
			p_ptr->energy_use = 0;
		}

		/* Discover invisible traps */
		else if (cave_feat[y][x] == FEAT_INVIS)
		{
			/* Disturb */
			disturb(0, 0);

			/* Message */
			msg_print("You found a trap!");

			/* Pick a trap */
			pick_trap(y, x);

			/* Hit the trap */
			hit_trap(y, x);
		}

		/* Set off an visible trap */
		else if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
		         (cave_feat[y][x] <= FEAT_TRAP_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hit the trap */
			hit_trap(y, x);
		}

		/* Set off a teleproter */
		else if ((cave_feat[y][x] >= FEAT_TELE_1) &&
		         (cave_feat[y][x] <= FEAT_FUN_TELE))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hit the trap */
			hit_tele(y, x);
		}

		else if (cave_feat[y][x] == FEAT_TRIGGER)
		{
			disturb(0,0);
			if (p_ptr->location == W_MASTERMIND){
			hit_trigger(y,x);
			}

			else
			{
				hit_trigger2(y,x);
			}
		}

		else if (cave_feat[y][x] == FEAT_SPECIAL_ENTRANCE)
		{
			disturb(0,0);
			hit_special(p_ptr->location, y, x);
		}

		else if (cave_feat[y][x] == FEAT_TRAIN_STATION)
		{
			disturb(0,0);
			hit_train_station();
		}
	}
}


/*
 * Hack -- Check for a "known wall" (see below)
 */
static int see_wall(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are not known walls XXX XXX XXX */
	if (!in_bounds(y, x)) return (FALSE);

	/* Non-wall grids are not known walls */
	if (cave_feat[y][x] < FEAT_SECRET) return (FALSE);

	/* Unknown walls are not known walls */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (FALSE);

	/* Default */
	return (TRUE);
}


/*
 * Hack -- Check for an "unknown corner" (see below)
 */
static int see_nothing(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are unknown XXX XXX XXX */
	if (!in_bounds(y, x)) return (TRUE);

	/* Memorized grids are always known */
	if (cave_info[y][x] & (CAVE_MARK)) return (FALSE);

	/* Default */
	return (TRUE);
}





/*
 * The running algorithm  -CJS-
 *
 * Basically, once you start running, you keep moving until something
 * interesting happens.  In an enclosed space, you run straight, but
 * you follow corners as needed (i.e. hallways).  In an open space,
 * you run straight, but you stop before entering an enclosed space
 * (i.e. a room with a doorway).  In a semi-open space (with walls on
 * one side only), you run straight, but you stop before entering an
 * enclosed space or an open space (i.e. running along side a wall).
 *
 * All discussions below refer to what the player can see, that is,
 * an unknown wall is just like a normal floor.  This means that we
 * must be careful when dealing with "illegal" grids.
 *
 * No assumptions are made about the layout of the dungeon, so this
 * algorithm works in hallways, rooms, town, destroyed areas, etc.
 *
 * In the diagrams below, the player has just arrived in the grid
 * marked as '@', and he has just come from a grid marked as 'o',
 * and he is about to enter the grid marked as 'x'.
 *
 * Running while confused is not allowed, and so running into a wall
 * is only possible when the wall is not seen by the player.  This
 * will take a turn and stop the running.
 *
 * Several conditions are tracked by the running variables.
 *
 *   p_ptr->run_open_area (in the open on at least one side)
 *   p_ptr->run_break_left (wall on the left, stop if it opens)
 *   p_ptr->run_break_right (wall on the right, stop if it opens)
 *
 * When running begins, these conditions are initialized by examining
 * the grids adjacent to the requested destination grid (marked 'x'),
 * two on each side (marked 'L' and 'R').  If either one of the two
 * grids on a given side is a wall, then that side is considered to
 * be "closed".  Both sides enclosed yields a hallway.
 *
 *    LL                     @L
 *    @x      (normal)       RxL   (diagonal)
 *    RR      (east)          R    (south-east)
 *
 * In the diagram below, in which the player is running east along a
 * hallway, he will stop as indicated before attempting to enter the
 * intersection (marked 'x').  Starting a new run in any direction
 * will begin a new hallway run.
 *
 * #.#
 * ##.##
 * o@x..
 * ##.##
 * #.#
 *
 * Note that a minor hack is inserted to make the angled corridor
 * entry (with one side blocked near and the other side blocked
 * further away from the runner) work correctly. The runner moves
 * diagonally, but then saves the previous direction as being
 * straight into the gap. Otherwise, the tail end of the other
 * entry would be perceived as an alternative on the next move.
 *
 * In the diagram below, the player is running east down a hallway,
 * and will stop in the grid (marked '1') before the intersection.
 * Continuing the run to the south-east would result in a long run
 * stopping at the end of the hallway (marked '2').
 *
 * ##################
 * o@x       1
 * ########### ######
 * #2          #
 * #############
 *
 * After each step, the surroundings are examined to determine if
 * the running should stop, and to determine if the running should
 * change direction.  We examine the new current player location
 * (at which the runner has just arrived) and the direction from
 * which the runner is considered to have come.
 *
 * Moving one grid in some direction places you adjacent to three
 * or five new grids (for straight and diagonal moves respectively)
 * to which you were not previously adjacent (marked as '!').
 *
 *   ...!              ...
 *   .o@!  (normal)    .o.!  (diagonal)
 *   ...!  (east)      ..@!  (south east)
 *                      !!!
 *
 * If any of the newly adjacent grids are "interesting" (monsters,
 * objects, some terrain features) then running stops.
 *
 * If any of the newly adjacent grids seem to be open, and you are
 * looking for a break on that side, then running stops.
 *
 * If any of the newly adjacent grids do not seem to be open, and
 * you are in an open area, and the non-open side was previously
 * entirely open, then running stops.
 *
 * If you are in a hallway, then the algorithm must determine if
 * the running should continue, turn, or stop.  If only one of the
 * newly adjacent grids appears to be open, then running continues
 * in that direction, turning if necessary.  If there are more than
 * two possible choices, then running stops.  If there are exactly
 * two possible choices, separated by a grid which does not seem
 * to be open, then running stops.  Otherwise, as shown below, the
 * player has probably reached a "corner".
 *
 *    ###             o##
 *    o@x  (normal)   #@!   (diagonal)
 *    ##!  (east)     ##x   (south east)
 *
 * In this situation, there will be two newly adjacent open grids,
 * one touching the player on a diagonal, and one directly adjacent.
 * We must consider the two "option" grids further out (marked '?').
 * We assign "option" to the straight-on grid, and "option2" to the
 * diagonal grid.  For some unknown reason, we assign "check_dir" to
 * the grid marked 's', which may be incorrectly labelled.
 *
 *    ###s
 *    o@x?   (may be incorrect diagram!)
 *    ##!?
 *
 * If both "option" grids are closed, then there is no reason to enter
 * the corner, and so we can cut the corner, by moving into the other
 * grid (diagonally).  If we choose not to cut the corner, then we may
 * go straight, but we pretend that we got there by moving diagonally.
 * Below, we avoid the obvious grid (marked 'x') and cut the corner
 * instead (marked 'n').
 *
 *    ###:               o##
 *    o@x#   (normal)    #@n    (maybe?)
 *    ##n#   (east)      ##x#
 *                       ####
 *
 * If one of the "option" grids is open, then we may have a choice, so
 * we check to see whether it is a potential corner or an intersection
 * (or room entrance).  If the grid two spaces straight ahead, and the
 * space marked with 's' are both open, then it is a potential corner
 * and we enter it if requested.  Otherwise, we stop, because it is
 * not a corner, and is instead an intersection or a room entrance.
 *
 *    ###
 *    o@x
 *    ##!#
 *
 * I do not think this documentation is correct.
 */




/*
 * Hack -- allow quick "cycling" through the legal directions
 */
static const byte cycle[] =
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static const byte chome[] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };



/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. (?)
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #                  #
 *       #x#                 @x#
 *       @p.                  p
 */
static void run_init(int dir)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, row, col;

	bool deepleft, deepright;
	bool shortleft, shortright;


	/* Save the direction */
	p_ptr->run_cur_dir = dir;

	/* Assume running straight */
	p_ptr->run_old_dir = dir;

	/* Assume looking for open area */
	p_ptr->run_open_area = TRUE;

	/* Assume not looking for breaks */
	p_ptr->run_break_right = FALSE;
	p_ptr->run_break_left = FALSE;

	/* Assume no nearby walls */
	deepleft = deepright = FALSE;
	shortright = shortleft = FALSE;

	/* Find the destination grid */
	row = py + ddy[dir];
	col = px + ddx[dir];

	/* Extract cycle index */
	i = chome[dir];

	/* Check for nearby wall */
	if (see_wall(cycle[i+1], py, px))
	{
		p_ptr->run_break_left = TRUE;
		shortleft = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i+1], row, col))
	{
		p_ptr->run_break_left = TRUE;
		deepleft = TRUE;
	}

	/* Check for nearby wall */
	if (see_wall(cycle[i-1], py, px))
	{
		p_ptr->run_break_right = TRUE;
		shortright = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i-1], row, col))
	{
		p_ptr->run_break_right = TRUE;
		deepright = TRUE;
	}

	/* Looking for a break */
	if (p_ptr->run_break_left && p_ptr->run_break_right)
	{
		/* Not looking for open area */
		p_ptr->run_open_area = FALSE;

		/* Hack -- allow angled corridor entry */
		if (dir & 0x01)
		{
			if (deepleft && !deepright)
			{
				p_ptr->run_old_dir = cycle[i - 1];
			}
			else if (deepright && !deepleft)
			{
				p_ptr->run_old_dir = cycle[i + 1];
			}
		}

		/* Hack -- allow blunt corridor entry */
		else if (see_wall(cycle[i], row, col))
		{
			if (shortleft && !shortright)
			{
				p_ptr->run_old_dir = cycle[i - 2];
			}
			else if (shortright && !shortleft)
			{
				p_ptr->run_old_dir = cycle[i + 2];
			}
		}
	}
}


/*
 * Update the current "run" path
 *
 * Return TRUE if the running should be stopped
 */
static bool run_test(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int prev_dir;
	int new_dir;
	int check_dir = 0;

	int row, col;
	int i, max, inv;
	int option, option2;


	/* No options yet */
	option = 0;
	option2 = 0;

	/* Where we came from */
	prev_dir = p_ptr->run_old_dir;


	/* Range of newly adjacent grids */
	max = (prev_dir & 0x01) + 1;


	/* Look at every newly adjacent square. */
	for (i = -max; i <= max; i++)
	{
		s16b this_o_idx, next_o_idx = 0;


		/* New direction */
		new_dir = cycle[chome[prev_dir] + i];

		/* New location */
		row = py + ddy[new_dir];
		col = px + ddx[new_dir];


		/* Visible monsters abort running */
		if (cave_m_idx[row][col] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[row][col]];

			/* Visible monster */
			if (m_ptr->ml) return (TRUE);
		}

		/* Visible objects abort running */
		for (this_o_idx = cave_o_idx[row][col]; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr;

			/* Get the object */
			o_ptr = &o_list[this_o_idx];

			/* Get the next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Visible object */
			if (o_ptr->marked) return (TRUE);
		}


		/* Assume unknown */
		inv = TRUE;

		/* Check memorized grids */
		if (cave_info[row][col] & (CAVE_MARK))
		{
			bool notice = TRUE;

			/* Examine the terrain */
			switch (cave_feat[row][col])
			{
				/* Floors */
				case FEAT_FLOOR:

				/* Invis traps */
				case FEAT_INVIS:

				/* Secret doors */
				case FEAT_SECRET:

				/* Normal veins */
				case FEAT_MAGMA:
				case FEAT_QUARTZ:

				/* Hidden treasure */
				case FEAT_MAGMA_H:
				case FEAT_QUARTZ_H:

				/* Walls */
				case FEAT_WALL_EXTRA:
				case FEAT_WALL_INNER:
				case FEAT_WALL_OUTER:
				case FEAT_WALL_SOLID:
				case FEAT_PERM_EXTRA:
				case FEAT_PERM_INNER:
				case FEAT_PERM_OUTER:
				case FEAT_PERM_SOLID:
				case FEAT_GRAVE_STONE:
				case FEAT_GRAVE_ASANO:

				{
					/* Ignore */
					notice = FALSE;

					/* Done */
					break;
				}

				/* Open doors */
				case FEAT_OPEN:
				case FEAT_BROKEN:
				{
					/* Option -- ignore */
					if (run_ignore_doors) notice = FALSE;

					/* Done */
					break;
				}

				/* Stairs */
				case FEAT_LESS:
				case FEAT_MORE:
				{
					/* Option -- ignore */
					if (run_ignore_stairs) notice = FALSE;

					/* Done */
					break;
				}
			}

			/* Interesting feature */
			if (notice) return (TRUE);

			/* The grid is "visible" */
			inv = FALSE;
		}

		/* Analyze unknown grids and floors */
		if (inv || cave_floor_bold(row, col))
		{
			/* Looking for open area */
			if (p_ptr->run_open_area)
			{
				/* Nothing */
			}

			/* The first new direction. */
			else if (!option)
			{
				option = new_dir;
			}

			/* Three new directions. Stop running. */
			else if (option2)
			{
				return (TRUE);
			}

			/* Two non-adjacent new directions.  Stop running. */
			else if (option != cycle[chome[prev_dir] + i - 1])
			{
				return (TRUE);
			}

			/* Two new (adjacent) directions (case 1) */
			else if (new_dir & 0x01)
			{
				check_dir = cycle[chome[prev_dir] + i - 2];
				option2 = new_dir;
			}

			/* Two new (adjacent) directions (case 2) */
			else
			{
				check_dir = cycle[chome[prev_dir] + i + 1];
				option2 = option;
				option = new_dir;
			}
		}

		/* Obstacle, while looking for open area */
		else
		{
			if (p_ptr->run_open_area)
			{
				if (i < 0)
				{
					/* Break to the right */
					p_ptr->run_break_right = TRUE;
				}

				else if (i > 0)
				{
					/* Break to the left */
					p_ptr->run_break_left = TRUE;
				}
			}
		}
	}


	/* Looking for open area */
	if (p_ptr->run_open_area)
	{
		/* Hack -- look again */
		for (i = -max; i < 0; i++)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = py + ddy[new_dir];
			col = px + ddx[new_dir];

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
			    (cave_feat[row][col] < FEAT_SECRET))
			{
				/* Looking to break right */
				if (p_ptr->run_break_right)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break left */
				if (p_ptr->run_break_left)
				{
					return (TRUE);
				}
			}
		}

		/* Hack -- look again */
		for (i = max; i > 0; i--)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = py + ddy[new_dir];
			col = px + ddx[new_dir];

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
			    (cave_feat[row][col] < FEAT_SECRET))
			{
				/* Looking to break left */
				if (p_ptr->run_break_left)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break right */
				if (p_ptr->run_break_right)
				{
					return (TRUE);
				}
			}
		}
	}


	/* Not looking for open area */
	else
	{
		/* No options */
		if (!option)
		{
			return (TRUE);
		}

		/* One option */
		else if (!option2)
		{
			/* Primary option */
			p_ptr->run_cur_dir = option;

			/* No other options */
			p_ptr->run_old_dir = option;
		}

		/* Two options, examining corners */
		else if (run_use_corners && !run_cut_corners)
		{
			/* Primary option */
			p_ptr->run_cur_dir = option;

			/* Hack -- allow curving */
			p_ptr->run_old_dir = option2;
		}

		/* Two options, pick one */
		else
		{
			/* Get next location */
			row = py + ddy[option];
			col = px + ddx[option];

			/* Don't see that it is closed off. */
			/* This could be a potential corner or an intersection. */
			if (!see_wall(option, row, col) ||
			    !see_wall(check_dir, row, col))
			{
				/* Can not see anything ahead and in the direction we */
				/* are turning, assume that it is a potential corner. */
				if (run_use_corners &&
				    see_nothing(option, row, col) &&
				    see_nothing(option2, row, col))
				{
					p_ptr->run_cur_dir = option;
					p_ptr->run_old_dir = option2;
				}

				/* STOP: we are next to an intersection or a room */
				else
				{
					return (TRUE);
				}
			}

			/* This corner is seen to be enclosed; we cut the corner. */
			else if (run_cut_corners)
			{
				p_ptr->run_cur_dir = option2;
				p_ptr->run_old_dir = option2;
			}

			/* This corner is seen to be enclosed, and we */
			/* deliberately go the long way. */
			else
			{
				p_ptr->run_cur_dir = option;
				p_ptr->run_old_dir = option2;
			}
		}
	}


	/* About to hit a known wall, stop */
	if (see_wall(p_ptr->run_cur_dir, py, px))
	{
		return (TRUE);
	}


	/* Failure */
	return (FALSE);
}



/*
 * Take one step along the current "run" path
 *
 * Called with a real direction to begin a new run, and with zero
 * to continue a run in progress.
 */
void run_step(int dir)
{
	/* Start run */
	if (dir)
	{
		/* Initialize */
		run_init(dir);

		/* Hack -- Set the run counter */
		p_ptr->running = (p_ptr->command_arg ? p_ptr->command_arg : 1000);

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);
	}

	/* Continue run */
	else
	{
		/* Update run */
		if (run_test())
		{
			/* Disturb */
			disturb(0, 0);

			/* Done */
			return;
		}
	}

	/* Decrease counter */
	p_ptr->running--;

	/* Take time */
	p_ptr->energy_use = 100;

	/* Move the player */
	move_player(p_ptr->run_cur_dir, FALSE);
}

