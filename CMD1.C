/* File: cmd1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Return barehanded damage dice for the player's race.
 *
 * Damage on blow 'num'.
 */
void race_barehand_dice(int num, int *ds, int *dd)
{
	if (p_ptr->prace >= RACE_MIN_DRAGON)
	{
		(*ds) = p_ptr->lev+adj_drag[p_ptr->stat_ind[A_DEX]] - 128;
		(*dd) = p_ptr->lev+adj_drag[p_ptr->stat_ind[A_STR]] - 128;

		if ((*ds)<1) (*ds)=1;
		if ((*dd)<1) (*dd)=1;

		/* Damage dice */
		(*ds) = num%2 ? (*ds)/5+1 : (*ds)/15+2;
		(*dd) = num%2 ? (*dd)/15+2 : (*dd)/5+1;
	}
	else
	{
		(*ds) = 1;
		(*dd) = 1;
	}

	return;
}



/*
 * Determine if the player "hits" a monster (non-magical combat).
 *
 * Note -- Always miss 5%, always hit 5%, otherwise use variable "chance" to
 * determine whether the blow lands.
 */
bool test_hit_combat(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Instant hit.  Chance to miss removed in Oangband because
	 * of the way monster ACs now work (fewer truly easy targets).
	 */
	if (k < 5) return (TRUE);

	/* Invisible monsters are harder to hit */
	if (!vis) chance = chance / 2;

	/* Power competes against armor. */
	if ((chance > 0) && (rand_int(chance) >= ac)) return (TRUE);

	/* Assume miss */
	return (FALSE);
}


/*
 * Calculation of critical hits for objects fired or thrown by the player. -LM-
 */
sint critical_shot(int chance, int sleeping_bonus, bool thrown_weapon,
    char o_name[], char m_name[], int visible, monster_type *m_ptr)
{
	int i, k;
	int mult_a_crit = 10;

	/* Extract missile power.  */
	i = (chance + sleeping_bonus);

	/* Test for critical hit. */
    if (randint(i + 200) <= i ||
        (p_ptr->pclass == CLASS_RANGER && m_ptr->csleep > 0))
	{
		/* Encourage the player to throw weapons at sleeping
		 * monsters. -LM-
		 */
		if (sleeping_bonus)
		{
			msg_print("You rudely awaken the monster!");
		}

		/* Determine level of critical hit */
		k = randint(i) + randint(100);

		/* This portion of the function determines the level of critical hit,
		 * then adjusts the damage dice multiplier and displays an appropriate
		 * combat message.
		 * A distinction is made between visible and invisible monsters.
		 */
		if (k < 125)
		{

				if (!visible)
				{
				/* Invisible monster */
					msg_format("The %s finds a mark.", o_name);
				}
				else
				{
				/* Visible monster */
					msg_format("The %s strikes %s.", o_name, m_name);
				}
			mult_a_crit = 15;
		}
		else if (k < 215)
		{
				if (!visible)
				{
				/* Invisible monster */
					msg_format("The %s finds a mark.", o_name);
				}
				else
				{
				/* Visible monster */
					msg_format("The %s penetrates %s.", o_name, m_name);
				}
			mult_a_crit = 21;
		}
		else if (k < 275)
		{
				if (!visible)
				{
				/* Invisible monster */
					msg_format("The %s finds a mark.", o_name);
				}
				else
				{
				/* Visible monster */
					msg_format("The %s drives into %s!", o_name, m_name);
				}
			mult_a_crit = 28;
		}
		else
		{
				if (!visible)
				{
				/* Invisible monster */
					msg_format("The %s finds a mark.", o_name);
				}
				else
				{
				/* Visible monster */
					msg_format("The %s transpierces %s!", o_name, m_name);
				}
			mult_a_crit = 35;
		}
	}
	/* If the shot is not a critical hit, then the default message is shown. */
	else
	{
		mult_a_crit = 10;
		msg_format("The %s hits %s.", o_name, m_name);
	}

	return (mult_a_crit);
}


/*
 * Calculation of critical hits by the player in hand-to-hand combat. -LM-
 */
sint critical_melee(int chance, int sleeping_bonus, char m_name[], object_type *o_ptr, monster_type *m_ptr)
{
	bool dragon = FALSE;

	int i, k;
	int mult_m_crit;

	/* number and sides of dice */
	int dn,ds;

	dn = o_ptr->dd;
	ds = o_ptr->ds;

	/* Dragon message and dice */
	if (p_ptr->prace >= RACE_MIN_DRAGON)
	{
		dn = p_ptr->lev+adj_drag[p_ptr->stat_ind[A_DEX]] - 128;
		ds = p_ptr->lev+adj_drag[p_ptr->stat_ind[A_STR]] - 128;

		dragon = TRUE;

    /* paranoid */
    if (ds < 1) ds = 1;
    if (dn < 1) dn = 1;
    }

	/* Extract melee attack power.  */
	i = (chance + sleeping_bonus);

	/* Test for critical hit. */
    if (randint(i + 200) <= i ||
        (p_ptr->pclass == CLASS_ROGUE && m_ptr->csleep))
	{
		/* Encourage the player to make sneak attacks on
		 * sleeping monsters. -LM-
		 */
		if ((sleeping_bonus) && (p_ptr->pclass == CLASS_ROGUE))
			msg_print("You ruthlessly sneak attack!");

		/* Hack - Weapons that normally do little damage benefit most from
		 * critical hits (10x inflation).
		 */
		mult_m_crit = 120 / (dn * (ds + 1));
		if (mult_m_crit > 20) mult_m_crit = 20;
		if (mult_m_crit < 10) mult_m_crit = 10;


		/* Determine level of critical hit */
		k = randint(i) + randint(100);

		/* This portion of the function determines the level of critical hit,
		 * the critical mult_m_crit, and displays an appropriate combat
		 * message.  A distinction is often made between edged and blunt
		 * weapons.  Unfortunately, whips sometimes display rather odd
		 * messages...
		 */
		if (k < 100)
		{
			mult_m_crit *= 15;

			if (!dragon)
			{
				msg_format("You strike %s.", m_name);
			}
		}
		else if (k < 160)
		{
			mult_m_crit *= 17;

			if (!dragon)
			{
				if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
					msg_format("You hack at %s.", m_name);
				else
					msg_format("You bash %s.", m_name);
			}
		}
		else if (k < 210)
		{
			mult_m_crit *= 20;

			if (!dragon)
			{
				if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
					msg_format("You slash %s.", m_name);
				else
					msg_format("You pound %s.", m_name);
			}
		}
		else if (k < 250)
		{
			mult_m_crit *= 23;

			if (!dragon)
			{
				if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
					msg_format("You score %s!", m_name);
				else
					msg_format("You batter %s!", m_name);
			}
		}
		else if (k < 280)
		{
			mult_m_crit *= 27;

			if (!dragon)
			{
				if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
					msg_format("You gouge %s!", m_name);
				else
					msg_format("You bludgeon %s!", m_name);
			}
		}
		else
  		{
			mult_m_crit *= 32;

			if (!dragon)
			{
				msg_format("You *smite* %s!", m_name);
			}
		}


		/* Compensate for the weak weapon bonus by deflating the critical
		 * hit multiplier.
		 */
		mult_m_crit /= 10;
	}

	/* If the blow is not a critical hit, display the default attack
	 * message and apply the standard multiplier.
	 */
	else
	{
		mult_m_crit = 10;

		if (!dragon) msg_format("You hit %s.", m_name);
	}

	return (mult_m_crit);
}


/*
 * Calculate the ego multiplier.
 *
 * Note that flasks of oil do NOT do fire damage, although they
 * certainly could be made to do so.  XXX XXX
 *
 * Most slays are x2, except Slay Animal (x1.7), and Slay Evil (x1.5).
 * Weapons of *slaying* now get a larger bonus. All brands are x1.7. -LM-
 *
 * Players may have temporary magic branding.  Paladins do not get to apply
 * temporary brands to missiles.  A nasty hack, but necessary. -LM-
 */
sint tot_dam_aux(object_type *o_ptr, monster_type *m_ptr)
{
	int mult_ego = 10;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);


	/* Wielded weapons and diggers and fired missiles may do extra damage. */
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			/* Slay Animal */
			if ((f1 & (TR1_SLAY_ANIMAL)) &&
			    (r_ptr->flags3 & (RF3_ANIMAL)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_ANIMAL);
				}

				if ((o_ptr->name2 == EGO_KILL_ANIMAL) &&
					(mult_ego < 20)) mult_ego = 20;

				else if (mult_ego < 17) mult_ego = 17;
			}

			/* Slay Evil */
			if ((f1 & (TR1_SLAY_EVIL)) &&
				(r_ptr->flags3 & (RF3_EVIL)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_EVIL);
				}

				if ((o_ptr->name2 == EGO_KILL_EVIL) &&
					(mult_ego < 17)) mult_ego = 17;

				else if (mult_ego < 15) mult_ego = 15;
			}

			/* Slay Undead */
			if ((f1 & (TR1_SLAY_UNDEAD)) &&
			    (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_UNDEAD);
				}

				if ((o_ptr->name2 == EGO_KILL_UNDEAD) &&
					(mult_ego < 25)) mult_ego = 25;

				else if (mult_ego < 20) mult_ego = 20;
			}

			/* Slay Demon */
			if ((f1 & (TR1_SLAY_DEMON)) &&
			    (r_ptr->flags3 & (RF3_DEMON)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_DEMON);
				}

				if ((o_ptr->name2 == EGO_KILL_DEMON) &&
					(mult_ego < 25)) mult_ego = 25;

				else if (mult_ego < 20) mult_ego = 20;
			}

			/* Slay Orc */
			if ((f1 & (TR1_SLAY_ORC)) &&
			    (r_ptr->flags3 & (RF3_ORC)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_ORC);
				}

				if ((o_ptr->name2 == EGO_KILL_ORC) &&
					(mult_ego < 25)) mult_ego = 25;

				else if (mult_ego < 20) mult_ego = 20;
			}

			/* Slay Troll */
			if ((f1 & (TR1_SLAY_TROLL)) &&
			    (r_ptr->flags3 & (RF3_TROLL)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_TROLL);
				}

				if ((o_ptr->name2 == EGO_KILL_TROLL) &&
					(mult_ego < 25)) mult_ego = 25;

				else if (mult_ego < 20) mult_ego = 20;
			}

			/* Slay Giant */
			if ((f1 & (TR1_SLAY_GIANT)) &&
			    (r_ptr->flags3 & (RF3_GIANT)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_GIANT);
				}

				if ((o_ptr->name2 == EGO_KILL_GIANT) &&
					(mult_ego < 25)) mult_ego = 25;

				else if (mult_ego < 20) mult_ego = 20;
			}

			/* Slay Dragon */
			if ((f1 & (TR1_SLAY_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_DRAGON);
				}

				if ((o_ptr->name2 == EGO_KILL_DRAGON) &&
					(mult_ego < 25)) mult_ego = 25;

				else if (mult_ego < 20) mult_ego = 20;
			}

			/* Brand (Acid) */
			if (f1 & (TR1_BRAND_ACID))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ACID))
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= (RF3_IM_ACID);
					}
				}

				/* Otherwise, take extra damage */
				else if (mult_ego < 17) mult_ego = 17;
			}

			/* Brand (Elec) */
			if (f1 & (TR1_BRAND_ELEC))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ELEC))
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= (RF3_IM_ELEC);
					}
				}

				/* Otherwise, take extra damage */
				else if (mult_ego < 17) mult_ego = 17;
			}

			/* Brand (Fire) */
			if (f1 & (TR1_BRAND_FIRE))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_FIRE))
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= (RF3_IM_FIRE);
					}
				}

				/* Otherwise, take extra damage */
				else
				{
					if (mult_ego < 17) mult_ego = 17;
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
						r_ptr->r_flags3 |= (RF3_IM_COLD);
					}
				}

				/* Otherwise, take extra damage */
				else if (mult_ego < 17) mult_ego = 17;
			}

			break;
		}
	}

	/* Return the final damage multiplier. */
	return (mult_ego);
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

					/* Pick a door XXX XXX XXX */
					cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

					/* Disturb */
					disturb(0, 0);
				}

				/* Scan all objects in the grid */
				for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
				{
					object_type *o_ptr;

					/* Acquire object */
					o_ptr = &o_list[this_o_idx];

					/* Acquire next object */
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
 * Make the player carry everything in a grid
 *
 * If "pickup" is FALSE then only gold will be picked up
 */
void py_pickup(int pickup)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	s16b this_o_idx, next_o_idx = 0;

	char o_name[180];


	/* Scan the pile of objects */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Acquire next object */
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
		}

		/* Pick up objects */
		else
		{
			/* Describe the object */
			if (!pickup)
			{
				msg_format("You see %s.", o_name);
			}

			/* Note that the pack is too full */
			else if (!inven_carry_okay(o_ptr))
			{
				msg_format("You have no room for %s.", o_name);
			}

			/* Pick up the item (if requested and allowed) */
			else
			{
				int okay = TRUE;

				/* Hack -- query every item */
				if (carry_query_flag)
				{
					char out_val[160];
					sprintf(out_val, "Pick up %s? ", o_name);
					okay = get_check(out_val);
				}

				/* Attempt to pick up an object. */
				if (okay)
				{
					int slot;

					/* Carry the item */
					slot = inven_carry(o_ptr);

					/* Get the item again */
					o_ptr = &inventory[slot];

					/* Describe the object */
					object_desc(o_name, o_ptr, TRUE, 3);

					/* Message */
					msg_format("You have %s (%c).", o_name, index_to_label(slot));

					/* Delete the object */
					delete_object_idx(this_o_idx);
				}
			}
		}
	}
}




/*
 * Determine if a trap affects the player.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match trap power against player armor.
 */
static int check_trap_hit(int power)
{
	int k, ac;
	bool hit = FALSE;

	/* This function is called from the trap attack code, which generally
	 * uses the simple RNG.  We temporarily switch over to the complex
	 * RNG for true randomness. - LM-
	 */
	Rand_quick = FALSE;

	/* Percentile dice */
	k = rand_int(100);

	/* 5% minimum chance to hit, 5% minimum chance to miss */
	if (k < 10) hit = (k < 5);

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Power competes against Armor */
	if ((power > 0) && (randint(power) >= (ac * 3 / 4))) hit = TRUE;

	/* Resume use of the simple RNG. */
	Rand_quick = TRUE;

	/* Return hit or miss. */
	return (hit);

}




/*
 * Handle player hitting a real trap.  Rewritten in Oangband to allow a
 * greater variety of traps, with effects controlled by dungeon level.
 * To allow a trap to choose one of a variety of effects consistantly,
 * the quick RNG is often used, and xy coordinates input as a seed value.
 */
void hit_trap(int y, int x)
{
	int i, num;
	int dam = 0;

	int nastyness, selection;

	cptr name = "a trap";

	/* Use the "simple" RNG to insure that traps are consistant. */
	Rand_quick = TRUE;

	/* Use the coordinates of the trap to seed the RNG. */
	Rand_value = y * x;

	/* Disturb the player */
	disturb(0, 0);

	/* Analyze XXX XXX XXX */
	switch (cave_feat[y][x])
	{
		/* trap door. */
		case FEAT_TRAP_HEAD + 0x00:
		{
			Rand_quick = FALSE;


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

			/* New depth */
			p_ptr->depth++;

			/* Leaving */
			p_ptr->leaving = TRUE;

			Rand_quick = TRUE;

			break;
		}

		/* pits. */
		case FEAT_TRAP_HEAD + 0x01:
		{
			/* determine how dangerous the trap is allowed to be. */
			nastyness = randint(p_ptr->depth);
			if (randint(20) == 1) nastyness += 20;
			else if (randint(5) == 1) nastyness += 10;

			/* Player is now in pit. */
			monster_swap(p_ptr->py, p_ptr->px, y, x);

			/* Center on player. */
			y = p_ptr->py;
			x = p_ptr->px;

			/* pit of daggers. */
			if ((nastyness > 80) && (randint(3) != 3))
			{
				msg_print("You fall into a pit of daggers!");

				if (p_ptr->ffall)
				{
					msg_print("You float gently to the floor of the pit.");
					msg_print("You carefully avoid setting off the daggers.");
				}

				else
				{
					/* a trap of morgul. */
					if (randint(6) == 1)
					{
						Rand_quick = FALSE;


						msg_print("A single coldly gleaming dagger pierces you deeply!");

						/* activate the Black Breath. */
						(void)set_nasty_poisoned(TRUE);

						/* lots of damage. */
						dam = damroll(20,15);

						/* undead may be attracted. */
						if (randint(2) == 1)
						{
							msg_print("Undead suddenly appear and call you to them!");
							for (i = 0; i < randint(3) + 2; i++)
							{
								summon_specific(y, x, p_ptr->depth, SUMMON_UNDEAD);
							}
						}

						/* morgul-traps are one-time only. */
						cave_info[y][x] &= ~(CAVE_MARK);
						cave_set_feat(y, x, FEAT_FLOOR);

						Rand_quick = TRUE;
					}

					else
					{
						Rand_quick = FALSE;

						/* activate the ordinary daggers. */
						msg_print("Daggers pierce you everywhere!");

						for (i = 0; i < randint(10) + 5; i++)
						{
							dam += damroll(3, 4);
						}

						Rand_quick = TRUE;
					}

					/* cut the player. */
					(void)set_cut(p_ptr->cut + randint(dam));

					/* Take the damage. */
					take_hit(dam, name);
				}
			}

			/* poisoned spiked pit. */
			else if ((nastyness > 55) && (randint(3) != 3))
			{
				msg_print("You fall into a spiked pit!");

				if (p_ptr->ffall)
				{
					msg_print("You float gently to the floor of the pit.");
					msg_print("You carefully avoid touching the spikes.");
				}

				else
				{
					Rand_quick = FALSE;

					/* Base damage */
					dam = damroll(2, 6);

					/* Extra spike damage */
					if (rand_int(100) < 85)
					{
						msg_print("You are impaled on poisonous spikes!");

						dam = dam * (randint(6) + 3);
						(void)set_cut(p_ptr->cut + randint(dam));

						if (p_ptr->resist_pois || p_ptr->oppose_pois)
						{
							msg_print("The poison does not affect you!");
						}

						else
						{
							dam = 3 * dam / 2;
							(void)set_poisoned(p_ptr->poisoned + randint(dam));
						}
					}

					/* Take the damage */
					take_hit(dam, name);

					Rand_quick = TRUE;
				}
			}

			/* spiked pit. */
			else if ((nastyness > 30) && (randint(3) != 3))
			{
				msg_print("You fall into a spiked pit!");

				if (p_ptr->ffall)
				{
					msg_print("You float gently to the floor of the pit.");
					msg_print("You carefully avoid touching the spikes.");
				}

				else
				{
					Rand_quick = FALSE;

					/* Base damage */
					dam = damroll(2, 6);

					/* Extra spike damage */
					if (rand_int(100) < 85)
					{
						msg_print("You are impaled!");

						dam = dam * (2 + randint(4));
						(void)set_cut(p_ptr->cut + randint(dam));
					}

					/* Take the damage */
					take_hit(dam, name);

					Rand_quick = TRUE;
				}
			}

			/* ordinary pit in all other cases. */
			else
			{
				msg_print("You fall into a pit!");
				if (p_ptr->ffall)
				{
					msg_print("You float gently to the bottom of the pit.");
				}
				else
				{
					Rand_quick = FALSE;

					dam = damroll(2, 6);
					take_hit(dam, name);

					Rand_quick = TRUE;
				}
			}

			break;
		}

		/* stat-reducing dart traps. */
		case FEAT_TRAP_HEAD + 0x02:
		{
			/* decide if the dart hits. */
			if (check_trap_hit(50 + p_ptr->depth))
			{
				/* select a stat to drain. */
				selection = rand_int(6);

				Rand_quick = FALSE;

				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name);

				/* Determine how dangerous the trap is allowed to be. */
				nastyness = randint(p_ptr->depth);

				/* decide how much to drain the stat by. */
				if ((nastyness > 50) && (randint(3) == 1))
				{
					num = randint(4);
				}
				else num = 1;

				/* drain the stat. */
				for (i = 0; i < num; i++)
				{
					(void)do_dec_stat(selection);
				}

				Rand_quick = TRUE;
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		/* discolored spots. */
		case FEAT_TRAP_HEAD + 0x03:
		{
			/* determine how dangerous the trap is allowed to be. */
			nastyness = randint(p_ptr->depth);
			if (randint(5) == 1) nastyness += 10;

			/* pick a elemental attack type. */
			selection = randint(4);


			/* electicity trap. */
			if (selection == 1)
			{
				if ((nastyness >= 50) && (randint(2) == 1))
				{
					Rand_quick = FALSE;

					msg_print("You are struck by lightning!");
					dam = damroll(6, 30);

					Rand_quick = TRUE;
				}
				else
				{
					Rand_quick = FALSE;

					msg_print("You get zapped!");
					dam = damroll(4, 8);

					Rand_quick = TRUE;
				}
				Rand_quick = FALSE;
				elec_dam(dam, "an electricity trap");
				Rand_quick = TRUE;

			}

			/* frost trap. */
			if (selection == 2)
			{
				if ((nastyness >= 50) && (randint(2) == 1))
				{
					Rand_quick = FALSE;

					msg_print("You are lost within a blizzard!");
					dam = damroll(6, 30);

					Rand_quick = TRUE;
				}
				else
				{
					Rand_quick = FALSE;

					msg_print("You are coated in frost!");
					dam = damroll(4, 8);

					Rand_quick = TRUE;
				}
				Rand_quick = FALSE;
				cold_dam(dam, "a frost trap");
				Rand_quick = TRUE;
			}

			/* fire trap. */
			if (selection == 3)
			{
				if ((nastyness >= 50) && (randint(2) == 1))
				{
					Rand_quick = FALSE;

					msg_print("You are enveloped in a column of fire!");
					dam = damroll(6, 30);

					Rand_quick = TRUE;
				}
				else
				{
					Rand_quick = FALSE;

					msg_print("You are surrounded by flames!");
					dam = damroll(4, 8);

					Rand_quick = TRUE;
				}
				Rand_quick = FALSE;
				fire_dam(dam, "a fire trap");
				Rand_quick = TRUE;
			}

			/* acid trap. */
			if (selection == 4)
			{
				if ((nastyness >= 50) && (randint(2) == 1))
				{
					Rand_quick = FALSE;

					msg_print("A cauldron of acid is tipped over your head!");
					dam = damroll(6, 30);

					Rand_quick = TRUE;
				}
				else
				{
					Rand_quick = FALSE;

					msg_print("You are splashed with acid!");
					dam = damroll(4, 8);

					Rand_quick = TRUE;
				}
				Rand_quick = FALSE;
				acid_dam(dam, "an acid trap");
				Rand_quick = TRUE;
			}

			break;
		}

		/* gas traps. */
		case FEAT_TRAP_HEAD + 0x04:
		{
			selection = randint(4);

			/* blinding trap. */
			if (selection == 1)
			{
				msg_print("You are surrounded by a black gas!");
				if (!p_ptr->resist_blind)
				{
					Rand_quick = FALSE;

					(void)set_blind(p_ptr->blind + rand_int(30) + 15);

					Rand_quick = TRUE;
				}
			}

			/* confusing trap. */
			if (selection == 2)
			{
				msg_print("You are surrounded by a gas of scintillating colors!");
				if (!p_ptr->resist_confu)
				{
					Rand_quick = FALSE;

					(void)set_confused(p_ptr->confused + rand_int(20) + 10);

					Rand_quick = TRUE;
				}
			}

			/* poisoning trap. */
			if (selection == 3)
			{
				msg_print("You are surrounded by a pungent green gas!");
				if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
				{
					Rand_quick = FALSE;

					(void)set_poisoned(p_ptr->poisoned + rand_int(20) + 10);

					Rand_quick = TRUE;
				}
			}

			/* sleeping trap. */
			if (selection == 4)
			{
				msg_print("You are surrounded by a strange white mist!");
				if (!p_ptr->free_act)
				{
					(void)set_paralyzed(p_ptr->paralyzed + rand_int(10) + 5);
				}
			}

			break;
		}

		/* summoning traps. */
		case FEAT_TRAP_HEAD + 0x05:
		{
			/* sometimes summon thieves. */
			if ((p_ptr->depth > 8) && (randint(5) == 1))
			{
				msg_print("You have aroused a den of thieves!");

				Rand_quick = FALSE;

				num = 2 + randint(3);
				for (i = 0; i < num; i++)
				{
					(void)summon_specific(y, x, p_ptr->depth, SUMMON_THIEF);
				}

				Rand_quick = TRUE;
			}

			/* sometimes summon a nasty unique. */
			else if (randint(8) == 1)
			{
				msg_print("You are enveloped in a cloud of smoke!");

				Rand_quick = FALSE;

				(void)summon_specific(y, x, p_ptr->depth + 5, SUMMON_UNIQUE);

				Rand_quick = TRUE;
			}

			/* otherwise, the ordinary summon monsters. */
			else
			{
				msg_print("You are enveloped in a cloud of smoke!");

				Rand_quick = FALSE;

				num = 2 + randint(3);
				for (i = 0; i < num; i++)
				{
					(void)summon_specific(y, x, p_ptr->depth, 0);
				}

				Rand_quick = TRUE;
			}

			/* these are all one-time traps. */
			cave_info[y][x] &= ~(CAVE_MARK);
			cave_set_feat(y, x, FEAT_FLOOR);

			break;
		}

		/* dungeon alteration traps. */
		case FEAT_TRAP_HEAD + 0x06:
		{
			/* determine how dangerous the trap is allowed to be. */
			nastyness = randint(p_ptr->depth);
			if (randint(5) == 1) nastyness += 10;

			/* make room for alterations. */
			cave_info[y][x] &= ~(CAVE_MARK);
			cave_set_feat(y, x, FEAT_FLOOR);

			/* Everything truely random from here on. */
			Rand_quick = FALSE;

			/* dungeon destruction trap. */
			if ((nastyness > 60) && (randint(12) == 1))
			{
				msg_print("A ear-splitting howl shatters your mind as the dungeon is smashed by hammer blows!");

				(void)destroy_area(p_ptr->py, p_ptr->px, 15, TRUE);

				/* the player is hard-hit. */
				(void)set_confused(p_ptr->confused + rand_int(20) + 10);
				(void)set_blind(p_ptr->blind + rand_int(30) + 15);
				(void)set_stun(p_ptr->stun + randint(50) + 50);
				dam = damroll(15,15);
				take_hit(dam, name);
			}

			/* earthquake trap. */
			else if ((nastyness > 20) && (randint(4) == 1))
			{
				msg_print("A tremor shakes the dungeon around you");
				earthquake(y, x, 10);
			}

			/* falling rock trap. */
			else if ((nastyness > 4) && (randint(2) == 1))
			{
				msg_print("A rock falls on your head.");
				dam = damroll(2,10);
				take_hit(dam, name);

				(void)set_stun(p_ptr->stun + randint(10) + 10);
			}

			/* a few pebbles. */
			else
			{
				msg_print("A bunch of pebbles rain down on you.");
				dam = damroll(1,8);
				take_hit(dam, name);
			}

			Rand_quick = TRUE;

			break;
		}

		/* various char and equipment-alteration traps, lumped together
		 * to avoid any one effect being too common (some of them can be
		 * rather nasty).
		 */
		case FEAT_TRAP_HEAD + 0x07:
		{
			/* determine how dangerous the trap is allowed to be. */
			nastyness = rand_int(100);

			/* these are all one-time traps. */
			cave_info[y][x] &= ~(CAVE_MARK);
			cave_set_feat(y, x, FEAT_FLOOR);

			/* Everything truely random from here on. */
			Rand_quick = FALSE;

			/* trap of drain wands. */
			if (nastyness < 15)
			{
				/* Hold the object information. */
				object_type *o_ptr;

				/* Find an item */
				for (i = 0; i < 20; i++)
				{
					/* Pick an item */
					i = rand_int(INVEN_PACK);

					/* Obtain the item */
					o_ptr = &inventory[i];

					/* use "num" to decide if a item can be
					 * uncharged.  By default, assume it can't. */
					num = 0;

					/* Skip non-objects */
					if (!o_ptr->k_idx) continue;

					/* Drain charged wands/staffs/rods */
					if ((o_ptr->tval == TV_STAFF) ||
						(o_ptr->tval == TV_WAND) ||
						(o_ptr->tval == TV_ROD))
					{
						/* case of charged wands/staffs. */
						if (((o_ptr->tval == TV_STAFF) ||
							(o_ptr->tval == TV_WAND)) &&
							(o_ptr->pval)) num = 1;

						/* case of charged rods. */
						if ((o_ptr->tval == TV_ROD) &&
						    (o_ptr->timeout < o_ptr->pval)) num = 1;


						if (num == 1)
						{
							/* Message */
							msg_print("Energy drains from your pack!");

							/* Uncharge */
							if ((o_ptr->tval == TV_STAFF) ||
								(o_ptr->tval == TV_WAND))
								o_ptr->pval = 0;

							if (o_ptr->tval == TV_ROD)
								o_ptr->timeout = o_ptr->pval * 2;


							/* Combine / Reorder the pack */
							p_ptr->notice |= (PN_COMBINE |
							PN_REORDER);

							/* Window stuff */
							p_ptr->window |= (PW_INVEN);

							/* not more than one inventory
							 * slot effected. */
							break;
						}
						else continue;
					}
				}
			}

			/* trap of forgetting. */
			else if (nastyness < 35)
			{
				if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You hang on to your memories!");
				}
				else if (lose_all_info())
				{
					msg_print("Your memories fade away.");
				}
			}

			/* trap of alter reality. */
			else if (nastyness < 50)
			{
				msg_print("The world changes!");

				/* Leaving */
				p_ptr->leaving = TRUE;
			}

			/* trap of remold player. */
			else if (nastyness < 75)
			{
				int max1, cur1, max2, cur2, ii, jj;

				msg_print("You feel yourself being twisted by wild magic!");

				if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
				}
				else
				{
					msg_print("Your body starts to scramble...");

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
			}

			/* time ball trap. */
			else
			{
				msg_print("You feel time itself assault you!");

				/* Target the player with a radius 0 ball attack. */
				(void)project(0, 0, p_ptr->py, p_ptr->px, 0, GF_TIME, (PROJECT_KILL | PROJECT_PLAY), 0, 0);
				take_hit(damroll(5,50), name);
			}

			Rand_quick = TRUE;

			break;
		}

		/* teleport trap */
		case FEAT_TRAP_HEAD + 0x08:
		{
			msg_print("You teleport across the dungeon.");

			Rand_quick = FALSE;

			teleport_player(250);

			Rand_quick = TRUE;

			break;
		}

		/* murder holes. */
		case FEAT_TRAP_HEAD + 0x09:
		{
			/* hold the object info. */
			object_type *o_ptr;
			object_type object_type_body;

			/* hold the missile type and name. */
			int sval = 0;
			int tval = 0;
			cptr missile_name = "";


			/* Determine the missile type and base damage. */
			if (randint(3) == 1)
			{
				if (p_ptr->depth < 40)
				{
					missile_name = "shot";
					dam = damroll(2,3);
					tval = TV_SHOT;
					sval = SV_AMMO_NORMAL;
				}
				else
				{
					missile_name = "seeker shot";
					dam = damroll(3,7);
					tval = TV_SHOT;
					sval = SV_AMMO_HEAVY;
				}
			}

			else if (randint(2) == 1)
			{
				if (p_ptr->depth < 55)
				{
					missile_name = "arrow";
					dam = damroll(2,4);
					tval = TV_ARROW;
					sval = SV_AMMO_NORMAL;
				}
				else
				{
					missile_name = "seeker arrow";
					dam = damroll(3,9);
					tval = TV_ARROW;
					sval = SV_AMMO_HEAVY;
				}
			}

			else
			{
				if (p_ptr->depth < 65)
				{
					missile_name = "bolt";
					dam = damroll(2,5);
					tval = TV_BOLT;
					sval = SV_AMMO_NORMAL;
				}
				else
				{
					missile_name = "seeker bolt";
					dam = damroll(3,11);
					tval = TV_BOLT;
					sval = SV_AMMO_HEAVY;
				}
			}

			/* determine if the missile hits. */
			if (check_trap_hit(75 + p_ptr->depth))
			{
				msg_format("A %s hits you from above.", missile_name);

				Rand_quick = FALSE;

				/* critical hits. */
				if (randint(2) == 1)
				{
					msg_print("It was well-aimed!");
					dam *= 1 + randint(2);
				}
				if (randint(2) == 1)
				{
					msg_print("It gouges you!");
					dam = 3 * dam / 2;

					/* cut the player. */
					(void)set_cut(p_ptr->cut + randint(dam));
				}

				Rand_quick = TRUE;

				take_hit(dam, name);
			}

			/* Explain what just happened. */
			else msg_format("A %s wizzes by your head.", missile_name);

			/* Get local object */
			o_ptr = &object_type_body;

			/* Make a missile, identify it, and drop it near the player. */
			object_prep(o_ptr, lookup_kind(tval, sval));
			object_aware(o_ptr);
			object_known(o_ptr);
			drop_near(o_ptr, -1, y, x);

			break;
		}

		/* undefined trap. */
		case FEAT_TRAP_HEAD + 0x0A:
		{
			msg_print("A dagger is thrown at you from the shadows!");
			dam = damroll(3,4);
			take_hit(dam, name);

			break;
		}

		/* undefined trap. */
		case FEAT_TRAP_HEAD + 0x0B:
		{
			msg_print("A dagger is thrown at you from the shadows!");
			dam = damroll(3,4);
			take_hit(dam, name);

			break;
		}

		/* undefined trap. */
		case FEAT_TRAP_HEAD + 0x0C:
		{
			msg_print("A dagger is thrown at you from the shadows!");
			dam = damroll(3,4);
			take_hit(dam, name);

			break;
		}

		/* undefined trap. */
		case FEAT_TRAP_HEAD + 0x0D:
		{
			msg_print("A dagger is thrown at you from the shadows!");
			dam = damroll(3,4);
			take_hit(dam, name);

			break;
		}

		/* undefined trap. */
		case FEAT_TRAP_HEAD + 0x0E:
		{
			msg_print("A dagger is thrown at you from the shadows!");
			dam = damroll(3,4);
			take_hit(dam, name);

			break;
		}

		/* A Rogue's monster trap. */
		case FEAT_TRAP_HEAD + 0x0F:
		{
			msg_print("You inspect your cunning trap.");
			break;
		}
	}

	/* Revert to usage of the complex RNG. */
	Rand_quick = FALSE;
}




/*
 * Attack the monster at the given location
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
void py_attack(int y, int x)
{
	/* Number of dice, also total damage. */
	s32b k;

	/* The whole and fractional damage dice and their resulting damage. */
	int k_remainder, k_whole;

	/* Bonus to attack if monster is sleeping, for certain classes. */
	int sleeping_bonus = 0;

	/* Blows counts */
	int num = 0;
    int blows_used = 0;

	int bonus, chance, total_deadliness;
	int l, m;

	int blows;

	monster_type *m_ptr;
	monster_race *r_ptr;

	object_type *o_ptr;

	char m_name[80];
	char message[80];

	bool fear = FALSE;

	bool do_quake = FALSE;


	/* Access the monster */
	m_ptr = &m_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];


	/* Disturb the player */
	disturb(0, 0);

	/* Initial blows available. */
	blows = p_ptr->num_blow;

	/* If the monster is sleeping and visible, it can be hit
	 * more effectively by some classes. -LM-
	 */
	if ((m_ptr->csleep) && (m_ptr->ml))
	{
		if (p_ptr->pclass == CLASS_ROGUE)
			sleeping_bonus = 10 + 2 * p_ptr->lev / 5;
		else sleeping_bonus = 0;
	}


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


	/* Access the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Initialize. */
	total_deadliness = p_ptr->to_d + o_ptr->to_d;

	/* Paranoia.  Ensure legal table access. */
	if (total_deadliness > 150) total_deadliness = 150;

	/* Calculate the "attack quality".  As BTH_PLUS_ADJ has been reduced
	 * to 1, base skill and modifiers to skill are given equal weight. -LM-
	 */
	bonus = p_ptr->to_h + o_ptr->to_h;
	chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));


	/* Attack once for each legal blow */
	while (num++ < p_ptr->num_blow)
	{
		/* Test for hit */
		if (test_hit_combat(chance + sleeping_bonus, r_ptr->ac, m_ptr->ml))
		{
			/* Sound */
			sound(SOUND_HIT);

			/* Hack -- bare hands do one damage */
			k = 1;

			/* Handle normal weapon */
			if (o_ptr->k_idx)
			{
				/* base damage dice. */
				k = o_ptr->dd;

				/* multiply by slays or brands. (10x inflation) */
				k *= tot_dam_aux(o_ptr, m_ptr);

				/* multiply by critical hit. (10x inflation) */
                k *= critical_melee(chance, sleeping_bonus, m_name, o_ptr, m_ptr);

				/* Convert total Deadliness into a percentage, and apply
				 * it as a bonus or penalty. (100x inflation)
				 */
				if (total_deadliness > 0)
					k *= (100 + deadliness_conversion[total_deadliness]);
				else if (total_deadliness > -31)
					k *= (100 -
					deadliness_conversion[ABS(total_deadliness)]);
				else
					k = 0;

				/* Get the whole number of dice by deflating the result. */
				k_whole = k / 10000;

				/* Calculate the remainder (the fractional die, x10000). */
				k_remainder = k % 10000;


				/* Calculate and combine the damages of the whole and
				 * fractional dice.
				 */
				k = damroll(k_whole, o_ptr->ds) +
					(k_remainder * damroll(1, o_ptr->ds) / 10000);


				/* hack -- check for earthquake. */
				if (p_ptr->impact && (k > 49)) do_quake = TRUE;
			}

			/* Handle dragon's attacks */
			else if (p_ptr->prace >= RACE_MIN_DRAGON)
			{
				/* Message */
                sprintf(message, "%d. You %s %s.", num, num%2?"bite":"claw", m_name);
				msg_print(message);

				/* Get damage dice */
				race_barehand_dice(num, &l, &m);

				/* base damage dice. */
				k = l;

				/* no slays. 10x inflation */
				k *= 10;

				/* multiply by critical hit. (10x inflation) */
                k *= critical_melee(chance, sleeping_bonus, m_name, o_ptr, m_ptr);

				/* Convert total Deadliness into a percentage, and apply
				 * it as a bonus or penalty. (100x inflation)
				 */
				if (total_deadliness > 0)
					k *= (100 + deadliness_conversion[total_deadliness]);
				else if (total_deadliness > -31)
					k *= (100 -
					deadliness_conversion[ABS(total_deadliness)]);
				else
					k = 0;

				/* Get the whole number of dice by deflating the result. */
				k_whole = k / 10000;

				/* Calculate the remainder (the fractional die, x10000). */
				k_remainder = k % 10000;


				/* Calculate and combine the damages of the whole and
				 * fractional dice.
				 */
				k = damroll(k_whole, m) +
					(k_remainder * damroll(1, m) / 10000);

			}

			/* No negative damage */
			if (k < 0) k = 0;

			/* Complex message */
			if (p_ptr->wizard)
			{
				msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
			}

			/* Damage, check for fear and death */
			if (mon_take_hit(cave_m_idx[y][x], k, &fear, NULL)) break;

			/* Confusion attack */
			if (p_ptr->confusing)
			{
				/* Cancel glowing hands */
				p_ptr->confusing = FALSE;

				/* Message */
				if (p_ptr->prace < RACE_MIN_DRAGON)
				{
					msg_print("Your hands stop glowing.");
				}
				else
				{
					msg_print("Your claws stop glowing.");
				}

				/* Confuse the monster */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= (RF3_NO_CONF);
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
		}

		/* Player misses */
		else
		{
			/* Sound */
			sound(SOUND_MISS);

			/* Message */
			msg_format("You miss %s.", m_name);
		}
	}

    /* set the number of blows used */
    blows_used = num;

	/* Hack -- delay fear messages */
	if (fear && m_ptr->ml)
	{
		/* Sound */
		sound(SOUND_FLEE);

		/* Message */
		msg_format("%^s flees in terror!", m_name);
	}


	/* Mega-Hack -- apply earthquake brand */
	if (do_quake)
	{
		int py = p_ptr->py;
		int px = p_ptr->px;

		earthquake(py, px, 5);
	}

    /* Check to see if I still have blows left and a monster */
    py_attack_fractal_aux(blows_used);

}
/* Fractal Attacks.
 * If the player has some attacks left over, he will attack an adjacent
 * monster when he kills the first one.  He will continue to attack
 * adjacent monsters until all the monsters are dead or all of his
 * attacks are used up.
 *
 * Process:
 * 1.  py_attack :
 *      reset the num_blow to normal
 *      keep track of blows left over.
 * 2.  Check to see if we have any attacks left over
 * 3.  Check to see if there is a monster adjacent to us.
 * 4.  Modified py_attack_fractal with:
 *      A. the number of remaining blows inserted.
 *      B. the monster location inserted.
 *     keep track of the number of blows left over.
 * 5.  Go back to #2
 */
void py_attack_fractal(int y, int x, int blows_used)
{
	/* Number of dice, also total damage. */
	s32b k;

	/* The whole and fractional damage dice and their resulting damage. */
	int k_remainder, k_whole;

	/* Bonus to attack if monster is sleeping, for certain classes. */
	int sleeping_bonus = 0;

    /* Blows counts */
    int num = blows_used;

	int bonus, chance, total_deadliness;
	int l, m;

	int blows;

	monster_type *m_ptr;
	monster_race *r_ptr;

	object_type *o_ptr;

	char m_name[80];
	char message[80];

	bool fear = FALSE;

	bool do_quake = FALSE;


	/* Access the monster */
	m_ptr = &m_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];


	/* Disturb the player */
	disturb(0, 0);

	/* Initial blows available. */
    blows = p_ptr->num_blow - blows_used;

	/* If the monster is sleeping and visible, it can be hit
	 * more effectively by some classes. -LM-
	 */
	if ((m_ptr->csleep) && (m_ptr->ml))
	{
		if (p_ptr->pclass == CLASS_ROGUE)
			sleeping_bonus = 10 + 2 * p_ptr->lev / 5;
		else sleeping_bonus = 0;
	}


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


	/* Access the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Initialize. */
	total_deadliness = p_ptr->to_d + o_ptr->to_d;

	/* Paranoia.  Ensure legal table access. */
	if (total_deadliness > 150) total_deadliness = 150;

	/* Calculate the "attack quality".  As BTH_PLUS_ADJ has been reduced
	 * to 1, base skill and modifiers to skill are given equal weight. -LM-
	 */
	bonus = p_ptr->to_h + o_ptr->to_h;
	chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));

    /* Attack once for each legal blow remaining */
    while (blows_used++ < p_ptr->num_blow)
	{
		/* Test for hit */
		if (test_hit_combat(chance + sleeping_bonus, r_ptr->ac, m_ptr->ml))
		{
			/* Sound */
			sound(SOUND_HIT);

			/* Hack -- bare hands do one damage */
			k = 1;

			/* Handle normal weapon */
			if (o_ptr->k_idx)
			{
				/* base damage dice. */
				k = o_ptr->dd;

				/* multiply by slays or brands. (10x inflation) */
				k *= tot_dam_aux(o_ptr, m_ptr);

				/* multiply by critical hit. (10x inflation) */
                k *= critical_melee(chance, sleeping_bonus, m_name, o_ptr, m_ptr);

				/* Convert total Deadliness into a percentage, and apply
				 * it as a bonus or penalty. (100x inflation)
				 */
				if (total_deadliness > 0)
					k *= (100 + deadliness_conversion[total_deadliness]);
				else if (total_deadliness > -31)
					k *= (100 -
					deadliness_conversion[ABS(total_deadliness)]);
				else
					k = 0;

				/* Get the whole number of dice by deflating the result. */
				k_whole = k / 10000;

				/* Calculate the remainder (the fractional die, x10000). */
				k_remainder = k % 10000;


				/* Calculate and combine the damages of the whole and
				 * fractional dice.
				 */
				k = damroll(k_whole, o_ptr->ds) +
					(k_remainder * damroll(1, o_ptr->ds) / 10000);


				/* hack -- check for earthquake. */
				if (p_ptr->impact && (k > 49)) do_quake = TRUE;
			}

			/* Handle dragon's attacks */
			else if (p_ptr->prace >= RACE_MIN_DRAGON)
			{
				/* Message */
                sprintf(message, "You %s %s.", blows_used%2?"bite":"claw", m_name);
				msg_print(message);

				/* Get damage dice */
				race_barehand_dice(num, &l, &m);

				/* base damage dice. */
				k = l;

				/* no slays. 10x inflation */
				k *= 10;

				/* multiply by critical hit. (10x inflation) */
                k *= critical_melee(chance, sleeping_bonus, m_name, o_ptr, m_ptr);

				/* Convert total Deadliness into a percentage, and apply
				 * it as a bonus or penalty. (100x inflation)
				 */
				if (total_deadliness > 0)
					k *= (100 + deadliness_conversion[total_deadliness]);
				else if (total_deadliness > -31)
					k *= (100 -
					deadliness_conversion[ABS(total_deadliness)]);
				else
					k = 0;

				/* Get the whole number of dice by deflating the result. */
				k_whole = k / 10000;

				/* Calculate the remainder (the fractional die, x10000). */
				k_remainder = k % 10000;


				/* Calculate and combine the damages of the whole and
				 * fractional dice.
				 */
				k = damroll(k_whole, m) +
					(k_remainder * damroll(1, m) / 10000);

			}

			/* No negative damage */
			if (k < 0) k = 0;

			/* Complex message */
			if (p_ptr->wizard)
			{
                msg_format("%d. You do %d (out of %d) damage.", blows_used, k, m_ptr->hp);
			}

			/* Damage, check for fear and death */
			if (mon_take_hit(cave_m_idx[y][x], k, &fear, NULL)) break;

			/* Confusion attack */
			if (p_ptr->confusing)
			{
				/* Cancel glowing hands */
				p_ptr->confusing = FALSE;

				/* Message */
				if (p_ptr->prace < RACE_MIN_DRAGON)
				{
					msg_print("Your hands stop glowing.");
				}
				else
				{
					msg_print("Your claws stop glowing.");
				}

				/* Confuse the monster */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= (RF3_NO_CONF);
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
		}

		/* Player misses */
		else
		{
			/* Sound */
			sound(SOUND_MISS);

			/* Message */
			msg_format("You miss %s.", m_name);
		}
	}

	/* Hack -- delay fear messages */
	if (fear && m_ptr->ml)
	{
		/* Sound */
		sound(SOUND_FLEE);

		/* Message */
		msg_format("%^s flees in terror!", m_name);
	}


	/* Mega-Hack -- apply earthquake brand */
	if (do_quake)
	{
		int py = p_ptr->py;
		int px = p_ptr->px;

		earthquake(py, px, 5);
	}

    /* Check to see if I still have blows left and a monster */
    py_attack_fractal_aux(blows_used);
}

/*
 * This routine will check to see if the player still has attacks left
 * over and if so, use them on an adjacent monster.
 */
void py_attack_fractal_aux(int blows_used)
{
    int y, x;
    int b_y = -1, b_x = -1;

    /* Do I have have any attacks left over? */
    if (blows_used >= p_ptr->num_blow) return;

    /* Check for Adjacent Monster */
    /* Search the nearby grids, which contain monsters */
    for (y = (p_ptr->py - 1); y <= (p_ptr->py + 1); y++)
	{
        for (x = (p_ptr->px - 1); x <= (p_ptr->px + 1); x++)
		{

            /* if no monster, skip it */
            if (!(cave_m_idx[y][x] > 0)) continue;

            /* Store the last legal monster */
            b_y = y;
            b_x = x;
        }
    }

    /* Do the attack again */
    if (b_y != -1)
    {
        py_attack_fractal(b_y, b_x, blows_used);
    }
    return;
}


/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should only be called when energy has been expended.
 *
 * Note that this routine handles monsters in the destination grid,
 * and also handles attempting to move into walls/doors/rubble/etc.
 */
void move_player(int dir, int do_pickup)
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
				msg_print("You feel a pile of rubble blocking your way.");
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
			}

			/* Closed door */
			else if (cave_feat[y][x] < FEAT_SECRET)
			{

				msg_print("You feel a door blocking your way.");
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
			}


			/* Wall (or secret door) */
			else
			{
				msg_print("You feel a wall blocking your way.");
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
				msg_print("There is a pile of rubble blocking your way.");
			}

			/* Closed door */
			else if (cave_feat[y][x] < FEAT_SECRET)
			{
				if (easy_open_door(y, x)) return;
			}

			/* Wall (or secret door) */
			else
			{
				msg_print("There is a wall blocking your way.");
			}
		}

		/* Sound */
		sound(SOUND_HITWALL);
	}

	/* Normal movement */
	else
	{
		/* Sound XXX XXX XXX */
		/* sound(SOUND_WALK); */

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
		py_pickup(do_pickup);

		/* Handle "store doors" */
		if ((cave_feat[y][x] >= FEAT_SHOP_HEAD) &&
		    (cave_feat[y][x] <= FEAT_SHOP_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter store */
			p_ptr->command_new = '_';
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
static byte cycle[] =
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static byte chome[] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };



/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. ???
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

			/* Acquire object */
			o_ptr = &o_list[this_o_idx];

			/* Acquire next object */
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

				/* Safety */
				default:
				{
					/* Notice it */
					notice = TRUE;

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

	/* Move the player, using the "pickup" flag */
	move_player(p_ptr->run_cur_dir, always_pickup);
}

