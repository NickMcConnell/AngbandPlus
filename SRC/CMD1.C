/* File: cmd1.c */

/* Purpose: Movement commands (part 1) */

/*
 * Decide if a blow or shot hits, apply critical hits and slay/brands,
 * calculate monk blows.  Searching, pickup, traps (chances & effects),
 * the melee combat algorithm, move one square, and the running algorithm.
 *
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#define MAX_VAMPIRIC_DRAIN 100


/*
 * Determine if the player "hits" a monster (normal combat).
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit_fire(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Instant miss or hit */
	if (k < 10) return (k < 5);

	/* Never hit */
	if (chance <= 0) return (FALSE);

	/* Invisible monsters are harder to hit */
	if (!vis) chance = (chance + 1) / 2;

	/* Power competes against armor */
	if (rand_int(chance) < (ac * 3 / 4)) return (FALSE);

	/* Assume hit */
	return (TRUE);
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

	/* Wimpy attack never hits */
	if (chance <= 0) return (FALSE);

	/* Penalize invisible targets */
	if (!vis) chance = (chance + 1) / 2;

	/* Power must defeat armor */
	if (rand_int(chance) < (ac * 3 / 4)) return (FALSE);

	/* Assume hit */
	return (TRUE);
}



/*
 * Critical hits (from objects thrown by player)
 * Factor in item weight, total plusses, and player level.
 */
s16b critical_shot(int weight, int plus, int dam)
{
	int i, k;

	/* Extract "shot" power */
	i = (weight + ((p_ptr->to_h + plus) * 4) + (p_ptr->lev * 2));

	/* Critical hit */
	if (randint(5000) <= i)
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
 * ~ Critical hits have been majorly altered. 
 # ~ This has been modified to give certain classes access to 'higher'
 # ~ blow powers, giving warriors a bit of an edge in combat and
 # ~ encouraging the use of heavier weapons by them.
 # ~
 # ~ First of all, warriors now have a greater chance of reaching
 # ~ the higher critical hit bonuses with bigger weapons. (this
 # ~ table is *very* dependant on weight for warriors.) other classes
 # ~ have varying degrees of success, but classes like rogues, mages
 # ~ and rangers don't have a good bonus, so they may want to stick
 # ~ with lighter weapons for the extra hits.
 # ~
 # ~ second, these higher critical hits are *huge*. the highest one
 # ~ is 15 * damage + 50. A level 50 warrior with Deathwreaker 
 # ~ (40 pounds) will be able to achieve this with about 8% of all 
 # ~ critical hits, for 155 to 650 (average 470 with a *sharp* curve) 
 # ~ damage, not including bonuses. With a dagger, the best he can manage 
 # ~ is a *GREAT* hit, which happens about 20% of the time, for a whopping 
 # ~ 24 to 28 damage, not including bonuses.
 # ~ Note that warriors will not be able to achieve the highest crit without
 # ~ a weapon weighing at least 36 pounds or so, and Maces of Disruption
 # ~ and Grond are the only weapons weighing this much; and Grond doesn't
 # ~ count (:B
 # ~ Mages, on the other hand, still use the algorithm in standard Angband,
 # ~ Which of course means that he won't ever manage a 'higher' crit without
 # ~ Grond (actually, he can't even with Grond.)
 # ~ -- neko
 */
s16b critical_norm(int weight, int plus, int dam)
{
	int i, k;

	/* Extract "blow" power */
	i = (weight + ((p_ptr->to_h + plus) * 5) + (p_ptr->lev * 3));

	/* Chance */
	if (randint(5000) <= i)
	{
		switch (p_ptr->pclass)
		{
			case CLASS_WARRIOR:
				/* ~ very good bonus, *very* dependant on weight
				 # ~ (i'm writing this section for them)
				 */
				k = (p_ptr->lev *10) + (weight * 2) + randint(1000 + weight * 4);
				break;
			case CLASS_MAGE:
			case CLASS_HIGH_MAGE:
			case CLASS_MONK:
			case CLASS_MINDCRAFTER:
				/* ~ mages don't need no steeenkin' crits! 
				 # ~ this is the same as in standard Angband.
				 */
				k = weight + randint(650);
				break;
			case CLASS_PRIEST:
				/* ~ fair bonus, but quite dependant on weight since
				 # ~ priests tend to use heavy weapons alot (maces, etc)
				 */
				k =( p_ptr->lev * 2) + (weight * 2) + randint(650);
				break;
			case CLASS_ROGUE:
			case CLASS_WARRIOR_MAGE:
				/* ~ Okay bonus (light weapons fit the rogue's idiom more) */
				k = (p_ptr->lev * 5) + weight + randint(650);
				break;
			case CLASS_RANGER:
				/* ~ Again, okay bonus (but slightly better than the rogue's) */
				k = (p_ptr->lev * 5) + weight + randint(650 + weight);
				break;
			case CLASS_PALADIN:
			case CLASS_CHAOS_WARRIOR:
				/* ~ Good, weight-dependant bonus */
				k = (p_ptr->lev * 8) + (weight * 3) + randint(800 + weight * 2);
				break;
			/* ~ paranoia -- non-existant classes get no crits */
			default:
				k = 0;
				break;
		} /* ~ switch */

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
		else if (k < 1700)
		{
			msg_print("It was a *SUPERB* hit!");
			dam = ((7 * dam) / 2) + 25;
		}		
		else if (k < 2500)
		{
			msg_print("It was a *TREMENDOUS* hit!");
			dam = (5 * dam) + 30;
		}
		else if (k < 3000)
		{
			msg_print("It was an *INCREDIBLE* hit!");
			dam = ((15 * dam) / 2) + 35;
		}
		else if (k < 3700)
		{
			msg_print("It was an *UNBELIEVABLE* hit!");
			dam = (10 * dam) + 45;
		}
		else
		{ /* ~ this one may even be too powerful, but it's *rare* */
			msg_print("It was an *EARTH-SHATTERING* hit!");
			dam = (15 * dam) + 50;
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
s16b tot_dam_aux(object_type *o_ptr, int tdam, monster_type *m_ptr)
{
	int mult = 1;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u64b f1, f2, f3;

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
                case TV_AXE:
                case TV_DAGGER:
                case TV_TWO_HANDED:
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
				   (mult < 3)) mult = 3;
				else if (mult < 2) mult = 2;
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
				   (mult < 3)) mult = 3;
				else if (mult < 2) mult = 2;
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
				   (mult < 5)) mult = 5;
				else if (mult < 3) mult = 3;
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
				   (mult < 5)) mult = 5;
				else if (mult < 3) mult = 3;
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
				   (mult < 5)) mult = 5;
				else if (mult < 3) mult = 3;
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
				   (mult < 5)) mult = 5;
				else if (mult < 3) mult = 3;
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
				   (mult < 5)) mult = 5;
				else if (mult < 3) mult = 3;
			}

			/* Slay Dragon  */
			if ((f1 & (TR1_SLAY_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_DRAGON);
				}

				if (mult < 3) mult = 3;
			}

			/* Execute Dragon */
			if ((f1 & (TR1_KILL_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_DRAGON);
				}

				if (mult < 5) mult = 5;

				if ((o_ptr->name1 == ART_AEGLIN) &&
				    strstr(r_name + r_ptr->name, "Fafner"))
					mult *= 3;
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

				/* Notice susceptibility */
				else if (r_ptr->flags9 & (RF9_HURT_ACID))
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags9 |= (RF9_HURT_ACID);
					}
					if (mult < 6) mult = 6;
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
						r_ptr->r_flags3 |= (RF3_IM_ELEC);
					}
				}

				/* Notice susceptibility */
				else if (r_ptr->flags9 & (RF9_HURT_ELEC))
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags9 |= (RF9_HURT_ELEC);
					}
					if (mult < 6) mult = 6;
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
						r_ptr->r_flags3 |= (RF3_IM_FIRE);
					}
				}

				/* Notice susceptibility */
				else if (r_ptr->flags3 & (RF3_HURT_FIRE))
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= (RF3_HURT_FIRE);
					}
					if (mult < 6) mult = 6;
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
						r_ptr->r_flags3 |= (RF3_IM_COLD);
					}
				}

				/* Notice susceptibility */
				else if (r_ptr->flags3 & (RF3_HURT_COLD))
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= (RF3_HURT_COLD);
					}
					if (mult < 6) mult = 6;
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
						r_ptr->r_flags3 |= (RF3_IM_POIS);
					}
				}
				
                                /* Notice susceptibility */
				else if (r_ptr->flags9 & (RF9_HURT_POIS))
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags9 |= (RF9_HURT_POIS);
					}
					if (mult < 6) mult = 4;
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3) mult = 2;
				}
			}
			break;
		}
	}


	/* Return the total damage */
	return (tdam * mult);
}

/*
 * Compute the "total damage" to a monster that resists a certain form
 * of attack.
 */
s16b tot_dam_loss(object_type * o_ptr, int tdam, monster_type * m_ptr)
{
	int div = 1;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Some "weapons" and "ammo" can do less damage */
	switch (o_ptr->tval)
	{
	case TV_SWORD: case TV_DAGGER: case TV_TWO_HANDED:
		{
			if (r_ptr->flags7 & (RF7_RES_SLASH))
			{
				div = 2;
				if (m_ptr->ml)
					r_ptr->r_flags7 |= (RF7_RES_SLASH);
			}
			if (r_ptr->flags7 & (RF7_IM_SLASH))
			{
				div = 6;
				if (m_ptr->ml)
					r_ptr->r_flags7 |= (RF7_IM_SLASH);
			}
			break;
		}
	case TV_HAFTED: case TV_AXE:
	case TV_DIGGING:
		{
			if (r_ptr->flags7 & (RF7_RES_BASH))
			{
				div = 2;
				if (m_ptr->ml)
					r_ptr->r_flags7 |= (RF7_RES_BASH);
			}
			if (r_ptr->flags7 & (RF7_IM_BASH))
			{
				div = 6;
				if (m_ptr->ml)
					r_ptr->r_flags7 |= (RF7_IM_BASH);
			}
			break;
		}
	case TV_POLEARM:
		{
			if (r_ptr->flags7 & (RF7_RES_STAB))
			{
				div = 2;
				if (m_ptr->ml)
					r_ptr->r_flags7 |= (RF7_RES_STAB);
			}
			if (r_ptr->flags7 & (RF7_IM_STAB))
			{
				div = 6;
				if (m_ptr->ml)
					r_ptr->r_flags7 |= (RF7_IM_STAB);
			}
			break;
		}
	}


	/* Return the total damage */
	return (tdam / div);
}

/*
 * Compute the weapon damage based on the weapon's base damage.
 */
static s16b tot_dam_aux3(s16b k, s16b ac) 
{
  int mul = 1, div = 1;

  /* Warriors don't damage their weapon as much as
   * others. */
  if (p_ptr->pclass == CLASS_WARRIOR) 
      div += (p_ptr->skill_thn / 50);

  /* Strong characters will tend to break their weapon. */
  mul += (adj_str_hold[p_ptr->stat_ind[A_STR]] / 30);

  /* Soft monsters don't damage the weapon much. */
  mul += (ac / 25);

  return ((k * mul) / div);

}

/*
 * Search for hidden things
 */
void search(void)
{
	int y, x, chance;

	s16b this_o_idx, next_o_idx = 0;

	cave_type *c_ptr;


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
				/* Access the grid */
				c_ptr = &cave[y][x];

				/* Invisible trap */
				if ((c_ptr->t_idx != 0) &&
				    !(c_ptr->info & CAVE_TRDT))
				{
					/* Pick a trap */
					pick_trap(y, x);

					/* Message */
					msg_print("You have found a trap.");

					/* Disturb */
					disturb(0, 0);
				}

				/* Secret door */
				if (c_ptr->feat == FEAT_SECRET)
				{
					bool locked = FALSE;
					
        				if (dun_level > rand_int(MAX_DEPTH)) 
                                            locked = TRUE;
					
					/* Message */
					msg_print("You have found a secret door.");

					/* Pick a door XXX XXX XXX */
					if (locked)
					cave_set_feat(y, x, FEAT_DOOR_HEAD + randint(7));
					else place_closed_door(y, x);

					/* Disturb */
					disturb(0, 0);
				}

				/* Scan all objects in the grid */
				for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
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
 * Automatically destroy items in this grid.
 */
static void auto_destroy_items(cave_type *c_ptr)
{
	s16b this_o_idx, next_o_idx = 0;

	char o_name[80];

	/* Scan the pile of objects */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Acquire object */
		object_type *o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Known to be worthless? */
		if (auto_destroy && (object_value(o_ptr) < 1))
		{
			/* Artifact? */
			if (!(artifact_p(o_ptr) || o_ptr->art_name))
			{
				/* Describe the object (with {terrible/special}) */
				object_desc(o_name, o_ptr, TRUE, 3);

				/* Message */
				msg_format("You cannot auto-destroy %s.", o_name);

				/* Done */
				return;
			}

			/* Describe the object */
			object_desc(o_name, o_ptr, TRUE, 3);

			/* Print a message */
			msg_format("Auto-destroying %s.", o_name);

			/* Destroy the item */
			delete_object_idx(this_o_idx);

			continue;
		}
	}
}


/*
 * Player "wants" to pick up an object or gold.
 * Note that we ONLY handle things that can be picked up.
 * See "move_player()" for handling of other things.
 */
void carry(int pickup)
{
	cave_type *c_ptr = &cave[py][px];

	s16b this_o_idx, next_o_idx = 0;

	char o_name[80];

	/* Automatically destroy items */
	auto_destroy_items(c_ptr);

#ifdef ALLOW_EASY_FLOOR

	if (easy_floor)
	{
		py_pickup_floor(pickup);
		return;
	}

#endif /* ALLOW_EASY_FLOOR */

	/* Scan the pile of objects */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

#ifdef ALLOW_EASY_SENSE	/* TNB */

		/* Option: Make item sensing easy */
		if (easy_sense)
		{
			/* Sense the object */
			(void) sense_object(o_ptr);
		}

#endif /* ALLOW_EASY_SENSE -- TNB */

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
			msg_format("You collect %ld pieces worth of %s.",
				   (long)o_ptr->pval, o_name);

			sound(SOUND_SELL);

			/* Collect the gold */
			p_ptr->au += o_ptr->pval*o_ptr->number;

			/* Redraw gold */
			p_ptr->redraw |= (PR_GOLD | PR_MAP);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);

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
					int i;

					/* Carry the item */
					slot = inven_carry(o_ptr, FALSE);

					/* Get the item again */
					o_ptr = &inventory[slot];

					/* Describe the object */
					object_desc(o_name, o_ptr, TRUE, 3);

					/* Message */
					msg_format("You have %s (%c).", o_name, index_to_label(slot));

					/* Check if completed a quest */
					for (i = 0; i < max_quests; i++)
					{
						if ((quest[i].type == 3) && (quest[i].status == 1) &&
						    (quest[i].k_idx == o_ptr->name1) && (o_ptr->name1 > 0))
						{
							quest[i].status = QUEST_STATUS_COMPLETED;
							msg_print("You completed your quest!");
							msg_print(NULL);
						}
					}

					/* Delete the object */
					delete_object_idx(this_o_idx);
				}
			}
		}
	}
}






/*
 * Handle player hitting a real trap
 */
static void hit_trap(void)
{
	bool ident=FALSE;

	cave_type *c_ptr;

	/* Disturb the player */
	disturb(0, 0);

	/* Get the cave grid */
	c_ptr = &cave[py][px];
	if (c_ptr->t_idx != 0)
	{
		ident = player_activate_trap_type(py, px, NULL, -1);
		if (ident)
		{
			t_info[c_ptr->t_idx].ident = TRUE;
			msg_format("You identified the trap as %s.",
				   t_name + t_info[c_ptr->t_idx].name);
		}
	}
}


void touch_zap_player(monster_type *m_ptr)
{
	int aura_damage = 0;
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	if (r_ptr->flags2 & (RF2_AURA_FIRE))
	{
		if (!(p_immune_fire))
		{
			char aura_dam[80];

			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

			/* Hack -- Get the "died from" name */
			monster_desc(aura_dam, m_ptr, 0x88);

			msg_print("You are suddenly very hot!");

			if (p_ptr->oppose_fire) aura_damage = (aura_damage+2) / 3;
			if (p_resist_fire) aura_damage = (aura_damage+2) / 3;
                        if (p_sensible_fire) aura_damage = aura_damage * 2;

			take_hit(aura_damage, aura_dam);
			r_ptr->r_flags2 |= RF2_AURA_FIRE;
			handle_stuff();
		}
	}

	if (r_ptr->flags3 & (RF3_AURA_COLD))
	{
		if (!(p_immune_cold))
		{
			char aura_dam[80];

			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

			/* Hack -- Get the "died from" name */
			monster_desc(aura_dam, m_ptr, 0x88);

			msg_print("You are suddenly very cold!");

			if (p_ptr->oppose_cold) aura_damage = (aura_damage+2) / 3;
			if (p_resist_cold) aura_damage = (aura_damage+2) / 3;
                        if (p_sensible_cold) aura_damage = aura_damage * 2;

			take_hit(aura_damage, aura_dam);
			r_ptr->r_flags3 |= RF3_AURA_COLD;
			handle_stuff();
		}
	}

	if (r_ptr->flags2 & (RF2_AURA_ELEC))
	{
		if (!(p_immune_elec))
		{
			char aura_dam[80];

			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

			/* Hack -- Get the "died from" name */
			monster_desc(aura_dam, m_ptr, 0x88);

			if (p_ptr->oppose_elec) aura_damage = (aura_damage+2) / 3;
			if (p_resist_elec) aura_damage = (aura_damage+2) / 3;
                        if (p_sensible_elec) aura_damage = aura_damage * 2;

			msg_print("You get zapped!");
			take_hit(aura_damage, aura_dam);
			r_ptr->r_flags2 |= RF2_AURA_ELEC;
			handle_stuff();
		}
	}

	if (r_ptr->flags2 & (RF2_AURA_FEAR))
	{
		if (!(p_resist_fear))
		{
			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));
			set_afraid(aura_damage);
			r_ptr->r_flags2 |= RF2_AURA_FEAR;
			handle_stuff();
		}
	}
}

static void natural_attack(s16b m_idx, int attack, bool *fear, bool *mdeath)
{
	int             k, bonus, chance;
	int             n_weight = 0;
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	char            m_name[80];

	int             dss, ddd;

	char            *atk_desc;

	switch (attack)
	{
		case MUT2_SCOR_TAIL:
			dss = 3;
			ddd = 7;
			n_weight = 5;
			atk_desc = "tail";
			break;
		case MUT2_HORNS:
			dss = 2;
			ddd = 6;
			n_weight = 15;
			atk_desc = "horns";
			break;
		case MUT2_BEAK:
			dss = 2;
			ddd = 4;
			n_weight = 5;
			atk_desc = "beak";
			break;
		case MUT2_TRUNK:
			dss = 1;
			ddd = 4;
			n_weight = 35;
			atk_desc = "trunk";
			break;
		case MUT2_TENTACLES:
			dss = 2;
			ddd = 5;
			n_weight = 5;
			atk_desc = "tentacles";
			break;
		default:
			dss = ddd = n_weight = 1;
			atk_desc = "undefined body part";
	}

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);


	/* Calculate the "attack quality" */
	bonus = p_ptr->to_h;
	chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));

	/* Test for hit */
	if (test_hit_norm(chance, r_ptr->ac, m_ptr->ml))
	{
		/* Sound */
		sound(SOUND_HIT);

		msg_format("You hit %s with your %s.", m_name, atk_desc);

		k = damroll(ddd, dss);
		k = critical_norm(n_weight, p_ptr->to_h, k);

		/* Apply the player damage bonuses */
		k += p_ptr->to_d;

		/* No negative damage */
		if (k < 0) k = 0;

		/* Complex message */
		if (wizard)
		{
			msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
		}

		if (!is_hostile(m_ptr))
		{
			msg_format("%^s gets angry!", m_name);
			set_hostile(m_ptr);
		}

		/* Damage, check for fear and mdeath */
		switch (attack)
		{
			case MUT2_SCOR_TAIL:
				project(0, 0, m_ptr->fy, m_ptr->fx, k, GF_POIS, PROJECT_KILL);
				*mdeath = (m_ptr->r_idx == 0);
				break;
			case MUT2_HORNS:
				*mdeath = mon_take_hit(m_idx, k, fear, NULL);
				break;
			case MUT2_BEAK:
				*mdeath = mon_take_hit(m_idx, k, fear, NULL);
				break;
			case MUT2_TRUNK:
				*mdeath = mon_take_hit(m_idx, k, fear, NULL);
				break;
			case MUT2_TENTACLES:
				*mdeath = mon_take_hit(m_idx, k, fear, NULL);
				break;
			default:
				*mdeath = mon_take_hit(m_idx, k, fear, NULL);
		}

		touch_zap_player(m_ptr);
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

/*
 * Fetch an attack description from dam_*.txt files.
 */

static void flavored_attack(int percent, char *output) {

  if (percent < 5) {
    if (!flavored_attacks) strcpy(output, "You scratch %s.");
    else get_rnd_line("dam_none.txt", output);
  } else if (percent < 30) {
    if (!flavored_attacks) strcpy(output, "You hit %s.");
    else get_rnd_line("dam_med.txt", output);
  } else if (percent < 60) {
    if (!flavored_attacks) strcpy(output, "You wound %s.");
    else get_rnd_line("dam_lots.txt", output);
  } else if (percent < 95) {
    if (!flavored_attacks) strcpy(output, "You cripple %s.");
    else get_rnd_line("dam_huge.txt", output);
  } else {
    if (!flavored_attacks) strcpy(output, "You demolish %s.");
    else get_rnd_line("dam_xxx.txt", output);
  }

}


/*
 * Carried monster can attack too.
 * Based on monst_attack_monst.
 */
static void polymorphed_attack(s16b m_idx, bool *fear, bool *mdeath, int x, int y, int num)
{
        monster_type    *t_ptr = &m_list[m_idx];
        monster_race    *r_ptr;
        monster_race    *tr_ptr = &r_info[t_ptr->r_idx];
        cave_type       *c_ptr;
	int             ac, rlev, pt, effect, method, d_dice, d_side;
        char            t_name[80];
        char            temp[80];
	bool            blinked = FALSE, touched = FALSE;
	byte            y_saver = t_ptr->fy;
	byte            x_saver = t_ptr->fx;
	bool            visible = FALSE;
	bool            obvious = FALSE;
        int             power = 0;
	int             damage = 0;
	cptr            act = NULL;

        r_ptr = &r_info[p_ptr->polymon];

        /* Extract the attack infomation */
	effect = r_ptr->blow[num].effect;
	method = r_ptr->blow[num].method;
	d_dice = r_ptr->blow[num].d_dice;
	d_side = r_ptr->blow[num].d_side;

        c_ptr = &cave[y][x];

	/* Not allowed to attack */
        if (r_ptr->flags1 & RF1_NEVER_BLOW) return;

	/* Total armor */
	ac = tr_ptr->ac;

	/* Extract the effective monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	/* Get the monster name (or "it") */
	monster_desc(t_name, t_ptr, 0);

	/* Assume no blink */
	blinked = FALSE;

        /* No longer in LOS? */
        if (!t_ptr->ml) return;

	/* Stop attacking if the target dies! */
	if (t_ptr->fx != x_saver || t_ptr->fy != y_saver)
		return;

	/* Hack -- no more attacks */
	if (!method) return;

	if (blinked) /* Stop! */
	{
		return;
	}

	/* Extract visibility (before blink) */
        visible = TRUE;

	/* Extract the attack "power" */
	switch (effect)
	{
	case RBE_HURT:          power = 20; break;
	case RBE_POISON:        power =  5; break;
	case RBE_UN_BONUS:      power = 20; break;
	case RBE_UN_POWER:      power = 15; break;
	case RBE_EAT_GOLD:      power =  5; break;
	case RBE_EAT_ITEM:      power =  5; break;
	case RBE_EAT_FOOD:      power =  5; break;
	case RBE_EAT_LITE:      power =  5; break;
	case RBE_ACID:          power =  0; break;
	case RBE_ELEC:          power = 10; break;
	case RBE_FIRE:          power = 10; break;
	case RBE_COLD:          power = 10; break;
	case RBE_BLIND:         power =  2; break;
	case RBE_CONFUSE:       power = 10; break;
	case RBE_TERRIFY:       power = 10; break;
	case RBE_PARALYZE:      power =  2; break;
	case RBE_LOSE_STR:      power =  0; break;
	case RBE_LOSE_DEX:      power =  0; break;
	case RBE_LOSE_CON:      power =  0; break;
	case RBE_LOSE_INT:      power =  0; break;
	case RBE_LOSE_WIS:      power =  0; break;
	case RBE_LOSE_CHR:      power =  0; break;
	case RBE_LOSE_ALL:      power =  2; break;
	case RBE_SHATTER:       power = 20; break;
	case RBE_EXP_10:        power =  5; break;
	case RBE_EXP_20:        power =  5; break;
	case RBE_EXP_40:        power =  5; break;
	case RBE_EXP_80:        power =  5; break;
	case RBE_DISEASE:       power =  5; break;
	case RBE_TIME:          power =  5; break;
        case RBE_INSANITY:      power = 20; break;
        case RBE_LOSE_LUC:      power =  0; break;
        case RBE_HALLU:         power = 10; break;
        case RBE_HUNGER:        power =  5; break;
        case RBE_DEATH:         power = 10; break;
        case RBE_STONE:         power =  0; break;
	}
	
	
	/* Monster hits*/
	if (!effect || check_hit2(power, rlev, ac))
	{
		/* Always disturbing */
		disturb(1, 0);
		
		/* Describe the attack method */
		switch (method)
		{
			case RBM_HIT:
				{
                                       act = "hit %s.";
					touched = TRUE;
					break;
				}
				
			case RBM_TOUCH:
				{
                                       act = "touch %s.";
					touched = TRUE;
					break;
				}
				
			case RBM_PUNCH:
				{
                                       act = "punch %s.";
					touched = TRUE;
					break;
				}
				
			case RBM_KICK:
				{
                                       act = "kick %s.";
					touched = TRUE;
					break;
				}
				
			case RBM_CLAW:
				{
                                       act = "claw %s.";
					touched = TRUE;
					break;
				}
				
			case RBM_BITE:
				{
                                       act = "bite %s.";
					touched = TRUE;
					break;
				}
				
			case RBM_STING:
				{
                                       act = "sting %s.";
					touched = TRUE;
					break;
				}
				
			case RBM_BUTT:
				{
                                       act = "butt %s.";
					touched = TRUE;
					break;
				}
				
			case RBM_CRUSH:
				{
                                       act = "crush %s.";
					touched = TRUE;
					break;
				}
				
			case RBM_ENGULF:
				{
                                       act = "engulf %s.";
					touched = TRUE;
					break;
				}
				
			case RBM_CHARGE:
				{
                                       act = "charge %s.";
					touched = TRUE;
					break;
				}
				
			case RBM_CRAWL:
				{
                                       act = "crawl on %s.";
					touched = TRUE;
					break;
				}
				
			case RBM_DROOL:
				{
                                       act = "drool on %s.";
					touched = FALSE;
					break;
				}
				
			case RBM_SPIT:
				{
                                       act = "spit on %s.";
					touched = FALSE;
					break;
				}
				
			case RBM_GAZE:
				{
                                       act = "gaze at %s.";
					touched = FALSE;
					break;
				}
				
			case RBM_WAIL:
				{
                                       act = "wail at %s.";
					touched = FALSE;
					break;
				}
				
			case RBM_SPORE:
				{
                                       act = "release spores at %s.";
					touched = FALSE;
					break;
				}

			case RBM_EXPLODE:
				{
                                       act = "explode at %s.";
					touched = FALSE;
					break;
				}
				
				
			case RBM_BEG:
				{
                                       act = "beg %s for money.";
					touched = FALSE;
					t_ptr->csleep = 0;
					break;
				}
				
			case RBM_INSULT:
				{
                                       act = "insult %s.";
					touched = FALSE;
					t_ptr->csleep = 0;
					break;
				}
				
			case RBM_MOAN:
				{
                                       act = "moan at %s.";
					touched = FALSE;
					t_ptr->csleep = 0;
					break;
				}
				
			case RBM_SHOW:
				{
                                       act = "sing to %s.";
					touched = FALSE;
					t_ptr->csleep = 0;
					break;
				}
			}
			
			/* Message */
			if (act)
			{
				strfmt(temp, act, t_name);
                               if (t_ptr->ml)
                                       msg_format("You %s", temp);
			}
			
			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;
			
			/* Roll out the damage */
			damage = damroll(d_dice, d_side);
			
			pt = GF_MISSILE;
			
			/* Apply appropriate damage */
			switch (effect)
			{
			case 0:
				{
					damage = 0;
					pt  = 0;
					break;
				}
				
			case RBE_HURT:
                        case RBE_INSANITY:
				{
					damage -= (damage * ((ac < 150) ? ac : 150) / 250);
					break;
				}
				
			case RBE_POISON:
			case RBE_DISEASE:
				{
					pt = GF_POIS;
					break;
				}
				
			case RBE_UN_BONUS:
			case RBE_UN_POWER:
				{
					pt = GF_DISENCHANT;
					break;
				}
				
			case RBE_EAT_FOOD:
			case RBE_EAT_LITE:
				{
					pt = damage = 0;
					break;
				}
				
			case RBE_EAT_ITEM:
			case RBE_EAT_GOLD:
				{
					pt = damage = 0;
					if (randint(2)==1) blinked = TRUE;
					break;
				}
				
			case RBE_ACID:
				{
					pt = GF_ACID;
					break;
				}
				
			case RBE_ELEC:
				{
					pt = GF_ELEC;
					break;
				}
				
			case RBE_FIRE:
				{
					pt = GF_FIRE;
					break;
				}
				
			case RBE_COLD:
				{
					pt = GF_COLD;
					break;
				}
				
			case RBE_HUNGER:
				{
					break;
				}

			case RBE_STONE:
				{
				        pt = GF_STONE;
				}
								
			case RBE_CONFUSE:
			case RBE_HALLU:
			case RBE_BLIND:
				{
					pt = GF_CONFUSION;
					break;
				}
				
			case RBE_TERRIFY:
				{
					pt = GF_TURN_ALL;
					break;
				}
				
			case RBE_PARALYZE:
				{
					pt = GF_OLD_SLEEP; /* sort of close... */
					break;
				}
				
			case RBE_LOSE_STR:
			case RBE_LOSE_INT:
			case RBE_LOSE_WIS:
			case RBE_LOSE_DEX:
			case RBE_LOSE_CON:
			case RBE_LOSE_CHR:
			case RBE_LOSE_LUC:
			case RBE_LOSE_ALL:
				{
					break;
				}
			case RBE_SHATTER:
				{
					if (damage > 23)
					{
						/* Prevent destruction of quest levels and town */
						if (!is_quest(dun_level) && dun_level)
                                                       earthquake(py, px, 8);
					}
					break;
				}
			case RBE_EXP_10:
			case RBE_EXP_20:
			case RBE_EXP_40:
			case RBE_EXP_80:
				{
					pt = GF_NETHER;
					break;
				}
			case RBE_TIME:
				{
					pt = GF_TIME;
					break;
				}
			default:
				{
					pt = 0;
					break;
				}
			}

			if (pt)
			{
                               /* Do damage if not exploding */
                               project(0, 0, t_ptr->fy, t_ptr->fx,
                                       (pt == GF_OLD_SLEEP ? p_ptr->lev * 2 : damage), pt, PROJECT_KILL | PROJECT_STOP);

				
				if (touched)
				{
					/* Aura fire */
					if ((tr_ptr->flags2 & RF2_AURA_FIRE) &&
						!(r_ptr->flags3 & RF3_IM_FIRE))
					{
                                               if (t_ptr->ml)
						{
							blinked = FALSE;
                                                       msg_format("You are suddenly very hot!");
							if(t_ptr->ml)
								tr_ptr->r_flags2 |= RF2_AURA_FIRE;
						}
                                               project(m_idx, 0, py, px,
							damroll (1 + ((tr_ptr->level) / 26),
							1 + ((tr_ptr->level) / 17)),
							GF_FIRE, PROJECT_KILL | PROJECT_STOP);
					}

					/* Aura elec */
					if ((tr_ptr->flags2 & (RF2_AURA_ELEC)) && 
                                               !(r_ptr->flags3 & (RF3_IM_ELEC)))
					{
                                               if (t_ptr->ml)
						{
							blinked = FALSE;
                                                       msg_format("You get zapped!");
							if(t_ptr->ml)
								tr_ptr->r_flags2 |= RF2_AURA_ELEC;
						}
                                               project(m_idx, 0, py, px,
							damroll (1 + ((tr_ptr->level) / 26),
							1 + ((tr_ptr->level) / 17)),
							GF_ELEC, PROJECT_KILL | PROJECT_STOP);
					}
                                       
					/* Aura elec */
					if ((tr_ptr->flags3 & (RF3_AURA_COLD)) && 
                                               !(r_ptr->flags3 & (RF3_IM_COLD)))
					{
                                               if (t_ptr->ml)
						{
							blinked = FALSE;
                                                       msg_format("You get zapped!");
							if(t_ptr->ml)
								tr_ptr->r_flags3 |= RF3_AURA_COLD;
						}
                                               project(m_idx, 0, py, px,
							damroll (1 + ((tr_ptr->level) / 26),
							1 + ((tr_ptr->level) / 17)),
							GF_COLD, PROJECT_KILL | PROJECT_STOP);
					}
					
				}
			}

		/* Complex message */
		if (wizard)
		{
			msg_format("You did %d (out of %d) damage.", damage, t_ptr->hp);
		}
	}	

		/* Missed */
		else
		{
			/* Analyze failed attacks */
			switch (method)
			{
			case RBM_HIT:
			case RBM_TOUCH:
			case RBM_PUNCH:
			case RBM_KICK:
			case RBM_CLAW:
			case RBM_BITE:
			case RBM_STING:
			case RBM_BUTT:
			case RBM_CRUSH:
			case RBM_ENGULF:
			case RBM_CHARGE:
				{				
						/* Disturbing */
						disturb(1, 0);
						
						/* Message */
                                               msg_format("You miss %s.", t_name);
					break;
				}
			}
		}
		
		
		/* Analyze "visible" monsters only */
		if (visible)
		{
			/* Count "obvious" attacks (and ones that cause damage) */
			if (obvious || damage || (r_ptr->r_blows[num] > 10))
			{
				/* Count attacks of this type */
				if (r_ptr->r_blows[num] < MAX_UCHAR)
				{
					r_ptr->r_blows[num]++;
				}
			}
		}
	

	/* Blink away */
	if (blinked)
	{
                msg_print("You flee laughing!");
		
                teleport_player(MAX_SIGHT * 2 + 5);
	}
}


/*
 * Player attacks a (poor, defenseless) creature        -RAK-
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
void py_attack(int y, int x)
{
	int             num = 0, k = 0, bonus, chance, fx, fy;
	int             base = 0;

	cave_type       *c_ptr = &cave[y][x];

	monster_type    *m_ptr = &m_list[c_ptr->m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];

	object_type     *o_ptr;

	char            m_name[80];

	bool            fear = FALSE;
	bool            mdeath = FALSE;

	bool            backstab = FALSE;
	bool            vorpal_cut = FALSE;
	int             chaos_effect = 0;
	int             terrain_bonus = 0;
	bool            stab_fleeing = FALSE;
	bool            do_quake = FALSE;
	bool		do_pois = FALSE;
	bool            drain_msg = TRUE;
	int             drain_result = 0, drain_heal = 0;
	int             drain_left = MAX_VAMPIRIC_DRAIN;
	u64b            f1, f2, f3; /* A massive hack -- life-draining weapons */
	bool            no_extra = FALSE, clone = FALSE;


	/* Disturb the player */
	disturb(0, 0);

        if (r_info[p_ptr->polymon].flags1 & RF1_NEVER_BLOW)
        {
                msg_print("You cannot attack in this form!");
                return;
        }

	/* Access the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	object_flags(o_ptr, &f1, &f2, &f3);

	if (p_ptr->pclass == CLASS_ROGUE && (o_ptr->tval == TV_SWORD || 
             o_ptr->tval == TV_DAGGER))
	{
		if ((m_ptr->csleep) && (m_ptr->ml))
		{
			/* Can't backstab creatures that we can't see, right? */
			backstab = TRUE;
		}
		else if ((m_ptr->monfear) && (m_ptr->ml))
		{
			stab_fleeing = TRUE;
		}
	}

	/* Disturb the monster */
	m_ptr->csleep = 0;

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Auto-Recall if possible and visible */
	if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

	/* Track a new monster */
	if (m_ptr->ml) health_track(c_ptr->m_idx);

	/* Stop if friendly */
	if (!is_hostile(m_ptr) &&
	    ! (p_ptr->stun || p_ptr->confused || p_ptr->image ||
	    ((p_ptr->muta2 & MUT2_BERS_RAGE) && p_ptr->shero) ||
	    !(m_ptr->ml)))
	{
		if (!(inventory[INVEN_WIELD].art_name))
		{
			msg_format("You stop to avoid hitting %s.", m_name);
			return;
		}

		if (!(streq(quark_str(inventory[INVEN_WIELD].art_name), "'Stormbringer'")))
		{
			msg_format("You stop to avoid hitting %s.", m_name);
			return;
		}

		msg_format("Your black blade greedily attacks %s!", m_name);
	}

	fy = m_ptr->fy;
	fx = m_ptr->fx;

	/* Monsters in rubble can take advantage of cover. -LM- */
	if (cave[fy][fx].feat == FEAT_RUBBLE)
	{
		terrain_bonus = r_ptr->ac / 7 + 5;
	}
	/* Monsters in trees can take advantage of cover, except from rangers. -LM- */
	if ((cave[fy][fx].feat == FEAT_TREES) && (p_ptr->pclass != CLASS_RANGER))
	{
		terrain_bonus = r_ptr->ac / 7 + 5;
	}
	/* Monsters in water are vulnerable.  Make certain this value is not less than zero. -LM- */
	if (cave[fy][fx].feat == FEAT_DEEP_WATER && !(r_ptr->flags7 & RF7_AQUATIC))
	{
		terrain_bonus = -(r_ptr->ac / 4);
	}

	/* Handle player fear */
	if (p_ptr->afraid)
	{
		/* Message */
		if (m_ptr->ml)
		{
			char whine[80];

			get_rnd_line("pfear.txt", whine);
			msg_format("%^s", whine);
		}
		else
			msg_format ("There is something scary in your way!");

		/* Done */
		return;
	}

	/* Calculate the "attack quality" */
	bonus = p_ptr->to_h + o_ptr->to_h;
	chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));


	/* Attack once for each legal blow */
	while (num++ < p_ptr->num_blow)
	{
                /* Hit as a polymorphed monster */
                if ((p_ptr->polymon) && (!o_ptr->k_idx))
                {
                   polymorphed_attack (c_ptr->m_idx, &fear, &mdeath, y, x, num - 1);
                }
		/* Test for hit */
		else if (test_hit_norm(chance, r_ptr->ac + terrain_bonus, m_ptr->ml))
		{
			/* Sound */
			sound(SOUND_HIT);

			if (backstab)
				msg_format("You cruelly stab the helpless, sleeping %s!",
				    (r_name + r_info[m_ptr->r_idx].name));
			if (stab_fleeing)
				msg_format("You backstab the fleeing %s!",
				    (r_name + r_info[m_ptr->r_idx].name));

			/* Hack -- bare hands do one damage */
			k = 1;

			/* Select a chaotic effect (50% chance) */
			if ((f1 & TR1_CHAOTIC) && (randint(2)==1))
			{
				if (randint(5) < 3)
				{
					/* Vampiric (20%) */
					chaos_effect = 1;
				}
				else if (randint(250) == 1)
				{
					/* Quake (0.12%) */
					chaos_effect = 2;
				}
				else if (randint(10) != 1)
				{
					/* Confusion (26.892%) */
					chaos_effect = 3;
				}
				else if (randint(2) == 1)
				{
					/* Teleport away (1.494%) */
					chaos_effect = 4;
				}
				else
				{
					/* Polymorph (1.494%) */
					chaos_effect = 5;
				}
			}

			/* Select a cloning effect (10% chance) */
			if ((f1 & TR3_CLONE_CURSE) && (randint(10)==1))
			{
                                clone = TRUE;
			}


			/* Vampiric drain */
			if ((f1 & TR1_VAMPIRIC) || (chaos_effect == 1))
			{
				if (monster_living(r_ptr))
					drain_result = m_ptr->hp;
				else
					drain_result = 0;
			}

			if (f1 & TR1_VORPAL && (randint((o_ptr->name1 == ART_VORPAL_BLADE)?3:6) == 1))
				vorpal_cut = TRUE;
			if ((p_ptr->sign == SIGN_SCORPIO) && (randint(200)==1))
				vorpal_cut = TRUE;			
			else vorpal_cut = FALSE;

			if (f1 & TR1_BRAND_POIS) do_pois = TRUE;
		
			if (p_ptr->pclass == CLASS_MONK && monk_empty_hands())
			{
				int special_effect = 0, stun_effect = 0, times = 0;
				martial_arts * ma_ptr = &ma_blows[0], * old_ptr = &ma_blows[0];
				int resist_stun = 0;
				if (r_ptr->flags1 & RF1_UNIQUE) resist_stun += 88;
				if (r_ptr->flags3 & RF3_NO_CONF) resist_stun += 44;
				if (r_ptr->flags3 & RF3_NO_SLEEP) resist_stun += 44;
				if ((r_ptr->flags3 & RF3_UNDEAD) || (r_ptr->flags3 & RF3_NONLIVING))
					resist_stun += 88;

				for (times = 0; times < (p_ptr->lev<7?1:p_ptr->lev/7); times++)
				/* Attempt 'times' */
				{
					do
					{
						ma_ptr = &ma_blows[(rand_int(MAX_MA))];
					}
					while ((ma_ptr->min_level > p_ptr->lev)
					    || (randint(p_ptr->lev)<ma_ptr->chance));

					/* keep the highest level attack available we found */
					if ((ma_ptr->min_level > old_ptr->min_level) &&
					    !(p_ptr->stun || p_ptr->confused))
					{
						old_ptr = ma_ptr;

						if (wizard && cheat_xtra)
						{
							msg_print("Attack re-selected.");
						}
					}
					else
					{
						ma_ptr = old_ptr;
					}
				}

				k = damroll(ma_ptr->dd, ma_ptr->ds);

				if (ma_ptr->effect == MA_KNEE)
				{
					if (r_ptr->flags1 & RF1_MALE)
					{
						msg_format("You hit %s in the groin with your knee!", m_name);
						sound(SOUND_PAIN);
						special_effect = MA_KNEE;
					}
					else
						msg_format(ma_ptr->desc, m_name);
				}

				else if (ma_ptr->effect == MA_SLOW)
				{
					if (!((r_ptr->flags1 & RF1_NEVER_MOVE)
					    || strchr("UjmeEv$,DdsbBFIJQSXclnw!=?~", r_ptr->d_char)))
					{
						msg_format("You kick %s in the ankle.", m_name);
						special_effect = MA_SLOW;
					}
					else msg_format(ma_ptr->desc, m_name);
				}
				else
				{
					if (ma_ptr->effect)
					{
						stun_effect = (ma_ptr->effect/2) + randint(ma_ptr->effect/2);
					}

					msg_format(ma_ptr->desc, m_name);
				}

				k = critical_norm(p_ptr->lev * (randint(10)), ma_ptr->min_level, k);

				if ((special_effect == MA_KNEE) && ((k + p_ptr->to_d) < m_ptr->hp))
				{
					msg_format("%^s moans in agony!", m_name);
					stun_effect = 7 + randint(13);
					resist_stun /= 3;
				}

				else if ((special_effect == MA_SLOW) && ((k + p_ptr->to_d) < m_ptr->hp))
				{
					if (!(r_ptr->flags1 & RF1_UNIQUE) &&
					    (randint(p_ptr->lev) > r_ptr->level) &&
					    m_ptr->mspeed > 60)
					{
						msg_format("%^s starts limping slower.", m_name);
						m_ptr->mspeed -= 10;
					}
				}

				if (stun_effect && ((k + p_ptr->to_d) < m_ptr->hp))
				{
					if (p_ptr->lev > randint(r_ptr->level + resist_stun + 10))
					{
						if (m_ptr->stunned)
							msg_format("%^s is more stunned.", m_name);
						else
							msg_format("%^s is stunned.", m_name);

						m_ptr->stunned += stun_effect;
					}
				}
							
			}

			/* Handle normal weapon */
			else if (o_ptr->k_idx)
			{
				k = damroll(o_ptr->dd, o_ptr->ds);
				base = k;
				k = tot_dam_aux(o_ptr, k, m_ptr);

				if (backstab)
				{
					backstab = FALSE;
					k *= (3 + (p_ptr->lev / 40));
				}
				else if (stab_fleeing)
				{
					k = ((3 * k) / 2);
				}

				if ((p_impact && ((k > 50) || randint(7)==1)) ||
				    (chaos_effect == 2))
				{
					do_quake = TRUE;
				}

				k = critical_norm(o_ptr->weight, o_ptr->to_h, k);

				if (vorpal_cut)
				{
        				/*
					* The vorpal blade does average:
					*	(e+2)/3 x normal damage.
					* A normal weapon with the vorpal flag does average:
					*   e-3/2 x normal damage.
					* Note: this has changed from before - the vorpal blade
					*  has been toned down because of the oangband based
					*  combat.
					*/
					   
					
					
					int mult = 2;

					int inc_chance = (o_ptr->name1 == ART_VORPAL_BLADE) ? 2 : 4;

					if ((o_ptr->name1 == ART_ELVAGIL) && (randint(2) != 1))
					{
						char chainsword_noise[1024];
						if (!get_rnd_line("chainswd.txt", chainsword_noise))
						{
							msg_print(chainsword_noise);
						}
					}

					if (o_ptr->name1 == ART_VORPAL_BLADE)
					{
						msg_print("Your Vorpal Blade goes snicker-snack!");
					}
					else
					{
						msg_format("Your weapon cuts deep into %s!", m_name);
					}

					/* Try to increase the damage */
					while (one_in_(inc_chance))
					{
						mult++;
						inc_chance++;
					}

					k *= mult;

					/* Ouch! */
					if (k > m_ptr->hp)
					{
						msg_format("You cut %s in half!", m_name);
					}
					else
					{
						switch(mult)
						{
							case 2:	msg_format("You gouge %s!", m_name);		break;
							case 3:	msg_format("You maim %s!", m_name);			break;
							case 4:	msg_format("You carve %s!", m_name);		break;
							case 5:	msg_format("You cleave %s!", m_name);		break;
							case 6:	msg_format("You smite %s!", m_name);		break;
							case 7:	msg_format("You eviscerate %s!", m_name);	break;
							default:	msg_format("You shred %s!", m_name);		break;
						}
					}
				}

				k += o_ptr->to_d;
			}

			/* Apply the player damage bonuses */
			k += p_ptr->to_d;
			k = tot_dam_loss(o_ptr, k, m_ptr);

                        /* Penalty for could-2H when having a shield */
                        
                        if ((f1 & TR1_COULD2H) && inventory[INVEN_ARM].k_idx)
                             k /= 2;

			/* No negative damage */
			if (k < 0) k = 0;

			if (!(strchr("evwjmlQX,.*|(+/!?", r_ptr->d_char)))
			{
			   if (!(p_ptr->pclass == CLASS_MONK && monk_empty_hands()))
			   {
			   char  buff[255];
			   flavored_attack((100*k)/m_ptr->maxhp, buff);
			   msg_format(buff, m_name);
			   }
			}
			else
			   if (!(p_ptr->pclass == CLASS_MONK && monk_empty_hands()))
			      msg_format("You hit %s.", m_name);

		        if (do_pois && ((k + p_ptr->to_d) < m_ptr->hp) && 
		           !(r_ptr->flags3 & RF3_IM_POIS))
			{
			    if (p_ptr->lev > randint(r_ptr->level / 2))
			    {
				if (m_ptr->monpois)
					msg_format("%^s is more poisoned.", m_name);
				else
					msg_format("%^s is poisoned.", m_name);
					m_ptr->monpois += (randint(10) + k / 10) > 200 ?
							    200 : randint(10 + k / 10);
			    }
			}

                        if (clone) fire_ball (GF_OLD_CLONE, 5, 0, 2);

        		/* Complex message */
			if (wizard)
			{
				msg_format("You did %d (out of %d) damage.", k, m_ptr->hp);
			}

			/* Damage the weapon by a factor of base damage.
			   Take into account player's strength, monster's AC.
			   This prevents obscene damage from rings of
			   damage etc. -GSN- */

			if (o_ptr)
			{
			  int tmp = tot_dam_aux3(base, r_ptr->ac);

			  object_take_hit(o_ptr, tmp, "deteriorated");

			  if (wizard && ironman_damage_items)
			  {
			    msg_format("Weapon took %d damage.", tmp);
			  }
			}
                        
                        /* Time passes... */
                        
                        sc_time += randint(o_ptr->weight / 30);

			/* Damage, check for fear and death */
			if (mon_take_hit(c_ptr->m_idx, k, &fear, NULL))
			{
				mdeath = TRUE;
				break;
			}

			if (!is_hostile(m_ptr))
			{
				msg_format("%^s gets angry!", m_name);
				set_hostile(m_ptr);
			}

			touch_zap_player(m_ptr);

			/* Are we draining it?  A little note: If the monster is
			dead, the drain does not work... */

			if (drain_result)
			{
				drain_result -= m_ptr->hp;  /* Calculate the difference */

				if (drain_result > 0) /* Did we really hurt it? */
				{
					drain_heal = damroll(4,(drain_result / 6));

					if (cheat_xtra)
					{
						msg_format("Draining left: %d", drain_left);
					}

					if (drain_left)
					{
						if (drain_heal < drain_left)
						{
							drain_left -= drain_heal;
						}
						else
						{
							drain_heal = drain_left;
							drain_left = 0;
						}

						if (drain_msg)
						{
							msg_format("Your weapon drains life from %s!", m_name);
							drain_msg = FALSE;
						}

						drain_heal = (drain_heal * mutant_regenerate_mod) / 100;

						hp_player(drain_heal);
						/* We get to keep some of it! */
					}
				}
			}

			/* Confusion attack */
			if ((p_ptr->confusing) || (chaos_effect == 3))
			{
				/* Cancel glowing hands */
				if (p_ptr->confusing)
				{
					p_ptr->confusing = FALSE;
					msg_print("Your hands stop glowing.");
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

			else if (chaos_effect == 4)
			{
				msg_format("%^s disappears!", m_name);
				teleport_away(c_ptr->m_idx, 50);
				num = p_ptr->num_blow + 1; /* Can't hit it anymore! */
				no_extra = TRUE;
			}

			else if ((chaos_effect == 5) && cave_floor_bold(y,x)
			    && (randint(90) > r_ptr->level))
			{
				if (!((r_ptr->flags1 & RF1_UNIQUE) ||
				      (r_ptr->flags4 & RF4_BR_CHAO) ||
				      (r_ptr->flags1 & RF1_QUESTOR)))
				{
					int tmp = poly_r_idx(m_ptr->r_idx);

					/* Pick a "new" monster race */

					/* Handle polymorph */
					if (tmp != m_ptr->r_idx)
					{
						msg_format("%^s changes!", m_name);

						/* "Kill" the "old" monster */
						delete_monster_idx(c_ptr->m_idx);

						/* Create a new monster (no groups) */
						(void)place_monster_aux(y, x, tmp, FALSE, FALSE, FALSE, FALSE);

						/* XXX XXX XXX Hack -- Assume success */

						/* Hack -- Get new monster */
						m_ptr = &m_list[c_ptr->m_idx];

						/* Oops, we need a different name... */
						monster_desc(m_name, m_ptr, 0);

						/* Hack -- Get new race */
						r_ptr = &r_info[m_ptr->r_idx];

						fear = FALSE;

					}
				}
				else
					msg_format("%^s is unaffected.", m_name);
			}
		}

		/* Player misses */
		else
		{
			/* Sound */
			sound(SOUND_MISS);

			backstab = FALSE; /* Clumsy! */

			/* Message */
			msg_format("You miss %s.", m_name);
		}
	}


	/* Mutations which yield extra 'natural' attacks */
	if (!no_extra)
	{
		if (p_ptr->muta2 & MUT2_HORNS && !mdeath)
			natural_attack(c_ptr->m_idx, MUT2_HORNS, &fear, &mdeath);
		if (p_ptr->muta2 & MUT2_BEAK && !mdeath)
			natural_attack(c_ptr->m_idx, MUT2_BEAK, &fear, &mdeath);
		if (p_ptr->muta2 & MUT2_SCOR_TAIL && !mdeath)
			natural_attack(c_ptr->m_idx, MUT2_SCOR_TAIL, &fear, &mdeath);
		if (p_ptr->muta2 & MUT2_TRUNK && !mdeath)
			natural_attack(c_ptr->m_idx, MUT2_TRUNK, &fear, &mdeath);
		if (p_ptr->muta2 & MUT2_TENTACLES && !mdeath)
			natural_attack(c_ptr->m_idx, MUT2_TENTACLES, &fear, &mdeath);
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
		/* Prevent destruction of quest levels and town */
		if (!is_quest(dun_level) && dun_level)
			earthquake(py, px, 10);
	}


	/* Handle ``weird attacks''. */
	if (p_weird_attack && p_ptr->inside_arena != 1) {
	  int rad = (p_ptr->lev/10)*randint(2);
	  int typ = rand_range(GF_ARROW, GF_MAKE_TRAP);
	  char d_attack[80];

	  if (rad < 1) rad = 1;
	  if (rad > 9) rad = 9;

	  describe_attack(typ, d_attack);

	  msg_format("A ball of %s sprouts from your fingers.", d_attack);

	  fire_ball(typ, 0, damroll(5, p_ptr->lev/3+8), rad);
	}
               

}
static bool pattern_tile(int y, int x)
{
	return ((cave[y][x].feat <= FEAT_PATTERN_XTRA2) &&
	    (cave[y][x].feat >= FEAT_PATTERN_START));
}


static bool pattern_seq(int c_y, int c_x, int n_y, int n_x)
{
	if (!(pattern_tile(c_y, c_x)) && !(pattern_tile(n_y, n_x)))
		return TRUE;

	if (cave[n_y][n_x].feat == FEAT_PATTERN_START)
	{
		if ((!(pattern_tile(c_y, c_x))) &&
		    !(p_ptr->confused || p_ptr->stun || p_ptr->image))
		{
			if (get_check("If you start walking the Road, you must walk the whole way. Ok? "))
				return TRUE;
			else
				return FALSE;
		}
		else
			return TRUE;
	}
	else if ((cave[n_y][n_x].feat == FEAT_PATTERN_OLD) ||
	    (cave[n_y][n_x].feat == FEAT_PATTERN_END) ||
	    (cave[n_y][n_x].feat == FEAT_PATTERN_XTRA2))
	{
		if (pattern_tile(c_y, c_x))
		{
			return TRUE;
		}
		else
		{
			msg_print("You must start walking the Road from the startpoint.");
			return FALSE;
		}
	}
	else if ((cave[n_y][n_x].feat == FEAT_PATTERN_XTRA1)||
	    (cave[c_y][c_x].feat == FEAT_PATTERN_XTRA1))
	{
		return TRUE;
	}
	else if (cave[c_y][c_x].feat == FEAT_PATTERN_START)
	{
		if (pattern_tile(n_y, n_x))
			return TRUE;
		else
		{
			msg_print("You must walk the Road in correct order.");
			return FALSE;
		}
	}
	else if ((cave[c_y][c_x].feat == FEAT_PATTERN_OLD) ||
	    (cave[c_y][c_x].feat == FEAT_PATTERN_END) ||
	    (cave[c_y][c_x].feat == FEAT_PATTERN_XTRA2))
	{
		if (!pattern_tile(n_y, n_x))
		{
			msg_print("You may not step off from the Road.");
			return FALSE;
		}
		else
		{
			return TRUE;
		}
	}
	else
	{
		if (!pattern_tile(c_y, c_x))
		{
			msg_print("You must start walking the Road from the startpoint.");
			return FALSE;
		}
		else
		{
			byte ok_move = FEAT_PATTERN_START;
			switch (cave[c_y][c_x].feat)
			{
				case FEAT_PATTERN_1:
					ok_move = FEAT_PATTERN_2;
					break;
				case FEAT_PATTERN_2:
					ok_move = FEAT_PATTERN_3;
					break;
				case FEAT_PATTERN_3:
					ok_move = FEAT_PATTERN_4;
					break;
				case FEAT_PATTERN_4:
					ok_move = FEAT_PATTERN_1;
					break;
				default:
					if (wizard)
						msg_format("Funny Road walking, %d.", cave[c_y][c_x]);
					return TRUE; /* Goof-up */
			}

			if ((cave[n_y][n_x].feat == ok_move) ||
			    (cave[n_y][n_x].feat == cave[c_y][c_x].feat))
				return TRUE;
			else
			{
				if (!pattern_tile(n_y, n_x))
					msg_print("You may not step off from the Road.");
				else
					msg_print("You must walk the Road in correct order.");

				return FALSE;
			}
		}
	}
}



bool player_can_enter(byte feature)
{
	bool pass_wall;
        monster_race *r_ptr = &r_info[p_ptr->polymon];

	/* Player can not walk through "walls" unless in Shadow Form */
	if ((p_ptr->wraith_form) || (p_ptr->prace == RACE_SPECTRE) ||
              (r_ptr->flags2 & RF2_PASS_WALL) || (r_ptr->flags2 & RF2_KILL_WALL))
		pass_wall = TRUE;
	else
		pass_wall = FALSE;


	switch (feature)
	{
		case FEAT_DEEP_WATER:
			{
				int wt = (adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100) / 2;
				if ((total_weight < wt) || (p_ffall))
					return (TRUE);
				else
					return (FALSE);
			}

		case FEAT_SHAL_LAVA:
			{
				if ((p_resist_fire) ||
					(p_immune_fire) ||
					(p_ptr->oppose_fire) ||
					(p_ffall))
					return (TRUE);
				else
					return (FALSE);
			}

		case FEAT_DEEP_LAVA:
			{
				if (p_ffall &&
				  ((p_resist_fire) ||
				   (p_ptr->oppose_fire) ||
				   (p_immune_fire)))
					return (TRUE);
				else
					return (FALSE);
			}

		case FEAT_SWAMP:
		{
			int wt = (adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100) / 2;
			if ((total_weight < wt) || (p_ffall))
			   return (TRUE);
			else
		           return (FALSE);
		}

                default:
			{
                                if ((p_ffall) && ((f_info[feature].flags1 & FF1_CAN_FLY) || (f_info[feature].flags1 & FF1_CAN_LEVITATE)))
					return (TRUE);
                                else if ((pass_wall) && (f_info[feature].flags1 & FF1_CAN_PASS))
					return (TRUE);
                                else if (f_info[feature].flags1 & FF1_NO_WALK)
                                        return (FALSE);
			}

	}

	return (TRUE);
}


/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should (probably) always induce energy expenditure.
 *
 * Note that moving will *always* take a turn, and will *always* hit
 * any monster which might be in the destination grid.  Previously,
 * moving into walls was "free" and did NOT hit invisible monsters.
 */
void move_player(int dir, int do_pickup)
{
	int y, x, wt;
        int seconds = 0;

	cave_type *c_ptr;
	monster_type *m_ptr;
        monster_race *r_ptr = &r_info[p_ptr->polymon];

	char m_name[80];

	bool p_can_pass_walls = FALSE;
	bool stormbringer = FALSE;

	bool oktomove = TRUE;

        if ((p_ptr->polymon) && (r_ptr->flags1 & RF1_NEVER_MOVE))
        {
               msg_print("You are immobile!");
               return;
        }

        /* Handle polymorphed random movement */

        if ((p_ptr->polymon) && ((r_ptr->flags1 & RF1_RAND_50) && 
                (r_ptr->flags1 & RF1_RAND_25)) && (randint(100) < 75))
                dir = randint(9);
        else if ((p_ptr->polymon) && (r_ptr->flags1 & RF1_RAND_50) && (randint(100) < 50))
                dir = randint(9);
        else if ((p_ptr->polymon) && (r_ptr->flags1 & RF1_RAND_25) && (randint(100) < 25))
                dir = randint(9);
        
	/* Find the result of moving */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Examine the destination */
	c_ptr = &cave[y][x];

	/* Exit the area */
	if ((!dun_level) &&
		((x == 0) || (x == cur_wid-1) ||
		 (y == 0) || (y == cur_hgt-1)))
	{
		/* Can the player enter the grid? */
		if (player_can_enter(c_ptr->mimic))
		{
			/* Hack: move to new area */
			if ((y == 0) && (x == 0))
			{
				p_ptr->wilderness_y--;
				p_ptr->wilderness_x--;
				p_ptr->oldpy = cur_hgt - 2;
				p_ptr->oldpx = cur_wid - 2;
			}

			else if ((y == 0) && (x == MAX_WID-1))
			{
				p_ptr->wilderness_y--;
				p_ptr->wilderness_x++;
				p_ptr->oldpy = cur_hgt - 2;
				p_ptr->oldpx = 1;
			}

			else if ((y == MAX_HGT-1) && (x == 0))
			{
				p_ptr->wilderness_y++;
				p_ptr->wilderness_x--;
				p_ptr->oldpy = 1;
				p_ptr->oldpx = cur_wid - 2;
			}

			else if ((y == MAX_HGT-1) && (x == MAX_WID-1))
			{
				p_ptr->wilderness_y++;
				p_ptr->wilderness_x++;
				p_ptr->oldpy = 1;
				p_ptr->oldpx = 1;
			}

			else if (y == 0)
			{
				p_ptr->wilderness_y--;
				p_ptr->oldpy = cur_hgt - 2;
				p_ptr->oldpx = x;
			}

			else if (y == cur_hgt-1)
			{
				p_ptr->wilderness_y++;
				p_ptr->oldpy = 1;
				p_ptr->oldpx = x;
			}

			else if (x == 0)
			{
				p_ptr->wilderness_x--;
				p_ptr->oldpx = cur_wid - 2;
				p_ptr->oldpy = y;
			}

			else if (x == cur_wid-1)
			{
				p_ptr->wilderness_x++;
				p_ptr->oldpx = 1;
				p_ptr->oldpy = y;
			}

			p_ptr->leftbldg = TRUE;
			p_ptr->leaving = TRUE;

			return;
		}
	}

	/* Get the monster */
	m_ptr = &m_list[c_ptr->m_idx];


	if (inventory[INVEN_WIELD].art_name)
	{
		if (streq(quark_str(inventory[INVEN_WIELD].art_name), "'Stormbringer'"))
			stormbringer = TRUE;
	}

	/* Player can not walk through "walls"... */
	/* unless in Shadow Form */
	if ((p_ptr->wraith_form) || (p_ptr->prace == RACE_SPECTRE) ||
                (r_ptr->flags2 & RF2_PASS_WALL) || (r_ptr->flags2 & RF2_KILL_WALL))
		p_can_pass_walls = TRUE;

	if ((cave[y][x].feat >= FEAT_PERM_EXTRA) &&
	    (cave[y][x].feat <= FEAT_PERM_SOLID))
	{
		p_can_pass_walls = FALSE;
	}

	/* Hack -- attack monsters */
	if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y,x) || p_can_pass_walls))
	{

		/* Attack -- only if we can see it OR it is not in a wall */
		if (!is_hostile(m_ptr) &&
		    !(p_ptr->confused || p_ptr->image || !(m_ptr->ml) || p_ptr->stun ||
		    ((p_ptr->muta2 & MUT2_BERS_RAGE) && p_ptr->shero)) &&
		    (pattern_seq(py, px, y, x)) &&
		    ((cave_floor_bold(y, x)) || (p_can_pass_walls)))
		{
			m_ptr->csleep = 0;

			/* Extract monster name (or "it") */
			monster_desc(m_name, m_ptr, 0);

			/* Auto-Recall if possible and visible */
			if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

			/* Track a new monster */
			if (m_ptr->ml) health_track(c_ptr->m_idx);

			/* displace? */
			if (stormbringer && (randint(1000)>666))
			{                        
				py_attack(y,x);
			}
			else if (cave_floor_bold(py, px) ||
			    (r_info[m_ptr->r_idx].flags2 & RF2_PASS_WALL))
			{
				msg_format("You push past %s.", m_name);
				m_ptr->fy = py;
				m_ptr->fx = px;
				cave[py][px].m_idx = c_ptr->m_idx;
				c_ptr->m_idx = 0;
				update_mon(cave[py][px].m_idx, TRUE);
                                seconds = randint(10); /* Pushing takes time */
			}
			else
			{
				msg_format("%^s is in your way!", m_name);
				energy_use = 0;
				oktomove = FALSE;
			}

			/* now continue on to 'movement' */
		}
		else
		{
			py_attack(y, x);
			oktomove = FALSE;
		}
	}

	else if (c_ptr->feat == FEAT_DEEP_WATER)
	{
		wt = (adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100) / 2;
		if ((total_weight < wt) || (p_ffall))
                {
			oktomove = TRUE;
                        seconds = randint(10);
                }
		else
		{
			msg_print("You can't swim with that much weight!");
			running = 0;
			oktomove = FALSE;
		}
	}

	else if (c_ptr->feat == FEAT_SHAL_LAVA)
	{
		if ((p_resist_fire) || (p_immune_fire) ||
		    (p_ptr->oppose_fire) || (p_ffall))
                {    
			oktomove = TRUE;
                        seconds = randint(7);
                }                       
		else
		{
			msg_print("The lava is too hot!");
			running = 0;
			oktomove = FALSE;
		}
	}

	else if ((c_ptr->feat == FEAT_DEEP_LAVA) && !p_ffall)
	{
		msg_print("The lava is too hot!");
		running = 0;
		oktomove = FALSE;
                seconds = randint(30);
	}

	else if ((c_ptr->feat == FEAT_DEEP_LAVA) && p_ffall &&
	    !((p_resist_fire) || (p_ptr->oppose_fire) ||
	    (p_immune_fire)))
	{
		msg_print("The heat is too intense to move over it.");
		running = 0;
		oktomove = FALSE;
	}

	else if ((c_ptr->feat == FEAT_DARK_PIT) && !p_ffall)
	{
		msg_print("You can't cross the chasm.");
		running = 0;
		oktomove = FALSE;
	}

	else if (c_ptr->feat == FEAT_MOUNTAIN)
	{
		msg_print("You can't climb the mountains!");
		running = 0;
		oktomove = FALSE;
	}
	/*
	 * Player can move through trees and
	 * has effective -10 speed
	 * Rangers can move without penality
	 */
	else if (c_ptr->feat == FEAT_TREES)
	{
		oktomove = TRUE;
		if (p_ptr->pclass != CLASS_RANGER) energy_use += 10;
                seconds = randint(6);
	}
	
	else if (c_ptr->feat == FEAT_SWAMP)
	{
		oktomove = TRUE;
		energy_use += 100;
                seconds = randint(10);
	}

	else if (c_ptr->feat == FEAT_CHAOS_FOG)
	{
		oktomove = TRUE;
		energy_use += 10;
                seconds = randint(8);
	}

	else if (c_ptr->feat == FEAT_RUBBLE)
	{
		oktomove = TRUE;
		energy_use += 100;
                seconds = randint(8);
	}
        else if ((c_ptr->feat >= FEAT_ALTAR_HEAD) &&
		(c_ptr->feat <= FEAT_ALTAR_TAIL))
	{
		oktomove = TRUE;
                seconds = randint(5);
	}

	else if ((c_ptr->feat >= FEAT_QUEST_ENTER) &&
		(c_ptr->feat <= FEAT_QUEST_EXIT))
	{
		oktomove = TRUE;
                seconds = randint(5);
	}

#ifdef ALLOW_EASY_DISARM /* TNB */

	/* Disarm a visible trap */
	else if ((do_pickup != easy_disarm) &&
		(c_ptr->t_idx != 0) && (c_ptr->info & CAVE_TRDT))
	{
                (void) do_cmd_disarm_aux(y, x, dir);
		return;
	}

#endif /* ALLOW_EASY_DISARM -- TNB */

	/* Player can not walk through "walls" unless in wraith form...*/
        else if ((f_info[c_ptr->feat].flags1 & FF1_WALL) &&
                (!(f_info[c_ptr->feat].flags1 & FF1_CAN_PASS) ||
                 (!p_can_pass_walls)))
	{
		oktomove = FALSE;

		/* Disturb the player */
		disturb(0, 0);

		/* Notice things in the dark */
		if ((!(c_ptr->info & (CAVE_MARK))) &&
		    (p_ptr->blind || !(c_ptr->info & (CAVE_LITE))))
		{
			/* Closed door */
			if (c_ptr->feat < FEAT_SECRET)
			{
				msg_print("You feel a closed door blocking your way.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y, x);
			}

			/* Wall (or secret door) */
			else
			{
				msg_print("You feel a wall blocking your way.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y, x);
			}
		}

		/* Notice things */
		else
		{
			/* Closed doors */
		        if (c_ptr->feat < FEAT_SECRET)
			{
#ifdef ALLOW_EASY_OPEN

				if (easy_open)
				{
					if (easy_open_door(y, x)) return;
				}
				else

#endif /* ALLOW_EASY_OPEN */

				{
					msg_print("There is a closed door blocking your way.");

					if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
						energy_use = 0;
				}
			}

			/* Wall (or secret door) */
			else
			{
				msg_print("There is a wall blocking your way.");

				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					energy_use = 0;
			}
		}

		/* Sound */
		sound(SOUND_HITWALL);
	}

	/* Normal movement */
	if (!pattern_seq(py, px, y, x))
	{
		if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
		{
			energy_use = 0;
		}

		disturb(0,0); /* To avoid a loop with running */

		oktomove = FALSE;
	}

	/* Normal movement */
	if (oktomove)
	{
		int oy, ox;

		/* Save old location */
		oy = py;
		ox = px;

		/* Move the player */
		py = y;
		px = x;

		/* Redraw new spot */
		lite_spot(py, px);

		/* Redraw old spot */
		lite_spot(oy, ox);

		/* Sound */
		/* sound(SOUND_WALK); */

                /* Walking increases time */
                /* You are supposed to cover 10 feet in 2.5 seconds at
                   normal pace. Slight variations are allowed to simulate
                   the fact that any marching speed varies over time */
                seconds = randint(5);

		/* Check for new panel (redraw map) */
		verify_panel();

		/* Update stuff */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

		/* Update the monsters */
		p_ptr->update |= (PU_DISTANCE);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);


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
#ifdef ALLOW_EASY_DISARM /* TNB */

		carry(do_pickup != always_pickup);

#else /* ALLOW_EASY_DISARM -- TNB */

		carry(do_pickup);

#endif /* ALLOW_EASY_DISARM -- TNB */

		/* Handle "store doors" */
		if ((c_ptr->feat >= FEAT_SHOP_HEAD) &&
		    (c_ptr->feat <= FEAT_SHOP_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter store */
			command_new = '_';
		}

		/* Handle "building doors" -KMW- */
		else if ((c_ptr->feat >= FEAT_BLDG_HEAD) &&
		    (c_ptr->feat <= FEAT_BLDG_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter building */
			command_new = ']';
		}

		/* Handle quest areas -KMW- */
		else if (cave[y][x].feat == FEAT_QUEST_ENTER)
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter quest level */
			command_new = '[';
		}

		else if (cave[y][x].feat == FEAT_QUEST_EXIT)
		{
			if (quest[p_ptr->inside_quest].type == QUEST_TYPE_FIND_EXIT)
			{
				quest[p_ptr->inside_quest].status = QUEST_STATUS_COMPLETED;
				msg_print("You accomplished your quest!");
				msg_print(NULL);
			}

			leaving_quest = p_ptr->inside_quest;
			p_ptr->inside_quest = cave[y][x].special;
			dun_level = 0;
			p_ptr->oldpx = 0;
			p_ptr->oldpy = 0;
			p_ptr->leaving = TRUE;
		}

		else if ((cave[y][x].feat >= FEAT_ALTAR_HEAD &&
			 cave[y][x].feat <= FEAT_ALTAR_TAIL) ||
		         (cave[y][x].feat >= FEAT_LESS &&
			 cave[y][x].feat <= FEAT_QUEST_EXIT))
			 {
			    cptr name = f_name + f_info[cave[y][x].feat].name;
			    cptr pref = (is_a_vowel(name[0])) ? "an" : "a";
			    msg_format("You see %s %s.", pref, name);
			 }

		/* Discover invisible traps */
		else if ((c_ptr->t_idx != 0) &&
			 !(f_info[cave[y][x].feat].flags1 & FF1_DOOR))
		{
			/* Disturb */
			disturb(0, 0);

			if (!(c_ptr->info & CAVE_TRDT))
			{
				/* Message */
				msg_print("You found a trap!");

				/* Pick a trap */
				pick_trap(py, px);
			}

			/* Hit the trap */
			hit_trap();
		}
	}
        
        /* Add time passed in this function */       
        sc_time += seconds;
}


/*
 * Hack -- Check for a "known wall" (see below)
 */
static int see_wall(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are not known walls */
	if (!in_bounds2(y, x)) return (FALSE);

	/* Non-wall grids are not known walls */
	if (cave[y][x].feat < FEAT_SECRET) return (FALSE);

	if ((cave[y][x].feat >= FEAT_DEEP_WATER) &&
	    (cave[y][x].feat <= FEAT_GRASS)) return (FALSE);

	if ((cave[y][x].feat >= FEAT_SHOP_HEAD) &&
	    (cave[y][x].feat <= FEAT_SHOP_TAIL)) return (FALSE);

	if ((cave[y][x].feat >= FEAT_BLDG_HEAD) &&
	    (cave[y][x].feat <= FEAT_BLDG_TAIL)) return (FALSE);

        if ((f_info[cave[y][x].feat].flags1 & FF1_CAN_RUN)) return (FALSE);

	/* Must be known to the player */
	if (!(cave[y][x].info & (CAVE_MARK))) return (FALSE);

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

	/* Illegal grids are unknown */
	if (!in_bounds2(y, x)) return (TRUE);

	/* Memorized grids are always known */
	if (cave[y][x].info & (CAVE_MARK)) return (FALSE);

	/* Non-floor grids are unknown */
	if (!cave_floor_bold(y, x)) return (TRUE);

	/* Viewable door/wall grids are known */
	if (player_can_see_bold(y, x)) return (FALSE);

	/* Default */
	return (TRUE);
}





/*
 * The running algorithm:                       -CJS-
 *
 * In the diagrams below, the player has just arrived in the
 * grid marked as '@', and he has just come from a grid marked
 * as 'o', and he is about to enter the grid marked as 'x'.
 *
 * Of course, if the "requested" move was impossible, then you
 * will of course be blocked, and will stop.
 *
 * Overview: You keep moving until something interesting happens.
 * If you are in an enclosed space, you follow corners. This is
 * the usual corridor scheme. If you are in an open space, you go
 * straight, but stop before entering enclosed space. This is
 * analogous to reaching doorways. If you have enclosed space on
 * one side only (that is, running along side a wall) stop if
 * your wall opens out, or your open space closes in. Either case
 * corresponds to a doorway.
 *
 * What happens depends on what you can really SEE. (i.e. if you
 * have no light, then running along a dark corridor is JUST like
 * running in a dark room.) The algorithm works equally well in
 * corridors, rooms, mine tailings, earthquake rubble, etc, etc.
 *
 * These conditions are kept in static memory:
 * find_openarea         You are in the open on at least one
 * side.
 * find_breakleft        You have a wall on the left, and will
 * stop if it opens
 * find_breakright       You have a wall on the right, and will
 * stop if it opens
 *
 * To initialize these conditions, we examine the grids adjacent
 * to the grid marked 'x', two on each side (marked 'L' and 'R').
 * If either one of the two grids on a given side is seen to be
 * closed, then that side is considered to be closed. If both
 * sides are closed, then it is an enclosed (corridor) run.
 *
 * LL           L
 * @x          LxR
 * RR          @R
 *
 * Looking at more than just the immediate squares is
 * significant. Consider the following case. A run along the
 * corridor will stop just before entering the center point,
 * because a choice is clearly established. Running in any of
 * three available directions will be defined as a corridor run.
 * Note that a minor hack is inserted to make the angled corridor
 * entry (with one side blocked near and the other side blocked
 * further away from the runner) work correctly. The runner moves
 * diagonally, but then saves the previous direction as being
 * straight into the gap. Otherwise, the tail end of the other
 * entry would be perceived as an alternative on the next move.
 *
 * #.#
 * ##.##
 * .@x..
 * ##.##
 * #.#
 *
 * Likewise, a run along a wall, and then into a doorway (two
 * runs) will work correctly. A single run rightwards from @ will
 * stop at 1. Another run right and down will enter the corridor
 * and make the corner, stopping at the 2.
 *
 * #@x    1
 * ########### ######
 * 2        #
 * #############
 * #
 *
 * After any move, the function area_affect is called to
 * determine the new surroundings, and the direction of
 * subsequent moves. It examines the current player location
 * (at which the runner has just arrived) and the previous
 * direction (from which the runner is considered to have come).
 *
 * Moving one square in some direction places you adjacent to
 * three or five new squares (for straight and diagonal moves
 * respectively) to which you were not previously adjacent,
 * marked as '!' in the diagrams below.
 *
 * ...!   ...
 * .o@!   .o.!
 * ...!   ..@!
 * !!!
 *
 * You STOP if any of the new squares are interesting in any way:
 * for example, if they contain visible monsters or treasure.
 *
 * You STOP if any of the newly adjacent squares seem to be open,
 * and you are also looking for a break on that side. (that is,
 * find_openarea AND find_break).
 *
 * You STOP if any of the newly adjacent squares do NOT seem to be
 * open and you are in an open area, and that side was previously
 * entirely open.
 *
 * Corners: If you are not in the open (i.e. you are in a corridor)
 * and there is only one way to go in the new squares, then turn in
 * that direction. If there are more than two new ways to go, STOP.
 * If there are two ways to go, and those ways are separated by a
 * square which does not seem to be open, then STOP.
 *
 * Otherwise, we have a potential corner. There are two new open
 * squares, which are also adjacent. One of the new squares is
 * diagonally located, the other is straight on (as in the diagram).
 * We consider two more squares further out (marked below as ?).
 *
 * We assign "option" to the straight-on grid, and "option2" to the
 * diagonal grid, and "check_dir" to the grid marked 's'.
 *
 * .s
 * @x?
 * #?
 *
 * If they are both seen to be closed, then it is seen that no
 * benefit is gained from moving straight. It is a known corner.
 * To cut the corner, go diagonally, otherwise go straight, but
 * pretend you stepped diagonally into that next location for a
 * full view next time. Conversely, if one of the ? squares is
 * not seen to be closed, then there is a potential choice. We check
 * to see whether it is a potential corner or an intersection/room entrance.
 * If the square two spaces straight ahead, and the space marked with 's'
 * are both blank, then it is a potential corner and enter if find_examine
 * is set, otherwise must stop because it is not a corner.
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
 * The direction we are running
 */
static byte find_current;

/*
 * The direction we came from
 */
static byte find_prevdir;

/*
 * We are looking for open area
 */
static bool find_openarea;

/*
 * We are looking for a break
 */
static bool find_breakright;
static bool find_breakleft;



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
	int             row, col, deepleft, deepright;
	int             i, shortleft, shortright;


	/* Save the direction */
	find_current = dir;

	/* Assume running straight */
	find_prevdir = dir;

	/* Assume looking for open area */
	find_openarea = TRUE;

	/* Assume not looking for breaks */
	find_breakright = find_breakleft = FALSE;

	/* Assume no nearby walls */
	deepleft = deepright = FALSE;
	shortright = shortleft = FALSE;

	/* Find the destination grid */
	row = py + ddy[dir];
	col = px + ddx[dir];

	/* Extract cycle index */
	i = chome[dir];

	/* Check for walls */
	if (see_wall(cycle[i+1], py, px))
	{
		find_breakleft = TRUE;
		shortleft = TRUE;
	}
	else if (see_wall(cycle[i+1], row, col))
	{
		find_breakleft = TRUE;
		deepleft = TRUE;
	}

	/* Check for walls */
	if (see_wall(cycle[i-1], py, px))
	{
		find_breakright = TRUE;
		shortright = TRUE;
	}
	else if (see_wall(cycle[i-1], row, col))
	{
		find_breakright = TRUE;
		deepright = TRUE;
	}

	/* Looking for a break */
	if (find_breakleft && find_breakright)
	{
		/* Not looking for open area */
		find_openarea = FALSE;

		/* Hack -- allow angled corridor entry */
		if (dir & 0x01)
		{
			if (deepleft && !deepright)
			{
				find_prevdir = cycle[i - 1];
			}
			else if (deepright && !deepleft)
			{
				find_prevdir = cycle[i + 1];
			}
		}

		/* Hack -- allow blunt corridor entry */
		else if (see_wall(cycle[i], row, col))
		{
			if (shortleft && !shortright)
			{
				find_prevdir = cycle[i - 2];
			}
			else if (shortright && !shortleft)
			{
				find_prevdir = cycle[i + 2];
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
	int         prev_dir, new_dir, check_dir = 0;
	int         row, col;
	int         i, max, inv;
	int         option = 0, option2 = 0;
	cave_type   *c_ptr;

	/* Where we came from */
	prev_dir = find_prevdir;


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

		/* Access grid */
		c_ptr = &cave[row][col];


		/* Visible monsters abort running */
		if (c_ptr->m_idx)
		{
			monster_type *m_ptr = &m_list[c_ptr->m_idx];

			/* Visible monster */
			if (m_ptr->ml) return (TRUE);
		}

		/* Visible objects abort running */
		for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
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
		if (c_ptr->info & (CAVE_MARK))
		{
			bool notice = TRUE;

			/* Examine the terrain */
			switch (c_ptr->feat)
			{
				/* Floors */
				case FEAT_FLOOR:

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
				/* dirt, grass, trees, ... */
				case FEAT_SHAL_WATER:
				case FEAT_DIRT:
				case FEAT_GRASS:
				case FEAT_DARK_PIT:
				case FEAT_TREES:
				case FEAT_MOUNTAIN:
				{
					/* Ignore */
					notice = FALSE;

					/* Done */
					break;
				}

				/* quest features */
				case FEAT_QUEST_ENTER:
				case FEAT_QUEST_EXIT:
				{
					/* Notice */
					notice = TRUE;

					/* Done */
					break;
				}

				case FEAT_DEEP_LAVA:
				case FEAT_SHAL_LAVA:
				{
					/* Ignore */
					if (p_ptr->invuln || p_immune_fire) notice = FALSE;

					/* Done */
					break;
				}

				case FEAT_DEEP_WATER:
				{
					/* Ignore */
					if (p_ffall) notice = FALSE;

					/* Done */
					break;
				}

				/* Open doors */
				case FEAT_OPEN:
				case FEAT_BROKEN:
				{
					/* Option -- ignore */
					if (find_ignore_doors) notice = FALSE;

					/* Done */
					break;
				}

				/* Stairs */
				case FEAT_LESS:
				case FEAT_MORE:
				{
					/* Option -- ignore */
					if (find_ignore_stairs) notice = FALSE;

					/* Done */
					break;
				}
			}

                        if(f_info[c_ptr->feat].flags1 & FF1_NO_NOTICE_RUNNING) 
                               notice = FALSE;


			/* Interesting feature */
			if (notice) return (TRUE);

			/* The grid is "visible" */
			inv = FALSE;
		}

		/* Analyze unknown grids and floors */
		if (inv || cave_floor_bold(row, col) ||
		    (cave[row][col].feat == FEAT_TREES))
		{
			/* Looking for open area */
			if (find_openarea)
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
			if (find_openarea)
			{
				if (i < 0)
				{
					/* Break to the right */
					find_breakright = TRUE;
				}

				else if (i > 0)
				{
					/* Break to the left */
					find_breakleft = TRUE;
				}
			}
		}
	}


	/* Looking for open area */
	if (find_openarea)
	{
		/* Hack -- look again */
		for (i = -max; i < 0; i++)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = py + ddy[new_dir];
			col = px + ddx[new_dir];

			/* Access grid */
			c_ptr = &cave[row][col];

			/* Unknown grid or non-wall XXX XXX XXX cave_floor_grid(c_ptr)) */
			if (!(c_ptr->info & (CAVE_MARK)) ||
			    ((c_ptr->feat < FEAT_SECRET) ||
			    ((c_ptr->feat >= FEAT_DEEP_WATER) &&
				 (c_ptr->feat <= FEAT_GRASS))))

			{
				/* Looking to break right */
				if (find_breakright)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break left */
				if (find_breakleft)
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

			/* Access grid */
			c_ptr = &cave[row][col];

			/* Unknown grid or non-wall XXX XXX XXX cave_floor_grid(c_ptr)) */
			if (!(c_ptr->info & (CAVE_MARK)) ||
			    ((c_ptr->feat < FEAT_SECRET) ||
			    ((c_ptr->feat >= FEAT_DEEP_WATER) &&
				 (c_ptr->feat <= FEAT_GRASS))))

			{
				/* Looking to break left */
				if (find_breakleft)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break right */
				if (find_breakright)
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
			find_current = option;

			/* No other options */
			find_prevdir = option;
		}

		/* Two options, examining corners */
		else if (find_examine && !find_cut)
		{
			/* Primary option */
			find_current = option;

			/* Hack -- allow curving */
			find_prevdir = option2;
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
				if (find_examine &&
				    see_nothing(option, row, col) &&
				    see_nothing(option2, row, col))
				{
					find_current = option;
					find_prevdir = option2;
				}

				/* STOP: we are next to an intersection or a room */
				else
				{
					return (TRUE);
				}
			}

			/* This corner is seen to be enclosed; we cut the corner. */
			else if (find_cut)
			{
				find_current = option2;
				find_prevdir = option2;
			}

			/* This corner is seen to be enclosed, and we */
			/* deliberately go the long way. */
			else
			{
				find_current = option;
				find_prevdir = option2;
			}
		}
	}


	/* About to hit a known wall, stop */
	if (see_wall(find_current, py, px))
	{
		return (TRUE);
	}


	/* Failure */
	return (FALSE);
}



/*
 * Take one step along the current "run" path
 */
void run_step(int dir)
{
	/* Start running */
	if (dir)
	{
		/* Hack -- do not start silly run */
		if (see_wall(dir, py, px) &&
		   (cave[py+ddy[dir]][px+ddx[dir]].feat != FEAT_TREES))
		{
			/* Message */
			msg_print("You cannot run in that direction.");

			/* Disturb */
			disturb(0, 0);

			/* Done */
			return;
		}

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);

		/* Initialize */
		run_init(dir);
	}

	/* Keep running */
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

	/* Decrease the run counter */
	if (--running <= 0) return;

	/* Take time */
	energy_use = 100;

	/* Move the player, using the "pickup" flag */
#ifdef ALLOW_EASY_DISARM /* TNB */

	move_player(find_current, FALSE);

#else /* ALLOW_EASY_DISARM -- TNB */

	move_player(find_current, always_pickup);

#endif /* ALLOW_EASY_DISARM -- TNB */
}
