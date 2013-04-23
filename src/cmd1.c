#define CMD1_C
/* File: cmd1.c */

/* Purpose: Movement commands (part 1) */

/*
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
static bool test_hit_norm(int chance, int ac, int vis)
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
	i = (weight + ((p_ptr->to_h + plus) * 4) + (skill_set[SKILL_MISSILE].value));

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
 */
static s16b critical_norm(int weight, int plus, int dam)
{
	int i, k;

	/* Extract "blow" power */
	i = (weight + ((p_ptr->to_h + plus) * 5) + (skill_set[p_ptr->wield_skill].value));

	/* Chance */
	if (randint(5000) <= i)
	{
		k = weight + randint(650);

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
s16b tot_dam_aux(object_type *o_ptr, int tdam, monster_type *m_ptr)
{
	int mult = 1;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

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
		{
			/* Slay Animal */
			if ((f1 & (TR1_SLAY_ANIMAL)) &&
			    (r_ptr->flags3 & (RF3_ANIMAL)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_ANIMAL);
				}

				if (mult < 2) mult = 2;
			}

			/* Slay Evil */
			if ((f1 & (TR1_SLAY_EVIL)) &&
			    (r_ptr->flags3 & (RF3_EVIL)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_EVIL);
				}

				if (mult < 2) mult = 2;
			}

			/* Slay Undead */
			if ((f1 & (TR1_SLAY_UNDEAD)) &&
			    (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_UNDEAD);
				}

				if (mult < 3) mult = 3;
			}

			/* Slay Demon */
			if ((f1 & (TR1_SLAY_DEMON)) &&
			    (r_ptr->flags3 & (RF3_DEMON)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_DEMON);
				}

				if (mult < 3) mult = 3;
			}

			/* Slay Orc */
			if ((f1 & (TR1_SLAY_ORC)) &&
			    (r_ptr->flags3 & (RF3_ORC)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_ORC);
				}

				if (mult < 3) mult = 3;
			}

			/* Slay Troll */
			if ((f1 & (TR1_SLAY_TROLL)) &&
			    (r_ptr->flags3 & (RF3_TROLL)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_TROLL);
				}



				if (mult < 3) mult = 3;
			}

			/* Slay Giant */
			if ((f1 & (TR1_SLAY_GIANT)) &&
			    (r_ptr->flags3 & (RF3_GIANT)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_GIANT);
				}

				if (mult < 3) mult = 3;
			}

			/* Slay Dragon  */
			if ((f1 & (TR1_ALL_SLAY_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_DRAGON);
				}

				switch (f1 & (TR1_ALL_SLAY_DRAGON))
				{
					case TR1_SLAY_DRAGON: mult = 3; break;
					case TR1_KILL_DRAGON: mult = 5; break;
					case TR1_X15_DRAGON: mult = 15; break;
				}
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
				/* Otherwise, take the damage */
				else
				{
					if (mult < 3) mult = 3;
				}
			}

	    /* Brand (Poison) - Zangband*/
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
				if (c_ptr->feat == FEAT_INVIS)
				{
					/* Pick a trap */
					pick_trap(y, x);

					/* Message */
					msg_print("You have found a trap.");

					/* Gain exp */
					skill_exp(SKILL_SEARCH);
					skill_exp(SKILL_PERCEPTION);

					/* Disturb */
					disturb(0, 0);
				}

				/* Secret door */
				if (c_ptr->feat == FEAT_SECRET)
				{
					/* Message */
					msg_print("You have found a secret door.");
					gain_exp(1);

					/* Gain exp */
					skill_exp(SKILL_SEARCH);
					skill_exp(SKILL_PERCEPTION);

					/* Pick a door XXX XXX XXX */
					replace_secret_door(y, x);

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

						/* Gain exp */
						skill_exp(SKILL_SEARCH);
						skill_exp(SKILL_PERCEPTION);

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
 * Modified version of get_check() to give a [y/n/q] prompt, returning
 * information available. Returns 'y', 'n' or 'q' as appropriate.
 */
static char get_check_ynq(cptr prompt)
{
	char rc;

	/* Help */
	help_track("ynq_prompt");

	rc = get_check_aux(prompt, "%.*s[y/n]%s%v", "nN\033yY\rQq", "nnnyyyqq");

	/* Leave a message. */
	message_add(format(0));

	/* Done with help */
	help_track(NULL);

	return rc;
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

	bool gold_only = FALSE;

	/* Scan the pile of objects */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;
			
		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- disturb */
		disturb(0, 0);

		/* Pick up gold */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Message */
            msg_format("You collect %ld gold pieces worth of %v.",
				   (long)o_ptr->pval, object_desc_f3, o_ptr, FALSE, 0);

			/* Collect the gold */
			p_ptr->au += o_ptr->pval;

			/* Redraw gold */
			p_ptr->redraw |= (PR_GOLD);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);

			/* Delete the gold */
			delete_dun_object(o_ptr);
		}

		/* gold_only cancels the collection of objects, but gold is picked up
		 * automatically. */
		else if (gold_only);

		/* Pick up objects */
		else
		{
			/* Display description if needed. */
			object_track(o_ptr);

			/* Describe the object */
			if (!pickup)
			{
				msg_format("You see %v.", object_desc_f3, o_ptr, TRUE, 3);
			}

			/* Note that the pack is too full */
			else if (!inven_carry_okay(o_ptr))
			{
				msg_format("You have no room for %v.",
					object_desc_f3, o_ptr, TRUE, 3);
			}

			/* Pick up the item (if requested and allowed) */
			else
			{
				int okay = TRUE;

				/* Hack -- query every item */
				if (carry_query_flag && !strstr(quark_str(o_ptr->note), "=g"))
				{
					char c = get_check_ynq(format("Pick up %.*v? ",
						Term->wid-strlen("Pick up ? "),
						object_desc_f3, o_ptr, TRUE, 3));

					/* Pick up this object. */
					okay = (c == 'y');

					/* Pick up no more objects. */
					gold_only = (c == 'q');
				}

				/* Attempt to pick up an object. */
				if (okay)
				{
					/* Carry the item */
					object_type *j_ptr = inven_carry(o_ptr);

					/* Message */
					msg_format("You have %v (%c).",
						object_desc_f3, j_ptr, TRUE, 3, index_to_label(j_ptr));

					/* Remember the object */
					object_track(j_ptr);

					/* Delete the object */
					delete_dun_object(o_ptr);
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
	int                     i, num, dam;

	cave_type               *c_ptr;

	cptr            name = "a trap";


	/* Disturb the player */
	disturb(0, 0);

	/* Get the cave grid */
	c_ptr = &cave[py][px];

	/* Analyze XXX XXX XXX */
	switch (c_ptr->feat)
	{
		case FEAT_TRAP_HEAD + 0x00:
		{

			if (p_ptr->ffall)
			{
                msg_print("You fly over a trap door.");
			}
			else
			{
                msg_print("You fell through a trap door!");
				dam = damroll(2, 8);
                name = "a trap door";
		   		if (dun_defs[cur_dungeon].flags & DF_TOWER)
				{
					change_level(dun_level-1, START_RANDOM);
				}
				else
				{
					change_level(dun_level+1, START_RANDOM);
				}
			take_hit(dam, name, MON_TRAP);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x01:
		{

			if (p_ptr->ffall)
			{
                msg_print("You fly over a pit trap.");
			}
			else
			{
                msg_print("You fell into a pit!");
				dam = damroll(2, 6);
                name = "a pit trap";
				take_hit(dam, name, MON_TRAP);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x02:
		{


			if (p_ptr->ffall)
			{
                msg_print("You fly over a spiked pit.");
			}

			else
			{
                msg_print("You fall into a spiked pit!");
				/* Base damage */
		name = "a pit trap";
				dam = damroll(2, 6);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled!");

		    name = "a spiked pit";
					dam = dam * 2;
					(void)set_cut(p_ptr->cut + randint(dam));
				}

				/* Take the damage */
				take_hit(dam, name, MON_TRAP);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x03:
		{


			if (p_ptr->ffall)
			{
                msg_print("You fly over a spiked pit.");
			}

			else
			{
			msg_print("You fall into a spiked pit!");
				/* Base damage */
				dam = damroll(2, 6);

		name = "a pit trap";

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled on poisonous spikes!");

		    name = "a spiked pit";

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
				take_hit(dam, name, MON_TRAP);
			}

			break;
		}

		case FEAT_TRAP_HEAD + 0x04:
		{
            msg_print("There is a flash of shimmering light!");
			c_ptr->info &= ~(CAVE_MARK);
			cave_set_feat(py, px, FEAT_FLOOR);
			num = 2 + randint(3);
			for (i = 0; i < num; i++)
                {
					 (void)summon_specific(py, px, dun_depth, 0);

                }
			if ((dun_depth)>randint(100)) /* No nasty effect for low levels */
                        { do { activate_ty_curse(); } while (randint(6)==1); }
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
			fire_dam(dam, "a fire trap", MON_TRAP);
			break;
		}

		case FEAT_TRAP_HEAD + 0x07:
		{
			msg_print("You are splashed with acid!");
			dam = damroll(4, 6);
			acid_dam(dam, "an acid trap", MON_TRAP);
			break;
		}

		case FEAT_TRAP_HEAD + 0x08:
		{
			if (check_hit(125, 0))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name, MON_TRAP);
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
			if (check_hit(125, 0))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
		take_hit(dam, "a dart trap", MON_TRAP);
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
			if (check_hit(125, 0))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
		take_hit(dam, "a dart trap", MON_TRAP);
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
			if (check_hit(125, 0))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
		take_hit(dam, "a dart trap", MON_TRAP);
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
			msg_print("A black gas surrounds you!");
			if (!p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + rand_int(50) + 25);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0D:
		{
			msg_print("A gas of scintillating colors surrounds you!");
			if (!p_ptr->resist_conf)
			{
				(void)set_confused(p_ptr->confused + rand_int(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0E:
		{
			msg_print("A pungent green gas surrounds you!");
			if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
			{
				(void)set_poisoned(p_ptr->poisoned + rand_int(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0F:
		{
			msg_print("A strange white mist surrounds you!");
			if (!p_ptr->free_act)
			{
				(void)set_paralyzed(p_ptr->paralyzed + rand_int(10) + 5);
			}
			break;
		}
	}
}


static void touch_zap_player(monster_type *m_ptr)
{
        int             aura_damage = 0;
        monster_race    *r_ptr = &r_info[m_ptr->r_idx];

        if (r_ptr->flags2 & (RF2_AURA_FIRE))
        {
            if (!(p_ptr->immune_fire))
            {
                aura_damage
                    = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

                msg_print("You are suddenly very hot!");

                if (p_ptr->oppose_fire) aura_damage = (aura_damage+2) / 3;
                if (p_ptr->resist_fire) aura_damage = (aura_damage+2) / 3;

                take_hit(aura_damage,
					format("%v", monster_desc_f2, m_ptr, 0x88), m_ptr->r_idx);
                r_ptr->r_flags2 |= RF2_AURA_FIRE;
                handle_stuff();
            }
        }


        if (r_ptr->flags2 & (RF2_AURA_ELEC))
        {
            if (!(p_ptr->immune_elec))
            {
                aura_damage
                    = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

                if (p_ptr->oppose_elec) aura_damage = (aura_damage+2) / 3;
                if (p_ptr->resist_elec) aura_damage = (aura_damage+2) / 3;
                
                msg_print("You get zapped!");
                take_hit(aura_damage,
					format("%v", monster_desc_f2, m_ptr, 0x88), m_ptr->r_idx);
                r_ptr->r_flags2 |= RF2_AURA_ELEC;
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

    int dss, ddd;

	const char * atk_desc;
	
	/* Slow down the attack */
	energy_use += TURN_ENERGY/10;

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


	/* Calculate the "attack quality" */
    bonus = p_ptr->to_h;
	chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));

		/* Test for hit */
		if (test_hit_norm(chance, r_ptr->ac, m_ptr->ml))
		{
			/* Sound */
			sound(SOUND_HIT);

            msg_format("You hit %v with your %s.",
				monster_desc_f2, m_ptr, 0, atk_desc);
			
			/* Give experience if it wasn't too easy */
			if (chance < (r_ptr->ac * 3))
			{
				skill_exp(SKILL_CLOSE);
			}

            k = damroll(ddd, dss);
            k = critical_norm(n_weight, p_ptr->to_h, k);

			/* Apply the player damage bonuses */
            k += p_ptr->to_d;   

			/* No negative damage */
			if (k < 0) k = 0;

			/* Complex message */
			if (cheat_wzrd)
			{
				msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
			}


            if (m_ptr->smart & SM_ALLY)
            {
                msg_format("%^s gets angry!", monster_desc_f2, m_ptr, 0);
                m_ptr->smart &= ~SM_ALLY;
            }

		/* Damage, check for fear and mdeath */
		switch (attack)
		{
		case MUT2_SCOR_TAIL:
			project(0, 0, m_ptr->fy, m_ptr->fx, k, GF_POIS, PROJECT_KILL);
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
			project(0, 0, m_ptr->fy, m_ptr->fx, k, GF_HELL_FIRE, PROJECT_KILL);
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
			msg_format("You miss %s.", monster_desc_f2, m_ptr, 0);
		}
}



/*
 * Player attacks a (poor, defenseless) creature        -RAK-
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
void py_attack(int y, int x)
{
	int                     k, bonus, chance;

	cave_type               *c_ptr = &cave[y][x];

	monster_type    *m_ptr = &m_list[c_ptr->m_idx];
    monster_race    *r_ptr = &r_info[m_ptr->r_idx];

	object_type             *o_ptr;

	C_TNEW(m_name, MNAME_MAX, char);


	bool            fear = FALSE;
    bool            mdeath = FALSE;

    bool        backstab = FALSE, vorpal_cut = FALSE, chaos_effect = FALSE;
    bool        stab_fleeing = FALSE;
    bool        do_quake = FALSE;
	bool		drain_msg = TRUE, drain_life = FALSE;
    u32b        f1, f2, f3; /* A massive hack -- life-draining weapons */
    bool        no_extra = FALSE;



    if (p_ptr->sneaking)
    {
        if ((m_ptr->csleep) && (m_ptr->ml))
        {
            /* Can't backstab creatures that we can't see, right? */
            backstab = TRUE;
        }
	}
    if ((m_ptr->monfear) && (m_ptr->ml) && (rand_int(50) < p_ptr->skill_stl))
    {
		stab_fleeing = TRUE;
    }

	/* Disturb the player */
	/*disturb(1, 0);*/


	/* Disturb the monster */
	m_ptr->csleep = 0;


	/* Extract monster name (or "it") */
	strnfmt(m_name, MNAME_MAX, "%v", monster_desc_f2, m_ptr, 0);

    /* Auto-Recall if possible and visible */
	if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

	/* Track a new monster */
    if (m_ptr->ml) health_track(c_ptr->m_idx);



         /* Stop if friendly */
         if (m_ptr->smart & SM_ALLY &&
            ! (p_ptr->stun || p_ptr->confused || p_ptr->image ||
              ((p_ptr->muta2 & MUT2_BERS_RAGE) && p_ptr->shero) ||
                !(m_ptr->ml)))
   {
		if (inventory[INVEN_WIELD].name1 != ART_STORMBRINGER)
     {
       msg_format("You stop to avoid hitting %s.", m_name);

		TFREE(m_name);
       return;
     }

            msg_format("Your black blade greedily attacks %s!",
                m_name);

   }



	/* Handle player fear */
	if (p_ptr->afraid)
    {   /* Message */
        if (m_ptr->ml)
		msg_format("You are too afraid to attack %s!", m_name);

        else
        msg_format ("There is something scary in your way!");

		TFREE(m_name);

		/* Done */
		return;
	}


	/* Access the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Calculate the "attack quality" */
	bonus = p_ptr->to_h + o_ptr->to_h;
	chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));

	/* Attack speed is based on theoretical number of blows per 60 turns*/
	energy_use=(TURN_ENERGY*60/p_ptr->num_blow);
		/* Test for hit */
		if (test_hit_norm(chance, r_ptr->ac, m_ptr->ml))
		{
			/* Sound */
			sound(SOUND_HIT);

			/* Message */
            if (!(backstab || stab_fleeing))
                {
                    if (!(ma_empty_hands()))
                    msg_format("You hit %s.", m_name);
                }
            else if (backstab)
			{
                msg_format("You cruelly stab the helpless, sleeping %v!",
                    monster_desc_aux_f3, r_ptr, 1, 0);
				skill_exp(SKILL_STEALTH);
            }
			else
			{
                msg_format("You backstab the fleeing %v!",
                    monster_desc_aux_f3, r_ptr, 1, 0);
				skill_exp(SKILL_STEALTH);
			}

			/* Give experience if it wasn't too easy */
			if (chance < (r_ptr->ac * 3))
			{
				skill_exp(p_ptr->wield_skill);
            }
				/* Hack -- bare hands do one damage */
			k = 1;


	   object_flags(o_ptr, &f1, &f2, &f3);

       if ((f1 & TR1_CHAOTIC) && (randint(2)==1))
           chaos_effect = TRUE;
       else
            chaos_effect = FALSE;

       if ((f1 & TR1_VAMPIRIC) || (chaos_effect && (randint(5)<3)))
       /* Prepare for drain... */
	   {
        chaos_effect = FALSE;
        if (!((r_ptr->flags3 & RF3_UNDEAD) || (r_ptr->flags3 & RF3_NONLIVING)))
		drain_life = TRUE;

	}

    if (f1 & TR1_VORPAL && (randint((o_ptr->name1 == ART_VORPAL_BLADE)?3:6) == 1))
        vorpal_cut = TRUE;
    else vorpal_cut = FALSE;

    if (ma_empty_hands())
    {
        int special_effect = 0, stun_effect = 0, times = 0;
        martial_arts * ma_ptr = &ma_blows[0], * old_ptr = &ma_blows[0];
        int resist_stun = 0;
        if (r_ptr->flags1 & RF1_UNIQUE) resist_stun += 88;
        if (r_ptr->flags3 & RF3_NO_CONF) resist_stun += 44;
        if (r_ptr->flags3 & RF3_NO_SLEEP) resist_stun += 44;
        if ((r_ptr->flags3 & RF3_UNDEAD) || (r_ptr->flags3 & RF3_NONLIVING))
            resist_stun += 88;

        for (times = 0; times < (skill_set[SKILL_MA].value<14?1:skill_set[SKILL_MA].value/14); times++)
        /* Attempt 'times' */
        {
            do
            {
				if(skill_set[SKILL_MA].value < 2)
				{
					ma_ptr = &ma_blows[0];
				}
				else
				{
					ma_ptr = &ma_blows[(randint(MAX_MA-1))];
				}
            }
            while ((ma_ptr->min_level > skill_set[SKILL_MA].value/2)
                    || (randint(skill_set[SKILL_MA].value/2) < ma_ptr->chance));

            /* keep the highest level attack available we found */
            if ((ma_ptr->min_level > old_ptr->min_level) &&
                !(p_ptr->stun || p_ptr->confused))
            {
                old_ptr = ma_ptr;
                if (cheat_wzrd && cheat_xtra)
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
                special_effect = MA_KNEE;
            }
            else
                msg_format(ma_ptr->desc, m_name);
        }

        else if (ma_ptr->effect == MA_SLOW)
        {
            if (!((r_ptr->flags1 & RF1_NEVER_MOVE)
                    || strchr("UjmeEv$,DdsbBFIJQSXclnw!=?", r_ptr->d_char)))
            {
                msg_format("You kick %s in the ankle.", m_name);
                special_effect = MA_SLOW;

            }
            else

                msg_format(ma_ptr->desc, m_name);
        }
        else {
                if (ma_ptr->effect)
                    {
                    stun_effect
                        = (ma_ptr->effect/2) + randint(ma_ptr->effect/2);
                    }

                msg_format(ma_ptr->desc, m_name);
            }


        k = critical_norm((skill_set[SKILL_MA].value/2) * (randint(10)), ma_ptr->min_level, k);

        if ((special_effect == MA_KNEE) && ((k + p_ptr->to_d) < m_ptr->hp))
        {
            msg_format("%^s moans in agony!", m_name);
            stun_effect = 7 + randint(13);
            resist_stun /= 3;
        }

        else if ((special_effect == MA_SLOW) && ((k + p_ptr->to_d) < m_ptr->hp))
        {
            if (!(r_ptr->flags1 & RF1_UNIQUE) &&
               (randint(skill_set[SKILL_MA].value/2) > r_ptr->level)
               && m_ptr->mspeed > 60)
            {
                msg_format("%^s starts limping slower.", m_name);
                m_ptr->mspeed -= 10;
            }
        }


        if (stun_effect && ((k + p_ptr->to_d) < m_ptr->hp))
        {
            if ((skill_set[SKILL_MA].value/2) > randint(r_ptr->level + resist_stun + 10))
            {
                if (m_ptr->stunned)
                  msg_format("%^s is more stunned.", m_name);
                else
                  msg_format("%^s is stunned.", m_name);

                m_ptr->stunned += (stun_effect);
            }

        }
		if ((chance < (r_ptr->ac * 3)) || (skill_set[SKILL_MA].value * 3 < r_ptr->ac * 4))
		{
			skill_exp(SKILL_MA);
		}
    }

	/* Handle normal weapon */
    else if (o_ptr->k_idx)
	{
		k = damroll(o_ptr->dd, o_ptr->ds);
		k = tot_dam_aux(o_ptr, k, m_ptr);
        if (backstab)
        {
            backstab = FALSE;
            k *= (3 + (skill_set[SKILL_STEALTH].value / 20));
        }
        else if (stab_fleeing)
        {
            k = ((3 * k) / 2);
        }
        if ((p_ptr->impact && ((k > 50) || randint(7)==1))
            || (chaos_effect && (randint(250)==1)))
            {
		    do_quake = TRUE;
            chaos_effect = FALSE;
            }

        k = critical_norm(o_ptr->weight, o_ptr->to_h, k);

        if (vorpal_cut)
        {
            int step_k = k;

            if ((o_ptr->name1 == ART_DEMONBLADE) && randint(2)!=1)
            {
                char chainsword_noise[80];
                msg_format("%v", get_rnd_line_f1, "chainswd.txt");
                msg_print(chainsword_noise);
            }

            if (o_ptr->name1 == ART_VORPAL_BLADE)
                msg_print("Your Vorpal Blade goes snicker-snack!");
            else
                msg_format("Your weapon cuts deep into %s!", m_name);
            do { k += step_k; }
            while (randint((o_ptr->name1 == ART_VORPAL_BLADE)?2:4)==1);

        }

        k += o_ptr->to_d;

			}

			/* Apply the player damage bonuses */
            k += p_ptr->to_d;   

			/* No negative damage */
			if (k < 0) k = 0;

			/* Complex message */
			if (cheat_wzrd)
			{
				msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
			}

		/* Drain the life (message if it has an effect). */
		if (drain_life && (hp_player(k) || !object_known_p(o_ptr)))
		{
			s16b drain_heal = damroll(4,(k / 6));
			if (drain_heal > MAX_VAMPIRIC_DRAIN) drain_heal = MAX_VAMPIRIC_DRAIN;
			if (drain_msg) msg_format("Your weapon drains life from %s!", m_name);
		}	


			/* Damage, check for fear and death */
            if (mon_take_hit(c_ptr->m_idx, k, &fear, NULL))
            {
                mdeath = TRUE;
            }

            if (m_ptr->smart & SM_ALLY)
            {
                msg_format("%^s gets angry!", m_name);
                m_ptr->smart &= ~SM_ALLY;
            }


        touch_zap_player(m_ptr);


			/* Confusion attack */
            if ((p_ptr->confusing) || (chaos_effect && (randint(10)!=1)))
			{
				/* Cancel glowing hands */
			if (p_ptr->confusing)
			{
				p_ptr->confusing = FALSE;
				msg_print("Your hands stop glowing.");
			}

			/* Prevent chaotic effects further down */
			chaos_effect = FALSE;


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
					m_ptr->confused += 10 + rand_int(skill_set[SKILL_DEVICE].value) / 10;
				}
			}

    else if (chaos_effect && (randint(2)==1))
    {
        chaos_effect = FALSE;
        msg_format("%^s disappears!", m_name);
        teleport_away(c_ptr->m_idx, 50);
        no_extra = TRUE;

    }


    else if (chaos_effect && cave_floor_bold(y,x)
            && (randint(90) > r_ptr->level))
	{
        if (!((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags4 & RF4_BR_CHAO) || (r_ptr->flags1 & RF1_GUARDIAN)))
        {

				/* Pick a "new" monster race */
        int tmp = poly_r_idx(m_ptr->r_idx);
        chaos_effect = FALSE;

        /* Handle polymorph */
		if (tmp != m_ptr->r_idx)
		{
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			monster_race *r2_ptr = &r_info[tmp];
			byte i = 0;
			/* Hack - SHAPECHANGERs change regularly anyway. */
			if (r_ptr->flags1 & RF1_ATTR_MULTI && r_ptr->flags2 & RF2_SHAPECHANGER) i |= 1;
			if (r2_ptr->flags1 & RF1_ATTR_MULTI && r2_ptr->flags2 & RF2_SHAPECHANGER) i |= 2;
			switch (i)
			{
				case 0: msg_format("%^s changes!", m_name); break;
				case 1: msg_format("%^s stops changing!", m_name); break;
				case 2: msg_format("%^s starts changing rapidly!", m_name); break;
				case 3: msg_format("%^s changes subtly!", m_name); break;
			}

            /* "Kill" the "old" monster */
			delete_monster_idx(c_ptr->m_idx,TRUE);

			/* Create a new monster (no groups) */
			(void)place_monster_aux(y, x, tmp, FALSE, FALSE, FALSE, FALSE);

			/* XXX XXX XXX Hack -- Assume success */

			/* Hack -- Get new monster */
			m_ptr = &m_list[c_ptr->m_idx];

            /* Oops, we need a different name... */
            strnfmt(m_name, MNAME_MAX, "%v", monster_desc_f2, m_ptr, 0);

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

	/*
     * Chaos features which yield extra 'natural' attacks
	 * only give attacks occasionally - depending on number
	 * on speed of player with weapon
	 */
    if ((!no_extra) && (randint((p_ptr->num_blow+30)/60)==1))
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


	/* Mega-Hack -- apply earthquake brand only once*/
	if (do_quake) earthquake(py, px, 10);

	TFREE(m_name);
}


/*
 * Attack a monster.
 */
void do_cmd_attack(void)
{
	int			y, x, dir;

	cave_type	*c_ptr;

	bool		more = FALSE;


	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a direction to attack, or Abort */
	if (get_rep_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid */
		c_ptr = &cave[y][x];

		/* Oops */
		if (!c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = extract_energy[p_ptr->pspeed];
			msg_print("You attack the empty air.");
		}
		/* Try fighting. */
		else
		{
			py_attack(y, x);
			if (c_ptr->m_idx) more = TRUE;
		}
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}

static bool pattern_tile(byte y, byte x)
{
    return
    ((cave[y][x].feat <= FEAT_PATTERN_XTRA2) &&
     (cave[y][x].feat >= FEAT_PATTERN_START));

}

static bool pattern_seq(byte c_y, byte c_x, byte n_y,byte  n_x)
{
    if (!(pattern_tile(c_y, c_x)) && !(pattern_tile(n_y, n_x)))
            return TRUE;

    if (cave[n_y][n_x].feat == FEAT_PATTERN_START)
    {
        if ((!(pattern_tile(c_y, c_x)))
                && !(p_ptr->confused || p_ptr->stun || p_ptr->image))
        {
            if (get_check("If you start walking the Pattern, you must walk the whole way. Ok? "))
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
            msg_print("You must start walking the Pattern from the startpoint.");
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
                msg_print("You must walk the Pattern in correct order.");
                return FALSE;
            }
    }
    else if ((cave[c_y][c_x].feat == FEAT_PATTERN_OLD) ||
             (cave[c_y][c_x].feat == FEAT_PATTERN_END) ||
             (cave[c_y][c_x].feat == FEAT_PATTERN_XTRA2))
    {
        if (!pattern_tile(n_y, n_x))
        {
            msg_print("You may not step off from the Pattern.");
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
            msg_print("You must start walking the Pattern from the startpoint.");
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
                    if (cheat_wzrd)
                        msg_format("Funny Pattern walking, %d.", cave[c_y][c_x].feat);
                    return TRUE; /* Goof-up */
            }
            if ((cave[n_y][n_x].feat == ok_move) ||
                (cave[n_y][n_x].feat == cave[c_y][c_x].feat))
                return TRUE;
            else
            {
                if (!pattern_tile(n_y, n_x))
                    msg_print("You may not step off from the Pattern.");
                else
                    msg_print("You must walk the Pattern in correct order.");

                return FALSE;
            }
        }
    }

}


/*
 * Actually move the player to a given location. 
 */
void move_to(s16b y, s16b x)
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

		/* Check for new panel (redraw map) */
		verify_panel();

		/* Update stuff */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

		/* A different area of the grid is eligible for monster creation. */
		full_grid = MAX_FULL_GRID;

		/* Update the monsters */
		p_ptr->update |= (PU_DISTANCE);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);
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
	int                     y, x;

	cave_type               *c_ptr;
	monster_type    *m_ptr;

	C_TNEW(m_name, MNAME_MAX, char);

	bool p_can_pass_walls = FALSE;
	bool wall_is_perma = FALSE;
	bool stormbringer = FALSE;

    /* Find the result of moving */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Examine the destination */
	c_ptr = &cave[y][x];

	/* Get the monster */
	m_ptr = &m_list[c_ptr->m_idx];


	if (inventory[INVEN_WIELD].name1 == ART_STORMBRINGER)
	{
		stormbringer = TRUE;
	}

	/* Player can not walk through "permanent walls"... */
	/* unless in Shadow Form */
	if ((p_ptr->wraith_form) || (p_ptr->prace == RACE_SPECTRE))
	{
		p_can_pass_walls = TRUE;
        if ((cave[y][x].feat >= FEAT_PERM_BUILDING) && (cave[y][x].feat <= FEAT_PERM_SOLID))
		{
			wall_is_perma = TRUE;
			p_can_pass_walls = FALSE;
		}
   }

	/* Hack -- attack monsters */
    if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y,x) || p_can_pass_walls))
	{
		/* Attack -- only if we can see it OR it is not in a wall */
		if ((m_ptr->smart & SM_ALLY) &&
			!(p_ptr->confused || p_ptr->image || !(m_ptr->ml) || p_ptr->stun
			|| ((p_ptr->muta2 & MUT2_BERS_RAGE) && p_ptr->shero))
			&& (pattern_seq((byte)py,(byte)px,(byte)y,(byte)x)) &&
			((cave_floor_bold(y, x)) || (p_can_pass_walls)))
		{
			m_ptr->csleep = 0;
			/* Extract monster name (or "it") */
			strnfmt(m_name, MNAME_MAX, "%v", monster_desc_f2, m_ptr, 0);
			/* Auto-Recall if possible and visible */
			if (m_ptr->ml) monster_race_track(m_ptr->r_idx);
			/* Track a new monster */
			if (m_ptr->ml) health_track(c_ptr->m_idx);
			/* displace? */
			if ( stormbringer && (randint(1000)>666))
			{
				py_attack(y,x);
			}
			else if (cave_floor_bold(py, px) ||
				(r_info[m_ptr->r_idx].flags2 & RF2_PASS_WALL))
           {
				msg_format("You push past %s.", m_name);
				m_ptr->fy = (byte)py;
				m_ptr->fx = (byte)px;
				cave[py][px].m_idx = c_ptr->m_idx;
				c_ptr->m_idx = 0;
				update_mon(cave[py][px].m_idx, TRUE);
			}
            else
            {
				msg_format("%^s is in your way!", m_name);
                energy_use = 0;
				TFREE(m_name);
                return;
            }
           /* now continue on to 'movement' */
		}
		else
		{
			py_attack(y, x);
			TFREE(m_name);
			return;
		}
	}
 #ifdef ALLOW_EASY_DISARM
 
 	/* Disarm a visible trap */
 	if ((do_pickup != easy_disarm) &&
 		(c_ptr->feat >= FEAT_TRAP_HEAD) &&
 		(c_ptr->feat <= FEAT_TRAP_TAIL))
 	{
 		(void) do_cmd_disarm_aux(y, x, dir);
		TFREE(m_name);
 		return;
 	}
 
 #endif /* ALLOW_EASY_DISARM -- TNB */
 

    /* Player can not walk through "walls" unless in wraith form...*/
   if (((!cave_floor_bold(y, x)) && (c_ptr->feat != FEAT_BUSH) && ((!p_can_pass_walls) || 
	   (c_ptr->feat == FEAT_WILD_BORDER) || (c_ptr->feat == FEAT_PATH_BORDER))) ||
	   (c_ptr->feat == FEAT_WATER))
	{
		/* Disturb the player */
		disturb(0, 0);

		/* Notice things in the dark */
		if (!(c_ptr->info & (CAVE_MARK)) &&
		    (p_ptr->blind || !(c_ptr->info & (CAVE_LITE))))
		{
			/* Rubble */
			if (c_ptr->feat == FEAT_RUBBLE)
			{
				msg_print("You feel some rubble blocking your way.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y, x);
			}
			
			/* Tree */
			else if (c_ptr->feat == FEAT_TREE)
			{
				msg_print("You feel a tree blocking your way.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y,x);
			}
			/* Water */
			else if (c_ptr->feat == FEAT_WATER)
			{
				msg_print("Your way seems to be blocked by water.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y,x);
			}
			
			else if ((c_ptr->feat == FEAT_WILD_BORDER) || (c_ptr->feat == FEAT_PATH_BORDER))
			{

				/* Leave town */
				if(is_town_p(wildy, wildx))
				{
					cur_town = wild_grid[wildy][wildx].dungeon;
					msg_format("You stumble out of %s.",town_name+town_defs[cur_town].name);
				}
				/* Test which border has been crossed */
				if(y==0)
				{
					py=cur_hgt-2;
					wildy--;
				}
				if(y==cur_hgt-1)
				{
					py=1;
					wildy++;
				}
				if(x==0)
				{
					px=cur_wid-2;
					wildx--;
				}
				if(x==cur_wid-1)
				{
					px=1;
					wildx++;
				}
				if(is_town_p(wildy, wildx))
				{
					cur_town = wild_grid[wildy][wildx].dungeon;
					msg_format("You stumble into %s.",town_name+town_defs[cur_town].name);
				}
				change_level(0, START_WALK);
			}
			
			/* Closed door */
			else if (c_ptr->feat < FEAT_SECRET)
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
			/* Rubble */
			if (c_ptr->feat == FEAT_RUBBLE)
			{
				msg_print("There is rubble blocking your way.");
                if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
                    energy_use = 0;

                /* Well, it makes sense that you lose time bumping into
                   a wall _if_ you are confused, stunned or blind; but
                   typing mistakes should not cost you a turn... */
			}

			/* Tree */
			else if (c_ptr->feat == FEAT_TREE)
			{
				msg_print("There is a tree blocking your way.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y,x);
			/* Assume that the player didn't really want to do that. */
	          		if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					energy_use = 0;
			}

			/* Water */
			else if (c_ptr->feat == FEAT_WATER)
			{
				msg_print("You cannot swim.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y,x);
			/* Assume that the player didn't really want to do that. */
	          		if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					energy_use = 0;
			}

			else if ((c_ptr->feat == FEAT_WILD_BORDER) || (c_ptr->feat == FEAT_PATH_BORDER))
			{

				/* Leave town */
				if(is_town_p(wildy, wildx))
				{
					cur_town = wild_grid[wildy][wildx].dungeon;
					msg_format("You leave %s.",town_name+town_defs[cur_town].name);
				}
				/* Test which border has been crossed */
				if(y==0)
				{
					py=cur_hgt-2;
					wildy--;
				}
				if(y==cur_hgt-1)
				{
					py=1;
					wildy++;
				}
				if(x==0)
				{
					px=cur_wid-2;
					wildx--;
				}
				if(x==cur_wid-1)
				{
					px=1;
					wildx++;
				}
				if(is_town_p(wildy, wildx))
				{
					cur_town = wild_grid[wildy][wildx].dungeon;
					msg_format("You enter %s.",town_name+town_defs[cur_town].name);
				}
				change_level(0, START_WALK);
			}

			/* Closed doors */
			else if (c_ptr->feat < FEAT_SECRET)
			{
 #ifdef ALLOW_EASY_OPEN
 
 				if (easy_open_door(y, x))
				{
					TFREE(m_name);
					return;
				}
 
 #else /* ALLOW_EASY_OPEN -- TNB */
 
  				msg_print("There is a closed door blocking your way.");
  
  				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
  					energy_use = 0;
 
 #endif /* ALLOW_EASY_OPEN -- TNB */
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
		TFREE(m_name);
        return;
	}

	/* Normal movement */
    if (!pattern_seq((byte)py,(byte)px,(byte)y,(byte)x))
    {
                if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
                {
                    energy_use = 0;
                }
                disturb(0,0); /* To avoid a loop with running */
				TFREE(m_name);
                return;
    }
/*    else */
	move_to(y,x);

		/* Sound */
		/* sound(SOUND_WALK); */

		/* Spontaneous Searching */
		if ((p_ptr->skill_fos >= 50) ||
		    (0 == rand_int(50 - p_ptr->skill_fos)))
		{
			search();
		}

		/* Continuous Searching */
		if (p_ptr->sneaking)
		{
			search();
		}

		/* Handle "objects" */
 #ifdef ALLOW_EASY_DISARM
 
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
			command_new = KTRL('E');
		}

		/* Discover invisible traps */
		else if (c_ptr->feat == FEAT_INVIS)
		{
			/* Disturb */
			disturb(0, 0);

			/* Message */
			msg_print("You found a trap!");

			/* Pick a trap */
			pick_trap(py, px);

			/* Hit the trap */
			hit_trap();
		}

		/* Set off an visible trap */
		else if ((c_ptr->feat >= FEAT_TRAP_HEAD) &&
			 (c_ptr->feat <= FEAT_TRAP_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hit the trap */
			hit_trap();
		}
	TFREE(m_name);
	}


/*
 * Hack -- Check for a "motion blocker" (see below)
 */
static int see_wall(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are blank */
	if (!in_bounds2(y, x)) return (FALSE);

	/* Must be a motion blocker */
	if (cave_floor_bold(y, x)) return (FALSE);

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


static s16b ignm_idx;

/*
 * Update the current "run" path
 *
 * Return TRUE if the running should be stopped
 */
static bool run_test(void)
{
	int                     prev_dir, new_dir, check_dir = 0;

	int                     row, col;
	int                     i, max, inv;
	int                     option, option2;

	cave_type               *c_ptr;


	/* No options yet */
	option = 0;
	option2 = 0;

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
		if (c_ptr->m_idx && c_ptr->m_idx != ignm_idx)
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
				case FEAT_PERM_BUILDING:
				case FEAT_PERM_INNER:
				case FEAT_PERM_OUTER:
				case FEAT_PERM_SOLID:

				/* Surface Features */
				case FEAT_TREE:
				case FEAT_BUSH:
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

			/* Interesting feature */
			if (notice) return (TRUE);

			/* The grid is "visible" */
			inv = FALSE;
		}

		/* Analyze unknown grids and floors */
		if (inv || cave_floor_bold(row, col))
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
			if (!(c_ptr->info & (CAVE_MARK)) || (c_ptr->feat < FEAT_SECRET))
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
			if (!(c_ptr->info & (CAVE_MARK)) || (c_ptr->feat < FEAT_SECRET))
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

		/* At least one option which isn't in the current direction */
		else if (stop_corner && (option2 || option != prev_dir))
		{
			return TRUE;
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
		if (see_wall(dir, py, px))
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

		/* Hack - attempting to run into a monster turns the run command
		 * into a command which attempts to fight the monster until the
		 * player is disturbed or the monster dies. */
		ignm_idx = cave[py+ddy[find_current]][px+ddx[find_current]].m_idx;
		if (!m_list[ignm_idx].r_idx || !m_list[ignm_idx].ml) ignm_idx = 0;
	}

	/* Keep fighting */
	else if (ignm_idx)
	{
		/* If the expected monster isn't seen to be there, stop running. */
		if ((cave[py+ddy[find_current]][px+ddx[find_current]].m_idx != ignm_idx)
			|| !(m_list[ignm_idx].r_idx) || !(m_list[ignm_idx].ml))
		{
			/* Disturb */
			disturb(0, 0);

			/* Done */
			return;
		}
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
	energy_use = extract_energy[p_ptr->pspeed]; 

	/* Move the player, using the "pickup" flag */
 #ifdef ALLOW_EASY_DISARM
 
 	move_player(find_current, FALSE);
 
 #else /* ALLOW_EASY_DISARM -- TNB */
 
 	move_player(find_current, always_pickup);
 
 #endif /* ALLOW_EASY_DISARM -- TNB */
}

/*
 * Get town at given coordinates
 */
#if 0
static byte get_cur_town(int y, int x)
{
	int i;
	for(i=0;i<MAX_TOWNS;i++)
	{
		if((x==town_defs[i].x) && (y==town_defs[i].y))
		{
			return(i);
		}
	}
	return 0;
}
#endif
