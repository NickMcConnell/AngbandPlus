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
* The Oangband combat system has been partially implemented. -SF-
* The list of things still to do is:
*
* Artifacts - rebalance damage :done + damage from activations: not done
* RandArts - rebalance (lower number of attacks)
*  - Number of attacks has now been toned down for the larger weapons.
* Potions (death, detonations etc.) May need to rebalance
* Wands
* Rods
* Staves  Damage from these three probably need to be adjusted. Done.
*
* Spells  - will probably have to wait for new system
* Ego items.
*  - Number of attacks for various ego items has been lowered for larger
*          weapons.
*     Oangband also has various modifiers for ego items that are not
*     implemented as yet.  (See ego shooters in [o]):  Not likely to be done
* Class rebalancing
*     In [o] the weapon proficiancy is level dependant: done.
*  - The number of blows for each class have been changed.
*
* Thrown items - done
*   There are no "normal" throwing items.  Only artifacts.
*   We may want to add more ego flags+ "Throwing daggers", "Throwing Hammers"
*   and the like.
* Shift - C command (info screen)
*      (deadliness instead of + to dam.)
*  - done
* Weapon Master
*  - done
*
* Shops - item prices may need to change to reflect their altered value to
*      the player.  Done.
*
*/



/*
 * Determine if the player "hits" a monster (normal combat).
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
static bool test_hit_norm(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = randint0(100);

	/* Hack -- Instant hit.  Chance to miss removed in Oangband because
	 * of the way monster ACs now work (fewer truly easy targets).
	 */
	if (k < 5) return (TRUE);

	if (chance <= 0) return (FALSE);

	/* Invisible monsters are harder to hit */
	if (!vis) chance = chance / 2;

	/* Power competes against armor. */
	if (randint0(chance) >= (ac * 3 / 4)) return (TRUE);

	/* Assume miss */
	return (FALSE);
}


/*
 * Critical hits (by player)
 *
 * Factor in weapon weight, total plusses, player level.
 * This is used by the old Monk attack routine
 * mutuations, + a few of the ego weapons.
 */
static s16b critical_norm(int weight, int chance, int dam)
{
	int i, k;

	/* Extract "blow" power */
	i = (weight + (chance * 5) + (p_ptr->lev * 3));

	/* Chance */
	if (randint1(4000) <= i)
	{
		k = weight + randint1(650);
		if (chance > 0) k += randint1(chance);

		if (k < 400)
		{
#ifdef JP
			msg_print("手ごたえがあった！");
#else
			msg_print("It was a good hit!");
#endif
	  		sound(SOUND_HIT_GOOD);
			dam = 2 * dam + 5;
		}
		else if (k < 700)
		{
#ifdef JP
			msg_print("かなりの手ごたえがあった！");
#else
			msg_print("It was a great hit!");
#endif
	  		sound(SOUND_HIT_GREAT);
			dam = 3 * dam + 10;
		}
		else if (k < 900)
		{
#ifdef JP
			msg_print("会心の一撃だ！");
#else
			msg_print("It was a superb hit!");
#endif
	  		sound(SOUND_HIT_SUPERB);
			dam = 4 * dam + 10;
		}
		else if (k < 1300)
		{
#ifdef JP
			msg_print("最高の会心の一撃だ！");
#else
			msg_print("It was a *GREAT* hit!");
#endif
	  		sound(SOUND_HIT_HI_GREAT);
			dam = 4 * dam + 20;
		}
		else
		{
#ifdef JP
			msg_print("比類なき最高の会心の一撃だ！");
#else
			msg_print("It was a *SUPERB* hit!");
#endif
			dam = ((7 * dam) / 2) + 25;
	  		sound(SOUND_HIT_HI_SUPERB);
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
 * Note that most brands and slays are x2, except Slay Animal (x2.5),
 * Slay Evil (x2), and Kill dragon (x3). -SF-
 */
s16b tot_dam_aux(object_type *o_ptr, int tdam, monster_type *m_ptr)
{
	int mult = 10;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Extra brands */
	if (p_ptr->tim_brand) f1 |= p_ptr->xtra_brand;

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
			/* Slay Human */
			if ((f1 & TR1_SLAY_HUMAN) &&
			    (r_ptr->flags3 & RF3_HUMAN))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_HUMAN;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Animal */
			if ((f1 & TR1_SLAY_ANIMAL) &&
			    (r_ptr->flags3 & RF3_ANIMAL))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_ANIMAL;
				}

				if (mult < 25) mult = 25;
			}

			/* Slay Evil */
			if ((f1 & TR1_SLAY_EVIL) &&
			    (r_ptr->flags3 & RF3_EVIL))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_EVIL;
				}

				if (mult < 20) mult = 20;
			}

			/* Slay Undead */
			if ((f1 & TR1_SLAY_UNDEAD) &&
			    (r_ptr->flags3 & RF3_UNDEAD))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_UNDEAD;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Demon */
			if ((f1 & TR1_SLAY_DEMON) &&
			    (r_ptr->flags3 & RF3_DEMON))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_DEMON;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Orc */
			if ((f1 & TR1_SLAY_ORC) &&
			    (r_ptr->flags3 & RF3_ORC))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_ORC;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Troll */
			if ((f1 & TR1_SLAY_TROLL) &&
			    (r_ptr->flags3 & RF3_TROLL))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_TROLL;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Giant */
			if ((f1 & TR1_SLAY_GIANT) &&
			    (r_ptr->flags3 & RF3_GIANT))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_GIANT;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Dragon  */
			if ((f1 & TR1_SLAY_DRAGON) &&
			    (r_ptr->flags3 & RF3_DRAGON))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_DRAGON;
				}

				if (mult < 30) mult = 30;
			}

			/* Execute Dragon */
			if ((f1 & TR1_KILL_DRAGON) &&
			    (r_ptr->flags3 & RF3_DRAGON))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_DRAGON;
				}

				if (mult < 50) mult = 50;
			}

			/* Brand (Acid) */
			if (f1 & TR1_BRAND_ACID)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & RF3_IM_ACID)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= RF3_IM_ACID;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 25) mult = 25;
				}
			}

			/* Brand (Elec) */
			if (f1 & TR1_BRAND_ELEC)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & RF3_IM_ELEC)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= RF3_IM_ELEC;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 25) mult = 25;
				}
			}

			/* Brand (Fire) */
			if (f1 & TR1_BRAND_FIRE)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & RF3_IM_FIRE)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= RF3_IM_FIRE;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 25) mult = 25;
				}
			}

			/* Brand (Cold) */
			if (f1 & TR1_BRAND_COLD)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & RF3_IM_COLD)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= RF3_IM_COLD;
					}
				}
				/* Otherwise, take the damage */
				else
				{
					if (mult < 25) mult = 25;
				}
			}

			/* Brand (Poison) */
			if (f1 & TR1_BRAND_POIS)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & RF3_IM_POIS)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= RF3_IM_POIS;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 25) mult = 25;
				}
			}
			break;
		}
	}

	/* Return the total damage */
	return (tdam * mult / 10);
}


/*
 * Search for hidden things
 */
void search(void)
{
	int y, x, chance;

	s16b this_o_idx, next_o_idx;

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
			if (randint0(100) < chance)
			{
				/* Access the grid */
				c_ptr = &cave[y][x];

				/* Invisible trap */
				if (c_ptr->feat == FEAT_INVIS)
				{
					/* Pick a trap */
					pick_trap(y, x);

					/* Message */
#ifdef JP
					msg_print("トラップを発見した。");
#else
					msg_print("You have found a trap.");
#endif


					/* Disturb */
					disturb(0, 0);
				}

				/* Secret door */
				if (c_ptr->feat == FEAT_SECRET)
				{
					/* Message */
#ifdef JP
					msg_print("隠しドアを発見した。");
#else
					msg_print("You have found a secret door.");
#endif


					/* Pick a door */
					place_closed_door(y, x);

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
#ifdef JP
						msg_print("箱に仕掛けられたトラップを発見した！");
#else
						msg_print("You have discovered a trap on the chest!");
#endif


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
 * Helper routine for py_pickup() and py_pickup_floor().
 *
 * Add the given dungeon object to the character's inventory.
 *
 * Delete the object afterwards.
 */
void py_pickup_aux(int o_idx)
{
	int slot, i;

#ifdef JP
/*
 * アイテムを拾った際に「２つのケーキを持っている」
 * "You have two cakes." とアイテムを拾った後の合計のみの表示がオリジナル
 * だが、違和感が
 * あるという指摘をうけたので、「〜を拾った、〜を持っている」という表示
 * にかえてある。そのための配列。
 */
	char o_name[MAX_NLEN];
/*	char	old_name[80];*/
	char kazu_str[80];
	int hirottakazu;
#else
	char o_name[MAX_NLEN];
#endif

	object_type *o_ptr;

	o_ptr = &o_list[o_idx];

#ifdef JP
/*	object_desc(old_name, o_ptr, OD_NAME_ONLY);*/
	object_desc_kosuu(kazu_str, o_ptr);
	hirottakazu = o_ptr->number;
#endif
	/* Carry the object */
	slot = inven_carry(o_ptr);

	/* Get the object again */
	o_ptr = &inventory[slot];

	/* Describe the object */
	object_desc(o_name, o_ptr, 0);

	/* Message */
#ifdef JP
	if (o_ptr->number > hirottakazu) {
	    msg_format("%s拾って、%s(%c)を持っている。",
		       kazu_str, o_name, index_to_label(slot));
	} else {
	    msg_format("%s(%c)を拾った。", o_name, index_to_label(slot));
	}
#else
	msg_format("You have %s (%c).", o_name, index_to_label(slot));
#endif


	/* Check if completed a quest */
	for (i = 0; i < max_quests; i++)
	{
		if ((quest[i].type == QUEST_TYPE_FIND_ARTIFACT) &&
		    (quest[i].status == QUEST_STATUS_TAKEN) &&
			   (quest[i].k_idx == o_ptr->name1))
		{
			quest[i].status = QUEST_STATUS_COMPLETED;
			quest[i].complev = (byte)p_ptr->lev;
#ifdef JP
			msg_print("クエストを達成した！");
#else
			msg_print("You completed your quest!");
#endif
	  		sound(SOUND_LEVEL); /* (Sound substitute) No quest sound */
			msg_print(NULL);
		}
	}
	
	/* Delete the object */
	delete_object_idx(o_idx);
}


/*
 * Player "wants" to pick up an object or gold.
 * Note that we ONLY handle things that can be picked up.
 * See "move_player()" for handling of other things.
 */
void carry(int pickup)
{
	cave_type *c_ptr = &cave[py][px];

	s16b this_o_idx, next_o_idx;

	char o_name[MAX_NLEN];

	/* Recenter the map around the player */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Handle stuff */
	handle_stuff();

	/* Automatically pickup/destroy/inscribe items */
	autopick_pickup_items(c_ptr);

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

#ifdef ALLOW_EASY_SENSE /* TNB */

		/* Option: Make item sensing easy */
		if (easy_sense)
		{
			/* Sense the object */
			(void)sense_object(o_ptr);
		}

#endif /* ALLOW_EASY_SENSE -- TNB */

		/* Describe the object */
		object_desc(o_name, o_ptr, 0);

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- disturb */
		disturb(0, 0);

		/* Pick up gold */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Message */
#ifdef JP
		msg_format(" $%ld の価値がある%sを見つけた。",
			   (long)o_ptr->pval, o_name);
#else
			msg_format("You collect %ld gold pieces worth of %s.",
				   (long)o_ptr->pval, o_name);
#endif

			if(o_ptr->pval<200)
			{
				   sound(SOUND_MONEY1);
            } 
            else if(o_ptr->pval<600)
			{ 
				   sound(SOUND_MONEY2);
            } 
            else
			{ 
				   sound(SOUND_MONEY3);
            } 

			/* Collect the gold */
			p_ptr->au += o_ptr->pval;

			/* Redraw gold */
			p_ptr->redraw |= (PR_GOLD);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);

			/* Delete the gold */
			delete_object_idx(this_o_idx);
		}

		/* Pick up objects */
		else
		{
			/* Hack - some objects were handled in autopick_pickup_items(). */
			if (o_ptr->marked & OM_NOMSG)
			{
				/* Clear the flag. */
				o_ptr->marked &= ~OM_NOMSG;
			}

			/* Describe the object */
			else if (!pickup)
			{
#ifdef JP
				msg_format("%sがある。", o_name);
#else
				msg_format("You see %s.", o_name);
#endif

			}

			/* Note that the pack is too full */
			else if (!inven_carry_okay(o_ptr))
			{
#ifdef JP
				msg_format("ザックには%sを入れる隙間がない。", o_name);
#else
				msg_format("You have no room for %s.", o_name);
#endif

			}

			/* Pick up the item (if requested and allowed) */
			else
			{
				int okay = TRUE;

				/* Hack -- query every item */
				if (carry_query_flag)
				{
					char out_val[160];
#ifdef JP
					sprintf(out_val, "%sを拾いますか? ", o_name);
#else
					sprintf(out_val, "Pick up %s? ", o_name);
#endif

					okay = get_check(out_val);
				}

				/* Attempt to pick up an object. */
				if (okay)
				{
					/* Pick up the object */
					py_pickup_aux(this_o_idx);
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
static int check_hit(int power)
{
	int k, ac;

	/* Percentile dice */
	k = randint0(100);

	/* Hack -- 5% hit, 5% miss */
	if (k < 10) return (k < 5);

	/* Paranoia -- No power */
	if (power <= 0) return (FALSE);

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Power competes against Armor */
	if (randint1(power) > ((ac * 3) / 4)) return (TRUE);

	/* Assume miss */
	return (FALSE);
}


/*
 * Handle player hitting a real trap
 */
static void hit_trap(void)
{
	int i, num, dam;

	cave_type *c_ptr;

#ifdef JP
	cptr		name = "トラップ";
#else
	cptr name = "a trap";
#endif



	/* Disturb the player */
	disturb(0, 0);

	/* Get the cave grid */
	c_ptr = &cave[py][px];

	/* Analyze XXX XXX XXX */
	switch (c_ptr->feat)
	{
		case FEAT_TRAP_TRAPDOOR:
		{
			if (p_ptr->ffall)
			{
#ifdef JP
				msg_print("落し戸を飛び越えた。");
#else
				msg_print("You fly over a trap door.");
#endif

			}
			else
			{
#ifdef JP
				msg_print("落し戸に落ちた！");
#else
				msg_print("You have fallen through a trap door!");
#endif

/* 				sound(SOUND_FALL); */ /* No fall sound */
				dam = damroll(2, 8);
#ifdef JP
				name = "落し戸";
#else
				name = "a trap door";
#endif

				take_hit(dam, name);

				/* Still alive and autosave enabled */
				if (autosave_l && (p_ptr->chp >= 0))
					do_cmd_save_game(TRUE);

				dun_level++;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
			break;
		}

		case FEAT_TRAP_PIT:
		{
			if (p_ptr->ffall)
			{
#ifdef JP
				msg_print("落し穴を飛び越えた。");
#else
				msg_print("You fly over a pit trap.");
#endif

			}
			else
			{
#ifdef JP
				msg_print("落し穴に落ちてしまった！");
#else
				msg_print("You have fallen into a pit!");
#endif

				dam = damroll(2, 6);
#ifdef JP
				name = "落し穴";
#else
				name = "a pit trap";
#endif

				take_hit(dam, name);
			}
			break;
		}

		case FEAT_TRAP_SPIKED_PIT:
		{
			if (p_ptr->ffall)
			{
#ifdef JP
				msg_print("トゲのある落し穴を飛び越えた。");
#else
				msg_print("You fly over a spiked pit.");
#endif

			}
			else
			{
#ifdef JP
			msg_print("スパイクが敷かれた落し穴に落ちてしまった！");
#else
				msg_print("You fall into a spiked pit!");
#endif


				/* Base damage */
#ifdef JP
				name = "落し穴";
#else
				name = "a pit trap";
#endif

				dam = damroll(2, 6);

				/* Extra spike damage */
				if (randint0(100) < 50)
				{
#ifdef JP
					msg_print("スパイクが刺さった！");
#else
					msg_print("You are impaled!");
#endif


#ifdef JP
					name = "トゲのある落し穴";
#else
					name = "a spiked pit";
#endif

					dam = dam * 2;
					(void)set_cut(p_ptr->cut + randint1(dam));
				}

				/* Take the damage */
				take_hit(dam, name);
			}
			break;
		}

		case FEAT_TRAP_POISON_PIT:
		{
			if (p_ptr->ffall)
			{
#ifdef JP
				msg_print("トゲのある落し穴を飛び越えた。");
#else
				msg_print("You fly over a spiked pit.");
#endif

			}
			else
			{
#ifdef JP
			msg_print("スパイクが敷かれた落し穴に落ちてしまった！");
#else
				msg_print("You fall into a spiked pit!");
#endif


				/* Base damage */
				dam = damroll(2, 6);

#ifdef JP
				name = "落し穴";
#else
				name = "a pit trap";
#endif


				/* Extra spike damage */
				if (randint0(100) < 50)
				{
#ifdef JP
					msg_print("毒を塗られたスパイクが刺さった！");
#else
					msg_print("You are impaled on poisonous spikes!");
#endif


#ifdef JP
					name = "トゲのある落し穴";
#else
					name = "a spiked pit";
#endif


					dam = dam * 2;
					(void)set_cut(p_ptr->cut + randint1(dam));

					if (p_ptr->resist_pois || p_ptr->oppose_pois)
					{
#ifdef JP
						msg_print("しかし毒の影響はなかった！");
#else
						msg_print("The poison does not affect you!");
#endif

					}

					else
					{
						dam = dam * 2;
						(void)set_poisoned(p_ptr->poisoned + randint1(dam));
					}
				}

				/* Take the damage */
				take_hit(dam, name);
			}

			break;
		}

		case FEAT_TRAP_TY_CURSE:
		{
#ifdef JP
			msg_print("何かがピカッと光った！");
#else
			msg_print("There is a flash of shimmering light!");
#endif

			c_ptr->info &= ~(CAVE_MARK);
			cave_set_feat(py, px, FEAT_FLOOR);
			num = 2 + randint1(3);
			for (i = 0; i < num; i++)
			{
				(void)summon_specific(0, py, px, dun_level, 0, TRUE, FALSE, FALSE);
			}

			if (dun_level > randint1(100)) /* No nasty effect for low levels */
			{
				bool stop_ty = FALSE;
				int count = 0;

				do
				{
					stop_ty = activate_ty_curse(stop_ty, &count);
				}
				while (randint1(6) == 1);
			}
			break;
		}

		case FEAT_TRAP_TELEPORT:
		{
#ifdef JP
			msg_print("テレポート・トラップにひっかかった！");
#else
			msg_print("You hit a teleport trap!");
#endif

			teleport_player(100);
			break;
		}

		case FEAT_TRAP_FIRE:
		{
#ifdef JP
			msg_print("炎に包まれた！");
#else
			msg_print("You are enveloped in flames!");
#endif

			dam = damroll(4, 6);
#ifdef JP
			fire_dam(dam, "炎のトラップ");
#else
			fire_dam(dam, "a fire trap");
#endif

			break;
		}

		case FEAT_TRAP_ACID:
		{
#ifdef JP
			msg_print("酸が吹きかけられた！");
#else
			msg_print("You are splashed with acid!");
#endif

			dam = damroll(4, 6);
#ifdef JP
			acid_dam(dam, "酸のトラップ");
#else
			acid_dam(dam, "an acid trap");
#endif

			break;
		}

		case FEAT_TRAP_SLOW:
		{
			if (check_hit(125))
			{
#ifdef JP
				msg_print("小さなダーツが飛んできて刺さった！");
#else
				msg_print("A small dart hits you!");
#endif

				dam = damroll(1, 4);
				take_hit(dam, name);
				(void)set_slow(p_ptr->slow + randint0(20) + 20);
			}
			else
			{
#ifdef JP
				msg_print("小さなダーツが飛んできた！が、運良く当たらなかった。");
#else
				msg_print("A small dart barely misses you.");
#endif

			}
			break;
		}

		case FEAT_TRAP_LOSE_STR:
		{
			if (check_hit(125))
			{
#ifdef JP
				msg_print("小さなダーツが飛んできて刺さった！");
#else
				msg_print("A small dart hits you!");
#endif

				dam = damroll(1, 4);
#ifdef JP
				take_hit(dam, "ダーツの罠");
#else
				take_hit(dam, "a dart trap");
#endif

				(void)do_dec_stat(A_STR);
			}
			else
			{
#ifdef JP
				msg_print("小さなダーツが飛んできた！が、運良く当たらなかった。");
#else
				msg_print("A small dart barely misses you.");
#endif

			}
			break;
		}

		case FEAT_TRAP_LOSE_DEX:
		{
			if (check_hit(125))
			{
#ifdef JP
				msg_print("小さなダーツが飛んできて刺さった！");
#else
				msg_print("A small dart hits you!");
#endif

				dam = damroll(1, 4);
#ifdef JP
				take_hit(dam, "ダーツの罠");
#else
				take_hit(dam, "a dart trap");
#endif

				(void)do_dec_stat(A_DEX);
			}
			else
			{
#ifdef JP
				msg_print("小さなダーツが飛んできた！が、運良く当たらなかった。");
#else
				msg_print("A small dart barely misses you.");
#endif

			}
			break;
		}

		case FEAT_TRAP_LOSE_CON:
		{
			if (check_hit(125))
			{
#ifdef JP
				msg_print("小さなダーツが飛んできて刺さった！");
#else
				msg_print("A small dart hits you!");
#endif

				dam = damroll(1, 4);
#ifdef JP
				take_hit(dam, "ダーツの罠");
#else
				take_hit(dam, "a dart trap");
#endif

				(void)do_dec_stat(A_CON);
			}
			else
			{
#ifdef JP
				msg_print("小さなダーツが飛んできた！が、運良く当たらなかった。");
#else
				msg_print("A small dart barely misses you.");
#endif

			}
			break;
		}

		case FEAT_TRAP_BLIND:
		{
#ifdef JP
			msg_print("黒いガスに包み込まれた！");
#else
			msg_print("A black gas surrounds you!");
#endif

			if (!p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + randint0(50) + 25);
			}
			break;
		}

		case FEAT_TRAP_CONFUSE:
		{
#ifdef JP
			msg_print("きらめくガスに包み込まれた！");
#else
			msg_print("A gas of scintillating colors surrounds you!");
#endif

			if (!p_ptr->resist_conf)
			{
				(void)set_confused(p_ptr->confused + randint0(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_POISON:
		{
#ifdef JP
			msg_print("刺激的な緑色のガスに包み込まれた！");
#else
			msg_print("A pungent green gas surrounds you!");
#endif

			if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
			{
				(void)set_poisoned(p_ptr->poisoned + randint0(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_SLEEP:
		{
#ifdef JP
			msg_print("奇妙な白い霧に包まれた！");
#else
			msg_print("A strange white mist surrounds you!");
#endif

			if (!p_ptr->free_act)
			{
#ifdef JP
msg_print("あなたは眠りに就いた。");
#else
				msg_print("You fall asleep.");
#endif


				if (ironman_nightmare)
				{
#ifdef JP
msg_print("身の毛もよだつ光景が頭に浮かんだ。");
#else
					msg_print("A horrible vision enters your mind.");
#endif


					/* Pick a nightmare */
					get_mon_num_prep(get_nightmare, NULL);

					/* Have some nightmares */
					have_nightmare(get_mon_num(MAX_DEPTH));

					/* Remove the monster restriction */
					get_mon_num_prep(NULL, NULL);
				}
				(void)set_paralyzed(p_ptr->paralyzed + randint0(10) + 5);
			}
			break;
		}

		case FEAT_TRAP_TRAPS:
		{
#ifdef JP
msg_print("まばゆい閃光が走った！");
#else
			msg_print("There is a bright flash of light!");
#endif


			/* Destroy this trap */
			cave_set_feat(py, px, FEAT_FLOOR);

			/* Make some new traps */
			project(0, 1, py, px, 0, GF_MAKE_TRAP, PROJECT_HIDE | PROJECT_JUMP | PROJECT_GRID);

			break;
		}
	}
}


static void touch_zap_player(monster_type *m_ptr)
{
	int aura_damage;
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	if (r_ptr->flags2 & RF2_AURA_FIRE)
	{
		if (!p_ptr->immune_fire)
		{
			char aura_dam[80];

			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

			/* Hack -- Get the "died from" name */
			monster_desc(aura_dam, m_ptr, 0x88);

#ifdef JP
			msg_print("突然とても熱くなった！");
#else
			msg_print("You are suddenly very hot!");
#endif


			if (p_ptr->oppose_fire) aura_damage = (aura_damage + 2) / 3;
			if (p_ptr->resist_fire) aura_damage = (aura_damage + 2) / 3;

			take_hit(aura_damage, aura_dam);
			r_ptr->r_flags2 |= RF2_AURA_FIRE;
			handle_stuff();
		}
	}

	if (r_ptr->flags3 & RF3_AURA_COLD)
	{
		if (!p_ptr->immune_cold)
		{
			char aura_dam[80];

			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

			/* Hack -- Get the "died from" name */
			monster_desc(aura_dam, m_ptr, 0x88);

#ifdef JP
			msg_print("突然とても寒くなった！");
#else
			msg_print("You are suddenly very cold!");
#endif


			if (p_ptr->oppose_cold) aura_damage = (aura_damage + 2) / 3;
			if (p_ptr->resist_cold) aura_damage = (aura_damage + 2) / 3;

			take_hit(aura_damage, aura_dam);
			r_ptr->r_flags3 |= RF3_AURA_COLD;
			handle_stuff();
		}
	}

	if (r_ptr->flags2 & RF2_AURA_ELEC)
	{
		if (!p_ptr->immune_elec)
		{
			char aura_dam[80];

			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

			/* Hack -- Get the "died from" name */
			monster_desc(aura_dam, m_ptr, 0x88);

			if (p_ptr->oppose_elec) aura_damage = (aura_damage + 2) / 3;
			if (p_ptr->resist_elec) aura_damage = (aura_damage + 2) / 3;

#ifdef JP
			msg_print("電撃をくらった！");
#else
			msg_print("You get zapped!");
#endif

			take_hit(aura_damage, aura_dam);
			r_ptr->r_flags2 |= RF2_AURA_ELEC;
			handle_stuff();
		}
	}
}


/*
 * culcurate melee bonus for each classes.
 */
int calc_melee_bonus(int hand)
{
	int bonus = 0;
	int slot = INVEN_WIELD + hand;

	if (hand > 1) return (0);

	/* two handed combat gives melee penalty according to a weight of weapon */
	if (is_two_handed())
	{
		bonus -= (inventory[slot].weight / 30);
	}

	return (bonus);
}


#ifndef TINYANGBAND
/**** The monster bashing code. -LM- ****/
static bool monster_bash(int *blows, int sleeping_bonus, cave_type *c_ptr, 
			bool *fear, char *m_name)
{
	int bash_chance, bash_quality, bash_dam;

	monster_type    *m_ptr = &m_list[c_ptr->m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	
	/* No shield on arm, no bash.  */
	if (!inventory[INVEN_ARM].k_idx || is_two_handed())
	{
		bash_chance = 0;
	}

	/* Players do not bash if they could otherwise take advantage of special
	 * bonuses against sleeping monsters, or if the monster is low-level.
	 */
	else if ((sleeping_bonus) || (r_ptr->level < p_ptr->lev / 2))
	{
		bash_chance = 0;
	}

	/* Bashing chance depends on melee Skill, Dex, and a class level bonus. */
	else bash_chance = p_ptr->skill_thn +
			   (adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128 +
			   (((p_ptr->pclass == CLASS_WARRIOR) ||
			     (p_ptr->pclass == CLASS_PALADIN) ||
			     (p_ptr->pclass == CLASS_WARRIOR_MAGE)) ? p_ptr->lev : 0);

	/* Players bash more often when they see a real need. */
	if (bash_chance)
	{
		if ((inventory[INVEN_WIELD].dd * inventory[INVEN_WIELD].ds * (*blows))
			< (inventory[INVEN_ARM].dd * inventory[INVEN_ARM].ds * 3))
			bash_chance *= 2;
	}

	/* Try to get in a shield bash. */
	if (bash_chance > randint0(240 + r_ptr->level * 9))
	{
#ifdef JP
		msg_print("シールドでモンスターに体当りした！");
#else
		msg_print("You get in a shield bash!");
#endif

		/* Calculate attack quality, a mix of momentum and accuracy. */
		bash_quality = p_ptr->skill_thn + (p_ptr->wt / 8) +
			(p_ptr->total_weight / 80) + (inventory[INVEN_ARM].weight / 3);

		/* Calculate damage.  Big shields are deadly. */
		bash_dam = damroll(inventory[INVEN_ARM].dd, inventory[INVEN_ARM].ds);

		/* Multiply by quality and experience factors */
		bash_dam *= bash_quality / 20 + p_ptr->lev / 7;

		/* Strength bonus. */
		bash_dam += (adj_str_td[p_ptr->stat_ind[A_STR]] - 128);

		/* Paranoia. */
		if (bash_dam > 125) bash_dam = 125;

		/* Encourage the player to keep wearing that heavy shield. */
		if (randint1(bash_dam) > 30 + randint1(bash_dam / 2))
#ifdef JP
			msg_print("バーン！");
#else
			msg_print("WHAMM!");
#endif

		/* Complex message */
		if (wizard)
		{
#ifdef JP
			msg_format("%d/%d のダメージを与えた。", bash_dam, m_ptr->hp);
#else
			msg_format("You do %d (out of %d) damage.", bash_dam, m_ptr->hp);
#endif
		}

		/* Damage, check for fear and death. */
		if (mon_take_hit(c_ptr->m_idx, bash_dam, fear, NULL))
		{
			/* Fight's over. */
			return (TRUE);
		}

		/* Stunning. */
		if (bash_quality + p_ptr->lev > randint1(200 + r_ptr->level * 8))
		{
#ifdef JP
			msg_format("%^sはフラフラになった。", m_name);
#else
			msg_format("%^s is stunned.", m_name);
#endif
			m_ptr->stunned += randint0(p_ptr->lev / 5) + 4;
			if (m_ptr->stunned > 24) m_ptr->stunned = 24;
		}

		/* Confusion. */
		if (bash_quality + p_ptr->lev > randint1(300 + r_ptr->level * 6) &&
			(!r_ptr->flags3 & (RF3_NO_CONF)))
		{
#ifdef JP
			msg_format("%^sは混乱したようだ。", m_name);
#else
			msg_format("%^s appears confused.", m_name);
#endif
			m_ptr->confused += randint0(p_ptr->lev / 5) + 4;
		}

		/* The player will sometimes stumble. */
		if ((30 + adj_dex_th[p_ptr->stat_ind[A_DEX]] - 128) < randint1(60))
			*blows -= randint1(*blows);
	}
	
	/* Monster is not dead */
	return (FALSE);
}
#endif /* ifndef TINYANGBAND */


/*
 * Player attacks a (poor, defenseless) creature        -RAK-
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
static void py_attack_aux(int y, int x, int mode)
{
	int i;

	/* Number of dice, also total damage. */
	int k;

	/* blow count */
	int num = 0;

	/* Bonus to attack if monster is sleeping, for certain classes. */
	int sleeping_bonus = 0;

	/* Bonus to effective monster ac if it can take cover in terrain. */
	int terrain_bonus = 0;

	int bonus, chance;

	int blows;

	cave_type       *c_ptr = &cave[y][x];

	monster_type    *m_ptr = &m_list[c_ptr->m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];

	object_type     *o_ptr;

	char            m_name[80];


	bool            fear = FALSE;
	bool            mdeath = FALSE;

	bool            vorpal_cut;
	int             chaos_effect = 0;
	bool            do_quake = FALSE;
	bool			can_drain = FALSE;
	bool            drain_msg = TRUE;
	int             drain_result = 0, drain_heal;
	int             drain_left = MAX_VAMPIRIC_DRAIN;
	u32b            f1, f2, f3; /* A massive hack -- life-draining weapons */

	bool            backstab = FALSE;
	bool            stab_fleeing = FALSE;

	/* Disturb the player */
	disturb(0, 0);

	/* Disturb the monster */
	m_ptr->csleep = 0;
	if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Auto-Recall if possible and visible */
	if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

	/* Track a new monster */
	if (m_ptr->ml) health_track(c_ptr->m_idx);

	/* Stop if friendly */
	if (!is_hostile(m_ptr) &&
		!(p_ptr->stun || p_ptr->confused || p_ptr->image ||
		((p_ptr->muta & MUT_BERS_RAGE) && p_ptr->shero) ||
		!m_ptr->ml))
	{
		if (inventory[INVEN_WIELD].name2 == EGO_WEIRD ||
			inventory[INVEN_ARM].name2 == EGO_WEIRD)
		{
#ifdef JP
			msg_format("黒い刃は強欲に%sを攻撃した！", m_name);
#else
			msg_format("Your black blade greedily attacks %s!", m_name);
#endif
		}
#ifdef JP
		else if (!get_check("本当に攻撃しますか？"))
#else
		else if (!get_check("Really hit it? "))
#endif
		{
#ifdef JP
			msg_format("%sを攻撃するのを止めた。", m_name);
#else
			msg_format("You stop to avoid hitting %s.", m_name);
#endif
			return;
		}
	}

	/* Handle player fear */
	if (p_ptr->afraid)
	{
		/* Message */
		if (m_ptr->ml)
#ifdef JP
			msg_format("恐くて%sを攻撃できない！", m_name);
#else
			msg_format("You are too afraid to attack %s!", m_name);
#endif
		else
#ifdef JP
			msg_format ("そっちには何か恐いものがいる！");
#else
			msg_format ("There is something scary in your way!");
#endif

		/* Done */
		return;
	}

	/* Monsters in rubble can take advantage of cover. -LM- */
	if (c_ptr->feat == FEAT_RUBBLE)
	{
		terrain_bonus = r_ptr->ac / 7 + 5;
	}
	/*
	* Monsters in trees can take advantage of cover,
	* except from rangers.
	*/
	else if (c_ptr->feat == FEAT_TREES)
	{
		terrain_bonus = r_ptr->ac / 7 + 5;
	}
	/* Monsters in water are vulnerable. -LM- */
	else if (c_ptr->feat == FEAT_DEEP_WATER)
	{
		terrain_bonus -= r_ptr->ac / 5;
	}

	for (i = 0; i < 2; i++)
	{
		if ((i == 1) && !is_two_handed()) break;

		/* Initial some effects */
		num = 0;
		vorpal_cut = FALSE;
		chaos_effect = 0;
		can_drain = FALSE;

		/* Initial blows available. */
		blows = p_ptr->num_blow[i];

		/* ATTACK_BAKUSIN */
		if (mode == ATTACK_BAKUSIN)
		{
			blows++;
			if (one_in_(3) == 1) blows++;
		}
#if 0
		if (p_ptr->pclass == CLASS_RANGER)
		{
			if (m_ptr->csleep && m_ptr->ml)
			{
				/* Can't backstab creatures that we can't see, right? */
				sleeping_bonus = 200 + p_ptr->lev * 4;
				backstab = TRUE;
			}
			else if (m_ptr->monfear && m_ptr->ml)
			{
				sleeping_bonus = 100 + p_ptr->lev * 2;
				stab_fleeing = TRUE;
			}
		}
#endif
#ifndef TINYANGBAND	/* No bashing in TinyAngband */
		/* Attempt to shield bash the monster */
		if (i == 0)
			if (monster_bash(&blows, sleeping_bonus, c_ptr, &fear, m_name)) return;
#endif
		/* Access the weapon */
		o_ptr = &inventory[INVEN_WIELD + i];

		/* Calculate the "attack quality".  As BTH_PLUS_ADJ has been reduced
		 * to 1, base skill and modifiers to skill are given equal weight. -LM-
		 */
		bonus = p_ptr->to_h + o_ptr->to_h + calc_melee_bonus(i);
		chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));

		/* Attack once for each legal blow */
		while (num++ < blows)
		{
			/* Test for hit */
			if (test_hit_norm(chance + sleeping_bonus,
						r_ptr->ac + terrain_bonus, m_ptr->ml))
			{
				/* Sound */
				sound(SOUND_HIT);

				/* Hack -- bare hands do one damage */
				k = 1;

				object_flags(o_ptr, &f1, &f2, &f3);

				/* Select a chaotic effect (50% chance) */
				if ((f1 & TR1_CHAOTIC) && (randint1(2) == 1))
				{
					if (randint1(5) < 3)
					{
						/* Vampiric (20%) */
						chaos_effect = 1;
					}
					else if (randint1(250) == 1)
					{
						/* Quake (0.12%) */
						chaos_effect = 2;
					}
					else if (randint1(10) != 1)
					{
						/* Confusion (26.892%) */
						chaos_effect = 3;
					}
					else if (randint1(2) == 1)
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

				/* Vampiric drain */
				if ((f1 & TR1_VAMPIRIC) || (chaos_effect == 1))
				{
					/* Only drain "living" monsters */
					if (monster_living(r_ptr))
						can_drain = TRUE;
					else
						can_drain = FALSE;
				}

				if (((f1 & TR1_VORPAL) || (mode == ATTACK_VORPAL)) && (randint1(6) == 1))
					vorpal_cut = TRUE;
				else vorpal_cut = FALSE;

				if (o_ptr->k_idx)
				{
#ifdef JP
					if (backstab) msg_format("あなたは冷酷にも眠っている無力な%sを突き刺した！", m_name);
					else if (stab_fleeing) msg_format("逃げる%sを背中から突き刺した！", m_name);
					else msg_format("%sを攻撃した。", m_name);
#else
					if (backstab) msg_format("You cruelly stab the helpless, sleeping %s!", m_name);
					else if (stab_fleeing) msg_format("You backstab the fleeing %s!",  m_name);
					else msg_format("You hit %s.", m_name);
#endif

					k = damroll(o_ptr->dd, o_ptr->ds);
					k = tot_dam_aux(o_ptr, k, m_ptr);

					/* hack -- check for earthquake. */
					if (p_ptr->impact && (k > 49)) do_quake = TRUE;

					if ((p_ptr->impact && ((k > 50) || randint1(7) == 1)) ||
						 (chaos_effect == 2))
					{
						do_quake = TRUE;
					}

					k = critical_norm(o_ptr->weight, bonus + sleeping_bonus, k);

					drain_result = k;

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
						int inc_chance = 4;

#ifdef JP
						msg_format("%sをグッサリ切り裂いた！", m_name);
#else
						msg_format("Your weapon cuts deep into %s!", m_name);
#endif

						/* Try to increase the damage */
						while (one_in_(inc_chance))
						{
							mult++;
						}

						k *= mult;

						/* Ouch! */
						if (k > m_ptr->hp)
						{
#ifdef JP
							msg_format("%sを真っ二つにした！", m_name);
#else
							msg_format("You cut %s in half!", m_name);
#endif
						}
						else
						{
							switch(mult)
							{
#ifdef JP
								case 2:	msg_format("%sを斬った！", m_name);	break;
								case 3:	msg_format("%sをぶった斬った！", m_name); break;
								case 4:	msg_format("%sをメッタ斬りにした！", m_name); break;
								case 5:	msg_format("%sをメッタメタに斬った！", m_name);	break;
								case 6:	msg_format("%sを刺身にした！", m_name);	break;
								case 7:	msg_format("%sを斬って斬って斬りまくった！", m_name); break;
								default:	msg_format("%sを細切れにした！", m_name); break;
#else
								case 2: msg_format("You gouge %s!", m_name); break;
								case 3: msg_format("You maim %s!", m_name); break;
								case 4: msg_format("You carve %s!", m_name); break;
								case 5: msg_format("You cleave %s!", m_name); break;
								case 6: msg_format("You smite %s!", m_name); break;
								case 7: msg_format("You eviscerate %s!", m_name); break;
								default: msg_format("You shred %s!", m_name); break;
#endif
							}
						}
						drain_result = drain_result * 3 / 2;
					}

					/* Plus To-Dam bonus to damege */
					k += o_ptr->to_d;
					drain_result += o_ptr->to_d;
				}

				/* Apply the player damage bonuses */
				k += p_ptr->to_d;
				drain_result += p_ptr->to_d;

				/* No negative damage */
				if (k < 0) k = 0;

				if (mode == ATTACK_TOUKI)
				{
				/* ATTACK_TOUKI bypass invulnerability */
					if (m_ptr->invulner)
#ifdef JP
						msg_print("闘気が無敵のバリアを斬り裂いた！");
#else
						msg_print("The barrier is penetrated!");
#endif
				}
				else
				{
					/* Modify the damage */
					k = mon_damage_mod(m_ptr, k, 0);
				}

				/* Complex message */
				if (wizard)
				{
#ifdef JP
					msg_format("%d/%d のダメージを与えた。", k, m_ptr->hp);
#else
					msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
#endif
				}

				if (k <= 0) can_drain = FALSE;
				if (drain_result > m_ptr->hp) drain_result = m_ptr->hp;

				/* Damage, check for fear and death */
				if (mon_take_hit(c_ptr->m_idx, k, &fear, NULL))
				{
					/* Hack -- High-level warriors can spread their attacks out
					 * among weaker foes. -LM-
					 */
					if ((p_ptr->pclass == CLASS_WARRIOR) &&
						 (p_ptr->lev > 39) && (num < p_ptr->num_blow[i]) &&
						(energy_use))
					{
						if (i == 1)
							energy_use = energy_use * (p_ptr->num_blow[0] + num)
							/ (p_ptr->num_blow[0] + p_ptr->num_blow[1]);
						else
							energy_use = energy_use * num / p_ptr->num_blow[0];
					}

					mdeath = TRUE;
					break;
				}

				/* Anger the monster */
				if (k > 0) anger_monster(m_ptr);

				touch_zap_player(m_ptr);

				/* Are we draining it?  A little note: If the monster is
				dead, the drain does not work... */

				if (can_drain)
				{
					if (drain_result > 0) /* Did we really hurt it? */
					{
						drain_heal = damroll(4, drain_result / 6);

						if (cheat_xtra)
						{
#ifdef JP
							msg_format("Draining left: %d", drain_left);
#else
							msg_format("Draining left: %d", drain_left);
#endif
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
#ifdef JP
								msg_format("刃が%sから生命力を吸い取った！", m_name);
#else
								msg_format("Your weapon drains life from %s!", m_name);
#endif
								drain_msg = FALSE;
							}

							drain_heal = (drain_heal * mutant_regenerate_mod) / 100;

							hp_player(drain_heal);
							/* We get to keep some of it! */
						}
					}
				}

				/* Confusion attack */
				if (p_ptr->confusing || (chaos_effect == 3))
				{
					/* Cancel glowing hands */
					if (p_ptr->confusing)
					{
						p_ptr->confusing = FALSE;
#ifdef JP
						msg_print("手の輝きがなくなった。");
#else
						msg_print("Your hands stop glowing.");
#endif
						p_ptr->redraw |= (PR_STATUS);
					}

					/* Confuse the monster */
					if (r_ptr->flags3 & RF3_NO_CONF)
					{
						if (m_ptr->ml)
						{
							r_ptr->r_flags3 |= RF3_NO_CONF;
						}

#ifdef JP
						msg_format("%^sには効果がなかった。", m_name);
#else
						msg_format("%^s is unaffected.", m_name);
#endif
					}
					else if (randint0(100) < r_ptr->level)
					{
#ifdef JP
						msg_format("%^sには効果がなかった。", m_name);
#else
						msg_format("%^s is unaffected.", m_name);
#endif
					}
					else
					{
#ifdef JP
						msg_format("%^sは混乱したようだ。", m_name);
#else
						msg_format("%^s appears confused.", m_name);
#endif
						m_ptr->confused += 10 + randint0(p_ptr->lev) / 5;
					}
				}

				else if (chaos_effect == 4)
				{
					bool resists_tele = FALSE;

					if (r_ptr->flags3 & RF3_RES_TELE)
					{
						if (r_ptr->flags1 & RF1_UNIQUE)
						{
							if (m_ptr->ml) r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
							msg_format("%^sには効果がなかった。", m_name);
#else
							msg_format("%^s is unaffected!", m_name);
#endif
							resists_tele = TRUE;
						}
						else if (r_ptr->level > randint1(100))
						{
							if (m_ptr->ml) r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
							msg_format("%^sは抵抗力を持っている！", m_name);
#else
							msg_format("%^s resists!", m_name);
#endif
							resists_tele = TRUE;
						}
					}

					if (!resists_tele)
					{
#ifdef JP
						msg_format("%^sは消えた！", m_name);
#else
						msg_format("%^s disappears!", m_name);
#endif
						teleport_away(c_ptr->m_idx, 50);
						num = p_ptr->num_blow[i] + 1; /* Can't hit it anymore! */
					}
				}

				else if ((chaos_effect == 5) && cave_floor_grid(c_ptr) &&
					 (randint1(90) > r_ptr->level))
				{
					if (!(r_ptr->flags1 & RF1_UNIQUE) &&
						!(r_ptr->flags4 & RF4_BR_CHAO) &&
						!(r_ptr->flags1 & RF1_QUESTOR))
					{
						if (polymorph_monster(y, x))
						{
#ifdef JP
							msg_format("%^sは変化した！", m_name);
#else
							msg_format("%^s changes!", m_name);
#endif

							/* Hack -- Get new monster */
							m_ptr = &m_list[c_ptr->m_idx];

							/* Oops, we need a different name... */
							monster_desc(m_name, m_ptr, 0);

							/* Hack -- Get new race */
							r_ptr = &r_info[m_ptr->r_idx];

							fear = FALSE;
						}
						else
						{
#ifdef JP
						msg_format("%^sには効果がなかった。", m_name);
#else
							msg_format("%^s is unaffected.", m_name);
#endif
						}
					}
				}
			}

			/* Player misses */
			else
			{
				/* Sound */
				sound(SOUND_MISS);

				/* Message */
#ifdef JP
				msg_format("ミス！ %sにかわされた。", m_name);
#else
				msg_format("You miss %s.", m_name);
#endif
			}
		} /* while */

		/* When target monster already died. */
		if (mdeath) break;
	} /* for */

	/* ATTACK_BAKUSIN */
	if (mode == ATTACK_BAKUSIN && !mdeath)
	{
		if(!((r_ptr->flags3 & RF3_NO_STUN) || (r_ptr->flags1 & RF1_UNIQUE)))
		{
#ifdef JP
			msg_format("%^sはフラフラになった。", m_name);
#else
			msg_format("%^s is stunned.", m_name);
#endif
			m_ptr->stunned += randint0(p_ptr->lev / 5) + 4;
			if (m_ptr->stunned > 24) m_ptr->stunned = 24;
		}
		if(!(r_ptr->flags3 & RF3_NO_FEAR))
		{
			m_ptr->monfear += randint0(p_ptr->lev / 5) + 4;
			fear = TRUE;
		}
	}

	/* Hack -- delay fear messages */
	if (fear && m_ptr->ml)
	{
		/* Sound */
		sound(SOUND_FLEE);

		/* Message */
#ifdef JP
		msg_format("%^sは恐怖して逃げ出した！", m_name);
#else
		msg_format("%^s flees in terror!", m_name);
#endif

	}

	/* Mega-Hack -- apply earthquake brand */
	if (do_quake)
	{
		earthquake(py, px, 10);
	}
}


/*
 * Player attacks a (poor, defenseless) creature        -RAK-
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
void py_attack(int y, int x)
{
	py_attack_aux(y, x, ATTACK_NORMAL);
}

void py_attack_special(int y, int x, int mode)
{
	py_attack_aux(y, x, mode);
}


bool player_can_enter(byte feature)
{
	bool pass_wall;

	/* Player can not walk through "walls" unless in Shadow Form */
	if (p_ptr->wraith_form || p_ptr->pass_wall || p_ptr->kill_wall)
		pass_wall = TRUE;
	else
		pass_wall = FALSE;


	switch (feature)
	{
		case FEAT_DEEP_WATER:
		case FEAT_SHAL_LAVA:
		case FEAT_DEEP_LAVA:
			return (TRUE);

		case FEAT_DARK_PIT:
		{
			if (p_ptr->ffall)
				return (TRUE);
			else
				return (FALSE);
		}

		case FEAT_TREES:
		{
			return (TRUE);
		}

		case FEAT_RUBBLE:
		case FEAT_MAGMA:
		case FEAT_QUARTZ:
		case FEAT_MAGMA_H:
		case FEAT_QUARTZ_H:
		case FEAT_MAGMA_K:
		case FEAT_QUARTZ_K:
		case FEAT_WALL_EXTRA:
		case FEAT_WALL_INNER:
		case FEAT_WALL_OUTER:
		case FEAT_WALL_SOLID:
		{
			return (pass_wall);
		}

		case FEAT_MOUNTAIN:
		{
			return (!dun_level && p_ptr->ffall);
		}

		case FEAT_PERM_EXTRA:
		case FEAT_PERM_INNER:
		case FEAT_PERM_OUTER:
		case FEAT_PERM_SOLID:
		{
			return (FALSE);
		}
	}

	return (TRUE);
}


static bool pattern_tile(int y, int x)
{
	return ((cave[y][x].feat <= FEAT_PATTERN_XTRA2) &&
		(cave[y][x].feat >= FEAT_PATTERN_START));
}


static bool pattern_seq(int c_y, int c_x, int n_y, int n_x)
{
	if (!pattern_tile(c_y, c_x) && !pattern_tile(n_y, n_x))
		return TRUE;

	if (cave[n_y][n_x].feat == FEAT_PATTERN_START)
	{
		if (!pattern_tile(c_y, c_x) &&
		    !p_ptr->confused && !p_ptr->stun && !p_ptr->image)
		{
#ifdef JP
			if (get_check("パターンの上を歩き始めると、全てを歩かなければなりません。いいですか？"))
#else
			if (get_check("If you start walking the Pattern, you must walk the whole way. Ok? "))
#endif
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
#ifdef JP
			msg_print("パターンの上を歩くにはスタート地点から歩き始めなくてはなりません。");
#else
			msg_print("You must start walking the Pattern from the startpoint.");
#endif
			return FALSE;
		}
	}
	else if ((cave[n_y][n_x].feat == FEAT_PATTERN_XTRA1) ||
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
#ifdef JP
			msg_print("パターンの上は正しい順序で歩かねばなりません。");
#else
			msg_print("You must walk the Pattern in correct order.");
#endif
			return FALSE;
		}
	}
	else if ((cave[c_y][c_x].feat == FEAT_PATTERN_OLD) ||
		 (cave[c_y][c_x].feat == FEAT_PATTERN_END) ||
		 (cave[c_y][c_x].feat == FEAT_PATTERN_XTRA2))
	{
		if (!pattern_tile(n_y, n_x))
		{
#ifdef JP
			msg_print("パターンを踏み外してはいけません。");
#else
			msg_print("You may not step off from the Pattern.");
#endif
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
#ifdef JP
			msg_print("パターンの上を歩くにはスタート地点から歩き始めなくてはなりません。");
#else
			msg_print("You must start walking the Pattern from the startpoint.");
#endif
			return FALSE;
		}
		else
		{
			byte ok_move;
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
#ifdef JP
						msg_format("おかしなパターン歩行、%d。", cave[c_y][c_x].feat);
#else
						msg_format("Funny Pattern walking, %d.", cave[c_y][c_x].feat);
#endif
					return TRUE; /* Goof-up */
			}

			if ((cave[n_y][n_x].feat == ok_move) ||
			    (cave[n_y][n_x].feat == cave[c_y][c_x].feat))
				return TRUE;
			else
			{
				if (!pattern_tile(n_y, n_x))
#ifdef JP
					msg_print("パターンを踏み外してはいけません。");
#else
					msg_print("You may not step off from the Pattern.");
#endif
				else
#ifdef JP
					msg_print("パターンの上は正しい順序で歩かねばなりません。");
#else
					msg_print("You must walk the Pattern in correct order.");
#endif
				return FALSE;
			}
		}
	}
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
	int y, x;

	cave_type *c_ptr;
	monster_type *m_ptr;

	char m_name[80];

	bool p_can_pass_walls = FALSE;
	bool p_can_kill_walls = FALSE;
	bool stormbringer = FALSE;

	bool oktomove = TRUE;

	/* Find the result of moving */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Examine the destination */
	c_ptr = &cave[y][x];

	/* Exit the wilderness-area */
	if (!dun_level &&
		((x == 0) || (x == MAX_WID - 1) ||
		 (y == 0) || (y == MAX_HGT - 1)))
	{
		/* Can the player enter the grid? */
		if (c_ptr->mimic && player_can_enter(c_ptr->mimic))
		{
			/* Hack: move to new area */
			p_ptr->oldpx = x;
			p_ptr->oldpy = y;

			if (y == 0)
			{
				p_ptr->wilderness_y--;
				p_ptr->oldpy = cur_hgt - 2;
			}
			else if (y == MAX_HGT - 1)
			{
				p_ptr->wilderness_y++;
				p_ptr->oldpy = 1;
			}

			if (x == 0)
			{
				p_ptr->wilderness_x--;
				p_ptr->oldpx = cur_wid - 2;
			}
			else if (x == MAX_WID - 1)
			{
				p_ptr->wilderness_x++;
				p_ptr->oldpx = 1;
			}

			p_ptr->leaving = TRUE;

			return;
		}

		oktomove = FALSE;
	}

	/* Get the monster */
	m_ptr = &m_list[c_ptr->m_idx];

	if (inventory[INVEN_WIELD].name2 == EGO_WEIRD)
		stormbringer = TRUE;

	/* Player can not walk through "walls"... */
	/* unless in Shadow Form */
	if (p_ptr->wraith_form || p_ptr->pass_wall || p_ptr->kill_wall)
		p_can_pass_walls = TRUE;

	if ((cave[y][x].feat >= FEAT_PERM_EXTRA) &&
	    (cave[y][x].feat <= FEAT_PERM_SOLID))
	{
		p_can_pass_walls = FALSE;
	}

	p_can_kill_walls = (p_ptr->kill_wall) ? p_can_pass_walls : FALSE;

	/* Hack -- attack monsters */
	if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x) || p_can_pass_walls))
	{
		/* Attack -- only if we can see it OR it is not in a wall */
		if (!is_hostile(m_ptr) &&
		    !(p_ptr->confused || p_ptr->image || !m_ptr->ml || p_ptr->stun ||
		    ((p_ptr->muta & MUT_BERS_RAGE) && p_ptr->shero)) &&
		    (pattern_seq(py, px, y, x)) &&
		    ((cave_floor_bold(y, x)) || p_can_pass_walls))
		{
			m_ptr->csleep = 0;
			if (r_info[m_ptr->r_idx].flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);

			/* Extract monster name (or "it") */
			monster_desc(m_name, m_ptr, 0);

			/* Auto-Recall if possible and visible */
			if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

			/* Track a new monster */
			if (m_ptr->ml) health_track(c_ptr->m_idx);

			/* displace? */
			if (stormbringer && (randint1(1000) > 666))
			{
				py_attack(y, x);
			}
			else if (cave_floor_bold(py, px) ||
			    (r_info[m_ptr->r_idx].flags2 & RF2_PASS_WALL))
			{
#ifdef JP
				msg_format("%sを押し退けた。", m_name);
#else
				msg_format("You push past %s.", m_name);
#endif

				m_ptr->fy = (byte)py;
				m_ptr->fx = (byte)px;
				cave[py][px].m_idx = c_ptr->m_idx;
				c_ptr->m_idx = 0;
				update_mon(cave[py][px].m_idx, TRUE);
			}
			else
			{
#ifdef JP
				msg_format("%^sが邪魔だ！", m_name);
#else
				msg_format("%^s is in your way!", m_name);
#endif

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

	else if ((c_ptr->feat == FEAT_DARK_PIT) && !p_ptr->ffall)
	{
#ifdef JP
		msg_print("裂け目を横切ることはできません。");
#else
		msg_print("You can't cross the chasm.");
#endif

		running = 0;
		energy_use = 0;
		oktomove = FALSE;
	}

	else if (c_ptr->feat == FEAT_MOUNTAIN)
	{
		if (dun_level || !p_ptr->ffall)
		{
#ifdef JP
			msg_print("山には登れません！");
#else
			msg_print("You can't climb the mountains!");
#endif

			running = 0;
			energy_use = 0;
			oktomove = FALSE;
		}
	}
	/*
	 * Player can move through trees and
	 * has effective -10 speed
	 * Rangers can move without penality
	 */
	else if (c_ptr->feat == FEAT_TREES)
	{
		oktomove = TRUE;
		energy_use += 10;
	}
	/* Quest features */
	else if ((c_ptr->feat >= FEAT_QUEST_ENTER) &&
		 (c_ptr->feat <= FEAT_QUEST_EXIT))
	{
		oktomove = TRUE;
	}
	/* Closed door */
	else if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
		 (c_ptr->feat <= FEAT_DOOR_TAIL))
	{
		/* Pass through the door? */
		if (p_can_pass_walls)
		{
#ifdef ALLOW_EASY_OPEN
			/* Automatically open the door? */
			if (easy_open && easy_open_door(y, x))
			{
				oktomove = FALSE;

				/* Disturb the player */
				disturb(0, 0);
			}
#endif /* ALLOW_EASY_OPEN */
		}
		else
		{
			oktomove = FALSE;

			/* Disturb the player */
			disturb(0, 0);

			/* Notice things in the dark */
			if ((!(c_ptr->info & CAVE_MARK)) &&
				(p_ptr->blind || !(c_ptr->info & CAVE_LITE)))
			{
#ifdef JP
				msg_print("ドアが行く手をはばんでいるようだ。");
#else
				msg_print("You feel a closed door blocking your way.");
#endif

				c_ptr->info |= (CAVE_MARK);
				lite_spot(y, x);
			}

			/* Notice things */
			else
			{
#ifdef ALLOW_EASY_OPEN
				if (easy_open && easy_open_door(y, x)) return;
#endif /* ALLOW_EASY_OPEN */

#ifdef JP
				msg_print("ドアが行く手をはばんでいる。");
#else
				msg_print("There is a closed door blocking your way.");
#endif


				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					energy_use = 0;
			}

			/* Sound */
			sound(SOUND_HITWALL);
		}
	}

	/* Disarm a visible trap */
	else if (!do_pickup && is_trap(c_ptr->feat))
	{
		bool ignore = FALSE;
		switch (c_ptr->feat)
		{
			case FEAT_TRAP_TRAPDOOR:
			case FEAT_TRAP_PIT:
			case FEAT_TRAP_SPIKED_PIT:
			case FEAT_TRAP_POISON_PIT:
				if (p_ptr->ffall) ignore = TRUE;
				break;
			case FEAT_TRAP_TELEPORT:
				if (p_ptr->anti_tele) ignore = TRUE;
				break;
			case FEAT_TRAP_FIRE:
				if (p_ptr->immune_fire) ignore = TRUE;
				break;
			case FEAT_TRAP_ACID:
				if (p_ptr->immune_acid) ignore = TRUE;
				break;
			case FEAT_TRAP_BLIND:
				if (p_ptr->resist_blind) ignore = TRUE;
				break;
			case FEAT_TRAP_CONFUSE:
				if (p_ptr->resist_conf) ignore = TRUE;
				break;
			case FEAT_TRAP_POISON:
				if (p_ptr->resist_pois) ignore = TRUE;
				break;
			case FEAT_TRAP_SLEEP:
				if (p_ptr->free_act) ignore = TRUE;
				break;
		}

		if (!ignore)
		{
			(void)do_cmd_disarm_aux(y, x, dir);
			return;
		}
	}

	/* Player can not walk through "walls" unless in wraith form...*/
	else if (!cave_floor_bold(y, x) && !p_can_pass_walls)
	{
		oktomove = FALSE;

		/* Disturb the player */
		disturb(0, 0);

		/* Notice things in the dark */
		if ((!(c_ptr->info & (CAVE_MARK))) &&
		    (p_ptr->blind || !(c_ptr->info & (CAVE_LITE))))
		{
			/* Rubble */
			if (c_ptr->feat == FEAT_RUBBLE)
			{
#ifdef JP
				msg_print("岩石が行く手をはばんでいるようだ。");
#else
				msg_print("You feel some rubble blocking your way.");
#endif

				c_ptr->info |= (CAVE_MARK);
				lite_spot(y, x);
			}
			/* Wall (or secret door) */
			else
			{
#ifdef JP
				msg_print("壁が行く手をはばんでいるようだ。");
#else
				msg_print("You feel a wall blocking your way.");
#endif

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
#ifdef JP
				msg_print("岩石が行く手をはばんでいる。");
#else
				msg_print("There is rubble blocking your way.");
#endif


				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					energy_use = 0;

				/*
				 * Well, it makes sense that you lose time bumping into
				 * a wall _if_ you are confused, stunned or blind; but
				 * typing mistakes should not cost you a turn...
				 */
			}
			/* Wall (or secret door) */
			else
			{
#ifdef JP
				msg_print("壁が行く手をはばんでいる。");
#else
				msg_print("There is a wall blocking your way.");
#endif


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

		/* To avoid a loop with running */
		disturb(0, 0);

		oktomove = FALSE;
	}

	/* Hit an invisible wall */
	else if (c_ptr->feat == FEAT_WALL_INVIS)
	{
		oktomove = FALSE;

		disturb(0, 0);

#ifdef JP
msg_print("何かにぶちあたった。");
#else
		msg_print("You bump into something.");
#endif

	}

	/* Normal movement */
	if (oktomove)
	{
		int oy, ox;

		/* Save old location */
		oy = py;
		ox = px;

		if (p_ptr->warning)
		{
			if (process_warning(x, y))
			{
#ifdef JP
				if (!get_check("本当にこのまま進みますか？"))
#else
				if (!get_check("Really want to go ahead?"))
#endif
				{
					energy_use = 25;
					return;
				}
			}
		}

		/* Can kill walls */
		if (p_can_kill_walls)
		{
			if ((c_ptr->feat >= FEAT_SECRET) && (c_ptr->feat <= FEAT_WALL_SOLID))
			{
				c_ptr->feat = FEAT_FLOOR;
				c_ptr->info |= (CAVE_MARK);
			}
		}

		/* Move the player */
		py = y;
		px = x;

		/* Remove "unsafe" flag */
		if ((!p_ptr->blind && !no_lite()) || ((c_ptr->feat != FEAT_INVIS) && !is_trap(c_ptr->feat)))
			c_ptr->info &= ~(CAVE_UNSAFE);

		/* Redraw new spot */
		lite_spot(py, px);

		/* Redraw old spot */
		lite_spot(oy, ox);

		/* Sound */
		/* sound(SOUND_WALK); /* Why disabled ? */

		/* Check for new panel (redraw map) */
		verify_panel();

		/* Update stuff */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);

		/* Update the monsters */
		p_ptr->update |= (PU_DISTANCE);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);


		/* Spontaneous Searching */
		if ((p_ptr->skill_fos >= 50) ||
		    (0 == randint0(50 - p_ptr->skill_fos)))
		{
			search();
		}

		/* Continuous Searching */
		if (p_ptr->searching)
		{
			search();
		}

		/* Handle "objects" */
		carry(do_pickup != always_pickup);

		/* Handle "store doors" */
		if ((c_ptr->feat >= FEAT_SHOP_HEAD) &&
			 (c_ptr->feat <= FEAT_SHOP_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter store */
			command_new = SPECIAL_KEY_STORE;
		}

		/* Handle "building doors" -KMW- */
		else if ((c_ptr->feat >= FEAT_BLDG_HEAD) &&
		    (c_ptr->feat <= FEAT_BLDG_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter building */
			command_new = SPECIAL_KEY_BUILDING;
		}

		/* Handle quest areas -KMW- */
		else if (cave[y][x].feat == FEAT_QUEST_ENTER)
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter quest level */
			command_new = SPECIAL_KEY_QUEST;
		}

		else if (cave[y][x].feat == FEAT_QUEST_EXIT)
		{
			if (quest[p_ptr->inside_quest].type == QUEST_TYPE_FIND_EXIT)
			{
				quest[p_ptr->inside_quest].status = QUEST_STATUS_COMPLETED;
				quest[p_ptr->inside_quest].complev = (byte)p_ptr->lev;
#ifdef JP
				msg_print("クエストを達成した！");
#else
				msg_print("You accomplished your quest!");
#endif
	  			sound(SOUND_LEVEL); /* (Sound substitute) No quest sound */
				msg_print(NULL);
			}

			leave_quest_check();

			p_ptr->inside_quest = cave[y][x].special;
			dun_level = 0;
			p_ptr->oldpx = 0;
			p_ptr->oldpy = 0;
			p_ptr->leaving = TRUE;
		}

		/* Discover invisible traps */
		else if (c_ptr->feat == FEAT_INVIS)
		{
			/* Disturb */
			disturb(0, 0);

			/* Message */
#ifdef JP
			msg_print("トラップだ！");
#else
			msg_print("You found a trap!");
#endif


			/* Pick a trap */
			pick_trap(py, px);

			/* Hit the trap */
			hit_trap();
		}

		/* Set off an visible trap */
		else if (is_trap(c_ptr->feat))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hit the trap */
			hit_trap();
		}

		/* Warn when leaving trap detected region */
		if ((disturb_trap_detect || alert_trap_detect)
		    && p_ptr->dtrap && !(cave[py][px].info & CAVE_IN_DETECT))
		{
			/* No duplicate warning */
			p_ptr->dtrap = FALSE;

			/* You are just on the edge */
			if (!(cave[py][px].info & CAVE_UNSAFE))
			{
				if (alert_trap_detect)
				{
#ifdef JP
					msg_print("* 注意:この先はトラップの感知範囲外です！ *");
#else
					msg_print("*Leaving trap detect region!*");
#endif
				}
				
				if (disturb_trap_detect)
				{
					disturb(0, 0);
				}
			}
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

	if (find_ignore_trees)
	{
		if (cave[y][x].feat == FEAT_TREES) return (FALSE);
	}

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

	/* break run when leaving trap detected region */
	if ((disturb_trap_detect || alert_trap_detect)
	    && p_ptr->dtrap && !(cave[py][px].info & CAVE_IN_DETECT))
	{
		/* No duplicate warning */
		p_ptr->dtrap = FALSE;

		/* You are just on the edge */
		if (!(cave[py][px].info & CAVE_UNSAFE))
		{
			if (alert_trap_detect)
			{
#ifdef JP
				msg_print("* 注意:この先はトラップの感知範囲外です！ *");
#else
				msg_print("*Leaving trap detect region!*");
#endif
			}

			if (disturb_trap_detect)
			{
				/* Break Run */
				return(TRUE);
			}
		}
	}

	/* Look at every newly adjacent square. */
	for (i = -max; i <= max; i++)
	{
		s16b this_o_idx, next_o_idx;

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
			if (o_ptr->marked & OM_FOUND) return (TRUE);
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
				case FEAT_PERM_EXTRA:
				case FEAT_PERM_INNER:
				case FEAT_PERM_OUTER:
				case FEAT_PERM_SOLID:
				/* dirt, grass, trees, ... */
				case FEAT_SHAL_WATER:
				case FEAT_DIRT:
				case FEAT_GRASS:
				case FEAT_TREES:
				case FEAT_DARK_PIT:
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
					if (p_ptr->invuln || p_ptr->immune_fire) notice = FALSE;

					/* Done */
					break;
				}

				case FEAT_DEEP_WATER:
				{
					/* Ignore */
					if (p_ptr->ffall) notice = FALSE;

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
				case FEAT_LESS_LESS:
				case FEAT_MORE_MORE:
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
		if (inv || cave_floor_bold(row, col) ||
		    (find_ignore_trees && (cave[row][col].feat == FEAT_TREES)))
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
#ifdef JP
			msg_print("その方向には行けません。");
#else
			msg_print("You cannot run in that direction.");
#endif

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
	move_player(find_current, FALSE);
}


#ifdef TRAVEL
/*
 * Test for traveling
 */
static bool travel_test(void)
{
	int prev_dir, new_dir = 0;
	int row, col;
	int i, max;
	bool stop = TRUE;
	cave_type *c_ptr;

	/* Where we came from */
	prev_dir = find_prevdir;

	/* Range of newly adjacent grids */
	max = (prev_dir & 0x01) + 1;

	for (i = 0; i < 8; i++)
	{
		if (travel.cost[py+ddy_ddd[i]][px+ddx_ddd[i]] < travel.cost[py][px]) stop = FALSE;
	}

	if (stop) return (TRUE);

	/* break run when leaving trap detected region */
	if ((disturb_trap_detect || alert_trap_detect)
	    && p_ptr->dtrap && !(cave[py][px].info & CAVE_IN_DETECT))
	{
		/* No duplicate warning */
		p_ptr->dtrap = FALSE;

		/* You are just on the edge */
		if (!(cave[py][px].info & CAVE_UNSAFE))
		{
			if (alert_trap_detect)
			{
#ifdef JP
				msg_print("* 注意:この先はトラップの感知範囲外です！ *");
#else
				msg_print("*Leaving trap detect region!*");
#endif
			}

			if (disturb_trap_detect)
			{
				/* Break Run */
				return(TRUE);
			}
		}
	}

	/* Cannot travel when blind */
	if (p_ptr->blind || no_lite())
	{
#ifdef JP
		msg_print("目が見えない！");
#else
		msg_print("You cannot see!");
#endif
		return (TRUE);
	}

	/* Look at every newly adjacent square. */
	for (i = -max; i <= max; i++)
	{
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
	}

	/* Failure */
	return (FALSE);
}


/*
 * Travel command
 */
void travel_step(void)
{
	int i;
	int dir = travel.dir;
	int old_run = travel.run;

	find_prevdir = dir;

	/* disturb */
	if (travel_test())
	{
		disturb(0, 0);
		return;
	}

	energy_use = 100;

	for (i = 1; i <= 9; i++)
	{
		if (i == 5) continue;

		if (travel.cost[py+ddy[i]][px+ddx[i]] < travel.cost[py+ddy[dir]][px+ddx[dir]])
		{
			dir = i;
		}
	}

	/* Close door */
	if (!easy_open && (cave[py+ddy[dir]][px+ddx[dir]].feat == FEAT_DOOR_HEAD))
	{
		disturb(0, 0);
		return;
	}

	travel.dir = dir;
	move_player(dir, FALSE);
	travel.run = old_run;

	if ((py == travel.y) && (px == travel.x))
		travel.run = 0;
	else
		travel.run--;
}
#endif
