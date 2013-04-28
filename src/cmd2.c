/* File: cmd2.c */

/*
 * Go up and down stairs, toggle sneaking.  Handle chests.  Handle doors,
 * tunnel, disarm, bash, alter a grid, and spike.  Movement and resting
 * commands.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"



/*
 * Use an up staircase
 */
void do_cmd_go_up(void)
{
	/* Verify stairs */
	if (!cave_up_stairs(p_ptr->py, p_ptr->px))
	{
		msg_print("I see no up staircase here.");
		return;
	}

	/* Ironman */
	if ((p_ptr->character_type == PCHAR_IRONMAN) && (!p_ptr->total_winner))
	{
		msg_print("The only way back is through Morgoth, Lord of Darkness!");
		return;
	}

	/* Hack -- take a turn */
	p_ptr->energy_use = 100;

	/* Success */
	message(MSG_STAIRS_UP, 0, "You enter a maze of up staircases.");

	/* Create a way back (usually) */
	p_ptr->create_stair = FEAT_MORE;

	/* New depth */
	p_ptr->depth--;

	/* Go up another level if allowed */
	if ((cave_feat[p_ptr->py][p_ptr->px] == FEAT_LESS2) &&
	    (!quest_check(p_ptr->depth)) && (p_ptr->depth > 0))
	{
		p_ptr->depth--;

		/* Create a way back (usually) */
		p_ptr->create_stair = FEAT_MORE2;
	}

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Use a down staircase
 */
void do_cmd_go_down(void)
{
	/* Verify stairs */
	if (!cave_down_stairs(p_ptr->py, p_ptr->px))
	{
		msg_print("I see no down staircase here.");
		return;
	}

	/* Hack -- take a turn */
	p_ptr->energy_use = 100;

	/* Success */
	message(MSG_STAIRS_DOWN, 0, "You enter a maze of down staircases.");

	/* Create a way back (usually) */
	p_ptr->create_stair = FEAT_LESS;

	/* New level */
	p_ptr->depth++;

	/* Go down another level if allowed */
	if ((cave_feat[p_ptr->py][p_ptr->px] == FEAT_MORE2) &&
	    (!quest_check(p_ptr->depth)) && (p_ptr->depth < MAX_DEPTH - 1))
	{
		p_ptr->depth++;

		/* Create a way back (usually) */
		p_ptr->create_stair = FEAT_LESS2;
	}

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Hack -- toggle sneaking mode
 */
void do_cmd_sneaking(void)
{
	/* Stop sneaking */
	if (p_ptr->sneaking)
	{
		/* Clear the sneaking flag */
		p_ptr->sneaking = FALSE;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Start sneaking */
	else if (can_use_talent(TALENT_SNEAKING, TALENT_UTILITY) > 0)
	{
		/* Set the sneaking flag */
		p_ptr->sneaking = TRUE;

		/* Update stuff */
		p_ptr->update |= (PU_BONUS);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_STATE | PR_SPEED);
	}
	else
	{
		msg_format("You are not good enough at Stealth to sneak effectively.");
	}
}


/*
 * Determine if a grid contains a chest
 */
static s16b chest_check(int y, int x)
{
	s16b this_o_idx, next_o_idx = 0;

	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Skip unknown chests XXX XXX */
		if (!o_ptr->marked) continue;

		/* Check for chest */
		if (o_ptr->tval == TV_CHEST) return (this_o_idx);
	}

	/* No chest */
	return (0);
}


/*
 * A function that returns the tval of the items that will be generated
 * when a chest is opened.  -LM-
 */
static byte get_choice(int lev)
{
	byte choice = rand_int(100);
	int tval = TV_NONE;


	/* Get the character specialty */
	(void)get_title(30, FALSE, FALSE);

	/* Sometimes go for a specialized item type */
	if (((p_ptr->specialty == SPECIALTY_FIGHT_BURGLAR) ||
	     (p_ptr->specialty == SPECIALTY_FIGHTER)) && (choice < 33))
	{
		int s = sweapon(inventory[INVEN_WIELD].tval);
		if (s == S_SWORD) return (TV_SWORD);
		if (s == S_HAFTED) return (TV_HAFTED);
		if (s == S_POLEARM) return (TV_POLEARM);
	}
	else if ((p_ptr->specialty == SPECIALTY_P_WIZARD) && (choice < 33) && (lev >= 40))
		return (TV_MAGIC_BOOK);
	else if ((p_ptr->specialty == SPECIALTY_H_WIZARD) && (choice < 20) && (lev >= 40))
		return (TV_MAGIC_BOOK);
	else if ((p_ptr->specialty == SPECIALTY_P_PRIEST) && (choice < 33) && (lev >= 40))
		return (TV_PRAYER_BOOK);
	else if ((p_ptr->specialty == SPECIALTY_H_PRIEST) && (choice < 20) && (lev >= 40))
		return (TV_PRAYER_BOOK);
	else if ((p_ptr->specialty == SPECIALTY_P_DRUID) && (choice < 33) && (lev >= 40))
		return (TV_NATURE_BOOK);
	else if ((p_ptr->specialty == SPECIALTY_H_DRUID) && (choice < 20) && (lev >= 40))
		return (TV_NATURE_BOOK);
	else if ((p_ptr->specialty == SPECIALTY_P_NECRO) && (choice < 33) && (lev >= 40))
		return (TV_DARK_BOOK);
	else if ((p_ptr->specialty == SPECIALTY_H_NECRO) && (choice < 20) && (lev >= 40))
		return (TV_DARK_BOOK);
	else if ((p_ptr->specialty == SPECIALTY_SWORDS) && (choice < 40))
		return (TV_SWORD);
	else if ((p_ptr->specialty == SPECIALTY_POLEARMS) && (choice < 40))
		return (TV_POLEARM);
	else if ((p_ptr->specialty == SPECIALTY_HAFTED) && (choice < 40))
		return (TV_HAFTED);
	else if ((p_ptr->specialty == SPECIALTY_CROSSBOW) && (choice < 40))
		return (TV_BOLT);
	else if ((p_ptr->specialty == SPECIALTY_BOW) && (choice < 40))
		return (TV_ARROW);
	else if ((p_ptr->specialty == SPECIALTY_SLING) && (choice < 40))
		return (TV_SHOT);
	else if ((p_ptr->specialty == SPECIALTY_DEVICE) && (choice < 40))
	{
		if (one_in_(3)) return (TV_STAFF);
		if (one_in_(2)) return (TV_WAND);
		return (TV_ROD);
	}


	/* Repeat until satisfied */
	while (TRUE)
	{
		/* Randomize */
		choice = rand_int(100);

		/* Random type of item */
		if      (choice <  4) tval = TV_COMPONENT;
		else if (choice <  8) tval = TV_LITE;
		else if (choice < 12) tval = TV_MAGIC_BOOK;
		else if (choice < 16) tval = TV_PRAYER_BOOK;
		else if (choice < 20) tval = TV_NATURE_BOOK;
		else if (choice < 24) tval = TV_DARK_BOOK;
		else if (choice < 28) tval = TV_SHOT;
		else if (choice < 32) tval = TV_ARROW;
		else if (choice < 36) tval = TV_BOLT;
		else if (choice < 40)
		{
			choice = randint(3);
			if ((choice == 1) &&
				(get_skill(S_SLING, 0, 100) >= p_ptr->power / 2)) tval = TV_SLING;
			else if ((choice == 2) &&
				(get_skill(S_BOW, 0, 100) >= p_ptr->power / 2)) tval = TV_BOW;
			else if ((choice == 3) &&
				(get_skill(S_CROSSBOW, 0, 100) >= p_ptr->power / 2)) tval = TV_CROSSBOW;
			else continue;
		}
		else if (choice < 44) tval = TV_WAND;
		else if (choice < 48) tval = TV_STAFF;
		else if (choice < 52) tval = TV_ROD;
		else if (choice < 56) tval = TV_HAFTED;
		else if (choice < 60) tval = TV_POLEARM;
		else if (choice < 64) tval = TV_SWORD;
		else if (choice < 67) tval = TV_BOOTS;
		else if (choice < 70) tval = TV_GLOVES;
		else if (choice < 73) tval = TV_HELM;
		else if (choice < 76) tval = TV_CROWN;
		else if (choice < 79) tval = TV_SHIELD;
		else if (choice < 82) tval = TV_CLOAK;
		else if (choice < 85) tval = TV_SOFT_ARMOR;
		else if (choice < 88) tval = TV_HARD_ARMOR;
		else if (choice < 91) tval = TV_SCROLL;
		else if (choice < 94) tval = TV_POTION;
		else if (choice < 97) tval = TV_RING;
		else                  tval = TV_AMULET;

		/* Reject item if useless */
		if ((tval == TV_LITE) && (lev >= 40)) tval = TV_COMPONENT;
		if ((tval == TV_SHOT) && (get_skill(S_SLING, 0, 100) < p_ptr->power / 2)) continue;
		if ((tval == TV_ARROW) && (get_skill(S_BOW, 0, 100) < p_ptr->power / 2)) continue;
		if ((tval == TV_BOLT) && (get_skill(S_CROSSBOW, 0, 100) < p_ptr->power / 2)) continue;
		if ((tval == TV_HAFTED) && (get_skill(S_HAFTED, 0, 100) < p_ptr->power / 2)) continue;
		if ((tval == TV_POLEARM) && (get_skill(S_POLEARM, 0, 100) < p_ptr->power / 2)) continue;
		if ((tval == TV_SWORD) && (get_skill(S_SWORD, 0, 100) < p_ptr->power / 2)) continue;
		if ((tval == TV_WAND) && (get_skill(S_DEVICE, 0, 100) < p_ptr->power / 2)) continue;
		if ((tval == TV_STAFF) && (get_skill(S_DEVICE, 0, 100) < p_ptr->power / 2)) continue;
		if ((tval == TV_ROD) && (get_skill(S_DEVICE, 0, 100) < p_ptr->power / 2)) continue;
		if ((tval == TV_COMPONENT) && (get_skill(S_FORGE_WEAPON, 0, 100) < p_ptr->power / 2) &&
		    (get_skill(S_FORGE_ARMOR, 0, 100) < p_ptr->power / 2)) continue;

		if ((tval == TV_MAGIC_BOOK)  && (p_ptr->realm != MAGE)) continue;
		if ((tval == TV_PRAYER_BOOK) && (p_ptr->realm != PRIEST)) continue;
		if ((tval == TV_NATURE_BOOK) && (p_ptr->realm != DRUID)) continue;
		if ((tval == TV_DARK_BOOK)   && (p_ptr->realm != NECRO)) continue;


		/* Accept item otherwise */
		return (tval);
	}

	/* Oops */
	return (0);
}

/*
 * Allocate objects upon opening a chest.
 *
 * Disperse treasures from the given chest, centered at (x,y).
 *
 * In Oangband, chests are nice finds.  Small chests distribute 3-4 items,
 * while large chests can distribute 5-6.  Item types are biased to be
 * useful for the character, and they can frequently be of good quality
 * (or better).  -LM-
 *
 * The "value" of the items in a chest is based on the "power" of the chest,
 * which is in turn based on the level on which the chest is generated.
 */
static void chest_death(bool scattered, int y, int x, object_type *o_ptr)
{
	int number, i;
	bool obj_success = FALSE;

	object_type *i_ptr;
	object_type object_type_body;


	/* Determine how much to drop. */
	if (o_ptr->sval >= SV_CHEST_MIN_LARGE) number = rand_range(5, 6);
	else number = rand_range(3, 4);

	/* Zero pval means empty chest */
	if (!o_ptr->pval) number = 0;

	/* Set object level to the power of the chest */
	object_level = ABS(o_ptr->pval);

	/* Select an item type that the chest will disperse. */
	required_tval = get_choice(object_level);

	/* Drop some objects */
	for (i = 0; i < number; i++)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/*
		 * Make an object with a specified tval.  Grant a possibility for
		 * items to be forced good, or even great.  With the new definitions
		 * of goodness, this can make for quite interesting loot.  -LM-
		 */
		switch (required_tval)
		{
			case TV_HARD_ARMOR:
			case TV_SOFT_ARMOR:
			case TV_DRAG_ARMOR:
			case TV_SHIELD:
			case TV_CLOAK:
			case TV_BOOTS:
			case TV_GLOVES:
			case TV_HELM:
			case TV_CROWN:
			case TV_SLING:
			case TV_BOW:
			case TV_CROSSBOW:
			case TV_SWORD:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_DIGGING:
			case TV_SHOT:
			case TV_BOLT:
			case TV_ARROW:
			{
				if (rand_int(200) < object_level)
				{
					obj_success = make_object(i_ptr, TRUE, TRUE, TRUE);
					break;
				}

				else if (rand_int(40) < object_level)
				{
					obj_success = make_object(i_ptr, TRUE, FALSE, TRUE);
					break;
				}
				else
				{
					obj_success = make_object(i_ptr, FALSE, FALSE, TRUE);
					break;
				}
			}

			case TV_LITE:
			case TV_MAGIC_BOOK:
			case TV_PRAYER_BOOK:
			case TV_NATURE_BOOK:
			case TV_DARK_BOOK:
			{
				if (rand_int(80) < object_level)
				{
					obj_success = make_object(i_ptr, TRUE, FALSE, TRUE);
				}
				else
				{
					obj_success = make_object(i_ptr, FALSE, FALSE, TRUE);
				}

				break;
			}

			case TV_SCROLL:
			case TV_POTION:
			case TV_RING:
			case TV_AMULET:
			case TV_WAND:
			case TV_STAFF:
			case TV_ROD:
			{
				if (rand_int(100) < (object_level - 10) / 2)
				{
					obj_success = make_object(i_ptr, TRUE, FALSE, TRUE);
				}

				else
				{
					obj_success = make_object(i_ptr, FALSE, FALSE, TRUE);
				}

				break;
			}

			default:
			{
				obj_success = make_object(i_ptr, FALSE, FALSE, TRUE);
				break;
			}
		}

		/* If no object was made, we need to try another tval.  XXX */
		if ((!obj_success) && (i == 0))
		{
			required_tval = get_choice(object_level);
			i--;
		}

		/* Otherwise, we can place the object */
		else
		{
			/* If chest scatters its contents, pick any floor square. */
			if (scattered)
			{
				int j;

				for (j = 0; j < 200; j++)
				{
					/* Pick a totally random spot. */
					y = rand_int(dungeon_hgt);
					x = rand_int(dungeon_wid);

					/* Grid must be capable of (safely) containing an object. */
					if (!cave_allow_object_bold(y, x)) continue;

					/* Place the object there. */
					drop_near(i_ptr, 0, y, x, 0x00);

					/* Done. */
					break;
				}
			}

			/* Normally, drop object near the chest. */
			else drop_near(i_ptr, 0, y, x, 0x00);
		}
	}

	/* Clear required tval */
	required_tval = 0;

	/* Reset the object level */
	object_level = p_ptr->depth;


	/* Clear restriction */
	get_obj_num_hook = NULL;

	/* Prepare allocation table */
	get_obj_num_prep();


	/* Empty */
	o_ptr->pval = 0;

	/* Known */
	object_known(o_ptr);
}


/*
 * Given a chest, determine what traps it has, if any.
 *
 * At present, we allow a chest to have only one trap.
 */
int check_chest_traps(const object_type *o_ptr, bool had_traps)
{
	/* Get chest level (corresponds to the equivalent dungeon level) */
	int level = o_ptr->pval;
	int trap;

	/* Require a chest */
	if (o_ptr->tval != TV_CHEST) return (0);

	/* Check if chest had traps */
	if (had_traps) level = ABS(o_ptr->pval);

	/* Chest has (or had) no traps */
	if (level <= 0) return (0);


	/* Use the quick RNG */
	Rand_quick = TRUE;

	/* A given pval always produces the same traps */
	Rand_value = level;

	/* Chests, especially low-level ones, may have no traps at all */
	if (randint(level + 4) <= 4)
	{
		Rand_quick = FALSE;
		return (0);
	}


	/*** Given chest level, choose a trap ***/


	/* Poison, disease, or paralyzation */
	if ((level < 30) && (one_in_(3)))
	{
		trap = CHEST_GAS;
	}

	/* Drain stats */
	else if ((level < 35) && (one_in_(3)))
	{
		trap = CHEST_DRAIN_STAT;
	}

	/* Summoning */
	else if (one_in_(4))
	{
		trap = CHEST_SUMMON;
	}

	/* Chests does weird things */
	else if ((level < 35) || (one_in_(2)))
	{
		trap = CHEST_WEIRD;
	}

	/* Smash mind, curse, etc. (only at level 35 and above) */
	else
	{
		trap = CHEST_CURSE;
	}

	/* Use the standard RNG */
	Rand_quick = FALSE;

	/* Return the trap type */
	return (trap);
}


/*
 * Chests have traps too.  High-level chests can be very dangerous, no
 * matter what level they are opened at.  Various traps added in Oangband
 * and Sangband.  -LM-
 *
 * Traps eventually break; a character with no disarming skill can
 * get into a chest, but it's painful...
 *
 * Return TRUE if chest was destroyed.
 */
bool hit_chest_trap(int y, int x, object_type *o_ptr)
{
	int i, trap;
	int break_prob = 5;
	bool destroy_chest = FALSE;

	/* Ignore disarmed chests */
	if (o_ptr->pval <= 0) return (FALSE);

	/* Get the trap (if any) */
	trap = check_chest_traps(o_ptr, FALSE);

	/* Use the quick RNG */
	Rand_quick = TRUE;

	/* A given pval always produces the same trap effects */
	Rand_value = o_ptr->pval;


	/* Drain a stat */
	if (trap == CHEST_DRAIN_STAT)
	{
		/* May drain any of the first six stats */
		int choice = randint(6);

		/* Usually drain one point */
		int d = 1;

		/* Allow nasty draining */
		if (o_ptr->pval > rand_range(15, 40)) d = randint(5);

		/* Drain strength */
		if (choice == 1)
		{
			(void)take_hit(damroll(1, 4), 0, "A small needle has pricked you!",
				"a poison needle");
			(void)do_dec_stat(A_STR, d, FALSE, "You feel weakened!", NULL);
		}
		else if (choice == 2)
		{
			(void)take_hit(damroll(1, 4), 0, "A small needle has pricked you!",
				"a poison needle");
			(void)do_dec_stat(A_CON, d, FALSE, "Your health is damaged!",
				"Your body resists the effects of the disease.");
		}
		else if (choice == 3)
		{
			(void)take_hit(damroll(1, 4), 0, "A small needle has pricked you!",
				"a poison needle");
			(void)do_dec_stat(A_DEX, d, FALSE, "Your nerves go limp!",
				"Your dexterity is sustained.");
		}
		else if (choice == 4)
		{
			(void)take_hit(damroll(1, 4), 0, "Something is invading your mind!",
				"a poison needle");
			(void)do_dec_stat(A_INT, d, FALSE, "Your intelligence drains away!",
				"You feel dull-witted for a moment, but quickly recover.");
		}
		else if (choice == 5)
		{
			(void)take_hit(damroll(1, 4), 0, "Something is crushing your mind!",
				"a poison needle");
			(void)do_dec_stat(A_WIS, d, FALSE, "You suddenly feel naive!",
				"Your wisdom resists the attack.");
		}
		else if (choice == 6)
		{
			(void)take_hit(damroll(1, 4), 0, "Something is clawing at your mind!",
				"a poison needle");
			(void)do_dec_stat(A_CHR, d, FALSE, "Your charisma diminishes!",
				"Your charisma is sustained.");
		}
	}

	/* Gas traps */
	else if (trap == CHEST_GAS)
	{
		/* Lingering poison cloud -- only at level 20 and higher */
		if ((o_ptr->pval >= 20) && (one_in_(3)))
		{
			/* Get a new effect index */
			i = effect_prep();

			/* Require success */
			if (i >= 0)
			{
				/* Fair warning */
				msg_print("You are surrounded by a deadly cloud of venom!");

				/* We want an lingering cloud, */
				x_list[i].index = EFFECT_IRREGULAR_CLOUD;

				/* Of poison */
				x_list[i].type = GF_POIS;

				/* That starts at the character location. */
				x_list[i].y0 = x_list[i].y1 = p_ptr->py;
				x_list[i].x0 = x_list[i].x1 = p_ptr->px;

				/* Practices no skills */
				x_list[i].practice_skill = (byte) S_NOSKILL;

				/* It attacks every 10 game turns, */
				x_list[i].time_delay = 10;

				/* Does damage, has a large radius, */
				x_list[i].power = 10;
				x_list[i].power2 = 7;

				/* And lasts for 25 attacks */
				x_list[i].lifespan = 25;
			}
		}

		/* Disease -- only at level 15 and higher */
		if ((o_ptr->pval >= 15) && (one_in_(3)))
		{
			int pow = o_ptr->pval;

			msg_print("The lock ejects a cloud of spores!");

			/* Details are random */
			Rand_quick = FALSE;

			/* Cause disease */
			disease(&pow);
		}

		/* Paralyze -- only at level 10 and higher */
		else if (randint(o_ptr->pval) >= 10)
		{
			/* Details are random */
			Rand_quick = FALSE;

			msg_print("A puff of yellow gas surrounds you.");
			if (!p_ptr->free_act)
			{
				msg_print("You choke and pass out!");
				(void)set_paralyzed(p_ptr->paralyzed + rand_range(20, 30));
			}
			else
			{
				msg_print("You are unaffected.");
			}
		}

		/* Poisoning */
		else
		{
			/* Details are random */
			Rand_quick = FALSE;

			msg_print("A puff of green gas surrounds you!");
			if (!p_ptr->resist_pois || !p_ptr->oppose_pois)
			{
				if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
				{
					(void)set_poisoned(p_ptr->poisoned + rand_range(20, 30));
				}
				else
				{
					(void)set_poisoned(p_ptr->poisoned + rand_range(10, 15));
				}
			}
		}
	}

	/* Summon monsters */
	else if (trap == CHEST_SUMMON)
	{
		int choice = randint(7);
		int summon_level = o_ptr->pval + 3;

		/* Details are random */
		Rand_quick = FALSE;

		/* Summon sound */
		sound(MSG_SUM_MONSTER);

		/* Summon monsters */
		if (choice <= 2)
		{
			msg_print("You are enveloped in a cloud of smoke!");
			(void)summon_specific(y, x, FALSE, summon_level, 0, rand_range(3, 6));
		}

		/* Summon elementals */
		else if (choice == 3)
		{
			msg_print("Elemental beings appear to protect their treasures!");
			(void)summon_specific(y, x, FALSE, summon_level,
					SUMMON_ELEMENTAL, rand_range(4, 8));
		}

		/* Summon demons */
		else if (choice == 4)
		{
			msg_print("Demons materialize in clouds of fire and brimstone!");

			(void)explosion(0, 4, y, x, o_ptr->pval / 2, GF_FIRE);

			(void)summon_specific(y, x, FALSE, summon_level,
					SUMMON_DEMON, rand_range(3, 6));
		}

		/* Summon dragons */
		else if (choice == 5)
		{
			msg_print("Draconic forms loom out of the darkness!");

			(void)summon_specific(y, x, FALSE, summon_level,
					SUMMON_DRAGON, rand_range(3, 4));
		}

		/* Summon hybrids */
		else if (choice == 6)
		{
			msg_print("Creatures strange and twisted assault you!");

			(void)summon_specific(y, x, FALSE, summon_level,
				SUMMON_HYBRID, rand_range(4, 8));
		}

		/* Summon vortices (scattered) */
		else
		{
			msg_print("Vortices coalesce and wreak destruction!");

			(void)summon_specific(y, x, TRUE, summon_level,
					SUMMON_VORTEX, rand_range(3, 7));
		}

		/* Summon traps break more quickly */
		break_prob = 2;
	}

	/* Chest-animation */
	else if (trap == CHEST_WEIRD)
	{
		/* Totally random effects */
		Rand_quick = FALSE;

		/* Chest turns into a chest mimic */
		if ((o_ptr->pval >= r_info[MON_CHEST_MIMIC].level - 5) &&
			 (one_in_(2)))
		{
			/* XXX - hard-coding.  Really need to find a better way. */
			summon_index_type = MON_CHEST_MIMIC;

			/* Attempt to summon a chest mimic */
			if (summon_specific(y, x, FALSE, 100, SUMMON_INDEX, 1))
			{
				msg_print("The chest morphs before your eyes!");
				destroy_chest = TRUE;
			}
		}

		/* Shrieking chest */
		else if (randint(o_ptr->pval) <= 6)
		{
			/* Aggravate monsters */
			aggravate_monsters(-1, FALSE, "The chest shrieks for help!");
		}

		/* Jumping chest */
		else if (randint(o_ptr->pval) <= 16)
		{
			int j;
			int y1, x1;

			for (j = 0; j < 200; j++)
			{
				/* Pick a totally random spot. */
				y1 = rand_int(dungeon_hgt);
				x1 = rand_int(dungeon_wid);

				/* Grid must be capable of (safely) containing an object. */
				if (!cave_allow_object_bold(y1, x1)) continue;

				/* Place the chest there. */
				drop_near(o_ptr, 0, y1, x1, 0x00);

				/* Delete the original chest (later) */
				destroy_chest = TRUE;

				/* Done. */
				break;
			}

			/* Message */
			if (!player_can_see_or_infra_bold(y1, x1))
				msg_print("The chest vanishes!");
			else if ((y1 != y) || (x1 != x))
				msg_print("The chest blinks away!");
		}

		/* Chest scatters contents */
		else if (!one_in_(3))
		{
			msg_print("The contents of the chest scatter all over the dungeon!");
			chest_death(TRUE, y, x, o_ptr);
			o_ptr->pval = 0;
		}

		/* Exploding chest */
		else
		{
			msg_print("The chest explodes violently!");
			destroy_chest = TRUE;

			/* Shards -- Full damage to adjacent grids */
			(void)project_ball(0, 3, y, x, y, x, o_ptr->pval * 4,
				GF_SHARD, 0L, 20);
		}
	}

	/* Cursing, mind-smashing, and dispelling */
	else if (trap == CHEST_CURSE)
	{
		/* Curse character */
		if (randint(o_ptr->pval) <= 25)
		{
			/* Details are random */
			Rand_quick = FALSE;

			curse_player(o_ptr->pval);
		}

		/* Smash mind */
		else if (randint(o_ptr->pval) <= 50)
		{
			/* Details are random */
			Rand_quick = FALSE;

			msg_print("You feel something focusing on your mind.");

			/* Project a mental attack */
			(void)explosion(0, 0, p_ptr->py, p_ptr->px, o_ptr->pval, GF_PSI);
		}

		/* Dispel character */
		else
		{
			/* Determine how many nasty tricks can be played. */
			int nasty_tricks_count = 3 + o_ptr->pval / 33;

			/* Details are random */
			Rand_quick = FALSE;

			/* Message. */
			message(TERM_VIOLET, 100,
				"Hideous voices bid: 'Let the darkness have thee!'");

			/* This is gonna hurt... */
			for (; nasty_tricks_count > 0; nasty_tricks_count--)
			{
				/* ...but a high saving throw does help a little. */
				if (!check_save(2 * o_ptr->pval))
				{
					int choice = rand_int(6);

					if (choice == 0)
					{
						take_hit(damroll(5, 20), 0, NULL, "a chest dispel-player trap");
					}
					else if (choice == 1)
					{
						(void)set_cut(p_ptr->cut + 200);
					}
					else if (choice == 2)
					{
						if (!p_ptr->free_act)
							(void)set_paralyzed(p_ptr->paralyzed + rand_range(4, 8));
						else
							(void)set_stun(p_ptr->stun + rand_range(80, 120));
					}
					else if (choice == 3)
					{
						apply_disenchant(0);
					}
					else if (choice == 4)
					{
						(void)do_dec_stat(A_STR, 1, FALSE, NULL, NULL);
						(void)do_dec_stat(A_DEX, 1, FALSE, NULL, NULL);
						(void)do_dec_stat(A_CON, 1, FALSE, NULL, NULL);
						(void)do_dec_stat(A_INT, 1, FALSE, NULL, NULL);
						(void)do_dec_stat(A_WIS, 1, FALSE, NULL, NULL);
						(void)do_dec_stat(A_CHR, 1, FALSE, NULL, NULL);
					}
					else
					{
						(void)explosion(0, 4, y, x, 4 * o_ptr->pval, GF_NETHER);
					}
				}
			}
		}
	}

	/* Use the standard RNG */
	Rand_quick = FALSE;

	/* Break the trap sometimes */
	if (!destroy_chest && o_ptr->tval && one_in_(break_prob))
	{
		msg_print("You hear a click.");
		o_ptr->pval = 0 - o_ptr->pval;
	}

	/* Note if we have to delete the chest */
	return (destroy_chest);
}


/*
 * Attempt to open the given chest at the given location.  Assume that
 * there is no monster blocking the way.
 *
 * Returns TRUE if repeated commands may (and need to) continue.
 */
static bool do_cmd_open_chest(int y, int x, s16b o_idx)
{
	int skill, chance;
	object_type *o_ptr = &o_list[o_idx];

	/* Can open unlocked chests */
	bool can_open = (bool)(o_ptr->pval <= 0);

	/* Assume no repeated action needed or allowed */
	bool more = FALSE;


	/* You have to disarm a trapped chest before you can open it */
	if (check_chest_traps(o_ptr, FALSE))
	{
		/* Learn about traps the hard way */
		object_known(o_ptr);

		/* Message */
		msg_print("You set off a trap!");

		/* Activate the traps */
		if (hit_chest_trap(y, x, o_ptr))
		{
			/* Chest needs to be destroyed */
			delete_object_idx(o_idx);
		}

		/* Cancel action */
		return (FALSE);
	}

	/* Attempt to unlock the chest */
	if (o_ptr->pval > 0)
	{
		/* Get lock-picking skill (disarming plus 5) */
		skill = 5 + p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind    || (no_light() && (p_ptr->see_infra <= 0)))   skill /= 10;
		if (p_ptr->confused || p_ptr->image) skill /= 10;
		if (p_ptr->berserk)                  skill /= 2;

		/* Chance to pick the lock (no guarantee) */
		chance = skill - o_ptr->pval;

		/* High disarming improves success with lower-level locks */
		if (o_ptr->pval <= get_skill(S_DISARM, 0, 100))
		{
			if (chance < get_skill(S_DISARM, 0, 100))
			    chance = get_skill(S_DISARM, 0, 100);
		}

		/* No chance */
		if (chance <= 0)
		{
			/* Message */
			msg_print("You fumble with the lock, getting nowhere.");

			/* Stop */
		}

		/* Success */
		else if (rand_int(100) < chance)
		{
			/* Message */
			msg_print("You have picked the lock.");

			/* Gain a little experience */
			gain_exp(1, S_DISARM);

			/* Can now open the chest */
			can_open = TRUE;
		}

		/* Failure -- Keep trying */
		else
		{
			/* We may continue repeating */
			more = TRUE;

			/* Optional flush, message */
			if (flush_failure) flush();
			message(MSG_LOCKPICK_FAIL, 0, "You failed to pick the lock.");
		}
	}

	/* Allowed to open */
	if (can_open)
	{
		/* Let the chest drop items */
		chest_death(FALSE, y, x, o_ptr);
	}

	/* Result */
	return (more);
}


/*
 * Attempt to disarm the chest at the given location.  Assume that there
 * is no monster blocking the way.
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_chest(int y, int x, s16b o_idx)
{
	int skill, chance;

	bool more = FALSE;
	bool safe = FALSE;

	object_type *o_ptr = &o_list[o_idx];


	/* Must find the trap first. */
	if (!object_known_p(o_ptr))
	{
		msg_print("I don't see any traps.");
		return (FALSE);
	}

	/* No traps to find. */
	if (!check_chest_traps(o_ptr, FALSE))
	{
		/* Had traps, but not any more */
		if (check_chest_traps(o_ptr, TRUE))
		{
			msg_print("You have already disarmed this chest.");
		}
		else
		{
			msg_print("This chest has no traps.");
		}
		return (FALSE);
	}


	/* Get disarming skill */
	skill = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind    || (no_light() && (p_ptr->see_infra <= 0)))   skill /= 10;
	if (p_ptr->confused || p_ptr->image) skill /= 10;
	if (p_ptr->berserk)                  skill /= 2;

	/* Chance to disarm the chest (always at least 1%) */
	chance = MAX(1, skill - o_ptr->pval);

	/* High disarming renders one safe from lower-level traps */
	if (o_ptr->pval + 10 <= get_skill(S_DISARM, 0, 110)) safe = TRUE;


	/* Attempt to disarm the chest */
	if (rand_int(100) < chance)
	{
		/* Message */
		message_format(MSG_DISARM, 0, "You have disarmed the chest.");

		/* Gain some experience */
		gain_exp(randint(o_ptr->pval + (o_ptr->pval * o_ptr->pval / 20)),
			S_DISARM);

		/* Hack -- mark the chest as being untrapped */
		o_ptr->pval = (0 - o_ptr->pval);
	}

	/* Failure -- Keep trying */
	else if (safe || randint(chance) > 5)
	{
		/* We may keep trying */
		more = TRUE;

		/* Optional flush, message */
		if (flush_failure) flush();
		msg_print("You failed to disarm the chest.");
	}

	/* Failure -- Set off the trap */
	else
	{
		msg_print("You set off a trap!");

		/* Activate the traps */
		if (hit_chest_trap(y, x, o_ptr))
		{
			/* Chest needs to be destroyed */
			delete_object_idx(o_idx);
		}
	}

	/* Result */
	return (more);
}

/*
 * Return TRUE if the given feature is a closed door
 */
static bool is_closed(int feat)
{
	return ((feat >= FEAT_DOOR_HEAD) &&
	        (feat <= FEAT_DOOR_TAIL));
}


/*
 * Return the number of doors/traps around (or under) the character.
 */
static int count_feats(int *y, int *x, bool (*test)(int feat), bool under)
{
	int d;
	int xx, yy;
	int count = 0; /* Count how many matches */

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* Option -- search under the character */
		if ((d == 8) && !under) continue;

		/* Extract adjacent (legal) location */
		yy = p_ptr->py + ddy_ddd[d];
		xx = p_ptr->px + ddx_ddd[d];

		/* Paranoia */
		if (!in_bounds_fully(yy, xx)) continue;

		/* Must have knowledge */
		if (!(cave_info[yy][xx] & (CAVE_MARK))) continue;

		/* Not looking for this feature */
		if (!((*test)(cave_feat[yy][xx]))) continue;


		/* Count it */
		count++;

		/* Remember the location of the last feature of this type found */
		*y = yy;
		*x = xx;
	}

	/* All done */
	return (count);
}

/*
 * Return the number of chests around (or under) the character.  -TNB-
 * If requested, count only trapped chests.
 */
static int count_chests(int *y, int *x, bool trapped)
{
	int d, count, o_idx;

	object_type *o_ptr;

	/* Count how many matches */
	count = 0;

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent (legal) location */
		int yy = p_ptr->py + ddy_ddd[d];
		int xx = p_ptr->px + ddx_ddd[d];

		/* No (visible) chest is there */
		if ((o_idx = chest_check(yy, xx)) == 0) continue;

		/* Grab the object */
		o_ptr = &o_list[o_idx];

		/* Already open */
		if (o_ptr->pval == 0) continue;

		/* Option -- only search for chests with known traps */
		if (trapped)
		{
			/* Require known traps */
			if (!object_known_p(o_ptr)) continue;
			if (!check_chest_traps(o_ptr, FALSE)) continue;
		}

		/* OK */
		count++;

		/* Remember the location of the last chest found */
		*y = yy;
		*x = xx;
	}

	/* All done */
	return (count);
}

/*
 * Convert an adjacent location to a direction.
 */
static int coords_to_dir(int y, int x)
{
	int d[3][3] =
	{
		{ 7, 4, 1 },
		{ 8, 5, 2 },
		{ 9, 6, 3 }
	};
	int dy, dx;

	dy = y - p_ptr->py;
	dx = x - p_ptr->px;

	/* Ensure legal table access */
	if (ABS(dx) > 1 || ABS(dy) > 1) return (0);

	return (d[dx + 1][dy + 1]);
}


/*
 * Perform the basic "open" command on doors
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Must be a closed door */
	if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
	      (cave_feat[y][x] <= FEAT_DOOR_TAIL)))
	{
		/* Message */
		message(MSG_NOTHING_TO_OPEN, 0, "You see nothing there to open.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Perform the basic "open" command on doors
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_aux(int y, int x)
{
	int skill, j;

	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_open_test(y, x)) return (FALSE);


	/* Jammed door (up to level 8)  XXX XXX */
	if (cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Stuck */
		msg_print("The door appears to be stuck.");
	}

	/* Locked door (up to level 7)  XXX XXX */
	else if (cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x01)
	{
		/* Get lock-picking skill (disarming plus 5) */
		skill = 5 + p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind    || (no_light() && (p_ptr->see_infra <= 0)))   skill /= 10;
		if (p_ptr->confused || p_ptr->image) skill /= 10;

		/* Extract the lock power  (change the coefficient as needed) XXX */
		j = skill - (cave_feat[y][x] - FEAT_DOOR_HEAD) * 6;

		/* No chance */
		if (j <= 0)
		{
			/* Message */
			msg_print("You fumble with the lock, getting nowhere.");

			/* Stop */
		}

		/* Success (at least 4% chance, if any chance at all) */
		else if (rand_int(100) < MAX(4, j))
		{
			/* Message */
			message(MSG_OPENDOOR, 0, "You have picked the lock.");

			/* Open the door */
			cave_set_feat(y, x, FEAT_OPEN);

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

			/* Experience -- unless able to lock doors */
			if (get_skill(S_BURGLARY, 0, 100) < LEV_REQ_LOCK_DOOR)
				gain_exp(1, S_DISARM);
		}

		/* Failure */
		else
		{
			/* Failure */
			if (flush_failure) flush();

			/* Message */
			message(MSG_LOCKPICK_FAIL, 0, "You failed to pick the lock.");

			/* We may keep trying */
			more = TRUE;
		}
	}

	/* Closed door */
	else
	{
		/* Open the door */
		cave_set_feat(y, x, FEAT_OPEN);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Sound */
		sound(MSG_OPENDOOR);
	}

	/* Result */
	return (more);
}



/*
 * Open a closed/locked/jammed door or a closed/locked chest.
 *
 * Unlocking a locked door/chest is worth one experience point (unless
 * you can lock doors, in which case it is worth nothing).
 */
void do_cmd_open(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	s16b o_idx;

	bool more = FALSE;

	int num_doors, num_chests;

	/* Count closed doors */
	num_doors = count_feats(&y, &x, is_closed, FALSE);

	/* Count chests (locked) */
	num_chests = count_chests(&y, &x, FALSE);

	/* See if only one target  -TNB- */
	if ((num_doors + num_chests) == 1)
	{
		p_ptr->command_dir = coords_to_dir(y, x);
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Check for chests */
	o_idx = chest_check(y, x);


	/* Verify legality */
	if (!o_idx && !do_cmd_open_test(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Check for chest */
		o_idx = chest_check(y, x);
	}


	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack -- usually do not move */
		if (py_attack(y, x)) return;
	}

	/* Chest */
	if (o_idx)
	{
		/* Open the chest */
		more = do_cmd_open_chest(y, x, o_idx);
	}

	/* Door */
	else
	{
		/* Open the door */
		more = do_cmd_open_aux(y, x);
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0, 0);
}



/*
 * Attempt to force back a monster barring a doorway.  -LM-
 *
 * Works 3 times in 4 against all monsters that do not multiply, and 1
 * time in 3 against those that do.
 *
 * We could test DEX, STR, power, monster level, etc., but we need to
 * make sure that door slamming is something apprentice mages can do.
 */
static bool retreat_monster(int y, int x)
{
	int i, dir0, dir, ny, nx;

	/* Character location */
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Get the monster */
	monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Get the monster location (paranoia) */
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;


	/* Get the direction from the character to the monster */
	for (dir0 = 1; dir0 < 10; dir0++)
	{
		/* Scan adjacent grids */
		ny = py + ddy[dir0];
		nx = px + ddx[dir0];

		/* The monster is here */
		if ((fy == ny) && (fx == nx))
		{
			/* Use this direction */
			break;
		}
	}

	/* Monster cannot be found adjacent to the character -- return */
	if (dir0 >= 10) return (FALSE);

	/* Use left-hand bias 50% of the time */
	if (rand_int(2)) dir0 += 10;


	/*
	 * Scan each of the eight possible directions, in the order of
	 * priority given by the table "side_dirs", choosing the one that
	 * looks like it will get the monster away from the character most
	 * effectively.
	 */
	for (i = 0; i <= 8; i++)
	{
		/* Out of options */
		if (i == 8) break;

		/* Get the actual direction */
		dir = side_dirs[dir0][i];

		/* Get the grid in our chosen direction */
		ny = fy + ddy[dir];
		nx = fx + ddx[dir];

		/* The monster can exist in this grid */
		if (cave_exist_mon(r_ptr, ny, nx, FALSE, FALSE))
		{
			/* Does the monster multiply (must not be confused)? */
			bool breed = ((r_ptr->flags2 & (RF2_MULTIPLY)) && (!m_ptr->confused));

			/* Roll for success */
			if (((!breed) && (!one_in_(4))) || ((breed) && (one_in_(3))))
			{
				/* Move the monster to it */
				monster_swap(fy, fx, ny, nx);

				/* Report success */
				return (TRUE);
			}

			/* Failed to retreat the monster */
			return (FALSE);
		}
	}

	/* We could find no clear space -- report failure */
	return (FALSE);
}



/*
 * Determine if a given grid may be "closed"
 */
static bool do_cmd_close_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require open/broken door */
	if ((cave_feat[y][x] != FEAT_OPEN) &&
	    (cave_feat[y][x] != FEAT_BROKEN))
	{
		/* Message */
		msg_print("You see nothing there to close.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Perform the basic "close" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_close_aux(int y, int x)
{
	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_close_test(y, x)) return (FALSE);


	/* Broken door */
	if (cave_feat[y][x] == FEAT_BROKEN)
	{
		/* Message */
		msg_print("The door appears to be broken.");
		p_ptr->energy_use = 0;
	}

	/* Open door */
	else
	{
		/* Close the door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Sound */
		sound(MSG_SHUTDOOR);
	}

	/* Result */
	return (more);
}


/*
 * Close an open door.
 */
void do_cmd_close(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	bool more = FALSE;


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_close_test(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}


	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}


	/* Monster is in the way - attempt to slam the door  -LM-  */
	if (cave_m_idx[y][x] > 0)
	{
		char m_name[DESC_LEN];

		/* Get the monster */
		monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

		/* The monster gets actions equivalent to 1 character move.  XXX XXX */
		monster_free_moves(m_ptr, 100);

		/* We are leaving, or we are not in our original position */
		if ((p_ptr->leaving) || (p_ptr->py != py) || (p_ptr->px != px))
		{
			/* Disturb and cancel action */
			disturb(0, 0);
			return;
		}

		/* Attempt to retreat the monster */
		if (retreat_monster(y, x))
		{
			/* Get the monster name "the kobold" */
			monster_desc(m_name, m_ptr, 0x44);

			/* Success! */
			msg_format("You drive %s back, and slam the door shut!", m_name);

			/* Monster loses a turn */
			m_ptr->mflag |= (MFLAG_TURN);

			/* Character takes only half a turn */
			p_ptr->energy_use = 50;
		}

		/* Failure */
		else
		{
			/* Cancel action */
			msg_format("You fail to slam the door.");
			return;
		}
	}

	/* Close door */
	more = do_cmd_close_aux(y, x);

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
}



/*
 * Determine if a given grid may be "tunneled"
 */
static bool do_cmd_tunnel_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Must be a rock, closed door, or tree */
	if (!(f_info[cave_feat[y][x]].flags & (TF_ROCK | TF_DOOR_CLOSED)) &&
	    (cave_feat[y][x] != FEAT_TREE))
	{
		/* Message */
		msg_print("You see nothing there to tunnel.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Tunnel through wall.  Assumes valid location.
 *
 * Note that it is impossible to "extend" rooms past their
 * outer walls (which are actually part of the room).
 *
 * Attempting to do so will produce floor grids which are not part
 * of the room, and whose "illumination" status do not change with
 * the rest of the room.
 */
static bool twall(int y, int x)
{
	/* Paranoia -- Refuse to change permanent terrain */
	if (cave_perma_bold(y, x)) return (FALSE);

	/* Sound */
	sound(MSG_DIG);

	/* Forget the wall */
	cave_info[y][x] &= ~(CAVE_MARK);

	/* Remove the feature */
	cave_set_feat(y, x, get_nearby_floor(y, x));

	/* Update the visuals */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	/* Result */
	return (TRUE);
}



/*
 * Perform the basic "tunnel" command
 *
 * Assumes that no monster is blocking the destination
 *
 * Uses "twall" (above) to do all "terrain feature changing".
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(int y, int x)
{
	bool more = FALSE;

	char buf[DESC_LEN];

	/* Verify legality */
	if (!do_cmd_tunnel_test(y, x)) return (FALSE);


	/* Make some noise. */
	add_wakeup_chance += 1000;

	/* Get base name of digger */
	if (inventory[INVEN_WIELD].k_idx)
		strip_name(buf, inventory[INVEN_WIELD].k_idx);
	else
		strcpy(buf, "bare hands");


	/* Permanent rock */
	if (cave_permwall(y, x))
	{
		/* Failure */
		if (flush_failure) flush();

		msg_print("This seems to be permanent rock.");
		return (FALSE);
	}

	/* Pillar */
	else if ((cave_feat[y][x] == FEAT_PILLAR) ||
	         (cave_feat[y][x] == FEAT_PILLAR_GOLD))
	{
		bool gold = (cave_feat[y][x] == FEAT_PILLAR_GOLD);
		int i;

		/* No chance */
		if (p_ptr->skill_dig <= 40)
		{
			/* Failure */
			if (flush_failure) flush();

			msg_format("Your %s make%s no impression on the %spillar.",
				buf, inventory[INVEN_WIELD].k_idx ? "s" : "",
				(gold ? "golden " : ""));
			return (FALSE);
		}

		/* Tunnel */
		if ((p_ptr->skill_dig > rand_range(40, 1640)) && twall(y, x))
		{
			msg_print("You knock the pillar to bits.");

			/* A pillar of gold is lucrative */
			if (gold)
			{
				msg_print("Gold glitters on the ground!");
				coin_type = SV_GOLD;
				for (i = 0; i < 1 + p_ptr->depth / 14; i++) place_gold(y, x);
				coin_type = 0;
			}
		}

		/* Keep trying */
		else
		{
			/* We may continue tunneling */
			msg_print("You tunnel into the pillar.");
			more = TRUE;
		}
	}

	/* Granite */
	else if (cave_feat[y][x] >= FEAT_WALL_EXTRA)
	{
		/* No chance */
		if (p_ptr->skill_dig <= 40)
		{
			/* Failure */
			if (flush_failure) flush();

			msg_format("Your %s make%s no impression on the granite wall.",
				buf, inventory[INVEN_WIELD].k_idx ? "s" : "");
			return (FALSE);
		}

		/* Tunnel */
		if ((p_ptr->skill_dig > rand_range(40, 1640)) && twall(y, x))
		{
			msg_print("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunneling */
			msg_print("You tunnel into the granite wall.");
			more = TRUE;
		}
	}

	/* Quartz / Magma */
	else if (cave_feat[y][x] >= FEAT_MAGMA)
	{
		bool okay = FALSE;
		bool gold = FALSE;
		bool hard = FALSE;

		/* Found gold */
		if (cave_feat[y][x] >= FEAT_MAGMA_H)
		{
			gold = TRUE;
		}

		/* Extract "quartz" flag XXX XXX XXX */
		if ((cave_feat[y][x] - FEAT_MAGMA) & 0x01)
		{
			hard = TRUE;
		}

		/* Quartz */
		if (hard)
		{
			/* No chance */
			if (p_ptr->skill_dig <= 25)
			{
				/* Failure */
				if (flush_failure) flush();

				msg_format("Your %s make%s no impression on the quartz vein.",
					buf, inventory[INVEN_WIELD].k_idx ? "s" : "");
				return (FALSE);
			}

			okay = (p_ptr->skill_dig > rand_range(25, 625));
		}

		/* Magma */
		else
		{
			/* No chance */
			if (p_ptr->skill_dig <= 20)
			{
				/* Failure */
				if (flush_failure) flush();

				msg_format("Your %s make%s no impression on the magma intrusion.",
					buf, inventory[INVEN_WIELD].k_idx ? "s" : "");
				return (FALSE);
			}

			okay = (p_ptr->skill_dig > rand_range(20, 420));
		}

		/* Success */
		if (okay && twall(y, x))
		{
			/* Found treasure */
			if (gold)
			{
				/* Place some gold */
				place_gold(y, x);

				/* Message */
				msg_print("You have found something!");
			}

			/* Found nothing */
			else
			{
				/* Message */
				msg_print("You have finished the tunnel.");
			}
		}

		/* Failure (quartz) */
		else if (hard)
		{
			/* Message, continue digging */
			msg_print("You tunnel into the quartz vein.");
			more = TRUE;
		}

		/* Failure (magma) */
		else
		{
			/* Message, continue digging */
			msg_print("You tunnel into the magma intrusion.");
			more = TRUE;
		}
	}

	/* Rubble */
	else if (cave_feat[y][x] == FEAT_RUBBLE)
	{
		/* Remove the rubble */
		if ((p_ptr->skill_dig > rand_int(200)) && twall(y, x))
		{
			/* Message */
			msg_print("You have removed the rubble.");

			/* There is a hidden object here */
			if (cave_loose_rock(y, x))
			{
				/* Remove the loose rock */
				remove_trap_kind(y, x, TRAP_LOOSE_ROCK);

				/* Create a simple object */
				place_object(y, x, FALSE, FALSE, FALSE);

				/* Observe new object */
				if ((cave_o_idx[y][x] != 0) && (player_can_see_or_infra_bold(y, x)))
				{
					msg_print("You have found something!");
				}
			}

			/* Hack -- place a boulder (no messages) */
			else if (one_in_(2))
			{
				make_boulder(y, x, p_ptr->depth);
			}
		}

		else
		{
			/* Message, keep digging */
			msg_print("You dig in the rubble.");
			more = TRUE;
		}
	}

	/* Secret doors */
	else if (cave_feat[y][x] >= FEAT_SECRET)
	{
		/* No chance */
		if (p_ptr->skill_dig <= 20)
		{
			/* Failure */
			if (flush_failure) flush();

			msg_format("Your %s make%s no impression on the granite wall.",
				buf, inventory[INVEN_WIELD].k_idx ? "s" : "");
			return (FALSE);
		}

		/* Tunnel */
		else if ((p_ptr->skill_dig > rand_range(20, 820)) && twall(y, x))
		{
			msg_print("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunneling */
			msg_print("You tunnel into the granite wall.");
			more = TRUE;

			/* Occasional Search XXX XXX */
			if (one_in_(4)) search();
		}
	}

	/* Trees -- easy to make an impression on, hard to remove quickly */
	else if (cave_feat[y][x] == FEAT_TREE)
	{
		/* No chance */
		if (p_ptr->skill_dig <= 20)
		{
			/* Failure */
			if (flush_failure) flush();

			msg_format("Your %s make%s no impression on the trees.",
				buf, inventory[INVEN_WIELD].k_idx ? "s" : "");
			return (FALSE);
		}

		/* Tunnel */
		else if ((p_ptr->skill_dig > rand_range(20, 2420)) && twall(y, x))
		{
			msg_print("You have chopped down the trees.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunneling */
			msg_print("You chop at the trees.");
			more = TRUE;
		}
	}

	/* Known closed doors */
	else if (cave_known_closed_door(y, x))
	{
		/* No chance */
		if (p_ptr->skill_dig <= 20)
		{
			/* Failure */
			if (flush_failure) flush();

			msg_format("Your %s make%s no impression on the door.",
				buf, inventory[INVEN_WIELD].k_idx ? "s" : "");
			return (FALSE);
		}

		/* Tunnel */
		else if ((p_ptr->skill_dig > rand_range(20, 820)) && twall(y, x))
		{
			msg_print("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunneling */
			msg_print("You tunnel into the door.");
			more = TRUE;
		}
	}

	/* Result */
	return (more);
}


/*
 * Tunnel through "walls" (including rubble and secret doors)
 *
 * Digging is very difficult without a digging tool, but can be
 * accomplished by strong players using heavy weapons.
 */
void do_cmd_tunnel(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	bool more = FALSE;


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Oops */
	if (!do_cmd_tunnel_test(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}


	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack -- usually do not move */
		if (py_attack(y, x)) return;
	}

	/* Tunnel through walls */
	more = do_cmd_tunnel_aux(y, x);

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}


/*
 * Return the number of traps around (or under) the character.
 */
static int count_disarmable_traps(int *y, int *x)
{
	int d;
	int xx, yy;
	int count = 0; /* Count how many matches */

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent (legal) location */
		yy = p_ptr->py + ddy_ddd[d];
		xx = p_ptr->px + ddx_ddd[d];

		/* Paranoia */
		if (!in_bounds_fully(yy, xx)) continue;

		/* Check the trap marker */
		if (!(cave_info[yy][xx] & (CAVE_TRAP))) continue;

		/* Grid contains disarmable traps */
		if (has_disarmable_trap(yy, xx))
		{
			/* Count it */
			count++;

			/* Remember the location of the last feature of this type found */
			*y = yy;
			*x = xx;
		}
	}

	/* All done */
	return (count);
}


/*
 * Perform the basic "disarm" command on a trap or glyph.
 *
 * Assume there is no monster blocking the destination (tested by
 * do_cmd_disarm).  Traps now have level-dependent difficulty.
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_trap(int y, int x)
{
	int skill, chance, diff;

	cptr name;

	bool more = FALSE;
	bool safe = FALSE;

	int idx;
	trap_type *t_ptr;


	/* Paranoia -- must be fully in bounds */
	if (!in_bounds_fully(y, x)) return (FALSE);

	/* Verify legality */
	if (!has_disarmable_trap(y, x)) return (FALSE);


	/* Choose a trap to disarm (automatic if only one) */
	get_trap(y, x, &idx);

	/* No trap was chosen */
	if (idx < 0) return (FALSE);

	/* Get the trap */
	t_ptr = &t_list[idx];

	/* Get the trap name */
	name = t_kind_info[t_ptr->t_idx].name;


	/* Cannot disarm pit if inside it (unless you have feather fall) */
	if ((t_ptr->t_idx == TRAP_PIT) && (p_ptr->py == y) &&
	    (p_ptr->px == x) && (!p_ptr->ffall))
	{
		msg_print("You cannot disarm the pit you've fallen in.");
		return (FALSE);
	}


	/* Get disarm skill */
	skill = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind    || (no_light() && (p_ptr->see_infra <= 0)))   skill /= 10;
	if (p_ptr->confused || p_ptr->image) skill /= 10;
	if (p_ptr->berserk)                  skill /=  2;


	/* Calculate trap difficulty, which is simply the trap depth. */
	diff = p_ptr->depth;
	if (diff > 100) diff = 100;
	if (diff <   1) diff =   1;

	/* Some traps are easy to disarm. */
	if (t_kind_info[t_ptr->t_idx].flags & (TRAP_EASY_DISARM)) diff = 0;

	/* Extract the disarm probability (minimum of 10%) */
	chance = MAX(10, skill - diff);

	/* High disarming renders one safer from lower-level traps */
	if (get_skill(S_DISARM, 0, 100) >= m_bonus(100, p_ptr->depth, 100))
	{
		/* But only a skill of 100 guarantees anything */
		safe = TRUE;
	}

	/* Update display of when maximum number of traps are set */
	if (t_ptr->t_idx == TRAP_MONSTER)
	{
		p_ptr->redraw |= PR_CONDITIONS;
	}

	/* Success */
	if ((diff == 0) || (rand_int(100) < chance))
	{
		/* Special message for glyphs. */
		if (t_ptr->t_idx == TRAP_GLYPH)
		{
			msg_format("You have desanctified the %s.", name);
		}

		/* Attempt to loot the trap */
		else if (t_ptr->t_idx != TRAP_MONSTER)
		{
			/* Chance goes up rapidly with disarm skill */
			int loot = get_skill(S_DISARM, 0, 100) *
			           get_skill(S_DISARM, 0, 100);

			/* However, a high trap level is difficult to overcome */
			loot -= (80 * diff);

			/* And chance cannot rise too high */
			if (loot > 500) loot = 500;

			/* Roll for looting (maximum 33% chance), check for object */
			if ((loot >= rand_int(1500)) && (loot_trap(y, x, idx)))
			{
				/* Cool */
				msg_print("You disarm the trap and find something inside.");
			}

			/* Ordinary disarm */
			else
			{
				msg_format("You have disarmed the %s.", name);
			}
		}

		/* Trap is allowed to give experience */
		if (!(t_ptr->flags & (TRAP_NO_EXP)))
		{
			/* Gain some experience */
			if ((diff) && (t_ptr->t_idx != TRAP_MONSTER))
				gain_exp(randint(diff + (diff * diff / 20)), S_DISARM);
		}

		/* Kill the trap (always visible) */
		remove_trap(y, x, idx);
		lite_spot(y, x);
	}

	/* Failure -- Keep trying */
	else if (safe || randint(chance) > 5)
	{
		/* Failure */
		if (flush_failure) flush();

		/* Message */
		msg_format("You failed to disarm the %s.", name);

		/* We may keep trying */
		more = TRUE;
	}

	/* Failure -- Set off the trap */
	else
	{
		/* Message */
		msg_format("You set off the %s!", name);

		/* Hit the trap */
		hit_trap(-1, y, x);
	}

	/* Result */
	return (more);
}



/*
 * Disarms a trap, a glyph, or a chest.
 */
void do_cmd_disarm(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	s16b o_idx;

	bool more = FALSE;

	int num_traps, num_chests;

	/* Count disarmable traps */
	num_traps = count_disarmable_traps(&y, &x);

	/* Count chests (trapped) */
	num_chests = count_chests(&y, &x, TRUE);

	/* Handle a single visible trap or trapped chest  -TNB- */
	if (num_traps || num_chests)
	{
		if (num_traps + num_chests <= 1)
			p_ptr->command_dir = coords_to_dir(y, x);
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Check for chests */
	o_idx = chest_check(y, x);


	/* Verify legality */
	if (!o_idx && !has_disarmable_trap(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Check for chests */
		o_idx = chest_check(y, x);
	}


	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack -- usually do not move */
		if (py_attack(y, x)) return;
	}

	/* Disarm trap */
	if (has_disarmable_trap(y, x))
	{
		/* Disarm the trap */
		more = do_cmd_disarm_trap(y, x);
	}

	/* Chest */
	else
	{
		/* Disarm the chest */
		more = do_cmd_disarm_chest(y, x, o_idx);
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
}


/*
 * Yell to make noise.
 */
static void do_yell(void)
{
	/* Make lots of noise */
	add_wakeup_chance = 50000;

	/* Should also make a sound here  XXX */

	/* Randomize fear, display messages */
	if (p_ptr->afraid)
	{
		int f = rand_int(10) - rand_int(10);

		if (f >= 0)
			msg_print("You scream in panic!");
		else if (f < (-p_ptr->afraid))
			msg_print("You shout, and all fear leaves you!");
		else
			msg_print("You shout boldly!");

		(void)set_afraid(p_ptr->afraid + f);
	}
	else
	{
		msg_print("You yell loudly.");
	}

	/* Take a turn */
	p_ptr->energy_use = 100;
}


/*
 * Bash a chest.
 */
static void do_bash_chest(int y, int x)
{
	object_type *o_ptr;
	int bash, chance;
	s16b this_o_idx, next_o_idx = 0;


	/* Scan the pile of objects */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get object */
		o_ptr = &o_list[this_o_idx];

		/* Find a locked or trapped chest */
		if ((o_ptr->tval == TV_CHEST) && (o_ptr->pval > 0))
		{
			/* Hack -- Bash power based on strength (-10 to 10) */
			bash = (p_ptr->stat_ind[A_STR] > 20 ? 20 :
					  p_ptr->stat_ind[A_STR]) - 10;

			/* Good wrestlers get a bonus */
			bash += get_skill(S_WRESTLING, 0, 4);

			/* Compare bash power to chest power */
			chance = (25 + bash * 3) - (ABS(o_ptr->pval) / 10);

			/* We are wimpy */
			if (chance <= 0)
			{
				msg_print("Your weak blows bounce off the chest.");
			}

			/* Chest is smashed open */
			else if (rand_int(100) < chance)
			{
				/* Message */
				message(MSG_OPENDOOR, 0, "You smash open the chest!");

				/* Loot! */
				chest_death(FALSE, y, x, o_ptr);
			}

			/* Bashing failed -- hurt the chest */
			else
			{
				o_ptr->pval -= 1 + o_ptr->pval / 20;
				if (o_ptr->pval < 0) o_ptr->pval = 0;

				msg_print("You beat furiously on the chest; it remains closed.");
			}

			/* Make a fair amount of noise. */
			add_wakeup_chance = MAX(add_wakeup_chance, 7500);

			break;
		}
	}
}




/*
 * Bash a door.
 */
static void do_bash_door(int y, int x)
{
	int bash, diff, chance;

	/* Message */
	msg_print("You smash into the door!");

	/* Make a lot of noise. */
	add_wakeup_chance = 12000;

	/* Hack -- Bash power based on strength and weight (From ~7 to ~130) */
	bash = rsqrt(p_ptr->wt) + adj_str_hold[p_ptr->stat_ind[A_STR]];

	/* Good wrestlers get a bonus */
	bash += get_skill(S_WRESTLING, 0, 20);


	/* Extract door power */
	diff = ((cave_feat[y][x] - FEAT_DOOR_HEAD) % 8);

	/* Closed and locked doors are easier to bust down */
	if (cave_feat[y][x] - FEAT_DOOR_HEAD < 8) diff /= 2;


	/* Compare bash power to door power XXX XXX XXX */
	chance = (bash - (diff * 10));

	/* Hack -- always have a chance */
	if (chance < 2) chance = 2;

	/* Hack -- attempt to bash down the door */
	if (rand_int(100) < chance)
	{
		/* Break down the door */
		if (one_in_(2))
		{
			cave_set_feat(y, x, FEAT_BROKEN);
			if (p_ptr->prace == RACE_HALF_TROLL)
				message(MSG_OPENDOOR, 0, "The door shatters into splinters!");
			else
				message(MSG_OPENDOOR, 0, "The door smashes open!");
		}

		/* Open the door */
		else
		{
			cave_set_feat(y, x, FEAT_OPEN);
			message(MSG_OPENDOOR, 0, "The door crashes open!");
		}

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Saving throw against stun */
	else if (rand_int(40) < p_ptr->stat_ind[A_DEX] +
		get_skill(S_WRESTLING, 0, 8) + get_skill(S_KARATE, 0, 8))
	{
		/* Message */
		msg_print("The door holds firm.");
	}
	else
	{
		/* Message */
		msg_print("You are off-balance.");

		/* Hack -- Lose balance a la paralysis */
		(void)set_paralyzed(p_ptr->paralyzed + rand_range(2, 3));
	}
}


/*
 * Bash open a door, smash a chest, or yell.
 *
 * This code has an interactive inline version of "get_rep_dir()".
 *
 * Deliberate side-effects of this are that it is possible to bash a
 * chest in the same grid, but not possible to auto-repeat bashing.
 */
void do_cmd_bash(void)
{
	int y, x, dir = 0;
	object_type *o_ptr;

	bool did_action = FALSE;

	char ch;
	cptr p;


	/* You cannot bash or yell when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* Get and test a direction, or choose to yell */
	while (!dir)
	{
		/* Handle pre-set directions */
		if (p_ptr->command_dir)
		{
			dir = p_ptr->command_dir;
			p_ptr->command_dir = 0;
		}

		/* Allow choosing a direction after the command */
		else
		{
			/* Choose a prompt */
			p = "Direction (Escape to cancel, or 'y' to yell)?";

			/* Get a command (allow cancel) */
			if (!get_com(p, &ch)) break;

			/* Handle yelling */
			if ((ch == 'Y') || (ch == 'y'))
			{
				do_yell();
				break;

			}
			/* Convert keypress into a direction */
			dir = target_dir(ch);

			/* A direction of zero means "this grid"  XXX */
			if (dir == 0) dir = 5;
		}

		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];


		/* Must have knowledge */
		if (!(cave_info[y][x] & (CAVE_MARK)))
		{
			/* Can't see anything */
			msg_print("You see nothing to bash here.");
			continue;
		}

		/* Take a turn */
		p_ptr->energy_use = 100;

		/* Monster */
		if (cave_m_idx[y][x] > 0)
		{
			/* Attack -- usually do not move */
			if (py_attack(y, x)) did_action = TRUE;
		}

		/* Scan for chests, if any are present */
		for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
		{
			/* Chest (first one in the stack) */
			if (o_ptr->tval == TV_CHEST)
			{
				/* Locked or trapped chests can be bashed */
				if (o_ptr->pval > 0)
				{
					do_bash_chest(y, x);
					did_action = TRUE;
					break;
				}
			}
		}

		/* Require a door */
		if ((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
		    (cave_feat[y][x] <= FEAT_DOOR_TAIL))
		{
			do_bash_door(y, x);
			did_action = TRUE;
			break;
		}

		/* Note failure, or stop on success */
		if (!did_action) msg_print("You see nothing there to bash.");
		else break;
	}

	/* Save desired direction, if a valid direction is given */
	/* if ((dir > 0) && (dir > 10) && (dir != 5)) p_ptr->command_dir = dir; */
}


/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open and close
 * doors, and, for burglars, set traps, lock doors, and steal objects.
 *
 * This command must always take energy, to prevent free detection
 * of invisible monsters.
 *
 * The direction of the alter command must be chosen before the player
 * is confused, and the grid in the actual direction must be verified.
 *
 * Note that there are two versions of this command; one with extra
 * power, and one with any "annoyance" removed.  The first should only
 * be activated when one deliberately types the '+' key.
 */
void do_cmd_alter(bool deliberate)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	int feat;

	bool more = FALSE;

	/* Get a direction */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Original feature */
	feat = cave_feat[y][x];

	/* Must have knowledge to know feature XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) feat = FEAT_NONE;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}


	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/*
	 * If a monster is present, and visible, a Burglar may steal from it.
	 * Otherwise, the player will simply attack. -LM-
	 */
	if (cave_m_idx[y][x] > 0)
	{
		if ((!p_ptr->schange) &&
		    (get_skill(S_BURGLARY, 0, 100) >= LEV_REQ_BURGLE))
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

			/* Steal or attack -- never move */
			if (mon_fully_visible(m_ptr)) py_steal(y, x);
			else                    (void)py_attack(y, x);
		}
		else
		{
			(void)py_attack(y, x);
		}
	}

	/* Disarm traps */
	else if (cave_visible_trap(y, x))
	{
		int action = -1;

		/* Monster traps can be loaded or disarmed  -LM- */
		if ((cave_monster_trap(y, x)) &&
		    (!p_ptr->blind) && (!p_ptr->confused))
		{
			/* Cancel repeat */
			disturb(0, 0);

			/* Interact with the trap, note if we want to disarm */
			action = load_trap(y, x);

			/* Allow cancel */
			if (!action)
			{
				p_ptr->energy_use = 0;
				return;
			}
		}

		/* Disarm */
		if (action == -1) more = do_cmd_disarm_trap(y, x);
	}

	/* Tunnel through rock */
	else if (cave_rock_bold(y, x))
	{
		/* Tunnel */
		more = do_cmd_tunnel_aux(y, x);
	}

	/* Open known closed doors */
	else if (cave_known_closed_door(y, x))
	{
		bool flag = FALSE;

		/* If the door is a closed, unlocked door, Burglars can lock it */
		if ((feat == FEAT_DOOR_HEAD) && (deliberate))
		{
			if (get_skill(S_BURGLARY, 0, 100) >= LEV_REQ_LOCK_DOOR)
			{
				if (get_check("Lock this door?"))
				{
					int pow = rand_range(get_skill(S_BURGLARY, 1, 6),
					                     get_skill(S_BURGLARY, 1, 8));

					cave_set_feat(y, x, FEAT_DOOR_HEAD + pow);

					flag = TRUE;
				}
			}
		}

		/* Open -- unless we just locked it */
		if (!flag) more = do_cmd_open_aux(y, x);
	}

	/* Close open doors */
	else if (feat == FEAT_OPEN)
	{
		/* Close */
		more = do_cmd_close_aux(y, x);
	}

	/* If a burglar, set a trap.  -LM- */
	else if ((cave_trap_allowed(y, x)) &&
	         (get_skill(S_BURGLARY, 0, 100) >= LEV_REQ_TRAP))
	{
		more = py_set_trap(y, x, dir);
	}

	/* Oops */
	else
	{
		/* Oops */
		msg_print("You can make no alterations here.");
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}


/*
 * Find the index of some "spikes", if possible.
 */
static bool get_spike(int *ip)
{
	int i;

	/* Check every item in the pack */
	for (i = 0; i < INVEN_PACK; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Check the "tval" code */
		if (o_ptr->tval == TV_SPIKE)
		{
			/* Save the spike index */
			(*ip) = i;

			/* Success */
			return (TRUE);
		}
	}

	/* Oops */
	return (FALSE);
}


/*
 * Determine if a given grid may be "spiked"
 */
static bool do_cmd_spike_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require a known closed door */
	if (!cave_known_closed_door(y, x))
	{
		/* Message */
		msg_print("You see nothing there to spike.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}

/*
 * Jam a closed door
 */
void do_cmd_spike_aux(int y, int x)
{
	/* Convert "locked" to "stuck" XXX XXX XXX */
	if (cave_feat[y][x] < FEAT_DOOR_HEAD + 0x08)
	{
		cave_feat[y][x] += 0x08;
	}

	/* Spiking an unbarred door has double effect  -LM- */
	if (cave_feat[y][x] == FEAT_DOOR_HEAD)
	{
		cave_feat[y][x] += 0x01;
	}

	/* Add one spike to the door */
	if (cave_feat[y][x] < FEAT_DOOR_TAIL)
	{
		cave_feat[y][x] += 0x01;
	}

	/* Door is fully barred and character is perceptive enough to notice */
	if ((cave_feat[y][x] == FEAT_DOOR_TAIL) &&
	         (get_skill(S_PERCEPTION, 20, 100) > rand_int(100)))
	{
		msg_print("This door is now so firmly barred that further spiking will have no effect.");
	}
}



/*
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir, item = 0;


	/* Get a spike */
	if (!get_spike(&item))
	{
		/* Message */
		msg_print("You have no spikes.");

		/* Done */
		return;
	}


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_spike_test(y, x)) return;

	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}


	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Take a full turn  XXX */
		p_ptr->energy_use = 100;

		/* Attack -- usually do not move */
		if (py_attack(y, x)) return;
	}

	/* Go for it */
	else
	{
		/* Take half a turn */
		p_ptr->energy_use = 50;

		/* Verify legality */
		if (!do_cmd_spike_test(y, x)) return;

		/* Message */
		msg_print("You jam the door with a spike.");

		/* Spike the door */
		do_cmd_spike_aux(y, x);

		/* Use up, and describe, a single spike, from the bottom */
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}
}

/*
 * Determine if a given grid may be "walked".
 *
 * Players can "walk" into visible monsters in walls.  This prevents the
 * annoyance of having to tunnel into a wall to get at a ghost you can see
 * perfectly well.
 */
static bool do_cmd_walk_test(int y, int x)
{
	/* Check for monsters. */
	if (cave_m_idx[y][x] != 0)
	{
		monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

		/* If a monster can be seen, it can be attacked normally. */
		if (m_ptr->ml) return (TRUE);
	}


	/* Hack -- walking obtains knowledge XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (TRUE);


	/* Special case -- characters in wraithform pass through walls */
	if ((p_ptr->wraithform) && (!cave_permwall(y, x))) return (TRUE);

	/* Otherwise, require passable terrain */
	if (!cave_passable_bold(y, x))
	{
		/* Known closed doors */
		if (cave_known_closed_door(y, x))
		{
			/* Doors can be opened. */
			return (TRUE);
		}

		/* Wall */
		else if (cave_wall_bold(y, x))
		{
			/* Message */
			msg_print("There is a wall in the way.");
		}

		/* Special non-passable */
		else
		{
			msg_format("You cannot cross %s.",
				f_name + f_info[cave_feat[y][x]].name);
		}

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for the "walk" and "jump" commands.
 */
static void do_cmd_walk_or_jump(int pickup)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality (even if confused) */
	if (!do_cmd_walk_test(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Verify legality of new location */
		if (!do_cmd_walk_test(y, x)) return;
	}

	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Move the player */
	move_player(dir, pickup);
}

/*
 * Walk into a grid (pick up objects as set by the auto-pickup option)
 */
void do_cmd_walk(void)
{
	/* Move (usually pickup) */
	do_cmd_walk_or_jump(always_pickup);
}


/*
 * Jump into a grid (flip pickup mode)
 */
void do_cmd_jump(void)
{
	/* Move (usually do not pickup) */
	do_cmd_walk_or_jump(!always_pickup);
}


/*
 * Start running.
 *
 * Note that running while confused is not allowed.
 */
void do_cmd_run(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;


	/* Hack XXX XXX XXX */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;


	/* Start run */
	run_step(dir);
}



/*
 * Stay still.  Search.  Enter stores.
 * Pick up treasure and objects if "pickup" is true.
 */
static void do_cmd_hold_or_stay(int pickup)
{
	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Spontaneous searching for traps and doors (a bit easier than normal) */
	if (rand_int(50 + p_ptr->depth) < 5 * p_ptr->skill_srh / 4)
	{
		search();
	}

	/* Spontaneous searching -- essences */
	if (get_skill(S_INFUSION, 0, 100) >= LEV_REQ_INFUSE)
	{
		/* Skill competes with depth (a little easier than normal) */
		if (rand_int(25 + p_ptr->depth) <
		    get_skill(S_INFUSION, 0, 120))
		{
			search_essence(FALSE);
		}
	}

	/* Notice unseen objects */
	notice_unseen_objects();

	/* Handle objects.  Do not charge extra energy for objects picked up. */
	(void)py_pickup(pickup);

	/* Hack -- enter a store if we are on one */
	if (cave_shop_bold(p_ptr->py, p_ptr->px))
	{
		/* Enter the store */
		do_cmd_store();
	}
}


/*
 * Hold still (pick up objects if always_pickup is TRUE).
 */
void do_cmd_hold(void)
{
	/* Hold still (optional pickup) */
	do_cmd_hold_or_stay(always_pickup);
}


/*
 * Pick up objects on the floor beneath you.  -LM-
 */
void do_cmd_pickup(void)
{
	int energy_cost;

	/* Pick up floor objects, forcing a menu for multiple objects. */
	energy_cost = py_pickup(2) * 10;

	/* Maximum time expenditure is a full turn. */
	if (energy_cost > 100) energy_cost = 100;

	/* Charge this amount of energy. */
	p_ptr->energy_use = energy_cost;
}



/*
 * Rest (restores hit points and mana and such)
 */
void do_cmd_rest(void)
{
	/* Prompt for time if needed */
	if (p_ptr->command_arg <= 0)
	{
		cptr p = "Rest (0-9999, '*' for HP/SP, '&' as needed):";

		char out_val[DESC_LEN];

		/* Default */
		strcpy(out_val, "&");

		/* Ask for duration */
		if (!get_string(p, out_val, 5)) return;

		/* Rest until done */
		if (out_val[0] == '&')
		{
			p_ptr->command_arg = (-2);
		}

		/* Rest a lot */
		else if (out_val[0] == '*')
		{
			p_ptr->command_arg = (-1);
		}

		/* Rest some */
		else
		{
			p_ptr->command_arg = atoi(out_val);
			if (p_ptr->command_arg <= 0) return;
		}
	}

	/* Paranoia */
	if (p_ptr->command_arg > 9999) p_ptr->command_arg = 9999;

	/* Take a turn XXX XXX XXX (?) */
	p_ptr->energy_use = 100;

	/* Save the rest code */
	p_ptr->resting = p_ptr->command_arg;

	/* Cancel the arg */
	p_ptr->command_arg = 0;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Print "regen" */
	left_panel_display(DISPLAY_REGEN, 0);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Refresh XXX XXX XXX */
	(void)Term_fresh();
}
