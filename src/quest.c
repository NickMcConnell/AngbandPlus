/* File: quest.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * This file contains functions used for handling quests by Eytan Zweig,
 * rewritten by way of Jeff Green (NPPangband) and implimented in Steamband
 * by Courtney C. Campbell
 */

#include "angband.h"

#define QMODE_HALF_1 1
#define QMODE_HALF_2 2
#define QMODE_SHORT  3
#define QMODE_FULL   4

#define GUILD_QUESTS	3	/* Number of quests offered at the guild at any given time */

#define GUILD_FAME_SPECIAL	12

static int avail_quest;

/*
 * The Adventurer's guild's selection
 */
static s16b guild[GUILD_QUESTS] =
{
	1,	5,	9
};

/*
 * Mega-hack - Fix plural names of monsters
 *
 * Taken from PernAngband via EY, via NPP modified to fit Steam monster list
 */
void plural_aux(char *name, size_t max)
{
	int name_len = strlen(name);

	if (strstr(name, " of "))
	{
		cptr aider = strstr(name, " of ");
		char dummy[80];
		int i = 0;
		cptr ctr = name;

		while (ctr < aider)
		{
			dummy[i] = *ctr;
			ctr++;
			i++;
		}

		if (dummy[i - 1] == 's')
		{
			strcpy (&(dummy[i]), "es");
			i++;
		}
		else
		{
			strcpy (&(dummy[i]), "s");
		}

		strcpy(&(dummy[i + 1]), aider);
		my_strcpy(name, dummy, max);
	}
	else if ((strstr(name, "coins")) || (strstr(name, "gems")))
	{
		char dummy[80];
		strcpy (dummy, "Piles of c");
		my_strcat (dummy, &(name[1]), sizeof(dummy));
		my_strcpy (name, dummy, max);
		return;
	}
	else if ((strstr(name, "Manes")) ||(strstr(name, "Yaksas")) ||(strstr(name, "Bori")) ||
		(name[name_len-1] == 'u') || (strstr(name, "Yeti")) ||
		(streq(&(name[name_len-2]), "ua")) || (streq(&(name[name_len-2]), "es")) || 
		(streq(&(name[name_len-3]), "nee")) || (streq(&(name[name_len-6]), "Steeds")) ||
		(streq(&(name[name_len-4]), "idhe")))
	{
		return;
	}
	else if (streq(&(name[name_len - 2]), "fe"))
	{
		strcpy (&(name[name_len - 2]), "ves");
	}
	else if (streq(&(name[name_len - 3]), "Rex"))
	{
		strcpy (&(name[name_len - 3]), "Rexes");
	}
	else if (streq(&(name[name_len - 4]), "nkey"))
	{
		strcpy (&(name[name_len - 4]), "nkeys");
	}
	else if (streq(&(name[name_len - 4]), "aton"))
	{
		strcpy (&(name[name_len - 4]), "ata");
	}
	else if (streq(&(name[name_len - 3]), "Man"))
	{
		strcpy (&(name[name_len - 3]), "Men");
	}
	else if (name[name_len-1] == 'y')
	{
		strcpy(&(name[name_len - 1]), "ies");
	}
	else if (streq(&(name[name_len - 4]), "ouse"))
	{
		strcpy (&(name[name_len - 4]), "ice");
	}
	else if (streq(&(name[name_len - 4]), "lung"))
	{
		strcpy (&(name[name_len - 4]), "lungen");
	}
	else if (streq(&(name[name_len - 3]), "sus"))
	{
		strcpy (&(name[name_len - 3]), "si");
	}
	else if (streq(&(name[name_len - 4]), "star"))
	{
		strcpy (&(name[name_len - 4]), "stari");
	}
	else if (streq(&(name[name_len - 3]), "aia"))
	{
		strcpy (&(name[name_len - 3]), "aiar");
	}
	else if (streq(&(name[name_len - 3]), "inu"))
	{
		strcpy (&(name[name_len - 3]), "inur");
	}
	else if (streq(&(name[name_len - 5]), "culus"))
	{
		strcpy (&(name[name_len - 5]), "culi");
	}
	else if (streq(&(name[name_len - 3]), "men"))
	{
		strcpy (&(name[name_len - 3]), "man");
	}
	else if (streq(&(name[name_len - 3]), "man"))
	{
		strcpy (&(name[name_len - 3]), "men");
	}
	else if (streq(&(name[name_len - 4]), "lman"))
	{
		strcpy (&(name[name_len - 4]), "lmen");
	}
	else if (streq(&(name[name_len - 2]), "ex"))
	{
		strcpy (&(name[name_len - 2]), "ices");
	}
	else if (streq(&(name[name_len - 2]), "ix"))
	{
		strcpy (&(name[name_len - 2]), "ixes");
	}
	else if ((name[name_len - 1] == 'f') && (!streq(&(name[name_len - 2]), "ff")))
	{
		strcpy (&(name[name_len - 1]), "ves");
	}
	else if ((streq(&(name[name_len - 2]), "ch")) || (name[name_len - 1] == 's'))
	{
		strcpy (&(name[name_len]), "es");
	}
	else
	{
		strcpy (&(name[name_len]), "s");
	}
}


/*
 * Provide a description of the quest
 */
cptr describe_quest(s16b level, int mode)
{
	int q_idx = quest_num(level);
	char name[80];
	char intro[80];
	char targets[80];
	char where[80];
	char race_name[80];
	char what[25];

	quest_type *q_ptr = &q_info[q_idx];
	monster_race *r_ptr = &r_info[q_ptr->mon_idx];

	/* Vault quests */
	if (q_ptr->type == QUEST_VAULT)
	{
		strcpy(intro, "Return to the library a large, sealed jeweled chest from a vault");

		/* The location of the quest */
		if (!depth_in_feet) strcpy(where, format("on dungeon level %d.", 51-level));
		else strcpy(where, format("at a depth of %d feet.", 2550-(level * 50)));

		if (mode == QMODE_SHORT) return (format("%s.", intro));
		else if (mode == QMODE_FULL) return (format("%s %s", intro, where));
		else if (mode == QMODE_HALF_1) return (format("%s", intro));
		else if (mode == QMODE_HALF_2) return (format("%s", where));
	}

	/* Not a vault quest, so get the monster race name (singular)*/
	monster_desc_race(race_name, sizeof(race_name), q_ptr->mon_idx);

	if ((q_ptr->type == QUEST_UNIQUE) || (q_ptr->type == QUEST_FIXED_U))
	{

		/* Monster quests */
		my_strcpy(targets, race_name, sizeof(targets));
		strcpy(intro, "To fulfill your task, you must");
	}

	else
	{
		/* Monster quests */
		my_strcpy(name, race_name, sizeof(name));

		/* Multiple quest monsters */
		if ((q_ptr->max_num - q_ptr->cur_num) > 1)
		{
			plural_aux(name, sizeof(name));
			my_strcpy(targets,
				format("%d %s",(q_ptr->max_num - q_ptr->cur_num), name), sizeof(targets));
		}

		/* One quest monster */
		else
		{
			if (is_a_vowel(name[0])) my_strcpy(targets, format("an %s", name), sizeof(targets));
			else my_strcpy(targets, format("a %s", name), sizeof(targets));
		}
	}

	if (monster_nonliving(r_ptr)) strcpy(what, "destroy");
	else strcpy(what, "kill");

	/* The type of the quest */
	if (q_ptr->type == QUEST_FIXED) strcpy(intro, "For eternal glory, you must");
	else if (q_ptr->type == QUEST_FIXED_U) strcpy(intro, "For eternal glory, you must");
	else if (q_ptr->type == QUEST_GUILD) strcpy(intro, "To fulfill your task, you must");

	/* The location of the quest */
	if (!depth_in_feet) strcpy(where, format("on dungeon level %d.", 51-level));
	else strcpy(where, format("at a depth of %d feet.", 2550-(level * 50)));

	/* Output */
	if (mode == QMODE_SHORT) return (format("%s %s %s.", intro, what, targets));
	else if (mode == QMODE_FULL) return (format("%s %s %s %s", intro, what, targets, where));
	else if (mode == QMODE_HALF_1) return (format("%s %s %s", intro, what, targets));
	else if (mode == QMODE_HALF_2) return (format("%s", where));

	if (!p_ptr->cur_quest > 0) return "none";
	/* Paranoia */
	return "none";
}

/*
 * Give a reward to the player
 *
 * Hack - The entire "tailored item" routine makes quite a few assumptions
 * as to what's a good reward, plus it relies a lot of item price to make
 * deductions. It is basically a list of special cases.
 */
static void grant_reward(byte type)
{
	int i, x;

	char o_name[80];

	bool got_item = FALSE;

	int repeats;

	object_type *i_ptr, *j_ptr;
 	object_type object_type_body;
	store_type *st_ptr = &store[STORE_HOME];

	/* Get local object */
	i_ptr = &object_type_body;

	/* No reward */
	if (!type) return;

	/*ugly, but effective hack - make extra gold, prevent chests & allow artifacts*/
	/* XCCCX */
	/* chest_or_quest = QUEST_ITEM; */

	/* Create a gold reward */
	if (type == REWARD_GOLD)
	{
		/*base amount of gold on fame*/
		repeats = p_ptr->fame;

		/*but put in a minimum*/
		if (repeats < 3) repeats = 3;

		/* Give a good gold type for the level */
		for (i = 0; i < repeats; i++)
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Make some gold */
			while (!make_gold(i_ptr)) continue;

			/* Describe the object */
			object_desc(o_name, i_ptr, TRUE, 3);

			/* Message */
			msg_format("You have been rewarded with %ld gold pieces.",
			           (long)i_ptr->pval);
			message_flush();

			/* Collect the gold */
			p_ptr->au += i_ptr->pval;

			/* Redraw gold */
			p_ptr->redraw |= (PR_GOLD);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		}

		/*undo special item level creation*/
		/* chest_or_quest = FALSE; */

		return;
	}
	
	if (type == REWARD_SKILL)
	{
		int sk = 0;
		
		/*base amount of gold on fame*/
		repeats = p_ptr->fame / 5;

		/*but put in a minimum*/
		if (repeats < 2) repeats = 2;

		/* Give a good gold type for the level */
		for (i = 0; i < repeats; i++)
		{
			sk += rand_int(2);
		}		
		
		p_ptr->free_skpts += sk + 1;
		
		/* Message */
		msg_format("You have been rewarded with %d skill points.", sk + 1);
		message_flush();
		return;
	}

	/* Create an item reward */

	/* A tailored reward */
	if (type == REWARD_TAILORED)
	{
		/*
		 * First, maybe we want to give a spellbook to a spellcaster.
		 * Hack - greater chance if a "pure" spellcaster.
		 */
		if ((p_ptr->skills[SK_LATIN].skill_max > 10) &&
			(rand_int(100) < (cp_ptr->flags & CF_ZERO_FAIL ? 40 : 25)))
		{
			/*200 tries at a book*/
			for (i = 0; i < 10; i++)
			{
				bool already_own = FALSE;

				/* Get local object */
				i_ptr = &object_type_body;

				/* Wipe the object */
				object_wipe(i_ptr);

				required_tval = TV_MAGIC_BOOK;

				/* need to make this drop spellbooks only and by skill XCCCX */
				/*try to make a spellbook, frequently returns nothing*/
				if (!make_object(i_ptr, TRUE, TRUE, TRUE)) continue;

				/* Was this already a reward (marked tried) */
				if (k_info[i_ptr->k_idx].tried) continue;

				/* Look for item in the pack */
				for (x = 0; x < INVEN_PACK; x++)
				{
					/* Get the item */
					j_ptr = &inventory[x];

					/* Nothing here */
					if (!j_ptr->k_idx) continue;

					/*Is it the same spellbook?*/
					if ((j_ptr->tval == i_ptr->tval) && (j_ptr->sval == i_ptr->sval))
						already_own = TRUE;
				}

				/* Look for item in home */
				if (st_ptr->stock_num)
				{
					for (x = 0; x < st_ptr->stock_num; x++)
					{
						j_ptr = &st_ptr->stock[x];

						/* Nothing here */
						if (!j_ptr->k_idx) continue;

						if ((j_ptr->tval == i_ptr->tval) && (j_ptr->sval == i_ptr->sval))
							already_own = TRUE;
					}
 				}

				/* Hack - Mark as tried */
				k_info[i_ptr->k_idx].tried = TRUE;

				/*we already have a book*/
				if (already_own) continue;

				/*we found a book we can use as a reward*/
				got_item = TRUE;
				required_tval = 0;

				break;
			}
		}

		/* Maybe we want to give a potion of augmentation */
		if (!got_item)
		{
			/*clear the counter*/
			x = 0;

			/* Add up the stats indexes over 13*/
			for (i = 0; i < A_MAX; i++)
			{
				x += (MIN(0, p_ptr->stat_max[i]));
			}

			if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
			{
				/* Nothing - SteamMecha/Automata can't drink potions. */
			}

			/*We only want to give potion if stats are low.*/
			else if ((rand_int(900) + 1200) < x)
			{
				/* Get local object */
				i_ptr = &object_type_body;

				/* Wipe the object */
				object_wipe(i_ptr);

				/* Make a potion of augmentation */
				object_prep(i_ptr, lookup_kind(TV_TONIC, SV_TONIC_AUGMENTATION));

				got_item = TRUE;
			}
		}

		/* We didn't find anything else, so lets find something to wear */
		if (!got_item)
		{
			s32b price[INVEN_TOTAL];
			s32b val[INVEN_TOTAL];
			s32b diff = 1;
			int droptype = 0;
			st_ptr = &store[STORE_HOME];

			/* First, figure out the best price for each slot */
			for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
			{
				/* Skip second ring */
				if (i == INVEN_RIGHT) continue;

				price[i] = 0;

				/* First, the item actually in the slot */
				j_ptr = &inventory[i];

				if (j_ptr->k_idx) price[i] = object_value(j_ptr);

				/* Handle ring */
				if (i == INVEN_LEFT)
				{
					/* Compare to second ring */
					j_ptr = &inventory[INVEN_RIGHT];

					if (j_ptr->k_idx)
					{
						/* Compare prices */
						if (object_value(j_ptr) > price[i]) price[i] = object_value(j_ptr);
					}
				}

				/* Look for item in the pack */
				for (x = 0; x < INVEN_PACK; x++)
				{
					/* Get the item */
					j_ptr = &inventory[x];

					/* Nothing here */
					if (!j_ptr->k_idx) continue;

					/* Rings */
					if (i == INVEN_LEFT)
					{
						if (j_ptr->tval != TV_RING) continue;
					}
					/* Make sure we're not trying to place a ring elsewhere */
					else if (j_ptr->tval == TV_RING) continue;
					/* Not for the same slot */
					else if (wield_slot(j_ptr) != i) continue;

					/* Compare prices */
					if (object_value(j_ptr) > price[i]) price[i] = object_value(j_ptr);
				}

				/* Look for item in home */
				if (st_ptr->stock_num)
				{
					for (x = 0; x < st_ptr->stock_num; x++)
					{
						j_ptr = &st_ptr->stock[x];

						/* Nothing here */
						if (!j_ptr->k_idx) continue;

						/* Rings */
						if (i == INVEN_LEFT)
						{
							if (j_ptr->tval != TV_RING) continue;
						}
						/* Make sure we're not trying to place a ring elsewhere */
						else if (j_ptr->tval == TV_RING) continue;
						/* Not for the same slot */
						else if (wield_slot(j_ptr) != i) continue;

						/* Compare prices */
						if (object_value(j_ptr) > price[i]) price[i] = object_value(j_ptr);
					}
 				}
			}

			/*base number fo loops on fame, but put in a minimum of 4*/
			repeats = p_ptr->fame / 5;
			if (repeats < 4) repeats = 4;

			/* attempts at an item based on p_ptr fame, but fame is a minimum of 10*/
			for (x = 0; x < repeats; x++)
			{
				bool do_great = FALSE;
				int entry;

				/*create the items in the guild....*/
				store_type *st_ptr = &store[STORE_LIBRARY];

				/* Find first empty slot */
				entry = st_ptr->stock_num;
				
				/* Get the existing object */
				j_ptr = &st_ptr->stock[entry + 1];

				/* Now, produce items */
				i = rand_int(INVEN_TOTAL - INVEN_WIELD) + INVEN_WIELD;

				/* Skip second ring & lite
			     * since rings & jewelery are in the same theme, only count them once
				 */
				if ((i == INVEN_RIGHT) || (i == INVEN_LITE) || (i == INVEN_NECK))
				{
					x--;
				    continue;
				}
				/* Pick a theme  */
				switch (i)
				{

					case INVEN_WIELD:
					{

						/* First tailor the weapon for */
						/* specialists. -CJN- */
						if (p_ptr->skills[SK_HAFTED].skill_max > 1)
						{
							required_tval = TV_HAFTED;						}
						else if (p_ptr->skills[SK_POLEARM].skill_max > 1)
						{
							required_tval = TV_POLEARM;						}
						else if (p_ptr->skills[SK_SWORD].skill_max > 1)
						{
							required_tval = TV_SWORD;						}
						else if (p_ptr->skills[SK_DAGGER].skill_max > 1)
						{
							required_tval = TV_DAGGER;						}
						else if (p_ptr->skills[SK_AXES].skill_max > 1)
						{
							required_tval = TV_AXES;						}
						else if (p_ptr->skills[SK_BLUNT].skill_max > 1)
						{
							required_tval = TV_BLUNT;						}
						else
						{
							/* Non specialists gets */
							/* random type. */
							int choice = rand_int(5);
							switch (choice)
							{
							case 0: required_tval = TV_HAFTED; break;
							case 1: required_tval = TV_POLEARM; break;
							case 2: required_tval = TV_SWORD; break;
							case 3: required_tval = TV_DAGGER; break;
							case 4: required_tval = TV_AXES; break;
							case 5: required_tval = TV_BLUNT; break;
							}
						}
						break;
					}
					case INVEN_GUN:
					{
						required_tval = TV_GUN;
						break;
					}
					case INVEN_LEFT:
					{
						required_tval = TV_RING;
						break;
					}
					case INVEN_BODY:
					{
						if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
						{
							required_tval = TV_MECHA_TORSO;
						}
						else if (one_in_(3)) required_tval = TV_HARD_ARMOR;
						else required_tval = TV_SOFT_ARMOR;
						break;
					}
					case INVEN_OUTER:
					{
						required_tval = TV_CLOAK;
						break;
					}
					case INVEN_LEG:
					{
						required_tval = TV_LEG;
						break;
					}
					case INVEN_HEAD:
					{
						if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
						{
							required_tval = TV_MECHA_HEAD;
						}
						else required_tval = TV_HELM;
						break;
					}
					case INVEN_HANDS:
					{
						if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
						{
							required_tval = TV_MECHA_ARMS;
						}
						else required_tval = TV_GLOVES;
						break;
					}
					case INVEN_FEET:
					{
						if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
						{
							required_tval = TV_MECHA_FEET;
						}
						else required_tval = TV_BOOTS;
						break;
					}

					/*paranoia*/
					default: required_tval = 0;
				}

				/* Wipe the object */
				object_wipe(j_ptr);

				/*sometimes make good counter true to give 4 artifact rarity rolls*/
				if (randint(200) < p_ptr->fame) do_great = TRUE;

				/* Valid item exists?  If not, don't count it*/
				if (!make_object(j_ptr, TRUE, do_great, TRUE))
				{
					x--;
					required_tval = 0;
					continue;
				}

				/* Hack -- in case of artifact, mark it as not created yet */
				if (j_ptr->name1) a_info[j_ptr->name1].cur_num = 0;

				/* Make sure weapon isn't too heavy */
				if ((i == INVEN_WIELD) &&
					((p_ptr->stat_use[A_MUS] / 9 + 5) < j_ptr->weight / 10))
			   	{
					/*don't count this one*/
					x--;
				    continue;
				}
				
				/* Make sure gloves won't ruin spellcasting */
				if ((i == INVEN_HANDS) && (cp_ptr->flags & CF_CUMBER_GLOVE))
				{
					u32b f1, f2, f3;

					object_flags(j_ptr, &f1, &f2, &f3);

					/* Limit to legal glove types */
					if (!((f2 & (TR2_FREE_ACT)) || (get_object_pval(j_ptr, TR_PVAL_AGI) <= 0)))
					{

					    /*don't count this one*/
						x--;

 						continue;
					}
				}

				/* Hack - mark as identified */
				object_known(j_ptr);

				/* Mark price - hack - increase value if we had nothing for this slot */
				if (price[i]) val[i] = (object_value(j_ptr) - price[i]);
				else val[i] = 20 * object_value(j_ptr);

				/* Don't accept items that aren't better than the old ones */
				if (val[i] <= 0) continue;

				/* Best item yet? */
				if (val[i] > diff)
				{
					diff = val[i];

					/* Get local object */
					i_ptr = &object_type_body;

					/* Wipe the object */
					object_wipe(i_ptr);

					/* Structure Copy */
					object_copy(i_ptr, j_ptr);

					got_item = TRUE;

				}

				/* Identical values */
				else if (val[i] == diff)
				{
					/* Maybe use second one */
					if (rand_int(2))
					{
						/* Get local object */
						i_ptr = &object_type_body;

						/* Wipe the object */
						object_wipe(i_ptr);

						/* Structure Copy */
						object_copy(i_ptr, j_ptr);

						got_item = TRUE;
					}
				}

			}
		}
		/* hopefully eliminate the &nothing bug*/
		if (!i_ptr->k_idx)
		{
			got_item = FALSE;
		}

	}

	/* We don't have a tailored item, or the reward is good or great*/
	if (!got_item)
	{

		/* Complex message */
		if (p_ptr->wizard)
		{
			msg_print("Tailored item failed");
		}

		while (!got_item)
		{

			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Make an appropriate object (if possible) */
			if (!make_object(i_ptr, TRUE, TRUE, FALSE)) continue;

			/* hopefully eliminate the &nothing bug*/
			if (!i_ptr->k_idx) continue;

			got_item = TRUE;
		}
	}




	/* Identify it fully */
	object_aware(i_ptr);
	object_known(i_ptr);

	/* Mark the item as fully known */
	i_ptr->ident |= (IDENT_MENTAL);

	/* Handle Artifacts */
	if (i_ptr->name1)
	{
		/*
		 * Artifact might not yet be marked as created (if it was chosen from tailored
		 * rewards), so now it's the right time to mark it
		 */
		a_info[i_ptr->name1].cur_num = 1;
	}

	/* Describe the object */
	object_desc(o_name, i_ptr, TRUE, 3);

	/* Note that the pack is too full */
	if (!inven_carry_okay(i_ptr))
	{
		msg_format("You have no room in your backpack for %s.", o_name);

		/* Drop the object */
		drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);

		/* Inform the player */
		msg_print("Your reward is waiting outside!");

	}

	/* Give it to the player */
	else
	{
		int item_new;

		/* Give it to the player */
		item_new = inven_carry(i_ptr);

		/* Message */
		msg_format("Your reward is %s (%c).", o_name, index_to_label(item_new));
	}

	message_flush();

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/*undo special item level creation*/
	/* chest_or_quest = FALSE; */
}

/*
 * Actually give the character a quest.
 * easy difficulty increases number by 1, medium by 3, hard by 5
 */
static bool place_mon_quest(int q, int lev, int number, int difficulty)
{
	int i, chance, chance2;

	int r_idx;

	alloc_entry *table = alloc_race_table;

	long value;

	int total = 0;

	bool unique = FALSE;

	int max_depth, min_depth;

	monster_race *r_ptr;

	/* Monsters can be up to 1 levels out of depth with difficulty 0 */
	max_depth = 3 + (difficulty / 2) + lev;

	/*assumes lev is always greater than 0*/
	min_depth = 1 + (difficulty / 2) + lev;

	/* Try to place a unique quest*/
	if ((rand_int(100)) < (2 + (2 * (difficulty + (lev / 3)))))
	{
		unique = TRUE;
	}

	/*process the table for uniques*/
	if (unique)
	{
		/*since only one monster, make it a harder one*/
		min_depth +=3;
		max_depth +=3;

		/* Process probabilities, it is OK for this one to fail (no choices),
	     * because then then a non-unique monster is selected
		 */
		for (i = 0; i < alloc_race_size; i++)
		{
			/* Monsters are sorted by depth */
			if (table[i].level > max_depth) break;

			/* Default */
			table[i].prob3 = 0;

			/* not too shallow*/
			if (table[i].level < min_depth) continue;
			if (table[i].level <= 0) continue;

			/* Get the "r_idx" of the chosen monster */
			r_idx = table[i].index;

			/* Get the actual race */
			r_ptr = &r_info[r_idx];

			/* Must be unique and alive and not on level */
			if (!(r_ptr->flags1 & (RF1_UNIQUE))) continue;
			if (r_ptr->cur_num >= r_ptr->max_num) continue;

			/*not too shallow*/
			if (table[i].level < (lev + ((2 * difficulty) / 3)) + 1) continue;

			/* Depth Monsters never appear in quests*/
			if (r_ptr->flags1 & (RF1_FORCE_DEPTH)) continue;

			/* Accept */
			table[i].prob3 = table[i].prob2;

			/* Total */
			total += table[i].prob3;
		}

		/*reverse the depth adjustment above*/
		min_depth -= 3;
		max_depth -= 3;

	}

	/*no eligible uniques*/
	if (!total)
	{
    	unique = FALSE;

	}

	/* Mark as a unique quest */
	else
	{
		q_info[q].type = QUEST_UNIQUE;

		number = 1;
	}

	/* Not a unique quest */
	if (!unique)
	{
		/*can't make the monsters harder, so make more of them*/
		if (min_depth > 40)
		{
			i = (min_depth - 40) /2;

			number += i;

			min_depth = 40;
		}

		/* Occasional "nasty" monster. but less of them */
		if (rand_int(NASTY_MON) < difficulty)
		{
			/* Pick a level bonus */
			int d = max_depth / 4 + 2;

			/* Boost the level */
			max_depth += ((d < 4) ? d : 4);
			min_depth += ((d < 4) ? d : 4);

			/*reduce the number*/
			number -= ((d < 4) ? d : 4);
		}

		/* More likely chance of climbing down, but adding more monsters */
		else if (rand_int(70) < (difficulty * 10))
		{
			number += (damroll(2,2) - 1);
			max_depth--;
			min_depth--;
		}

		/* Process probabilities */
		for (i = 0; i < alloc_race_size; i++)
		{
			/* Monsters are sorted by depth */
			if (table[i].level > max_depth) break;

			/* Default */
			table[i].prob3 = 0;

			/* not too shallow*/
			if (table[i].level < min_depth) continue;
			if (table[i].level <= 0) continue;

			/* Get the "r_idx" of the chosen monster */
			r_idx = table[i].index;

			/* Get the actual race */
			r_ptr = &r_info[r_idx];

			/* No uniques*/
			if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

			/*no quests for certain kinds of animals the guild wouldn't care about*/
			if ((strchr("IJKSRTefiklmnrtw,", r_ptr->d_char)) &&
				(!(r_ptr->flags2 & (RF2_SMART)))) continue;

			/* Hack -- No town monsters in quests */
			if (table[i].level <= 0) continue;

			/* Never any monster that multiplies */
			if (r_ptr->flags2 & RF2_MULTIPLY) continue;

			/* Depth Monsters never appear in quests*/
			if (r_ptr->flags1 & (RF1_FORCE_DEPTH)) continue;

			/* Check if a immobile monster can still hurt the player from a distance */
			if (r_ptr->flags1 & RF1_NEVER_MOVE)
			{
				bool okay = FALSE;

				/* Allow if the monster has at least one summon spell*/
				if (r_ptr->flags7)	okay = TRUE;

				/* Allow if the monster can attack */
				if ((r_ptr->flags4 & (RF4_ATTACK_MASK)) ||
					(r_ptr->flags5 & (RF5_ATTACK_MASK)) ||
					(r_ptr->flags6 & (RF6_ATTACK_MASK)))
					okay = TRUE;
				/* Allow if the monster can bring the player to itself */
				if  (r_ptr->flags6 & (RF6_TELE_TO))
					okay = TRUE;

				if (!okay) continue;
			}

			/* Accept */
			table[i].prob3 = table[i].prob2;

			/* Total */
			total += table[i].prob3;

		}

		/* Paranoia */
		if (number <= 0) number = 1;
		if (number > 63) number = 63;

		/* Actually write the quest */
		if (total > 0) q_info[q].type = QUEST_GUILD;
	}

	/* Paranoia */
	if (total == 0)
	{
		/* No monsters - no quest */
		msg_print("There are no elligable monsters to quest for");

		return FALSE;
	}

	/* Select the monster from eligible monsters */
	else
	{
		/* Pick a monster */
		value = rand_int(total);

		/* Find the monster */
		for (i = 0; i < alloc_race_size; i++)
		{
			/* Found the entry */
			if (value < table[i].prob3) break;

			/* Decrement */
			value = value - table[i].prob3;


		}

	}

	/* Set up the quest */
	q_info[q].mon_idx = table[i].index;
	q_info[q].max_num = number;
	q_info[q].cur_num = 0;
	q_info[q].started = FALSE;
	q_info[q].base_level = lev;
	q_info[q].active_level = lev;

	/* Set current quest */
	p_ptr->cur_quest = lev;

	/* Decide on reward type */

	/* Chance of gold reward */
	chance = 95 - ((difficulty - 1) * 20) - (lev * 2);

	/* Better rewards for unique quests */
	if (unique) chance -= 10;

	/* No negative chances */
	if (chance < 0) chance = 0;

	/* First roll for gold award */
	if (rand_int(100) < chance) q_info[q].reward = REWARD_GOLD;

	else
	{
		/* Maybe a tailored reward */
		if (10 + rand_int(50) < p_ptr->fame) q_info[q].reward = REWARD_TAILORED;
		else q_info[q].reward = REWARD_SKILL;
	}

	/*success*/
	return TRUE;
}

/*
 * Actually give the character a quest
 */
static bool place_vault_quest(int q, int lev)
{
	/* Actually write the quest */
	q_info[q].type = QUEST_VAULT;
	q_info[q].base_level = lev;
	q_info[q].active_level = lev;
	if (10 + rand_int(50) < p_ptr->fame) q_info[q].reward = REWARD_TAILORED;
	else q_info[q].reward = REWARD_SKILL;
	q_info[q].cur_num = q_info[q].mon_idx = q_info[q].max_num = 0;
	q_info[q].started = FALSE;

	/* Set current quest */
	p_ptr->cur_quest = lev;

	/*success*/
	return TRUE;
}

/*
 * Display the "contents" of the adventurer's guild
 */
void display_guild(void)
{
	int i, j;
	byte attr;
	cptr p, q_out;

	bool do_reward;

	/* Check for outstanding rewards */
	for (i = 0; i < z_info->q_max ; i++)
	{
		do_reward = FALSE;

		if ((q_info[i].type == QUEST_GUILD) || (q_info[i].type == QUEST_UNIQUE))
		{
			/* Skip incomplete quests */
			if (q_info[i].active_level) continue;

			/* Check to see if there's a reward */
			if (!q_info[i].reward) continue;

			else
			{
				do_reward = TRUE;

				/* Grant fame bonus */
				p_ptr->fame += randint(4);

				/* Reset the quest */
				p_ptr->cur_quest = 0;
			}
		}

		else if (q_info[i].type == QUEST_VAULT)
		{
			/* Check to see if there's a reward */
			if (!q_info[i].reward) continue;

			/* Find quest item in the inventory*/
			j = quest_item_slot();

			/* Didn't find it */
			if (j == -1) continue;

			/* Found it */
			do_reward = TRUE;

			/* Grant fame bonus */
			p_ptr->fame += 5;

			/* Finish the quest */
			q_info[i].active_level = 0;

			/* Reset the quest */
			p_ptr->cur_quest = 0;

			/* Destroy the quest item in the pack */
			inven_item_increase(j, -1);
			inven_item_optimize(j);
		}

		/* Reward the player */
		if (do_reward)
		{
			/* Generate object at quest level */
			object_level = q_info[i].base_level;

			/* Create the reward*/
			grant_reward(q_info[i].reward);

			/* Notice stuff */
			notice_stuff();

			/* Handle stuff */
			handle_stuff();

			/* Reset object level */
			object_level = p_ptr->depth;

			/* Reset the reward */
			q_info[i].reward = 0;

			/* Clear the screen */
			Term_clear();

		}
	}

	/* Describe the guild */
	/* put_str("The Adventurer's Guild", 3, 30); */

	/* Player's title */
	if (p_ptr->fame > 110) p = "oh glorious one";
	else if (p_ptr->fame > 80) p = "oh great one";
	else if (p_ptr->fame > 50)
	{
		if (p_ptr->psex == SEX_MALE) p = "my lord";
		else p = "my lady";
	}
	else if (p_ptr->fame > 25)
	{
		if (p_ptr->psex == SEX_MALE) p = "sir";
		else p = "madam";
	}
	else if (p_ptr->fame > 10)
	{
		if (!op_ptr->full_name[0])
		{
			p = c_name + cp_ptr->name;
		}
		else p = op_ptr->full_name;
	}
	else p = "bloke";

	/* Introduction */
	put_str(format("Do you wish to take a quest, %s?", p), 5, 3);

	/* Player's reputation */
	switch (p_ptr->fame / 5)
	{
		case 0:
		{
			attr = TERM_RED;
			p = "poor";
			break;
		}
		case 1:
		case 2:
		case 3:
		{
			attr = TERM_YELLOW;
			p = "fair";
			break;
		}
		case 4:
		case 5:
		case 6:
		{
			attr = TERM_YELLOW;
			p = "good";
			break;
		}
		case 7:
		case 8:
		{
			attr = TERM_YELLOW;
			p = "very good";
			break;
		}
		case 9:
		case 10:
		{
			attr = TERM_L_GREEN;
			p = "excellent";
			break;
		}
		case 11:
		case 12:
		case 13:
		{
			attr = TERM_L_GREEN;
			p = "superb";
			break;
		}
		case 14:
		case 15:
		case 16:
		case 17:
		{
			attr = TERM_L_GREEN;
			p = "heroic";
			break;
		}
		default:
		{
			attr = TERM_L_GREEN;
			p = "legendary";
			break;
		}
	}

	/* Quest count */
	put_str("You might be interested to know that your current reputation is", 6, 3);
	c_put_str(attr, p, 6, 67);

	/* Label the quest descriptions */
	if (!p_ptr->cur_quest)
	{
		/* Not Currently in a quest */
		put_str("Available Quests: (press (a) to select)", 8, 3);

		avail_quest = 0;

		for (j = 0;j < GUILD_QUESTS; j++)
		{
			if (guild[j] <= 2)
			{
				attr = TERM_L_GREEN;
				p = "An easy";
			}
			else if (guild[j] <= 5)
			{
				attr = TERM_YELLOW;
				p = "A moderate";
			}
			else
			{
				attr = TERM_ORANGE;
				p = "A difficult";
			}

			put_str(format("%c)", I2A(avail_quest)), 10 + avail_quest, 0);
			c_put_str(attr, format ("%s quest.", p), 10 + avail_quest, 4);
			avail_quest++;
		}

		/*sometimes offer a vault quest*/
		if (p_ptr->fame % GUILD_FAME_SPECIAL == 7)
		{
			put_str(format("%c)", I2A(avail_quest)), 10 + avail_quest, 0);
			c_put_str(TERM_VIOLET, "A special quest.", 10 + avail_quest, 4);
			avail_quest++;
		}
	}

	else
	{
		put_str("Your current quest:", 8, 3);
		q_out = describe_quest(p_ptr->cur_quest, QMODE_FULL);

		/* Break into two lines if necessary*/
		if (strlen(q_out) < 70) put_str(q_out, 10, 3);
		else
		{
			q_out = describe_quest(p_ptr->cur_quest, QMODE_HALF_1);
			put_str(q_out, 10, 3);
			q_out = describe_quest(p_ptr->cur_quest, QMODE_HALF_2);
			put_str(q_out, 11, 3);
		}
	}


}

/*
 * Choose a quest from the list
 */
static int get_quest(void)
{
	int item;
	char which;
	char buf[160];

	/* Build the prompt */
	sprintf(buf, "(Items %c-%c, ESC to exit)", I2A(0), I2A(avail_quest - 1));

	/* Ask until done */
	while (TRUE)
	{
		/* Escape */
		if (!get_com(buf, &which)) return (-1);

		/* Lowercase */
		which = tolower((unsigned char)which);

		/* Convert response to item */
		item = A2I(which);

		/* Oops */
		if ((item < 0) || (item > avail_quest - 1 ))
		{
			/* Oops */
			bell("Illegal guild quest choice!");
			continue;
		}

		break;
	}

	/* Success */
	return (item);
}

/*
 * "Purchase" a quest from the guild
 */
void guild_purchase(void)
{
	int item;
	int i, qlev, num;
	int slot = 0;
	bool found = FALSE;

	if (p_ptr->cur_quest)
	{
		msg_print("Finish your current quest first!");
		return;
	}

	/* Get the quest number */
	item = get_quest();

	/* Quit if no quest chosen */
	if (item == -1) return;

	/* Get level for quest - if never been in dungeon at 100', otherwise 1-2 levels deeper */
	if (!p_ptr->max_depth) qlev = 1 + randint(3);
	else qlev = p_ptr->max_depth + 2 + rand_int(2);

	/*nowhere to quest*/
	if (qlev >= 51)
	{
		msg_print("You have completely conquered the dungeon!");
		return;
	}

	/* Check list of quests */
	for (i = 0; i < z_info->q_max ; i++)
	{
		/* check fixed quests to see that they're not on the same level*/
		if ((q_info[i].type == QUEST_FIXED) || (q_info[i].type == QUEST_FIXED_U))
		{
			if (q_info[i].active_level == qlev)
			{
				msg_print("A greater task lies before you!");
				return;
			}
		}
		found = TRUE;
		slot = i;
	}


	if ((found) || (slot))
	{
		if (item < GUILD_QUESTS)
		{
			/* How many monsters? */
			num = damroll(4,4) + 1 + (p_ptr->fame / 30);
			if (!place_mon_quest(slot, qlev, num, guild[item]))
			{
		    	return;
			}

		}
		else if (!place_vault_quest(slot, qlev)) return;
	}

	else msg_print("You can't accept any more quests!");

	/* Clear screen */
	Term_clear();

	display_guild();

	return;

}

/*
 * Hack -- Check if a level is a "quest" level - returns quest type
 */
byte quest_check(int lev)
{
	int i;

	/* Town is never a quest */
	if (!lev) return 0;

	/* Check quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		/* Check for quest */
		if (q_info[i].active_level == lev) return (q_info[i].type);
	}

	/* Nope */
	return 0;
}

/*
 * Return the index of the quest for current level
 */
int quest_num(int lev)
{
	int i;

	/* Town is never a quest */
	if (!lev) return 0;

	/* Count incomplete quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		/* Quest level? */
		if (q_info[i].active_level == lev) return i;
	}

	/* No quest */
	return 0;
}

/*
 * Return the slot of a quest item in the inventory
 */
int quest_item_slot(void)
{
	int i;
	object_type *o_ptr;

	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		if (o_ptr->ident & IDENT_QUEST) return (i);
	}

	/* No quest item */
	return -1;
}

/*
 * Fail your quest
 */
void quest_fail(void)
{
	quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];
	monster_race *r_ptr = &r_info[q_ptr->mon_idx];

	/* Message */
	msg_print("You have failed in your quest!");

	/* Reputation penalty */
	if (p_ptr->fame) p_ptr->fame /= 2;

	/* Mark quest as done */
	q_ptr->active_level = 0;
	p_ptr->cur_quest = 0;

	/* No reward for failed quest */
	q_ptr->reward = 0;

	/* Disturb */
	if (disturb_minor) disturb(0,0);
}
