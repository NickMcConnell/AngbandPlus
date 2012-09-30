/* File: bldg.c */

/*
 * Purpose: Building commands
 * Created by Ken Wigle for Kangband - a variant of Angband 2.8.3
 * -KMW-
 *
 * Rewritten for Kangband 2.8.3i using Kamband's version of
 * bldg.c as written by Ivan Tkatchev
 *
 * Changed for ZAngband by Robert Ruehlmann
 * Rewritten yet again by Steven Fuerst to use the fields code.
 */

#include "angband.h"


void have_nightmare(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	char m_name[80];
	bool happened = FALSE;
	int power = r_ptr->level + 10;
	cptr desc = r_name + r_ptr->name;


	if (!(r_ptr->flags1 & RF1_UNIQUE))
	{
		/* Describe it */
		sprintf(m_name, "%s %s", (is_a_vowel(desc[0]) ? "an" : "a"), desc);

		if (r_ptr->flags1 & RF1_FRIENDS)
		{
			power /= 2;
		}
	}
	else
	{
		/* Describe it */
		sprintf(m_name, "%s", desc);

		power *= 2;
	}

	if (saving_throw(p_ptr->skill_sav * 100 / power))
	{
		msg_format("%^s chases you through your dreams.", m_name);

		/* Safe */
		return;
	}

	if (p_ptr->image)
	{
		/* Something silly happens... */
		msg_format("You behold the %s visage of %s!",
					  funny_desc[randint0(MAX_SAN_FUNNY)], m_name);

		if (one_in_(3))
		{
			msg_print(funny_comments[randint0(MAX_SAN_COMMENT)]);
			p_ptr->image = p_ptr->image + randint1(r_ptr->level);
		}

		/* Never mind; we can't see it clearly enough */
		return;
	}

	/* Something frightening happens... */
	msg_format("You behold the %s visage of %s!",
				  horror_desc[randint0(MAX_SAN_HORROR)], desc);

	r_ptr->r_flags2 |= RF2_ELDRITCH_HORROR;

	switch(p_ptr->prace)
	{
		/* Imps may make a saving throw */
		case RACE_IMP:
		{
			if (saving_throw(20 + p_ptr->lev)) return;

			break;
		}
		/* Undead may make a saving throw */
		case RACE_SKELETON:
		case RACE_ZOMBIE:
		case RACE_SPECTRE:
		case RACE_VAMPIRE:
		{
			if (saving_throw(10 + p_ptr->lev)) return;

			break;
		}
	}

	/* Mind blast */
	if (!saving_throw(p_ptr->skill_sav * 100 / power))
	{
		if (!p_ptr->resist_confu)
		{
			(void)set_confused(p_ptr->confused + randint0(4) + 4);
		}
		if (!p_ptr->resist_chaos && one_in_(3))
		{
			(void)set_image(p_ptr->image + randint0(250) + 150);
		}
		return;
	}

	/* Lose int & wis */
	if (!saving_throw(p_ptr->skill_sav * 100 / power))
	{
		do_dec_stat(A_INT);
		do_dec_stat(A_WIS);
		return;
	}

	/* Brain smash */
	if (!saving_throw(p_ptr->skill_sav * 100 / power))
	{
		if (!p_ptr->resist_confu)
		{
			(void)set_confused(p_ptr->confused + randint0(4) + 4);
		}
		if (!p_ptr->free_act)
		{
			(void)set_paralyzed(p_ptr->paralyzed + randint0(4) + 4);
		}
		while (!saving_throw(p_ptr->skill_sav))
		{
			(void)do_dec_stat(A_INT);
		}
		while (!saving_throw(p_ptr->skill_sav))
		{
			(void)do_dec_stat(A_WIS);
		}
		if (!p_ptr->resist_chaos)
		{
			(void)set_image(p_ptr->image + randint0(250) + 150);
		}
		return;
	}

	/* Permanent lose int & wis */
	if (!saving_throw(p_ptr->skill_sav * 100 / power))
	{
		if (dec_stat(A_INT, 10, TRUE)) happened = TRUE;
		if (dec_stat(A_WIS, 10, TRUE)) happened = TRUE;
		if (happened)
		{
			msg_print("You feel much less sane than before.");
		}
		return;
	}

	/* Amnesia */
	if (!saving_throw(p_ptr->skill_sav * 100 / power))
	{
		if (lose_all_info())
		{
			msg_print("You forget everything in your utmost terror!");
		}
		return;
	}

	/* Else gain permanent insanity */
	if ((p_ptr->muta3 & MUT3_MORONIC) && (p_ptr->muta2 & MUT2_BERS_RAGE) &&
		((p_ptr->muta2 & MUT2_COWARDICE) || (p_ptr->resist_fear)) &&
		((p_ptr->muta2 & MUT2_HALLU) || (p_ptr->resist_chaos)))
	{
		/* The poor bastard already has all possible insanities! */
		return;
	}

	while (!happened)
	{
		switch (randint1(4))
		{
			case 1:
			{
				if (!(p_ptr->muta3 & MUT3_MORONIC))
				{
					msg_print("You turn into an utter moron!");
					if (p_ptr->muta3 & MUT3_HYPER_INT)
					{
						msg_print("Your brain is no longer a living computer.");
						p_ptr->muta3 &= ~(MUT3_HYPER_INT);
					}
					p_ptr->muta3 |= MUT3_MORONIC;
					happened = TRUE;
				}
				break;
			}
			case 2:
			{
				if (!(p_ptr->muta2 & MUT2_COWARDICE) && !p_ptr->resist_fear)
				{
					msg_print("You become paranoid!");

					/* Duh, the following should never happen, but anyway... */
					if (p_ptr->muta3 & MUT3_FEARLESS)
					{
						msg_print("You are no longer fearless.");
						p_ptr->muta3 &= ~(MUT3_FEARLESS);
					}

					p_ptr->muta2 |= MUT2_COWARDICE;
					happened = TRUE;
				}
				break;
			}
			case 3:
			{
				if (!(p_ptr->muta2 & MUT2_HALLU) && !p_ptr->resist_chaos)
				{
					msg_print("You are afflicted by a hallucinatory insanity!");
					p_ptr->muta2 |= MUT2_HALLU;
					happened = TRUE;
				}
				break;
			}
			default:
			{
				if (!(p_ptr->muta2 & MUT2_BERS_RAGE))
				{
					msg_print("You become subject to fits of berserk rage!");
					p_ptr->muta2 |= MUT2_BERS_RAGE;
					happened = TRUE;
				}
				break;
			}
		}
	}

	p_ptr->update |= PU_BONUS;
	handle_stuff();
}


bool get_nightmare(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Require eldritch horrors */
	if (!(r_ptr->flags2 & (RF2_ELDRITCH_HORROR))) return (FALSE);

	/* Require high level */
	if (r_ptr->level <= p_ptr->lev) return (FALSE);

	/* Accept this monster */
	return (TRUE);
}


/* Array of places to find an inscription */
static cptr find_quest[] =
{
	"You find the following inscription in the floor",
	"You see a message inscribed in the wall",
	"There is a sign saying",
	"Something is written on the staircase",
	"You find a scroll with the following message",
	"You hear",
};


/*
 * Discover quest
 */
void quest_discovery(int q_idx)
{
	quest_type      *q_ptr = &quest[q_idx];
	monster_race    *r_ptr = &r_info[q_ptr->r_idx];
	int             q_num = q_ptr->max_num - q_ptr->cur_num;
	char            name[80];

	/* No quest index */
	if (!q_idx) return;

	strcpy(name, (r_name + r_ptr->name));

	msg_print(find_quest[rand_range(0, 5)]);
	msg_print(NULL);

	if (q_num == 1)
	{
		/* Unique */
		msg_format("Beware, this level is protected by %s!", name);
	}
	else
	{
		/* Normal monsters */
		plural_aux(name);
		msg_format("Be warned, this level is guarded by %d %s!", q_num, name);
	}
}


/*
 * Hack -- Check if a level is a "quest" level
 */
int quest_number(int level)
{
	int i;

	/* Check quests */
	if (p_ptr->inside_quest)
		return (p_ptr->inside_quest);

	for (i = 0; i < max_quests; i++)
	{
		if (quest[i].status != QUEST_STATUS_TAKEN) continue;

		if ((quest[i].type == QUEST_TYPE_KILL_LEVEL) &&
			!(quest[i].flags & QUEST_FLAG_PRESET) &&
		    (quest[i].level == level))
			return (i);
	}

	/* Check for random quest */
	return (random_quest_number(level));
}


/*
 * Return the index of the random quest on this level
 * (or zero)
 */
int random_quest_number(int level)
{
	int i;

	for (i = MIN_RANDOM_QUEST; i < MAX_RANDOM_QUEST + 1; i++)
	{
		if ((quest[i].type == QUEST_TYPE_RANDOM) &&
		    (quest[i].status == QUEST_STATUS_TAKEN) &&
		    (quest[i].level == level))
		{
			return i;
		}
	}

	/* Nope */
	return 0;
}

/* Count the number of random quests chosen */
int number_of_quests(void)
{
	int i, j;

	/* Clear the counter */
	i = 0;

	for (j = MIN_RANDOM_QUEST; j < MAX_RANDOM_QUEST + 1; j++)
	{
		if (quest[j].status != QUEST_STATUS_UNTAKEN)
		{
			/* Increment count of quests taken. */
			i++;
		}
	}

	/* Return the number of quests taken */
	return (i);
}


#if 0

/* hack as in leave_store in store.c */

static bool is_owner(building_type *bldg)
{
	if (bldg->member_class[p_ptr->pclass] == BUILDING_OWNER)
	{
		return (TRUE);
	}

	if (bldg->member_race[p_ptr->prace] == BUILDING_OWNER)
	{
		return (TRUE);
	}

	if ((bldg->member_realm[p_ptr->realm1] == BUILDING_OWNER) ||
		(bldg->member_realm[p_ptr->realm2] == BUILDING_OWNER))
	{
		return (TRUE);
	}

	return (FALSE);
}


static bool is_member(building_type *bldg)
{
	if (bldg->member_class[p_ptr->pclass])
	{
		return (TRUE);
	}

	if (bldg->member_race[p_ptr->prace])
	{
		return (TRUE);
	}

	if ((bldg->member_realm[p_ptr->realm1]) || 
		(bldg->member_realm[p_ptr->realm2]))
	{
		return (TRUE);
	}

	return (FALSE);
}

#endif /* 0 */

/*
 * Clear the building information
 */
static void clear_bldg(int min_row, int max_row)
{
	int   i;

	for (i = min_row; i <= max_row; i++)
		prt("", i, 0);
}


static void building_prt_gold(void)
{
	char tmp_str[80];

	prt("Gold Remaining: ", 23, 53);

	sprintf(tmp_str, "%9ld", (long)p_ptr->au);
	prt(tmp_str, 23, 68);
}


/*
 * Display a building.
 */
static void display_build(field_type *f_ptr, store_type *b_ptr)
{
	char tmp_str[80];
	
	b_own_type *bo_ptr = &b_owners[f_ptr->data[0]][b_ptr->owner];
	
	cptr build_name = t_info[f_ptr->t_idx].name;
	cptr owner_name = (bo_ptr->owner_name);
	cptr race_name = race_info[bo_ptr->owner_race].title;

	Term_clear();
	sprintf(tmp_str, "%s (%s) %s", owner_name, race_name, build_name);
	prt(tmp_str, 2, 1);
	prt("You may:", 19, 0);


	prt(" ESC) Exit building", 23, 0);

	/* Display building-specific information */
	switch (f_ptr->data[0])
	{
		case BLDG_WEAPONMASTER:
		{
			sprintf(tmp_str, " E) Examine Weapons (%dgp)",
				 f_ptr->data[1] * bo_ptr->inflate);
			c_put_str(TERM_YELLOW, tmp_str, 19, 35);
			break;
		}
		
		case BLDG_RECHARGE:
		{
			sprintf(tmp_str, " R) Recharge Items");
			c_put_str(TERM_YELLOW, tmp_str, 19, 0);
			sprintf(tmp_str, " I) Identify Items (%dgp)",
			f_ptr->data[2] * bo_ptr->inflate);
			c_put_str(TERM_YELLOW, tmp_str, 19, 35);
			break;
		}
		
		case BLDG_PLUS_WEAPON:
		{
			sprintf(tmp_str, " E) Enchant Weapons (%dgp)",
				 f_ptr->data[1] * bo_ptr->inflate);
			c_put_str(TERM_YELLOW, tmp_str, 19, 35);
			break;
		}
		
		case BLDG_PLUS_ARMOUR:
		{
			sprintf(tmp_str, " E) Enchant Armour (%dgp)",
				 f_ptr->data[1] * bo_ptr->inflate);
			c_put_str(TERM_YELLOW, tmp_str, 19, 35);
			break;
		}
		
		case BLDG_MUTATE:
		{
			sprintf(tmp_str, " E) Expose yourself to raw chaos (%dgp)",
				 f_ptr->data[1] * bo_ptr->inflate * (count_mutations() + 1));
			c_put_str(TERM_YELLOW, tmp_str, 19, 30);
			break;
		}
		
		case BLDG_MAP:
		{
			sprintf(tmp_str, " B) Buy Map (%dgp)",
				 f_ptr->data[1] * bo_ptr->inflate);
			c_put_str(TERM_YELLOW, tmp_str, 19, 35);
			break;
		}
	}


	building_prt_gold();
}

#if 0

/*
 * display fruit for dice slots
 */
static void display_fruit(int row, int col, int fruit)
{
	switch (fruit)
	{
		case 0: /* lemon */
			c_put_str(TERM_YELLOW, "   ####.", row, col);
			c_put_str(TERM_YELLOW, "  #    #", row + 1, col);
			c_put_str(TERM_YELLOW, " #     #", row + 2, col);
			c_put_str(TERM_YELLOW, "#      #", row + 3, col);
			c_put_str(TERM_YELLOW, "#      #", row + 4, col);
			c_put_str(TERM_YELLOW, "#     # ", row + 5, col);
			c_put_str(TERM_YELLOW, "#    #  ", row + 6, col);
			c_put_str(TERM_YELLOW, ".####   ", row + 7, col);
			prt(                   " Lemon  ", row + 8, col);
			break;
		case 1: /* orange */
			c_put_str(TERM_ORANGE, "   ##   ", row, col);
			c_put_str(TERM_ORANGE, "  #..#  ", row + 1, col);
			c_put_str(TERM_ORANGE, " #....# ", row + 2, col);
			c_put_str(TERM_ORANGE, "#......#", row + 3, col);
			c_put_str(TERM_ORANGE, "#......#", row + 4, col);
			c_put_str(TERM_ORANGE, " #....# ", row + 5, col);
			c_put_str(TERM_ORANGE, "  #..#  ", row + 6, col);
			c_put_str(TERM_ORANGE, "   ##   ", row + 7, col);
			prt(                   " Orange ", row + 8, col);
			break;
		case 2: /* sword */
			c_put_str(TERM_SLATE, "   /\\  " , row, col);
			c_put_str(TERM_SLATE, "   ##   " , row + 1, col);
			c_put_str(TERM_SLATE, "   ##   " , row + 2, col);
			c_put_str(TERM_SLATE, "   ##   " , row + 3, col);
			c_put_str(TERM_SLATE, "   ##   " , row + 4, col);
			c_put_str(TERM_SLATE, "   ##   " , row + 5, col);
			c_put_str(TERM_UMBER, " ###### " , row + 6, col);
			c_put_str(TERM_UMBER, "   ##   " , row + 7, col);
			prt(                  " Sword  " , row + 8, col);
			break;
		case 3: /* shield */
			c_put_str(TERM_SLATE, " ###### ", row, col);
			c_put_str(TERM_SLATE, "#      #", row + 1, col);
			c_put_str(TERM_SLATE, "# ++++ #", row + 2, col);
			c_put_str(TERM_SLATE, "# +==+ #", row + 3, col);
			c_put_str(TERM_SLATE, "#  ++  #", row + 4, col);
			c_put_str(TERM_SLATE, " #    # ", row + 5, col);
			c_put_str(TERM_SLATE, "  #  #  ", row + 6, col);
			c_put_str(TERM_SLATE, "   ##   ", row + 7, col);
			prt(                  " Shield ", row + 8, col);
			break;
		case 4: /* plum */
			c_put_str(TERM_VIOLET, "   ##   ", row, col);
			c_put_str(TERM_VIOLET, " ###### ", row + 1, col);
			c_put_str(TERM_VIOLET, "########", row + 2, col);
			c_put_str(TERM_VIOLET, "########", row + 3, col);
			c_put_str(TERM_VIOLET, "########", row + 4, col);
			c_put_str(TERM_VIOLET, " ###### ", row + 5, col);
			c_put_str(TERM_VIOLET, "  ####  ", row + 6, col);
			c_put_str(TERM_VIOLET, "   ##   ", row + 7, col);
			prt(                   "  Plum  ", row + 8, col);
			break;
		case 5: /* cherry */
			c_put_str(TERM_RED, "      ##", row, col);
			c_put_str(TERM_RED, "   ###  ", row + 1, col);
			c_put_str(TERM_RED, "  #..#  ", row + 2, col);
			c_put_str(TERM_RED, "  #..#  ", row + 3, col);
			c_put_str(TERM_RED, " ###### ", row + 4, col);
			c_put_str(TERM_RED, "#..##..#", row + 5, col);
			c_put_str(TERM_RED, "#..##..#", row + 6, col);
			c_put_str(TERM_RED, " ##  ## ", row + 7, col);
			prt(                " Cherry ", row + 8, col);
			break;
	}
}


/*
 * gamble_comm
 */
static bool gamble_comm(int cmd)
{
	int roll1, roll2, roll3, choice, odds, win;
	s32b wager;
	s32b maxbet;
	s32b oldgold;
	static const char *fruit[6] = {"Lemon", "Orange", "Sword", "Shield", "Plum", "Cherry"};
	char out_val[160], tmp_str[80], again;
	cptr p;

	screen_save();

	if (cmd == BACT_GAMBLE_RULES)
	{
		/* Peruse the gambling help file */
		(void)show_file("gambling.txt", NULL, 0, 0);
	}
	else
	{
		/* No money */
		if (p_ptr->au < 1)
		{
			msg_print("Hey! You don't have gold - get out of here!");
			msg_print(NULL);
			screen_load();
			return FALSE;
		}

		clear_bldg(5, 23);

		/* Set maximum bet */
		if (p_ptr->lev < 10)
			maxbet = p_ptr->lev * 100;
		else
			maxbet = p_ptr->lev * 1000;

		/* We can't bet more than we have */
		maxbet = MIN(maxbet, p_ptr->au);

		/* Get the wager */
		strcpy(out_val, "");
		sprintf(tmp_str,"Your wager (1-%ld) ? ", maxbet);

		/*
		 * Use get_string() because we may need more than
		 * the s16b value returned by get_quantity().
		 */
		if (get_string(tmp_str, out_val, 32))
		{
			/* Strip spaces */
			for (p = out_val; *p == ' '; p++);

			/* Get the wager */
			wager = atol(p);

			if (wager > p_ptr->au)
			{
				msg_print("Hey! You don't have the gold - get out of here!");
				msg_print(NULL);
				screen_load();
				return (FALSE);
			}
			else if (wager > maxbet)
			{
				msg_format("I'll take %ld gold of that. Keep the rest.", maxbet);
				wager = maxbet;
			}
			else if (wager < 1)
			{
				msg_print("Ok, we'll start with 1 gold.");

				wager = 1;
			}
			msg_print(NULL);
			win = FALSE;
			odds = 0;
			oldgold = p_ptr->au;

			sprintf(tmp_str, "Gold before game: %9ld", oldgold);
			prt(tmp_str, 20, 2);

			sprintf(tmp_str, "Current Wager:    %9ld", wager);
			prt(tmp_str, 21, 2);

			/* Prevent savefile-scumming of the casino */
			Rand_quick = TRUE;
			Rand_value = time(NULL);

			do
			{
				switch (cmd)
				{
				 case BACT_IN_BETWEEN: /* Game of In-Between */
					c_put_str(TERM_GREEN, "In Between", 5, 2);
					odds = 3;
					win = FALSE;
					roll1 = randint1(10);
					roll2 = randint1(10);
					choice = randint1(10);
					sprintf(tmp_str, "Black die: %d       Black Die: %d", roll1, roll2);
					prt(tmp_str, 8, 3);
					sprintf(tmp_str, "Red die: %d", choice);
					prt(tmp_str, 11, 14);
					if (((choice > roll1) && (choice < roll2)) ||
						((choice < roll1) && (choice > roll2)))
						win = TRUE;
					break;
				case BACT_CRAPS:  /* Game of Craps */
					c_put_str(TERM_GREEN, "Craps", 5, 2);
					win = 3;
					odds = 1;
					roll1 = randint1(6);
					roll2 = randint1(6);
					roll3 = roll1 + roll2;
					choice = roll3;
					sprintf(tmp_str, "First roll: %d %d    Total: %d", roll1,
						 roll2, roll3);
					prt(tmp_str, 7, 5);
					if ((roll3 == 7) || (roll3 == 11))
						win = TRUE;
					else if ((roll3 == 2) || (roll3 == 3) || (roll3 == 12))
						win = FALSE;
					else
						do
						{
							msg_print("Hit any key to roll again");
							msg_print(NULL);
							roll1 = randint1(6);
							roll2 = randint1(6);
							roll3 = roll1 + roll2;

							sprintf(tmp_str, "Roll result: %d %d   Total:     %d",
								 roll1, roll2, roll3);
							prt(tmp_str, 8, 5);
							if (roll3 == choice)
								win = TRUE;
							else if (roll3 == 7)
								win = FALSE;
						} while ((win != TRUE) && (win != FALSE));
					break;

				case BACT_SPIN_WHEEL:  /* Spin the Wheel Game */
					win = FALSE;
					odds = 8;
					c_put_str(TERM_GREEN, "Wheel", 5, 2);
					prt("0  1  2  3  4  5  6  7  8  9", 7, 5);
					prt("--------------------------------", 8, 3);
					strcpy(out_val, "");
					get_string("Pick a number (0-9): ", out_val, 32);
					for (p = out_val; *p == ' '; p++);
					choice = atol(p);
					if (choice < 0)
					{
						msg_print("I'll put you down for 0.");
						choice = 0;
					}
					else if (choice > 9)
					{
						msg_print("Ok, I'll put you down for 9.");
						choice = 9;
					}
					msg_print(NULL);
					roll1 = randint0(10);
					sprintf(tmp_str, "The wheel spins to a stop and the winner is %d",
						roll1);
					prt(tmp_str, 13, 3);
					prt("", 9, 0);
					prt("*", 9, (3 * roll1 + 5));
					if (roll1 == choice)
						win = TRUE;
					break;

				case BACT_DICE_SLOTS: /* The Dice Slots */
					c_put_str(TERM_GREEN, "Dice Slots", 5, 2);
					win = FALSE;
					roll1 = randint1(6);
					roll2 = randint1(6);
					choice = randint1(6);
					(void)sprintf(tmp_str, "%s %s %s", fruit[roll1 - 1], fruit[roll2 - 1],
						 fruit[choice - 1]);
					prt(tmp_str, 15, 37);
					prt("/--------------------------\\", 7, 2);
					prt("\\--------------------------/", 17, 2);
					display_fruit(8,  3, roll1 - 1);
					display_fruit(8, 12, roll2 - 1);
					display_fruit(8, 21, choice - 1);
					if ((roll1 == roll2) && (roll2 == choice))
					{
						win = TRUE;
						if (roll1 == 1)
							odds = 4;
						else if (roll1 == 2)
							odds = 6;
						else
							odds = roll1 * roll1;
					}
					else if ((roll1 == 6) && (roll2 == 6))
					{
						win = TRUE;
						odds = choice + 1;
					}
					break;
				}

				if (win)
				{
					prt("YOU WON", 16, 37);
					p_ptr->au += odds * wager;
					sprintf(tmp_str, "Payoff: %d", odds);
					prt(tmp_str, 17, 37);
				}
				else
				{
					prt("You Lost", 16, 37);
					p_ptr->au -= wager;
					prt("", 17, 37);
				}
				sprintf(tmp_str, "Current Gold:     %9ld", p_ptr->au);
				prt(tmp_str, 22, 2);
				prt("Again(Y/N)?", 18, 37);
				move_cursor(18, 49);
				again = inkey();
				if (wager > p_ptr->au)
				{
					msg_print("Hey! You don't have the gold - get out of here!");
					msg_print(NULL);

					/* Get out here */
					break;
				}
			} while ((again == 'y') || (again == 'Y'));

			/* Switch back to complex RNG */
			Rand_quick = FALSE;

			prt("", 18, 37);
			if (p_ptr->au >= oldgold)
				msg_print("You came out a winner! We'll win next time, I'm sure.");
			else
				msg_print("You lost gold! Haha, better head home.");
		}
		msg_print(NULL);
	}
	screen_load();
	return (TRUE);
}


/*
 * Inn commands
 * Note that resting for the night was a perfect way to avoid player
 * ghosts in the town *if* you could only make it to the inn in time (-:
 * Now that the ghosts are temporarily disabled in 2.8.X, this function
 * will not be that useful.  I will keep it in the hopes the player
 * ghost code does become a reality again. Does help to avoid filthy urchins.
 * Resting at night is also a quick way to restock stores -KMW-
 */
static bool inn_comm(int cmd)
{
	s32b dawnval;

	switch (cmd)
	{
		case BACT_FOOD: /* Buy food & drink */
			msg_print("The barkeep gives you some gruel and a beer.");
			msg_print(NULL);
			(void)set_food(PY_FOOD_MAX - 1);
			break;

		case BACT_REST: /* Rest for the night */
			dawnval = ((turn % (10L * TOWN_DAWN)));
			if (dawnval > 50000)
			{  /* nighttime */
				if ((p_ptr->poisoned) || (p_ptr->cut))
				{
					msg_print("You need a healer, not a room.");
					msg_print(NULL);
					msg_print("Sorry, but don't want anyone dying in here.");
					return (FALSE);
				}
				else
				{
					turn = ((turn / 50000) + 1) * 50000;
					p_ptr->chp = p_ptr->mhp;

					if (ironman_nightmare)
					{
						msg_print("Horrible visions flit through your mind as you sleep.");

						/* Pick a nightmare */
						get_mon_num_prep(get_nightmare, NULL);

						/* Have some nightmares */
						while (1)
						{
							have_nightmare(get_mon_num(MAX_DEPTH));

							if (!one_in_(3)) break;
						}

						/* Remove the monster restriction */
						get_mon_num_prep(NULL, NULL);

						msg_print("You awake screaming.");
					}
					else
					{
						set_blind(0);
						set_confused(0);
						p_ptr->stun = 0;
						p_ptr->chp = p_ptr->mhp;
						p_ptr->csp = p_ptr->msp;

						msg_print("You awake refreshed for the new day.");
					}

					msg_print(NULL);
				}
			}
			else
			{
				msg_print("The rooms are available only at night.");
				msg_print(NULL);
				return (FALSE);
			}
			break;
		case BACT_RUMORS: /* Listen for rumors */
			{
				char Rumor[1024];

				if (!get_rnd_line("rumors.txt", 0, Rumor))
					msg_format("%s", Rumor);
				msg_print(NULL);
				break;
			}
	}

	return (TRUE);
}


/*
 * Share gold for thieves
 */
static void share_gold(void)
{
	int i = (p_ptr->lev * 2) * 10;
	msg_format("You collect %d gold pieces", i);
	msg_print(NULL);
	p_ptr->au += i;
}


/*
 * Display quest information
 */
static void get_questinfo(int questnum)
{
	int     i;
	int     old_quest;
	char    tmp_str[80];


	/* Clear the text */
	for (i = 0; i < 10; i++)
	{
		quest_text[i][0] = '\0';
	}

	quest_text_line = 0;

	/* Set the quest number temporary */
	old_quest = p_ptr->inside_quest;
	p_ptr->inside_quest = questnum;

	/* Get the quest text */
	init_flags = INIT_SHOW_TEXT | INIT_ASSIGN;
	process_dungeon_file("q_info.txt", 0, 0, 0, 0);

	/* Reset the old quest number */
	p_ptr->inside_quest = old_quest;

	/* Print the quest info */
	sprintf(tmp_str, "Quest Information (Danger level: %d)", quest[questnum].level);
	prt(tmp_str, 5, 0);

	prt(quest[questnum].name, 7, 0);

	for (i = 0; i < 10; i++)
	{
		c_put_str(TERM_YELLOW, quest_text[i], i + 8, 0);
	}
}


/*
 * Request a quest from the Lord.
 */
static void castle_quest(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int             q_index = 0;
	monster_race    *r_ptr;
	quest_type      *q_ptr;
	cptr            name;


	clear_bldg(7, 18);

	/* Current quest of the building */
	q_index = cave[py][px].special;

	/* Is there a quest available at the building? */
	if (!q_index)
	{
		put_str("I don't have a quest for you at the moment.", 8, 0);
		return;
	}

	q_ptr = &quest[q_index];

	/* Quest is completed */
	if (q_ptr->status == QUEST_STATUS_COMPLETED)
	{
		/* Rewarded quest */
		q_ptr->status = QUEST_STATUS_REWARDED;

		get_questinfo(q_index);
	}
	/* Failed quest */
	else if (q_ptr->status == QUEST_STATUS_FAILED)
	{
		get_questinfo(q_index);

		/* Mark quest as done (but failed) */
		q_ptr->status = QUEST_STATUS_FAILED_DONE;
	}
	/* Quest is still unfinished */
	else if (q_ptr->status == QUEST_STATUS_TAKEN)
	{
		put_str("You have not completed your current quest yet!", 8, 0);
		put_str("Use CTRL-Q to check the status of your quest.", 9, 0);
		put_str("Return when you have completed your quest.", 12, 0);
	}
	/* No quest yet */
	else if (q_ptr->status == QUEST_STATUS_UNTAKEN)
	{
		q_ptr->status = QUEST_STATUS_TAKEN;

		/* Assign a new quest */
		if (q_ptr->type == QUEST_TYPE_KILL_ANY_LEVEL)
		{
			if (q_ptr->r_idx == 0)
			{
				/* Random monster at least 5 - 10 levels out of deep */
				q_ptr->r_idx = get_mon_num(q_ptr->level + 4 + randint1(6));
			}

			r_ptr = &r_info[q_ptr->r_idx];

			while ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->rarity != 1))
			{
				q_ptr->r_idx = get_mon_num(q_ptr->level) + 4 + randint1(6);
				r_ptr = &r_info[q_ptr->r_idx];
			}

			if (q_ptr->max_num == 0)
			{
				/* Random monster number */
				if (randint1(10) > 7)
					q_ptr->max_num = 1;
				else
					q_ptr->max_num = randint1(3) + 1;
			}

			q_ptr->cur_num = 0;
			name = (r_name + r_ptr->name);
			msg_format("Your quest: kill %d %s", q_ptr->max_num, name);
			msg_print(NULL);
		}
		else
		{
			get_questinfo(q_index);
		}
	}
}


/*
 * Display town history
 */
static void town_history(void)
{
	/* Save screen */
	screen_save();

	/* Peruse the building help file */
	(void)show_file("bldg.txt", NULL, 0, 0);

	/* Load screen */
	screen_load();
}


#endif /* 0 */

/*
 * Display the damage figure of an object
 * (used by compare_weapon_aux1)
 *
 * Only accurate for the current weapon, because it includes
 * the current +dam of the player.
 */
static void compare_weapon_aux2(object_type *o_ptr, int numblows,
	 int r, int c, cptr attr, byte color, byte slay)
{
	char tmp_str[80];
	long maxdam, mindam;
	int dambonus;

	int intmaxdam, intmindam;

	dambonus = o_ptr->to_d + p_ptr->to_d;

	if (dambonus > 0)
		mindam = (100 + deadliness_conversion[dambonus]);
	else if (dambonus > -31)
		mindam = (100 - deadliness_conversion[ABS(dambonus)]);
	else
		mindam = 0;
	
	/* Include effects of slaying bonus */
	mindam = (mindam * slay) / 10;

	/* Effect of damage dice */
	maxdam = mindam * (o_ptr->ds * o_ptr->dd);
	mindam *= o_ptr->dd;

	/* number of blows */
	maxdam *= numblows;
	mindam *= numblows;

	/* rescale */
	intmaxdam = maxdam / 100;
	intmindam = mindam / 100;

	/* Print the intro text */
	c_put_str(color, attr, r, c);

	/* Calculate the min and max damage figures */
	sprintf(tmp_str, "Attack: %d-%d damage", intmindam, intmaxdam);

	/* Print the damage */
	put_str(tmp_str, r, c + 8);
}


/*
 * Show the damage figures for the various monster types
 *
 * Only accurate for the current weapon, because it includes
 * the current number of blows for the player.
 */
static void compare_weapon_aux1(object_type *o_ptr, int col, int r)
{
	u32b f1, f2, f3;

	/* Get the flags of the weapon */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Print the relevant lines */
	if (f1 & TR1_SLAY_ANIMAL) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, "Animals:", TERM_YELLOW, 17);
	if (f1 & TR1_SLAY_EVIL)   compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, "Evil:", TERM_YELLOW, 15);
	if (f1 & TR1_SLAY_UNDEAD) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, "Undead:", TERM_YELLOW, 20);
	if (f1 & TR1_SLAY_DEMON)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, "Demons:", TERM_YELLOW, 20);
	if (f1 & TR1_SLAY_ORC)    compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, "Orcs:", TERM_YELLOW, 20);
	if (f1 & TR1_SLAY_TROLL)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, "Trolls:", TERM_YELLOW, 20);
	if (f1 & TR1_SLAY_GIANT)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, "Giants:", TERM_YELLOW, 20);
	if (f1 & TR1_SLAY_DRAGON) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, "Dragons:", TERM_YELLOW, 20);
	if (f1 & TR1_KILL_DRAGON) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, "Dragons:", TERM_YELLOW, 30);
	if (f1 & TR1_BRAND_ACID)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, "Acid:", TERM_RED, 20);
	if (f1 & TR1_BRAND_ELEC)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, "Elec:", TERM_RED, 20);
	if (f1 & TR1_BRAND_FIRE)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, "Fire:", TERM_RED, 20);
	if (f1 & TR1_BRAND_COLD)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, "Cold:", TERM_RED, 20);
	if (f1 & TR1_BRAND_POIS)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, "Poison:", TERM_RED, 20);
}


/*
 * Displays all info about a weapon
 *
 * Only accurate for the current weapon, because it includes
 * various info about the player's +to_dam and number of blows.
 */
static void list_weapon(object_type *o_ptr, int row, int col)
{
	char o_name[80];
	char tmp_str[80];

	long maxdam, mindam;
	int dambonus;

	int intmaxdam, intmindam;

	/* Print the weapon name */
	object_desc(o_name, o_ptr, TRUE, 0);
	c_put_str(TERM_YELLOW, o_name, row, col);

	/* Print to_hit and to_dam of the weapon */
	sprintf(tmp_str, "To Hit: %d  Deadliness: %d", o_ptr->to_h, o_ptr->to_d);
	put_str(tmp_str, row+1, col);

	/* Print the weapons base damage dice */
	sprintf(tmp_str, "Dice: %d   Sides: %d", o_ptr->dd, o_ptr->ds);
	put_str(tmp_str, row+2, col);

	/* Print the player's number of blows */
	sprintf(tmp_str, "Number of Blows: %d", p_ptr->num_blow);
	put_str(tmp_str, row+3, col);

	c_put_str(TERM_YELLOW, "Possible Damage:", row+5, col);

	dambonus = o_ptr->to_d + p_ptr->to_d;

	if (dambonus > 0)
		mindam = (100 + deadliness_conversion[dambonus]);
	else if (dambonus > -31)
		mindam = (100 - deadliness_conversion[ABS(dambonus)]);
	else
		mindam = 0;

	/* Effect of damage dice */
	maxdam = mindam * (o_ptr->ds * o_ptr->dd);
	mindam *= o_ptr->ds;

	/* rescale */
	intmaxdam = maxdam / 100;
	intmindam = mindam / 100;

	/* Damage for one blow (if it hits) */
	sprintf(tmp_str, "One Strike: %d-%d damage", intmindam, intmaxdam);
	put_str(tmp_str, row+6, col+1);

	/* rescale */
	intmaxdam = (maxdam * p_ptr->num_blow) / 100;
	intmindam = (mindam * p_ptr->num_blow) / 100;

	/* Damage for the complete attack (if all blows hit) */
	sprintf(tmp_str, "One Attack: %d-%d damage", intmindam, intmaxdam);
	put_str(tmp_str, row+7, col+1);
}


/*
 * Hook to specify "weapon"
 */
static bool item_tester_hook_melee_weapon(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

#if 0 

/*
 * Hook to specify "ammo"
 */
static bool item_tester_hook_ammo(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

#endif /* 0 */

/*
 * Compare weapons
 *
 * Copies the weapons to compare into the weapon-slot and
 * compares the values for both weapons.
 */
static void compare_weapons(void)
{
	int item, item2;
	object_type *o1_ptr, *o2_ptr;
	object_type orig_weapon;
	object_type *i_ptr;
	cptr q, s;
	int row = 6;

	/* Clear the screen */
	clear_bldg(6, 18);

	/* Store copy of original wielded weapon */
	i_ptr = &inventory[INVEN_WIELD];
	object_copy(&orig_weapon, i_ptr);

	/* Only compare melee weapons */
	item_tester_hook = item_tester_hook_melee_weapon;

	/* Get the first weapon */
	q = "What is your first weapon? ";
	s = "You have nothing to compare.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return;

	/* Get the item (in the pack) */
	o1_ptr = &inventory[item];

	/* Only compare melee weapons */
	item_tester_hook = item_tester_hook_melee_weapon;

	/* Get the second weapon */
	q = "What is your second weapon? ";
	s = "You have nothing to compare.";
	if (!get_item(&item2, q, s, (USE_EQUIP | USE_INVEN))) return;

	/* Get the item (in the pack) */
	o2_ptr = &inventory[item2];

	put_str("Based on your current abilities, here is what your weapons will do", 4, 2);

	/* Copy first weapon into the weapon slot (if it's not already there) */
	if (o1_ptr != i_ptr)
		object_copy(i_ptr, o1_ptr);

	/* Get the new values */
	calc_bonuses();

	/* List the new values */
	list_weapon(o1_ptr, row, 2);
	compare_weapon_aux1(o1_ptr, 2, row + 8);

	/* Copy second weapon into the weapon slot (if it's not already there) */
	if (o2_ptr != i_ptr)
		object_copy(i_ptr, o2_ptr);
	else
		object_copy(i_ptr, &orig_weapon);

	/* Get the new values */
	calc_bonuses();

	/* List the new values */
	list_weapon(o2_ptr, row, 40);
	compare_weapon_aux1(o2_ptr, 40, row + 8);

	/* Copy back the original weapon into the weapon slot */
	object_copy(i_ptr, &orig_weapon);

	/* Reset the values for the old weapon */
	calc_bonuses();

	put_str("(Only highest damage applies per monster. Special damage not cumulative.)", 20, 0);

	/* Done */
	return;
}



/*
 * Enchant item
 */
static bool enchant_item(int cost, int to_hit, int to_dam, int to_ac)
{
	int         i, item;
	bool        okay = FALSE;
	object_type *o_ptr;
	cptr        q, s;
	int         maxenchant = (p_ptr->lev / 5);
	char        tmp_str[80];


	clear_bldg(5, 18);
	prt(format("  Based on your skill, we can improve up to +%d.", maxenchant), 5, 0);
	prt(format("  The price for the service is %d gold per item.", cost), 7, 0);

	/* Get an item */
	q = "Improve which item? ";
	s = "You have nothing to improve.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return (FALSE);

	/* Get the item (in the pack) */
	o_ptr = &inventory[item];

	/* Check if the player has enough money */
	if (p_ptr->au < (cost * o_ptr->number))
	{
		object_desc(tmp_str, o_ptr, TRUE, 0);
		msg_format("You do not have the gold to improve %s!", tmp_str);
		msg_print(NULL);
		return (FALSE);
	}

	/* Enchant to hit */
	for (i = 0; i < to_hit; i++)
	{
		if (o_ptr->to_h < maxenchant)
		{
			if (enchant(o_ptr, 1, (ENCH_TOHIT | ENCH_FORCE)))
			{
				okay = TRUE;
				break;
			}
		}
	}

	/* Enchant to damage */
	for (i = 0; i < to_dam; i++)
	{
		if (o_ptr->to_d < maxenchant)
		{
			if (enchant(o_ptr, 1, (ENCH_TODAM | ENCH_FORCE)))
			{
				okay = TRUE;
				break;
			}
		}
	}

	/* Enchant to AC */
	for (i = 0; i < to_ac; i++)
	{
		if (o_ptr->to_a < maxenchant)
		{
			if (enchant(o_ptr, 1, (ENCH_TOAC | ENCH_FORCE)))
			{
				okay = TRUE;
				break;
			}
		}
	}

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message */
		msg_print("The improvement failed.");

		return (FALSE);
	}
	else
	{
		object_desc(tmp_str, o_ptr, TRUE, 1);
		msg_format("Improved %s for %d gold.", tmp_str, cost * o_ptr->number);
		msg_print(NULL);

		/* Charge the money */
		p_ptr->au -= (cost * o_ptr->number);

		/* Something happened */
		return (TRUE);
	}
}



/*
 * Recharge rods, wands and staves
 *
 * The player can select the number of charges to add
 * (up to a limit), and the recharge never fails.
 *
 * The cost for rods depends on the level of the rod. The prices
 * for recharging wands and staves are dependent on the cost of
 * the base-item.
 */
static void building_recharge(long cost)
{
	int         item, lev;
	object_type *o_ptr;
	object_kind *k_ptr;
	cptr        q, s;
	int         price;
	int         charges;
	int         max_charges;
	char        tmp_str[80];


	/* Display some info */
	clear_bldg(5, 18);
	prt("  The prices of recharge depend on the type.", 6, 0);

	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item */
	q = "Recharge which item? ";
	s = "You have nothing to recharge.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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

	k_ptr = &k_info[o_ptr->k_idx];

	/*
	 * We don't want to give the player free info about
	 * the level of the item or the number of charges.
	 */
	/* The item must be "known" */
	if (!object_known_p(o_ptr))
	{
		msg_format("The item must be identified first!");
		msg_print(NULL);

		if ((p_ptr->au >= 50) &&
			get_check("Identify for 50 gold? "))
		{
			/* Pay the price */
			p_ptr->au -= 50;

			/* Identify it */
			identify_item(o_ptr);

			/* Description */
			object_desc(tmp_str, o_ptr, TRUE, 3);

			msg_format("You have: %s.", tmp_str);
		}
		else
		{
			return;
		}
	}

	/* Extract the object "level" */
	lev = get_object_level(o_ptr);

	/* Price for a rod */
	if (o_ptr->tval == TV_ROD)
	{
		if (o_ptr->timeout > 0)
		{
			/* Fully recharge */
			price = (lev * o_ptr->timeout) / k_ptr->pval;
		}
		else
		{
			/* No recharge necessary */
			msg_format("That doesn't need to be recharged.");
			msg_print(NULL);
			return;
		}
	}
	else if (o_ptr->tval == TV_STAFF)
	{
		/*
		 * Price per charge ( = double the price paid 
		 * by shopkeepers for the charge)
		 */
		price = (o_ptr->cost / 10) * o_ptr->number;

		/* Pay at least 10 gold per charge */
		price = MAX(10, price);
	}
	else
	{
		/*
		 * Price per charge ( = double the price paid
		 * by shopkeepers for the charge)
		 */
		price = (o_ptr->cost / 10);

		/* Pay at least 10 gold per charge */
		price = MAX(10, price);
	}

	/* Limit the number of charges for wands and staves */
	if (((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF)) &&
	     (o_ptr->pval / o_ptr->number >= k_ptr->pval))
	{
		if ((o_ptr->tval == TV_WAND) && (o_ptr->number == 1))
			msg_print("This wand is already fully charged.");
		else if ((o_ptr->tval == TV_WAND) && (o_ptr->number > 1))
			msg_print("These wands are already fully charged.");
		else if ((o_ptr->tval == TV_STAFF) && (o_ptr->number == 1))
			msg_print("This staff is already fully charged.");
		else if ((o_ptr->tval == TV_STAFF) && (o_ptr->number > 1))
			msg_print("These staffs are already fully charged.");

		msg_print(NULL);
		return;
	}

	/* Factor in shopkeeper greed */
	price = (price * cost / 100);
	
	/* Check if the player has enough money */
	if (p_ptr->au < price)
	{
		object_desc(tmp_str, o_ptr, TRUE, 0);
		msg_format("You need %d gold to recharge %s!", price, tmp_str);
		msg_print(NULL);
		return;
	}

	if (o_ptr->tval == TV_ROD)
	{
		if (get_check(format("Recharge the %s for %d gold? ",
			((o_ptr->number > 1) ? "rods" : "rod"), price)))
		{
			/* Recharge fully */
			o_ptr->timeout = 0;
		}
		else
		{
			return;
		}
	}
	else
	{
		if (o_ptr->tval == TV_STAFF)
			max_charges = k_ptr->pval - o_ptr->pval;
		else
			max_charges = o_ptr->number * k_ptr->pval - o_ptr->pval;

		/* Get the quantity for staves and wands */
		charges = get_quantity(format("Add how many charges for %d gold? ",
		              price), MIN(p_ptr->au / price, max_charges));

		/* Do nothing */
		if (charges < 1) return;

		/* Get the new price */
		price *= charges;

		/* Recharge */
		o_ptr->pval += charges;
		
		/* Hack - no "used" charges */
		o_ptr->ac = 0;

		/* We no longer think the item is empty */
		o_ptr->ident &= ~(IDENT_EMPTY);
	}

	/* Give feedback */
	object_desc(tmp_str, o_ptr, TRUE, 3);
	msg_format("%^s %s recharged for %d gold.", tmp_str, ((o_ptr->number > 1) ? "were" : "was"), price);
	msg_print(NULL);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);

	/* Pay the price */
	p_ptr->au -= price;

	/* Finished */
	return;
}


#if 0
/*
 * Execute a building command
 */
static void bldg_process_command(building_type *bldg, int i)
{
	int bact = bldg->actions[i];
	int bcost;
	bool paid = FALSE;
	int amt;

	if (is_owner(bldg))
		bcost = bldg->member_costs[i];
	else
		bcost = bldg->other_costs[i];

	/* action restrictions */
	if (((bldg->action_restr[i] == 1) && !is_member(bldg)) ||
	    ((bldg->action_restr[i] == 2) && !is_owner(bldg)))
	{
		msg_print("You have no right to choose that!");
		msg_print(NULL);
		return;
	}

	/* check gold (HACK - Recharge uses variable costs) */
	if ((bact != BACT_RECHARGE) &&
	    (((bldg->member_costs[i] > p_ptr->au) && is_owner(bldg)) ||
	     ((bldg->other_costs[i] > p_ptr->au) && !is_owner(bldg))))
	{
		msg_print("You do not have the gold!");
		msg_print(NULL);
		return;
	}


	switch (bact)
	{
		case BACT_NOTHING:
			/* Do nothing */
			break;
		case BACT_RESEARCH_ITEM:
			paid = identify_fully();
			break;
		case BACT_TOWN_HISTORY:
			town_history();
			break;
		case BACT_RACE_LEGENDS:
			race_legends();
			break;
#if 0
		case BACT_QUEST:
			castle_quest();
			break;
#endif
		case BACT_KING_LEGENDS:
		case BACT_ARENA_LEGENDS:
		case BACT_LEGENDS:
			show_highclass();
			break;
		case BACT_POSTER:
		case BACT_ARENA_RULES:
		case BACT_ARENA:
			arena_comm(bact);
			break;
		case BACT_IN_BETWEEN:
		case BACT_CRAPS:
		case BACT_SPIN_WHEEL:
		case BACT_DICE_SLOTS:
		case BACT_GAMBLE_RULES:
			gamble_comm(bact);
			break;
		case BACT_REST:
		case BACT_RUMORS:
		case BACT_FOOD:
			paid = inn_comm(bact);
			break;
		case BACT_RESEARCH_MONSTER:
			paid = research_mon();
			break;
		case BACT_ENCHANT_WEAPON:
			item_tester_hook = item_tester_hook_melee_weapon;
			enchant_item(bcost, 1, 1, 0);
			break;
		case BACT_ENCHANT_ARMOR:
			item_tester_hook = item_tester_hook_armour;
			enchant_item(bcost, 0, 0, 1);
			break;
		case BACT_RECHARGE:
			building_recharge();
			break;
		case BACT_IDENTS: /* needs work */
			identify_pack();

			/* Combine / Reorder the pack (later) */
			p_ptr->notice |= (PN_COMBINE | PN_REORDER);

			msg_print("Your posessions have been identified.");
			msg_print(NULL);
			paid = TRUE;
			break;
		case BACT_LEARN:
			do_cmd_study();
			break;
		case BACT_HEALING: /* needs work */
			hp_player(200);
			set_poisoned(0);
			set_blind(0);
			set_confused(0);
			set_cut(0);
			set_stun(0);
			paid = TRUE;
			break;
		case BACT_RESTORE: /* needs work */
			if (do_res_stat(A_STR)) paid = TRUE;
			if (do_res_stat(A_INT)) paid = TRUE;
			if (do_res_stat(A_WIS)) paid = TRUE;
			if (do_res_stat(A_DEX)) paid = TRUE;
			if (do_res_stat(A_CON)) paid = TRUE;
			if (do_res_stat(A_CHR)) paid = TRUE;
			break;
		case BACT_GOLD: /* set timed reward flag */
			if (!p_ptr->rewards[BACT_GOLD])
			{
				share_gold();
				p_ptr->rewards[BACT_GOLD] = TRUE;
			}
			else
			{
				msg_print("You just had your daily allowance!");
				msg_print(NULL);
			}
			break;
		case BACT_ENCHANT_ARROWS:
			item_tester_hook = item_tester_hook_ammo;
			enchant_item(bcost, 1, 1, 0);
			break;
		case BACT_ENCHANT_BOW:
			item_tester_tval = TV_BOW;
			enchant_item(bcost, 1, 1, 0);
			break;
		case BACT_RECALL:
			p_ptr->word_recall = 1;
			msg_print("The air about you becomes charged...");
			paid = TRUE;
			p_ptr->redraw |= (PR_STATUS);
			break;
		case BACT_TELEPORT_LEVEL:
			amt = get_quantity("Teleport to which level? ", 98);
			if (amt > 0)
			{
				p_ptr->word_recall = 1;
				p_ptr->max_dlv = amt;
				msg_print("The air about you becomes charged...");
				paid = TRUE;
				p_ptr->redraw |= (PR_STATUS);
			}
			break;
		case BACT_LOSE_MUTATION:
			paid = lose_mutation(0);
			/* ToDo: Better message text. */
			if (!paid)
				msg_print("You feel oddly normal.");

			break;
	}

	if (paid)
	{
		p_ptr->au -= bcost;
	}
}


/*
 * Enter quest level
 */
void do_cmd_quest(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	if (cave[py][px].feat != FEAT_QUEST_ENTER)
	{
		msg_print("You see no quest level here.");
		return;
	}
	else
	{
		/* Player enters a new quest */
		leaving_quest = p_ptr->inside_quest;

		/* Leaving an 'only once' quest marks it as failed */
		if (leaving_quest &&
			(quest[leaving_quest].flags & QUEST_FLAG_ONCE) &&
			(quest[leaving_quest].status == QUEST_STATUS_TAKEN))
		{
			quest[leaving_quest].status = QUEST_STATUS_FAILED;
		}

		p_ptr->inside_quest = cave[py][px].special;
		dun_level = 1;

		p_ptr->leaving = TRUE;
	}
}


#endif /* 0 */

/* Does the player have enough gold for this action? */
static bool test_gold(int *cost)
{
	if (p_ptr->au < *cost)
	{
		/* Player does not have enough gold */

		msg_format("You need %d gold to do this!", *cost);
		msg_print(NULL);
		
		*cost = 0;
		
		return (FALSE);
	
	}
	
	/* Player has enough gold */
	return (TRUE);
}

static bool process_build_hook(field_type *f_ptr, store_type *b_ptr)
{
	int		cost = 0;
	bool	done = FALSE;
	
	b_own_type *bo_ptr = &b_owners[f_ptr->data[0]][b_ptr->owner];
	
	switch (f_ptr->data[0])
	{
		case BLDG_WEAPONMASTER:
		{
			if (p_ptr->command_cmd == 'E')
			{
				cost = f_ptr->data[1] * bo_ptr->inflate;
				
				if (test_gold(&cost))
				{
					compare_weapons();
				}
				
				done = TRUE;
			}
			
			break;
		}
		
		case BLDG_RECHARGE:
		{
			if (p_ptr->command_cmd == 'R')
			{
				building_recharge(f_ptr->data[1] * bo_ptr->inflate);
				
				done = TRUE;
			}
			
			if (p_ptr->command_cmd == 'I')
			{
				cost = f_ptr->data[2] * bo_ptr->inflate;
				
				if (test_gold(&cost))
				{
					identify_pack();

					/* Combine / Reorder the pack (later) */
					p_ptr->notice |= (PN_COMBINE | PN_REORDER);

					msg_print("Your posessions have been identified.");
					msg_print(NULL);
				}
								
				done = TRUE;
			}

			break;	
		}
		
		case BLDG_PLUS_WEAPON:
		{
			if (p_ptr->command_cmd == 'E')
			{
				item_tester_hook = item_tester_hook_melee_weapon;
				
				enchant_item(f_ptr->data[1] * bo_ptr->inflate, 1, 1, 0);
				
				done = TRUE;
			}		
		
			break;
		}
		
		case BLDG_PLUS_ARMOUR:
		{
			if (p_ptr->command_cmd == 'E')
			{
				item_tester_hook = item_tester_hook_armour;
				
				enchant_item(f_ptr->data[1] * bo_ptr->inflate, 0, 0, 1);
				
				done = TRUE;
			}
		
			break;
		}
		
		case BLDG_MUTATE:
		{
			if (p_ptr->command_cmd == 'E')
			{
				cost = f_ptr->data[1] * bo_ptr->inflate * (count_mutations()+1);
				
				if (test_gold(&cost))
				{
					if (lose_mutation(0))
					{
						msg_print("You feel oddly normal.");
					}
					else
					{
						(void) gain_random_mutation(0);
					}
				}
				
				done = TRUE;
			}
			
			break;
		}
		
		case BLDG_MAP:
		{
			if (p_ptr->command_cmd == 'B')
			{
				cost = f_ptr->data[1] * bo_ptr->inflate;
				
				if (test_gold(&cost))
				{
					msg_print("You learn of the lay of the lands.");
					
					/* Map a radius-20 circle around the player */
					map_wilderness(20,
						 p_ptr->wilderness_x / 16, p_ptr->wilderness_y / 16);
				}
				
				done = TRUE;
			}
			
			break;
		}
	}
	
	/* Subtract off cost */
	p_ptr->au -= cost;
	
	/* Did we do anything? */
	return (done);
}


/*
 * Process a command in a building
 *
 * Note that we must disable some commands which are allowed
 * in the dungeon but not in the stores, to prevent chaos.
 */
static bool build_process_command(field_type *f_ptr, store_type *b_ptr)
{
	/* Handle repeating the last command */
	repeat_check();

	if (rogue_like_commands && p_ptr->command_cmd == 'l')
	{
		p_ptr->command_cmd = 'x';	/* hack! */
	}

	/* Process the building-specific commands */
	if (process_build_hook(f_ptr, b_ptr)) return(FALSE);
	
	/* Parse the command */
	switch (p_ptr->command_cmd)
	{
		/* Leave */
		case ESCAPE:
		{
			return (TRUE);
		}

		/* Redraw */
		case KTRL('R'):
		{
			do_cmd_redraw();
			display_build(f_ptr, b_ptr);
			break;
		}

		/* Ignore return */
		case '\r':
		{
			break;
		}

		/*** Inventory Commands ***/

		/* Wear/wield equipment */
		case 'w':
		{
			do_cmd_wield();
			break;
		}

		/* Take off equipment */
		case 't':
		{
			do_cmd_takeoff();
			break;
		}

		/* Destroy an item */
		case 'k':
		{
			do_cmd_destroy();
			break;
		}

		/* Equipment list */
		case 'e':
		{
			do_cmd_equip();
			break;
		}

		/* Inventory list */
		case 'i':
		{
			do_cmd_inven();
			break;
		}


		/*** Various commands ***/

		/* Identify an object */
		case 'I':
		{
			do_cmd_observe();
			break;
		}

		/* Hack -- toggle windows */
		case KTRL('I'):
		{
			toggle_inven_equip();
			break;
		}


		/*** Use various objects ***/

		/* Browse a book */
		case 'b':
		{
			do_cmd_browse();
			break;
		}

		/* Inscribe an object */
		case '{':
		{
			do_cmd_inscribe();
			break;
		}

		/* Uninscribe an object */
		case '}':
		{
			do_cmd_uninscribe();
			break;
		}



		/*** Help and Such ***/

		/* Help */
		case '?':
		{
			do_cmd_help();
			break;
		}

		/* Identify symbol */
		case '/':
		{
			do_cmd_query_symbol();
			break;
		}

		/* Character description */
		case 'C':
		{
			do_cmd_character();
			display_build(f_ptr, b_ptr);
			break;
		}


		/*** System Commands ***/

		/* Hack -- User interface */
		case '!':
		{
			(void)Term_user(0);
			break;
		}

		/* Single line from a pref file */
		case '"':
		{
			do_cmd_pref();
			break;
		}

		/* Interact with macros */
		case '@':
		{
			do_cmd_macros();
			break;
		}

		/* Interact with visuals */
		case '%':
		{
			do_cmd_visuals();
			break;
		}

		/* Interact with colors */
		case '&':
		{
			do_cmd_colors();
			break;
		}

		/* Interact with options */
		case '=':
		{
			do_cmd_options(OPT_FLAG_SERVER | OPT_FLAG_PLAYER);
			break;
		}

		/*** Misc Commands ***/

		/* Take notes */
		case ':':
		{
			do_cmd_note();
			break;
		}

		/* Version info */
		case 'V':
		{
			do_cmd_version();
			break;
		}

		/* Repeat level feeling */
		case KTRL('F'):
		{
			do_cmd_feeling();
			break;
		}

		/* Show previous message */
		case KTRL('O'):
		{
			do_cmd_message_one();
			break;
		}

		/* Show previous messages */
		case KTRL('P'):
		{
			do_cmd_messages();
			break;
		}

		/* Check artifacts, uniques etc. */
		case '~':
		case '|':
		{
			do_cmd_knowledge();
			break;
		}

		/* Load "screen dump" */
		case '(':
		{
			do_cmd_load_screen();
			break;
		}

		/* Save "screen dump" */
		case ')':
		{
			do_cmd_save_screen();
			break;
		}

		/* Hack -- Unknown command */
		default:
		{
			msg_print("That command does not work in buildings.");
			break;
		}
	}
	
	return (FALSE);
}


/*
 * Do building commands
 */
void do_cmd_bldg(field_type *f_ptr)
{
	int		i, which = -1;
	store_type   *b_ptr;
	bool	leave_build = FALSE;

	town_type	*twn_ptr = &town[p_ptr->town_num];
	
	/* Get the building the player is on */
	for (i = 0; i < twn_ptr->numstores; i++)
	{
		if ((p_ptr->py - twn_ptr->y * 16 == twn_ptr->store[i].y) && 
		 (p_ptr->px - twn_ptr->x * 16 == twn_ptr->store[i].x))
		{
			which = i;
		}
	}
	
	/* Paranoia */
	if (which == -1)
	{
		msg_print("Could not locate building!");
		return;
	}


	b_ptr = &twn_ptr->store[which];

	/* Forget the view */
	forget_view();

	/* Hack -- Increase "icky" depth */
	character_icky++;
	
	/* No command argument */
	p_ptr->command_arg = 0;

	/* No repeated command */
	p_ptr->command_rep = 0;

	/* No automatic command */
	p_ptr->command_new = 0;

	/* Display the building */
	display_build(f_ptr, b_ptr);

	/* Interact with player */
	while (!leave_build)
	{
		prt("", 1, 0);

		/* Clear */
		clear_from(21);

		/* Basic commands */
		prt(" ESC) Exit from Building.", 22, 0);
		
		/* Get a command */
		request_command(FALSE);

		/* Process the command */
		leave_build = build_process_command(f_ptr, b_ptr);

		/* Hack -- Character is still in "icky" mode */
		character_icky = TRUE;

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();
	}

	/* Free turn XXX XXX XXX */
	p_ptr->energy_use = 0;

	/* Hack -- Character is no longer in "icky" mode */
	character_icky = FALSE;


	/* Hack -- Cancel automatic command */
	p_ptr->command_new = 0;

	/* Hack -- Cancel "see" mode */
	p_ptr->command_see = FALSE;


	/* Flush messages XXX XXX XXX */
	msg_print(NULL);


	/* Clear the screen */
	Term_clear();


	/* Update everything */
	p_ptr->update |= (PU_VIEW);
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw entire screen */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_EQUIPPY);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
}

/*
 * Initialize a building
 */
void build_init(int town_num, int build_num, byte build_type)
{
	/* Activate that building */
	store_type *st_ptr = &town[town_num].store[build_num];

	/* Pick an owner */
	st_ptr->owner = (byte)randint0(MAX_B_OWN);

	/* Set the type */
	st_ptr->type = build_type;

	/* Initialize */
	st_ptr->store_open = 0;
	st_ptr->insult_cur = 0;
	st_ptr->good_buy = 0;
	st_ptr->bad_buy = 0;
	st_ptr->stock_num = 0;
	st_ptr->last_visit = 0;
}
