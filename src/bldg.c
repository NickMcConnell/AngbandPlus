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


/* Hack - force exit from building */
static bool force_build_exit = FALSE;


static void have_nightmare_aux(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	char m_name[80];
	bool happened = FALSE;
	int power = r_ptr->hdice * 2 + 10;
	cptr desc = mon_race_name(r_ptr);

	int i;

	if (!FLAG(r_ptr, RF_UNIQUE))
	{
		/* Describe it */
		strnfmt(m_name, 80, "%s %s", (is_a_vowel(desc[0]) ? "an" : "a"), desc);

		if (FLAG(r_ptr, RF_FRIENDS))
		{
			power -= 50;
		}
	}
	else
	{
		/* Describe it */
		strnfmt(m_name, 80, "%s", desc);

		power += 50;
	}

	if (player_save(power))
	{
		msgf("%^s chases you through your dreams.", m_name);

		/* Safe */
		return;
	}

	if (query_timed(TIMED_IMAGE))
	{
		/* Something silly happens... */
		msgf("You behold the %s visage of %s!",
				   funny_desc[randint0(MAX_SAN_FUNNY)], m_name);

		if (one_in_(3))
		{
			msgf(funny_comments[randint0(MAX_SAN_COMMENT)]);
			(void) inc_image(randint1(r_ptr->hdice * 2));
		}

		/* Never mind; we can't see it clearly enough */
		return;
	}

	/* Something frightening happens... */
	msgf("You behold the %s visage of %s!",
			   horror_desc[randint0(MAX_SAN_HORROR)], desc);

	r_ptr->r_flags[3] |= RF3_ELDRITCH_HORROR;

	switch (p_ptr->rp.prace)
	{
		case RACE_IMP:
		{
			/* Imps may make a saving throw */
			if (saving_throw(20 + p_ptr->lev)) return;

			break;
		}

		case RACE_SKELETON:
		case RACE_ZOMBIE:
		case RACE_SPECTRE:
		case RACE_VAMPIRE:
		case RACE_GHOUL:
		{
			/* Undead may make a saving throw */
			if (saving_throw(10 + p_ptr->lev)) return;

			break;
		}
	}

	/* Mind blast */
	if (!player_save(power))
	{
		if (!(FLAG(p_ptr, TR_RES_CONF)) && !query_timed(TIMED_OPPOSE_CONF))
		{
			(void)inc_confused(rand_range(4, 8));
		}
		if (!(FLAG(p_ptr, TR_RES_CHAOS)) && one_in_(3) && !query_timed(TIMED_OPPOSE_CONF))
		{
			(void)inc_image(rand_range(250, 400));
		}
		return;
	}

	/* Lose int & wis */
	if (!player_save(power))
	{
		(void)do_dec_stat(A_INT);
		(void)do_dec_stat(A_WIS);
		return;
	}

	/* Brain smash */
	if (!player_save(power))
	{
		if (!(FLAG(p_ptr, TR_RES_CONF)) && !query_timed(TIMED_OPPOSE_CONF))
		{
			(void)inc_confused(rand_range(4, 8));
		}
		if (!(FLAG(p_ptr, TR_FREE_ACT)))
		{
			(void)inc_paralyzed(rand_range(4, 8));
		}

		for (i = 0; !player_save(power - i); i += 10)
		{
			(void)do_dec_stat(A_INT);
			(void)do_dec_stat(A_WIS);
		}

		if (!(FLAG(p_ptr, TR_RES_CHAOS))  && !query_timed(TIMED_OPPOSE_CONF))
		{
			(void)inc_image(rand_range(250, 400));
		}
		return;
	}

	/* Permanent lose int & wis */
	if (!player_save(power))
	{
		if (dec_stat(A_INT, 10, TRUE)) happened = TRUE;
		if (dec_stat(A_WIS, 10, TRUE)) happened = TRUE;
		if (happened)
		{
			msgf("You feel much less sane than before.");
		}
		return;
	}

	/* Amnesia */
	if (!player_save(power))
	{
		if (lose_all_info())
		{
			msgf("You forget everything in your utmost terror!");
		}
		return;
	}

	/* Else gain permanent insanity */
	if ((p_ptr->muta3 & MUT3_MORONIC) && (p_ptr->muta2 & MUT2_BERS_RAGE) &&
		((p_ptr->muta2 & MUT2_COWARDICE) || (FLAG(p_ptr, TR_RES_FEAR))) &&
		((p_ptr->muta2 & MUT2_HALLU) || (FLAG(p_ptr, TR_RES_CHAOS))))
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
					msgf("You turn into an utter moron!");
					if (p_ptr->muta3 & MUT3_HYPER_INT)
					{
						msgf("Your brain is no longer a living computer.");
						p_ptr->muta3 &= ~(MUT3_HYPER_INT);
					}
					p_ptr->muta3 |= MUT3_MORONIC;
					happened = TRUE;
				}
				break;
			}
			case 2:
			{
				if (!(p_ptr->muta2 & MUT2_COWARDICE) &&
					!(FLAG(p_ptr, TR_RES_FEAR)))
				{
					msgf("You become paranoid!");

					/* Duh, the following should never happen, but anyway... */
					if (p_ptr->muta3 & MUT3_FEARLESS)
					{
						msgf("You are no longer fearless.");
						p_ptr->muta3 &= ~(MUT3_FEARLESS);
					}

					p_ptr->muta2 |= MUT2_COWARDICE;
					happened = TRUE;
				}
				break;
			}
			case 3:
			{
				if (!(p_ptr->muta2 & MUT2_HALLU) &&
					!(FLAG(p_ptr, TR_RES_CHAOS)))
				{
					msgf("You are afflicted by a hallucinatory insanity!");
					p_ptr->muta2 |= MUT2_HALLU;
					happened = TRUE;
				}
				break;
			}
			default:
			{
				if (!(p_ptr->muta2 & MUT2_BERS_RAGE))
				{
					msgf("You become subject to fits of berserk rage!");
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


/*
 * Wrapper function around the nightmare-making routine
 * so we make sure the monster summon list is restored.
 */
void have_nightmare(void)
{
	/* Get a monster */
	int r_idx = get_filter_mon_num(MAX_DEPTH, get_nightmare);

	/* Have some nightmares */
	have_nightmare_aux(r_idx);
}

bool get_nightmare(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Require eldritch horrors */
	if (!FLAG(r_ptr, RF_ELDRITCH_HORROR)) return (FALSE);

	/* Require high level */
	if (r_ptr->hdice * 2 <= p_ptr->lev) return (FALSE);

	/* Accept this monster */
	return (TRUE);
}


static void building_prt_gold(void)
{
	prtf(40, 23, "Gold Remaining: %9ld", (long)p_ptr->au);
}

/* Does the player have enough gold for this action? */
bool test_gold(s32b cost)
{
	if (p_ptr->au < cost)
	{
		/* Player does not have enough gold */

		msgf("You need %ld gold to do this!", (long)cost);
		message_flush();

		return (FALSE);
	}

	/* Player has enough gold */
	return (TRUE);
}


static const store_type *build_ptr;


/* Does this building have an outstanding quest */
bool build_has_quest(void)
{
	if (lookup_quest_building(build_ptr)) return (TRUE);

	return (FALSE);
}

bool build_next_quest(void)
{
	if (lookup_quest_building_next(build_ptr) != -1) return (TRUE);

	return (FALSE);
}


void build_cmd_quest(int level, field_type * f_ptr)
{
	quest_type *q_ptr = lookup_quest_building(build_ptr);

	(void)f_ptr;

	/* Do we already have a quest? */
	if (q_ptr)
	{
		/* Give reward? */
		reward_quest(q_ptr);
	}
	else
	{
		/* Make a new quest */
		request_quest(build_ptr, level);
	}

	/* Display messages */
	message_flush();
}


/* Global that contains which building the player is in */
static field_type resize_f_ptr;

/*
 * Display a building.
 */
void display_build(const field_type *f_ptr)
{
	int factor;

	cptr build_name = field_name(f_ptr);
	cptr owner_name = quark_str(build_ptr->owner_name);

	/* remember for the resize */
	resize_f_ptr = *f_ptr;

	/* The charisma factor */
	factor = adj_chr_gold[p_ptr->stat[A_CHR].ind];

	factor = ((factor + 200) * build_ptr->greed) / 400;

	Term_clear();
	prtf(1, 2, "%s %s", owner_name, build_name);
	prtf(0, 19, "You may:");

	/* Display building-specific information */
	(void) field_script_const(f_ptr, FIELD_ACT_BUILD_ACT1, "i", LUA_VAR(factor));

	prtf(0, 23, " ESC) Exit building");

	/* Show your gold */
	building_prt_gold();
}


/* Display the building without argument, relies on display_build going first */
static void resize_build(void)
{
	/* Get the argument from the global */
	display_build(&resize_f_ptr);
}

/*
 * display fruit for dice slots
 */
static void display_fruit(int col, int row, int fruit)
{
	switch (fruit)
	{
		case 0:
		{
			/* lemon */
			put_fstr(col, row, CLR_YELLOW
							"   ####.\n"
							"  #    #\n"
							" #     #\n"
							"#      #\n"
							"#      #\n"
							"#     # \n"
							"#    #  \n"
							".####   \n"
				CLR_WHITE	" Lemon  ");
			break;
		}
		case 1:
		{
			/* orange */
			put_fstr(col, row, CLR_ORANGE
							"   ##   \n"
							"  #..#  \n"
							" #....# \n"
							"#......#\n"
							"#......#\n"
							" #....# \n"
							"  #..#  \n"
							"	##   \n"
				CLR_WHITE	" Orange ");
			break;
		}
		case 2:
		{
			/* sword */
			put_fstr(col, row, CLR_SLATE
							"   /\\   \n"
							"	##   \n"
							"	##   \n"
							"	##   \n"
							"	##   \n"
							"	##   \n"
				CLR_UMBER	" ###### \n"
				CLR_UMBER	"   ##   \n"
				CLR_WHITE	" Sword  ");
			break;
		}
		case 3:
		{
			/* shield */
			put_fstr(col, row, CLR_SLATE
							" ###### \n"
							"#  	#\n"
							"# ++++ #\n"
							"# +==+ #\n"
							"#  ++  #\n"
							" #    # \n"
							"  #  #  \n"
							"	##   \n"
				CLR_WHITE	" Shield ");
			break;
		}
		case 4:
		{
			/* plum */
			put_fstr(col, row, CLR_VIOLET
							"   ##   \n"
							" ###### \n"
							"########\n"
							"########\n"
							"########\n"
							" ###### \n"
							"  ####  \n"
							"	##   \n"
				CLR_WHITE	"  Plum  ");
			break;
		}
		case 5:
		{
			/* cherry */
			put_fstr(col, row, CLR_GREEN
            				"      ##\n"
					CLR_RED	"   ##" CLR_GREEN "#  \n"
					CLR_RED "  #..#  \n"
							"  #..#  \n"
							" ###### \n"
							"#..##..#\n"
							"#..##..#\n"
							" ##  ## \n"
				CLR_WHITE	" Cherry ");
			break;
		}
	}
}


/* The amount of gold you have before gambling */
static s32b gamble_oldgold;


/*
 * Initialize gambling by getting bet.
 * Return a wager of zero, if something goes wrong.
 *
 * Hack - we return with screen still saved if wager is non-zero
 */
static s32b gamble_init(void)
{
	char out_val[160];
	cptr p;

	s32b wager;
	s32b maxbet;

	/* Save gold before gambling */
	gamble_oldgold = p_ptr->au;

	screen_save();

	/* No money */
	if (p_ptr->au < 1)
	{
		msgf("Hey! You don't have gold - get out of here!");

		screen_load();
		return (0);
	}

	clear_region(0, 5,23);

	/* Set maximum bet */
	if (p_ptr->lev < 10)
		maxbet = p_ptr->lev * 100;
	else
		maxbet = p_ptr->lev * 1000;

	/* We can't bet more than we have */
	maxbet = MIN(maxbet, p_ptr->au);

	/* Get the wager */
	out_val[0] = 0;

	/*
	 * Use get_string() because we may need more than
	 * the s16b value returned by get_quantity().
	 */
	if (!get_string(out_val, 33, "Your wager (1-%ld) ? ", maxbet))
	{
		screen_load();
		return (0);
	}

	/* Strip spaces */
	for (p = out_val; *p == ' '; p++) ;

	/* Get the wager */
	wager = atol(p);

	if (wager > p_ptr->au)
	{
		msgf("Hey! You don't have the gold - get out of here!");

		screen_load();
		return (0);
	}
	else if (wager > maxbet)
	{
		msgf("I'll take %ld gold of that. Keep the rest.", maxbet);
		wager = maxbet;
	}
	else if (wager < 1)
	{
		msgf("Ok, we'll start with 1 gold.");
		wager = 1;
	}

	message_flush();

	prtf(2, 20, "Gold before game: %9ld\n"
				"Current Wager:    %9ld", p_ptr->au, wager);

	/* Prevent savefile-scumming of the casino */
	Rand_quick = TRUE;
	Rand_value = time(NULL);

	/* Return the amount of gold bet */
	return (wager);
}


static bool gamble_again(bool win, int odds, s32b wager)
{
	char again;

	if (win)
	{
		prtf(37, 16, "YOU WON\n"
					 "Payoff: %ld\n"
                     "Again(y/n)?", odds * wager);
        p_ptr->au += odds * wager;
	}
	else
	{
		prtf(37, 16, "You Lost\n"
					 "\n"
                     "Again(y/n)?");
        p_ptr->au -= wager;
	}

	prtf(2, 22, "Current Gold:     %9ld", p_ptr->au);
	Term_gotoxy(49, 18);
	again = inkey();

	if ((again != 'y') && (again != 'Y')) return (FALSE);

	if (wager > p_ptr->au)
	{
		msgf("Hey! You don't have the gold - get out of here!");
		message_flush();

		/* Get out here */
		return (FALSE);
	}

	/* The player wants another go */
	return (TRUE);
}


/*
 * Finished gambling
 */
static void gamble_done(void)
{
	/* Switch back to complex RNG */
	Rand_quick = FALSE;

	prtf(37, 18, "");
	if (p_ptr->au >= gamble_oldgold)
		msgf("You came out a winner! We'll win next time, I'm sure.");
	else
		msgf("You lost gold! Haha, better head home.");

	message_flush();

	screen_load();
}


void gamble_help(void)
{
	/* Peruse the gambling help file */
	(void)show_file("gambling.txt", NULL, 0, 0);
}


void gamble_in_between(void)
{
	s32b wager = gamble_init();
	int roll1, roll2, choice;

	bool win;

	/* Exit if the player is out of gold */
	if (!wager) {
		Rand_quick = FALSE;
		return;
	}

	while (TRUE)
	{
		win = FALSE;

		put_fstr(2, 5, CLR_GREEN "In Between");

		roll1 = randint1(10);
		roll2 = randint1(10);
		choice = randint1(10);

		put_fstr(3, 8, "Black die: %d       Black Die: %d", roll1, roll2);
		put_fstr(14, 11, "Red die: %d", choice);

		if (((choice > roll1) && (choice < roll2)) ||
			((choice < roll1) && (choice > roll2)))
			win = TRUE;
		if (!gamble_again(win, 3, wager)) break;
	}

	gamble_done();
}


void gamble_craps(void)
{
	s32b wager = gamble_init();

	int roll1, roll2, roll3, choice;

	bool win;

	/* Exit if the player is out of gold */
	if (!wager) {
		Rand_quick = FALSE;
		return;
	}

	while (TRUE)
	{
		put_fstr(2, 5, CLR_GREEN "Craps");

		/* Roll the dice */
		roll1 = randint1(6);
		roll2 = randint1(6);
		roll3 = roll1 + roll2;
		choice = roll3;

		put_fstr(5, 7, "First roll: %d %d    Total: %d", roll1, roll2, roll3);

		/* Is it is result straight away? */
		if ((roll3 == 7) || (roll3 == 11))
			win = TRUE;
		else if ((roll3 == 2) || (roll3 == 3) || (roll3 == 12))
			win = FALSE;
		else
			while (TRUE)
			{
				/* Ok - we need to roll a few more times */
				msgf("Hit any key to roll again");
				message_flush();

				roll1 = randint1(6);
				roll2 = randint1(6);
				roll3 = roll1 + roll2;

				prtf(5, 8, "Roll result: %d %d   Total:     %d",
						roll1, roll2, roll3);

				if (roll3 == choice)
					win = TRUE;
				else if (roll3 == 7)
					win = FALSE;
				else
					continue;

				/* We have a result */
				break;
			}
		if (!gamble_again(win, 1, wager)) break;
	}

	gamble_done();
}


void gamble_spin_wheel(void)
{
	s32b wager = gamble_init();

	int roll1, choice;

	bool win;

	/* Exit if the player is out of gold */
	if (!wager) {
		Rand_quick = FALSE;
		return;
	}

	while (TRUE)
	{
		win = FALSE;

		put_fstr(2, 5, CLR_GREEN "Wheel");
		prtf(5, 7, "1  2  3  4  5  6  7  8  9 10");
		prtf(3, 8, "--------------------------------");

		choice = get_quantity("Pick a number (1-10): ", 10);

		message_flush();
		roll1 = randint1(10);
		prtf(3, 13, "The wheel spins to a stop and the winner is %d",
				roll1);
		clear_row(9);
		prtf((3 * roll1 + 2), 9, "*");

		if (roll1 == choice) win = TRUE;

		if (!gamble_again(win, 8, wager)) break;
	}

	gamble_done();
}


void gamble_dice_slots(void)
{
	static const char *fruit[6] =
	{ "Lemon", "Orange", "Sword", "Shield", "Plum", "Cherry" };

	s32b wager = gamble_init();

	int roll1, roll2, choice;
	int odds;

	bool win;

	/* Exit if the player is out of gold */
	if (!wager) {
		Rand_quick = FALSE;
		return;
	}

	while (TRUE)
	{
		put_fstr(2, 5, CLR_GREEN "Dice Slots");
		win = FALSE;
		odds = 0;

		/* Roll the dice */
		roll1 = randint1(6);
		roll2 = randint1(6);
		choice = randint1(6);

		/* Show the result */
		prtf(37, 15, "%s %s %s", fruit[roll1 - 1], fruit[roll2 - 1],
				fruit[choice - 1]);
		prtf(2, 7, "/--------------------------\\");
		prtf(2, 17, "\\--------------------------/");

		display_fruit(3, 8, roll1 - 1);
		display_fruit(12, 8, roll2 - 1);
		display_fruit(21, 8, choice - 1);

		/* What did we win? */
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

		if (!gamble_again(win, odds, wager)) break;
	}

	gamble_done();
}


/*
 * The Inn
 * Note that resting for the night was a perfect way to avoid player
 * ghosts in the town *if* you could only make it to the inn in time (-:
 * Now that the ghosts are temporarily disabled in 2.8.X, this function
 * will not be that useful.
 *
 * (Although food is fairly hard to find elsewhere - so the Inn is quite
 *  useful when you are hungry after crossing the wilderness.)
 *
 * Resting at night is also a quick way to restock stores -KMW-
 */
bool inn_rest(void)
{
	s32b new_turn;
	/* Only at night time */
	if ((turn % TOWN_DAY) < TOWN_HALF_DAY)
	{
		msgf("The rooms are available only at night.");
		message_flush();

		return (FALSE);
	}

	/* Hurt?  (Modified to allow people to rest if they are only cut */
	if ((query_timed(TIMED_POISONED)))
	{
		msgf("You need a healer, not a room.");
		message_flush();
		msgf("Sorry, but don't want anyone getting sick in here.");
		message_flush();

		return (FALSE);
	}

	/* Rest all night */
	new_turn = ((turn / TOWN_HALF_DAY) + 1) * TOWN_HALF_DAY;

	/* Save the number of turns skipped; they don't "count" */
	turn_offset += (new_turn - turn);
	turn = new_turn;
	p_ptr->chp = p_ptr->mhp;

	/*
	 * Nightmare mode has a TY_CURSE at midnight...
	 * and the player may want to avoid that.
	 */
	if (ironman_nightmare)
	{
		msgf("Horrible visions flit through your mind as you sleep.");

		/* Have some nightmares */
		do
		{
			have_nightmare();
		}
		while (one_in_(3));

		msgf("You awake screaming.");
		message_flush();

		return (TRUE);
	}

	/* Normally heal the player */
	(void)clear_blind();
	(void)clear_confused();
	(void)clear_stun();
	p_ptr->csp = p_ptr->msp;

	msgf("You awake refreshed for the new day.");
	message_flush();

	return (TRUE);
}

/*
 * Calculate the probability of successful hit for a weapon and certain AC
 *
 * Only accurate for the current weapon, because it includes
 * player's +to_hit.
 */
static int hit_prob(int to_h, int ac)
{
	int chance = p_ptr->skills[SKILL_THN] + (p_ptr->to_h + to_h) * BTH_PLUS_ADJ;
	int prob = 0;

	if (chance > 0 && ac < chance) prob = (100 * (chance - ac) / chance);
	return (5 + 95 * prob / 100);
}


#define WEP_MAST_COL1	2
#define WEP_MAST_COL2	45

/* Show how much damage a player does without a weapon */
static bool compare_weaponless(void)
{
	int i;
	int intmaxdam = 0, intmindam = 10;

	if (p_ptr->rp.pclass != CLASS_MONK)
	{
		if (p_ptr->rp.prace == RACE_GHOUL)
		{
			/* Claw */
			msgf("Maybe you will paralyze your foes with your claws.");
		}
		else
		{
			/* Just punching */
			msgf("You can do little damage with your fists.");
		}

		/* Free of charge */
		return (FALSE);
	}

	/* State the obvious */
	msgf("Monks without a weapon use martial arts.");

	/* Need more room on the screen so clear a part of it */
	if (p_ptr->lev >= 45) clear_region(0, 15, 21);

	/* Show the list of attacks */
	put_fstr(WEP_MAST_COL1, 4, CLR_L_BLUE "Possible Attacks:");

	for (i = 0; i < MAX_MA && ma_blows[i].min_level < p_ptr->lev; i++)
	{
		const martial_arts *ma_ptr = &ma_blows[i];

		intmaxdam = ma_ptr->dd * ma_ptr->ds;
		intmindam = MIN(intmindam, ma_ptr->dd);

		/* Show a line from the table */
		put_fstr(WEP_MAST_COL1, i + 5, "%s (%dd%d)",
				 format(ma_ptr->desc, "a monster"),
				 ma_ptr->dd, ma_ptr->ds);
	}

	put_fstr(WEP_MAST_COL2, 4, CLR_L_BLUE "Possible Damage:");

	/* Damage for one blow (if it hits) */
	put_fstr(WEP_MAST_COL2, 5, "One Strike: %d-%d damage", intmindam, intmaxdam);

	/* The whole attack */
	intmindam *= p_ptr->num_blow;
	intmaxdam *= p_ptr->num_blow;

	/* Damage for the complete attack (if all blows hit) */
	put_fstr(WEP_MAST_COL2, 6, "One Attack: %d-%d damage", intmindam, intmaxdam);

	/* Print hit probabilities */
	put_fstr(WEP_MAST_COL2, 8, "Enemy AC:  Low   Medium  High \n"
							   "Hit Prob:  %2d%% %2d%% %2d%% %2d%% %2d%%",
			hit_prob(0, 25), hit_prob(0, 50), hit_prob(0, 75), hit_prob(0, 100),
			hit_prob(0, 200));

	/* This info costs money */
	return (TRUE);
}


/*
 * Display the damage figure of an object
 * (used by compare_weapon_aux1)
 *
 * Only accurate for the current weapon, because it includes
 * the current +dam of the player.
 */
static void compare_weapon_aux2(const object_type *o_ptr, int numblows,
                                int r, cptr attr, byte slay)
{
	long maxdam, mindam;
	int dambonus;

	int intmaxdam, intmindam;

	dambonus = o_ptr->to_d + p_ptr->to_d;

	/* Include effects of slaying bonus */
	mindam = o_ptr->dd * slay * 10;

	maxdam = (o_ptr->dd * o_ptr->ds * deadliness_calc(dambonus) * slay) / 10;

	/* Include effects of slaying bonus */
	mindam += (slay - 10) * 100;
	maxdam += (slay - 10) * 100;

	/* Number of blows */
	maxdam *= numblows;
	mindam *= numblows;

	/* rescale */
	intmaxdam = maxdam / 100;
	intmindam = mindam / 100;

	/* Print the intro text */
	put_fstr(WEP_MAST_COL2, r, attr);

	/* Print the damage */
	put_fstr(WEP_MAST_COL2 + 8, r, " %d-%d damage", intmindam, intmaxdam);
}


/*
 * Show the damage figures for the various monster types
 *
 * Only accurate for the current weapon, because it includes
 * the current number of blows for the player.
 */
static void compare_weapon_aux1(const object_type *o_ptr)
{
	int r = 10;

	/* Print the relevant lines */
	if (FLAG(o_ptr, TR_SLAY_ANIMAL))
	{
		compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++,
							CLR_YELLOW "Animals:", 17);
	}
	if (FLAG(o_ptr, TR_SLAY_EVIL))
	{
		compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++,
							CLR_YELLOW "Evil:", 15);
	}
	if (FLAG(o_ptr, TR_SLAY_UNDEAD))
	{
		compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++,
							CLR_YELLOW "Undead:", 20);
	}
	if (FLAG(o_ptr, TR_SLAY_DEMON))
	{
		compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++,
							CLR_YELLOW "Demons:", 20);
	}
	if (FLAG(o_ptr, TR_SLAY_ORC))
	{
		compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++,
							CLR_YELLOW "Orcs:", 20);
	}
	if (FLAG(o_ptr, TR_SLAY_TROLL))
	{
		compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++,
							CLR_YELLOW "Trolls:", 20);
	}
	if (FLAG(o_ptr, TR_SLAY_GIANT))
	{
		compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++,
							CLR_YELLOW "Giants:", 20);
	}
	if (FLAG(o_ptr, TR_SLAY_DRAGON))
	{
		compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++,
							CLR_YELLOW "Dragons:", 20);
	}
	if (FLAG(o_ptr, TR_KILL_DRAGON))
	{
		compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++,
							CLR_YELLOW "Dragons:", 30);
	}
	if (FLAG(o_ptr, TR_BRAND_ACID))
	{
		compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++,
							CLR_RED "Acid:", 20);
	}
	if (FLAG(o_ptr, TR_BRAND_ELEC))
	{
		compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++,
							CLR_RED "Elec:", 20);
	}
	if (FLAG(o_ptr, TR_BRAND_FIRE))
	{
		compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++,
							CLR_RED "Fire:", 20);
	}
	if (FLAG(o_ptr, TR_BRAND_COLD))
	{
		compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++,
							CLR_RED "Cold:", 20);
	}
	if (FLAG(o_ptr, TR_BRAND_POIS))
	{
		compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++,
							CLR_RED "Poison:", 20);
	}
}


/*
 * Calculate the probability of critical hit for a weapon
 *
 * Only accurate for the current weapon, because it includes
 * player's +to_hit.
 */
static int critical_prob(int to_h, int number)
{
	int chance = p_ptr->skills[SKILL_THN] + (p_ptr->to_h + to_h) * BTH_PLUS_ADJ;

	if (chance <= 0) return (0);

	/* Chance we make a critical */
	chance = (chance * 100) / (chance + 240);

	/* Which critical do we want? */
	switch (number)
	{
		case 0:
		{
			/* No critical */
			return (100 - chance);
		}

		case 1:
		{
			/* x 1.5 */
			chance = ((chance * 89) / 90);
			chance = ((chance * 39) / 40);
			chance = ((chance * 11) / 12);

			return (2 * chance / 3);
		}

		case 2:
		{
			/* x 2.0 */
			chance = ((chance * 89) / 90);
			chance = ((chance * 39) / 40);
			chance = ((chance * 11) / 12);

			return (chance / 3);
		}

		case 3:
		{
			/* x 2.7 */
			chance = ((chance * 89) / 90);
			chance = ((chance * 39) / 40);

			return (chance / 12);
		}

		case 4:
		{
			/* x 3.6 */
			chance = ((chance * 89) / 90);

			return (chance / 40);
		}

		case 5:
		{
			/* x 5.0 */
			return (chance / 90);
		}
	}

	/* Paranoia */
	return (0);
}


/*
 * Displays all info about a weapon
 *
 * Only accurate for the current weapon, because it includes
 * various info about the player's +to_dam and number of blows.
 */
static void list_weapon(const object_type *o_ptr)
{
	int dambonus;

	int intmaxdam, intmindam;

	/* Print the weapon name */
	put_fstr(WEP_MAST_COL1, 6, CLR_L_BLUE "%v", OBJECT_FMT(o_ptr, TRUE, 0));

	/* Print to_hit and to_dam of the weapon */
	put_fstr(WEP_MAST_COL1, 8, "To Hit: %d  Deadliness: %d", o_ptr->to_h, o_ptr->to_d);

	/* Print the weapons base damage dice and blows */
	put_fstr(WEP_MAST_COL1, 10, "Dice: %dd%d    Number of Blows: %d",
			(int)o_ptr->dd, (int)o_ptr->ds, p_ptr->num_blow);

	/* Print hit probabilities */
	put_fstr(WEP_MAST_COL1, 12, "Enemy AC:  Low   Medium  High \n"
								"Hit Prob:  %2d%% %2d%% %2d%% %2d%% %2d%%",
			hit_prob(o_ptr->to_h, 25), hit_prob(o_ptr->to_h, 50),
			hit_prob(o_ptr->to_h, 75), hit_prob(o_ptr->to_h, 100),
			hit_prob(o_ptr->to_h, 200));

	/* Print critical hit probabilities */
	put_fstr(WEP_MAST_COL1, 15, CLR_RED "Critical:" CLR_WHITE " 1.0 1.5 2.0 2.7 3.6 5.0\n"
										"          %2d%% %2d%% %2d%% %2d%% %2d%% %2d%%",
			critical_prob(o_ptr->to_h, 0),
			critical_prob(o_ptr->to_h, 1),
			critical_prob(o_ptr->to_h, 2),
			critical_prob(o_ptr->to_h, 3),
			critical_prob(o_ptr->to_h, 4), critical_prob(o_ptr->to_h, 5));

	put_fstr(WEP_MAST_COL2, 6, CLR_L_BLUE "Possible Damage:");

	dambonus = o_ptr->to_d + p_ptr->to_d;

	/* Calculate max and min damage */
	intmindam = o_ptr->dd;
	intmaxdam = (o_ptr->dd * o_ptr->ds * deadliness_calc(dambonus)) / 100;


	/* Damage for one blow (if it hits) */
	put_fstr(WEP_MAST_COL2, 7, "One Strike: %d-%d damage", intmindam, intmaxdam);

	/* Rescale */
	intmindam *= p_ptr->num_blow;
	intmaxdam *= p_ptr->num_blow;

	/* Damage for the complete attack (if all blows hit) */
	put_fstr(WEP_MAST_COL2, 8, "One Attack: %d-%d damage", intmindam, intmaxdam);
}


/*
 * Compare weapons
 *
 * Copies the weapons to compare into the weapon-slot and
 * compares the values for both weapons.
 */
bool compare_weapons(void)
{
	object_type *o_ptr;
	bool result = TRUE;;

	/* Clear the screen */
    clear_region(0, 6, 18);

	/* Point to wielded weapon */
	o_ptr = &p_ptr->equipment[EQUIP_WIELD];

	/* Check to see if we have one */
	if (o_ptr->k_idx)
	{
		/* Identify the weapon */
		identify_item(o_ptr);
		object_mental(o_ptr);

		/* Save all the known flags */
		o_ptr->kn_flags[0] = o_ptr->flags[0];
		o_ptr->kn_flags[1] = o_ptr->flags[1];
		o_ptr->kn_flags[2] = o_ptr->flags[2];
		o_ptr->kn_flags[3] = o_ptr->flags[3];

		/* Erase the "feeling" */
		o_ptr->feeling = FEEL_NONE;


		/* List the new values */
		list_weapon(o_ptr);
		compare_weapon_aux1(o_ptr);

		put_fstr(0, 19,
				"(Only highest damage applies per monster. Special damage not cumulative.)");

		msgf("Based on your current abilities, here is what your weapon will do:");
	}
	else
	{
		/* Bare hands */
		result = compare_weaponless();
	}

	/* Give the player a chance to see it all */
	if (auto_more) (void)inkey();

	/* Done */
	return (result);
}


/*
 * Enchant item
 */
bool enchant_item(s32b cost, bool to_hit, bool to_dam, bool to_ac, bool weap)
{
	bool okay = FALSE;
	object_type *o_ptr;
	cptr q, s;
	int maxenchant = (p_ptr->lev / 5);
	int maxenchant_d = (p_ptr->lev / 3);

	if (weap)
	{
		/* Select weapons */
		item_tester_hook = item_tester_hook_melee_weapon;
	}
	else
	{
		/* Select armour */
		item_tester_hook = item_tester_hook_armour;
	}

	/* Get an item */
	q = "Improve which item? ";
	s = "You have nothing to improve.";

	/* Get the item */
	o_ptr = get_item(q, s, (USE_INVEN | USE_EQUIP));

	/* No valid items */
	if (!o_ptr) return (FALSE);

	/* Check if the player has enough money */
	if (p_ptr->au < (cost * o_ptr->number))
	{
		msgf("You do not have the gold to improve %v!",
			OBJECT_FMT(o_ptr, TRUE, 0));
		message_flush();
		return (FALSE);
	}

	/* Note that enchanting something a negative number of times will fail */

	/* Enchant to hit */
	if ((to_hit) && (enchant(o_ptr, maxenchant - o_ptr->to_h,
							 (ENCH_TOHIT | ENCH_FORCE), 0)))
	{
		okay = TRUE;
	}

	/* Enchant to damage */
	if ((to_dam) && (enchant(o_ptr, maxenchant_d - o_ptr->to_d,
							 (ENCH_TODAM | ENCH_FORCE), 0)))
	{
		okay = TRUE;
	}

	/* Enchant to AC */
	if ((to_ac) && (enchant(o_ptr, maxenchant - o_ptr->to_a,
							(ENCH_TOAC | ENCH_FORCE), 0)))
	{
		okay = TRUE;
	}

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message */
		msgf("The improvement failed.");

		return (FALSE);
	}
	else
	{
		msgf("Improved %v for %d gold.", OBJECT_FMT(o_ptr, TRUE, 1),
			 cost * o_ptr->number);
		message_flush();

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
void building_recharge(s32b cost)
{
	int lev;
	object_type *o_ptr;
	object_kind *k_ptr;
	cptr q, s;
	int price;
	int charges;
	int max_charges;


	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item */
	q = "Recharge which item? ";
	s = "You have nothing to recharge.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* No valid item */
	if (!o_ptr) return;

	k_ptr = &k_info[o_ptr->k_idx];

	/*
	 * We don't want to give the player free info about
	 * the level of the item or the number of charges.
	 */
	/* The item must be "known" */
	if (!object_known_p(o_ptr))
	{
		msgf("The item must be identified first!");
		message_flush();

		/* Allow ID if wand/staff type already known but charges not known */
		if (object_aware_p(o_ptr) && (p_ptr->au >= 50) && get_check("Identify for 50 gold? "))
		{
			/* Pay the price */
			p_ptr->au -= 50;

			/* Identify it */
			identify_item(o_ptr);

			/* Description */
			msgf("You have: %v.", OBJECT_FMT(o_ptr, TRUE, 3));
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
			msgf("That doesn't need to be recharged.");
			message_flush();
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
			msgf("This wand is already fully charged.");
		else if ((o_ptr->tval == TV_WAND) && (o_ptr->number > 1))
			msgf("These wands are already fully charged.");
		else if ((o_ptr->tval == TV_STAFF) && (o_ptr->number == 1))
			msgf("This staff is already fully charged.");
		else if ((o_ptr->tval == TV_STAFF) && (o_ptr->number > 1))
			msgf("These staffs are already fully charged.");

		message_flush();
		return;
	}

	/* Factor in shopkeeper greed */
	price = (price * cost / 100);

	/* Check if the player has enough money */
	if (p_ptr->au < price)
	{
		msgf("You need %d gold to recharge %v!", price,
			 OBJECT_FMT(o_ptr, TRUE, 0));
		message_flush();
		return;
	}

	if (o_ptr->tval == TV_ROD)
	{
		if (get_check("Recharge the %s for %d gold? ",
							 ((o_ptr->number > 1) ? "rods" : "rod"), price))
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
		char buf[160];

		if (o_ptr->tval == TV_STAFF)
			max_charges = k_ptr->pval - o_ptr->pval;
		else
			max_charges = o_ptr->number * k_ptr->pval - o_ptr->pval;

		/* Get prompt */
		strnfmt(buf, 160, "Add how many charges for %d gold? ", price);

		/* Get the quantity for staves and wands */
		charges = get_quantity(buf, MIN(p_ptr->au / price, max_charges));

		/* Do nothing */
		if (charges < 1) return;

		/* Get the new price */
		price *= charges;

		/* Recharge */
		o_ptr->pval += charges;

		/* Hack - no "used" charges */
		o_ptr->ac = 0;

		/* We no longer think the item is empty */
		o_ptr->info &= ~(OB_EMPTY);
	}

	/* Give feedback */
	msgf("%^v %s recharged for %d gold.", OBJECT_FMT(o_ptr, TRUE, 3),
			   ((o_ptr->number > 1) ? "were" : "was"), price);
	message_flush();

	/* Notice changes */
	notice_inven();

	/* Pay the price */
	p_ptr->au -= price;

	/* Finished */
	return;
}


bool building_healer(void)
{
	bool paid = FALSE;

	if (do_res_stat(A_STR)) paid = TRUE;
	if (do_res_stat(A_INT)) paid = TRUE;
	if (do_res_stat(A_WIS)) paid = TRUE;
	if (do_res_stat(A_DEX)) paid = TRUE;
	if (do_res_stat(A_CON)) paid = TRUE;
	if (do_res_stat(A_CHR)) paid = TRUE;

	if (paid)
	{
		msgf("You are infused with magic, and your ailments disappear.");
		message_flush();
	}

	return (paid);
}


static int collect_magetower_links(int n, int *link_p, int *link_w, s32b *cost,
                                   int factor)
{
	place_type *pl_ptr = &place[p_ptr->place_num];

	int i, j;
	int max_link = 0;

	/* Get current town location */
	int x = pl_ptr->x, y = pl_ptr->y;

	/* Find the magetowers we're linked to */
	for (i = 0; i < place_count; i++)
	{
		place_type *pl_ptr = &place[i];

		/* Skip current town */
		if (i == p_ptr->place_num) continue;

		for (j = 0; j < pl_ptr->numstores; j++)
		{
			store_type *st_ptr = &pl_ptr->store[j];

			if (max_link >= n) return (max_link);

			/* Hack - only allow teleportation to known magetowers */
			if (!st_ptr->data) continue;

			/* Is it a mage tower? */
			if ((st_ptr->type == BUILD_MAGETOWER0) ||
				(st_ptr->type == BUILD_MAGETOWER1))
			{
				link_p[max_link] = i;
				link_w[max_link] = j;
				cost[max_link] = distance(x, y, pl_ptr->x, pl_ptr->y) * factor;
				max_link++;

				/* Only collect 1 link per city */
				break;
			}
		}
	}

	return max_link;
}

/*
 * Record that the player has visited a magetower
 * and paid for the privelidge of being able to
 * teleport there from another magetower
 */
void record_aura(void)
{
	store_type *b_ptr = get_current_store();

	if (!b_ptr->data)
	{
		/*
		 * Hack XXX - save the fact we have "noticed" this tower
		 * in this variable, which later will have to be removed
		 * from store_type anyway.
		 */
		b_ptr->data = 1;

		msgf("The portal keeper notes your aura.");
	}
}


bool building_magetower(int factor, bool display)
{
	int link_p[24], link_w[24];
	int max_link = 0;
	int i;

	s32b cost[24];

	char out_val[160];

	/* Collect links */
	max_link = collect_magetower_links(24, link_p, link_w, cost, factor);

	if (display)
	{
		/* Haven't been here before? */
		if (!build_ptr->data)
		{
			put_fstr(35, 18, CLR_YELLOW " R) Record aura (%dgp)", factor * 5);
		}

		put_fstr(35, 19, CLR_YELLOW " T) Teleport");

		for (i = 0; i < max_link; i++)
		{
			int row = i % 12 + 4;
			int col = (i / 12) * 40;

			/* Label it, clear the line --(-- */
			prtf(col, row, "%c) ", I2A(i));

			/* Print place name */
			prtf(col + 3, row, place[link_p[i]].name);

			/* Print cost */
			prtf(col + 30, row, "%ld au", (long)cost[i]);
		}
	}
	else
	{
		char command;

		if (max_link == 0)
		{
			msgf("You do not know any other towns to teleport to.");
			return (FALSE);
		}

		/* Build the prompt */
		strnfmt(out_val, 160, "(Towns %c-%c, ESC to exit)",
					  I2A(0), I2A(max_link - 1));

		while (TRUE)
		{
			int k;

			/* Escape */
			if (!get_com(out_val, &command)) break;

			k = (islower(command) ? A2I(command) : -1);

			if ((k >= 0) && (k < max_link) && test_gold(cost[k]))
			{
				place_type *pl_ptr2 = &place[link_p[k]];
				store_type *st_ptr2 = &pl_ptr2->store[link_w[k]];

				/* Subtract off cost */
				p_ptr->au -= cost[k];

				/* Move the player */
				p_ptr->px = pl_ptr2->x * 16 + st_ptr2->x;
				p_ptr->py = pl_ptr2->y * 16 + st_ptr2->y;

				p_ptr->wilderness_x = p_ptr->px;
				p_ptr->wilderness_y = p_ptr->py;

				/* Notice player location */
				Term_move_player();

				/* Remove all monster lights */
				lite_n = 0;

				/* Notice the move */
				move_wild();

				/* Check for new panel (redraw map) */
				verify_panel();

				/* Update stuff */
				p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MON_LITE);

				/* Update the monsters */
				p_ptr->update |= (PU_DISTANCE);

				/* Window stuff */
				p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

				force_build_exit = TRUE;

				return (TRUE);
			}

			/* Oops */
			bell("Illegal choice!");
		}
	}

	return (FALSE);
}


static bool process_build_hook(const field_type *f_ptr)
{
	int factor;

	char command[2];

	bool done = FALSE;

	/* The charisma factor */
	factor = adj_chr_gold[p_ptr->stat[A_CHR].ind];

	factor = ((factor + 200) * build_ptr->greed) / 400;

	/* Hack - lua expects a string instead of a character */
	command[0] = (byte) p_ptr->cmd.cmd;
	command[1] = '\0';

	field_script_const(f_ptr, FIELD_ACT_BUILD_ACT2, "is:b",
						LUA_VAR(factor), LUA_VAR(command),
						LUA_RETURN(done));

	/* Redraw screen */
	display_build(f_ptr);

	/* Did we do anything? */
	return (done);
}


/*
 * Process a command in a building
 *
 * Note that we must disable some commands which are allowed
 * in the dungeon / stores but not in the buildings, to prevent chaos,
 * and also to give more free keys in order to have building
 * specific commands.
 *
 * Hack - we buypass macros / keymaps to prevent silliness when
 * people use the roguelike keyset and press a 'direction' key
 * which also corresponds to a building command.
 */
static bool build_process_command(const field_type *f_ptr)
{
	/* Hack - Get a command */
	p_ptr->cmd.cmd = inkey();

	/* Handle repeating the last command */
	repeat_check();

	/* Process the building-specific commands */
	if (process_build_hook(f_ptr)) return (FALSE);

	/* Parse the command */
	switch (p_ptr->cmd.cmd)
	{
		case ESCAPE:
		{
			/* Leave */
			return (TRUE);
		}

		case KTRL('R'):
		{
			/* Redraw */
			do_cmd_redraw();
			display_build(f_ptr);
			break;
		}

		case ' ':
		case '\r':
		{
			/* Ignore return */
			break;
		}

		default:
		{
			/* Is it a standard command? */
			if (!do_standard_command(p_ptr->cmd.cmd))
			{
				/* Hack -- Unknown command */
				msgf("That command does not work in buildings.");
			}
			else
			{
				/* Why this is neeed so often I don't know */
				message_flush();
			}

			break;
		}
	}

	return (FALSE);
}


/*
 * Do building commands
 */
void do_cmd_bldg(const field_type *f_ptr)
{
	store_type *b_ptr;
	bool leave_build = FALSE;

	/* Disturb */
	disturb(FALSE);

	b_ptr = get_current_store();

	/* Paranoia */
	if (!b_ptr) return;

	/* Save building pointer for lua hook */
	build_ptr = b_ptr;

	/* Init building if required */
	field_script_const(f_ptr, FIELD_ACT_SB_INIT, "");

	/* Some quests are finished by finding a building */
	trigger_quest_complete(QX_FIND_SHOP, (vptr)b_ptr);

	/* Forget the view */
	forget_view();

	/* Hack -- Increase "icky" depth */
	screen_save();

	/* No command argument */
	p_ptr->cmd.arg = 0;

	/* No repeated command */
	p_ptr->cmd.rep = 0;

	/* No automatic command */
	p_ptr->cmd.new = 0;

	/* Display the building */
	display_build(f_ptr);

	/* Hack - change the redraw hook so bigscreen works */
	angband_term[0]->resize_hook = resize_build;

	/* Interact with player */
	while (!leave_build)
	{
		/* Clear */
		clear_from(21);

		/* Basic commands */
		prtf(0, 23, " ESC) Exit building");

		/* Show your gold */
		building_prt_gold();

		/* Process the command */
		leave_build = build_process_command(f_ptr);

		if (force_build_exit)
		{
			force_build_exit = FALSE;
			break;
		}

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();

		message_flush();
	}

	/* Free turn XXX XXX XXX */
	p_ptr->state.energy_use = 0;

	/* Hack -- Character is no longer in "icky" mode */
	screen_load();

	/* Hack -- Cancel automatic command */
	p_ptr->cmd.new = 0;

	/* Flush messages XXX XXX XXX */
	message_flush();

	/* Hack - reset the redraw hook */
	angband_term[0]->resize_hook = resize_map;

	/* Clear the screen */
	Term_clear();

	/* Update for the changed screen size */
	resize_map();

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
	cptr own_name = owner_names[randint0(owner_names_max)];
	cptr own_suffix = owner_suffix[randint0(owner_suffix_max)];

	/* Activate that building */
	store_type *st_ptr = &place[town_num].store[build_num];

	/* Pick an owner */
	st_ptr->owner_name = quark_fmt("%s %s", own_name, own_suffix);

	/* These are set in place_sb() via lua hooks */
	st_ptr->greed = 0;
	st_ptr->max_cost = 0;

	/* Set the type */
	st_ptr->type = build_type;

	/* Initialize */
	st_ptr->data = 0;
	st_ptr->last_visit = 0;
}

/*
 * Repair all equipped and weapons to +0, if worse than that
 */
void build_cmd_repair(void)
{
	int i = 0;
	bool allcurse = FALSE;
	bool known = FALSE;
	bool repaired = FALSE;
	object_type * o_ptr;

	for (i = 0; i < EQUIP_MAX; i++)
	{
		o_ptr = &(p_ptr->equipment[i]);

		/* Weapon */
		if (i == EQUIP_WIELD || i == EQUIP_BOW) {
			if (cursed_p(o_ptr)) {
				if (!repaired) allcurse = TRUE;
				continue;
			}
			if (o_ptr->to_d < 0 || o_ptr->to_h < 0) {
				o_ptr->to_d = MAX(o_ptr->to_d, 0);
				o_ptr->to_h = MAX(o_ptr->to_h, 0);
				if (object_known_p(o_ptr)) known = TRUE;
				repaired = TRUE;
				allcurse = FALSE;
			}
		} else if (i < EQUIP_BODY) {
			continue;
		/* Armor */
		} else {
			if (cursed_p(o_ptr)) {
				if (!repaired) allcurse = TRUE;
				continue;
			}
			if (o_ptr->to_a < 0) {
				o_ptr->to_a = 0;
				if (object_known_p(o_ptr)) known = TRUE;
				repaired = TRUE;
				allcurse = FALSE;
			}
		}
	}

	/* Now let the player know the result */

	if (allcurse && !repaired)
	{
		msgf ("We can't repair cursed equipment!");
	}
	else if (!repaired)
	{
		msgf ("Your equipment was in fine shape already.");
	}
	else if (!known)
	{
		msgf ("They repair your equipment, but you don't notice a difference.");
	}
	else
		msgf ("Your equipment is repaired.");

}

/*
 * Decide on the "level" for the next quest.
 */
void build_set_qlevel(void)
{
	store_type * st_ptr = get_current_store();

	int level = st_ptr->data;  /* this is where we store it */

	/* max_stock used in quest buildings as 'quest base' */
	int base = st_ptr->max_stock;

	/* Only set a level if there isn't one already */
	if (level) return;

	/* Level base: whichever is greater, the player's (max) level or the building's minimum */
	level = MAX(p_ptr->max_lev, base);

	/* Add a bit */
	level += randint1(5);

	/* The minimum goes up */
	st_ptr->max_stock = level+3;

	/* Set the level */
	st_ptr->data = level;
}

int build_get_qlevel(void)
{
	store_type * st_ptr = get_current_store();
	quest_type * q_ptr;
	int q_num;

	if (st_ptr->type == BUILD_FARM || st_ptr->type == BUILD_COURIER)
		return (st_ptr->data);

	q_num = lookup_quest_building_next(st_ptr);

	if (q_num == -1) return (-1);

	q_ptr = &quest[q_num];

	return (q_ptr->level);
}

/*
 * Player takes out or repays a loan.
 */
void build_cmd_loan(int factor)
{
	store_type * st_ptr = get_current_store();
	store_type * st_ptr2 = get_loaner();
	factor = (factor < 110 ? 110 : (factor > 135 ? 135 : factor));

	if (st_ptr2 && st_ptr2 != st_ptr)
	{
		/* Player already has a loan, with a different bank. */
		msgf ("You can't pay off your loan here.");
		return;
	}
	else if (st_ptr2 == st_ptr)
	{
		/* Player has a loan; ask them if they want to pay it off. */
		s32b amt = get_loan_amount();

		if (p_ptr->au < amt)
		{
			/* Can't pay it off.  For now, just boot them.  Later, perhaps implement
			   a partial pay-off. */
			msgf ("You owe %i gold pieces.  Come back when you have the money.");
			return;
		}
		if (get_check("Pay off your loan for %i gold pieces? ", amt))
		{
			/* Pay off the loan. */
			p_ptr->au -= amt;
			msgf ("Your loan is paid.");

			/* Get rid of the loan quest */
			trigger_quest_complete(QX_LOAN, (vptr)st_ptr);
			return;
		}
	}
	else
	{
		/* No loan; offer one. */
		int amt;

	   	/* Really simple loan qualification formula */
	    amt = p_ptr->au + (p_ptr->lev * p_ptr->lev * 25);

		message_flush();

		if (get_check("We can loan you up to %i gp at %i%%.  Take out loan?", amt, (factor-100)))
		{
			/* How much */
			s32b amt2 = get_quantity_big("How much gold? ", amt);

			if (amt2 == 0) return;  /* No loan */

			if (!insert_loan((amt2 * factor) / 100))
			{
				msgf ("Sorry, we ran out of gold.");
				message_flush();
				return;
			}

			p_ptr->au += amt2;

			msgf ("Your loan is approved.  You will need to pay %d to repay your loan.", ((amt2 * factor) / 100));
			message_flush();

			/* Keep track of when the loan was issued. */
			st_ptr->last_visit = turn;
		}
	}
}

/*
 * Checks if the player has a mortgage that is past due.  This is called when the player tries to
 * enter their home, and will explain why the player is not being let in.
 *
 * Currently, does not give the player an opportunity to pay the loan; the player still must go to the
 * bank to do it.
 */

#define LOAN_PERIOD 20000

bool check_mortgage(void)
{
	store_type * st_ptr = get_loaner();

	/* No loan */
	if (st_ptr == NULL) return FALSE;

 	/* Loan is not past due */
 	if (turn - st_ptr->last_visit < LOAN_PERIOD) return FALSE;

 	/* Otherwise, the home is "foreclosed". */
 	msgf ("The doors are locked.");
	msgf ("A note reads: Loan is due.");
	msgf ("(Press CTRL-Q to check your loan status.)");

	return TRUE;
}

/*
 * Player pays price for a complement of 1st-level spellbooks.
 */
void build_cmd_spellbooks(int price)
{
    object_type *o_ptr, *j_ptr;
	int tv, sv;

	/* Make sure the player casts spells.  This should not be a problem... */
	if (p_ptr->spell.realm[0] == REALM_NONE)
	{
		msgf ("You don't use spellbooks!");
		return;
	}

	/* Make sure the player has enough gold. */
	if (!test_gold(price))
		return;

	/* Charge money */
	p_ptr->au -= price;

	/* First realm spellbook. */
	tv = TV_LIFE_BOOK + p_ptr->spell.realm[0] - 1;
	sv = 0;

	current_object_source.type = OM_STORE;
	current_object_source.place_num = p_ptr->place_num;
	current_object_source.depth = 0;
	/* Hack: gets .data field */
	(void)get_current_store();

	o_ptr = object_prep(lookup_kind(tv,sv));

	/* Make sure we can carry it */
	if (!inven_carry_okay(o_ptr))
	{
		drop_near(o_ptr, -1, p_ptr->px, p_ptr->py);
	}
	else
	{
		j_ptr = inven_carry(o_ptr);
		msgf("You have %v (%c).", OBJECT_FMT(j_ptr, TRUE, 3), I2A(get_item_position(p_ptr->inventory, j_ptr)));
		message_flush();
	}

	/* See if we have a second realm. */
	if (p_ptr->spell.realm[1] == REALM_NONE)
	{
		/* only 1 realm */
		return;
	}

	/* Second realm spellbook. */
	tv = TV_LIFE_BOOK + p_ptr->spell.realm[1] - 1;
	sv = 0;

	o_ptr = object_prep(lookup_kind(tv,sv));

	/* Make sure we can carry it */
	if (!inven_carry_okay(o_ptr))
	{
		drop_near(o_ptr, -1, p_ptr->px, p_ptr->py);
	}
	else
	{
		j_ptr = inven_carry(o_ptr);
		msgf("You have %v (%c).", OBJECT_FMT(j_ptr, TRUE, 3), I2A(get_item_position(p_ptr->inventory, j_ptr)));
		message_flush();
	}

}

void build_cmd_food (int price)
{
    object_type *o_ptr, *j_ptr;

	/* Make sure the player has enough gold. */
	if (!test_gold(price))
		return;

	/* Charge money */
	p_ptr->au -= price;

	current_object_source.type = OM_STORE;
	current_object_source.place_num = p_ptr->place_num;
	current_object_source.depth = 0;
	/* Hack: gets data */
	(void)get_current_store();

	o_ptr = object_prep(lookup_kind(TV_FOOD,SV_FOOD_RATION));

	/* Make sure we can carry it */
	if (!inven_carry_okay(o_ptr))
	{
		drop_near(o_ptr, -1, p_ptr->px, p_ptr->py);
		msgf ("Done.");
		message_flush();
	}
	else
	{
		j_ptr = inven_carry(o_ptr);
		msgf("You have %v (%c).", OBJECT_FMT(j_ptr, TRUE, 3), I2A(get_item_position(p_ptr->inventory, j_ptr)));
		message_flush();
	}
}

/*
 * Do Word of recall if it will work.
 */
bool build_cmd_recall (void)
{
	if (!check_down_wild()) return FALSE;
	if (ironman_downward) return FALSE;
	word_of_recall();
	return TRUE;
}

/*
 * Show top scores.
 */
void build_cmd_grave (void)
{
	top_twenty();
}

/*
 * Initializes a quest building.
 */
void place_qinit(int member, int base)
{
	store_type *st_ptr = get_current_store();

	/* Only initialized if not already initialized. */
	if (!st_ptr->max_cost)
	{
		/* max_cost stores membership, max_stock stores quest level base */
		st_ptr->max_stock = base;
		st_ptr->max_cost = member;
	}
}

bool is_member(void)
{
	store_type *st_ptr = get_current_store();

	return (st_ptr->max_cost == GS_MEMBER || st_ptr->max_cost == GS_LOW_MEMBER);
}

bool is_full_member(void)
{
	store_type *st_ptr = get_current_store();

	return (st_ptr->max_cost == GS_MEMBER);
}
