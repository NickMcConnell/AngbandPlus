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
 */

#include "angband.h"

/* hack as in leave_store in store.c */
static bool leave_bldg = FALSE;

/* remember building location */
static int building_loc = 0;

static bool reinit_wilderness = FALSE;

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

	if ((bldg->member_realm[p_ptr->realm1]) || (bldg->member_realm[p_ptr->realm2]))
	{
		return (TRUE);
	}

	return (FALSE);
}


/*
 * Clear the building information
 */
static void clear_bldg(int min_row, int max_row)
{
	int   i;

	for (i = min_row; i <= max_row; i++)
		prt("",i,0);
}


#if 0

/*
 * Places a building reward at the doorstep for the player -KMW-
 */
static void put_reward(byte thetval, byte thesval, int dunlevel)
{
	object_type *q_ptr, forge;
	int         i, choice;

	choice = 0;

	for(i = 1; i < max_k_idx; i++)
	{
		object_kind *k_ptr = &k_info[i];
		if ((k_ptr->tval == thetval) && (k_ptr->sval == thesval))
		{
			choice = i;
			break;
		}
	}
	q_ptr = &forge;
	object_prep(q_ptr, choice);
	apply_magic(q_ptr, dunlevel, TRUE, TRUE, TRUE);
	object_aware(q_ptr);
	object_known(q_ptr);
	drop_near(q_ptr, -1, py, px);
}

#endif


/*
 * Display a building.
 */
static void show_building(building_type* bldg)
{
	char buff[20];
	int i;
	byte action_color;
	char tmp_str[80];

	Term_clear();
	sprintf(tmp_str, "%s (%s) %35s", bldg->owner_name, bldg->owner_race, bldg->name);
	prt(tmp_str, 2, 1);
	prt("You may:", 19, 0);

	for (i = 0; i < 6; i++)
	{
		if (bldg->letters[i])
		{
			if (bldg->action_restr[i] == 0)
			{
				if ((is_owner(bldg) && (bldg->member_costs[i] == 0)) ||
					(!is_owner(bldg) && (bldg->other_costs[i] == 0)))
				{
					action_color = TERM_WHITE;
					buff[0] = '\0';
				}
				else if (is_owner(bldg))
				{
					action_color = TERM_YELLOW;
					sprintf(buff, "(%dgp)", bldg->member_costs[i]);
				}
				else
				{
					action_color = TERM_YELLOW;
					sprintf(buff, "(%dgp)", bldg->other_costs[i]);
				}
			}
			else if (bldg->action_restr[i] == 1)
			{
				if (!is_member(bldg))
				{
					action_color = TERM_L_DARK;
					strcpy(buff, "(closed)");
				}
				else if ((is_owner(bldg) && (bldg->member_costs[i] == 0)) ||
					(is_member(bldg) && (bldg->other_costs[i] == 0)))
				{
					action_color = TERM_WHITE;
					buff[0] = '\0';
				}
				else if (is_owner(bldg))
				{
					action_color = TERM_YELLOW;
					sprintf(buff, "(%dgp)", bldg->member_costs[i]);
				}
				else
				{
					action_color = TERM_YELLOW;
					sprintf(buff, "(%dgp)", bldg->other_costs[i]);
				}
			}
			else
			{
				if (!is_owner(bldg))
				{
					action_color = TERM_L_DARK;
					strcpy(buff, "(closed)");
				}
				else if (bldg->member_costs[i] != 0)
				{
					action_color = TERM_YELLOW;
					sprintf(buff, "(%dgp)", bldg->member_costs[i]);
				}
				else
				{
					action_color = TERM_WHITE;
					buff[0] = '\0';
				}
			}

			sprintf(tmp_str," %c) %s %s", bldg->letters[i], bldg->act_names[i], buff);
			c_put_str(action_color, tmp_str, 19+(i/2), 35*(i%2));
		}
	}

	prt(" ESC) Exit building", 23, 0);
}


/* reset timed flags */
static void reset_tim_flags()
{
	p_ptr->fast = 0;            /* Timed -- Fast */
	p_ptr->slow = 0;            /* Timed -- Slow */
	p_ptr->blind = 0;           /* Timed -- Blindness */
	p_ptr->paralyzed = 0;       /* Timed -- Paralysis */
	p_ptr->confused = 0;        /* Timed -- Confusion */
	p_ptr->afraid = 0;          /* Timed -- Fear */
	p_ptr->image = 0;           /* Timed -- Hallucination */
	p_ptr->poisoned = 0;        /* Timed -- Poisoned */
	p_ptr->cut = 0;             /* Timed -- Cut */
	p_ptr->stun = 0;            /* Timed -- Stun */

	p_ptr->protevil = 0;        /* Timed -- Protection */
	p_ptr->invuln = 0;          /* Timed -- Invulnerable */
	p_ptr->hero = 0;            /* Timed -- Heroism */
	p_ptr->shero = 0;           /* Timed -- Super Heroism */
	p_ptr->shield = 0;          /* Timed -- Shield Spell */
	p_ptr->blessed = 0;         /* Timed -- Blessed */
	p_ptr->tim_invis = 0;       /* Timed -- Invisibility */
	p_ptr->tim_infra = 0;       /* Timed -- Infra Vision */

	p_ptr->oppose_acid = 0;     /* Timed -- oppose acid */
	p_ptr->oppose_elec = 0;     /* Timed -- oppose lightning */
	p_ptr->oppose_fire = 0;     /* Timed -- oppose heat */
	p_ptr->oppose_cold = 0;     /* Timed -- oppose cold */
	p_ptr->oppose_pois = 0;     /* Timed -- oppose poison */
	p_ptr->tim_res_conf = 0;
	p_ptr->tim_res_blind = 0;
	p_ptr->tim_invisible = 0;
	
	p_ptr->confusing = 0;       /* Touch of Confusion */
}


/*
 * arena commands
 */
static void arena_comm(int cmd)
{
	char tmp_str[80];
	monster_race *r_ptr;
	cptr name;

	switch(cmd)
	{
		case BACT_ARENA:
			if (p_ptr->arena_number == MAX_ARENA_MONS)
			{
				clear_bldg(5,19);
				prt("               Arena Victor!", 5, 0);
				prt("Congratulations!  You have defeated all before you.", 7, 0);
				prt("For that, receive the prize: 10,000 gold pieces", 8, 0);
				prt("",10,0);
				prt("", 11, 0);
				p_ptr->au += 10000;
				msg_print("Press the space bar to continue");
				msg_print(NULL);
				p_ptr->arena_number++;
			}
			else if (p_ptr->arena_number > MAX_ARENA_MONS)
			{
				msg_print("You enter the arena briefly and bask in your glory.");
				msg_print(NULL);
			}
			else
			{
				p_ptr->leftbldg = TRUE;
				p_ptr->inside_arena = TRUE;
				p_ptr->exit_bldg = FALSE;
				reset_tim_flags();
				p_ptr->leaving = TRUE;
				leave_bldg = TRUE;
			}
			break;
		case BACT_POSTER:
			if (p_ptr->arena_number == MAX_ARENA_MONS)
				msg_print("You are victorious. Enter the arena for the ceremony.");
			else if (p_ptr->arena_number > MAX_ARENA_MONS)
				msg_print("You have won against all foes.");
			else
			{
				r_ptr = &r_info[arena_monsters[p_ptr->arena_number]];
				name = (r_name + r_ptr->name);
				(void) sprintf(tmp_str,"Do I hear any challenges against: %s", name);
				msg_print(tmp_str);
				msg_print(NULL);
			}
			break;
		case BACT_ARENA_RULES:

			/* Save screen */
			screen_save();

			/* Peruse the arena help file */
			(void)show_file("arena.txt", NULL, 0, 0);

			/* Load screen */
			screen_load();

			break;
	}
}


/*
 * display fruit for dice slots
 */
static void display_fruit(int row, int col, int fruit)
{
	switch(fruit)
	{
		case 0: /* lemon */
			c_put_str(TERM_YELLOW,"   ####.",row,col);
			c_put_str(TERM_YELLOW,"  #    #",row+1,col);
			c_put_str(TERM_YELLOW," #     #",row+2,col);
			c_put_str(TERM_YELLOW,"#      #",row+3,col);
			c_put_str(TERM_YELLOW,"#      #",row+4,col);
			c_put_str(TERM_YELLOW,"#     # ",row+5,col);
			c_put_str(TERM_YELLOW,"#    #  ",row+6,col);
			c_put_str(TERM_YELLOW,".####   ",row+7,col);
			prt(" Lemon  ",row+8,col);
			break;
		case 1: /* orange */
			c_put_str(TERM_ORANGE,"   ##   ",row,col);
			c_put_str(TERM_ORANGE,"  #..#  ",row+1,col);
			c_put_str(TERM_ORANGE," #....# ",row+2,col);
			c_put_str(TERM_ORANGE,"#......#",row+3,col);
			c_put_str(TERM_ORANGE,"#......#",row+4,col);
			c_put_str(TERM_ORANGE," #....# ",row+5,col);
			c_put_str(TERM_ORANGE,"  #..#  ",row+6,col);
			c_put_str(TERM_ORANGE,"   ##   ",row+7,col);
			prt(" Orange ",row+8,col);
			break;
		case 2: /* sword */
			c_put_str(TERM_SLATE,"   /\\   ",row,col);
			c_put_str(TERM_SLATE,"   ##   ",row+1,col);
			c_put_str(TERM_SLATE,"   ##   ",row+2,col);
			c_put_str(TERM_SLATE,"   ##   ",row+3,col);
			c_put_str(TERM_SLATE,"   ##   ",row+4,col);
			c_put_str(TERM_SLATE,"   ##   ",row+5,col);
			c_put_str(TERM_UMBER," ###### ",row+6,col);
			c_put_str(TERM_UMBER,"   ##   ",row+7,col);
			prt(" Sword  ",row+8,col);
			break;
		case 3: /* shield */
			c_put_str(TERM_SLATE," ###### ",row,col);
			c_put_str(TERM_SLATE,"#      #",row+1,col);
			c_put_str(TERM_SLATE,"# ++++ #",row+2,col);
			c_put_str(TERM_SLATE,"# +==+ #",row+3,col);
			c_put_str(TERM_SLATE,"#  ++  #",row+4,col);
			c_put_str(TERM_SLATE," #    # ",row+5,col);
			c_put_str(TERM_SLATE,"  #  #  ",row+6,col);
			c_put_str(TERM_SLATE,"   ##   ",row+7,col);
			prt(" Shield ",row+8,col);
			break;
		case 4: /* plum */
			c_put_str(TERM_VIOLET,"   ##   ",row,col);
			c_put_str(TERM_VIOLET," ###### ",row+1,col);
			c_put_str(TERM_VIOLET,"########",row+2,col);
			c_put_str(TERM_VIOLET,"########",row+3,col);
			c_put_str(TERM_VIOLET,"########",row+4,col);
			c_put_str(TERM_VIOLET," ###### ",row+5,col);
			c_put_str(TERM_VIOLET,"  ####  ",row+6,col);
			c_put_str(TERM_VIOLET,"   ##   ",row+7,col);
			prt("  Plum  ",row+8,col);
			break;
		case 5: /* cherry */
			c_put_str(TERM_RED,"      ##",row,col);
			c_put_str(TERM_RED,"   ###  ",row+1,col);
			c_put_str(TERM_RED,"  #..#  ",row+2,col);
			c_put_str(TERM_RED,"  #..#  ",row+3,col);
			c_put_str(TERM_RED," ###### ",row+4,col);
			c_put_str(TERM_RED,"#..##..#",row+5,col);
			c_put_str(TERM_RED,"#..##..#",row+6,col);
			c_put_str(TERM_RED," ##  ## ",row+7,col);
			prt(" Cherry ",row+8,col);
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
	static const char *fruit[6]={"Lemon", "Orange", "Sword", "Shield", "Plum", "Cherry"};
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
		clear_bldg(5, 23);

		/* Set maximum bet */
		if (p_ptr->lev < 10)
			maxbet = (p_ptr->lev * 100);
		else
			maxbet = (p_ptr->lev * 1000);

		/* Get the wager */
		strcpy(out_val, "");
		sprintf(tmp_str,"Your wager (1-%ld) ? ", maxbet);
		get_string (tmp_str,out_val,32);

		/* Strip spaces */
		for (p = out_val; *p == ' '; p++);

		wager = atol(p);

		if (wager > p_ptr->au)
		{
			msg_print("Hey! You don't have the gold - get out of here!");
			msg_print(NULL);
			screen_load();
			return(FALSE);
		}
		else if (wager > maxbet)
		{
			sprintf(tmp_str,"I'll take $%ld of that. Keep the rest.", maxbet);
			msg_print(tmp_str);
			wager = maxbet;
		}
		else if (wager < 1)
		{
			msg_print("Ok, we'll start with $1.");

			wager = 1;
		}
		msg_print(NULL);
		win = FALSE;
		odds = 0;
		oldgold = p_ptr->au;

		sprintf(tmp_str,"Gold before game: %9ld",oldgold);
		prt(tmp_str,20,2);

		sprintf(tmp_str,"Current Wager:    %9ld",wager);
		prt(tmp_str,21,2);

		/* Prevent savefile-scumming of the casino */
		Rand_quick = TRUE;
		Rand_value = time(NULL);

		do
		{
			switch(cmd)
			{
			 case BACT_IN_BETWEEN: /* Game of In-Between */
				c_put_str(TERM_GREEN, "In Between",5,2);
				odds = 3;
				win = FALSE;
				roll1 = randint(10);
				roll2 = randint(10);
				choice = randint(10);
				sprintf(tmp_str,"Black die: %d       Black Die: %d", roll1, roll2);
				prt(tmp_str,8,3);
				sprintf(tmp_str,"Red die: %d", choice);
				prt(tmp_str,11,14);
				if (((choice > roll1) && (choice < roll2)) ||
				    ((choice < roll1) && (choice > roll2)))
					win = TRUE;
				break;
			case BACT_CRAPS:  /* Game of Craps */
				c_put_str(TERM_GREEN, "Craps",5,2);
				win = 3;
				odds = 1;
				roll1 = randint(6);
				roll2 = randint(6);
				roll3 = roll1 +  roll2;
				choice = roll3;
				sprintf(tmp_str,"First roll: %d %d    Total: %d", roll1,
				     roll2, roll3);
				prt(tmp_str,7,5);
				if ((roll3 == 7) || (roll3 == 11))
					win = TRUE;
				else if ((roll3 == 2) || (roll3 == 3) || (roll3 == 12))
					win = FALSE;
				else
					do
					{
						msg_print("Hit any key to roll again");
						msg_print(NULL);
						roll1 = randint(6);
						roll2 = randint(6);
						roll3 = roll1 +  roll2;

						sprintf(tmp_str,"Roll result: %d %d   Total:     %d",
						     roll1, roll2, roll3);
						prt(tmp_str,8,5);
						if (roll3 == choice)
							win = TRUE;
						else if (roll3 == 7)
							win = FALSE;
					} while ((win != TRUE) && (win != FALSE));
				break;

			case BACT_SPIN_WHEEL:  /* Spin the Wheel Game */
				win = FALSE;
				odds = 10;
				c_put_str(TERM_GREEN,"Wheel", 5, 2);
				prt("0  1  2  3  4  5  6  7  8  9", 7, 5);
				prt("--------------------------------", 8, 3);
				strcpy(out_val, "");
				get_string ("Pick a number (1-9): ", out_val, 32);
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
				roll1 = randint(10) - 1;
				sprintf(tmp_str, "The wheel spins to a stop and the winner is %d",
				    roll1);
				prt(tmp_str,13,3);
				prt("", 9, 0);
				prt("*", 9, (3 * roll1 + 5));
				if (roll1 == choice)
					win = TRUE;
				break;

			case BACT_DICE_SLOTS: /* The Dice Slots */
				c_put_str(TERM_GREEN,"Dice Slots",5,2);
				win = FALSE;
				roll1 = randint(6);
				roll2 = randint(6);
				choice = randint(6);
				(void) sprintf(tmp_str, "%s %s %s", fruit[roll1-1], fruit[roll2-1],
				     fruit[choice-1]);
				prt(tmp_str,15,37);
				prt("/--------------------------\\",7,2);
				prt("\\--------------------------/",17,2);
				display_fruit(8,  3, roll1-1);
				display_fruit(8, 12, roll2-1);
				display_fruit(8, 21, choice-1);
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
				prt("YOU WON",16,37);
				p_ptr->au = p_ptr->au + (odds * wager);
				sprintf(tmp_str,"Payoff: %d", odds);
				prt(tmp_str, 17, 37);
			}
			else
			{
				prt("You Lost", 16, 37);
				p_ptr->au = p_ptr->au - wager;
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
				screen_load();
				return(FALSE);
/*                              sprintf(tmp_str,"Current Wager:    %9ld",wager);
				prt(tmp_str, 17, 2); */
			}
		} while ((again == 'y') || (again == 'Y'));

		/* Switch back to complex RNG */
		Rand_quick = FALSE;

		prt("", 18, 37);
		if (p_ptr->au >= oldgold)
			msg_print("You came out a winner! We'll win next time, I'm sure.");
		else
			msg_print("You lost gold! Haha, better head home.");
		msg_print(NULL);
	}
	screen_load();
	return(TRUE);
}


/*
 * inn commands
 * Note that resting for the night was a perfect way to avoid player
 * ghosts in the town *if* you could only make it to the inn in time (-:
 * Now that the ghosts are temporarily disabled in 2.8.X, this function
 * will not be that useful.  I will keep it in the hopes the player
 * ghost code does become a reality again. Does help to avoid filthy urchins.
 * Resting at night is also a quick way to restock stores -KMW-
 */
static bool inn_comm(int cmd)
{
	int dawnval;

	switch(cmd)
	{
		case BACT_FOOD: /* Buy food & drink */
			msg_print("The barkeep gives you some gruel and a beer.");
			msg_print(NULL);
			(void) set_food(PY_FOOD_MAX - 1);
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
					return(FALSE);
				}
				else
				{
					turn = ((turn/50000)+1)*50000;
					p_ptr->chp = p_ptr->mhp;
					set_blind(0);
					set_confused(0);
					p_ptr->stun = 0;
					msg_print("You awake refreshed for the new day.");
					p_ptr->leftbldg = TRUE;
					p_ptr->leaving = TRUE;
				}
			}
			else
			{
				msg_print("The rooms are available only at night.");
				msg_print(NULL);
				return(FALSE);
			}
			break;
		case BACT_RUMORS: /* Listen for rumors */
			{
				char Rumor[90];

				get_rnd_line("rumors.txt", Rumor);
				msg_format("%s", Rumor);
				msg_print(NULL);
				break;
			}
	}
	return(TRUE);
}



/*
 * share gold for thieves
 */
static void share_gold(void)
{
	char tmp_str[80];
	int i;

	i = (p_ptr->lev * 2) * 10;
	sprintf(tmp_str, "You collect %d gold pieces", i);
	msg_print(tmp_str);
	msg_print(NULL);
	p_ptr->au += i;
}


/*
 * Display quest information
 */
static void get_questinfo(int questnum)
{
	int i;
	int old_quest;
	char tmp_str[80];

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
		c_put_str(TERM_YELLOW, quest_text[i], i+8, 0);
	}
}


/*
 * Request a quest from the Lord.
 */
static void castle_quest(void)
{
	char            tmp_str[80];
	int             q_index = 0;
	monster_race    *r_ptr;
	quest_type      *q_ptr;
	cptr            name;


	clear_bldg(7,18);

	/* Current quest of the building */
	q_index = cave[py][px].special;

	/* Is there a quest available at the building? */
	if (!q_index)
	{
		put_str("I don't have a quest for you at the moment.",8,0);
		return;
	}

	q_ptr = &quest[q_index];
	
	/* Quest is completed */
	if (q_ptr->status == QUEST_STATUS_COMPLETED)
	{
		/* Rewarded quest */
		q_ptr->status = QUEST_STATUS_REWARDED;

		get_questinfo(q_index);

		reinit_wilderness = TRUE;
	}

	/* Quest is still unfinished */
	else if (q_ptr->status == QUEST_STATUS_TAKEN)
	{
		put_str("You have not completed your current quest yet!    ",8,0);
		put_str("Use CTRL-Q to check the status of your quest.",9,0);
		put_str("Return when you have completed your quest.",12,0);
	}
	/* No quest yet */
	else if (q_ptr->status == QUEST_STATUS_UNTAKEN)
	{
		get_questinfo(q_index);
	
		if (p_ptr->lev < (q_ptr->level/2))
		{
		clear_bldg (5, 19);
		put_str("You are too inexperienced to go off on this quest!", 8, 0);
		(void) sprintf(tmp_str,"Return when you have reached level %d.", q_ptr->level/2);
		put_str(tmp_str, 9, 0);
		return;
		}
	
		else q_ptr->status = QUEST_STATUS_TAKEN;

		reinit_wilderness = TRUE;

		/* Assign a new quest */
		if (q_ptr->type == QUEST_TYPE_KILL_ANY_LEVEL)
		{
			if (q_ptr->r_idx == 0)
			{
				/* Random monster at least 5 - 10 levels out of deep */
				q_ptr->r_idx = get_mon_num(q_ptr->level) + 4 + randint(6);
			}

			r_ptr = &r_info[q_ptr->r_idx];

			while ((r_ptr->flags1 & (RF1_UNIQUE)) ||
			  (r_ptr->rarity != 1))
			{
				q_ptr->r_idx = get_mon_num(q_ptr->level) + 4 + randint(6);
				r_ptr = &r_info[q_ptr->r_idx];
			}

			if (q_ptr->max_num == 0)
			{
				/* Random monster number */
				if (randint(10) > 7)
					q_ptr->max_num = 1;
				else
					q_ptr->max_num = randint(3) + 1;
			}

			q_ptr->cur_num = 0;
			name = (r_name + r_ptr->name);
			sprintf(tmp_str,"Your quest: kill %d %s",
				q_ptr->max_num, name);
			msg_print(tmp_str);
			msg_print(NULL);
		}
	}
}


/*
 * Greet the lord of the castle
 */
static void castle_greet(void)
{
#if 0
	int increment, j, oldlevel;
	char tmp_str[80];

	increment = ((p_ptr->lev - 1) / 5);
	if (increment < 1)
	{
		(void) sprintf(tmp_str,"Ah, a young adventurer, return when: %s (Level %d)",
		player_title[p_ptr->pclass][1], 6);
		msg_print(tmp_str);
		msg_print(NULL);
	}
	else if ((increment >= 5) && (p_ptr->rewards[BACT_GREET]))
	{
		msg_print("Your greeting is returned with reverance and respect");
	}
	else if (p_ptr->rewards[increment + 9])
	{
		(void) sprintf(tmp_str,"You have been rewarded, return when %s (Level %d)",
		player_title[p_ptr->pclass][increment+1], ((increment*5)+6));
		msg_print(tmp_str);
		msg_print(NULL);
	}
	else
	{ /* set last reward that hasn't been given to TRUE */
		for (j=10;j<20;j++)
		{
			if (p_ptr->rewards[j] == 0)
			{
				p_ptr->rewards[j] = 1;
				break;
			}
		}

		if (j == 10)
		{
			msg_print("Well done! Please take up residence in the house down the street.");
			msg_print(NULL);
		}
		else if (j == 11)
		{
			msg_print("Very good! The weaponsmaster will be able to help you now.");
			msg_print(NULL);
		}
		else if (j == 12)
		{
			msg_print("You are proving yourself worthy. You may visit the Beastmaster.");
			msg_print(NULL);
		}
		else
		{
			msg_print("A great gift awaits you outside!");
			msg_print(NULL);
			oldlevel = object_level;
			object_level = p_ptr->lev * 4;
			acquirement(py,px,1,0,1);
			object_level = oldlevel;
		}
	}
#endif
}


# if 0

/*
 * greet_reward
 */
static void greet_reward(int gclass)
{
	byte tval, sval;
	int randitem = randint(5);

	tval = 0;
	sval = 0;

	switch(gclass)
	{
		case CLASS_ROGUE:
		case CLASS_WARRIOR:
		case CLASS_PALADIN:
		case CLASS_RANGER:
			switch(randitem)
			{
				case 1:
					tval = TV_BOW;
					sval = SV_LONG_BOW;
					break;
				case 2:
					tval = TV_BOOTS;
					sval = SV_PAIR_OF_METAL_SHOD_BOOTS;
					break;
				case 3:
					tval = TV_HARD_ARMOR;
					sval = SV_MITHRIL_CHAIN_MAIL;
					break;
				case 4:
					tval = TV_HELM;
					sval = SV_STEEL_HELM;
					break;
				case 5:
					tval = TV_SWORD;
					sval = SV_BROAD_SWORD;
					break;
			}
			break;

		case CLASS_MAGE:
		case CLASS_PRIEST:
			switch(randitem)
			{
				case 1:
					tval = TV_ROD;
					sval = 23 + randint(4);
					break;
				case 2:
					tval = TV_RING;
					sval = SV_RING_PROTECTION;
					break;
				case 3:
					tval = TV_AMULET;
					sval = SV_AMULET_THE_MAGI;
					break;
				case 4:
					tval = TV_CLOAK;
					sval = SV_SHADOW_CLOAK;
					break;
				case 5:
					tval = TV_STAFF;
					sval = SV_STAFF_POWER;
					break;
			}
	}

	put_reward(tval,sval,(p_ptr->lev*2));
}

#endif


/*
 * greet_char
 */
static void greet_char(void)
{
#if 0
	int increment, j;
	char tmp_str[80];

	increment = (((p_ptr->lev - 1) / 5)/2);
	if (p_ptr->lev == 50)
		increment = 5;

	if (increment < 1)
	{
		(void) sprintf(tmp_str,"You are young yet, return when: %s (Level %d)",
		    player_title[p_ptr->pclass][2], 11);
		msg_print(tmp_str);
		msg_print(NULL);
	}
	else if ((increment == 5) && (p_ptr->rewards[increment - 1]))
	{
		msg_print("Your greeting is returned with reverence and respect");
	}
	else if (p_ptr->rewards[increment - 1])
	{
		if (increment == 4)
		{
			(void) sprintf(tmp_str,"You have been rewarded, return when %s (Level %d)",
			    player_title[p_ptr->pclass][9], 50);
		}
		else
		{
			(void) sprintf(tmp_str,"You have been rewarded, return when %s (Level %d)",
			    player_title[p_ptr->pclass][(increment+1) * 2], (((increment+1)*10)+1));
			msg_print(tmp_str);
			msg_print(NULL);
		}
	}
	else
	{
		for (j=0;j<10;j++)
		{
			if (p_ptr->rewards[j] == FALSE)
			{
				p_ptr->rewards[j] = TRUE;
				break;
			}
		}
		msg_print("Take this gift as a reward for your diligence");
		msg_print(NULL);
		greet_reward(p_ptr->pclass);
	}
#endif
}


/*
 * Displaying town history -KMW-
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


/*
 * compare_weapon_aux2 -KMW-
 */
static void compare_weapon_aux2(object_type *o_ptr, int numblows, int r, int c, int mult, char attr[80], u32b f1, u32b f2, u32b f3, byte color)
{
	char tmp_str[80];

	c_put_str(color,attr,r,c);
	sprintf(tmp_str,"Attack: %d-%d damage, %.1f avg",
	    mult * (numblows * (o_ptr->dd + o_ptr->to_d + p_ptr->to_d)),
	    mult * (numblows * (o_ptr->ds * o_ptr->dd + o_ptr->to_d + p_ptr->to_d)),
	    mult * (numblows * (average_dice(o_ptr->ds, o_ptr->dd) + o_ptr->to_d + p_ptr->to_d)));
	put_str(tmp_str,r,c+8);
	r++;
}


/*
 * compare_weapon_aux1 -KMW-
 */
static void compare_weapon_aux1(object_type *o_ptr, int col, int r)
{
	u32b f1, f2, f3;

	object_flags(o_ptr, &f1, &f2, &f3);

	if (f1 & TR1_SLAY_ANIMAL) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 2, "Animals:", f1, f2, f3, TERM_YELLOW);
	if (f1 & TR1_SLAY_EVIL)   compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 2, "Evil:", f1, f2, f3, TERM_YELLOW);
	if (f1 & TR1_SLAY_UNDEAD) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Undead:", f1, f2, f3, TERM_YELLOW);
	if (f1 & TR1_SLAY_DEMON)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Demons:", f1, f2, f3, TERM_YELLOW);
	if (f1 & TR1_SLAY_ORC)    compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Orcs:", f1, f2, f3, TERM_YELLOW);
	if (f1 & TR1_SLAY_TROLL)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Trolls:", f1, f2, f3, TERM_YELLOW);
	if (f1 & TR1_SLAY_GIANT)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Giants:", f1, f2, f3, TERM_YELLOW);
	if (f1 & TR1_SLAY_DRAGON) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Dragons:", f1, f2, f3, TERM_YELLOW);
	if (f1 & TR1_KILL_DRAGON) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 5, "Dragons:", f1, f2, f3, TERM_YELLOW);
	if (f1 & TR1_BRAND_ACID)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Acid:", f1, f2, f3, TERM_RED);
	if (f1 & TR1_BRAND_ELEC)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Elec:", f1, f2, f3, TERM_RED); 
	if (f1 & TR1_BRAND_FIRE)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Fire:", f1, f2, f3, TERM_RED); 
	if (f1 & TR1_BRAND_COLD)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Cold:", f1, f2, f3, TERM_RED);
	if (f1 & TR1_BRAND_POIS)  compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Poison:", f1, f2, f3, TERM_RED);
}


/*
 * list_weapon -KMW-
 */
static void list_weapon(object_type *o_ptr, int row, int col)
{
	char o_name[80];
	char tmp_str[80];

	object_desc(o_name, o_ptr, TRUE, 0);
	c_put_str(TERM_YELLOW,o_name,row,col);
	sprintf(tmp_str,"To Hit: %d   To Damage: %d",o_ptr->to_h, o_ptr->to_d);
	put_str(tmp_str,row+1,col);
	sprintf(tmp_str,"Dice: %d   Sides: %d",o_ptr->dd, o_ptr->ds);
	put_str(tmp_str,row+2,col);
	sprintf(tmp_str,"Number of Blows: %d", p_ptr->num_blow);
	put_str(tmp_str,row+3,col);
	c_put_str(TERM_YELLOW, "Possible Damage:",row+5,col);
	sprintf(tmp_str,"One Strike: %d-%d damage, %.1f avg",
	       o_ptr->dd + o_ptr->to_d + p_ptr->to_d,
	       o_ptr->ds * o_ptr->dd + o_ptr->to_d + p_ptr->to_d,
	       average_dice(o_ptr->dd, o_ptr->ds) + o_ptr->to_d + p_ptr->to_d);
	put_str(tmp_str,row+6,col+1);
	sprintf(tmp_str,"One Attack: %d-%d damage, %.1f avg",
	       p_ptr->num_blow * (o_ptr->dd + o_ptr->to_d + p_ptr->to_d),
	       p_ptr->num_blow * (o_ptr->ds * o_ptr->dd + o_ptr->to_d + p_ptr->to_d),
	       p_ptr->num_blow * (average_dice(o_ptr->dd, o_ptr->ds) + o_ptr->to_d + p_ptr->to_d));
	put_str(tmp_str,row+7,col+1);
}

/*
 * compare_weapons -KMW-
 */
static bool compare_weapons(void)
{
	int item, item2, i;
	object_type *o1_ptr, *o2_ptr, *orig_ptr;
	object_type *i_ptr;
	cptr q, s;

	clear_bldg(6,18);

	o1_ptr = NULL; o2_ptr = NULL; i_ptr = NULL;

	/* Store copy of original wielded weapon in pack slot */
	i_ptr = &inventory[INVEN_WIELD];
	orig_ptr = &inventory[INVEN_PACK];
	object_copy(orig_ptr, i_ptr);

	i = 6;
	/* Get an item */
	q = "What is your first weapon? ";
	s = "You have nothing to compare.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN)))
	{
		inven_item_increase(INVEN_PACK, -1);
		inven_item_optimize(INVEN_PACK);
		return(FALSE);
	}

	/* Get the item (in the pack) */
	if (item >= 0)
		o1_ptr = &inventory[item];

	if ((o1_ptr->tval < TV_BOW) || (o1_ptr->tval > TV_SWORD))
	{
		msg_print("Not a weapon! Try again.");
		msg_print(NULL);
		inven_item_increase(INVEN_PACK, -1);
		inven_item_optimize(INVEN_PACK);
		return(FALSE);
	}

	/* Get an item */
	q = "What is your second weapon? ";
	s = "You have nothing to compare.";
	if (!get_item(&item2, q, s, (USE_EQUIP | USE_INVEN)))
	{
		inven_item_increase(INVEN_PACK, -1);
		inven_item_optimize(INVEN_PACK);
		return(FALSE);
	}

	/* Get the item (in the pack) */
	if (item2 >= 0)
		o2_ptr = &inventory[item2];

	if ((o2_ptr->tval < TV_BOW) || (o2_ptr->tval > TV_SWORD))
	{
		msg_print("Not a weapon! Try again.");
		msg_print(NULL);
		inven_item_increase(INVEN_PACK, -1);
		inven_item_optimize(INVEN_PACK);
		return(FALSE);
	}

	put_str("Based on your current abilities, here is what your weapons will do", 4, 2);

	i_ptr = &inventory[INVEN_WIELD];
	object_copy(i_ptr, o1_ptr);
	calc_bonuses();

	list_weapon(o1_ptr,i,2);
	compare_weapon_aux1(o1_ptr, 2, i+8);

	i_ptr = &inventory[INVEN_WIELD];
	if (item2 == INVEN_WIELD)
		object_copy(i_ptr, orig_ptr);
	else
		object_copy(i_ptr, o2_ptr);
	calc_bonuses();

	list_weapon(o2_ptr,i,40);
	compare_weapon_aux1(o2_ptr, 40, i+8);

	i_ptr = &inventory[INVEN_WIELD];
	object_copy(i_ptr, orig_ptr);
	calc_bonuses();

	inven_item_increase(INVEN_PACK, -1);
	inven_item_optimize(INVEN_PACK);

	put_str("(Only highest damage applies per monster. Special damage not cumulative)",20,0);

	return(TRUE);
}


/*
 * general all-purpose fixing routine for items from building personnel
 * sharpen arrows, repair armor, repair weapon
 * -KMW-
 */
static bool fix_item(int istart, int iend, int ispecific, bool iac, int ireward, bool set_reward)
{
	int i;
	int j = 9;
	int maxenchant = (p_ptr->lev / 5);
	object_type *o_ptr;
	char out_val[80], tmp_str[80];
	bool repaired = FALSE;

	if (set_reward && p_ptr->rewards[ireward])
	{
		msg_print("You already have been rewarded today.");
		msg_print(NULL);

		return (FALSE);
	}

	clear_bldg(5,18);
	sprintf(tmp_str,"  Based on your skill, we can improve up to +%d", maxenchant);
	prt(tmp_str, 5, 0);
	prt("Status", 7, 30);

	for (i = istart; i <= iend; i++)
	{
		o_ptr = &inventory[i];
		if (ispecific > 0)
		{
			if ((o_ptr->tval != ispecific) ||
			   (ispecific == TV_BOW && o_ptr->sval != SV_SHORT_BOW &&
			    o_ptr->sval != SV_LONG_BOW))
				continue;
		}

		if (o_ptr->tval)
		{
			object_desc(tmp_str, o_ptr, FALSE, 1);

			if ((o_ptr->art_name) || ((o_ptr->name1) &&
			    (o_ptr->ident & 0x08)))
				sprintf(out_val, "%-40s: beyond our skills!", tmp_str);
			else if (o_ptr->name1)
				sprintf(out_val, "%-40s: in fine condition", tmp_str);
			else
			{
				if ((iac) && (o_ptr->to_a <= -3))
				{
					sprintf(out_val, "%-40s: beyond repair, buy a new one", tmp_str);
				}
				else if ((iac) && (o_ptr->to_a < maxenchant))
				{
					o_ptr->to_a++;
					sprintf(out_val, "%-40s: polished -> (%d)", tmp_str, o_ptr->to_a);
					repaired = TRUE;
				}
				else if ((!iac) && ((o_ptr->to_h <= -3) || (o_ptr->to_d <= -3)))
				{
					sprintf(out_val, "%-40s: beyond repair, buy a new one", tmp_str);
				}
				/* Sharpen a weapon */
				else if ((!iac) && ((o_ptr->to_h  < maxenchant) ||
					    (o_ptr->to_d < maxenchant)))
				{
					if (o_ptr->to_h  < maxenchant)
						o_ptr->to_h++;
					if (o_ptr->to_d < maxenchant)
						o_ptr->to_d++;
					sprintf(out_val, "%-40s: sharpened -> (%d,%d)", tmp_str,
					    o_ptr->to_h, o_ptr->to_d);
					repaired = TRUE;
				}
				else
					sprintf(out_val, "%-40s: in fine condition", tmp_str);
			}
			prt(out_val,j++,0);
		}
	}

	if (!repaired)
	{
		msg_print("You don't have anything appropriate.");
		msg_print(NULL);
	}
	else
	{
		if (set_reward)
			p_ptr->rewards[ireward] = TRUE;

		msg_print("Press the spacebar to continue");
		msg_print(NULL);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
	}
	clear_bldg(5,18);

	return (repaired);
}


/*
 * Research Item
 */
static bool research_item(void)
{
	clear_bldg(5,18);
	identify_fully();
	return (TRUE);
}


/*
 * Execute a building command
 */
static void bldg_process_command(building_type *bldg, int i)
{

	int bact = bldg->actions[i];
	int bcost;
	bool paid = FALSE;
	bool set_reward = FALSE;
	int amt;

	if (is_owner(bldg))
		bcost = bldg->member_costs[i];
	else
		bcost = bldg->other_costs[i];

	/* action restrictions */
	if (((bldg->action_restr[i] == 1) && (!is_member(bldg))) ||
	    ((bldg->action_restr[i] == 2) && (!is_owner(bldg))))
	{
		msg_print("You have no right to choose that!");
		msg_print(NULL);
		return;
	}

	/* check gold */
	if (((bldg->member_costs[i] > p_ptr->au) && (is_owner(bldg))) ||
	    ((bldg->other_costs[i] > p_ptr->au) && (!is_owner(bldg))))
	{
		msg_print("You do not have the gold!");
		msg_print(NULL);
		return;
	}

	if (!bcost) set_reward = TRUE;

	switch (bact)
	{
		case BACT_RESEARCH_ITEM:
			paid = research_item();
			break;

		case BACT_TOWN_HISTORY:
			town_history();
			break;

		case BACT_RACE_LEGENDS:
			race_legends();
			break;

		case BACT_GREET_KING:
			castle_greet();
			break;

		case BACT_QUEST:
			castle_quest();
			break;

		case BACT_KING_LEGENDS:
		case BACT_ARENA_LEGENDS:
		case BACT_LEGENDS:
			show_highclass(building_loc);
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

		case BACT_COMPARE_WEAPONS:
			paid = compare_weapons();
			break;

		case BACT_GREET:
			greet_char();
			break;

		case BACT_ENCHANT_WEAPON:
			paid = fix_item(INVEN_WIELD, INVEN_WIELD, 0, FALSE, BACT_ENCHANT_WEAPON, set_reward);
			break;

		case BACT_ENCHANT_ARMOR:
			paid = fix_item(INVEN_BODY, INVEN_FEET, 0, TRUE, BACT_ENCHANT_ARMOR, set_reward);
			break;

		case BACT_RECHARGE: /* needs work */
			if (!p_ptr->rewards[BACT_RECHARGE])
			{
				if (recharge(80))
				{
					p_ptr->rewards[BACT_RECHARGE] = TRUE;
					paid = TRUE;
				}
			else
			{
				msg_print("Sorry, we can only recharge one item per day.");
				msg_print(NULL);
			}
			}
			break;

		case BACT_IDENTS: /* needs work */
			identify_pack();
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
			paid = fix_item(0, INVEN_WIELD, TV_ARROW, FALSE, BACT_ENCHANT_ARROWS, set_reward);
			break;

		case BACT_ENCHANT_BOW:
			paid = fix_item(INVEN_BOW, INVEN_BOW, TV_BOW, FALSE, BACT_ENCHANT_BOW, set_reward);
			break;

		case BACT_RECALL:
			p_ptr->word_recall = 1;
			msg_print("The air about you becomes charged...");
			paid = TRUE;
			break;

		case BACT_TELEPORT_LEVEL:
			amt = get_quantity("Teleport to which level? ", 98);
			if (amt > 0)
			{
				p_ptr->word_recall = 1;
				p_ptr->max_dlv = amt;
				msg_print("The air about you becomes charged...");
				paid = TRUE;
			}
			break;

		case BACT_GAIN_MUTATION:
			(void)gain_random_mutation(0);
			break;

		case BACT_REMOVE_MUTATIONS:
		  if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
		    {
			msg_print("All of your mutations are removed.");
			p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
		    }
		    break;
		case BACT_BRAND_CHAOS_WEAPON:
		    brand_weapon(EGO_CHAOTIC, TRUE);
		    break;
		case BACT_BRAND_SLAY_WEAPON:
		    brand_weapon(rand_range(EGO_SLAY_ANIMAL, EGO_KILL_DRAGON), TRUE);
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
	if (!(cave[py][px].feat == FEAT_QUEST_ENTER))
	{
		msg_print("You see no quest level here.");
		return;
	}
	else
	{
		/* Player enters a new quest */
		p_ptr->oldpy = 0;
		p_ptr->oldpx = 0;

		leaving_quest = p_ptr->inside_quest;
		p_ptr->inside_quest = cave[py][px].special;
		dun_level = 1;
		p_ptr->leftbldg = TRUE;
		p_ptr->leaving = TRUE;
	}
}


/*
 * Do building commands
 */
void do_cmd_bldg(void)
{
	int             i,which;
	char            command;
	bool            validcmd;
	building_type   *bldg;

	if (!((cave[py][px].feat >= FEAT_BLDG_HEAD) &&
		  (cave[py][px].feat <= FEAT_BLDG_TAIL)))
	{

		msg_print("You see no building here.");
		return;
	}

	which = (cave[py][px].feat - FEAT_BLDG_HEAD);
	building_loc = which;

	bldg = &building[which];

	if ((which == 2) && (p_ptr->inside_arena) && (!p_ptr->exit_bldg))
	{
		prt("The gates are closed.  The monster awaits!",0,0);
		return;
	}
	else if ((which == 2) && (p_ptr->inside_arena))
	{
		p_ptr->leaving = TRUE;
		p_ptr->inside_arena = FALSE;
	}
	else
	{
		p_ptr->oldpy = py;
		p_ptr->oldpx = px;
	}

	/* Forget the lite */
	forget_lite();

	/* Forget the view */
	forget_view();

	/* Hack -- Increase "icky" depth */
	character_icky++;

	command_arg = 0;
	command_rep = 0;
	command_new = 0;

	show_building(bldg);
	leave_bldg = FALSE;

	while (!leave_bldg)
	{
		validcmd = FALSE;
		prt("",1,0);
		command = inkey();

		if (command == ESCAPE)
		{
			leave_bldg = TRUE;
			p_ptr->inside_arena = FALSE;
			break;
		}

		for (i = 0; i < 6; i++)
		{
			if (bldg->letters[i])
			{
				if (bldg->letters[i] == command)
				{
					validcmd = TRUE;
					break;
				}
			}
		}

		if (validcmd)
			bldg_process_command(bldg, i);
		else
			msg_print("invalid command");

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();
	}

	/* Flush messages XXX XXX XXX */
	msg_print(NULL);

	/* Reinit wilderness to activate quests ... */
	if (reinit_wilderness)
		p_ptr->leaving = TRUE;

	/* Hack -- Decrease "icky" depth */
	character_icky--;

	/* Clear the screen */
	Term_clear();

	/* Update the visuals */
	p_ptr->update |= (PU_VIEW | PU_MONSTERS | PU_BONUS);

	/* Redraw entire screen */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_EQUIPPY | PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}


/* Array of places to find an inscription */
static cptr find_quest[] =
{
	"You find the following inscription in the floor",
	"You see a message inscribed in the wall",
	"There is a sign saying",
	"Something is writen on the staircase",
	"You find a scroll with the following message",
};


/*
 * Discover quest
 */
void quest_discovery(int q_idx)
{
	quest_type      *q_ptr = &quest[q_idx];
	monster_race    *r_ptr = &r_info[q_ptr->r_idx];
	int             q_num = q_ptr->max_num;
	char            name[80];

	/* No quest index */
	if (!q_idx) return;

	strcpy(name, (r_name + r_ptr->name));

	msg_print(find_quest[rand_range(0,4)]);
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

