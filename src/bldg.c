/* File: bldg.c */

/*
 * Purpose: Building commands
 * Created by Ken Wigle for Kangband - a variant of Angband 2.8.2
 * -KMW-
 */

#include "angband.h"

/* remember building location */
static int building = 0;

/*
 * Clear the building information
 */
void clear_bldg(int min_row, int max_row)
{
	int   i;

	for(i = min_row;i <= max_row;i++)
		prt("",i,0);
}


/*
 * Places a building reward at the doorstep for the player -KMW-
 */
void put_reward(byte thetval, byte thesval, int dunlevel)
{
	object_type *q_ptr, forge;
	int i, choice;

	choice = 0;

	for(i=1; i < MAX_K_IDX; i++)
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
	drop_near(q_ptr, -1, p_ptr->py, p_ptr->px);
}


/*
 * Displays building
 */
static void display_bldg(int bldg_num)
{
	Term_clear();
	switch (bldg_num)
	{
		case 0:
			put_str("Astinus (Human?)                        Library", 3, 10);
			prt("You may:", 21, 0);
			prt(" a) Research item (2000gp)     h) Town history",22,0);
			prt(" l) Race legends               ESC) Exit building", 23, 0);
			break;
		case 1:
			put_str("Denethor (Human)                        Castle", 3, 10);
			prt("You may:", 21, 0);
			prt(" g) Greet King                 l) Look at busts of Kings", 22, 0);
			prt(" q) Request quest              ESC) Exit building", 23, 0);
			break;
		case 2:
			put_str("Arack (Dwarf)                           Arena", 3, 10);
			prt("You may:", 20, 0);
			prt(" p) Read poster                r) Arena Rules", 21, 0);
			prt(" a) Enter arena                l) Look at plaque", 22, 0);
			prt(" ESC) Exit building", 23, 0);
			break;
		case 3:
			put_str("Silk (Human)                            Gambling House", 3, 10);
			prt("You may:", 20, 0);
			prt(" b) In-Between                 r) Game rules", 21, 0);
			prt(" c) Play craps                 s) Spin the wheel", 22, 0);
			prt(" d) Play dice slots            ESC) Exit building", 23, 0);
			break;
		case 4:
			put_str("Otik (Human)                            Inn", 3, 10);
			prt("You may:", 20, 0);
			prt(" r) Rest for the night (20gp)    f) Buy food and drink (1gp)", 21, 0);
/*			prt(" u) Listen for rumors          ESC) Exit building", 22, 0);	*/
			prt(" ESC) Exit building", 22, 0);
			break;
		case 5:
			put_str("Lorien (Human)                          Beastmaster", 3, 10);
			prt("You may:", 20, 0);
			prt(" r) Research Monster (1000gp)  ESC) Exit building", 21, 0);
			prt(" ESC) Exit building", 22, 0);
			break;
		case 6:
			put_str("Suiyan (Human)                          Weaponsmaster", 3, 10);
			prt("You may:", 20, 0);
			prt(" c) Compare Weapons (1000gp)   ESC) Exit building", 21, 0);
			prt(" ESC) Exit building", 22, 0);
			break;
		case 10:
			put_str("Barak (Human)                           Fighter's Hall", 3, 10);
			prt("You may:", 20, 0);
			prt(" g) Greet Lord                 l) Look at plaque", 21, 0);
			if (p_ptr->pclass == CLASS_WARRIOR)
				prt(" r) Repair weapon              a) Repair armor",22,0);
			else
				prt(" r) Repair weapon (1000gp)     a) Repair armor (1000gp)",22,0);
			prt(" ESC) Exit building", 23, 0);
			break;
		case 11:
			put_str("Par-Salian (Human)                      Tower of Sorcery",3, 10);
			prt("You may:", 20, 0);
			prt(" g) Greet Wizard Lord          l) Look at spires", 21, 0);
			if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_ILLUSIONIST))
				prt(" r) Recharge item              p) Identify possessions",22,0);
			else
				prt(" r) Recharge item              p) Identify possessions (1000gp)",22,0);
			prt(" z) Learn spells               ESC) Exit building", 23, 0);
			break;
		case 12:
			put_str("Crysania (Human)                        Inner Temple", 3, 10);
			prt("You may:", 20, 0);
			prt(" g) Greet Priest               l) Look at busts", 21, 0);
			if (p_ptr->pclass == CLASS_PRIEST)
				prt(" h) Healing prayer             r) Restoration",22,0);
			else
				prt(" h) Healing prayer (1000gp)    r) Restoration",22,0);
			prt(" z) Learn prayers              ESC) Exit building", 23, 0);
			break;
		case 13:
			put_str("Javelin (Human)                         House of Thieves",3, 10);
			prt("You may:", 20, 0);
			prt(" g) Greet Master Thief         l) Look at wall", 21, 0);
			prt(" s) Get share of stolen gold   r) Rest for the night",22,0);
			prt(" p) Identify possessions       ESC) Exit building", 23, 0);
			break;
		case 14:
			put_str("Pig-faced William (Human)               Ranger's Tavern", 3, 10);
			prt("You may:", 20, 0);
			prt(" g) Greet High Ranger          l) Look at plaque", 21, 0);
			if (p_ptr->pclass == CLASS_RANGER)
				prt(" a) Sharpen arrows             b) Restring bow", 22,0);
			else
				prt(" a) Sharpen arrows (1000gp)    b) Restring bow (1000gp)", 22,0);
			prt("ESC) Exit building", 23, 0);
			break;
		case 15:
			put_str("Lan (Human)                             Order of Paladins",3, 10);
			prt("You may:", 20, 0);
			prt(" g) Greet Warder               l) Look at plaque", 21, 0);
			if (p_ptr->pclass == CLASS_PALADIN)
				prt(" a) Repair armor               h) See healers",22,0);
			else
				prt(" a) Repair armor (1000gp)      h) See healers (1000gp)",22,0);
			prt(" ESC) Exit building", 23, 0);
			break;
		case 16:
			put_str("Kenault (Elf)                           Tower of Illusion",3, 10);
			prt("You may:", 20, 0);
			prt(" g) Greet Shadow Lord          l) Look at spires", 21, 0);
			if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_ILLUSIONIST))
				prt(" r) Recharge item              p) Identify possessions",22,0);
			else
				prt(" r) Recharge item              p) Identify possessions (1000gp)",22,0);
			prt(" z) Learn spells               ESC) Exit building", 23, 0);
			break;
	}
}


/*
 * arena commands
 */
void arena_comm(char cmd)
{
	char tmp_str[80];
	monster_race *r_ptr;
	cptr name;

	switch(cmd)
	{
		case 'a':
			if (p_ptr->arena_number == MAX_ARENA_MONS) {
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
			} else if (p_ptr->arena_number > MAX_ARENA_MONS) {
				msg_print("You enter the arena briefly and bask in your glory.");
				msg_print(NULL);
			} else {
				p_ptr->leftbldg = TRUE;
				p_ptr->inside_special = 1;
				p_ptr->exit_bldg = FALSE;
				p_ptr->leaving = TRUE;
			}
			break;
		case 'p':
			if (p_ptr->arena_number == MAX_ARENA_MONS) 
				msg_print("You are victorious. Enter the arena for the ceremony.");
			else if (p_ptr->arena_number > MAX_ARENA_MONS)
				msg_print("You have won against all foes.");
			else {
				r_ptr = &r_info[arena_monsters[p_ptr->arena_number]];
				name = (r_name + r_ptr->name);
				(void) sprintf(tmp_str,"Do I hear any challenges against: %s", name);
				msg_print(tmp_str);
				msg_print(NULL);
			}
			break;
		case 'r':
			clear_bldg(5,19);
			prt("               Rules of the Arena", 5, 0);
			prt("The Arena is a contest pitting the best against each other.", 7, 0); 
			prt("To ensure a good contest, note the following:", 8, 0);
			prt(" 1) NO MAGIC - No potions, wands, staffs, etc.", 10, 0);
			prt(" 1) NO THROWS- This is head-to-head.", 11, 0);
			prt("If fortunate, you will face the Minotaur, most feared of all",13,0);
			prt("in the arena.  Defeat him and claim the prize of 10,000 gold pieces.",14,0);
			msg_print("Press the spacebar to continue");
			msg_print(NULL);
			clear_bldg(5,19);
			break;
	}
}


/*
 * display fruit for dice slots
 */
void display_fruit(int row, int col, int fruit)
{
	switch(fruit) {
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
void gamble_comm(char cmd)
{
	int roll1, roll2, roll3, choice, odds, win;
	s32b wager;
	s32b maxbet;
	s32b oldgold;
	static const char *fruit[6]={"Lemon", "Orange", "Sword", "Shield", "Plum", "Cherry"};
	char out_val[160], tmp_str[80], again;
	cptr p;

	if (cmd == 'r') {
		clear_bldg(5,19);
		prt("               Gambling Rules", 5, 0);
		prt("Between : Three 18-sided dice rolled; 2 black, 1 red. The red",7,0);
		prt("       die must be between both black to win. If the red die.",8,0);
		prt("       matches a black die, you lose. Pays 3 to 1",9,0);
		prt("Craps: Two dice are rolled. On first roll, a 7 or 11 wins. A",10,0);
		prt("       2,3 or 12 loses. Otherwise roll until the first roll",11,0);
		prt("       is matched (win) or a 7 is rolled (loss). Pays 2 to 1",12,0);
		prt("Wheel: Pick a number from 0-9. If the number shows on wheel",13, 0);
		prt("       after it stops spinning, you win. Pays 10 to 1",14,0);
		prt(" ",15,0);
		msg_print("Press the spacebar to continue");
		msg_print(NULL);
		prt("Slots: Three dice rolled. Matches win gold. Numbers are:",7,0);
		prt("       1=Lemon, 2=Orange, 3=Sword, 4=Shield, 5=Plum, 6=Cherry", 8, 0);
		prt("       Payoffs are as follows:",9,0);
		prt("       Cherry Cherry Lemon  2-1   Cherry Cherry Orange  3-1",10,0);
		prt("       Cherry Cherry Sword  4-1   Cherry Cherry Shield  5-1",11,0);
		prt("       Cherry Cherry Plum   6-1",12,0);
		prt("       Lemon Lemon Lemon    4-1   Orange Orange Orange 16-1",13,0);
		prt("       Sword Sword Sword    6-1   Shield Shield Shield 25-1",14,0);
		prt("       Plum Plum Plum       9-1   Cherry Cherry Cherry 36-1",15,0);
		msg_print("Press the spacebar to continue");
		msg_print(NULL);
	} else {
		clear_bldg(5,23);
		if (p_ptr->lev < 10)
			maxbet = (p_ptr->lev * 100);
		else
			maxbet = (p_ptr->lev * 1000);
		strcpy(out_val, "");
		sprintf(tmp_str,"Your wager (1-%ld) ? ", maxbet);
		get_string (tmp_str,out_val,32);
		for (p=out_val;*p == ' '; p++);
		wager = atol(p);
		if (wager > maxbet) {
			sprintf(tmp_str,"I'll take $%ld of that. Keep the rest.", maxbet);
			msg_print(tmp_str);
			wager = maxbet;
		} else if (wager < 1) {
			msg_print("Ok, we'll start with $1.");
			wager = 1;
		}
		if (wager > p_ptr->au) {
			msg_print(NULL);
			msg_print("Hey! You don't have the gold - wager is zip");
			wager = 0;
		}
		msg_print(NULL);
		win = FALSE;
		odds = 0;
		oldgold = p_ptr->au;
		(void) sprintf(tmp_str,"Gold before game: %9ld",oldgold);
		prt(tmp_str,20,2);
		(void) sprintf(tmp_str,"Current Wager:    %9ld",wager);
		prt(tmp_str,21,2);
		do {
			switch(cmd) {
			 case 'b': /* Game of In-Between */
				prt("In Between",5,2);
				odds = 3;
				win = FALSE;
				roll1 = randint(18);
				roll2 = randint(18);
				choice = randint(18);
				(void) sprintf(tmp_str,"Black die: %d       Black Die: %d", roll1, roll2);
				prt(tmp_str,8,3);
				(void) sprintf(tmp_str,"Red die: %d", choice);
				prt(tmp_str,11,14);
				if (((choice > roll1) && (choice < roll2)) || ((choice < roll1)
				     && (choice > roll2)))
					win = TRUE;
				break;

			case 'c':  /* Game of Craps */
				prt("Craps",5,2);
				win = 3;
				odds = 1;
				roll1 = randint(6);
				roll2 = randint(6);
				roll3 = roll1 +  roll2;
				choice = roll3;
				(void) sprintf(tmp_str,"First roll: %d %d    Total: %d", roll1, 
				     roll2, roll3);
				prt(tmp_str,7,5);
				if ((roll3 == 7) || (roll3 == 11))
					win = TRUE;
				else if ((roll3 == 2) || (roll3 == 3) || (roll3 == 12))
					win = FALSE;
				else
					do {
						msg_print("Hit any key to roll again");
						msg_print(NULL);
						roll1 = randint(6);
						roll2 = randint(6);
						roll3 = roll1 +  roll2;
					(void) sprintf(tmp_str,"Roll result: %d %d   Total:     %d",
						     roll1, roll2, roll3);
						prt(tmp_str,8,5);
						if (roll3 == choice)
							win = TRUE;
						else if (roll3 == 7)
							win = FALSE;
					} while ((win != TRUE) && (win != FALSE));
				break;

			case 's':  /* Spin the Wheel Game */
				win = FALSE;
				odds = 10;
				prt("Wheel",5,2);
				prt("0  1  2  3  4  5  6  7  8  9",7,5);
				prt("--------------------------------",8,3);
				strcpy(out_val, "");
				get_string ("Pick a number (1-9): ",out_val,32);
				for (p=out_val;*p == ' '; p++);
				choice = atol(p);
				if (choice < 0) {
					msg_print("I'll put you down for 0.");
					choice = 0;
				} else if (choice > 9) {
					msg_print("Ok, I'll put you down for 9.");
					choice = 9;
				}
				msg_print(NULL);
				roll1 = randint(10) - 1;
				(void) sprintf(tmp_str, "The wheel spins to a stop and the winner is %d",
				    roll1);
				prt(tmp_str,13,3);
				prt("",9,0);
				prt("*",9,(3*roll1+5));
				if (roll1 == choice)
					win = TRUE;
				break;

			case 'd': /* The Dice Slots */
				prt("Dice Slots",5,2);
				win = FALSE;
				roll1 = randint(6); 
				roll2 = randint(6); 
				choice = randint(6); 
				(void) sprintf(tmp_str, "%s %s %s", fruit[roll1-1], fruit[roll2-1],
				     fruit[choice-1]);
				prt(tmp_str,15,37);
				prt("/--------------------------\\",7,2);
				prt("\\--------------------------/",17,2);
				display_fruit(8,3,roll1-1);
				display_fruit(8,12,roll2-1);
				display_fruit(8,21,choice-1);
				if ((roll1 == roll2) && (roll2 == choice)) {
					win = TRUE;
					if (roll1 == 1)
						odds = 4;
					else if (roll1 == 2)
						odds = 6;
					else
						odds = roll1 * roll1;
				} else if ((roll1 == 6) && (roll2 == 6)) {
					win = TRUE;
					odds = choice + 1;
				}
				break;
			}

			if (win) {
				prt("YOU WON",16,37);
				p_ptr->au = p_ptr->au + (odds * wager);
				(void) sprintf(tmp_str,"Payoff: %d",odds);
				prt(tmp_str,17,37);
			} else {
				prt("You Lost",16,37);
				p_ptr->au = p_ptr->au - wager;
				prt("", 17, 37);
			}
			(void) sprintf(tmp_str,"Current Gold:     %9ld",p_ptr->au);
			prt(tmp_str,22,2);
			prt("Again(Y/N)?",18,37);
			move_cursor(18, 49);
			again = inkey();
			if (wager > p_ptr->au) {
				msg_print("Hey! You don't have the gold - wager is zip");
				msg_print(NULL);
				wager = 0;
				(void) sprintf(tmp_str,"Current Wager:    %9ld",wager);
				prt(tmp_str,17,2);
			}
		} while ((again == 'y') || (again == 'Y'));

		prt("",18,37);
		if (p_ptr->au >= oldgold) 
			msg_print("You came out a winner! We'll win next time, I'm sure.");
		else
			msg_print("You lost gold! Haha, better head home.");
		msg_print(NULL);
	}
	clear_bldg(5,23);
	display_bldg(3);
}


/*
 * inn commands
 */
void inn_comm(char cmd,int cost)
{
	int n, dawnval;

	switch(cmd) {
		case 'f': /* Buy food & drink */
			if (p_ptr->au < cost) {
				msg_print("You have not the gold!");
				msg_print(NULL);
			} else {
				msg_print("The barkeep gives you some gruel and a beer.");
				msg_print(NULL);
				p_ptr->au = p_ptr->au - cost;
				(void) set_food(PY_FOOD_MAX-1);
			}
			break;

		/* Note that resting for the night was a perfect way to avoid player
		ghosts in the town *if* you could only make it to the inn in time (-:
		Now that the ghosts are temporarily disabled in 2.8.X, this function
		will not be that useful.  I will keep it in the hopes the player
		ghost code does become a reality again. Does help to avoid filthy urchins.
		Resting at night is also a quick way to restock stores? -KMW- */
		case 'r': /* Rest for the night */
			dawnval = ((turn % (10L * TOWN_DAWN)));
			if (dawnval > 50000) {  /* nighttime */
				if (p_ptr->au < cost)
					msg_print("You have not the gold!");
				else if ((p_ptr->poisoned > 0) || (p_ptr->cut > 0)){
					msg_print("You need a healer, not a room.");
					msg_print(NULL);
					msg_print("Sorry, but don't want anyone dying in here.");
				} else {
					turn = ((turn/50000)+1)*50000;
					p_ptr->au = p_ptr->au - cost;
					p_ptr->chp = p_ptr->mhp;
					set_blind(0);
					set_confused(0);
					p_ptr->stun = 0;
					msg_print("You awake refreshed for the new day.");
					p_ptr->oldpy = p_ptr->py;
					p_ptr->oldpx = p_ptr->px;
					p_ptr->leftbldg = TRUE;
					p_ptr->leaving = TRUE;
					/* Maintain each shop (except home) */
					for (n = 0; n < MAX_STORES - 1; n++)
					{
						/* Maintain */
						store_maint(n);
					}
				}
			} else { 
				msg_print("The rooms are available only at night.");
				msg_print(NULL);
			}
			break;
		case 'u': /* Listen for rumors */
			msg_print("Not Implemented Yet (sigh).");
			break;
	}
}


/*
 * share gold for thieves
 */
void share_gold()
{
	char tmp_str[80];
	int i;

	i = (p_ptr->lev * 2) * 10;
	(void) sprintf(tmp_str, "You collect %d gold pieces", i);
	msg_print(tmp_str);
	msg_print(NULL);
	p_ptr->au += i; 
}

/*
 * Display quest information
 */
void get_questinfo(int questnum)
{
	int i, j, qidx;
	vault_type *v_ptr;

	i = 5;
	j = 0;
	qidx = questnum - QUEST_OFFSET1;
	v_ptr = &v_info[qidx];

	prt(q_list[qidx].qname,i+2,0);

	c_put_str(TERM_YELLOW,q_list[qidx].qtext1,8,0);
	c_put_str(TERM_YELLOW,q_list[qidx].qtext2,9,0);
	c_put_str(TERM_YELLOW,q_list[qidx].qtext3,10,0);
	c_put_str(TERM_YELLOW,q_list[qidx].qtext4,11,0);
	c_put_str(TERM_YELLOW,q_list[qidx].qtext5,12,0);
	c_put_str(TERM_YELLOW,q_list[qidx].qtext6,13,0);
	c_put_str(TERM_YELLOW,q_list[qidx].qtext7,14,0);
	c_put_str(TERM_YELLOW,q_list[qidx].qtext8,15,0);
	c_put_str(TERM_YELLOW,q_list[qidx].qtext9,16,0);

	prt("Complete this quest and return for a reward.",18,0);

	prt("Quest Information:",i,0);
	msg_print(NULL);
}


/*
 * Request a quest from the Lord.  This function will use the rewards array,
 * positions 20-29.  When the quest is awarded by the Castle, the
 * reward flag will be set to 1.  When the quest is completed, flag set to 2.
 * When the person returns to the castle, they are given a gift and
 * the reward flag is set to 3 (for completion).  Currently, there are
 * ten castle quests and two "final quests".  There are five quests that
 * require the player to kill either x number of monsters or 
 * one particular monster.  The other five quests involve special levels
 * that have to be completed.  The last two quests are not castle quests but
 * rather the built-in quests of Sauron and Morgoth.
 */
void castle_quest()
{
	int increment, j, j2, oldlevel;
	char tmp_str[80];
	int qstatus;
	int qidx;
	monster_race *r_ptr;
	cptr name;

	qstatus = 0;
	qidx = 0;
	increment = ((p_ptr->lev) / 2);
	clear_bldg(7,19);
	if (increment < 1) {
		put_str("You are too green to go off on a quest!", 8, 0);
		(void) sprintf(tmp_str,"Return when you have reached level %d.", 2);
		put_str(tmp_str,10,0);
		msg_print(NULL);
	} else if ((increment >= 25) && (p_ptr->rewards[(QUEST_REWARD_TAIL - 1 - QUEST_DIFF)] == 3)) {
		put_str("You have completed all quests I have given and have",8,0);
		put_str("vanquished the evil Morgoth!  We are in your debt.",9,0);
		put_str("You have fulfilled your destiny!",12,0);
	} else if (p_ptr->rewards[increment + 29] == 3) {
		put_str("You fulfilled your last quest but you are ready for another.",8,0);
		sprintf(tmp_str,"Return when you have reached level %d", (increment*2)+2);
		put_str(tmp_str,12,0);
		msg_print(NULL);
	} else { /* set last reward that hasn't been given to TRUE */
		increment = 0;

		for (j2=QUEST_REWARD_HEAD;j2 < QUEST_REWARD_TAIL;j2++) {
			increment = j2;
			j = j2 - QUEST_DIFF;
			if (p_ptr->rewards[j] == 0) {
				p_ptr->rewards[j] = 1;
				qstatus = 0;
				break;
			} else if (p_ptr->rewards[j] == 1) {
				put_str("You have not completed your current quest yet!",8,0);
				put_str("Use CTRL-Q to check the status of your quest.",9,0);
				put_str("Return when you have completed your quest.",12,0);
				qstatus = 1;
				break;
			} else if (p_ptr->rewards[j] == 2) {
				qstatus = 2;
				/* just fulfilled quest */
				p_ptr->rewards[j] = 3;
				break;
			}
		}

		switch(qstatus)
		{
			case 0: /* need to assign quest */
			{
				qidx = increment - QUEST_OFFSET1;
				if (q_list[qidx].quest_type == 2) {
					q_list[qidx].r_idx = randint(50) + (40 * (increment - QUEST_OFFSET1));
					r_ptr = &r_info[q_list[qidx].r_idx];
					while ((r_ptr->flags1 & (RF1_UNIQUE)) ||
					  (r_ptr->rarity != 1)) {
						q_list[qidx].r_idx = randint(50) + 100;
						r_ptr = &r_info[q_list[qidx].r_idx];
					}
					if (randint(10) > 7)
						q_list[qidx].max_num = 1;
					else
						q_list[qidx].max_num = randint(3) + 1;
					q_list[qidx].cur_num = 0;
					name = (r_name + r_ptr->name);
					sprintf(tmp_str,"Your quest: kill %d %s", 
					    q_list[qidx].max_num, name);
					msg_print(tmp_str);
					msg_print(NULL);
				} else {
					get_questinfo(increment);
					p_ptr->rewards[j] = 1;
					wilderness_gen(1);
				}
				break;
			}

			case 1: /* have not completed quest yet */
			{
				break;
			}

			case 2: /* completed quest, need to be rewarded */
			{
				msg_print("A great gift awaits you outside!");
				msg_print(NULL);
				oldlevel = p_ptr->depth;
				p_ptr->depth = 30;
				if (j < ((QUEST_REWARD_HEAD + QUEST_REWARD_TAIL - 140)/2))
					acquirement(p_ptr->py,p_ptr->px,1,0);
				else
					acquirement(p_ptr->py,p_ptr->px,1,1);
				p_ptr->depth = oldlevel;
				break;
			}
		}
	}
}


/*
 * Greet the lord of the castle
 */
void castle_greet()
{
	int increment, j, oldlevel;
	char tmp_str[80];

	increment = ((p_ptr->lev - 1) / 5);
	if (increment < 1) {
		(void) sprintf(tmp_str,"Ah, a young adventurer, return when: %s (Level %d)",
		player_title[p_ptr->pclass][1], 6);
		msg_print(tmp_str);
		msg_print(NULL);
	} else if ((increment >= 5) && (p_ptr->rewards[14])) {
		msg_print("Your greeting is returned with reverance and respect");
	} else if (p_ptr->rewards[increment + 9]) {
		(void) sprintf(tmp_str,"You have been rewarded, return when %s (Level %d)",
		player_title[p_ptr->pclass][increment+1], ((increment*5)+6));
		msg_print(tmp_str);
		msg_print(NULL);
	} else { /* set last reward that hasn't been given to TRUE */
		for (j=10;j<20;j++) {
			if (p_ptr->rewards[j] == 0) {
				p_ptr->rewards[j] = 1;
				break;
			}
		}

		if (j == 10) {
			msg_print("Well done! Please take up residence in the house down the street.");
			msg_print(NULL);
		} else if (j == 11) {
			msg_print("Very good! The weaponsmaster will be able to help you now.");
			msg_print(NULL);
			wilderness_gen(1);
		} else if (j == 12) {
			msg_print("You are proving yourself worthy. You may visit the Beastmaster.");
			msg_print(NULL);
			wilderness_gen(1);
		} else if (j == 13) {
			msg_print("You have earned a great deal of respect.");
			msg_print(NULL);
			msg_print("You have my permission to visit any building you wish.");
			msg_print(NULL);
/*			msg_print("A great gift awaits you outside!");
			msg_print(NULL);
			oldlevel = p_ptr->depth;
			p_ptr->depth = 30;
			acquirement(p_ptr->py,p_ptr->px,1,1);
			p_ptr->depth = oldlevel; */
		} else if (j >= 14) {
			msg_print("A nice gift awaits you outside!");
			msg_print(NULL);
			oldlevel = p_ptr->depth;
			p_ptr->depth = 20;
			acquirement(p_ptr->py,p_ptr->px,1,0);
			p_ptr->depth = oldlevel;
		}
	}
}


/*
 * greet_char
 */
void greet_char()
{
	int increment, j, oldlevel;
	char tmp_str[80];

	increment = (((p_ptr->lev - 1) / 5)/2);
	if (p_ptr->lev == 50)
		increment = 5;
	if (increment < 1) {
		(void) sprintf(tmp_str,"You are young yet, return when: %s (Level %d)",
		player_title[p_ptr->pclass][2], 11);
		msg_print(tmp_str);
		msg_print(NULL);
	} else if ((increment == 5) && (p_ptr->rewards[increment - 1]))
		msg_print("Your greeting is returned with reverance and respect");
	else if (p_ptr->rewards[increment - 1]) {
		if (increment == 4) 
			(void) sprintf(tmp_str,"You have been rewarded, return when %s (Level %d)",
			     player_title[p_ptr->pclass][9], 50);
		else
			(void) sprintf(tmp_str,"You have been rewarded, return when %s (Level %d)",
			     player_title[p_ptr->pclass][(increment+1) * 2], (((increment+1)*10)+1));
			msg_print(tmp_str);
			msg_print(NULL);
		} else {
			for (j=0;j<10;j++) {
				if (p_ptr->rewards[j] == FALSE) {
					p_ptr->rewards[j] = TRUE;
					break;
				}
			}
			switch(building) {
			case 10: /* Hall of Fighters Rewards */
				if (j == 0) {
					msg_print("You have done well. A gift awaits you outside.");
					msg_print(NULL);
					put_reward(TV_RING, SV_RING_STR, 10);
				} else if (j == 1) { 
					msg_print("You have done well. A gift awaits you outside.");
					msg_print(NULL);
					put_reward(TV_RING, SV_RING_CON, 20);
				} else if (j == 2) { 
					msg_print("Well done! A great gift awaits you outside!");
					msg_print(NULL);
					oldlevel = p_ptr->depth;
					p_ptr->depth = 30;
					acquirement(p_ptr->py,p_ptr->px,1,1);
					p_ptr->depth = oldlevel;
				} else if (j == 3) { 
					msg_print("An excellent gift awaits you outside!");
					msg_print(NULL);
					put_reward(TV_DRAG_ARMOR, randint(5), 40);
				} else if (j == 4) {
					msg_print("Well done! Great items await you outside.");
					msg_print(NULL);
					oldlevel = p_ptr->depth;
					p_ptr->depth = 40;
					acquirement(p_ptr->py,p_ptr->px,3,1);
					p_ptr->depth = oldlevel;
				}
				break;
			case 11: /* Tower of Sorcery Rewards */
				if (j == 0) {
					msg_print("You're improving. You may request recharging.");
					msg_print(NULL);
				} else if (j == 1) {
					msg_print("Impressive. You may partake of identifications.");
					msg_print(NULL);
				} else if (j == 2) { 
					msg_print("Nice. A spellbook awaits you outside.");
					msg_print(NULL);
					put_reward(TV_MAGIC_BOOK, (randint(4)+3), 20);
				} else if (j == 3) { 
					msg_print("Excellent! A spellbook awaits you outside.");
					msg_print(NULL);
					put_reward(TV_MAGIC_BOOK, (randint(4)+3), 20);
				} else if (j == 4) { 
					msg_print("Please accept this rarest of spellbooks outside.");
					msg_print(NULL);
					put_reward(TV_MAGIC_BOOK, 8, 20);
				}
				break;
			case 12: /* Inner Temple Rewards */
				if (j == 0) {
					msg_print("I'm pleased with you. Restoration is available.");
					msg_print(NULL);
				} else if (j == 1) {
					msg_print("You have done well. A gift awaits you outside.");
					msg_print(NULL);
					put_reward(TV_POTION, SV_POTION_LIFE, 10);
				} else if (j == 2) {
					msg_print("Nice. A prayer book awaits you outside.");
					msg_print(NULL);
					put_reward(TV_PRAYER_BOOK, (randint(4)+3), 20);
				} else if (j == 3) {
					msg_print("Excellent! A prayer book awaits you outside.");
					msg_print(NULL);
					put_reward(TV_PRAYER_BOOK, (randint(4)+3), 20);
				} else if (j == 4) {
					msg_print("Please accept this rarest of prayer books.");
					msg_print(NULL);
					put_reward(TV_PRAYER_BOOK, 8, 20);
				}
				break;
			case 13: /* House of Thieves Rewards */
				if (j == 0) {
					msg_print("Well done! You may take your share of gold.");
					msg_print(NULL);
				} else if (j == 1) {
					msg_print("Good job! Now, we'll identify your possessions.");
					msg_print(NULL);
				} else if (j == 2) { 
					msg_print("A great gift awaits you outside!");
					msg_print(NULL);
					oldlevel = p_ptr->depth;
					p_ptr->depth = 30;
					acquirement(p_ptr->py,p_ptr->px,1,1);
					p_ptr->depth = oldlevel;
				} else if (j == 3) { 
					msg_print("A great gift awaits you outside!");
					msg_print(NULL);
					oldlevel = p_ptr->depth;
					p_ptr->depth = 30;
					acquirement(p_ptr->py,p_ptr->px,1,1);
					p_ptr->depth = oldlevel;
				} else if (j == 4) { 
					msg_print("Well done! Great items await you outside.");
					msg_print(NULL);
					oldlevel = p_ptr->depth;
					p_ptr->depth = 40;
					acquirement(p_ptr->py,p_ptr->px,3,1);
					p_ptr->depth = oldlevel;
				}
				break;
			case 14: /* Ranger's Tavern Rewards */
				if (j == 0) { 
					msg_print("You have done well. A gift awaits you outside.");
					msg_print(NULL);
					put_reward(TV_RING, SV_RING_STR, 10);
				} else if (j == 1) { 
					msg_print("You have done well. A gift awaits you outside.");
					msg_print(NULL);
					put_reward(TV_RING, SV_RING_INT, 20);
				} else if (j == 2) { 
					msg_print("A great gift awaits you outside!");
					msg_print(NULL);
					oldlevel = p_ptr->depth;
					p_ptr->depth = 30;
					acquirement(p_ptr->py,p_ptr->px,1,1);
					p_ptr->depth = oldlevel;
				} else if (j == 3) { 
					msg_print("An excellent gift awaits you outside!");
					msg_print(NULL);
					put_reward(TV_DRAG_ARMOR, randint(5), 40);
				} else if (j == 4) { 
					msg_print("Well done! Great items await you outside.");
					msg_print(NULL);
					oldlevel = p_ptr->depth;
					p_ptr->depth = 40;
					acquirement(p_ptr->py,p_ptr->px,3,1);
					p_ptr->depth = oldlevel;
				}
				break;
			case 15: /* Order of Paladins Rewards */
				if (j == 0) {
					msg_print("You have done well. A gift awaits you outside.");
					msg_print(NULL);
					put_reward(TV_RING, SV_RING_STR, 10);
				} else if (j == 1) {
					msg_print("You have done well. A gift awaits you outside.");
					msg_print(NULL);
					put_reward(TV_RING, SV_RING_CON, 20);
				} else if (j == 2) {
					msg_print("A great gift awaits you outside!");
					msg_print(NULL);
					oldlevel = p_ptr->depth;
					p_ptr->depth = 30;
					acquirement(p_ptr->py,p_ptr->px,1,1);
					p_ptr->depth = oldlevel;
				} else if (j == 3) { 
					msg_print("An excellent gift awaits you outside!");
					msg_print(NULL);
					put_reward(TV_DRAG_ARMOR, randint(5), 40);
				} else if (j == 4) { 
					msg_print("Well done! Great items await you outside.");
					msg_print(NULL);
					oldlevel = p_ptr->depth;
					p_ptr->depth = 40;
					acquirement(p_ptr->py,p_ptr->px,3,1);
					p_ptr->depth = oldlevel;
				}
				break;
			case 16: /* Tower of Illusion Rewards */
				if (j == 0) {
					msg_print("You're improving. You may request recharging.");
					msg_print(NULL);
				} else if (j == 1) {
					msg_print("Impressive. You may partake of identifications.");
					msg_print(NULL);
				} else if (j == 2) {
					msg_print("Nice. An illusion book awaits you outside.");
					msg_print(NULL);
					put_reward(TV_ILLUSION_BOOK, (randint(4)+3), 20);
				} else if (j == 3) {
					msg_print("Excellent. An illusion book awaits you outside.");
					msg_print(NULL);
					put_reward(TV_ILLUSION_BOOK, (randint(4)+3), 20);
				} else if (j == 4) {
					msg_print("Please accept this rarest of illusion books.");
					msg_print(NULL);
					put_reward(TV_ILLUSION_BOOK, 8, 20);
				}
				break;
			default:
				bell();
				break;
		}
	}
}



/*
 * Displaying town history
 * Will want this to simply list a file one page at a time, rather
 * than have the text contents right here.  Could also be in help
 * format where no command would be necessary.
 */
void town_history()
{
	clear_bldg(5,18);
	prt("               Historical Town View", 5, 0);
	prt("The town is composed of both stores and buildings. Stores:",7,0);
	prt("   Magic Shop: get your wands and spellbooks here.",8,0);
	prt("   Alchemist: for all sorts of bubbling potions and scrolls.",9,0);
	prt("   Weaponsmith: they carry anything sharp and to the point.",10,0);
	prt("   Armorer: to protect from the ravages of the dungeon.",11,0);
	prt("   General Store: food, torches, the necessities.",12,0);
	prt("   Temple Shop: prayerbooks and those items holy.",13,0);
	prt("   Black Market: the prices are high, but item unique.",14,0);
	prt("   Home: To store your precious treasures.",15,0);
	prt("Make sure you go into the correct door on some buildings.",16,0);
	msg_print("Press the spacebar to continue");
	msg_print(NULL);
	prt("General buildings with common themes:",7,0);
	prt("Please greet those you meet in your specialty and otherwise.",8,0);
	prt("  They may impart advice or reward diligent adventurers.",9,0);
	prt("Take a look at spires, busts, etc. You may one day be honored.",10,0);
	prt("",11,0);
	prt("The Fighter's Hall, Order of Paladins, Ranger's Tavern, House",12,0);
	prt("of Thieves, and Tower of Illusion are restricted to those involved.",13,0);
	prt("The Tower of Sorcery is open for rogues and rangers to learn spells.",14,0);
	prt("The Temple is open for paladins to learn spells.",15,0);
	prt("  All players must be taught here. Only the masters have the knowledge.",16,0);
	prt("",17,0);
	msg_print("Press the spacebar to continue");
	msg_print(NULL);
	prt("Unique buildings that anyone can enter, if they dare:",7,0);
	prt("  Arena: Try hand-to-hand combat but read the rules!",8,0);
	prt("    Also, checking the poster first is always a good idea!",9,0);
	prt("  Gambling House: Read the rules before paying. And, the games",10,0);
	prt("    are not rigged, just naturally difficult.",11,0); 
	prt("  Library: For information of all kinds.",12,0);
	prt("  Inn: Resting will refresh, and those ghosts prowl at night...",13,0);
	prt("    Rumors can be helpful or just plain silly.",14,0);
	prt("",15,0);
	prt("All buildings are made of stone and unlikely to move around.",16,0);
	msg_print("Press the spacebar to continue");
	msg_print(NULL);
	clear_bldg(5,18);
}


/*
 * Calculate the weapon ability
 */
int calc_weapon(object_type *o_ptr)
{
        int i;

	int old_speed;

	int old_dis_ac;
	int old_dis_to_a;
	int dis_to_d, dis_to_h;
	int to_h, to_d;

	int extra_blows;
	int extra_shots;
	int extra_might;

	int old_stat_top[6];
	int stat_top[6];
	int old_stat_use[6];
	int old_stat_ind[6];
	int stat_add[6];
	int stat_cur[6];
	int stat_use[6];
        int stat_ind[6];
	bool bless_blade, impact, heavy_wield;

	u32b f1, f2, f3;
	int pspeed, num_blow;


	old_speed = p_ptr->pspeed;

	old_dis_ac = p_ptr->dis_ac;
	old_dis_to_a = p_ptr->dis_to_a;

	for (i = 0; i < 6; i++)
	{
		old_stat_top[i] = p_ptr->stat_top[i];
		old_stat_use[i] = p_ptr->stat_use[i];
		old_stat_ind[i] = p_ptr->stat_ind[i];
		stat_add[i] = 0;
		stat_cur[i] = p_ptr->stat_cur[i];
	}

	pspeed = 110;
	num_blow = 1;
	extra_blows = 0;
	extra_shots = 0;

	to_h = 0;
	to_d = 0;

	object_flags(o_ptr, &f1, &f2, &f3);

	if (f1 & (TR1_STR)) stat_add[A_STR] += o_ptr->pval;
	if (f1 & (TR1_INT)) stat_add[A_INT] += o_ptr->pval;
	if (f1 & (TR1_WIS)) stat_add[A_WIS] += o_ptr->pval;
	if (f1 & (TR1_DEX)) stat_add[A_DEX] += o_ptr->pval;
	if (f1 & (TR1_CON)) stat_add[A_CON] += o_ptr->pval;
	if (f1 & (TR1_CHR)) stat_add[A_CHR] += o_ptr->pval;

	if (f1 & (TR1_SPEED)) pspeed += o_ptr->pval;

	if (f1 & (TR1_BLOWS)) extra_blows += o_ptr->pval;

	if (f1 & (TR1_SHOTS)) extra_shots += o_ptr->pval;

	if (f1 & (TR1_MIGHT)) extra_might += o_ptr->pval;

	if (f3 & (TR3_BLESSED)) bless_blade = TRUE;

	if (f3 & (TR3_IMPACT)) impact = TRUE;

	to_h += o_ptr->to_h;
	to_d += o_ptr->to_d;

	if (object_known_p(o_ptr)) dis_to_h += o_ptr->to_h;
	if (object_known_p(o_ptr)) dis_to_d += o_ptr->to_d;

	for (i = 0; i < 6; i++)
	{
		int add, top, use, ind;

		add = stat_add[i];

		if (p_ptr->maximize)
		{
			add += (rp_ptr->r_adj[i] + cp_ptr->c_adj[i]);
		}

		top = modify_stat_value(p_ptr->stat_max[i], add);

		stat_top[i] = top;

		use = modify_stat_value(stat_cur[i], add);

		stat_use[i] = use;

		if (use <= 18) ind = (use - 3);

		else if (use <= 18+219) ind = (15 + (use - 18) / 10);

		else ind = (37);

		stat_ind[i] = ind;
	}

	if (p_ptr->fast)
	{
		pspeed += 10;
	}

	if (p_ptr->slow)
	{
		pspeed -= 10;
	}

	to_d += ((int)(adj_str_td[stat_ind[A_STR]]) - 128);
	to_h += ((int)(adj_dex_th[stat_ind[A_DEX]]) - 128);
	to_h += ((int)(adj_str_th[stat_ind[A_STR]]) - 128);

	p_ptr->dis_to_d += ((int)(adj_str_td[stat_ind[A_STR]]) - 128);
	p_ptr->dis_to_h += ((int)(adj_dex_th[stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_h += ((int)(adj_str_th[stat_ind[A_STR]]) - 128);

	heavy_wield = FALSE;

	if (o_ptr->k_idx && !p_ptr->heavy_wield)
	{
		int str_index, dex_index;

		int num = 0, wgt = 0, mul = 0, div = 0;

		switch (p_ptr->pclass)
		{
			case CLASS_WARRIOR: num = 6; wgt = 30; mul = 5; break;

			case CLASS_MAGE:    num = 4; wgt = 35; mul = 3; div *= 2; break;

			case CLASS_PRIEST:  num = 5; wgt = 40; mul = 2; break;

			case CLASS_ROGUE:   num = 5; wgt = 30; mul = 3; break;

			case CLASS_RANGER:  num = 5; wgt = 35; mul = 4; break;

			case CLASS_PALADIN: num = 5; wgt = 35; mul = 4; break;

 			case CLASS_ILLUSIONIST:    num = 4; wgt = 35; mul = 3; div *= 2; break;
		}

		div = ((o_ptr->weight < wgt) ? wgt : o_ptr->weight);

		str_index = (adj_str_blow[stat_ind[A_STR]] * mul / div);

		if (str_index > 11) str_index = 11;

		dex_index = (adj_dex_blow[stat_ind[A_DEX]]);
		if (dex_index > 11) dex_index = 11;
		num_blow = blows_table[str_index][dex_index];
		if (num_blow > num) num_blow = num;
		num_blow += extra_blows;
		if (num_blow < 1) num_blow = 1;
	}

	if ((p_ptr->pclass == 2) && (!bless_blade) &&
	    ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
	{
		to_h -= 2;
		to_d -= 2;

		dis_to_h -= 2;
		dis_to_d -= 2;
	}

	if (((p_ptr->pclass == 1) || (p_ptr->pclass == 6)) &&
	    ((o_ptr->sval != SV_DAGGER) && (o_ptr->sval != SV_QUARTERSTAFF) &&
	    (o_ptr->tval)))
	{
		to_h -= 5;
		to_d -= 5;
		dis_to_h -= 5;
		dis_to_d -= 5;
	}

	return(num_blow);
}


/*
 * compare_weapons
 */
bool compare_weapons(void)
{
	int item, i;
	int nb;
	object_type *o1_ptr, *o2_ptr;
	char o1_name[80], o2_name[80];
	char tmp_str[80];
	cptr q, s;

	clear_bldg(6,18);

	i = 8;
	/* Get an item */
	q = "What is your first weapon? ";
	s = "You have nothing to compare.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o1_ptr = &inventory[item];
		object_desc(o1_name, o1_ptr, TRUE, 0);
	}
	if ((o1_ptr->tval < TV_BOW) || (o1_ptr->tval > TV_SWORD))
	{
		msg_print("Not a weapon! Try again.");
		msg_print(NULL);
		return(FALSE);
	}

	/* Get an item */
	q = "What is your second weapon? ";
	s = "You have nothing to compare.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o2_ptr = &inventory[item];
		object_desc(o2_name, o2_ptr, TRUE, 0);
	}
	if ((o2_ptr->tval < TV_BOW) || (o2_ptr->tval > TV_SWORD))
	{
		msg_print("Not a weapon! Try again.");
		msg_print(NULL);
		return(FALSE);
	}

	put_str("Based on your current abilities, here is what your weapons will do", 6, 2);

	nb = calc_weapon(o1_ptr);
	/* TODO - take into account the other pieces such as speed from
	calc_weapon */
	c_put_str(TERM_YELLOW,o1_name,i,5);
	sprintf(tmp_str,"To Hit: %d   To Damage: %d",o1_ptr->to_h, o1_ptr->to_d);
	put_str(tmp_str,i+1,5);
	sprintf(tmp_str,"Dice: %d   Sides: %d",o1_ptr->dd, o1_ptr->ds);
	put_str(tmp_str,i+2,5);
	sprintf(tmp_str,"Number of Blows: %d", nb);
	put_str(tmp_str,i+3,5);
	c_put_str(TERM_YELLOW, "Possible Damage:",i+5,5);
	sprintf(tmp_str,"One Strike: %d-%d damage",o1_ptr->dd*1+o1_ptr->to_d,
	    o1_ptr->ds*o1_ptr->dd + o1_ptr->to_d);
	put_str(tmp_str,i+6,6);
	sprintf(tmp_str,"One Attack: %d-%d damage",nb*o1_ptr->dd + o1_ptr->to_d,
	    nb*o1_ptr->ds*o1_ptr->dd + o1_ptr->to_d);
	put_str(tmp_str,i+7,6);

	nb = calc_weapon(o2_ptr);
	c_put_str(TERM_YELLOW,o2_name,i,40);
	sprintf(tmp_str,"To Hit: %d   To Damage: %d",o2_ptr->to_h, o2_ptr->to_d);
	put_str(tmp_str,i+1,40);
	sprintf(tmp_str,"Dice: %d   Sides: %d",o2_ptr->dd, o2_ptr->ds);
	put_str(tmp_str,i+2,40);
	sprintf(tmp_str,"Number of Blows: %d", nb);
	put_str(tmp_str,i+3,40);
	c_put_str(TERM_YELLOW, "Possible Damage:",i+5,40);
	sprintf(tmp_str,"One Strike: %d-%d damage",o2_ptr->dd*1 + o2_ptr->to_d,
	    o2_ptr->ds*o2_ptr->dd + o2_ptr->to_d);
	put_str(tmp_str,i+6,41);
	sprintf(tmp_str,"One Attack: %d-%d damage",nb*o2_ptr->dd + o2_ptr->to_d,
	    nb*o2_ptr->ds*o2_ptr->dd + o2_ptr->to_d);
	put_str(tmp_str,i+7,41);

	return(TRUE);
}


/*
 * sharpen arrows
 */
void sharpen_arrows(int cost)
{
	int i,j,k, maxenchant;
	object_type *o_ptr;
	char out_val[80], tmp_str[80];

	clear_bldg(5,18);
	k = 0;
	maxenchant = (p_ptr->lev / 5);
	sprintf(tmp_str,"  Based on your skill, we can enchant your arrows up to +%d", maxenchant);
	prt(tmp_str, 5, 0);
	prt("                             Arrow Status", 7, 0);
	j = 9;

	for (i = 0; i < INVEN_WIELD; i++) {
		o_ptr = &inventory[i];
		if (o_ptr->tval == TV_ARROW) {
			k = 1;
			object_desc(tmp_str, o_ptr, FALSE, 1);
			if (o_ptr->name1) 
				sprintf(out_val, "%-40s: In fine condition", tmp_str);
			else if ((o_ptr->to_h < -3) || (o_ptr->to_d < -3))
				sprintf(out_val, "%-40s: Beyond repair, buy new arrows", tmp_str);
			else if ((o_ptr->to_h >= maxenchant) && (o_ptr->to_d >= maxenchant))
				sprintf(out_val, "%-40s: In fine condition", tmp_str);
			else  {
				if (o_ptr->to_h < maxenchant)
					o_ptr->to_h++;
				if (o_ptr->to_d < maxenchant)
					o_ptr->to_d++;
				sprintf(out_val, "%-40s: sharpened -> (%d,%d)", tmp_str,
				     o_ptr->to_h, o_ptr->to_d);
			}
			prt(out_val,j++,0);
		}
	}
	if (k == 0) {
		msg_print("You were not carrying any arrows.");
		msg_print(NULL);
		clear_bldg(5,18);
	} else {
		if (cost == 0)
			p_ptr->rewards[20] = TRUE;
		msg_print("Press the spacebar to continue");
		msg_print(NULL);
		clear_bldg(5,18);
		p_ptr->au = p_ptr->au - cost;
	}
}


/*
 * restring bow
 */
void restring_bow(int cost)
{
	object_type *o_ptr;
	int maxenchant;
	char out_val[80], tmp_str[80];

	maxenchant = (p_ptr->lev / 5);
	o_ptr = &inventory[INVEN_BOW];
	if (o_ptr->tval != TV_BOW) {
		msg_print("You aren't wielding a bow!");
		msg_print(NULL);
		return;
	}
	clear_bldg(5,18);
	sprintf(tmp_str,"  Based on your skill, we can enchant your bow up to +%d", maxenchant);
	prt(tmp_str, 5, 0);
	prt("                             Bow Status", 7, 0);
	object_desc(tmp_str, o_ptr, FALSE,1);
	if ((o_ptr->name1 && (o_ptr->ident & 0x08)))
		sprintf(out_val, "%-40s: Beyond our skills!", tmp_str);
	else if (o_ptr->name1) 
		sprintf(out_val, "%-40s: In fine condition", tmp_str);
	else if ((o_ptr->to_h < -3) || (o_ptr->to_d < -3))
		sprintf(out_val, "%-40s: Beyond repair, buy a new bow", tmp_str);
	else if ((o_ptr->to_h >= maxenchant) && (o_ptr->to_d >= maxenchant))
		sprintf(out_val, "%-40s: In fine condition", tmp_str);
	else  {
		if (o_ptr->to_h < maxenchant)
			o_ptr->to_h++;
		if (o_ptr->to_d < maxenchant)
			o_ptr->to_d++;
		sprintf(out_val, "%-40s: tightened -> (%d,%d)", tmp_str, o_ptr->to_h,
		     o_ptr->to_d);
	}
	prt(out_val,9,0);  
	if (cost == 0)
		p_ptr->rewards[21] = TRUE;
	p_ptr->au = p_ptr->au - cost;
	msg_print("Press the spacebar to continue");
	msg_print(NULL);
	clear_bldg(5,18);
}


/*
 * repair wielded weapons
 */
void repair_weapon(int cost)
{
	object_type *o_ptr;
	char out_val[80], tmp_str[80];
	int maxenchant;

	maxenchant = (p_ptr->lev / 5);
	sprintf(tmp_str,"  Based on your skill, we can enchant your weapon up to +%d", maxenchant);
	prt(tmp_str, 5, 0);
	prt("                             Weapon Status", 7, 0);
	o_ptr = &inventory[INVEN_WIELD];

	object_desc(tmp_str, o_ptr, FALSE,1);
	if (o_ptr->tval) {
		if ((o_ptr->name1 && (o_ptr->ident & 0x08)))
			sprintf(out_val, "%-40s: Beyond our skills!", tmp_str);
		else if (o_ptr->name1) 
			sprintf(out_val, "%-40s: In fine condition", tmp_str);
		else if ((o_ptr->to_h < -3) || (o_ptr->to_d < -3))
			sprintf(out_val, "%-40s: Beyond repair, buy a new one", tmp_str);
		else if ((o_ptr->to_h >= maxenchant) && (o_ptr->to_d >= maxenchant))
			sprintf(out_val, "%-40s: In fine condition", tmp_str);
		else  {
			if (o_ptr->to_h < maxenchant)
				o_ptr->to_h++;
			if (o_ptr->to_d < maxenchant)
				o_ptr->to_d++;
			sprintf(out_val, "%-40s: sharpened -> (%d,%d)",
			tmp_str, o_ptr->to_h, o_ptr->to_d);
		}
		prt(out_val,9,0);
		if (cost == 0)
			p_ptr->rewards[21] = TRUE;
		p_ptr->au = p_ptr->au - cost;
	} else {
		prt("You are not wielding a weapon.",7,0);
	}
	msg_print("Press the spacebar to continue");
	msg_print(NULL);
	clear_bldg(5,18);
}


/*
 * repair all pieces of armor up one in ac, not exceeding zero
 */
void repair_armor(int cost)
{
	int i,j, repaired, maxenchant;
	object_type *o_ptr;
	char out_val[80], tmp_str[80];

	maxenchant = (p_ptr->lev / 5);
	clear_bldg(5,18);
	sprintf(tmp_str,"  Based on your skill, we can enchant armor up to +%d", maxenchant);
	prt(tmp_str, 5, 0);

	repaired = 0;
	prt("                             Armor Status", 7, 0);
	j = 9;

	for (i = INVEN_BODY; i <= INVEN_FEET; i++) {
		o_ptr = &inventory[i];
		if (o_ptr->tval)  {
			object_desc(tmp_str, o_ptr, FALSE,1);
			if ((o_ptr->name1 && (o_ptr->ident & 0x08)))
				sprintf(out_val, "%-40s: Beyond our skills!", tmp_str);
			else if (o_ptr->name1)
				sprintf(out_val, "%-40s: In fine condition", tmp_str);
			else if (o_ptr->to_a < -3)
				sprintf(out_val, "%-40s: Beyond repair, buy a new one", tmp_str);
			else if (o_ptr->to_a >= maxenchant)
				sprintf(out_val, "%-40s: In fine condition", tmp_str);
			else {
				o_ptr->to_a++;
				sprintf(out_val, "%-40s: polished -> (%d)",
				tmp_str, o_ptr->to_a);
			}
			prt(out_val,j++,0);
			repaired = 1;
		}
	}
	if (repaired == 0)
		prt("You are not wearing any armor.",7,0);
	else {
		if (cost == 0)
			p_ptr->rewards[20] = TRUE;
		p_ptr->au = p_ptr->au - cost;
	}
	msg_print("Press the spacebar to continue");
	msg_print(NULL);
	clear_bldg(5,18);
}


/*
 * Research Item
 */
void research_item(int cost)
{
	clear_bldg(5,18);
	if (p_ptr->au < cost) {
		msg_print("You have not the gold!");
		msg_print(NULL);
	} else {
		if (identify_fully())
			p_ptr->au = p_ptr->au - cost;
	}
}


/* hack as in leave_store in store.c */
static bool leave_bldg = FALSE;


/*
 * Entering a building
 */
static void bldg_process_command(void)
{

	switch (p_ptr->command_cmd)
	{
		case ESCAPE:
		{
			leave_bldg = TRUE;
			break;
		}

		case 'a':
		{
			if (building == 0)
				research_item(2000);
			else if (building == 2) {
				arena_comm(p_ptr->command_cmd);
				if (p_ptr->inside_special) 
					leave_bldg = TRUE; 
			} else if (building == 14)
				if (p_ptr->pclass != CLASS_RANGER) {
					if (p_ptr->au >= 1000)
						sharpen_arrows(1000);
					else {
						msg_print("You have not the gold");
						msg_print(NULL);
					}
				} else if (!p_ptr->rewards[20])
					sharpen_arrows(0);
				else
					msg_print("Return after you've spent time in the dungeon.");
			else if ((building == 10) || (building == 15)) 
				if ((p_ptr->pclass != CLASS_WARRIOR) &&
				  (p_ptr->pclass != CLASS_PALADIN)) {
					if (p_ptr->au >= 1000)
						repair_armor(1000);
					else {
						msg_print("You have not the gold");
						msg_print(NULL);
					}
				} else if (!p_ptr->rewards[20]) {
					repair_armor(0);
				} else
					msg_print("Return after you've spent time in the dungeon.");
			else
				bell();
			break;
		}

		case 'b':
		{
			if (building == 3)
				gamble_comm(p_ptr->command_cmd);
			else if (building == 14)
				if (p_ptr->pclass != CLASS_RANGER) {
					if (p_ptr->au >= 1000)
						restring_bow(1000);
					else {
						msg_print("You have not the gold");
						msg_print(NULL);
					}
				} else if (!p_ptr->rewards[21]) {
					restring_bow(0);
				} else
					msg_print("Return after you've spent time in the dungeon.");
			else
				bell(); 
				break;
		}

		case 'c':
		{
			if (building == 3)
				gamble_comm(p_ptr->command_cmd);
			else if (building == 6) {
				if (p_ptr->au < 1000)
					msg_print("You have not the gold!");
				else if (compare_weapons())
					p_ptr->au -= 1000;
			} else
				bell();
			break;
		}

		case 'd':
		{
			if (building == 3)
				gamble_comm(p_ptr->command_cmd);
			else
				bell(); 
			break;
		}

		case 'f':
		{
			if (building == 4)
				inn_comm(p_ptr->command_cmd,1);
			else
				bell(); 
			break;
		}

		case 'g':
		{
			if (building == 1)
				castle_greet();
			else if (building > 6) {
				if ((building - 10) == p_ptr->pclass)
					greet_char();
				else {
					msg_print("Only those of our class can do that here.");
					msg_print(NULL);
				}
			} else
				bell();
			break;
		}

		case 'h':
		{
			if (building == 0)
				town_history();
			else if ((building == 12) || (building == 15)) {
				if ((p_ptr->pclass != CLASS_PRIEST) &&
				   (p_ptr->pclass != CLASS_PALADIN)) {
					if (p_ptr->au >= 1000) {
						hp_player(200);
						p_ptr->au = p_ptr->au - 1000;
						set_poisoned(0);
						p_ptr->cut = 0;
						p_ptr->stun = 0;
						msg_print("You are once again healthy & vigorous!");
						msg_print(NULL);
					} else {
						msg_print("You have not the gold");
						msg_print(NULL);
					}
				} else if (!p_ptr->rewards[21]) {
					if (p_ptr->pclass == CLASS_PALADIN)
						hp_player(200);
					else
						hp_player(1200);
					set_poisoned(0);
					set_blind(0);
					set_confused(0);
					p_ptr->cut = 0;
					p_ptr->stun = 0;
					p_ptr->rewards[21] = TRUE;
					msg_print("You are once again healthy & vigorous!");
					msg_print(NULL);
				} else
					msg_print("Return after you've spent time in the dungeon.");
			} else
				bell();
			break;
		}

		case 'l':
		{
			if (building == 0)
				race_legends();
			else if ((building < 3) || (building > 6))
				show_highclass(building); 
			else 
				bell(); 
			break;
		}

		case 'p':
		{
			if (building == 2)
				arena_comm(p_ptr->command_cmd);
			else if (building == 13) {
				if ((p_ptr->rewards[1]) && (!p_ptr->rewards[21])) {
					identify_pack(0);
					msg_print("Your possessions have been identified");
					p_ptr->rewards[21] = TRUE;
				} else if (!p_ptr->rewards[1]) {
					msg_print("You must get permission from the Master Thief");
					msg_print(NULL);
				} else
					msg_print("Return after you've spent time in the dungeon.");
			} else if ((building == 11) || (building == 16))
				if ((p_ptr->pclass != CLASS_MAGE) &&
				   (p_ptr->pclass != CLASS_ILLUSIONIST)) {
					if (p_ptr->au >= 1000) {
						identify_pack(1000);
						msg_print("Your possessions have been identified");
					} else {
						msg_print("You have not the gold");
						msg_print(NULL);
					}
				} else if ((p_ptr->rewards[1]) && (!p_ptr->rewards[21])) {
					identify_pack(0);
					msg_print("Your possessions have been identified");
					p_ptr->rewards[21] = TRUE;
				} else if (!p_ptr->rewards[1]) {
					msg_print("You have not attained the status for that.");
					msg_print(NULL);
				} else
					msg_print("Return after you've spent time in the dungeon.");
			else
				bell(); 
			break;
		}

		case 'q': /* request quest */
		{
			if (building == 1) 
				castle_quest();
			else
				bell();
			break;
		}

		case 'r':
		{
			if (building == 3)
				gamble_comm(p_ptr->command_cmd);
			else if (building == 4)
				inn_comm(p_ptr->command_cmd,20);
			else if (building == 2)
				arena_comm(p_ptr->command_cmd);
			else if (building == 5) {
				if (p_ptr->au < 1000)
					msg_print("You have not the gold!");
				else if (!research_mon())
					p_ptr->au -= 1000;
			} else if (building == 13)
				inn_comm(p_ptr->command_cmd,0);
			else if (building == 10)
				if (p_ptr->pclass != CLASS_WARRIOR) {
					if (p_ptr->au >= 1000)
						repair_weapon(1000);
					else {
						msg_print("You have not the gold");
						msg_print(NULL);
					}
				} else if (!p_ptr->rewards[21]) {
					clear_bldg(5,18);
					repair_weapon(0);
				} else
					msg_print("Return after you've spent time in the dungeon.");
			else if (((building == 11) || (building == 16)) &&
			     ((p_ptr->pclass == 1) || (p_ptr->pclass == 6)))
				if ((p_ptr->rewards[0]) && (!p_ptr->rewards[20])) {
					(void)recharge(80);
					p_ptr->rewards[20] = TRUE;
				} else if (!p_ptr->rewards[0]) {
					msg_print("You have not attained the status for that.");
					msg_print(NULL);
				} else
					msg_print("Return after you've spent time in the dungeon.");
			else if ((building == 11) || (building == 16)) {
				msg_print("Only mages/illusionists can do that here.");
				msg_print(NULL);
			} else if ((building == 12) && (p_ptr->pclass == CLASS_PRIEST)) 
				if ((p_ptr->rewards[0]) && (!p_ptr->rewards[20])) {
					restore_level();
					res_stat(A_STR);
					res_stat(A_INT);
					res_stat(A_WIS);
					res_stat(A_DEX);
					res_stat(A_CON);
					res_stat(A_CHR);
					p_ptr->rewards[20] = TRUE;
					msg_print("All your stats are normal");
					msg_print(NULL);
				} else if (!p_ptr->rewards[0]) {
					msg_print("You have not attained the status for that.");
					msg_print(NULL);
				} else
					msg_print("Return after you've spent time in the dungeon.");
			else if (building == 12) {
				msg_print("Only priests can do that here.");
				msg_print(NULL);
			} else
				bell(); 
				break;
		}

		case 's':
		{
			if (building == 3)
				gamble_comm(p_ptr->command_cmd);
			else if (building == 13)
				if ((p_ptr->rewards[0]) && (!p_ptr->rewards[20])) {
					share_gold();
					p_ptr->rewards[20] = TRUE;
				} else if (!p_ptr->rewards[0]) {
					msg_print("You must get permission from the Master Thief");
					msg_print(NULL);
				} else {
					msg_print("Come back later! Gold doesn't grow on trees!");
					msg_print(NULL);
				}
			else
				bell(); 
			break;
		}

		case 'u':
		{
			/* if (building == 4)
				inn_comm(p_ptr->command_cmd,0);
			else*/
				bell();
			break;
		}

		case 'z':
		{
			if ((building == 11) || (building == 16) || (building == 12)) 
				do_cmd_study(TRUE); 
			else
				bell(); 
			break;
		}

			/* Ignore return */
		case '\r':
		{
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

			/* Help */
		case '?':
		{
			do_cmd_help();
			break;
		}

			/* Hack -- Unknown command */
		default:
		{
			bell();
			break;
		}
	}
}


/*
 * Enter quest level
 */
void do_cmd_quest(void)
{
	if (!(cave_feat[p_ptr->py][p_ptr->px] == FEAT_QUEST_ENTER))
	{
		msg_print("You see no quest level here.");
		return;
	}

	else {
		p_ptr->oldpy = p_ptr->py;
		p_ptr->oldpx = p_ptr->px;
		p_ptr->inside_special = 2;
		p_ptr->depth = 1;
		p_ptr->leftbldg = TRUE;
		p_ptr->leaving = TRUE;
	}
}


/*
 * Do building commands
 */
void do_cmd_bldg(void)
{
	int which;
	int px=p_ptr->px;
	int py=p_ptr->py;

	if (!((cave_feat[py][px] >= FEAT_BLDG_HEAD) &&
	    (cave_feat[py][px] <= FEAT_BLDG_TAIL)))
	{
		msg_print("You see no building here.");
		return;
	}

	which = (cave_feat[py][px] - FEAT_BLDG_HEAD);
	building = which;
	
	if ((which == 2) && (p_ptr->inside_special) && (p_ptr->exit_bldg == FALSE)) {
		prt("Gates are closed!  The monster awaits.",0,0);
		return;
	} else if ((which == 2) && (p_ptr->inside_special == 1)) {
		p_ptr->leaving = TRUE;
		p_ptr->inside_special = 0;
	} else if (which == 2) {
		p_ptr->oldpy = p_ptr->py;
		p_ptr->oldpx = p_ptr->px;
	}

	if (p_ptr->rewards[13] == 0) {
		if ((which == 10) && (p_ptr->pclass != CLASS_WARRIOR)) {
			prt("Only fighters allowed inside.",0,0);
			return;
		} else if ((which == 11) && ((p_ptr->pclass != CLASS_MAGE)
		     && (p_ptr->pclass != CLASS_ROGUE) && (p_ptr->pclass != CLASS_RANGER))) {
			prt("Only mages, rogues, & rangers allowed inside.",0,0);
			return;
		} else if ((which == 12) && ((p_ptr->pclass != CLASS_PRIEST)
		     && (p_ptr->pclass != CLASS_PALADIN))) {
			prt("Only priests & paladins allowed inside.",0,0);
			return;
		} else if ((which == 13) && (p_ptr->pclass != CLASS_ROGUE)) {
			prt("Only rogues allowed inside.",0,0);
			return;
		} else if ((which == 14) && (p_ptr->pclass != CLASS_RANGER)) {
			prt("Only rangers allowed inside.",0,0);
			return;
		} else if ((which == 15) && (p_ptr->pclass != CLASS_PALADIN)) {
			prt("Only paladins allowed inside.",0,0);
			return;
		} else if ((which == 16) && (p_ptr->pclass != CLASS_ILLUSIONIST)) {
			prt("Only illusionists allowed inside.",0,0);
			return;
		}
	}

	forget_lite();
	forget_view();

	character_icky = TRUE;
	p_ptr->command_arg = 0;
	p_ptr->command_rep = 0;
	p_ptr->command_new = 0;

	display_bldg(which);
	leave_bldg = FALSE;

	while (!leave_bldg)
	{
		prt("",1,0);
		request_command(2);
		bldg_process_command();
		character_icky = TRUE;
	}

	/* have left building */
	p_ptr->energy_use = 0;
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
	p_ptr->update |= (PU_VIEW | PU_LITE);
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw entire screen */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}

