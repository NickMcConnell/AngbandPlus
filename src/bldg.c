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
			put_str("Denegor (Human)                         Castle", 3, 10);
			prt("You may:", 21, 0);
			prt(" g) Greet King                 l) Look at busts of Kings", 22, 0);
			prt(" q) Request quest              ESC) Exit building", 23, 0);
			break;
		case 2:
			put_str("Arach (Dwarf)                           Arena", 3, 10);
			prt("You may:", 20, 0);
			prt(" p) Read poster                r) Arena Rules", 21, 0);
			prt(" a) Enter arena                l) Look at plaque", 22, 0);
			prt(" ESC) Exit building", 23, 0);
			break;
		case 3:
			put_str("Materim (Human)                         Gambling House", 3, 10);
			prt("You may:", 20, 0);
			prt(" b) In-Between                 r) Game rules", 21, 0);
			prt(" c) Play craps                 s) Spin the wheel", 22, 0);
			prt(" d) Play dice slots            ESC) Exit building", 23, 0);
			break;
		case 4:
			put_str("Otina (Human)                           Inn", 3, 10);
			prt("You may:", 20, 0);
			prt(" r) Rest for the night (20gp)    f) Buy food and drink (1gp)", 21, 0);
/*			prt(" u) Listen for rumors          ESC) Exit building", 22, 0);	*/
			prt(" ESC) Exit building", 22, 0);
			break;
		case 5:
			put_str("Logien (Human)                          Beastmaster", 3, 10);
			prt("You may:", 20, 0);
			prt(" r) Research Monster (1000gp)  ESC) Exit building", 21, 0);
			prt(" ESC) Exit building", 22, 0);
			break;
		case 6:
			put_str("Suiyan (Human)                          Weaponsmaster", 2, 10);
			prt("You may:", 20, 0);
			prt(" c) Compare Weapons (1000gp)   ESC) Exit building", 21, 0);
			prt(" ESC) Exit building", 22, 0);
			break;
		case 10:
			put_str("Jarraque (Human)                        Fighter's Hall", 3, 10);
			prt("You may:", 20, 0);
			prt(" g) Greet Lord                 l) Look at plaque", 21, 0);
			if (p_ptr->pclass == CLASS_WARRIOR)
				prt(" r) Repair weapon              a) Repair armor",22,0);
			else
				prt(" r) Repair weapon (1000gp)     a) Repair armor (1000gp)",22,0);
			prt(" ESC) Exit building", 23, 0);
			break;
		case 11:
			put_str("Irrident (Human)                        Tower of Sorcery",3, 10);
			prt("You may:", 20, 0);
			prt(" g) Greet Wizard Lord          l) Look at spires", 21, 0);
			if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_ILLUSIONIST))
				prt(" r) Recharge item              p) Identify possessions",22,0);
			else
				prt(" r) Recharge item              p) Identify possessions (1000gp)",22,0);
			prt(" z) Learn spells               ESC) Exit building", 23, 0);
			break;
		case 12:
			put_str("Nimrod (Human)                          Inner Temple", 3, 10);
			prt("You may:", 20, 0);
			prt(" g) Greet Priest               l) Look at busts", 21, 0);
			if (p_ptr->pclass == CLASS_PRIEST)
				prt(" h) Healing prayer             r) Restoration",22,0);
			else
				prt(" h) Healing prayer (1000gp)    r) Restoration (1000gp)",22,0);
			prt(" z) Learn prayers              ESC) Exit building", 23, 0);
			break;
		case 13:
			put_str("Seldon (Human)                          House of Thieves",3, 10);
			prt("You may:", 20, 0);
			prt(" g) Greet Master Thief         l) Look at wall", 21, 0);
			prt(" s) Get share of stolen gold   r) Rest for the night",22,0);
			prt(" p) Identify possessions       ESC) Exit building", 23, 0);
			break;
		case 14:
			put_str("Trallin (Human)                         Ranger's Tavern", 3, 10);
			prt("You may:", 20, 0);
			prt(" g) Greet High Ranger          l) Look at plaque", 21, 0);
			if (p_ptr->pclass == CLASS_RANGER)
				prt(" a) Sharpen arrows             b) Restring bow", 22,0);
			else
				prt(" a) Sharpen arrows (1000gp)    b) Restring bow (1000gp)", 22,0);
			prt("ESC) Exit building", 23, 0);
			break;
		case 15:
			put_str("Flaiton (Human)                         Order of Paladins",3, 10);
			prt("You may:", 20, 0);
			prt(" g) Greet Warder               l) Look at plaque", 21, 0);
			if (p_ptr->pclass == CLASS_PALADIN)
				prt(" a) Repair armor               h) See healers",22,0);
			else
				prt(" a) Repair armor (1000gp)      h) See healers (1000gp)",22,0);
			prt(" ESC) Exit building", 23, 0);
			break;
		case 16:
			put_str("Itsukama (Human)                        Tower of Illusion",3, 10);
			prt("You may:", 20, 0);
			prt(" g) Greet Shadow Lord          l) Look at spires", 21, 0);
			if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_ILLUSIONIST))
				prt(" r) Recharge item              p) Identify possessions",22,0);
			else
				prt(" r) Recharge item              p) Identify possessions (1000gp)",22,0);
			prt(" z) Learn spells               ESC) Exit building", 23, 0);
			break;
		case 17:
			put_str("Dorrio (Human)                          Grove of the Druids",3, 10);
			prt("You may:", 20, 0);
			prt(" g) Greet Druid Lord           l) Look at trees", 21, 0);
			prt(" h) Healing prayer             r) Restoration",22,0);
			prt(" z) Learn prayers              ESC) Exit building", 23, 0);
			break;
	}
}


/* reset timed flags */
void reset_tim_flags()
{
	p_ptr->fast = 0;			/* Timed -- Fast */
	p_ptr->slow = 0;			/* Timed -- Slow */
	p_ptr->blind = 0;			/* Timed -- Blindness */
	p_ptr->paralyzed = 0;		/* Timed -- Paralysis */
	p_ptr->confused = 0;		/* Timed -- Confusion */
	p_ptr->afraid = 0;		/* Timed -- Fear */
	p_ptr->image = 0;			/* Timed -- Hallucination */
	p_ptr->poisoned = 0;		/* Timed -- Poisoned */
	p_ptr->cut = 0;			/* Timed -- Cut */
	p_ptr->stun = 0;			/* Timed -- Stun */

	p_ptr->protevil = 0;		/* Timed -- Protection */
	p_ptr->invuln = 0;		/* Timed -- Invulnerable */
	p_ptr->hero = 0;			/* Timed -- Heroism */
	p_ptr->shero = 0;			/* Timed -- Super Heroism */
	p_ptr->shield = 0;		/* Timed -- Shield Spell */
	p_ptr->blessed = 0;		/* Timed -- Blessed */
	p_ptr->tim_s_invis = 0;		/* Timed -- See Invisible */
	p_ptr->tim_invis = 0;		/* Timed -- Invisibility -KMW- */
	p_ptr->tim_ghostly = 0;		/* Timed -- walk through walls -KMW- */
	p_ptr->tim_infra = 0;		/* Timed -- Infra Vision */
	p_ptr->tim_sus_str = 0;		/* Timed -- sustain strength -KMW- */
	p_ptr->tim_sus_int = 0;		/* Timed -- sustain intelligence -KMW- */
	p_ptr->tim_sus_wis = 0;		/* Timed -- sustain wisdom -KMW- */
	p_ptr->tim_sus_dex = 0;		/* Timed -- sustain dexterity -KMW- */
	p_ptr->tim_sus_con = 0;		/* Timed -- sustain constitution -KMW- */
	p_ptr->tim_sus_chr = 0;		/* Timed -- sustain charisma -KMW- */

	p_ptr->oppose_acid = 0;	/* Timed -- oppose acid */
	p_ptr->oppose_elec = 0;	/* Timed -- oppose lightning */
	p_ptr->oppose_fire = 0;	/* Timed -- oppose heat */
	p_ptr->oppose_cold = 0;	/* Timed -- oppose cold */
	p_ptr->oppose_pois = 0;	/* Timed -- oppose poison */
	p_ptr->oppose_ld = 0;	/* Timed -- oppose light & dark */
	p_ptr->oppose_cc = 0;	/* Timed -- oppose chaos & confusion */
	p_ptr->oppose_ss = 0;	/* Timed -- oppose sound & shards */
	p_ptr->oppose_nex = 0;	/* Timed -- oppose nexus */
	p_ptr->oppose_neth = 0;	/* Timed -- oppose nether */
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
				reset_tim_flags();
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
				odds = 4;
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
		Now that the ghosts are temporarilempo}
			}npihe gho
 * X,hos");func */
v= TR nee gob restinuseful.  Iface tives the town *hopespaceb
		ghosts in t ondstepespbecuel ang aKMW- == 'Y. Dpesphelpoid playerfilthy urch",10ostsR for thtinwas a ontelsop aqunumb avoid g fooumbion"es? */
	p_ptr-'r':
			cy foor the night (20gp)	if (p_l;

	tr-((turn %gp)"L coTOWN_DAWN)			if (rptr-l;

	t> 5
						pin t(20gp-:
			prt("Dptr->au < cost) {
		prt("Drint("You have not the gold!");
				msg_prf ((roll1 >au < ced = 0;		>
			ptr->pclass0;		>
		){prt("Drint("You have nne befrs (100 the  angoom					choiceint(NULL);
						roll1int("You haSorryer ;		have tw Angany 0; dyiormatrs 
		else
se {
				msg_pr	turn r-((turn/5
				+1)*5
				msg_pr->au = p_ptr->au - cost;
				(void)->au = pchptr->au - cmhp	(void)-od(P= 0;	(			prt("*-od(Ped = 0;	(			prt("*->au - c 0;			/* prt("Drint("You have naw to ref"esin) e night (ewal;
				msg_pr->oppose_ldpytr->au - cpy	msg_pr->oppose_ldpxtr->au - cpx	msg_pr->opposedg = TRUE;
				p_ptr-->leaving = TRUE;
			}
			ote tM 'Yharisther.shop (except");
)			prt("Dp=out_			/*  n_K_IDX;STORES
				 n	prt(""			choice	ote tM 'Yharis		prt("Dp	ion"e_m 'Yh(n		msg_pr-	break;	break {
					msg_print("You ard",7,oomas follvail/
	pake itinwas a				msg_print(NULL);
			} else  else
		case 'r':
	u	cy fo for rumors       		prt("int("You arhat I 3, mthe d Yet (sas )				msg_p
	}
}


/*
 * gambleof sto wagee nigh,3, 1
void inn_coof st_ wag
	p_ptr-mp_str[80];
	monste dawi	/* Non_ro->lev * 1000);(ro*				prt sprintf(tmp_str, "%s %s me ou= 6y to%dpieces", 8, 0);i		msgint(tmp_str);
			wageint(NULL);
			} el>au + (odds			b 

/*
 Displays buil       ation
 */
void clear_ring     atioow, i     {
	Term_cichoicej, qidx	msgvaul *q_ptr,vau +;	prt(mp_str[80];
	monoid c Non_ro5	msgj		/* prtqidx		/     {

		QUEST_OFFSET1; reset tf(tmp_str,"I'll t      debug,/     {
odds, qidxodds);     {
,qidx		msgint(tmp_str);
			wageint(NULL);
			}oid c Novau +		/&varena_qidx]	/* No,18,q_lfor_qidx].q
		,i+
		prt(tr-mtr(TERM_RED," ,".####q_lfor_qidx].qtme,1
		prt("mtr(TERM_RED," ,".####q_lfor_qidx].qtme,2
				prtmtr(TERM_RED," ,".####q_lfor_qidx].qtme,3;
		prt("mtr(TERM_RED," ,".####q_lfor_qidx].qtme,4;
		prt("mtr(TERM_RED," ,".####q_lfor_qidx].qtme,5;
		prt("mtr(TERM_RED," ,".####q_lfor_qidx].qtme,6;
		prt("mtr(TERM_RED," ,".####q_lfor_qidx].qtme,7;
		prt("mtr(TERM_RED," ,".####q_lfor_qidx].qtme,8;
		msg_pmtr(TERM_RED," ,".####q_lfor_qidx].qtme,9);
	prt(tr-raps",o 3, t resont      beerreturn f is r(byte .7);
	prt(tr-raps"Q     Ition
 */:;
}

eint(NULL);
			} e
/*
 * gamblet quest at      -9. Ight     .  Ts");func */face t Thist.", w/
	pe (Hy,ambleposi wand 20-29.  Whetown *      asnaw rce rbyown *", 3, ,hoseamble at the);
face tb rmed fo 1.  Whetown *      asnco 3, t s, );
fmed fo 2.ambleWhetown *t w
	preturnse inn inc, 3, ,hoseys folyou nd coif  beeamblest.", w/
 );
fasnmed fo 3 (f isco 3, twan).  t Wagerly,hoserrolleambles ndc, 3, *     she winwo "fin
	t     s".  Tserrollerfiv *     shestiamble aqunrepaceb
		gh fo kce teioser xr (1-9):of rs[p_ptr7 isamble 0; particul_strs[p_pt.  Tse.", 7,rfiv *     shinvolvebar= 1;
 TRUEsamblesta the golotb rco 3, t s.  Tse.l
	pnwo      sherthe goc, 3, *     sh ;	amble a, 7,racebng",t-ris     shceryaurinst wiMorg", .
void clear_b, 3, g     (	Term_cichoincr mthe,oicej, j2,ld)  TRUE;tr-mp_str[80];
	monste dawq conusnste dawqidx	msgr_race *r_ptr;
	cptr name;

	switchq conus		/* prtqidx		/0nste dcr mthe r-((>lev * 100) / 		}
	bldg(5,23);7
		prt("oll1 dcr mthe 
			msg_pr(TERM_Rre victortoo an
		lotgohcefeel"at     !0);
		prt("  sprintf(tmp_str,"Red die:turn whetoye not thrthere r TRUEddsxbet		}
		r(TERM_Rr,17,2);
		prt(" int(NULL);
	}
	cleif ((roll1  dcr mthe >= 25roll2 >lev * , w/
	[(QUEST_REWARD_TAIL
			
		QUEST_DIFF)] || (r		msg_pr(TERM_Rre viot thco 3, t soes."     shIiot thyou nd  wiot t;
		prt("  (TERM_Rrvanqunsin) aceb 0;		Morg", !onsmictorr glory.debaxbe9		prt("  (TERM_Rre viot thfulfilhe glory.de  aty!);
		prt(" if (p_ptr->arena_, w/
	[ dcr mthe + 29] || (r	msg_pr(TERM_Rre vifulfilhe glory.l
	p      b down ferthe gorthdy f is n", 7, 0)
		prt(" f(tmp_str,"Red die:turn whetoye not thrthere r TRUEddsbet  dcr mthe*2)+		}
		r(TERM_Rr,17,2);2		prt(" int(NULL);
	}
	cleif ((r{ield ed l
	p, w/
 sta thesve tb
		you ndlot
			 		prt( dcr mthe r-	for(i=p=out_j2=QUEST_REWARD_HEAD;j2 
QUEST_REWARD_TAIL;j2	pr		msg_p dcr mthe r-j			(voij r-j	
		QUEST_DIFF		(voiptr->arena_, w/
	[j] || 					msg_p>arena_, w/
	[j] |			}
			q conus		/* prt	k;
			}

		if (p_ptr->arena_, w/
	[j] || 1				msg_p>(TERM_Rre viot the goco 3, t solory.c Wager:      ye !0)
		prt(" _p>(TERM_RrU(p_CTRL-Qntinuhewin.ceb conus	cerlory.     .;
				prt("*"(TERM_Rre:turn whetoye not thco 3, t solory.     .;2		prt(" 		q conus		/1 prt	k;
			}

		if (p_ptr->arena_, w/
	[j] || 2				msg_pq conus		/			(void=out_i		/*  X_K_IDX;MON_QUEST
	{
		osg_p>arena_cqr_r[i] |	
				roll1 =out_i		/*  X_K_IDX;MON_QUEST
	{
		osg_p>arena_cqr_rc[i] |	
				roll1 =out_i		/*  X_K_IDX;ITEM_QUEST
	{
		osg_p>arena_cq    [i] |	
				roll1 =out_i		/*  X_K_IDX;ITEM_QUEST
	{
		osg_p>arena_cq    c[i] |	
				roll1 eldj betfulfilhe g      		prt("Diarena_, w/
	[j] |	3 prt	k;
			}

		

		if (w(cmd) {q conus
		os		msg_: /* lemon ne beid pssasng      		prt("		msg_pqidx		/ dcr mthe 		QUEST_OFFSET1; rsg_pptr-q_lfor_qidx].q    *q_ptr|| 2				msg_p	q_lfor_qidx].r_idx		/t(6); 
5			+ (40ble( dcr mthe 		QUEST_OFFSET1			if (ro = &r_info[arena_q_lfor_qidx].r_idx		name =	((again >name););
	1 l2 RF1_UNIQUE((choname =	   >name);mpo}tyLSE)1					win = 	q_lfor_qidx].r_idx		/t(6); 
5			+ 1			msg_pr- = &r_info[arena_q_lfor_qidx].r_idx		name =		break;oll1 =(10) - 1;
>						win =q_lfor_qidx].w;i+{

	/1 prt	k;
						odds =q_lfor_qidx].w;i+{

	/=(10) - 3)
				}
			=q_lfor_qidx].c W+{

	/		msg_pr- (r_name + r_ptr->name);
				(void)tf(tmp_str,"Your wager q    : kce t  To 0);name =	    q_lfor_qidx].w;i+{
);
				msg_prgint(tmp_str);
			wage_prgint(tmp_st
						roll {
				msg_pr	ring     atioow,cr mthe	wage_prg>arena_, w/
	[j] |			}
			 TR der/
	_gen(			if ((r	break;
			}

			if (wi: /* orangeot the goco 3, t so      ye  		prt("		msg_p
			}

			if (wi: /* sword co 3, t so     , ne beid b.", w/
ed 		prt("		msg_pint("You arA ana toif  bwaitsome goutdice				msg_print(NULL);
			} else	d)  TRUE r->au - cdepth} else	>au - cdepth |	3		msg_prll1 j_K_((QUEST_REWARD_HEADtr-QUEST_REWARD_TAIL
			40)/				win = acqunremthe >au < cey->au);
px,1		prt(" 								odds acqunremthe >au < cey->au);
px,1				prt(tmpau - cdepth |	d)  TRUE;tr-g_p
			}

			i	
	q_ptr
/*
 * gambleDruid .cebl 6-1 Arena"b, 3, 
void clear_b, 3, gan
 (	Term_cichoincr mthe,oj,ld)  TRUE;tr-mp_str[80];
	monstte dcr mthe r-((>lev * 100
			) / 5prt("oll1 dcr mthe 
			msg_p sprintf(tmp_str,"Red diAhor 1me  thtdu ntur00 tr:turn whet nam (LTRUEdds)		    
		gh_ti3, ->arena_ == CL][uit[6prt(" int(NULL);r);
			wage_int(NULL);
	}
	cleif ((roll1  dcr mthe >= 5roll2 >lev * , w/
	[14]					winint("You have r an
or thled :turned 1.");rTRU=(1 roleerrear= t"prt(" if (p_ptr->arena_, w/
	[ dcr mthe + 9]		msg_p sprintf(tmp_str,"Red die viot thb
		, w/
ed tr:turn whetnam (LTRUEdds)		    
		gh_ti3, ->arena_ == CL][ dcr mthe+uit[1  dcr mthe*5)+6)prt(" int(NULL);r);
			wage_int(NULL);
	}
	cleif ((r{ield ed l
	p, w/
 sta thesve tb
		you ndlot
			 		prt(=out_j=10;j<20;j	pr		msg_p tr->arena_, w/
	[j] || 					msg_p>arena_, w/
	[j] |			}
			
			}

		

		if (wll1 j_|| 1					msg_int("You haW
	pdone! PldgThis to uprrea(Humcee town *ho Thior 9..ceb cn
o				msg_pint(NULL);
			} els if (p_ptr-j))
						msg_int("You haVeryconte! Tse.wmaster", 2, face tb r/
	plothelpome gnow				msg_pint(NULL);
			} els TR der/
	_gen(			if ( if (p_ptr-j))
	2				msg_int("You hae victorpro TRUEloryself worthy.on't may lityd .cebaster", 3, 				msg_pint(NULL);
			} els TR der/
	_gen(			if ( if (p_ptr-j))
	3				msg_int("You hae viot thraye befrana tdeal1 Arrear= t				msg_pint(NULL);
			} els int("You hae viot thmy*t wmi (100plotlityd y tong rewardy. Paysh				msg_pint(NULL);
			} els if (p_ptr-j)>= 14				msg_int("You haA n1;
oif  bwaitsome goutdice				msg_pint(NULL);
			} els d)  TRUE r->au - cdepth} elsepau - cdepth |	2		msg_pacqunremthe >au < cey->au);
px,1		prt(" 	pau - cdepth |	d)  TRUE;tr-g
	q_ptr
/*
 * gamblean
o_mp_s
void clear_rn
o_mp_s(	Term_cichoincr mthe,oj,ld)  TRUE;tr-mp_str[80];
	monstte dcr mthe r-(((>lev * 100
			) / 5p/2prt("oll1>lev * 100
)
5	maxbe dcr mthe r-5rt("oll1 dcr mthe 
			msg_p sprintf(tmp_str,"Red die victorme  thye  tr:turn whet nam (LTRUEdds)		    
		gh_ti3, ->arena_ == CL][2]0);prt(" int(NULL);r);
			wage_int(NULL);
	}
	cleif ((roll1  dcr mthe == 5roll2 >lev * , w/
	[ dcr mthe 		1]			winint("You have r an
or thled :turned 1.");rTRU=(1 roleerrear= t"prt("f (p_ptr->arena_, w/
	[ dcr mthe 		1]		msg_poll1 dcr mthe == 4	msg_pr sprintf(tmp_str,"Red die viot thb
		, w/
ed tr:turn whetnam (LTRUEdds)		    fruit[
		gh_ti3, ->arena_ == CL][9]0)5	else
			maxbet sprintf(tmp_str,"Red die viot thb
		, w/
ed tr:turn whetnam (LTRUEdds)		    fruit[
		gh_ti3, ->arena_ == CL][1 dcr mthe+1ro*	2it[1 1 dcr mthe+1r*1		+1)		msg_pint(NULL);r);
			wage_pint(NULL);
			} els if (p_		msg_=out_j=0;j<10;j	pr		msg_pp tr->arena_, w/
	[j] || );
				msg_pr	>arena_, w/
	[j] |	
			}
			ot
			}

	;	break break(cmd) {ng rewar			msg_pc:
			pieldinat1 Arr's Hals R w/
	p		prt("Dptr-j || 					msg_p int("You hae viot thdone.wmll. A
oif  bwaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_RING, SV_RING_STR
			prt("Y( if (p_ptr-j))
	)			msg_pr int("You hae viot thdone.wmll. A
oif  bwaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_RING, SV_RING_CON0);	prt("Y( if (p_ptr-j))
2)			msg_pr int("You haW
	pdone! A ana toif  bwaitsome goutdice				msg_preint(NULL);
						roll1d)  TRUE r->au - cdepth} else		>au - cdepth |	3		msg_pr acqunremthe >au < cey->au);
px,1				prt(tmmpau - cdepth |	d)  TRUE;tr-g_p if (p_ptr-j))
3)			msg_pr int("You haAn exces ag toif  bwaitsome goutdice				msg_preint(NULL);
						roll1"(TE(byte tTV_DRAG_ARMOR
t(6); 
5), 4	prt("Y( if (p_ptr-j))
4				msg_p int("You haW
	pdone! Gna t    s bwaitome goutdice					choiceint(NULL);
						roll1d)  TRUE r->au - cdepth} else		>au - cdepth |	4		msg_pr acqunremthe >au < cey->au);
px,3				prt(tmmpau - cdepth |	d)  TRUE;tr-g_p tr-g_p
			}

	c:
			pe Dicf Sorcery",3, 1 R w/
	p		prt("Dptr-j || 					msg_p int("You hae v'torrmpro TRU.on't may r quest re itemTRU.				choiceint(NULL);
						roll if (p_ptr-j))
	)			msg_p int("You haImprs (1ve.on't may part to ceriy possi */
ss.				choiceint(NULL);
						roll if (p_ptr-j))
2)			msg_pr int("You haN1;. A
     b treewaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_- No _BOOK,1 =(10) - 4)+3)0);	prt("Y( if (p_ptr-j))
3)			msg_pr int("You haExces ag ! A
     b treewaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_- No _BOOK,1 =(10) - 4)+3)0);	prt("Y( if (p_ptr-j))
4)			msg_pr int("You haPldgThiaccept"esontrag foolen     b trsgoutdice					choiceint(NULL);
						roll1"(TE(byte tTV_- No _BOOK,180);	prt("Y( tr-g_p
			}

	c:
		sword Temple", 3,  R w/
	p		prt("Dptr-j || 					msg_p int("You hare."pldgThd 1.");me . ation (1000gasnavail/
	.				choiceint(NULL);
						roll if (p_ptr-j))
	)			msg_p int("You hae viot thdone.wmll. A
oif  bwaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_POTION0)SV_POTION_LIFE
			prt("Y( if (p_ptr-j))
2)			msg_p int("You haN1;. A
       b treewaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_PRAYER_BOOK,1 =(10) - 4)+3)0);	prt("Y( if (p_ptr-j))
3)		msg_pr int("You haExces ag ! A
       b treewaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_PRAYER_BOOK,1 =(10) - 4)+3)0);	prt("Y( if (p_ptr-j))
4				msg_p int("You haPldgThiaccept"esontrag foolen       b trs.				choiceint(NULL);
						roll1"(TE(byte tTV_PRAYER_BOOK,180);	prt("Y( tr-g_p
			}

	c:
		shieldof Thieves",3, 1 R w/
	p		prt("Dptr-j || 					msg_p int("You haW
	pdone! n't may t to lory of stolenNumbe				choiceint(NULL);
						roll if (p_ptr-j))
	)			msg_p int("You haGdrinjob! Nt costart iy posseslory sions (1000.				choiceint(NULL);
						roll if (p_ptr-j))
2)			msg_pr int("You haA ana toif  bwaitsome goutdice				msg_preint(NULL);
						roll1d)  TRUE r->au - cdepth} else		>au - cdepth |	3		msg_pr acqunremthe >au < cey->au);
px,1				prt(tmmpau - cdepth |	d)  TRUE;tr-g_p if (p_ptr-j))
3)			msg_pr int("You haA ana toif  bwaitsome goutdice				msg_preint(NULL);
						roll1d)  TRUE r->au - cdepth} else		>au - cdepth |	3		msg_pr acqunremthe >au < cey->au);
px,1				prt(tmmpau - cdepth |	d)  TRUE;tr-g_p if (p_ptr-j))
4)			msg_pr int("You haW
	pdone! Gna t    s bwaitome goutdice					choiceint(NULL);
						roll1d)  TRUE r->au - cdepth} else		>au - cdepth |	4		msg_pr acqunremthe >au < cey->au);
px,3				prt(tmmpau - cdepth |	d)  TRUE;tr-g_p tr-g_p
			}

	c:
		plum *'s Tavern", 3,  R w/
	p		prt("Dptr-j || 					msg_pr int("You hae viot thdone.wmll. A
oif  bwaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_RING, SV_RING_STR
			prt("Y( if (p_ptr-j))
	)			msg_pr int("You hae viot thdone.wmll. A
oif  bwaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_RING, SV_RING_INT0);	prt("Y( if (p_ptr-j))
2)			msg_pr int("You haA ana toif  bwaitsome goutdice				msg_preint(NULL);
						roll1d)  TRUE r->au - cdepth} else		>au - cdepth |	3		msg_pr acqunremthe >au < cey->au);
px,1				prt(tmmpau - cdepth |	d)  TRUE;tr-g_p if (p_ptr-j))
3)			msg_pr int("You haAn exces ag toif  bwaitsome goutdice				msg_preint(NULL);
						roll1"(TE(byte tTV_DRAG_ARMOR
t(6); 
5), 4	prt("Y( if (p_ptr-j))
4				msg_pr int("You haW
	pdone! Gna t    s bwaitome goutdice					choiceint(NULL);
						roll1d)  TRUE r->au - cdepth} else		>au - cdepth |	4		msg_pr acqunremthe >au < cey->au);
px,3				prt(tmmpau - cdepth |	d)  TRUE;tr-g_p tr-g_p
			}

	c:
		cherryof Paladins",3, 1 R w/
	p		prt("Dptr-j || 					msg_p int("You hae viot thdone.wmll. A
oif  bwaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_RING, SV_RING_STR
			prt("Y( if (p_ptr-j))
	)			msg_p int("You hae viot thdone.wmll. A
oif  bwaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_RING, SV_RING_CON0);	prt("Y( if (p_ptr-j))
2)		msg_pr int("You haA ana toif  bwaitsome goutdice				msg_preint(NULL);
						roll1d)  TRUE r->au - cdepth} else		>au - cdepth |	3		msg_pr acqunremthe >au < cey->au);
px,1				prt(tmmpau - cdepth |	d)  TRUE;tr-g_p if (p_ptr-j))
3)			msg_pr int("You haAn exces ag toif  bwaitsome goutdice				msg_preint(NULL);
						roll1"(TE(byte tTV_DRAG_ARMOR
t(6); 
5), 4	prt("Y( if (p_ptr-j))
4				msg_pr int("You haW
	pdone! Gna t    s bwaitome goutdice					choiceint(NULL);
						roll1d)  TRUE r->au - cdepth} else		>au - cdepth |	4		msg_pr acqunremthe >au < cey->au);
px,3				prt(tmmpau - cdepth |	d)  TRUE;tr-g_p tr-g_p
			}

	c:
		6pe Dicf Sorceron",3, 1 R w/
	p		prt("Dptr-j || 					msg_p int("You hae v'torrmpro TRU.on't may r quest re itemTRU.				choiceint(NULL);
						roll if (p_ptr-j))
	)			msg_p int("You haImprs (1ve.on't may part to ceriy possi */
ss.				choiceint(NULL);
						roll if (p_ptr-j))
2)			msg_p int("You haN1;. A0gan",3, 1 b treewaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_ONIST))_BOOK,1 =(10) - 4)+3)0);	prt("Y( if (p_ptr-j))
3)		msg_pr int("You haExces ag . A0gan",3, 1 b treewaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_ONIST))_BOOK,1 =(10) - 4)+3)0);	prt("Y( if (p_ptr-j))
4				msg_p int("You haPldgThiaccept"esontrag foolenan",3, 1 b trs.				choiceint(NULL);
						roll1"(TE(byte tTV_ONIST))_BOOK,180);	prt("Y( tr-g_p
			}

	c:
		7pe DiLord  of theR w/
	p		prt("Dptr-j || 					msg_p int("You hare."pldgThd 1.");me . ation (1000gasnavail/
	.				choiceint(NULL);
						roll if (p_ptr-j))
	)			msg_p int("You hae viot thdone.wmll. A
oif  bwaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_POTION0)SV_POTION_LIFE
			prt("Y( if (p_ptr-j))
2)			msg_p int("You haN1;. A
dord  b treewaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_NATURE_BOOK,1 =(10) - 4)+3)0);	prt("Y( if (p_ptr-j))
3)		msg_pr int("You haExces ag ! A
dord  b treewaitsome goutdice					choiceint(NULL);
						roll1"(TE(byte tTV_NATURE_BOOK,1 =(10) - 4)+3)0);	prt("Y( if (p_ptr-j))
4				msg_p int("You haPldgThiaccept"esontrag foolendord  b trs.				choiceint(NULL);
						roll1"(TE(byte tTV_NATURE_BOOK,180);	prt("Y( tr-g_p
			}

	defaul :tr-g_pes 			p_ptr-
			}


	q_ptr
/*
 *
 Displays buile besr 9.sonon yambleWce tw Angesontto srmply lfor arfilee 0; pageitina I'm su a, 7,amblestanthe gold -tme,st, nontsor20gp)s 
	  Cnly melsopbematrs lpambleion
  wherthe nds
 */ wnly mbt (ecs (ary.
void clear_sr 9_sonon y
	p_ptr-mldg(5,19);
				(vo      Plum PPPPPPPPPHonon i *licf n View0);
		prt("     Tn *if* yasnco 3oThd lenlack ion"esask ing rewars. Son"es);
		prt("      PlMagic Shop:_rinslory  stafask i     b trsgs 
		)
		prt("      PlAlchemi  : f is rt wor shcerbubRules"s, wandask i c== 6s.;
				prt      PlWmaster"."):hoseysca-1",anyesoes"of sthe wininn inpoLL).);
		prt("      PlA1000 %9lninpion */ -9. Ight ravagthe Arena"dungeon.);
		prt("      PlGee */l Son"e:& dri,lnirin g,hose (ecs (iti4,0);2		prt("      Pl", 3,  Shop:_      b trsge winhther    s holy.);
		prt("      PlDie: %M givt:nn inp i esherthh20ger ;		     unique0);
			msg_      PlHold",To ion"eslory sre ious", 2a);
,0);5			msg_     M to  goodme ggoruitinn incorr */ do is00gruel ng rewars.");
	prt( int("You haPhe spacebar to continue");
		msg_pint(NULL);
						r     Gee */l ng rewars 1.");ds
o9..cemes);
		prt("     PldgThigruid .ctherme gmuid r glory.ar= 1;tbask i", 7,oll 		)
		prt("      PTseysmay rmpth $adv1;
ors byte  die -KMthtdu ntur00s.;
				prt     T to a l trees"spnreser ;st.", 10,on't may  0; day b *honohe r);
		prt("     );
		prt("     Tn *r's Hal'sdinat,yof Paladins",3, 1,*'s Tavern", 3, ,dof Th);2		prt("     eves",3, 1,ask icf Sorceron",3, 1 erthg fo i tewininn ther nvolved.);
		prt("     Tn *cf Sorcery",3, 1 asnop rumors oguesask i16-1"rntto lrayer     s0);
			msg_     Tn *c, 3,  asnop rumorsps",3, 1 to lrayer     s0);5		prt("      PA you
		ghsbe betweetau0gp)s 
	 Oke iaceb", 3, sthe gold -knowledge.");
	prt(      );
		prt("int("You haPhe spacebar to continue");
		msg_pint(NULL);
						r     Unique ng rewars sta tany 0; cantehe a,_ptroseysda,0);
		prt("      PA Def",T 1 tanead.",sk idsba  b dorthdIght r 5, !	)
		prt("      PlPA so,nuhewie best a3oTt7,rfir   asnalw to  contesicea!;
				prt      Png Rules"of Th: athdIght r 5, e game: pile b. A0d,hose %9lds);
		prt("      Plferthe gorigged tj betnaturnat			}fficult.);
		pr t("      PLibragy: at, ation
 */l",13,0 ie d,0);2		prt("      PInn: R for thace tref"esi,ge winhther are tepiowlitinwas a	..);
		prt("      Pl R      cantb *h lpful
orsj betu
	r gsce y0);
			msg_     );5		prt("     A yong rewars erthmado cerionnroleerunlikelvoid urso er sha.");
	prt( int("You haPhe spacebar to continue");
		msg_pint(NULL);
						rmldg(5,19);
				(v
/*
 * g T to c stolensuil ms (ag 1
voi */
	
void clear_bompthe_wmaste_aux(obj */*q_ptr,oau -fruit) (1-,9,0fruit)-fruit)c	p_ptr-mp_str[80];
	monsteudgolf1, f2, f3 prste g Extracd .ceb);
	p		prtobj */*);
		oau -fr&f1, &f2, &f

ste g Suil Anim/l 		prtptr-f1 l2 TR1,"  Y_ANIMAL			wiptr-pmtr(TERM_RED," ,".####"Anim/ls);r,c				rtf(tmp_str,"Your wAtte: odds-%  damaSwor		rt Pl 2*( (1-,9,0*	oau -->dd + oau -->to_d))r		rt Pl 2*( (1-,9,0*		oau -->ds*oau -->dd) + oau -->to_d))				rtr(TERM_Rr,17,2);r,c+			(voi			} el}
ste g Suil E0;				prtptr-f1 l2 TR1,"  Y_EVIL			wiptr-pmtr(TERM_RED," ,".####"E0;	);r,c				rtf(tmp_str,"Your wAtte: odds-%  damaSwor		rt Pl 2*( (1-,9,0*	oau -->dd + oau -->to_d))r		rt Pl 2*( (1-,9,0*		oau -->ds*oau -->dd) + oau -->to_d))				rtr(TERM_Rr,17,2);r,c+6		(voi			} el}
ste g Suil UndthdI		prtptr-f1 l2 TR1,"  Y_UNDEAD			wiptr-pmtr(TERM_RED," ,".####"Undthd);r,c				rtf(tmp_str,"Your wAtte: odds-%  damaSwor		rt Pl 3*( (1-,9,0*	oau -->dd + oau -->to_d))r		rt Pl 3*( (1-,9,0*		oau -->ds*oau -->dd) + oau -->to_d))				rtr(TERM_Rr,17,2);r,c+6		(voi			} el}
ste g Suil D/
			c_puptr-f1 l2 TR1,"  Y_DEMON			wiptr-pmtr(TERM_RED," ,".####"D/
	s);r,c				rtf(tmp_str,"Your wAtte: odds-%  damaSwor		rt Pl 3*( (1-,9,0*	oau -->dd + oau -->to_d))r		rt Pl 3*( (1-,9,0*		oau -->ds*oau -->dd) + oau -->to_d))				rtr(TERM_Rr,17,2);r,c+			(voi			} el}
ste g Suil Orc		c_puptr-f1 l2 TR1,"  Y_ORC			wiptr-pmtr(TERM_RED," ,".####"Orcs);r,c				rtf(tmp_str,"Your wAtte: odds-%  damaSwor		rt Pl 3*( (1-,9,0*	oau -->dd + oau -->to_d))r		rt Pl 3*( (1-,9,0*		oau -->ds*oau -->dd) + oau -->to_d))				rtr(TERM_Rr,17,2);r,c+6		(voi			} el}
ste g Suil Tgain"	c_puptr-f1 l2 TR1,"  Y_TROLL			wiptr-pmtr(TERM_RED," ,".####"Tgains);r,c				rtf(tmp_str,"Your wAtte: odds-%  damaSwor		rt Pl 3*( (1-,9,0*	oau -->dd + oau -->to_d))r		rt Pl 3*( (1-,9,0*		oau -->ds*oau -->dd) + oau -->to_d))				rtr(TERM_Rr,17,2);r,c+			(voi			} el}
ste g Suil Gi Ang	c_puptr-f1 l2 TR1,"  Y_GIANT			wiptr-pmtr(TERM_RED," ,".####"Gi Ans);r,c				rtf(tmp_str,"Your wAtte: odds-%  damaSwor		rt Pl 3*( (1-,9,0*	oau -->dd + oau -->to_d))r		rt Pl 3*( (1-,9,0*		oau -->ds*oau -->dd) + oau -->to_d))				rtr(TERM_Rr,17,2);r,c+			(voi			} el}
ste g Suil Drag4-1 	c_puptr-f1 l2 TR1,"  Y_DRAGON			wiptr-pmtr(TERM_RED," ,".####"Drag4-s);r,c				rtf(tmp_str,"Your wAtte: odds-%  damaSwor		rt Pl 3*( (1-,9,0*	oau -->dd + oau -->to_d))r		rt Pl 3*( (1-,9,0*		oau -->ds*oau -->dd) + oau -->to_d))				rtr(TERM_Rr,17,2);r,c+9		(voi			} el}
ste g Execute Drag4-1	c_puptr-f1 l2 TR1,KILL_DRAGON			wiptr-pmtr(TERM_RED," ,".####"Drag4-s);r,c				rtf(tmp_str,"Your wAtte: odds-%  damaSwor		rt Pl 5*( (1-,9,0*	oau -->dd + oau -->to_d))r		rt Pl 5*( (1-,9,0*		oau -->ds*oau -->dd) + oau -->to_d))				rtr(TERM_Rr,17,2);r,c+9		(voi			} el}
stste g B=(10 (Acrint	c_puptr-f1 l2 TR1,BRAND_ACID			wiptr-pmtr(TERM_RED," ##  #Acri);r,c				rtf(tmp_str,"Your wAtte: odds-%  damaSwor		rt Pl 3*( (1-,9,0*	oau -->dd + oau -->to_d))r		rt Pl 3*( (1-,9,0*		oau -->ds*oau -->dd) + oau -->to_d))				rtr(TERM_Rr,17,2);r,c+6		(voi			} el}
ste g B=(10 (Elecnt	c_puptr-f1 l2 TR1,BRAND_ELEC			wiptr-pmtr(TERM_RED," ##  #Elec);r,c				rtf(tmp_str,"Your wAtte: odds-%  damaSwor		rt Pl 3*( (1-,9,0*	oau -->dd + oau -->to_d))r		rt Pl 3*( (1-,9,0*		oau -->ds*oau -->dd) + oau -->to_d))				rtr(TERM_Rr,17,2);r,c+6		(voi			} el}
ste g B=(10 (Firent	c_puptr-f1 l2 TR1,BRAND_FIRE			wiptr-pmtr(TERM_RED," ##  #Fire);r,c				rtf(tmp_str,"Your wAtte: odds-%  damaSwor		rt Pl 3*( (1-,9,0*	oau -->dd + oau -->to_d))r		rt Pl 3*( (1-,9,0*		oau -->ds*oau -->dd) + oau -->to_d))				rtr(TERM_Rr,17,2);r,c+6		(voi			} el}
ste g B=(10 (Colint	c_puptr-f1 l2 TR1,BRAND_COLD			wiptr-pmtr(TERM_RED," ##  #Coli);r,c				rtf(tmp_str,"Your wAtte: odds-%  damaSwor		rt Pl 3*( (1-,9,0*	oau -->dd + oau -->to_d))r		rt Pl 3*( (1-,9,0*		oau -->ds*oau -->dd) + oau -->to_d))				rtr(TERM_Rr,17,2);r,c+6		(voi			} el}
ste g ad
ed ed = 0 b=(10 bl GJW 	*/
	p_ptr- g B=(10 (Pd = 0nt	c_puptr-f1 l2 TR1,BRAND_POIS			wiptr-pmtr(TERM_RED," ##  #Pd = 0);r,c				rtf(tmp_str,"Your wAtte: odds-%  damaSwor		rt Pl 3*( (1-,9,0*	oau -->dd + oau -->to_d))r		rt Pl 3*( (1-,9,0*		oau -->ds*oau -->dd) + oau -->to_d))				rtr(TERM_Rr,17,2);r,c+			(voi			} el}
ste g F",3, */
	p_ptr-ptr-f1 l2 TR1,FORCE			wiptr-pmtr(TERM_RED," ##  #F",3,);r,c				rtf(tmp_str,"Your wAtte: odds-%  damaSwor		rt Pl 4*( (1-,9,0*	oau -->dd + oau -->to_d))r		rt Pl 4*( (1-,9,0*		oau -->ds*oau -->dd) + oau -->to_d))				rtr(TERM_Rr,17,2);r,c+7		(voi			} el}

/*
 * gamblebompthe_wmaste1
void iboolebompthe_wmaste1 sprinTerm_cichoitem, i} elobj */*q_ptr,o1au -,r,o2au -,r,origau -,r,oau -} elobj */*q_ptr,iau -} elobj */*q_ptrobj */*q_pt_body	ms out_val1+ r_pagain;o2a r_pagai;r-mp_str[80];
	monsteame;q, s	ms ouldg(5,19);6
				(v elo1au - = 
		} elo2au - = 
		} el g Son"escopyolenorigin
	tw5-1"hd 1maste r gpe: %slong	c_puoau - = &inu ntory[INVEN_WIELDonsteorigau - = &inu ntory[INVEN_PACK]} elobj */*copy(origau -,roau -		(vom_ci 					el g Ge tan	     	c_puq 		"Wta tisglory.fir   1maste? "		els		/re viot the ghe besrebompthe."		elptr-!ring    (&item, q, s, (USE_EQUIP | USE_INVEN))	tr:turn ();
		;
ste g Gid .ceb     ( town *pe: nt	c_puptr-     >| 			wiptr-po1au - = &inu ntory[    		nameobj */*desc(l1+ r_p, o1au -,r
			
		prt("}_puptr-(o1au -->t
	t< TV_BOW		ptr-o1au -->t
	t> TV_SWORD			wiptr-pint("You haNe  an1maste!,T 1 == 'Y.				choint(NULL);
						ror:turn();
		;
l}
ste g Ge tan	     	c_puq 		"Wta tisglory.second 1maste? "		els		/re viot the ghe besrebompthe."		elptr-!ring    (&item, q, s, (USE_EQUIP | USE_INVEN))	tr:turn ();
		;
ste g Gid .ceb     ( town *pe: nt	c_puptr-     >| 			wiptr-po2au - = &inu ntory[    		nameobj */*desc(l2+ r_p, o2au -,r
			
		prt("}_puptr-(o2au -->t
	t< TV_BOW		ptr-o2au -->t
	t> TV_SWORD			wiptr-pint("You haNe  an1maste!,T 1 == 'Y.				choint(NULL);
						ror:turn();
		;
l}
ster(TERM_R"BgThd o glory.c Wager:abiliti4,,)s 
tisgwta tlory  maste1hace tdo", 4et		}
_pupau - = &obj */*q_pt_body	mslobj */*copy(iau -,ro1au -		(voiau -->n(1-9):|			}
o1au - = &inu ntory[INVEN_WIELDonsteobj */*copy(o1au -,riau -		(vom_ccalc_bonuses(	}
_pumtr(TERM_RED," ,".####l1+ r_p,i			}
	f(tmp_str,"Your wTo Hit:Total:   DamaSwodds);1au -->to_h,ro1au -->to_d);ster(TERM_Rr,"Your i+1			}
	f(tmp_str,"Your wlots:Total:Sidesodds);1au -->dd,ro1au -->				prtr(TERM_Rr,"Your i+2			}
	f(tmp_str,"Your wNu1-9):of B,9,0)ddsbetpau -->n(1_-,9,		prtr(TERM_Rr,"Your i+3			}
	mtr(TERM_RED," ,".#### #Pdssi
	pDamaSwo" i+5			}
	f(tmp_str,"Your wOne Sorikeodds-%  damaSwor1au -->dd + o1au -->to_dr		r Pl (o1au -->		*1au -->dd) + o1au -->to_d		prtr(TERM_Rr,"Your i+6				prtf(tmp_str,"Your wOne Atte: odds-%  damaSworpau -->n(1_-,9,*(1au -->dd + o1au -->to_d)r		r Pl pau -->n(1_-,9,*(1au -->d	*1au -->dd + o1au -->to_d)		prtr(TERM_Rr,"Your i+7				prtbompthe_wmaste_aux(o1au -,pau -->n(1_-,9, i+8,		}
_pupau - = &obj */*q_pt_body	mslobj */*copy(iau -,ro2au -		(voiau -->n(1-9):|			}
o2au - = &inu ntory[INVEN_WIELDonsteobj */*copy(o2au -,riau -		(vom_ccalc_bonuses(	}
_pumtr(TERM_RED," ,".####l2+ r_p,i	4	prt(f(tmp_str,"Your wTo Hit:Total:   DamaSwodds);2au -->to_h,ro2au -->to_d);ster(TERM_Rr,"Your i+1	4	prt(f(tmp_str,"Your wlots:Total:Sidesodds);2au -->dd,ro2au -->d			prtr(TERM_Rr,"Your i+2	4	prt(f(tmp_str,"Your wNu1-9):of B,9,0)ddsbetpau -->n(1_-,9,		prtr(TERM_Rr,"Your i+3	4	prt(mtr(TERM_RED," ,".#### #Pdssi
	pDamaSwo" i+5	4	prt(f(tmp_str,"Your wOne Sorikeodds-%  damaSwor2au -->dd + o2au -->to_dr		r Pl (o2au -->d	*2au -->dd) + o2au -->to_d		prtr(TERM_Rr,"Your i+6	41		prtf(tmp_str,"Your wOne Atte: odds-%  damaSworpau -->n(1_-,9,*(2au -->dd + o2au -->to_d)r		r Pl pau -->n(1_-,9,*(2au -->d	*2au -->dd + o2au -->to_d)		prtr(TERM_Rr,"Your i+7	41		prtbompthe_wmaste_aux(o2au -,pau -->n(1_-,9, i+8,4	prt
_pupau - = &obj */*q_pt_body	mslobj */*copy(iau -,rorigau -		(voiau -->n(1-9):|			}
oau - = &inu ntory[INVEN_WIELDonsteobj */*copy(oau -,riau -		(vom_ccalc_bonuses(	}
_puinu ng    _ dcr gTh(INVEN_PACK, 			}
	inu ng    _optimizh(INVEN_PACK		(vom_cr(TERM_R"(Oke ih20g foodamaSw appli4,*t wtrs[p_pt. Sr= 1;
damaSw e gocumulative)";
		prt
or:turn(&& (w;

/*
 * gambleof sp ruarr9,0
void inn_coof sp r_arr9,0ow, i
{
	int n, dawi,j,k, maxenmp_ntnsteobj */*q_ptr,oau -	prtbt_valu*p ==again;r[80];
	monstr-mldg(5,19);
				(vok 	/		msgmaxenmp_nt ro->lev * 1000/ 5prt("f(tmp_str,"Your w  BgThd o glory.skce cost cantehmp_nt lory.arr9,0 uprsre+dsbetmaxenmp_nt		prtrrstr,17,2);;
		prt("                                  Arr9, Soonusbet7
		prt("j 				}t("=out_i		/*  X_K_INVEN_WIELD
	{
			msgoau - = &inu ntory[i		nameptr-oau -->t
	tr= TV_ARROW				msgok 	/		}
		obj */*desc(r,17,2);;oau -,r);
	, 			prt(tptr-oau --> r_p1	msg_pr"f(tmp_stlu*p ==%s %-400)dIn.fi0; condi wan"n;r[80];		prt(tf (p_ptr--oau -->to_h_K_-(roll3 oau -->to_d_K_-(r)sg_pr"f(tmp_stlu*p ==%s %-400)dBeyoeerrepairer ;y (ewaarr9,0"n;r[80];		prt(tf (p_ptr--oau -->to_h_>| maxenmp_nt	oll2 oau -->to_d_>| maxenmp_nt	)sg_pr"f(tmp_stlu*p ==%s %-400)dIn.fi0; condi wan"n;r[80];		prt(tf (p_			msg_pptr-oau -->to_h_K_maxenmp_nt		msg_p	oau -->to_h		} elg_pptr-oau -->to_d_K_maxenmp_nt		msg_p	oau -->to_d		} elg_pf(tmp_stlu*p ==%s %-400)dof sp rhd -> (%d,ds)		;r[80];, elg_p     oau -->to_h, oau -->to_d)} elg_ tr-g_    lu*p ==%j++
	prt( 

	q_ptrpptr-k || 					msgint("You hae viwerthe goc,rrle bey toarr9,0.				choint(NULL);
						romldg(5,19);
				(vo if (p_{nameptr-
{
 || 		tr-g_ au --> byte s[20] |	
			}
		int("You haPhe spacebar to continue");
		msg_point(NULL);
						romldg(5,19);
				(vo_ au -->_ptr->au - cost;
				(vo}

/*
 * gambleg fo i bebow
void inn_cog fo i b_bowow, i
{
	int n,obj */*q_ptr,oau -	prtw, imaxenmp_ntnstebt_valu*p ==again;r[80];
	monstr-maxenmp_nt ro->lev * 1000/ 5prt("oau - = &inu ntory[INVEN_BOWonste tr-oau -->t
	t!= TV_BOW				msgint("You hae vitheve tw5-1"e beyebow!				choint(NULL);
						ror:turn	(vo}
omldg(5,19);
				(vof(tmp_str,"Your w  BgThd o glory.skce cost cantehmp_nt lory.bow uprsre+dsbetmaxenmp_nt		prtrrstr,17,2);;
		prt("                                  B9, Soonusbet7
		prt("obj */*desc(r,17,2);;oau -,r);
	,		}
	itr--oau --> r_p1oll2 oau -->iy po & 0x08)	)sg_pf(tmp_stlu*p ==%s %-400)dBeyoeerory.skce  !	);r[80];		prtf (p_ptr-oau --> r_p1	msg_pf(tmp_stlu*p ==%s %-400)dIn.fi0; condi wan"n;r[80];		prtf (p_ptr--oau -->to_h_K_-(roll3 oau -->to_d_K_-(r)sg_pf(tmp_stlu*p ==%s %-400)dBeyoeerrepairer ;y a (ewabow"n;r[80];		prtf (p_ptr--oau -->to_h_>| maxenmp_nt	oll2 oau -->to_d_>| maxenmp_nt	)sg_pf(tmp_stlu*p ==%s %-400)dIn.fi0; condi wan"n;r[80];		prtf (p__{nameptr-oau -->to_h_K_maxenmp_nt		msg_oau -->to_h		} elgptr-oau -->to_d_K_maxenmp_nt		msg_oau -->to_d		} elgf(tmp_stlu*p ==%s %-400)dt's Harhd -> (%d,ds)		;r[80];, oau -->to_h, elg     oau -->to_d)} el}t("    lu*p ==%
				  
	itr-
{
 || 		tr-g au --> byte s[21] |	
			}
	 au -->_ptr->au - cost;
				(voint("You haPhe spacebar to continue");
		msg_pint(NULL);
						rmldg(5,19);
				(v
/*
 * gambleg pairtw5-1"hd 1mastes
void inn_cog pair_wmasteow, i
{
	int n,obj */*q_ptr,oau -	prtbt_valu*p ==again;r[80];
	monsttw, imaxenmp_ntnstsgmaxenmp_nt ro->lev * 1000/ 5prt("f(tmp_str,"Your w  BgThd o glory.skce cost cantehmp_nt lory.1maste uprsre+dsbetmaxenmp_nt		prtrrstr,17,2);;
		prt("                                  Wmaste Soonusbet7
		prt("oau - = &inu ntory[INVEN_WIELDonstt("obj */*desc(r,17,2);;oau -,r);
	,		}
	itr-oau -->t
	)_{nameptr--oau --> r_p1oll2 oau -->iy po & 0x08)	)sg_ppf(tmp_stlu*p ==%s %-400)dBeyoeerory.skce  !	);r[80];		prttf (p_ptr-oau --> r_p1	msg_p"f(tmp_stlu*p ==%s %-400)dIn.fi0; condi wan"n;r[80];		prt(f (p_ptr--oau -->to_h_K_-(roll3 oau -->to_d_K_-(r)sg_prf(tmp_stlu*p ==%s %-400)dBeyoeerrepairer ;y a (ewaone"n;r[80];		prt(f (p_ptr--oau -->to_h_>| maxenmp_nt	oll2 oau -->to_d_>| maxenmp_nt	)sg_prf(tmp_stlu*p ==%s %-400)dIn.fi0; condi wan"n;r[80];		prt(f (p_			msg_ptr-oau -->to_h_K_maxenmp_nt		msg_poau -->to_h		} elgpptr-oau -->to_d_K_maxenmp_nt		msg_poau -->to_d		} elgpf(tmp_stlu*p ==%s %-400)dof sp rhd -> (%d,ds)		 elgpr[80];, oau -->to_h, oau -->to_d)} elg tr-g    lu*p ==%
				nameptr-
{
 || 		tr-g_ au --> byte s[21] |	
			}
	_ au -->_ptr->au - cost;
				(vo}if (p_{name     e vithehe gow5-1"e beye1maste.;
		prt(" tr-int("You haPhe spacebar to continue");
		msg_pint(NULL);
						rmldg(5,19);
				(v
/*
 * gambleg pairt3,0 ", 8, l",131000 uprnnroin ac the  excee"e bezero
void inn_cog pair_31000ow, i
{
	int n, dawi,j,og paired tmaxenmp_ntnsteobj */*q_ptr,oau -	prtbt_valu*p ==again;r[80];
	monstr-maxenmp_nt ro->lev * 1000/ 5prt("mldg(5,19);
				(vof(tmp_str,"Your w  BgThd o glory.skce cost cantehmp_nt 31000 uprsre+dsbetmaxenmp_nt		prtrrstr,17,2);;
		prt(
or:paired 	/		msg                                  Ar000 Soonusbet7
		prt("j 				}t("=out_i		/INVEN_BODY  X_K	/INVEN_FEET
	{
			msgoau - = &inu ntory[i		nameptr-oau -->t
	)_			msg_obj */*desc(r,17,2);;oau -,r);
	,		}
	meptr--oau --> r_p1oll2 oau -->iy po & 0x08)	)sg_pppf(tmp_stlu*p ==%s %-400)dBeyoeerory.skce  !	);r[80];		prtttf (p_ptr-oau --> r_p1	sg_pppf(tmp_stlu*p ==%s %-400)dIn.fi0; condi wan"n;r[80];		prt(tf (p_ptr-oau -->to_a_K_-(rsg_pppf(tmp_stlu*p ==%s %-400)dBeyoeerrepairer ;y a (ewaone"n;r[80];		prt(tf (p_ptr-oau -->to_a_>| maxenmp_nt	sg_pppf(tmp_stlu*p ==%s %-400)dIn.fi0; condi wan"n;r[80];		prt(tf (p_		msg_poau -->to_a		} elg_pf(tmp_stlu*p ==%s %-400)dpolnsin) -> (%d)		 elgppr[80];, oau -->to_a		prt(t tr-g_    lu*p ==%j++
	prt( 
or:paired 	/1} elg tr-ptrpptr-r:paired 	| 		tr-g     e vithehe gowdg(e bey toar000.;
		prt("f (p_{nameptr-
{
 || 		tr-g_ au --> byte s[20] |	
			}
		 au -->_ptr->au - cost;
				(vo}
-int("You haPhe spacebar to continue");
		msg_pint(NULL);
						rmldg(5,19);
				(v
/*
 * gambleResdg(ch Item
void inn_cog fdg(chg    (w, i
{
	int n,mldg(5,19);
				(voptr->au - cost<i
{
				msgint("You hae viot the goose %old!				choint(NULL);
						r if (p_{nameptr-iy posse_fully()	tr-g_ au -->_ptr->au - cost;
				(vo}

/*
 * giotck ashin lrave_ion"esr gson"e.c		c_p conic boolelrave_ TRUE;F
				ro
 * gambleEhe ae beyebg rewar
void i conic nn_co TRU(NUocs (_ds
 */ sprinTerm_(vo(cmd) r->au - cds
 */_cminTeiptr-pmgThiESCAPE:tr-g		msg_lrave_ TRUE;
			}
			>au - cindice_ar= 1;
	/		msg_p
			}


	qtr-pmgThi'a':tr-g		msg_ptr-ng reward|| 		tr-g_	g fdg(chg    (200	prt( 
of (p_ptr-ng reward|| 2)			msg_ptheva_ds
->au - cds
 */_cmin} elg_pptr->au - cindice_ar= 1;	msg_pr"_lrave_ TRUE;
			}msg_pr if (p_ptr-ng reward|| 14) elg_pptr->au - c == CLt!= CLASS_RANGER				msg_p ptr->au - cost>= 100	p	msg_p 	of sp r_arr9,0o100	prt( 
o(tf (p_		msg_psgint("You hae viot the goose %old				choiceeint(NULL);
						roll1 tr-g_r if (p_ptr-! au --> byte s[20]p	msg_p of sp r_arr9,0o	prt( 
o(f (p	msg_p int("You hae:turn aft7,rlor'vebar=Angeimee town *dungeon.)prt( 
of (p_ptr--ng reward|| 10roll3 ng reward|| 15)	msg_pr"ptr-->au - c == CLt!= CLASS_WARRIOR	ollsg_pr" r->au - c == CLt!= CLASS_PALADIN)				msg_p ptr->au - cost>= 100	p	msg_p 	g pair_31000o100	prt( 
o(tf (p_		msg_psgint("You hae viot the goose %old				choiceeint(NULL);
						roll1 tr-g_r if (p_ptr-! au --> byte s[20]p_		msg_psg pair_31000o	prt( 
o( if (p	msg_p int("You hae:turn aft7,rlor'vebar=Angeimee town *dungeon.)prt( 
of (p	msg_pes 			p_ptr
			}


	qtr-pmgThi'b':tr-g		msg_ptr-ng reward|| (rsg_pppgg Rue_ds
->au - cds
 */_cmin} elg_f (p_ptr-ng reward|| 14) elg_pptr->au - c == CLt!= CLASS_RANGER				msg_p ptr->au - cost>= 100	p	msg_p 	g fo i b_bowo100	prt( 
o(tf (p_		msg_psgint("You hae viot the goose %old				choiceeint(NULL);
						roll1 tr-g_r if (p_ptr-! au --> byte s[21]p_		msg_psg fo i b_bowo	prt( 
o( if (p	msg_p int("You hae:turn aft7,rlor'vebar=Angeimee town *dungeon.)prt( 
of (p	msg_pes 			msg_pr"
			}


	qtr-pmgThi'c':tr-g		msg_ptr-ng reward|| (rsg_pppgg Rue_ds
->au - cds
 */_cmin} elg_f (p_ptr-ng reward|| 6p_		msg_pptr->au - cost<i100	p	msg_p int("You hae viot the goose %old!				chog_f (p_ptr-bompthe_wmaste1 			win = >au - cost;= 100			chog if (p	msg_pes 			p_ptr
			}


	qtr-pmgThi'd':tr-g		msg_ptr-ng reward|| (rsg_pppgg Rue_ds
->au - cds
 */_cmin} elg_f (p	msg_pes 			msg_pr
			}


	qtr-pmgThi'f':tr-g		msg_ptr-ng reward|| 4) elg_ppnn_ds
->au - cds
 */_cmi,		}
	mef (p	msg_pes 			msg_pr
			}


	qtr-pmgThi'g':tr-g		msg_ptr-ng reward|| 1	sg_pppb, 3, gan
 (	} elg_f (p_ptr-ng reward> 6p_		msg_pptr--ng reward- 10ro=r->au - c == CL		win = rn
o_mp_s(			chog_f (p_		msg_psint("You haOke iactherlenory.c= CLtcantdo sta ts 
						roll1int(NULL);
						roll}	chog if (p	msg_pes 			p_ptr
			}


	qtr-pmgThi'h':tr-g		msg_ptr-ng reward|| 		tr-g_	sr 9_sonon y
	} elg_f (p_ptr--ng reward|| 12roll3 ng reward|| 15)oll3 ng reward|| 17)p_		msg_pptr-->au - c == CLt!= CLASS_PRIEST	ollsg_pr" rr->au - c == CLt!= CLASS_PALADIN)ollsg_pr" rr->au - c == CLt!= CLASS_DRUID					msg_p ptr->au - cost>= 100	p_		msg_psgh>au
		gh(200			choicee au -->_ptr->au - cost;100			choggggs
o_ed = 0ed(0			choicee au -->cut
	/		msg_pcee au -->stun
	/		msg_pceeint("You hae vitheaon rol= 'Yts althy & vigorous!				choiceeint(NULL);
						roll1  f (p_		msg_psgint("You hae viot the goose %old				choiceeint(NULL);
						roll1 tr-g_r if (p_ptr-! au --> byte s[21]p_		msg_psptr->au - c == CLt== CLASS_PALADIN)	msg_psgh>au
		gh(200			choicef (p	msg_p gh>au
		gh(1200			choices
o_ed = 0ed(0			choices
o_Ruled(0			choices
o_ue"fused(0			choice au -->cut
	/		msg_pce au -->stun
	/		msg_pce au --> byte s[21]E;
			}
			eeint("You hae vitheaon rol= 'Yts althy & vigorous!				choiceint(NULL);
						roll}if (p	msg_p int("You hae:turn aft7,rlor'vebar=Angeimee town *dungeon.)prt( 
o if (p	msg_pes 			p_ptr
			}


	qtr-pmgThi'l':tr-g		msg_ptr-ng reward|| 		tr-g_	g to_legends
	} elg_f (p_ptr--ng reward< 3)oll3 ng reward> 6p	sg_pppfhow_h20g== CL{ng rewar		msg_prf (p_	msg_pes 			msg_pr
			}


	qtr-pmgThi'p':tr-g		msg_ptr-ng reward|| 2	sg_ppptheva_ds
->au - cds
 */_cmin} elg_f (p_ptr-ng reward|| 13p_		msg_pptr-->au - c byte s[1]p_ll2 ! au --> byte s[21]pp_		msg_pspy posse_pe: (0			choiceint("You hae vy sions (1000iot thb
		iy possied				choice au --> byte s[21]E;
			}
			e if (p_ptr-! au --> byte s[1]p_		msg_psint("You hae vie betrinst wmi (100p-9. Ight M, 2, fs",3f				choiceint(NULL);
						roll}if (p	msg_p int("You hae:turn aft7,rlor'vebar=Angeimee town *dungeon.)prt( 
o if (p_ptr--ng reward|| 11)oll3 ng reward|| 16p	sg_pppptr-->au - c == CLt!= CLASS_MAGE)ollsg_pr" rr->au - c == CLt!= CLASS_ONIST))IST					msg_p ptr->au - cost>= 100	p_		msg_psgpy posse_pe: (100	prt( 
o(teint("You hae vy sions (1000iot thb
		iy possied				choice  f (p_		msg_psgint("You hae viot the goose %old				choiceeint(NULL);
						roll1 tr-g_r if (p_ptr-->au - c byte s[1]p_ll2 ! au --> byte s[21]pp_		msg_pspy posse_pe: (0			choiceint("You hae vy sions (1000iot thb
		iy possied				choice au --> byte s[21]E;
			}
			e if (p_ptr-! au --> byte s[1]p_		msg_psint("You hae viot the goatt 'YedIght soonusumorssta .				choiceint(NULL);
						roll if (p	msg_p int("You hae:turn aft7,rlor'vebar=Angeimee town *dungeon.)prt( 
of (p	msg_pes 			msg_pr
			}


	qtr-pmgThi'q'pe Dir quest quest 	c_pug		msg_ptr-ng reward|| 1	_	msg_pb, 3, gquest(prt( 
of (p	msg_pes 			sg_pr
			}


	qtr-pmgThi'r':tr-g		msg_ptr-ng reward|| (rsg_pppgg Rue_ds
->au - cds
 */_cmin} elg_f (p_ptr-ng reward|| 4) elg_ppnn_ds
->au - cds
 */_cmi,2	prt( 
of (p_ptr-ng reward|| 2) elg_ptheva_ds
->au - cds
 */_cmin} elg_f (p_ptr-ng reward|| 5p_		msg_pptr->au - cost<i100	p	msg_p int("You hae viot the goose %old!				chog_f (p_ptr-!g fdg(chgmon 			win = >au - cost;= 100			chog if (p_ptr-ng reward|| 13p elg_ppnn_ds
->au - cds
 */_cmi,	prt( 
of (p_ptr-ng reward|| 10)	msg_pptr->au - c == CLt!= CLASS_WARRIOR	o		msg_p ptr->au - cost>= 100	p	msg_p sg pair_wmasteo100	prt( 
o(tf (p_		msg_psgint("You hae viot the goose %old				choiceeint(NULL);
						roll1 tr-g_r if (p_ptr-! au --> byte s[21]p_		msg_psmldg(5,19);
				(vo_p sg pair_wmasteo	prt( 
o( if (p	msg_p int("You hae:turn aft7,rlor'vebar=Angeimee town *dungeon.)prt( 
of (p_ptr---ng reward|| 11)oll3 ng reward|| 16p	ollsg_pr     -->au - c == CLt|| 1	_ll3 >au - c == CLt|| 6)p	sg_pppptr-->au - c byte s[0]p_ll2 ! au --> byte s[20]					msg_p ptr-re iteme(80			win = _ au --> byte s[20] |	
			}
		_r if (p_ptr-! au --> byte s[0]p_		msg_psint("You hae viot the goatt 'YedIght soonusumorssta .				choiceint(NULL);
						roll if (p	msg_p int("You hae:turn aft7,rlor'vebar=Angeimee town *dungeon.)prt( 
of (p_ptr--ng reward|| 11)oll3 ng reward|| 16p	_		msg_pint("You haOke imaSws/an",3, 1ie tecantdo sta ts 
						rollint(NULL);
						rol if (p_ptr---ng reward|| 12roll2 >lev *  == CLt== CLASS_PRIEST	)ollsg_pr   --ng reward|| 17roll2 >lev *  == CLt== CLASS_DRUID			sg_pppptr-->au - c byte s[0]p_ll2 ! au --> byte s[20]					msg_p rtion o_leve 			p_ptr- rti_soon(A_STR		p_ptr- rti_soon(A_INT		p_ptr- rti_soon(A_WIS		p_ptr- rti_soon(A_DEX		p_ptr- rti_soon(A_CON		p_ptr- rti_soon(A_CHR		p_ptr-  au --> byte s[20] |	
			}
		_rpint("You haA yolory.soonsithehe rmal				choiceint(NULL);
						roll if (p_ptr-! au --> byte s[0]p_		msg_psint("You hae viot the goatt 'YedIght soonusumorssta .				choiceint(NULL);
						roll if (p	msg_p int("You hae:turn aft7,rlor'vebar=Angeimee town *dungeon.)prt( 
of (p_ptr-ng reward|| 12ro{sg_pppptr->au - cost>= 100	p_		msg_psrtion o_leve 			p_ptr- rti_soon(A_STR		p_ptr- rti_soon(A_INT		p_ptr- rti_soon(A_WIS		p_ptr- rti_soon(A_DEX		p_ptr- rti_soon(A_CON		p_ptr- rti_soon(A_CHR		p_ptr-  au -->_ptr->au - cost;100			chogg  f (p_		msg_psint("You hae viot the goose %old				choiceint(NULL);
						roll 		rol if (p		rolles 			msg_pr"
			}


	qtr-pmgThi's':tr-g		msg_ptr-ng reward|| (rsg_pppgg Rue_ds
->au - cds
 */_cmin} elg_f (p_ptr-ng reward|| 13	sg_pppptr-->au - c byte s[0]p_ll2 ! au --> byte s[20]					msg_p of st_%old(		p_ptr-  au --> byte s[20] |	
			}
		_r if (p_ptr-! au --> byte s[0]p_		msg_psint("You hae vie betrinst wmi (100p-9. Ight M, 2, fs",3f				choiceint(NULL);
						roll}if (p_		msg_psint("You haCuel ntck la2, ! Goldtdoesve tgr9, o9..rees!				choiceint(NULL);
						roll} elg_f (p		rolles 			msg_pr
			}


	qtr-pmgThi'u':tr-g		msg_ Diptr-ng reward|| 4) elg_ppnn_ds
->au - cds
 */_cmi,	prt( 
of (p	c_pug_pes 			sg_pr
			}


	qtr-pmgThi'z':tr-g		msg_ptr--ng reward|| 11)oll3 ng reward|| 16poll3 ng reward|| 12roll3 ng reward|| 17)	msg_pr"do_cmi_soudy(&& (w;msg_prf (p		rolles 			msg_pr
			}


	qtr-p_ DiIgnohetr:turn 	c_pugmgThi'\r':tr-g		msg_
			}


	qtr-p_ DiEquipmthe lfor 	c_pugmgThi'e':tr-g		msg_do_cmi_equip			sg_pr
			}


	qtr-p_ DiInu ntory lfor 	c_pugmgThi'i':tr-g		msg_do_cmi_inu n			sg_pr
			}


	qtr-p_ DiHelp 	c_pugmgThi'?':tr-g		msg_do_cmi_h lp			sg_pr
			}


	qtr-p_ DiHtck -- Unknown ds
 */ 	c_pugdefaul :tr-g		msg_es 			sg_pr
			}


	qo}

/*
 * gambleEhe a quest leve 
void inn_codo_cmi_quest(sprinTerm_citr-!(cave_feat[>lev *  y][>lev *  x]d|| FEAT_QUEST_ENTER			wiptr-pint("You hae visethe nquest leve ts 
						ror:turn	(vo}
("f (p_{name>lev * oldpytr->au - cpy	msle>lev * oldpxtr->au - cpx	msle>lev * indice_ar= 1;
	/2	msle>lev * depth |	1	msle>lev * left TRUE;
			}
		>lev * leavward|
			}
	}

/*
 * gambleDoebg rewar ds
 */s
void inn_codo_cmi_,19);sprinTerm_cichowhich} elichopx=>au - cpx	mslichopy=>au - cpy	msm_citr-!((cave_feat[>y][>x]d>| FEAT_BLDG_HEAD	ollsg_    -cave_feat[>y][>x]d<| FEAT_BLDG_TAIL				wiptr-pint("You hae visethe nbg rewar s 
						ror:turn	(vo}
("which ro-cave_feat[>y][>x]d- FEAT_BLDG_HEAD		(vong reward|owhich} elm_citr-(which r| 2)	ll2 >lev * indice_ar= 1;	mll2 >lev * exit_ TRUE;;F
								msg     Ga2,sithehcloThd! PTsetrs[p_pteewaits.",
		prt("or:turn	(vo}if (p_ptr--which r| 2)	ll2 >lev * indice_ar= 1;t|| 1	)_{name>lev * leavward|
			}
		>au - cindice_ar= 1;
	/		msg}if (p_ptr-which r| 2)	{name>lev * oldpytr->au - cpy	msle>lev * oldpxtr->au - cpx	msl}msm_citr- au --> byte s[13]d|| 	p_		msgptr--which r| 10roll2 >lev *  == CLt!= CLASS_WARRIOR	p_		msg_     Oke if's HalsnallowedIindice.",
		prt("oor:turn	(voo}if (p_ptr--which r| 11)oll2 ->au - c == CLt!= CLASS_MAGE)(voo      ll2 >lev *  == CLt!= CLASS_RANGER		p_		msg_     Oke imaSws &i16-1"rntallowedIindice.",
		prt("oor:turn	(voo}if (p_ptr--which r| 12)oll2 ->au - c == CLt!= CLASS_PRIEST	(voo     ll2 >lev *  == CLt!= CLASS_PALADIN)	p_		msg_     Oke i"Yoee te&sps",3, 1 allowedIindice.",
		prt("oor:turn	(voo}if (p_ptr--which r| 14roll2 >lev *  == CLt!= CLASS_RANGER		_		msg_     Oke i16-1"rntallowedIindice.",
		prt("oor:turn	(voo}if (p_ptr--which r| 15roll2 >lev *  == CLt!= CLASS_PALADIN)				msg_     Oke i"s",3, 1 allowedIindice.",
		prt("oor:turn	(voo}if (p_ptr--which r| 16)oll2 ->au - c == CLt!= CLASS_ONIST))IST	(voo      ll2 >lev *  == CLt!= CLASS_ROG (w)				msg_     Oke ian",3, 1ie tesk i1oguesasllowedIindice.",
		prt("oor:turn	(voo Die ge sta toke iac,3, 1 sk idord tecant*setlesteirs bar= tive(voo   ng rewars. iHence the neewininuhewi. */
	p_ptr-

	qo}
	qomorringlite			sg_morringview(	}
_pumf sacHal_ickyd|
			}
	>au - cds
 */_arg
	/		msg>au - cds
 */_rep
	/		msg>au - cds
 */_(ewa	/		msmsgdi buil_,19);which		sg_lrave_ TRUE;F
				ro
"whiler-!lrave_ TRU		wiptr-p     );		prt("or:quest_ds
 */ 		}
		 TRU(NUocs (_ds
 */ 	}
		mf sacHal_ickyd|
			}
	}
	qo giot thleftnbg rewar _ptr->lev * energy_u(p_	/		msgmf sacHal_ickyd|F
				ro_ DiHtck -- Cancel automonic ds
 */ 	c_pu>au - cds
 */_(ewa	/		ms_ DiHtck -- Cancel "set"trsde 	c_pu>au - cds
 */_seth;F
				ro
" g F",3h ms (ag 1 XXX XXX XXX 	c_puint(NULL);
						
" g Cldg(Ight scr
			c_puT wm_mldg((				
" g Upda2, 3, ryesoes"	c_pu>au - cupda2, |ro-PU_VIEW | PU_LIT		;
l>au - cupda2, |ro-PU_MONSTERS				
" g Redrawteheirt scr
			c_pu au --> bdrawt|ro-PR_BASIC | PR_EXTRA				
" g Redrawtmap		c_pu au --> bdrawt|ro-PR_MAP				
" g Wind9, souff		c_pu au -->wind9, |ro-PW_OVERHEAD		(v}
	q