/* File: bldg.c */

/*
 * Purpose: Building commands
 * Created by Ken Wigle for Kangband - a variant of Angband 2.8.3
 * -KMW-
 *
 * Rewritten for Kangband 2.8.3i using Kamband's version of
 * bldg.c as written by Ivan Tkatchev
 *
 * Note also that rewards for players have changed. You no longer need a
 * reward to use any of the current menu choices. Timed rewards are not being
 * used with the exception of thieves sharing gold. In place of this, costs
 * for the players have been added though where bldg->class == p_ptr->pclass,
 * the costs are less. In future, rewards will open up more choices.
 * Rewards 0-10 are class rewards, 10-19 castle rewards, 20-29 time constraint
 * rewards, 30-49 castle quests and 50-99 unused.
 */

#include "angband.h"

/* hack as in leave_store in store.c */
static bool leave_bldg = FALSE;

#define BACT_RESEARCH_ITEM	1
#define BACT_TOWN_HISTORY	2
#define BACT_RACE_LEGENDS	3
#define BACT_GREET_KING		4
#define BACT_KING_LEGENDS	5
#define BACT_QUEST		6
#define BACT_GOLD			7
#define BACT_POSTER		8
#define BACT_ARENA_RULES	9
#define BACT_ARENA		10
#define BACT_ARENA_LEGENDS	11
#define BACT_IN_BETWEEN		12
#define BACT_GAMBLE_RULES	13
#define BACT_CRAPS		14
#define BACT_SPIN_WHEEL		15
#define BACT_DICE_SLOTS		16
#define BACT_REST			17
#define BACT_FOOD			18
#define BACT_RUMORS		19
#define BACT_RESEARCH_MONSTER	20
#define BACT_COMPARE_WEAPONS	21
#define BACT_LEGENDS		22
#define BACT_ENCHANT_WEAPON	23
#define BACT_ENCHANT_ARMOR	24
#define BACT_RECHARGE		25
#define BACT_IDENTS		26
#define BACT_LEARN		27
#define BACT_HEALING		28
#define BACT_RESTORE		29
#define BACT_ENCHANT_ARROWS	30
#define BACT_ENCHANT_BOW	31
#define BACT_GREET		32

/*
 * The Buildings. The values in this array are as follows: (see types.h)
 *  1 proprietor name, proprietor race, number of actions
 *  2 action names (with a maximum of 6 actions)
 *  3 cost of action for member of class or guild
 *  4 cost of action to other classes
 *  5 letters of the action that the player presses in the game
 *  6 code for the action which is then handled in this file
 *  7 action restrictions: 0=all classes, 1=guild-classes, 2=only owner class
 *  8 guild-definition, for each 1 in the class array, that class is in guild
 *     The order of the array matches the class order with fighters first
 *     followed by mage, priest, rogue, ranger, paladin, illus., druid
 *  9 owner class for building.  This class can do all functions.  Ex: warrior
 *     would be the owner class for the Fighter's Hall.  -1 = no owner class
 */

static building city[7] = {
	{
	"Astinus", "Human?", 3,
	{ "Research item", "Town history", "Race legends" },
	{ 2000, 0, 0 },
	{ 2000, 0, 0 },
	{ 'a', 'h', 'l' },
	{ BACT_RESEARCH_ITEM, BACT_TOWN_HISTORY, BACT_RACE_LEGENDS },
	{ 0, 0, 0 },
	{ 1, 1, 1, 1, 1, 1, 1, 1},
	-1
	},

	{
	"Denegor", "Human", 3,
	{ "Greet King", "Look at busts of Kings", "Request quest" },
	{ 0, 0, 0 },
	{ 0, 0, 0 },
	{ 'g', 'l', 'q' },
	{ BACT_GREET_KING, BACT_KING_LEGENDS, BACT_QUEST },
	{ 0, 0, 0 },
	{ 1, 1, 1, 1, 1, 1, 1, 1},
	-1
	},

	{
	"Arack", "Dwarf", 4,
	{ "Read poster", "Arena rules", "Enter arena", "Look at plaque" },
	{ 0, 0, 0, 0 },
	{ 0, 0, 0, 0 },
	{ 'p', 'r', 'a', 'l' },
	{ BACT_POSTER, BACT_ARENA_RULES, BACT_ARENA, BACT_ARENA_LEGENDS },
	{ 0, 0, 0, 0 },
	{ 1, 1, 1, 1, 1, 1, 1, 1},
	-1
	},

	{
	"Materim", "Human", 5,
	{ "In-Between", "Game rules", "Play craps", "Spin the wheel",
	"Play dice slots" },
	{ 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0 },
	{ 'b', 'r', 'c', 's', 'd' },
	{ BACT_IN_BETWEEN, BACT_GAMBLE_RULES, BACT_CRAPS, BACT_SPIN_WHEEL,
	BACT_DICE_SLOTS },
	{ 0, 0, 0, 0, 0 },
	{ 1, 1, 1, 1, 1, 1, 1, 1},
	-1
	},

	{
	"Otick", "Human", 3,
	{ "Rest for the night", "Buy food and drink", "Listen for rumors" },
	{ 20, 1, 0 },
	{ 20, 1, 0 },
	{ 'r', 'f', 'u' },
	{ BACT_REST, BACT_FOOD, BACT_RUMORS },
	{ 0, 0, 0 },
	{ 1, 1, 1, 1, 1, 1, 1, 1},
	-1
	},

	{
	"Lorien", "Elf", 1,
	{ "Research monster" },
	{ 10000 },
	{ 10000 },
	{ 'r' },
	{ BACT_RESEARCH_MONSTER },
	{ 0 },
	{ 1, 1, 1, 1, 1, 1, 1, 1},
	-1
	},

	{
	"Suiyan", "Human", 1,
	{ "Compare weapons" },
	{ 1000 },
	{ 1000 },
	{ 'c' },
	{ BACT_COMPARE_WEAPONS },
	{ 0 },
	{ 1, 1, 1, 1, 1, 1, 1, 1},
	-1
	}
};


static building class_bldgs[8] = {
	{
	"Barak", "Human", 4,
	{ "Greet Lord", "Look at plaque", "Enchant weapon", "Enchant armor" },
	{ 0, 0, 200, 200 },
	{ 0, 0, 1000, 1000 },
	{ 'g', 'l', 'w', 'a' },
	{ BACT_GREET, BACT_LEGENDS, BACT_ENCHANT_WEAPON, BACT_ENCHANT_ARMOR },
	{ 2, 0, 0, 0 },
	{ 1, 0, 0, 0, 0, 0, 0, 0},
	CLASS_WARRIOR
	},

	{
	"Irrident", "Human", 5,
	{ "Greet Wizard", "Look at spires", "Recharge item",
	"Identify posessions", "Learn spells" },
	{ 0, 0, 200, 200, 0 },
	{ 0, 0, 1000, 1000, 0 },
	{ 'g', 'l', 'r', 'i', 'z' },
	{ BACT_GREET, BACT_LEGENDS, BACT_RECHARGE, BACT_IDENTS, BACT_LEARN },
	{ 2, 0, 0, 0, 1 },
	{ 0, 1, 0, 0, 1, 0, 0, 0},
	CLASS_MAGE
	},

	{
	"Crysania", "Human", 5,
	{ "Greet Priest", "Look at busts", "Healing prayer", "Restoration",
	"Learn spells" },
	{ 0, 0, 0, 200, 0 },
	{ 0, 0, 1000, 1000, 0 },
	{ 'g', 'l', 'h', 'r', 'z' },
	{ BACT_GREET, BACT_LEGENDS, BACT_HEALING, BACT_RESTORE,
	BACT_LEARN },
	{ 2, 0, 0, 0, 1 },
	{ 0, 0, 1, 0, 0, 1, 0, 0},
	CLASS_PRIEST
	},

	{
	"Lardbottom", "Hobbit", 6,
	{ "Greet Master Thief", "Look at wall", "Get share of stolen gold",
	"Rest for the night", "Identify possessions", "Learn spells" },
	{ 0, 0, 0, 0, 100, 0 },
	{ 0, 0, 0, 100, 2000, 0 },
	{ 'g', 'l', 's', 'r', 'i', 'z' },
	{ BACT_GREET, BACT_LEGENDS, BACT_GOLD, BACT_REST, BACT_IDENTS,
	BACT_LEARN },
	{ 2, 0, 2, 0, 0, 2 },
	{ 0, 0, 0, 1, 0, 0, 0, 0},
	CLASS_ROGUE
	},

	{
	"Trallin", "Half-elf", 5,
	{ "Greet Master Ranger", "Look at plaque", "Enchant arrows",
	"Enchant bow", "Learn spells" },
	{ 0, 0, 200, 200, 0 },
	{ 0, 0, 1000, 1000, 0 },
	{ 'g', 'l', 'a', 'b', 'z' },
	{ BACT_GREET, BACT_LEGENDS, BACT_ENCHANT_ARROWS, BACT_ENCHANT_BOW,
	BACT_LEARN },
	{ 2, 0, 0, 0, 2 },
	{ 0, 0, 0, 0, 1, 0, 0, 0},
	CLASS_RANGER
	},

	{
	"Langordathur", "Human", 5,
	{ "Greet Warden", "Look at shrine", "Enchant armor", "See Healers",
	"Learn spells" },
	{ 0, 0, 200, 0, 0 },
	{ 0, 0, 1000, 1000, 0 },
	{ 'g', 'l', 'a', 'h', 'z' },
	{ BACT_GREET, BACT_LEGENDS, BACT_ENCHANT_ARMOR, BACT_HEALING,
	BACT_LEARN },
	{ 2, 0, 0, 0, 2 },
	{ 0, 0, 0, 0, 0, 1, 0, 0},
	CLASS_PALADIN
	},

	{
	"Itsukama", "Human", 5,
	{ "Greet Shadow Lord", "Look at spires", "Recharge item",
	"Identify possessions", "Learn spells" },
	{ 0, 0, 200, 200, 0 },
	{ 0, 0, 1000, 1000, 0 },
	{ 'g', 'l', 'r', 'i', 'z' },
	{ BACT_GREET, BACT_LEGENDS, BACT_RECHARGE, BACT_IDENTS, BACT_LEARN },
	{ 2, 0, 0, 0, 1 },
	{ 0, 0, 0, 1, 0, 0, 1, 0},
	CLASS_ILLUSIONIST
	},

	{
	"Dorrio", "Elf", 5,
	{ "Greet Druid Lord", "Look at trees", "Healing prayer", "Restoration",
	"Learn prayers" },
	{ 0, 0, 0, 200, 0 },
	{ 0, 0, 1000, 10000, 0 },
	{ 'g', 'l', 'h', 'r', 'z' },
	{ BACT_GREET, BACT_LEGENDS, BACT_HEALING, BACT_RESTORE, BACT_LEARN },
	{ 2, 0, 0, 0, 2 },
	{ 0, 0, 0, 0, 0, 0, 0, 1},
	CLASS_DRUID
	}
};



/* remember building location */
static int building_loc = 0;


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

	for(i=1; i < z_info->k_max; i++)
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
	drop_near(q_ptr, -1, p_ptr->py, p_ptr->px);
}


/*
 * Display a building.
 */

static void show_building(building* bldg)
{
	char buff[20];
	int i;
	int action_color;
	int j = bldg->num_actions;
	char tmp_str[80];

	Term_clear();
	sprintf(tmp_str, "%s (%s) %35s", bldg->name, bldg->race,
	    f_name + f_info[cave_feat[p_ptr->py][p_ptr->px]].name);
	prt(tmp_str, 2, 1);
	prt("You may:", 19, 0);

	for (i = 0; i < j; i++) {

		action_color = TERM_WHITE;
		if (bldg->action_restr[i] == 0)
		{
			if (((bldg->class == p_ptr->pclass) && (bldg->class_costs[i] == 0)) ||
			    ((bldg->class != p_ptr->pclass) && (bldg->other_costs[i] == 0)))
				buff[0] = '\0';
			else if (bldg->class == p_ptr->pclass)
			{
				action_color = TERM_YELLOW;
				sprintf(buff, "(%dgp)", bldg->class_costs[i]);
			}
			else
			{
				action_color = TERM_YELLOW;
				sprintf(buff, "(%dgp)", bldg->other_costs[i]);
			}
		}
		else if (bldg->action_restr[i] == 1)
		{
			if (!(bldg->g_class[p_ptr->pclass]))
			{
				strcpy(buff, "(closed)");
				action_color = TERM_L_DARK;
			}
			else if (((bldg->class == p_ptr->pclass) && (bldg->class_costs[i] == 0)) ||
			    ((bldg->class != p_ptr->pclass) && (bldg->other_costs[i] == 0)))
				buff[0] = '\0';
			else if (bldg->class == p_ptr->pclass)
			{
				action_color = TERM_YELLOW;
				sprintf(buff, "(%dgp)", bldg->class_costs[i]);
			}
			else
			{
				action_color = TERM_YELLOW;
				sprintf(buff, "(%dgp)", bldg->other_costs[i]);
			}
		}
		else
		{
			if (p_ptr->pclass != bldg->class)
			{
				strcpy(buff, "(closed)");
				action_color = TERM_L_DARK;
			}
			else if (bldg->class_costs[i] != 0)
			{
				action_color = TERM_YELLOW;
				sprintf(buff, "(%dgp)", bldg->class_costs[i]);
			}
			else
				buff[0] = '\0';
		}

		sprintf(tmp_str," %c) %s %s", bldg->letters[i], bldg->act_names[i], buff);
		c_put_str(action_color, tmp_str, 19+(i/2), 35*(i%2));
	}

	prt(" ESC) Exit building", 23, 0);
}


/* reset timed flags */
void reset_tim_flags(void)
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
	p_ptr->tim_levitate = 0;	/* Timed -- Levitation */
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
void arena_comm(int cmd)
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
				p_ptr->inside_special = 1;
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

		/* Load screen */
		screen_load();

	}
	else
	{
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
		if (wager > maxbet)
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
		if (wager > p_ptr->au)
		{
			msg_print("Hey! You don't have the gold - get out of here!");
			msg_print(NULL);
			/* Load screen */
			screen_load();

			return(FALSE);
		}
		msg_print(NULL);
		win = FALSE;
		odds = 0;
		oldgold = p_ptr->au;
		(void) sprintf(tmp_str,"Gold before game: %9ld",oldgold);
		prt(tmp_str,20,2);
		(void) sprintf(tmp_str,"Current Wager:    %9ld",wager);
		prt(tmp_str,21,2);
		do
		{
			switch(cmd) {
			 case BACT_IN_BETWEEN: /* Game of In-Between */
				c_put_str(TERM_GREEN, "In Between",5,2);
				odds = 3;
				win = FALSE;
				roll1 = randint(10);
				roll2 = randint(10);
				choice = randint(10);
				(void) sprintf(tmp_str,"Black die: %d       Black Die: %d", roll1, roll2);
				prt(tmp_str,8,3);
				(void) sprintf(tmp_str,"Red die: %d", choice);
				prt(tmp_str,11,14);
				if (((choice > roll1) && (choice < roll2)) || ((choice < roll1)
				     && (choice > roll2)))
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
				(void) sprintf(tmp_str,"First roll: %d %d    Total: %d", roll1,
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
					(void) sprintf(tmp_str,"Roll result: %d %d   Total:     %d",
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
				c_put_str(TERM_GREEN,"Wheel",5,2);
				prt("0  1  2  3  4  5  6  7  8  9",7,5);
				prt("--------------------------------",8,3);
				strcpy(out_val, "");
				get_string ("Pick a number (1-9): ",out_val,32);
				for (p=out_val;*p == ' '; p++);
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
				(void) sprintf(tmp_str, "The wheel spins to a stop and the winner is %d",
				    roll1);
				prt(tmp_str,13,3);
				prt("",9,0);
				prt("*",9,(3*roll1+5));
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
				display_fruit(8,3,roll1-1);
				display_fruit(8,12,roll2-1);
				display_fruit(8,21,choice-1);
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
				(void) sprintf(tmp_str,"Payoff: %d",odds);
				prt(tmp_str,17,37);
			}
			else
			{
				prt("You Lost",16,37);
				p_ptr->au = p_ptr->au - wager;
				prt("", 17, 37);
			}
			(void) sprintf(tmp_str,"Current Gold:     %9ld",p_ptr->au);
			prt(tmp_str,22,2);
			prt("Again(Y/N)?",18,37);
			move_cursor(18, 49);
			again = inkey();
			if (wager > p_ptr->au)
			{
				msg_print("Hey! You don't have the gold - get out of here!");
				msg_print(NULL);
				/* Load screen */
				screen_load();

				return(FALSE);
/*				(void) sprintf(tmp_str,"Current Wager:    %9ld",wager);
				prt(tmp_str,17,2); */
			}
		} while ((again == 'y') || (again == 'Y'));

		prt("",18,37);
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
	int n, dawnval;

	switch(cmd)
	{
		case BACT_FOOD: /* Buy food & drink */
			msg_print("The barkeep gives you some gruel and a beer.");
			msg_print(NULL);
			(void) set_food(PY_FOOD_MAX-1);
			break;

		case BACT_REST: /* Rest for the night */
			dawnval = ((turn % (10L * TOWN_DAWN)));
			if (dawnval > 50000) {  /* nighttime */
				if ((p_ptr->poisoned > 0) || (p_ptr->cut > 0))
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
			}
			else
			{
				msg_print("The rooms are available only at night.");
				msg_print(NULL);
				return(FALSE);
			}
			break;
		case BACT_RUMORS: /* Listen for rumors */
			msg_print("Not Implemented Yet (sigh).");
			break;
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
	(void) sprintf(tmp_str, "You collect %d gold pieces", i);
	msg_print(tmp_str);
	msg_print(NULL);
	p_ptr->au += i;
}


/*
 * Display quest information
 */
static void get_questinfo(int questnum)
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

	prt("Complete this quest and return for a reward.",17,0);

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
static void castle_quest(void)
{
	int increment, i, j, j2, oldlevel;
	char tmp_str[80];
	int qstatus;
	int qidx;
	monster_race *r_ptr;
	cptr name;

	qstatus = 0;
	qidx = 0;
	increment = ((p_ptr->lev) / 2);
	clear_bldg(7,18);
	if (increment < 1)
	{
		put_str("You are too green to go off on a quest!", 8, 0);
		(void) sprintf(tmp_str,"Return when you have reached level %d.", 2);
		put_str(tmp_str,10,0);
		msg_print(NULL);
	}
	else if ((increment >= 25) && (p_ptr->rewards[(QUEST_REWARD_TAIL - 1 - QUEST_DIFF)] == QUEST_REWARDED))
	{
		put_str("You have completed all quests I have given and have",8,0);
		put_str("vanquished the evil Morgoth!  We are in your debt.",9,0);
		put_str("You have fulfilled your destiny!",12,0);
	}
	else if (p_ptr->rewards[increment + 29] == QUEST_REWARDED)
	{
		put_str("You fulfilled your last quest but you are not ready for another.",8,0);
		sprintf(tmp_str,"Return when you have reached level %d", (increment*2)+2);
		put_str(tmp_str,12,0);
		msg_print(NULL);
	}
	else /* set last reward that hasn't been given to TRUE */
	{
		increment = 0;

		for (j2=QUEST_REWARD_HEAD;j2 < QUEST_REWARD_TAIL;j2++)
		{
			increment = j2;
			j = j2 - QUEST_DIFF;
			if (!p_ptr->rewards[j])
			{
				p_ptr->rewards[j] = QUEST_ACTIVE;
				qstatus = 0;
				break;
			}
			else if (p_ptr->rewards[j] == 1)
			{
				put_str("You have not completed your current quest yet!",8,0);
				put_str("Use CTRL-Q to check the status of your quest.",9,0);
				put_str("Return when you have completed your quest.",12,0);
				qstatus = 1;
				break;
			}
			else if (p_ptr->rewards[j] == QUEST_COMPLETED)
			{
				qstatus = 2;
				for (i = 0; i < MAX_MON_QUEST; i++)
					p_ptr->cqmon[i] = FALSE;
				for (i = 0; i < MAX_MON_QUEST; i++)
					p_ptr->cqmonc[i] = FALSE;
				for (i = 0; i < MAX_ITEM_QUEST; i++)
					p_ptr->cqitem[i] = FALSE;
				for (i = 0; i < MAX_ITEM_QUEST; i++)
					p_ptr->cqitemc[i] = FALSE;
				/* just fulfilled quest */
				p_ptr->rewards[j] = QUEST_REWARDED;
				break;
			}
		}

		switch(qstatus)
		{
			case 0: /* need to assign quest */
			{
				qidx = increment - QUEST_OFFSET1;
				if (q_list[qidx].quest_type == QUEST_OBJ_KILL_ANY)
				{
					if (!q_list[qidx].r_idx)
						q_list[qidx].r_idx = randint(50) + (40 * (increment - QUEST_OFFSET1));
					r_ptr = &r_info[q_list[qidx].r_idx];
					while ((r_ptr->flags1 & (RF1_UNIQUE)) ||
					  (r_ptr->rarity != 1))
					{
						q_list[qidx].r_idx = randint(50) + 100;
						r_ptr = &r_info[q_list[qidx].r_idx];
					}
					/* Choose a random number of monsters, if necessary */
					if (q_list[qidx].max_num == 0)
					{
						if (randint(10) > 7)
							q_list[qidx].max_num = 1;
						else
							q_list[qidx].max_num = randint(3) + 1;
					}
					q_list[qidx].cur_num = 0;
					name = (r_name + r_ptr->name);
					sprintf(tmp_str,"Your quest: kill %d %s",
					    q_list[qidx].max_num, name);
					msg_print(tmp_str);
					msg_print(NULL);
				}
				else
				{
					get_questinfo(increment);
					p_ptr->rewards[j] = QUEST_ACTIVE;
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
				oldlevel = object_level;
				object_level = p_ptr->lev * 2;
				acquirement(p_ptr->py,p_ptr->px,1,1,1);
				object_level = oldlevel;
				break;
			}
		}
	}
}


/*
 * Greet the lord of the castle
 */
static void castle_greet(void)
{
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
	else if ((increment >= 5) && (p_ptr->rewards[14]))
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
			if (!p_ptr->rewards[j])
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
			msg_print("An excellent gift awaits you outside!");
			msg_print(NULL);
			oldlevel = object_level;
			object_level = p_ptr->lev * 4;
			acquirement(p_ptr->py,p_ptr->px,1,0,1);
			object_level = oldlevel;
		}
	}
}


/*
 * greet_reward
 */
static void greet_reward(int gclass)
{
	int tval, sval;
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
		case CLASS_ILLUSIONIST:
		case CLASS_DRUID:
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


/*
 * greet_char
 */
static void greet_char(void)
{
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
 * show_highclass - selectively list highscores based on class
 * -KMW-
 */
static void show_highclass(int building)
{

	register int i = 0, j, m = 0;
	int pr, pc, clev, al;
	high_score the_score;
	char buf[1024], out_val[256];

	switch(building)
	{
		case 1:
			prt("               Busts of Greatest Kings", 5, 0);
			break;
		case 2:
			prt("               Plaque - Greatest Arena Champions", 5, 0);
			break;
		case 10:
			prt("               Plaque - Greatest Fighters", 5, 0);
			break;
		case 11:
			prt("               Spires of the Greatest Magic-Users", 5, 0);
			break;
		case 12:
			prt("               Busts of Greatest Priests", 5, 0);
			break;
		case 13:
			prt("               Wall Inscriptions - Greatest Thieves", 5, 0);
			break;
		case 14:
			prt("               Plaque - Greatest Rangers", 5, 0);
			break;
		case 15:
			prt("               Plaque - Greatest Paladins", 5, 0);
			break;
		case 16:
			prt("               Spires of the Greatest Illusionists", 5, 0);
			break;
		default:
			bell("Search string not found!");
			break;
	}

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	highscore_fd = fd_open(buf, O_RDONLY);

	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		msg_print(NULL);
		return;
	}

	if (highscore_seek(0)) return;

	for (i = 0; i < MAX_HISCORES; i++)
		if (highscore_read(&the_score)) break;

	m=0;
	j=0;
	clev = 0;

	while ((m < 9) || (j < MAX_HISCORES))
	{
		if (highscore_seek(j)) break;
		if (highscore_read(&the_score)) break;
		pr = atoi(the_score.p_r);
		pc = atoi(the_score.p_c);
		clev = atoi(the_score.cur_lev);
		al = atoi(the_score.arena_number);
		if (((pc == (building - 10)) && (building != 1) && (building != 2)) ||
		    ((building == 1) && (clev >= PY_MAX_LEVEL)) ||
		    ((building == 2) && (al > MAX_ARENA_MONS)))
		{
			sprintf(out_val, "%3d) %s the %s (Level %2d)",
			    (m + 1), the_score.who,p_name + rp_ptr->name, clev);
			prt(out_val, (m + 7), 0);
			m++;
		}
		j++;
	}

	/* Now, list the active player if they qualify */
	if ((building == 1) && (p_ptr->lev >= PY_MAX_LEVEL))
	{
		sprintf(out_val, "You) %s the %s (Level %2d)",
		    op_ptr->full_name,p_name + rp_ptr->name, p_ptr->lev);
		prt(out_val, (m + 8), 0);
	}
	else if ((building == 2) && (p_ptr->arena_number > MAX_ARENA_MONS))
	{
		sprintf(out_val, "You) %s the %s (Level %2d)",
		    op_ptr->full_name,p_name + rp_ptr->name, p_ptr->lev);
		prt(out_val, (m + 8), 0);
	}
	else if ((building != 1) && (building != 2))
	{
		if ((p_ptr->lev > clev) && (p_ptr->pclass == (building - 10)))
		{
			sprintf(out_val, "You) %s the %s (Level %2d)",
			    op_ptr->full_name,p_name + rp_ptr->name, p_ptr->lev);
			prt(out_val, (m + 8), 0);
		}
	}

	(void)fd_close(highscore_fd);
	highscore_fd = -1;
	msg_print("Hit any key to continue");
	msg_print(NULL);
	for (j=5;j<18;j++)
		prt("",j,0);
}


/*
 * Race Legends
 * -KMW-
 */
static void race_score(int race_num)
{
	register int i = 0, j, m = 0;
	int pr, pc, clev, al, lastlev;
	high_score the_score;
	char buf[1024], out_val[256], tmp_str[80];
/*
 *	static const char *race_name[11]={"Humans", "Half-Elves", "Elves",
 *	    "Hobbits", "Gnomes", "Dwarves", "Half-Orcs", "Half-Trolls",
 *	    "Dunadain", "High Elves", "Kobolds"};
 */

	rp_ptr = &p_info[race_num];

	lastlev = 0;
	/* (void) sprintf(tmp_str,"The Greatest of all the %s",race_name[race_num]); */
	(void) sprintf(tmp_str,"The Greatest heroes of all time (%s)", p_name + rp_ptr->name);
	prt(tmp_str, 5, 15);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	highscore_fd = fd_open(buf, O_RDONLY);

	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		msg_print(NULL);
		return;
	}

	if (highscore_seek(0)) return;

	for (i = 0; i < MAX_HISCORES; i++) {
		if (highscore_read(&the_score)) break;
	}

	m=0;
	j=0;

	while ((m < 10) || (j < MAX_HISCORES))
	{
		if (highscore_seek(j)) break;
		if (highscore_read(&the_score)) break;
		pr = atoi(the_score.p_r);
		pc = atoi(the_score.p_c);
		clev = atoi(the_score.cur_lev);
		al = atoi(the_score.arena_number);
		if (pr == race_num)
		{
			sprintf(out_val, "%3d) %s the %s (Level %3d)",
			    (m + 1), the_score.who,
			p_name + rp_ptr->name, clev);
			prt(out_val, (m + 7), 0);
			m++;
			lastlev = clev;
		}
		j++;
	}

	/* add player if qualified */
	if ((p_ptr->prace == race_num) && (p_ptr->lev >= lastlev)) {
		sprintf(out_val, "You) %s the %s (Level %3d)",
		    op_ptr->full_name, p_name + rp_ptr->name, p_ptr->lev);
		prt(out_val, (m + 8), 0);
	}

	(void)fd_close(highscore_fd);
	highscore_fd = -1;
}


/*
 * Race Legends
 * -KMW-
 */
static void race_legends(void)
{
	int i,j;

	for (i = 0; i < z_info->p_max; i++) {
		race_score(i);
		msg_print("Hit any key to continue");
		msg_print(NULL);
		for (j = 5; j < 19; j++)
			prt("", j, 0);
	}
}


/*
 * compare_weapon_aux2 -KMW-
 */
static void compare_weapon_aux2(object_type *o_ptr, int numblows, int r, int c, int mult, char attr[80], u32b f1, u32b f2, u32b f3, int color)
{
	char tmp_str[80];

	c_put_str(color,attr,r,c);
	sprintf(tmp_str,"Attack: %d-%d damage",
	    mult*(numblows*(o_ptr->dd + o_ptr->to_d)),
	    mult*(numblows*((o_ptr->ds*o_ptr->dd) + o_ptr->to_d)));
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

	if (f1 & (TR1_SLAY_ANIMAL)) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 2, "Animals:", f1, f2, f3, TERM_YELLOW);
	if (f1 & (TR1_SLAY_EVIL)) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 2, "Evil:", f1, f2, f3, TERM_YELLOW);
	if (f1 & (TR1_SLAY_UNDEAD)) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Undead:", f1, f2, f3, TERM_YELLOW);
	if (f1 & (TR1_SLAY_DEMON)) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Demons:", f1, f2, f3, TERM_YELLOW);
	if (f1 & (TR1_SLAY_ORC)) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Orcs:", f1, f2, f3, TERM_YELLOW);
	if (f1 & (TR1_SLAY_TROLL)) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Trolls:", f1, f2, f3, TERM_YELLOW);
	if (f1 & (TR1_SLAY_GIANT)) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Giants:", f1, f2, f3, TERM_YELLOW);
	if (f1 & (TR1_SLAY_DRAGON)) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Dragons:", f1, f2, f3, TERM_YELLOW);
	if (f1 & (TR1_KILL_DRAGON)) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 5, "Dragons:", f1, f2, f3, TERM_YELLOW);
	if (f1 & (TR1_BRAND_ACID)) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Acid:", f1, f2, f3, TERM_RED);
	if (f1 & (TR1_BRAND_ELEC)) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Elec:", f1, f2, f3, TERM_RED);
	if (f1 & (TR1_BRAND_FIRE)) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Fire:", f1, f2, f3, TERM_RED);
	if (f1 & (TR1_BRAND_COLD)) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Cold:", f1, f2, f3, TERM_RED);
	if (f1 & (TR1_BRAND_POIS)) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Poison:", f1, f2, f3, TERM_RED);
	if (f1 & (TR1_FORCE)) compare_weapon_aux2(o_ptr, p_ptr->num_blow, r++, col, 3, "Force:", f1, f2, f3, TERM_RED);
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
	sprintf(tmp_str,"One Strike: %d-%d damage",o_ptr->dd + o_ptr->to_d,
	    (o_ptr->ds*o_ptr->dd) + o_ptr->to_d);
	put_str(tmp_str,row+6,col+1);
	sprintf(tmp_str,"One Attack: %d-%d damage",p_ptr->num_blow*(o_ptr->dd + o_ptr->to_d),
	    p_ptr->num_blow*(o_ptr->ds*o_ptr->dd + o_ptr->to_d));
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

	/* Added this to avoid warning - remove if necessary */
	orig_ptr = NULL;

	o1_ptr = NULL; o2_ptr = NULL; i_ptr = NULL;

	/* Store copy of original wielded weapon in pack slot */
	i_ptr = &inventory[INVEN_WIELD];
	object_copy(orig_ptr, i_ptr);

	i = 6;
	/* Get an item */
	q = "What is your first weapon? ";
	s = "You have nothing to compare.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN)))
		return(FALSE);

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
static bool fix_item(int istart, int iend, int ispecific, bool iac, int ireward, int bclass)
{
	int i, j, maxenchant;
	object_type *o_ptr;
	char out_val[80], tmp_str[80];
	bool repaired;

	clear_bldg(5,18);
	repaired = FALSE;
	maxenchant = (p_ptr->lev / 5);
	sprintf(tmp_str,"  Based on your skill, we can improve up to +%d", maxenchant);
	prt(tmp_str, 5, 0);
	prt("Status", 7, 30);
	j = 9;

	for (i = istart; i <= iend; i++)
	{
		o_ptr = &inventory[i];
		if (ispecific > 0)
		{
			if (o_ptr->tval != ispecific)
				continue;
		}

		if (o_ptr->tval) {
			object_desc(tmp_str, o_ptr, FALSE, 1);

			if ((o_ptr->name1 && (o_ptr->ident & 0x08)))
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
				else if ((!iac) && ((o_ptr->to_h  < maxenchant) &&
					    (o_ptr->to_d < maxenchant)))
				{
					o_ptr->to_h++;
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
		if (bclass == p_ptr->pclass)
			p_ptr->rewards[ireward] = TRUE;
		msg_print("Press the spacebar to continue");
		msg_print(NULL);
	}
	clear_bldg(5,18);
	return(repaired);
}


/*
 * Research Item
 */
static bool research_item(void)
{
	clear_bldg(5,18);
	identify_fully();
	return(TRUE);
}


/*
 * Execute a building command
 */
static void bldg_process_command(building *bldg, int i)
{

	int bact = bldg->actions[i];
	int bcost;
	bool paid = FALSE;

	if (p_ptr->pclass == bldg->class)
		bcost = bldg->class_costs[i];
	else
		bcost = bldg->other_costs[i];

	/* action restrictions */
	if (((bldg->action_restr[i] == 1) && (!(bldg->g_class[p_ptr->pclass]))) ||
	    ((bldg->action_restr[i] == 2) && (p_ptr->pclass != bldg->class)))
	{
		msg_print("You have no right to choose that!");
		msg_print(NULL);
		return;
	}

	/* check gold */
	if (((bldg->class_costs[i] > p_ptr->au) && (p_ptr->pclass == bldg->class)) ||
	    ((bldg->other_costs[i] > p_ptr->au) && (p_ptr->pclass != bldg->class)))
	{
		msg_print("You do not have the gold!");
		msg_print(NULL);
		return;
	}

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
			paid = fix_item(INVEN_WIELD, INVEN_WIELD, 0, FALSE,21, bldg->class);
			break;

		case BACT_ENCHANT_ARMOR:
			paid = fix_item(INVEN_BODY, INVEN_FEET, 0, TRUE,20, bldg->class);
			break;

		case BACT_RECHARGE: /* needs work */
			if ((p_ptr->rewards[0]) && (!p_ptr->rewards[20]))
				if (recharge(80))
				{
					p_ptr->rewards[20] = TRUE;
					paid = TRUE;
				}
			break;

		case BACT_IDENTS: /* needs work */
			identify_pack();
			msg_print("Your posessions have been identified.");
			msg_print(NULL);
			paid = TRUE;
			break;

		case BACT_LEARN:
			do_cmd_study(TRUE);
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

		case BACT_GOLD: /* set timed reward flag - hack */
			if (!p_ptr->rewards[20])
			{
				share_gold();
				p_ptr->rewards[20] = TRUE;
			}
			else
			{
				msg_print("You just had your daily allowance!");
				msg_print(NULL);
			}
			break;

		case BACT_ENCHANT_ARROWS:
			paid = fix_item(0, INVEN_WIELD, TV_ARROW, FALSE,20, bldg->class);
			break;

		case BACT_ENCHANT_BOW:
			paid = fix_item(INVEN_BOW, INVEN_BOW, TV_BOW, FALSE,21, bldg->class);
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
	int i,which;
	bool validcmd;
	int px=p_ptr->px;
	int py=p_ptr->py;
	building *bldg;

	if (!((cave_feat[py][px] >= FEAT_BLDG_HEAD) &&
	    (cave_feat[py][px] <= FEAT_BLDG_TAIL)))
	{
		msg_print("You see no building here.");
		return;
	}

	which = (cave_feat[py][px] - FEAT_BLDG_HEAD);
	building_loc = which;

	/* Hack -- Check the "locked doors" */
	if (adult_no_stores)
	{
		msg_print("The doors are locked.");
		return;
	}

	if (which < 7)
		bldg = &city[which];
	else
		bldg = &class_bldgs[which-10];

	if ((which == 2) && (p_ptr->inside_special) && (p_ptr->exit_bldg == FALSE))
	{
		prt("The gates are closed.  The monster awaits!",0,0);
		return;
	}
	else if ((which == 2) && (p_ptr->inside_special == 1))
	{
		p_ptr->leaving = TRUE;
		p_ptr->inside_special = 0;
	}
	else if (which == 2)
	{
		p_ptr->oldpy = p_ptr->py;
		p_ptr->oldpx = p_ptr->px;
	}

	/* Forget the view */
	forget_view();

	/* Hack -- Increase "icky" depth */
	character_icky++;

	p_ptr->command_arg = 0;
	p_ptr->command_rep = 0;
	p_ptr->command_new = 0;

	show_building(bldg);
	leave_bldg = FALSE;

	while (!leave_bldg)
	{
		validcmd = FALSE;
		prt("",1,0);
		msg_flag = FALSE;
		p_ptr->command_cmd = inkey();

		if (p_ptr->command_cmd == ESCAPE)
		{
			leave_bldg = TRUE;
			p_ptr->inside_special = 0;
			break;
		}

		for (i = 0; i < bldg->num_actions; i++)
		{
			if (bldg->letters[i] == p_ptr->command_cmd)
			{
				validcmd = TRUE;
				break;
			}
		}

		if (validcmd)
			bldg_process_command(bldg, i);
		else
		{
			msg_print("Invalid command.");
		}
		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();
	}

	/* have left building */
	/* Hack -- Cancel automatic command */
	p_ptr->command_new = 0;

	/* Hack -- Cancel "see" mode */
	p_ptr->command_see = FALSE;

	/* Flush messages XXX XXX XXX */
	msg_print(NULL);

	wilderness_gen(1);

	/* Hack -- Decrease "icky" depth */
	character_icky--;

	/* Clear the screen */
	Term_clear();

	/* Update the visuals */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw entire screen */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}
