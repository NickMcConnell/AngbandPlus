/* File: bldg.c */

/*
 * Purpose: Building commands
 * Created by Ken Wigle for Kangband - a variant of Angband 2.8.2
 *
 * NOTE:
 * Totally rewritten for Kamband 1.2
 * This file is not related to Kangband except they share the same purpose.
 * -KMW-
 */

#include "angband.h"


#define LEGENDS_RACE   1
#define LEGENDS_CLASS  2
#define LEGENDS_KING   3
#define LEGENDS_ARENA  4

#define GAME_IN_BETWEEN 1
#define GAME_CRAPS      2
#define GAME_SPIN_WHEEL 3
#define GAME_DICE_SLOTS 4

#define BACT_RESEARCH_ITEM   1
#define BACT_TOWN_HISTORY    2
#define BACT_RACE_LEGENDS    3
#define BACT_GREET_KING      4
#define BACT_KING_LEGENDS    5
#define BACT_QUEST           6
#define BACT_SWITCH_ARENA    7
#define BACT_POSTER          8
#define BACT_ARENA_RULES     9
#define BACT_ARENA          10
#define BACT_ARENA_LEGENDS  11
#define BACT_IN_BETWEEN     12
#define BACT_GAMBLE_RULES   13
#define BACT_CRAPS          14
#define BACT_SPIN_WHEEL     15
#define BACT_DICE_SLOTS     16
#define BACT_REST           17
#define BACT_FOOD           18
#define BACT_RUMORS         19
#define BACT_RESEARCH_MONSTER    20
#define BACT_RESEARCH_WEAPON     21
#define BACT_LEGENDS        22
#define BACT_ENCHANT_WEAPON  23
#define BACT_ENCHANT_ARMOR   24
#define BACT_RECHARGE       25
#define BACT_IDENTS         26
#define BACT_LEARN          27
#define BACT_HEALING        28
#define BACT_RESTORE        29
#define BACT_RELIGION       30
#define BACT_ENCHANT_ARROWS  31
#define BACT_ENCHANT_BOW     32
#define BACT_NORMAL_SHAPE   33
#define BACT_GREET          34


/* //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\ */


static cptr favor_text[11] = {
  "impossible",
  "impossible",
  "impossible",
  "extremely unlikely",
  "extremely unlikely",
  "extremely unlikely",
  "unlikely",
  "unlikely",
  "likely",
  "certain",
  "certain beyond any doubt"
};

static cptr quest_types[3] = {
  "Debugging",
  "Search and Destroy",
  "Scout"
};

static bool mega_hack_exit_bldg;



/* //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\ */


static building city[7] = {
  {
    "Astinus", "Human", 3,

    { "Research item", "Town history", "Race legends" },
    { 2000, 0, 0 },
    { 'a', 'h', 'l' },
    { BACT_RESEARCH_ITEM, BACT_TOWN_HISTORY, BACT_RACE_LEGENDS },
    -1
  },

  {
    "Denethor", "Human", 3,

    { "Greet King", "Look at heroic shrines", "Request quest" },
    { 0, 0, 0 },
    { 'g', 'l', 'q' },
    { BACT_GREET_KING, BACT_KING_LEGENDS, BACT_QUEST },
    -1
  },

  {
    "Arack", "Dwarf", 5,

    { "Switch arena type", "Read poster", "Arena rules", "Enter arena",
      "Look at plaque" },
    { 0, 0, 0, 0, 0 },
    { 's', 'p', 'r', 'a', 'l' },
    { BACT_SWITCH_ARENA, BACT_POSTER, BACT_ARENA_RULES, BACT_ARENA,
      BACT_ARENA_LEGENDS },
    -1
  },

  {
    "Globutar", "Mutant", 5,

    { "In-Between", "Game rules", "Play craps", "Spin the wheel", 
      "Play dice slots" },
    { 0, 0, 0, 0, 0 },
    { 'b', 'r', 'c', 'w', 's' },
    { BACT_IN_BETWEEN, BACT_GAMBLE_RULES, BACT_CRAPS, BACT_SPIN_WHEEL,
      BACT_DICE_SLOTS },
    -1
  },

  {
    "Otik", "Human", 3,

    { "Rest for the night", "Buy food and drink", "Listen for rumors" },
    { 20, 1, 0 },
    { 'r', 'f', 'u' },
    { BACT_REST, BACT_FOOD, BACT_RUMORS },
    -1
  },

  {
    "Lorien", "Elf", 1,

    { "Research monster" },
    { 10000 },
    { 'r' },
    { BACT_RESEARCH_MONSTER },
    -1
  },

  {
    "Seldegard", "Golem", 1,

    { "Research weapon" },
    { 3000 },
    { 'r' },
    { BACT_RESEARCH_WEAPON },
    -1
  }
};
 

static building class_bldgs[9] = {
  {
    "Barak", "Human", 4, 
 
    { "Greet Lord", "Look at plaque", "Enchant weapon", "Enchant armor" },
    { 0, 0, 1000, 1000 },
    { 'g', 'l', 'w', 'a' },
    { BACT_GREET, BACT_LEGENDS, BACT_ENCHANT_WEAPON, BACT_ENCHANT_ARMOR },
    CLASS_WARRIOR
  },

  {
    "Gamenlon", "Half-elf", 5,

    { "Greet Wizard", "Look at spires", "Recharge item", 
      "Identify posessions", "Learn spells" },
    { 0, 0, 1000, 1000, 0 },
    { 'g', 'l', 'r', 'i', 'z' },
    { BACT_GREET, BACT_LEGENDS, BACT_RECHARGE, BACT_IDENTS, BACT_LEARN },
    CLASS_MAGE
  },

  {
    "Crysania", "Elf", 6,

    { "Greet Priest", "Look at busts", "Healing prayer", "Restoration",
      "Religious lore", "Learn spells" },
    { 0, 0, 1000, 1000, 8000, 0 },
    { 'g', 'l', 'h', 'r', 'a', 'z' },
    { BACT_GREET, BACT_LEGENDS, BACT_HEALING, BACT_RESTORE,
      BACT_RELIGION, BACT_LEARN },
    CLASS_PRIEST
  },

  {
    "Lardbottom", "Hobbit", 5,

    { "Greet Master Thief", "Look at wall",
      "Rest for the night", "Identify possessions", "Learn spells" },
    { 0, 0, 0, 0, 0 },
    { 'g', 'l', 'r', 'i', 'z' },
    { BACT_GREET, BACT_LEGENDS, BACT_REST, BACT_IDENTS,
      BACT_LEARN },
    CLASS_ROGUE
  },

  {
    "Myrbald", "Half-elf", 5,

    { "Greet Master Ranger", "Look at plaque", "Enchant arrows", 
      "Enchant bow", "Learn spells" },
    { 0, 0, 1000, 1000 },
    { 'g', 'l', 'a', 'b', 'z' },
    { BACT_GREET, BACT_LEGENDS, BACT_ENCHANT_ARROWS, BACT_ENCHANT_BOW,
      BACT_LEARN },
    CLASS_RANGER
  },

  {
    "Langordathur", "Human", 5,
    
    { "Greet Warden", "Look at shrine", "Enchant armor", "Visit medic",
      "Learn spells" },
    { 0, 0, 1000, 1000 },
    { 'g', 'l', 'a', 'm', 'z' },
    { BACT_GREET, BACT_LEGENDS, BACT_ENCHANT_ARMOR, BACT_HEALING,
      BACT_LEARN },
    CLASS_PALADIN
  },

  {
    "Kenault", "Mutant", 5,

    { "Greet Shadow Lord", "Look at spires", "Recharge item", 
      "Identify possessions", "Learn spells" },
    { 0, 0, 1000, 1000, 0 },
    { 'g', 'l', 'r', 'i', 'z' },
    { BACT_GREET, BACT_LEGENDS, BACT_RECHARGE, BACT_IDENTS, BACT_LEARN },
    CLASS_ILLUSIONIST
  },

  {
    "Grishnakh", "Mutant", 4,
    
    { "Greet the Corruptor", "Look at memory stone", "Restore attributes",
      "Identify possessions" },
    { 0, 0, 0, 0 },
    { 'g', 'l', 'r', 'i' },
    { BACT_GREET, BACT_LEGENDS, BACT_RESTORE, BACT_IDENTS },
    CLASS_CORRUPTED
  },

  {
    "Benetar", "Human", 4,

    { "Greet the Master Mimic", "Look at spires", "Identify possessions",
      "Restore natural shape" },
    { 0, 0, 1000, 0 },
    { 'g', 'l', 'i', 'r' },
    { BACT_GREET, BACT_LEGENDS, BACT_IDENTS, BACT_NORMAL_SHAPE },
    CLASS_MIMIC
  }
};


/* //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\ */


/*
 * Clear the building information
 */
static void clear_bldg(void) {
  int i;
  
  for(i = 3; i < 19; i++) {
    prt("",i,0);
  }
  prt("", 0, 0);
}

/*
 * Places a building reward at the doorstep for the player -KMW-
 */

static void put_reward(byte thetval, byte thesval, int dunlevel) {
  object_type *q_ptr, forge;
  int i, choice;

  choice = 0;
  
  for(i=1; i < MAX_K_IDX; i++) {
    object_kind *k_ptr = &k_info[i];

    if (k_ptr->tval == thetval && k_ptr->sval == thesval) {
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
 * Display fruit for dice slots.
 */

static void display_fruit(int row, int col, int fruit) {
  switch(fruit) {
  case 0:
    c_put_str(TERM_YELLOW, "   ####.", row, col);
    c_put_str(TERM_YELLOW, "  #    #", row+1, col);
    c_put_str(TERM_YELLOW, " #     #", row+2, col);
    c_put_str(TERM_YELLOW, "#      #", row+3, col);
    c_put_str(TERM_YELLOW, "#      #", row+4, col);
    c_put_str(TERM_YELLOW, "#     # ", row+5, col);
    c_put_str(TERM_YELLOW, "#    #  ", row+6, col);
    c_put_str(TERM_YELLOW, ".####   ", row+7, col);
    prt(" Lemon  ", row+8, col);
    break;

  case 1:
    c_put_str(TERM_ORANGE, "   ##   ", row, col);
    c_put_str(TERM_ORANGE, "  #..#  ", row+1, col);
    c_put_str(TERM_ORANGE, " #....# ", row+2, col);
    c_put_str(TERM_ORANGE, "#......#", row+3, col);
    c_put_str(TERM_ORANGE, "#......#", row+4, col);
    c_put_str(TERM_ORANGE, " #....# ", row+5, col);
    c_put_str(TERM_ORANGE, "  #..#  ", row+6, col);
    c_put_str(TERM_ORANGE, "   ##   ", row+7, col);
    prt(" Orange ", row+8, col);
    break;

  case 2:
    c_put_str(TERM_SLATE, "   /\\  ", row, col);
    c_put_str(TERM_SLATE, "   ##   ", row+1, col);
    c_put_str(TERM_SLATE, "   ##   ", row+2, col);
    c_put_str(TERM_SLATE, "   ##   ", row+3, col);
    c_put_str(TERM_SLATE, "   ##   ", row+4, col);
    c_put_str(TERM_SLATE, "   ##   ", row+5, col);
    c_put_str(TERM_UMBER, " ###### ", row+6, col);
    c_put_str(TERM_UMBER, "   ##   ", row+7, col);
    prt(" Sword  ", row+8, col);
    break;

  case 3:
    c_put_str(TERM_SLATE, " ###### ", row, col);
    c_put_str(TERM_SLATE, "#      #", row+1, col);
    c_put_str(TERM_SLATE, "# ++++ #", row+2, col);
    c_put_str(TERM_SLATE, "# +==+ #", row+3, col);
    c_put_str(TERM_SLATE, "#  ++  #", row+4, col);
    c_put_str(TERM_SLATE, " #    # ", row+5, col);
    c_put_str(TERM_SLATE, "  #  #  ", row+6, col);
    c_put_str(TERM_SLATE, "   ##   ", row+7, col);
    prt(" Shield ", row+8, col);
    break;

  case 4:
    c_put_str(TERM_VIOLET, "   ##   ", row, col);
    c_put_str(TERM_VIOLET, " ###### ", row+1, col);
    c_put_str(TERM_VIOLET, "########", row+2, col);
    c_put_str(TERM_VIOLET, "########", row+3, col);
    c_put_str(TERM_VIOLET, "########", row+4, col);
    c_put_str(TERM_VIOLET, " ###### ", row+5, col);
    c_put_str(TERM_VIOLET, "  ####  ", row+6, col);
    c_put_str(TERM_VIOLET, "   ##   ", row+7, col);
    prt("  Plum  ", row+8, col);
    break;

  case 5:
    c_put_str(TERM_RED, "      ##", row, col);
    c_put_str(TERM_RED, "   ###  ", row+1, col);
    c_put_str(TERM_RED, "  #..#  ", row+2, col);
    c_put_str(TERM_RED, "  #..#  ", row+3, col);
    c_put_str(TERM_RED, " ###### ", row+4, col);
    c_put_str(TERM_RED, "#..##..#", row+5, col);
    c_put_str(TERM_RED, "#..##..#", row+6, col);
    c_put_str(TERM_RED, " ##  ## ", row+7, col);
    prt(" Cherry ", row+8, col);
    break;
  }
}



/*
 * Gambling games.
 */

static void gamble_game(int type) {
  char buff[30];
  long wager;
  int odds = 0;
  bool winner = FALSE;

  clear_bldg();

  if (p_ptr->au == 0) {
    mprint(MSG_TEMP, "Haha, another one suckered. You better go home.");
    msg_print(NULL);
    return;
  }

  buff[0] = '\0';

  get_string("Your wager? ", buff, 10);

  wager = atol(buff);

  if (wager < 1) {
    mprint(MSG_TEMP, "OK, let's start with 1gp.");
    msg_print(NULL);
    wager = 1;
  }

  if (p_ptr->au < wager) {
    mprint(MSG_TEMP, "You don't have the gold.");
    mformat(MSG_TEMP, "I'll take %ldgp of that.", p_ptr->au);
    msg_print(NULL);

    wager = p_ptr->au;
  }

  prt(format("Gold before game: %ld", p_ptr->au), 16, 2);
  prt(format("Current wager:    %ld", wager), 17, 2);



  switch (type) {
  case GAME_IN_BETWEEN:
    {
      int roll1, roll2, choice;

      c_prt(TERM_GREEN, "In-Between", 4, 2);
      odds = 3;
      roll1 = randint(20);
      roll2 = randint(20);
      choice = randint(20);

      prt(format("Black die: %-10d Black die: %d", roll1, roll2), 8, 2);
      prt(format("Red die: %d", choice), 9, 2);

      if ((choice > roll1 && choice < roll2) || 
	  (choice < roll1 && choice > roll2)) {
	winner = TRUE;
      }
      break;
    }

  case GAME_CRAPS:
    {
      int roll1, roll2, choice;
      int prev = 0;

      c_prt(TERM_GREEN, "Craps", 4, 2);
      odds = 3;

      while (TRUE) {

	roll1 = randint(6);
	roll2 = randint(6);
	choice = roll1 + roll2;

	prt(format("First roll: %-6d Second roll: %-6d Total: %-d", 
		   roll1, roll2, choice), 8, 2);

	if (prev == 0) {

	  if (choice == 7 || choice == 11) {
	    winner = TRUE;
	    break;
	  } else if (choice == 2 || choice == 3 || choice == 12) {
	    winner = FALSE;
	    break;
	  }

	} else {
	  
	  if (choice == prev) {
	    winner = TRUE;
	    break;
	  } else if (choice == 3) {
	    winner = FALSE;
	    break;
	  }

	}

	prev = choice;
      }
      break;
    }
	  
  case GAME_SPIN_WHEEL:
    {
      int choice, roll1;

      odds = 10;
      
      c_prt(TERM_GREEN, "Wheel", 4, 2);
      
      prt("0  1  2  3  4  5  6  7  8  9", 8, 2);
      prt("----------------------------", 9, 2);

      buff[0] = '\0';
      get_string("Pick a number (0-9): ", buff, 1);
      choice = atoi(buff);

      roll1 = rand_int(10);
      prt(format("The wheel spins to a stop, and winner is: %d", roll1), 
	  13, 2);
      
      prt("", 9, 0);
      prt("*", 9, (3*roll1)+5);
      
      if (roll1 == choice) {
	winner = TRUE;
      }
      break;
    }

  case GAME_DICE_SLOTS:
    {
      int roll3, roll1, roll2;

      clear_bldg();
      c_prt(TERM_GREEN, "Dice Slots", 4, 2);
      
      roll1 = randint(6);
      roll2 = randint(6);
      roll3 = randint(6);

      prt("/--------------------------\\", 7, 2);
      prt("\\--------------------------/", 17, 2);

      display_fruit(8, 3, roll1-1);
      display_fruit(8, 12, roll2-1);
      display_fruit(8, 21, roll3-1);
      
      if (roll1 == roll2 && roll2 == roll3) {
	winner = TRUE;

	if (roll1 == 1)       odds = 4;
	else if (roll1 == 2)  odds = 6;
	else                  odds = roll1 * roll1;

      } else if (roll1 == 6 && roll2 == 6) {
	winner = TRUE;
	odds = roll3 + 1;

      }
      break;
    }
  }

  if (winner) {
    c_prt(TERM_YELLOW, "You Won", 16, 37);
    p_ptr->au = p_ptr->au + (odds * wager);
    prt(format("Payoff: %d", odds), 17, 37);
  } else {
    c_prt(TERM_RED, "You Lost", 16, 37);
    p_ptr->au = p_ptr->au - wager;
    prt("", 17, 37);
  }

  prt(format("Current Gold: %ld", p_ptr->au), 6, 2);
}


/*
 * Reward the player, given a class. Use a class of -1 for ``generic''
 * rewards.
 */

static void greet_char(int class, bool free) {
  int max_idx = p_ptr->lev / 5;
  int cur_idx = -1;
  int tval = 0;
  int sval = 0;
  int rand = randint(5);

  if (max_idx > MAX_REWARDS) max_idx = MAX_REWARDS;

  /* Check the rewards array. */

  if (!free) {
    int i;

    /* Level is too low. */
    if (max_idx == 0) {
      mprint(MSG_TEMP, "Return when you have faced the reality of the dungeon, apprentice.");
      msg_print(NULL);
      return;
    }

    /* Scan the rewards array, save first available spot as cur_idx */
    for (i = 0; i < max_idx; i++) {
      if (!rewards[i]) {
	cur_idx = i;
	break;
      }
    }

    /* All rewards up to current level have been collected. */
    if (cur_idx < 0) {

      /* No more rewards to collect */
      if (max_idx == MAX_REWARDS) {
	mprint(MSG_TEMP, "Your greeting is returned with respect.");
	msg_print(NULL);
	return;

      } else {
	mformat(MSG_TEMP, "You have been rewarded -- come back when level %d",
		5*(max_idx+1));
	msg_print(NULL);
	return;
      }
    }

    /* Collect the reward */
    rewards[cur_idx] = TRUE;
  }

  mprint(MSG_BONUS, "Well done. You have earned a gift.");
  msg_print(NULL);
      
  switch (class) {
  case -1:
    object_level = p_ptr->lev;
    acquirement(p_ptr->py, p_ptr->px, 1, TRUE);
    object_level = p_ptr->depth;
    break;

  case CLASS_WARRIOR:
  case CLASS_LYCANTH:
    
    switch (rand) {
    case 1:
      tval = TV_SWORD;
      sval = SV_DAGGER;
      break;

    case 2:
      tval = TV_SWORD;
      sval = SV_LONG_SWORD;
      break;

    case 3:
      tval = TV_HAFTED;
      sval = SV_MACE;
      break;

    case 4:
      tval = TV_POLEARM;
      sval = SV_LANCE;
      break;

    case 5:
      tval = TV_POLEARM;
      sval = SV_BROAD_AXE;
      break;
    }

    put_reward(tval, sval, p_ptr->lev);
    break;

  case CLASS_ROGUE:
    {
      int i = p_ptr->lev * 40;

      p_ptr->au += i;
      mformat(MSG_BONUS, "You collected %d gold pieces of stolen property.", i);
      break;
    }


  case CLASS_MAGE:
  case CLASS_MIMIC:
  case CLASS_ILLUSIONIST:
  case CLASS_CORRUPTED:

    switch (rand) {
    case 1:
      tval = TV_RING;
      sval = SV_RING_FLAMES;
      break;

    case 2:
      tval = TV_ROD;
      sval = SV_ROD_RECALL;
      break;

    case 3:
      tval = TV_STAFF;
      sval = SV_STAFF_POWER;
      break;

    case 4:
      tval = TV_RING;
      sval = SV_RING_ICE;
      break;

    case 5:
      tval = TV_ROD;
      sval = SV_ROD_DISARMING;
      break;
    }

    put_reward(tval, sval, p_ptr->lev);
    break;

  case CLASS_PRIEST:
  case CLASS_PALADIN:

    switch (rand) {
    case 1:
      tval = TV_RING;
      sval = SV_RING_PROTECTION;
      break;

    case 2:
      tval = TV_STAFF;
      sval = SV_STAFF_DISPEL_EVIL;
      break;

    case 3:
      tval = TV_HAFTED;
      sval = SV_MACE;
      break;

    case 4:
      tval = TV_HAFTED;
      sval = SV_FLAIL;
      break;

    case 5:
      tval = TV_SCROLL;
      sval = SV_SCROLL_ACQUIREMENT;
      break;
    }

    put_reward(tval, sval, p_ptr->lev);
    break;

  case CLASS_RANGER:

    switch (rand) {
    case 1:
    case 2:
      tval = TV_BOW;
      sval = SV_LONG_BOW;
      break;

    case 3:
    case 4:
      tval = TV_ARROW;
      sval = SV_AMMO_HEAVY;
      break;

    case 5:
      tval = TV_BOOTS;
      sval = SV_PAIR_OF_METAL_SHOD_BOOTS;
      break;
    }

    put_reward(tval, sval, p_ptr->lev);
    break;

  case CLASS_BEASTMASTER:

    switch (rand) {
    case 1:
    case 2:
      tval = TV_POTION;
      sval = SV_POTION_HEALING;
      break;

    case 3:
    case 4:
      tval = TV_POTION;
      sval = SV_POTION_ENLIGHTENMENT;
      break;

    case 5:
      tval = TV_HAFTED;
      sval = SV_WHIP;
      break;
    }

    put_reward(tval, sval, p_ptr->lev);
    break;

  }
}



/*
 * Print simple analysis of weapon.
 */

static void research_weapon(void) {
  object_type* o_ptr = NULL;
  char o_name[80];

  clear_bldg();

  o_ptr = &inventory[INVEN_WIELD];

  if (!o_ptr->k_idx) {
    mprint(MSG_TEMP, "You are not wielding a weapon.");
    msg_print(NULL);
    return;
  }

  p_ptr->update |= (PU_BONUS);
  update_stuff();

  put_str("Based on your current abilities, here is what your weapon will do:", 6, 2);
  
  /* TODO - take into account the other pieces such as speed from
     calc_weapon */
  
  object_desc(o_name, o_ptr, FALSE, 0);
  c_put_str(TERM_YELLOW, o_name, 8, 5);
  
  put_str(format("To Hit: %-2d    To Damage: %-2d", 
		 o_ptr->to_h, o_ptr->to_d), 9, 5);
  
  put_str(format("Dice:   %-2d    Sides:     %-2d", 
		 o_ptr->dd, o_ptr->ds), 10, 5);
  
  put_str(format("Number of Blows: %d", p_ptr->num_blow), 11, 5);
  
  c_put_str(TERM_YELLOW, "Possible Damage:", 13, 5);


  put_str(format("One Strike:  %d-%d damage", 
		 o_ptr->dd * 1 + o_ptr->to_d,
		 o_ptr->ds * o_ptr->dd + o_ptr->to_d), 14, 5);

  put_str(format("One Attack:  %d-%d damage",
		 p_ptr->num_blow * (o_ptr->dd + o_ptr->to_d),
		 p_ptr->num_blow * (o_ptr->dd * o_ptr->ds + o_ptr->to_d)),
	  15, 5);
}


/*
 * Enchant something. It could be some stuff in your pack, or
 * something you're weilding.
 */

static bool enchant_something(cptr name, int where, int tval, 
			     bool plural, bool armor) {

  int maxenchant, i = 0;
  object_type *o_ptr;
  char o_name[80];
  bool ret = FALSE;

  if (where >= INVEN_TOTAL) return FALSE;

  if (where == INVEN_PACK) {
    for (i = 0; i < INVEN_PACK; i++) {
      o_ptr = &inventory[i];

      if (o_ptr->tval == tval) break;
    }

  } else {
    o_ptr = &inventory[where];
  }

  if (i == INVEN_PACK || !o_ptr->k_idx) {
    mprint(MSG_TEMP, "You do not have anything we could enchant.");
    msg_print(NULL);
    return FALSE;
  }

  maxenchant = (p_ptr->lev / 5);

  if (maxenchant == 0) {
    mprint(MSG_TEMP, "You have not attained the status for that.");
    msg_print(NULL);
    return FALSE;
  }

  object_desc(o_name, o_ptr, FALSE, 1);

  clear_bldg();   

  prt(format("  Based on your skill, we can echant your %s%s up to +%d", 
	     name, (plural ? "s" : ""), maxenchant), 5, 0);

  prt(format("  %^s Status:", name), 7, 0);


  if (o_ptr->name1) {
    prt(format("%-40s: In fine condition", o_name), 9, 0);

  } else if ((!armor && (o_ptr->to_h < -3 || o_ptr->to_d < -3)) || 
	     (armor && o_ptr->to_a < -3)) {
    prt(format("%-40s: Beyond repair -- buy %s new %s%s", 
	       o_name, 
	       (plural ? "some" : "a"), name,
	       (plural ? "s" : "")), 9, 0);

  } else if ((o_ptr->to_h >= maxenchant && o_ptr->to_d >= maxenchant) ||
	     o_ptr->to_a >= maxenchant) {
    prt(format("%-40s: In fine condition", o_name), 9, 0);

  } else  {
    ret = TRUE;
    if (armor) {
      o_ptr->to_a = maxenchant;

      prt(format("%-40s: Enchanted to (%d)", o_name, o_ptr->to_a), 9, 0);

    } else {
      o_ptr->to_h = maxenchant;
      o_ptr->to_d = maxenchant;

      prt(format("%-40s: Enchanted to (%d,%d)", o_name,
		 o_ptr->to_h, o_ptr->to_d), 9, 0);
    }
  }

  return ret;
}

/*
 * Enter the arena.
 */

static void enter_arena(void) {
  if (p_ptr->arena_number[p_ptr->which_arena] == MAX_ARENA_MONS) {
    clear_bldg();
    prt("               Arena Victor!", 5, 0);
    prt("Congratulations!  You have defeated all before you.", 7, 0); 
    prt("For that, receive the prize: 10,000 gold pieces", 8, 0);
    prt("",10,0);
    prt("", 11, 0);
    p_ptr->au += 10000;
    mprint(MSG_TEMP, "Press the space bar to continue");
    msg_print(NULL);
    p_ptr->arena_number[p_ptr->which_arena]++;

  } else if (p_ptr->arena_number[p_ptr->which_arena] > MAX_ARENA_MONS) {
    mprint(MSG_TEMP, "You enter the arena briefly and bask in your glory.");
    msg_print(NULL);

  } else {
    /* Player enters the arena from the town */
    if (!p_ptr->inside_special) {
      p_ptr->inside_special = 1;
      p_ptr->oldpy = p_ptr->py;
      p_ptr->oldpx = p_ptr->px;
    }

    p_ptr->leaving = TRUE;
    p_ptr->exit_bldg = FALSE;

    mega_hack_exit_bldg = TRUE;
  }
}

/*
 * Show religious info.
 */

bool show_god_info(bool ext) {
  int badness = interpret_grace();
  int pgod = p_ptr->pgod - 1;
  int tmp;

  if (pgod < 0) {
    mprint(MSG_TEMP, "You don't worship anyone.");
    msg_print(NULL);
    return FALSE;
  } else {
    msg_print(NULL);

    Term_save();

    roff(format("You worship %s, the God of %s. ", deity_info[pgod].name,
	       deity_info[pgod].god_of));
    roff(format("%s is %s, and you are %s by him. ", deity_info[pgod].name,
	       deity_niceness[deity_info[pgod].grace_deduction],
	       deity_standing[badness]));
    roff(format("%s hates %s. He holds sacred %s.", deity_info[pgod].name,
	       deity_affiliation[deity_info[pgod].opposed-1],
	       deity_affiliation[deity_info[pgod].aligned-1]));
    roff("\n");

    if (ext) {
      int fav = badness - interpret_favor();

      roff(format("\nIt is %s that your prayers will be answered.\n", favor_text[fav]));
    }

    tmp = inkey();

    Term_load();
  }

  return TRUE;
}


/*
 * Display quest information
 */

static void show_quest_text(vault_type* v_ptr) {
  clear_bldg();
  move_cursor(6, 0);

  c_roff(TERM_RED, format("Quest name: %-20s Type: %s\n\n",
			  v_name + v_ptr->name, quest_types[v_ptr->q_type]));
  c_roff(TERM_YELLOW, q_text + v_ptr->q_text);
}


/*
 * Request a quest from the Lord. 
 */

static void ask_quest() {
  int i, ch;
  byte foo;

  if (p_ptr->which_quest != 0) {
    foo = quest_status[p_ptr->which_quest-1];

    if (foo == QUEST_ASSIGNED || foo == QUEST_IN_PROGRESS) {
      mprint(MSG_TEMP, "Return when you have completed your current quest.");
      msg_print(NULL);
      return;
    }

    if (foo == QUEST_COMPLETED) {
      greet_char(-1, TRUE);
      p_ptr->which_quest = 0;
      return;
    }
  }

  for (i = 0; i < max_quests; i++) {
    if (quest_status[i] == 0) {
      show_quest_text(q_v_ptrs[i]);

      ch = get_three_way_check("Accept this quest? ");

      if      (ch == -1) return;
      else if (ch == 0)  continue;

      quest_status[i] = QUEST_ASSIGNED;

      /* Shifted by one, since zero means ``no quest'', as always. */
      p_ptr->which_quest = i+1;
      return;
    }
  }

  mformat(MSG_TEMP,
	  "All hail %s, the greatest of heroes who has completed all quests!",
	  op_ptr->full_name);
  mformat(MSG_TEMP, "I humbly offer you this meager gift.");
  greet_char(-1, TRUE);
}


/*
 * Show town legends based on a display type, class, and race.
 */

static void show_legends(cptr msg, int type, int class, int race) {
  int i = 5;
  int j = 0;

  int k;
  bool foo;
  score_info score;

  clear_bldg();

  c_prt(TERM_YELLOW, msg, 4, 2);
  open_highscore();

  while (TRUE) {

    if (!next_highscore(&score)) break;

    foo = FALSE;

    for (k = 0; k < MAX_ARENAS; k++) {
      if (score.arena_number[k] > MAX_ARENA_MONS) foo = TRUE;
    }

    if ((type == LEGENDS_RACE && score.race == race) ||
	(type == LEGENDS_CLASS && score.class == class) ||
	(type == LEGENDS_KING && score.plev >= PY_MAX_LEVEL) ||
	(type == LEGENDS_ARENA && foo)) {
      prt(format("%3d) %s the %s (Level %2d)", j, score.name, 
		 race_info[score.race].title, score.plev), i, 0);
      j++;
      i++;
    }
    
    if (i == 17) {
      i = 5;
      mprint(MSG_TEMP, "Press space for more.");
      msg_print(NULL);
    }
  }

  close_highscore();
}


/* Show race legends */

static void race_legends(void) {
  int i;

  for (i = 0; i < MAX_RACES; i++) {
    clear_bldg();
    show_legends(format("The greatest of the %ss", race_info[i].title),
		 LEGENDS_RACE, 0, i);
    mprint(MSG_TEMP, "Press space for more.");
    msg_print(NULL);
  }
}


/*
 * Rest for the night.
 */

static void mass_rest(void) {
  int dawnval;
  int n;

  dawnval = ((turn % (10L * TOWN_DAWN)));
	
  /* Nighttime */
  if (dawnval > 50000) {

    if (p_ptr->poisoned > 0 || p_ptr->cut > 0) {
      mprint(MSG_TEMP, "You need a healer, not a room.");
      mprint(MSG_TEMP, "Sorry, but don't want anyone dying in here.");
      msg_print(NULL);

    } else {
      turn = ((turn/50000)+1)*50000;
      p_ptr->chp = p_ptr->mhp;
      set_blind(0);
      set_confused(0);
      p_ptr->stun = 0;
      mprint(MSG_TEMP, "You awake refreshed for the new day.");
      msg_print(NULL);

      /* Maintain each shop (except home) */
      for (n = 0; n < MAX_STORES - 1; n++) {
	/* Maintain */
	store_maint(n);
      }
    }

  } else {
      mprint(MSG_TEMP, "The rooms are available only at night.");
      msg_print(NULL);
  }
}

/*
 * Select an arena.
 */

static void select_arena(void) {
  char inp;

  clear_bldg();

  prt("            Available arenas:", 5, 0);
  prt("a) Humanoid monsters.", 7, 0);
  prt("b) Animal monsters.", 8, 0);
  prt("c) Monstrosities.", 9, 0);

  while(1) {
    inp = tolower(inkey());

    if (inp == ESCAPE) {
      clear_bldg();
      break;

    } else if (islower(inp) && A2I(inp) < MAX_ARENAS) {
      p_ptr->which_arena = A2I(inp);
      clear_bldg();
      break;

    } else {
      bell();
    }
  }
}


/*
 * Display a building.
 */

static void show_building(building* bldg) {
  char buff[20];
  int i;
  int j = bldg->num_actions;

  prt(format("%s (%s) %35s", bldg->name, bldg->race, 
	     f_name + f_info[cave_feat[p_ptr->py][p_ptr->px]].name), 2, 1);
  prt("You may:", 19, 0);

  for (i = 0; i < j; i++) {

    if (bldg->class >= 0) {
      if (bldg->costs[i] == 0 && bldg->class != p_ptr->pclass) {
	strcpy(buff, "(closed)");

      } else if (bldg->class != p_ptr->pclass) {
	sprintf(buff, "(%dgp)", bldg->costs[i]);

      } else {
	buff[0] = '\0';
      }

    } else {
      if (bldg->costs[i] == 0) {
	buff[0] = '\0';

      } else {
	sprintf(buff, "(%dgp)", bldg->costs[i]);
      }
    }

    prt(format(" %c) %s %s", bldg->letters[i], bldg->act_names[i], buff),
	19+(i/2), 35*(i%2));
  }

  prt(" ESC) Exit building", 23, 0);
}


/*
 * Execute a building activation. 
 */

static bool bldg_activation(building* bldg, int i) {
  int bact = bldg->actions[i];
  bool ret = TRUE;

  switch (bact) {
  case BACT_RESEARCH_ITEM:
    identify_fully();
    break;

  case BACT_TOWN_HISTORY:
    show_file("town.txt", NULL, 0, 0);
    clear_bldg();
    show_building(bldg);
    break;

  case BACT_RACE_LEGENDS:
    race_legends();
    break;

  case BACT_GREET_KING:
    greet_char(-1, FALSE);
    break;

  case BACT_KING_LEGENDS:
    show_legends("Greatest of the ancient Heroes", LEGENDS_KING, 0, 0);
    break;

  case BACT_QUEST:
    ask_quest();
    break;

  case BACT_SWITCH_ARENA:
    select_arena();
    break;

  case BACT_POSTER:
    {
      monster_race* r_ptr;
      cptr name;

      if (p_ptr->arena_number[p_ptr->which_arena] == MAX_ARENA_MONS) {
	mprint(MSG_TEMP, "You are victorious. Enter the arena for the ceremony.");
	msg_print(NULL);

      } else if (p_ptr->arena_number[p_ptr->which_arena] > MAX_ARENA_MONS) {
	mprint(MSG_TEMP, "You have won against all foes.");
	msg_print(NULL);

      } else {
	r_ptr = &r_info[arena_monsters[p_ptr->which_arena]
		       [p_ptr->arena_number[p_ptr->which_arena]]];
	name = (r_name + r_ptr->name);

	mformat(MSG_TEMP, "Do I hear any challenges against: %s", name);
	msg_print(NULL);
      }
      break;
    }

  case BACT_ARENA_RULES:
    show_file("arena.txt", NULL, 0, 0);
    clear_bldg();
    show_building(bldg);
    break;

  case BACT_ARENA:
    enter_arena();
    break;

  case BACT_ARENA_LEGENDS:
    show_legends("Arena Champions", LEGENDS_ARENA, 0, 0);
    break;

  case BACT_IN_BETWEEN:
    gamble_game(GAME_IN_BETWEEN);
    break;

  case BACT_GAMBLE_RULES:
    show_file("gambling.txt", NULL, 0, 0);
    clear_bldg();
    show_building(bldg);
    break;

  case BACT_CRAPS:
    gamble_game(GAME_CRAPS);
    break;

  case BACT_SPIN_WHEEL:
    gamble_game(GAME_SPIN_WHEEL);
    break;

  case BACT_DICE_SLOTS:
    gamble_game(GAME_DICE_SLOTS);
    break;

  case BACT_REST:
    mass_rest();
    break;

  case BACT_FOOD:
    mprint(MSG_TEMP, "The barkeep gives you some gruel and a beer.");
    msg_print(NULL);
    set_food(PY_FOOD_MAX-1);
    break;

  case BACT_RUMORS:
    {
      char buff[80];

      get_random_line("rumors.txt", buff);
      mprint(MSG_TEMP, buff);
      msg_print(NULL);
      break;
    }

  case BACT_RESEARCH_MONSTER:
    research_mon();
    break;

  case BACT_RESEARCH_WEAPON:
    research_weapon();
    break;

  case BACT_LEGENDS:
    show_legends(format("Greatest %ss", class_info[bldg->class].title),
		 LEGENDS_CLASS, bldg->class, 0);
    break;

  case BACT_ENCHANT_WEAPON:
    ret = enchant_something("weapon", INVEN_WIELD, 0, FALSE, FALSE);
    break;

  case BACT_ENCHANT_ARMOR:
    ret = enchant_something("armor", INVEN_BODY, 0, FALSE, TRUE);
    break;

  case BACT_RECHARGE:
    if (p_ptr->lev < 25) {
      mprint(MSG_TEMP, "You have not attained the status for that.");
      msg_print(NULL);
      ret = FALSE;
    } else {
      recharge(30);
    }
    break;

  case BACT_IDENTS:
    if (p_ptr->lev < 35) {
      mprint(MSG_TEMP, "You have not attained the status for that.");
      msg_print(NULL);
      ret = FALSE;
    } else {
      identify_pack();
      mprint(MSG_TEMP, "Your posessions have been identified.");
      msg_print(NULL);
    }
    break;

  case BACT_LEARN:
    do_cmd_study();
    break;

  case BACT_HEALING:
    if (p_ptr->lev < 25) {
      mprint(MSG_TEMP, "You have not attained the status for that.");
      msg_print(NULL);
      ret = FALSE;
    } else {
      ret = FALSE;

      if (hp_player(200)) ret = TRUE;
      if (set_poisoned(0)) ret = TRUE;
      if (set_blind(0)) ret = TRUE;
      if (set_confused(0)) ret = TRUE;
      if (set_cut(0)) ret = TRUE;
      if (set_stun(0)) ret = TRUE;
    }
    break;

  case BACT_RESTORE:
    if (p_ptr->lev < 25) {
      mprint(MSG_TEMP, "You have not attained the status for that.");
      msg_print(NULL);
      ret = FALSE;
    } else {
      ret = FALSE;

      if (do_res_stat(A_STR)) ret = TRUE;
      if (do_res_stat(A_INT)) ret = TRUE;
      if (do_res_stat(A_WIS)) ret = TRUE;
      if (do_res_stat(A_DEX)) ret = TRUE;
      if (do_res_stat(A_CON)) ret = TRUE;
      if (do_res_stat(A_CHR)) ret = TRUE;
    }
    break;

  case BACT_RELIGION:
    ret = show_god_info(TRUE);
    break;

  case BACT_ENCHANT_ARROWS:
    ret = enchant_something("arrow", INVEN_PACK, TV_ARROW, TRUE, FALSE);
    break;
    
  case BACT_ENCHANT_BOW:
    ret = enchant_something("launcher", INVEN_BOW, 0, FALSE, FALSE);
    break;

  case BACT_NORMAL_SHAPE:
    set_shape(0, 0);
    msg_print(NULL);
    break;

  case BACT_GREET:
    greet_char(bldg->class, FALSE);
    break;
  }

  return ret;
}


/*
 * Process building commands.
 */

static void bldg_command(int which, bool class_b) {
  building* bldg;
  char inp;
  int i;
  int j;
  int cost;
  bool did_key = FALSE;


  /* Enter icky mode */
  character_icky = TRUE;
  Term_clear();

  /* Mega-hack -- flag to exit building */
  mega_hack_exit_bldg = FALSE;


  if (class_b) {
    bldg = &class_bldgs[which];
  } else {
    bldg = &city[which];
  }

  j = bldg->num_actions;

  show_building(bldg);
  
  while (TRUE) {
    inp = inkey();
    did_key = FALSE;

    if (isalnum(inp)) {
      did_key = FALSE;

      for (i = 0; i < j; i++) {
	if (inp == bldg->letters[i]) {

	  did_key = TRUE;

	  cost = bldg->costs[i];

	  /* Pay for services */
	  if (cost > 0) {
	    if (class_b && bldg->class == p_ptr->pclass) {
	      bldg_activation(bldg, i);

	    } else if (p_ptr->au < cost) {
	      mprint(MSG_TEMP, "You don't have the gold!");
	      msg_print(NULL);
	      break;

	    } else {
	      /* Activate the building */
	      if (bldg_activation(bldg, i))
		p_ptr->au -= cost;
	    }

	  } else {
	    if (class_b && bldg->class != p_ptr->pclass) {
	      mformat(MSG_TEMP, "Only %ss can do that here!", 
			 class_info[bldg->class].title);
	      msg_print(NULL);
	      break;
	      
	    } else {
	      bldg_activation(bldg, i);

	    }
	  }
	}
      }
    }
      

    if (inp == ESCAPE || mega_hack_exit_bldg) {
      /* Player exits from the arena */
      if (p_ptr->inside_special) {
	p_ptr->leaving = TRUE;

	/* Arena task completed */
	if (p_ptr->exit_bldg) {
	  p_ptr->inside_special = FALSE;
	}
      }
      break;
    }

    if (!did_key) {
      bell();
    }
  }

  /* Exit icky mode */
  character_icky = FALSE;

  
  p_ptr->update |= (PU_TORCH);
  p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_SANITY);
  p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);
  p_ptr->update |= (PU_VIEW | PU_LITE);
  p_ptr->update |= (PU_MONSTERS);

  p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP);

  p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);
  p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_MONSTER | PW_OBJECT);

  handle_stuff();
}


/*
 * Do building commands
 */

void do_cmd_bldg(void) {
  int which;
  int px = p_ptr->px;
  int py = p_ptr->py;
  
  if (!(cave_feat[py][px] >= FEAT_BLDG_HEAD && 
	cave_feat[py][px] <= FEAT_BLDG_TAIL)) {
    mprint(MSG_TEMP, "You see no building here.");
    return;
  }

  which = (cave_feat[py][px] - FEAT_BLDG_HEAD);

  if (!p_ptr->exit_bldg) {
    mprint(MSG_TEMP, "The gates are closed. The monster awaits!");
    msg_print(NULL);
    return;
  }

  if (which < 7) {
    bldg_command(which, FALSE);
  } else if (which >= 10 && which <= 19) {
    bldg_command(which - 10, TRUE);
  } else {
    mformat(MSG_TEMP, "The %s is off-limits!", 
	       f_name + f_info[cave_feat[py][px]].name);
    return;
  }
};




/*
 * Enter quest level
 */
void do_cmd_quest(void) {
  if (!(cave_feat[p_ptr->py][p_ptr->px] == FEAT_QUEST_ENTER)) {
    mprint(MSG_TEMP, "You see no quest level here.");
    return;

  } else if (p_ptr->which_quest == 0) {
    mprint(MSG_TEMP, "This entrance is locked for some reason.");
    return;
    
  } else if (quest_status[p_ptr->which_quest-1] == QUEST_COMPLETED) {
    mprint(MSG_TEMP, "You already completed your quest.");
    return;

  } else {
    p_ptr->oldpy = p_ptr->py;
    p_ptr->oldpx = p_ptr->px;
    p_ptr->inside_special = 2;
    p_ptr->depth = 1;

    p_ptr->leaving = TRUE;
  }
}


/*
 * Complete your current quest.
 */

void complete_quest(void) {
  /* Paranoia */
  if (p_ptr->which_quest == 0) return;

  quest_status[p_ptr->which_quest-1] = QUEST_COMPLETED;
  mprint(MSG_BONUS, "You have completed your quest.");
  msg_print(NULL);
}

/*
 * Exit a quest level.
 */

void exit_quest(void) {
  vault_type* v_ptr;

  p_ptr->inside_special = 0;
  p_ptr->depth = 0;
  p_ptr->leaving = TRUE;

  /* Paranoia */
  if (p_ptr->which_quest == 0) return;

  v_ptr = q_v_ptrs[p_ptr->which_quest-1];

  if (v_ptr->q_type == QT_SCOUT) {
    complete_quest();
  }
}
