#define STORE_C
/* File: store.c */

/* Purpose: Store commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


#define RUMOR_CHANCE 8

#define MAX_COMMENT_1	6

/* A toggle to prevent store_maint() from allowing multiple similar stacks. */
/* #define MAINT_99_MAX */

/*
 * We store the current "store number" here so everyone can access it
 */
static int cur_store_num = 0;
static byte cur_store_type = 7;

/*
 * We store the current "store page" here so everyone can access it
 */
static int store_top = 0;

/*
 * We store the current "store pointer" here so everyone can access it
 */
static store_type *st_ptr = NULL;

/*
 * We store the current "owner type" here so everyone can access it
 */
static owner_type *ot_ptr = NULL;

static cptr comment_1[MAX_COMMENT_1] =
{
	"Okay.",
	"Fine.",
	"Accepted!",
	"Agreed!",
	"Done!",
	"Taken!"
};

#define MAX_COMMENT_2A	2

static cptr comment_2a[MAX_COMMENT_2A] =
{
	"You try my patience.  %s is final.",
	"My patience grows thin.  %s is final."
};

#define MAX_COMMENT_2B	12

static cptr comment_2b[MAX_COMMENT_2B] =
{
	"I can take no less than %s gold pieces.",
	"I will accept no less than %s gold pieces.",
	"Ha!  No less than %s gold pieces.",
	"You knave!  No less than %s gold pieces.",
	"That's a pittance!  I want %s gold pieces.",
	"That's an insult!  I want %s gold pieces.",
	"As if!  How about %s gold pieces?",
	"My arse!  How about %s gold pieces?",
	"May the fleas of 1000 orcs molest you!  Try %s gold pieces.",
	"May your most favourite parts go moldy!  Try %s gold pieces.",
	"May Cthulhu find you tasty!  Perhaps %s gold pieces?",
	"Your mother was an Ogre!  Perhaps %s gold pieces?"
};

#define MAX_COMMENT_3A	2

static cptr comment_3a[MAX_COMMENT_3A] =
{
	"You try my patience.  %s is final.",
	"My patience grows thin.  %s is final."
};


#define MAX_COMMENT_3B	12

static cptr comment_3b[MAX_COMMENT_3B] =
{
	"Perhaps %s gold pieces?",
	"How about %s gold pieces?",
	"I will pay no more than %s gold pieces.",
	"I can afford no more than %s gold pieces.",
	"Be reasonable.  How about %s gold pieces?",
	"I'll buy it as scrap for %s gold pieces.",
	"That is too much!  How about %s gold pieces?",
	"That looks war surplus!  Say %s gold pieces?",
	"Never!  %s is more like it.",
	"That's an insult!  %s is more like it.",
	"%s gold pieces and be thankful for it!",
	"%s gold pieces and not a copper more!"
};

#define MAX_COMMENT_4A	4

static cptr comment_4a[MAX_COMMENT_4A] =
{
	"Enough!  You have abused me once too often!",
	"Arghhh!  I have had enough abuse for one day!",
	"That does it!  You shall waste my time no more!",
	"This is getting nowhere!"
};

#define MAX_COMMENT_4B	4

static cptr comment_4b[MAX_COMMENT_4B] =
{
	"Leave my store!",
	"Get out of my sight!",
	"Begone, you scoundrel!",
	"Out, out, out!"
};

#define MAX_COMMENT_5	8

static cptr comment_5[MAX_COMMENT_5] =
{
	"Try again.",
	"Ridiculous!",
	"You will have to do better than that!",
	"Do you wish to do business or not?",
	"You've got to be kidding!",
	"You'd better be kidding!",
	"You try my patience.",
	"Hmmm, nice weather we're having."
};

#define MAX_COMMENT_6	4

static cptr comment_6[MAX_COMMENT_6] =
{
	"I must have heard you wrong.",
	"I'm sorry, I missed that.",
	"I'm sorry, what was that?",
	"Sorry, what was that again?"
};

/* Shop service names, in the order r,z */
static cptr service_name[MAX_STORE_TYPES][2] =
{
	{"", ""},	/* General */
	{"Enchant your armour",""},	/* Armoury */
	{"Enchant your weapon",""},	/* Weapon Shop */
	{"buy Restoration","Spirit Initiation"}, /* Temple */
	{"Identify all",""}, /* Alchemists' Shop */
	{"ritual of Recall","Learn Folk Magic"},	/* Magic Shop */
	{"",""},	/* Black Market */
	{"Rest a while",""},	/* Home */
	{"Research a spell",""},	/* Library */
	{"hire a Room",""},	/* Inn */
	{"Buy a house",""},	/* Hall */
	{"",""}	/* Pawn Shop */
};

static s32b cur_ask, final_ask;
static cptr pmt;
static bool noneedtobargain(s32b minprice);
static int get_which_store(void);
static void do_store_browse( object_type *o_ptr);

/*
 * Determine if haggling is necessary.
 * purse should be 0 if buying as infinity isn't an s32b.
 */

static bool needtohaggle(s32b purse)
{
	int noneed = noneedtobargain(final_ask);
	char message[80];
	bool maxpayout = ((purse > 0) && (final_ask >= purse));

	/* No need to haggle */
	if (noneed || auto_haggle || maxpayout)
	{

		/* No reason to haggle */
		if (maxpayout)
		{
			/* Message */
			sprintf(message, "You instantly agree upon the price.");

			/* Offer full purse */
			final_ask = purse;
		}

		/* No need to haggle */
		else if (noneed)
		{
			/* Message summary */
			sprintf(message, "You eventually agree upon the price.");
		}

		/* No haggle option */
		else if (auto_haggle)
		{
			/* Message summary */
			sprintf(message, "You quickly agree upon the price.");

			/* Apply Sales Tax */
			if (purse == 0)
			{
				/* Buying */
				final_ask = final_ask * 11 / 10;
			}
			else
			{
				/* Selling */
				final_ask = final_ask * 9 / 10;
			}
		}

		/* Final price */
		cur_ask = final_ask;

		if (verbose_haggle)
		{
			/* Print the message */
			msg_print(message);
			msg_print(NULL);
			pmt = "Final Offer";
		}
		else
		{
			/* Take it or leave it. */
			if (purse)
				pmt = "Accept";
			else
				pmt = "Pay";
		}

		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

/*
 * Rest for a night in the Inn or House
 * If the night flag is set, then rest until nightfalll instead of daybreak
  */
static void room_rest(bool night)
{
	int n;
	int temp_store_num;
	byte temp_store_type;
	if(night)
	{
		if (turn < 50000)
		{
			turn = 50000;
		}
		else
		{
			turn = (((turn - 50000)/100000)+1)*100000 + 50000;
		}
	}
	else
	{
		turn = ((turn/100000)+1)*100000;
	}
	p_ptr->chp = p_ptr->mhp;
	set_blind(0);
	set_confused(0);
	p_ptr->stun = 0;
	if(night)
	{
		msg_print("You awake, ready for the night.");
	}
	else
	{
		msg_print("You awake refreshed for the new day.");
	}
	/* Store globals to stop them getting corrupted */
	temp_store_num=cur_store_num;
	temp_store_type=cur_store_type;
	/* Maintain each shop (except homes etc) */
	for (n = 0; n < MAX_STORES_TOTAL - 1; n++)
	{
		/* Maintain */
		if ((store[n].type !=99) &&
			(store[n].type != STORE_HOME) &&
			(store[n].type != STORE_HALL) &&
			(store[n].type != STORE_PAWN))
		{
			store_maint(n);
		}
	}
	/* And restore the globals */
	cur_store_num=temp_store_num;
	cur_store_type=temp_store_type;
	/* Reset the Store and Owner pointers */
	st_ptr = &store[cur_store_num];
	ot_ptr = &owners[st_ptr->owner];

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
	p_ptr->see_inv = 0;		/* Timed -- See Invisible */
	p_ptr->tim_invis = 0;		/* Timed -- Invisibility -KMW- */
	p_ptr->wraith_form = 0;		/* Timed -- walk through walls -KMW- */
	p_ptr->tim_infra = 0;		/* Timed -- Infra Vision */
	p_ptr->oppose_acid = 0;	/* Timed -- oppose acid */
	p_ptr->oppose_elec = 0;	/* Timed -- oppose lightning */
	p_ptr->oppose_fire = 0;	/* Timed -- oppose heat */
	p_ptr->oppose_cold = 0;	/* Timed -- oppose cold */
	p_ptr->oppose_pois = 0;	/* Timed -- oppose poison */

	new_level_flag = TRUE;
	came_from = START_WALK; /* We don't want the player to be moved */
}

/*
 * Free Homes - check if the town has any homes for sale
  */

static bool free_homes(void)
{
	int i,tot;
	tot=0;
	for(i= (MAX_STORES_PER_TOWN * cur_town); i < ((MAX_STORES_PER_TOWN * cur_town) + (town_defs[cur_town].numstores)); i++)
	{
		if((store[i].type == STORE_HOME) && (store[i].bought == 0))
		{
			return (TRUE);
		}
	}
	return (FALSE);
}


/*
 * Successful haggle.
 */
static void say_comment_1(void)
{
	if (!auto_haggle || verbose_haggle)
	msg_print(comment_1[rand_int(MAX_COMMENT_1)]);
    if (randint(RUMOR_CHANCE) == 1 && speak_unique )
	{
		msg_print("The shopkeeper whispers something into your ear:");
        msg_format("%v", get_rnd_line_f1, "rumors.txt");
	}
}


/*
 * Continue haggling (player is buying)
 */
static void say_comment_2(s32b value, int annoyed)
{
	char	tmp_val[80];

	/* Prepare a string to insert */
	sprintf(tmp_val, "%ld", (long)value);

	/* Final offer */
	if (annoyed > 0)
	{
		/* Formatted message */
		msg_format(comment_2a[rand_int(MAX_COMMENT_2A)], tmp_val);
	}

	/* Normal offer */
	else
	{
		/* Formatted message */
		msg_format(comment_2b[rand_int(MAX_COMMENT_2B)], tmp_val);
	}
}


/*
 * Continue haggling (player is selling)
 */
static void say_comment_3(s32b value, int annoyed)
{
	char	tmp_val[80];

	/* Prepare a string to insert */
	sprintf(tmp_val, "%ld", (long)value);

	/* Final offer */
	if (annoyed > 0)
	{
		/* Formatted message */
		msg_format(comment_3a[rand_int(MAX_COMMENT_3A)], tmp_val);
	}

	/* Normal offer */
	else
	{
		/* Formatted message */
		msg_format(comment_3b[rand_int(MAX_COMMENT_3B)], tmp_val);
	}
}


/*
 * Kick 'da bum out.					-RAK-
 */
static void say_comment_4(void)
{
	msg_print(comment_4a[rand_int(MAX_COMMENT_4A)]);
	msg_print(comment_4b[rand_int(MAX_COMMENT_4B)]);
}


/*
 * You are insulting me
 */
static void say_comment_5(void)
{
	msg_print(comment_5[rand_int(MAX_COMMENT_5)]);
}


/*
 * That makes no sense.
 */
static void say_comment_6(void)
{
	msg_print(comment_6[rand_int(5)]);
}



/*
 * Messages for reacting to purchase prices.
 */

#define MAX_COMMENT_7A	4

static cptr comment_7a[MAX_COMMENT_7A] =
{
	"Arrgghh!",
	"You bastard!",
	"You hear someone sobbing...",
	"The shopkeeper howls in agony!"
};

#define MAX_COMMENT_7B	4

static cptr comment_7b[MAX_COMMENT_7B] =
{
	"Damn!",
	"You bastard!",
	"The shopkeeper points at you, then curses.", /* NetHack */
	"The shopkeeper glares at you."
};

#define MAX_COMMENT_7C	4

static cptr comment_7c[MAX_COMMENT_7C] =
{
	"Cool!",
	"You've made my day!",
	"The shopkeeper giggles.",
	"The shopkeeper laughs loudly."
};

#define MAX_COMMENT_7D	4

static cptr comment_7d[MAX_COMMENT_7D] =
{
	"Yipee!",
	"I think I'll retire!",
	"The shopkeeper jumps for joy.",
	"The shopkeeper smiles gleefully."
};

/*
 * Let a shop-keeper React to a purchase
 *
 * We paid "price", it was worth "value", and we thought it was worth "guess"
 */
static void purchase_analyze(s32b price, s32b value, s32b guess)
{
	/* Item was worthless, but we bought it */
	if ((value <= 0) && (price > value))
	{
		/* Comment */
		msg_print(comment_7a[rand_int(MAX_COMMENT_7A)]);

		/* Sound */
		sound(SOUND_STORE1);
	}

	/* Item was cheaper than we thought, and we paid more than necessary */
	else if ((value < guess) && (price > value))
	{
		/* Comment */
		msg_print(comment_7b[rand_int(MAX_COMMENT_7B)]);

		/* Sound */
		sound(SOUND_STORE2);
	}

	/* Item was a good bargain, and we got away with it */
	else if ((value > guess) && (value < (4 * guess)) && (price < value))
	{
		/* Comment */
		msg_print(comment_7c[rand_int(MAX_COMMENT_7C)]);

		/* Sound */
		sound(SOUND_STORE3);
	}

	/* Item was a great bargain, and we got away with it */
	else if ((value > guess) && (price < value))
	{
		/* Comment */
		msg_print(comment_7d[rand_int(MAX_COMMENT_7D)]);

		/* Sound */
		sound(SOUND_STORE4);
	}
}

/*
 * Buying and selling adjustments for race combinations.
 * Entry[owner][player] gives the basic "cost inflation".
 */
static byte rgold_adj[MAX_RACES][MAX_RACES] =
{
    /*Hum, HfE, Elf,  Hal, Gno, Dwa, HfO, HfT, Dun, HiE, Barbarian,
     HfOg, HGn, HTn, Cyc, Yek, Klc, Kbd, Nbl, DkE, Drc, Mind Flayer,
     Imp,  Glm, Skl, Zombie, Vampire, Spectre */

	/* Human */
    { 100, 105, 105, 110, 113, 115, 120, 125, 100, 105, 100,
      124, 120, 110, 125, 115, 120, 120, 120, 120, 115, 120,
      115, 105, 125, 125, 125, 125, 105, 120 },

	/* Half-Elf */
    { 110, 100, 100, 105, 110, 120, 125, 130, 110, 100, 110,
      120, 115, 108, 115, 110, 110, 120, 120, 115, 115, 110,
      120, 110, 110, 110, 120, 110, 100, 125 },

	/* Elf */
    { 110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 110,
      120, 120, 105, 120, 110, 105, 125, 125, 110, 115, 108,
      120, 115, 110, 110, 120, 110, 100, 125},

	/* Halfling */
    { 115, 110, 105,  95, 105, 110, 115, 130, 115, 105, 115,
      125, 120, 120, 125, 115, 110, 120, 120, 120, 115, 115,
      120, 110, 120, 120, 130, 110, 110, 130 },

	/* Gnome */
    { 115, 115, 110, 105,  95, 110, 115, 130, 115, 110, 115,
      120, 125, 110, 120, 110, 105, 120, 110, 110, 105, 110,
      120, 101, 110, 110, 120, 120, 115, 130 },

	/* Dwarf */
    { 115, 120, 120, 110, 110,  95, 125, 135, 115, 120, 115,
      125, 140, 130, 130, 120, 115, 115, 115, 135, 125, 120,
      120, 105, 115, 115, 115, 115, 120, 130 },

	/* Half-Orc */
    { 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
      110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
      115, 125, 120, 120, 115, 120, 125, 115 },

	/* Half-Troll */
    { 110, 115, 115, 110, 110, 130, 110, 110, 110, 115, 110,
      110, 115, 120, 110, 120, 120, 110, 110, 110, 115, 110,
      110, 115, 112, 112, 115, 112, 120, 110 },

    /* Great One Breed (Dunedain)  */
    { 100, 105, 105, 110, 113, 115, 120, 125, 100, 105, 100,
      120, 120, 105, 120, 115, 105, 115, 120, 110, 105, 105,
      120, 105, 120, 120, 125, 120, 105, 135 },

	/* High_Elf */
    { 110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 110,
      125, 125, 101, 120, 115, 110, 115, 125, 110, 110, 110,
      125, 115, 120, 120, 125, 120, 100, 125 },

    /* Human / Barbarian (copied from human) */
    { 100, 105, 105, 110, 113, 115, 120, 125, 100, 105, 100,
      124, 120, 110, 125, 115, 120, 120, 120, 120, 115, 120,
      115, 105, 125, 125, 130, 125, 115, 120 },

    /* Half-Ogre: theoretical, copied from half-troll */
    { 110, 115, 115, 110, 110, 130, 110, 110, 110, 115, 110,
      110, 115, 120, 110, 120, 120, 110, 110, 110, 115, 110,
      110, 115, 112, 112, 115, 112, 120, 110 },

    /* Half-Giant: theoretical, copied from half-troll */
    { 110, 115, 115, 110, 110, 130, 110, 110, 110, 115, 110,
      110, 115, 120, 110, 120, 120, 110, 110, 110, 115, 110,
      110, 115, 112, 112, 115, 112, 130, 120 },

    /* Half-Titan: theoretical, copied from High_Elf */
    { 110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 110,
      125, 125, 101, 120, 115, 110, 115, 125, 110, 110, 110,
      125, 115, 120, 120, 120, 120, 130, 130   },

    /* Cyclops: theoretical, copied from half-troll */
    { 110, 115, 115, 110, 110, 130, 110, 110, 110, 115, 110,
      110, 115, 120, 110, 120, 120, 110, 110, 110, 115, 110,
      110, 115, 112, 112, 115, 112, 130, 130  },

    /* Yeek: theoretical, copied from Half-Orc */
    { 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
      110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
      115, 125, 120, 120, 120, 120, 130, 130  },

    /* Klackon: theoretical, copied from Gnome */
    { 115, 115, 110, 105,  95, 110, 115, 130, 115, 110, 115,
      120, 125, 110, 120, 110, 105, 120, 110, 110, 105, 110,
      120, 101, 110, 110, 120, 120, 130, 130  },

    /* Kobold: theoretical, copied from Half-Orc */
    { 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
      110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
      115, 125, 120, 120, 120, 120, 130, 130  },

    /* Nibelung: theoretical, copied from Dwarf */
    { 115, 120, 120, 110, 110,  95, 125, 135, 115, 120, 115,
      125, 140, 130, 130, 120, 115, 115, 115, 135, 125, 120,
      120, 105, 115, 115, 120, 120, 130, 130   },

    /* Dark Elf */
    { 110, 110, 110, 115, 120, 130, 115, 115, 120, 110, 115,
      115, 115, 116, 115, 120, 120, 115, 115, 101, 110, 110,
      110, 110, 112, 122, 110, 110, 110, 115  },

    /* Draconian: theoretical, copied from High_Elf */
    { 110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 110,
      125, 125, 101, 120, 115, 110, 115, 125, 110, 110, 110,
      125, 115, 120, 120, 120, 120, 130, 130  },

    /* Mind Flayer: theoretical, copied from High_Elf */
    { 110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 110,
      125, 125, 101, 120, 115, 110, 115, 125, 110, 110, 110,
      125, 115, 120, 120, 120, 120, 130, 130   },

    /* Imp: theoretical, copied from High_Elf */
    { 110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 110,
      125, 125, 101, 120, 115, 110, 115, 125, 110, 110, 110,
      125, 115, 120, 120, 120, 120, 130, 130   },

    /* Golem: theoretical, copied from High_Elf */
    { 110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 110,
      125, 125, 101, 120, 115, 110, 115, 125, 110, 110, 110,
      125, 115, 120, 120, 120, 120, 130, 130 },

    /* Skeleton: theoretical, copied from half-orc */
    { 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
      110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
      115, 125, 120, 120, 120, 120, 130, 130   },

    /* Zombie: Theoretical, copied from half-orc */
    { 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
      110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
      115, 125, 120, 120, 120, 120, 130, 130   },

    /* Vampire: Theoretical, copied from half-orc */
    { 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
      110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
      115, 125, 120, 120, 120, 120, 130, 130   },

    /* Spectre: Theoretical, copied from half-orc */
    { 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
      110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
      115, 125, 120, 120, 120, 120, 130, 130   },

    /* Sprite: Theoretical, copied from half-orc */
    { 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
      110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
      115, 125, 120, 120, 120, 120, 130, 130   },

    /* Broo: Theoretical, copied from half-orc */
    { 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
      110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
      115, 125, 120, 120, 120, 120, 130, 130   },

};




/*
 * Determine the price of an item (qty one) in a store.
 *
 * This function takes into account the player's charisma, and the
 * shop-keepers friendliness, and the shop-keeper's base greed, but
 * never lets a shop-keeper lose money in a transaction.
 *
 * The "greed" value should exceed 100 when the player is "buying" the
 * item, and should be less than 100 when the player is "selling" it.
 *
 * Hack -- the black market always charges twice as much as it should.
 * Hack -- the pawnbroker always charges 33% of what it should.
 *
 * Charisma adjustment runs from 80 to 130
 * Racial adjustment runs from 95 to 130
 *
 * Since greed/charisma/racial adjustments are centered at 100, we need
 * to adjust (by 200) to extract a usable multiplier.  Note that the
 * "greed" value is always something (?).
 */
static s32b price_item(object_type *o_ptr, int greed, bool flip)
{
	int     factor;
	int     adjust;
	s32b    price;


	/* Get the value of one of the items */
	price = object_value(o_ptr);

	/* Worthless items */
	if (price <= 0) return (0L);


	/* Compute the racial factor */
	factor = rgold_adj[ot_ptr->owner_race][p_ptr->prace];

	/* Add in the charisma factor */
	factor += adj_chr_gold[p_ptr->stat_ind[A_CHR]];


	/* Shop is buying */
	if (flip)
	{
		/* Adjust for greed */
		adjust = 100 + (300 - (greed + factor));

		/* Never get "silly" */
		if (adjust > 100) adjust = 100;

		/* Mega-Hack -- Black market sucks */
		if (cur_store_type == STORE_BLACK) price = price / 2;
		if (cur_store_type == STORE_PAWN) price = price / 3; 
	}

	/* Shop is selling */
	else
	{
		/* Adjust for greed */
		adjust = 100 + ((greed + factor) - 300);

		/* Never get "silly" */
		if (adjust < 100) adjust = 100;

		/* Mega-Hack -- Black market sucks */
		if (cur_store_type == STORE_BLACK) price = price * 2;
		if (cur_store_type == STORE_PAWN) price = price / 3;
	}

	/* Compute the final price (with rounding) */
	price = (price * adjust + 50L) / 100L;

	/* Note -- Never become "free" */
	if (price <= 0L) return (1L);

	/* Return the price */
	return (price);
}


/*
 * Special "mass production" computation
 */
static int mass_roll(int num, int max)
{
	int i, t = 0;
	for (i = 0; i < num; i++) t += rand_int(max);
	return (t);
}


/*
 * Certain "cheap" objects should be created in "piles"
 * Some objects can be sold at a "discount" (in small piles)
 */
static void mass_produce(object_type *o_ptr)
{
	int size = 1;
	int discount = 0;

	s32b cost = object_value(o_ptr);


	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Food, Flasks, and Lites */
		case TV_FOOD:
		case TV_FLASK:
		case TV_LITE:
		{
			if (cost <= 5L) size += mass_roll(3, 5);
			if (cost <= 20L) size += mass_roll(3, 5);
			break;
		}

		case TV_POTION:
		case TV_SCROLL:
		{
			if (cost <= 60L) size += mass_roll(3, 5);
			if (cost <= 240L) size += mass_roll(1, 5);
			break;
		}

        case TV_SORCERY_BOOK:
        case TV_THAUMATURGY_BOOK:
        case TV_CONJURATION_BOOK:
        case TV_NECROMANCY_BOOK:
		{
			if (cost <= 50L) size += mass_roll(2, 3);
			if (cost <= 500L) size += mass_roll(1, 3);
			break;
		}

		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SHIELD:
		case TV_GLOVES:
		case TV_BOOTS:
		case TV_CLOAK:
		case TV_HELM:
		case TV_CROWN:
		case TV_SWORD:
		case TV_POLEARM:
		case TV_HAFTED:
		case TV_DIGGING:
		case TV_BOW:
		{
			if (o_ptr->name2) break;
			if (cost <= 10L) size += mass_roll(3, 5);
			if (cost <= 100L) size += mass_roll(3, 5);
			break;
		}

		case TV_SPIKE:
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			if (cost <= 5L) size += mass_roll(5, 5);
			if (cost <= 50L) size += mass_roll(5, 5);
			if (cost <= 500L) size += mass_roll(5, 5);
			break;
		}
	}


	/* Pick a discount */
	if (cost < 5)
	{
		discount = 0;
	}
	else if (rand_int(25) == 0)
	{
		discount = 25;
	}
	else if (rand_int(150) == 0)
	{
		discount = 50;
	}
	else if (rand_int(300) == 0)
	{
		discount = 75;
	}
	else if (rand_int(500) == 0)
	{
		discount = 90;
	}


    if (o_ptr->art_name)
    {
        if (cheat_peek && discount)
        {
            msg_print("No discount on random artifacts.");
        }
        discount = 0;
    }

	/* Save the discount */
	o_ptr->discount = discount;

	/* Save the total pile size */
	o_ptr->number = size - (size * discount / 100);
}








/*
 * Determine if a store item can "absorb" another item
 *
 * See "object_similar()" for the same function for the "player"
 */
static bool store_object_similar(object_type *o_ptr, object_type *j_ptr)
{
	/* Hack -- Identical items cannot be stacked */
	if (o_ptr == j_ptr) return (0);

	/* Different objects cannot be stacked */
	if (o_ptr->k_idx != j_ptr->k_idx) return (0);

	/* Different charges (etc) cannot be stacked */
	if (o_ptr->pval != j_ptr->pval) return (0);

	/* Require many identical values */
	if (o_ptr->to_h  !=  j_ptr->to_h) return (0);
	if (o_ptr->to_d  !=  j_ptr->to_d) return (0);
	if (o_ptr->to_a  !=  j_ptr->to_a) return (0);

	/* Require identical "artifact" names */
	if (o_ptr->name1 != j_ptr->name1) return (0);

	/* Require identical "ego-item" names */
	if (o_ptr->name2 != j_ptr->name2) return (0);

    /* Random artifacts don't stack !*/
    if (o_ptr->art_name || j_ptr->art_name) return (0);

    /* Hack -- Identical flags! */
    if ((o_ptr->flags1 != j_ptr->flags1) ||
        (o_ptr->flags2 != j_ptr->flags2) ||
        (o_ptr->flags3 != j_ptr->flags3))
            return (0);

	/* Hack -- Never stack "powerful" items */
	if (o_ptr->xtra1 || j_ptr->xtra1) return (0);

	/* Hack -- Never stack recharging items */
	if (o_ptr->timeout || j_ptr->timeout) return (0);

	/* Require many identical values */
	if (o_ptr->ac    !=  j_ptr->ac)   return (0);
	if (o_ptr->dd    !=  j_ptr->dd)   return (0);
	if (o_ptr->ds    !=  j_ptr->ds)   return (0);

	/* Hack -- Never stack chests */
	if (o_ptr->tval == TV_CHEST) return (0);

	/* Require matching discounts */
	if (o_ptr->discount != j_ptr->discount) return (0);

	/* Require matching identification states */
	if (object_known_p(o_ptr) != object_known_p(j_ptr)) return (0);
	
	/* They match, so they must be similar */
	return (TRUE);
}


/*
 * Allow a store item to absorb another item
 * Returns true if the entire stack was absorbed.
 */
bool store_object_absorb(object_type *j_ptr, object_type *o_ptr)
{
	int total = o_ptr->number + j_ptr->number;

	if (total <= MAX_STACK_SIZE-1)
	{
		j_ptr->number = total;
		return TRUE;
	}
	else
	{
		j_ptr->number = MAX_STACK_SIZE-1;
		o_ptr->number = total-MAX_STACK_SIZE+1;
		return FALSE;
	}
}


/*
 * Check to see if the shop will be carrying too many objects	-RAK-
 * Note that the shop, just like a player, will not accept things
 * it cannot hold.  Before, one could "nuke" potions this way.
 *
 * Returns the number of items that can be absorbed from o_ptr.
 */
static int store_check_num(object_type *o_ptr)
{
	int        i;
	object_type *j_ptr;

	/* Free space is always usable */
	if (st_ptr->stock_num < st_ptr->stock_size) return MAX_STACK_SIZE-1;

	/* The "home" acts like the player */
	if (cur_store_type == STORE_HOME)
	{
		/* Check all the items */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			/* Get the existing item */
			j_ptr = &st_ptr->stock[i];

			/* Can the new object be combined with the old one? */
			if (object_similar(j_ptr, o_ptr)) return (MAX_STACK_SIZE-1-j_ptr->number);
		}
	}

	/* Normal stores do special stuff */
	else
	{
		/* Check all the items */
		for (i = st_ptr->stock_num; i; i--)
		{
			/* Get the existing item */
			j_ptr = &st_ptr->stock[i-1];

			/* How many obects can be added to the inventory? The only similar
			* pile which can accept this object is the last one (the others
			* having 99 objects each), so only one similar is checked.
			* This may be 0. */
			if (store_object_similar(j_ptr, o_ptr)) return (MAX_STACK_SIZE-1-j_ptr->number);
		}
	}

	/* But there was no room at the inn... */
	return (0);
}


static bool is_blessed(object_type *o_ptr)
{
    u32b f1, f2, f3;
    object_flags(o_ptr, &f1, &f2, &f3);
    if (f3 & TR3_BLESSED) return (TRUE);
    else return (FALSE);
}



/*
 * Determine if the current store will purchase the given item
 *
 * Note that a shop-keeper must refuse to buy "worthless" items
 */
static bool store_will_buy(object_type *o_ptr)
{
	/* Hack -- The Home is simple */
	if (cur_store_type == STORE_HOME) return (TRUE);
	if (cur_store_type == 99) return (FALSE);

	/* Switch on the store */
	switch (cur_store_type)
	{
		/* General Store */
		case 0:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_FOOD:
				case TV_LITE:
				case TV_FLASK:
				case TV_SPIKE:
				case TV_SHOT:
				case TV_ARROW:
				case TV_BOLT:
				case TV_DIGGING:
				case TV_CLOAK:
                case TV_BOTTLE: /* 'Green', recycling Angband */
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Armoury */
		case 1:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_BOOTS:
				case TV_GLOVES:
				case TV_CROWN:
				case TV_HELM:
				case TV_SHIELD:
				case TV_CLOAK:
				case TV_SOFT_ARMOR:
				case TV_HARD_ARMOR:
				case TV_DRAG_ARMOR:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Weapon Shop */
		case 2:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_SHOT:
				case TV_BOLT:
				case TV_ARROW:
				case TV_BOW:
				case TV_DIGGING:
				case TV_HAFTED:
				case TV_POLEARM:
				case TV_SWORD:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Temple */
		case 3:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_SCROLL:
				case TV_POTION:
				case TV_HAFTED:
				break;
				case TV_POLEARM:
				case TV_SWORD:
                if (is_blessed(o_ptr))
                    break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Alchemist */
		case 4:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_SCROLL:
				case TV_POTION:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Magic Shop */
		case 5:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_CHARM:
                case TV_SORCERY_BOOK:
                case TV_THAUMATURGY_BOOK:
                case TV_CONJURATION_BOOK:
                case TV_NECROMANCY_BOOK:
				case TV_AMULET:
				case TV_RING:
				case TV_STAFF:
				case TV_WAND:
				case TV_ROD:
				case TV_SCROLL:
				case TV_POTION:
				break;
				default:
				return (FALSE);
			}
			break;
		}
		/* Bookstore */
		case 8:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
		                case TV_SORCERY_BOOK:
		                case TV_THAUMATURGY_BOOK:
		                case TV_CONJURATION_BOOK:
                		case TV_NECROMANCY_BOOK:
					break;
				default:
					return (FALSE);
			}
			break;
		}
		/* Inn */
		case 9:
		/* The Inn will not buy anything */
		{
			return (FALSE);
		}
		/* Hall of Records */
		case 10:
		{
			/* Hall does not buy */
			return (FALSE);
		}
		/* Pawnbrokers */
		case 11:
		{
			/* Will buy anything */
			return (TRUE);
		}
	}

	/* XXX XXX XXX Ignore "worthless" items */
	if (object_value(o_ptr) <= 0) return (FALSE);

	/* Okay if there's room */
	return (store_check_num(o_ptr));
}



/*
 * Add the item "o_ptr" to the inventory of the "Home"
 *
 * In all cases, return the slot (or -1) where the object was placed
 *
 * Note that this is a hacked up version of "inven_carry()".
 *
 * Also note that it may not correctly "adapt" to "knowledge" bacoming
 * known, the player may have to pick stuff up and drop it again.
 */
static int home_carry(object_type *o_ptr)
{
	int                 slot;
	s32b               value, j_value;
	int		i;
	object_type *j_ptr;


	/* Check each existing item (try to combine) */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get the existing item */
		j_ptr = &st_ptr->stock[slot];

		/* The home acts just like the player */
		if (object_similar_2(j_ptr, o_ptr) &&
			/* Save the new number of items */
			object_absorb(j_ptr, o_ptr))

			/* All done */
			return (slot);
		}

	/* No space? */
	if (st_ptr->stock_num >= st_ptr->stock_size) return (-1);


	/* Determine the "value" of the item */
	value = object_value(o_ptr);

	/* Check existing slots to see if we must "slide" */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get that item */
		j_ptr = &st_ptr->stock[slot];

		/* Objects sort by decreasing type */
		if (o_ptr->tval > j_ptr->tval) break;
		if (o_ptr->tval < j_ptr->tval) continue;

		/* Can happen in the home */
		if (!object_aware_p(o_ptr)) continue;
		if (!object_aware_p(j_ptr)) break;

		/* Objects sort by increasing k_idx */
		if (o_ptr->k_idx < j_ptr->k_idx) break;
		if (o_ptr->k_idx > j_ptr->k_idx) continue;

		/* Objects in the home can be unknown */
		if (!object_known_p(o_ptr)) continue;
		if (!object_known_p(j_ptr)) break;

  
       /* Hack:  otherwise identical rods sort by
          increasing recharge time --dsb */
       if (o_ptr->tval == TV_ROD) {
           if (o_ptr->timeout < j_ptr->timeout) break;
           if (o_ptr->timeout > j_ptr->timeout) continue;
       }

		/* Objects sort by decreasing value */
		j_value = object_value(j_ptr);
		if (value > j_value) break;
		if (value < j_value) continue;
	}

	/* Slide the others up */
	for (i = st_ptr->stock_num; i > slot; i--)
	{
		st_ptr->stock[i] = st_ptr->stock[i-1];
	}

	/* More stuff now */
	st_ptr->stock_num++;

	/* Insert the new item */
	st_ptr->stock[slot] = *o_ptr;

	/* Return the location */
	return (slot);
}


/*
 * Add the item "o_ptr" to a real stores inventory.
 *
 * If the item is "worthless", it is thrown away (except in the home).
 *
 * If the item cannot be combined with an object already in the inventory,
 * make a new slot for it, and calculate its "per item" price.  Note that
 * this price will be negative, since the price will not be "fixed" yet.
 * Adding an item to a "fixed" price stack will not change the fixed price.
 *
 * In all cases, return the slot (or -1) where the object was placed
 */
static int store_carry(object_type *o_ptr)
{
	int		i, slot;
	s32b	value, j_value;
	object_type	*j_ptr;


	/* Evaluate the object */
	value = object_value(o_ptr);

	/* Cursed/Worthless items "disappear" when sold */
	if (value <= 0) return (-1);

	/* All store items are fully *identified* */
	if(cur_store_type != STORE_PAWN)
	{
		o_ptr->ident |= (IDENT_MENTAL | IDENT_SENSE);
	}

	/* Note that item is in a store */
	o_ptr->ident |= (IDENT_STORE);

	/* Check each existing item (try to combine) */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get the existing item */
		j_ptr = &st_ptr->stock[slot];

		/* Can the existing items be incremented? */
		if (store_object_similar(j_ptr, o_ptr))
		{
			/* Return the last slot objects were left in. */
			if (store_object_absorb(j_ptr, o_ptr)) return (slot);
		}
	}

	/* No space? */
	if (st_ptr->stock_num >= st_ptr->stock_size) return (-1);


	/* Check existing slots to see if we must "slide" */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get that item */
		j_ptr = &st_ptr->stock[slot];

		/* Objects sort by decreasing type */
		if (o_ptr->tval > j_ptr->tval) break;
		if (o_ptr->tval < j_ptr->tval) continue;

		/* Objects sort by increasing k_idx */
		if (o_ptr->k_idx < j_ptr->k_idx) break;
		if (o_ptr->k_idx > j_ptr->k_idx) continue;


       /* Hack:  otherwise identical rods sort by
          increasing recharge time --dsb */
       if (o_ptr->tval == TV_ROD) {
           if (o_ptr->timeout < j_ptr->timeout) break;
           if (o_ptr->timeout > j_ptr->timeout) continue;
       }
  
        /* Evaluate that slot */
		j_value = object_value(j_ptr);

		/* Objects sort by decreasing value */
		if (value > j_value) break;
		if (value < j_value) continue;
	}

	/* Slide the others up */
	for (i = st_ptr->stock_num; i > slot; i--)
	{
		st_ptr->stock[i] = st_ptr->stock[i-1];
	}

	/* More stuff now */
	st_ptr->stock_num++;

	/* Insert the new item */
	st_ptr->stock[slot] = *o_ptr;

	/* Return the location */
	return (slot);
}


/*
 * Increase, by a given amount, the number of a certain item
 * in a certain store.  This can result in zero items.
 */
static void store_item_increase(int item, int num)
{
	int         cnt;
	object_type *o_ptr;

	/* Get the item */
	o_ptr = &st_ptr->stock[item];

	/* Verify the number */
	cnt = o_ptr->number + num;
	if (cnt > 255) cnt = 255;
	else if (cnt < 0) cnt = 0;
	num = cnt - o_ptr->number;

	/* Save the new number */
	o_ptr->number += num;
}


static void store_item_optimize(int item);

/*
 * Try to fill a stack from later stacks.
 */
static void store_item_combine(int item)
{
	int         j;

	/* Get the item */
	object_type *o_ptr = &st_ptr->stock[item];

	/* Must exist */
	if (!o_ptr->k_idx) return;

	/* Find the next dissimilar stack. */
	for (j = item+1; j < st_ptr->stock_num; j++)
	{
		object_type *j_ptr = &st_ptr->stock[j];
		if (!store_object_similar(j_ptr, o_ptr)) break;

		/* A non-full stack only ever precedes a dissimilar one. */
		if (j_ptr->number != MAX_STACK_SIZE-1)
		{
			j++;
			break;
		}
	}

	/* Decrement j to find the last similar one. */
	j--;

	/* No similar items. */
	if (j == item) return;
	
	/* More than MAX_STACK_SIZE-1 in these two stacks. */
	if (!store_object_absorb(o_ptr, &st_ptr->stock[j])) return;

	/* Move the part-filled stack to the end. */
	st_ptr->stock[j].number = o_ptr->number;
	o_ptr->number = 0;

	/* Remove this (now empty) stack. */
	store_item_optimize(item);
}
	

/*
 * Remove a slot if it is empty
 */
static void store_item_optimize(int item)
{
	int         j;
	object_type *o_ptr;

	/* Get the item */
	o_ptr = &st_ptr->stock[item];

	if (o_ptr->number)
	{
		store_item_combine(item);
		return;
	}

	/* One less item */
	st_ptr->stock_num--;

	/* Slide everyone */
	for (j = item; j < st_ptr->stock_num; j++)
	{
		st_ptr->stock[j] = st_ptr->stock[j + 1];
	}

	/* Nuke the final slot */
	object_wipe(&st_ptr->stock[j]);
}


/*
 * This function will keep 'crap' out of the black market.
 * Crap is defined as any item that is "available" elsewhere
 * Based on a suggestion by "Lee Vogt" <lvogt@cig.mcel.mot.com>
 */
static bool black_market_crap(object_type *o_ptr)
{
	int		i, j;

	/* Ego items are never crap */
	if (o_ptr->name2) return (FALSE);

	/* Good items are never crap */
	if (o_ptr->to_a > 0) return (FALSE);
	if (o_ptr->to_h > 0) return (FALSE);
	if (o_ptr->to_d > 0) return (FALSE);

	/* Check the other "normal" stores */
	for (i = 0; i < 6; i++)
	{
		/* Check every item in the store */
		for (j = 0; j < store[i].stock_num; j++)
		{
			object_type *j_ptr = &store[i].stock[j];

			/* Duplicate item "type", assume crappy */
			if (o_ptr->k_idx == j_ptr->k_idx) return (TRUE);
		}
	}

	/* Assume okay */
	return (FALSE);
}


/*
 * Attempt to delete (some of) a random item from the store
 * Hack -- we attempt to "maintain" piles of items when possible.
 */
static void store_delete(void)
{
	int what, num;

	/* Pick a random slot */
	what = rand_int(st_ptr->stock_num);

	/* Determine how many items are here */
	num = st_ptr->stock[what].number;

	/* Hack -- sometimes, only destroy half the items */
	if (rand_int(100) < 50) num = (num + 1) / 2;

	/* Hack -- sometimes, only destroy a single item */
	if (rand_int(100) < 50) num = 1;

	/* Actually destroy (part of) the item */
	store_item_increase(what, -num);
	store_item_optimize(what);
}


/*
 * Creates a random item and gives it to a store
 * This algorithm needs to be rethought.  A lot.
 * Currently, "normal" stores use a pre-built array.
 *
 * Note -- the "level" given to "obj_get_num()" is a "favored"
 * level, that is, there is a much higher chance of getting
 * items with a level approaching that of the given level...
 *
 * Should we check for "permission" to have the given item?
 */
static void store_create(void)
{
	int i, tries, level;

	object_type forge;
	object_type *q_ptr;


	/* Paranoia -- no room left */
	if (st_ptr->stock_num >= st_ptr->stock_size) return;


	/* Hack -- consider up to four items */
	for (tries = 0; tries < 4; tries++)
	{
		/* Black Market */
		if (cur_store_type == STORE_BLACK)
		{
			/* Pick a level for object/magic */
			level = 25 + rand_int(25);

			/* Random item (usually of given level) */
			i = get_obj_num(level);

			/* Handle failure */
			if (!i) continue;
		}

		/* Normal Store */
		else
		{
			/* Hack -- Pick an item to sell */
			i = store_table[st_ptr->type][rand_int(STORE_CHOICES)];

			/* Hack -- fake level for apply_magic() */
			level = rand_range(1, STORE_OBJ_LEVEL);
		}


		/* Get local object */
		q_ptr = &forge;

		/* Create a new object of the chosen kind */
		object_prep(q_ptr, i);

		/* Apply some "low-level" magic (no artifacts) */
		apply_magic(q_ptr, level, FALSE, FALSE, FALSE);

		/* Store objects never get inscriptions. */
		q_ptr->note = 0;

		/* Hack -- Charge lite's */
		if (q_ptr->tval == TV_LITE)
		{
			if (q_ptr->k_idx == OBJ_WOODEN_TORCH) q_ptr->pval = FUEL_TORCH / 2;
			if (q_ptr->k_idx == OBJ_BRASS_LANTERN) q_ptr->pval = FUEL_LAMP / 2;
		}


		/* The item is "known" */
		object_known(q_ptr);

		/* Mark it storebought */
		q_ptr->ident |= IDENT_STOREB;

		/* Mega-Hack -- no chests in stores */
		if (q_ptr->tval == TV_CHEST) continue;

		/* Prune the black market */
		if (cur_store_type == STORE_BLACK)
		{
			/* Hack -- No "crappy" items */
			if (black_market_crap(q_ptr)) continue;

			/* Hack -- No "cheap" items */
			if (object_value(q_ptr) < 10) continue;

			/* No "worthless" items */
			/* if (object_value(q_ptr) <= 0) continue; */
		}

		/* Prune normal stores */
		else
		{
			/* No "worthless" items */
			if (object_value(q_ptr) <= 0) continue;
		}


		/* Mass produce and/or Apply discount */
		mass_produce(q_ptr);

		/* Attempt to carry the (known) item */
		i = store_carry(q_ptr);

#ifdef MAINT_99_MAX
		/* Prune to 99 if necessary. */
		if (i > 0 && store_object_similar(st_ptr->stock+i, st_ptr->stock+i-1))
		{
			store_item_increase(i, -st_ptr->stock[i].number);
			store_item_optimize(i);
		}
#endif /* MAINT_99_MAX */

		/* Definitely done */
		break;
	}
}



/*
 * Eliminate need to bargain if player has haggled well in the past
 */
static bool noneedtobargain(s32b minprice)
{
	s32b good = st_ptr->good_buy;
	s32b bad = st_ptr->bad_buy;

	/* Cheap items are "boring" */
	if (minprice < 10L) return (TRUE);

	/* Perfect haggling */
	if (good == MAX_SHORT) return (TRUE);

	/* Reward good haggles, punish bad haggles, notice price */
	if (good > ((3 * bad) + (5 + (minprice/50)))) return (TRUE);

	/* Return the flag */
	return (FALSE);
}


/*
 * Update the bargain info
 */
static void updatebargain(s32b price, s32b minprice)
{
	/* Hack -- auto-haggle */
	if (auto_haggle) return;

	/* Cheap items are "boring" */
	if (minprice < 10L) return;

	/* Count the successful haggles */
	if (price == minprice)
	{
		/* Just count the good haggles */
		if (st_ptr->good_buy < MAX_SHORT)
		{
			st_ptr->good_buy++;
		}
	}

	/* Count the failed haggles */
	else
	{
		/* Just count the bad haggles */
		if (st_ptr->bad_buy < MAX_SHORT)
		{
			st_ptr->bad_buy++;
		}
	}
}



/*
 * Re-displays a single store entry
 */
static void display_entry(int pos)
{
	int			i;
	object_type		*o_ptr;
	s32b		x;

	C_TNEW(o_name, ONAME_MAX, char);
	char		out_val[160];


	int maxwid = 75;

	/* Get the item */
	o_ptr = &st_ptr->stock[pos];

	/* Get the "offset" */
	i = pos - store_top;

	/* Label it, clear the line --(-- */
	(void)sprintf(out_val, "%c) ", I2A(i));
	c_prt((object_aware_p(o_ptr)) ? TERM_WHITE : TERM_SLATE, out_val, i+6, 0);


	/* Describe an item in the home */
	if (cur_store_type == STORE_HOME)
	{
		maxwid = 75;

		/* Leave room for weights, if necessary -DRS- */
		if (show_weights) maxwid -= 10;

		/* Describe the object */
		strnfmt(o_name, ONAME_MAX, "%v", object_desc_f3, o_ptr, TRUE, 3);
		o_name[maxwid] = '\0';
		c_put_str(tval_to_attr[o_ptr->tval], o_name, i+6, 3);

		/* Show weights */
		if (show_weights)
		{
			/* Only show the weight of an individual item */
			int wgt = o_ptr->weight;
			(void)sprintf(out_val, "%3d.%d lb", wgt / 10, wgt % 10);
			put_str(out_val, i+6, 68);
		}
	}

	/* Describe an item (fully) in a store */
	else
	{
		/* Must leave room for the "price" */
		maxwid = 65;

		/* Leave room for weights, if necessary -DRS- */
		if (show_weights) maxwid -= 7;

		/* Describe the object (fully) */
		if (cur_store_type == STORE_PAWN)
		{
			strnfmt(o_name, ONAME_MAX, "%v", object_desc_f3, o_ptr, TRUE, 3);
		}
		else
		{
			strnfmt(o_name, ONAME_MAX, "%v", object_desc_store_f3, o_ptr, TRUE, 3);
		}


		o_name[maxwid] = '\0';
		c_put_str(tval_to_attr[o_ptr->tval], o_name, i+6, 3);
		TFREE(o_name);

		/* Show weights */
		if (show_weights)
		{
			/* Only show the weight of an individual item */
			int wgt = o_ptr->weight;
			(void)sprintf(out_val, "%3d.%d", wgt / 10, wgt % 10);
			put_str(out_val, i+6, 61);
		}

		/* Display a "fixed" cost */
		if (o_ptr->ident & (IDENT_FIXED))
		{
			/* Extract the "minimum" price */
			x = price_item(o_ptr, ot_ptr->min_inflate, FALSE);
		}
		else if (auto_haggle)
		{
			/* Extract the "minimum" price */
			x = price_item(o_ptr, ot_ptr->min_inflate, FALSE);

			/* Hack -- Apply Sales Tax if needed */
			if (!noneedtobargain(x)) x += x / 10;
		}
		/* Display a "haggle" cost */
		else
		{
			/* Extrect the "maximum" price */
			x = price_item(o_ptr, ot_ptr->max_inflate, FALSE);
		}

		/* Make a string from the price. */
			(void)sprintf(out_val, "%9ld  ", (long)x);

		/* IDENT_FIXED items have a 'F' at the end of their names
		 * instead of a space. */
		if (o_ptr->ident & (IDENT_FIXED))
		{
			strchr(out_val, '\0')[-1] = 'F';
		}

		/* Choose a colour to make unaffordable items distinctive. */
		if (x > p_ptr->au) x = TERM_SLATE;
		else x = TERM_WHITE;

		/* Actually draw the price */
		c_put_str(x, out_val, i+6, 68);
	}
}


/*
 * Displays a store's inventory			-RAK-
 * All prices are listed as "per individual object".  -BEN-
 */
static void display_inventory(void)
{
	int i, k;
	const int items = term_screen->hgt-12;

	/* Display the next 12 items */
	for (k = 0; k < items; k++)
	{
		/* Do not display "dead" items */
		if (store_top + k >= st_ptr->stock_num) break;

		/* Display that line */
		display_entry(store_top + k);
	}

	/* Erase the extra lines and the "more" prompt */
	for (i = k; i < items+1; i++) prt("", i + 6, 0);

	/* Assume "no current page" */
	put_str("        ", 5, 20);

	/* Visual reminder of "more items" */
	if (st_ptr->stock_num > items || store_top)
	{
		/* Show "more" reminder (after the last item) */
		prt("-more-", k + 6, 3);

		/* Indicate the "current page" */
		put_str(format("(Page %d)", (store_top+items-1)/items + 1), 5, 20);
	}
}


/*
 * Displays players gold					-RAK-
 */
static void store_prt_gold(void)
{
	char out_val[64];

	const int y = Term->hgt-5;

	prt("Gold Remaining: ", y, 53);

	sprintf(out_val, "%9ld", (long)p_ptr->au);
	prt(out_val, y, 68);
}


/*
 * Work out the title of the current shop.
 */
static cptr store_title_aux(void)
{
	cptr out;

	switch (cur_store_type)
	{
		/* The "Home" is special */
		case STORE_HOME:
		{
			/* Already yours. */
			if (st_ptr->bought)
			{
				out = format("%28s%s", "", "Your Home");
			}
			/* Name and price. */
			else
			{
				/* The price of a house is an aspect of the town, not the house itself. */
				int town = cur_store_num/MAX_STORES_PER_TOWN;
				u32b price = town_defs[town].house_price;

				/* Hack - Must be the same as needtohaggle(). */
				if (auto_haggle) price = price*11/10;

				out = format("%28s%s (%ld)", "", "House for Sale", price);
			}
			break;
		}
		/* So is the Hall */
		case STORE_HALL:		
		{
			out = format("%28s%s", "", "Hall of Records");
			break;
	}
	/* Normal stores */
		default:
	{
			cptr tmp_str;
			cptr store_name = (f_name + f_info[FEAT_SHOP_HEAD + st_ptr->type].name);
		cptr owner_name = (s_name+ot_ptr->name);
		cptr race_name = race_info[ot_ptr->owner_race].title;
		object_type tmp;
			s16b old_charisma;

		/* Put the owner name and race */
			tmp_str = string_make(format("%s (%s)", owner_name, race_name));

		/* Hack - Create a 10000gp item for price_item() */
		object_prep(&tmp, OBJ_NO_TEA);
		object_aware(&tmp);

			/* Hack - standardise the player's charisma. */
			old_charisma = p_ptr->stat_ind[A_CHR];
			p_ptr->stat_ind[A_CHR] = CHR_PRICE_COMPARE;

		/* Show the max price in the store (above prices) */
			out = format("%10s%-40s%s (%ld) [%ld]", "", tmp_str, store_name, (long)(ot_ptr->max_cost),
			price_item(&tmp, ot_ptr->min_inflate, TRUE));

			/* Hack - return real charisma. */
			p_ptr->stat_ind[A_CHR] = old_charisma;

			string_free(tmp_str);
		}
	}

	/* Done. */	
	return out;
}

/*
 * A wrapper around store_title_aux to remove the assumption that the static
 * variables are correct.
 */
cptr store_title(int store_num)
{
	cptr out;

	/* Hack - price_item() requires correct cur_store_type and ot_ptr as well
	 * as correct parameters. */
	int real_cur_store_num = cur_store_num;
	store_type *real_st_ptr = st_ptr;
	owner_type *real_ot_ptr = ot_ptr;
	byte real_cur_store_type = cur_store_type;
	
	cur_store_num = store_num;
	st_ptr = &store[cur_store_num];
	ot_ptr = &owners[st_ptr->owner];
	cur_store_type = st_ptr->type;

	/* Determine the title. */
	out = store_title_aux();

	/* Hack - return static variables to original values. */
	store_num = real_cur_store_num;
	st_ptr = real_st_ptr;
	ot_ptr = real_ot_ptr;
	cur_store_type = real_cur_store_type;

	return out;
}


/*
 * Displays store (after clearing screen)		-RAK-
 */
static void display_store(void)
{
	/* Clear screen */
	clear_from(1);

	/* Display the title. */
	put_str(store_title_aux(), 3, 0);

	/* The "Home" is special */
	if (cur_store_type == STORE_HOME)
	{
		/* Label the item descriptions */
		put_str("Item Description", 5, 3);

		/* If showing weights, show label */
		if (show_weights)
		{
			put_str("Weight", 5, 70);
		}
	}
	
	/* Normal stores */
	if ((cur_store_type != STORE_HOME) && (cur_store_type != STORE_HALL))
	{
		/* Label the item descriptions */
		put_str("Item Description", 5, 3);
		/* If showing weights, show label */
		if (show_weights)
		{
			put_str("Weight", 5, 60);
		}

		/* Label the asking price (in stores) */
		put_str("Price", 5, 72);
	}

	/* Display the current gold */
	store_prt_gold();
}



/*
 * Get the ID of a store item and return its value	-RAK-
 */
static int get_stock_aux(int *com_val, cptr pmt, int i, int j)
{
	char	command;

	char	out_val[160];

 #ifdef ALLOW_REPEAT
     
     /* Get the item index */
     if (repeat_pull(com_val)) {
                 
         /* Verify the item */
         if ((*com_val >= i) && (*com_val <= j)) {
 	        
 	        /* Success */
 	        return (TRUE);
         }
     }
 
 #endif /* ALLOW_REPEAT -- TNB */

	/* Paranoia XXX XXX XXX */
	msg_print(NULL);


	/* Assume failure */
	*com_val = (-1);

	/* Build the prompt */
	(void)sprintf(out_val, "(Items %c-%c, ESC to exit) %s",
	              I2A(i), I2A(j), pmt);

	/* Ask until done */
	while (TRUE)
	{
		int k;

		/* Escape */
		if (!get_com(out_val, &command)) break;

		/* Convert */
		k = (islower(command) ? A2I(command) : -1);

		/* Legal responses */
		if ((k >= i) && (k <= j))
		{
			*com_val = k;
			break;
		}

		/* Oops */
		bell();
	}

	/* Clear the prompt */
	prt("", 0, 0);

	/* Cancel */
	if (command == ESCAPE) return (FALSE);

 #ifdef ALLOW_REPEAT
 
 	repeat_push(*com_val);
     
 #endif /* ALLOW_REPEAT -- TNB */
 

	/* Success */
	return (TRUE);
}

/*
 * A wrapper to provide the (known) limits for the prompt above.
 */
static int get_stock(int *com_val, cptr pmt)
{
	int i = MIN(st_ptr->stock_num - store_top, term_screen->hgt-12);
	return get_stock_aux(com_val, pmt, 0, i-1);
}

/*
 * Increase the insult counter and get angry if too many -RAK-
 */
static int increase_insults(void)
{
	/* Increase insults */
	st_ptr->insult_cur++;

	/* Become insulted */
	if (st_ptr->insult_cur > ot_ptr->insult_max)
	{
		/* Complain */
		say_comment_4();

		/* Reset insults */
		st_ptr->insult_cur = 0;
		st_ptr->good_buy = 0;
		st_ptr->bad_buy = 0;

		/* Open tomorrow */
		st_ptr->store_open = turn + 25000 + randint(25000);

		/* Closed */
		return (TRUE);
	}

	/* Not closed */
	return (FALSE);
}


/*
 * Decrease insults					-RAK-
 */
static void decrease_insults(void)
{
	/* Decrease insults */
	if (st_ptr->insult_cur) st_ptr->insult_cur--;
}


/*
 * Have insulted while haggling				-RAK-
 */
static int haggle_insults(void)
{
	/* Increase insults */
	if (increase_insults()) return (TRUE);

	/* Display and flush insult */
	say_comment_5();

	/* Still okay */
	return (FALSE);
}


/*
 * Mega-Hack -- Enable "increments"
 */
static bool allow_inc = FALSE;

/*
 * Mega-Hack -- Last "increment" during haggling
 */
static s32b last_inc = 0L;


/*
 * Get a haggle
 */
static int get_haggle(cptr pmt, s32b *poffer, s32b price, int final)
{
	s32b		i;

	cptr		p;

	char                buf[128];
	char		out_val[160];


	/* Clear old increment if necessary */
	if (!allow_inc) last_inc = 0L;


	/* Final offer */
	if (final)
	{
		sprintf(buf, "%s [accept] ", pmt);
	}

	/* Old (negative) increment, and not final */
	else if (last_inc < 0)
	{
		sprintf(buf, "%s [-%ld] ", pmt, (long)(ABS(last_inc)));
	}

	/* Old (positive) increment, and not final */
	else if (last_inc > 0)
	{
		sprintf(buf, "%s [+%ld] ", pmt, (long)(ABS(last_inc)));
	}

	/* Normal haggle */
	else
	{
		sprintf(buf, "%s ", pmt);
	}
	

	/* Paranoia XXX XXX XXX */
	msg_print(NULL);


	/* Ask until done */
	while (TRUE)
	{
		/* Default */
		strcpy(out_val, "");

		/* Ask the user for a response */
		if (!get_string(buf, out_val, 32)) return (FALSE);

		/* Skip leading spaces */
		for (p = out_val; *p == ' '; p++) /* loop */;

		/* Empty response */
		if (*p == '\0')
		{
			/* Accept current price */
			if (final)
			{
				*poffer = price;
				last_inc = 0L;
				break;
			}

			/* Use previous increment */
			if (allow_inc && last_inc)
			{
				*poffer += last_inc;
				break;
			}
		}

		/* Normal response */
		else
		{
			/* Extract a number */
			i = atol(p);

			/* Handle "incremental" number */
			if ((*p == '+' || *p == '-'))
			{
				/* Allow increments */
				if (allow_inc)
				{
					/* Use the given "increment" */
					*poffer += i;
					last_inc = i;
					break;
				}
			}

			/* Handle normal number */
			else
			{
				/* Use the given "number" */
				*poffer = i;
				last_inc = 0L;
				break;
			}
		}

		/* Warning */
		msg_print("Invalid response.");
		msg_print(NULL);
	}

	/* Success */
	return (TRUE);
}


/*
 * Receive an offer (from the player)
 *
 * Return TRUE if offer is NOT okay
 */
static bool receive_offer(cptr pmt, s32b *poffer,
                          s32b last_offer, int factor,
                          s32b price, int final)
{
	/* Haggle till done */
	while (TRUE)
	{
		/* Get a haggle (or cancel) */
		if (!get_haggle(pmt, poffer, price, final)) return (TRUE);

		/* Acceptable offer */
		if (((*poffer) * factor) >= (last_offer * factor)) break;

		/* Insult, and check for kicked out */
		if (haggle_insults()) return (TRUE);

		/* Reject offer (correctly) */
		(*poffer) = last_offer;
	}

	/* Success */
	return (FALSE);
}


/*
 * Haggling routine					-RAK-
 *
 * Return TRUE if purchase is NOT successful
 */
static bool purchase_haggle(object_type *o_ptr, s32b *price)
{
	s32b               last_offer, offer;
	s32b               x1, x2, x3;
	s32b               min_per, max_per;
	int                flag, loop_flag;
	int                annoyed = 0, final = FALSE;

	bool		cancel = FALSE;
	char		out_val[160];
	C_TNEW(o_name, ONAME_MAX, char);

	pmt = "Asking";
	*price = 0;


	/* Extract the starting offer and the final offer */
	cur_ask = price_item(o_ptr, ot_ptr->max_inflate, FALSE);
	final_ask = price_item(o_ptr, ot_ptr->min_inflate, FALSE);

	/* Determine if haggling is necessary. */
	final = needtohaggle(0);


	/* Haggle for the whole pile */
	cur_ask *= o_ptr->number;
	final_ask *= o_ptr->number;

	if ((auto_haggle || final) && !verbose_haggle)
	{
		if(cur_store_type == STORE_PAWN)
			strnfmt(o_name, ONAME_MAX, "%v", object_desc_f3, o_ptr, TRUE, 3);
		else
		strnfmt(o_name, ONAME_MAX, "%v", object_desc_store_f3, o_ptr, TRUE, 3);
		sprintf(out_val, "%s %ld for %s? ", pmt, cur_ask, o_name);
		*price = final_ask;
		return !get_check(out_val);
	}

	/* Haggle parameters */
	min_per = ot_ptr->haggle_per;
	max_per = min_per * 3;

	last_offer = 1;
	/* No offer yet */
	offer = 0;

	/* No incremental haggling yet */
	allow_inc = FALSE;

	/* Haggle until done */
	for (flag = FALSE; !flag; )
	{
		loop_flag = TRUE;

		while (!flag && loop_flag)
		{
			(void)sprintf(out_val, "%s :  %ld", pmt, (long)cur_ask);
			put_str(out_val, 1, 0);
			cancel = receive_offer("What do you offer? ",
			                       &offer, last_offer, 1, cur_ask, final);

			if (cancel)
			{
				flag = TRUE;
			}
			else if (offer > cur_ask)
			{
				say_comment_6();
				offer = last_offer;
			}
			else if (offer == cur_ask)
			{
				flag = TRUE;
				*price = offer;
			}
			else
			{
				loop_flag = FALSE;
			}
		}

		if (!flag)
		{
			x1 = 100 * (offer - last_offer) / (cur_ask - last_offer);
			if (x1 < min_per)
			{
				if (haggle_insults())
				{
					flag = TRUE;
					cancel = TRUE;
				}
			}
			else if (x1 > max_per)
			{
				x1 = x1 * 3 / 4;
				if (x1 < max_per) x1 = max_per;
			}
			x2 = rand_range(x1-2, x1+2);
			x3 = ((cur_ask - offer) * x2 / 100L) + 1;
			/* don't let the price go up */
			if (x3 < 0) x3 = 0;
			cur_ask -= x3;

			/* Too little */
			if (cur_ask < final_ask)
			{
				final = TRUE;
				cur_ask = final_ask;
				pmt = "Final Offer";
				annoyed++;
				if (annoyed > 3)
				{
					(void)(increase_insults());
					cancel = TRUE;
					flag = TRUE;
				}
			}
			else if (offer >= cur_ask)
			{
				flag = TRUE;
				*price = offer;
			}

			if (!flag)
			{
				last_offer = offer;
				allow_inc = TRUE;
				prt("", 1, 0);
				(void)sprintf(out_val, "Your last offer: %ld",
				              (long)last_offer);
				put_str(out_val, 1, 39);
				say_comment_2(cur_ask, annoyed);
			}
		}
	}

	TFREE(o_name);

	/* Cancel */
	if (cancel) return (TRUE);

	/* Update bargaining info */
	updatebargain(*price, final_ask);

	/* Do not cancel */
	return (FALSE);
}

/*
 * Gets help for a service based on the index given in store_process_command()
 */
static void service_help(byte type)
{
	/* Erase the prompt */
	prt("", 0, 0);
	/* Give some relevant help */
	switch (type)
	{
		case STORE_ARMOURY: /* Enchant armour */
			msg_print("Almost as if you were to use four scrolls of enchant armour on an item.");
			break;
		case STORE_WEAPON: /* Enchant weapon */
			msg_print("Almost as if you were to use four scrolls of enchant weapon to-dam, and four of enchant weapon to-hit, on an item.");
			break;
		case STORE_TEMPLE: /* Restoration */
		{
			object_type forge;
			object_prep(&forge, OBJ_FOOD_RESTORING);
			/* This gives an unwelcome "Item attributes" description. */
			if (!identify_fully_aux(&forge, TRUE))
				msg_print("This won't help you at present.");
			break;
 		}
		case STORE_ALCHEMIST: /* Identify all */
		{
			int i, j = 0;
			for (i = 0; i < INVEN_TOTAL; i++)
			{
				object_type *o_ptr = &inventory[i];
				if (o_ptr->k_idx && !object_known_p(o_ptr)) j++;
			}
			if (j)
				msg_format("Identifies your %d unidentified items", j);
			else
				msg_print("This won't help you at present.");
			break;
		}
		case STORE_MAGIC: /* Ritual of Recall */
			msg_print("Revives you here at the point of death.");
			break;
		case STORE_INN: /* Rest */
			switch (p_ptr->prace)
			{
				case RACE_SPECTRE: case RACE_ZOMBIE:
				case RACE_SKELETON: case RACE_VAMPIRE:
					msg_print("Lets you rest here for the day");
				default:
					msg_print("Lets you rest here for the night");
			}			
			break;
		case STORE_HALL: /* Buy a house */
			msg_format("Gives you a house in %s to store your belongings.", town_name+town_defs[cur_town].name);
			break;
		case 128: case 129: case 130: case 131:
		case 132: case 133: case 134: case 135: /* Associate with spirit */
		default: /* Just in case */
		msg_print("Sorry, no help available");
	}
	/* Display the prompt */
	msg_print(NULL);
}

/*
 * Modified version of get_check() to give a [y/n/?] prompt with some extra
 * information available.
 */
static bool get_check_service(cptr prompt, byte type)
{
	char rc;

	/* Help */
	help_track("yn?_prompt");

	while (1)
	{
		rc = get_check_aux(
			prompt, "%.*s[y/n/?]%s%v", "nN\033yY\r?", "\0\0\0\1\1\1?");

		/* Accept boolean responses. */
		if (rc != '?') break;

		service_help(type);
	}

	/* Leave a message. */
	message_add(format(0));

	/* Done with help */
	help_track(NULL);

	return (rc != '\0');
}

/* Haggle for a fixed price service from a store owner */
/* Altered form of purchase_haggle by DA */
static bool service_haggle(s32b service_cost, s32b *price, cptr service, byte type)
{
	s32b               last_offer, offer;
	s32b               x1, x2, x3;
	s32b               min_per, max_per;
	int                flag, loop_flag;
	int                annoyed = 0, final = FALSE;

	bool		cancel = FALSE;
	char		out_val[160];


	pmt = "Asking";
	*price = 0;


	/* Extract the starting offer and the final offer */
	cur_ask = service_cost * 2;
	final_ask = service_cost;

	/* Determine if haggling is necessary. */
	final = needtohaggle(0);

	if (auto_haggle && !verbose_haggle)
		{
		sprintf(out_val, "%s %ld to %s? ", pmt, cur_ask, service);
		*price = final_ask;
		return !get_check_service(out_val, type);
		}

	/* Haggle parameters */
	min_per = ot_ptr->haggle_per;
	max_per = min_per * 3;

	last_offer = 1;
	/* No offer yet */
	offer = 0;

	/* No incremental haggling yet */
	allow_inc = FALSE;

	/* Haggle until done */
	for (flag = FALSE; !flag; )
	{
		loop_flag = TRUE;

		while (!flag && loop_flag)
		{
			(void)sprintf(out_val, "%s :  %ld", pmt, (long)cur_ask);
			put_str(out_val, 1, 0);
			cancel = receive_offer("What do you offer? ",
			                       &offer, last_offer, 1, cur_ask, final);

			if (cancel)
			{
				flag = TRUE;
			}
			else if (offer > cur_ask)
			{
				say_comment_6();
				offer = last_offer;
			}
			else if (offer == cur_ask)
			{
				flag = TRUE;
				*price = offer;
			}
			else
			{
				loop_flag = FALSE;
			}
		}

		if (!flag)
		{
			x1 = 100 * (offer - last_offer) / (cur_ask - last_offer);
			if (x1 < min_per)
			{
				if (haggle_insults())
				{
					flag = TRUE;
					cancel = TRUE;
				}
			}
			else if (x1 > max_per)
			{
				x1 = x1 * 3 / 4;
				if (x1 < max_per) x1 = max_per;
			}
			x2 = rand_range(x1-2, x1+2);
			x3 = ((cur_ask - offer) * x2 / 100L) + 1;
			/* don't let the price go up */
			if (x3 < 0) x3 = 0;
			cur_ask -= x3;

			/* Too little */
			if (cur_ask < final_ask)
			{
				final = TRUE;
				cur_ask = final_ask;
				pmt = "Final Offer";
				annoyed++;
				if (annoyed > 3)
				{
					(void)(increase_insults());
					cancel = TRUE;
					flag = TRUE;
				}
			}
			else if (offer >= cur_ask)
			{
				flag = TRUE;
				*price = offer;
			}

			if (!flag)
			{
				last_offer = offer;
				allow_inc = TRUE;
				prt("", 1, 0);
				(void)sprintf(out_val, "Your last offer: %ld",
				              (long)last_offer);
				put_str(out_val, 1, 39);
				say_comment_2(cur_ask, annoyed);
			}
		}
	}

	/* Cancel */
	if (cancel) return (TRUE);

	/* Update bargaining info */
	updatebargain(*price, final_ask);

	/* Do not cancel */
	return (FALSE);
}


/*
 * Haggling routine					-RAK-
 *
 * Return TRUE if purchase is NOT successful
 */
static bool sell_haggle(object_type *o_ptr, s32b *price)
{
	s32b               last_offer = 0, offer = 0;
	s32b               x1, x2, x3;
	s32b               min_per, max_per;

	int			flag, loop_flag;
	int			annoyed = 0, final = FALSE;

	bool		cancel = FALSE;
	char		out_val[160];

	pmt = "Offer";
	*price = 0;


	/* Obtain the starting offer and the final offer */
	cur_ask = price_item(o_ptr, ot_ptr->max_inflate, TRUE);
	final_ask = price_item(o_ptr, ot_ptr->min_inflate, TRUE);

	/* Determine if haggling is necessary. */
	final = needtohaggle(ot_ptr->max_cost);

	/* Haggle for the whole pile */
	cur_ask *= o_ptr->number;
	final_ask *= o_ptr->number;

	if (auto_haggle && !verbose_haggle)
	{
		C_TNEW(o_name, ONAME_MAX, char);
		strnfmt(o_name, ONAME_MAX, "%v", object_desc_f3, o_ptr, TRUE, 3);
		sprintf(out_val, "%s %ld for %s? ", pmt, cur_ask, o_name);
		TFREE(o_name);
		*price = final_ask;
		return !get_check(out_val);
	}

	/* XXX XXX XXX Display commands */

	/* Haggling parameters */
	min_per = ot_ptr->haggle_per;
	max_per = min_per * 3;

	/* Mega-Hack -- artificial "last offer" value */
	last_offer = object_value(o_ptr) * o_ptr->number;
	last_offer = last_offer * ot_ptr->max_inflate / 100L;

	/* No offer yet */
	offer = 0;

	/* No incremental haggling yet */
	allow_inc = FALSE;

	/* Haggle */
	for (flag = FALSE; !flag; )
	{
		while (1)
		{
			loop_flag = TRUE;

			(void)sprintf(out_val, "%s :  %ld", pmt, (long)cur_ask);
			put_str(out_val, 1, 0);
			cancel = receive_offer("What price do you ask? ",
			                       &offer, last_offer, -1, cur_ask, final);

			if (cancel)
			{
				flag = TRUE;
			}
			else if (offer < cur_ask)
			{
				say_comment_6();
				/* rejected, reset offer for incremental haggling */
				offer = last_offer;
			}
			else if (offer == cur_ask)
			{
				flag = TRUE;
				*price = offer;
			}
			else
			{
				loop_flag = FALSE;
			}

			/* Stop */
			if (flag || !loop_flag) break;
		}

		if (!flag)
		{
			x1 = 100 * (last_offer - offer) / (last_offer - cur_ask);
			if (x1 < min_per)
			{
				if (haggle_insults())
				{
					flag = TRUE;
					cancel = TRUE;
				}
			}
			else if (x1 > max_per)
			{
				x1 = x1 * 3 / 4;
				if (x1 < max_per) x1 = max_per;
			}
			x2 = rand_range(x1-2, x1+2);
			x3 = ((offer - cur_ask) * x2 / 100L) + 1;
			/* don't let the price go down */
			if (x3 < 0) x3 = 0;
			cur_ask += x3;

			if (cur_ask > final_ask)
			{
				cur_ask = final_ask;
				final = TRUE;
				pmt = "Final Offer";
				annoyed++;
				if (annoyed > 3)
				{
					flag = TRUE;
					(void)(increase_insults());
				}
			}
			else if (offer <= cur_ask)
			{
				flag = TRUE;
				*price = offer;
			}

			if (!flag)
			{
				last_offer = offer;
				allow_inc = TRUE;
				prt("", 1, 0);
				(void)sprintf(out_val,
				              "Your last bid %ld", (long)last_offer);
				put_str(out_val, 1, 39);
				say_comment_3(cur_ask, annoyed);
			}
		}
	}

	/* Cancel */
	if (cancel) return (TRUE);

	/* Update bargaining info */
	updatebargain(*price, final_ask);

	/* Do not cancel */
	return (FALSE);
}





/*
 * Buy an item from a store				-RAK-
 */
static void store_purchase_aux(char *o_name)
{
	int i, amt, choice;
	int item;

	s32b price, best;

	object_type forge;
	object_type *j_ptr;

	object_type *o_ptr;

	char out_val[160];


	/* Empty? */
	if (st_ptr->stock_num <= 0)
	{
		if (cur_store_type == STORE_HOME) msg_print("Your home is empty.");
		else msg_print("I am currently out of stock.");
		return;
	}


	/* Prompt */
	if (cur_store_type == STORE_HOME)
	{
		sprintf(out_val, "Which item do you want to take? ");
	}
	else
	{
		sprintf(out_val, "Which item are you interested in? ");
	}

	/* Get the item number to be bought */
	if (!get_stock(&item, out_val)) return;

	/* Get the actual index */
	item = item + store_top;

	/* Get the actual item */
	o_ptr = &st_ptr->stock[item];

	/* Assume the player wants just one of them */
	amt = 1;

	/* Get local object */
	j_ptr = &forge;
	
	/* Get a copy of the object */
	object_copy(j_ptr, o_ptr);

	/* Modify quantity */
	j_ptr->number = amt;

	/* Hack -- require room in pack */
	if (!inven_carry_okay(j_ptr))
	{
		msg_print("You cannot carry that many different items.");
		return;
	}

	/* Determine the "best" price (per item) */
	best = price_item(j_ptr, ot_ptr->min_inflate, FALSE);

	/* Find out how many the player wants */
	if (o_ptr->number > 1)
	{
		/* Hack -- note cost of "fixed" items */
		if ((cur_store_type != 7) && (o_ptr->ident & (IDENT_FIXED)) && verbose_haggle)
		{
			msg_format("That costs %ld gold per item.", (long)(best));
		}

		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number,FALSE);

		/* Allow user abort */
		if (amt <= 0) return;
	}

	/* Get local object */
	j_ptr = &forge;
	
	/* Get desired object */
	object_copy(j_ptr, o_ptr);

	/* Modify quantity */
	j_ptr->number = amt;

	/* Hack -- require room in pack */
	if (!inven_carry_okay(j_ptr))
	{
		msg_print("You cannot carry that many items.");
		return;
	}

	/* Attempt to buy it */
	if (cur_store_type != 7)
	{
		/* Fixed price, quick buy */
		if ((o_ptr->ident & (IDENT_FIXED)) && verbose_haggle)
		{
			/* Assume accept */
			choice = 0;

			/* Go directly to the "best" deal */
			price = (best * j_ptr->number);
		}

		/* Haggle for it */
		else
		{
			/* Describe the object (fully) */
			if(cur_store_type == STORE_PAWN)
			{
				strnfmt(o_name, ONAME_MAX, "%v", object_desc_f3, j_ptr, TRUE, 3);
			}
			else
			{
				strnfmt(o_name, ONAME_MAX, "%v", object_desc_store_f3, j_ptr, TRUE, 3);
			}
			/* Message */
			if (!auto_haggle || verbose_haggle)
			{
			msg_format("Buying %s (%c).", o_name, I2A(item));
			msg_print(NULL);
			}

			/* Haggle for a final price */
			choice = purchase_haggle(j_ptr, &price);

			/* Hack -- Got kicked out */
			if (st_ptr->store_open >= turn) return;
		}


		/* Player wants it */
		if (choice == 0)
		{
			/* Fix the item price (if "correctly" haggled) */
			if (price == (best * j_ptr->number)) o_ptr->ident |= (IDENT_FIXED);

			/* Player can afford it */
			if (p_ptr->au >= price)
			{
				/* Say "okay" */
				say_comment_1();

				/* Make a sound */
				sound(SOUND_BUY);

				/* Be happy */
				decrease_insults();

				/* Spend the money */
				p_ptr->au -= price;

				/* Update the display */
				store_prt_gold();

				/* Hack -- buying an item makes you aware of it */
				if(cur_store_type != STORE_PAWN) object_aware(j_ptr);

				/* Hack -- clear the "fixed" flag from the item */
				j_ptr->ident &= ~(IDENT_FIXED);

				/* Hack -- clear the "store" flag from the item */
				j_ptr->ident &= ~(IDENT_STORE);

				/* Describe the transaction */
				strnfmt(o_name, ONAME_MAX, "%v", object_desc_f3, j_ptr, TRUE, 3);

				/* Message */
				if (!auto_haggle || verbose_haggle)
				{
				if (cur_store_type ==STORE_PAWN)
				{
					msg_format("You bought back %s for %ld gold.",o_name,(long)price);
				}
				else
				{
					msg_format("You bought %s for %ld gold.", o_name, (long)price);
				}
				}

				/* Erase the inscription */
				j_ptr->note = 0;

				/* Give it to the player */
				j_ptr = inven_carry(j_ptr);

				/* Describe the final result */
				strnfmt(o_name, ONAME_MAX, "%v", object_desc_f3, j_ptr, TRUE, 3);

				/* Message */
				msg_format("You have %s (%c).",
				           o_name, index_to_label(j_ptr));

				/* Handle stuff */
				handle_stuff();

				/* Note how many slots the store used to have */
				i = st_ptr->stock_num;

				/* Remove the bought items from the store */
				store_item_increase(item, -amt);
				store_item_optimize(item);

				/* Store is empty*/
				if (st_ptr->stock_num == 0)
				{
								
					/* Nothing left  in Pawnbrokers*/
					if(cur_store_type == STORE_PAWN)
					{
						store_top = 0;
					}
					else
					{
						/* Shuffle */
						if (rand_int(STORE_SHUFFLE) == 0)
						{
							/* Message */
							msg_print("The shopkeeper retires.");

							/* Shuffle the store */
							store_shuffle(cur_store_num);
						}

						/* Maintain */
						else
						{
							/* Message */
							msg_print("The shopkeeper brings out some new stock.");
						}

						/* New inventory */
						for (i = 0; i < 10; i++)
						{
							/* Maintain the store */
							store_maint(cur_store_num);
						}

						/* Start over */
						store_top = 0;
					}
				}

				/* The item is gone */
				else if (st_ptr->stock_num != i)
				{
					/* Pick the correct screen */
					if (store_top >= st_ptr->stock_num) store_top -= 12;
				}
			}

			/* Player cannot afford it */
			else
			{
				/* Simple message (no insult) */
				msg_print("You do not have enough gold.");
			}
		}
	}

	/* Home is much easier */
	else
	{
		/* Give it to the player */
		j_ptr = inven_carry(j_ptr);

		/* Describe just the result */
		strnfmt(o_name, ONAME_MAX, "%v", object_desc_f3, j_ptr, TRUE, 3);

		/* Message */
		msg_format("You have %s (%c).", o_name, index_to_label(j_ptr));

		/* Handle stuff */
		handle_stuff();

		/* Take note if we take the last one */
		i = st_ptr->stock_num;

		/* Remove the items from the home */
		store_item_increase(item, -amt);
		store_item_optimize(item);

		/* The item is gone */
		if (i != st_ptr->stock_num)
		{
			/* Nothing left */
			if (st_ptr->stock_num == 0) store_top = 0;

			/* Nothing left on that screen */
			else if (store_top >= st_ptr->stock_num) store_top -= 12;
		}
	}

	/* Not kicked out */
	return;
}

/*
 * A wrapper around the above for memory allocation.
 */
static void store_purchase(void)
{
	C_TNEW(o_name, ONAME_MAX, char);
	store_purchase_aux(o_name);
	TFREE(o_name);
}

/*
 * Sell an item to the store (or home)
 */
static void store_sell_aux(char *o_name)
{
	errr err;
	int choice;
	int item_pos;
	int amt;

	s32b price, value, dummy;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;

	cptr pmt = "Sell which item? ";


	/* Prepare a prompt */
	if (cur_store_type == STORE_HOME) pmt = "Drop which item? ";

	/* Only allow items the store will buy */
	item_tester_hook = store_will_buy;

	/* Get an item (from equip or inven) */
	if (!((o_ptr = get_item(&err, pmt, TRUE, TRUE, FALSE))))
	{
		if (err == -2) msg_print("You have nothing that I want.");
		return;
	}


	/* Hack -- Cannot remove cursed items */
	if (!item_tester_hook_destroy(o_ptr))
	{
		/* Oops */
		msg_print("Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}


	/* Assume one item */
	amt = 1;

	/* Get local object */
	q_ptr = &forge;
	
	/* Get a copy of the object */
	object_copy(q_ptr, o_ptr);

	/* Get a full description */
	strnfmt(o_name, ONAME_MAX, "%v", object_desc_f3, q_ptr, TRUE, 3);

	/* Remove any inscription for stores */
	if (cur_store_type != 7) q_ptr->note = 0;


	/* Is there room in the store (or the home?) */
	if (!((amt = store_check_num(q_ptr))))
	{
		if (cur_store_type == STORE_HOME) msg_print("Your home is full.");
		else msg_print("I have not the room in my store to keep it.");
		return;
	}
	else
	{
		amt = MIN(amt, o_ptr->number);
	}

	/* Find out how many the player wants (letter means "all") */
	if (o_ptr->number > 1)
	{
		/* Get a quantity */
		amt = get_quantity(NULL,amt,TRUE);

		/* Allow user abort */
		if (amt <= 0) return;
	}

	/* Modify quantity */
	q_ptr->number = amt;

	/* Real store */
	if (cur_store_type != STORE_HOME)
	{
		/* Describe the transaction */
		if (!auto_haggle || verbose_haggle)
		{
		msg_format("Selling %s (%c).", o_name, index_to_label(o_ptr));
		msg_print(NULL);
		}

		/* Haggle for it */
		choice = sell_haggle(q_ptr, &price);

		/* Kicked out */
		if (st_ptr->store_open >= turn) return;

		/* Sold... */
		if (choice == 0)
		{
			/* Say "okay" */
			say_comment_1();

			/* Make a sound */
			sound(SOUND_SELL);

			/* Be happy */
			decrease_insults();

			/* Get some money */
			p_ptr->au += price;

			/* Update the display */
			store_prt_gold();

			/* Get the "apparent" value */
			dummy = object_value(q_ptr) * q_ptr->number;
			
			if (cur_store_type != STORE_PAWN)
			{
				/* Identify original item */
				object_aware(o_ptr);
				object_known(o_ptr);
			}

			/* Combine / Reorder the pack (later) */
			p_ptr->notice |= (PN_COMBINE | PN_REORDER);

			/* Window stuff */
			p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

			/* Get local object */
			q_ptr = &forge;
	
			/* Get a copy of the object */
			object_copy(q_ptr, o_ptr);

			/* Modify quantity */
			q_ptr->number = amt;

			/* Clear inscription (again?) */
			q_ptr->note = 0;

			/* Get the "actual" value */
			if (cur_store_type == STORE_PAWN)
			{
				value = dummy;
			}
			else
			{
				value = object_value(q_ptr) * q_ptr->number;
				/* Get the description all over again */
				strnfmt(o_name, ONAME_MAX, "%v", object_desc_f3, q_ptr, TRUE, 3);
			}


			if (!auto_haggle || verbose_haggle)
			{
			if (cur_store_type != STORE_PAWN)
			{
			/* Describe the result (in message buffer) */
				msg_format("You sold %s for %ld gold.", o_name, (long)price);
			}
			else
			{
				msg_format("You pawn %s for %ld gold.",o_name,(long)price);
			}
			}

			/* Analyze the prices (and comment verbally) */
				purchase_analyze(price, value, dummy);

			/* Take the item from the player, describe the result */
			item_increase(o_ptr, -amt);
			item_describe(o_ptr);
			item_optimize(o_ptr);

			/* Handle stuff */
			handle_stuff();
			if(cur_store_type != STORE_PAWN)
			{
				/* The store gets that (known) item */
				item_pos = store_carry(q_ptr);
			}
			else
			{
				/* The pawnshop gets that (unknown) item */
				item_pos = home_carry(q_ptr);
			}

			/* Re-display if item is now in store */
			if (item_pos >= 0)
			{
				store_top = (item_pos / 12) * 12;
			}
		}
	}

	/* Player is at home */
	else
	{
		/* Describe */
		msg_format("You drop %s (%c).", o_name, index_to_label(o_ptr));

		/* Take it from the players inventory */
		item_increase(o_ptr, -amt);
		item_describe(o_ptr);
		item_optimize(o_ptr);

		/* Handle stuff */
		handle_stuff();

		/* Let the home carry it */
		item_pos = home_carry(q_ptr);

		/* Update store display */
		if (item_pos >= 0)
		{
			store_top = (item_pos / 12) * 12;
		}
	}
}

/*
 * A wrapper around the above for memory allocation.
 */
static void store_sell(void)
{
	C_TNEW(o_name, ONAME_MAX, char);
	store_sell_aux(o_name);
	TFREE(o_name);
}


 /*
  * Examine an item in a store             -JDL-
  */
 static void store_examine(void)
 {
   int item;

   object_type *o_ptr;

   char out_val[160];


   /* Empty? */
   if (st_ptr->stock_num <= 0)
   {
       if (cur_store_type == STORE_HOME) msg_print("Your home is empty.");
       else if (cur_store_type == STORE_PAWN) msg_print("I have nothing of yours.");
       else msg_print("I am currently out of stock.");
       return;
   }


   /* Prompt */
   sprintf(out_val, "Which item do you want to examine? ");

   /* Get the item number to be examined */
   if (!get_stock(&item, out_val)) return;

   /* Get the actual index */
   item = item + store_top;

   /* Get the actual item */
   o_ptr = &st_ptr->stock[item];

   /* If it is a spell book then browse it */
   if ((o_ptr->tval == TV_SORCERY_BOOK) ||
        (o_ptr->tval == TV_THAUMATURGY_BOOK) ||
        (o_ptr->tval == TV_CONJURATION_BOOK) || (o_ptr->tval == TV_NECROMANCY_BOOK))
	{
		do_store_browse(o_ptr);
		return;
	}

   /* Description */
   if (cur_store_type == STORE_HOME || cur_store_type == STORE_PAWN)
	{
		C_TNEW(o_name, ONAME_MAX, char);
		strnfmt(o_name, ONAME_MAX, "%v", object_desc_f3, o_ptr, TRUE, 3);
		msg_format("Examining %s...", o_name);
		if (!identify_fully_aux(o_ptr, FALSE)) msg_print("You see nothing special.");
		TFREE(o_name);
	}
	else /* Make it look as though we are aware of the item. */
	{
		C_TNEW(o_name, ONAME_MAX, char);
		strnfmt(o_name, ONAME_MAX, "%v", object_desc_store_f3, o_ptr, TRUE, 3);
		msg_format("Examining %s...", o_name);
		if (!identify_fully_aux(o_ptr, TRUE)) msg_print("You see nothing special.");
		TFREE(o_name);
	}

   return;
 }



store_type *find_house(int town)
{
	int j;
	for (j = 0; j < town_defs[town].numstores; j++)
	{
		store_type *st_ptr = &store[j+MAX_STORES_PER_TOWN * town];
		if (st_ptr->type == STORE_HOME) return st_ptr;
	}
	return NULL;
}


/*
 * Hack -- set this to leave the store
 */
static bool leave_store = FALSE;

/* Work out the player's maximum skill */
static s32b max_player_skill(void)
{
	int i;
	byte max = 0;
	for (i=0;i<MAX_SKILLS;i++)
	{
		if(skill_set[i].max_value > max) max = skill_set[i].max_value;
	}
	return (s32b)max;
}

/*
 * Process a command in a store
 *
 * Note that we must allow the use of a few "special" commands
 * in the stores which are not allowed in the dungeon, and we
 * must disable some commands which are allowed in the dungeon
 * but not in the stores, to prevent chaos.
 */
static void store_process_command(void)
{
	s32b price,cost;
	int i;
	char buf[80];

	const int items = term_screen->hgt-12;

 #ifdef ALLOW_REPEAT
 
     /* Handle repeating the last command */
     repeat_check();
 
 #endif /* ALLOW_REPEAT -- TNB */
 
    if (rogue_like_commands && command_cmd == 'l')
    {
        command_cmd = 'x';  /* hack! */
    }

	/* Parse the command */
	switch (command_cmd)
	{
			/* Leave */
		case ESCAPE:
		{
			leave_store = TRUE;
			break;
		}

			/* Browse */
        case ' ':
		{
			if (st_ptr->stock_num <= items && !store_top)
			{
				msg_print("Entire inventory is shown.");
			}
			else
			{
				store_top += items;
				if (store_top >= st_ptr->stock_num) store_top = 0;
			}
			break;
		}

			/* Redraw */
		case KTRL('R'):
		{
			do_cmd_redraw();
			display_store();
			break;
		}

			/* Get (purchase) */
		case 'g':
		{
			store_purchase();
			break;
		}

			/* Drop (Sell) */
		case 'd':
		{
			store_sell();
			break;
		}

           /* Examine */
       case 'x':
       {
           store_examine();
           break;
       }

			/* View hiscores in hall */
	   case 'c':
		   {
			   if(cur_store_type == STORE_HALL)
			   {
					template_score(p_ptr->ptemplate);
			   }
			   else
			   {
					msg_print("That command does not work in this store.");
			   }
			   break;
		   }
	   case 'h': case '4':
		   {
			   if(cur_store_type == STORE_HALL)
			   {
					race_score(p_ptr->prace);
			   }
			   else
			   {
					msg_print("That command does not work in this store.");
			   }
				break;
		   }

		/* perform 'special' for store */
	   case 'r':
		   {
			   switch (cur_store_type)
			   {
			   case STORE_GENERAL:
				   {
						msg_print("That command does not work in this store.");
					   break;
				   }
			   case STORE_ARMOURY:
				   {
						if (!service_haggle(400,&price, service_name[cur_store_type][0], STORE_ARMOURY))
						{
							if (price > p_ptr->au)
							{
								msg_format("You do not have the gold!");
							}
							else
							{
								/* Say "okay" */
								say_comment_1();
								/* Make a sound */
								sound(SOUND_BUY);
								if (enchant_spell(0,0,4))
								{
									p_ptr->au -= price;
								/* Be happy */
								decrease_insults();
								store_prt_gold();
								}
							}
							p_ptr->window |= (PW_PLAYER);
							handle_stuff();
						}
						break;
				   }
			   case STORE_WEAPON:
				   {
						if (!service_haggle(800,&price, service_name[cur_store_type][0], STORE_WEAPON))
						{
							if (price > p_ptr->au)
							{
								msg_format("You do not have the gold!");
							}
							else
							{
								/* Say "okay" */
								say_comment_1();
								/* Make a sound */
								sound(SOUND_BUY);
								if (enchant_spell(4,4,0))
								{
									p_ptr->au -= price;
								/* Be happy */
								decrease_insults();
								store_prt_gold();
								}
							}
							p_ptr->window |= (PW_PLAYER);
							handle_stuff();
						}
						break;
				   }
			   case STORE_TEMPLE:
					{
						if (!service_haggle(750,&price, service_name[cur_store_type][0], STORE_TEMPLE))
						{
							if (price > p_ptr->au)
							{
								msg_format("You do not have the gold!");
							}
							else
							{
								p_ptr->au -= price;
								/* Say "okay" */
								say_comment_1();
								/* Make a sound */
								sound(SOUND_BUY);
								/* Be happy */
								decrease_insults();
								store_prt_gold();
								do_res_stat(A_STR);
								do_res_stat(A_INT);
								do_res_stat(A_WIS);
								do_res_stat(A_DEX);
								do_res_stat(A_CON);
								do_res_stat(A_CHR);
								restore_level();
							}
							p_ptr->window |= (PW_SPELL | PW_PLAYER);
							handle_stuff();
						}
						break;
					}
				case STORE_ALCHEMIST:
					{
						if (!service_haggle(500,&price, service_name[cur_store_type][0], STORE_ALCHEMIST))
						{
							if (price > p_ptr->au)
							{
								msg_format("You do not have the gold!");
							}
							else
							{
								p_ptr->au -= price;
								/* Say "okay" */
								say_comment_1();
								/* Make a sound */
								sound(SOUND_BUY);
								/* Be happy */
								decrease_insults();
								store_prt_gold();
								identify_pack();
								msg_format("All your goods have been identified.");
							}
							p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
							handle_stuff();
						}
						break;
				   }
			   case STORE_MAGIC:
				   {
					   	if (p_ptr->ritual != TOWN_NONE)
					   {
						   msg_format("You have already done the ritual!");
					   }
					   else
					   {
							/* cost based on experience */
							cost=10*max_player_skill();
							cost = cost * cost;
							/* minimum of 100 gold */
							if(cost < 100) cost = 100;
							if (!service_haggle(cost,&price, service_name[cur_store_type][0], STORE_MAGIC))
							{
								if (price > p_ptr->au)
								{
									msg_format("You do not have the gold!");
								}
								else
								{
									p_ptr->au -= price;
									/* Say "okay" */
									say_comment_1();
									/* Make a sound */
									sound(SOUND_BUY);
									/* Be happy */
									decrease_insults();
									store_prt_gold();
									p_ptr->ritual = cur_town;
									msg_format("You perform the Ritual of True Recall.");
								}
								p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
							handle_stuff();
							}
						}
						break;
				   }
			   case STORE_BLACK:
				   {
						msg_print("That command does not work in this store.");
					   break;
				   }
			   case STORE_HOME:
				   {
						if ((p_ptr->poisoned > 0) || (p_ptr->cut > 0))
						{
							msg_print("Your wounds prevent you from sleeping.");
						}
						else
						{
							if ((p_ptr->prace == RACE_SPECTRE) ||
								 (p_ptr->prace == RACE_ZOMBIE) ||
								 (p_ptr->prace == RACE_SKELETON) ||
								 (p_ptr->prace == RACE_VAMPIRE))
							{
								room_rest(TRUE);
							}
							else
							{
								room_rest(FALSE);
							}
						}
					break;
				   }
			   case STORE_LIBRARY:
				   {
					   do_cmd_study();
						break;
					}
			   case STORE_INN:
					{
						if ((p_ptr->poisoned > 0) || (p_ptr->cut > 0))
						{
							msg_print("You need a healer, not a room!");
							msg_print("I'm sorry, but  I don't want anyone dying in here.");
						}
						else
						{
							if (!service_haggle(10,&price, service_name[cur_store_type][0], STORE_INN))
							{
								if (price > p_ptr->au)
								{
									msg_format("You do not have the gold!");
								}
								else
								{
									p_ptr->au -= price;
									/* Say "okay" */
									say_comment_1();
									/* Make a sound */
									sound(SOUND_BUY);
									/* Be happy */
									decrease_insults();
									store_prt_gold();
									if ((p_ptr->prace == RACE_SPECTRE) ||
										 (p_ptr->prace == RACE_ZOMBIE) ||
										 (p_ptr->prace == RACE_SKELETON) ||
										 (p_ptr->prace == RACE_VAMPIRE))
									{
										room_rest(TRUE);
									}
									else
									{
										room_rest(FALSE);
									}
								}
							}
						}
					break;
				   }
			   case STORE_HALL:
				   {
						store_type *h_ptr = find_house(cur_town);
					   if (h_ptr && h_ptr->bought)
					   {
						   msg_format("You already have the deeds!");
					   }
					   else if (!free_homes())
					   {
						   msg_format("Sorry, we have no houses on our books.");
					   }
					   else
					   {
							if (!service_haggle(town_defs[cur_town].house_price,&price, service_name[cur_store_type][0], STORE_HALL))
							{
								if (price > p_ptr->au)
								{
									msg_format("You do not have the gold!");
								}
								else
								{
									p_ptr->au -= price;
									/* Say "okay" */
									say_comment_1();
									/* Make a sound */
									sound(SOUND_BUY);
									/* Be happy */
									decrease_insults();
									store_prt_gold();
									h_ptr->bought = 1;
									msg_format("You may move in at once.");
								}
								p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
								handle_stuff();
							}
					   }
					break;
				   }
			   case STORE_PAWN:
				   {
						msg_print("That command does not work in this store.");
						break;
				   }
			   }
			   break;
		   }
		   case 'z': case 'a':   /* 'z' typed in turns to 'a' in roguelike keyset. */
			   switch(cur_store_type)
			   {
			   case STORE_MAGIC:
				   get_cantrip(&i,0);
				   msg_print("You learn some folk magic.");
				   break;
			   case STORE_TEMPLE:
					if (!get_spirit(&i,"Form a pact with",FALSE))
					{
						if (i == -2)
						{
							msg_print("There are no spirits that you can form a pact with.");
							break;
						}
					}
					else /* We got a spirit back */
					{
						sprintf(buf, "form a pact with %s", spirits[i].name);

						switch(spirits[i].favour_flags)
						{
						case 0x000000ff:
							if (!service_haggle(100,&price, buf, i + 128))
							{
								if (price > p_ptr->au)
								{
									msg_format("You do not have the gold!");
								}
								else
								{
									p_ptr->au -= price;
									/* Say "okay" */
									say_comment_1();
									/* Make a sound */
									sound(SOUND_BUY);
									/* Be happy */
									decrease_insults();
									store_prt_gold();
									spirits[i].pact=TRUE;
									dec_stat(A_CHR,10,TRUE);
									msg_print("You recieve scars during the ritual.");
								}
							}
							break;
						case 0x0000ff00:
							if (!service_haggle(1000,&price, buf, i + 128))
							{
								if (price > p_ptr->au)
								{
									msg_format("You do not have the gold!");
								}
								else
								{
									p_ptr->au -= price;
									/* Say "okay" */
									say_comment_1();
									/* Make a sound */
									sound(SOUND_BUY);
									/* Be happy */
									decrease_insults();
									store_prt_gold();
									dec_stat(A_CHR,10,TRUE);
									dec_stat(A_CON,10,TRUE);
									spirits[i].pact=TRUE;
									msg_print("You recieve scars during the ritual.");
									msg_print("The fasting damages your constitution.");
								}
							}
							break;
						   case 0x00ff0000:
								if (!service_haggle(5000,&price, buf, i + 128))
							{
								if (price > p_ptr->au)
								{
									msg_format("You do not have the gold!");
								}
								else
								{
									p_ptr->au -= price;
									/* Say "okay" */
									say_comment_1();
									/* Make a sound */
									sound(SOUND_BUY);
									/* Be happy */
									decrease_insults();
									store_prt_gold();
									dec_stat(A_CHR,10,TRUE);
									dec_stat(A_CON,10,TRUE);
									dec_stat(A_STR,10,TRUE);
								    spirits[i].pact=TRUE;
									msg_print("You recieve scars during the ritual.");
									msg_print("The fasting damages your constitution.");
									msg_print("The blood sacrifice saps your strength.");
								}
							}
							break;
					   case 0xff000000:
								if (!service_haggle(25000,&price, buf, i + 128))
							{
								if (price > p_ptr->au)
								{
									msg_format("You do not have the gold!");
								}
								else
								{
									p_ptr->au -= price;
									/* Say "okay" */
									say_comment_1();
									/* Make a sound */
									sound(SOUND_BUY);
									/* Be happy */
									decrease_insults();
									store_prt_gold();
									dec_stat(A_CHR,10,TRUE);
									dec_stat(A_CON,10,TRUE);
									dec_stat(A_STR,10,TRUE);
									dec_stat(A_DEX,10,TRUE);
								    spirits[i].pact=TRUE;
									msg_print("You recieve scars during the ritual.");
									msg_print("The fasting damages your constitution.");
									msg_print("The blood sacrifice saps your strength.");
									msg_print("The halucinagenic herbs give you a twitch.");
								}
							}
							break;
					   }
				   }
				   break;
			   default:
				   msg_print("That command does not work in this store.");
				   break;
			   }
			   break;
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
			do_cmd_browse(NULL);
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
			do_cmd_help(syshelpfile);
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
			do_cmd_change_name();
            display_store();
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
			do_cmd_options();
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
			do_cmd_feeling(FALSE);
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
			msg_print("That command does not work in stores.");
			break;
		}
	}
}

/*
 * Display store commands.
 */
static void display_store_extra(void)
{
 	char			buf[80];

	const int y = Term->hgt - 3;

	/* Basic commands */
	prt(" ESC) Exit from Building.", y+1, 0);

	/* Browse if necessary */
	if (st_ptr->stock_num > 12)
	{
		prt(" SPACE) Next page of stock", y+2, 0);
	}

	/* Home commands */
  	if (cur_store_type == STORE_HOME)
  	{
		prt(" g) Get an item.", y+1, 31);
		prt(" d) Drop an item.", y+2, 31);
	}

	/* Shop commands XXX XXX XXX */
	if ((cur_store_type != STORE_HOME) && (cur_store_type != STORE_HALL))
	{
		prt(" p) Purchase an item.", y+1, 31);
		prt(" s) Sell an item.", y+2, 31);
	}

	if (cur_store_type == STORE_HALL)
	{
		prt(" h) view racial Heroes.", y+1, 31);
		prt(" c) view Template heroes.", y+2,31);
	}
	else
	/* Add in the eXamine option */
	{
		prt(" x) eXamine an item.", y+1, 56);
	}

	/* Special for each store */

	if (service_name[cur_store_type][0][0])
	{
		sprintf(buf, " r) %s", service_name[cur_store_type][0]);
		prt(buf, y+2, 56);
	}
	if (service_name[cur_store_type][1][0])
	{
		sprintf(buf, " z) %s", service_name[cur_store_type][1]);
		prt(buf, y, 56);
	}
}

/*
 * Handle the resizing of the window with a shop display.
 */
static void resize_store(void)
{
	/* Display the store frame. */
	display_store();

	/* Display store inventory. */
	display_inventory();

	/* Display store commands. */
	display_store_extra();
}

/*
 * Enter a store, and interact with it.
 *
 * Note that we use the standard "request_command()" function
 * to get a command, allowing us to use "command_arg" and all
 * command macros and other nifty stuff, but we use the special
 * "shopping" argument, to force certain commands to be converted
 * into other commands, normally, we convert "p" (pray) and "m"
 * (cast magic) into "g" (get), and "s" (search) into "d" (drop).
 */
void do_cmd_store(void)
{
	int			which;

	int			tmp_chr = 0;

	cave_type		*c_ptr;

	/* Access the player grid */
	c_ptr = &cave[py][px];

	/* Verify a store */
	if (!((c_ptr->feat >= FEAT_SHOP_HEAD) &&
	      (c_ptr->feat <= FEAT_SHOP_TAIL)))
	{
		msg_print("You see no store here.");
		return;
	}

	/* Extract the store number */

	which = get_which_store();

	/* Paranoia - not a real shop. */
	if (which < 0)
	{
		msg_print("The shop is empty...");
		return;
	}

	/* Check for 'Ironman' option */

	if((ironman_shop) && (store[which].type != STORE_LIBRARY))
	{
		msg_print("Only wimps hide indoors!");
		return;
	}
	
	/* Hack -- Check the "locked doors" */
	if (store[which].store_open >= turn)
	{
		msg_print("The door is locked.");
		return;
	}

	/* Can't enter house unless you own it */
	if((store[which].type == STORE_HOME) && (store[which].bought == 0))
	{
		msg_print("The door is locked.");
		return;
	}

	/* Unless, of course, you're buying */
	if((store[which].type == STORE_HOME) && (store[which].bought == 1))
	{
		store[which].bought = 2;
		msg_print("You unlock your new house.");
		return;
	}


	/* Forget the lite */
	forget_lite();

	/* Forget the view */
	forget_view();
	
	/* Set the resize hook. */
	add_resize_hook(resize_store);

	/* Hack -- Character is in "icky" mode */
	character_icky = TRUE;


	/* No command argument */
	command_arg = 0;

	/* No repeated command */
	command_rep = 0;

	/* No automatic command */
	command_new = 0;


	/* Save the store number */
	cur_store_num = which;

	/* Save the store and owner pointers */
	st_ptr = &store[cur_store_num];
	cur_store_type=st_ptr->type;
	ot_ptr = &owners[st_ptr->owner];


	/* Start at the beginning */
	store_top = 0;

	/* Do not leave */
	leave_store = FALSE;

	/* Interact with player */
	while (!leave_store)
	{
		/* Clear */
        clear_from(21);
        

		/* Hack -- Check the charisma */
		if (tmp_chr != p_ptr->stat_use[A_CHR]) display_store();
		tmp_chr = p_ptr->stat_use[A_CHR];

		/* Display store inventory. */
		display_inventory();

		/* Display store commands. */
		display_store_extra();

  		/* Prompt */
  		put_str("You may: ", Term->hgt-3, 0);

		/* Get a command */
		request_command(TRUE);

		/* Process the command */
		store_process_command();

		/* Hack -- Character is still in "icky" mode */
		character_icky = TRUE;

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();

		/* XXX XXX XXX Pack Overflow */
		if (inventory[INVEN_PACK].k_idx)
		{
			int item = INVEN_PACK;			

			object_type *o_ptr = &inventory[item];

			/* Hack -- Flee from the store */
			if (cur_store_type != 7)
			{
				/* Message */
				msg_print("Your pack is so full that you flee the store...");

				/* Leave */
				leave_store = TRUE;
			}

			/* Hack -- Flee from the home */
			else if (!store_check_num(o_ptr))
			{
				/* Message */
				msg_print("Your pack is so full that you flee your home...");

				/* Leave */
				leave_store = TRUE;
			}

			/* Hack -- Drop items into the home */
			else
			{
				int item_pos;

				object_type forge;
				object_type *q_ptr;

				C_TNEW(o_name, ONAME_MAX, char);


				/* Give a message */
				msg_print("Your pack overflows!");

				/* Get local object */
				q_ptr = &forge;
	
				/* Grab a copy of the item */
				object_copy(q_ptr, o_ptr);

				/* Describe it */
				strnfmt(o_name, ONAME_MAX, "%v", object_desc_f3, q_ptr, TRUE, 3);

				/* Message */
				msg_format("You drop %s (%c).", o_name, index_to_label(o_ptr));

				/* Remove it from the players inventory */
				item_increase(o_ptr, -255);
				item_describe(o_ptr);
				item_optimize(o_ptr);

				/* Handle stuff */
				handle_stuff();

				/* Let the home carry it */
				item_pos = home_carry(q_ptr);

				/* Redraw the home */
				if (item_pos >= 0)
				{
					store_top = (item_pos / 12) * 12;
				}
				TFREE(o_name);
			}
		}

		/* Hack -- get kicked out of the store */
		if (st_ptr->store_open >= turn) leave_store = TRUE;
	}


	/* Free turn XXX XXX XXX */
	energy_use = 0;


	/* Hack -- Character is no longer in "icky" mode */
	character_icky = FALSE;


	/* Hack -- Cancel automatic command */
	command_new = 0;

	/* Hack -- Cancel "see" mode */
	command_see = FALSE;
	

	/* Flush messages XXX XXX XXX */
	msg_print(NULL);


	/* Clear the screen */
	Term_clear();

	/* Reset the resize hook. */
	delete_resize_hook(resize_store);

	/* Update everything */
	p_ptr->update |= (PU_VIEW | PU_LITE);
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw entire screen */
    p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_EQUIPPY);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}



/*
 * Select an owner for the current shop.
 */
static void choose_owner(void)
{
	int i,j;
	C_TNEW(o_table, NUM_OWNERS, int);

	for (i = j = 0; i < NUM_OWNERS; i++)
	{
		owner_type *ow_ptr = owners+i;

		/* Different owner. */
		if ((i != st_ptr->owner) &&

			/* Right type. */
			(ow_ptr->shop_type == store[cur_store_num].type) &&

		/* Right town. */
			(ow_ptr->town != cur_store_num/MAX_STORES_PER_TOWN ||
			ow_ptr->town != TOWN_NONE))
		{
			/* Count the owner. */
			o_table[j++] = i;
		}
	}

	/* Pick a shopkeeper, if any were found. */
	if (j)
	{
		st_ptr->owner = o_table[rand_int(j)];
	}
	/* No valid shopkeepers. */
	else
	{
		/* Give it a "valid" shopkeeper. */
		st_ptr->owner = 0;

		/* Warn the player, if desired. */
		if (alert_failure)
			msg_format("Shop %d has no (other?) valid shopkeepers.",
				cur_store_num);
	}

	/* Finished. */
	TFREE(o_table);
}

/*
 * Shuffle one of the stores.
 */
void store_shuffle(int which)
{
	int i;

	/* Hack - set the owner everywhere it is unset. */
	if (store[which].owner != -1)
	{
		/* Ignore home, hall and pawnbroker */
		if (store[which].type == 7 || store[which].type > 9) return;
	}

	/* Save the store index */
	cur_store_num = which;

	/* Activate that store */
	st_ptr = &store[cur_store_num];

	/* Paranoia - require a sensible owner type. */
	if (st_ptr->type >= MAX_STORE_TYPES && st_ptr->type != STORE_NONE)
	{
		quit_fmt("Shop %d has strange type %d.", which, st_ptr->type);
	}

	/* Pick a new owner */
	choose_owner();

	/* Activate the new owner */
	ot_ptr = &owners[st_ptr->owner];


	/* Reset the owner data */
	st_ptr->insult_cur = 0;
	st_ptr->store_open = 0;
	st_ptr->good_buy = 0;
	st_ptr->bad_buy = 0;


	/* Hack -- discount all the items */
	for (i = 0; i < st_ptr->stock_num; i++)
	{
		object_type *o_ptr;

		/* Get the item */
		o_ptr = &st_ptr->stock[i];

		/* Hack -- Sell all old items for "half price" */
        if (!(o_ptr->art_name))
            o_ptr->discount = 50;

		/* Hack -- Items are no longer "fixed price" */
		o_ptr->ident &= ~(IDENT_FIXED);
	}
}


/*
 * Maintain the inventory at the stores.
 */
void store_maint(int which)
{
	int         j;

	int		old_rating = rating;

	/* Ignore home */
	if ((store[which].type == STORE_HOME) || (store[which].type == STORE_HALL) || (store[which].type == STORE_PAWN)) return;

	
	/* Save the store indices */
	cur_store_num = which;
	cur_store_type = store[which].type;

	/* Activate that store */
	st_ptr = &store[cur_store_num];

	/* Activate the owner */
	ot_ptr = &owners[st_ptr->owner];


	/* Store keeper forgives the player */
	st_ptr->insult_cur = 0;


	/* Mega-Hack -- prune the black market */
	if (st_ptr->type == STORE_BLACK)
	{
		/* Destroy crappy black market items */
		for (j = st_ptr->stock_num - 1; j >= 0; j--)
		{
			object_type *o_ptr = &st_ptr->stock[j];

			/* Destroy crappy items */
			if (black_market_crap(o_ptr))
			{
				/* Destroy the item */
				store_item_increase(j, 0 - o_ptr->number);
				store_item_optimize(j);
			}
		}
	}

#ifdef MAINT_99_MAX
	/* Never keep more than 99 of something. */
	if (st_ptr->stock_num > 1)
	{
		for (j = 1; j < st_ptr->stock_num; j++)
		{
			/* Find slots which have the same contents as their predecessors. */
			if (store_object_similar(st_ptr->stock+j, st_ptr->stock+j-1))
			{
				/* Destroy the item */
				store_item_increase(j, -(st_ptr->stock[j].number));
				store_item_optimize(j);
			}
		}
	}
#endif /* MAINT_99_MAX */

	/* Choose the number of slots to keep */
	j = st_ptr->stock_num;

	/* Sell a few items */
	j = j - randint(STORE_TURNOVER);

	/* Never keep more than "STORE_MAX_KEEP" slots */
	if (j > STORE_MAX_KEEP) j = STORE_MAX_KEEP;

	/* Always "keep" at least "STORE_MIN_KEEP" items */
	if (j < STORE_MIN_KEEP) j = STORE_MIN_KEEP;

	/* Hack -- prevent "underflow" */
	if (j < 0) j = 0;

	/* Destroy objects until only "j" slots are left */
	while (st_ptr->stock_num > j) store_delete();


	/* Choose the number of slots to fill */
	j = st_ptr->stock_num;

	/* Buy some more items */
	j = j + randint(STORE_TURNOVER);

	/* Never keep more than "STORE_MAX_KEEP" slots */
	if (j > STORE_MAX_KEEP) j = STORE_MAX_KEEP;

	/* Always "keep" at least "STORE_MIN_KEEP" items */
	if (j < STORE_MIN_KEEP) j = STORE_MIN_KEEP;

	/* Hack -- prevent "overflow" */
	if (j >= st_ptr->stock_size) j = st_ptr->stock_size - 1;

	/* Hack -- Inn only has four possible items so use all four */
	if ((j > 4) && ( st_ptr->type == STORE_INN)) j=4;
	
	/* Acquire some new items */
	while (st_ptr->stock_num < j)
	{
		store_create();
	}


	/* Hack -- Restore the rating */
	rating = old_rating;
}


/*
 * Initialize the stores
 */
void store_init(int which)
{
	int         k;



	/* Save the store index */
	cur_store_num = which;

	/* work out the type - MEGAHACK! */
	cur_store_type=cur_store_num;
	k=0;
	while(cur_store_type >= MAX_STORES_PER_TOWN)
	{
		cur_store_type -= MAX_STORES_PER_TOWN;
		k=k+1;
	}
	/* but this is only an index, get the actual value... */
	cur_store_type = town_defs[k].store[cur_store_type];
	
	/* Activate that store */
	st_ptr = &store[cur_store_num];

	/* Tell the store what type it is */
	st_ptr->type = cur_store_type;

	/* Don't do much with non-existant stores. */
	if (cur_store_type == STORE_NONE) return;

	/* Pick an owner */
	st_ptr->owner = -1;
	store_shuffle(which);

	st_ptr->bought = 0;

	/* Activate the new owner */
	ot_ptr = &owners[st_ptr->owner];

	/* Initialize the store */
	st_ptr->store_open = 0;
	st_ptr->insult_cur = 0;
	st_ptr->good_buy = 0;
	st_ptr->bad_buy = 0;

	/* Nothing in stock */
	st_ptr->stock_num = 0;

	/* Clear any old items */
	for (k = 0; k < st_ptr->stock_size; k++)
	{
		object_wipe(&st_ptr->stock[k]);
	}
}


void move_to_black_market(object_type * o_ptr)
{
        st_ptr = &store[6];
        o_ptr->ident |= IDENT_STOREB;
        (void)store_carry(o_ptr);
        object_wipe(o_ptr); /* Don't leave a bogus object behind... */
}

static int get_which_store(void)
{
	int i;

	for(i= (MAX_STORES_PER_TOWN * cur_town); i < (MAX_STORES_PER_TOWN * (cur_town+1)); i++)
	{
		if((px == store[i].x) && (py == store[i].y))
		{
			return i;
		}
	}
	/* Should never get to here, but just in case... */
	return -1;
}

/*
 * Peruse the spells/prayers in a Book in the shop
 *
 * Note that *all* spells in the book are listed
 *
 */
static void do_store_browse( object_type *o_ptr)
{
	int		sval;
	int		spell = -1;
	int		num = 0;

	byte		spells[64];


	/* Access the item's sval */
	sval = k_info[o_ptr->k_idx].extra;

	/* Extract spells */
	for (spell = 0; spell < 32; spell++)
	{
		/* Check for this spell */
		if ((spell_flags[sval] & (1L << spell)))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}


	/* Save the screen */
	Term_save();

	/* Display the spells */
	print_spells(spells, num, 1, -1, (o_ptr->tval-90));

	/* Clear the top line */
	prt("", 0, 0);

	/* Prompt user */
	put_str("[Press any key to continue]", 0, 23);

	/* Wait for key */
	(void)inkey();

	/* Restore the screen */
	Term_load();
}
