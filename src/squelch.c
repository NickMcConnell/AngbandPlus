/* File: squelch.c */

/* Item squelching support.  Setting squelch parameters for both squelch
 * on generate and squelch on identify.   Handling squelching menus.
 * Determining if items should be squelched.  Actually squelching items.
 *
 * Copyright (c) 2000 David Blackston and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */


#include "angband.h"

static int do_qual_squelch(void);


/*
 * This stores the various squelch levels for the secondary squelching.
 * It is currently hardcoded at 24 bytes, but since there are only 16
 * applicable tvals there shouldn't be a problem.
 */

byte squelch_level[24];

/*
 * These are the various levels of squelching supported by the game.
 * Less concisely:
 * 0 ---> No squelching
 * 1 ---> Squelch cursed items
 * 2 ---> Squelch average and worse items
 * 3 ---> Squelch good and worse items
 * 4 ---> squelch all but artifacts
 */

#define SQUELCH_NONE	 0
#define SQUELCH_CURSED	 1
#define SQUELCH_AVERAGE	 2
#define SQUELCH_GOOD	 3
#define SQUELCH_ALL	 4



/*
 * These are the base types for automatic squelching on creation.
 * I've combined some of the tvals to make this list a little more
 * reasonable.
 */

#define TYPE_RING    11
#define TYPE_STAFF   12
#define TYPE_WAND    13
#define TYPE_ROD     14
#define TYPE_SCROLL  15
#define TYPE_POTION  16
#define TYPE_AMULET  17
#define TYPE_BOOK    18
#define TYPE_FOOD    19
#define TYPE_MISC    20

/*
 * This (admittedly hacky) stores the mapping from tval to typeval
 * and is reinitialized every time do_cmd_squelch is called.  This
 * can certainly be done more cleanly.
 */
static int tv_to_type[100];

/*
 * These structures are lifted from wizard2.c where they were used in
 * the 'create item' command.  I have adapted them for my code.
 */

typedef struct tval_desc
{
	int	   tval;
	cptr	   desc;
} tval_desc;

static char head[4] =
{ 'a', 'A', '0', ':' };

/*
 * Here are the categories for squelch-on-creation.
 */
static tval_desc typevals[] =
{

	{TYPE_AMULET, "Amulets"},
	{TYPE_RING, "Rings"},
	{TYPE_STAFF, "Staves"},
	{TYPE_WAND, "Wands"},
	{TYPE_ROD, "Rods"},
	{TYPE_SCROLL, "Scrolls"},
	{TYPE_POTION, "Potions"},
	{TYPE_BOOK, "Magic Books"},
	{TYPE_FOOD, "Food Items"},
	{TYPE_MISC, "Miscellaneous"},
	{0, NULL}

};

/*
 * Here are the categories for squelch-on-identification.
 * This array is lifted (and edited) from wizard2.c, hence
 * the spacy formatting.
 */

static tval_desc tvals[] =
{
	{ TV_SWORD,		"Sword"                },
	{ TV_POLEARM,		"Polearm"              },
	{ TV_HAFTED,		"Hafted Weapon"        },
	{ TV_BOW,		"Bow"                  },
	{ TV_ARROW,		"Arrows"               },
	{ TV_BOLT,		"Bolts"                },
	{ TV_SHOT,		"Shots"		       },
	{ TV_SHIELD,		"Shield"	       },
	{ TV_CROWN,		"Crown"		       },
	{ TV_HELM,		"Helm"		       },
	{ TV_GLOVES,		"Gloves"	       },
	{ TV_BOOTS,		"Boots"		       },
	{ TV_CLOAK,		"Cloak"		       },
	{ TV_DRAG_ARMOR,	"Dragon Scale Mail"    },
	{ TV_HARD_ARMOR,	"Hard Armor"           },
	{ TV_SOFT_ARMOR,	"Soft Armor"           },
	{ TV_DIGGING,	        "Tools"                },
	{ 0,			NULL }
};

/*
 * This code is the heavy pseudoidentify code.  I lifted it
 * from dungeon.c.  It is used in the quality squelching code.
 */

/*
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
static int value_check_aux1(object_type *o_ptr)
{
	/* Artifacts */
	if (artifact_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (FEEL_TERRIBLE);

		/* Normal */
		return (FEEL_SPECIAL);
	}

	/* Ego-Items */
	if (ego_item_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (FEEL_WORTHLESS);

		/* Normal */
		return (FEEL_EXCELLENT);
	}

	/* Cursed items */
	if (cursed_p(o_ptr)) return (FEEL_CURSED);

	/* Broken items */
	if (broken_p(o_ptr)) return (FEEL_BROKEN);

	/* Good "armor" bonus */
	if (o_ptr->to_a > 0) return (FEEL_GOOD);

	/* Good "weapon" bonus */
	if (o_ptr->to_h + o_ptr->to_d > 0) return (FEEL_GOOD);

	/* Default to "average" */
	return (FEEL_AVERAGE);
}

/*
 * This subroutine actually handles the squelching menus.
 */

static int do_cmd_squelch_aux(void)
{
	int i, j, temp, num, max_num;
	int col, row;
	int typeval;
	cptr tval_desc;
	char ch, sq;

	int choice[60];

	char buf[160];
	byte color;

	/* Clear screen */
	Term_clear();

	/*
	 * Print all typeval's and their descriptions
	 *
	 * This uses the above arrays.  I combined a few of the
	 * tvals into single typevals.
	 */

	for (num = 0; (num<60) && typevals[num].tval; num++)
	{
		row = 3 + (num % 20);
		col = 30 * (num / 20);
		ch = head[num/26] +num%26;
		prt(format("[%c] %s", ch, typevals[num].desc), row, col);
	}

	/* We need to know the maximal possible tval_index */
	max_num = num;

	/* Choose! */
	if (!get_com("Squelch what type of object? (Q: Secondary Menu for Weapons and Armour) ", &ch)) return (0);

	if (ch=='Q')
	{
		/* Switch to secondary squelching menu */
		do_qual_squelch();
	}
	else
	{

		/* Analyze choice */
		num = ch-'a';

		/* Bail out if choice is illegal */
		if ((num < 0) || (num >= max_num)) return (0);

		/* Base object type chosen, fill in tval */
		typeval = typevals[num].tval;
		tval_desc = typevals[num].desc;


		/*** And now we go for k_idx ***/

		/* Clear screen */

		while (1)
		{
			Term_clear();

			/* First sort based on value */
			/* Step 1: Read into choice array */

			for (num = 0, i = 1; (num < 60) && (i < MAX_K_IDX); i++)
			{
				object_kind *k_ptr = &k_info[i];

				if (tv_to_type[k_ptr->tval] == typeval)
				{
					if (k_ptr->flags3 & (TR3_INSTA_ART)) continue;
					if (!(k_ptr->aware)) continue;
					choice[num++] = i;
				}
			}

			max_num = num;

			/* Step 2: Simple bubble sort */
			for (i=0; i<max_num; i++)
			{
				for (j=i; j<max_num; j++)
				{
					if ((k_info[choice[i]].tval>k_info[choice[j]].tval) ||
					    ((k_info[choice[i]].tval==k_info[choice[j]].tval) &&
					     (k_info[choice[i]].cost>k_info[choice[j]].cost)))
					{
						temp = choice[i];
						choice[i] = choice[j];
						choice[j] = temp;
					}
				}
			}

			if (!max_num) c_put_str(TERM_RED, "No known objects of this type.", 3, 0);

			else
			{
				for (num = 0; num<max_num; num++)
				{
					object_kind *k_ptr = &k_info[choice[num]];

					/* Prepare it */
					row = 3 + (num % 20);
					col = 30 * (num / 20);
					ch = head[num/26] + (num%26);

					/* Acquire the "name" of object "i" */
					strip_name(buf, choice[num]);

					/* Get the squelch character */
					sq = (k_ptr->squelch ? '*' : ' ');

					/* Get the color */
					color = (k_ptr->squelch ? TERM_RED : TERM_L_GREEN);

					/* Print it */
					prt(format("[%c%c] ", ch, sq), row, col);
					c_put_str(color, buf, row, col+5);
				}

				/* Print the legend */
				prt("'*': Squelch           ' ': Do not squelch", 1, 0);

			}

			/* Choose! */
			if (!get_com(format("%s : Command? (^A: Squelch all   ^U: Unsquelch all)", tval_desc), &ch)) return (1);

			if (ch==KTRL('A'))
			{
				/* ^A --> Squelch all items */
				for (i=0; i<max_num; i++)
				{
					k_info[choice[i]].squelch = TRUE;
				}
			}
			else if (ch==KTRL('U'))
			{
				/* ^U --> Unsquelch all items */
				for (i=0; i<max_num; i++)
				{
					k_info[choice[i]].squelch = FALSE;
				}
			}
			else
			{
				/* Analyze choice */
				num = -1;
				if ((ch >= head[0]) && (ch < head[0] + 26)) num = ch - head[0];
				if ((ch >= head[1]) && (ch < head[1] + 26)) num = ch - head[1] + 26;
				if ((ch >= head[2]) && (ch < head[2] + 17)) num = ch - head[2] + 52;

				/* Bail out if choice is "illegal" */
				if ((num < 0) || (num >= max_num)) return (1);

				/* Toggle */
				k_info[choice[num]].squelch =
				  (k_info[choice[num]].squelch ? FALSE : TRUE);
			}
		}
	}
	/* And return successful */
	return (1);
}

/*
 * This command handles the secondary squelch menu.
 */

static int do_qual_squelch(void)
{
	int i, index;
	int col, row;
	char ch;

	char squelch_str[5] = "NCVGA";

	int num=0;
	int max_num=0;

	index = 0;

	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Print all tval's and their descriptions */
		for (num = 0; (num<60) && tvals[num].tval; num++)
		{
			row = 2 + (num % 20);
			col = 30 * (num / 20);
			prt(format("(%c): %s", squelch_str[squelch_level[num]], tvals[num].desc), row, col);
		}

		/* Print out the rest of the screen */
		prt("Legend:", 2, 30);
		prt("N     : Squelch Nothing", 4, 30);
		prt("C     : Squelch Cursed Items", 5, 30);
		prt("V     : Squelch Average and Below", 6, 30);
		prt("G     : Squelch Good and Below", 7, 30);
		prt("A     : Squelch All but Artifacts", 8, 30);

		prt("Commands:", 11, 30);
		prt("Arrows: Move and adjust settings", 13, 30);
		prt("ncvga : Change a single setting", 14, 30);
		prt("NCVGA : Change all settings", 15, 30);
		prt("ESC   : Exit Secondary Menu", 17, 30);
		prt("Secondary Squelching Menu: Weapons/Armor (Squelch only after identify)", 0,0);

		/* Need to know maximum index */
		max_num=num;

		/* Place the cursor */
		move_cursor(index+ 2, 1);

		/* Get a key */
		ch = inkey();

		/* Analyze */
		switch (ch)
		{
			case ESCAPE:
			{
				return 0;
			}

			case 'n':
			{
				squelch_level[index] = SQUELCH_NONE;
				break;
			}

			case 'N':
			{
				for (i=0; i<24; i++)
				{
					squelch_level[i] = SQUELCH_NONE;
				}
				break;
			}

			case 'c':
			{
				squelch_level[index] = SQUELCH_CURSED;
				break;
			}

			case 'C':
			{
				for (i=0; i<24; i++)
				{
					squelch_level[i] = SQUELCH_CURSED;
				}
				break;
			}

			case 'v':
			{
				squelch_level[index] = SQUELCH_AVERAGE;
				break;
			}

			case 'V':
			{
				for (i=0; i<24; i++)
				{
					squelch_level[i] = SQUELCH_AVERAGE;
				}
				break;
			}

			case 'g':
			{
				squelch_level[index] = SQUELCH_GOOD;
				break;
			}

			case 'G':
			{
				for (i=0; i<24; i++)
				{
					squelch_level[i] = SQUELCH_GOOD;
				}
				break;
			}

			case 'a':
			{
				squelch_level[index] = SQUELCH_ALL;
				break;
			}

			case 'A':
			{
				for (i=0; i<24; i++)
				{
					squelch_level[i] = SQUELCH_ALL;
				}
				break;
			}

			case '-':
			case '8':
			{
				index = (max_num + index - 1) % max_num;
				break;
			}

			case ' ':
			case '\n':
			case '\r':
			case '2':
			{
				index = (index + 1) % max_num;
				break;
			}

			case '6':
			{
				squelch_level[index] = (squelch_level[index]+1)%(SQUELCH_ALL+1);
				break;
			}

			case '4':
			{
				squelch_level[index] = (SQUELCH_ALL+squelch_level[index])%(SQUELCH_ALL+1);
				break;
			}

			default:
			{
				bell("");
				break;
			}
		}

	}

}

/*
 * Hack -- initialize the mapping from tvals to typevals.
 * This is currently called every time the squelch menus are
 * accessed.  This can certainly be improved.
 */

void init_tv_to_type(void)
{
	tv_to_type[TV_SKELETON]=TYPE_MISC;
	tv_to_type[TV_BOTTLE]=TYPE_MISC;
	tv_to_type[TV_JUNK]=TYPE_MISC;
	tv_to_type[TV_SPIKE]=TYPE_MISC;
	tv_to_type[TV_CHEST]=TYPE_MISC;
	tv_to_type[TV_LITE]=TYPE_MISC;
	tv_to_type[TV_AMULET]=TYPE_AMULET;
	tv_to_type[TV_RING]=TYPE_RING;
	tv_to_type[TV_STAFF]=TYPE_STAFF;
	tv_to_type[TV_WAND]=TYPE_WAND;
	tv_to_type[TV_ROD]=TYPE_ROD;
	tv_to_type[TV_SCROLL]=TYPE_SCROLL;
	tv_to_type[TV_POTION]=TYPE_POTION;
	tv_to_type[TV_FLASK]=TYPE_MISC;
	tv_to_type[TV_FOOD]=TYPE_FOOD;
	tv_to_type[TV_MAGIC_BOOK]=TYPE_BOOK;
	tv_to_type[TV_PRAYER_BOOK]=TYPE_BOOK;
	tv_to_type[TV_DRUID_BOOK]=TYPE_BOOK;
	tv_to_type[TV_NECRO_BOOK]=TYPE_BOOK;
}

void do_cmd_squelch(void)
{

	int flag;
	init_tv_to_type();


	flag=1;

	/* Simple loop */
	while (flag)
	{
		flag = do_cmd_squelch_aux();
	}

	/* Restore the screen */
	Term_load();

	return;
}

/*
 * These are the return values of squelch_itemp()
 */

#define SQUELCH_FAILED -1
#define SQUELCH_NO      0
#define SQUELCH_YES     1

/*
 * Determines if an object is going to be squelched on identification.
 * Input:
 *  o_ptr   : This is a pointer to the object type being identified.
 *  feeling : This is the feeling of the object if it is being
 *	      pseudoidentified or 0 if the object is being identified.
 *  fullid  : This is 1 if the object is being identified and 0 otherwise.
 *
 * Output: One of the three above values.
 */

int squelch_itemp(object_type *o_ptr, byte feeling, int fullid)
{
	int i, num, result;
	byte feel;

	/* default */
	result = SQUELCH_NO;


	/* Check to see if the object is eligible for squelching on id. */
	num=-1;
	for (i=0; tvals[i].tval; i++)
	{
		if (tvals[i].tval==o_ptr->tval)
		{
			num=i;
		}
	}
	if (num==-1) return result;

	/*
	 * Get the "feeling" of the object.  If the object is being identified
	 * get the feeling returned by a heavy pseudoid.
	 */
	feel = feeling;
	if (fullid==1)
	  feel = value_check_aux1(o_ptr);

	/* Get result based on the feeling and the squelch_level */
	switch (squelch_level[num])
	{
		case SQUELCH_NONE:
		{
			return result;
			break;
		}

		case SQUELCH_CURSED:
		{
			result = (((feel==FEEL_BROKEN) ||
				   (feel==FEEL_TERRIBLE) ||
				   (feel==FEEL_WORTHLESS) ||
				   (feel==FEEL_CURSED)) ? SQUELCH_YES : SQUELCH_NO);
			break;
		}

		case SQUELCH_AVERAGE:
		{
			result = (((feel==FEEL_BROKEN) ||
				   (feel==FEEL_TERRIBLE) ||
				   (feel==FEEL_WORTHLESS) ||
				   (feel==FEEL_CURSED) ||
				   (feel==FEEL_AVERAGE)) ? SQUELCH_YES : SQUELCH_NO);
			break;
		}

		case SQUELCH_GOOD:
		{
			result = (((feel==FEEL_BROKEN) ||
				   (feel==FEEL_TERRIBLE) ||
				   (feel==FEEL_WORTHLESS) ||
				   (feel==FEEL_CURSED) ||
				   (feel==FEEL_AVERAGE) ||
				   (feel==FEEL_GOOD)) ? SQUELCH_YES : SQUELCH_NO);
			break;
		}

		case SQUELCH_ALL:
		{
			if (!(artifact_p(o_ptr))) result = SQUELCH_YES;
			break;
		}
	}


	if (result==SQUELCH_NO) return result;

	/* Extra Paranoia */
	if (artifact_p(o_ptr))
	  result = SQUELCH_FAILED;

	/* Items inscribed with '!k' or '!*' are note squelchable. */
	if (o_ptr->note)
	{
	        cptr s = strchr(quark_str(o_ptr->note), '!');
		while (s)
		{
		        if ((s[1] == 'k') || (s[1] == '*')) result = SQUELCH_FAILED;
			s = strchr(s + 1, '!');
		}
	}

	return result;
}

/*
 * The "Squelch on walk-on" function.
 */
void do_squelch_pile(int y, int x)
{
	s16b o_idx, next_o_idx;
	object_type *o_ptr;
	bool sq_flag = FALSE;

	for (o_idx = cave_o_idx[y][x]; o_idx; o_idx = next_o_idx)
	{

		o_ptr = &(o_list[o_idx]);

		next_o_idx = o_ptr->next_o_idx;

		/* Always squelch "&nothing" */
		if (!o_ptr->k_idx) sq_flag = TRUE;

		/* Hack - never squelch artifacts */
		else if (artifact_p(o_ptr)) sq_flag = FALSE;

		/* Squelch it? */
		else sq_flag = (k_info[o_ptr->k_idx].squelch & k_info[o_ptr->k_idx].aware);

		/* Unwanted and unloved */
		if (sq_flag)
		{

			/* Actual Squelch */
			if (strong_squelch) delete_object_idx(o_idx);

			/* Or inscription */
			else o_ptr->note = quark_add("SQUELCH");
		}
	}
}

/*
 * Squelch on identify function
 */
void do_squelch_item(int item, object_type *o_ptr)
{
	/* Either delete the item... */
	if (strong_squelch)
	{
		if (item >= 0)
		{
			inven_item_increase(item, -o_ptr->number);
			inven_item_optimize(item);
		}
		else
		{
			floor_item_increase(0 - item, -o_ptr->number);
			floor_item_optimize(0 - item);
		}
	}

	/* ...or mark it for manual deletion. */
	else
	{
		o_ptr->note = 0;
		o_ptr->note = quark_add("SQUELCH");
	}

	return;
}





