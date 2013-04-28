/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* squelch.c: auto-squelcher */

#include "posband.h"


static int do_qual_squelch(void);


/*
 * This stores the various squelch levels for the secondary squelching.
 * It is currently hardcoded at 24 bytes, but since there are only 20
 * applicable tvals there shouldn't be a problem.
 */

byte squelch_level[SQUELCH_BYTES];
byte auto_destroy;
bool squelch_corpses;


/*
 * These are the base types for automatic squelching on creation.
 * I've combined some of the tvals to make this list a little more
 * reasonable.
 */

#define TYPE_AMMO    1
#define TYPE_BOW     2
#define TYPE_WEAPON1 3
#define TYPE_WEAPON2 4
#define TYPE_BODY    5
#define TYPE_CLOAK   6
#define TYPE_SHIELD  7
#define TYPE_HELM    8
#define TYPE_GLOVES  9
#define TYPE_BOOTS   10
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
	int        tval;
	cptr       desc;
} tval_desc;

static char head[4] =
{ 'a', 'A', '0', ':' };

/*
 * Here are the categories for squelch-on-creation.
 */
static tval_desc typevals[] =
{
  {TYPE_AMMO, "Missiles"},
  {TYPE_BOW, "Missile Launchers"},
  {TYPE_WEAPON1, "Weapons (Swords)"},
  {TYPE_WEAPON2, "Weapons (Non Swords)"},
  {TYPE_BODY, "Body Armor"},
  {TYPE_CLOAK, "Cloaks"},
  {TYPE_SHIELD, "Shields"},
  {TYPE_HELM, "Helmets"},
  {TYPE_GLOVES, "Gloves"},
  {TYPE_BOOTS, "Boots"},
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
 * This array is lifted (and edited_ from wizard2.c, hence
 * the spacy formatting.
 */

static tval_desc tvals[] =
{
	{ TV_SWORD,             "Sword"                },
	{ TV_POLEARM,           "Polearm"              },
	{ TV_HAFTED,            "Hafted Weapon"        },
	{ TV_BOW,               "Bow"                  },
	{ TV_ARROW,             "Arrows"               },
	{ TV_BOLT,              "Bolts"                },
	{ TV_SHOT,              "Shots"                },
	{ TV_SHIELD,            "Shield"               },
	{ TV_CROWN,             "Crown"                },
	{ TV_HELM,              "Helm"                 },
	{ TV_GLOVES,            "Gloves"               },
	{ TV_BOOTS,             "Boots"                },
	{ TV_CLOAK,             "Cloak"                },
	{ TV_DRAG_ARMOR,        "Dragon Scale Mail"    },
	{ TV_HARD_ARMOR,        "Hard Armor"           },
	{ TV_SOFT_ARMOR,        "Soft Armor"           },
	{ TV_DIGGING,           "Diggers"              },
	{ TV_RING,              "Rings"                },
	{ TV_AMULET,            "Amulets"              },
	{ TV_CHEST,             "Open Chests"		   },
	{0, NULL}
};

/*
 * This code is the heavy pseudoidentify code.  I lifted it
 * from dungeon.c.  It is used in the quality squelching code.
 */
/* dungeon.c function is used now (declared as extern) */

/*
 * Strip an "object name" into a buffer.  Lifted from wizard2.c.
 */
static void strip_name(char *buf, int k_idx)
{
	char *t;

	object_kind *k_ptr = &k_info[k_idx];

	cptr str = (k_name + k_ptr->name);


	/* Skip past leading characters */
	while ((*str == ' ') || (*str == '&')) str++;

	/* Copy useful chars */
	for (t = buf; *str; str++)
	{
		if (*str != '~') *t++ = *str;
	}

	/* Terminate the new name */
	*t = '\0';
}


/*
 * This subroutine actually handles the squelching menus.
 */

static int do_cmd_squelch_aux(void)
{
	int i, j, temp, num, max_num;
	int tval, sval, squelch;
	int col, row;
	int typeval;
	cptr tval_desc;
	char ch, sq;

	int choice[60];

	char ftmp[80];
	FILE *fff;
	char buf[1024];

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

	/* Me need to know the maximal possible tval_index */
	max_num = num;

	prt("Commands:", 3, 30);
	prt("[a-t]: Go to item squelching sub-menu.", 5, 30);
	prt("Q    : Go to quality squelching sub-menu*.", 6, 30);
	prt(format("A    : Toggle auto_destroy (Currently %s).",
		   (auto_destroy ? "ON" : "OFF")), 7, 30);
	prt("S    : Save squelch values to pref file.", 8, 30);
	prt("L    : Load squelch values from pref file.", 9, 30);
	prt("ESC  : Back to options menu.", 10, 30);
	prt(format("C    : Toggle monster corpse generation (%s).",
		   (!squelch_corpses ? "ON" : "OFF")), 11, 30);
	prt("     :*includes squelching opened chests.", 12, 30);

	/* Choose! */
	if (!get_com("Item Squelching Main Menu: ", &ch)) return (0);

	if (ch=='Q')
	{
	  	/* Switch to secondary squelching menu */
	  	do_qual_squelch();
	}

	/*toggle the auto-destroy*/
	else if (ch=='A')
	{
		auto_destroy = 1-auto_destroy;
	}
	
	/* Toggle squelch corpses */
	else if (ch == 'C')
	{
		squelch_corpses = 1 - squelch_corpses;
	}

	else if (ch=='S')
	{
	 	/* Prompt */
	  	prt("Command: Dump Squelch Info", 12, 30);

	  	/* Prompt */
	  	prt("File: ", 13, 30);

	  	/* Default filename */
	  	sprintf(ftmp, "%s.squ", op_ptr->base_name);

	  	/* Get a filename */
	  	if (askfor_aux(ftmp, 80))
	    {

	      	/* Build the filename */
	      	path_build(buf, 1024, ANGBAND_DIR_USER, ftmp);

	      	/* Drop priv's */
	      	safe_setuid_drop();

	      	/* Append to the file */
	      	fff = my_fopen(buf, "a");

	      	/* Grab priv's */
	      	safe_setuid_grab();

	      	/* Test for success */
	      	if (fff)
			{

		  		/* Skip some lines */
		  		fprintf(fff, "\n\n");

		  		/* Start dumping */
		  		fprintf(fff, "# Squelch bits\n\n");

		  		/* Dump squelch bits */
		  		for (i = 1; i < z_info->k_max; i++)
		 		{
		      		tval = k_info[i].tval;
		      		sval = k_info[i].sval;
		      		squelch = (k_info[i].squelch ? 1 : 0);

		     		 /* Dump the squelch info */
		      		if (tval || sval)
						fprintf(fff, "Q:%d:%d:%d:%d\n", i, tval, sval, squelch);
		 		 }

		 		 fprintf(fff, "\n\n# squelch_level array\n\n");

		  		 for(i = 0 ; i < SQUELCH_BYTES; i++)
			  		fprintf(fff, "Q:%d:%d\n", i, squelch_level[i]);

		  		 fprintf(fff, "\n\n# auto_destroy bit\n\n");

		  		 fprintf(fff, "Q:%d\n", auto_destroy);


		  		 /* All done */
		  		 fprintf(fff, "\n\n\n\n");

		  		 /* Close */
		  		 my_fclose(fff);

		  		 /* Ending message */
		  		 prt("Squelch file saved successfully.  (Hit a key.)", 15, 30);
		  		 get_com("", &sq);
			}

	    }
	}

	else if (ch=='L')
	{
	 	/* Prompt */
	 	prt("Command: Load squelch info from file", 12, 30);

	 	/* Prompt */
	 	prt("File: ", 13, 30);

	 	/* Default filename */
	 	sprintf(ftmp, "%s.squ", op_ptr->base_name);

	 	/* Ask for a file (or cancel) */
	 	if (askfor_aux(ftmp, 80))
	  	{
	    	/* Process the given filename */
	    	if (process_pref_file(ftmp))
	    	{
	    		/* Mention failure */
	      		prt("Failed to load squelch file!  (Hit a key.)", 15, 30);
	      		get_com("", &sq);
	    	}

	    	else
	    	{
	      		/* Mention success */
	      		prt("Squelch data loaded!  (Hit a key.)", 15, 30);
	      		get_com("", &sq);
	    	}
	  	}

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

	    	for (num = 0, i = 1; (num < 63) && (i < z_info->k_max); i++)
	      	{
				object_kind *k_ptr = &k_info[i];

				if (tv_to_type[k_ptr->tval] == typeval)
				{
		  			if (k_ptr->flags3 & (TR3_INSTA_ART)) continue;

					/*skip empty objects*/
					if (!k_ptr->name) continue;

					/*hack - sometimes gold shows up*/
					if (k_ptr->tval == TV_GOLD) continue;

					/*haven't seen the item yet*/
					if (!k_ptr->aware) continue;

		  			choice[num++] = i;
				}
	      	}

	    	max_num = num;

	    	/* Step 2: Simple bubble sort */
	    	for (i=0; i<max_num; i++)
		{
	      		for (j=i; j<max_num; j++)
			{
				if ((k_info[choice[i]].tval > k_info[choice[j]].tval) ||
		    		   ((k_info[choice[i]].tval == k_info[choice[j]].tval) &&
		     		    (k_info[choice[i]].cost > k_info[choice[j]].cost)))
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
		    		for (num = 0; num < max_num; num++)
		    		{
		    			object_kind *k_ptr = &k_info[choice[num]];

					k_ptr->everseen |= k_ptr->aware;

		      			/* Prepare it */
		      			row = 3 + (num % 20);
		      			col = 30 * (num / 20);
	      				ch = head[num/26] + (num%26);

	      				/* Acquire the "name" of object "i" */
	      				strip_name(buf, choice[num]);

		      			/* Get the squelch character */
		      			sq = (k_ptr->squelch ? '*' : ' ');

		      			/* Get the color */
		      			color = (k_ptr->squelch ?
				       		(k_ptr->aware ? TERM_RED : TERM_L_UMBER) :
				       		(k_ptr->aware ? TERM_L_GREEN : TERM_GREEN));

		      			/* Print it */
		      			prt(format("[%c%c] ", ch, sq), row, col);
		      			c_put_str(color, buf, row, col+5);
		    		}

	    			/* Print the legend */
		    		prt("'*': Squelch (ided)     '.': Squelch (not ided)     ' ': Allow generation", 1, 0);
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
	int i, num, max_num, index;
	int col, row;
	char ch;
	/* the index for the rings*/
	#define RING_INDEX  17
	/* the index for the amulets*/
	#define AMULET_INDEX  18
	/* - open chest TVAL in defines*/

	char squelch_str[6] = "NCVGAO";

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
	  	prt("O     : Squelch Chests After Opening", 9, 30);

	  	prt("Commands:", 11, 30);
	  	prt("Arrows: Move and adjust settings", 13, 30);
	  	prt("ncvgao : Change a single setting", 14, 30);
	  	prt("NCVGAO : Change all allowable settings", 15, 30);
	  	prt("ESC   : Exit Secondary Menu", 16, 30);
	  	prt("Secondary Squelching Menu", 0,0);
	  	prt("Rings:   N, C or A only", 19, 30);
	  	prt("Amulets: N, C or A only", 20, 30);
	  	prt("Opened Chests: N or O only", 21, 30);

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
	      			for (i=0; i < SQUELCH_BYTES; i++)
		  		{
					squelch_level[i] = SQUELCH_NONE;
		      		}
	      			break;
			}

		    	case 'c':
			{
		      		if (index != CHEST_INDEX) squelch_level[index] = SQUELCH_CURSED;
	      			break;
			}

		    	case 'C':
			{
		      		for (i = 0; i < SQUELCH_BYTES; i++)
			  	{
					/* Hack - don't check chests as cursed */
					if (i != CHEST_INDEX) squelch_level[i] = SQUELCH_CURSED;
	      	  		}
	      			break;
			}

		    	case 'v':
			{
		  		if ((index != CHEST_INDEX) && (index != AMULET_INDEX)
						&& (index != RING_INDEX)) squelch_level[index] = SQUELCH_AVERAGE;
	      			break;
			}

		    	case 'V':
			{
		      		for (i = 0; i < SQUELCH_BYTES ; i++)
				{
					if ((i != CHEST_INDEX) && (i != AMULET_INDEX)
						&& (i != RING_INDEX)) squelch_level[i] = SQUELCH_AVERAGE;
		      		}
	      			break;
			}

		    	case 'g':
			{
		      		if ((index != CHEST_INDEX) && (index != AMULET_INDEX)
					&& (index != RING_INDEX)) squelch_level[index] = SQUELCH_GOOD;
		      		break;
			}

		    	case 'G':
			{
	      			for (i = 0; i < SQUELCH_BYTES; i++)
				{
					if ((i != CHEST_INDEX) && (i != AMULET_INDEX)
						&& (i != RING_INDEX)) squelch_level[i] = SQUELCH_GOOD;
		      		}
		      		break;
			}

		    	case 'a':
			{
		      		if (index != CHEST_INDEX) squelch_level[index] = SQUELCH_ALL;
		      		break;
			}

	    		case 'A':
			{
	      			for (i = 0; i < SQUELCH_BYTES; i++)
			  	{
					/* Hack - don't check chests as destroy all */
					if (i != CHEST_INDEX) squelch_level[i] = SQUELCH_ALL;
		      	  	}
		      		break;
			}

			/* Hack "O" works from anywhere only on open chests */
			case 'O':
			case 'o':
			{
				squelch_level[(CHEST_INDEX)] = SQUELCH_OPENED_CHESTS;
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

		    	case '4':
		      	{
				/* Hack - only allowable options to be toggled through */

				/* First do the rings and amulets */
				if ((index == AMULET_INDEX) || (index == RING_INDEX))
				{
					/*amulets and rings can only be none, cursed, and all but artifact*/
					if (squelch_level[index] > 1) squelch_level[index] = SQUELCH_CURSED;
					else squelch_level[index] = SQUELCH_NONE;
					break;
				}

				/* now do the chests*/
				else if (index == CHEST_INDEX)
				{
					squelch_level[index] = SQUELCH_NONE;
					break;
				}

				/*then toggle all else*/
				else
				{
					if (squelch_level [index] >= SQUELCH_ALL) squelch_level [index] = SQUELCH_GOOD;
					else if (squelch_level [index] > 0)  squelch_level [index] -= 1;
					else squelch_level [index] = 0;
					break;
				}
		      	}

		    	case '6':
		      	{
				/* Hack - only allowable options to be toggled through */

				/* First do the rings and amulets */
				if ((index == AMULET_INDEX) || (index == RING_INDEX))
				{
					/*amulets and rings can only be none, cursed, and all but artifact*/
					if (squelch_level[index] > 0) squelch_level[index] = SQUELCH_ALL;
					else squelch_level[index] = SQUELCH_CURSED;
					break;
				}

				/* Now do the chests */
				else if (index == CHEST_INDEX)
				{
					squelch_level[index] = SQUELCH_OPENED_CHESTS;
					break;
				}

				/* Then toggle all else */
				else
				{
					if (squelch_level [index] >= SQUELCH_ALL) squelch_level [index] = SQUELCH_ALL;
					else squelch_level [index] += 1;
					break;
				}
		      	}

		    	default:
		    	{
				bell("");
				break;
		    	}
		}
	}
	return 0;
}


/*
 * Hack -- initialize the mapping from tvals to typevals.
 * This is currently called every time the squelch menus are
 * accessed.  This can certainly be improved.
 */
void init_tv_to_type(void)
{
	tv_to_type[TV_SKELETON] = TYPE_MISC;
	tv_to_type[TV_BOTTLE] = TYPE_MISC;
	tv_to_type[TV_JUNK] = TYPE_MISC;
	tv_to_type[TV_SPIKE] = TYPE_MISC;
	tv_to_type[TV_CHEST] = TYPE_MISC;
	tv_to_type[TV_SHOT] = TYPE_AMMO;
	tv_to_type[TV_ARROW] = TYPE_AMMO;
	tv_to_type[TV_BOLT] = TYPE_AMMO;
	tv_to_type[TV_BOW] = TYPE_BOW;
	tv_to_type[TV_DIGGING] = TYPE_WEAPON2;
	tv_to_type[TV_HAFTED] = TYPE_WEAPON2;
	tv_to_type[TV_POLEARM] = TYPE_WEAPON2;
	tv_to_type[TV_SWORD] = TYPE_WEAPON1;
	tv_to_type[TV_BOOTS] = TYPE_BOOTS;
	tv_to_type[TV_GLOVES] = TYPE_GLOVES;
	tv_to_type[TV_HELM] = TYPE_HELM;
	tv_to_type[TV_CROWN] = TYPE_HELM;
	tv_to_type[TV_SHIELD] = TYPE_SHIELD;
	tv_to_type[TV_CLOAK] = TYPE_CLOAK;
	tv_to_type[TV_SOFT_ARMOR] = TYPE_BODY;
	tv_to_type[TV_HARD_ARMOR] = TYPE_BODY;
	tv_to_type[TV_DRAG_ARMOR] = TYPE_BODY;
	tv_to_type[TV_LITE] = TYPE_MISC;
	tv_to_type[TV_AMULET] = TYPE_AMULET;
	tv_to_type[TV_RING] = TYPE_RING;
	tv_to_type[TV_STAFF] = TYPE_STAFF;
	tv_to_type[TV_WAND] = TYPE_WAND;
	tv_to_type[TV_ROD] = TYPE_ROD;
	tv_to_type[TV_SCROLL] = TYPE_SCROLL;
	tv_to_type[TV_POTION] = TYPE_POTION;
	tv_to_type[TV_FLASK] = TYPE_MISC;
	tv_to_type[TV_FOOD] = TYPE_FOOD;
	tv_to_type[TV_MAGIC_BOOK] = TYPE_BOOK;
	tv_to_type[TV_PRAYER_BOOK] = TYPE_BOOK;
}

void do_cmd_squelch(void)
{
	int flag;
	int x, y;
	init_tv_to_type();

	flag=1;

	/* Simple loop */
	while (flag)
	{
		flag = do_cmd_squelch_aux();
	}

	/* Rearrange all the stacks to reflect squelch menus were touched. */
	for(x = 0; x < p_ptr->cur_map_wid; x++)
	{
	  	for(y = 0; y < p_ptr->cur_map_hgt; y++)
		{
		    	rearrange_stack(y, x);
		}
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
 *            pseudoidentified or 0 if the object is being identified.
 *  fullid  : This is 1 if the object is being identified and 0 otherwise.
 *
 * Output: One of the three above values.
 */

int squelch_itemp(object_type *o_ptr, byte feeling, int fullid)
{
  	int i, num, result;
  	byte feel;

  	/* Default */
  	result = SQUELCH_NO;

  	/*n Nver squelch quest items */
  	if (o_ptr->ident & IDENT_QUEST) return result;

	/* Never squelch any artifacts */
	if (o_ptr->name1 || o_ptr->name3 || o_ptr->rart_name) return result;

  	/* Check to see if the object is eligible for squelching on id. */
  	num = -1;

	/* Find the appropriate squelch group */
  	for (i = 0; tvals[i].tval; i++)
  	{
  		if (tvals[i].tval == o_ptr->tval)
		{
	      		num = i;
		}
  	}

	/* Never squelched */
  	if (num == -1) return result;

  	/*
   	 * Get the "feeling" of the object.  If the object is being identified
   	 * get the feeling returned by a heavy pseudoid.
   	 */
  	feel = feeling;

  	if (fullid == 1)  feel = value_check_aux1(o_ptr);

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
	      		result = (((feel==INSCRIP_BROKEN) ||
			 	   (feel==INSCRIP_TERRIBLE) ||
			 	   (feel==INSCRIP_WORTHLESS) ||
			 	   (feel==INSCRIP_CURSED)) ? SQUELCH_YES : SQUELCH_NO);
      			break;
		}

   		case SQUELCH_AVERAGE:
		{
	     		result = (((feel==INSCRIP_BROKEN) ||
			 	   (feel==INSCRIP_TERRIBLE) ||
			 	   (feel==INSCRIP_WORTHLESS) ||
			 	   (feel==INSCRIP_CURSED) ||
			 	   (feel==INSCRIP_AVERAGE)) ? SQUELCH_YES : SQUELCH_NO);
      			break;
		}

	    	case SQUELCH_GOOD:
		{
	      		result = (((feel==INSCRIP_BROKEN) ||
			 	   (feel==INSCRIP_TERRIBLE) ||
			 	   (feel==INSCRIP_WORTHLESS) ||
		 		   (feel==INSCRIP_CURSED) ||
			 	   (feel==INSCRIP_AVERAGE) ||
			 	   (feel==INSCRIP_GOOD)) ? SQUELCH_YES : SQUELCH_NO);
     			 break;
		}

	    	case SQUELCH_ALL:
		{
      			result = SQUELCH_YES;
	      		break;
		}
	}


  	if (result==SQUELCH_NO) return result;

  	/* Squelching will fail on an artifact */
  	if ((artifact_p(o_ptr)) || (o_ptr->note)) result = SQUELCH_FAILED;

  	return result;
}


/*
 * This performs the squelch, actually removing the item from the
 * game.  It returns 1 if the item was squelched, and 0 otherwise.
 * This return value is never actually used.
 */
int do_squelch_item(int squelch, int item, object_type *o_ptr)
{

  	if (squelch != SQUELCH_YES) return 0;

	/* Hack - never squelch quest items */
	if (o_ptr->ident & IDENT_QUEST) return 0;

  	if (item > 0)
	{
	    	inven_item_increase(item, -o_ptr->number);
    		inven_item_optimize(item);
  	}
  	else
	{
	    	floor_item_increase(0 - item, -o_ptr->number);
    		floor_item_optimize(0 - item);
  	}

  	return 1;
}

void rearrange_stack(int y, int x)
{
	s16b o_idx, next_o_idx;
	s16b first_bad_idx, first_good_idx, cur_bad_idx, cur_good_idx;

	object_type *o_ptr;

	bool sq_flag=FALSE;

	first_bad_idx = 0;
	first_good_idx = 0;
	cur_bad_idx = 0;
	cur_good_idx = 0;

	for(o_idx = cave_o_idx[y][x]; o_idx; o_idx = next_o_idx)
	{
	    	o_ptr = &(o_list[o_idx]);
	    	next_o_idx = o_ptr->next_o_idx;
    		sq_flag=(k_info[o_ptr->k_idx].squelch & k_info[o_ptr->k_idx].aware);

	    	if (sq_flag)
		{
      			if (first_bad_idx == 0)
			{
				first_bad_idx = o_idx;
				cur_bad_idx = o_idx;
	      		}
			else
			{
				o_list[cur_bad_idx].next_o_idx = o_idx;
				cur_bad_idx = o_idx;
      			}
	    	}
		else
		{
      			if (first_good_idx==0)
			{
				first_good_idx = o_idx;
				cur_good_idx = o_idx;
			}
			else
			{
				o_list[cur_good_idx].next_o_idx = o_idx;
				cur_good_idx = o_idx;
	      		}
	    	}
  	}

  	if (first_good_idx != 0)
	{
	    	cave_o_idx[y][x] = first_good_idx;
	    	o_list[cur_good_idx].next_o_idx = first_bad_idx;
	    	o_list[cur_bad_idx].next_o_idx = 0;
  	}
	else
	{
	    	cave_o_idx[y][x] = first_bad_idx;
  	}
}



void do_squelch_pile(int y, int x)
{

  	s16b o_idx, next_o_idx;
  	object_type *o_ptr;
  	bool sq_flag=FALSE;

	for(o_idx = cave_o_idx[y][x]; o_idx; o_idx = next_o_idx)
	{
	    	o_ptr = &(o_list[o_idx]);

    		next_o_idx = o_ptr->next_o_idx;

	    	sq_flag = (k_info[o_ptr->k_idx].squelch & k_info[o_ptr->k_idx].aware);
	
		/* Never squelch any artifacts */
	    	sq_flag &= !artifact_p(o_ptr) && !o_ptr->name3 && !o_ptr->rart_name;

		/* Always squelch "&nothing" */
		if (!o_ptr->k_idx) sq_flag = TRUE;

		/* Never delete quest items */
	    	if ((sq_flag) && (!(o_ptr->ident & IDENT_QUEST)))
		{
      			delete_object_idx(o_idx);
		}
  	}
}




