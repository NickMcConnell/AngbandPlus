
#include "angband.h"

static void do_qual_squelch(void);
static int do_ego_item_squelch(void);


/*
 * This stores the various squelch levels for the secondary squelching.
 * It is currently hardcoded at 24 bytes, but since there are only 20
 * applicable tvals there shouldn't be a problem.
 */

byte squelch_level[SQUELCH_BYTES];

#define LINES_PER_COLUMN   19

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
	{ TV_DRAG_SHIELD,       "Dragon Scale Shields" },
	{ TV_HARD_ARMOR,        "Hard Armor"           },
	{ TV_SOFT_ARMOR,        "Soft Armor"           },
	{ TV_DIGGING,           "Diggers"              },
	{ TV_RING,              "Rings"                },
	{ TV_AMULET,            "Amulets"              },
	{ TV_CHEST,             "Open Chests"		   },
	{ TV_LITE, 				"Lite Sources"		   },
	{0, NULL}
};

/*
 * This subroutine actually handles the squelching menus.
 */

static int do_cmd_squelch_aux(void)
{
	int i, j, temp, num, max_num;
	int tval, sval, squelch;
	int col, row;
	int typeval;
	cptr tval_desc2;
	char ch, sq;

	int choice[60];

	char ftmp[80];
	FILE *fff;
	char buf[80];

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
	prt("E    : Go to ego-item squelching sub_menu.", 7, 30);
	prt("S    : Save squelch values to pref file.", 8, 30);
	prt("L    : Load squelch values from pref file.", 9, 30);
	prt("ESC  : Back to options menu.", 10, 30);
	prt("     :*includes squelching opened chests.", 12, 30);

	/* Choose! */
	if (!get_com("Item Squelching Main Menu: ", &ch)) return (0);

	if (ch=='Q')
	{
	  	/* Switch to secondary squelching menu */
	  	do_qual_squelch();
	}

	else if (ch=='E')
	{
		/* Switch to ego-item squelching menu */
		do_ego_item_squelch();
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
		      		squelch = (k_info[i].squelch);

		     		 /* Dump the squelch info */
		      		if (tval || sval)
						fprintf(fff, "Q:%d:%d:%d:%d\n", i, tval, sval, squelch);
		 		 }

		 		 fprintf(fff, "\n\n# squelch_level array\n\n");

		  		 for(i = 0 ; i < SQUELCH_BYTES; i++)
			  		fprintf(fff, "Q:%d:%d\n", i, squelch_level[i]);

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
		int active = 0;

		/*
		 * One variable is enough, but I used two to make
	     * the code more readable -DG
  		 */
		int old_active = -1;
		int display_all = 1;

		/* Analyze choice */
		num = ch - 'a';

		/* Bail out if choice is illegal */
		if ((num < 0) || (num >= max_num)) return (0);

		/* Base object type chosen, fill in tval */
		typeval = typevals[num].tval;
		tval_desc2 = typevals[num].desc;

		/* Moved out the sorting code of the while loop */

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
		for (i = 0; i < max_num; i++)
		{
			for (j = i; j < max_num; j++)
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


		/*** And now we go for k_idx ***/

		/* Clear screen */

		while (TRUE)
		{
			if (display_all)
			Term_clear();

			/*no objects found*/
			if (!max_num)
			{
				if (display_all)
				c_put_str(TERM_RED, "No known objects of this type.", 5, 0);
			}

			else
			{

				byte color;

				for (num = 0; num < max_num; num++)
				{
					object_kind *k_ptr = &k_info[choice[num]];

					k_ptr->everseen |= k_ptr->aware;

					/* Reduce flickering */
					if (!display_all && num != active && num != old_active)
					continue;

					/* Prepare it */
					row = 5 + (num % LINES_PER_COLUMN);
					col = 30 * (num / LINES_PER_COLUMN);
					ch = head[num / 26] + (num % 26);

					/* Acquire the "name" of object "i" */
					strip_name(buf, choice[num]);


					/*get the color and character*/

					/*
					 * Items player wants to squelch when they walk over them
					 */
					if (k_ptr->squelch == SQUELCH_ALWAYS)
					{
						/*use a 'S' for always squelch*/
						sq = 'S';
						color = TERM_L_RED;
					}

					/*
					 * Items player doesn't want to squelch, but doesn't want to
					 * auto-destroy either
					 */

					else if (k_ptr->squelch ==  NO_SQUELCH_NEVER_PICKUP)
					{
						/*use a 'S' for always squelch*/
						sq = 'L';
						color = TERM_L_GREEN;
					}

					/*
					 * Items player always wants to pickup, regardless of
					 * other options.
					 */

					else if (k_ptr->squelch ==  NO_SQUELCH_ALWAYS_PICKUP)
					{
						/*use a 'S' for always squelch*/
						sq = 'A';
						color = TERM_L_UMBER;
					}

					/* Never Squelch */
					else
					{
						/*use a 'S' for never squelch*/
						sq = 'N';
						color = TERM_L_BLUE;
					}

					/* Print it */
					c_put_str(((num == active) ? TERM_YELLOW : TERM_WHITE),
					format("%c)'%c'", ch, sq), row, col);
					c_put_str(color, buf, row, col + 6);
				}

		    }

			/*header text*/
			if (display_all)
			{
				c_put_str(TERM_L_BLUE, "CTRL-N: No Squelch - defer to Never_pickup option", 1, 0);
				prt("Esc   : Return", 1, 55);
				c_put_str(TERM_L_GREEN, "CTRL-L: Never Pickup", 2, 0);
				c_put_str(TERM_L_UMBER, "CTRL-A: Always Pickup", 2, 22);
				prt("+/-     Toggle Selection", 2, 55);
				c_put_str(TERM_L_RED,"CTRL-S: Squelch", 3, 0);
				prt("Use direction keys to Navigate list; or enter a letter", 3, 22);
			}

			display_all = 0;
			old_active = -1;

			/* Choose! */
			if (!get_com(format("%s : Command? ", tval_desc2), &ch))
			return (1);

			/* Bugfix - Avoid crashes */
			if (!max_num) continue;

			/*Switch to Never Squelch Option*/
			if (ch == KTRL('N'))
			{
				k_info[choice[active]].squelch = SQUELCH_NEVER;
			}

			/*Switch to Never Pickup Option*/
			else if (ch == KTRL('L'))
			{
				k_info[choice[active]].squelch =  NO_SQUELCH_NEVER_PICKUP;
			}

			/*Switch to Never Pickup Option*/
			else if (ch == KTRL('A'))
			{
				k_info[choice[active]].squelch =  NO_SQUELCH_ALWAYS_PICKUP;
			}

			/*Switch to Squelch Option*/
			else if (ch == KTRL('S'))
			{
				k_info[choice[active]].squelch =  SQUELCH_ALWAYS;
			}

			/*toggle choice down one*/
			else if (ch == '-')
			{

				/*boundry control*/
				if (k_info[choice[active]].squelch <= SQUELCH_HEAD)
				{
					k_info[choice[active]].squelch = SQUELCH_TAIL;
				}

				else k_info[choice[active]].squelch -= 1;
			}

			/*toggle choice up one*/
			else if (ch == '+')
			{
				/*boundry control*/
				if (k_info[choice[active]].squelch >= SQUELCH_TAIL)
				{
					k_info[choice[active]].squelch = SQUELCH_HEAD;
				}

				else k_info[choice[active]].squelch += 1;
			}

			else if (ch == '8')
			{
				/* Redraw the current active */
				old_active = active;

				/*move up one row*/
				active -= 1;

				/*boundry control*/
				if (active < 0) active = max_num - 1;
			}

			else if (ch == '2')
			{
				/* Redraw the current active */
				old_active = active;

				/*move down one row*/
				active += 1;

				/*boundry control*/
				if (active > (max_num - 1)) active = 0;
			}

			else if (ch == '6')
			{

				/*move one column to right, but check first*/
				if ((active + LINES_PER_COLUMN) <= max_num - 1)
				{
					/* Redraw the current active */
					old_active = active;

					active += LINES_PER_COLUMN;
				}

				else bell("");

	    	}

			else if (ch == '4')
			{

				/*move one column to left, but check first*/
				if ((active - LINES_PER_COLUMN) >= 0)
				{
					/* Redraw the current active */
					old_active = active;

					active -= LINES_PER_COLUMN;
				}


				else bell("");

				}

			else
			{
				/*save the old choice*/
				int old_num = active;

				/* Analyze choice */
				active = -1;
				if ((ch >= head[0]) && (ch < head[0] + 26))		active = ch - head[0];
				if ((ch >= head[1]) && (ch < head[1] + 26))		active = ch - head[1] + 26;
				if ((ch >= head[2]) && (ch < head[2] + 17))		active = ch - head[2] + 52;

				/* Bail out if choice is "illegal" */
				if ((active < 0) || (active >= max_num)) active = old_num;
				else old_active = old_num;
			}
		}
	}


	/* And return successful */
	return (1);
}

/*
 * This command handles the secondary squelch menu.
 */
static void do_qual_squelch(void)
{
	int i, num, max_num, index;
	int col, row;
	char ch;
	/* the index for the rings*/
	#define RING_INDEX  18
	/* the index for the amulets*/
	#define AMULET_INDEX  19
	/* - open chest TVAL in defines*/

	char squelch_str[7] = "NCVGWAO";

	int old_index = -1;
	int display_all = 1;

	index = 0;
	while (1)
	{
		/* Clear screen */
		if (display_all) Term_clear();

	  	/* Print all tval's and their descriptions */
	  	for (num = 0; (num<60) && tvals[num].tval; num++)
	    {
			/* Reduce flickering */
			if (!display_all && num != index && num != old_index)
			continue;


			row = 2 + (num % 22);
			col = 30 * (num / 22);
	      	c_put_str(TERM_WHITE, format("(%c): %s", squelch_str[squelch_level[num]], tvals[num].desc), row, col);
	    }

		if (display_all)
		{

		  	/* Print out the rest of the screen */
			prt("Secondary Squelching Menu", 0,0);

			prt("Legend:", 2, 30);

		  	prt("N  : Squelch Nothing", 4, 30);
		  	prt("C  : Squelch Cursed Items", 5, 30);
		  	prt("V  : Squelch Average and Below", 6, 30);
		  	prt("G  : Squelch Good (Strong Pseudo_ID and Identify)", 7, 30);
			prt("W  : Squelch Good (Weak Pseudo-ID)", 8, 30);
		  	prt("A  : Squelch All but Artifacts", 9, 30);
		  	prt("O  : Squelch Chests After Opening", 10, 30);

		  	prt("Commands:", 12, 30);
			prt("Arrows: Move and adjust settings", 14, 30);
			prt("ncvgao : Change a single setting", 15, 30);
			prt("NCVGWAO : Change all allowable settings", 16, 30);
			prt("ESC   : Exit Secondary Menu", 17, 30);
		  	prt("Rings:   N, C or A only", 19, 30);
			prt("Amulets: N, C or A only", 20, 30);
			prt("Opened Chests: N or O only", 21, 30);
		}

		display_all = 0;
		old_index = -1;


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
				return;
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
				display_all = 1;
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
					/*HACK - don't check chests as cursed*/
					if (i != CHEST_INDEX) squelch_level[i] = SQUELCH_CURSED;
	      	  	}
				display_all = 1;
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
				display_all = 1;
	      		break;
			}

	    	case 'g':
			{
	      		if ((index != CHEST_INDEX) && (index != AMULET_INDEX)
						&& (index != RING_INDEX)) squelch_level[index] = SQUELCH_GOOD_STRONG;
	      		break;
			}

	    	case 'G':
			{
	      		for (i = 0; i < SQUELCH_BYTES; i++)
				{
					if ((i != CHEST_INDEX) && (i != AMULET_INDEX)
						&& (i != RING_INDEX)) squelch_level[i] = SQUELCH_GOOD_STRONG;
	      		}
				display_all = 1;
      			break;
			}

			case 'w':
			{
      			if ((index != CHEST_INDEX) && (index != AMULET_INDEX)
					&& (index != RING_INDEX)) squelch_level[index] = SQUELCH_GOOD_WEAK;
      			break;
			}
    		case 'W':
			{
      			for (i = 0; i < SQUELCH_BYTES; i++)
				{
					if ((i != CHEST_INDEX) && (i != AMULET_INDEX)
						&& (i != RING_INDEX)) squelch_level[i] = SQUELCH_GOOD_WEAK;
      			}
				display_all = 1;
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
					/*HACK - don't check chests as destroy all*/
					if (i != CHEST_INDEX) squelch_level[i] = SQUELCH_ALL;
      	  		}
				display_all = 1;
      			break;
			}

			/*hack "O" works from anywhere only on open chests*/
			case 'O':
			case 'o':
			{
				squelch_level[(CHEST_INDEX)] = SQUELCH_OPENED_CHESTS;
				display_all = 1;
				break;
			}
    		case '-':
    		case '8':
      		{
				old_index = index;

				index = (max_num + index - 1) % max_num;
				break;
      		}

    		case ' ':
    		case '\n':
    		case '\r':
    		case '2':
      		{
				old_index = index;

				index = (index + 1) % max_num;
				break;
      		}

    		case '4':
      		{
				/*HACK - only allowable  options to be toggled through*/

				/*first do the rings and amulets*/
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
					if (squelch_level [index] >= SQUELCH_ALL) squelch_level [index] = SQUELCH_GOOD_WEAK;
					else if (squelch_level [index] > 0)  squelch_level [index] -= 1;
					else squelch_level [index] = 0;
					break;
				}
    		}

    		case '6':
     		{
				/*HACK - only allowable  options to be toggled through*/

				/*first do the rings and amulets*/
				if ((index == AMULET_INDEX) || (index == RING_INDEX))
				{
					/*amulets and rings can only be none, cursed, and all but artifact*/
					if (squelch_level[index] > 0) squelch_level[index] = SQUELCH_ALL;
					else squelch_level[index] = SQUELCH_CURSED;
					break;
				}

				/* now do the chests*/
				else if (index == CHEST_INDEX)
				{
					squelch_level[index] = SQUELCH_OPENED_CHESTS;
					break;
				}

				/*then toggle all else*/
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

	return;
}

#define MAX_EGO_ROWS 19
#define MAX_EGO_COLS 79

/* This MUST be sorted by tval */
static tval_desc raw_tvals[] =
{
	{TV_SKELETON, "Skeletons"},
	{TV_BOTTLE, "Bottles"},
	{TV_JUNK, "Junk"},
	{TV_SPIKE, "Spikes"},
	{TV_CHEST, "Chests"},
	{TV_SHOT, "Shots"},
	{TV_ARROW, "Arrows"},
	{TV_BOLT, "Bolts"},
	{TV_BOW, "Launchers"},
	{TV_DIGGING, "Diggers"},
	{TV_HAFTED, "Maces"},
	{TV_POLEARM, "Polearms"},
	{TV_SWORD, "Swords"},
	{TV_BOOTS, "Boots"},
	{TV_GLOVES, "Gloves"},
	{TV_HELM, "Helmets"},
	{TV_CROWN, "Crowns"},
	{TV_SHIELD, "Shields"},
	{TV_CLOAK, "Cloaks"},
	{TV_SOFT_ARMOR, "Soft Armor"},
	{TV_HARD_ARMOR, "Hard Armor"},
	{TV_DRAG_ARMOR, "DSMails"},
	{TV_LITE, "Lites"},
	{TV_AMULET, "Amulets"},
	{TV_DRAG_SHIELD, "DSShields"},
	{TV_RING, "Rings"},
	{TV_STAFF, "Staves"},
	{TV_WAND, "Wands"},
	{TV_ROD, "Rods"},
	{TV_SCROLL, "Scrolls"},
	{TV_POTION, "Potions"},
	{TV_FLASK, "Flaskes"},
	{TV_FOOD, "Food"},
	{TV_MAGIC_BOOK, "Magic Books"},
	{TV_PRAYER_BOOK, "Prayer Books"},
};

#define NUM_RAW_TVALS (sizeof(raw_tvals) / sizeof(raw_tvals[0]))

/*
 * Skip common prefixes in ego-item names.
 */
static const char *strip_ego_name(const char *name)
{
 	if (prefix(name, "of the "))	return name + 7;
 	if (prefix(name, "of "))	return name + 3;
 	return name;
}

/*
 * Utility function used to find tval names.
 */
static int tval_searching_func(const void *a_ptr, const void *b_ptr)
{
	int a = ((tval_desc *)a_ptr)->tval;
	int b = ((tval_desc *)b_ptr)->tval;
	return a - b;
}

/*
 * Display an ego-item type on the screen.
 */
static void display_ego_item(ego_item_type *e_ptr, int y, int x, bool active)
{
	int tval_table[EGO_TVALS_MAX], i, n = 0;
	char buf[100];
	const char *str, *name;

	/* Fast appending, and easier to code ;) */
 	editing_buffer ebuf, *ebuf_ptr = &ebuf;

	/* Copy the valid tvals of this ego-item type */
	for (i = 0; i < EGO_TVALS_MAX; i++)
	{
    	/* Ignore "empty" entries */
    	if (e_ptr->tval[i] < 1) continue;

    	tval_table[n++] = e_ptr->tval[i];
	}

	/* Hack - Sort the tvals using bubbles */
	for (i = 0; i < n; i++)
	{
    	int j;

    	for (j = i + 1; j < n; j++)
    	{
    		if (tval_table[i] > tval_table[j])
      		{
				int temp = tval_table[i];
				tval_table[i] = tval_table[j];
				tval_table[j] = temp;
      		}
    	}
  	}

	/* Initialize the editing_buffer structure */
	editing_buffer_init(ebuf_ptr, "[ ] ", 100);

	/* Concatenate the tval' names */
	for (i = 0; i < n; i++)
	{
    	/* Fast searching */
    	tval_desc key, *result;

    	/* Find the tval's name using binary search */
    	key.tval = tval_table[i];
    	key.desc = NULL;
    	result = bsearch(&key, raw_tvals, NUM_RAW_TVALS, sizeof(raw_tvals[0]),
		tval_searching_func);
    	if (result) name = result->desc;
    	/* Paranoia */
    	else	name = "????";

    	/* Append the respective separator first, if any */
    	if (i > 0)
    	{
    	  if (i < n - 1)	editing_buffer_put_str(ebuf_ptr, ", ", -1);
    	  else		editing_buffer_put_str(ebuf_ptr, " and ", -1);
    	}
    	/* Append the name */
    	editing_buffer_put_str(ebuf_ptr, name, -1);
  	}

	/* Append one  extra space */
	editing_buffer_put_chr(ebuf_ptr, ' ');

	/* Hack - Find common ego-item name' prefixes */
	name = e_name + e_ptr->name;
	str = strip_ego_name(name);

 	/* Append the prefix to the buffer, if any */
 	editing_buffer_put_str(ebuf_ptr, name, str - name);

 	/* Get the buffer */
 	editing_buffer_get_all(ebuf_ptr, buf, sizeof(buf));

  	/* Free resources */
 	editing_buffer_destroy(ebuf_ptr);

 	/* Show the buffer */
 	c_put_str(active ? TERM_YELLOW: TERM_WHITE, buf, y, x);

  	if (e_ptr->squelch) c_put_str(TERM_L_RED, "*", y, x + 1);

  	/* Show the stripped ego-item name with another colour */
  	c_put_str(e_ptr->squelch ? TERM_L_RED: TERM_L_BLUE, str, y, x + strlen(buf));
}

/*
 * Utility function used for sorting an array of ego-item indices by
 * ego-item name.
 */
static int ego_sorting_func(const void *a_ptr, const void *b_ptr)
{
  	s16b a = *(s16b *)a_ptr;
  	s16b b = *(s16b *)b_ptr;

  	/* Note the removal of common prefixes */
  	return strcmp(strip_ego_name(e_name + e_info[a].name),
 				  strip_ego_name(e_name + e_info[b].name));
}

/*
 * Handle the squelching of ego-items.
*/
static int do_ego_item_squelch(void)
{
	int i, idx, max_num = 0, first, last, active, old_active;
	bool display_all;
	char ch, *msg;
	ego_item_type *e_ptr;
 	s16b *choice;

 	/* Alloc the array of ego indices */
 	C_MAKE(choice, alloc_ego_size, s16b);

 	/* Get the valid ego-items */
 	for (i = 0; i < alloc_ego_size; i++)
 	{
    	idx = alloc_ego_table[i].index;

    	e_ptr = &e_info[idx];

    	/* Only valid known ego-items allowed */
    	if (!e_ptr->name || !e_ptr->everseen) continue;

    	/* Append the index */
    	choice[max_num++] = idx;
	}

  	/* Quickly sort the array by ego-item name */
  	qsort(choice, max_num, sizeof(choice[0]), ego_sorting_func);

  	/* Display the whole screen */
  	display_all = TRUE;
  	active = 0;
  	old_active = -1;

  	/* Determine the first ego-item to display in the screen */
  	first = 0;

  	/* Determine the last ego-item to display in the screen */
  	/* Note that if "max_num" is 0, "last" will be -1 */
  	last = MIN(first + MAX_EGO_ROWS - 1, max_num - 1);

  	while(1)
  	{
    	if (display_all)
    	{
    		/* Clear the screen */
   		 	Term_clear();

    		/* Header */
    		c_put_str(TERM_WHITE, "[ ]:", 1, 0);
    		c_put_str(TERM_L_RED, "*", 1, 1);
    		c_put_str(TERM_L_RED, "Squelch", 1, 5);

    		c_put_str(TERM_WHITE, "[ ]:", 1, 15);
    		c_put_str(TERM_L_BLUE, "No squelch", 1, 20);

    		/* No ego-items */
    		msg = "You have not seen any ego-items yet.";
    		if (max_num < 1) c_put_str(TERM_RED, msg, 3, 0);

      		/* Hack - Make the UI more friendly if needed */
      		if (first > 0) c_put_str(TERM_WHITE, "-more-", 2, 4);
      		if (last < max_num - 1) c_put_str(TERM_WHITE, "-more-", 22, 4);

      		/* Page foot */
      		msg = "Navigation: 2, 8, 3, 9, 1, 7 or movement keys";
      		c_put_str(TERM_WHITE, msg, 23, 0);
    	}

    	/* Only show a portion of the list */
    	for (i = first; i <= last; i++)
    	{
      		/* Avoid flickering */
      		if (!display_all && (i != active) && (i != old_active)) continue;

      		e_ptr = &e_info[choice[i]];

      		/* Show the entry */
      		display_ego_item(e_ptr, i - first + 3, 0, i == active);
    	}

    	/* Reset some flags */
    	display_all = FALSE;
    	old_active = -1;

    	/* Get a command */
    	msg = "Command? (SPACE, RET: Toggle selection - ESC: Return) ";
    	if (!get_com(msg, &ch)) break;

    	/* Avoid crash */
    	if (max_num < 1) continue;

    	/* Get the selected ego-item type */
    	e_ptr = &e_info[choice[active]];

    	/* Process the command */
    	switch (ch)
    	{
      		case ' ':
      		case '\r':
      		case '\n':
			{
	  			/* Toggle the "squelch" flag */
	  			e_ptr->squelch = !e_ptr->squelch;
	  			break;
			}
      		case '2':
			{
	  			/* Advance a position */
	  			old_active = active;
	  			active = MIN(active + 1, max_num - 1);
	  			if (active > last)
	  			{
	    			++first;
	    			++last;
	    			/* Redraw all */
	    			display_all = 1;
	  			}
	  			break;
			}
      		case '8':
			{
				/* Retrocede a position */
	  			old_active = active;
	  			active = MAX(active - 1, 0);
	  			if (active < first)
	  			{
	    			--first;
	    			--last;

					/* Redraw all */
	    			display_all = 1;
	  			}

				break;
			}

			case '3':
			{
	  			/* Advance one "screen" */
	  			active = MIN(active + MAX_EGO_ROWS, max_num - 1);
	  			last = MIN(last + MAX_EGO_ROWS, max_num - 1);
	  			first = MAX(last - MAX_EGO_ROWS + 1, 0);

	  			/* Redraw all */
	  			display_all = 1;
	  			break;
			}

			case '9':
			{
	  			/* Retrocede one "screen" */
	 			active = MAX(active - MAX_EGO_ROWS, 0);
	  			first = MAX(first - MAX_EGO_ROWS, 0);
	  			last = MIN(first + MAX_EGO_ROWS - 1, max_num - 1);

	  			/* Redraw all */
	  			display_all = 1;
	  			break;
			}
      		case '1':
			{
	  			/* Go the last ego-item */
	  			active = last = max_num - 1;
	  			first = MAX(last - MAX_EGO_ROWS + 1, 0);

				/* Redraw all */
	  			display_all = 1;
	  			break;
			}

			case '7':
			{
	  			/* Go to the first ego-item */
	  			active = first = 0;
	  			last = MIN(first + MAX_EGO_ROWS - 1, max_num - 1);

	  			/* Redraw all */
	  			display_all = 1;
	  			break;
			}
    	}
  	}

  	/* Free resources */
  	FREE(choice);
  	return 0;
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
  tv_to_type[TV_SHOT]=TYPE_AMMO;
  tv_to_type[TV_ARROW]=TYPE_AMMO;
  tv_to_type[TV_BOLT]=TYPE_AMMO;
  tv_to_type[TV_BOW]=TYPE_BOW;
  tv_to_type[TV_DIGGING]=TYPE_WEAPON2;
  tv_to_type[TV_HAFTED]=TYPE_WEAPON2;
  tv_to_type[TV_POLEARM]=TYPE_WEAPON2;
  tv_to_type[TV_SWORD]=TYPE_WEAPON1;
  tv_to_type[TV_BOOTS]=TYPE_BOOTS;
  tv_to_type[TV_GLOVES]=TYPE_GLOVES;
  tv_to_type[TV_HELM]=TYPE_HELM;
  tv_to_type[TV_CROWN]=TYPE_HELM;
  tv_to_type[TV_SHIELD]=TYPE_SHIELD;
  tv_to_type[TV_CLOAK]=TYPE_CLOAK;
  tv_to_type[TV_SOFT_ARMOR]=TYPE_BODY;
  tv_to_type[TV_HARD_ARMOR]=TYPE_BODY;
  tv_to_type[TV_DRAG_ARMOR]=TYPE_BODY;
  tv_to_type[TV_DRAG_SHIELD]=TYPE_SHIELD;
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
 *  fullid  : Is the object is being identified?
 *
 * Output: One of the three above values.
 */

int squelch_itemp(object_type *o_ptr, byte feelings, bool fullid)
{
  	int i, num, result;
  	byte feel;

  	/* default */
  	result = SQUELCH_NO;

  	/*never squelch quest items*/
  	if (o_ptr->ident & IDENT_QUEST) return result;

	/* Squelch some ego items */
	if ((ego_item_p(o_ptr)) && (e_info[o_ptr->name2].squelch))
	{
		return ((o_ptr->note) ? SQUELCH_FAILED: SQUELCH_YES);
	}

  	/* Check to see if the object is eligible for squelching on id. */
  	num = -1;

	/*find the appropriate squelch group*/
  	for (i=0; tvals[i].tval; i++)
  	{
  		if (tvals[i].tval == o_ptr->tval)
		{

      		num = i;

    	}
  	}

	/*never squelched*/
  	if (num == -1) return result;

  	/*
   	 * Get the "feeling" of the object.  If the object is being identified
   	 * get the feeling returned by a heavy pseudoid.
   	 */
  	feel = feelings;

	/*handle fully identified objects*/
  	if (fullid)  feel = value_check_aux1(o_ptr);

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

    	case SQUELCH_GOOD_STRONG:
		{
      		result = (((feel==INSCRIP_BROKEN) ||
		 	(feel==INSCRIP_TERRIBLE) ||
		 	(feel==INSCRIP_WORTHLESS) ||
		 	(feel==INSCRIP_CURSED) ||
		 	(feel==INSCRIP_AVERAGE) ||
		 	(feel==INSCRIP_GOOD_STRONG)) ? SQUELCH_YES : SQUELCH_NO);
     		 break;
		}

		case SQUELCH_GOOD_WEAK:
		{
      		result = (((feel==INSCRIP_BROKEN) ||
		 			   (feel==INSCRIP_TERRIBLE) ||
		 			   (feel==INSCRIP_WORTHLESS) ||
		 			   (feel==INSCRIP_CURSED) ||
		 			   (feel==INSCRIP_AVERAGE) ||
		 			   (feel==INSCRIP_GOOD_STRONG) ||
					   (feel==INSCRIP_GOOD_WEAK)) ? SQUELCH_YES : SQUELCH_NO);
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

	/*hack - never squelch quest items*/
	if (o_ptr->ident & IDENT_QUEST) return 0;

  	if (item>0)
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

  /* Initialize */
  first_bad_idx = 0;
  first_good_idx = 0;
  cur_bad_idx = 0;
  cur_good_idx = 0;

  /*go through all the objects*/
  for(o_idx = cave_o_idx[y][x]; o_idx; o_idx = next_o_idx)
  {
    	o_ptr = &(o_list[o_idx]);
    	next_o_idx = o_ptr->next_o_idx;

		/*is it marked for squelching*/
    	sq_flag = ((k_info[o_ptr->k_idx].squelch == SQUELCH_ALWAYS) &&
				   (k_info[o_ptr->k_idx].aware));

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

   		sq_flag =  ((k_info[o_ptr->k_idx].squelch == SQUELCH_ALWAYS) &&
	   					(k_info[o_ptr->k_idx].aware));

		/*hack - never squelch artifacts*/
		if artifact_p(o_ptr) sq_flag = FALSE;

		/*always squelch "&nothing*/
		if (!o_ptr->k_idx) sq_flag = TRUE;

		/*never delete quest items*/
    	if ((sq_flag) && (!(o_ptr->ident & IDENT_QUEST)))
		{
      		delete_object_idx(o_idx);
		}
  	}
}




