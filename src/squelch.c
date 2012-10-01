#include "angband.h"

static int do_qual_squelch(void);

/*
 * These are the various levels of squelching supported by the game.  
 * Less concisely:
 */
#define SQUELCH_NONE     0 /* No squelching */
#define SQUELCH_CURSED   1 /* Squelch cursed items */
#define SQUELCH_AVERAGE  2 /* Squelch average and worse items */
#define SQUELCH_GOOD     3 /* Squelch good and worse items */
#define SQUELCH_ALL      4 /* Squelch all but artifacts */

/*
 * This (admittedly hacky) stores the mapping from tval to typeval
 * and is reinitialized every time do_cmd_squelch is called.  This 
 * can certainly be done more cleanly.
 */
static int tv_to_type[100];

static char head[4] = { 'a', 'A', '0', ':' };

/*
 * Here are the categories for squelch-on-creation.
 */
static tval_desc_type typevals[MAX_SQ_TYPES + 1] =
{
	{SQ_TYPE_AMMO,		"Missiles"				},
	{SQ_TYPE_BOW,		"Missile launchers"		},
	{SQ_TYPE_WEAPON,	"Melee weapons"			},
	{SQ_TYPE_BODY,		"Body armor"			},
	{SQ_TYPE_CLOAK,		"Cloaks"				},
	{SQ_TYPE_SHIELD,	"Shields"				},
	{SQ_TYPE_HELM,		"Head gear"				},
	{SQ_TYPE_BOOT,		"Gloves and boots"		},
	{SQ_TYPE_AMULET,	"Amulets"				},
	{SQ_TYPE_RING,		"Rings"					},
	{SQ_TYPE_STAFF,		"Staves"				},
	{SQ_TYPE_WAND,		"Wands"					},
	{SQ_TYPE_ROD,		"Rods and talismans"	},
	{SQ_TYPE_SCROLL,	"Scrolls"				},
	{SQ_TYPE_POTION,	"Potions"				},
	{SQ_TYPE_POWDER,	"Powders"				},
	{SQ_TYPE_BOOK,		"Spellbooks"			},
	{SQ_TYPE_MUSIC,		"Musical instruments"	},
	{SQ_TYPE_FOOD,		"Food items"			},
	{SQ_TYPE_LITE,		"Light sources"			},
	{SQ_TYPE_MISC,		"Miscellaneous"			},
	{0, NULL}
};

/*
 * Here are the categories for squelch-on-identification.
 */
static tval_desc_type tvals_on_id[] =
{
	{ TV_SWORD,			"Sword"				},
	{ TV_POLEARM,		"Polearm"			},
	{ TV_HAFTED,		"Hafted Weapon"		},
	{ TV_BOW,			"Bow"				},
	{ TV_ARROW,			"Arrows"			},
	{ TV_BOLT,			"Bolts"				},
	{ TV_SHOT,			"Shots"				},
	{ TV_SHIELD,		"Shield"			},
	{ TV_HEADGEAR,		"Head Gear"			},
	{ TV_GLOVES,		"Gloves"			},
	{ TV_BOOTS,			"Boots"				},
	{ TV_CLOAK,			"Cloak"				},
	{ TV_DRAG_ARMOR,	"Dragon Scale Mail"	},
	{ TV_BODY_ARMOR,	"Body Armor"		},
	{ TV_DIGGING,		"Diggers"			},
	{ 0,				NULL }
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
	cptr tval_desc;
	char ch, sq;

	int choice[64];

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

	for (num = 0; (num < 63) && typevals[num].tval; num++)
	{
		row = 2 + (num % 21);
		col = 30 * (num / 21);
		ch = head[num / 26] +num % 26;
		prt(format("[%c] %s", ch, typevals[num].desc), row, col);
	}

	/* Me need to know the maximal possible tval_index */
	max_num = num;

	prt("Commands:", 2, 30);
	prt("[a-u]: Go to item squelching sub-menu.", 4, 30);
	prt("Q    : Go to quality squelching sub-menu.", 5, 30);
	prt(format("J    : Toggle squelch_junk (Currently %s).",
		(squelch_junk ? "ON" : "OFF")), 6, 30);
	prt(format("A    : Toggle auto_squelch (Currently %s).",
		(auto_squelch ? "ON" : "OFF")), 7, 30);
	prt("S    : Save squelch values to pref file.", 9, 30);
	prt("L    : Load squelch values from pref file.", 10, 30);
	prt("ESC  : Back to options menu.", 12, 30);

	/* Choose! */
	if (!get_com("Item Squelching Main Menu: ", &ch)) return FALSE;

	if (ch=='Q') 
	{
		/* Switch to secondary squelching menu */
		do_qual_squelch();
	} 
	else if (ch=='J') 
	{
		squelch_junk = !squelch_junk;
	} 
	else if (ch=='A') 
	{
		auto_squelch = !auto_squelch;
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
			  
				for (i = 0; i < MAX_SQ_TYPES; i++) 
					fprintf(fff, "Q:%d:%d\n", i, op_ptr->squelch_level[i]);
			  
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
		if ((num < 0) || (num >= max_num)) return FALSE;

		/* Base object type chosen, fill in tval */
		typeval = typevals[num].tval;
		tval_desc = typevals[num].desc;

		/*** And now we go for k_idx ***/

		while (TRUE) 
		{
			Term_clear();

			/* First sort based on value */
			/* Step 1: Read into choice array */

			for (num = 0, i = 1; (num < 60) && (i < z_info->k_max); i++)
			{
				object_kind *k_ptr = &k_info[i];
		
				if (tv_to_type[k_ptr->tval] == typeval) 
				{
					int j, k;

					/* Skip items with no distribution (special artifacts) */
					for (j = 0, k = 0; j < MAX_OBJ_ALLOC; j++) k += k_ptr->chance[j];
					if (!k_ptr->everseen) continue;
					if (!k)  continue; 

					choice[num++] = i;
				}
			}

			max_num = num;

			/* Step 2: Simple bubble sort */
			for (i=0; i < max_num; i++)
			{ 
				for (j=i; j < max_num; j++) 
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
				for (num = 0; num<max_num; num++)
				{
					object_kind *k_ptr = &k_info[choice[num]];

					/* Prepare it */
					row = 3 + (num % 21);
					col = 28 * (num / 21);
					ch = head[num / 26] + (num % 26);

					/* Acquire the "name" of object "i" */
					strip_name(buf, choice[num]);

					/* Get the squelch character */
					sq = (k_ptr->squelch ? '*' : ' ');

					/* Get the color */
					color = (k_ptr->squelch ? 
						(k_ptr->aware ? TERM_RED : TERM_L_UMBER) :
						(k_ptr->aware ? TERM_L_GREEN : TERM_SLATE));

					/* Print it */
					prt(format("[%c%c] ", ch, sq), row, col);
					c_put_str(color, buf, row, col + 5);
				}

				/* Print the legend */
				prt("'*': Squelch           ' ': Do not squelch", 1, 0);
			}

			/* Choose! */
			if (!get_com(format("%s : Command? (^A: Squelch all   ^U: Unsquelch all)", 
				tval_desc), &ch)) return TRUE;

			if (ch == KTRL('A')) 
			{
				/* ^A --> Squelch all items */
				for (i = 0; i < max_num; i++) 
				{
					k_info[choice[i]].squelch = TRUE;
				}
			} 
			else if (ch == KTRL('U')) 
			{
				/* ^U --> Unsquelch all items */
				for (i = 0; i < max_num; i++) 
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
				k_info[choice[num]].squelch = (k_info[choice[num]].squelch ? FALSE : TRUE);
			}
		}
	}

	/* And return successful */
	return TRUE;
}

/*
 * This command handles the secondary squelch menu. 
 */
static int do_qual_squelch(void)
{
	int i, num, max_num, index;
	int col, row;
	char ch;

	char squelch_str[5] = "NCVGA";

	index = 0;
	while (TRUE) 
	{
		/* Clear screen */
		Term_clear();

		/* Print all tval's and their descriptions */
		for (num = 0; (num<60) && tvals_on_id[num].tval; num++)
		{
			row = 2 + (num % 20);
			col = 30 * (num / 20);
			prt(format("(%c): %s", 
				squelch_str[op_ptr->squelch_level[num]], tvals_on_id[num].desc), row, col);
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
		prt("ESC   : Exit Secondary Menu", 16, 30);
		prt("Secondary Squelching Menu", 0,0);

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
				return FALSE;
			}

			case 'n':
			{
				op_ptr->squelch_level[index] = SQUELCH_NONE;
				break;
			}

			case 'N':
			{
				for (i = 0; i < MAX_SQ_TYPES; i++) op_ptr->squelch_level[i] = SQUELCH_NONE;
				break;
			}

			case 'c':
			{
				op_ptr->squelch_level[index] = SQUELCH_CURSED;
				break;
			}

			case 'C':
			{
				for (i = 0; i < MAX_SQ_TYPES; i++) op_ptr->squelch_level[i] = SQUELCH_CURSED;

				break;
			}

			case 'v':
			{
				op_ptr->squelch_level[index] = SQUELCH_AVERAGE;
				break;
			}

			case 'V':
			{
				for (i = 0; i < MAX_SQ_TYPES; i++) op_ptr->squelch_level[i] = SQUELCH_AVERAGE;
				break;
			}

			case 'g':
			{
				op_ptr->squelch_level[index] = SQUELCH_GOOD;
				break;
			}

			case 'G':
			{
				for (i = 0; i < MAX_SQ_TYPES; i++) op_ptr->squelch_level[i] = SQUELCH_GOOD;
				break;
			}

			case 'a':
			{
				op_ptr->squelch_level[index] = SQUELCH_ALL;
				break;
			}

			case 'A':
			{
				for (i = 0; i < MAX_SQ_TYPES; i++) op_ptr->squelch_level[i] = SQUELCH_ALL;
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
				op_ptr->squelch_level[index] = 
					(op_ptr->squelch_level[index] + 1) % (SQUELCH_ALL + 1);
				break;
			}

			case '4':
			{
				op_ptr->squelch_level[index] = 
					(SQUELCH_ALL + op_ptr->squelch_level[index]) % (SQUELCH_ALL + 1);
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
static void init_tv_to_type(void) 
{
	  tv_to_type[TV_CHEST]			= SQ_TYPE_MISC;
	  tv_to_type[TV_SHOT]			= SQ_TYPE_AMMO;
	  tv_to_type[TV_ARROW]			= SQ_TYPE_AMMO;
	  tv_to_type[TV_BOLT]			= SQ_TYPE_AMMO;
	  tv_to_type[TV_BOW]			= SQ_TYPE_BOW;
	  tv_to_type[TV_DIGGING]		= SQ_TYPE_WEAPON;
	  tv_to_type[TV_HAFTED]			= SQ_TYPE_WEAPON;
	  tv_to_type[TV_POLEARM]		= SQ_TYPE_WEAPON;
	  tv_to_type[TV_SWORD]			= SQ_TYPE_WEAPON;
	  tv_to_type[TV_BOOTS]			= SQ_TYPE_BOOT;
	  tv_to_type[TV_GLOVES]			= SQ_TYPE_BOOT;
	  tv_to_type[TV_HEADGEAR]		= SQ_TYPE_HELM;
	  tv_to_type[TV_SHIELD]			= SQ_TYPE_SHIELD;
	  tv_to_type[TV_CLOAK]			= SQ_TYPE_CLOAK;
	  tv_to_type[TV_BODY_ARMOR]		= SQ_TYPE_BODY;
	  tv_to_type[TV_DRAG_ARMOR]		= SQ_TYPE_BODY;
	  tv_to_type[TV_LITE]			= SQ_TYPE_LITE;
	  tv_to_type[TV_LITE_SPECIAL]	= SQ_TYPE_LITE;
	  tv_to_type[TV_AMULET]			= SQ_TYPE_AMULET;
	  tv_to_type[TV_RING]			= SQ_TYPE_RING;
	  tv_to_type[TV_STAFF]			= SQ_TYPE_STAFF;
	  tv_to_type[TV_WAND]			= SQ_TYPE_WAND;
	  tv_to_type[TV_ROD]			= SQ_TYPE_ROD;
	  tv_to_type[TV_TALISMAN]		= SQ_TYPE_ROD;
	  tv_to_type[TV_SCROLL]			= SQ_TYPE_SCROLL;
	  tv_to_type[TV_POTION]			= SQ_TYPE_POTION;
	  tv_to_type[TV_FLASK]			= SQ_TYPE_MISC;
	  tv_to_type[TV_FOOD]			= SQ_TYPE_FOOD;
	  tv_to_type[TV_MAGIC_BOOK]		= SQ_TYPE_BOOK;
	  tv_to_type[TV_POWDER]			= SQ_TYPE_POWDER;
	  tv_to_type[TV_MUSIC]			= SQ_TYPE_MUSIC;
} 

void do_cmd_squelch(void)
{
	/* Init type table */
	init_tv_to_type();

	/* Simple loop */
	while (TRUE)  
	{
		if (!do_cmd_squelch_aux()) break;
	}
}

/*
 * Determines if an object is going to be squelched on identification.
 */
bool squelch_itemp(object_type *o_ptr)
{
	int i, num, result;
	bool heavy_pseudo_id = ((cp_ptr->flags & CF_PSEUDO_ID_HEAVY) ? TRUE : FALSE);

	/* default */
	result = FALSE;

	/* Try to squelch junk */
	if ((squelch_junk) && (object_value(o_ptr) == 0))
	{
		if (!artifact_p(o_ptr) && (o_ptr->tval != TV_QUEST)) result = TRUE;
	}

	/* Try to squelch by type */
	if (k_info[o_ptr->k_idx].squelch)
	{
		if (object_aware_p(o_ptr)) result = TRUE;
	}
	else 
	{
		bool squelch_id = TRUE;
		byte feel;

		/* Check to see if the object is eligible for squelching on id. */
		num = -1;

		for (i = 0; tvals_on_id[i].tval; i++) 
		{
			if (tvals_on_id[i].tval == o_ptr->tval) 
			{
				num = i;
				break;
			}
		}
		if (num == -1) squelch_id = FALSE;
 
		/* 
		 * Get the "feeling" of the object.  If the object is being identified
		 * get the feeling returned by a heavy pseudoid.
		 */
		if (o_ptr->ident & IDENT_KNOWN) feel = value_check_aux1(o_ptr);
		else if (o_ptr->ident & IDENT_SENSE) 
			feel = (heavy_pseudo_id) ? value_check_aux1(o_ptr) : value_check_aux2(o_ptr);
		else squelch_id = FALSE;

		if (squelch_id)
		{
			/* 
			 * Get result based on the feeling and the squelch_level.
			 * Note that the checks fall through. 
			 */
			switch (op_ptr->squelch_level[num]) 
			{
				case SQUELCH_ALL:
				{
					result = TRUE;
					break;
				}
				case SQUELCH_GOOD:
				{
					/* Non-heavy pseudo-ID, only treat good inscriptions if fully ID'd */
					if ((feel == INSCRIP_GOOD) && (heavy_pseudo_id || (o_ptr->ident & IDENT_KNOWN)))
						result = TRUE;
				}
				case SQUELCH_AVERAGE:
				{
					if (feel == INSCRIP_AVERAGE) result = TRUE;
				}
				case SQUELCH_CURSED:
				{
					if ((feel == INSCRIP_BROKEN) || (feel == INSCRIP_WORTHLESS) || 
						(feel == INSCRIP_CURSED) || (feel == INSCRIP_TERRIBLE)) result = TRUE;
				}
				case SQUELCH_NONE:
				{	
					break;
				}
			}
		}
	}

	if (result)
	{
		/* Squelching will fail on an artifact or if there is an inscription */
		if (artifact_p(o_ptr))
		{
			result = FALSE;

			/* Remove special inscription, if any */
			if (!object_known_p(o_ptr)) switch (o_ptr->discount)
			{
				case 0:
				case INSCRIP_NULL:
				case INSCRIP_UNCURSED:
				case INSCRIP_INDESTRUCT:
				{
					o_ptr->discount = INSCRIP_INDESTRUCT;
					break;
				}
				case INSCRIP_TERRIBLE:
				case INSCRIP_CURSED:
				{
					o_ptr->discount = INSCRIP_TERRIBLE;
					break;
				}
				case INSCRIP_GOOD:
				case INSCRIP_SPECIAL:
				{
					o_ptr->discount = INSCRIP_SPECIAL;
					break;
				}
			}
		}
		else if (o_ptr->note) result = FALSE;
	}
  
	return result;
}

/*
 * This performs the squelch, actually removing the item from the 
 * game.The return value is never actually used.
 */
void do_squelch_item(object_type *o_ptr)
{
	o_ptr->note = 0;

	o_ptr->note = quark_add("squelch");

	/* We should destroy some items */
	if (auto_squelch) p_ptr->update |= PU_SQUELCH;
}

/*
 * An "item_tester_hook" for items marked squelch
 */
static bool item_tester_squelched(object_type *o_ptr)
{
	cptr s;

	/* Should be inscribed */
	if (!o_ptr->note) return (FALSE);

	/* Look for the SQUELCH inscribtion */
	s = strstr(quark_str(o_ptr->note), "squelch");
	if (s) return (TRUE);

	/* Assume not okay */
	return (FALSE);
}

/*
 * Destroy all items marked squelch
 */
void destroy_squelched_items(void)
{
	int floor_num, floor_list[24];
	int n;
	int count=0;

	object_type *o_ptr;

	item_tester_hook = item_tester_squelched;

	floor_num = scan_floor(floor_list, 24, p_ptr->py, p_ptr->px, 0x01);

	if (floor_num)
	{
		for (n = 0; n < floor_num; n++)
		{
			o_ptr = &o_list[floor_list[n]];

			/* Hack -- skip artifacts */
			if (artifact_p(o_ptr)) continue;

			/* Mega hack -- skip quest objects */
			if (o_ptr->tval == TV_QUEST) continue;

			if (item_tester_okay(o_ptr))
			{
				/* Destroy item */
				floor_item_increase(floor_list[n], -o_ptr->number);
				floor_item_optimize(floor_list[n]);

				/* Count the casualties */
				count ++;
			}
		}
	}

	/* Scan through the slots backwards */
	for (n = INVEN_PACK - 1; n >= 0; n--)
	{
		o_ptr = &inventory[n];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Hack -- skip artifacts */
		if (artifact_p(o_ptr)) continue;

		/* Mega hack -- skip quest objects */
		if (o_ptr->tval == TV_QUEST) continue;

		/* Give this item slot a shot at death */
		if (item_tester_okay(o_ptr))
		{
			/* Destroy item */
			inven_item_increase(n, -o_ptr->number);
			inven_item_optimize(n);

			/* Count the casualties */
			count ++;
		}
	}

	item_tester_hook = NULL;	  

	/* message */
	if (count > 0) message_format(MSG_SUCCEED, 0, "%d item%s squelched.",
		count, ((count > 1) ? "s" : ""));
	else message(MSG_FAIL, 0, "No squelched items to destroy.");

	return;
}
	