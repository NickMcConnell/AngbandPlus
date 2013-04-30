/* File: birth.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-6 Andrew Doull. Modifications to the Angband 2.9.6
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"


/*
 * Forward declare
 */
typedef struct birther birther;
typedef struct birth_menu birth_menu;

/*
 * A structure to hold "rolled" information
 */
struct birther
{
	s16b age;
	s16b wt;
	s16b ht;
	s16b sc;

	s32b au;

	s16b stat[A_MAX];

	char history[250];
};

/*
 * A structure to hold the menus
 */
struct birth_menu
{
	bool ghost;
	cptr name;
	int choice;
};

/*
 * The last character displayed
 */
static birther prev;


/*
 * Current stats (when rolling a character).
 */
static s16b stat_use[A_MAX];



/*
 * Save the currently rolled data for later.
 */
static void save_prev_data(void)
{
	int i;


	/*** Save the current data ***/

	/* Save the data */
	prev.age = p_ptr->age;
	prev.wt = p_ptr->wt;
	prev.ht = p_ptr->ht;
	prev.sc = p_ptr->sc;
	prev.au = p_ptr->au;

	/* Save the stats */
	for (i = 0; i < A_MAX; i++)
	{
		prev.stat[i] = p_ptr->stat_max[i];
	}

	/* Save the history */
	my_strcpy(prev.history, p_ptr->history, sizeof(prev.history));
}


/*
 * Load the previously rolled data.
 */
static void load_prev_data(void)
{
	int i;

	birther temp;


	/*** Save the current data ***/

	/* Save the data */
	temp.age = p_ptr->age;
	temp.wt = p_ptr->wt;
	temp.ht = p_ptr->ht;
	temp.sc = p_ptr->sc;
	temp.au = p_ptr->au;

	/* Save the stats */
	for (i = 0; i < A_MAX; i++)
	{
		temp.stat[i] = p_ptr->stat_max[i];
	}

	/* Save the history */
	my_strcpy(temp.history, p_ptr->history, sizeof(temp.history));

	/*** Load the previous data ***/

	/* Load the data */
	p_ptr->age = prev.age;
	p_ptr->wt = prev.wt;
	p_ptr->ht = prev.ht;
	p_ptr->sc = prev.sc;
	p_ptr->au = prev.au;

	/* Load the stats */
	for (i = 0; i < A_MAX; i++)
	{
		p_ptr->stat_max[i] = prev.stat[i];
		p_ptr->stat_cur[i] = prev.stat[i];
	}

	/* Load the history */
	my_strcpy(p_ptr->history, prev.history, sizeof(p_ptr->history));

	/*** Save the current data ***/

	/* Save the data */
	prev.age = temp.age;
	prev.wt = temp.wt;
	prev.ht = temp.ht;
	prev.sc = temp.sc;
	prev.au = temp.au;

	/* Save the stats */
	for (i = 0; i < A_MAX; i++)
	{
		prev.stat[i] = temp.stat[i];
	}

	/* Save the history */
	my_strcpy(prev.history, temp.history, sizeof(prev.history));
}


/*
 * Adjust a stat by an amount.
 *
 * The "auto_roll" flag selects maximal changes for use 
 * with the auto-roller initialization code
 * or minimal for the point-based character generation.  
 * Otherwise, semi-random changes will occur.
 */
static int adjust_stat(int value, int amount, int auto_roll)
{
	/* Negative amounts */
	if (amount < 0)
	{
		return (modify_stat_value(value, amount));
	}

	/* Increase, using the real stat increase function */
	else
	{
		int i;

		/* Apply reward */
		for (i = 0; i < amount; i++)
		{
			value = calc_inc_stat(value, auto_roll); 
		}

		return (value);
	}
}


/*
 * Roll for a characters stats
 *
 * For efficiency, we include a chunk of "calc_bonuses()".
 */
static void get_stats(void)
{
	int i, j;

	int bonus_add;

	int dice[A_MAX * 3];


	/* Roll and verify some stats */
	while (TRUE)
	{
		/* Roll some dice */
		for (j = i = 0; i < A_MAX * 3; i++)
		{
			/* Roll the dice */
			dice[i] = randint(3 + i % 3);

			/* Collect the maximum */
			j += dice[i];
		}

		/* Verify totals; (A_MAX - 1) to fit spell-point generation */
		if ((j > (A_MAX - 1) * 7) && (j < (A_MAX - 1) * 9)) break;
	}

	/* Roll the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Extract 5 + 1d3 + 1d4 + 1d5 */
		j = 5 + dice[3*i] + dice[3*i+1] + dice[3*i+2];

		/* Obtain a "bonus" for "race" and "class" */
		bonus_add = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

		/* Apply the bonus to the stat (randomly) */
		stat_use[i] = adjust_stat(j, bonus_add, 0);

		/* Save the resulting stat maximum */
		p_ptr->stat_cur[i] = p_ptr->stat_max[i] = stat_use[i];

	}
}


/*
 * Get the racial history, and social class, using the "history charts".
 */
static void get_history(void)
{
	int i, chart, roll, social_class;


	/* Clear the previous history strings */
	p_ptr->history[0] = '\0';


	/* Initial social class */
	social_class = randint(4);

	/* Starting place */
	chart = rp_ptr->hist;


	/* Process the history */
	while (chart)
	{
		/* Start over */
		i = 0;

		/* Roll for nobility */
		roll = randint(100);

		/* Get the proper entry in the table */
		while ((chart != h_info[i].chart) || (roll > h_info[i].roll)) i++;

		/* Get the textual history */
		my_strcat(p_ptr->history, (h_text + h_info[i].text), sizeof(p_ptr->history));

		/* Add in the social class */
		social_class += (int)(h_info[i].bonus) - 50;

		/* Enter the next chart */
		chart = h_info[i].next;
	}



	/* Verify social class */
	if (social_class > 100) social_class = 100;
	else if (social_class < 1) social_class = 1;

	/* Save the social class */
	p_ptr->sc = social_class;
}


/*
 * Computes character's age and height
 */
static void get_ahw(void)
{
	/* Calculate the age */
	p_ptr->age = rp_ptr->b_age + randint(rp_ptr->m_age);

	/* Calculate the height for males */
	if (p_ptr->psex == SEX_MALE)
	{
		p_ptr->ht = Rand_normal(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
	}

	/* Calculate the height for females */
	else if (p_ptr->psex == SEX_FEMALE)
	{
		p_ptr->ht = Rand_normal(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
	}
}




/*
 * Get the player's starting money
 */
static void get_money(void)
{
	int i;

	int gold;

	/* Social Class determines starting gold */
	gold = (p_ptr->sc * 6) + randint(100) + 300;

	/* Process the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Mega-Hack -- reduce gold for high stats */
		if (stat_use[i] >= 18+50) gold -= 300;
		else if (stat_use[i] >= 18+20) gold -= 200;
		else if (stat_use[i] > 18) gold -= 150;
		else gold -= (stat_use[i] - 8) * 10;
	}

	/* Minimum 100 gold */
	if (gold < 100) gold = 100;

	/* Save the gold */
	p_ptr->au = gold;
}


/*
 * Structure for quickstart information
 */
quickstart_type normal_quickstart;

/*
 * Clear all the global "character" data
 */
static void player_wipe(void)
{
	int i, j;

	/* Copy player quickstart information */
	if (character_quickstart)
	{
		/* Copy across the quickstart structure */
		normal_quickstart.psex = p_ptr->psex;
		normal_quickstart.prace = p_ptr->prace;
		normal_quickstart.pclass = p_ptr->pclass;
		normal_quickstart.pstyle = p_ptr->pstyle;
		normal_quickstart.psval = p_ptr->psval;
		normal_quickstart.pschool = p_ptr->pschool;
		normal_quickstart.birth_au = p_ptr->birth_au;
		
		/* Copy across the stats */
		for (i = 0; i < A_MAX; i++)
		{
			/* Set up the stats */
			normal_quickstart.stat_birth[i] = p_ptr->stat_birth[i];
		}		
	}
	
	/* Wipe the player */
	(void)WIPE(p_ptr, player_type);

	/* Clear the inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_wipe(&inventory[i]);
	}

	/* Clear the bags */
	for (i = 0; i < SV_BAG_MAX_BAGS; i++)
	{
		for (j = 0; j < INVEN_BAG_TOTAL; j++)
		{
			bag_contents[i][j] = 0;
		}
	}

	/* Clear the maximum depths */
	for (i = 0; i < z_info->t_max; i++)
	{
		t_info[i].attained_depth = min_depth(i);
		t_info[i].visited = 0;
	}

	/* Start with no artifacts made yet */
	for (i = 0; i < z_info->a_max; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		a_ptr->cur_num = 0;
	}

	/* Reset the quests */
	for (i = 0; i < MAX_Q_IDX; i++)
	{
		quest_type *q_ptr = &q_list[i];

		/* Wipe the structure */
		(void)WIPE(q_ptr, quest_type);
	}

	/* Reset the "objects" */
	for (i = 1; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Reset "guess" */
		k_ptr->guess = 0;

		/* Reset "tried" */
		k_ptr->tried = FALSE;

		/* Reset "aware" */
		k_ptr->aware = FALSE;
	}


	/* Reset the "monsters" */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];

		/* Hack -- Reset the counter */
		r_ptr->cur_num = 0;

		/* Hack -- Reset the max counter */
		r_ptr->max_num = 100;

		/* Hack -- Reset the max counter */
		if (r_ptr->flags1 & (RF1_UNIQUE)) r_ptr->max_num = 1;

		/* Clear player kills */
		l_ptr->pkills = 0;
	}


	/* Hack -- no ghosts */
	r_info[z_info->r_max-1].max_num = 0;

	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;

	/* Hack -- Well rested player */
	p_ptr->rest = PY_REST_FULL - 1;

	/* None of the spells have been learned yet */
	for (i = 0; i < PY_MAX_SPELLS; i++) p_ptr->spell_order[i] = 0;
}


/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit(void)
{
	int i;
	const start_item *e_ptr;
	object_type *i_ptr;
	object_type object_type_body;
	int show_idx = 1;

	/* Hack -- Give the player his equipment */
	for (i = 0; i < MAX_CLASS_ITEMS + MAX_COMMON_ITEMS + 1; i++)
	{
		object_type *o_ptr;

		/* Access the item */
		if (i < MAX_CLASS_ITEMS) e_ptr = &(cp_ptr->start_items[i]);
		else e_ptr = &(common_items[i - MAX_CLASS_ITEMS]);

		/* Check the social class */
		if ((p_ptr->sc < e_ptr->social_min) || (p_ptr->sc > e_ptr->social_max)) continue;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Hack	-- Give the player an object */
		if (e_ptr->tval > 0)
		{
			int slot;

			/* Get the object_kind */
			s16b k_idx = lookup_kind(e_ptr->tval, e_ptr->sval);

			/* MegaHack -- undead start with 'foods' */
			if (rp_ptr->flags4 & (TR4_UNDEAD)) switch (e_ptr->tval)
			{
				case TV_FOOD:
				{
					switch(rand_int(4))
					{
						case 0:
							k_idx = lookup_kind(TV_FLASK, SV_FLASK_BLOOD);
							break;
						case 1:
							k_idx = lookup_kind(TV_BONE, SV_BONE_BONE);
							break;
						case 2:
							k_idx = lookup_kind(TV_BODY, SV_BODY_ARM);
							break;
						case 3:
							k_idx = lookup_kind(TV_BODY, SV_BODY_LEG);
							break;
					}
				}
			}

			/* Hack -- schools change starting spell book */
			if (p_ptr->pschool) switch (e_ptr->tval)
			{
				case TV_MAGIC_BOOK:
				case TV_PRAYER_BOOK:
				case TV_SONG_BOOK:
				{
					if (e_ptr->sval >= SV_BOOK_MAX_GOOD)
					{
						k_idx = lookup_kind(e_ptr->tval, e_ptr->sval - SV_BOOK_MAX_GOOD + p_ptr->pschool);
					}
				}
			}

			/* Hack -- style lookups to change basic equipment */
			if (p_ptr->pstyle) switch (e_ptr->tval)
			{
				case TV_SWORD:
				case TV_HAFTED:
				case TV_POLEARM:
				{
					switch (p_ptr->pstyle)
					{
						case WS_UNARMED:
						{
							k_idx = lookup_kind(TV_GLOVES, SV_SET_OF_CESTI);
							break;
						}
						case WS_SWORD:
						{
							k_idx = lookup_kind(TV_SWORD, SV_LONG_SWORD);
							break;
						}
						case WS_HAFTED:
						{
							k_idx = lookup_kind(TV_HAFTED, SV_WAR_HAMMER);
							break;
						}
						case WS_POLEARM:
						{
							k_idx = lookup_kind(TV_POLEARM, SV_PIKE);
							break;
						}
					}
					break;
				}

				case TV_BOW:
				case TV_INSTRUMENT:
				{
					switch (p_ptr->pstyle)
					{
					case WS_RING:
						  {
						    k_idx = lookup_kind(TV_RING, rand_int(1) ? SV_RING_AGGRAVATION : rand_int(5) ? SV_RING_TELEPORTATION : SV_RING_WOE);
						    break;
						  }
						case WS_TWO_WEAPON:
						  {
						    k_idx = lookup_kind(TV_SWORD, SV_DAGGER);
						    break;
						  }
						case WS_THROWN:
						  {
						    if (rp_ptr->r_tht > rp_ptr->r_thb)
							{
							  k_idx = lookup_kind(TV_SHOT, SV_AMMO_LIGHT);
							}
							else
							{
							  k_idx = lookup_kind(TV_BOW, SV_SLING);
							}
						    break;
						  }
						case WS_SLING:
						{
							k_idx = lookup_kind(TV_BOW, SV_SLING);
							break;
						}
						case WS_BOW:
						{
							k_idx = lookup_kind(TV_BOW, SV_LONG_BOW);
							break;
						}
						case WS_XBOW:
						{
							k_idx = lookup_kind(TV_BOW, SV_LIGHT_XBOW);
							break;
						}
					}
					break;	
				}

				case TV_SHOT:
				case TV_ARROW:
				case TV_BOLT:
				{
					switch (p_ptr->pstyle)
					{
						case WS_TWO_WEAPON:
						  {
						    /* Used to polish weapons */
						    k_idx = lookup_kind(TV_FLASK, SV_FLASK_OIL);
						    break;
						  }
						case WS_THROWN:
						  {
						    if (rp_ptr->r_tht > rp_ptr->r_thb)
						      {
							k_idx = lookup_kind(TV_SHOT, SV_AMMO_LIGHT);
						      }
						    else
						      {
							switch (randint(3))
							  {
							  case 1:
							    {
							      k_idx = lookup_kind(TV_SWORD, SV_DAGGER);
							      break;
							    }
							  case 2:
							    {
							      k_idx = lookup_kind(TV_POLEARM, SV_DART);
							      break;
							    }
							  case 3:
							    {
							      k_idx = lookup_kind(TV_SPIKE, 0);
							      break;
							    }
							  }
						      }
						    break;
						  }
						case WS_RING:
						  {
						    k_idx = lookup_kind(TV_ROPE, SV_ROPE_ELVEN);
						    break;
						  }
						case WS_SLING:
						{
							k_idx = lookup_kind(TV_SHOT, SV_AMMO_NORMAL);
							break;
						}
						case WS_BOW:
						{
							k_idx = lookup_kind(TV_ARROW, SV_AMMO_NORMAL);
							break;
						}
						case WS_XBOW:
						{
							k_idx = lookup_kind(TV_BOLT, SV_AMMO_NORMAL);
							break;
						}
					}
					break;
				}
			}

			/* Valid item? */
			if (!k_idx) continue;

			/* Prepare the item */
			object_prep(i_ptr, k_idx);
			i_ptr->number = (byte)rand_range(e_ptr->number_min, e_ptr->number_max);

			/* Modify the charges */
			if ((e_ptr->charge_min) && (e_ptr->charge_max)) i_ptr->charges = rand_range(e_ptr->charge_min, e_ptr->charge_max);

			/* Rings are mysterious and powerful */
			if (i_ptr->tval == TV_RING)
			  {
			    apply_magic(i_ptr, 50, FALSE, TRUE, TRUE);
			  }
			else
			  {
			    object_aware(i_ptr, FALSE);
			    object_known(i_ptr);
			  }

			/* Check the slot */
			slot = wield_slot(i_ptr);

			/* Ammo gets special rules */
			if (IS_QUIVER_SLOT(slot))
			{
				bool combined = FALSE;
				int k;

				/* Reset the slot. Check quiver space */
				slot = -1;

				if (quiver_carry_okay(i_ptr, i_ptr->number, -1))
				{
					/* Check quiver slots */
					for (k = INVEN_QUIVER; k < END_QUIVER; k++)
					{
						/* Get the slot */
						o_ptr = &inventory[k];

						/* Empty slot */
						if (!o_ptr->k_idx)
						{
							/* Remember the slot */
							slot = k;
						}
						/* Occupied slot */
						else if (object_similar(o_ptr, i_ptr))
						{
							/* Remember the slot */
							slot = k;

							/* Trigger object combination */
							combined = TRUE;

							/* Done */
							break;
						}
					}
				}

	 			/* Found a slot */
				if (slot != -1)
 				{
					/* Get the slot again */
					o_ptr = &inventory[slot];

					/* Check insertion mode */
					if (!combined)
					{
						/* Raw copy */
						object_copy(o_ptr, i_ptr);

						/* Hack -- Set a unique show_idx */
						o_ptr->show_idx = show_idx++;
					}
					else
					{
						/* Combine */
						object_absorb(o_ptr, i_ptr, FALSE);
					}

					/* Increase the weight by hand */
					p_ptr->total_weight += (i_ptr->weight * i_ptr->number);

					/* Compute quiver size */
					find_quiver_size();

					/* Reorder quiver, refresh slot */
					slot = reorder_quiver(slot);

					/* We have used up all the object */					
					i_ptr->number = 0;
				}
			}
			/* If player can wield an item, and slot not already occupied, do so */
			else if ((slot >= INVEN_WIELD) && !(inventory[slot].k_idx))
			{
				/* Get the wield slot */
				o_ptr = &inventory[slot];

				/* Wear the new stuff */
				object_copy(o_ptr, i_ptr);

				/* Wield one */
				o_ptr->number = 1;

				/* Reduce stack size */
				i_ptr->number--;

				/* Increment the equip counter by hand */
				p_ptr->equip_cnt++;

				/* Increase the weight */
				p_ptr->total_weight += o_ptr->weight;

				/* Hack -- Set a unique show_idx */
				o_ptr->show_idx = show_idx++;
			}

			/* Any left to carry? */
			if (i_ptr->number > 0)
			{
				/*put it in the inventory*/
				(void)inven_carry(i_ptr);

				/* Hack -- Assume we use the next slot */
				show_idx++;
			}
		}

		/*Bugfix:  So we don't get duplicate objects*/
		object_wipe (i_ptr);
	}
}



/* Locations of the tables on the screen */
#define HEADER_ROW		1
#define QUESTION_ROW	7
#define TABLE_ROW		10

#define QUESTION_COL	3
#define KEYBOARD_COL	0
#define DIFFICULTY_COL	12
#define QUICKSTART_COL	0
#define SEX_COL			0
#define RACE_COL		10
#define RACE_AUX_COL    27
#define RACE_AUX2_COL   37
#define CLASS_COL		27
#define CLASS_AUX_COL   43
#define CLASS_AUX2_COL  53
#define STYLE_COL       43
#define STYLE_AUX_COL   62
#define BOOK_COL		62
#define SCHOOL_COL		62
#define ROLLER_COL		62

#define INVALID_CHOICE 255


/*
 * Clear the previous question
 */
static void clear_question(void)
{
	int i;

	for (i = QUESTION_ROW; i < TABLE_ROW; i++)
	{
		/* Clear line, position cursor */
		Term_erase(0, i, 255);
	}
}

/*
 * Generic "get choice from menu" function
 */
static int get_player_choice(birth_menu *choices, int num, int col, int wid,
     cptr helpfile, void (*hook)(birth_menu))
{
	int top = 0, cur = 0;
	int i, dir;
	key_event ke;
	char buf[80];
	bool done = FALSE;
	int hgt;
	byte attr;
	int delay = 0;

	/* Autoselect if able */
	if (num == 1) done = TRUE;

	/* Clear */
	for (i = TABLE_ROW; i < Term->hgt; i++)
	{
		/* Clear */
		Term_erase(col, i, Term->wid - wid);	
	}

	/* Choose */
	while (TRUE)
	{
		hgt = Term->hgt - TABLE_ROW - 1;

		/* Redraw the list */
		for (i = 0; ((i + top < num) && (i <= hgt)); i++)
		{
			if (i + top < 26)
			{
				sprintf(buf, "%c) %s", I2A(i + top), choices[i + top].name);
			}
			else
			{
				/* ToDo: Fix the ASCII dependency */
				sprintf(buf, "%c) %s", 'A' + (i + top - 26), choices[i + top].name);
			}

			/* Clear */
			Term_erase(col, i + TABLE_ROW, wid);

			/* Display */
			if (i == (cur - top))
			{
				/* Highlight the current selection */
				if (choices[i + top].ghost) attr = TERM_BLUE;
				else attr = TERM_L_BLUE;
			}
			else
			{
				if (choices[i + top].ghost) attr = TERM_SLATE;
				else attr = TERM_WHITE;
			}

			Term_putstr(col, i + TABLE_ROW, wid, attr, buf);
		}

		/* Display auxiliary information if any is available. */
		if (hook) hook(choices[cur]);

		/* Move the cursor */
		put_str("", TABLE_ROW + cur - top, col);

		if (delay)
		{
			/* Force screen update */
			Term_fresh();

			/* Delay */
			Term_xtra(TERM_XTRA_DELAY, delay);

			delay = 0;
		}

		if (done) return (choices[cur].choice);

		ke = inkey_ex();

		if (ke.key == KTRL('X'))
		{
			quit(NULL);
		}
		if (ke.key == ESCAPE)
		{
			/* Mega Hack - go back. */
			return (INVALID_CHOICE);
		}
		if (ke.key == '*')
		{
			int count = 0;
			
			/* Select a legal choice at random */
			while (count++ < 100)
			{
				cur = rand_int(num);
				if (!choices[cur].ghost) break;
			}

			/* Move it onto the screen */
			if ((cur < top) || (cur > top + hgt))
			{
				top = cur;
			}

			/* Done */
			done = TRUE;
		}
		else if (ke.key == '?')
		{
			sprintf(buf, "%s#%s", helpfile, choices[cur].name);

			screen_save();
			(void)show_file(buf, NULL, 0, 0);
			(void)show_file("birth.hlp", NULL, 0, 0);
			screen_load();
		}
		else if (ke.key == '=')
		{
			do_cmd_menu(MENU_OPTIONS, "options");
		}
		else if ((ke.key == '\n') || (ke.key == '\r'))
		{
			/* Done */
			return (choices[cur].choice);
		}
		else if (ke.key == '\xff')
		{
			int row = ke.mousey - TABLE_ROW + top;

			if (ke.mousebutton)
			{
				if ((row >= 0) && (row < num) && (row < hgt)
						&& (ke.mousex >= col)
						&& (ke.mousex < col + (int)strlen(choices[top + row].name)))
				{
					cur = row;
					done = TRUE;
				}
			}
			else if ((ke.mousex >= col) && (ke.mousex <= col + wid))
			{
				if ((row >= 0) && (row < num) && (row < hgt)
						&& (ke.mousex < col + (int)strlen(choices[top + row].name))) cur = row;

				/* Scroll up */
				if ((top > 0) && ((row - top) < 4))
				{
					/* Scroll up */
					top--;

					/* Delay after update */
					delay = 100;
				}

				/* Scroll down */
				if ((top + hgt < (num - 1)) && ((top + hgt - row) < 4))
				{
					/* Scroll down */
					top++;

					/* Delay after update */
					delay = 100;
				}
			}
		}
		else if (isdigit(ke.key))
		{
			/* Get a direction from the key */
			dir = target_dir(ke.key);

			/* Going up? */
			if (dir == 8)
			{
				if (cur != 0)
				{
					/* Move selection */
					cur--;
				}

				if ((top > 0) && ((cur - top) < 4))
				{
					/* Scroll up */
					top--;
				}
			}

			/* Going down? */
			if (dir == 2)
			{
				if (cur != (num - 1))
				{
					/* Move selection */
					cur++;
				}

				if ((top + hgt < (num - 1)) && ((top + hgt - cur) < 4))
				{
					/* Scroll down */
					top++;
				}
			}
		}
		else if (isalpha(ke.key))
		{
			int choice;

			if (islower(ke.key))
			{
				choice = A2I(ke.key);
			}
			else
			{
				choice = ke.key - 'A' + 26;
			}

			/* Validate input */
			if ((choice > -1) && (choice < num))
			{
				cur = choice;

				/* Move it onto the screen */
				if ((cur < top) || (cur > top + hgt))
				{
					top = cur;
				}

				/* Done */
				done = TRUE;
			}
			else
			{
				bell("Illegal response to question!");
			}
		}

		/* Invalid input */
		else bell("Illegal response to question!");
	}

	return (INVALID_CHOICE);
}


/*
 * Display additional information about each race during the selection.
 */
static void race_aux_hook(birth_menu r_str)
{
	int race, i;
	char s[50];
	byte likert_attr;
	cptr desc;
	int xthn, xthb, xtht, xsrh, xdig;
	int xdis, xdev, xsav, xstl;

	/* Get the race */
	race = r_str.choice;

	/* Save the race pointer */
	rp_ptr = &p_info[race];

	/* Display relevant details. */
	for (i = 0; i < A_MAX; i++)
	{
		sprintf(s, "%s%+d ", stat_names_reduced[i],
		rp_ptr->r_adj[i]);
		Term_putstr(RACE_AUX_COL, TABLE_ROW + i, -1, TERM_WHITE, s);
	}

	/* Process stats */
	for (i = 0; i < A_MAX; i++)
	  {
	    /* Obtain a "bonus" for "race" and "class" */
	    int bonus_add = rp_ptr->r_adj[i];

	    /* Get the minimally increased stat for the bonuses */
	    int value_min = adjust_stat(10, bonus_add, 1);

	    /* Get the maximally increased stat for the bonuses */
	    int value_max = adjust_stat(10, bonus_add, 99);

	    /* Get the stat increased by the average for the bonuses */
	    int value = value_min + (value_max - value_min) / 2;

	    /* Apply the racial bonuses */
	    p_ptr->stat_cur[i] = p_ptr->stat_max[i] = value;
	  }

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp;

	sprintf(s, "Experience: %d%%  ", p_ptr->expfact);
	Term_putstr(RACE_AUX2_COL, TABLE_ROW + A_MAX + 2, -1, TERM_WHITE, s);
	sprintf(s, "Infravision: %d ft  ", rp_ptr->infra * 10);
	Term_putstr(RACE_AUX2_COL, TABLE_ROW + A_MAX + 3, -1, TERM_WHITE, s);

	/* Skills - scaled up to exaggerate differences */
	xthn = rp_ptr->r_thn * 2 + 24;
	xthb = rp_ptr->r_thb * 2 + 24;
	xtht = rp_ptr->r_tht * 2 + 24;
	xdis = rp_ptr->r_dis * 2 + 16;
	xdev = rp_ptr->r_dev * 2 + 12;
	xsav = rp_ptr->r_sav * 2 + 12;
	xstl = rp_ptr->r_stl * 2 + 2;
	xsrh = rp_ptr->r_srh * 2 + 12;
	xdig = rp_ptr->r_dig * 2 + 2;

	put_str("Fighting", TABLE_ROW, RACE_AUX2_COL);
	desc = likert(xthn, 12, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW, RACE_AUX2_COL+11);

	put_str("Shooting", TABLE_ROW + 1, RACE_AUX2_COL);
	desc = likert(xthb, 12, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 1, RACE_AUX2_COL+11);

	put_str("Throwing", TABLE_ROW + 2, RACE_AUX2_COL);
	desc = likert(xtht, 12, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 2, RACE_AUX2_COL+11);

	put_str("Stealth", TABLE_ROW + 3, RACE_AUX2_COL);
	desc = likert(xstl, 1, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 3, RACE_AUX2_COL+11);

	put_str("Save Throw", TABLE_ROW + 4, RACE_AUX2_COL);
	desc = likert(xsav, 6, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 4, RACE_AUX2_COL+11);

	put_str("Disarming", TABLE_ROW + 5, RACE_AUX2_COL);
	desc = likert(xdis, 8, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 5, RACE_AUX2_COL+11);

	put_str("Devices", TABLE_ROW + 6, RACE_AUX2_COL);
	desc = likert(xdev, 6, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 6, RACE_AUX2_COL+11);

	put_str("Searching", TABLE_ROW + 7, RACE_AUX2_COL);
	desc = likert(xsrh, 6, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 7, RACE_AUX2_COL+11);

	put_str("Digging", TABLE_ROW + 8, RACE_AUX2_COL);
	desc = likert(xdig, 6, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 8, RACE_AUX2_COL+11);
}


/*
 * Player race
 */
static bool get_player_race()
{
	int i, j = 0;
	birth_menu *races;

	races = C_ZNEW(z_info->g_max, birth_menu);

	/* Extra info */
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		"Your 'race' determines various intrinsic factors and bonuses.");
	if (!birth_intermediate) Term_putstr(QUESTION_COL, QUESTION_ROW + 1, -1, TERM_YELLOW,
	    "Any greyed-out entries should only be used by advanced players.");

	/* Tabulate races */
	for (i = 0; i < z_info->g_max; i++)
	{
		/* Intermediates only get races that they know about */
		if ((birth_intermediate) && (p_info[i].r_idx) && !(l_list[p_info[i].r_idx].tkills)) continue;

		/* Add race to list */
		races[j].name = p_name + p_info[i].name;
		races[j].choice = i;
		races[j++].ghost = (p_info[i].r_idx != 0);
	}

	/* Get the player race */
	p_ptr->prace = get_player_choice(races, j, RACE_COL, RACE_AUX_COL - RACE_COL - 1,
		"races.txt", race_aux_hook);

	/* No selection? */
	if (p_ptr->prace == INVALID_CHOICE)
	{
		p_ptr->prace = 0;

		FREE(races);

		return (FALSE);
	}

	/* Save the player shape */
	p_ptr->pshape = p_ptr->prace;

	/* Save the starting town */
	p_ptr->town = rp_ptr->home;
	p_ptr->dungeon = 7;

	FREE(races);

	/* Success */
	return (TRUE);
}


/*
 * Display additional information about each class during the selection.
 */
static void class_aux_hook(birth_menu c_str)
{
	int class_idx, i;
	char s[128];
	byte likert_attr;
	cptr desc;
	int xthn, xthb, xtht, xsrh, xdig;
	int xdis, xdev, xsav, xstl;

	/* Get the class */
	class_idx = c_str.choice;

	/* Set class */
	cp_ptr = &c_info[class_idx];

	/* Display relevant details. */
	for (i = 0; i < A_MAX; i++)
	{
		sprintf(s, "%s%+d ", stat_names_reduced[i],
		cp_ptr->c_adj[i] + rp_ptr->r_adj[i]);
		Term_putstr(CLASS_AUX_COL, TABLE_ROW + i, -1, TERM_WHITE, s);
	}

	/* Process stats */
	for (i = 0; i < A_MAX; i++)
	  {
	    /* Obtain a "bonus" for "race" and "class" */
	    int bonus_add = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

	    /* Get the minimally increased stat for the bonuses */
	    int value_min = adjust_stat(10, bonus_add, 1);

	    /* Get the maximally increased stat for the bonuses */
	    int value_max = adjust_stat(10, bonus_add, 99);

	    /* Get the stat increased by the average for the bonuses */
	    int value = value_min + (value_max - value_min) / 2;

	    /* Apply the racial/class bonuses */
	    p_ptr->stat_cur[i] = p_ptr->stat_max[i] = value;
	  }

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

	sprintf(s, "Experience: %d%%  ", p_ptr->expfact);
	Term_putstr(CLASS_AUX2_COL, TABLE_ROW + A_MAX + 2, -1, TERM_WHITE, s);
	sprintf(s, "Infravision: %d ft  ", rp_ptr->infra * 10);
	Term_putstr(CLASS_AUX2_COL, TABLE_ROW + A_MAX + 3, -1, TERM_WHITE, s);


	/* Skills */
	xthn = rp_ptr->r_thn + cp_ptr->c_thn + cp_ptr->x_thn;
	xthb = rp_ptr->r_thb + cp_ptr->c_thb + cp_ptr->x_thb;
	xtht = rp_ptr->r_tht + cp_ptr->c_dis + cp_ptr->x_tht;
	xdis = rp_ptr->r_dis + cp_ptr->c_dis + cp_ptr->x_dis;
	xdev = rp_ptr->r_dev + cp_ptr->c_dev + cp_ptr->x_dev;
	xsav = rp_ptr->r_sav + cp_ptr->c_sav + cp_ptr->x_sav;
	xstl = rp_ptr->r_stl + cp_ptr->c_stl + cp_ptr->x_stl;
	xsrh = rp_ptr->r_srh + cp_ptr->c_srh + cp_ptr->x_srh;
	xdig = rp_ptr->r_dig + cp_ptr->c_dig + cp_ptr->x_dig;

	put_str("Fighting", TABLE_ROW, CLASS_AUX2_COL);
	desc = likert(xthn, 12, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW, CLASS_AUX2_COL+11);

	put_str("Shooting", TABLE_ROW + 1, CLASS_AUX2_COL);
	desc = likert(xthb, 12, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 1, CLASS_AUX2_COL+11);

	put_str("Throwing", TABLE_ROW + 2, CLASS_AUX2_COL);
	desc = likert(xtht, 12, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 2, CLASS_AUX2_COL+11);

	put_str("Stealth", TABLE_ROW + 3, CLASS_AUX2_COL);
	desc = likert(xstl, 1, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 3, CLASS_AUX2_COL+11);

	put_str("Save Throw", TABLE_ROW + 4, CLASS_AUX2_COL);
	desc = likert(xsav, 6, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 4, CLASS_AUX2_COL+11);

	put_str("Disarming", TABLE_ROW + 5, CLASS_AUX2_COL);
	desc = likert(xdis, 8, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 5, CLASS_AUX2_COL+11);

	put_str("Devices", TABLE_ROW + 6, CLASS_AUX2_COL);
	desc = likert(xdev, 6, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 6, CLASS_AUX2_COL+11);

	put_str("Searching", TABLE_ROW + 7, CLASS_AUX2_COL);
	desc = likert(xsrh, 6, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 7, CLASS_AUX2_COL+11);

	put_str("Digging", TABLE_ROW + 8, CLASS_AUX2_COL);
	desc = likert(xdig, 6, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), TABLE_ROW + 8, CLASS_AUX2_COL+11);	
}


/*
 * Player class
 */
static bool get_player_class(void)
{
	int  i, k = 0;
	birth_menu *classes;

	classes = C_ZNEW(z_info->c_max, birth_menu);

	/* Extra info */
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		"Your 'class' determines various intrinsic abilities and bonuses.");
	if (!birth_intermediate) Term_putstr(QUESTION_COL, QUESTION_ROW + 1, -1, TERM_YELLOW,
	    "Any greyed-out entries should only be used by advanced players.");

	/* Tabulate classes */
	for (i = 0; i < z_info->c_max; i++)
	{
		/* Don't ghost classes by default */
		bool ghost = FALSE;

		/* Ghost if not warrior, and two stats are -4 or below, or 1 is -5 or below */
		if (i)
		{
			int j;
			bool minus_4 = FALSE;
			
			for (j = 0; j < A_MAX; j++)
			{
			    /* Obtain a "bonus" for "race" and "class" */
				int value = rp_ptr->r_adj[j] + c_info[i].c_adj[j];

				if ((value <= -5) || (minus_4 && value <= -4))
				{
					ghost = TRUE;
				}
				else if (value <= -4)
				{
					minus_4 = TRUE;
				}
			}
		}

		/* 'Ghosted' entries unavailable for intermediate players */
		if (birth_intermediate && ghost) continue;
		
		/* Save the string */
		classes[k].name = c_name + c_info[i].name;
		classes[k].choice = i;
		
		/* Save the ghosting */
		classes[k++].ghost = ghost;
	}

	p_ptr->pclass = get_player_choice(classes, k, CLASS_COL, CLASS_AUX_COL - CLASS_COL - 1,
				      "classes.txt", class_aux_hook);

	/* No selection? */
	if (p_ptr->pclass == INVALID_CHOICE)
	{
		p_ptr->pclass = 0;

		FREE(classes);

		return (FALSE);
	}

	FREE(classes);

	return (TRUE);
}


/*
 * Display additional information about each class during the selection.
 */
static void style_aux_hook(birth_menu w_str)
{
	int style_idx,i;
	char s[128];

	/* Style index */
	style_idx = w_str.choice;

	/* Display relevant details. */
	for (i = 0; i < A_MAX; i++)
	{
		sprintf(s, "%s%+d ", stat_names_reduced[i],
		cp_ptr->c_adj[i] + rp_ptr->r_adj[i]);
		Term_putstr(STYLE_AUX_COL, TABLE_ROW + i, -1, TERM_WHITE, s);
	}

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp + (style_idx ? 10 : 0);

	sprintf(s, "Experience: %d%%  ", p_ptr->expfact);
	Term_putstr(STYLE_AUX_COL, TABLE_ROW + A_MAX + 2, -1, TERM_WHITE, s);
	sprintf(s, "Infravision: %d ft  ", rp_ptr->infra * 10);
	Term_putstr(STYLE_AUX_COL, TABLE_ROW + A_MAX + 3, -1, TERM_WHITE, s);
}


/*
 * Player class
 */
static bool get_player_style(void)
{
	int i, j = 0;
	birth_menu styles[MAX_WEAP_STYLES];
	u32b style;

	/*** Player weapon speciality ***/


	style = (1L<<WS_NONE);	/* Every class can choose this style */


	/* Collect styles */
	for (i = 0;i< z_info->w_max;i++)
	{
		if (w_info[i].class != p_ptr->pclass) continue;

		style |= w_info[i].styles;
	}

	/* Analyse styles */
	for (i = 0;i<MAX_WEAP_STYLES;i++)
	{
		if (style & (1L<<i))
		{
			/* Save the string */
			styles[j].name = w_name_style[i];
			styles[j].choice = i;
			styles[j++].ghost = FALSE;
		}
	}
	
	/* Has one choice */
	if (j == 1)
	{
		p_ptr->pstyle = styles[0].choice;
		
		return (TRUE);
	}

	/* Hack */
	styles[0].ghost = TRUE;

	/* Extra info */
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
					"Your 'style' determines under what circumstances you get extra bonuses.");

	p_ptr->pstyle = get_player_choice(styles, j, STYLE_COL, STYLE_AUX_COL - STYLE_COL - 1,
				   "styles.txt", style_aux_hook);

	/* No selection? */
	if (p_ptr->pstyle == INVALID_CHOICE)
	{
		p_ptr->pstyle = 0;

		return (FALSE);
	}

	return (TRUE);
}


/*
 * Player book
 */
static bool get_player_book(void)
{
	int     i, j = 0;
	birth_menu *books;

	/* Hack -- until we set up schools for song books,
	 * we use this to determine whether we check sval < SV_BOOK_MAX_GOOD
	 * or sval >= SV_BOOK_MIN_GOOD */
	bool max_good = FALSE;
	
	/*** Player book speciality ***/

	int bookc = 0;

	object_kind *k_ptr;

	int tval=0;
	
	cptr help = NULL;

	switch (p_ptr->pstyle)
	{
		case WS_MAGIC_BOOK:
			tval = TV_MAGIC_BOOK;
			max_good = TRUE;
			help = "magic.txt";
			break;

		case WS_PRAYER_BOOK:
			tval = TV_PRAYER_BOOK;
			max_good = TRUE;
			help = "prayers.txt";
			break;

		case WS_SONG_BOOK:
			tval = TV_SONG_BOOK;
			help = "songs.txt";
			break;
			
		default:
			return (FALSE);
	}

	/* Count books */
	for (i = 0;i<z_info->k_max;i++)
	{
		k_ptr = &k_info[i];

		/* Hack -- ignore books that the player has not seen yet */
		if ((birth_intermediate) && !(k_ptr->aware) && (max_good ? k_ptr->sval < SV_BOOK_MAX_GOOD : k_ptr->sval >= SV_BOOK_MIN_GOOD)) continue;
		
		/* Hack -- count one of non-dungeon books per school */
		if ((k_ptr->sval >= SV_BOOK_MAX_GOOD) && (k_ptr->sval % SV_BOOK_SCHOOL /* != SV_BOOK_SCHOOL - 1 */)) continue;

		/* Hack -- count one of non-dungeon books per school */
		if (!(max_good) && (k_ptr->sval < SV_BOOK_MIN_GOOD) && (k_ptr->sval % SV_BOOK_SCHOOL != SV_BOOK_SCHOOL - 1)) continue;

		/* Book */
		if (k_ptr->tval == tval) bookc++;
	}

	/* No books */
	if (!bookc) return (FALSE);

	books = C_ZNEW(bookc, birth_menu);

	/* Analyse books */
	for (i = 0;i<z_info->k_max;i++)
	{	
		/* Check spells with pre-requisites */
		k_ptr = &k_info[i];

		/* Hack -- ignore books that the player has not seen yet */
		if ((birth_intermediate) && !(k_ptr->aware) && (max_good ? k_ptr->sval < SV_BOOK_MAX_GOOD : k_ptr->sval >= SV_BOOK_MIN_GOOD)) continue;
		
		/* Hack -- count one of non-dungeon books per school */
		if ((max_good) && (k_ptr->sval >= SV_BOOK_MAX_GOOD) && (k_ptr->sval % SV_BOOK_SCHOOL != SV_BOOK_SCHOOL - 1)) continue;

		/* Hack -- count one of non-dungeon books per school */
		if (!(max_good) && (k_ptr->sval < SV_BOOK_MIN_GOOD) && (k_ptr->sval % SV_BOOK_SCHOOL != SV_BOOK_SCHOOL - 1)) continue;

		/* Correct tval */
		if (k_ptr->tval == tval)
		{		
			/* Save the string. Note offset to skip 'of ' */
			books[j].name = k_name + k_ptr->name + 3;
			books[j].choice = k_ptr->sval;
			books[j++].ghost = FALSE;
		}
	}

	if (j == 1)
	{
		p_ptr->psval = books[0].choice;

		FREE(books);

		return (TRUE);
	}

	/* Extra info */
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		    "Your 'book' determines which spell book you benefit most using.");
	if (!birth_intermediate) Term_putstr(QUESTION_COL, QUESTION_ROW + 1, -1, TERM_YELLOW,
	    "Any greyed-out entries should only be used by advanced players.");

	p_ptr->psval = get_player_choice(books, j, BOOK_COL, 80 - BOOK_COL - 1,
				     help, NULL);

	FREE(books);

	/* No selection? */
	if (p_ptr->psval == INVALID_CHOICE)
	{
		p_ptr->psval = 0;

		return (FALSE);
	}

	return (TRUE);
}


/*
 * Player school
 */
static bool get_player_school(void)
{
	int     i, j, k = 0;
	birth_menu *schools;

	/*** Player school speciality ***/

	int schoolc = 0;

	object_kind *k_ptr;

	int tval=0;

	cptr text = NULL;
	cptr help = NULL;
	
	switch (p_ptr->pstyle)
	{
		case WS_MAGIC_BOOK:
			tval = TV_MAGIC_BOOK;
			break;

		case WS_PRAYER_BOOK:
			tval = TV_PRAYER_BOOK;
			break;

		case WS_SONG_BOOK:
			tval = TV_SONG_BOOK;
			break;
			
		default:
			tval = c_info[p_ptr->pclass].spell_book;
			break;
	}

	switch (tval)
	{
		case TV_MAGIC_BOOK:
			text = "school";
			help = "schools.txt";
			break;
	
		case TV_PRAYER_BOOK:
			text = "religion";
			help = "religions.txt";
			break;
	
		case TV_SONG_BOOK:
			text = "college";
			help = "colleges.txt";
			break;
	}
	
	
	/* No spell book style */
	if (!tval) return (TRUE);

	/* Count schools */
	for (i = 0;i<z_info->k_max;i++)
	{
		k_ptr = &k_info[i];

		/* Hack -- count one of non-dungeon books per school */
		if ((k_ptr->sval < SV_BOOK_MAX_GOOD) || (k_ptr->sval % SV_BOOK_SCHOOL != SV_BOOK_SCHOOL - 1)) continue;

		if (k_ptr->tval == tval) schoolc++;
	}

	if (!schoolc) return (TRUE);

	schools = C_ZNEW(schoolc, birth_menu);

	/* Analyse books */
	for (i = 0;i<z_info->k_max;i++)
	{
		k_ptr = &k_info[i];
		
		/* Hack -- count one of non-dungeon books per school */
		if ((k_ptr->sval < SV_BOOK_MAX_GOOD) || (k_ptr->sval % SV_BOOK_SCHOOL != SV_BOOK_SCHOOL - 1)) continue;

		/* 'School' specialists cannot learn spells from basic 'school' books other than their school */
		if (p_ptr->psval >= SV_BOOK_MAX_GOOD)
		{
			/* Sval hackery */
			if (k_ptr->sval - (k_ptr->sval % SV_BOOK_SCHOOL) + SV_BOOK_SCHOOL - 1 != p_ptr->psval) continue;
		}

		if (k_ptr->tval == tval)		
		{
			/* Save the string. Note offset to skip 'of ' */
			schools[k].name = k_name + k_ptr->name + 3;
			schools[k].choice = k_ptr->sval - SV_BOOK_SCHOOL + 1;
			schools[k].ghost = FALSE;
			
			/* Mega-hack for ghosting starting mages/rangers/artisans */
			switch (p_ptr->pclass)
			{
				case 1: if ((tval == TV_MAGIC_BOOK) && (k_ptr->sval >= SV_BOOK_MAX_GOOD + 20)) schools[k].ghost = TRUE; break;
				case 3: if ((tval == TV_MAGIC_BOOK) && (k_ptr->sval < SV_BOOK_MAX_GOOD + 20)) schools[k].ghost = TRUE; break;
				case 4: if ((tval == TV_MAGIC_BOOK) && (k_ptr->sval < SV_BOOK_MAX_GOOD + 20)) schools[k].ghost = TRUE; break;
				default:
				{
					/* Gifted or chosen mage */
					s16b book[26];
					int num;
					object_type *o_ptr;
					object_type object_type_body;
					
					/* Get the object */
					o_ptr = &object_type_body;
					
					/* Check if there are valid spells in the 'first' book */
					object_prep(o_ptr, lookup_kind(tval, k_ptr->sval - 3));
					
					fill_book(o_ptr, book, &num);
					schools[k].ghost = TRUE;
					
					for (j=0; j < num; j++)
					{
						if (spell_legible(book[j])) schools[k].ghost = FALSE;
					}
					break;
				}
			}
			
			k++;
		}
	}

	if (k == 1)
	{
		p_ptr->pschool = schools[0].choice;

		FREE(schools);

		return (TRUE);
	}

	/* Extra info */
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		    format("Your '%s' determines which spell book you start with.", text));
	Term_putstr(QUESTION_COL, QUESTION_ROW + 1, -1, TERM_YELLOW,
	    "Any greyed-out entries should only be used by advanced players.");

	p_ptr->pschool = get_player_choice(schools, k, SCHOOL_COL, 80 - SCHOOL_COL - 1,
				     help, NULL);

	FREE(schools);

	/* No selection? */
	if (p_ptr->pschool == INVALID_CHOICE)
	{
		p_ptr->pschool = 0;

		return (FALSE);
	}

	return (TRUE);
}


#define MAX_ROLLER_CHOICES	3

/*
 * Player roller
 */
static bool get_player_roller(void)
{
	int     choice;
	birth_menu roller[MAX_ROLLER_CHOICES] =
	{
		{TRUE, "Just roll", 0},
		{FALSE, "Choose minimum", 1},
		{FALSE,	"Spend points", 2},		
	};

	/*** Player roller choice ***/

	/* Extra info */
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		    "Choose how you specify your starting stats.");

	choice = get_player_choice(roller, MAX_ROLLER_CHOICES, ROLLER_COL, 80 - ROLLER_COL - 1,
				     "rollers.txt", NULL);

	/* Selection? */
	switch(choice)
	{
		case INVALID_CHOICE:
			return (FALSE);
	
		case 0:
			birth_point_based = FALSE;
			birth_auto_roller = FALSE;
			break;
		case 1:
			birth_point_based = FALSE;
			birth_auto_roller = TRUE;
			break;
		case 2:
			birth_point_based = TRUE;
			birth_auto_roller = FALSE;
			break;
	}

	return (TRUE);
}



/*
 * Player sex
 */
static bool get_player_sex(void)
{
	int i;
	birth_menu genders[2];

	/* Extra info */
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		"Your 'sex' does not have any significant gameplay effects.");

	/* Tabulate genders */
	for (i = 0; i < 2; i++)
	{
		genders[i].name = sex_info[i].title;
		genders[i].choice = i;
		genders[i].ghost = FALSE;
	}

	p_ptr->psex = get_player_choice(genders, 2, SEX_COL, 10,
				 "birth.txt",   NULL);

	/* No selection? */
	if (p_ptr->psex == INVALID_CHOICE)
	{
		p_ptr->psex = 0;
		return (FALSE);
	}

	/* Save the sex pointer */
	sp_ptr = &sex_info[p_ptr->psex];

	return (TRUE);
}


#define MAX_DIFFICULTY_CHOICES	4

/*
 * Player difficulty
 */
static bool get_player_difficulty(void)
{
	int     choice;
	birth_menu difficulty[MAX_DIFFICULTY_CHOICES] =
	{
		{FALSE, "Beginner", 0},
		{FALSE, "Played roguelikes before", 1 },
		{FALSE, "Played Angband before", 2 },
		{FALSE, "Played Unangband before", 3}
	};

	/*** Player roller choice ***/

	/* Extra info */
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		    "Describe your knowledge of playing Unangband and related games.");

	choice = get_player_choice(difficulty, MAX_DIFFICULTY_CHOICES, DIFFICULTY_COL, 80 - DIFFICULTY_COL - 1,
				     "difficulty.txt", NULL);

	/* Selection? */
	switch(choice)
	{
		case INVALID_CHOICE:
			return(FALSE);

		case 0:
			birth_beginner = TRUE;
			birth_small_levels = FALSE;
			birth_intermediate = FALSE;
			break;
		case 1:
			birth_beginner = FALSE;
			birth_small_levels = TRUE;
			birth_intermediate = TRUE;
			break;
		case 2:
			birth_beginner = FALSE;
			birth_small_levels = FALSE;
			birth_intermediate = TRUE;
			break;
		case 3:
			birth_beginner = FALSE;
			birth_small_levels = FALSE;
			birth_intermediate = FALSE;
			break;
	}

	return (TRUE);
}


#define MAX_KEYBOARD_CHOICES	2

/*
 * Player difficulty
 */
static bool get_player_keyboard(void)
{
	int     choice;
	birth_menu keyboard[MAX_KEYBOARD_CHOICES] =
	{
		{FALSE, "Desktop", 0},
		{FALSE, "Laptop", 1}
	};
	
	/*** Player roller choice ***/

	/* Extra info */
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		    "Choose your current keyboard layout.");

	choice = get_player_choice(keyboard, MAX_KEYBOARD_CHOICES, KEYBOARD_COL, DIFFICULTY_COL - KEYBOARD_COL - 1,
				     "keyboard.txt", NULL);

	/* Selection? */
	switch(choice)
	{
		case INVALID_CHOICE:
			return(FALSE);

		case 0:
			rogue_like_commands = FALSE;
			break;
		case 1:
			rogue_like_commands = TRUE;
			break;
	}

	return (TRUE);
}

#define MAX_QUICKSTART_CHOICES	2

/*
 * Player difficulty
 */
static bool get_player_quickstart(void)
{
	int     choice;
	birth_menu quickstart[MAX_QUICKSTART_CHOICES] =
	{
		{FALSE, "Yes", 0},
		{FALSE, "No", 1}
	};
	
	/*** Player roller choice ***/

	/* Extra info */
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		    "Quick start the game using the same choices as the last character?");

	choice = get_player_choice(quickstart, MAX_QUICKSTART_CHOICES, QUICKSTART_COL, 80 - QUICKSTART_COL - 1,
				     "quickstart.txt", NULL);

	/* Selection? */
	switch(choice)
	{
		case INVALID_CHOICE:
			return(FALSE);

		case 0:
			birth_quickstart = TRUE;
			break;
		case 1:
			birth_quickstart = FALSE;
			break;
	}

	return (TRUE);
}


/*
 * Structure used for a beginner quickstart.
 * 
 * Race is Maia, class is Istari, no speciality.
 * 
 * All stats start at 15.
 */
quickstart_type beginner_quickstart =
{
	SEX_MALE,
	RACE_MAIA,
	CLASS_ISTARI,
	0, 		/* No style */
	0,		/* No substyle */
	0,		/* No school */
	{15, 15, 15, 15, 15, 15, 15, 15},
	100L
};


/*
 * Quick start a character. Takes a quick start structure and fills in the
 * required values.
 */
static void player_birth_quickstart(quickstart_type *q_ptr)
{
	int i;
	
	/* Copy across the quickstart structure */
	p_ptr->psex = q_ptr->psex;
	p_ptr->pshape = p_ptr->prace = q_ptr->prace;
	p_ptr->pclass = q_ptr->pclass;
	p_ptr->pstyle = q_ptr->pstyle;
	p_ptr->psval = q_ptr->psval;
	p_ptr->pschool = q_ptr->pschool;
	
	/* Set up the class and race */
	sp_ptr = &sex_info[p_ptr->psex];
	rp_ptr = &p_info[p_ptr->prace];
	cp_ptr = &c_info[p_ptr->pclass];

	/* Copy across the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Set up the stats */
		p_ptr->stat_birth[i] = p_ptr->stat_cur[i] = p_ptr->stat_max[i] = q_ptr->stat_birth[i];
	}

	/* Roll for age/height/weight */
	get_ahw();

	/* Roll for social class */
	get_history();

	/* Gold get birth gold */
	p_ptr->au = p_ptr->birth_au = q_ptr->birth_au;

	/* Calculate the bonuses and hitpoints */
	p_ptr->update |= (PU_BONUS | PU_HP);

	/* Update stuff */
	update_stuff();

	/* Fully healed */
	p_ptr->chp = p_ptr->mhp;

	/* Fully rested */
	p_ptr->csp = p_ptr->msp;
	
	/* Set up secondary stats */
	p_ptr->town = rp_ptr->home;
	p_ptr->dungeon = 7;
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp + (p_ptr->pstyle ? 10 : 0);

	/* Display the player */
	display_player(0);
}




/*
 * Helper function for 'player_birth()'.
 *
 * This function allows the player to select a sex, race, and class, and
 * modify options (including the birth options).
 *
 * Taken from Zangband via Eyangband
 */
static bool player_birth_aux_1(void)
{
	/*** Instructions ***/

	/* Clear screen */
	Term_clear();

	/* Display some helpful information */
	Term_putstr(QUESTION_COL, HEADER_ROW + 2, -1, TERM_WHITE,
		    "Use the movement keys to scroll the menu, 'Enter' to select the current");
	Term_putstr(QUESTION_COL, HEADER_ROW + 3, -1, TERM_WHITE,
		    "menu item, '*' for a random menu item, 'ESC' to restart the character");
	Term_putstr(QUESTION_COL, HEADER_ROW + 4, -1, TERM_WHITE,
		    "selection, '=' for the birth options, '?' for help, or 'Ctrl-X' to quit.");

	/* Level one */
	p_ptr->max_lev = p_ptr->lev = 1;

	/* First time player */
	if (birth_first_time)
	{
		FILE *fff;

		char buf[1024];

		/* Choose the player's keyboard layout */
		if (!get_player_keyboard()) return (FALSE);

		/* Clean up */
		clear_question();

		/* Choose the players difficulty */
		if (!get_player_difficulty()) return (FALSE);

		/* Clean up */
		clear_question();
		
		/* Answered these questions for good */
		birth_first_time = FALSE;
		
		/* Build the filename */
		path_build(buf, 1024, ANGBAND_DIR_USER, "Startup.prf");

		/* File type is "TEXT" */
		FILE_TYPE(FILE_TYPE_TEXT);

		/* Write to the file */
		fff = my_fopen(buf, "w");

		/* Failure */
		if (!fff) return (FALSE);

		/* Skip some lines */
		fprintf(fff, "\n\n");

		/* Start dumping */
		fprintf(fff, "# Automatic startup option dump\n\n");

		/* Dump startup options */
		fprintf(fff, "%c:%s\n", birth_first_time ? 'Y' : 'X', option_text[OPT_birth_first_time]);

		/* Dump startup options */
		fprintf(fff, "%c:%s\n", rogue_like_commands ? 'Y' : 'X', option_text[OPT_rogue_like_commands]);

		/* Dump startup options */
		fprintf(fff, "%c:%s\n", birth_beginner ? 'Y' : 'X', option_text[OPT_birth_beginner]);

		/* Dump startup options */
		fprintf(fff, "%c:%s\n", birth_intermediate ? 'Y' : 'X', option_text[OPT_birth_intermediate]);
		
		/* Close */
		my_fclose(fff);
	}
	
	/* Allow quickstart? */
	else if (character_quickstart)
	{
		/* Choose whether to use the last game's start-up */
		if (!get_player_quickstart()) return (FALSE);

		/* Clean up */
		clear_question();
		
		/* Don't show choice any longer */
		character_quickstart = FALSE;

		/* If player is quickstarting, we are done */
		if (birth_quickstart)
		{
			/* Quick start the character */
			player_birth_quickstart(&normal_quickstart);
		
			return (TRUE);
		}
	}
	
	/* Not quickstarting */
	birth_quickstart = FALSE;
	
	Term_putstr(QUESTION_COL, HEADER_ROW, -1, TERM_L_BLUE,
		    "Please select your character from the menu below.");

	/* Choose the player's gender */
	if (!get_player_sex()) return (FALSE);

	/* Clean up */
	clear_question();

	/* If player is beginner, we are done */
	if (birth_beginner)
	{
		/* Quick start the character */
		player_birth_quickstart(&beginner_quickstart);
		
		return (TRUE);
	}
	
	/* Choose the players race */
	if (!get_player_race()) return (FALSE);

	/* Clean up */
	clear_question();

	/* Choose the players class */
	if (!get_player_class()) return (FALSE);

	/* Clean up */
	clear_question();

	/* Choose the style */
	if (!get_player_style()) return (FALSE);

	/* Choose the book */	
	if ((p_ptr->pstyle == WS_MAGIC_BOOK) || (p_ptr->pstyle == WS_PRAYER_BOOK) || (p_ptr->pstyle == WS_SONG_BOOK))
	{
		/* Clean up */
		clear_question();

		/* Choose the style */
		if (!get_player_book()) return (FALSE);
	}

	/* Choose the school - hack: just magic for the moment */
	if ((p_ptr->pstyle == WS_MAGIC_BOOK) || (p_ptr->pstyle == WS_PRAYER_BOOK) /*|| (p_ptr->pstyle == WS_SONG_BOOK) */
		|| (c_info[p_ptr->pclass].spell_book == TV_MAGIC_BOOK) || (c_info[p_ptr->pclass].spell_book == TV_PRAYER_BOOK)
		/*|| (c_info[p_ptr->pclass].spell_book == TV_SONG_BOOK)*/)
	{
		/* Clean up */
		clear_question();

		/* Choose the style */
		if (!get_player_school()) return (FALSE);
	}

	/* Clean up */
	clear_question();
	
	/* Choose the roller */
	if (!get_player_roller()) return (FALSE);

	/* Clear */
	Term_clear();

	/* Done */
	return (TRUE);
}


/*
 * Initial stat costs (initial stats always range from 10 to 18 inclusive).
 */
static const int birth_stat_costs[(18-10)+1] = { 0, 1, 3, 5, 7, 10, 13, 16, 23};


/*
 * Helper function for 'player_birth()'.
 *
 * This function handles "point-based" character creation.
 *
 * The player selects, for each stat, a value from 10 to 18 (inclusive),
 * each costing a certain amount of points (as above), from a pool of 48
 * available points, to which race/class modifiers are then applied.
 *
 * Each unused point is converted into 100 gold pieces.
 */
static bool player_birth_aux_2(void)
{
	int i;

	int row = 2;
	int col = 42;

	int stat = 0;

	int stats[A_MAX];

	int cost;

	char ch;

	char buf[80];


	/* Initialize stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Initial stats */
		stats[i] = 10;
	}

	/* Roll for age/height/weight */
	get_ahw();

	/* Roll for social class */
	get_history();

	/* Interact */
	while (1)
	{
		/* Reset cost */
		cost = 0;

		/* Process stats */
		for (i = 0; i < A_MAX; i++)
		{
			/* Obtain a "bonus" for "race" and "class" */
			int bonus_add = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

			/* Get the minimally increased stat for the bonuses */
			int value_min = adjust_stat(stats[i], bonus_add, 1);

			/* Get the maximally increased stat for the bonuses */
			int value_max = adjust_stat(stats[i], bonus_add, 99);

			/* Get the stat increased by the average for the bonuses */
			int value = value_min + (value_max - value_min) / 2;

			/* Apply the racial/class bonuses */
			p_ptr->stat_cur[i] = p_ptr->stat_max[i] = value;

			/* Total cost */
			cost += birth_stat_costs[stats[i] - 10];
		}

		/* Restrict cost */
		if (cost > 48)
		{
			/* Warning */
			bell("Excessive stats!");

			/* Reduce stat */
			stats[stat]--;

			/* Recompute costs */
			continue;
		}

		/* Gold is inversely proportional to cost */
		p_ptr->au = (100 * (48 - cost)) + 100;

		/* Calculate the bonuses and hitpoints */
		p_ptr->update |= (PU_BONUS | PU_HP);

		/* Update stuff */
		update_stuff();

		/* Fully healed */
		p_ptr->chp = p_ptr->mhp;

		/* Fully rested */
		p_ptr->csp = p_ptr->msp;

		/* Display the player */
		display_player(0);

		/* Display the costs header */
		put_str("Cost ", row - 1, col + 33);

		/* Display the costs */
		for (i = 0; i < A_MAX; i++)
		{
			/* Display cost */
			sprintf(buf, "%4d", birth_stat_costs[stats[i] - 10]);
			put_str(buf, row + i, col + 33);
		}


		/* Prompt XXX XXX XXX */
		sprintf(buf, "Total Cost %2d/48.  Use 2/8 to move, 4/6 to modify, Enter to accept.", cost);
		prt(buf, 0, 0);

		/* Place cursor just after cost of current stat */
		Term_gotoxy(col + 36, row + stat);

		/* Get key */
		ch = inkey();

		/* Quit */
		if (ch == 'Q') quit(NULL);

		/* Start over */
		if ((ch == 'S') || (ch == ESCAPE)) return (FALSE);

		/* Done */
		if ((ch == '\n') || (ch == '\r')) break;

		/* Prev stat */
		if (ch == '8')
		{
			stat = (stat + A_MAX - 1) % A_MAX;
		}

		/* Next stat */
		if (ch == '2')
		{
			stat = (stat + 1) % A_MAX;
		}

		/* Decrease stat */
		if ((ch == '4') && (stats[stat] > 10))
		{
			stats[stat]--;
		}

		/* Increase stat */
		if ((ch == '6') && (stats[stat] < 18))
		{
			stats[stat]++;
		}
	}


	/* Done */
	return (TRUE);
}


/*
 * Helper function for 'player_birth()'.
 *
 * This function handles "auto-rolling" and "random-rolling".
 *
 * The delay may be reduced, but is recommended to keep players
 * from continuously rolling up characters, which can be VERY
 * expensive CPU wise.  And it cuts down on player stupidity.
 */
static bool player_birth_aux_3(void)
{
	int i, j, m, v;

	bool flag;
	bool prev = FALSE;

	key_event ke;

	char b1 = '[';
	char b2 = ']';

	char buf[80];


#ifdef ALLOW_AUTOROLLER

	s16b stat_limit[A_MAX];

	s32b stat_match[A_MAX];

	s32b auto_round = 0L;

	s32b last_round;


	/*** Autoroll ***/

	/* Initialize */
	if (adult_auto_roller)
	{
		int mval[A_MAX];

		char inp[80];


		/* Extra info */
		Term_putstr(5, 10, -1, TERM_WHITE,
			    "The auto-roller will automatically ignore characters which do");
		Term_putstr(5, 11, -1, TERM_WHITE,
			    "not meet the minimum values for any stats specified below.");
		Term_putstr(5, 12, -1, TERM_WHITE,
			    "Note that stats are not independant, so it is not possible to");
		Term_putstr(5, 13, -1, TERM_WHITE,
			    "get perfect (or even high) values for all your stats.");

		/* Prompt for the minimum stats */
		put_str("Enter minimum value for: ", 15, 2);

		/* Output the maximum stats */
		for (i = 0; i < A_MAX; i++)
		{
			/* Reset the "success" counter */
			stat_match[i] = 0;

			/* Race/Class bonus */
			j = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

			/* Obtain the "maximal" stat */
			m = adjust_stat(17, j, 99);

			/* Save the maximum */
			mval[i] = m;

			/* Extract a textual format */
			/* cnv_stat(m, inp); */

			/* Above 18 */
			if (m > 18)
			{
				sprintf(inp, "(Max of 18/%02d):", (m - 18));
			}

			/* From 3 to 18 */
			else
			{
				sprintf(inp, "(Max of %2d):", m);
			}

			/* Prepare a prompt */
			sprintf(buf, "%-5s%-20s", stat_names[i], inp);

			/* Dump the prompt */
			put_str(buf, 16 + i, 5);
		}

		/* Input the minimum stats */
		for (i = 0; i < A_MAX; i++)
		{
			/* Get a minimum stat */
			while (TRUE)
			{
				char *s;

				/* Move the cursor */
				put_str("", 16 + i, 30);

				/* Default */
				strcpy(inp, "");

				/* Get a response (or escape) */
				if (!askfor_aux(inp, 9)) inp[0] = '\0';

				/* Hack -- add a fake slash */
				my_strcat(inp, "/", sizeof(inp));

				/* Hack -- look for the "slash" */
				s = strchr(inp, '/');

				/* Hack -- Nuke the slash */
				*s++ = '\0';

				/* Hack -- Extract an input */
				v = atoi(inp) + atoi(s);

				/* Break on valid input */
				if (v <= mval[i]) break;
			}

			/* Save the minimum stat */
			stat_limit[i] = (v > 0) ? v : 0;
		}
	}

#endif /* ALLOW_AUTOROLLER */

	/* Clean up */
	clear_from(10);


	/*** Generate ***/

	/* Roll */
	while (TRUE)
	{
		int col = 42;

		/* Feedback */
		if (adult_auto_roller)
		{
			Term_clear();

			/* Label */
			put_str(" Limit", 2, col+5);

			/* Label */
			put_str("  Freq", 2, col+13);

			/* Label */
			put_str("  Roll", 2, col+24);

			/* Put the minimal stats */
			for (i = 0; i < A_MAX; i++)
			{
				/* Label stats */
				put_str(stat_names[i], 3+i, col);

				/* Put the stat */
				cnv_stat(stat_limit[i], buf);
				c_put_str(TERM_L_BLUE, buf, 3+i, col+5);
			}

			/* Note when we started */
			last_round = auto_round;

			/* Label count */
			put_str("Round:", 10, col+13);

			/* Indicate the state */
			put_str("(Hit Enter to stop)", 12, col+13);

			/* Auto-roll */
			while (1)
			{
				bool accept = TRUE;

				/* Get a new character */
				get_stats();

				/* Advance the round */
				auto_round++;

				/* Hack -- Prevent overflow */
				if (auto_round >= 1000000L) break;

				/* Check and count acceptable stats */
				for (i = 0; i < A_MAX; i++)
				{
					/* This stat is okay */
					if (stat_use[i] >= stat_limit[i])
					{
						stat_match[i]++;
					}

					/* This stat is not okay */
					else
					{
						accept = FALSE;
					}
				}

				/* Break if "happy" */
				if (accept) break;

				/* Take note every 25 rolls */
				flag = (!(auto_round % 25L));

				/* Update display occasionally */
				if (flag || (auto_round < last_round + 100))
				{
					/* Put the stats (and percents) */
					for (i = 0; i < A_MAX; i++)
					{
						/* Put the stat */
						cnv_stat(stat_use[i], buf);
						c_put_str(TERM_L_GREEN, buf, 3+i, col+24);

						/* Put the percent */
						if (stat_match[i])
						{
							int p = 1000L * stat_match[i] / auto_round;
							byte attr = (p < 100) ? TERM_YELLOW : TERM_L_GREEN;
							sprintf(buf, "%3d.%d%%", p/10, p%10);
							c_put_str(attr, buf, 3+i, col+13);
						}

						/* Never happened */
						else
						{
							c_put_str(TERM_RED, "(NONE)", 3+i, col+13);
						}
					}

					/* Dump round */
					put_str(format("%10ld", auto_round), 10, col+20);

					/* Make sure they see everything */
					Term_fresh();

					/* Delay 1/10 second */
					if (flag) Term_xtra(TERM_XTRA_DELAY, 100);

					/* Do not wait for a key */
					inkey_scan = TRUE;

					/* Check for a keypress */
					if (anykey().key) break;
				}
			}
		}

		/* Otherwise just get a character */
		else
		{
			/* Get a new character */
			get_stats();
		}

		/* Flush input */
		flush();


		/*** Display ***/

		/* Roll for age/height/weight */
		get_ahw();

		/* Roll for social class */
		get_history();

		/* Roll for gold */
		get_money();

		/* Input loop */
		while (TRUE)
		{
			/* Calculate the bonuses and hitpoints */
			p_ptr->update |= (PU_BONUS | PU_HP);

			/* Update stuff */
			update_stuff();

			/* Fully healed */
			p_ptr->chp = p_ptr->mhp;

			/* Fully rested */
			p_ptr->csp = p_ptr->msp;

			/* Display the player */
			display_player(0);

			/* Prepare a prompt (must squeeze everything in) */
			Term_gotoxy(2, 23);
			Term_addch(TERM_WHITE, b1);
			Term_addstr(-1, TERM_WHITE, "'r' to reroll");
			if (prev) Term_addstr(-1, TERM_WHITE, ", 'p' for prev");
			Term_addstr(-1, TERM_WHITE, ", or Enter to accept");
			Term_addch(TERM_WHITE, b2);

			/* Prompt and get a command */
			ke = anykey();

			/* Quit */
			if (ke.key == 'Q') quit(NULL);

			/* Start over */
			if ((ke.key == 'S')||(ke.key == ESCAPE)) return (FALSE);

			/* Enter accepts the roll */
			if ((ke.key == '\n') || (ke.key == '\r')) break;

			/* Reroll this character */
			if ((ke.key == ' ') || (ke.key == 'r')) break;

			/* Hack -- left click to reroll, other click to accept */
			if (ke.key == '\xff')
			{
				if (ke.mousebutton != 1) ke.key = '\n';
				break;
			}

			/* Previous character */
			if (prev && (ke.key == 'p'))
			{
				load_prev_data();
				continue;
			}

			/* Help */
			if (ke.key == '?')
			{
				do_cmd_help();
				continue;
			}

			/* Warning */
			bell("Illegal auto-roller command!");
		}

		/* Are we done? */
		if ((ke.key == '\n') || (ke.key == '\r')) break;

		/* Save this for the "previous" character */
		save_prev_data();

		/* Note that a previous roll exists */
		prev = TRUE;
	}

	/* Clear prompt */
	clear_from(23);

	/* Done */
	return (TRUE);
}


/*
 * Helper function for 'player_birth()'.
 *
 * See "display_player" for screen layout code.
 */
static bool player_birth_aux(void)
{
	int i;

	key_event ke;

	/* Ask questions */
	if (!player_birth_aux_1()) return (FALSE);

	/* Set adult options from birth options */
	for (i = OPT_BIRTH; i < OPT_CHEAT; i++)
	{
		op_ptr->opt[OPT_ADULT + (i - OPT_BIRTH)] = op_ptr->opt[i];
	}

	/* Reset score options from cheat options */
	for (i = OPT_CHEAT; i < OPT_ADULT; i++)
	{
		op_ptr->opt[OPT_SCORE + (i - OPT_CHEAT)] = op_ptr->opt[i];
	}

	/* Quickstarting */
	if (adult_beginner || adult_quickstart)
	{
		/* Already rolled stats */
	}
	
	/* Point-based */
	else if (adult_point_based)
	{
		/* Point based */
		if (!player_birth_aux_2()) return (FALSE);
	}

	/* Random */
	else
	{
		/* Auto-roll */
		if (!player_birth_aux_3()) return (FALSE);
	}

	/* Get a name, prepare savefile */
	get_name();

	/* Display the player */
	display_player(0);

	/* Prompt for it */
	prt("['Q' to suicide, 'S' to start over, or Enter to continue]", 23, 10);

	/* Get a key */
	ke = anykey();

	/* Quit */
	if (ke.key == 'Q') quit(NULL);

	/* Start over */
	if ((ke.key == 'S')||(ke.key == ESCAPE)) return (FALSE);

	/* Accept */
	return (TRUE);
}


void roll_hp_table(void)
{
  int i, j, min_value, max_value; 
  int random_levels = PY_MAX_LEVEL - 2;

  /* Minimum hitpoints at highest level - 1 */
  min_value = random_levels * 9 * 3 / 8;
  min_value += random_levels;

  /* Maximum hitpoints at highest level - 1 */
  max_value = random_levels * 9 * 5 / 8;
  max_value += random_levels;

  /* Set level 1 hitdice */
  p_ptr->player_hp[0] = 10;

  /* Roll out the hitpoints */
  while (TRUE)
    {
      /* Roll the hitpoint values */
      for (i = 1; i <= random_levels ; i++)
	{
	  j = randint(10);
	  p_ptr->player_hp[i] = p_ptr->player_hp[i-1] + j;
	}

      /* Require "valid" hitpoints at various levels */
      if (p_ptr->player_hp[random_levels/5] - 10 <= min_value/5) 
	continue;
      if (p_ptr->player_hp[random_levels/5] - 10 >= max_value/5) 
	continue;

      if (p_ptr->player_hp[random_levels/2] - 10 <= min_value/2) 
	continue;
      if (p_ptr->player_hp[random_levels/2] - 10 >= max_value/2) 
	continue;

      if (p_ptr->player_hp[random_levels] - 10 <= min_value) 
	continue;
      if (p_ptr->player_hp[random_levels] - 10 >= max_value) 
	continue;

      /* Acceptable */
      break;
    }

  /* Set level 50 hitdice */
  p_ptr->player_hp[PY_MAX_LEVEL - 1] = 
    p_ptr->player_hp[random_levels] + 10;
}


/*
 * Create a new character.
 *
 * Note that we may be called with "junk" leftover in the various
 * fields, so we must be sure to clear them first.
 */
void player_birth(void)
{
	int n;
	
	/* Wipe the player */
	player_wipe();

	/* Roll the hitpoints table */
	roll_hp_table();

	/* Create a new character */
	while (1)
	{
		/* Roll up a new character */
		if (player_birth_aux()) break;
	}

	/* Save starting money for later analysis */
	p_ptr->birth_au = p_ptr->au;

	/* Record stats for later analysis*/
	for (n = 0; n < A_MAX; n++)
	{
		p_ptr->stat_birth[n] = p_ptr->stat_max[n];
	}

	/* Note player birth in the message recall */
	message_add(" ", MSG_GENERIC);
	message_add("  ", MSG_GENERIC);
	message_add("====================", MSG_GENERIC);
	message_add("  ", MSG_GENERIC);
	message_add(" ", MSG_GENERIC);
	
	/* Hack - don't display above for easy_more */
	if (easy_more)
	{
		/* Arcane weirdness */
		msg_print(" ");
		message_flush();
	}
	
	/* Initialise birth tips */
	if (adult_beginner)
	{
		n = 1;

		/* Queue tips birth1, birth2, etc. */
		while (queue_tip(format("birth%d.txt", n))) n++;
	}

	/* Use quickstart as a proxy for played this class/race before */
	if (!adult_quickstart)
	{
		/* Race tips */
		queue_tip(format("race%d.txt", p_ptr->prace));

		/* Class tips */
		queue_tip(format("class%d.txt", p_ptr->pclass));

		/* Style tips */
		queue_tip(format("style%d.txt", p_ptr->pstyle));
		
		/* Specialists get tval tips */
		if (style2tval[p_ptr->pstyle])
		{
			queue_tip(format("tval%d.txt", style2tval[p_ptr->pstyle]));		
		}
	}
	
	/* Hack -- assume the new shape */
	change_shape(p_ptr->prace, p_ptr->lev);

	/* Hack -- outfit the player */
	player_outfit();
	
	/* Hack -- set the dungeon. */
	if (adult_campaign) p_ptr->dungeon = 1;
	else p_ptr->dungeon = z_info->t_max - 2;

	/* Hack -- set the town. This is now required for shop restocking. */
	if (adult_campaign) p_ptr->town = 1;
	else p_ptr->town = z_info->t_max - 2;

	/* Set last disturb */
	p_ptr->last_disturb = turn;

	/* Initialize */
	store_init(STORE_HOME);

	/* Maintain the shop (ten times) */
	for (n = 0; n < 10; n++) store_maint(STORE_HOME);

	/* Hack -- name home */
	store[STORE_HOME]->index = 8;

	/* Quests */
	for (n = 0; n < z_info->q_max; n++)
	{
		/* Copy the structure */
		COPY(&q_list[n], &q_info[n], quest_type);
	}
}

