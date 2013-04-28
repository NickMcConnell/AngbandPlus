/* File: obj_make.c */

/*
 * Creation and manipulation of objects by the character.
 *
 * Choose an essence, add magic using essences, forge objects, poison ammo.
 *
 * Copyright (c) 2007
 * Leon Marrick, Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */


#include "angband.h"



static int grenade_pval = -1;
static int grenade_essence_num = 0;
static int ave_potential = 0;

/*
 * Open up the pouch, display it, and (optionally) select an essence.
 *
 * I could integrate this code with "get_item()", or with "show_inven()",
 * but keeping it separate will make it easier to improve.
 *
 * Return -2 if no essences are available, -1 if the user cancelled, or
 * the sval of the chosen essence(s) if successful.
 */
int get_essence(bool just_looking)
{
	int essence_sval = -1;   /* Assume cancel */

	object_type *i_ptr;
	object_type object_type_body;
	object_kind *k_ptr;

	char ch;
	int i, j, k, l, z;
	int col, len, lim, l_margin, num;

	char o_name[DESC_LEN];

	char tmp_val[DESC_LEN];

	int out_index[NUM_ESSENCE];
	byte out_color[NUM_ESSENCE];
	char out_desc[NUM_ESSENCE][DESC_LEN];


	/* Default length */
	len = 79 - 50;

	/* Maximum space allowed for descriptions (columns - 4) */
	lim = Term->cols - 4;

	/* Find the "final" slot */
	for (z = 0, i = 0; i < NUM_ESSENCE; i++)
	{
		/* Skip empty slots */
		if (!p_ptr->essence[i]) continue;

		/* Track */
		z = i + 1;
	}


	/* No essences available */
	if (z == 0) return (-2);


	/* Get essence names and indexes */
	for (k = 0, i = 0; i < z; i++)
	{
		/* Skip empty slots */
		if (!p_ptr->essence[i]) continue;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Hack -- Make a (fake) essence */
		object_prep(i_ptr, lookup_kind(TV_ESSENCE, i));

		/* Hack, Hack -- get quantity */
		i_ptr->number = p_ptr->essence[i];

		/* Get object kind */
		k_ptr = &k_info[i_ptr->k_idx];

		/* Describe the essence */
		object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 1);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		/* Save the index */
		out_index[k] = i;

		/* Get essence color */
		out_color[k] = k_ptr->d_attr;

		/* Save the object description */
		strcpy(out_desc[k], o_name);

		/* Find the predicted "line length" */
		l = strlen(out_desc[k]) + 5;

		/* Maintain the maximum length */
		if (l > len) len = l;

		/* Advance to next "line" */
		k++;
	}


	/* Save screen */
	screen_save(FALSE);


	/* Find the column to start in (centered or left-justified) */
	col = MAX(0, (lim - len) / 2);

	/* Remember the left margin (one-space margin unless at left edge already) */
	l_margin = MAX(0, col - 1);

	/* Output each entry */
	for (j = 0; j < k; j++)
	{
		/* Clear the text area */
		put_str(format("%*s", len+1, ""), j+1, l_margin);

		/* Prepare an index --(-- */
		(void)strnfmt(tmp_val, sizeof(tmp_val), "%c)", I2A(j));

		/* Clear the line with the (possibly indented) index */
		put_str(tmp_val, j+1, col);

		/* Display the entry itself */
		c_put_str(out_color[j], out_desc[j], j + 1, col + 3);
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < Term->rows - 1)) put_str(format("%*s", len+1, ""), j + 1, l_margin);

	/* We're just looking -- wait for any key */
	if (just_looking)
	{
		/* The exit sign */
		get_com("Press any key to close the pouch...", &ch);

		/* Load screen */
		screen_load();

		/* All done */
		return (-1);
	}

	/* We want to select an essence -- Repeat until satisfied */
	else
	{
		while (TRUE)
		{
			/* Get a command from the user */
			get_com("Choose which essence (ESC cancels):", &ch);

			/* ESC cancels */
			if (ch == ESCAPE) break;

			/* Choose an essence */
			if (isalpha(ch))
			{
				/* Lowercase */
				ch = my_tolower(ch);

				/* Convert choice to a number */
				num = A2I(ch);

				/* Convert to essence type */
				i = out_index[num];

				/* Asking for an essence we have */
				if (p_ptr->essence[i])
				{
					essence_sval = i;
					break;
				}
			}
		}
	}

	/* Load screen */
	screen_load();

	/* Return */
	return (essence_sval);
}


/*
 * Choose an essence and get a projection type
 */
int essence_to_magic(int *adjust, int *sval)
{
	/* Get an essence (if necessary) */
	if ((*sval < 0) || (*sval > NUM_ESSENCE)) *sval = get_essence(FALSE);

	/* No essences */
	if (*sval == -2) return (-2);

	/* Note failure XXX */
	if ((*sval < 0) || (*sval > NUM_ESSENCE)) return (-1);

	/* Convert essence to projection type, calculate effectiveness */
	switch (*sval)
	{
		case ESSENCE_ACID:   *adjust = 115;  return (GF_ACID);
		case ESSENCE_ELEC:   *adjust = 115;  return (GF_ELEC);
		case ESSENCE_FIRE:   *adjust = 125;  return (GF_FIRE);
		case ESSENCE_COLD:   *adjust = 125;  return (GF_COLD);
		case ESSENCE_POIS:   *adjust = 125;  return (GF_POIS);
		case ESSENCE_LITE:   *adjust = 100;  return (GF_LITE);
		case ESSENCE_DARK:   *adjust = 100;  return (GF_DARK);
		case ESSENCE_CONFU:  *adjust = 100;  return (GF_CONFUSION);
		case ESSENCE_FORCE:  *adjust =  80;  return (GF_FORCE);
		case ESSENCE_NEXUS:  *adjust =  90;  return (GF_NEXUS);
		case ESSENCE_NETHR:  *adjust = 100;  return (GF_NETHER);
		case ESSENCE_CHAOS:  *adjust = 100;  return (GF_CHAOS);
		case ESSENCE_TIME:   *adjust =  80;  return (GF_TIME);
		case ESSENCE_MAGIC:  *adjust =  80;  return (GF_MANA);
		case ESSENCE_LIFE:   *adjust =  80;  return (GF_DO_HEAL);
		case ESSENCE_DEATH:  *adjust =  80;  return (GF_DEATH);
		case ESSENCE_KNOW:   *adjust =  60;  return (GF_ENLIGHTENMENT);
		case ESSENCE_SHIELD: *adjust =  60;  return (GF_PROTECTION);
		case ESSENCE_BLESS:  *adjust =  60;  return (GF_HOLY_ORB);
		default:             *adjust =  80;  return (GF_HURT);
	}

	/* Oops */
	return (-1);
}



/*
 * Check if you have enough essences to make or recharge an object.
 */
bool enough_essences(const object_type *o_ptr)
{
	int i;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Check essence costs */
	for (i = 0; i < 4; i++)
	{
		/* Get sval of this essence */
		int sval = k_ptr->e_type[i];

		/* Not enough essences */
		if (p_ptr->essence[sval] < k_ptr->e_num[i])
		{
			return (FALSE);
		}
	}

	/* Probably OK */
	return (TRUE);
}


/*
 * Given an object, use up the essences it requires for creation or
 * recharging.
 */
bool use_up_essences(const object_type *o_ptr)
{
	int i;
	object_kind *k_ptr = &k_info[o_ptr->k_idx];


	/* Check to see if we actually have enough essences */
	if (!enough_essences(o_ptr)) return (FALSE);

	/* Use up essences */
	for (i = 0; i < 4; i++)
	{
		/* Get sval of this essence */
		int sval = k_ptr->e_type[i];

		/* Use up some essences */
		p_ptr->essence[sval] -= k_ptr->e_num[i];
	}

	/* Done */
	return (TRUE);
}


/*
 * Choose a type of item to make, get maximum quantity, note level of
 * item.
 *
 * Normally, we cannot choose any item that we have not seen.  Rings and
 * amulets are the exceptions.
 *
 * Return the maximum number of objects the player can make in this
 * session.
 *
 * Code taken from the function "wiz_create_itemtype()"
 */
static int alchemy_choose_item(int *tval, int *sval,
	bool need_essence, int max_level, cptr desc)
{
	int i, j;
	int num, last_num = 0, max_num;
	int quantity = 0;
	int col, row;
	int wgt;

	char ch;
	char o_name[DESC_LEN];
	char buf[DESC_LEN];

	object_kind *k_ptr;
	int k_idx;

	int choice[60];

	bool cost_too_high = FALSE;

	int tmp_rows;

	bool is_grenade = FALSE;


	/* Use the tall display for potions and scrolls  XXX */
	if (((*tval == TV_SCROLL) || (*tval == TV_POTION)) && (term_main->rows < 46))
	{
		/* Save the screen, use the tall display, and center */
		display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_TALL, 80, 0);
	}
	else
	{
		display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_NORM, 80, 0);
	}


	/* Get working height of screen */
	tmp_rows = Term->rows - 1;


	/* Interact until satisfied */
	while (TRUE)
	{
		int x = 0, y = 0;

		/* Search the whole item list. */
		for (num = 0, i = 1; i < z_info->k_max; i++)
		{
			k_ptr = &k_info[i];

			/* Skip items not of the requested type */
			if (*tval != TV_BOW)
			{
				if (k_ptr->tval != *tval) continue;
			}
			else
			{
				if (!is_missile_weapon(k_ptr)) continue;
			}

			/* Hack -- Skip instant artifacts */
			if (k_ptr->flags3 & (TR3_INSTA_ART)) continue;

			/* Test for magical grenades (which have special rules) */
			if ((k_ptr->tval == TV_POTION) &&
			    (k_ptr->sval == SV_POTION_GRENADE)) is_grenade = TRUE;
			else                                    is_grenade = FALSE;

			/* Usually require awareness or seen */
			if ((*tval != TV_RING) && (*tval != TV_AMULET))
			{
				if (is_grenade)
				{
					/* Grenades are always known */
				}
				else if (!(k_ptr->special & (SPECIAL_EVER_SEEN | SPECIAL_AWARE)))
				{
					continue;
				}
			}

			/* No out-of-depth objects */
			if (k_ptr->level > max_level) continue;

			/* Hack -- you need some infusion skill to make grenades */
			if (is_grenade)
			{
				if (get_skill(S_INFUSION, 0, 100) < LEV_REQ_GRENADE) continue;
			}

			/* Assume cost is not too high */
			cost_too_high = FALSE;

			/* Require objects with an essence cost */
			if (need_essence)
			{
				/* No essence cost, so must be illegal */
				if ((!k_ptr->e_num[0]) && (!is_grenade)) continue;

				/* Scan the essence costs */
				for (j = 0; j < 4; j++)
				{
					if (k_ptr->e_num[j] > p_ptr->essence[k_ptr->e_type[j]])
					{
						cost_too_high = TRUE;
						break;
					}
				}
			}

			/* Prepare it */
			row = 3 + (num % (tmp_rows - 5));
			col = 40 * (num / (tmp_rows - 5));
			ch = index_chars[num];

			/* Acquire the "name" of object "i" */
			strip_name(o_name, i);

			/* Truncate object name */
			o_name[27] = '\0';

			/* Calculate weight */
			wgt = use_metric ? make_metric(k_ptr->weight) : k_ptr->weight;

			/* Build the string */
			(void)strnfmt(buf, sizeof(buf), "%c] %-27s%3d %3d.%d",
			        ch, o_name, k_ptr->level, wgt / 10, wgt % 10);

			/* Print it out */
			if (!cost_too_high)
			{
				if (num == last_num) c_prt(TERM_YELLOW, buf, row, col);
				else                 c_prt(TERM_WHITE, buf, row, col);
			}
			else
			{
				if (num == last_num) c_prt(TERM_SLATE, buf, row, col);
				else                 c_prt(TERM_L_DARK, buf, row, col);
			}

			/* Remember the object index */
			choice[num++] = i;
		}


		/* Display some headers */
		prt("Lev  Wgt", 2, 31);

		/* We need to know the maximal possible remembered object_index */
		max_num = num;


		/* Print information about the currently selected object */
		if ((last_num >= 0) && (last_num < max_num))
		{
			int attr = TERM_L_BLUE;

			char buf1[DESC_LEN];
			char buf2[DESC_LEN];
			cptr str;

			/* Point to the object kind */
			k_ptr = &k_info[choice[last_num]];

			/* Assume cost is not too high */
			cost_too_high = FALSE;

			strcpy(buf2, "Max quantity: ");

			/* If we need essences, we need to display a cost */
			if (need_essence)
			{
				/* Titles of cost, essences available, and quantity strings */
				strcpy(buf1, "Cost [available]:  ");

				/* Get and display essence costs */
				for (i = 0; i < 4; i++)
				{
					int cost = k_ptr->e_num[i];
					object_kind *k2_ptr;

					/* We need some of this kind of essence */
					if (cost)
					{
						/* Get the sval of this essence */
						int essence_sval = k_ptr->e_type[i];

						/* Look up the index number of this essence */
						k_idx = lookup_kind(TV_ESSENCE, essence_sval);

						/* Point to the essence kind */
						k2_ptr = &k_info[k_idx];

						/* Get base name */
						str = (k_name + k2_ptr->name);

						/* Add spaces between essence types */
						if (i != 0) strcat(buf1, "   ");

						/* Build the essence cost string */
						strcat(buf1, format("%s-%d", str, cost));

						strcat(buf1, format(" [%d]", p_ptr->essence[essence_sval]));

						/* We don't have enough of this kind of essence */
						if (p_ptr->essence[essence_sval] < k_ptr->e_num[i])
						{
							cost_too_high = TRUE;
						}
					}
				}

				/* Cost too high */
				if (cost_too_high) attr = TERM_SLATE;

				/* Display cost */
				center_string(buf, sizeof(buf), buf1, display_width());
				c_prt(attr, buf, tmp_rows - 2, 0);
			}

			/* Point to the object kind */
			k_ptr = &k_info[choice[last_num]];


			/* Hack -- up to four grenades can be made at a time */
			if ((*tval == TV_POTION) && (k_ptr->sval == SV_POTION_GRENADE))
			{
				quantity = get_skill(S_INFUSION, 2, 4);
			}

			/* Potions and scrolls can be made in quantity */
			else if ((*tval == TV_POTION) || (*tval == TV_SCROLL))
			{
				/* Number depends on infusion skill and object value */
				quantity = get_skill(S_INFUSION, 0, 70) /
					MAX(3, rsqrt(k_ptr->cost));
				if (quantity > 15) quantity = 15;
				if (quantity <  1) quantity =  1;
			}

			/* Can only make one of all other objects */
			else
			{
				quantity = 1;
			}

			/* Display the number that can be made */
			strcat(buf2, format("%d", quantity));

			/* It is dangerous to make lots of non-grenades at once */
			if ((quantity > 1) &&
				 ((*tval != TV_POTION) || (k_ptr->sval != SV_POTION_GRENADE)))
			{
				strcat(buf2, "  (risk increases as you approach this limit)");
			}

			/* Display */
			center_string(buf, sizeof(buf), buf2, display_width());
			c_prt(TERM_WHITE, buf, tmp_rows - 1, 0);

			/* Move cursor */
			move_cursor(y, x);
		}


		/* Make a choice */
		if (!get_com(format("What kind of %s? (RET: accept, ESC: cancel, '?': help, '~': inspect) ", desc), &ch))
		{
			last_num = -1;
			break;
		}

		/* Clear the status line */
		clear_row(1);

		/* Get position of cursor */
		(void)Term_locate(&x, &y);

		/* Help me. */
		if (ch == '?')
		{
			p_ptr->get_help_index = HELP_FORGING;
			do_cmd_help();
			(void)Term_clear();
		}

		/* Pressed RETURN */
		else if ((ch == '\n') || (ch == '\r'))
		{
			/* Have already selected a legal object type */
			if ((last_num >= 0) && (last_num < max_num))
			{
				/* But we don't have enough essences */
				if (need_essence && cost_too_high)
				{
					/* Complain */
					center_string(buf, sizeof(buf),
						"You don't have enough essences to make this object.",
						display_width());
					c_prt(TERM_YELLOW, buf, 1, 0);

					/* Move cursor */
					move_cursor(y, x);
				}

				/* A-OK */
				else
				{
					break;
				}
			}

			/* Oops -- need to select something */
			else
			{
				/* Complain */
				center_string(buf, sizeof(buf),
					"Please choose an object first.", display_width());
				c_prt(TERM_YELLOW, buf, 1, 0);

				/* Move cursor */
				move_cursor(y, x);
			}
		}


		/* Describe the selected item */
		else if (ch == '~')
		{
			/* Have already selected a legal object type */
			if ((last_num >= 0) && (last_num < max_num))
			{
				/* Point to the object kind */
				k_ptr = &k_info[choice[last_num]];

				/* This object has been seen or is aware */
				if (k_ptr->special & (SPECIAL_EVER_SEEN | SPECIAL_AWARE))
				{
					/* Get local object */
					object_type object_type_body;
					object_type *o_ptr = &object_type_body;

					/* Hack -- Make the fake item */
					object_prep(o_ptr, choice[last_num]);

					/* Identify */
					object_aware(o_ptr);
					object_known(o_ptr);

					/* Describe the item (as though it were in a store XXX) */
					do_cmd_observe(o_ptr, TRUE);
				}

				/* Cannot learn about an unseen object */
				else
				{
					/* Complain */
					center_string(buf, sizeof(buf),
						"You have never seen this object.  You can make it, but know nothing about it.",
						display_width());
					c_prt(TERM_YELLOW, buf, 1, 0);

					/* Move cursor */
					move_cursor(y, x);
				}
			}

			/* Oops -- need to select something */
			else
			{
				/* Complain */
				center_string(buf, sizeof(buf), "Please choose an object first.",
					display_width());
				c_prt(TERM_YELLOW, buf, 1, 0);

				/* Move cursor */
				move_cursor(y, x);
			}
		}

		/* Allow movement (cannot use roguelike keys here) */
		else if (ch == '8')
		{
			if (last_num > 0) last_num--;
		}
		else if (ch == '2')
		{
			if (last_num < max_num - 1) last_num++;
		}

		/* All other commands */
		else
		{
			/* Check for object selection */
			num = get_index(ch, FALSE);

			/* We choose a legal object */
			if ((num >= 0) && (num < max_num))
			{
				last_num = num;
			}

			/* Complain if choice is "illegal" */
			else
			{
				/* Complain */
				center_string(buf, sizeof(buf),
					"Please type an index to choose an object, RETURN to accept, or ESC to cancel.",
					display_width());
				c_prt(TERM_YELLOW, buf, 1, 0);

				/* Move cursor */
				move_cursor(y, x);
			}
		}
	}

	/* Require a valid choice */
	if (last_num < 0) quantity = 0;

	/* Object is valid -- remember it */
	else
	{
		*sval = k_info[choice[last_num]].sval;
		*tval = k_info[choice[last_num]].tval;
	}

	/* Restore previous display */
	display_change(DSP_RESTORE | DSP_LOAD, 0, 0);

	/* And return quantity */
	return (quantity);
}


/*
 * What happens when the essences in a particular object go wild.
 */
void essence_wild_magic(object_type *o_ptr, int dam)
{
	int i, j;
	int essence_sval;
	int typ, dummy;

	/* Storage of essence type and number */
	int count[4];
	int type[4];

	/* Total essences */
	int total;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* We have not calculated a damage yet */
	if (dam < 0)
	{
		/* Damage depends on item level */
		dam = k_ptr->level + 1;

		/* Rings and amulets are particularly dangerous */
		if ((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET)) dam *= 2;

		/* Special case - potions of essences */
		if ((o_ptr->tval == TV_POTION) && (o_ptr->sval == SV_POTION_GRENADE))
		{
			dam = damroll(o_ptr->dd, o_ptr->ds);
		}
	}

	/* Store essence costs */
	for (total = 0, i = 0; i < 4; i++)
	{
		/* Store sval and number of essences */
		count[i] = k_ptr->e_num[i];
		type[i]  = k_ptr->e_type[i];

		/* Sum up total number of essences required */
		total += k_ptr->e_num[i];
	}

	/* Special case - potions of essences */
	if ((o_ptr->tval == TV_POTION) && (o_ptr->sval == SV_POTION_GRENADE))
	{
		type[0] = o_ptr->pval;
		total = 1;
	}

	/* Paranoia -- Require some essence cost */
	if (!total) return;

	/* Allow up to two explosions (usually) */
	for (i = 0; i < 2; i++)
	{
		/* Choose randomly, biased towards the essences most used */
		int choice = rand_int(total);

		/* Choose an essence to use */
		for (j = 3; choice < total; j--)
		{
			total -= count[j];
		}

		/* Get the essence sval */
		essence_sval = type[i];

		/* Given an essence sval, convert to a magic type */
		typ = essence_to_magic(&dummy, &essence_sval);

		/* Make some conversions to nastier spell effects */
		if ((typ == GF_MANA) && (one_in_(2))) typ = GF_DISENCHANT;

		/* Some magics have no effect */
		if ((typ == GF_PROTECTION) || (typ == -1)) return;

		/* Explode */
		project_star(0, 3 + dam / 50, p_ptr->py, p_ptr->px, p_ptr->py, p_ptr->px, dam, typ, 0L);

		/* Skill and good luck can prevent a second explosion */
		if (i == 0)
		{
			if ((rand_range(100, 200)) <=
			    (get_skill(S_INFUSION, 0, 100) + p_ptr->luck))
			{
				break;
			}
		}
	}
}



/*
 * Inflict the penalties for failing to make an object correctly.
 */
static bool kaboom(object_type *o_ptr)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int dam;

	/* Object usually does not survive */
	bool object_survives = FALSE;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Damage depends on item level */
	dam = k_ptr->level + 1;

	/* Rings and amulets are particularly dangerous */
	if ((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET)) dam *= 2;

	/* Potions often smash */
	if ((o_ptr->tval == TV_POTION) && (one_in_(2)))
	{
		msg_print("The potion explodes!");
		(void)potion_smash_effect(0, py, px, o_ptr);
	}

	/* Scroll-making magic sometimes attacks the mind */
	else if ((o_ptr->tval == TV_SCROLL) && (one_in_(2)))
	{
		msg_print("The binding spells go wild!");

		if (check_save(50 + k_ptr->level))
		{
			msg_print("You resist the effects!");
		}
		else
		{
			if (take_hit(damroll(2, k_ptr->level + 1), 0,
				"Your mind is blasted!", "binding magics gone wild")) return (TRUE);

			if ((!p_ptr->resist_confu) && (randint(k_ptr->level) >= 4))
			{
				(void)set_confused(p_ptr->confused + rand_range(4, 8));
			}
			if (randint(k_ptr->level) >= 8)
			{
				(void)set_slow(p_ptr->slow + rand_range(4, 8));
			}
			if ((!p_ptr->resist_blind) && (randint(k_ptr->level) >= 12))
			{
				(void)set_blind(p_ptr->blind + rand_range(8, 16), NULL);
			}
			if ((!p_ptr->free_act) && (randint(k_ptr->level) >= 24))
			{
				(void)set_paralyzed(p_ptr->paralyzed + rand_range(4, 8));
			}
		}
	}

	/* Otherwise, we deal damage based on the essences used */
	else
	{
		msg_print("The magic escapes from your control!");

		/* Magic goes wild */
		essence_wild_magic(o_ptr, -1);
	}


	/* Rings and amulets sometimes survive, but go bad */
	if (((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET)) &&
	     (one_in_(2)))
	{
		/* Message */
		msg_print("Your item got corrupted!");

		/* Object survives */
		object_survives = TRUE;

		/* Reverse positive pvals */
		if (o_ptr->pval  > 0) o_ptr->pval  = -(o_ptr->pval);
		if (o_ptr->pval2 > 0) o_ptr->pval2 = -(o_ptr->pval2);
		if (o_ptr->pval3 > 0) o_ptr->pval3 = -(o_ptr->pval3);

		/* Reverse positive plusses */
		if (o_ptr->to_d > 0) o_ptr->to_d  = -(o_ptr->to_d);
		if (o_ptr->to_h > 0) o_ptr->to_h  = -(o_ptr->to_h);
		if (o_ptr->to_a > 0) o_ptr->to_a  = -(o_ptr->to_a);

		/* Assorted nastiness */
		if      (one_in_(5)) o_ptr->flags3 |= (TR3_AGGRAVATE);
		else if (one_in_(4)) o_ptr->flags3 |= (TR3_TELEPORT);
		else if (one_in_(12)) o_ptr->flags3 |= (TR3_DRAIN_EXP);
		else if (one_in_(12)) o_ptr->flags3 |= (TR3_NOMAGIC);

		/* And a parting curse */
		if (k_ptr->level > rand_range(45, 85))
			o_ptr->flags3 |= (TR3_HEAVY_CURSE);
		else
			o_ptr->flags3 |= (TR3_LIGHT_CURSE);
	}

	/*
	 * In addition to normal effects, all items except scrolls can cause
	 * physical destruction when they go off bang.  Alchemists are
	 * notorious for rearranging the landscape.
	 */
	if ((o_ptr->tval != TV_SCROLL) && (dam > rand_range(10, 50)))
	{
		/* Earthquake */
		earthquake(py + rand_range(-1, 1), px + rand_range(-1, 1),
			1 + dam / 20);

		/* Cut */
		if (dam > randint(60)) set_cut(rand_spread(dam / 2, dam));

		/* Stun */
		if (dam > randint(80)) set_stun(rand_spread(dam / 4, dam / 2));
	}

	/* Note whether the object survives or not */
	return (object_survives);
}


/*
 * Arm the Grenades of War!
 *
 * Return the damage of the grenade.
 *
 * The grenade code is very ugly; we will want to clean it up sometime...
 */
static int make_grenade(object_type *o_ptr, bool use_essence)
{
	int dice, sides;
	int dam, adjust;

	/* Making grenades depends on infusion skill, not alchemy */
	int skill = get_skill(S_INFUSION, 0, 100);


	/* All grenades in a batch are of the same type */
	if (use_essence)
	{
		grenade_essence_num = 0;
		grenade_pval = -1;
	}

	/* We have not already chosen a grenade type */
	if (grenade_pval < 0)
	{
		int max;

		/* Choose a type of grenade */
		(void)essence_to_magic(&adjust, &grenade_pval);

		/* We cancelled */
		if (grenade_pval == -1)
		{
			msg_print("Cancelled.");
			return (-1);
		}

		/* May not use more than three essences (or whatever you've got) */
		max = MIN(p_ptr->essence[grenade_pval], 3);

		/* Decide how many essences to use */
		grenade_essence_num =
			(int)get_quantity(format("Use how many essences (max is %d)?",
			max), 0, max);
	}
	else
	{
		/* Choose a type of grenade */
		(void)essence_to_magic(&adjust, &grenade_pval);
	}

	/* Note cancel */
	if (!grenade_essence_num)
	{
		msg_print("Cancelled.");
		return (-1);
	}

	/* Hack -- Use up the essences */
	if (use_essence)
	{
		if (p_ptr->essence[grenade_pval] >= grenade_essence_num)
			p_ptr->essence[grenade_pval] -= grenade_essence_num;

		else return (-1);
	}

	/* More essences, more damage */
	if      (grenade_essence_num == 1) dam = 14L;
	else if (grenade_essence_num == 2) dam = 22L;
	else                               dam = 28L;

	/* Damage depends on essence type, number, and skill */
	dam = (dam * skill * adjust) / 1000L;

	/* Make a grenade */
	object_prep(o_ptr, lookup_kind(TV_POTION, SV_POTION_GRENADE));

	/* Identify */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Turn into dice - always have the same dice for a given damage */
	dam_to_dice((int)dam, &dice, &sides, FALSE);

	/* Adjust damage dice */
	o_ptr->dd = dice;
	o_ptr->ds = sides;

	/* Store essence type */
	o_ptr->pval = grenade_pval;

	/* Grenades have some value */
	o_ptr->b_cost = 1L + (dam * dam / 40L);

	/* Return the number of essence we used */
	return (grenade_essence_num);
}


/*
 * Make scrolls, potions, rings, or amulets.  Always requires essences,
 * sometimes requires other inputs (like parchments or bottles).
 */
static bool do_alchemy_aux(int input_tval, int output_tval,
	int skill, cptr input_name, cptr output_name)
{
	int i;
	int item = 0;

	int max, sval, slevel;
	int max_level = skill;

	bool has_essence = FALSE;
	bool grenade = FALSE;
	bool object_survives = TRUE;

	int kaboom_chance = 0;

	object_kind *k_ptr = &k_info[0];
	object_type *o_ptr;
	object_type *i_ptr;
	object_type forge;


	/* Require at least one essence */
	for (i = 0; i <= NUM_ESSENCE; i++)
	{
		if (p_ptr->essence[i] >= 1) has_essence = TRUE;
	}

	/* No essences */
	if (!has_essence)
	{
		msg_print("You have no essences.");
		return (FALSE);
	}


	/* Component level helps determine ring or amulet selection  XXX */
	if ((output_tval == TV_RING) || (output_tval == TV_AMULET))
	{
		char q[DESC_LEN];
		char s[DESC_LEN];

		strcpy(q, format("Choose which %s?", input_name));
		strcpy(s, format("You have no %ss.", input_name));

		/* Require a particular input */
		item_tester_tval = input_tval;

		/* Choose an item, break on cancel */
		if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR)))
			return (FALSE);
		item_to_object(i_ptr, item);


		/* Get the item kind */
		k_ptr = &k_info[i_ptr->k_idx];

		/* We do not need another input  XXX */
		input_tval = 0;

		/* Maximum object level also depends on component level */
		max_level = (skill + 5 * k_ptr->level / 4) / 2;
	}


	/* Choose a type of item to make, get max quantity. */
	max = alchemy_choose_item(&output_tval, &sval, TRUE, max_level,
		output_name);

	/* Cancelled */
	if (max < 1) return (FALSE);


	/* Hack -- use up components */
	if ((output_tval == TV_RING) || (output_tval == TV_AMULET))
	{
		/* Use up an input (pack) */
		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}

		/* Use up an input (floor) */
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}
	}



	/* Special case -- Grenades */
	if ((output_tval == TV_POTION) && (sval == SV_POTION_GRENADE))
	{
		/* Note that we're making a grenade */
		grenade = TRUE;
	}


	/* Messages */
	else if (max > 1)
	{
		msg_format("You can make up to %d %ss if you have enough %ss.",
			max, output_name, input_name);
	}
	else if ((output_tval == TV_SCROLL) || (output_tval == TV_POTION))
	{
		msg_format("You can make only one %s of this type at a time.",
			output_name);
	}

	/*
	 * Keep choosing non-essence inputs until all the objects are made, the
	 * player cancels, or something goes wrong.
	 */
	for (i = 0; i < max;)
	{
		/* Get local object */
		o_ptr = &forge;

		/* Prepare it */
		object_prep(o_ptr, lookup_kind(output_tval, sval));

		/* Note object level */
		slevel = k_info[o_ptr->k_idx].level;


		/* Some sort of input other than essences is required */
		if (input_tval)
		{
			char q[DESC_LEN];
			char s[DESC_LEN];

			strcpy(q, format("Choose which %s (ESC to cancel)?", input_name));
			strcpy(s, format("You have no %ss.", input_name));

			/* Require a particular input */
			item_tester_tval = input_tval;

			/* Choose an item, break on cancel */
			if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR)))
				return (i != 0);
			item_to_object(i_ptr, item);


			/* Get the item kind */
			k_ptr = &k_info[i_ptr->k_idx];


			/* Special case -- Grenades */
			if (grenade)
			{
				int pow;

				/* Make the grenade  (Hack -- use up essences immediately) */
				pow = make_grenade(o_ptr, i == 0);

				if (pow <= 0) return (i != 0);

				/* Chance depends on damage, bottle level, and skill */
				kaboom_chance = pow - (k_ptr->level * 3) - skill;

				/* Chance depends on damage, bottle level, and skill */
				kaboom_chance = (pow * 15) - (k_ptr->level / 3) - skill / 3;
			}

			/* Durability of item depends on base item used */
			if ((output_tval == TV_POTION) || (output_tval == TV_SCROLL))
			{
				o_ptr->ac = k_ptr->ac;
			}

			/* Use up an input (pack) */
			if (item >= 0)
			{
				inven_item_increase(item, -1);
				inven_item_describe(item);
				inven_item_optimize(item);
			}

			/* Use up an input (floor) */
			else
			{
				floor_item_increase(0 - item, -1);
				floor_item_describe(0 - item);
				floor_item_optimize(0 - item);
			}
		}

		/* Determine failure chance for anything other than grenades */
		if (!grenade)
		{
			/* Base failure chance depends on alchemy skill */
			if ((output_tval == TV_RING) || (output_tval == TV_AMULET))
			{
				kaboom_chance = 40 - (skill / 5);
			}
			else
			{
				kaboom_chance = 20 - (skill / 10);
			}

			/* Using an input */
			if (k_ptr->level)
			{
				/* High-level inputs and low-level objects are good */
				kaboom_chance = kaboom_chance * slevel / k_ptr->level;
			}

			/* There is a penalty for making more than one object at a time */
			if (i > 0)
			{
				kaboom_chance += (i * 100) / (get_skill(S_INFUSION, 2, 10) * max);
			}
		}

		/* Use up essences */
		if ((i == 0) && (!grenade)) use_up_essences(o_ptr);

		/* Attempt to make the item */
		i++;


		/* Check for failure */
		if (kaboom_chance > rand_int(100))
		{
			/* "Where's the earth-shattering kaboom?" */
			object_survives = kaboom(o_ptr);

			/* Party's over */
			break;
		}

		/* Object survives (always, unless a failure destroys it) */
		if (object_survives)
		{
			/* Add magic -- skill determines level */
			if (!grenade) apply_magic(o_ptr, skill, TRUE, FALSE, FALSE);

			/* Identify */
			object_aware(o_ptr);
			object_known(o_ptr);

			/* Give it to the character */
			give_object(o_ptr, FALSE);
		}
	}

	return (TRUE);
}



/*
 * Perform alchemy.
 */
bool do_alchemy(void)
{
	int skill = get_skill(S_ALCHEMY, 0, 100);

	int input_tval = 0;
	int output_tval = 0;

	char tmp;

	cptr input_name;
	cptr output_name;

	/* Interact until satisfied */
	while (TRUE)
	{
		/* Choose which type of object to make */
		if (skill < LEV_REQ_ALCHEMY_RING)
		{
			if (!get_com("Make (S)crolls or (P)otions?", &tmp))
			{
				msg_print("Cancelled.");
				return (FALSE);
			}
		}
		else
		{
			if (!get_com("Make (S)crolls, (P)otions, (R)ings, or (A)mulets?", &tmp))
			{
				msg_print("Cancelled.");
				return (FALSE);
			}
		}

		/* Choose an object type */
		if      ((tmp == 'S') || (tmp == 's')) output_tval = TV_SCROLL;
		else if ((tmp == 'P') || (tmp == 'p')) output_tval = TV_POTION;
		else if (skill >= LEV_REQ_ALCHEMY_RING)
		{
			if      ((tmp == 'R') || (tmp == 'r')) output_tval = TV_RING;
			else if ((tmp == 'A') || (tmp == 'a')) output_tval = TV_AMULET;
		}

		/* Verify tval, or complain */
		if (output_tval) break;
		else msg_print("Please choose a type of object to make, or press ESC.");
	}

	/* Some types of objects require specific inputs */
	if      (output_tval == TV_SCROLL) input_tval = TV_PARCHMENT;
	else if (output_tval == TV_POTION) input_tval = TV_BOTTLE;
	else                               input_tval = TV_COMPONENT;

	/* Build input descriptions */
	if      (input_tval == TV_PARCHMENT) input_name = "parchment";
	else if (input_tval == TV_BOTTLE)    input_name = "bottle";
	else if (input_tval == 0)            input_name = "(none)";
	else                                 input_name = "component";

	/* Build output descriptions */
	if      (output_tval == TV_SCROLL)    output_name = "scroll";
	else if (output_tval == TV_POTION)    output_name = "potion";
	else if (output_tval == TV_RING)      output_name = "ring";
	else if (output_tval == TV_AMULET)    output_name = "amulet";
	else                                  output_name = "undescribed";

	/* Make object(s) */
	return (do_alchemy_aux(input_tval, output_tval, skill, input_name,
	        output_name));
}





/*
 * Print the navigation bar for essence-based forging
 */
static void forging_essence_navbar(int row, int set)
{
	/* First set */
	c_prt((set == 0 ? TERM_YELLOW : TERM_SLATE), "Sustain/Slay", row, 0);

	/* Second set */
	c_prt((set == 1 ? TERM_YELLOW : TERM_SLATE), "Resist", row, 14);

	/* Third set */
	c_prt((set == 2 ? TERM_YELLOW : TERM_SLATE), "Qualities", row, 22);

	/* Fourth set */
	c_prt((set == 3 ? TERM_YELLOW : TERM_SLATE), "Bonuses", row, 33);

	/* Instructions */
	c_prt(TERM_WHITE, "'<' for previous, '>' for next screen", row, 42);
}


/*
 * Storage space for "do_forging_essence()" -- flag creation information.
 */
typedef struct flag_forging_info
{
	s16b p_index;   /* Index of pval associated with this flag, if any */
	s16b invested;  /* Number of essences invested */
	s16b loss;      /* Loss of potential */
	s16b max;       /* Maximum pval or chance for this flag, for this item */
} flag_forging_info;

static flag_forging_info ffi[128];



/*
 * Get the number of essences available of a given type.
 */
static int essences_available(int sval)
{
	int i;
	int total = 0;

	/* Scan the essence cost for the flags we desire */
	for (i = 0; i < 128; i++)
	{
		/* Total up the number we've used of this type of essence */
		if (flag_creation_data[i].essence_sval == sval)
			total += ffi[i].invested;
	}

	return (p_ptr->essence[sval] - total);
}

/*
 * Get the approximate proportion of used to total potential, and
 * return a warning color.
 */
static int color_potential(int potential)
{
	int i;
	long total = 0L;
	long abundance;

	/* Scan the potential loss for the flags we desire */
	for (i = 0; i < 128; i++)
	{
		/* Total up the potential loss */
		total += (long)ffi[i].loss;
	}

	/* No potential used yet */
	if (total == 0L) return (TERM_GREEN);

	/* Get relative potential abundance */
	abundance = 10L * potential / total;

	/* Return a warning color */
	if      (abundance >= 30L) return (TERM_GREEN);
	else if (abundance >= 22L) return (TERM_L_GREEN);
	else if (abundance >= 17L) return (TERM_WHITE);
	else if (abundance >= 13L) return (TERM_YELLOW);
	else if (abundance >= 10L) return (TERM_ORANGE);
	else if (abundance >=  5L) return (TERM_RED);
	else                       return (TERM_L_DARK);
}


/*
 * Calculate chance to infuse the object with this flag.
 *
 *  cost =     1     2     3     5
 *
 *  invested  success percentage
 *  1         30     0     0     0
 *  2         53    30     0     0
 *  3         69    48    30     0
 *  4         79    61    44     0
 *  5         86    70    55    30
 *  6         90    78    64    40
 *  7         90    83    71    49
 *  8         90    88    77    56
 *  9         90    90    82    62
 *  10        90    90    85    68
 *  11        90    90    88    72
 */
static int calc_infusion_chance(int cost, int invested)
{
	int i;

	/* You get a 30% chance if you invest the minimum */
	int chance = 30;

	/* High cost hurts chance, high investment helps it */
	for (i = cost; i < invested; i++)
	{
		/* Each additional essence helps less */
		chance += (100 - chance) / (MIN(cost, 8) + 2);
	}

	/* Return chance (max of 90%), if investment is at least cost */
	if (invested < cost) return (0);
	return (MIN(90, chance));
}



/*
 * Briefly name an essence.
 */
static cptr short_essence_name(int sval)
{
	cptr str;
	char name[DESC_LEN];

	/* Look up the index number of this essence */
	int k_idx = lookup_kind(TV_ESSENCE, sval);

	/* Point to the essence kind */
	object_kind *k_ptr = &k_info[k_idx];

	/* Skip empty essences */
	if (!k_name) return ("");

	/* Get base name */
	str = (k_name + k_ptr->name);

	/* Format name */
	strcpy(name, format("%s", str));

	/* Hack -- change a name */
	if (sval == ESSENCE_ELEC) strcpy(name, format("Elec ", str));

	/* Truncate the name */
	name[5] = '\0';

	/* Return the name */
	return (format("%s", name));
}



/*
 * Display available essences.
 */
static void prt_essences_avail(void)
{
	int i, j;

	char buf[320];
	cptr str;


	/* Build up information about available essences */
	for (buf[0] = '\0', i = 0, j = 0; i < NUM_ESSENCE; i++)
	{
		/* Character must have some of this essence */
		if (!p_ptr->essence[i]) continue;

		/* Get the short name of this essence */
		str = short_essence_name(i);

		/* Add spaces between essence types */
		if (j++ != 0) strcat(buf, "  ");

		/* Build the essence available string */
		strcat(buf, format("%-5s:%3d", str, essences_available(i)));
	}

	/* Position the cursor (allow four lines for display) */
	move_cursor(17, 0);

	/* Show the essences */
	c_roff(TERM_WHITE, buf, 1, 80);
}


/*
 * Choose pvals and pval-dependant qualities.
 *
 * Note that this display is only capable of showing 26 flags.
 */
static s16b do_forging_essence_pval(s16b *pval_val)
{
	int action = 0;

	char ch;

	char desc[DESC_LEN];

	/* Conversion from index of active flag to flag position */
	int index_to_flag[32];

	/* Current flag costs (depends on pval) */
	int flag_cost[32];

	int selection = -3;

	int col, row;
	int num = 0;

	int i, z, attr, pval, invest, sval, cost;

	bool pval_changed = FALSE;
	bool dummy;


	/* Clear screen */
	(void)Term_clear();


	/* Fill in flag index reference */
	for (i = 0; i < 32; i++)
	{
		/* We're allowed to purchase this flag */
		if (ffi[i].max)
		{
			/* Flag is available for purchase with essences */
			if (flag_creation_data[i].cost > 0)
			{
				/* Store it in the reference, increment number of flags */
				index_to_flag[num++] = i;
			}
		}

		/* Get current flag pval */
		pval = pval_val[ffi[i].p_index];

		/* Get current flag cost (depends on pval) */
		flag_cost[i] = MAX(1, (flag_creation_data[i].cost * pval + 5) / 10);
		if (!pval) flag_cost[i] = 0;
	}


	/* Interact */
	while (TRUE)
	{
		/* Print the navigation bar -- highlight set #3 */
		forging_essence_navbar(21, 3);

		/* Get and center the potential display */
		center_string(desc, sizeof(desc), "Potential", display_width());

		/* Get a warning color and display it (use average potential) */
		c_prt(color_potential(ave_potential), desc, 16, 0);

		/* Print essences */
		prt_essences_avail();

		/* Print pvals */
		c_put_str((selection == -3 ? TERM_YELLOW : TERM_WHITE),
			"Bonus #1: ", 0, 14);
		c_put_str(TERM_L_BLUE, format("%2d", pval_val[1]), 0, 23);

		c_put_str((selection == -2 ? TERM_YELLOW : TERM_WHITE),
			"Bonus #2: ", 0, 34);
		c_put_str(TERM_L_GREEN, format("%2d", pval_val[2]), 0, 43);

		c_put_str((selection == -1 ? TERM_YELLOW : TERM_WHITE),
			"Bonus #3: ", 0, 54);
		c_put_str(TERM_PURPLE, format("%2d", pval_val[3]), 0, 63);

		c_prt(TERM_L_WHITE, "Attribute              Bonus   Spent    Attribute              Bonus   Spent", 2, 0);

		/* Display the pval-dependant flags */
		for (col = 0, row = 3, z = 0; z < num; z++)
		{
			/* Get flag from index */
			i = index_to_flag[z];

			/* Halfway point -- Go to next column of flags */
			if (z == (num + 1) / 2)
			{
				col = 40;
				row = 3;
			}

			/* Get color of flag name */
			if (z == selection) attr = TERM_YELLOW;
			else                attr = TERM_WHITE;

			/* Print out this flag's name */
			c_prt(attr, format("%s", flag_creation_data[i].desc), row, col);

			/* Get flag cost */
			cost = flag_cost[i];

			/* We have enough essences to buy this attribute */
			if (MAX(1, cost) <=
				p_ptr->essence[flag_creation_data[i].essence_sval])
			{
				/* Color-coded pvals */
				attr = TERM_WHITE;
				if (ffi[i].p_index == 1) attr = TERM_L_BLUE;
				if (ffi[i].p_index == 2) attr = TERM_L_GREEN;
				if (ffi[i].p_index == 3) attr = TERM_PURPLE;

				/* Print out this flag's pval, if any */
				if (ffi[i].p_index != 0)
					c_prt(attr, format("%2d", pval_val[ffi[i].p_index]), row, col + 25);
				else
					c_prt(TERM_L_WHITE, "--", row, col + 25);

				/* Display invested essences */
				if (ffi[i].p_index)
				{
					/* Get chance of success */
					int chance = calc_infusion_chance(cost, ffi[i].invested);

					/* Colorful indicator of success chance */
					if      (chance >= 90) attr = TERM_PURPLE;
					else if (chance >= 80) attr = TERM_BLUE;
					else if (chance >= 70) attr = TERM_L_BLUE;
					else if (chance >= 50) attr = TERM_WHITE;
					else                   attr = TERM_SLATE;

					/* Display */
					c_prt(attr, format("%2d", ffi[i].invested), row, col + 31);
				}
				else
				{
					/* Print the essence needed to buy this flag */
					c_prt(TERM_SLATE, format("%s",
						short_essence_name(flag_creation_data[i].essence_sval)),
						row, col + 31);
				}
			}

			/* Note that attribute is too expensive to buy */
			else
			{
				/* Print the essence needed to buy this flag */
				c_prt(TERM_L_DARK, format("%s",
					short_essence_name(flag_creation_data[i].essence_sval)),
					row, col + 31);
			}

			/* Next row */
			row++;
		}

		/* Prompt */
		prt("Bonuses    <dir> to move, +/- to alter essences or bonuses, '!' to apply bonus", 23, 0);
		prt("           </> to change screens, '?' for help, Return to accept, ESC to cancel", 24, 0);


		/* Hide the cursor */
		inkey_cursor_hack[TERM_MAIN] = -1;

		/* Wait for legal input */
		while (TRUE)
		{
			/* Get key */
			ch = inkey(ALLOW_CLICK);

			/* Allow orthogonal direction keys */
			get_ui_direction(&ch, UI_NODIAG | UI_NOMOUSE, &dummy);

			/* Accept anything except untranslated mouse actions */
			if (ch != MOUSEKEY) break;
		}

		/* Allow the cursor to be shown again */
		inkey_cursor_hack[TERM_MAIN] = 0;

		/* Clear status line */
		clear_row(2);


		/* Cancel */
		if (ch == ESCAPE)
		{
			action = -99;
			break;
		}

		/* Accept changes */
		else if ((ch == '\r') || (ch == '\n'))
		{
			action = 0;
			break;
		}

		/* Switch to next screen */
		else if ((ch == '\t') || (ch == '>'))
		{
			action = 1;
			break;
		}

		/* Help me. */
		else if (ch == '?')
		{
			p_ptr->get_help_index = HELP_INFUSION;
			do_cmd_help();
			(void)Term_clear();
		}

		/* Switch to previous flag set */
		else if (ch == '<')
		{
			action = -1;
			break;
		}

		/* Go to previous flag or pval */
		else if (ch == '8')
		{
			if (selection > -3) selection--;
		}

		/* Go to next flag or pval */
		else if ((ch == '2') || (ch == ' '))
		{
			if (selection < num - 1) selection++;
		}

		/* Go to previous pval or flag column */
		else if (ch == '4')
		{
			if ((selection <= 0) && (selection > -3)) selection--;
			else if (selection >= (num+1) / 2) selection -= (num+1) / 2;
		}

		/* Go to next pval or flag column */
		else if (ch == '6')
		{
			if (selection < 0) selection++;
			else if (selection < num / 2) selection += (num+1) / 2;
		}

		/* Raise a number */
		else if ((ch == '+') || (ch == '='))
		{
			/* Raising a pval */
			if (selection < 0)
			{
				if (selection == -3) pval_val[1]++;
				if (selection == -2) pval_val[2]++;
				if (selection == -1) pval_val[3]++;

				/* Hack -- Maximum pval of 10  XXX */
				if (pval_val[1] > 10) pval_val[1] = 10;
				if (pval_val[2] > 10) pval_val[2] = 10;
				if (pval_val[3] > 10) pval_val[3] = 10;

				/* We've changed a pval -- costs also change */
				pval_changed = TRUE;
			}

			/* Raising the essences committed to a flag */
			else
			{
				/* Get flag from index */
				i = index_to_flag[selection];

				/* Jump from no to base number of essences */
				if (ffi[i].invested == 0) invest = flag_cost[i];

				/* Otherwise, increment investment by 1 */
				else invest = ffi[i].invested + 1;

				/* Get essence kind */
				sval = flag_creation_data[i].essence_sval;

				/* Ante up if we have enough essences of this kind */
				if (essences_available(sval) + ffi[i].invested  >= invest)
				{
					/* New flag being bought -- get potential cost */
					ffi[i].loss = get_cost_of_flag(i, pval_val[ffi[i].p_index]);

					/* Invest essences */
					ffi[i].invested = invest;
				}
			}
		}

		/* Lower a number */
		else if ((ch == '-') || (ch == '_'))
		{
			/* Lowering a pval */
			if (selection < 0)
			{
				if (selection == -3) pval_val[1]--;
				if (selection == -2) pval_val[2]--;
				if (selection == -1) pval_val[3]--;

				/* Minimum pval of 0 */
				if (pval_val[1] < 0) pval_val[1] = 0;
				if (pval_val[2] < 0) pval_val[2] = 0;
				if (pval_val[3] < 0) pval_val[3] = 0;

				/* We've (probably) changed a pval -- costs also change */
				pval_changed = TRUE;
			}

			/* Lowering the essences committed to a flag */
			else
			{
				/* Get flag from index */
				i = index_to_flag[selection];

				/* Drop from base number to no essences */
				if (ffi[i].invested <= flag_cost[i])
				{
					invest = 0;
				}

				/* Decrement investment by 1 */
				else
				{
					invest = ffi[i].invested - 1;
				}

				/* Lower the number of essences */
				ffi[i].invested = invest;

				/* Nothing invested -- remove potential cost */
				if (!ffi[i].invested) ffi[i].loss = 0;
			}
		}

		/* Change the index of the pval controlling a flag */
		else if (ch == '!')
		{
			/* Only works when a flag is selected */
			if (selection >= 0)
			{
				/* Get flag position */
				i = index_to_flag[selection];

				/* Scan */
				while (TRUE)
				{
					/* Go to next pval index, wrap around */
					ffi[i].p_index = ((ffi[i].p_index + 1) % 4);

					/* Stop if we find a pval that's not too high */
					if (pval_val[ffi[i].p_index] <= ffi[i].max) break;
				}

				/* Get this flag's pval */
				pval = pval_val[ffi[i].p_index];

				/* Recalculate this flag's essence cost */
				flag_cost[i] =
					MAX(1, (flag_creation_data[i].cost * pval + 5) / 10);
				if (!pval) flag_cost[i] = 0;
			}
		}

		/* We've changed a pval; costs have to be recalculated */
		if (pval_changed)
		{
			/* Scan all the listed flags */
			for (z = 0; z < num; z++)
			{
				/* Get flag from index */
				i = index_to_flag[z];

				/* This flag has a pval */
				if (ffi[i].p_index)
				{
					/* Get this flag's pval */
					pval = pval_val[ffi[i].p_index];

					/* Do not allow pvals to exceed maximum */
					if (pval > ffi[i].max)
					{
						/* Reset pval  XXX */
						ffi[i].p_index = 0;
						pval = 0;
					}

					/* Recalculate this flag's essence cost */
					flag_cost[i] =
						MAX(1, (flag_creation_data[i].cost * pval + 5) / 10);
					if (!pval) flag_cost[i] = 0;

					/* This flag has some essences invested in it */
					if (ffi[i].invested)
					{
						/* Recalculate potential cost */
						ffi[i].loss = get_cost_of_flag(i, pval);
					}
				}
			}

			pval_changed = FALSE;
		}
	}

	/* Return */
	return (action);
}



/*
 * Choose non pval-dependant qualities.
 */
static s16b do_forging_essence_flags(int set)
{
	int action = 0;

	char ch;

	char desc[DESC_LEN];

	/* Conversion from index of active flag to flag position */
	int index_to_flag[32];

	int selection = 0;

	int col, row;
	int num = 0;

	int i, z, attr, invest, sval, cost;

	bool dummy;


	/* Clear screen */
	(void)Term_clear();


	/* Fill in flag index reference */
	for (i = 32 + set * 32; (i < set * 32 + 64); i++)
	{
		/* We're allowed to purchase this flag */
		if (ffi[i].max)
		{
			/* Flag is available for purchase with essences */
			if (flag_creation_data[i].cost > 0)
			{
				/* Store it in the reference, increment number of flags */
				index_to_flag[num++] = i;
			}
		}
	}

	/* Interact */
	while (TRUE)
	{
		/* Print the navigation bar -- highlight current set */
		forging_essence_navbar(21, set);

		/* Get and center the potential display */
		center_string(desc, sizeof(desc), "Potential", display_width());

		/* Get a warning color and display it */
		c_prt(color_potential(ave_potential), desc, 16, 0);

		/* Display available essences */
		prt_essences_avail();

		c_prt(TERM_L_WHITE, "Attribute               Cost   Spent    Attribute               Cost   Spent", 0, 0);

		/* Display the flags in this set */
		for (col = 0, row = 1, z = 0; z < num; z++)
		{
			/* Get flag from index */
			i = index_to_flag[z];

			/* Halfway point -- Go to next column of flags */
			if (z == (num + 1) / 2)
			{
				col = 40;
				row = 1;
			}

			/* Get color of flag name */
			if (z == selection) attr = TERM_YELLOW;
			else                attr = TERM_WHITE;

			/* Print out this flag's name */
			c_prt(attr, format("%s", flag_creation_data[i].desc), row, col);

			/* Get flag cost */
			cost = flag_creation_data[i].cost;

			/* Print out this flag's cost, if any */
			if (cost) prt(format("%2d", cost), row, col + 26);

			/* Display invested essences */
			if (ffi[i].invested)
			{
				/* Get chance of success */
				int chance = calc_infusion_chance(cost, ffi[i].invested);

				/* Colorful indicator of success chance */
				if      (chance >= 90) attr = TERM_PURPLE;
				else if (chance >= 80) attr = TERM_BLUE;
				else if (chance >= 70) attr = TERM_L_BLUE;
				else if (chance >= 50) attr = TERM_WHITE;
				else                   attr = TERM_SLATE;

				/* Display */
				c_prt(attr, format(":%2d", ffi[i].invested), row, col + 31);
			}

			/* No essences invested yet */
			else
			{
				if (p_ptr->essence[flag_creation_data[i].essence_sval] >= cost)
					attr = TERM_SLATE;
				else
					attr = TERM_L_DARK;

				/* Display type of essence needed */
				c_prt(attr, format("%s",
					short_essence_name(flag_creation_data[i].essence_sval)),
					row, col + 31);
			}

			/* Next row */
			row++;
		}

		/* Prompt */
		prt("Qualities  <dir> to move, +/- to invest essences", 23, 0);
		prt("           </> to change screens, '?' for help, Return to accept, ESC to cancel", 24, 0);


		/* Hide the cursor */
		inkey_cursor_hack[TERM_MAIN] = -1;

		/* Wait for legal input */
		while (TRUE)
		{
			/* Get key */
			ch = inkey(ALLOW_CLICK);

			/* Allow orthogonal direction keys */
			get_ui_direction(&ch, UI_NODIAG | UI_NOMOUSE, &dummy);

			/* Accept anything except untranslated mouse actions */
			if (ch != MOUSEKEY) break;
		}

		/* Allow the cursor to be shown again */
		inkey_cursor_hack[TERM_MAIN] = 0;


		/* Cancel */
		if (ch == ESCAPE)
		{
			action = -99;
			break;
		}

		/* Accept changes */
		if ((ch == '\r') || (ch == '\n'))
		{
			action = 0;
			break;
		}

		/* Switch to next flag set */
		else if ((ch == '\t') || (ch == '>'))
		{
			action = 1;
			break;
		}

		/* Switch to previous flag set */
		else if (ch == '<')
		{
			action = -1;
			break;
		}

		/* Help me. */
		else if (ch == '?')
		{
			p_ptr->get_help_index = HELP_INFUSION;
			do_cmd_help();
			(void)Term_clear();
		}

		/* Go to previous flag */
		else if (ch == '8')
		{
			if (selection > 0) selection--;
		}

		/* Go to next flag */
		else if ((ch == '2') || (ch == ' '))
		{
			if (selection < num - 1) selection++;
		}

		/* Go to previous column */
		else if (ch == '4')
		{
			if (selection >= (num+1) / 2) selection -= (num+1) / 2;
		}

		/* Go to next column */
		else if (ch == '6')
		{
			if (selection < num / 2) selection += (num+1) / 2;
		}

		/* Raise a number */
		else if ((ch == '+') || (ch == '='))
		{
			/* Get flag from index */
			i = index_to_flag[selection];

			/* Jump from no to base number of essences */
			if (ffi[i].invested == 0)
			{
				invest = flag_creation_data[i].cost;
			}

			/* Increment investment by 1 */
			else
			{
				invest = ffi[i].invested + 1;
			}

			/* Get essence kind */
			sval = flag_creation_data[i].essence_sval;

			/* Ante up if we have enough essences of this kind */
			if (essences_available(sval) + ffi[i].invested  >= invest)
			{
				/* New flag being bought -- get potential cost */
				ffi[i].loss = get_cost_of_flag(i, 0);

				/* Invest essences */
				ffi[i].invested = invest;
			}
		}

		/* Lower a number */
		else if ((ch == '-') || (ch == '_'))
		{
			/* Get flag from index */
			i = index_to_flag[selection];

			/* Drop from base number to no essences */
			if (ffi[i].invested <=
				flag_creation_data[i].cost)
			{
				invest = 0;
			}

			/* Decrement investment by 1 */
			else
			{
				invest = ffi[i].invested - 1;
			}

			/* Lower the number of essences */
			ffi[i].invested = invest;

			/* Nothing invested -- remove potential cost */
			if (!ffi[i].invested) ffi[i].loss = 0;
		}
	}

	/* Return */
	return (action);
}




/*
 * Use essences to bias for particular object flags.
 */
static bool do_forging_essence(int a_idx, int *potential)
{
	int tmp = 0;
	int i, j, k;

	bool is_melee_weapon = FALSE;
	bool is_digger       = FALSE;
	bool is_launcher     = FALSE;
	bool is_ammo         = FALSE;
	bool is_armor       = FALSE;
	bool forbid          = FALSE;

	int low, high, adjust, val;
	int idx;

	artifact_type *a_ptr = &a_info[a_idx];

	int rand_chooser[DESC_LEN];
	int cost, chance;

	/* There are four sets of flags.  We start on the first one. */
	int mode = 0;
	int num_modes = 4;

	/* Pval storage (first entry is always zero) */
	s16b pval_val[4] = { 0, 1, 0, 0 };


	/* Save the screen, use the standard display, and center */
	display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_NORM, 80, 0);


	/* Wipe the flag-creation information */
	for (i = 0; i < 128; i++)
	{
		ffi[i].p_index = 0;
		ffi[i].invested = 0;
		ffi[i].loss = 0;
	}

	/* Put object in various categories */
	if (is_melee_weapon(a_ptr))    is_melee_weapon = TRUE;
	if (a_ptr->tval == TV_DIGGING) is_digger       = TRUE;
	if (is_missile_weapon(a_ptr))  is_launcher     = TRUE;
	if (is_missile(a_ptr))         is_ammo         = TRUE;
	if (is_any_armor(a_ptr))      is_armor       = TRUE;


	/* Determine pval maxima and disallow/restrict flags */
	for (i = 0; i < 128; i++)
	{
		/* Ignore flags that we can't afford */
		if (get_cost_of_flag(i, (i < 32) ? 1 : 0) > *potential)
		{
			ffi[i].max = 0;
			continue;
		}

		/* Assume that this object may have this flag */
		forbid = FALSE;
		val = 1;
		adjust = 0;

		/* Handle pval-dependant flags */
		if (i < 32)
		{
			if ((i == PVAL_AWARE) || (i == PVAL_INFRA) || (i == PVAL_MANA))
			{
				/* All items other than headgear get penalized */
				if ((a_ptr->tval != TV_HELM) &&
					 (a_ptr->tval != TV_CROWN))
				{
					adjust = -(pval_range[i][1] / 2);
				}
			}
			if (i == PVAL_TUNNEL)
			{
				/* Diggers get a bonus */
				if (is_digger) adjust = 3;

				/* Otherwise, must be a melee weapon */
				else if (!is_melee_weapon) forbid = TRUE;
			}
			if (i == PVAL_BLOWS)
			{
				/* Must be a melee weapon */
				if (!is_melee_weapon) forbid = TRUE;
			}
			if ((i == PVAL_SHOTS) || (i == PVAL_MIGHT))
			{
				/* Must be a missile launcher */
				if (!is_launcher) forbid = TRUE;
			}
			if (i == PVAL_SPEED)
			{
				/* Boots get a bonus */
				if (a_ptr->tval == TV_BOOTS) adjust = 4;

				/* Everything else gets a penalty */
				else adjust = -3;
			}
			if ((i == PVAL_STEALTH) || (i == PVAL_INVIS))
			{
				/* Cloaks get a bonus (except that they can't be made yet!) */
				if (a_ptr->tval == TV_CLOAK) adjust = 2;
			}
			if ((i == PVAL_DISARM) || (i == PVAL_DEVICE))
			{
				/* Gloves get a bonus */
				if (a_ptr->tval == TV_GLOVES) adjust = 3;

				/* Everything else gets a penalty */
				else adjust = -(pval_range[i][1] / 3);
			}


			/* Ammo can never have pvals */
			if (is_ammo) forbid = TRUE;

			/* Get upper and lower bounds */
			low  = pval_range[i][0];
			high = pval_range[i][1];

			/* Get a pval within the range, using potential */
			val = low + ((high + adjust - low) * (*potential) / 5000);

			/* Never go above maximum or below 1 */
			if (val > high) val = high;
			if (val < 1)    val = 1;
		}

		/* Many flags only make sense for weapons and ammo */
		else if ((i == SLAY_ANIMAL) || (i == SLAY_EVIL)   ||
		         (i == SLAY_UNDEAD) || (i == SLAY_DEMON)  ||
		         (i == SLAY_ORC)    || (i == SLAY_TROLL)  ||
		         (i == SLAY_GIANT)  || (i == SLAY_DRAGON) ||
		         (i == BRAND_ACID)  || (i == BRAND_ELEC)  ||
		         (i == BRAND_FIRE)  || (i == BRAND_COLD)  ||
		         (i == BRAND_POIS)  || (i == VORPAL))
		{
			if ((!is_melee_weapon) && (!is_digger) && (!is_ammo))
				forbid = TRUE;
		}

		/* Some other flags only make sense for weapons */
		else if ((i == THROWING) || (i == PERFECT_BALANCE) ||
		         (i == BLESSED)  || (i == IMPACT)          ||
		         (i == SOULSTEAL))
		{
			if ((!is_melee_weapon) && (!is_digger))
				forbid = TRUE;
		}

		/* Ammo cannot have any other flags */
		else if (is_ammo) forbid = TRUE;


		/* If forbidden, set max to zero */
		if (forbid) ffi[i].max = 0;

		/* Otherwise, set it to calculated value (or allow the flag) */
		else ffi[i].max = val;
	}


	/* Repeat until user is satisfied */
	while (TRUE)
	{
		/* Pval-dependant flags */
		if (mode == 3)
		{
			tmp = do_forging_essence_pval(pval_val);
		}

		/* Non-pval flags */
		else
		{
			tmp = do_forging_essence_flags(mode);
		}

		/* Advance to next set */
		if (tmp == 1)
		{
			mode += 1;
			if (mode >= num_modes) mode = 0;
		}

		/* Retreat to previous set */
		else if (tmp == -1)
		{
			mode -= 1;
			if (mode < 0) mode = num_modes - 1;
		}

		/* Cancel or accept changes */
		else if (tmp != 0)
		{
			/* Restore previous display */
			display_change(DSP_RESTORE | DSP_LOAD, 0, 0);

			return (FALSE);
		}
		else
		{
			break;
		}
	}

	/* Save indexes of chosen flags */
	for (tmp = 0, i = 0; i < 128; i++)
	{
		/* We've invested something in this flag */
		if (ffi[i].invested) rand_chooser[tmp++] = i;
	}

	/* We chose no flags */
	if (!tmp)
	{
		/* Restore previous display */
		display_change(DSP_RESTORE | DSP_LOAD, 0, 0);

		/* Leave */
		return (TRUE);
	}


	/* Mix up the indexes */
	for (i = 0; i < tmp; i++)
	{
		j = rand_int(tmp);
		k = rand_chooser[i];
		rand_chooser[i] = rand_chooser[j];
		rand_chooser[j] = k;
	}

	/* Pick flags until we run out of flags or remaining potential */
	for (idx = 0; idx < tmp; idx++)
	{
		/* Get flag index */
		i = rand_chooser[idx];

		/* Paranoia -- require some potential cost */
		if (!ffi[i].loss) continue;

		/* If we don't have enough potential left, stop immediately */
		if (*potential < ffi[i].loss) break;

		/* For pval-dependant flags, essence cost depends on desired pval */
		if (idx < 32)
		{
			cost = (flag_creation_data[i].cost *
			        pval_val[ffi[i].p_index] + 5) / 10;
			if (cost < 1) cost = 1;
		}

		/* For other flags, essence cost is fixed */
		else cost = flag_creation_data[i].cost;

		/* We have enough essences */
		if (p_ptr->essence[flag_creation_data[i].essence_sval] >= ffi[i].invested)
		{
			/* Calculate chance */
			chance = calc_infusion_chance(cost, ffi[i].invested);

			/* We succeeded in making this flag */
			if (chance > rand_int(100))
			{
				/* Use up some potential */
				*potential -= ffi[i].loss;

				/* Add the flag */
				if (i < 32)
				{
					/* Add it to the right pval set */
					if (ffi[i].p_index == 1)
					{
						a_ptr->flags_pval1 |= (1L << i);
						a_ptr->pval1 = pval_val[ffi[i].p_index];
					}
					if (ffi[i].p_index == 2)
					{
						a_ptr->flags_pval2 |= (1L << i);
						a_ptr->pval2 = pval_val[ffi[i].p_index];
					}
					if (ffi[i].p_index == 3)
					{
						a_ptr->flags_pval3 |= (1L << i);
						a_ptr->pval3 = pval_val[ffi[i].p_index];
					}
				}
				else if (i < 64)
				{
					a_ptr->flags1 |= (1L << (i % 32));
				}
				else if (i < 96)
				{
					a_ptr->flags2 |= (1L << (i % 32));
				}
				else if (i < 128)
				{
					a_ptr->flags3 |= (1L << (i % 32));
				}
			}

			/* Use up the essences invested (always) */
			p_ptr->essence[flag_creation_data[i].essence_sval] -= ffi[i].invested;
		}
	}


	/* Restore previous display */
	display_change(DSP_RESTORE | DSP_LOAD, 0, 0);

	/* Return */
	return (TRUE);
}

/*
 * Adjust effective forging skill.
 *
 * Because the forging skills might otherwise degenerate into a money-
 * maker, we require some combat skill in order to forge powerful items.
 *
 * Note that a combat skill of 60 removes any restrictions; we're pretty
 * lenient.
 *
 * We must be careful about the test for throwing skill; it was previously
 * applied incorrectly to ammo.
 */
static int do_forging_adjust_power(object_type *i_ptr, int power)
{
	int skill = -1;


	/* Find the appropriate skill */
	if (i_ptr->tval == TV_SHOT)
	{
		/* Slingshot use the better of throwing or slings */
		int t = get_skill(S_THROWING, 0, 100);
		int s = get_skill(S_SLING, 0, 100);

		skill = (t > s) ? S_THROWING : S_SLING;
	}
	else if (i_ptr->tval == TV_SLING)        skill = S_SLING;
	else if (i_ptr->tval == TV_ARROW)        skill = S_BOW;
	else if (i_ptr->tval == TV_BOW)          skill = S_BOW;
	else if (i_ptr->tval == TV_BOLT)         skill = S_CROSSBOW;
	else if (i_ptr->tval == TV_CROSSBOW)     skill = S_CROSSBOW;

	else if (i_ptr->flags1 & (TR1_THROWING)) skill = S_THROWING;
	else if (i_ptr->tval == TV_SWORD)        skill = S_SWORD;
	else if (i_ptr->tval == TV_POLEARM)      skill = S_POLEARM;
	else if (i_ptr->tval == TV_HAFTED)       skill = S_HAFTED;


	/* Restrict forging power if necessary */
	if (skill >= 0)
	{
		int s = get_skill(skill, 10, 160);

		/* Note restriction */
		if (power > s)
		{
			msg_print("Your lack of skill in using this object makes it more difficult to forge...");
			power = s;
		}
	}

	/* Return */
	return (power);
}


/*
 * Forge a weapon, missile launcher, armor, or ammunition.
 *
 * There is, ah, a certain amount of hackishness in this here code.
 * Forged objects use artifact templates to gather attributes, are marked
 * with ego-item indexes as a substitute for a real object memory, require
 * extra flags in the basic object structure, and generally break a lot of
 * rules.  But it's worth it!
 */
static bool do_forging(int skill, int tval, cptr output_name)
{
	object_type *o_ptr;
	object_type *i_ptr;
	object_type forge;
	object_kind *k_ptr;

	/* Hack -- use the first artifact slot as a template */
	int a_idx = 0;
	artifact_type *a_ptr = &a_info[a_idx];

	int i;

	int item, sval;
	int max_level, level;

	int potential, max_potential, initial_potential;
	int component_level, component_sval;

	int count, chance;

	s32b tmp;

	bool corrupt_item = FALSE;
	bool accept = TRUE;

	char buf[DESC_LEN];

	cptr q = "Use which component?";
	cptr s = "You have no components to use.";


	/* Require a particular input */
	item_tester_tval = TV_COMPONENT;

	/* Choose an item, return on cancel */
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);
	item_to_object(o_ptr, item);


	/* Remember the component sval */
	component_sval = o_ptr->sval;

	/* Get level of component */
	component_level = k_info[o_ptr->k_idx].level;

	/* Maximum object level depends on skill and components (max of 100) */
	max_level = MIN(100, (5 * component_level / 4 + skill) / 2);

	/* Choose an item kind to make (or cancel) */
	if (!alchemy_choose_item(&tval, &sval, FALSE, max_level, output_name))
		return (FALSE);


	/* Get local object */
	i_ptr = &forge;

	/* Make object */
	object_prep(i_ptr, lookup_kind(tval, sval));

	/* Reduce forging ability if you don't know how to use the object */
	max_level = do_forging_adjust_power(i_ptr, max_level);

	/* Get object name */
	strip_name(buf, i_ptr->k_idx);

	/* Get the object kind */
	k_ptr = &k_info[i_ptr->k_idx];


	/* Initialize the artifact template, get maximal potential */
	max_potential = init_temporary_artifact(a_idx, tval, sval);
	if (max_potential > 7000) max_potential = 7000;

	/* High-level objects get (slightly) less potential. */
	level = div_round(k_ptr->level, 4);

	/* Never allow low-level objects to get too good or safe */
	if (level < max_level / 10) level = max_level / 10;

	/* The potential ranges up to about 6500, */
	potential = (max_level - level) * 80 - 500;

	/* But is adjusted according to maximum potential */
	tmp = (s32b)potential * max_potential / 7000;

	/* Always allow something to work with */
	potential = MAX(200, (int)tmp);

	/* Remember average potential */
	ave_potential = potential;

	/* Randomize actual potential slightly */
	potential = Rand_normal(potential, potential / 10);

	/* Remember initial potential */
	initial_potential = potential;

	/* Check to see if the character has any essences */
	for (count = 0, i = 0; i < NUM_ESSENCE; i++)
	{
		count += p_ptr->essence[i];
	}

	/* Character has some essences on hand, and has some infusion skill */
	if ((count) && (get_skill(S_INFUSION, 0, 100) >= LEV_REQ_INFUSE))
	{
		/* Ask */
		if (get_check("Would you like to use essences to directly alter your forged item?"))
		{
			/* Use essences to bias for specific flags, use up potential */
			accept = do_forging_essence(a_idx, &potential);
		}
	}

	/* Allow cancel */
	if (!accept)
	{
		/* Clean up -- Wipe the temporary artifact template */
		WIPE(a_ptr, artifact_type);

		/* Return */
		return (FALSE);
	}


	/* Reduce inventory */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_optimize(item);
	}

	/* Reduce floor item */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_optimize(0 - item);
	}



	/* Failure chance is determined by object level versus max level */
	chance = div_round(150 * level, max_level);

	/* Failure chance increases with amount of potential used on essences */
	chance += (initial_potential - potential) /
	          (get_skill(S_INFUSION, 6, 30) * get_skill(S_INFUSION, 6, 20));

	/* Check for failure */
	if (chance > rand_int(100))
	{
		/* Check for total disaster */
		if (chance > rand_int(100))
		{
			/* Oh no! */
			msg_format("Your %s explodes violently!", buf);

			/* Explode (can do more than 350 damage on rare occasions) */
			(void)explosion(0, (int)(1 + (initial_potential / 1200)),
			                p_ptr->py, p_ptr->px,
			                (int)(20 + initial_potential / 20), GF_SHARD);

			/* Sometimes cause an earthquake */
			if (!p_ptr->leaving)
			{
				if (initial_potential > rand_range(1500, 7500))
				{
					earthquake(p_ptr->py, p_ptr->px,
						3 + initial_potential / 500);
				}
			}

			/* All over, man */
			return (TRUE);
		}

		/* Partial failure */
		else
		{
			/* Item gets corrupted, player gets warned */
			msg_format("You do something wrong; your %s was corrupted!", buf);
			corrupt_item = TRUE;
		}
	}


	/* Use up the remaining potential by adding semi-random attributes */
	design_temporary_artifact(a_idx, potential, corrupt_item);


	/* Hack -- transfer attributes to the object */
	i_ptr->pval  = a_ptr->pval1;
	i_ptr->pval2 = a_ptr->pval2;
	i_ptr->pval3 = a_ptr->pval3;

	i_ptr->flags_pval1 = a_ptr->flags_pval1;
	i_ptr->flags_pval2 = a_ptr->flags_pval2;
	i_ptr->flags_pval3 = a_ptr->flags_pval3;

	i_ptr->flags1 = a_ptr->flags1;
	i_ptr->flags2 = a_ptr->flags2;
	i_ptr->flags3 = a_ptr->flags3;

	i_ptr->to_h = a_ptr->to_h;
	i_ptr->to_d = a_ptr->to_d;
	i_ptr->to_a = a_ptr->to_a;

	i_ptr->ac = a_ptr->ac;

	i_ptr->dd = a_ptr->dd;
	i_ptr->ds = a_ptr->ds;

	/* Hack -- extract the "cursed" flag */
	if (i_ptr->flags3 & (TR3_LIGHT_CURSE)) i_ptr->ident |= (IDENT_CURSED);

	/* Transfer weight */
	i_ptr->weight = a_ptr->weight;

	/* Allow some variance for mithril (light) and adamant (heavy) */
	if (o_ptr->sval == SV_COMPONENT_MITHRIL)
	{
		if (i_ptr->weight >= 35)
			i_ptr->weight -= (i_ptr->weight / 20) * 5;
	}
	if (o_ptr->sval == SV_COMPONENT_ADAMANT)
	{
		if (i_ptr->weight >= 35)
			i_ptr->weight += (i_ptr->weight / 20) * 5;
	}

	/* Hack -- Activations on forged items are rare */
	if ((!corrupt_item) && (i_ptr->tval != TV_DRAG_ARMOR) &&
	    (initial_potential < randint(10000)))
	{
		i_ptr->activate = 0;
	}
	else
	{
		i_ptr->activate = a_ptr->activate;
	}

	/* Special case -- some items come in quantity */
	if ((k_ptr->gen_dice > 1) || (k_ptr->gen_side > 1))
	{
		i_ptr->number = damroll(k_ptr->gen_dice, k_ptr->gen_side);
	}

	/* Base value is 1/10ths normal, but sale price may well be higher */
	if (!corrupt_item)
	{
		i_ptr->b_cost = k_ptr->cost + initial_potential;
		i_ptr->b_cost /= i_ptr->number;

		/* Paranoia -- always let items be sold */
		if (i_ptr->b_cost < 1) i_ptr->b_cost = 1;
	}

	/* Hack -- assume that corrupted items have no base value */
	else
	{
		i_ptr->b_cost = 0L;
	}


	/* Hack -- If an object resists an element, it also ignores it */
	if ((i_ptr->flags2 & (TR2_RES_ACID)) || (i_ptr->flags2 & (TR2_IM_ACID)))
		i_ptr->flags2 |= (TR2_IGNORE_ACID);
	if ((i_ptr->flags2 & (TR2_RES_ELEC)) || (i_ptr->flags2 & (TR2_IM_ELEC)))
		i_ptr->flags2 |= (TR2_IGNORE_ELEC);
	if ((i_ptr->flags2 & (TR2_RES_FIRE)) || (i_ptr->flags2 & (TR2_IM_FIRE)))
		i_ptr->flags2 |= (TR2_IGNORE_FIRE);
	if ((i_ptr->flags2 & (TR2_RES_COLD)) || (i_ptr->flags2 & (TR2_IM_COLD)))
		i_ptr->flags2 |= (TR2_IGNORE_COLD);

	/*
	 * Hack -- Give the forged object an ego-item index that depends on
	 * the component used to make it.
	 */
	if (component_sval == SV_COMPONENT_COPPER)
	     i_ptr->ego_item_index = EGO_FORGED_COPPER;
	else if (component_sval == SV_COMPONENT_BRONZE)
	     i_ptr->ego_item_index = EGO_FORGED_BRONZE;
	else if (component_sval == SV_COMPONENT_IRON)
	     i_ptr->ego_item_index = EGO_FORGED_IRON;
	else if (component_sval == SV_COMPONENT_STEEL)
	     i_ptr->ego_item_index = EGO_FORGED_STEEL;
	else if (component_sval == SV_COMPONENT_RUNEGOLD)
	     i_ptr->ego_item_index = EGO_FORGED_RUNEGOLD;
	else if (component_sval == SV_COMPONENT_MITHRIL)
	     i_ptr->ego_item_index = EGO_FORGED_MITHRIL;
	else if (component_sval == SV_COMPONENT_ADAMANT)
	     i_ptr->ego_item_index = EGO_FORGED_ADAMANT;


	/* Full identification (we always give full ID  XXX) */
	object_aware(i_ptr);
	object_mental(i_ptr);

	/* Give the forged object to the character */
	give_object(i_ptr, FALSE);

	/* Describe the object fully (also reset screen display) */
	do_cmd_observe(i_ptr, FALSE);

	/* Clean up -- Wipe the temporary artifact template */
	WIPE(a_ptr, artifact_type);

	return (TRUE);
}


/*
 * Make melee weapon
 */
bool make_melee_weapon(void)
{
	char tmp;
	int tval = 0;
	cptr desc;

	int weap  = get_skill(S_FORGE_WEAPON, 0, 80);
	int infus = get_skill(S_INFUSION, 0, 20);
	int skill = weap + infus;

	/* Interact until satisfied */
	while (TRUE)
	{
		/* Choose which type of object to make */
		if (!get_com("Forge a (S)word, (P)olearm, or (B)lunt weapon?", &tmp))
		{
			msg_print("Cancelled...");
			return (FALSE);
		}

		/* Choose a weapon type */
		if      ((tmp == 'S') || (tmp == 's')) tval = TV_SWORD;
		else if ((tmp == 'P') || (tmp == 'p')) tval = TV_POLEARM;
		else if ((tmp == 'H') || (tmp == 'h')) tval = TV_HAFTED;
		else if ((tmp == 'B') || (tmp == 'b')) tval = TV_HAFTED;

		/* Verify tval, or complain */
		if (tval) break;
		else msg_print("Please choose a type of object to make, or press ESC.");
	}

	/* Get object name */
	if      (tval == TV_SWORD)   desc = "sword";
	else if (tval == TV_POLEARM) desc = "polearm";
	else if (tval == TV_HAFTED)  desc = "hafted weapon";
	else return (FALSE);

	/* Make item(s) */
	return (do_forging(skill, tval, desc));
}

/*
 * Make armor
 */
bool make_armor(void)
{
	char tmp;
	int tval = 0;
	cptr desc;

	/* Get skill */
	int armor = get_skill(S_FORGE_ARMOR, 0, 80);
	int infus = get_skill(S_INFUSION, 0, 20);
	int skill = armor + infus;

	/* Interact until satisfied */
	while (TRUE)
	{
		/* Choose which type of object to make */
		if (!get_com("Forge (B)oots, (G)loves, (H)eadgear, a (S)hield, (L)ight or (P)late armor?",	&tmp))
		{
			msg_print("Cancelled...");
			return (FALSE);
		}

		/* Choose an armor type */
		if      ((tmp == 'B') || (tmp == 'b')) tval = TV_BOOTS;
		else if ((tmp == 'G') || (tmp == 'g')) tval = TV_GLOVES;
		else if ((tmp == 'H') || (tmp == 'h')) tval = TV_HELM;
		else if ((tmp == 'S') || (tmp == 's')) tval = TV_SHIELD;
		else if ((tmp == 'L') || (tmp == 'l')) tval = TV_SOFT_ARMOR;
		else if ((tmp == 'P') || (tmp == 'p')) tval = TV_HARD_ARMOR;

		/* Verify tval, or complain */
		if (tval) break;
		else msg_print("Please choose a type of object to make, or press ESC.");
	}

	/* Get object name */
	if (tval == TV_BOOTS)           desc = "footwear";
	else if (tval == TV_GLOVES)     desc = "gloves";
	else if (tval == TV_HELM)       desc = "headgear";
	else if (tval == TV_SHIELD)     desc = "shield";
	else if (tval == TV_SOFT_ARMOR) desc = "soft body armor";
	else if (tval == TV_HARD_ARMOR) desc = "hard body armor";
	else return (FALSE);

	/* Make item(s) */
	return (do_forging(skill, tval, desc));
}

/*
 * Make a missile launcher or ammunition
 */
bool make_launcher_or_ammo(void)
{
	char tmp;
	int tval = 0;
	cptr desc;

	int weap  = get_skill(S_FORGE_WEAPON, 0, 80);
	int infus = get_skill(S_INFUSION, 0, 20);
	int skill = weap + infus;

	/* Interact until satisfied */
	while (TRUE)
	{
		/* Choose which type of object to make */
		if (!get_com("Forge a (M)issile launcher, or (S)hots, (A)rrows, or (B)olts?", &tmp))
		{
			msg_print("Cancelled...");
			return (FALSE);
		}

		/* Choose a launcher or ammo type */
		if      ((tmp == 'M') || (tmp == 'm')) tval = TV_BOW;
		else if ((tmp == 'S') || (tmp == 's')) tval = TV_SHOT;
		else if ((tmp == 'A') || (tmp == 'a')) tval = TV_ARROW;
		else if ((tmp == 'B') || (tmp == 'b')) tval = TV_BOLT;

		/* Verify tval, or complain */
		if (tval) break;
		else msg_print("Please choose a type of object to make, or press ESC.");
	}

	/* Get object name */
	if      (tval == TV_BOW)   desc = "missile launcher";
	else if (tval == TV_SHOT)  desc = "shot";
	else if (tval == TV_ARROW) desc = "arrows";
	else if (tval == TV_BOLT)  desc = "bolts";
	else return (FALSE);

	/* Make item(s) */
	return(do_forging(skill, tval, desc));
}



/*
 * Hook for "get_item()".  Determine if something can be poisoned.
 */
static bool item_tester_hook_poisonable(const object_type *o_ptr)
{
	/* Poison any type of ammo */
	if (is_missile(o_ptr))
	{
		/* Require that the ammo be identified */
		if (!object_known_p(o_ptr)) return (FALSE);

		/* Require that there be nothing fancy about the ammo */
		if (o_ptr->artifact_index) return (FALSE);
		if (o_ptr->ego_item_index) return (FALSE);
		if (o_ptr->flags1)
		{
			u32b temp_flags1 = o_ptr->flags1;

			/* Some flags1 are allowable */
			temp_flags1 &= ~(TR1_THROWING | TR1_TWO_HANDED_REQ | TR1_TWO_HANDED_DES);

			/* Test */
			if (temp_flags1) return (FALSE);
		}
		if (o_ptr->flags2)
		{
			u32b temp_flags2 = o_ptr->flags2;

			/* Some flags2 are allowable */
			temp_flags2 &= ~(TR2_IGNORE_ACID | TR2_IGNORE_ELEC | TR2_IGNORE_FIRE | TR2_IGNORE_COLD);

			/* Test */
			if (temp_flags2) return (FALSE);
		}

		/* Flags3 deliberately not tested -- might change this later */

		/* *Sniff* -- we'll take it */
		return (TRUE);
	}

	/* Nope */
	return (FALSE);
}


/*
 * Hook for "get_item()".  Determine if something can be used to poison.
 */
static bool item_tester_hook_can_poison(const object_type *o_ptr)
{
	/* Require that the object be aware */
	if (!object_aware_p(o_ptr)) return (FALSE);

	/* Accept various kinds of mushrooms */
	if (o_ptr->tval == TV_FOOD)
	{
		if (o_ptr->sval == SV_FOOD_POISON)       return (TRUE);
		if (o_ptr->sval == SV_FOOD_ENVENOMATION) return (TRUE);
		if (o_ptr->sval == SV_FOOD_SICKNESS)     return (TRUE);
		if (o_ptr->sval == SV_FOOD_DISEASE)      return (TRUE);
	}

	/* Accept one kind of potion */
	if (o_ptr->tval == TV_POTION)
	{
		if (o_ptr->sval == SV_POTION_POISON)     return (TRUE);
	}

	/* Let's not get too permissive here */
	return (FALSE);
}


/*
 * Poison some ammunition
 */
bool poison_ammo(int val)
{
	object_type *o_ptr;
	object_type *i_ptr;
	object_type *j_ptr;
	object_type object_type_body;

	int item1, item2;

	int num;
	int can_poison = 1;
	int num_to_poison = 1;
	int poisons_used = 1;


	/* Choose something to poison with */
	cptr q = "Use what to poison with?";
	cptr s = "You have nothing that will poison missiles.";

	/* Require something to poison with */
	item_tester_hook = item_tester_hook_can_poison;

	/* Choose an item, return on cancel */
	if (!get_item(&item1, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);
	item_to_object(o_ptr, item1);


	/* Get some missiles to poison */
	q = "Poison which missiles?";
	s = "You have no missiles to poison.";

	/* Require something to poison with */
	item_tester_hook = item_tester_hook_poisonable;

	/* Choose an item, return on cancel */
	if (!get_item(&item2, q, s, (USE_INVEN | USE_FLOOR | USE_EQUIP))) return (FALSE);
	item_to_object(i_ptr, item2);


	/* Calculate the number of missiles each source can poison */
	if (o_ptr->tval == TV_FOOD)
	{
		if (o_ptr->sval == SV_FOOD_POISON)            num = (val + 2) / 3;
		else if (o_ptr->sval == SV_FOOD_ENVENOMATION) num = val;
		else if (o_ptr->sval == SV_FOOD_SICKNESS)     num = (val + 3) / 4;
		else if (o_ptr->sval == SV_FOOD_DISEASE)      num = (val + 1) / 2;
		else                                          num = 1;
	}
	else if (o_ptr->tval == TV_POTION)
	{
		if (o_ptr->sval == SV_POTION_POISON)          num = (val + 4) / 5;
		else                                          num = 1;
	}
	else
	{
		num = 1;
	}

	/* Be reasonable. */
	if (num < 1) num = 1;

	/* We're going to poison at least the basic number of missiles */
	num_to_poison = num;

	/* With more missiles than one source can poison, we ask for quantity */
	if (i_ptr->number > num)
	{
		/* We can poison missiles until either of our inputs run out */
		can_poison = MIN(o_ptr->number * num, i_ptr->number);
		num_to_poison =
			(int)get_quantity(format("Poison how many missiles (out of %d)?", i_ptr->number), 0, can_poison);

        if (!num_to_poison) return (FALSE);

		poisons_used = (num_to_poison + (num-1)) / num;
	}

	/* Cannot poison more ammo than we have */
	else num_to_poison = i_ptr->number;

	/* Get local object */
	j_ptr = &object_type_body;

	/* Copy the existing missiles */
	object_copy(j_ptr, i_ptr);

	/* Get quantity */
	j_ptr->number = num_to_poison;

	/* Print message */
	msg_print("You poison some missiles.");

	/* Learn about the item */
	object_known(j_ptr);

	/* Brand */
	j_ptr->ego_item_index = EGO_POISON;

	/* Prevent money-making. */
	j_ptr->cost_adjust = 20;


	/*
	 * Special note:  We have to reduce the missiles first because they are
	 * listed after the potions and mushrooms.  Were we to do it the other
	 * way, the object listed by item2 might change...  XXX XXX XXX
	 */

	/* Reduce inventory -- missiles */
	if (item2 >= 0)
	{
		inven_item_increase(item2, -num_to_poison);
		inven_item_optimize(item2);
	}

	/* Reduce floor item */
	else
	{
		floor_item_increase(0 - item2, -num_to_poison);
		floor_item_optimize(0 - item2);
	}

	/* Reduce inventory -- poisonous items */
	if (item1 >= 0)
	{
		inven_item_increase(item1, -poisons_used);
		inven_item_optimize(item1);
	}

	/* Reduce floor item */
	else
	{
		floor_item_increase(0 - item1, -poisons_used);
		floor_item_optimize(0 - item1);
	}

	/* Give the poisoned missiles to the character */
	if (item2 < INVEN_Q1)
	{
		give_object(j_ptr, FALSE);
	}
    else if (!quiver_carry(j_ptr, -1))  /* Try to add to quiver first */
    {
		give_object(j_ptr, FALSE);
    }

	return (TRUE);
}

