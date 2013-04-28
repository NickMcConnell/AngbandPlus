
/* File: obj_make.c */


/*
 * Creation and manipulation of objects by the character.
 *
 * Copyright (c) 2002
 * Leon Marrick, Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */


#include "angband.h"



/*
 * Open up the pouch, display it, and (optionally) select an essence.
 *
 * I could integrate this code with "get_item()", or with "show_inven()",
 * but keeping it separate will make it easier to improve.
 *
 * Return -2 if no essences are available, -1 if the user cancelled, or
 * the sval of the chosen essence(s).
 */
int get_essence(bool just_looking)
{
	int essence_sval = -1;   /* Assume cancel */

	object_type *i_ptr;
	object_type object_type_body;
	object_kind *k_ptr;

	char ch;
	int i, j, k, l, z = 0;
	int col, len, lim, num;

	char o_name[80];

	char tmp_val[80];

	int out_index[NUM_ESSENCE];
	byte out_color[NUM_ESSENCE];
	char out_desc[NUM_ESSENCE][80];


	/* Default length */
	len = 79 - 50;

	/* Maximum space allowed for descriptions */
	lim = 79 - 3;

	/* Find the "final" slot */
	for (i = 0; i < NUM_ESSENCE; i++)
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
		object_desc(o_name, i_ptr, TRUE, 1);

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
	screen_save();


	/* Find the column to start in */
	col = (len > 76) ? 0 : (79 - len);

	/* Output each entry */
	for (j = 0; j < k; j++)
	{
		/* Clear the line */
		prt("", j + 1, col ? col - 2 : col);

		/* Prepare an index --(-- */
		sprintf(tmp_val, "%c)", I2A(j));

		/* Clear the line with the (possibly indented) index */
		put_str(tmp_val, j+1, col);

		/* Display the entry itself */
		c_put_str(out_color[j], out_desc[j], j + 1, col + 3);
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);


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
				ch = tolower(ch);

				/* Convert choice to a number */
				num = A2I(ch);

				/* Asking for an essence we have */
				if (num < z)
				{
					essence_sval = out_index[num];
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
		default:             *adjust =  80;  return (GF_MISSILE);
	}
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
static int alchemy_choose_item(int tval, int *sval,
	bool need_essence, int max_level, cptr desc)
{
	int i, j;
	int num, last_num = 0, max_num;
	int quantity = 0;
	int col, row;
	int wgt;

	char ch;
	char o_name[160];
	char buf[160];

	object_kind *k_ptr;
	int k_idx;

	int choice[60];

	bool cost_too_high = FALSE;

	int old_rows = screen_rows;
	int tmp_rows;


	/* Save screen */
	screen_save();

	/* Clear screen */
	Term_clear();

	/* Set to 50 screen rows if scrolls or potions, 25 otherwise  XXX */
	if ((tval == TV_SCROLL) || (tval == TV_POTION))
	{
		Term_rows(TRUE);
	}
	else
	{
		Term_rows(FALSE);
	}

	/* Get working height of screen */
	tmp_rows = screen_rows - 1;


	/* Interact until satisfied */
	while (TRUE)
	{
		int x, y;

		/* Search the whole item list. */
		for (num = 0, i = 1; i < z_info->k_max; i++)
		{
			k_ptr = &k_info[i];

			/* Skip items not of the requested type */
			if (k_ptr->tval != tval) continue;

			/* Hack -- Skip instant artifacts */
			if (k_ptr->flags3 & (TR3_INSTA_ART)) continue;

			/* Usually require awareness or seen */
			if ((tval != TV_RING) && (tval != TV_AMULET))
			{
				if (!(k_ptr->special & (SPECIAL_EVER_SEEN | SPECIAL_AWARE)))
				{
					continue;
				}
			}

			/* No out-of-depth objects */
			if (k_ptr->level > max_level) continue;

			/* Hack -- you need some infusion skill to make grenades */
			if ((k_ptr->tval == TV_POTION) &&
			    (k_ptr->sval == SV_POTION_GRENADE))
			{
				if (get_skill(S_INFUSION, 0, 100) < LEV_REQ_GRENADE) continue;
			}

			/* Assume cost is not too high */
			cost_too_high = FALSE;

			/* Require objects with an essence cost */
			if (need_essence)
			{
				/* No essence cost, so must be illegal */
				if (!k_ptr->e_type[0]) continue;

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
			strnfmt(buf, sizeof(buf), "%c] %-27s%3d %3d.%d",
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

			char buf1[100];
			char buf2[100];
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
				center_string(buf, buf1, 80);
				c_prt(attr, buf, tmp_rows - 2, 0);
			}

			/* Point to the object kind */
			k_ptr = &k_info[choice[last_num]];


			/* Hack -- three grenades can be made at a time */
			if ((tval == TV_POTION) && (k_ptr->sval == SV_POTION_GRENADE))
			{
				quantity = 3;
			}

			/* Potions and scrolls can be made in quantity */
			else if ((tval == TV_POTION) || (tval == TV_SCROLL))
			{
				/* Number depends on infusion skill and object value */
				quantity = get_skill(S_INFUSION, 0, 70) /
					MAX(1, rsqrt(k_ptr->cost));
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

			/* It is dangeous to make lots of non-grenades at once */
			if ((quantity > 1) &&
				 ((tval != TV_POTION) || (k_ptr->sval != SV_POTION_GRENADE)))
			{
				strcat(buf2, "  (risk increases as you approach this limit)");
			}

			/* Display */
			center_string(buf, buf2, 80);
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
			Term_clear();
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
					center_string(buf, "You don't have enough essences to make this object.", 80);
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
				center_string(buf, "Please choose an object first.", 80);
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

					/* Describe the item */
					do_cmd_observe(o_ptr, TRUE);
				}

				/* Cannot learn about an unseen object */
				else
				{
					/* Complain */
					center_string(buf, "You have never seen this object.  You can make it, but know nothing about it.", 80);
					c_prt(TERM_YELLOW, buf, 1, 0);

					/* Move cursor */
					move_cursor(y, x);
				}
			}

			/* Oops -- need to select something */
			else
			{
				/* Complain */
				center_string(buf, "Please choose an object first.", 80);
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
				center_string(buf, "Please type an index to choose an object, RETURN to accept, or ESC to cancel.", 80);
				c_prt(TERM_YELLOW, buf, 1, 0);

				/* Move cursor */
				move_cursor(y, x);
			}
		}
	}

	/* Require a valid choice */
	if (last_num < 0) quantity = 0;

	/* Object is valid -- remember it */
	else *sval = k_info[choice[last_num]].sval;

	/* Clear screen */
	Term_clear();

	/* Set to previous screen rows */
	if (old_rows != screen_rows)
	{
		p_ptr->redraw |= (PR_MAP | PR_BASIC | PR_EXTRA);

		if (old_rows == 50) Term_rows(TRUE);
		else                Term_rows(FALSE);
	}

	/* Load screen */
	screen_load();


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

	/* Store essence costs */
	for (total = 0, i = 0; i < 4; i++)
	{
		/* Store sval and number of essences */
		count[i] = k_ptr->e_num[i];
		type[i]  = k_ptr->e_type[i];

		/* Sum up total number of essences required */
		total += k_ptr->e_num[i];
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
		essence_sval = type[i + 1];

		/* Given an essence sval, convert to a magic type */
		typ = essence_to_magic(&dummy, &essence_sval);

		/* Make some conversions to nastier spell effects */
		if ((typ == GF_MANA) && (one_in_(2))) typ = GF_DISENCHANT;

		/* Some magics have no effect */
		if ((typ == GF_PROTECTION) || (typ == -1)) return;

		/* Explode */
		project_star(0, 3 + dam / 50, p_ptr->py, p_ptr->px, dam, typ, 0L);

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
	int dam;

	int py = p_ptr->py;
	int px = p_ptr->px;

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

		if (rand_int(150) < p_ptr->skill_sav)
		{
			msg_print("You resist the effects!");
		}
		else
		{
			take_hit(damroll(2, k_ptr->level + 1), 0,
				"Your mind is blasted!", "binding magics gone wild");

			if (randint(k_ptr->level) >= 5)
			{
				(void)set_slow(p_ptr->slow + rand_range(4, 8));
			}
			if ((!p_ptr->resist_confu) && (randint(k_ptr->level) >= 10))
			{
				(void)set_confused(p_ptr->confused + rand_range(4, 8));
			}
			if ((!p_ptr->resist_blind) && (randint(k_ptr->level) >= 15))
			{
				(void)set_blind(p_ptr->blind + rand_range(8, 16), NULL);
			}
			if ((!p_ptr->free_act) && (randint(k_ptr->level) >= 20))
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
		essence_wild_magic(o_ptr, dam);
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
	 * physical destruction and hurt the character when they go off
	 * bang.  Alchemists are notorious for rearranging the landscape.
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


static int grenade_typ = -1;
static long grenade_dam = 0L;
static int essence_num = 0;

/*
 * Arm the Grenades of War!
 */
static bool make_grenade(object_type *o_ptr, int skill)
{
	byte dice, sides;

	/* We have not already chosen an essence */
	if (grenade_typ < 0)
	{
		int sval = -1;
		int adjust;
		int max;

		/* Choose an essence type */
		grenade_typ = essence_to_magic(&adjust, &sval);

		/* Note failure */
		if (grenade_typ == -1)
		{
			msg_print("Cancelled.");
			return (0);
		}

		/* May not use more than three essences (or whatever you've got) */
		max = MIN(p_ptr->essence[sval], 3);

		/* Decide how many essences to use */
		essence_num =
			get_quantity(format("Use how many essences (max is %d)?",
			max), max);

		/* Note cancel */
		if (!essence_num)
		{
			msg_print("Cancelled.");
			return (FALSE);
		}

		/* Hack -- Use up the essences */
		if (p_ptr->essence[sval] >= essence_num)
		    p_ptr->essence[sval] -= essence_num;
		else return (FALSE);

		/* More essences, more damage */
		if      (essence_num == 1) grenade_dam = 13L;
		else if (essence_num == 2) grenade_dam = 19L;
		else                       grenade_dam = 24L;

		/* Damage depends on essence type, number, and skill */
		grenade_dam = (grenade_dam * skill * rsqrt(skill) * adjust) / 10000L;
	}

	/* Make a grenade */
	object_prep(o_ptr, lookup_kind(TV_POTION, SV_POTION_GRENADE));

	/* Identify */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Turn into dice - always have the same dice for a given damage */
	dam_to_dice((int)grenade_dam, &dice, &sides, FALSE);

	/* Adjust damage dice */
	o_ptr->dd = dice;
	o_ptr->ds = sides;

	/* Store essence type */
	o_ptr->pval = grenade_typ;

	/* Grenades have some value */
	o_ptr->b_cost = (grenade_dam * grenade_dam / 40L);

	return (TRUE);
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
		if (p_ptr->essence[i]) has_essence = TRUE;
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
		char q[80];
		char s[80];

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
	max = alchemy_choose_item(output_tval, &sval, TRUE, max_level,
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
			char q[80];
			char s[80];

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
				/* Making grenades depends on infusion skill, not alchemy */
				skill = get_skill(S_INFUSION, 0, 100);

				/* Make the grenade  (Hack -- use up essences immediately) */
				if (!make_grenade(o_ptr, skill)) return (i != 0);

				/* Chance depends on damage, bottle level, and skill */
				kaboom_chance = grenade_dam - (k_ptr->level * 3) - skill;

				/* Never get too nasty */
				if (kaboom_chance > 50) kaboom_chance = 50;
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
		}

		/* There is a penalty for making more than one object at a time */
		if ((i > 0) && (!grenade))
		{
			kaboom_chance += (i * 100) / (get_skill(S_INFUSION, 2, 10) * max);
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
			give_object(o_ptr);
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
	c_prt((set == 0 ? TERM_YELLOW : TERM_SLATE), "Pvals", row, 1);

	/* Second set */
	c_prt((set == 1 ? TERM_YELLOW : TERM_SLATE), "Sustain/Slay", row, 9);

	/* Third set */
	c_prt((set == 2 ? TERM_YELLOW : TERM_SLATE), "Resist", row, 24);

	/* Fourth set */
	c_prt((set == 3 ? TERM_YELLOW : TERM_SLATE), "Qualities", row, 33);

	/* Instructions */
	c_prt(TERM_WHITE, "'<' for previous, '>' for next set", row, 45);
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
 * Get the approximate proportion of used to total potential, and
 * return a warning color.
 */
static cptr short_essence_name(int sval)
{
	cptr str;
	char name[80];

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
	int i;

	char buf[320];
	cptr str;


	/* Build up information about available essences */
	for (buf[0] = '\0', i = 0; i < NUM_ESSENCE; i++)
	{
		/* Character must have some of this essence */
		if (!p_ptr->essence[i]) continue;

		/* Get the short name of this essence */
		str = short_essence_name(i);

		/* Add spaces between essence types */
		if (i != 0) strcat(buf, "  ");

		/* Build the essence available string */
		strcat(buf, format("%-5s:%3d", str, essences_available(i)));
	}

	/* Position the cursor (allow four lines for display) */
	move_cursor(20, 0);

	/* Show the essences */
	c_roff(TERM_WHITE, buf, 1, 80);
}


/*
 * Choose pvals and pval-dependant qualities.
 *
 * Note that this display is only capable of showing 26 flags.
 */
static s16b do_forging_essence_pval(s16b *pval_val, int potential)
{
	int action = 0;

	char ch;

	char desc[100];

	/* Conversion from index of active flag to flag position */
	int index_to_flag[32];

	/* Current flag costs (depends on pval) */
	int flag_cost[32];

	int selection = -3;

	int col, row;
	int num = 0;

	int i, z, attr, pval, invest, sval, cost;

	bool pval_changed = FALSE;


	/* Clear screen */
	Term_clear();


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
		/* Print the navigation bar -- highlight set #0 */
		forging_essence_navbar(24, 0);

		/* Get and center the potential display */
		center_string(desc, "Potential", 80);

		/* Get a warning color and display it */
		c_prt(color_potential(potential), desc, 19, 0);

		/* Print essences */
		prt_essences_avail();

		/* Print pvals */
		c_put_str((selection == -3 ? TERM_YELLOW : TERM_WHITE),
			"Pval #1: ", 3, 17);
		c_put_str(TERM_L_BLUE, format("%2d", pval_val[1]), 3, 26);

		c_put_str((selection == -2 ? TERM_YELLOW : TERM_WHITE),
			"Pval #2: ", 3, 34);
		c_put_str(TERM_L_GREEN, format("%2d", pval_val[2]), 3, 43);

		c_put_str((selection == -1 ? TERM_YELLOW : TERM_WHITE),
			"Pval #3: ", 3, 51);
		c_put_str(TERM_VIOLET, format("%2d", pval_val[3]), 3, 60);

		c_prt(TERM_SLATE, "Attribute           Bonus Max  Spent    Attribute           Bonus Max  Spent", 4, 0);

		/* Display the pval-dependant flags */
		for (col = 0, row = 5, z = 0; z < num; z++)
		{
			/* Get flag from index */
			i = index_to_flag[z];

			/* Halfway point -- Go to next column of flags */
			if (z == (num + 1) / 2)
			{
				col = 40;
				row = 5;
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
				/* Color-colded pvals */
				attr = TERM_WHITE;
				if (ffi[i].p_index == 1) attr = TERM_L_BLUE;
				if (ffi[i].p_index == 2) attr = TERM_L_GREEN;
				if (ffi[i].p_index == 3) attr = TERM_VIOLET;

				/* Print out this flag's pval */
				c_prt(attr, format("%2d", pval_val[ffi[i].p_index]), row, col + 22);

				/* Print out this flag's maximum pval */
				prt(format("%d", ffi[i].max), row, col + 27);

				/* Display invested essences */
				if (ffi[i].p_index)
				{
					/* Colorful indicator of approximate chance of success */
					if      (cost*3 <= ffi[i].invested-1) attr = TERM_BLUE;
					else if (cost*2 <= ffi[i].invested-1) attr = TERM_L_BLUE;
					else if (cost   <= ffi[i].invested)   attr = TERM_WHITE;
					else                                  attr = TERM_RED;

					c_prt(attr, format("%2d", ffi[i].invested), row, col + 31);
				}
				else
				{
					c_prt(TERM_L_DARK, format("%s",
						short_essence_name(flag_creation_data[i].essence_sval)),
						row, col + 31);
				}
			}

			/* Note that attribute is too expensive to buy */
			else
			{
				c_prt(TERM_L_DARK, format("%s",
					short_essence_name(flag_creation_data[i].essence_sval)),
					row, col + 31);
			}

			/* Next row */
			row++;
		}

		/* Prompt */
		prt("Pvals     2/8 to move, +/- to adjust numbers, 4/6 to change pval indexes", 0, 0);
		prt("          </> to change flag sets, '?' for help, Return to accept, ESC to cancel", 1, 0);

		/* Hide the cursor XXX */
		move_cursor(26, 0);


		/* Get key */
		ch = inkey();

		/* Clear status line */
		clear_row(2);

		/* Cancel */
		if ((ch == ESCAPE) || (ch == 'q'))
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

		/* Help me. */
		else if (ch == '?')
		{
			p_ptr->get_help_index = HELP_INFUSION;
			do_cmd_help();
			Term_clear();
		}

		/* Switch to previous flag set */
		else if (ch == '<')
		{
			action = -1;
			break;
		}

		/* Go to previous flag or pval */
		else if ((ch == '8') || (ch == 'k'))
		{
			if (selection > -3) selection--;
		}

		/* Go to next flag or pval */
		else if ((ch == '2') || (ch == ' ') || (ch == 'j'))
		{
			if (selection < num - 1) selection++;
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
				if (ffi[i].invested == 0)
				{
					invest = flag_cost[i];
				}

				/* Increment investment by 1 */
				else
				{
					invest = ffi[i].invested + 1;
				}

				/* Get essence kind */
				sval = flag_creation_data[i].essence_sval;

				/* Ante up if we have enough essences of this kind */
				if (p_ptr->essence[sval] >= invest)
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

				/* Minumum pval of 0 */
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
		else if ((ch == '6') || (ch == '4') || (ch == 'l') || (ch == 'h'))
		{
			/* Only works when a flag is selected */
			if (selection >= 0)
			{
				/* get flag position */
				i = index_to_flag[selection];

				if ((ch == '6') || (ch == 'l'))
				{
					while (TRUE)
					{
						/* Go to next pval index, wrap around */
						ffi[i].p_index = ((ffi[i].p_index + 1) % 4);

						/* Stop if we find a pval that's not too high */
						if (pval_val[ffi[i].p_index] <= ffi[i].max) break;
					}
				}
				else
				{
					while (TRUE)
					{
						/* Go to previous pval index, wrap around */
						ffi[i].p_index--;
						if (ffi[i].p_index < 0) ffi[i].p_index = 3;

						/* Stop if we find a pval that's not too high */
						if (pval_val[ffi[i].p_index] <= ffi[i].max) break;
					}
				}

				/* Get this flag's pval */
				pval = pval_val[ffi[i].p_index];

				/* Recalculate this flag's essence cost */
				flag_cost[i] =
					MAX(1, (flag_creation_data[i].cost * pval + 5) / 10);
				if (!pval) flag_cost[i] = 0;
			}

			/* Undocumented, but intuitive, movement command */
			else
			{
				if ((ch == '6') || (ch == 'l')) selection++;
				else                            selection--;

				if (selection < -3) selection = -3;
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
						ffi[i].loss =
							get_cost_of_flag(i, pval);
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
 *
 * There is a bit of a space problem on this screen.
 */
static s16b do_forging_essence_flags(int set, int potential)
{
	int action = 0;

	char ch;

	char desc[100];

	/* Conversion from index of active flag to flag position */
	int index_to_flag[32];

	int selection = 0;

	int col, row;
	int num = 0;

	int i, z, attr, invest, sval, cost;

	/* Clear screen */
	Term_clear();


	/* Fill in flag index reference */
	for (i = set * 32; (i < set * 32 + 32); i++)
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
		forging_essence_navbar(24, set);

		/* Get and center the potential display */
		center_string(desc, "Potential", 80);

		/* Get a warning color and display it */
		c_prt(color_potential(potential), desc, 19, 0);

		/* Display available esences */
		prt_essences_avail();

		c_prt(TERM_SLATE, "Attribute               Cost   Spent    Attribute               Cost   Spent", 3, 0);

		/* Display the flags in this set */
		for (col = 0, row = 4, z = 0; z < num; z++)
		{
			/* Get flag from index */
			i = index_to_flag[z];

			/* Halfway point -- Go to next column of flags */
			if (z == (num + 1) / 2)
			{
				col = 40;
				row = 4;
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
				/* Colorful indicator of approximate chance of success */
				if      (cost*3 <= ffi[i].invested-1) attr = TERM_BLUE;
				else if (cost*2 <= ffi[i].invested-1) attr = TERM_L_BLUE;
				else if (cost   <= ffi[i].invested)   attr = TERM_WHITE;
				else                                  attr = TERM_RED;

				c_prt(attr, format(":%2d", ffi[i].invested), row, col + 31);
			}
			else
			{
				c_prt(TERM_L_DARK, format("%s",
					short_essence_name(flag_creation_data[i].essence_sval)),
					row, col + 31);
			}

			/* Next row */
			row++;
		}

		/* Prompt */
		prt("Flags     2/8 to move, +/- to adjust numbers, </> to change flag sets", 0, 0);
		prt("          '?' for help, Return to accept, ESC to cancel", 1, 0);

		/* Hide the cursor */
		move_cursor(26, 0);

		/* Get key */
		ch = inkey();

		/* Cancel */
		if ((ch == ESCAPE) || (ch == 'q'))
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
			Term_clear();
		}

		/* Go to previous flag */
		else if ((ch == '8') || (ch == 'k'))
		{
			if (selection > 0) selection--;
		}

		/* Go to next flag */
		else if ((ch == '2') || (ch == ' ') || (ch == 'j'))
		{
			if (selection < num - 1) selection++;
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
			if (p_ptr->essence[sval] >= invest)
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
	bool is_armour       = FALSE;
	bool forbid          = FALSE;

	int low, high, adjust, val;
	int idx;

	artifact_type *a_ptr = &a_info[a_idx];

	int rand_chooser[128];
	int cost, chance;

	/* There are four sets of flags.  We start on the first one. */
	int mode = 0;
	int num_modes = 4;

	/* Pval storage (first entry is always zero) */
	s16b pval_val[4] = { 0, 0, 0, 0 };


	/* Save the screen */
	screen_save();


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
	if (a_ptr->tval == TV_BOW)     is_launcher     = TRUE;
	if (is_missile(a_ptr))         is_ammo         = TRUE;
	if (is_any_armour(a_ptr))      is_armour       = TRUE;


	/* Determine pval maxima and disallow/restrict flags */
	for (i = 0; i < 128; i++)
	{
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
		if (mode == 0)
		{
			tmp = do_forging_essence_pval(pval_val, *potential);
		}

		/* Non-pval flags */
		else
		{
			tmp = do_forging_essence_flags(mode, *potential);
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
			/* Load the screen */
			screen_load();

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
		clear_from(0);
		screen_load();
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
			/* High cost hurts chance, high investment helps it */
			for (chance = 30, j = cost; j < ffi[i].invested; j++)
			{
				chance += (100 - chance) / (MIN(cost, 8) + 2);
			}

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

	/* Clear the screen */
	clear_from(0);

	/* Load the screen */
	screen_load();

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
 */
static int do_forging_adjust_power(int tval, int sval, int power)
{
	int skill = -1;

	/* Find the appropriate skill */
	if      (tval == TV_SWORD)   skill = S_SWORD;
	else if (tval == TV_POLEARM) skill = S_POLEARM;
	else if (tval == TV_HAFTED)  skill = S_HAFTED;
	else if (tval == TV_SHOT)    skill = S_SLING;
	else if (tval == TV_ARROW)   skill = S_BOW;
	else if (tval == TV_BOLT)    skill = S_CROSSBOW;
	else if (tval == TV_BOW)     skill = sbow(sval);

	/* Restrict forging power if necessary */
	if (skill >= 0) power = MIN(get_skill(skill, 10, 160), power);

	/* Return */
	return (power);
}


/*
 * Forge a weapon, missile launcher, armour, or ammunition.
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

	char buf[80];

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
	if (!alchemy_choose_item(tval, &sval, FALSE, max_level, output_name))
		return (FALSE);

	/* Note type of object, adjust power if necessary */
	max_level = do_forging_adjust_power(tval, sval, max_level);


	/* Get local object */
	i_ptr = &forge;

	/* Make object */
	object_prep(i_ptr, lookup_kind(tval, sval));

	/* Get object name */
	strip_name(buf, i_ptr->k_idx);

	/* Get the object kind */
	k_ptr = &k_info[i_ptr->k_idx];


	/* Initialize the artifact template, get maximal potential */
	max_potential = init_temporary_artifact(a_idx, tval, sval);
	if (max_potential > 7000) max_potential = 7000;

	/* High-level objects get (slightly) less potential. */
	level = k_ptr->level / 4;

	/* Never allow low-level objects to get too good or safe */
	if (level < max_level / 5) level = max_level / 5;

	/* The potential ranges up to about 6200 ... */
	potential = (max_level - level) * 84 - 500;

	/* ... with some variation (usually not more than 20%) */
	potential = Rand_normal(potential, potential / 10);

	/* Adjust according to maximum potential */
	tmp = (s32b)potential * max_potential / 7000;

	/* Always allow something to work with */
	potential = MAX(200, (int)tmp);

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


	/* Base failure chance depends on forging skill */
	chance = 40 - div_round(skill, 5);

	/* Failure chance is affected by object level and component quality */
	chance = div_round(chance * level, component_level);

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
			                (int)(25 + initial_potential / 20), GF_SHARD);

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


	/* Transfer weight */
	i_ptr->weight = a_ptr->weight;

	/* Allow some variance for mithril (light) and adamant (heavy) */
	if (o_ptr->sval == SV_COMPONENT_MITHRIL)
	{
		if (i_ptr->weight >= 40)
			i_ptr->weight -= (i_ptr->weight / 40) * 10;
	}
	if (o_ptr->sval == SV_COMPONENT_ADAMANT)
	{
		if (i_ptr->weight >= 40)
			i_ptr->weight += (i_ptr->weight / 40) * 10;
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
	else               i_ptr->b_cost = 0L;


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
	 if (component_sval == SV_COMPONENT_BRONZE)
	     i_ptr->ego_item_index = EGO_FORGED_BRONZE;
	 if (component_sval == SV_COMPONENT_IRON)
	     i_ptr->ego_item_index = EGO_FORGED_IRON;
	 if (component_sval == SV_COMPONENT_STEEL)
	     i_ptr->ego_item_index = EGO_FORGED_STEEL;
	 if (component_sval == SV_COMPONENT_RUNEGOLD)
	     i_ptr->ego_item_index = EGO_FORGED_RUNEGOLD;
	 if (component_sval == SV_COMPONENT_MITHRIL)
	     i_ptr->ego_item_index = EGO_FORGED_MITHRIL;
	 if (component_sval == SV_COMPONENT_ADAMANT)
	     i_ptr->ego_item_index = EGO_FORGED_ADAMANT;


	/* Full identification (we always give full ID  XXX) */
	object_aware(i_ptr);
	object_mental(i_ptr);

	/* Give the forged object to the character */
	give_object(i_ptr);

	/* Describe the object fully (also reset screen rows) */
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

	int skill = get_skill(S_FORGE_WEAPON, 0, 100);

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
 * Make armour
 */
bool make_armour(void)
{
	char tmp;
	int tval = 0;
	cptr desc;

	/* Get skill */
	int skill = get_skill(S_FORGE_ARMOR, 0, 100);


	/* Interact until satisfied */
	while (TRUE)
	{
		/* Choose which type of object to make */
		if (!get_com(
"Forge (B)oots, (G)loves, (H)eadgear, a (S)hield, (L)ight or (P)late armour?",
	&tmp))
		{
			msg_print("Cancelled...");
			return (FALSE);
		}

		/* Choose an armour type */
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
	else if (tval == TV_SOFT_ARMOR) desc = "soft body armour";
	else if (tval == TV_HARD_ARMOR) desc = "hard body armour";
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

	int skill = get_skill(S_FORGE_BOW, 0, 100);

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
		if (o_ptr->flags1)         return (FALSE);
		if (o_ptr->flags2)         return (FALSE);

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
	if (!get_item(&item2, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);
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
			get_quantity(format("Poison how many missiles (out of %d)?", i_ptr->number), can_poison);

		poisons_used = (num_to_poison + (num-1)) / num;
	}

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
	give_object(j_ptr);

	return (TRUE);
}

