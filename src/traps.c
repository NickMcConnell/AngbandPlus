/* File: traps.c */

/*
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

static char t_name[80];

/*
 * Efficient version of '(T) += strfmt((T), "%s", (S))'
 */
#define trap_desc_str_macro(T,S) do { \
 \
	cptr s = (S); \
 \
	/* Copy the string */ \
	while (*s) *(T)++ = *s++; \
 \
} while (0)

/*
 * Return a trap's name, plus appropriate article.
 *
 * Modes -
 *   0 -- Fire trap
 *   1 -- A fire trap
 *   2 -- The fire trap
 */
cptr trap_name(int w_idx, int mode)
{
	trap_widget *w_ptr = &w_info[w_idx];
	
	char *t, *s;

	/* Paranoia - if this happens, we're in trouble */
	if (!w_idx) return (NULL);

	t = t_name;
	s = w_name + w_ptr->name;

	if (mode == 1)
	{
		if (is_a_vowel(s[0])) trap_desc_str_macro(t, "an ");
		else trap_desc_str_macro(t, "a ");
	}
	else if (mode == 2)
	{
		trap_desc_str_macro(t, "the ");
	}

	/* Copy the string */
	for (; *s; s++)
	{
		if (*s == '^') continue;

		*t++ = *s;
	}

	/* Terminate string */
	*t = '\0';	

	/* Return name */
	return (&t_name[0]);
}

/*
 * Wipe a trap clean.
 */
static void trap_wipe(trap_type *t_ptr)
{
	/* Wipe the structure */
	(void)WIPE(t_ptr, trap_type);
}

/*
 * Prepare a trap based on an existing trap
 */
static void trap_copy(trap_type *t1_ptr, const trap_type *t2_ptr)
{
	/* Copy the structure */
	COPY(t1_ptr, t2_ptr, trap_type);
}

/*
 * Delete/Remove all the traps when the player leaves the level
 *
 * This is an efficient method of simulating multiple calls to the
 * "delete_trap()" function, with no visual effects.
 */
void wipe_t_list(void)
{
	int i;

	/* Delete all the traps */
	for (i = t_max - 1; i >= 1; i--)
	{
		trap_type *t_ptr = &t_list[i];

		/* Skip empty */
		if (!t_ptr->w_idx) continue;

		/* trap is gone */
		cave_t_idx[t_ptr->fy][t_ptr->fx] = 0;

		/* Wipe the trap */
		(void)WIPE(t_ptr, trap_type);
	}

	/* Reset "m_max" */
	t_max = 1;

	/* Reset "m_cnt" */
	t_cnt = 0;
}

/*
 * Delete a trap by index.
 */
static void delete_trap_idx(int i)
{
	int x, y;

	trap_type *t_ptr = &t_list[i];

	/* Get location */
	y = t_ptr->fy;
	x = t_ptr->fx; 

	/* trap is gone */
	cave_t_idx[y][x] = 0;

	/* Wipe the trap */
	(void)WIPE(t_ptr, trap_type);

	/* Count traps */
	t_cnt--;

	/* Visual update */
	lite_spot(y, x);
}

/*
 * Delete the trap, if any, at a given location
 */
void delete_trap(int y, int x)
{
	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Delete the trap (if any) */
	if (cave_t_idx[y][x] > 0) delete_trap_idx(cave_t_idx[y][x]);
}

/*
 * Move a trap from index i1 to index i2 in the trap list
 */
static void compact_traps_aux(int i1, int i2)
{
	int y, x;

	trap_type *t_ptr;

	/* Do nothing */
	if (i1 == i2) return;

	/* Get the trap */
	t_ptr = &t_list[i1];

	/* Get location */
	y = t_ptr->fy;
	x = t_ptr->fx;

	/* Repair grid */
	if (cave_t_idx[y][x] == i1)
	{
		/* Repair */
		cave_t_idx[y][x] = i2;
	}

	/* Hack -- move trap */
	trap_copy(&t_list[i2], &t_list[i1]);

	/* Hack -- wipe hole */
	trap_wipe(t_ptr);
}

/*
 * Compact and Reorder the trap list
 *
 * This function can be very dangerous, use with caution!
 *
 * After "compacting" (if needed), we "reorder" the traps into a more
 * compact order, and we reset the allocation info, and the "live" array.
 */
void compact_traps(int size)
{
	int i, y, x, num, cnt;

	int cur_lev, cur_dis, chance;

	/* Compact */
	if (size)
	{
		/* Message */
		message(MSG_GENERIC, 0, "Compacting traps...");

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);
	}

	/* Compact at least 'size' traps */
	for (num = 0, cnt = 1; num < size; cnt++)
	{
		/* Get more vicious each iteration */
		cur_lev = 5 * cnt;

		/* Get closer each iteration */
		cur_dis = 5 * (20 - cnt);

		/* Examine the traps */
		for (i = 1; i < t_max; i++)
		{
			trap_type *t_ptr = &t_list[i];

			/* Skip dead traps */
			if (!t_ptr->w_idx) continue;

			/* Get the location */
			y = t_ptr->fy;
			x = t_ptr->fx;

			/* Nearby traps start out "immune" */
			if ((cur_dis > 0) && (distance(p_ptr->py, p_ptr->px, y, x) < cur_dis)) continue;

			/* Saving throw */
			chance = 90;

			/* Apply the saving throw */
			if (rand_int(100) < chance) continue;

			/* Delete the trap */
			delete_trap_idx(i);

			/* Count it */
			num++;
		}
	}

	/* Excise dead traps (backwards!) */
	for (i = t_max - 1; i >= 1; i--)
	{
		trap_type *t_ptr = &t_list[i];

		/* \Skip real traps */
		if (t_ptr->w_idx) continue;

		/* Move last trap into open hole */
		compact_traps_aux(t_max - 1, i);

		/* Compress "o_max" */
		t_max--;
	}
}

/*
 * Get and return the index of a "free" trap.
 *
 * This routine should almost never fail, but it *can* happen.
 */
static s16b t_pop(void)
{
	int i;

	/* Normal allocation */
	if (t_max < z_info->t_max)
	{
		/* Get the next hole */
		i = t_max;

		/* Expand the array */
		t_max++;

		/* Count traps */
		t_cnt++;

		/* Return the index */
		return (i);
	}

	/* Recycle dead traps */
	for (i = 1; i < t_max; i++)
	{
		trap_type *t_ptr;

		/* Get the trap */
		t_ptr = &t_list[i];

		/* Skip live traps */
		if (t_ptr->w_idx) continue;

		/* Count traps */
		t_cnt++;

		/* Use this trap */
		return (i);
	}

	/* Warn the player (except during dungeon creation) */
	if (character_dungeon) message(MSG_GENERIC, 0, "Too many traps!");

	/* Try not to crash */
	return (0);
}

/*
 * Place a copy of a trap in the dungeon XXX XXX
 */
s16b trap_place(int y, int x, const trap_type *x_ptr)
{
	s16b t_idx;

	trap_type *t_ptr;

	/* Paranoia XXX XXX */
	if (cave_t_idx[y][x] != 0) return (0);

	/* Get a new record */
	t_idx = t_pop();

	/* Oops */
	if (t_idx)
	{
		/* Make a new trap */
		cave_t_idx[y][x] = t_idx;

		/* Get the new trap */
		t_ptr = &t_list[t_idx];

		/* Copy the trap XXX */
		trap_copy(t_ptr, x_ptr);

		/* Location */
		t_ptr->fy = y;
		t_ptr->fx = x;
	}

	/* Result */
	return (t_idx);
}

/*
 * Places a random trap at the given location.
 */
bool place_trap_dungeon(int y, int x)
{
	trap_type trap_type_body;
	trap_type *t_ptr = &trap_type_body;
	trap_widget *w_ptr;

	int w_idx;

	/* Paranoia */
	if (!in_bounds(y, x)) return (FALSE);

	/* Require empty space */
	if (!cave_empty_bold(y, x)) return (FALSE);

	/* Assign a trap type */
	while (TRUE)
	{
		w_idx = randint(z_info->w_max - 1);
		w_ptr = &w_info[w_idx];

		/* Check if floor trap */
		if (!(w_ptr->flags & WGF_FLOOR)) continue;

		/* Ensure minimum depth */
		if (p_ptr->depth < w_ptr->level) continue;

		/* Ensure that it affects players */
		if (!(w_ptr->flags & WGF_PLAYER)) continue;

		/* Rarity check */
		if (rand_int(w_ptr->rarity)) continue;

		/* HACK - no trap doors on the lowest level */
		if ((w_idx == WG_TRAP_DOOR) && (p_ptr->depth == MAX_DEPTH - 1)) continue;

		/* HACK - no trap doors on a fixed quest level  */
		if ((w_idx == WG_TRAP_DOOR) && ((quest_check(p_ptr->depth) == QUEST_FIXED) || 
			(quest_check(p_ptr->depth) == QUEST_FIXED_U))) continue;

		/* Accept */
		break;
	}

	t_ptr->w_idx = w_idx;
	t_ptr->visible = FALSE;

	/* Add charges to some trap types */
	if (w_ptr->max_charges)
	{
		t_ptr->charges = randint((w_ptr->max_charges * p_ptr->depth) / 100);
		if (t_ptr->charges == 0) t_ptr->charges = 1;
	}
	else t_ptr->charges = 0;

	if (!trap_place(y, x, t_ptr)) return (FALSE);

	/* Result */
	return (TRUE);
}

/*
 * Place a trap as a result of a monster spell
 */
bool place_trap_monster(u32b typ, int y, int x)
{
	trap_type trap_type_body;
	trap_type *t_ptr = &trap_type_body;
	trap_widget *w_ptr;

	int w_idx;

	/* Paranoia */
	if (!in_bounds(y, x)) return (FALSE);

	/* Require empty space */
	if (!cave_empty_bold(y, x)) return (FALSE);

	/* Assign a trap type */
	while (TRUE)
	{
		w_idx = randint(z_info->w_max - 1);

		w_ptr = &w_info[w_idx];

		/* Check if right spell trap */
		if (!(w_ptr->flags & typ)) continue;

		/* HACK - don't Ensure minimum depth */
		/* if (p_ptr->depth < w_ptr->level) continue; */

		/* Rarity check */
		if (rand_int(w_ptr->rarity)) continue;

		/* HACK - no trap doors on the lowest level */
		if ((w_idx == WG_TRAP_DOOR) && (p_ptr->depth == MAX_DEPTH - 1)) continue;

		/* HACK - no trap doors on a fixed quest level  */
		if ((w_idx == WG_TRAP_DOOR) && ((quest_check(p_ptr->depth) == QUEST_FIXED) || 
			(quest_check(p_ptr->depth) == QUEST_FIXED_U))) continue;

		/* Accept */
		break;
	}

	t_ptr->w_idx = w_idx;
	t_ptr->visible = FALSE;

	/* Add charges to some trap types */
	if (w_ptr->max_charges)
	{
		t_ptr->charges = randint((w_ptr->max_charges * p_ptr->depth) / 100);
		if (t_ptr->charges == 0) t_ptr->charges = 1;
	}
	else t_ptr->charges = 0;

	if (!trap_place(y, x, t_ptr)) return (FALSE);

	/* Result */
	return (TRUE);
}

/* 
 * Anti-monster traps placed by thieves. Note that this is taken from Oangband because
 * I didn't have time to implement the system I wanted. In future versions, this will
 * be replaced. For now, thanks to Oangbanf for the loan :)
 */
bool place_trap_player(int y, int x)
{
	int i;
	trap_type trap_type_body;
	trap_type *t_ptr = &trap_type_body;

	/* Paranoia -- Forbid more than one trap being set. */
	for (i =0; i < t_max; i++)
	{
		if (t_list[i].w_idx == WG_ANTI_MONSTER)
		{
			message(MSG_FAIL, 0, "You must disarm your existing trap to free up your equipment.");
			return (FALSE);
		}
	}

	/* Paranoia */
	if (!in_bounds(y, x)) return (FALSE);

	/* Require empty space */
	if (!cave_clean_bold(y, x)) return (FALSE);

	t_ptr->w_idx = WG_ANTI_MONSTER;
	t_ptr->visible = TRUE;
	t_ptr->charges = 0;

	if (!trap_place(y, x, t_ptr)) return (FALSE);

	/* Show the trap */
	lite_spot(y, x);

	/* Notify the player. */
	message(MSG_SUCCEED, 0, "You set a monster trap.");

	return (TRUE);
}

/*
 * Places a random trap at the given location.
 */
bool place_trap_chest(int y, int x)
{
	trap_type trap_type_body;
	trap_type *t_ptr = &trap_type_body;
	trap_widget *w_ptr;

	int w_idx;

	/* Paranoia */
	if (!in_bounds(y, x)) return (FALSE);

	/* Require closed, or secret door*/
	if ((cave_feat[y][x] != FEAT_CHEST) && (cave_feat[y][x] != FEAT_QST_CHEST)) 
		return (FALSE);

	/* Assign a trap type */
	while (TRUE)
	{
		w_idx = randint(z_info->w_max - 1);

		w_ptr = &w_info[w_idx];

		/* Check if floor trap */
		if (!(w_ptr->flags & WGF_CHEST)) continue;

		/* Ensure minimum depth */
		if (p_ptr->depth < w_ptr->level) continue;

		/* Ensure that it affects players */
		if (!(w_ptr->flags & WGF_PLAYER)) continue;

		/* Rarity check */
		if (rand_int(w_ptr->rarity)) continue;

		/* Accept */
		break;
	}
	
	/* Trap's visibility */
	t_ptr->w_idx = w_idx;
	t_ptr->visible = FALSE;

	/* HACK - Regular locks get random power, all other get full power */
	t_ptr->charges = w_ptr->max_charges;

	/* Spot difficulty */
	t_ptr->spot_factor = w_ptr->spot_factor;

	if (!trap_place(y, x, t_ptr)) return (FALSE);

	/* Result */
	return (TRUE);
}

/*
 * Places a random trap at the given location.
 */
bool place_lock(int y, int x, bool visible, byte type)
{
	trap_type trap_type_body;
	trap_type *t_ptr = &trap_type_body;

	/* Paranoia */
	if (!in_bounds(y, x)) return (FALSE);

	/* Require closed, or secret door*/
	if ((cave_feat[y][x] != FEAT_CLOSED) && (cave_feat[y][x] != FEAT_SECRET)) return (FALSE);

	t_ptr->w_idx = type;
	
	/* Locks visibility */
	t_ptr->visible = visible;

	/* HACK - Regular locks get random power, all other get full power */
	if (type == WG_DOOR_LOCK) t_ptr->charges = randint(w_info[type].max_charges);
	else t_ptr->charges = w_info[type].max_charges;

	if (!trap_place(y, x, t_ptr)) return (FALSE);

	/* Result */
	return (TRUE);
}

/*
 * Determine if a trap affects the player.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match trap power against player armor.
 */
static int check_hit(int power)
{
	int k, ac;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- 5% hit, 5% miss */
	if (k < 10) return (k < 5);

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Power competes against Armor */
	if ((power > 0) && (randint(power) >= (ac * 3 / 4))) return (TRUE);

	/* Assume miss */
	return (FALSE);
}

/*
 * Handle player hitting a real trap
 */
void hit_trap(int y, int x)
{
	trap_type *t_ptr = &t_list[cave_t_idx[y][x]];

	int dam, num, i;

	/* Actual trap effect */	
	switch (t_ptr->w_idx)
	{
		case WG_TRAP_DOOR:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You fall through a trap door!");
			if (p_ptr->ffall)
			{
				message(MSG_FFALL, 0, "You float gently down to the next level.");
			}
			else
			{
				dam = damroll(2, 8);
				take_hit(dam, trap_name(t_ptr->w_idx, 1));
			}

			/* New depth */
			p_ptr->depth++;

			/* Leaving */
			p_ptr->leaving = TRUE;

			break;
		}

		case WG_PIT_OPEN:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You fall into a pit!");
			if (p_ptr->ffall)
			{
				message(MSG_FFALL, 0, "You float gently to the bottom of the pit.");
			}
			else
			{
				dam = damroll(2, 6);
				take_hit(dam, trap_name(t_ptr->w_idx, 1));
			}
			break;
		}

		case WG_PIT_SPIKED:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You fall into a spiked pit!");

			if (p_ptr->ffall)
			{
				message(MSG_FFALL, 0, "You float gently to the floor of the pit.");
				message(MSG_FFALL, 0, "You carefully avoid touching the spikes.");
			}

			else
			{
				/* Base damage */
				dam = damroll(2, 6);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					message(MSG_TRAP, t_ptr->w_idx, "You are impaled!");

					dam = dam * 2;
					if (!p_ptr->no_cut) (void)set_cut(p_ptr->cut + randint(dam));
				}

				/* Take the damage */
				take_hit(dam, trap_name(t_ptr->w_idx, 1));
			}
			break;
		}

		case WG_PIT_DAGGER:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You fall into a pit of daggers!");

			if (p_ptr->ffall)
			{
				message(MSG_FFALL, 0, "You float gently to the floor of the pit.");
				message(MSG_FFALL, 0, "You carefully avoid setting off the daggers.");
			}

			else
			{
				/* activate the ordinary daggers. */
				message(MSG_TRAP, t_ptr->w_idx, "Daggers pierce you everywhere!");
				 
				/* Base damage */
				dam = damroll(2, 6);

				for (i = 0; i < randint(10) + 5; i++)
				{
					dam += damroll(3, 3);
				}

				/* cut the player. */
				if (!p_ptr->no_cut) (void)set_cut(p_ptr->cut + randint(dam));

				/* Take the damage. */
				take_hit(dam, trap_name(t_ptr->w_idx, 1));
			}

			break;
		}

		case WG_PIT_POISON:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You fall into a spiked pit!");

			if (p_ptr->ffall)
			{
				message(MSG_FFALL, 0, "You float gently to the floor of the pit.");
				message(MSG_FFALL, 0, "You carefully avoid touching the spikes.");
			}

			else
			{
				/* Base damage */
				dam = damroll(2, 6);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					message(MSG_TRAP, t_ptr->w_idx, "You are impaled on poisonous spikes!");

					dam = dam * 2;
					if (!p_ptr->no_cut) set_cut(p_ptr->cut + randint(dam));

					if (p_ptr->no_poison || resist_effect(RS_PSN))
					{
						message(MSG_RESIST, 0, "The poison does not affect you!");
					}

					else
					{
						dam = dam * 2;
						(void)set_poisoned(p_ptr->poisoned + randint(dam));
					}
				}

				/* Take the damage */
				take_hit(dam, trap_name(t_ptr->w_idx, 1));
			}

			break;
		}

		case WG_RUNE_SUMMON:
		case WG_CHEST_SUMMON:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You are enveloped in a cloud of smoke!");
			num = 2 + randint(3);
			for (i = 0; i < num; i++)
			{
				(void)summon_specific(y, x, p_ptr->depth, 0);
			}
			break;
		}

		case WG_RUNE_TELE:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You hit a teleport trap!");
			teleport_player(90);
			break;
		}

		case WG_RUNE_FORGET:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You hit an amnesia trap!");	
			if (rand_int(100) < p_ptr->skill[SK_SAV])
			{
				message(MSG_RESIST, t_ptr->w_idx, "You hang on to your memories!");
			}
			else if (lose_all_info())
			{
				message(MSG_EFFECT, 0, "Your memories fade away.");
			}
			break;
		}

		case WG_RUNE_CURSE1:
		case WG_CHEST_CURSE:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You have awoken an ancient curse!");

			if (!curse_minor())
			{
				message(MSG_TRAP, t_ptr->w_idx, "Nothing happens.");
			}
			break;
		}

		case WG_RUNE_CURSE2:
		{
			bool effect = FALSE;
			message(MSG_TRAP, t_ptr->w_idx, "You have awoken an ancient curse!");

			if (rand_int(2) == 0)
			{
				if (curse_armor()) effect = TRUE;
			}
			else 
			{
				if (curse_weapon()) effect = TRUE;
			}

			if (!effect)
			{
				message(MSG_TRAP, t_ptr->w_idx, "Nothing happens.");
			}
			break;
		}

		case WG_RUNE_SHRIEK:
		case WG_CHEST_SHRIEK:
		{
			message(MSG_TRAP, t_ptr->w_idx, "A high pitch noise suddenly arises!");
			aggravate_monsters(0);
			break;
		}
		case WG_SPOT_FIRE:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You are enveloped in flames!");
			dam = damroll(4, 6);
			fire_dam(dam, trap_name(t_ptr->w_idx, 1));
			break;
		}

		case WG_SPOT_ACID:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You are splashed with acid!");
			dam = damroll(4, 6);
			acid_dam(dam, trap_name(t_ptr->w_idx, 1));
			break;
		}

		case WG_SPOT_ELEC:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You are struck by lightning!");
			dam = damroll(4, 6);
			elec_dam(dam, trap_name(t_ptr->w_idx, 1));
			break;
		}

		case WG_CHEST_DISEN:
		case WG_SPOT_DISEN:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You feel a sudden surge of unmagic!");
			apply_disenchant();
			break;
		}

		case WG_DART_SLOW:
		{
			if (check_hit(125))
			{
				message(MSG_TRAP, t_ptr->w_idx, "A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, trap_name(t_ptr->w_idx, 1));
				(void)set_slow(p_ptr->slow + rand_int(20) + 20);
			}
			else
			{
				message(MSG_TRAP, t_ptr->w_idx, "A small dart barely misses you.");
			}
			break;
		}

		case WG_CHEST_LOSE_STR:
		case WG_DART_LOSE_STR:
		{
			if (check_hit(125))
			{
				message(MSG_TRAP, t_ptr->w_idx, "A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, trap_name(t_ptr->w_idx, 1));
				(void)do_dec_stat(A_STR, 1, FALSE, TRUE);
			}
			else
			{
				message(MSG_TRAP, t_ptr->w_idx, "A small dart barely misses you.");
			}
			break;
		}

		case WG_CHEST_LOSE_DEX:
		case WG_DART_LOSE_DEX:
		{
			if (check_hit(125))
			{
				message(MSG_TRAP, t_ptr->w_idx, "A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, trap_name(t_ptr->w_idx, 1));
				(void)do_dec_stat(A_DEX, 1, FALSE, TRUE);
			}
			else
			{
				message(MSG_TRAP, t_ptr->w_idx, "A small dart barely misses you.");
			}
			break;
		}

		case WG_CHEST_LOSE_CON:
		case WG_DART_LOSE_CON:
		{
			if (check_hit(125))
			{
				message(MSG_TRAP, t_ptr->w_idx, "A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, trap_name(t_ptr->w_idx, 1));
				(void)do_dec_stat(A_CON, 1, FALSE, TRUE);
			}
			else
			{
				message(MSG_TRAP, t_ptr->w_idx, "A small dart barely misses you.");
			}
			break;
		}

		case WG_GAS_BLIND:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You are surrounded by a black gas!");
			if (!p_ptr->no_blind)
			{
				(void)set_blind(p_ptr->blind + rand_int(50) + 25);
			}
			break;
		}

		case WG_CHEST_CONF:
		case WG_GAS_CONF:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You are surrounded by a gas of scintillating colors!");
			if (!p_ptr->no_confuse)
			{
				(void)set_confused(p_ptr->confused + rand_int(20) + 10);
			}
			break;
		}

		case WG_CHEST_POISON:
		case WG_GAS_POISON:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You are surrounded by a pungent green gas!");
			if (!p_ptr->no_poison && !resist_effect(RS_PSN))
			{
				(void)set_poisoned(p_ptr->poisoned + rand_int(20) + 10);
			}
			break;
		}

		case WG_CHEST_PARALYZE:
		case WG_GAS_SLEEP:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You are surrounded by a strange white mist!");
			if (!p_ptr->free_act)
			{
				(void)set_paralyzed(p_ptr->paralyzed + rand_int(10) + 5);
			}
			break;
		}

		case WG_CHEST_HALLUC:
		case WG_GAS_HALLUC:
		{
			message(MSG_TRAP, t_ptr->w_idx, "You are surrounded by a strange multi-hued mist!");
			if (!resist_effect(RS_CHS))
			{
				(void)set_image(p_ptr->image + rand_int(20) + 20);
			}
			break;
		}

		case WG_BLADES:
		{
			if (check_hit(175))
			{
				message(MSG_TRAP, t_ptr->w_idx, "Large blades emerge from the wall and hit you!");
				dam = damroll (1 + p_ptr->depth / 5, 6);
				take_hit(dam, trap_name(t_ptr->w_idx, 1));
			}
			else
			{
				message(MSG_TRAP, t_ptr->w_idx, "Large blades emerge from the wall, but miss you.");
			}
			break;
		}

		case WG_BLADES_SPIN:
		{
			if (check_hit(200))
			{
				message(MSG_TRAP, t_ptr->w_idx, "Spinning discs shoot out from the wall and hit you!");
				dam = damroll (1 + p_ptr->depth / 8, 6);

				/* cut the player. */
				if (!p_ptr->no_cut) (void)set_cut(p_ptr->cut + dam/2 + randint(dam/2));

				take_hit(dam, trap_name(t_ptr->w_idx, 1));
			}
			else
			{
				message(MSG_TRAP, t_ptr->w_idx, "Spinning discs shoot out from the wall, but miss you.");
			}
			break;
		}

		case WG_QUAKE:
		{
			message(MSG_TRAP, t_ptr->w_idx, "A tremor shakes the dungeon around you");
			earthquake(y, x, 10);
			break;
		}

		case WG_ROCK:
		{
			message(MSG_TRAP, t_ptr->w_idx, "A rock falls on your head.");
			dam = damroll(2,10);
				take_hit(dam, trap_name(t_ptr->w_idx, 1));

			if (!p_ptr->no_stun) (void)set_stun(p_ptr->stun + randint(10) + 10);
			break;
		}
	}

	/* Charges */
	if (t_ptr->charges)
	{
		t_ptr->charges--;

		/* Out of charges */
		if (!t_ptr->charges)
		{
			delete_trap(y, x);
		}
	}

	/* Remember the trap */
	cave_info[y][x] |= (CAVE_MARK);
}

/*
 * Handle player disarming a real trap
 */
bool do_disarm_trap(int y, int x)
{
	int i, j;
	int diff;
	cptr name = "trap";
	bool more = FALSE;

	trap_type *t_ptr = &t_list[cave_t_idx[y][x]];
	trap_widget *w_ptr = &w_info[t_ptr->w_idx];

	/* Paranoia */
	if (!in_bounds(y, x)) return (FALSE);

	/* No visible trap */
	if (!trap_disarmable(y, x))	return (FALSE);

	/* Get the "disarm" factor */
	i = p_ptr->skill[SK_DIS];

	/* Penalize some conditions */
	if (p_ptr->blind || !player_can_see_bold(p_ptr->py, p_ptr->px)) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* Extract the difficulty */
	diff = 1 + (w_ptr->disarm_factor * p_ptr->depth) / 100;

	/* Actual chance of success */
	j = i - diff;

	/* Always have a small chance of success */
	if (j < 2) j = 2;

	/* Success */
	if ((rand_int(100) < j) || !(trap_player(y,x)))
	{
		if (trap_player(y, x))
		{
			int exp = 1 + (p_ptr->depth / 2) + (diff * 2);

			/* Message */
			message_format(MSG_DISARM_SUCCEED, 0, "You have disarmed the %s.", name);

			/* Bonus exp for charges */
			if (t_ptr->charges > 1) exp *= (t_ptr->charges / 2);

			/* Reward */
			gain_exp(exp);
		}

		/* Forget the trap */
		delete_trap(y, x);
	}

	/* Failure -- Keep trying */
	else if ((i > diff) && (randint(i) > diff))
	{
		/* Failure */
		if (flush_failure) flush();

		/* Message */
		message_format(MSG_DISARM_FAIL, 0, "You failed to disarm the %s.", name);

		/* We may keep trying */
		more = TRUE;
	}

	/* Failure -- Set off the trap */
	else
	{
		/* Message */
		message_format(MSG_DISARM_FAIL, 0, "You set off the %s!", name);

		/* Hit the trap */
		hit_trap(y, x);
	}

	/* Return more */
	return (more);
}

/*
 * Leave a "glyph of warding" which prevents movement
 */
bool warding_glyph(byte type)
{
	trap_type trap_type_body;
	trap_type *t_ptr = &trap_type_body;

	/* Require empty space */
	if (cave_feat[p_ptr->py][p_ptr->px] != FEAT_FLOOR) return FALSE;

	if (cave_t_idx[p_ptr->py][p_ptr->px]) return FALSE;

	t_ptr->w_idx = type;
	t_ptr->visible = TRUE;

	/* Actually place the glyph */
	if (!trap_place(p_ptr->py, p_ptr->px, t_ptr)) return FALSE;

	/* Show the trap */
	lite_spot(p_ptr->py, p_ptr->px);

	return TRUE;
}

/*
 * Hack - Check if glyph affects monster
 */
bool mon_glyph_check(int m_idx, int y, int x)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);

	trap_type *t_ptr = &t_list[cave_t_idx[y][x]];

	/* Paranoia */
	if (!w_info[t_ptr->w_idx].flags & WGF_GLYPH) return FALSE;

	switch (t_ptr->w_idx)
	{
		case WG_GLYPH_LESSER:
		case WG_GLYPH:
		{
			/* Affects everyone */
			return TRUE;
		}
		case WG_GLYPH_HOLY:
		{
			/* Affects undead */
			if (r_ptr->flags4 & RF4_UNDEAD) return TRUE;
			
			/* Affects demons */
			if (r_ptr->flags4 & RF4_DEMON) return TRUE;

			return FALSE;
		}
	}

	/* Default */
	return FALSE;
}
