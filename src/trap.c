/* File: trap.c */

/* Purpose: Trap code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Hack -- instantiate a trap
 *
 * XXX XXX XXX This routine should be redone to reflect trap "level".
 * That is, it does not make sense to have spiked pits at 50 feet.
 * Actually, it is not this routine, but the "trap instantiation"
 * code, which should also check for "trap doors" on quest levels.
 */
void pick_trap(int y, int x)
{
	int feat;

	cave_type *c_ptr = &cave[y][x];

	/* Paranoia */
	if (c_ptr->feat != FEAT_INVIS) return;

	/* Pick a trap */
	while (1)
	{
		/* Hack -- pick a trap */
		feat = FEAT_TRAP_HEAD + rand_int(16);

		/* Hack -- no trap doors on special levels */
		if ((feat == FEAT_TRAP_HEAD + 0x00) && quest_number(dun_level))
			continue;

		/* Hack -- no trap doors on the deepest level */
		if ((feat == FEAT_TRAP_HEAD + 0x00) && (dun_level >= MAX_DEPTH-1))
			continue;

		/* Done */
		break;
	}

	/* Activate the trap */
	cave_set_feat(y, x, feat);
}


/*
 * Places a random trap at the given location.
 *
 * The location must be a legal, naked, floor grid.
 *
 * Note that all traps start out as "invisible" and "untyped", and then
 * when they are "discovered" (by detecting them or setting them off),
 * the trap is "instantiated" as a visible, "typed", trap.
 */
void place_trap(int y, int x)
{
	/* Paranoia -- verify location */
	if (!in_bounds(y, x)) return;

	/* Require empty, clean, floor grid */
	if (!cave_naked_bold(y, x)) return;

	/* Place an invisible trap */
	cave_set_feat(y, x, FEAT_INVIS);
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

	/* Paranoia -- No power */
	if (power <= 0) return (FALSE);

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Power competes against Armor */
	if (randint(power) > ((ac * 3) / 4)) return (TRUE);

	/* Assume miss */
	return (FALSE);
}



/*
 * Handle player hitting a real trap
 */
void hit_trap(void)
{
	int i, num, dam;

	cave_type *c_ptr;

	cptr name = "a trap";


	/* Disturb the player */
	disturb(0, 0);

	/* Get the cave grid */
	c_ptr = &cave[py][px];

	/* Analyze XXX XXX XXX */
	switch (c_ptr->feat)
	{
		case FEAT_TRAP_HEAD + 0x00:
		{
			if (p_ptr->ffall)
			{
				msg_print("You fly over a trap door.");
			}
			else
			{
				msg_print("You have fallen through a trap door!");
				sound(SOUND_FALL);
				dam = damroll(2, 8);
				name = "a trap door";
				take_hit(dam, name);

				/* Still alive and autosave enabled */
				if (autosave_l && (p_ptr->chp >= 0))
					do_cmd_save_game(TRUE);

				dun_level++;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x01:
		{
			if (p_ptr->ffall)
			{
				msg_print("You fly over a pit trap.");
			}
			else
			{
				msg_print("You have fallen into a pit!");
				dam = damroll(2, 6);
				name = "a pit trap";
				take_hit(dam, name);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x02:
		{
			if (p_ptr->ffall)
			{
				msg_print("You fly over a spiked pit.");
			}
			else
			{
				msg_print("You fall into a spiked pit!");

				/* Base damage */
				name = "a pit trap";
				dam = damroll(2, 6);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled!");

					name = "a spiked pit";
					dam = dam * 2;
					(void)set_cut(p_ptr->cut + randint(dam));
				}

				/* Take the damage */
				take_hit(dam, name);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x03:
		{
			if (p_ptr->ffall)
			{
				msg_print("You fly over a spiked pit.");
			}
			else
			{
				msg_print("You fall into a spiked pit!");

				/* Base damage */
				dam = damroll(2, 6);

				name = "a pit trap";

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled on poisonous spikes!");

					name = "a spiked pit";

					dam = dam * 2;
					(void)set_cut(p_ptr->cut + randint(dam));

					if (p_ptr->resist_pois || p_ptr->oppose_pois)
					{
						msg_print("The poison does not affect you!");
					}

					else
					{
						dam = dam * 2;
						(void)set_poisoned(p_ptr->poisoned + randint(dam));
					}
				}

				/* Take the damage */
				take_hit(dam, name);
			}

			break;
		}

		case FEAT_TRAP_HEAD + 0x04:
		{
			msg_print("There is a flash of shimmering light!");
			c_ptr->info &= ~(CAVE_MARK);
			cave_set_feat(py, px, FEAT_FLOOR);
			num = 2 + randint(3);
			for (i = 0; i < num; i++)
			{
				(void)summon_specific(py, px, dun_level, 0, TRUE, FALSE, FALSE);
			}

			if (dun_level > randint(100)) /* No nasty effect for low levels */
			{
				bool stop_ty = FALSE; /* Prfnoff */

				do
				{
					stop_ty = activate_ty_curse(stop_ty);
				}
				while (randint(6) == 1);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x05:
		{
			msg_print("You hit a teleport trap!");
			teleport_player(100);
			break;
		}

		case FEAT_TRAP_HEAD + 0x06:
		{
			msg_print("You are enveloped in flames!");
			dam = damroll(4, 6);
			fire_dam(dam, "a fire trap");
			break;
		}

		case FEAT_TRAP_HEAD + 0x07:
		{
			msg_print("You are splashed with acid!");
			dam = damroll(4, 6);
			acid_dam(dam, "an acid trap");
			break;
		}

		case FEAT_TRAP_HEAD + 0x08:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name);
				(void)set_slow(p_ptr->slow + rand_int(20) + 20);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x09:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, "a dart trap");
				(void)do_dec_stat(A_STR);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0A:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, "a dart trap");
				(void)do_dec_stat(A_DEX);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0B:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, "a dart trap");
				(void)do_dec_stat(A_CON);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0C:
		{
			msg_print("A black gas surrounds you!");
			if (!p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + rand_int(50) + 25);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0D:
		{
			msg_print("A gas of scintillating colors surrounds you!");
			if (!p_ptr->resist_conf)
			{
				(void)set_confused(p_ptr->confused + rand_int(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0E:
		{
			msg_print("A pungent green gas surrounds you!");
			if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
			{
				(void)set_poisoned(p_ptr->poisoned + rand_int(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0F:
		{
			msg_print("A strange white mist surrounds you!");
			if (!p_ptr->free_act)
			{
				(void)set_paralyzed(p_ptr->paralyzed + rand_int(10) + 5);
			}
			break;
		}
	}
}
