/*
 * File: trap.c
 * Purpose: Trap triggering, selection, and placement
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
#include "angband.h"

/* enum doesn't make sense here */
#define TRAP_DOOR FEAT_TRAP_HEAD + 0x00
#define TRAP_PIT FEAT_TRAP_HEAD + 0x01
#define TRAP_SPIKED_PIT FEAT_TRAP_HEAD + 0x02
#define TRAP_POISON_SPIKED_PIT FEAT_TRAP_HEAD + 0x03
#define RUNE_SUMMON FEAT_TRAP_HEAD + 0x04
#define RUNE_TELEPORT FEAT_TRAP_HEAD + 0x05
#define TRAP_FIRE FEAT_TRAP_HEAD + 0x06
#define TRAP_ACID FEAT_TRAP_HEAD + 0x07
#define DART_SLOW FEAT_TRAP_HEAD + 0x08
#define DART_STR FEAT_TRAP_HEAD + 0x09
#define DART_DEX FEAT_TRAP_HEAD + 0x0A
#define DART_CON FEAT_TRAP_HEAD + 0x0B
#define GAS_BLIND FEAT_TRAP_HEAD + 0x0C
#define GAS_CONFUSE FEAT_TRAP_HEAD + 0x0D
#define GAS_POISON FEAT_TRAP_HEAD + 0x0E
#define GAS_PARALYZE FEAT_TRAP_HEAD + 0x0F

ZAIBAND_STATIC_ASSERT(IS_TRAP(TRAP_DOOR));
ZAIBAND_STATIC_ASSERT(IS_TRAP(TRAP_PIT));
ZAIBAND_STATIC_ASSERT(IS_TRAP(TRAP_SPIKED_PIT));
ZAIBAND_STATIC_ASSERT(IS_TRAP(TRAP_POISON_SPIKED_PIT));
ZAIBAND_STATIC_ASSERT(IS_TRAP(RUNE_SUMMON));
ZAIBAND_STATIC_ASSERT(IS_TRAP(RUNE_TELEPORT));
ZAIBAND_STATIC_ASSERT(IS_TRAP(TRAP_ACID));
ZAIBAND_STATIC_ASSERT(IS_TRAP(TRAP_ACID));
ZAIBAND_STATIC_ASSERT(IS_TRAP(DART_SLOW));
ZAIBAND_STATIC_ASSERT(IS_TRAP(DART_STR));
ZAIBAND_STATIC_ASSERT(IS_TRAP(DART_DEX));
ZAIBAND_STATIC_ASSERT(IS_TRAP(DART_CON));
ZAIBAND_STATIC_ASSERT(IS_TRAP(GAS_BLIND));
ZAIBAND_STATIC_ASSERT(IS_TRAP(GAS_CONFUSE));
ZAIBAND_STATIC_ASSERT(IS_TRAP(GAS_POISON));
ZAIBAND_STATIC_ASSERT(IS_TRAP(GAS_PARALYZE));

/*
 * Determine if a trap affects the player.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match trap power against player armor.
 */
static bool check_hit(int power)
{
	return test_hit(power, p_ptr->ac + p_ptr->to_a, TRUE);
}

/*
 * Hack -- instantiate a trap
 *
 * XXX XXX XXX This routine should be redone to reflect trap "level".
 * That is, it does not make sense to have spiked pits at 50 feet.
 * Actually, it is not this routine, but the "trap instantiation"
 * code, which should also check for "trap doors" on quest levels.
 */
void pick_trap(coord g)
{
	int feat;
	u32b f[OBJECT_FLAG_STRICT_UB];

	/* Paranoia */
#ifdef NDEBUG
	if (cave_feat[g.y][g.x] != FEAT_INVIS) return;
#else
	assert(FEAT_INVIS == cave_feat[g.y][g.x]);
#endif

	/* get player flags */
	p_ptr->flags(f);

	/* Pick a trap */
	while (1)
	{
		/* pick a trap */
		feat = FEAT_TRAP_HEAD + rand_int(16);

		if ((TRAP_DOOR == feat))
			{
			if (is_quest(p_ptr->depth)) continue;		/* no trap doors on quest levels */
			if (p_ptr->depth >= MAX_DEPTH-1) continue;	/* no trap doors on the deepest level */
			};

		/* accept useless traps some of the time */
		if (one_in_(2)) break;

		/* useless darts get replaced */
		if (DART_STR == feat && (f[1] & (TR2_SUST_STR))) continue;
		if (DART_DEX == feat && (f[1] & (TR2_SUST_DEX))) continue;
		if (DART_CON == feat && (f[1] & (TR2_SUST_CON))) continue;

		/* useless traps get replaced */
		if (TRAP_FIRE == feat && (f[1] & (TR2_IM_FIRE))) continue;
		if (TRAP_ACID == feat && (f[1] & (TR2_IM_ACID))) continue;

		/* useless gases get replaced */
		if (GAS_BLIND == feat && (f[1] & (TR2_RES_BLIND))) continue;
		if (GAS_CONFUSE == feat && (f[1] & (TR2_RES_CONFU))) continue;
		if (GAS_POISON == feat && (f[1] & (TR2_RES_POIS))) continue;
		if (GAS_PARALYZE == feat && (f[2] & (TR3_FREE_ACT))) continue;

		/* Done */
		break;
	}

	/* Activate the trap */
	cave_set_feat(g.y, g.x, feat);
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
	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Require empty, clean, floor grid */
	if (!cave_naked_bold(y, x)) return;

	/* Place an invisible trap */
	cave_set_feat(y, x, FEAT_INVIS);
}

/*
 * Handle player hitting a real trap
 */
void hit_trap(coord g)
{
	int i, num, dam;

	const char* const name = "a trap";

	/* Paranoia */
	assert(is_trap(cave_feat[g.y][g.x]));

	/* Disturb the player */
	disturb(0, 0);

	/* Analyze XXX XXX XXX */
	switch (cave_feat[g.y][g.x])
	{
		case TRAP_DOOR:
		{
			msg_print("You fall through a trap door!");
			if (p_ptr->ffall)
			{
				msg_print("You float gently down to the next level.");
			}
			else
			{
				dam = NdS(2, 8);
				take_hit(dam, name);
			}

			/* New depth */
			p_ptr->depth++;

			/* Leaving */
			p_ptr->leaving = TRUE;

			break;
		}

		case TRAP_PIT:
		{
			msg_print("You fall into a pit!");
			if (p_ptr->ffall)
			{
				msg_print("You float gently to the bottom of the pit.");
			}
			else
			{
				dam = NdS(2, 6);
				take_hit(dam, name);
			}
			break;
		}

		case TRAP_SPIKED_PIT:
		{
			msg_print("You fall into a spiked pit!");

			if (p_ptr->ffall)
			{
				msg_print("You float gently to the floor of the pit.");
				msg_print("You carefully avoid touching the spikes.");
			}

			else
			{
				/* Base damage */
				dam = NdS(2, 6);

				/* Extra spike damage */
				if (one_in_(2))
				{
					msg_print("You are impaled!");

					dam *= 2;
					(void)p_ptr->inc_timed<TMD_CUT>(randint(dam));
				}

				/* Take the damage */
				take_hit(dam, name);
			}
			break;
		}

		case TRAP_POISON_SPIKED_PIT:
		{
			msg_print("You fall into a spiked pit!");

			if (p_ptr->ffall)
			{
				msg_print("You float gently to the floor of the pit.");
				msg_print("You carefully avoid touching the spikes.");
			}

			else
			{
				/* Base damage */
				dam = NdS(2, 6);

				/* Extra spike damage */
				if (one_in_(2))
				{
					msg_print("You are impaled on poisonous spikes!");

					dam *= 2;
					(void)p_ptr->inc_timed<TMD_CUT>(randint(dam));

					if (p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS])
					{
						msg_print("The poison does not affect you!");
					}
					else
					{
						dam *= 2;
						(void)p_ptr->inc_timed<TMD_POISONED>(randint(dam));
					}
				}

				/* Take the damage */
				take_hit(dam, name);
			}

			break;
		}

		case RUNE_SUMMON:
		{
			sound(MSG_SUM_MONSTER);
			msg_print("You are enveloped in a cloud of smoke!");
			cave_info[g.y][g.x] &= ~(CAVE_MARK);
			cave_set_feat(g.y, g.x, FEAT_FLOOR);
			num = 2 + randint(3);
			for (i = 0; i < num; i++)
			{
				(void)summon_specific(g, p_ptr->depth, 0);
			}
			break;
		}

		case RUNE_TELEPORT:
		{
			msg_print("You hit a teleport trap!");
			teleport_player(100);
			break;
		}

		case TRAP_FIRE:
		{
			msg_print("You are enveloped in flames!");
			dam = NdS(4, 6);
			fire_dam(dam, "a fire trap");
			break;
		}

		case TRAP_ACID:
		{
			msg_print("You are splashed with acid!");
			dam = NdS(4, 6);
			acid_dam(dam, "an acid trap");
			break;
		}

		case DART_SLOW:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = NdS(1, 4);
				take_hit(dam, name);
				(void)p_ptr->inc_timed<TMD_SLOW>(rand_int(20) + 20);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case DART_STR:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = NdS(1, 4);
				take_hit(dam, name);
				(void)do_dec_stat(A_STR);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case DART_DEX:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = NdS(1, 4);
				take_hit(dam, name);
				(void)do_dec_stat(A_DEX);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case DART_CON:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = NdS(1, 4);
				take_hit(dam, name);
				(void)do_dec_stat(A_CON);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case GAS_BLIND:
		{
			msg_print("You are surrounded by a black gas!");
			if (!p_ptr->resist_blind)
			{
				(void)p_ptr->inc_timed<TMD_BLIND>(rand_int(50) + 25);
			}
			break;
		}

		case GAS_CONFUSE:
		{
			msg_print("You are surrounded by a gas of scintillating colors!");
			if (!p_ptr->resist_confu)
			{
				(void)p_ptr->inc_timed<TMD_CONFUSED>(rand_int(20) + 10);
			}
			break;
		}

		case GAS_POISON:
		{
			msg_print("You are surrounded by a pungent green gas!");
			if (!p_ptr->resist_pois && !p_ptr->timed[TMD_OPP_POIS])
			{
				(void)p_ptr->inc_timed<TMD_POISONED>(rand_int(20) + 10);
			}
			break;
		}

		case GAS_PARALYZE:
		{
			msg_print("You are surrounded by a strange white mist!");
			if (!p_ptr->free_act)
			{
				(void)p_ptr->inc_timed<TMD_PARALYZED>(rand_int(10) + 5);
			}
			break;
		}
#ifndef NDEBUG
		default:
		{	/* shouldn't get here */
			assert(0);
		}
#endif
	}
}

