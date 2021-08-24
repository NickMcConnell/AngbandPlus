/**
 * \file game-world.c
 * \brief Game core management of the game world
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
#include "cmds.h"
#include "effects.h"
#include "game-world.h"
#include "generate.h"
#include "init.h"
#include "mon-make.h"
#include "mon-move.h"
#include "mon-util.h"
#include "obj-fault.h"
#include "obj-desc.h"
#include "obj-gear.h"
#include "obj-knowledge.h"
#include "obj-make.h"
#include "obj-pile.h"
#include "obj-tval.h"
#include "obj-util.h"
#include "player-ability.h"
#include "player-calcs.h"
#include "player-timed.h"
#include "player-util.h"
#include "source.h"
#include "store.h"
#include "target.h"
#include "trap.h"
#include "world.h"
#include "z-queue.h"

u16b daycount = 0;
u32b seed_randart;		/* Hack -- consistent random artifacts */
u32b seed_flavor;		/* Hack -- consistent object colors */
s32b turn;				/* Current game turn */
bool character_generated;	/* The character exists */
bool character_dungeon;		/* The character has a dungeon */
struct level *world;

/**
 * This table allows quick conversion from "speed" to "energy"
 * The basic function WAS ((S>=110) ? (S-110) : (100 / (120-S)))
 * Note that table access is *much* quicker than computation.
 *
 * Note that the table has been changed at high speeds.  From
 * "Slow (-40)" to "Fast (+30)" is pretty much unchanged, but
 * at speeds above "Fast (+30)", one approaches an asymptotic
 * effective limit of 50 energy per turn.  This means that it
 * is relatively easy to reach "Fast (+30)" and get about 40
 * energy per turn, but then speed becomes very "expensive",
 * and you must get all the way to "Fast (+50)" to reach the
 * point of getting 45 energy per turn.  After that point,
 * furthur increases in speed are more or less pointless,
 * except to balance out heavy inventory.
 *
 * Note that currently the fastest monster is "Fast (+30)".
 */
const byte extract_energy[200] =
{
	/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	/* S-50 */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	/* S-40 */     2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
	/* S-30 */     2,  2,  2,  2,  2,  2,  2,  3,  3,  3,
	/* S-20 */     3,  3,  3,  3,  3,  4,  4,  4,  4,  4,
	/* S-10 */     5,  5,  5,  5,  6,  6,  7,  7,  8,  9,
	/* Norm */    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
	/* F+10 */    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
	/* F+20 */    30, 31, 32, 33, 34, 35, 36, 36, 37, 37,
	/* F+30 */    38, 38, 39, 39, 40, 40, 40, 41, 41, 41,
	/* F+40 */    42, 42, 42, 43, 43, 43, 44, 44, 44, 44,
	/* F+50 */    45, 45, 45, 45, 45, 46, 46, 46, 46, 46,
	/* F+60 */    47, 47, 47, 47, 47, 48, 48, 48, 48, 48,
	/* F+70 */    49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
	/* Fast */    49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
};

/**
 * Find a level by its name
 */
struct level *level_by_name(char *name)
{
	struct level *lev = world;
	while (lev) {
		if (streq(lev->name, name)) {
			break;
		}
		lev = lev->next;
	}
	return lev;
}

/**
 * Find a level by its depth
 * If it is the surface, or it matches your current town
 */
struct level *level_by_depth(int depth)
{
	assert(player);
	assert(player->town);
	struct level *lev = world;
	while (lev) {
		if (lev->depth == depth) {
			if (lev->depth == 0)
				break;
			if ((player->town->downto) && (!streq(player->town->downto, lev->name)))
				break;
		}
		lev = lev->next;
	}
	return lev;
}

/**
 * Say whether it's daytime or not
 */
bool is_daytime(void)
{
	if ((turn % (10L * z_info->day_length)) < ((10L * z_info->day_length) / 2)) 
		return true;

	return false;
}

/**
 * Return, in a static buffer, a time (turns) given as HH:MM, 24-hour clock.
 */
static char *do_format_time(int turns, const char *fmt)
{
	static char buf[8];
	int hh, mm;
	/* There are 24*60 minutes, and 10L * z_info->day_length turns, in a day
	 * and the first half of a day is daytime.
	 */
	turns %= (10L * z_info->day_length);
	turns *= (24 * 60);
	turns /= (10L * z_info->day_length);

	/* Convert minutes to hours and minutes */
	mm = turns % 60;
	hh = turns / 60;
	sprintf(buf, "%02d:%02d", hh, mm);
	return buf;
}

/**
 * Return, in a static buffer, the time of day (turns) given as HH:MM, 24-hour clock.
 * The offset is so that the sun rises at 6am, not midnight.
 */
char *format_time(int turns)
{
	return do_format_time(turns + ((10L * z_info->day_length) / 4), "%02d:%02d");
}

/**
 * Return, in a static buffer, a duration (turns) given as HH:MM, 24-hour clock.
 * Unlike times of day, there is no offset and leading zeroes are suppressed.
 */
char *format_duration(int turns)
{
	return do_format_time(turns, "%d:%02d");
}

/**
 * The amount of energy gained in a turn by a player or monster
 */
int turn_energy(int speed)
{
	return extract_energy[speed] * z_info->move_energy / 100;
}

/**
 * If player has inscribed the object with "!!", let him know when it's
 * recharged. -LM-
 * Also inform player when first item of a stack has recharged. -HK-
 * Notify all recharges w/o inscription if notify_recharge option set -WP-
 */
static void recharged_notice(const struct object *obj, bool all)
{
	char o_name[120];

	const char *s;

	bool notify = false;

	if (OPT(player, notify_recharge)) {
		notify = true;
	} else if (obj->note) {
		/* Find a '!' */
		s = strchr(quark_str(obj->note), '!');

		/* Process notification request */
		while (s) {
			/* Find another '!' */
			if (s[1] == '!') {
				notify = true;
				break;
			}

			/* Keep looking for '!'s */
			s = strchr(s + 1, '!');
		}
	}

	if (!notify) return;

	/* Describe (briefly) */
	object_desc(o_name, sizeof(o_name), obj, ODESC_BASE);

	/* Disturb the player */
	disturb(player);

	/* Notify the player */
	if (obj->number > 1) {
		if (all) msg("Your %s have recharged.", o_name);
		else msg("One of your %s has recharged.", o_name);
	} else if (obj->artifact)
		msg("The %s has recharged.", o_name);
	else
		msg("Your %s has recharged.", o_name);
}


/**
 * Recharge activatable objects in the player's equipment
 * and rods in the inventory and on the ground.
 */
static void recharge_objects(void)
{
	int i;
	bool discharged_stack;
	struct object *obj;

	/* Recharge carried gear */
	for (obj = player->gear; obj; obj = obj->next) {
		/* Skip non-objects */
		assert(obj->kind);

		/* Recharge equipment */
		if (object_is_equipped(player->body, obj)) {
			/* Recharge activatable objects */
			if (recharge_timeout(obj)) {
				/* Message if an item recharged */
				recharged_notice(obj, true);

				/* Window stuff */
				player->upkeep->redraw |= (PR_EQUIP);
			}
		} else {
			/* Recharge the inventory */
			discharged_stack =
				(number_charging(obj) == obj->number) ? true : false;
			/* Recharge items, and update if any items are recharged */

			if (tval_can_have_timeout(obj) && recharge_timeout(obj)) {
				/* Entire stack is recharged */
				if (obj->timeout == 0)
					recharged_notice(obj, true);

				/* Previously exhausted stack has acquired a charge */
				else if (discharged_stack)
					recharged_notice(obj, false);

				/* Combine pack */
				player->upkeep->notice |= (PN_COMBINE);

				/* Redraw stuff */
				player->upkeep->redraw |= (PR_INVEN);
			}
		}
	}

	/* Recharge other level objects */
	for (i = 1; i < cave->obj_max; i++) {
		obj = cave->objects[i];
		if (!obj) continue;

		/* Recharge rods */
		if (tval_can_have_timeout(obj))
			recharge_timeout(obj);
	}
}


/**
 * Play an ambient sound dependent on dungeon level, and day or night in town
 */
void play_ambient_sound(void)
{
	if (player->depth == 0) {
		if (is_daytime())
			sound(MSG_AMBIENT_DAY);
		else 
			sound(MSG_AMBIENT_NITE);
	} else if (player->depth <= 20) {
		sound(MSG_AMBIENT_DNG1);
	} else if (player->depth <= 40) {
		sound(MSG_AMBIENT_DNG2);
	} else if (player->depth <= 60) {
		sound(MSG_AMBIENT_DNG3);
	} else if (player->depth <= 80) {
		sound(MSG_AMBIENT_DNG4);
	} else {
		sound(MSG_AMBIENT_DNG5);
	}
}

/**
 * Helper for process_world -- decrement player->timed[], fault effect and technique cooldown fields
 */
static void decrease_timeouts(void)
{
	int adjust = (adj_con_fix[player->state.stat_ind[STAT_CON]] + 1);
	int i;

	/* Most timed effects decrement by 1 */
	for (i = 0; i < TMD_MAX; i++) {
		int decr = 1;
		if (!player->timed[i])
			continue;

		/* If an unwanted effect and below 0, skip */
		if (player->chp < 0)
			if (timed_effects[i].flag_general & PG_NASTY)
				continue;

		/* Special cases */
		switch (i) {
            case TMD_FOOD:
            {
                /* Handled separately */
                decr = 0;
                break;
            }

			case TMD_CUT:
			{
				/* Check for truly "mortal" wound */
				if (player_timed_grade_eq(player, i, "Mortal Wound")) {
					decr = 0;
				} else {
					decr = adjust;
				}

				/* Rock players just maintain */
				if (player_has(player, PF_ROCK)) {
					decr = 0;
				}

				break;
			}

			case TMD_POISONED:
			case TMD_STUN:
			{
				decr = adjust;
				break;
			}

			case TMD_COMMAND:
			{
				struct monster *mon = get_commanded_monster();
				if (!los(cave, player->grid, mon->grid)) {
					/* Out of sight is out of mind */
					mon_clear_timed(mon, MON_TMD_COMMAND, MON_TMD_FLG_NOTIFY);
					player_clear_timed(player, TMD_COMMAND, true);
				} else {
					/* Keep monster timer aligned */
					mon_dec_timed(mon, MON_TMD_COMMAND, decr, 0);
				}
				break;
			}
		}
		/* Decrement the effect */
		player_dec_timed(player, i, decr, false);
	}

	/* Fault effects always decrement by 1 */
	for (i = 0; i < player->body.count; i++) {
		struct fault_data *fault = NULL;
		if (player->body.slots[i].obj == NULL) {
			continue;
		}
		fault = player->body.slots[i].obj->faults;
		if (fault) {
			int j;
			for (j = 0; j < z_info->fault_max; j++) {
				if (fault[j].power) {
					fault[j].timeout--;
					if (!fault[j].timeout) {
						struct fault *c = &faults[j];
						if (do_fault_effect(j, player->body.slots[i].obj)) {
							player_learn_fault(player, c);
						}
						fault[j].timeout = randcalc(c->obj->time, 0, RANDOMISE);
					}
				}
			}
		}
	}

	/* Check for abilities with random activation */
	for(int i=0;i<PF_MAX;i++) {
		if (ability[i] && player_has(player, i) && ability[i]->effect_randomly && one_in_(ability[i]->effect_randomly)) {
			bool ident;
			effect_do(ability[i]->effect, source_player(), NULL, &ident, true, 0, 0, 0, NULL, 0);
		}
	}

	/* Technique cooldowns */
	if (player->cooldown) {
		for(int i=0;i<total_spells;i++) {
			if (player->cooldown[i] > 0)
				player->cooldown[i]--;
		}
	}

	return;
}


/**
 * Every turn, the character makes enough noise that nearby monsters can use
 * it to home in.
 *
 * This function actually just computes distance from the player; this is
 * used in combination with the player's stealth value to determine what
 * monsters can hear.  We mark the player's grid with 0, then fill in the noise
 * field of every grid that the player can reach with that "noise"
 * (actally distance) plus the number of steps needed to reach that grid
 * - so higher values mean further from the player.
 *
 * Monsters use this information by moving to adjacent grids with lower noise
 * values, thereby homing in on the player even though twisty tunnels and
 * mazes.  Monsters have a hearing value, which is the largest sound value
 * they can detect.
 */
static void make_noise(struct player *p)
{
	struct loc next = p->grid;
	int y, x, d;
	int noise = 0;
	int noise_increment = p->timed[TMD_COVERTRACKS] ? 4 : 1;
    struct queue *queue = q_new(cave->height * cave->width);
	struct loc decoy = cave_find_decoy(cave);

	/* Set all the grids to silence */
	for (y = 1; y < cave->height - 1; y++) {
		for (x = 1; x < cave->width - 1; x++) {
			cave->noise.grids[y][x] = 0;
		}
	}

	/* If there's a decoy, use that instead of the player */
	if (!loc_is_zero(decoy)) {
		next = decoy;
	}

	/* Player makes noise */
	cave->noise.grids[next.y][next.x] = noise;
	q_push_int(queue, grid_to_i(next, cave->width));
	noise += noise_increment;

	/* Propagate noise */
	while (q_len(queue) > 0) {
		/* Get the next grid */
		i_to_grid(q_pop_int(queue), cave->width, &next);

		/* If we've reached the current noise level, put it back and step */
		if (cave->noise.grids[next.y][next.x] == noise) {
			q_push_int(queue, grid_to_i(next, cave->width));
			noise += noise_increment;
			continue;
		}

		/* Assign noise to the children and enqueue them */
		for (d = 0; d < 8; d++)	{
			/* Child location */
			struct loc grid = loc_sum(next, ddgrid_ddd[d]);

			if (!square_in_bounds(cave, grid)) continue;

			/* Ignore features that don't transmit sound */
			if (square_isnoflow(cave, grid)) continue;

			/* Skip grids that already have noise */
			if (cave->noise.grids[grid.y][grid.x] != 0) continue;

			/* Skip the player grid */
			if (loc_eq(p->grid, grid)) continue;

			/* Save the noise */
			cave->noise.grids[grid.y][grid.x] = noise;

			/* Enqueue that entry */
			q_push_int(queue, grid_to_i(grid, cave->width));
		}
	}

	q_free(queue);
}

/**
 * Characters leave scent trails for perceptive monsters to track.
 *
 * Scent is rather more limited than sound.  Many creatures cannot use
 * it at all, it doesn't extend very far outwards from the character's
 * current position, and monsters can use it to home in the character,
 * but not to run away.
 *
 * Scent is valued according to age.  When a character takes his turn,
 * scent is aged by one, and new scent is laid down.  Monsters have a smell
 * value which indicates the oldest scent they can detect.  Grids where the
 * player has never been will have scent 0.  The player's grid will also have
 * scent 0, but this is OK as no monster will ever be smelling it.
 */
static void update_scent(void)
{
	int y, x;
	int scent_strength[5][5] = {
		{2, 2, 2, 2, 2},
		{2, 1, 1, 1, 2},
		{2, 1, 0, 1, 2},
		{2, 1, 1, 1, 2},
		{2, 2, 2, 2, 2},
	};

	/* Update scent for all grids */
	for (y = 1; y < cave->height - 1; y++) {
		for (x = 1; x < cave->width - 1; x++) {
			if (cave->scent.grids[y][x] > 0) {
				cave->scent.grids[y][x]++;
			}
		}
	}

	/* Scentless player */
	if (player->timed[TMD_COVERTRACKS]) return;

	/* Lay down new scent around the player */
	for (y = 0; y < 5; y++) {
		for (x = 0; x < 5; x++) {
			struct loc scent;
			int new_scent = scent_strength[y][x];
			int d;
			bool add_scent = false;

			/* Initialize */
			scent.y = y + player->grid.y - 2;
			scent.x = x + player->grid.x - 2;

			/* Ignore invalid or non-scent-carrying grids */
			if (!square_in_bounds(cave, scent)) continue;
			if (square_isnoscent(cave, scent)) continue;

			/* Check scent is spreading on floors, not going through walls */
			for (d = 0; d < 8; d++)	{
				struct loc adj = loc_sum(scent, ddgrid_ddd[d]);

				if (!square_in_bounds(cave, adj)) {
					continue;
				}

				/* Player grid is always valid */
				if (x == 2 && y == 2) {
					add_scent = true;
				}

				/* Adjacent to a closer grid, so valid */
				if (cave->scent.grids[adj.y][adj.x] == new_scent - 1) {
					add_scent = true;
				}
			}

			/* Not valid */
			if (!add_scent) {
				continue;
			}

			/* Mark the scent */
			cave->scent.grids[scent.y][scent.x] = new_scent;
		}
	}
}

/**
 * Handle things that need updating once every 10 game turns
 */
void process_world(struct chunk *c)
{
	struct player *p = player;
	int i, y, x;

	/* Compact the monster list if we're approaching the limit */
	if (cave_monster_count(c) + 32 > z_info->level_monster_max)
		compact_monsters(64);

	/* Too many holes in the monster list - compress */
	if (cave_monster_count(c) + 32 < cave_monster_max(c))
		compact_monsters(0);

	/*** Check the Time ***/

	/* Play an ambient sound at regular intervals. */
	if (!(turn % ((10L * z_info->day_length) / 4)))
		play_ambient_sound();

	/* Handle stores and sunshine */
	if (!player->depth) {
		/* Daybreak/Nightfall in town */
		if (!(turn % ((10L * z_info->day_length) / 2))) {
			/* Check for dawn */
			bool dawn = (!(turn % (10L * z_info->day_length)));

			if (dawn) {
				/* Day breaks */
				msg("The sun has risen.");
			} else {
				/* Night falls */
				msg("The sun has fallen.");
			}

			/* Illuminate */
			cave_illuminate(c, dawn);
		}
	} else {
		/* Update the stores once a day (while in the dungeon).
		   The changes are not actually made until return to town,
		   to avoid giving details away in the knowledge menu. */
		if (!(turn % (10L * z_info->store_turns))) daycount++;
	}

	/* Check for light change */
	if (player_has(player, PF_UNLIGHT)) {
		player->upkeep->update |= PU_BONUS;
	}

	/* Check for creature generation */
	if (one_in_(z_info->alloc_monster_chance))
		(void)pick_and_place_distant_monster(c, player, z_info->max_sight + 5,
											 true, player->depth);

	/*** Abilities and mutations */

	/* Occasionally puke. CON helps reduce the chance, and it won't happen if you are paralyzed to ensure it doesn't
	 * ever result in more than one turn of paralysis. It also won't happen if you are already weak with hunger.
	 */
	if (player_has(p, PF_PUKING) && one_in_((p->state.stat_ind[STAT_CON] + 3) * 100) &&
		(p->timed[TMD_PARALYZED] == 0) && (p->timed[TMD_FOOD] > PY_FOOD_HUNGRY)) {
		msg("You throw up!");
		p->timed[TMD_PARALYZED] = 2;	// as it will be reduced by 1 later in process_world
		p->timed[TMD_FOOD] = PY_FOOD_WEAK;
	}

	/* Occasionally pick a slot. If it contains a steel item, magnetize it */
	if (player_of_has(player, OF_MAGNETIC)) {
		if (one_in_(50)) {
			int slot = randint0(p->body.count);
			struct object *obj = p->body.slots[slot].obj;
			if (obj) {
				if (obj->kind->material == MAT_STEEL) {
					if (!of_has(obj->flags, OF_STICKY)) {
						char oname[256];
						object_desc(oname, sizeof(oname), obj, ODESC_BASE);
						msg("Your %s jerks.", oname);
						of_on(obj->flags, OF_STICKY);
					}
				}
			}
		}
	}

	/*** Damage (or healing) over Time ***/

	/* Take damage from radiation */
	if (player->timed[TMD_RAD]) {
		take_hit(player, 1, "radiation");
		if (randint0(5000 + p->timed[TMD_RAD]) < p->timed[TMD_RAD]) {
			int reduce = 0;
			int maxreduce = 0;
			/* So is this message, FIXME */
			msg("Your radiation sickness is causing trouble...");

			/* Do nasty things */
			switch(randint0(10)) {
				case 9:
				/* Mutate */
				if (mutate()) {
					reduce = 5;
					maxreduce = 400;
					break;
				}
				/* fall through */
				case 0:
				case 1:
				case 2:
				/* Scramble stats */
				player_inc_timed(p, TMD_SCRAMBLE, rand_range(10, 10+(p->timed[TMD_RAD] / 4)), true, true);
				reduce = 2;
				maxreduce = 50;
				break;
				case 3:
				/* If it's powerful, drain CON or STR permanently */
				player_stat_dec(p, randint0(2) ? STAT_CON : STAT_STR, (randint0(1000 + p->timed[TMD_RAD]) < (p->timed[TMD_RAD] - 200)));
				reduce = 4;
				maxreduce = 250;
				/* fall through */
				case 4:
				case 5:
				/* Drain a stat or two */
				player_stat_dec(p, randint0(STAT_MAX), false);
				reduce = 3;
				maxreduce = 125;
				break;
				case 6:
				/* Rarely drain exp permanently */
				if (randint0(p->timed[TMD_RAD]) > 500) {
					player_exp_lose(p, ((double)p->max_exp * (double)(randint1(MIN(100, (p->timed[TMD_RAD] / 10))))) / 10000.0, true);
					reduce = 5;
					maxreduce = 400;
					break;
				}
				/* fall through */
				case 7:
				case 8:
				/* Drain exp */
				player_exp_lose(p, ((double)p->max_exp * (double)(randint1(MIN(100, (p->timed[TMD_RAD] / 10))))) / 1000.0, false);
				reduce = 3;
				maxreduce = 125;
				break;
			}

			/* Be kind, rewind your radiation sickness to limit the amount of nasty you
			 * get from one episode. Don't reduce it below 1, and do proportionally more
			 * for milder cases (where the divisor has more effect than the subtraction)
			 */
			if (reduce) {
				int newrad1 = (p->timed[TMD_RAD] / reduce) + 1;
				int newrad2 = p->timed[TMD_RAD] - maxreduce;
				p->timed[TMD_RAD] = MAX(newrad1, newrad2);
			}
		}
	}

	/* Giants return to normal form after a while.
	 * How long is unpredictable, but strongly dependent on HP.
	 **/
	if (streq(p->shape->name, "giant")) {
		int turns;
		if (player->chp == player->mhp)
			turns = 256;
		else {
			/* Scale nonlinearly.
			 * This reduces to 1/4 of full scale (256) at 1/2 HP,
			 * to 1/16 at 1/4 HP input etc.
			 */
			double chp = MAX(0, player->chp) * 16; /* sqrt (256) */
			double mhp = player->mhp;
			double turns_s = chp / mhp;
			turns = turns_s * turns_s;
		}
		if (turns < 5)
			turns = 5;
		if (!randint0(turns)) {
			/* Failed check: return to normal form */
			player_resume_normal_shape(player);
		}
	}

	/* Take damage from poison */
	if (player->timed[TMD_POISONED])
		take_hit(player, 1, "poison");

	/* Damage from being below 0 HP */
	if (player->chp < 0) {
		take_hit(player, 1, NULL);
	}

	/* Take damage from cuts, worse from serious cuts */
	if (player->timed[TMD_CUT]) {
		if (player_has(player, PF_ROCK)) {
			/* Rock players just maintain */
			i = 0;
		} else if (player_timed_grade_eq(player, TMD_CUT, "Mortal Wound") ||
				   player_timed_grade_eq(player, TMD_CUT, "Deep Gash")) {
			i = 3;
		} else if (player_timed_grade_eq(player, TMD_CUT, "Severe Cut")) {
			i = 2;
		} else {
			i = 1;
		}

		/* Take damage */
		take_hit(player, i, "a fatal wound");
	}

	/* Side effects of diminishing bloodlust */
	if (player->timed[TMD_BLOODLUST]) {
		player_over_exert(player, PY_EXERT_HP | PY_EXERT_CUT | PY_EXERT_SLOW,
						  MAX(0, 10 - player->timed[TMD_BLOODLUST]),
						  player->chp / 10);
	}

	/* Timed healing */
	if (player->chp >= 0) {
		if (player->timed[TMD_HEAL]) {
			bool ident = false;
			effect_simple(EF_HEAL_HP, source_player(), "30", 0, 0, 0, 0, 0, &ident);
		}
	}

	/* Effects of Black Breath */
	if (player->timed[TMD_BLACKBREATH]) {
		if (one_in_(2)) {
			msg("The Black Breath sickens you.");
			player_stat_dec(player, STAT_CON, false);
		}
		if (one_in_(2)) {
			msg("The Black Breath saps your strength.");
			player_stat_dec(player, STAT_STR, false);
		}
		if (one_in_(2)) {
			/* Life draining */
			int drain = 100 + (player->exp / 100) * z_info->life_drain_percent;
			msg("The Black Breath dims your life force.");
			player_exp_lose(player, drain, false);
		}
	}

	/*** Check the Food, and Regenerate ***/

	/* Digest */
	if (!player_timed_grade_eq(player, TMD_FOOD, "Full")) {
		/* Digest normally */
		if (!(turn % 100)) {
			/* Basic digestion rate based on speed */
			i = turn_energy(player->state.speed);

			/* Adjust for food value */
			i = (i * 100) / z_info->food_value;

			/* Regeneration takes more food */
			if (player_of_has(player, OF_REGEN)) i *= 2;

			/* Slow digestion takes less food */
			if (player_of_has(player, OF_SLOW_DIGEST)) i /= 2;

			/* Foraging helps if you are hungry - and if you are moving, occasionally you find food. */
			if (player_has(player, PF_FORAGING)) {
				if ((player->timed[TMD_FOOD] < PY_FOOD_HUNGRY)) {
					i /= 2;
					if (!player_is_resting(player)) {
						int chance = player->timed[TMD_FOOD];
						if (chance < PY_FOOD_FAINT)
							chance = PY_FOOD_FAINT;
						chance *= 300;
						chance /= z_info->food_value;
						if (one_in_(chance)) {
							/* No mushrooms growing in lava, water */
							if (square_isprojectable(cave, player->grid) &&
								(!square_isfiery(cave, player->grid)) &&
								(!square_iswater(cave, player->grid))) {
								/* Food item or mushie, sometimes high level.
								 * You are more likely to get food items rather than shrooms - and more likely
								 * to get low level ones - when very low food
								 **/
								bool shroom = (randint0(PY_FOOD_HUNGRY * 3) < player->timed[TMD_FOOD]);
								if (square_isradioactive(cave, player->grid))
									shroom = true;
								bool hilevel = (randint0(PY_FOOD_HUNGRY * 2) < player->timed[TMD_FOOD]);
								int level = player->depth;
								if (hilevel) {
									level += player->lev + 25;
								}
								if (level > 100)
									level = 100;
								const char *fmsg;
								
								struct object *food = make_object(cave, level, false, false, false, NULL, shroom? TV_MUSHROOM : TV_FOOD);
								if (food) {
									if (shroom)
										fmsg = "You spot a mushroom growing near your feet.";
									else
										fmsg = "You notice a cache of food hidden under a rock.";
									msg(fmsg);
									drop_near(cave, &food, 0, player->grid, false, true);
								}
							}
						}
					}
				}
			}

			/* Minimal digestion */
			if (i < 1) i = 1;

			/* Digest some food */
			player_dec_timed(player, TMD_FOOD, i, false);
		}

		/* Fast metabolism */
		if (player->timed[TMD_HEAL]) {
			player_dec_timed(player, TMD_FOOD, 8 * z_info->food_value, false);
			if (player->timed[TMD_FOOD] < PY_FOOD_HUNGRY) {
				player_set_timed(player, TMD_HEAL, 0, true);
			}
		}
	} else {
		/* Digest quickly when gorged */
		player_dec_timed(player, TMD_FOOD, 5000 / z_info->food_value, false);
		player->upkeep->update |= PU_BONUS;
	}

	/* Faint or starving */
	if (player_timed_grade_eq(player, TMD_FOOD, "Faint")) {
		/* Faint occasionally */
		if (!player->timed[TMD_PARALYZED] && one_in_(10)) {
			if (player_has(player, PF_FORAGING)) {
				/* Foraging helps you ignore it, but you should still get a warning.
				 * Not too often though as it gets annoying.
				 **/
				if (one_in_(10))
					msg("You momentarily feel faint from the lack of food.");
			} else {
				/* Message */
				msg("You faint from the lack of food.");
				disturb(player);

				/* Faint (bypass free action) */
				(void)player_inc_timed(player, TMD_PARALYZED, 1 + randint0(5),
									   true, false);
			}
		}
	} else if (player_timed_grade_eq(player, TMD_FOOD, "Starving")) {
		/* Calculate damage */
		i = (PY_FOOD_STARVE - player->timed[TMD_FOOD]) / 10;

		/* Take damage */
		take_hit(player, i, "starvation");
	}

	/* Regenerate Hit Points if needed */
	if ((player->chp < player->mhp) && (player->chp >= 0))
		player_regen_hp(player);

	/* Timeout various things */
	decrease_timeouts();

	/* Process light */
	player_update_light(player);

	/* Update noise and scent (not if resting) */
	if (!player_is_resting(player)) {
		make_noise(player);
		update_scent();
	}


	/*** Process Inventory ***/

	/* Handle experience draining */
	if (player_of_has(player, OF_DRAIN_EXP)) {
		if ((player->exp > 0) && one_in_(10)) {
			s32b d = damroll(10, 6) +
				(player->exp / 100) * z_info->life_drain_percent;
			player_exp_lose(player, d / 10, false);
		}

		equip_learn_flag(player, OF_DRAIN_EXP);
	}

	/* Recharge activatable objects and rods */
	recharge_objects();

	/* Notice things after time */
	if (!(turn % 100))
		equip_learn_after_time(player);

	/* Decrease trap timeouts */
	for (y = 0; y < c->height; y++) {
		for (x = 0; x < c->width; x++) {
			struct loc grid = loc(x, y);
			struct trap *trap = square(c, grid)->trap;
			while (trap) {
				if (trap->timeout) {
					trap->timeout--;
					if (!trap->timeout)
						square_light_spot(c, grid);
				}
				trap = trap->next;
			}
		}
	}


	/*** Involuntary Movement ***/

	/* Delayed Word-of-Recall */
	if (player->word_recall) {
		/* Count down towards recall */
		player->word_recall--;

		/* Activate the recall */
		if (!player->word_recall) {
			/* Disturbing! */
			disturb(player);

			/* Determine the level */
			if (player->depth) {
				msgt(MSG_TPLEVEL, "You feel yourself yanked upwards!");
				dungeon_change_level(player, 0);
			} else {
				msgt(MSG_TPLEVEL, "You feel yourself yanked downwards!");
				player_set_recall_depth(player);
				dungeon_change_level(player, player->recall_depth);
			}
		}
	}

	/* Delayed Deep Descent */
	if (player->deep_descent) {
		/* Count down towards descent */
		player->deep_descent--;

		/* Activate the descent */
		if (player->deep_descent == 0) {
			/* Calculate target depth */
			int target_increment = (4 / z_info->stair_skip) + 1;
			int target_depth = dungeon_get_next_level(player->max_depth,
													  target_increment);
			disturb(player);

			/* Determine the level */
			if (target_depth > player->depth) {
				msgt(MSG_TPLEVEL, "The floor opens beneath you!");
				dungeon_change_level(player, target_depth);
			} else {
				/* Otherwise do something disastrous */
				msgt(MSG_TPLEVEL, "You are thrown back in an explosion!");
				effect_simple(EF_DESTRUCTION, source_none(), "0", 0, 5, 0, 0, 0, NULL);
			}
		}
	}
}


/**
 * Housekeeping after the processing of a player command
 */
static void process_player_cleanup(void)
{
	int i;

	/* Significant */
	if (player->upkeep->energy_use) {
		/* Use some energy */
		player->energy -= player->upkeep->energy_use;

		/* Increment the total energy counter */
		player->total_energy += player->upkeep->energy_use;

		/* Player can be damaged by terrain */
		player_take_terrain_damage(player, player->grid);

		/* Do nothing else if player has auto-dropped stuff */
		if (!player->upkeep->dropping) {
			/* Hack -- constant hallucination */
			if (player->timed[TMD_IMAGE])
				player->upkeep->redraw |= (PR_MAP);

			/* Shimmer multi-hued monsters */
			for (i = 1; i < cave_monster_max(cave); i++) {
				struct monster *mon = cave_monster(cave, i);
				if (!mon->race)
					continue;
				if (!rf_has(mon->race->flags, RF_ATTR_MULTI))
					continue;
				square_light_spot(cave, mon->grid);
			}

			/* Maintain momentum.
			 * If the difference between the current grid and the previous grid is the
			 * same as the difference between the previous grid and the one before that
			 * then you are moving in a straight line (or stopped, or jumping).
			 * If you have also moved 1 grid in any of the 8 directions from the previous
			 * then you aren't stopped, jumping etc and can increase the momentum count,
			 * otherwise zero it.
			 **/
			bool moving = false;
			int mom = player->momentum;
			if ((player->grid.x != player->grid_last_1.x) || (player->grid.y != player->grid_last_1.y)) {
				/* Last action was movement */
				int xd1 = player->grid.x - player->grid_last_1.x;
				int yd1 = player->grid.y - player->grid_last_1.y;
				if ((xd1 >= -1) && (yd1 >= -1) && (xd1 <= 1) && (yd1 <= 1)) {
					/* Of one grid */
					int xd2 = player->grid_last_1.x - player->grid_last_2.x;
					int yd2 = player->grid_last_1.y - player->grid_last_2.y;
					if ((xd2 == xd1) && (yd2 == yd1)) {
						/* In the right direction */
						moving = true;
					}
				}
			}
			if (moving)
				player->momentum++;
			else
				player->momentum = 0;
			player->grid_last_2 = player->grid_last_1;
			player->grid_last_1 = player->grid;

			if (mom != player->momentum)
				player->upkeep->update |= PU_BONUS;

			/* Clear NICE flag, and show marked monsters */
			for (i = 1; i < cave_monster_max(cave); i++) {
				struct monster *mon = cave_monster(cave, i);
				mflag_off(mon->mflag, MFLAG_NICE);
				if (mflag_has(mon->mflag, MFLAG_MARK)) {
					if (!mflag_has(mon->mflag, MFLAG_SHOW)) {
						mflag_off(mon->mflag, MFLAG_MARK);
						update_mon(mon, cave, false);
					}
				}
			}
		}
	}

	/* Clear SHOW flag and player drop status */
	for (i = 1; i < cave_monster_max(cave); i++) {
		struct monster *mon = cave_monster(cave, i);
		mflag_off(mon->mflag, MFLAG_SHOW);
	}
	player->upkeep->dropping = false;

	/* Hack - update needed first because inventory may have changed */
	update_stuff(player);
	redraw_stuff(player);
}


/**
 * Process player commands from the command queue, finishing when there is a
 * command using energy (any regular game command), or we run out of commands
 * and need another from the user, or the character changes level or dies, or
 * the game is stopped.
 *
 * Notice the annoying code to handle "pack overflow", which
 * must come first just in case somebody manages to corrupt
 * the savefiles by clever use of menu commands or something. (Can go? NRM)
 *
 * Notice the annoying code to handle "monster memory" changes,
 * which allows us to avoid having to update the window flags
 * every time we change any internal monster memory field, and
 * also reduces the number of times that the recall window must
 * be redrawn.
 */
void process_player(void)
{
	/* Check for interrupts */
	player_resting_complete_special(player);
	event_signal(EVENT_CHECK_INTERRUPT);

	/* Repeat until energy is reduced */
	do {
		/* Refresh */
		notice_stuff(player);
		handle_stuff(player);
		event_signal(EVENT_REFRESH);

		/* Hack -- Pack Overflow */
		pack_overflow(NULL);

		/* Assume free turn */
		player->upkeep->energy_use = 0;

		/* Dwarves detect treasure */
		if (player_has(player, PF_SEE_ORE)) {
			/* Only if they are in good shape */
			if (!player->timed[TMD_IMAGE] &&
				!player->timed[TMD_CONFUSED] &&
				!player->timed[TMD_AMNESIA] &&
				!player->timed[TMD_STUN] &&
				!player->timed[TMD_PARALYZED] &&
				!player->timed[TMD_TERROR] &&
				!player->timed[TMD_AFRAID])
				effect_simple(EF_DETECT_GOLD, source_none(), "0", 0, 0, 0, 3, 3, NULL);
		}

		/* Paralyzed or Knocked Out player gets no turn */
		if (player->timed[TMD_PARALYZED] ||
			player_timed_grade_eq(player, TMD_STUN, "Knocked Out")) {
			cmdq_push(CMD_SLEEP);
		}

		/* Prepare for the next command */
		if (cmd_get_nrepeats() > 0) {
			event_signal(EVENT_COMMAND_REPEAT);
		} else {
			/* Check monster recall */
			if (player->upkeep->monster_race)
				player->upkeep->redraw |= (PR_MONSTER);

			/* Place cursor on player/target */
			event_signal(EVENT_REFRESH);
		}

		/* Get a command from the queue if there is one */
		if (!cmdq_pop(CTX_GAME))
			break;

		if (!player->upkeep->playing)
			break;

		process_player_cleanup();
	} while (!player->upkeep->energy_use &&
			 !player->is_dead &&
			 !player->upkeep->generate_level);

	/* Notice stuff (if needed) */
	notice_stuff(player);
}

/** Handle timed danger.
 * If the time limit is active and sufficient time has passed,
 * increase the danger level and mark stores for destruction.
 **/
void increase_danger_level(void)
{
	if (OPT(player,birth_time_limit)) {
		int danger = 0;
		int pturn = turn / 10;
		if (pturn >= z_info->town_easy_turns) {
			danger = 1 + ((pturn - z_info->town_easy_turns) / z_info->town_levelup_turns);
		}
		if (danger >= z_info->max_depth) {
			danger = z_info->max_depth-1;
		}
		/* This assumes that player->danger can't be decreased */
		if (danger != player->danger) {
			assert(player->danger < danger);
			while (player->danger < danger) {
				player->danger++;
				/* Effects of timed danger - destroy a store? */
				for(int i=0;i<MAX_STORES;i++) {
					if (player->danger == stores[i].max_danger)
						stores[i].destroy = true;
				}
			}
		}
	}
}

/**
 * Housekeeping on arriving on a new level
 */
void on_new_level(void)
{
	/* Handle timed danger */
	increase_danger_level();

	/* Arena levels are not really a level change */
	if (!player->upkeep->arena_level) {
		/* Play ambient sound on change of level. */
		play_ambient_sound();

		/* Cancel the target */
		target_set_monster(0);

		/* Cancel the health bar */
		health_track(player->upkeep, NULL);
	}

	/* Disturb */
	disturb(player);

	/* Track maximum player level */
	if (player->max_lev < player->lev)
		player->max_lev = player->lev;

	/* Track maximum dungeon level */
	if (player->active_quest < 0)
		if (player->max_depth < player->depth)
			player->max_depth = player->recall_depth = player->depth;

	/* Flush messages */
	event_signal(EVENT_MESSAGE_FLUSH);

	/* Update display */
	event_signal(EVENT_NEW_LEVEL_DISPLAY);

	/* Update player */
	player->upkeep->update |= (PU_BONUS | PU_HP | PU_SPELLS | PU_INVEN);
	player->upkeep->notice |= (PN_COMBINE);
	notice_stuff(player);
	update_stuff(player);
	redraw_stuff(player);

	/* Refresh */
	event_signal(EVENT_REFRESH);

	if (player->upkeep->arena_level) {
		return;
	}

	/* Announce (or repeat) the feeling */
	if ((player->depth) && (player->active_quest < 0))
		display_feeling(false);

	/* Check the surroundings */
	search(player);

	/* Give player minimum energy to start a new level, but do not reduce
	 * higher value from savefile for level in progress */
	if (player->energy < z_info->move_energy)
		player->energy = z_info->move_energy;
}

/**
 * Housekeeping on leaving a level
 */
static void on_leave_level(void) {
	/* Cancel any command */
	player_clear_timed(player, TMD_COMMAND, false);

	/* Don't allow command repeat if moved away from item used. */
	if (cmdq_does_previous_use_floor_item()) {
		cmd_disable_repeat();
	}

	/* Any pending processing */
	notice_stuff(player);
	update_stuff(player);
	redraw_stuff(player);

	/* Flush messages */
	event_signal(EVENT_MESSAGE_FLUSH);
}


/**
 * The main game loop.
 *
 * This function will run until the player needs to enter a command, or closes
 * the game, or the character dies.
 */
void run_game_loop(void)
{
	/* Tidy up after the player's command */
	process_player_cleanup();

	/* Keep processing the player until they use some energy or
	 * another command is needed */
	while (player->upkeep->playing) {
		process_player();
		if (player->upkeep->energy_use)
			break;
		else
			return;
	}

	/* The player may still have enough energy to move, so we run another
	 * player turn before processing the rest of the world */
	while (player->energy >= z_info->move_energy) {
		/* Do any necessary animations */
		event_signal(EVENT_ANIMATE);
		
		/* Process monster with even more energy first */
		process_monsters(cave, player->energy + 1);
		if (player->is_dead || !player->upkeep->playing ||
			player->upkeep->generate_level)
			break;

		/* Process the player until they use some energy */
		while (player->upkeep->playing) {
			process_player();
			if (player->upkeep->energy_use)
				break;
			else
				return;
		}
	}

	/* Now that the player's turn is fully complete, we run the main loop 
	 * until player input is needed again */
	while (true) {
		notice_stuff(player);
		handle_stuff(player);
		event_signal(EVENT_REFRESH);

		/* Process the rest of the world, give the player energy and 
		 * increment the turn counter unless we need to stop playing or
		 * generate a new level */
		if (player->is_dead || !player->upkeep->playing)
			return;
		else if (!player->upkeep->generate_level) {
			/* Process the rest of the monsters */
			process_monsters(cave, 0);

			/* Mark all monsters as ready to act when they have the energy */
			reset_monsters();

			/* Refresh */
			notice_stuff(player);
			handle_stuff(player);
			event_signal(EVENT_REFRESH);
			if (player->is_dead || !player->upkeep->playing)
				return;

			/* Process the world every ten turns */
			if (!(turn % 10) && !player->upkeep->generate_level) {
				process_world(cave);

				/* Refresh */
				notice_stuff(player);
				handle_stuff(player);
				event_signal(EVENT_REFRESH);
				if (player->is_dead || !player->upkeep->playing)
					return;
			}

			/* Give the player some energy */
			player->energy += turn_energy(player->state.speed);

			/* Count game turns */
			turn++;
		}

		/* Make a new level if requested */
		if (player->upkeep->generate_level) {
			bool arena = false;
			if (character_dungeon) {
				on_leave_level();
				if (cave->name && streq(cave->name, "arena")) {
					arena = true;
				}
			}

			prepare_next_level(&cave, player);
			on_new_level();

			player->upkeep->generate_level = false;

			/* Kill arena monster */
			if (arena) {
				player->upkeep->arena_level = false;
				if (player->upkeep->health_who) {
					kill_arena_monster(player->upkeep->health_who);
				}
			}
		}

		/* If the player has enough energy to move they now do so, after
		 * any monsters with more energy take their turns */
		while (player->energy >= z_info->move_energy) {
			/* Do any necessary animations */
			event_signal(EVENT_ANIMATE);

			/* Process monster with even more energy first */
			process_monsters(cave, player->energy + 1);
			if (player->is_dead || !player->upkeep->playing ||
				player->upkeep->generate_level)
				break;

			/* Process the player until they use some energy */
			while (player->upkeep->playing) {
				process_player();
				if (player->upkeep->energy_use)
					break;
				else
					return;
			}
		}
	}
}
