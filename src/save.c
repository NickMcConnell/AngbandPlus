/* File: save.c */

/*
 * Copyright (c) 1997 Ben Harrison, Jeff Greene, Diego Gonzalez and ohers
 *
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


ang_file *fff;

/*
 * Some "local" parameters, used to help write savefiles
 */

static byte	xor_byte;	/* Simple encryption */

static u32b	v_stamp = 0L;	/* A simple "checksum" on the actual values */
static u32b	x_stamp = 0L;	/* A simple "checksum" on the encoded bytes */



/*
 * These functions place information into a savefile a byte at a time
 */

/*
 * These functions place information into a savefile a byte at a time
 */

static void sf_put(byte v)
{
	/* Encode the value, write a character */
	xor_byte ^= v;
	(void)file_putc((int)xor_byte, fff);

	/* Maintain the checksum info */
	v_stamp += v;
	x_stamp += xor_byte;
}

static void wr_byte(byte v)
{
	sf_put(v);
}

static void wr_u16b(u16b v)
{
	sf_put((byte)(v & 0xFF));
	sf_put((byte)((v >> 8) & 0xFF));
}

static void wr_s16b(s16b v)
{
	wr_u16b((u16b)v);
}

static void wr_u32b(u32b v)
{
	sf_put((byte)(v & 0xFF));
	sf_put((byte)((v >> 8) & 0xFF));
	sf_put((byte)((v >> 16) & 0xFF));
	sf_put((byte)((v >> 24) & 0xFF));
}

static void wr_s32b(s32b v)
{
	wr_u32b((u32b)v);
}

static void wr_string(cptr str)
{
	while (*str)
	{
		wr_byte(*str);
		str++;
	}
	wr_byte(*str);
}


/*
 * These functions write info in larger logical records
 */


/*
 * Write an "item" record
 */
static void wr_item(const object_type *o_ptr)
{
	wr_s16b(o_ptr->k_idx);

	/* Location */
	wr_byte(o_ptr->iy);
	wr_byte(o_ptr->ix);

	wr_byte(o_ptr->tval);
	wr_byte(o_ptr->sval);
	wr_s16b(o_ptr->pval);

	wr_byte(o_ptr->discount);

	wr_byte(o_ptr->number);
	wr_s16b(o_ptr->weight);

	wr_byte(o_ptr->art_num);
	wr_byte(o_ptr->ego_num);

	wr_s16b(o_ptr->timeout);

	wr_s16b(o_ptr->to_h);
	wr_s16b(o_ptr->to_d);
	wr_s16b(o_ptr->to_a);
	wr_s16b(o_ptr->ac);
	wr_byte(o_ptr->dd);
	wr_byte(o_ptr->ds);

	wr_u32b(o_ptr->ident);

	wr_byte(o_ptr->marked);

	wr_s16b(o_ptr->mimic_r_idx);

	/* Old flags */
	wr_u32b(0L);
	wr_u32b(0L);
	wr_u32b(0L);

	/* Held by monster index */
	wr_s16b(o_ptr->held_m_idx);

	/* Extra information */
	wr_byte(o_ptr->xtra1);
	wr_u32b(o_ptr->xtra2);

	/* Save the inscription (if any) */
	if (o_ptr->obj_note)
	{
		wr_string(quark_str(o_ptr->obj_note));
	}
	else
	{
		wr_string("");
	}

	/* Object history */
	wr_byte(o_ptr->origin_nature);
	wr_s16b(o_ptr->origin_dlvl);
	wr_s16b(o_ptr->origin_r_idx);
	if (o_ptr->origin_m_name)
	{
		wr_string(quark_str(o_ptr->origin_m_name));
	}
	else
	{
		wr_string("");
	}
}



/*
 * Special monster flags that get saved in the savefile
 */
#define SAVE_MON_FLAGS (MFLAG_STERILE | MFLAG_ACTV | MFLAG_TOWN | MFLAG_WARY | \
						MFLAG_SLOWER | MFLAG_FASTER | MFLAG_ALWAYS_CAST | \
						MFLAG_AGGRESSIVE | MFLAG_ATTACKED_BAD | MFLAG_BONUS_ITEM | \
						MFLAG_HIT_BY_RANGED | MFLAG_HIT_BY_MELEE | MFLAG_QUEST | MFLAG_DESPERATE | \
						MFLAG_NEED_PASS_WALL_FLOW | MFLAG_QUEST_SUMMON | MFLAG_JUST_SCARED)


/*
 * Write a "monster" record
 */
static void wr_monster(const monster_type *m_ptr)
{
	u32b tmp32u;
	int i;

	wr_s16b(m_ptr->r_idx);

	wr_byte(m_ptr->fy);
	wr_byte(m_ptr->fx);
	wr_s16b(m_ptr->hp);
	wr_s16b(m_ptr->maxhp);
	wr_byte(m_ptr->m_speed);
	wr_s16b(m_ptr->m_energy);

	/* Find the number of monster timed effects */
	wr_byte(MON_TMD_MAX);

	/* Write all the monster timed effects, in a loop */
	for (i = 0; i < MON_TMD_MAX; i++)	wr_s16b(m_ptr->m_timed[i]);

	/*save the temporary flags*/
	tmp32u = m_ptr->mflag & (SAVE_MON_FLAGS);
	wr_u32b(tmp32u);
	wr_u32b(m_ptr->smart);
	wr_byte(m_ptr->target_y);
	wr_byte(m_ptr->target_x);
	wr_byte(m_ptr->mana);
	wr_byte(0);
}

/*
 * Write an "item" record
 */
static void wr_effect(const effect_type *x_ptr)
{

	wr_byte(x_ptr->x_type);
	wr_u16b(x_ptr->x_f_idx);

	wr_byte(x_ptr->x_cur_y);
	wr_byte(x_ptr->x_cur_x);


	wr_byte(x_ptr->x_countdown);
	wr_byte(x_ptr->x_repeats);

	wr_u16b(x_ptr->x_power);

	wr_s16b(x_ptr->x_source);

	wr_u16b(x_ptr->x_flags);

	wr_s16b(x_ptr->x_r_idx);
}


/*
 * Write a "lore" record
 */
static void wr_monster_lore(int r_idx)
{
	int i;

	monster_race *r_ptr = &r_info[r_idx];
	monster_lore *l_ptr = &l_list[r_idx];

	/* Count sights/deaths/kills */
	wr_s16b(l_ptr->sights);
	wr_s16b(l_ptr->deaths);
	wr_s16b(l_ptr->pkills);
	wr_s16b(l_ptr->tkills);

	/* Count wakes and ignores */
	wr_byte(l_ptr->wake);
	wr_byte(l_ptr->ignore);

	/* Extra stuff */
	wr_byte(l_ptr->xtra1);
	wr_byte(l_ptr->xtra2);

	/* Count drops */
	wr_byte(l_ptr->drop_gold);
	wr_byte(l_ptr->drop_item);

	/* Count spells */
	wr_byte(l_ptr->ranged);

	/* Count blows of each type */
	for (i = 0; i < MONSTER_BLOW_MAX; i++)
		wr_byte(l_ptr->blows[i]);

	/* Memorize flags */
	wr_u32b(l_ptr->r_l_flags1);
	wr_u32b(l_ptr->r_l_flags2);
	wr_u32b(l_ptr->r_l_flags3);
	wr_u32b(l_ptr->r_l_flags4);
	wr_u32b(l_ptr->r_l_flags5);
	wr_u32b(l_ptr->r_l_flags6);
	wr_u32b(l_ptr->r_l_flags7);
	wr_u32b(l_ptr->r_l_native);

	/* Monster limit per level */
	wr_byte(r_ptr->max_num);

	/* Later (?) */
	wr_byte(0);
	wr_byte(0);
	wr_byte(0);
}


/*
 * Write an "xtra" record
 */
static void wr_xtra(int k_idx)
{
	byte tmp8u = 0;

	object_kind *k_ptr = &k_info[k_idx];

	if (k_ptr->aware) tmp8u |= 0x01;
	if (k_ptr->tried) tmp8u |= 0x02;

	if (k_ptr->everseen) tmp8u |= 0x08;

	wr_byte(tmp8u);

	/*write the squelch settings*/
	tmp8u = k_ptr->squelch;

	wr_byte(tmp8u);
}


/*
 * Write a "store" record
 */
static void wr_store(const store_type *st_ptr)
{
	int j;

	/* Unused */
	wr_u32b(0L);

	/* Save the "insults" */
	wr_s16b(0);

	/* Save the current owner */
	wr_byte(st_ptr->owner);

	/* Save the stock size */
	wr_byte(st_ptr->stock_num);

	/* Save the "haggle" info */
	wr_s16b(0);
	wr_s16b(0);

	/* Save the stock */
	for (j = 0; j < st_ptr->stock_num; j++)
	{
		/* Save each item in stock */
		wr_item(&st_ptr->stock[j]);
	}
}

/*
 * Write an "artifact lore" record
 */
static void wr_artifact_lore(int a_idx)
{
	byte tmp8u = 0;

	/* We know about this artifact */
	if (a_l_list[a_idx].was_fully_identified) tmp8u |= 0x01;

	/* Write the flags */
	wr_byte(tmp8u);

	/* For future use */
	wr_byte(0);
	wr_byte(0);
	wr_byte(0);
}


/*
 * Write a "terrain lore" record
 */
static void wr_feature_lore(int f_idx)
{
	int i;

	/* Get the feature */
	feature_type *f_ptr = &f_info[f_idx];
	feature_lore *f_l_ptr = &f_l_list[f_idx];
	byte tmp8u = 0;

	/* Save the "everseen flag" */
	if (f_ptr->f_everseen) tmp8u |= 0x01;

	wr_byte(tmp8u);

	/* Write the terrain_lore memory*/
	wr_byte(f_l_ptr->f_l_sights);

	/*Write the lore flags*/
	wr_u32b(f_l_ptr->f_l_flags1);
	wr_u32b(f_l_ptr->f_l_flags2);
	wr_u32b(f_l_ptr->f_l_flags3);

	wr_byte(f_l_ptr->f_l_defaults);

	/*record the max amount of feat states*/
	wr_byte(MAX_FEAT_STATES);

	for (i = 0; i < MAX_FEAT_STATES; i++)
	{
		wr_byte(f_l_ptr->f_l_state[i]);
	}

	wr_byte(f_l_ptr->f_l_power);

	wr_byte(f_l_ptr->f_l_dam_non_native);
	wr_byte(f_l_ptr->f_l_native_moves);
	wr_byte(f_l_ptr->f_l_non_native_moves);
	wr_byte(f_l_ptr->f_l_native_to_hit_adj);
	wr_byte(f_l_ptr->f_l_non_native_to_hit_adj);
	wr_byte(f_l_ptr->f_l_stealth_adj);

}


/*
 * Write RNG state
 */
static errr wr_randomizer(void)
{
	int i;

	/* Zero */
	wr_u16b(0);

	/* Place */
	wr_u16b(Rand_place);

	/* State */
	for (i = 0; i < RAND_DEG; i++)
	{
		wr_u32b(Rand_state[i]);
	}

	/* Success */
	return (0);
}


/*
 * Write the "options"
 */
static void wr_options(void)
{
	int i, k;

	u32b flag[8];
	u32b mask[8];
	u32b window_flag[ANGBAND_TERM_MAX];
	u32b window_mask[ANGBAND_TERM_MAX];


	/*** Oops ***/

	/* Oops */
	for (i = 0; i < 4; i++) wr_u32b(0L);


	/*** Special Options ***/

	/* Write "delay_factor" */
	wr_byte(op_ptr->delay_factor);

	/* Write "hitpoint_warn" */
	wr_byte(op_ptr->hitpoint_warn);

	wr_u16b(0);	/* oops */


	/*** Normal options ***/

	/* Reset */
	for (i = 0; i < 8; i++)
	{
		flag[i] = 0L;
		mask[i] = 0L;
	}

	/* Analyze the options */
	for (i = 0; i < OPT_MAX; i++)
	{
		int os = i / 32;
		int ob = i % 32;

		/* Process real entries */
		if (options[i].name)
		{
			/* Set flag */
			if (op_ptr->opt[i])
			{
				/* Set */
				flag[os] |= (1L << ob);
			}

			/* Set mask */
			mask[os] |= (1L << ob);
		}
	}

	/* Dump the flags */
	for (i = 0; i < 8; i++) wr_u32b(flag[i]);

	/* Dump the masks */
	for (i = 0; i < 8; i++) wr_u32b(mask[i]);


	/*** Window options ***/

	/* Reset */
	for (i = 0; i < ANGBAND_TERM_MAX; i++)
	{
		/* Flags */
		window_flag[i] = op_ptr->window_flag[i];

		/* Mask */
		window_mask[i] = 0L;

		/* Build the mask */
		for (k = 0; k < 32; k++)
		{
			/* Set mask */
			if (window_flag_desc[k])
			{
				window_mask[i] |= (1L << k);
			}
		}
	}

	/* Dump the flags */
	for (i = 0; i < ANGBAND_TERM_MAX; i++) wr_u32b(window_flag[i]);

	/* Dump the masks */
	for (i = 0; i < ANGBAND_TERM_MAX; i++) wr_u32b(window_mask[i]);
}


/*
 * Write some "extra" info
 */
static void wr_extra(void)
{
	int i;

	wr_string(op_ptr->full_name);

	wr_string(p_ptr->died_from);

	wr_string(p_ptr->history);

	/* Race/Class/Gender/Spells */
	wr_byte(p_ptr->prace);
	wr_byte(p_ptr->pclass);
	wr_byte(p_ptr->psex);
	wr_byte(0);	/* oops */

	wr_byte(p_ptr->hitdie);
	wr_byte(p_ptr->expfact);

	wr_s16b(p_ptr->age);
	wr_s16b(p_ptr->ht);
	wr_s16b(p_ptr->wt);

	/* Dump the stats (maximum and current) */
	for (i = 0; i < A_MAX; ++i) wr_s16b(p_ptr->stat_max[i]);
	for (i = 0; i < A_MAX; ++i) wr_s16b(p_ptr->stat_cur[i]);
	for (i = 0; i < A_MAX; ++i) wr_s16b(p_ptr->stat_birth[i]);
	for (i = 0; i < A_MAX; ++i) wr_s16b(p_ptr->stat_quest_add[i]);

	wr_s16b(p_ptr->ht_birth);
	wr_s16b(p_ptr->wt_birth);
	wr_s16b(p_ptr->sc_birth);
	wr_s32b(p_ptr->au_birth);

	/* Ignore the transient stats */
	for (i = 0; i < 12; ++i) wr_s16b(0);

	wr_u16b(p_ptr->q_fame);
	wr_u16b(p_ptr->deferred_rewards);

	wr_u32b(p_ptr->au);

	wr_u32b(p_ptr->max_exp);
	wr_u32b(p_ptr->exp);
	wr_u16b(p_ptr->exp_frac);
	wr_s16b(p_ptr->lev);

	wr_s16b(p_ptr->mhp);
	wr_s16b(p_ptr->chp);
	wr_u16b(p_ptr->chp_frac);

	wr_s16b(p_ptr->msp);
	wr_s16b(p_ptr->csp);
	wr_u16b(p_ptr->csp_frac);

	/* Max Player and Dungeon Levels */
	wr_s16b(p_ptr->max_lev);
	wr_s16b(p_ptr->max_depth);
	wr_s16b(p_ptr->recall_depth);
	wr_s16b(p_ptr->quest_depth);

	/* More info */
	wr_s16b(0);	/* oops */
	wr_s16b(0);	/* oops */
	wr_s16b(0);	/* oops */
	wr_s16b(p_ptr->sc);
	wr_s16b(0);	/* oops */

	wr_s16b(0);		/* old "rest" */
	wr_s16b(p_ptr->food);

	wr_s16b(0);	/* old "food_digested" */
	wr_s16b(0);	/* old "protection" */
	wr_s16b(p_ptr->p_energy);
	wr_s16b(p_ptr->word_recall);
	wr_s16b(p_ptr->state.see_infra);

	wr_byte(p_ptr->confusing);
	wr_byte(0);	/* oops */
	wr_byte(p_ptr->searching);
	wr_byte(0);	/* oops */
	wr_byte(0);	/* oops */
	wr_byte(0);

	/* Find the number of timed effects */
	wr_byte(TMD_MAX);

	/* Write all the effects, in a loop */
	for (i = 0; i < TMD_MAX; i++) wr_s16b(p_ptr->timed[i]);

	/* 4gai use */
	wr_s16b(p_ptr->base_wakeup_chance);
	wr_s16b(total_wakeup_chance);

	/* Save item-quality squelch sub-menu */
	for (i = 0; i < SQUELCH_BYTES; i++) wr_byte(squelch_level[i]);

	/* Store the name of the current greater vault */
	wr_string(g_vault_name);

	/* Save the current number of ego-item types */
	wr_u16b(z_info->e_max);

	/* Save ego-item squelch settings */
	for (i = 0; i < z_info->e_max; i++)
	{
		ego_item_type *e_ptr = &e_info[i];
		byte tmp8u = 0;

		if (e_ptr->squelch) tmp8u |= 0x01;
		if (e_ptr->everseen) tmp8u |= 0x02;

		wr_byte(tmp8u);
	}

	/*Write the current number of auto-inscriptions*/
	wr_u16b(inscriptionsCount);

	/*Write the autoinscriptions array*/
	for(i = 0; i < inscriptionsCount; i++)
	{
		wr_s16b(inscriptions[i].kindIdx);
		wr_string(quark_str(inscriptions[i].inscriptionIdx));
	}

	/* Store the bones file selector, if the player is not dead. -LM- */
	wr_s16b(player_ghost_num);

	/* Store the number of thefts on the level. -LM- */
	wr_byte(0);

	/* Store number of monster traps on this level. -LM- */
	wr_byte(num_trap_on_level);

	/* Future use */
	for (i = 0; i < 13; i++) wr_byte(0);

	/* Save the current dungeon summon mask */
	wr_u32b (dungeon_summon_mask_f7);

	/* Random artifact seed */
	wr_u32b(seed_randart);


	/* Ignore some flags */
	wr_u32b(0L);	/* oops */
	wr_u32b(0L);	/* oops */
	wr_u32b(0L);	/* oops */


	/* Write the "special seeds" */
	wr_u32b(seed_flavor);
	wr_u32b(seed_town);
	wr_u32b(seed_ghost);

	/* Special stuff */
	wr_u16b(p_ptr->panic_save);
	wr_u16b(p_ptr->total_winner);
	wr_u16b(p_ptr->noscore);

	/* Write death */
	wr_byte(p_ptr->is_dead);

	/* Write feeling */
	wr_byte(feeling);

	/* Turn of last "feeling" */
	wr_byte(do_feeling);

	/* Current turn */
	wr_s32b(turn);

	/*Current Player Turn*/
	wr_s32b(p_ptr->p_turn);

	/* Turn counter for the quest indicator */
	{
		/* Get the timer value. Clear the last bit */
		u16b tmp16u = quest_indicator_timer & ~(QUEST_INDICATOR_COMPLETE_BIT);

		/* Turn on the last bit if the quest was completed */
		if (quest_indicator_complete) tmp16u |= (QUEST_INDICATOR_COMPLETE_BIT);

		/* Write timer + complete bit */
		wr_u16b(tmp16u);
	}

	/* Panel change offsets */
	wr_u16b(panel_change_offset_y);
	wr_u16b(panel_change_offset_x);
}


/*
 * Dump the random artifacts
 */
static void wr_randarts(void)
{
	int i, begin;

	if (adult_rand_artifacts) begin = 0;
	else begin = z_info->art_norm_max;

	wr_u16b(begin);
	wr_u16b(z_info->art_max);
	wr_u16b(z_info->art_norm_max);


	for (i = begin; i < z_info->art_max; i++)
	{
		artifact_type *a_ptr = &a_info[i];

		wr_string(a_ptr->name);

		wr_byte(a_ptr->tval);
		wr_byte(a_ptr->sval);
		wr_s16b(a_ptr->pval);

		wr_s16b(a_ptr->to_h);
		wr_s16b(a_ptr->to_d);
		wr_s16b(a_ptr->to_a);
		wr_s16b(a_ptr->ac);

		wr_byte(a_ptr->dd);
		wr_byte(a_ptr->ds);

		wr_s16b(a_ptr->weight);

		wr_s32b(a_ptr->cost);

		wr_u32b(a_ptr->a_flags1);
		wr_u32b(a_ptr->a_flags2);
		wr_u32b(a_ptr->a_flags3);
		wr_u32b(a_ptr->a_native);

		wr_byte(a_ptr->a_level);
		wr_byte(a_ptr->a_rarity);

		wr_byte(a_ptr->activation);
		wr_u16b(a_ptr->time);
		wr_u16b(a_ptr->randtime);
	}

}

/*
 * Write the notes into the savefile. Every savefile has at least NOTES_MARK.
 */
static void wr_notes(void)
{
	char end_note[80];

	/* Paranoia */
	if (adult_take_notes && notes_file)
	{
    	char tmpstr[100];

    	(void)file_close(notes_file);

      	/* Re-open for readding */
    	notes_file = file_open(notes_fname, MODE_READ, -1);

    	while (TRUE)
    	{
			/* Read the note from the tempfile */
			if (!file_getl(notes_file, tmpstr, sizeof(tmpstr)))
			{
				/* Found the end */
				break;
			}

			/* Paranoia */
			if (strcmp(tmpstr, NOTES_MARK) == 0) continue;

      		/* Write it into the savefile */
      		wr_string(tmpstr);
    	}

    	(void)file_close(notes_file);

    	/* Re-open for appending */
    	notes_file = file_open(notes_fname, MODE_APPEND, FTYPE_TEXT);
  	}

	my_strcpy(end_note, NOTES_MARK, sizeof(end_note));

  	/* Always write NOTES_MARK */
  	wr_string(end_note);
}


/*
 * Write variable extensions to the savefile
 */
static void wr_extensions(void)
{
	/* Write end mark for extensions */
	wr_s16b(END_EXTENSIONS);
}


/*
 * The cave grid flags that get saved in the savefile
 */
#define IMPORTANT_FLAGS (CAVE_MARK | CAVE_GLOW | CAVE_ICKY | CAVE_DTRAP | \
						 CAVE_ROOM | CAVE_MARKED | CAVE_G_VAULT)


/*
 * Write the current dungeon
 */
static void wr_dungeon(void)
{
	int i, y, x;

	byte tmp8u;

	byte count;
	byte prev_char;


	/*** Basic info ***/

	/* Dungeon specific info follows */
	wr_u16b(p_ptr->depth);
	wr_u16b(p_ptr->dungeon_type);
	wr_u16b(p_ptr->py);
	wr_u16b(p_ptr->px);
	wr_byte(p_ptr->cur_map_hgt);
	wr_byte(p_ptr->cur_map_wid);
	wr_u16b(altered_inventory_counter);
	wr_u16b(0);


	/*** Simple "Run-Length-Encoding" of cave ***/

	/* Note that this will induce two wasted bytes */
	count = 0;
	prev_char = 0;

	/* Dump the cave */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Extract the important cave_info flags */
			tmp8u = (cave_info[y][x] & (IMPORTANT_FLAGS));

			/* If the run is broken, or too full, flush it */
			if ((tmp8u != prev_char) || (count == MAX_UCHAR))
			{
				wr_byte((byte)count);
				wr_byte((byte)prev_char);
				prev_char = tmp8u;
				count = 1;
			}

			/* Continue the run */
			else
			{
				count++;
			}
		}
	}

	/* Flush the data (if any) */
	if (count)
	{
		wr_byte((byte)count);
		wr_byte((byte)prev_char);
	}


	/*** Simple "Run-Length-Encoding" of cave ***/

	/* Note that this will induce two wasted bytes */
	count = 0;
	prev_char = 0;

	/* Dump the cave */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Extract a byte */
			tmp8u = cave_feat[y][x];

			/* If the run is broken, or too full, flush it */
			if ((tmp8u != prev_char) || (count == MAX_UCHAR))
			{
				wr_byte((byte)count);
				wr_byte((byte)prev_char);
				prev_char = tmp8u;
				count = 1;
			}

			/* Continue the run */
			else
			{
				count++;
			}
		}
	}

	/* Flush the data (if any) */
	if (count)
	{
		wr_byte((byte)count);
		wr_byte((byte)prev_char);
	}


	/*** Compact ***/

	/* Compact the objects */
	compact_objects(0);

	/* Compact the monsters */
	compact_monsters(0);


	/*** Dump objects ***/

	/* Total objects */
	wr_u16b(o_max);

	/* Dump the objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Dump it */
		wr_item(o_ptr);
	}


	/*** Dump the monsters ***/

	/* Total monsters */
	wr_u16b(mon_max);

	/* Dump the monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

		/* Dump it */
		wr_monster(m_ptr);
	}

	/*** Dump the effects ***/

	/* Total Effects */
	wr_u16b(x_max);

	/* Dump the Effects */
	for (i = 1; i < x_max; i++)
	{
		effect_type *x_ptr = &x_list[i];

		/* Dump it */
		wr_effect(x_ptr);
	}
}



/*
 * Actually write a save-file
 */
static bool wr_savefile_new(void)
{
	int i;

	u32b now;

	u16b tmp16u;

	/* Guess at the current time */
	now = time((time_t *)0);

	/* Note the operating system */
	sf_xtra = 0L;

	/* Note when the file was saved */
	sf_when = now;

	/* Note the number of saves */
	sf_saves++;

	/*** Actually write the file ***/

	/* Dump the file header */
	xor_byte = 0;
	wr_byte(VERSION_MAJOR);

	xor_byte = 0;

	wr_byte(VERSION_MINOR);
	xor_byte = 0;
	wr_byte(VERSION_PATCH);
	xor_byte = 0;
	wr_byte(VERSION_EXTRA);
	xor_byte = 0;
	wr_byte(game_mode);
	xor_byte = 0;

	/* Reset the checksum */
	v_stamp = 0L;
	x_stamp = 0L;

	/* Operating system */
	wr_u32b(sf_xtra);

	/* Time file last saved */
	wr_u32b(sf_when);

	/* Number of past lives */
	wr_u16b(sf_lives);

	/* Number of times saved */
	wr_u16b(sf_saves);


	/* Space */
	wr_u32b(0L);
	wr_u32b(0L);


	/* Write the RNG state */
	wr_randomizer();

	/* Write the boolean "options" */
	wr_options();

	/* Dump the number of "messages" */
	tmp16u = messages_num();
	if (tmp16u > 80) tmp16u = 80;
	wr_u16b(tmp16u);

	/* Dump the messages (oldest first!) */
	for (i = tmp16u - 1; i >= 0; i--)
	{
		wr_string(message_str((s16b)i));
		wr_u16b(message_type((s16b)i));
	}


	/* Dump the monster lore */
	tmp16u = z_info->r_max;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++) wr_monster_lore(i);

	/* Dump the object memory */
	tmp16u = z_info->k_max;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++) wr_xtra(i);

	/* Hack -- Dump the quests */
	tmp16u = z_info->q_max;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		quest_type *q_ptr = &q_info[i];

		wr_byte(q_ptr->q_type);

		/* Only limited info for permanent quests.  The rest is detailed in quest.txt */
		if (q_ptr->q_type == QUEST_PERMANENT)
		{
			wr_byte(q_ptr->q_flags);
			wr_s16b(q_ptr->q_num_killed);
			continue;
		}

		wr_u16b(q_ptr->q_reward);
		wr_u16b(q_ptr->q_fame_inc);
		wr_byte(q_ptr->base_level);
		wr_byte(q_ptr->q_theme);
		wr_s16b(q_ptr->mon_idx);
		wr_s32b(q_ptr->turn_counter);
		wr_s16b(q_ptr->q_num_killed);
		wr_s16b(q_ptr->q_max_num);
		wr_byte(q_ptr->q_flags);
	}

	/* Hack -- Dump the artifacts */
	tmp16u = z_info->art_max;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		wr_byte(a_ptr->a_cur_num);
		wr_byte(0);
		wr_byte(0);
		wr_byte(0);
	}


	/* Write the "extra" information */
	wr_extra();

	/* Dump the "player hp" entries */
	tmp16u = z_info->max_level;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		wr_s16b(p_ptr->player_hp[i]);
	}

	/* Write spell data */
	wr_u16b(PY_MAX_SPELLS);

	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
		wr_byte(p_ptr->spell_flags[i]);
	}

	/* Dump the ordered spells */
	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
		wr_byte(p_ptr->spell_order[i]);
	}

	/*Write the randarts*/
	wr_randarts();

	/*Copy the notes file into the savefile*/
	wr_notes();

	/* Extensions */
	wr_extensions();

	/* Write the inventory */
	for (i = 0; i < ALL_INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Dump index */
		wr_u16b((u16b)i);

		/* Dump object */
		wr_item(o_ptr);
	}

	/* Add a sentinel */
	wr_u16b(0xFFFF);


	/* Note the stores */
	tmp16u = MAX_STORES;
	wr_u16b(tmp16u);

	/* Dump the stores */
	for (i = 0; i < tmp16u; i++) wr_store(&store[i]);

	/* Dump the current number of terrain features */
	tmp16u = z_info->f_max;
	wr_u16b(tmp16u);

	/* Dump terrain lore */
	for (i = 0; i < tmp16u; i++) wr_feature_lore(i);

	/* Dump the current number of artifacts (normal + special) */
	tmp16u = z_info->art_norm_max;
	wr_u16b(tmp16u);

	/* Dump artifact lore */
	for (i = 0; i < tmp16u; i++) wr_artifact_lore(i);

	/* Player is not dead, write the dungeon */
	if (!p_ptr->is_dead)
	{
		/* Dump the dungeon */
		wr_dungeon();
	}

	/* Write the "value check-sum" */
	wr_u32b(v_stamp);

	/* Write the "encoded checksum" */
	wr_u32b(x_stamp);

	/* Error in save */
	if (file_error(fff) || (file_flush(fff) == EOF)) return FALSE;

	/* Successful save */
	return TRUE;
}

/*
 * Medium level player saver
 *
 * XXX XXX XXX Angband 2.8.0 will use "fd" instead of "fff" if possible
 */
static bool save_player_aux(cptr name)
{
	bool ok = FALSE;

	int fd;

	/* No file yet */
	fff = NULL;

	/* Grab permissions */
	safe_setuid_grab();

	/* Create the savefile */
	fff = file_open(name, MODE_WRITE, FTYPE_TEXT
			);
	if (fff) fd = 1;
	else fd = 0;

	/* Drop permissions */
	safe_setuid_drop();

	/* File is okay */
	if (fd >= 0)
	{
		/* Close the "fd" */
		file_close(fff);

		/* Grab permissions */
		safe_setuid_grab();

		/* Open the savefile */
		fff = file_open(name, MODE_WRITE, FTYPE_TEXT);

		/* Drop permissions */
		safe_setuid_drop();

		/* Successful open */
		if (fff)
		{
			/* Write the savefile */
			if (wr_savefile_new()) ok = TRUE;

			/* Attempt to close it */
			if (!file_close(fff)) ok = FALSE;
		}

		/* Grab permissions */
		safe_setuid_grab();

		/* Remove "broken" files */
		if (!ok) file_delete(name);

		/* Drop permissions */
		safe_setuid_drop();
	}

	else file_close(fff);

	/* Failure */
	if (!ok) return (FALSE);

	/* Successful save */
	character_saved = TRUE;

	/* Success */
	return (TRUE);
}



/*
 * Attempt to save the player in a savefile
 */
bool save_player(void)
{
	int result = FALSE;

	char safe[1024];

	save_player_ghost_file();

	/* New savefile */
	my_strcpy(safe, savefile, sizeof(safe));
	my_strcat(safe, ".new", sizeof(safe));

	/* Grab permissions */
	safe_setuid_grab();

	/* Remove it */
	file_delete(safe);

	/* Drop permissions */
	safe_setuid_drop();

	/* Attempt to save the player */
	if (save_player_aux(safe))
	{
		char temp[1024];

		/* Old savefile */
		my_strcpy(temp, savefile, sizeof(temp));
		my_strcat(temp, ".old", sizeof(temp));

		/* Grab permissions */
		safe_setuid_grab();

		/* Remove it */
		file_delete(temp);

			/* Preserve old savefile */
		file_move(savefile, temp);

		/* Activate new savefile */
		file_move(safe, savefile);

		/* Remove preserved savefile */
		file_delete(temp);

		/* Drop permissions */
		safe_setuid_drop();

		/* Hack -- Pretend the character was loaded */
		character_loaded = TRUE;

		/* Success */
		result = TRUE;
	}


	/* Return the result */
	return (result);
}

