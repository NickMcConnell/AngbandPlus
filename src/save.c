/* File: save.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Some "local" parameters, used to help write savefiles
 */

static FILE	*fff;		/* Current save "file" */

static byte	xor_byte;	/* Simple encryption */

static u32b	v_stamp = 0L;	/* A simple "checksum" on the actual values */
static u32b	x_stamp = 0L;	/* A simple "checksum" on the encoded bytes */

/*
 * These functions place information into a savefile a byte at a time
 */

static void sf_put(byte v)
{
	/* Encode the value, write a character */
	xor_byte ^= v;
	(void)putc((int)xor_byte, fff);

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
static void wr_item(object_type *o_ptr)
{
	u16b save_flags = 0;

	/* Don't waste space on "rare" fields */
	if (o_ptr->pval)		 save_flags |= 0x0001;
	if (o_ptr->discount)	 save_flags |= 0x0002;
	if (o_ptr->timeout)		 save_flags |= 0x0004;
	if (o_ptr->to_h)		 save_flags |= 0x0008;
	if (o_ptr->to_d)		 save_flags |= 0x0010;
	if (o_ptr->to_a)		 save_flags |= 0x0020;
	if (o_ptr->ac)			 save_flags |= 0x0040;
	if (o_ptr->dd)			 save_flags |= 0x0080;
	if (o_ptr->ds)			 save_flags |= 0x0100;
	if (o_ptr->origin_r_idx) save_flags |= 0x0200;
	if (o_ptr->origin_s_idx) save_flags |= 0x0400;
 	if (o_ptr->origin_u_idx) save_flags |= 0x0800;
	if (o_ptr->marked)		 save_flags |= 0x1000;
	if (o_ptr->held_m_idx)	 save_flags |= 0x2000;
	if (o_ptr->xtra1)		 save_flags |= 0x4000;
	if (o_ptr->xtra2)		 save_flags |= 0x8000;

	wr_u16b(save_flags);
	
	/* The item */
	wr_s16b(o_ptr->k_idx);
	
	/* Location */
	wr_byte(o_ptr->iy);
	wr_byte(o_ptr->ix);

	wr_byte(o_ptr->tval);
	wr_byte(o_ptr->sval);
	if (o_ptr->pval) wr_s16b(o_ptr->pval);

	if (o_ptr->discount) wr_byte(o_ptr->discount);

	wr_byte(o_ptr->number);

	wr_byte(o_ptr->a_idx); 
	wr_byte(o_ptr->e_idx);
	wr_byte(o_ptr->prefix_idx);

	if (o_ptr->timeout) wr_s16b(o_ptr->timeout);

	if (o_ptr->to_h) wr_s16b(o_ptr->to_h);
	if (o_ptr->to_d) wr_s16b(o_ptr->to_d);
	if (o_ptr->to_a) wr_s16b(o_ptr->to_a);
	if (o_ptr->ac) wr_s16b(o_ptr->ac);
	if (o_ptr->dd) wr_byte(o_ptr->dd);
	if (o_ptr->ds) wr_byte(o_ptr->ds);

	wr_byte(o_ptr->ident);

	wr_byte(o_ptr->origin_nature);
	wr_s16b(o_ptr->origin_dlvl);
	if (o_ptr->origin_r_idx) wr_s16b(o_ptr->origin_r_idx);
	if (o_ptr->origin_s_idx) wr_s16b(o_ptr->origin_s_idx);
	if (o_ptr->origin_u_idx) wr_s16b(o_ptr->origin_u_idx);

	if (o_ptr->marked) wr_byte(o_ptr->marked);

	/* Held by monster index */
	if (o_ptr->held_m_idx) wr_s16b(o_ptr->held_m_idx);

	/* Extra information */
	if (o_ptr->xtra1) wr_byte(o_ptr->xtra1);
	if (o_ptr->xtra2) wr_byte(o_ptr->xtra2);

	/* Save the inscription (if any) */
	if (o_ptr->note)
	{
		wr_string(quark_str(o_ptr->note));
	}
	else
	{
		wr_string("");
	}
}

/*
 * Write a "monster" record
 */
static void wr_monster(monster_type *m_ptr)
{
	wr_s16b(m_ptr->r_idx);
	wr_s16b(m_ptr->s_idx);
	wr_s16b(m_ptr->u_idx);
	wr_byte(m_ptr->attr);
	wr_byte(m_ptr->fy);
	wr_byte(m_ptr->fx);
	wr_s16b(m_ptr->hp);
	wr_s16b(m_ptr->maxhp);
	wr_s16b(m_ptr->csleep);
	wr_byte(m_ptr->mspeed);
	wr_byte(m_ptr->energy);
	wr_byte(m_ptr->stunned);
	wr_byte(m_ptr->confused);
	wr_byte(m_ptr->monfear);
	wr_byte(m_ptr->blinded);
	wr_byte(m_ptr->calmed);

	wr_u16b(m_ptr->bleeding);
	wr_u16b(m_ptr->poisoned);

}
	
/*
 * Write a "lore" record
 */
static void wr_lore(bool unique, int idx)
{
	monster_lore *l_ptr = (unique ? &lu_list[idx] : &lr_list[idx]);

	u16b save_flags = 0;

	/* Don't waste space on unused fields */
	if (l_ptr->r_sights)		save_flags |= 0x0001;
	if (l_ptr->r_deaths)		save_flags |= 0x0002;
	if (l_ptr->r_pkills)		save_flags |= 0x0004;
	if (l_ptr->r_tkills)		save_flags |= 0x0008;
	if (l_ptr->r_wake)			save_flags |= 0x0010;
	if (l_ptr->r_cast)			save_flags |= 0x0020;
	if (l_ptr->r_blows[0])		save_flags |= 0x0040;
	if (l_ptr->r_blows[1])		save_flags |= 0x0080;
	if (l_ptr->r_blows[2])		save_flags |= 0x0100;
	if (l_ptr->r_blows[3])		save_flags |= 0x0200;
	if (l_ptr->flags1)			save_flags |= 0x0400;
	if (l_ptr->flags2)			save_flags |= 0x0800;
	if (l_ptr->flags3)			save_flags |= 0x1000;
	if (l_ptr->s_flags1)		save_flags |= 0x2000;
	if (l_ptr->s_flags2)		save_flags |= 0x4000;
	if (l_ptr->s_flags3)		save_flags |= 0x8000;

	wr_u16b(save_flags);

	/* Count sights/deaths/kills */
	if (l_ptr->r_sights) wr_s16b(l_ptr->r_sights);
	if (l_ptr->r_deaths) wr_s16b(l_ptr->r_deaths);
	if (l_ptr->r_pkills) wr_s16b(l_ptr->r_pkills);
	if (l_ptr->r_tkills) wr_s16b(l_ptr->r_tkills);

	/* Count wakes and ignores */
	if (l_ptr->r_wake) wr_byte(l_ptr->r_wake);
	wr_byte(l_ptr->r_ignore);

	/* Count drops */
	wr_byte(l_ptr->r_drop_gold);
	wr_byte(l_ptr->r_drop_item);

	/* Count spells */
	if (l_ptr->r_cast) wr_byte(l_ptr->r_cast);

	/* Count blows of each type */
	if (l_ptr->r_blows[0]) wr_byte(l_ptr->r_blows[0]);
	if (l_ptr->r_blows[1]) wr_byte(l_ptr->r_blows[1]);
	if (l_ptr->r_blows[2]) wr_byte(l_ptr->r_blows[2]);
	if (l_ptr->r_blows[3]) wr_byte(l_ptr->r_blows[3]);

	/* Memorized flags */
	if (l_ptr->flags1) wr_u32b(l_ptr->flags1);
	if (l_ptr->flags2) wr_u32b(l_ptr->flags2);
	if (l_ptr->flags3) wr_u32b(l_ptr->flags3);
	if (l_ptr->s_flags1) wr_u32b(l_ptr->s_flags1);
	if (l_ptr->s_flags2) wr_u32b(l_ptr->s_flags2);
	if (l_ptr->s_flags3) wr_u32b(l_ptr->s_flags3);

	if (!unique) wr_byte(r_info[idx].cur_unique);
}

/*
 * Write a "monster" record
 */
static void wr_trap(trap_type *t_ptr)
{
	byte tmp8u = 0;

	wr_s16b(t_ptr->w_idx);
	wr_byte(t_ptr->fy);
	wr_byte(t_ptr->fx);
	wr_byte(t_ptr->charges);

	if (t_ptr->visible) tmp8u |= 0x01;
	wr_byte(tmp8u);
}

/*
 * Write an item memory record
 */
static void wr_item_memory(int k_idx)
{
	byte tmp8u = 0;

	object_kind *k_ptr = &k_info[k_idx];

	if (k_ptr->aware) tmp8u |= 0x01;
	if (k_ptr->tried) tmp8u |= 0x02;
	if (k_ptr->squelch) tmp8u |= 0x04;
	if (k_ptr->everseen) tmp8u |= 0x08;

	wr_byte(tmp8u);
}

/*
 * Write a "store" record
 */
static void wr_store(store_type *st_ptr)
{
	int j;

	/* Save the current owner */
	wr_byte(st_ptr->owner);

	/* Save the stock size */
	wr_byte(st_ptr->stock_num);

	/* Save the stock */
	for (j = 0; j < st_ptr->stock_num; j++)
	{
		/* Save each item in stock */
		wr_item(&st_ptr->stock[j]);
	}
}

/*
 * Write RNG state
 */
static errr wr_randomizer(void)
{
	int i;

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
 *
 * Note - Vanilla (and EyAngband 0.1.4 and older) used options masks To check if options 
 * are valid - if major modifications to the options lists are made they should be re-instated.
 */
static void wr_options(void)
{
	int i;

	u16b flag16[8];

	/*** Special Options ***/

	/* Write "delay_factor" */
	wr_byte(op_ptr->delay_factor);

	/* Write "hitpoint_warn" */
	wr_byte(op_ptr->hitpoint_warn);

	/*** Normal options ***/

	/* Reset */
	for (i = 0; i < 4; i++) flag16[i] = 0L;

	/* Analyze the options */
	for (i = 0; i < OPT_NORMAL; i++)
	{
		int os = i / 16;
		int ob = i % 16;

		/* Process real entries */
		if ((options[i].text) && (op_ptr->opt[i])) flag16[os] |= (1L << ob);
	}

	/* Dump the flags */
	for (i = 0; i < 4; i++) wr_u16b(flag16[i]);

	/*** Birth and Adult options ***/

	/* Reset */
	for (i = 0; i < 4; i++) flag16[i] = 0L;

	/* Analyze the options */
	for (i = 0; i < OPT_BIRTH; i++)
	{
		int os = i / 16;
		int ob = i % 16;

		/* Process real entries */
		if (options_birth[i].text)
		{
			if (op_ptr->opt_birth[i]) flag16[os]   |= (1L << ob);
			if (op_ptr->opt_adult[i]) flag16[os+2] |= (1L << ob);
		}
	}

	/* Dump the flags */
	for (i = 0; i < 4; i++) wr_u16b(flag16[i]);

	/*** Cheating/scoring options ***/

	/* Reset */
	flag16[0] = 0L;
	flag16[1] = 0L;

	/* Analyze the options */
	for (i = 0; i < OPT_CHEAT; i++)
	{
		int ob = i % 16;

		/* Process real entries */
		if (options_cheat[i].text)
		{
			if (op_ptr->opt_cheat[i]) flag16[0] |= (1L << ob);
			if (op_ptr->opt_score[i]) flag16[1] |= (1L << ob);
		}

	}

	/* Dump the flags */
	wr_u16b(flag16[0]);
	wr_u16b(flag16[1]);

	/*** Squelching options ***/

	/* Squelch bytes */
	for (i = 0; i < MAX_SQ_TYPES; i++) wr_byte(op_ptr->squelch_level[i]);

	/* Reset */
	flag16[0] = 0L;

	/* Analyze the options */
	for (i = 0; i < OPT_SQUELCH; i++)
	{
		int ob = i % 16;

		/* Process real entries */
		if (options_squelch[i].text)
		{
			if (op_ptr->opt_squelch[i]) flag16[0] |= (1L << ob);
		}
	}

	/* Dump the flags */
	wr_u16b(flag16[0]);

	/*** Window options ***/

	/* Reset */
	for (i = 0; i < ANGBAND_TERM_MAX; i++)	flag16[i] = op_ptr->window_flag[i];

	/* Dump the flags */
	for (i = 0; i < ANGBAND_TERM_MAX; i++) wr_u16b(flag16[i]);
}

/*
 * Write some "extra" info
 */
static void wr_extra(void)
{
	int i;

	wr_string(op_ptr->full_name);

	wr_string(p_ptr->died_from);

	for (i = 0; i < 4; i++)
	{
		wr_string(p_ptr->history[i]);
	}

	/* Race/Class/Gender */
	wr_byte(p_ptr->prace);
	wr_byte(p_ptr->pclass);
	wr_byte(p_ptr->psex);

	wr_byte(p_ptr->hitdie);
	wr_u16b(p_ptr->expfact);

	wr_s16b(p_ptr->age);
	wr_s16b(p_ptr->ht);
	wr_s16b(p_ptr->wt);

	/* Dump the stats (maximum and current) */
	for (i = 0; i < A_MAX; ++i) 
	{
		wr_byte(p_ptr->stat_max[i]);
		wr_byte(p_ptr->stat_cur[i]);
		wr_byte(p_ptr->stat_birth[i]);
	}

	wr_u16b(p_ptr->fame);

	wr_s32b(p_ptr->au);
	wr_s32b(p_ptr->au_birth);

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

	/* More info */
	wr_s16b(p_ptr->sc);

	wr_s16b(p_ptr->blind);
	wr_s16b(p_ptr->paralyzed);
	wr_s16b(p_ptr->confused);
	wr_s16b(p_ptr->food);
	wr_s16b(p_ptr->energy);
	wr_s16b(p_ptr->fast);
	wr_s16b(p_ptr->slow);
	wr_s16b(p_ptr->afraid);
	wr_s16b(p_ptr->cut);
	wr_s16b(p_ptr->stun);
	wr_s16b(p_ptr->poisoned);
	wr_s16b(p_ptr->diseased);
	wr_s16b(p_ptr->image);
	wr_s16b(p_ptr->protevil);
	wr_s16b(p_ptr->resilient);
	wr_s16b(p_ptr->absorb);
	wr_s16b(p_ptr->hero);
	wr_s16b(p_ptr->rage);
	wr_s16b(p_ptr->shield);
	wr_s16b(p_ptr->blessed);
	wr_s16b(p_ptr->tim_see_invis);
	wr_s16b(p_ptr->word_recall);
	wr_s16b(p_ptr->see_infra);
	wr_s16b(p_ptr->tim_infra);
	wr_s16b(p_ptr->tim_stealth);
	wr_s16b(p_ptr->tim_invis);
	wr_s16b(p_ptr->stability);
	wr_s16b(p_ptr->racial_power);

	/* Write resistances */
	for (i = 0; i < RS_MAX; i++) wr_s16b(p_ptr->tim_res[i]);

	wr_byte(p_ptr->searching);

	/* Write the "object seeds" */
	wr_u32b(seed_flavor);
	wr_u32b(seed_alchemy);
	wr_u32b(seed_town);

	/* Special stuff */
	wr_u16b(p_ptr->panic_save);
	wr_u16b(p_ptr->total_winner);
	wr_u16b(p_ptr->noscore);

	/* Write death */
	wr_byte(p_ptr->is_dead);

	/* Write feeling */
	wr_byte(p_ptr->feeling);

	/* Turn of last "feeling" */
	wr_s32b(p_ptr->feeling_cnt);

	/* Current turn */
	wr_s32b(turn);
}

/*
 * Write the spell info
 */
static void wr_spells(void)
{
	byte i;

	u16b tmp16u1 = 0; /* Number of legal spellbooks */
	u16b tmp16u2 = 0; /* Number of legal spells */

	for (i = 0; i < SV_BOOK_MAX; i++)
	{
		if (cp_ptr->spell_book[i]) 
		{
			tmp16u1++;
			tmp16u2 += count_spells(i);
		}
	}
	
	wr_u16b(tmp16u1); /* Number of legal spellbooks */
	wr_u16b(tmp16u2); /* Number of legal spells */

	for (i =0; i < SV_BOOK_MAX; i++)
	{
		if (cp_ptr->spell_book[i]) 
		{
			wr_byte(i); /* The book index */
			wr_u16b(p_ptr->spell_learned[i]);
			wr_u16b(p_ptr->spell_forgotten[i]);
		}
	}

	/* Dump the ordered spells */
	for (i = 0; i < tmp16u2; i++)
	{
		wr_byte(p_ptr->spell_order[i][0]);
		wr_byte(p_ptr->spell_order[i][1]);
	}
}

/*
 * The cave grid flags that get saved in the savefile
 */
#define IMPORTANT_FLAGS (CAVE_MARK | CAVE_GLOW | CAVE_ICKY | CAVE_ROOM)

/*
 * Write the current dungeon
 */
static void wr_dungeon(void)
{
	int i, j, y, x;

	byte tmp8u;

	byte count;
	byte prev_char;

	/*** Basic info ***/

	/* Dungeon specific info follows */
	wr_u16b(p_ptr->depth);
	wr_u16b(p_ptr->py);
	wr_u16b(p_ptr->px);
	wr_byte(p_ptr->cur_hgt);
	wr_byte(p_ptr->cur_wid);

	/*** Simple "Run-Length-Encoding" of cave ***/

	/* Note that this will induce two wasted bytes */
	count = 0;
	prev_char = 0;

	/* Dump the cave */
	for (y = 0; y < p_ptr->cur_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_wid; x++)
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
	for (y = 0; y < p_ptr->cur_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_wid; x++)
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

	/*** Dump room descriptions ***/
	for (x = 0; x < MAX_ROOMS_ROW; x++)
	{
		for (y = 0; y < MAX_ROOMS_COL; y++)
		{
			wr_byte(dun_room[x][y]);
		}
	}

	for (i = 1; i < DUN_ROOMS; i++)
	{
		wr_byte(room_info[i].type);
		tmp8u = 0;

		if (room_info[i].seen) tmp8u |= 0x01;
		wr_byte(tmp8u);

		if (room_info[i].type == ROOM_NORMAL)
		{
			for (j = 0; j < ROOM_DESC_SECTIONS; j++)
			{
				wr_s16b(room_info[i].section[j]);

				if (room_info[i].section[j] == -1) break;
			}
		}
	}
	
	/*** Compact ***/

	/* Compact the objects */
	compact_objects(0);

	/* Compact the monsters */
	compact_monsters(0);

	/* Compact the traps */
	compact_traps(0);

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
	wr_u16b(m_max);

	/* Dump the monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Dump it */
		wr_monster(m_ptr);
	}

	/*** Dump objects ***/

	/* Total objects */
	wr_u16b(t_max);

	/* Dump the objects */
	for (i = 1; i < t_max; i++)
	{
		trap_type *t_ptr = &t_list[i];

		/* Dump it */
		wr_trap(t_ptr);
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
	byte tmp8u;

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

	/* Write the RNG state */
	wr_randomizer();

	/* Write the boolean "options" */
	wr_options();

	/* Dump the number of "messages" */
	tmp16u = message_num();
	if (compress_savefile && (tmp16u > 40)) tmp16u = 40;
	wr_u16b(tmp16u);

	/* Dump the messages (oldest first!) */
	for (i = tmp16u - 1; i >= 0; i--)
	{
		wr_string(message_str((s16b)i));
		wr_u16b(message_type((s16b)i));
	}

	/* Dump the unique info */
	tmp16u = z_info->u_max;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		monster_unique *u_ptr = &u_info[i];

		tmp8u = 0;

		if (u_ptr->dead) tmp8u |= 0x01;
		wr_byte(tmp8u);

		wr_s16b(u_ptr->depth);
		wr_lore(TRUE, i);
	}

	/* Dump the monster lore */
	tmp16u = z_info->r_max;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++) wr_lore(FALSE, i);

	/* Dump the object memory */
	tmp16u = z_info->k_max;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++) wr_item_memory(i);

	/* Dump the alchemy info */
	tmp16u = SV_POTION_MAX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		tmp8u = 0;

		if (potion_alch[i].known1) tmp8u |= 0x01;
		if (potion_alch[i].known2) tmp8u |= 0x02;
		wr_byte(tmp8u);
	}

	/* Dump the quests */
	tmp16u = z_info->q_max;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		wr_byte(q_info[i].type);

		if (q_info[i].type == QUEST_FIXED)
		{
			wr_byte(q_info[i].active_level);
			wr_s16b(q_info[i].cur_num);
		}
		else if (q_info[i].type == QUEST_GUILD)
		{
			wr_byte(q_info[i].reward);
			wr_byte(q_info[i].active_level);
			wr_byte(q_info[i].base_level);

			wr_s16b(q_info[i].r_idx);

			wr_s16b(q_info[i].cur_num);
			wr_s16b(q_info[i].max_num);
			wr_byte(q_info[i].started);
		}
		else if (q_info[i].type == QUEST_VAULT)
		{
			wr_byte(q_info[i].reward);
			wr_byte(q_info[i].active_level);
			wr_byte(q_info[i].base_level);
		}
	}

	/* Hack -- Dump the artifacts */
	tmp16u = z_info->a_max;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		wr_byte(a_ptr->status);
	}

	/* Write the "extra" information */
	wr_extra();

	/* Dump the "player hp" entries */
	tmp16u = PY_MAX_LEVEL;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		wr_s16b(p_ptr->player_hp[i]);
	}

	/* Write the spell information */
	wr_spells();
  
	/* Write the inventory */
	for (i = 0; i < INVEN_MAX; i++)
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

	/* Player is not dead, write the dungeon */
	if (!p_ptr->is_dead) wr_dungeon();

	/* Write the "value check-sum" */
	wr_u32b(v_stamp);

	/* Write the "encoded checksum" */
	wr_u32b(x_stamp);

	/* Error in save */
	if (ferror(fff) || (fflush(fff) == EOF)) return FALSE;

	/* Successful save */
	return TRUE;
}

/*
 * Medium level player saver
 */
static bool save_player_aux(char *name)
{
	bool ok = FALSE;

	int fd;

	int mode = 0644;

	/* No file yet */
	fff = NULL;

	/* File type is "SAVE" */
	FILE_TYPE(FILE_TYPE_SAVE);

	/* Grab permissions */
	safe_setuid_grab();

 	/* Create the savefile */
 	fd = fd_make(name, mode);
 
	/* Drop permissions */
	safe_setuid_drop();

	/* File is okay */
	if (fd >= 0)
	{
		/* Close the "fd" */
		fd_close(fd);

		/* Grab permissions */
		safe_setuid_grab();

 		/* Open the savefile */
 		fff = my_fopen(name, "wb");
 
		/* Drop permissions */
		safe_setuid_drop();

		/* Successful open */
		if (fff)
		{
			/* Write the savefile */
			if (wr_savefile_new()) ok = TRUE;

			/* Attempt to close it */
			if (my_fclose(fff)) ok = FALSE;
		}

		/* Grab permissions */
		safe_setuid_grab();

 		/* Remove "broken" files */
 		if (!ok) fd_kill(name);

		/* Drop permissions */
		safe_setuid_drop();
	}

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

#ifdef SET_UID

# ifdef SECURE

	/* Get "games" permissions */
	beGames();

# endif

#endif

	/* New savefile */
	strcpy(safe, savefile);
	strcat(safe, ".new");

#ifdef VM
	/* Hack -- support "flat directory" usage on VM/ESA */
	strcpy(safe, savefile);
	strcat(safe, "n");
#endif /* VM */

	/* Remove it */
	fd_kill(safe);

	/* Attempt to save the player */
	if (save_player_aux(safe))
	{
		char temp[1024];

		/* Old savefile */
		strcpy(temp, savefile);
		strcat(temp, ".old");

#ifdef VM
		/* Hack -- support "flat directory" usage on VM/ESA */
		strcpy(temp, savefile);
		strcat(temp, "o");
#endif /* VM */

		/* Grab permissions */
		safe_setuid_grab();

		/* Remove it */
		fd_kill(temp);

		/* Preserve old savefile */
		fd_move(savefile, temp);

		/* Activate new savefile */
		fd_move(safe, savefile);

		/* Remove preserved savefile */
		fd_kill(temp);

		/* Drop permissions */
		safe_setuid_drop();

		/* Hack -- Pretend the character was loaded */
		character_loaded = TRUE;

		/* Success */
		result = TRUE;
	}

#ifdef SET_UID

# ifdef SECURE

	/* Drop "games" permissions */
	bePlayer();

# endif

#endif

	/* Return the result */
	return (result);
}

/*
 * Attempt to Load a "savefile"
 *
 * On multi-user systems, you may only "read" a savefile if you will be
 * allowed to "write" it later, this prevents painful situations in which
 * the player loads a savefile belonging to someone else, and then is not
 * allowed to save his game when he quits.
 *
 * We return "TRUE" if the savefile was usable, and we set the global
 * flag "character_loaded" if a real, living, character was loaded.
 *
 * Note that we always try to load the "current" savefile, even if
 * there is no such file, so we must check for "empty" savefile names.
 */
bool load_player(void)
{
	int fd = -1;

	errr err = 0;

	byte vvv[4];

	cptr what = "generic";

	/* Paranoia */
	turn = 0;

	/* Paranoia */
	p_ptr->is_dead = FALSE;

	/* Allow empty savefile name */
	if (!savefile[0]) return (TRUE);

	/* Grab permissions */
	safe_setuid_grab();
 
	/* Open the savefile */
	fd = fd_open(savefile, O_RDONLY);
 
	/* Drop permissions */
	safe_setuid_drop();
 
	/* No file */
	if (fd < 0)

	{
		/* Give a message */
		message(MSG_GENERIC, 0, "Savefile does not exist.");
		message_flush();

		/* Allow this */
		return (TRUE);
	}

	/* Close the file */
	fd_close(fd);

	/* Okay */
	if (!err)
	{
		/* Grab permissions */
		safe_setuid_grab();

 		/* Open the savefile */
 		fd = fd_open(savefile, O_RDONLY);
 
		/* Drop permissions */
		safe_setuid_drop();

		/* No file */
		if (fd < 0) err = -1;

		/* Message (below) */
		if (err) what = "Cannot open savefile";
	}

	/* Process file */
	if (!err)
	{
		/* Read the first four bytes */
		if (fd_read(fd, (char*)(vvv), 4)) err = -1;

		/* What */
		if (err) what = "Cannot read savefile";

		/* Close the file */
		fd_close(fd);
	}

	/* Process file */
	if (!err)
	{
		/* Extract version */
		sf_major = vvv[0];
		sf_minor = vvv[1];
		sf_patch = vvv[2];
		sf_extra = vvv[3];

		/* Clear screen */
		Term_clear();

		err = rd_savefile_new();

		/* Message (below) */
		if (err) what = "Cannot parse savefile";
	}

	/* Paranoia */
	if (!err)
	{
		/* Invalid turn */
		if (!turn) err = -1;

		/* Message (below) */
		if (err) what = "Broken savefile";
	}

	/* Okay */
	if (!err)
	{
		/* Give a conversion warning */
		if ((version_major != sf_major) ||
		    (version_minor != sf_minor) ||
		    (version_patch != sf_patch))
		{
			/* Message */
			message_format(MSG_GENERIC, 0, "Converted a %d.%d.%d savefile.",
			           sf_major, sf_minor, sf_patch);
			message_flush();
		}

		/* Player is dead */
		if (p_ptr->is_dead)
		{
			/* In wizard mode, allow loading of tombstones */
			if (cheat_wizard)
			{
				/* A character was loaded */
				character_loaded = TRUE;

				/* Done */
				return (TRUE);
			}

			/* Forget death */
			p_ptr->is_dead = FALSE;

			/* Count lives */
			sf_lives++;

			/* Forget turns */
			turn = 0;
			
			/* Reset feeling counter */
			p_ptr->feeling_cnt = 0;

			/* A character once existed on this savefile */
			character_existed = TRUE;

			/* Done */
			return (TRUE);
		}

		/* A character was loaded */
		character_loaded = TRUE;

		/* Still alive */
		if (p_ptr->chp >= 0)
		{
			/* Reset cause of death */
			strcpy(p_ptr->died_from, "(alive and well)");
		}

		/* Success */
		return (TRUE);
	}

	/* Message */
		message_format(MSG_GENERIC, 0, "Error (%s) reading %d.%d.%d savefile.",
			what, sf_major, sf_minor, sf_patch);	message_flush();

	/* Oops */
	return (FALSE);
}

