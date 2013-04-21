/* File: save.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


#ifdef FUTURE_SAVEFILES

/*
 * XXX XXX XXX Ignore this for now...
 *
 * The basic format of Angband 2.8.0 (and later) savefiles is simple.
 *
 * The savefile itself is a "header" (4 bytes) plus a series of "blocks",
 * plus, perhaps, some form of "footer" at the end.
 *
 * The "header" contains information about the "version" of the savefile.
 * Conveniently, pre-2.8.0 savefiles also use a 4 byte header, though the
 * interpretation of the "sf_extra" byte is very different.  Unfortunately,
 * savefiles from Angband 2.5.X reverse the sf_major and sf_minor fields,
 * and must be handled specially, until we decide to start ignoring them.
 *
 * Each "block" is a "type" (2 bytes), plus a "size" (2 bytes), plus "data",
 * plus a "check" (2 bytes), plus a "stamp" (2 bytes).  The format of the
 * "check" and "stamp" bytes is still being contemplated, but it would be
 * nice for one to be a simple byte-checksum, and the other to be a complex
 * additive checksum of some kind.  Both should be zero if the block is empty.
 *
 * Standard types:
 *   TYPE_BIRTH --> creation info
 *   TYPE_OPTIONS --> option settings
 *   TYPE_MESSAGES --> message recall
 *   TYPE_PLAYER --> player information
 *   TYPE_SPELLS --> spell information
 *   TYPE_INVEN --> player inven/equip
 *   TYPE_STORES --> store information
 *   TYPE_RACES --> monster race data
 *   TYPE_KINDS --> object kind data
 *   TYPE_UNIQUES --> unique info
 *   TYPE_ARTIFACTS --> artifact info
 *   TYPE_QUESTS --> quest info
 *
 * Dungeon information:
 *   TYPE_DUNGEON --> dungeon info
 *   TYPE_FEATURES --> dungeon features
 *   TYPE_OBJECTS --> dungeon objects
 *   TYPE_MONSTERS --> dungeon monsters
 *
 * Conversions:
 *   Break old "races" into normals/uniques
 *   Extract info about the "unique" monsters
 *
 * Question:
 *   Should there be a single "block" for info about all the stores, or one
 *   "block" for each store?  Or one "block", which contains "sub-blocks" of
 *   some kind?  Should we dump every "sub-block", or just the "useful" ones?
 *
 * Question:
 *   Should the normals/uniques be broken for 2.8.0, or should 2.8.0 simply
 *   be a "fixed point" into which older savefiles are converted, and then
 *   future versions could ignore older savefiles, and the "conversions"
 *   would be much simpler.
 */


/*
 * XXX XXX XXX
 */
#define TYPE_OPTIONS 17362


/*
 * Hack -- current savefile
 */
static int data_fd = -1;


/*
 * Hack -- current block type
 */
static u16b data_type;

/*
 * Hack -- current block size
 */
static u16b data_size;

/*
 * Hack -- pointer to the data buffer
 */
static byte *data_head;

/*
 * Hack -- pointer into the data buffer
 */
static byte *data_next;



/*
 * Hack -- write the current "block" to the savefile
 */
static errr wr_block(void)
{
	errr err;

	byte fake[4];

	/* Save the type and size */
	fake[0] = (byte) (data_type);
	fake[1] = (byte) (data_type >> 8);
	fake[2] = (byte) (data_size);
	fake[3] = (byte) (data_size >> 8);

	/* Dump the head */
	err = fd_write(data_fd, (char *) &fake, 4);

	/* Dump the actual data */
	err = fd_write(data_fd, (char *) data_head, data_size);

	/* XXX XXX XXX */
	fake[0] = 0;
	fake[1] = 0;
	fake[2] = 0;
	fake[3] = 0;

	/* Dump the tail */
	err = fd_write(data_fd, (char *) &fake, 4);

	/* Hack -- reset */
	data_next = data_head;

	/* Wipe the data block */
	C_WIPE(data_head, 65535, byte);

	/* Success */
	return (0);
}



/*
 * Hack -- add data to the current block
 */
static void put_byte(byte v)
{
	*data_next++ = v;
}

/*
 * Hack -- add data to the current block
 */
static void put_char(char v)
{
	put_byte((byte) (v));
}

/*
 * Hack -- add data to the current block
 */
static void put_u16b(u16b v)
{
	*data_next++ = (byte) (v);
	*data_next++ = (byte) (v >> 8);
}

/*
 * Hack -- add data to the current block
 */
static void put_s16b(s16b v)
{
	put_u16b((u16b) (v));
}

/*
 * Hack -- add data to the current block
 */
static void put_u32b(u32b v)
{
	*data_next++ = (byte) (v);
	*data_next++ = (byte) (v >> 8);
	*data_next++ = (byte) (v >> 16);
	*data_next++ = (byte) (v >> 24);
}

/*
 * Hack -- add data to the current block
 */
static void put_s32b(s32b v)
{
	put_u32b((u32b) (v));
}

/*
 * Hack -- add data to the current block
 */
static void put_string(char *str)
{
	while ((*data_next++ = *str++) != '\0');
}




/*
 * Hack -- read the next "block" from the savefile
 */
static errr rd_block(void)
{
	errr err;

	byte fake[4];

	/* Read the head data */
	err = fd_read(data_fd, (char *) &fake, 4);

	/* Extract the type and size */
	data_type = (fake[0] | ((u16b) fake[1] << 8));
	data_size = (fake[2] | ((u16b) fake[3] << 8));

	/* Wipe the data block */
	C_WIPE(data_head, 65535, byte);

	/* Read the actual data */
	err = fd_read(data_fd, (char *) data_head, data_size);

	/* Read the tail data */
	err = fd_read(data_fd, (char *) &fake, 4);

	/* XXX XXX XXX Verify */

	/* Hack -- reset */
	data_next = data_head;

	/* Success */
	return (0);
}


/*
 * Hack -- get data from the current block
 */
static void get_byte(byte * ip)
{
	byte d1;
	d1 = (*data_next++);
	(*ip) = (d1);
}

/*
 * Hack -- get data from the current block
 */
static void get_char(char *ip)
{
	get_byte((byte *) ip);
}

/*
 * Hack -- get data from the current block
 */
static void get_u16b(u16b * ip)
{
	u16b d0, d1;
	d0 = (*data_next++);
	d1 = (*data_next++);
	(*ip) = (d0 | (d1 << 8));
}

/*
 * Hack -- get data from the current block
 */
static void get_s16b(s16b * ip)
{
	get_u16b((u16b *) ip);
}

/*
 * Hack -- get data from the current block
 */
static void get_u32b(u32b * ip)
{
	u32b d0, d1, d2, d3;
	d0 = (*data_next++);
	d1 = (*data_next++);
	d2 = (*data_next++);
	d3 = (*data_next++);
	(*ip) = (d0 | (d1 << 8) | (d2 << 16) | (d3 << 24));
}

/*
 * Hack -- get data from the current block
 */
static void get_s32b(s32b * ip)
{
	get_u32b((u32b *) ip);
}



/*
 * Read a savefile.
 */
static errr rd_savefile(void)
{
	bool done = FALSE;

	byte fake[4];


	/* Open the savefile */
	data_fd = fd_open(savefile, O_RDONLY);

	/* No file */
	if (data_fd < 0)
		return (1);

	/* Strip the first four bytes (see below) */
	if (fd_read(data_fd, (char *) (fake), 4))
		return (1);


	/* Make array XXX XXX XXX */
	C_MAKE(data_head, 65535, byte);

	/* Hack -- reset */
	data_next = data_head;


	/* Read blocks */
	while (!done)
	{
		/* Read the block */
		if (rd_block())
			break;

		/* Analyze the type */
		switch (data_type)
		{
				/* Done XXX XXX XXX */
			case 0:
			{
				done = TRUE;
				break;
			}

				/* Grab the options */
			case TYPE_OPTIONS:
			{
				if (get_options())
					err = -1;
				break;
			}
		}

		/* XXX XXX XXX verify "data_next" */
		if (data_next - data_head > data_size)
			break;
	}


	/* XXX XXX XXX Check for errors */


	/* Kill array XXX XXX XXX */
	C_KILL(data_head, 65535, byte);


	/* Success */
	return (0);
}


#endif /* FUTURE_SAVEFILES */




/*
 * Some "local" parameters, used to help write savefiles
 */

static FILE *fff; /* Current save "file" */

static byte xor_byte; /* Simple encryption */

static u32b v_stamp = 0L; /* A simple "checksum" on the actual values */
static u32b x_stamp = 0L; /* A simple "checksum" on the encoded bytes */



/*
 * These functions place information into a savefile a byte at a time
 */

static void sf_put(byte v)
{
	/* Encode the value, write a character */
	xor_byte ^= v;
	(void) putc((int) xor_byte, fff);

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
	sf_put(v & 0xFF);
	sf_put((v >> 8) & 0xFF);
}

static void wr_s16b(s16b v)
{
	wr_u16b((u16b) v);
}

static void wr_u32b(u32b v)
{
	sf_put(v & 0xFF);
	sf_put((v >> 8) & 0xFF);
	sf_put((v >> 16) & 0xFF);
	sf_put((v >> 24) & 0xFF);
}

static void wr_s32b(s32b v)
{
	wr_u32b((u32b) v);
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
static void wr_item(object_type * o_ptr)
{
	wr_s16b(o_ptr->k_idx);

	wr_u32b(o_ptr->flags1);
	wr_u32b(o_ptr->flags2);
	wr_u32b(o_ptr->flags3);

	wr_byte(o_ptr->tval);
	wr_byte(o_ptr->sval);
	wr_s16b(o_ptr->pval);

	wr_byte(o_ptr->discount);
	wr_byte(o_ptr->number);
	wr_s16b(o_ptr->weight);
	wr_s16b(o_ptr->chp);
	wr_s16b(o_ptr->mhp);

	wr_byte(o_ptr->stuff);
	wr_byte(o_ptr->fate);

	wr_byte(o_ptr->name1);
	wr_byte(o_ptr->name2);
	wr_s16b(o_ptr->timeout);

	wr_s16b(o_ptr->to_h);
	wr_s16b(o_ptr->to_d);
	wr_s16b(o_ptr->to_a);
	wr_s16b(o_ptr->ac);
	wr_byte(o_ptr->dd);
	wr_byte(o_ptr->ds);

	wr_byte(o_ptr->ident);

	wr_byte(o_ptr->marked);

	wr_byte(o_ptr->tag);

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
static void wr_monster(monster_type * m_ptr)
{
	object_type *o_ptr;

	wr_s16b(m_ptr->r_idx);
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
	wr_byte(m_ptr->is_pet);
	wr_byte(m_ptr->fate);
	wr_s16b(m_ptr->random_name_idx);
	wr_s16b(m_ptr->mflag);

	for (o_ptr = m_ptr->inventory; o_ptr != NULL; o_ptr = o_ptr->next)
	{
		/* Paranoia: Don't write dead item */
		if (!o_ptr->k_idx) continue;

		/* Write the item */
		wr_item(o_ptr);
	}

	/* End-of-list marker. */
	wr_s16b(0);

}


/*
 * Write a "lore" record
 */
static void wr_lore(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Count sights/deaths/kills */
	wr_s16b(r_ptr->r_sights);
	wr_s16b(r_ptr->r_deaths);
	wr_s16b(r_ptr->r_pkills);
	wr_s16b(r_ptr->r_tkills);

	/* Count wakes and ignores */
	wr_byte(r_ptr->r_wake);
	wr_byte(r_ptr->r_ignore);

	/* Extra stuff */
	wr_byte(r_ptr->r_xtra1);
	wr_byte(r_ptr->r_xtra2);

	/* Count drops */
	wr_byte(r_ptr->r_drop_gold);
	wr_byte(r_ptr->r_drop_item);

	/* Count spells */
	wr_byte(r_ptr->r_cast_inate);
	wr_byte(r_ptr->r_cast_spell);

	/* Count blows of each type */
	wr_byte(r_ptr->r_blows[0]);
	wr_byte(r_ptr->r_blows[1]);
	wr_byte(r_ptr->r_blows[2]);
	wr_byte(r_ptr->r_blows[3]);

	/* Memorize flags */
	wr_u32b(r_ptr->r_flags1);
	wr_u32b(r_ptr->r_flags2);
	wr_u32b(r_ptr->r_flags3);
	wr_u32b(r_ptr->r_flags4);
	wr_u32b(r_ptr->r_flags5);
	wr_u32b(r_ptr->r_flags6);
	wr_u32b(r_ptr->r_flags7);


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

	if (k_ptr->aware)
		tmp8u |= 0x01;
	if (k_ptr->tried)
		tmp8u |= 0x02;

	wr_byte(tmp8u);
}


/*
 * Write a "store" record
 */
static void wr_store(store_type * st_ptr)
{
	object_type *o_ptr;

	/* Save the "open" counter */
	wr_u32b(st_ptr->store_open);

	/* Save the "insults" */
	wr_s16b(st_ptr->insult_cur);

	/* Save the current owner */
	wr_byte(st_ptr->owner);

	/* Save the "haggle" info */
	wr_s16b(st_ptr->good_buy);
	wr_s16b(st_ptr->bad_buy);

	/* Save the stock */
	for (o_ptr = st_ptr->stock; o_ptr != NULL; o_ptr = o_ptr->next_global)
	{
		/* Paranoia: Don't write dead item */
		if (!o_ptr->k_idx) continue;

		wr_item(o_ptr);
		wr_byte(o_ptr->iy);
		wr_byte(o_ptr->ix);
	}

	/* End-of-list. */
	wr_s16b(0);
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

	u16b c;

	u32b flag[8];
	u32b mask[8];


	/*** Special Options ***/

	/* Write "delay_factor" */
	wr_byte(op_ptr->delay_factor);

	/* Write "hitpoint_warn" */
	wr_byte(op_ptr->hitpoint_warn);


	/*** Cheating options ***/

	c = 0;

	if (p_ptr->wizard)
		c |= 0x0002;

	/* Save the cheating flags */
	for (i = 0; i < CHEAT_MAX; i++)
	{
		if (p_ptr->cheat[i])
			c |= (0x0100 << i);
	}

	wr_u16b(c);


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
		if (option_text[i])
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
	for (i = 0; i < 8; i++)
		wr_u32b(flag[i]);

	/* Dump the masks */
	for (i = 0; i < 8; i++)
		wr_u32b(mask[i]);


	/*** Window options ***/

	/* Reset */
	for (i = 0; i < 8; i++)
	{
		/* Flags */
		flag[i] = op_ptr->window_flag[i];

		/* Mask */
		mask[i] = 0L;

		/* Build the mask */
		for (k = 0; k < 32; k++)
		{
			/* Set mask */
			if (window_flag_desc[k])
			{
				mask[i] |= (1L << k);
			}
		}
	}

	/* Dump the flags */
	for (i = 0; i < 8; i++)
		wr_u32b(flag[i]);

	/* Dump the masks */
	for (i = 0; i < 8; i++)
		wr_u32b(mask[i]);
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

	/* Race/Class/Gender/Spells */
	wr_byte(p_ptr->prace);
	wr_byte(p_ptr->pclass);
	wr_byte(p_ptr->psex);
	wr_byte(p_ptr->prace_info);

	wr_byte(p_ptr->hitdie);
	wr_byte(p_ptr->expfact);

	wr_s16b(p_ptr->age);
	wr_s16b(p_ptr->ht);
	wr_s16b(p_ptr->wt);

	/* Dump the stats (maximum and current) */
	for (i = 0; i < 6; ++i)
		wr_s16b(p_ptr->stat_max[i]);
	for (i = 0; i < 6; ++i)
		wr_s16b(p_ptr->stat_cur[i]);

	wr_u32b(p_ptr->au);

	wr_u32b(p_ptr->max_exp);
	wr_u32b(p_ptr->exp);
	wr_u16b(p_ptr->exp_frac);
	wr_s16b(p_ptr->lev);

	/* Write arena and rewards information -KMW- */
	wr_byte(p_ptr->which_arena);
	wr_byte(p_ptr->which_quest);
	wr_s16b(p_ptr->which_town);
	wr_s16b(p_ptr->which_arena_layout);

	for (i = 0; i < MAX_ARENAS; i++)
		wr_s16b(p_ptr->arena_number[i]);
	for (i = 0; i < MAX_REWARDS; i++)
		wr_byte(rewards[i]);

	for (i = 0; i < MAX_BOUNTIES; i++)
	{
		wr_s16b(bounties[i][0]);
		wr_s16b(bounties[i][1]);
	}

	wr_byte(p_ptr->exit_bldg);
	wr_s16b(p_ptr->s_idx);
	wr_s16b(p_ptr->load_dungeon);

	wr_s16b(p_ptr->mhp);
	wr_s16b(p_ptr->chp);
	wr_u16b(p_ptr->chp_frac);

	wr_s32b(p_ptr->grace);
	wr_s32b(p_ptr->god_favor);
	wr_s32b(p_ptr->luck);
	wr_byte(p_ptr->pgod);

	wr_byte(p_ptr->number_pets);

	wr_byte(p_ptr->is_evil);

	wr_s16b(p_ptr->msp);
	wr_s16b(p_ptr->csp);
	wr_u16b(p_ptr->csp_frac);

	wr_s16b(p_ptr->msane);
	wr_s16b(p_ptr->csane);
	wr_u16b(p_ptr->csane_frac);

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
	wr_s16b(p_ptr->image);
	wr_s16b(p_ptr->protevil);
	wr_s16b(p_ptr->invuln);
	wr_s16b(p_ptr->hero);
	wr_s16b(p_ptr->shero);
	wr_s16b(p_ptr->shield);
	wr_s16b(p_ptr->blessed);
	wr_s16b(p_ptr->tim_invis);
	wr_s16b(p_ptr->word_recall);
	wr_s16b(p_ptr->see_infra);
	wr_s16b(p_ptr->tim_infra);
	wr_s16b(p_ptr->oppose_fire);
	wr_s16b(p_ptr->oppose_cold);
	wr_s16b(p_ptr->oppose_acid);
	wr_s16b(p_ptr->oppose_elec);
	wr_s16b(p_ptr->oppose_pois);
	wr_s16b(p_ptr->shape_timed);
	wr_byte(p_ptr->shape);
	wr_s16b(p_ptr->immov_cntr);

	wr_u32b(p_ptr->mutations1);
	wr_u32b(p_ptr->mutations2);
	wr_u32b(p_ptr->mutations3);

	wr_byte(p_ptr->confusing);
	wr_byte(p_ptr->searching);
	wr_byte(p_ptr->maximize);
	wr_byte(p_ptr->preserve);

	/* Future use */
	for (i = 0; i < 12; i++)
		wr_u32b(0L);

	/* Write the "object seeds" */
	wr_u32b(seed_flavor);
	wr_u32b(seed_town);
	wr_u32b(seed_dungeon);
	wr_u32b(seed_wild);


	/* Special stuff */
	wr_u16b(p_ptr->panic_save);
	wr_u16b(p_ptr->total_winner);
	wr_u16b(p_ptr->noscore);


	/* Write death */
	wr_byte(p_ptr->is_dead);

	/* Write feeling */
	wr_s16b(feeling);

	/* Turn of last "feeling" */
	wr_s32b(old_turn);

	/* Current turn */
	wr_s32b(turn);
}



/*
 * Write the current dungeon
 */
static void wr_dungeon(void)
{
	int i, y, x;

	byte tmp8u;

	byte count;
	byte prev_char;

	object_type *o_ptr;

	/*** Basic info ***/

	/* Dungeon specific info follows */
	wr_s16b(p_ptr->depth);
	wr_s16b(p_ptr->inside_special);
	wr_s16b(p_ptr->wild_y);
	wr_s16b(p_ptr->wild_x);
	wr_s16b(p_ptr->wilderness_py);
	wr_s16b(p_ptr->wilderness_px);
	wr_s16b(p_ptr->wilderness_depth);
	wr_s16b(p_ptr->py);
	wr_s16b(p_ptr->px);

	wr_s16b(DUNGEON_HGT);
	wr_s16b(DUNGEON_WID);


	/*** Simple "Run-Length-Encoding" of cave ***/

	/* Note that this will induce two wasted bytes */
	count = 0;
	prev_char = 0;

	/* Dump the cave */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			/* Extract a byte */
			tmp8u = cave_info[y][x];

			/* If the run is broken, or too full, flush it */
			if ((tmp8u != prev_char) || (count == MAX_UCHAR))
			{
				wr_byte((byte) count);
				wr_byte((byte) prev_char);
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
		wr_byte((byte) count);
		wr_byte((byte) prev_char);
	}


	/*** Simple "Run-Length-Encoding" of cave ***/

	/* Note that this will induce two wasted bytes */
	count = 0;
	prev_char = 0;

	/* Dump the cave */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			/* Extract a byte */
			tmp8u = cave_feat[y][x];

			/* If the run is broken, or too full, flush it */
			if ((tmp8u != prev_char) || (count == MAX_UCHAR))
			{
				wr_byte((byte) count);
				wr_byte((byte) prev_char);
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
		wr_byte((byte) count);
		wr_byte((byte) prev_char);
	}


	/*** Compact ***/

	/* Compact the monsters */
	compact_monsters(0);


	/*** Dump objects ***/

	/* Dump the objects */
	for (o_ptr = o_list; o_ptr != NULL; o_ptr = o_ptr->next_global)
	{
		/* Paranoia: Don't write dead item */
		if (!o_ptr->k_idx) continue;

		if (o_ptr->stack == STACK_FLOOR)
		{
			/* Dump it */
			wr_item(o_ptr);

			/* Write location. */
			wr_byte(o_ptr->iy);
			wr_byte(o_ptr->ix);
		}
	}

	/* Place end-of-list marker. */
	wr_s16b(0);

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
}


/*
 * Write out a spell.
 */

static void wr_spell(spell * s_ptr)
{
	proj_node *pnode = s_ptr->proj_list;
	int len = 0;

	wr_string(s_ptr->name);
	wr_string(s_ptr->desc);
	wr_byte(s_ptr->class);
	wr_byte(s_ptr->level);
	wr_byte(s_ptr->mana);
	wr_byte(s_ptr->untried);
	wr_byte(s_ptr->unknown);

	/* Figure out the length of the projectable list. */
	while (pnode)
	{
		len++;
		pnode = pnode->next;
	}

	wr_byte(len);

	/* Rewind to the beginning. */
	pnode = s_ptr->proj_list;

	while (pnode)
	{
		wr_u32b(pnode->proj_flags);
		wr_byte(pnode->safe);
		wr_byte(pnode->attack_kind);
		wr_byte(pnode->radius);
		wr_s16b(pnode->dam_dice);
		wr_s16b(pnode->dam_sides);

		pnode = pnode->next;
	}
}




/*
 * Actually write a save-file
 */
static bool wr_savefile_new(void)
{
	int i;

	u32b now;

	byte tmp8u;
	u16b tmp16u;

	object_type *o_ptr;


	/* Guess at the current time */
	now = time((time_t *) 0);


	/* Note the operating system */
	sf_xtra = 0L;

	/* Note when the file was saved */
	sf_when = now;

	/* Note the number of saves */
	sf_saves++;


	/*** Actually write the file ***/

	/* Dump the file header */
	xor_byte = 0;
	wr_byte(KAM_VERSION_MAJOR);
	xor_byte = 0;
	wr_byte(KAM_VERSION_MINOR);
	xor_byte = 0;
	wr_byte(KAM_VERSION_PATCH);
	xor_byte = 0;
	tmp8u = rand_int(256);
	wr_byte(tmp8u);


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
	tmp16u = message_num();
	if (compress_savefile && (tmp16u > 40))
		tmp16u = 40;
	wr_u16b(tmp16u);

	/* Dump the messages (oldest first!) */
	for (i = tmp16u - 1; i >= 0; i--)
	{
		wr_string(message_str(i));
		wr_byte(message_prior(i));
	}


	/* Dump the monster lore */
	tmp16u = MAX_R_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
		wr_lore(i);


	/* Dump the object memory */
	tmp16u = MAX_K_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
		wr_xtra(i);


	/* Hack -- Dump the quests -KMW- */
	tmp16u = MAX_QUESTS;
	wr_u16b(tmp16u);

	for (i = 0; i < tmp16u; i++)
	{
		wr_byte(quest_status[i]);
	}

	/* Dump the recipe recall. */
	tmp16u = MAX_RECIPES;
	wr_u16b(tmp16u);

	for (i = 0; i < tmp16u; i++)
	{
		wr_byte(recipe_recall[i]);
	}


	/* Dump the random artifacts */
	tmp16u = MAX_RANDARTS;
	wr_u16b(tmp16u);

	for (i = 0; i < tmp16u; i++)
	{
		random_artifact *ra_ptr = &random_artifacts[i];

		wr_string(ra_ptr->name_full);
		wr_string(ra_ptr->name_short);
		wr_byte(ra_ptr->level);
		wr_byte(ra_ptr->attr);
		wr_u32b(ra_ptr->cost);
		wr_s16b(ra_ptr->activation);
		wr_byte(ra_ptr->generated);
	}

	/* Dump the random spells. */

	tmp16u = MAX_SPELLS;
	wr_u16b(tmp16u);
	wr_u16b(spell_num);

	for (i = 0; i < spell_num; i++)
	{
		spell *rspell = &spells[i];

		wr_spell(rspell);
	}

	/* Hack -- Dump the artifacts */
	tmp16u = MAX_A_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		wr_byte(a_ptr->cur_num);
		wr_byte(0);
		wr_byte(0);
		wr_byte(0);
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


	/* Write the inventory */
	for (o_ptr = inventory; o_ptr != NULL; o_ptr = o_ptr->next)
	{
		bool foo = FALSE;

		/* Paranoia: Don't write dead item */
		if (!o_ptr->k_idx) continue;

		/* Dump object */
		wr_item(o_ptr);

		/* Hack -- Write equipment slot. */
		for (i = 0; i < EQUIP_MAX; i++)
		{
			if (equipment[i] == o_ptr)
			{
				wr_s16b(i);
				foo = TRUE;
			}
		}

		if (!foo)
		{
			wr_s16b(-1);
		}
	}

	/* Write end-of-list marker. */
	wr_s16b(0);

	/* Note the stores */
	tmp16u = MAX_STORES;
	wr_u16b(tmp16u);

	/* Dump the stores */
	for (i = 0; i < tmp16u; i++)
	{
		wr_store(&store[i]);
	}

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
	if (ferror(fff) || (fflush(fff) == EOF))
		return FALSE;

	/* Successful save */
	return TRUE;
}


/*
 * Medium level player saver
 *
 */
static bool save_player_aux(char *name)
{
	bool ok = FALSE;

	int fd = -1;

	int mode = 0644;


	/* No file yet */
	fff = NULL;


	/* File type is "SAVE" */
	FILE_TYPE(FILE_TYPE_SAVE);


	/* Create the savefile */
	fd = fd_make(name, mode);

	/* File is okay */
	if (fd >= 0)
	{
		/* Close the "fd" */
		fd_close(fd);

		/* Open the savefile */
		fff = my_fopen(name, "wb");

		/* Successful open */
		if (fff)
		{
			/* Write the savefile */
			if (wr_savefile_new())
				ok = TRUE;

			/* Attempt to close it */
			if (my_fclose(fff))
				ok = FALSE;
		}

		/* Remove "broken" files */
		if (!ok)
			fd_kill(name);
	}


	/* Failure */
	if (!ok)
		return (FALSE);

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

		/* Remove it */
		fd_kill(temp);

		/* Preserve old savefile */
		fd_move(savefile, temp);

		/* Activate new savefile */
		fd_move(safe, savefile);

		/* Remove preserved savefile */
		fd_kill(temp);

		/* Hack -- Pretend the character was loaded */
		character_loaded = TRUE;

#ifdef VERIFY_SAVEFILE

		/* Lock on savefile */
		strcpy(temp, savefile);
		strcat(temp, ".lok");

		/* Remove lock file */
		fd_kill(temp);

#endif

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
 */
bool load_player(void)
{
	int fd = -1;

	errr err = 0;

	byte vvv[4];

#ifdef VERIFY_TIMESTAMP
	struct stat statbuf;
#endif

	cptr what = "generic";


	/* Paranoia */
	turn = 0;

	/* Paranoia */
	p_ptr->is_dead = FALSE;


	/* Allow empty savefile name */
	if (!savefile[0])
		return (TRUE);


#if !defined(MACINTOSH) && !defined(WINDOWS) && !defined(VM)

	/* XXX XXX XXX Fix this */

	/* Verify the existance of the savefile */
	if (access(savefile, 0) < 0)
	{
		/* Give a message */
		msg_print("Savefile does not exist.");
		msg_print(NULL);

		/* Allow this */
		return (TRUE);
	}

#endif


#ifdef VERIFY_SAVEFILE

	/* Verify savefile usage */
	if (!err)
	{
		FILE *fkk;

		char temp[1024];

		/* Extract name of lock file */
		strcpy(temp, savefile);
		strcat(temp, ".lok");

		/* Check for lock */
		fkk = my_fopen(temp, "r");

		/* Oops, lock exists */
		if (fkk)
		{
			/* Close the file */
			my_fclose(fkk);

			/* Message */
			msg_print("Savefile is currently in use.");
			msg_print(NULL);

			/* Oops */
			return (FALSE);
		}

		/* Create a lock file */
		fkk = my_fopen(temp, "w");

		/* Dump a line of info */
		fprintf(fkk, "Lock file for savefile '%s'\n", savefile);

		/* Close the lock file */
		my_fclose(fkk);
	}

#endif


	/* Okay */
	if (!err)
	{
		/* Open the savefile */
		fd = fd_open(savefile, O_RDONLY);

		/* No file */
		if (fd < 0)
			err = -1;

		/* Message (below) */
		if (err)
			what = "Cannot open savefile";
	}

	/* Process file */
	if (!err)
	{

#ifdef VERIFY_TIMESTAMP
		/* Get the timestamp */
		(void) fstat(fd, &statbuf);
#endif

		/* Read the first four bytes */
		if (fd_read(fd, (char *) (vvv), 4))
			err = -1;

		/* What */
		if (err)
			what = "Cannot read savefile";

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

		/* Parse "new" savefiles */
		if (sf_major == version_major && sf_minor == version_minor)
		{
			/* Attempt to load */
			err = rd_savefile_new();
		}

		/* Parse "future" savefiles */
		else
		{
			/* Error XXX XXX XXX */
			err = -1;
		}

		/* Message (below) */
		if (err)
			what = "Cannot parse savefile";
	}

	/* Paranoia */
	if (!err)
	{
		/* Invalid turn */
		if (!turn)
			err = -1;

		/* Message (below) */
		if (err)
			what = "Broken savefile";
	}

#ifdef VERIFY_TIMESTAMP
	/* Verify timestamp */
	if (!err && !arg_wizard)
	{
		/* Hack -- Verify the timestamp */
		if (sf_when > (statbuf.st_ctime + 100) ||
			sf_when < (statbuf.st_ctime - 100))
		{
			/* Message */
			what = "Invalid timestamp";

			/* Oops */
			err = -1;
		}
	}
#endif


	/* Okay */
	if (!err)
	{
		/* Give a conversion warning */
	  if ((version_major != sf_major) || (version_minor != sf_minor) ||
	      (version_patch != sf_patch))
	    {
			/* Message */
			msg_format("Converted a %d.%d.%d savefile.", sf_major,
				sf_minor, sf_patch);
			msg_print(NULL);
		}

		/* Player is dead */
		if (p_ptr->is_dead)
		{
			/* Forget death */
			p_ptr->is_dead = FALSE;

			/* Cheat death */
			if (arg_wizard)
			{
				/* A character was loaded */
				character_loaded = TRUE;

				/* Done */
				return (TRUE);
			}

			/* Count lives */
			sf_lives++;

			/* Forget turns */
			turn = old_turn = 0;

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


#ifdef VERIFY_SAVEFILE

	/* Verify savefile usage */
	if (TRUE)
	{
		char temp[1024];

		/* Extract name of lock file */
		strcpy(temp, savefile);
		strcat(temp, ".lok");

		/* Remove lock */
		fd_kill(temp);
	}

#endif


	/* Message */
	msg_format("Error (%s) reading %d.%d.%d savefile.", what, sf_major,
		sf_minor, sf_patch);
	msg_print(NULL);

	/* Oops */
	return (FALSE);
}



/*
 * Attempt to save a temporary dungeon.
 */
bool save_dungeon(s16b tag)
{
	char temp[128];
	char path[1024];
	int fd = -1;
	int mode = 0644;

	/* Paranoia */
	if (tag > 999 || tag < 0)
		return TRUE;

	sprintf(temp, "%s.%d", op_ptr->base_name, tag);
	path_build(path, 1024, ANGBAND_DIR_SAVE, temp);

	/* File type is "SAVE" */
	FILE_TYPE(FILE_TYPE_SAVE);

	/* Delete the old file. */
	fd_kill(path);

	/* Create the savefile */
	fd = fd_make(path, mode);

	/* File is okay */
	if (fd < 0)
		return TRUE;

	/* Close the "fd" */
	fd_close(fd);

	/* Open the savefile */
	fff = my_fopen(path, "wb");

	/* Successful open */
	if (!fff)
		return TRUE;

	xor_byte = 0;

	/* Write the savefile */
	wr_dungeon();

	if (ferror(fff) || (fflush(fff) == EOF))
	{
		fd_kill(path);
		return TRUE;
	}

	/* Attempt to close it */
	if (my_fclose(fff))
	{
		fd_kill(path);
		return TRUE;
	}

	return FALSE;
}
