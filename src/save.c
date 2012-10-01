/* File: save.c */

/* Creation of savefiles.  Loading a player from a savefile.
 *
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
	fake[0] = (byte)(data_type);
	fake[1] = (byte)(data_type >> 8);
	fake[2] = (byte)(data_size);
	fake[3] = (byte)(data_size >> 8);

	/* Dump the head */
	err = fd_write(data_fd, (char*)&fake, 4);

	/* Dump the actual data */
	err = fd_write(data_fd, (char*)data_head, data_size);

	/* XXX XXX XXX */
	fake[0] = 0;
	fake[1] = 0;
	fake[2] = 0;
	fake[3] = 0;

	/* Dump the tail */
	err = fd_write(data_fd, (char*)&fake, 4);

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
	put_byte((byte)(v));
}

/*
 * Hack -- add data to the current block
 */
static void put_u16b(u16b v)
{
	*data_next++ = (byte)(v);
	*data_next++ = (byte)(v >> 8);
}

/*
 * Hack -- add data to the current block
 */
static void put_s16b(s16b v)
{
	put_u16b((u16b)(v));
}

/*
 * Hack -- add data to the current block
 */
static void put_u32b(u32b v)
{
	*data_next++ = (byte)(v);
	*data_next++ = (byte)(v >> 8);
	*data_next++ = (byte)(v >> 16);
	*data_next++ = (byte)(v >> 24);
}

/*
 * Hack -- add data to the current block
 */
static void put_s32b(s32b v)
{
	put_u32b((u32b)(v));
}

/*
 * Hack -- add data to the current block
 */
static void put_string(char *str)
{
	while ((*data_next++ = *str++) != '\0');
}



/*
 * Write a savefile for Angband 2.8.0
 */
static errr wr_savefile(void)
{
	int i;

	u32b now;

	byte tmp8u;
	u16b tmp16u;

	errr err;

	byte fake[4];


	/*** Hack -- extract some data ***/

	/* Hack -- Acquire the current time */
	now = time((time_t*)(NULL));

	/* Note the operating system */
	sf_xtra = 0L;

	/* Note when the file was saved */
	sf_when = now;

	/* Note the number of saves */
	sf_saves++;


	/*** Actually write the file ***/

	/* Open the file XXX XXX XXX */
	data_fd = -1;

	/* Dump the version */
	fake[0] = (byte)(VERSION_MAJOR);
	fake[1] = (byte)(VERSION_MINOR);
	fake[2] = (byte)(VERSION_PATCH);
	fake[3] = (byte)(VERSION_EXTRA);

	/* Dump the data */
	err = fd_write(data_fd, (char*)&fake, 4);


	/* Make array XXX XXX XXX */
	C_MAKE(data_head, 65535, byte);

	/* Hack -- reset */
	data_next = data_head;


#if 0
	/* Operating system */
	wr_u32b(sf_xtra);

	/* Time file last saved */
	wr_u32b(sf_when);

	/* Number of past lives */
	wr_u16b(sf_lives);

	/* Number of times saved */
	wr_u16b(sf_saves);

	/* XXX XXX XXX */

	/* Set the type */
	data_type = TYPE_BASIC;

	/* Set the "options" size */
	data_size = (data_next - data_head);

	/* Write the block */
	wr_block();
#endif


	/* Dump the "options" */
	put_options();

	/* Set the type */
	data_type = TYPE_OPTIONS;

	/* Set the "options" size */
	data_size = (data_next - data_head);

	/* Write the block */
	wr_block();

	/* XXX XXX XXX */

#if 0

	/* Dump the "messages" */

	/* Dump the number of "messages" */
	tmp16u = message_num();
	if (compress_savefile && (tmp16u > 40)) tmp16u = 40;
	wr_u16b(tmp16u);

	/* Dump the messages and types (oldest first!) */
	for (i = tmp16u - 1; i >= 0; i--)
	{
		wr_string(message_str(i));
		wr_u16b(message_type(i));
	}

	/* Dump the monster lore */
	tmp16u = MAX_R_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++) wr_lore(i);


	/* Dump the object memory */
	tmp16u = MAX_K_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++) wr_xtra(i);


	/* Hack -- Dump the quests */
	tmp16u = MAX_Q_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		wr_byte(q_list[i].level);
		wr_byte(0);
		wr_byte(0);
		wr_byte(0);
	}

	/* Hack -- Dump the artifacts */
	tmp16u = MAX_A_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		wr_byte(a_ptr->creat_stat);
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
		wr_s16b(player_hp[i]);
	}


	/* Write spell data */
	wr_u32b(0);
	wr_u32b(0);
	wr_u32b(spell_worked1);
	wr_u32b(spell_worked2);
	wr_u32b(0);
	wr_u32b(0);

	/* Was: Dump the ordered spells */
	for (i = 0; i < 64; i++)
	{
		wr_byte(0);
	}


	/* Write the inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Dump index */
		wr_u16b((s16b)i);

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
	if (!p_ptr->is_dead)
	{
		/* Dump the dungeon */
		wr_dungeon();

		/* Dump the ghost */
		wr_ghost();
	}

#endif

	/* Dump the "final" marker XXX XXX XXX */
	/* Type zero, Size zero, Contents zero, etc */


	/* XXX XXX XXX Check for errors */


	/* Kill array XXX XXX XXX */
	C_KILL(data_head, 65535, byte);


	/* Success */
	return (0);
}





/*
 * Hack -- read the next "block" from the savefile
 */
static errr rd_block(void)
{
	errr err;

	byte fake[4];

	/* Read the head data */
	err = fd_read(data_fd, (char*)&fake, 4);

	/* Extract the type and size */
	data_type = (fake[0] | ((u16b)fake[1] << 8));
	data_size = (fake[2] | ((u16b)fake[3] << 8));

	/* Wipe the data block */
	C_WIPE(data_head, 65535, byte);

	/* Read the actual data */
	err = fd_read(data_fd, (char*)data_head, data_size);

	/* Read the tail data */
	err = fd_read(data_fd, (char*)&fake, 4);

	/* XXX XXX XXX Verify */

	/* Hack -- reset */
	data_next = data_head;

	/* Success */
	return (0);
}


/*
 * Hack -- get data from the current block
 */
static void get_byte(byte *ip)
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
	get_byte((byte*)ip);
}

/*
 * Hack -- get data from the current block
 */
static void get_u16b(u16b *ip)
{
	u16b d0, d1;
	d0 = (*data_next++);
	d1 = (*data_next++);
	(*ip) = (d0 | (d1 << 8));
}

/*
 * Hack -- get data from the current block
 */
static void get_s16b(s16b *ip)
{
	get_u16b((u16b*)ip);
}

/*
 * Hack -- get data from the current block
 */
static void get_u32b(u32b *ip)
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
static void get_s32b(s32b *ip)
{
	get_u32b((u32b*)ip);
}



/*
 * Read a savefile for Angband 2.8.0
 */
static errr rd_savefile(void)
{
	bool done = FALSE;

	byte fake[4];


	/* Open the savefile */
	data_fd = fd_open(savefile, O_RDONLY);

	/* No file */
	if (data_fd < 0) return (1);

	/* Strip the first four bytes (see below) */
	if (fd_read(data_fd, (char*)(fake), 4)) return (1);


	/* Make array XXX XXX XXX */
	C_MAKE(data_head, 65535, byte);

	/* Hack -- reset */
	data_next = data_head;


	/* Read blocks */
	while (!done)
	{
		/* Read the block */
		if (rd_block()) break;

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
				if (get_options()) err = -1;
				break;
			}
		}

		/* XXX XXX XXX verify "data_next" */
		if (data_next - data_head > data_size) break;
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

	/* Old flags */
	wr_u32b(0L);
	wr_u32b(0L);
	wr_u32b(0L);

	/* Held by monster index */
	wr_s16b(o_ptr->held_m_idx);

	/* Extra information */
	wr_byte(o_ptr->xtra1);
	wr_byte(o_ptr->xtra2);

	/* Feeling */
	wr_byte(o_ptr->feel);

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
	wr_byte(m_ptr->stasis);

	wr_byte(m_ptr->black_breath);

	wr_u32b(m_ptr->smart); /* Flags for 'smart-learn' */

	/* Dummy writes for features soon to be implemented */
	wr_byte(0);
	wr_byte(0);
	wr_byte(0);
	wr_byte(0);

	/* Extra desire to cast harassment spells */
	wr_byte(m_ptr->harass);

	/* Current Mana */
	wr_byte(m_ptr->mana);

	/* Spare */
	wr_s16b(0);
}


/*
 * Write a "lore" record
 */
static void wr_lore(int r_idx)
{
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
	wr_byte(l_ptr->cast_inate);
	wr_byte(l_ptr->cast_spell);

	/* Count blows of each type */
	wr_byte(l_ptr->blows[0]);
	wr_byte(l_ptr->blows[1]);
	wr_byte(l_ptr->blows[2]);
	wr_byte(l_ptr->blows[3]);

	/* Memorize flags */
	wr_u32b(l_ptr->flags1);
	wr_u32b(l_ptr->flags2);
	wr_u32b(l_ptr->flags3);
	wr_u32b(l_ptr->flags4);
	wr_u32b(l_ptr->flags5);
	wr_u32b(l_ptr->flags6);
	wr_u32b(l_ptr->flags7);

	/* Monster limit per level */
	wr_byte(r_ptr->max_num);
	wr_byte(0);
	wr_byte(0);
	wr_byte(0);
}


/*
 * Write an "xtra" record.  Records knowledge of object kinds.
 */
static void wr_xtra(int k_idx)
{
	byte tmp8u = 0;

	object_kind *k_ptr = &k_info[k_idx];

	if (k_ptr->aware) tmp8u |= 0x01;
	if (k_ptr->tried) tmp8u |= 0x02;
	if (k_ptr->known_effect) tmp8u |= 0x04;
	if (k_ptr->squelch) tmp8u |= 0x08;

	wr_byte(tmp8u);
}


/*
 * Write a "store" record
 */
static void wr_store(store_type *st_ptr)
{
	int j;

	/* Save the "open" counter */
	wr_u32b(st_ptr->store_open);

	/* Save the "insults" */
	wr_s16b(st_ptr->insult_cur);

	/* Save the current owner*/
	wr_byte(st_ptr->owner);

	/* Save the stock size */
	wr_byte((byte)st_ptr->stock_num);

	/* Save the "haggle" info */
	wr_s16b(st_ptr->good_buy);
	wr_s16b(st_ptr->bad_buy);

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


	/*** Oops ***/

	/* Once contained options.  Reduced from four to three and 1/4
	 * longints in Oangband. */
	for (i = 0; i < 3; i++) wr_u32b(0L);
	wr_byte(0);


	/*** Timed Autosave (inspired by Zangband) ***/
	wr_byte(autosave);
	wr_s16b(autosave_freq);


	/*** Special Options ***/

	/* Write "delay_factor" */
	wr_byte((byte)op_ptr->delay_factor);

	/* Write "hitpoint_warn" */
	wr_byte((byte)op_ptr->hitpoint_warn);

	wr_u16b(0);     /* Was cheating options */


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
	for (i = 0; i < 8; i++) wr_u32b(flag[i]);

	/* Dump the masks */
	for (i = 0; i < 8; i++) wr_u32b(mask[i]);


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
	for (i = 0; i < 8; i++) wr_u32b(flag[i]);

	/* Dump the masks */
	for (i = 0; i < 8; i++) wr_u32b(mask[i]);
}


/*
 * Hack -- Write the "ghost" info
 */
static void wr_ghost(void)
{
	int i;

	/* Name */
	wr_string("Broken Ghost");

	/* Hack -- stupid data */
	for (i = 0; i < 60; i++) wr_byte(0);
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
	wr_byte(0);	/* oops */

	wr_byte(p_ptr->hitdie);
	wr_byte(0);

	wr_s16b(p_ptr->age);
	wr_s16b(p_ptr->ht);
	wr_s16b(p_ptr->wt);

	/* Dump the stats (maximum and current) */
	for (i = 0; i < A_MAX; ++i) wr_s16b(p_ptr->stat_max[i]);
	for (i = 0; i < A_MAX; ++i) wr_s16b(p_ptr->stat_cur[i]);

	/* Ignore the transient stats */
	for (i = 0; i < 12; ++i) wr_s16b(0);

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

	/* More info */
	wr_s16b(p_ptr->speed_boost);	/* Specialty Fury */
	wr_s16b(p_ptr->heighten_power);	/* Specialty Heighten Magic */
	wr_byte(p_ptr->attune_tval);	/* Specialty Attunement */
	wr_byte(p_ptr->attune_sval);	/* Specialty Attunement */
	wr_s16b(0);	/* oops */
	wr_s16b(p_ptr->sc);
	wr_s16b(0);	/* oops */

	wr_s16b(0);		/* old "rest" */
	wr_s16b(p_ptr->blind);
	wr_s16b(p_ptr->paralyzed);
	wr_s16b(p_ptr->confused);
	wr_s16b(p_ptr->food);
	wr_s16b(0);	/* old "food_digested" */
	wr_s16b(0);	/* old "protection" */
	wr_s16b(p_ptr->energy);
	wr_s16b(p_ptr->fast);
	wr_s16b(p_ptr->slow);
	wr_s16b(p_ptr->afraid);
	wr_s16b(p_ptr->cut);
	wr_s16b(p_ptr->stun);
	wr_s16b(p_ptr->poisoned);
	wr_s16b(p_ptr->image);
	wr_s16b(p_ptr->protevil);
	wr_s16b(p_ptr->magicdef);
	wr_s16b(p_ptr->hero);
	wr_s16b(p_ptr->shero);
	wr_s16b(p_ptr->shield);
	wr_s16b(p_ptr->blessed);
	wr_s16b(p_ptr->tim_invis);
	wr_s16b(p_ptr->tim_esp);
	wr_s16b(p_ptr->superstealth);
	wr_s16b(p_ptr->ele_attack);
	wr_s16b(p_ptr->word_recall);
	wr_s16b(p_ptr->see_infra);
	wr_s16b(p_ptr->tim_infra);
	wr_s16b(p_ptr->oppose_fire);
	wr_s16b(p_ptr->oppose_cold);
	wr_s16b(p_ptr->oppose_acid);
	wr_s16b(p_ptr->oppose_elec);
	wr_s16b(p_ptr->oppose_pois);

	wr_u32b(p_ptr->special_attack); /* Attack modifier flags. */
	wr_byte(0);	/* oops */
	wr_byte(0);	/* oops */
	wr_byte(p_ptr->black_breath);	/* Now used to store Black Breath. */
	wr_byte(p_ptr->searching);
	wr_byte(0);	/* formerly maximize */
	wr_byte(0);	/* formerly preserve */
	wr_byte(p_ptr->schange); /* Now used to store shapechange. */

	/* Store the bones file selector, if the player is not dead. -LM- */
	if (!(p_ptr->is_dead)) wr_byte(bones_selector);
	else wr_byte(0);

	/* Store the number of thefts on the level. -LM- */
	wr_byte(number_of_thefts_on_level);

	/* Store number of monster traps on this level. -LM- */
	wr_byte(num_trap_on_level);

	/* Store number of glyphs on this level. -LM- */
	wr_byte(num_glyph_on_level);

	wr_byte(p_ptr->themed_level); /* Stores the current themed level. -LM- */

	/* Stores what themed levels have already appeared. -LM- */
	wr_u32b(p_ptr->themed_level_appeared);

	/* Squelch */
	for (i = 0; i < 24; i++) wr_byte(squelch_level[i]);
	for (i = 0; i < 15; i++) wr_byte(0);

	/* Specialty abilties */
	for (i = 0; i < MAX_SPECIALTIES; i++) wr_byte(p_ptr->specialty_order[i]);

	/* Ignore some flags */
	wr_s16b(0L);	/* oops */

	/* Write the "object seeds" */
	wr_u32b(seed_flavor);
	wr_u32b(seed_town);


	/* Special stuff */
	wr_u16b(p_ptr->panic_save);
	wr_u16b(p_ptr->total_winner);
	wr_u16b(p_ptr->noscore);


	/* Write death */
	wr_byte(p_ptr->is_dead);

	/* Write feeling */
	wr_byte((byte)feeling);

	/* Turn of last "feeling" */
	wr_s32b(old_turn);

	/* Current turn */
	wr_s32b(turn);
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
	int i, y, x;

	byte tmp8u;

	byte count;
	byte prev_char;


	/*** Basic info ***/

	/* Dungeon specific info follows */
	wr_u16b(p_ptr->depth);
	wr_u16b(0);
	wr_u16b(p_ptr->py);
	wr_u16b(p_ptr->px);
	wr_u16b(DUNGEON_HGT);
	wr_u16b(DUNGEON_WID);
	wr_u16b(0);
	wr_u16b(0);


	/*** Simple "Run-Length-Encoding" of cave ***/

	/* Note that this will induce two wasted bytes */
	count = 0;
	prev_char = 0;

	/* Dump the cave */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
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
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
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
 * Actually write a save-file
 */
static bool wr_savefile_new(void)
{
	int i;

	u32b now;

	byte tmp8u;
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
	tmp8u = (byte)rand_int(256);
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

	/* Oangband version information. */
	wr_byte(O_VERSION_MAJOR);
	wr_byte(O_VERSION_MINOR);
	wr_byte(O_VERSION_PATCH);
	wr_byte(O_VERSION_EXTRA);

	/* Space */
	wr_u32b(0L);


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


	/* Dump the monster lore */
	tmp16u = MAX_R_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++) wr_lore(i);


	/* Dump the object memory */
	tmp16u = MAX_K_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++) wr_xtra(i);


	/* Hack -- Dump the quests */
	tmp16u = MAX_Q_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		wr_byte((byte)q_list[i].level);
		wr_byte(0);
		wr_byte(0);
		wr_byte(0);
	}


	/* Record the total number of artifacts. */
	tmp16u = MAX_A_IDX;
	wr_u16b(tmp16u);

	/* Record the number of random artifacts. */
	tmp16u = MAX_A_IDX - ART_MIN_RANDOM;
	wr_u16b(tmp16u);


	/* As the least bad of various possible options considered, Oangband now
	 * saves all random artifact data in savefiles.  This requires (44 * 40)
	 * = 1760 extra bytes in the savefile, which does not seem unreasonable.
	 */
	/* Write the artifact info. */
	for (i = 0; i < MAX_A_IDX; i++)
	{
		artifact_type *a_ptr = &a_info[i];

		/* Most regular artifact info is stored in a_info.raw. */
		if (i < ART_MIN_RANDOM)
		{
			wr_byte(a_ptr->creat_stat);
			wr_byte(0);
			wr_byte(0);
			wr_byte(0);
		}
		/* But random artifacts are specific to each player. */
		else
		{
			wr_u16b(a_ptr->name);
			wr_u16b(a_ptr->text);

			wr_byte(a_ptr->tval);
			wr_byte(a_ptr->sval);
			wr_u16b(a_ptr->pval);

			wr_u16b(a_ptr->to_h);
			wr_u16b(a_ptr->to_d);
			wr_u16b(a_ptr->to_a);

			wr_byte(a_ptr->dd);
			wr_byte(a_ptr->ds);

			wr_u16b(a_ptr->ac);
			wr_u16b(a_ptr->weight);

			wr_u32b(a_ptr->cost);

			wr_u32b(a_ptr->flags1);
			wr_u32b(a_ptr->flags2);
			wr_u32b(a_ptr->flags3);

			wr_byte(a_ptr->level);
			wr_byte(a_ptr->rarity);

			wr_byte(a_ptr->creat_stat);
			wr_byte(a_ptr->activation);

			/* Add some filler space for later expansion. */
			wr_u32b(0);
		}
	}

	/* Note down how many random artifacts have names.  In Oangband 0.5.0
	 * there are 40 random artifact names.
	 */
	wr_u16b(MAX_A_IDX - ART_MIN_RANDOM);

	/* Write the list of random artifact names. */
	for (i = ART_MIN_RANDOM; i < MAX_A_IDX; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		wr_string(format("%s", a_name + a_ptr->name));
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


	/* Write spell data */
	wr_u32b(0);
	wr_u32b(0);
	wr_u32b(p_ptr->spell_worked1);
	wr_u32b(p_ptr->spell_worked2);
	wr_u32b(0);
	wr_u32b(0);

	/* Was: Dump the ordered spells */
	for (i = 0; i < 64; i++)
	{
		wr_byte(0);
	}


	/* Write the inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Dump index */
		wr_u16b(i);

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
	if (!p_ptr->is_dead)
	{
		/* Dump the dungeon */
		wr_dungeon();

		/* Dump the ghost */
		wr_ghost();
	}


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
 *
 * XXX XXX XXX Angband 2.8.0 will use "fd" instead of "fff" if possible
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
			if (wr_savefile_new()) ok = TRUE;

			/* Attempt to close it */
			if (my_fclose(fff)) ok = FALSE;
		}

		/* Remove "broken" files */
		if (!ok) fd_kill(name);
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

	/* Write to the savefile list */
	save_savefile_names();

	/* Return the result */
	return (result);
}

/*
 * Hack
 *
 * Check worn items and apply sets as set data is not stored -GS-
 */
void check_item_sets(void)
{
	int j;

	/* Check all equipped items. */
	for (j=INVEN_WIELD;j<=INVEN_FEET;j++)
	{
		object_type *o_ptr;
		o_ptr = &inventory[j];

		/* Is it an artifact? */
		if (o_ptr->name1)
		{
			artifact_type *a_ptr=&a_info[o_ptr->name1];

			/* Is it a set item? */
			if (a_ptr->set_no != 0)
			{

				/* Check for complete set. */
				if (check_set(a_ptr->set_no))
				{

					/* Apply set bonuses */
					apply_set(a_ptr->set_no);
				}
			}
		}
	}
}

/*
 * Attempt to Load a "savefile"
 *
 * Version 2.7.0 introduced a slightly different "savefile" format from
 * older versions, requiring a completely different parsing method.
 *
 * Note that savefiles from 2.7.0 - 2.7.2 are completely obsolete.
 *
 * Pre-2.8.0 savefiles lose some data, see "load2.c" for info.
 *
 * Pre-2.7.0 savefiles lose a lot of things, see "load1.c" for info.
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
bool load_player(bool silent)
{
	errr err = 0;

	cptr i = NULL;
	cptr j = NULL;


#ifdef SAVEFILE_USE_UID
	char base_name_temp[1024];
#endif

#ifdef VERIFY_TIMESTAMP
	struct stat	statbuf;
#endif

	cptr what = "generic";


	/* Paranoia */
	turn = 0;

	/* Paranoia */
	p_ptr->is_dead = FALSE;


	/* Allow empty savefile name */
	if (!savefile[0]) return (TRUE);

	/* Build base name */
	i = savefile;

	/* Strip path */
	while (TRUE)
	{
		j = strstr(i, PATH_SEP);
		if (j != NULL) i = j + 1;
		else break;
	}

	strcpy(op_ptr->base_name, i);

#ifdef SAVEFILE_USE_UID
	strcpy(base_name_temp, i);

	/* Strip UID */
	i = strstr(base_name_temp, ".");

	if (i != NULL)
	{
		strcpy(op_ptr->base_name, i + 1);
	}

#endif

#if !defined(MACINTOSH) && !defined(WINDOWS) && !defined(VM)

	/* XXX XXX XXX Fix this */

	/* Verify the existance of the savefile */
	if (access(savefile, 0) < 0)
	{
		/* Give a message */
		if (!silent)
		{
			msg_print("Savefile does not exist.");
			msg_print(NULL);
		}

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

#ifdef VERIFY_TIMESTAMP
	/* Okay */
	if (!err)
	{
		/* Open the savefile */
		fd = fd_open(savefile, O_RDONLY);

		/* No file */
		if (fd < 0) err = -1;

		/* Message (below) */
		if (err) what = "Cannot open savefile";

		/* Get the timestamp */
		(void)fstat(fd, &statbuf);

		/* Close the file */
		fd_close(fd);
	}
#endif

	/* Collect the Angband and Oangband version information. */
	if (!err)
	{
		/* Read file. */
		err = rd_version_info();

		/* Describe errors */
		if (err == -1) what = "Cannot open savefile";
	}

	/* Process file */
	if (!err)
	{
		/* Very old savefiles */
		if ((sf_major == 5) && (sf_minor == 2))
		{
			sf_major = 2;
			sf_minor = 5;
		}

		/* Extremely old savefiles */
		if (sf_major > 2)
		{
			sf_major = 1;
		}

		/* Clear screen */
		Term_clear();

		/* Parse "ancient" savefiles */
		if (sf_major < 2)
		{
			/* Attempt to load */
			err = rd_savefile_old();
		}

		/* Parse "old" savefiles */
		else if ((sf_major == 2) && (sf_minor < 7))
		{
			/* Attempt to load */
			err = rd_savefile_old();
		}

		/* Parse "new" savefiles */
		else if (sf_major == 2)
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
		if ((o_version_major != o_sf_major) ||
		    (o_version_minor != o_sf_minor) ||
		    (o_version_patch != o_sf_patch))
		{
			/* Message */
			if ((o_sf_major == 0) && (o_sf_minor == 2))
				msg_format("Converted an Angband %d.%d.%d savefile.",
				sf_major, sf_minor, sf_patch);
			else msg_format("Converted an Oangband %d.%d.%d savefile.",
				o_sf_major, o_sf_minor, o_sf_patch);
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
				/* Check the character for completed item sets */
				check_item_sets();

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

		/* Check the character for completed item sets */
		check_item_sets();

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
	if ((o_sf_major == 0) && (o_sf_minor == 2))
		msg_format("Error (%s) reading an Angband %d.%d.%d savefile.",
		what, sf_major, sf_minor, sf_patch);
	else msg_format("Error (%s) reading an Oangband %d.%d.%d savefile.",
		what, o_sf_major, o_sf_minor, o_sf_patch);

	msg_print(NULL);

	/* Oops */
	return (FALSE);
}



/**************************************************************/
/*                                                            */
/*                  Savefile Management Code                  */
/*                                                            */
/**************************************************************/

#define SAVE_MENU_HEADER_ROW 1
#define SAVE_MENU_INSTRUCTIONS_ROW 3
#define SAVE_MENU_DESC_ROW 7
#define SAVE_MENU_DISPLAY_ROW 9

#define SAVE_MENU_SCROLL_GUARD 4

#define SAVE_MENU_MAX_DISPLAY_NITEMS 9
#define SAVE_MENU_INTERACTIVE_NROW 1

/*
 * Temporary storage of savefile paths, descriptions, and dead/alive status.
 */
char savefile_names[46][40];
char savefile_character_names[46][40];
char savefile_desc[46][80];
bool savefile_alive[46];

/*
 * Read the savefile record.  -DG-
 */
static int load_savefile_names(void)
{
	char buf[1024];
	char tmp[50];
	int max = 0;
	int fd;

	/* Build the filename */
#ifdef SAVEFILE_USE_UID
	sprintf(tmp,"user.%d.svg", player_uid);
#else
	sprintf(tmp, "global.svg");
#endif /* SAVEFILE_USE_UID */

	/* Attempt to load the savefile record */
	if (path_build(buf, sizeof(buf), ANGBAND_DIR_SAVE, tmp)) return (0);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Grab permissions */
	safe_setuid_grab();

	/* Read the file */
	fff = my_fopen(buf, "r");

	/* Drop permissions */
	safe_setuid_drop();

	/* Failure */
	if (!fff) return (0);

	/* Parse the savefile record */
	while (0 == my_fgets(fff, buf, sizeof(buf)))
	{
		int i = 1;
		int j;

		/* Read "dead/alive" */
		if      (buf[0] == '0') savefile_alive[max] = FALSE;
		else if (buf[0] == '1') savefile_alive[max] = TRUE;

		/* Read the savefile name */
		while (TRUE)
		{
			/* Build path */
			if (buf[i] != '#') savefile_names[max][i - 1] = buf[i];
			else
			{
				/* Terminate */
				savefile_names[max][i - 1] = '\0';
				break;
			}

			i++;
		}

		/* Skip the '#' */
		i++;

		/* Mark start of character name */
		j = i;

		/* Read the character name */
		while (TRUE)
		{
			/* Build path */
			if (buf[i] != '#') savefile_character_names[max][i - j] = buf[i];
			else
			{
				/* Terminate */
				savefile_character_names[max][i - j] = '\0';
				break;
			}

			i++;
		}

		/* Skip the '#' */
		i++;

		/* Read the character description */
		sprintf(savefile_desc[max], buf + i);

		/* Append user ID to filename if necessary */
#ifdef SAVEFILE_USE_UID
		sprintf(tmp,"%d.%s", player_uid, savefile_names[max]);
#else
		sprintf(tmp, "%s", savefile_names[max]);
#endif

		/* Confirm that file still exists */
		path_build(buf, sizeof(buf), ANGBAND_DIR_SAVE, tmp);

		/* Grab permissions */
		safe_setuid_grab();

		/* File type is "SAVE" */
		FILE_TYPE(FILE_TYPE_SAVE);

		/* Check for file */
		fd = fd_open(buf, O_RDONLY);

		/* Drop permissions */
		safe_setuid_drop();

		/* File exists */
		if (fd >= 0)
		{
			fd_close(fd);

			/* Increment file count, go to next record */
			max++;
		}
	}

	/* Close the savefile record */
	my_fclose(fff);

	/* Return number of valid savefiles */
	return (max);
}


/*
 * Write the savefile record.  -DG-
 */
void save_savefile_names()
{
	char buf[1024];
	char tmp[50];

	/* Load existing savefile records */
	int max = load_savefile_names(), i;

	/* Build the filename - use user index if necessary */
#ifdef SAVEFILE_USE_UID
	sprintf(tmp,"user.%d.svg", player_uid);
#else
	sprintf(tmp, "global.svg");
#endif /* SAVEFILE_USE_UID */

	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_SAVE, tmp);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Read the file */
	fff = my_fopen(buf, "w");

	/* Failure */
	if (!fff) return;

	/* Save information about the current character savefile */
	fprintf(fff, "%c%s#%s#the %s (level %d %s %s) is %s\n", ((p_ptr->is_dead)? '0' : '1'),
		op_ptr->base_name,
		op_ptr->full_name,
		cp_text + cp_ptr->title[(p_ptr->lev - 1) / 5],
		p_ptr->lev,
		rp_name + rp_info[p_ptr->prace].name,
		cp_name + cp_info[p_ptr->pclass].name,
		(!p_ptr->is_dead) ? "alive" : "dead");

	/* Rewrite all other savefile records - do not exceed 46 records */
	for (i = 0; i < MIN(max, 45); i++)
	{
		/* Do not record the current savefile more than once */
		if (!strcmp(savefile_names[i], op_ptr->base_name)) continue;

		/* Write a savefile record */
		fprintf(fff, "%c%s#%s#%s\n", (savefile_alive[i])?'1':'0',
			savefile_names[i], savefile_character_names[i], savefile_desc[i]);
	}

	/* Close */
	my_fclose(fff);
}

/*
 * Update limits on menu display based on current position.
 */
static void savefile_menu_aux(int sel, int max, int* menu_start)
{
	int start_crowding, end_crowding;
	int sel_display_position;

	if (max <= (Term->hgt - SAVE_MENU_MAX_DISPLAY_NITEMS)) return;

	sel_display_position = sel - *menu_start;
	start_crowding = SAVE_MENU_SCROLL_GUARD - sel_display_position;
	end_crowding = SAVE_MENU_SCROLL_GUARD -
	  (Term->hgt - SAVE_MENU_MAX_DISPLAY_NITEMS - sel_display_position);

	if (start_crowding > 0)
	{
		*menu_start -= start_crowding;
	}
	if (end_crowding > 0) {
		*menu_start += end_crowding;
	}

	*menu_start = MIN((MAX(*menu_start, 0)), (max - (Term->hgt - SAVE_MENU_MAX_DISPLAY_NITEMS)));
}

/*
 * Interact with the savefile management screen.  -DG-
 * Changed menu format  BR
 */
static void savefile_menu(bool *new_game, bool *reload_menu)
{
	int k, sel, max;

	int i;

	int x, y;

	int y_desc;
	int y_menu;

	char buf[256];

	char ind;

	int menu_start, display_items;

	/* Default outputs */
	*new_game = FALSE;
	*reload_menu = FALSE;

	/* No savefile has been loaded yet */
	character_loaded = FALSE;

	/* Wipe the player */
	player_clear(TRUE);

	/* Load savefile records, leave room for two miscellaneous menu choices */
	max = load_savefile_names() + 2;

	/* Highlight the most recent savefile, or "create new character" */
	if (max > 2) sel = 2;
	else         sel = 0;

	/* Clear screen */
	(void)Term_clear();

	/* Display the header */
	c_put_str(TERM_L_BLUE,
	          format("Welcome to %s.  To play you will need a character.", VERSION_NAME),
	          SAVE_MENU_HEADER_ROW, 2);

	/* Show available commands */
	Term_gotoxy(2, SAVE_MENU_INSTRUCTIONS_ROW);
	roff("Use the ", 2, 78);
	c_roff(TERM_L_GREEN, "movement keys", 2, 78);
	roff(" to scroll the menu, ", 2, 78);
	c_roff(TERM_L_GREEN, "Enter", 2, 78);
	roff(" to select the current menu item, ", 2, 78);
	c_roff(TERM_L_GREEN, "backspace", 2, 78);
	roff(" to delete the currently selected savefile, or ", 2, 78);
	c_roff(TERM_L_GREEN, "ESCAPE", 2, 78);
	roff(" to quit.  ", 2, 78);

	/* Display system-specific comments */

	/* The windows menus are not currently usable once play_game
	 * has been called.  This may be changed later.  BR
	 */
/* 	if (strstr(ANGBAND_SYS, "win")) */
/* 		roff("You may also use the menu commands to handle savefiles.", 2, 0); */

	/*
	 * y_desc is where the description is placed ("PLAYER is alive").
	 * y_menu is where the menu starts.
	 *
	 * A padding of two lines between each looks uncluttered.
	 */
	Term_locate(&x, &y);

	y_desc = SAVE_MENU_DESC_ROW;
	y_menu = SAVE_MENU_DISPLAY_ROW;


	display_items = MIN((Term->hgt - SAVE_MENU_MAX_DISPLAY_NITEMS), max);
	menu_start = 0;

	/* Interact with the menu */
	while (TRUE)
	{
		int x;

		/* Display the menu choices */
		for (i = menu_start; i < menu_start + display_items; i++)
		{
			ind = I2A(i % 26);
			if (i >= 26) ind = toupper(ind);

			/* Save the y co-ordinate */
			y = y_menu + i - menu_start;

			/* Get text for this menu option */
			if (i == 0)      sprintf(buf, "%c) New Character", ind);
			else if (i == 1) sprintf(buf, "%c) Load Savefile", ind);
			else sprintf(buf, "%c) %s", ind, savefile_character_names[i - 2]);

			/* Display this menu option */
			Term_erase(0, y_menu + i - menu_start, Term->wid);
			c_put_str(((sel == i) ?
				   TERM_L_BLUE :
				   (((savefile_alive[i - 2]) || (i < 2)) ?
				    TERM_WHITE :
				    TERM_L_RED)),
				  buf, y, 2);

			/* This menu choice is selected */
			if (sel == i)
			{
				Term_erase(0, y_desc, Term->wid);

				/* Load an existing savefile in the list */
				if (i >= 2)
				{
					/* Color depending on dead or alive */
					int attr = ((savefile_alive[i - 2]) ? TERM_L_GREEN : TERM_L_RED);
					c_put_str(attr, format("%s, %s.", savefile_character_names[i - 2],
							       savefile_desc[i - 2]), y_desc, 2);
				}

				/* Load an existing savefile not in the list */
				else if (i == 1)
				{
					c_put_str(TERM_YELLOW, "Load an existing savefile that is not in the list.",
						  y_desc, 2);
				}

				/* Create a new character */
				else
				{
					c_put_str(TERM_YELLOW, "Create a new character.", y_desc, 2);
				}
			}
		}

		/* Move the cursor to the letter of the selection */
		(void)Term_gotoxy(2, y_menu + sel - menu_start);

		/* Get response */
		k = inkey();

		/* Process commands */
		if (k == ESCAPE)
		{
			quit(NULL);
		}
		else if (k == '2')
		{
			/* Go down one line of the menu */
			sel++;
			if (sel >= max) sel = max - 1;
			savefile_menu_aux(sel, max, &menu_start);
			continue;
		}
		else if (k == '8')
		{
			/* Go up one line of the menu */
			sel--;
			if (sel < 0) sel = 0;
			savefile_menu_aux(sel, max, &menu_start);
			continue;
		}
		else if ((k == '\r') || (k == '\n'))
		{
			/* Choose this menu option */
			if (sel < 26) k = I2A(sel);
			else k = toupper(I2A(sel - 26));
		}

		/* Delete savefile */
		if (((k == 0x7F) || (k == '\010')) && (sel >= 2))
		{
			char filename[1024];
			char tmp[50];

			if (!get_check(format("Really delete '%s'? ", savefile_character_names[sel - 2]))) continue;

			/* Append user ID to filename if necessary */
#ifdef SAVEFILE_USE_UID
			sprintf(tmp,"%d.%s", player_uid, savefile_names[sel - 2]);
#else
			sprintf(tmp, "%s", savefile_names[sel - 2]);
#endif

			/* Build the file name */
			path_build(filename, sizeof(filename), ANGBAND_DIR_SAVE, tmp);

			/* Delete this file */
			fd_kill(filename);

			/* Reload the savefile record */
			max = load_savefile_names() + 2;
			if (max > 2) sel = 2;
			else         sel = 0;
			menu_start = 0;

			*reload_menu = TRUE;
			return;
		}

		/* Create new character */
		else if (k == 'a')
		{
			/* Move current selection */
			sel = 0;
			savefile_menu_aux(sel, max, &menu_start);

			if (get_check("Create new character? "))
			{
				*new_game = TRUE;
				return;
			}
		}

		/* Find new savefile */
		else if (k == 'b')
		{
			/* Move current selection */
			sel = 1;
			savefile_menu_aux(sel, max, &menu_start);

			/* Display prompt */
			Term_erase(0, Term->hgt - SAVE_MENU_INTERACTIVE_NROW, Term->wid);
			prt("Enter the name of a savefile: ", Term->hgt - SAVE_MENU_INTERACTIVE_NROW, 0);

			/* Ask the user for a string */
			if (!askfor_aux(op_ptr->base_name, 30))
			{
				Term_erase(0, Term->hgt - SAVE_MENU_INTERACTIVE_NROW, Term->wid);
				continue;
			}

			/* Process the player name, change savefile name */
			process_player_name(FALSE);

			return;
		}

		/* Fall through for character loading */

		/* Process command */
		if (islower(k)) x = A2I(k);
		else x = A2I(tolower(k)) + 26;

		/* Stay legal */
		if ((x < 2) || (x >= max)) continue;

		sel = x;
		savefile_menu_aux(sel, max, &menu_start);

		if (get_check(format("Load character '%s'? ", savefile_character_names[sel - 2])))
		{
			/* Get player name */
			sprintf(op_ptr->base_name, "%s", savefile_names[sel - 2]);

			/* Process the player name, change savefile name */
			process_player_name(FALSE);

			return;
		}
	}

	/* Return */
	return;
}


/*
 * Load a character.
 *
 * When the game starts, this function is called to handle savefile loading.
 * We automatically load the most recent savefile recorded in our savefile
 * record.  If this fails, we load any savefile named "player" (this
 * replicates the old behavior).  Otherwise we load nothing and show the
 * savefile management screen.  -LM-
 *
 * An option exists to go directly to the savefile management menu.  This is
 * used if the player deliberately calls this function.
 *
 * As has traditionally been the case, we use "PLAYER" as a default savefile
 * name.
 *
 * This function must cooperate with the menu commands offered by various
 * ports.  XXX XXX XXX
 */
void savefile_load(bool force_menu)
{
	bool new_game;

	/* Load the savefile record */
	int max = load_savefile_names();


	/* No savefile has been loaded yet */
	character_loaded = FALSE;

	/* We're not forcing menus */
	if (!force_menu)
	{
		/* Non-empty savefile record, most recent char is alive */
		if ((max > 0) && (savefile_alive[0]))
		{
			/* Store the base name of the most recent savefile entry */
			strcpy(op_ptr->base_name, savefile_names[0]);

			/* Process this base name, change savefile name */
			process_player_name(FALSE);

			/* Try to load this savefile */
			if (load_player(TRUE)) return;
		}

		/* We'll always load a savefile named "PLAYER" */

		/* Hack -- Default base_name */
#ifdef SAVEFILE_USE_UID
		user_name(op_ptr->full_name, player_uid);
#else
		strcpy(op_ptr->full_name, "PLAYER");
#endif

		/* Process this base name, change savefile name */
		process_player_name(FALSE);

		/* Try to load the file "PLAYER" */
		if (load_player(TRUE)) return;
	}

	/* We're forcing a menu, or haven't found an obvious savefile */
	if (TRUE)
	{
		/* Repeat until satisfied */
		while (TRUE)
		{
			int error;
			bool reload_menu = TRUE;

			/* Show the menu immediately, store chosen name */
			while (reload_menu) savefile_menu(&new_game, &reload_menu);

			/* We want to start a new game (not using any previous savefile) */
			if (new_game)
			{
				/* Hack -- Default base_name */
#ifdef SAVEFILE_USE_UID
				user_name(op_ptr->full_name, player_uid);
#else
				strcpy(op_ptr->full_name, "PLAYER");
#endif

				/* Return */
				return;
			}

			/* Try to load this character */
			error = (load_player(FALSE) ? 0 : -1);

			/* File is unreadable */
			if (error)
			{
				/* Oops */
				if (error > 0)
				{
					msg_format("Sorry, this savefile cannot be read by %s %s.",
					VERSION_NAME, VERSION_STRING);
				}

				/* Oops */
				msg_print("Please choose another file or start a new game.");

				/* Wait for it */
				(void)inkey();
			}

			/* Successfully loaded an old character (alive or dead) */
			else
			{
				/* Return */
				return;
			}
		}
	}
}
