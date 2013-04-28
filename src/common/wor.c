/* File: wor.c */

/* Purpose: dungeon memory */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)

#include "angband.h"

#include "tnb.h"

#ifdef ALLOW_REMEMBER_RECALL

#define LEVEL_BUFFER_MAX (80L * 1024L) /* Size of level buffer */

static byte	xor_byte;	/* Simple encryption */

static byte *data_head = NULL;		/* Level buffer */
static byte *data_next;		/* Pointer into level buffer */
static u32b data_size; /* Length of data written to level buffer */

static int s_level_to_save; /* Delayed action: save this level */
static bool s_do_restore; /* Delayed action: restore the level */
static int s_saved_level; /* Dungeon level saved, or 0 */
static s32b s_save_turn;			/* Turn the level was saved */




static u32b	v_stamp = 0L;		/* A simple "checksum" on the actual values */
static u32b	x_stamp = 0L;		/* A simple "checksum" on the encoded bytes */

/*
 * Append a byte to the level buffer.
 */
static void put_byte(byte v)
{
	/* Error if the buffer gets full */
	if (data_size >= LEVEL_BUFFER_MAX) return;

	/* Append the byte to our buffer */
	*data_next++ = v;

	/* Count the bytes */
	data_size++;
}

static void sf_put(byte v)
{
    /* Encode the value, write a character */
    xor_byte ^= v;
    put_byte(xor_byte);

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
	sf_put((byte) v & 0xFF);
	sf_put((byte) (v >> 8) & 0xFF);
}

static void wr_s16b(s16b v)
{
	wr_u16b((u16b)v);
}

static void wr_u32b(u32b v)
{
	sf_put((byte) v & 0xFF);
	sf_put((byte) (v >> 8) & 0xFF);
	sf_put((byte) (v >> 16) & 0xFF);
	sf_put((byte) (v >> 24) & 0xFF);
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

#if defined(OANGBANDTK)
	wr_byte(o_ptr->feel);
#endif

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
#if defined(KANGBANDTK)
	wr_byte(m_ptr->is_pet);
	wr_byte(m_ptr->is_friendly);
#endif /* KANGBANDTK */
#if defined(ANGBANDTK) || defined(KANGBANDTK)
	wr_byte(0);
#endif
#if defined(OANGBANDTK)
	wr_byte(m_ptr->stasis);
	wr_byte(m_ptr->black_breath);
	wr_u32b(m_ptr->smart);
	wr_byte(m_ptr->harass);
	wr_byte(m_ptr->mana);
#endif
}

/*
 * Write some "extra" info
 */
static void wr_extra(void)
{
#if defined(OANGBANDTK)

	/* Store the bones file selector, if the player is not dead. -LM- */
	if (!(p_ptr->is_dead))
		wr_byte(bones_selector);
	else
		wr_byte(0);

	/* Store the number of thefts on the level. -LM- */
	wr_byte(number_of_thefts_on_level);

#if defined(OANGBANDTK)

	/* Read number of monster traps on level. -LM- */
	wr_byte(num_trap_on_level);

	/* Read number of glyphs on level. -LM- */
	wr_byte(num_glyph_on_level);

	/* Is the level themed and, if so, which theme is it? -LM- */
	wr_byte(p_ptr->themed_level);

	/* What themed levels have already appeared? -LM- */
	wr_u32b(p_ptr->themed_level_appeared);

#endif /* */

#endif /* OANGBANDTK */

    /* Write feeling */
    wr_byte(feeling);

    /* Turn of last "feeling" */
    wr_s32b(old_turn);
    
    /* Turn we saved level */
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

static void wr_grid_xtra(void)
{
	int y, x;
    byte count, prev_char;
    byte tmp8u;

    /* Note that this will induce two wasted bytes */
    count = 0;
    prev_char = 0;

    /* Dump the cave */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
            /* Get the flags */
            tmp8u = (byte) g_grid[y][x].xtra;

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
}

static void wr_recall_level(void)
{
    int depth;
    byte tmp8u;

    /* Write the xor_byte encoder */
    xor_byte = 0;
    tmp8u = rand_int(256);
    wr_byte(tmp8u);

    /* Reset the checksum */
    v_stamp = 0L;
    x_stamp = 0L;
    
    /* XXX Hack -- Fiddle with the level */
    depth = p_ptr->depth;
    p_ptr->depth = s_level_to_save;
    
    /* Write the dungeon */
    wr_dungeon();

    /* XXX Hack -- Fiddle with the level */
    p_ptr->depth = depth;
    
    /* Write stuff */
    wr_extra();
    
    /* Write cave_xtra array */
    wr_grid_xtra();

	/* Write the "value check-sum" */
	wr_u32b(v_stamp);

	/* Write the "encoded checksum" */
	wr_u32b(x_stamp);
}


static u32b	v_check;		/* A simple "checksum" on the actual values */
static u32b	x_check;		/* A simple "checksum" on the encoded bytes */

/*
 * Get a byte from the level buffer.
 */
static byte get_byte(void)
{
	/* Error if the buffer is empty */
	if (data_next >= data_head + data_size) return (0);
	
	/* Get the next byte to our buffer */
	return *data_next++;
}

static byte sf_get(void)
{
    byte c, v;

    /* Get a character, decode the value */
    c = get_byte();
    v = c ^ xor_byte;
    xor_byte = c;

    /* Maintain the checksum info */
    v_check += v;
    x_check += xor_byte;

    /* Return the value */
    return (v);
}

static void rd_byte(byte *ip)
{
	*ip = sf_get();
}

static void rd_u16b(u16b *ip)
{
	(*ip) = sf_get();
	(*ip) |= ((u16b)(sf_get()) << 8);
}

static void rd_s16b(s16b *ip)
{
	rd_u16b((u16b*)ip);
}

static void rd_u32b(u32b *ip)
{
	(*ip) = sf_get();
	(*ip) |= ((u32b)(sf_get()) << 8);
	(*ip) |= ((u32b)(sf_get()) << 16);
	(*ip) |= ((u32b)(sf_get()) << 24);
}

static void rd_s32b(s32b *ip)
{
	rd_u32b((u32b*)ip);
}


/*
 * Hack -- read a string
 */
static void rd_string(char *str, int max)
{
	int i;

	/* Read the string */
	for (i = 0; TRUE; i++)
	{
		byte tmp8u;

		/* Read a byte */
		rd_byte(&tmp8u);

		/* Collect string while legal */
		if (i < max) str[i] = tmp8u;

		/* End of string */
		if (!tmp8u) break;
	}

	/* Terminate */
	str[max-1] = '\0';
}

/*
 * Hack -- strip some bytes
 */
static void strip_bytes(int n)
{
	byte tmp8u;

	/* Strip the bytes */
	while (n--) rd_byte(&tmp8u);
}

static void rd_item(object_type *o_ptr)
{
	char buf[128];

	/* Kind */
	rd_s16b(&o_ptr->k_idx);

	/* Location */
	rd_byte(&o_ptr->iy);
	rd_byte(&o_ptr->ix);

	/* Type/Subtype */
	rd_byte(&o_ptr->tval);
	rd_byte(&o_ptr->sval);

	/* Special pval */
	rd_s16b(&o_ptr->pval);

	rd_byte(&o_ptr->discount);
	rd_byte(&o_ptr->number);
	rd_s16b(&o_ptr->weight);

	rd_byte(&o_ptr->name1);
	rd_byte(&o_ptr->name2);
	rd_s16b(&o_ptr->timeout);

	rd_s16b(&o_ptr->to_h);
	rd_s16b(&o_ptr->to_d);
	rd_s16b(&o_ptr->to_a);

	rd_s16b(&o_ptr->ac);

	rd_byte(&o_ptr->dd);
	rd_byte(&o_ptr->ds);

	rd_byte(&o_ptr->ident);

	rd_byte(&o_ptr->marked);

	/* Old flags */
	strip_bytes(12);

	/* Monster holding object */
	rd_s16b(&o_ptr->held_m_idx);

	/* Special powers */
	rd_byte(&o_ptr->xtra1);
	rd_byte(&o_ptr->xtra2);

#if defined(OANGBANDTK)
	rd_byte(&o_ptr->feel);
#endif

	/* Inscription */
	rd_string(buf, 128);

	/* Save the inscription */
	if (buf[0]) o_ptr->note = quark_add(buf);
}

/*
 * Read a monster
 */
static void rd_monster(monster_type *m_ptr)
{
	byte tmp8u;

	/* Read the monster race */
	rd_s16b(&m_ptr->r_idx);

	/* Read the other information */
	rd_byte(&m_ptr->fy);
	rd_byte(&m_ptr->fx);
	rd_s16b(&m_ptr->hp);
	rd_s16b(&m_ptr->maxhp);
	rd_s16b(&m_ptr->csleep);
	rd_byte(&m_ptr->mspeed);
	rd_byte(&m_ptr->energy);
	rd_byte(&m_ptr->stunned);
	rd_byte(&m_ptr->confused);
	rd_byte(&m_ptr->monfear);
#if defined(KANGBANDTK)
	rd_byte(&m_ptr->is_pet);
	rd_byte(&m_ptr->is_friendly);
#endif /* KANGBANDTK */
	rd_byte(&tmp8u);
#if defined(OANGBANDTK)
	m_ptr->stasis = tmp8u;
	rd_byte(&m_ptr->black_breath);
	rd_u32b(&m_ptr->smart);
	rd_byte(&m_ptr->harass);
	rd_byte(&m_ptr->mana);
#endif
}

/*
 * Read the "extra" information
 */
static errr rd_extra(void)
{
    byte tmp8u;

#if defined(OANGBANDTK)

	/* The number of the bone file (if any) that player ghosts should use to 
	 * reacquire a name, sex, class, and race. -LM-
	 */
	rd_byte(&bones_selector);

	/* Find out how many thefts have already occured on this level. -LM- */
	rd_byte(&number_of_thefts_on_level);

#if defined(OANGBANDTK)

	/* Read number of monster traps on level. -LM- */
	rd_byte(&num_trap_on_level);

	/* Read number of glyphs on level. -LM- */
	rd_byte(&num_glyph_on_level);

	/* Is the level themed and, if so, which theme is it? -LM- */
	rd_byte(&p_ptr->themed_level);

	/* What themed levels have already appeared? -LM- */
	rd_u32b(&p_ptr->themed_level_appeared);

#endif /* */

#endif /* OANGBANDTK */

    /* Read "feeling" */
    rd_byte(&tmp8u);
    feeling = tmp8u;

    /* Turn of last "feeling" */
    rd_s32b(&old_turn);

    /* Turn we saved level */
    rd_s32b(&s_save_turn);

	return (0);
}

/*
 * Read the dungeon
 *
 * The monsters/objects must be loaded in the same order
 * that they were stored, since the actual indexes matter.
 *
 * Note that the size of the dungeon is now hard-coded to
 * DUNGEON_HGT by DUNGEON_WID, and any dungeon with another
 * size will be silently discarded by this routine.
 */
static errr rd_dungeon(void)
{
	int i, y, x;

	s16b depth;
	s16b py, px;
	s16b ymax, xmax;

	byte count;
	byte tmp8u;
	u16b tmp16u;

	u16b limit;


	/*** Basic info ***/

	/* Header info */
	rd_s16b(&depth);
	rd_u16b(&tmp16u);
	rd_s16b(&py);
	rd_s16b(&px);
	rd_s16b(&ymax);
	rd_s16b(&xmax);
	rd_u16b(&tmp16u);
	rd_u16b(&tmp16u);


	/* Ignore illegal dungeons */
	if ((depth < 0) || (depth >= MAX_DEPTH))
	{
		dbwin(format("Ignoring illegal dungeon depth (%d)\n", depth));
		return (0);
	}

	/* Ignore illegal dungeons */
	if ((ymax != DUNGEON_HGT) || (xmax != DUNGEON_WID))
	{
		/* XXX XXX XXX */
		dbwin(format("Ignoring illegal dungeon size (%d,%d).\n", xmax, ymax));
		return (0);
	}

	/* Ignore illegal dungeons */
	if ((px < 0) || (px >= DUNGEON_WID) ||
	    (py < 0) || (py >= DUNGEON_HGT))
	{
		dbwin(format("Ignoring illegal player location (%d,%d).\n", px, py));
		return (1);
	}


	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < DUNGEON_HGT; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Extract "info" */
			cave_info[y][x] = tmp8u;

			/* Advance/Wrap */
			if (++x >= DUNGEON_WID)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= DUNGEON_HGT) break;
			}
		}
	}


	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < DUNGEON_HGT; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Extract "feat" */
			cave_set_feat(y, x, tmp8u);

			/* Advance/Wrap */
			if (++x >= DUNGEON_WID)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= DUNGEON_HGT) break;
			}
		}
	}


	/*** Player ***/

	/* Save depth */
	p_ptr->depth = depth;

	/* Place player in dungeon */
	if (!player_place(py, px))
	{
		dbwin(format("Cannot place player (%d,%d)!\n", py, px));
		return (162);
	}


	/*** Objects ***/

	/* Read the item count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit >= MAX_O_IDX)
	{
		dbwin(format("Too many (%d) object entries!\n", limit));
		return (151);
	}

	/* Read the dungeon items */
	for (i = 1; i < limit; i++)
	{
		object_type *i_ptr;
		object_type object_type_body;


		/* Acquire place */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Read the item */
		rd_item(i_ptr);


		/* Monster */
		if (i_ptr->held_m_idx)
		{
			/* Give the object to the monster */
			if (!monster_carry(i_ptr->held_m_idx, i_ptr))
			{
				dbwin(format("Cannot place object %d!\n", o_max));
				return (152);
			}
		}

		/* Dungeon */
		else
		{
			/* Give the object to the floor */
			if (!floor_carry(i_ptr->iy, i_ptr->ix, i_ptr))
			{
				dbwin(format("Cannot place object %d!\n", o_max));
				return (152);
			}
		}
	}


	/*** Monsters ***/

	/* Read the monster count */
	rd_u16b(&limit);

	/* Hack -- verify */
	if (limit >= MAX_M_IDX)
	{
		dbwin(format("Too many (%d) monster entries!\n", limit));
		return (161);
	}

	/* Read the monsters */
	for (i = 1; i < limit; i++)
	{
		monster_type *n_ptr;
		monster_type monster_type_body;

#if defined(OANGBANDTK)
		monster_race *r_ptr;
		int r_idx;
#endif /* OANGBANDTK */

		/* Get local monster */
		n_ptr = &monster_type_body;

		/* Clear the monster */
		(void) WIPE(n_ptr, monster_type);

		/* Read the monster */
		rd_monster(n_ptr);

#if defined(OANGBANDTK)

		/* Access the "r_idx" of the chosen monster */
		r_idx = n_ptr->r_idx;

		/* Access the actual race */
		r_ptr = &r_info[r_idx];

		/* If a player ghost, some special features need to be added. -LM- */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			prepare_ghost(n_ptr->r_idx, n_ptr, TRUE);
		}

#endif /* OANGBANDTK */

		/* Place monster in dungeon */
		if (!monster_place(n_ptr->fy, n_ptr->fx, n_ptr))
		{
			dbwin(format("Cannot place monster %d\n", i));
			return (162);
		}
	}

	/*
	 * Attach objects carried by a monster to the monster again.
	 * We look for the each object in o_list[] that is carried by
	 * a monster. If the monster isn't carrying any object yet,
	 * then assign it the object. The object with the highest
	 * o_idx is assumed to be at the head of the list of objects
	 * carried by a monster.
	 */
	for (i = o_max; i > 0; i--)
	{
		object_type *o_ptr = &o_list[i];
		if (!o_ptr->held_m_idx) continue;
		if (m_list[o_ptr->held_m_idx].hold_o_idx) continue;
		m_list[o_ptr->held_m_idx].hold_o_idx = i;
	}

	/* Success */
	return (0);
}

static errr rd_grid_xtra(void)
{
	int i;
	byte count;
	byte ychar, xchar;
	byte tmp8u;
	int ymax, xmax;
	int total_count;

dbwin("rd_grid_xtra\n");

	/* Only read as necessary */
	ymax = DUNGEON_HGT;
	xmax = DUNGEON_WID;

	/* Read in the actual "cave" data */
	total_count = 0;
	xchar = ychar = 0;

	/* Read until done */
	while (total_count < ymax * xmax)
	{
		/* Extract some RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Prevent over-run */
			if ((ychar >= ymax) || (xchar >= xmax))
			{
				return (81);
			}

			/* Save the flags */
			g_grid[ychar][xchar].xtra = (int) tmp8u;
			
			/* Advance the cave pointers */
			xchar++;

			/* Wrap to the next line */
			if (xchar >= xmax)
			{
				xchar = 0;
				ychar++;
			}
		}

		/* Advance the count */
		total_count += count;
	}
	
	/* Note that we have the flags */
	g_grid_xtra_init = TRUE;
	
	/* Success */
	return (0);
}

static errr rd_recall_level(void)
{
	u32b n_x_check, n_v_check;
	u32b o_x_check, o_v_check;

	/* Get the xor_byte */
	xor_byte = 0;
	rd_byte(&xor_byte);

	/* Clear the checksums */
	v_check = 0L;
	x_check = 0L;

	/* Read the dungeon */
	if (rd_dungeon())
	{
		return (34);
	}

	/* Read other stuff */
	rd_extra();
	
	/* Read cave_xtra array */
	if (rd_grid_xtra())
	{
		return (35);
	}

	/* Save the checksum */
	n_v_check = v_check;

	/* Read the old checksum */
	rd_u32b(&o_v_check);

	/* Verify */
	if (o_v_check != n_v_check)
	{
		dbwin("Invalid checksum\n");
		return (11);
	}

	/* Save the encoded checksum */
	n_x_check = x_check;

	/* Read the checksum */
	rd_u32b(&o_x_check);

	/* Verify */
	if (o_x_check != n_x_check)
	{
		dbwin("Invalid encoded checksum\n");
		return (11);
	}

	/* Success */
	return (0);
}

void wor_init(void)
{
	/* Only allocate storage if needed */
	data_head = NULL;

	/* The currently-saved level, 0 means none (never the Town) */
	s_saved_level = 0;

	/* Booleans: should the level be saved, should it be restored */
	s_level_to_save = 0;
	s_do_restore = FALSE;
}

void wor_save(void)
{
	/* Option */
	if (!remember_recall) return;

	/* Wasn't asked to save the level */
	if (s_level_to_save == 0) return;

	/* Alloc storage if needed */
	if (data_head == NULL)
	{
		/* Get some memory */
		C_MAKE(data_head, LEVEL_BUFFER_MAX, byte);	
	}

	/* Remember the saved level */
	s_saved_level = s_level_to_save;

	/* Set up the buffer */
	data_next = data_head;
	data_size = 0;

	/* Actually save the level */
	wr_recall_level();

	/* Buffer overflow */
	if (data_size >= LEVEL_BUFFER_MAX)
	{
		/* Message */
		plog_fmt("Buffer overflow in wor_save()!\nForgetting recall level!");
		
		/* Forget we saved it */
		wor_forget();
		
		return;
	}
	
	dbwin("wor_save(), data_size == %ld\n", data_size);
}

bool wor_load(void)
{
	errr e;
	int i;
	s32b t;

	/* Option */
	if (!remember_recall) return (FALSE);
	
	/* Wasn't asked to restore the level */
	if (!s_do_restore) return (FALSE);
	
	/* No saved level */
	if (!s_saved_level) return (FALSE);
 
	 /*
	 * Forget the saved level. Otherwise, if the character runs around
	 * on this level, then walks back to the Town and calls Word of Recall
	 * back to this level, it would restore outdated data.
	 */
	wor_forget();

	/* Set up the buffer */
	data_next = data_head;
	
	/* Actually read the recall level */
	e = rd_recall_level();
	
	dbwin("wor_load(), e == %d\n", e);

	/*
	 * If an unknown artifact was saved in the recall buffer,
	 * and the same artifact was later created as the character
	 * adventured elsewhere, we will delete the artifact.
	 * If the artifact was not created (and identified) elsewhere,
	 * then we must mark the artifact as existing, otherwise it
	 * may get created multiple times.
	 */
	if (p_ptr_preserve)
	{
		/* Process the objects (backwards) */
		for (i = o_max - 1; i > 0; i--)
		{
			/* Access object */
			object_type *o_ptr = &o_list[i];
	
			/* Skip dead objects */
			if (!o_ptr->k_idx) continue;
	
			/* It's an unknown artifact */
			if (artifact_p(o_ptr) && !object_known_p(o_ptr))
			{
				/* Get the artifact */
				artifact_type *a_ptr = &a_info[o_ptr->name1];

				/* The artifact was created elsewhere */
				if (ART_CURNUM(a_ptr))
				{
					dbwin("delete artifact\n");

					delete_object_idx(i);
				}
	
				/* The artifact was not created */
				else
				{
					dbwin("restore artifact\n");

					/* XXX Mega-Hack -- Preserve the artifact */
					ART_CURNUM(a_ptr) = 1;
				}
			}
		}
	}

	/*
	 * Time has passed on this level. Regenerate monsters and recharge
	 * treasure. We should also handle other stuff ala process_monster()
	 * (confusion, sleep, etc). Speed is a concern.
	 */
	for (t = s_save_turn; t <= turn; ++t)
	{
		/*** Process the monsters ***/
		
		if (!(t % 10))
		{
			/* Check for creature generation */
			if (rand_int(MAX_M_ALLOC_CHANCE) == 0)
			{
				/* Make a new monster */
#if defined(ANGBANDTK) || defined(KANGBANDTK)
				(void)alloc_monster(MAX_SIGHT + 5, FALSE);
#endif
#if defined(OANGBANDTK)
				(void)alloc_monster(MAX_SIGHT + 5, FALSE, FALSE);
#endif
			}
		}
		
		/* Hack -- Check for creature regeneration */
		if (!(t % 100)) regen_monsters();
		
		/*** Process the objects ***/

		if (!(t % 10))
		{
			/* Process objects */
			for (i = 1; i < o_max; i++)
			{
				/* Access object */
				object_type *o_ptr = &o_list[i];
		
				/* Skip dead objects */
				if (!o_ptr->k_idx) continue;

#if defined(ANGBANDTK) || defined(KANGBANDTK)
				/* Recharge activatable objects */
				if (o_ptr->timeout > 0)
				{
					/* Recharge */
					o_ptr->timeout--;
				}
		
				/* Recharge rods on the ground */
				if ((o_ptr->tval == TV_ROD) && (o_ptr->pval)) o_ptr->pval--;
#endif /* */
#if defined(OANGBANDTK)
				if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
				{
					/* Charge it */
					o_ptr->timeout -= o_ptr->number;
		
					/* Boundary control. */
					if (o_ptr->timeout < 0) o_ptr->timeout = 0;
				}
#endif /* */
			}
		}
	}

	/* Success */
	if (e == 0)
	{
		int y, x;

		/* The dungeon is ready */
		character_dungeon = TRUE;

		/* Check each grid */
		for (y = 0; y < DUNGEON_HGT; y++)
		{
			for (x = 0; x < DUNGEON_WID; x++)
			{
				set_grid_assign(y, x);
			}
		}

		return (TRUE);
	}

	/* Failure */
	return (FALSE);
}

/* */
void wor_forget(void)
{
	/* No level is currently saved */
	if (s_saved_level == 0) return;
	
	/* Now no level is saved */
	s_saved_level = 0;
}

void wor_delay_save(bool save)
{
#if defined(KANGBANDTK)

	if (p_ptr->inside_arena) save = FALSE;
	if (p_ptr->inside_quest) save = FALSE;

#endif /* KANGBANDTK */

	if (save)
	{
		s_level_to_save = p_ptr->depth;
	}
	else
	{
		s_level_to_save = 0;
	}
}

void wor_delay_load(bool load)
{
	s_do_restore = load;
}

#endif /* ALLOW_REMEMBER_RECALL */

#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */

