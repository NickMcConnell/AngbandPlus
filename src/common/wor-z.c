/* File: wor.c */

/* Purpose: dungeon memory */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#if defined(ZANGBANDTK)

#include "angband.h"
#include "tnb.h"

#ifdef ALLOW_REMEMBER_RECALL

bool wor_trump_hack = FALSE;

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

#if 0
	/* Old flags */
    if (o_ptr->art_name || o_ptr->art_flags1 || o_ptr->art_flags2 ||
        o_ptr->art_flags3)
    {
#endif
    wr_u32b(o_ptr->art_flags1);
    wr_u32b(o_ptr->art_flags2);
    wr_u32b(o_ptr->art_flags3);
#if 0
    }
    else                
    {
	wr_u32b(0L);
	wr_u32b(0L);
	wr_u32b(0L);
    }
#endif

	/* Held by monster index */
	wr_s16b(o_ptr->held_m_idx);

	/* Extra information */
	wr_byte(o_ptr->xtra1);
	wr_byte(o_ptr->xtra2);

	/* Feelings */
	wr_byte(o_ptr->feeling);

	/* Save the inscription (if any) */
	if (o_ptr->inscription)
	{
		wr_string(quark_str(o_ptr->inscription));
	}
	else
	{
		wr_string("");
	}
	
	/* If it is a "new" named artifact, save the name */        
	if (o_ptr->art_name)
	{
        wr_string(quark_str(o_ptr->art_name));
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
	wr_byte(m_ptr->invulner);
	wr_u32b(m_ptr->smart);
	wr_byte(0);
}

/*
 * Write some "extra" info
 */
static void wr_extra(void)
{
    /* Write feeling */
    wr_byte(feeling);

    /* Turn of last "feeling" */
    wr_s32b(old_turn);
    
    /* Turn we saved level */
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
	s16b prev_s16b, tmp16s;
	
	cave_type *c_ptr;


	/*** Basic info ***/

	/* Dungeon specific info follows */
	wr_u16b(dun_level);
	wr_u16b(num_repro);
	wr_u16b(py);
	wr_u16b(px);
	wr_u16b(cur_hgt);
	wr_u16b(cur_wid);
	wr_u16b(max_panel_rows);
	wr_u16b(max_panel_cols);


	/*** Simple "Run-Length-Encoding" of cave ***/

	/* Note that this will induce two wasted bytes */
	count = 0;
	prev_char = 0;

	/* Dump the cave */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			/* Get the cave */
			c_ptr = &cave[y][x];

			/* Extract a byte */
			tmp8u = c_ptr->info;
			
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
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			/* Get the cave */
			c_ptr = &cave[y][x];

			/* Extract a byte */
			tmp8u = c_ptr->feat;
			
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
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			/* Get the cave */
			c_ptr = &cave[y][x];

			/* Extract a byte */
			tmp8u = c_ptr->mimic;
			
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
	prev_s16b = 0;

	/* Dump the cave */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			/* Get the cave */
			c_ptr = &cave[y][x];

			/* Extract a byte */
			tmp16s = c_ptr->special;
			
			/* If the run is broken, or too full, flush it */
			if ((tmp16s != prev_s16b) || (count == MAX_UCHAR))
			{
				wr_byte((byte)count);
				wr_u16b(prev_s16b);
				prev_s16b = tmp16s;
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
		wr_u16b(prev_s16b);
	}


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
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
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
    tmp8u = (byte) rand_int(256);
    wr_byte(tmp8u);

    /* Reset the checksum */
    v_stamp = 0L;
    x_stamp = 0L;
    
    /* XXX Hack -- Fiddle with the level */
    depth = dun_level;
    dun_level = s_level_to_save;
    
    /* Write the dungeon */
    wr_dungeon();

    /* XXX Hack -- Fiddle with the level */
    dun_level = depth;
    
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
    rd_u32b(&o_ptr->art_flags1);
    rd_u32b(&o_ptr->art_flags2);
    rd_u32b(&o_ptr->art_flags3);

	/* Monster holding object */
	rd_s16b(&o_ptr->held_m_idx);

	/* Special powers */
	rd_byte(&o_ptr->xtra1);
	rd_byte(&o_ptr->xtra2);

	/* Feeling */
	rd_byte(&o_ptr->feeling);

	/* Inscription */
	rd_string(buf, 128);

	/* Save the inscription */
	if (buf[0]) o_ptr->inscription = quark_add(buf);

	/* Random artifact name */
	rd_string(buf, 128);

	/* Save the artifact name */
	if (buf[0]) o_ptr->art_name = quark_add(buf);
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
	rd_byte(&m_ptr->invulner);
	rd_u32b(&m_ptr->smart);
	rd_byte(&tmp8u);
}

/*
 * Read the "extra" information
 */
static errr rd_extra(void)
{
    byte tmp8u;

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
 */
static errr rd_dungeon(void)
{
	int i, y, x;

	int ymax, xmax;

	byte count;
	byte tmp8u;

	u16b limit;

	cave_type *c_ptr;

	s16b tmp16s;
	

	/*** Basic info ***/

	/* Header info */
	rd_s16b(&dun_level);
	rd_s16b(&num_repro);
	rd_s16b(&py);
	rd_s16b(&px);
	rd_s16b(&cur_hgt);
	rd_s16b(&cur_wid);
	rd_s16b(&max_panel_rows);
	rd_s16b(&max_panel_cols);


	/* Maximal size */
	ymax = cur_hgt;
	xmax = cur_wid;


	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < ymax; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Access the cave */
			c_ptr = &cave[y][x];

			/* Extract "info" */
			c_ptr->info = tmp8u;

			/* Advance/Wrap */
			if (++x >= xmax)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= ymax) break;
			}
		}
	}


	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < ymax; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Access the cave */
			c_ptr = &cave[y][x];

			/* Extract "feat" */
			c_ptr->feat = tmp8u;

			/* Advance/Wrap */
			if (++x >= xmax)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= ymax) break;
			}
		}
	}

	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < ymax; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Access the cave */
			c_ptr = &cave[y][x];

			/* Extract "feat" */
			c_ptr->mimic = tmp8u;

			/* Advance/Wrap */
			if (++x >= xmax)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= ymax) break;
			}
		}
	}

	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < ymax; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_s16b(&tmp16s);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Access the cave */
			c_ptr = &cave[y][x];

			/* Extract "feat" */
			c_ptr->special = tmp16s;

			/* Advance/Wrap */
			if (++x >= xmax)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= ymax) break;
			}
		}
	}


	/*** Player ***/

	(void) player_place(py, px); /* TNB */


	/*** Objects ***/

	/* Read the item count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit >= max_o_idx)
	{
		dbwin("Too many (%d) object entries!", limit);
		return (151);
	}

	/* Read the dungeon items */
	for (i = 1; i < limit; i++)
	{
		int o_idx;

		object_type *o_ptr;


		/* Get a new record */
		o_idx = o_pop();

		/* Oops */
		if (i != o_idx)
		{
			dbwin("Object allocation error (%d <> %d)", i, o_idx);
			return (152);
		}


		/* Acquire place */
		o_ptr = &o_list[o_idx];

		/* Read the item */
		rd_item(o_ptr);


		/* XXX XXX XXX XXX XXX */

		/* Monster */
		if (o_ptr->held_m_idx)
		{
			monster_type *m_ptr;

			/* Monster */
			m_ptr = &m_list[o_ptr->held_m_idx];

			/* Build a stack */
			o_ptr->next_o_idx = m_ptr->hold_o_idx;

			/* Place the object */
			m_ptr->hold_o_idx = o_idx;
		}
		
		/* Dungeon */
		else
		{
			/* Access the item location */
			c_ptr = &cave[o_ptr->iy][o_ptr->ix];

			/* Build a stack */
			o_ptr->next_o_idx = c_ptr->o_idx;

			/* Place the object */
			c_ptr->o_idx = o_idx;
		}
	}


	/*** Monsters ***/

	/* Read the monster count */
	rd_u16b(&limit);

	/* Hack -- verify */
	if (limit >= max_m_idx)
	{
		dbwin("Too many (%d) monster entries!", limit);
		return (161);
	}

	/* Read the monsters */
	for (i = 1; i < limit; i++)
	{
		int m_idx;

		monster_type *m_ptr;

		monster_race *r_ptr;


		/* Get a new record */
		m_idx = m_pop();

		/* Oops */
		if (i != m_idx)
		{
			dbwin("Monster allocation error (%d <> %d)", i, m_idx);
			return (162);
		}


		/* Acquire monster */
		m_ptr = &m_list[m_idx];

		/* Read the monster */
		rd_monster(m_ptr);


		/* Access grid */
		c_ptr = &cave[m_ptr->fy][m_ptr->fx];

		/* Mark the location */
		c_ptr->m_idx = m_idx;


		/* Access race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Count XXX XXX XXX */
		r_ptr->cur_num++;
	}


	/*** Success ***/

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
	ymax = cur_hgt;
	xmax = cur_wid;

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

bool remember_recall = TRUE;

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
				/* The artifact was created elsewhere */
				if (a_info[o_ptr->name1].cur_num)
				{
					dbwin("delete artifact\n");

					delete_object_idx(i);
				}
	
				/* The artifact was not created */
				else
				{
					dbwin("restore artifact\n");

					/* XXX Mega-Hack -- Preserve the artifact */
					a_info[o_ptr->name1].cur_num = 1;
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
				(void)alloc_monster(MAX_SIGHT + 5, FALSE);
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

				/* Recharge rods on the ground.  No messages. */
				if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
				{
					/* Charge it */
					o_ptr->timeout -= o_ptr->number;
		
					/* Boundary control. */
					if (o_ptr->timeout < 0)
						o_ptr->timeout = 0;
				}
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
		for (y = 0; y < cur_hgt; y++)
		{
			for (x = 0; x < cur_wid; x++)
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
	if (p_ptr->inside_quest) save = FALSE;
	if (p_ptr->inside_arena) save = FALSE;
	
	if (save)
	{
		s_level_to_save = dun_level;
	}
	else
	{
		s_level_to_save = 0;
	}
}

void wor_delay_load(bool load)
{
	/*
	 * Hack -- When using the Trump Tower to teleport to a level, ignore the
	 * remember_recall option
	 */
	if (wor_trump_hack) load = FALSE;
	wor_trump_hack = FALSE;

	s_do_restore = load;
}

#endif /* ALLOW_REMEMBER_RECALL */

#endif /* ZANGBANDTK */
