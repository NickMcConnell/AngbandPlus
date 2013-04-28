/* File: savefile.c */

/* Purpose: savefile parsing */

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

static byte s_vers_major;			/* Savefile's "version_major" */
static byte s_vers_minor;			/* Savefile's "version_minor" */
static byte s_vers_patch;			/* Savefile's "version_patch" */
static byte s_vers_extra;			/* Savefile's "version_extra" */
static u32b s_version;             /* Savefile's "version" */
static u32b s_info_xtra;			/* Operating system info */
static u32b s_info_when;			/* Time when savefile created */
static u16b s_info_lives;			/* Number of past "lives" with this file */
static u16b s_info_saves;			/* Number of "saves" during this life */

typedef struct t_savefile_info {
	bool is_dead; /* Player is dead */
	char full_name[32];	/* Full name */
	char died_from[80]; /* Cause of death */
	byte psex;			/* Sex index */
	byte prace;			/* Race index */
	byte pclass;		/* Class index */
	s16b age;			/* Characters age */
	s16b ht;			/* Height */
	s16b wt;			/* Weight */
	s16b sc;			/* Social Class */
	s32b au;			/* Current Gold */
	s16b max_depth;		/* Max depth */
	s16b depth;			/* Cur depth */
	s16b max_lev;		/* Max level */
	s16b lev;			/* Cur level */
	s32b max_exp;		/* Max experience */
	s32b exp;			/* Cur experience */
	s16b mhp;			/* Max hit pts */
	s16b chp;			/* Cur hit pts */
	s16b msp;			/* Max mana pts */
	s16b csp;			/* Cur mana pts */
	u32b turn;
	byte major_tnb, minor_tnb, patch_tnb;
} t_savefile_info;

t_savefile_info savefile_info_body, *s_info = &savefile_info_body;

static FILE     *fff;
static byte     xor_byte;
static u32b     v_check = 0L;
static u32b     x_check = 0L;

#include <setjmp.h>
static jmp_buf jmpb;
static int eof_cnt;

static bool z_older_than(byte x, byte y, byte z)
{
	/* Much older, or much more recent */
    if (s_vers_major < x) return (TRUE);
    if (s_vers_major > x) return (FALSE);

	/* Distinctly older, or distinctly more recent */
    if (s_vers_minor < y) return (TRUE);
    if (s_vers_minor > y) return (FALSE);

	/* Barely older, or barely more recent */
    if (s_vers_patch < z) return (TRUE);
    if (s_vers_patch > z) return (FALSE);

	/* Identical versions */
	return (FALSE);
}

static byte *data_head;		/* Temporary buffer */
static byte *data_next;		/* Pointer into temporary buffer */
static u32b data_size;		/* Size of buffer */

static void fill_buffer(void)
{
	/* Actually read data from the file */
	data_size = fread(data_head, 1, SAVEFILE_BUFFER_MAX, fff);

	/* Reset */
	data_next = data_head;
}

static byte get_byte(void)
{
	/* Fill/reset the buffer when empty */
	if (data_next >= data_head + data_size) fill_buffer();

	/* Handle read past EOF */
	if (!data_size)
	{
		if (eof_cnt++ > 10)
		{
			dbwin("longjmp()\n");
			/* Jump out of infinite loops */
			longjmp(jmpb, 1);
		}
		return (0);
	}
	
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
	strip_bytes(2);

	/* Location */
	strip_bytes(2);

	/* Type/Subtype */
	strip_bytes(2);

	/* Special pval */
	strip_bytes(2);

	strip_bytes(4);

	strip_bytes(4);

	strip_bytes(12);

    /* Old flags */
    strip_bytes(12);

	/* Monster holding object */
	strip_bytes(2);

	/* Special powers */
	strip_bytes(2);

	/* Feeling */
	if (s_version >= 1)
		strip_bytes(1);

	/* Inscription */
	rd_string(buf, 128);

	rd_string(buf, 128);

	if (!z_older_than(2, 2, 4))
	{
		s32b tmp32s;

		rd_s32b(&tmp32s);
		strip_bytes(tmp32s);
	}
}

static void rd_monster(monster_type *m_ptr)
{
	/* Read the monster race */
	strip_bytes(2);

	/* Read the other information */
	strip_bytes(13);

	/* Invulner */
	if (s_version >= 2)
		strip_bytes(1);

	/* Smart */
    if (!(z_major == 2 && z_minor == 0 && z_patch == 6))
        strip_bytes(4);
	strip_bytes(1);
}

static void rd_lore(int r_idx)
{
	/* Count sights/deaths/kills */
	strip_bytes(8);

	/* Count wakes and ignores */
	strip_bytes(2);

	/* Extra stuff */
	strip_bytes(2);

	/* Count drops */
	strip_bytes(2);

	/* Count spells */
	strip_bytes(2);

	/* Count blows of each type */
	strip_bytes(4);

	/* Memorize flags */
	strip_bytes(24);

	/* Read the "Racial" monster limit per level */
	strip_bytes(1);

	/* Later (?) */
	strip_bytes(3);
}

static errr rd_store(int town_number, int store_number)
{
	int j;
	byte num;

	/* Read the basic info */
	strip_bytes(7);
	rd_byte(&num);
	strip_bytes(4);

	if (!z_older_than(2, 1, 3))
	{
		/* Read last visit */
		strip_bytes(4);
	}

	/* Read the items */
	for (j = 0; j < num; j++)
	{
		object_type forge;

		/* Read the item */
		rd_item(&forge);
	}

	/* Success */
	return (0);
}

static void rd_randomizer(void)
{
	int i;

	/* Tmp */
	strip_bytes(2);
	
	/* Place */
	strip_bytes(2);
	
	/* State */
	for (i = 0; i < RAND_DEG; i++)
	{
		strip_bytes(4);
	}
}

static void rd_options(void)
{
	/*** Oops ***/

	/* Ignore old options */
	strip_bytes(16);


	/*** Special info */

	/* Read "delay_factor" */
	strip_bytes(1);

	/* Read "hitpoint_warn" */
	strip_bytes(1);


	/*** Cheating options ***/

	strip_bytes(2);

	strip_bytes(4);


	/*** Normal Options ***/

	/* Read the option flags */
	strip_bytes(32);

	/* Read the option masks */
	strip_bytes(32);


	/*** Window Options ***/

	/* Read the window flags */
	strip_bytes(32);

	/* Read the window masks */
	strip_bytes(32);
}

static void rd_ghost(void)
{
	char buf[64];

	/* Strip name */
	rd_string(buf, 64);

	/* Strip old data */
	strip_bytes(60);
}

static void rd_extra(void)
{
	char buf[80];

	int i;

	byte tmp8u;

	rd_string(s_info->full_name, 32);

	rd_string(s_info->died_from, 80);

	for (i = 0; i < 4; i++)
	{
		rd_string(buf, 60);
	}

	/* Class/Race/Gender/Spells */
	rd_byte(&s_info->prace);
	rd_byte(&s_info->pclass);
	rd_byte(&s_info->psex);
    strip_bytes(3);

	/* Special Race/Class info */
	strip_bytes(3);

	/* Age/Height/Weight */
	rd_s16b(&s_info->age);
	rd_s16b(&s_info->ht);
	rd_s16b(&s_info->wt);

	/* Read the stat info */
	strip_bytes(12);
	strip_bytes(12);

	strip_bytes(24);        /* oops */

	rd_s32b(&s_info->au);

	rd_s32b(&s_info->max_exp);
	rd_s32b(&s_info->exp);
	strip_bytes(2);

	rd_s16b(&s_info->lev);

	/* Current version */
	if (!z_older_than(2, 1, 3))
	{
		s16b tmp16s;
		
		strip_bytes(14);
		
		rd_s16b(&tmp16s);
		if (tmp16s > MAX_BACT)
		{
			dbwin("Too many (%d) building rewards!\n", tmp16s);
		}
		strip_bytes(tmp16s * 2);
	}

	/* 2.1.2 beta version */
	else if (z_major == 2 && z_minor == 1 && z_patch == 2)
	{
		strip_bytes(10 + 200 + 20 + 20 + 10 + 10);
	}

	/* 2.1.0 or older */
	else
	{
	}
	
	rd_s16b(&s_info->mhp);
	rd_s16b(&s_info->chp);
	strip_bytes(2);

	rd_s16b(&s_info->msp);
	rd_s16b(&s_info->csp);
	strip_bytes(2);

	rd_s16b(&s_info->max_lev);
	rd_s16b(&s_info->max_depth);

	/* More info */
	strip_bytes(8);
	rd_s16b(&s_info->sc);
	strip_bytes(2);

	/* Read the flags */
	strip_bytes(2); /* Old "rest" */
	strip_bytes(8);
	strip_bytes(4); /* Old "food_digested" / "protection" */
	strip_bytes(46);

	strip_bytes(24); /* tim_esp -> chaos_patron */
	strip_bytes(12); /* muta1, muta2, muta3 */

	if (s_version >= 5)
		strip_bytes(MAX_PLAYER_VIRTUES * 2 * 2);

	strip_bytes(8); /* confusing, etc */

	/* Future use */
	strip_bytes(48);

	/* Skip the flags */
	strip_bytes(12);


	/* Hack -- the two "special seeds" */
	strip_bytes(8);


	/* Special stuff */
	strip_bytes(6);


	/* Read "death" */
	rd_byte(&tmp8u);
	s_info->is_dead = tmp8u;

	/* Read "feeling" */
	strip_bytes(1);

	/* Turn of last "feeling" */
	strip_bytes(4);

	/* Current turn */
	rd_u32b(&s_info->turn);
}

static errr rd_inventory(void)
{
	object_type forge;

	/* Read until done */
	while (1)
	{
		u16b n;

		/* Get the next item index */
		rd_u16b(&n);

		/* Nope, we reached the end */
		if (n == 0xFFFF) break;

		/* Read the item */
		rd_item(&forge);
	}

	/* Success */
	return (0);
}

static void rd_messages(void)
{
	int i;
	char buf[128];

	s16b num;

	/* Total */
	rd_s16b(&num);

	/* Read the messages */
	for (i = 0; i < num; i++)
	{
		/* Read the message */
		rd_string(buf, 128);
	}
}

static errr rd_dungeon(void)
{
	int i, y, x;

	int ymax, xmax;

	byte count;
	byte tmp8u;

	u16b limit;
	u16b tmp16s;

	/*** Basic info ***/

	/* Header info */
	rd_s16b(&s_info->depth);

	/* Read the base level */
	if (!z_older_than(2, 2, 2))
	{
		strip_bytes(2);
	}
	
	strip_bytes(6);
	rd_s16b(&cur_hgt);
	rd_s16b(&cur_wid);
	strip_bytes(4);

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

	if (!z_older_than(2,1,3))
	{
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
	}

	/*** Objects ***/

	/* Read the item count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit >= max_o_idx)
	{
		dbwin("Too many (%d) object entries!\n", limit);
		return (151);
	}

	/* Read the dungeon items */
	for (i = 1; i < limit; i++)
	{
		object_type object_type_body;
		rd_item(&object_type_body);
	}

	/*** Monsters ***/

	/* Read the monster count */
	rd_u16b(&limit);

	/* Hack -- verify */
	if (limit >= max_m_idx)
	{
		dbwin("Too many (%d) monster entries!\n", limit);
		return (161);
	}

	/* Read the monsters */
	for (i = 1; i < limit; i++)
	{
		monster_type monster_type_body;
		rd_monster(&monster_type_body);
	}

	/*** Success ***/

	/* Success */
	return (0);
}

errr rd_tnb(void)
{
	byte	fake[4];

	xor_byte = 0; rd_byte(&fake[0]);
	xor_byte = 0; rd_byte(&fake[1]);
	xor_byte = 0; rd_byte(&fake[2]);
	xor_byte = 0; rd_byte(&fake[3]);
	
	/* My extra stuff isn't present */
	if (fake[0] != 'T' ||
		fake[1] != 'N' ||
		fake[2] != 'B' ||
		fake[3] != ' ') return (0);
	
	/* Get the version info */
	xor_byte = 0; rd_byte(&s_info->major_tnb);
	xor_byte = 0; rd_byte(&s_info->minor_tnb);
	xor_byte = 0; rd_byte(&s_info->patch_tnb);

	return (0);
}

static errr rd_savefile_new_aux(void)
{
	int i, j, town_count;

	byte tmp8u;
	u16b tmp16u;
	u32b tmp32u;
	
	u32b n_x_check, n_v_check;
	u32b o_x_check, o_v_check;

	/* Version */
	xor_byte = 0; rd_byte(&s_vers_major);
	xor_byte = 0; rd_byte(&s_vers_minor);
	xor_byte = 0; rd_byte(&s_vers_patch);
	xor_byte = 0; rd_byte(&s_vers_extra);

	/* Don't read older than 2.1.0 */
	if (z_older_than(2, 1, 0))
	{
		return (0);
	}

	/* Hack -- decrypt */
	xor_byte = s_vers_extra;

	/* Clear the checksums */
	v_check = 0L;
	x_check = 0L;

#if SAVEFILE_VERSION
	/* Read the version number of the savefile */
	if (!z_older_than(2, 2, 8) &&
		!(s_vers_major == 2 && s_vers_minor == 3 && s_vers_patch == 0))
	{
		rd_u32b(&s_version);
	}
	else
	{
		s_version = 0;
	}
#endif /* SAVEFILE_VERSION */

	/* Operating system info */
	rd_u32b(&s_info_xtra);

	/* Time of savefile creation */
	rd_u32b(&s_info_when);

	/* Number of resurrections */
	rd_u16b(&s_info_lives);

	/* Number of times played */
	rd_u16b(&s_info_saves);

	/* Later use (always zero) */
	rd_u32b(&tmp32u);

	/* Later use (always zero) */
	rd_u32b(&tmp32u);

	/* Read RNG state */
	rd_randomizer();

	/* Then the options */
	rd_options();

	/* Then the "messages" */
	rd_messages();

	/* Monster Memory */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > max_r_idx)
	{
		dbwin("Too many (%u) monster races!\n", tmp16u);
		return (21);
	}

	/* Read the available records */
	for (i = 0; i < tmp16u; i++)
	{
		/* Read the lore */
		rd_lore(i);
	}

	/* Object Memory */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > max_k_idx)
	{
		dbwin("Too many (%u) object kinds!\n", tmp16u);
		return (22);
	}

	/* Read the object memory */
	for (i = 0; i < tmp16u; i++)
	{
		byte tmp8u;
		rd_byte(&tmp8u);
	}

	/* 2.1.3 or newer version*/
	if (!z_older_than(2, 1, 3))
	{
		u16b max_towns_load;
		u16b max_quests_load;
		s32b wild_x_size, wild_y_size;
	
		/* Number of towns */
		rd_u16b(&max_towns_load);

		/* 2.2.2 or older version */
		if (z_older_than(2, 2, 3))
		{
			/* Ignore higher numbers of towns */
			if (max_towns_load > max_towns)
				max_towns_load = max_towns;
		}

		/* Incompatible save files */
		if (max_towns_load > max_towns)
		{
			dbwin("Too many (%u) towns!\n", max_towns_load);
			return (23);
		}

		/* Number of quests */
		rd_u16b(&max_quests_load);

		/* 2.2.3 or newer version */
		if (!z_older_than(2, 2, 3))
		{
			/* Incompatible save files */
			if (max_quests_load > max_quests)
			{
				dbwin("Too many (%u) quests!\n", max_quests_load);
				return (23);
			}
		}

		for (i = 0; i < max_quests_load; i++)
		{
			if (i < max_quests)
			{
				s16b status;
				
				rd_s16b(&status);
	
				if (!z_older_than(2, 2, 0))
				{
					strip_bytes(2);
				}
				
				/* Load quest status if quest is running */
				if (status == QUEST_STATUS_TAKEN)
				{
					strip_bytes(6);
					if (z_older_than(2, 2, 0))
					{
						strip_bytes(2);
					}
					strip_bytes(2);
	
					/* Load quest item index */
					if (!z_older_than(2, 2, 1))
					{
						strip_bytes(2);
					}

					/* Load quest flags */
					if (!z_older_than(2, 2, 3))
					{
						strip_bytes(1);
					}
	
					if (z_older_than(2, 2, 0))
					{
						strip_bytes(40);
					}
				}
			}

			/* Ignore empty quests from old versions */
			else
			{
				/* Ignore empty quests */
				strip_bytes(2);

				/* Ignore quest level */
				if (!z_older_than(2, 2, 0))
				{
					strip_bytes(2);
				}
			}
		}

		/* Only in 2.2.1 and 2.2.2 */
		if (!z_older_than(2, 2, 1) && z_older_than(2, 2, 3))
		{
			strip_bytes(2);
		}

		/* Position in the wilderness */
		strip_bytes(8);

		/* Size of the wilderness */
		rd_s32b(&wild_x_size);
		rd_s32b(&wild_y_size);

		/* Incompatible save files */
		if ((wild_x_size > max_wild_x) || (wild_y_size > max_wild_y))
		{
			dbwin("Wilderness is too big (%u/%u)!\n", wild_x_size, wild_y_size);
			return (23);
		}

		/* Load the wilderness seeds */
		for (i = 0; i < wild_x_size; i++)
		{
			for (j = 0; j < wild_y_size; j++)
			{
				rd_u32b(&tmp32u);
			}
		}
	}
	
	/* rr9: Load old savegame without the quest infos */
	else if (z_older_than(2, 1, 1))
	{
		rd_u16b(&tmp16u);
		strip_bytes(tmp16u * 4);
	}
	
	/* rr9: Load 2.1.1 savegame quest infos */
	else if (z_older_than(2, 1, 2))
	{
		rd_u16b(&tmp16u);
		strip_bytes(tmp16u * 5);
	}
	
	/* 2.1.2 beta version */
	else if (z_older_than(2, 1, 3))
	{
		/* Load the number of quests */
		rd_u16b(&tmp16u);

		/* Incompatible save files */
		if (tmp16u > 20)
		{
			dbwin("Too many (%u) quests!\n", tmp16u);
			return (23);
		}

		/* Load the quest information */
		strip_bytes(tmp16u * 14);
	}
					
	/* Load the Artifacts */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > max_a_idx)
	{
		dbwin("Too many (%u) artifacts!\n", tmp16u);
		return (24);
	}

	/* Read the artifact flags */
	for (i = 0; i < tmp16u; i++)
	{
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
	}

	/* Read the extra stuff */
	rd_extra();

	/* Read the player_hp array */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > PY_MAX_LEVEL)
	{
		dbwin("Too many (%u) hitpoint entries!\n", tmp16u);
		return (25);
	}

	/* Read the player_hp array */
	for (i = 0; i < tmp16u; i++)
	{
		strip_bytes(2);
	}

	/* Read spell info */
	strip_bytes(24);

	strip_bytes(64);


	/* Read the inventory */
	if (rd_inventory())
	{
		dbwin("Unable to read inventory\n");
		return (21);
	}

	/* Read number of towns */
	if (!z_older_than(2,1,3))
	{
		rd_u16b(&tmp16u);
		town_count = tmp16u;
	}
	else
	{
		/* Only one town */
		town_count = 2;
	}

	/* Read the stores */
	rd_u16b(&tmp16u);
	for (i = 1; i < town_count; i++)
	{
		/* HACK - ignore the empty towns */
		if (z_older_than(2, 2, 3) && (i >= 6))
		{
			for (j = 0; j < tmp16u; j++)
			{
				/* Read the info into the empty town 5 (R'Lyeh) */
				if (rd_store(5, i)) {
					dbwin("Error reading store data\n");
					return (22);
				}
			}
		}
		else
		{
			for (j = 0; j < tmp16u; j++)
			{
				if (rd_store(i, j)) {
					dbwin("Error reading store data\n");
					return (22);
				}
			}
		}
	}

	/* Read the pet command settings */
	if (s_version > 2)
	{
		strip_bytes(4);
	}
	else if (!z_older_than(2, 2, 3))
	{
		strip_bytes(3);
	}
	
	/* I'm not dead yet... */
	if (!s_info->is_dead)
	{
		/* Dead players have no dungeon */
		if (rd_dungeon())
		{
			dbwin("Error reading dungeon data\n");
			return (34);
		}

		/* Read the ghost info */
		rd_ghost();

		if (!z_older_than(2, 2, 4))
		{
			s32b tmp32s;

			rd_s32b(&tmp32s);
			strip_bytes(tmp32s);
		}
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

    /* Read savefile extension */
    if (rd_tnb()) {
        dbwin("Error reading savefile extension\n");
        return (35);
    }

	/* Success */
	return (0);
}

errr angtk_savefile_info(char *filename, char *varName)
{
	errr err;

	/* The savefile is a binary file */
	fff = my_fopen(filename, "rb");

	/* Paranoia */
	if (!fff) return (-1);

	/* Temporary file buffer */
	C_MAKE(data_head, SAVEFILE_BUFFER_MAX, byte);
	
	/* Reset */
	data_next = data_head;
	data_size = 0;
	eof_cnt = 0;

	(void) WIPE(s_info, t_savefile_info);

	/* Jump out of infinite loops */
	if (setjmp(jmpb) == 0)
	{
		/* Call the sub-function */
		err = rd_savefile_new_aux();
	}
	else
	{
		err = -1;
	}

	/* Kill the buffer */
	C_KILL(data_head, SAVEFILE_BUFFER_MAX, byte);

	/* Check for errors */
	if (ferror(fff)) err = -1;

	/* Close the file */
	my_fclose(fff);

	/* Argle Bargle, Level 23 Half-Elf Ranger, Depth 33/33, Turn 1234567 */
	if (!err) {
		ExtToUtf_SetArrayValueString(varName, "version", format("%d.%d.%d",
			s_vers_major, s_vers_minor, s_vers_patch));
		if (!z_older_than(2, 1, 0)) {
			ExtToUtf_SetArrayValueString(varName, "name", s_info->full_name);
			SetArrayValueLong(varName, "lev", s_info->lev);
			SetArrayValueLong(varName, "depth", s_info->depth);
			SetArrayValueLong(varName, "max_depth", s_info->max_depth);
			ExtToUtf_SetArrayValueString(varName, "class", (char *) class_info[s_info->pclass].title);
			ExtToUtf_SetArrayValueString(varName, "race", (char *) race_info[s_info->prace].title);
			SetArrayValueLong(varName, "turn", s_info->turn);
			SetArrayValueLong(varName, "is_dead", s_info->is_dead);
			ExtToUtf_SetArrayValueString(varName, "died_from", s_info->died_from);
		}
	}

	/* Result */
	return (err);
}

#endif /* ZANGBANDTK */
