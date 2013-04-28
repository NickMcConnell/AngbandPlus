/* File: savefile.c */

/* Purpose: savefile parsing */

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

static byte s_vers_major;			/* Savefile's "version_major" */
static byte s_vers_minor;			/* Savefile's "version_minor" */
static byte s_vers_patch;			/* Savefile's "version_patch" */
static byte s_vers_extra;			/* Savefile's "version_extra" */
static u32b s_info_xtra;			/* Operating system info */
static u32b s_info_when;			/* Time when savefile created */
static u16b s_info_lives;			/* Number of past "lives" with this file */
static u16b s_info_saves;			/* Number of "saves" during this life */

#if defined(ANGBANDTK) || defined(KANGBANDTK)
#define FILE_VERS_MAJOR s_vers_major
#define FILE_VERS_MINOR s_vers_minor
#define FILE_VERS_PATCH s_vers_patch
#endif /* */

/* Oldest version we know how to parse */

#if defined(ANGBANDTK)
#define MIN_VERS_MAJOR 2
#define MIN_VERS_MINOR 8
#define MIN_VERS_PATCH 3
#endif /* ANGBANDTK */

#if defined(KANGBANDTK)
#define MIN_VERS_MAJOR 2
#define MIN_VERS_MINOR 8
#define MIN_VERS_PATCH 3
#endif /* KANGBANDTK */

#if defined(OANGBANDTK)
#define MIN_VERS_MAJOR 2
#define MIN_VERS_MINOR 8
#define MIN_VERS_PATCH 3
#endif /* OANGBANDTK */

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

static FILE	*fff;
static byte	xor_byte;
static u32b	v_check = 0L;
static u32b	x_check = 0L;

static byte *data_head;		/* Temporary buffer */
static byte *data_next;		/* Pointer into temporary buffer */
static u32b data_size;		/* Size of buffer */

#include <setjmp.h>
static jmp_buf jmpb;
static u32b eof_cnt;

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

	/* Handle read past EOF */
	if (!data_size) return (0);
	
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

static bool newer_than(byte x, byte y, byte z)
{
	/*  */
	if (s_vers_major > x) return (TRUE);
	if (s_vers_major < x) return (FALSE);

	/*  */
	if (s_vers_minor > y) return (TRUE);
	if (s_vers_minor < y) return (FALSE);

	/*  */
	if (s_vers_patch > z) return (TRUE);
	if (s_vers_patch < z) return (FALSE);

	/* Identical versions */
	return (FALSE);
}

static bool older_than(byte x, byte y, byte z, byte e)
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

#if defined(KANGBANDTK)
	/* Barely older, or barely more recent */
	if (s_vers_extra < e) return (TRUE);
	if (s_vers_extra > e) return (FALSE);
#endif

	/* Identical versions */
	return (FALSE);
}

#if defined(OANGBANDTK)

static byte o_vers_major;
static byte o_vers_minor;
static byte o_vers_patch;
static byte o_vers_extra;

#define FILE_VERS_MAJOR o_vers_major
#define FILE_VERS_MINOR o_vers_minor
#define FILE_VERS_PATCH o_vers_patch

/*
 * This function determines if the version of the savefile
 * currently being read is older than Oangband version "x.y.z".
 * Note that savefiles from both Oangband version 0.1.0 and 
 * 0.2.0 are treated as 0.2.0.
 */
static bool o_older_than(byte x, byte y, byte z, byte e)
{
	/* Much older, or much more recent */
	if (o_vers_major < x) return (TRUE);
	if (o_vers_major > x) return (FALSE);

	/* Distinctly older, or distinctly more recent */
	if (o_vers_minor < y) return (TRUE);
	if (o_vers_minor > y) return (FALSE);

	/* Barely older, or barely more recent */
	if (o_vers_patch < z) return (TRUE);
	if (o_vers_patch > z) return (FALSE);

	/* Identical versions */
	return (FALSE);
}

#endif /* OANGBANDTK */

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

	strip_bytes(6);

	strip_bytes(2);

	strip_bytes(2);

	strip_bytes(2);

	/* Old flags */
	strip_bytes(12);

	/* Monster holding object */
	strip_bytes(2);

	/* Special powers */
	strip_bytes(2);

#if defined(OANGBANDTK)
	/* Feeling */
	if (!o_older_than(0, 4, 1, 0)) strip_bytes(1);
#endif

	/* Inscription */
	rd_string(buf, 128);
}

static void rd_monster(monster_type *m_ptr)
{
#if defined(ANGBANDTK) || defined(OANGBANDTK)
	strip_bytes(16);
#endif /* ANGBANDTK */
#if defined(KANGBANDTK)
	strip_bytes(18);
#endif /* */
#if defined(OANGBANDTK)
	if (!o_older_than(0, 5, 0, 0))
		strip_bytes(13);
#endif /* */
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

static errr rd_store(int n)
{
	byte num;
	int j;

	/* Read the basic info */
	strip_bytes(7);
	rd_byte(&num);
	strip_bytes(4);

	/* Read the items */
	for (j = 0; j < num; j++)
	{
		object_type object_type_body;

		/* Read the item */
		rd_item(&object_type_body);
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

#if defined(OANGBANDTK)

static int convert_saved_names(void)
{
	char temp[64];
	int i;

	for (i = ART_MIN_RANDOM; i < MAX_A_IDX; i++)
	{
		rd_string(temp, 64);
	}

	return 0;
}

#endif /* OANGBANDTK */

static errr rd_extra(void)
{
	char buf[80];

	int i;

	byte tmp8u;
	u16b tmp16u;
	s16b tmp16;

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
	rd_byte(&tmp8u);	/* oops */

	/* Special Race/Class info */
	rd_byte(&tmp8u);
	rd_byte(&tmp8u);

	/* Age/Height/Weight */
	rd_s16b(&s_info->age);
	rd_s16b(&s_info->ht);
	rd_s16b(&s_info->wt);

	/* Read the stat info */
	strip_bytes(12);
	strip_bytes(12);

	strip_bytes(24);	/* oops */

	rd_s32b(&s_info->au);

	rd_s32b(&s_info->max_exp);
	rd_s32b(&s_info->exp);
	rd_u16b(&tmp16u);

	rd_s16b(&s_info->lev);

#if defined(KANGBANDTK)

	/* Plot index */
	strip_bytes(2);

	/* read arena information */
	strip_bytes(2);

	strip_bytes(2); /* inside_arena */

	if (!older_than(2, 9, 2, 0))
		strip_bytes(2); /* inside_quest */

	strip_bytes(2); /* exit_bldg, leftbldg */

	/* 2.9.2 Changed quest handling */
	if (older_than(2, 9, 2, 0))
	{
		for (i = 0; i < 100; i++) strip_bytes(2);
		for (i = 0; i < 10; i++) strip_bytes(2);
		for (i = 0; i < 10; i++) strip_bytes(2);
		for (i = 0; i < 5; i++) strip_bytes(2);
		for (i = 0; i < 5; i++) strip_bytes(2);
	}

#endif /* KANGBANDTK */

	rd_s16b(&s_info->mhp);
	rd_s16b(&s_info->chp);
	rd_u16b(&tmp16u);

	rd_s16b(&s_info->msp);
	rd_s16b(&s_info->csp);
	rd_u16b(&tmp16u);

	rd_s16b(&s_info->max_lev);
	rd_s16b(&s_info->max_depth);

	/* More info */
	strip_bytes(8);
	rd_s16b(&s_info->sc);
	strip_bytes(2);

	/* Read the flags */
	strip_bytes(2);	/* Old "rest" */
	strip_bytes(8);
	strip_bytes(4);	/* Old "food_digested" / "protection" */
	strip_bytes(46);
#if defined(OANGBANDTK)
	if (!o_older_than(0, 3, 5, 0))
	{
		strip_bytes(6); /* tim_esp, superstealth, ele_attack */
	}
#endif
#if defined(KANGBANDTK)
	if (!older_than(2, 9, 1, 1))
	{
		/* tim_levitate */
		strip_bytes(2);
	}

	strip_bytes(28);
#endif /* KANGBANDTK */

#if defined(ANGBANDTK) || defined(KANGBANDTK)
	strip_bytes(8);
#endif
#if defined(OANGBANDTK)
	if (!o_older_than(0, 3, 5, 0))
	{
		strip_bytes(4);
	}
	else
	{
		strip_bytes(1);
	}

	strip_bytes(16);
#endif

#if defined(ANGBANDTK) || defined(KANGBANDTK)
	/* Future use */
	strip_bytes(40);

	/* Randart version & seed */
	strip_bytes(8);
#endif
#if defined(OANGBANDTK)
	/* Future use */
	strip_bytes(39);
#endif

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
	rd_byte(&tmp8u);

	/* Turn of last "feeling" */
	strip_bytes(4);

	/* Current turn */
	rd_u32b(&s_info->turn);

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
		rd_s16b(&tmp16);
	}

	/* Read spell info */
	strip_bytes(24);
	strip_bytes(64);

	return (0);
}

static errr rd_inventory(void)
{
	object_type object_type_body;

	/* Read until done */
	while (1)
	{
		u16b n;

		/* Get the next item index */
		rd_u16b(&n);

		/* Nope, we reached the end */
		if (n == 0xFFFF) break;

		/* Read the item */
		rd_item(&object_type_body);
	}

	/* Success */
	return (0);
}

static void rd_messages(void)
{
	int i;
	char buf[128];
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	u16b tmp16u;
#endif /* */

	s16b num;

	/* Total */
	rd_s16b(&num);

	/* Read the messages */
	for (i = 0; i < num; i++)
	{
		/* Read the message */
		rd_string(buf, 128);

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
		/* Read the message type */
		if (!older_than(2, 9, 1, 0))
			rd_u16b(&tmp16u);
#endif /* A */
	}
}

errr rd_tnb(void)
{
	byte fake[4];
	
	xor_byte = 0; rd_byte(&fake[0]);
	xor_byte = 0; rd_byte(&fake[1]);
	xor_byte = 0; rd_byte(&fake[2]);
	xor_byte = 0; rd_byte(&fake[3]);

dbwin("rd_tnb %c %c %c %c\n", fake[0], fake[1], fake[2], fake[3]);

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

static errr rd_dungeon(void)
{
	int i, y, x;

	s16b depth;
	s16b py, px;
	s16b ymax, xmax;

	byte count;
	byte tmp8u;
#if defined(KANGBANDTK)
	s16b tmp16s;
#endif
	u16b tmp16u;

	u16b limit;


	/*** Basic info ***/

	/* Header info */
	rd_s16b(&depth);
	rd_u16b(&tmp16u);
	rd_s16b(&py);
	rd_s16b(&px);
#if defined(KANGBANDTK)
	rd_u16b(&tmp16u); /* oldpy */
	rd_u16b(&tmp16u); /* oldpx */
#endif /* KANGBANDTK */
	rd_s16b(&ymax);
	rd_s16b(&xmax);
	rd_u16b(&tmp16u);
	rd_u16b(&tmp16u);


	/* Ignore illegal dungeons */
	if ((depth < 0) || (depth >= MAX_DEPTH))
	{
		dbwin("Ignoring illegal dungeon depth (%d)\n", depth);
		return (0);
	}

	/* Ignore illegal dungeons */
	if ((ymax != DUNGEON_HGT) || (xmax != DUNGEON_WID))
	{
		/* XXX XXX XXX */
		dbwin("Ignoring illegal dungeon size (%d,%d).\n", xmax, ymax);
		return (0);
	}

	/* Ignore illegal dungeons */
	if ((px < 0) || (px >= DUNGEON_WID) ||
	    (py < 0) || (py >= DUNGEON_HGT))
	{
		dbwin("Ignoring illegal player location (%d,%d).\n", px, py);
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

#if defined(KANGBANDTK)

	/*** Run length decoding ***/

	if (!older_than(2, 9, 2, 0))
	{
		/* Load the dungeon data */
		for (x = y = 0; y < DUNGEON_HGT; )
		{
			/* Grab RLE info */
			rd_byte(&count);
			rd_s16b(&tmp16s);

			/* Apply the RLE info */
			for (i = count; i > 0; i--)
			{
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
	}

#endif /* KANGBANDTK */

	/*** Player ***/

	/* Save depth */
	s_info->depth = depth;


	/*** Objects ***/

	/* Read the item count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit >= MAX_O_IDX)
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
	if (limit >= MAX_M_IDX)
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

	/* Success */
	return (0);
}

static errr rd_savefile_new_aux(void)
{
	int i;

#if defined(KANGBANDTK)
	s16b tmp16s;
#endif
	u16b tmp16u;
	u32b tmp32u;

	u32b n_x_check, n_v_check;
	u32b o_x_check, o_v_check;

#if defined(OANGBANDTK)
	int total_artifacts;
	int random_artifacts = 0;
	bool need_random_artifacts = FALSE;
#endif /* */

	/* Version */
	xor_byte = 0; rd_byte(&s_vers_major);
	xor_byte = 0; rd_byte(&s_vers_minor);
	xor_byte = 0; rd_byte(&s_vers_patch);
	xor_byte = 0; rd_byte(&s_vers_extra);

dbwin("version %d.%d.%d\n", s_vers_major, s_vers_minor, s_vers_patch);

	/* Ignore non-savefiles */
	if (newer_than(VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH))
	{
		return (1);
	}

	/* Ignore "obsolete" versions */
	if (older_than(MIN_VERS_MAJOR, MIN_VERS_MINOR, MIN_VERS_PATCH, 0))
	{
		return (0);
	}

	/* Hack -- decrypt */
	xor_byte = s_vers_extra;

	/* Clear the checksums */
	v_check = 0L;
	x_check = 0L;

	/* Operating system info */
	rd_u32b(&s_info_xtra);

	/* Time of savefile creation */
	rd_u32b(&s_info_when);

	/* Number of resurrections */
	rd_u16b(&s_info_lives);

	/* Number of times played */
	rd_u16b(&s_info_saves);

#if defined(ANGBANDTK) || defined(KANGBANDTK)
	/* Later use (always zero) */
	rd_u32b(&tmp32u);
#endif /* */
#if defined(OANGBANDTK)
	rd_byte(&o_vers_major);
	rd_byte(&o_vers_minor);
	rd_byte(&o_vers_patch);
	rd_byte(&o_vers_extra);

dbwin("o_vers=%d.%d.%d\n", o_vers_major, o_vers_minor, o_vers_patch);
	if (o_vers_minor == 0) o_vers_minor = 2;
#endif /* */

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
	if (tmp16u > MAX_R_IDX)
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

dbwin("number of monster races %d\n", tmp16u);

	/* Object Memory */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > MAX_K_IDX)
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

dbwin("number of objects %d\n", tmp16u);

	/* Load the Quests */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
#if defined(ANGBANDTK)
	if (tmp16u > 4)
#endif /* ANGBANDTK */
#if defined(KANGBANDTK)
	if (tmp16u > MAX_Q_IDX)
#endif /* */
#if defined(OANGBANDTK)
	if (tmp16u > MAX_Q_IDX)
#endif /* */
	{
		dbwin("Too many (%u) quests!\n", tmp16u);
		return (23);
	}

	/* Load the Quests */
#if defined(ANGBANDTK) || defined(OANGBANDTK)
	for (i = 0; i < tmp16u; i++)
	{
		strip_bytes(4);
	}
#endif /* ANGBANDTK, OANGBANDTK */
#if defined(KANGBANDTK)
	if (older_than(2, 9, 2, 0))
	{
		for (i = 0; i < tmp16u; i++)
		{
			strip_bytes(14);
		}
	}
	else
	{
		for (i = 0; i < tmp16u; i++)
		{
			rd_s16b(&tmp16s);
			strip_bytes(2);

			if (tmp16s == QUEST_STATUS_TAKEN)
			{
				strip_bytes(11);
			}
		}
	}
#endif /* KANGBANDTK */

#if defined(ANGBANDTK) || defined(KANGBANDTK)

	/* Load the Artifacts */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > MAX_A_IDX)
	{
		dbwin("Too many (%u) artifacts!\n", tmp16u);
		return (24);
	}

	/* Read the artifact flags */
	for (i = 0; i < tmp16u; i++)
	{
		strip_bytes(4);
	}

dbwin("number of artifacts %d\n", tmp16u);

#endif /* */
#if defined(OANGBANDTK)

	/* Load the Artifacts */
	rd_u16b(&tmp16u);
	total_artifacts = tmp16u;
	
	/* If an Oangband 0.3.0 savefile or newer, load the random artifacts. 
	 * Although this value is not currently used, it may be useful someday.
	 */
	if (!(o_older_than(0,3,0, 0)))
	{
		rd_u16b(&tmp16u);
		random_artifacts = tmp16u;
	}

dbwin("total_artifacts=%d random_artifacts=%d\n", total_artifacts, random_artifacts);

	/* Incompatible save files */
	if (total_artifacts > MAX_A_IDX)
	{
		dbwin("Too many (%u) artifacts!\n", total_artifacts);
		return (24);
	}

	/* Reading an old savefile, with no random artifacts. */
	if (o_older_than(0,3,0, 0))
	{
		/* Read the artifact flags */
		for (i = 0; i < total_artifacts; i++)
		{
			strip_bytes(4);
		}

		/* Later, we need to create the random artifacts, just as if 
		 * the player were being born.
		 */
		need_random_artifacts = TRUE;
	}

	/* Reading a Oangband 0.3.0+ savefile, with random artifacts. */
	else
	{
		/* Read the artifact info. */
		for (i = 0; i < total_artifacts; i++)
		{
			/* Most regular artifact info is stored in a_info.raw.  0.3.X 
			 * savefiles have 127 regular artifacts, with indexes that need 
			 * converting, and newer savefiles have 209, with up-to-date 
			 * indexes.
			 */
#if defined(OANGBANDTK)
			if (i < (o_older_than(0,3,6, 0) ? 128 : ART_MIN_RANDOM))
#endif
			{
				strip_bytes(4);
			}
			/* But random artifacts are specific to each player. */
			else
			{
				strip_bytes(44);
			}
		}
	}


	/* If random artifacts do not need to be generated, read the 
	 * random artifact names, and add them to the a_name array.
	 */
	if (!need_random_artifacts)
	{
		u16b num_of_random_arts;

		/* Find out how many random artifacts have stored names. */
		rd_u16b(&num_of_random_arts);


		/* Verify that number, and warn the chap who modified the game that 
		 * he still has work to do on failure.
		 */
		if (num_of_random_arts != (MAX_A_IDX - ART_MIN_RANDOM))
		{
			dbwin("Number of stored random artifact names (%d) "
				"does not match the number of random artifacts in your "
				"copy of Oangband (%d).\n", num_of_random_arts, 
				MAX_A_IDX - ART_MIN_RANDOM);
		}

		/* Otherwise, add the new names to the a_name structure. */
		else
		{
			int err = convert_saved_names();

			/* Complain if naming fails. */
			if (err) dbwin("Warning - random artifact naming failed!\n");
		}
	}

#endif /* OANGBANDTK */

	/* Read the extra stuff */
	if (rd_extra()) return (25);

dbwin("player name %s\n", s_info->full_name);

	/* Read the inventory */
	if (rd_inventory())
	{
		dbwin("Unable to read inventory\n");
		return (21);
	}

	/* Read the stores */
	rd_u16b(&tmp16u);
dbwin("number of stores %d\n", tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		if (rd_store(i)) return (22);
	}

#if defined(KANGBANDTK)
	/* Read the pet command settings */
	if (!older_than(2, 9, 2, 2))
	{
		strip_bytes(4);
	}
#endif /* KANGBANDTK */

	/* I'm not dead yet... */
	if (!s_info->is_dead)
	{
		/* Dead players have no dungeon */
		if (rd_dungeon())
		{
			dbwin("Error reading dungeon data\n");
			return (34);
		}
dbwin("rd_dungeon okay\n");

		/* Read the ghost info */
		rd_ghost();
	}

	/* Save the checksum */
	n_v_check = v_check;

	/* Read the old checksum */
	rd_u32b(&o_v_check);

	/* Verify */
	if (o_v_check != n_v_check)
	{
		dbwin("Invalid checksum old=%lu new=%lu\n", o_v_check, n_v_check);
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
    if (rd_tnb())
    {
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
	if (!err)
	{
		ExtToUtf_SetArrayValueString(varName, "version", format("%d.%d.%d",
			FILE_VERS_MAJOR, FILE_VERS_MINOR, FILE_VERS_PATCH));
		if (!older_than(MIN_VERS_MAJOR, MIN_VERS_MINOR, MIN_VERS_PATCH, 0))
		{
			ExtToUtf_SetArrayValueString(varName, "name", s_info->full_name);
			SetArrayValueLong(varName, "lev", s_info->lev);
			SetArrayValueLong(varName, "depth", s_info->depth);
			SetArrayValueLong(varName, "max_depth", s_info->max_depth);
			ExtToUtf_SetArrayValueString(varName, "class",
				(char *) class_info[s_info->pclass].title);
			ExtToUtf_SetArrayValueString(varName, "race",
				(char *) p_name + p_info[s_info->prace].name);
			SetArrayValueLong(varName, "turn", s_info->turn);
			SetArrayValueLong(varName, "is_dead", s_info->is_dead);
			ExtToUtf_SetArrayValueString(varName, "died_from",
				s_info->died_from);
		}
	}

	/* Result */
	return (err);
}

#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */
