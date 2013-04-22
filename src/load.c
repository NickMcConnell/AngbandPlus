/* File: load.c */

/* Purpose: support for loading savefiles -BEN- */

#include "angband.h"


/*
 * This file loads savefiles from Angband 2.7.X and 2.8.X
 *
 * Ancient savefiles (pre-2.7.0) are loaded by another file.
 *
 * Note that Angband 2.7.0 through 2.7.3 are now officially obsolete,
 * and savefiles from those versions may not be successfully converted.
 *
 * We attempt to prevent corrupt savefiles from inducing memory errors.
 *
 * Note that this file should not use the random number generator, the
 * object flavors, the visual attr/char mappings, or anything else which
 * is initialized *after* or *during* the "load character" function.
 *
 * This file assumes that the monster/object records are initialized
 * to zero, and the race/kind tables have been loaded correctly.  The
 * order of object stacks is currently not saved in the savefiles, but
 * the "next" pointers are saved, so all necessary knowledge is present.
 *
 * We should implement simple "savefile extenders" using some form of
 * "sized" chunks of bytes, with a {size,type,data} format, so everyone
 * can know the size, interested people can know the type, and the actual
 * data is available to the parsing routines that acknowledge the type.
 *
 * Consider changing the "globe of invulnerability" code so that it
 * takes some form of "maximum damage to protect from" in addition to
 * the existing "number of turns to protect for", and where each hit
 * by a monster will reduce the shield by that amount.
 *
 * XXX XXX XXX
 */



/*
 * Maximum number of tries for selection of a proper quest monster
 */
#define MAX_TRIES 100


/*
 * Local "savefile" pointer
 */
static FILE	*fff;

/*
 * Hack -- old "encryption" byte
 */
static byte	xor_byte;

/*
 * Hack -- simple "checksum" on the actual values
 */
static u32b	v_check = 0L;

/*
 * Hack -- simple "checksum" on the encoded bytes
 */
static u32b	x_check = 0L;



/*
 * This function determines if the version of the savefile
 * currently being read is older than version "x.y.z".
 */
static bool older_than(byte x, byte y, byte z)
{
	/* Much older, or much more recent */
	if (z_major < x) return (TRUE);
	if (z_major > x) return (FALSE);

	/* Distinctly older, or distinctly more recent */
	if (z_minor < y) return (TRUE);
	if (z_minor > y) return (FALSE);

	/* Barely older, or barely more recent */
	if (z_patch < z) return (TRUE);
	if (z_patch > z) return (FALSE);

	/* Identical versions */
	return (FALSE);
}


/*
 * Hack -- Show information on the screen, one line at a time.
 *
 * Avoid the top two lines, to avoid interference with "msg_print()".
 */
static void note(cptr msg)
{
	static int y = 2;

	/* Draw the message */
	prt(msg, y, 0);

	/* Advance one line (wrap if needed) */
	if (++y >= 24) y = 2;

	/* Flush it */
	Term_fresh();
}


/*
 * Hack -- determine if an item is "wearable" (or a missile)
 */
static bool wearable_p(object_type *o_ptr)
{
	/* Valid "tval" codes */
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_LITE:
		case TV_AMULET:
		case TV_RING:
		{
			return (TRUE);
		}
	}

	/* Nope */
	return (FALSE);
}


/*
 * Hack -- determine if an item is a "weapon" (or a missile)
 */
static bool is_weapon(object_type *o_ptr)
{
	/* Valid "tval" codes */
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		case TV_BOW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			return (TRUE);
		}
	}

	/* Nope */
	return (FALSE);
}


/*
 * The following functions are used to load the basic building blocks
 * of savefiles.  They also maintain the "checksum" info for 2.7.0+
 */

static byte sf_get(void)
{
	byte c, v;

	/* Get a character, decode the value */
	c = getc(fff) & 0xFF;
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

static void rd_bool(bool *ip) /* Prfnoff */
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


/*
 * Old pre-2.7.4 inventory slot values
 */
#define OLD_INVEN_WIELD     22
#define OLD_INVEN_HEAD      23
#define OLD_INVEN_NECK      24
#define OLD_INVEN_BODY      25
#define OLD_INVEN_ARM       26
#define OLD_INVEN_HANDS     27
#define OLD_INVEN_RIGHT     28
#define OLD_INVEN_LEFT      29
#define OLD_INVEN_FEET      30
#define OLD_INVEN_OUTER     31
#define OLD_INVEN_LITE      32
#define OLD_INVEN_AUX       33

/*
 * Analyze pre-2.7.4 inventory slots
 */
static s16b convert_slot(int old)
{
	/* Move slots */
	switch (old)
	{
		case OLD_INVEN_WIELD: return (INVEN_WIELD);
		case OLD_INVEN_HEAD: return (INVEN_HEAD);
		case OLD_INVEN_NECK: return (INVEN_NECK);
		case OLD_INVEN_BODY: return (INVEN_BODY);
		case OLD_INVEN_ARM: return (INVEN_ARM);
		case OLD_INVEN_HANDS: return (INVEN_HANDS);
		case OLD_INVEN_RIGHT: return (INVEN_RIGHT);
		case OLD_INVEN_LEFT: return (INVEN_LEFT);
		case OLD_INVEN_FEET: return (INVEN_FEET);
		case OLD_INVEN_OUTER: return (INVEN_OUTER);
		case OLD_INVEN_LITE: return (INVEN_LITE);

		/* Hack -- "hold" old aux items */
		case OLD_INVEN_AUX: return (INVEN_WIELD - 1);
	}

	/* Default */
	return (old);
}





/*
 * Hack -- convert pre-2.7.8 ego-item indexes
 */
static byte convert_ego_item[128] =
{
	0,                      /* 0 */
	EGO_RESISTANCE,         /* 1 = EGO_RESIST (XXX) */
	EGO_RESIST_ACID,        /* 2 = EGO_RESIST_A (XXX) */
	EGO_RESIST_FIRE,        /* 3 = EGO_RESIST_F (XXX) */
	EGO_RESIST_COLD,        /* 4 = EGO_RESIST_C (XXX) */
	EGO_RESIST_ELEC,        /* 5 = EGO_RESIST_E (XXX) */
	EGO_HA,                 /* 6 = EGO_HA */
	EGO_DF,                 /* 7 = EGO_DF */
	EGO_SLAY_ANIMAL,        /* 8 = EGO_SLAY_ANIMAL */
	EGO_SLAY_DRAGON,        /* 9 = EGO_SLAY_DRAGON */
	EGO_SLAY_EVIL,          /* 10 = EGO_SLAY_EVIL (XXX) */
	EGO_SLAY_UNDEAD,        /* 11 = EGO_SLAY_UNDEAD (XXX) */
	EGO_BRAND_FIRE,         /* 12 = EGO_FT */
	EGO_BRAND_COLD,         /* 13 = EGO_FB */
	EGO_FREE_ACTION,        /* 14 = EGO_FREE_ACTION (XXX) */
	EGO_SLAYING,            /* 15 = EGO_SLAYING */
	0,                      /* 16 */
	0,                      /* 17 */
	EGO_SLOW_DESCENT,       /* 18 = EGO_SLOW_DESCENT */
	EGO_SPEED,              /* 19 = EGO_SPEED */
	EGO_STEALTH,            /* 20 = EGO_STEALTH (XXX) */
	0,                      /* 21 */
	0,                      /* 22 */
	0,                      /* 23 */
	EGO_INTELLIGENCE,       /* 24 = EGO_INTELLIGENCE */
	EGO_WISDOM,             /* 25 = EGO_WISDOM */
	EGO_INFRAVISION,        /* 26 = EGO_INFRAVISION */
	EGO_MIGHT,              /* 27 = EGO_MIGHT */
	EGO_LORDLINESS,         /* 28 = EGO_LORDLINESS */
	EGO_MAGI,               /* 29 = EGO_MAGI (XXX) */
	EGO_BEAUTY,             /* 30 = EGO_BEAUTY */
	EGO_SEEING,             /* 31 = EGO_SEEING (XXX) */
	EGO_REGENERATION,       /* 32 = EGO_REGENERATION */
	0,                      /* 33 */
	0,                      /* 34 */
	0,                      /* 35 */
	0,                      /* 36 */
	0,                      /* 37 */
	EGO_PERMANENCE,         /* 38 = EGO_ROBE_MAGI */
	EGO_PROTECTION,         /* 39 = EGO_PROTECTION */
	0,                      /* 40 */
	0,                      /* 41 */
	0,                      /* 42 */
	EGO_BRAND_FIRE,         /* 43 = EGO_FIRE (XXX) */
	EGO_HURT_EVIL,          /* 44 = EGO_AMMO_EVIL */
	EGO_HURT_DRAGON,        /* 45 = EGO_AMMO_DRAGON */
	0,                      /* 46 */
	0,                      /* 47 */
	0,                      /* 48 */
	0,                      /* 49 */
	EGO_FLAME,              /* 50 = EGO_AMMO_FIRE */
	0,                      /* 51 */        /* oops */
	EGO_FROST,              /* 52 = EGO_AMMO_SLAYING */
	0,                      /* 53 */
	0,                      /* 54 */
	EGO_HURT_ANIMAL,        /* 55 = EGO_AMMO_ANIMAL */
	0,                      /* 56 */
	0,                      /* 57 */
	0,                      /* 58 */
	0,                      /* 59 */
	EGO_EXTRA_MIGHT,        /* 60 = EGO_EXTRA_MIGHT */
	EGO_EXTRA_SHOTS,        /* 61 = EGO_EXTRA_SHOTS */
	0,                      /* 62 */
	0,                      /* 63 */
	EGO_VELOCITY,           /* 64 = EGO_VELOCITY */
	EGO_ACCURACY,           /* 65 = EGO_ACCURACY */
	0,                      /* 66 */
	EGO_SLAY_ORC,           /* 67 = EGO_SLAY_ORC */
	EGO_POWER,              /* 68 = EGO_POWER */
	0,                      /* 69 */
	0,                      /* 70 */
	EGO_WEST,               /* 71 = EGO_WEST */
	EGO_BLESS_BLADE,        /* 72 = EGO_BLESS_BLADE */
	EGO_SLAY_DEMON,         /* 73 = EGO_SLAY_DEMON */
	EGO_SLAY_TROLL,         /* 74 = EGO_SLAY_TROLL */
	0,                      /* 75 */
	0,                      /* 76 */
	EGO_WOUNDING,           /* 77 = EGO_AMMO_WOUNDING */
	0,                      /* 78 */
	0,                      /* 79 */
	0,                      /* 80 */
	EGO_LITE,               /* 81 = EGO_LITE */
	EGO_AGILITY,            /* 82 = EGO_AGILITY */
	0,                      /* 83 */
	0,                      /* 84 */
	EGO_SLAY_GIANT,         /* 85 = EGO_SLAY_GIANT */
	EGO_TELEPATHY,          /* 86 = EGO_TELEPATHY */
	EGO_ELVENKIND,          /* 87 = EGO_ELVENKIND (XXX) */
	0,                      /* 88 */
	0,                      /* 89 */
	EGO_ATTACKS,            /* 90 = EGO_ATTACKS */
	EGO_AMAN,               /* 91 = EGO_AMAN */
	0,                      /* 92 */
	0,                      /* 93 */
	0,                      /* 94 */
	0,                      /* 95 */
	0,                      /* 96 */
	0,                      /* 97 */
	0,                      /* 98 */
	0,                      /* 99 */
	0,                      /* 100 */
	0,                      /* 101 */
	0,                      /* 102 */
	0,                      /* 103 */
	EGO_WEAKNESS,           /* 104 = EGO_WEAKNESS */
	EGO_STUPIDITY,          /* 105 = EGO_STUPIDITY */
	EGO_NAIVETY,            /* 106 = EGO_DULLNESS */
	EGO_SICKLINESS,         /* 107 = EGO_SICKLINESS */
	EGO_CLUMSINESS,         /* 108 = EGO_CLUMSINESS */
	EGO_UGLINESS,           /* 109 = EGO_UGLINESS */
	EGO_TELEPORTATION,      /* 110 = EGO_TELEPORTATION */
	0,                      /* 111 */
	EGO_IRRITATION,         /* 112 = EGO_IRRITATION */
	EGO_VULNERABILITY,      /* 113 = EGO_VULNERABILITY */
	EGO_ENVELOPING,         /* 114 = EGO_ENVELOPING */
	0,                      /* 115 */
	EGO_SLOWNESS,           /* 116 = EGO_SLOWNESS */
	EGO_NOISE,              /* 117 = EGO_NOISE */
	EGO_ANNOYANCE,          /* 118 = EGO_GREAT_MASS */
	0,                      /* 119 */
	EGO_BACKBITING,         /* 120 = EGO_BACKBITING */
	0,                      /* 121 */
	0,                      /* 122 */
	0,                      /* 123 */
	EGO_MORGUL,             /* 124 = EGO_MORGUL */
	0,                      /* 125 */
	EGO_SHATTERED,          /* 126 = EGO_SHATTERED */
	EGO_BLASTED             /* 127 = EGO_BLASTED (XXX) */
};


/*
 * Read an object
 *
 * This function attempts to "repair" old savefiles, and to extract
 * the most up to date values for various object fields.
 *
 * Note that Angband 2.7.9 introduced a new method for object "flags"
 * in which the "flags" on an object are actually extracted when they
 * are needed from the object kind, artifact index, ego-item index,
 * and two special "xtra" fields which are used to encode any "extra"
 * power of certain ego-items.  This had the side effect that items
 * imported from pre-2.7.9 savefiles will lose any "extra" powers they
 * may have had, and also, all "uncursed" items will become "cursed"
 * again, including Calris, even if it is being worn at the time.  As
 * a complete hack, items which are inscribed with "uncursed" will be
 * "uncursed" when imported from pre-2.7.9 savefiles.
 */
static void rd_item(object_type *o_ptr)
{
	byte old_dd;
	byte old_ds;

	u32b f1, f2, f3;

	object_kind *k_ptr;

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

	rd_byte(&old_dd);
	rd_byte(&old_ds);

	rd_byte(&o_ptr->ident);

	rd_byte(&o_ptr->marked);

	rd_u32b(&o_ptr->art_flags1);
	rd_u32b(&o_ptr->art_flags2);
	rd_u32b(&o_ptr->art_flags3);

	/* Monster holding object */
	rd_s16b(&o_ptr->held_m_idx);

	/* Special powers */
	rd_byte(&o_ptr->xtra1);
	rd_byte(&o_ptr->xtra2);

	if (!older_than(2, 3, 1))
	{
		/* Feelings */
		rd_byte(&o_ptr->feeling);

		/* Inscription */
		rd_string(buf, 128);
	}
	else
	{
		byte i;

		/* Inscription */
		rd_string(buf, 128);

		for (i = 0; i <= FEEL_MAX; i++)
		{
			if (game_inscriptions[i] == NULL)
			{
				continue;
			}
			
			if (streq(buf, game_inscriptions[i])) 
			{
				o_ptr->feeling = i;
				buf[0] = 0;
				break;
			}
		}
	}

	/* Save the inscription */
	if (buf[0]) o_ptr->note = quark_add(buf);

	rd_string(buf, 128);
	if (buf[0]) o_ptr->art_name = quark_add(buf);

	/* The Python object */
	if (!older_than(2, 2, 4))
	{
		s32b tmp32s;

		rd_s32b(&tmp32s);
#ifdef USE_SCRIPT
		if (tmp32s)
		{
			char *python_object = (char*) malloc(tmp32s + 1);
			rd_string(python_object, tmp32s + 1);
			o_ptr->python = object_load_callback(python_object);
			free(python_object);
		}
#else /* USE_SCRIPT */
		strip_bytes(tmp32s);
#endif /* USE_SCRIPT */
	}

	/* Mega-Hack -- handle "dungeon objects" later */
	if ((o_ptr->k_idx >= 445) && (o_ptr->k_idx <= 479)) return;

	/* Obtain the "kind" template */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Obtain tval/sval from k_info */
	o_ptr->tval = k_ptr->tval;
	o_ptr->sval = k_ptr->sval;

	/* For rod-stacking */
	if (older_than(2, 2, 5) && (o_ptr->tval == TV_ROD))
	{
		o_ptr->timeout = o_ptr->pval * o_ptr->number;
		o_ptr->pval = k_ptr->pval * o_ptr->number;
	}

	/* Hack -- notice "broken" items */
	if (k_ptr->cost <= 0) o_ptr->ident |= (IDENT_BROKEN);


	/* Repair non "wearable" items */
	if (!wearable_p(o_ptr))
	{
		/* Acquire correct fields */
		o_ptr->to_h = k_ptr->to_h;
		o_ptr->to_d = k_ptr->to_d;
		o_ptr->to_a = k_ptr->to_a;

		/* Acquire correct fields */
		o_ptr->ac = k_ptr->ac;
		o_ptr->dd = k_ptr->dd;
		o_ptr->ds = k_ptr->ds;

		/* Acquire correct weight */
		o_ptr->weight = k_ptr->weight;

		/* Paranoia */
		o_ptr->name1 = o_ptr->name2 = 0;

		/* All done */
		return;
	}


	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);


	/* Paranoia */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr;

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->name1];

		/* Verify that artifact */
		if (!a_ptr->name) o_ptr->name1 = 0;
	}

	/* Paranoia */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr;

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->name2];

		/* Verify that ego-item */
		if (!e_ptr->name) o_ptr->name2 = 0;
	}


	/* Acquire standard fields */
	o_ptr->ac = k_ptr->ac;
	o_ptr->dd = k_ptr->dd;
	o_ptr->ds = k_ptr->ds;

	/* Acquire standard weight */
	o_ptr->weight = k_ptr->weight;

	/* Hack -- extract the "broken" flag */
	if (!o_ptr->pval < 0) o_ptr->ident |= (IDENT_BROKEN);


	/* Artifacts */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr;

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->name1];

		/* Acquire new artifact "pval" */
		o_ptr->pval = a_ptr->pval;

		/* Acquire new artifact fields */
		o_ptr->ac = a_ptr->ac;
		o_ptr->dd = a_ptr->dd;
		o_ptr->ds = a_ptr->ds;

		/* Acquire new artifact weight */
		o_ptr->weight = a_ptr->weight;

		/* Hack -- extract the "broken" flag */
		if (!a_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);
	}

	/* Ego items */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr;

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->name2];


		o_ptr->dd = old_dd;
		o_ptr->ds = old_ds;

		/* Hack -- extract the "broken" flag */
		if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);
	}

	if (o_ptr->art_name) /* A random artifact */
	{
		o_ptr->dd = old_dd;
		o_ptr->ds = old_ds;
	}

	/* Change shattered weapons from 0d0 to 1d1 */
	if (is_weapon(o_ptr))
	{
		if (o_ptr->dd == 0) o_ptr->dd = 1;
		if (o_ptr->ds == 0) o_ptr->ds = 1;
	}
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

	/* Monster invulnerability introduced from 2.3.3+ */
	if (older_than(2, 3, 3))
		m_ptr->invulner = 0;
	else
		rd_byte(&m_ptr->invulner);

	if (!(z_major == 2 && z_minor == 0 && z_patch == 6))
		rd_u32b(&m_ptr->smart);
	else
		m_ptr->smart = 0;
	rd_byte(&tmp8u);
}





/*
 * Read the monster lore
 */
static void rd_lore(int r_idx)
{
	byte tmp8u;

	monster_race *r_ptr = &r_info[r_idx];


	/* Pre-2.2.0 (old r_info.txt) */
	if (older_than(2, 2, 0))
	{
		/* Throw away old info */
		strip_bytes(48);
	}

	/* Current */
	else
	{
		/* Count sights/deaths/kills */
		rd_s16b(&r_ptr->r_sights);
		rd_s16b(&r_ptr->r_deaths);
		rd_s16b(&r_ptr->r_pkills);
		rd_s16b(&r_ptr->r_tkills);

		/* Count wakes and ignores */
		rd_byte(&r_ptr->r_wake);
		rd_byte(&r_ptr->r_ignore);

		/* Extra stuff */
		rd_byte(&r_ptr->r_xtra1);
		rd_byte(&r_ptr->r_xtra2);

		/* Count drops */
		rd_byte(&r_ptr->r_drop_gold);
		rd_byte(&r_ptr->r_drop_item);

		/* Count spells */
		rd_byte(&r_ptr->r_cast_inate);
		rd_byte(&r_ptr->r_cast_spell);

		/* Count blows of each type */
		rd_byte(&r_ptr->r_blows[0]);
		rd_byte(&r_ptr->r_blows[1]);
		rd_byte(&r_ptr->r_blows[2]);
		rd_byte(&r_ptr->r_blows[3]);

		/* Memorize flags */
		rd_u32b(&r_ptr->r_flags1);
		rd_u32b(&r_ptr->r_flags2);
		rd_u32b(&r_ptr->r_flags3);
		rd_u32b(&r_ptr->r_flags4);
		rd_u32b(&r_ptr->r_flags5);
		rd_u32b(&r_ptr->r_flags6);


		/* Read the "Racial" monster limit per level */
		rd_byte(&r_ptr->max_num);

		/* Later (?) */
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
	}

	/* Repair the lore flags */
	r_ptr->r_flags1 &= r_ptr->flags1;
	r_ptr->r_flags2 &= r_ptr->flags2;
	r_ptr->r_flags3 &= r_ptr->flags3;
	r_ptr->r_flags4 &= r_ptr->flags4;
	r_ptr->r_flags5 &= r_ptr->flags5;
	r_ptr->r_flags6 &= r_ptr->flags6;
}




/*
 * Read a store
 */
static errr rd_store(int town_number, int store_number)
{
	store_type *st_ptr = &town[town_number].store[store_number];

	int j;

	byte own, num;

	/* Read the basic info */
	rd_s32b(&st_ptr->store_open);
	rd_s16b(&st_ptr->insult_cur);
	rd_byte(&own);
	rd_byte(&num);
	rd_s16b(&st_ptr->good_buy);
	rd_s16b(&st_ptr->bad_buy);

	if (!older_than(2, 1, 3))
	{
		/* Read last visit */
		rd_s32b(&st_ptr->last_visit);
	}
	else
	{
		/* Reset last visit to the current turn */
		st_ptr->last_visit = turn;
	}

	/* Extract the owner (see above) */
	st_ptr->owner = own;

	/* Read the items */
	for (j = 0; j < num; j++)
	{
		object_type forge;
		object_type *q_ptr;

		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Read the item */
		rd_item(q_ptr);

		/* Acquire valid items */
		if (st_ptr->stock_num < STORE_INVEN_MAX)
		{
			int k = st_ptr->stock_num++;

			/* Acquire the item */
			object_copy(&st_ptr->stock[k], q_ptr);
		}
	}

	/* Success */
	return (0);
}



/*
 * Read RNG state (added in 2.8.0)
 */
static void rd_randomizer(void)
{
	int i;

	u16b tmp16u;

	/* Tmp */
	rd_u16b(&tmp16u);

	/* Place */
	rd_u16b(&Rand_place);

	/* State */
	for (i = 0; i < RAND_DEG; i++)
	{
		rd_u32b(&Rand_state[i]);
	}

	/* Accept */
	Rand_quick = FALSE;
}



/*
 * Read options (ignore most pre-2.8.0 options)
 *
 * Note that the normal options are now stored as a set of 256 bit flags,
 * plus a set of 256 bit masks to indicate which bit flags were defined
 * at the time the savefile was created.  This will allow new options
 * to be added, and old options to be removed, at any time, without
 * hurting old savefiles.
 *
 * The window options are stored in the same way, but note that each
 * window gets 32 options, and their order is fixed by certain defines.
 */
static void rd_options(void)
{
	int i, n;

	byte b;

	u16b c;

	u32b flag[8];
	u32b mask[8];


	/*** Oops ***/

	/* Ignore old options */
	strip_bytes(16);


	/*** Special info */

	/* Read "delay_factor" */
	rd_byte(&b);
	delay_factor = b;

	/* Read "hitpoint_warn" */
	rd_byte(&b);
	hitpoint_warn = b;


	/*** Cheating options ***/

	rd_u16b(&c);

	if (c & 0x0002) wizard = TRUE;

	cheat_peek = (c & 0x0100) ? TRUE : FALSE;
	cheat_hear = (c & 0x0200) ? TRUE : FALSE;
	cheat_room = (c & 0x0400) ? TRUE : FALSE;
	cheat_xtra = (c & 0x0800) ? TRUE : FALSE;
	cheat_know = (c & 0x1000) ? TRUE : FALSE;
	cheat_live = (c & 0x2000) ? TRUE : FALSE;

	if (older_than(2, 1, 0))
	{
		autosave_t = autosave_l = 0;
		autosave_freq = 0;
	}
	else
	{
		rd_bool(&autosave_l);
		rd_bool(&autosave_t);
		rd_s16b(&autosave_freq);
	}


	/*** Normal Options ***/

	/* Read the option flags */
	for (n = 0; n < 8; n++) rd_u32b(&flag[n]);

	/* Read the option masks */
	for (n = 0; n < 8; n++) rd_u32b(&mask[n]);

	/* Analyze the options */
	for (n = 0; n < 8; n++)
	{
		/* Analyze the options */
		for (i = 0; i < 32; i++)
		{
			/* Process valid flags */
			if (mask[n] & (1L << i))
			{
				/* Process valid flags */
				if (option_mask[n] & (1L << i))
				{
					/* Set */
					if (flag[n] & (1L << i))
					{
						/* Set */
						option_flag[n] |= (1L << i);
					}

					/* Clear */
					else
					{
						/* Clear */
						option_flag[n] &= ~(1L << i);
					}
				}
			}
		}
	}


	/*** Window Options ***/

	/* Read the window flags */
	for (n = 0; n < 8; n++) rd_u32b(&flag[n]);

	/* Read the window masks */
	for (n = 0; n < 8; n++) rd_u32b(&mask[n]);

	/* Analyze the options */
	for (n = 0; n < 8; n++)
	{
		/* Analyze the options */
		for (i = 0; i < 32; i++)
		{
			/* Process valid flags */
			if (mask[n] & (1L << i))
			{
				/* Process valid flags */
				if (window_mask[n] & (1L << i))
				{
					/* Set */
					if (flag[n] & (1L << i))
					{
						/* Set */
						window_flag[n] |= (1L << i);
					}

					/* Clear */
					else
					{
						/* Clear */
						window_flag[n] &= ~(1L << i);
					}
				}
			}
		}
	}
}





/*
 * Hack -- strip the "ghost" info
 *
 * XXX XXX XXX This is such a nasty hack it hurts.
 */
static void rd_ghost(void)
{
	char buf[64];

	/* Strip name */
	rd_string(buf, 64);

	/* Strip old data */
	strip_bytes(60);
}




/*
 * Read the "extra" information
 */
static void rd_extra(void)
{
	int i;

	byte tmp8u;
	s16b tmp16s;

	rd_string(player_name, 32);

	rd_string(died_from, 80);

	for (i = 0; i < 4; i++)
	{
		rd_string(history[i], 60);
	}

	/* Class/Race/Gender/Spells */
	rd_byte(&p_ptr->prace);
	rd_byte(&p_ptr->pclass);
	rd_byte(&p_ptr->psex);
	rd_byte(&p_ptr->realm1);
	rd_byte(&p_ptr->realm2);
	rd_byte(&tmp8u); /* oops */

	/* Special Race/Class info */
	rd_byte(&p_ptr->hitdie);
	rd_u16b(&p_ptr->expfact);

	/* Age/Height/Weight */
	rd_s16b(&p_ptr->age);
	rd_s16b(&p_ptr->ht);
	rd_s16b(&p_ptr->wt);

	/* Read the stat info */
	for (i = 0; i < 6; i++) rd_s16b(&p_ptr->stat_max[i]);
	for (i = 0; i < 6; i++) rd_s16b(&p_ptr->stat_cur[i]);

	strip_bytes(24); /* oops */

	rd_s32b(&p_ptr->au);

	rd_s32b(&p_ptr->max_exp);
	rd_s32b(&p_ptr->exp);
	rd_u16b(&p_ptr->exp_frac);

	rd_s16b(&p_ptr->lev);

	/* Current version */
	if (!older_than(2, 1, 3))
	{
		rd_s16b(&p_ptr->town_num);

		/* Read arena and rewards information */
		rd_s16b(&tmp16s);
		rd_s16b(&tmp16s);
		rd_s16b(&p_ptr->inside_quest);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);

		rd_s16b(&p_ptr->oldpx);
		rd_s16b(&p_ptr->oldpy);

		rd_s16b(&tmp16s);

		if (tmp16s > MAX_BACT)
		{
			note(format("Too many (%d) building rewards!", tmp16s));
		}

		for (i = 0; i < tmp16s; i++) rd_s16b(&p_ptr->rewards[i]);
	}
	/* 2.1.2 beta version */
	else if (z_major == 2 && z_minor == 1 && z_patch == 2)
	{
		/* Town index */
		rd_s16b(&tmp16s);
		p_ptr->town_num = 1;
		p_ptr->oldpx = 0;
		p_ptr->oldpy = 0;

		/* read arena information and rewards -KMW- */
		rd_s16b(&tmp16s);
		rd_s16b(&tmp16s);
		rd_s16b(&p_ptr->inside_quest);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);

		/* Throw away old quest informations */
		for (i = 0; i < 100; i++) rd_s16b(&tmp16s);
		for (i = 0; i < 10; i++) rd_s16b(&tmp16s);
		for (i = 0; i < 10; i++) rd_s16b(&tmp16s);
		for (i = 0; i < 5; i++) rd_s16b(&tmp16s);
		for (i = 0; i < 5; i++) rd_s16b(&tmp16s);
	}
	else /* 2.1.0 or older */
	{
		p_ptr->town_num = 1;
		p_ptr->oldpx = 0;
		p_ptr->oldpy = 0;

		/* Initialize arena information -KMW- */
		p_ptr->inside_quest = 0;

		for (i = 0; i < MAX_BACT; ++i) p_ptr->rewards[i] = 0;
	}

	rd_s16b(&p_ptr->mhp);
	rd_s16b(&p_ptr->chp);
	rd_u16b(&p_ptr->chp_frac);

	rd_s16b(&p_ptr->msp);
	rd_s16b(&p_ptr->csp);
	rd_u16b(&p_ptr->csp_frac);

	rd_s16b(&p_ptr->max_plv);
	rd_s16b(&p_ptr->max_dlv);

	/* Repair maximum player level XXX XXX XXX */
	if (p_ptr->max_plv < p_ptr->lev) p_ptr->max_plv = p_ptr->lev;

	/* Repair maximum dungeon level */
	if (p_ptr->max_dlv < 0) p_ptr->max_dlv = 1;

	/* More info */
	strip_bytes(8);
	rd_s16b(&p_ptr->sc);
	strip_bytes(2);

	/* Read the flags */
	strip_bytes(2); /* Old "rest" */
	rd_s16b(&p_ptr->blind);
	rd_s16b(&p_ptr->paralyzed);
	rd_s16b(&p_ptr->confused);
	rd_s16b(&p_ptr->food);
	strip_bytes(4); /* Old "food_digested" / "protection" */
	rd_s16b(&p_ptr->energy);
	rd_s16b(&p_ptr->fast);
	rd_s16b(&p_ptr->slow);
	rd_s16b(&p_ptr->afraid);
	rd_s16b(&p_ptr->cut);
	rd_s16b(&p_ptr->stun);
	rd_s16b(&p_ptr->poisoned);
	rd_s16b(&p_ptr->image);
	rd_s16b(&p_ptr->protevil);
	rd_s16b(&p_ptr->invuln);
	rd_s16b(&p_ptr->hero);
	rd_s16b(&p_ptr->shero);
	rd_s16b(&p_ptr->shield);
	rd_s16b(&p_ptr->blessed);
	rd_s16b(&p_ptr->tim_invis);
	rd_s16b(&p_ptr->word_recall);
	rd_s16b(&p_ptr->see_infra);
	rd_s16b(&p_ptr->tim_infra);
	rd_s16b(&p_ptr->oppose_fire);
	rd_s16b(&p_ptr->oppose_cold);
	rd_s16b(&p_ptr->oppose_acid);
	rd_s16b(&p_ptr->oppose_elec);
	rd_s16b(&p_ptr->oppose_pois);

	/* Old savefiles do not have the following fields... */
	if ((z_major == 2) && (z_minor == 0) && (z_patch == 6))
	{
		p_ptr->tim_esp = 0;
		p_ptr->wraith_form = 0;
		p_ptr->resist_magic = 0;
		p_ptr->tim_xtra1 = 0;
		p_ptr->tim_xtra2 = 0;
		p_ptr->tim_xtra3 = 0;
		p_ptr->tim_xtra4 = 0;
		p_ptr->tim_xtra5 = 0;
		p_ptr->tim_xtra6 = 0;
		p_ptr->tim_xtra7 = 0;
		p_ptr->tim_xtra8 = 0;
		p_ptr->chaos_patron = get_chaos_patron();
		p_ptr->muta1 = 0;
		p_ptr->muta2 = 0;
		p_ptr->muta3 = 0;
	}
	else
	{
		rd_s16b(&p_ptr->tim_esp);
		rd_s16b(&p_ptr->wraith_form);
		rd_s16b(&p_ptr->resist_magic);
		rd_s16b(&p_ptr->tim_xtra1);
		rd_s16b(&p_ptr->tim_xtra2);
		rd_s16b(&p_ptr->tim_xtra3);
		rd_s16b(&p_ptr->tim_xtra4);
		rd_s16b(&p_ptr->tim_xtra5);
		rd_s16b(&p_ptr->tim_xtra6);
		rd_s16b(&p_ptr->tim_xtra7);
		rd_s16b(&p_ptr->tim_xtra8);
		rd_s16b(&p_ptr->chaos_patron);
		rd_u32b(&p_ptr->muta1);
		rd_u32b(&p_ptr->muta2);
		rd_u32b(&p_ptr->muta3);
	}

	/* Calc the regeneration modifier for mutations */
	mutant_regenerate_mod = calc_mutant_regenerate_mod();

	rd_byte(&p_ptr->confusing);
	rd_byte(&tmp8u); /* oops */
	rd_byte(&tmp8u); /* oops */
	rd_byte(&tmp8u); /* oops */
	rd_byte(&p_ptr->searching);
	rd_byte(&p_ptr->maximize);
	rd_byte(&p_ptr->preserve);
	rd_byte(&tmp8u);

	/* Future use */
	for (i = 0; i < 48; i++) rd_byte(&tmp8u);

	/* Skip the flags */
	strip_bytes(12);


	/* Hack -- the two "special seeds" */
	rd_u32b(&seed_flavor);
	rd_u32b(&seed_town);


	/* Special stuff */
	rd_u16b(&panic_save);
	rd_u16b(&total_winner);
	rd_u16b(&noscore);


	/* Read "death" */
	rd_byte(&tmp8u);
	death = tmp8u;

	/* Read "feeling" */
	rd_byte(&tmp8u);
	feeling = tmp8u;

	/* Turn of last "feeling" */
	rd_s32b(&old_turn);

	/* Current turn */
	rd_s32b(&turn);
}




/*
 * Read the player inventory
 *
 * Note that the inventory changed in Angband 2.7.4.  Two extra
 * pack slots were added and the equipment was rearranged.  Note
 * that these two features combine when parsing old save-files, in
 * which items from the old "aux" slot are "carried", perhaps into
 * one of the two new "inventory" slots.
 *
 * Note that the inventory is "re-sorted" later by "dungeon()".
 */
static errr rd_inventory(void)
{
	int slot = 0;

	object_type forge;
	object_type *q_ptr;

	/* No weight */
	p_ptr->total_weight = 0;

	/* No items */
	inven_cnt = 0;
	equip_cnt = 0;

	/* Read until done */
	while (1)
	{
		u16b n;

		/* Get the next item index */
		rd_u16b(&n);

		/* Nope, we reached the end */
		if (n == 0xFFFF) break;

		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Read the item */
		rd_item(q_ptr);

		/* Hack -- verify item */
		if (!q_ptr->k_idx) return (53);

		/* Wield equipment */
		if (n >= INVEN_WIELD)
		{
			/* Copy object */
			object_copy(&inventory[n], q_ptr);

			/* Add the weight */
			p_ptr->total_weight += (q_ptr->number * q_ptr->weight);

			/* One more item */
			equip_cnt++;
		}

		/* Warning -- backpack is full */
		else if (inven_cnt == INVEN_PACK)
		{
			/* Oops */
			note("Too many items in the inventory!");

			/* Fail */
			return (54);
		}

		/* Carry inventory */
		else
		{
			/* Get a slot */
			n = slot++;

			/* Copy object */
			object_copy(&inventory[n], q_ptr);

			/* Add the weight */
			p_ptr->total_weight += (q_ptr->number * q_ptr->weight);

			/* One more item */
			inven_cnt++;
		}
	}

	/* Success */
	return (0);
}



/*
 * Read the saved messages
 */
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

		/* Save the message */
		message_add(buf);
	}
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
	s16b tmp16s;
	u16b limit;
	cave_type *c_ptr;


	/*** Basic info ***/

	/* Header info */
	rd_s16b(&dun_level);

	/* Set the base level for old versions */
	base_level = dun_level;

	/* Read the base level */
	if (!older_than(2, 2, 2))
	{
		rd_s16b(&base_level);
	}

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


	if (!older_than(2, 1, 3))
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
	}

	/*** Objects ***/

	/* Read the item count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit >= max_o_idx)
	{
		note(format("Too many (%d) object entries!", limit));
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
			note(format("Object allocation error (%d <> %d)", i, o_idx));
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
		note(format("Too many (%d) monster entries!", limit));
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
			note(format("Monster allocation error (%d <> %d)", i, m_idx));
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

	/* Regenerate the dungeon for old savefiles and corrupted panic-saves */
	if (older_than(2, 1, 3) || (py == 0) || (px == 0))
	{
		character_dungeon = FALSE;
	}
	else
	{
		/* The dungeon is ready */
		character_dungeon = TRUE;
	}

	/* Success */
	return (0);
}



/*
 * Actually read the savefile
 */
static errr rd_savefile_new_aux(void)
{
	int i, j;
	int town_count;

	s32b wild_x_size;
	s32b wild_y_size;

	byte tmp8u;
	u16b tmp16u;
	u32b tmp32u;

#ifdef VERIFY_CHECKSUMS
	u32b n_x_check, n_v_check;
	u32b o_x_check, o_v_check;
#endif


	/* Mention the savefile version */
	note(format("Loading a %d.%d.%d savefile...",
		z_major, z_minor, z_patch));


	/* Strip the version bytes */
	strip_bytes(4);

	/* Hack -- decrypt */
	xor_byte = sf_extra;


	/* Clear the checksums */
	v_check = 0L;
	x_check = 0L;


	/* Operating system info */
	rd_u32b(&sf_xtra);

	/* Time of savefile creation */
	rd_u32b(&sf_when);

	/* Number of resurrections */
	rd_u16b(&sf_lives);

	/* Number of times played */
	rd_u16b(&sf_saves);


	/* Later use (always zero) */
	rd_u32b(&tmp32u);

	/* Later use (always zero) */
	rd_u32b(&tmp32u);


	/* Read RNG state */
	rd_randomizer();
	if (arg_fiddle) note("Loaded Randomizer Info");


	/* Then the options */
	rd_options();
	if (arg_fiddle) note("Loaded Option Flags");

	/* Switch streams on for old savefiles */
	if (older_than(2, 2, 6))
		terrain_streams = TRUE;

	/* Then the "messages" */
	rd_messages();
	if (arg_fiddle) note("Loaded Messages");


	/* Monster Memory */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > max_r_idx)
	{
		note(format("Too many (%u) monster races!", tmp16u));
		return (21);
	}

	/* Read the available records */
	for (i = 0; i < tmp16u; i++)
	{
		monster_race *r_ptr;

		/* Read the lore */
		rd_lore(i);

		/* Access that monster */
		r_ptr = &r_info[i];
	}

	/* Pre 2.2.0 version (old r_info.txt) */
	if (older_than(2, 2, 0))
	{
		monster_race *r_ptr;

		for (i = 0; i < max_r_idx; i++)
		{
			/* Access that monster */
			r_ptr = &r_info[i];

			/* Hack -- Reset the death counter */
			r_ptr->max_num = 100;
			if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->max_num = 1;
			if (r_ptr->flags3 & RF3_UNIQUE_7) r_ptr->max_num = 7;
		}
	}

	if (arg_fiddle) note("Loaded Monster Memory");


	/* Object Memory */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > max_k_idx)
	{
		note(format("Too many (%u) object kinds!", tmp16u));
		return (22);
	}

	/* Read the object memory */
	for (i = 0; i < tmp16u; i++)
	{
		byte tmp8u;
		object_kind *k_ptr = &k_info[i];

		rd_byte(&tmp8u);

		k_ptr->aware = (tmp8u & 0x01) ? TRUE: FALSE;
		k_ptr->tried = (tmp8u & 0x02) ? TRUE: FALSE;
	}
	if (arg_fiddle) note("Loaded Object Memory");

#if 0
	/*
	 * Initialize quest and rewards information
	 */
	p_ptr->inside_quest = 0;

	/* Start in town 1 */
	p_ptr->town_num = 1;

	p_ptr->wilderness_x = 4;
	p_ptr->wilderness_y = 4;

#endif

	/* Init the wilderness seeds */
	for (i = 0; i < max_wild_x; i++)
	{
		for (j = 0; j < max_wild_y; j++)
		{
			wilderness[j][i].seed = rand_int(0x10000000);
		}
	}

	/* 2.1.3 or newer version */
	if (!older_than(2, 1, 3))
	{
		u16b max_towns_load;
		u16b max_quests_load;

		/* Number of towns */
		rd_u16b(&max_towns_load);

		/* 2.2.2 or older version */
		if (older_than(2, 2, 3))
		{
			/* Ignore higher numbers of towns */
			if (max_towns_load > max_towns)
				max_towns_load = max_towns;
		}

		/* Incompatible save files */
		if (max_towns_load > max_towns)
		{
			note(format("Too many (%u) towns!", max_towns_load));
			return (23);
		}

		/* Number of quests */
		rd_u16b(&max_quests_load);

		/* 2.2.3 or newer version */
		if (!older_than(2, 2, 3))
		{
			/* Incompatible save files */
			if (max_quests_load > max_quests)
			{
				note(format("Too many (%u) quests!", max_quests_load));
				return (23);
			}
		}

		for (i = 0; i < max_quests_load; i++)
		{
			if (i < max_quests)
			{
				rd_s16b(&quest[i].status);

				if (!older_than(2, 2, 0))
				{
					rd_s16b(&quest[i].level);
				}

				/* Load quest status if quest is running */
				if (quest[i].status == QUEST_STATUS_TAKEN)
				{
					rd_s16b(&quest[i].cur_num);
					rd_s16b(&quest[i].max_num);
					rd_s16b(&quest[i].type);

					if (older_than(2, 2, 0))
					{
						strip_bytes(2);
					}

					/* Load quest monster index */
					rd_s16b(&quest[i].r_idx);

					/* Load quest item index */
					if (!older_than(2, 2, 1))
					{
						rd_s16b(&quest[i].k_idx);

						if (quest[i].k_idx)
							a_info[quest[i].k_idx].flags3 |= TR3_QUESTITEM;
					}

					/* Load quest flags */
					if (!older_than(2, 2, 3))
					{
						rd_byte(&quest[i].flags);
					}

					if (older_than(2, 2, 0))
					{
						strip_bytes(40);
					}

					/* Mark uniques */
					if (r_info[quest[i].r_idx].flags1 & RF1_UNIQUE)
							r_info[quest[i].r_idx].flags1 |= RF1_QUESTOR;
				}
			}
			/* Ignore the empty quests from old versions */
			else
			{
				/* Ignore quest status */
				strip_bytes(2);

				/* Ignore quest level */
				if (!older_than(2, 2, 0))
				{
					strip_bytes(2);
				}

				/*
				 * We don't have to care about the other info,
				 * since status should be 0 for these quests anyway
				 */
			}
		}

		/* Only in 2.2.1 and 2.2.2 */
		if (!older_than(2, 2, 1) && older_than(2, 2, 3))
		{
			/* "Hard quests" flag */
			rd_bool(&ironman_hard_quests);

			/****** HACK ******/
			if (ironman_hard_quests)
			{
				/* Set the option by hand */
				option_flag[6] |= (1L << 6);
			}

			/* Inverted "Wilderness" flag */
			/* Not used anymore -- Prfnoff */
			rd_byte(&tmp8u);
		}

		/* Position in the wilderness */
		rd_s32b(&p_ptr->wilderness_x);
		rd_s32b(&p_ptr->wilderness_y);

		/* Size of the wilderness */
		rd_s32b(&wild_x_size);
		rd_s32b(&wild_y_size);

		/* Incompatible save files */
		if ((wild_x_size > max_wild_x) || (wild_y_size > max_wild_y))
		{
			note(format("Wilderness is too big (%u/%u)!", wild_x_size, wild_y_size));
			return (23);
		}

		/* Load the wilderness seeds */
		for (i = 0; i < wild_x_size; i++)
		{
			for (j = 0; j < wild_y_size; j++)
			{
				rd_u32b(&wilderness[j][i].seed);
			}
		}
	}
	/* rr9: Load old savegame without the quest infos */
	else if (older_than(2, 1, 1))
	{
		/* Load the number of quests */
		rd_u16b(&tmp16u);

		/* Ignore all infos */
		for (i = 0; i < tmp16u; i++)
		{
			strip_bytes(4);
		}
	}
	/* rr9: Load 2.1.1 savegame quest infos */
	else if (older_than(2, 1, 2))
	{
		/* Load the number of quests */
		rd_u16b(&tmp16u);

		j = tmp16u;

		/* Ignore the quests */
		for (i = 0; i < j; i++)
		{
			strip_bytes(5);
		}
	}
	/* 2.1.2 beta version */
	else if (older_than(2, 1, 3))
	{
		/* Load the number of quests */
		rd_u16b(&tmp16u);

		/* Incompatible save files */
		if (tmp16u > 20)
		{
			note(format("Too many (%u) quests!", tmp16u));
			return (23);
		}

		/* Load the quest information */
		for (i = 0; i < tmp16u; i++)
		{
			/* Throw it away */
			strip_bytes(14);
		}
	}

	/*
	 * Select the number of random quests
	 * when importing old savefiles.
	 */
	if (older_than(2, 2, 0))
	{
		char inp[80];
		int i, v;

		/* Wipe the quests */
		for (i = 0; i < max_quests; i++)
		{
			quest[i].status = QUEST_STATUS_UNTAKEN;

			quest[i].cur_num = 0;
			quest[i].max_num = 0;
			quest[i].type = 0;
			quest[i].level = 0;
			quest[i].r_idx = 0;
		}

		/* Clean up */
		clear_from(10);

		/*** User enters number of quests ***/
		/* Heino Vander Sanden and Jimmy De Laet */

		/* Extra info */
		Term_putstr(5, 15, -1, TERM_WHITE,
			"You can input yourself the number of quest you'd like to");
		Term_putstr(5, 16, -1, TERM_WHITE,
			"perform next to two obligatory ones ( Oberon and the Serpent of Chaos )");
		Term_putstr(5, 17, -1, TERM_WHITE,
			"In case you do not want any additional quest, just enter 0");

		/* Ask the number of additional quests */
		while (TRUE)
		{
			put_str(format("Number of additional quest? (<%u) ", MAX_RANDOM_QUEST - MIN_RANDOM_QUEST + 2), 20, 2);

			/* Get a the number of additional quest */
			while (TRUE)
			{
				/* Move the cursor */
				put_str("", 20, 37);

				/* Default */
				strcpy(inp, "20");

				/* Get a response (or escape) */
				if (!askfor_aux(inp, 2)) inp[0] = '\0';
				v = atoi(inp);

				/* Break on valid input */
				if ((v <= MAX_RANDOM_QUEST - MIN_RANDOM_QUEST + 1) && (v >= 0)) break;
			}
			break;
		}

		/* Clear */
		clear_from(15);

		/* Init the random quests */
		init_flags = INIT_ASSIGN;
		p_ptr->inside_quest = MIN_RANDOM_QUEST;
		process_dungeon_file("q_info.txt", 0, 0, 0, 0);
		p_ptr->inside_quest = 0;

		/* Prepare allocation table */
		get_mon_num_prep(monster_quest, NULL);

		/* Generate quests */
		for (i = MIN_RANDOM_QUEST + v - 1; i >= MIN_RANDOM_QUEST; i--)
		{
			quest_type *q_ptr = &quest[i];
			monster_race *r_ptr = NULL;

			q_ptr->status = QUEST_STATUS_TAKEN;

			for (j = 0; j < MAX_TRIES; j++)
			{
				/*
				 * Random monster 5 - 10 levels out of depth
				 * (depending on level)
				 */
				int r_idx = get_mon_num(q_ptr->level + 4 + randint(q_ptr->level / 10));
				r_ptr = &r_info[r_idx];

				/* Save the index if the monster is deeper than out current monster */
				if (!q_ptr->r_idx || (r_info[r_idx].level > r_info[q_ptr->r_idx].level))
				{
					q_ptr->r_idx = r_idx;
				}

				/*
				 * Accept monsters that are 2 - 6 levels
				 * out of depth depending on the quest level
				 */
				if (r_ptr->level > (q_ptr->level + (q_ptr->level / 20) + 1)) break;
			}

			/* Get the number of monsters */
			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				/* Mark uniques */
				r_ptr->flags1 |= RF1_QUESTOR;

				q_ptr->max_num = 1;
			}
			else
			{
				q_ptr->max_num = 5 + (s16b)rand_int(q_ptr->level/3 + 5);
			}
		}

		/* Init the two main quests (Oberon + Serpent) */
		init_flags = INIT_ASSIGN;
		p_ptr->inside_quest = QUEST_OBERON;
		process_dungeon_file("q_info.txt", 0, 0, 0, 0);
		quest[QUEST_OBERON].status = QUEST_STATUS_TAKEN;

		p_ptr->inside_quest = QUEST_SERPENT;
		process_dungeon_file("q_info.txt", 0, 0, 0, 0);
		quest[QUEST_SERPENT].status = QUEST_STATUS_TAKEN;
		p_ptr->inside_quest = 0;
	}

	/*
	 * Select 'hard random quests mode'
	 * when importing old savefiles.
	 */
	if (older_than(2, 2, 1))
	{
		char c;

		/* Clear */
		clear_from(15);

		/*** Hard quests mode ***/

		/* Extra info */
		Term_putstr(5, 14, -1, TERM_WHITE,
			"Using 'hard quests' mode makes the random quests harder, because");
		Term_putstr(5, 15, -1, TERM_WHITE,
			"you have to kill all monsters at the same visit to the quest level.");
		Term_putstr(5, 16, -1, TERM_WHITE,
			"If you leave the level while some quest monsters are still alive,");
		Term_putstr(5, 17, -1, TERM_WHITE,
			"then all killed quest monsters are revived on your next visit");
		Term_putstr(5, 18, -1, TERM_WHITE,
			"to this level.");

		/* Ask about "hard quests" mode */
		while (1)
		{
			put_str("Use 'Hard quests'? (y/n/*) ", 20, 2);
			c = inkey();
			if (c == 'Q') quit(NULL);
			if (c == 'S') return (FALSE);
			if (c == '*')
			{
				c = 'y';
				if (randint(2) == 1)
					c = 'n';
				break;
			}
			if (c == ESCAPE) break;
			if ((c == 'y') || (c == 'n')) break;
			if (c == '?') do_cmd_help();
			else bell();
		}

		/* Set "hard quests" mode */
		ironman_hard_quests = (c == 'y');

		/* Clear */
		clear_from(15);
	}

	if (arg_fiddle) note("Loaded Quests");

	/* A version without the wilderness */
	if (older_than(2, 1, 2))
	{
		char c;

		/* Clear */
		clear_from(14);

		/*** Wilderness mode ***/

		/* Extra info */
		Term_putstr(5, 14, -1, TERM_WHITE,
			"'Wilderness' mode enables the extended wilderness which");
		Term_putstr(5, 15, -1, TERM_WHITE,
			"gives you a wilderness and several new towns to explore.");
		Term_putstr(5, 16, -1, TERM_WHITE,
			"Switching off 'wilderness' mode is recommended for slower computers,");
		Term_putstr(5, 17, -1, TERM_WHITE,
			"because the wilderness slows down the system a bit.");

		/* Ask about "wilderness" mode */
		while (1)
		{
			put_str("Use 'wilderness'? (y/n/*) ", 20, 2);
			c = inkey();
			if (c == 'Q') quit(NULL);
			if (c == 'S') return (FALSE);
			if (c == '*')
			{
				c = 'y';
				if (randint(2) == 1)
					c = 'n';
				break;
			}
			if (c == ESCAPE) break;
			if ((c == 'y') || (c == 'n')) break;
			if (c == '?') do_cmd_help();
			else bell();
		}

		/* Set "wilderness" mode */
		vanilla_town = (c == 'n');

		/* Clear */
		clear_from(14);
	}


	/* Load the Artifacts */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > max_a_idx)
	{
		note(format("Too many (%u) artifacts!", tmp16u));
		return (24);
	}

	/* Read the artifact flags */
	for (i = 0; i < tmp16u; i++)
	{
		rd_byte(&tmp8u);
		a_info[i].cur_num = tmp8u;
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
	}
	if (arg_fiddle) note("Loaded Artifacts");


	/* Read the extra stuff */
	rd_extra();
	if (arg_fiddle) note("Loaded extra information");

	/* Read the player_hp array */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > PY_MAX_LEVEL)
	{
		note(format("Too many (%u) hitpoint entries!", tmp16u));
		return (25);
	}

	/* Read the player_hp array */
	for (i = 0; i < tmp16u; i++)
	{
		rd_s16b(&player_hp[i]);
	}


	/* Important -- Initialize the sex */
	sp_ptr = &sex_info[p_ptr->psex];

	/* Important -- Initialize the race/class */
	rp_ptr = &race_info[p_ptr->prace];
	cp_ptr = &class_info[p_ptr->pclass];

	/* Important -- Initialize the magic */
	mp_ptr = &magic_info[p_ptr->pclass];


	/* Read spell info */
	rd_u32b(&spell_learned1);
	rd_u32b(&spell_learned2);
	rd_u32b(&spell_worked1);
	rd_u32b(&spell_worked2);
	rd_u32b(&spell_forgotten1);
	rd_u32b(&spell_forgotten2);

	for (i = 0; i < 64; i++)
	{
		rd_byte(&spell_order[i]);
	}


	/* Read the inventory */
	if (rd_inventory())
	{
		note("Unable to read inventory");
		return (21);
	}

	/* Read number of towns */
	if (!older_than(2, 1, 3))
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
		if (older_than(2, 2, 3) && (i >= 6))
		{
			for (j = 0; j < tmp16u; j++)
			{
				/* Read the info into the empty town 5 (R'Lyeh) */
				if (rd_store(5, j)) return (22);
			}
		}
		else
		{
			for (j = 0; j < tmp16u; j++)
			{
				if (rd_store(i, j)) return (22);
			}
		}
	}

	/* Read the pet command settings */
	if (!older_than(2, 3, 3))
	{
		rd_s16b(&p_ptr->pet_follow_distance);
		rd_bool(&p_ptr->pet_open_doors);
		rd_bool(&p_ptr->pet_pickup_items);
	}
	else if (!older_than(2, 2, 3))
	{
		rd_byte(&tmp8u);
		p_ptr->pet_follow_distance = tmp8u;
		rd_byte(&tmp8u);
		p_ptr->pet_open_doors = tmp8u;
		rd_byte(&tmp8u);
		p_ptr->pet_pickup_items = tmp8u;
	}
	else
	{
		/* Default pet command settings */
		p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
		p_ptr->pet_open_doors = FALSE;
		p_ptr->pet_pickup_items = FALSE;
	}

	/* I'm not dead yet... */
	if (!death)
	{
		/* Dead players have no dungeon */
		note("Restoring Dungeon...");
		if (rd_dungeon())
		{
			note("Error reading dungeon data");
			return (34);
		}

		/* Read the ghost info */
		rd_ghost();

		if (!older_than(2, 2, 4))
		{
			s32b tmp32s;

			rd_s32b(&tmp32s);
#ifdef USE_SCRIPT
			if (tmp32s)
			{
				char *callbacks = (char*) malloc(tmp32s + 1);
				rd_string(callbacks, tmp32s + 1);
				callbacks_load_callback(callbacks);
				free(callbacks);
			}
#else /* USE_SCRIPT */
			strip_bytes(tmp32s);
#endif /* USE_SCRIPT */
		}
	}


#ifdef VERIFY_CHECKSUMS

	/* Save the checksum */
	n_v_check = v_check;

	/* Read the old checksum */
	rd_u32b(&o_v_check);

	/* Verify */
	if (o_v_check != n_v_check)
	{
		note("Invalid checksum");
		return (11);
	}


	/* Save the encoded checksum */
	n_x_check = x_check;

	/* Read the checksum */
	rd_u32b(&o_x_check);


	/* Verify */
	if (o_x_check != n_x_check)
	{
		note("Invalid encoded checksum");
		return (11);
	}

#endif

	/* Success */
	return (0);
}


/*
 * Actually read the savefile
 */
errr rd_savefile_new(void)
{
	errr err;

	/* The savefile is a binary file */
	fff = my_fopen(savefile, "rb");

	/* Paranoia */
	if (!fff) return (-1);

	/* Call the sub-function */
	err = rd_savefile_new_aux();

	/* Check for errors */
	if (ferror(fff)) err = -1;

	/* Close the file */
	my_fclose(fff);

	/* Result */
	return (err);
}


