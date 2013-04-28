/* File: mon_memory.c */

/* Purpose: monster recall to an array */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)

#include <tcl.h>
#include "angband.h"
#include "tnb.h"
#include "interp.h"

/*
Acidic cytoplasm
A squishy black smoking puddle of acid.
Type: intelligent, evil, undead
Kills: 8
Points: 150
Level: 20
Armor: 25
Health: 19d25
Attacks:
    hit to attack with damage 5d5
    hit to terrify with damage 2d3
    breathe lightning
    breathe acid
Spells:
    invoke mana storms
    produce fire balls
Abilities:
	destroy weaker monsters
	destroy objects
Immunity:
	stun, confuse, fear, sleep
Resists:
	acid, confusion, nether
Weakness:
	hurt by bright light
Breath/Spell Frequency: 1 time in 4
Speed: 1.0
Movement: 75% erratic
Rarity: common
Companions: appears in groups.
Drop:
	up to 2 objects or treasures
*/

#if defined(OANGBANDTK)

/*
 * Descriptions of monster rarity. -LM-
 */
static char *wd_rarity(byte rarity, bool unique)
{
	static char rarity_desc[40];

	if (unique)
	{
		if (rarity == 1) strcpy(rarity_desc, "very often encountered");
		else if (rarity == 2) strcpy(rarity_desc, "often encountered");
		else if (rarity == 3) strcpy(rarity_desc, "fairly often encountered");
		else if (rarity == 4) strcpy(rarity_desc, "infrequently encountered");
		else if ((rarity == 5) || (rarity == 6)) strcpy(rarity_desc, "seldom encountered");
		else if (rarity < 10) strcpy(rarity_desc, "very seldom encountered");
		else strcpy(rarity_desc, "almost never encountered");
	}
	else
	{
#if 1 /* TNB */
		if (rarity == 1) strcpy(rarity_desc, "very common");
#else /* TNB */
		if (rarity == 1) strcpy(rarity_desc, "ubiquitous");
#endif /* TNB */
		else if (rarity == 2) strcpy(rarity_desc, "common");
		else if (rarity == 3) strcpy(rarity_desc, "fairly common");
		else if (rarity == 4) strcpy(rarity_desc, "not very common");
		else if ((rarity == 5) || (rarity == 6)) strcpy(rarity_desc, "fairly rare");
		else if (rarity < 10) strcpy(rarity_desc, "rare");
		else strcpy(rarity_desc, "extremely rare");
	}

	return rarity_desc;
}

#endif /* OANGBANDTK */

/*
 * Pluralizer.  Args(count, singular, plural)
 */
#define plural(c,s,p) \
    (((c) == 1) ? (s) : (p))

static Tcl_Obj *g_ObjPtr;

static void roff_string(cptr field, cptr s)
{
	Tcl_Obj *part2Ptr = Tcl_NewStringObj(field, -1);
	Tcl_Obj *valuePtr = Tcl_NewStringObj(s, -1);
	Tcl_IncrRefCount(part2Ptr);
	Tcl_IncrRefCount(valuePtr);
	Tcl_ObjSetVar2(g_interp, g_ObjPtr, part2Ptr, valuePtr, 0);
	Tcl_DecrRefCount(part2Ptr);
	Tcl_DecrRefCount(valuePtr);
}

static void roff_append(cptr field, cptr s)
{
	Tcl_Obj *part2Ptr = Tcl_NewStringObj(field, -1);
	Tcl_Obj *valuePtr = Tcl_NewStringObj(s, -1);
	Tcl_IncrRefCount(part2Ptr);
	Tcl_IncrRefCount(valuePtr);
	Tcl_ObjSetVar2(g_interp, g_ObjPtr, part2Ptr, valuePtr,
		TCL_APPEND_VALUE);
	Tcl_DecrRefCount(part2Ptr);
	Tcl_DecrRefCount(valuePtr);
}

static void roff_number(cptr field, long n)
{
	Tcl_Obj *part2Ptr = Tcl_NewStringObj(field, -1);
	Tcl_Obj *valuePtr = Tcl_NewLongObj(n);
	Tcl_IncrRefCount(part2Ptr);
	Tcl_IncrRefCount(valuePtr);
	Tcl_ObjSetVar2(g_interp, g_ObjPtr, part2Ptr, valuePtr, 0);
	Tcl_DecrRefCount(part2Ptr);
	Tcl_DecrRefCount(valuePtr);
}

static void roff_bool(cptr field, bool b)
{
	roff_string(field, b ? "TRUE" : "FALSE");
}

static void roff_fmt(cptr field, cptr fmt, ...)
{
	va_list vp;
	int len;
	char buf[256];

	/* Begin the Varargs Stuff */
	va_start(vp, fmt);

	/* Build the string */
	len = vstrnfmt(buf, sizeof(buf), fmt, vp);

	/* End the Varargs Stuff */
	va_end(vp);

	buf[len] = '\0';
	roff_string(field, buf);
}

static void roff_append_fmt(cptr field, cptr fmt, ...)
{
	va_list vp;
	int len;
	char buf[256];

	/* Begin the Varargs Stuff */
	va_start(vp, fmt);

	/* Build the string */
	len = vstrnfmt(buf, sizeof(buf), fmt, vp);

	/* End the Varargs Stuff */
	va_end(vp);

	buf[len] = '\0';
	roff_append(field, buf);
}

static void roff_lappend(cptr field, cptr s)
{
	Tcl_Obj *part2Ptr = Tcl_NewStringObj(field, -1);
	Tcl_Obj *valuePtr = Tcl_NewStringObj(s, -1);
	Tcl_IncrRefCount(part2Ptr);
	Tcl_IncrRefCount(valuePtr);
	Tcl_ObjSetVar2(g_interp, g_ObjPtr, part2Ptr, valuePtr,
		TCL_APPEND_VALUE | TCL_LIST_ELEMENT);
	Tcl_DecrRefCount(part2Ptr);
	Tcl_DecrRefCount(valuePtr);
}

static void roff_lappend_fmt(cptr field, cptr fmt, ...)
{
	va_list vp;
	int len;
	char buf[256];

	/* Begin the Varargs Stuff */
	va_start(vp, fmt);

	/* Build the string */
	len = vstrnfmt(buf, sizeof(buf), fmt, vp);

	/* End the Varargs Stuff */
	va_end(vp);

	buf[len] = '\0';
	roff_lappend(field, buf);
}

/*
 * Determine if the "armor" is known
 * The higher the level, the fewer kills needed.
 */
static bool know_armour(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	s32b level = r_ptr->level;

	s32b kills = LORE_NTH(r_idx).LF(tkills);

#if defined(OANGBANDTK)
	/* Rangers learn quickly. -LM- */
	if (p_ptr->pclass == CLASS_RANGER) kills *= 2;
#endif /* OANGBANDTK */

	/* Normal monsters */
	if (kills > 304 / (4 + level)) return (TRUE);

	/* Skip non-uniques */
	if (!(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/* Unique monsters */
	if (kills > 304 / (38 + (5*level) / 4)) return (TRUE);

	/* Assume false */
	return (FALSE);
}


/*
 * Determine if the "damage" of the given attack is known
 * the higher the level of the monster, the fewer the attacks you need,
 * the more damage an attack does, the more attacks you need
 */
static bool know_damage(int r_idx, int i)
{
	monster_race *r_ptr = &r_info[r_idx];

	s32b level = r_ptr->level;

	s32b a = LORE_NTH(r_idx).LF(blows)[i];

	s32b d1 = r_ptr->blow[i].d_dice;
	s32b d2 = r_ptr->blow[i].d_side;

	s32b d = d1 * d2;

#if defined(OANGBANDTK)
	/* Hack -- Rangers learn quickly. -LM- */
	if (p_ptr->pclass == CLASS_RANGER) level = 10 + 3 * level / 2;
#endif /* */

	/* Normal monsters */
	if ((4 + level) * a > 80 * d) return (TRUE);

	/* Skip non-uniques */
	if (!(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/* Unique monsters */
	if ((4 + level) * (2 * a) > 80 * d) return (TRUE);

	/* Assume false */
	return (FALSE);
}

/*
 *--------------------------------------------------------------
 *
 * MonsterMemoryToArray --
 *
 *	Write monster memory to the given array variable.
 *
 *--------------------------------------------------------------
 */

void MonsterMemoryToArray(int r_idx, Tcl_Obj *objPtr)
{
	monster_race *r_ptr;
	DECLARE_LORE(l_ptr);

	bool old = FALSE;
	bool sin = FALSE;

	int m, n, r;

	cptr p, q;

	int msex = 0;

	bool breath = FALSE;
	bool magic = FALSE;

	u32b flags1;
	u32b flags2;
	u32b flags3;
	u32b flags4;
	u32b flags5;
	u32b flags6;

	int vn = 0;
	cptr vp[64];

	LORE_TYPE save_mem;

	long i, j;

	bool detail = TRUE; /* Ignore "show_details" option */

	/* Global access to the output variable */
	g_ObjPtr = objPtr;

	/* Access the race and lore */
	r_ptr = &r_info[r_idx];
	l_ptr = &LORE_NTH(r_idx);

	/* Cheat -- know everything */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
	if (cheat_know)
#endif /* */
#if defined(OANGBANDTK)
	if ((cheat_know) || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
#endif /* */
	{
		/* XXX XXX XXX */

		/* Hack -- save memory */
		COPY(&save_mem, l_ptr, LORE_TYPE);

		/* Hack -- Maximal kills */
		l_ptr->LF(tkills) = MAX_SHORT;

		/* Hack -- Maximal info */
		l_ptr->LF(wake) = l_ptr->LF(ignore) = MAX_UCHAR;

		/* Observe "maximal" attacks */
		for (m = 0; m < 4; m++)
		{
			/* Examine "actual" blows */
			if (r_ptr->blow[m].effect || r_ptr->blow[m].method)
			{
				/* Hack -- maximal observations */
				l_ptr->LF(blows[m]) = MAX_UCHAR;
			}
		}

		/* Hack -- maximal drops */
		l_ptr->LF(drop_gold) = l_ptr->LF(drop_item) =
		(((r_ptr->flags1 & (RF1_DROP_4D2)) ? 8 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_3D2)) ? 6 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_2D2)) ? 4 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_1D2)) ? 2 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_90))  ? 1 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_60))  ? 1 : 0));

		/* Hack -- but only "valid" drops */
		if (r_ptr->flags1 & (RF1_ONLY_GOLD)) l_ptr->LF(drop_item) = 0;
		if (r_ptr->flags1 & (RF1_ONLY_ITEM)) l_ptr->LF(drop_gold) = 0;

		/* Hack -- observe many spells */
		l_ptr->LF(cast_inate) = MAX_UCHAR;
		l_ptr->LF(cast_spell) = MAX_UCHAR;

		/* Hack -- know all the flags */
		l_ptr->LF(flags1) = r_ptr->flags1;
		l_ptr->LF(flags2) = r_ptr->flags2;
		l_ptr->LF(flags3) = r_ptr->flags3;
		l_ptr->LF(flags4) = r_ptr->flags4;
		l_ptr->LF(flags5) = r_ptr->flags5;
		l_ptr->LF(flags6) = r_ptr->flags6;
	}


	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE)) msex = 1;


	/* Obtain a copy of the "known" flags */
	flags1 = (r_ptr->flags1 & l_ptr->LF(flags1));
	flags2 = (r_ptr->flags2 & l_ptr->LF(flags2));
	flags3 = (r_ptr->flags3 & l_ptr->LF(flags3));
	flags4 = (r_ptr->flags4 & l_ptr->LF(flags4));
	flags5 = (r_ptr->flags5 & l_ptr->LF(flags5));
	flags6 = (r_ptr->flags6 & l_ptr->LF(flags6));


	/* Assume some "obvious" flags */
	if (r_ptr->flags1 & (RF1_UNIQUE)) flags1 |= (RF1_UNIQUE);
	if (r_ptr->flags1 & (RF1_QUESTOR)) flags1 |= (RF1_QUESTOR);
	if (r_ptr->flags1 & (RF1_MALE)) flags1 |= (RF1_MALE);
	if (r_ptr->flags1 & (RF1_FEMALE)) flags1 |= (RF1_FEMALE);

	/* Assume some "creation" flags */
	if (r_ptr->flags1 & (RF1_FRIEND)) flags1 |= (RF1_FRIEND);
	if (r_ptr->flags1 & (RF1_FRIENDS)) flags1 |= (RF1_FRIENDS);
	if (r_ptr->flags1 & (RF1_ESCORT)) flags1 |= (RF1_ESCORT);
	if (r_ptr->flags1 & (RF1_ESCORTS)) flags1 |= (RF1_ESCORTS);

	/* Killing a monster reveals some properties */
	if (l_ptr->LF(tkills))
	{
		/* Know "race" flags */
		if (r_ptr->flags3 & (RF3_ORC)) flags3 |= (RF3_ORC);
		if (r_ptr->flags3 & (RF3_TROLL)) flags3 |= (RF3_TROLL);
		if (r_ptr->flags3 & (RF3_GIANT)) flags3 |= (RF3_GIANT);
		if (r_ptr->flags3 & (RF3_DRAGON)) flags3 |= (RF3_DRAGON);
		if (r_ptr->flags3 & (RF3_DEMON)) flags3 |= (RF3_DEMON);
		if (r_ptr->flags3 & (RF3_UNDEAD)) flags3 |= (RF3_UNDEAD);
		if (r_ptr->flags3 & (RF3_EVIL)) flags3 |= (RF3_EVIL);
		if (r_ptr->flags3 & (RF3_ANIMAL)) flags3 |= (RF3_ANIMAL);

#if defined(ZANGBANDTK)
		if (r_ptr->flags3 & (RF3_GOOD)) flags3 |= (RF3_GOOD);
		if (r_ptr->flags3 & (RF3_AMBERITE)) flags3 |= (RF3_AMBERITE);

		if (r_ptr->flags2 & (RF2_QUANTUM)) flags2 |= (RF2_QUANTUM);
#endif /* */

		/* Know "forced" flags */
		if (r_ptr->flags1 & (RF1_FORCE_DEPTH)) flags1 |= (RF1_FORCE_DEPTH);
		if (r_ptr->flags1 & (RF1_FORCE_MAXHP)) flags1 |= (RF1_FORCE_MAXHP);
	}


	/* Require a flag to show kills */
	if (!(detail))
	{
		/* nothing */
	}

	/* Treat uniques differently */
	else if (flags1 & (RF1_UNIQUE))
	{
		roff_bool("unique", TRUE);
		roff_number("deaths", l_ptr->LF(deaths));
	}

	/* Not unique, but killed us */
	else if (l_ptr->LF(deaths))
	{
		roff_bool("unique", FALSE);
		roff_number("deaths", l_ptr->LF(deaths));
		roff_number("kills", l_ptr->LF(pkills));
		roff_number("tkills", l_ptr->LF(tkills));
	}

	/* Normal monsters */
	else
	{
		roff_bool("unique", FALSE);
		roff_number("deaths", 0);
		roff_number("kills", l_ptr->LF(pkills));
		roff_number("tkills", l_ptr->LF(tkills));
	}


	/* Descriptions */
	if (detail)
	{
		char buf[2048];

#ifdef DELAY_LOAD_R_TEXT

		int fd;

		/* Build the filename */
		path_build(buf, 1024, ANGBAND_DIR_DATA, "r_info.raw");

		/* Open the "raw" file */
		fd = fd_open(buf, O_RDONLY);

		/* Use file */
		if (fd >= 0)
		{
			huge pos;

			/* Starting position */
			pos = r_ptr->text;

			/* Additional offsets */
			pos += r_head->head_size;
			pos += r_head->info_size;
			pos += r_head->name_size;

#if 0

			/* Maximal length */
			len = r_head->text_size - r_ptr->text;

			/* Actual length */
			for (i = r_idx+1; i < MAX_R_IDX; i++)
			{
				/* Actual length */
				if (r_info[i].text > r_ptr->text)
				{
					/* Extract length */
					len = r_info[i].text - r_ptr->text;

					/* Done */
					break;
				}
			}

			/* Maximal length */
			if (len > 2048) len = 2048;

#endif

			/* Seek */
			fd_seek(fd, pos);

			/* Read a chunk of data */
			fd_read(fd, buf, 2048);

			/* Close it */
			fd_close(fd);
		}

#else

		/* Simple method */
		strcpy(buf, r_text + r_ptr->text);

#endif

		/* Dump it */
		roff_string("desc", buf);
	}


	/* Nothing yet */
	old = FALSE;

#if defined(OANGBANDTK)
	/* Player ghosts may have unique descriptions. -LM- */
	if ((r_ptr->flags2 & (RF2_PLAYER_GHOST)) && (ghost_string_type == 2))
		roff_append("desc", ghost_string);
#endif /* */

	/* Describe location */
	if (r_ptr->level == 0)
	{
		roff_number("level", r_ptr->level);
	}
	else if (l_ptr->LF(tkills))
	{
		roff_number("level", r_ptr->level);
#if defined(OANGBANDTK)
		roff_string("rarity",
			wd_rarity(r_ptr->rarity, r_ptr->flags1 & (RF1_UNIQUE)));
#endif /* */
	}

	/* Stationary */
	roff_bool("never_move", (flags1 & RF1_NEVER_MOVE) ? TRUE : FALSE);

	/* Random-ness */
	if ((flags1 & (RF1_RAND_50)) || (flags1 & (RF1_RAND_25)))
	{
		/* Adverb */
		if ((flags1 & (RF1_RAND_50)) && (flags1 & (RF1_RAND_25)))
		{
			roff_number("random_move", 75);
		}
		else if (flags1 & (RF1_RAND_50))
		{
			roff_number("random_move", 50);
		}
		else if (flags1 & (RF1_RAND_25))
		{
			roff_number("random_move", 25);
		}
	}

	/* Speed */
	roff_number("speed", r_ptr->speed);

	/* Describe experience if known */
	if (l_ptr->LF(tkills))
	{
		/* Describe the "quality" */
		if (flags2 & (RF2_SMART)) roff_lappend("type", "intelligent");
#if defined(ZANGBANDTK)
        if (flags2 & (RF2_ELDRITCH_HORROR)) roff_lappend("type", "sanity-blasting");
#endif /* ZANGBANDTK */
		if (flags3 & (RF3_ANIMAL)) roff_lappend("type", "animal");
		if (flags3 & (RF3_EVIL)) roff_lappend("type", "evil");
#if defined(ZANGBANDTK)
        if (flags3 & (RF3_GOOD)) roff_lappend("type", "good");
#endif /* ZANGBANDTK */
		if (flags3 & (RF3_UNDEAD)) roff_lappend("type", "undead");

		/* Describe the "race" */
		if (flags3 & (RF3_DRAGON)) roff_lappend("type", "dragon");
		else if (flags3 & (RF3_DEMON)) roff_lappend("type", "demon");
		else if (flags3 & (RF3_GIANT)) roff_lappend("type", "giant");
		else if (flags3 & (RF3_TROLL)) roff_lappend("type", "troll");
		else if (flags3 & (RF3_ORC)) roff_lappend("type", "orc");
#if defined(ZANGBANDTK)
        else if (flags3 & (RF3_AMBERITE)) roff_lappend("type", "Amberite");
        else if (flags2 & (RF2_QUANTUM)) roff_lappend("type", "quantum creature");
#endif /* ZANGBANDTK */
//		else roff_lappend("type", "creature");

		/* calculate the integer exp part */
		i = (long)r_ptr->mexp * r_ptr->level / p_ptr->lev;

		/* calculate the fractional exp part scaled by 100, */
		/* must use long arithmetic to avoid overflow  */
		j = ((((long)r_ptr->mexp * r_ptr->level % p_ptr->lev) *
		      (long)1000 / p_ptr->lev + 5) / 10);

		/* Mention the experience */
		roff_fmt("points", "%ld.%02ld", (long)i, (long)j);
	}

#if defined(OANGBANDTK)
	/* If no kills, known racial information should still be displayed. -LM- */
	else if ((flags3 & (RF3_ANIMAL)) || (flags3 & (RF3_EVIL)) || 
		(flags3 & (RF3_UNDEAD)) || (flags3 & (RF3_DRAGON)) || 
		(flags3 & (RF3_DEMON)) || (flags3 & (RF3_GIANT)) || 
		(flags3 & (RF3_TROLL)) || (flags3 & (RF3_ORC)))
	{
		/* Describe the "quality" */
		if (flags3 & (RF3_ANIMAL)) 
		{
			roff_lappend("type", "animal");
			add = TRUE;
		}
		if (flags3 & (RF3_EVIL))	
		{
			roff_lappend("type", "evil");
		}
		if (flags3 & (RF3_UNDEAD)) 
		{
			roff_lappend("type", "undead");
		}

		/* Describe the "race" */
		if (flags3 & (RF3_DRAGON)) roff_lappend("type", "dragon");
		else if (flags3 & (RF3_DEMON)) roff_lappend("type", "demon");
		else if (flags3 & (RF3_GIANT)) roff_lappend("type", "giant");
		else if (flags3 & (RF3_TROLL)) roff_lappend("type", "troll");
		else if (flags3 & (RF3_ORC)) 
		{
			roff_lappend("type", "orc");
		}
//		else roff_lappend("type", "creature");
	}
#endif /* OANGBANDTK */

#if defined(ZANGBANDTK)
    if ((flags2 & (RF2_AURA_FIRE)) && (flags2 & (RF2_AURA_ELEC)))
    {
        roff_lappend("special", "surrounded by flames and electricity");
    }
	else if ((flags3 & (RF3_AURA_COLD)) && (flags2 & (RF2_AURA_ELEC)))
	{
		roff_lappend("special", "surrounded by ice and electricity");
	}
    else if (flags2 & (RF2_AURA_FIRE))
    {
        roff_lappend("special", "surrounded by flames");
    }
	else if (flags3 & (RF3_AURA_COLD))
	{
		roff_lappend("special", "surrounded by ice");
	}
    else if (flags2 & (RF2_AURA_ELEC))
    {
        roff_lappend("special", "surrounded by electricity");
    }

    if (flags2 & (RF2_REFLECTING))
    {
        roff_lappend("special", "reflects bolt spells");
    }
#endif /* ZANGBANDTK */

	/* Describe escorts */
	if ((flags1 & (RF1_ESCORT)) || (flags1 & (RF1_ESCORTS)))
	{
		roff_string("companions", "escorts");
	}

	/* Describe friends */
	else if ((flags1 & (RF1_FRIEND)) || (flags1 & (RF1_FRIENDS)))
	{
		roff_string("companions", "groups");
	}

	/* Collect inate attacks */
	vn = 0;
	if (flags4 & (RF4_SHRIEK))		vp[vn++] = "shriek for help";
#if defined(ANGBANDTK) || defined(KANGBANDTK)
	if (flags4 & (RF4_XXX2))		vp[vn++] = "do something";
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
	if (flags4 & (RF4_XXX3))		vp[vn++] = "do something";
#endif /* */
#if defined(ANGBANDTK) || defined(KANGBANDTK)
	if (flags4 & (RF4_XXX4))		vp[vn++] = "do something";
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
    if (flags4 & (RF4_ROCKET))		vp[vn++] = "shoot a rocket";
#endif /* ZANGBANDTK */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
	if (flags4 & (RF4_ARROW_1))		vp[vn++] = "fire an arrow";
	if (flags4 & (RF4_ARROW_2))		vp[vn++] = "fire arrows";
#endif /* */
#if defined(OANGBANDTK)
	if (flags4 & (RF4_BOULDER))		vp[vn++] = "throw a boulder";
	if (flags4 & (RF4_ARROW_5))		vp[vn++] = "fire a seeker arrow";
	if (flags4 & (RF4_ARROW_1))		vp[vn++] = "fire a little arrow";
	if (flags4 & (RF4_ARROW_2))		vp[vn++] = "fire an arrow";
#endif /* OANGBANDTK */
	if (flags4 & (RF4_ARROW_3))		vp[vn++] = "fire a missile";
	if (flags4 & (RF4_ARROW_4))		vp[vn++] = "fire missiles";

	/* Describe inate attacks */
	if (vn)
	{
		for (n = 0; n < vn; n++)
		{
			roff_lappend("attacks", vp[n]);
		}
	}


	/* Collect breaths */
	vn = 0;
	if (flags4 & (RF4_BR_ACID))		vp[vn++] = "acid";
	if (flags4 & (RF4_BR_ELEC))		vp[vn++] = "lightning";
	if (flags4 & (RF4_BR_FIRE))		vp[vn++] = "fire";
	if (flags4 & (RF4_BR_COLD))		vp[vn++] = "frost";
	if (flags4 & (RF4_BR_POIS))		vp[vn++] = "poison";
	if (flags4 & (RF4_BR_NETH))		vp[vn++] = "nether";
	if (flags4 & (RF4_BR_LITE))		vp[vn++] = "light";
	if (flags4 & (RF4_BR_DARK))		vp[vn++] = "darkness";
	if (flags4 & (RF4_BR_CONF))		vp[vn++] = "confusion";
	if (flags4 & (RF4_BR_SOUN))		vp[vn++] = "sound";
	if (flags4 & (RF4_BR_CHAO))		vp[vn++] = "chaos";
	if (flags4 & (RF4_BR_DISE))		vp[vn++] = "disenchantment";
	if (flags4 & (RF4_BR_NEXU))		vp[vn++] = "nexus";
	if (flags4 & (RF4_BR_TIME))		vp[vn++] = "time";
	if (flags4 & (RF4_BR_INER))		vp[vn++] = "inertia";
	if (flags4 & (RF4_BR_GRAV))		vp[vn++] = "gravity";
	if (flags4 & (RF4_BR_SHAR))		vp[vn++] = "shards";
	if (flags4 & (RF4_BR_PLAS))		vp[vn++] = "plasma";
	if (flags4 & (RF4_BR_WALL))		vp[vn++] = "force";
	if (flags4 & (RF4_BR_MANA))		vp[vn++] = "mana";
#if defined(ANGBANDTK) || defined(KANGBANDTK)
	if (flags4 & (RF4_XXX5))		vp[vn++] = "something";
	if (flags4 & (RF4_XXX6))		vp[vn++] = "something";
	if (flags4 & (RF4_XXX7))		vp[vn++] = "something";
	if (flags4 & (RF4_XXX8))		vp[vn++] = "something";
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
    if (flags4 & (RF4_BR_NUKE))  vp[vn++] = "toxic waste";
    if (flags4 & (RF4_BR_DISI))  vp[vn++] = "disintegration";
#endif /* ZANGBANDTK */

	/* Describe breaths */
	if (vn)
	{
		/* Note breath */
		breath = TRUE;

		for (n = 0; n < vn; n++)
		{
			roff_lappend("breath", vp[n]);
		}
	}


	/* Collect spells */
	vn = 0;
	if (flags5 & (RF5_BA_ACID))		vp[vn++] = "produce acid balls";
	if (flags5 & (RF5_BA_ELEC))		vp[vn++] = "produce lightning balls";
	if (flags5 & (RF5_BA_FIRE))		vp[vn++] = "produce fire balls";
	if (flags5 & (RF5_BA_COLD))		vp[vn++] = "produce frost balls";
	if (flags5 & (RF5_BA_POIS))		vp[vn++] = "produce poison balls";
	if (flags5 & (RF5_BA_NETH))		vp[vn++] = "produce nether balls";
	if (flags5 & (RF5_BA_WATE))		vp[vn++] = "produce water balls";
#if defined(ZANGBANDTK)
    if (flags4 & (RF4_BA_NUKE))  vp[vn++] = "produce balls of radiation";
#endif /* ZANGBANDTK */
	if (flags5 & (RF5_BA_MANA))		vp[vn++] = "invoke mana storms";
	if (flags5 & (RF5_BA_DARK))		vp[vn++] = "invoke darkness storms";
#if defined(ZANGBANDTK)
    if (flags4 & (RF4_BA_CHAO))  vp[vn++] = "invoke raw Logrus";
    if (flags6 & (RF6_HAND_DOOM))  vp[vn++] = "invoke the Hand of Doom";
#endif /* ZANGBANDTK */
	if (flags5 & (RF5_DRAIN_MANA))	vp[vn++] = "drain mana";
	if (flags5 & (RF5_MIND_BLAST))	vp[vn++] = "cause mind blasting";
	if (flags5 & (RF5_BRAIN_SMASH))	vp[vn++] = "cause brain smashing";
#if defined(ANGBANDTK) || defined(KANGBANDTK)
	if (flags5 & (RF5_CAUSE_1))		vp[vn++] = "cause light wounds";
	if (flags5 & (RF5_CAUSE_2))		vp[vn++] = "cause serious wounds";
	if (flags5 & (RF5_CAUSE_3))		vp[vn++] = "cause critical wounds";
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
    if (flags5 & (RF5_CAUSE_1))  vp[vn++] = "cause light wounds and cursing";
    if (flags5 & (RF5_CAUSE_2))  vp[vn++] = "cause serious wounds and cursing";
    if (flags5 & (RF5_CAUSE_3))  vp[vn++] = "cause critical wounds and cursing";
#endif /* ZANGBANDTK */
	if (flags5 & (RF5_CAUSE_4))		vp[vn++] = "cause mortal wounds";
	if (flags5 & (RF5_BO_ACID))		vp[vn++] = "produce acid bolts";
	if (flags5 & (RF5_BO_ELEC))		vp[vn++] = "produce lightning bolts";
	if (flags5 & (RF5_BO_FIRE))		vp[vn++] = "produce fire bolts";
	if (flags5 & (RF5_BO_COLD))		vp[vn++] = "produce frost bolts";
	if (flags5 & (RF5_BO_POIS))		vp[vn++] = "produce poison bolts";
	if (flags5 & (RF5_BO_NETH))		vp[vn++] = "produce nether bolts";
	if (flags5 & (RF5_BO_WATE))		vp[vn++] = "produce water bolts";
	if (flags5 & (RF5_BO_MANA))		vp[vn++] = "produce mana bolts";
	if (flags5 & (RF5_BO_PLAS))		vp[vn++] = "produce plasma bolts";
	if (flags5 & (RF5_BO_ICEE))		vp[vn++] = "produce ice bolts";
	if (flags5 & (RF5_MISSILE))		vp[vn++] = "produce magic missiles";
	if (flags5 & (RF5_SCARE))		vp[vn++] = "terrify";
	if (flags5 & (RF5_BLIND))		vp[vn++] = "blind";
	if (flags5 & (RF5_CONF))		vp[vn++] = "confuse";
	if (flags5 & (RF5_SLOW))		vp[vn++] = "slow";
	if (flags5 & (RF5_HOLD))		vp[vn++] = "paralyze";
	if (flags6 & (RF6_HASTE))		vp[vn++] = "haste-self";
#if defined(ANGBANDTK) || defined(KANGBANDTK)
	if (flags6 & (RF6_XXX1))		vp[vn++] = "do something";
#endif /* ANGBANDTK, KANGBANDTK */
	if (flags6 & (RF6_HEAL))		vp[vn++] = "heal-self";
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	if (flags6 & (RF6_XXX2))		vp[vn++] = "do something";
#endif
#if defined(ZANGBANDTK)
	if (flags6 & (RF6_INVULNER))	vp[vn++] = "make invulnerable";
#endif
	if (flags6 & (RF6_BLINK))		vp[vn++] = "blink-self";
	if (flags6 & (RF6_TPORT))		vp[vn++] = "teleport-self";
	if (flags6 & (RF6_XXX3))		vp[vn++] = "do something";
	if (flags6 & (RF6_XXX4))		vp[vn++] = "do something";
	if (flags6 & (RF6_TELE_TO))		vp[vn++] = "teleport to";
	if (flags6 & (RF6_TELE_AWAY))		vp[vn++] = "teleport away";
	if (flags6 & (RF6_TELE_LEVEL))	vp[vn++] = "teleport level";
	if (flags6 & (RF6_XXX5))		vp[vn++] = "do something";
	if (flags6 & (RF6_DARKNESS))		vp[vn++] = "create darkness";
	if (flags6 & (RF6_TRAPS))		vp[vn++] = "create traps";
	if (flags6 & (RF6_FORGET))		vp[vn++] = "cause amnesia";
#if defined(OANGBANDTK)
	if (flags6 & (RF6_BA_LITE))		vp[vn++] = "invoke starbursts";
	if (flags6 & (RF6_S_KIN))		vp[vn++] = "summon simmilar creatures";
	if (flags6 & (RF6_S_HI_DEMON))	vp[vn++] = "summon greater demons";
#endif /* */
#if defined(ANGBANDTK) || defined(KANGBANDTK)
	if (flags6 & (RF6_S_KIN))		vp[vn++] = "summon similar monsters";
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
	if (flags6 & (RF6_RAISE_DEAD))      vp[vn++] = "raise dead";
#endif /* ZANGBANDTK */
	if (flags6 & (RF6_S_MONSTER))		vp[vn++] = "summon a monster";
	if (flags6 & (RF6_S_MONSTERS))	vp[vn++] = "summon monsters";
#if defined(ZANGBANDTK)
    if (flags6 & (RF6_S_KIN))  vp[vn++] = "summon aid";
#endif /* ZANGBANDTK */
	if (flags6 & (RF6_S_ANT))		vp[vn++] = "summon ants";
	if (flags6 & (RF6_S_SPIDER))		vp[vn++] = "summon spiders";
	if (flags6 & (RF6_S_HOUND))		vp[vn++] = "summon hounds";
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
	if (flags6 & (RF6_S_HYDRA))		vp[vn++] = "summon hydras";
#endif /* */
#if defined(OANGBANDTK)
	if (flags6 & (RF6_XXX9))		vp[vn++] = "do something";
	if (flags6 & (RF6_XXX10))		vp[vn++] = "do something";
#endif /* */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
	if (flags6 & (RF6_S_ANGEL))		vp[vn++] = "summon an angel";
#endif
	if (flags6 & (RF6_S_DEMON))		vp[vn++] = "summon a demon";
	if (flags6 & (RF6_S_UNDEAD))		vp[vn++] = "summon an undead";
	if (flags6 & (RF6_S_DRAGON))		vp[vn++] = "summon a dragon";
	if (flags6 & (RF6_S_HI_UNDEAD))	vp[vn++] = "summon Greater Undead";
	if (flags6 & (RF6_S_HI_DRAGON))	vp[vn++] = "summon Ancient Dragons";
#if defined(ANGBANDTK)
	if (flags6 & (RF6_S_HI_DEMON))		vp[vn++] = "summon Greater Demons";
	if (flags6 & (RF6_S_WRAITH))		vp[vn++] = "summon Ring Wraiths";
#endif /* ANGBANDTK */
#if defined(KANGBANDTK)
	if (flags6 & (RF6_S_WRAITH))		vp[vn++] = "summon Ring Wraiths";
#endif /* KANGBANDTK */
#if defined(ZANGBANDTK)
    if (flags6 & (RF6_S_CYBER)) vp[vn++] = "summon Cyberdemons";
    if (flags6 & (RF6_S_AMBERITES))  vp[vn++] = "summon Lords of Amber";
#endif /* ZANGBANDTK */
	if (flags6 & (RF6_S_UNIQUE))		vp[vn++] = "summon Unique Monsters";

	/* Describe spells */
	if (vn)
	{
		/* Note magic */
		magic = TRUE;

		for (n = 0; n < vn; n++)
		{
			roff_lappend("spells", vp[n]);
		}
	}


	/* End the sentence about inate/other spells */
	if (breath || magic)
	{
		/* Total casting */
		m = l_ptr->LF(cast_inate) + l_ptr->LF(cast_spell);

		/* Average frequency */
		n = (r_ptr->freq_inate + r_ptr->freq_spell) / 2;

		/* Describe the spell frequency */
		if (m > 100)
		{
			roff_bool("frequency_known", TRUE);
			roff_number("frequency", 100 / n);
		}

		/* Guess at the frequency */
		else if (m)
		{
			roff_bool("frequency_known", FALSE);
			n = ((n + 9) / 10) * 10;
			roff_number("frequency", 100 / n);
		}
	}


	/* Describe monster "toughness" */
	if (know_armour(r_idx))
	{
		/* Armor */
		roff_number("armor", r_ptr->ac);

		/* Maximized hitpoints */
		if (flags1 & (RF1_FORCE_MAXHP))
		{
			roff_number("health", r_ptr->hdice * r_ptr->hside);
		}

		/* Variable hitpoints */
		else
		{
			roff_fmt("health", "%dd%d", r_ptr->hdice, r_ptr->hside);
		}
	}


	/* Collect special abilities. */
	vn = 0;
	if (flags2 & (RF2_OPEN_DOOR)) vp[vn++] = "open doors";
	if (flags2 & (RF2_BASH_DOOR)) vp[vn++] = "bash down doors";
	if (flags2 & (RF2_PASS_WALL)) vp[vn++] = "pass through walls";
	if (flags2 & (RF2_KILL_WALL)) vp[vn++] = "bore through walls";
	if (flags2 & (RF2_MOVE_BODY)) vp[vn++] = "push past weaker monsters";
	if (flags2 & (RF2_KILL_BODY)) vp[vn++] = "destroy weaker monsters";
	if (flags2 & (RF2_TAKE_ITEM)) vp[vn++] = "pick up objects";
	if (flags2 & (RF2_KILL_ITEM)) vp[vn++] = "destroy objects";

	/* Describe special abilities. */
	if (vn)
	{
		for (n = 0; n < vn; n++)
		{
			roff_lappend("special", vp[n]);
		}
	}


	/* Describe special abilities. */
	if (flags2 & (RF2_INVISIBLE))
	{
		roff_lappend("type", "invisible");
	}
	if (flags2 & (RF2_COLD_BLOOD))
	{
		roff_lappend("type", "cold blooded");
	}
	if (flags2 & (RF2_EMPTY_MIND))
	{
		roff_lappend("special", "not detected by telepathy");
	}
	if (flags2 & (RF2_WEIRD_MIND))
	{
		roff_lappend("special", "rarely detected by telepathy");
	}
	if (flags2 & (RF2_MULTIPLY))
	{
		roff_lappend("special", "breeds explosively");
	}
	if (flags2 & (RF2_REGENERATE))
	{
		roff_lappend("special", "regenerates quickly");
	}


	/* Collect susceptibilities */
	vn = 0;
	if (flags3 & (RF3_HURT_ROCK)) vp[vn++] = "rock remover";
	if (flags3 & (RF3_HURT_LITE)) vp[vn++] = "bright light";
	if (flags3 & (RF3_HURT_FIRE)) vp[vn++] = "fire";
	if (flags3 & (RF3_HURT_COLD)) vp[vn++] = "cold";

	/* Describe susceptibilities */
	if (vn)
	{
		for (n = 0; n < vn; n++)
		{
			roff_lappend("weakness", vp[n]);
		}
	}


	/* Collect immunities */
	vn = 0;
	if (flags3 & (RF3_IM_ACID)) vp[vn++] = "acid";
	if (flags3 & (RF3_IM_ELEC)) vp[vn++] = "lightning";
	if (flags3 & (RF3_IM_FIRE)) vp[vn++] = "fire";
	if (flags3 & (RF3_IM_COLD)) vp[vn++] = "cold";
	if (flags3 & (RF3_IM_POIS)) vp[vn++] = "poison";
#if defined(KANGBANDTK)
	if (flags3 & (RF3_NO_CONF)) vp[vn++] = "confusion"; /* -KMW- */
	if (flags3 & (RF3_NO_SLEEP)) vp[vn++] = "sleep";  /* -KMW- */
#endif /* KANGBANDTK */

	/* Describe immunities */
	if (vn)
	{
		for (n = 0; n < vn; n++)
		{
			roff_lappend("resist", vp[n]);
		}
	}


	/* Collect resistances */
	vn = 0;
	if (flags3 & (RF3_RES_NETH)) vp[vn++] = "nether";
	if (flags3 & (RF3_RES_WATE)) vp[vn++] = "water";
	if (flags3 & (RF3_RES_PLAS)) vp[vn++] = "plasma";
	if (flags3 & (RF3_RES_NEXU)) vp[vn++] = "nexus";
	if (flags3 & (RF3_RES_DISE)) vp[vn++] = "disenchantment";
#if defined(ZANGBANDTK)
    if (flags3 & (RF3_RES_TELE) && !(r_ptr->flags1 & RF1_UNIQUE)) vp[vn++] = "teleportation";
#endif /* ZANGBANDTK */

	/* Describe resistances */
	if (vn)
	{
		for (n = 0; n < vn; n++)
		{
			roff_lappend("resist", vp[n]);
		}
	}


	/* Collect non-effects */
	vn = 0;
#if defined(ANGBANDTK) || defined(KANGBANDTK)
	if (flags3 & (RF3_NO_STUN)) vp[vn++] = "stun";
	if (flags3 & (RF3_NO_FEAR)) vp[vn++] = "fear";
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ANGBANDTK)
	if (flags3 & (RF3_NO_CONF)) vp[vn++] = "confuse";
	if (flags3 & (RF3_NO_SLEEP)) vp[vn++] = "sleep";
#endif /* ANGBANDTK */
#if defined(ZANGBANDTK)
	if (flags3 & RF3_NO_STUN) vp[vn++] = "stun";
	if (flags3 & RF3_NO_FEAR) vp[vn++] = "fear";
	if (flags3 & RF3_NO_CONF) vp[vn++] = "confuse";
	if (flags3 & RF3_NO_SLEEP && !(r_ptr->flags1 & RF1_UNIQUE)) vp[vn++] = "sleep";
	if (flags3 & RF3_RES_TELE && (r_ptr->flags1 & RF1_UNIQUE)) vp[vn++] = "teleport";
#endif /* ZANGBANDTK */

	/* Describe non-effects */
	if (vn)
	{
		for (n = 0; n < vn; n++)
		{
			roff_lappend("immune", vp[n]);
		}
	}


	/* Do we know how aware it is? */
	if ((((int)l_ptr->LF(wake) * (int)l_ptr->LF(wake)) > r_ptr->sleep) ||
	    (l_ptr->LF(ignore) == MAX_UCHAR) ||
	    ((r_ptr->sleep == 0) && (l_ptr->LF(tkills) >= 10)))
	{
		roff_number("sleep", r_ptr->sleep);
		roff_number("aaf", r_ptr->aaf);
	}


	/* Drops gold and/or items */
	if (l_ptr->LF(drop_gold) || l_ptr->LF(drop_item))
	{
		/* No "n" needed */
		sin = FALSE;

		/* Count maximum drop */
		n = MAX(l_ptr->LF(drop_gold), l_ptr->LF(drop_item));

		/* One drop (may need an "n") */
		if (n == 1)
		{
			roff_append("drop", "a");
			sin = TRUE;
		}

		/* Two drops */
		else if (n == 2)
		{
			roff_append("drop", "one or two");
		}

		/* Many drops */
		else
		{
			roff_append_fmt("drop", "up to %d", n);
		}


		/* Great */
		if (flags1 & (RF1_DROP_GREAT))
		{
			p = " exceptional";
		}

		/* Good (no "n" needed) */
		else if (flags1 & (RF1_DROP_GOOD))
		{
			p = " good";
			sin = FALSE;
		}

		/* Okay */
		else
		{
			p = NULL;
		}


		/* Objects */
		if (l_ptr->LF(drop_item))
		{
			/* Handle singular "an" */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
			if (sin) roff_append("drop", "n");
#endif /* */
#if defined(OANGBANDTK)
			if ((sin) && (!(r_ptr->flags1 & (RF1_DROP_CHEST))))
				roff_append("drop", "n");
#endif /* */
			sin = FALSE;

			/* Dump "object(s)" */
			if (p) roff_append("drop", p);
#if defined(OANGBANDTK)
			if (r_ptr->flags1 & (RF1_DROP_CHEST))
				roff_append("drop", " chest");
			else
#endif /* */
			roff_append("drop", " object");
			if (n != 1) roff_append("drop", "s");

			/* Conjunction replaces variety, if needed for "gold" below */
			p = " or";
		}

		/* Treasures */
		if (l_ptr->LF(drop_gold))
		{
			/* Cancel prefix */
			if (!p) sin = FALSE;

			/* Handle singular "an" */
			if (sin) roff_append("drop", "n");
			sin = FALSE;

			/* Dump "treasure(s)" */
			if (p) roff_append("drop", p);
			roff_append("drop", " treasure");
			if (n != 1) roff_append("drop", "s");
		}
	}


	/* Count the number of "known" attacks */
	for (n = 0, m = 0; m < 4; m++)
	{
		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Count known attacks */
		if (l_ptr->LF(blows)[m]) n++;
	}

	/* Examine (and count) the actual attacks */
	for (r = 0, m = 0; m < 4; m++)
	{
		int method, effect, d1, d2;

		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Skip unknown attacks */
		if (!l_ptr->LF(blows)[m]) continue;


		/* Extract the attack info */
		method = r_ptr->blow[m].method;
		effect = r_ptr->blow[m].effect;
		d1 = r_ptr->blow[m].d_dice;
		d2 = r_ptr->blow[m].d_side;


		/* No method yet */
		p = NULL;

		/* Acquire the method */
		switch (method)
		{
			case RBM_HIT:	p = "hit"; break;
			case RBM_TOUCH:	p = "touch"; break;
			case RBM_PUNCH:	p = "punch"; break;
			case RBM_KICK:	p = "kick"; break;
			case RBM_CLAW:	p = "claw"; break;
			case RBM_BITE:	p = "bite"; break;
			case RBM_STING:	p = "sting"; break;
			case RBM_XXX1:	break;
			case RBM_BUTT:	p = "butt"; break;
			case RBM_CRUSH:	p = "crush"; break;
			case RBM_ENGULF:	p = "engulf"; break;
#if defined(ANGBANDTK) || defined(KANGBANDTK)
			case RBM_XXX2:	break;
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
            case RBM_CHARGE: p = "charge";   break;
#endif /* ZANGBANDTK */
			case RBM_CRAWL:	p = "crawl on you"; break;
			case RBM_DROOL:	p = "drool on you"; break;
			case RBM_SPIT:	p = "spit"; break;
#if defined(ANGBANDTK) || defined(KANGBANDTK)
			case RBM_XXX3:	break;
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
			case RBM_EXPLODE:	p = "explode"; break;
#endif /* ZANGBANDTK */
			case RBM_GAZE:	p = "gaze"; break;
			case RBM_WAIL:	p = "wail"; break;
			case RBM_SPORE:	p = "release spores"; break;
			case RBM_XXX4:	break;
			case RBM_BEG:	p = "beg"; break;
			case RBM_INSULT:	p = "insult"; break;
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
			case RBM_MOAN:	p = "moan"; break;
#endif /* */
#if defined(ANGBANDTK) || defined(KANGBANDTK)
			case RBM_XXX5:	break;
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(OANGBANDTK)
			case RBM_SNEER:	p = "sneer"; break;
			case RBM_REQUEST:	p = "offer to trade"; break;
#endif /* */
#if defined(ZANGBANDTK)
            case RBM_SHOW:  p = "sing"; break;
#endif /* ZANGBANDTK */
		}


		/* Default effect */
		q = NULL;

		/* Acquire the effect */
		switch (effect)
		{
			case RBE_HURT:	q = "attack"; break;
			case RBE_POISON:	q = "poison"; break;
			case RBE_UN_BONUS:	q = "disenchant"; break;
			case RBE_UN_POWER:	q = "drain charges"; break;
			case RBE_EAT_GOLD:	q = "steal gold"; break;
			case RBE_EAT_ITEM:	q = "steal items"; break;
			case RBE_EAT_FOOD:	q = "eat your food"; break;
			case RBE_EAT_LITE:	q = "absorb light"; break;
			case RBE_ACID:	q = "shoot acid"; break;
#if defined(ANGBANDTK) || defined(KANGBANDTK)
			case RBE_ELEC:	q = "electrify"; break;
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
            case RBE_ELEC: q = "electrocute"; break;
#endif /* ZANGBANDTK */
			case RBE_FIRE:	q = "burn"; break;
			case RBE_COLD:	q = "freeze"; break;
			case RBE_BLIND:	q = "blind"; break;
			case RBE_CONFUSE:	q = "confuse"; break;
			case RBE_TERRIFY:	q = "terrify"; break;
			case RBE_PARALYZE:	q = "paralyze"; break;
			case RBE_LOSE_STR:	q = "reduce strength"; break;
			case RBE_LOSE_INT:	q = "reduce intelligence"; break;
			case RBE_LOSE_WIS:	q = "reduce wisdom"; break;
			case RBE_LOSE_DEX:	q = "reduce dexterity"; break;
			case RBE_LOSE_CON:	q = "reduce constitution"; break;
			case RBE_LOSE_CHR:	q = "reduce charisma"; break;
			case RBE_LOSE_ALL:	q = "reduce all stats"; break;
			case RBE_SHATTER:	q = "shatter"; break;
#if defined(ANGBANDTK) || defined(ZANGBANDTK)
			case RBE_EXP_10:	q = "lower experience (by 10d6+)"; break;
			case RBE_EXP_20:	q = "lower experience (by 20d6+)"; break;
			case RBE_EXP_40:	q = "lower experience (by 40d6+)"; break;
			case RBE_EXP_80:	q = "lower experience (by 80d6+)"; break;
#endif /* */
#if defined(KANGBANDTK)
			case RBE_EXP_10:	q = "lower experience"; break;
			case RBE_EXP_20:	q = "lower experience"; break;
			case RBE_EXP_40:	q = "lower experience"; break;
			case RBE_EXP_80:	q = "lower experience"; break;
#endif /* */
#if defined(ZANGBANDTK)
			case RBE_DISEASE:	q = "disease"; break;
			case RBE_TIME: q = "time"; break;
			case RBE_EXP_VAMP: q = "drain life force"; break;
#endif /* ZANGBANDTK */
		}


		/* Hack -- force a method */
		if (!p) p = "do something weird";

		/* Describe the effect (if any) */
		if (q)
		{
			/* Describe damage (if known) */
			if (d1 && d2 && know_damage(r_idx, m))
			{
				roff_lappend_fmt("attacks", "%s to %s with damage %dd%d", p, q, d1, d2);
			}
			else
			{
				roff_lappend_fmt("attacks", "%s to %s", p, q);
			}
		}
		else
		{
			roff_lappend("attacks", p);
		}


		/* Count the attacks as printed */
		r++;
	}


#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	/* Notice "Quest" monsters */
	if (flags1 & (RF1_QUESTOR))
#endif /* */
#if defined(ZANGBANDTK)
	/*
	 * Notice "Quest" monsters, but only if you
	 * already encountered the monster.
	 */
	if ((flags1 & RF1_QUESTOR) && (l_ptr->LF(sights)))
#endif /* */
	{
		roff_lappend("type", "questor");
	}

	/* Cheat -- know everything */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
	if (cheat_know)
#endif /* */
#if defined(OANGBANDTK)
	if ((cheat_know) || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
#endif /* */
	{
		/* Hack -- restore memory */
		COPY(l_ptr, &save_mem, LORE_TYPE);
	}
}

#endif /* ANGBANDTK, KANGBANDTK, ZANGBANDTK */
