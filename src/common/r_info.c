/* File: r_info.c */

/* Purpose: monster recall to a buffer */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)

#include "angband.h"
#include "tnb.h"

/*
 * Pronoun arrays, by gender.
 */
static cptr wd_he[3] =
{ "it", "he", "she" };
static cptr wd_his[3] =
{ "its", "his", "her" };

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

/* The output buffer */
static char *s_buffer;

/* Write a string into the output buffer */
static void roff2(cptr str)
{
	s_buffer += strcpy_len(s_buffer, str);
}

/* Write a formatted string into the output buffer */
static void roff2fmt(cptr fmt, ...)
{
	va_list vp;

	/* Begin the Varargs Stuff */
	va_start(vp, fmt);

	/* Build the string, assume 32K buffer */
	s_buffer += vstrnfmt(s_buffer, 32767, fmt, vp);

	/* End the Varargs Stuff */
	va_end(vp);
}

/*
 *--------------------------------------------------------------
 *
 * angtk_roff --
 *
 *	Write monster memory to the given text buffer.
 *
 *--------------------------------------------------------------
 */

long angtk_roff(int r_idx, char *buffer)
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

	/* Global access to the output buffer */
	s_buffer = buffer;

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
		/* Hack -- Determine if the unique is "dead" */
		bool dead = (r_ptr->max_num == 0) ? TRUE : FALSE;

		/* We've been killed... */
		if (l_ptr->LF(deaths))
		{
			/* Killed ancestors */
			roff2fmt("%^s has slain %d of your ancestors",
				wd_he[msex], l_ptr->LF(deaths));

			/* But we've also killed it */
			if (dead)
			{
				roff2fmt(", but you have avenged %s!  ",
					plural(l_ptr->LF(deaths), "him", "them"));
			}

			/* Unavenged (ever) */
			else
			{
				roff2fmt(", who %s unavenged.  ",
					plural(l_ptr->LF(deaths), "remains", "remain"));
			}
		}

		/* Dead unique who never hurt us */
		else if (dead)
		{
			roff2("You have slain this foe.  ");
		}
	}

	/* Not unique, but killed us */
	else if (l_ptr->LF(deaths))
	{
		/* Dead ancestors */
		roff2fmt(
			"%d of your ancestors %s been killed by this creature, ",
			l_ptr->LF(deaths), plural(l_ptr->LF(deaths), "has", "have"));

		/* Some kills this life */
		if (l_ptr->LF(pkills))
		{
			roff2fmt(
				"and you have exterminated at least %d of the creatures.  ",
			    l_ptr->LF(pkills));
		}

		/* Some kills past lives */
		else if (l_ptr->LF(tkills))
		{
			roff2fmt(
				"and %s have exterminated at least %d of the creatures.  ",
			    "your ancestors", l_ptr->LF(tkills));
		}

		/* No kills */
		else
		{
			roff2fmt(
				"and %s is not ever known to have been defeated.  ",
			    wd_he[msex]);
		}
	}

	/* Normal monsters */
	else
	{
		/* Killed some this life */
		if (l_ptr->LF(pkills))
		{
			roff2fmt(
				"You have killed at least %d of these creatures.  ",
			    l_ptr->LF(pkills));
		}

		/* Killed some last life */
		else if (l_ptr->LF(tkills))
		{
			roff2fmt(
				"Your ancestors have killed at least %d of these creatures.  ",
			    l_ptr->LF(tkills));
		}

		/* Killed none */
		else
		{
			roff2("No battles to the death are recalled.  ");
		}
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
		roff2(buf);
		roff2("  ");
	}


	/* Nothing yet */
	old = FALSE;

#if defined(OANGBANDTK)
	/* Player ghosts may have unique descriptions. -LM- */
	if ((r_ptr->flags2 & (RF2_PLAYER_GHOST)) && (ghost_string_type == 2))
		roff2fmt("%s  ", ghost_string);
#endif /* */

	/* Describe location */
	if (r_ptr->level == 0)
	{
		roff2fmt("%^s lives in the town", wd_he[msex]);
		old = TRUE;
	}
	else if (l_ptr->LF(tkills))
	{
		if (depth_in_feet)
		{
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
			roff2fmt(
				"%^s is normally found at depths of %d feet",
				wd_he[msex], r_ptr->level * 50);
#endif /* */
#if defined(OANGBANDTK)
			if (use_metric)
			{
				roff2fmt(
					"%^s is %s, normally found at depths of %d m",
					wd_he[msex],
					wd_rarity(r_ptr->rarity, r_ptr->flags1 & (RF1_UNIQUE)),
					r_ptr->level * 15);
			}
			else
			{
				roff2fmt(
					"%^s is %s, normally found at depths of %d feet",
					wd_he[msex],
					wd_rarity(r_ptr->rarity, r_ptr->flags1 & (RF1_UNIQUE)),
					r_ptr->level * 50);
			}
#endif /* */
		}
		else
		{
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
			roff2fmt(
				"%^s is normally found on dungeon level %d",
			    wd_he[msex], r_ptr->level);
#endif /* */
#if defined(OANGBANDTK)
			roff2fmt(
				"%^s is %s, normally found on dungeon level %d",
			    wd_he[msex],
			    wd_rarity(r_ptr->rarity, r_ptr->flags1 & (RF1_UNIQUE)),
			    r_ptr->level);
#endif /* */
		}
		old = TRUE;
	}


	/* Introduction */
	if (old)
	{
		roff2(", and ");
	}
	else
	{
		roff2fmt("%^s ", wd_he[msex]);
		old = TRUE;
	}
	if (flags1 & RF1_NEVER_MOVE)
		roff2("acts");
	else
		roff2("moves");

	/* Random-ness */
	if ((flags1 & (RF1_RAND_50)) || (flags1 & (RF1_RAND_25)))
	{
		/* Adverb */
		if ((flags1 & (RF1_RAND_50)) && (flags1 & (RF1_RAND_25)))
		{
			roff2(" extremely");
		}
		else if (flags1 & (RF1_RAND_50))
		{
			roff2(" somewhat");
		}
		else if (flags1 & (RF1_RAND_25))
		{
			roff2(" a bit");
		}

		/* Adjective */
		roff2(" erratically");

		/* Hack -- Occasional conjunction */
		if (r_ptr->speed != 110) roff2(", and");
	}

	/* Speed */
	if (r_ptr->speed > 110)
	{
		if (r_ptr->speed > 130) roff2(" incredibly");
		else if (r_ptr->speed > 120) roff2(" very");
#if defined(OANGBANDTK)
		else if (r_ptr->speed < 116) roff2(" moderately");
#endif /* */
		roff2(" quickly");
	}
	else if (r_ptr->speed < 110)
	{
		if (r_ptr->speed < 90) roff2(" extremely");
		else if (r_ptr->speed < 100) roff2(" very");
		roff2(" slowly");
	}
	else
	{
		roff2(" at normal speed");
	}

	/* The code above includes "attack speed" */
	if (flags1 & (RF1_NEVER_MOVE))
	{
		/* Introduce */
		if (old)
		{
			roff2(", but ");
		}
		else
		{
			roff2fmt("%^s ", wd_he[msex]);
			old = TRUE;
		}

		/* Describe */
		roff2("does not deign to chase intruders");
	}

	/* End this sentence */
	if (old)
	{
		roff2(".  ");
		old = FALSE;
	}


	/* Describe experience if known */
	if (l_ptr->LF(tkills))
	{
		/* Introduction */
		if (flags1 & (RF1_UNIQUE))
		{
			roff2("Killing this");
		}
		else
		{
			roff2("A kill of this");
		}

		/* Describe the "quality" */
#if defined(ZANGBANDTK)
        if (flags2 & (RF2_ELDRITCH_HORROR)) roff2(" sanity-blasting");
#endif /* ZANGBANDTK */
		if (flags3 & (RF3_ANIMAL)) roff2(" natural");
		if (flags3 & (RF3_EVIL)) roff2(" evil");
#if defined(ZANGBANDTK)
        if (flags3 & (RF3_GOOD)) roff2(" good");
#endif /* ZANGBANDTK */
		if (flags3 & (RF3_UNDEAD)) roff2(" undead");

		/* Describe the "race" */
		if (flags3 & (RF3_DRAGON)) roff2(" dragon");
		else if (flags3 & (RF3_DEMON)) roff2(" demon");
		else if (flags3 & (RF3_GIANT)) roff2(" giant");
		else if (flags3 & (RF3_TROLL)) roff2(" troll");
		else if (flags3 & (RF3_ORC)) roff2(" orc");
#if defined(ZANGBANDTK)
        else if (flags3 & (RF3_AMBERITE)) roff2(" Amberite");
        else if (flags2 & (RF2_QUANTUM)) roff2(" quantum creature");
#endif /* ZANGBANDTK */
		else roff2(" creature");

		/* calculate the integer exp part */
		i = (long)r_ptr->mexp * r_ptr->level / p_ptr->lev;

		/* calculate the fractional exp part scaled by 100, */
		/* must use long arithmetic to avoid overflow  */
		j = ((((long)r_ptr->mexp * r_ptr->level % p_ptr->lev) *
		      (long)1000 / p_ptr->lev + 5) / 10);

		/* Mention the experience */
		roff2fmt(
			" is worth %ld.%02ld point%s",
		    (long)i, (long)j,
		    (((i == 1) && (j == 0)) ? "" : "s"));

		/* Take account of annoying English */
		p = "th";
		i = p_ptr->lev % 10;
		if ((p_ptr->lev / 10) == 1) /* nothing */;
		else if (i == 1) p = "st";
		else if (i == 2) p = "nd";
		else if (i == 3) p = "rd";

		/* Take account of "leading vowels" in numbers */
		q = "";
		i = p_ptr->lev;
		if ((i == 8) || (i == 11) || (i == 18)) q = "n";

		/* Mention the dependance on the player's level */
		roff2fmt(
			" for a%s %lu%s level character.  ",
		    q, (long)i, p);
	}

#if defined(OANGBANDTK)
	/* If no kills, known racial information should still be displayed. -LM- */
	else if ((flags3 & (RF3_ANIMAL)) || (flags3 & (RF3_EVIL)) || 
		(flags3 & (RF3_UNDEAD)) || (flags3 & (RF3_DRAGON)) || 
		(flags3 & (RF3_DEMON)) || (flags3 & (RF3_GIANT)) || 
		(flags3 & (RF3_TROLL)) || (flags3 & (RF3_ORC)))
	{
		bool add = FALSE;

		/* Pronoun. */
		if (flags1 & (RF1_UNIQUE)) 
			roff2fmt("%^s is a", wd_he[msex]);
		else roff2("It is a");

		/* Describe the "quality" */
		if (flags3 & (RF3_ANIMAL)) 
		{
			roff2(" natural");
			add = TRUE;
		}
		if (flags3 & (RF3_EVIL))	
		{
			if (add) roff2(" evil");
			else roff("n evil", 0, 0);
			add = TRUE;
		}
		if (flags3 & (RF3_UNDEAD)) 
		{
			if (add) roff2(" undead");
			else roff2("n undead");
			add = TRUE;
		}

		/* Describe the "race" */
		if (flags3 & (RF3_DRAGON)) roff2(" dragon");
		else if (flags3 & (RF3_DEMON)) roff2(" demon");
		else if (flags3 & (RF3_GIANT)) roff2(" giant");
		else if (flags3 & (RF3_TROLL)) roff2(" troll");
		else if (flags3 & (RF3_ORC)) 
		{
			if (add) roff2(" orc");
			else roff2("n orc");
		}
		else roff2(" creature");

		/* End sentence. */
		roff2(".  ");
	}
#endif /* OANGBANDTK */

#if defined(ZANGBANDTK)
    if ((flags2 & (RF2_AURA_FIRE)) && (flags2 & (RF2_AURA_ELEC)))
    {
        roff2fmt("%^s is surrounded by flames and electricity.  ", wd_he[msex]);
    }
	else if ((flags3 & (RF3_AURA_COLD)) && (flags2 & (RF2_AURA_ELEC)))
	{
		roff2fmt("%^s is surrounded by ice and electricity.  ", wd_he[msex]);
	}
    else if (flags2 & (RF2_AURA_FIRE))
    {
        roff2fmt("%^s is surrounded by flames.  ", wd_he[msex]);
    }
	else if (flags3 & (RF3_AURA_COLD))
	{
		roff2fmt("%^s is surrounded by ice.  ", wd_he[msex]);
	}
    else if (flags2 & (RF2_AURA_ELEC))
    {
        roff2fmt("%^s is surrounded by electricity.  ", wd_he[msex]);
    }

    if (flags2 & (RF2_REFLECTING))
    {
        roff2fmt("%^s reflects bolt spells.  ", wd_he[msex]);
    }
#endif /* ZANGBANDTK */

	/* Describe escorts */
	if ((flags1 & (RF1_ESCORT)) || (flags1 & (RF1_ESCORTS)))
	{
		roff2fmt(
			"%^s usually appears with escorts.  ",
		    wd_he[msex]);
	}

	/* Describe friends */
	else if ((flags1 & (RF1_FRIEND)) || (flags1 & (RF1_FRIENDS)))
	{
		roff2fmt(
			"%^s usually appears in groups.  ",
		    wd_he[msex]);
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
		/* Intro */
		roff2fmt("%^s", wd_he[msex]);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff2(" may ");
			else if (n < vn-1) roff2(", ");
			else roff2(" or ");

			/* Dump */
			roff2(vp[n]);
		}

		/* End */
		roff2(".  ");
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

		/* Intro */
		roff2fmt("%^s", wd_he[msex]);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff2(" may breathe ");
			else if (n < vn-1) roff2(", ");
			else roff2(" or ");

			/* Dump */
			roff2(vp[n]);
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

		/* Intro */
		if (breath)
		{
			roff2(", and is also");
		}
		else
		{
			roff2fmt("%^s is", wd_he[msex]);
		}

		/* Verb Phrase */
		roff2(" magical, casting spells");

		/* Adverb */
		if (flags2 & (RF2_SMART)) roff2(" intelligently");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff2(" which ");
			else if (n < vn-1) roff2(", ");
			else roff2(" or ");

			/* Dump */
			roff2(vp[n]);
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
			roff2fmt("; 1 time in %d", 100 / n);
		}

		/* Guess at the frequency */
		else if (m)
		{
			n = ((n + 9) / 10) * 10;
			roff2fmt("; about 1 time in %d", 100 / n);
		}

		/* End this sentence */
		roff2(".  ");
	}


	/* Describe monster "toughness" */
	if (know_armour(r_idx))
	{
		/* Armor */
		roff2fmt(
			"%^s has an armor rating of %d",
		    wd_he[msex], r_ptr->ac);

		/* Maximized hitpoints */
		if (flags1 & (RF1_FORCE_MAXHP))
		{
			roff2fmt(
				" and a life rating of %d.  ",
			    r_ptr->hdice * r_ptr->hside);
		}

		/* Variable hitpoints */
		else
		{
			roff2fmt(
				" and a life rating of %dd%d.  ",
			    r_ptr->hdice, r_ptr->hside);
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
		/* Intro */
		roff2fmt("%^s", wd_he[msex]);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff2(" can ");
			else if (n < vn-1) roff2(", ");
			else roff2(" and ");

			/* Dump */
			roff2(vp[n]);
		}

		/* End */
		roff2(".  ");
	}


	/* Describe special abilities. */
	if (flags2 & (RF2_INVISIBLE))
	{
		roff2fmt(
			"%^s is invisible.  ", wd_he[msex]);
	}
	if (flags2 & (RF2_COLD_BLOOD))
	{
		roff2fmt(
			"%^s is cold blooded.  ", wd_he[msex]);
	}
	if (flags2 & (RF2_EMPTY_MIND))
	{
		roff2fmt(
			"%^s is not detected by telepathy.  ", wd_he[msex]);
	}
	if (flags2 & (RF2_WEIRD_MIND))
	{
		roff2fmt(
			"%^s is rarely detected by telepathy.  ", wd_he[msex]);
	}
	if (flags2 & (RF2_MULTIPLY))
	{
		roff2fmt(
			"%^s breeds explosively.  ", wd_he[msex]);
	}
	if (flags2 & (RF2_REGENERATE))
	{
		roff2fmt(
			"%^s regenerates quickly.  ", wd_he[msex]);
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
		/* Intro */
		roff2fmt("%^s", wd_he[msex]);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff2(" is hurt by ");
			else if (n < vn-1) roff2(", ");
			else roff2(" and ");

			/* Dump */
			roff2(vp[n]);
		}

		/* End */
		roff2(".  ");
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
		/* Intro */
		roff2fmt("%^s", wd_he[msex]);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff2(" resists ");
			else if (n < vn-1) roff2(", ");
			else roff2(" and ");

			/* Dump */
			roff2(vp[n]);
		}

		/* End */
		roff2(".  ");
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
		/* Intro */
		roff2fmt("%^s", wd_he[msex]);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff2(" resists ");
			else if (n < vn-1) roff2(", ");
			else roff2(" and ");

			/* Dump */
			roff2(vp[n]);
		}

		/* End */
		roff2(".  ");
	}


	/* Collect non-effects */
	vn = 0;
#if defined(ANGBANDTK) || defined(KANGBANDTK)
	if (flags3 & (RF3_NO_STUN)) vp[vn++] = "stunned";
	if (flags3 & (RF3_NO_FEAR)) vp[vn++] = "frightened";
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ANGBANDTK)
	if (flags3 & (RF3_NO_CONF)) vp[vn++] = "confused";
	if (flags3 & (RF3_NO_SLEEP)) vp[vn++] = "slept";
#endif /* ANGBANDTK */
#if defined(ZANGBANDTK)
	if (flags3 & RF3_NO_STUN) vp[vn++] = "stunned";
	if (flags3 & RF3_NO_FEAR) vp[vn++] = "frightened";
	if (flags3 & RF3_NO_CONF) vp[vn++] = "confused";
	if (flags3 & RF3_NO_SLEEP && !(r_ptr->flags1 & RF1_UNIQUE)) vp[vn++] = "slept";
	if (flags3 & RF3_RES_TELE && (r_ptr->flags1 & RF1_UNIQUE)) vp[vn++] = "teleported";
#endif /* ZANGBANDTK */

	/* Describe non-effects */
	if (vn)
	{
		/* Intro */
		roff2fmt("%^s", wd_he[msex]);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff2(" cannot be ");
			else if (n < vn-1) roff2(", ");
			else roff2(" or ");

			/* Dump */
			roff2(vp[n]);
		}

		/* End */
		roff2(".  ");
	}


	/* Do we know how aware it is? */
	if ((((int)l_ptr->LF(wake) * (int)l_ptr->LF(wake)) > r_ptr->sleep) ||
	    (l_ptr->LF(ignore) == MAX_UCHAR) ||
	    ((r_ptr->sleep == 0) && (l_ptr->LF(tkills) >= 10)))
	{
		cptr act;

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
		if (r_ptr->sleep > 200)
		{
			act = "prefers to ignore";
		}
		else if (r_ptr->sleep > 95)
		{
			act = "pays very little attention to";
		}
#endif /* */
#if defined(OANGBANDTK)
		if (r_ptr->sleep > 150)
		{
			act = "is nearly oblivious of";
		}
		else if (r_ptr->sleep > 95)
		{
			act = "prefers to ignore";
		}
#endif /* */
		else if (r_ptr->sleep > 75)
		{
			act = "pays little attention to";
		}
		else if (r_ptr->sleep > 45)
		{
			act = "tends to overlook";
		}
		else if (r_ptr->sleep > 25)
		{
			act = "takes quite a while to see";
		}
		else if (r_ptr->sleep > 10)
		{
			act = "takes a while to see";
		}
		else if (r_ptr->sleep > 5)
		{
			act = "is fairly observant of";
		}
		else if (r_ptr->sleep > 3)
		{
			act = "is observant of";
		}
		else if (r_ptr->sleep > 1)
		{
			act = "is very observant of";
		}
		else if (r_ptr->sleep > 0)
		{
			act = "is vigilant for";
		}
		else
		{
			act = "is ever vigilant for";
		}

		roff2fmt(
			"%^s %s intruders, which %s may notice from %d feet.  ",
		    wd_he[msex], act, wd_he[msex], 10 * r_ptr->aaf);
	}


	/* Drops gold and/or items */
	if (l_ptr->LF(drop_gold) || l_ptr->LF(drop_item))
	{
		/* No "n" needed */
		sin = FALSE;

		/* Intro */
		roff2fmt("%^s may carry", wd_he[msex]);

		/* Count maximum drop */
		n = MAX(l_ptr->LF(drop_gold), l_ptr->LF(drop_item));

		/* One drop (may need an "n") */
		if (n == 1)
		{
			roff2(" a");
			sin = TRUE;
		}

		/* Two drops */
		else if (n == 2)
		{
			roff2(" one or two");
		}

		/* Many drops */
		else
		{
			roff2fmt(" up to %d", n);
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
			if (sin) roff2("n");
#endif /* */
#if defined(OANGBANDTK)
			if ((sin) && (!(r_ptr->flags1 & (RF1_DROP_CHEST))))
				roff2("n");
#endif /* */
			sin = FALSE;

			/* Dump "object(s)" */
			if (p) roff2(p);
#if defined(OANGBANDTK)
			if (r_ptr->flags1 & (RF1_DROP_CHEST))
				roff2(" chest");
			else
#endif /* */
			roff2(" object");
			if (n != 1) roff2("s");

			/* Conjunction replaces variety, if needed for "gold" below */
			p = " or";
		}

		/* Treasures */
		if (l_ptr->LF(drop_gold))
		{
			/* Cancel prefix */
			if (!p) sin = FALSE;

			/* Handle singular "an" */
			if (sin) roff2("n");
			sin = FALSE;

			/* Dump "treasure(s)" */
			if (p) roff2(p);
			roff2(" treasure");
			if (n != 1) roff2("s");
		}

		/* End this sentence */
		roff2(".  ");
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


		/* Introduce the attack description */
		if (!r)
		{
			roff2fmt("%^s can ", wd_he[msex]);
		}
		else if (r < n-1)
		{
			roff2(", ");
		}
		else
		{
			roff2(", and ");
		}


		/* Hack -- force a method */
		if (!p) p = "do something weird";

		/* Describe the method */
		roff2(p);


		/* Describe the effect (if any) */
		if (q)
		{
			/* Describe the attack type */
			roff2(" to ");
			roff2(q);

			/* Describe damage (if known) */
			if (d1 && d2 && know_damage(r_idx, m))
			{
				/* Display the damage */
				roff2(" with damage");
				roff2fmt(" %dd%d", d1, d2);
			}
		}


		/* Count the attacks as printed */
		r++;
	}

	/* Finish sentence above */
	if (r)
	{
		roff2(".  ");
	}

	/* Notice lack of attacks */
	else if (flags1 & (RF1_NEVER_BLOW))
	{
		roff2fmt("%^s has no physical attacks.  ", wd_he[msex]);
	}

	/* Or describe the lack of knowledge */
	else
	{
		roff2fmt("Nothing is known about %s attack.  ", wd_his[msex]);
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
		roff2("You feel an intense desire to kill this monster...  ");
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

	n = s_buffer - buffer;
	
	/* Trim trailing whitespace */
	while (buffer[n - 1] == ' ') n--;

	/* Terminate (again) */
	buffer[n] = '\0';
	
	/* Return the number of characters written */
	return n;
}

#endif /* ANGBANDTK, KANGBANDTK, ZANGBANDTK */
