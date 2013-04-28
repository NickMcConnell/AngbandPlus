/* File: r_info.c */

/* Purpose: monster recall to a buffer */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#if defined(OANGBANDTK)

#include "angband.h"
#include "tnb.h"


/* The output buffer */
static char *s_buffer;
static int s_len;

/* Write a string into the output buffer */
#undef c_roff
#undef roff
#define c_roff(a,s,x,y) _roff(s)
#define roff(s,x,y) _roff(s)
static void _roff(cptr str)
{
	s_len += strcpy_len(s_buffer + s_len, str);
}

/*
 * Pronoun arrays, by gender.
 */
static cptr wd_he[3] = { "it", "he", "she" };
static cptr wd_his[3] = { "its", "his", "her" };

/*
 * Descriptions of monster rarity.
 */
static char *wd_rarity(byte rarity, bool unique)
{
	static char rarity_desc[40];

	if (unique)
	{
		if (rarity == 1)
			strcpy(rarity_desc, "very often encountered");
		else if (rarity == 2)
			strcpy(rarity_desc, "often encountered");
		else if (rarity == 3)
			strcpy(rarity_desc, "fairly often encountered");
		else if (rarity == 4)
			strcpy(rarity_desc, "infrequently encountered");
		else if ((rarity == 5) || (rarity == 6))
			strcpy(rarity_desc, "seldom encountered");
		else if (rarity < 10)
			strcpy(rarity_desc, "very seldom encountered");
		else
			strcpy(rarity_desc, "almost never encountered");
	}
	else
	{
		if (rarity == 1)
			strcpy(rarity_desc, "very common");
		else if (rarity == 2)
			strcpy(rarity_desc, "common");
		else if (rarity == 3)
			strcpy(rarity_desc, "not very common");
		else if ((rarity == 4) || (rarity == 5))
			strcpy(rarity_desc, "fairly rare");
		else if (rarity < 9)
			strcpy(rarity_desc, "rare");
		else
			strcpy(rarity_desc, "extremely rare");
	}

	return rarity_desc;
}

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

	s32b kills = l_list[r_idx].tkills;

	/* Rangers learn quickly. */
	if (p_ptr->pclass == CLASS_RANGER)
		kills *= 2;

	/* Normal monsters */
	if (kills > 304 / (4 + level))
		return (TRUE);

	/* Skip non-uniques */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
		return (FALSE);

	/* Unique monsters */
	if (kills > 304 / (38 + (5 * level) / 4))
		return (TRUE);

	/* Assume false */
	return (FALSE);
}


/*
 * Determine if the "mana" is known
 * The higher the level, the fewer kills needed.
 */
static bool know_mana(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	s32b level = r_ptr->level;

	s32b kills = l_list[r_idx].tkills;

	/* Mages learn quickly. */
	if (p_ptr->pclass == CLASS_MAGE)
		kills *= 2;

	/* Normal monsters */
	if (kills > 304 / (4 + level))
		return (TRUE);

	/* Skip non-uniques */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
		return (FALSE);

	/* Unique monsters */
	if (kills > 304 / (38 + (5 * level) / 4))
		return (TRUE);

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

	s32b a = l_list[r_idx].blows[i];

	s32b d1 = r_ptr->blow[i].d_dice;
	s32b d2 = r_ptr->blow[i].d_side;

	s32b d = d1 * d2;

	/* Hack - keep the target number reasonable */
	if (d > 100)
		d = 100;

	/* Hack -- Rangers learn quickly. */
	if (p_ptr->pclass == CLASS_RANGER)
		level = 10 + 3 * level / 2;

	/* Normal monsters */
	if ((4 + level) * a > 80 * d)
		return (TRUE);

	/* Skip non-uniques */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
		return (FALSE);

	/* Unique monsters */
	if ((4 + level) * (2 * a) > 80 * d)
		return (TRUE);

	/* Assume false */
	return (FALSE);
}


/*
 * Hack -- display monster information using "roff()"
 *
 * Note that there is now a compiler option to only read the monster
 * descriptions from the raw file when they are actually needed, which
 * saves about 60K of memory at the cost of disk access during monster
 * recall, which is optional to the user.
 *
 * This function should only be called with the cursor placed at the
 * left edge of the screen, on a cleared line, in which the recall is
 * to take place.  One extra blank line is left after the recall.
 */
static void roff_aux(int r_idx)
{
	monster_race *r_ptr;
	monster_lore *l_ptr;

	bool old = FALSE;
	bool sin = FALSE;
	bool unique = FALSE;

	int m, n, r;

	cptr p, q;
	cptr name;

	int msex = 0;

	bool breath = FALSE;
	bool magic = FALSE;

	u32b flags1;
	u32b flags2;
	u32b flags3;
	u32b flags4;
	u32b flags5;
	u32b flags6;
	u32b flags7;

	int spower;

	int vn = 0;
	cptr vp[64];

	monster_lore save_mem;

#if 0

	/* Nothing erased */
	roff_old = 0;

	/* Reset the row */
	roff_row = 1;

	/* Reset the pointer */
	roff_p = roff_buf;

	/* No spaces yet */
	roff_s = NULL;

#endif


	/* Access the race and lore */
	r_ptr = &r_info[r_idx];
	l_ptr = &l_list[r_idx];

	/* Get the monster name */
	name = (r_name + r_ptr->name);

	/* Cheat -- know everything */
	if ((cheat_know) || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
	{
		/* XXX XXX XXX */

		/* Hack -- save memory */
		COPY(&save_mem, l_ptr, monster_lore);

		/* Hack -- Maximal kills */
		l_ptr->tkills = MAX_SHORT;

		/* Hack -- Maximal info */
		l_ptr->wake = l_ptr->ignore = MAX_UCHAR;

		/* Observe "maximal" attacks */
		for (m = 0; m < 4; m++)
		{
			/* Examine "actual" blows */
			if (r_ptr->blow[m].effect || r_ptr->blow[m].method)
			{
				/* Hack -- maximal observations */
				l_ptr->blows[m] = MAX_UCHAR;
			}
		}

		/* Hack -- maximal drops */
		l_ptr->drop_gold = l_ptr->drop_item =
			(((r_ptr->flags1 & (RF1_DROP_4D2)) ? 8 : 0) +
			((r_ptr->flags1 & (RF1_DROP_3D2)) ? 6 : 0) +
			((r_ptr->flags1 & (RF1_DROP_2D2)) ? 4 : 0) +
			((r_ptr->flags1 & (RF1_DROP_1D2)) ? 2 : 0) +
			((r_ptr->flags1 & (RF1_DROP_90)) ? 1 : 0) +
			((r_ptr->flags1 & (RF1_DROP_60)) ? 1 : 0));

		/* Hack -- but only "valid" drops */
		if (r_ptr->flags1 & (RF1_ONLY_GOLD))
			l_ptr->drop_item = 0;
		if (r_ptr->flags1 & (RF1_ONLY_ITEM))
			l_ptr->drop_gold = 0;

		/* Hack -- observe many spells */
		l_ptr->cast_inate = MAX_UCHAR;
		l_ptr->cast_spell = MAX_UCHAR;

		/* Hack -- know all the flags */
		l_ptr->flags1 = r_ptr->flags1;
		l_ptr->flags2 = r_ptr->flags2;
		l_ptr->flags3 = r_ptr->flags3;
		l_ptr->flags4 = r_ptr->flags4;
		l_ptr->flags5 = r_ptr->flags5;
		l_ptr->flags6 = r_ptr->flags6;
		l_ptr->flags7 = r_ptr->flags7;
	}


	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & (RF1_FEMALE))
		msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE))
		msex = 1;


	/* Obtain a copy of the "known" flags */
	flags1 = (r_ptr->flags1 & l_ptr->flags1);
	flags2 = (r_ptr->flags2 & l_ptr->flags2);
	flags3 = (r_ptr->flags3 & l_ptr->flags3);
	flags4 = (r_ptr->flags4 & l_ptr->flags4);
	flags5 = (r_ptr->flags5 & l_ptr->flags5);
	flags6 = (r_ptr->flags6 & l_ptr->flags6);
	flags7 = (r_ptr->flags7 & l_ptr->flags7);


	/* Assume some "obvious" flags */
	if (r_ptr->flags1 & (RF1_UNIQUE))
		flags1 |= (RF1_UNIQUE);
	if (r_ptr->flags1 & (RF1_QUESTOR))
		flags1 |= (RF1_QUESTOR);
	if (r_ptr->flags1 & (RF1_MALE))
		flags1 |= (RF1_MALE);
	if (r_ptr->flags1 & (RF1_FEMALE))
		flags1 |= (RF1_FEMALE);

	/* Assume some "creation" flags */
	if (r_ptr->flags1 & (RF1_FRIEND))
		flags1 |= (RF1_FRIEND);
	if (r_ptr->flags1 & (RF1_FRIENDS))
		flags1 |= (RF1_FRIENDS);
	if (r_ptr->flags1 & (RF1_ESCORT))
		flags1 |= (RF1_ESCORT);
	if (r_ptr->flags1 & (RF1_ESCORTS))
		flags1 |= (RF1_ESCORTS);

	/* Killing a monster reveals some properties */
	if (l_ptr->tkills)
	{
		/* Know "race" flags */
		if (r_ptr->flags3 & (RF3_ORC))
			flags3 |= (RF3_ORC);
		if (r_ptr->flags3 & (RF3_TROLL))
			flags3 |= (RF3_TROLL);
		if (r_ptr->flags3 & (RF3_GIANT))
			flags3 |= (RF3_GIANT);
		if (r_ptr->flags3 & (RF3_DRAGON))
			flags3 |= (RF3_DRAGON);
		if (r_ptr->flags3 & (RF3_DEMON))
			flags3 |= (RF3_DEMON);
		if (r_ptr->flags3 & (RF3_UNDEAD))
			flags3 |= (RF3_UNDEAD);
		if (r_ptr->flags3 & (RF3_EVIL))
			flags3 |= (RF3_EVIL);
		if (r_ptr->flags3 & (RF3_ANIMAL))
			flags3 |= (RF3_ANIMAL);

		/* Know "forced" flags */
		if (r_ptr->flags1 & (RF1_FORCE_DEPTH))
			flags1 |= (RF1_FORCE_DEPTH);
		if (r_ptr->flags1 & (RF1_FORCE_MAXHP))
			flags1 |= (RF1_FORCE_MAXHP);
	}

	/* Define a convenience variable */
	if (r_ptr->flags1 & (RF1_UNIQUE))
		unique = TRUE;

	/* Descriptions */
	if (show_details)
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
			for (i = r_idx + 1; i < MAX_R_IDX; i++)
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
			if (len > 2048)
				len = 2048;

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
		c_roff(TERM_L_BLUE, buf, 0, 0);
		roff("  ", 0, 0);
	}

	/* Player ghosts may have unique descriptions. */
	if ((r_ptr->flags2 & (RF2_PLAYER_GHOST)) && (ghost_string_type == 2))
		c_roff(TERM_L_BLUE, format("%s  ", ghost_string), 0, 0);


	/* Notice "Quest" monsters */
	if (flags1 & (RF1_QUESTOR))
	{
		c_roff(TERM_VIOLET,
			"You feel an intense desire to kill this monster...  ", 0, 0);
	}

	/* Something was written */
	if (s_len)
	{
		/* Hack -- trim spaces */
		while (s_buffer[s_len - 1] == ' ') s_len--;
	
		/* New paragraph. */
		roff("\n\n", 0, 0);
	}

	/* Require a flag to show kills */
	if (!(show_details))
	{
		/* nothing */
	}

	/* Treat uniques differently */
	else if (unique)
	{
		/* Hack -- Determine if the unique is "dead" */
		bool dead = (r_ptr->max_num == 0) ? TRUE : FALSE;

		/* We've been killed... */
		if (l_ptr->deaths)
		{
			/* Killed ancestors */
			roff(format("%^s has slain %d of your ancestors", wd_he[msex],
					l_ptr->deaths), 0, 0);

			/* But we've also killed it */
			if (dead)
			{
				roff(format(", but you have avenged %s!  ",
						plural(l_ptr->deaths, "him", "them")), 0, 0);
			}

			/* Unavenged (ever) */
			else
			{
				roff(format(", who %s unavenged.  ", plural(l_ptr->deaths,
							"remains", "remain")), 0, 0);
			}
		}

		/* Dead unique who never hurt us */
		else if (dead)
		{
			roff("You have slain this foe.  ", 0, 0);
		}

	}

	/* Not unique, but killed us */
	else if (l_ptr->deaths)
	{
		/* Dead ancestors */
		roff(format
			("%d of your ancestors %s been killed by this creature, ",
				l_ptr->deaths, plural(l_ptr->deaths, "has", "have")), 0,
			0);

		/* Some kills this life */
		if (l_ptr->pkills)
		{
			roff(format
				("and you have exterminated at least %d of the creatures.  ",
					l_ptr->pkills), 0, 0);
		}

		/* Some kills past lives */
		else if (l_ptr->tkills)
		{
			roff(format
				("and %s have exterminated at least %d of the creatures.  ",
					"your ancestors", l_ptr->tkills), 0, 0);
		}

		/* No kills */
		else
		{
			roff(format
				("and %s is not ever known to have been defeated.  ",
			 wd_he[msex]), 0, 0);
		}
	}

	/* Normal monsters */
	else
	{
		/* Killed some this life */
		if (l_ptr->pkills)
		{
			roff(format
				("You have killed at least %d of these creatures.  ",
			 l_ptr->pkills), 0, 0);
		}

		/* Killed some last life */
		else if (l_ptr->tkills)
		{
			roff(format
				("Your ancestors have killed at least %d of these creatures.  ",
					l_ptr->tkills), 0, 0);
		}

		/* Killed none */
		else
		{
			roff("No battles to the death are recalled.  ", 0, 0);
		}
	}

	/* Nothing yet */
	old = FALSE;

	/* Describe location */
	if (r_ptr->level == 0)
	{
		roff(format("%^s lives in the town", wd_he[msex]), 0, 0);
		old = TRUE;
	}
	else if (l_ptr->tkills)
	{
		char depth_desc[80];

		if ((depth_in_feet) && (use_metric))
		{
			strcpy(depth_desc, format("at depths of %d meters",
					r_ptr->level * 15));
		}
		else if (depth_in_feet)
		{
			strcpy(depth_desc, format("at depths of %d feet",
					r_ptr->level * 50));
		}
		else
		{
			strcpy(depth_desc, format("on dungeon level %d",
					r_ptr->level));
		}


		/* Build the description of rarity and location. */
		if (r_ptr->flags1 & (RF1_QUESTOR))
		{
			/* Questor monsters are fixed-depth, and always appear. */
			roff(format("%^s is always found %s", wd_he[msex], depth_desc),
				0, 0);
		}

		else if (r_ptr->flags1 & (RF1_FORCE_DEPTH))
		{
			roff(format("%^s is %s, always found %s", wd_he[msex],
					wd_rarity(r_ptr->rarity, unique), depth_desc), 0, 0);
		}
		else
		{
			roff(format("%^s is %s, normally found %s", wd_he[msex],
					wd_rarity(r_ptr->rarity, unique), depth_desc), 0, 0);
		}

		old = TRUE;
	}



	/* Describe movement */
	if (TRUE)
	{
		/* Introduction */
		if (old)
		{
			roff(", and ", 0, 0);
		}
		else
		{
			roff(format("%^s ", wd_he[msex]), 0, 0);
			old = TRUE;
		}
		if (flags1 & (RF1_NEVER_MOVE))
			roff("acts", 0, 0);
		else
			roff("moves", 0, 0);

		/* Random-ness */
		if ((flags1 & (RF1_RAND_50)) || (flags1 & (RF1_RAND_25)))
		{
			/* Adverb */
			if ((flags1 & (RF1_RAND_50)) && (flags1 & (RF1_RAND_25)))
			{
				roff(" extremely", 0, 0);
			}
			else if (flags1 & (RF1_RAND_50))
			{
				roff(" somewhat", 0, 0);
			}
			else if (flags1 & (RF1_RAND_25))
			{
				roff(" a bit", 0, 0);
			}

			/* Adjective */
			roff(" erratically", 0, 0);

			/* Hack -- Occasional conjunction */
			if (r_ptr->speed != 110)
				roff(", and", 0, 0);
		}

		/* Speed */
		if (r_ptr->speed > 110)
		{
			if (r_ptr->speed > 130)
				roff(" incredibly", 0, 0);
			else if (r_ptr->speed > 120)
				roff(" very", 0, 0);
			else if (r_ptr->speed < 116)
				roff(" fairly", 0, 0);
			roff(" quickly", 0, 0);
		}
		else if (r_ptr->speed < 110)
		{
			if (r_ptr->speed < 90)
				roff(" extremely", 0, 0);
			else if (r_ptr->speed < 100)
				roff(" very", 0, 0);
			roff(" slowly", 0, 0);
		}
		else
		{
			roff(" at normal speed", 0, 0);
		}
	}

	/* The code above includes "attack speed" */
	if (flags1 & (RF1_NEVER_MOVE))
	{
		/* Introduce */
		if (old)
		{
			roff(", but ", 0, 0);
		}
		else
		{
			roff(format("%^s ", wd_he[msex]), 0, 0);
			old = TRUE;
		}

		/* Describe */
		roff("does not deign to chase intruders", 0, 0);
	}

	/* End this sentence */
	if (old)
	{
		roff(".  ", 0, 0);
		old = FALSE;
	}


	/* Describe experience if known */
	if (l_ptr->tkills)
	{
		/* Introduction */
		if (flags1 & (RF1_UNIQUE))
		{
			roff("Killing this", 0, 0);
		}
		else
		{
			roff("A kill of this", 0, 0);
		}

		/* Describe the "quality" */
		if (flags3 & (RF3_ANIMAL))
			roff(" natural", 0, 0);
		if (flags3 & (RF3_EVIL))
			roff(" evil", 0, 0);
		if (flags3 & (RF3_UNDEAD))
			roff(" undead", 0, 0);

		/* Describe the "race" */
		if (flags3 & (RF3_DRAGON))
			roff(" dragon", 0, 0);
		else if (flags3 & (RF3_DEMON))
			roff(" demon", 0, 0);
		else if (flags3 & (RF3_GIANT))
			roff(" giant", 0, 0);
		else if (flags3 & (RF3_TROLL))
			roff(" troll", 0, 0);
		else if (flags3 & (RF3_ORC))
			roff(" orc", 0, 0);
		else
			roff(" creature", 0, 0);

		/* Group some variables */
		if (TRUE)
		{
			long i, j;

			/* calculate the integer exp part */
			i = (long) r_ptr->mexp * r_ptr->level / p_ptr->lev;

			/* calculate the fractional exp part scaled by 100, */
			/* must use long arithmetic to avoid overflow  */
			j =
				((((long) r_ptr->mexp * r_ptr->level % p_ptr->lev) *
					(long) 1000 / p_ptr->lev + 5) / 10);

			/* Mention the experience */
			roff(format(" is worth %ld.%02ld point%s", (long) i, (long) j,
					(((i == 1) && (j == 0)) ? "" : "s")), 0, 0);

			/* Take account of annoying English */
			p = "th";
			i = p_ptr->lev % 10;
			if ((p_ptr->lev / 10) == 1) /* nothing */ ;
			else if (i == 1)
				p = "st";
			else if (i == 2)
				p = "nd";
			else if (i == 3)
				p = "rd";

			/* Take account of "leading vowels" in numbers */
			q = "";
			i = p_ptr->lev;
			if ((i == 8) || (i == 11) || (i == 18))
				q = "n";

			/* Mention the dependance on the player's level */
			roff(format(" for a%s %lu%s level character.  ", q, (long) i,
					p), 0, 0);
		}
	}
	/* If no kills, known racial information should still be displayed. */
	else if ((flags3 & (RF3_ANIMAL)) || (flags3 & (RF3_EVIL)) ||
		(flags3 & (RF3_UNDEAD)) || (flags3 & (RF3_DRAGON)) ||
		(flags3 & (RF3_DEMON)) || (flags3 & (RF3_GIANT)) ||
		(flags3 & (RF3_TROLL)) || (flags3 & (RF3_ORC)))
	{
		bool add = FALSE;

		/* Pronoun. */
		if (flags1 & (RF1_UNIQUE))
			roff(format("%^s is a", wd_he[msex]), 0, 0);
		else
			roff("It is a", 0, 0);

		/* Describe the "quality" */
		if (flags3 & (RF3_ANIMAL))
		{
			roff(" natural", 0, 0);
			add = TRUE;
		}
		if (flags3 & (RF3_EVIL))
		{
			if (add)
				roff(" evil", 0, 0);
			else
				roff("n evil", 0, 0);
			add = TRUE;
		}
		if (flags3 & (RF3_UNDEAD))
		{
			if (add)
				roff(" undead", 0, 0);
			else
				roff("n undead", 0, 0);
			add = TRUE;
		}

		/* Describe the "race" */
		if (flags3 & (RF3_DRAGON))
			roff(" dragon", 0, 0);
		else if (flags3 & (RF3_DEMON))
			roff(" demon", 0, 0);
		else if (flags3 & (RF3_GIANT))
			roff(" giant", 0, 0);
		else if (flags3 & (RF3_TROLL))
			roff(" troll", 0, 0);
		else if (flags3 & (RF3_ORC))
		{
			if (add)
				roff(" orc", 0, 0);
			else
				roff("n orc", 0, 0);
		}
		else
			roff(" creature", 0, 0);

		/* End sentence. */
		roff(".  ", 0, 0);
	}

	/* Describe escorts */
	if ((flags1 & (RF1_ESCORT)) || (flags1 & (RF1_ESCORTS)))
	{
		roff(format("%^s usually appears with escorts.  ", wd_he[msex]), 0,
			0);
	}

	/* Describe friends */
	else if ((flags1 & (RF1_FRIEND)) || (flags1 & (RF1_FRIENDS)))
	{
		roff(format("%^s usually appears in groups.  ", wd_he[msex]), 0,
			0);
	}

	/* Get spell power */
	spower = r_ptr->spell_power;


	/* Collect inate attacks */
	vn = 0;
	if (flags4 & (RF4_SHRIEK))
		vp[vn++] = "shriek for help";
	if (flags4 & (RF4_LASH))
	{
		if (flags3 & (RF3_ANIMAL) || (r_ptr->blow[0].effect == RBE_ACID))
			vp[vn++] = "spit at you from a distance";
		else
			vp[vn++] = "lash you if nearby";
	}
	if (flags4 & (RF4_BOULDER))
	{
		if (spower < 10)
			vp[vn++] = "throw rocks";
		else
			vp[vn++] = "throw boulders";
	}
	if (flags4 & (RF4_SHOT))
	{
		if (spower < 5)
			vp[vn++] = "sling pebbles";
		else if (spower < 15)
			vp[vn++] = "sling leaden pellets";
		else
			vp[vn++] = "sling seeker shot";
	}
	if (flags4 & (RF4_ARROW))
	{
		if (spower < 8)
			vp[vn++] = "shoot little arrows";
		else if (spower < 15)
			vp[vn++] = "shoot arrows";
		else
			vp[vn++] = "shoot seeker arrows";
	}
	if (flags4 & (RF4_BOLT))
	{
		if (spower < 8)
			vp[vn++] = "fire bolts";
		else if (spower < 15)
			vp[vn++] = "fire crossbow quarrels";
		else
			vp[vn++] = "fire seeker bolts";
	}
	if (flags4 & (RF4_MISSL))
	{
		if (spower < 8)
			vp[vn++] = "fire little missiles";
		else if (spower < 15)
			vp[vn++] = "fire missiles";
		else
			vp[vn++] = "fire heavy missiles";
	}
	if (flags4 & (RF4_PMISSL))
	{
		if (flags2 & (RF2_MORGUL_MAGIC))
			vp[vn++] = "hurl black darts";
		else
			vp[vn++] = "whip poisoned darts";
	}

	/* Describe inate attacks */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]), 0, 0);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0)
			{
				roff(" may ", 0, 0);
				if (flags2 & (RF2_ARCHER))
					roff("frequently ", 0, 0);
			}
			else if (n < vn - 1)
				roff(", ", 0, 0);
			else if (n == 1)
				roff(" or ", 0, 0);
			else
				roff(", or ", 0, 0);

			/* Dump */
			roff(vp[n], 0, 0);
		}

		/* End */
		roff(".  ", 0, 0);
	}

	/* Collect breaths */
	vn = 0;
	if (flags4 & (RF4_BRTH_ACID))
		vp[vn++] = "acid";
	if (flags4 & (RF4_BRTH_ELEC))
		vp[vn++] = "lightning";
	if (flags4 & (RF4_BRTH_FIRE))
		vp[vn++] = "fire";
	if (flags4 & (RF4_BRTH_COLD))
		vp[vn++] = "frost";
	if (flags4 & (RF4_BRTH_POIS))
		vp[vn++] = "poison";
	if (flags4 & (RF4_BRTH_PLAS))
		vp[vn++] = "plasma";

	if (flags4 & (RF4_BRTH_LITE))
		vp[vn++] = "light";
	if (flags4 & (RF4_BRTH_DARK))
	{
		if (flags2 & (RF2_MORGUL_MAGIC))
			vp[vn++] = "Night";
		else
			vp[vn++] = "darkness";
	}
	if (flags4 & (RF4_BRTH_CONFU))
		vp[vn++] = "confusion";
	if (flags4 & (RF4_BRTH_SOUND))
		vp[vn++] = "sound";
	if (flags4 & (RF4_BRTH_SHARD))
		vp[vn++] = "shards";
	if (flags4 & (RF4_BRTH_INER))
		vp[vn++] = "inertia";
	if (flags4 & (RF4_BRTH_GRAV))
		vp[vn++] = "gravity";
	if (flags4 & (RF4_BRTH_FORCE))
		vp[vn++] = "force";

	if (flags4 & (RF4_BRTH_NEXUS))
		vp[vn++] = "nexus";
	if (flags4 & (RF4_BRTH_NETHR))
		vp[vn++] = "nether";
	if (flags4 & (RF4_BRTH_CHAOS))
		vp[vn++] = "chaos";
	if (flags4 & (RF4_BRTH_DISEN))
		vp[vn++] = "disenchantment";
	if (flags4 & (RF4_BRTH_TIME))
		vp[vn++] = "time";

	if (flags4 & (RF4_XXX2))
		vp[vn++] = "something";
	if (flags4 & (RF4_XXX3))
		vp[vn++] = "something";
	if (flags4 & (RF4_XXX4))
		vp[vn++] = "something";
	if (flags4 & (RF4_XXX5))
		vp[vn++] = "something";
	if (flags4 & (RF4_XXX6))
		vp[vn++] = "something";

	/* Describe breaths */
	if (vn)
	{
		/* Note breath */
		breath = TRUE;

		/* Intro */
		roff(format("%^s", wd_he[msex]), 0, 0);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0)
				roff(" may breathe ", 0, 0);
			else if (n < vn - 1)
				roff(", ", 0, 0);
			else if (n == 1)
				roff(" or ", 0, 0);
			else
				roff(", or ", 0, 0);

			/* Dump */
			roff(vp[n], 0, 0);
		}
		if (flags2 & (RF2_POWERFUL))
			roff(" powerfully", 0, 0);
	}


	/* Collect spells */
	vn = 0;
	if (flags5 & (RF5_BALL_ACID))
	{
		if (spower < 70)
			vp[vn++] = "produce acid balls";
		else
			vp[vn++] = "produce acid storms";
	}
	if (flags5 & (RF5_BALL_ELEC))
	{
		if (spower < 70)
			vp[vn++] = "produce lightning balls";
		else
			vp[vn++] = "produce lightning storms";
	}
	if (flags5 & (RF5_BALL_FIRE))
	{
		if (flags2 & (RF2_UDUN_MAGIC))
		{
			if (spower < 70)
				vp[vn++] = "produce balls of hellfire";
			else if (spower < 110)
				vp[vn++] = "invoke storms of Udun-fire";
			else
				vp[vn++] = "call upon the fires of Udun";
		}
		else
		{
			if (spower < 70)
				vp[vn++] = "produce fire balls";
			else
				vp[vn++] = "produce fire storms";
		}
	}
	if (flags5 & (RF5_BALL_COLD))
	{
		if (flags2 & (RF2_MORGUL_MAGIC))
		{
			if (spower < 70)
				vp[vn++] = "produce spheres of deadly cold";
			else
				vp[vn++] = "invoke storms of deadly cold";
		}
		else
		{
			if (spower < 70)
				vp[vn++] = "produce frost balls";
			else
				vp[vn++] = "produce frost storms";
		}
	}
	if (flags5 & (RF5_BALL_POIS))
	{
		if (flags2 & (RF2_MORGUL_MAGIC))
		{
			if (spower < 15)
				vp[vn++] = "produce clouds of venom";
			else if (spower < 70)
				vp[vn++] = "produce venomous balls";
			else
				vp[vn++] = "raise storms of venom";
		}
		else
		{
			if (spower < 15)
				vp[vn++] = "produce stinking clouds";
			else if (spower < 70)
				vp[vn++] = "produce poison balls";
			else
				vp[vn++] = "produce storms of poison";
		}
	}
	if (flags5 & (RF5_BALL_LITE))
	{
		if (spower < 15)
			vp[vn++] = "produce spheres of light";
		else if (spower < 70)
			vp[vn++] = "produce explosions of light";
		else
			vp[vn++] = "invoke starbursts";
	}
	if (flags5 & (RF5_BALL_DARK))
	{
		if (flags2 & (RF2_MORGUL_MAGIC))
		{
			if (spower < 70)
				vp[vn++] = "produce spheres of Night";
			else
				vp[vn++] = "conjure up storms of Night";
		}
		else
		{
			if (spower < 70)
				vp[vn++] = "produce balls of darkness";
			else
				vp[vn++] = "produce storms of darkness";
		}
	}
	if (flags5 & (RF5_BALL_CONFU))
	{
		if (spower < 70)
			vp[vn++] = "produce confusion balls";
		else
			vp[vn++] = "produce storms of confusion";
	}
	if (flags5 & (RF5_BALL_SOUND))
	{
		if (spower < 15)
			vp[vn++] = "produce explosions of sound";
		else if (spower < 70)
			vp[vn++] = "produce thunderclaps";
		else
			vp[vn++] = "unleash storms of sound";
	}
	if (flags5 & (RF5_BALL_SHARD))
	{
		if (spower < 15)
			vp[vn++] = "produce blasts of shards";
		else if (spower < 90)
			vp[vn++] = "produce whirlwinds of shards";
		else
			vp[vn++] = "call up shardstorms";
	}
	if (flags5 & (RF5_BALL_STORM))
	{
		if (spower < 30)
			vp[vn++] = "produce little storms";
		else if (spower < 70)
			vp[vn++] = "produce whirlpools";
		else
			vp[vn++] = "call up raging storms";
	}
	if (flags5 & (RF5_BALL_NETHR))
	{
		if (spower < 30)
			vp[vn++] = "produce nether orbs";
		else if (spower < 70)
			vp[vn++] = "produce nether balls";
		else
			vp[vn++] = "invoke nether storms";
	}
	if (flags5 & (RF5_BALL_CHAOS))
	{
		if (spower < 20)
			vp[vn++] = "produce spheres of chaos";
		else if (spower < 70)
			vp[vn++] = "produce explosions of chaos";
		else
			vp[vn++] = "call up maelstroms of raw chaos";
	}
	if (flags5 & (RF5_BALL_MANA))
	{
		if (spower < 40)
			vp[vn++] = "produce manabursts";
		else if (spower < 90)
			vp[vn++] = "produce balls of mana";
		else
			vp[vn++] = "invoke mana storms";
	}
	if (flags5 & (RF5_BOLT_ACID))
		vp[vn++] = "produce acid bolts";
	if (flags5 & (RF5_BOLT_ELEC))
		vp[vn++] = "produce lightning bolts";
	if (flags5 & (RF5_BOLT_FIRE))
		vp[vn++] = "produce fire bolts";
	if (flags5 & (RF5_BOLT_COLD))
		vp[vn++] = "produce frost bolts";
	if (flags5 & (RF5_BOLT_POIS))
		vp[vn++] = "produce poison bolts";
	if (flags5 & (RF5_BOLT_PLAS))
		vp[vn++] = "produce plasma bolts";
	if (flags5 & (RF5_BOLT_ICE))
		vp[vn++] = "produce ice bolts";
	if (flags5 & (RF5_BOLT_WATER))
		vp[vn++] = "produce water bolts";
	if (flags5 & (RF5_BOLT_NETHR))
		vp[vn++] = "produce nether bolts";
	if (flags5 & (RF5_BOLT_MANA))
		vp[vn++] = "produce mana bolts";
	if (flags5 & (RF5_BEAM_ELEC))
		vp[vn++] = "shoot sparks of lightning";
	if (flags5 & (RF5_BEAM_ICE))
		vp[vn++] = "cast lances of ice";
	if (flags5 & (RF5_BEAM_NETHR))
	{
		if (spower < 40)
			vp[vn++] = "cast beams of nether";
		else if (spower < 90)
			vp[vn++] = "hurl lances of nether";
		else
			vp[vn++] = "shoot rays of death";
	}
	if (flags5 & (RF5_ARC__HFIRE))
	{
		if (flags2 & (RF2_UDUN_MAGIC))
		{
			if (spower < 50)
				vp[vn++] = "produce columns of hellfire";
			else if (spower < 100)
				vp[vn++] = "envelop you in hellfire";
			else
				vp[vn++] = "breath like the Balrog";
		}
		else
		{
			if (spower < 50)
				vp[vn++] = "produce columns of fire";
			else if (spower < 100)
				vp[vn++] = "envelop you in fire";
			else
				vp[vn++] = "envelop you in flamestrikes";
		}
	}
	if (flags5 & (RF5_ARC__FORCE))
	{
		if (spower < 50)
			vp[vn++] = "thrust you away";
		else if (spower < 100)
			vp[vn++] = "hurl you away";
		else
			vp[vn++] = "snatch you up, and throws you away";
	}
	if (flags6 & (RF6_HASTE))
		vp[vn++] = "haste-self";
	if (flags6 & (RF6_ADD_MANA))
		vp[vn++] = "restore mana";
	if (flags6 & (RF6_HEAL))
		vp[vn++] = "heal-self";
	if (flags6 & (RF6_CURE))
		vp[vn++] = "cure what ails it";
	if (flags6 & (RF6_BLINK))
		vp[vn++] = "blink-self";
	if (flags6 & (RF6_TPORT))
		vp[vn++] = "teleport-self";
	if (flags6 & (RF6_TELE_TO))
		vp[vn++] = "teleport to";
	if (flags6 & (RF6_TELE_AWAY))
		vp[vn++] = "teleport away";
	if (flags6 & (RF6_TELE_LEVEL))
		vp[vn++] = "teleport level";
	if (flags6 & (RF6_DARKNESS))
		vp[vn++] = "create darkness";
	if (flags6 & (RF6_TRAPS))
		vp[vn++] = "create traps";
	if (flags6 & (RF6_FORGET))
		vp[vn++] = "cause amnesia";
	if (flags6 & (RF6_DRAIN_MANA))
		vp[vn++] = "drain mana";
	if (flags6 & (RF6_MIND_BLAST))
		vp[vn++] = "cause mind blasting";
	if (flags6 & (RF6_BRAIN_SMASH))
		vp[vn++] = "cause brain smashing";
	if (flags6 & (RF6_WOUND))
	{
		if (spower < 7)
			vp[vn++] = "cause light wounds";
		else if (spower < 15)
			vp[vn++] = "cause medium wounds";
		else if (spower < 30)
			vp[vn++] = "cause serious wounds";
		else if (spower < 50)
			vp[vn++] = "cause critical wounds";
		else
			vp[vn++] = "cause mortal wounds";
	}
	if (flags6 & (RF6_SCARE))
		vp[vn++] = "terrify";
	if (flags6 & (RF6_BLIND))
		vp[vn++] = "blind";
	if (flags6 & (RF6_CONF))
		vp[vn++] = "confuse";
	if (flags6 & (RF6_SLOW))
		vp[vn++] = "slow";
	if (flags6 & (RF6_HOLD))
		vp[vn++] = "paralyze";

	m = 0;

	/* Summons are described somewhat differently. */
	if (flags7)
	{
		/* Save current spell. */
		m = vn;

		/* Summons */
		if (flags7 & (RF7_S_KIN))
		{
			if (r_ptr->flags1 & (RF1_UNIQUE))
				vp[vn++] = "its minions";
			else
				vp[vn++] = "similar monsters";
		}
		if (flags7 & (RF7_S_MONSTER))
			vp[vn++] = "a monster";
		if (flags7 & (RF7_S_MONSTERS))
			vp[vn++] = "monsters";
		if (flags7 & (RF7_S_ANT))
			vp[vn++] = "ants";
		if (flags7 & (RF7_S_SPIDER))
			vp[vn++] = "spiders";
		if (flags7 & (RF7_S_HOUND))
			vp[vn++] = "hounds";
		if (flags7 & (RF7_S_ANIMAL))
			vp[vn++] = "natural creatures";
		if (flags7 & (RF7_S_THIEF))
			vp[vn++] = "thieves";
		if (flags7 & (RF7_S_BERTBILLTOM))
			vp[vn++] = "his friends";
		if (flags7 & (RF7_S_DRAGON))
			vp[vn++] = "a dragon";
		if (flags7 & (RF7_S_HI_DRAGON))
			vp[vn++] = "Ancient Dragons";
		if (flags7 & (RF7_S_DEMON))
			vp[vn++] = "a demon";
		if (flags7 & (RF7_S_HI_DEMON))
			vp[vn++] = "Greater Demons";
		if (flags7 & (RF7_S_UNDEAD))
			vp[vn++] = "an undead";
		if (flags7 & (RF7_S_HI_UNDEAD))
			vp[vn++] = "Greater Undead";
		if (flags7 & (RF7_S_WRAITH))
			vp[vn++] = "the Ringwraiths";
		if (flags7 & (RF7_S_UNIQUE))
			vp[vn++] = "Unique Monsters";
	}

	/* Describe spells */
	if (vn)
	{
		/* Note magic */
		magic = TRUE;

		/* Intro */
		if (breath)
		{
			roff(", and is also", 0, 0);
		}
		else
		{
			roff(format("%^s is", wd_he[msex]), 0, 0);
		}


		/* Verb Phrase */
		roff(" magical, casting", 0, 0);

		/* Describe magic */
		if ((flags2 & (RF2_UDUN_MAGIC)) && (flags2 & (RF2_MORGUL_MAGIC)))
			roff(" perilous spells of Udun and of Morgul", 0, 0);
		else if (flags2 & (RF2_MORGUL_MAGIC))
			roff(" Morgul-spells", 0, 0);
		else if (flags2 & (RF2_UDUN_MAGIC))
			roff(" spells of Udun", 0, 0);

		else
			roff(" spells", 0, 0);

		/* Adverb */
		if (flags2 & (RF2_SMART))
		{
			/* Suppress text if monster has both Udun and Morgul-magic. */
			if (!((flags2 & (RF2_UDUN_MAGIC)) &&
					(flags2 & (RF2_MORGUL_MAGIC))))
			{
				roff(" intelligently", 0, 0);
			}
		}

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if ((m) && (n == m))
			{
				if (m > 1)
					roff(",", 0, 0);
				roff(" or summon ", 0, 0);
			}
			else if (n == 0)
				roff(" which ", 0, 0);
			else if (n < vn - 1)
				roff(", ", 0, 0);
			else if (n == 1)
				roff(" or ", 0, 0);
			else
			{
				if ((m) && (m == vn - 2))
					roff(" or ", 0, 0);
				else
					roff(", or ", 0, 0);
			}

			/* Dump */
			roff(vp[n], 0, 0);
		}
	}

	/* End the sentence about inate/other spells */
	if (breath || magic)
	{
		/* Total casting */
		m = l_ptr->cast_inate + l_ptr->cast_spell;

		/* Average frequency */
		n = (r_ptr->freq_inate + r_ptr->freq_spell) / 2;

		/* Describe the spell frequency */
		if (m > 100)
		{
			roff(format("; 1 time in %d", 100 / n), 0, 0);
		}

		/* Guess at the frequency */
		else if (m)
		{
			n = ((n + 9) / 10) * 10;
			roff(format("; about 1 time in %d", 100 / n), 0, 0);
		}

		/* Describe monster mana */
		if (r_ptr->mana && know_mana(r_idx))
		{
			/* Mana */
			roff(format(" with a mana rating of %d", r_ptr->mana), 0, 0);

		}

		/* End this sentence */
		roff(".  ", 0, 0);
	}


	/* Describe monster "toughness" */
	if (know_armour(r_idx))
	{
		/* Armor */
		roff(format("%^s has an armour rating of %d", wd_he[msex],
				r_ptr->ac), 0, 0);

		/* Maximized hitpoints */
		if (flags1 & (RF1_FORCE_MAXHP))
		{
			roff(format(" and a life rating of %d.  ",
					r_ptr->hdice * r_ptr->hside), 0, 0);
		}

		/* Variable hitpoints */
		else
		{
			roff(format(" and an average life rating of %d.  ",
					(r_ptr->hdice * (r_ptr->hside + 1)) / 2), 0, 0);
		}
	}


	/* Collect special abilities. */
	vn = 0;
	if (flags2 & (RF2_OPEN_DOOR))
		vp[vn++] = "open doors";
	if (flags2 & (RF2_BASH_DOOR))
		vp[vn++] = "bash down doors";
	if (flags2 & (RF2_PASS_WALL))
		vp[vn++] = "pass through walls";
	if (flags2 & (RF2_KILL_WALL))
		vp[vn++] = "bore through walls";
	if (flags2 & (RF2_MOVE_BODY))
		vp[vn++] = "push past other monsters";
	if (flags2 & (RF2_KILL_BODY))
		vp[vn++] = "destroy weaker monsters";
	if (flags2 & (RF2_TAKE_ITEM))
		vp[vn++] = "pick up objects";
	if (flags2 & (RF2_KILL_ITEM))
		vp[vn++] = "destroy objects";

	/* Describe special abilities. */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]), 0, 0);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0)
				roff(" can ", 0, 0);
			else if (n < vn - 1)
				roff(", ", 0, 0);
			else if (n == 1)
				roff(" and ", 0, 0);
			else
				roff(", and ", 0, 0);

			/* Dump */
			roff(vp[n], 0, 0);
		}

		/* End */
		roff(".  ", 0, 0);
	}


	/* Describe special abilities. */
	if (flags2 & (RF2_INVISIBLE))
	{
		roff(format("%^s is invisible.  ", wd_he[msex]), 0, 0);
	}
	if (flags2 & (RF2_COLD_BLOOD))
	{
		roff(format("%^s is cold blooded.  ", wd_he[msex]), 0, 0);
	}
	if (flags2 & (RF2_EMPTY_MIND))
	{
		roff(format("%^s is not detected by telepathy.  ", wd_he[msex]), 0,
			0);
	}
	if (flags2 & (RF2_WEIRD_MIND))
	{
		roff(format("%^s is rarely detected by telepathy.  ", wd_he[msex]),
			0, 0);
	}
	if (flags2 & (RF2_MULTIPLY))
	{
		roff(format("%^s breeds explosively.  ", wd_he[msex]), 0, 0);
	}
	if (flags2 & (RF2_REGENERATE))
	{
		roff(format("%^s regenerates quickly.  ", wd_he[msex]), 0, 0);
	}


	/* Collect susceptibilities */
	vn = 0;
	if (flags3 & (RF3_HURT_ROCK))
		vp[vn++] = "rock remover";
	if (flags3 & (RF3_HURT_LITE))
		vp[vn++] = "bright light";
	if (flags3 & (RF3_HURT_FIRE))
		vp[vn++] = "fire";
	if (flags3 & (RF3_HURT_COLD))
		vp[vn++] = "cold";

	/* Describe susceptibilities */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]), 0, 0);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0)
				roff(" is hurt by ", 0, 0);
			else if (n < vn - 1)
				roff(", ", 0, 0);
			else if (n == 1)
				roff(" and ", 0, 0);
			else
				roff(", and ", 0, 0);

			/* Dump */
			roff(vp[n], 0, 0);
		}

		/* End */
		roff(".  ", 0, 0);
	}


	/* Collect immunities */
	vn = 0;
	if (flags3 & (RF3_IM_ACID))
		vp[vn++] = "acid";
	if (flags3 & (RF3_IM_ELEC))
		vp[vn++] = "lightning";
	if (flags3 & (RF3_IM_FIRE))
		vp[vn++] = "fire";
	if (flags3 & (RF3_IM_COLD))
		vp[vn++] = "cold";
	if (flags3 & (RF3_IM_POIS))
		vp[vn++] = "poison";

	/* Describe immunities */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]), 0, 0);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0)
				roff(" resists ", 0, 0);
			else if (n < vn - 1)
				roff(", ", 0, 0);
			else if (n == 1)
				roff(" and ", 0, 0);
			else
				roff(", and ", 0, 0);

			/* Dump */
			roff(vp[n], 0, 0);
		}

		/* End */
		roff(".  ", 0, 0);
	}


	/* Collect resistances */
	/* Neatness: mention some borderline high-level resistances only if others have already been. */
	vn = 0;
	if (flags4 & (RF4_BRTH_LITE))
		vp[vn++] = "light";
	if ((flags4 & (RF4_BRTH_DARK)) || (flags2 & (RF2_MORGUL_MAGIC)) ||
		(flags3 & (RF3_ORC)))
		vp[vn++] = "darkness";

	if (flags4 & (RF4_BRTH_CONFU))
		vp[vn++] = "confusion";
	if (flags4 & (RF4_BRTH_SOUND))
		vp[vn++] = "sound";
	if (flags4 & (RF4_BRTH_SHARD))
		vp[vn++] = "shards";
	if (flags4 & (RF4_BRTH_INER))
		vp[vn++] = "inertia";
	if (flags4 & (RF4_BRTH_GRAV))
		vp[vn++] = "gravity";
	if (flags4 & (RF4_BRTH_FORCE))
		vp[vn++] = "force";
	if ((flags3 & (RF3_RES_WATE)) || (prefix(name, "Water")))
		vp[vn++] = "water";

	if ((flags4 & (RF4_BRTH_PLAS)) || (flags3 & (RF3_RES_PLAS)) || ((vn) &&
			((flags3 & (RF3_IM_ELEC)) || (flags3 & (RF3_IM_FIRE)))) ||
		prefix(name, "Plasma"))
		vp[vn++] = "plasma";

	if ((flags3 & (RF3_RES_NEXU)) || prefix(name, "Nexus") ||
		(flags4 & (RF4_BRTH_NEXUS))) vp[vn++] = "nexus";
	if ((flags3 & (RF3_UNDEAD)) || (flags3 & (RF3_RES_NETH)) ||
		(flags4 & (RF4_BRTH_NETHR))) vp[vn++] = "nether";
	if ((flags3 & (RF3_RES_DISE)) || (flags4 & (RF4_BRTH_DISEN)) ||
		prefix(name, "Disen"))
		vp[vn++] = "disenchantment";
	if (flags4 & (RF4_BRTH_TIME))
		vp[vn++] = "time";


	/* Describe resistances */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]), 0, 0);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0)
				roff(" resists ", 0, 0);
			else if (n < vn - 1)
				roff(", ", 0, 0);
			else if (n == 1)
				roff(" and ", 0, 0);
			else
				roff(", and ", 0, 0);

			/* Dump */
			roff(vp[n], 0, 0);
		}

		/* End */
		roff(".  ", 0, 0);
	}


	/* Collect non-effects */
	vn = 0;
	if ((flags3 & (RF3_NO_STUN)) || (flags4 & (RF4_BRTH_SOUND)) ||
		(flags4 & (RF4_BRTH_FORCE)))
		vp[vn++] = "stunned";
	if (flags3 & (RF3_NO_FEAR))
		vp[vn++] = "frightened";
	if ((flags3 & (RF3_NO_CONF)) || (flags4 & (RF4_BRTH_CONFU)) ||
		(flags4 & (RF4_BRTH_CHAOS)))
		vp[vn++] = "confused";
	if (flags3 & (RF3_NO_SLEEP))
		vp[vn++] = "slept";

	/* Describe non-effects */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]), 0, 0);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0)
				roff(" cannot be ", 0, 0);
			else if (n < vn - 1)
				roff(", ", 0, 0);
			else if (n == 1)
				roff(" or ", 0, 0);
			else
				roff(", or ", 0, 0);

			/* Dump */
			roff(vp[n], 0, 0);
		}

		/* End */
		roff(".  ", 0, 0);
	}


	/* Do we know how aware it is? */
	if ((((int) l_ptr->wake * (int) l_ptr->wake) > r_ptr->sleep) ||
		(l_ptr->ignore == MAX_UCHAR) || ((r_ptr->sleep == 0) &&
			(l_ptr->tkills >= 10)))
	{
		cptr act;

		if (r_ptr->sleep > 150)
		{
			act = "is nearly oblivious of";
		}
		else if (r_ptr->sleep > 95)
		{
			act = "prefers to ignore";
		}
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

		roff(format
			("%^s %s intruders, which %s may notice from %d feet.  ",
		 wd_he[msex], act, wd_he[msex], 10 * r_ptr->aaf), 0, 0);
	}


	/* Drops gold and/or items */
	if (l_ptr->drop_gold || l_ptr->drop_item)
	{
		/* No "n" needed */
		sin = FALSE;

		/* Intro */
		roff(format("%^s may carry", wd_he[msex]), 0, 0);

		/* Count maximum drop */
		n = MAX(l_ptr->drop_gold, l_ptr->drop_item);

		/* One drop (may need an "n") */
		if (n == 1)
		{
			roff(" a", 0, 0);
			sin = TRUE;
		}

		/* Two drops */
		else if (n == 2)
		{
			roff(" one or two", 0, 0);
		}

		/* Many drops */
		else
		{
			roff(format(" up to %d", n), 0, 0);
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
		if (l_ptr->drop_item)
		{
			/* Handle singular "an" */
			if ((sin) && (!(r_ptr->flags1 & (RF1_DROP_CHEST))))
				roff("n", 0, 0);
			sin = FALSE;

			/* Dump "object(s)" */
			if (p)
				roff(p, 0, 0);
			if (r_ptr->flags1 & (RF1_DROP_CHEST))
				roff(" chest", 0, 0);
			else
				roff(" object", 0, 0);
			if (n != 1)
				roff("s", 0, 0);

			/* Conjunction replaces variety, if needed for "gold" below */
			p = " or";
		}

		/* Treasures */
		if (l_ptr->drop_gold)
		{
			/* Cancel prefix */
			if (!p)
				sin = FALSE;

			/* Handle singular "an" */
			if (sin)
				roff("n", 0, 0);
			sin = FALSE;

			/* Dump "treasure(s)" */
			if (p)
				roff(p, 0, 0);
			roff(" treasure", 0, 0);
			if (n != 1)
				roff("s", 0, 0);
		}

		/* End this sentence */
		roff(".  ", 0, 0);
	}


	/* Count the number of "known" attacks */
	for (n = 0, m = 0; m < 4; m++)
	{
		/* Skip non-attacks */
		if (!r_ptr->blow[m].method)
			continue;

		/* Count known attacks */
		if (l_ptr->blows[m])
			n++;
	}

	/* Examine (and count) the actual attacks */
	for (r = 0, m = 0; m < 4; m++)
	{
		int method, effect, d1, d2;

		/* Skip non-attacks */
		if (!r_ptr->blow[m].method)
			continue;

		/* Skip unknown attacks */
		if (!l_ptr->blows[m])
			continue;


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
			case RBM_HIT:
				p = "hit";
				break;
			case RBM_TOUCH:
				p = "touch";
				break;
			case RBM_PUNCH:
				p = "punch";
				break;
			case RBM_KICK:
				p = "kick";
				break;
			case RBM_CLAW:
				p = "claw";
				break;
			case RBM_BITE:
				p = "bite";
				break;
			case RBM_STING:
				p = "sting";
				break;
			case RBM_XXX1:
				break;
			case RBM_BUTT:
				p = "butt";
				break;
			case RBM_CRUSH:
				p = "crush";
				break;
			case RBM_ENGULF:
				p = "engulf";
				break;
			case RBM_XXX2:
				break;
			case RBM_CRAWL:
				p = "crawl on you";
				break;
			case RBM_DROOL:
				p = "drool on you";
				break;
			case RBM_SPIT:
				p = "spit";
				break;
			case RBM_XXX3:
				break;
			case RBM_GAZE:
				p = "gaze";
				break;
			case RBM_WAIL:
				p = "wail";
				break;
			case RBM_SPORE:
				p = "release spores";
				break;
			case RBM_XXX4:
				break;
			case RBM_BEG:
				p = "beg";
				break;
			case RBM_INSULT:
				p = "insult";
				break;
			case RBM_SNEER:
				p = "sneer";
				break;
			case RBM_REQUEST:
				p = "offer to trade";
				break;
		}


		/* Default effect */
		q = NULL;

		/* Acquire the effect */
		switch (effect)
		{
			case RBE_HURT:
				q = "attack";
				break;
			case RBE_POISON:
				q = "poison";
				break;
			case RBE_UN_BONUS:
				q = "disenchant";
				break;
			case RBE_UN_POWER:
				q = "drain charges";
				break;
			case RBE_EAT_GOLD:
				q = "steal gold";
				break;
			case RBE_EAT_ITEM:
				q = "steal items";
				break;
			case RBE_EAT_FOOD:
				q = "eat your food";
				break;
			case RBE_EAT_LITE:
				q = "absorb light";
				break;
			case RBE_ACID:
				q = "shoot acid";
				break;
			case RBE_ELEC:
				q = "electrify";
				break;
			case RBE_FIRE:
				q = "burn";
				break;
			case RBE_COLD:
				q = "freeze";
				break;
			case RBE_BLIND:
				q = "blind";
				break;
			case RBE_CONFUSE:
				q = "confuse";
				break;
			case RBE_TERRIFY:
				q = "terrify";
				break;
			case RBE_PARALYZE:
				q = "paralyze";
				break;
			case RBE_LOSE_STR:
				q = "reduce strength";
				break;
			case RBE_LOSE_INT:
				q = "reduce intelligence";
				break;
			case RBE_LOSE_WIS:
				q = "reduce wisdom";
				break;
			case RBE_LOSE_DEX:
				q = "reduce dexterity";
				break;
			case RBE_LOSE_CON:
				q = "reduce constitution";
				break;
			case RBE_LOSE_CHR:
				q = "reduce charisma";
				break;
			case RBE_LOSE_ALL:
				q = "reduce all stats";
				break;
			case RBE_SHATTER:
				q = "shatter";
				break;
			case RBE_EXP_10:
				q = "lower experience (by 10d6+)";
				break;
			case RBE_EXP_20:
				q = "lower experience (by 20d6+)";
				break;
			case RBE_EXP_40:
				q = "lower experience (by 40d6+)";
				break;
			case RBE_EXP_80:
				q = "lower experience (by 80d6+)";
				break;
		}


		/* Introduce the attack description */
		if (!r)
		{
			roff(format("%^s can ", wd_he[msex]), 0, 0);
		}
		else if (r < n - 1)
		{
			roff(", ", 0, 0);
		}
		else
		{
			roff(", and ", 0, 0);
		}


		/* Hack -- force a method */
		if (!p)
			p = "do something weird";

		/* Describe the method */
		roff(p, 0, 0);


		/* Describe the effect (if any) */
		if (q)
		{
			/* Describe the attack type */
			roff(" to ", 0, 0);
			roff(q, 0, 0);

			/* Describe damage (if known) */
			if (d1 && d2 && know_damage(r_idx, m))
			{
				/* Display the damage */
				roff(" with damage", 0, 0);
				roff(format(" %dd%d", d1, d2), 0, 0);
			}
		}


		/* Count the attacks as printed */
		r++;
	}

	/* Finish sentence above */
	if (r)
	{
		roff(".  ", 0, 0);
	}

	/* Notice lack of attacks */
	else if (flags1 & (RF1_NEVER_BLOW))
	{
		roff(format("%^s has no physical attacks.  ", wd_he[msex]), 0, 0);
	}

	/* Or describe the lack of knowledge */
	else
	{
		roff(format("Nothing is known about %s attack.  ", wd_his[msex]),
			0, 0);
	}

	/* Cheat -- know everything */
	if ((cheat_know) || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
	{
		/* Hack -- restore memory */
		COPY(l_ptr, &save_mem, monster_lore);
	}
}

long angtk_roff(int r_idx, char *buffer)
{
	/* Global access to the output buffer */
	s_buffer = buffer;
	s_len = 0;

	roff_aux(r_idx);
	
	/* Trim trailing whitespace */
	while (isspace(buffer[s_len - 1])) s_len--;

	/* Terminate (again) */
	buffer[s_len] = '\0';
	
	/* Return the number of characters written */
	return s_len;
}

#endif /* OANGBANDTK */
