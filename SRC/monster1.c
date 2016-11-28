/* File: monster1.c */

/* Purpose: describe monsters (using monster memory) */

/*
 * how the character gains new monster info and the monster recall, including
 * all descriptions.  The player ghost code.  What monsters can be assigned
 * as quests, can appear in and cross various terrain, whether a monster
 * is hostile, friendly or pet, code for making monsters pets and hostile,
 * which monsters are mutual enemies (including good vs. evil)
 *
 * Copyright (c) 1989 James E. Wilson, Christopher J. Stuart
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Pronoun arrays, by gender.
 */
static cptr wd_he[3] =
{ "it", "he", "she" };
static cptr wd_his[3] =
{ "its", "his", "her" };

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
		if (rarity == 1) strcpy(rarity_desc, "very common");
		else if (rarity == 2) strcpy(rarity_desc, "common");
		else if (rarity == 3) strcpy(rarity_desc, "fairly common");
		else if (rarity == 4) strcpy(rarity_desc, "not very common");
		else if ((rarity == 5) || (rarity == 6)) strcpy(rarity_desc, "fairly rare");
		else if (rarity < 10) strcpy(rarity_desc, "rare");
		else strcpy(rarity_desc, "extremely rare");
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

	s32b kills = r_ptr->r_tkills;

	/* Rangers learn quickly. -LM- */
	if (p_ptr->pclass == CLASS_RANGER) kills *= 2;

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

	s32b a = r_ptr->r_blows[i];

	s32b d1 = r_ptr->blow[i].d_dice;
	s32b d2 = r_ptr->blow[i].d_side;

	s32b d = d1 * d2;

	/* Hack -- Rangers learn quickly. -LM- */
	if (p_ptr->pclass == CLASS_RANGER) level = 10 + 3 * level / 2;

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
static void roff_aux(int r_idx, int remem)
{
	monster_race    *r_ptr;

	bool            old = FALSE;
	bool            sin = FALSE;

	int             m, n, r;

	cptr            p, q;

	int             msex = 0;

	bool            breath = FALSE;
	bool            magic = FALSE;

	u32b            flags1;
	u32b            flags2;
	u32b            flags3;
	u64b            flags4;
	u64b            flags5;
	u64b            flags6;
	u32b            flags7;
	u32b            flags8;
	u32b            flags9;

	int             vn = 0;
        byte            color[64];
	cptr            vp[64];

	monster_race    save_mem;



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


	/* Cheat -- Know everything */
	if ((cheat_know) || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
	{
		/* XXX XXX XXX */

		/* Save the "old" memory */
		save_mem = *r_ptr;

		/* Hack -- Maximal kills */
		r_ptr->r_tkills = MAX_SHORT;

		/* Hack -- Maximal info */
		r_ptr->r_wake = r_ptr->r_ignore = MAX_UCHAR;

		/* Observe "maximal" attacks */
		for (m = 0; m < 4; m++)
		{
			/* Examine "actual" blows */
			if (r_ptr->blow[m].effect || r_ptr->blow[m].method)
			{
				/* Hack -- maximal observations */
				r_ptr->r_blows[m] = MAX_UCHAR;
			}
		}

		/* Hack -- maximal drops */
		r_ptr->r_drop_gold = r_ptr->r_drop_item =
		(((r_ptr->flags1 & (RF1_DROP_4D2)) ? 8 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_3D2)) ? 6 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_2D2)) ? 4 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_1D2)) ? 2 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_90))  ? 1 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_60))  ? 1 : 0));

		/* Hack -- but only "valid" drops */
		if (r_ptr->flags1 & (RF1_ONLY_GOLD)) r_ptr->r_drop_item = 0;
		if (r_ptr->flags1 & (RF1_ONLY_ITEM)) r_ptr->r_drop_gold = 0;

		/* Hack -- observe many spells */
		r_ptr->r_cast_inate = MAX_UCHAR;
		r_ptr->r_cast_spell = MAX_UCHAR;

		/* Hack -- know all the flags */
		r_ptr->r_flags1 = r_ptr->flags1;
		r_ptr->r_flags2 = r_ptr->flags2;
		r_ptr->r_flags3 = r_ptr->flags3;
		r_ptr->r_flags4 = r_ptr->flags4;
		r_ptr->r_flags5 = r_ptr->flags5;
		r_ptr->r_flags6 = r_ptr->flags6;
		r_ptr->r_flags7 = r_ptr->flags7;
		r_ptr->r_flags8 = r_ptr->flags8;
		r_ptr->r_flags9 = r_ptr->flags9;
	}


	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE)) msex = 1;


	/* Obtain a copy of the "known" flags */
	flags1 = (r_ptr->flags1 & r_ptr->r_flags1);
	flags2 = (r_ptr->flags2 & r_ptr->r_flags2);
	flags3 = (r_ptr->flags3 & r_ptr->r_flags3);
	flags4 = (r_ptr->flags4 & r_ptr->r_flags4);
	flags5 = (r_ptr->flags5 & r_ptr->r_flags5);
	flags6 = (r_ptr->flags6 & r_ptr->r_flags6);
	flags7 = (r_ptr->flags7 & r_ptr->r_flags7);
	flags8 = (r_ptr->flags8 & r_ptr->r_flags8);
	flags9 = (r_ptr->flags9 & r_ptr->r_flags9);


	/* Assume some "obvious" flags */
	if (r_ptr->flags1 & (RF1_UNIQUE)) flags1 |= (RF1_UNIQUE);
	if (r_ptr->flags1 & (RF1_QUESTOR)) flags1 |= (RF1_QUESTOR);
	if (r_ptr->flags1 & (RF1_MALE)) flags1 |= (RF1_MALE);
	if (r_ptr->flags1 & (RF1_FEMALE)) flags1 |= (RF1_FEMALE);
	if (r_ptr->flags2 & (RF2_NOMISS)) flags2 |= (RF2_NOMISS);

	/* Assume some "creation" flags */
	if (r_ptr->flags1 & (RF1_FRIEND)) flags1 |= (RF1_FRIEND);
	if (r_ptr->flags1 & (RF1_FRIENDS)) flags1 |= (RF1_FRIENDS);
	if (r_ptr->flags1 & (RF1_ESCORT)) flags1 |= (RF1_ESCORT);
	if (r_ptr->flags1 & (RF1_ESCORTS)) flags1 |= (RF1_ESCORTS);

	/* Killing a monster reveals some properties */
	if (r_ptr->r_tkills)
	{
		/* Know "race" flags */
		if (r_ptr->flags3 & (RF3_ORC)) flags3 |= (RF3_ORC);
		if (r_ptr->flags3 & (RF3_TROLL)) flags3 |= (RF3_TROLL);
		if (r_ptr->flags3 & (RF3_GIANT)) flags3 |= (RF3_GIANT);
		if (r_ptr->flags3 & (RF3_DRAGON)) flags3 |= (RF3_DRAGON);
		if (r_ptr->flags3 & (RF3_DEMON)) flags3 |= (RF3_DEMON);
		if (r_ptr->flags3 & (RF3_UNDEAD)) flags3 |= (RF3_UNDEAD);
		if (r_ptr->flags3 & (RF3_EVIL)) flags3 |= (RF3_EVIL);
		if (r_ptr->flags3 & (RF3_GOOD)) flags3 |= (RF3_GOOD);
		if (r_ptr->flags2 & (RF2_LAWFUL)) flags2 |= (RF2_LAWFUL);
		if (r_ptr->flags2 & (RF2_CHAOTIC)) flags2 |= (RF2_CHAOTIC);
		if (r_ptr->flags3 & (RF3_ANIMAL)) flags3 |= (RF3_ANIMAL);
		if (r_ptr->flags3 & (RF3_DUNADAN)) flags3 |= (RF3_DUNADAN);
		if (r_ptr->flags2 & (RF2_DOOM)) flags2 |= (RF2_DOOM);

		/* Know "forced" flags */
		if (r_ptr->flags1 & (RF1_FORCE_DEPTH)) flags1 |= (RF1_FORCE_DEPTH);
		if (r_ptr->flags1 & (RF1_FORCE_MAXHP)) flags1 |= (RF1_FORCE_MAXHP);
	}


	/* Require a flag to show kills */
	if (!(show_details))
	{
		/* nothing */
	}

	/* Treat uniques differently */
	else if (flags1 & (RF1_UNIQUE))
	{
		/* Hack -- Determine if the unique is "dead" */
		bool dead = (r_ptr->max_num == 0) ? TRUE : FALSE;

		/* We've been killed... */
		if (r_ptr->r_deaths)
		{
			/* Killed ancestors */
			roff(format("%^s has slain %d of your ancestors",
				    wd_he[msex], r_ptr->r_deaths));

			/* But we've also killed it */
			if (dead)
			{
				roff(format(", but you have avenged %s!  ",
					    plural(r_ptr->r_deaths, "him", "them")));
			}

			/* Unavenged (ever) */
			else
			{
				roff(format(", who %s unavenged.  ",
					    plural(r_ptr->r_deaths, "remains", "remain")));
			}
		}

		/* Dead unique who never hurt us */
		else if (dead)
		{
			roff("You have slain this foe.  ");
		}
	}

	/* Not unique, but killed us */
	else if (r_ptr->r_deaths)
	{
		/* Dead ancestors */
		roff(format("%d of your ancestors %s been killed by this creature, ",
			    r_ptr->r_deaths, plural(r_ptr->r_deaths, "has", "have")));

		/* Some kills this life */
		if (r_ptr->r_pkills)
		{
			roff(format("and you have exterminated at least %d of the creatures.  ",
				    r_ptr->r_pkills));
		}

		/* Some kills past lives */
		else if (r_ptr->r_tkills)
		{
			roff(format("and %s have exterminated at least %d of the creatures.  ",
				    "your ancestors", r_ptr->r_tkills));
		}

		/* No kills */
		else
		{
			roff(format("and %s is not ever known to have been defeated.  ",
				    wd_he[msex]));
		}
	}

	/* Normal monsters */
	else
	{
		/* Killed some this life */
		if (r_ptr->r_pkills)
		{
			roff(format("You have killed at least %d of these creatures.  ",
				    r_ptr->r_pkills));
		}

		/* Killed some last life */
		else if (r_ptr->r_tkills)
		{
			roff(format("Your ancestors have killed at least %d of these creatures.  ",
				    r_ptr->r_tkills));
		}

		/* Killed none */
		else
		{
			roff("No battles to the death are recalled.  ");
		}
	}

	roff("\n");

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
			for (i = r_idx+1; i < max_r_idx; i++)
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
			(void)fd_seek(fd, pos);

			/* Read a chunk of data */
			(void)fd_read(fd, buf, 2048);

			/* Close it */
			(void)fd_close(fd);
		}

#else

		/* Simple method */
		strcpy(buf, r_text + r_ptr->text);
#endif

		/* Dump it */
		roff(buf);
		roff("  ");
	}

	roff("\n");

	/* Nothing yet */
	old = FALSE;

	/* Describe location */
	if (r_ptr->flags7 & RF7_PET)
	{
		roff(format("%^s is friendly to you", wd_he[msex]));
		old = TRUE;
	}

	/* Describe location */
	if (r_ptr->level == 0)
	{
		roff(format("%^s lives in the town", wd_he[msex]));
		old = TRUE;
	}
	else if (r_ptr->r_tkills)
	{
		if (depth_in_feet)
		{
			roff(format("%^s is %s and normally found at depths of %d feet",
				    wd_he[msex], wd_rarity(r_ptr->rarity, r_ptr->flags1 & (RF1_UNIQUE)), r_ptr->level * 50));
		}
		else
		{
			roff(format("%^s is %s and normally found on dungeon level %d",
				    wd_he[msex], wd_rarity(r_ptr->rarity, r_ptr->flags1 & (RF1_UNIQUE)), r_ptr->level));
		}
		old = TRUE;
	}


	/* Describe movement */
	if (TRUE)
	{
		/* Introduction */
		if (old)
		{
			roff(", and ");
		}
		else
		{
			roff(format("%^s ", wd_he[msex]));
			old = TRUE;
		}
		roff("moves");

		/* Random-ness */
		if ((flags1 & (RF1_RAND_50)) || (flags1 & (RF1_RAND_25)))
		{
			/* Adverb */
			if ((flags1 & (RF1_RAND_50)) && (flags1 & (RF1_RAND_25)))
			{
				roff(" extremely");
			}
			else if (flags1 & (RF1_RAND_50))
			{
				roff(" somewhat");
			}
			else if (flags1 & (RF1_RAND_25))
			{
				roff(" a bit");
			}

			/* Adjective */
			roff(" erratically");

			/* Hack -- Occasional conjunction */
			if (r_ptr->speed != 110) roff(", and");
		}

		/* Speed */
		if (r_ptr->speed > 110)
		{
			if (r_ptr->speed > 130) c_roff(TERM_RED, " incredibly");
			else if (r_ptr->speed > 120) c_roff(TERM_ORANGE, " very");
			c_roff(TERM_L_RED," quickly");
		}
		else if (r_ptr->speed < 110)
		{
                        if (r_ptr->speed < 90) c_roff(TERM_L_GREEN, " incredibly");
                        else if (r_ptr->speed < 100) c_roff(TERM_BLUE, " very");
                        c_roff(TERM_L_BLUE, " slowly");
		}
		else
		{
			roff(" at normal speed");
		}

	}

	/* The code above includes "attack speed" */
	if (flags1 & (RF1_NEVER_MOVE))
	{
		/* Introduce */
		if (old)
		{
			roff(", but ");
		}
		else
		{
			roff(format("%^s ", wd_he[msex]));
			old = TRUE;
		}

		/* Describe */
		roff("does not deign to chase intruders");
	}

	/* End this sentence */
	if (old)
	{
		roff(".  ");
		old = FALSE;
	}

	roff("\n");

	/* Describe experience if known */
	if (r_ptr->r_tkills)
	{
		/* Introduction */
		if (flags1 & (RF1_UNIQUE))
		{
			roff("Killing this");
		}
		else
		{
			roff("A kill of this");
		}

		/* Describe the "quality" */
                if (flags2 & (RF2_ELDRITCH_HORROR)) c_roff(TERM_VIOLET, " sanity-blasting");
		if (flags3 & (RF3_ANIMAL))          roff(" natural");
                
                /* Describe alignment */

                if (!(flags2 & (RF2_CHAOTIC) || flags2 & (RF2_LAWFUL) ||
                      flags3 & (RF3_EVIL) || flags3 & (RF3_GOOD)))
                roff (" true neutral");
                else
                {
		if (flags2 & (RF2_CHAOTIC))         roff(" chaotic");
		else if (flags2 & (RF2_LAWFUL))     roff(" lawful");
                else roff(" neutral");
		if (flags3 & (RF3_EVIL))            roff(" evil");
		else if (flags3 & (RF3_GOOD))       roff(" good");
                else roff(" neutral");
                }


		/* Describe the "race" */
		if (flags3 & (RF3_DRAGON))          roff(" dragon");
                else if (flags3 & (RF3_DEMON))      c_roff(TERM_VIOLET, " demon");
		else if (flags3 & (RF3_GIANT))      roff(" giant");
		else if (flags3 & (RF3_TROLL))      roff(" troll");
		else if (flags3 & (RF3_ORC))        roff(" orc");
		else if (flags3 & (RF3_DUNADAN))    roff(" Dunadan");
		else                                roff(" creature");

		if (flags2 & (RF2_DOOM))          roff(" of Id");

		/* Group some variables */
		if (TRUE)
		{
			long i, j;

			/* calculate the integer exp part */
			i = (long)r_ptr->mexp * r_ptr->level / p_ptr->lev;

			/* calculate the fractional exp part scaled by 100, */
			/* must use long arithmetic to avoid overflow  */
			j = ((((long)r_ptr->mexp * r_ptr->level % p_ptr->lev) *
			       (long)1000 / p_ptr->lev + 5) / 10);

			/* Mention the experience */
			roff(format(" is worth %ld.%02ld point%s",
				    (long)i, (long)j,
				    (((i == 1) && (j == 0)) ? "" : "s")));

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
			roff(format(" for a%s %lu%s level character.  ",
				    q, (long)i, p));
		}
	}

	roff("\n");

	if ((flags2 & (RF2_AURA_FIRE)) && (flags2 & (RF2_AURA_ELEC)) &&
	   (flags3 & (RF3_AURA_COLD)))
	{
		roff(format("%^s is surrounded by ice, flames and electricity.  ", wd_he[msex]));
		roff("\n");
	}
	else if ((flags2 & (RF2_AURA_FIRE)) && (flags2 & (RF2_AURA_ELEC)))
	{
                c_roff(TERM_VIOLET, format("%^s is surrounded by flames and electricity.  ", wd_he[msex]));
		roff("\n");
	}
	else if ((flags3 & (RF3_AURA_COLD)) && (flags2 & (RF2_AURA_ELEC)))
	{
                c_roff(TERM_L_BLUE, format("%^s is surrounded by ice and electricity.  ", wd_he[msex]));
		roff("\n");
	}
	else if (flags2 & (RF2_AURA_FIRE))
	{
                c_roff(TERM_RED, format("%^s is surrounded by flames.  ", wd_he[msex]));
		roff("\n");
	}
	else if (flags3 & (RF3_AURA_COLD))
	{
                c_roff(TERM_L_WHITE, format("%^s is surrounded by ice.  ", wd_he[msex]));
		roff("\n");
	}
	else if (flags2 & (RF2_AURA_ELEC))
	{
                c_roff(TERM_BLUE, format("%^s is surrounded by electricity.  ", wd_he[msex]));
		roff("\n");
	}
	if (flags2 & (RF2_AURA_ELEC))
	{
                c_roff(TERM_L_DARK, format("%^s is cloaked in fear.  ", wd_he[msex]));
		roff("\n");
	}


	if (flags2 & (RF2_REFLECTING))
	{
		roff(format("%^s reflects bolt spells.  ", wd_he[msex]));
		roff("\n");
	}

	/* Describe escorts */
	if ((flags1 & (RF1_ESCORT)) || (flags1 & (RF1_ESCORTS)))
	{
		roff(format("%^s usually appears with escorts.  ",
			    wd_he[msex]));
		roff("\n");
	}

	/* Describe friends */
	else if ((flags1 & (RF1_FRIEND)) || (flags1 & (RF1_FRIENDS)))
	{
		roff(format("%^s usually appears in groups.  ",
			    wd_he[msex]));
		roff("\n");
	}

	/* Collect inate attacks */
	vn = 0;
	if (flags4 & (RF4_SHRIEK))              vp[vn++] = "shriek for help";
	if (flags4 & (RF4_ARROW_5))             vp[vn++] = "fire a seeker arrow";
	if (flags4 & (RF4_ROCKET))              vp[vn++] = "shoot a rocket";
	if (flags4 & (RF4_ARROW_1))             vp[vn++] = "fire an arrow";
	if (flags4 & (RF4_ARROW_2))             vp[vn++] = "fire arrows";
	if (flags4 & (RF4_ARROW_3))             vp[vn++] = "fire a missile";
	if (flags4 & (RF4_ARROW_4))             vp[vn++] = "fire missiles";

	/* Describe inate attacks */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" may ");
			else if (n < vn-1) roff(", ");
			else roff(" or ");

			/* Dump */
			roff(vp[n]);
		}

		/* End */
		roff(".  ");
		roff("\n");
	}


	/* Collect breaths */
	vn = 0;
	if (flags4 & (RF4_BR_ACID))             vp[vn++] = "acid";
	if (flags4 & (RF4_BR_ELEC))             vp[vn++] = "lightning";
	if (flags4 & (RF4_BR_FIRE))             vp[vn++] = "fire";
	if (flags4 & (RF4_BR_COLD))             vp[vn++] = "frost";
	if (flags4 & (RF4_BR_POIS))             vp[vn++] = "poison";
	if (flags4 & (RF4_BR_NETH))             vp[vn++] = "nether";
	if (flags4 & (RF4_BR_LITE))             vp[vn++] = "light";
	if (flags4 & (RF4_BR_DARK))             vp[vn++] = "darkness";
	if (flags4 & (RF4_BR_CONF))             vp[vn++] = "confusion";
	if (flags4 & (RF4_BR_SOUN))             vp[vn++] = "sound";
	if (flags4 & (RF4_BR_CHAO))             vp[vn++] = "chaos";
	if (flags4 & (RF4_BR_DISE))             vp[vn++] = "disenchantment";
	if (flags4 & (RF4_BR_NEXU))             vp[vn++] = "nexus";
	if (flags4 & (RF4_BR_TIME))             vp[vn++] = "time";
	if (flags4 & (RF4_BR_INER))             vp[vn++] = "inertia";
	if (flags4 & (RF4_BR_GRAV))             vp[vn++] = "gravity";
	if (flags4 & (RF4_BR_SHAR))             vp[vn++] = "shards";
	if (flags4 & (RF4_BR_PLAS))             vp[vn++] = "plasma";
	if (flags4 & (RF4_BR_WALL))             vp[vn++] = "force";
	if (flags4 & (RF4_BR_MANA))             vp[vn++] = "mana";
	if (flags4 & (RF4_BR_NUKE))             vp[vn++] = "toxic waste";
	if (flags4 & (RF4_BR_DISI))             vp[vn++] = "disintegration";
	if (flags6 & (RF6_BR_WIND))             vp[vn++] = "wind";
	if (flags4 & (RF4_BR_SKUL))             vp[vn++] = "flying skulls";

	/* Describe breaths */
	if (vn)
	{
		/* Note breath */
		breath = TRUE;

		/* Intro */
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" may breathe ");
			else if (n < vn-1) roff(", ");
			else roff(" or ");

			/* Dump */
			roff(vp[n]);
		}
	}

	/* Collect spells */
	vn = 0;
	if (flags5 & (RF5_BA_ACID))         vp[vn++] = "produce acid balls";
	if (flags5 & (RF5_BA_ELEC))         vp[vn++] = "produce lightning balls";
	if (flags5 & (RF5_BA_FIRE))         vp[vn++] = "produce fire balls";
	if (flags5 & (RF5_BA_COLD))         vp[vn++] = "produce frost balls";
	if (flags5 & (RF5_BA_POIS))         vp[vn++] = "produce poison balls";
	if (flags5 & (RF5_BA_NETH))         vp[vn++] = "produce nether balls";
	if (flags5 & (RF5_BA_WATE))         vp[vn++] = "produce water balls";
	if (flags4 & (RF4_BA_NUKE))         vp[vn++] = "produce balls of radiation";
	if (flags6 & (RF6_BA_WIND))         vp[vn++] = "produce wind balls";
	if (flags5 & (RF5_BA_MANA))         vp[vn++] = "invoke mana storms";
	if (flags5 & (RF5_BA_DARK))         vp[vn++] = "invoke darkness storms";
	if (flags4 & (RF4_BA_CHAO))         vp[vn++] = "invoke raw Logrus";
	if (flags6 & (RF6_HAND_DOOM))       vp[vn++] = "invoke the Hand of Doom";
	if (flags4 & (RF4_BO_SHAR))	    vp[vn++] = "fire shard bolts";
	if (flags4 & (RF4_BL_SHAR))	    vp[vn++] = "fire shard blasts";
	if (flags4 & (RF4_CH_SHAR))	    vp[vn++] = "fire chainshards";
	if (flags5 & (RF5_DRAIN_MANA))      vp[vn++] = "drain mana";
	if (flags5 & (RF5_MIND_BLAST))      vp[vn++] = "cause mind blasting";
	if (flags5 & (RF5_BRAIN_SMASH))     vp[vn++] = "cause brain smashing";
	if (flags5 & (RF5_CAUSE_1))         vp[vn++] = "cause light wounds and cursing";
	if (flags5 & (RF5_CAUSE_2))         vp[vn++] = "cause serious wounds and cursing";
	if (flags5 & (RF5_CAUSE_3))         vp[vn++] = "cause critical wounds and cursing";
	if (flags5 & (RF5_CAUSE_4))         vp[vn++] = "cause mortal wounds";
	if (flags5 & (RF5_BO_ACID))         vp[vn++] = "produce acid bolts";
	if (flags5 & (RF5_BO_ELEC))         vp[vn++] = "produce lightning bolts";
	if (flags5 & (RF5_BO_FIRE))         vp[vn++] = "produce fire bolts";
	if (flags5 & (RF5_BO_COLD))         vp[vn++] = "produce frost bolts";
	if (flags5 & (RF5_BO_POIS))         vp[vn++] = "produce poison bolts";
	if (flags5 & (RF5_BO_NETH))         vp[vn++] = "produce nether bolts";
	if (flags5 & (RF5_BO_WATE))         vp[vn++] = "produce water bolts";
	if (flags5 & (RF5_BO_MANA))         vp[vn++] = "produce mana bolts";
	if (flags5 & (RF5_BO_PLAS))         vp[vn++] = "produce plasma bolts";
	if (flags5 & (RF5_BO_ICEE))         vp[vn++] = "produce ice bolts";
	if (flags5 & (RF5_MISSILE))         vp[vn++] = "produce magic missiles";
	if (flags5 & (RF5_SCARE))           vp[vn++] = "terrify";
	if (flags5 & (RF5_BLIND))           vp[vn++] = "blind";
	if (flags5 & (RF5_CONF))            vp[vn++] = "confuse";
	if (flags5 & (RF5_SLOW))            vp[vn++] = "slow";
	if (flags5 & (RF5_HOLD))            vp[vn++] = "paralyze";
	if (flags6 & (RF6_HASTE))           vp[vn++] = "haste-self";
	if (flags6 & (RF6_HEAL))            vp[vn++] = "heal-self";
	if (flags6 & (RF6_HEAL2))           vp[vn++] = "super heal-self";
	if (flags6 & (RF6_BLINK))           vp[vn++] = "blink-self";
	if (flags6 & (RF6_TPORT))           vp[vn++] = "teleport-self";
	if (flags6 & (RF6_TELE_TO))         vp[vn++] = "teleport to";
	if (flags6 & (RF6_TELE_AWAY))       vp[vn++] = "teleport away";
	if (flags6 & (RF6_TELE_LEVEL))      vp[vn++] = "teleport level";
	if (flags6 & (RF6_DOOR))            vp[vn++] = "create dimension doors";
	if (flags6 & (RF6_DARKNESS))        vp[vn++] = "create darkness";
	if (flags6 & (RF6_TRAPS))           vp[vn++] = "create traps";
	if (flags6 & (RF6_FORGET))          vp[vn++] = "cause amnesia";
	if (flags6 & (RF6_RAISE_DEAD))      vp[vn++] = "raise dead";
	if (flags6 & (RF6_S_MONSTER))       vp[vn++] = "summon a monster";
	if (flags6 & (RF6_S_MONSTERS))      vp[vn++] = "summon monsters";
	if (flags6 & (RF6_S_KIN))           vp[vn++] = "summon aid";
	if (flags6 & (RF6_S_ANT))           vp[vn++] = "summon ants";
	if (flags6 & (RF6_S_SPIDER))        vp[vn++] = "summon spiders";
	if (flags6 & (RF6_S_HOUND))         vp[vn++] = "summon hounds";
	if (flags6 & (RF6_S_HYDRA))         vp[vn++] = "summon hydras";
	if (flags6 & (RF6_S_ANGEL))         vp[vn++] = "summon an angel";
	if (flags6 & (RF6_S_DEMON))         vp[vn++] = "summon a demon";
	if (flags6 & (RF6_S_UNDEAD))        vp[vn++] = "summon an undead";
	if (flags6 & (RF6_S_DRAGON))        vp[vn++] = "summon a dragon";
	if (flags6 & (RF6_S_HI_UNDEAD))     vp[vn++] = "summon Greater Undead";
	if (flags6 & (RF6_S_HI_DRAGON))     vp[vn++] = "summon Ancient Dragons";
	if (flags6 & (RF6_S_CYBER))         vp[vn++] = "summon Cyberdemons";
	if (flags6 & (RF6_S_WRAITH))        vp[vn++] = "summon wraiths";
	if (flags6 & (RF6_S_UNIQUE))        vp[vn++] = "summon unique monsters";
	if (flags6 & (RF6_S_DOOM))          vp[vn++] = "summon servants of Id";
	if (flags6 & (RF6_S_HI_DOOM))       vp[vn++] = "summon powerful servants of Id";

	/* Describe spells */
	if (vn)
	{
		/* Note magic */
		magic = TRUE;

		/* Intro */
		if (breath)
		{
			roff(", and is also");
		}
		else
		{
			roff(format("%^s is", wd_he[msex]));
		}

		/* Verb Phrase */
		roff(" magical, casting spells");

		/* Adverb */
                if (flags2 & (RF2_SMART)) c_roff(TERM_YELLOW, " intelligently");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" which ");
			else if (n < vn-1) roff(", ");
			else roff(" or ");

			/* Dump */
			roff(vp[n]);
		}
	}

	/* End the sentence about inate/other spells */
	if (breath || magic)
	{
		/* Total casting */
		m = r_ptr->r_cast_inate + r_ptr->r_cast_spell;

		/* Average frequency */
		n = (r_ptr->freq_inate + r_ptr->freq_spell) / 2;

		/* Describe the spell frequency */
		if (m > 100)
		{
			roff(format("; 1 time in %d", 100 / n));
		}

		/* Guess at the frequency */
		else if (m)
		{
			n = ((n + 9) / 10) * 10;
			roff(format("; about 1 time in %d", 100 / n));
		}

		/* End this sentence */
		roff(".  ");
		roff("\n");
	}


	/* Describe monster "toughness" */
	if (know_armour(r_idx))
	{
		/* Armor */
		roff(format("%^s has an armor rating of %d",
			    wd_he[msex], r_ptr->ac));

		/* Maximized hitpoints */
		if (flags1 & (RF1_FORCE_MAXHP))
		{
			roff(format(" and a life rating of %d.  ",
				    r_ptr->hdice * r_ptr->hside));
			roff("\n");
		}

		/* Variable hitpoints */
		else
		{
			roff(format(" and a life rating of %dd%d.  ",
				    r_ptr->hdice, r_ptr->hside));
			roff("\n");
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
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" can ");
			else if (n < vn-1) roff(", ");
			else roff(" and ");

			/* Dump */
			roff(vp[n]);
		}

		/* End */
		roff(".  ");
		roff("\n");
	}


	/* Describe special abilities. */
	if (flags2 & (RF2_INVISIBLE))
	{
		roff(format("%^s is invisible.  ", wd_he[msex]));
		roff("\n");
	}
	if (flags2 & (RF2_COLD_BLOOD))
	{
		roff(format("%^s is cold blooded.  ", wd_he[msex]));
		roff("\n");
	}
	if (flags2 & (RF2_EMPTY_MIND))
	{
		roff(format("%^s is not detected by telepathy.  ", wd_he[msex]));
		roff("\n");
	}
	if (flags2 & (RF2_WEIRD_MIND))
	{
		roff(format("%^s is rarely detected by telepathy.  ", wd_he[msex]));
		roff("\n");
	}
	if (flags2 & (RF2_MULTIPLY))
	{
                c_roff(TERM_L_UMBER, format("%^s breeds explosively.  ", wd_he[msex]));
		roff("\n");
	}
	if (flags2 & (RF2_REGENERATE))
	{
                c_roff(TERM_L_WHITE, format("%^s regenerates quickly.  ", wd_he[msex]));
		roff("\n");
	}


	/* Collect susceptibilities */
	vn = 0;
        if (flags3 & (RF3_HURT_ROCK)) {vp[vn++] = "rock remover"; color[vn - 1] = TERM_UMBER;}
        if (flags3 & (RF3_HURT_LITE)) {vp[vn++] = "bright light"; color[vn - 1] = TERM_YELLOW;}
        if (flags3 & (RF3_HURT_FIRE)) {vp[vn++] = "fire"; color[vn - 1] = TERM_RED;}
        if (flags3 & (RF3_HURT_COLD)) {vp[vn++] = "cold"; color[vn - 1] = TERM_L_WHITE;}
        if (flags9 & (RF9_HURT_ACID)) {vp[vn++] = "acid"; color[vn - 1] = TERM_GREEN;}
        if (flags9 & (RF9_HURT_ELEC)) {vp[vn++] = "lightning"; color[vn - 1] = TERM_L_BLUE;}
        if (flags9 & (RF9_HURT_POIS)) {vp[vn++] = "poison"; color[vn - 1] = TERM_L_GREEN;}

	/* Describe susceptibilities */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" is hurt by ");
			else if (n < vn-1) roff(", ");
			else roff(" and ");

			/* Dump */
                        c_roff(color[n], vp[n]);
		}

		/* End */
		roff(".  ");
		roff("\n");
	}


	/* Collect immunities */
	vn = 0;
	if (flags7 & (RF7_RES_BASH)) {vp[vn++] = "blunt weapons"; color[vn - 1] = TERM_WHITE;}
	if (flags7 & (RF7_RES_SLASH)) {vp[vn++] = "edged weapons"; color[vn - 1] = TERM_WHITE;}
	if (flags7 & (RF7_RES_STAB)) {vp[vn++] = "polearms"; color[vn - 1] = TERM_WHITE;}
        if (flags3 & (RF3_IM_ACID)) {vp[vn++] = "acid"; color[vn - 1] = TERM_L_GREEN;}
        if (flags3 & (RF3_IM_ELEC)) {vp[vn++] = "lightning"; color[vn - 1] = TERM_L_BLUE;}
        if (flags3 & (RF3_IM_FIRE)) {vp[vn++] = "fire"; color[vn - 1] = TERM_RED;}
        if (flags3 & (RF3_IM_COLD)) {vp[vn++] = "cold"; color[vn - 1] = TERM_L_WHITE;}
        if (flags3 & (RF3_IM_POIS)) {vp[vn++] = "poison"; color[vn - 1] = TERM_L_GREEN;}
	if (flags3 & (RF3_RES_NETH)) {vp[vn++] = "nether"; color[vn - 1] = TERM_L_DARK;}
	if (flags3 & (RF3_RES_WATE)) {vp[vn++] = "water"; color[vn - 1] = TERM_BLUE;}
	if (flags3 & (RF3_RES_PLAS)) {vp[vn++] = "plasma"; color[vn - 1] = TERM_ORANGE;}
	if (flags3 & (RF3_RES_NEXU)) {vp[vn++] = "nexus"; color[vn - 1] = TERM_VIOLET;}
	if (flags3 & (RF3_RES_DISE)) {vp[vn++] = "disenchantment"; color[vn - 1] = TERM_L_UMBER;}
	if (flags3 & (RF3_RES_TELE)) {vp[vn++] = "teleportation";color[vn - 1] = TERM_YELLOW;}

	/* Describe immunities */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" resists ");
			else if (n < vn-1) roff(", ");
			else roff(" and ");

			/* Dump */
                        c_roff(color[n], vp[n]);
		}

		/* End */
		roff(".  ");
		roff("\n");
	}


	/* Collect non-effects */
	vn = 0;
	if (flags3 & (RF3_NO_STUN)) vp[vn++] = "stunned";
	if (flags3 & (RF3_NO_FEAR)) vp[vn++] = "frightened";
	if (flags3 & (RF3_NO_CONF)) vp[vn++] = "confused";
	if (flags3 & (RF3_NO_SLEEP)) vp[vn++] = "slept";
	if (flags7 & (RF7_IM_BASH)) vp[vn++] = "harmed by blunt weapons";
	if (flags7 & (RF7_IM_SLASH)) vp[vn++] = "harmed by edged weapons";
	if (flags7 & (RF7_IM_STAB)) vp[vn++] = "harmed by polearms";

	/* Describe non-effects */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" cannot be ");
			else if (n < vn-1) roff(", ");
			else roff(" or ");

			/* Dump */
			roff(vp[n]);
		}

		/* End */
		roff(".  ");
		roff("\n");
	}


	/* Do we know how aware it is? */
	if ((((int)r_ptr->r_wake * (int)r_ptr->r_wake) > r_ptr->sleep) ||
		  (r_ptr->r_ignore == MAX_UCHAR) ||
		 ((r_ptr->sleep == 0) && (r_ptr->r_tkills >= 10)))
	{
		cptr act;

		if (r_ptr->sleep > 200)
		{
			act = "prefers to ignore";
		}
		else if (r_ptr->sleep > 95)
		{
			act = "pays very little attention to";
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

		roff(format("%^s %s intruders, which %s may notice from %d feet.  ",
			    wd_he[msex], act, wd_he[msex], 10 * r_ptr->aaf));
		roff("\n");
	}


	/* Drops gold and/or items */
	if (r_ptr->r_drop_gold || r_ptr->r_drop_item)
	{
		/* No "n" needed */
		sin = FALSE;

		/* Intro */
		roff(format("%^s may carry", wd_he[msex]));

		/* Count maximum drop */
		n = MAX(r_ptr->r_drop_gold, r_ptr->r_drop_item);

		/* One drop (may need an "n") */
		if (n == 1)
		{
			roff(" a");
			sin = TRUE;
		}

		/* Two drops */
		else if (n == 2)
		{
			roff(" one or two");
		}

		/* Many drops */
		else
		{
			roff(format(" up to %d", n));
		}


		/* Great */
		if (flags1 & (RF1_DROP_GREAT))
		{
			p = " great";
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
		if (r_ptr->r_drop_item)
		{
			/* Handle singular "an" */
			if (sin) roff("n");
			sin = FALSE;

			/* Dump "object(s)" */
			if (p) roff(p);
			roff(" object");
			if (n != 1) roff("s");

			/* Conjunction replaces variety, if needed for "gold" below */
			p = " or";
		}

		/* Treasures */
		if (r_ptr->r_drop_gold)
		{
			/* Cancel prefix */
			if (!p) sin = FALSE;

			/* Handle singular "an" */
			if (sin) roff("n");
			sin = FALSE;

			/* Dump "treasure(s)" */
			if (p) roff(p);
			roff(" treasure");
			if (n != 1) roff("s");
		}

		/* End this sentence */
		roff(".  ");
		roff("\n");
	}


	/* Count the number of "known" attacks */
	for (n = 0, m = 0; m < 4; m++)
	{
		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Count known attacks */
		if (r_ptr->r_blows[m]) n++;
	}

	/* Examine (and count) the actual attacks */
	for (r = 0, m = 0; m < 4; m++)
	{
		int method, effect, d1, d2;

		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Skip unknown attacks */
		if (!r_ptr->r_blows[m]) continue;


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
			case RBM_HIT:           p = "hit"; break;
			case RBM_TOUCH:         p = "touch"; break;
			case RBM_PUNCH:         p = "punch"; break;
			case RBM_KICK:          p = "kick"; break;
			case RBM_CLAW:          p = "claw"; break;
			case RBM_BITE:          p = "bite"; break;
			case RBM_STING:         p = "sting"; break;
			case RBM_BEAK:          p = "beak"; break;
			case RBM_BUTT:          p = "butt"; break;
			case RBM_CRUSH:         p = "crush"; break;
			case RBM_ENGULF:        p = "engulf"; break;
			case RBM_CHARGE:        p = "charge";   break;
			case RBM_CRAWL:         p = "crawl on you"; break;
			case RBM_DROOL:         p = "drool on you"; break;
			case RBM_SPIT:          p = "spit"; break;
			case RBM_EXPLODE:       p = "explode"; break;
			case RBM_GAZE:          p = "gaze"; break;
			case RBM_WAIL:          p = "wail"; break;
			case RBM_SPORE:         p = "release spores"; break;
			case RBM_XXX4:          break;
			case RBM_BEG:           p = "beg"; break;
			case RBM_INSULT:        p = "insult"; break;
			case RBM_MOAN:          p = "moan"; break;
			case RBM_SHOW:          p = "sing"; break;
		}


		/* Default effect */
		q = NULL;

		/* Acquire the effect */
		switch (effect)
		{
			case RBE_HURT:          q = "attack"; break;
			case RBE_POISON:        q = "poison"; break;
			case RBE_UN_BONUS:      q = "disenchant"; break;
			case RBE_UN_POWER:      q = "drain charges"; break;
			case RBE_EAT_GOLD:      q = "steal gold"; break;
			case RBE_EAT_ITEM:      q = "steal items"; break;
			case RBE_EAT_FOOD:      q = "eat your food"; break;
			case RBE_EAT_LITE:      q = "absorb light"; break;
			case RBE_ACID:          q = "corrode"; break;
			case RBE_ELEC:          q = "electrocute"; break;
			case RBE_FIRE:          q = "burn"; break;
			case RBE_COLD:          q = "freeze"; break;
			case RBE_BLIND:         q = "blind"; break;
			case RBE_CONFUSE:       q = "confuse"; break;
			case RBE_TERRIFY:       q = "terrify"; break;
			case RBE_PARALYZE:      q = "paralyze"; break;
			case RBE_LOSE_STR:      q = "reduce strength"; break;
			case RBE_LOSE_INT:      q = "reduce intelligence"; break;
			case RBE_LOSE_WIS:      q = "reduce wisdom"; break;
			case RBE_LOSE_DEX:      q = "reduce dexterity"; break;
			case RBE_LOSE_CON:      q = "reduce constitution"; break;
			case RBE_LOSE_CHR:      q = "reduce charisma"; break;
			case RBE_LOSE_LUC:      q = "reduce luck"; break;
			case RBE_LOSE_ALL:      q = "reduce all stats"; break;
			case RBE_SHATTER:       q = "shatter"; break;
			case RBE_EXP_10:        q = "lower experience (by 10d6+)"; break;
			case RBE_EXP_20:        q = "lower experience (by 20d6+)"; break;
			case RBE_EXP_40:        q = "lower experience (by 40d6+)"; break;
			case RBE_EXP_80:        q = "lower experience (by 80d6+)"; break;
			case RBE_DISEASE:       q = "cause disease"; break;
			case RBE_TIME:          q = "cause time effects"; break;
			case RBE_INSANITY:      q = "cause insanity"; break;
			case RBE_HALLU:         q = "induce hallucinations"; break;
			case RBE_HUNGER:        q = "cause hunger"; break;
			case RBE_DEATH:         q = "cause instant death"; break;
			case RBE_STONE:         q = "stone"; break;
		}


		/* Introduce the attack description */
		if (!r)
		{
			roff(format("%^s can ", wd_he[msex]));
		}
		else if (r < n-1)
		{
			roff(", ");
		}
		else
		{
			roff(", and ");
		}


		/* Hack -- force a method */
		if (!p) p = "do something weird";

		/* Describe the method */
		roff(p);


		/* Describe the effect (if any) */
		if (q)
		{
			/* Describe the attack type */
			roff(" to ");
			roff(q);

			/* Describe damage (if known) */
			if (d1 && d2 && know_damage(r_idx, m))
			{
				/* Display the damage */
				roff(" with damage");
				roff(format(" %dd%d", d1, d2));
			}
		}


		/* Count the attacks as printed */
		r++;
	}

	/* Finish sentence above */
	if (r)
	{
		/* Does it miss? */
		if (flags2 & (RF2_NOMISS))
		{
			roff(format(", and %s never misses.  ", wd_he[msex]));
		}
		else
		{
		roff(".  ");
		roff("\n");
		}
	}

	/* Notice lack of attacks */
	else if (flags1 & (RF1_NEVER_BLOW))
	{
		roff(format("%^s has no physical attacks.  ", wd_he[msex]));
		roff("\n");
	}

	/* Or describe the lack of knowledge */
	else
	{
		roff(format("Nothing is known about %s attack.  ", wd_his[msex]));
		roff("\n");
	}

	/*
	 * Notice "Quest" monsters, but only if you
	 * already encountered the monster.
	 */
	if ((flags1 & RF1_QUESTOR) && (r_ptr->r_sights))
	{
                c_roff(TERM_VIOLET, "You feel an intense desire to kill this monster...  ");
	}

	/* All done */
	roff("\n");

	/* Cheat -- know everything */
	if (((cheat_know) && (remem == 0)) || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
	{
		/* Hack -- restore memory */
		COPY(r_ptr, &save_mem, monster_race);
	}
}





/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
static void roff_top(int r_idx)
{
	monster_race    *r_ptr = &r_info[r_idx];

	byte            a1, a2;
	char            c1, c2;


	/* Access the chars */
	c1 = r_ptr->d_char;
	c2 = r_ptr->x_char;

	/* Access the attrs */
	a1 = r_ptr->d_attr;
	a2 = r_ptr->x_attr;

	/* Hack -- fake monochrome */
	if (!use_color) a1 = TERM_WHITE;
	if (!use_color) a2 = TERM_WHITE;


	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* A title (use "The" for non-uniques) */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		Term_addstr(-1, TERM_WHITE, "The ");
	}

	/* Dump the name */
	Term_addstr(-1, TERM_WHITE, (r_name + r_ptr->name));

	/* Append the "standard" attr/char info */
	Term_addstr(-1, TERM_WHITE, " ('");
	Term_addch(a1, c1);
	Term_addstr(-1, TERM_WHITE, "')");

	/* Append the "optional" attr/char info */
	Term_addstr(-1, TERM_WHITE, "/('");
	Term_addch(a2, c2);
	Term_addstr(-1, TERM_WHITE, "'):");
}



/*
 * Hack -- describe the given monster race at the top of the screen
 */
void screen_roff(int r_idx, int remember)
{
	/* Flush messages */
	msg_print(NULL);

	/* Begin recall */
	Term_erase(0, 1, 255);

	/* Recall monster */
	roff_aux(r_idx, remember);

	/* Describe monster */
	roff_top(r_idx);
}




/*
 * Hack -- describe the given monster race in the current "term" window
 */
void display_roff(int r_idx)
{
	int y;

	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Recall monster */
	roff_aux(r_idx, 0);

	/* Describe monster */
	roff_top(r_idx);
}


bool monster_quest(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Random quests are in the dungeon */
	if (!(r_ptr->flags8 & RF8_DUNGEON)) return FALSE;

	/* No random quests for aquatic monsters */
	if (r_ptr->flags7 & RF7_AQUATIC) return FALSE;

	/* No random quests for multiplying monsters */
	if (r_ptr->flags2 & RF2_MULTIPLY) return FALSE;

	/* No quests to kill friendly monsters */
	if (r_ptr->flags7 & RF7_FRIENDLY) return FALSE;

	/* No quests to kill pet monsters */
	if (r_ptr->flags7 & RF7_PET) return FALSE;

	/* No quests to kill unfair monsters if that option's off */
	if (!ironman_unfair_monsters && r_ptr->flags7 & RF7_UNFAIR) return FALSE;

	return TRUE;
}


bool monster_dungeon(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & RF8_DUNGEON)
		return TRUE;
	else
		return FALSE;
}


bool monster_ocean(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & RF8_WILD_OCEAN)
		return TRUE;
	else
		return FALSE;
}


bool monster_shore(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & RF8_WILD_SHORE)
		return TRUE;
	else
		return FALSE;
}


bool monster_waste(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & RF8_WILD_WASTE)
		return TRUE;
	else
		return FALSE;
}


bool monster_town(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & RF8_WILD_TOWN)
		return TRUE;
	else
		return FALSE;
}


bool monster_wood(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & RF8_WILD_WOOD)
		return TRUE;
	else
		return FALSE;
}


bool monster_volcano(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & RF8_WILD_VOLCANO)
		return TRUE;
	else
		return FALSE;
}


bool monster_mountain(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & RF8_WILD_MOUNTAIN)
		return TRUE;
	else
		return FALSE;
}


bool monster_grass(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & RF8_WILD_GRASS)
		return TRUE;
	else
		return FALSE;
}


bool monster_deep_water(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (!monster_dungeon(r_idx)) return FALSE;

	if (r_ptr->flags7 & RF7_AQUATIC)
		return TRUE;
	else
		return FALSE;
}


bool monster_shallow_water(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (!monster_dungeon(r_idx)) return FALSE;

	if (r_ptr->flags2 & RF2_AURA_FIRE)
		return FALSE;
	else if (r_ptr->flags7 & RF7_CAN_SWIM)
		return TRUE;
                
        return FALSE;
}

bool monster_swamp(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & RF8_WILD_SWAMP)
		return TRUE;
	else
		return FALSE;
}

bool monster_lava(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (!monster_dungeon(r_idx)) return FALSE;

	if (((r_ptr->flags3 & RF3_IM_FIRE) ||
	     (r_ptr->flags7 & RF7_CAN_FLY)) &&
	    !(r_ptr->flags3 & RF3_AURA_COLD))
		return TRUE;
	else
		return FALSE;
}

bool monster_rubble(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if ((r_ptr->flags2 & (RF2_PASS_WALL)) ||
		(r_ptr->flags2 & (RF2_KILL_WALL))) return(TRUE);

	if (r_ptr->flags2 & (RF2_SMART))
	{
		if (randint(2) == 1) return(FALSE);
	}
		else
	{
		if (randint(3) != 1) return(FALSE);
	}

	return (FALSE);
}

monster_hook_type get_monster_hook(void)
{
	if (!dun_level)
	{
		switch(wilderness[p_ptr->wilderness_y][p_ptr->wilderness_x].terrain)
		{
		case TERRAIN_TOWN:
			return monster_town;
		case TERRAIN_DEEP_WATER:
			return monster_ocean;
		case TERRAIN_SHALLOW_WATER:
			return monster_shore;
		case TERRAIN_DIRT:
			return monster_waste;
		case TERRAIN_GRASS:
			return monster_grass;
		case TERRAIN_TREES:
			return monster_wood;
		case TERRAIN_SHALLOW_LAVA:
		case TERRAIN_DEEP_LAVA:
			return monster_volcano;
		case TERRAIN_MOUNTAIN:
			return monster_mountain;
		case TERRAIN_SWAMP:
			return monster_swamp;
		default:
			return monster_dungeon;
		}
	}
	else
	{
		return monster_dungeon;
	}
}


monster_hook_type get_monster_hook2(int y, int x)
{
	/* Set the monster list */
	switch (cave[y][x].feat)
	{
	case FEAT_SHAL_WATER:
		return monster_shallow_water;
	case FEAT_DEEP_WATER:
		return monster_deep_water;
	case FEAT_DEEP_LAVA:
	case FEAT_SHAL_LAVA:
		return monster_lava;
	case FEAT_RUBBLE:
		return monster_rubble;
	default:
		return NULL;
	}
}

bool is_friendly(monster_type *m_ptr)
{
	if (m_ptr->smart & SM_FRIENDLY)
		return (TRUE);
	else
		return (FALSE);
}

void set_friendly(monster_type *m_ptr)
{
	m_ptr->smart |= SM_FRIENDLY;
}

/*
 * Is the monster friendly or a pet?
 */
bool is_hostile(monster_type *m_ptr)
{
	if (is_friendly(m_ptr) || is_pet(m_ptr))
		return (FALSE);
	else
		return (TRUE);
}


bool is_pet(monster_type *m_ptr)
{
	if (m_ptr->smart & (SM_PET))
	{
		return (TRUE);
	}
	else
	{
		return (FALSE);
	}
}

void set_pet(monster_type *m_ptr)
{
	m_ptr->smart |= SM_PET;
}

/*
 * Makes the monster hostile towards the player
 */
void set_hostile(monster_type *m_ptr)
{
	m_ptr->smart &= ~SM_PET;
	m_ptr->smart &= ~SM_FRIENDLY;
}

/*
 * Check if monster can cross terrain
 */
bool monster_can_cross_terrain(byte feat, monster_race *r_ptr)
{
	if (f_info[feat].flags1 & FF1_NO_WALK) return (FALSE);

	/* Deep water */
	if (feat == FEAT_DEEP_WATER)
	{
		if ((r_ptr->flags7 & RF7_AQUATIC) ||
		    (r_ptr->flags7 & RF7_CAN_FLY) ||
		    (r_ptr->flags7 & RF7_CAN_SWIM))
			return TRUE;
		else
			return FALSE;
	}
	/* Shallow water */
	else if (feat == FEAT_SHAL_WATER)
	{
		if (r_ptr->flags2 & RF2_AURA_FIRE)
			return FALSE;
		else
			return TRUE;
	}
	/* Aquatic monster */
	else if ((r_ptr->flags7 & RF7_AQUATIC) &&
		    !(r_ptr->flags7 & RF7_CAN_FLY))
	{
		return FALSE;
	}
	/* Lava */
	else if ((feat == FEAT_SHAL_LAVA) ||
	    (feat == FEAT_DEEP_LAVA))
	{
		if ((r_ptr->flags3 & RF3_IM_FIRE) ||
		    (r_ptr->flags7 & RF7_CAN_FLY))
			return TRUE;
		else
			return FALSE;
	}
	else if (feat == FEAT_RUBBLE) return (TRUE);

	return TRUE;
}

/*
 * Check if two monsters are enemies
 */
bool are_enemies(monster_type *m_ptr1, monster_type *m_ptr2)
{
	monster_race *r_ptr1 = &r_info[m_ptr1->r_idx];
	monster_race *r_ptr2 = &r_info[m_ptr2->r_idx];

        /* No fighting in town */
        
        if ((r_ptr1->level == 0) && (r_ptr2->level == 0))
                return FALSE;

	/* Never pet vs. pet, friendly vs. friendly, normal vs. normal */
	if ((is_friendly(m_ptr1) == is_friendly(m_ptr2)) &&
		(is_pet(m_ptr1) == is_pet(m_ptr2)))
	{
		return FALSE;
	}

	/* Pet vs. normal */
	if ((is_pet(m_ptr1) && !is_friendly(m_ptr2)) ||
	    (is_pet(m_ptr2) && !is_friendly(m_ptr1)))
	{
		return TRUE;
	}

	/* Friendly vs. opposite aligned normal or pet */
	if (((r_ptr1->flags3 & RF3_EVIL) &&
	     (r_ptr2->flags3 & RF3_GOOD)) ||
	    ((r_ptr1->flags3 & RF3_GOOD) &&
	     (r_ptr2->flags3 & RF3_EVIL)) ||
	    ((r_ptr1->flags2 & RF2_CHAOTIC) &&
	     (r_ptr2->flags2 & RF2_LAWFUL)) ||
	    ((r_ptr1->flags2 & RF2_LAWFUL) &&
	     (r_ptr2->flags2 & RF2_CHAOTIC)))
	{
			return TRUE;
	}

	/* Default */
	return FALSE;
}

/*
 * Is the monster "alive"?
 *
 * Used to determine the message to print for a killed monster.
 * ("dies", "destroyed")
 */
bool monster_living(monster_race *r_ptr)
{
	/* Non-living, undead, or demon */
	if (r_ptr->flags3 & (RF3_DEMON | RF3_UNDEAD | RF3_NONLIVING))
		return FALSE;
	else
		return TRUE;
}


/* Add various player ghost attributes depending on race. -LM- */
static void process_ghost_race(int ghost_race, int r_idx, monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[r_idx];
	int mlev = r_ptr->level;
	byte n;

	switch(ghost_race)
	{
		/* Human */
		case RACE_HUMAN:
		{
			/* No differences */
			break;
		}
		/* Half-Elf */
		case RACE_HALF_ELF:
		{
			if (r_ptr->freq_spell) r_ptr->freq_spell += 3;
			r_ptr->aaf += 2;
			r_ptr->hdice = 6 * r_ptr->hdice / 7;
			break;
		}
		/* Elf */
		case RACE_ELF:
		{
			if (r_ptr->freq_spell) r_ptr->freq_spell += 5;
			r_ptr->aaf += 4;
			r_ptr->hdice = 4 * r_ptr->hdice / 5;
			if (r_ptr->flags3 & (RF3_HURT_LITE))
				r_ptr->flags3 &= ~(RF3_HURT_LITE);
			break;
		}
		/* Hobbit */
		case RACE_HOBBIT:
		{
			r_ptr->hdice = 3 * r_ptr->hdice / 4;

			if (randint(3) == 1)
			{
				for (n = 0; n < 4; n++)
				{
					if (r_ptr->blow[n].effect == RBE_HURT)
					{
						if (randint(2) == 1)
							r_ptr->blow[n].effect = RBE_EAT_GOLD;
						else r_ptr->blow[n].effect = RBE_EAT_ITEM;

						r_ptr->blow[n].d_side =
							2 * r_ptr->blow[n].d_side / 3;
						break;
					}
				}
			}
			else
			{
				if (r_ptr->freq_spell == 0) r_ptr->freq_spell = 8;

				if (mlev <= 15) r_ptr->flags4 |= (RF4_ARROW_1);
				else if (mlev <= 30) r_ptr->flags4 |= (RF4_ARROW_2);
				else r_ptr->flags4 |= (RF4_ARROW_5);
			}

			break;
		}
		/* Gnome */
		case RACE_GNOME:
		{
			r_ptr->flags6 |= (RF6_BLINK);
			r_ptr->flags3 |= (RF3_NO_SLEEP);
			r_ptr->hdice = 4 * r_ptr->hdice / 5;
			break;
		}
		/* Dwarf */
		case RACE_DWARF:
		{
			r_ptr->hdice = 6 * r_ptr->hdice / 5;
			break;
		}
		/* Half-Orc */
		case RACE_HALF_ORC:
		{
			r_ptr->flags3 |= (RF3_ORC);
			break;
		}
		/* Half-Troll */
		case RACE_HALF_TROLL:
		{
			if (r_ptr->freq_spell > 5)
			r_ptr->freq_spell /= 2;

			r_ptr->flags3 |= (RF3_TROLL);

			r_ptr->hdice = 3 * r_ptr->hdice / 2;
			r_ptr->aaf -= 2;

			r_ptr->ac += r_ptr->level / 10 + 10;

			if (m_ptr->mspeed < 111) m_ptr->mspeed -= 2;
			else m_ptr->mspeed -= 4;

			for (n = 0; n < 4; n++)
			{
				r_ptr->blow[n].d_side = 4 * r_ptr->blow[n].d_side / 3;
			}

			break;
		}
		/* Dunadan */
		case RACE_DUNADAN:
		{
			r_ptr->ac += r_ptr->level / 10 + 5;

			for (n = 0; n < 4; n++)
			{
				if (randint(2) == 1)
					r_ptr->blow[n].d_side = 6 * r_ptr->blow[n].d_side / 5;
			}
			break;
		}
		/* High-Elf */
		case RACE_HIGH_ELF:
		{
			r_ptr->ac += r_ptr->level / 10 + 2;

			if (r_ptr->freq_spell) r_ptr->freq_spell += 8;
			r_ptr->aaf += 5;
			if (r_ptr->flags3 & (RF3_HURT_LITE))
				r_ptr->flags3 &= ~(RF3_HURT_LITE);
			break;
		}

		/* Ent */
		case RACE_ENT:
		{
			r_ptr->hdice = 4 * r_ptr->hdice / 3;
			if (r_ptr->flags3 & (RF3_IM_FIRE))
				r_ptr->flags3 &= ~(RF3_IM_FIRE);
			break;
		}
	}
}

/* Add various attributes player ghost depending on class. -LM- */
static void process_ghost_class(int ghost_class, int r_idx, monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[r_idx];
	int mlev = r_ptr->level;
	byte n;

	/* Note the care taken to make sure that all monsters that get spells
	 * can also cast them, since not all racial templates include spells.
	 */
	switch(ghost_class)
	{
		/* Warrior */
		case CLASS_WARRIOR:
		{
			if (r_ptr->freq_spell <= 10) r_ptr->freq_spell = 5;
			else r_ptr->freq_spell -= 5;

			r_ptr->hdice = 5 * r_ptr->hdice / 4;
			r_ptr->ac += r_ptr->level / 10 + 5;

			for (n = 0; n < 4; n++)
			{
				if (r_ptr->blow[n].effect != RBE_HURT)
				{
					r_ptr->blow[n].effect = RBE_HURT;

					r_ptr->blow[n].d_side = 3 * r_ptr->blow[n].d_side / 2;
					break;
				}
			}

			break;
		}
		/* Mage */
		case CLASS_MAGE:
		{
			if (r_ptr->freq_spell == 0) r_ptr->freq_spell = 12;
			else r_ptr->freq_spell += 10;

			if (mlev < 15) r_ptr->flags5 |= (RF5_MISSILE);
			else if (mlev >= 15) r_ptr->flags5 |= (RF5_BA_POIS);
			else if (mlev >= 25) r_ptr->flags5 |= (RF5_BA_ELEC);
			else if (mlev >= 35) r_ptr->flags5 |= (RF5_BA_COLD);
			else if (mlev >= 50) r_ptr->flags5 |= (RF5_BA_ACID);
			else if (mlev >= 75) r_ptr->flags5 |= (RF5_BA_MANA);

			if (mlev > 19) r_ptr->flags6 |= (RF6_HASTE);
			if (mlev > 39) m_ptr->mspeed += 5;
			if (m_ptr->mspeed > 130) m_ptr->mspeed = 130;

			r_ptr->flags6 |= (RF6_BLINK);
			if (mlev > 45) r_ptr->flags6 |= (RF6_TPORT);

			r_ptr->hdice = 2 * r_ptr->hdice / 3;

			for (n = 0; n < 4; n++)
			{
				if (randint(3) == 1) r_ptr->blow[n].d_side = 4 * r_ptr->blow[n].d_side / 5;
			}

			break;
		}
		/* Priest */
		case CLASS_PRIEST:
		{
			if (r_ptr->freq_spell == 0) r_ptr->freq_spell = 10;
			else r_ptr->freq_spell += 5;

			if (mlev < 15) r_ptr->flags5 |= (RF5_CAUSE_1);
			else if (mlev < 30) r_ptr->flags5 |= (RF5_CAUSE_2);
			else if (mlev < 45) r_ptr->flags5 |= (RF5_CAUSE_3);
			else if (mlev < 60) r_ptr->flags5 |= (RF5_CAUSE_4);
			else r_ptr->flags6 |= (RF6_S_MONSTERS);

			if (mlev > 20) r_ptr->flags6 |= (RF6_HEAL);

			r_ptr->hdice = 4 * r_ptr->hdice / 5;

			break;
		}
		/* Rogue */
		case CLASS_ROGUE:
		{
			if (r_ptr->freq_spell == 0) r_ptr->freq_spell = 8;

			if (mlev > 15) r_ptr->flags6 |= (RF6_HASTE);
			if (mlev > 35) m_ptr->mspeed += 5;
			if (m_ptr->mspeed > 130) m_ptr->mspeed = 130;

			r_ptr->hdice = 4 * r_ptr->hdice / 5;

			r_ptr->flags6 |= (RF6_TRAPS);

			for (n = 0; n < 4; n++)
			{
				if (r_ptr->blow[n].effect == RBE_HURT)
				{
					if (randint(2) == 1) r_ptr->blow[n].effect = RBE_EAT_GOLD;
					else r_ptr->blow[n].effect = RBE_EAT_ITEM;

					r_ptr->blow[n].d_side = 2 * r_ptr->blow[n].d_side / 3;
					break;
				}
			}

			break;
		}
		/* Ranger */
		case CLASS_RANGER:
		{
			if (r_ptr->freq_spell == 0) r_ptr->freq_spell = 8;

			if (mlev <= 15) r_ptr->flags4 |= (RF4_ARROW_1);
			else if (mlev <= 30) r_ptr->flags4 |= (RF4_ARROW_2);
			else r_ptr->flags4 |= (RF4_ARROW_5);

			r_ptr->flags6 |= (RF6_BLINK);

			break;
		}
		/* Paladin */
		case CLASS_PALADIN:
		{
			if (r_ptr->freq_spell == 0) r_ptr->freq_spell = 8;

			r_ptr->flags4 |= (RF4_SHRIEK);

			if (mlev <= 25) r_ptr->flags5 |= (RF5_CAUSE_1);
			else if (mlev <= 40) r_ptr->flags5 |= (RF5_CAUSE_2);
			else if (mlev <= 55) r_ptr->flags5 |= (RF5_CAUSE_3);
			else if (mlev <= 70) r_ptr->flags5 |= (RF5_CAUSE_4);

			r_ptr->flags3 |= (RF3_IM_FIRE | RF3_IM_COLD | RF3_IM_ELEC | RF3_IM_ACID);

			break;
		}
	}
}

static void process_ghost_mutations(u32b muta1, u32b muta2, u32b muta3, int r_idx, monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (muta1 & MUT1_SPIT_ACID) r_ptr->flags4 |= (RF4_BR_ACID);
	if (muta1 & MUT1_BR_FIRE) r_ptr->flags4 |= (RF4_BR_FIRE);
	if (muta1 & MUT1_HYPN_GAZE)
	{
	    int foo = rand_int(4);
	    (r_ptr->blow[foo].method = RBM_GAZE);
	    (r_ptr->blow[foo].effect = RBE_PARALYZE);
	}
	if (muta1 & MUT1_VTELEPORT) r_ptr->flags6 |= (RF6_TPORT);
	if (muta1 & MUT1_MIND_BLST) r_ptr->flags5 |= (RF5_MIND_BLAST);
	if (muta1 & MUT1_VAMPIRISM)
	{
	    int foo = rand_int(4);
	    (r_ptr->blow[foo].method = RBM_HIT);
	    (r_ptr->blow[foo].effect = RBE_EXP_10 + rand_int(4));
	}
	if (muta1 & MUT1_BLINK) r_ptr->flags6 |= (RF6_BLINK);
	if (muta1 & MUT1_EAT_ROCK) r_ptr->flags2 |= (RF2_KILL_WALL);
	if (muta1 & MUT1_SHRIEK) r_ptr->flags4 |= (RF4_SHRIEK);
	if (muta1 & MUT1_BERSERK) r_ptr->hdice = r_ptr->hdice * 4 / 3;
	if (muta1 & MUT1_POLYMORPH) r_ptr->flags1 |= (RF1_CHAR_MULTI);
	if ((muta1 & MUT1_MIDAS_TCH) || (muta1 & MUT1_PANIC_HIT))
	{
	    int foo = rand_int(4);
	    (r_ptr->blow[foo].method = RBM_TOUCH);
	    (r_ptr->blow[foo].effect = RBE_EAT_ITEM);
	}
	if (muta1 & MUT1_POLYMORPH)
	{
	    r_ptr->flags3 |= (RF3_IM_FIRE);
	    r_ptr->flags3 |= (RF3_IM_COLD);
	    r_ptr->flags3 |= (RF3_IM_ELEC);
	    r_ptr->flags3 |= (RF3_IM_ACID);
	}
	if (muta1 & MUT1_EAT_MAGIC)
	{
	    int foo = rand_int(4);
	    (r_ptr->blow[foo].method = RBM_TOUCH);
	    (r_ptr->blow[foo].effect = RBE_UN_BONUS);
	}
	if (muta1 & MUT1_EARTHQUAKE)
	{
	    int foo = rand_int(4);
	    (r_ptr->blow[foo].method = RBM_HIT);
	    (r_ptr->blow[foo].effect = RBE_SHATTER);
	}
	if (muta1 & MUT1_COLD_TOUCH)
	{
	    int foo = rand_int(4);
	    (r_ptr->blow[foo].method = RBM_TOUCH);
	    (r_ptr->blow[foo].effect = RBE_COLD);
	}

	if (muta2 & MUT2_COWARDICE)
	{
	    if (r_ptr->flags3 & RF3_NO_FEAR)
		r_ptr->flags3 &= ~(RF3_NO_FEAR);
	}
	if ((muta2 & MUT2_ALCOHOL) || (muta2 & MUT2_HALLU))
	{
	    if (r_ptr->flags3 & RF3_NO_CONF)
		r_ptr->flags3 &= ~(RF3_NO_CONF);
	}
	if (muta2 & MUT2_ATT_DEMON) r_ptr->flags6 |= (RF6_S_DEMON);
	if (muta2 & MUT2_ATT_ANIMAL) r_ptr->flags6 |= (RF6_S_MONSTERS);
	if (muta2 & MUT2_ATT_DRAGON) r_ptr->flags6 |= (RF6_S_DRAGON);
	if (muta2 & MUT2_WEIRD_MIND) r_ptr->flags2 |= (RF2_WEIRD_MIND);
	if (muta2 & MUT2_WRAITH) r_ptr->flags2 |= (RF2_PASS_WALL);
	if (muta3 & MUT3_XTRA_LEGS) m_ptr->mspeed += 3 + randint (3);
	if (muta3 & MUT3_SHORT_LEG) m_ptr->mspeed -= 3 + randint (3);
	if (muta3 & MUT3_ELEC_TOUC) r_ptr->flags2 |= (RF2_AURA_ELEC);
	if (muta3 & MUT3_FIRE_BODY) r_ptr->flags2 |= (RF2_AURA_FIRE);
	if (muta3 & MUT3_WINGS) r_ptr->flags7 |= (RF7_CAN_FLY);
	if (muta3 & MUT3_ESP) r_ptr->aaf += 20; /* Simulates ESP */
	if (muta3 & MUT3_MOTION) r_ptr->flags3 |= (RF3_NO_STUN);

}

/*
 * Once a monster with the flag "PLAYER_GHOST" is generated, it needs to have
 * a little color added, if it hasn't been prepared before.  This function uses
 * a bones file to get a name, give the ghost a gender, and add a few features
 * depending on the race and class of the slain adventurer.  -LM-
 */
bool prepare_ghost(int r_idx, monster_type *m_ptr, bool from_savefile)
{
	monster_race    *r_ptr = &r_info[r_idx];
	int             r_ghost;

	u32b            muta1, muta2, muta3 = 0;
	int             ghost_sex, ghost_race, ghost_class, temphp;
	byte            try, i, backup_file_selector;

	bool            broken = FALSE;
	FILE            *fp;
	char            path[1024];

	/* Paranoia. */
	if (!(r_ptr->flags2 & (RF2_PLAYER_GHOST))) return (TRUE);

	/* Hack -- If the ghost has a sex, then it must already have been prepared. */
	if ((r_ptr->flags1 & RF1_MALE) || (r_ptr->flags1 & RF1_FEMALE)) return (TRUE);

	/* Hack -- No easy player ghosts, unless the ghost is from a savefile.
	 * This also makes player ghosts much rarer, and effectively (almost)
	 * prevents more than one from being on a level.
	 */
	if ((r_ptr->level < dun_level - 5) && (from_savefile == FALSE))

	/* Store the index of the base race. */
	r_ghost = r_idx;

	/* Choose a bones file.  Use the variable bones_selector if it has any
	 * information in it (this allows saved ghosts to reacquire all special
	 * features), then use the current depth, and finally pick at random.
	 */
	for (try = 0; try < 40; ++try)
	{
		/* Prepare a path, and store the file number for future reference. */
		if (try == 0)
		{
			if (bones_selector)
			{
				sprintf(path, "%s/bone.%03d", ANGBAND_DIR_BONE, bones_selector);
			}
			else
			{
				sprintf(path, "%s/bone.%03d", ANGBAND_DIR_BONE, dun_level);
				bones_selector = dun_level;
			}
		}
		else
		{
			backup_file_selector = randint(MAX_DEPTH - 1);
			sprintf(path, "%s/bone.%03d", ANGBAND_DIR_BONE, backup_file_selector);
			bones_selector = backup_file_selector;
		}

		/* Attempt to open the bones file. */
		fp = my_fopen(path, "r");

		/* No bones file with that number, try again. */
		if (!fp)
		{
			bones_selector = 0;
			continue;
		}

		/* Success. */
		if (fp) break;
	}

	/* No bones file found, so no Ghost is made. */
	if (!fp) return (FALSE);

	/* XXX XXX XXX Scan the file (new style) */
	broken = (fscanf(fp, "%[^\n]\n%d\n%d\n%d\n%d\n%ld\n%ld\n%ld\n", ghost_name,
			&temphp, &ghost_race, &ghost_class, &ghost_sex,
			&muta1, &muta2, &muta3) != 8);

	r_ptr->hdice = temphp / 5; /* For lack of a better approx */
	r_ptr->hside = 5;

	/* Hack -- broken file */
	if (broken) return (FALSE);

	/* Close the file */
	my_fclose(fp);


	/*** Process the ghost name and store it in a global variable. ***/

	/* XXX XXX XXX Find the first comma, or end of string */
	for (i = 0; (i < 16) && (ghost_name[i]) && (ghost_name[i] != ','); i++);

	/* Terminate the name */
	ghost_name[i] = '\0';

	/* Force a name */
	if (!ghost_name[0]) strcpy(ghost_name, "Nobody");

	/* Capitalize the name */
	if (islower(ghost_name[0])) ghost_name[0] = toupper(ghost_name[0]);


	/*** Process sex. ***/

	/* Sanity check. */
	if ((ghost_sex >= MAX_SEXES) || (ghost_class < 0)) ghost_sex = rand_int(MAX_SEXES);

	/* And use that number to toggle on either the male or the female flag. */
	if (ghost_sex == 0) r_ptr->flags1 |= (RF1_FEMALE);
	if (ghost_sex == 1) r_ptr->flags1 |= (RF1_MALE);


	/*** Process race. ***/

	/* Sanity check. */
	if (ghost_race >= MAX_RACES) ghost_race = rand_int(MAX_RACES);

	/* And use the ghost race to gain some flags. */
	process_ghost_race(ghost_race, r_idx, m_ptr);

	/*** Process class. ***/

	/* Sanity check. */
	if (ghost_class >= MAX_CLASS) ghost_class = rand_int(MAX_CLASS);

	/* And use the ghost class to gain some flags. */
	process_ghost_class(ghost_class, r_idx, m_ptr);

	/* And use the mutations to gain some flags. */
	process_ghost_mutations(muta1, muta2, muta3, r_idx, m_ptr);

	/* Hack -- increase the level feeling */
	rating += 10;

	/* A ghost makes the level special */
	good_item_flag = TRUE;

	/* Hack - Player ghosts are "seen" whenever generated, to conform with
	 * previous practice.
	 */
	if (from_savefile == FALSE) r_ptr->r_sights = 1;

	/* Success */
	return (TRUE);
}

/*
 * Swap the players/monsters (if any) at two locations XXX XXX XXX
 */
void monster_swap(int y1, int x1, int y2, int x2)
{
	int m1, m2;

	monster_type *m_ptr;


	/* Monsters */
	m1 = cave[y1][x1].m_idx;
	m2 = cave[y2][x2].m_idx;


	/* Update grids */
	cave[y1][x1].m_idx = m2;
	cave[y2][x2].m_idx = m1;


	/* Monster 1 */
	if (m1 > 0)
	{
		m_ptr = &m_list[m1];

		/* Move monster */
		m_ptr->fy = y2;
		m_ptr->fx = x2;

		/* Update monster */
		update_mon(m1, TRUE);
	}

	/* Player 1 */
	else if (m1 < 0)
	{
		/* Move player */
		py = y2;
		px = x2;

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);
	}

	/* Monster 2 */
	if (m2 > 0)
	{
		m_ptr = &m_list[m2];

		/* Move monster */
		m_ptr->fy = y1;
		m_ptr->fx = x1;

		/* Update monster */
		update_mon(m2, TRUE);
	}

	/* Player 2 */
	else if (m2 < 0)
	{
		/* Move player */
		py = y1;
		px = x1;

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);
	}


	/* Redraw */
	lite_spot(y1, x1);
	lite_spot(y2, x2);
}

