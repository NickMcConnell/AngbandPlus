/* File: monster1.c */

/* Purpose: describe monsters (using monster memory) */

/*
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

	u32b		flags1;
	u32b		flags2;
	u32b		flags3;
	u32b		flags4;
	u32b		flags5;
	u32b		flags6;
        u32b            flags7;
        u32b            flags8;
        u32b            flags9;

	int		vn = 0;
        byte            color[64];
	cptr		vp[64];

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
        if (cheat_know || p_ptr->pclass == CLASS_MONSTER_MAGE || p_ptr->pclass == CLASS_LEADER ||
         p_ptr->pclass == CLASS_COMMANDER || p_ptr->prace == RACE_MONSTER)
	{
		/* XXX XXX XXX */

		/* Save the "old" memory */
		save_mem = *r_ptr;

		/* Hack -- Maximal kills */
                if (cheat_know) r_ptr->r_tkills = MAX_SHORT;

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
		if (r_ptr->flags3 & (RF3_ANIMAL)) flags3 |= (RF3_ANIMAL);
                if (r_ptr->flags3 & (RF3_DRAGONRIDER)) flags3 |= (RF3_DRAGONRIDER);
                if (r_ptr->flags3 & (RF3_DARKLORD)) flags3 |= (RF3_DARKLORD);

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


	/* Nothing yet */
	old = FALSE;

	/* Describe location */
        if (r_ptr->flags7 & RF7_PET)
	{
                roff(format("%^s is friendly to you. ", wd_he[msex]));
		old = TRUE;
	}

	/* Describe location */
        if (r_ptr->level == 0 && !(r_ptr->flags9 & RF9_SPECIAL_GENE))
	{
		roff(format("%^s lives in the town", wd_he[msex]));
		old = TRUE;
	}
	/* Describe location */
        else if (r_ptr->flags9 & RF9_SPECIAL_GENE)
	{
                roff(format("This creature normally never appear"));
		old = TRUE;
	}

	else if (r_ptr->r_tkills)
	{
		if (depth_in_feet)
		{
			roff(format("%^s is normally found at depths of %d feet",
			            wd_he[msex], r_ptr->level * 50));
		}
		else
		{
			roff(format("%^s is normally found on dungeon level %d",
			            wd_he[msex], r_ptr->level));
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
                        c_roff(TERM_L_RED, " quickly");
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
		if (flags3 & (RF3_EVIL))            roff(" evil");
		if (flags3 & (RF3_GOOD))            roff(" good");
                if (flags3 & (RF3_UNDEAD))          c_roff(TERM_VIOLET, " undead");

		/* Describe the "race" */
		if (flags3 & (RF3_DRAGON))          roff(" dragon");
                else if (flags3 & (RF3_DEMON))      c_roff(TERM_VIOLET, " demon");
		else if (flags3 & (RF3_GIANT))      roff(" giant");
		else if (flags3 & (RF3_TROLL))      roff(" troll");
		else if (flags3 & (RF3_ORC))        roff(" orc");
                else if (flags3 & (RF3_DRAGONRIDER))roff(" DragonRider");
                else if (flags3 & (RF3_DARKLORD))        roff(" Dark Lord");
		else                                roff(" creature");

		/* Group some variables */
		if (TRUE)
		{
			long i, j;

			/* calculate the integer exp part */
                        i = ((long)r_ptr->mexp * r_ptr->level / p_ptr->lev) / 10;

			/* calculate the fractional exp part scaled by 100, */
			/* must use long arithmetic to avoid overflow  */
                        j = (((((long)r_ptr->mexp * r_ptr->level % p_ptr->lev) / 10) *
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

	if ((flags2 & (RF2_AURA_FIRE)) && (flags2 & (RF2_AURA_ELEC)))
	{
                c_roff(TERM_VIOLET, format("%^s is surrounded by flames and electricity.  ", wd_he[msex]));
	}
	else if (flags2 & (RF2_AURA_FIRE))
	{
                c_roff(TERM_RED, format("%^s is surrounded by flames.  ", wd_he[msex]));
	}
	else if (flags2 & (RF2_AURA_ELEC))
	{
                c_roff(TERM_L_BLUE, format("%^s is surrounded by electricity.  ", wd_he[msex]));
	}

	if (flags2 & (RF2_REFLECTING))
	{
		roff(format("%^s reflects bolt spells.  ", wd_he[msex]));
	}


	/* Describe escorts */
	if ((flags1 & (RF1_ESCORT)) || (flags1 & (RF1_ESCORTS)))
	{
		roff(format("%^s usually appears with escorts.  ",
		            wd_he[msex]));
	}

	/* Describe friends */
	else if ((flags1 & (RF1_FRIEND)) || (flags1 & (RF1_FRIENDS)))
	{
		roff(format("%^s usually appears in groups.  ",
		            wd_he[msex]));
	}


	/* Collect inate attacks */
	vn = 0;
	if (flags4 & (RF4_SHRIEK))		vp[vn++] = "shriek for help";
	if (flags4 & (RF4_XXX3))		vp[vn++] = "do something";
	if (flags4 & (RF4_ROCKET))		vp[vn++] = "shoot a rocket";
	if (flags4 & (RF4_ARROW_1))		vp[vn++] = "fire an arrow";
	if (flags4 & (RF4_ARROW_2))		vp[vn++] = "fire arrows";
	if (flags4 & (RF4_ARROW_3))		vp[vn++] = "fire a missile";
	if (flags4 & (RF4_ARROW_4))		vp[vn++] = "fire missiles";

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
	if (flags4 & (RF4_BR_NUKE))		vp[vn++] = "toxic waste";
	if (flags4 & (RF4_BR_DISI))		vp[vn++] = "disintegration";

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
	if (flags5 & (RF5_BA_MANA))         vp[vn++] = "invoke mana storms";
	if (flags5 & (RF5_BA_DARK))         vp[vn++] = "invoke darkness storms";
        if (flags4 & (RF4_BA_CHAO))         vp[vn++] = "invoke raw chaos";
	if (flags6 & (RF6_HAND_DOOM))       vp[vn++] = "invoke the Hand of Doom";
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
	if (flags6 & (RF6_XXX2))            vp[vn++] = "do something";
	if (flags6 & (RF6_BLINK))           vp[vn++] = "blink-self";
	if (flags6 & (RF6_TPORT))           vp[vn++] = "teleport-self";
        if (flags6 & (RF6_S_BUG))           vp[vn++] = "summon software bugs";
        if (flags6 & (RF6_S_RNG))           vp[vn++] = "summon RNG";
	if (flags6 & (RF6_TELE_TO))         vp[vn++] = "teleport to";
	if (flags6 & (RF6_TELE_AWAY))       vp[vn++] = "teleport away";
	if (flags6 & (RF6_TELE_LEVEL))      vp[vn++] = "teleport level";
        if (flags6 & (RF6_S_DRAGONRIDER))   vp[vn++] = "summon a DragonRider";
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
        if (flags6 & (RF6_S_WRAITH))        vp[vn++] = "summon Ringwraith";
	if (flags6 & (RF6_S_UNIQUE))        vp[vn++] = "summon Unique Monsters";

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
		}

		/* Variable hitpoints */
		else
		{
			roff(format(" and a life rating of %dd%d.  ",
			            r_ptr->hdice, r_ptr->hside));
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
	}


	/* Describe special abilities. */
	if (flags2 & (RF2_INVISIBLE))
	{
		roff(format("%^s is invisible.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_COLD_BLOOD))
	{
		roff(format("%^s is cold blooded.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_EMPTY_MIND))
	{
		roff(format("%^s is not detected by telepathy.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_WEIRD_MIND))
	{
		roff(format("%^s is rarely detected by telepathy.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_MULTIPLY))
	{
                c_roff(TERM_L_UMBER, format("%^s breeds explosively.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_REGENERATE))
	{
                c_roff(TERM_L_WHITE, format("%^s regenerates quickly.  ", wd_he[msex]));
	}
        if (r_ptr->flags7 & (RF7_SEDUCE_MALES))
	{
                c_roff(TERM_L_GREEN, format("%^s is so attractive, no males can resist her beauty.  ", wd_he[msex]));
	}
        if (r_ptr->flags7 & (RF7_SEDUCE_FEMALES))
	{
                c_roff(TERM_L_GREEN, format("%^s is so handsome, any females would be charmed.  ", wd_he[msex]));
	}

        if (r_ptr->flags7 & (RF7_MORTAL))
	{
                c_roff(TERM_RED, format("%^s is a mortal being.  ", wd_he[msex]));
	}
        else
	{
                c_roff(TERM_L_BLUE, format("%^s is an immortal being.  ", wd_he[msex]));
	}


	/* Collect susceptibilities */
	vn = 0;
        if (flags3 & (RF3_HURT_ROCK)) {vp[vn++] = "rock remover"; color[vn - 1] = TERM_UMBER;}
        if (flags3 & (RF3_HURT_LITE)) {vp[vn++] = "bright light"; color[vn - 1] = TERM_YELLOW;}
        if (flags3 & (RF3_SUSCEP_FIRE)) {vp[vn++] = "fire"; color[vn - 1] = TERM_RED;}
        if (flags3 & (RF3_SUSCEP_COLD)) {vp[vn++] = "cold"; color[vn - 1] = TERM_L_WHITE;}
        if (flags9 & (RF9_SUSCEP_ACID)) {vp[vn++] = "acid"; color[vn - 1] = TERM_GREEN;}
        if (flags9 & (RF9_SUSCEP_ELEC)) {vp[vn++] = "lightning"; color[vn - 1] = TERM_L_BLUE;}
        if (flags9 & (RF9_SUSCEP_POIS)) {vp[vn++] = "poison"; color[vn - 1] = TERM_L_GREEN;}

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
	}


	/* Collect immunities */
	vn = 0;
        if (flags3 & (RF3_IM_ACID)) {vp[vn++] = "acid"; color[vn - 1] = TERM_GREEN;}
        if (flags3 & (RF3_IM_ELEC)) {vp[vn++] = "lightning"; color[vn - 1] = TERM_L_BLUE;}
        if (flags3 & (RF3_IM_FIRE)) {vp[vn++] = "fire"; color[vn - 1] = TERM_RED;}
        if (flags3 & (RF3_IM_COLD)) {vp[vn++] = "cold"; color[vn - 1] = TERM_L_WHITE;}
        if (flags3 & (RF3_IM_POIS)) {vp[vn++] = "poison"; color[vn - 1] = TERM_L_GREEN;}

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
	}


	/* Collect resistances */
	vn = 0;
	if (flags3 & (RF3_RES_NETH)) vp[vn++] = "nether";
	if (flags3 & (RF3_RES_WATE)) vp[vn++] = "water";
	if (flags3 & (RF3_RES_PLAS)) vp[vn++] = "plasma";
	if (flags3 & (RF3_RES_NEXU)) vp[vn++] = "nexus";
	if (flags3 & (RF3_RES_DISE)) vp[vn++] = "disenchantment";
	if (flags3 & (RF3_RES_TELE)) vp[vn++] = "teleportation";

	/* Describe resistances */
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
			roff(vp[n]);
		}

		/* End */
		roff(".  ");
	}


	/* Collect non-effects */
	vn = 0;
	if (flags3 & (RF3_NO_STUN)) vp[vn++] = "stunned";
	if (flags3 & (RF3_NO_FEAR)) vp[vn++] = "frightened";
	if (flags3 & (RF3_NO_CONF)) vp[vn++] = "confused";
	if (flags3 & (RF3_NO_SLEEP)) vp[vn++] = "slept";

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
			case RBM_HIT:		p = "hit"; break;
			case RBM_TOUCH:		p = "touch"; break;
			case RBM_PUNCH:		p = "punch"; break;
			case RBM_KICK:		p = "kick"; break;
			case RBM_CLAW:		p = "claw"; break;
			case RBM_BITE:		p = "bite"; break;
			case RBM_STING:		p = "sting"; break;
			case RBM_XXX1:		break;
			case RBM_BUTT:		p = "butt"; break;
			case RBM_CRUSH:		p = "crush"; break;
			case RBM_ENGULF:	p = "engulf"; break;
			case RBM_CHARGE: 	p = "charge";   break;
			case RBM_CRAWL:		p = "crawl on you"; break;
			case RBM_DROOL:		p = "drool on you"; break;
			case RBM_SPIT:		p = "spit"; break;
			case RBM_EXPLODE:	p = "explode"; break;
			case RBM_GAZE:		p = "gaze"; break;
			case RBM_WAIL:		p = "wail"; break;
			case RBM_SPORE:		p = "release spores"; break;
			case RBM_XXX4:		break;
			case RBM_BEG:		p = "beg"; break;
			case RBM_INSULT:	p = "insult"; break;
			case RBM_MOAN:		p = "moan"; break;
			case RBM_SHOW:  	p = "sing"; break;
		}


		/* Default effect */
		q = NULL;

		/* Acquire the effect */
		switch (effect)
		{
			case RBE_HURT:    	q = "attack"; break;
			case RBE_POISON:  	q = "poison"; break;
			case RBE_UN_BONUS:	q = "disenchant"; break;
			case RBE_UN_POWER:	q = "drain charges"; break;
			case RBE_EAT_GOLD:	q = "steal gold"; break;
			case RBE_EAT_ITEM:	q = "steal items"; break;
			case RBE_EAT_FOOD:	q = "eat your food"; break;
			case RBE_EAT_LITE:	q = "absorb light"; break;
			case RBE_ACID:    	q = "shoot acid"; break;
			case RBE_ELEC:    	q = "electrocute"; break;
			case RBE_FIRE:    	q = "burn"; break;
			case RBE_COLD:    	q = "freeze"; break;
			case RBE_BLIND:   	q = "blind"; break;
			case RBE_CONFUSE: 	q = "confuse"; break;
			case RBE_TERRIFY: 	q = "terrify"; break;
			case RBE_PARALYZE:	q = "paralyze"; break;
			case RBE_LOSE_STR:	q = "reduce strength"; break;
			case RBE_LOSE_INT:	q = "reduce intelligence"; break;
			case RBE_LOSE_WIS:	q = "reduce wisdom"; break;
			case RBE_LOSE_DEX:	q = "reduce dexterity"; break;
			case RBE_LOSE_CON:	q = "reduce constitution"; break;
			case RBE_LOSE_CHR:	q = "reduce charisma"; break;
			case RBE_LOSE_ALL:	q = "reduce all stats"; break;
			case RBE_SHATTER:	q = "shatter"; break;
			case RBE_EXP_10:	q = "lower experience (by 10d6+)"; break;
			case RBE_EXP_20:	q = "lower experience (by 20d6+)"; break;
			case RBE_EXP_40:	q = "lower experience (by 40d6+)"; break;
			case RBE_EXP_80:	q = "lower experience (by 80d6+)"; break;
			case RBE_DISEASE:	q = "disease"; break;
                        case RBE_TIME:          q = "time"; break;
                        case RBE_SANITY:        q = "blast sanity"; break;
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

                                /* Know the exact amount of damages you do... */
                                /* if (p_ptr->body_monster == r_idx)
                                {
                                        c_roff(TERM_L_GREEN, format(" %dd%d", d1 + p_ptr->lev / 2, d2 + p_ptr->lev / 2));
                                }
                                else roff(format(" %dd%d", d1, d2)); */

                                roff(format(" %dd%d", d1, d2));
			}
		}


		/* Count the attacks as printed */
		r++;
	}

	/* Finish sentence above */
	if (r)
	{
		roff(".  ");
	}

	/* Notice lack of attacks */
	else if (flags1 & (RF1_NEVER_BLOW))
	{
		roff(format("%^s has no physical attacks.  ", wd_he[msex]));
	}

	/* Or describe the lack of knowledge */
	else
	{
		roff(format("Nothing is known about %s attack.  ", wd_his[msex]));
	}


	/* Notice "Quest" monsters */
	if (flags1 & RF1_QUESTOR)
	{
                c_roff(TERM_VIOLET, "You feel an intense desire to kill this monster...  ");
	}


	/* All done */
	roff("\n");

	/* Cheat -- know everything */
	if ((cheat_know) && (remem == 0))
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
	monster_race	*r_ptr = &r_info[r_idx];

	byte		a1, a2;
	char		c1, c2;


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
	else
		return TRUE;
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


void set_mon_num_hook(void)
{
	if (!dun_level)
	{
                switch(wf_info[wild_map[p_ptr->wilderness_y][p_ptr->wilderness_x].feat].terrain_idx)
		{
		case TERRAIN_TOWN:
			get_mon_num_hook = monster_town;
			break;
		case TERRAIN_DEEP_WATER:
			get_mon_num_hook = monster_ocean;
			break;
		case TERRAIN_SHALLOW_WATER:
			get_mon_num_hook = monster_shore;
			break;
		case TERRAIN_DIRT:
			get_mon_num_hook = monster_waste;
			break;
		case TERRAIN_GRASS:
			get_mon_num_hook = monster_grass;
			break;
		case TERRAIN_TREES:
			get_mon_num_hook = monster_wood;
			break;
		case TERRAIN_SHALLOW_LAVA:
		case TERRAIN_DEEP_LAVA:
			get_mon_num_hook = monster_volcano;
			break;
		case TERRAIN_MOUNTAIN:
			get_mon_num_hook = monster_mountain;
			break;
		default:
			get_mon_num_hook = monster_dungeon;
			break;
		}
	}
	else
	{
		get_mon_num_hook = monster_dungeon;
	}
}


void set_mon_num2_hook(int y, int x)
{
	/* Set the monster list */
	switch (cave[y][x].feat)
	{
	case FEAT_SHAL_WATER:
		get_mon_num2_hook = monster_shallow_water;
		break;
	case FEAT_DEEP_WATER:
		get_mon_num2_hook = monster_deep_water;
		break;
	case FEAT_DEEP_LAVA:
	case FEAT_SHAL_LAVA:
		get_mon_num2_hook = monster_lava;
		break;
	default:
		get_mon_num2_hook = NULL;
		break;
	}
}


bool is_pet(monster_type *m_ptr)
{
	if (m_ptr->smart & (SM_FRIEND))
	{
		return (TRUE);
	}
	else
	{
		return (FALSE);
	}
}

void set_pet(monster_type *m_ptr, bool pet)
{
	if (pet)
	{
		m_ptr->smart |= SM_FRIEND;
	}
	else
	{
		m_ptr->smart &= ~SM_FRIEND;
	}
}

/*
 * Check if monster can cross terrain
 */
bool monster_can_cross_terrain(byte feat, monster_race *r_ptr)
{
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
	
	return TRUE;
}

void info_boss_abilities(monster_type *m_ptr)
{
                roff("----- ABILITIES -----");
                roff("\n");
                if (m_ptr->abilities & (BOSS_IMMUNE_WEAPONS))
                {
                        c_roff(TERM_L_BLUE, "Immune To Weapons/Physical attacks");
                        roff("\n");
                }
                if (m_ptr->abilities & (BOSS_IMMUNE_MAGIC))
                {
                        c_roff(TERM_L_BLUE, "Immune To Magical attacks");
                        roff("\n");
                }
                if (m_ptr->abilities & (BOSS_DOUBLE_DAMAGES))
                {
                        c_roff(TERM_L_BLUE, "Does double damages with physical attacks");
                        roff("\n");
                }
                if (m_ptr->abilities & (BOSS_HALVE_DAMAGES))
                {
                        c_roff(TERM_L_BLUE, "50% resistance to all damages(Physical or Magical)");
                        roff("\n");
                }
                if (m_ptr->abilities & (BOSS_CURSED_HITS))
                {
                        c_roff(TERM_L_BLUE, "Monster's attacks scare, blind and confuse");
                        roff("\n");
                }
                if (m_ptr->abilities & (BOSS_DOUBLE_MAGIC))
                {
                        c_roff(TERM_L_BLUE, "Does double damages with magical attacks");
                        roff("\n");
                }
                if (m_ptr->abilities & (BOSS_RETURNING))
                {
                        c_roff(TERM_L_BLUE, "Returns 50% physical damages to the player");
                        roff("\n");
                }
                if (m_ptr->abilities & (BOSS_MAGIC_RETURNING))
                {
                        c_roff(TERM_L_BLUE, "Returns 50% magic damages to the player");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_LOWER_POWER))
                {
                        c_roff(TERM_ORANGE, "Cursed with Lower Power.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_LOWER_MAGIC))
                {
                        c_roff(TERM_ORANGE, "Cursed with Lower Magic.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_HALVE_HP))
                {
                        c_roff(TERM_ORANGE, "Cursed with Life Blast.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_LOCK))
                {
                        c_roff(TERM_ORANGE, "Cursed with Lock.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_SLOW_DOWN))
                {
                        c_roff(TERM_ORANGE, "Cursed with Slow Down.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_HALVE_SPEED))
                {
                        c_roff(TERM_ORANGE, "Cursed with Speed Sap.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_HALVE_DAMAGES))
                {
                        c_roff(TERM_ORANGE, "Cursed with Halve Damages.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_HALVE_MAGIC))
                {
                        c_roff(TERM_ORANGE, "Cursed with Halve Magic.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_HALVE_LEVEL))
                {
                        c_roff(TERM_ORANGE, "Cursed with Halve Level.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_DAMAGES_CURSE))
                {
                        c_roff(TERM_ORANGE, "Cursed with Damages Curse.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_FRAILNESS))
                {
                        c_roff(TERM_ORANGE, "Cursed with Frailness.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_INEPTITUDE))
                {
                        c_roff(TERM_ORANGE, "Cursed with Ineptitude.");
                        roff("\n");
                }
                if (m_ptr->abilities & (EYE_STABBED))
                {
                        c_roff(TERM_ORANGE, "Blinded by Eye Stab ability.");
                        roff("\n");
                }
                if (m_ptr->abilities & (MUTILATE_LEGS))
                {
                        c_roff(TERM_ORANGE, "Mutilated legs(can't move).");
                        roff("\n");
                }
                if (m_ptr->abilities & (MUTILATE_ARMS))
                {
                        c_roff(TERM_ORANGE, "Mutilated arms(can't attack).");
                        roff("\n");
                }
                if (m_ptr->abilities & (PSYCHIC_HITRATE))
                {
                        c_roff(TERM_ORANGE, "Blinded by fearful illusions.");
                        roff("\n");
                }
                if (m_ptr->abilities & (WAR_BLESSED))
                {
                        c_roff(TERM_YELLOW, "Blessed with War Blessing.");
                        roff("\n");
                }
                if (m_ptr->abilities & (MORALE_BOOST))
                {
                        c_roff(TERM_YELLOW, "Morale Boost.");
                        roff("\n");
                }

}        


void screen_roff_boss(int r_idx, int remember, monster_type *m_ptr)
{
	/* Flush messages */
	msg_print(NULL);

	/* Begin recall */
	Term_erase(0, 1, 255);

	/* Recall monster */
        roff_aux_boss(r_idx, remember, m_ptr);

	/* Describe monster */
	roff_top(r_idx);
}

void roff_aux_boss(int r_idx, int remem, monster_type *m_ptr)
{
	monster_race    *r_ptr;

	bool            old = FALSE;
	bool            sin = FALSE;

	int             m, n, r;

	cptr            p, q;

	int             msex = 0;

	bool            breath = FALSE;
	bool            magic = FALSE;

	u32b		flags1;
	u32b		flags2;
	u32b		flags3;
	u32b		flags4;
	u32b		flags5;
	u32b		flags6;
        u32b            flags7;
        u32b            flags8;
        u32b            flags9;

	int		vn = 0;
        byte            color[64];
	cptr		vp[64];

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
        if (cheat_know || p_ptr->pclass == CLASS_MONSTER_MAGE || p_ptr->pclass == CLASS_LEADER ||
         p_ptr->pclass == CLASS_COMMANDER || p_ptr->prace == RACE_MONSTER)
	{
		/* XXX XXX XXX */

		/* Save the "old" memory */
		save_mem = *r_ptr;

		/* Hack -- Maximal kills */
                if (cheat_know) r_ptr->r_tkills = MAX_SHORT;

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
		if (r_ptr->flags3 & (RF3_ANIMAL)) flags3 |= (RF3_ANIMAL);
                if (r_ptr->flags3 & (RF3_DRAGONRIDER)) flags3 |= (RF3_DRAGONRIDER);
                if (r_ptr->flags3 & (RF3_DARKLORD)) flags3 |= (RF3_DARKLORD);

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


	/* Nothing yet */
	old = FALSE;

	/* Describe location */
        if (r_ptr->flags7 & RF7_PET)
	{
                roff(format("%^s is friendly to you. ", wd_he[msex]));
		old = TRUE;
	}

	/* Describe location */
        if (r_ptr->level == 0 && !(r_ptr->flags9 & RF9_SPECIAL_GENE))
	{
		roff(format("%^s lives in the town", wd_he[msex]));
		old = TRUE;
	}
	/* Describe location */
        else if (r_ptr->flags9 & RF9_SPECIAL_GENE)
	{
                roff(format("This creature normally never appear"));
		old = TRUE;
	}

	else if (r_ptr->r_tkills)
	{
		if (depth_in_feet)
		{
			roff(format("%^s is normally found at depths of %d feet",
			            wd_he[msex], r_ptr->level * 50));
		}
		else
		{
			roff(format("%^s is normally found on dungeon level %d",
			            wd_he[msex], r_ptr->level));
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
                        c_roff(TERM_L_RED, " quickly");
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
		if (flags3 & (RF3_EVIL))            roff(" evil");
		if (flags3 & (RF3_GOOD))            roff(" good");
                if (flags3 & (RF3_UNDEAD))          c_roff(TERM_VIOLET, " undead");

		/* Describe the "race" */
		if (flags3 & (RF3_DRAGON))          roff(" dragon");
                else if (flags3 & (RF3_DEMON))      c_roff(TERM_VIOLET, " demon");
		else if (flags3 & (RF3_GIANT))      roff(" giant");
		else if (flags3 & (RF3_TROLL))      roff(" troll");
		else if (flags3 & (RF3_ORC))        roff(" orc");
                else if (flags3 & (RF3_DRAGONRIDER))roff(" DragonRider");
                else if (flags3 & (RF3_DARKLORD))        roff(" Dark Lord");
		else                                roff(" creature");

		/* Group some variables */
		if (TRUE)
		{
			long i, j;

			/* calculate the integer exp part */
                        /* i = (long)r_ptr->mexp * r_ptr->level / p_ptr->lev;*/
                        i = ((long)r_ptr->mexp * r_ptr->level / p_ptr->lev) / 10;

			/* calculate the fractional exp part scaled by 100, */
			/* must use long arithmetic to avoid overflow  */
                        /* j = ((((long)r_ptr->mexp * r_ptr->level % p_ptr->lev) * */
                        /*       (long)1000 / p_ptr->lev + 5) / 10); */
                        j = (((((long)r_ptr->mexp * r_ptr->level % p_ptr->lev) / 10) *
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

	if ((flags2 & (RF2_AURA_FIRE)) && (flags2 & (RF2_AURA_ELEC)))
	{
                c_roff(TERM_VIOLET, format("%^s is surrounded by flames and electricity.  ", wd_he[msex]));
	}
	else if (flags2 & (RF2_AURA_FIRE))
	{
                c_roff(TERM_RED, format("%^s is surrounded by flames.  ", wd_he[msex]));
	}
	else if (flags2 & (RF2_AURA_ELEC))
	{
                c_roff(TERM_L_BLUE, format("%^s is surrounded by electricity.  ", wd_he[msex]));
	}

	if (flags2 & (RF2_REFLECTING))
	{
		roff(format("%^s reflects bolt spells.  ", wd_he[msex]));
	}


	/* Describe escorts */
	if ((flags1 & (RF1_ESCORT)) || (flags1 & (RF1_ESCORTS)))
	{
		roff(format("%^s usually appears with escorts.  ",
		            wd_he[msex]));
	}

	/* Describe friends */
	else if ((flags1 & (RF1_FRIEND)) || (flags1 & (RF1_FRIENDS)))
	{
		roff(format("%^s usually appears in groups.  ",
		            wd_he[msex]));
	}


	/* Collect inate attacks */
	vn = 0;
	if (flags4 & (RF4_SHRIEK))		vp[vn++] = "shriek for help";
	if (flags4 & (RF4_XXX3))		vp[vn++] = "do something";
	if (flags4 & (RF4_ROCKET))		vp[vn++] = "shoot a rocket";
	if (flags4 & (RF4_ARROW_1))		vp[vn++] = "fire an arrow";
	if (flags4 & (RF4_ARROW_2))		vp[vn++] = "fire arrows";
	if (flags4 & (RF4_ARROW_3))		vp[vn++] = "fire a missile";
	if (flags4 & (RF4_ARROW_4))		vp[vn++] = "fire missiles";

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
	if (flags4 & (RF4_BR_NUKE))		vp[vn++] = "toxic waste";
	if (flags4 & (RF4_BR_DISI))		vp[vn++] = "disintegration";

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
	if (flags5 & (RF5_BA_MANA))         vp[vn++] = "invoke mana storms";
	if (flags5 & (RF5_BA_DARK))         vp[vn++] = "invoke darkness storms";
        if (flags4 & (RF4_BA_CHAO))         vp[vn++] = "invoke raw chaos";
	if (flags6 & (RF6_HAND_DOOM))       vp[vn++] = "invoke the Hand of Doom";
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
	if (flags6 & (RF6_XXX2))            vp[vn++] = "do something";
	if (flags6 & (RF6_BLINK))           vp[vn++] = "blink-self";
	if (flags6 & (RF6_TPORT))           vp[vn++] = "teleport-self";
        if (flags6 & (RF6_S_BUG))           vp[vn++] = "summon software bugs";
        if (flags6 & (RF6_S_RNG))           vp[vn++] = "summon RNG";
	if (flags6 & (RF6_TELE_TO))         vp[vn++] = "teleport to";
	if (flags6 & (RF6_TELE_AWAY))       vp[vn++] = "teleport away";
	if (flags6 & (RF6_TELE_LEVEL))      vp[vn++] = "teleport level";
        if (flags6 & (RF6_S_DRAGONRIDER))   vp[vn++] = "summon a DragonRider";
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
        if (flags6 & (RF6_S_WRAITH))        vp[vn++] = "summon Ringwraith";
	if (flags6 & (RF6_S_UNIQUE))        vp[vn++] = "summon Unique Monsters";

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
		}

		/* Variable hitpoints */
		else
		{
			roff(format(" and a life rating of %dd%d.  ",
			            r_ptr->hdice, r_ptr->hside));
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
	}


	/* Describe special abilities. */
	if (flags2 & (RF2_INVISIBLE))
	{
		roff(format("%^s is invisible.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_COLD_BLOOD))
	{
		roff(format("%^s is cold blooded.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_EMPTY_MIND))
	{
		roff(format("%^s is not detected by telepathy.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_WEIRD_MIND))
	{
		roff(format("%^s is rarely detected by telepathy.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_MULTIPLY))
	{
                c_roff(TERM_L_UMBER, format("%^s breeds explosively.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_REGENERATE))
	{
                c_roff(TERM_L_WHITE, format("%^s regenerates quickly.  ", wd_he[msex]));
	}
        if (r_ptr->flags7 & (RF7_MORTAL))
	{
                c_roff(TERM_RED, format("%^s is a mortal being.  ", wd_he[msex]));
	}
        else
	{
                c_roff(TERM_L_BLUE, format("%^s is an immortal being.  ", wd_he[msex]));
	}


	/* Collect susceptibilities */
	vn = 0;
        if (flags3 & (RF3_HURT_ROCK)) {vp[vn++] = "rock remover"; color[vn - 1] = TERM_UMBER;}
        if (flags3 & (RF3_HURT_LITE)) {vp[vn++] = "bright light"; color[vn - 1] = TERM_YELLOW;}
        if (flags3 & (RF3_SUSCEP_FIRE)) {vp[vn++] = "fire"; color[vn - 1] = TERM_RED;}
        if (flags3 & (RF3_SUSCEP_COLD)) {vp[vn++] = "cold"; color[vn - 1] = TERM_L_WHITE;}
        if (flags9 & (RF9_SUSCEP_ACID)) {vp[vn++] = "acid"; color[vn - 1] = TERM_GREEN;}
        if (flags9 & (RF9_SUSCEP_ELEC)) {vp[vn++] = "lightning"; color[vn - 1] = TERM_L_BLUE;}
        if (flags9 & (RF9_SUSCEP_POIS)) {vp[vn++] = "poison"; color[vn - 1] = TERM_L_GREEN;}

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
	}


	/* Collect immunities */
	vn = 0;
        if (flags3 & (RF3_IM_ACID)) {vp[vn++] = "acid"; color[vn - 1] = TERM_GREEN;}
        if (flags3 & (RF3_IM_ELEC)) {vp[vn++] = "lightning"; color[vn - 1] = TERM_L_BLUE;}
        if (flags3 & (RF3_IM_FIRE)) {vp[vn++] = "fire"; color[vn - 1] = TERM_RED;}
        if (flags3 & (RF3_IM_COLD)) {vp[vn++] = "cold"; color[vn - 1] = TERM_L_WHITE;}
        if (flags3 & (RF3_IM_POIS)) {vp[vn++] = "poison"; color[vn - 1] = TERM_L_GREEN;}

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
	}


	/* Collect resistances */
	vn = 0;
	if (flags3 & (RF3_RES_NETH)) vp[vn++] = "nether";
	if (flags3 & (RF3_RES_WATE)) vp[vn++] = "water";
	if (flags3 & (RF3_RES_PLAS)) vp[vn++] = "plasma";
	if (flags3 & (RF3_RES_NEXU)) vp[vn++] = "nexus";
	if (flags3 & (RF3_RES_DISE)) vp[vn++] = "disenchantment";
	if (flags3 & (RF3_RES_TELE)) vp[vn++] = "teleportation";
        if (flags7 & (RF7_RES_WIND)) vp[vn++] = "wind";
        if (flags7 & (RF7_RES_LITE)) vp[vn++] = "light";
        if (flags7 & (RF7_RES_DARK)) vp[vn++] = "darkness";
        if (flags7 & (RF7_RES_INER)) vp[vn++] = "inertia";
        if (flags7 & (RF7_RES_TIME)) vp[vn++] = "time";
        if (flags7 & (RF7_RES_CHAOS)) vp[vn++] = "chaos";
        if (flags7 & (RF7_RES_GRAV)) vp[vn++] = "gravity";
        if (flags7 & (RF7_RES_FORCE)) vp[vn++] = "force";
        if (flags7 & (RF7_RES_NUKE)) vp[vn++] = "nuclear";
        if (flags7 & (RF7_RES_SHARDS)) vp[vn++] = "shards";
        if (flags7 & (RF7_RES_SOUND)) vp[vn++] = "sound";

        /* Describe resistances */
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
			roff(vp[n]);
		}

		/* End */
		roff(".  ");
	}


	/* Collect non-effects */
	vn = 0;
	if (flags3 & (RF3_NO_STUN)) vp[vn++] = "stunned";
	if (flags3 & (RF3_NO_FEAR)) vp[vn++] = "frightened";
	if (flags3 & (RF3_NO_CONF)) vp[vn++] = "confused";
	if (flags3 & (RF3_NO_SLEEP)) vp[vn++] = "slept";

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
			case RBM_HIT:		p = "hit"; break;
			case RBM_TOUCH:		p = "touch"; break;
			case RBM_PUNCH:		p = "punch"; break;
			case RBM_KICK:		p = "kick"; break;
			case RBM_CLAW:		p = "claw"; break;
			case RBM_BITE:		p = "bite"; break;
			case RBM_STING:		p = "sting"; break;
			case RBM_XXX1:		break;
			case RBM_BUTT:		p = "butt"; break;
			case RBM_CRUSH:		p = "crush"; break;
			case RBM_ENGULF:	p = "engulf"; break;
			case RBM_CHARGE: 	p = "charge";   break;
			case RBM_CRAWL:		p = "crawl on you"; break;
			case RBM_DROOL:		p = "drool on you"; break;
			case RBM_SPIT:		p = "spit"; break;
			case RBM_EXPLODE:	p = "explode"; break;
			case RBM_GAZE:		p = "gaze"; break;
			case RBM_WAIL:		p = "wail"; break;
			case RBM_SPORE:		p = "release spores"; break;
			case RBM_XXX4:		break;
			case RBM_BEG:		p = "beg"; break;
			case RBM_INSULT:	p = "insult"; break;
			case RBM_MOAN:		p = "moan"; break;
			case RBM_SHOW:  	p = "sing"; break;
		}


		/* Default effect */
		q = NULL;

		/* Acquire the effect */
		switch (effect)
		{
			case RBE_HURT:    	q = "attack"; break;
			case RBE_POISON:  	q = "poison"; break;
			case RBE_UN_BONUS:	q = "disenchant"; break;
			case RBE_UN_POWER:	q = "drain charges"; break;
			case RBE_EAT_GOLD:	q = "steal gold"; break;
			case RBE_EAT_ITEM:	q = "steal items"; break;
			case RBE_EAT_FOOD:	q = "eat your food"; break;
			case RBE_EAT_LITE:	q = "absorb light"; break;
			case RBE_ACID:    	q = "shoot acid"; break;
			case RBE_ELEC:    	q = "electrocute"; break;
			case RBE_FIRE:    	q = "burn"; break;
			case RBE_COLD:    	q = "freeze"; break;
			case RBE_BLIND:   	q = "blind"; break;
			case RBE_CONFUSE: 	q = "confuse"; break;
			case RBE_TERRIFY: 	q = "terrify"; break;
			case RBE_PARALYZE:	q = "paralyze"; break;
			case RBE_LOSE_STR:	q = "reduce strength"; break;
			case RBE_LOSE_INT:	q = "reduce intelligence"; break;
			case RBE_LOSE_WIS:	q = "reduce wisdom"; break;
			case RBE_LOSE_DEX:	q = "reduce dexterity"; break;
			case RBE_LOSE_CON:	q = "reduce constitution"; break;
			case RBE_LOSE_CHR:	q = "reduce charisma"; break;
			case RBE_LOSE_ALL:	q = "reduce all stats"; break;
			case RBE_SHATTER:	q = "shatter"; break;
			case RBE_EXP_10:	q = "lower experience (by 10d6+)"; break;
			case RBE_EXP_20:	q = "lower experience (by 20d6+)"; break;
			case RBE_EXP_40:	q = "lower experience (by 40d6+)"; break;
			case RBE_EXP_80:	q = "lower experience (by 80d6+)"; break;
			case RBE_DISEASE:	q = "disease"; break;
                        case RBE_TIME:          q = "time"; break;
                        case RBE_SANITY:        q = "blast sanity"; break;
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
                                /* Know the exact amount of damages you do... */
                                if (p_ptr->body_monster == r_idx)
                                {
                                        c_roff(TERM_L_GREEN, format(" %dd%d", d1 + p_ptr->lev / 2, d2 + p_ptr->lev / 2));
                                }
                                else roff(format(" %dd%d", d1, d2));
			}
		}


		/* Count the attacks as printed */
		r++;
	}

	/* Finish sentence above */
	if (r)
	{
		roff(".  ");
	}

	/* Notice lack of attacks */
	else if (flags1 & (RF1_NEVER_BLOW))
	{
		roff(format("%^s has no physical attacks.  ", wd_he[msex]));
	}

	/* Or describe the lack of knowledge */
	else
	{
		roff(format("Nothing is known about %s attack.  ", wd_his[msex]));
	}


	/* Notice "Quest" monsters */
	if (flags1 & RF1_QUESTOR)
	{
                c_roff(TERM_VIOLET, "You feel an intense desire to kill this monster...  ");
	}


	/* All done */
	roff("\n");

	/* Cheat -- know everything */
	if ((cheat_know) && (remem == 0))
	{
		/* Hack -- restore memory */
		COPY(r_ptr, &save_mem, monster_race);
	}
        if (m_ptr->boss >= 0)
        {
                roff("----- ABILITIES -----");
                roff("\n");
                if (m_ptr->abilities & (BOSS_IMMUNE_WEAPONS))
                {
                        c_roff(TERM_L_BLUE, "Immune To Weapons/Physical attacks");
                        roff("\n");
                }
                if (m_ptr->abilities & (BOSS_IMMUNE_MAGIC))
                {
                        c_roff(TERM_L_BLUE, "Immune To Magical attacks");
                        roff("\n");
                }
                if (m_ptr->abilities & (BOSS_DOUBLE_DAMAGES))
                {
                        c_roff(TERM_L_BLUE, "Does double damages with physical attacks");
                        roff("\n");
                }
                if (m_ptr->abilities & (BOSS_HALVE_DAMAGES))
                {
                        c_roff(TERM_L_BLUE, "50% resistance to all damages(Physical or Magical)");
                        roff("\n");
                }
                if (m_ptr->abilities & (BOSS_CURSED_HITS))
                {
                        c_roff(TERM_L_BLUE, "Monster's attacks scare, blind and confuse");
                        roff("\n");
                }
                if (m_ptr->abilities & (BOSS_DOUBLE_MAGIC))
                {
                        c_roff(TERM_L_BLUE, "Does double damages with magical attacks");
                        roff("\n");
                }
                if (m_ptr->abilities & (BOSS_RETURNING))
                {
                        c_roff(TERM_L_BLUE, "Returns 50% physical damages to the player");
                        roff("\n");
                }
                if (m_ptr->abilities & (BOSS_MAGIC_RETURNING))
                {
                        c_roff(TERM_L_BLUE, "Returns 50% magic damages to the player");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_LOWER_POWER))
                {
                        c_roff(TERM_ORANGE, "Cursed with Lower Power.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_LOWER_MAGIC))
                {
                        c_roff(TERM_ORANGE, "Cursed with Lower Magic.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_HALVE_HP))
                {
                        c_roff(TERM_ORANGE, "Cursed with Life Blast.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_LOCK))
                {
                        c_roff(TERM_ORANGE, "Cursed with Lock.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_SLOW_DOWN))
                {
                        c_roff(TERM_ORANGE, "Cursed with Slow Down.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_HALVE_SPEED))
                {
                        c_roff(TERM_ORANGE, "Cursed with Speed Sap.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_HALVE_DAMAGES))
                {
                        c_roff(TERM_ORANGE, "Cursed with Halve Damages.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_HALVE_MAGIC))
                {
                        c_roff(TERM_ORANGE, "Cursed with Halve Magic.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_HALVE_LEVEL))
                {
                        c_roff(TERM_ORANGE, "Cursed with Halve Level.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_DAMAGES_CURSE))
                {
                        c_roff(TERM_ORANGE, "Cursed with Damages Curse.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_FRAILNESS))
                {
                        c_roff(TERM_ORANGE, "Cursed with Frailness.");
                        roff("\n");
                }
                if (m_ptr->abilities & (CURSE_INEPTITUDE))
                {
                        c_roff(TERM_ORANGE, "Cursed with Ineptitude.");
                        roff("\n");
                }
                if (m_ptr->abilities & (EYE_STABBED))
                {
                        c_roff(TERM_ORANGE, "Blinded by Eye Stab ability.");
                        roff("\n");
                }
                if (m_ptr->abilities & (MUTILATE_LEGS))
                {
                        c_roff(TERM_ORANGE, "Mutilated legs(can't move).");
                        roff("\n");
                }
                if (m_ptr->abilities & (MUTILATE_ARMS))
                {
                        c_roff(TERM_ORANGE, "Mutilated arms(can't attack).");
                        roff("\n");
                }
                if (m_ptr->abilities & (PSYCHIC_HITRATE))
                {
                        c_roff(TERM_ORANGE, "Blinded by fearful illusions.");
                        roff("\n");
                }
                if (m_ptr->abilities & (WAR_BLESSED))
                {
                        c_roff(TERM_YELLOW, "Blessed with War Blessing.");
                        roff("\n");
                }
                if (m_ptr->abilities & (MORALE_BOOST))
                {
                        c_roff(TERM_YELLOW, "Morale Boost.");
                        roff("\n");
                }

        }
}

void vision_scan_monster(monster_type *m_ptr)
{
        char query;
        char mtype[30];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Save what's on screen */
        Term_save();

	/* Flush messages */
	msg_print(NULL);

        /* Begin... */
	Term_erase(0, 1, 255);

        /* Now, let's scan the monster! */
        roff(format("Name: %s", (r_name + r_ptr->name)));
        roff("\n");

        /* Let's check the monster's type now... */
        if (r_ptr->d_char == 'a') strcpy(mtype, "Ant");
        else if (r_ptr->d_char == 'b') strcpy(mtype, "Bat");
        else if (r_ptr->d_char == 'c') strcpy(mtype, "Centipede");
        else if (r_ptr->d_char == 'd') strcpy(mtype, "Small Dragon");
        else if (r_ptr->d_char == 'e') strcpy(mtype, "Eye");
        else if (r_ptr->d_char == 'f') strcpy(mtype, "Feline");
        else if (r_ptr->d_char == 'g') strcpy(mtype, "Golem");
        else if (r_ptr->d_char == 'h') strcpy(mtype, "Humanoid");
        else if (r_ptr->d_char == 'i') strcpy(mtype, "Icky Thing");
        else if (r_ptr->d_char == 'j') strcpy(mtype, "Jelly");
        else if (r_ptr->d_char == 'k') strcpy(mtype, "Kobold");
        else if (r_ptr->d_char == 'l') strcpy(mtype, "Louse");
        else if (r_ptr->d_char == 'm') strcpy(mtype, "Plant/Mold");
        else if (r_ptr->d_char == 'n') strcpy(mtype, "Naga");
        else if (r_ptr->d_char == 'o') strcpy(mtype, "Orc");
        else if (r_ptr->d_char == 'p') strcpy(mtype, "Human");
        else if (r_ptr->d_char == 'q') strcpy(mtype, "Quadroped");
        else if (r_ptr->d_char == 'r') strcpy(mtype, "Rodent");
        else if (r_ptr->d_char == 's') strcpy(mtype, "Skeleton");
        else if (r_ptr->d_char == 't') strcpy(mtype, "Townsfolk");
        else if (r_ptr->d_char == 'u') strcpy(mtype, "Lesser Demon");
        else if (r_ptr->d_char == 'v') strcpy(mtype, "Vortex");
        else if (r_ptr->d_char == 'w') strcpy(mtype, "Worm");
        else if (r_ptr->d_char == 'x' || r_ptr->d_char == '~') strcpy(mtype, "Fish/Sea Monster");
        else if (r_ptr->d_char == 'y') strcpy(mtype, "Yeek");
        else if (r_ptr->d_char == 'z') strcpy(mtype, "Zombie");
        else if (r_ptr->d_char == 'A') strcpy(mtype, "Angel");
        else if (r_ptr->d_char == 'B') strcpy(mtype, "Bird");
        else if (r_ptr->d_char == 'C') strcpy(mtype, "Canine");
        else if (r_ptr->d_char == 'D') strcpy(mtype, "Great Dragon");
        else if (r_ptr->d_char == 'E') strcpy(mtype, "Spirit/Elemental");
        else if (r_ptr->d_char == 'F') strcpy(mtype, "Dragon Fly");
        else if (r_ptr->d_char == 'G') strcpy(mtype, "Ghost");
        else if (r_ptr->d_char == 'H') strcpy(mtype, "Hybrid");
        else if (r_ptr->d_char == 'I') strcpy(mtype, "Insect");
        else if (r_ptr->d_char == 'J') strcpy(mtype, "Snake");
        else if (r_ptr->d_char == 'K') strcpy(mtype, "Killer Beetle");
        else if (r_ptr->d_char == 'L') strcpy(mtype, "Lich");
        else if (r_ptr->d_char == 'M') strcpy(mtype, "Hydra");
        else if (r_ptr->d_char == 'N') strcpy(mtype, "????");
        else if (r_ptr->d_char == 'O') strcpy(mtype, "Ogre");
        else if (r_ptr->d_char == 'P') strcpy(mtype, "Giant/Special");
        else if (r_ptr->d_char == 'Q') strcpy(mtype, "Quylthulg");
        else if (r_ptr->d_char == 'R') strcpy(mtype, "Reptile");
        else if (r_ptr->d_char == 'S') strcpy(mtype, "Spider");
        else if (r_ptr->d_char == 'T') strcpy(mtype, "Troll");
        else if (r_ptr->d_char == 'U') strcpy(mtype, "Greater Demon");
        else if (r_ptr->d_char == 'V') strcpy(mtype, "Vampire");
        else if (r_ptr->d_char == 'W') strcpy(mtype, "Wraith");
        else if (r_ptr->d_char == 'X') strcpy(mtype, "Xorn/Xaren/Umber Hulk");
        else if (r_ptr->d_char == 'Y') strcpy(mtype, "Yeti");
        else if (r_ptr->d_char == 'Z') strcpy(mtype, "Hound");
        else if (r_ptr->d_char == ',') strcpy(mtype, "Mushroom");
        else if (r_ptr->d_char == '$') strcpy(mtype, "Creeping Coin");
        else if (r_ptr->d_char == '#') strcpy(mtype, "Wall");
        else if (r_ptr->d_char == '&') strcpy(mtype, "Devling");
        else if (r_ptr->d_char == '*') strcpy(mtype, "Sphere");
        else strcpy(mtype, "????");
        roff(format("Kind: %s", mtype));
        roff("\n");
        roff("Type: ");
        if (r_ptr->flags3 & (RF3_EVIL)) c_roff(TERM_VIOLET, "Evil ");
        if (r_ptr->flags3 & (RF3_GOOD)) c_roff(TERM_VIOLET, "Good ");
        if (r_ptr->flags3 & (RF3_UNDEAD)) c_roff(TERM_VIOLET, "Undead ");
        if (r_ptr->flags3 & (RF3_ORC)) c_roff(TERM_VIOLET, "Orc ");
        if (r_ptr->flags3 & (RF3_TROLL)) c_roff(TERM_VIOLET, "Troll ");
        if (r_ptr->flags3 & (RF3_GIANT)) c_roff(TERM_VIOLET, "Giant ");
        if (r_ptr->flags3 & (RF3_DRAGON)) c_roff(TERM_VIOLET, "Dragon ");
        if (r_ptr->flags3 & (RF3_DEMON)) c_roff(TERM_VIOLET, "Demon ");
        if (r_ptr->flags3 & (RF3_ANIMAL)) c_roff(TERM_VIOLET, "Animal ");

        roff("\n");
        roff("\n");
        roff(format("Level: %d", m_ptr->level));
        roff("\n");
        roff(format("Hp: %ld/%ld", m_ptr->hp, m_ptr->maxhp));
        roff("\n");
        roff(format("Hit Rate: %d", m_ptr->hitrate));
        roff("\n");
        roff(format("Defense: %d", m_ptr->defense));
        roff("\n");
        roff("\n");
        roff("Weaknesses: ");
        if (r_ptr->flags3 & (RF3_SUSCEP_FIRE)) c_roff(TERM_RED, "Fire ");
        if (r_ptr->flags3 & (RF3_SUSCEP_COLD)) c_roff(TERM_L_WHITE, "Cold ");
        if (r_ptr->flags9 & (RF9_SUSCEP_ELEC)) c_roff(TERM_L_BLUE, "Electricity ");
        if (r_ptr->flags9 & (RF9_SUSCEP_ACID)) c_roff(TERM_GREEN, "Acid ");
        if (r_ptr->flags9 & (RF9_SUSCEP_POIS)) c_roff(TERM_L_GREEN, "Poison ");
        if (r_ptr->flags3 & (RF3_HURT_LITE)) c_roff(TERM_YELLOW, "Light ");
        roff("\n");
        roff("\n");
        roff("Relation: ");
        if (is_pet(m_ptr))
        {
                if (m_ptr->friend == 1) c_roff(TERM_L_GREEN, "Friend");
                else if (m_ptr->imprinted) c_roff(TERM_L_GREEN, "Pet");
                else c_roff(TERM_YELLOW, "Ally");
        }
        else c_roff(TERM_RED, "Enemy");
        roff("\n");

        /* End of scanning */
        query = inkey();

        /* Load term */
        Term_load();
}

void open_monster_generator()
{
        char query;

        /* Save what's on screen */
        Term_save();

	/* Flush messages */
	msg_print(NULL);

        /* Begin... */
	Term_erase(0, 1, 255);

        /* Now, let's scan the monster! */
        roff("----- NEWANGBAND MONSTER GENERATOR -----\n");
        roff("-----        Version: 1.0.1        -----\n");
        roff("\n");
        roff("\n");
        roff("Before using, please, consult the Newbies Guide!\n");
        roff("This is the first version, and the generator currently\n");
        roff("isen't very 'smart', so use it at your own risks!\n");
        roff("Remember to backup r_info.txt in the lib/edit directory!\n");
        roff("\n");
        roff("Press 'g' to generate the monsters. Press any keys to exit.\n");
        roff("\n");

        /* End of scanning */
        query = inkey();

        if (query == 'g' || query == 'G')
        {
                monster_generator();
                msg_print("Monster generation completed!");
                query = inkey();
        }

        /* Load term */
        Term_load();
}

