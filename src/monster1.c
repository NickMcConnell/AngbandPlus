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
	if (!(r_ptr->flags1 & RF1_UNIQUE)) return (FALSE);

	/* Unique monsters */
	if (kills > 304 / (38 + (5 * level) / 4)) return (TRUE);

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
	if (!(r_ptr->flags1 & RF1_UNIQUE)) return (FALSE);

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
	monster_race *r_ptr = &r_info[r_idx];

	bool old = FALSE;
	bool sin = FALSE;

	int m, n, r;
	cptr p, q;

	int msex = 0;
	int speed = (ironman_nightmare) ? r_ptr->speed + 5 : r_ptr->speed;

	bool breath = FALSE;
	bool magic = FALSE;

	u32b flags1, flags2, flags3, flags4, flags5, flags6, flags7;

	int vn = 0;
	cptr vp[80];

	monster_race save_mem;

	/* Descriptions */
	char buf[2048];

	/* Cheat -- Know everything */
	if (cheat_know)
	{
		/* XXX XXX XXX */

		/* Save the "old" memory (structure copy) */
		save_mem = *r_ptr;

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
			(((r_ptr->flags1 & RF1_DROP_4D2) ? 8 : 0) +
			 ((r_ptr->flags1 & RF1_DROP_3D2) ? 6 : 0) +
			 ((r_ptr->flags1 & RF1_DROP_2D2) ? 4 : 0) +
			 ((r_ptr->flags1 & RF1_DROP_1D2) ? 2 : 0) +
			 ((r_ptr->flags1 & RF1_DROP_90) ? 1 : 0) +
			 ((r_ptr->flags1 & RF1_DROP_60) ? 1 : 0));

		/* Hack -- but only "valid" drops */
		if (r_ptr->flags1 & RF1_ONLY_GOLD) r_ptr->r_drop_item = 0;
		if (r_ptr->flags1 & RF1_ONLY_ITEM) r_ptr->r_drop_gold = 0;

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
	}


	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	else if (r_ptr->flags1 & RF1_MALE) msex = 1;


	/* Obtain a copy of the "known" flags */
	flags1 = (r_ptr->flags1 & r_ptr->r_flags1);
	flags2 = (r_ptr->flags2 & r_ptr->r_flags2);
	flags3 = (r_ptr->flags3 & r_ptr->r_flags3);
	flags4 = (r_ptr->flags4 & r_ptr->r_flags4);
	flags5 = (r_ptr->flags5 & r_ptr->r_flags5);
	flags6 = (r_ptr->flags6 & r_ptr->r_flags6);
	flags7 = (r_ptr->flags7);


	/* Assume some "obvious" flags */
	if (r_ptr->flags1 & RF1_UNIQUE) flags1 |= (RF1_UNIQUE);
	if (r_ptr->flags1 & RF1_QUESTOR) flags1 |= (RF1_QUESTOR);
	if (r_ptr->flags1 & RF1_MALE) flags1 |= (RF1_MALE);
	if (r_ptr->flags1 & RF1_FEMALE) flags1 |= (RF1_FEMALE);

	/* Assume some "creation" flags */
	if (r_ptr->flags1 & RF1_CHAR_MIMIC) flags1 |= (RF1_CHAR_MIMIC);
	if (r_ptr->flags1 & RF1_FRIENDS) flags1 |= (RF1_FRIENDS);
	if (r_ptr->flags1 & RF1_ESCORT) flags1 |= (RF1_ESCORT);
	if (r_ptr->flags1 & RF1_ESCORTS) flags1 |= (RF1_ESCORTS);

	/* Killing a monster reveals some properties */
	if (r_ptr->r_tkills || cheat_know)
	{
		/* Know "race" flags */
		if (r_ptr->flags3 & RF3_ORC) flags3 |= (RF3_ORC);
		if (r_ptr->flags3 & RF3_TROLL) flags3 |= (RF3_TROLL);
		if (r_ptr->flags3 & RF3_GIANT) flags3 |= (RF3_GIANT);
		if (r_ptr->flags3 & RF3_DRAGON) flags3 |= (RF3_DRAGON);
		if (r_ptr->flags3 & RF3_DEMON) flags3 |= (RF3_DEMON);
		if (r_ptr->flags3 & RF3_UNDEAD) flags3 |= (RF3_UNDEAD);
		if (r_ptr->flags3 & RF3_EVIL) flags3 |= (RF3_EVIL);
		if (r_ptr->flags3 & RF3_GOOD) flags3 |= (RF3_GOOD);
		if (r_ptr->flags3 & RF3_ANIMAL) flags3 |= (RF3_ANIMAL);
		if (r_ptr->flags3 & RF3_AMBERITE) flags3 |= (RF3_AMBERITE);

		/* Know 'quantum' flag */
		if (r_ptr->flags2 & RF2_QUANTUM) flags2 |= (RF2_QUANTUM);

		/* Know "forced" flags */
		if (r_ptr->flags1 & RF1_FORCE_DEPTH) flags1 |= (RF1_FORCE_DEPTH);
		if (r_ptr->flags1 & RF1_FORCE_MAXHP) flags1 |= (RF1_FORCE_MAXHP);
	}

	/* Treat uniques differently */
	if (flags1 & RF1_UNIQUE)
	{
		/* Hack -- Determine if the unique is "dead" */
		bool dead = (r_ptr->max_num == 0) ? TRUE : FALSE;

		/* We've been killed... */
		if (r_ptr->r_deaths)
		{
			/* Killed ancestors */
			c_roff(TERM_WHITE, format("%^s has slain %d of your ancestors",
									  wd_he[msex], r_ptr->r_deaths));

			/* But we've also killed it */
			if (dead)
			{
				c_roff(TERM_WHITE, format(", but you have avenged %s!  ",
										  plural(r_ptr->r_deaths, "him",
												 "them")));
			}

			/* Unavenged (ever) */
			else
			{
				c_roff(TERM_WHITE, format(", who %s unavenged.  ",
										  plural(r_ptr->r_deaths, "remains",
												 "remain")));
			}
		}

		/* Dead unique who never hurt us */
		else if (dead)
		{
			c_roff(TERM_L_DARK, "You have slain this foe.  ");
		}
	}

	/* Not unique, but killed us */
	else if (r_ptr->r_deaths)
	{
		/* Dead ancestors */
		c_roff(TERM_WHITE,
			   format("%d of your ancestors %s been killed by this creature, ",
					  r_ptr->r_deaths, plural(r_ptr->r_deaths, "has", "have")));

		/* Some kills this life */
		if (r_ptr->r_pkills)
		{
			c_roff(TERM_WHITE,
				   format
				   ("and you have exterminated at least %d of the creatures.  ",
					r_ptr->r_pkills));
		}

		/* Some kills past lives */
		else if (r_ptr->r_tkills)
		{
			c_roff(TERM_WHITE,
				   format
				   ("and %s have exterminated at least %d of the creatures.  ",
					"your ancestors", r_ptr->r_tkills));
		}

		/* No kills */
		else
		{
			c_roff(TERM_RED,
				   format("and %s is not ever known to have been defeated.  ",
						  wd_he[msex]));
		}
	}

	/* Normal monsters */
	else
	{
		/* Killed some this life */
		if (r_ptr->r_pkills)
		{
			c_roff(TERM_WHITE,
				   format("You have killed at least %d of these creatures.  ",
						  r_ptr->r_pkills));
		}

		/* Killed some last life */
		else if (r_ptr->r_tkills)
		{
			c_roff(TERM_WHITE,
				   format
				   ("Your ancestors have killed at least %d of these creatures.  ",
					r_ptr->r_tkills));
		}

		/* Killed none */
		else
		{
			c_roff(TERM_WHITE, "No battles to the death are recalled.  ");
		}
	}


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
	c_roff(TERM_WHITE, buf);
	c_roff(TERM_WHITE, "  ");


	/* Nothing yet */
	old = FALSE;

	/* Describe location */
	if (r_ptr->level == 0)
	{
		c_roff(TERM_WHITE, format("%^s lives in the town", wd_he[msex]));
		old = TRUE;
	}
	else if (r_ptr->r_tkills || cheat_know)
	{
		if (depth_in_feet)
		{
			c_roff(TERM_SLATE,
				   format("%^s is normally found at depths of %d feet",
						  wd_he[msex], r_ptr->level * 50));
		}
		else
		{
			c_roff(TERM_SLATE,
				   format("%^s is normally found on dungeon level %d",
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
			c_roff(TERM_WHITE, ", and ");
		}
		else
		{
			c_roff(TERM_WHITE, format("%^s ", wd_he[msex]));
			old = TRUE;
		}
		c_roff(TERM_WHITE, "moves");

		/* Random-ness */
		if ((flags1 & RF1_RAND_50) || (flags1 & RF1_RAND_25))
		{
			/* Adverb */
			if ((flags1 & RF1_RAND_50) && (flags1 & RF1_RAND_25))
			{
				c_roff(TERM_WHITE, " extremely");
			}
			else if (flags1 & RF1_RAND_50)
			{
				c_roff(TERM_WHITE, " somewhat");
			}
			else if (flags1 & RF1_RAND_25)
			{
				c_roff(TERM_WHITE, " a bit");
			}

			/* Adjective */
			c_roff(TERM_WHITE, " erratically");

			/* Hack -- Occasional conjunction */
			if (speed != 110) c_roff(TERM_WHITE, ", and");
		}

		/* Speed */
		if (speed > 110)
		{
			if (speed > 130) c_roff(TERM_GREEN, " incredibly");
			else if (speed > 120) c_roff(TERM_GREEN, " very");
			c_roff(TERM_GREEN, " quickly");
		}
		else if (speed < 110)
		{
			if (speed < 90) c_roff(TERM_GREEN, " incredibly");
			else if (speed < 100) c_roff(TERM_GREEN, " very");
			c_roff(TERM_GREEN, " slowly");
		}
		else
		{
			c_roff(TERM_GREEN, " at normal speed");
		}
	}

	/* The code above includes "attack speed" */
	if (flags1 & RF1_NEVER_MOVE)
	{
		/* Introduce */
		if (old)
		{
			c_roff(TERM_WHITE, ", but ");
		}
		else
		{
			c_roff(TERM_WHITE, format("%^s ", wd_he[msex]));
			old = TRUE;
		}

		/* Describe */
		c_roff(TERM_WHITE, "does not deign to chase intruders");
	}

	/* End this sentence */
	if (old)
	{
		c_roff(TERM_WHITE, ".  ");
		old = FALSE;
	}


	/* Describe experience if known */
	if (r_ptr->r_tkills || cheat_know)
	{
		/* Introduction */
		if (flags1 & RF1_UNIQUE)
		{
			c_roff(TERM_WHITE, "Killing this");
		}
		else
		{
			c_roff(TERM_WHITE, "A kill of this");
		}

		/* Describe the "quality" */
		if (flags2 & RF2_XXX_1) c_roff(TERM_L_BLUE, " some property");
		if (flags3 & RF3_ANIMAL) c_roff(TERM_L_BLUE, " natural");
		if (flags3 & RF3_EVIL) c_roff(TERM_L_BLUE, " evil");
		if (flags3 & RF3_GOOD) c_roff(TERM_L_BLUE, " good");
		if (flags3 & RF3_UNDEAD) c_roff(TERM_L_BLUE, " undead");

		/* Describe the "race" */
		if (flags3 & RF3_DRAGON) c_roff(TERM_L_BLUE, " dragon");
		else if (flags3 & RF3_DEMON) c_roff(TERM_L_BLUE, " demon");
		else if (flags3 & RF3_GIANT) c_roff(TERM_L_BLUE, " giant");
		else if (flags3 & RF3_TROLL) c_roff(TERM_L_BLUE, " troll");
		else if (flags3 & RF3_ORC) c_roff(TERM_L_BLUE, " orc");
		else if (flags3 & RF3_AMBERITE) c_roff(TERM_L_BLUE, " Amberite");
		else if (flags2 & RF2_QUANTUM) c_roff(TERM_L_BLUE, " quantum creature");
		else
			c_roff(TERM_WHITE, " creature");

		/* Group some variables */
		if (TRUE)
		{
			s32b new_exp, new_exp_frac;
			int i;

			/* Get the xp for a kill */
			exp_for_kill(r_ptr, &new_exp, &new_exp_frac);

			/* calculate the fractional exp part scaled by 100, */
			/* must use long arithmetic to avoid overflow */
			new_exp_frac =
				(((long)new_exp_frac + 0x10000L / 500) * 100) / 0x10000L;

			/* Mention the experience */
			c_roff(TERM_WHITE, format(" is worth %ld.%02ld point%s",
									  (long)new_exp, (long)new_exp_frac,
									  (((new_exp == 1)
										&& (new_exp_frac == 0)) ? "" : "s")));

			/* Take account of annoying English */
			p = "th";
			i = p_ptr->lev % 10;
			if ((p_ptr->lev / 10) == 1) /* nothing */ ;
			else if (i == 1) p = "st";
			else if (i == 2) p = "nd";
			else if (i == 3) p = "rd";

			/* Take account of "leading vowels" in numbers */
			q = "";
			i = p_ptr->lev;
			if ((i == 8) || (i == 11) || (i == 18)) q = "n";

			/* Mention the dependance on the player's level */
			c_roff(TERM_WHITE, format(" for a%s %lu%s level character.  ",
									  q, (long)i, p));
		}
	}

	if ((flags2 & RF2_AURA_FIRE) && (flags2 & RF2_AURA_ELEC))
	{
		c_roff(TERM_YELLOW,
			   format("%^s is surrounded by flames and electricity.  ",
					  wd_he[msex]));
	}
	else if ((flags3 & RF3_AURA_COLD) && (flags2 & RF2_AURA_ELEC))
	{
		c_roff(TERM_YELLOW,
			   format("%^s is surrounded by ice and electricity.  ",
					  wd_he[msex]));
	}
	else if (flags2 & RF2_AURA_FIRE)
	{
		c_roff(TERM_YELLOW,
			   format("%^s is surrounded by flames.  ", wd_he[msex]));
	}
	else if (flags3 & RF3_AURA_COLD)
	{
		c_roff(TERM_YELLOW, format("%^s is surrounded by ice.  ", wd_he[msex]));
	}
	else if (flags2 & RF2_AURA_ELEC)
	{
		c_roff(TERM_YELLOW, format("%^s is surrounded by electricity.  ",
								   wd_he[msex]));
	}

	if (flags2 & RF2_REFLECTING)
	{
		c_roff(TERM_YELLOW, format("%^s reflects bolt spells.  ", wd_he[msex]));
	}

	/* Describe escorts */
	if ((flags1 & RF1_ESCORT) || (flags1 & RF1_ESCORTS))
	{
		c_roff(TERM_WHITE, format("%^s usually appears with escorts.  ",
								  wd_he[msex]));
	}

	/* Describe friends */
	else if (flags1 & RF1_FRIENDS)
	{
		c_roff(TERM_WHITE, format("%^s usually appears in groups.  ",
								  wd_he[msex]));
	}

	else if (flags1 & RF1_CHAR_MIMIC)
	{
		c_roff(TERM_WHITE, format("%^s is a mimic.  ", wd_he[msex]));
	}

	/* Collect inate attacks */
	if (flags4 & RF4_SHRIEK) vp[vn++] = "shriek for help";
	if (flags4 & RF4_ELDRITCH_HORROR) vp[vn++] = "blast your sanity";
	if (flags4 & RF4_ROCKET) vp[vn++] = "shoot a rocket";
	if (flags4 & RF4_ARROW_1) vp[vn++] = "fire an arrow";
	if (flags4 & RF4_ARROW_2) vp[vn++] = "fire arrows";
	if (flags4 & RF4_ARROW_3) vp[vn++] = "fire a missile";
	if (flags4 & RF4_ARROW_4) vp[vn++] = "fire missiles";

	/* Describe inate attacks */
	if (vn)
	{
		/* Intro */
		c_roff(TERM_WHITE, format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(TERM_WHITE, " may ");
			else if (n < vn - 1) c_roff(TERM_WHITE, ", ");
			else
				c_roff(TERM_WHITE, " or ");

			/* Dump */
			c_roff(TERM_L_RED, vp[n]);
		}

		/* End */
		c_roff(TERM_WHITE, ".  ");
	}


	/* Collect breaths */
	vn = 0;
	if (flags4 & (RF4_BR_ACID)) vp[vn++] = "acid";
	if (flags4 & (RF4_BR_ELEC)) vp[vn++] = "lightning";
	if (flags4 & (RF4_BR_FIRE)) vp[vn++] = "fire";
	if (flags4 & (RF4_BR_COLD)) vp[vn++] = "frost";
	if (flags4 & (RF4_BR_POIS)) vp[vn++] = "poison";
	if (flags4 & (RF4_BR_NETH)) vp[vn++] = "nether";
	if (flags4 & (RF4_BR_LITE)) vp[vn++] = "light";
	if (flags4 & (RF4_BR_DARK)) vp[vn++] = "darkness";
	if (flags4 & (RF4_BR_CONF)) vp[vn++] = "confusion";
	if (flags4 & (RF4_BR_SOUN)) vp[vn++] = "sound";
	if (flags4 & (RF4_BR_CHAO)) vp[vn++] = "chaos";
	if (flags4 & (RF4_BR_DISE)) vp[vn++] = "disenchantment";
	if (flags4 & (RF4_BR_NEXU)) vp[vn++] = "nexus";
	if (flags4 & (RF4_BR_TIME)) vp[vn++] = "time";
	if (flags4 & (RF4_BR_INER)) vp[vn++] = "inertia";
	if (flags4 & (RF4_BR_GRAV)) vp[vn++] = "gravity";
	if (flags4 & (RF4_BR_SHAR)) vp[vn++] = "shards";
	if (flags4 & (RF4_BR_PLAS)) vp[vn++] = "plasma";
	if (flags4 & (RF4_BR_WALL)) vp[vn++] = "force";
	if (flags4 & (RF4_BR_MANA)) vp[vn++] = "mana";
	if (flags4 & (RF4_BR_NUKE)) vp[vn++] = "toxic waste";
	if (flags4 & (RF4_BR_DISI)) vp[vn++] = "disintegration";

	/* Describe breaths */
	if (vn)
	{
		/* Note breath */
		breath = TRUE;

		/* Intro */
		c_roff(TERM_WHITE, format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(TERM_WHITE, " may breathe ");
			else if (n < vn - 1) c_roff(TERM_WHITE, ", ");
			else
				c_roff(TERM_WHITE, " or ");

			/* Dump */
			c_roff(TERM_L_RED, vp[n]);
		}
	}


	/* Collect spells */
	vn = 0;
	if (flags5 & (RF5_BA_ACID)) vp[vn++] = "produce acid balls";
	if (flags5 & (RF5_BA_ELEC)) vp[vn++] = "produce lightning balls";
	if (flags5 & (RF5_BA_FIRE)) vp[vn++] = "produce fire balls";
	if (flags5 & (RF5_BA_COLD)) vp[vn++] = "produce frost balls";
	if (flags5 & (RF5_BA_POIS)) vp[vn++] = "produce poison balls";
	if (flags5 & (RF5_BA_NETH)) vp[vn++] = "produce nether balls";
	if (flags5 & (RF5_BA_WATE)) vp[vn++] = "produce water balls";
	if (flags4 & (RF4_BA_NUKE)) vp[vn++] = "produce balls of radiation";
	if (flags5 & (RF5_BA_MANA)) vp[vn++] = "invoke mana storms";
	if (flags5 & (RF5_BA_DARK)) vp[vn++] = "invoke darkness storms";
	if (flags4 & (RF4_BA_CHAO)) vp[vn++] = "invoke raw Logrus";
	if (flags6 & (RF6_HAND_DOOM)) vp[vn++] = "invoke the Hand of Doom";
	if (flags5 & (RF5_DRAIN_MANA)) vp[vn++] = "drain mana";
	if (flags5 & (RF5_MIND_BLAST)) vp[vn++] = "cause mind blasting";
	if (flags5 & (RF5_BRAIN_SMASH)) vp[vn++] = "cause brain smashing";
	if (flags5 & (RF5_CAUSE_1)) vp[vn++] = "cause light wounds and cursing";
	if (flags5 & (RF5_CAUSE_2)) vp[vn++] = "cause serious wounds and cursing";
	if (flags5 & (RF5_CAUSE_3)) vp[vn++] = "cause critical wounds and cursing";
	if (flags5 & (RF5_CAUSE_4)) vp[vn++] = "cause mortal wounds";
	if (flags5 & (RF5_BO_ACID)) vp[vn++] = "produce acid bolts";
	if (flags5 & (RF5_BO_ELEC)) vp[vn++] = "produce lightning bolts";
	if (flags5 & (RF5_BO_FIRE)) vp[vn++] = "produce fire bolts";
	if (flags5 & (RF5_BO_COLD)) vp[vn++] = "produce frost bolts";
	if (flags5 & (RF5_BO_POIS)) vp[vn++] = "produce poison bolts";
	if (flags5 & (RF5_BO_NETH)) vp[vn++] = "produce nether bolts";
	if (flags5 & (RF5_BO_WATE)) vp[vn++] = "produce water bolts";
	if (flags5 & (RF5_BO_MANA)) vp[vn++] = "produce mana bolts";
	if (flags5 & (RF5_BO_PLAS)) vp[vn++] = "produce plasma bolts";
	if (flags5 & (RF5_BO_ICEE)) vp[vn++] = "produce ice bolts";
	if (flags5 & (RF5_MISSILE)) vp[vn++] = "produce magic missiles";
	if (flags5 & (RF5_SCARE)) vp[vn++] = "terrify";
	if (flags5 & (RF5_BLIND)) vp[vn++] = "blind";
	if (flags5 & (RF5_CONF)) vp[vn++] = "confuse";
	if (flags5 & (RF5_SLOW)) vp[vn++] = "slow";
	if (flags5 & (RF5_HOLD)) vp[vn++] = "paralyze";
	if (flags6 & (RF6_HASTE)) vp[vn++] = "haste-self";
	if (flags6 & (RF6_HEAL)) vp[vn++] = "heal-self";
	if (flags6 & (RF6_INVULNER)) vp[vn++] = "make invulnerable";
	if (flags6 & (RF6_BLINK)) vp[vn++] = "blink-self";
	if (flags6 & (RF6_TPORT)) vp[vn++] = "teleport-self";
	if (flags6 & (RF6_XXX3)) vp[vn++] = "do something";
	if (flags6 & (RF6_XXX4)) vp[vn++] = "do something";
	if (flags6 & (RF6_TELE_TO)) vp[vn++] = "teleport to";
	if (flags6 & (RF6_TELE_AWAY)) vp[vn++] = "teleport away";
	if (flags6 & (RF6_TELE_LEVEL)) vp[vn++] = "teleport level";
	if (flags6 & (RF6_XXX5)) vp[vn++] = "do something";
	if (flags6 & (RF6_DARKNESS)) vp[vn++] = "create darkness";
	if (flags6 & (RF6_TRAPS)) vp[vn++] = "create traps";
	if (flags6 & (RF6_FORGET)) vp[vn++] = "cause amnesia";
	if (flags6 & (RF6_RAISE_DEAD)) vp[vn++] = "raise dead";
	if (flags6 & (RF6_S_MONSTER)) vp[vn++] = "summon a monster";
	if (flags6 & (RF6_S_MONSTERS)) vp[vn++] = "summon monsters";
	if (flags6 & (RF6_S_KIN)) vp[vn++] = "summon aid";
	if (flags6 & (RF6_S_ANT)) vp[vn++] = "summon ants";
	if (flags6 & (RF6_S_SPIDER)) vp[vn++] = "summon spiders";
	if (flags6 & (RF6_S_HOUND)) vp[vn++] = "summon hounds";
	if (flags6 & (RF6_S_HYDRA)) vp[vn++] = "summon hydras";
	if (flags6 & (RF6_S_ANGEL)) vp[vn++] = "summon an angel";
	if (flags6 & (RF6_S_DEMON)) vp[vn++] = "summon a demon";
	if (flags6 & (RF6_S_UNDEAD)) vp[vn++] = "summon an undead";
	if (flags6 & (RF6_S_DRAGON)) vp[vn++] = "summon a dragon";
	if (flags6 & (RF6_S_HI_UNDEAD)) vp[vn++] = "summon Greater Undead";
	if (flags6 & (RF6_S_HI_DRAGON)) vp[vn++] = "summon Ancient Dragons";
	if (flags6 & (RF6_S_CYBER)) vp[vn++] = "summon Cyberdemons";
	if (flags6 & (RF6_S_AMBERITES)) vp[vn++] = "summon Lords of Amber";
	if (flags6 & (RF6_S_UNIQUE)) vp[vn++] = "summon Unique Monsters";

	/* Describe spells */
	if (vn)
	{
		/* Note magic */
		magic = TRUE;

		/* Intro */
		if (breath)
		{
			c_roff(TERM_WHITE, ", and is also");
		}
		else
		{
			c_roff(TERM_WHITE, format("%^s is", wd_he[msex]));
		}

		/* Verb Phrase */
		c_roff(TERM_WHITE, " magical, casting spells");

		/* Adverb */
		if (flags2 & RF2_SMART) c_roff(TERM_ORANGE, " intelligently");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(TERM_WHITE, " which ");
			else if (n < vn - 1) c_roff(TERM_WHITE, ", ");
			else
				c_roff(TERM_WHITE, " or ");

			/* Dump */
			c_roff(TERM_L_RED, vp[n]);
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
			c_roff(TERM_WHITE, format("; 1 time in %d", 100 / n));
		}

		/* Guess at the frequency */
		else if (m)
		{
			n = ((n + 9) / 10) * 10;
			c_roff(TERM_WHITE, format("; about 1 time in %d", 100 / n));
		}

		/* End this sentence */
		c_roff(TERM_WHITE, ".  ");
	}


	/* Describe monster "toughness" */
	if (know_armour(r_idx))
	{
		/* Armor */
		c_roff(TERM_WHITE, format("%^s has an armor rating of %d",
								  wd_he[msex], r_ptr->ac));

		/* Maximized hitpoints */
		if (flags1 & RF1_FORCE_MAXHP)
		{
			c_roff(TERM_WHITE, format(" and a life rating of %d.  ",
									  r_ptr->hdice * r_ptr->hside));
		}

		/* Variable hitpoints */
		else
		{
			c_roff(TERM_WHITE, format(" and a life rating of %dd%d.  ",
									  r_ptr->hdice, r_ptr->hside));
		}
	}



	/* Collect special abilities. */
	vn = 0;
	if (flags2 & RF2_OPEN_DOOR) vp[vn++] = "open doors";
	if (flags2 & RF2_BASH_DOOR) vp[vn++] = "bash down doors";
	if (flags2 & RF2_PASS_WALL) vp[vn++] = "pass through walls";
	if (flags2 & RF2_KILL_WALL) vp[vn++] = "bore through walls";
	if (flags2 & RF2_MOVE_BODY) vp[vn++] = "push past weaker monsters";
	if (flags2 & RF2_KILL_BODY) vp[vn++] = "destroy weaker monsters";
	if (flags2 & RF2_TAKE_ITEM) vp[vn++] = "pick up objects";
	if (flags2 & RF2_KILL_ITEM) vp[vn++] = "destroy objects";
	if (flags7 & (RF7_LITE_1 | RF7_LITE_2)) vp[vn++] = "light the dungeon";

	/* Describe special abilities. */
	if (vn)
	{
		/* Intro */
		c_roff(TERM_WHITE, format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(TERM_WHITE, " can ");
			else if (n < vn - 1) c_roff(TERM_WHITE, ", ");
			else
				c_roff(TERM_WHITE, " and ");

			/* Dump */
			c_roff(TERM_L_UMBER, vp[n]);
		}

		/* End */
		c_roff(TERM_WHITE, ".  ");
	}


	/* Describe special abilities. */
	if (flags2 & RF2_INVISIBLE)
	{
		c_roff(TERM_L_BLUE, format("%^s is invisible.  ", wd_he[msex]));
	}
	if (flags2 & RF2_COLD_BLOOD)
	{
		c_roff(TERM_WHITE, format("%^s is cold blooded.  ", wd_he[msex]));
	}
	if (flags2 & RF2_EMPTY_MIND)
	{
		c_roff(TERM_WHITE,
			   format("%^s is not detected by telepathy.  ", wd_he[msex]));
	}
	if (flags2 & RF2_WEIRD_MIND)
	{
		c_roff(TERM_WHITE,
			   format("%^s is rarely detected by telepathy.  ", wd_he[msex]));
	}
	if (flags2 & RF2_MULTIPLY)
	{
		c_roff(TERM_L_UMBER, format("%^s breeds explosively.  ", wd_he[msex]));
	}
	if (flags2 & RF2_REGENERATE)
	{
		c_roff(TERM_WHITE, format("%^s regenerates quickly.  ", wd_he[msex]));
	}


	/* Collect susceptibilities */
	vn = 0;
	if (flags3 & RF3_HURT_ROCK) vp[vn++] = "rock remover";
	if (flags3 & RF3_HURT_LITE) vp[vn++] = "bright light";
	if (flags3 & RF3_HURT_FIRE) vp[vn++] = "fire";
	if (flags3 & RF3_HURT_COLD) vp[vn++] = "cold";

	/* Describe susceptibilities */
	if (vn)
	{
		/* Intro */
		c_roff(TERM_WHITE, format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(TERM_WHITE, " is hurt by ");
			else if (n < vn - 1) c_roff(TERM_WHITE, ", ");
			else
				c_roff(TERM_WHITE, " and ");

			/* Dump */
			c_roff(TERM_YELLOW, vp[n]);
		}

		/* End */
		c_roff(TERM_WHITE, ".  ");
	}


	/* Collect immunities */
	vn = 0;
	if (flags3 & RF3_IM_ACID) vp[vn++] = "acid";
	if (flags3 & RF3_IM_ELEC) vp[vn++] = "lightning";
	if (flags3 & RF3_IM_FIRE) vp[vn++] = "fire";
	if (flags3 & RF3_IM_COLD) vp[vn++] = "cold";
	if (flags3 & RF3_IM_POIS) vp[vn++] = "poison";

	/* Describe immunities */
	if (vn)
	{
		/* Intro */
		c_roff(TERM_WHITE, format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(TERM_WHITE, " resists ");
			else if (n < vn - 1) c_roff(TERM_WHITE, ", ");
			else
				c_roff(TERM_WHITE, " and ");

			/* Dump */
			c_roff(TERM_ORANGE, vp[n]);
		}

		/* End */
		c_roff(TERM_WHITE, ".  ");
	}


	/* Collect resistances */
	vn = 0;
	if (flags3 & RF3_RES_NETH) vp[vn++] = "nether";
	if (flags3 & RF3_RES_WATE) vp[vn++] = "water";
	if (flags3 & RF3_RES_PLAS) vp[vn++] = "plasma";
	if (flags3 & RF3_RES_NEXU) vp[vn++] = "nexus";
	if (flags3 & RF3_RES_DISE) vp[vn++] = "disenchantment";
	if ((flags3 & RF3_RES_TELE)
		&& !(r_ptr->flags1 & RF1_UNIQUE)) vp[vn++] = "teleportation";

	/* Describe resistances */
	if (vn)
	{
		/* Intro */
		c_roff(TERM_WHITE, format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(TERM_WHITE, " resists ");
			else if (n < vn - 1) c_roff(TERM_WHITE, ", ");
			else
				c_roff(TERM_WHITE, " and ");

			/* Dump */
			c_roff(TERM_ORANGE, vp[n]);
		}

		/* End */
		c_roff(TERM_WHITE, ".  ");
	}


	/* Collect non-effects */
	vn = 0;
	if (flags3 & RF3_NO_STUN) vp[vn++] = "stunned";
	if (flags3 & RF3_NO_FEAR) vp[vn++] = "frightened";
	if (flags3 & RF3_NO_CONF) vp[vn++] = "confused";
	if (flags3 & RF3_NO_SLEEP) vp[vn++] = "slept";
	if ((flags3 & RF3_RES_TELE)
		&& (r_ptr->flags1 & RF1_UNIQUE)) vp[vn++] = "teleported";

	/* Describe non-effects */
	if (vn)
	{
		/* Intro */
		c_roff(TERM_WHITE, format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(TERM_WHITE, " cannot be ");
			else if (n < vn - 1) c_roff(TERM_WHITE, ", ");
			else
				c_roff(TERM_WHITE, " or ");

			/* Dump */
			c_roff(TERM_YELLOW, vp[n]);
		}

		/* End */
		c_roff(TERM_WHITE, ".  ");
	}


	/* Do we know how aware it is? */
	if ((((int)r_ptr->r_wake * (int)r_ptr->r_wake) > r_ptr->sleep) ||
		(r_ptr->r_ignore == MAX_UCHAR) ||
		((r_ptr->sleep == 0) && ((r_ptr->r_tkills >= 10) || cheat_know)))
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

		c_roff(TERM_WHITE,
			   format("%^s %s intruders, which %s may notice from %d feet.  ",
					  wd_he[msex], act, wd_he[msex], 10 * r_ptr->aaf));
	}


	/* Drops gold and/or items */
	if (r_ptr->r_drop_gold || r_ptr->r_drop_item)
	{
		/* No "n" needed */
		sin = FALSE;

		/* Intro */
		c_roff(TERM_WHITE, format("%^s may carry", wd_he[msex]));

		/* Count maximum drop */
		n = MAX(r_ptr->r_drop_gold, r_ptr->r_drop_item);

		/* One drop (may need an "n") */
		if (n == 1)
		{
			c_roff(TERM_WHITE, " a");
			sin = TRUE;
		}

		/* Two drops */
		else if (n == 2)
		{
			c_roff(TERM_WHITE, " one or two");
		}

		/* Many drops */
		else
		{
			c_roff(TERM_WHITE, format(" up to %d", n));
		}


		/* Great */
		if (flags1 & RF1_DROP_GREAT)
		{
			p = " exceptional";
		}

		/* Good (no "n" needed) */
		else if (flags1 & RF1_DROP_GOOD)
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
			if (sin) c_roff(TERM_WHITE, "n");
			sin = FALSE;

			/* Dump "object(s)" */
			if (p) c_roff(TERM_WHITE, p);
			c_roff(TERM_WHITE, " object");
			if (n != 1) c_roff(TERM_WHITE, "s");

			/* Conjunction replaces variety, if needed for "gold" below */
			p = " or";
		}

		/* Treasures */
		if (r_ptr->r_drop_gold)
		{
			/* Cancel prefix */
			if (!p) sin = FALSE;

			/* Handle singular "an" */
			if (sin) c_roff(TERM_WHITE, "n");
			sin = FALSE;

			/* Dump "treasure(s)" */
			if (p) c_roff(TERM_WHITE, p);
			c_roff(TERM_WHITE, " treasure");
			if (n != 1) c_roff(TERM_WHITE, "s");
		}

		/* End this sentence */
		c_roff(TERM_WHITE, ".  ");
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

		/* Acquire the method */
		p = rbm_info[method].name;

		/* Default effect */
		q = NULL;

		/* Acquire the effect */
		switch (effect)
		{
			case RBE_HURT:
			{
				q = "attack";
				break;
			}
			case RBE_POISON:
			{
				q = "poison";
				break;
			}
			case RBE_UN_BONUS:
			{
				q = "disenchant";
				break;
			}
			case RBE_UN_POWER:
			{
				q = "drain charges";
				break;
			}
			case RBE_EAT_GOLD:
			{
				q = "steal gold";
				break;
			}
			case RBE_EAT_ITEM:
			{
				q = "steal items";
				break;
			}
			case RBE_EAT_FOOD:
			{
				q = "eat your food";
				break;
			}
			case RBE_EAT_LITE:
			{
				q = "absorb light";
				break;
			}
			case RBE_ACID:
			{
				q = "shoot acid";
				break;
			}
			case RBE_ELEC:
			{
				q = "electrocute";
				break;
			}
			case RBE_FIRE:
			{
				q = "burn";
				break;
			}
			case RBE_COLD:
			{
				q = "freeze";
				break;
			}
			case RBE_BLIND:
			{
				q = "blind";
				break;
			}
			case RBE_CONFUSE:
			{
				q = "confuse";
				break;
			}
			case RBE_TERRIFY:
			{
				q = "terrify";
				break;
			}
			case RBE_PARALYZE:
			{
				q = "paralyze";
				break;
			}
			case RBE_LOSE_STR:
			{
				q = "reduce strength";
				break;
			}
			case RBE_LOSE_INT:
			{
				q = "reduce intelligence";
				break;
			}
			case RBE_LOSE_WIS:
			{
				q = "reduce wisdom";
				break;
			}
			case RBE_LOSE_DEX:
			{
				q = "reduce dexterity";
				break;
			}
			case RBE_LOSE_CON:
			{
				q = "reduce constitution";
				break;
			}
			case RBE_LOSE_CHR:
			{
				q = "reduce charisma";
				break;
			}
			case RBE_LOSE_ALL:
			{
				q = "reduce all stats";
				break;
			}
			case RBE_SHATTER:
			{
				q = "shatter";
				break;
			}
			case RBE_EXP_10:
			{
				q = "lower experience (by 10d6+)";
				break;
			}
			case RBE_EXP_20:
			{
				q = "lower experience (by 20d6+)";
				break;
			}
			case RBE_EXP_40:
			{
				q = "lower experience (by 40d6+)";
				break;
			}
			case RBE_EXP_80:
			{
				q = "lower experience (by 80d6+)";
				break;
			}
			case RBE_DISEASE:
			{
				q = "disease";
				break;
			}
			case RBE_TIME:
			{
				q = "time";
				break;
			}
			case RBE_EXP_VAMP:
			{
				q = "drain life force";
				break;
			}
		}


		/* Introduce the attack description */
		if (!r)
		{
			c_roff(TERM_WHITE, format("%^s can ", wd_he[msex]));
		}
		else if (r < n - 1)
		{
			c_roff(TERM_WHITE, ", ");
		}
		else
		{
			c_roff(TERM_WHITE, ", and ");
		}


		/* Hack -- force a method */
		if (!p) p = "do something weird";

		/* Describe the method */
		c_roff(TERM_WHITE, p);


		/* Describe the effect (if any) */
		if (q)
		{
			/* Describe the attack type */
			c_roff(TERM_WHITE, " to ");
			c_roff(TERM_L_RED, q);

			/* Describe damage (if known) */
			if (d1 && d2 && know_damage(r_idx, m))
			{
				/* Display the damage */
				c_roff(TERM_WHITE, " with damage");
				c_roff(TERM_WHITE, format(" %dd%d", d1, d2));
			}
		}


		/* Count the attacks as printed */
		r++;
	}

	/* Finish sentence above */
	if (r)
	{
		c_roff(TERM_WHITE, ".  ");
	}

	/* Notice lack of attacks */
	else if (flags1 & RF1_NEVER_BLOW)
	{
		c_roff(TERM_WHITE,
			   format("%^s has no physical attacks.  ", wd_he[msex]));
	}

	/* Or describe the lack of knowledge */
	else
	{
		c_roff(TERM_WHITE,
			   format("Nothing is known about %s attack.  ", wd_his[msex]));
	}


	/*
	 * Notice "Quest" monsters, but only if you
	 * already encountered the monster.
	 */
	if ((flags1 & RF1_QUESTOR) && (r_ptr->r_sights))
	{
		c_roff(TERM_WHITE,
			   "You feel an intense desire to kill this monster...  ");
	}


	/* All done */
	c_roff(TERM_WHITE, "\n");

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
	monster_race *r_ptr = &r_info[r_idx];

	byte a1, a2;
	char c1, c2;


	/* Access the chars */
	c1 = r_ptr->d_char;
	c2 = r_ptr->x_char;

	/* Access the attrs */
	a1 = r_ptr->d_attr;
	a2 = r_ptr->x_attr;

	/* Hack -- fake monochrome */
	if (!use_color || ironman_moria)
	{
		a1 = TERM_WHITE;
		a2 = TERM_WHITE;
	}


	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* A title (use "The" for non-uniques) */
	if (!(r_ptr->flags1 & RF1_UNIQUE))
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

	/* Wizards get extra info */
	if (p_ptr->wizard)
	{
		char buf[6];

		sprintf(buf, "%d", r_idx);

		Term_addstr(-1, TERM_WHITE, " (");
		Term_addstr(-1, TERM_L_BLUE, buf);
		Term_addch(TERM_WHITE, ')');
	}
}



/*
 * Hack -- describe the given monster race at the top of the screen
 */
void screen_roff(int r_idx, int remember)
{
	/* Flush messages */
	message_flush();

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


/*
 * Hack -- show a list of the visible monsters in the current "term" window
 */
void display_visible(void)
{
	int i, y;
	char c1, c2;
	byte a1, a2;
	monster_race *r_ptr;


	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* Are we hallucinating? */
	if (p_ptr->image)
	{
		/* Go to top left of screen */
		Term_gotoxy(0, 10);

		Term_addstr(-1, TERM_VIOLET, "Hallucinations");

		return;
	}

	i = p_ptr->max_seen_r_idx;

	/* Show the list */
	for (y = 0; y < Term->hgt; y++)
	{
		/* No more to display */
		if (!i) return;

		/* Go to left of screen */
		Term_gotoxy(0, y);

		/* Note we have assumed that r_info.txt has been sorted */

		/* Access monster */
		r_ptr = &r_info[i];

		/* Access the chars */
		c1 = r_ptr->d_char;
		c2 = r_ptr->x_char;

		/* Access the attrs */
		a1 = r_ptr->d_attr;
		a2 = r_ptr->x_attr;

		/* Hack -- fake monochrome */
		if (!use_color || ironman_moria)
		{
			a1 = TERM_WHITE;
			a2 = TERM_WHITE;
		}

		/* Dump the name */
		if (r_ptr->flags1 & RF1_UNIQUE)
		{
			Term_addstr(-1, TERM_L_BLUE, (r_name + r_ptr->name));
		}
		else
		{
			Term_addstr(-1, TERM_WHITE, (r_name + r_ptr->name));
		}

		/* Append the "standard" attr/char info */
		Term_addstr(-1, TERM_WHITE, " ('");
		Term_addch(a1, c1);
		Term_addstr(-1, TERM_WHITE, "')");

		/* Append the "optional" attr/char info */
		Term_addstr(-1, TERM_WHITE, "/('");
		Term_addch(a2, c2);
		Term_addstr(-1, TERM_WHITE, "'):");

		/* Wizards get extra info */
		if (p_ptr->wizard)
		{
			char buf[6];

			sprintf(buf, "%d", i);

			Term_addstr(-1, TERM_WHITE, " (");
			Term_addstr(-1, TERM_L_BLUE, buf);
			Term_addch(TERM_WHITE, ')');
		}

		/* Append count */
		Term_addstr(-1, TERM_WHITE, format("[%d]", r_ptr->r_see));

		/* Look for the next one */
		while (i > 0)
		{
			i--;

			if (r_info[i].r_see)
			{
				break;
			}
		}
	}
}



static byte mon_wild;
static monster_hook_type wild_mon_hook;

static bool validate_mon_wild(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Want first 8 flags, do not want next 8. */
	return ((r_ptr->flags8 & 0x000000FF) && (!(r_ptr->flags8 & 0x0000FF00)));
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

bool monster_town(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & RF8_WILD_TOWN)
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

bool monster_deep_water_dun(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (!monster_dungeon(r_idx)) return FALSE;

	if (r_ptr->flags7 & RF7_AQUATIC)
		return TRUE;
	else
		return FALSE;
}


bool monster_shallow_water_dun(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (!monster_dungeon(r_idx)) return FALSE;

	if (r_ptr->flags2 & RF2_AURA_FIRE)
		return FALSE;
	else
		return TRUE;
}


bool monster_lava_dun(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (!monster_dungeon(r_idx)) return FALSE;

	if (((r_ptr->flags3 & RF3_IM_FIRE) ||
		 (r_ptr->flags7 & RF7_CAN_FLY)) && !(r_ptr->flags3 & RF3_AURA_COLD))
		return TRUE;
	else
		return FALSE;
}

bool monster_acid_dun(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (!monster_dungeon(r_idx)) return FALSE;

	if (((r_ptr->flags3 & RF3_IM_ACID) || (r_ptr->flags7 & RF7_CAN_FLY)))
		return TRUE;
	else
		return FALSE;
}

bool monster_swamp_dun(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (!monster_dungeon(r_idx)) return FALSE;

	if (((r_ptr->flags3 & RF3_IM_POIS) || (r_ptr->flags7 & RF7_CAN_FLY)))
		return TRUE;
	else
		return FALSE;
}

bool monster_deep_water_wild(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Check wilderness flags */
	if (!wild_mon_hook(r_idx)) return FALSE;

	if (r_ptr->flags7 & RF7_AQUATIC)
		return TRUE;
	else
		return FALSE;
}


bool monster_shallow_water_wild(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Check wilderness flags */
	if (!wild_mon_hook(r_idx)) return FALSE;

	if (r_ptr->flags2 & RF2_AURA_FIRE)
		return FALSE;
	else
		return TRUE;
}


bool monster_lava_wild(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Check wilderness flags */
	if (!wild_mon_hook(r_idx)) return FALSE;

	if (((r_ptr->flags3 & RF3_IM_FIRE) ||
		 (r_ptr->flags7 & RF7_CAN_FLY)) && !(r_ptr->flags3 & RF3_AURA_COLD))
		return TRUE;
	else
		return FALSE;
}

bool monster_acid_wild(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Check wilderness flags */
	if (!wild_mon_hook(r_idx)) return FALSE;

	if (((r_ptr->flags3 & RF3_IM_ACID) || (r_ptr->flags7 & RF7_CAN_FLY)))
		return TRUE;
	else
		return FALSE;
}

bool monster_swamp_wild(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Check wilderness flags */
	if (!wild_mon_hook(r_idx)) return FALSE;

	if (((r_ptr->flags3 & RF3_IM_POIS) || (r_ptr->flags7 & RF7_CAN_FLY)))
		return TRUE;
	else
		return FALSE;
}


monster_hook_type get_monster_hook(void)
{
	if (p_ptr->depth)
	{
		/* In dungeon */
		return &(monster_dungeon);
	}

	/* Not in dungeon */
	return NULL;
}


monster_hook_type get_monster_hook2(int x, int y)
{
	wild_done_type *w_ptr;

	/* In dungeon */
	if (p_ptr->depth)
	{
		/* Set the monster list */
		switch (area(x, y)->feat)
		{
			case FEAT_SHAL_WATER:
			{
				return &(monster_shallow_water_dun);
			}
			case FEAT_DEEP_WATER:
			{
				return &(monster_deep_water_dun);
			}
			case FEAT_DEEP_LAVA:
			case FEAT_SHAL_LAVA:
			{
				return &(monster_lava_dun);
			}
			case FEAT_DEEP_ACID:
			case FEAT_SHAL_ACID:
			{
				return &(monster_acid_dun);
			}
			case FEAT_DEEP_SWAMP:
			case FEAT_SHAL_SWAMP:
			{
				return &(monster_swamp_dun);
			}
			default:
			{
				return NULL;
			}
		}
	}

	/* Point to wilderness block info */
	w_ptr = &wild[y / 16][x / 16].done;


	/* Mega Hack XXX XXX- Set level of monster */
	/* This breaks summoning level changes. */
	monster_level = w_ptr->mon_gen;


	if (w_ptr->wild > WILD_SEA)
	{
		/* Ocean */
		wild_mon_hook = &monster_ocean;
	}
	else if (w_ptr->info & WILD_INFO_WATER)
	{
		/* Shore */
		wild_mon_hook = &monster_shore;
	}
	else if (w_ptr->info & WILD_INFO_ACID)
	{
		/* Acid */
		wild_mon_hook = &monster_acid_wild;
	}
	else if (w_ptr->info & WILD_INFO_LAVA)
	{
		/* Lava */
		wild_mon_hook = &monster_lava_wild;
	}
	else
	{
		/*
		 * Get wilderness type flags and store
		 * into static variable above.
		 */
		mon_wild = wild_gen_data[w_ptr->wild].rough_type;

		/* Set wilderness hook */
		if (mon_wild == 0)
		{
			/* No other terrain - use grass */
			wild_mon_hook = &monster_grass;
		}
		else
		{
			/* Normal wilderness terrain */
			wild_mon_hook = &validate_mon_wild;
		}
	}

	if (w_ptr->place)
	{
		/* Have a place. Hack - use town hook. */
		wild_mon_hook = &monster_town;
	}

	/* Set the monster list */
	switch (area(x, y)->feat)
	{
		case FEAT_SHAL_WATER:
		{
			return &(monster_shallow_water_wild);
		}
		case FEAT_DEEP_WATER:
		{
			return &(monster_deep_water_wild);
		}
		case FEAT_OCEAN_WATER:
		{
			return (wild_mon_hook);
		}
		case FEAT_DEEP_LAVA:
		case FEAT_SHAL_LAVA:
		{
			return &(monster_lava_wild);
		}
		case FEAT_DEEP_ACID:
		case FEAT_SHAL_ACID:
		{
			return &(monster_acid_wild);
		}
		case FEAT_DEEP_SWAMP:
		case FEAT_SHAL_SWAMP:
		{
			return &(monster_swamp_wild);
		}
		default:
		{
			return (wild_mon_hook);
		}
	}
}


void set_friendly(monster_type *m_ptr)
{
	m_ptr->smart |= SM_FRIENDLY;
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
 * Anger the monster
 */
void anger_monster(monster_type *m_ptr)
{
	if (!is_hostile(m_ptr))
	{
		char m_name[80];

		monster_desc(m_name, m_ptr, 0);
		msg_format("%^s gets angry!", m_name);
		set_hostile(m_ptr);

		chg_virtue(V_INDIVIDUALISM, 1);
		chg_virtue(V_HONOUR, -1);
		chg_virtue(V_JUSTICE, -1);
		chg_virtue(V_COMPASSION, -1);
	}
}


/*
 * Check if monster can cross terrain
 */
bool monster_can_cross_terrain(byte feat, monster_race *r_ptr)
{
	/* Ocean */
	if (feat == FEAT_OCEAN_WATER)
	{
		if (r_ptr->flags8 & RF8_WILD_OCEAN)
			return TRUE;
		else
			return FALSE;
	}

	/* Deep water */
	if (feat == FEAT_DEEP_WATER)
	{
		if ((r_ptr->flags7 & RF7_AQUATIC) ||
			(r_ptr->flags7 & RF7_CAN_FLY) || (r_ptr->flags7 & RF7_CAN_SWIM))
			return TRUE;
		else
			return FALSE;
	}

	/* Shallow water */
	if (feat == FEAT_SHAL_WATER)
	{
		if (r_ptr->flags2 & RF2_AURA_FIRE)
			return FALSE;
		else
			return TRUE;
	}

	/* Aquatic monster */
	if ((r_ptr->flags7 & RF7_AQUATIC) && !(r_ptr->flags7 & RF7_CAN_FLY))
	{
		return FALSE;
	}

	/* Lava */
	if ((feat == FEAT_SHAL_LAVA) || (feat == FEAT_DEEP_LAVA))
	{
		if ((r_ptr->flags3 & RF3_IM_FIRE) || (r_ptr->flags7 & RF7_CAN_FLY))
			return TRUE;
		else
			return FALSE;
	}

	/* Acid */
	if ((feat == FEAT_SHAL_ACID) || (feat == FEAT_DEEP_ACID))
	{
		if ((r_ptr->flags3 & RF3_IM_ACID) || (r_ptr->flags7 & RF7_CAN_FLY))
			return TRUE;
		else
			return FALSE;
	}

	/* Swamp */
	if ((feat == FEAT_SHAL_SWAMP) || (feat == FEAT_DEEP_SWAMP))
	{
		if ((r_ptr->flags3 & RF3_IM_POIS) || (r_ptr->flags7 & RF7_CAN_FLY))
			return TRUE;
		else
			return FALSE;
	}

	return TRUE;
}


/*
 * Check if two monsters are enemies
 */
bool are_enemies(const monster_type *m_ptr, const monster_type *n_ptr)
{
	const monster_race *r_ptr = &r_info[m_ptr->r_idx];
	const monster_race *s_ptr = &r_info[n_ptr->r_idx];

	/* Friendly vs. opposite aligned normal or pet */
	if (((r_ptr->flags3 & RF3_EVIL) &&
		 (s_ptr->flags3 & RF3_GOOD)) ||
		((r_ptr->flags3 & RF3_GOOD) && (s_ptr->flags3 & RF3_EVIL)))
	{
		return TRUE;
	}

	/* Hostile vs. non-hostile */
	if (is_hostile(m_ptr) != is_hostile(n_ptr))
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
bool monster_living(const monster_race *r_ptr)
{
	/* Non-living, undead, or demon */
	if (r_ptr->flags3 & (RF3_DEMON | RF3_UNDEAD | RF3_NONLIVING))
		return FALSE;
	else
		return TRUE;
}



/* Dwarves */
static cptr dwarf_syllable1[] =
{
	"B", "D", "F", "G", "Gl", "H", "K", "L", "M", "N", "R", "S", "T", "Th", "V",
};

static cptr dwarf_syllable2[] =
{
	"a", "e", "i", "o", "oi", "u",
};

static cptr dwarf_syllable3[] =
{
	"bur", "fur", "gan", "gnus", "gnar", "li", "lin", "lir", "mli", "nar",
	"nus", "rin", "ran", "sin", "sil", "sur",
};

/* Elves */
static cptr elf_syllable1[] =
{
	"Al", "An", "Bal", "Bel", "Cal", "Cel", "El", "Elr", "Elv", "Eow", "F",
	"Fal", "Fel", "Fin", "G", "Gal", "Gel", "Gl", "Is", "Lan", "Leg", "N",
	"Nal", "Nel", "S", "Sal", "Sel", "T", "Tal", "Tel", "Thr", "Tin",
};

static cptr elf_syllable2[] =
{
	"a", "adrie", "ara", "e", "ebri", "ele", "ere", "i", "io", "ithra",
	"ilma", "il-Ga", "ili", "o", "orfi", "u", "y",
};

static cptr elf_syllable3[] =
{
	"l", "las", "lad", "ldor", "ldur", "linde", "lith", "mir", "n", "nd",
	"ndel", "ndil", "ndir", "nduil", "ng", "mbor", "r", "rith", "ril",
	"riand", "rion", "s", "thien", "viel", "wen", "wyn",
};

/* Gnomes */
static cptr gnome_syllable1[] =
{
	"Aar", "An", "Ar", "As", "C", "H", "Han", "Har", "Hel", "Iir", "J",
	"Jan", "Jar", "K", "L", "M", "Mar", "N", "Nik", "Os", "Ol", "P", "R",
	"S", "Sam", "San", "T", "Ter", "Tom", "Ul", "V", "W", "Y",
};

static cptr gnome_syllable2[] =
{
	"a", "aa", "ai", "e", "ei", "i", "o", "uo", "u", "uu",
};

static cptr gnome_syllable3[] =
{
	"ron", "re", "la", "ki", "kseli", "ksi", "ku", "ja", "ta", "na",
	"namari", "neli", "nika", "nikki", "nu", "nukka", "ka", "ko", "li",
	"kki", "rik", "po", "to", "pekka", "rjaana", "rjatta", "rjukka", "la",
	"lla", "lli", "mo", "nni",
};

/* Hobbit */
static cptr hobbit_syllable1[] =
{
	"B", "Ber", "Br", "D", "Der", "Dr", "F", "Fr", "G", "H", "L", "Ler",
	"M", "Mer", "N", "P", "Pr", "Per", "R", "S", "T", "W",
};

static cptr hobbit_syllable2[] =
{
	"a", "e", "i", "ia", "o", "oi", "u",
};

static cptr hobbit_syllable3[] =
{
	"bo", "ck", "decan", "degar", "do", "doc", "go", "grin", "lba", "lbo",
	"lda", "ldo", "lla", "ll", "lo", "m", "mwise", "nac", "noc", "nwise",
	"p", "ppin", "pper", "tho", "to",
};

/* Human */
static cptr human_syllable1[] =
{
	"Ab", "Ac", "Ad", "Af", "Agr", "Ast", "As", "Al", "Adw", "Adr", "Ar",
	"B", "Br", "C", "Cr", "Ch", "Cad", "D", "Dr", "Dw", "Ed", "Eth", "Et",
	"Er", "El", "Eow", "F", "Fr", "G", "Gr", "Gw", "Gal", "Gl", "H", "Ha",
	"Ib", "Jer", "K", "Ka", "Ked", "L", "Loth", "Lar", "Leg", "M", "Mir",
	"N", "Nyd", "Ol", "Oc", "On", "P", "Pr", "R", "Rh", "S", "Sev", "T",
	"Tr", "Th", "V", "Y", "Z", "W", "Wic",
};

static cptr human_syllable2[] =
{
	"a", "ae", "au", "ao", "are", "ale", "ali", "ay", "ardo", "e", "ei",
	"ea", "eri", "era", "ela", "eli", "enda", "erra", "i", "ia", "ie",
	"ire", "ira", "ila", "ili", "ira", "igo", "o", "oa", "oi", "oe",
	"ore", "u", "y",
};

static cptr human_syllable3[] =
{
	"a", "and", "b", "bwyn", "baen", "bard", "c", "ctred", "cred", "ch",
	"can", "d", "dan", "don", "der", "dric", "dfrid", "dus", "f", "g",
	"gord", "gan", "l", "li", "lgrin", "lin", "lith", "lath", "loth", "ld",
	"ldric", "ldan", "m", "mas", "mos", "mar", "mond", "n", "nydd", "nidd",
	"nnon", "nwan", "nyth", "nad", "nn", "nnor", "nd", "p", "r", "ron",
	"rd", "s", "sh", "seth", "sean", "t", "th", "tha", "tlan", "trem",
	"tram", "v", "vudd", "w", "wan", "win", "wyn", "wyr", "wyr", "wyth",
};

/* Orc */
static cptr orc_syllable1[] =
{
	"B", "Er", "G", "Gr", "H", "P", "Pr", "R", "V", "Vr", "T", "Tr", "M", "Dr",
};

static cptr orc_syllable2[] =
{
	"a", "i", "o", "oo", "u", "ui",
};

static cptr orc_syllable3[] =
{
	"dash", "dish", "dush", "gar", "gor", "gdush", "lo", "gdish", "k",
	"lg", "nak", "rag", "rbag", "rg", "rk", "ng", "nk", "rt", "ol", "urk",
	"shnak", "mog", "mak", "rak",
};


/*
 * Random Name Generator
 * based on a Javascript by Michael Hensley
 * "http://geocities.com/timessquare/castle/6274/"
 */
void create_name(int type, char *name)
{
	int cs = sizeof(char *);

	/* Paranoia */
	if (!name) return;

	/* Select the monster type */
	switch (type)
	{
			/* Create the monster name */
		case NAME_DWARF:
		{
			strcpy(name,
				   dwarf_syllable1[randint0(sizeof(dwarf_syllable1) / cs)]);
			strcat(name,
				   dwarf_syllable2[randint0(sizeof(dwarf_syllable2) / cs)]);
			strcat(name,
				   dwarf_syllable3[randint0(sizeof(dwarf_syllable3) / cs)]);
			break;
		}

		case NAME_ELF:
		{
			strcpy(name, elf_syllable1[randint0(sizeof(elf_syllable1) / cs)]);
			strcat(name, elf_syllable2[randint0(sizeof(elf_syllable2) / cs)]);
			strcat(name, elf_syllable3[randint0(sizeof(elf_syllable3) / cs)]);
			break;
		}

		case NAME_GNOME:
		{
			strcpy(name,
				   gnome_syllable1[randint0(sizeof(gnome_syllable1) / cs)]);
			strcat(name,
				   gnome_syllable2[randint0(sizeof(gnome_syllable2) / cs)]);
			strcat(name,
				   gnome_syllable3[randint0(sizeof(gnome_syllable3) / cs)]);
			break;
		}

		case NAME_HOBBIT:
		{
			strcpy(name, hobbit_syllable1[randint0(sizeof(hobbit_syllable1)
												   / cs)]);
			strcat(name, hobbit_syllable2[randint0(sizeof(hobbit_syllable2)
												   / cs)]);
			strcat(name, hobbit_syllable3[randint0(sizeof(hobbit_syllable3)
												   / cs)]);
			break;
		}

		case NAME_HUMAN:
		{
			strcpy(name,
				   human_syllable1[randint0(sizeof(human_syllable1) / cs)]);
			strcat(name,
				   human_syllable2[randint0(sizeof(human_syllable2) / cs)]);
			strcat(name,
				   human_syllable3[randint0(sizeof(human_syllable3) / cs)]);
			break;
		}

		case NAME_ORC:
		{
			strcpy(name, orc_syllable1[randint0(sizeof(orc_syllable1) / cs)]);
			strcat(name, orc_syllable2[randint0(sizeof(orc_syllable2) / cs)]);
			strcat(name, orc_syllable3[randint0(sizeof(orc_syllable3) / cs)]);
			break;
		}

			/* Create an empty name */
		default:
		{
			name[0] = '\0';
			break;
		}
	}
}
