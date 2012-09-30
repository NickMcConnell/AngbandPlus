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
 * Get the monster race in r_info[]
 */
monster_race *monst_race(int r_idx)
{
	return (&r_info[r_idx]);
}

/*
 * Get the monster name from r_info[]
 */
cptr mon_race_name(const monster_race *r_ptr)
{
	return (r_name + r_ptr->name);
}

/*
 * Does the monster name contain string str?
 */
bool mon_name_cont(const monster_race *r_ptr, cptr str)
{
	return (strstr(mon_race_name(r_ptr), str) ? TRUE : FALSE);
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

	/* Normal monsters */
	if (kills > 304 / (4 + level)) return (TRUE);

	/* Skip non-uniques */
	if (!(FLAG(r_ptr, RF_UNIQUE))) return (FALSE);

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
	if (!(FLAG(r_ptr, RF_UNIQUE))) return (FALSE);

	/* Unique monsters */
	if ((4 + level) * (2 * a) > 80 * d) return (TRUE);

	/* Assume false */
	return (FALSE);
}

/*
 * Hack - a type for 'monster flags'
 */
typedef struct monster_flags monster_flags;

struct monster_flags
{
	u32b flags[7];
};


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
static void roff_mon_aux(int r_idx, int remem)
{
	monster_race *r_ptr = &r_info[r_idx];

	bool old = FALSE;
	bool sin = FALSE;

	/* Should all knowledge be displayed? */
	bool know_all = cheat_know || (r_ptr->r_flags[6] & RF6_LIBRARY);

	int m, n, r;
	cptr p, q;

	int msex = 0;
	int speed = (ironman_nightmare) ? r_ptr->speed + 5 : r_ptr->speed;

	bool breath = FALSE;
	bool magic = FALSE;

	monster_flags mflags;
	monster_flags *mf_ptr = &mflags;

	int vn = 0;
	cptr vp[80];

	monster_race save_mem;

	/* Descriptions */
	char buf[2048];

	/* Cheat -- Know everything */
	if (know_all)
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
			(((FLAG(r_ptr, RF_DROP_4D2)) ? 8 : 0) +
			 ((FLAG(r_ptr, RF_DROP_3D2)) ? 6 : 0) +
			 ((FLAG(r_ptr, RF_DROP_2D2)) ? 4 : 0) +
			 ((FLAG(r_ptr, RF_DROP_1D2)) ? 2 : 0) +
			 ((FLAG(r_ptr, RF_DROP_90)) ? 1 : 0) +
			 ((FLAG(r_ptr, RF_DROP_60)) ? 1 : 0));

		/* Hack -- but only "valid" drops */
		if (FLAG(r_ptr, RF_ONLY_GOLD)) r_ptr->r_drop_item = 0;
		if (FLAG(r_ptr, RF_ONLY_ITEM)) r_ptr->r_drop_gold = 0;

		/* Hack -- observe many spells */
		r_ptr->r_cast_inate = MAX_UCHAR;
		r_ptr->r_cast_spell = MAX_UCHAR;

		/* Hack -- know all the flags */
		r_ptr->r_flags[0] = r_ptr->flags[0];
		r_ptr->r_flags[1] = r_ptr->flags[1];
		r_ptr->r_flags[2] = r_ptr->flags[2];
		r_ptr->r_flags[3] = r_ptr->flags[3];
		r_ptr->r_flags[4] = r_ptr->flags[4];
		r_ptr->r_flags[5] = r_ptr->flags[5];
	}


	/* Extract a gender (if applicable) */
	if (FLAG(r_ptr, RF_FEMALE)) msex = 2;
	else if (FLAG(r_ptr, RF_MALE)) msex = 1;


	/* Obtain a copy of the "known" flags */
	mf_ptr->flags[0] = (r_ptr->flags[0] & r_ptr->r_flags[0]);
	mf_ptr->flags[1] = (r_ptr->flags[1] & r_ptr->r_flags[1]);
	mf_ptr->flags[2] = (r_ptr->flags[2] & r_ptr->r_flags[2]);
	mf_ptr->flags[3] = (r_ptr->flags[3] & r_ptr->r_flags[3]);
	mf_ptr->flags[4] = (r_ptr->flags[4] & r_ptr->r_flags[4]);
	mf_ptr->flags[5] = (r_ptr->flags[5] & r_ptr->r_flags[5]);
	mf_ptr->flags[6] = (r_ptr->flags[6]);

	/*
	 * Hack.  All flags from flag[6] are known.  But the swimming flag should
	 * not be known until discovered.  I suppose I can write it in one
	 * line of code but then noone knows why I did so.

	 * So if the monster is not known to be a swimmer
	 */
	if  (!(r_ptr->r_flags[6] & RF6_CAN_SWIM))
	{
		/* Take away the swimming flag */
		mf_ptr->flags[6] &= ~(RF6_CAN_SWIM);
	}


	/* Assume some "obvious" flags */
	COPY_FLAG(r_ptr, mf_ptr, RF_UNIQUE);
	COPY_FLAG(r_ptr, mf_ptr, RF_QUESTOR);
	COPY_FLAG(r_ptr, mf_ptr, RF_MALE);
	COPY_FLAG(r_ptr, mf_ptr, RF_FEMALE);

	/* Assume some "creation" flags */
	COPY_FLAG(r_ptr, mf_ptr, RF_CHAR_MIMIC);
	COPY_FLAG(r_ptr, mf_ptr, RF_FRIENDS);
	COPY_FLAG(r_ptr, mf_ptr, RF_ESCORT);
	COPY_FLAG(r_ptr, mf_ptr, RF_ESCORTS);

	/* Killing a monster reveals some properties */
	if (r_ptr->r_tkills || know_all)
	{
		/* Know "race" flags */
		COPY_FLAG(r_ptr, mf_ptr, RF_ORC);
		COPY_FLAG(r_ptr, mf_ptr, RF_TROLL);
		COPY_FLAG(r_ptr, mf_ptr, RF_GIANT);
		COPY_FLAG(r_ptr, mf_ptr, RF_DRAGON);
		COPY_FLAG(r_ptr, mf_ptr, RF_DEMON);
		COPY_FLAG(r_ptr, mf_ptr, RF_UNDEAD);
		COPY_FLAG(r_ptr, mf_ptr, RF_EVIL);
		COPY_FLAG(r_ptr, mf_ptr, RF_GOOD);
		COPY_FLAG(r_ptr, mf_ptr, RF_ANIMAL);
		COPY_FLAG(r_ptr, mf_ptr, RF_AMBERITE);

		/* Know 'quantum' flag */
		COPY_FLAG(r_ptr, mf_ptr, RF_QUANTUM);

		/* Know "forced" flags */
		COPY_FLAG(r_ptr, mf_ptr, RF_FORCE_DEPTH);
		COPY_FLAG(r_ptr, mf_ptr, RF_FORCE_MAXHP);
	}

	/* Treat uniques differently */
	if (FLAG(mf_ptr, RF_UNIQUE))
	{
		/* Hack -- Determine if the unique is "dead" */
		bool dead = (r_ptr->max_num == 0) ? TRUE : FALSE;

		/* We've been killed... */
		if (r_ptr->r_deaths)
		{
			/* Killed ancestors */
			roff("%^s has slain %d of your ancestors",
									  wd_he[msex], r_ptr->r_deaths);

			/* But we've also killed it */
			if (dead)
			{
				roff(", but you have avenged %s!  ",
										  plural(r_ptr->r_deaths, "him",
												 "them"));
			}

			/* Unavenged (ever) */
			else
			{
				roff(", who %s unavenged.  ",
										  plural(r_ptr->r_deaths, "remains",
												 "remain"));
			}
		}

		/* Dead unique who never hurt us */
		else if (dead)
		{
			roff(CLR_L_DARK "You have slain this foe.  ");
		}
	}

	/* Not unique, but killed us */
	else if (r_ptr->r_deaths)
	{
		/* Dead ancestors */
		roff("%d of your ancestors %s been killed by this creature, ",
					  r_ptr->r_deaths, plural(r_ptr->r_deaths, "has", "have"));

		/* Some kills this life */
		if (r_ptr->r_pkills)
		{
			roff("and you have exterminated at least %d of the creatures.  ",
					r_ptr->r_pkills);
		}

		/* Some kills past lives */
		else if (r_ptr->r_tkills)
		{
			roff("and %s have exterminated at least %d of the creatures.  ",
					"your ancestors", r_ptr->r_tkills);
		}

		/* No kills */
		else
		{
			roff(CLR_RED "and %s is not ever known to have been defeated.  ",
						  wd_he[msex]);
		}
	}

	/* Normal monsters */
	else
	{
		/* Killed some this life */
		if (r_ptr->r_pkills)
		{
			roff("You have killed at least %d of these creatures.  ",
						  r_ptr->r_pkills);
		}

		/* Killed some last life */
		else if (r_ptr->r_tkills)
		{
			roff("Your ancestors have killed at least %d of these creatures.  ",
					r_ptr->r_tkills);
		}

		/* Killed none */
		else
		{
			roff("No battles to the death are recalled.  ");
		}
	}


#ifdef DELAY_LOAD_R_TEXT

	int fd;

	/* Build the filename */
	path_make(buf, ANGBAND_DIR_DATA, "r_info.raw");

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
	roff(buf);
	roff("  ");


	/* Nothing yet */
	old = FALSE;

	/* Describe location */
	if (r_ptr->level == 0)
	{
		roff("%^s lives in the town", wd_he[msex]);
		old = TRUE;
	}
	else if (r_ptr->r_tkills || know_all)
	{
		roff(CLR_SLATE "%^s is ", wd_he[msex]);

		if (r_ptr->r_tkills * r_ptr->rarity >= 30 || know_all)
		{
			if (r_ptr->rarity < 2)
				roff(CLR_SLATE "very common");
			else if (r_ptr->rarity < 4)
				roff(CLR_SLATE "common");
			else if (r_ptr->rarity < 8)
				roff(CLR_SLATE "uncommon");
			else if (r_ptr->rarity < 16)
				roff(CLR_SLATE "rare");
			else
				roff(CLR_SLATE "very rare");
		}
		else
		{
			roff(CLR_SLATE "normally found");
		}

		if (FLAG(mf_ptr, RF_AQUATIC))
		{
			roff(CLR_SLATE " in water");
		}
		
		if (depth_in_feet)
		{
			roff(CLR_SLATE " at depths of %d feet", r_ptr->level * 50);
		}
		else
		{
			roff(CLR_SLATE " on dungeon level %d", r_ptr->level);
		}
		old = TRUE;
	}


	/* Describe movement */
	if (TRUE)
	{
		/* Introduction */
		if (old)
		{
			roff(" and ");
		}
		else
		{
			roff("%^s ", wd_he[msex]);
			old = TRUE;
		}

		if (FLAG(mf_ptr, RF_CAN_FLY))
		{
			roff("flies");
		}
		else
			roff("moves");

		/* Random-ness */
		if ((FLAG(mf_ptr, RF_RAND_50)) || (FLAG(mf_ptr, RF_RAND_25)))
		{
			/* Adverb */
			if ((FLAG(mf_ptr, RF_RAND_50)) && (FLAG(mf_ptr, RF_RAND_25)))
			{
				roff(" extremely");
			}
			else if (FLAG(mf_ptr, RF_RAND_50))
			{
				roff(" somewhat");
			}
			else if (FLAG(mf_ptr, RF_RAND_25))
			{
				roff(" a bit");
			}

			/* Adjective */
			roff(" erratically");

			/* Hack -- Occasional conjunction */
			if (speed != 110) roff(" and");
		}

		/* Speed */
		if (speed > 110)
		{
			if (speed > 130) roff(CLR_GREEN " incredibly");
			else if (speed > 120) roff(CLR_GREEN " very");
			roff(CLR_GREEN " quickly");
		}
		else if (speed < 110)
		{
			if (speed < 90) roff(CLR_GREEN " incredibly");
			else if (speed < 100) roff(CLR_GREEN " very");
			roff(CLR_GREEN " slowly");
		}
		else
		{
			roff(CLR_GREEN " at normal speed");
		}
	}

	/* The code above includes "attack speed" */
	if (FLAG(mf_ptr, RF_NEVER_MOVE))
	{
		/* Introduce */
		if (old)
		{
			roff(", but ");
		}
		else
		{
			roff("%^s ", wd_he[msex]);
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
	if (r_ptr->r_tkills || know_all)
	{
		/* Introduction */
		if (FLAG(mf_ptr, RF_UNIQUE))
		{
			roff("Killing this");
		}
		else
		{
			roff("A kill of this");
		}

		/* Describe the "quality" */
		if (FLAG(mf_ptr, RF_XXX_1)) roff(CLR_L_BLUE " some property");
		if (FLAG(mf_ptr, RF_ANIMAL)) roff(CLR_L_BLUE " natural");
		if (FLAG(mf_ptr, RF_EVIL)) roff(CLR_L_BLUE " evil");
		if (FLAG(mf_ptr, RF_GOOD)) roff(CLR_L_BLUE " good");
		if (FLAG(mf_ptr, RF_UNDEAD)) roff(CLR_L_BLUE " undead");

		/* Describe the "race" */
		if (FLAG(mf_ptr, RF_DRAGON)) roff(CLR_L_BLUE " dragon");
		else if (FLAG(mf_ptr, RF_DEMON)) roff(CLR_L_BLUE " demon");
		else if (FLAG(mf_ptr, RF_GIANT)) roff(CLR_L_BLUE " giant");
		else if (FLAG(mf_ptr, RF_TROLL)) roff(CLR_L_BLUE " troll");
		else if (FLAG(mf_ptr, RF_ORC)) roff(CLR_L_BLUE " orc");
		else if (FLAG(mf_ptr, RF_AMBERITE)) roff(CLR_L_BLUE " Amberite");
		else if (FLAG(mf_ptr, RF_QUANTUM)) roff(CLR_L_BLUE " quantum creature");
		else
			roff(" creature");

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
			roff(" is worth %ld.%02ld point%s",
					(long)new_exp, (long)new_exp_frac,
					(((new_exp == 1) && (new_exp_frac == 0)) ? "" : "s"));

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
			roff(" for a%s %lu%s level character.  ", q, (long)i, p);
		}
	}

	if ((FLAG(mf_ptr, RF_AURA_FIRE)) && (FLAG(mf_ptr, RF_AURA_ELEC)))
	{
		roff(CLR_YELLOW "%^s is surrounded by flames and electricity.  ",
					  wd_he[msex]);
	}
	else if ((FLAG(mf_ptr, RF_AURA_COLD)) && (FLAG(mf_ptr, RF_AURA_ELEC)))
	{
		roff(CLR_YELLOW "%^s is surrounded by ice and electricity.  ",
					  wd_he[msex]);
	}
	else if (FLAG(mf_ptr, RF_AURA_FIRE))
	{
		roff(CLR_YELLOW "%^s is surrounded by flames.  ", wd_he[msex]);
	}
	else if (FLAG(mf_ptr, RF_AURA_COLD))
	{
		roff(CLR_YELLOW "%^s is surrounded by ice.  ", wd_he[msex]);
	}
	else if (FLAG(mf_ptr, RF_AURA_ELEC))
	{
		roff(CLR_YELLOW "%^s is surrounded by electricity.  ", wd_he[msex]);
	}

	if (FLAG(mf_ptr, RF_REFLECTING))
	{
		roff(CLR_YELLOW "%^s reflects bolt spells.  ", wd_he[msex]);
	}

	/* Describe escorts */
	if ((FLAG(mf_ptr, RF_ESCORT)) || (FLAG(mf_ptr, RF_ESCORTS)))
	{
		roff("%^s usually appears with escorts.  ", wd_he[msex]);
	}

	/* Describe friends */
	else if (FLAG(mf_ptr, RF_FRIENDS))
	{
		roff("%^s usually appears in groups.  ", wd_he[msex]);
	}

	else if (FLAG(mf_ptr, RF_CHAR_MIMIC))
	{
		roff("%^s is a mimic.  ", wd_he[msex]);
	}

	/* Collect inate attacks */
	if (FLAG(mf_ptr, RF_SHRIEK)) vp[vn++] = "shriek for help";
	if (FLAG(mf_ptr, RF_ELDRITCH_HORROR)) vp[vn++] = "blast your sanity";
	if (FLAG(mf_ptr, RF_ROCKET)) vp[vn++] = "shoot a rocket";
	if (FLAG(mf_ptr, RF_ARROW)) vp[vn++] = "fire arrows";

	/* Describe inate attacks */
	if (vn)
	{
		/* Intro */
		roff("%^s", wd_he[msex]);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" may ");
			else if (n < vn - 1) roff(", ");
			else
				roff(" or ");

			/* Dump */
			roff(CLR_L_RED "%s", vp[n]);
		}

		/* End */
		roff(".  ");
	}


	/* Collect breaths */
	vn = 0;
	if (FLAG(mf_ptr, RF_BR_ACID)) vp[vn++] = "acid";
	if (FLAG(mf_ptr, RF_BR_ELEC)) vp[vn++] = "lightning";
	if (FLAG(mf_ptr, RF_BR_FIRE)) vp[vn++] = "fire";
	if (FLAG(mf_ptr, RF_BR_COLD)) vp[vn++] = "frost";
	if (FLAG(mf_ptr, RF_BR_POIS)) vp[vn++] = "poison";
	if (FLAG(mf_ptr, RF_BR_NETH)) vp[vn++] = "nether";
	if (FLAG(mf_ptr, RF_BR_LITE)) vp[vn++] = "light";
	if (FLAG(mf_ptr, RF_BR_DARK)) vp[vn++] = "darkness";
	if (FLAG(mf_ptr, RF_BR_CONF)) vp[vn++] = "confusion";
	if (FLAG(mf_ptr, RF_BR_SOUN)) vp[vn++] = "sound";
	if (FLAG(mf_ptr, RF_BR_CHAO)) vp[vn++] = "chaos";
	if (FLAG(mf_ptr, RF_BR_DISE)) vp[vn++] = "disenchantment";
	if (FLAG(mf_ptr, RF_BR_NEXU)) vp[vn++] = "nexus";
	if (FLAG(mf_ptr, RF_BR_TIME)) vp[vn++] = "time";
	if (FLAG(mf_ptr, RF_BR_INER)) vp[vn++] = "inertia";
	if (FLAG(mf_ptr, RF_BR_GRAV)) vp[vn++] = "gravity";
	if (FLAG(mf_ptr, RF_BR_SHAR)) vp[vn++] = "shards";
	if (FLAG(mf_ptr, RF_BR_PLAS)) vp[vn++] = "plasma";
	if (FLAG(mf_ptr, RF_BR_WALL)) vp[vn++] = "force";
	if (FLAG(mf_ptr, RF_BR_MANA)) vp[vn++] = "mana";
	if (FLAG(mf_ptr, RF_BR_NUKE)) vp[vn++] = "toxic waste";
	if (FLAG(mf_ptr, RF_BR_DISI)) vp[vn++] = "disintegration";

	/* Describe breaths */
	if (vn)
	{
		/* Note breath */
		breath = TRUE;

		/* Intro */
		roff("%^s", wd_he[msex]);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" may breathe ");
			else if (n < vn - 1) roff(", ");
			else
				roff(" or ");

			/* Dump */
			roff(CLR_L_RED "%s", vp[n]);
		}
	}


	/* Collect spells */
	vn = 0;
	if (FLAG(mf_ptr, RF_BA_ACID)) vp[vn++] = "produce acid balls";
	if (FLAG(mf_ptr, RF_BA_ELEC)) vp[vn++] = "produce lightning balls";
	if (FLAG(mf_ptr, RF_BA_FIRE)) vp[vn++] = "produce fire balls";
	if (FLAG(mf_ptr, RF_BA_COLD)) vp[vn++] = "produce frost balls";
	if (FLAG(mf_ptr, RF_BA_POIS)) vp[vn++] = "produce poison balls";
	if (FLAG(mf_ptr, RF_BA_NETH)) vp[vn++] = "produce nether balls";
	if (FLAG(mf_ptr, RF_BA_WATE)) vp[vn++] = "produce water balls";
	if (FLAG(mf_ptr, RF_BA_NUKE)) vp[vn++] = "produce balls of radiation";
	if (FLAG(mf_ptr, RF_BA_MANA)) vp[vn++] = "invoke mana storms";
	if (FLAG(mf_ptr, RF_BA_DARK)) vp[vn++] = "invoke darkness storms";
	if (FLAG(mf_ptr, RF_BA_CHAO)) vp[vn++] = "invoke raw Logrus";
	if (FLAG(mf_ptr, RF_HAND_DOOM)) vp[vn++] = "invoke the Hand of Doom";
	if (FLAG(mf_ptr, RF_DRAIN_MANA)) vp[vn++] = "drain mana";
	if (FLAG(mf_ptr, RF_MIND_BLAST)) vp[vn++] = "cause mind blasting";
	if (FLAG(mf_ptr, RF_BRAIN_SMASH)) vp[vn++] = "cause brain smashing";
	if (FLAG(mf_ptr, RF_CAUSE_1)) vp[vn++] = "cause light wounds and cursing";
	if (FLAG(mf_ptr, RF_CAUSE_2)) vp[vn++] = "cause serious wounds and cursing";
	if (FLAG(mf_ptr, RF_CAUSE_3)) vp[vn++] = "cause critical wounds and cursing";
	if (FLAG(mf_ptr, RF_CAUSE_4)) vp[vn++] = "cause mortal wounds";
	if (FLAG(mf_ptr, RF_BO_ACID)) vp[vn++] = "produce acid bolts";
	if (FLAG(mf_ptr, RF_BO_ELEC)) vp[vn++] = "produce lightning bolts";
	if (FLAG(mf_ptr, RF_BO_FIRE)) vp[vn++] = "produce fire bolts";
	if (FLAG(mf_ptr, RF_BO_COLD)) vp[vn++] = "produce frost bolts";
	if (FLAG(mf_ptr, RF_BO_POIS)) vp[vn++] = "produce poison bolts";
	if (FLAG(mf_ptr, RF_BO_NETH)) vp[vn++] = "produce nether bolts";
	if (FLAG(mf_ptr, RF_BO_WATE)) vp[vn++] = "produce water bolts";
	if (FLAG(mf_ptr, RF_BO_MANA)) vp[vn++] = "produce mana bolts";
	if (FLAG(mf_ptr, RF_BO_PLAS)) vp[vn++] = "produce plasma bolts";
	if (FLAG(mf_ptr, RF_BO_ICEE)) vp[vn++] = "produce ice bolts";
	if (FLAG(mf_ptr, RF_MISSILE)) vp[vn++] = "produce magic missiles";
	if (FLAG(mf_ptr, RF_SCARE)) vp[vn++] = "terrify";
	if (FLAG(mf_ptr, RF_BLIND)) vp[vn++] = "blind";
	if (FLAG(mf_ptr, RF_CONF)) vp[vn++] = "confuse";
	if (FLAG(mf_ptr, RF_SLOW)) vp[vn++] = "slow";
	if (FLAG(mf_ptr, RF_HOLD)) vp[vn++] = "paralyze";
	if (FLAG(mf_ptr, RF_HASTE)) vp[vn++] = "haste-self";
	if (FLAG(mf_ptr, RF_HEAL)) vp[vn++] = "heal-self";
	if (FLAG(mf_ptr, RF_INVULNER)) vp[vn++] = "make invulnerable";
	if (FLAG(mf_ptr, RF_BLINK)) vp[vn++] = "blink-self";
	if (FLAG(mf_ptr, RF_TPORT)) vp[vn++] = "teleport-self";
	if (FLAG(mf_ptr, RF_XXX3)) vp[vn++] = "do something";
	if (FLAG(mf_ptr, RF_XXX4)) vp[vn++] = "do something";
	if (FLAG(mf_ptr, RF_TELE_TO)) vp[vn++] = "teleport to";
	if (FLAG(mf_ptr, RF_TELE_AWAY)) vp[vn++] = "teleport away";
	if (FLAG(mf_ptr, RF_TELE_LEVEL)) vp[vn++] = "teleport level";
	if (FLAG(mf_ptr, RF_XXX5)) vp[vn++] = "do something";
	if (FLAG(mf_ptr, RF_DARKNESS)) vp[vn++] = "create darkness";
	if (FLAG(mf_ptr, RF_TRAPS)) vp[vn++] = "create traps";
	if (FLAG(mf_ptr, RF_FORGET)) vp[vn++] = "cause amnesia";
	if (FLAG(mf_ptr, RF_RAISE_DEAD)) vp[vn++] = "raise dead";
	if (FLAG(mf_ptr, RF_S_MONSTER)) vp[vn++] = "summon a monster";
	if (FLAG(mf_ptr, RF_S_MONSTERS)) vp[vn++] = "summon monsters";
	if (FLAG(mf_ptr, RF_S_KIN)) vp[vn++] = "summon aid";
	if (FLAG(mf_ptr, RF_S_ANT)) vp[vn++] = "summon ants";
	if (FLAG(mf_ptr, RF_S_SPIDER)) vp[vn++] = "summon spiders";
	if (FLAG(mf_ptr, RF_S_HOUND)) vp[vn++] = "summon hounds";
	if (FLAG(mf_ptr, RF_S_HYDRA)) vp[vn++] = "summon hydras";
	if (FLAG(mf_ptr, RF_S_ANGEL)) vp[vn++] = "summon an angel";
	if (FLAG(mf_ptr, RF_S_DEMON)) vp[vn++] = "summon a demon";
	if (FLAG(mf_ptr, RF_S_UNDEAD)) vp[vn++] = "summon an undead";
	if (FLAG(mf_ptr, RF_S_DRAGON)) vp[vn++] = "summon a dragon";
	if (FLAG(mf_ptr, RF_S_HI_UNDEAD)) vp[vn++] = "summon Greater Undead";
	if (FLAG(mf_ptr, RF_S_HI_DRAGON)) vp[vn++] = "summon Ancient Dragons";
	if (FLAG(mf_ptr, RF_S_CYBER)) vp[vn++] = "summon Cyberdemons";
	if (FLAG(mf_ptr, RF_S_AMBERITES)) vp[vn++] = "summon Lords of Amber";
	if (FLAG(mf_ptr, RF_S_UNIQUE)) vp[vn++] = "summon Unique Monsters";

	/* Describe spells */
	if (vn)
	{
		/* Note magic */
		magic = TRUE;

		/* Intro */
		if (breath)
		{
			roff(" and is also");
		}
		else
		{
			roff("%^s is", wd_he[msex]);
		}

		/* Verb Phrase */
		roff(" magical, casting spells");

		/* Adverb */
		if (FLAG(mf_ptr, RF_SMART)) roff(CLR_ORANGE " intelligently");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" which ");
			else if (n < vn - 1) roff(", ");
			else
				roff(" or ");

			/* Dump */
			roff(CLR_L_RED "%s", vp[n]);
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
			roff("; 1 time in %d", 100 / n);
		}

		/* Guess at the frequency */
		else if (m)
		{
			n = ((n + 9) / 10) * 10;
			roff("; about 1 time in %d", 100 / n);
		}

		/* End this sentence */
		roff(".  ");
	}


	/* Describe monster "toughness" */
	if (know_armour(r_idx))
	{
		/* Armor */
		roff("%^s has an armor rating of %d", wd_he[msex], r_ptr->ac);

		/* Maximized hitpoints */
		if (FLAG(mf_ptr, RF_FORCE_MAXHP))
		{
			roff(" and a life rating of %d.  ",
									  r_ptr->hdice * r_ptr->hside);
		}

		/* Variable hitpoints */
		else
		{
			roff(" and a life rating of %dd%d.  ",
									  r_ptr->hdice, r_ptr->hside);
		}
	}



	/* Collect special abilities. */
	vn = 0;
	if (FLAG(mf_ptr, RF_CAN_SWIM))  vp[vn++] = "swim";
	if (FLAG(mf_ptr, RF_OPEN_DOOR)) vp[vn++] = "open doors";
	if (FLAG(mf_ptr, RF_BASH_DOOR)) vp[vn++] = "bash down doors";
	if (FLAG(mf_ptr, RF_PASS_WALL)) vp[vn++] = "pass through walls";
	if (FLAG(mf_ptr, RF_KILL_WALL)) vp[vn++] = "bore through walls";
	if (FLAG(mf_ptr, RF_MOVE_BODY)) vp[vn++] = "push past weaker monsters";
	if (FLAG(mf_ptr, RF_KILL_BODY)) vp[vn++] = "destroy weaker monsters";
	if (FLAG(mf_ptr, RF_TAKE_ITEM)) vp[vn++] = "pick up objects";
	if (FLAG(mf_ptr, RF_KILL_ITEM)) vp[vn++] = "destroy objects";
	if (FLAG(mf_ptr, RF_LITE_1) || FLAG(mf_ptr, RF_LITE_2)) vp[vn++] = "light the dungeon";

	/* Describe special abilities. */
	if (vn)
	{
		/* Intro */
		roff("%^s", wd_he[msex]);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" can ");
			else if (n < vn - 1) roff(", ");
			else
				roff(" and ");

			/* Dump */
			roff(CLR_L_UMBER "%s", vp[n]);
		}

		/* End */
		roff(".  ");
	}


	/* Describe special abilities. */
	if (FLAG(mf_ptr, RF_INVISIBLE))
	{
		roff(CLR_L_BLUE "%^s is invisible.  ", wd_he[msex]);
	}
	if (FLAG(mf_ptr, RF_COLD_BLOOD))
	{
		roff("%^s is cold blooded.  ", wd_he[msex]);
	}
	if (FLAG(mf_ptr, RF_EMPTY_MIND))
	{
		roff("%^s is not detected by telepathy.  ", wd_he[msex]);
	}
	if (FLAG(mf_ptr, RF_WEIRD_MIND))
	{
		roff("%^s is rarely detected by telepathy.  ", wd_he[msex]);
	}
	if (FLAG(mf_ptr, RF_MULTIPLY))
	{
		roff(CLR_L_UMBER "%^s breeds explosively.  ", wd_he[msex]);
	}
	if (FLAG(mf_ptr, RF_REGENERATE))
	{
		roff("%^s regenerates quickly.  ", wd_he[msex]);
	}


	/* Collect susceptibilities */
	vn = 0;
	if (FLAG(mf_ptr, RF_HURT_ROCK)) vp[vn++] = "rock remover";
	if (FLAG(mf_ptr, RF_HURT_LITE)) vp[vn++] = "bright light";
	if (FLAG(mf_ptr, RF_HURT_FIRE)) vp[vn++] = "fire";
	if (FLAG(mf_ptr, RF_HURT_COLD)) vp[vn++] = "cold";

	/* Describe susceptibilities */
	if (vn)
	{
		/* Intro */
		roff("%^s", wd_he[msex]);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" is hurt by ");
			else if (n < vn - 1) roff(", ");
			else
				roff(" and ");

			/* Dump */
			roff(CLR_YELLOW "%s", vp[n]);
		}

		/* End */
		roff(".  ");
	}


	/* Collect immunities */
	vn = 0;
	if (FLAG(mf_ptr, RF_IM_ACID)) vp[vn++] = "acid";
	if (FLAG(mf_ptr, RF_IM_ELEC)) vp[vn++] = "lightning";
	if (FLAG(mf_ptr, RF_IM_FIRE)) vp[vn++] = "fire";
	if (FLAG(mf_ptr, RF_IM_COLD)) vp[vn++] = "cold";
	if (FLAG(mf_ptr, RF_IM_POIS)) vp[vn++] = "poison";

	/* Describe immunities */
	if (vn)
	{
		/* Intro */
		roff("%^s", wd_he[msex]);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" resists ");
			else if (n < vn - 1) roff(", ");
			else
				roff(" and ");

			/* Dump */
			roff(CLR_ORANGE "%s", vp[n]);
		}

		/* End */
		roff(".  ");
	}


	/* Collect resistances */
	vn = 0;
	if (FLAG(mf_ptr, RF_RES_NETH)) vp[vn++] = "nether";
	if (FLAG(mf_ptr, RF_RES_WATE)) vp[vn++] = "water";
	if (FLAG(mf_ptr, RF_RES_PLAS)) vp[vn++] = "plasma";
	if (FLAG(mf_ptr, RF_RES_NEXU)) vp[vn++] = "nexus";
	if (FLAG(mf_ptr, RF_RES_DISE)) vp[vn++] = "disenchantment";
	if ((FLAG(mf_ptr, RF_RES_TELE))
		&& !(FLAG(r_ptr, RF_UNIQUE))) vp[vn++] = "teleportation";

	/* Describe resistances */
	if (vn)
	{
		/* Intro */
		roff("%^s", wd_he[msex]);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" resists ");
			else if (n < vn - 1) roff(", ");
			else
				roff(" and ");

			/* Dump */
			roff(CLR_ORANGE "%s", vp[n]);
		}

		/* End */
		roff(".  ");
	}


	/* Collect non-effects */
	vn = 0;
	if (FLAG(mf_ptr, RF_NO_STUN)) vp[vn++] = "stunned";
	if (FLAG(mf_ptr, RF_NO_FEAR)) vp[vn++] = "frightened";
	if (FLAG(mf_ptr, RF_NO_CONF)) vp[vn++] = "confused";
	if (FLAG(mf_ptr, RF_NO_SLEEP)) vp[vn++] = "slept";
	if ((FLAG(mf_ptr, RF_RES_TELE))
		&& (FLAG(r_ptr, RF_UNIQUE))) vp[vn++] = "teleported";

	/* Describe non-effects */
	if (vn)
	{
		/* Intro */
		roff("%^s", wd_he[msex]);

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" cannot be ");
			else if (n < vn - 1) roff(", ");
			else
				roff(" or ");

			/* Dump */
			roff(CLR_YELLOW "%s", vp[n]);
		}

		/* End */
		roff(".  ");
	}


	/* Do we know how aware it is? */
	if ((((int)r_ptr->r_wake * (int)r_ptr->r_wake) > r_ptr->sleep) ||
		(r_ptr->r_ignore == MAX_UCHAR) ||
		(r_ptr->sleep == 0 && (r_ptr->r_tkills >= 10 || know_all)))
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

		roff("%^s %s intruders, which %s may notice from %d feet.  ",
					  wd_he[msex], act, wd_he[msex], 10 * r_ptr->aaf);
	}


	/* Drops gold and/or items */
	if (r_ptr->r_drop_gold || r_ptr->r_drop_item)
	{
		/* No "n" needed */
		sin = FALSE;

		/* Intro */
		roff("%^s may carry", wd_he[msex]);

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
			roff(" up to %d", n);
		}


		/* Great */
		if (FLAG(mf_ptr, RF_DROP_GREAT))
		{
			p = " exceptional";
		}

		/* Good (no "n" needed) */
		else if (FLAG(mf_ptr, RF_DROP_GOOD))
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
				q = "distrupt the time continuum";
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
			roff("%^s can ", wd_he[msex]);
		}
		else if (r < n - 1)
		{
			roff(", ");
		}
		else
		{
			roff(" and ");
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
			roff(CLR_L_RED "%s", q);

			/* Describe damage (if known) */
			if (d1 && d2 && know_damage(r_idx, m))
			{
				/* Display the damage */
				roff(" with damage");
				roff(" %dd%d", d1, d2);
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
	else if (FLAG(mf_ptr, RF_NEVER_BLOW))
	{
		roff("%^s has no physical attacks.  ", wd_he[msex]);
	}

	/* Or describe the lack of knowledge */
	else
	{
		roff("Nothing is known about %s attack.  ", wd_his[msex]);
	}


	/*
	 * Notice "Quest" monsters, but only if you
	 * already encountered the monster.
	 */
	if ((FLAG(mf_ptr, RF_QUESTOR)) && (r_ptr->r_sights))
	{
		roff("You feel an intense desire to kill this monster...  ");
	}


	/* All done */
	roff("\n");

	/* Cheat -- know everything */
	if (know_all && (remem == 0))
	{
		/* Hack -- restore memory */
		COPY(r_ptr, &save_mem, monster_race);
	}
}



/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
void roff_mon_top(int r_idx)
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
	if (!use_color)
	{
		a1 = TERM_WHITE;
		a2 = TERM_WHITE;
	}


	/* Clear the top line */
	clear_msg();
    
	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* A title (use "The" for non-uniques) */
	if (!(FLAG(r_ptr, RF_UNIQUE)))
	{
		roff("The ");
	}

	/* Dump the name */
	roff(mon_race_name(r_ptr));

	/* Append the "standard" attr/char info */
	roff(" ('");
	Term_addch(a1, c1);
	roff("')");

	/* Append the "optional" attr/char info */
	roff("/('");
	Term_addch(a2, c2);
	roff("'):");

	/* Wizards get extra info */
	if (p_ptr->state.wizard)
	{
		roff(" (" CLR_L_BLUE "%d)", r_idx);
	}
}



/*
 * Hack -- describe the given monster race at the top of the screen
 */
void screen_roff_mon(int r_idx, int remember)
{
	/* Flush messages */
	message_flush();

	/* Begin recall */
	clear_row(1);

	/* Recall monster */
	roff_mon_aux(r_idx, remember);

	/* Describe monster */
	roff_mon_top(r_idx);
}




/*
 * Hack -- describe the given monster race in the current "term" window
 */
void display_roff_mon(int r_idx)
{
	/* Erase the window */
    clear_from(0);

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Recall monster */
	roff_mon_aux(r_idx, 0);

	/* Describe monster */
	roff_mon_top(r_idx);
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
    clear_from(0);

	/* Are we hallucinating? */
	if (p_ptr->tim.image)
	{
		put_fstr(0, 10, CLR_VIOLET "Hallucinations");

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
		if (!use_color)
		{
			a1 = TERM_WHITE;
			a2 = TERM_WHITE;
		}

		/* Dump the name */
		if (FLAG(r_ptr, RF_UNIQUE))
		{
			roff(CLR_L_BLUE "%s", mon_race_name(r_ptr));
		}
		else if (FLAG(r_ptr, RF_QUESTOR))
		{
			roff(CLR_L_RED "%s", mon_race_name(r_ptr));
		}
		else
		{
			roff("%s", mon_race_name(r_ptr));
		}

		/* Append the "standard" attr/char info */
		roff(" ('");
		Term_addch(a1, c1);
		roff("')");

		/* Append the "optional" attr/char info */
		roff("/('");
		Term_addch(a2, c2);
		roff("'):");

		/* Wizards get extra info */
		if (p_ptr->state.wizard)
		{
			roff(" (" CLR_L_BLUE "%d)", i);
		}

		/* Append count */
		roff("[%d]", r_ptr->r_see);

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
		msgf("%^v gets angry!", MONSTER_FMT(m_ptr, 0));
		set_hostile(m_ptr);

		chg_virtue(V_INDIVIDUALISM, 1);
		chg_virtue(V_HONOUR, -1);
		chg_virtue(V_JUSTICE, -1);
		chg_virtue(V_COMPASSION, -1);
	}
}


/*
 * Check if two monsters are enemies
 */
bool are_enemies(const monster_type *m_ptr, const monster_type *n_ptr)
{
	const monster_race *r_ptr = &r_info[m_ptr->r_idx];
	const monster_race *s_ptr = &r_info[n_ptr->r_idx];

	/* Friendly vs. opposite aligned normal or pet */
	if (((FLAG(r_ptr, RF_EVIL)) &&
		 (FLAG(s_ptr, RF_GOOD))) ||
		((FLAG(r_ptr, RF_GOOD)) && (FLAG(s_ptr, RF_EVIL))))
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
	if (FLAG(r_ptr, RF_DEMON) || FLAG(r_ptr, RF_UNDEAD) ||
		FLAG(r_ptr, RF_NONLIVING))
		return FALSE;
	else
		return TRUE;
}

/*
 * Shimmer monsters
 */
void change_shimmer(void)
{
	int i;
	
	monster_type *m_ptr;
	monster_race *r_ptr;

	/* Shimmer multi-hued monsters */
	for (i = 1; i < m_max; i++)
	{
		/* Access monster */
		m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Access the monster race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Skip non-multi-hued monsters */
		if (!FLAG(r_ptr, RF_ATTR_MULTI)) continue;

		/* Reset the shimmer flag */
		p_ptr->change |= PC_SHIMMER;

		/* Redraw regardless */
		lite_spot(m_ptr->fx, m_ptr->fy);
	}
}

/*
 * Repair monsters after detection.
 */
void change_repair(void)
{
	int i;

	monster_type *m_ptr;
	
	/* Rotate detection flags */
	for (i = 1; i < m_max; i++)
	{
		/* Access monster */
		m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Nice monsters get mean */
		m_ptr->mflag &= ~(MFLAG_NICE);

		/* Handle memorized monsters */
		if (m_ptr->mflag & MFLAG_MARK)
		{
			/* Maintain detection */
			if (m_ptr->mflag & MFLAG_SHOW)
			{
				/* Forget flag */
				m_ptr->mflag &= ~(MFLAG_SHOW);

				/* Still need repairs */
				p_ptr->change |= (PC_REPAIR);
			}

			/* Remove detection */
			else
			{
				/* Forget flag */
				m_ptr->mflag &= ~(MFLAG_MARK);

				/* Update the monster */
				update_mon(i, FALSE);
			}
		}
	}
}
