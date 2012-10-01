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
	int		ramt[64];

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


	/* Collect resistances */
	vn = 0;
        if (r_ptr->fireres > 0 && r_ptr->r_resist[GF_FIRE] == 1) {vp[vn++] = "fire"; ramt[vn - 1] = r_ptr->fireres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->coldres > 0 && r_ptr->r_resist[GF_COLD] == 1) {vp[vn++] = "cold"; ramt[vn - 1] = r_ptr->coldres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->elecres > 0 && r_ptr->r_resist[GF_ELEC] == 1) {vp[vn++] = "electricity"; ramt[vn - 1] = r_ptr->elecres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->acidres > 0 && r_ptr->r_resist[GF_ACID] == 1) {vp[vn++] = "acid"; ramt[vn - 1] = r_ptr->acidres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->poisres > 0 && r_ptr->r_resist[GF_POIS] == 1) {vp[vn++] = "poison"; ramt[vn - 1] = r_ptr->poisres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->lightres > 0 && r_ptr->r_resist[GF_LITE] == 1) {vp[vn++] = "light"; ramt[vn - 1] = r_ptr->lightres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->darkres > 0 && r_ptr->r_resist[GF_DARK] == 1) {vp[vn++] = "darkness"; ramt[vn - 1] = r_ptr->darkres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->warpres > 0 && r_ptr->r_resist[GF_WARP] == 1) {vp[vn++] = "warp"; ramt[vn - 1] = r_ptr->warpres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->waterres > 0 && r_ptr->r_resist[GF_WATER] == 1) {vp[vn++] = "water"; ramt[vn - 1] = r_ptr->waterres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->windres > 0 && r_ptr->r_resist[GF_WIND] == 1) {vp[vn++] = "wind"; ramt[vn - 1] = r_ptr->windres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->earthres > 0 && r_ptr->r_resist[GF_EARTH] == 1) {vp[vn++] = "earth"; ramt[vn - 1] = r_ptr->earthres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->soundres > 0 && r_ptr->r_resist[GF_SOUND] == 1) {vp[vn++] = "sound"; ramt[vn - 1] = r_ptr->soundres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->radiores > 0 && r_ptr->r_resist[GF_RADIO] == 1) {vp[vn++] = "radioactivity"; ramt[vn - 1] = r_ptr->radiores; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->chaosres > 0 && r_ptr->r_resist[GF_CHAOS] == 1) {vp[vn++] = "chaos"; ramt[vn - 1] = r_ptr->chaosres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->physres > 0 && r_ptr->r_resist[GF_PHYSICAL] == 1) {vp[vn++] = "physical"; ramt[vn - 1] = r_ptr->physres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->manares > 0 && r_ptr->r_resist[GF_MANA] == 1) {vp[vn++] = "mana"; ramt[vn - 1] = r_ptr->manares; color[vn - 1] = TERM_WHITE;}

	/* Describe resistances */
	if (vn)
	{
		char str[80];

		/* Intro */
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" resists ");
			else if (n < vn-1) roff(", ");
			else roff(" and ");

			sprintf(str, "%s(%d%)", vp[n], ramt[n]);
			/* Dump */
                        c_roff(color[n], str);
		}

		/* End */
		roff(".  ");
	}

	/* Collect weaknesses */
	vn = 0;
        if (r_ptr->fireres < 0 && r_ptr->r_resist[GF_FIRE] == 1) {vp[vn++] = "fire"; ramt[vn - 1] = r_ptr->fireres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->coldres < 0 && r_ptr->r_resist[GF_COLD] == 1) {vp[vn++] = "cold"; ramt[vn - 1] = r_ptr->coldres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->elecres < 0 && r_ptr->r_resist[GF_ELEC] == 1) {vp[vn++] = "electricity"; ramt[vn - 1] = r_ptr->elecres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->acidres < 0 && r_ptr->r_resist[GF_ACID] == 1) {vp[vn++] = "acid"; ramt[vn - 1] = r_ptr->acidres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->poisres < 0 && r_ptr->r_resist[GF_POIS] == 1) {vp[vn++] = "poison"; ramt[vn - 1] = r_ptr->poisres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->lightres < 0 && r_ptr->r_resist[GF_LITE] == 1) {vp[vn++] = "light"; ramt[vn - 1] = r_ptr->lightres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->darkres < 0 && r_ptr->r_resist[GF_DARK] == 1) {vp[vn++] = "darkness"; ramt[vn - 1] = r_ptr->darkres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->warpres < 0 && r_ptr->r_resist[GF_WARP] == 1) {vp[vn++] = "warp"; ramt[vn - 1] = r_ptr->warpres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->waterres < 0 && r_ptr->r_resist[GF_WATER] == 1) {vp[vn++] = "water"; ramt[vn - 1] = r_ptr->waterres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->windres < 0 && r_ptr->r_resist[GF_WIND] == 1) {vp[vn++] = "wind"; ramt[vn - 1] = r_ptr->windres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->earthres < 0 && r_ptr->r_resist[GF_EARTH] == 1) {vp[vn++] = "earth"; ramt[vn - 1] = r_ptr->earthres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->soundres < 0 && r_ptr->r_resist[GF_SOUND] == 1) {vp[vn++] = "sound"; ramt[vn - 1] = r_ptr->soundres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->radiores < 0 && r_ptr->r_resist[GF_RADIO] == 1) {vp[vn++] = "radioactivity"; ramt[vn - 1] = r_ptr->radiores; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->chaosres < 0 && r_ptr->r_resist[GF_CHAOS] == 1) {vp[vn++] = "chaos"; ramt[vn - 1] = r_ptr->chaosres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->physres < 0 && r_ptr->r_resist[GF_PHYSICAL] == 1) {vp[vn++] = "physical"; ramt[vn - 1] = r_ptr->physres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->manares < 0 && r_ptr->r_resist[GF_MANA] == 1) {vp[vn++] = "mana"; ramt[vn - 1] = r_ptr->manares; color[vn - 1] = TERM_WHITE;}

	/* Describe resistances */
	if (vn)
	{
		char str[80];

		/* Intro */
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" is weak against ");
			else if (n < vn-1) roff(", ");
			else roff(" and ");

			sprintf(str, "%s(%d%)", vp[n], (ramt[n] * (-1)));
			/* Dump */
                        c_roff(color[n], str);
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

	/* Notice "Quest" monsters */
	if (flags1 & RF1_QUESTOR)
	{
                c_roff(TERM_VIOLET, "You feel an intense desire to kill this monster...  ");
	}

	/* All done */
	roff("\n");

	/* List of monster attacks */
	roff("----- ATTACKS -----");
        roff("\n");

	for (m = 0; m < 20; m++)
	{
		if ((r_ptr->attack[m].type != 0) && r_ptr->r_blows[m] == 1)
		{
			roff(r_ptr->attack[m].act);
			if (!(strstr(r_ptr->attack[m].name, "!")))
			{
				roff(" with ");
				roff(r_ptr->attack[m].name);
			}
			roff(" (");
			roff(get_element_name(r_ptr->attack[m].element));
			roff(")");
			roff("\n");
		}
	}

	roff("\n");

	/* List of monster spells */
	roff("----- SPELLS -----");
        roff("\n");

	for (m = 0; m < 20; m++)
	{
		if ((r_ptr->spell[m].type != 0) && r_ptr->r_spells[m] == 1)
		{
			roff(r_ptr->spell[m].name);
			if (r_ptr->spell[m].type == 1 || r_ptr->spell[m].type == 2)
			{
				roff(" (");
				roff(get_element_name(r_ptr->spell[m].special1));
				roff(")");
			}
			roff("\n");
		}
	}

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
	monster_type *m_ptr;

	m_ptr = &m_list[monster_type_idx];

	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Recall monster */
	if (!m_ptr) roff_aux(r_idx, 0);
	else roff_aux_boss(r_idx, 0, m_ptr);

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
                get_mon_num_hook = monster_dungeon;
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
	int		ramt[64];

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


	/* Collect resistances */
	vn = 0;
        if (r_ptr->fireres > 0 && r_ptr->r_resist[GF_FIRE] == 1) {vp[vn++] = "fire"; ramt[vn - 1] = r_ptr->fireres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->coldres > 0 && r_ptr->r_resist[GF_COLD] == 1) {vp[vn++] = "cold"; ramt[vn - 1] = r_ptr->coldres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->elecres > 0 && r_ptr->r_resist[GF_ELEC] == 1) {vp[vn++] = "electricity"; ramt[vn - 1] = r_ptr->elecres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->acidres > 0 && r_ptr->r_resist[GF_ACID] == 1) {vp[vn++] = "acid"; ramt[vn - 1] = r_ptr->acidres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->poisres > 0 && r_ptr->r_resist[GF_POIS] == 1) {vp[vn++] = "poison"; ramt[vn - 1] = r_ptr->poisres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->lightres > 0 && r_ptr->r_resist[GF_LITE] == 1) {vp[vn++] = "light"; ramt[vn - 1] = r_ptr->lightres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->darkres > 0 && r_ptr->r_resist[GF_DARK] == 1) {vp[vn++] = "darkness"; ramt[vn - 1] = r_ptr->darkres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->warpres > 0 && r_ptr->r_resist[GF_WARP] == 1) {vp[vn++] = "warp"; ramt[vn - 1] = r_ptr->warpres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->waterres > 0 && r_ptr->r_resist[GF_WATER] == 1) {vp[vn++] = "water"; ramt[vn - 1] = r_ptr->waterres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->windres > 0 && r_ptr->r_resist[GF_WIND] == 1) {vp[vn++] = "wind"; ramt[vn - 1] = r_ptr->windres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->earthres > 0 && r_ptr->r_resist[GF_EARTH] == 1) {vp[vn++] = "earth"; ramt[vn - 1] = r_ptr->earthres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->soundres > 0 && r_ptr->r_resist[GF_SOUND] == 1) {vp[vn++] = "sound"; ramt[vn - 1] = r_ptr->soundres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->radiores > 0 && r_ptr->r_resist[GF_RADIO] == 1) {vp[vn++] = "radioactivity"; ramt[vn - 1] = r_ptr->radiores; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->chaosres > 0 && r_ptr->r_resist[GF_CHAOS] == 1) {vp[vn++] = "chaos"; ramt[vn - 1] = r_ptr->chaosres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->physres > 0 && r_ptr->r_resist[GF_PHYSICAL] == 1) {vp[vn++] = "physical"; ramt[vn - 1] = r_ptr->physres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->manares > 0 && r_ptr->r_resist[GF_MANA] == 1) {vp[vn++] = "mana"; ramt[vn - 1] = r_ptr->manares; color[vn - 1] = TERM_WHITE;}

	/* Describe resistances */
	if (vn)
	{
		char str[80];

		/* Intro */
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" resists ");
			else if (n < vn-1) roff(", ");
			else roff(" and ");

			sprintf(str, "%s(%d%)", vp[n], ramt[n]);
			/* Dump */
                        c_roff(color[n], str);
		}

		/* End */
		roff(".  ");
	}

	/* Collect weaknesses */
	vn = 0;
        if (r_ptr->fireres < 0 && r_ptr->r_resist[GF_FIRE] == 1) {vp[vn++] = "fire"; ramt[vn - 1] = r_ptr->fireres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->coldres < 0 && r_ptr->r_resist[GF_COLD] == 1) {vp[vn++] = "cold"; ramt[vn - 1] = r_ptr->coldres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->elecres < 0 && r_ptr->r_resist[GF_ELEC] == 1) {vp[vn++] = "electricity"; ramt[vn - 1] = r_ptr->elecres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->acidres < 0 && r_ptr->r_resist[GF_ACID] == 1) {vp[vn++] = "acid"; ramt[vn - 1] = r_ptr->acidres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->poisres < 0 && r_ptr->r_resist[GF_POIS] == 1) {vp[vn++] = "poison"; ramt[vn - 1] = r_ptr->poisres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->lightres < 0 && r_ptr->r_resist[GF_LITE] == 1) {vp[vn++] = "light"; ramt[vn - 1] = r_ptr->lightres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->darkres < 0 && r_ptr->r_resist[GF_DARK] == 1) {vp[vn++] = "darkness"; ramt[vn - 1] = r_ptr->darkres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->warpres < 0 && r_ptr->r_resist[GF_WARP] == 1) {vp[vn++] = "warp"; ramt[vn - 1] = r_ptr->warpres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->waterres < 0 && r_ptr->r_resist[GF_WATER] == 1) {vp[vn++] = "water"; ramt[vn - 1] = r_ptr->waterres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->windres < 0 && r_ptr->r_resist[GF_WIND] == 1) {vp[vn++] = "wind"; ramt[vn - 1] = r_ptr->windres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->earthres < 0 && r_ptr->r_resist[GF_EARTH] == 1) {vp[vn++] = "earth"; ramt[vn - 1] = r_ptr->earthres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->soundres < 0 && r_ptr->r_resist[GF_SOUND] == 1) {vp[vn++] = "sound"; ramt[vn - 1] = r_ptr->soundres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->radiores < 0 && r_ptr->r_resist[GF_RADIO] == 1) {vp[vn++] = "radioactivity"; ramt[vn - 1] = r_ptr->radiores; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->chaosres < 0 && r_ptr->r_resist[GF_CHAOS] == 1) {vp[vn++] = "chaos"; ramt[vn - 1] = r_ptr->chaosres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->physres < 0 && r_ptr->r_resist[GF_PHYSICAL] == 1) {vp[vn++] = "physical"; ramt[vn - 1] = r_ptr->physres; color[vn - 1] = TERM_WHITE;}
	if (r_ptr->manares < 0 && r_ptr->r_resist[GF_MANA] == 1) {vp[vn++] = "mana"; ramt[vn - 1] = r_ptr->manares; color[vn - 1] = TERM_WHITE;}

	/* Describe resistances */
	if (vn)
	{
		char str[80];

		/* Intro */
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" is weak against ");
			else if (n < vn-1) roff(", ");
			else roff(" and ");

			sprintf(str, "%s(%d%)", vp[n], (ramt[n] * (-1)));
			/* Dump */
                        c_roff(color[n], str);
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

	/* Notice "Quest" monsters */
	if (flags1 & RF1_QUESTOR)
	{
                c_roff(TERM_VIOLET, "You feel an intense desire to kill this monster...  ");
	}


	/* All done */
	roff("\n");

	/* List of monster attacks */
	roff("----- ATTACKS -----");
        roff("\n");

	for (m = 0; m < 20; m++)
	{
		if ((r_ptr->attack[m].type != 0) && r_ptr->r_blows[m] == 1)
		{
			roff(r_ptr->attack[m].act);
			if (!(strstr(r_ptr->attack[m].name, "!")))
			{
				roff(" with ");
				roff(r_ptr->attack[m].name);
			}
			roff(" (");
			roff(get_element_name(r_ptr->attack[m].element));
			roff(")");
			roff("\n");
		}
	}

	roff("\n");

	/* List of monster spells */
	roff("----- SPELLS -----");
        roff("\n");

	for (m = 0; m < 20; m++)
	{
		if ((r_ptr->spell[m].type != 0) && r_ptr->r_spells[m] == 1)
		{
			roff(r_ptr->spell[m].name);
			if (r_ptr->spell[m].type == 1 || r_ptr->spell[m].type == 2)
			{
				roff(" (");
				roff(get_element_name(r_ptr->spell[m].special1));
				roff(")");
			}
			roff("\n");
		}
	}
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
	roff(format("Strength: %d", m_ptr->str));
        roff("\n");
	roff(format("Dexterity: %d", m_ptr->dex));
        roff("\n");
	roff(format("Mind: %d", m_ptr->mind));
        roff("\n");
	roff(format("Attack Skill: %d", m_ptr->skill_attack));
        roff("\n");
	roff(format("Magic Skill: %d", m_ptr->skill_magic));
        roff("\n");
	
        
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

