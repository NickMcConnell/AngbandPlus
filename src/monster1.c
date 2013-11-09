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
#ifdef JP
{ "それ", "彼", "彼女" };
#else
{ "it", "he", "she" };
#endif

static cptr wd_his[3] =
#ifdef JP
{ "それの", "彼の", "彼女の" };
#else
{ "its", "his", "her" };
#endif



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

	if (cheat_know || easy_band) return (TRUE);

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

	if (d >= ((4+level)*MAX_UCHAR)/80) d = ((4+level)*MAX_UCHAR-1)/80;

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
 * Prepare hook for c_roff(). It will be changed for spoiler generation in wizard1.c.
 */
void (*hook_c_roff)(byte attr, cptr str) = c_roff;

static void hooked_roff(cptr str)
{
	/* Spawn */
	hook_c_roff(TERM_WHITE, str);
}


/*
 * Hack -- display monster information using "hooked_roff()"
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
static void roff_aux(int r_idx, int mode)
{
	monster_race    *r_ptr = &r_info[r_idx];

	bool            old = FALSE;
#ifndef JP
	bool            sin = FALSE;
#endif

	int             m, n, r;

	cptr            p, q;

#ifdef JP
	char            jverb_buf[64];
#endif
	int             msex = 0;

	int speed = (ironman_nightmare) ? r_ptr->speed + 5 : r_ptr->speed;

	bool            breath = FALSE;
	bool            magic = FALSE;

	u32b		flags1;
	u32b		flags2;
	u32b		flags3;
	u32b		flags4;
	u32b		flags5;
	u32b		flags6;
	u32b		flags7;
	u32b		flagsa;
	u32b		flagsr;

	byte drop_gold, drop_item;

	int		vn = 0;
	byte		color[128];
	cptr		vp[128];

	bool know_everything = FALSE;

	bool is_questor = FALSE;

	/* Obtain a copy of the "known" number of drops */
	drop_gold = r_ptr->r_drop_gold;
	drop_item = r_ptr->r_drop_item;

	/* Obtain a copy of the "known" flags */
	flags1 = (r_ptr->flags1 & r_ptr->r_flags1);
	flags2 = (r_ptr->flags2 & r_ptr->r_flags2);
	flags3 = (r_ptr->flags3 & r_ptr->r_flags3);
	flags4 = (r_ptr->flags4 & r_ptr->r_flags4);
	flags5 = (r_ptr->flags5 & r_ptr->r_flags5);
	flags6 = (r_ptr->flags6 & r_ptr->r_flags6);
	flags7 = (r_ptr->flags7 & r_ptr->flags7);
	flagsa = (r_ptr->flagsa & r_ptr->r_flagsa);
	flagsr = (r_ptr->flagsr & r_ptr->r_flagsr);

	/* cheat_know or reserch_mon() */
	if (cheat_know || easy_band || (mode & 0x01))
		know_everything = TRUE;

	/* Cheat -- Know everything */
	if (know_everything)
	{
		/* Hack -- maximal drops */
		drop_gold = drop_item =
		(((r_ptr->flags1 & RF1_DROP_4D2) ? 8 : 0) +
		 ((r_ptr->flags1 & RF1_DROP_3D2) ? 6 : 0) +
		 ((r_ptr->flags1 & RF1_DROP_2D2) ? 4 : 0) +
		 ((r_ptr->flags1 & RF1_DROP_1D2) ? 2 : 0) +
		 ((r_ptr->flags1 & RF1_DROP_90)  ? 1 : 0) +
		 ((r_ptr->flags1 & RF1_DROP_60)  ? 1 : 0));

		/* Hack -- but only "valid" drops */
		if (r_ptr->flags1 & RF1_ONLY_GOLD) drop_item = 0;
		if (r_ptr->flags1 & RF1_ONLY_ITEM) drop_gold = 0;

		/* Hack -- know all the flags */
		flags1 = r_ptr->flags1;
		flags2 = r_ptr->flags2;
		flags3 = r_ptr->flags3;
		flags4 = r_ptr->flags4;
		flags5 = r_ptr->flags5;
		flags6 = r_ptr->flags6;
		flagsa = r_ptr->flagsa;
		flagsr = r_ptr->flagsr;
	}


	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	else if (r_ptr->flags1 & RF1_MALE) msex = 1;

	/* Assume some "obvious" flags */
	if (r_ptr->flags1 & RF1_UNIQUE)  flags1 |= (RF1_UNIQUE);
	if (r_ptr->flags1 & RF1_QUESTOR) flags1 |= (RF1_QUESTOR);
	if (r_ptr->flags1 & RF1_MALE)    flags1 |= (RF1_MALE);
	if (r_ptr->flags1 & RF1_FEMALE)  flags1 |= (RF1_FEMALE);

	/* Assume some "element" flags */
	if (r_ptr->flags3 & RF3_ELEM_FIRE) flags3 |= (RF3_ELEM_FIRE);
	if (r_ptr->flags3 & RF3_ELEM_AQUA) flags3 |= (RF3_ELEM_AQUA);
	if (r_ptr->flags3 & RF3_ELEM_EARTH) flags3 |= (RF3_ELEM_EARTH);
	if (r_ptr->flags3 & RF3_ELEM_WIND) flags3 |= (RF3_ELEM_WIND);
	if (r_ptr->flags3 & RF3_ELEM_MULTI) flags3 |= (RF3_ELEM_MULTI);

	/* Assume some "ethnicity" flags */
	if (r_ptr->flags2 & RF2_WALSTANIAN) flags2 |= (RF2_WALSTANIAN);
	if (r_ptr->flags2 & RF2_GARGASTAN)  flags2 |= (RF2_GARGASTAN);
	if (r_ptr->flags2 & RF2_BACRUM)     flags2 |= (RF2_BACRUM);
	if (r_ptr->flags2 & RF2_ZENOBIAN)   flags2 |= (RF2_ZENOBIAN);
	if (r_ptr->flags2 & RF2_LODIS)      flags2 |= (RF2_LODIS);

	/* Assume some "creation" flags */
	if (r_ptr->flags1 & RF1_FRIENDS) flags1 |= (RF1_FRIENDS);
	if (r_ptr->flags1 & RF1_ESCORT)  flags1 |= (RF1_ESCORT);
	if (r_ptr->flags1 & RF1_ESCORTS) flags1 |= (RF1_ESCORTS);

	/* Killing a monster reveals some properties */
	if (r_ptr->r_tkills || know_everything)
	{
		/* Know "race" flags */
		if (r_ptr->flags3 & RF3_ORC)      flags3 |= (RF3_ORC);
		if (r_ptr->flags3 & RF3_TROLL)    flags3 |= (RF3_TROLL);
		if (r_ptr->flags3 & RF3_GIANT)    flags3 |= (RF3_GIANT);
		if (r_ptr->flags3 & RF3_DRAGON)   flags3 |= (RF3_DRAGON);
		if (r_ptr->flags3 & RF3_DEMON)    flags3 |= (RF3_DEMON);
		if (r_ptr->flags3 & RF3_UNDEAD)   flags3 |= (RF3_UNDEAD);
		if (r_ptr->flags3 & RF3_EVIL)     flags3 |= (RF3_EVIL);
		if (r_ptr->flags3 & RF3_GOOD)     flags3 |= (RF3_GOOD);
		if (r_ptr->flags3 & RF3_ANIMAL)   flags3 |= (RF3_ANIMAL);
		if (r_ptr->flags3 & RF3_TEMPLE)   flags3 |= (RF3_TEMPLE);
		if (r_ptr->flags2 & RF2_HUMAN)    flags2 |= (RF2_HUMAN);

		/* Know "forced" flags */
		if (r_ptr->flags1 & RF1_FORCE_DEPTH) flags1 |= (RF1_FORCE_DEPTH);
		if (r_ptr->flags1 & RF1_FORCE_MAXHP) flags1 |= (RF1_FORCE_MAXHP);
	}

	/* For output_monster_spoiler() */
	if (mode & 0x02)
	{
		/* Nothing to do */
	}
	else

	/* Treat uniques differently */
	if (flags1 & RF1_UNIQUE)
	{
		/* Hack -- Determine if the unique is "dead" */
		bool dead = (r_ptr->max_num == 0) ? TRUE : FALSE;

		/* We've been killed... */
		if (r_ptr->r_deaths)
		{
			/* Killed ancestors */
#ifdef JP
			hooked_roff(format("%^sはあなたの先祖を %d 人葬っている",
			            wd_he[msex], r_ptr->r_deaths));
#else
			hooked_roff(format("%^s has slain %d of your ancestors",
			            wd_he[msex], r_ptr->r_deaths));
#endif


			/* But we've also killed it */
			if (dead)
			{
#ifdef JP
				hooked_roff(format("が、すでに仇討ちは果たしている！"));
#else
				hooked_roff(format(", but you have avenged %s!  ",
				            plural(r_ptr->r_deaths, "him", "them")));
#endif

			}

			/* Unavenged (ever) */
			else
			{
#ifdef JP
				hooked_roff(format("のに、まだ仇討ちを果たしていない。"));
#else
				hooked_roff(format(", who %s unavenged.  ",
				            plural(r_ptr->r_deaths, "remains", "remain")));
#endif

			}

			/* Start a new line */
			hooked_roff("\n");
		}

		/* Dead unique who never hurt us */
		else if (dead)
		{
#ifdef JP
			hooked_roff("あなたはこの仇敵をすでに葬り去っている。");
#else
			hooked_roff("You have slain this foe.  ");
#endif

			/* Start a new line */
			hooked_roff("\n");
		}
	}

	/* Not unique, but killed us */
	else if (r_ptr->r_deaths)
	{
		/* Dead ancestors */
#ifdef JP
		hooked_roff(format("このモンスターはあなたの先祖を %d 人葬っている",
		            r_ptr->r_deaths ));
#else
		hooked_roff(format("%d of your ancestors %s been killed by this creature, ",
		            r_ptr->r_deaths, plural(r_ptr->r_deaths, "has", "have")));
#endif


		/* Some kills this life */
		if (r_ptr->r_pkills)
		{
#ifdef JP
			hooked_roff(format("が、あなたはこのモンスターを少なくとも %d 体は倒している。", r_ptr->r_pkills));
#else
			hooked_roff(format("and you have exterminated at least %d of the creatures.  ", r_ptr->r_pkills));
#endif

		}

		/* Some kills past lives */
		else if (r_ptr->r_tkills)
		{
#ifdef JP
			hooked_roff(format("が、%sはこのモンスターを少なくとも %d 体は倒している。",
			            "あなたの先祖", r_ptr->r_tkills));
#else
			hooked_roff(format("and %s have exterminated at least %d of the creatures.  ",
			            "your ancestors", r_ptr->r_tkills));
#endif

		}

		/* No kills */
		else
		{
#ifdef JP
			hooked_roff(format("が、まだ%sを倒したことはない。", wd_he[msex]));
#else
			hooked_roff(format("and %s is not ever known to have been defeated.  ", wd_he[msex]));
#endif

		}

		/* Start a new line */
		hooked_roff("\n");
	}

	/* Normal monsters */
	else
	{
		/* Killed some this life */
		if (r_ptr->r_pkills)
		{
#ifdef JP
			hooked_roff(format("あなたはこのモンスターを少なくとも %d 体は殺している。", r_ptr->r_pkills));
#else
			hooked_roff(format("You have killed at least %d of these creatures.  ", r_ptr->r_pkills));
#endif

		}

		/* Killed some last life */
		else if (r_ptr->r_tkills)
		{
#ifdef JP
			hooked_roff(format("あなたの先祖はこのモンスターを少なくとも %d 体は殺している。", r_ptr->r_tkills));
#else
			hooked_roff(format("Your ancestors have killed at least %d of these creatures.  ", r_ptr->r_tkills));
#endif

		}

		/* Killed none */
		else
		{
#ifdef JP
			hooked_roff("このモンスターを倒したことはない。");
#else
			hooked_roff("No battles to the death are recalled.  ");
#endif
		}

		/* Start a new line */
		hooked_roff("\n");
	}


	/* Descriptions */
	{
		char buf[2048];

#ifdef DELAY_LOAD_R_TEXT

		int fd;

		/* Build the filename */
#ifdef JP
		path_build(buf, sizeof(buf), ANGBAND_DIR_DATA, "r_info_j.raw");
#else
		path_build(buf, sizeof(buf), ANGBAND_DIR_DATA, "r_info.raw");
#endif


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

		if (buf[0])
		{
			/* Dump it */
			hooked_roff(buf);

			/* Start a new line */
			hooked_roff("\n");
		}
	}

	/* Nothing yet */
	old = FALSE;

	/* Describe location */
	if (r_ptr->level == 0)
	{
#ifdef JP
		hooked_roff(format("%^sは町に住み", wd_he[msex]));
#else
		hooked_roff(format("%^s lives in the town", wd_he[msex]));
#endif

		old = TRUE;
	}
	else if (r_ptr->r_tkills || know_everything)
	{
		if (depth_in_feet)
		{
#ifdef JP
			hooked_roff(format("%^sは通常 %d フィートで出現し",
#else
			hooked_roff(format("%^s is normally found at depths of %d feet",
#endif

			            wd_he[msex], r_ptr->level * 50));
		}
		else
		{
#ifdef JP
			hooked_roff(format("%^sは通常 %d 階で出現し",
#else
			hooked_roff(format("%^s is normally found on dungeon level %d",
#endif

			            wd_he[msex], r_ptr->level));
		}
		old = TRUE;
	}


	/* Describe movement */

	/* Introduction */
	if (old)
	{
#ifdef JP
		hooked_roff("、");
#else
		hooked_roff(", and ");
#endif

	}
	else
	{
#ifdef JP
		hooked_roff(format("%^sは", wd_he[msex]));
#else
		hooked_roff(format("%^s ", wd_he[msex]));
#endif

		old = TRUE;
	}
#ifndef JP
	hooked_roff("moves");
#endif

	/* Random-ness */
	if ((flags1 & RF1_RAND_50) || (flags1 & RF1_RAND_25))
	{
		/* Adverb */
		if ((flags1 & RF1_RAND_50) && (flags1 & RF1_RAND_25))
		{
#ifdef JP
			hooked_roff("かなり");
#else
			hooked_roff(" extremely");
#endif

		}
		else if (flags1 & RF1_RAND_50)
		{
#ifdef JP
			hooked_roff("幾分");
#else
			hooked_roff(" somewhat");
#endif

		}
		else if (flags1 & RF1_RAND_25)
		{
#ifdef JP
			hooked_roff("少々");
#else
			hooked_roff(" a bit");
#endif

		}

		/* Adjective */
#ifdef JP
		hooked_roff("不規則に");
#else
		hooked_roff(" erratically");
#endif


		/* Hack -- Occasional conjunction */
#ifdef JP
		if (speed != 110) hooked_roff("、かつ");
#else
		if (speed != 110) hooked_roff(", and");
#endif

	}

	/* Speed */
	if (speed > 110)
	{
#ifdef JP
		if (speed > 139) hook_c_roff(TERM_RED, "信じ難いほど");
		else if (speed > 134) hook_c_roff(TERM_ORANGE, "猛烈に");
		else if (speed > 129) hook_c_roff(TERM_ORANGE, "非常に");
		else if (speed > 124) hook_c_roff(TERM_UMBER, "かなり");
		else if (speed < 120) hook_c_roff(TERM_L_UMBER, "やや");
		hook_c_roff(TERM_L_RED, "素早く");
#else
		if (speed > 139) hook_c_roff(TERM_RED, " incredibly");
		else if (speed > 134) hook_c_roff(TERM_ORANGE, " extremely");
		else if (speed > 129) hook_c_roff(TERM_ORANGE, " very");
		else if (speed > 124) hook_c_roff(TERM_UMBER, " fairly");
		else if (speed < 120) hook_c_roff(TERM_L_UMBER, " somewhat");
		hook_c_roff(TERM_L_RED, " quickly");
#endif

	}
	else if (speed < 110)
	{
#ifdef JP
		if (speed < 90) hook_c_roff(TERM_L_GREEN, "信じ難いほど");
		else if (speed < 95) hook_c_roff(TERM_BLUE, "非常に");
		else if (speed < 100) hook_c_roff(TERM_BLUE, "かなり");
		else if (speed > 104) hook_c_roff(TERM_GREEN, "やや");
		hook_c_roff(TERM_L_BLUE, "ゆっくりと");
#else
		if (speed < 90) hook_c_roff(TERM_L_GREEN, " incredibly");
		else if (speed < 95) hook_c_roff(TERM_BLUE, " very");
		else if (speed < 100) hook_c_roff(TERM_BLUE, " fairly");
		else if (speed > 104) hook_c_roff(TERM_GREEN, " somewhat");
		hook_c_roff(TERM_L_BLUE, " slowly");
#endif

	}
	else
	{
#ifdef JP
		hooked_roff("普通の速さで");
#else
		hooked_roff(" at normal speed");
#endif

	}
#ifdef JP
	hooked_roff("動いている");
#endif

	/* The code above includes "attack speed" */
	if (flags1 & RF1_NEVER_MOVE)
	{
		/* Introduce */
		if (old)
		{
#ifdef JP
			hooked_roff("、しかし");
#else
			hooked_roff(", but ");
#endif

		}
		else
		{
#ifdef JP
			hooked_roff(format("%^sは", wd_he[msex]));
#else
			hooked_roff(format("%^s ", wd_he[msex]));
#endif

			old = TRUE;
		}

		/* Describe */
#ifdef JP
		hooked_roff("侵入者を追跡しない");
#else
		hooked_roff("does not deign to chase intruders");
#endif

	}

	/* End this sentence */
	if (old)
	{
#ifdef JP
		hooked_roff("。");
#else
		hooked_roff(".  ");
#endif

		old = FALSE;
	}


	/* Describe experience if known */
	if (r_ptr->r_tkills || know_everything)
	{
		/* Introduction */
#ifdef JP
		hooked_roff("この");
#else
		if (flags1 & RF1_UNIQUE)
		{
			hooked_roff("Killing this");
		}
		else
		{
			hooked_roff("A kill of this");
		}
#endif

		/* Describe the "element" */
		if (flags3 & RF3_ELEM_MASK)
		{
#ifdef JP
			if (flags3 & RF3_ELEM_MULTI)  hook_c_roff(TERM_YELLOW, "可変");
			else if (flags3 & RF3_ELEM_FIRE)  hook_c_roff(elem_attr(ELEM_FIRE), "火");
			else if (flags3 & RF3_ELEM_AQUA)  hook_c_roff(elem_attr(ELEM_AQUA), "水");
			else if (flags3 & RF3_ELEM_EARTH) hook_c_roff(elem_attr(ELEM_EARTH), "地");
			else if (flags3 & RF3_ELEM_WIND)  hook_c_roff(elem_attr(ELEM_WIND), "風");
			hooked_roff("のエレメントを持つ");
#else
			if (flags3 & RF3_ELEM_MULTI)  hooked_roff(" multi");
			else if (flags3 & RF3_ELEM_FIRE)  hooked_roff(" fire");
			else if (flags3 & RF3_ELEM_AQUA)  hooked_roff(" aqua");
			else if (flags3 & RF3_ELEM_EARTH) hooked_roff(" earth");
			else if (flags3 & RF3_ELEM_WIND)  hooked_roff(" wind");
			hooked_roff("-elemented");
#endif
		}


		/* Describe the "quality" */
#ifdef JP
		if (flags2 & RF2_ELDRITCH_HORROR) hook_c_roff(TERM_VIOLET, "狂気を誘う");/*nuke me*/
#else
		if (flags2 & RF2_ELDRITCH_HORROR) hooked_roff(" sanity-blasting");
#endif

#ifdef JP
		if (flags3 & RF3_ANIMAL)          hook_c_roff(TERM_L_GREEN, "自然界の");
#else
		if (flags3 & RF3_ANIMAL)          hooked_roff(" natural");
#endif

#ifdef JP
		if (flags3 & RF3_EVIL)            hook_c_roff(TERM_L_DARK, "邪悪なる");
#else
		if (flags3 & RF3_EVIL)            hooked_roff(" evil");
#endif

#ifdef JP
		if (flags3 & RF3_GOOD)            hook_c_roff(TERM_YELLOW, "善良な");
#else
		if (flags3 & RF3_GOOD)            hooked_roff(" good");
#endif

#ifdef JP
		if (flags7 & RF7_LAWFUL)          hook_c_roff(TERM_L_WHITE, "秩序の");
#else
		if (flags7 & RF7_LAWFUL)          hooked_roff(" lawful");
#endif

#ifdef JP
		if (flags7 & RF7_CHAOTIC)         hook_c_roff(TERM_VIOLET, "混沌の");
#else
		if (flags7 & RF7_CHAOTIC)         hooked_roff(" chaotic");
#endif

#ifdef JP
		if (flags3 & RF3_UNDEAD)          hook_c_roff(TERM_VIOLET, "アンデッドの");
#else
		if (flags3 & RF3_UNDEAD)          hooked_roff(" undead");
#endif


		if (flags2 & RF2_ETHNICITY_MASK)
		{
#ifdef JP
			if (flags2 & RF2_WALSTANIAN) hook_c_roff(TERM_L_WHITE, "ウォルスタ");
			if (flags2 & RF2_GARGASTAN)  hook_c_roff(TERM_L_WHITE, "ガルガスタン");
			if (flags2 & RF2_BACRUM)     hook_c_roff(TERM_L_WHITE, "バクラム");
			if (flags2 & RF2_ZENOBIAN)   hook_c_roff(TERM_L_WHITE, "ゼノビア");
			if (flags2 & RF2_LODIS)      hook_c_roff(TERM_L_WHITE, "ローディス");
			if (flags2 & RF2_HUMAN)      hook_c_roff(TERM_L_WHITE, "人");
			if (!(flags2 & RF2_HUMAN) || (flags3 & (RF3_RACE_MASK | RF3_TEMPLE)) || (flags7 & RF7_ZENOBIAN_FORCES)) hook_c_roff(TERM_L_WHITE, "の");
#else
			if (flags2 & RF2_WALSTANIAN) hooked_roff(" Walstanian");
			if (flags2 & RF2_GARGASTAN)  hooked_roff(" Gargastan");
			if (flags2 & RF2_BACRUM)     hooked_roff(" Bacrum");
			if (flags2 & RF2_ZENOBIAN)   hooked_roff(" Zenobian");
			if (flags2 & RF2_LODIS)      hooked_roff(" Lodis");
#endif
		}


		if (flags3 & RF3_TEMPLE)
		{
#ifdef JP
			hook_c_roff(TERM_L_DARK, "暗黒騎士");
			if ((!(flags2 & RF2_ETHNICITY_MASK) && (flags2 & RF2_HUMAN)) || (flags3 & RF3_RACE_MASK)) hook_c_roff(TERM_L_DARK, "の");
#else
			hooked_roff(" Temple Knight");
#endif
		}
		if (flags7 & RF7_ZENOBIAN_FORCES)
		{
#ifdef JP
			hook_c_roff(TERM_WHITE, "神聖騎士");
			if ((!(flags2 & RF2_ETHNICITY_MASK) && (flags2 & RF2_HUMAN)) || (flags3 & RF3_RACE_MASK)) hook_c_roff(TERM_L_DARK, "の");
#else
			hooked_roff(" White Knight");
#endif
		}


		if ((flags3 & (RF3_RACE_MASK | RF3_TEMPLE)) || (flags2 & RF2_HUMAN) || (flags7 & RF7_ZENOBIAN_FORCES))
		{
			/* Describe the "race" */
#ifdef JP
			if (flags3 & RF3_DRAGON) hook_c_roff(TERM_ORANGE, "ドラゴン");
#else
			if (flags3 & RF3_DRAGON) hooked_roff(" dragon");
#endif

#ifdef JP
			if (flags3 & RF3_DEMON)  hook_c_roff(TERM_VIOLET, "デーモン");
#else
			if (flags3 & RF3_DEMON)  hooked_roff(" demon");
#endif

#ifdef JP
			if (flags3 & RF3_GIANT)  hook_c_roff(TERM_L_UMBER, "ジャイアント");
#else
			if (flags3 & RF3_GIANT)  hooked_roff(" giant");
#endif

#ifdef JP
			if (flags3 & RF3_TROLL)  hook_c_roff(TERM_BLUE, "トロル");
#else
			if (flags3 & RF3_TROLL)  hooked_roff(" troll");
#endif

#ifdef JP
			if (flags3 & RF3_ORC)    hook_c_roff(TERM_UMBER, "オーク");
#else
			if (flags3 & RF3_ORC)    hooked_roff(" orc");
#endif

			if (flags2 & RF2_HUMAN)
			{
#ifdef JP
				if (!(flags2 & RF2_ETHNICITY_MASK)) hook_c_roff(TERM_L_WHITE, "人間");
#else
				hooked_roff(" Human");
#endif
			}

		}
#ifdef JP
		else hooked_roff("モンスター");
#else
		else hooked_roff(" creature");
#endif


#ifdef JP
		hooked_roff("を倒すことは");
#endif
		/* Group some variables */
		{
			long i, j;

#ifdef JP
			i = p_ptr->lev;
			hooked_roff(format(" %lu レベルのキャラクタにとって", (long)i));

			i = (long)r_ptr->mexp * r_ptr->level / (p_ptr->max_plv+2);
			j = ((((long)r_ptr->mexp * r_ptr->level % (p_ptr->max_plv+2)) *
			       (long)1000 / (p_ptr->max_plv+2) + 5) / 10);

			hooked_roff(format(" 約%ld.%02ld ポイントの経験となる。",
			        (long)i, (long)j ));
#else
			/* calculate the integer exp part */
			i = (long)r_ptr->mexp * r_ptr->level / (p_ptr->max_plv+2);

			/* calculate the fractional exp part scaled by 100, */
			/* must use long arithmetic to avoid overflow  */
			j = ((((long)r_ptr->mexp * r_ptr->level % (p_ptr->max_plv+2)) *
			       (long)1000 / (p_ptr->max_plv+2) + 5) / 10);

			/* Mention the experience */
			hooked_roff(format(" is worth about %ld.%02ld point%s",
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
			hooked_roff(format(" for a%s %lu%s level character.  ",
			            q, (long)i, p));
#endif

		}
	}

	if ((flags2 & RF2_AURA_FIRE) && (flags2 & RF2_AURA_ELEC) && (flags3 & RF3_AURA_COLD))
	{
#ifdef JP
		hook_c_roff(TERM_VIOLET, format("%^sは炎と氷とスパークに包まれている。", wd_he[msex]));
#else
		hooked_roff(format("%^s is surrounded by flames and electricity.  ", wd_he[msex]));
#endif

	}
	else if ((flags2 & RF2_AURA_FIRE) && (flags2 & RF2_AURA_ELEC))
	{
#ifdef JP
		hook_c_roff(TERM_L_RED, format("%^sは炎とスパークに包まれている。", wd_he[msex]));
#else
		hooked_roff(format("%^s is surrounded by flames and electricity.  ", wd_he[msex]));
#endif

	}
	else if ((flags2 & RF2_AURA_FIRE) && (flags3 & RF3_AURA_COLD))
	{
#ifdef JP
		hook_c_roff(TERM_BLUE, format("%^sは炎と氷に包まれている。", wd_he[msex]));
#else
		hooked_roff(format("%^s is surrounded by flames and electricity.  ", wd_he[msex]));
#endif

	}
	else if ((flags3 & RF3_AURA_COLD) && (flags2 & RF2_AURA_ELEC))
	{
#ifdef JP
		hook_c_roff(TERM_L_GREEN, format("%^sは氷とスパークに包まれている。", wd_he[msex]));
#else
		hooked_roff(format("%^s is surrounded by ice and electricity.  ", wd_he[msex]));
#endif

	}
	else if (flags2 & RF2_AURA_FIRE)
	{
#ifdef JP
		hook_c_roff(TERM_RED, format("%^sは炎に包まれている。", wd_he[msex]));
#else
		hooked_roff(format("%^s is surrounded by flames.  ", wd_he[msex]));
#endif

	}
	else if (flags3 & RF3_AURA_COLD)
	{
#ifdef JP
		hook_c_roff(TERM_BLUE, format("%^sは氷に包まれている。", wd_he[msex]));
#else
		hooked_roff(format("%^s is surrounded by ice.  ", wd_he[msex]));
#endif

	}
	else if (flags2 & RF2_AURA_ELEC)
	{
#ifdef JP
		hook_c_roff(TERM_L_BLUE, format("%^sはスパークに包まれている。", wd_he[msex]));
#else
		hooked_roff(format("%^s is surrounded by electricity.  ", wd_he[msex]));
#endif

	}

	if (flags2 & RF2_REFLECTING)
	{
#ifdef JP
		hooked_roff(format("%^sは矢の呪文を跳ね返す。", wd_he[msex]));
#else
		hooked_roff(format("%^s reflects bolt spells.  ", wd_he[msex]));
#endif

	}

	/* Describe escorts */
	if ((flags1 & RF1_ESCORT) || (flags1 & RF1_ESCORTS))
	{
#ifdef JP
		hooked_roff(format("%^sは通常護衛を伴って現れる。",
#else
		hooked_roff(format("%^s usually appears with escorts.  ",
#endif

		            wd_he[msex]));
	}

	/* Describe friends */
	else if (flags1 & RF1_FRIENDS)
	{
#ifdef JP
		hooked_roff(format("%^sは通常集団で現れる。",
#else
		hooked_roff(format("%^s usually appears in groups.  ",
#endif

		            wd_he[msex]));
	}


	/* Collect inate attacks */
	vn = 0;
#ifdef JP
	if (flags4 & RF4_SHRIEK)     {vp[vn] = "悲鳴で助けを求める";color[vn++] = TERM_L_WHITE;}
#else
	if (flags4 & RF4_SHRIEK)     {vp[vn] = "shriek for help";color[vn++] = TERM_L_WHITE;}
#endif

#ifdef JP
	if (flags4 & RF4_ROCKET)     {vp[vn] = "ロケットを発射する";color[vn++] = TERM_UMBER;}
#else
	if (flags4 & RF4_ROCKET)     {vp[vn] = "shoot a rocket";color[vn++] = TERM_UMBER;}
#endif

#ifdef JP
	if (flags4 & RF4_SHOOT)      {vp[vn] = "射撃をする";color[vn++] = TERM_UMBER;}
#else
	if (flags4 & RF4_SHOOT)      {vp[vn] = "fire an arrow";color[vn++] = TERM_UMBER;}
#endif

#ifdef JP
	if (flags4 & RF4_SHOOT_GUN)  {vp[vn] = "銃を撃つ";color[vn++] = TERM_UMBER;}
#else
	if (flags4 & RF4_SHOOT_GUN)  {vp[vn] = "shoot a gun";color[vn++] = TERM_UMBER;}
#endif

#ifdef JP
	if (flagsa & RFA_STONE_GAZE) {vp[vn] = "邪眼を使う";color[vn++] = TERM_SLATE;}
#else
	if (flagsa & RFA_STONE_GAZE) {vp[vn] = "use stone gaze";color[vn++] = TERM_SLATE;}
#endif

#ifdef JP
	if (flags6 & (RF6_SPECIAL))  {vp[vn] = "特別な行動をする";color[vn++] = TERM_VIOLET;}
#else
	if (flags6 & (RF6_SPECIAL))  {vp[vn] = "do something";color[vn++] = TERM_VIOLET;}
#endif

	/* Describe inate attacks */
	if (vn)
	{
		/* Intro */
#ifdef JP
		hooked_roff(format("%^sは", wd_he[msex]));
#else
		hooked_roff(format("%^s", wd_he[msex]));
#endif


		/* Scan */
		for (n = 0; n < vn; n++)
		{
#ifdef JP
			if(n!=vn-1){
			  jverb(vp[n],jverb_buf,JVERB_OR);
			  hook_c_roff(color[n], jverb_buf);
			  hook_c_roff(color[n], "り、");
			}
			else  hook_c_roff(color[n], vp[n]);
#else
			/* Intro */
			if (n == 0) hooked_roff(" may ");
			else if (n < vn-1) hooked_roff(", ");
			else hooked_roff(" or ");

			/* Dump */
			hooked_roff(vp[n]);
#endif

		}

		/* End */
#ifdef JP
		hooked_roff("ことがある。");
#else
		hooked_roff(".  ");
#endif

	}


	/* Collect breaths */
	vn = 0;
#ifdef JP
	if (flags4 & (RF4_BR_ACID))		{vp[vn] = "酸";color[vn++] = TERM_GREEN;}
#else
	if (flags4 & (RF4_BR_ACID))		{vp[vn] = "acid";color[vn++] = TERM_GREEN;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_ELEC))		{vp[vn] = "稲妻";color[vn++] = TERM_BLUE;}
#else
	if (flags4 & (RF4_BR_ELEC))		{vp[vn] = "lightning";color[vn++] = TERM_BLUE;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_FIRE))		{vp[vn] = "火炎";color[vn++] = TERM_RED;}
#else
	if (flags4 & (RF4_BR_FIRE))		{vp[vn] = "fire";color[vn++] = TERM_RED;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_COLD))		{vp[vn] = "冷気";color[vn++] = TERM_L_WHITE;}
#else
	if (flags4 & (RF4_BR_COLD))		{vp[vn] = "frost";color[vn++] = TERM_L_WHITE;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_POIS))		{vp[vn] = "毒";color[vn++] = TERM_L_GREEN;}
#else
	if (flags4 & (RF4_BR_POIS))		{vp[vn] = "poison";color[vn++] = TERM_L_GREEN;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_NETH))		{vp[vn] = "地獄";color[vn++] = TERM_L_DARK;}
#else
	if (flags4 & (RF4_BR_NETH))		{vp[vn] = "nether";color[vn++] = TERM_L_DARK;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_LITE))		{vp[vn] = "閃光";color[vn++] = TERM_YELLOW;}
#else
	if (flags4 & (RF4_BR_LITE))		{vp[vn] = "light";color[vn++] = TERM_YELLOW;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_DARK))		{vp[vn] = "暗黒";color[vn++] = TERM_L_DARK;}
#else
	if (flags4 & (RF4_BR_DARK))		{vp[vn] = "darkness";color[vn++] = TERM_L_DARK;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_CONF))		{vp[vn] = "混乱";color[vn++] = TERM_L_UMBER;}
#else
	if (flags4 & (RF4_BR_CONF))		{vp[vn] = "confusion";color[vn++] = TERM_L_UMBER;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_SOUN))		{vp[vn] = "轟音";color[vn++] = TERM_ORANGE;}
#else
	if (flags4 & (RF4_BR_SOUN))		{vp[vn] = "sound";color[vn++] = TERM_ORANGE;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_CHAO))		{vp[vn] = "カオス";color[vn++] = TERM_VIOLET;}
#else
	if (flags4 & (RF4_BR_CHAO))		{vp[vn] = "chaos";color[vn++] = TERM_VIOLET;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_DISE))		{vp[vn] = "劣化";color[vn++] = TERM_VIOLET;}
#else
	if (flags4 & (RF4_BR_DISE))		{vp[vn] = "disenchantment";color[vn++] = TERM_VIOLET;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_STON))		{vp[vn] = "石化";color[vn++] = TERM_SLATE;}
#else
	if (flags4 & (RF4_BR_STON))		{vp[vn] = "stone";color[vn++] = TERM_SLATE;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_TIME))		{vp[vn] = "時間逆転";color[vn++] = TERM_L_BLUE;}
#else
	if (flags4 & (RF4_BR_TIME))		{vp[vn] = "time";color[vn++] = TERM_L_BLUE;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_INER))		{vp[vn] = "遅鈍";color[vn++] = TERM_SLATE;}
#else
	if (flags4 & (RF4_BR_INER))		{vp[vn] = "inertia";color[vn++] = TERM_SLATE;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_GRAV))		{vp[vn] = "重力";color[vn++] = TERM_SLATE;}
#else
	if (flags4 & (RF4_BR_GRAV))		{vp[vn] = "gravity";color[vn++] = TERM_SLATE;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_SHAR))		{vp[vn] = "破片";color[vn++] = TERM_L_UMBER;}
#else
	if (flags4 & (RF4_BR_SHAR))		{vp[vn] = "shards";color[vn++] = TERM_L_UMBER;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_PLAS))		{vp[vn] = "プラズマ";color[vn++] = TERM_L_RED;}
#else
	if (flags4 & (RF4_BR_PLAS))		{vp[vn] = "plasma";color[vn++] = TERM_L_RED;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_WALL))		{vp[vn] = "フォース";color[vn++] = TERM_UMBER;}
#else
	if (flags4 & (RF4_BR_WALL))		{vp[vn] = "force";color[vn++] = TERM_UMBER;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_MANA))		{vp[vn] = "魔力";color[vn++] = TERM_L_BLUE;}
#else
	if (flags4 & (RF4_BR_MANA))		{vp[vn] = "mana";color[vn++] = TERM_L_BLUE;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_NUKE))		{vp[vn] = "放射性廃棄物";color[vn++] = TERM_L_GREEN;}
#else
	if (flags4 & (RF4_BR_NUKE))		{vp[vn] = "toxic waste";color[vn++] = TERM_L_GREEN;}
#endif

#ifdef JP
	if (flags4 & (RF4_BR_DISI))		{vp[vn] = "分解";color[vn++] = TERM_SLATE;}
#else
	if (flags4 & (RF4_BR_DISI))		{vp[vn] = "disintegration";color[vn++] = TERM_SLATE;}
#endif

#ifdef JP
	if (flagsa & (RFA_BR_PURE_FIRE))	{vp[vn] = "*火炎*";color[vn++] = elem_attr(ELEM_FIRE);}
#else
	if (flagsa & (RFA_BR_PURE_FIRE))	{vp[vn] = "*fire*";color[vn++] = elem_attr(ELEM_FIRE);}
#endif

#ifdef JP
	if (flagsa & (RFA_BR_PURE_AQUA))	{vp[vn] = "*水*";color[vn++] = elem_attr(ELEM_AQUA);}
#else
	if (flagsa & (RFA_BR_PURE_AQUA))	{vp[vn] = "*aqua*";color[vn++] = elem_attr(ELEM_AQUA);}
#endif

#ifdef JP
	if (flagsa & (RFA_BR_PURE_EARTH))	{vp[vn] = "*大地*";color[vn++] = elem_attr(ELEM_EARTH);}
#else
	if (flagsa & (RFA_BR_PURE_EARTH))	{vp[vn] = "*earth*";color[vn++] = elem_attr(ELEM_EARTH);}
#endif

#ifdef JP
	if (flagsa & (RFA_BR_PURE_WIND))	{vp[vn] = "*風*";color[vn++] = elem_attr(ELEM_WIND);}
#else
	if (flagsa & (RFA_BR_PURE_WIND))	{vp[vn] = "*wind*";color[vn++] = elem_attr(ELEM_WIND);}
#endif


	/* Describe breaths */
	if (vn)
	{
		/* Note breath */
		breath = TRUE;

		/* Intro */
#ifdef JP
		hooked_roff(format("%^sは", wd_he[msex]));
#else
		hooked_roff(format("%^s", wd_he[msex]));
#endif


		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
#ifdef JP
			if ( n != 0 ) hooked_roff("や");
#else
			if (n == 0) hooked_roff(" may breathe ");
			else if (n < vn-1) hooked_roff(", ");
			else hooked_roff(" or ");
#endif


			/* Dump */
			hook_c_roff(color[n], vp[n]);
		}
#ifdef JP
		hooked_roff("のブレスを吐くことがある");
#endif
	}


	/* Collect spells */
	vn = 0;
#ifdef JP
	if (flags5 & (RF5_BA_ACID))         {vp[vn] = "アシッドクラウド";color[vn++] = TERM_GREEN;}
#else
	if (flags5 & (RF5_BA_ACID))         {vp[vn] = "produce acid clouds";color[vn++] = TERM_GREEN;}
#endif

#ifdef JP
	if (flags5 & (RF5_BA_ELEC))         {vp[vn] = "サンダーフレア";color[vn++] = TERM_BLUE;}
#else
	if (flags5 & (RF5_BA_ELEC))         {vp[vn] = "produce thunder flares";color[vn++] = TERM_BLUE;}
#endif

#ifdef JP
	if (flags5 & (RF5_BA_FIRE))         {vp[vn] = "ファイアストーム";color[vn++] = TERM_RED;}
#else
	if (flags5 & (RF5_BA_FIRE))         {vp[vn] = "produce fire storms";color[vn++] = TERM_RED;}
#endif

#ifdef JP
	if (flags5 & (RF5_BA_COLD))         {vp[vn] = "アイスブラスト";color[vn++] = TERM_L_WHITE;}
#else
	if (flags5 & (RF5_BA_COLD))         {vp[vn] = "produce ice blasts";color[vn++] = TERM_L_WHITE;}
#endif

#ifdef JP
	if (flags5 & (RF5_BA_POIS))         {vp[vn] = "悪臭雲";color[vn++] = TERM_L_GREEN;}
#else
	if (flags5 & (RF5_BA_POIS))         {vp[vn] = "produce poison balls";color[vn++] = TERM_L_GREEN;}
#endif

#ifdef JP
	if (flags5 & (RF5_BA_NETH))         {vp[vn] = "地獄球";color[vn++] = TERM_L_DARK;}
#else
	if (flags5 & (RF5_BA_NETH))         {vp[vn] = "produce nether balls";color[vn++] = TERM_L_DARK;}
#endif

#ifdef JP
	if (flags5 & (RF5_BA_WATE))         {vp[vn] = "ウォーター・ボール";color[vn++] = TERM_BLUE;}
#else
	if (flags5 & (RF5_BA_WATE))         {vp[vn] = "produce water balls";color[vn++] = TERM_BLUE;}
#endif

#ifdef JP
	if (flags4 & (RF4_BA_NUKE))         {vp[vn] = "放射能球";color[vn++] = TERM_L_GREEN;}
#else
	if (flags4 & (RF4_BA_NUKE))         {vp[vn] = "produce balls of radiation";color[vn++] = TERM_L_GREEN;}
#endif

#ifdef JP
	if (flags5 & (RF5_BA_MANA))         {vp[vn] = "魔力の嵐";color[vn++] = TERM_L_BLUE;}
#else
	if (flags5 & (RF5_BA_MANA))         {vp[vn] = "invoke mana storms";color[vn++] = TERM_L_BLUE;}
#endif

#ifdef JP
	if (flags5 & (RF5_BA_DARK))         {vp[vn] = "暗黒の嵐";color[vn++] = TERM_L_DARK;}
#else
	if (flags5 & (RF5_BA_DARK))         {vp[vn] = "invoke darkness storms";color[vn++] = TERM_L_DARK;}
#endif

#ifdef JP
	if (flags5 & (RF5_BA_LITE))         {vp[vn] = "スターバースト";color[vn++] = TERM_YELLOW;}
#else
	if (flags5 & (RF5_BA_LITE))         {vp[vn] = "invoke starburst";color[vn++] = TERM_YELLOW;}
#endif

#ifdef JP
	if (flags4 & (RF4_BA_CHAO))         {vp[vn] = "純カオス";color[vn++] = TERM_VIOLET;}
#else
	if (flags4 & (RF4_BA_CHAO))         {vp[vn] = "invoke raw chaos";color[vn++] = TERM_VIOLET;}
#endif

#ifdef JP
	if (flags6 & (RF6_HAND_DOOM))       {vp[vn] = "破滅の手";color[vn++] = TERM_VIOLET;}
#else
	if (flags6 & (RF6_HAND_DOOM))       {vp[vn] = "invoke the Hand of Doom";color[vn++] = TERM_VIOLET;}
#endif

#ifdef JP
	if (flags6 & (RF6_GODLY_SPEAR))     {vp[vn] = "神の槍";color[vn++] = TERM_YELLOW;}
#else
	if (flags6 & (RF6_GODLY_SPEAR))     {vp[vn] = "throw godly spears";color[vn++] = TERM_YELLOW;}
#endif

#ifdef JP
	if (flagsa & (RFA_PURE_ELEM_BEAM))  {vp[vn] = "*元素*ビーム";color[vn++] = TERM_YELLOW;}
#else
	if (flagsa & (RFA_PURE_ELEM_BEAM))  {vp[vn] = "invoke *element* beams";color[vn++] = TERM_YELLOW;}
#endif

#ifdef JP
	if (flags5 & (RF5_DRAIN_MANA))      {vp[vn] = "魔力吸収";color[vn++] = TERM_SLATE;}
#else
	if (flags5 & (RF5_DRAIN_MANA))      {vp[vn] = "drain mana";color[vn++] = TERM_SLATE;}
#endif

#ifdef JP
	if (flags5 & (RF5_MIND_BLAST))      {vp[vn] = "精神攻撃";color[vn++] = TERM_L_RED;}
#else
	if (flags5 & (RF5_MIND_BLAST))      {vp[vn] = "cause mind blasting";color[vn++] = TERM_L_RED;}
#endif

#ifdef JP
	if (flags5 & (RF5_BRAIN_SMASH))     {vp[vn] = "脳攻撃";color[vn++] = TERM_RED;}
#else
	if (flags5 & (RF5_BRAIN_SMASH))     {vp[vn] = "cause brain smashing";color[vn++] = TERM_RED;}
#endif

#ifdef JP
	if (flags5 & (RF5_CAUSE_1))         {vp[vn] = "軽傷＋呪い";color[vn++] = TERM_L_WHITE;}
#else
	if (flags5 & (RF5_CAUSE_1))         {vp[vn] = "cause light wounds and cursing";color[vn++] = TERM_L_WHITE;}
#endif

#ifdef JP
	if (flags5 & (RF5_CAUSE_2))         {vp[vn] = "重傷＋呪い";color[vn++] = TERM_L_WHITE;}
#else
	if (flags5 & (RF5_CAUSE_2))         {vp[vn] = "cause serious wounds and cursing";color[vn++] = TERM_L_WHITE;}
#endif

#ifdef JP
	if (flags5 & (RF5_CAUSE_3))         {vp[vn] = "致命傷＋呪い";color[vn++] = TERM_L_WHITE;}
#else
	if (flags5 & (RF5_CAUSE_3))         {vp[vn] = "cause critical wounds and cursing";color[vn++] = TERM_L_WHITE;}
#endif

#ifdef JP
	if (flags5 & (RF5_CAUSE_4))         {vp[vn] = "瀕死傷";color[vn++] = TERM_L_WHITE;}
#else
	if (flags5 & (RF5_CAUSE_4))         {vp[vn] = "cause mortal wounds";color[vn++] = TERM_L_WHITE;}
#endif

#ifdef JP
	if (flags5 & (RF5_BO_ACID))         {vp[vn] = "アシッド・ボルト";color[vn++] = TERM_GREEN;}
#else
	if (flags5 & (RF5_BO_ACID))         {vp[vn] = "produce acid bolts";color[vn++] = TERM_GREEN;}
#endif

#ifdef JP
	if (flags5 & (RF5_BO_ELEC))         {vp[vn] = "サンダー・ボルト";color[vn++] = TERM_BLUE;}
#else
	if (flags5 & (RF5_BO_ELEC))         {vp[vn] = "produce lightning bolts";color[vn++] = TERM_BLUE;}
#endif

#ifdef JP
	if (flags5 & (RF5_BO_FIRE))         {vp[vn] = "ファイア・ボルト";color[vn++] = TERM_RED;}
#else
	if (flags5 & (RF5_BO_FIRE))         {vp[vn] = "produce fire bolts";color[vn++] = TERM_RED;}
#endif

#ifdef JP
	if (flags5 & (RF5_BO_COLD))         {vp[vn] = "アイス・ボルト";color[vn++] = TERM_L_WHITE;}
#else
	if (flags5 & (RF5_BO_COLD))         {vp[vn] = "produce frost bolts";color[vn++] = TERM_L_WHITE;}
#endif

#ifdef JP
	if (flags5 & (RF5_BO_NETH))         {vp[vn] = "地獄の矢";color[vn++] = TERM_L_DARK;}
#else
	if (flags5 & (RF5_BO_NETH))         {vp[vn] = "produce nether bolts";color[vn++] = TERM_L_DARK;}
#endif

#ifdef JP
	if (flags5 & (RF5_BO_WATE))         {vp[vn] = "ウォーター・ボルト";color[vn++] = TERM_BLUE;}
#else
	if (flags5 & (RF5_BO_WATE))         {vp[vn] = "produce water bolts";color[vn++] = TERM_BLUE;}
#endif

#ifdef JP
	if (flags5 & (RF5_BO_MANA))         {vp[vn] = "魔力の矢";color[vn++] = TERM_L_BLUE;}
#else
	if (flags5 & (RF5_BO_MANA))         {vp[vn] = "produce mana bolts";color[vn++] = TERM_L_BLUE;}
#endif

#ifdef JP
	if (flags5 & (RF5_BO_PLAS))         {vp[vn] = "プラズマ・ボルト";color[vn++] = TERM_L_RED;}
#else
	if (flags5 & (RF5_BO_PLAS))         {vp[vn] = "produce plasma bolts";color[vn++] = TERM_L_RED;}
#endif

#ifdef JP
	if (flags5 & (RF5_BO_ICEE))         {vp[vn] = "極寒の矢";color[vn++] = TERM_WHITE;}
#else
	if (flags5 & (RF5_BO_ICEE))         {vp[vn] = "produce ice bolts";color[vn++] = TERM_WHITE;}
#endif

#ifdef JP
	if (flags5 & (RF5_MISSILE))         {vp[vn] = "マジックミサイル";color[vn++] = TERM_SLATE;}
#else
	if (flags5 & (RF5_MISSILE))         {vp[vn] = "produce magic missiles";color[vn++] = TERM_SLATE;}
#endif

#ifdef JP
	if (flags5 & (RF5_SCARE))           {vp[vn] = "恐怖";color[vn++] = TERM_SLATE;}
#else
	if (flags5 & (RF5_SCARE))           {vp[vn] = "terrify";color[vn++] = TERM_SLATE;}
#endif

#ifdef JP
	if (flags5 & (RF5_BLIND))           {vp[vn] = "目くらまし";color[vn++] = TERM_L_DARK;}
#else
	if (flags5 & (RF5_BLIND))           {vp[vn] = "blind";color[vn++] = TERM_L_DARK;}
#endif

#ifdef JP
	if (flags5 & (RF5_CONF))            {vp[vn] = "混乱";color[vn++] = TERM_L_UMBER;}
#else
	if (flags5 & (RF5_CONF))            {vp[vn] = "confuse";color[vn++] = TERM_L_UMBER;}
#endif

#ifdef JP
	if (flags5 & (RF5_SLOW))            {vp[vn] = "減速";color[vn++] = TERM_UMBER;}
#else
	if (flags5 & (RF5_SLOW))            {vp[vn] = "slow";color[vn++] = TERM_UMBER;}
#endif

#ifdef JP
	if (flags5 & (RF5_HOLD))            {vp[vn] = "麻痺";color[vn++] = TERM_RED;}
#else
	if (flags5 & (RF5_HOLD))            {vp[vn] = "paralyze";color[vn++] = TERM_RED;}
#endif

#ifdef JP
	if (flags6 & (RF6_HASTE))           {vp[vn] = "加速";color[vn++] = TERM_L_GREEN;}
#else
	if (flags6 & (RF6_HASTE))           {vp[vn] = "haste-self";color[vn++] = TERM_L_GREEN;}
#endif

#ifdef JP
	if (flags6 & (RF6_HEAL))            {vp[vn] = "治癒";color[vn++] = TERM_WHITE;}
#else
	if (flags6 & (RF6_HEAL))            {vp[vn] = "heal-self";color[vn++] = TERM_WHITE;}
#endif

#ifdef JP
	if (flags6 & (RF6_INVULNER))        {vp[vn] = "無敵化";color[vn++] = TERM_WHITE;}
#else
	if (flags6 & (RF6_INVULNER))        {vp[vn] = "make invulnerable";color[vn++] = TERM_WHITE;}
#endif

#ifdef JP
	if (flags4 & RF4_DISPEL)            {vp[vn] = "魔力消去";color[vn++] = TERM_L_WHITE;}
#else
	if (flags4 & RF4_DISPEL)            {vp[vn] = "dispel-magic";color[vn++] = TERM_L_WHITE;}
#endif

#ifdef JP
	if (flags6 & (RF6_BLINK))           {vp[vn] = "ショートテレポート";color[vn++] = TERM_UMBER;}
#else
	if (flags6 & (RF6_BLINK))           {vp[vn] = "blink-self";color[vn++] = TERM_UMBER;}
#endif

#ifdef JP
	if (flags6 & (RF6_TPORT))           {vp[vn] = "テレポート";color[vn++] = TERM_ORANGE;}
#else
	if (flags6 & (RF6_TPORT))           {vp[vn] = "teleport-self";color[vn++] = TERM_ORANGE;}
#endif

#ifdef JP
	if (flags6 & (RF6_STOP_TIME))       {vp[vn] = "時間停止";color[vn++] = TERM_L_BLUE;}
#else
	if (flags6 & (RF6_STOP_TIME))       {vp[vn] = "stop the time";color[vn++] = TERM_L_BLUE;}
#endif

#ifdef JP
	if (flags6 & (RF6_TELE_TO))         {vp[vn] = "テレポートバック";color[vn++] = TERM_L_UMBER;}
#else
	if (flags6 & (RF6_TELE_TO))         {vp[vn] = "teleport to";color[vn++] = TERM_L_UMBER;}
#endif

#ifdef JP
	if (flags6 & (RF6_TELE_AWAY))       {vp[vn] = "テレポートアウェイ";color[vn++] = TERM_UMBER;}
#else
	if (flags6 & (RF6_TELE_AWAY))       {vp[vn] = "teleport away";color[vn++] = TERM_UMBER;}
#endif

#ifdef JP
	if (flags6 & (RF6_TELE_LEVEL))      {vp[vn] = "テレポート・レベル";color[vn++] = TERM_ORANGE;}
#else
	if (flags6 & (RF6_TELE_LEVEL))      {vp[vn] = "teleport level";color[vn++] = TERM_ORANGE;}
#endif

	if (flags6 & (RF6_DARKNESS))
	{
		if (r_ptr->flags3 & (RF3_UNDEAD | RF3_HURT_LITE))
		{
#ifdef JP
			vp[vn] =  "暗闇"; color[vn++] = TERM_L_DARK;
#else
			vp[vn] = "create darkness"; color[vn++] = TERM_L_DARK;
#endif
		}
		else
		{
#ifdef JP
			vp[vn] = "閃光"; color[vn++] = TERM_YELLOW;
#else
			vp[vn] = "create light"; color[vn++] = TERM_YELLOW;
#endif
		}
	}

#ifdef JP
	if (flags6 & (RF6_TRAPS))           {vp[vn] = "トラップ";color[vn++] = TERM_BLUE;}
#else
	if (flags6 & (RF6_TRAPS))           {vp[vn] = "create traps";color[vn++] = TERM_BLUE;}
#endif

#ifdef JP
	if (flags6 & (RF6_FORGET))          {vp[vn] = "記憶消去";color[vn++] = TERM_BLUE;}
#else
	if (flags6 & (RF6_FORGET))          {vp[vn] = "cause amnesia";color[vn++] = TERM_BLUE;}
#endif

#ifdef JP
	if (flags6 & (RF6_RAISE_DEAD))      {vp[vn] = "死者復活";color[vn++] = TERM_RED;}
#else
	if (flags6 & (RF6_RAISE_DEAD))      {vp[vn] = "raise dead";color[vn++] = TERM_RED;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_MONSTER))       {vp[vn] = "モンスター一体召喚";color[vn++] = TERM_SLATE;}
#else
	if (flags6 & (RF6_S_MONSTER))       {vp[vn] = "summon a monster";color[vn++] = TERM_SLATE;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_MONSTERS))      {vp[vn] = "モンスター複数召喚";color[vn++] = TERM_L_WHITE;}
#else
	if (flags6 & (RF6_S_MONSTERS))      {vp[vn] = "summon monsters";color[vn++] = TERM_L_WHITE;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_KIN))           {vp[vn] = "救援召喚";color[vn++] = TERM_ORANGE;}
#else
	if (flags6 & (RF6_S_KIN))           {vp[vn] = "summon aid";color[vn++] = TERM_ORANGE;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_ANT))           {vp[vn] = "アリ召喚";color[vn++] = TERM_RED;}
#else
	if (flags6 & (RF6_S_ANT))           {vp[vn] = "summon ants";color[vn++] = TERM_RED;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_SPIDER))        {vp[vn] = "クモ召喚";color[vn++] = TERM_L_DARK;}
#else
	if (flags6 & (RF6_S_SPIDER))        {vp[vn] = "summon spiders";color[vn++] = TERM_L_DARK;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_HOUND))         {vp[vn] = "ハウンド召喚";color[vn++] = TERM_L_UMBER;}
#else
	if (flags6 & (RF6_S_HOUND))         {vp[vn] = "summon hounds";color[vn++] = TERM_L_UMBER;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_BEAST))         {vp[vn] = "魔獣召喚";color[vn++] = TERM_L_GREEN;}
#else
	if (flags6 & (RF6_S_BEAST))         {vp[vn] = "summon beasts";color[vn++] = TERM_L_GREEN;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_ANGEL))         {vp[vn] = "天使一体召喚";color[vn++] = TERM_YELLOW;}
#else
	if (flags6 & (RF6_S_ANGEL))         {vp[vn] = "summon an angel";color[vn++] = TERM_YELLOW;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_DEMON))         {vp[vn] = "デーモン一体召喚";color[vn++] = TERM_L_RED;}
#else
	if (flags6 & (RF6_S_DEMON))         {vp[vn] = "summon a demon";color[vn++] = TERM_L_RED;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_UNDEAD))        {vp[vn] = "アンデッド一体召喚";color[vn++] = TERM_L_DARK;}
#else
	if (flags6 & (RF6_S_UNDEAD))        {vp[vn] = "summon an undead";color[vn++] = TERM_L_DARK;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_DRAGON))        {vp[vn] = "ドラゴン一体召喚";color[vn++] = TERM_ORANGE;}
#else
	if (flags6 & (RF6_S_DRAGON))        {vp[vn] = "summon a dragon";color[vn++] = TERM_ORANGE;}
#endif

#ifdef JP
	if (flagsa & (RFA_S_HI_DEMON))      {vp[vn] = "上級デーモン召喚";color[vn++] = TERM_L_RED;}
#else
	if (flagsa & (RFA_S_HI_DEMON))      {vp[vn] = "summon Major Demons";color[vn++] = TERM_L_RED;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_HI_UNDEAD))     {vp[vn] = "強力なアンデッド召喚";color[vn++] = TERM_L_DARK;}
#else
	if (flags6 & (RF6_S_HI_UNDEAD))     {vp[vn] = "summon Greater Undead";color[vn++] = TERM_L_DARK;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_HI_DRAGON))     {vp[vn] = "古代ドラゴン召喚";color[vn++] = TERM_ORANGE;}
#else
	if (flags6 & (RF6_S_HI_DRAGON))     {vp[vn] = "summon Ancient Dragons";color[vn++] = TERM_ORANGE;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_CYBER))         {vp[vn] = "サイバーデーモン召喚";color[vn++] = TERM_UMBER;}
#else
	if (flags6 & (RF6_S_CYBER))         {vp[vn] = "summon Cyberdemons";color[vn++] = TERM_UMBER;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_TEMPLES))       {vp[vn] = "暗黒騎士召喚";color[vn++] = TERM_VIOLET;}
#else
	if (flags6 & (RF6_S_TEMPLES))       {vp[vn] = "summon Temple Knights";color[vn++] = TERM_VIOLET;}
#endif

#ifdef JP
	if (flagsa & (RFA_S_ZENOBIAN))      {vp[vn] = "神聖騎士召喚";color[vn++] = TERM_L_WHITE;}
#else
	if (flagsa & (RFA_S_ZENOBIAN))      {vp[vn] = "summon White Knights";color[vn++] = TERM_L_WHITE;}
#endif

#ifdef JP
	if (flags6 & (RF6_S_UNIQUE))        {vp[vn] = "ユニーク・モンスター召喚";color[vn++] = TERM_VIOLET;}
#else
	if (flags6 & (RF6_S_UNIQUE))        {vp[vn] = "summon Unique Monsters";color[vn++] = TERM_VIOLET;}
#endif

#ifdef JP
	if (flagsa & (RFA_FIRE_STORM))      {vp[vn] = "*火炎*の嵐";color[vn++] = elem_attr(ELEM_FIRE);}
#else
	if (flagsa & (RFA_FIRE_STORM))      {vp[vn] = "invoke *fire* storms";color[vn++] = elem_attr(ELEM_FIRE);}
#endif

#ifdef JP
	if (flagsa & (RFA_AQUA_STORM))      {vp[vn] = "*水*の嵐";color[vn++] = elem_attr(ELEM_AQUA);}
#else
	if (flagsa & (RFA_AQUA_STORM))      {vp[vn] = "invoke *aqua* storms";color[vn++] = elem_attr(ELEM_AQUA);}
#endif

#ifdef JP
	if (flagsa & (RFA_EARTH_STORM))     {vp[vn] = "*大地*の嵐";color[vn++] = elem_attr(ELEM_EARTH);}
#else
	if (flagsa & (RFA_EARTH_STORM))     {vp[vn] = "invoke *earth* storms";color[vn++] = elem_attr(ELEM_EARTH);}
#endif

#ifdef JP
	if (flagsa & (RFA_WIND_STORM))      {vp[vn] = "*風*の嵐";color[vn++] = elem_attr(ELEM_WIND);}
#else
	if (flagsa & (RFA_WIND_STORM))      {vp[vn] = "invoke *wind* storms";color[vn++] = elem_attr(ELEM_WIND);}
#endif

#ifdef JP
	if (flagsa & (RFA_PETRO_CLOUD))     {vp[vn] = "ペトロクラウド";color[vn++] = TERM_SLATE;}
#else
	if (flagsa & (RFA_PETRO_CLOUD))     {vp[vn] = "produce petroclouds";color[vn++] = TERM_SLATE;}
#endif

#ifdef JP
	if (flagsa & (RFA_SAND_STORM))      {vp[vn] = "神砂嵐";color[vn++] = TERM_L_UMBER;}
#else
	if (flagsa & (RFA_SAND_STORM))      {vp[vn] = "invoke godly sand storms";color[vn++] = TERM_L_UMBER;}
#endif

#ifdef JP
	if (flagsa & (RFA_ERASE_ELEM))      {vp[vn] = "エレメント消去";color[vn++] = TERM_L_WHITE;}
#else
	if (flagsa & (RFA_ERASE_ELEM))      {vp[vn] = "erase elements";color[vn++] = TERM_L_WHITE;}
#endif

#ifdef JP
	if (flagsa & (RFA_CHANGE_ELEM))     {vp[vn] = "エレメント・チェンジ";color[vn++] = TERM_YELLOW;}
#else
	if (flagsa & (RFA_CHANGE_ELEM))     {vp[vn] = "change elements";color[vn++] = TERM_YELLOW;}
#endif

#ifdef JP
	if (flags4 & (RF4_SHIFT_ELEM))      {vp[vn] = "シフト・エレメント"; color[vn++] = TERM_YELLOW;}
#else
	if (flags4 & (RF4_SHIFT_ELEM))      {vp[vn] = "shift element"; color[vn++] = TERM_YELLOW;}
#endif

#ifdef JP
	if (flagsa & (RFA_SALAMANDER))      {vp[vn] = "精霊サラマンダー召喚";color[vn++] = TERM_RED;}
#else
	if (flagsa & (RFA_SALAMANDER))      {vp[vn] = "call the Salamander";color[vn++] = TERM_RED;}
#endif

#ifdef JP
	if (flagsa & (RFA_FENRER))          {vp[vn] = "精霊フェンリル召喚";color[vn++] = TERM_L_WHITE;}
#else
	if (flagsa & (RFA_FENRER))          {vp[vn] = "call the Fenrer";color[vn++] = TERM_L_WHITE;}
#endif

#ifdef JP
	if (flagsa & (RFA_GNOME))           {vp[vn] = "精霊ノーム召喚";color[vn++] = TERM_GREEN;}
#else
	if (flagsa & (RFA_GNOME))           {vp[vn] = "call the Gnome";color[vn++] = TERM_GREEN;}
#endif

#ifdef JP
	if (flagsa & (RFA_THUNDERBIRD))     {vp[vn] = "精霊サンダーバード召喚";color[vn++] = TERM_BLUE;}
#else
	if (flagsa & (RFA_THUNDERBIRD))     {vp[vn] = "call the Thunderbird";color[vn++] = TERM_BLUE;}
#endif

#ifdef JP
	if (flagsa & (RFA_IGNIS_FATUUS))    {vp[vn] = "精霊イグニスファタス召喚";color[vn++] = TERM_WHITE;}
#else
	if (flagsa & (RFA_IGNIS_FATUUS))    {vp[vn] = "call the Ignis Fatuus";color[vn++] = TERM_WHITE;}
#endif

#ifdef JP
	if (flagsa & (RFA_DARK_LORE))       {vp[vn] = "精霊ファントム召喚";color[vn++] = TERM_L_DARK;}
#else
	if (flagsa & (RFA_DARK_LORE))       {vp[vn] = "call the Phantom";color[vn++] = TERM_L_DARK;}
#endif

#ifdef JP
	if (flagsa & (RFA_HOLY_ORB))        {vp[vn] = "聖なる光球";color[vn++] = TERM_WHITE;}
#else
	if (flagsa & (RFA_HOLY_ORB))        {vp[vn] = "produce holy orbs";color[vn++] = TERM_WHITE;}
#endif

#ifdef JP
	if (flagsa & (RFA_DARK_FIRE))       {vp[vn] = "闇の焔";color[vn++] = TERM_L_DARK;}
#else
	if (flagsa & (RFA_DARK_FIRE))       {vp[vn] = "produce petit hell fires";color[vn++] = TERM_L_DARK;}
#endif

#ifdef JP
	if (flagsa & (RFA_BA_DISI))         {vp[vn] = "原子分解";color[vn++] = TERM_SLATE;}
#else
	if (flagsa & (RFA_BA_DISI))         {vp[vn] = "invoke disintegration";color[vn++] = TERM_SLATE;}
#endif


	/* Describe spells */
	if (vn)
	{
		/* Note magic */
		magic = TRUE;

		/* Intro */
		if (breath)
		{
#ifdef JP
			hooked_roff("、なおかつ");
#else
			hooked_roff(", and is also");
#endif

		}
		else
		{
#ifdef JP
			hooked_roff(format("%^sは", wd_he[msex]));
#else
			hooked_roff(format("%^s is", wd_he[msex]));
#endif

		}

#ifdef JP
		/* Adverb */
		if (flags2 & (RF2_SMART)) hook_c_roff(TERM_YELLOW, "的確に");

		/* Verb Phrase */
		hooked_roff("魔法を使うことができ、");
#else
		/* Verb Phrase */
		hooked_roff(" magical, casting spells");

		/* Adverb */
		if (flags2 & RF2_SMART) hooked_roff(" intelligently");
#endif


		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
#ifdef JP
			if ( n != 0 ) hooked_roff("、");
#else
			if (n == 0) hooked_roff(" which ");
			else if (n < vn-1) hooked_roff(", ");
			else hooked_roff(" or ");
#endif


			/* Dump */
			hook_c_roff(color[n], vp[n]);
		}
#ifdef JP
		hooked_roff("の呪文を唱えることがある");
#endif
	}


	/* End the sentence about inate/other spells */
	if (breath || magic)
	{
		/* Total casting */
		m = r_ptr->r_cast_inate + r_ptr->r_cast_spell;

		/* Average frequency */
		n = (r_ptr->freq_inate + r_ptr->freq_spell) / 2;

		/* Describe the spell frequency */
		if (m > 100 || know_everything)
		{
#ifdef JP
			hooked_roff(format("(確率:1/%d)", 100 / n));
#else
			hooked_roff(format("; 1 time in %d", 100 / n));
#endif

		}

		/* Guess at the frequency */
		else if (m)
		{
			n = ((n + 9) / 10) * 10;
#ifdef JP
			hooked_roff(format("(確率:約1/%d)", 100 / n));
#else
			hooked_roff(format("; about 1 time in %d", 100 / n));
#endif

		}

		/* End this sentence */
#ifdef JP
		hooked_roff("。");
#else
		hooked_roff(".  ");
#endif

	}

	/* Describe monster "toughness" */
	if (know_armour(r_idx) || know_everything)
	{
		/* Armor */
#ifdef JP
		hooked_roff(format("%^sは AC%d の防御力と",
#else
		hooked_roff(format("%^s has an armor rating of %d",
#endif

		            wd_he[msex], r_ptr->ac));

		/* Maximized hitpoints */
		if (flags1 & RF1_FORCE_MAXHP)
		{
#ifdef JP
			hooked_roff(format(" %d の体力がある。",
#else
			hooked_roff(format(" and a life rating of %d.  ",
#endif

			            MIN(20000000, r_ptr->hdice * r_ptr->hside)));
		}

		/* Variable hitpoints */
		else
		{
#ifdef JP
			hooked_roff(format(" %dd%d の体力がある。",
#else
			hooked_roff(format(" and a life rating of %dd%d.  ",
#endif

			            r_ptr->hdice, r_ptr->hside));
		}
	}



	/* Collect special abilities. */
	vn = 0;
#ifdef JP
if (flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) vp[vn++] = "ダンジョンを照らす";
#else
	if (flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) vp[vn++] = "illuminate the dungeon";
#endif

#ifdef JP
if (flags2 & RF2_OPEN_DOOR) vp[vn++] = "ドアを開ける";
#else
	if (flags2 & RF2_OPEN_DOOR) vp[vn++] = "open doors";
#endif

#ifdef JP
if (flags2 & RF2_BASH_DOOR) vp[vn++] = "ドアを打ち破る";
#else
	if (flags2 & RF2_BASH_DOOR) vp[vn++] = "bash down doors";
#endif

#ifdef JP
if (flags2 & RF2_PASS_WALL) vp[vn++] = "壁をすり抜ける";
#else
	if (flags2 & RF2_PASS_WALL) vp[vn++] = "pass through walls";
#endif

#ifdef JP
if (flags2 & RF2_KILL_WALL) vp[vn++] = "壁を掘り進む";
#else
	if (flags2 & RF2_KILL_WALL) vp[vn++] = "bore through walls";
#endif

#ifdef JP
if (flags2 & RF2_MOVE_BODY) vp[vn++] = "弱いモンスターを押しのける";
#else
	if (flags2 & RF2_MOVE_BODY) vp[vn++] = "push past weaker monsters";
#endif

#ifdef JP
if (flags2 & RF2_KILL_BODY) vp[vn++] = "弱いモンスターを倒す";
#else
	if (flags2 & RF2_KILL_BODY) vp[vn++] = "destroy weaker monsters";
#endif

#ifdef JP
if (flags2 & RF2_TAKE_ITEM) vp[vn++] = "アイテムを拾う";
#else
	if (flags2 & RF2_TAKE_ITEM) vp[vn++] = "pick up objects";
#endif

#ifdef JP
if (flags2 & RF2_KILL_ITEM) vp[vn++] = "アイテムを壊す";
#else
	if (flags2 & RF2_KILL_ITEM) vp[vn++] = "destroy objects";
#endif


	/* Describe special abilities. */
	if (vn)
	{
		/* Intro */
#ifdef JP
		hooked_roff(format("%^sは", wd_he[msex]));
#else
		hooked_roff(format("%^s", wd_he[msex]));
#endif


		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
#ifdef JP
			if(n!=vn-1){
			  jverb(vp[n],jverb_buf,JVERB_AND);
			  hooked_roff(jverb_buf);
			  hooked_roff("、");
			}
			else  hooked_roff(vp[n]);
#else
			if (n == 0) hooked_roff(" can ");
			else if (n < vn-1) hooked_roff(", ");
			else hooked_roff(" and ");

			/* Dump */
			hooked_roff(vp[n]);
#endif

		}

		/* End */
#ifdef JP
		hooked_roff("ことができる。");
#else
		hooked_roff(".  ");
#endif

	}


	/* Describe special abilities. */
	if (flags3 & RF3_ANTI_MAGIC)
	{
#ifdef JP
		hook_c_roff(TERM_VIOLET, format("%^sは反魔法フィールドを張っている。", wd_he[msex]));
#else
		hook_c_roff(TERM_VIOLET, format("%^s is surrounded by anti-magic field.  ", wd_he[msex]));
#endif

	}
	if (flags3 & RF3_FEAR_FIELD)
	{
#ifdef JP
		hook_c_roff(TERM_SLATE, format("%^sは恐怖フィールドを張っている。", wd_he[msex]));
#else
		hook_c_roff(TERM_SLATE, format("%^s is surrounded by fear field.  ", wd_he[msex]));
#endif

	}
	if (flags7 & (RF7_SELF_LITE_1 | RF7_SELF_LITE_2))
	{
#ifdef JP
		hooked_roff(format("%^sは光っている。", wd_he[msex]));
#else
		hooked_roff(format("%^s illuminate the dungeon.  ", wd_he[msex]));
#endif

	}
	if (flags2 & RF2_INVISIBLE)
	{
#ifdef JP
		hooked_roff(format("%^sは透明で目に見えない。", wd_he[msex]));
#else
		hooked_roff(format("%^s is invisible.  ", wd_he[msex]));
#endif

	}
	if (flags2 & RF2_COLD_BLOOD)
	{
#ifdef JP
		hooked_roff(format("%^sは冷血動物である。", wd_he[msex]));
#else
		hooked_roff(format("%^s is cold blooded.  ", wd_he[msex]));
#endif

	}
	if (flags2 & RF2_EMPTY_MIND)
	{
#ifdef JP
		hooked_roff(format("%^sはテレパシーでは感知できない。", wd_he[msex]));
#else
		hooked_roff(format("%^s is not detected by telepathy.  ", wd_he[msex]));
#endif

	}
	else if (flags2 & RF2_WEIRD_MIND)
	{
#ifdef JP
		hooked_roff(format("%^sはまれにテレパシーで感知できる。", wd_he[msex]));
#else
		hooked_roff(format("%^s is rarely detected by telepathy.  ", wd_he[msex]));
#endif

	}
	if (flags2 & RF2_MULTIPLY)
	{
#ifdef JP
		hook_c_roff(TERM_L_UMBER, format("%^sは爆発的に増殖する。", wd_he[msex]));
#else
		hook_c_roff(TERM_L_UMBER, format("%^s breeds explosively.  ", wd_he[msex]));
#endif

	}
	if (flags2 & RF2_REGENERATE)
	{
#ifdef JP
		hook_c_roff(TERM_L_WHITE, format("%^sは素早く体力を回復する。", wd_he[msex]));
#else
		hook_c_roff(TERM_L_WHITE, format("%^s regenerates quickly.  ", wd_he[msex]));
#endif

	}
	if (flags7 & RF7_RIDING)
	{
#ifdef JP
		hook_c_roff(TERM_SLATE, format("%^sに乗ることができる。", wd_he[msex]));
#else
		hook_c_roff(TERM_SLATE, format("%^s is suitable for riding.  ", wd_he[msex]));
#endif

	}


	/* Collect susceptibilities */
	vn = 0;
#ifdef JP
	if (flags3 & RF3_HURT_ROCK) {vp[vn] = "岩を除去するもの";color[vn++] = TERM_UMBER;}
#else
	if (flags3 & RF3_HURT_ROCK) {vp[vn] = "rock remover";color[vn++] = TERM_UMBER;}
#endif

#ifdef JP
	if (flags3 & RF3_HURT_LITE) {vp[vn] = "明るい光";color[vn++] = TERM_YELLOW;}
#else
	if (flags3 & RF3_HURT_LITE) {vp[vn] = "bright light";color[vn++] = TERM_YELLOW;}
#endif

#ifdef JP
	if (flags3 & RF3_HURT_ACID) {vp[vn] = "酸";color[vn++] = TERM_GREEN;}
#else
	if (flags3 & RF3_HURT_ACID) {vp[vn] = "acid";color[vn++] = TERM_GREEN;}
#endif

#ifdef JP
	if (flags3 & RF3_HURT_ELEC) {vp[vn] = "稲妻";color[vn++] = TERM_BLUE;}
#else
	if (flags3 & RF3_HURT_ELEC) {vp[vn] = "lightning";color[vn++] = TERM_BLUE;}
#endif

#ifdef JP
	if (flags3 & RF3_HURT_FIRE) {vp[vn] = "炎";color[vn++] = TERM_RED;}
#else
	if (flags3 & RF3_HURT_FIRE) {vp[vn] = "fire";color[vn++] = TERM_RED;}
#endif

#ifdef JP
	if (flags3 & RF3_HURT_COLD) {vp[vn] = "冷気";color[vn++] = TERM_L_WHITE;}
#else
	if (flags3 & RF3_HURT_COLD) {vp[vn] = "cold";color[vn++] = TERM_L_WHITE;}
#endif


	/* Describe susceptibilities */
	if (vn)
	{
		/* Intro */
#ifdef JP
		hooked_roff(format("%^sには", wd_he[msex]));
#else
		hooked_roff(format("%^s", wd_he[msex]));
#endif


		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
#ifdef JP
			if ( n != 0 ) hooked_roff("や");
#else
			if (n == 0) hooked_roff(" is hurt by ");
			else if (n < vn-1) hooked_roff(", ");
			else hooked_roff(" and ");
#endif


			/* Dump */
			hook_c_roff(color[n], vp[n]);
		}

		/* End */
#ifdef JP
		hooked_roff("でダメージを与えられる。");
#else
		hooked_roff(".  ");
#endif

	}


	/* Collect resistances */
	vn = 0;
#ifdef JP
	if (flagsr & RFR_RES_ACID) {vp[vn] = "酸";color[vn++] = TERM_GREEN;}
#else
	if (flagsr & RFR_RES_ACID) {vp[vn] = "acid";color[vn++] = TERM_GREEN;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_ELEC) {vp[vn] = "稲妻";color[vn++] = TERM_BLUE;}
#else
	if (flagsr & RFR_RES_ELEC) {vp[vn] = "lightning";color[vn++] = TERM_BLUE;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_FIRE) {vp[vn] = "炎";color[vn++] = TERM_RED;}
#else
	if (flagsr & RFR_RES_FIRE) {vp[vn] = "fire";color[vn++] = TERM_RED;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_COLD) {vp[vn] = "冷気";color[vn++] = TERM_L_WHITE;}
#else
	if (flagsr & RFR_RES_COLD) {vp[vn] = "cold";color[vn++] = TERM_L_WHITE;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_POIS) {vp[vn] = "毒";color[vn++] = TERM_L_GREEN;}
#else
	if (flagsr & RFR_RES_POIS) {vp[vn] = "poison";color[vn++] = TERM_L_GREEN;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_LITE) {vp[vn] = "閃光";color[vn++] = TERM_YELLOW;}
#else
	if (flagsr & RFR_RES_LITE) {vp[vn] = "light";color[vn++] = TERM_YELLOW;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_DARK) {vp[vn] = "暗黒";color[vn++] = TERM_L_DARK;}
#else
	if (flagsr & RFR_RES_DARK) {vp[vn] = "dark";color[vn++] = TERM_L_DARK;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_NETH) {vp[vn] = "地獄";color[vn++] = TERM_L_DARK;}
#else
	if (flagsr & RFR_RES_NETH) {vp[vn] = "nether";color[vn++] = TERM_L_DARK;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_WATE) {vp[vn] = "水";color[vn++] = TERM_BLUE;}
#else
	if (flagsr & RFR_RES_WATE) {vp[vn] = "water";color[vn++] = TERM_BLUE;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_PLAS) {vp[vn] = "プラズマ";color[vn++] = TERM_L_RED;}
#else
	if (flagsr & RFR_RES_PLAS) {vp[vn] = "plasma";color[vn++] = TERM_L_RED;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_SHAR) {vp[vn] = "破片";color[vn++] = TERM_L_UMBER;}
#else
	if (flagsr & RFR_RES_SHAR) {vp[vn] = "shards";color[vn++] = TERM_L_UMBER;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_SOUN) {vp[vn] = "轟音";color[vn++] = TERM_ORANGE;}
#else
	if (flagsr & RFR_RES_SOUN) {vp[vn] = "sound";color[vn++] = TERM_ORANGE;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_CONF) {vp[vn] = "混乱";color[vn++] = TERM_L_UMBER;}
#else
	if (flagsr & RFR_RES_CONF) {vp[vn] = "conf";color[vn++] = TERM_L_UMBER;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_CHAO) {vp[vn] = "カオス";color[vn++] = TERM_VIOLET;}
#else
	if (flagsr & RFR_RES_CHAO) {vp[vn] = "chaos";color[vn++] = TERM_VIOLET;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_STON) {vp[vn] = "石化";color[vn++] = TERM_SLATE;}
#else
	if (flagsr & RFR_RES_STON) {vp[vn] = "stone";color[vn++] = TERM_SLATE;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_DISE) {vp[vn] = "劣化";color[vn++] = TERM_VIOLET;}
#else
	if (flagsr & RFR_RES_DISE) {vp[vn] = "disenchantment";color[vn++] = TERM_VIOLET;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_WALL) {vp[vn] = "フォース";color[vn++] = TERM_UMBER;}
#else
	if (flagsr & RFR_RES_WALL) {vp[vn] = "sound";color[vn++] = TERM_UMBER;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_INER) {vp[vn] = "遅鈍";color[vn++] = TERM_SLATE;}
#else
	if (flagsr & RFR_RES_INER) {vp[vn] = "inertia";color[vn++] = TERM_SLATE;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_TIME) {vp[vn] = "時間逆転";color[vn++] = TERM_L_BLUE;}
#else
	if (flagsr & RFR_RES_TIME) {vp[vn] = "time";color[vn++] = TERM_L_BLUE;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_GRAV) {vp[vn] = "重力";color[vn++] = TERM_SLATE;}
#else
	if (flagsr & RFR_RES_GRAV) {vp[vn] = "gravity";color[vn++] = TERM_SLATE;}
#endif

#ifdef JP
	if ((flags3 & RF3_RES_TELE) && !(r_ptr->flags1 & RF1_UNIQUE)) {vp[vn] = "テレポート";color[vn++] = TERM_ORANGE;}
#else
	if ((flags3 & RF3_RES_TELE) && !(r_ptr->flags1 & RF1_UNIQUE)) {vp[vn] = "teleportation";color[vn++] = TERM_ORANGE;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_BLUNT) {vp[vn] = "鈍器";color[vn++] = TERM_L_DARK;}
#else
	if (flagsr & RFR_RES_BLUNT) {vp[vn] = "blunt weapons";color[vn++] = TERM_L_DARK;}
#endif

#ifdef JP
	if (flagsr & RFR_RES_EDGED) {vp[vn] = "鋭器";color[vn++] = TERM_L_WHITE;}
#else
	if (flagsr & RFR_RES_EDGED) {vp[vn] = "edged weapons";color[vn++] = TERM_L_WHITE;}
#endif


	/* Describe resistances */
	if (vn)
	{
		/* Intro */
#ifdef JP
		hooked_roff(format("%^sは", wd_he[msex]));
#else
		hooked_roff(format("%^s", wd_he[msex]));
#endif


		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
#ifdef JP
			if ( n != 0 ) hooked_roff("と");
#else
			if (n == 0) hooked_roff(" resists ");
			else if (n < vn-1) hooked_roff(", ");
			else hooked_roff(" and ");
#endif


			/* Dump */
			hook_c_roff(color[n], vp[n]);
		}

		/* End */
#ifdef JP
		hooked_roff("の耐性を持っている。");
#else
		hooked_roff(".  ");
#endif

	}


	if ((r_ptr->r_xtra1 & MR1_SINKA) || know_everything)
	{
		if (r_ptr->next_r_idx)
		{
#ifdef JP
			hooked_roff(format("%^sは経験を積むと、", wd_he[msex]));
#else
			hooked_roff(format("%^s will evolve into ", wd_he[msex]));
#endif
			hook_c_roff(TERM_YELLOW, format("%s", r_name+r_info[r_ptr->next_r_idx].name));
#ifdef JP
			hooked_roff(format("に進化する。"));
#else
			hooked_roff(format(" when %s gets enugh experience.  ", wd_he[msex]));
#endif
		}
		else if (!(r_ptr->flags1 & RF1_UNIQUE))
		{
#ifdef JP
			hooked_roff(format("%sは進化しない。", wd_he[msex]));
#else
			hooked_roff(format("%s won't evolve.  ", wd_he[msex]));
#endif
		}
	}

	/* Collect non-effects */
	vn = 0;
#ifdef JP
	if (flags3 & RF3_NO_STUN)  {vp[vn] = "朦朧としない";color[vn++] = TERM_ORANGE;}
#else
	if (flags3 & RF3_NO_STUN)  {vp[vn] = "stunned";color[vn++] = TERM_ORANGE;}
#endif

#ifdef JP
	if (flags3 & RF3_NO_FEAR)  {vp[vn] = "恐怖を感じない";color[vn++] = TERM_SLATE;}
#else
	if (flags3 & RF3_NO_FEAR)  {vp[vn] = "frightened";color[vn++] = TERM_SLATE;}
#endif

#ifdef JP
	if (flags3 & RF3_NO_CONF)  {vp[vn] = "混乱しない";color[vn++] = TERM_L_UMBER;}
#else
	if (flags3 & RF3_NO_CONF)  {vp[vn] = "confused";color[vn++] = TERM_L_UMBER;}
#endif

#ifdef JP
	if (flags3 & RF3_NO_SLEEP) {vp[vn] = "眠らされない";color[vn++] = TERM_BLUE;}
#else
	if (flags3 & RF3_NO_SLEEP) {vp[vn] = "slept";color[vn++] = TERM_BLUE;}
#endif

#ifdef JP
	if (flags3 & RF3_NO_STONE) {vp[vn] = "石化しない";color[vn++] = TERM_SLATE;}
#else
	if (flags3 & RF3_NO_STONE) {vp[vn] = "stoned";color[vn++] = TERM_SLATE;}
#endif

#ifdef JP
	if ((flags3 & RF3_RES_TELE) && (r_ptr->flags1 & RF1_UNIQUE)) {vp[vn] = "テレポートされない";color[vn++] = TERM_ORANGE;}
#else
	if ((flags3 & RF3_RES_TELE) && (r_ptr->flags1 & RF1_UNIQUE)) {vp[vn] = "teleported";color[vn++] = TERM_ORANGE;}
#endif

#ifdef JP
	if (flagsr & RFR_IM_BLUNT) {vp[vn] = "鈍器でほとんど傷つかない";color[vn++] = TERM_L_DARK;}
#else
	if (flagsr & RFR_IM_BLUNT) {vp[vn] = "much harmed by blunt weapons";color[vn++] = TERM_L_DARK;}
#endif

#ifdef JP
	if (flagsr & RFR_IM_EDGED) {vp[vn] = "鋭器でほとんど傷つかない";color[vn++] = TERM_L_WHITE;}
#else
	if (flagsr & RFR_IM_EDGED) {vp[vn] = "much harmed by edged weapons";color[vn++] = TERM_L_WHITE;}
#endif

	/* Describe non-effects */
	if (vn)
	{
		/* Intro */
#ifdef JP
		hooked_roff(format("%^sは", wd_he[msex]));
#else
		hooked_roff(format("%^s", wd_he[msex]));
#endif


		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
#ifdef JP
			if ( n != 0 ) hooked_roff("し、");
#else
			if (n == 0) hooked_roff(" cannot be ");
			else if (n < vn - 1) hooked_roff(", ");
			else hooked_roff(" or ");
#endif


			/* Dump */
			hook_c_roff(color[n], vp[n]);
		}

		/* End */
#ifdef JP
		hooked_roff("。");
#else
		hooked_roff(".  ");
#endif

	}


	/* Do we know how aware it is? */
	if ((((int)r_ptr->r_wake * (int)r_ptr->r_wake) > r_ptr->sleep) ||
	          (r_ptr->r_ignore == MAX_UCHAR) ||
	    (r_ptr->sleep == 0 && r_ptr->r_tkills >= 10) || know_everything)
	{
		cptr act;

		if (r_ptr->sleep > 200)
		{
#ifdef JP
			act = "を無視しがちであるが";
#else
			act = "prefers to ignore";
#endif

		}
		else if (r_ptr->sleep > 95)
		{
#ifdef JP
			act = "に対してほとんど注意を払わないが";
#else
			act = "pays very little attention to";
#endif

		}
		else if (r_ptr->sleep > 75)
		{
#ifdef JP
			act = "に対してあまり注意を払わないが";
#else
			act = "pays little attention to";
#endif

		}
		else if (r_ptr->sleep > 45)
		{
#ifdef JP
			act = "を見過ごしがちであるが";
#else
			act = "tends to overlook";
#endif

		}
		else if (r_ptr->sleep > 25)
		{
#ifdef JP
			act = "をほんの少しは見ており";
#else
			act = "takes quite a while to see";
#endif

		}
		else if (r_ptr->sleep > 10)
		{
#ifdef JP
			act = "をしばらくは見ており";
#else
			act = "takes a while to see";
#endif

		}
		else if (r_ptr->sleep > 5)
		{
#ifdef JP
			act = "を幾分注意深く見ており";
#else
			act = "is fairly observant of";
#endif

		}
		else if (r_ptr->sleep > 3)
		{
#ifdef JP
			act = "を注意深く見ており";
#else
			act = "is observant of";
#endif

		}
		else if (r_ptr->sleep > 1)
		{
#ifdef JP
			act = "をかなり注意深く見ており";
#else
			act = "is very observant of";
#endif

		}
		else if (r_ptr->sleep > 0)
		{
#ifdef JP
			act = "を警戒しており";
#else
			act = "is vigilant for";
#endif

		}
		else
		{
#ifdef JP
			act = "をかなり警戒しており";
#else
			act = "is ever vigilant for";
#endif

		}

#ifdef JP
		hooked_roff(format("%^sは侵入者%s、 %d フィート先から侵入者に気付くことがある。",
		     wd_he[msex], act, 10 * r_ptr->aaf));
#else
		hooked_roff(format("%^s %s intruders, which %s may notice from %d feet.  ",
		            wd_he[msex], act, wd_he[msex], 10 * r_ptr->aaf));
#endif

	}


	/* Drops gold and/or items */
	if (drop_gold || drop_item)
	{
		/* Intro */
#ifdef JP
		hooked_roff(format("%^sは", wd_he[msex]));
#else
		hooked_roff(format("%^s may carry", wd_he[msex]));

		/* No "n" needed */
		sin = FALSE;
#endif


		/* Count maximum drop */
		n = MAX(drop_gold, drop_item);

		/* One drop (may need an "n") */
		if (n == 1)
		{
#ifdef JP
			hooked_roff("一つの");
#else
			hooked_roff(" a");

			sin = TRUE;
#endif
		}

		/* Two drops */
		else if (n == 2)
		{
#ifdef JP
			hooked_roff("一つか二つの");
#else
			hooked_roff(" one or two");
#endif

		}

		/* Many drops */
		else
		{
#ifdef JP
			hooked_roff(format(" %d 個までの", n));
#else
			hooked_roff(format(" up to %d", n));
#endif

		}


		/* Special (no "n" needed) */
		if (flags1 & RF1_DROP_SPECIAL)
		{
#ifdef JP
			p = "特別な";
#else
			p = " special";

			sin = FALSE;
#endif

		}

		/* Great */
		else if (flags1 & RF1_DROP_GREAT)
		{
#ifdef JP
			p = "高級な";
#else
			p = " excellent";
#endif

		}

		/* Good (no "n" needed) */
		else if (flags1 & RF1_DROP_GOOD)
		{
#ifdef JP
			p = "上質な";
#else
			p = " good";

			sin = FALSE;
#endif
		}

		/* Okay */
		else
		{
			p = NULL;
		}


		/* Objects */
		if (drop_item)
		{
#ifndef JP
			/* Handle singular "an" */
			if (sin) hooked_roff("n");
			sin = FALSE;
#endif

			/* Dump "object(s)" */
			if (p) hooked_roff(p);
#ifdef JP
			hooked_roff("アイテム");
#else
			hooked_roff(" object");
			if (n != 1) hooked_roff("s");
#endif


			/* Conjunction replaces variety, if needed for "gold" below */
#ifdef JP
			p = "や";
#else
			p = " or";
#endif

		}

		/* Treasures */
		if (drop_gold)
		{
#ifndef JP
			/* Cancel prefix */
			if (!p) sin = FALSE;

			/* Handle singular "an" */
			if (sin) hooked_roff("n");
			sin = FALSE;
#endif

			/* Dump "treasure(s)" */
			if (p) hooked_roff(p);
#ifdef JP
			hooked_roff("財宝");
#else
			hooked_roff(" treasure");
			if (n != 1) hooked_roff("s");
#endif

		}

		/* End this sentence */
#ifdef JP
		hooked_roff("を持っていることがある。");
#else
		hooked_roff(".  ");
#endif

	}


	/* Count the number of "known" attacks */
	for (n = 0, m = 0; m < 4; m++)
	{
		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;
		if (r_ptr->blow[m].method == RBM_SHOOT) continue;

		/* Count known attacks */
		if (r_ptr->r_blows[m] || know_everything) n++;
	}

	/* Examine (and count) the actual attacks */
	for (r = 0, m = 0; m < 4; m++)
	{
		int method, effect, d1, d2;

		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;
		if (r_ptr->blow[m].method == RBM_SHOOT) continue;

		/* Skip unknown attacks */
		if (!r_ptr->r_blows[m] && !know_everything) continue;

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
#ifdef JP
case RBM_HIT:		p = "殴る"; break;
#else
			case RBM_HIT:		p = "hit"; break;
#endif

#ifdef JP
case RBM_TOUCH:		p = "触る"; break;
#else
			case RBM_TOUCH:		p = "touch"; break;
#endif

#ifdef JP
case RBM_PUNCH:		p = "パンチする"; break;
#else
			case RBM_PUNCH:		p = "punch"; break;
#endif

#ifdef JP
case RBM_KICK:		p = "蹴る"; break;
#else
			case RBM_KICK:		p = "kick"; break;
#endif

#ifdef JP
case RBM_CLAW:		p = "ひっかく"; break;
#else
			case RBM_CLAW:		p = "claw"; break;
#endif

#ifdef JP
case RBM_BITE:		p = "噛む"; break;
#else
			case RBM_BITE:		p = "bite"; break;
#endif

#ifdef JP
case RBM_STING:		p = "刺す"; break;
#else
			case RBM_STING:		p = "sting"; break;
#endif

#ifdef JP
case RBM_SLASH:		p = "斬る"; break;
#else
			case RBM_SLASH:		p = "slash"; break;
#endif

#ifdef JP
case RBM_BUTT:		p = "角で突く"; break;
#else
			case RBM_BUTT:		p = "butt"; break;
#endif

#ifdef JP
case RBM_CRUSH:		p = "体当たりする"; break;
#else
			case RBM_CRUSH:		p = "crush"; break;
#endif

#ifdef JP
case RBM_ENGULF:	p = "飲み込む"; break;
#else
			case RBM_ENGULF:	p = "engulf"; break;
#endif

#ifdef JP
case RBM_CHARGE: 	p = "請求書をよこす"; break;
#else
			case RBM_CHARGE: 	p = "charge";   break;
#endif

#ifdef JP
case RBM_CRAWL:		p = "体の上を這い回る"; break;
#else
			case RBM_CRAWL:		p = "crawl on you"; break;
#endif

#ifdef JP
case RBM_DROOL:		p = "よだれをたらす"; break;
#else
			case RBM_DROOL:		p = "drool on you"; break;
#endif

#ifdef JP
case RBM_SPIT:		p = "つばを吐く"; break;
#else
			case RBM_SPIT:		p = "spit"; break;
#endif

#ifdef JP
case RBM_EXPLODE:	p = "爆発する"; break;
#else
			case RBM_EXPLODE:	p = "explode"; break;
#endif

#ifdef JP
case RBM_GAZE:		p = "にらむ"; break;
#else
			case RBM_GAZE:		p = "gaze"; break;
#endif

#ifdef JP
case RBM_WAIL:		p = "泣き叫ぶ"; break;
#else
			case RBM_WAIL:		p = "wail"; break;
#endif

#ifdef JP
case RBM_SPORE:		p = "胞子を飛ばす"; break;
#else
			case RBM_SPORE:		p = "release spores"; break;
#endif

			case RBM_XXX4:		break;
#ifdef JP
case RBM_BEG:		p = "金をせがむ"; break;
#else
			case RBM_BEG:		p = "beg"; break;
#endif

#ifdef JP
case RBM_INSULT:	p = "侮辱する"; break;
#else
			case RBM_INSULT:	p = "insult"; break;
#endif

#ifdef JP
case RBM_SING:  	p = "歌う"; break;
#else
			case RBM_SING:  	p = "sing"; break;
#endif

		}


		/* Default effect */
		q = NULL;

		/* Acquire the effect */
		switch (effect)
		{
#ifdef JP
			case RBE_SUPERHURT:
			case RBE_HURT:    	q = "攻撃する"; break;
#else
			case RBE_SUPERHURT:
			case RBE_HURT:    	q = "attack"; break;
#endif

#ifdef JP
			case RBE_POISON:  	q = "毒をくらわす"; break;
#else
			case RBE_POISON:  	q = "poison"; break;
#endif

#ifdef JP
			case RBE_UN_BONUS:	q = "劣化させる"; break;
#else
			case RBE_UN_BONUS:	q = "disenchant"; break;
#endif

#ifdef JP
			case RBE_UN_POWER:	q = "魔力を吸い取る"; break;
#else
			case RBE_UN_POWER:	q = "drain charges"; break;
#endif

#ifdef JP
			case RBE_EAT_GOLD:	q = "金を盗む"; break;
#else
			case RBE_EAT_GOLD:	q = "steal gold"; break;
#endif

#ifdef JP
			case RBE_EAT_ITEM:	q = "アイテムを盗む"; break;
#else
			case RBE_EAT_ITEM:	q = "steal items"; break;
#endif

#ifdef JP
			case RBE_EAT_FOOD:	q = "あなたの食料を食べる"; break;
#else
			case RBE_EAT_FOOD:	q = "eat your food"; break;
#endif

#ifdef JP
			case RBE_EAT_LITE:	q = "明かりを吸収する"; break;
#else
			case RBE_EAT_LITE:	q = "absorb light"; break;
#endif

#ifdef JP
			case RBE_ACID:    	q = "酸を飛ばす"; break;
#else
			case RBE_ACID:    	q = "shoot acid"; break;
#endif

#ifdef JP
			case RBE_ELEC:    	q = "感電させる"; break;
#else
			case RBE_ELEC:    	q = "electrocute"; break;
#endif

#ifdef JP
			case RBE_FIRE:    	q = "燃やす"; break;
#else
			case RBE_FIRE:    	q = "burn"; break;
#endif

#ifdef JP
			case RBE_COLD:    	q = "凍らせる"; break;
#else
			case RBE_COLD:    	q = "freeze"; break;
#endif

#ifdef JP
			case RBE_BLIND:   	q = "盲目にする"; break;
#else
			case RBE_BLIND:   	q = "blind"; break;
#endif

#ifdef JP
			case RBE_CONFUSE: 	q = "混乱させる"; break;
#else
			case RBE_CONFUSE: 	q = "confuse"; break;
#endif

#ifdef JP
			case RBE_TERRIFY: 	q = "恐怖させる"; break;
#else
			case RBE_TERRIFY: 	q = "terrify"; break;
#endif

#ifdef JP
			case RBE_PARALYZE:	q = "麻痺させる"; break;
#else
			case RBE_PARALYZE:	q = "paralyze"; break;
#endif

#ifdef JP
			case RBE_LOSE_STR:	q = "腕力を減少させる"; break;
#else
			case RBE_LOSE_STR:	q = "reduce strength"; break;
#endif

#ifdef JP
			case RBE_LOSE_INT:	q = "知能を減少させる"; break;
#else
			case RBE_LOSE_INT:	q = "reduce intelligence"; break;
#endif

#ifdef JP
			case RBE_LOSE_WIS:	q = "賢さを減少させる"; break;
#else
			case RBE_LOSE_WIS:	q = "reduce wisdom"; break;
#endif

#ifdef JP
			case RBE_LOSE_DEX:	q = "器用さを減少させる"; break;
#else
			case RBE_LOSE_DEX:	q = "reduce dexterity"; break;
#endif

#ifdef JP
			case RBE_LOSE_CON:	q = "耐久力を減少させる"; break;
#else
			case RBE_LOSE_CON:	q = "reduce constitution"; break;
#endif

#ifdef JP
			case RBE_LOSE_CHR:	q = "魅力を減少させる"; break;
#else
			case RBE_LOSE_CHR:	q = "reduce charisma"; break;
#endif

#ifdef JP
			case RBE_LOSE_ALL:	q = "全ステータスを減少させる"; break;
#else
			case RBE_LOSE_ALL:	q = "reduce all stats"; break;
#endif

#ifdef JP
			case RBE_SHATTER:	q = "粉砕する"; break;
#else
			case RBE_SHATTER:	q = "shatter"; break;
#endif

#ifdef JP
			case RBE_EXP_10:	q = "経験値を減少(10d6+)させる"; break;
#else
			case RBE_EXP_10:	q = "lower experience (by 10d6+)"; break;
#endif

#ifdef JP
			case RBE_EXP_20:	q = "経験値を減少(20d6+)させる"; break;
#else
			case RBE_EXP_20:	q = "lower experience (by 20d6+)"; break;
#endif

#ifdef JP
			case RBE_EXP_40:	q = "経験値を減少(40d6+)させる"; break;
#else
			case RBE_EXP_40:	q = "lower experience (by 40d6+)"; break;
#endif

#ifdef JP
			case RBE_EXP_80:	q = "経験値を減少(80d6+)させる"; break;
#else
			case RBE_EXP_80:	q = "lower experience (by 80d6+)"; break;
#endif

#ifdef JP
			case RBE_DISEASE:	q = "病気にする"; break;
#else
			case RBE_DISEASE:	q = "disease"; break;
#endif

#ifdef JP
			case RBE_TIME:      q = "時間を逆戻りさせる"; break;
#else
			case RBE_TIME:      q = "time"; break;
#endif

#ifdef JP
			case RBE_EXP_VAMP:  q = "生命力を吸収する"; break;
#else
			case RBE_EXP_VAMP:  q = "drain life force"; break;
#endif

#ifdef JP
			case RBE_DR_MANA:  q = "魔力を奪う"; break;
#else
			case RBE_DR_MANA:  q = "drain mana force"; break;
#endif

#ifdef JP
			case RBE_STONE:    q = "石化させる"; break;
#else
			case RBE_STONE:    q = "stone"; break;
#endif

#ifdef JP
			case RBE_HOLY:     q = "聖なる力で攻撃する"; break;
#else
			case RBE_HOLY:     q = "holy attack"; break;
#endif

#ifdef JP
			case RBE_HELL:     q = "邪悪な力で攻撃する"; break;
#else
			case RBE_HELL:     q = "hell attack"; break;
#endif

		}


#ifdef JP
		if ( r == 0 ) hooked_roff( format("%^sは", wd_he[msex]) );

		/***若干表現を変更 ita ***/

		/* Describe damage (if known) */
		if (d1 && d2 && (know_everything || know_damage(r_idx, m)))
		{
			/* Display the damage */
			hooked_roff(format(" %dd%d ", d1, d2));
			hooked_roff("のダメージで");
		}
		/* Hack -- force a method */
		if (!p) p = "何か奇妙なことをする";

		/* Describe the method */
		/* XXしてYYし/XXしてYYする/XXし/XXする */
		if(q) jverb( p ,jverb_buf, JVERB_TO);
		else if(r!=n-1) jverb( p ,jverb_buf, JVERB_AND);
		else strcpy(jverb_buf, p);

		hooked_roff(jverb_buf);

		/* Describe the effect (if any) */
		if (q)
		{
		  if(r!=n-1) jverb( q,jverb_buf, JVERB_AND);
		  else strcpy(jverb_buf,q); 
		  hooked_roff(jverb_buf);
		}
		if(r!=n-1) hooked_roff("、");
#else
		/* Introduce the attack description */
		if (!r)
		{
			hooked_roff(format("%^s can ", wd_he[msex]));
		}
		else if (r < n-1)
		{
			hooked_roff(", ");
		}
		else
		{
			hooked_roff(", and ");
		}


		/* Hack -- force a method */
		if (!p) p = "do something weird";

		/* Describe the method */
		hooked_roff(p);


		/* Describe the effect (if any) */
		if (q)
		{
			/* Describe the attack type */
			hooked_roff(" to ");
			hooked_roff(q);

			/* Describe damage (if known) */
			if (d1 && d2 && (know_everything || know_damage(r_idx, m)))
			{
				/* Display the damage */
				hooked_roff(" with damage");
				hooked_roff(format(" %dd%d", d1, d2));
			}
		}
#endif



		/* Count the attacks as printed */
		r++;
	}

	/* Finish sentence above */
	if (r)
	{
#ifdef JP
		hooked_roff("。");
#else
		hooked_roff(".  ");
#endif

	}

	/* Notice lack of attacks */
	else if (flags1 & RF1_NEVER_BLOW)
	{
#ifdef JP
		hooked_roff(format("%^sは物理的な攻撃方法を持たない。", wd_he[msex]));
#else
		hooked_roff(format("%^s has no physical attacks.  ", wd_he[msex]));
#endif

	}

	/* Or describe the lack of knowledge */
	else
	{
#ifdef JP
		hooked_roff(format("%s攻撃については何も知らない。", wd_his[msex]));
#else
		hooked_roff(format("Nothing is known about %s attack.  ", wd_his[msex]));
#endif

	}


	if ((flags1 & RF1_QUESTOR) && r_ptr->r_sights && r_ptr->max_num)
	{
		switch (r_idx)
		{
		case MON_LANCELOT:
			if (quest[QUEST_LANCELOT].status == QUEST_STATUS_TAKEN) is_questor = TRUE;
			break;

		case MON_DENIM:
			if (quest[QUEST_DENIM].status == QUEST_STATUS_TAKEN) is_questor = TRUE;
			break;

		case MON_DOLGARUA:
			if (quest[QUEST_DOLGARUA].status == QUEST_STATUS_TAKEN) is_questor = TRUE;
			break;

		case MON_FELLANA:
			if (quest[QUEST_FELLANA].status == QUEST_STATUS_TAKEN) is_questor = TRUE;
			break;

		case MON_HOLP:
			if (quest[QUEST_HOLP].status == QUEST_STATUS_TAKEN) is_questor = TRUE;
			break;

		case MON_ISHTALLE:
			if (quest[QUEST_ISHTALLE].status == QUEST_STATUS_TAKEN) is_questor = TRUE;
			break;

		case MON_FILARHH:
			if (quest[QUEST_FILARHH].status == QUEST_STATUS_TAKEN) is_questor = TRUE;
			break;

		default:
			if (r_idx == runeweapon_r_idx_from(1))
			{
				if (quest[QUEST_RUNEWEAPON].status == QUEST_STATUS_TAKEN) is_questor = TRUE;
			}
			break;
		}
	}

	/*
	 * Notice "Quest" monsters, but only if you
	 * already encountered the monster.
	 */
	if (is_questor)
	{
#ifdef JP
		hook_c_roff(TERM_VIOLET, "あなたはこのモンスターを殺したいという強い欲望を感じている...");
#else
		hook_c_roff(TERM_VIOLET, "You feel an intense desire to kill this monster...  ");
#endif

	}

	else if (flags7 & RF7_GUARDIAN)
	{
#ifdef JP
		hook_c_roff(TERM_L_RED, "このモンスターはダンジョンの主である。");
#else
		hook_c_roff(TERM_L_RED, "This monster is the master of a dungeon.");
#endif

	}


	/* All done */
	hooked_roff("\n");

}



/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
void roff_top(int r_idx)
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


	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* A title (use "The" for non-uniques) */
#ifndef JP
	if (!(r_ptr->flags1 & RF1_UNIQUE))
	{
		Term_addstr(-1, TERM_WHITE, "The ");
	}
#endif

	/* Dump the name */
	Term_addstr(-1, TERM_WHITE, (r_name + r_ptr->name));

	/* Append the "standard" attr/char info */
	Term_addstr(-1, TERM_WHITE, " ('");
	Term_addch(a1, c1);
	if (use_bigtile && (a1 & 0x80)) Term_addch(255, -1);
	Term_addstr(-1, TERM_WHITE, "')");

	/* Append the "optional" attr/char info */
	Term_addstr(-1, TERM_WHITE, "/('");
	Term_addch(a2, c2);
	if (use_bigtile && (a2 & 0x80)) Term_addch(255, -1);
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
void screen_roff(int r_idx, int mode)
{
	/* Flush messages */
	msg_print(NULL);

	/* Begin recall */
	Term_erase(0, 1, 255);

	hook_c_roff = c_roff;

	/* Recall monster */
	roff_aux(r_idx, mode);

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

	hook_c_roff = c_roff;

	/* Recall monster */
	roff_aux(r_idx, 0);

	/* Describe monster */
	roff_top(r_idx);
}



/*
 * Hack -- output description of the given monster race
 */
void output_monster_spoiler(int r_idx, void (*roff_func)(byte attr, cptr str))
{
	hook_c_roff = roff_func;

	/* Recall monster */
	roff_aux(r_idx, 0x03);
}


bool mon_hook_dungeon(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (!(r_ptr->flags8 & RF8_WILD_ONLY))
		return TRUE;
	else
	{
		if (r_ptr->flags8 & RF8_WILD_MOUNTAIN)
		{
			dungeon_info_type *d_ptr = &d_info[dungeon_type];
			if (d_ptr->mflags8 & RF8_WILD_MOUNTAIN) return TRUE;
			if (!dun_level && p_ptr->town_num) return TRUE;
		}
		return FALSE;
	}
}


static bool mon_hook_ocean(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & RF8_WILD_OCEAN)
		return TRUE;
	else
		return FALSE;
}


static bool mon_hook_shore(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & RF8_WILD_SHORE)
		return TRUE;
	else
		return FALSE;
}


static bool mon_hook_waste(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & (RF8_WILD_WASTE | RF8_WILD_ALL))
		return TRUE;
	else
		return FALSE;
}


static bool mon_hook_town(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & (RF8_WILD_TOWN | RF8_WILD_ALL))
		return TRUE;
	else
		return FALSE;
}


static bool mon_hook_wood(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & (RF8_WILD_WOOD | RF8_WILD_ALL))
		return TRUE;
	else
		return FALSE;
}


static bool mon_hook_volcano(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & RF8_WILD_VOLCANO)
		return TRUE;
	else
		return FALSE;
}


static bool mon_hook_mountain(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & RF8_WILD_MOUNTAIN)
		return TRUE;
	else
		return FALSE;
}


static bool mon_hook_grass(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (r_ptr->flags8 & (RF8_WILD_GRASS | RF8_WILD_ALL))
		return TRUE;
	else
		return FALSE;
}


static bool mon_hook_tundra(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (((r_ptr->flagsr & RFR_RES_COLD) ||
	     (r_ptr->flags7 & RF7_CAN_FLY) ||
	     (r_ptr->flags3 & RF3_DRAGON) ||
	     (r_ptr->d_char == 'D') ||
	     (r_ptr->d_char == 'd')) &&
	    !(r_ptr->flags2 & RF2_AURA_FIRE) &&
		!(r_ptr->flags4 & RF4_BR_FIRE))
		return TRUE;
	else
		return FALSE;
}


static bool mon_hook_deep_sea(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (!mon_hook_dungeon(r_idx)) return FALSE;

	if (((r_ptr->flags7 & RF7_AQUATIC) || (r_ptr->flags7 & RF7_CAN_FLY)) &&
		(r_ptr->level >= 40))
		return TRUE;
	else
		return FALSE;
}


static bool mon_hook_deep_water(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (!mon_hook_dungeon(r_idx)) return FALSE;

	if ((r_ptr->flags7 & RF7_AQUATIC) || (r_ptr->flags7 & RF7_CAN_FLY))
		return TRUE;
	else
		return FALSE;
}


static bool mon_hook_shallow_water(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (!mon_hook_dungeon(r_idx)) return FALSE;

	if (r_ptr->flags2 & RF2_AURA_FIRE)
		return FALSE;
	else
		return TRUE;
}


static bool mon_hook_lava(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (!mon_hook_dungeon(r_idx)) return FALSE;

	if (((r_ptr->flagsr & RFR_RES_FIRE) ||
	     (r_ptr->flags7 & RF7_CAN_FLY)) &&
	    !(r_ptr->flags3 & RF3_AURA_COLD))
		return TRUE;
	else
		return FALSE;
}


static bool mon_hook_flying(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (!mon_hook_dungeon(r_idx)) return FALSE;

	if (r_ptr->flags7 & RF7_CAN_FLY)
		return TRUE;
	else
		return FALSE;
}


static bool mon_hook_floor(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (!(r_ptr->flags7 & RF7_AQUATIC) ||
	    (r_ptr->flags7 & RF7_CAN_FLY))
		return TRUE;
	else
		return FALSE;
}


monster_hook_type get_monster_hook(void)
{
	if (!dun_level && !p_ptr->inside_quest)
	{
		switch (wilderness[p_ptr->wilderness_y][p_ptr->wilderness_x].terrain)
		{
		case TERRAIN_TOWN:
			return (monster_hook_type)mon_hook_town;
		case TERRAIN_DEEP_WATER:
			return (monster_hook_type)mon_hook_ocean;
		case TERRAIN_SHALLOW_WATER:
		case TERRAIN_SWAMP:
			return (monster_hook_type)mon_hook_shore;
		case TERRAIN_DIRT:
		case TERRAIN_DESERT:
			return (monster_hook_type)mon_hook_waste;
		case TERRAIN_GRASS:
			return (monster_hook_type)mon_hook_grass;
		case TERRAIN_TREES:
			return (monster_hook_type)mon_hook_wood;
		case TERRAIN_SHALLOW_LAVA:
		case TERRAIN_DEEP_LAVA:
			return (monster_hook_type)mon_hook_volcano;
		case TERRAIN_MOUNTAIN:
			return (monster_hook_type)mon_hook_mountain;
		case TERRAIN_TUNDRA:
			return (monster_hook_type)mon_hook_tundra;
		case TERRAIN_DEEP_SEA:
			return (monster_hook_type)mon_hook_deep_sea;
		default:
			return (monster_hook_type)mon_hook_dungeon;
		}
	}
	else
	{
		return (monster_hook_type)mon_hook_dungeon;
	}
}


monster_hook_type get_monster_hook2(int y, int x)
{
	/* Set the monster list */
	switch (cave[y][x].feat)
	{
	case FEAT_SHAL_WATER:
	case FEAT_SWAMP:
		return (monster_hook_type)mon_hook_shallow_water;
	case FEAT_DEEP_WATER:
		return (monster_hook_type)mon_hook_deep_water;
	case FEAT_DEEP_LAVA:
	case FEAT_SHAL_LAVA:
		return (monster_hook_type)mon_hook_lava;
	case FEAT_DARK_PIT:
	case FEAT_AIR:
		return (monster_hook_type)mon_hook_flying;
	default:
		return NULL;
	}
}


void set_friendly(monster_type *m_ptr)
{
	m_ptr->smart1 |= SM1_FRIENDLY;
}

void set_pet(monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Check for quest completion */
	check_quest_completion(m_ptr);

	m_ptr->smart1 |= SM1_PET;
	if (!(r_ptr->flags3 & (RF3_EVIL | RF3_GOOD)))
		m_ptr->sub_align &= ~(SUB_ALIGN_GNE_MASK);
	if (!(r_ptr->flags7 & (RF7_LAWFUL | RF7_CHAOTIC)))
		m_ptr->sub_align &= ~(SUB_ALIGN_LNC_MASK);
	if (!(r_ptr->flags3 & RF3_TEMPLE) && !(r_ptr->flags7 & RF7_ZENOBIAN_FORCES))
		m_ptr->sub_align &= ~(SUB_ALIGN_CLASS_MASK);
}

/*
 * Makes the monster hostile towards the player
 */
void set_hostile(monster_type *m_ptr)
{
	m_ptr->smart1 &= ~SM1_PET;
	m_ptr->smart1 &= ~SM1_FRIENDLY;
}


/*
 * Anger the monster
 */
void anger_monster(monster_type *m_ptr)
{
	if (is_friendly(m_ptr))
	{
		char m_name[80];

		monster_desc(m_name, m_ptr, 0);
#ifdef JP
msg_format("%^sは怒った！", m_name);
#else
		msg_format("%^s gets angry!", m_name);
#endif

		set_hostile(m_ptr);
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
	else if ((feat == FEAT_SHAL_WATER) || (feat == FEAT_SWAMP))
	{
		if (!(r_ptr->flags2 & RF2_AURA_FIRE) ||
		    (r_ptr->flags7 & RF7_AQUATIC) ||
		    (r_ptr->flags7 & RF7_CAN_FLY) ||
		    (r_ptr->flags7 & RF7_CAN_SWIM))
			return TRUE;
		else
			return FALSE;
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
		if ((r_ptr->flagsr & RFR_RES_FIRE) ||
		    (r_ptr->flags7 & RF7_CAN_FLY))
			return TRUE;
		else
			return FALSE;
	}
	/* Pit & Air */
	else if ((feat == FEAT_DARK_PIT) || (feat == FEAT_AIR))
	{
		if (r_ptr->flags7 & RF7_CAN_FLY)
			return TRUE;
		else
			return FALSE;
	}

	return TRUE;
}


/*
 * Check if this monster has "hostile" alignment (aux)
 */
static bool monster_has_hostile_alignment_aux(byte sub_align1, byte sub_align2)
{
	byte m_sub_align, n_sub_align;

	/* First, check classification */
	m_sub_align = sub_align1 & SUB_ALIGN_CLASS_MASK;
	n_sub_align = sub_align2 & SUB_ALIGN_CLASS_MASK;
	if (m_sub_align != n_sub_align)
	{
		if (((m_sub_align & SUB_ALIGN_TEMPLE) && (n_sub_align & SUB_ALIGN_WHITE)) ||
			((m_sub_align & SUB_ALIGN_WHITE) && (n_sub_align & SUB_ALIGN_TEMPLE)))
			return TRUE;
	}

	/* Next, check GNE alignment */
	m_sub_align = sub_align1 & SUB_ALIGN_GNE_MASK;
	n_sub_align = sub_align2 & SUB_ALIGN_GNE_MASK;
	if (m_sub_align != n_sub_align)
	{
		if (((m_sub_align & SUB_ALIGN_EVIL) && (n_sub_align & SUB_ALIGN_GOOD)) ||
			((m_sub_align & SUB_ALIGN_GOOD) && (n_sub_align & SUB_ALIGN_EVIL)))
			return TRUE;

		/* At last, check LNC alignment */
		m_sub_align = sub_align1 & SUB_ALIGN_LNC_MASK;
		n_sub_align = sub_align2 & SUB_ALIGN_LNC_MASK;
		if (m_sub_align != n_sub_align)
		{
			if (((m_sub_align & SUB_ALIGN_LAWFUL) && (n_sub_align & SUB_ALIGN_CHAOTIC)) ||
				((m_sub_align & SUB_ALIGN_CHAOTIC) && (n_sub_align & SUB_ALIGN_LAWFUL)))
				return TRUE;
		}
	}

	/* Non-hostile alignment */
	return FALSE;
}

/*
 * Strictly check if monster can enter the grid
 */
bool monster_can_enter(int y, int x, monster_race *r_ptr)
{
	cave_type *c_ptr = &cave[y][x];
	byte feat = c_ptr->feat;

	/* Player or other monster */
	if ((y == py) && (x == px)) return FALSE;
	if (c_ptr->m_idx) return FALSE;

	/* Permanent wall */
	if ((c_ptr->feat >= FEAT_PERM_EXTRA) &&
	    (c_ptr->feat <= FEAT_PERM_SOLID))
		return FALSE;

	/* Can fly over mountain on the surface */
	if (feat == FEAT_MOUNTAIN)
	{
		if (!dun_level && 
		    ((r_ptr->flags7 & RF7_CAN_FLY) ||
		     (r_ptr->flags8 & RF8_WILD_MOUNTAIN)))
			return TRUE;
		else
			return FALSE;
	}

	/* Cannot enter wall without pass wall ability */
	if (!cave_floor_grid(c_ptr) && !(r_ptr->flags2 & RF2_PASS_WALL))
		return FALSE;

	/* Pit & Air */
	if ((feat == FEAT_DARK_PIT) || (feat == FEAT_AIR))
	{
		if (r_ptr->flags7 & RF7_CAN_FLY)
			return TRUE;
		else
			return FALSE;
	}
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
	else if ((feat == FEAT_SHAL_WATER) || (feat == FEAT_SWAMP))
	{
		if (!(r_ptr->flags2 & RF2_AURA_FIRE) ||
		    (r_ptr->flags7 & RF7_AQUATIC) ||
		    (r_ptr->flags7 & RF7_CAN_FLY) ||
		    (r_ptr->flags7 & RF7_CAN_SWIM))
			return TRUE;
		else
			return FALSE;
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
		if ((r_ptr->flagsr & RFR_RES_FIRE) ||
		    (r_ptr->flags7 & RF7_CAN_FLY))
			return TRUE;
		else
			return FALSE;
	}

	return TRUE;
}



/*
 * Check if two monsters are enemies
 */
bool are_enemies(monster_type *m_ptr, monster_type *n_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_race *s_ptr = &r_info[n_ptr->r_idx];
	byte m_sub_align, n_sub_align;

	if ((r_ptr->flags8 & (RF8_WILD_TOWN | RF8_WILD_ALL))
	    && (s_ptr->flags8 & (RF8_WILD_TOWN | RF8_WILD_ALL)))
	{
		if (!is_pet(m_ptr) && !is_pet(n_ptr)) return FALSE;
	}

	if (monster_has_hostile_alignment_aux(m_ptr->sub_align, n_ptr->sub_align))
		return TRUE;

	/* Hostile vs. non-hostile */
	if (is_hostile(m_ptr) != is_hostile(n_ptr))
	{
		return TRUE;
	}

	/* Default */
	return FALSE;
}


/*
 * Check if this monster has "hostile" alignment
 * If user is player, m_ptr == NULL.
 */
bool monster_has_hostile_alignment(monster_type *m_ptr, monster_race *r_ptr)
{
	byte sub_align1 = SUB_ALIGN_NEUTRAL;
	byte sub_align2 = SUB_ALIGN_NEUTRAL;

	if (m_ptr) /* For a monster */
	{
		sub_align1 = m_ptr->sub_align;
	}
	else /* For player */
	{
		switch (get_your_alignment_gne())
		{
		case ALIGN_GNE_GOOD:
			sub_align1 |= (SUB_ALIGN_GOOD);
			break;
		case ALIGN_GNE_EVIL:
			sub_align1 |= (SUB_ALIGN_EVIL);
			break;
		}
		switch (get_your_alignment_lnc())
		{
		case ALIGN_LNC_LAWFUL:
			sub_align1 |= (SUB_ALIGN_LAWFUL);
			break;
		case ALIGN_LNC_CHAOTIC:
			sub_align1 |= (SUB_ALIGN_CHAOTIC);
			break;
		}
		switch (p_ptr->pclass)
		{
		case CLASS_TEMPLEKNIGHT:
			sub_align1 |= (SUB_ALIGN_TEMPLE);
			break;
		case CLASS_WHITEKNIGHT:
			sub_align1 |= (SUB_ALIGN_WHITE);
			break;
		}
	}

	if (r_ptr->flags3 & RF3_EVIL) sub_align2 |= SUB_ALIGN_EVIL;
	if (r_ptr->flags3 & RF3_GOOD) sub_align2 |= SUB_ALIGN_GOOD;
	if (r_ptr->flags7 & RF7_LAWFUL) sub_align2 |= SUB_ALIGN_LAWFUL;
	if (r_ptr->flags7 & RF7_CHAOTIC) sub_align2 |= SUB_ALIGN_CHAOTIC;
	if (r_ptr->flags3 & RF3_TEMPLE) sub_align2 |= SUB_ALIGN_TEMPLE;
	if (r_ptr->flags7 & RF7_ZENOBIAN_FORCES) sub_align2 |= SUB_ALIGN_WHITE;

	if (monster_has_hostile_alignment_aux(sub_align1, sub_align2))
		return TRUE;

	/* Non-hostile alignment */
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
