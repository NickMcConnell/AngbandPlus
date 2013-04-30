/* File: monster1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-3 Andrew Doull. Modifications to the Angband 2.9.6
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
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
static bool know_armour(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	s32b level = r_ptr->level;

	s32b kills = l_ptr->tkills;

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
static bool know_damage(int r_idx, const monster_lore *l_ptr, int i)
{
	const monster_race *r_ptr = &r_info[r_idx];

	s32b level = r_ptr->level;

	s32b a = l_ptr->blows[i];

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


static void describe_monster_desc(int r_idx)
{
	const monster_race *r_ptr = &r_info[r_idx];
	char buf[2048];

#ifdef DELAY_LOAD_R_TEXT

	int fd;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "monster.raw");

	/* Open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Use file */
	if (fd >= 0)
	{
		long pos;

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
		for (i = r_idx+1; i < z_info->r_max; i++)
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
		fd_read(fd, buf, sizeof(buf));

		/* Close it */
		fd_close(fd);
	}

#else

	/* Simple method */
	strcpy(buf, r_text + r_ptr->text);

#endif

	/* Dump it */
	text_out(buf);

	if (strlen(buf)) text_out("  ");
}


static void describe_monster_spells(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];
	int m, n;
	int msex = 0;
	bool breath = FALSE;
	bool magic = FALSE;
	int vn;
	cptr vp[64];


	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	else if (r_ptr->flags1 & RF1_MALE) msex = 1;

	/* Collect innate attacks */
	vn = 0;
	if (l_ptr->flags4 & (RF4_SHRIEK))      vp[vn++] = "shriek for help";
	if (l_ptr->flags4 & (RF4_SPORE))vp[vn++] = "release spores";
	if (l_ptr->flags4 & (RF4_GAZE))vp[vn++] = "gaze at you";
	if (l_ptr->flags4 & (RF4_WAIL))vp[vn++] = "wail at you";
	if (l_ptr->flags4 & (RF4_SPIT))     vp[vn++] = "spit on you";
	if (l_ptr->flags4 & (RF4_LASH))     vp[vn++] = "lash at you";
	if (l_ptr->flags4 & (RF4_SHOOT))     vp[vn++] = "shoot at you";
	if (l_ptr->flags4 & (RF4_EXPLODE))     vp[vn++] = "explode";
	if (l_ptr->flags4 & (RF4_AURA)) vp[vn++] = "radiate a powerful aura";
	if (l_ptr->flags4 & (RF4_THROW)) vp[vn++] = "throw things at you";


	/* Describe innate attacks */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" may ");
			else if (n < vn-1) text_out(", ");
			else text_out(" or ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
	}


	/* Collect breaths */
	vn = 0;
	if (l_ptr->flags4 & RF4_BR_ACID)		vp[vn++] = "acid";
	if (l_ptr->flags4 & RF4_BR_ELEC)		vp[vn++] = "lightning";
	if (l_ptr->flags4 & RF4_BR_FIRE)		vp[vn++] = "fire";
	if (l_ptr->flags4 & RF4_BR_COLD)		vp[vn++] = "frost";
	if (l_ptr->flags4 & RF4_BR_POIS)		vp[vn++] = "poison";
	if (l_ptr->flags4 & RF4_BR_NETH)		vp[vn++] = "nether";
	if (l_ptr->flags4 & RF4_BR_LITE)		vp[vn++] = "light";
	if (l_ptr->flags4 & RF4_BR_DARK)		vp[vn++] = "darkness";
	if (l_ptr->flags4 & RF4_BR_CONF)		vp[vn++] = "confusion";
	if (l_ptr->flags4 & RF4_BR_SOUN)		vp[vn++] = "sound";
	if (l_ptr->flags4 & RF4_BR_CHAO)		vp[vn++] = "chaos";
	if (l_ptr->flags4 & RF4_BR_DISE)		vp[vn++] = "disenchantment";
	if (l_ptr->flags4 & RF4_BR_NEXU)		vp[vn++] = "nexus";
	if (l_ptr->flags4 & RF4_BR_TIME)		vp[vn++] = "time";
	if (l_ptr->flags4 & RF4_BR_INER)		vp[vn++] = "inertia";
	if (l_ptr->flags4 & RF4_BR_GRAV)		vp[vn++] = "gravity";
	if (l_ptr->flags4 & RF4_BR_SHAR)		vp[vn++] = "shards";
	if (l_ptr->flags4 & RF4_BR_PLAS)		vp[vn++] = "plasma";
	if (l_ptr->flags4 & RF4_BR_WALL)		vp[vn++] = "force";
	if (l_ptr->flags4 & RF4_BR_MANA)		vp[vn++] = "mana";
	if (l_ptr->flags4 & RF4_BR_FEAR)		vp[vn++] = "fear";
	if (l_ptr->flags4 & RF4_XXX6)		vp[vn++] = "something";

	/* Describe breaths */
	if (vn)
	{
		/* Note breath */
		breath = TRUE;

		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" may breathe ");
			else if (n < vn-1) text_out(", ");
			else text_out(" or ");

			/* Dump */
			text_out(vp[n]);
		}
	}


	/* Collect spells */
	vn = 0;
	if (l_ptr->flags5 & RF5_BA_ACID)     vp[vn++] = "produce acid balls";
	if (l_ptr->flags5 & RF5_BA_ELEC)     vp[vn++] = "produce lightning balls";
	if (l_ptr->flags5 & RF5_BA_FIRE)     vp[vn++] = "produce fire balls";
	if (l_ptr->flags5 & RF5_BA_COLD)     vp[vn++] = "produce frost balls";
	if (l_ptr->flags5 & RF5_BA_POIS)     vp[vn++] = "produce poison balls";
	if (l_ptr->flags5 & RF5_BA_NETH)     vp[vn++] = "produce nether balls";
	if (l_ptr->flags5 & RF5_BA_WATE)     vp[vn++] = "produce water balls";
	if (l_ptr->flags5 & RF5_BA_MANA)     vp[vn++] = "invoke mana storms";
	if (l_ptr->flags5 & RF5_BA_DARK)     vp[vn++] = "invoke darkness storms";
	if (l_ptr->flags5 & RF5_DRAIN_MANA)  vp[vn++] = "drain mana";
	if (l_ptr->flags5 & RF5_MIND_BLAST)  vp[vn++] = "cause mind blasting";
	if (l_ptr->flags5 & RF5_BRAIN_SMASH) vp[vn++] = "cause brain smashing";
	if (l_ptr->flags5 & RF5_WOUND)       vp[vn++] = "cause wounds";
	if (l_ptr->flags5 & RF5_HUNGER)      vp[vn++] = "cause hunger";
	if (l_ptr->flags5 & RF5_XXX3)        vp[vn++] = "do something";
	if (l_ptr->flags5 & RF5_XXX4)        vp[vn++] = "do something";
	if (l_ptr->flags5 & RF5_BO_ACID)     vp[vn++] = "produce acid bolts";
	if (l_ptr->flags5 & RF5_BO_ELEC)     vp[vn++] = "produce lightning bolts";
	if (l_ptr->flags5 & RF5_BO_FIRE)     vp[vn++] = "produce fire bolts";
	if (l_ptr->flags5 & RF5_BO_COLD)     vp[vn++] = "produce frost bolts";
	if (l_ptr->flags5 & RF5_BO_POIS)     vp[vn++] = "produce poison bolts";
	if (l_ptr->flags5 & RF5_BO_NETH)     vp[vn++] = "produce nether bolts";
	if (l_ptr->flags5 & RF5_BO_WATE)     vp[vn++] = "produce water bolts";
	if (l_ptr->flags5 & RF5_BO_MANA)     vp[vn++] = "produce mana bolts";
	if (l_ptr->flags5 & RF5_BO_PLAS)     vp[vn++] = "produce plasma bolts";
	if (l_ptr->flags5 & RF5_BO_ICEE)     vp[vn++] = "produce ice bolts";
	if (l_ptr->flags5 & RF5_MISSILE)     vp[vn++] = "produce magic missiles";
	if (l_ptr->flags5 & RF5_SCARE)       vp[vn++] = "terrify";
	if (l_ptr->flags5 & RF5_BLIND)       vp[vn++] = "blind";
	if (l_ptr->flags5 & RF5_CONF)vp[vn++] = "confuse";
	if (l_ptr->flags5 & RF5_SLOW)vp[vn++] = "slow";
	if (l_ptr->flags5 & RF5_HOLD)vp[vn++] = "paralyze";
	if (l_ptr->flags6 & RF6_HASTE)       vp[vn++] = "haste-self";
	if (l_ptr->flags6 & RF6_ADD_MANA)vp[vn++] = "recover mana";
	if (l_ptr->flags6 & RF6_HEAL)vp[vn++] = "heal-self";
	if (l_ptr->flags6 & RF6_CURE)vp[vn++] = "cure-self";
	if (l_ptr->flags6 & RF6_BLINK)       vp[vn++] = "blink-self";
	if (l_ptr->flags6 & RF6_TPORT)       vp[vn++] = "teleport-self";
	if (l_ptr->flags6 & RF6_XXX3)vp[vn++] = "do something";
	if (l_ptr->flags6 & RF6_TELE_SELF_TO)vp[vn++] = "teleport-self to";
	if (l_ptr->flags6 & RF6_TELE_TO)     vp[vn++] = "teleport to";
	if (l_ptr->flags6 & RF6_TELE_AWAY)   vp[vn++] = "teleport away";
	if (l_ptr->flags6 & RF6_TELE_LEVEL)  vp[vn++] = "teleport level";
	if (l_ptr->flags6 & RF6_XXX5)vp[vn++] = "do something";
	if (l_ptr->flags6 & RF6_DARKNESS)    vp[vn++] = "create darkness";
	if (l_ptr->flags6 & RF6_TRAPS)       vp[vn++] = "create traps";
	if (l_ptr->flags6 & RF6_FORGET)      vp[vn++] = "cause amnesia";
	if (l_ptr->flags6 & RF6_XXX6)vp[vn++] = "do something";
	if (l_ptr->flags6 & RF6_S_KIN)       vp[vn++] = "summon similar monsters";
	if (l_ptr->flags6 & RF6_S_MONSTER)   vp[vn++] = "summon a monster";
	if (l_ptr->flags6 & RF6_S_MONSTERS)  vp[vn++] = "summon monsters";
	if (l_ptr->flags6 & RF6_S_ANIMAL)    vp[vn++] = "summon animals";
	if (l_ptr->flags6 & RF6_S_SPIDER)    vp[vn++] = "summon spiders";
	if (l_ptr->flags6 & RF6_S_HOUND)     vp[vn++] = "summon hounds";
	if (l_ptr->flags6 & RF6_S_HYDRA)     vp[vn++] = "summon hydras";
	if (l_ptr->flags6 & RF6_S_ANGEL)     vp[vn++] = "summon an angel";
	if (l_ptr->flags6 & RF6_S_DEMON)     vp[vn++] = "summon a demon";
	if (l_ptr->flags6 & RF6_S_UNDEAD)    vp[vn++] = "summon an undead";
	if (l_ptr->flags6 & RF6_S_DRAGON)    vp[vn++] = "summon a dragon";
	if (l_ptr->flags6 & RF6_S_HI_UNDEAD) vp[vn++] = "summon Greater Undead";
	if (l_ptr->flags6 & RF6_S_HI_DRAGON) vp[vn++] = "summon Ancient Dragons";
	if (l_ptr->flags6 & RF6_S_HI_DEMON)  vp[vn++] = "summon Greater Demons";
	if (l_ptr->flags6 & RF6_S_WRAITH)    vp[vn++] = "summon Ring Wraiths";
	if (l_ptr->flags6 & RF6_S_UNIQUE)    vp[vn++] = "summon Unique Monsters";

	/* Describe spells */
	if (vn)
	{
		/* Note magic */
		magic = TRUE;

		/* Intro */
		if (breath)
		{
			text_out(", and is also");
		}
		else
		{
			text_out(format("%^s is", wd_he[msex]));
		}

		/* Verb Phrase */
		text_out(" magical, casting spells");

		/* Adverb */
		if (l_ptr->flags2 & RF2_SMART) text_out(" intelligently");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" which ");
			else if (n < vn-1) text_out(", ");
			else text_out(" or ");

			/* Dump */
			text_out(vp[n]);
		}
	}


	/* End the sentence about innate/other spells */
	if (breath || magic)
	{
		/* Total casting */
		m = l_ptr->cast_innate + l_ptr->cast_spell;

		/* Average frequency */
		n = (r_ptr->freq_innate + r_ptr->freq_spell) / 2;

		/* Describe the spell frequency */
		if (m > 100)
		{
			text_out(format("; 1 time in %d", 100 / n));
		}

		/* Guess at the frequency */
		else if (m)
		{
			n = ((n + 9) / 10) * 10;
			text_out(format("; about 1 time in %d", 100 / n));
		}

		/* End this sentence */
		text_out(".  ");
	}
}


static void describe_monster_drop(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	bool sin = FALSE;
	bool plu = TRUE;

	int n;

	cptr p;

	int msex = 0;

	int vn;
	cptr vp[64];

	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	else if (r_ptr->flags1 & RF1_MALE) msex = 1;

	/* Drops gold and/or items */
	if (l_ptr->drop_gold || l_ptr->drop_item)
	{
		/* Intro */
		text_out(format("%^s may carry", wd_he[msex]));

		/* Count maximum drop */
		n = MAX(l_ptr->drop_gold, l_ptr->drop_item);

		/* One drop (may need an "n") */
		if (n == 1)
		{
			text_out(" a");
			sin = TRUE;
			plu = FALSE;
		}

		/* Two drops */
		else if (n == 2)
		{
			text_out(" one or two");
		}

		/* Many drops */
		else
		{
			text_out(format(" up to %d", n));
		}


		/* Great */
		if (l_ptr->flags1 & RF1_DROP_GREAT)
		{
			p = " exceptional ";
		}

		/* Good (no "n" needed) */
		else if (l_ptr->flags1 & RF1_DROP_GOOD)
		{
			p = " good ";
			sin = FALSE;
		}

		/* Okay */
		else
		{
			p = " ";
		}

		/* Collect special abilities. */
		vn = 0;
		if (l_ptr->flags7 & (RF7_DROP_CHEST)) vp[vn++] = "chest";
		if (l_ptr->flags7 & (RF7_DROP_TOOL)) vp[vn++] = "tool";
		if (l_ptr->flags7 & (RF7_DROP_LITE)) vp[vn++] = "lite";
		if (l_ptr->flags7 & (RF7_DROP_WEAPON)) vp[vn++] = "weapon";
		if (l_ptr->flags7 & (RF7_DROP_MISSILE)) vp[vn++] = "missile weapon";
		if (l_ptr->flags7 & (RF7_DROP_ARMOR)) vp[vn++] = "armour";
		if (l_ptr->flags7 & (RF7_DROP_CLOTHES)) vp[vn++] = "garment";
		if (l_ptr->flags7 & (RF7_DROP_JEWELRY)) vp[vn++] = "adornment";
		if (l_ptr->flags7 & (RF7_DROP_RSW)) vp[vn++] = "magical device";
		if (l_ptr->flags7 & (RF7_DROP_WRITING)) vp[vn++] = "written item";
		if (l_ptr->flags7 & (RF7_DROP_MUSIC)) vp[vn++] = "musical item";
		if (l_ptr->flags7 & (RF7_DROP_POTION)) vp[vn++] = "potion";
		if (l_ptr->flags7 & (RF7_DROP_FOOD)) vp[vn++] = "edible item";
		if (l_ptr->flags7 & (RF7_DROP_JUNK)) vp[vn++] = "junk item";

		/* Objects */
		if (((!n) || (!vn))&& (l_ptr->drop_item))
		{
			/* Dump "object(s)" */
			if (get_food_type(r_ptr)) { vp[0] = "mushroom"; vn = 1; }
			else vp[vn++] = "object";
		}

		/* Treasures */
		if (l_ptr->drop_gold)
		{
			/* Dump "treasure(s)" */
			if (get_coin_type(r_ptr)) vp[vn++] = "precious metal";
			else vp[vn++] = "treasure";
		}

		/* Fix up singular */
		if ((vn) && !(is_a_vowel(vp[0][0]))) sin = FALSE;

		/* Describe special abilities. */
		if (vn)
		{
			/* Scan */
			for (n = 0; n < vn; n++)
			{
				/* Intro */
				if (n == 0) { if (sin) text_out("n"); text_out(p);  }
				else if (n < vn-1) text_out(", ");
				else text_out(" or ");

				/* Dump */
				text_out(format("%s%s", vp[n], (plu ? "s" : "")));
			}

			/* End */
			text_out(".  ");
		}
	}
}


static void describe_monster_attack(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];
	int m, n, r;
	cptr p, q;

	int msex = 0;

	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	else if (r_ptr->flags1 & RF1_MALE) msex = 1;

	
	/* Count the number of "known" attacks */
	for (n = 0, m = 0; m < 4; m++)
	{
		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Count known attacks */
		if (l_ptr->blows[m]) n++;
	}

	/* Examine (and count) the actual attacks */
	for (r = 0, m = 0; m < 4; m++)
	{
		int method, effect, d1, d2;

		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Skip unknown attacks */
		if (!l_ptr->blows[m]) continue;


		/* Extract the attack info */
		method = r_ptr->blow[m].method;
		effect = r_ptr->blow[m].effect;
		d1 = r_ptr->blow[m].d_dice;
		d2 = r_ptr->blow[m].d_side;


		/* No method yet */
		p = NULL;

		/* Get the method */
		switch (method)
		{
			case RBM_HIT:	p = "hit"; break;
			case RBM_TOUCH:	p = "touch"; break;
			case RBM_PUNCH:	p = "punch"; break;
			case RBM_KICK:	p = "kick"; break;
			case RBM_CLAW:	p = "claw"; break;
			case RBM_BITE:	p = "bite"; break;
			case RBM_STING:	p = "sting"; break;
			case RBM_VOMIT:	p = "vomit"; break;
			case RBM_BUTT:	p = "butt"; break;
			case RBM_CRUSH:	p = "crush"; break;
			case RBM_ENGULF:	p = "engulf"; break;
			case RBM_PECK:	p = "peck"; break;
			case RBM_CRAWL:	p = "crawl on you"; break;
			case RBM_DROOL:	p = "drool on you"; break;
			case RBM_SLIME:	p = "slimed you"; break;
			case RBM_SPIT:	p = "spit"; break;
			case RBM_GAZE:	p = "gaze"; break;
			case RBM_WAIL:	p = "wail"; break;
			case RBM_SPORE:	p = "release spores"; break;
			case RBM_LASH:	p = "lash you with a whip"; break;
			case RBM_BEG:	p = "beg"; break;
			case RBM_INSULT:	p = "insult"; break;
			case RBM_MOAN:	p = "moan"; break;
			case RBM_THROW:	p = "throw"; break;
			case RBM_TRAP: p = "trap"; break;
			case RBM_SHOOT: p = "shoot"; break;
			case RBM_AURA: p = "radiate"; break;
			case RBM_SELF: p = "affect itself";break;
			case RBM_ADJACENT: p = "affect all adjacent"; break;
			case RBM_HANDS: p = "affect an adjacent target"; break;
			case RBM_MISSILE: p = "fires a missile"; break;
			case RBM_BOLT_10: p = "create a bolt"; break;
			case RBM_BOLT: p = "create a powerful bolt"; break;
			case RBM_BEAM: p = "create a beam"; break;
			case RBM_BLAST: p = "create an adjacent blast"; break;
			case RBM_WALL: p = "create a wall"; break;
			case RBM_BALL: p = "create a ball"; break;
			case RBM_CLOUD: p = "create a cloud"; break;
			case RBM_STORM: p = "create a storm"; break;
			case RBM_BREATH: p = "breathes"; break;
			case RBM_AREA: p = "affect an area"; break;
			case RBM_LOS: p = "affect all in line of sight"; break;
			case RBM_LINE: p = "creats a line"; break;
			case RBM_AIM: p = "affect a target"; break;
			case RBM_ORB: p = "create an orb"; break;
			case RBM_STAR: p = "create a star"; break;
			case RBM_CROSS: p = "create a cross"; break;
			case RBM_SPHERE: p = "create a sphere"; break;
			case RBM_PANEL: p = "affect the current panel"; break;
			case RBM_LEVEL: p = "affect the current level"; break;
		}


		/* Default effect */
		q = NULL;

		/* Get the effect */
		switch (effect)
		{
			case GF_NOTHING: q = "nothing"; break;
			case GF_ARROW: p= "hurt with arrows"; break;
			case GF_MISSILE: q="blast with magic missiles";break;
			case GF_MANA: q="blast with magic";break;       
			case GF_HOLY_ORB: q="blast with holy magic";break;
			case GF_LITE_WEAK: q="light up";break;
			case GF_DARK_WEAK: q="darken";break;
			case GF_WATER_WEAK: q="soak with water";break;
			case GF_PLASMA: q="blast with plasma";break;
			case GF_METEOR: q="blast with meteors";break;
			case GF_ICE: q="cover with ice";break;
			case GF_GRAVITY: q="crush with gravity";break;
			case GF_INERTIA: q="slow with inertia";break;
			case GF_FORCE: q="impact with force";break;
			case GF_TIME: q="take back in time";break;
			case GF_ACID:   q = "dissolve"; break;
			case GF_ELEC:   q = "electrify"; break;
			case GF_FIRE:   q = "burn"; break;
			case GF_COLD:   q = "freeze"; break;
			case GF_POIS: q = "poison"; break;
			case GF_LITE: q = "blast with powerful light";break;
			case GF_DARK: q = "blast with powerful darkness";break;
			case GF_WATER: q="blast with water";break;
			case GF_CONFUSION:      q = "confuse"; break;
			case GF_SOUND: q = "deafen";break;
			case GF_SHARD: q = "blast with shards";break;
			case GF_NEXUS: q = "blast with nexus";break;
			case GF_NETHER: q = "blast with nether";break;
			case GF_CHAOS: q = "blast with chaoas";break;
			case GF_DISENCHANT: q = "blast with disenchantment";break;
			case GF_KILL_WALL: q = "remove rocks";break;
			case GF_KILL_DOOR: q = "remove doors";break;
			case GF_KILL_TRAP: q = "remove traps";break;
			case GF_MAKE_WALL: q = "create walls";break;
			case GF_MAKE_DOOR: q = "create doors";break;
			case GF_MAKE_TRAP: q = "create traps";break;
			case GF_BRIDGE: q = "create a stone bridge"; break;
			case GF_AWAY_UNDEAD: q = "teleport away undead";break;
			case GF_AWAY_EVIL: q = "teleport away evil";break;
			case GF_AWAY_ALL: q = "teleport away";break;
			case GF_TURN_UNDEAD: q = "turn undead";break;
			case GF_TURN_EVIL: q = "turn evil";break;
			case GF_TURN_ALL: q = "turn all";break;
			case GF_DISP_UNDEAD: q = "dispel undead";break;
			case GF_DISP_EVIL: q = "dispel evil";break;
			case GF_DISP_ALL: q = "dispel all";break;
			case GF_OLD_CLONE: q = "clone";break;
			case GF_OLD_POLY: q = "polymorph";break;
			case GF_OLD_HEAL: q = "heal";break;
			case GF_OLD_SPEED: q = "hasten";break;
			case GF_OLD_SLOW: q = "slow";break;
			case GF_OLD_CONF: q = "confuse";break;
			case GF_OLD_SLEEP: q = "send to sleep";break;
			case GF_OLD_DRAIN: q = "drain life";break;
			case GF_LAVA: q = "burn with lava";break;
			case GF_BWATER: q = "scald with boiling water";break;
			case GF_BMUD: q = "splash with boiling mud";break;
			case GF_HURT:   q = "attack"; break;
			case GF_UN_BONUS:       q = "disenchant"; break;
			case GF_UN_POWER:       q = "drain charges"; break;
			case GF_EAT_GOLD:       q = "steal gold"; break;
			case GF_EAT_ITEM:       q = "steal items"; break;
			case GF_EAT_FOOD:       q = "eat your food"; break;
			case GF_EAT_LITE:       q = "absorb light"; break;
			case GF_FALL: q = "drop into a pit";break;
			case GF_FALL_MORE: q = "drop through the floor";break;
			case GF_FALL_SPIKE: q = "drop into a spiked pit";break;
			case GF_FALL_POIS: q = "drop into a poison spiked pit";break;
			case GF_BLIND:  q = "blind"; break;
			case GF_TERRIFY:	q = "terrify"; break;
			case GF_PARALYZE:       q = "paralyze"; break;
			case GF_LOSE_STR:       q = "reduce strength"; break;
			case GF_LOSE_INT:       q = "reduce intelligence"; break;
			case GF_LOSE_WIS:       q = "reduce wisdom"; break;
			case GF_LOSE_DEX:       q = "reduce dexterity"; break;
			case GF_LOSE_CON:       q = "reduce constitution"; break;
			case GF_LOSE_CHR:       q = "reduce charisma"; break;
			case GF_LOSE_ALL:       q = "reduce all stats"; break;
			case GF_SHATTER:	q = "shatter"; break;
			case GF_EXP_10: q = "lower experience (by 10d6+)"; break;
			case GF_EXP_20: q = "lower experience (by 20d6+)"; break;
			case GF_EXP_40: q = "lower experience (by 40d6+)"; break;
			case GF_EXP_80: q = "lower experience (by 80d6+)"; break;
			case GF_RAISE:	   q = "raise water"; break;
			case GF_LOWER:		q = "lower water"; break;
			case GF_LOCK_DOOR:	q = "lock doors"; break;
			case GF_HALLU:	  q = "create hallucinations"; break;
			case GF_FEATURE:	q = "surround you with something"; break;
			case GF_STEAM:	q = "scald with steam"; break;
			case GF_VAPOUR:	q = "dissolve with acidic vapour"; break;
			case GF_SMOKE:	q = "burn with smoke"; break;
			case GF_SUFFOCATE:	q = "suffocate"; break;
			case GF_HUNGER:		q = "starve"; break;
			case GF_DISEASE:		q = "infect with disease"; break;
			case GF_LOSE_MANA:	q = "drain mana"; break;
			case GF_WOUND:		q = "wound"; break;
			case GF_BATTER:		q = "batter"; break;
		}


		/* Introduce the attack description */
		if (!r)
		{
			text_out(format("%^s can ", wd_he[msex]));
		}
		else if (r < n-1)
		{
			text_out(", ");
		}
		else
		{
			text_out(", and ");
		}


		/* Hack -- force a method */
		if (!p) p = "do something weird";

		/* Describe the method */
		text_out(p);


		/* Describe the effect (if any) */
		if (q)
		{
			/* Describe the attack type */
			text_out(" to ");
			text_out(q);

			/* Describe damage (if known) */
			if (d1 && d2 && know_damage(r_idx, l_ptr, m))
			{
				/* Display the damage */
				text_out(" with damage");
				text_out(format(" %dd%d", d1, d2));
			}
		}


		/* Count the attacks as printed */
		r++;
	}

	/* Finish sentence above */
	if (r)
	{
		text_out(".  ");
	}

	/* Notice lack of attacks */
	else if (l_ptr->flags1 & RF1_NEVER_BLOW)
	{
		text_out(format("%^s has no physical attacks.  ", wd_he[msex]));
	}

	/* Or describe the lack of knowledge */
	else
	{
		text_out(format("Nothing is known about %s attack.  ", wd_his[msex]));
	}
}


static void describe_monster_abilities(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int n;

	int vn;
	cptr vp[64];

	int msex = 0;


	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	else if (r_ptr->flags1 & RF1_MALE) msex = 1;

	/* Collect special abilities. */
	vn = 0;
	if (l_ptr->flags2 & RF2_HAS_LITE) vp[vn++] = "illuminate the dungeon";
	if (l_ptr->flags2 & RF2_OPEN_DOOR) vp[vn++] = "open doors";
	if (l_ptr->flags2 & RF2_BASH_DOOR) vp[vn++] = "bash down doors";
	if (l_ptr->flags2 & RF2_PASS_WALL) vp[vn++] = "pass through walls";
	if (l_ptr->flags2 & RF2_KILL_WALL) vp[vn++] = "bore through walls";
	if (l_ptr->flags2 & RF2_KILL_BODY) vp[vn++] = "destroy weaker monsters";
	if (l_ptr->flags2 & RF2_TAKE_ITEM) vp[vn++] = "pick up objects";
	if (l_ptr->flags2 & RF2_KILL_ITEM) vp[vn++] = "destroy objects";
	if (l_ptr->flags3 & RF3_OOZE) vp[vn++] = "ooze through tiny cracks";
	if (l_ptr->flags2 & RF2_CAN_CLIMB) vp[vn++] = "climb on walls and ceilings";
	if (l_ptr->flags2 & RF2_CAN_DIG) vp[vn++] = "dig through earth and rubble";
	if (l_ptr->flags2 & RF2_SNEAKY) vp[vn++] = "hide in unusual places";
	if ((l_ptr->flags2 & RF2_CAN_SWIM) && !(l_ptr->flags2 & RF2_MUST_SWIM)) vp[vn++] = "swim under water";
	if ((l_ptr->flags2 & RF2_CAN_FLY) && !(l_ptr->flags2 & RF2_MUST_FLY)) vp[vn++] = "fly over obstacles";

	/* Describe special abilities. */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" can ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
	}

	/* Describe special abilities. */
	if (l_ptr->flags2 & (RF2_NEED_LITE))
	{
		text_out(format("%^s needs light to see you.  ", wd_he[msex]));
	}

	/* Describe special abilities. */
	if (l_ptr->flags2 & (RF2_MUST_SWIM))
	{
		text_out(format("%^s must swim and cannot move out of water.  ", wd_he[msex]));
	}

	/* Describe special abilities. */
	if (l_ptr->flags2 & (RF2_MUST_FLY))
	{
		text_out(format("%^s must fly and cannot move underwater or through webs.  ", wd_he[msex]));
	}

	/* Describe special abilities. */
	if (l_ptr->flags2 & RF2_INVISIBLE)
	{
		text_out(format("%^s is invisible.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & RF2_COLD_BLOOD)
	{
		text_out(format("%^s is cold blooded.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & RF2_EMPTY_MIND)
	{
		text_out(format("%^s is not detected by telepathy.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & RF2_WEIRD_MIND)
	{
		text_out(format("%^s is rarely detected by telepathy.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & RF2_MULTIPLY)
	{
		text_out(format("%^s breeds explosively.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & RF2_REGENERATE)
	{
		text_out(format("%^s regenerates quickly.  ", wd_he[msex]));
	}


	/* Collect susceptibilities */
	vn = 0;
	if (l_ptr->flags3 & RF3_HURT_ROCK) vp[vn++] = "rock remover";
	if (l_ptr->flags3 & RF3_HURT_LITE) vp[vn++] = "bright light";
	if (l_ptr->flags3 & RF3_HURT_WATER) vp[vn++] = "water remover";

	/* Describe susceptibilities */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" is hurt by ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
	}


	/* Collect immunities */
	vn = 0;
	if (l_ptr->flags3 & RF3_IM_ACID) vp[vn++] = "acid";
	if (l_ptr->flags3 & RF3_IM_ELEC) vp[vn++] = "lightning";
	if (l_ptr->flags3 & RF3_IM_FIRE) vp[vn++] = "fire";
	if (l_ptr->flags3 & RF3_IM_COLD) vp[vn++] = "cold";
	if (l_ptr->flags3 & RF3_IM_POIS) vp[vn++] = "poison";
	if (l_ptr->flags3 & RF3_IM_WATER) vp[vn++] = "water";

	/* Describe immunities */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" resists ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
	}


	/* Collect resistances */
	vn = 0;
	if (l_ptr->flags3 & RF3_RES_NETH) vp[vn++] = "nether";
	if (l_ptr->flags3 & RF3_RES_LAVA) vp[vn++] = "lava";
	if (l_ptr->flags3 & RF3_RES_PLAS) vp[vn++] = "plasma";
	if (l_ptr->flags3 & RF3_RES_NEXU) vp[vn++] = "nexus";
	if (l_ptr->flags3 & RF3_RES_DISE) vp[vn++] = "disenchantment";

	/* Describe resistances */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" resists ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
	}


	/* Collect non-effects */
	vn = 0;
	if (l_ptr->flags3 & RF3_NO_STUN) vp[vn++] = "stunned";
	if (l_ptr->flags3 & RF3_NO_FEAR) vp[vn++] = "frightened";
	if (l_ptr->flags3 & RF3_NO_CONF) vp[vn++] = "confused";
	if (l_ptr->flags3 & RF3_NO_SLEEP) vp[vn++] = "slept";

	/* Describe non-effects */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" cannot be ");
			else if (n < vn-1) text_out(", ");
			else text_out(" or ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
	}


	/* Do we know how aware it is? */
	if ((((int)l_ptr->wake * (int)l_ptr->wake) > r_ptr->sleep) ||
	    (l_ptr->ignore == MAX_UCHAR) ||
	    ((r_ptr->sleep == 0) && (l_ptr->tkills >= 10)))
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

		text_out(format("%^s %s intruders, which %s may notice from %d feet.  ",
		    wd_he[msex], act, wd_he[msex], 10 * r_ptr->aaf));
	}

	/* Describe escorts */
	if ((l_ptr->flags1 & RF1_ESCORT) || (l_ptr->flags1 & RF1_ESCORTS))
	{
		text_out(format("%^s usually appears with escorts.  ",
		    wd_he[msex]));
	}

	/* Describe friends */
	else if (l_ptr->flags1 & RF1_FRIENDS)
	{
		text_out(format("%^s usually appears in groups.  ",
		    wd_he[msex]));
	}
}


static void describe_monster_kills(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int msex = 0;


	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	else if (r_ptr->flags1 & RF1_MALE) msex = 1;


	/* Treat uniques differently */
	if (l_ptr->flags1 & RF1_UNIQUE)
	{
		/* Hack -- Determine if the unique is "dead" */
		bool dead = (r_ptr->max_num == 0) ? TRUE : FALSE;

		/* We've been killed... */
		if (l_ptr->deaths)
		{
			/* Killed ancestors */
			text_out(format("%^s has slain %d of your ancestors",
			    wd_he[msex], l_ptr->deaths));

			/* But we've also killed it */
			if (dead)
			{
				text_out(", but you have taken revenge!  ");
			}

			/* Unavenged (ever) */
			else
			{
				text_out(format(", who %s unavenged.  ",
				    plural(l_ptr->deaths, "remains", "remain")));
			}
		}

		/* Dead unique who never hurt us */
		else if (dead)
		{
			text_out("You have slain this foe.  ");
		}
		else
		{
			/* Alive and never killed us */
		}
	}

	/* Not unique, but killed us */
	else if (l_ptr->deaths)
	{
		/* Dead ancestors */
		text_out(format("%d of your ancestors %s been killed by this creature, ",
		    l_ptr->deaths, plural(l_ptr->deaths, "has", "have")));

		/* Some kills this life */
		if (l_ptr->pkills)
		{
			text_out(format("and you have exterminated at least %d of the creatures.  ",
			    l_ptr->pkills));
		}

		/* Some kills past lives */
		else if (l_ptr->tkills)
		{
			text_out(format("and %s have exterminated at least %d of the creatures.  ",
			    "your ancestors", l_ptr->tkills));
		}

		/* No kills */
		else
		{
			text_out(format("and %s is not ever known to have been defeated.  ",
			    wd_he[msex]));
		}
	}

	/* Normal monsters */
	else
	{
		/* Killed some this life */
		if (l_ptr->pkills)
		{
			text_out(format("You have killed at least %d of these creatures.  ",
			    l_ptr->pkills));
		}

		/* Killed some last life */
		else if (l_ptr->tkills)
		{
			text_out(format("Your ancestors have killed at least %d of these creatures.  ",
			    l_ptr->tkills));
		}

		/* Killed none */
		else
		{
			text_out("No battles to the death are recalled.  ");
		}
	}
}


static void describe_monster_toughness(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int msex = 0;


	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	else if (r_ptr->flags1 & RF1_MALE) msex = 1;
	
	/* Describe monster "toughness" */
	if (know_armour(r_idx, l_ptr))
	{
		/* Armor */
		text_out(format("%^s has an armor rating of %d",
		    wd_he[msex], r_ptr->ac));

		/* Maximized hitpoints */
		if (l_ptr->flags1 & RF1_FORCE_MAXHP)
		{
			text_out(format(" and a life rating of %d.  ",
			    r_ptr->hdice * r_ptr->hside));
		}

		/* Variable hitpoints */
		else
		{
			text_out(format(" and a life rating of %dd%d.  ",
			    r_ptr->hdice, r_ptr->hside));
		}
	}
}


static void describe_monster_exp(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	cptr p, q;

	long i, j;


	/* Describe experience if known */
	if (l_ptr->tkills)
	{
		/* Introduction */
		if (l_ptr->flags1 & RF1_UNIQUE)
			text_out("Killing");
		else
			text_out("A kill of");

		text_out(" this creature");

		/* calculate the integer exp part */
		i = (long)r_ptr->mexp * r_ptr->level / p_ptr->lev;

		/* calculate the fractional exp part scaled by 100, */
		/* must use long arithmetic to avoid overflow */
		j = ((((long)r_ptr->mexp * r_ptr->level % p_ptr->lev) *
			  (long)1000 / p_ptr->lev + 5) / 10);

		/* Mention the experience */
		text_out(format(" is worth %ld.%02ld point%s",
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
		text_out(format(" for a%s %lu%s level character.  ",
			q, (long)i, p));
	}
}


static void describe_monster_movement(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int vn;
	cptr vp[64];
	int n;

	bool old = FALSE;

	text_out("This");

	/* Describe the "quality" */
	if (l_ptr->flags3 & (RF3_EVIL)) text_out(" evil");
	if (l_ptr->flags3 & (RF3_UNDEAD)) text_out(" undead");

	/* Collect races */
	vn = 0;

	/* Describe the "race" */
	if (l_ptr->flags3 & (RF3_ANIMAL)) vp[vn++] ="animal";
	if (l_ptr->flags3 & (RF3_ORC)) vp[vn++] ="orc";
	if (l_ptr->flags3 & (RF3_TROLL)) vp[vn++] ="troll";
	if (l_ptr->flags3 & (RF3_GIANT)) vp[vn++] ="giant";
	if (l_ptr->flags3 & (RF3_DRAGON)) vp[vn++] ="dragon";
	if (l_ptr->flags3 & (RF3_DEMON)) vp[vn++] ="demon";
	if (l_ptr->flags3 & (RF3_PLANT)) vp[vn++] ="plant";
	if (l_ptr->flags3 & (RF3_INSECT)) vp[vn++] ="insect";

	/* Describe "races" */
	if (vn)
	{

		/* Intro */
		if (vn > 1) text_out(" mix of");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");
			/* Dump */
			text_out(vp[n]);
		}

	}

	/* Hack -- If not a mix, describe class */
	if (vn < 2)
	{

		if (l_ptr->flags2 & (RF2_ARMOR)) text_out(" warrior"); /* Hack */

		if ((l_ptr->flags2 & (RF2_PRIEST)) && (l_ptr->flags2 & (RF2_MAGE))) text_out(" shaman");
		else if (l_ptr->flags2 & (RF2_PRIEST)) text_out(" priest");
		else if (l_ptr->flags2 & (RF2_MAGE)) text_out(" mage");
		else if (l_ptr->flags2 & (RF2_SNEAKY)) text_out(" thief");
		else if (l_ptr->flags2 & (RF2_ARMOR)) {} /* Hack */
		else if (l_ptr->flags2 & (RF2_ARCHER)) text_out(" archer"); /* Hack */
		else if ((!vn) && (strchr("pqt", r_ptr->d_char))) text_out(" person");
		else if (!vn) text_out(" creature");
	}

	/* Describe location */
	if (r_ptr->level == 0)
	{
		text_out(" lives in the town");
		old = TRUE;
	}
	else if (l_ptr->tkills)
	{
		if (l_ptr->flags1 & RF1_FORCE_DEPTH)
			text_out(" is found ");
		else
			text_out(" is normally found ");
		
		if (depth_in_feet)
		{
			text_out(format("at depths of %d feet",
			    r_ptr->level * 50));
		}
		else
		{
			text_out(format("on dungeon level %d",
			    r_ptr->level));
		}
		old = TRUE;
	}

	if (old) text_out(", and");

	text_out(" moves");

	/* Random-ness */
	if ((l_ptr->flags1 & RF1_RAND_50) || (l_ptr->flags1 & RF1_RAND_25))
	{
		/* Adverb */
		if ((l_ptr->flags1 & RF1_RAND_50) && (l_ptr->flags1 & RF1_RAND_25))
		{
			text_out(" extremely");
		}
		else if (l_ptr->flags1 & RF1_RAND_50)
		{
			text_out(" somewhat");
		}
		else if (l_ptr->flags1 & RF1_RAND_25)
		{
			text_out(" a bit");
		}

		/* Adjective */
		text_out(" erratically");

		/* Hack -- Occasional conjunction */
		if (r_ptr->speed != 110) text_out(", and");
	}

	/* Speed */
	if (r_ptr->speed > 110)
	{
		if (r_ptr->speed > 130) text_out( " incredibly");
		else if (r_ptr->speed > 120) text_out(" very");
		text_out( " quickly");
	}
	else if (r_ptr->speed < 110)
	{
		if (r_ptr->speed < 90) text_out(" incredibly");
		else if (r_ptr->speed < 100) text_out(" very");
		text_out(" slowly");
	}
	else
	{
		text_out(" at normal speed");
	}

	/* The code above includes "attack speed" */
	if (l_ptr->flags1 & RF1_NEVER_MOVE)
	{
		text_out(", but does not deign to chase intruders");
	}

	/* End this sentence */
	text_out(".  ");

#if 0

	if (flags1 & RF1_FORCE_SLEEP)
	{
		sprintf(buf, "%s is always created sluggish.  ", wd_che[msex]);
	}

#endif
}



/*
 * Learn everything about a monster (by cheating)
 */
static void cheat_monster_lore(int r_idx, monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int i;


	/* Hack -- Maximal kills */
	l_ptr->tkills = MAX_SHORT;

	/* Hack -- Maximal info */
	l_ptr->wake = l_ptr->ignore = MAX_UCHAR;

	/* Observe "maximal" attacks */
	for (i = 0; i < 4; i++)
	{
		/* Examine "actual" blows */
		if (r_ptr->blow[i].effect || r_ptr->blow[i].method)
		{
			/* Hack -- maximal observations */
			l_ptr->blows[i] = MAX_UCHAR;
		}
	}

	/* Hack -- maximal drops */
	l_ptr->drop_gold = l_ptr->drop_item =
	(((r_ptr->flags1 & RF1_DROP_4D2) ? 8 : 0) +
	 ((r_ptr->flags1 & RF1_DROP_3D2) ? 6 : 0) +
	 ((r_ptr->flags1 & RF1_DROP_2D2) ? 4 : 0) +
	 ((r_ptr->flags1 & RF1_DROP_1D2) ? 2 : 0) +
	 ((r_ptr->flags1 & RF1_DROP_90)  ? 1 : 0) +
	 ((r_ptr->flags1 & RF1_DROP_60)  ? 1 : 0));

	/* Hack -- but only "valid" drops */
	if (r_ptr->flags1 & RF1_ONLY_GOLD) l_ptr->drop_item = 0;
	if (r_ptr->flags1 & RF1_ONLY_ITEM) l_ptr->drop_gold = 0;

	/* Hack -- observe many spells */
	l_ptr->cast_innate = MAX_UCHAR;
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
void describe_monster(int r_idx, bool spoilers)
{
	monster_lore lore;

	/* Get the race and lore */
	const monster_race *r_ptr = &r_info[r_idx];
	monster_lore *l_ptr = &l_list[r_idx];


	/* Hack -- create a copy of the monster-memory */
	COPY(&lore, l_ptr, monster_lore);

	/* Assume some "obvious" flags */
	lore.flags1 |= (r_ptr->flags1 & RF1_OBVIOUS_MASK);


	/* Killing a monster reveals some properties */
	if (lore.tkills)
	{
		/* Know "race" flags */
		lore.flags3 |= (r_ptr->flags3 & RF3_RACE_MASK);

		/* Know "forced" flags */
		lore.flags1 |= (r_ptr->flags1 & (RF1_FORCE_DEPTH | RF1_FORCE_MAXHP));
	}

	/* Cheat -- know everything */
	if (cheat_know || spoilers) cheat_monster_lore(r_idx, &lore);

	/* Show kills of monster vs. player(s) */
	if (!spoilers && show_details)
		describe_monster_kills(r_idx, &lore);

	/* Monster description */
	if (spoilers || show_details)
		describe_monster_desc(r_idx);

	/* Describe the movement and level of the monster */
	describe_monster_movement(r_idx, &lore);

	/* Describe experience */
	if (!spoilers) describe_monster_exp(r_idx, &lore);

	/* Describe spells and innate attacks */
	describe_monster_spells(r_idx, &lore);

	/* Describe monster "toughness" */
	if (!spoilers) describe_monster_toughness(r_idx, &lore);

	/* Describe the abilities of the monster */
	describe_monster_abilities(r_idx, &lore);

	/* Describe the monster drop */
	describe_monster_drop(r_idx, &lore);

	/* Describe the known attacks */
	describe_monster_attack(r_idx, &lore);

	/* Notice "Quest" monsters */
	if (lore.flags1 & RF1_QUESTOR)
	{
		text_out("You feel an intense desire to kill this monster...  ");
	}

	/* Notice "Guardian" monsters */
	if (lore.flags1 & RF1_GUARDIAN)
	{
		text_out("It is a dungeon guardian, impeding your progress further.  ");
	}

	/* All done */
	text_out("\n");
}





/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
static void roff_top(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	byte a1, a2;
	char c1, c2;


	/* Get the chars */
	c1 = r_ptr->d_char;
	c2 = r_ptr->x_char;

	/* Get the attrs */
	a1 = r_ptr->d_attr;
	a2 = r_ptr->x_attr;


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

	if (!use_dbltile && !use_trptile)
	{
		/* Append the "standard" attr/char info */
		Term_addstr(-1, TERM_WHITE, " ('");
		Term_addch(a1, c1);
		Term_addstr(-1, TERM_WHITE, "')");

		/* Append the "optional" attr/char info */
		Term_addstr(-1, TERM_WHITE, "/('");
		Term_addch(a2, c2);
		if (use_bigtile && (a2 & 0x80)) Term_addch(255, -1);
		Term_addstr(-1, TERM_WHITE, "'):");
	}
}



/*
 * Hack -- describe the given monster race at the top of the screen
 */
void screen_roff(int r_idx)
{
	/* Flush messages */
	message_flush();

	/* Begin recall */
	Term_erase(0, 1, 255);

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Recall monster */
	describe_monster(r_idx, FALSE);

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

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Recall monster */
	describe_monster(r_idx, FALSE);

	/* Describe monster */
	roff_top(r_idx);
}
