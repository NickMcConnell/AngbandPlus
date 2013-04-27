/* File: hero.c */

/* Purpose: code for randomly generated unique monsters */

/*
 * Copyright (c) 2008 Mangojuice
 *
 * This software is licensed under the Gnu Product License (GPL).
 */

#include "angband.h"

static cptr magical = "deuADEGHLUVW#.";
static cptr humanoid = "dhknopsuyzADGHLOPTUVWY";
static cptr hasheroes = "dehknopsuyzADEGHJLOPSTUVWXY#.";


#define PACKAGE_WARRIOR		1
#define PACKAGE_THIEF		2
#define PACKAGE_PRIEST		3
#define PACKAGE_MAGE		4
#define PACKAGE_DRAGON		5
#define PACKAGE_UNDEAD		6
#define PACKAGE_HORROR		7

/*
 * Makes r_ptr be a "new" duplicate of r_ptr_src.  Used in hero
 * creation to get a baseline copy we can modify.
 */
static void copy_race(monster_race * r_ptr, const monster_race * r_ptr_src)
{
	/* Copy all fields */
	COPY(r_ptr, r_ptr_src, monster_race);

	/* None of this monster yet */
	r_ptr->cur_num = 0;

	/* Reset monster knowledge */
	r_ptr->r_sights = 0;
	r_ptr->r_deaths = 0;
	r_ptr->r_wake = 0;
	r_ptr->r_ignore = 0;
	r_ptr->r_drop_gold = 0;
	r_ptr->r_drop_item = 0;
	r_ptr->r_pkills = 0;
	r_ptr->r_tkills = 0;
	r_ptr->r_cast_inate = 0;
	r_ptr->r_cast_spell = 0;
	r_ptr->r_see = 0;

	/* Note that this intentionally does not reset the known racial flags
	   or experience with monster blows; those knowledges are inherited
	   from the base monster race.  However, we do want to remove "library"
	   research status.  */
	r_ptr->r_flags[6] &= ~RF6_LIBRARY;
}

/*
 * Returns the number of non-inate spell abilities
 * the monster has.
 */
static int count_spells(monster_race * r_ptr)
{
	u32b f3 = r_ptr->flags[3] & 0x50000003;
	u32b f4 = r_ptr->flags[4];
	u32b f5 = r_ptr->flags[5];
	int i;
	int cnt = 0;

	for (i = 0; i < 32; i++)
	{
		if (f3 & 0x00000001) cnt++;
		f3 >>= 1;

		if (f4 & 0x00000001) cnt++;
		f4 >>= 1;

		if (f5 & 0x00000001) cnt++;
		f5 >>= 1;
	}

	return cnt;
}

/*
 * Determine what type of upgrade to apply to the given monster race.  Currently,
 * types include only warriors, rogues, mages, and priests.
 */
static int choose_package(const monster_race * r_ptr)
{
	bool q_priest = FALSE;
	bool q_thief = FALSE;
	bool q_mage = FALSE;
	int package_type = -1;
	int i;

	for (i = 0; i < 4; i++)
	{
		if (r_ptr->blow[i].effect == RBE_EAT_GOLD ||
			r_ptr->blow[i].effect == RBE_EAT_ITEM)
		{
			q_thief = TRUE;
		}
	}

	/* Demons / horrors are special */
	if (FLAG(r_ptr, RF_DEMON) || FLAG(r_ptr, RF_ELDRITCH_HORROR) || FLAG(r_ptr, RF_CTH))
		return PACKAGE_HORROR;

	/* Dragons are special */
	if (FLAG(r_ptr, RF_DRAGON) || toupper(r_ptr->d_char) == 'D')
		return PACKAGE_DRAGON;

	/* Things with a breath weapon and few spells get counted as dragons */
	if (r_ptr->flags[3] & 0xAFFFDF00 && count_spells(r_ptr) < 4)
		return PACKAGE_DRAGON;

	/* Priests are priests */
	if (mon_name_cont(r_ptr, "riest") || mon_name_cont(r_ptr, "haman"))
		return PACKAGE_PRIEST;

	/* Things that can heal or cause wounds can be priests */
	if (FLAG(r_ptr, RF_HEAL) || FLAG(r_ptr, RF_CAUSE_1) || FLAG(r_ptr, RF_CAUSE_2) ||
		FLAG(r_ptr, RF_CAUSE_3) || FLAG(r_ptr, RF_CAUSE_4) || mon_name_cont(r_ptr, "aladin"))
		q_priest = TRUE;

	/* Things that have spells can be mages */
	if ((r_ptr->freq_spell > 0) && (r_ptr->flags[3] & ~RF3_ARROW || r_ptr->flags[4] || r_ptr->flags[5]))
	    q_mage = TRUE;

	i = 2;

	package_type = PACKAGE_WARRIOR;

	if (q_priest)
	{
		i++;
		if (one_in_(i)) package_type = PACKAGE_PRIEST;
	}

	if (q_mage)
	{
		i++;
		if (one_in_(i)) package_type = PACKAGE_MAGE;
	}

	if (q_thief)
	{
		i++;
		if (one_in_(i)) package_type = PACKAGE_THIEF;
	}

	/* Undead are special */
	if (FLAG(r_ptr, RF_UNDEAD) &&
		(package_type != PACKAGE_MAGE))
		return PACKAGE_UNDEAD;

	return (package_type);
}

/*
 * Give the upgrades that every upgraded monster should get
 */
static void hero_general_upgrade(monster_race * r_ptr, int offset)
{
	/* Basic modifications */
	r_ptr->hdice += offset;
	r_ptr->level += offset;
	r_ptr->rarity *= (offset+1);
	r_ptr->max_num = 1;
	r_ptr->cur_num = 0;

	r_ptr->aaf = POWER(r_ptr->aaf, 25);
	r_ptr->sleep = POWER(r_ptr->sleep, -25);

	SET_FLAG(r_ptr, RF_UNIQUE);

	/* Up hsides a bit to compensate for those heroes who
	   don't gain anything from the FORCE_MAXHP flag */
	if (FLAG(r_ptr, RF_FORCE_MAXHP))
	{
		r_ptr->hside = (3 * r_ptr->hside)/2;
	}

	SET_FLAG(r_ptr, RF_FORCE_MAXHP);

	if (strchr(humanoid, r_ptr->d_char))
	{
		SET_FLAG(r_ptr, RF_CAN_SPEAK);
		SET_FLAG(r_ptr, RF_OPEN_DOOR);
		if (one_in_(2)) SET_FLAG(r_ptr, RF_BASH_DOOR);
		if (one_in_(4))
		{
			SET_FLAG(r_ptr, RF_FEMALE);
			r_ptr->flags[0] &= ~RF0_MALE;
		}
		else
		{
			SET_FLAG(r_ptr, RF_MALE);
			r_ptr->flags[0] &= ~RF0_FEMALE;
		}
	}

	if (FLAG(r_ptr, RF_FRIENDS) || strchr("koruyAOPTUVWY", r_ptr->d_char))
	{
		if (r_ptr->level > randint1(50)) SET_FLAG(r_ptr, RF_ESCORTS);
		SET_FLAG(r_ptr, RF_ESCORT);
	}

	/* Lose RAND_25, FRIENDS, ONLY_GOLD */
	r_ptr->flags[0] &= ~(RF0_RAND_25 | RF0_FRIENDS | RF0_ONLY_GOLD);

	/* Downgrade RAND_50 */
	if (FLAG(r_ptr, RF_RAND_50))
	{
		r_ptr->flags[0] &= ~RF0_RAND_50;
		r_ptr->flags[0] |= RF0_RAND_25;
	}

	/* Remove STUPID */
	r_ptr->flags[1] &= ~RF1_STUPID;

	/* Remove HURT_LITE */
	r_ptr->flags[2] &= ~RF2_HURT_LITE;

	/* Upgrade drops if not already very plentiful */
	if (!FLAG(r_ptr, RF_DROP_4D2))
	{
		/* All flags other than drops */
		u32b tmp = r_ptr->flags[0] & 0xF03FFFFF;

		/* Only the drops.  Note we could use 0x07C00000 since we know
	       the monster doesn't have DROP_4D2. */
		r_ptr->flags[0] &= 0x0FC00000;
		r_ptr->flags[0] <<= 1;

		r_ptr->flags[0] |= tmp;
	}
	else
	{
		u32b flg = RF0_DROP_60;

		while (flg != RF0_DROP_4D2)
		{
			if (!(r_ptr->flags[0] & flg))
			{
				r_ptr->flags[0] |= flg;
				break;
			}

			flg <<= 1;
		}
	}


	/* Upgrade drop quality */
	if (FLAG(r_ptr, RF_DROP_GOOD))
	{
		SET_FLAG(r_ptr, RF_DROP_GREAT);
		SET_FLAG(r_ptr, RF_ONLY_ITEM);
	}
	else
	{
		SET_FLAG(r_ptr, RF_DROP_GOOD);
		if (FLAG(r_ptr, RF_ONLY_GOLD) && one_in_(3))
		{
			r_ptr->flags[0] &= ~RF0_ONLY_GOLD;
			SET_FLAG(r_ptr, RF_ONLY_ITEM);
		}
		else if (!one_in_(4))
			SET_FLAG(r_ptr, RF_ONLY_ITEM);
	}
}

/*
 * Set some available upgrade abilities
 * not based on the hero package.
 */
static void hero_general_qualifications(monster_race * r_ptr, u32b *gen_quals)
{
	if (!FLAG(r_ptr, RF_IM_ACID))
		gen_quals[2] |= RF2_IM_ACID;
	if (!FLAG(r_ptr, RF_IM_ELEC))
		gen_quals[2] |= RF2_IM_ELEC;
	if (!FLAG(r_ptr, RF_IM_FIRE))
		gen_quals[2] |= RF2_IM_FIRE;
	if (!FLAG(r_ptr, RF_IM_COLD))
		gen_quals[2] |= RF2_IM_COLD;
	if (!FLAG(r_ptr, RF_IM_POIS))
		gen_quals[2] |= RF2_IM_POIS;
	if (!FLAG(r_ptr, RF_NO_STUN))
		gen_quals[2] |= RF2_NO_STUN;
	if (!FLAG(r_ptr, RF_NO_SLEEP))
		gen_quals[2] |= RF2_NO_SLEEP;
	if (!FLAG(r_ptr, RF_NO_CONF))
		gen_quals[2] |= RF2_NO_CONF;

	if (FLAG(r_ptr, RF_ANIMAL) || strchr(magical, r_ptr->d_char) || (r_ptr->flags[3] | 0xAFFFFF00))
	{
		if (!FLAG(r_ptr, RF_NO_FEAR))
			gen_quals[2] |= RF2_NO_FEAR;
		if (!FLAG(r_ptr, RF_CAN_SWIM))
			gen_quals[6] |= RF6_CAN_SWIM;
	}

	if (strchr(magical, r_ptr->d_char) || (r_ptr->flags[3] | 0xAFFFFF00))
	{
		if (!FLAG(r_ptr, RF_CAN_FLY))
			gen_quals[6] |= RF6_CAN_FLY;
		if (!FLAG(r_ptr, RF_DRAIN_MANA))
			gen_quals[4] |= RF4_DRAIN_MANA;
		if (!FLAG(r_ptr, RF_IM_ACID))
			gen_quals[9] |= RF2_IM_ACID;
		if (!FLAG(r_ptr, RF_IM_ELEC))
			gen_quals[9] |= RF2_IM_ELEC;
		if (!FLAG(r_ptr, RF_IM_FIRE))
			gen_quals[9] |= RF2_IM_FIRE;
		if (!FLAG(r_ptr, RF_IM_COLD))
			gen_quals[9] |= RF2_IM_COLD;
		if (!FLAG(r_ptr, RF_IM_POIS))
			gen_quals[9] |= RF2_IM_POIS;
	}
	/* Amberites! */
	if (r_ptr->d_char == 'p' && r_ptr->level > 50)
		gen_quals[2] |= RF2_AMBERITE;
}

/*
 * Qualify for spell upgrades
 */
static void hero_basic_magic(monster_race *r_ptr, u32b *quals)
{
	if (FLAG(r_ptr, RF_MISSILE) || r_ptr->level < 20)
		quals[4] |= RF4_MISSILE;
	if (FLAG(r_ptr, RF_SCARE) || r_ptr->level < 20)
		quals[4] |= RF4_SCARE;

	quals[5] |= RF5_DARKNESS;

	if (FLAG(r_ptr, RF_BO_ACID) && !FLAG(r_ptr, RF_BA_ACID))
		quals[4] |= RF4_BA_ACID;
	if (FLAG(r_ptr, RF_BO_ELEC) && !FLAG(r_ptr, RF_BA_ELEC))
		quals[4] |= RF4_BA_ELEC;
	if (FLAG(r_ptr, RF_BO_FIRE) && !FLAG(r_ptr, RF_BA_FIRE))
		quals[4] |= RF4_BA_FIRE;
	if (FLAG(r_ptr, RF_BO_COLD) && !FLAG(r_ptr, RF_BA_COLD))
		quals[4] |= RF4_BA_COLD;
	if (FLAG(r_ptr, RF_CAUSE_1) && !FLAG(r_ptr, RF_CAUSE_2))
		quals[4] |= RF4_CAUSE_2;
	if (FLAG(r_ptr, RF_CAUSE_2) && !FLAG(r_ptr, RF_CAUSE_3))
		quals[4] |= RF4_CAUSE_3;
	if (FLAG(r_ptr, RF_CAUSE_3) && !FLAG(r_ptr, RF_CAUSE_4))
		quals[4] |= RF4_CAUSE_4;
	if (FLAG(r_ptr, RF_S_MONSTER) && !FLAG(r_ptr, RF_S_MONSTERS))
		quals[5] |= RF5_S_MONSTERS;
	if (!FLAG(r_ptr, RF_S_MONSTER))
		quals[5] |= RF5_S_MONSTER;
	if ((FLAG(r_ptr, RF_TPORT) || FLAG(r_ptr, RF_TELE_TO)) && !FLAG(r_ptr, RF_TELE_AWAY))
		quals[5] |= RF5_TELE_AWAY;
	if ((FLAG(r_ptr, RF_TPORT) || FLAG(r_ptr, RF_BLINK)) && !FLAG(r_ptr, RF_TELE_TO))
		quals[5] |= RF5_TELE_TO;
	if (FLAG(r_ptr, RF_TELE_AWAY) && !FLAG(r_ptr, RF_TELE_LEVEL))
		quals[5] |= RF5_TELE_LEVEL;
	if (!FLAG(r_ptr, RF_HASTE))
		quals[5] |= RF5_HASTE;
	if (!FLAG(r_ptr, RF_BLIND))
		quals[4] |= RF4_BLIND;
	if (!FLAG(r_ptr, RF_CONF))
		quals[4] |= RF4_CONF;
	if (!FLAG(r_ptr, RF_SLOW))
		quals[4] |= RF4_SLOW;
	if (!FLAG(r_ptr, RF_HOLD))
		quals[4] |= RF4_HOLD;
	if (!FLAG(r_ptr, RF_BLINK))
		quals[5] |= RF5_BLINK;
	if (!FLAG(r_ptr, RF_TPORT) && FLAG(r_ptr, RF_BLINK))
		quals[5] |= RF5_TPORT;
	if (!FLAG(r_ptr, RF_HEAL))
		quals[5] |= RF5_HEAL;
	if (FLAG(r_ptr, RF_HEAL) && r_ptr->level > 75 && !FLAG(r_ptr, RF_INVULNER))
		quals[5] |= RF5_INVULNER;
}

static void hero_package_upgrade_warrior(monster_race * r_ptr, u32b * package_quals, int offset)
{
	int i;
	int atks = 0;
	int best = -1;
	int best_avg = -1;

	r_ptr->ac = POWER(r_ptr->ac, offset+rand_range(25,50))+2;
	r_ptr->hside = POWER(r_ptr->hside, offset+rand_range(25,50))+1;
	SET_FLAG(r_ptr, RF_BASH_DOOR);

	/* Extract info about blows */
	for (i = 0; i < 4; i++)
	{
		if (r_ptr->blow[i].method) atks++;
		else break;

		if ((r_ptr->blow[i].d_dice * (r_ptr->blow[i].d_side + 1)) > best_avg)
		{
			best = i;
			best_avg = (r_ptr->blow[i].d_dice * (r_ptr->blow[i].d_side + 1));
		}

		/* Change attacks that don't do damage into "hit" to "hurt", except steals. */
		if (r_ptr->blow[i].d_dice == 0 && (r_ptr->blow[i].effect != RBE_EAT_GOLD &&
			r_ptr->blow[i].effect != RBE_EAT_ITEM))
		{
			r_ptr->blow[i].method = RBM_HIT;
			r_ptr->blow[i].effect = RBE_HURT;
		}
	}

	/* Add blows equal to half the offset */
	atks += (offset+1)/2;
	atks = MIN(4, atks);

	for (i = 0; i < atks; i++)
	{
		/* New attack? */
		if (!r_ptr->blow[i].method)
		{
			r_ptr->blow[i].method = RBM_HIT;
			r_ptr->blow[i].effect = RBE_HURT;
		}

		/* Damage is uniformly the best */
		r_ptr->blow[i].d_dice = r_ptr->blow[best].d_dice;
		r_ptr->blow[i].d_side = r_ptr->blow[best].d_side;
	}

	/* Upgrade damage */
	for (i = 0; i < atks; i++)
	{
		r_ptr->blow[i].d_side = POWER(r_ptr->blow[i].d_side, offset+rand_range(25,50));
	}

	/* Qualifications */
	if (!FLAG(r_ptr, RF_IM_ACID))
		package_quals[2] |= RF2_IM_ACID;
	if (!FLAG(r_ptr, RF_IM_ELEC))
		package_quals[2] |= RF2_IM_ELEC;
	if (!FLAG(r_ptr, RF_IM_FIRE))
		package_quals[2] |= RF2_IM_FIRE;
	if (!FLAG(r_ptr, RF_IM_COLD))
		package_quals[2] |= RF2_IM_COLD;
	if (!FLAG(r_ptr, RF_IM_POIS))
		package_quals[2] |= RF2_IM_POIS;
	if (!FLAG(r_ptr, RF_NO_STUN))
		package_quals[2] |= RF2_NO_STUN;
	if (!FLAG(r_ptr, RF_NO_SLEEP))
		package_quals[2] |= RF2_NO_SLEEP;
	if (!FLAG(r_ptr, RF_NO_CONF))
		package_quals[2] |= RF2_NO_CONF;

	if (r_ptr->speed <= 110)
		package_quals[9] |= 0x00000003;
	else if (r_ptr->speed <= 120)
		package_quals[9] |= 0x00000001;


	for (i = 0; i < 4; i++)
	{
		if (r_ptr->blow[i].effect == RBE_HURT ||
			r_ptr->blow[i].effect == RBE_POISON ||
			r_ptr->blow[i].effect == RBE_EXP_10 ||
			r_ptr->blow[i].effect == RBE_EXP_20 ||
			r_ptr->blow[i].effect == RBE_EXP_40)
				package_quals[9] |= 0x00000010 << i;
	}

	if (FLAG(r_ptr, RF_MOVE_BODY) && !FLAG(r_ptr, RF_KILL_BODY))
		package_quals[1] |= RF1_KILL_BODY;
	else if (!FLAG(r_ptr, RF_MOVE_BODY))
		package_quals[1] |= RF1_MOVE_BODY;
	if (FLAG(r_ptr, RF_TAKE_ITEM) && !FLAG(r_ptr, RF_KILL_ITEM))
		package_quals[1] |= RF1_KILL_ITEM;
	else if (FLAG(r_ptr, RF_CAN_SPEAK) && !FLAG(r_ptr, RF_TAKE_ITEM))
		package_quals[1] |= RF1_TAKE_ITEM;

}


static void hero_package_upgrade_thief(monster_race * r_ptr, u32b * package_quals, int offset)
{
	int i;
	int atks = -1;

	for (i = 0; i < 4; i++)
	{
		if (r_ptr->blow[i].method) atks++;
		else break;
	}

	if (atks < 0) atks = 0;
	else if (atks < 3) atks++;

	/* Last attack becomes a thieving attack for no damage */
	r_ptr->blow[atks].method = RBM_TOUCH;
	r_ptr->blow[atks].effect = (one_in_(2) ? RBE_EAT_GOLD : RBE_EAT_ITEM);
	r_ptr->blow[atks].d_dice = 0;
	r_ptr->blow[atks].d_side = 0;

	r_ptr->ac = POWER(r_ptr->ac, offset+rand_range(25,50))+5;
	r_ptr->hside = POWER(r_ptr->hside, offset+rand_range(10,25));

	/* Upgrade damage */
	for (i = 0; i < atks; i++)
	{
		if (r_ptr->blow[i].d_side)
			r_ptr->blow[i].d_side = POWER(r_ptr->blow[i].d_side, offset+rand_range(25,50));
	}

	/* Double-qualify for speed upgrade, qualify for drops upgrade */
	package_quals[9] |= 0x00000007;

	/* Fully alert. */
	r_ptr->sleep = 0;

	/* Add'l 20% to vision area; makes 50% total */
	r_ptr->aaf = POWER(r_ptr->aaf, 20);

	if (!FLAG(r_ptr, RF_IM_POIS))
	{
		package_quals[2] |= RF2_IM_POIS;
		package_quals[9] |= RF2_IM_POIS;
	}

	for (i = 0; i <= atks; i++)
	{
		if (r_ptr->blow[i].effect == RBE_HURT ||
			r_ptr->blow[i].d_dice == 0 || r_ptr->blow[i].d_side == 0)
				package_quals[9] |= 0x00000010 << i;
	}

	if (!FLAG(r_ptr, RF_ARROW))
		package_quals[3] |= RF3_ARROW;
	if (!FLAG(r_ptr, RF_HASTE))
		package_quals[5] |= RF5_HASTE;
	if (!FLAG(r_ptr, RF_DARKNESS))
		package_quals[5] |= RF5_DARKNESS;
	if (!FLAG(r_ptr, RF_TRAPS))
		package_quals[5] |= RF5_TRAPS;
	if (!FLAG(r_ptr, RF_BLINK))
		package_quals[5] |= RF5_BLINK;
	if (!FLAG(r_ptr, RF_INVISIBLE))
		package_quals[1] |= RF1_INVISIBLE;
	if (!FLAG(r_ptr, RF_TAKE_ITEM))
		package_quals[1] |= RF1_TAKE_ITEM;
}

static void hero_package_upgrade_priest(monster_race * r_ptr, u32b * gen_quals, u32b * package_quals, int offset)
{
	r_ptr->ac = POWER(r_ptr->ac, offset+rand_range(10,25));
	r_ptr->hside = POWER(r_ptr->hside, offset+rand_range(10,25));

	r_ptr->freq_spell += offset+randint1(20+offset);
	r_ptr->freq_inate = r_ptr->freq_spell;

	/* Basic magic upgrades */
	hero_basic_magic(r_ptr, gen_quals);

	if (FLAG(r_ptr, RF_S_UNDEAD) && !FLAG(r_ptr, RF_S_HI_UNDEAD))
		package_quals[5] |= RF5_S_HI_UNDEAD;
	if (!FLAG(r_ptr, RF_S_KIN) && (r_ptr->flags[5] & 0xFFFF0000))
		package_quals[5] |= RF5_S_KIN;
	if (!FLAG(r_ptr, RF_S_UNDEAD) && FLAG(r_ptr, RF_EVIL))
		package_quals[5] |= RF5_S_UNDEAD;
	if (!FLAG(r_ptr, RF_S_DEMON) && FLAG(r_ptr, RF_EVIL))
		package_quals[5] |= RF5_S_DEMON;
	if (!FLAG(r_ptr, RF_S_ANGEL) && FLAG(r_ptr, RF_GOOD))
		package_quals[5] |= RF5_S_ANGEL;
}

static void hero_package_upgrade_mage(monster_race * r_ptr, u32b * gen_quals, u32b * package_quals, int offset)
{
	r_ptr->hside = POWER(r_ptr->hside, offset+rand_range(10,25));

	r_ptr->freq_spell += offset+randint1(20+offset);
	r_ptr->freq_inate = r_ptr->freq_spell;

	/* Basic magic upgrades */
	hero_basic_magic(r_ptr, gen_quals);

	/* Hack that guarantees only one speed upgrade */
	gen_quals[9] |= 0x00000001;
	package_quals[9] |= 0x00000001;

	if (FLAG(r_ptr, RF_BO_MANA) && !FLAG(r_ptr, RF_BA_CHAO))
		package_quals[3] |= RF3_BA_CHAO;
	if ((FLAG(r_ptr, RF_BA_MANA) || FLAG(r_ptr, RF_BA_CHAO)) && !FLAG(r_ptr, RF_BA_CHAO))
		package_quals[3] |= RF3_BA_NUKE;
	if (FLAG(r_ptr, RF_BO_NETH) && !FLAG(r_ptr, RF_BA_NETH))
		package_quals[4] |= RF4_BA_NETH;
	if (FLAG(r_ptr, RF_BO_MANA) && !FLAG(r_ptr, RF_BA_MANA))
		package_quals[4] |= RF4_BA_MANA;
	if (FLAG(r_ptr, RF_BO_FIRE) && !FLAG(r_ptr, RF_BO_PLAS))
		package_quals[4] |= RF4_BO_PLAS;
	if (FLAG(r_ptr, RF_BO_WATE) && !FLAG(r_ptr, RF_BA_WATE))
		package_quals[4] |= RF4_BA_WATE;
	if ((FLAG(r_ptr, RF_BO_FIRE) || FLAG(r_ptr, RF_BO_ELEC)) && !FLAG(r_ptr, RF_BO_MANA))
		package_quals[4] |= RF4_BO_MANA;
	if (FLAG(r_ptr, RF_BA_COLD) && !FLAG(r_ptr, RF_BO_ICEE))
		package_quals[4] |= RF4_BO_ICEE;
	if (!FLAG(r_ptr, RF_BO_ELEC))
		package_quals[4] |= RF4_BO_ELEC;
	if (!FLAG(r_ptr, RF_BO_FIRE))
		package_quals[4] |= RF4_BO_FIRE;
	if (!FLAG(r_ptr, RF_BO_COLD))
		package_quals[4] |= RF4_BO_COLD;
	if (!FLAG(r_ptr, RF_BO_ACID))
		package_quals[4] |= RF4_BO_ACID;
	if (FLAG(r_ptr, RF_MIND_BLAST) && !FLAG(r_ptr, RF_BRAIN_SMASH))
		package_quals[4] |= RF4_BRAIN_SMASH;
	if (FLAG(r_ptr, RF_BRAIN_SMASH) && !FLAG(r_ptr, RF_HAND_DOOM))
		package_quals[5] |= RF5_HAND_DOOM;
}

static void hero_package_upgrade_dragon(monster_race * r_ptr, u32b * package_quals, int offset)
{
	int i;
	int atks = 0;
	int best = -1;
	int best_avg = -1;

	r_ptr->ac = POWER(r_ptr->ac, offset+rand_range(25,50))+2;
	r_ptr->hside = POWER(r_ptr->hside, offset+rand_range(25,50))+1;
	SET_FLAG(r_ptr, RF_BASH_DOOR);

	/* Extract info about blows */
	for (i = 0; i < 4; i++)
	{
		if (r_ptr->blow[i].method) atks++;
		else break;

		if ((r_ptr->blow[i].d_dice * (r_ptr->blow[i].d_side + 1)) > best_avg)
		{
			best = i;
			best_avg = (r_ptr->blow[i].d_dice * (r_ptr->blow[i].d_side + 1));
		}
	}

	/* Add blows equal to half the offset */
	atks += (offset+1)/2;
	atks = MIN(4, atks);

	for (i = 0; i < atks; i++)
	{
		/* New attack? */
		if (!r_ptr->blow[i].method)
		{
			r_ptr->blow[i].method = RBM_CLAW;
			r_ptr->blow[i].effect = RBE_HURT;
			r_ptr->blow[i].d_dice = r_ptr->blow[best].d_dice;
			r_ptr->blow[i].d_side = r_ptr->blow[best].d_side;
		}
	}

	/* Upgrade damage */
	for (i = 0; i < atks; i++)
	{
		r_ptr->blow[i].d_side = POWER(r_ptr->blow[i].d_side, offset+rand_range(25,50));
	}

	/* Qualifications */
	if (!FLAG(r_ptr, RF_IM_ACID))
		package_quals[2] |= RF2_IM_ACID;
	if (!FLAG(r_ptr, RF_IM_ELEC))
		package_quals[2] |= RF2_IM_ELEC;
	if (!FLAG(r_ptr, RF_IM_FIRE))
		package_quals[2] |= RF2_IM_FIRE;
	if (!FLAG(r_ptr, RF_IM_COLD))
		package_quals[2] |= RF2_IM_COLD;
	if (!FLAG(r_ptr, RF_IM_POIS))
		package_quals[2] |= RF2_IM_POIS;
	if (!FLAG(r_ptr, RF_NO_STUN))
		package_quals[2] |= RF2_NO_STUN;
	if (!FLAG(r_ptr, RF_NO_SLEEP))
		package_quals[2] |= RF2_NO_SLEEP;
	if (!FLAG(r_ptr, RF_NO_CONF))
		package_quals[2] |= RF2_NO_CONF;

	if (r_ptr->speed <= 110)
		package_quals[9] |= 0x00000003;
	else if (r_ptr->speed <= 120)
		package_quals[9] |= 0x00000001;

	if (FLAG(r_ptr, RF_MOVE_BODY) && !FLAG(r_ptr, RF_KILL_BODY))
		package_quals[1] |= RF1_KILL_BODY;
	else if (!FLAG(r_ptr, RF_MOVE_BODY))
		package_quals[1] |= RF1_MOVE_BODY;
	if (FLAG(r_ptr, RF_TAKE_ITEM) && !FLAG(r_ptr, RF_KILL_ITEM))
		package_quals[1] |= RF1_KILL_ITEM;
	else if (FLAG(r_ptr, RF_CAN_SPEAK) && !FLAG(r_ptr, RF_TAKE_ITEM))
		package_quals[1] |= RF1_TAKE_ITEM;

	/* Up breath frequency; no changes to spell / ability list otherwise */
	r_ptr->freq_spell += 10;
	r_ptr->freq_inate += 10;

	if (!FLAG(r_ptr, RF_POWERFUL))
		package_quals[1] |= RF1_POWERFUL;
	if (!FLAG(r_ptr, RF_NO_FEAR))
		package_quals[2] |= RF2_NO_FEAR;
	if (!FLAG(r_ptr, RF_S_DRAGON))
		package_quals[5] |= RF5_S_DRAGON;

	/* Unique dragons should not drop treasure */
	SET_FLAG(r_ptr, RF_ONLY_ITEM);

	/* Allow blow upgrades */
	package_quals[9] |= 0x000000F0;
}


static void hero_package_upgrade_undead(monster_race * r_ptr, u32b * gen_quals, u32b * package_quals, int offset)
{
	int i;
	int atks = 0;
	int best = -1;
	int best_avg = -1;

	r_ptr->ac = POWER(r_ptr->ac, offset+rand_range(25,50))+2;
	r_ptr->hside = POWER(r_ptr->hside, offset+rand_range(25,50))+1;
	SET_FLAG(r_ptr, RF_BASH_DOOR);

	/* Extract info about blows */
	for (i = 0; i < 4; i++)
	{
		if (r_ptr->blow[i].method) atks++;
		else break;

		if ((r_ptr->blow[i].d_dice * (r_ptr->blow[i].d_side + 1)) > best_avg)
		{
			best = i;
			best_avg = (r_ptr->blow[i].d_dice * (r_ptr->blow[i].d_side + 1));
		}
	}

	/* Add blows equal to half the offset */
	atks += (offset+1)/2;
	atks = MIN(4, atks);

	for (i = 0; i < atks; i++)
	{
		/* New attack? */
		if (!r_ptr->blow[i].method)
		{
			r_ptr->blow[i].method = RBM_HIT;
			r_ptr->blow[i].effect = RBE_HURT;
			r_ptr->blow[i].d_dice = r_ptr->blow[best].d_dice;
			r_ptr->blow[i].d_side = r_ptr->blow[best].d_side;
		}
	}

	/* Upgrade damage */
	for (i = 0; i < atks; i++)
	{
		r_ptr->blow[i].d_side = POWER(r_ptr->blow[i].d_side, offset+rand_range(10,25));
		r_ptr->blow[i].d_dice += (one_in_(3) ? 1 : 0);
	}

	/* Qualifications */
	if (!FLAG(r_ptr, RF_IM_ACID))
		package_quals[2] |= RF2_IM_ACID;
	if (!FLAG(r_ptr, RF_IM_ELEC))
		package_quals[2] |= RF2_IM_ELEC;
	if (!FLAG(r_ptr, RF_IM_FIRE))
		package_quals[2] |= RF2_IM_FIRE;
	if (!FLAG(r_ptr, RF_IM_COLD))
		package_quals[2] |= RF2_IM_COLD;
	if (!FLAG(r_ptr, RF_IM_POIS))
		package_quals[2] |= RF2_IM_POIS;
	if (!FLAG(r_ptr, RF_NO_STUN))
		package_quals[2] |= RF2_NO_STUN;
	if (!FLAG(r_ptr, RF_NO_SLEEP))
		package_quals[2] |= RF2_NO_SLEEP;
	if (!FLAG(r_ptr, RF_NO_CONF))
		package_quals[2] |= RF2_NO_CONF;
	if (!FLAG(r_ptr, RF_NO_FEAR))
		package_quals[2] |= RF2_NO_FEAR;

	if (!FLAG(r_ptr, RF_INVISIBLE))
		package_quals[1] |= RF1_INVISIBLE;
	if (!FLAG(r_ptr, RF_PASS_WALL) &&
		(strchr("LWVG", r_ptr->d_char) || r_ptr->level > 40))
		package_quals[1] |= RF1_PASS_WALL;


	if (r_ptr->speed <= 120)
		package_quals[9] |= 0x00000003;
	else if (r_ptr->speed <= 130)
		package_quals[9] |= 0x00000001;


	/* Blow type upgrades */
	package_quals[9] |= 0x000000F0;

	if (FLAG(r_ptr, RF_MOVE_BODY) && !FLAG(r_ptr, RF_KILL_BODY))
		package_quals[1] |= RF1_KILL_BODY;
	else if (!FLAG(r_ptr, RF_MOVE_BODY))
		package_quals[1] |= RF1_MOVE_BODY;
	if (FLAG(r_ptr, RF_TAKE_ITEM) && !FLAG(r_ptr, RF_KILL_ITEM))
		package_quals[1] |= RF1_KILL_ITEM;
	else if (FLAG(r_ptr, RF_CAN_SPEAK) && !FLAG(r_ptr, RF_TAKE_ITEM))
		package_quals[1] |= RF1_TAKE_ITEM;

	/* Unique undead should not drop treasure */
	SET_FLAG(r_ptr, RF_ONLY_ITEM);

	/* General magic upgrade */
	if (randint1(50) < r_ptr->level)
	{
		hero_basic_magic(r_ptr, gen_quals);
		if (FLAG(r_ptr, RF_BO_NETH) && !FLAG(r_ptr, RF_BA_NETH))
			package_quals[4] |= RF4_BA_NETH;
		if (!FLAG(r_ptr, RF_BO_NETH))
			package_quals[4] |= RF4_BO_NETH;
		if (!FLAG(r_ptr, RF_S_UNDEAD))
			package_quals[5] |= RF5_S_UNDEAD;
		if (!FLAG(r_ptr, RF_RAISE_DEAD))
			package_quals[5] |= RF5_RAISE_DEAD;
		if (!FLAG(r_ptr, RF_S_HI_UNDEAD) && FLAG(r_ptr, RF_S_UNDEAD) && r_ptr->level > 50)
			package_quals[5] |= RF5_S_HI_UNDEAD;
	}
}


static void hero_package_upgrade_horror(monster_race * r_ptr, u32b * gen_quals, u32b * package_quals, int offset)
{
	int i;
	int atks = 0;
	int best = -1;
	int best_avg = -1;

	r_ptr->ac = POWER(r_ptr->ac, offset+rand_range(25,50))+2;
	r_ptr->hside = POWER(r_ptr->hside, offset+rand_range(25,50))+1;
	SET_FLAG(r_ptr, RF_BASH_DOOR);

	/* Extract info about blows */
	for (i = 0; i < 4; i++)
	{
		if (r_ptr->blow[i].method) atks++;
		else break;

		if ((r_ptr->blow[i].d_dice * (r_ptr->blow[i].d_side + 1)) > best_avg)
		{
			best = i;
			best_avg = (r_ptr->blow[i].d_dice * (r_ptr->blow[i].d_side + 1));
		}
	}

	/* Add blows equal to half the offset */
	atks += (offset+1)/2;
	atks = MIN(4, atks);

	for (i = 0; i < atks; i++)
	{
		/* New attack? */
		if (!r_ptr->blow[i].method)
		{
			r_ptr->blow[i].method = RBM_HIT;
			r_ptr->blow[i].effect = RBE_HURT;
			r_ptr->blow[i].d_dice = r_ptr->blow[best].d_dice;
			r_ptr->blow[i].d_side = r_ptr->blow[best].d_side;
		}
	}

	/* Upgrade damage */
	for (i = 0; i < atks; i++)
	{
		r_ptr->blow[i].d_side = POWER(r_ptr->blow[i].d_side, offset+rand_range(10,25));
		r_ptr->blow[i].d_dice += (one_in_(3) ? 1 : 0);
	}

	/* Qualifications */
	if (!FLAG(r_ptr, RF_IM_ACID))
		package_quals[2] |= RF2_IM_ACID;
	if (!FLAG(r_ptr, RF_IM_ELEC))
		package_quals[2] |= RF2_IM_ELEC;
	if (!FLAG(r_ptr, RF_IM_FIRE))
		package_quals[2] |= RF2_IM_FIRE;
	if (!FLAG(r_ptr, RF_IM_COLD))
		package_quals[2] |= RF2_IM_COLD;
	if (!FLAG(r_ptr, RF_IM_POIS))
		package_quals[2] |= RF2_IM_POIS;
	if (!FLAG(r_ptr, RF_NO_STUN))
		package_quals[2] |= RF2_NO_STUN;
	if (!FLAG(r_ptr, RF_NO_SLEEP))
		package_quals[2] |= RF2_NO_SLEEP;
	if (!FLAG(r_ptr, RF_NO_CONF))
		package_quals[2] |= RF2_NO_CONF;
	if (!FLAG(r_ptr, RF_NO_FEAR))
		package_quals[2] |= RF2_NO_FEAR;

	if (r_ptr->speed <= 120)
		package_quals[9] |= 0x00000003;
	else if (r_ptr->speed <= 130)
		package_quals[9] |= 0x00000001;

	if (FLAG(r_ptr, RF_MOVE_BODY) && !FLAG(r_ptr, RF_KILL_BODY))
		package_quals[1] |= RF1_KILL_BODY;
	else if (!FLAG(r_ptr, RF_MOVE_BODY))
		package_quals[1] |= RF1_MOVE_BODY;
	if (FLAG(r_ptr, RF_TAKE_ITEM) && !FLAG(r_ptr, RF_KILL_ITEM))
		package_quals[1] |= RF1_KILL_ITEM;
	else if (FLAG(r_ptr, RF_CAN_SPEAK) && !FLAG(r_ptr, RF_TAKE_ITEM))
		package_quals[1] |= RF1_TAKE_ITEM;
	if (!FLAG(r_ptr, RF_KILL_WALL))
		package_quals[1] |= RF1_KILL_WALL;

	/* Blow type upgrades */
	package_quals[9] |= 0x000000F0;

	/* Magic upgrade */
	hero_basic_magic(r_ptr, gen_quals);

	r_ptr->freq_spell += 10;
	r_ptr->freq_inate += 10;

	if (FLAG(r_ptr, RF_BO_MANA) && !FLAG(r_ptr, RF_BA_CHAO))
		package_quals[3] |= RF3_BA_CHAO;
	if ((FLAG(r_ptr, RF_BA_MANA) || FLAG(r_ptr, RF_BA_CHAO)) && !FLAG(r_ptr, RF_BA_CHAO))
		package_quals[3] |= RF3_BA_NUKE;
	if (FLAG(r_ptr, RF_BO_NETH) && !FLAG(r_ptr, RF_BA_NETH))
		package_quals[4] |= RF4_BA_NETH;
	if (FLAG(r_ptr, RF_BO_MANA) && !FLAG(r_ptr, RF_BA_MANA))
		package_quals[4] |= RF4_BA_MANA;
	if (FLAG(r_ptr, RF_BO_FIRE) && !FLAG(r_ptr, RF_BO_PLAS))
		package_quals[4] |= RF4_BO_PLAS;
	if (FLAG(r_ptr, RF_BO_WATE) && !FLAG(r_ptr, RF_BA_WATE))
		package_quals[4] |= RF4_BA_WATE;
	if ((FLAG(r_ptr, RF_BO_FIRE) || FLAG(r_ptr, RF_BO_ELEC)) && !FLAG(r_ptr, RF_BO_MANA))
		package_quals[4] |= RF4_BO_MANA;
	if (FLAG(r_ptr, RF_BA_COLD) && !FLAG(r_ptr, RF_BO_ICEE))
		package_quals[4] |= RF4_BO_ICEE;
	if (!FLAG(r_ptr, RF_BO_ELEC))
		package_quals[4] |= RF4_BO_ELEC;
	if (!FLAG(r_ptr, RF_BO_FIRE))
		package_quals[4] |= RF4_BO_FIRE;
	if (!FLAG(r_ptr, RF_BO_COLD))
		package_quals[4] |= RF4_BO_COLD;
	if (!FLAG(r_ptr, RF_BO_ACID))
		package_quals[4] |= RF4_BO_ACID;
	if (FLAG(r_ptr, RF_MIND_BLAST) && !FLAG(r_ptr, RF_BRAIN_SMASH))
		package_quals[4] |= RF4_BRAIN_SMASH;
	if (FLAG(r_ptr, RF_BRAIN_SMASH) && !FLAG(r_ptr, RF_HAND_DOOM))
		package_quals[5] |= RF5_HAND_DOOM;
	if (!FLAG(r_ptr, RF_MIND_BLAST))
		package_quals[4] |= RF4_MIND_BLAST;
	if (!FLAG(r_ptr, RF_ELDRITCH_HORROR) && r_ptr->level > 50)
		package_quals[3] |= RF3_ELDRITCH_HORROR;
}


static void hero_package_upgrade(monster_race * r_ptr, u32b * gen_quals, u32b * package_quals, int package_type, int offset)
{
	int i;

	switch (package_type)
	{
		case PACKAGE_WARRIOR:
			hero_package_upgrade_warrior(r_ptr, package_quals, offset);
			break;
		case PACKAGE_THIEF:
			hero_package_upgrade_thief(r_ptr, package_quals, offset);
			break;
		case PACKAGE_PRIEST:
			hero_package_upgrade_priest(r_ptr, gen_quals, package_quals, offset);
			break;
		case PACKAGE_MAGE:
			hero_package_upgrade_mage(r_ptr, gen_quals, package_quals, offset);
			break;
		case PACKAGE_DRAGON:
			hero_package_upgrade_dragon(r_ptr, package_quals, offset);
			break;
		case PACKAGE_HORROR:
			hero_package_upgrade_horror(r_ptr, gen_quals, package_quals, offset);
			break;
		case PACKAGE_UNDEAD:
			hero_package_upgrade_undead(r_ptr, gen_quals, package_quals, offset);
			break;
		case -1:
			msgf ("Error, bad package!");
			break;
	}
}

static void hero_choose_upgrade(u32b *val, int *slt, u32b *quals)
{
	int i, j, b = 0;
	int choice;

	/* Gather number of bits set */
	for (i = 0; i < 10; i++)
	{
		for (j = 0; j < 32; j++)
		{
			if (quals[i] & (1 << j)) b++;
		}
	}

	if (!b)
	{
		*val = 0;
		*slt = 0;
		return;
	}

	choice = randint0(b);

	/* Find the right one */
	for (i = 0; i < 10; i++)
	{
		for (j = 0; j < 32; j++)
		{
			if (!(quals[i] & (1 << j))) continue;

			if (!choice)
			{
				/* Found it */
				*slt = i;
				*val = 1 << j;
				return;
			}
			else choice--;
		}
	}

	/* Paranoia, should be unreachable */
	*val = 0;
	*slt = 0;
	return;
}

static void hero_upgrade_blow (int bl_idx, monster_race * r_ptr, int package_type)
{
	monster_blow * bl_ptr = &r_ptr->blow[bl_idx];

	switch(package_type)
	{
		case PACKAGE_WARRIOR:
		{
			switch(bl_ptr->effect)
			{
				case RBE_HURT:
				{
					if (one_in_(3)) bl_ptr->effect = RBE_POISON;
					else if (one_in_(2)) bl_ptr->effect = RBE_CONFUSE;
					else bl_ptr->effect = RBE_TERRIFY;
					break;
				}
				case RBE_POISON:
				{
					if (one_in_(3)) bl_ptr->effect = RBE_LOSE_STR;
					else if (one_in_(2)) bl_ptr->effect = RBE_LOSE_CON;
					else bl_ptr->effect = RBE_DISEASE;
					break;
				}
				case RBE_EXP_10:
					bl_ptr->effect = RBE_EXP_20;
					break;
				case RBE_EXP_20:
					bl_ptr->effect = RBE_EXP_40;
					break;
				case RBE_EXP_40:
					bl_ptr->effect = RBE_EXP_80;
					break;
			}
			break;
		}
		case PACKAGE_THIEF:
			if (bl_ptr->d_dice == 0)
			{
				int bl_idx2;
				if (bl_idx)
				{
					bl_idx2 = randint0(bl_idx);
					bl_ptr->d_dice = r_ptr->blow[bl_idx2].d_dice;
					bl_ptr->d_side = r_ptr->blow[bl_idx2].d_side;
				}

				if (!bl_ptr->d_dice)
				{
					bl_ptr->d_dice = 2;
					bl_ptr->d_side = 6;
				}
			}
			else if (bl_ptr->effect == RBE_HURT)
			{
				if (one_in_(3) || one_in_(3)) bl_ptr->effect = RBE_POISON;
				else bl_ptr->effect = RBE_BLIND;
			}
			break;
		case PACKAGE_DRAGON:
			/* Upgrade damage dice */
			bl_ptr->d_dice *= 2;
			bl_ptr->d_side += 2;
			break;
		case PACKAGE_UNDEAD:
		{
			/* Overwrite with new blow */
			int v = randint0(5) + r_ptr->level/20;

			switch(v)
			{
				case 0:
					bl_ptr->effect = RBE_TERRIFY;
					break;
				case 1:
					bl_ptr->d_dice = 0;
					bl_ptr->d_side = 0;
					bl_ptr->method = RBM_TOUCH;
					bl_ptr->effect = RBE_EXP_10;
					break;
				case 2:
					bl_ptr->d_dice = 0;
					bl_ptr->d_side = 0;
					bl_ptr->method = RBM_TOUCH;
					bl_ptr->effect = RBE_EXP_20;
					break;
				case 3:
					bl_ptr->effect = rand_range(RBE_LOSE_STR, RBE_LOSE_CON);
					break;
				case 4:
					bl_ptr->effect = RBE_PARALYZE;
					break;
				case 5:
					bl_ptr->d_dice = 0;
					bl_ptr->d_side = 0;
					bl_ptr->method = RBM_TOUCH;
					bl_ptr->effect = RBE_EXP_40;
					break;
				case 6:
					bl_ptr->d_dice = 0;
					bl_ptr->d_side = 0;
					bl_ptr->method = RBM_TOUCH;
					bl_ptr->effect = RBE_UN_BONUS;
					break;
				case 7:
					bl_ptr->d_dice = 0;
					bl_ptr->d_side = 0;
					bl_ptr->method = RBM_TOUCH;
					bl_ptr->effect = RBE_EXP_80;
					break;
				case 8:
					bl_ptr->effect = RBE_UN_BONUS;
					break;
				default:
					bl_ptr->effect = RBE_EXP_VAMP;
					break;
			}
			break;
		}
		case PACKAGE_HORROR:
		{
			/* Change blow effect */
			int v = randint0(5) + r_ptr->level/20;

			switch(v)
			{
				case 0:
					bl_ptr->effect = RBE_ACID;
					break;
				case 1:
					bl_ptr->effect = RBE_TERRIFY;
					break;
				case 2:
					bl_ptr->effect = RBE_CONFUSE;
					break;
				case 3:
					bl_ptr->effect = rand_range(RBE_LOSE_STR, RBE_LOSE_CON);
					break;
				case 4:
					bl_ptr->effect = RBE_BLIND;
					break;
				case 5:
					bl_ptr->effect = RBE_EXP_40;
					break;
				case 6:
					bl_ptr->effect = RBE_PARALYZE;
					break;
				case 7:
					bl_ptr->effect = RBE_DISEASE;
					break;
				case 8:
					bl_ptr->effect = RBE_UN_BONUS;
					break;
				default:
					bl_ptr->effect = RBE_SHATTER;
					break;
			}
			break;
		}
	}
}

static void hero_apply_upgrade (int val, int slt, monster_race * r_ptr, u32b * gen_quals, u32b * package_quals, int package_type)
{
	int i;

	if (slt < 9)
	{
		/* XOR, because we may sometimes make something be removed */
		r_ptr->flags[slt] ^= val;

		/* Remove IMs from second qual slots */
		if (slt == 2 && (val & 0x001F0000))
		{
			gen_quals[9] &= ~val;
			package_quals[9] &= ~val;
		}
	}
	else
	{
		/* Speed upgrade */
		if (val & 0x00000003)
			r_ptr->speed += 10;

		/* Drops upgrade */
		else if (val & 0x00000004)
		{
			/* All flags other than drops */
			u32b tmp = r_ptr->flags[0] & 0xF03FFFFF;

			/* Only the lower drops */
			r_ptr->flags[0] &= 0x01C00000;
			r_ptr->flags[0] <<= 1;

			r_ptr->flags[0] |= tmp;
		}

		/* Immunities */
		else if (val & 0x001F0000)
		{
			r_ptr->flags[2] |= val;
			gen_quals[2] &= ~val;
			package_quals[2] &= ~val;
		}

		/* Blow upgrade */
		else if (val & 0x000000F0)
		{
			i = 0;

			/* Find the one to upgrade */
			val >>= 4;
			while (!(val & 1))
			{
				i++;
				val >>= 1;
			}

			hero_upgrade_blow(i, r_ptr, package_type);
		}
	}
}

/*
 * Find a slot for a new active hero race
 * How this should work:
 * HERO_MIN is the first r_idx reserved for heroes.
 *
 * Monster race index i corresponds to hero number i - HERO_MIN;
 * the hero information is located in h_list.
 *
 * In order of priority, we will:
 * 1.  Use an empty (rarity 0) slot.
 * 2.  Overwrite a hero that isn't a quest monster and has never
 *     been sighted.
 * 3.  Overwrite a dead hero.
 * 4.  Overwrite a live hero that isn't a quest monster, and isn't
 *     in the monster list.
 * 5.  Fail.
 *
 * When we are about to reuse a hero slot, we need to remember to
 * quark_remove the name of the hero for garbage collection.  We
 * also need to change all objects dropped by that monster to
 * dropped by QW_DELETED_HERO, so they don't show up wrong.
 */
static s16b hero_get_slot(void)
{
	int i, priority, j, base = z_info->h_max;
	monster_race *r_ptr;
	object_type *o_ptr;

	for (priority = 1; priority < 5; priority++)
	{
		/* Loop through the indices, ending at base */
		for (i = base+1; i % z_info->h_max != base % z_info->h_max; i++)
		{
			/* Wrap around if necessary */
			i = i % z_info->h_max;

			r_ptr = &r_info[HERO_MIN + i];

			/* All priorities skip questors */
			if (FLAG(r_ptr, RF_QUESTOR) || h_list[i].flags & HF_QUEST) continue;

			/* All priorities skip currently live monsters */
			if (r_ptr->cur_num) continue;

			/* Top priority skips all existing monsters */
			if (priority == 1 && r_ptr->rarity) continue;

			/* Second priority uses only unsighted monsters */
			if (priority <= 2 && r_ptr->r_sights) continue;

			/* Third priority uses only dead monsters */
			if (priority <= 3 && r_ptr->max_num) continue;

			/* Use this one */
			quark_remove((s16b *)&r_ptr->name);

			/* Look for things dropped by the old hero */
			for (j = 0; j < o_max; j++)
			{
				o_ptr = &o_list[j];

				/* Skip non-objects */
				if (!o_ptr->k_idx) continue;

				/* Skip objects not dropped by a hero */
				if (o_ptr->mem.type != OM_HERO) continue;

				/* or by other heroes */
				if (o_ptr->mem.data != (u32b)(i)) continue;

				/* Reassign */
				o_ptr->mem.data = h_list[i].r_idx;
				o_ptr->mem.type = OM_MONST;
			}

			return (HERO_MIN + i);
		}

		/* Randomize the base.  We randomize here instead of
		   initially so ensure that empty slots are used in
		   order.  After that pass, randomize so we don't
		   always replace the heroes in the same slots */
		base = randint0(z_info->h_max);
	}

	/* If we get here, it implies that there are no open slots.
	   Fail. */
	return 0;
}

static cptr hero_title[3][4][10] =
{
	/* Male */
	{
		/* Warrior */
		{
			"",
			" Brute",
			" Fighter",
			" Warrior",
			" Hero",
			" Captain",
			" Champion",
			" Lord",
			" Duke",
			" King",
		},

		/* Thief */
		{
			"",
			" Scout",
			" Rogue",
			" Spy",
			" Robber",
			" Thief",
			" Burglar",
			" Bandit",
			" Outlaw",
			" Master Thief",
		},

		/* Priest */
		{
			"",
			"",
			" Shaman",
			" Sage",
			" Acolyte",
			" Adept",
			" Mystic",
			" Priest",
			" High priest",
			" Archpriest",
		},

		/* Mage */
		{
			"",
			" Initiate",
			" Illusionist",
			" Apprentice",
			" Spellbinder",
			" Mage",
			" Wizard",
			" Warlock",
			" Sorceror",
			" Archmage",
		},
	},
	/* Female */
	{
		/* Warrior */
		{
			"",
			" Brute",
			" Shieldmaiden",
			" Warrior",
			" Heroine",
			" Captain",
			" Champion",
			" Countess",
			" Duchess",
			" Queen",
		},

		/* Thief */
		{
			"",
			" Scout",
			" Rogue",
			" Spy",
			" Robber",
			" Thief",
			" Burglar",
			" Bandit",
			" Outlaw",
			" Master Thief",
		},

		/* Priest */
		{
			"",
			"",
			" Shaman",
			" Sage",
			" Acolyte",
			" Adept",
			" Mystic",
			" Priestess",
			" High Priestess",
			" Archpriest",
		},

		/* Mage */
		{
			"",
			" Initiate",
			" Illusionist",
			" Apprentice",
			" Spellbinder",
			" Mage",
			" Wizard",
			" Witch",
			" Sorceress",
			" Archmage",
		},
	},
	/* Neuter / Magical */
	{
		/* Warrior */
		{
			"",
			"",
			" Captain",
			" Lord",
			" Chief",
			" Chieftain",
			" Overlord",
			" Prince",
			" King",
			" Emperor",
		},

		/* Thief */
		{
			"",
			"",
			"",
			"",
			"",
			"",
			"",
			"",
			"",
			"",
		},

		/* Priest */
		{
			"",
			"",
			" Shaman",
			" Shaman",
			" Shaman",
			" Mystic",
			" Priest",
			" Necromancer",
			" High Priest",
			" Archpriest",
		},

		/* Mage */
		{
			"",
			"",
			"",
			" Mage",
			" Mage",
			" Wizard",
			" Warlock",
			" Sorceror",
			" Sorceror",
			" Archmage",
		},
	},
};

cptr name_remove_list[5] =
{
	"Novice",
	"Scruffy looking",
	"Hardened",
	"Master",
	"Malicious",
};

cptr name_title_list[] =
{
	"Shaman",
	"shaman",
	"Anti-paladin"
	"Mage",
	"mage",
	"High priest",
	"Enchantress",
	"Archpriest",
	"Sorceror",
	"sorceror",
	"Priest",
	"priest",
	"Ranger",
	"ranger",
	"Paladin",
	"paladin",
	"Archer",
	"archer",
	"Rogue",
	"rogue",
	"Thief",
	"thief",
	"Illusionist",
	"Druid",
	"king",
	" lord",
	"slinger",
	"Emperor",
	"Necromancer",
	"Demonologist",
	"warrior",
	"Warrior",
	"chieftain",
	"captain",
	"Captain",
	NULL,
};

/*
 * Extract the base monster name from a full monster name.
 *
 */
static void hero_base_name(char * buf, uint max, cptr name, cptr title)
{
	char buf2[100];
	int i = 0;
	char * t, *u;
	cptr s;

	/* These monsters should not have titles */
	if (strstr(name, " of "))
	{
		strnfmt(buf, max, "%s", name);
		return;
	}

	strcpy(buf, name);

	/* Remove unwanted modifiers */
	while (TRUE)
	{
		if (i < 5) s = name_remove_list[i];
		else if (strlen(title) == 0) break;
		else s = name_title_list[i-5];

		if (!s) break;

		i++;

		t = strstr(buf, s);

		/* See if s appears in the string. */
		if (!t)
			continue;

		/* Find the end of the string */
		u = t+strlen(s);

		/* Look forward for space */
		while (u[0])
		{
			if (isspace(u[0]))
				u++;
			else
				break;
		}

		/* Look back for a space */
		while (t >= buf)
		{
			t--;
			if (!isspace(t[0]))
			{
				break;
			}
		}

		t++;

		/* Strip this string out */
		t[0] = 0;

		strnfmt(buf2, 100, "%s%s", buf, u);

		strcpy(buf, buf2);
	}

	strnfmt(buf, max, "%s%s", buf, title);

	/* Capitalize each word */
	t = strchr(buf, ' ');

	while(t)
	{
		t++;
		t[0] = toupper(t[0]);
		t = strchr(t, ' ');
	}
}

static void hero_make_name(monster_race * r_ptr, s16b r_idx, int package_type, int offset)
{
	char name[25];
	char fullname[125];
	char basename[100];
	cptr title;
	int n;
	int sex;

	/* For now, just create a table name */
	get_table_name(name, FALSE);

	n = offset+rand_range(-2,2);
	if (n < 0) n = 0;

	if (FLAG(r_ptr, RF_FEMALE)) sex = 1;
	else if (FLAG(r_ptr, RF_MALE)) sex = 0;
	else sex = 2;

	if (strchr(magical, r_ptr->d_char)) sex = 2;

	if (package_type <= PACKAGE_MAGE)
		title = hero_title[sex][package_type-1][n];
	else title = "";

	if (one_in_(offset-1)) title = "";

	/* Get the base monster name */
	hero_base_name(basename, 100, mon_race_name(&r_info[r_idx]), title);

	strnfmt(fullname, 200, "%s, the%s%s%s", name, (isspace(basename[0]) ? "" : " "), basename,
		FLAG(r_ptr, RF_AMBERITE) ? "Amberite" : "");


	r_ptr->name = quark_add(fullname);

	if (!r_ptr->name) get_check("Quark out of space.");
}

/*
 * Creates the hero described by h_ptr at the given r_idx.
 *
 * Note that this function assumes that r_idx does not need freeing up.
 */
static void restore_hero_at (monster_race * r_ptr, s16b r_idx, int offset, u32b seed)
{
	int i, ups = 0;
	monster_race *r_ptr_src = &r_info[r_idx];
	u32b gen_quals[10];
	u32b package_quals[10];
	int package_type = -1;
	bool old_Rand_quick = Rand_quick;
	u32b old_Rand_value = Rand_value;

	Rand_value = seed;
	Rand_quick = TRUE;

	/* Copy data from the source monster */
	copy_race(r_ptr, r_ptr_src);

	/* Determine package type */
	package_type = choose_package(r_ptr_src);

	/* Initialize qualifications */
	for (i = 0; i < 10; i++)
	{
		gen_quals[i] = 0;
		package_quals[i] = 0;
	}

	/* Process general upgrades */
	hero_general_upgrade(r_ptr, offset);

	/* Process general qualifications */
	hero_general_qualifications(r_ptr, gen_quals);

	/* Process package upgrades and qualifications */
	hero_package_upgrade(r_ptr, gen_quals, package_quals, package_type, offset);


	/* Roll for number of upgrades */
	for (i = 0; i < 2*offset; i++)
	{
		if (randint1(50+(10*ups)) < r_ptr_src->level) ups++;
	}

	/* Choose upgrades */
	for (i = 0; i < ups; i++)
	{
		u32b val = 0;
		int slt = 0;

		/* Choose one */
		hero_choose_upgrade(&val, &slt, (i & 1 ? package_quals : gen_quals));

		/* This choice has been taken */
		gen_quals[slt] &= ~val;
		package_quals[slt] &= ~val;

		/* Apply it */
		hero_apply_upgrade(val, slt, r_ptr, gen_quals, package_quals, package_type);
	}

	/* Just in case */
	if (r_ptr->flags[3] | r_ptr->flags[4] | r_ptr->flags[5])
	{
		if (r_ptr->freq_spell < 20) r_ptr->freq_spell = 20;
	}

	/* Calculate experience */
	r_ptr->mexp = monster_auto_experience(r_ptr, FALSE);

	/* Give it a name */
	hero_make_name(r_ptr, r_idx, package_type, offset);

	Rand_quick = old_Rand_quick;
	Rand_value = old_Rand_value;
}

/*
 * Fills in monster memory knowledge for heroes
 */
static void hero_knowledge (monster_race * r_ptr, hero_type * h_ptr)
{
	int i;
	bool lib = (r_ptr->r_flags[6] & RF6_LIBRARY);

	if (h_ptr->flags & HF_QUEST)
		SET_FLAG(r_ptr, RF_QUESTOR);

	/* Restrict knowledge of flags to actual flags */
	for (i = 0; i < 9; i++)
	{
		r_ptr->r_flags[i] &= r_ptr->flags[i];
	}

	/* Restore library research flag if present */
	if (lib) r_ptr->r_flags[6] |= RF6_LIBRARY;
}

/*
 * Make a +offset hero upgrade of the given monster race.
 *
 * Returns the r_idx of the new monster race
 */
static s16b create_hero_aux(int r_idx, int offset, u32b seed)
{
	s16b h_r_idx = hero_get_slot();

	if (h_r_idx >= HERO_MIN) restore_hero_at(&r_info[h_r_idx], r_idx, offset, seed);

	return h_r_idx;
}


bool hero_okay (s16b r_idx)
{
	int i;

	monster_race * r_ptr = &r_info[r_idx];

	/* Don't make heroes out of heroes */
	if (r_idx >= HERO_MIN) return FALSE;

	/* For some monsters, refuse */
	if (FLAG(r_ptr, RF_UNIQUE)) return FALSE;
	if (FLAG(r_ptr, RF_MULTIPLY)) return FALSE;
	if (FLAG(r_ptr, RF_FRIENDLY)) return FALSE;
	for (i = 0; i < 4; i++)
	{
		if (r_ptr->blow[i].method == RBM_EXPLODE)
			return FALSE;
	}

	if (!strchr(hasheroes, r_ptr->d_char)) return FALSE;

	return TRUE;
}


/*
 * Create a new hero based on the given monster race and the given level offset,
 * returning the r_idx of the new hero.
 *
 * Use a slot in the h_list array, fill in all the values.
 */
s16b create_hero (s16b r_idx, int offset, bool quest)
{
	s16b hero_r_idx;
	u32b seed = randint0(0x10000000);
	hero_type * h_ptr = NULL;

	/* Force reasonable values */
	if (offset <= 0) offset = 1;
	if (offset > 8) offset = 8;

	if (!hero_okay(r_idx)) return 0;

	/* Debug */
	/* return 0; */

	hero_r_idx = create_hero_aux(r_idx, offset, seed);

	if (!hero_r_idx) return 0;

	h_ptr = &h_list[hero_r_idx - HERO_MIN];

	h_ptr->r_idx = r_idx;
	h_ptr->flags = quest ? HF_QUEST : 0;
	h_ptr->seed = seed;
	h_ptr->offset = offset;

	/* Set knowledge */
	hero_knowledge(&r_info[hero_r_idx], h_ptr);

	/* Set of heroes has changed, update allocation table */
	reinit_alloc();

	return (hero_r_idx);
}

void restore_hero (s16b hero_idx)
{
	hero_type * h_ptr = &h_list[hero_idx];
	monster_race * r_ptr;

	r_ptr = &r_info[HERO_MIN + hero_idx];

	if (!h_ptr->r_idx) return;

	restore_hero_at(r_ptr, h_ptr->r_idx, h_ptr->offset, h_ptr->seed);

	hero_knowledge(r_ptr, h_ptr);

	reinit_alloc();
}

void hero_death(s16b hero_idx)
{
	hero_type * h_ptr = &h_list[hero_idx];

	h_ptr->flags |= HF_DEAD;
}

/*
 * De-allocate all heroes.  
 * 
 * It is really really not safe to use this function during the game,
 * but it is okay during birth.
 */
void wipe_all_heroes(void)
{
	int i;
	
	/* Free up names */
	for (i = HERO_MIN; i < HERO_MIN + z_info->h_max; i++)
	{
		monster_race *r_ptr = &r_info[i];
		while (r_ptr->name){
			quark_remove ((s16b *)&r_ptr->name); 
		} 
	}
	/* Wipe entries in r_info */
	C_WIPE(&r_info[HERO_MIN], z_info->h_max, monster_race);

	/* Wipe the h_list */
	C_WIPE(h_list, z_info->h_max, hero_type);
	
	/* Set of heroes has changed, update allocation table */
	reinit_alloc();
}


