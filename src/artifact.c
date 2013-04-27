/* File: artifact.c */

/* Purpose: Artifact code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#include "script.h"

/* Chance of using syllables to form the name instead of the "template" files */
#define TABLE_NAME      45

/* Chance of a random artifact being cursed (1 in 13) */
#define A_CURSED        13

/* Chance of getting a 'basic 4' immunity rather than resist (1 in 48) */
#define LOW_IM_LUCK  	48
/* Chance of getting a 'high' immunity rather than resist (1 in 12) */
#define HI_IM_LUCK	12

#define ACTIVATION_CHANCE 3

/*
 * Chooses a random brand.
 *   o_ptr is the object to be branded.
 *   gpts holds "good points" value
 *   bpgs holds "bad points" value
 *   flag holds ability flag
 *   flagslt holds flag group number
 *   misses holds number of misses.
 *
 * Chooses randomly a brand for the object, puts its flag, slot, good and bad points in
 * Doesn't actually grant the brand; just sets up the values.
 */
static void random_bow_ability(int *gpts, long *flag, int *flagslt)
{
    int v2 = randint1(20);
	*flagslt = 2;
	if (v2 <= 9) {
		*flag = TR2_XTRA_MIGHT;
		*gpts = 4;
	} else if (v2 <= 18) {
		*flag = TR2_XTRA_SHOTS;
		*gpts = 4;
	} else {
		*flag = TR3_WILD_SHOT;
		*gpts = 1;
		*flagslt = 3;
	}
	return;
}

static void random_brand(object_type *o_ptr, int *gpts, int *bpts, long *flag, int *flagslt)
{
	int v2;

	if (o_ptr->tval == TV_BOW)
	{
		random_bow_ability(gpts, flag, flagslt);
	}

	/* melee weapons */
	v2 = randint1(130);
	*flagslt = 0;
	if (v2 <= 80) {
		*gpts = (FLAG(o_ptr, TR_BRAND_ACID) ||
				FLAG(o_ptr, TR_BRAND_ELEC) ||
				FLAG(o_ptr, TR_BRAND_COLD) ||
				FLAG(o_ptr, TR_BRAND_FIRE) ||
				FLAG(o_ptr, TR_BRAND_POIS) ? 5 : 6);

		if (v2 <= 16)  *flag = TR0_BRAND_ACID;
		else if (v2 <= 32) *flag = TR0_BRAND_ELEC;
		else if (v2 <= 50) *flag = TR0_BRAND_COLD;
		else if (v2 <= 66) *flag = TR0_BRAND_FIRE;
		else *flag = TR0_BRAND_POIS;
	} else if (v2 <= 90) {
		*flag = TR0_CHAOTIC;
		*gpts = 2;
	} else if (v2 <= 97) {
		*flag = TR0_VAMPIRIC;
		*gpts = 8;
	} else if (v2 <= 105) {
		if (o_ptr->tval == TV_HAFTED) {
			*flag = TR0_IMPACT;
			*gpts = 2;
			*bpts = 2;
		} else {
			*flag = TR0_VORPAL;
			*gpts = 5;
		}
	} else if (v2 <= 115) {
		*flag = TR1_THROW;
		*gpts = 1;
		*flagslt = 1;
	} else if (v2 <= 120) {
		*flag = TR3_RETURN;
		*gpts = 2;
		*flagslt = 3;
	} else {
		if (o_ptr->tval == TV_HAFTED || o_ptr->tval == TV_DIGGING) {
			*flag = TR0_SLAY_UNDEAD;
			*gpts = (FLAG(o_ptr, TR_SLAY_EVIL) ? 2 : 4);
		} else {
			*flag = TR2_BLESSED;
			*gpts = 4;
			*flagslt = 2;
		}
	}
	return;
}

static void random_plus(object_type *o_ptr, int *gpts, long *flag, int *flagslt, int *misses,
	int *goodpts, bool force_low)
{
	int pval = o_ptr->pval;
	int i, j;

	*flagslt = 0;
	/* never give one when pval = 0. */
	if (pval == 0)  return;

	switch (randint1(force_low ? 8 : 40))
	{
		case 1:  case 15:  case 18:  case 40:
			*flag = TR0_STR;
			*gpts = 2*pval + 2;
			break;
		case 2:  case 9:
			*flag = TR0_INT;
			*gpts = 2*pval;
			break;
		case 3:  case 10:
			*flag = TR0_WIS;
			*gpts = 2*pval;
			break;
		case 4:  case 11:  case 12:
			*flag = TR0_DEX;
			*gpts = 2*pval;
			break;
		case 5:  case 13:
			*flag = TR0_CON;
			*gpts = 2*pval + 2;
			break;
		case 6:  case 14:  case 29:
			*flag = TR0_CHR;
			*gpts = (pval < 3 ? 2 : pval);
			break;
		case 7:  case 16:  case 17:
			*flag = TR0_STEALTH;
			*gpts = (pval < 2 ? 1 : pval - 1);
			break;
		case 8:  case 19:  case 20:
			*flag = TR0_SEARCH;
			*gpts = (pval < 3 ? 1 : pval - 2);
			break;
		case 21:  case 22:  case 23:
			*flag = TR0_INFRA;
			*gpts = (pval+1)/2;
			break;
		case 24:
			*flag = TR0_SPEED;
			*gpts = (3*pval*pval)/2;
			break;
		case 25:
			*flag = TR0_SP;
			*gpts = 4*pval - 1;
			break;
		case 26:  case 27:  case 28:
			if (o_ptr->tval == TV_BOW || o_ptr->tval >= TV_BOOTS) return;
			*flag = TR0_TUNNEL;
			*gpts = 2;
			break;
		case 30:
			if (o_ptr->tval == TV_BOW) return;
			*flag = TR0_BLOWS;
			*gpts = 4*pval*pval;
			if (o_ptr->tval >= TV_BOOTS) *gpts = 3*(*gpts)/2;
			break;
		case 31:
			*flag = TR0_STR | TR0_INT | TR0_WIS | TR0_DEX | TR0_CON | TR0_CHR;
			*gpts = 7 + (7*pval);
			break;
		case 32:
			*flag = TR0_STEALTH | TR0_SPEED;
			*gpts = (pval < 2 ? 1 : pval - 1) + ((3*pval*pval)/2);
			break;
		case 33:
			*flag = TR0_INT | TR0_WIS;
			*gpts = (8*pval)/3;
			break;
		case 34:  case 35:  /* slaying */
			if (o_ptr->to_h == 0 && o_ptr->to_d == 0 && o_ptr->tval >= TV_BOOTS) {
				/* 1d2 x 3 plusses now */
				i = randint1(2);
				*gpts = 4*i;
				j = randint1(2);
				*gpts += 4*i;
			} else {
				i = (one_in_(2) ? 1 : 0);
				j = 1-i;
				*gpts = 4;
			}
			/* hack: give bonus now if we can. */
			if (*goodpts >= *gpts) {
				*goodpts -= *gpts;
				o_ptr->to_h = 3*i;
				o_ptr->to_d = 3*j;
				SET_FLAG(o_ptr, TR_SHOW_MODS);
			} else {
				*flag = 0;  /* paranoia; make sure there's no miss later */
				(*misses)++;
			}

			break;
		case 36: case 37: case 38: case 39:
			*gpts = 4;
			/* hack: give bonus now if we can. */
			if (*goodpts >= *gpts) {
				*goodpts -= *gpts;
				o_ptr->to_a += 3;
			} else {
				*flag = 0;
				(*misses)++;
			}
			break;
	}
	return;
}

static void random_resistance(int *gpts, long *flag, int *flagslt, bool force_low)
{
	int v2 = randint1(force_low ? 4 : 30);

	*flagslt = 1;
	if (v2 <= 4) {  /* low resistance */
		if (one_in_(LOW_IM_LUCK))  /* immunity */
		{
			*gpts = 12;
			*flag = ((v2<3) ?  ((v2 == 1) ? TR1_IM_ACID : TR1_IM_ELEC) :
							((v2 == 3) ? TR1_IM_FIRE : TR1_IM_COLD));
		} else {
			*gpts = 3;
			*flag = ((v2<3) ?  ((v2 == 1) ? TR1_RES_ACID : TR1_RES_ELEC) :
							((v2 == 3) ? TR1_RES_FIRE : TR1_RES_COLD));
		}
	} else if (v2 <= 6) {
		*flag = TR1_RES_BLIND;
		*gpts = 5;
	} else if (v2 <= 8) {
		*flag = TR1_RES_FEAR;
		*gpts = 2;
	} else if (v2 <= 17) { /* light, darkness, or poison */
		if (one_in_(HI_IM_LUCK)) /* immunity */
		{
			*gpts = 10;
			*flag = ((v2 <= 11) ? TR1_IM_POIS : ((v2 <= 14) ? TR3_IM_LITE : TR3_IM_DARK));
			if (v2 > 11) *flagslt = 3;
		} else {
			*gpts = ((v2 <= 11) ? 6 : 3);
			*flag = ((v2 <= 11) ? TR1_RES_POIS : ((v2 <= 14) ? TR1_RES_LITE : TR1_RES_DARK));
		}
	} else if (v2 <= 19) {
		*flag = TR1_RES_CONF;
		*gpts = 7;
	} else if (v2 <= 21) {
		*flag = TR1_RES_SOUND;
		*gpts = 6;
	} else if (v2 <= 23) {
		*flag = TR1_RES_SHARDS;
		*gpts = 6;
	} else if (v2 <= 25) {
		*flag = TR1_RES_NETHER;
		*gpts = 8;
	} else if (v2 <= 27) {
		*flag = TR1_RES_NEXUS;
		*gpts = 6;
	} else if (v2 <= 29) {
		*flag = TR1_RES_CHAOS;
		*gpts = 8;
	} else {
		*flag = TR1_RES_DISEN;
		*gpts = 9;
	}
	return;
}

static void random_misc(object_type *o_ptr, int *gpts, int *bpts, long *flag, int *flagslt, bool force_low)
{
	*flagslt = 2;
	switch (randint1(force_low ? 11 : 53))
	{
		case 1: case 12:
			*flag = TR1_SUST_STR;
			*flagslt = 1;
			*gpts = (o_ptr->flags[1] & (TR1_SUST_STR | TR1_SUST_INT | TR1_SUST_WIS |
										TR1_SUST_DEX | TR1_SUST_CON | TR1_SUST_CHR) ? 3 : 4);
			break;
		case 2: case 13:
			*flag = TR1_SUST_INT;
			*flagslt = 1;
			*gpts = (o_ptr->flags[1] & (TR1_SUST_STR | TR1_SUST_INT | TR1_SUST_WIS |
										TR1_SUST_DEX | TR1_SUST_CON | TR1_SUST_CHR) ? 1 : 2);
			break;
		case 3: case 14:
			*flag = TR1_SUST_WIS;
			*flagslt = 1;
			*gpts = (o_ptr->flags[1] & (TR1_SUST_STR | TR1_SUST_INT | TR1_SUST_WIS |
										TR1_SUST_DEX | TR1_SUST_CON | TR1_SUST_CHR) ? 1 : 2);
			break;
		case 4: case 15:
			*flag = TR1_SUST_DEX;
			*flagslt = 1;
			*gpts = (o_ptr->flags[1] & (TR1_SUST_STR | TR1_SUST_INT | TR1_SUST_WIS |
										TR1_SUST_DEX | TR1_SUST_CON | TR1_SUST_CHR) ? 2 : 3);
			break;
		case 5: case 16:
			*flag = TR1_SUST_CON;
			*flagslt = 1;
			*gpts = (o_ptr->flags[1] & (TR1_SUST_STR | TR1_SUST_INT | TR1_SUST_WIS |
										TR1_SUST_DEX | TR1_SUST_CON | TR1_SUST_CHR) ? 2 : 3);
			break;
		case 6: case 17:
			*flag = TR1_SUST_CHR;
			*flagslt = 1;
			*gpts = (o_ptr->flags[1] & (TR1_SUST_STR | TR1_SUST_INT | TR1_SUST_WIS |
										TR1_SUST_DEX | TR1_SUST_CON | TR1_SUST_CHR) ? 1 : 2);
			break;
		case 7: case 18:
			*flag = TR2_LITE;
			*gpts = 2;
			break;
		case 8: case 19: case 20:
			*flag = TR2_SEE_INVIS;
			*gpts = 3;
			break;
		case 9: case 21:
			*flag = TR2_FEATHER;
			*gpts = 2;
			break;
		case 10: case 22:
			*flag = TR2_SLOW_DIGEST;
			*gpts = 2;
			break;
		case 11: case 23:
			*flag = TR2_REGEN;
			*gpts = 3;
			break;
		case 51: case 24: case 25:
			*flag = TR1_FREE_ACT;
			*flagslt = 1;
			*gpts = 5;
			break;
		case 52: case 26:
			*flag = TR1_HOLD_LIFE;
			*flagslt = 1;
			*gpts = (FLAG(o_ptr, TR_RES_NETHER) ? 2 : 4);
			break;
		case 27:
			*flag = TR3_GHOUL_TOUCH;
			*gpts = 4;
			break;
		case 28:
			*flag = TR2_TELEPATHY;
			*gpts = 10;
			break;
		case 29:
			*flag = TR2_TELEPORT;
			*gpts = 2;
			*bpts = 5;  /* semi-curse */
			break;
		case 30:
			*flag = TR2_NO_MAGIC;
			*gpts = 2;
			*bpts = 10;  /* semi-curse */
			break;
		case 31:
			*flag = TR2_NO_TELE;
			*gpts = 2;
			*bpts = 3;  /* semi-curse */
			break;
		case 32:  case 33:
			*flag = TR3_MUTATE;
			*gpts = 5;
			*bpts = 2;  /* semi-curse */
			*flagslt = 3;
			break;
		case 34:  case 35:
			*flag = TR3_PATRON;
			*gpts = 3;
			*bpts = 2;  /* semi-curse */
			*flagslt = 3;
			break;
		case 36:  case 37:
			*flag = TR3_STRANGE_LUCK;
			*gpts = 3;
			*bpts = 2;  /* semi-curse */
			*flagslt = 3;
			break;
		case 38:  case 39:
			*flag = TR3_LUCK_10;
			*gpts = 4;
			*flagslt = 3;
			break;
		case 40:
			*flag = TR3_WILD_WALK;
			*gpts = 2;
			*flagslt = 3;
			break;
		case 41:  case 42:
			*flag = TR1_REFLECT;
			*gpts = 4;
			*flagslt = 1;
			break;
		case 43:  case 49:
			*flag = TR2_SH_FIRE;
			*gpts = 6;
			break;
		case 44:  case 50:
			*flag = TR2_SH_ELEC;
			*gpts = 6;
			break;
		case 45:
			*flag = TR3_SH_COLD;
			*flagslt = 3;
			*gpts = 6;
			break;
		case 46:
			*flag = TR3_SH_ACID;
			*flagslt = 3;
			*gpts = 6;
			break;
		case 47:
			if (one_in_(8)) {
				*flag = TR3_PASS_WALL;
				*flagslt = 3;
				*gpts = 10;
			} else {
				*flag = TR2_SEE_INVIS;
				*gpts = 3;
			}
			break;
		case 48:
			*flag = TR1_SUST_CHR | TR1_SUST_STR | TR1_SUST_INT | TR1_SUST_WIS | TR1_SUST_CON | TR1_SUST_DEX;
			*flagslt = 1;
			*gpts = 6;
			break;
		case 53:
			*flag = TR3_SH_FEAR;
			*flagslt = 3;
			*gpts = 8;
			break;
	}
}

static int random_curse(object_type *o_ptr, bool evil)
{
	int points = 0;
	switch (randint1(evil ? 32 : 18))
	{
		case 1:
		case 19:
			SET_FLAG(o_ptr, TR_HURT_ACID);
			points = 6;
			break;
		case 2:
		case 20:
			SET_FLAG(o_ptr, TR_HURT_ELEC);
			points = 6;
			break;
		case 3:
		case 21:
			SET_FLAG(o_ptr, TR_HURT_FIRE);
			points = 6;
			break;
		case 4:
		case 22:
			SET_FLAG(o_ptr, TR_HURT_COLD);
			points = 6;
			break;
		case 5:
			SET_FLAG(o_ptr, TR_HURT_LITE);
			points = 8;
			break;
		case 6:
			SET_FLAG(o_ptr, TR_HURT_DARK);
			points = 6;
			break;
		case 7:
		case 8:
			SET_FLAG(o_ptr, TR_AGGRAVATE);
			points = 8;
			break;
		case 9:
			SET_FLAG(o_ptr, TR_SLOW_HEAL);
			points = 4;
			break;
		case 10:
		case 23:
			SET_FLAG(o_ptr, TR_DRAIN_STATS);
			points = 10;
			break;
		case 11:
		case 12:
			SET_FLAG(o_ptr, TR_AUTO_CURSE);
			points = 8;
			break;
		case 13:
		case 14:
			SET_FLAG(o_ptr, TR_CANT_EAT);
			points = 4;
			break;
		case 15:
		case 16:
			SET_FLAG(o_ptr, TR_CURSED);
			points = 4;
			break;
		case 17:
			points = (s16b) rand_range(5, 15);
			o_ptr->to_a -= points;
			break;
		case 18:
			points = (s16b) rand_range(5, 10);
			o_ptr->to_h -= points;
			o_ptr->to_d -= points;
			o_ptr->to_d += rand_range(-3,3);
			points *= 2;
			break;
		case 24:
		case 25:
			SET_FLAG(o_ptr, TR_TELEPORT);
			points = 5;
			break;
		case 26:
			SET_FLAG(o_ptr, TR_DRAIN_EXP);
			points = 8;
			break;
		case 27:
		case 28:
			SET_FLAG(o_ptr, TR_TY_CURSE);
			points = 10;
			break;
		case 29:
		case 30:
		case 31:
			SET_FLAG(o_ptr, TR_CURSED);
			SET_FLAG(o_ptr, TR_HEAVY_CURSE);
			points = 10;
			break;
		case 32:
			SET_FLAG(o_ptr, TR_NO_MAGIC);
			points = 10;
			break;
	}
	return points;
}

static void random_slay(object_type *o_ptr, int *gpts, long *flag, int *flagslt)
{
	int v2 = randint1(50);
	*flagslt = 0;
	if (v2 <= 6) {
		*flag = TR0_SLAY_ANIMAL;
		*gpts = 3;
	} else if (v2 <= 13) {
		*flag = TR0_SLAY_UNDEAD;
		*gpts = (FLAG(o_ptr, TR_SLAY_EVIL) ? 2 : 4);
	} else if (v2 <= 19) {
		*flag = TR0_SLAY_DEMON;
		*gpts = (FLAG(o_ptr, TR_SLAY_EVIL) ? 2 : 4);
	} else if (v2 <= 26) {
		*flag = TR0_SLAY_ORC;
		*gpts = (FLAG(o_ptr, TR_SLAY_EVIL) ? 0 : (FLAG(o_ptr, TR_SLAY_TROLL) ? 1 : 2));
	} else if (v2 <= 33) {
		*flag = TR0_SLAY_TROLL;
		*gpts = (FLAG(o_ptr, TR_SLAY_EVIL) ? 3 : 1);
		if (FLAG(o_ptr, TR_SLAY_ORC)) (*gpts)--;
		if (FLAG(o_ptr, TR_SLAY_GIANT) && !(*gpts)) (*gpts)--;
	} else if (v2 <= 38) {
		*flag = TR0_SLAY_GIANT;
		*gpts = 2;
		if (FLAG(o_ptr, TR_SLAY_EVIL)) (*gpts)--;
		if (FLAG(o_ptr, TR_SLAY_TROLL)) (*gpts)--;
	} else if (v2 <= 42) {
		*flag = TR0_SLAY_DRAGON;
		*gpts = (FLAG(o_ptr, TR_SLAY_EVIL) ? 2 : 4);
	} else if (v2 <= 43) {
		*flag = TR0_KILL_DRAGON;
		*gpts = (FLAG(o_ptr, TR_SLAY_EVIL) ? 4 : 6);
	} else {
		*flag = TR0_SLAY_EVIL;
		*gpts = (FLAG(o_ptr, TR_SLAY_UNDEAD) ? 4 : 6);
		if (FLAG(o_ptr, TR_SLAY_ORC))  *gpts = (*gpts < 2 ? 0 : *gpts-2);
		if (FLAG(o_ptr, TR_SLAY_TROLL))  *gpts = (*gpts < 2 ? 0 : *gpts-2);
		if (FLAG(o_ptr, TR_SLAY_DEMON))  *gpts = (*gpts < 2 ? 0 : *gpts-2);
		if (FLAG(o_ptr, TR_SLAY_DRAGON))  *gpts = (*gpts < 2 ? 0 : *gpts-2);
		if (FLAG(o_ptr, TR_KILL_DRAGON))  *gpts = (*gpts < 2 ? 0 : *gpts-2);
		if (FLAG(o_ptr, TR_SLAY_GIANT))  *gpts = (*gpts < 1 ? 0 : *gpts-1);
	}
	/* hack: slays not so powerful on non-weapons */
	if (o_ptr->tval >= TV_BOOTS || o_ptr->tval == TV_BOW) {
		if (*gpts > 0) *gpts = 1;
	}
	return;
}

/*
 * Attempts to choose a random type-themed power for the object.
 */
static void random_themed(object_type *o_ptr, int *gpts, int *bpts, long *flag, int *flagslt,
	int *misses, int *goodpts)
{
	int v = randint1(20);
	switch(o_ptr->tval)
	{
		case TV_BOW:
			/* Bows have no themes.  Give them extra plusses. */
			if (v <= 7) {
				if (*goodpts < 4) {
					(*misses)++;
					*flag = 0;
				} else {
					o_ptr->to_h += 3;
					*goodpts -= 4;
					*flag = 0;
				}
			} else if (v <= 14) {
				if (*goodpts < 4) {
					(*misses)++;
					*flag = 0;
				} else {
					o_ptr->to_d += 3;
					*goodpts -= 4;
					*flag = 0;
				}
			} else if (v <= 16) {
				if (*goodpts < 4) {
					(*misses)++;
					*flag = 0;
				} else {
					o_ptr->to_a += 3;
					*goodpts -= 4;
					*flag = 0;
				}
			} else {
				random_bow_ability(gpts, flag, flagslt);
			}
			break;
		case TV_DIGGING:
			if (v <= 10) {
				*flag = TR0_BRAND_ACID;
				*flagslt = 0;
				*gpts = 4;  /* discount */
			} else if (v <= 18) {
				*flag = TR2_REGEN;
				*flagslt = 2;
				*gpts = 3;
			} else {
				*flag = TR0_IMPACT;
				*flagslt = 0;
				*gpts = 2;
				*bpts = 2;
			}
			break;
		case TV_HAFTED:
			if (v <= 3) {
				*flag = TR2_BLESSED | TR2_SEE_INVIS;
				*gpts = 2;  /* discount */
				*flagslt = 2;
			} else if (v <= 6) {
				*flag = TR1_HOLD_LIFE;
				*flagslt = 1;
				*gpts = 3;  /* discount */
			} else if (v <= 12) {
				*flag = TR0_SLAY_EVIL | TR0_SLAY_UNDEAD;
				*flagslt = 0;
				*gpts = 3;  /* discount */
			} else if (v <= 14) {
				*flag = TR2_SH_FIRE;
				*flagslt = 2;
				*gpts = 6;
			} else {
				if (*goodpts < 4) {
					(*misses)++;
					*flag = 0;
				} else {
					o_ptr->to_a += 3;
					*goodpts -= 4;
					*flag = 0;
				}
			}
			break;
		case TV_POLEARM:
			if (v <= 5) {  /* increase dice sides */
				if (*goodpts < 2) {
					(*misses)++;
					*flag = 0;
				} else {
					o_ptr->ds += 3;
					*goodpts -= 2;
					*flag = 0;
				}
			} else if (v <= 9) {
				*flag = TR0_SLAY_TROLL;
				*flagslt = 0;
				*gpts = 1;  /* discount */
			} else if (v <= 13) {
				*flag = TR0_SLAY_GIANT;
				*flagslt = 0;
				*gpts = 1;  /* discount */
			} else {  /* increaase to-hit and damage */
				if (*goodpts < 6) { /* discount */
					(*misses)++;
					*flag = 0;
				} else {
					o_ptr->to_h += 3;
					o_ptr->to_d += 3;
					*goodpts -= 6;
					*flag = 0;
				}
			}
			break;
		case TV_SWORD:
			if (v <= 1) {
				*flag = TR0_VORPAL;
				*flagslt = 0;
				*gpts = 5;
			} else if (v <= 2) {
				*flag = TR3_PSI_CRIT;
				*flagslt = 3;
				*gpts = 4;
			} else if (v <= 4) { /* increase base damage dice */
				if (*goodpts < 4) {
					(*misses)++;
					*flag = 0;
				} else {
					*goodpts -= 4;
					o_ptr->dd += 1;
					*flag = 0;
				}
			} else if (v <= 14) {
				random_slay(o_ptr, gpts, flag, flagslt);
			} else {
				random_brand(o_ptr, gpts, bpts, flag, flagslt);
			}
			break;
		case TV_BOOTS:
			if (v <= 3) {
				*flag = TR0_SPEED;
				*flagslt = 0;
				*gpts = (3*(o_ptr->pval)*(o_ptr->pval))/2;
			} else if (v <= 4) {
				*flag = TR3_WILD_WALK;
				*flagslt = 3;
				*gpts = 2;
			} else if (v <= 9) {
				*flag = TR0_STEALTH;
				*flagslt = 0;
				*gpts = (o_ptr->pval < 2 ? 1 : o_ptr->pval - 1);
			} else if (v <= 12) {  /* AC and low resist */
				if (*goodpts < 4) {
					(*misses)++;
				} else {
					*goodpts -= 4;
					o_ptr->to_a += 3;
				}
				random_resistance(gpts, flag, flagslt, TRUE);
			} else {
				random_resistance(gpts, flag, flagslt, FALSE);
			}
			break;
		case TV_GLOVES:
			if (v <= 2 && o_ptr->pval) {
				*flag = TR0_DEX;
				*flagslt = 0;
				*gpts = (2*o_ptr->pval);
			} else if (v <= 4 && o_ptr->pval) {
				*flag = TR0_STR;
				*flagslt = 0;
				*gpts = (2*o_ptr->pval)+2;
			} else if (v <= 9) {
				*flag = TR1_FREE_ACT;
				*flagslt = 1;
				*gpts = 3;  /* discount */
			} else if (v <= 12) {  /* AC and low resist */
				if (*goodpts < 4) {
					(*misses)++;
				} else {
					*goodpts -= 4;
					o_ptr->to_a += 3;
				}
				random_resistance(gpts, flag, flagslt, TRUE);
			} else {
				random_resistance(gpts, flag, flagslt, FALSE);
			}
			break;
		case TV_HELM:
		case TV_CROWN:
			if (v <= 2 && o_ptr->pval) {
				*flag = TR0_INT | TR0_WIS;
				*flagslt = 0;
				*gpts = (8*o_ptr->pval)/3;
			} else if (v <= 3 && o_ptr->pval) {
				*flag = TR0_SEARCH;
				*flagslt = 0;
				*gpts = (o_ptr->pval < 3 ? 1 : o_ptr->pval - 2);
			} else if (v <= 5 && o_ptr->pval) {
				*flag = TR0_INFRA;
				*flagslt = 0;
				*gpts = (1+o_ptr->pval)/2;
			} else if (v <= 7) {
				*flag = TR2_SEE_INVIS;
				*flagslt = 2;
				*gpts = 2;  /* discount */
			} else if (v <= 9) {
				*flag = TR2_TELEPATHY;
				*flagslt = 2;
				*gpts = 8;  /* discount */
			} else if (v <= 12) {  /* AC and low resist */
				if (*goodpts < 4) {
					(*misses)++;
				} else {
					*goodpts -= 4;
					o_ptr->to_a += 3;
				}
				random_resistance(gpts, flag, flagslt, TRUE);
			} else {
				random_resistance(gpts, flag, flagslt, FALSE);
			}
			break;
		case TV_SHIELD:
			if (v <= 3) {
				*flag = TR1_REFLECT;
				*flagslt = 1;
				*gpts = 4;
			} else if (v <= 12) {  /* AC and low resist */
				if (*goodpts < 4) {
					(*misses)++;
				} else {
					*goodpts -= 4;
					o_ptr->to_a += 3;
				}
				random_resistance(gpts, flag, flagslt, TRUE);
			} else {
				random_resistance(gpts, flag, flagslt, FALSE);
			}
			break;
		case TV_CLOAK:
			if (v <= 3) {
				*flag = TR0_STEALTH;
				*flagslt = 0;
				*gpts = (o_ptr->pval < 2 ? 1 : o_ptr->pval - 1);
			} else if (v <= 4) {
				*flag = TR3_LUCK_10;
				*flagslt = 3;
				*gpts = 3;
			} else if (v <= 9) {  /* aura */
				*flag = (v <= 7 ? (v <= 5 ? TR2_SH_ELEC : TR2_SH_FIRE) :
								  (v == 8 ? TR3_SH_COLD : TR3_SH_ACID));
				*flagslt = (v <= 7 ? 2 : 3);
				*gpts = 6;
			} else if (v <= 12) {  /* AC and low resist */
				if (*goodpts < 4) {
					(*misses)++;
				} else {
					*goodpts -= 4;
					o_ptr->to_a += 3;
				}
				random_resistance(gpts, flag, flagslt, TRUE);
			} else {
				random_resistance(gpts, flag, flagslt, FALSE);
			}
			break;
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
			if (v <= 3 && o_ptr->pval) {
				*flag = TR0_CON;
				*flagslt = 0;
				*gpts = (2*o_ptr->pval)+2;
			} else if (v <= 6 && o_ptr->pval) {
				*flag = (o_ptr->tval == TV_SOFT_ARMOR ? TR0_DEX : TR0_STR);
				*flagslt = 0;
				*gpts = (2*o_ptr->pval)+(o_ptr->tval == TV_SOFT_ARMOR ? 0 : 2);
			} else if (v <= 12) {  /* AC and low resist */
				if (*goodpts < 4) {
					(*misses)++;
				} else {
					*goodpts -= 4;
					o_ptr->to_a += 3;
				}
				random_resistance(gpts, flag, flagslt, TRUE);
			} else {
				random_resistance(gpts, flag, flagslt, FALSE);
			}
			break;
		case TV_DRAG_ARMOR:
			/* MegaHack:  No themed ability, instead, loses 3 goodpts. */
			*goodpts = ((*goodpts) > 3 ? (*goodpts)-3 : 0);
			break;
		default:  /* lite, amulet, ring */
			if (v <= 7) {
				random_plus(o_ptr, gpts, flag, flagslt, misses, goodpts, FALSE);
			} else if (v <= 14) {
				random_resistance(gpts, flag, flagslt, FALSE);
			} else {
				random_misc(o_ptr, gpts, bpts, flag, flagslt, FALSE);
			}
			break;
	}
}


static cptr activation_text[] = {
	"The %v glows extremely brightly...",
	"The %v throbs deep green...",
	"The %v glows an intense red...",
	"The %v glows black...",
	"The %v glows an intense blue...",
	"The %v throbs red...",
	"The %v glows deep red...",
	"The %v glows bright white...",
	"The %v glows deep blue...",
	"The %v glows in scintillating colours...",
	"The %v vibrates...",
	"The %v glows violet...",
	"The %v lets out a long, shrill note...",
	"The %v twists in your hands...",
	"The %v shudders...",
	"The %v fades in and out...",
	"The %v hums softly...",
	"The %v blinks in and out...",
	"The %v radiates light blue...",
	"The %v radiates deep purple...",
	"The %v glows deep green...",
	"The %v lets out a shrill wail...",
	"The %v glows brightly...",
	"The %v shines brightly...",
	"The %v glows yellow...",
	"The %v glows light blue...",
	"The %v glows brown...",
	"The %v pulsates...",
	"The %v hums...",
	"The %v glows bright yellow..."
};

static cptr element_list[] =
{
	"GF_ACID", "GF_ELEC", "GF_FIRE", "GF_COLD", "GF_POIS",
	"GF_PLASMA", "GF_WATER", "GF_LITE", "GF_DARK", "GF_SHARDS",
	"GF_SOUND", "GF_CONFUSION", "GF_FORCE", "GF_INERTIA", "GF_MANA",
	"GF_ICE", "GF_CHAOS", "GF_NETHER", "GF_NEXUS", "GF_TIME",
	"GF_GRAVITY", "GF_NUKE", "GF_HOLY_FIRE", "GF_HELL_FIRE", "GF_MISSILE",
};
static cptr element_names[] =
{
	"acid", "lightning", "fire", "cold", "poison",
	"plasma", "water", "light", "darkness", "shards",
	"sound", "confusion", "force", "inertia", "mana",
	"ice", "chaos", "nether", "nexus", "time",
	"gravity", "toxic waste", "holy power", "unholy power", "elemental force",
};
static cptr element_colors[] =
{
	"dark grey", "light blue", "bright red", "pale white", "dark green",
	"bright orange", "dark blue", "bright white", "black", "dark red",
	"amber", "dark purple", "light grey", "silver", "many colors",
	"pale white", "many colors", "black", "yellow", "silver",
	"dark grey", "sickly yellow", "pure white", "dark red", "many colors",
};
static cptr glow_desc[] =
{
	"glows", "shines", "pulses", "throbs", "radiates", "sparks"
};

/*
 * An activation for random artifacts
 */
typedef struct randart_activation randart_activation;

struct randart_activation
{
	cptr text;
	cptr desc;
	cptr effect;
	int freq;
	bool aimed;
	int pp;
	int dice;
	int bonus;
};

#define SPDICE_MM -1

/*
 * {
 * text,
 * desc,
 * effect,
 * freq, aimed, pp, dice, bonus
 * }
 */
static const struct randart_activation randart_activations[] =
{
	{
		NULL,
		"remove fear and cure poison",
		"clear_afraid(); clear_poisoned()",
		20, FALSE, 100, 0, 0
	},
	{
		NULL,
		"stone to mud",
		"wall_to_mud(dir)",
		100, TRUE, 1000, 0, 0
	},
	{
		NULL,
		"destroy doors",
		"destroy_doors_touch()",
		20, FALSE, 1000, 0, 0
	},
	{
		NULL,
		"confuse monster",
		"confuse_monster(dir, 50)",
		50, TRUE, 1000, 0, 0
	},
	{
		NULL,
		"sleep monster",
		"sleep_monster(dir)",
		50, TRUE, 1000, 0, 0
	},
	{
		NULL,
		"slow monster",
		"slow_monster(dir)",
		50, TRUE, 1000, 0, 0
	},
	{
		NULL,
		"identify",
		"if not ident_spell() then return end",
		100, FALSE, 1000, 0, 0
	},
	{
		NULL,
		"recharging",
		"recharge(130)",
		25, FALSE, 1000, 0, 0
	},
	{
		NULL,
		"sleep nearby monsters",
		"sleep_monsters_touch()",
		25, FALSE, 1000, 0, 0
	},
	{
		NULL,
		"sleep monsters",
		"sleep_monsters()",
		10, FALSE, 5000, 0, 0
	},
	{
		"You open a dimensional gate. Choose a destination.",
		"dimension door",
		"if not dimension_door() return end",
		25, FALSE, 1000, 0, 0
	},
	{
		"The %v twists space around you...",
		"teleport (100)",
		"teleport_player(100)",
		100, FALSE, 1000, 0, 0
	},
	{
		"An image forms in your mind...",
		"detection",
		"detect_all()",
		50, FALSE, 1000, 0, 0
	},
	{
		"The %v glows bright red...",
		"explosive rune",
		"explosive_rune()",
		5, FALSE, 5000, 0, 0
	},
	{
		NULL,
		"word of recall",
		"word_of_recall()",
		100, FALSE, 5000, 0, 0
	},
	{
		NULL,
		"restore life levels",
		"restore_level()",
		10, FALSE, 25000, 0, 0
	},
	{
		NULL,
		"teleport away",
		"fire_beam(GF_AWAY_ALL, dir, player.lev)",
		50, TRUE, 5000, 0, 0
	},
	{
		NULL,
		"charm animal",
		"charm_animal(dir, player.lev)",
		10, TRUE, 5000, 0, 0
	},
	{
		NULL,
		"charm animal (level %s)",
		"charm_animal(dir, %s)",
		10, TRUE, 200, 100, 0	/* lev */
	},
	{
		NULL,
		"enslave undead",
		"control_one_undead(dir, player.lev)",
		10, TRUE, 7500, 0, 0
	},
	{
		NULL,
		"enslave undead (level %s)",
		"control_one_undead(dir, %s)",
		10, TRUE, 300, 100, 0	/* lev */
	},
	{
		NULL,
		"charm monster",
		"charm_monster(dir, player.lev)",
		10, TRUE, 10000, 0, 0
	},
	{
		NULL,
		"charm monster (level %s)",
		"charm_monster(dir, %s)",
		10, TRUE, 400, 100, 0	/* lev */
	},
	{
		NULL,
		"alchemy",
		"alchemy()",
		5, FALSE, 10000, 0, 0
	},
	{
		NULL,
		"rune of protection",
		"warding_glyph()",
		10, FALSE, 10000, 0, 0
	},
	{
		NULL,
		"animal friendship",
		"charm_animals(player.lev * 2)",
		5, FALSE, 10000, 0, 0
	},
	{
		NULL,
		"animal friendship (level %s)",
		"charm_animals(%s * 2)",
		5, FALSE, 750, 100, 0	/* lev */
	},
	{
		"The power of the %v banishes evil!",
		"banish evil",
		"banish_evil(200)",
		25, FALSE, 10000, 0, 0
	},
	{
		NULL,
		"genocide",
		"genocide(TRUE)",
		10, FALSE, 10000, 0, 0
	},
	{
		NULL,
		"satisfy hunger",
		"set_food(PY_FOOD_MAX - 1)",
		100, FALSE, 10000, 0, 0
	},
	{
		"The %v emits a blast of air...",
		"whirlwind attack",
		"whirlwind_attack()",
		10, FALSE, 20000, 0, 0
	},
	{
		"The %v glows in scintillating colours...",
		"call chaos",
		"call_chaos()",
		10, FALSE, 25000, 0, 0
	},
	{
		NULL,
		"mass genocide",
		"mass_genocide(TRUE)",
		5, FALSE, 25000, 0, 0
	},
	{
		NULL,
		"mass charm",
		"charm_monsters(player.lev * 2)",
		5, FALSE, 25000, 0, 0
	},
	{
		NULL,
		"mass charm (level %s)",
		"charm_monsters(%s * 2)",
		5, FALSE, 2000, 100, 0	/* lev */
	},
	{
		NULL,
		"restore stats",
		"restore_all_stats()",
		10, FALSE, 25000, 0, 0
	},
	{
		NULL,
		"restore stats and life levels",
		"restore_all_stats(); restore_level()",
		5, FALSE, 50000, 0, 0
	},
	{
		NULL,
		"identify true",
		"identify_fully()",
		25, FALSE, 25000, 0, 0
	},
	{
		NULL,
		"detection, probing and identify true",
		"detect_all(); probing(); identify_fully()",
		5, FALSE, 50000, 0, 0
	},
	{
		"A line of sunlight appears.",
		"beam of sunlight (%sd8)",
		"lite_line(dir, damroll(%s, 8))",
		100, TRUE, 100, 10, 4	/* lev / 10 + 4 */
	},
	{
		"The %v glow extremely brightly...",
		"magic missile (%sd6)",
		"fire_bolt(GF_MISSILE, dir, damroll(%s, 6))",
		100, TRUE, 12, 10, 2	/* lev / 10 + 2 */
	},
	{
		"The %v throbs deep green...",
		"stinking cloud (%s)",
		"fire_ball(GF_POIS, dir, %s, 2)",
		50, TRUE, 5, 100, 10	/* lev + 10 */
	},
	{
		"The %v glows black...",
		"drain life (%s)",
		"drain_life(dir, %s)",
		25, TRUE, 10, 500, 0	/* lev * 5 */
	},
	{
		"The %v throbs red...",
		"vampiric drain (%s)",
		"drain_gain_life(dir, %s)",
		25, TRUE, 15, 500, 0	/* lev * 5 */
	},
	{
		"The %v grows magical spikes...",
		"arrows (%s)",
		"fire_bolt(GF_ARROW, dir, %s)",
		10, TRUE, 10, 500, 0	/* lev * 5 */
	},
	{
		"The %v grows magical spikes...",
		"arrows (%sd25)",
		"fire_bolt(GF_ARROW, dir, damroll(%s, 25))",
		5, TRUE, 125, 33, 1	/* lev / 3 + 1 */
	},
	{
		"The %v grows magical spikes...",
		"arrows (%sd50)",
		"fire_bolt(GF_ARROW, dir, damroll(%s, 50))",
		5, TRUE, 250, 33, 1	/* lev / 3 + 1 */
	},
	{
		"You launch a rocket!",
		"launch rocket (%s)",
		"fire_ball(GF_ROCKET, dir, %s, 2)",
		5, TRUE, 20, 500, 100	/* rlev * 5 + 100 */
	},
	{
		"The %v floods the area with goodness...",
		"dispel evil (%s)",
		"dispel_evil(%s)",
		50, FALSE, 100, 500, 0	/* rlev * 5 */
	},
	{
		"The %v floods the area with evil...",
		"dispel good (%s)",
		"dispel_good(%s)",
		25, FALSE, 100, 500, 0	/* rlev * 5 */
	},
	{
		/* Note that this is more powerful than a normal breath activation */
		"You breathe the elements.",
		"breathe the elements (%s, rad. 4)",
		"fire_ball(GF_MISSILE, dir, %s, 4)",
		5, TRUE, 40, 1000, 0	/* rlev * 10 */
	},
	{
		"The %v vibrates...",
		"earthquake (rad. %s)",
		"earthquake(px, py, %s)",
		10, FALSE, 250, 25, 5	/* lev / 4 + 5 */
	},
	{
		"The %v emits a loud blast...",
		"terror",
		"turn_monsters(40 + player.lev)",
		10, FALSE, 2500, 0, 0
	},
	{
		"You summon a beast.",
		"summon animal",
		"summon_controlled(SUMMON_ANIMAL_RANGER)",
		25, FALSE, 10000, 0, 0
	},
	{
		"You summon a phantasmal servant.",
		"summon phantasmal servant",
		"summon_controlled(SUMMON_PHANTOM)",
		25, FALSE, 10000, 0, 0
	},
	{
		"You summon an elemental.",
		"summon elemental",
		"summon_unsafe(SUMMON_ELEMENTAL)",
		25, FALSE, 25000, 0, 0
	},
	{
		"Ancient, long-dead forms rise from the ground.",
		"summon undead",
		"summon_unsafe(SUMMON_UNDEAD)",
		25, FALSE, 25000, 0, 0
	},
	{
		"Ancient, long-dead forms rise from the ground.",
		"summon greater undead",
		"summon_unsafe(SUMMON_HI_UNDEAD)",
		10, FALSE, 50000, 0, 0
	},
	{
		"The area fills with the stench of sulphur and brimstone.",
		"summon demon",
		"summon_unsafe(SUMMON_DEMON)",
		25, FALSE, 25000, 0, 0
	},
	{
		"The area fills with brilliant white light.",
		"summon angel",
		"summon_controlled(SUMMON_ANGEL)",
		10, FALSE, 50000, 0, 0
	},
	{
		NULL,
		"remove fear and heal (%sd10)",
		"clear_afraid(); hp_player(damroll(%s, 10))",
		25, FALSE, 100, 33, 1	/* lev / 3 + 1 */
	},
	{
		NULL,
		"cure wounds and heal (%sd10)",
		"hp_player(damroll(%s, 10)); clear_cut(); clear_stun()",
		100, FALSE, 100, 100, 0	/* lev */
	},
	{
		NULL,
		"cure wounds and heal (%s)",
		"hp_player(%s); clear_cut(); clear_stun()",
		100, FALSE, 20, 1000, 0	/* lev * 10 */
	},
	{
		"The %v enters your thoughts...",
		"telepathy (%s+ turns)",
		"inc_tim_esp(rand_range2(%s))",
		25, FALSE, 200, 100, 0	/* lev */
	},
	{
		NULL,
		"heroism (%s+ turns)",
		"inc_hero(rand_range2(%s))",
		25, FALSE, 150, 100, 0	/* lev */
	},
	{
		NULL,
		"berserk (%s+ turns)",
		"inc_shero(rand_range2(%s))",
		25, FALSE, 150, 100, 0	/* lev */
	},
	{
		NULL,
		"bless (%s+ turns)",
		"inc_blessed(rand_range2(%s))",
		25, FALSE, 100, 100, 0	/* lev */
	},
	{
		NULL,
		"protection from evil (%s+ turns)",
		"inc_protevil(rand_range2(%s))",
		25, FALSE, 100, 200, 0	/* lev * 2 */
	},
	{
		"The %v glows many colours...",
		"resist elements (%s+ turns)",
		"inc_oppose_all(rand_range2(%s))",
		25, FALSE, 250, 100, 0	/* lev */
	},
	{
		NULL,
		"resist acid (%s+ turns)",
		"inc_oppose_acid(rand_range2(%s))",
		10, FALSE, 100, 100, 0	/* lev */
	},
	{
		NULL,
		"resist lightning (%s+ turns)",
		"inc_oppose_elec(rand_range2(%s))",
		10, FALSE, 100, 100, 0	/* lev */
	},
	{
		NULL,
		"resist fire (%s+ turns)",
		"inc_oppose_fire(rand_range2(%s))",
		10, FALSE, 100, 100, 0	/* lev */
	},
	{
		NULL,
		"resist cold (%s+ turns)",
		"inc_oppose_cold(rand_range2(%s))",
		10, FALSE, 100, 100, 0	/* lev */
	},
	{
		NULL,
		"resist poison (%s+ turns)",
		"inc_oppose_poison(rand_range2(%s))",
		5, FALSE, 100, 100, 0	/* lev */
	},
	{
		NULL,
		"speed (%s+ turns)",
		"inc_fast(rand_range2(%s))",
		25, FALSE, 200, 100, 0	/* lev */
	},
	{
		"The %v fades out...",
		"wraith form (%s+ turns)",
		"inc_wraith_form(rand_range2(%s))",
		5, FALSE, 2000, 33, 1	/* lev / 3 + 1 */
	},
	{
		"The %v fires a beam of bright light at you...",
		"invulnerability (%s+ turns)",
		"inc_invuln(rand_range2(%s))",
		5, FALSE, 2000, 10, 3	/* lev / 10 + 3 */
	},
	{
		"The %v wells with clear light...",
		"light area (2d20)",
		"lite_area(damroll(2, 20), 3)",
		100, FALSE, 100, 0, 0
	},
	{
		"The %v shines brightly...",
		"magic mapping and light area (5d20)",
		"map_area(); lite_area(damroll(5, 20), 3)",
		50, FALSE, 400, 0, 0
	},
	{
		NULL,
		"detect evil",
		"detect_monsters_evil()",
		25, FALSE, 1000, 0, 0
	},
	{
		NULL,
		"detect monsters",
		"detect_monsters()",
		50, FALSE, 1000, 0, 0
	},
	{
		NULL,
		"detect traps and doors",
		"detect_traps(TRUE); detect_doors(); detect_stairs()",
		50, FALSE, 1000, 0, 0
	},
	{
		NULL,
		"remove curse",
		"remove_curse()",
		10, FALSE, 25000, 0, 0
	},
	{
		NULL,
		"dispel curse",
		"remove_all_curse()",
		5, FALSE, 50000, 0, 0
	},
	{
		NULL,
		"dispel undead (%s)",
		"dispel_undead(%s)",
		25, FALSE, 50, 500, 0	/* rlev * 5 */
	},
	{
		NULL,
		"dispel demons (%s)",
		"dispel_demons(%s)",
		25, FALSE, 50, 500, 0	/* rlev * 5 */
	},
	{
		NULL,
		"dispel living (%s)",
		"dispel_living (%s)",
		5, FALSE, 200, 400, 0	/* lev * 4 */
	},
	{
		NULL,
		"dispel monsters (%s)",
		"dispel_monsters(%s)",
		5, FALSE, 250, 400, 0	/* lev * 4 */
	},
	{
		NULL,
		"slow monsters",
		"slow_monsters()",
		10, FALSE, 25000, 0, 0
	},
	{
		NULL,
		"detect objects",
		"detect_objects_normal()",
		25, FALSE, 1000, 0, 0
	},
	{
		NULL,
		"detect treasure",
		"detect_treasure(); detect_objects_gold()",
		10, FALSE, 1000, 0, 0
	},
	{
		NULL,
		"detect enchantment",
		"detect_objects_magic()",
		10, FALSE, 2500, 0, 0
	},
	{
		NULL,
		"self knowledge",
		"self_knowledge()",
		10, FALSE, 10000, 0, 0
	},
	{
		NULL,
		"teleport level",
		"teleport_player_level()",
		5, FALSE, 10000, 0, 0
	},
	{
		NULL,
		"create doors",
		"door_creation()",
		5, FALSE, 2500, 0, 0
	},
	{
		NULL,
		"create stairs",
		"stair_creation()",
		5, FALSE, 10000, 0, 0
	},
	{
		NULL,
		"alter reality",
		"alter_reality()",
		5, FALSE, 10000, 0, 0
	},
	{
		NULL,
		"polymorph self",
		"polymorph_self()",
		10, FALSE, 5000, 0, 0
	},
	{
		NULL,
		"phase door",
		"teleport_player(10)",
		100, FALSE, 100, 0, 0
	},
	{
		NULL,
		"banishment",
		"banish_monsters(200)",
		5, FALSE, 25000, 0, 0
	},

	/* XXX stun, confuse, turn, stasis */

};

static void apply_activation_power(object_type *o_ptr, cptr text, cptr desc, cptr effect, bool aimed, int pp, int level)
{
	char buf[1024];
	char text_buf[256];
	int len;
	int charge_min;

	/* Don't add a power if there already is one */
	if (FLAG(o_ptr, TR_ACTIVATE))
		return;

	/* Calculate charge time */
	if (level > 0)
		charge_min = pp / level;
	else
		charge_min = pp;

	/* Round to nice numbers */
	if (charge_min >= 1000)
		charge_min -= charge_min % 100;
	else if (charge_min >= 250)
		charge_min -= charge_min % 50;
	else if (charge_min >= 100)
		charge_min -= charge_min % 10;
	else if (charge_min >= 25)
		charge_min -= charge_min % 5;

	/* Enforce minimum & maximum charge time */
	if (charge_min < 1) charge_min = 1;
	if (charge_min > 5000) charge_min = 5000;


	/* Get a description if needed */
	if (text == NULL)
		text = activation_text[randint0(NUM_ELEMENTS(activation_text))];

	/* Get the basic name of the object in the description */
	strnfmt(text_buf, 256, text, OBJECT_FMT(o_ptr, FALSE, 0));

	/* Construct the usage script */
	len = strnfmt(buf, 1024, "msgf(\"%s\"); ", text_buf);

	if (aimed) strnfcat(buf, 1024, &len,
				"local success; local dir; "
				"success, dir = get_aim_dir(); "
				"if not success then return; end; ");

	strnfcat(buf, 1024, &len, "%s; object.timeout = rand_range(%i, %i)", effect, charge_min, charge_min * 2);

	o_ptr->trigger[TRIGGER_USE] = quark_add(buf);


	/* Description script */
	len = strnfmt(buf, 1024, "return \"%s every %i-%i turns\"", desc, charge_min, charge_min * 2);
	o_ptr->trigger[TRIGGER_DESC] = quark_fmt(buf);

	SET_FLAG(o_ptr, TR_ACTIVATE);
	o_ptr->timeout = 0;
}

static void attack_activation_power(object_type *o_ptr, int level, cptr fix_element)
{
	static char text[256];
	char effect[256] = "";
	char desc[256] = "";
	int element;
	int dice = 0;
	int sides = 1;
	int radius = 0;
	int pp = 0;	/* Charge time * level */
	bool aimed = TRUE;

	int rlev = level * rand_range(50, 150) / 100;
	if (rlev < 1) rlev = 1;

	if (fix_element == NULL)
	{
		/* Pick a low or high element */
		if (level <= randint1(60))
			element = randint0(5);
		else
			element = randint0(NUM_ELEMENTS(element_list));
	}
	else
	{
		for (element = 0; strcmp(fix_element, element_list[element]) != 0; element++)
			;
	}

	/* Describe the visual */
	strnfmt(text, 256, "The %%v %s %s...", glow_desc[randint0(NUM_ELEMENTS(glow_desc))], element_colors[element]);

	switch (randint1(10))
	{
	/* Breathe */
	case 1:
		strnfmt(text, 256, "You breathe %s.", element_names[element]);

		/* Dice, radius, and charge time */
		dice = rlev * 5;
		radius = 2 + dice / rand_range(100, 200);
		pp = dice * 10 * radius;

		/* Create the lua */
		strnfmt(desc, 256, "breathe %s (%i, rad. %i)", element_names[element], dice, radius);
		strnfmt(effect, 256, "fire_ball(%s, dir, %i, %i)", element_list[element], dice, radius);

		break;

	/* Emit a blast */
	case 2:
		strnfmt(text, 256, "The %%v emits a blast of %s...", element_names[element]);

		/* Dice, radius, and charge time */
		dice = rlev * 10;
		radius = 2 + dice / rand_range(50, 100);
		pp = dice * 2 * radius;

		aimed = FALSE;

		/* Create the lua */
		strnfmt(desc, 256, "%s blast (%i, rad. %i)", element_names[element], dice, radius);
		strnfmt(effect, 256, "fire_ball(%s, 0, %i, %i)", element_list[element], dice, radius);

		break;

	/* Fire a ball */
	case 3:
	case 4:
	case 5:
		/* Dice, radius, and charge time */
		dice = 5 * (1 + rlev / 2);
		radius = 2 + dice / rand_range(100, 200);
		pp = dice * 10 * radius;

		/* Create the lua */
		strnfmt(desc, 256, "%s%s ball (%i)", radius > 2 ? "large " : "", element_names[element], dice);
		strnfmt(effect, 256, "fire_ball(%s, dir, %i, %i)", element_list[element], dice, radius);

		break;

	/* Fire a beam */
	case 6:
		/* Dice and charge time */
		dice = 1 + rlev / 3;
		sides = rand_range(5, 8);
		pp = dice * sides * 10;

		/* Create the lua */
		strnfmt(desc, 256, "%s beam (%id%i)", element_names[element], dice, sides);
		strnfmt(effect, 256, "fire_beam(%s, dir, damroll(%i, %i))", element_list[element], dice, sides);

		break;

	/* Fire a bolt */
	default:
		/* Dice and charge time */
		dice = 1 + rlev / 2;
		sides = rand_range(5, 8);
		pp = dice * sides * 5;

		/* Create the lua */
		strnfmt(desc, 256, "%s bolt (%id%i)", element_names[element], dice, sides);
		strnfmt(effect, 256, "fire_bolt(%s, dir, damroll(%i, %i))", element_list[element], dice, sides);

		break;
	}

	apply_activation_power(o_ptr, text, desc, effect, aimed, pp, level);
}

static void misc_activation_power(object_type *o_ptr, int level, cptr fix_power)
{
	const struct randart_activation *act = NULL;
	char dice[32];
	char dice_desc[32];
	char effect[256] = "";
	char desc[256] = "";
	int pp;

	int rlev = level * rand_range(50, 150) / 100;
	if (rlev < 1) rlev = 1;

	if (!fix_power)
	{
		do
		{
			act = &randart_activations[randint0(NUM_ELEMENTS(randart_activations))];
		}
		while (act->freq < randint1(100));
	}
	else
	{
		int i;
		for (i = 0; strcmp(randart_activations[i].desc, fix_power) != 0; i++)
			;
		act = &randart_activations[i];
	}

	/* Sometimes make the activation dependent on the player's level */
	if (one_in_(10) && act->dice >= 100 && act->bonus == 0)
	{
		/* Deeper items may give better multipliers */
		int mult = act->dice * rand_range(50, 100 + 2 * rlev) / 10000;
		if (mult < 1) mult = 1;

		if (mult > 1)
		{
			strnfmt(dice, 32, "player.lev * %i", mult);
			strnfmt(dice_desc, 32, "plev * %i", mult);
		}
		else
		{
			strcpy(dice, "player.lev");
			strcpy(dice_desc, "plev");
		}

		/* Fill in the dice */
		strnfmt(desc, 256, act->desc, dice_desc);
		strnfmt(effect, 256, act->effect, dice);

		/* Assume plev 30 for power calculation */
		pp = act->pp * mult * 30;
	}
	else
	{
		int d = rlev * act->dice / 100 + act->bonus;
		if (d < 1) d = 1;

		/* Fill in the dice */
		strnfmt(dice, 32, "%i", d);
		strnfmt(desc, 256, act->desc, dice);
		strnfmt(effect, 256, act->effect, dice);

		pp = act->pp * d;
	}

	apply_activation_power(o_ptr, act->text, desc, effect, act->aimed, pp, level);
}

static void random_activation_power(object_type *o_ptr, int level)
{
	int rlev = level * rand_range(50, 150) / 100;
	if (rlev < 1) rlev = 1;

	/* 60%/20% chance of a random attack */
	if (randint0(100) < (o_ptr->tval < TV_BOOTS ? 60 : 20))
		attack_activation_power(o_ptr, level, NULL);
	else
		misc_activation_power(o_ptr, level, NULL);
}

static void get_random_name(char *return_name, byte tval, int power)
{
	if ((randint1(100) <= TABLE_NAME) ||
		(tval == TV_AMULET) || (tval == TV_RING))
	{
		get_table_name(return_name, TRUE);
	}
	else
	{
		cptr filename;

		/* Armour or a Weapon? */
		if (tval >= TV_BOOTS)
		{
			switch (power)
			{
				case 0:
					filename = "a_cursed.txt";
					break;
				case 1:
					filename = "a_low.txt";
					break;
				case 2:
					filename = "a_med.txt";
					break;
				default:
					filename = "a_high.txt";
			}
		}
		else
		{
			switch (power)
			{
				case 0:
					filename = "w_cursed.txt";
					break;
				case 1:
					filename = "w_low.txt";
					break;
				case 2:
					filename = "w_med.txt";
					break;
				default:
					filename = "w_high.txt";
			}
		}

		(void)get_rnd_line(filename, 0, return_name);
	}
}


static void curse_artifact(object_type *o_ptr)
{
	if (o_ptr->pval > 0) o_ptr->pval = 0 - (o_ptr->pval + randint1(4));
	if (o_ptr->to_a > 0) o_ptr->to_a = 0 - (o_ptr->to_a + randint1(4));
	if (o_ptr->to_h > 0) o_ptr->to_h = 0 - (o_ptr->to_h + randint1(4));
	if (o_ptr->to_d > 0) o_ptr->to_d = 0 - (o_ptr->to_d + randint1(4));

	SET_FLAG(o_ptr, TR_CURSED);

}


bool create_artifact(object_type *o_ptr, int level, bool a_scroll)
{
	char new_name[1024];
	int power_level = m_bonus(5, level)+randint1(5); /* range is 1 to 10, higher level = more likely in upper end,
														but not automatic. */
	int goodpts = 0;
	int badpts = 0;
	int target_good, target_bad, pval, i, bad;
	bool a_cursed = FALSE;
	int misses = 0;

	/* Check if random artifacts are allowed; doesn't apply to scrolls. */
	if (!allow_randart && !a_scroll)  return (FALSE);

	/* MEGA hack: Don't allow artifacts to be created in "rand_quick" mode */
	if (Rand_quick) return (FALSE);

	/* No activation yet */
	o_ptr->a_idx = 0;

	new_name[0] = 0;

	if (!a_scroll && one_in_(A_CURSED))
		a_cursed = TRUE;

	/* determine targets for good and bad powers */
	/* halve the good power level if randart_weak option chosen */
	if (power_level < 8)
	{
		target_good = 10+power_level*8 + randint1(8);
		target_good = (randart_weak ? 1+(target_good)/2 : target_good);
		target_bad = 0;
	} else if (power_level < 10) {
		target_good = power_level*12 - 18 + randint1(12);
		while(one_in_(3)) target_good += randint1(6);
		target_good = (randart_weak ? 1+(target_good)/2 : target_good);
		target_bad = (target_good > 100 ? target_good-100 : 0);
		target_bad /= 2;
	} else {
		target_good = 105 + damroll(3,10);
		while(one_in_(3)) target_good += randint1(10);
		target_good = (randart_weak ? 1+(target_good)/2 : target_good);
		target_bad = (target_good-100)/2;
		target_bad = (target_bad > 10 ? 10+(target_bad-10)/6 : target_bad);
	}

	if (a_cursed)
	{
		target_good /= 2;
		target_bad += 10+power_level;
	}

	goodpts = target_good;
	badpts = target_bad;

	/* Hack: Lights get LITE for free */
	if (o_ptr->tval == TV_LITE)
	{
		SET_FLAG(o_ptr, TR_LITE);
	}

	/* Just to be sure */
	o_ptr->flags[2] |= (TR2_IGNORE_ACID | TR2_IGNORE_ELEC |
					  TR2_IGNORE_FIRE | TR2_IGNORE_COLD | TR2_HIDDEN_POWERS);

	/* Determine activation */
	if (!a_cursed && one_in_((o_ptr->tval >= TV_BOOTS)
							 ? ACTIVATION_CHANCE * 2 : ACTIVATION_CHANCE))
	{
		int activation_level = level + randint0(10);

		if (one_in_(5))
			activation_level = activation_level * 3 / 2;

		if (activation_level > 100) activation_level = 100;

		/*
		 * Get a random activation
		 */
		random_activation_power(o_ptr, activation_level);
		/* Mega-hack: dumb way of counting it. */
		goodpts -= 8;
	}

	/* Choose a pval.  Will set back to 0 if irrelevant, later.  */
	i = randint1(100);

	if (i <= 5)
		o_ptr->pval = 0;
	else if (i <= 40)
		o_ptr->pval = 1;
	else if (i <= 70)
		o_ptr->pval = 2;
	else if (i <= 85)
		o_ptr->pval = 3;
	else if (i <= 99)
		o_ptr->pval = 4;
	else
		o_ptr->pval = 5;

	pval = o_ptr->pval;

	/* Good power loop */
	while (misses < goodpts)
	{
		int v = rand_range((o_ptr->tval < TV_BOOTS ? 1 : 21), 100);
		long flag = 0;
		int flagslt=0;
		int gpts = 0;
		int bpts = 0;

		if (v <= 10) {
			random_brand(o_ptr, &gpts, &bpts, &flag, &flagslt);
		} else if (v <= 24) {
			if (o_ptr->tval == TV_BOW && v <= 20)  /* Slays should be rare for bows. */
			{
				random_bow_ability(&gpts, &flag, &flagslt);
			} else {
				random_slay(o_ptr, &gpts, &flag, &flagslt);
			}
		} else if (v <= 32) {
			random_resistance(&gpts, &flag, &flagslt, TRUE);
		} else if (v <= 45) {
			random_resistance(&gpts, &flag, &flagslt, FALSE);
		} else if (v <= 52) {
			random_plus(o_ptr, &gpts, &flag, &flagslt, &misses, &goodpts, TRUE);
		} else if (v <= 65) {
			random_plus(o_ptr, &gpts, &flag, &flagslt, &misses, &goodpts, FALSE);
		} else if (v <= 70) {
			random_misc(o_ptr, &gpts, &bpts, &flag, &flagslt, TRUE);
		} else if (v <= 80) {
			random_misc(o_ptr, &gpts, &bpts, &flag, &flagslt, FALSE);
		} else {
			random_themed(o_ptr, &gpts, &bpts, &flag, &flagslt, &misses, &goodpts);
		}

		/* See if we can apply the power */
		if (flag == 0) continue;  /* no power chosen this time */

		if (o_ptr->flags[flagslt] & flag) continue;  /* chosen power overlaps existing ones */

		/* Special cases */
		if (FLAG(o_ptr, TR_SLAY_DRAGON) && flag == TR0_KILL_DRAGON && flagslt == 0) continue;
		if (FLAG(o_ptr, TR_KILL_DRAGON) && flag == TR0_SLAY_DRAGON && flagslt == 0) continue;
		if (FLAG(o_ptr, TR_IM_FIRE) && flag == TR1_RES_FIRE && flagslt == 1) continue;
		if (FLAG(o_ptr, TR_IM_COLD) && flag == TR1_RES_COLD && flagslt == 1) continue;
		if (FLAG(o_ptr, TR_IM_ACID) && flag == TR1_RES_ACID && flagslt == 1) continue;
		if (FLAG(o_ptr, TR_IM_ELEC) && flag == TR1_RES_ELEC && flagslt == 1) continue;
		if (FLAG(o_ptr, TR_IM_POIS) && flag == TR1_RES_POIS && flagslt == 1) continue;
		if (FLAG(o_ptr, TR_IM_LITE) && flag == TR1_RES_LITE && flagslt == 1) continue;
		if (FLAG(o_ptr, TR_IM_DARK) && flag == TR1_RES_DARK && flagslt == 1) continue;

		/* Only other way to not apply the power is if gpts was too high. */
		if (gpts > goodpts) {
			misses++;
		} else {
			o_ptr->flags[flagslt] |= flag;
			goodpts -= gpts;
			badpts -= bpts;
		}
	}

	/* remove pval if irrelevant */
	if (!FLAG(o_ptr, TR_PVAL_MASK))
		o_ptr->pval = 0;

	/* give it some plusses... */
	if (o_ptr->tval >= TV_BOOTS && o_ptr->tval < TV_LITE)
		o_ptr->to_a += 9 + randint1(3);
	else if (o_ptr->tval < TV_BOOTS)
	{
		o_ptr->to_h += 9 + randint1(3);
		o_ptr->to_d += 9 + randint1(3);
	}

	bad = target_bad - badpts;

	/* Curse loop */
	while (badpts > 0) {
		i = random_curse(o_ptr, FALSE);
		bad += i;
		badpts -= i;
	}

	if (target_good >= 90 && one_in_(12) && !a_scroll)
		(void)random_curse(o_ptr, TRUE);




	if (cheat_peek) msgf("Artifact: [%d:%d]/[%d:%d]", target_good, target_good-goodpts, target_bad, bad);

	if (a_cursed) curse_artifact(o_ptr);

	if (a_cursed) power_level = 0;
	else if (target_good < 30) power_level = 1;
	else if (target_good < 70) power_level = 2;
	else power_level = 3;

	/* If this non-weapon has the show_mod flag */
	if (o_ptr->tval >= TV_BOOTS &&
		o_ptr->tval <= TV_RING &&
		FLAG(o_ptr, TR_SHOW_MODS))
	{
		/* Check if it is not accidentally zero */
		if (!o_ptr->to_h && !o_ptr->to_d)
		{
			/* Turn of that flag, no silly (+0, +0%) display */
			o_ptr->flags[2] &= ~(TR2_SHOW_MODS);
		}
	}

	if (a_scroll)
	{
		char dummy_name[80];
		dummy_name[0] = 0;

		/* Identify it fully */
		object_aware(o_ptr);
		object_known(o_ptr);
		object_mental(o_ptr);

		/* Save all the known flags */
		o_ptr->kn_flags[0] = o_ptr->flags[0];
		o_ptr->kn_flags[1] = o_ptr->flags[1];
		o_ptr->kn_flags[2] = o_ptr->flags[2];
		o_ptr->kn_flags[3] = o_ptr->flags[3];

		identify_fully_aux(o_ptr);

		if (!(get_string(dummy_name, 80,
        				 "What do you want to call the artifact? ")))
		{
			get_random_name(new_name, o_ptr->tval, power_level);
		}
		else
		{
			strnfmt(new_name, 1024, "'%s'", dummy_name);
		}
		chg_virtue(V_INDIVIDUALISM, 2);
		chg_virtue(V_ENCHANT, 5);
	}
	else
	{
		get_random_name(new_name, o_ptr->tval, power_level);
	}

	/* Save the inscription */
	o_ptr->xtra_name = quark_add(new_name);

	/* Make the object an artifact */
	SET_FLAG(o_ptr, TR_INSTA_ART);

	/* Set the cost */
	o_ptr->cost = k_info[o_ptr->k_idx].cost + flag_cost(o_ptr, o_ptr->pval);

	/* Notice changes */
	notice_item();

	return TRUE;
}

/*
 * Create the artifact of the specified number
 */
void create_named_art(int a_idx, int x, int y)
{
	object_type *q_ptr;
	int i;

	artifact_type *a_ptr = &a_info[a_idx];

	/* Ignore "empty" artifacts */
	if (!a_ptr->name) return;

	/* Don't create a second one */
	if (a_ptr->cur_num) return;

	/* Acquire the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!i) return;

	/* Create the artifact */
	q_ptr = object_prep(i);

	/* Save the artifact number */
	q_ptr->a_idx = a_idx;

	/* Add any special scripts */
	for (i = 0; i < MAX_TRIGGER; i++)
	{
		if (a_ptr->trigger[i])
			q_ptr->trigger[i] = quark_add(a_text + a_ptr->trigger[i]);
	}

	/* Do not make another one */
	a_ptr->cur_num = 1;

	/* Save the artifact flags */
	q_ptr->flags[0] |= a_ptr->flags[0];
	q_ptr->flags[1] |= a_ptr->flags[1];
	q_ptr->flags[2] |= a_ptr->flags[2] | TR2_HIDDEN_POWERS;
	q_ptr->flags[3] |= a_ptr->flags[3];

	/* Extract the fields */
	q_ptr->pval = a_ptr->pval;
	q_ptr->ac = a_ptr->ac;
	q_ptr->dd = a_ptr->dd;
	q_ptr->ds = a_ptr->ds;
	q_ptr->to_a = a_ptr->to_a;
	q_ptr->to_h = a_ptr->to_h;
	q_ptr->to_d = a_ptr->to_d;
	q_ptr->weight = a_ptr->weight;

	/* Save the inscription */
	q_ptr->xtra_name = quark_add(a_name + a_ptr->name);

	/* Apply special scripts */
	apply_object_trigger(TRIGGER_MAKE, q_ptr, "i", "lev", a_ptr->level);

	if (!a_ptr->cost)
	{
		/* Hack -- "worthless" artifacts */
		q_ptr->cost = 0L;
	}
	else
	{
		/* Hack - use the artifact price */
		q_ptr->cost = k_info[q_ptr->k_idx].cost + a_ptr->cost;
	}

	/* Drop the artifact from heaven */
	drop_near(q_ptr, -1, x, y);
}
