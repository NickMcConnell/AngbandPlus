#define POWERS_C
/* File: powers.c */

/* Purpose: Specific actions of objects */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

/*
 * This file contains code to perform an action appropriate to a specific
 * object, spell, etc.. Code which is specific to a particular access method
 * should go elsewhere, although there are some fairly generic access functions
 * here.
 *
 * N.B. The effects here have not been tested more widely than they are used
 * here. Care must therefore be taken when calling a power via a new
 * mechanism to ensure that it uses I/O, etc., as the new caller expects.
 */

#include "angband.h"

typedef const struct add_timed_type add_timed_type;
struct add_timed_type
{
	s16b power;
	s16b flag;
	s16b min;
	s16b max;
	bool resist; /* Certain resistances cancel this altogether. */
};

static add_timed_type power_add_timed_table[] =
{
	{OBJ_FOOD_POISON+PO_K_IDX, TIMED_POISONED, 10, 19, FALSE},
	{OBJ_FOOD_BLINDNESS+PO_K_IDX, TIMED_BLIND, 200, 399, FALSE},
	{OBJ_FOOD_PARANOIA+PO_K_IDX, TIMED_AFRAID, 10, 19, FALSE},
	{OBJ_FOOD_CONFUSION+PO_K_IDX, TIMED_CONFUSED, 10, 19, FALSE},
	{OBJ_FOOD_HALLUCINATION+PO_K_IDX, TIMED_IMAGE, 250, 499, FALSE},
	{OBJ_FOOD_PARALYSIS+PO_K_IDX, TIMED_PARALYZED, 10, 19, FALSE},
	{OBJ_POTION_SLOWNESS+PO_K_IDX, TIMED_SLOW, 15, 39, FALSE},
	{OBJ_POTION_POISON+PO_K_IDX, TIMED_POISONED, 10, 24, TRUE},
	{OBJ_POTION_BLINDNESS+PO_K_IDX, TIMED_BLIND, 100, 199, TRUE},
	{OBJ_POTION_SLEEP+PO_K_IDX, TIMED_PARALYZED, 4, 7, TRUE},
	{OBJ_POTION_INFRA_VISION+PO_K_IDX, TIMED_INFRA, 100, 199, FALSE},
	{OBJ_POTION_DETECT_INVIS+PO_K_IDX, TIMED_INVIS, 12, 23, FALSE},
	{OBJ_POTION_RES_HEAT+PO_K_IDX, TIMED_OPPOSE_FIRE, 11, 20, FALSE},
	{OBJ_POTION_RES_COLD+PO_K_IDX, TIMED_OPPOSE_COLD, 11, 20, FALSE},
	{OBJ_POTION_RESISTANCE, TIMED_OPPOSE_ACID, 21, 40, FALSE},
	{OBJ_POTION_RESISTANCE, TIMED_OPPOSE_ELEC, 21, 40, FALSE},
	{OBJ_POTION_RESISTANCE, TIMED_OPPOSE_FIRE, 21, 40, FALSE},
	{OBJ_POTION_RESISTANCE, TIMED_OPPOSE_COLD, 21, 40, FALSE},
	{OBJ_POTION_RESISTANCE, TIMED_OPPOSE_POIS, 21, 40, FALSE},
	{ART_BARZAI+PO_NAME1, TIMED_OPPOSE_ACID, 21, 40, FALSE},
	{ART_BARZAI+PO_NAME1, TIMED_OPPOSE_ELEC, 21, 40, FALSE},
	{ART_BARZAI+PO_NAME1, TIMED_OPPOSE_FIRE, 21, 40, FALSE},
	{ART_BARZAI+PO_NAME1, TIMED_OPPOSE_COLD, 21, 40, FALSE},
	{ART_BARZAI+PO_NAME1, TIMED_OPPOSE_POIS, 21, 40, FALSE},
	{OBJ_POTION_INVULNERABILITY+PO_K_IDX, TIMED_INVULN, 8, 14, FALSE},
	{OBJ_SCROLL_BLESSING+PO_K_IDX, TIMED_BLESSED, 7, 18, FALSE},
	{OBJ_SCROLL_HOLY_CHANT+PO_K_IDX, TIMED_BLESSED, 13, 36, FALSE},
	{OBJ_SCROLL_HOLY_PRAYER+PO_K_IDX, TIMED_BLESSED, 25, 72, FALSE},
	{OBJ_STAFF_SLOWNESS+PO_K_IDX, TIMED_SLOW, 16, 45, FALSE},
	{OBJ_RING_ACID+PO_K_IDX, TIMED_OPPOSE_ACID, 21, 40, FALSE},
	{OBJ_RING_ICE+PO_K_IDX, TIMED_OPPOSE_COLD, 21, 40, FALSE},
	{OBJ_RING_FIRE+PO_K_IDX, TIMED_OPPOSE_FIRE, 21, 40, FALSE},
	{ACT_ESP+PO_ACTIVATION, TIMED_ESP, 26, 55, FALSE},
	{ACT_BERSERK+PO_ACTIVATION, TIMED_SHERO, 51, 100, FALSE},
	{ACT_BERSERK+PO_ACTIVATION, TIMED_BLESSED, 51, 100, FALSE},
	{ACT_INVULN+PO_ACTIVATION, TIMED_INVULN, 9, 16, FALSE},
	{SP_BLESS, TIMED_BLESSED, 13, 24, FALSE},
	{SP_SENSE_UNSEEN, TIMED_INVIS, 25, 48, FALSE},
	{SP_PRAYER, TIMED_BLESSED, 49, 96, FALSE},
	{SP_HOLY_INVULNERABILITY, TIMED_INVULN, 8, 14, FALSE},
	{SP_RESIST_ENVIRONMENT, TIMED_OPPOSE_COLD, 21, 40, FALSE},
	{SP_RESIST_ENVIRONMENT, TIMED_OPPOSE_FIRE, 21, 40, FALSE},
	{SP_RESIST_ENVIRONMENT, TIMED_OPPOSE_ELEC, 21, 40, FALSE},
	{SP_STONE_SKIN, TIMED_SHIELD, 31, 50, FALSE},
	{SP_RESISTANCE_TRUE, TIMED_OPPOSE_ACID, 21, 40, FALSE},
	{SP_RESISTANCE_TRUE, TIMED_OPPOSE_ELEC, 21, 40, FALSE},
	{SP_RESISTANCE_TRUE, TIMED_OPPOSE_FIRE, 21, 40, FALSE},
	{SP_RESISTANCE_TRUE, TIMED_OPPOSE_COLD, 21, 40, FALSE},
	{SP_RESISTANCE_TRUE, TIMED_OPPOSE_POIS, 21, 40, FALSE},
	{SP_SENSE_MINDS, TIMED_ESP, 26, 55, FALSE},
	{SP_GLOBE_OF_INVULNERABILITY, TIMED_INVULN, 9, 16, FALSE},
	{SP_PLANAR_SPYING, TIMED_ESP, 26, 55, FALSE},
	{SP_RESIST_POISON, TIMED_OPPOSE_POIS, 21, 40, FALSE},
	{SP_RESIST_COLD, TIMED_OPPOSE_COLD, 21, 40, FALSE},
	{SP_RESIST_FIRE, TIMED_OPPOSE_FIRE, 21, 40, FALSE},
	{SP_RESIST_ACID, TIMED_OPPOSE_ACID, 21, 40, FALSE},
	{SP_RESIST_LIGHTNING, TIMED_OPPOSE_ELEC, 21, 40, FALSE},
	{SP_SEE_INVISIBLE, TIMED_INVIS, 25, 48, FALSE},
};

static cptr timer_verbs[TIMED_MAX] =
{
	"blinds you",
	"confuses you",
	"poisons you",
	"scares you",
	"paralyses you",
	"gives you hallucinations",
	"speeds you",
	"slows you",
	"protects you from harm",
	"blesses you",
	"makes you heroic",
	"makes you berserk",
	"protects you from evil",
	"makes you incorporeal",
	"makes you invulnerable",
	"makes you telepathic",
	"lets you see invisible things",
	"lets you see infra-red light",
	"protects you from acid",
	"protects you from electricity",
	"protects you from fire",
	"protects you from cold",
	"protects you from poison",
	"stuns you",
	"cuts you",
	"feeds you",
	"TIMED_VAMP"
};


typedef const struct project_ball_type project_ball_type;
struct project_ball_type
{
	int power;
	int type;
	int dam;
	int rad;
	cptr str;
};

static project_ball_type power_project_ball_table[] =
{
	{OBJ_WAND_STINKING_CLOUD+PO_K_IDX, GF_POIS, 12, 2, NULL},
	{OBJ_WAND_ACID_BALL+PO_K_IDX, GF_ACID, 60, 2, NULL},
	{OBJ_WAND_ELEC_BALL+PO_K_IDX, GF_ELEC, 32, 2, NULL},
	{OBJ_WAND_FIRE_BALL+PO_K_IDX, GF_FIRE, 72, 2, NULL},
	{OBJ_WAND_COLD_BALL+PO_K_IDX, GF_COLD, 48, 2, NULL},
	{OBJ_WAND_DRAGON_FIRE+PO_K_IDX, GF_FIRE, 100, 3, NULL},
	{OBJ_WAND_DRAGON_COLD+PO_K_IDX, GF_COLD, 80, 3, NULL},
	{OBJ_WAND_DRAGON_BREATH+PO_K_IDX, GF_ACID, 100, -3, NULL},
	{OBJ_WAND_DRAGON_BREATH+PO_K_IDX, GF_ELEC, 80, -3, NULL},
	{OBJ_WAND_DRAGON_BREATH+PO_K_IDX, GF_FIRE, 100, -3, NULL},
	{OBJ_WAND_DRAGON_BREATH+PO_K_IDX, GF_COLD, 80, -3, NULL},
	{OBJ_WAND_DRAGON_BREATH+PO_K_IDX, GF_POIS, 60, -3, NULL},
	{OBJ_ROD_ACID_BALL+PO_K_IDX, GF_ACID, 60, 2, NULL},
	{OBJ_ROD_ELEC_BALL+PO_K_IDX, GF_ELEC, 32, 2, NULL},
	{OBJ_ROD_FIRE_BALL+PO_K_IDX, GF_FIRE, 72, 2, NULL},
	{OBJ_ROD_COLD_BALL+PO_K_IDX, GF_COLD, 48, 2, NULL},
	{ACT_BA_POIS_1+PO_ACTIVATION, GF_POIS, 12, 3, NULL},
	{ACT_BA_COLD_1+PO_ACTIVATION, GF_COLD, 48, 2, NULL},
	{ACT_BA_FIRE_1+PO_ACTIVATION, GF_FIRE, 72, 2, NULL},
	{ACT_BA_COLD_2+PO_ACTIVATION, GF_COLD, 100, 2, NULL},
	{ACT_BA_ELEC_2+PO_ACTIVATION, GF_ELEC, 100, 3, NULL},
	{ACT_BA_FIRE_2+PO_ACTIVATION, GF_FIRE, 120, 3, NULL},
	{ACT_BA_COLD_3+PO_ACTIVATION, GF_COLD, 200, 3, NULL},
	{ACT_BA_ELEC_3+PO_ACTIVATION, GF_ELEC, 250, 3, NULL},
/* {ACT_BA_MISS_3+PO_ACTIVATION, GF_MISSILE, 300, -4, NULL}, */ /* Strange message order. */
	{OBJ_RING_ACID+PO_K_IDX, GF_ACID, 50, 2, NULL},
	{OBJ_RING_ICE+PO_K_IDX, GF_COLD, 50, 2, NULL},
	{OBJ_RING_FIRE+PO_K_IDX, GF_FIRE, 50, 2, NULL},
	{ART_ELEMFIRE+PO_NAME1, GF_FIRE, 120, 3, NULL},
	{ART_ELEMICE+PO_NAME1, GF_COLD, 200, 3, NULL},
	{ART_ELEMSTORM+PO_NAME1, GF_ELEC, 250, 3, NULL},
	{ART_THOTH+PO_NAME1, GF_POIS, 12, 3, NULL},
	{ART_ICICLE+PO_NAME1, GF_COLD, 48, 2, NULL},
	{ART_STARLIGHT+PO_NAME1, GF_COLD, 100, 2, NULL},
	{ART_EVERFLAME+PO_NAME1, GF_FIRE, 72, 2, NULL},
	{ART_ODIN+PO_NAME1, GF_ELEC, 100, 3, NULL},
	{ART_FIRESTAR+PO_NAME1, GF_FIRE, 72, 3, NULL},
	{SP_DARKNESS_STORM, GF_DARK, 120, 4, NULL},
};

/*
 * Return 0 if the TIMED_* flag cannot be increased, 1 if it can be at present,
 * and -1 if it can always be increased (except in cases of overflow).
 */
static int allow_timer(int flag)
{
	switch (flag)
	{
		case TIMED_POISONED: return !p_ptr->resist_pois && !p_ptr->oppose_pois;
		case TIMED_BLIND: return !p_ptr->resist_blind;
		case TIMED_PARALYZED: return !p_ptr->free_act;
		default: return -1;
	}
}

static errr power_add_timed(int power, bool *ident)
{
	add_timed_type *ptr;
	errr err = POWER_ERROR_NO_SUCH_POWER;
	FOR_ALL_IN(power_add_timed_table, ptr)
	{
		if (ptr->power == power)
		{
			if (!ptr->resist || allow_timer(ptr->flag))
			{
				(*ident) = add_flag(ptr->flag, rand_range(ptr->min, ptr->max));
			}
			err = SUCCESS;
		}
	}
	return err;
}

/*
 * Combine a list of strings as "init str[0], str[1], ..., conj str[total-1]."
 */
static cptr list_timers(cptr init, cptr conj, cptr *str, int total)
{
	char buf[1024];
	int i, l = 0;
	const int maxlen = sizeof(buf)-1;
	cptr end;

	assert(init && conj);

	/* Nothing to do. */
	if (!total) return 0;

	l = sprintf(buf+0, "%.*s ", maxlen-0, init);

	for (i = 0; i < total; i++)
	{
		if (i == total-1) end = ".";
		else if (i == total-2) end = conj;
		else end = ", ";

		l += sprintf(buf+l, "%.*s", maxlen-l, str[i]);

		l += sprintf(buf+l, "%.*s", maxlen-l, end);
	}

	return format(buf);
}

/* Display the period of this effect. Currently unused. */
/* #define SHOW_TIMES */

/*
 * Put a string in str which represents the whole (possible nested) phrase.
 * Create arrays to call itself on a subset if needed.
 *
 * This should call list_timers for each section of the string, but it isn't
 * needed at present.
 * It should also sort the strings initially to limit redundancy.
 */
static cptr timers_to_strings(add_timed_type **tim, int total)
{
	int i, j, k, match_p[10], match_s[10];
#ifdef SHOW_TIMES
	bool match_n[10];
#endif

	char buf[10][80];
	cptr str[10];

	for (i = 0; i < 10; i++) str[i] = buf[i];
	for (i = 0; i < 10; i++) strcpy(buf[i], "!!");

	assert(total <= 10);

	/* Boring... */
	if (!total) return 0;

	for (i = 0; i < total-1; i++)
	{
		add_timed_type *t1 = tim[i], *t2 = tim[i+1];
		cptr s1 = timer_verbs[t1->flag];
		cptr s2 = timer_verbs[t2->flag];
		cptr e1 = strchr(s1, '\0');
		cptr e2 = strchr(s2, '\0');
		cptr v1, v2;

#ifdef SHOW_TIMES
		/* Notice if the numbers match. */
		match_n[i] = (t1->min == t2->min && t1->max == t2->max);
#endif

		/* Notice an initial matching section. */
		for (v1 = s1, v2 = s2; *v1 && *v1 == *v2; v1++, v2++) ;
		match_p[i] = v1-s1;

		/* Notice a final matching section (ignoring the \0). */
		for (v1 = e1, v2 = e2; e1 != s1 && e2 != s2 && *e1 == *e2; e1--, e2--) ;
		match_s[i] = MAX(0, e1-v1-1);
	}

	match_p[total-1] = 0;
	match_s[total-1] = 0;
#ifdef SHOW_TIMES
	match_n[total-1] = FALSE;
#endif

	for (j = 0; j < total; j = i)
	{
		for (i = j+1; i < total; i++)
		{
#ifdef SHOW_TIMES
			if (!match_n[i-1]) break;
#endif
			if (match_p[i-1] != match_p[j]) break;
			if (match_s[i-1] != match_s[j]) break;
		}

		for (k = j; k < i; k++)
		{
			add_timed_type *t_ptr = tim[k];
			cptr s = timer_verbs[t_ptr->flag];
			int len = strlen(s);

			if (k != j)
			{
				len -= match_p[j];
				s += match_p[j];
			}
#ifdef SHOW_TIMES
			if (k != i-1)
#endif

			{
				len -= match_s[j];
				strnfmt(buf[k], sizeof(buf[k]), "%.*s", len, s);
			}
#ifdef SHOW_TIMES
			else
			{
				strnfmt(buf[k], sizeof(buf[k]),
					"%.*s for %d-%d turns", len, s, t_ptr->min, t_ptr->max);
			}
#endif
		}
	}
	return list_timers("It", " and ", str, total);
}


/*
 * Fire a ball with fixed damage and radius at something.
 * The fixed restriction does mean that this is unsuitable for most spells.
 */
static errr power_project_ball(int power, int dir, bool *ident)
{
	int i = 0;
	project_ball_type *ptr;

	/* Require a direction. */
	if (!dir) return POWER_ERROR_NO_SUCH_DIR;

	FOR_ALL_IN(power_project_ball_table, ptr)
	{
		if (ptr->power == power) i++;
	}

	/* Choose a random attack from the alternatives offered. */
	i = rand_int(i);

	FOR_ALL_IN(power_project_ball_table, ptr)
	{
		/* Not the right power. */
		if (ptr->power != power) continue;

		/* Not the chosen attack. */
		if (i--) continue;

		/* Print a message, if necessary. */
		if (ptr->str) msg_print(ptr->str);

		/* Fire it. */
		fire_ball(ptr->type, dir, ptr->dam, ptr->rad);
		*ident = TRUE;
		return SUCCESS;
	}

	return POWER_ERROR_NO_SUCH_POWER;
}

/*
 * Convert a list of possible ball spells into a descriptive list.
 * This doesn't mention damage or radius.
 */
static cptr balls_to_strings(project_ball_type **table, int total)
{
	int i;
	cptr str[10], pre, s;
	assert(total >= 0 && total <= 10);
	if (!total) return 0;

	for (i = 0; i < total; i++)
	{
		str[i] = string_make(format("%s (%d, rad %d)",
			lookup_gf(table[i]->type)->desc, table[i]->dam, table[i]->rad));
	}
	if (table[0]->rad < 0) pre = "It fires a breath of";
	else pre = "It fires a ball of";

	/* Format the list as a sentence. */
	s = list_timers(pre, " or ", str, total);

	/* Clean up. */
	for (i = 0; i < total; i++) FREE(str[i]);

	return s;
}

/*
 * Describe an add_timed_type power.
 * This should have a format something like "It protects you from A for B-C
 * turns, protects you from D and E for F-G turns and protects you from H and
 * Is you for J-K turns."
 *
 * It doesn't handle that correctly, but does give good enough descriptions of
 * the powers actually used, so it's good enough for now.
 */
static cptr power_describe_timer(int power)
{
	add_timed_type *ptr, *a[10];
	int j = 0;

	FOR_ALL_IN(power_add_timed_table, ptr)
	{
		if (ptr->power == power)
		{
			assert(j < 10);
			a[j++] = ptr;
		}
	}
	return timers_to_strings(a, j);
}

static cptr power_describe_ball(int power)
{
	project_ball_type *ptr, *a[10];
	int j = 0;
	FOR_ALL_IN(power_project_ball_table, ptr)
	{
		if (ptr->power == power)
		{
			assert(j < 10);
			a[j++] = ptr;
		}
	}
	return balls_to_strings(a, j);
}

/*
 * Describe various miscellaneous powers.
 */
static cptr power_describe_misc(int power, int lev)
{
	switch (power)
	{
		case SP_PRECOGNITION:
		{
			cptr board[6], init = "Detects";
			int j = 0;

			if (lev >= 45)
				return "Lights the dungeon and detects everything in it.";

			if (lev < 20) board[j++] = "monsters";
			else board[j++] = "all monsters";
			if (lev >= 5) board[j++] = "traps";
			if (lev >= 15) board[j++] = "doors";
			if (lev >= 15) board[j++] = "stairs";
			if (lev >= 20) board[j++] = "walls";
			if (lev >= 30) board[j++] = "objects";
			return list_flags(init, "and", board, j);
		}
		case SP_NEURAL_BLAST:
		{
			return "Fires a bolt of mental energy at a monster.";
		}
		case SP_MINOR_DISPLACEMENT:
		{
			if (lev < 25) return "Teleports you a short distance away.";
			else return "Teleports you to a nearby spot of your choosing.";
		}
		case SP_MAJOR_DISPLACEMENT:
		{
			if (lev < 30) return "Teleports you far away.";
			else return "Teleports you, and other nearby monsters, far away.";
		}
		case SP_DOMINATION:
		{
			if (lev < 30) return "Charms a monster.";
			else return "Charms all nearby monsters.";
		}
		case SP_PULVERISE:
		{
			return "Creates a ball of sound at a location of your choosing.";
		}
		case SP_CHARACTER_ARMOUR:
		{
			cptr board[6];
			cptr init = "Gives AC and resistance to";
			int j = 0;

			if (lev >= 15) board[j++] = "acid";
			if (lev >= 20) board[j++] = "fire";
			if (lev >= 25) board[j++] = "cold";
			if (lev >= 30) board[j++] = "electricity";
			if (lev >= 35) board[j++] = "poison";

			if (j)
				return list_flags(init, "and", board, j);
			else
				return "Helps to protect you from melee attack.";
		}
		case SP_PSYCHOMETRY:
		{
			if (lev >= 40) return "Identifies an object.";
			else return "Pseudo-identifies an object.";
		}
		case SP_MIND_WAVE:
		{
			if (lev >= 25)
				return "Fires mental energy at all visible monsters.";
			else
				return "Fires mental energy at nearby monsters.";
		}
		case SP_ADRENALINE_CHANNELING:
		{
			if (lev >= 35)
				return "Heals you, hastes you and drives you berserk.";
			else
				return "Heals you, hastes you and makes you heroic.";
		}
		case SP_PSYCHIC_DRAIN:
		{
			return "Fires mental energy at nearby monsters to gain extra chi.";
		}
		case SP_TELEKINETIC_WAVE:
		{
			return "Harms, stuns and teleports nearby monsters.";
		}
		/* None written. */
		case SP_DETECT_DOORS_AND_TRAPS: return "finds traps, doors and stairs";
		case RP_HOBBIT: return "produces food";
		case RP_GNOME: return "teleports, range LEV+1;";
		case SP_REMOVE_FEAR: return "removes fear";
		case RP_HALF_TROLL: return "enters berserk fury";
		case RP_GREAT: return "allows dream travel";
		case RP_GREAT_2: return "lets you dream a better self";
		case SP_EXPLOSIVE_RUNE: return "sets an explosive rune";
		case RP_HALF_GIANT: return "breaks stone walls";
		case RP_HALF_TITAN: return "probes monsters";
		case RP_CYCLOPS: return "throws a boulder, dam LEV*3;";
		case RP_YEEK: return "makes a terrifying scream";
		case RP_KLACKON: return "spits acid, dam. LEV;";
		case RP_KOBOLD: return "throws a dart of poison, dam. LEV;";
		case RP_DARK_ELF: return "casts a Magic Missile, dam LEV+4/5+2;";
		case RP_DRACONIAN: return "breathes, dam. LEV*2;";
		case RP_MIND_FLAYER: return "mind blasts your enemies, dam LEV;";
		case RP_IMP:
			if (lev > 29)
				return "casts a Fire Ball, dam. LEV;";
			else
				return "casts a Fire Bolt, dam. LEV;";
		case SP_STONE_SKIN: return "turns your skin to stone, dur d20+30";
		case SP_RESTORE_LIFE: return "restores lost life forces";
		case RP_VAMPIRE:
			return "steals life from a foe, dam. LEV*11/10>11;-LEV/10+1>2*LEV";
		case RP_BROO: return "terrifies your enemies";
		case RP_SPRITE: return "throws magic dust which induces sleep";
		default:
		{
			return 0;
		}
	}
}

cptr describe_power(int power, int lev)
{
	char buf[1024] = "";
	cptr s;
	int l = 0, max = sizeof(buf)-2;
	if ((s = power_describe_timer(power)))
		l += sprintf(buf+l, "%.*s ", max-l, s);
	if ((s = power_describe_ball(power)))
		l += sprintf(buf+l, "%.*s ", max-l, s);
	if ((s = power_describe_misc(power, lev)))
		l += sprintf(buf+l, "%.*s ", max-l, s);
	return format("%s", buf);
}

static void phlogiston (void)
{

	int max_flog = 0;
	object_type * o_ptr = &inventory[INVEN_LITE];

	/* It's a lamp */
	if (o_ptr->k_idx == OBJ_BRASS_LANTERN)
	{
		max_flog = FUEL_LAMP;
	}

	/* It's a torch */
	else if (o_ptr->k_idx == OBJ_WOODEN_TORCH)
	{
		max_flog = FUEL_TORCH;
	}

	/* No torch to refill */
	else
	{
		msg_print("You are not wielding anything which uses phlogiston.");
		return;
	}

	if (o_ptr->pval >= max_flog)
	{
		msg_print("No more phlogiston can be put in this item.");
		return;
	}

	/* Refuel */
	o_ptr->pval += (max_flog / 2);

	/* Message */
	msg_print("You add phlogiston to your light item.");

	/* Comment */
	if (o_ptr->pval >= max_flog)
	{
		o_ptr->pval = max_flog;
		msg_print("Your light item is full.");
	}


	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);
}

static void call_the_(void)
{
	int i;

	if (cave_floor_bold(py-1,px-1) && cave_floor_bold(py-1, px) &&
		cave_floor_bold(py-1,px+1) && cave_floor_bold(py,px-1) &&
		cave_floor_bold(py,px+1) && cave_floor_bold(py+1,px-1) &&
		cave_floor_bold(py+1,px) && cave_floor_bold(py+1,px+1))
	{
		for (i = 1; i < 10; i++)
		if (i-5) fire_ball(GF_SHARD, i,
				175, 2);

		for (i = 1; i < 10; i++)
		if (i-5) fire_ball(GF_MANA, i,
				175, 3);

		for (i = 1; i < 10; i++)
		if (i-5) fire_ball(GF_NUKE, i,
				175, 4);
	}
	else
	{
		msg_format("You %s the %s too close to a wall!",
			"cast",
			"spell");
		msg_print("There is a loud explosion!");
		destroy_area(py, px, 20+(skill_set[SKILL_THAUMATURGY].value/2), TRUE);
		msg_print("The dungeon collapses...");
		take_hit(100 + (randint(150)), "a suicidal Call the Void", MON_CALLING_THE_VOID);
	}
}

/*
 * Choose a random wand from a list for the effect of a wand of wonder.
 */
static int choose_random_wand(void)
{
	int low_wands[] =
	{
		OBJ_WAND_LIGHT,
		OBJ_WAND_TAME_MONSTER,
		OBJ_WAND_COLD_BOLT,
		OBJ_WAND_FIRE_BOLT,
		OBJ_WAND_STONE_TO_MUD,
		OBJ_WAND_POLYMORPH,
		OBJ_WAND_HEAL_MONSTER,
		OBJ_WAND_HASTE_MONSTER,
		OBJ_WAND_SLOW_MONSTER,
		OBJ_WAND_CONFUSE_MONSTER,
		OBJ_WAND_SLEEP_MONSTER,
		OBJ_WAND_DRAIN_LIFE,
		OBJ_WAND_TRAP_DOOR_DESTRUCTION,
		OBJ_WAND_MAGIC_MISSILE,
		OBJ_WAND_CLONE_MONSTER,
		OBJ_WAND_SCARE_MONSTER,
		OBJ_WAND_TELEPORT_OTHER,
		OBJ_WAND_DISARMING,
		OBJ_WAND_ELEC_BALL,
		OBJ_WAND_COLD_BALL,
		OBJ_WAND_FIRE_BALL,
		OBJ_WAND_STINKING_CLOUD,
		OBJ_WAND_ACID_BALL,
		OBJ_WAND_ACID_BOLT,
	};
	return RAND_ELEMENT(low_wands);
}

/*
 * Hack -- activate the ring of power
 */
static void ring_of_power(int dir)
{
	/* Pick a random effect */
	switch (randint(10))
	{
		case 1:
		case 2:
		{
			/* Message */
			msg_print("You are surrounded by a malignant aura.");

			/* Decrease all stats (permanently) */
			(void)dec_stat(A_STR, 50, TRUE);
			(void)dec_stat(A_INT, 50, TRUE);
			(void)dec_stat(A_WIS, 50, TRUE);
			(void)dec_stat(A_DEX, 50, TRUE);
			(void)dec_stat(A_CON, 50, TRUE);
			(void)dec_stat(A_CHR, 50, TRUE);

			/* Lose some experience (permanently) */
			p_ptr->exp -= (p_ptr->exp / 4);

			break;
		}

		case 3:
		{
			/* Message */
			msg_print("You are surrounded by a powerful aura.");

			/* Dispel monsters */
			dispel_monsters(1000);

			break;
		}

		case 4:
		case 5:
		case 6:
		{
			/* Mana Ball */
			fire_ball(GF_MANA, dir, 300, 3);

			break;
		}

		case 7:
		case 8:
		case 9:
		case 10:
		{
			/* Mana Bolt */
			fire_bolt(GF_MANA, dir, 250);

			break;
		}
	}
}

/*
 * Enchant some bolts
 */
static bool brand_bolts(void)
{
	int i;

	/* Use the first acceptable bolts */
	for (i = 0; i < INVEN_PACK; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-bolts */
		if (o_ptr->tval != TV_BOLT) continue;

		/* Skip artifacts and ego-items */
		if (allart_p(o_ptr) || ego_item_p(o_ptr))
			continue;

		/* Skip cursed/broken items */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) continue;

		/* Randomize */
		if (rand_int(100) < 75) continue;

		/* Message */
		msg_print("Your bolts are covered in a fiery aura!");

		/* Ego-item */
		o_ptr->name2 = EGO_FLAME;
		apply_magic_2(o_ptr, dun_depth);

		/* Recalculate/redraw stuff (later) */
		update_object(o_ptr);

		/* Enchant */
		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);

		/* Notice */
		return (TRUE);
	}

	/* Flush */
	if (flush_failure) flush();

	/* Fail */
	msg_print("The fiery enchantment failed.");

	/* Notice */
	return (TRUE);
}



static void rustproof(void)
{
	errr err;

	object_type *o_ptr;

	/* Select a piece of armour */
	item_tester_hook = item_tester_hook_armour;

	/* Get an item (from equip or inven or floor) */
	if (!((o_ptr = get_item(&err, "Rustproof which piece of armour? ", TRUE, TRUE, TRUE))))
	{
		if (err == -2) msg_print("You have nothing to rustproof.");
		return;
	}


	o_ptr->flags3 |= TR3_IGNORE_ACID;

	if ((o_ptr->to_a < 0) && !(o_ptr->ident & IDENT_CURSED))
	{
		msg_format("%s %v look%s as good as new!",
			((is_inventory_p(o_ptr)) ? "Your" : "The"),
			object_desc_f3, o_ptr, FALSE, 0, ((o_ptr->number > 1) ? "" : "s"));
			o_ptr->to_a = 0;
	}

	msg_format("%s %v %s now protected against corrosion.",
		((is_inventory_p(o_ptr)) ? "Your" : "The"),
		object_desc_f3, o_ptr, FALSE, 0, ((o_ptr->number > 1) ? "are" : "is"));

	return;
}

void do_poly_wounds(int cause)
{

	s16b wounds = p_ptr->cut, hit_p = (p_ptr->mhp - p_ptr->chp);
	s16b change = damroll(skill_set[SKILL_TOUGH].value/2, 5);
	bool Nasty_effect = (randint(5)==1);

	if (!(wounds || hit_p || Nasty_effect)) return;

	if (Nasty_effect)
		{
			msg_print("A new wound was created!");
			take_hit(change, "a polymorphed wound", cause);
			set_flag(TIMED_CUT, change);
		}
	else
		{
			msg_print("Your wounds are polymorphed into less serious ones.");
			hp_player(change);
			set_flag(TIMED_CUT, (p_ptr->cut)-(change/2));
		}
}

static void do_poly_race(void)
{
	/* Pick a random race other than the current one. */
	int new_race = rand_int(MAX_RACES-1);
	if (new_race == p_ptr->prace) new_race = MAX_RACES-1;

	/* Set the globals for the new race. */
	p_ptr->prace = new_race;
	rp_ptr = &race_info[p_ptr->prace];

	msg_format("You turn into %s %s!",
		is_a_vowel(rp_ptr->title[0]) ? "an" : "a", rp_ptr->title);

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp;

	/* Calculate the height/weight for males */
	if (p_ptr->psex == SEX_MALE)
	{
		p_ptr->ht = randnor(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
		p_ptr->wt = randnor(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
	}

	/* Calculate the height/weight for females */
	else if (p_ptr->psex == SEX_FEMALE)
	{
		p_ptr->ht = randnor(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
		p_ptr->wt = randnor(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
	}

	/* Load race-specific pref file information if allowed. */
	if (get_check("Re-load preference files for new race (y/n)?"))
	{
		process_some_user_pref_files();
	}

	p_ptr->update |= (PU_BONUS);

	handle_stuff();

	lite_spot(py, px);
}

void do_poly_self(void)
{
	int effects, tmp;

	msg_print("You feel a change coming over you...");

	for (effects = randint(2), tmp = 0; effects; effects--)
	{
		switch (rand_int(12))
		{
			case 1: case 2:
			{
				do_poly_wounds(MON_FATAL_POLYMORPH);
				break;
			}
			case 3: case 4:
			{
				(void) gain_chaos_feature(0);
				break;
			}
			case 5: case 6: case 7: /* Racial polymorph! Uh oh... */
			{
				do_poly_race();

				/* Do nothing more. */
				return;
			}
			case 8:
			{
				msg_print("You polymorph into an abomination!");
				while (tmp < 6)
				{
					dec_stat(tmp, rand_range(7, 12), one_in(3));
					tmp++;
				}
				if (one_in(6))
				{
					msg_print("You find living difficult in your present form!");
					take_hit(damroll(randint(skill_set[SKILL_TOUGH].value/2),skill_set[SKILL_TOUGH].value/2), "a lethal chaos feature", MON_FATAL_POLYMORPH);
				}
				chaos_feature_shuffle();
				break;
			}
			default:
			{
				chaos_feature_shuffle();
			}
		}
	}
}

/*
 * mvd hook for fetch(). Stop on the first item or before the first wall or the
 * edge of the range.
 */
static int PURE mvd_no_object(int y, int x, int d)
{
	if (d == MAX_RANGE)
	{
		return MVD_STOP_HERE;
	}
	else if (!cave_floor_bold(y, x))
	{
		return MVD_STOP_BEFORE_HERE;
	}
	else if (cave[y][x].o_idx)
	{
		return MVD_STOP_HERE;
	}
	else
	{
		return MVD_CONTINUE;
	}
}

/*
 * Fetch an item (teleport it right underneath the caster)
 */
static void fetch(int dir, int wgt, bool require_los)
{
	int x, y;
	cave_type *c_ptr;
	object_type *o_ptr;

	/* Check to see if an object is already there */
	if(cave[py][px].o_idx)
	{
		msg_print("You can't fetch when you're already standing on something.");
		return;
	}

	/* Find the target. */
	get_dir_target(&x, &y, dir, mvd_no_object);

	/* Extract the target. */
	c_ptr = &cave[y][x];

	/* Extract the "top" item (the only one this can fetch). */
	o_ptr = &o_list[c_ptr->o_idx];

	if (!c_ptr->o_idx)
	{
		msg_print("There is no object to retrieve.");
	}
	else if (distance(py, px, y, x) > MAX_RANGE)
	{
		msg_print("You can't fetch something that far away!");
	}
	else if (require_los && (!player_has_los_bold(y, x)))
	{
		msg_print("You have no direct line of sight to that location.");
	}
	/* Too heavy to 'fetch' */
	else if (o_ptr->weight > wgt)
	{
		msg_print("The object is too heavy.");
	}
	else
	{
		/* "move" the object. */
		cave[py][px].o_idx = c_ptr->o_idx;
		c_ptr->o_idx = 0;
		o_ptr->iy = (byte)py;
		o_ptr->ix = (byte)px;

		note_spot(py,px);
		p_ptr->redraw |= PR_MAP;
	}
}

static void brand_weapon(int brand_type)
{
	object_type *o_ptr = &inventory[INVEN_WIELD];

	/* you can never modify artifacts / ego-items */
	/* you can never modify cursed items */
	/* TY: You _can_ modify broken items (if you're silly enough) */
	if (o_ptr->k_idx && !allart_p(o_ptr) && !ego_item_p(o_ptr) && !cursed_p(o_ptr))
	{
		cptr act = NULL;
		int name2 = brand_type;

		switch (brand_type)
		{
			case EGO_PLANAR:
				act = "seems very unstable now.";
				break;
			case EGO_VAMPIRIC:
				act = "thirsts for blood!";
				break;
			case EGO_BRAND_POIS:
				act = "is coated with poison.";
				break;
			case EGO_CHAOTIC:
				act = "is engulfed in raw chaos!";
				break;
			default:
			if (one_in(4))
			{
				act = "glows deep, icy blue!";
				name2 = EGO_BRAND_COLD;
			}
			else
			{
				act = "is covered in a fiery shield!";
				name2 = EGO_BRAND_FIRE;
			}
		}

		/* Display a message. */
		msg_format("Your %v %s", object_desc_f3, o_ptr, FALSE, 0, act);

		/* Apply the enchantment. */
		o_ptr->name2 = name2;
		apply_magic_2(o_ptr, dun_depth);
		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}

	else
	{
		if (flush_failure) flush();

		msg_print("The Branding failed.");
	}
}




void wild_magic(int spell)
{
	int counter = 0;

	switch(randint(spell) + randint(8) + 1)

	{
		case 1: case 2: case 3:
			teleport_player(10);
			break;
		case 4: case 5: case 6:
			teleport_player(100);
			break;
		case 7: case 8:
			teleport_player(200);
			break;
		case 9: case 10: case 11:
			unlite_area(10, 3);
			break;
		case 12: case 13: case 14:
			lite_area(damroll(2, 3), 2);
			break;
		case 15:
			destroy_doors_touch();
			break;
		case 16: case 17:
			wall_breaker(50);
			break;
		case 18:
			sleep_monsters_touch(50);
			break;
		case 19: case 20:
			trap_creation();
			break;
		case 21: case 22:
			door_creation();
			break;
		case 23: case 24: case 25:
			aggravate_monsters(NULL);
			break;
		case 26:
			earthquake(py, px, 5);
			break;
		case 27: case 28:
			(void) gain_chaos_feature(0);
			break;
		case 29: case 30:
			apply_disenchant(0);
			break;
		case 31:
			lose_all_info();
			break;
		case 32:
			fire_ball(GF_CHAOS, 0, spell + 5, 1 + (spell/10));
			break;
		case 33:
			wall_stone();
			break;
		case 34: case 35:
		{
			int wild_monsters[] =
			{
				SUMMON_MOULD,
				SUMMON_BAT,
				SUMMON_QUYLTHULG,
				SUMMON_VORTEX,
				SUMMON_TREASURE,
				SUMMON_MIMIC,
			};
			int type = RAND_ELEMENT(wild_monsters);
			while (counter++ < 8)
			{
			(void) summon_specific(py, px, ((dun_depth) * 3) / 2, type);
					}
			break;
		}
		case 36: case 37:
			activate_hi_summon();
			break;
		case 38:
			summon_reaver();
			break;
		default:
			activate_ty_curse();
	}
	return;
}

/*
 * Dismiss your allies.
 */
static void dismiss_pets(bool some)
{
	bool all_pets = FALSE;
	s16b i,Dismissed = 0;
	if (some && get_check("Dismiss all allies? ")) all_pets = TRUE;
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		monster_type *m_ptr = &m_list[i];
		if (m_ptr->smart & (SM_ALLY)) /* Get rid of it! */
		{
			bool delete_this = all_pets || get_check(
					format("Dismiss %v? ", monster_desc_f2, m_ptr, 0x80));

			if (delete_this)
			{
				delete_monster_idx(i,TRUE);
				Dismissed++;
			}
		}
	}
	msg_format("You have dismissed %d all%s.", Dismissed,
		(Dismissed==1?"y":"ies"));
}

/*
 * Remove all mutations from the player.
 */
static void cure_mutations(void)
{
	/* Nothing to do. */
	if (!p_mutated()) return;

	/* Clear everything and update. */
	msg_print("You are cured of all chaos features.");
	p_clear_mutations();
	p_ptr->update |= PU_BONUS;
	handle_stuff();
}

/*
 * Speed the player up for a period of time. Has little effect on characters
 * who have already been sped up.
 */
static bool speed_up(int min, int max)
{
	if (!p_ptr->fast)
	{
		return set_flag(TIMED_FAST, rand_range(min, max));
	}
	else
	{
		add_flag(TIMED_FAST, 5);
		return FALSE;
	}
}

/*
 * Carry out one type of dangerous summon spell.
 */
static void summon_1(int type, bool group, int plev,
	cptr s1, cptr s2_good, cptr s2_bad)
{
	bool friendly = !one_in(3);
	if (!friendly) group = TRUE;

	if (!summon_specific_aux(py, px, (plev*3)/2, type, group, friendly)) return;

	msg_print(s1);
	msg_print(friendly ? s2_good : s2_bad);
}

/*
 * Carry out another, more structured, type of dangerous summon spell.
 */
static bool summon_2(int type, int chance, bool group, int plev,
	cptr where, cptr what)
{
	bool friendly = percent(chance);

	/* Hostile summons can always come in groups. */
	if (!friendly) group = TRUE;

	if (where) msg_format("You reach out your mind to %s...", where);

	if (!summon_specific_aux(py, px, plev, type, group, friendly))
	{
		msg_print("Nobody answers to your call.");
	}
	else if (!friendly && what)
	{
		/* Hack - assume singular, as this can't be checked. */
		msg_format("The summoned %s gets angry!", what);
	}
	return friendly;
}

/*
 * Use whatever power an object has, and set three variables to represent the
 * effect.
 *
 * power identifies the power used, calculated as in get_power().
 * dir indicates a direction which has been chosen
 * The calling function may remove/drain the object if use is unset, but must
 * do so if it is set.
 */
static errr do_power(int power, int plev, int dir, bool known, bool *use, bool *ident)
{
	bool b;
	int i;

	/* Assume that the object was used without identifying it by default. */
	(*use) = TRUE;
	(*ident) = FALSE;

	switch (power)
	{
		case OBJ_FOOD_POISON+PO_K_IDX:
		case OBJ_FOOD_BLINDNESS+PO_K_IDX:
		case OBJ_FOOD_PARANOIA+PO_K_IDX:
		case OBJ_FOOD_CONFUSION+PO_K_IDX:
		case OBJ_FOOD_HALLUCINATION+PO_K_IDX:
		case OBJ_FOOD_PARALYSIS+PO_K_IDX:
		case OBJ_POTION_SLOWNESS+PO_K_IDX:
		case OBJ_POTION_POISON+PO_K_IDX:
		case OBJ_POTION_BLINDNESS+PO_K_IDX:
		case OBJ_POTION_SLEEP+PO_K_IDX:
		case OBJ_POTION_INFRA_VISION+PO_K_IDX:
		case OBJ_POTION_DETECT_INVIS+PO_K_IDX:
		case OBJ_POTION_RES_HEAT+PO_K_IDX:
		case OBJ_POTION_RES_COLD+PO_K_IDX:
		case OBJ_POTION_RESISTANCE:
		case OBJ_POTION_INVULNERABILITY+PO_K_IDX:
		case OBJ_SCROLL_BLESSING+PO_K_IDX:
		case OBJ_SCROLL_HOLY_CHANT+PO_K_IDX:
		case OBJ_SCROLL_HOLY_PRAYER+PO_K_IDX:
		case OBJ_STAFF_SLOWNESS+PO_K_IDX:
		case ACT_ESP+PO_ACTIVATION:
		case ACT_BERSERK+PO_ACTIVATION:
		case ACT_INVULN+PO_ACTIVATION:
		case SP_BLESS:
		case SP_SENSE_UNSEEN:
		case SP_PRAYER:
		case SP_HOLY_INVULNERABILITY:
		case SP_RESIST_ENVIRONMENT:
		case SP_STONE_SKIN:
		case SP_RESISTANCE_TRUE:
		case SP_SENSE_MINDS:
		case SP_GLOBE_OF_INVULNERABILITY:
		case SP_PLANAR_SPYING:
		case SP_RESIST_POISON:
		case SP_RESIST_COLD:
		case SP_RESIST_FIRE:
		case SP_RESIST_ACID:
		case SP_RESIST_LIGHTNING:
		case SP_SEE_INVISIBLE:
		case ART_BARZAI+PO_NAME1:
		{
			return power_add_timed(power, ident);
		}

		case OBJ_WAND_STINKING_CLOUD+PO_K_IDX:
		case OBJ_WAND_ACID_BALL+PO_K_IDX:
		case OBJ_WAND_ELEC_BALL+PO_K_IDX:
		case OBJ_WAND_FIRE_BALL+PO_K_IDX:
		case OBJ_WAND_COLD_BALL+PO_K_IDX:
		case OBJ_WAND_DRAGON_FIRE+PO_K_IDX:
		case OBJ_WAND_DRAGON_COLD+PO_K_IDX:
		case OBJ_WAND_DRAGON_BREATH+PO_K_IDX:
		case OBJ_ROD_ACID_BALL+PO_K_IDX:
		case OBJ_ROD_ELEC_BALL+PO_K_IDX:
		case OBJ_ROD_FIRE_BALL+PO_K_IDX:
		case OBJ_ROD_COLD_BALL+PO_K_IDX:
		case ACT_BA_POIS_1+PO_ACTIVATION:
		case ACT_BA_COLD_1+PO_ACTIVATION:
		case ACT_BA_FIRE_1+PO_ACTIVATION:
		case ACT_BA_COLD_2+PO_ACTIVATION:
		case ACT_BA_ELEC_2+PO_ACTIVATION:
		case ACT_BA_FIRE_2+PO_ACTIVATION:
		case ACT_BA_COLD_3+PO_ACTIVATION:
		case ACT_BA_ELEC_3+PO_ACTIVATION:
		case ART_ELEMFIRE+PO_NAME1:
		case ART_ELEMICE+PO_NAME1:
		case ART_ELEMSTORM+PO_NAME1:
		case ART_THOTH+PO_NAME1:
		case ART_ICICLE+PO_NAME1:
		case ART_STARLIGHT+PO_NAME1:
		case ART_EVERFLAME+PO_NAME1:
		case ART_ODIN+PO_NAME1:
		case ART_FIRESTAR+PO_NAME1:
		case SP_DARKNESS_STORM:
		{
			return power_project_ball(power, dir, ident);
		}
		case OBJ_RING_ACID+PO_K_IDX:
		case OBJ_RING_ICE+PO_K_IDX:
		case OBJ_RING_FIRE+PO_K_IDX:
		{
			return power_project_ball(power, dir, ident) ||
				power_add_timed(power, ident);
		}

		case OBJ_FOOD_DEC_STR+PO_K_IDX:
		{
			take_hit(damroll(6, 6), "poisonous food.", MON_POISONOUS_FOOD);
			(void)do_dec_stat(A_STR);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_FOOD_SICKNESS+PO_K_IDX:
		{
			take_hit(damroll(6, 6), "poisonous food.", MON_POISONOUS_FOOD);
			(void)do_dec_stat(A_CON);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_FOOD_DEC_INT+PO_K_IDX:
		{
			take_hit(damroll(8, 8), "poisonous food.", MON_POISONOUS_FOOD);
			(void)do_dec_stat(A_INT);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_FOOD_DEC_WIS+PO_K_IDX:
		{
			take_hit(damroll(8, 8), "poisonous food.", MON_POISONOUS_FOOD);
			(void)do_dec_stat(A_WIS);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_FOOD_UNHEALTH+PO_K_IDX:
		{
			take_hit(damroll(10, 10), "poisonous food.", MON_POISONOUS_FOOD);
			(void)do_dec_stat(A_CON);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_FOOD_DISEASE+PO_K_IDX:
		{
			take_hit(damroll(10, 10), "poisonous food.", MON_POISONOUS_FOOD);
			(void)do_dec_stat(A_STR);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_FOOD_CURE_POISON+PO_K_IDX:
		case OBJ_POTION_NEUTRALIZE_POISON+PO_K_IDX:
		case SP_CURE_POISON:
		{
			if (set_flag(TIMED_POISONED, 0)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_FOOD_CURE_BLINDNESS+PO_K_IDX:
		{
			if (set_flag(TIMED_BLIND, 0)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_FOOD_CURE_PARANOIA+PO_K_IDX:
		case OBJ_POTION_BOLDNESS+PO_K_IDX:
		case SP_REMOVE_FEAR:
		{
			if (set_flag(TIMED_AFRAID, 0)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_FOOD_CURE_CONFUSION+PO_K_IDX:
		{
			if (set_flag(TIMED_CONFUSED, 0)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_FOOD_CURE_SERIOUS+PO_K_IDX:
		{
			if (hp_player(damroll(4, 8))) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_FOOD_RES_STR+PO_K_IDX:
		{
			if (do_res_stat(A_STR)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_FOOD_RES_CON+PO_K_IDX:
		{
			if (do_res_stat(A_CON)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_FOOD_RESTORING+PO_K_IDX:
		{
			if (do_res_stats()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_PIECE_OF_ELVISH_WAYBREAD+PO_K_IDX:
		{
			msg_print("That tastes good.");
			(void)set_flag(TIMED_POISONED, 0);
			(void)hp_player(damroll(4, 8));
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_WATER+PO_K_IDX:
		case OBJ_POTION_APPLE_JUICE+PO_K_IDX:
		case OBJ_POTION_SLIME_MOULD_JUICE+PO_K_IDX:
		{
			msg_print("You feel less thirsty.");
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_SALT_WATER+PO_K_IDX:
		{
			msg_print("The potion makes you vomit!");
			(void)set_flag(TIMED_FOOD, PY_FOOD_STARVE - 1);
			(void)set_flag(TIMED_POISONED, 0);
			(void)add_flag(TIMED_PARALYZED, 4);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_BOOZE+PO_K_IDX: /* Booze */
		{
			if (!((p_ptr->resist_conf) || (p_ptr->resist_chaos)))
			{
				if (add_flag(TIMED_CONFUSED, rand_int(20) + 15))
				{
					(*ident) = TRUE;
				}
				if (randint(2)==1)
				{
					if (add_flag(TIMED_IMAGE, rand_int(150) + 150))
					{
						(*ident) = TRUE;
					}
				}
				if (randint(13)==1)
				{
					(*ident) = TRUE;
					if(randint(3)==1) lose_all_info();
					else wiz_dark();
					teleport_player(100);
					wiz_dark();
					msg_print("You wake up somewhere with a sore head...");
					msg_print("You can't remember a thing, or how you got here!");
				}
			}
			return SUCCESS;
		}

		case OBJ_POTION_LOSE_MEMORIES+PO_K_IDX:
		{
			if (!p_ptr->hold_life)
			{
				msg_print("You feel your memories fade.");
				lose_skills(10);
				(*ident) = TRUE;
			}
			return SUCCESS;
		}

		case OBJ_POTION_RUINATION+PO_K_IDX:
		{
			msg_print("Your nerves and muscles feel weak and lifeless!");
			take_hit(damroll(10, 10), "a potion of Ruination", MON_HARMFUL_POTION);
			(void)dec_stat(A_DEX, 25, TRUE);
			(void)dec_stat(A_WIS, 25, TRUE);
			(void)dec_stat(A_CON, 25, TRUE);
			(void)dec_stat(A_STR, 25, TRUE);
			(void)dec_stat(A_CHR, 25, TRUE);
			(void)dec_stat(A_INT, 25, TRUE);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_DEC_STR+PO_K_IDX:
		{
			if (do_dec_stat(A_STR)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_DEC_INT+PO_K_IDX:
		{
			if (do_dec_stat(A_INT)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_DEC_WIS+PO_K_IDX:
		{
			if (do_dec_stat(A_WIS)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_DEC_DEX+PO_K_IDX:
		{
			if (do_dec_stat(A_DEX)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_DEC_CON+PO_K_IDX:
		{
			if (do_dec_stat(A_CON)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_DEC_CHR+PO_K_IDX:
		{
			if (do_dec_stat(A_CHR)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_DETONATIONS+PO_K_IDX:
		{
			msg_print("Massive explosions rupture your body!");
			take_hit(damroll(50, 20), "a potion of Detonation", MON_HARMFUL_POTION);
			(void)add_flag(TIMED_STUN, 75);
			(void)add_flag(TIMED_CUT, 5000);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_IOCAINE+PO_K_IDX:
		{
			msg_print("A feeling of Death flows through your body.");
			take_hit(5000, "a potion of Death", MON_HARMFUL_POTION);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_SLOW_POISON+PO_K_IDX:
		{
			if (set_flag(TIMED_POISONED, p_ptr->poisoned / 2)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_SPEED+PO_K_IDX:
		{
			if (speed_up(16, 40)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_HEROISM+PO_K_IDX:
		{
			if (set_flag(TIMED_AFRAID, 0)) (*ident) = TRUE;
			if (add_flag(TIMED_HERO, randint(25) + 25)) (*ident) = TRUE;
			if (hp_player(10)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_BERSERK_STR+PO_K_IDX:
		case SP_BERSERK:
		case MUT_BERSERK+PO_MUTA:
		{
			if (set_flag(TIMED_AFRAID, 0)) (*ident) = TRUE;
			if (add_flag(TIMED_SHERO, randint(25) + 25)) (*ident) = TRUE;
			if (hp_player(30)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_CURE_LIGHT+PO_K_IDX:
		{
			if (hp_player(damroll(2, 8))) (*ident) = TRUE;
			if (set_flag(TIMED_BLIND, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CUT, p_ptr->cut - 10)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_CURE_SERIOUS+PO_K_IDX:
		{
			if (hp_player(damroll(4, 8))) (*ident) = TRUE;
			if (set_flag(TIMED_BLIND, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CONFUSED, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CUT, (p_ptr->cut / 2) - 50)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_CURE_CRITICAL+PO_K_IDX:
		{
			if (hp_player(damroll(6, 8))) (*ident) = TRUE;
			if (set_flag(TIMED_BLIND, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CONFUSED, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_POISONED, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_STUN, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CUT, 0)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_HEALING+PO_K_IDX:
		{
			if (hp_player(300)) (*ident) = TRUE;
			if (set_flag(TIMED_BLIND, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CONFUSED, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_POISONED, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_STUN, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CUT, 0)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_STAR_HEALING+PO_K_IDX:
		{
			if (hp_player(1200)) (*ident) = TRUE;
			if (set_flag(TIMED_BLIND, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CONFUSED, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_POISONED, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_STUN, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CUT, 0)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_LIFE+PO_K_IDX:
		{
			msg_print("You feel life flow through your body!");
			restore_level();
			hp_player(5000);
			(void)set_flag(TIMED_POISONED, 0);
			(void)set_flag(TIMED_BLIND, 0);
			(void)set_flag(TIMED_CONFUSED, 0);
			(void)set_flag(TIMED_IMAGE, 0);
			(void)set_flag(TIMED_STUN, 0);
			(void)set_flag(TIMED_CUT, 0);
			do_res_stats();
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_RES_MANA+PO_K_IDX:
		{
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				msg_print("Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);
				(*ident) = TRUE;
			}
			if (p_ptr->cchi < p_ptr->mchi)
			{
				p_ptr->cchi = p_ptr->mchi;
				p_ptr->chi_frac = 0;
				msg_print("Your feel your chi harmonise.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);
				(*ident) = TRUE;
			}
			return SUCCESS;
		}

		case OBJ_POTION_RES_LIFE_LEVELS+PO_K_IDX:
		case ACT_REST_LIFE+PO_ACTIVATION:
		case ART_NYOGTHA+PO_NAME1:
		case SP_RESTORE_LIFE:
		{
			if (restore_level()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_RES_STR+PO_K_IDX:
		{
			if (do_res_stat(A_STR)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_RES_INT+PO_K_IDX:
		{
			if (do_res_stat(A_INT)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_RES_WIS+PO_K_IDX:
		{
			if (do_res_stat(A_WIS)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_RES_DEX+PO_K_IDX:
		{
			if (do_res_stat(A_DEX)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_RES_CON+PO_K_IDX:
		{
			if (do_res_stat(A_CON)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_RES_CHR+PO_K_IDX:
		{
			if (do_res_stat(A_CHR)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_INC_STR+PO_K_IDX:
		{
			if (do_inc_stat(A_STR)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_INC_INT+PO_K_IDX:
		{
			if (do_inc_stat(A_INT)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_INC_WIS+PO_K_IDX:
		{
			if (do_inc_stat(A_WIS)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_INC_DEX+PO_K_IDX:
		{
			if (do_inc_stat(A_DEX)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_INC_CON+PO_K_IDX:
		{
			if (do_inc_stat(A_CON)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_INC_CHR+PO_K_IDX:
		{
			if (do_inc_stat(A_CHR)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_AUGMENTATION+PO_K_IDX:
		{
			if (do_inc_stat(A_STR)) (*ident) = TRUE;
			if (do_inc_stat(A_INT)) (*ident) = TRUE;
			if (do_inc_stat(A_WIS)) (*ident) = TRUE;
			if (do_inc_stat(A_DEX)) (*ident) = TRUE;
			if (do_inc_stat(A_CON)) (*ident) = TRUE;
			if (do_inc_stat(A_CHR)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_ENLIGHTENMENT+PO_K_IDX:
		{
			msg_print("An image of your surroundings forms in your mind...");
			wiz_lite();
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_STAR_ENLIGHTENMENT+PO_K_IDX:
		{
			msg_print("You begin to feel more enlightened...");
			msg_print(NULL);
			wiz_lite();
			(void)do_inc_stat(A_INT);
			(void)do_inc_stat(A_WIS);
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			(void)detect_treasure();
			(void)detect_objects_gold();
			(void)detect_objects_normal();
			identify_pack();
			self_knowledge();
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_SELF_KNOWLEDGE+PO_K_IDX:
		{
			msg_print("You begin to know yourself a little better...");
			msg_print(NULL);
			self_knowledge();
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_EXPERIENCE+PO_K_IDX:
		{
				msg_print("You feel more experienced.");
				gain_skills(200);
				(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_POTION_CURING+PO_K_IDX:
		{
			if (hp_player(50)) (*ident) = TRUE;
			if (set_flag(TIMED_BLIND, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_POISONED, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CONFUSED, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_STUN, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CUT, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_IMAGE, 0)) (*ident) = TRUE;
		return SUCCESS;
		}

		case OBJ_POTION_NEW_LIFE+PO_K_IDX:
		{
		do_cmd_rerate();
		cure_mutations();
		(*ident) = TRUE;
		return SUCCESS;
		}
		case OBJ_SCROLL_DARKNESS+PO_K_IDX:
		{
			if (!(p_ptr->resist_blind) && !(p_ptr->resist_dark))
			{
				(void)add_flag(TIMED_BLIND, 3 + randint(5));
			}
			if (unlite_area(10, 3)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_AGGRAVATE_MONSTER+PO_K_IDX:
		{
			msg_print("There is a high pitched humming noise.");
			aggravate_monsters(NULL);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_CURSE_ARMOUR+PO_K_IDX:
		{
			if (curse_armor()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_CURSE_WEAPON+PO_K_IDX:
		{
			if (curse_weapon()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_SUMMON_MONSTER+PO_K_IDX:
		{
			switch (rand_int(9))
			{
				case 0: case 1: case 2: i = 1; break;
				case 3: case 4: case 5: case 6: i = 2; break;
				default: i = 3;
			}

			for (; i; i--)
			{
				if (summon_specific(py, px, (dun_depth), 0))
				{
					(*ident) = TRUE;
				}
			}
			return SUCCESS;
		}

		case OBJ_SCROLL_SUMMON_UNDEAD+PO_K_IDX:
		{
			switch (rand_int(9))
			{
				case 0: case 1: case 2: i = 1; break;
				case 3: case 4: case 5: case 6: i = 2; break;
				default: i = 3;
			}

			for (; i; i--)
			{
				if (summon_specific(py, px, (dun_depth), SUMMON_UNDEAD))
				{
					(*ident) = TRUE;
				}
			}
			return SUCCESS;
		}

		case OBJ_SCROLL_TRAP_CREATION+PO_K_IDX:
		{
			if (trap_creation()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_PHASE_DOOR+PO_K_IDX:
		case SP_PHASE_DOOR:
		case MUT_BLINK+PO_MUTA:
		{
			teleport_player(10);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_TELEPORTATION+PO_K_IDX:
		case OBJ_STAFF_TELEPORTATION+PO_K_IDX:
		case ACT_TELEPORT+PO_ACTIVATION:
		case ACT_TELEPORT_WAIT+PO_ACTIVATION:
		case ART_SHIFTER+PO_NAME1:
		{
			teleport_player(100);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_TELEPORT_LEVEL+PO_K_IDX:
		case SP_TELEPORT_LEVEL:
		{
			(void)teleport_player_level();
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_WORD_OF_RECALL+PO_K_IDX:
		case OBJ_ROD_RECALL+PO_K_IDX:
		case ACT_RECALL+PO_ACTIVATION:
		case ART_GHARNE+PO_NAME1:
		case MUT_RECALL+PO_MUTA:
		{
			set_recall(FALSE);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_IDENTIFY+PO_K_IDX:
		case OBJ_STAFF_PERCEPTION+PO_K_IDX:
		case OBJ_ROD_PERCEPTION+PO_K_IDX:
		case ACT_ID_PLAIN+PO_ACTIVATION:
		case ART_ERIRIL+PO_NAME1:
		case SP_IDENTIFY:
		{
			(*ident) = TRUE;
			if (!ident_spell()) (*use) = FALSE;
			return SUCCESS;
		}

		case OBJ_SCROLL_STAR_IDENTIFY+PO_K_IDX:
		case ACT_ID_FULL+PO_ACTIVATION:
		case SP_IDENTIFY_TRUE:
		{
			(*ident) = TRUE;
			if (!identify_fully()) (*use) = FALSE;
			return SUCCESS;
		}

		case OBJ_SCROLL_REMOVE_CURSE+PO_K_IDX:
		{
			if (remove_curse())
			{
				msg_print("You feel as if someone is watching over you.");
				(*ident) = TRUE;
			}
			return SUCCESS;
		}

		case OBJ_SCROLL_STAR_REMOVE_CURSE+PO_K_IDX:
		{
			remove_all_curse();
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_ENCHANT_ARMOUR+PO_K_IDX:
		{
			(*ident) = TRUE;
			if (!enchant_spell(0, 0, 1)) (*use) = FALSE;
			return SUCCESS;
		}

		case OBJ_SCROLL_ENCHANT_WEAPON_TO_HIT+PO_K_IDX:
		{
			if (!enchant_spell(1, 0, 0)) (*use) = FALSE;
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_ENCHANT_WEAPON_TO_DAM+PO_K_IDX:
		{
			if (!enchant_spell(0, 1, 0)) (*use) = FALSE;
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_STAR_ENCHANT_ARMOUR+PO_K_IDX:
		{
			if (!enchant_spell(0, 0, randint(3) + 2)) (*use) = FALSE;
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_STAR_ENCHANT_WEAPON+PO_K_IDX:
		{
			if (!enchant_spell(randint(3), randint(3), 0)) (*use) = FALSE;
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_RECHARGING+PO_K_IDX:
		case ACT_RECHARGE+PO_ACTIVATION:
		case ART_SWASHBUCKLER+PO_NAME1:
		{
			if (!recharge(60)) (*use) = FALSE;
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_LIGHT+PO_K_IDX:
		{
			if (lite_area(damroll(2, 8), 2)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_MAGIC_MAPPING+PO_K_IDX:
		{
			map_area();
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_TREASURE_DETECTION+PO_K_IDX:
		{
			if (detect_treasure()) (*ident) = TRUE;
			if (detect_objects_gold()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_OBJECT_DETECTION+PO_K_IDX:
		{
			if (detect_objects_normal()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_TRAP_DETECTION+PO_K_IDX:
		case OBJ_STAFF_TRAP_LOCATION+PO_K_IDX:
		case OBJ_ROD_TRAP_LOCATION+PO_K_IDX:
		{
			if (known || detect_traps_p())
			{
				(*ident) = TRUE;
				detect_traps();
			}
			return SUCCESS;
		}

		case OBJ_SCROLL_DOOR_STAIR_LOCATION+PO_K_IDX:
		case OBJ_STAFF_DOOR_STAIR_LOCATION+PO_K_IDX:
		case OBJ_ROD_DOOR_STAIR_LOCATION+PO_K_IDX:
		{
			if (detect_doors()) (*ident) = TRUE;
			if (detect_stairs()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_DETECT_INVIS+PO_K_IDX:
		{
			if (detect_monsters_invis()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_SATISFY_HUNGER+PO_K_IDX:
		case ACT_SATIATE+PO_ACTIVATION:
		case SP_SATISFY_HUNGER:
		{
			if (set_flag(TIMED_FOOD, PY_FOOD_MAX - 1)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_MONSTER_CONFUSION+PO_K_IDX:
		{
			if (p_ptr->confusing == 0)
			{
				msg_print("Your hands begin to glow.");
				p_ptr->confusing = TRUE;
				(*ident) = TRUE;
			}
			return SUCCESS;
		}

		case OBJ_SCROLL_PROTECTION_FROM_EVIL+PO_K_IDX:
		case ACT_PROT_EVIL+PO_ACTIVATION:
		case ART_LOBON+PO_NAME1:
		case SP_PROTECTION_FROM_EVIL:
		{
			i = 3 * plev;
			if (add_flag(TIMED_PROTEVIL, randint(25) + i)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_RUNE_OF_PROTECTION+PO_K_IDX:
		{
			warding_glyph();
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_TRAP_DOOR_DESTRUCTION+PO_K_IDX:
		case ACT_DEST_DOOR+PO_ACTIVATION:
		case ART_OGRELORDS+PO_NAME1:
		case SP_TRAP_DOOR_DESTRUCTION:
		{
			if (destroy_doors_touch()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_TRAP_DOOR_DESTRUCTION+PO_K_IDX:
		case SP_TRAP_DOOR_DESTRUCTION_2:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (destroy_door(dir)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_STAR_DESTRUCTION+PO_K_IDX:
		case OBJ_STAFF_STAR_DESTRUCTION+PO_K_IDX:
		case SP_WORD_OF_DESTRUCTION:
		{
			destroy_area(py, px, 15, TRUE);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_DISPEL_UNDEAD+PO_K_IDX:
		{
			if (dispel_undead(60)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_GENOCIDE+PO_K_IDX:
		case OBJ_STAFF_GENOCIDE+PO_K_IDX:
		case ACT_GENOCIDE+PO_ACTIVATION:
		case ART_ORCS+PO_NAME1:
		case SP_GENOCIDE:
		{
			if (genocide(TRUE) == POWER_ERROR_ABORT) (*use) = FALSE;
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_MASS_GENOCIDE+PO_K_IDX:
		case ACT_MASS_GENO+PO_ACTIVATION:
		case ART_TROLLS+PO_NAME1:
		case SP_MASS_GENOCIDE:
		{
			(void)mass_genocide(TRUE);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_ACQUIREMENT+PO_K_IDX:
		{
			acquirement(py, px, 1, TRUE);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_STAR_ACQUIREMENT+PO_K_IDX:
		{
			acquirement(py, px, randint(2) + 1, TRUE);
			(*ident) = TRUE;
			return SUCCESS;
		}

		/* New Zangband scrolls */
		case OBJ_SCROLL_FIRE+PO_K_IDX:
		{
			fire_ball(GF_FIRE, 0,
				150, 4); /* Note: "Double" damage since it is centered on
							the player ... */
			if (!(p_ptr->oppose_fire || p_ptr->resist_fire || p_ptr->immune_fire))
				take_hit(50+randint(50), "a Scroll of Fire", MON_HARMFUL_SCROLL);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_ICE+PO_K_IDX:
		{
			fire_ball(GF_ICE, 0,
				175, 4);
			if (!(p_ptr->oppose_cold || p_ptr->resist_cold || p_ptr->immune_cold))
				take_hit(100+randint(100), "a Scroll of Ice", MON_HARMFUL_SCROLL);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_CHAOS+PO_K_IDX:
		{
			fire_ball(GF_CHAOS, 0,
				222, 4);
			if (!p_ptr->resist_chaos)
				take_hit(111+randint(111), "a Scroll of Chaos", MON_HARMFUL_SCROLL);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_RUMOUR+PO_K_IDX:
		{
			msg_print("There is message on the scroll. It says:");
			msg_print(NULL);
			switch(randint(20))
			{   case 1:
					msg_format("%v", get_rnd_line_f1, "chainswd.txt");
					break;
				case 2:
					msg_format("%v", get_rnd_line_f1, "error.txt");
					break;
				case 3: case 4: case 5:
					msg_format("%v", get_rnd_line_f1, "death.txt");
					break;
				default:
					msg_format("%v", get_rnd_line_f1, "rumors.txt");

			}
			msg_print(NULL);
			msg_print("The scroll disappears in a puff of smoke!");
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_SCROLL_ARTIFACT_CREATION+PO_K_IDX:
		{
			(void) artifact_scroll();
			(*ident) = TRUE;
			return SUCCESS;
		}
		case OBJ_STAFF_DARKNESS+PO_K_IDX:
		{
			if (!(p_ptr->resist_blind) && !(p_ptr->resist_dark))
			{
				if (add_flag(TIMED_BLIND, 3 + randint(5))) (*ident) = TRUE;
			}
			if (unlite_area(10, 3)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_HASTE_MONSTERS+PO_K_IDX:
		{
			if (speed_monsters()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_SUMMONING+PO_K_IDX:
		{
			switch (rand_int(32))
			{
				case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7:
					i = 1; break;
				case 20: case 21: case 22: case 23: case 24: case 25: case 26:
				case 27: case 28:
					i = 3; break;
				case 29: case 30: case 31:
					i = 4; break;
				default: i = 2; break;
			}
			for (; i; i--)
			{
				if (summon_specific(py, px, (dun_depth), 0))
				{
					(*ident) = TRUE;
				}
			}
			return SUCCESS;
		}

		case OBJ_STAFF_REMOVE_CURSE+PO_K_IDX:
		{
			if (remove_curse())
			{
				if (!p_ptr->blind)
				{
					msg_print("The staff glows blue for a moment...");
				}
				(*ident) = TRUE;
			}
			return SUCCESS;
		}

		case OBJ_STAFF_STARLIGHT+PO_K_IDX:
		{
			if (!p_ptr->blind)
			{
				msg_print("The end of the staff glows brightly...");
			}
			for (i = 0; i < 8; i++) lite_line(ddd[i]);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_ENLIGHTENMENT+PO_K_IDX:
		{
			map_area();
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_TREASURE_LOCATION+PO_K_IDX:
		{
			if (detect_treasure()) (*ident) = TRUE;
			if (detect_objects_gold()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_OBJECT_LOCATION+PO_K_IDX:
		{
			if (detect_objects_normal()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_DETECT_INVIS+PO_K_IDX:
		{
			if (detect_monsters_invis()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_DETECT_EVIL+PO_K_IDX:
		case SP_DETECT_EVIL:
		{
			if (detect_monsters_evil()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_CURE_LIGHT+PO_K_IDX:
		{
			if (hp_player(randint(8))) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_CURING+PO_K_IDX:
		{
			if (set_flag(TIMED_BLIND, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_POISONED, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CONFUSED, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_STUN, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CUT, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_IMAGE, 0)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_HEALING+PO_K_IDX:
		{
			if (hp_player(300)) (*ident) = TRUE;
			if (set_flag(TIMED_STUN, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CUT, 0)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_THE_MAGI+PO_K_IDX:
		{
			if (do_res_stat(A_INT)) (*ident) = TRUE;
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				(*ident) = TRUE;
				msg_print("Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);
			}
			if (p_ptr->cchi < p_ptr->mchi)
			{
				p_ptr->cchi = p_ptr->mchi;
				p_ptr->chi_frac = 0;
				(*ident) = TRUE;
				msg_print("Your feel your chi harmonise.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);
			}
			return SUCCESS;
		}

		case OBJ_STAFF_SLEEP_MONSTERS+PO_K_IDX:
		{
			if (sleep_monsters((plev))) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_SLOW_MONSTERS+PO_K_IDX:
		{
			if (slow_monsters(plev)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_SPEED+PO_K_IDX:
		{
			if (speed_up(16, 50)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_PROBING+PO_K_IDX:
		case OBJ_ROD_PROBING+PO_K_IDX:
		case RP_HALF_TITAN:
		{
			probing();
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_DISPEL_EVIL+PO_K_IDX:
		{
			if (dispel_evil(60)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_POWER+PO_K_IDX:
		{
			if (dispel_monsters(120)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_HOLINESS+PO_K_IDX:
		{
			if (dispel_evil(120)) (*ident) = TRUE;
			if (add_flag(TIMED_PROTEVIL, randint(25) + 3*plev)) (*ident) = TRUE;
			if (set_flag(TIMED_POISONED, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_AFRAID, 0)) (*ident) = TRUE;
			if (hp_player(50)) (*ident) = TRUE;
			if (set_flag(TIMED_STUN, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CUT, 0)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_STAFF_EARTHQUAKES+PO_K_IDX:
		{
			earthquake(py, px, 10);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_HEAL_MONSTER+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_bolt(GF_OLD_HEAL, dir, damroll(4, 6))) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_HASTE_MONSTER+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_bolt(GF_OLD_SPEED, dir, plev)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_CLONE_MONSTER+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_bolt(GF_OLD_CLONE, dir, 0)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_TELEPORT_OTHER+PO_K_IDX:
		case OBJ_ROD_TELEPORT_OTHER+PO_K_IDX:
		case ART_GNORRI+PO_NAME1:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_beam(GF_AWAY_ALL, dir, MAX_SIGHT * 5)) (*ident) = TRUE;
			return SUCCESS;
		}

		case ACT_TELE_AWAY+PO_ACTIVATION:
		case SP_TELEPORT_OTHER:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_beam(GF_AWAY_ALL, dir, plev)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_DISARMING+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (disarm_trap(dir)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_STONE_TO_MUD+PO_K_IDX:
		case ACT_STONE_MUD+PO_ACTIVATION:
		case ART_DESTINY+PO_NAME1:
		case SP_STONE_TO_MUD:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (wall_to_mud(dir)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_LIGHT+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_SLEEP_MONSTER+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_bolt(GF_OLD_SLEEP, dir, plev)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_SLOW_MONSTER+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_bolt(GF_OLD_SLOW, dir, plev)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_CONFUSE_MONSTER+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_bolt(GF_OLD_CONF, dir, 10)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_SCARE_MONSTER+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_bolt(GF_TURN_ALL, dir, 10)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_DRAIN_LIFE+PO_K_IDX:
		case OBJ_ROD_DRAIN_LIFE+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_bolt(GF_OLD_DRAIN, dir, 75)) (*ident) = TRUE;
			return SUCCESS;
		}

		case ART_JUSTICE+PO_NAME1:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt(GF_OLD_DRAIN, dir, 90);
			return SUCCESS;
		}

		case ACT_DRAIN_1+PO_ACTIVATION:
		case ART_MAGIC+PO_NAME1:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_bolt(GF_OLD_DRAIN, dir, 100))
			return SUCCESS;
		}

		case ACT_DRAIN_2+PO_ACTIVATION:
		case ART_THEODEN+PO_NAME1:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt(GF_OLD_DRAIN, dir, 120);
			return SUCCESS;
		}

		case OBJ_WAND_ANNIHILATION+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_bolt(GF_OLD_DRAIN, dir, 125)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_POLYMORPH+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_bolt(GF_OLD_POLY, dir, plev)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_MAGIC_MISSILE+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt_or_beam(20, GF_MISSILE, dir, damroll(2, 6));
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_ACID_BOLT+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt_or_beam(20, GF_ACID, dir, damroll(3, 8));
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_TAME_MONSTER+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (charm_monster(dir, 45))
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_FIRE_BOLT+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt_or_beam(20, GF_FIRE, dir, damroll(6, 8));
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_COLD_BOLT+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt_or_beam(20, GF_COLD, dir, damroll(3, 8));
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_WAND_SHARD_BALL+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_ball(GF_SHARD, dir, 75 + (randint(50)), 2);
			(*ident) = TRUE;
			return SUCCESS;
		}
		case OBJ_WAND_WONDER+PO_K_IDX:
		{
			return do_power(choose_random_wand(), plev, dir, FALSE, use, ident);
		}

		case OBJ_ROD_ENLIGHTENMENT+PO_K_IDX:
		{
			map_area();
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_ROD_DETECTION+PO_K_IDX:
		{
			detect_all();
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_ROD_CURING+PO_K_IDX:
		{
			if (set_flag(TIMED_BLIND, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_POISONED, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CONFUSED, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_STUN, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CUT, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_IMAGE, 0)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_ROD_HEALING+PO_K_IDX:
		{
			if (hp_player(500)) (*ident) = TRUE;
			if (set_flag(TIMED_STUN, 0)) (*ident) = TRUE;
			if (set_flag(TIMED_CUT, 0)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_ROD_RESTORATION+PO_K_IDX:
		case ACT_REST_ALL+PO_ACTIVATION:
		case SP_RESTORATION:
		{
			if (restore_level()) (*ident) = TRUE;
			if (do_res_stats()) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_ROD_SPEED+PO_K_IDX:
		{
			if (speed_up(16, 45)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_ROD_DISARMING+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (disarm_trap(dir)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_ROD_LIGHT+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_ROD_SLEEP_MONSTER+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_bolt(GF_OLD_SLEEP, dir, plev)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_ROD_SLOW_MONSTER+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_bolt(GF_OLD_SLOW, dir, plev)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_ROD_POLYMORPH+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_bolt(GF_OLD_POLY, dir, plev)) (*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_ROD_ACID_BOLT+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt_or_beam(10, GF_ACID, dir, damroll(6, 8));
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_ROD_ELEC_BOLT+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt_or_beam(10, GF_ELEC, dir, damroll(3, 8));
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_ROD_FIRE_BOLT+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt_or_beam(10, GF_FIRE, dir, damroll(8, 8));
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_ROD_COLD_BOLT+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt_or_beam(10, GF_COLD, dir, damroll(5, 8));
			(*ident) = TRUE;
			return SUCCESS;
		}

		case OBJ_ROD_HAVOC+PO_K_IDX:
		{
			call_chaos(plev);
			(*ident) = TRUE;
			return SUCCESS;
		}

		case ACT_SUNLIGHT+PO_ACTIVATION:
		{
		if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("A line of sunlight appears.");
			lite_line(dir);
				return SUCCESS;
			}

		case ACT_BO_MISS_1+PO_ACTIVATION:
		case ART_LIGHT+PO_NAME1:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt(GF_MISSILE, dir, damroll(2, 6));
			return SUCCESS;
		}

		case ACT_BO_ELEC_1+PO_ACTIVATION:
		case ART_WHITESPARK+PO_NAME1:
		case ART_CHARITY+PO_NAME1:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt(GF_ELEC, dir, damroll(4, 8));
			return SUCCESS;
		}

		case ACT_BO_ACID_1+PO_ACTIVATION:
		case ART_DEAD+PO_NAME1:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt(GF_ACID, dir, damroll(5, 8));
			return SUCCESS;
		}

		case ACT_BO_COLD_1+PO_ACTIVATION:
		case ART_GHOULS+PO_NAME1:
		case ART_HOPE+PO_NAME1:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt(GF_COLD, dir, damroll(6, 8));
			return SUCCESS;
		}

		case ACT_BO_FIRE_1+PO_ACTIVATION:
		case ART_IRONFIST+PO_NAME1:
		case ART_FAITH+PO_NAME1:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt(GF_FIRE, dir, damroll(9, 8));
			return SUCCESS;
		}

		case ACT_VAMPIRE_1+PO_ACTIVATION:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			for (i = 0; i < 3; i++)
			{
				if (fire_bolt(GF_OLD_DRAIN, dir, 50))
					hp_player(50);
			}
			return SUCCESS;
		}

		case ACT_BO_MISS_2+PO_ACTIVATION:
		case ART_COMBAT+PO_NAME1:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt(GF_ARROW, dir, 150);
			return SUCCESS;
		}

		case ACT_WHIRLWIND+PO_ACTIVATION:
	{
		{
		int y = 0, x = 0;
		cave_type       *c_ptr;
		monster_type    *m_ptr;

		for (dir = 0; dir < 8; dir++) {
			y = py + ddy_ddd[dir];
			x = px + ddx_ddd[dir];
			c_ptr = &cave[y][x];

			/* Get the monster */
			m_ptr = &m_list[c_ptr->m_idx];

			/* Hack -- attack monsters */
			if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x)))
			py_attack(y, x);
		}
		}
		return SUCCESS;
		}

		case ACT_VAMPIRE_2+PO_ACTIVATION:
		case SP_VAMPIRISM_TRUE:
		{
		if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			for (i = 0; i < 3; i++)
			{
				if (fire_bolt(GF_OLD_DRAIN, dir, 100))
					hp_player(100);
				}

				return SUCCESS;
			}

		case ACT_CALL_CHAOS+PO_ACTIVATION:
		{
			call_chaos(plev);
			return SUCCESS;
		}

		case ACT_SHARD+PO_ACTIVATION:
		{
		if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_ball(GF_SHARD, dir,
					120 + (plev), 2);
			return SUCCESS;
		}

		case ACT_DISP_EVIL+PO_ACTIVATION:
		case ART_ALHAZRED+PO_NAME1:
		{
			dispel_evil(plev * 5);
			return SUCCESS;
		}

		case ACT_BA_MISS_3+PO_ACTIVATION:
		{
		if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				msg_print("You breathe the elements.");
				fire_ball(GF_MISSILE, dir, 300, -4);
			return SUCCESS;
		}

		case ACT_DISP_GOOD+PO_ACTIVATION:
		{
			dispel_good(plev * 5);
			return SUCCESS;
		}

		case ACT_CONFUSE+PO_ACTIVATION:
		case ART_TOTILA+PO_NAME1:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (fire_bolt(GF_OLD_CONF, dir, 20)) (*ident) = TRUE;
			return SUCCESS;
		}

		case ACT_SLEEP+PO_ACTIVATION:
		case ART_DARKNESS+PO_NAME1:
		{
			sleep_monsters_touch(plev);
			return SUCCESS;
		}

		case ACT_QUAKE+PO_ACTIVATION:
		{
			earthquake(py, px, 10);
				return SUCCESS;
			}

		case ACT_TERROR+PO_ACTIVATION:
		case ART_POWER+PO_NAME1:
		case ART_MASK+PO_NAME1:
		{
			turn_monsters(40 + plev);
			return SUCCESS;
		}

		case ACT_BANISH_EVIL+PO_ACTIVATION:
		case SP_BANISH_2:
		{
			if (banish_evil(100))
			{
				msg_print("The power of the artifact banishes evil!");
			}
			return SUCCESS;
		}
		case SP_BANISH:
		{
			banish_monsters(plev*4);
			return SUCCESS;
		}
		case ACT_CHARM_ANIMAL+PO_ACTIVATION:
		{
		if (!dir) return POWER_ERROR_NO_SUCH_DIR;
		(void) charm_animal(dir, plev);
		return SUCCESS;
		}

		case ACT_CHARM_UNDEAD+PO_ACTIVATION:
		{
		if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			(void)control_one_undead(dir, plev);
				return SUCCESS;
			}

		case ACT_CHARM_OTHER+PO_ACTIVATION:
		{
		if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			(void) charm_monster(dir, plev);
			return SUCCESS;
		}

		case ACT_CHARM_ANIMALS+PO_ACTIVATION:
		{
			(void) charm_animals(plev * 2);
			return SUCCESS;
		}

		case ACT_CHARM_OTHERS+PO_ACTIVATION:
		{
			charm_monsters(plev * 2);
			return SUCCESS;
		}

		case ACT_SUMMON_ANIMAL+PO_ACTIVATION:
		{
			(void)summon_specific_friendly(py, px, plev, SUMMON_ANIMAL_RANGER, TRUE);
				return SUCCESS;
			}

		case ACT_SUMMON_PHANTOM+PO_ACTIVATION:
		{
			(void)summon_specific_friendly(py, px, (dun_depth), SUMMON_PHANTOM, TRUE);
			msg_print("You summon a phantasmal servant.");
			return SUCCESS;
		}

		case ACT_SUMMON_ELEMENTAL+PO_ACTIVATION:
		{
			summon_1(SUMMON_ELEMENTAL, (plev == 50), plev,
				"An elemental materializes...", "It seems obedient to you.",
				"You fail to control it!");
			return SUCCESS;
		}

		case ACT_SUMMON_UNDEAD+PO_ACTIVATION:
		{
			bool group = (plev > 24 && one_in(3));
			int type = (plev > 47) ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD;

			summon_1(type, group, plev, "Cold winds begin to blow around you, "
				"carrying with them the stench of decay...",
				"Ancient, long-dead forms arise from the ground to serve you!",
				"'The dead arise... to punish you for disturbing them!'");
			return SUCCESS;
		}

		case SP_SUMMON_DEMON:
		case ACT_SUMMON_DEMON+PO_ACTIVATION:
		{
			summon_1(SUMMON_DEMON, (plev == 50), plev,
				"The area fills with a stench of sulphur and brimstone.",
				"'What is thy bidding... Master?'",
				"'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
			return SUCCESS;
		}

		case ACT_CURE_LW+PO_ACTIVATION:
		{
			(void)set_flag(TIMED_AFRAID, 0);
			(void)hp_player(30);
			return SUCCESS;
		}

		case ACT_CURE_MW+PO_ACTIVATION:
		case ART_SPLEENSLICER+PO_NAME1:
		case SP_CURE_MEDIUM_WOUNDS:
		{
			hp_player(damroll(4, 8));
			(void)set_flag(TIMED_CUT, (p_ptr->cut / 2) - 50);
			return SUCCESS;
		}

		case ACT_CURE_POISON+PO_ACTIVATION:
		case ART_DANCING+PO_NAME1:
		{
			(void)set_flag(TIMED_AFRAID, 0);
			(void)set_flag(TIMED_POISONED, 0);
			return SUCCESS;
		}

		case ACT_CURE_700+PO_ACTIVATION:
		case ART_SUN+PO_NAME1:
		{
			msg_print("You feel a warm tingling inside...");
			(void)hp_player(700);
			(void)set_flag(TIMED_CUT, 0);
			return SUCCESS;
		}

		case ACT_CURE_1000+PO_ACTIVATION:
		case ART_SOULKEEPER+PO_NAME1:
		{
			msg_print("You feel much better...");
			(void)hp_player(1000);
			(void)set_flag(TIMED_CUT, 0);
			return SUCCESS;
		}

		case ACT_RESIST_ALL+PO_ACTIVATION:
		{
			(void)add_flag(TIMED_OPPOSE_ACID, randint(40) + 40);
			(void)add_flag(TIMED_OPPOSE_ELEC, randint(40) + 40);
			(void)add_flag(TIMED_OPPOSE_FIRE, randint(40) + 40);
			(void)add_flag(TIMED_OPPOSE_COLD, randint(40) + 40);
			(void)add_flag(TIMED_OPPOSE_POIS, randint(40) + 40);
			return SUCCESS;
		}

		case ACT_SPEED+PO_ACTIVATION:
		case ART_ITHAQUA+PO_NAME1:
		case ART_THUNDER+PO_NAME1:
		{
			if (speed_up(21, 40)) (*ident) = TRUE;
			return SUCCESS;
		}

		case ACT_XTRA_SPEED+PO_ACTIVATION:
		case ART_BAST+PO_NAME1:
		{
			if (speed_up(76, 150)) (*ident) = TRUE;
			return SUCCESS;
		}

		case ACT_WRAITH+PO_ACTIVATION:
		{
			add_flag(TIMED_WRAITH, randint(plev/2) + (plev/2));
			return SUCCESS;
		}

		case ACT_LIGHT+PO_ACTIVATION:
		case ART_POLARIS+PO_NAME1:
		{
			lite_area(damroll(2, 15), 3);
			return SUCCESS;
		}

		case ACT_MAP_LIGHT+PO_ACTIVATION:
		case ART_XOTH+PO_NAME1:
		{
			map_area();
			lite_area(damroll(2, 15), 3);
			return SUCCESS;
		}

		case ACT_DETECT_ALL+PO_ACTIVATION:
		case ART_SKULLKEEPER+PO_NAME1:
		{
			msg_print("An image forms in your mind...");
			detect_all();
			return SUCCESS;
		}

		case ACT_DETECT_XTRA+PO_ACTIVATION:
		case ART_ATAL+PO_NAME1:
		{
			detect_all();
			probing();
			identify_fully();
			return SUCCESS;
		}

		case ACT_RUNE_EXPLO+PO_ACTIVATION:
		case SP_EXPLOSIVE_RUNE:
		{
			explosive_rune();
			return SUCCESS;
		}

		case ACT_RUNE_PROT+PO_ACTIVATION:
		{
			warding_glyph();
			return SUCCESS;
		}

		case ACT_ALCHEMY+PO_ACTIVATION:
		{
			(void) alchemy();
			return SUCCESS;
		}

		case ACT_DIM_DOOR+PO_ACTIVATION:
		{
			if (!dimension_door(plev, 10)) (*ident) = FALSE;
			return SUCCESS;
		}

		case OBJ_DSM_BLUE+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You breathe lightning.");
			fire_ball(GF_ELEC, dir, 100, -2);
			return SUCCESS;
		}

		case OBJ_DSM_WHITE+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You breathe frost.");
			fire_ball(GF_COLD, dir, 110, -2);
			return SUCCESS;
		}

		case OBJ_DSM_BLACK+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You breathe acid.");
			fire_ball(GF_ACID, dir, 130, -2);
			return SUCCESS;
		}

		case OBJ_DSM_GREEN+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You breathe poison gas.");
			fire_ball(GF_POIS, dir, 150, -2);
			return SUCCESS;
		}

		case OBJ_DSM_RED+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You breathe fire.");
			fire_ball(GF_FIRE, dir, 200, -2);
			return SUCCESS;
		}

		case OBJ_DSM_MULTI_HUED+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			i = rand_int(5);
			msg_format("You breathe %s.",
						((i == 1) ? "lightning" :
						((i == 2) ? "frost" :
						((i == 3) ? "acid" :
							((i == 4) ? "poison gas" : "fire")))));
			fire_ball(((i == 1) ? GF_ELEC :
						((i == 2) ? GF_COLD :
						((i == 3) ? GF_ACID :
						((i == 4) ? GF_POIS : GF_FIRE)))),
						dir, 250, -2);
			return SUCCESS;
		}

		case OBJ_DSM_BRONZE+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You breathe confusion.");
			fire_ball(GF_CONFUSION, dir, 120, -2);
			return SUCCESS;
		}

		case OBJ_DSM_GOLD+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You breathe sound.");
			fire_ball(GF_SOUND, dir, 130, -2);
			return SUCCESS;
		}

		case OBJ_DSM_CHAOS+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			i = rand_int(2);
			msg_format("You breathe %s.",
						((i == 1 ? "chaos" : "disenchantment")));
			fire_ball((i == 1 ? GF_CHAOS : GF_DISENCHANT),
						dir, 220, -2);
			return SUCCESS;
		}

		case OBJ_DSM_LAW+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			i = rand_int(2);
			msg_format("You breathe %s.",
						((i == 1 ? "sound" : "shards")));
			fire_ball((i == 1 ? GF_SOUND : GF_SHARDS),
						dir, 230, -2);
			return SUCCESS;
		}

		case OBJ_DSM_BALANCE+PO_K_IDX:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			i = rand_int(4);
			msg_format("You breathe %s.",
						((i == 1) ? "chaos" :
						((i == 2) ? "disenchantment" :
						((i == 3) ? "sound" : "shards"))));
			fire_ball(((i == 1) ? GF_CHAOS :
						((i == 2) ? GF_DISENCHANT :
						((i == 3) ? GF_SOUND : GF_SHARDS))),
						dir, 250, -2);
			return SUCCESS;
		}

		case OBJ_DSM_PSEUDO+PO_K_IDX:
		{
		if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			i = rand_int(2);
			msg_format("You breathe %s.",
						((i == 0 ? "light" : "darkness")));
			fire_ball((i == 0 ? GF_LITE : GF_DARK), dir, 200, -2);
			return SUCCESS;
		}

		case OBJ_DSM_POWER+PO_K_IDX:
		{
		if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You breathe the elements.");
			fire_ball(GF_MISSILE, dir, 300, -3);
			return SUCCESS;
		}

		case ART_TRAPEZOHEDRON+PO_NAME1:
		{
			wiz_lite();
			msg_print("The gemstone drains your vitality...");
			take_hit(damroll(3, 8), "the Gemstone 'Trapezohedron'", MON_DANGEROUS_EQUIPMENT);
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			if (get_check("Activate recall? "))
			{
				set_recall(FALSE);
			}

			return SUCCESS;
		}

		case ART_NYARLATHOTEP+PO_NAME1:
		{
		if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			ring_of_power(dir);
			return SUCCESS;
		}

		case ART_RAZORBACK+PO_NAME1:
		{
			for (i = 0; i < 8; i++) fire_ball(GF_ELEC, ddd[i], 150, 3);
			return SUCCESS;
		}

		case ART_BLADETURNER+PO_NAME1:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You breathe the elements.");
			fire_ball(GF_MISSILE, dir, 300, -4);
			msg_print("Your armor glows many colours...");
			(void)set_flag(TIMED_AFRAID, 0);
			(void)add_flag(TIMED_SHERO, randint(50) + 50);
			(void)hp_player(30);
			(void)add_flag(TIMED_BLESSED, randint(50) + 50);
			(void)add_flag(TIMED_OPPOSE_ACID, randint(50) + 50);
			(void)add_flag(TIMED_OPPOSE_ELEC, randint(50) + 50);
			(void)add_flag(TIMED_OPPOSE_FIRE, randint(50) + 50);
			(void)add_flag(TIMED_OPPOSE_COLD, randint(50) + 50);
			(void)add_flag(TIMED_OPPOSE_POIS, randint(50) + 50);
			return SUCCESS;
		}

		case ART_VAMPLORD+PO_NAME1:
		{
			(void)set_flag(TIMED_POISONED, 0);
			(void)set_flag(TIMED_CUT, 0);
			(void)set_flag(TIMED_STUN, 0);
			(void)set_flag(TIMED_CONFUSED, 0);
			(void)set_flag(TIMED_BLIND, 0);
			(void)add_flag(TIMED_HERO, randint(25) + 25);
			(void)hp_player(777);
			return SUCCESS;
		}

		case ART_KARAKAL+PO_NAME1:
		{
			switch(randint(13))
			{
				case 1: case 2: case 3: case 4: case 5:
					teleport_player(10);
					return SUCCESS;
				case 6: case 7: case 8: case 9: case 10:
					teleport_player(222);
					return SUCCESS;
				case 11: case 12:
					(void)stair_creation();
					return SUCCESS;
				default:
					if(get_check("Leave this level? "))
					{
						change_level(dun_level, START_RANDOM);
					}
					return SUCCESS;
			}
		}

		case ART_DAWN+PO_NAME1:
		{
			(void)summon_specific_friendly(py, px, (dun_depth), SUMMON_REAVER, TRUE);
			return SUCCESS;
		}

		case ART_DEATH+PO_NAME1:
		{
			(void)brand_bolts();
			return SUCCESS;
		}

		case TV_FOOD+PO_TVAL:
		{
			msg_print("That tastes good.");
			(*ident) = TRUE;
			(*use) = FALSE;
			return SUCCESS;
		}

		case SP_PRECOGNITION:
		{
			if (plev > 44)
			{
				wiz_lite();
			}
			else if (plev > 19)
			{
				map_area();
			}
			if (plev < 30)
			{
				b = detect_monsters_normal();
				if (plev > 19)  b |=  detect_monsters_invis();
				if (plev > 14)
				{
					b |= detect_doors();
					b |= detect_stairs();
				}
				if (plev > 4)
				{
					b |=  detect_traps();
				}
			}
			else
			{
				b = detect_all();
			}
			if (!b)  msg_print("You feel safe.");
			return SUCCESS;
		}
		case SP_NEURAL_BLAST:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;

			fire_bolt_or_beam(plev*2-1, GF_PSI, dir,
				damroll(3 + ((plev - 1) / 4), (3+plev/15)));

			return SUCCESS;
		}
		case SP_MINOR_DISPLACEMENT:
		{
			if (plev < 25)
			{
				teleport_player(10);
			}
			else
			{
				if (!dimension_door(plev, 20)) (*use) = FALSE;
			}
			return SUCCESS;
		}
		case SP_MAJOR_DISPLACEMENT:
		{
			if (plev > 29) banish_monsters(plev);
			teleport_player(plev * 5);
			return SUCCESS;
		}
		case SP_DOMINATION:
		{
			if (plev < 30)
			{
				if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				fire_ball(GF_DOMINATION, dir, plev, 0);
			}
			else
			{
				charm_monsters(plev * 2);
			}
			return SUCCESS;
		}
		case SP_PULVERISE:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_ball(GF_SOUND, dir, damroll(8+((plev-5)/4), 8),(plev > 20 ? (plev-20)/8 + 1 : 0));
			return SUCCESS;
		}
		case SP_CHARACTER_ARMOUR:
		{
			add_flag(TIMED_SHIELD, plev);
			if (plev > 14)   add_flag(TIMED_OPPOSE_ACID, plev);
			if (plev > 19)   add_flag(TIMED_OPPOSE_FIRE, plev);
			if (plev > 24)   add_flag(TIMED_OPPOSE_COLD, plev);
			if (plev > 29)   add_flag(TIMED_OPPOSE_ELEC, plev);
			if (plev > 34)   add_flag(TIMED_OPPOSE_POIS, plev);
			return SUCCESS;
		}
		case SP_PSYCHOMETRY:
		{
			if (plev < 40)
			{
				psychometry();
			}
			else
			{
				ident_spell();
			}
			return SUCCESS;
		}
		case SP_MIND_WAVE:
		{
			msg_print("Mind-warping forces emanate from your brain!");
			if (plev < 25)
			{
				project(0, 2+plev/10, py, px,(plev*3)/2, GF_PSI, PROJECT_KILL);
			}
			else
			{
				(void)mindblast_monsters(plev * ((plev-5) / 10 + 1));
			}
			return SUCCESS;
		}
		case SP_ADRENALINE_CHANNELING:
		{
			set_flag(TIMED_AFRAID, 0);
			set_flag(TIMED_STUN, 0);
			hp_player(plev);
			b = 10 + randint((plev*3)/2);
			if (plev < 35)
			{
				add_flag(TIMED_HERO, b);
			}
			else
			{
				add_flag(TIMED_SHERO, b);
			}
			add_flag(TIMED_FAST, b);
			return SUCCESS;
		}
		case SP_PSYCHIC_DRAIN:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			b = damroll(plev/2, 6);
			if (fire_ball(GF_PSI_DRAIN, dir, b,  0 + (plev-25)/10))
			p_ptr->energy -= randint(TURN_ENERGY*15/10);
			return SUCCESS;
		}
		case SP_TELEKINETIC_WAVE:
		{
			msg_print("A wave of pure physical force radiates out from your body!");
			project(0, 3+plev/10, py, px,
			plev * (plev > 39 ? 4 : 3), GF_TELEKINESIS, PROJECT_KILL|PROJECT_ITEM|PROJECT_GRID);
			return SUCCESS;
		}

		case SP_CURE_LIGHT_WOUNDS_2:
		{
			(void)hp_player(damroll(2, 10));
			(void)set_flag(TIMED_CUT, p_ptr->cut - 10);
			return SUCCESS;
		}
		case SP_LIGHT_AREA:
		case MUT_ILLUMINE+PO_MUTA:
		{
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			return SUCCESS;
		}
		case SP_DETECT_DOORS_AND_TRAPS:
		{
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			return SUCCESS;
		}
		case SP_CURE_MEDIUM_WOUNDS_2:
		{
				(void)hp_player(damroll(4, 10));
				(void)set_flag(TIMED_CUT, (p_ptr->cut / 2) - 20);
			return SUCCESS;
		}
		case SP_REMOVE_CURSE:
		{
				remove_curse();
			return SUCCESS;
		}
		case SP_CURE_CRITICAL_WOUNDS:
		{
				(void)hp_player(damroll(8, 10));
				(void)set_flag(TIMED_STUN, 0);
				(void)set_flag(TIMED_CUT, 0);
			return SUCCESS;
		}
		case SP_HOLY_ORB:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				fire_ball(GF_HOLY_FIRE, dir,
					(damroll(3, 6) + plev + (plev /  4)),((plev < 30) ? 2 : 3));
			return SUCCESS;
		}
		case SP_HEALING:
		{
				(void)hp_player(300);
				(void)set_flag(TIMED_STUN, 0);
				(void)set_flag(TIMED_CUT, 0);
			return SUCCESS;
		}
		case SP_GLYPH_OF_WARDING:
		{
				warding_glyph();
			return SUCCESS;
		}
		case SP_EXORCISM:
		{
			(void) dispel_undead(plev);
			(void) dispel_demons(plev);
			(void) turn_evil(plev);
			return SUCCESS;
		}
		case SP_DISPEL_CURSE:
		{
				(void)remove_all_curse();
			return SUCCESS;
		}
		case SP_DISPEL_UNDEAD_AND_DEMONS:
		{
				(void)dispel_undead(plev * 3);
			(void)dispel_demons(plev * 3);
			return SUCCESS;
		}
		case SP_DAY_OF_THE_DOVE:
		{
			charm_monsters(plev * 2);
			return SUCCESS;
		}
		case SP_DISPEL_EVIL:
		{
				(void)dispel_evil(plev * 4);
			return SUCCESS;
		}
		case SP_HOLY_WORD:
		{
			(void)dispel_evil(plev * 4);
				(void)hp_player(1000);
				(void)set_flag(TIMED_AFRAID, 0);
				(void)set_flag(TIMED_POISONED, 0);
				(void)set_flag(TIMED_STUN, 0);
				(void)set_flag(TIMED_CUT, 0);
			return SUCCESS;
		}
		case SP_WARDING_TRUE:
		{
			warding_glyph();
			glyph_creation();
			return SUCCESS;
		}
		case SP_HEROISM:
		{
				(void)add_flag(TIMED_HERO, randint(25) + 25);
				(void)hp_player(10);
				(void)set_flag(TIMED_AFRAID, 0);
			return SUCCESS;
		}
		case SP_BLESS_WEAPON:
		{
				bless_weapon();
			return SUCCESS;
		}
		case SP_HEALING_TRUE:
		{
			(void)hp_player(2000);
			(void)set_flag(TIMED_STUN, 0);
			(void)set_flag(TIMED_CUT, 0);
			return SUCCESS;
		}
		case SP_DIVINE_INTERVENTION:
		{
			project(0, 1, py, px, 777, GF_HOLY_FIRE,   PROJECT_KILL);
			dispel_monsters(plev * 4);
			slow_monsters(plev * 4);
			stun_monsters(plev*4);
			confuse_monsters(plev*4);
			turn_monsters(plev*4);
			stasis_monsters(plev*4);
			(void)add_flag(TIMED_SHERO, randint(25) + 25);
			(void)hp_player(300);
			speed_up(plev+1, 20+plev*2); /* Haste */
			(void)set_flag(TIMED_AFRAID, 0);
			return SUCCESS;
		}
		case SP_DETECT_MONSTERS:
		case MUT_SMELL_MON+PO_MUTA:
		{
				(void)detect_monsters_normal();
			return SUCCESS;
		}
		case SP_FIRST_AID:
		{
				(void)hp_player(damroll(2, 8));
				(void)set_flag(TIMED_CUT, p_ptr->cut - 15);
			return SUCCESS;
		}
		case SP_DAYLIGHT:
		{
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			if (p_ptr->hurt_light && !(p_ptr->resist_lite))
			{
				msg_print("The daylight scorches your flesh!");
				take_hit(damroll(2, 2), "daylight", MON_LIGHT);
			}
			return SUCCESS;
		}
		case SP_ANIMAL_TAMING:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			(void) charm_animal(dir, plev);
			return SUCCESS;
		}
		case SP_CURE_WOUNDS_AND_POISON:
		{
				(void)set_flag(TIMED_CUT, 0);
				(void)set_flag(TIMED_POISONED, 0);
			return SUCCESS;
		}
		case SP_LIGHTNING_BOLT:
		{
					if (!dir) return POWER_ERROR_NO_SUCH_DIR;
					fire_bolt_or_beam(plev-10, GF_ELEC, dir,
								damroll(3+((plev-5)/4), 8));
			return SUCCESS;
		}
		case SP_NATURE_AWARENESS:
		{
				map_area();
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				(void)detect_monsters_normal();
			return SUCCESS;
		}
		case SP_FROST_BOLT:
		{
				if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				fire_bolt_or_beam(plev-10, GF_COLD, dir,
					damroll(5+((plev-5)/4), 8));
			return SUCCESS;
		}
		case SP_RAY_OF_SUNLIGHT:
		{
				if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				msg_print("A line of sunlight appears.");
				lite_line(dir);
			return SUCCESS;
		}
		case SP_ENTANGLE:
		{
				slow_monsters(plev * 4);
			return SUCCESS;
		}
		case SP_SUMMON_ANIMAL_2:
		{
			if (!(summon_specific_friendly(py, px, plev, SUMMON_ANIMAL_RANGER, TRUE)))
				msg_print("No animals arrive.");
			return SUCCESS;
		}
		case SP_HERBAL_HEALING:
		{
				(void)hp_player(1000);
				(void)set_flag(TIMED_STUN, 0);
				(void)set_flag(TIMED_CUT, 0);
				(void)set_flag(TIMED_POISONED, 0);
			return SUCCESS;
		}
		case SP_DOOR_BUILDING:
		{
				(void)door_creation();
			return SUCCESS;
		}
		case SP_STAIR_BUILDING:
		{
				(void)stair_creation();
			return SUCCESS;
		}
		case SP_ANIMAL_FRIENDSHIP:
		{
			(void) charm_animals(plev * 2);
			return SUCCESS;
		}
		case SP_WALL_OF_STONE:
		{
			(void)wall_stone();
			return SUCCESS;
		}
		case SP_PROTECT_FROM_CORROSION:
		{
					rustproof();
			return SUCCESS;
		}
		case SP_EARTHQUAKE:
		{
				earthquake(py, px, 10);
			return SUCCESS;
		}
		case SP_WHIRLWIND_ATTACK:
		{
			{
			int y = 0, x = 0;
			cave_type       *c_ptr;
			monster_type    *m_ptr;

			for (dir = 0; dir < 8; dir++) {
				y = py + ddy_ddd[dir];
				x = px + ddx_ddd[dir];
				c_ptr = &cave[y][x];

				/* Get the monster */
				m_ptr = &m_list[c_ptr->m_idx];

				/* Hack -- attack monsters */
				if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x)))
				py_attack(y, x);
			}
			}
			return SUCCESS;
		}
		case SP_BLIZZARD:
		{
				if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				fire_ball(GF_COLD, dir,
					70 + (plev), (plev/12)+1);
			return SUCCESS;
		}
		case SP_LIGHTNING_STORM:
		{
				if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				fire_ball(GF_ELEC, dir,
					90 + (plev), (plev/12)+1);
			return SUCCESS;
		}
		case SP_WHIRLPOOL:
		{
				if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				fire_ball(GF_WATER, dir,
					100 + (plev), (plev/12)+1);
			return SUCCESS;
		}
		case SP_CALL_SUNLIGHT:
		{

				fire_ball(GF_LITE, 0, 150, 8);
				wiz_lite();
				if (p_ptr->hurt_light && !(p_ptr->resist_lite))
				{
					msg_print("The sunlight scorches your flesh!");
					take_hit(50, "sunlight", MON_LIGHT);
				}
			return SUCCESS;
		}
		case SP_ELEMENTAL_BRANDING:
		{
				brand_weapon(-1);
			return SUCCESS;
		}
		case SP_NATURES_WRATH:
		{
				(void)dispel_monsters(plev * 4);
				earthquake(py, px, 20 + (plev / 2) );
			project(0, 1+plev/12, py, px,
				100+plev, GF_DISINTEGRATE, PROJECT_KILL|PROJECT_ITEM);
			return SUCCESS;
		}
		case SP_CONFUSE_MONSTER:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			(void)fire_bolt(GF_OLD_CONF, dir, ( plev * 3) / 2 );
			return SUCCESS;
		}
		case SP_TELEPORT:
		{
			teleport_player(plev * 5);
			return SUCCESS;
		}
		case SP_TELEPORT_2:
		{
			teleport_player(plev * 4);
			return SUCCESS;
		}
		case SP_SLEEP_MONSTER:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			(void)fire_bolt(GF_OLD_SLEEP, dir,plev);
			return SUCCESS;
		}
		case SP_RECHARGING:
		{
				(void)recharge(plev * 2);
			return SUCCESS;
		}
		case SP_MAGIC_MAPPING:
		{
			map_area();
			return SUCCESS;
		}
		case SP_SLOW_MONSTER:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			(void)fire_bolt(GF_OLD_SLOW, dir,plev);
			return SUCCESS;
		}
		case SP_MASS_SLEEP:
		{
			(void)sleep_monsters(plev);
			return SUCCESS;
		}
		case SP_HASTE_SELF:
		{
			if (speed_up(1+plev, 20+plev*2)) (*ident) = TRUE;
			return SUCCESS;
		}
		case SP_DETECTION_TRUE:
		{
			(void)detect_all();
			return SUCCESS;
		}
		case SP_DETECT_OBJECTS_AND_TREASURE:
		{
			(void)detect_objects_normal();
			(void)detect_treasure();
			(void)detect_objects_gold();
			return SUCCESS;
		}
		case SP_DETECT_ENCHANTMENT:
		{
			(void)detect_objects_magic();
			return SUCCESS;
		}
		case SP_CHARM_MONSTER:
		{
				if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				(void) charm_monster(dir, plev);
			return SUCCESS;
		}
		case SP_DIMENSION_DOOR:
		{
		{
			if (!dimension_door(plev, 10)) (*use) = FALSE;
			return SUCCESS;
		}
		}

		case SP_SELF_KNOWLEDGE:
		{
			(void)self_knowledge();
			return SUCCESS;
		}
		case SP_WORD_OF_RECALL:
		{
			set_recall(TRUE);
			return SUCCESS;
		}
		case SP_STASIS:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			(void)fire_bolt(GF_STASIS, dir,plev);
			return SUCCESS;
		}
		case SP_TELEKINESIS:
		{
		if (!dir) return POWER_ERROR_NO_SUCH_DIR;
		fetch(dir, plev*15, FALSE);
			return SUCCESS;
		}
		case SP_ENCHANT_WEAPON:
		{
			(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
			return SUCCESS;
		}
		case SP_ENCHANT_ARMOUR:
		{
			(void)enchant_spell(0, 0, rand_int(3) + 2);
			return SUCCESS;
		}
		case SP_ALCHEMY:
		{
				(void) alchemy();
			return SUCCESS;
		}

		case SP_MAGIC_MISSILE:
		{
				if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				fire_bolt_or_beam(plev-10, GF_MISSILE, dir,
							damroll(3 + ((plev - 1) / 5), 4));
			return SUCCESS;
		}
		case SP_TOUCH_OF_CONFUSION:
		{
			if (!(p_ptr->confusing))
			{
				msg_print("Your hands start glowing.");
				p_ptr->confusing = TRUE;
			}
			return SUCCESS;
		}
		case SP_MANA_BURST:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_ball(GF_MISSILE, dir,
			(damroll(3, 5) + plev +
			(plev / 4)),
			((plev < 30) ? 2 : 3));
			/* Shouldn't actually use GF_MANA, as it will destroy all
		* items on the floor */
			return SUCCESS;
		}
		case SP_FIRE_BOLT:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt_or_beam(plev, GF_FIRE, dir,
				damroll(8+((plev-5)/4), 8));
			return SUCCESS;
		}
		case SP_FIST_OF_FORCE:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_ball(GF_DISINTEGRATE, dir,
				damroll(8+((plev-5)/4), 8), 0);
			return SUCCESS;
		}
		case SP_WONDER:
		{
			{
			/* This spell should become more useful (more
				controlled) as the player gains experience levels.
				Thus, add 1/5 of the player's level to the die roll.
				This eliminates the worst effects later on, while
				keeping the results quite random.  It also allows
				some potent effects only at high level. */

				int die = randint(100) + plev / 5;

				if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				if (die > 100)
					msg_print ("You feel a surge of power!");
				if (die < 8) fire_bolt(GF_OLD_CLONE, dir, 0);
				else if (die < 14) fire_bolt(GF_OLD_SPEED, dir, plev);
				else if (die < 26) fire_bolt(GF_OLD_HEAL, dir, plev);
				else if (die < 31) fire_bolt(GF_OLD_POLY, dir, plev);
				else if (die < 36)
					fire_bolt_or_beam (plev - 10,
					GF_MISSILE, dir,
					damroll(3 + ((plev - 1) / 5), 4));
				else if (die < 41) fire_bolt(GF_OLD_CONF, dir, plev);
				else if (die < 46) fire_ball (GF_POIS, dir, 20 + (plev / 2), 3);
				else if (die < 51) lite_line (dir);
				else if (die < 56)
					fire_bolt_or_beam (plev - 10, GF_ELEC, dir,
					damroll(3+((plev-5)/4), 8));
				else if (die < 61)
					fire_bolt_or_beam (plev - 10, GF_COLD, dir,
					damroll(5+((plev-5)/4), 8));
				else if (die < 66)
					fire_bolt_or_beam (plev, GF_ACID, dir,
					damroll(6+((plev-5)/4), 8));
				else if (die < 71)
					fire_bolt_or_beam (plev, GF_FIRE, dir,
					damroll(8+((plev-5)/4), 8));
				else if (die < 76) fire_bolt(GF_OLD_DRAIN, dir, 75);
				else if (die < 81) fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
				else if (die < 86) fire_ball(GF_ACID, dir, 40 + plev, 2);
				else if (die < 91) fire_ball(GF_ICE, dir, 70 + plev, 3);
				else if (die < 96) fire_ball(GF_FIRE, dir, 80 + plev, 3);
				else if (die < 101) fire_bolt(GF_OLD_DRAIN, dir, 100 + plev);
				else if (die < 104) earthquake(py, px, 12);
				else if (die < 106) destroy_area(py, px, 15, TRUE);
				else if (die < 108) genocide(TRUE);
				else if (die < 110) dispel_monsters(120);
				else /* RARE */
				{
					dispel_monsters (150);
					slow_monsters(plev);
					sleep_monsters(plev);
					hp_player (300);
				}
			return SUCCESS;
		}
			}
		case SP_CHAOS_BOLT:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt_or_beam(plev, GF_CHAOS, dir,
				damroll(10+((plev-5)/4), 8));
			return SUCCESS;
		}
		case SP_SONIC_BOOM:
		{
					project(0, 2+plev/10, py, px,
				45+plev, GF_SOUND, PROJECT_KILL|PROJECT_ITEM);
			return SUCCESS;
		}
		case SP_DOOM_BOLT:
		{
				if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				fire_beam(GF_MANA, dir, damroll(11+((plev-5)/4), 8));
			return SUCCESS;
		}
		case SP_FIRE_BALL:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_ball(GF_FIRE, dir,
					55 + (plev), 2);
			return SUCCESS;
		}
		case SP_INVOKE_CHAOS:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_ball(GF_CHAOS, dir,
					66 + (plev), (plev / 5));
			return SUCCESS;
		}
		case SP_POLYMORPH_OTHER:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			(void)fire_bolt(GF_OLD_POLY, dir,plev);
			return SUCCESS;
		}
		case SP_CHAIN_LIGHTNING:
		{
			for (dir = 0; dir <= 9; dir++)
			fire_beam(GF_ELEC, dir, damroll(5+(plev/10), 8));
			return SUCCESS;
		}
		case SP_ARCANE_BINDING:
		{
			(void)recharge(40);
			return SUCCESS;
		}
		case SP_DISINTEGRATE:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_ball(GF_DISINTEGRATE, dir,
				80 + (plev), 3 + (plev/40));
			return SUCCESS;
		}
		case SP_ALTER_REALITY:
		{
			msg_print("The world changes!");
			change_level(dun_level, START_RANDOM);
			return SUCCESS;
		}
		case SP_POLYMORPH_SELF:
		{
			do_poly_self();
			return SUCCESS;
		}
		case SP_CHAOS_BRANDING:
		{
		brand_weapon(EGO_CHAOTIC);
			return SUCCESS;
		}
		case SP_BEAM_OF_GRAVITY:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				fire_beam(GF_GRAVITY, dir, damroll(9+((plev-5)/4), 8));
			return SUCCESS;
		}
		case SP_METEOR_SWARM:
		{
			{
				int x, y, dx, dy, d, count = 0;
				int b = 10 + randint(10);
				for (i = 0; i < b; i++) {
				do {
					count++;
					if (count > 1000)  break;
					x = px - 5 + randint(10);
					y = py - 5 + randint(10);
					dx = (px > x) ? (px - x) : (x - px);
					dy = (py > y) ? (py - y) : (y - py);
					/* Approximate distance */
					d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));
				} while ((d > 5) || (!(player_has_los_bold(y, x))));

				if (count > 1000)   break;
				count = 0;
				project(0, 2, y, x, (plev*3)/2, GF_METEOR, PROJECT_KILL|PROJECT_JUMP|PROJECT_ITEM);
				}
			}
			return SUCCESS;
		}
		case SP_FLAME_STRIKE:
		{
			fire_ball(GF_FIRE, 0,
				150 + (2*plev), 8);
			return SUCCESS;
		}
		case SP_CALL_CHAOS:
		{
			call_chaos(plev);
			return SUCCESS;
		}
		case SP_SHARD_BALL:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_ball(GF_SHARD, dir,
					120 + (plev), 2);
			return SUCCESS;
		}
		case SP_MANA_STORM:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_ball(GF_MANA, dir,
				300 + (plev * 2), 4);
			return SUCCESS;
		}
		case SP_BREATHE_CHAOS:
		{
				if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				fire_ball(GF_CHAOS,dir,p_ptr->chp,
					-2);
			return SUCCESS;
		}
		case SP_CALL_THE_VOID:
		{
			call_the_();
			return SUCCESS;
		}

		case SP_MIND_BLAST:
		{
				if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				fire_bolt_or_beam(plev-10, GF_PSI, dir,
								damroll(3 + ((plev - 1) / 5), 3));
			return SUCCESS;
		}
		case SP_TAROT_DRAW:
		{

			{
				/* A limited power 'wonder' spell */

				int die = die = (randint(110)) + plev / 5;
				/* get a level bonus */

			msg_print("You shuffle your Tarot deck and draw a card...");

			if (die < 7 )
			{
				msg_print("Oh no! It's the Blasted Tower!");
				for (i = 0; i < randint(3); i++)
					(void)activate_hi_summon();
			}
			else if (die < 14)
			{
				msg_print("Oh no! It's the Devil!");
				(void) summon_specific(py, px, (dun_depth), SUMMON_DEMON);
			}
			else if (die < 18 )
			{
				msg_print("Oh no! It's the Hanged Man.");
				activate_ty_curse();
			}
			else if (die < 22 )
			{
				msg_print("It's the swords of discord.");
				aggravate_monsters(NULL);
			}
			else if (die < 26)
			{
				msg_print("It's the Fool.");
				(void) do_dec_stat(A_INT);
				(void) do_dec_stat(A_WIS);
			}
			else if (die < 30)
			{
				msg_print("It's a picture of a strange monster.");
				if (!(summon_specific(py, px, ((dun_depth) * 3) / 2, 32 + randint(6))))
					msg_print("Nobody answers to your call.");
			}
			else if (die < 33)
			{
				msg_print("It's the Moon.");
				unlite_area(10, 3);
			}
			else if (die < 38)
			{
				msg_print("It's the Wheel of Fortune.");
				wild_magic((randint(32))-1);
			}
			else if (die < 40)
			{
				msg_print("It's a teleport card.");
				teleport_player(10);
			}
			else if (die <42)
			{
				msg_print("It's the Star.");
				add_flag(TIMED_BLESSED, plev);
			}
			else if (die <47)
			{
				msg_print("It's a teleport card.");
				teleport_player(100);
			}
			else if (die <52)
			{
				msg_print("It's a teleport card.");
				teleport_player(200);
			}
			else if (die <60)
			{
				msg_print("It's the Tower.");
				wall_breaker(plev);
			}
			else if (die <72)
			{
				msg_print("It's Temperance.");
				sleep_monsters_touch(plev);
			}
			else if (die <80)
			{
				msg_print("It's the Tower.");
				earthquake(py, px, 5);
			}
			else if (die<82)
			{
				msg_print("It's a picture of a friendly monster.");
				if (!(summon_specific_friendly(py, px, ((dun_depth) * 3) / 2, SUMMON_MOULD, FALSE)))
					msg_print("Nobody answers to your call.");
			}
			else if (die<84)
			{
				msg_print("It's a picture of a friendly monster.");
				if (!(summon_specific_friendly(py, px, ((dun_depth) * 3) / 2, SUMMON_BAT, FALSE)))
					msg_print("Nobody answers to your call.");
			}
			else if (die<86)
			{
				msg_print("It's a picture of a friendly monster.");
				if (!(summon_specific_friendly(py, px, ((dun_depth) * 3) / 2, SUMMON_VORTEX, FALSE)))
					msg_print("Nobody answers to your call.");
			}
			else if (die<88)
			{
				msg_print("It's a picture of a friendly monster.");
				if (!(summon_specific_friendly(py, px, ((dun_depth) * 3) / 2, SUMMON_TREASURE, FALSE)))
					msg_print("Nobody answers to your call.");
			}
			else if (die<96)
			{
				msg_print("It's the Lovers.");
				if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				(void) charm_monster(dir, MIN(plev, 20));
			}
			else if (die<101)
			{
				msg_print("It's the Hermit.");
				wall_stone();
			}
			else if (die< 111)
			{
				msg_print("It's the Judgement.");
				do_cmd_rerate();
				cure_mutations();
			}
			else if (die < 120)
			{
				msg_print("It's the Sun.");
				wiz_lite();
			}
			else
			{
				msg_print("It's the World.");
					msg_print("You feel more experienced.");
					gain_skills(100);
				}

			}
			return SUCCESS;
		}
		case SP_RESET_RECALL:
		{
			char ppp[] = "Reset to which level (1-   ): ", tmp_val[4];
				/* Prompt */
				sprintf(ppp, "Reset to which level (1-%d): ", p_ptr->max_dlv);

				/* Default */
				sprintf(tmp_val, "%d", MAX(dun_level, 1));

				/* Ask for a level */
				if (!get_string(ppp, tmp_val, 4))
				{
					(*use) = FALSE;
					return SUCCESS;
				}

				/* Extract request */
				i = atoi(tmp_val);

				/* Paranoia */
				if (i < 1) i = 1;

				/* Paranoia */
				if (i > p_ptr->max_dlv) i = p_ptr->max_dlv;

				/* Accept request */
				msg_format("Recall depth set to level %d (%d').", i, i * 50 );
			return SUCCESS;
		}
		case SP_SUMMON_OBJECT:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				fetch(dir, plev*15, TRUE);
			return SUCCESS;
		}
		case SP_SUMMON_ANIMAL:
		{
			bool friendly = percent(60);

			int type = (friendly) ? SUMMON_ANIMAL_RANGER : SUMMON_ANIMAL;

			msg_print ("You reach out your mind to the wilderness...");

			if (!summon_specific_aux(py, px, plev, type, !friendly, friendly))
			{
				msg_print("Nobody answers to your call.");
			}
			else if (!friendly)
			{
				msg_print("The summoned animal gets angry!");
			}
			return SUCCESS;
		}
		case SP_PHANTASMAL_SERVANT:
		{
			if (summon_specific_friendly(py, px, (plev*3)/2,
				SUMMON_PHANTOM, FALSE))
			{
				msg_print ("'Your wish, master?'");
			}
			else
			{
				msg_print("Nobody answers to your call.");
			}
			return SUCCESS;
		}
		case SP_SUMMON_MONSTER:
		{
			msg_print ("You reach out your mind...");
			summon_2(0, 60, FALSE, plev, NULL, "creature");
			return SUCCESS;
		}
		case SP_CONJURE_ELEMENTAL:
		{
			if (!summon_2(SUMMON_ELEMENTAL, 50, FALSE, plev, NULL, NULL))
			{
				msg_print("You fail to control the elemental creature!");
			}
			return SUCCESS;
		}
		case SP_JOKER_CARD:
		{
			int joke_monsters[] =
			{
				SUMMON_MOULD,
				SUMMON_BAT,
				SUMMON_VORTEX,
				SUMMON_TREASURE,
			};
			i = RAND_ELEMENT(joke_monsters);

			msg_print("You concentrate on a joker card...");

			summon_2(RAND_ELEMENT(joke_monsters), 50, FALSE, plev,
				NULL, "creature");

			return SUCCESS;
		}
		case SP_SUMMON_SPIDERS:
		{
			msg_print ("You reach out your mind along the planar webs...");
			summon_2(SUMMON_SPIDER, 60, TRUE, plev,
				NULL, "spider");
			return SUCCESS;
		}
		case SP_SUMMON_REPTILES:
		{
			summon_2(SUMMON_HYDRA, 60, TRUE, plev,
				"the cold, damp places", "reptile");
			return SUCCESS;
		}
		case SP_SUMMON_HOUNDS:
		{
			summon_2(SUMMON_HOUND, 60, TRUE, plev,
				"the elemental planes", "hound");
			return SUCCESS;
		}
		case SP_PLANAR_BRANDING:
		{
			brand_weapon(EGO_PLANAR);
			return SUCCESS;
		}
		case SP_PLANAR_BEING:
		{
			if (one_in(8)) i = MUT_WRAITH;
			else i = MUT_BLINK;
			if (gain_chaos_feature(i))
				msg_print("You have turned into a Planar Being.");
			return SUCCESS;
		}
		case SP_DEATH_DEALING:
		{
			(void)dispel_living(plev * 3);
			return SUCCESS;
		}
		case SP_SUMMON_REAVER:
		{
			summon_2(SUMMON_REAVER, 70, FALSE, plev,
				"the planes of order", "Black Reaver");
			return SUCCESS;
		}
		case SP_PLANAR_DIVINATION:
		{
			(void)detect_all();
			return SUCCESS;
		}
		case SP_SUMMON_UNDEAD:
		{
			summon_2(SUMMON_UNDEAD, 70, FALSE, plev,
				"beyond the graves", "undead creature");
			return SUCCESS;
		}
		case SP_SUMMON_DRAGON:
		{
			summon_2(SUMMON_DRAGON, 70, FALSE, plev,
				"the treasure troves", "dragon");
			return SUCCESS;
		}
		case SP_MASS_SUMMONS:
		{
			msg_print ("You concentrate on several images at once...");
			for (i = 0, b = TRUE; i < 3 + (plev / 10); i++)
			{
				bool friendly = percent(70);

				if (summon_specific_aux(py, px, plev, 0, FALSE, friendly))
				{
					b = FALSE;
					if (!friendly) msg_print("A summoned creature gets angry!");
				}
			}
			if (b) msg_print("Nobody answers to your call.");
			return SUCCESS;
		}
		case SP_SUMMON_DEMON_2:
		{
			summon_2(SUMMON_DEMON, 70, FALSE, plev,
				"the pits of hell", "demon");
			return SUCCESS;
		}
		case SP_SUMMON_ANCIENT_DRAGON:
		{
			summon_2(SUMMON_HI_DRAGON, 70, FALSE, plev,
				"the ancient caves", "ancient dragon");
			return SUCCESS;
		}
		case SP_SUMMON_GREATER_UNDEAD:
		{
			summon_2(SUMMON_HI_UNDEAD, 70, FALSE, plev,
				"the darkest tombs", "greater undead creature");
			return SUCCESS;
		}

	/* Necromancy */

		case SP_DETECT_UNLIFE:
		{
		(void) detect_monsters_nonliving();
			return SUCCESS;
		}
		case SP_MALEDICTION:
		{
		if (!dir) return POWER_ERROR_NO_SUCH_DIR;
		/* A radius-0 ball may (1) be aimed at objects etc.,
			* and will affect them; (2) may be aimed at ANY
			* visible monster, unlike a 'bolt' which must travel
			* to the monster. */

		fire_ball(GF_HELL_FIRE, dir,
			damroll(3 + ((plev - 1) / 5), 3), 0);
		if (randint(5)==1) {   /* Special effect first */
		i = randint(1000);
		if (i == 666)
			fire_bolt(GF_DEATH_RAY, dir, plev);
		else if (i < 500)
			fire_bolt(GF_TURN_ALL, dir, plev);
		else if (i < 800)
			fire_bolt(GF_OLD_CONF, dir, plev);
		else
			fire_bolt(GF_STUN, dir, plev);
		}
			return SUCCESS;
		}
		case SP_STINKING_CLOUD:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_ball(GF_POIS, dir,
				10 + (plev / 2), 2);
			return SUCCESS;
		}
		case SP_BLACK_SLEEP:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			(void)fire_bolt(GF_OLD_SLEEP, dir,plev);
			return SUCCESS;
		}
		case SP_HORRIFY:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			(void)fire_bolt(GF_TURN_ALL, dir, plev);
			(void) fire_bolt(GF_STUN, dir, plev);
			return SUCCESS;
		}
		case SP_ENSLAVE_UNDEAD:
		{
		if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			(void)control_one_undead(dir, plev);
			return SUCCESS;
		}
		case SP_ORB_OF_ENTROPY:
		{
		if (!dir) return POWER_ERROR_NO_SUCH_DIR;
		fire_ball(GF_OLD_DRAIN, dir,
			(damroll(3, 6) + plev + (plev / 4)),
			((plev < 30) ? 2 : 3));
			return SUCCESS;
		}
		case SP_NETHER_BOLT:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt_or_beam(plev, GF_NETHER, dir,
				damroll(6+((plev-5)/4), 8));
			return SUCCESS;
		}
		case SP_TERROR:
		{
			turn_monsters(30+plev);
			return SUCCESS;
		}
		case SP_VAMPIRIC_DRAIN:
		{
		if (!dir) return POWER_ERROR_NO_SUCH_DIR;
		i = plev + randint(plev) * MAX(1, plev/10);   /* Dmg */
				if (fire_bolt(GF_OLD_DRAIN, dir, i)) {
			(void)hp_player(i);
			/* Gain nutritional sustenance: 150/hp drained */
			/* A Food ration gives 5000 food points (by contrast) */
			/* Don't ever get more than "Full" this way */
			/* But if we ARE Gorged,  it won't cure us */
			i = p_ptr->food + MIN(5000, 100 * i);
			if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
			(void)set_flag(TIMED_FOOD, MIN(i, PY_FOOD_MAX-1));
		}
			return SUCCESS;
		}
		case SP_POISON_BRANDING:
		{
			brand_weapon(EGO_BRAND_POIS);
			return SUCCESS;
		}
		case SP_DISPEL_GOOD:
		{
			(void)dispel_good(plev * 4);
			return SUCCESS;
		}
		case SP_INVOKE_SPIRITS:
		{
			{
				int die = randint(100) + plev / 5;
				if (!dir) return POWER_ERROR_NO_SUCH_DIR;

				msg_print("You call on the power of the dead...");
				if (die > 100)
				msg_print ("You feel a surge of eldritch force!");

				if (die < 8) {
				msg_print("Oh no! Mouldering forms rise from the earth around you!");
				(void) summon_specific(py, px, (dun_depth), SUMMON_UNDEAD);
				} else if (die < 14) {
				msg_print("An unnamable evil brushes against your mind...");
				add_flag(TIMED_AFRAID, randint(4) + 4);
				} else if (die < 26) {
				msg_print("Your head is invaded by a horde of gibbering spectral voices...");
				add_flag(TIMED_CONFUSED, randint(4) + 4);
				} else if (die < 31) {
				fire_bolt(GF_OLD_POLY, dir,plev);
				} else if (die < 36) {
				fire_bolt_or_beam(plev - 10,
							GF_MISSILE, dir,
							damroll(3 + ((plev - 1) / 5), 4));
				} else if (die < 41) {
				fire_bolt(GF_OLD_CONF, dir, plev);
				} else if (die < 46) {
				fire_ball (GF_POIS, dir, 20 + (plev / 2), 3);
				} else if (die < 51) {
				lite_line(dir);
				} else if (die < 56) {
				fire_bolt_or_beam(plev - 10, GF_ELEC, dir,
							damroll(3+((plev-5)/4), 8));
				} else if (die < 61) {
				fire_bolt_or_beam(plev - 10, GF_COLD, dir,
							damroll(5+((plev-5)/4), 8));
				} else if (die < 66) {
				fire_bolt_or_beam(plev, GF_ACID, dir,
							damroll(6+((plev-5)/4), 8));
				} else if (die < 71) {
				fire_bolt_or_beam(plev, GF_FIRE, dir,
							damroll(8+((plev-5)/4), 8));
				} else if (die < 76) {
				fire_bolt(GF_OLD_DRAIN, dir, 75);
				} else if (die < 81) {
				fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
				} else if (die < 86) {
				fire_ball(GF_ACID, dir, 40 + plev, 2);
				} else if (die < 91) {
				fire_ball(GF_ICE, dir, 70 + plev, 3);
				} else if (die < 96) {
				fire_ball(GF_FIRE, dir, 80 + plev, 3);
				} else if (die < 101) {
				fire_bolt(GF_OLD_DRAIN, dir, 100 + plev);
				} else if (die < 104) {
				earthquake(py, px, 12);
				} else if (die < 106) {
				destroy_area(py, px, 15, TRUE);
				} else if (die < 108) {
				genocide(TRUE);
				} else if (die < 110) {
				dispel_monsters(120);
				} else { /* RARE */
				dispel_monsters (150);
				slow_monsters(plev);
				sleep_monsters(plev);
				hp_player (300);
				}

				if (die < 31)
				msg_print("Sepulchral voices chuckle. 'Soon you will join us, mortal.'");
			return SUCCESS;
		}
			}
		case SP_DARK_BOLT:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_bolt_or_beam(plev, GF_DARK, dir,
				damroll(4+((plev-5)/4), 8));
			return SUCCESS;
		}
		case SP_BATTLE_FRENZY:
		{
			(void)add_flag(TIMED_SHERO, randint(25) + 25);
			(void)hp_player(30);
			(void)set_flag(TIMED_AFRAID, 0);
			if (speed_up(1+plev/2, 20+plev/2*2)) (*ident) = TRUE;
			return SUCCESS;
		}
		case SP_VAMPIRIC_BRANDING:
		{
			brand_weapon(EGO_VAMPIRIC);
			return SUCCESS;
		}
		case SP_DEATH_RAY:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			(void)fire_bolt(GF_DEATH_RAY, dir, plev);
			return SUCCESS;
		}
		case SP_ESOTERIA:
		{
		if (randint(50)>plev)
			(void) ident_spell();
		else
			identify_fully();
			return SUCCESS;
		}
		case SP_WORD_OF_DEATH:
		{
		(void)dispel_living(plev * 3);
			return SUCCESS;
		}
		case SP_EVOCATION:
		{
		(void)dispel_monsters(plev * 4);
		turn_monsters(plev*4);
		banish_monsters(plev*4);
			return SUCCESS;
		}
		case SP_HELLFIRE:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_ball(GF_HELL_FIRE, dir,
					666, 3);
			take_hit(50+randint(50), "the strain of casting Hellfire", MON_CASTING_HELLFIRE);
			return SUCCESS;
		}
		case SP_OMNICIDE:
		{
		p_ptr->csp -= 100;  /* Display doesn't show mana cost (100)
		* as deleted until the spell has finished. This gives a
		* false impression of how high your mana is climbing.
		* Therefore, 'deduct' the cost temporarily before entering the
		* loop, then add it back at the end so that the rest of the
		* program can deduct it properly */
		for (i = 1; i < m_max; i++)
		{
			monster_type    *m_ptr = &m_list[i];
			monster_race    *r_ptr = &r_info[m_ptr->r_idx];

			/* Paranoia -- Skip dead monsters */
			if (!m_ptr->r_idx) continue;

			/* Hack -- Skip Unique Monsters */
			if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

			/* Skip Quest Monsters */
			if (r_ptr->flags1 & RF1_GUARDIAN) continue;

			/* Delete the monster */
			delete_monster_idx(i,TRUE);

			/* Take damage */
			take_hit(randint(4), "the strain of casting Omnicide", MON_CASTING_MASS_GENOCIDE);

			/* Absorb power of dead soul */
			p_ptr->csp++;

			/* Visual feedback */
			move_cursor_relative(py, px);

			/* Redraw */
			p_ptr->redraw |= (PR_HP | PR_MANA);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
			p_ptr->window |=(PW_SPELL);

			/* Handle */
			handle_stuff();

			/* Fresh */
			Term_fresh();

			/* Delay */
			Term_xtra(TERM_XTRA_DELAY, delay_factor);
		}
		p_ptr->csp += 100;   /* Restore, ready to be deducted properly */

			return SUCCESS;
		}
		case SP_WRAITHFORM:
		{
		add_flag(TIMED_WRAITH, randint(plev/2) + (plev/2));
			return SUCCESS;
		}
		case SP_ZAP:
		{
				if (!dir) return POWER_ERROR_NO_SUCH_DIR;
				fire_bolt_or_beam(plev-10, GF_ELEC, dir,
								damroll(3 + ((plev - 1) / 5), 3));
			return SUCCESS;
		}
		case SP_WIZARD_LOCK:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			(void) wizard_lock(dir);
			return SUCCESS;
		}
		case SP_DETECT_INVISIBILITY:
		{
			(void)detect_monsters_invis();
			return SUCCESS;
		}
		case SP_CURE_LIGHT_WOUNDS:
		{
			(void) hp_player(damroll(2, 8));
			(void) set_flag(TIMED_CUT, p_ptr->cut - 10);
			return SUCCESS;
		}
		case SP_PHLOGISTON:
		{
			phlogiston();
			return SUCCESS;
		}
		case SP_DETECT_TREASURE:
		{
			(void)detect_treasure();
			(void)detect_objects_gold();

			return SUCCESS;
		}
		case SP_DETECT_OBJECTS:
		{
			(void)detect_objects_normal();
			return SUCCESS;
		}
		case SP_RAY_OF_LIGHT:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("A line of light appears.");
			lite_line(dir);
			return SUCCESS;
		}
		case SP_ELEMENTAL_BALL:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			switch (randint(4))
			{
				case 1: i = GF_FIRE; break;
				case 2: i = GF_ELEC; break;
				case 3: i = GF_COLD; break;
				default: i = GF_ACID;
			}
			fire_ball(i, dir,
					75 + (plev), 2);
			return SUCCESS;
		}
		case SP_DETECTION:
		{
			(void)detect_all();
			return SUCCESS;
		}
		case SP_CLAIRVOYANCE:
		{
			wiz_lite();
			if (!(p_ptr->telepathy))
			{
				(void)add_flag(TIMED_ESP, randint(30) + 25);
			}
			return SUCCESS;
		}

		case RP_HOBBIT:
		{
			/* Get local object */
			object_type q;

			/* Create the item */
			object_prep(&q, OBJ_RATION_OF_FOOD);

			/* Drop the object from heaven */
			drop_near(&q, -1, py, px);
			msg_print("You cook some food.");
			return SUCCESS;

		}
		case RP_GNOME:
		{
			teleport_player(10 + (plev));
			return SUCCESS;
		}
		case RP_HALF_TROLL:
		{
			(void)set_flag(TIMED_AFRAID, 0);
			(void)add_flag(TIMED_SHERO, 10 + randint(plev));
			(void)hp_player(30);
			return SUCCESS;
		}
		case RP_GREAT:
		{ /* Dreaming */
			(void)set_flag(TIMED_POISONED, 0);
			(void)set_flag(TIMED_IMAGE, 0);
			(void)set_flag(TIMED_STUN, 0);
			(void)set_flag(TIMED_CUT, 0);
			(void)set_flag(TIMED_BLIND, 0);
			(void)set_flag(TIMED_AFRAID, 0);
			if (do_res_stats()) (*ident) = TRUE;
			(void)restore_level();
			return SUCCESS;
		}
		case RP_GREAT_2:
		{ /* dream travel */
			change_level(dun_level, START_RANDOM);
			return SUCCESS;
		}
		case RP_HALF_GIANT:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You bash at a stone wall.");
			(void)wall_to_mud(dir);
			return SUCCESS;
		}
		case RP_CYCLOPS:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You throw a huge boulder.");
			fire_bolt(GF_MISSILE, dir, (3 * plev) / 2);
			return SUCCESS;
		}
		case RP_YEEK:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You make a horrible scream!");
			(void)fire_bolt(GF_TURN_ALL, dir, plev);
			return SUCCESS;
		}
		case RP_KLACKON:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You spit acid.");
			if (plev < 25)
				fire_bolt(GF_ACID, dir, plev);
			else
				fire_ball(GF_ACID, dir, plev, 2);
			return SUCCESS;
		}
		case RP_KOBOLD:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You throw a dart of poison.");
			fire_bolt(GF_POIS, dir, plev);
			return SUCCESS;
		}
		case RP_DARK_ELF:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You cast a magic missile.");
			fire_bolt_or_beam(10, GF_MISSILE, dir,
			damroll(3 + ((plev - 1) / 5), 4));
			return SUCCESS;
		}
		case RP_DRACONIAN:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You breathe poison.");
			fire_ball(GF_POIS, dir, (plev)*2, -((plev)/15) + 1);
			return SUCCESS;
		}
		case RP_MIND_FLAYER:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			msg_print("You concentrate and your eyes glow red...");
			fire_bolt(GF_PSI, dir, plev);
			return SUCCESS;
		}
		case RP_IMP:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			if (plev >= 30)
			{
				msg_print("You cast a ball of fire.");
				fire_ball(GF_FIRE, dir, plev, 2);
			}
			else
			{
				msg_print("You cast a bolt of fire.");
				fire_bolt(GF_FIRE, dir, plev);
			}
			return SUCCESS;
		}
		case RP_VAMPIRE:
		{
			/* Only works on adjacent monsters */
			cave_type *c_ptr;
			s16b dummy;
			if (!dir) return POWER_ERROR_NO_SUCH_REP_DIR;   /* was get_aim_dir */
			c_ptr = &cave[py+ddy[dir]][px+ddx[dir]];
			if (!(c_ptr->m_idx))
			{
				msg_print("You bite into thin air!");
				return SUCCESS;
			}
			msg_print("You grin and bare your fangs...");
			dummy = plev + randint(plev) * MAX(1, plev/10);   /* Dmg */
			if (!fire_bolt(GF_OLD_DRAIN, dir, dummy))
			{
				msg_print("Yechh. That tastes foul.");
			}
			else
			{
				if (p_ptr->food < PY_FOOD_FULL)
				/* No heal if we are "full" (although we still get more full). */
					(void)hp_player(dummy);
				else
					msg_print("You were not hungry.");

				/* Gain nutritional sustenance: 150/hp drained */
				/* A Food ration gives 5000 food points (by contrast) */
				/* Don't ever get more than "Full" this way */
				/* But if we ARE Gorged,  it won't cure us */
				dummy = p_ptr->food + MIN(5000, 100 * dummy);
				if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
					(void)set_flag(TIMED_FOOD, dummy);
			}
			return SUCCESS;
		}
		case RP_BROO:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			(void)fire_bolt(GF_TURN_ALL, dir, plev);
			return SUCCESS;
		}
		case RP_SPRITE:
		{
			if (plev < 25)
				sleep_monsters_touch(plev);
			else
				(void)sleep_monsters(plev);
			return SUCCESS;
		}
		case MUT_SPIT_ACID+PO_MUTA:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_ball(GF_ACID, dir, plev, 1 + (plev/30));
			return SUCCESS;
		}
		case MUT_BR_FIRE+PO_MUTA:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fire_ball(GF_FIRE, dir, plev * 2, -(1 + (plev/20)));
			return SUCCESS;
		}
		case MUT_HYPN_GAZE+PO_MUTA:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			(void) charm_monster(dir, plev);
			return SUCCESS;
		}
		case MUT_TELEKINES+PO_MUTA:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_DIR;
			fetch(dir, plev * 10, TRUE);
			return SUCCESS;
		}
		case MUT_VTELEPORT+PO_MUTA:
		{
			teleport_player(10 + 4*(plev));
			return SUCCESS;
		}
		case MUT_MIND_BLST+PO_MUTA:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_REP_DIR;
			fire_bolt(GF_PSI, dir, damroll(3 + ((plev - 1) / 5), 3));
			return SUCCESS;
		}
		case MUT_RADIATION+PO_MUTA:
		{
			fire_ball(GF_NUKE, 0, (plev * 2), 3 + (plev / 20));
			return SUCCESS;
		}
		case MUT_VAMPIRISM+PO_MUTA:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_REP_DIR;
			if (fire_bolt(GF_OLD_DRAIN, dir, (plev * 2))) hp_player(plev + randint(plev));
			return SUCCESS;
		}
		case MUT_SMELL_MET+PO_MUTA:
		{
			(void)detect_treasure();
			return SUCCESS;
		}
		case MUT_EAT_ROCK+PO_MUTA:
		{
			int x,y;
			cave_type *c_ptr;

			if (!dir) return POWER_ERROR_NO_SUCH_REP_DIR;
			y = py + ddy[dir];
			x = px + ddx[dir];
			c_ptr = &cave[y][x];
			if (cave_floor_bold(y,x))
			{
				msg_print("You bite into thin air!");
			}
			else if (((c_ptr->feat >= FEAT_PERM_BUILDING) &&
				(c_ptr->feat <= FEAT_PERM_SOLID)))
			{
				msg_print("Ouch!  This wall is harder than your teeth!");
			}
			else if (c_ptr->m_idx)
			{
				msg_print("There's something in the way!");
			}
			else if (c_ptr->feat == FEAT_TREE)
			{
				msg_print("You don't like the woody taste!");
			}
			else
			{
				if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
					(c_ptr->feat <= FEAT_RUBBLE))
				{
					(void)add_flag(TIMED_FOOD, 3000);
				}
				else if ((c_ptr->feat >= FEAT_MAGMA) &&
					(c_ptr->feat <= FEAT_QUARTZ_K))
				{
					(void)add_flag(TIMED_FOOD, 5000);
				}
				else
				{
					msg_print("This granite is very filling!");
					(void)add_flag(TIMED_FOOD, 10000);
				}

				(void)wall_to_mud(dir);
				move_to(y,x);
			}
			return SUCCESS;
		}
		case MUT_SWAP_POS+PO_MUTA:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_REP_DIR;
			(void)teleport_swap(dir);
			return SUCCESS;
		}
		case MUT_SHRIEK+PO_MUTA:
		{
			(void)fire_ball(GF_SOUND, 0, 4 * plev, 8);
			(void)aggravate_monsters(NULL);
			return SUCCESS;
		}
		case MUT_DET_CURSE+PO_MUTA:
		{
			for (i=0; i < INVEN_TOTAL; i++)
			{
				object_type *o_ptr = &inventory[i];

				if (!o_ptr->k_idx) continue;
				o_ptr->ident |= IDENT_SENSE_CURSED;
			}
			return SUCCESS;
		}
		case MUT_POLYMORPH+PO_MUTA:
		{
			do_poly_self();
			return SUCCESS;
		}
		case MUT_MIDAS_TCH+PO_MUTA:
		{
			(void)alchemy();
			return SUCCESS;
		}
		case MUT_GROW_MOULD+PO_MUTA:
		{
			
			for (b = FALSE, i=0; i < 8; i++)
			{
				if (summon_specific_friendly(py, px, plev, SUMMON_MOULD, FALSE))
					b = TRUE;
			}

			if (!b)
			{
				object_type q[1];

				/* No room for monsters, so create some food. */
				object_prep(q, OBJ_SLIME_MOULD);

				/* Drop the object from heaven */
				drop_near(q, -1, py, px);
			}

			return SUCCESS;
		}
		case MUT_RESIST+PO_MUTA:
		{
			int num = plev/10;
			int dur = randint(20) + 20;

			if (rand_int(5) < num)
			{
				(void)add_flag(TIMED_OPPOSE_ACID, dur);
				num--;
			}
			if (rand_int(4) < num)
			{
				(void)add_flag(TIMED_OPPOSE_ELEC, dur);
				num--;
			}
			if (rand_int(3) < num)
			{
				(void)add_flag(TIMED_OPPOSE_FIRE, dur);
				num--;
			}
			if (rand_int(2) < num)
			{
				(void)add_flag(TIMED_OPPOSE_COLD, dur);
				num--;
			}
			if (num)
			{
				(void)add_flag(TIMED_OPPOSE_POIS, dur);
				num--;
			}
			return SUCCESS;
		}
		case MUT_EARTHQUAKE+PO_MUTA:
		{
			/* Prevent destruction of quest levels and town */
			if (!is_quest() && dun_level)
				earthquake(py, px, 10);
			return SUCCESS;
		}
		case MUT_EAT_MAGIC+PO_MUTA:
		{
			object_type * o_ptr;
			errr err;
			int lev;
			cptr q;

			item_tester_hook = item_tester_hook_recharge;

			/* Get an item */
			q = "Drain which item? ";
			if (!((o_ptr = get_item(&err, q, TRUE,TRUE,TRUE))))
			{
				(*use) = FALSE;
				return SUCCESS;
			}

			lev = wand_power(&k_info[o_ptr->k_idx]);

			if (o_ptr->tval == TV_ROD)
			{
				if (o_ptr->timeout > 0)
				{
					msg_print("You can't absorb energy from a discharged rod.");
				}
				else
				{
					p_ptr->csp += 2*lev;
					o_ptr->timeout = 500;
				}
			}
			else
			{
				if (o_ptr->pval > 0)
				{
					p_ptr->csp += o_ptr->pval * lev;
					o_ptr->pval = 0;
				}
				else
				{
					msg_print("There's no energy there to absorb!");
				}
				o_ptr->ident |= IDENT_EMPTY;
			}

			if (p_ptr->csp > p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
			}

			update_object(o_ptr);
			return SUCCESS;
		}
		case MUT_WEIGH_MAG+PO_MUTA:
		{
			report_magics();
			return SUCCESS;
		}
		case MUT_STERILITY+PO_MUTA:
		{
			/* Fake a population explosion. */
			take_hit(randint(30) + 30, "the strain of forcing abstinence", MON_DANGEROUS_MUTATION);
			num_repro += MAX_REPRO;
			return SUCCESS;
		}
		case MUT_PANIC_HIT+PO_MUTA:
		{
			int x,y;

			if (!dir) return POWER_ERROR_NO_SUCH_REP_DIR;
			y = py + ddy[dir];
			x = px + ddx[dir];
			if (cave[y][x].m_idx)
			{
				py_attack(y, x);
				teleport_player(30);
			}
			else
			{
				msg_print("You don't see any monster in this direction");
				msg_print(NULL);
			}
			return SUCCESS;
		}
		case MUT_DAZZLE+PO_MUTA:
		{
			stun_monsters(plev * 4);
			confuse_monsters(plev * 4);
			turn_monsters(plev * 4);
			return SUCCESS;
		}
		case MUT_EYE_BEAM+PO_MUTA:
		{
			if (!dir) return POWER_ERROR_NO_SUCH_REP_DIR;
			fire_beam(GF_LITE, dir, 2*plev);
			return SUCCESS;
		}
		case MUT_BANISH+PO_MUTA:
		{
			int x,y;
			cave_type *c_ptr;
			monster_type *m_ptr;
			monster_race *r_ptr;

			if (!dir) return POWER_ERROR_NO_SUCH_REP_DIR;
			y = py + ddy[dir];
			x = px + ddx[dir];
			c_ptr = &cave[y][x];
			if (!(c_ptr->m_idx))
			{
				msg_print("You sense no evil there!");
				return SUCCESS;
			}
			m_ptr = &m_list[c_ptr->m_idx];
			r_ptr = &r_info[m_ptr->r_idx];

			if (r_ptr->flags3 & RF3_EVIL)
			{
				/* Delete the monster, rather than killing it. */
				delete_monster_idx(c_ptr->m_idx,TRUE);
				msg_print("The evil creature vanishes in a puff of sulfurous smoke!");
			}
			else
			{
				msg_print("Your invocation is ineffectual!");
			}
			return SUCCESS;
		}
		case MUT_COLD_TOUCH+PO_MUTA:
		{
			int x,y;
			cave_type *c_ptr;

			if (!dir) return POWER_ERROR_NO_SUCH_REP_DIR;
			y = py + ddy[dir];
			x = px + ddx[dir];
			c_ptr = &cave[y][x];
			if (!(c_ptr->m_idx))
			{
				msg_print("You wave your hands in the air.");
				return SUCCESS;
			}
			fire_bolt(GF_COLD, dir, 2 * (plev));
			return SUCCESS;
		}
		case MUT_LAUNCHER+PO_MUTA:
		{
			/* Gives a multiplier of 2 at first, up to 5 at 48th */
			do_cmd_throw_hard(2 + (plev)/16);
			return SUCCESS;
		}
		case PET_DISMISS_ONE+PO_PETS:
		{
			dismiss_pets(FALSE);
			return SUCCESS;
		}
		case PET_DISMISS_MANY+PO_PETS:
		{
			dismiss_pets(TRUE);
			return SUCCESS;
		}
		default:
		{
			/* The object wasn't handled here... */
			return POWER_ERROR_NO_SUCH_POWER;
		}
	}
}

/*
 * Use a power from an object of some kind.
 * Return TRUE if the power exists, FALSE otherwise.
 *
 * power is the index of the power to use.
 * dir is the direction the power is to be used in, or 0 if unspecified.
 * *ident is TRUE if the player should know that this object has this power.
 *   It is set if the power has observable effects.
 * *use is set to TRUE if the power was used, FALSE if it was aborted, etc.
 *   Its initial valuo is ignored.
 */
bool use_object_power(const int power, int dir, bool *ident, bool *use)
{
	/* Define plev in a similar way to spells. */
	const int plev = MAX(1, skill_set[SKILL_DEVICE].value/2);

	switch (do_power(power, plev, dir, *ident, use, ident))
	{
		/* Get a direction, then try again. */
		case POWER_ERROR_NO_SUCH_DIR:
		{
			/* Finish if no direction is given. */
			if (!get_aim_dir(&dir))
			{
				(*use) = FALSE;
				return TRUE;
			}

			/* Otherwise, try again. */
			return use_object_power(power, dir, ident, use);
		}
		/* Get a direction, then try again. */
		case POWER_ERROR_NO_SUCH_REP_DIR:
		{
			/* Finish if no direction is given. */
			if (!get_rep_dir(&dir))
			{
				(*use) = FALSE;
				return TRUE;
			}

			/* Otherwise, try again. */
			return use_object_power(power, dir, ident, use);
		}
		/* Look for powers based on tval. */
		case POWER_ERROR_NO_SUCH_POWER:
		{
			return FALSE;
		}
		default:
		{
			return TRUE;
		}
	}
}

/*
 * Use a power listed by index.
 * Return FALSE if it is aborted.
 */
bool use_known_power(int power, int plev)
{
	bool ident, use;
	errr err = do_power(power, plev, 0, TRUE, &use, &ident);
	if (err == POWER_ERROR_NO_SUCH_DIR)
	{
		int dir;
		if (!get_aim_dir(&dir)) return FALSE;
		err = do_power(power, plev, dir, TRUE, &use, &ident);
	}
	else if (err == POWER_ERROR_NO_SUCH_REP_DIR)
	{
		int dir;
		if (!get_rep_dir(&dir)) return FALSE;
		err = do_power(power, plev, dir, TRUE, &use, &ident);
	}
	if (err == POWER_ERROR_NO_SUCH_POWER)
	{
		bell("Unknown power: %d", power);
		return FALSE;
	}
	return use;
}
