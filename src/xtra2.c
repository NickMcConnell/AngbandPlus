#define XTRA2_C
/* File: effects.c */

/* Purpose: effects of various "objects" */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#define REWARD_CHANCE 10


#define MAX_NUM 2147483647

/*
 * Find the lowest common multiple of two s32bs
 */
static s32b find_lcm(s32b ua, s32b ub)
{
	s32b a = ua, b = ub;
	/* Avoid problems from incorrect input. */
	if (a < 1 || b < 1) return 0;
	while (a && b)
	{
		if (a < b)
			b -= b/a*a;
		else
			a -= a/b*b;
	}
	a += b;
	/* Avoid overflow */
	if (MAX_NUM/ua < ub/a) return 0;
	else return ua/a*ub;
}

/*
 * The rolling routine for drop_special().
 */
static bool death_event_roll(death_event_type *d_ptr,
	bool *one_dropped, s32b *total_num, s32b *total_denom)
{
	s32b num = d_ptr->num;
	s32b denom = d_ptr->denom;
	s32b lcm;

	/* Wizards always get everything */
	if (cheat_wzrd) return TRUE;

	/* It's easy without ONLY_ONE. */
	if (!(d_ptr->flags & EF_ONLY_ONE)) return (num > rand_int(denom));

	/* No more drops possible */
	if (*one_dropped) return FALSE;

	/* Now arrange things so that denom = total_denom */

	/* Try to represent the fraction exactly. */
	if ((lcm = find_lcm(*total_denom, denom)))
{
		num *= (lcm/denom);
		*total_num *= (lcm/(*total_denom));
		*total_denom = lcm;
	}
	/* But use the largest available denominator if impossible. */
	else
	{
		num *= MAX_NUM/denom;
		*total_num *= MAX_NUM/(*total_denom);
		*total_denom = MAX_NUM;
	}

	/* Ensure that the total probability so far is sensible. */
	if (*total_num < num)
	{
		msg_format("Incoherent probabilities for event %d.\n",
			(d_ptr-death_event));
		*one_dropped = TRUE;
		return FALSE;
	}

	/* Ensure that the next roll uses a correct number. */
	*total_num -= num;

	/* Finally, roll a number. */
	if (num > rand_int(*total_denom))
	{
		/* Don't try again */
		*one_dropped = TRUE;
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

/*
 * Run cave_floor_bold() as a function. Used below to allow a pointer to it.
 */
static bool PURE cave_floor_bold_p(int y, int x)
{
	return cave_floor_bold(y, x);
}

/*
 * Create something at the point of death.
 * Return the type of coin to make if requested, 0 for no preference;
 */
static int drop_special(monster_type *m_ptr)
{
	s32b total_num = 1;
	s32b total_denom = 1;
	bool one_dropped = FALSE;
	int i, coin_type = 0;

	for (i = 0; i < MAX_DEATH_EVENTS; ++i)
	{
		death_event_type *d_ptr = &death_event[i];

		/* Ignore incorrect entries */
		if (d_ptr->r_idx > m_ptr->r_idx) break;
		if (!d_ptr->r_idx) break;
		if (d_ptr->r_idx < m_ptr->r_idx) continue;

		/* Decide whether to drop correct ones. */
		if (!death_event_roll(d_ptr, &one_dropped, &total_num, &total_denom))
			continue;

		/* Give some feedback to cheaters. */
		if (cheat_xtra) msg_format("Processing death event %d", i);

		/* Actually carry out event
		 * Note that illegal events and default values are dealt with
		 * in init1.c, not here.
		 */
		switch (d_ptr->type)
		{
			/* Drop some or fewer objects. */
			case DEATH_OBJECT:
			{
				object_type o_ptr[1];

				/* Create the item from the definition. */
				make_item(o_ptr, &d_ptr->par.item, event_name, FOUND_MONSTER,
					m_ptr->r_idx);

				if (d_ptr->text) msg_print(event_text+d_ptr->text);
				drop_near(o_ptr, -1, m_ptr->fy, m_ptr->fx);
				d_ptr->flags |= EF_KNOWN;
				break;
			}
			/* Create a monster nearby. */
			case DEATH_MONSTER:
			{
				make_monster_type *i_ptr = &d_ptr->par.monster;
				byte i,num = rand_range(i_ptr->min, i_ptr->max);
				bool seen = FALSE;
				for (i = 0; i < num; i++)
				{
					int wy, wx, j;
					int max = (i_ptr->strict) ? 1 : 10;

					/* Try to place within the given distance, but do accept
					 * greater distances if allowed.
					 */
					for (j = 0; j < max; j++)
					{
						int d = i_ptr->radius;
						if (!i_ptr->strict) d += j;
						if (scatter(&wy, &wx, m_ptr->fy, m_ptr->fx, d,
							cave_floor_bold_p)) goto good_DM;
					}

					/* Give up if there's nowhere appropriate */
					break;

good_DM:
					/* As creating the monster can give a message, give this
					 * message first.
					 */
					if (player_can_see_bold(wy, wx) && !seen)
					{
						if (d_ptr->text) msg_format(event_text+d_ptr->text);
						seen = TRUE;
						d_ptr->flags |= EF_KNOWN;
					}
					/* Actually place the monster (which should not fail) */
					(void)place_monster_one(wy, wx, i_ptr->num, FALSE,
						!!(m_ptr->smart & SM_ALLY), FALSE);
				}
				/* Only let the player know anything happened if within LOS. */
				break;
			}
			/* Cause an explosion centred on the monster */
			case DEATH_EXPLODE:
			{
				/* Set up the parameters. */
				make_explosion_type *i_ptr = &d_ptr->par.explosion;
				byte typ = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
				int damage = damroll(i_ptr->dice, i_ptr->sides);

				/* Print any specified text. */
				if (d_ptr->text) msg_print(event_text+d_ptr->text);

				/* Give cheaters a basic explanation. */
				if (cheat_xtra) msg_format(
					"Explosion of radius %d, power %d and type %d triggered.",
					i_ptr->radius, damage, typ);

				/* Then cause an explosion. */
				(void)project(m_ptr, i_ptr->radius, m_ptr->fy, m_ptr->fx,
					damage, i_ptr->method, typ);
				d_ptr->flags |= EF_KNOWN;
				break;
			}
			/* Hack - force the coin drop (later) to be of a specific type.
			 * This does not actually do anything itself, so a drop of (for
			 * instance) 20 pieces of silver would be best achieved via a coin
			 * artefact with a pval of 20.
			 */
			case DEATH_COIN:
			{
				make_coin_type *i_ptr = &d_ptr->par.coin;
				coin_type = i_ptr->metal;
				if (d_ptr->text) msg_print(event_text+d_ptr->text);
				d_ptr->flags |= EF_KNOWN;
				break;
			}

			/* Do nothing, except tell the player what you've done. */
			case DEATH_NOTHING:
			{
				if (d_ptr->text) msg_print(event_text+d_ptr->text);
				d_ptr->flags |= EF_KNOWN;
				break;
			}
			/* Just in case... */
			default:
			{
				msg_format("What strange action is %d?", d_ptr->type);
			}
		}
	}

	/* Let the caller know the coin type preference, if any. */
	return coin_type;
}

typedef struct temp_effect_type temp_effect_type;
struct temp_effect_type
{
#ifdef CHECK_ARRAYS
	int idx; /* The number of the flag being modified. */
#endif /* CHECK_ARRAYS */

	int text; /* The first relevent message in temp_effects_text[]. */

	/* A function to decide whether an effect is worth noticing. Returns
	 * negative if not, and an offset from temp_effects_text+text if so.
	 */
	int (*notice)(int old, int new);

	/* A function for a bad effect as the counter is noticed increasing. */
	void (*worsen)(int v);

	int prt_flag; /* A single thing to redraw, based on an XY_* index. */

	u32b redraw; /* The following should be redrawn if something is noticed. */
	u32b update; /* The following should be updated if something is noticed. */
	u32b window; /* The following windows may change if something is noticed. */
};

/* Hack - a secondary flag to trigger bad effects. */
static bool worsen = FALSE;

/* Hack - a request for a "boring" value to which everything is compared. */
#define TIMER_DEFAULT -1

/*
 * Only say something if a resistance has gone from 0 to non-0 or vice versa.
 */
static int notice_bool(int old, int new)
{
	/* Default value. */
	if (old == TIMER_DEFAULT) old = 0;

	/* Still positive or 0. */
	if (!old == !new) return -1;

	/* Finishing. */
	if (old) return 1;

	/* Starting. */
	else return 0;
}

/*
 * Return -1 if old and new are in the same section of t[],
 * 1 if exactly one of them is 0, and
 * 0 if they are in different positive sections.
 */
static int notice_res(int old, int new)
{
	int *i, *j, t[] = {10, 5, 0, -1};

	/* Default value. */
	if (old == TIMER_DEFAULT) old = 0;

	/* Paranoia - this was checked before. */
	if (old < 0 || new < 0) return -1;

	/* Find the smallest i such that v > t[i]. */
	for (i = t; old <= *i; i++);
	for (j = t; new <= *j; j++);

	/* Boring if they're the same. */
	if (i == j) return -1;

	/* Only give a real message if notice_bool() is true. */
	return notice_bool(old, new)+1;
}

/*
 * Return -1 if old and new are in the same section of t[],
 * 0 if new is in a smaller one than old, but both are positive,
 * 1 if new is 0, but old is greater, and
 * 2 or more if new is greater than old, with larger values giving higher
 * numbers.
 */
static int notice_stun_aux(int old, int new, int *t, int *t2)
{
	int *i, *j;

	/* Default value. */
	if (old == TIMER_DEFAULT) old = 0;

	/* Paranoia - this was checked before. */
	if (old < 0 || new < 0) return -1;

	/* Find the smallest i such that v > t[i]. */
	for (i = t; old <= *i; i++);
	for (j = t; new <= *j; j++);

	/* Boring if they're the same. */
	if (i == j) return -1;

	/* Getting better. */
	if (new < old)
	{
		/* Only mention a decrease if the effect goes completely. */
		return new ? 0 : 1;
	}
	else
	{
		/* Hack - alert the caller to allow extra bad effects. */
		worsen = TRUE;

		/* Print a message (later). */
		return t2-j;
	}
}

static int notice_stun(int old, int new)
{
	int t[] = {100, 50, 0, -1};

	return notice_stun_aux(old, new, t, END_PTR(t));
}

static int notice_cuts(int old, int new)
{
	int t[] = {1000, 200, 100, 50, 25, 10, 0, -1};

	return notice_stun_aux(old, new, t, END_PTR(t));
}

/*
 * Return -1 if old and new are in the same section of t[],
 * 0-5 if new is in a higher section, and
 * 6-10 if old is in a higher section.
 */
static int notice_food(int old, int new)
{
	int *i, *j, t[] = {PY_FOOD_MAX, PY_FOOD_FULL, PY_FOOD_ALERT, PY_FOOD_WEAK,
		PY_FOOD_FAINT, -1};

	/* Default value. */
	if (old == TIMER_DEFAULT) old = PY_FOOD_ALERT+1;

	/* Paranoia - this was checked before. */
	if (old < 0 || new < 0) return -1;

	/* Find the smallest i such that v > t[i]. */
	for (i = t; old <= *i; i++);
	for (j = t; new <= *j; j++);

	/* Boring if they're the same. */
	if (i == j) return -1;

	if (old < new) return END_PTR(t)-j-2;
	else return END_PTR(t)-j+4;
}

/*
 * A function to indicate that the game should never react to changes in
 * a value.
 */
static int notice_nothing(int UNUSED old, int UNUSED new)
{
	return -1;
}

/*
 * Handle the random side effects of worsening stunning.
 */
static void worsen_stun(int v)
{
	/* Do nothing most of the time. */
	if (rand_int(1000)>v && !one_in(16)) return;

	/* Print a constant message. */
	msg_print("A vicious blow hits your head.");

	/* Stat drain (unless sustained). */
	switch (rand_int(3))
	{
		case 0:
			if (!p_ptr->sustain[A_INT]) { (void) do_dec_stat(A_INT); }
			if (!p_ptr->sustain[A_WIS]) { (void) do_dec_stat(A_WIS); }
			break;
		case 1:
			if (!p_ptr->sustain[A_INT]) { (void) do_dec_stat(A_INT); }
			break;
		case 2:
			if (!p_ptr->sustain[A_WIS]) { (void) do_dec_stat(A_WIS); }
	}
}

/*
 * Handle the random side effects of worsening cuts.
 */
static void worsen_cuts(int v)
{
	/* Do nothing at all with sustained charisma. */
	if (p_ptr->sustain[A_CHR]) return;

	if (rand_int(1000)>v && !one_in(16)) return;

	/* Stat drain (unless sustained). */
	msg_print("You have been horribly scarred.");
	do_dec_stat(A_CHR);
}

/* Some strange macros to leave markers in temp_effects_text[] to verify
 * that each listed index points to "" (a string which cannot sensibly be
 * used by the array's standard use) to allow the spacing to be checked.
 */
#ifdef CHECK_ARRAYS
#define NEXT "",
#define OFFSET(V) ((V)+1)
#else /* CHECK_ARRAYS */
#define NEXT
#define OFFSET(V) 0
#endif /* CHECK_ARRAYS */


static cptr const temp_effects_show[] =
{
	NEXT "$oBlind", "",
	NEXT "$oConfused", "",
	NEXT "$oPoisoned", "",
	NEXT "$oAfraid", "",
	NEXT "$rParalysed!", "",
	NEXT "Hallucinating", "",
	NEXT "Faster", "", /* Fast */
	NEXT "Slower", "", /* Slow */
	NEXT "$sStone Skin", "",
	NEXT "$GBlessed", "",
	NEXT "$yHeroic", "",
	NEXT "$RBerserk", "",
	NEXT "ProtEvil", "",
	NEXT "$DWraith", "",
	NEXT "Invulnerable", "",
	NEXT "ESP", "",
	NEXT "SeeInvisible", "",
	NEXT "Infravision", "",
	NEXT "", "", "Acid", /* op. acid */
	NEXT "", "", "Electricity", /* op. electricity */
	NEXT "", "", "Fire", /* op. fire */
	NEXT "", "", "Cold", /* op. cold */
	NEXT "", "", "Poison", /* op. poison */
	NEXT "", "", "$oStun", "$oHeavy stun", "$rKnocked out",
	NEXT "", "", "$yGraze", "$yLight cut", "$oBad cut",
		"$oNasty cut", "$rSevere cut", "$rDeep gash", "$RMortal wound",
	NEXT "", "", "", "$GFull", "$gGorged", "$rWeak", "$oWeak",
		"$yHungry", "", "",
	NEXT
};

/*
 * The messages to print when a status changes.
 * 0 means "no message", and the order is set by temp_effects[].text, which
 * is in turn set by the number of messages needed for each
 * temp_effects[].notice.
 */
static cptr const temp_effects_text[] =
{
	NEXT "You are blind!", "You can see again.",
	NEXT "You are confused!", "You feel less confused now.",
	NEXT "You are poisoned!", "You are no longer poisoned.",
	NEXT "You are terrified!", "You feel bolder now.",
	NEXT "You are paralyzed!", "You can move again.",
	NEXT "Oh, wow! Everything looks so cosmic now!",
		"You can see clearly again.",
	NEXT "You feel yourself moving faster!", "You feel yourself slow down.",
	NEXT "You feel yourself moving slower!", "You feel yourself speed up.",
	NEXT "Your skin turns to stone.", "Your skin returns to normal.",
	NEXT "You feel righteous!", "The prayer has expired.",
	NEXT "You feel like a hero!", "The heroism wears off.",
	NEXT "You feel like a killing machine!", "You feel less Berserk.",
	NEXT "You feel safe from evil!", "You no longer feel safe from evil.",
	NEXT "You leave the physical world and turn into a wraith-being!",
		"You feel opaque.",
	NEXT "Invulnerability!", "The invulnerability wears off.",
	NEXT "You feel your consciousness expand!",
		"Your consciousness contracts again.",
	NEXT "Your eyes feel very sensitive!", "Your eyes feel less sensitive.",
	NEXT "Your eyes begin to tingle!", "Your eyes stop tingling.",
	NEXT "", "You feel resistant to acid!", "You feel less resistant to acid.",
	NEXT "", "You feel resistant to electricity!",
		"You feel less resistant to electricity.",
	NEXT "", "You feel resistant to fire!", "You feel less resistant to fire.",
	NEXT "", "You feel resistant to cold!", "You feel less resistant to cold.",
	NEXT "", "You feel resistant to poison!",
		"You feel less resistant to poison.",
	NEXT "", "You are no longer stunned.", "You have been stunned.",
		"You have been heavily stunned.", "You have been knocked out.",
	NEXT "", "You are no longer bleeding.", "You have been given a graze.",
		"You have been given a light cut.", "You have been given a bad cut.",
		"You have been given a nasty cut.", "You have been given a severe cut.",
		"You have been given a deep gash.",
		"You have been given a mortal wound.",
	NEXT "You are still weak.", "You are still hungry.",
		"You are no longer hungry.", "You are full!",
		"You have gorged yourself!", "You are getting faint from hunger!",
			"You are getting weak from hunger!", "You are getting hungry.",
			"You are no longer full.", "You are no longer gorged.",
	NEXT
};

static const temp_effect_type temp_effects[TIMED_MAX] =
{
	{IDX(TIMED_BLIND) 0, notice_bool, 0, XY_BLIND+1, PR_MAP,
		PU_UN_VIEW | PU_UN_LITE | PU_VIEW | PU_LITE | PU_MONSTERS, 0},
	{IDX(TIMED_CONFUSED) 2, notice_bool, 0, XY_CONFUSED+1, 0, 0, 0},
	{IDX(TIMED_POISONED) 4, notice_bool, 0, XY_POISONED+1, 0, 0, 0},
	{IDX(TIMED_AFRAID) 6, notice_bool, 0, XY_AFRAID+1, 0, 0, 0},
	{IDX(TIMED_PARALYZED) 8, notice_bool, 0, XY_PARALYSED+1, PR_STATE, 0, 0},
	{IDX(TIMED_IMAGE) 10, notice_bool, 0, XY_IMAGE+1,
		PR_MAP, PU_MONSTERS, 0 | PW_VISIBLE},
	{IDX(TIMED_FAST) 12, notice_bool, 0, XY_FAST+1, 0, PU_BONUS, 0},
	{IDX(TIMED_SLOW) 14, notice_bool, 0, XY_SLOW+1, 0, PU_BONUS, 0},
	{IDX(TIMED_SHIELD) 16, notice_bool, 0, XY_SHIELD+1, 0, PU_BONUS, 0},
	{IDX(TIMED_BLESSED) 18, notice_bool, 0, XY_BLESSED+1, 0, PU_BONUS, 0},
	{IDX(TIMED_HERO) 20, notice_bool, 0, XY_HERO+1, 0, PU_BONUS | PU_HP, 0},
	{IDX(TIMED_SHERO) 22, notice_bool, 0, XY_BERSERK+1, 0, PU_BONUS | PU_HP, 0},
	{IDX(TIMED_PROTEVIL) 24, notice_bool, 0, XY_PROTEVIL+1, 0, 0, 0},
	{IDX(TIMED_WRAITH) 26, notice_bool, 0, XY_WRAITH+1,
		PR_MAP, PU_MONSTERS | PU_BONUS, 0},
	{IDX(TIMED_INVULN) 28, notice_bool, 0, XY_INVULN+1,
		PR_MAP, PU_MONSTERS | PU_BONUS, 0},
	{IDX(TIMED_ESP) 30, notice_bool, 0, XY_ESP+1, 0, PU_MONSTERS | PU_BONUS, 0},
	{IDX(TIMED_INVIS) 32, notice_bool, 0,
		XY_INVIS+1, 0, PU_BONUS | PU_MONSTERS, 0},
	{IDX(TIMED_INFRA) 34, notice_bool, 0,
		XY_INFRA+1, 0, PU_BONUS | PU_MONSTERS, 0},
	{IDX(TIMED_OPPOSE_ACID) 36, notice_res, 0, XY_RACID+1, PR_STUDY, 0, 0},
	{IDX(TIMED_OPPOSE_ELEC) 39, notice_res, 0, XY_RELEC+1, PR_STUDY, 0, 0},
	{IDX(TIMED_OPPOSE_FIRE) 42, notice_res, 0, XY_RFIRE+1, PR_STUDY, 0, 0},
	{IDX(TIMED_OPPOSE_COLD) 45, notice_res, 0, XY_RCOLD+1, PR_STUDY, 0, 0},
	{IDX(TIMED_OPPOSE_POIS) 48, notice_res, 0, XY_RPOIS+1, PR_STUDY, 0, 0},
	{IDX(TIMED_STUN) 51, notice_stun, worsen_stun, XY_STUN+1, 0, PU_BONUS, 0},
	{IDX(TIMED_CUT) 56, notice_cuts, worsen_cuts, XY_CUT+1, 0, PU_BONUS, 0},
	{IDX(TIMED_FOOD) 65, notice_food, 0, XY_HUNGRY+1, 0, PU_BONUS, 0},
	{IDX(TIMED_VAMP) 75, notice_nothing, 0, 0, 0, 0, 0},
};

/*
 * Find the value connected with this flag.
 */
static s16b *get_flag(int flag)
{
	switch (flag)
	{
		case TIMED_BLIND: return &(p_ptr->blind);
		case TIMED_CONFUSED: return &(p_ptr->confused);
		case TIMED_POISONED: return &(p_ptr->poisoned);
		case TIMED_AFRAID: return &(p_ptr->afraid);
		case TIMED_PARALYZED: return &(p_ptr->paralyzed);
		case TIMED_IMAGE: return &(p_ptr->image);
		case TIMED_FAST: return &(p_ptr->fast);
		case TIMED_SLOW: return &(p_ptr->slow);
		case TIMED_SHIELD: return &(p_ptr->shield);
		case TIMED_BLESSED: return &(p_ptr->blessed);
		case TIMED_HERO: return &(p_ptr->hero);
		case TIMED_SHERO: return &(p_ptr->shero);
		case TIMED_PROTEVIL: return &(p_ptr->protevil);
		case TIMED_WRAITH: return &(p_ptr->wraith_form);
		case TIMED_INVULN: return &(p_ptr->invuln);
		case TIMED_ESP: return &(p_ptr->tim_esp);
		case TIMED_INVIS: return &(p_ptr->tim_invis);
		case TIMED_INFRA: return &(p_ptr->tim_infra);
		case TIMED_OPPOSE_ACID: return &(p_ptr->oppose_acid);
		case TIMED_OPPOSE_ELEC: return &(p_ptr->oppose_elec);
		case TIMED_OPPOSE_FIRE: return &(p_ptr->oppose_fire);
		case TIMED_OPPOSE_COLD: return &(p_ptr->oppose_cold);
		case TIMED_OPPOSE_POIS: return &(p_ptr->oppose_pois);
		case TIMED_STUN: return &(p_ptr->stun);
		case TIMED_CUT: return &(p_ptr->cut);
		case TIMED_FOOD: return &(p_ptr->food);
		case TIMED_VAMP: return &(p_ptr->vamp_drain);
		default: return 0;
	}
}

#ifdef CHECK_ARRAYS
/*
 * Check that temp_effects[] has the expected indices.
 */
void check_temp_effects(void)
{
	const temp_effect_type *ptr;
	cptr str;

	FOR_ALL_IN(temp_effects, ptr)
	{
		if (temp_effects + ptr->idx != ptr)
			quit_fmt("temp_effects index %d misplaced.", ptr->idx);
		if (!get_flag(ptr->idx))
			quit_fmt("No temp_effects flag available for index %d.", ptr->idx);

		/* Check that the text markers are in the right place. */
		str = temp_effects_text[ptr->text + ptr->idx];
		if (!str) str = "null";
		if (*str) quit_fmt("Text marker for temp_effects index %d is "
			"%s, when it should be \"\".", ptr->idx, str);

		/* Check that the show markers are in the right place. */
		str = temp_effects_show[ptr->text + ptr->idx];
		if (!str) str = "null";
		if (*str) quit_fmt("Show marker for temp effects index %d is "
			"\"%s\", when it should be NULL.", ptr->idx, str);

		/* Try to determine when a basic "change to current state" fails
		 * accidentally. */
		if ((ptr->notice != notice_nothing) && (*ptr->notice)(-1, 1) < 0 &&
			(*ptr->notice)(0, 1) < 0 && (*ptr->notice)(20000, 0) < 0)
			quit_fmt("Notice function not expressive enough for temp_effects "
				"index %d.", ptr->idx);
	}
}
#endif /* CHECK_ARRAYS */

/*
 * Hack - the player can sometimes be immune to specific timed effects.
 * As existing conditions may time out naturally, this is only called when
 * the timer is being increased.
 */
static bool allow_set_flag_p(int flag)
{
	switch (flag)
	{
		case TIMED_STUN: return !p_ptr->no_stun;
		case TIMED_CUT: return !p_ptr->no_cut;
		default: return TRUE;
	}
}

static void prt_timer(const temp_effect_type *t_ptr)
{
	const redraw_type *r_ptr;
	int y, x, m;

	/* No simple "draw" function. */
	if (!t_ptr->prt_flag) return;

	/* Find the location data. */
	r_ptr = screen_coords+t_ptr->prt_flag-1;

	/* Nothing to draw. */
	if (!r_ptr->l) return;

	/* Extract the location. */
	Term_get_size(&y, &x);
	m = y; y += r_ptr->y; y %= m;
	m = x; x += r_ptr->x; x %= m;

	/* Print it. */
	mc_put_lfmt(y, x, r_ptr->l, "%s", prt_flag(t_ptr - temp_effects));
}

/*
 * Set one of the above variables to a specific value, and react appropriately.
 */
static bool set_flag_aux(int flag, int v, bool add)
{
	int notice;
	s16b *var = get_flag(flag);
	cptr msg;
	const temp_effect_type *t_ptr = temp_effects+flag;

	/* Allow the call to be as a modifier. */
	if (add) (v += *var);

	/* Hack - handle immunity to timed effects. */
	if (v > *var && !allow_set_flag_p(flag)) v = *var;

	/* Bound the new value. */
	v = MIN(MAX(v, 0), 20000);

	/* Hack - an extra return value for notice. */
	worsen = FALSE;

	/* Determine whether further needs to be done. */
	notice = (*t_ptr->notice)(*var, v);

	/* Set the new value. */
	*var = v;

	/* Nothing to notice */
	if (notice < 0) return (FALSE);

	/* Find the text string. */
	msg = temp_effects_text[t_ptr->text + notice + OFFSET(flag)];

	/* Print it if it's real. */
	if (*msg) msg_print(msg);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Hack - carry out side-effects, if any. */
	if (worsen && t_ptr->worsen) (*t_ptr->worsen)(v);

	/* Print the on-screen symbol, if any. */
	prt_timer(t_ptr);

	/* Recalculate various things. */
	p_ptr->update |= t_ptr->update;
	p_ptr->redraw |= t_ptr->redraw;
	p_ptr->window |= t_ptr->window;
	handle_stuff();

	/* Result */
	return (TRUE);
}

/* Some simple wrappers to set a flag by index. */

bool add_flag(int flag, int v)
{
	return set_flag_aux(flag, v, TRUE);
}

bool set_flag(int flag, int v)
{
	return set_flag_aux(flag, v, FALSE);
}

/*
 * Return an on-screen message to indicates a flag's current state.
 */
cptr PURE prt_flag(int flag)
{
	s16b *var = get_flag(flag);

	int (*notice)(int, int) = temp_effects[flag].notice;

	/* Find an offset for setting a flag to a value. */
	int n = (*notice)(TIMER_DEFAULT, *var);

	/* Hack - try a couple of specific values if the default is current. */
	if (n < 0) n = (*notice)(0, *var);
	if (n < 0) n = (*notice)(20000, *var);

	/* Paranoia - didn't find one. */
	if (n < 0) return "$rERROR";

	/* Return the text string as is. */
	return temp_effects_show[n + temp_effects[flag].text + OFFSET(flag)];
}

/*
 * Return the message given when a flag moves from its "normal" state to its
 * current one.
 */
cptr PURE prt_flag_long(int flag)
{
	s16b *var = get_flag(flag);

	/* Find an offset for setting a flag to a value. */
	int n = (*temp_effects[flag].notice)(TIMER_DEFAULT, *var);

	/* Didn't find one. */
	if (n < 0) return 0;

	/* Return the long form of the message. */
	return temp_effects_text[n + temp_effects[flag].text + OFFSET(flag)];
}

/*
 * Print every on-soreen timer.
 */
void prt_timers(void)
{
	const temp_effect_type *t_ptr;

	FOR_ALL_IN(temp_effects, t_ptr)
	{
		/* Print this timer, if possible. */
		prt_timer(t_ptr);
	}
}

/*
 * Gain experience
 */
void gain_exp(s32b amount)
{
	/* Gain some experience */
	p_ptr->exp += amount;
}

/*
 * Gain random skills
 *
 * Hack - play with object_skill_count to allow a large number of skill gains.
 */
void gain_skills(s32b amount)
{
	const int old = object_skill_count;

	while (amount-- > 0)
	{
		object_skill_count = 0;

		skill_exp(rand_int(MAX_SKILLS));
	}

	object_skill_count = old;
}

/*
 * Lose experience
 */
void lose_skills(s32b amount)
{
	byte lost[MAX_SKILLS];
	int i,j,which,prev;

	/* clear the initial array */
	for (i=0;i<MAX_SKILLS;i++) lost[i] = 0;
	/* dummy 'previous' */
	prev=rand_int(MAX_SKILLS);

	/* Repeat 'amount' times */
	for (i=0;i<amount;i++)
	{
		/* Have 1000 goes at picking a random skill */
		for(j=0;j<1000;j++)
		{
			which=rand_int(MAX_SKILLS);
			/* 60% chance of continuing with previous skill */
			if(rand_int(100) < 60) which = prev;
			/* Never remove a skill completely */
			if (skill_set[which].value > 1)
			{
				/* Decrement the skill */
				skill_set[which].value--;
				/* Keep count */
				lost[which]++;
				/* This was valid so stop the loop */
				j = 1000;
				prev = which;
			}
		}
	}
	/* Tell the player what has happened */
	for(i=0;i<MAX_SKILLS;i++)
	{
		if (lost[i] > 0)
		{
			player_skill *sk_ptr = &skill_set[i];
			msg_format("Your %s skill has been lost (%d%%->%d%%)",
				sk_ptr->name,sk_ptr->value+lost[i],sk_ptr->value);

			/* Window stuff */
			p_ptr->window |= PW_PLAYER_SKILLS;
		}
	}
	/* Re-calculate some things */
	{
		/* Hack - prevent the update of arbitrary things (I haven't checked
		 * that this is necessary, but it may be). */
		u32b hack_update = p_ptr->update;

		/* Recalculate hit points, mana and spells, but nothing else. */
		p_ptr->update = PU_HP | PU_MANA | PU_SPELLS;
		update_stuff();

		p_ptr->update = hack_update;
	}

	/* Redraw spirits, as a reduction may render some inaccessible. */
	if (lost[SKILL_SHAMAN])
	{
		for (i = 0; i < MAX_SPIRITS; i++)
		{
			spirit_type *s_ptr = &spirits[i];
			if (s_ptr->pact && s_ptr->minskill > skill_set[SKILL_SHAMAN].value)
				p_ptr->redraw |= PR_SPIRIT;
		}
	}

	/* Redraw if required. */
	redraw_stuff();
}


/*
 * Alters the number of a type of monster which have been killed.
 */
static void note_monster_death(monster_race *r_ptr, s16b deaths)
{
	s16b max = MAX_SHORT-deaths;
	/* Count kills this life */
	if (r_ptr->r_pkills < max) r_ptr->r_pkills += deaths;

	/* Count kills in all lives */
	if (r_ptr->r_tkills < max) r_ptr->r_tkills += deaths;

	/* Hack -- Auto-recall */
	monster_race_track(r_ptr-r_info);
}


/*
 * Turn some of the stairs around when the player finishes a quest.
 */
static void handle_quest_stairs(void)
{
	int force_leave, force_change, t, x, y, seen;

	const int stairs[2] = {FEAT_LESS, FEAT_MORE};

	/* Notice whether the level is full of down stairs or up ones. */
	const int in = (dun_defs[cur_dungeon].flags & DF_TOWER) ? 0 : 1;

	/* Don't create stairs out of the bottom of a dungeon. */
	if (dun_level == dun_defs[cur_dungeon].max_level) return;

	/* Count the stairs. */
	for (y = t = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			if (cave[y][x].feat == stairs[!in]) t++;
		}
	}

	/* Level generation should always create at least two sets of stairs. */
	assert(t >= 2);

	/* Ensure that one is left in each direction by this process. */
	force_leave = rand_int(t);
	force_change = rand_int(t-1);
	if (force_leave == force_change) force_change = t-1;

	/* Randomise the rest. */
	for (y = t = seen = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			if (cave[y][x].feat == stairs[!in])
			{
				if (force_change == t || (force_leave != t && one_in(2)))
				{
					if (player_can_see_bold(y, x)) seen = TRUE;
					cave_set_feat(y, x, stairs[in]);
				}
				t++;
			}
		}
	}

	/* Explain the stairways. */
	msg_format("You hear a rumbling sound%s.", (seen) ? "" : " in the distance");
}

/*
 * Handle the "death" of a monster.
 *
 * Disperse treasures centered at the monster location based on the
 * various flags contained in the monster flags fields.
 *
 * Check for "Quest" completion when a quest monster is killed.
 *
 * Note that only the player can induce "monster_death()" on Uniques.
 * Thus (for now) all Quest monsters should be Uniques.
 *
 * Note that monsters can now carry objects, and when a monster dies,
 * it drops all of its objects, which may disappear in crowded rooms.
 */
void monster_death(int m_idx)
{
	int i, j, y, x;

	int dump_item = 0;
	int dump_gold = 0;

	int number = 0;
	int coin_type;

	bool quest = FALSE;


	s16b this_o_idx, next_o_idx = 0;

	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool visible = (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)));

	bool good = (r_ptr->flags1 & (RF1_DROP_GOOD)) ? TRUE : FALSE;
	bool great = (r_ptr->flags1 & (RF1_DROP_GREAT)) ? TRUE : FALSE;

	bool do_gold = (!(r_ptr->flags1 & (RF1_ONLY_ITEM)));
	bool do_item = (!(r_ptr->flags1 & (RF1_ONLY_GOLD)));

	bool cloned = FALSE;

	object_type forge;
	object_type *q_ptr;


	/* Get the location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	if (m_ptr->smart &(SM_CLONED))
		cloned = TRUE;

	/* Drop objects being carried */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Paranoia */
		o_ptr->held_m_idx = 0;

		/* Get local object */
		q_ptr = &forge;

		/* Copy the object */
		object_copy(q_ptr, o_ptr);

		/* Delete the object */
		delete_dun_object(o_ptr);

		/* Drop it */
		drop_near(q_ptr, -1, y, x);
	}

	/* Forget objects */
	m_ptr->hold_o_idx = 0;

	/* Do special things when appropriate. This may change coin_type. */
	coin_type = drop_special(m_ptr);

	/* Determine how much we can drop */
	if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 60)) number++;
	if ((r_ptr->flags1 & (RF1_DROP_90)) && (rand_int(100) < 90)) number++;
	if (r_ptr->flags1 & (RF1_DROP_1D2)) number += damroll(1, 2);
	if (r_ptr->flags1 & (RF1_DROP_2D2)) number += damroll(2, 2);
	if (r_ptr->flags1 & (RF1_DROP_3D2)) number += damroll(3, 2);
	if (r_ptr->flags1 & (RF1_DROP_4D2)) number += damroll(4, 2);

	if (cloned) number = 0; /* Clones drop no stuff */
	if ((is_quest()) && (r_ptr->flags1 & RF1_GUARDIAN))
	{
		quest_type *q_ptr = get_quest();
		q_ptr->cur_num++;
		if (visible) q_ptr->cur_num_known++;

		if (q_ptr->cur_num == q_ptr->max_num)
		{
			/* The quest monsters must have all died. */
			note_monster_death(r_ptr, q_ptr->cur_num-q_ptr->cur_num_known);

			/* Remove the block on normal generation. */
			r_ptr->flags1 &= ~(RF1_GUARDIAN);

			/* Drop at least 2 items. */
			number += 2;
			quest = TRUE;
			q_ptr->level=0;
		}

	}




	/* Average dungeon and monster levels */
	object_level = ((dun_depth) + r_ptr->level) / 2;

	/* Drop some objects */
	for (j = 0; j < number; j++)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* The first two items for a quest monster are great */
		if (quest && j < 2)
		{
			if (!make_object(q_ptr, TRUE, TRUE, FOUND_QUEST, 0)) continue;
		}
		/* Make Gold, but not for first two quest items */
		else if (do_gold && (!do_item || one_in(2)))
		{
			/* Make some gold */
			if (!make_gold(q_ptr, FOUND_MONSTER, m_ptr->r_idx, coin_type))
				continue;

			/* XXX XXX XXX */
			dump_gold++;
		}

		/* Make Object */
		else
		{
			/* Make an object */
			if (!make_object(q_ptr, good, great, FOUND_MONSTER,
				m_ptr->r_idx)) continue;

			/* XXX XXX XXX */
			dump_item++;
		}

		/* Drop it in the dungeon */
		drop_near(q_ptr, -1, y, x);
	}

	/* Reset the object level */
	object_level = (dun_depth);


	/* Take note of any dropped treasure */
	if (visible && (dump_item || dump_gold))
	{
		/* Take notes on treasure */
		lore_treasure(m_ptr->r_idx, dump_item, dump_gold);
	}


	/* Only process completed monster quests. */
	if (!quest) return;

	handle_quest_stairs();

	/* Check for an unfinished quest. */
	for (i = 0; i < MAX_Q_IDX; i++)
	{
		quest_type *q_ptr = &q_list[i];
		if (q_ptr->level || (q_ptr->cur_num != q_ptr->max_num)) return;
	}

	/* Total winner */
	total_winner = TRUE;

	/* Congratulations */
	msg_print("*** CONGRATULATIONS ***");
	msg_print("You have won the game!");
	msg_print("You may retire (commit suicide) when you are ready.");
}




/*
 * Decreases monsters hit points, handling monster death.
 *
 * We return TRUE if the monster has been killed (and deleted).
 *
 * We announce monster death (using an optional "death message"
 * if given, and a otherwise a generic killed/destroyed message).
 *
 * Only "physical attacks" can induce the "You have slain" message.
 * Missile and Spell attacks will induce the "dies" message, or
 * various "specialized" messages.  Note that "You have destroyed"
 * and "is destroyed" are synonyms for "You have slain" and "dies".
 *
 * Hack -- unseen monsters yield "You have killed it." message.
 *
 * Added fear (DGK) and check whether to print fear messages -CWS
 *
 * Genericized name, sex, and capitilization -BEN-
 *
 * As always, the "ghost" processing is a total hack.
 *
 * Hack -- we "delay" fear messages by passing around a "fear" flag.
 *
 * XXX XXX XXX Consider decreasing monster experience over time, say,
 * by using "(m_exp * m_lev * (m_lev)) / (p_lev * (m_lev + n_killed))"
 * instead of simply "(m_exp * m_lev) / (p_lev)", to make the first
 * monster worth more than subsequent monsters.  This would also need
 * to induce changes in the monster recall code.
 */
bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	s32b div, new_exp, new_exp_frac;


	/* Redraw (later) if needed */
	if (health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);


	/* Wake it up */
	m_ptr->csleep = 0;

	/* Hurt it */
	m_ptr->hp -= dam;

	/* Notice how much it has been hurt. */
	m_ptr->pl_cdam += dam;

	/* It is dead now */
	if (m_ptr->hp < 0)
	{
		C_TNEW(m_name, MNAME_MAX, char);

		/* Extract monster name */
		strnfmt(m_name, MNAME_MAX, "%v", monster_desc_f2, m_ptr, 0);

		if ((r_ptr->flags3 & (RF3_GREAT_OLD_ONE)) && (randint(2)==1))
		{
			int curses = 1 + randint(3);
			msg_format("%^s retreats into another dimension!",m_name);
			msg_print("Nyarlathotep puts a terrible curse on you!");
			curse_equipment(100, 50);
			do { activate_ty_curse(); } while (--curses);
		}

		if (speak_unique && (r_ptr->flags2 & (RF2_CAN_SPEAK)))
		{
			/* Dump a message */
			msg_format("%^s says: %v", m_name,
				get_rnd_line_f1, "mondeath.txt");

			if (one_in(REWARD_CHANCE))
			{
				int reward;

				msg_format("There was a price on %s's head.", m_name);
				msg_format("%^s was wanted for %v", m_name,
					get_rnd_line_f1, "crime.txt");
				reward = 250 * (rand_range(-4, 6) + r_ptr->level);
				reward = MIN(MAX(reward, 250), 32000); /* Force 'good' values */

				msg_format("You collect a reward of %d gold pieces.", reward);
				p_ptr->au += reward;
				p_ptr->redraw |= (PR_GOLD);
			}
		}



		/* Make a sound */
		sound(SOUND_KILL);

		/* Death by Missile/Spell attack */
		if (note)
		{
			msg_format("%^s%s", m_name, note);
		}

		/* Death by physical attack -- invisible monster */
		else if (!m_ptr->ml)
		{
			msg_format("You have killed %s.", m_name);
		}

		/* Death by Physical attack -- non-living monster */
		else if (!live_monster_p(r_ptr))
		{
			msg_format("You have destroyed %s.", m_name);
		}

		/* Death by Physical attack -- living monster */
		else
		{
			msg_format("You have slain %s.", m_name);
		}

		div = 10;

		/* Half experience for common monsters */
		if(r_ptr->r_pkills >= 19)
		{
			div = div * 2;
		}
		/* Triple experience for first kill */
		if(r_ptr->r_pkills == 0)
		{
			div = div / 3;
		}
		/* double experience for second or third kill */
		if (r_ptr->r_pkills == 1)
		{
			div = div /2;
		}
		if (r_ptr->r_pkills == 2)
		{
			div = div/2;
		}
		/* don't divide by 0 */
		if (div < 1) div = 1;
		/* Give some experience for the kill */
		new_exp = ((long)r_ptr->mexp * r_ptr->level*10) / div;

		/* Handle fractional experience */
		new_exp_frac = ((((long)r_ptr->mexp * r_ptr->level) % div)
						* 0x10000L / div) + p_ptr->exp_frac;

		/* Keep track of experience */
		if (new_exp_frac >= 0x10000L)
		{
			new_exp++;
			p_ptr->exp_frac = (u16b)(new_exp_frac - 0x10000L);
		}
		else
		{
			p_ptr->exp_frac = (u16b)new_exp_frac;
		}

		/* Gain experience */
		gain_exp(new_exp);

		/* Generate treasure */
		monster_death(m_idx);

		/* When the player kills a Unique, it stays dead */
		if (r_ptr->flags1 & (RF1_UNIQUE)) r_ptr->max_num = 0;

		/* XXX XXX Mega-Hack -- allow another ghost later
		 * Remove the slain bone file */
		if (m_ptr->r_idx == MON_PLAYER_GHOST)
		{
			char file[1024], level[8];
			sprintf(level, "bone.%03d", dun_depth);

			r_ptr->max_num = 1;

			/* Delete the bones file */
			strnfmt(file, sizeof(file), "%v",
				path_build_f2, ANGBAND_DIR_BONE, level);

			fd_kill(file);
		}

		/* Recall even invisible uniques or winners */
		if (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)))
		{
			note_monster_death(r_ptr, 1);
		}

		/* Delete the monster */
		delete_monster_idx(m_idx,TRUE);

		/* Update window */
		p_ptr->window |= PW_VISIBLE;

		/* Not afraid */
		(*fear) = FALSE;

		TFREE(m_name);

		/* Monster is dead */
		return (TRUE);
	}


#ifdef ALLOW_FEAR

	/* Mega-Hack -- Pain cancels fear if the monster can survive another hit. */
	if (m_ptr->monfear && (dam > 0) && m_ptr->hp > m_ptr->pl_mdam)
	{
		int tmp = randint(dam);

		/* Cure a little fear */
		if (tmp < m_ptr->monfear)
		{
			/* Reduce fear */
			m_ptr->monfear -= tmp;
		}

		/* Cure all the fear */
		else
		{
			/* Cure fear */
			m_ptr->monfear = 0;

			/* No more fear */
			(*fear) = FALSE;
		}
	}

	/* Sometimes a monster gets scared by damage */
	if (!m_ptr->monfear && !(r_ptr->flags3 & (RF3_NO_FEAR)))
	{
		int percentage;

		/* Percentage of fully healthy */
		percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

		/*
		 * Run (sometimes) if at 10% or less of max hit points,
		 * or (usually) when hit for half its current hit points
		 */
		if (((percentage <= 10) && (rand_int(10) < percentage)) ||
			((dam >= m_ptr->hp) && (rand_int(100) < 80)))
		{
			/* Hack -- note fear */
			(*fear) = TRUE;

			/* XXX XXX XXX Hack -- Add some timed fear */
			m_ptr->monfear = (randint(10) +
								(((dam >= m_ptr->hp) && (percentage > 7)) ?
								20 : ((11 - percentage) * 5)));
		}
	}

#endif

	/* Not dead yet */
	return (FALSE);
}



/*
 * Calculate which square to print at the top-left corner of the map.
 */
static void panel_bounds_prt(void)
{
	int panel_row_centre = ((panel_row_min + panel_row_max+1) / 2);
	int panel_col_centre = ((panel_col_min + panel_col_max+1) / 2);
	bool top, bottom, left, right, high, wide;

	panel_row_prt = (panel_row_centre - (Term->hgt-PRT_MINY)/2);
	panel_col_prt = (panel_col_centre - (Term->wid-PRT_MINX)/2);

	/* Blank space is always fine with scroll_edge. */
	if (scroll_edge) return;

	/* Look for space around the map edge. */
	top = (panel_row_prt < 0);
	bottom = (panel_row_prt > cur_hgt - (PRT_MAXY-PRT_MINY));
	high = (cur_hgt > PRT_MAXY - PRT_MINY);
	left = (panel_col_prt < 0);
	right = (panel_col_prt > cur_wid - (PRT_MAXX-PRT_MINX));
	wide = (cur_wid > PRT_MAXX - PRT_MINX);

	/* Only move if this increases the dungeon area shown. */
	if (top == bottom);

	/* Kill space above the map. */
	else if (top == high)
	{
		panel_row_prt = 0;
	}
	/* Kill space below the map. */
	else
	{
		panel_row_prt = cur_hgt - (PRT_MAXY-PRT_MINY);
	}

	/* Only move if this increases the dungeon area shown. */
	if (left == right);

	/* Kill space to the left of the map. */
	else if (left == wide)
	{
		panel_col_prt = 0;
	}
	/* Kill space to the right of the map. */
	else
	{
		panel_col_prt = cur_wid - (PRT_MAXX-PRT_MINX);
	}
}


/*
 * Calculates current boundaries
 * Called below and from "do_cmd_locate()".
 */
void panel_bounds(void)
{
	panel_row_min = panel_row * (SCREEN_HGT / 2);
	panel_row_max = panel_row_min + SCREEN_HGT - 1;
	panel_col_min = panel_col * (SCREEN_WID / 2);
	panel_col_max = panel_col_min + SCREEN_WID - 1;

	/* Calculate the printed area. */
	panel_bounds_prt();
}

void panel_bounds_center(void)
{
	panel_row = panel_row_min / (SCREEN_HGT / 2);
	panel_row_max = panel_row_min + SCREEN_HGT - 1;
	panel_col = panel_col_min / (SCREEN_WID / 2);
	panel_col_max = panel_col_min + SCREEN_WID - 1;

	/* Calculate the printed area. */
	panel_bounds_prt();
}


/*
 * Given an row (y) and col (x), this routine detects when a move
 * off the screen has occurred and figures new borders. -RAK-
 *
 * The map is reprinted if necessary.
 *
 * Setting force forces a "full update" to take place.
 */
void verify_panel(bool force)
{
	int y = py;
	int x = px;

	if ((centre_view) && !((no_centre_run && command_cmd == '.')))
	{
		int prow_min;
		int pcol_min;

		int max_prow_min = max_panel_rows * (SCREEN_HGT / 2);
		int max_pcol_min = max_panel_cols * (SCREEN_WID / 2);

		/* Center vertically */
		prow_min = y - SCREEN_HGT / 2;
		if (prow_min > max_prow_min) prow_min = max_prow_min;
		else if (prow_min < 0) prow_min = 0;

		/* Center horizontally */
		pcol_min = x - SCREEN_WID / 2;
		if (pcol_min > max_pcol_min) pcol_min = max_pcol_min;
		else if (pcol_min < 0) pcol_min = 0;

		/* Check for "no change" */
		if (!force && (prow_min == panel_row_min) && 
			(pcol_min == panel_col_min)) return;

		/* Save the new panel info */
		panel_row_min = prow_min;
		panel_col_min = pcol_min;

		/* Recalculate the boundaries */
		panel_bounds_center();
	}

	else
	{

		int prow = panel_row;
		int pcol = panel_col;

		/* Scroll screen when 2 grids from top/bottom edge */
		if (force || (y < panel_row_min + 2) || (y > panel_row_max - 2))
		{
			prow = ((y - SCREEN_HGT / 4) / (SCREEN_HGT / 2));
			if (prow > max_panel_rows) prow = max_panel_rows;
			else if (prow < 0) prow = 0;
		}

		/* Scroll screen when 4 grids from left/right edge */
		if (force || (x < panel_col_min + 4) || (x > panel_col_max - 4))
		{
			pcol = ((x - SCREEN_WID / 4) / (SCREEN_WID / 2));
			if (pcol > max_panel_cols) pcol = max_panel_cols;
			else if (pcol < 0) pcol = 0;
		}

		/* Check for "no change" */
		if (!force && (prow == panel_row) && (pcol == panel_col)) return;

		/* Hack -- optional disturb on "panel change" */
		if (disturb_panel) disturb(0);

		/* Save the new panel info */
		panel_row = prow;
		panel_col = pcol;

		/* Recalculate the boundaries */
		panel_bounds();
	}

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_PANEL);
}

/*
 * Map resizing whenever the main term changes size
 */
void resize_map(void)
{
	/* Only if the dungeon exists */
	if (!character_dungeon) return;

	/* Recalculate the map size. */
	panel_bounds_prt();

	/* Redraw everything except the top line. */
	p_ptr->redraw |= PR_ALL & ~PR_WIPE_0;

	/* Hack -- update */
	redraw_stuff();

	/* Place the cursor on the player */
	move_cursor_relative(px, py);

	/* Refresh */
	Term_fresh();
}


/*
 * Try to add an unusual keypress to the "queue".
 *
 * As this is the "end keymap" key, it will not be interpreted by keymaps.
 */
void resize_inkey(void)
{
	(void)Term_key_push(RESIZE_INKEY_KEY);
}


/*
 * Monster health description
 */
static cptr look_mon_desc(monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	int           perc;

	/* Determine if the monster is "living" (vs "undead") */
	bool living = live_monster_p(r_ptr);


	/* Healthy monsters */
	if (m_ptr->hp >= m_ptr->maxhp)
	{
		/* No damage */
		return (living ? "unhurt" : "undamaged");
	}


	/* Calculate a health "percentage" */
	perc = 100L * m_ptr->hp / m_ptr->maxhp;

	if (perc >= 60)
	{
		return (living ? "somewhat wounded" : "somewhat damaged");
	}

	if (perc >= 25)
	{
		return (living ? "wounded" : "damaged");
	}

	if (perc >= 10)
	{
		return (living ? "badly wounded" : "badly damaged");
	}

	return (living ? "almost dead" : "almost destroyed");
}





/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "comp()" and "swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
static void ang_sort_aux(vptr u, vptr v, int p, int q,
	bool (*comp)(vptr, vptr, int, int), void (*swap)(vptr, vptr, int, int))
{
	int z, a, b;

	/* Done sort */
	if (p >= q) return;

	/* Pivot */
	z = p;

	/* Begin */
	a = p;
	b = q;

	/* Partition */
	while (TRUE)
	{
		/* Slide i2 */
		while (!(*comp)(u, v, b, z)) b--;

		/* Slide i1 */
		while (!(*comp)(u, v, z, a)) a++;

		/* Done partition */
		if (a >= b) break;

		/* Swap */
		(*swap)(u, v, a, b);

		/* Advance */
		a++, b--;
	}

	/* Recurse left side */
	ang_sort_aux(u, v, p, b, comp, swap);

	/* Recurse right side */
	ang_sort_aux(u, v, b+1, q, comp, swap);
}


/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort(vptr u, vptr v, int n, bool (*comp)(vptr, vptr, int, int),
	void (*swap)(vptr, vptr, int, int))
{
	/* Sort the array */
	ang_sort_aux(u, v, 0, n-1, comp, swap);
}





/*** Targetting Code ***/

/*
 * Current target co-ordinates.
 */
static s16b target_col;
static s16b target_row;

/*
 * Determine is a monster makes a reasonable target
 *
 * The concept of "targetting" was stolen from "Morgul" (?)
 *
 * The player can target any location, or any "target-able" monster.
 *
 * Currently, a monster is "target_able" if it is visible, and if
 * the player can hit it with a projection, and the player is not
 * hallucinating.  This allows use of "use closest target" macros.
 *
 * Future versions may restrict the ability to target "trappers"
 * and "mimics", but the semantics is a little bit weird.
 */
static bool target_able(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];

	/* Monster must be alive */
	if (!m_ptr->r_idx) return (FALSE);

	/* Monster must be visible */
	if (!m_ptr->ml) return (FALSE);

#ifdef DRS_SMART_OPTIONS
	/* Monster must not be friendly */
	if (m_ptr->smart & SM_ALLY) return FALSE;
#endif

	/* Monster must be projectable */
	if (!projectable(py, px, m_ptr->fy, m_ptr->fx)) return (FALSE);

	/* Hack -- no targeting hallucinations */
	if (p_ptr->image) return (FALSE);

	/* XXX XXX XXX Hack -- Never target trappers */
	/* if (CLEAR_ATTR && (CLEAR_CHAR)) return (FALSE); */

	/* Assume okay */
	return (TRUE);
}




/*
 * Update (if necessary) and verify (if possible) the target.
 *
 * We return TRUE if the target is "okay" and FALSE otherwise.
 */
static bool target_okay(void)
{
	/* Accept stationary targets */
	if (target_who < 0) return (TRUE);

	/* Check moving targets */
	if (target_who > 0)
	{
		/* Accept reasonable targets */
		if (target_able(target_who))
		{
			monster_type *m_ptr = &m_list[target_who];

			/* Acquire monster location */
			target_row = m_ptr->fy;
			target_col = m_ptr->fx;

			/* Good target */
			return (TRUE);
		}
	}

	/* Assume no target */
	return (FALSE);
}



/*
 * Sorting hook -- comp function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by double-distance to the player.
 */
static bool ang_sort_comp_distance(vptr u, vptr v, int a, int b)
{
	byte *x = (byte*)(u);
	byte *y = (byte*)(v);

	int da, db, kx, ky;

	/* Absolute distance components */
	kx = x[a]; kx -= px; kx = ABS(kx);
	ky = y[a]; ky -= py; ky = ABS(ky);

	/* Approximate Double Distance to the first point */
	da = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

	/* Absolute distance components */
	kx = x[b]; kx -= px; kx = ABS(kx);
	ky = y[b]; ky -= py; ky = ABS(ky);

	/* Approximate Double Distance to the first point */
	db = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

	/* Compare the distances */
	return (da <= db);
}


/*
 * Sorting hook -- swap function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by distance to the player.
 */
static void ang_sort_swap_distance(vptr u, vptr v, int a, int b)
{
	byte *x = (byte*)(u);
	byte *y = (byte*)(v);

	byte temp;

	/* Swap "x" */
	temp = x[a];
	x[a] = x[b];
	x[b] = temp;

	/* Swap "y" */
	temp = y[a];
	y[a] = y[b];
	y[b] = temp;
}



/*
 * Hack -- help "select" a location (see below)
 */
static s16b target_pick(int y1, int x1, int dy, int dx)
{
	int i, v;

	int x2, y2, x3, y3, x4, y4;

	int b_i = -1, b_v = 9999;


	/* Scan the locations */
	for (i = 0; i < temp_n; i++)
	{
		/* Point 2 */
		x2 = temp_x[i];
		y2 = temp_y[i];

		/* Directed distance */
		x3 = (x2 - x1);
		y3 = (y2 - y1);

		/* Verify quadrant */
		if (dx && (x3 * dx <= 0)) continue;
		if (dy && (y3 * dy <= 0)) continue;

		/* Absolute distance */
		x4 = ABS(x3);
		y4 = ABS(y3);

		/* Verify quadrant */
		if (dy && !dx && (x4 > y4)) continue;
		if (dx && !dy && (y4 > x4)) continue;

		/* Approximate Double Distance */
		v = ((x4 > y4) ? (x4 + x4 + y4) : (y4 + y4 + x4));

		/* XXX XXX XXX Penalize location */

		/* Track best */
		if ((b_i >= 0) && (v >= b_v)) continue;

		/* Track best */
		b_i = i; b_v = v;
	}

	/* Result */
	return (b_i);
}


/*
 * Hack -- determine if a given location is "interesting"
 */
static bool target_set_accept(int y, int x)
{
	cave_type *c_ptr;

	s16b this_o_idx, next_o_idx = 0;


	/* Player grid is always interesting */
	if ((y == py) && (x == px)) return (TRUE);


	/* Handle hallucination */
	if (p_ptr->image) return (FALSE);


	/* Examine the grid */
	c_ptr = &cave[y][x];

	/* Visible monsters */
	if (c_ptr->m_idx)
	{
		monster_type *m_ptr = &m_list[c_ptr->m_idx];

		/* Visible monsters */
		if (m_ptr->ml) return (TRUE);
	}

	/* Scan all objects in the grid */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Memorized object */
		if (o_ptr->marked) return (TRUE);
	}

	/* Interesting memorized features */
	if (c_ptr->info & (CAVE_MARK))
	{
		/* Notice glyphs */
		if (c_ptr->feat == FEAT_GLYPH) return (TRUE);
		if (c_ptr->feat == FEAT_MINOR_GLYPH) return (TRUE);

		/* Notice the Pattern */
		if ((c_ptr->feat <= FEAT_PATTERN_XTRA2) &&
			(c_ptr->feat >= FEAT_PATTERN_START))
				return (TRUE);

		/* Notice doors */
		if (c_ptr->feat == FEAT_OPEN) return (TRUE);
		if (c_ptr->feat == FEAT_BROKEN) return (TRUE);

		/* Notice stairs */
		if (c_ptr->feat == FEAT_LESS) return (TRUE);
		if (c_ptr->feat == FEAT_MORE) return (TRUE);

		/* Notice shops */
		if ((c_ptr->feat >= FEAT_SHOP_HEAD) &&
			(c_ptr->feat <= FEAT_SHOP_TAIL)) return (TRUE);

		/* Notice traps */
		if ((c_ptr->feat >= FEAT_TRAP_HEAD) &&
			(c_ptr->feat <= FEAT_TRAP_TAIL)) return (TRUE);

		/* Notice doors */
		if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
			(c_ptr->feat <= FEAT_DOOR_TAIL)) return (TRUE);

		/* Notice rubble */
		if (c_ptr->feat == FEAT_RUBBLE) return (TRUE);

		/* Notice veins with treasure */
		if (c_ptr->feat == FEAT_MAGMA_K) return (TRUE);
		if (c_ptr->feat == FEAT_QUARTZ_K) return (TRUE);
	}

	/* Nope */
	return (FALSE);
}


/*
 * Prepare the "temp" array for "target_set"
 *
 * Return the number of target_able monsters in the set.
 */
static void target_set_prepare(int mode)
{
	int y, x;

	/* Reset "temp" array */
	temp_n = 0;

	/* Use "NONE" to disable monster targeting. */
	if (mode & TARGET_NONE) return;

	/* Scan the current panel */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			cave_type *c_ptr = &cave[y][x];

			/* Require line of sight, unless "look" is "expanded" */
			if (!expand_look && !player_has_los_bold(y, x)) continue;

			/* Require "interesting" contents */
			if (!target_set_accept(y, x)) continue;

			/* Require target_able monsters for "TARGET_KILL" */
			if ((mode & (TARGET_KILL)) && !target_able(c_ptr->m_idx)) continue;

			/* Save the location */
			temp_x[temp_n] = x;
			temp_y[temp_n] = y;
			temp_n++;
		}
	}

	/* Sort the positions */
	ang_sort(temp_x, temp_y, temp_n, ang_sort_comp_distance,
		ang_sort_swap_distance);
}


/*
 * Track the first known object in the same stack as o_ptr, if any.
 */
static void try_object_track(object_type *o_ptr)
{
	for (; o_ptr != o_list; o_ptr = o_list+o_ptr->next_o_idx)
	{
		if (o_ptr->marked)
		{
			object_track(o_ptr);
			return;
		}
	}
}

/*
 * Examine a grid, return a keypress.
 *
 * The "mode" argument contains the "TARGET_LOOK" bit flag, which
 * indicates that the "space" key should scan through the contents
 * of the grid, instead of simply returning immediately.  This lets
 * the "look" command get complete information, without making the
 * "target" command annoying.
 *
 * The "info" argument contains the "commands" which should be shown
 * inside the "[xxx]" text.  This string must never be empty, or grids
 * containing monsters will be displayed with an extra comma.
 *
 * Note that if a monster is in the grid, we update both the monster
 * recall info and the health bar info to track that monster.
 *
 * Eventually, we may allow multiple objects per grid, or objects
 * and terrain features in the same grid. XXX XXX XXX
 *
 * This function must handle blindness/hallucination.
 */
static int target_set_aux(int y, int x, int mode, cptr info)
{
	cave_type *c_ptr = &cave[y][x];

	s16b this_o_idx, next_o_idx = 0;

	cptr s1, s2, s3;

	bool boring;

	int feat;

	int query;

	char out_val[160];


	/* Repeat forever */
	while (1)
	{
		/* Paranoia */
		query = ' ';

		/* Assume boring */
		boring = TRUE;

		/* Default */
		s1 = "You see ";
		s2 = "";
		s3 = "";

		/* Hack -- under the player */
		if ((y == py) && (x == px))
		{
			/* Description */
			s1 = "You are ";

			/* Preposition */
			s2 = "on ";
		}


		/* Hack -- hallucination */
		if (p_ptr->image)
		{
			cptr name = "something strange";

			/* Display a message */
			sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, name, info);
			prt(out_val, 0, 0);
			move_cursor_relative(y, x);
			query = inkey();

			/* Stop on everything but "return" */
			if ((query != '\r') && (query != '\n')) break;

			/* Repeat forever */
			continue;
		}


		/* Actual monsters */
		if (c_ptr->m_idx)
		{
			monster_type *m_ptr = &m_list[c_ptr->m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Visible */
			if (m_ptr->ml)
			{
				bool recall = FALSE;

				/* Not boring */
				boring = FALSE;

				/* Hack -- track this monster race */
				monster_race_track(m_ptr->r_idx);

				/* Hack -- health bar for this monster */
				health_track(c_ptr->m_idx);

				/* Hack -- handle stuff */
				handle_stuff();

				/* Interact */
				while (1)
				{
					/* Recall */
					if (recall)
					{
						/* Save */
						Term_save();

						/* Recall on screen */
						screen_roff(m_ptr->r_idx);

						/* Hack -- Complete the prompt (again) */
						mc_add_fmt("  [r,%s]", info);

						/* Command */
						query = inkey();

						/* Restore */
						Term_load();
					}

					/* Normal */
					else
					{
						cptr clo = (m_ptr->smart & SM_CLONED) ? " (clone)": "";
						cptr all = (m_ptr->smart & SM_ALLY) ? " (allied)" : "";

						mc_put_fmt(0, 0, "%s%s%s%v (%s)%s%s [r,%s]%v",
							s1, s2, s3, monster_desc_f2, m_ptr, 0x08,
							look_mon_desc(m_ptr), clo, all, info, clear_f0);

						/* Place cursor */
						move_cursor_relative(y, x);

						/* Command */
						query = inkey();
					}

					/* Normal commands */
					if (query != 'r') break;

					/* Toggle recall */
					recall = !recall;
				}

				/* Always stop at "normal" keys */
				if ((query != '\r') && (query != '\n') && (query != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

				/* Change the intro */
				s1 = "It is ";

				/* Hack -- take account of gender */
				if (r_ptr->flags1 & (RF1_FEMALE)) s1 = "She is ";
				else if (r_ptr->flags1 & (RF1_MALE)) s1 = "He is ";

				/* Use a preposition */
				s2 = "carrying ";

				/* Scan all objects being carried */
				for (this_o_idx = m_ptr->hold_o_idx; this_o_idx;
					this_o_idx = next_o_idx)
				{
					object_type *o_ptr;

					/* Acquire object */
					o_ptr = &o_list[this_o_idx];

					/* Acquire next object */
					next_o_idx = o_ptr->next_o_idx;

					/* Describe the object */
					mc_put_fmt(0, 0, "%s%s%s%v [%s]%v", s1, s2, s3,
						object_desc_f3, o_ptr, TRUE, 3, info, clear_f0);
					move_cursor_relative(y, x);
					query = inkey();

					/* Always stop at "normal" keys */
					if (!strchr("\r\n ", query)) break;

					/* Sometimes stop at "space" key */
					if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

					/* Change the intro */
					s2 = "also carrying ";
				}

				/* Double break */
				if (this_o_idx) break;

				/* Use a preposition */
				s2 = "on ";
			}
		}


		/* Hack - track the first object in the square, if any. */
		if (c_ptr->o_idx) try_object_track(&o_list[c_ptr->o_idx]);

		/* Scan all objects in the grid */
		for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
		{
			/* Acquire object */
			object_type *o_ptr = &o_list[this_o_idx];

			/* Count the number of characters the name can take up. */
			int len = Term->wid - strlen(s1) - strlen(s2) - strlen(s3) -
				strlen(info) - strlen(" []");

			/* Acquire next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Describe it */
			if (!o_ptr->marked) continue;

			/* Not boring */
			boring = FALSE;

			/* Describe the object */
			mc_put_fmt(0, 0, "%s%s%s%.*v [%s]%v", s1, s2, s3,
				len, object_desc_f3, o_ptr, TRUE, 3, info, clear_f0);

			move_cursor_relative(y, x);
			query = inkey();

			/* Always stop at "normal" keys */
			if (!strchr("\r\n ", query)) break;

			/* Sometimes stop at "space" key */
			if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

			/* Change the intro */
			s1 = "It is ";

			/* Plurals */
			if (o_ptr->number != 1) s1 = "They are ";

			/* Preposition */
			s2 = "on ";
		}

		/* Double break */
		if (this_o_idx) break;


		/* Feature (apply "mimic") */
		feat = f_info[c_ptr->feat].mimic;

		/* Require knowledge about grid, or ability to see grid */
		if (!(c_ptr->info & (CAVE_MARK)) && !player_can_see_bold(y,x))
		{
			/* Forget feature */
			feat = FEAT_NONE;
		}

		/* Terrain feature if needed */
		if (boring || (feat > FEAT_INVIS))
		{
			/* Pick a prefix */
			if (*s2 && ((feat >= FEAT_MINOR_GLYPH) &&
						(feat <= FEAT_PATTERN_XTRA2))) s2 = "on ";
			else if (*s2 && (feat >= FEAT_DOOR_HEAD)) s2 = "in ";

			/* Pick proper indefinite article */
			s3 = "";

			/* Hack -- special introduction for store doors */
			if ((feat >= FEAT_SHOP_HEAD) && (feat <= FEAT_SHOP_TAIL))
			{
				s3 = "the entrance to ";
			}

			/* Display a message (mimic has already been applied). */
			mc_put_fmt(0, 0, "$!%s%s%s%v [%s]%v", s1, s2, s3,
				feature_desc_f2, feat, FDF_INDEF | FDF_REAL, info, clear_f0);

			move_cursor_relative(y, x);
			query = inkey();

			/* Always stop at "normal" keys */
			if (!strchr("\r\n ", query)) break;
		}

		/* Stop on everything but "return" */
		if ((query != '\r') && (query != '\n')) break;
	}

	/* Keep going */
	return (query);
}



/*
 * Hack - uniques flash violet in target mode.
 * The violet_uniques variable gives uniques, etc., a special colour when TRUE.
 */
static void do_violet_uniques(bool swap)
{
	int i;
	if (!violet_uniques && swap)
		violet_uniques = TRUE;
	else
		violet_uniques = FALSE;

	/* Redraw all visible monsters. */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Ignore unseen monsters */
		if (!m_ptr->ml) continue;

		/* Ignore colour-changing uniques half of the time unless
		 * hallucinating. This gives them the desired slow colour change. */
		if (r_ptr->flags1 & RF1_UNIQUE && r_ptr->flags1 & RF1_ATTR_MULTI &&
			!p_ptr->image && violet_uniques) continue;

		/* Redraw everything else. */
		lite_spot(m_ptr->fy, m_ptr->fx);
	}
}


/*
 * Convert a "location" (Y,X) into a "grid" (G)
 */
#define GRID(Y,X) \
	(256 * (Y) + (X))

/*
 * Convert a "grid" (G) into a "location" (Y)
 */
#define GRID_Y(G) \
	((int)((G) / 256U))

/*
 * Convert a "grid" (G) into a "location" (X)
 */
#define GRID_X(G) \
	((int)((G) % 256U))

/*
 * A simplified version of "project_path()" which places the co-ordinates
 * of a path in gp[]. The only restriction used in this version is that of
 * the range of the beam.
 */
static sint project_path(u16b *gp, int range, int y1, int x1, int y2, int x2)
{
	int y, x;

	int n = 0;
	int k = 0;

	/* Absolute */
	int ay, ax;

	/* Offsets */
	int sy, sx;

	/* Fractions */
	int frac;

	/* Scale factors */
	int full, half;

	/* Slope */
	int m;


	/* No path necessary (or allowed) */
	if ((x1 == x2) && (y1 == y2)) return (0);


	/* Analyze "dy" */
	if (y2 < y1)
	{
		ay = (y1 - y2);
		sy = -1;
	}
	else
	{
		ay = (y2 - y1);
		sy = 1;
	}

	/* Analyze "dx" */
	if (x2 < x1)
	{
		ax = (x1 - x2);
		sx = -1;
	}
	else
	{
		ax = (x2 - x1);
		sx = 1;
	}


	/* Number of "units" in one "half" grid */
	half = (ay * ax);

	/* Number of "units" in one "full" grid */
	full = half << 1;


	/* Vertical */
	if (ay > ax)
	{
		/* Start at tile edge */
		frac = ax * ax;

		/* Let m = ((dx/dy) * full) = (dx * dx * 2) = (frac * 2) */
		m = frac << 1;

		/* Start */
		y = y1 + sy;
		x = x1;

		/* Create the projection path */
		while (1)
		{
			/* Save grid */
			gp[n++] = GRID(y,x);

			/* Hack -- Check maximum range */
			if ((n + (k >> 1)) >= range) break;

			/* Slant */
			if (m)
			{
				/* Advance (X) part 1 */
				frac += m;

				/* Horizontal change */
				if (frac >= half)
				{
					/* Advance (X) part 2 */
					x += sx;

					/* Advance (X) part 3 */
					frac -= full;

					/* Track distance */
					k++;
				}
			}

			/* Advance (Y) */
			y += sy;
		}
	}

	/* Horizontal */
	else if (ax > ay)
	{
		/* Start at tile edge */
		frac = ay * ay;

		/* Let m = ((dy/dx) * full) = (dy * dy * 2) = (frac * 2) */
		m = frac << 1;

		/* Start */
		y = y1;
		x = x1 + sx;

		/* Create the projection path */
		while (1)
		{
			/* Save grid */
			gp[n++] = GRID(y,x);

			/* Hack -- Check maximum range */
			if ((n + (k >> 1)) >= range) break;

			/* Slant */
			if (m)
			{
				/* Advance (Y) part 1 */
				frac += m;

				/* Vertical change */
				if (frac >= half)
				{
					/* Advance (Y) part 2 */
					y += sy;

					/* Advance (Y) part 3 */
					frac -= full;

					/* Track distance */
					k++;
				}
			}

			/* Advance (X) */
			x += sx;
		}
	}

	/* Diagonal */
	else
	{
		/* Start */
		y = y1 + sy;
		x = x1 + sx;

		/* Create the projection path */
		while (1)
		{
			/* Save grid */
			gp[n++] = GRID(y,x);

			/* Hack -- Check maximum range */
			if ((n + (n >> 1)) >= range) break;

			/* Advance (Y) */
			y += sy;

			/* Advance (X) */
			x += sx;
		}
	}


	/* Length */
	return (n);
}

/*
 * Draw a visible path over the squares between (x1,y1) and (x2,y2).
 * The path consists of "*", which are white except where there is a
 * monster, object or feature in the grid.
 *
 * This routine has (at least) three weaknesses:
 * - remembered objects/walls which are no longer present are not shown,
 * - squares which (e.g.) the player has walked through in the dark are
 *   treated as unknown space.
 * - walls which appear strange due to hallucination aren't treated correctly.
 *
 * The first two result from information being lost from the dungeon arrays,
 * which requires changes elsewhere
 */
static sint draw_path(u16b *path, char *c, byte *a,
	int y1, int x1, int y2, int x2)
{
	int i;
	sint max;
	bool on_screen;

	/* Find the path. */
	max = project_path(path, MAX_RANGE, y1, x1, y2, x2);

	/* No path, so do nothing. */
	if (!max) return max;

	/* The starting square is never drawn, but notice if it is being
	 * displayed. In theory, it could be the last such square. */
	on_screen = panel_contains(y1, x1);

	/* Draw the path. */
	for (i = 0; i < max; i++)
	{
		byte colour;

		/* Find the co-ordinates on the level. */
		int y = GRID_Y(path[i]);
		int x = GRID_X(path[i]);

		cave_type *c_ptr = &cave[y][x];

		/*
		 * As path[] is a straight line and the screen is oblong,
		 * there is only section of path[] on-screen.
		 * If the square being drawn is visible, this is part of it.
		 * If none of it has been drawn, continue until some of it
		 * is found or the last square is reached.
		 * If some of it has been drawn, finish now as there are no
		 * more visible squares to draw.
		 *
		 *
		 */
		if (panel_contains(y,x))
			on_screen = TRUE;
		else if (on_screen)
			break;
		else
			continue;

		/* Find the position on-screen */
		move_cursor_relative(y,x);

		/* This square is being overwritten, so save the original. */
		Term_what(Term->scr->cx, Term->scr->cy, a+i, c+i);

		/* Choose a colour. */

		/* Visible monsters are red. */
		if (c_ptr->m_idx && m_list[c_ptr->m_idx].ml)
		{
			colour = TERM_L_RED;
		}
		/* Known objects are yellow. */
		else if (c_ptr->o_idx && o_list[c_ptr->o_idx].marked)
		{
			colour = TERM_YELLOW;
		}
		/* Known walls are blue. */
		else if (!cave_floor_bold(y,x) && (c_ptr->info & CAVE_MARK ||
			player_can_see_bold(y,x)))
		{
			/* Hallucination sometimes alters the player's
			 * perception. */
			if (a[i] != f_info[f_info[c_ptr->feat].mimic].gfx.xa ||
				c[i] != f_info[f_info[c_ptr->feat].mimic].gfx.xc)
			{
				switch (GRID(a[i], c[i]) % 3)
				{
					case 0: colour = TERM_L_RED; break;
					case 1: colour = TERM_YELLOW; break;
					default: colour = TERM_BLUE; break;
				}
			}
			else
			{
				colour = TERM_BLUE;
			}
		}
		/* Unknown squares are grey. */
		else if (!(c_ptr->info & (CAVE_MARK)) && !player_can_see_bold(y,x))
		{
			colour = TERM_L_DARK;
		}
		/* Unoccupied squares are white. */
		else
		{
			colour = TERM_WHITE;
		}

		/* Draw the path segment */
		(void)Term_addch(colour, '*');
	}
	return i;
}

/*
 * Load the attr/char at each point along "path" which is on screen from
 * "a" and "c". This was saved in draw_path().
 */
static void load_path(sint max, u16b *path, char *c, byte *a)
{
	int i;
	for (i = 0; i < max; i++)
	{
		if (!panel_contains(GRID_Y(path[i]), GRID_X(path[i]))) continue;
		move_cursor_relative(GRID_Y(path[i]), GRID_X(path[i]));
		(void)Term_addch(a[i], c[i]);
	}
	Term_fresh();
}


/*
 * Handle "target" and "look".
 *
 * Note that this code can be called from "get_aim_dir()".
 *
 * All locations must be on the current panel.  Consider the use of
 * "panel_bounds()" to allow "off-panel" targets, perhaps by using
 * some form of "scrolling" the map around the cursor.  XXX XXX XXX
 * That is, consider the possibility of "auto-scrolling" the screen
 * while the cursor moves around.  This may require changes in the
 * "update_mon()" code to allow "visibility" even if off panel, and
 * may require dynamic recalculation of the "temp" grid set.
 *
 * Hack -- targetting/observing an "outer border grid" may induce
 * problems, so this is not currently allowed.
 *
 * The player can use the direction keys to move among "interesting"
 * grids in a heuristic manner, or the "space", "+", and "-" keys to
 * move through the "interesting" grids in a sequential manner, or
 * can enter "location" mode, and use the direction keys to move one
 * grid at a time in any direction.  The "t" (set target) command will
 * only target a monster (as opposed to a location) if the monster is
 * target_able and the "interesting" mode is being used.
 *
 * The current grid is described using the "look" method above, and
 * a new command may be entered at any time, but note that if the
 * "TARGET_LOOK" bit flag is set (or if we are in "location" mode,
 * where "space" has no obvious meaning) then "space" will scan
 * through the description of the current grid until done, instead
 * of immediately jumping to the next "interesting" grid.  This
 * allows the "target" command to retain its old semantics.
 *
 * The "*", "+", and "-" keys may always be used to jump immediately
 * to the next (or previous) interesting grid, in the proper mode.
 *
 * The "return" key may always be used to scan through a complete
 * grid description (forever).
 *
 * This command will cancel any old target, even if used from
 * inside the "look" command.
 */
bool target_set(int mode)
{
	int i, d, m;

	int y = py;
	int x = px;

	bool done = FALSE;

	bool flag = TRUE;

	char query;

	char info[80];

	cave_type *c_ptr;

	/* Cancel target */
	target_who = 0;


	/* Cancel tracking */
	/* health_track(0); */


	/* Prepare the "temp" array */
	target_set_prepare(mode);

	/* Start near the player */
	m = 0;

	/* Interact */
	while (!done)
	{
		u16b path[MAX_RANGE];
		char path_char[MAX_RANGE];
		byte path_attr[MAX_RANGE];
		sint max = 0;

		/* Hack - uniques flash violet in target mode */
		do_violet_uniques(TRUE);

		/* Interesting grids */
		if (flag && temp_n)
		{
			y = temp_y[m];
			x = temp_x[m];

			/* Access */
			c_ptr = &cave[y][x];

			strcpy(info, "q");

			/* Allow target */
			if (target_able(c_ptr->m_idx)) strcat(info, ",t");

			if (y != py || x != px) strcat(info, ",p");
			strcat(info, ",o");
			if (temp_n > 1) strcat(info, ",+,-,<dir>");

			/* Draw the path in "target" mode, if there is one. */
			if (mode & TARGET_KILL)
				max = draw_path(path, path_char, path_attr, py, px, y, x);

			/* Describe and Prompt */
			query = target_set_aux(y, x, mode, info);

			/* Remove the path. */
			if (max) load_path(max, path, path_char, path_attr);

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no "direction" */
			d = 0;

			/* Analyze */
			switch (query)
			{
				case ESCAPE:
				case 'q':
				{
					done = TRUE;
					break;
				}

				case 't':
				case '.':
				case '5':
				case '0':
				{
					if (target_able(c_ptr->m_idx))
					{
						health_track(c_ptr->m_idx);
						target_who = c_ptr->m_idx;
						target_row = y;
						target_col = x;
						done = TRUE;
					}
					else
					{
						bell(0);
					}
					break;
				}

				case ' ':
				case '*':
				case '+':
				{
					if (++m == temp_n)
					{
						m = 0;
						if (!expand_list) done = TRUE;
					}

					/* Window stuff */
					cave_track(temp_y[m], temp_x[m]);
					break;
				}

				case '-':
				{
					if (m-- == 0)
					{
						m = temp_n - 1;
						if (!expand_list) done = TRUE;
					}

					/* Window stuff */
					cave_track(temp_y[m], temp_x[m]);
					break;
				}

				case 'p':
				{
					y = py;
					x = px;

					/* Window stuff */
					cave_track(target_row, target_col);
				}

				case 'o':
				{
					flag = FALSE;
					break;
				}

				case 'm':
				{
					break;
				}

				default:
				{
					d = get_keymap_dir(query);
					if (!d) bell(0);
					break;
				}
			}

			/* Hack -- move around */
			if (d)
			{
				/* Find a new monster */
				i = target_pick(temp_y[m], temp_x[m], ddy[d], ddx[d]);

				/* Use that grid */
				if (i >= 0) m = i;
			}
		}

		/* Arbitrary grids */
		else
		{
			/* Access */
			c_ptr = &cave[y][x];

			/* Default prompt */
			strcpy(info, "q,t");
			if (y != py || x != px) strcat(info, ",p");
			if (temp_n) strcat(info, ",m");
			strcat(info, ",<dir>");

			/* Draw the path, if there is one. */
			if (mode & TARGET_KILL)
				max = draw_path(path, path_char, path_attr, py, px, y, x);

			/* Describe and Prompt (enable "TARGET_LOOK") */
			query = target_set_aux(y, x, mode | TARGET_LOOK, info);

			/* Remove the path. */
			if (max) load_path(max, path, path_char, path_attr);

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no direction */
			d = 0;

			/* Analyze the keypress */
			switch (query)
			{
				case ESCAPE:
				case 'q':
				{
					done = TRUE;
					break;
				}

				case 't':
				case '.':
				case '5':
				case '0':
				{
					target_who = -1;
					target_row = y;
					target_col = x;
					done = TRUE;
					break;
				}

				case ' ':
				case '*':
				case '+':
				case '-':
				{
					break;
				}

				case 'p':
				{
					y = py;
					x = px;

					/* Window stuff */
					cave_track(target_row, target_col);
				}

				case 'o':
				{
					break;
				}

				case 'm':
				{
					flag = TRUE;
					break;
				}

				default:
				{
					d = get_keymap_dir(query);
					if (!d) bell(0);
					break;
				}
			}

			/* Handle "direction" */
			if (d)
			{
				x += ddx[d];
				y += ddy[d];

				/* Hack -- Verify target_col */
				if ((x>=cur_wid) || (x>panel_col_max)) x--;
				else if ((x<0) || (x<panel_col_min)) x++;

				/* Hack -- Verify target_row */
				if ((y>=cur_hgt) || (y>panel_row_max)) y--;
				else if ((y<0) || (y<panel_row_min)) y++;

				/* Window stuff */
				cave_track(y, x);
			}
		}
	}

	/* Forget */
	temp_n = 0;

	/* Clear the top line */
	prt("", 0, 0);

	/* Don't show violet uniques in normal play */
	do_violet_uniques(FALSE);

	/* Show the player's square again. */
	cave_track(py, px);

	/* Failure to set target */
	if (!target_who) return (FALSE);

	/* Success */
	return (TRUE);
}



/*
 * Get an "aiming direction" from the user.
 *
 * The "dir" is loaded with 1,2,3,4,6,7,8,9 for "actual direction", and
 * "0" for "current target", and "-1" for "entry aborted".
 *
 * Note that "Force Target", if set, will pre-empt user interaction,
 * if there is a usable target already set.
 *
 * Note that confusion over-rides any (explicit?) user choice.
 */
static bool get_aim_dir_aux(int *dp, bool allow_repeat)
{
	int dir;
	cptr    p;
	char command;

	if (allow_repeat)
	{

#ifdef ALLOW_REPEAT

		if (repeat_pull(dp)) {

			/* Confusion? */

			/* Verify */
			if (!(*dp == 5 && !target_okay())) {
				return (TRUE);
			}
		}

#endif /* ALLOW_REPEAT -- TNB */

		if (use_old_target && target_okay())
		{
			/* Hack -- auto-target if requested */
			dir = 5;
		}
		else
		{
			/* Global direction */
			dir = command_dir;
		}
	}
	else
	{
		dir = 0;
	}

	/* Ask until satisfied */
	while (!dir)
	{
		/* Choose a prompt */
		if (!target_okay())
		{
			p = "'*' to choose a target";
		}
		else
		{
			p = "'5' for target, '*' to re-target";
		}

		/* Get a command (or Cancel) */
		if (!get_com(&command, "Direction (%s, Escape to cancel)? ", p)) break;

		/* Convert various keys to "standard" keys */
		switch (command)
		{
			/* Use current target */
			case 'T':
			case 't':
			case '.':
			case '5':
			case '0':
			{
				dir = 5;
				break;
			}

			/* Set new target */
			case '*':
			{
				if (target_set(TARGET_KILL)) dir = 5;
				break;
			}

			default:
			{
				dir = get_keymap_dir(command);
				break;
			}
		}

		/* Verify requested targets */
		if ((dir == 5) && !target_okay()) dir = 0;

		/* Error */
		if (!dir) bell(0);
	}

	/* No direction */
	if (!dir)
	{
		(*dp) = 0;
		return (FALSE);
	}

	/* Save the direction */
	command_dir = dir;

	/* Check for confusion */
	if (p_ptr->confused)
	{
		/* XXX XXX XXX */
		/* Random direction */
		dir = ddd[rand_int(8)];
	}

	/* Notice confusion */
	if (command_dir != dir)
	{
		/* Warn the user */
		msg_print("You are confused.");
	}

	/* Save direction */
	(*dp) = dir;

#ifdef ALLOW_REPEAT

	repeat_push(dir);

#endif /* ALLOW_REPEAT -- TNB */

	/* A "valid" direction was entered */
	return (TRUE);
}

/*
 * A normal get_aim_dir() function as above.
 */
bool get_aim_dir(int *dp)
{
	return get_aim_dir_aux(dp, TRUE);
}

/*
 * A version of get_aim_dir which doesn't use previously stored targets, etc..
 * This is desirable when the player did not initiate the target call in the
 * first place (as with MUT_PROD_MANA).
 */
bool get_hack_dir(int *dp)
{
	return get_aim_dir_aux(dp, FALSE);
}

/*
 * Request a "movement" direction (1,2,3,4,6,7,8,9) from the user,
 * and place it into "command_dir", unless we already have one.
 *
 * This function should be used for all "repeatable" commands, such as
 * run, walk, open, close, bash, disarm, spike, tunnel, etc, as well
 * as all commands which must reference a grid adjacent to the player,
 * and which may not reference the grid under the player.  Note that,
 * for example, it is no longer possible to "disarm" or "open" chests
 * in the same grid as the player.
 *
 * Direction "5" is illegal and will (cleanly) abort the command.
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", to which "confusion" is applied.
 */
bool get_rep_dir(int *dp)
{
	int dir;

#ifdef ALLOW_REPEAT

	if (repeat_pull(dp)) {
		return (TRUE);
	}

#endif /* ALLOW_REPEAT -- TNB */

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = command_dir;

	/* Get a direction */
	while (!dir)
	{
		char ch;

		/* Get a command (or Cancel) */
		if (!get_com(&ch, "Direction (Escape to cancel)? ")) break;

		/* Look up the direction */
		dir = get_keymap_dir(ch);

		/* Oops */
		if (!dir) bell(0);
	}

	/* Prevent weirdness */
	if (dir == 5) dir = 0;

	/* Aborted */
	if (!dir) return (FALSE);

	/* Save desired direction */
	command_dir = dir;

	/* Apply "confusion" */
	if (p_ptr->confused)
	{
		/* Standard confusion */
		if (rand_int(100) < 75)
		{
			/* Random direction */
			dir = ddd[rand_int(8)];
		}
	}

	/* Notice confusion */
	if (command_dir != dir)
	{
		/* Warn the user */
		msg_print("You are confused.");
	}

	/* Save direction */
	(*dp) = dir;


#ifdef ALLOW_REPEAT

	repeat_push(dir);

#endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}

/*
 * Call get_rep_dir() and return its target as a location rather than a
 * direction.
 */
bool get_rep_target(int *x, int *y)
{
	int dir;

	/* Request a direction and abort if appropriate. */
	if (!get_rep_dir(&dir)) return FALSE;

	*x = px + ddx[dir];
	*y = py + ddy[dir];

	return TRUE;
}

/*
 * Extract the target from a get_aim_dir() call.
 * This does not check that the chosen target is suitable, as "okay" may fail
 * on targets which can be selected with direction 5.
 * If a test is needed, following this up with a suitable move_in_direction()
 * call is one way of obtaining it.
 */
void get_dir_target(int *x, int *y, int dir, int (*okay)(int, int, int))
{
	assert(x && y); /* Caller */

	if (dir == 5 && target_okay())
	{
		*x = target_col;
		*y = target_row;
	}
	else if (okay)
	{
		move_in_direction(x, y, px, py, px+99*ddx[dir], py+99*ddy[dir], okay);
	}
	else
	{
		*x = px+99*ddx[dir];
		*y = py+99*ddy[dir];
	}
}

/*
 * Run through a string backwards, replacing ". CM_ACT | MCI_ARTICLE " with an
 * indefinite article as we go. Return the start of the resulting string.
 *
 * NB: This assumes that CM_ACT | MCI_ARTICLE is never used at the start of the
 * string.
 */
void convert_articles(char *str)
{
	cptr s, a;
	char *t;

	/* Parse backwards, deciphering articles. */
	for (s = t = strchr(str, '\0'), a = "!";; s--, t--)
	{
		if (*s == (CM_ACT | MCI_ARTICLE))
		{
			if (a[1]) *t-- = a[1];
			*t = a[0];
			s--;
		}
		else if (s != t)
		{
			*t = *s;
		}
		if (!ISALNUM(*t)) ;
		else if (strchr("aeiouAEIOU8", *t)) a = "an";
		else a = "a";

		if (s == str) break;
	}

	/* Copy to the start of the string if necessary. */
	if (t != str) for (s = t, t = str; ((*t++ = *s++)););
}

/*
 * Describe the name of a feature.
 *
 * Format = feature_desc_f2, (int)feat, (int)flags
 */
void feature_desc_f2(char *buf, uint max, cptr UNUSED fmt, va_list *vp)
{
	int feat = va_arg(*vp, int);
	int flags = va_arg(*vp, int);
	int num = (flags & FDF_MANY) ? 2 : 1;
	cptr name;

	if (~flags & FDF_REAL) feat = f_info[feat].mimic;

	name = f_name+f_info[feat].name;

	flags &= (FDF_INDEF | FDF_DEF | FDF_YOUR | FDF_MANY);

	/* Use the same description method as for monsters. */
	strnfmt(buf, max, "%v", monster_desc_aux_f3, name, num, flags);
}

static const int chaos_weapon[][2] =
{
	{OBJ_DAGGER, 4},
	{OBJ_MAIN_GAUCHE, 8},
	{OBJ_RAPIER, 12},
	{OBJ_SMALL_SWORD, 16},
	{OBJ_SHORT_SWORD, 20},
	{OBJ_SABRE, 26},
	{OBJ_CUTLASS, 32},
	{OBJ_TULWAR, 34},
	{OBJ_BROAD_SWORD, 40},
	{OBJ_LONG_SWORD, 46},
	{OBJ_SCIMITAR, 52},
	{OBJ_KATANA, 54},
	{OBJ_BASTARD_SWORD, 58},
	{OBJ_TWO_HANDED_SWORD, 62},
	{OBJ_EXECUTIONERS_SWORD, 64},
	{OBJ_BLADE_OF_CHAOS, 100},
};

void gain_level_reward(int effect, int skill)
{
	cptr c_name = chaos_patron_shorts[p_ptr->chaos_patron];
	char wrath_reason[32] = "";
	int i;

	if (!effect)
	{
		int type, nasty_chance;

		if (multi_rew) return;
		else multi_rew = TRUE;

		if (one_in(6))
		{
			msg_format("%^s rewards you with a chaos feature!", c_name);
			(void)gain_chaos_feature(0);
			return;
		}
		else
		{
			if (skill/2 == 13) nasty_chance = 2;
			else if (!(skill/2 % 13)) nasty_chance = 3;
			else if (!(skill/2 % 14)) nasty_chance = 12;
			else nasty_chance = 6;

			if (one_in(nasty_chance))
				type = rand_int(20); /* Allow the 'nasty' effects */
			else
				type = rand_int(15) + 5; /* Or disallow them */

			effect = chaos_rewards[p_ptr->chaos_patron][type];
		}
	}

	sprintf(wrath_reason, "the Wrath of %s", c_name);

	switch (effect)
	{
		case REW_POLY_SLF:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Thou needst a new form, mortal!'");
			do_poly_self();
			break;
		case REW_GAIN_EXP:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Well done, mortal! Lead on!'");
			msg_print("You feel more experienced.");
			gain_skills(200);
			break;
		case REW_LOSE_EXP:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Thou didst not deserve that, slave.'");
			lose_skills(5);
			break;
		case REW_GOOD_OBJ:
			msg_format("The voice of %s whispers:", c_name);
			msg_print("'Use my gift wisely.'");
			acquirement(py, px, 1, FALSE);
			break;
		case REW_GREA_OBJ:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Use my gift wisely.'");
			acquirement(py, px, 1, TRUE);
			break;
		case REW_CHAOS_WP:
		{
			object_type q_ptr[1];

			/* Pick a random weapon. */
			int i, wp = rand_int(skill);

			/* Find it on the list. */
			for (i = 0; wp >= chaos_weapon[i][1]; i++);

			/* Message. */
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Thy deed hath earned thee a worthy blade.'");

			/* Create the weapon. */
			object_prep(q_ptr, chaos_weapon[i][0]);
			q_ptr->name2 = EGO_CHAOTIC;
			apply_magic_2(q_ptr, dun_depth);
			q_ptr->to_h = 3 + (randint((dun_depth)))%10;
			q_ptr->to_d = 3 + (randint((dun_depth)))%10;

			/* Set the "how it was found" information. */
			set_object_found(q_ptr, FOUND_CHAOS, p_ptr->chaos_patron+1);

			/* Drop it in the dungeon */
			drop_near(q_ptr, -1, py, px);
			break;
		}
		case REW_GOOD_OBS:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Thy deed hath earned thee a worthy reward.'");
			acquirement(py, px, randint(2) + 1, FALSE);
			break;
		case REW_GREA_OBS:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Behold, mortal, how generously I reward thy loyalty.'");
			acquirement(py, px, randint(2) + 1, TRUE);
			break;
		case REW_TY_CURSE:
			msg_format("The voice of %s thunders:", c_name);
			msg_print("'Thou art growing arrogant, mortal.'");
			activate_ty_curse();
			break;
		case REW_SUMMON_M:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'My pets, destroy the arrogant mortal!'");
			for (i = 0; i < rand_range(2, 6); i++)
			{
				(void) summon_specific(py, px, (dun_depth), 0);
			}
			break;
		case REW_H_SUMMON:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Thou needst worthier opponents!'");
			activate_hi_summon();
			break;
		case REW_DO_HAVOC:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Death and destruction! This pleaseth me!'");
			call_chaos(skill/2);
			break;
		case REW_GAIN_ABL:
			msg_format("The voice of %s rings out:", c_name);
			msg_print("'Stay, mortal, and let me mould thee.'");
			if ((randint(3)==1) && !(chaos_stats[p_ptr->chaos_patron] < 0))
				do_inc_stat(chaos_stats[p_ptr->chaos_patron]);
			else
				do_inc_stat((randint(6))-1);
			break;
		case REW_LOSE_ABL:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'I grow tired of thee, mortal.'");
			if ((randint(3)==1) && !(chaos_stats[p_ptr->chaos_patron] < 0))
				do_dec_stat(chaos_stats[p_ptr->chaos_patron]);
			else
				(void) do_dec_stat(randint(6)-1);
			break;
		case REW_RUIN_ABL:
			msg_format("The voice of %s thunders:", c_name);
			msg_print("'Thou needst a lesson in humility, mortal!'");
			msg_print("You feel less powerful!");
			for (i = 0; i < A_MAX; i++)
			{
				(void) dec_stat(i, 10 + randint(15), TRUE);
			}
			break;
		case REW_POLY_WND:
			msg_format("You feel the power of %s touch you.", c_name);
			do_poly_wounds(MON_CHAOS_PATRON);
			break;
		case REW_AUGM_ABL:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Receive this modest gift from me!'");
			for (i = 0; i < A_MAX; i++)
			{
				(void) do_inc_stat(i);
			}
			break;
		case REW_HURT_LOT:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Suffer, pathetic fool!'");
			fire_ball(GF_DISINTEGRATE, 0, (skill/2 * 4), 4);
			take_hit(skill/2 * 4, wrath_reason, MON_CHAOS_PATRON);
			break;
		case REW_HEAL_FUL:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Rise, my servant!'");
			restore_level();
			(void)set_flag(TIMED_POISONED, 0);
			(void)set_flag(TIMED_BLIND, 0);
			(void)set_flag(TIMED_CONFUSED, 0);
			(void)set_flag(TIMED_IMAGE, 0);
			(void)set_flag(TIMED_STUN, 0);
			(void)set_flag(TIMED_CUT, 0);
			hp_player(5000);
			do_res_stats();
			break;
		case REW_CURSE_WP:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Thou reliest too much on thy weapon.'");
			(void)curse_weapon();
			break;
		case REW_CURSE_AR:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Thou reliest too much on thine equipment.'");
			(void)curse_armor();
			break;
		case REW_PISS_OFF:
			msg_format("The voice of %s whispers:", c_name);
			msg_print("'Now thou shalt pay for annoying me.'");
			switch(randint(4))
			{
					case 1:
						activate_ty_curse();
						break;
					case 2:
						activate_hi_summon();
						break;
					case 3:
						if (randint(2)==1) (void)curse_weapon();
						else (void)curse_armor();
						break;
					default:
					for (i = 0; i < A_MAX; i++)
					{
						(void) dec_stat(i, 10 + randint(15), TRUE);
					}
			}
			break;
		case REW_WRATH:
			msg_format("The voice of %s thunders:", c_name);
			msg_print("'Die, mortal!'");
			take_hit(skill/2 * 4, wrath_reason, MON_CHAOS_PATRON);
			for (i = 0; i < A_MAX; i++)
			{
				(void) dec_stat(i, 10 + randint(15), FALSE);
			}
			activate_hi_summon();
			activate_ty_curse();
			if (randint(2)==1) (void)curse_weapon();
			if (randint(2)==1) (void)curse_armor();
			break;
		case REW_DESTRUCT:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Death and destruction! This pleaseth me!'");
			destroy_area(py, px, 25, TRUE);
			break;
		case REW_GENOCIDE:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Let me relieve thee of thine oppressors!'");
			(void) genocide(FALSE);
			break;
		case REW_MASS_GEN:
			msg_format("The voice of %s booms out:", c_name);
			msg_print("'Let me relieve thee of thine oppressors!'");
			(void) mass_genocide(FALSE);
			break;
		case REW_DISPEL_C:
			msg_format("You can feel the power of %s assault your enemies!",
				c_name);
			(void) dispel_monsters(skill/2 * 4);
			break;
		case REW_IGNORE:
			msg_format("%s ignores you.", c_name);
			break;
		case REW_SER_DEMO:
			msg_format("%s rewards you with a demonic servant!", c_name);
			if (!(summon_specific_friendly(py, px, dun_depth, SUMMON_DEMON,
				FALSE)))
				msg_print("Nobody ever turns up...");
			break;
		case REW_SER_MONS:
			msg_format("%s rewards you with a servant!",c_name);
			if (!(summon_specific_friendly(py, px, dun_depth, 0, FALSE)))
				msg_print("Nobody ever turns up...");
			break;
		case REW_SER_UNDE:
			msg_format("%s rewards you with an undead servant!",c_name);
			if (!(summon_specific_friendly(py, px, dun_depth, SUMMON_UNDEAD,
				FALSE)))
				msg_print("Nobody ever turns up...");
			break;
		default:
			msg_format("The voice of %s stammers:", c_name);
			msg_format("'Uh... uh... the answer's %d, what's the question?'",
				effect);
		}


}


typedef struct chaos_type chaos_type;
struct chaos_type
{
#ifdef CHECK_ARRAYS
	int idx;
#endif /* CHECK_ARRAYS */
	int chance;
	cptr desc;
	cptr gain;
	cptr lose;
};

static chaos_type chaos_info[] =
{
	{IDX(MUT_SPIT_ACID) 4,
		"You can spit acid (dam lvl).",
		"You gain the ability to spit acid.",
		"You lose the ability to spit acid."},
	{IDX(MUT_BR_FIRE) 3,
		"You can breathe fire (dam lvl * 2).",
		"You gain the ability to breathe fire.",
		"You lose the ability to breathe fire."},
	{IDX(MUT_HYPN_GAZE) 2,
		"Your gaze is hypnotic.",
		"Your eyes look mesmerizing...",
		"Your eyes look uninteresting."},
	{IDX(MUT_TELEKINES) 2,
		"You are telekinetic.",
		"You gain the ability to move objects telekinetically.",
		"You lose the ability to move objects telekinetically."},
	{IDX(MUT_VTELEPORT) 3,
		"You can teleport at will.",
		"You gain the power of teleportation at will.",
		"You lose the power of teleportation at will."},
	{IDX(MUT_MIND_BLST) 2,
		"You can Mind Blast your enemies.",
		"You gain the power of Mind Blast.",
		"You lose the power of Mind Blast."},
	{IDX(MUT_RADIATION) 2,
		"You can emit hard radiation at will.",
		"You start emitting hard radiation.",
		"You stop emitting hard radiation."},
	{IDX(MUT_VAMPIRISM) 2,
		"You can drain life from a foe like a vampire.",
		"You become vampiric.",
		"You are no longer vampiric."},
	{IDX(MUT_SMELL_MET) 3,
		"You can smell nearby precious metal.",
		"You smell a metallic odor.",
		"You no longer smell a metallic odor."},
	{IDX(MUT_SMELL_MON) 4,
		"You can smell nearby monsters.",
		"You smell filthy monsters.",
		"You no longer smell filthy monsters."},
	{IDX(MUT_BLINK) 3,
		"You can teleport yourself short distances.",
		"You gain the power of minor teleportation.",
		"You lose the power of minor teleportation."},
	{IDX(MUT_EAT_ROCK) 2,
		"You can consume solid rock.",
		"The walls look delicious.",
		"The walls look unappetizing."},
	{IDX(MUT_SWAP_POS) 2,
		"You can switch locations with another being.",
		"You feel like walking a mile in someone else's shoes.",
		"You feel like staying in your own shoes."},
	{IDX(MUT_SHRIEK) 3,
		"You can emit a horrible shriek.",
		"Your vocal cords get much tougher.",
		"Your vocal cords get much weaker."},
	{IDX(MUT_ILLUMINE) 3,
		"You can emit bright light.",
		"You can light up rooms with your presence.",
		"You can no longer light up rooms with your presence."},
	{IDX(MUT_DET_CURSE) 2,
		"You can feel the danger of evil magic.",
		"You can feel evil magics.",
		"You can no longer feel evil magics."},
	{IDX(MUT_BERSERK) 3,
		"You can drive yourself into a berserk frenzy.",
		"You feel a controlled rage.",
		"You no longer feel a controlled rage."},
	{IDX(MUT_POLYMORPH) 1,
		"You can polymorph yourself at will.",
		"Your body seems mutable.",
		"Your body seems stable."},
	{IDX(MUT_MIDAS_TCH) 2,
		"You can turn ordinary items to gold.",
		"You gain the Midas touch.",
		"You lose the Midas touch."},
	{IDX(MUT_GROW_MOULD) 1,
		"You can cause mould to grow near you.",
		"You feel a sudden affinity for mould.",
		"You feel a sudden dislike for mould."},
	{IDX(MUT_RESIST) 3,
		"You can harden yourself to the ravages of the elements.",
		"You feel like you can protect yourself.",
		"You feel like you might be vulnerable."},
	{IDX(MUT_EARTHQUAKE) 3,
		"You can bring down the dungeon around your ears.",
		"You gain the ability to wreck the dungeon.",
		"You lose the ability to wreck the dungeon."},
	{IDX(MUT_EAT_MAGIC) 1,
		"You can consume magic energy for your own use.",
		"Your magic items look delicious.",
		"Your magic items no longer look delicious."},
	{IDX(MUT_WEIGH_MAG) 2,
		"You can feel the strength of the magics affecting you.",
		"You feel you can better understand the magic around you.",
		"You no longer sense magic."},
	{IDX(MUT_STERILITY) 1,
		"You can cause mass impotence.",
		"You can give everything around you a headache.",
		"You hear a massed sigh of relief."},
	{IDX(MUT_PANIC_HIT) 2,
		"You can run for your life after hitting something.",
		"You suddenly understand how thieves feel.",
		"You no longer feel jumpy."},
	{IDX(MUT_DAZZLE) 3,
		"You can emit confusing, blinding radiation.",
		"You gain the ability to emit dazzling lights.",
		"You lose the ability to emit dazzling lights."},
	{IDX(MUT_EYE_BEAM) 3,
		"Your eyes can fire beams of light.",
		"Your eyes burn for a moment.",
		"Your eyes burn for a moment, then feel soothed."},
	{IDX(MUT_RECALL) 2,
		"You can travel between town and the depths.",
		"You feel briefly homesick, but it passes.",
		"You feel briefly homesick."},
	{IDX(MUT_BANISH) 1,
		"You can send evil creatures directly to Hell.",
		"You feel a holy wrath fill you.",
		"You no longer feel a holy wrath."},
	{IDX(MUT_COLD_TOUCH) 2,
		"You can freeze things with a touch.",
		"Your hands get very cold.",
		"Your hands warm up."},
	{IDX(MUT_LAUNCHER) 2,
		"You can hurl objects with great force.",
		"Your throwing arm feels much stronger.",
		"Your throwing arm feels much weaker."},
	{IDX(MUT_BERS_RAGE) 1,
		"You are subject to berserker fits.",
		"You become subject to fits of berserk rage!",
		"You are no longer subject to fits of berserk rage!"},
	{IDX(MUT_COWARDICE) 1,
		"You are subject to cowardice.",
		"You become an incredible coward!",
		"You are no longer an incredible coward!"},
	{IDX(MUT_RTELEPORT) 1,
		"You are teleporting randomly.",
		"Your position seems very uncertain...",
		"Your position seems more certain."},
	{IDX(MUT_ALCOHOL) 1,
		"Your body produces alcohol.",
		"Your body starts producing alcohol!",
		"Your body stops producing alcohol!"},
	{IDX(MUT_HALLU) 1,
		"You have a hallucinatory insanity.",
		"You are afflicted by a hallucinatory insanity!",
		"You are no longer afflicted by a hallucinatory insanity!"},
	{IDX(MUT_FLATULENT) 1,
		"You are subject to uncontrollable flatulence.",
		"You become subject to uncontrollable flatulence.",
		"You are no longer subject to uncontrollable flatulence."},
	{IDX(MUT_SCOR_TAIL) 2,
		"You have a scorpion tail (poison, 7d3).",
		"You grow a scorpion tail!",
		"You lose your scorpion tail!"},
	{IDX(MUT_HORNS) 2,
		"You have horns (dam. 6d2).",
		"Horns pop forth into your forehead!",
		"Your horns vanish from your forehead!"},
	{IDX(MUT_BEAK) 2,
		"You have a beak (dam. 4d2).",
		"Your mouth turns into a sharp, powerful beak!",
		"Your mouth reverts to normal!"},
	{IDX(MUT_ATT_DEMON) 2,
		"You attract demons.",
		"You start attracting demons.",
		"You stop attracting demons."},
	{IDX(MUT_PROD_MANA) 1,
		"You are producing magical energy uncontrollably.",
		"You start producing magical energy uncontrollably.",
		"You stop producing magical energy uncontrollably."},
	{IDX(MUT_SPEED_FLUX) 2,
		"You move faster or slower randomly.",
		"You become manic-depressive.",
		"You are no longer manic-depressive."},
	{IDX(MUT_BANISH_ALL) 2,
		"You sometimes cause nearby creatures to vanish.",
		"You feel a terrifying power lurking behind you.",
		"You no longer feel a terrifying power lurking behind you."},
	{IDX(MUT_EAT_LIGHT) 1,
		"You sometimes feed off of the light around you.",
		"You feel a strange kinship with Nyogtha.",
		"You feel the world's a brighter place."},
	{IDX(MUT_TRUNK) 2,
		"You have an elephantine trunk (dam 4).",
		"Your nose grows into an elephant-like trunk.",
		"Your nose returns to a normal length."},
	{IDX(MUT_ATT_ANIMAL) 1,
		"You attract animals.",
		"You start attracting animals.",
		"You stop attracting animals."},
	{IDX(MUT_TENTACLES) 1,
		"You have evil looking tentacles (hellfire, dam 5d2).",
		"Evil-looking tentacles sprout from your mouth.",
		"Your tentacles vanish."},
	{IDX(MUT_RAW_CHAOS) 1,
		"You occasionally are surrounded with raw chaos.",
		"You feel the universe is less stable around you.",
		"You feel the universe is more stable around you."},
	{IDX(MUT_NORMALITY) 3,
		"You may be chaotic, but you're recovering.",
		"You feel strangely normal.",
		"You feel normally strange."},
	{IDX(MUT_WRAITH) 1,
		"You fade in and out of physical reality.",
		"You start to fade in and out of the physical world.",
		"You are firmly in the physical world."},
	{IDX(MUT_POLY_WOUND) 1,
		"Your health is subject to chaotic forces.",
		"You feel forces of chaos entering your old scars.",
		"You feel forces of chaos departing your old scars."},
	{IDX(MUT_WASTING) 1,
		"You have a horrible wasting disease.",
		"You suddenly contract a horrible wasting disease.",
		"You are cured of the horrible wasting disease!"},
	{IDX(MUT_ATT_DRAGON) 1,
		"You attract dragons.",
		"You start attracting dragons.",
		"You stop attracting dragons."},
	{IDX(MUT_WEIRD_MIND) 2,
		"Your mind randomly expands and contracts.",
		"Your thoughts suddenly take off in strange directions.",
		"Your thoughts return to boring paths."},
	{IDX(MUT_NAUSEA) 1,
		"You have a seriously upset stomach.",
		"Your stomach starts to roil nauseously.",
		"Your stomach stops roiling."},
	{IDX(MUT_CHAOS_GIFT) 2,
		"Chaos deities give you gifts.",
		"You attract the notice of a chaos deity!",
		"You lose the attention of the chaos deities."},
	{IDX(MUT_WALK_SHAD) 1,
		"You occasionally stumble into other shadows.",
		"You feel like reality is as thin as paper.",
		"You feel like you're trapped in reality."},
	{IDX(MUT_WARNING) 2,
		"You receive warnings about your foes.",
		"You suddenly feel paranoid.",
		"You no longer feel paranoid."},
	{IDX(MUT_INVULN) 1,
		"You occasionally feel invincible.",
		"You are blessed with fits of invulnerability.",
		"You are no longer blessed with fits of invulnerability."},
	{IDX(MUT_SP_TO_HP) 2,
		"Your blood sometimes rushes to your muscles.",
		"You are subject to fits of magical healing.",
		"You are no longer subject to fits of magical healing."},
	{IDX(MUT_HP_TO_SP) 1,
		"Your blood sometimes rushes to your head.",
		"You are subject to fits of painful clarity.",
		"You are no longer subject to fits of painful clarity."},
	{IDX(MUT_DISARM) 1,
		"You occasionally stumble and drop things.",
		"Your feet grow to four times their former size.",
		"Your feet shrink to their former size."},
	{IDX(MUT_HYPER_STR) 3,
		"You are superhumanly strong (+4 STR).",
		"You turn into a superhuman he-man!",
		"Your muscles revert to normal."},
	{IDX(MUT_PUNY) 3,
		"You are puny (-4 STR).",
		"Your muscles wither away...",
		"Your muscles revert to normal."},
	{IDX(MUT_HYPER_INT) 3,
		"Your brain is a living computer (+4 INT/WIS).",
		"Your brain evolves into a living computer!",
		"Your brain reverts to normal."},
	{IDX(MUT_MORONIC) 3,
		"You are moronic (-4 INT/WIS).",
		"Your brain withers away...",
		"Your brain reverts to normal"},
	{IDX(MUT_RESILIENT) 2,
		"You are very resilient (+4 CON).",
		"You become extraordinarily resilient.",
		"You become ordinarily resilient again."},
	{IDX(MUT_XTRA_FAT) 2,
		"You are extremely fat (+2 CON, -2 speed).",
		"You become sickeningly fat!",
		"You benefit from a miracle diet!"},
	{IDX(MUT_ALBINO) 2,
		"You are albino (-4 CON).",
		"You turn into an albino! You feel frail...",
		"You are no longer an albino!"},
	{IDX(MUT_FLESH_ROT) 3,
		"Your flesh is rotting (-2 CON, -1 CHR).",
		"Your flesh is afflicted by a rotting disease!",
		"Your flesh is no longer afflicted by a rotting disease!"},
	{IDX(MUT_SILLY_VOI) 2,
		"Your voice is a silly squeak (-4 CHR).",
		"Your voice turns into a ridiculous squeak!",
		"Your voice returns to normal."},
	{IDX(MUT_BLANK_FAC) 2,
		"Your face is featureless (-1 CHR).",
		"Your face becomes completely featureless!",
		"Your facial features return."},
	{IDX(MUT_ILL_NORM) 1,
		"Your appearance is masked with illusion.",
		"You start projecting a reassuring image.",
		"You stop projecting a reassuring image."},
	{IDX(MUT_XTRA_EYES) 3,
		"You have an extra pair of eyes (+15 search).",
		"You grow an extra pair of eyes!",
		"Your extra eyes vanish!"},
	{IDX(MUT_MAGIC_RES) 2,
		"You are resistant to magic.",
		"You become resistant to magic.",
		"You become susceptible to magic again."},
	{IDX(MUT_XTRA_NOIS) 3,
		"You make a lot of strange noise (-3 stealth).",
		"You start making strange noise!",
		"You stop making strange noise!"},
	{IDX(MUT_INFRAVIS) 3,
		"You have remarkable infravision (+3).",
		"Your infravision is improved.",
		"Your infravision is degraded."},
	{IDX(MUT_XTRA_LEGS) 2,
		"You have an extra pair of legs (+3 speed).",
		"You grow an extra pair of legs!",
		"Your extra legs disappear!"},
	{IDX(MUT_SHORT_LEG) 2,
		"Your legs are short stubs (-3 speed).",
		"Your legs turn into short stubs!",
		"Your legs lengthen to normal."},
	{IDX(MUT_ELEC_TOUC) 2,
		"Electricity is running through your veins.",
		"Electricity starts running through you!",
		"Electricity stops running through you."},
	{IDX(MUT_FIRE_BODY) 2,
		"Your body is enveloped in flames.",
		"Your body is enveloped in flames!",
		"Your body is no longer enveloped in flames."},
	{IDX(MUT_WART_SKIN) 3,
		"Your skin is covered with warts (-2 CHR, +5 AC).",
		"Disgusting warts appear everywhere on you!",
		"Your warts disappear!"},
	{IDX(MUT_SCALES) 3,
		"Your skin has turned into scales (-1 CHR, +10 AC).",
		"Your skin turns into black scales!",
		"Your scales vanish!"},
	{IDX(MUT_IRON_SKIN) 2,
		"Your skin is made of steel (-1 DEX, +25 AC).",
		"Your skin turns to steel!",
		"Your skin reverts to flesh!"},
	{IDX(MUT_WINGS) 2,
		"You have wings.",
		"You grow a pair of wings.",
		"Your wings fall off."},
	{IDX(MUT_FEARLESS) 3,
		"You are completely fearless.",
		"You become completely fearless.",
		"You begin to feel fear again."},
	{IDX(MUT_REGEN) 2,
		"You are regenerating.",
		"You start regenerating.",
		"You stop regenerating."},
	{IDX(MUT_ESP) 2,
		"You are telepathic.",
		"You develop a telepathic ability!",
		"You lose your telepathic ability!"},
	{IDX(MUT_LIMBER) 3,
		"Your body is very limber (+3 DEX).",
		"Your muscles become limber.",
		"Your muscles stiffen."},
	{IDX(MUT_ARTHRITIS) 3,
		"Your joints ache constantly (-3 DEX).",
		"Your joints suddenly hurt.",
		"Your joints stop hurting."},
	{IDX(MUT_RES_TIME) 1,
		"You are protected from the ravages of time.",
		"You feel immortal.",
		"You feel all too mortal."},
	{IDX(MUT_VULN_ELEM) 1,
		"You are susceptible to damage from the elements.",
		"You feel strangely exposed.",
		"You feel less exposed."},
	{IDX(MUT_MOTION) 3,
		"Your movements are precise and forceful (+1 STL).",
		"You move with new assurance.",
		"You move with less assurance."},
	{IDX(MUT_SUS_STATS) 2,
		"Your body resists serious damage.",
		"You feel like you can recover from anything.",
		"You no longer feel like you can recover from anything."},
};

static int chaos_oppose[][2] =
{
	{MUT_HYPER_STR, MUT_PUNY},
	{MUT_HYPER_INT, MUT_MORONIC},
	{MUT_IRON_SKIN, MUT_SCALES},
	{MUT_IRON_SKIN, MUT_FLESH_ROT},
	{MUT_IRON_SKIN, MUT_WART_SKIN},
	{MUT_FEARLESS, MUT_COWARDICE},
	{MUT_REGEN, MUT_FLESH_ROT},
	{MUT_LIMBER, MUT_ARTHRITIS},
	{MUT_BEAK, MUT_TRUNK},
};

static int chaos_denom = 0;

/*
 * Calculate the denominator for the corruption choice function.
 * If desired, check that it's in MUT_* order.
 */
void init_chaos(void)
{
	chaos_type *ptr;
	FOR_ALL_IN(chaos_info, ptr)
	{
		chaos_denom += ptr->chance;
#ifdef CHECK_ARRAYS
		/* Check the order. */
		if (ptr->idx-1 != ptr - chaos_info) quit("chaos_info disordered.");
#endif /* CHECK_ARRAYS */
	}
}

/*
 * Return TRUE if the player has some mutations.
 */
bool PURE p_mutated(void)
{
	return (p_ptr->muta[0] || p_ptr->muta[1] || p_ptr->muta[2]);
}

/*
 * Remove all mutations from the player.
 */
void p_clear_mutations(void)
{
	C_WIPE(p_ptr->muta, 3, u32b);
}

/*
 * Determine if a player has a specific mutation.
 */
bool PURE p_has_mutation(int idx)
{
	u32b flag = p_ptr->muta[--idx/32];
	assert(idx >= 0 && idx < MUT_MAX); /* Caller uses MUT_* */
	return (flag & (1L << (idx%32))) != 0;
}

/*
 * Give the player a specific mutation.
 */
static void p_set_mutation(int idx)
{
	u32b *flag = &p_ptr->muta[--idx/32];
	assert(idx >= 0 && idx < MUT_MAX); /* Caller uses MUT_* */
	*flag |= (1L << (idx%32));
}

/*
 * Remove a specific mutation from the player.
 */
static void p_clear_mutation(int idx)
{
	u32b *flag = &p_ptr->muta[--idx/32];
	assert(idx >= 0 && idx < MUT_MAX); /* Caller uses MUT_* */
	*flag &= ~(1L << (idx%32));
}

/*
 * Choose a random mutation.
 */
static int get_rand_mutation(void)
{
	chaos_type *ch_ptr;
	int i = rand_int(chaos_denom);

	/* Set ch_ptr to a random mutation. */
	FOR_ALL_IN(chaos_info, ch_ptr)
	{
		i -= ch_ptr->chance;
		if (i < 0) break;
	}
	return ch_ptr - chaos_info+1;
}

/*
 * Choose a chaos feature.
 * If mut is 0 (or out of bounds), pick several times at random.
 * Otherwise, try the requested feature.
 */
static int pick_chaos_feature(int mut, bool want)
{
	if (mut > 0 && mut <= MUT_MAX)
	{
		if (p_has_mutation(mut) == want) return mut;
	}
	else
	{
		int i;
		for (i = 0; i < 20; i++)
		{
			mut = get_rand_mutation();
			if (p_has_mutation(mut) == want) return mut;
		}
	}
	return 0;
}

/*
 * Gain a chaos feature (choose_mut if suitable, random if not).
 * Return FALSE if nothing happened.
 */
bool gain_chaos_feature(int choose_mut)
{
	int i, (*co_ptr)[2];

	/* Choose a feature. */
	int mut = pick_chaos_feature(choose_mut, FALSE);

	/*
	 * Race-specific mutations are only considered if a random one has been
	 * chosen. They can, however, cause the program not to give a mutation.
	 */
	if (mut)
	{
		if (percent(rp_ptr->chaos_chance))
		{
			mut = (p_has_mutation(rp_ptr->chaos)) ? 0 : rp_ptr->chaos;
		}
	}

	if (!mut)
	{
		/* No feature chosen. */
		msg_print("You feel normal.");
		return FALSE;
	}

	msg_print("You change!");
	msg_print(chaos_info[mut-1].gain);
	p_set_mutation(mut);

	/* Some mutations are mutually exclusive. */
	for (i = 0; i < 2; i++)
	{
		FOR_ALL_IN(chaos_oppose, co_ptr)
		{
			if (co_ptr[0][i] == mut && p_has_mutation(co_ptr[0][!i]))
			{
				/* This is fine as lose_chaos_feature only prints one string. */
				lose_chaos_feature(co_ptr[0][!i]);
			}
		}
	}

	p_ptr->update |= PU_BONUS;
	handle_stuff();
	return TRUE;
}

/*
 * Lose a chaos feature (choose_mut if suitable, random if not).
 * Return FALSE if nothing happened.
 */
bool lose_chaos_feature(int choose_mut)
{
	/* Choose a feature. */
	int mut = pick_chaos_feature(choose_mut, TRUE);

	if (!mut)
	{
		/* No feature chosen. */
		msg_print("You feel oddly normal.");
		return FALSE;
	}

	msg_print(chaos_info[mut-1].lose);
	p_clear_mutation(mut);

	p_ptr->update |= PU_BONUS;
	handle_stuff();
	return TRUE;
}


/*
 * Copy a list of the mutations a player has to an array.
 * Copy the mutation strings to info and return the number of features found.
 * If "reject" is set, mutations which match it are ignored.
 */
int add_chaos_features(cptr *info, bool (*reject)(int))
{
	int i, j;

	assert(info && info+MUT_MAX); /* Caller */

	for (i = 1, j = 0; i <= MUT_MAX; i++)
	{
		if (p_has_mutation(i) && reject && !(*reject)(i))
		{
			info[j++] = chaos_info[i-1].desc;
		}
	}
	return j;
}

/*
 * Dummy reject function.
 */
static bool reject_nothing(int UNUSED i)
{
	return FALSE;
}

/*
 * Dump a list of the mutations a player has to a file.
 */
void dump_chaos_features(FILE * OutFile)
{
	cptr info[MUT_MAX];

	int i, j = add_chaos_features(info, reject_nothing);

	for (i = 0; i < j; i++)
	{
		fprintf(OutFile, " %s\n", info[i]);
	}
}
