/* File: xtra2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "game-event.h"
#include "option.h"
#include "raceflag.h"
#include "tvalsval.h"

#include "keypad.h"

/*
 * Pronoun sentence starter.  This relies on English's very simple verb agreement rules.
 */
static const char* const wd_He_is[3] =
{ "It is ", "He is ", "She is " };


/*** Timed effects ***/

/*
 * This code replace a lot of virtually identical functions and (ostensibly)
 * is a lot cleaner.  Note that the various "oppose" functions and the "stun"
 * and "cut" statuses need to be handled by special functions of their own,
 * as they are more complex than the ones handled by the generic code.  -AS-
 */
/*
 * KB: technically these could be private member functions, but I want to keep the headers clean
 */
static bool set_oppose_acid(int v,player_type& p);
static bool set_oppose_elec(int v,player_type& p);
static bool set_oppose_fire(int v,player_type& p);
static bool set_oppose_cold(int v,player_type& p);
static bool set_stun(int v,player_type& p);
static bool set_cut(int v,player_type& p);

typedef struct
{
  const char *on_begin, *on_end;
  u32b flag_redraw, flag_update;
  int msg;
} timed_effect;

static timed_effect effects[] =
{
	{ "You feel yourself moving faster!", "You feel yourself slow down.", 0, PU_BONUS, MSG_SPEED },
	{ "You feel yourself moving slower!", "You feel yourself speed up.", 0, PU_BONUS, MSG_SLOW },
	{ "You are blind.", "You can see again.", (PR_MAP | PR_BLIND),
	  (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS), MSG_BLIND },
	{ "You are paralyzed!", "You can move again.", PR_STATE, 0, MSG_PARALYZED },
	{ "You are confused!", "You feel less confused now.", PR_CONFUSED, 0, MSG_CONFUSED },
	{ "You are terrified!", "You feel bolder now.", PR_AFRAID, 0, MSG_AFRAID },
	{ "You feel drugged!", "You can see clearly again.", PR_MAP, 0, MSG_DRUGGED },
	{ "You are poisoned!", "You are no longer poisoned.", PR_POISONED, 0, MSG_POISONED },
	{ "", "", 0, 0, 0 },  /* TMD_CUT -- handled seperately */
	{ "", "", 0, 0, 0 },  /* TMD_STUN -- handled seperately */
	{ "You feel safe from evil!", "You no longer feel safe from evil.", 0, 0, MSG_PROT_EVIL },
	{ "You feel invulnerable!", "You feel vulnerable once more.", 0, PU_BONUS, MSG_INVULN },
	{ "You feel like a hero!", "The heroism wears off.", 0, PU_BONUS, MSG_HERO },
	{ "You feel like a killing machine!", "You feel less Berserk.", 0, PU_BONUS, MSG_BERSERK },
	{ "A mystic shield forms around your body!", "Your mystic shield crumbles away.", 0, PU_BONUS, MSG_SHIELD },
	{ "You feel righteous!", "The prayer has expired.", 0, PU_BONUS, MSG_BLESSED },
	{ "Your eyes feel very sensitive!", "Your eyes feel less sensitive.", 0, (PU_BONUS | PU_MONSTERS), MSG_SEE_INVIS },
	{ "Your eyes begin to tingle!", "Your eyes stop tingling.", 0, (PU_BONUS | PU_MONSTERS), MSG_INFRARED },
	{ "You feel resistant to poison!", "You feel less resistant to poison", PR_OPPOSE_ELEMENTS, 0, MSG_RES_POIS },
};

/*
 * Set a timed event (except timed resists, cutting and stunning).
 */
bool player_type::set_timed_clean(int idx, int v)
{
	bool notice = FALSE;
	const timed_effect* effect;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Hack -- call other functions */
	if (idx == TMD_STUN) return set_stun(v,*p_ptr);
	else if (idx == TMD_CUT) return set_cut(v,*p_ptr);
	else if (idx == TMD_OPP_ACID) return set_oppose_acid(v,*p_ptr);
	else if (idx == TMD_OPP_ELEC) return set_oppose_elec(v,*p_ptr);
	else if (idx == TMD_OPP_FIRE) return set_oppose_fire(v,*p_ptr);
	else if (idx == TMD_OPP_COLD) return set_oppose_cold(v,*p_ptr);

	/* Find the effect */
	effect = &effects[idx];

	/* Open */
	if (v)
	{
		if (!p_ptr->timed[idx])
		{
			message(effect->msg, 0, effect->on_begin);
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->timed[idx])
		{
			message(MSG_RECOVER, 0, effect->on_end);
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->timed[idx] = v;

	/* Nothing to notice */
	if (!notice) return FALSE;

	/* Disturb */
	if (OPTION(disturb_state)) disturb(0, 0);

	/* Update the visuals, as appropriate. */
	p_ptr->update |= effect->flag_update;
	p_ptr->redraw |= effect->flag_redraw;

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return TRUE;
}

bool player_type::dec_timed(int idx, int v)
{
	/* Check we have a valid effect */
	DEBUG_FAIL_OR_LEAVE((0 > idx) || (TMD_MAX<=idx),return FALSE);

	return set_timed_clean(idx, timed[idx] - v);
}

#ifndef ZAIBAND_STATIC_ASSERT
bool player_type::set_timed(int idx, int v)
{
	DEBUG_FAIL_OR_LEAVE((0 > idx) || (TMD_MAX<=idx),return FALSE);

	return set_timed_clean(idx,v);
}

bool player_type::inc_timed(int idx, int v)
{
	/* Check we have a valid effect */
	DEBUG_FAIL_OR_LEAVE((0 > idx) || (TMD_MAX<=idx),return FALSE);

	return set_timed_clean(idx, v + timed[idx]);
}
#endif

/* common aux function */
static bool set_timed_condition_w_immunity(int v, s16b& stat,bool immune, int msg_on,const char* const cond_on,int msg_off, const char* const cond_off)
{
	bool notice = false;

	/* Open */
	if (!immune)
	{
		if (v)
		{
			if (!stat)
			{
				message(msg_on, 0, cond_on);
				notice = true;
			}
		}

		/* Shut */
		else
		{
			if (stat)
			{
				message(msg_off, 0, cond_off);
				notice = true;
			}
		}
	}

	/* Use the value */
	stat = v;

	/* Disturb */
	if (notice && OPTION(disturb_state)) disturb(0, 0);

	return notice;
}

unsigned int stun_level(int v)
{
	if (v > 100) 	return 3;	/* Knocked out */
	if (v > 50) 	return 2;	/* Heavy stun */
	if (v > 0)	return 1;	/* Stun */
	return 0;
}

unsigned int cut_level(int v)
{
	if (v > 1000) return 7;	/* Mortal wound */
	if (v > 200) return 6;	/* Deep gash */
	if (v > 100) return 5;	/* Severe cut */
	if (v > 50) return 4;	/* Nasty cut */
	if (v > 25) return 3;	/* Bad cut */
	if (v > 10) return 2;	/* Light cut */
	if (v > 0) return 1;	/* Graze */
	return 0;
}

/*
 * Set "p_ptr->oppose_acid", notice observable changes
 */
bool set_oppose_acid(int v,player_type& p)
{
	bool notice = set_timed_condition_w_immunity(v,p.timed[TMD_OPP_ACID],p.immune_acid,MSG_RES_ACID,"You feel resistant to acid!",MSG_RECOVER,"You feel less resistant to acid.");

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Redraw */
	p.redraw |= PR_OPPOSE_ELEMENTS;

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->oppose_elec", notice observable changes
 */
bool set_oppose_elec(int v,player_type& p)
{
	bool notice = set_timed_condition_w_immunity(v,p.timed[TMD_OPP_ELEC],p.immune_elec,MSG_RES_ELEC,"You feel resistant to electricity!",MSG_RECOVER,"You feel less resistant to electricity.");

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Redraw */
	p.redraw |= PR_OPPOSE_ELEMENTS;

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->oppose_fire", notice observable changes
 */
bool set_oppose_fire(int v,player_type& p)
{
	bool notice = set_timed_condition_w_immunity(v,p.timed[TMD_OPP_FIRE],p.immune_fire,MSG_RES_FIRE,"You feel resistant to fire!",MSG_RECOVER,"You feel less resistant to fire.");

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Redraw */
	p_ptr->redraw |= PR_OPPOSE_ELEMENTS;

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->oppose_cold", notice observable changes
 */
bool set_oppose_cold(int v,player_type& p)
{
	bool notice = set_timed_condition_w_immunity(v,p.timed[TMD_OPP_COLD],p.immune_cold,MSG_RES_COLD,"You feel resistant to cold!",MSG_RECOVER,"You feel less resistant to cold.");

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Redraw */
	p.redraw |= PR_OPPOSE_ELEMENTS;

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


static const char* new_stun_level[3] =	{	"You have been stunned.",
											"You have been heavily stunned.",
											"You have been knocked out."
										};

/*
 * Set "p_ptr->stun", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
bool set_stun(int v,player_type& p)
{
	unsigned int old_aux, new_aux;

	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	old_aux = stun_level(p.timed[TMD_STUN]);
	new_aux = stun_level(v);

	/* Increase cut */
	if (new_aux > old_aux)
	{
		/* range 0..3 for both, so new_aux must be 1..3 */
		message(MSG_STUN, 0, new_stun_level[new_aux-1]);

		/* Notice */
		notice = TRUE;
	}

	/* Decrease cut */
	else if (new_aux < old_aux)
	{
		/* Describe the state */
		if (0==new_aux)
		{
			message(MSG_RECOVER, 0, "You are no longer stunned.");
		}

		/* Notice */
		notice = TRUE;
	}

	/* Use the value */
	p.timed[TMD_STUN] = v;

	/* No change */
	if (!notice) return (FALSE);

	/* Disturb */
	if (OPTION(disturb_state)) disturb(0, 0);

	/* Recalculate bonuses */
	p.update |= (PU_BONUS);

	/* Redraw the "stun" */
	p.redraw |= (PR_STUN);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

static const char* new_cut_level[7] =	{	"You have been given a graze.",
											"You have been given a light cut.",
											"You have been given a bad cut.",
											"You have been given a nasty cut.",
											"You have been given a severe cut.",
											"You have been given a deep gash.",
											"You have been given a mortal wound."
										};

/*
 * Set "p_ptr->cut", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
bool set_cut(int v,player_type& p)
{
	unsigned int old_aux, new_aux;

	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	old_aux = cut_level(p.timed[TMD_CUT]);
	new_aux = cut_level(v);

	/* Increase cut */
	if (new_aux > old_aux)
	{
		/* range 0..7 for both, so new_aux must be 1..7 */
		message(MSG_CUT, 0, new_cut_level[new_aux-1]);

		/* Notice */
		notice = TRUE;
	}

	/* Decrease cut */
	else if (new_aux < old_aux)
	{
		/* Describe the state */
		if (0==new_aux)
		{
			message(MSG_RECOVER, 0, "You are no longer bleeding.");
		};

		/* Notice */
		notice = TRUE;
	}

	/* Use the value */
	p.timed[TMD_CUT] = v;

	/* No change */
	if (!notice) return (FALSE);

	/* Disturb */
	if (OPTION(disturb_state)) disturb(0, 0);

	/* Recalculate bonuses */
	p.update |= (PU_BONUS);

	/* Redraw the "cut" */
	p.redraw |= (PR_CUT);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

unsigned int food_level(int v)
{
	if (v < PY_FOOD_FAINT) return 0;	/* Fainting / Starving */
	if (v < PY_FOOD_WEAK) return 1;		/* Weak */
	if (v < PY_FOOD_ALERT) return 2;	/* Hungry */
	if (v < PY_FOOD_FULL) return 3;		/* Normal */
	if (v < PY_FOOD_MAX) return 4;		/* Full */
	return 5;							/* Gorged */
}

static const char* less_hungry[5] =		{	"You are still weak.",
											"You are still hungry.",
											"You are no longer hungry.",
											"You are full!",
											"You have gorged yourself!"
										};

static const int more_hungry_sound[5] =	{	MSG_NOTICE,
											MSG_NOTICE,
											MSG_HUNGRY,
											MSG_NOTICE,
											MSG_NOTICE
										};

static const char* more_hungry[5] =		{	"You are getting faint from hunger!",
											"You are getting weak from hunger!",
											"You are getting hungry.",
											"You are no longer full.",
											"You are no longer gorged."
										};

/*
 * Set "p_ptr->food", notice observable changes
 *
 * The "p_ptr->food" variable can get as large as 20000, allowing the
 * addition of the most "filling" item, Elvish Waybread, which adds
 * 7500 food units, without overflowing the 32767 maximum limit.
 *
 * Perhaps we should disturb the player with various messages,
 * especially messages about hunger status changes.  XXX XXX XXX
 *
 * Digestion of food is handled in "dungeon.c", in which, normally,
 * the player digests about 20 food units per 100 game turns, more
 * when "fast", more when "regenerating", less with "slow digestion",
 * but when the player is "gorged", he digests 100 food units per 10
 * game turns, or a full 1000 food units per 100 game turns.
 *
 * Note that the player's speed is reduced by 10 units while gorged,
 * so if the player eats a single food ration (5000 food units) when
 * full (15000 food units), he will be gorged for (5000/100)*10 = 500
 * game turns, or 500/(100/5) = 25 player turns (if nothing else is
 * affecting the player speed).
 */
bool set_food(int v)
{
	unsigned int old_aux, new_aux;

	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 20000) ? 20000 : (v < 0) ? 0 : v;

	/* Fainting / Starving */
	old_aux = food_level(p_ptr->food);
	new_aux = food_level(v);

	/* Food increase */
	if (new_aux > old_aux)
	{
		msg_print(less_hungry[new_aux-1]);

		/* Change */
		notice = TRUE;
	}

	/* Food decrease */
	else if (new_aux < old_aux)
	{
		sound(more_hungry_sound[new_aux]);
		msg_print(more_hungry[new_aux]);

		/* Change */
		notice = TRUE;
	}

	/* Use the value */
	p_ptr->food = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (OPTION(disturb_state)) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw hunger */
	p_ptr->redraw |= (PR_HUNGER);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}





/*
 * Advance experience levels and print experience
 */
void check_experience(void)
{
	/* Hack -- lower limit */
	if (p_ptr->exp < 0) p_ptr->exp = 0;

	/* Hack -- lower limit */
	if (p_ptr->max_exp < 0) p_ptr->max_exp = 0;

	/* Hack -- upper limit */
	if (p_ptr->exp > PY_MAX_EXP) p_ptr->exp = PY_MAX_EXP;

	/* Hack -- upper limit */
	if (p_ptr->max_exp > PY_MAX_EXP) p_ptr->max_exp = PY_MAX_EXP;


	/* Hack -- maintain "max" experience */
	if (p_ptr->exp > p_ptr->max_exp) p_ptr->max_exp = p_ptr->exp;

	/* Redraw experience */
	p_ptr->redraw |= (PR_EXP);

	/* Handle stuff */
	handle_stuff();


	/* Lose levels while possible */
	while ((p_ptr->lev > 1) &&
	       (p_ptr->exp < (player_exp[p_ptr->lev-2] *
	                      p_ptr->expfact / 100L)))
	{
		/* Lose a level */
		p_ptr->lev--;

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE | PR_EXP);

		/* Handle stuff */
		handle_stuff();
	}


	/* Gain levels while possible */
	while ((p_ptr->lev < PY_MAX_LEVEL) &&
	       (p_ptr->exp >= (player_exp[p_ptr->lev-1] *
	                       p_ptr->expfact / 100L)))
	{
		/* Gain a level */
		p_ptr->lev++;

		/* Save the highest level */
		if (p_ptr->lev > p_ptr->max_lev) p_ptr->max_lev = p_ptr->lev;

		/* Message */
		message_format(MSG_LEVEL, p_ptr->lev, "Welcome to level %d.", p_ptr->lev);

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE | PR_EXP);

		/* Handle stuff */
		handle_stuff();
	}

	/* Gain max levels while possible */
	while ((p_ptr->max_lev < PY_MAX_LEVEL) &&
	       (p_ptr->max_exp >= (player_exp[p_ptr->max_lev-1] *
	                           p_ptr->expfact / 100L)))
	{
		/* Gain max level */
		p_ptr->max_lev++;

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE | PR_EXP);

		/* Handle stuff */
		handle_stuff();
	}
}


/*
 * Gain experience
 */
void gain_exp(s32b amount)
{
	/* Gain some experience */
	p_ptr->exp += amount;

	/* Slowly recover from experience drainage */
	if (p_ptr->exp < p_ptr->max_exp)
	{
		/* Gain max experience (10%) */
		p_ptr->max_exp += amount / 10;
	}

	/* Check Experience */
	check_experience();
}


/*
 * Lose experience
 */
void lose_exp(s32b amount)
{
	/* Never drop below zero experience */
	if (amount > p_ptr->exp) amount = p_ptr->exp;

	/* Lose some experience */
	p_ptr->exp -= amount;

	/* Check Experience */
	check_experience();
}




/*
 * Hack -- Return the "automatic coin type" of a monster race
 * Used to allocate proper treasure when "Creeping coins" die
 *
 * Note the use of actual "monster names".  XXX XXX XXX
 */
static SV_cash get_coin_type(const monster_race *r_ptr)
{
	const char* const name = r_ptr->name();

	/* Analyze "coin" monsters */
	if (r_ptr->d_char == '$')
	{
		/* Look for textual clues */
		if (strstr(name, " copper ")) return SV_COPPER;
		if (strstr(name, " silver ")) return SV_SILVER;
		if (strstr(name, " gold ")) return SV_GOLD;
		if (strstr(name, " mithril ")) return SV_MITHRIL;
		if (strstr(name, " adamantite ")) return SV_ADAMANTITE;

		/* Look for textual clues */
		if (strstr(name, "Copper ")) return SV_COPPER;
		if (strstr(name, "Silver ")) return SV_SILVER;
		if (strstr(name, "Gold ")) return SV_GOLD;
		if (strstr(name, "Mithril ")) return SV_MITHRIL;
		if (strstr(name, "Adamantite ")) return SV_ADAMANTITE;
	}

	/* Assume nothing */
	return SV_CASH;
}


/*
 * Create magical stairs after finishing a quest monster.
 */
static void build_quest_stairs(coord g)
{
	coord n;

	/* Stagger around */
	while (!cave_valid_bold(g.y, g.x))
	{
		/* Pick a location */
		scatter(n, g, 1, 0);

		/* Stagger */
		g = n;
	}

	/* Destroy any objects */
	delete_object(g.y, g.x);

	/* Explain the staircase */
	msg_print("A magical staircase appears...");

	/* Create stairs down */
	cave_set_feat(g.y, g.x, FEAT_MORE);

	/* Update the visuals */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	/* Fully update the flow */
	p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);
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
void monster_death(const m_idx_type m_idx)
{
	object_type object_type_body;
	object_type *i_ptr = &object_type_body;	/* Get local object */
	monster_type* const m_ptr = mon_list+m_idx;
	const monster_race* const r_ptr = m_ptr->race();

	int i, j;

	int dump_item = 0;
	int dump_gold = 0;

	int number = 0;
	int total = 0;

	const SV_cash force_coin = get_coin_type(r_ptr);

	s16b this_o_idx, next_o_idx = 0;

	const coord g = m_ptr->loc;	/* Get the location */

	const bool visible = (m_ptr->ml || (r_ptr->flags[0] & RF0_UNIQUE));

	const bool great = (r_ptr->flags[0] & RF0_DROP_GREAT);
	const bool good = great || (r_ptr->flags[0] & RF0_DROP_GOOD);

	const bool do_gold = (!(r_ptr->flags[0] & RF0_ONLY_ITEM));
	const bool do_item = (!(r_ptr->flags[0] & RF0_ONLY_GOLD));


	/* Drop objects being carried */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr = &o_list[this_o_idx];	/* Get the object */

		next_o_idx = o_ptr->next_o_idx;	/* Get the next object */
		o_ptr->held_m_idx = 0;			/* Paranoia */
		*i_ptr = *o_ptr;
		delete_object_idx(this_o_idx);
		drop_near(i_ptr, -1, g);		/* Drop it */
	}

	/* Forget objects */
	m_ptr->hold_o_idx = 0;


	/* Mega-Hack -- drop "winner" treasures */
	if (r_ptr->flags[0] & RF0_DROP_CHOSEN)
	{
		/* Mega-Hack -- Prepare to make "Grond" */
		object_prep(i_ptr, lookup_kind2(TV_HAFTED, SV_GROND));

		/* Mega-Hack -- Mark this item as "Grond" */
		i_ptr->name1 = ART_GROND;

		/* Mega-Hack -- Actually create "Grond" */
		apply_magic(i_ptr, -1, TRUE, TRUE, TRUE);

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, g);


		/* Mega-Hack -- Prepare to make "Morgoth" */
		object_prep(i_ptr, lookup_kind2(TV_CROWN, SV_MORGOTH));

		/* Mega-Hack -- Mark this item as "Morgoth" */
		i_ptr->name1 = ART_MORGOTH;

		/* Mega-Hack -- Actually create "Morgoth" */
		apply_magic(i_ptr, -1, TRUE, TRUE, TRUE);

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, g);
	}


	/* Determine how much we can drop */
	if ((r_ptr->flags[0] & RF0_DROP_60) && (rand_int(100) < 60)) ++number;
	if ((r_ptr->flags[0] & RF0_DROP_90) && (rand_int(100) < 90)) ++number;
	if (r_ptr->flags[0] & RF0_DROP_1D2) number += NdS(1, 2);
	if (r_ptr->flags[0] & RF0_DROP_2D2) number += NdS(2, 2);
	if (r_ptr->flags[0] & RF0_DROP_3D2) number += NdS(3, 2);
	if (r_ptr->flags[0] & RF0_DROP_4D2) number += NdS(4, 2);

	/* Average dungeon and monster levels */
	object_level = (p_ptr->depth + r_ptr->level) / 2;

	/* Drop some objects */
	for (j = 0; j < number; j++)
	{
		/* Wipe the object */
		WIPE(i_ptr);

		/* Make Gold */
		if (do_gold && (!do_item || one_in_(2)))
		{
			make_gold(i_ptr, force_coin);	/* Make some gold */
			dump_gold++;					/* Assume seen XXX XXX XXX */
		}

		/* Make Object */
		else
		{
			/* Make an object */
			if (!make_object(i_ptr, good, great)) continue;

			/* Assume seen XXX XXX XXX */
			dump_item++;
		}

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, g);
	}

	object_level = p_ptr->depth;	/* Reset the object level */

	/* Take note of any dropped treasure */
	if (visible && (dump_item || dump_gold))
	{
		/* Take notes on treasure */
		lore_treasure(m_idx, dump_item, dump_gold);
	}

	p_ptr->redraw |= PR_MONLIST;	/* Update monster list window */

	/* Only process "Quest Monsters" */
	if (!(r_ptr->flags[0] & RF0_QUESTOR)) return;


	/* Hack -- Mark quests as complete */
	for (i = 0; i < MAX_Q_IDX; i++)
	{
		/* Hack -- note completed quests */
		if (q_list[i].level == r_ptr->level) q_list[i].level = 0;

		/* Count incomplete quests */
		if (q_list[i].level) total++;
	}

	/* Build magical stairs */
	build_quest_stairs(g);

	/* Nothing left, game over... */
	if (total == 0)
	{
		p_ptr->total_winner = TRUE;		/* Total winner */
		p_ptr->redraw |= (PR_TITLE);	/* Redraw the "title" */

		/* Congratulations */
		msg_print("*** CONGRATULATIONS ***");
		msg_print("You have won the game!");
		msg_print("You may retire (commit suicide) when you are ready.");
	}
}

/*
 * KBB: Refactored non-living creature test, for grammatical purposes.
 *
 * This does misclassify jellies from a scientific perspective.  So don't use this to test whether drain life works.
 */
bool
monster_race::is_nonliving() const
{
	if (flags[2] & (RF2_DEMON | RF2_UNDEAD)) return true;
	if (flags[1] & RF1_STUPID) return true;
	if (strchr("Evg", d_char)) return true;
	return false;
}


/*
 * Decrease a monster's hit points, handle monster death.
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
 * Invisible monsters induce a special "You have killed it." message.
 *
 * Hack -- we "delay" fear messages by passing around a "fear" flag.
 *
 * Consider decreasing monster experience over time, say, by using
 * "(m_exp * m_lev * (m_lev)) / (p_lev * (m_lev + n_killed))" instead
 * of simply "(m_exp * m_lev) / (p_lev)", to make the first monster
 * worth more than subsequent monsters.  This would also need to
 * induce changes in the monster recall code.  XXX XXX XXX
 */
bool mon_take_hit(const m_idx_type m_idx, int dam, bool *fear, const char* note)
{
	monster_type* const m_ptr = mon_list+m_idx;
	monster_race* const r_ptr = m_ptr->race();
	monster_lore* const l_ptr = m_ptr->lore();

	s32b div, new_exp, new_exp_frac;


	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

	m_ptr->csleep = 0;	/* Wake it up */
	m_ptr->chp -= dam;	/* Hurt it */

	/* It is dead now */
	if (m_ptr->chp < 0)
	{
		char m_name[80];
		coord dead_mon_loc = m_ptr->loc;

		/* Assume normal death sound */
		int soundfx = MSG_KILL;

		/* Play a special sound if the monster was unique */
		if (r_ptr->flags[0] & RF0_UNIQUE) 
		{
			/* Mega-Hack -- Morgoth -- see monster_death() */
			if (r_ptr->flags[0] & RF0_DROP_CHOSEN)
				soundfx = MSG_KILL_KING;
			else
				soundfx = MSG_KILL_UNIQUE;
		}

		/* Extract monster name */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);

		/* Death by Missile/Spell attack */
		if (note)
		{
			message_format(soundfx, m_ptr->r_idx, "%^s%s", m_name, note);
		}

		/* Death by physical attack -- invisible monster */
		else if (!m_ptr->ml)
		{
			message_format(soundfx, m_ptr->r_idx, "You have killed %s.", m_name);
		}

		/* Death by Physical attack -- non-living monster */
		else if (r_ptr->is_nonliving())
		{
			message_format(soundfx, m_ptr->r_idx, "You have destroyed %s.", m_name);
		}

		/* Death by Physical attack -- living monster */
		else
		{
			message_format(soundfx, m_ptr->r_idx, "You have slain %s.", m_name);
		}

		/* Player level */
		div = p_ptr->lev;

		/* Give some experience for the kill */
		new_exp = ((long)r_ptr->mexp * r_ptr->level) / div;

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
		if (r_ptr->flags[0] & RF0_UNIQUE) r_ptr->max_num = 0;

		/* Recall even invisible uniques or winners */
		if (m_ptr->ml || (r_ptr->flags[0] & RF0_UNIQUE))
		{
			/* Count kills this life */
			if (l_ptr->pkills < MAX_S16B) l_ptr->pkills++;

			/* Count kills in all lives */
			if (l_ptr->tkills < MAX_S16B) l_ptr->tkills++;

			/* Hack -- Auto-recall */
			monster_race_track(m_ptr->r_idx);
		}

		delete_monster_idx(m_idx);	/* Delete the monster */
		(*fear) = FALSE;			/* Not afraid */
		lite_spot(dead_mon_loc);	/* XXX screen update XXX */
		return (TRUE);				/* Monster is dead */
	}


	/* Mega-Hack -- Pain cancels fear */
	if (m_ptr->monfear && (dam > 0))
	{
		int tmp = randint(dam);

		/* Cure a little fear */
		if (tmp < m_ptr->monfear)
		{
			m_ptr->monfear -= tmp;	/* Reduce fear */
		}

		/* Cure all the fear */
		else
		{
			m_ptr->monfear = 0;		/* Cure fear */
			(*fear) = FALSE;		/* No more fear */
		}
	}

	/* Sometimes a monster gets scared by damage */
	if (!m_ptr->monfear && !(r_ptr->flags[2] & RF2_NO_FEAR) && (dam > 0))
	{	/* Percentage of fully healthy */
		const int percentage = (100L * m_ptr->chp) / m_ptr->mhp;

		/*
		 * Run (sometimes) if at 10% or less of max hit points,
		 * or (usually) when hit for half its current hit points
		 */
		if ((randint(10) >= percentage) ||
		    ((dam >= m_ptr->chp) && (rand_int(100) < 80)))
		{
			(*fear) = TRUE;	/* Hack -- note fear */

			/* Hack -- Add some timed fear */
			m_ptr->monfear = (randint(10) +
			                  (((dam >= m_ptr->chp) && (percentage > 7)) ?
			                   20 : ((11 - percentage) * 5)));
		}
	}


	/* Not dead yet */
	return (FALSE);
}


/*
 * Modify the current panel to the given coordinates, adjusting only to
 * ensure the coordinates are legal, and return TRUE if anything done.
 *
 * The town should never be scrolled around.
 *
 * Note that monsters are no longer affected in any way by panel changes.
 *
 * As a total hack, whenever the current panel changes, we assume that
 * the "overhead view" window should be updated.
 */
bool modify_panel(term *t, int wy, int wx)
{
	int dungeon_hgt = (p_ptr->depth == 0) ? TOWN_HGT : DUNGEON_HGT;
	int dungeon_wid = (p_ptr->depth == 0) ? TOWN_WID : DUNGEON_WID;

	int screen_hgt = (t == Term) ? (t->hgt - ROW_MAP - 1) : t->hgt;
	int screen_wid = (t == Term) ? (t->wid - COL_MAP - 1) : t->wid;

	/* Bigtile panels only have half the width */
	if (use_bigtile) screen_wid = screen_wid / 2;

	/* Verify wy, adjust if needed */
	if (wy > dungeon_hgt - screen_hgt) wy = dungeon_hgt - screen_hgt;
	if (wy < 0) wy = 0;

	/* Verify wx, adjust if needed */
	if (wx > dungeon_wid - screen_wid) wx = dungeon_wid - screen_wid;
	if (wx < 0) wx = 0;

	/* React to changes */
	if ((t->offset_y != wy) || (t->offset_x != wx))
	{
		/* Save wy, wx */
		t->offset_y = wy;
		t->offset_x = wx;

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Changed */
		return (TRUE);
	}

	/* No change */
	return (FALSE);
}


/*
 * Monster health description
 */
static void look_mon_desc(char *buf, size_t max, const m_idx_type m_idx)
{
	monster_type* const m_ptr = mon_list+m_idx;
	monster_race* const r_ptr = m_ptr->race();

	bool living = r_ptr->is_nonliving();

	/* Healthy monsters */
	if (m_ptr->chp >= m_ptr->mhp)
	{
		/* No damage */
		my_strcpy(buf, (living ? "unhurt" : "undamaged"), max);
	}
	else
	{
		/* Calculate a health "percentage" */
		int perc = 100L * m_ptr->chp / m_ptr->mhp;

		if (perc >= 60)
			my_strcpy(buf, (living ? "somewhat wounded" : "somewhat damaged"), max);
		else if (perc >= 25)
			my_strcpy(buf, (living ? "wounded" : "damaged"), max);
		else if (perc >= 10)
			my_strcpy(buf, (living ? "badly wounded" : "badly damaged"), max);
		else
			my_strcpy(buf, (living ? "almost dead" : "almost destroyed"), max);
	}

	if (m_ptr->csleep) my_strcat(buf, ", asleep", max);
	if (m_ptr->confused) my_strcat(buf, ", confused", max);
	if (m_ptr->monfear) my_strcat(buf, ", afraid", max);
	if (m_ptr->stunned) my_strcat(buf, ", stunned", max);
}



/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort_aux(void *u, void *v, int p, int q)
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
		while (!(*ang_sort_comp)(u, v, b, z)) b--;

		/* Slide i1 */
		while (!(*ang_sort_comp)(u, v, z, a)) a++;

		/* Done partition */
		if (a >= b) break;

		/* Swap */
		(*ang_sort_swap)(u, v, a, b);

		/* Advance */
		a++, b--;
	}

	/* Recurse left side */
	ang_sort_aux(u, v, p, b);

	/* Recurse right side */
	ang_sort_aux(u, v, b+1, q);
}


/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort(void *u, void *v, int n)
{
	/* Sort the array */
	ang_sort_aux(u, v, 0, n-1);
}





/*** Targetting Code ***/


/*
 * Given a "source" and "target" location, extract a "direction",
 * which will move one step from the "source" towards the "target".
 *
 * Note that we use "diagonal" motion whenever possible.
 *
 * We return "5" if no motion is needed.
 */
int motion_dir(int y1, int x1, int y2, int x2)
{
	/* No movement required */
	if ((y1 == y2) && (x1 == x2)) return (5);

	/* South or North */
	if (x1 == x2) return ((y1 < y2) ? 2 : 8);

	/* East or West */
	if (y1 == y2) return ((x1 < x2) ? 6 : 4);

	/* South-east or South-west */
	if (y1 < y2) return ((x1 < x2) ? 3 : 1);

	/* North-east or North-west */
	if (y1 > y2) return ((x1 < x2) ? 9 : 7);

	/* Paranoia */
	return (5);
}


/*
 * Extract a direction (or zero) from a character
 */
int target_dir(char ch)
{
	int d = 0;
	int mode;
	const char* act;
	const char* s;


	/* Already a direction? */
	if (isdigit((unsigned char)ch))
	{
		d = D2I(ch);
	}
	else if (isarrow(ch))
	{
		switch (ch)
		{
			case ARROW_DOWN:	d = 2; break;
			case ARROW_LEFT:	d = 4; break;
			case ARROW_RIGHT:	d = 6; break;
			case ARROW_UP:		d = 8; break;
		}
	}
	else
	{
		/* Roguelike/Original */
		mode = (OPTION(rogue_like_commands)) ? KEYMAP_MODE_ROGUE : KEYMAP_MODE_ORIG;

		/* Extract the action (if any) */
		act = keymap_act[mode][(byte)(ch)];

		/* Analyze */
		if (act)
		{
			/* Convert to a direction */
			for (s = act; *s; ++s)
			{
				/* Use any digits in keymap */
				if (isdigit((unsigned char)*s)) d = D2I(*s);
			}
		}
	}

	/* Paranoia */
	if (d == 5) d = 0;

	/* Return direction */
	return (d);
}


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
bool target_able(int m_idx)
{
	monster_type *m_ptr;

	/* No monster */
	if (m_idx <= 0) return (FALSE);

	/* Get monster */
	m_ptr = m_ptr_from_m_idx(m_idx);

	/* Monster must be alive */
	if (!m_ptr->r_idx) return (FALSE);

	/* Monster must be visible */
	if (!m_ptr->ml) return (FALSE);

	/* Monster must be projectable */
	if (!projectable(p_ptr->loc, m_ptr->loc)) return (FALSE);

	/* Hack -- no targeting hallucinations */
	if (p_ptr->timed[TMD_IMAGE]) return (FALSE);

	/* Hack -- Never target trappers XXX XXX XXX */
	/* if (CLEAR_ATTR && (CLEAR_CHAR)) return (FALSE); */

	/* Assume okay */
	return (TRUE);
}




/*
 * Update (if necessary) and verify (if possible) the target.
 *
 * We return TRUE if the target is "okay" and FALSE otherwise.
 */
bool target_okay(void)
{
	/* No target */
	if (!p_ptr->target_set) return (FALSE);

	/* Accept "location" targets */
	if (p_ptr->target_who == 0) return (TRUE);

	/* Check "monster" targets */
	if (p_ptr->target_who > 0)
	{
		const m_idx_type m_idx = p_ptr->target_who;

		/* Accept reasonable targets */
		if (target_able(m_idx))
		{
			/* Get the monster location */
			p_ptr->target = mon_list[m_idx].loc;

			/* Good target */
			return (TRUE);
		}
	}

	/* Assume no target */
	return (FALSE);
}


/*
 * Set the target to a monster (or nobody)
 */
void target_set_monster(int m_idx)
{
	/* Acceptable target */
	if ((m_idx > 0) && target_able(m_idx))
	{
		const monster_type* const m_ptr = m_ptr_from_m_idx(m_idx);

		/* Save target info */
		p_ptr->target_set = TRUE;
		p_ptr->target_who = m_idx;
		p_ptr->target = m_ptr->loc;
	}

	/* Clear target */
	else
	{
		/* Reset target info */
		p_ptr->target_set = FALSE;
		p_ptr->target_who = 0;
		p_ptr->target.clear();
	}
}


/*
 * Set the target to a location
 */
void target_set_location(int y, int x)
{
	/* Legal target */
	if (in_bounds_fully(y, x))
	{
		/* Save target info */
		p_ptr->target_set = TRUE;
		p_ptr->target_who = 0;
		p_ptr->target = coord(x,y);
	}

	/* Clear target */
	else
	{
		/* Reset target info */
		p_ptr->target_set = FALSE;
		p_ptr->target_who = 0;
		p_ptr->target.clear();
	}
}


/*
 * Sorting hook -- comp function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by double-distance to the player.
 */
static bool ang_sort_comp_distance(const void *u, const void *v, int a, int b)
{
	int py = p_ptr->loc.y;
	int px = p_ptr->loc.x;

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
static void ang_sort_swap_distance(void *u, void *v, int a, int b)
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

		/* Penalize location XXX XXX XXX */

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
static bool target_set_interactive_accept(coord g)
{
	object_type *o_ptr;


	/* Player grids are always interesting */
	if (cave_m_idx[g.y][g.x] < 0) return (TRUE);


	/* Handle hallucination */
	if (p_ptr->timed[TMD_IMAGE]) return (FALSE);


	/* Visible monsters */
	if (cave_m_idx[g.y][g.x] > 0)
	{
		/* Visible monsters */
		if (m_ptr_from_m_idx(cave_m_idx[g.y][g.x])->ml) return (TRUE);
	}

	/* Scan all objects in the grid */
	for (o_ptr = get_first_object(g.y, g.x); o_ptr; o_ptr = get_next_object(o_ptr))
	{
		/* Memorized object */
		if (o_ptr->marked) return (TRUE);
	}

	/* Interesting memorized features */
	if (cave_info[g.y][g.x] & (CAVE_MARK))
	{
		/* Notice glyphs */
		if (cave_feat[g.y][g.x] == FEAT_GLYPH) return (TRUE);

		/* Notice doors */
		if (cave_feat[g.y][g.x] == FEAT_OPEN) return (TRUE);
		if (cave_feat[g.y][g.x] == FEAT_BROKEN) return (TRUE);

		/* Notice stairs */
		if (cave_feat[g.y][g.x] == FEAT_LESS) return (TRUE);
		if (cave_feat[g.y][g.x] == FEAT_MORE) return (TRUE);

		/* Notice shops */
		if (cave_feat_in_range(g.y,g.x,FEAT_SHOP_HEAD,FEAT_SHOP_TAIL)) return (TRUE);

		/* Notice traps */
		if (cave_feat_in_range(g.y,g.x,FEAT_TRAP_HEAD,FEAT_TRAP_TAIL)) return (TRUE);

		/* Notice doors */
		if (cave_feat_in_range(g.y,g.x,FEAT_DOOR_HEAD,FEAT_DOOR_TAIL)) return (TRUE);

		/* Notice rubble */
		if (cave_feat[g.y][g.x] == FEAT_RUBBLE) return (TRUE);

		/* Notice veins with treasure */
		if (cave_feat[g.y][g.x] == FEAT_MAGMA_K) return (TRUE);
		if (cave_feat[g.y][g.x] == FEAT_QUARTZ_K) return (TRUE);
	}

	/* Nope */
	return (FALSE);
}

static bool targetable_generic(coord g)
{
	/* Require line of sight, unless "look" is "expanded" */
	if (!OPTION(expand_look) && !player_has_los_bold(g.y, g.x)) return false;

	/* Require "interesting" contents */
	if (!target_set_interactive_accept(g)) return false;

	/* Save the location */
	temp_x[temp_n] = g.x;
	temp_y[temp_n] = g.y;
	temp_n++;

	return false;
}

static bool targetable_monster(coord g)
{
	/* Require line of sight, unless "look" is "expanded" */
	if (!OPTION(expand_look) && !player_has_los_bold(g.y, g.x)) return false;

	/* Require "interesting" contents */
	if (!target_set_interactive_accept(g)) return false;

	/* Special mode */
		/* Must contain a monster */
		if (!(cave_m_idx[g.y][g.x] > 0)) return false;

		/* Must be a targettable monster */
	 	if (!target_able(cave_m_idx[g.y][g.x])) return false;

	/* Save the location */
	temp_x[temp_n] = g.x;
	temp_y[temp_n] = g.y;
	temp_n++;

	return false;
}

/*
 * Prepare the "temp" array for "target_interactive_set"
 *
 * Return the number of target_able monsters in the set.
 */
static void target_set_interactive_prepare(int mode)
{
	/* Reset "temp" array */
	temp_n = 0;

	detect_on_panel((mode & (TARGET_KILL)) ? targetable_monster : targetable_generic);

	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_distance;
	ang_sort_swap = ang_sort_swap_distance;

	/* Sort the positions */
	ang_sort(temp_x, temp_y, temp_n);
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
 * This function correctly handles multiple objects per grid, and objects
 * and terrain features in the same grid, though the latter never happens.
 *
 * This function must handle blindness/hallucination.
 */
static int target_set_interactive_aux(coord g, int mode, const char* const info)
{
	s16b this_o_idx = 0, next_o_idx = 0;

	const char* s1;
	const char* s2;
	const char* s3;

	bool boring;
	bool floored;

	int feat;
	int query;

	char out_val[256];


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


		/* The player */
		if (cave_m_idx[g.y][g.x] < 0)
		{
			/* Description */
			s1 = "You are ";

			/* Preposition */
			s2 = "on ";
		}


		/* Hack -- hallucination */
		if (p_ptr->timed[TMD_IMAGE])
		{
			const char* name = "something strange";

			/* Display a message */
			if (p_ptr->wizard)
			{
				strnfmt(out_val, sizeof(out_val),
				        "%s%s%s%s [%s] (%d:%d)", s1, s2, s3, name, info, g.y, g.x);
			}
			else
			{
				strnfmt(out_val, sizeof(out_val),
				        "%s%s%s%s [%s]", s1, s2, s3, name, info);
			}

			prt(out_val, 0, 0);
			move_cursor_relative(g);
			query = inkey();

			/* Stop on everything but "return" */
			if ((query != '\n') && (query != '\r')) break;

			/* Repeat forever */
			continue;
		}


		/* Actual monsters */
		if (cave_m_idx[g.y][g.x] > 0)
		{
			monster_type* const m_ptr = m_ptr_from_m_idx(cave_m_idx[g.y][g.x]);
			monster_race* const r_ptr = m_ptr->race();

			/* Visible */
			if (m_ptr->ml)
			{
				bool recall = FALSE;

				char m_name[80];

				/* Not boring */
				boring = FALSE;

				/* Get the monster name ("a kobold") */
				monster_desc(m_name, sizeof(m_name), m_ptr, MDESC_IND2);

				/* Hack -- track this monster race */
				monster_race_track(m_ptr->r_idx);

				/* Hack -- health bar for this monster */
				health_track(cave_m_idx[g.y][g.x]);

				/* Hack -- handle stuff */
				handle_stuff();

				/* Interact */
				while (1)
				{
					/* Recall */
					if (recall)
					{
						/* Save screen */
						screen_save();

						/* Recall on screen */
						screen_roff(m_ptr->r_idx);

						/* Hack -- Complete the prompt (again) */
						Term_addstr(-1, TERM_WHITE, format("  [r,%s]", info));

						/* Command */
						query = inkey();

						/* Load screen */
						screen_load();
					}

					/* Normal */
					else
					{
						char buf[80];

						/* Describe the monster */
						look_mon_desc(buf, sizeof(buf), cave_m_idx[g.y][g.x]);

						/* Describe, and prompt for recall */
						if (p_ptr->wizard)
						{
							strnfmt(out_val, sizeof(out_val),
							        "%s%s%s%s (%s) [r,%s] (%d:%d)",
						            s1, s2, s3, m_name, buf, info, g.y, g.x);
						}
						else
						{
							strnfmt(out_val, sizeof(out_val),
							        "%s%s%s%s (%s) [r,%s]",
							        s1, s2, s3, m_name, buf, info);
						}

						prt(out_val, 0, 0);

						/* Place cursor */
						move_cursor_relative(g);

						/* Command */
						query = inkey();
					}

					/* Normal commands */
					if (query != 'r') break;

					/* Toggle recall */
					recall = !recall;
				}

				/* Stop on everything but "return"/"space" */
				if ((query != '\n') && (query != '\r') && (query != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

				/* Change the intro */
				s1 = wd_He_is[race_gender_index(*r_ptr)];

				/* Use a preposition */
				s2 = "carrying ";

				/* Scan all objects being carried */
				for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
				{
					char o_name[80];

					object_type *o_ptr = &o_list[this_o_idx];	/* Get the object */

					/* Get the next object */
					next_o_idx = o_ptr->next_o_idx;

					/* Obtain an object description */
					object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

					/* Describe the object */
					if (p_ptr->wizard)
					{
						strnfmt(out_val, sizeof(out_val),
						        "%s%s%s%s [%s] (%d:%d)",
						        s1, s2, s3, o_name, info, g.y, g.x);
					}
					else
					{
						strnfmt(out_val, sizeof(out_val),
						        "%s%s%s%s [%s]", s1, s2, s3, o_name, info);
					}

					prt(out_val, 0, 0);
					move_cursor_relative(g);
					query = inkey();

					/* Stop on everything but "return"/"space" */
					if ((query != '\n') && (query != '\r') && (query != ' ')) break;

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


		/* Assume not floored */
		floored = FALSE;

		/* Scan all objects in the grid */
		if (OPTION(easy_floor))
		{
			int floor_list[MAX_FLOOR_STACK];
			int floor_num = scan_floor(floor_list, MAX_FLOOR_STACK, g, 0x02);	/* Scan for floor objects */

			/* Actual pile */
			if (floor_num > 1)
			{
				/* Not boring */
				boring = FALSE;

				/* Floored */
				floored = TRUE;

				/* Describe */
				while (1)
				{
					/* Describe the pile */
					if (p_ptr->wizard)
					{
						strnfmt(out_val, sizeof(out_val),
						        "%s%s%sa pile of %d objects [r,%s] (%d:%d)",
						        s1, s2, s3, floor_num, info, g.y, g.x);
					}
					else
					{
						strnfmt(out_val, sizeof(out_val),
						        "%s%s%sa pile of %d objects [r,%s]",
						        s1, s2, s3, floor_num, info);
					}

					prt(out_val, 0, 0);
					move_cursor_relative(g);
					query = inkey();

					/* Display objects */
					if (query == 'r')
					{
						/* Save screen */
						screen_save();

						/* Display */
						show_floor(floor_list, floor_num);

						/* Describe the pile */
						prt(out_val, 0, 0);
						query = inkey();

						/* Load screen */
						screen_load();

						/* Continue on 'r' only */
						if (query == 'r') continue;
					}

					/* Done */
					break;
				}

				/* Stop on everything but "return"/"space" */
				if ((query != '\n') && (query != '\r') && (query != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

				/* Change the intro */
				s1 = "It is ";

				/* Preposition */
				s2 = "on ";
			}
		}

		/* Scan all objects in the grid */
		for (this_o_idx = cave_o_idx[g.y][g.x]; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr = &o_list[this_o_idx];	/* Get the object */

			/* Get the next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Skip objects if floored */
			if (floored) continue;

			/* Describe it */
			if (o_ptr->marked)
			{
				char o_name[80];

				/* Not boring */
				boring = FALSE;

				/* Obtain an object description */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

				/* Describe the object */
				if (p_ptr->wizard)
				{
					strnfmt(out_val, sizeof(out_val),
					        "%s%s%s%s [%s] (%d:%d)",
					        s1, s2, s3, o_name, info, g.y, g.x);
				}
				else
				{
					strnfmt(out_val, sizeof(out_val),
					        "%s%s%s%s [%s]", s1, s2, s3, o_name, info);
				}

				prt(out_val, 0, 0);
				move_cursor_relative(g);
				query = inkey();

				/* Stop on everything but "return"/"space" */
				if ((query != '\n') && (query != '\r') && (query != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

				/* Change the intro */
				s1 = "It is ";

				/* Plurals */
				if (o_ptr->number != 1) s1 = "They are ";

				/* Preposition */
				s2 = "on ";
			}
		}

		/* Double break */
		if (this_o_idx) break;


		/* Feature (apply "mimic") */
		feat = feature_type::f_info[cave_feat[g.y][g.x]].mimic;

		/* Require knowledge about grid, or ability to see grid */
		if (!(cave_info[g.y][g.x] & (CAVE_MARK)) && !player_can_see_bold(g.y,g.x))
		{
			/* Forget feature */
			feat = FEAT_NONE;
		}

		/* Terrain feature if needed */
		if (boring || (feat > FEAT_INVIS))
		{
			const char* name = feature_type::f_info[feat].name();

			/* Hack -- handle unknown grids */
			if (feat == FEAT_NONE) name = "unknown grid";

			/* Pick a prefix */
			if (*s2 && (feat >= FEAT_DOOR_HEAD)) s2 = "in ";

			/* Pick proper indefinite article */
			s3 = (is_a_vowel(name[0])) ? "an " : "a ";

			/* Hack -- special introduction for store doors */
			if ((feat >= FEAT_SHOP_HEAD) && (feat <= FEAT_SHOP_TAIL))
			{
				s3 = "the entrance to the ";
			}

			/* Display a message */
			if (p_ptr->wizard)
			{
				strnfmt(out_val, sizeof(out_val),
				        "%s%s%s%s [%s] (%d:%d)", s1, s2, s3, name, info, g.y, g.x);
			}
			else
			{
				strnfmt(out_val, sizeof(out_val),
				        "%s%s%s%s [%s]", s1, s2, s3, name, info);
			}

			prt(out_val, 0, 0);
			move_cursor_relative(g);
			query = inkey();

			/* Stop on everything but "return"/"space" */
			if ((query != '\n') && (query != '\r') && (query != ' ')) break;
		}

		/* Stop on everything but "return" */
		if ((query != '\n') && (query != '\r')) break;
	}

	/* Keep going */
	return (query);
}




/*
 * Handle "target" and "look".
 *
 * Note that this code can be called from "get_aim_dir()".
 *
 * Currently, when "flag" is true, that is, when
 * "interesting" grids are being used, and a directional key is used, we
 * only scroll by a single panel, in the direction requested, and check
 * for any interesting grids on that panel.  The "correct" solution would
 * actually involve scanning a larger set of grids, including ones in
 * panels which are adjacent to the one currently scanned, but this is
 * overkill for this function.  XXX XXX
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
bool target_set_interactive(int mode)
{
	int i, d, m, t, bd;

	coord tt = p_ptr->loc;

	bool done = FALSE;
	bool flag = TRUE;

	char query;

	char info[80];


	/* Cancel target */
	target_set_monster(0);


	/* Cancel tracking */
	/* health_track(0); */


	/* Prepare the "temp" array */
	target_set_interactive_prepare(mode);

	/* Start near the player */
	m = 0;

	/* Interact */
	while (!done)
	{
		/* Interesting grids */
		if (flag && temp_n)
		{
			tt.y = temp_y[m];
			tt.x = temp_x[m];

			/* Allow target */
			if ((cave_m_idx[tt.y][tt.x] > 0) && target_able(cave_m_idx[tt.y][tt.x]))
			{
				strcpy(info, "q,t,p,o,+,-,<dir>");
			}

			/* Dis-allow target */
			else
			{
				strcpy(info, "q,p,o,+,-,<dir>");
			}

			/* Adjust panel if needed */
			if (adjust_panel(tt))
			{
				/* Handle stuff */
				handle_stuff();
			}

			/* Describe and Prompt */
			query = target_set_interactive_aux(tt, mode, info);

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

				case ' ':
				case '*':
				case '+':
				{
					m = (m+1)%temp_n;
					break;
				}

				case '-':
				{
					m = (m+temp_n-1)%temp_n;
					break;
				}

				case 'p':
				{
					/* Recenter around player */
					event_signal(EVENT_PLAYERMOVED);

					/* Handle stuff */
					handle_stuff();

					tt = p_ptr->loc;
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

				case 't':
				case '5':
				case '0':
				case '.':
				{
					int m_idx = cave_m_idx[tt.y][tt.x];

					if ((m_idx > 0) && target_able(m_idx))
					{
						health_track(m_idx);
						target_set_monster(m_idx);
						done = TRUE;
					}
					else
					{
						bell("Illegal target!");
					}
					break;
				}

				default:
				{
					/* Extract direction */
					d = target_dir(query);

					/* Oops */
					if (!d) bell("Illegal command for target mode!");

					break;
				}
			}

			/* Hack -- move around */
			if (d)
			{
				int old_y = temp_y[m];
				int old_x = temp_x[m];

				/* Find a new monster */
				i = target_pick(old_y, old_x, ddy[d], ddx[d]);

				/* Scroll to find interesting grid */
				if (i < 0)
				{
					int old_wy = Term->offset_y;
					int old_wx = Term->offset_x;

					/* Change if legal */
					if (change_panel(d))
					{
						/* Recalculate interesting grids */
						target_set_interactive_prepare(mode);

						/* Find a new monster */
						i = target_pick(old_y, old_x, ddy[d], ddx[d]);

						/* Restore panel if needed */
						if ((i < 0) && modify_panel(Term, old_wy, old_wx))
						{
							/* Recalculate interesting grids */
							target_set_interactive_prepare(mode);
						}

						/* Handle stuff */
						handle_stuff();
					}
				}

				/* Use interesting grid if found */
				if (i >= 0) m = i;
			}
		}

		/* Arbitrary grids */
		else
		{
			/* Default prompt */
			strcpy(info, "q,t,p,m,+,-,<dir>");

			/* Describe and Prompt (enable "TARGET_LOOK") */
			query = target_set_interactive_aux(tt, mode | TARGET_LOOK, info);

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

				case ' ':
				case '*':
				case '+':
				case '-':
				{
					break;
				}

				case 'p':
				{
					/* Recenter around player */
					event_signal(EVENT_PLAYERMOVED);

					/* Handle stuff */
					handle_stuff();

					tt = p_ptr->loc;
				}

				case 'o':
				{
					break;
				}

				case 'm':
				{
					flag = TRUE;

					m = 0;
					bd = 999;

					/* Pick a nearby monster */
					for (i = 0; i < temp_n; i++)
					{
						t = distance(tt.y, tt.x, temp_y[i], temp_x[i]);

						/* Pick closest */
						if (t < bd)
						{
							m = i;
							bd = t;
						}
					}

					/* Nothing interesting */
					if (bd == 999) flag = FALSE;

					break;
				}

				case 't':
				case '5':
				case '0':
				case '.':
				{
					target_set_location(tt.y, tt.x);
					done = TRUE;
					break;
				}

				default:
				{
					/* Extract a direction */
					d = target_dir(query);

					/* Oops */
					if (!d) bell("Illegal command for target mode!");

					break;
				}
			}

			/* Handle "direction" */
			if (d)
			{
				int dungeon_hgt = (p_ptr->depth == 0) ? TOWN_HGT : DUNGEON_HGT;
				int dungeon_wid = (p_ptr->depth == 0) ? TOWN_WID : DUNGEON_WID;

				/* Move */
				tt += dd_coord[d];

				/* Slide into legality */
				if (tt.x >= dungeon_wid - 1) tt.x--;
				else if (tt.x <= 0) tt.x++;

				/* Slide into legality */
				if (tt.y >= dungeon_hgt - 1) tt.y--;
				else if (tt.y <= 0) tt.y++;

				/* Adjust panel if needed */
				if (adjust_panel(tt))
				{
					/* Handle stuff */
					handle_stuff();

					/* Recalculate interesting grids */
					target_set_interactive_prepare(mode);
				}
			}
		}
	}

	/* Forget */
	temp_n = 0;

	/* Clear the top line */
	prt("", 0, 0);

	/* Recenter around player */
	event_signal(EVENT_PLAYERMOVED);

	/* Handle stuff */
	handle_stuff();

	/* Failure to set target */
	if (!p_ptr->target_set) return (FALSE);

	/* Success */
	return (TRUE);
}



/*
 * Get an "aiming direction" (1,2,3,4,6,7,8,9 or 5) from the user.
 *
 * Return TRUE if a direction was chosen, otherwise return FALSE.
 *
 * The direction "5" is special, and means "use current target".
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", if it is set.
 *
 * Note that "Force Target", if set, will pre-empt user interaction,
 * if there is a usable target already set.
 *
 * Currently this function applies confusion directly.
 */
bool get_aim_dir(int *dp)
{
	int dir;
	char ch;
	const char* p;

	if (repeat_pull(dp))
	{
		/* Verify */
		if (!(*dp == 5 && !target_okay()))
		{
			return (TRUE);
		}
		else
		{
			/* Invalid repeat - reset it */
			repeat_clear();
		}
	}

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = p_ptr->command_dir;

	/* Hack -- auto-target if requested */
	if (OPTION(use_old_target) && target_okay()) dir = 5;

	/* Ask until satisfied */
	while (!dir)
	{
		/* Choose a prompt */
		if (!target_okay())
		{
			p = "Direction ('*' to choose a target, Escape to cancel)? ";
		}
		else
		{
			p = "Direction ('5' for target, '*' to re-target, Escape to cancel)? ";
		}

		/* Get a command (or Cancel) */
		if (!get_com(p, &ch)) break;

		/* Analyze */
		switch (ch)
		{
			/* Set new target, use target if legal */
			case '*':
			{
				if (target_set_interactive(TARGET_KILL)) dir = 5;
				break;
			}

			/* Use current target, if set and legal */
			case 't':
			case '5':
			case '0':
			case '.':
			{
				if (target_okay()) dir = 5;
				break;
			}

			/* Possible direction */
			default:
			{
				dir = target_dir(ch);
				break;
			}
		}

		/* Error */
		if (!dir) bell("Illegal aim direction!");
	}

	/* No direction */
	if (!dir) return (FALSE);

	/* Save the direction */
	p_ptr->command_dir = dir;

	/* Check for confusion */
	if (p_ptr->timed[TMD_CONFUSED])
	{
		/* Random direction */
		dir = ddd[rand_int(KEYPAD_DIR_MAX)];
	}

	/* Notice confusion */
	if (p_ptr->command_dir != dir)
	{
		/* Warn the user */
		msg_print("You are confused.");
	}

	/* Save direction */
	(*dp) = dir;

	repeat_push(dir);

	/* A "valid" direction was entered */
	return (TRUE);
}



/*
 * Request a "movement" direction (1,2,3,4,6,7,8,9) from the user.
 *
 * Return TRUE if a direction was chosen, otherwise return FALSE.
 *
 * This function should be used for all "repeatable" commands, such as
 * run, walk, open, close, bash, disarm, spike, tunnel, etc, as well
 * as all commands which must reference a grid adjacent to the player,
 * and which may not reference the grid under the player.
 *
 * Directions "5" and "0" are illegal and will not be accepted.
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", if it is set.
 */
bool get_rep_dir(int *dp)
{
	int dir;
	char ch;
	const char* p;

	if (repeat_pull(dp)) return TRUE;

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = p_ptr->command_dir;

	/* Get a direction */
	while (!dir)
	{
		/* Choose a prompt */
		p = "Direction (Escape to cancel)? ";

		/* Get a command (or Cancel) */
		if (!get_com(p, &ch)) break;

		/* Convert keypress into a direction */
		dir = target_dir(ch);

		/* Oops */
		if (!dir) bell("Illegal repeatable direction!");
	}

	/* Aborted */
	if (!dir) return (FALSE);

	/* Save desired direction */
	p_ptr->command_dir = dir;

	/* Save direction */
	(*dp) = dir;

	repeat_push(dir);

	/* Success */
	return (TRUE);
}


/*
 * Apply confusion, if needed, to a direction
 *
 * Display a message and return TRUE if direction changes.
 */
bool confuse_dir(int *dp)
{
	int dir;

	/* Default */
	dir = (*dp);

	/* Apply "confusion" */
	if (p_ptr->timed[TMD_CONFUSED])
	{
		/* Apply confusion XXX XXX XXX */
		if ((dir == 5) || !one_in_(4))
		{
			/* Random direction */
			dir = ddd[rand_int(KEYPAD_DIR_MAX)];
		}
	}

	/* Notice confusion */
	if ((*dp) != dir)
	{
		/* Warn the user */
		msg_print("You are confused.");

		/* Save direction */
		(*dp) = dir;

		/* Confused */
		return (TRUE);
	}

	/* Not confused */
	return (FALSE);
}

