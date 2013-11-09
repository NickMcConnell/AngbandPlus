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

void set_action(int typ)
{
	int prev_typ = p_ptr->action;

	if (typ == prev_typ)
	{
		return;
	}
	else
	{
		switch (prev_typ)
		{
			case ACTION_SEARCH:
			{
#ifdef JP
				msg_print("探索をやめた。");
#else
				msg_print("You no longer walk carefully.");
#endif
				p_ptr->redraw |= (PR_SPEED);
				break;
			}
			case ACTION_REST:
			{
				resting = 0;
				break;
			}
			case ACTION_SING:
			{
#ifdef JP
				msg_print("歌うのをやめた。");
#else
				msg_print("You stop singing.");
#endif
				break;
			}
			case ACTION_STEALTH:
			{
#ifdef JP
				msg_print("足音が大きくなった。");
#else
				msg_print("You no longer walk silently.");
#endif
				p_ptr->redraw |= (PR_SPEED);
				break;
			}
			case ACTION_ELEMSCOPE:
			{
#ifdef JP
				msg_print("エレメントに精神を尖らせるのをやめた。");
#else
				msg_print("You stopped feeling elements.");
#endif
				p_ptr->redraw |= (PR_MAP);
				break;
			}
		}
	}

	p_ptr->action = typ;

	/* If we are requested other action, stop singing */
	if (prev_typ == ACTION_SING) stop_singing();

	switch (p_ptr->action)
	{
		case ACTION_SEARCH:
		{
#ifdef JP
			msg_print("注意深く歩き始めた。");
#else
			msg_print("You begin to walk carefully.");
#endif
			p_ptr->redraw |= (PR_SPEED);
			break;
		}
		case ACTION_ELEMSCOPE:
		{
#ifdef JP
			msg_print("周囲の空間のエレメントを繊細に感じ始めた。");
#else
			msg_print("You begin to feel elements.");
#endif
			p_ptr->redraw |= (PR_MAP);
			break;
		}
		case ACTION_STEALTH:
		{
#ifdef JP
			msg_print("足音が小さくなった。");
#else
			msg_print("You begin to walk silently.");
#endif
			p_ptr->redraw |= (PR_SPEED);
			break;
		}
		default:
		{
			break;
		}
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);
}

/* reset timed flags */
void reset_tim_flags(void)
{
	p_ptr->fast = 0;            /* Timed -- Fast */
	p_ptr->slow = 0;            /* Timed -- Slow */
	p_ptr->blind = 0;           /* Timed -- Blindness */
	p_ptr->paralyzed = 0;       /* Timed -- Paralysis */
	p_ptr->confused = 0;        /* Timed -- Confusion */
	p_ptr->afraid = 0;          /* Timed -- Fear */
	p_ptr->image = 0;           /* Timed -- Hallucination */
	p_ptr->poisoned = 0;        /* Timed -- Poisoned */
	p_ptr->cut = 0;             /* Timed -- Cut */
	p_ptr->stun = 0;            /* Timed -- Stun */
	p_ptr->stoning = 0;         /* Timed -- Stoning */
	p_ptr->opposite_pelem = 0;  /* Timed -- Forced to opposite element */

	p_ptr->protevil = 0;        /* Timed -- Protection */
	p_ptr->invuln = 0;          /* Timed -- Invulnerable */
	p_ptr->hero = 0;            /* Timed -- Heroism */
	p_ptr->shero = 0;           /* Timed -- Super Heroism */
	p_ptr->shield = 0;          /* Timed -- Shield Spell */
	p_ptr->blessed = 0;         /* Timed -- Blessed */
	p_ptr->tim_invis = 0;       /* Timed -- Invisibility */
	p_ptr->tim_infra = 0;       /* Timed -- Infra Vision */
	p_ptr->tim_esp = 0;
	p_ptr->tim_sh_fire = 0;
	p_ptr->tim_sh_elec = 0;
	p_ptr->tim_sh_cold = 0;
	p_ptr->tim_sh_holy = 0;
	p_ptr->tim_eyeeye = 0;
	p_ptr->tim_res_time = 0;
	p_ptr->tim_inc_blow = 0;
	p_ptr->tim_dec_blow = 0;
	p_ptr->zoshonel_protect = 0;
	p_ptr->multishadow = 0;
	p_ptr->dustrobe = 0;
	p_ptr->action = ACTION_NONE;

	p_ptr->chargespell = 0;     /* Timed -- Charge spell */
	p_ptr->earth_spike = 0;     /* Timed -- Immune to teleport by others */
	p_ptr->wind_guard = 0;      /* Timed -- Avoidance to arrows */
	p_ptr->tim_resurrection = 0; /* Timed -- Avoidance to death (!!) */

	p_ptr->oppose_acid = 0;     /* Timed -- oppose acid */
	p_ptr->oppose_elec = 0;     /* Timed -- oppose lightning */
	p_ptr->oppose_fire = 0;     /* Timed -- oppose heat */
	p_ptr->oppose_cold = 0;     /* Timed -- oppose cold */
	p_ptr->oppose_pois = 0;     /* Timed -- oppose poison */

	p_ptr->word_recall = 0;
	p_ptr->alter_reality = 0;
	p_ptr->magical_weapon = 0;
	p_ptr->evil_weapon = 0;
	p_ptr->special_attack = 0L;

	p_ptr->wraith_form = 0;

	if (mw_old_weight)
	{
		object_type *o_ptr = &inventory[INVEN_RARM];
		o_ptr->weight = mw_old_weight;
		p_ptr->total_weight += (o_ptr->weight - 1) * o_ptr->number;
		mw_old_weight = 0;
	}
	if (mw_diff_to_melee)
	{
		object_type *o_ptr = &inventory[INVEN_RARM];
		o_ptr->to_h -= mw_diff_to_melee;
		o_ptr->to_d -= mw_diff_to_melee;
		mw_diff_to_melee = 0;
	}

	while (p_ptr->energy_need < 0) p_ptr->energy_need += ENERGY_NEED();
	stop_the_time_player = FALSE;

	if ((inventory[INVEN_RARM].k_idx && (inventory[INVEN_RARM].name1 == ART_BERSERK)) ||
	    (inventory[INVEN_LARM].k_idx && (inventory[INVEN_LARM].name1 == ART_BERSERK))) p_ptr->shero = 1;

	if (p_ptr->riding)
	{
		m_list[p_ptr->riding].fast = 0;
		m_list[p_ptr->riding].slow = 0;
		m_list[p_ptr->riding].invulner = 0;
	}

	p_ptr->singing = 0;
	p_ptr->song_start = 0;
}


/*
 * Set "p_ptr->blind", notice observable changes
 *
 * Note the use of "PU_UN_LITE" and "PU_UN_VIEW", which is needed to
 * memorize any terrain features which suddenly become "visible".
 * Note that blindness is currently the only thing which can affect
 * "player_can_see_bold()".
 */
bool set_blind(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (!p_ptr->blind)
		{
#ifdef JP
msg_print("目が見えなくなってしまった！");
#else
			msg_print("You are blind!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->blind)
		{
#ifdef JP
msg_print("やっと目が見えるようになった。");
#else
			msg_print("You can see again.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->blind = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Fully update the visuals */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE | PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->confused", notice observable changes
 */
bool set_confused(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (!p_ptr->confused)
		{
#ifdef JP
msg_print("あなたは混乱した！");
#else
			msg_print("You are confused!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->confused)
		{
#ifdef JP
msg_print("やっと混乱がおさまった。");
#else
			msg_print("You feel less confused now.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->confused = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->poisoned", notice observable changes
 */
bool set_poisoned(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (!p_ptr->poisoned)
		{
#ifdef JP
msg_print("毒に侵されてしまった！");
#else
			msg_print("You are poisoned!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->poisoned)
		{
#ifdef JP
msg_print("やっと毒の痛みがなくなった。");
#else
			msg_print("You are no longer poisoned.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->poisoned = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->afraid", notice observable changes
 */
bool set_afraid(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (!p_ptr->afraid)
		{
#ifdef JP
msg_print("何もかも恐くなってきた！");
#else
			msg_print("You are terrified!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->afraid)
		{
#ifdef JP
msg_print("やっと恐怖を振り払った。");
#else
			msg_print("You feel bolder now.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->afraid = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->paralyzed", notice observable changes
 */
bool set_paralyzed(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (!p_ptr->paralyzed)
		{
#ifdef JP
msg_print("体が麻痺してしまった！");
#else
			msg_print("You are paralyzed!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->paralyzed)
		{
#ifdef JP
msg_print("やっと動けるようになった。");
#else
			msg_print("You can move again.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->paralyzed = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->image", notice observable changes
 *
 * Note that we must redraw the map when hallucination changes.
 */
bool set_image(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;


	/* Open */
	if (v)
	{
		if (!p_ptr->image)
		{
#ifdef JP
msg_print("ワーオ！何もかも虹色に見える！");
#else
			msg_print("Oh, wow! Everything looks so cosmic now!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->image)
		{
#ifdef JP
msg_print("やっとはっきりと物が見えるようになった。");
#else
			msg_print("You can see clearly again.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->image = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->stoning", notice observable changes
 */
bool set_stoning(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->stoning)
		{
#ifdef JP
			msg_print("体が石化し始めた！");
#else
			msg_print("Your body gets stoning!");
#endif

			notice = TRUE;
		}
		else if (p_ptr->stoning >= 250)
		{
#ifdef JP
			msg_print("体が完全に石化してしまった...");
#else
			msg_print("Your body was stoned completely...");
#endif
			p_ptr->is_dead |= DEATH_STONED;
			take_hit(DAMAGE_LOSELIFE, 0, "徐々に進行する石化");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->stoning)
		{
#ifdef JP
			msg_print("体の石化が止まった。");
#else
			msg_print("Your body is no longer stoning.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->stoning = v;

	/* Redraw status bar*/
	p_ptr->redraw |= (PR_STATUS);
	p_ptr->redraw |= (PR_ARMOR | PR_SPEED);
	p_ptr->update |= (PU_BONUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->opposite_pelem", notice observable changes
 */
bool set_opposite_pelem(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->opposite_pelem)
		{
#ifdef JP
			msg_print("エレメントが反転した。");
#else
			msg_print("Your elements are reverted.");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->opposite_pelem)
		{
#ifdef JP
			msg_print("エレメントの反転が消えた。");
#else
			msg_print("Your elements are no longer reverted.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->opposite_pelem = v;

	/* Redraw status bar*/
	p_ptr->redraw |= (PR_STATUS);
	p_ptr->update |= (PU_BONUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	init_realm_table();

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	if (p_ptr->action == ACTION_ELEMSCOPE) lite_spot(py, px);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->fast", notice observable changes
 */
bool set_fast(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->fast && !do_dec)
		{
			if (p_ptr->fast > v) return FALSE;
		}
		else if (!p_ptr->fast)
		{
#ifdef JP
msg_print("素早く動けるようになった！");
#else
			msg_print("You feel yourself moving much faster!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->fast)
		{
#ifdef JP
msg_print("動きの素早さがなくなったようだ。");
#else
			msg_print("You feel yourself slow down.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->fast = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->slow", notice observable changes
 */
bool set_slow(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->slow && !do_dec)
		{
			if (p_ptr->slow > v) return FALSE;
		}
		else if (!p_ptr->slow)
		{
#ifdef JP
msg_print("体の動きが遅くなってしまった！");
#else
			msg_print("You feel yourself moving slower!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->slow)
		{
#ifdef JP
msg_print("動きの遅さがなくなったようだ。");
#else
			msg_print("You feel yourself speed up.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->slow = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->shield", notice observable changes
 */
bool set_shield(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->shield && !do_dec)
		{
			if (p_ptr->shield > v) return FALSE;
		}
		else if (!p_ptr->shield)
		{
#ifdef JP
msg_print("肌が石になった。");
#else
			msg_print("Your skin turns to stone.");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->shield)
		{
#ifdef JP
msg_print("肌が元に戻った。");
#else
			msg_print("Your skin returns to normal.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->shield = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}



/*
 * Set "p_ptr->magicdef", notice observable changes
 */
bool set_magicdef(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->magicdef && !do_dec)
		{
			if (p_ptr->magicdef > v) return FALSE;
		}
		else if (!p_ptr->magicdef)
		{
#ifdef JP
			msg_print("魔法の防御力が増したような気がする。");
#else
			msg_print("You feel more resistant to magic.");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->magicdef)
		{
#ifdef JP
			msg_print("魔法の防御力が元に戻った。");
#else
			msg_print("You feel less resistant to magic.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->magicdef = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}



/*
 * Set "p_ptr->blessed", notice observable changes
 */
bool set_blessed(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->blessed && !do_dec)
		{
			if (p_ptr->blessed > v) return FALSE;
		}
		else if (!p_ptr->blessed)
		{
#ifdef JP
msg_print("高潔な気分になった！");
#else
			msg_print("You feel righteous!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->blessed)
		{
#ifdef JP
msg_print("高潔な気分が消え失せた。");
#else
			msg_print("The prayer has expired.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->blessed = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->hero", notice observable changes
 */
bool set_hero(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->hero && !do_dec)
		{
			if (p_ptr->hero > v) return FALSE;
		}
		else if (!p_ptr->hero)
		{
#ifdef JP
msg_print("ヒーローになった気がする！");
#else
			msg_print("You feel like a hero!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->hero)
		{
#ifdef JP
msg_print("ヒーローの気分が消え失せた。");
#else
			msg_print("The heroism wears off.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->hero = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate hitpoints */
	p_ptr->update |= (PU_HP);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->shero", notice observable changes
 */
bool set_shero(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	if ((inventory[INVEN_RARM].k_idx && (inventory[INVEN_RARM].name1 == ART_BERSERK)) ||
	    (inventory[INVEN_LARM].k_idx && (inventory[INVEN_LARM].name1 == ART_BERSERK))) v = 1;

	/* Open */
	if (v)
	{
		if (p_ptr->shero && !do_dec)
		{
			if (p_ptr->shero > v) return FALSE;
		}
		else if (!p_ptr->shero)
		{
#ifdef JP
msg_print("殺戮マシーンになった気がする！");
#else
			msg_print("You feel like a killing machine!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->shero)
		{
#ifdef JP
msg_print("野蛮な気持ちが消え失せた。");
#else
			msg_print("You feel less Berserk.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->shero = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate hitpoints */
	p_ptr->update |= (PU_HP);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->protevil", notice observable changes
 */
bool set_protevil(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->protevil && !do_dec)
		{
			if (p_ptr->protevil > v) return FALSE;
		}
		else if (!p_ptr->protevil)
		{
#ifdef JP
msg_print("邪悪なる存在から守られているような感じがする！");
#else
			msg_print("You feel safe from evil!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->protevil)
		{
#ifdef JP
msg_print("邪悪なる存在から守られている感じがなくなった。");
#else
			msg_print("You no longer feel safe from evil.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->protevil = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->wraith_form", notice observable changes
 */
bool set_wraith_form(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->wraith_form && !do_dec)
		{
			if (p_ptr->wraith_form > v) return FALSE;
		}
		else if (!WRAITH_FORM())
		{
#ifdef JP
			msg_print("物質界を離れて幽鬼のような存在になった！");
#else
			msg_print("You leave the physical world and turn into a wraith-being!");
#endif

			notice = TRUE;

			change_your_alignment_lnc(-3);

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Update monsters */
			p_ptr->update |= (PU_MONSTERS);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->wraith_form && !p_ptr->wraith_form_perm)
		{
#ifdef JP
			msg_print("不透明になった感じがする。");
#else
			msg_print("You feel opaque.");
#endif

			notice = TRUE;

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Update monsters */
			p_ptr->update |= (PU_MONSTERS);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
		}
	}

	/* Use the value */
	p_ptr->wraith_form = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);




	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);

}


/*
 * Set "p_ptr->invuln", notice observable changes
 */
bool set_invuln(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->invuln && !do_dec)
		{
			if (p_ptr->invuln > v) return FALSE;
		}
		else if (!p_ptr->invuln)
		{
#ifdef JP
msg_print("無敵だ！");
#else
			msg_print("Invulnerability!");
#endif

			notice = TRUE;

			change_your_alignment_lnc(3);

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Update monsters */
			p_ptr->update |= (PU_MONSTERS);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->invuln)
		{
#ifdef JP
msg_print("無敵ではなくなった。");
#else
			msg_print("The invulnerability wears off.");
#endif

			notice = TRUE;

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Update monsters */
			p_ptr->update |= (PU_MONSTERS);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

			p_ptr->energy_need += ENERGY_NEED();
		}
	}

	/* Use the value */
	p_ptr->invuln = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->tim_esp", notice observable changes
 */
bool set_tim_esp(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_esp && !do_dec)
		{
			if (p_ptr->tim_esp > v) return FALSE;
		}
		else if (!p_ptr->tim_esp)
		{
#ifdef JP
msg_print("意識が広がった気がする！");
#else
			msg_print("You feel your consciousness expand!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_esp)
		{
#ifdef JP
msg_print("意識は元に戻った。");
#else
			msg_print("Your consciousness contracts again.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_esp = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->tim_invis", notice observable changes
 */
bool set_tim_invis(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_invis && !do_dec)
		{
			if (p_ptr->tim_invis > v) return FALSE;
		}
		else if (!p_ptr->tim_invis)
		{
#ifdef JP
msg_print("目が非常に敏感になった気がする！");
#else
			msg_print("Your eyes feel very sensitive!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_invis)
		{
#ifdef JP
msg_print("目の敏感さがなくなったようだ。");
#else
			msg_print("Your eyes feel less sensitive.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_invis = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->tim_infra", notice observable changes
 */
bool set_tim_infra(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_infra && !do_dec)
		{
			if (p_ptr->tim_infra > v) return FALSE;
		}
		else if (!p_ptr->tim_infra)
		{
#ifdef JP
msg_print("目がランランと輝き始めた！");
#else
			msg_print("Your eyes begin to tingle!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_infra)
		{
#ifdef JP
msg_print("目の輝きがなくなった。");
#else
			msg_print("Your eyes stop tingling.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_infra = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->chargespell", notice observable changes
 */
bool set_chargespell(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (p_ptr->chargespell && !do_dec)
		{
			if (p_ptr->chargespell > v) return FALSE;
		}
		else if (!p_ptr->chargespell)
		{
#ifdef JP
msg_print("全身に魔力がみなぎり、頭脳がクリアになった！");
#else
			msg_print("You are filled with mana, and your brain became clear.");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->chargespell)
		{
#ifdef JP
msg_print("魔力の充実が失われたようだ。");
#else
			msg_print("You are no longer filled with mana.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->chargespell = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate hitpoints */
	p_ptr->update |= (PU_MANA);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


void set_mermaid_in_water(void)
{
	bool notice = FALSE;

	if (p_ptr->is_dead) return;

	/* Open */
	if (IS_MERMAID_IN_WATER())
	{
		if (!p_ptr->mermaid_in_water) notice = TRUE;
	}

	/* Shut */
	else
	{
		if (p_ptr->mermaid_in_water) notice = TRUE;
	}

	/* Nothing to notice */
	if (!notice) return;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Disturb */
	/* if (disturb_state) disturb(0, 0); */
}


/*
 * Set "p_ptr->tim_sh_fire", notice observable changes
 */
bool set_tim_sh_fire(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_sh_fire && !do_dec)
		{
			if (p_ptr->tim_sh_fire > v) return FALSE;
		}
		else if (!p_ptr->tim_sh_fire)
		{
#ifdef JP
msg_print("体が炎のオーラで覆われた。");
#else
			msg_print("You have enveloped by fiery aura!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_sh_fire)
		{
#ifdef JP
msg_print("炎のオーラが消えた。");
#else
			msg_print("Fiery aura disappeared.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_sh_fire = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->tim_sh_elec", notice observable changes
 */
bool set_tim_sh_elec(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_sh_elec && !do_dec)
		{
			if (p_ptr->tim_sh_elec > v) return FALSE;
		}
		else if (!p_ptr->tim_sh_elec)
		{
#ifdef JP
msg_print("体が電気のオーラで覆われた。");
#else
			msg_print("You have enveloped by electrical aura!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_sh_elec)
		{
#ifdef JP
msg_print("電気のオーラが消えた。");
#else
			msg_print("Electrical aura disappeared.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_sh_elec = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->tim_sh_cold", notice observable changes
 */
bool set_tim_sh_cold(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_sh_cold && !do_dec)
		{
			if (p_ptr->tim_sh_cold > v) return FALSE;
		}
		else if (!p_ptr->tim_sh_cold)
		{
#ifdef JP
msg_print("体が冷気のオーラで覆われた。");
#else
			msg_print("You have enveloped by aura of coldness!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_sh_cold)
		{
#ifdef JP
msg_print("冷気のオーラが消えた。");
#else
			msg_print("Aura of coldness disappeared.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_sh_cold = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->tim_sh_holy", notice observable changes
 */
bool set_tim_sh_holy(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_sh_holy && !do_dec)
		{
			if (p_ptr->tim_sh_holy > v) return FALSE;
		}
		else if (!p_ptr->tim_sh_holy)
		{
#ifdef JP
msg_print("体が聖なるオーラで覆われた。");
#else
			msg_print("You have enveloped by holy aura!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_sh_holy)
		{
#ifdef JP
msg_print("聖なるオーラが消えた。");
#else
			msg_print("Holy aura disappeared.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_sh_holy = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->tim_eyeeye", notice observable changes
 */
bool set_tim_eyeeye(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_eyeeye && !do_dec)
		{
			if (p_ptr->tim_eyeeye > v) return FALSE;
		}
		else if (!p_ptr->tim_eyeeye)
		{
#ifdef JP
msg_print("法の守り手になった気がした！");
#else
			msg_print("You feel like a keeper of commandments!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_eyeeye)
		{
#ifdef JP
msg_print("懲罰を執行することができなくなった。");
#else
			msg_print("You no longer feel like a keeper.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_eyeeye = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->tim_inc_blow", notice observable changes
 */
bool set_tim_inc_blow(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_inc_blow && !do_dec)
		{
			if (p_ptr->tim_inc_blow > v) return FALSE;
		}
		else if (!p_ptr->tim_inc_blow)
		{
#ifdef JP
			msg_print("攻撃回数が上がった！");
#else
			msg_print("Number of blows is increased!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_inc_blow)
		{
#ifdef JP
			msg_print("元の攻撃回数に戻った。");
#else
			msg_print("Number of blows is no more increased.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_inc_blow = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->tim_dec_blow", notice observable changes
 */
bool set_tim_dec_blow(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_dec_blow && !do_dec)
		{
			if (p_ptr->tim_dec_blow > v) return FALSE;
		}
		else if (!p_ptr->tim_dec_blow)
		{
#ifdef JP
			msg_print("攻撃回数が下がった！");
#else
			msg_print("Number of blows is decreased!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_dec_blow)
		{
#ifdef JP
			msg_print("元の攻撃回数に戻った。");
#else
			msg_print("Number of blows is no more decreased.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_dec_blow = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->zoshonel_protect", notice observable changes
 */
bool set_zoshonel_protect(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->zoshonel_protect && !do_dec)
		{
			if (p_ptr->zoshonel_protect > v) return FALSE;
		}
		else if (!p_ptr->zoshonel_protect)
		{
#ifdef JP
			msg_print("炎の神ゾショネルの加護を得た。");
#else
			msg_print("You are protected by Zoshonel the God of Fire.");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->zoshonel_protect)
		{
#ifdef JP
			msg_print("炎の神ゾショネルの加護がなくなった。");
#else
			msg_print("You are no longer protected by Zoshonel the God of Fire.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->zoshonel_protect = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->multishadow", notice observable changes
 */
bool set_multishadow(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->multishadow && !do_dec)
		{
			if (p_ptr->multishadow > v) return FALSE;
		}
		else if (!p_ptr->multishadow)
		{
#ifdef JP
msg_print("あなたの周りに幻影が生まれた。");
#else
			msg_print("Your Shadow enveloped you.");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->multishadow)
		{
#ifdef JP
msg_print("幻影が消えた。");
#else
			msg_print("Your Shadow disappears.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->multishadow = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->dustrobe", notice observable changes
 */
bool set_dustrobe(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->dustrobe && !do_dec)
		{
			if (p_ptr->dustrobe > v) return FALSE;
		}
		else if (!p_ptr->dustrobe)
		{
#ifdef JP
msg_print("体が破片のオーラで覆われた。");
#else
			msg_print("You were enveloped by shards.");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->dustrobe)
		{
#ifdef JP
msg_print("破片のオーラが消えた。");
#else
			msg_print("The shards disappear.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->dustrobe = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set a temporary elemental brand.  Clear all other brands.  Print status 
 * messages. -LM-
 */
bool set_magical_weapon(u32b attack_type, int v, int item, bool item_disappear)
{
	object_type *o_ptr;

	if (!item_disappear)
	{
		/* Access the item (if in the pack) */
		if (item >= 0)
		{
			o_ptr = &inventory[item];
		}
		else
		{
			o_ptr = &o_list[0 - item];
		}
	}
	else
	{
		o_ptr = NULL;
	}

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Clear all elemental attacks (only one is allowed at a time). */
	if ((p_ptr->special_attack & (ATTACK_ACID)) && (attack_type != ATTACK_ACID))
	{
		p_ptr->special_attack &= ~(ATTACK_ACID);
#ifdef JP
		msg_print("酸で攻撃できなくなった。");
#else
		msg_print("Your temporary acidic brand fades away.");
#endif
	}
	if ((p_ptr->special_attack & (ATTACK_ELEC)) && (attack_type != ATTACK_ELEC))
	{
		p_ptr->special_attack &= ~(ATTACK_ELEC);
#ifdef JP
		msg_print("電撃で攻撃できなくなった。");
#else
		msg_print("Your temporary electrical brand fades away.");
#endif
	}
	if ((p_ptr->special_attack & (ATTACK_FIRE)) && (attack_type != ATTACK_FIRE))
	{
		p_ptr->special_attack &= ~(ATTACK_FIRE);
#ifdef JP
		msg_print("火炎で攻撃できなくなった。");
#else
		msg_print("Your temporary fiery brand fades away.");
#endif
	}
	if ((p_ptr->special_attack & (ATTACK_COLD)) && (attack_type != ATTACK_COLD))
	{
		p_ptr->special_attack &= ~(ATTACK_COLD);
#ifdef JP
		msg_print("冷気で攻撃できなくなった。");
#else
		msg_print("Your temporary frost brand fades away.");
#endif
	}
	if ((p_ptr->special_attack & (ATTACK_POIS)) && (attack_type != ATTACK_POIS))
	{
		p_ptr->special_attack &= ~(ATTACK_POIS);
#ifdef JP
		msg_print("毒で攻撃できなくなった。");
#else
		msg_print("Your temporary poison brand fades away.");
#endif
	}

	if ((v) && (attack_type))
	{
		/* Set attack type. */
		p_ptr->special_attack |= (attack_type);

		/* Set duration. */
		p_ptr->magical_weapon = v;

		/* Message. */
#ifdef JP
		msg_format("%sで攻撃できるようになった！",
			     ((attack_type == ATTACK_ACID) ? "酸" :
			      ((attack_type == ATTACK_ELEC) ? "電撃" :
			       ((attack_type == ATTACK_FIRE) ? "火炎" : 
			        ((attack_type == ATTACK_COLD) ? "冷気" : 
			         ((attack_type == ATTACK_POIS) ? "毒" : 
					"(なし)"))))));
#else
		msg_format("For a while, the blows you deal will %s",
			     ((attack_type == ATTACK_ACID) ? "melt with acid!" :
			      ((attack_type == ATTACK_ELEC) ? "shock your foes!" :
			       ((attack_type == ATTACK_FIRE) ? "burn with fire!" : 
			        ((attack_type == ATTACK_COLD) ? "chill to the bone!" : 
			         ((attack_type == ATTACK_POIS) ? "poison your enemies!" : 
					"do nothing special."))))));
#endif
	}
	else p_ptr->magical_weapon = 0;

	if (p_ptr->magical_weapon)
	{
		if (!mw_old_weight)
		{
			mw_old_weight = o_ptr->weight;
			p_ptr->total_weight -= (o_ptr->weight - 1) * o_ptr->number;
			o_ptr->weight = 1;
		}
		if (!mw_diff_to_melee)
		{
			mw_diff_to_melee = p_ptr->cexp_info[p_ptr->pclass].clev / 2;
			o_ptr->to_h += mw_diff_to_melee;
			o_ptr->to_d += mw_diff_to_melee;
		}
	}
	else
	{
		if (mw_old_weight)
		{
			if (!item_disappear)
			{
				o_ptr->weight = mw_old_weight;
				if (item >= 0) p_ptr->total_weight += (o_ptr->weight - 1) * o_ptr->number;
			}
			mw_old_weight = 0;
		}
		if (mw_diff_to_melee && !p_ptr->evil_weapon)
		{
			if (!item_disappear)
			{
				o_ptr->to_h -= mw_diff_to_melee;
				o_ptr->to_d -= mw_diff_to_melee;
			}
			mw_diff_to_melee = 0;
		}
	}

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	return (TRUE);
}


/*
 * Set "p_ptr->evil_weapon", notice observable changes
 */
bool set_evil_weapon(int v, bool do_dec, int item, bool item_disappear)
{
	bool notice = FALSE;
	object_type *o_ptr;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	if (!item_disappear)
	{
		/* Access the item (if in the pack) */
		if (item >= 0)
		{
			o_ptr = &inventory[item];
		}
		else
		{
			o_ptr = &o_list[0 - item];
		}
	}
	else
	{
		o_ptr = NULL;
	}

	/* Open */
	if (v)
	{
		if (p_ptr->evil_weapon && !do_dec)
		{
			if (p_ptr->evil_weapon > v) return FALSE;
		}
		else if (!p_ptr->evil_weapon)
		{
#ifdef JP
			msg_print("武器が禍々しくなった。");
#else
			msg_print("Your weapon seems evil.");
#endif

			p_ptr->special_attack |= (ATTACK_EVIL);

			if (!mw_diff_to_melee)
			{
				mw_diff_to_melee = p_ptr->cexp_info[p_ptr->pclass].clev / 2;
				o_ptr->to_h += mw_diff_to_melee;
				o_ptr->to_d += mw_diff_to_melee;
			}

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->evil_weapon)
		{
#ifdef JP
			msg_print("武器が禍々しくなくなった。");
#else
			msg_print("Your weapon no longer seems evil.");
#endif

			p_ptr->special_attack &= ~(ATTACK_EVIL);

			if (mw_diff_to_melee && !p_ptr->magical_weapon)
			{
				if (!item_disappear)
				{
					o_ptr->to_h -= mw_diff_to_melee;
					o_ptr->to_d -= mw_diff_to_melee;
				}
				mw_diff_to_melee = 0;
			}

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->evil_weapon = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->oppose_acid", notice observable changes
 */
bool set_oppose_acid(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->oppose_acid && !do_dec)
		{
			if (p_ptr->oppose_acid > v) return FALSE;
		}
		else if (!p_ptr->oppose_acid)
		{
#ifdef JP
msg_print("酸への耐性がついた気がする！");
#else
			msg_print("You feel resistant to acid!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_acid)
		{
#ifdef JP
msg_print("酸への耐性が薄れた気がする。");
#else
			msg_print("You feel less resistant to acid.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_acid = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->oppose_elec", notice observable changes
 */
bool set_oppose_elec(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->oppose_elec && !do_dec)
		{
			if (p_ptr->oppose_elec > v) return FALSE;
		}
		else if (!p_ptr->oppose_elec)
		{
#ifdef JP
msg_print("電撃への耐性がついた気がする！");
#else
			msg_print("You feel resistant to electricity!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_elec)
		{
#ifdef JP
msg_print("電撃への耐性が薄れた気がする。");
#else
			msg_print("You feel less resistant to electricity.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_elec = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->oppose_fire", notice observable changes
 */
bool set_oppose_fire(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->oppose_fire && !do_dec)
		{
			if (p_ptr->oppose_fire > v) return FALSE;
		}
		else if (!p_ptr->oppose_fire)
		{
#ifdef JP
msg_print("火への耐性がついた気がする！");
#else
			msg_print("You feel resistant to fire!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_fire)
		{
#ifdef JP
msg_print("火への耐性が薄れた気がする。");
#else
			msg_print("You feel less resistant to fire.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_fire = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->oppose_cold", notice observable changes
 */
bool set_oppose_cold(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->oppose_cold && !do_dec)
		{
			if (p_ptr->oppose_cold > v) return FALSE;
		}
		else if (!p_ptr->oppose_cold)
		{
#ifdef JP
msg_print("冷気への耐性がついた気がする！");
#else
			msg_print("You feel resistant to cold!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_cold)
		{
#ifdef JP
msg_print("冷気への耐性が薄れた気がする。");
#else
			msg_print("You feel less resistant to cold.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_cold = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->oppose_pois", notice observable changes
 */
bool set_oppose_pois(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->oppose_pois && !do_dec)
		{
			if (p_ptr->oppose_pois > v) return FALSE;
		}
		else if (!p_ptr->oppose_pois)
		{
#ifdef JP
msg_print("毒への耐性がついた気がする！");
#else
			msg_print("You feel resistant to poison!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_pois)
		{
#ifdef JP
msg_print("毒への耐性が薄れた気がする。");
#else
			msg_print("You feel less resistant to poison.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_pois = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->stun", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
bool set_stun(int v)
{
	int old_aux, new_aux;
	bool notice = FALSE;


	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	if ((p_ptr->pclass == CLASS_TERRORKNIGHT) && (p_ptr->cexp_info[CLASS_TERRORKNIGHT].clev > 34)) v = 0;

	old_aux = stun_level(p_ptr->stun);
	new_aux = stun_level(v);

	/* Increase stun */
	if (new_aux > old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* Stun */
			case 1:
#ifdef JP
			msg_print("意識がもうろうとしてきた。");
#else
			msg_print("You have been stunned.");
#endif

			break;

			/* Heavy stun */
			case 2:
#ifdef JP
			msg_print("意識がひどくもうろうとしてきた。");
#else
			msg_print("You have been heavily stunned.");
#endif

			break;

			/* Nearly faint */
			case 3:
#ifdef JP
			msg_print("今にも気絶しそうだ。");
#else
			msg_print("You have been nearly faint.");
#endif

			break;

			/* Knocked out */
			case 4:
#ifdef JP
			msg_print("頭がクラクラして意識が遠のいてきた。");
#else
			msg_print("You have been knocked out.");
#endif

			break;
		}

		if (randint1(1000) < v || one_in_(16))
		{
#ifdef JP
			msg_print("割れるような頭痛がする。");
#else
			msg_print("A vicious blow hits your head.");
#endif

			if (one_in_(3))
			{
				if (!p_ptr->sustain_int) (void)do_dec_stat(A_INT);
				if (!p_ptr->sustain_wis) (void)do_dec_stat(A_WIS);
			}
			else if (one_in_(2))
			{
				if (!p_ptr->sustain_int) (void)do_dec_stat(A_INT);
			}
			else
			{
				if (!p_ptr->sustain_wis) (void)do_dec_stat(A_WIS);
			}
		}

		/* Notice */
		notice = TRUE;
	}

	/* Decrease stun */
	else if (new_aux < old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* None */
			case 0:
#ifdef JP
			msg_print("やっと朦朧状態から回復した。");
#else
			msg_print("You are no longer stunned.");
#endif

			if (disturb_state) disturb(0, 0);
			break;
		}

		/* Notice */
		notice = TRUE;
	}

	/* Use the value */
	p_ptr->stun = v;

	/* No change */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->cut", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
bool set_cut(int v)
{
	int old_aux, new_aux;
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	if ((rp_ptr->r_flags & PRF_UNDEAD) || (cp_ptr->c_flags & PCF_UNDEAD))
		v = 0;

	old_aux = cut_level(p_ptr->cut);
	new_aux = cut_level(v);

	/* Increase cut */
	if (new_aux > old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* Graze */
			case 1:
#ifdef JP
			msg_print("かすり傷を負ってしまった。");
#else
			msg_print("You have been given a graze.");
#endif

			break;

			/* Light cut */
			case 2:
#ifdef JP
			msg_print("軽い傷を負ってしまった。");
#else
			msg_print("You have been given a light cut.");
#endif

			break;

			/* Bad cut */
			case 3:
#ifdef JP
			msg_print("ひどい傷を負ってしまった。");
#else
			msg_print("You have been given a bad cut.");
#endif

			break;

			/* Nasty cut */
			case 4:
#ifdef JP
			msg_print("大変な傷を負ってしまった。");
#else
			msg_print("You have been given a nasty cut.");
#endif

			break;

			/* Severe cut */
			case 5:
#ifdef JP
			msg_print("重大な傷を負ってしまった。");
#else
			msg_print("You have been given a severe cut.");
#endif

			break;

			/* Deep gash */
			case 6:
#ifdef JP
			msg_print("ひどい深手を負ってしまった。");
#else
			msg_print("You have been given a deep gash.");
#endif

			break;

			/* Mortal wound */
			case 7:
#ifdef JP
			msg_print("致命的な傷を負ってしまった。");
#else
			msg_print("You have been given a mortal wound.");
#endif

			break;
		}

		/* Notice */
		notice = TRUE;

		if (randint1(1000) < v || one_in_(16))
		{
			if (!p_ptr->sustain_chr)
			{
#ifdef JP
				msg_print("ひどい傷跡が残ってしまった。");
#else
				msg_print("You have been horribly scarred.");
#endif


				do_dec_stat(A_CHR);
			}
		}
	}

	/* Decrease cut */
	else if (new_aux < old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* None */
			case 0:
#ifdef JP
			msg_format("やっと出血が止まった。");
#else
			msg_print("You are no longer bleeding.");
#endif

			if (disturb_state) disturb(0, 0);
			break;
		}

		/* Notice */
		notice = TRUE;
	}

	/* Use the value */
	p_ptr->cut = v;

	/* No change */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


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
	int  old_aux, new_aux;
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 20000) ? 20000 : (v < 0) ? 0 : v;

	/* XXX Hack -- No digest */
	if (p_ptr->no_digest) v = p_ptr->food;

	/* Fainting / Starving */
	if (p_ptr->food < PY_FOOD_FAINT)
	{
		old_aux = 0;
	}

	/* Weak */
	else if (p_ptr->food < PY_FOOD_WEAK)
	{
		old_aux = 1;
	}

	/* Hungry */
	else if (p_ptr->food < PY_FOOD_ALERT)
	{
		old_aux = 2;
	}

	/* Normal */
	else if (p_ptr->food < PY_FOOD_FULL)
	{
		old_aux = 3;
	}

	/* Full */
	else if (p_ptr->food < PY_FOOD_MAX)
	{
		old_aux = 4;
	}

	/* Gorged */
	else
	{
		old_aux = 5;
	}

	/* Fainting / Starving */
	if (v < PY_FOOD_FAINT)
	{
		new_aux = 0;
	}

	/* Weak */
	else if (v < PY_FOOD_WEAK)
	{
		new_aux = 1;
	}

	/* Hungry */
	else if (v < PY_FOOD_ALERT)
	{
		new_aux = 2;
	}

	/* Normal */
	else if (v < PY_FOOD_FULL)
	{
		new_aux = 3;
	}

	/* Full */
	else if (v < PY_FOOD_MAX)
	{
		new_aux = 4;
	}

	/* Gorged */
	else
	{
		new_aux = 5;
	}

	/* Food increase */
	if (new_aux > old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* Weak */
			case 1:
#ifdef JP
msg_print("まだ空腹で倒れそうだ。");
#else
			msg_print("You are still weak.");
#endif

			break;

			/* Hungry */
			case 2:
#ifdef JP
msg_print("まだ空腹だ。");
#else
			msg_print("You are still hungry.");
#endif

			break;

			/* Normal */
			case 3:
#ifdef JP
msg_print("空腹感がおさまった。");
#else
			msg_print("You are no longer hungry.");
#endif

			break;

			/* Full */
			case 4:
#ifdef JP
msg_print("満腹だ！");
#else
			msg_print("You are full!");
#endif

			break;

			/* Bloated */
			case 5:
#ifdef JP
msg_print("食べ過ぎだ！");
#else
			msg_print("You have gorged yourself!");
#endif

			break;
		}

		/* Change */
		notice = TRUE;
	}

	/* Food decrease */
	else if (new_aux < old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* Fainting / Starving */
			case 0:
#ifdef JP
msg_print("あまりにも空腹で気を失ってしまった！");
#else
			msg_print("You are getting faint from hunger!");
#endif

			break;

			/* Weak */
			case 1:
#ifdef JP
msg_print("お腹が空いて倒れそうだ。");
#else
			msg_print("You are getting weak from hunger!");
#endif

			break;

			/* Hungry */
			case 2:
#ifdef JP
msg_print("お腹が空いてきた。");
#else
			msg_print("You are getting hungry.");
#endif

			break;

			/* Normal */
			case 3:
#ifdef JP
msg_print("満腹感がなくなった。");
#else
			msg_print("You are no longer full.");
#endif

			break;

			/* Full */
			case 4:
#ifdef JP
msg_print("やっとお腹がきつくなくなった。");
#else
			msg_print("You are no longer gorged.");
#endif

			break;
		}

		if (p_ptr->wild_mode && (new_aux < 2))
		{
			p_ptr->wilderness_x = px;
			p_ptr->wilderness_y = py;
			p_ptr->energy_need = 0;
			change_wild_mode();
		}

		/* Change */
		notice = TRUE;
	}

	/* Use the value */
	p_ptr->food = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

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
 * Increases a stat by one randomized level             -RAK-
 *
 * Note that this function (used by stat potions) now restores
 * the stat BEFORE increasing it.
 */
bool inc_stat(int stat)
{
	int value, gain;

	/* Then augment the current/max stat */
	value = p_ptr->stat_cur[stat];

	/* Cannot go above 18/200 */
	if (value < STAT_MAX_MAX)
	{
		/* Gain one (sometimes two) points */
		if (value < 18)
		{
			gain = ((randint0(100) < 75) ? 1 : 2);
			value += gain;
		}

		/* Gain fraction of distance to 18/200 */
		else if (value < (STAT_MAX_MAX - 4))
		{
			/* Approximate gain value */
			gain = ((STAT_MAX_MAX - value) / 3 + 5) / 3;

			/* Paranoia */
			if (gain < 1) gain = 1;

			/* Apply the bonus */
			value += randint1(gain) + gain / 2;

			/* Maximal value */
			if (value > (STAT_MAX_MAX - 1)) value = STAT_MAX_MAX - 1;
		}

		/* Gain one point at a time */
		else
		{
			value++;
		}

		/* Save the new value */
		p_ptr->stat_cur[stat] = value;

		/* Bring up the maximum too */
		if (value > p_ptr->stat_max[stat])
		{
			p_ptr->stat_max[stat] = value;
		}

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Success */
		return (TRUE);
	}

	/* Nothing to gain */
	return (FALSE);
}



/*
 * Decreases a stat by an amount indended to vary from 0 to 100 percent.
 *
 * Amount could be a little higher in extreme cases to mangle very high
 * stats from massive assaults.  -CWS
 *
 * Note that "permanent" means that the *given* amount is permanent,
 * not that the new value becomes permanent.  This may not work exactly
 * as expected, due to "weirdness" in the algorithm, but in general,
 * if your stat is already drained, the "max" value will not drop all
 * the way down to the "cur" value.
 */
bool dec_stat(int stat, int amount, int permanent)
{
	int cur, max, loss, same, res = FALSE;


	/* Acquire current value */
	cur = p_ptr->stat_cur[stat];
	max = p_ptr->stat_max[stat];

	/* Note when the values are identical */
	same = (cur == max);

	/* Damage "current" value */
	if (cur > 3)
	{
		/* Handle "low" values */
		if (cur <= 18)
		{
			if (amount > 90) cur--;
			if (amount > 50) cur--;
			if (amount > 20) cur--;
			cur--;
		}

		/* Handle "high" values */
		else
		{
			/* Hack -- Decrement by a random amount between one-quarter */
			/* and one-half of the stat bonus times the percentage, with a */
			/* minimum damage of half the percentage. -CWS */
			loss = (((cur-18) / 2 + 1) / 2 + 1);

			/* Paranoia */
			if (loss < 1) loss = 1;

			/* Randomize the loss */
			loss = ((randint1(loss) + loss) * amount) / 100;

			/* Maximal loss */
			if (loss < amount/2) loss = amount/2;

			/* Lose some points */
			cur = cur - loss;

			/* Hack -- Only reduce stat to 17 sometimes */
			if (cur < 18) cur = (amount <= 20) ? 18 : 17;
		}

		/* Prevent illegal values */
		if (cur < 3) cur = 3;

		/* Something happened */
		if (cur != p_ptr->stat_cur[stat]) res = TRUE;
	}

	/* Damage "max" value */
	if (permanent && (max > 3))
	{
		/* Handle "low" values */
		if (max <= 18)
		{
			if (amount > 90) max--;
			if (amount > 50) max--;
			if (amount > 20) max--;
			max--;
		}

		/* Handle "high" values */
		else
		{
			/* Hack -- Decrement by a random amount between one-quarter */
			/* and one-half of the stat bonus times the percentage, with a */
			/* minimum damage of half the percentage. -CWS */
			loss = (((max-18) / 2 + 1) / 2 + 1);
			loss = ((randint1(loss) + loss) * amount) / 100;
			if (loss < amount/2) loss = amount/2;

			/* Lose some points */
			max = max - loss;

			/* Hack -- Only reduce stat to 17 sometimes */
			if (max < 18) max = (amount <= 20) ? 18 : 17;
		}

		/* Hack -- keep it clean */
		if (same || (max < cur)) max = cur;

		/* Something happened */
		if (max != p_ptr->stat_max[stat]) res = TRUE;
	}

	/* Apply changes */
	if (res)
	{
		/* Actually set the stat to its new value. */
		p_ptr->stat_cur[stat] = cur;
		p_ptr->stat_max[stat] = max;

		/* Redisplay the stats later */
		p_ptr->redraw |= (PR_STATS);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);
	}

	/* Done */
	return (res);
}


/*
 * Restore a stat.  Return TRUE only if this actually makes a difference.
 */
bool res_stat(int stat)
{
	/* Restore if needed */
	if (p_ptr->stat_cur[stat] != p_ptr->stat_max[stat])
	{
		/* Restore */
		p_ptr->stat_cur[stat] = p_ptr->stat_max[stat];

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redisplay the stats later */
		p_ptr->redraw |= (PR_STATS);

		/* Success */
		return (TRUE);
	}

	/* Nothing to restore */
	return (FALSE);
}


/*
 * Increase players hit points, notice effects
 */
bool hp_player(int num)
{
	/* Healing needed */
	if (p_ptr->chp < p_ptr->mhp)
	{
		/* Gain hitpoints */
		p_ptr->chp += num;

		/* Enforce maximum */
		if (p_ptr->chp >= p_ptr->mhp)
		{
			p_ptr->chp = p_ptr->mhp;
			p_ptr->chp_frac = 0;
			sound(SOUND_X_HEAL);
		}
		else sound(SOUND_HEAL);

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

		/* Heal 0-4 */
		if (num < 5)
		{
#ifdef JP
msg_print("少し気分が良くなった。");
#else
			msg_print("You feel a little better.");
#endif

		}

		/* Heal 5-14 */
		else if (num < 15)
		{
#ifdef JP
msg_print("気分が良くなった。");
#else
			msg_print("You feel better.");
#endif

		}

		/* Heal 15-34 */
		else if (num < 35)
		{
#ifdef JP
msg_print("とても気分が良くなった。");
#else
			msg_print("You feel much better.");
#endif

		}

		/* Heal 35+ */
		else
		{
#ifdef JP
msg_print("ひじょうに気分が良くなった。");
#else
			msg_print("You feel very good.");
#endif

		}

		/* Notice */
		return (TRUE);
	}

	/* Ignore */
	return (FALSE);
}


/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_pos[] =
{
#ifdef JP
"強く",
#else
	"strong",
#endif

#ifdef JP
"知的に",
#else
	"smart",
#endif

#ifdef JP
"賢く",
#else
	"wise",
#endif

#ifdef JP
"器用に",
#else
	"dextrous",
#endif

#ifdef JP
"健康に",
#else
	"healthy",
#endif

#ifdef JP
"美しく"
#else
	"cute"
#endif

};


/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_neg[] =
{
#ifdef JP
"弱く",
"無知に",
"愚かに",
"不器用に",
"不健康に",
"醜く"
#else
	"weak",
	"stupid",
	"naive",
	"clumsy",
	"sickly",
	"ugly"
#endif

};


/*
 * Lose a "point"
 */
bool do_dec_stat(int stat)
{
	bool sust = FALSE;

	/* Access the "sustain" */
	switch (stat)
	{
		case A_STR: if (p_ptr->sustain_str) sust = TRUE; break;
		case A_INT: if (p_ptr->sustain_int) sust = TRUE; break;
		case A_WIS: if (p_ptr->sustain_wis) sust = TRUE; break;
		case A_DEX: if (p_ptr->sustain_dex) sust = TRUE; break;
		case A_CON: if (p_ptr->sustain_con) sust = TRUE; break;
		case A_CHR: if (p_ptr->sustain_chr) sust = TRUE; break;
	}

	/* Sustain */
	if (sust && (!ironman_nightmare || randint0(13)))
	{
		/* Message */
#ifdef JP
msg_format("%sなった気がしたが、すぐに元に戻った。",
#else
		msg_format("You feel %s for a moment, but the feeling passes.",
#endif

				desc_stat_neg[stat]);

		/* Notice effect */
		return (TRUE);
	}

	/* Attempt to reduce the stat */
	if (dec_stat(stat, 10, (ironman_nightmare && !randint0(13))))
	{
		/* Message */
#ifdef JP
msg_format("ひどく%sなった気がする。", desc_stat_neg[stat]);
#else
		msg_format("You feel very %s.", desc_stat_neg[stat]);
#endif


		/* Notice effect */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}


/*
 * Restore lost "points" in a stat
 */
bool do_res_stat(int stat)
{
	/* Attempt to increase */
	if (res_stat(stat))
	{
		/* Message */
#ifdef JP
msg_format("元通りに%sなった気がする。", desc_stat_pos[stat]);
#else
		msg_format("You feel less %s.", desc_stat_neg[stat]);
#endif


		/* Notice */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}


/*
 * Gain a "point" in a stat
 */
bool do_inc_stat(int stat)
{
	bool res;

	/* Restore strength */
	res = res_stat(stat);

	/* Attempt to increase */
	if (inc_stat(stat))
	{
		/* Message */
#ifdef JP
msg_format("ワーオ！とても%sなった！", desc_stat_pos[stat]);
#else
		msg_format("Wow!  You feel very %s!", desc_stat_pos[stat]);
#endif


		/* Notice */
		return (TRUE);
	}

	/* Restoration worked */
	if (res)
	{
		/* Message */
#ifdef JP
msg_format("元通りに%sなった気がする。", desc_stat_pos[stat]);
#else
		msg_format("You feel less %s.", desc_stat_neg[stat]);
#endif


		/* Notice */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}


/*
 * Restores any drained experience
 */
bool restore_level(void)
{
	cexp_info_type *cexp_ptr;
	bool restored = FALSE;
	int i;

	if (p_ptr->exp < p_ptr->max_exp)
	{
		/* Restore the experience */
		p_ptr->exp = p_ptr->max_exp;
		restored = TRUE;
	}

	for (i = 0; i < MAX_CLASS; i++)
	{
		cexp_ptr = &p_ptr->cexp_info[i];

		if (cexp_ptr->cexp < cexp_ptr->max_cexp)
		{
			/* Restore the experience */
			cexp_ptr->cexp = cexp_ptr->max_cexp;
			restored = TRUE;
		}
	}

	if (restored)
	{
		/* Message */
#ifdef JP
		msg_print("生命力が戻ってきた気がする。");
#else
		msg_print("You feel your life energies returning.");
#endif


		/* Check the experience */
		check_experience();

		/* Did something */
		return TRUE;
	}

	/* No effect */
	return FALSE;
}


/*
 * Forget everything
 */
bool lose_all_info(void)
{
	int i;

	/* Forget info about objects */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Allow "protection" by the MENTAL flag */
		if (o_ptr->ident & (IDENT_MENTAL)) continue;

		/* Remove "default inscriptions" */
		o_ptr->feeling = FEEL_NONE;

		/* Hack -- Clear the "empty" flag */
		o_ptr->ident &= ~(IDENT_EMPTY);

		/* Hack -- Clear the "known" flag */
		o_ptr->ident &= ~(IDENT_KNOWN);

		/* Hack -- Clear the "felt" flag */
		o_ptr->ident &= ~(IDENT_SENSE);

		/* Hack -- Clear the "tried" flag */
		o_ptr->ident &= ~(IDENT_TRIED);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	/* Mega-Hack -- Forget the map */
	wiz_dark(FALSE);

	/* It worked */
	return (TRUE);
}


void do_poly_wounds(void)
{
	/* Changed to always provide at least _some_ healing */
	s16b wounds = p_ptr->cut;
	s32b hit_p = (p_ptr->mhp - p_ptr->chp);
	s32b change = damroll(p_ptr->lev, 5);
	bool Nasty_effect = one_in_(5);

	if (!(wounds || hit_p || Nasty_effect)) return;

#ifdef JP
msg_print("傷がより軽いものに変化した。");
#else
	msg_print("Your wounds are polymorphed into less serious ones.");
#endif

	hp_player(change);
	if (Nasty_effect)
	{
#ifdef JP
msg_print("新たな傷ができた！");
take_hit(DAMAGE_LOSELIFE, change / 2, "変化した傷");
#else
		msg_print("A new wound was created!");
		take_hit(DAMAGE_LOSELIFE, change / 2, "a polymorphed wound");
#endif

		set_cut(change);
	}
	else
	{
		set_cut(p_ptr->cut - (change / 2));
	}
}


void do_poly_self(void)
{
	int power = p_ptr->lev;

#ifdef JP
msg_print("あなたは変化の訪れを感じた...");
#else
	msg_print("You feel a change coming over you...");
#endif

	if ((power > randint0(20)) && one_in_(3))
	{
		char effect_msg[80] = "";
		int h_percent;

		/* Some form of racial polymorph... */
		power -= 10;

		if ((power > randint0(5)) && one_in_(4))
		{
			/* sex change */
			power -= 2;

			if (p_ptr->psex == SEX_MALE)
			{
				p_ptr->psex = SEX_FEMALE;
				sp_ptr = &sex_info[p_ptr->psex];
#ifdef JP
sprintf(effect_msg, "女性");
#else
				sprintf(effect_msg, "female");
#endif

			}
			else
			{
				p_ptr->psex = SEX_MALE;
				sp_ptr = &sex_info[p_ptr->psex];
#ifdef JP
sprintf(effect_msg, "男性");
#else
				sprintf(effect_msg, "male");
#endif

			}
		}

		if ((power > randint0(30)) && one_in_(5))
		{
			int tmp = 0;

			/* Harmful deformity */
			power -= 15;

			while (tmp < A_MAX)
			{
				if (one_in_(2))
				{
					(void)dec_stat(tmp, randint1(6) + 6, one_in_(3));
					power -= 1;
				}
				tmp++;
			}

			/* Deformities are discriminated against! */
			(void)dec_stat(A_CHR, randint1(6), TRUE);

			if (effect_msg[0])
			{
				char tmp_msg[10];
#ifdef JP
				sprintf(tmp_msg,"%s",effect_msg);
				sprintf(effect_msg,"奇形の%s",tmp_msg);
#else
				sprintf(tmp_msg,"%s",effect_msg);
				sprintf(effect_msg,"deformed %s",tmp_msg);
#endif

			}
			else
			{
#ifdef JP
				sprintf(effect_msg,"奇形");
#else
				sprintf(effect_msg,"deformities");
#endif

			}
		}

		while ((power > randint0(20)) && one_in_(10))
		{
			/* Polymorph into a less mutated form */
			power -= 10;

			if (!lose_mutation(0))
#ifdef JP
msg_print("奇妙なくらい普通になった気がする。");
#else
				msg_print("You feel oddly normal.");
#endif

		}

		if (effect_msg[0])
#ifdef JP
			msg_format("あなたは%sに変化した！", effect_msg);
#else
			msg_format("You turn into a %s!", effect_msg);
#endif
		else
#ifdef JP
			msg_print("体格が変化した！");
#else
			msg_print("Your body form is changed!");
#endif


		/* Calculate the height/weight for males */
		if (p_ptr->psex == SEX_MALE)
		{
			p_ptr->ht = randnor(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
			h_percent = (int)(p_ptr->ht) * 100 / (int)(rp_ptr->m_b_ht);
			p_ptr->wt = randnor((int)(rp_ptr->m_b_wt) * h_percent /100
					    , (int)(rp_ptr->m_m_wt) * h_percent / 300 );
		}

		/* Calculate the height/weight for females */
		else if (p_ptr->psex == SEX_FEMALE)
		{
			p_ptr->ht = randnor(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
			h_percent = (int)(p_ptr->ht) * 100 / (int)(rp_ptr->f_b_ht);
			p_ptr->wt = randnor((int)(rp_ptr->f_b_wt) * h_percent /100
					    , (int)(rp_ptr->f_m_wt) * h_percent / 300 );
		}

		p_ptr->redraw |= (PR_BASIC);

		p_ptr->update |= (PU_BONUS);

		handle_stuff();
		lite_spot(py, px);
	}

	if ((power > randint0(30)) && one_in_(6))
	{
		int tmp = 0;

		/* Abomination! */
		power -= 20;

#ifdef JP
msg_format("内臓の構成が変化した！");
#else
		msg_print("Your internal organs are rearranged!");
#endif

		while (tmp < A_MAX)
		{
			(void)dec_stat(tmp, randint1(6) + 6, one_in_(3));
			tmp++;
		}
		if (one_in_(6))
		{
#ifdef JP
			msg_print("現在の姿で生きていくのは困難なようだ！");
			take_hit(DAMAGE_LOSELIFE, damroll(randint1(10), p_ptr->lev), "致命的な突然変異");
#else
			msg_print("You find living difficult in your present form!");
			take_hit(DAMAGE_LOSELIFE, damroll(randint1(10), p_ptr->lev), "a lethal mutation");
#endif

			power -= 10;
		}
	}

	if ((power > randint0(20)) && one_in_(4))
	{
		power -= 10;

		do_cmd_rerate();
	}

	while ((power > randint0(15)) && one_in_(3))
	{
		power -= 7;
		(void)gain_random_mutation(0, TRUE);
	}

	if (power > randint0(5))
	{
		power -= 5;
		do_poly_wounds();
	}

	/* Note: earlier deductions may have left power < 0 already. */
	while (power > 0)
	{
		mutate_player();
		power--;
	}
}


/*
 * If player is dead, resurrect the player.
 */
static bool resurrect_player(int item, int percent, int reincarnate)
{
	object_type *o_ptr;
	char         o_name[MAX_NLEN];
	int          i, j;
	char         buf[80];

	/* If you use Snap Dragon Spell, you cannot revive. */
	if (p_ptr->is_dead & DEATH_SNAP_DRAGON) return FALSE;

	/* Can player reincarnate? */
	if (reincarnate > -1)
	{
		if (!can_choose_class((byte)reincarnate, CLASS_CHOOSE_MODE_DEATH)) return FALSE;
	}

	/* Use inventory slot */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
		if (!o_ptr->k_idx) return FALSE;

		/* Describe */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Message */
#ifdef JP
		msg_format("%sの力で%sから蘇った！", o_name, (p_ptr->is_dead & DEATH_STONED) ? "石像" : "死");
#else
		msg_format("%s resurrects you from %s!", o_name, (p_ptr->is_dead & DEATH_STONED) ? "stone statue" : "death");
#endif

		msg_print(NULL);

#ifdef JP
		msg_format("%sは力を失い砕け散った...", o_name);
#else
		msg_format("%s lost the power and crashes...", o_name);
#endif

		/* Break the item */
		inven_item_increase(item, -1);
		inven_item_optimize(item);
	}

	/* Reincarnate class-change */
	if (reincarnate > -1)
	{
		cexp_info_type *cexp_ptr;

		p_ptr->pclass = (byte)reincarnate;
		cp_ptr = &class_info[p_ptr->pclass];
		mp_ptr = &m_info[p_ptr->pclass];
		p_ptr->s_ptr = &s_info[p_ptr->pclass];
		cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];

		change_level99_quest(FALSE);

		if (cp_ptr->c_flags & PCF_NO_DIGEST) p_ptr->food = PY_FOOD_FULL - 1;

		if (!cexp_ptr->max_clev)
		{
			cexp_ptr->max_clev = cexp_ptr->clev = 1;
			if (!cexp_ptr->max_max_clev) cexp_ptr->max_max_clev = 1;
		}

		/* Update stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Combine / Reorder the pack */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Notice stuff */
		notice_stuff();

		/* Update stuff */
		update_stuff();

		/* Update stuff */
		p_ptr->update |= (PU_HP | PU_MANA);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_EQUIPPY);

		/* Window stuff */
		p_ptr->window |= (PW_SPELL | PW_PLAYER);

#ifdef JP
		msg_format("あなたは%sとして転生した！", cp_ptr->title);
#else
		msg_format("You are reincarnated as %s!", cp_ptr->title);
#endif
#ifdef JP
		sprintf(buf, "1度殺されたが、%sへと転生して復活した。", cp_ptr->title);
#else
		sprintf(buf, "once killed, but reincarnated as %s.", cp_ptr->title);
#endif
		/* Load the "pref" files */
		load_all_pref_files();
	}
	else if (item >= 0)
	{
#ifdef JP
		sprintf(buf, "1度殺されたが、%sの力で復活した。", o_name);
#else
		sprintf(buf, "once killed, but resurrected by %s.", o_name);
#endif
	}
	else
	{
#ifdef JP
		sprintf(buf, "1度殺されたが復活した。");
#else
		sprintf(buf, "once killed, but resurrected.");
#endif
	}
	do_cmd_write_nikki(NIKKI_BUNSHOU, 0, buf);

	/* Do not die */
	p_ptr->is_dead = 0L;

	/* Restore hit points */
	p_ptr->chp = p_ptr->mhp * percent / 100;
	p_ptr->chp_frac = 0;

	/* Restore spell points */
	if (p_ptr->csp < (p_ptr->msp * percent / 100))
	{
		p_ptr->csp = p_ptr->msp * percent / 100;
		p_ptr->csp_frac = 0;
	}

	/* Hack -- Healing */
	(void)set_blind(0);
	(void)set_confused(0);
	(void)set_poisoned(0);
	(void)set_afraid(0);
	(void)set_paralyzed(0);
	(void)set_image(0);
	(void)set_stun(0);
	(void)set_cut(0);
	(void)set_stoning(0);
	(void)set_opposite_pelem(0);

	if (p_ptr->singing)
	{
		if (p_ptr->singing == MUSIC_SILENT)
			song_of_silence(0);
		p_ptr->singing = MUSIC_NONE;
		p_ptr->restart_singing = 0;
		p_ptr->song_start = 0;
#ifdef JP
		msg_print("歌が止まった。");
#else
		msg_print("Your singing is stopped.");
#endif
		p_ptr->action = ACTION_NONE;
	}

	/* Hack -- Timed flags reset */
	dispel_player();
	set_action(ACTION_NONE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP | PR_STATUS | PR_STATE | PR_BASIC | PR_STATUS);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	if (p_ptr->resurrection_cnt < MAX_SHORT) p_ptr->resurrection_cnt++;

	p_ptr->energy_need = 0;

	/* Revived! */
	return TRUE;
}


/*
 * Decreases players hit points and sets death flag if necessary
 *
 * XXX XXX XXX Invulnerability needs to be changed into a "shield"
 *
 * XXX XXX XXX Hack -- this function allows the user to save (or quit)
 * the game when he dies, since the "You die." message is shown before
 * setting the player to "dead".
 */
int take_hit(u32b damage_type, int damage, cptr hit_from)
{
	int old_chp = p_ptr->chp;

	char death_message[1024];
	char tmp[80];

	int warning = (p_ptr->mhp * hitpoint_warn / 10);

	/* Paranoia */
	if (p_ptr->is_dead & DEATH_DEAD) return 0;

	if (easy_band) damage = (damage+1)/2;

	if (p_ptr->mermaid_in_water)
	{
		if (damage_type & DAMAGE_ELEC)
		{
			if (!p_ptr->immune_elec) damage = damage * 4 / 3;
		}
		else if (!(damage_type & (DAMAGE_FORCE | DAMAGE_GENO | DAMAGE_LOSELIFE | DAMAGE_USELIFE)))
		{
			damage = damage * 3 / 4;
		}
	}

	if (!(damage_type & DAMAGE_USELIFE))
	{
		/* Disturb */
		disturb(1, 0);
		if (auto_more)
		{
			now_damaged = TRUE;
		}
	}

	/* Mega-Hack -- Apply "invulnerability" */
	if (!(damage_type & (DAMAGE_USELIFE | DAMAGE_LOSELIFE)))
	{
		if (p_ptr->invuln && (damage < 9000))
		{
			if (damage_type & DAMAGE_FORCE)
			{
#ifdef JP
				msg_print("バリアが切り裂かれた！");
#else
				msg_print("The attack cuts your shield of invulnerability open!");
#endif
			}
			else if (one_in_(PENETRATE_INVULNERABILITY))
			{
#ifdef JP
				msg_print("無敵のバリアを破って攻撃された！");
#else
				msg_print("The attack penetrates your shield of invulnerability!");
#endif
			}
			else
			{
				return 0;
			}
		}

		/* Multishadow effects is determined by IS_MULTISHADOW() */
		if (IS_MULTISHADOW(0))
		{
			if (damage_type & DAMAGE_FORCE)
			{
#ifdef JP
				msg_print("幻影もろとも体が切り裂かれた！");
#else
				msg_print("The attack hits Shadow together with you!");
#endif
			}
			else if ((damage_type & DAMAGE_ATTACK) && !(damage_type & DAMAGE_NOESCAPE))
			{
#ifdef JP
				msg_print("攻撃は幻影に命中し、あなたには届かなかった。");
#else
				msg_print("The attack hits Shadow, you are unharmed!");
#endif
				return 0;
			}
		}

		if (WRAITH_FORM())
		{
			if (damage_type & DAMAGE_FORCE)
			{
#ifdef JP
				msg_print("半物質の体が切り裂かれた！");
#else
				msg_print("The attack cuts through your ethereal body!");
#endif
			}
			else
			{
				damage /= 2;
				if ((damage == 0) && one_in_(2)) damage = 1;
			}
		}
	} /* not if LOSELIFE USELIFE */

	/* Hurt the player */
	p_ptr->chp -= damage;
	if (show_damage && (damage > 0) && !(damage_type & DAMAGE_USELIFE))
#ifdef JP
		msg_format("%dのダメージを受けた。", damage);
#else
		msg_format("You take %d damages.", damage);
#endif
	if ((damage_type & DAMAGE_GENO) && (p_ptr->chp < 0))
	{
		damage += p_ptr->chp;
		p_ptr->chp = 0;
	}

	/* Display the hitpoints */
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	handle_stuff();

	/* Dead player */
	if ((p_ptr->chp < 0) || p_ptr->is_dead)
	{
		char buf[10];

		/* Mega-Hack - Can player revive!? */
		if (p_ptr->tim_resurrection)
		{
			if (resurrect_player(-1, 100, -1)) return damage;
		}
		if (!p_ptr->inside_arena)
		{
			if (inventory[INVEN_LITE].k_idx && (inventory[INVEN_LITE].name1 == ART_BLESSING))
			{
				if (resurrect_player(INVEN_LITE, 40, -1)) return damage;
			}
			if (inventory[INVEN_LITE].k_idx && (inventory[INVEN_LITE].name1 == ART_BLISS))
			{
				if (resurrect_player(INVEN_LITE, 100, -1)) return damage;
			}
			if (inventory[INVEN_RIGHT].k_idx && (inventory[INVEN_RIGHT].name1 == ART_LICH))
			{
				if (resurrect_player(INVEN_RIGHT, 100, CLASS_LICH)) return damage;
			}
			if (inventory[INVEN_LEFT].k_idx && (inventory[INVEN_LEFT].name1 == ART_LICH))
			{
				if (resurrect_player(INVEN_LEFT, 100, CLASS_LICH)) return damage;
			}
			if (resurrect_player(-1, 100, CLASS_ANGELKNIGHT)) return damage;
		}

		/* 死んだ時に強制終了して死を回避できなくしてみた by Habu */
		if (!cheat_save)
			if (!save_player()) msg_print("セーブ失敗！");

		/* Sound */
		sound(SOUND_DEATH);

		/* Leaving */
		p_ptr->leaving = TRUE;

		/* Note death */
		p_ptr->is_dead |= DEATH_DEAD;

		if (p_ptr->inside_arena && !(p_ptr->is_dead & DEATH_SNAP_DRAGON))
		{
			cptr m_name = r_name+r_info[arena_monsters[p_ptr->arena_number]].name;
#ifdef JP
			msg_format("あなたは%sの前に敗れ去った。", m_name);
#else
			msg_format("You are beaten by %s.", m_name);
#endif
			msg_print(NULL);
			if (record_arena) do_cmd_write_nikki(NIKKI_ARENA, 99, m_name);
		}
		else
		{
			int q_idx = quest_number(dun_level);

			/* Make screen dump */
			screen_dump = make_screen_dump();

			/* Note cause of death */
#ifdef JP
			sprintf(p_ptr->died_from, "%s%s%s", !p_ptr->paralyzed ? "" : p_ptr->free_act ? "彫像状態で":"麻痺状態で", p_ptr->image ? "幻覚に歪んだ" : "", hit_from);
#else
			sprintf(p_ptr->died_from, "%s%s", hit_from, !p_ptr->paralyzed ? "" : " while helpless");
#endif

			/* No longer a winner */
			p_ptr->total_winner = FALSE;

			if (p_ptr->inside_arena)
#ifdef JP
				strcpy(buf, "アリーナ");
#else
				strcpy(buf, "in the Arena");
#endif
			else if (!dun_level)
#ifdef JP
				strcpy(buf, "地上");
#else
				strcpy(buf, "on the surface");
#endif
			else if (q_idx &&
			        (quest_is_fixed(q_idx) &&
			       !(quest[q_idx].flags & QUEST_FLAG_GUARDIAN)))
#ifdef JP
				strcpy(buf, "クエスト");
#else
				strcpy(buf, "in a quest");
#endif
			else
#ifdef JP
				sprintf(buf,"%d階", dun_level);
#else
				sprintf(buf,"level %d", dun_level);
#endif
			if (p_ptr->is_dead & DEATH_SNAP_DRAGON)
#ifdef JP
				sprintf(tmp,"%sでスナップドラゴンを使い生きた武器に変化した。", buf);
#else
				sprintf(tmp,"changed into a living weapon on %s.", buf);
#endif
			else
#ifdef JP
				sprintf(tmp,"%sで%sに%s。",buf, p_ptr->died_from, (p_ptr->is_dead & DEATH_STONED) ? "石化された" : "殺された");
#else
				sprintf(tmp,"%s by %s %s.", (p_ptr->is_dead & DEATH_STONED) ? "stoned" : "killed", p_ptr->died_from, buf);
#endif
			do_cmd_write_nikki(NIKKI_BUNSHOU, 0, tmp);
#ifdef JP
			do_cmd_write_nikki(NIKKI_GAMESTART, 1, "-------- ゲームオーバー --------");
#else
			do_cmd_write_nikki(NIKKI_GAMESTART, 1, "--------   Game  Over   --------");
#endif
			do_cmd_write_nikki(NIKKI_BUNSHOU, 1, "\n\n\n\n");

			flush();

#ifdef JP
if (get_check_strict("画面を保存しますか？", CHECK_NO_HISTORY))
#else
			if (get_check_strict("Dump the screen? ", CHECK_NO_HISTORY))
#endif

			{
				do_cmd_save_screen();
			}

			flush();

			/* Hack -- Note death */
			if (!last_words && !(p_ptr->is_dead & DEATH_SNAP_DRAGON))
			{
#ifdef JP
msg_format("あなたは%sました。", (p_ptr->is_dead & DEATH_STONED) ? "石化し" : "死に");
#else
				msg_print((p_ptr->is_dead & DEATH_STONED) ? "You are stoned." : "You die.");
#endif

				msg_print(NULL);
			}
			else if (last_words)
			{
#ifdef JP
				get_rnd_line("death_j.txt", 0, death_message);
#else
				get_rnd_line("death.txt", 0, death_message);
#endif
#ifdef JP
				while (!get_string((p_ptr->is_dead & DEATH_SNAP_DRAGON) ? "最後の言葉: " : "断末魔の叫び: ", death_message, 1024)) ;
#else
				while (!get_string("Last word: ", death_message, 1024)) ;
#endif
				if ((death_message[0] == '\0') && !(p_ptr->is_dead & DEATH_SNAP_DRAGON))
				{
#ifdef JP
					strcpy(death_message, format("あなたは%sました。", (p_ptr->is_dead & DEATH_STONED) ? "石化し" : "死に"));
#else
					strcpy(death_message, (p_ptr->is_dead & DEATH_STONED) ? "You are stoned." : "You die.");
#endif
				}
				else if (death_message[0] != '\0')
					msg_print(death_message);
			}
		}

		/* Dead */
		return damage;
	}

	/* Hitpoint warning */
	if (p_ptr->chp < warning)
	{
		/* Hack -- bell on first notice */
		if (alert_hitpoint && (old_chp > warning)) bell();

		sound(SOUND_WARN);

		if (record_danger && (old_chp > warning))
		{
			if (p_ptr->image && (damage_type & DAMAGE_ATTACK))
#ifdef JP
				hit_from = "何か";
#else
				hit_from = "something";
#endif

#ifdef JP
			sprintf(tmp,"%sによってピンチに陥った。",hit_from);
#else
			sprintf(tmp,"A critical situation because of %s.",hit_from);
#endif
			do_cmd_write_nikki(NIKKI_BUNSHOU, 0, tmp);
		}

		if (auto_more)
		{
			/* stop auto_more even if DAMAGE_USELIFE */
			now_damaged = TRUE;
		}

		/* Message */
#ifdef JP
msg_print("*** 警告:低ヒット・ポイント！ ***");
#else
		msg_print("*** LOW HITPOINT WARNING! ***");
#endif

		msg_print(NULL);
		flush();
	}
	if (p_ptr->wild_mode && !p_ptr->leaving && (p_ptr->chp < MAX(warning, p_ptr->mhp/5)))
	{
		p_ptr->wilderness_x = px;
		p_ptr->wilderness_y = py;
		p_ptr->energy_need = 0;
		change_wild_mode();
	}
	return damage;
}


/*
 * Gain class experience
 */
void gain_class_exp(s32b amount)
{
	cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];

	if (p_ptr->is_dead) return;

	/* Gain some class experience */
	cexp_ptr->cexp += amount;

	/* Slowly recover from class experience drainage */
	if (cexp_ptr->cexp < cexp_ptr->max_cexp)
	{
		/* Gain max class experience (20%) (was 10%) */
		cexp_ptr->max_cexp += amount / 5;
	}

	/* Check class experience */
	check_class_experience();
}


/*
 * Gain racial experience
 */
void gain_racial_exp(s32b amount)
{
	if (p_ptr->is_dead) return;

	/* Gain some racial experience */
	p_ptr->exp += amount;

	/* Slowly recover from racial experience drainage */
	if (p_ptr->exp < p_ptr->max_exp)
	{
		/* Gain max racial experience (20%) (was 10%) */
		p_ptr->max_exp += amount / 5;
	}

	/* Check racial experience */
	check_racial_experience();
}


/*
 * Gain experience
 */
void gain_exp(s32b amount)
{
	if (p_ptr->is_dead) return;

	gain_class_exp(amount);
	gain_racial_exp(amount);
}


/*
 * Lose class experience
 */
void lose_class_exp(s32b amount)
{
	cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];

	/* Never drop below zero class experience */
	if (amount > cexp_ptr->cexp) amount = cexp_ptr->cexp;

	/* Lose some class experience */
	cexp_ptr->cexp -= amount;

	/* Check class experience */
	check_class_experience();
}


/*
 * Lose racial experience
 */
void lose_racial_exp(s32b amount)
{
	/* Never drop below zero racial experience */
	if (amount > p_ptr->exp) amount = p_ptr->exp;

	/* Lose some racial experience */
	p_ptr->exp -= amount;

	/* Check racial experience */
	check_racial_experience();
}


/*
 * Lose experience
 */
void lose_exp(s32b amount)
{
	lose_class_exp(amount);
	lose_racial_exp(amount);
}


bool set_tim_res_time(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_res_time && !do_dec)
		{
			if (p_ptr->tim_res_time > v) return FALSE;
		}
		else if (!p_ptr->tim_res_time)
		{
#ifdef JP
msg_print("時間逆転の力に対して耐性がついた気がする！");
#else
			msg_print("You feel time resistant!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_res_time)
		{
#ifdef JP
msg_print("時間逆転の力に対する耐性が薄れた気がする。");
#else
			msg_print("You feel less time resistant");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_res_time = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Choose a warrior-mage elemental attack. -LM-
 */
bool choose_magical_weapon(void)
{
	int num;
	char choice;
	int clev = p_ptr->cexp_info[p_ptr->pclass].clev;

	if (!buki_motteruka(INVEN_RARM))
	{
#ifdef JP
		msg_format("利き腕に武器を持たないとマジカルウェポンは使えない。");
#else
		msg_format("You cannot use magical weapon branding with no main weapon.");
#endif
		return FALSE;
	}

	/* Save screen */
	screen_save();

	num = (clev - 20) / 5;

#ifdef JP
	c_prt(TERM_RED, "        a) 焼棄", 2, 14);
#else
	c_prt(TERM_RED, "        a) Fire Brand", 2, 14);
#endif

#ifdef JP
	if (num >= 2) c_prt(TERM_L_WHITE,"        b) 凍結", 3, 14);
#else
	if (num >= 2) c_prt(TERM_L_WHITE,"        b) Cold Brand", 3, 14);
#endif
	else prt("", 3, 14);

#ifdef JP
	if (num >= 3) c_prt(TERM_GREEN,  "        c) 毒殺", 4, 14);
#else
	if (num >= 3) c_prt(TERM_GREEN,  "        c) Poison Brand", 4, 14);
#endif
	else prt("", 4, 14);

#ifdef JP
	if (num >= 4) c_prt(TERM_L_DARK, "        d) 溶解", 5, 14);
#else
	if (num >= 4) c_prt(TERM_L_DARK, "        d) Acid Brand", 5, 14);
#endif
	else prt("", 5, 14);

#ifdef JP
	if (num >= 5) c_prt(TERM_BLUE,   "        e) 電撃", 6, 14);
#else
	if (num >= 5) c_prt(TERM_BLUE,   "        e) Elec Brand", 6, 14);
#endif
	else prt("", 6, 14);

	prt("", 7, 14);
	prt("", 8, 14);
	prt("", 9, 14);

	prt("", 1, 0);
#ifdef JP
	prt("        どの元素攻撃をしますか？", 1, 14);
#else
	prt("        Choose a temporary elemental brand ", 1, 14);
#endif

	choice = inkey();

	if ((choice == 'a') || (choice == 'A')) 
		set_magical_weapon(ATTACK_FIRE, clev/2 + randint1(clev/2), INVEN_RARM, FALSE);
	else if (((choice == 'b') || (choice == 'B')) && (num >= 2))
		set_magical_weapon(ATTACK_COLD, clev/2 + randint1(clev/2), INVEN_RARM, FALSE);
	else if (((choice == 'c') || (choice == 'C')) && (num >= 3))
		set_magical_weapon(ATTACK_POIS, clev/2 + randint1(clev/2), INVEN_RARM, FALSE);
	else if (((choice == 'd') || (choice == 'D')) && (num >= 4))
		set_magical_weapon(ATTACK_ACID, clev/2 + randint1(clev/2), INVEN_RARM, FALSE);
	else if (((choice == 'e') || (choice == 'E')) && (num >= 5))
		set_magical_weapon(ATTACK_ELEC, clev/2 + randint1(clev/2), INVEN_RARM, FALSE);
	else
	{
#ifdef JP
		msg_print("マジカルウェポンを使うのをやめた。");
#else
		msg_print("You cancel the magical weapon branding.");
#endif
		screen_load();
		return FALSE;
	}
	/* Load screen */
	screen_load();
	return TRUE;
}


bool set_earth_spike(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->earth_spike && !do_dec)
		{
			if (p_ptr->earth_spike > v) return FALSE;
		}
		else if (!p_ptr->earth_spike)
		{
#ifdef JP
			msg_print("体が大地にしっかり固定された気がする！");
#else
			msg_print("You feel your body is firmly fixed on earth!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->earth_spike)
		{
#ifdef JP
			msg_print("体が大地に固定された気分がなくなった。");
#else
			msg_print("You feel less fixed on earth.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->earth_spike = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


bool set_wind_guard(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->wind_guard && !do_dec)
		{
			if (p_ptr->wind_guard > v) return FALSE;
		}
		else if (!p_ptr->wind_guard)
		{
#ifdef JP
			msg_print("射撃をかわしきれる気がする！");
#else
			msg_print("You feel avoidance to shooting!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->wind_guard)
		{
#ifdef JP
			msg_print("射撃をかわしきれる気分がなくなった。");
#else
			msg_print("You feel less avoidance to shooting.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->wind_guard = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


bool set_tim_resurrection(int v, bool do_dec)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->is_dead) return FALSE;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_resurrection && !do_dec)
		{
			if (p_ptr->tim_resurrection > v) return FALSE;
		}
		else if (!p_ptr->tim_resurrection)
		{
#ifdef JP
			msg_print("純粋な生命のオーラに包まれた。");
#else
			msg_print("You are surrounded by a pure life aura.");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_resurrection)
		{
#ifdef JP
			msg_print("純粋な生命のオーラが消えた。");
#else
			msg_print("A pure life aura disappear.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_resurrection = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}
