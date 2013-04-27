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
	  		sound(SOUND_BLIND);
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->blind)
		{
#ifdef JP
			msg_print("やっと目が見えるようになった！");
#else
			msg_print("You can see again.");
#endif
	  		sound(SOUND_RECOVER);
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->blind = v;

	/* Redraw status bar*/
	p_ptr->redraw |=  (PR_STATUS);

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
	  		sound(SOUND_CONFUSED);
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
	  		sound(SOUND_RECOVER);
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->confused = v;

	/* Redraw status bar*/
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
	  		sound(SOUND_POISONED);
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
	  		sound(SOUND_RECOVER);
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
	  		sound(SOUND_AFRAID);
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
	  		sound(SOUND_RECOVER);
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
	  		sound(SOUND_PARALYZED);
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
	  		sound(SOUND_RECOVER);
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->paralyzed = v;

	/* Redraw status bar*/
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
	  		sound(SOUND_DRUGGED);
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
	  		sound(SOUND_RECOVER);
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->image = v;

	/* Redraw status bar*/
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
 * Set "p_ptr->fast", notice observable changes
 */
bool set_fast(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->fast)
		{
#ifdef JP
			msg_print("素早く動けるようになった！");
#else
			msg_print("You feel yourself moving faster!");
#endif
	  		sound(SOUND_SPEED);
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

	/* Redraw status bar*/
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
 * Set "p_ptr->slow", notice observable changes
 */
bool set_slow(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->slow)
		{
#ifdef JP
			msg_print("体の動きが遅くなってしまった！");
#else
			msg_print("You feel yourself moving slower!");
#endif
	  		sound(SOUND_SLOW);
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
	  		sound(SOUND_RECOVER);
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->slow = v;

	/* Redraw status bar*/
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
 * Set "p_ptr->shield", notice observable changes
 */
bool set_shield(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->shield)
		{
#ifdef JP
			msg_print("肌が石になった。");
#else
			msg_print("Your skin turns to stone.");
#endif
	  		sound(SOUND_SHIELD);
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

	/* Redraw status bar*/
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
bool set_magicdef(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->magicdef)
		{
#ifdef JP
			msg_print("魔法の防御力が増したような気がする。");
#else
			msg_print("You feel your magical vulnerablity diminish.");
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
			msg_print("Your magical defences fall to their normal values.");
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
 * Set "p_ptr->musou", notice observable changes
 */
bool set_musou(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->musou)
		{
#ifdef JP
			msg_print("あらゆることに対して耐性がついた気がする！");
#else
			msg_print("You feel resistance!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->musou)
		{
#ifdef JP
			msg_print("あらゆることに対する耐性が薄れた気がする。");
#else
			msg_print("You feel less resistantce");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->musou = v;

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
bool set_blessed(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->blessed)
		{
#ifdef JP
			msg_print("高潔な気分になった！");
#else
			msg_print("You feel righteous!");
#endif
	  		sound(SOUND_BLESSED);
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

	/* Redraw status bar*/
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
bool set_hero(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->hero)
		{
#ifdef JP
			msg_print("ヒーローになった気がする！");
#else
			msg_print("You feel like a hero!");
#endif
	  		sound(SOUND_HERO);
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

	/* Redraw status bar*/
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
bool set_shero(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->shero)
		{
#ifdef JP
			msg_print("殺戮マシーンになった気がする！");
#else
			msg_print("You feel like a killing machine!");
#endif
	  		sound(SOUND_HERO); /* (Sound substitute) no sound for beserk */
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

	/* Redraw status bar*/
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
bool set_protevil(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->protevil)
		{
#ifdef JP
			msg_print("邪悪なる存在から守られているような感じがする！");
#else
			msg_print("You feel safe from evil!");
#endif
	  		sound(SOUND_PROT_EVIL);
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

	/* Redraw status bar*/
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


void notice_wraith_form(bool flag)
{
	if (flag)
	{
#ifdef JP
		msg_print("物質界を離れて幽鬼のような存在になった！");
#else
		msg_print("You leave the physical world and turn into a wraith-being!");
#endif
	}
	else
	{
#ifdef JP
		msg_print("不透明になった感じがする。");
#else
		msg_print("You feel opaque.");
#endif
	}

	/* Redraw map and status bar */
	p_ptr->redraw |= (PR_MAP | PR_STATUS);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
}

/*
 * Set "p_ptr->wraith_form", notice observable changes
 */
bool set_wraith_form(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_wraith)
		{
			notice = TRUE;

		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_wraith)
		{
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_wraith = v;

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
bool set_invuln(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->invuln)
		{
#ifdef JP
			msg_print("無敵だ！");
#else
			msg_print("Invulnerability!");
#endif
	  		sound(SOUND_INVULN);
			notice = TRUE;

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

			if (ironman_hengband)
				p_ptr->energy_need += ENERGY_NEED();
		}
	}

	/* Use the value */
	p_ptr->invuln = v;

	/* Redraw status bar*/
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
bool set_tim_esp(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_esp)
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

	/* Redraw status bar*/
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
bool set_tim_invis(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_invis)
		{
#ifdef JP
			msg_print("目が非常に敏感になった気がする！");
#else
			msg_print("Your eyes feel very sensitive!");
#endif
	  		sound(SOUND_SEE_INVIS);
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

	/* Redraw status bar*/
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
bool set_tim_infra(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_infra)
		{
#ifdef JP
			msg_print("目がランランと輝き始めた！");
#else
			msg_print("Your eyes begin to tingle!");
#endif
	  		sound(SOUND_INFRARED);
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

	/* Redraw status bar*/
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
 * Set "p_ptr->tim_radar", notice observable changes
 */
bool set_tim_radar(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_radar)
		{
#ifdef JP
			msg_print("視界が良好になった！");
#else
			msg_print("Your visibility is very fine!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_radar)
		{
#ifdef JP
			msg_print("視界が暗くなった気がする。");
#else
			msg_print("Your visibility feels worth.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_radar = v;

	/* Redraw status bar*/
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
 * Set "p_ptr->tim_might", notice observable changes
 */
bool set_tim_might(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_might)
		{
#ifdef JP
			msg_print("力が強くなった気がする！");
#else
			msg_print("You get extra strength!");
#endif
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_might)
		{
#ifdef JP
			msg_print("力が弱くなった気がする。");
#else
			msg_print("You lost extra strength.");
#endif
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_might = v;

	/* Redraw status bar*/
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
 * Set "p_ptr->tim_regen", notice observable changes
 */
bool set_tim_regen(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_regen)
		{
#ifdef JP
			msg_print("回復力が増した！");
#else
			msg_print("You feel yourself regenerating quickly!");
#endif

			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_regen)
		{
#ifdef JP
			msg_print("素早く回復する感じがなくなった。");
#else
			msg_print("You feel yourself regenerating slowly.");
#endif

			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_regen = v;

	/* Redraw status bar*/
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


bool set_tim_brand(int v, u32b flg)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_brand)
		{
#ifdef JP
			msg_print("あなたの武器が輝いた！");
#else
			msg_print("Your weapon glows brightly!");
#endif
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_brand)
		{
#ifdef JP
			msg_print("武器の輝きがなくなった。");
#else
			msg_print("The shine of your weapon was lost.");
#endif
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_brand = v;

	if (p_ptr->tim_brand)
		p_ptr->xtra_brand |= flg;
	else
		p_ptr->xtra_brand = 0L;

	/* Redraw status bar*/
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
 * Set "p_ptr->tim_sh_fire", notice observable changes
 */
bool set_tim_sh_fire(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_sh_fire)
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
bool set_tim_sh_elec(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_sh_elec)
		{
#ifdef JP
			msg_print("体が電撃のオーラで覆われた。");
#else
			msg_print("You have been enveloped by electorical aura!");
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
			msg_print("電撃のオーラが消えた。");
#else
			msg_print("Your electorical aura has disappeared.");
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
bool set_tim_sh_cold(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_sh_cold)
		{
#ifdef JP
			msg_print("体が冷気のオーラで覆われた。");
#else
			msg_print("You have been enveloped by icy aura!");
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
			msg_print("Your icy aura disappeared.");
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
 * Set "p_ptr->oppose_acid", notice observable changes
 */
bool set_oppose_acid(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->oppose_acid)
		{
#ifdef JP
			msg_print("酸への耐性がついた気がする！");
#else
			msg_print("You feel resistant to acid!");
#endif
			sound(SOUND_RES_ACID);
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

	/* Redraw status bar*/
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
 * Set "p_ptr->oppose_elec", notice observable changes
 */
bool set_oppose_elec(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->oppose_elec)
		{
#ifdef JP
			msg_print("電撃への耐性がついた気がする！");
#else
			msg_print("You feel resistant to electricity!");
#endif
			sound(SOUND_RES_ELEC);
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

	/* Redraw status bar*/
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
 * Set "p_ptr->oppose_fire", notice observable changes
 */
bool set_oppose_fire(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->oppose_fire)
		{
#ifdef JP
			msg_print("火への耐性がついた気がする！");
#else
			msg_print("You feel resistant to fire!");
#endif
			sound(SOUND_RES_FIRE);
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

	/* Redraw status bar*/
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
 * Set "p_ptr->oppose_cold", notice observable changes
 */
bool set_oppose_cold(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->oppose_cold)
		{
#ifdef JP
			msg_print("冷気への耐性がついた気がする！");
#else
			msg_print("You feel resistant to cold!");
#endif
			sound(SOUND_RES_COLD);
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

	/* Redraw status bar*/
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
 * Set "p_ptr->oppose_pois", notice observable changes
 */
bool set_oppose_pois(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->oppose_pois)
		{
#ifdef JP
			msg_print("毒への耐性がついた気がする！");
#else
			msg_print("You feel resistant to poison!");
#endif
			sound(SOUND_RES_POIS);
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

	/* Redraw status bar*/
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

	/* Knocked out */
	if (p_ptr->stun > 100)
	{
		old_aux = 3;
	}

	/* Heavy stun */
	else if (p_ptr->stun > 50)
	{
		old_aux = 2;
	}

	/* Stun */
	else if (p_ptr->stun > 0)
	{
		old_aux = 1;
	}

	/* None */
	else
	{
		old_aux = 0;
	}

	/* Knocked out */
	if (v > 100)
	{
		new_aux = 3;
	}

	/* Heavy stun */
	else if (v > 50)
	{
		new_aux = 2;
	}

	/* Stun */
	else if (v > 0)
	{
		new_aux = 1;
	}

	/* None */
	else
	{
		new_aux = 0;
	}

	/* Increase cut */
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
			sound(SOUND_STUN);
			break;

			/* Heavy stun */
			case 2:
#ifdef JP
			msg_print("意識がひどくもうろうとしてきた。");
#else
			msg_print("You have been heavily stunned.");
#endif
			sound(SOUND_STUN);
			break;

			/* Knocked out */
			case 3:
#ifdef JP
			msg_print("頭がクラクラして意識が遠のいてきた。");
#else
			msg_print("You have been knocked out.");
#endif
			sound(SOUND_STUN);
			break;
		}

		/* 
		 * XXX XXX Hack -
		 * Mindcrafters cannot get this effect when
		 * casting a spell.  It really doesn't make sense.
		 * Unfortunately, there is no way to know if this is
		 * the case... so it is disabled in all circumstances
		 * if you are a Mindcrafter.  (Perhaps it can be
		 * explained away by their "superior mental skills" or
		 * something...
		 */
		if (randint1(1000) < v || randint1(16) == 1)
		{
#ifdef JP
			msg_print("割れるような頭痛がする。");
#else
			msg_print("A vicious blow hits your head.");
#endif

			if (randint1(3) == 1)
			{
				if (!p_ptr->sustain_int) (void)do_dec_stat(A_INT);
				if (!p_ptr->sustain_wis) (void)do_dec_stat(A_WIS);
			}
			else if (randint1(2) == 1)
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

	/* Decrease cut */
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
			sound(SOUND_RECOVER);

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

	/* Redraw the "stun" */
	p_ptr->redraw |= (PR_STUN);

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

	/* Mortal wound */
	if (p_ptr->cut > 1000)
	{
		old_aux = 7;
	}

	/* Deep gash */
	else if (p_ptr->cut > 200)
	{
		old_aux = 6;
	}

	/* Severe cut */
	else if (p_ptr->cut > 100)
	{
		old_aux = 5;
	}

	/* Nasty cut */
	else if (p_ptr->cut > 50)
	{
		old_aux = 4;
	}

	/* Bad cut */
	else if (p_ptr->cut > 25)
	{
		old_aux = 3;
	}

	/* Light cut */
	else if (p_ptr->cut > 10)
	{
		old_aux = 2;
	}

	/* Graze */
	else if (p_ptr->cut > 0)
	{
		old_aux = 1;
	}

	/* None */
	else
	{
		old_aux = 0;
	}

	/* Mortal wound */
	if (v > 1000)
	{
		new_aux = 7;
	}

	/* Deep gash */
	else if (v > 200)
	{
		new_aux = 6;
	}

	/* Severe cut */
	else if (v > 100)
	{
		new_aux = 5;
	}

	/* Nasty cut */
	else if (v > 50)
	{
		new_aux = 4;
	}

	/* Bad cut */
	else if (v > 25)
	{
		new_aux = 3;
	}

	/* Light cut */
	else if (v > 10)
	{
		new_aux = 2;
	}

	/* Graze */
	else if (v > 0)
	{
		new_aux = 1;
	}

	/* None */
	else
	{
		new_aux = 0;
	}

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
			sound(SOUND_CUT);
			break;

			/* Light cut */
			case 2:
#ifdef JP
			msg_print("軽い傷を負ってしまった。");
#else
			msg_print("You have been given a light cut.");
#endif
			sound(SOUND_CUT);
			break;

			/* Bad cut */
			case 3:
#ifdef JP
			msg_print("ひどい傷を負ってしまった。");
#else
			msg_print("You have been given a bad cut.");
#endif
			sound(SOUND_CUT);
			break;

			/* Nasty cut */
			case 4:
#ifdef JP
			msg_print("大変な傷を負ってしまった。");
#else
			msg_print("You have been given a nasty cut.");
#endif
			sound(SOUND_CUT);
			break;

			/* Severe cut */
			case 5:
#ifdef JP
			msg_print("重大な傷を負ってしまった。");
#else
			msg_print("You have been given a severe cut.");
#endif
			sound(SOUND_CUT);
			break;

			/* Deep gash */
			case 6:
#ifdef JP
			msg_print("ひどい深手を負ってしまった。");
#else
			msg_print("You have been given a deep gash.");
#endif
			sound(SOUND_CUT);
			break;

			/* Mortal wound */
			case 7:
#ifdef JP
			msg_print("致命的な傷を負ってしまった。");
#else
			msg_print("You have been given a mortal wound.");
#endif
			sound(SOUND_CUT);
			break;
		}

		/* Notice */
		notice = TRUE;

		if (randint1(1000) < v || randint1(16) == 1)
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
			msg_print("やっと出血が止まった。");
#else
			msg_print("You are no longer bleeding.");
#endif
			sound(SOUND_RECOVER);
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

	/* Redraw the "cut" */
	p_ptr->redraw |= (PR_CUT);

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
	int old_aux, new_aux;

	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 20000) ? 20000 : (v < 0) ? 0 : v;

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
			sound(SOUND_EAT);
			break;

			/* Hungry */
			case 2:
#ifdef JP
			msg_print("まだ空腹だ。");
#else
			msg_print("You are still hungry.");
#endif
			sound(SOUND_EAT);
			break;

			/* Normal */
			case 3:
#ifdef JP
			msg_print("空腹感がおさまった。");
#else
			msg_print("You are no longer hungry.");
#endif
			sound(SOUND_EAT);
			break;

			/* Full */
			case 4:
#ifdef JP
			msg_print("満腹だ！");
#else
			msg_print("You are full!");
#endif
			sound(SOUND_EAT);
			break;

			/* Bloated */
			case 5:
#ifdef JP
			msg_print("食べ過ぎだ！");
#else
			msg_print("You have gorged yourself!");
#endif
			sound(SOUND_EAT);
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
			sound(SOUND_HUNGRY);
			break;

			/* Weak */
			case 1:
#ifdef JP
			msg_print("お腹が空いて倒れそうだ。");
#else
			msg_print("You are getting weak from hunger!");
#endif
			sound(SOUND_HUNGRY);
			break;

			/* Hungry */
			case 2:
#ifdef JP
			msg_print("お腹が空いてきた。");
#else
			msg_print("You are getting hungry.");
#endif
			sound(SOUND_HUNGRY);
			break;

			/* Normal */
			case 3:
#ifdef JP
			msg_print("満腹感がなくなった。");
#else
			msg_print("You are no longer full.");
#endif
			sound(SOUND_NOTICE);

			break;

			/* Full */
			case 4:
#ifdef JP
			msg_print("やっとお腹がきつくなくなった。");
#else
			msg_print("You are no longer gorged.");
#endif
			sound(SOUND_NOTICE);

			break;
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

	/* Cannot go above 18/100 */
	if (value < 18+100)
	{
		/* Gain one (sometimes two) points */
		if (value < 18)
		{
			gain = ((randint0(100) < 75) ? 1 : ((value < 17) ? 2 : 11));
			value += gain;
		}

		/* Gain 1/6 to 1/3 of distance to 18/100 */
		else if (value < 18+98)
		{
			/* Approximate gain value */
			gain = (((18+100) - value) / 2 + 3) / 2;

			/* Paranoia */
			if (gain < 1) gain = 1;

			/* Apply the bonus */
			value += randint1(gain) + gain / 2;

			/* Maximal value */
			if (value > 18+99) value = 18 + 99;
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
		}

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
			sound(SOUND_RECOVER); /* (Sound substitute) No sound for heal, use recover */
		}

		/* Heal 5-14 */
		else if (num < 15)
		{
#ifdef JP
			msg_print("気分が良くなった。");
#else
			msg_print("You feel better.");
#endif
			sound(SOUND_RECOVER);
		}

		/* Heal 15-34 */
		else if (num < 35)
		{
#ifdef JP
			msg_print("とても気分が良くなった。");
#else
			msg_print("You feel much better.");
#endif
			sound(SOUND_RECOVER);
		}

		/* Heal 35+ */
		else
		{
#ifdef JP
			msg_print("ひじょうに気分が良くなった。");
#else
			msg_print("You feel very good.");
#endif
			sound(SOUND_RECOVER); /* (Sound substitute) No sound for X_heal, use recover */
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
		sound(SOUND_DRAIN_STAT);

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
		sound(SOUND_RECOVER);

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
		sound(SOUND_RECOVER);

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
	/* Restore experience */
	if (p_ptr->exp < p_ptr->max_exp)
	{
		/* Message */
#ifdef JP
		msg_print("生命力が戻ってきた気がする。");
#else
		msg_print("You feel your life energies returning.");
#endif
		sound(SOUND_RECOVER);

		/* Restore the experience */
		p_ptr->exp = p_ptr->max_exp;

		/* Check the experience */
		check_experience();

		/* Did something */
		return (TRUE);
	}

	/* No effect */
	return (FALSE);
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
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER | PW_STATS);

	/* Mega-Hack -- Forget the map */
	wiz_dark();

	/* It worked */
	return (TRUE);
}


void do_poly_wounds(void)
{
	/* Changed to always provide at least _some_ healing */
	s16b wounds = p_ptr->cut;
	s16b hit_p = (p_ptr->mhp - p_ptr->chp);
	s16b change = damroll(p_ptr->lev, 5);
	bool Nasty_effect = (randint1(5) == 1);

	if (!(wounds || hit_p || Nasty_effect)) return;

#ifdef JP
	msg_print("傷がより軽いものに変化した。");
#else
	msg_print("Your wounds are polymorphed into less serious ones.");
#endif
	sound(SOUND_RECOVER);

	hp_player(change);
	if (Nasty_effect)
	{
#ifdef JP
		msg_print("新たな傷ができた！");
		take_hit(change / 2, "変化した傷");
#else
		msg_print("A new wound was created!");
		take_hit(change / 2, "a polymorphed wound");
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


#if 0
	if ((power > randint0(20)) && (randint0(3) == 1))
	{
		char effect_msg[80] = "";
		int new_race, expfact, goalexpfact;
		int old_race = p_ptr->prace;

		/* Some form of racial polymorph... */
		power -= 10;

		if ((power > randint0(5)) && (randint0(4) == 1))
		{
			/* sex change */
			power -= 2;

			if (p_ptr->psex == SEX_MALE)
			{
				p_ptr->psex = SEX_FEMALE;
				sp_ptr = &sex_info[p_ptr->psex];
#ifdef JP
				sprintf(effect_msg, "女性の");
#else
				sprintf(effect_msg, "female ");
#endif

			}
			else
			{
				p_ptr->psex = SEX_MALE;
				sp_ptr = &sex_info[p_ptr->psex];
#ifdef JP
				sprintf(effect_msg, "男性の");
#else
				sprintf(effect_msg, "male ");
#endif

			}
		}

		if ((power > randint0(30)) && (randint0(5) == 1))
		{
			int tmp = 0;

			/* Harmful deformity */
			power -= 15;

			while (tmp < 6)
			{
				if (randint0(2) == 1)
				{
					(void)dec_stat(tmp, randint1(6) + 6, (randint1(3) == 1));
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
				sprintf(tmp_msg, "%s ", effect_msg);
				sprintf(effect_msg, "deformed %s ", tmp_msg);
#endif

			}
			else
			{
#ifdef JP
				sprintf(effect_msg,"奇形の");
#else
				sprintf(effect_msg,"deformed ");
#endif

			}
		}

		while ((power > randint0(20)) && (randint0(10) == 1))
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

		/*
		 * Restrict the race choices by exp penalty so
		 * weak polymorph always means weak race
		 */
		if (power < 0)
			goalexpfact = 100;
		else
			goalexpfact = 100 + 3 * randint0(power);

		do
		{
			new_race = randint0(MAX_RACES);
			expfact = race_info[new_race].r_exp;
		}
		while ((new_race == p_ptr->prace) && (expfact > goalexpfact));

		if (!effect_msg[0])
		{

#ifdef JP
			msg_format("あなたは%sに変化した！",
				race_info[new_race].title);
#else
			msg_format("You turn into a%s %s!",
			    (((new_race == RACE_DUNADAN) ||
			      (new_race == RACE_ELF) ||
			      (new_race == RACE_IMP)) ? "n" : ""),
				race_info[new_race].title);
#endif

		}
		else
		{


#ifdef JP
			msg_format("あなたは%s%sに変化した！", effect_msg,
				race_info[new_race].title);
#else
			msg_format("You turn into a %s%s!", effect_msg,
				race_info[new_race].title);
#endif

		}

		p_ptr->prace = new_race;
		rp_ptr = &race_info[p_ptr->prace];

		/* Experience factor */
		p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

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

		check_experience();
		p_ptr->max_plv = p_ptr->lev;

		p_ptr->redraw |= (PR_BASIC);

		p_ptr->update |= (PU_BONUS);

		handle_stuff();

		/* Load an autopick preference file */
		if (old_race != p_ptr->prace) autopick_load_pref(FALSE);

		lite_spot(py, px);
	}
#endif /* #if 0 */

	if ((power > randint0(30)) && (randint0(6) == 1))
	{
		int tmp = 0;

		/* Abomination! */
		power -= 20;

#ifdef JP
		msg_print("内臓の構成が変化した！");
#else
		msg_print("Your internal organs are rearranged!");
#endif

		while (tmp < 6)
		{
			(void)dec_stat(tmp, randint1(6) + 6, (randint1(3) == 1));
			tmp++;
		}
		if (randint1(6) == 1)
		{
#ifdef JP
			msg_print("現在姿で生きていくのは困難なようだ！");
			take_hit(damroll(randint1(10), p_ptr->lev), "致命的な突然変異");
#else
			msg_print("You find living difficult in your present form!");
			take_hit(damroll(randint1(10), p_ptr->lev), "a lethal mutation");
#endif

			power -= 10;
		}
	}

	if ((power > randint0(20)) && (randint0(4) == 1))
	{
		power -= 10;

		do_cmd_rerate(TRUE);
	}

	while ((power > randint0(15)) && (randint0(3) == 1))
	{
		power -= 7;
		(void)gain_random_mutation(0);
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
 * Decreases players hit points and sets death flag if necessary
 *
 * XXX XXX XXX Invulnerability needs to be changed into a "shield"
 *
 * XXX XXX XXX Hack -- this function allows the user to save (or quit)
 * the game when he dies, since the "You die." message is shown before
 * setting the player to "dead".
 */
int take_hit(int damage, cptr hit_from)
{
	int old_chp = p_ptr->chp;
	bool pen_invuln = FALSE;
	char death_message[1024];
	int warning = (p_ptr->mhp * hitpoint_warn / 10);

	/* Paranoia */
	if (death) return 0;

	/* Disturb */
	disturb(1, 0);

	/* Mega-Hack -- Apply "invulnerability" */
	if (p_ptr->invuln && (damage < 9000))
	{
		if (randint1(PENETRATE_INVULNERABILITY) == 1)
		{
			pen_invuln = TRUE;
		}
		else
		{
			return 0;
		}
	}

	if (p_ptr->wraith_form)
	{
		damage /= 3;
		if ((damage == 0) && (randint1(10) == 1)) damage = 1;
	}

	/* Hurt the player */
	p_ptr->chp -= damage;

	/* Display the hitpoints */
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	/* Do not skip the message */
	if (stop_more) p_ptr->skip_more = FALSE;

	if (pen_invuln)
#ifdef JP
		msg_print("無敵のバリアを破って攻撃された！");
#else
		msg_print("The attack penetrates your shield of invulnerability!");
#endif

	/* Dead player */
	if (p_ptr->chp < 0)
	{
		/* Sound */
		sound(SOUND_DEATH);

		/* Note cause of death */
		(void)strcpy(died_from, hit_from);

		if (p_ptr->image) strcat(died_from,"(?)");

		/* No longer a winner */
		total_winner = FALSE;

		/* Leaving */
		p_ptr->leaving = TRUE;

		/* Note death */
		death = TRUE;

		handle_stuff();

		/* Flush input */
		flush();

#ifdef JP
		if (get_check("画面を保存しますか？"))
#else
		if (get_check("Dump the screen? "))
#endif
		{
			do_cmd_save_screen();
		}

		/* Flush input */
		flush();
		p_ptr->skip_more = FALSE;

		/* Hack -- Note death */
		if (!last_words)
		{
#ifdef JP
			msg_print("あなたは死にました。");
#else
			msg_print("You die.");
#endif
			sound(SOUND_DEATH);
			msg_print(NULL);
		}
		else
		{
#ifdef JP
			get_rnd_line_jonly("death_j.txt", 0, death_message, 10);
			
			if (!get_string("断末魔の叫び: ", death_message, 1024))
			{
				msg_print("あなたは死にました。");
			}
			else
#else
			if (!get_rnd_line("death.txt", 0, death_message))
#endif
				msg_print(death_message);

			sound(SOUND_DEATH);
			msg_print(NULL);
		}

		/* Dead */
		return (damage);
	}

	/* Hitpoint warning */
	if (p_ptr->chp < warning)
	{
		/* Hack -- bell on first notice */
		if (alert_hitpoint && (old_chp > warning)) bell();

		sound(SOUND_HITPOINT_WARN);

		/* Message */
#ifdef JP
		msg_print("*** 警告:低ヒット・ポイント！ ***");
#else
		msg_print("*** LOW HITPOINT WARNING! ***");
#endif

		/* Flush input to force player to aware of the danger */
		flush();
		msg_print(NULL);
	}

	return (damage);
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
		/* Gain max experience (20%) (was 10%) */
		p_ptr->max_exp += amount / 5;
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
