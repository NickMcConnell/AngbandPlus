/* File: xtra2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "cmds.h"


/*** Timed effects ***/

/*
 * This code replace a lot of virtually identical functions and (ostensibly)
 * is a lot cleaner.  Note that the various "oppose" functions and the stun
 * and cut statuses need to be handled by special functions of their own,
 * as they are more complex than the ones handled by the generic code.  -AS-
 */
static bool set_oppose_acid(int v);
static bool set_oppose_elec(int v);
static bool set_oppose_fire(int v);
static bool set_oppose_cold(int v);
static bool set_stun(int v);
static bool set_cut(int v);
static bool set_shortboost(int v);


typedef struct
{
  const char *on_begin, *on_end;
  u32b flag_redraw, flag_window, flag_update;
  int msg;
} timed_effect;

static timed_effect effects[] =
{
	{ "You feel yourself moving faster!", "You feel yourself slow down.", 0, 0, PU_BONUS, MSG_SPEED }, /* TMD_FAST */
	{ "You feel yourself moving slower!", "You feel yourself speed up.", 0, 0, PU_BONUS, MSG_SLOW }, /* TMD_SLOW */
	{ "You are blind.", "You can see again.", (PR_MAP | PR_BLIND), (PW_OVERHEAD | PW_MAP | PW_OBJLIST), 
		(PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS), MSG_BLIND }, /* TMD_BLIND */
	{ "You are paralyzed!", "You can move again.", PR_STATE, 0, 0, MSG_PARALYZED }, /* TMD_PARALYZED */
	{ "You are confused!", "You feel less confused now.", PR_CONFUSED, 0, 0, MSG_CONFUSED }, /* TMD_CONFUSED */
	{ "You are terrified!", "You feel bolder now.", PR_AFRAID, 0, 0, MSG_AFRAID }, /* TMD_AFRAID */
	{ "You feel drugged!", "You can see clearly again.", PR_MAP, (PW_OVERHEAD | PW_MAP), 0, MSG_DRUGGED }, /* TMD_IMAGE */
	{ "You are poisoned!", "You are no longer poisoned.", PR_POISONED, 0, 0, MSG_POISONED }, /* TMD_POISONED */
	{ "", "", 0, 0, 0, 0 },  /* TMD_CUT -- handled seperately */
	{ "", "", 0, 0, 0, 0 },  /* TMD_STUN -- handled seperately */
	{ "You feel safe from evil!", "You no longer feel safe from evil.", 0, 0, 0, MSG_PROT_EVIL }, /* TMD_PROTEVIL */
	{ "You feel invulnerable!", "You feel vulnerable once more.", 0, 0, PU_BONUS, MSG_INVULN }, /* TMD_INVULN */
	{ "You feel like a hero!", "The heroism wears off.", 0, 0, PU_BONUS, MSG_HERO }, /* TMD_HERO */
	{ "You feel like a killing machine!", "You feel less Berserk.", 0, 0, PU_BONUS, MSG_BERSERK }, /* TMD_SHERO */
	{ "A powerful mystic shield forms around your body!", "Your mystic shield crumbles away.", 0, 0, PU_BONUS, MSG_SHIELD }, /* TMD_SHIELD */
	{ "You feel righteous!", "The prayer has expired.", 0, 0, PU_BONUS, MSG_BLESSED }, /* TMD_BLESSED */
	{ "Your eyes feel very sensitive!", "Your eyes feel less sensitive.", 0, 0, (PU_BONUS | PU_MONSTERS), MSG_SEE_INVIS }, /* TMD_SINVIS */
	{ "You feel very alert.", "You no longer feel extra alert.", 0, 0, (PU_BONUS | PU_MONSTERS), MSG_INFRARED }, /* TMD_SINFRA */
	{ "", "", 0, 0, 0, 0 },  /* TMD_OPP_ACID -- handled seperately */
	{ "", "", 0, 0, 0, 0 },  /* TMD_OPP_ELEC -- handled seperately */
	{ "", "", 0, 0, 0, 0 },  /* TMD_OPP_FIRE -- handled seperately */
	{ "", "", 0, 0, 0, 0 },  /* TMD_OPP_COLD -- handled seperately */
	{ "You feel resistant to poison!", "You feel less resistant to poison", PR_OPPOSE_ELEMENTS, 0, 0, MSG_RES_POIS }, /* TMD_OPP_POIS */
	{ "You feel your memories fade.", "Your memories come flooding back.", PR_CONFUSED, 0, 0, MSG_GENERIC }, /* TMD_AMNESIA */
	{ "With a smile, you decide you're not in the mood for fighting anymore.", "Your smile fades and you feel violent again.", PR_AFRAID, 0, 0, MSG_AFRAID }, /* TMD_CHARM */
	{ "You are more than irritated.. you burst into a careless frenzy!", "You calm down from your careless frenzy.", 0, 0, PU_BONUS, MSG_BERSERK }, /* TMD_FRENZY */
	{ "You begin to see what's really there.", "Your sight returns to normal.", 0, 0, (PU_TORCH | PU_BONUS | PU_MONSTERS), MSG_SEE_INVIS }, /* TMD_TSIGHT */
	{ "You strike at evil with holy wrath.", "The holy wrath wears off.", 0, 0, PU_BONUS, MSG_HERO }, /* TMD_SANCTIFY */
	{ "You feel safe from powerful evil!", "You no longer feel safe from evil.", 0, 0, 0, MSG_PROT_EVIL }, /* TMD_PROTEVIL2 */
	{ "You begin to sense other minds.", "You cease to sense other minds.", 0, 0, (PU_BONUS | PU_MONSTERS), MSG_SEE_INVIS }, /* TMD_ESP */
	{ "A mystic shield forms around your body!", "Your mystic shield crumbles away.", 0, 0, PU_BONUS, MSG_SHIELD }, /* TMD_WSHIELD */
	{ "You slip into the shadows.", "The shadows don't conceal you so much anymore.", 0, 0, PU_BONUS, MSG_GENERIC }, /* TMD_SHADOW */
	{ "You feel yourself moving at a different speed.", "You move at more normal speed now.", 0, 0, PU_BONUS, MSG_SPEED }, /* TMD_ADJUST */
	{ "You magic is enhanced and you can read runes with your hands!", "Your magic no longer seems more powerful than normal.", PR_BLIND, 0, PU_BONUS, MSG_SEE_INVIS }, /* TMD_BRAIL */
	{ "Your skin becomes like stone and you move slower.", "The stoneskin wears off.", 0, 0, PU_BONUS, MSG_SLOW }, /* TMD_STONESKIN */
	{ "You become desperate to escape!", "You feel bolder now.", PR_AFRAID, 0, PU_BONUS, MSG_AFRAID }, /* TMD_TERROR */
	{ "", "", (PR_MAP | PR_BLIND), (PW_OVERHEAD | PW_MAP | PW_OBJLIST),
	  (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_BONUS), MSG_BLIND }, /* TMD_MESP: (mind sight) telepathy only while blind (no message) */
	{ "You feel slightly resistant to poison.", "You feel less resistant to poison.", PR_OPPOSE_ELEMENTS, 0, PU_BONUS, MSG_RES_POIS }, /* TMD_WOPP_POIS */
	{ "You feel resistant to nether forces!", "You feel less resistant to nether.", PR_OPPOSE_ELEMENTS, 0, PU_BONUS, MSG_RES_POIS }, /* TMD_OPP_NETHR */
	{ "You feel safe from lifeless monsters!", "You no longer feel safe from lifeless monsters.", 0, 0, 0, MSG_PROT_EVIL }, /* TMD_PROTDEAD */
	{ "You feel resistant to darkness!", "You feel less resistant to darkness.", PR_OPPOSE_ELEMENTS, 0, PU_BONUS, MSG_RES_POIS }, /* TMD_OPP_DARK */
	{ "You feel your life force is protected!", "You no longer feel your life force is protected.", PR_OPPOSE_ELEMENTS, 0, PU_BONUS, MSG_RES_POIS }, /* TMD_HOLDLIFE */
	{ "You feel the spirit of the balrog in your blows!", "The spirit of the balrog leaves you.", 0, 0, PU_BONUS, MSG_PROT_EVIL }, /* TMD_BALROG */
	{ "You feel completely immune to fire!", "You no longer feel immune to fire.", PR_OPPOSE_ELEMENTS, 0, 0, MSG_RES_FIRE }, /* TMD_IMM_FIRE */
	{ "You stop breathing but continue to live as an undead.", "The lifebreath returns to you.", PR_OPPOSE_ELEMENTS, 0, PU_BONUS, MSG_RES_POIS }, /* TMD_BECOME_LICH */
	{ "", "", 0, 0, (PU_BONUS | PU_MONSTERS), MSG_INFRARED }, /* TMD_WSINFRA (no message) */
	{ "You sense demons stirring", "You no longer aggravate demons.", 0, 0, 0, MSG_GENERIC }, /* TMD_WITCH */
	{ "You gain extra speed in melee!", "Your melee is back to normal speed.", 0, 0, PU_BONUS, MSG_GENERIC }, /* TMD_XATTACK */
	{ "You feel like nothing can slow you down!", "Your speed is no longer sustained.", 0, 0, PU_BONUS, MSG_GENERIC }, /* TMD_SUST_SPEED */
	{ "A sphere of green light surrounds you!", "The sphere of charm dissapears.", 0, 0, 0, MSG_GENERIC }, /* TMD_SPHERE_CHARM */
	{ "", "", 0, 0, (PU_BONUS | PU_MONSTERS), MSG_GENERIC }, /* TMD_MDETECTION (no message) */
	{ "You can see monsters without light!", "You can no longer see without light.", PR_OPPOSE_ELEMENTS, 0, (PU_BONUS | PU_MONSTERS), MSG_GENERIC }, /* TMD_DARKVIS */
	{ "You feel very sneaky.", "You no longer feel especially sneaky.", 0, 0, PU_BONUS, MSG_HERO }, /* TMD_SUPER_ROGUE */
	{ "Are electrical field surrounds you.", "The electric field dissapates.", PR_BLIND, 0, PU_BONUS, MSG_GENERIC }, /* TMD_ZAPPING */
	{ "You have first sight and second thoughts.", "Your sight returns to normal", PR_BLIND, 0, (PU_BONUS | PU_MONSTERS), MSG_INFRARED }, /* TMD_2ND_THOUGHT */
	{ "Your magical shield protects against monster breath.", "The breath shield dissapears.", 0, 0, PU_BONUS, MSG_GENERIC }, /* TMD_BR_SHIELD */
	{ "Daylight surrounds you.", "The daylight enchantment expires and the shadows return.", 0, 0, (PU_TORCH | PU_BONUS | PU_MONSTERS), MSG_GENERIC }, /* TMD_DAYLIGHT */
	{ "You feel resistant to confusion!", "You no longer feel resistant to confusion.", PR_OPPOSE_ELEMENTS, 0, PU_BONUS, MSG_GENERIC }, /* TMD_CLEAR_MIND */
	{ "You feel forsaken.", "The curse wears off.", 0, 0, PU_BONUS, MSG_GENERIC }, /* TMD_CURSE */
	{ "You feel very skillfull!", "The skill boost wears off.", 0, 0, PU_BONUS, MSG_BLESSED }, /* TMD_SKILLFUL */
	{ "You have the throwing strength of a giant.", "You feel like you just shrunk back to your usual size.", 0, 0, PU_BONUS, MSG_BLESSED }, /* TMD_MIGHTY_HURL */
	{ "", "", PR_STATE, 0, PU_BONUS, MSG_PARALYZED }, /* TMD_BEAR_HOLD (messages are elseware, attack: RBE_BHOLD) */
	{ "a faint glow surrounds your quiver.", "your quiver is no longer protected.", 0, 0, PU_BONUS, MSG_GENERIC }, /* TMD_QUIVERGUARD */
	{ "You begin to give off light.", "You no longer give off light.", 0, 0, (PU_TORCH | PU_BONUS | PU_MONSTERS), MSG_GENERIC }, /* TMD_MINDLIGHT */
	{ "You feel resistant to silver magic.", "You are no longer resistant to silver magic.", PR_OPPOSE_ELEMENTS, 0, PU_BONUS, MSG_GENERIC }, /* TMD_OPP_SILV */
	{ "You feel unnaturally tough.", "You no longer feel more tough than usual.", 0, 0, PU_BONUS, MSG_GENERIC }, /* TMD_FALSE_LIFE */
	{ "You begin to give off a disgusting smell.", "You no longer smell any worse than usual..", 0, 0, PU_BONUS, MSG_GENERIC }, /* TMD_STINKY */
	{ "You make a symbol to ward off demons.", "The demon-warding symbol dissapears.", 0, 0, PU_BONUS, MSG_GENERIC }, /* TMD_DEMON_WARD */
	{ "", "", 0, 0, 0, 0 },  /* TMD_TMPBOOST -- handled seperately */
	{ "Your aim is especially good.", "The sniper's eye effect has worn off.", 0, 0, (PU_BONUS | PU_MONSTERS), MSG_GENERIC }, /* TMD_SNIPER */
	{ "An outside force is trying to control your body!", "You regain control.", 0, 0, PU_BONUS, MSG_GENERIC }, /* TMD_MIND_CONTROL (unused) */
	{ "You inventory is protected from acid.", "You inventory is no longer protected.", 0, 0, PU_BONUS, MSG_GENERIC }, /* TMD_ACID_BLOCK */
	{ "You step into darkness.", "You are no longer cloaked in darkness.", 0, 0, (PU_TORCH | PU_BONUS | PU_MONSTERS), MSG_GENERIC }, /* TMD_DARKSTEP */
};

/*
 * Set a timed event (except timed resists, cutting and stunning).
 */
bool set_timed(int idx, int v)
{
	bool notice = FALSE;
	timed_effect *effect;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;
	if ((idx < 0) || (idx > TMD_MAX)) return FALSE;

	/* Hack -- call other functions */
	if (idx == TMD_STUN) return set_stun(v);
	else if (idx == TMD_CUT) return set_cut(v);
	else if (idx == TMD_OPP_ACID) return set_oppose_acid(v);
	else if (idx == TMD_OPP_ELEC) return set_oppose_elec(v);
	else if (idx == TMD_OPP_FIRE) return set_oppose_fire(v);
	else if (idx == TMD_OPP_COLD) return set_oppose_cold(v);
	else if (idx == TMD_TMPBOOST) return set_shortboost(v);

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
			/* hack: reset speed adjustment */
			if (idx == TMD_ADJUST) p_ptr->spadjust = 0;
		}
	}

	/* Use the value */
	p_ptr->timed[idx] = v;

	/* Nothing to notice */
	if (!notice) return FALSE;

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Update the visuals, as appropriate. */
	p_ptr->update |= effect->flag_update;
	p_ptr->redraw |= effect->flag_redraw;
	p_ptr->window |= effect->flag_window;

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return TRUE;
}

bool inc_timed(int idx, int v)
{
	/* Check we have a valid effect */
	if ((idx < 0) || (idx > TMD_MAX)) return FALSE;

	/* Set v */
	v = v + p_ptr->timed[idx];

	return set_timed(idx, v);
}

bool dec_timed(int idx, int v)
{
	/* Check we have a valid effect */
	if ((idx < 0) || (idx > TMD_MAX)) return FALSE;

	/* Set v */
	v = p_ptr->timed[idx] - v;

	return set_timed(idx, v);
}

bool clear_timed(int idx)
{
	return set_timed(idx, 0);
}


/*
 * Set "p_ptr->timed[TMD_OPP_ACID]", notice observable changes
 */
static bool set_oppose_acid(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->timed[TMD_OPP_ACID] && !p_ptr->immune_acid)
		{
			message(MSG_RES_ACID, 0, "You feel resistant to acid!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->timed[TMD_OPP_ACID] && !p_ptr->immune_acid)
		{
			message(MSG_RECOVER, 0, "You feel less resistant to acid.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->timed[TMD_OPP_ACID] = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw */
	p_ptr->redraw |= PR_OPPOSE_ELEMENTS;

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->timed[TMD_OPP_ELEC]", notice observable changes
 */
static bool set_oppose_elec(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->timed[TMD_OPP_ELEC] && !p_ptr->immune_elec)
		{
			message(MSG_RES_ELEC, 0, "You feel resistant to electricity!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->timed[TMD_OPP_ELEC] && !p_ptr->immune_elec)
		{
			message(MSG_RECOVER, 0, "You feel less resistant to electricity.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->timed[TMD_OPP_ELEC] = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw */
	p_ptr->redraw |= PR_OPPOSE_ELEMENTS;

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->timed[TMD_OPP_FIRE]", notice observable changes
 */
static bool set_oppose_fire(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->timed[TMD_OPP_FIRE] && !p_ptr->immune_fire)
		{
			message(MSG_RES_FIRE, 0, "You feel resistant to fire!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->timed[TMD_OPP_FIRE] && !p_ptr->immune_fire)
		{
			message(MSG_RECOVER, 0, "You feel less resistant to fire.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->timed[TMD_OPP_FIRE] = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw */
	p_ptr->redraw |= PR_OPPOSE_ELEMENTS;

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->timed[TMD_OPP_COLD]", notice observable changes
 */
static bool set_oppose_cold(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->timed[TMD_OPP_COLD] && !p_ptr->immune_cold)
		{
			message(MSG_RES_COLD, 0, "You feel resistant to cold!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->timed[TMD_OPP_COLD] && !p_ptr->immune_cold)
		{
			message(MSG_RECOVER, 0, "You feel less resistant to cold.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->timed[TMD_OPP_COLD] = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw */
	p_ptr->redraw |= PR_OPPOSE_ELEMENTS;

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->timed[TMD_TMPBOOST]", notice observable changes
 * This actually raises a stat by 4 when the effect starts
 * and lowers it by 4 when the effect ends.
 */
static bool set_shortboost(int v)
{
	bool notice = FALSE;
	int wstat;
	cptr act;

	/* Hack -- Force good values */
	v = (v > 2000) ? 2000 : (v < 0) ? 0 : v;

	if (p_ptr->timed[TMD_TMPBOOST])
	{
		/* decrease same stat we increased earlier */
		wstat = p_ptr->see_infra;
	}
	else if (v)
	{
		/* strong casters get their spell stat increased */
		/* (unless it's already very high) */
		if ((cp_ptr->spell_first) && (cp_ptr->spell_first < 4))
		{
			if (cp_ptr->spell_stat == 1)  wstat = A_INT;
		    if (cp_ptr->spell_stat == 2)  wstat = A_WIS;
		}
		/* archers get dexterity increased */
		else if (cp_ptr->flags & CF_EXTRA_SHOT)
		{
			wstat = A_DEX;
		}
		/* others get strength  */
		else wstat = A_STR;

		/* strength is second choice if it isn't the first */
		if ((wstat > A_STR) && (p_ptr->stat_cur[wstat] > 18+70))
		{
			wstat = A_STR;
		}

		/* CON is the next choice */
		if (p_ptr->stat_cur[wstat] > 18+70)
		{
			wstat = A_CON;
		}

		/* dex is next choice for non-archers */
		if ((p_ptr->stat_cur[wstat] > 18+70) &&
			(!(cp_ptr->flags & CF_EXTRA_SHOT)))
		{
			wstat = A_DEX;
		}

		/* spell stat is next choice if not a strong caster */
		if ((p_ptr->stat_cur[wstat] > 18+70) &&
			(cp_ptr->spell_first >= 4))
		{
			if (cp_ptr->spell_stat == 1)  wstat = A_INT;
		    if (cp_ptr->spell_stat == 2)  wstat = A_WIS;
		}

		/* charisma is last choice */
		if (p_ptr->stat_cur[wstat] > 18+70)
		{
			wstat = A_CHR;
		}

		/* fail if all choices are very high */
		if ((v) && (p_ptr->stat_cur[wstat] > 18+70)) v = 0;
	}

	if (wstat == A_STR) act = "strong";
	if (wstat == A_DEX) act = "agile";
	if (wstat == A_CON) act = "healthy";
	if (wstat == A_INT) act = "bright";
	if (wstat == A_WIS) act = "wise";
	if (wstat == A_CHR) act = "attractive";

	/* Open */
	if (v)
	{
		if (!p_ptr->timed[TMD_TMPBOOST])
		{
			if (inc_stat(wstat, 4))
			{
				msg_format("You feel very %s!", act);
				notice = TRUE;
				/* remember which stat to decrease when effect expires */
				p_ptr->see_infra = wstat;
			}
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->timed[TMD_TMPBOOST])
		{
			if (dec_set_stat(wstat, 4))
			{
				msg_format("You're no longer any more %s than usual.", act);
				notice = TRUE;
			}
		}
	}

	/* Use the value */
	p_ptr->timed[TMD_TMPBOOST] = v;

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
 * Set "p_ptr->timed[TMD_STUN]", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
static bool set_stun(int v)
{
	int old_aux, new_aux;

	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Knocked out */
	if (p_ptr->timed[TMD_STUN] > 100)
	{
		old_aux = 3;
	}

	/* Heavy stun */
	else if (p_ptr->timed[TMD_STUN] > 50)
	{
		old_aux = 2;
	}

	/* Stun */
	else if (p_ptr->timed[TMD_STUN] > 0)
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
			{
				message(MSG_STUN, 0, "You have been stunned.");
				break;
			}

			/* Heavy stun */
			case 2:
			{
				message(MSG_STUN, 0, "You have been heavily stunned.");
				break;
			}

			/* Knocked out */
			case 3:
			{
				message(MSG_STUN, 0, "You have been knocked out.");
				break;
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
			{
				message(MSG_RECOVER, 0, "You are no longer stunned.");
				if (disturb_state) disturb(0, 0);
				break;
			}
		}

		/* Notice */
		notice = TRUE;
	}

	/* Use the value */
	p_ptr->timed[TMD_STUN] = v;

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
 * Set "p_ptr->timed[TMD_CUT]", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
static bool set_cut(int v)
{
	int old_aux, new_aux;

	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Mortal wound */
	if (p_ptr->timed[TMD_CUT] > 1000)
	{
		old_aux = 7;
	}

	/* Deep gash */
	else if (p_ptr->timed[TMD_CUT] > 200)
	{
		old_aux = 6;
	}

	/* Severe cut */
	else if (p_ptr->timed[TMD_CUT] > 100)
	{
		old_aux = 5;
	}

	/* Nasty cut */
	else if (p_ptr->timed[TMD_CUT] > 50)
	{
		old_aux = 4;
	}

	/* Bad cut */
	else if (p_ptr->timed[TMD_CUT] > 25)
	{
		old_aux = 3;
	}

	/* Light cut */
	else if (p_ptr->timed[TMD_CUT] > 10)
	{
		old_aux = 2;
	}

	/* Graze */
	else if (p_ptr->timed[TMD_CUT] > 0)
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
			{
				message(MSG_CUT, 0, "You have been given a graze.");
				break;
			}

			/* Light cut */
			case 2:
			{
				message(MSG_CUT, 0, "You have been given a light cut.");
				break;
			}

			/* Bad cut */
			case 3:
			{
				message(MSG_CUT, 0, "You have been given a bad cut.");
				break;
			}

			/* Nasty cut */
			case 4:
			{
				message(MSG_CUT, 0, "You have been given a nasty cut.");
				break;
			}

			/* Severe cut */
			case 5:
			{
				message(MSG_CUT, 0, "You have been given a severe cut.");
				break;
			}

			/* Deep gash */
			case 6:
			{
				message(MSG_CUT, 0, "You have been given a deep gash.");
				break;
			}

			/* Mortal wound */
			case 7:
			{
				message(MSG_CUT, 0, "You have been given a mortal wound.");
				break;
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
			{
				message(MSG_RECOVER, 0, "You are no longer bleeding.");
				if (disturb_state) disturb(0, 0);
				break;
			}
		}

		/* Notice */
		notice = TRUE;
	}

	/* Use the value */
	p_ptr->timed[TMD_CUT] = v;

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

	/* food is irrevelent to a golem */
	if (p_ptr->prace == 16) return FALSE;

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
			{
				msg_print("You are still weak.");
				break;
			}

			/* Hungry */
			case 2:
			{
				msg_print("You are still hungry.");
				break;
			}

			/* Normal */
			case 3:
			{
				msg_print("You are no longer hungry.");
				break;
			}

			/* Full */
			case 4:
			{
				msg_print("You are full!");
				break;
			}

			/* Bloated */
			case 5:
			{
				msg_print("You have gorged yourself!");
				break;
			}
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
			{
				sound(MSG_NOTICE);
				msg_print("You are getting faint from hunger!");
				break;
			}

			/* Weak */
			case 1:
			{
				sound(MSG_NOTICE);
				msg_print("You are getting weak from hunger!");
				break;
			}

			/* Hungry */
			case 2:
			{
				sound(MSG_HUNGRY);
				msg_print("You are getting hungry.");
				break;
			}

			/* Normal */
			case 3:
			{
				sound(MSG_NOTICE);
				msg_print("You are no longer full.");
				break;
			}

			/* Full */
			case 4:
			{
				sound(MSG_NOTICE);
				msg_print("You are no longer gorged.");
				break;
			}
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

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

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
		if (p_ptr->lev > p_ptr->max_lev)
        {
            p_ptr->max_lev = p_ptr->lev;

#ifdef yes_c_history
			/* If auto-note taking enabled, write a note to the file every 5th level. */
            if ((p_ptr->lev % 5) == 0)
			{
                char buf[120];

                /* Build the message */
                sprintf(buf, "Reached level %d", p_ptr->lev);

                /* Write message */
                do_cmd_note(buf,  p_ptr->depth, FALSE);
           	}
#endif
        }

		/* Message */
		message_format(MSG_LEVEL, p_ptr->lev, "Welcome to level %d.", p_ptr->lev);
		
		/* golems heal on levelup (because they don't regenerate naturally) */
		if (p_ptr->prace == 16)
		{
			int maxhp = p_ptr->mhp;
			int ammmmt = maxhp/5;
			if (p_ptr->timed[TMD_FALSE_LIFE]) maxhp += 2 * (p_ptr->lev + 10);

			if (p_ptr->chp < maxhp) p_ptr->chp += maxhp/5;
			else ammmmt = 0;

			/* Enforce maximum */
			if (p_ptr->chp >= maxhp)
			{
				ammmmt -= (p_ptr->chp - maxhp + 1);
				p_ptr->chp = maxhp;
				p_ptr->chp_frac = 0;
			}

			/* Redraw */
			p_ptr->redraw |= (PR_HP);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
			
			if (ammmmt > 0)
			{
	            /* Heal 0-6 */
				if (ammmmt < 7) msg_print("You feel a little better.");
				/* Heal 7-27 */
				else if (ammmmt < 28) msg_print("You feel better.");
				/* Heal 28+ */
				else msg_print("You feel much better.");
        	}
		}

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE | PR_EXP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

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

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

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
		if (p_ptr->hold_life) p_ptr->max_exp += amount / (14 + (goodluck+1)/2);
		else p_ptr->max_exp += amount / (10 + (goodluck+1)/2);
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
static int get_coin_type(const monster_race *r_ptr)
{
	cptr name = (r_name + r_ptr->name);

	/* Analyze "coin" monsters */
	if ((r_ptr->d_char == '$') || (r_ptr->d_char == 'g'))
	{
		/* Look for textual clues */
		if (strstr(name, " copper ")) return (3);
		if (strstr(name, " silver ")) return (6);
		if (strstr(name, " gold ")) return (11);
		if (strstr(name, "gold ")) return (11);
		if (strstr(name, " mithril ")) return (18);
		if (strstr(name, " adamantite ")) return (19);
		if (strstr(name, " diamond")) return (15);

		/* Look for textual clues */
		if (strstr(name, "Copper ")) return (3);
		if (strstr(name, "Silver ")) return (6);
		if (strstr(name, "Gold ")) return (11);
		if (strstr(name, "Mithril ")) return (18);
		if (strstr(name, "Adamantite ")) return (19);
	}

	/* Assume nothing */
	return (0);
}


/*
 * Create magical stairs after finishing a quest monster.
 */
static void build_quest_stairs(int y, int x)
{
	int ny, nx;


	/* Stagger around */
	while (!cave_valid_bold(y, x))
	{
		int d = 1;

		/* Pick a location */
		scatter(&ny, &nx, y, x, d, 0);

		/* Stagger */
		y = ny; x = nx;
	}

	/* Destroy any objects */
	delete_object(y, x);

	/* Explain the staircase */
	msg_print("A magical staircase appears...");

	/* Create stairs down */
	cave_set_feat(y, x, FEAT_MORE);

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
 * 
 * If the monster has died before and come back to life, it only
 * drops any items it has picked up, it does not drop its treasure again.
 */
void monster_death(int m_idx)
{
	int i, j, y, x;

	int dump_item = 0;
	int dump_gold = 0;
	int gold_chance, ablah, howgood = 0;

	int number = 0;
	int total = 0;

	s16b this_o_idx, next_o_idx = 0;

	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool visible = (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)));

	bool good = (r_ptr->flags1 & (RF1_DROP_GOOD)) ? TRUE : FALSE;
	bool great = (r_ptr->flags1 & (RF1_DROP_GREAT)) ? TRUE : FALSE;

	bool do_gold = (!(r_ptr->flags1 & (RF1_ONLY_ITEM)));
	bool do_item = (!(r_ptr->flags1 & (RF1_ONLY_GOLD)));

	int force_coin = get_coin_type(r_ptr);

	object_type *i_ptr;
	object_type object_type_body;

	/* Get the location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Drop objects being carried */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Paranoia */
		o_ptr->held_m_idx = 0;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Copy the object */
		object_copy(i_ptr, o_ptr);

		/* Delete the object */
		delete_object_idx(this_o_idx);

		/* Drop it */
		drop_near(i_ptr, -1, y, x);
	}

	/* Forget objects */
	m_ptr->hold_o_idx = 0;

	/* Update monster list window */
	p_ptr->window |= PW_MONLIST;
	
	/* done if the monster has died before (already dropped its treasure) */
    if (m_ptr->ninelives) return;


	/* Mega-Hack -- drop "winner" treasures */
	if (r_ptr->flags1 & (RF1_DROP_CHOSEN))
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Mega-Hack -- Prepare to make "Grond" */
		object_prep(i_ptr, lookup_kind(TV_HAFTED, SV_GROND));

		/* Mega-Hack -- Mark this item as "Grond" */
		i_ptr->name1 = ART_GROND;

		/* Mega-Hack -- Actually create "Grond" */
		apply_magic(i_ptr, -1, TRUE, TRUE, TRUE);

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);


		/* Get local object */
		i_ptr = &object_type_body;

		/* Mega-Hack -- Prepare to make "Morgoth artifacts" */
		object_prep(i_ptr, lookup_kind(TV_CROWN, SV_MORGOTH));

		/* Mega-Hack -- Mark this item as "Morgoth" */
		i_ptr->name1 = ART_MORGOTH;

		/* Mega-Hack -- Actually create "Morgoth" */
		apply_magic(i_ptr, -1, TRUE, TRUE, TRUE);

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);
	}
	/* 6% chance for deep uniques to drop a treasure map */
	else if ((r_ptr->flags1 & (RF1_UNIQUE)) && (rand_int(100) < 6) && 
		(r_ptr->level >= 35))
	{
		/* Get object base type (tval 4, sval 2) */
		int k_idx = lookup_kind(TV_SPECIAL, SV_TREASURE);

		/* (allow for failure in case something happens to the object.txt entry) */
		if (k_idx)
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Create the item */
			object_prep(i_ptr, k_idx);

			/* Drop it in the dungeon */
			drop_near(i_ptr, -1, y, x);
		}
	}
	/* if we nerf the One Ring a bit more, we can make Gollum have a */
	/* chance to drop it */

	/* Determine how much we can drop */
	if ((r_ptr->flags1 & (RF1_DROP_30)) && (rand_int(100) < 33)) number++;
	if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 60)) number++;
	if ((r_ptr->flags1 & (RF1_DROP_90)) && (rand_int(100) < 90)) number++;
	if (r_ptr->flags1 & (RF1_DROP_1D2)) number += damroll(1, 2);
	if (r_ptr->flags1 & (RF1_DROP_2D2)) number += damroll(2, 2);
	if (r_ptr->flags1 & (RF1_DROP_3D2)) number += damroll(3, 2);
	if (r_ptr->flags1 & (RF1_DROP_4D2)) number += damroll(4, 2);

	/* Hack -- handle creeping coins */
	coin_type = force_coin;

	/* Average dungeon and monster levels */
	object_level = (p_ptr->depth + r_ptr->level) / 2;
	
	/* reward for OOD monsters */
	if (r_ptr->level > p_ptr->depth)
	{
		if (rand_int(100) < 40 + goodluck*2) object_level = r_ptr->level;
		else object_level = (p_ptr->depth + (r_ptr->level*2)) / 3;
	}
	else if (r_ptr->level < p_ptr->depth)
	{
		if (rand_int(100) < 10 + badluck*2) object_level = r_ptr->level;
	}
	
	/* sometimes vary the object level a bit more */
	if (!(p_ptr->depth == r_ptr->level))
	{
		int oltwo;
        if (p_ptr->depth < r_ptr->level)
			oltwo = p_ptr->depth + randint(r_ptr->level - p_ptr->depth);
		else
			oltwo = r_ptr->level + rand_int(p_ptr->depth - r_ptr->level + 1);
			
		if ((oltwo < object_level) && (rand_int(100) < 8 + badluck/2 - goodluck)) 
			object_level = oltwo;
		if ((oltwo > object_level) && (rand_int(100) < 8 + goodluck/2 - badluck)) 
			object_level = oltwo;
	}

	/* Drop some objects */
	for (j = 0; j < number; j++)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* gold chance if monster doesn't have ONLY_ITEM, also increase gold value */
		if (great)
		{
			gold_chance = 30;
			howgood = 2;
		}
		else if (good)
		{
			gold_chance = 40;
			howgood = 1;
		}
		else /* normal */
		{
			gold_chance = 50;
			howgood = 0;
		}

		/* Make Gold */
		if (do_gold && (!do_item || (rand_int(100) < gold_chance)))
		{
			/* Make some gold */
			if (!make_gold(i_ptr, howgood)) continue;

			/* Assume seen XXX XXX XXX */
			dump_gold++;
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
		drop_near(i_ptr, -1, y, x);
	}

	/* Reset the object level */
	object_level = p_ptr->depth;

	/* Reset "coin" type */
	coin_type = 0;


	/* Take note of any dropped treasure */
	if (visible && (dump_item || dump_gold))
	{
		/* Take notes on treasure */
		lore_treasure(m_idx, dump_item, dump_gold);
	}


	/* Only process "Quest Monsters" */
	if (!(r_ptr->flags1 & (RF1_QUESTOR))) return;


	/* Hack -- Mark quests as complete */
	for (i = 0; i < MAX_Q_IDX; i++)
	{
		/* Hack -- note completed quests */
		if (q_list[i].level == r_ptr->level) q_list[i].level = 0;

		/* Count incomplete quests */
		if (q_list[i].level) total++;
	}

	/* Build magical stairs */
	build_quest_stairs(y, x);

	/* Nothing left, game over... */
	if (total == 0)
	{
	    const char *player_name;
		/* Total winner */
		p_ptr->total_winner = TRUE;

		/* Redraw the "title" */
		p_ptr->redraw |= (PR_TITLE);

		/* Congratulations */
		msg_print("*** CONGRATULATIONS ***");
		msg_print("You have won the game!");
		msg_print("You may retire when you are ready.");

#ifdef yes_c_history
 		/* Write a note */
  		if (1)
 		{
	 		/* Variable for the date */
 			time_t ct = time((time_t*)0);
 			char long_day[25];
			fprintf(notes_file, "============================================================\n");
  		    (void)strftime(long_day, 25, "%m/%d/%Y at %I:%M %p", localtime(&ct));
			fprintf(notes_file, "%s slew Morgoth on %s.\n", op_ptr->full_name, long_day);
 			fprintf(notes_file, "Long live %s!\n", op_ptr->full_name);
 		    fprintf(notes_file, "Long live %s!\n", op_ptr->full_name);
			fprintf(notes_file, "============================================================\n");
      	}
#endif
	}
}

/* find out if a given monster is able to grab */
/* (grabbing monsters are much less likely to run away because of low HP) */
static bool itgrabs(int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	int ap_cnt;

	/* Scan through all blows */
	for (ap_cnt = 0; ap_cnt < MONSTER_BLOW_MAX; ap_cnt++)
	{
		int effect = r_ptr->blow[ap_cnt].effect;
		if (effect == RBE_BHOLD) return TRUE;
	}

	/* not a grabber */
	return FALSE;
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
bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	s32b div, new_exp, new_exp_frac, racexp;
	bool wakemon, seen, wasdisguised = FALSE;
	int estl, killscore;
	char m_name[80];
	char dm_name[80];

	/* undisguise mimmics */
	if ((m_ptr->disguised) && (player_can_see_bold(m_ptr->fy, m_ptr->fx)))
	{
		m_ptr->disguised = 0;
		m_ptr->ml = TRUE;
		wasdisguised = TRUE;
	}
	
	seen = ((m_ptr->ml) || (r_ptr->flags1 & RF1_UNIQUE));

	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

    /* Extract monster name */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Hurt it */
	m_ptr->hp -= dam;

	/* It is dead now */
	if (m_ptr->hp < 0)
	{
		/* Assume normal death sound */
		int soundfx = MSG_KILL;
		
		bool lottakills = FALSE;
		/* These are monsters that you are more likely to kill a lot of */
		if ((r_ptr->flags2 & (RF2_FRIEND1)) || (r_ptr->flags1 & (RF1_FRIENDS)) || 
			(r_ptr->flags2 & (RF2_MULTIPLY))) lottakills = TRUE;

		/* Uniques */
		if (r_ptr->flags1 & RF1_UNIQUE) 
		{
#ifdef yes_c_history
		    char note2[120];

			/* Extract monster name */
			monster_desc(dm_name, sizeof(dm_name), m_ptr, 0x80);

			/* Write note */
		    if ((r_ptr->flags3 & (RF3_UNDEAD)) || (r_ptr->flags3 & (RF3_NON_LIVING)) ||
		         (r_ptr->flags2 & (RF2_STUPID)))
		    {
       		    my_strcpy(note2, format("Destroyed %s", dm_name), sizeof (note2));
            }
			else
            {
                my_strcpy(note2, format("Killed %s", dm_name), sizeof (note2));
            }
 		    do_cmd_note(note2, p_ptr->depth, FALSE);
#endif

			/* Mega-Hack -- Morgoth -- see monster_death() */
			if (r_ptr->flags1 & RF1_DROP_CHOSEN)
				soundfx = MSG_KILL_KING;
			else /* Play a special sound if the monster was unique */
				soundfx = MSG_KILL_UNIQUE;
		}

		/* always the same death message for NONMONSTERs */
        if (r_ptr->flags7 & (RF7_NONMONSTER))
		{
			message_format(soundfx, m_ptr->r_idx, "You have destroyed %s.", m_name);
		}

        /* Death by Missile/Spell attack */
		else if (note)
		{
			message_format(soundfx, m_ptr->r_idx, "%^s%s", m_name, note);
		}

		/* Death by physical attack -- invisible monster */
		else if (!m_ptr->ml)
		{
			message_format(soundfx, m_ptr->r_idx, "You have killed %s.", m_name);
		}

		/* Death by Physical attack -- non-living monster */
		else if ((r_ptr->flags3 & (RF3_DEMON)) ||
		         (r_ptr->flags3 & (RF3_NON_LIVING)) ||
		         (r_ptr->flags2 & (RF2_STUPID)))
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
		/* racial xp award */
		racexp = r_ptr->mexp;
		
		/* out of depth non-unique monsters have their to-hit lowered */
		/* so lower their xp a little too */
		if ((p_ptr->depth < r_ptr->level - 8) && (!(r_ptr->flags1 & (RF1_UNIQUE))))
        {
            racexp = (racexp * 24) / 25; /* (only slightly) */
        }
		
		/* most TOWNOK versions of monsters aren't worth any experience */
		if ((!p_ptr->depth) && (r_ptr->flags1 & (RF1_UNIQUE))) racexp = racexp/2;
		else if ((!p_ptr->depth) && (r_ptr->level > 10)) racexp = racexp/3; /* reduced */
        else if ((!p_ptr->depth) && (r_ptr->level > 0)) racexp = 0;
        
		/* negative XP rewards should increase with clvl, not decrease */
        if (r_ptr->mexp < 0)
		{
			/* Make the negative positive so you can subtract it. */
			s32b losethis = 0 - ((long)r_ptr->mexp * (1 + (p_ptr->lev/8)));
			/* xp penalty for killing townspeople doesn't apply to */
			/* evil character classes (the only demons with a negative XP */
			/* are the ones called by the summon demonic aid spell). */
			if ((!(cp_ptr->spell_book == TV_DARK_BOOK)) ||
				(r_ptr->flags3 & (RF3_DEMON)))
			{
				lose_exp(losethis);
			}
		}
		else if (racexp)
		{
			/* Give some experience for the kill */
			new_exp = ((long)racexp * r_ptr->level) / div;

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
		}


		/*** new scoring ***/
		/* base score = monlevel/5 rounded up */
		killscore = (r_ptr->level+4)/5;

		/* compare to character level (part1) */
		if ((p_ptr->lev - 25 > r_ptr->level) && (killscore < 3)) killscore = 0;
		else if ((p_ptr->lev - 10 > r_ptr->level) && (killscore == 2)) killscore -= 1;
		else if (p_ptr->lev - 10 > r_ptr->level) killscore = (r_ptr->level+5)/6;
		
		/* THEME_ONLY monsters worth a little extra */
		if ((r_ptr->flags7 & (RF7_THEME_ONLY)) && (killscore < 8)) killscore += 2;
		else if (r_ptr->flags7 & (RF7_THEME_ONLY)) killscore = (killscore * 5) / 4;
		/* Sauron & Morgoth */
		if (r_ptr->flags1 & RF1_DROP_CHOSEN) killscore = 1000;
		else if (r_ptr->flags1 & (RF1_QUESTOR)) killscore = 300;
		/* other uniques */
		else if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->level < 5)) killscore = 5;
		else if (r_ptr->flags1 & (RF1_UNIQUE)) killscore = r_ptr->level + 1;

		/* Get extra score for killing the first monster of each race */
		/* (if not unique) */
		else if (!l_ptr->pkills)
		{
			if (r_ptr->flags7 & (RF7_THEME_ONLY)) killscore += r_ptr->rarity;
			else killscore += (r_ptr->rarity + 1) / 2;
		}
		/* reduced score for killing a lot of the same monster */
		else if ((l_ptr->pkills > 15) && (lottakills))
		{
             killscore -= (l_ptr->pkills - 12) / 4;
             /* If you kill enough of the same monster, they stop giving score */
             if (killscore < 0) killscore = 0;
		}
		else if ((l_ptr->pkills > 5) && (!lottakills))
		{
             killscore -= (l_ptr->pkills - 4) / 2;
             /* If you kill enough of the same monster, they stop giving score */
             if (killscore < 0) killscore = 0;
		}
		/* compare to character level (part2) */
		if (r_ptr->level > 100) killscore += 2;
		else if (p_ptr->lev * 2 < r_ptr->level) killscore = killscore * 3;
		else if (p_ptr->lev * 3 / 2 < r_ptr->level) killscore = killscore * 2;
		else if (p_ptr->lev < r_ptr->level) killscore = killscore * 3 / 2;
		
		/* no score for town monsters or ordinary trees */
		if (racexp < 1) killscore = 0;

		/* if it comes back to life, you have to kill it twice */
		/* to get full score */
		if (r_ptr->flags2 & (RF2_RETURNS)) killscore = killscore / 2;

		/* count score */
        p_ptr->game_score += killscore;

		/* Generate treasure */
		monster_death(m_idx);

		/* certain monsters have a chance to */
		/* raise your luck when you kill them */
		/* LUCKY_KILL should only be on certain rare monsters or uniques */
		if (r_ptr->flags7 & (RF7_LUCKY_KILL))
		{
			/* don't include any temporary luck effects */
			int gluck = ((p_ptr->luck > 20) ? (p_ptr->luck - 20) : 0);
			int bluck = ((p_ptr->luck < 20) ? (20 - p_ptr->luck) : 0);
			/* (harder to raise luck when luck is already high) */
			int luckchance = 11 - ((gluck+1)/3);
			luckchance += (bluck+1)/3;
			/* killing black cats not as likely */
			if (r_ptr->flags3 & (RF3_HURT_SILV)) luckchance -= 3;
			if (r_ptr->flags1 & (RF1_UNIQUE)) luckchance += 3;
			if (rand_int(100) < luckchance)
			{
				p_ptr->luck += 1;
				if (p_ptr->luck >= 20) msg_print("You feel lucky.");
				else if (r_ptr->flags3 & (RF3_HURT_SILV))
					msg_print("You feel your luck is avenged.");
				else msg_print("You feel less unlucky.");
			}
		}

		/* don't kill monsters who are trying to help you */
		if ((r_ptr->flags3 & (RF3_HELPER)) && (!m_ptr->evil))
		{
			/* don't include any temporary luck effects */
			int gluck = ((p_ptr->luck > 20) ? (p_ptr->luck - 20) : 0);
			int bluck = ((p_ptr->luck < 20) ? (20 - p_ptr->luck) : 0);
			/* (harder to lose luck when luck is already low) */
			int luckchance = 15 - ((bluck+1)/3);
			luckchance += (gluck+1)/3;
			if (rand_int(100) < luckchance)
			{
				p_ptr->luck -= 1;
				msg_print("Maybe that wasn't a good idea..");
			}
		}

		/* When the player kills a Unique, it stays dead */
		if (r_ptr->flags1 & (RF1_UNIQUE)) r_ptr->max_num = 0;

		/* if you've killed the monster before, don't increment the count */
		if (m_ptr->ninelives < 3)
		{
			/* Recall even invisible uniques or winners */
			if (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)))
			{
				/* Count kills this life */
				if (l_ptr->pkills < MAX_SHORT) l_ptr->pkills++;

				/* Count kills in all lives */
				if (l_ptr->tkills < MAX_SHORT) l_ptr->tkills++;

				/* Hack -- Auto-recall */
				monster_race_track(m_ptr->r_idx);
			}
			else if (!l_ptr->pkills)
			{
				/* always count the first kill, visible or not */
            	if (l_ptr->pkills < 1) l_ptr->pkills++;
			}
		}

		/* Delete the monster */
		delete_monster_idx(m_idx, TRUE);

		/* Not afraid */
		(*fear) = FALSE;

		/* Monster is dead */
		return (TRUE);
	}

	/* update visual of disguised monster (if not dead) */
	if (wasdisguised) lite_spot(m_ptr->fy, m_ptr->fx);

	/* effective stealth (lower the better) */
    estl = 108 - (p_ptr->skills[SKILL_STL] * 6);
    /* extremely high stealth still can wake the monster up */
    if (estl < 12) estl = 12;
    /* if it's awake it's more likely to notice you */
    if ((m_ptr->roaming) && (m_ptr->cdis < 3)) estl += 40;
	else if (m_ptr->roaming) estl += 20;
    /* if you hit but did (almost) no damage, */
	/* there's still a slight chance that the monster doesn't notice */
	/* especially if it's big and stupid */
	if ((r_ptr->flags2 & (RF2_STUPID)) && (m_ptr->hp > 1000)) estl += 10 + (2 * dam);
	else if (r_ptr->flags2 & (RF2_STUPID)) estl += 25 + (5 * dam);
	else if ((m_ptr->hp > 1000) && (dam < 12)) estl += 40 + (5 * dam);
    else if (dam < 5) estl += 50 + (10 * dam);
    wakemon = FALSE;

    if (dam >= 5) wakemon = TRUE;
	if (((r_ptr->flags2 & (RF2_STUPID)) || (m_ptr->hp > 1000)) && (dam < 12)) wakemon = FALSE;
	if (randint(100) < estl) wakemon = TRUE;
	if (estl > 50) estl = 50;
	/* rare ways of causing damage without the monster noticing */
	if (spellswitch == 23)
	{
		wakemon = FALSE;
		estl = estl/4;
	}
	else if (dam <= 0)
	{
		wakemon = FALSE;
		estl = estl/3;
	}
	if (estl >= m_ptr->csleep) wakemon = TRUE;

    /* disturb the monster (not automatic wakeup) */
    if (wakemon)
    {
        m_ptr->roaming = 0;
        m_ptr->csleep = 0;
    }
    else if (m_ptr->csleep) /* (this estl is always positive) */
    {
        m_ptr->csleep -= estl;
    }

	/* Mega-Hack -- Pain cancels fear */
	if (m_ptr->monfear && (dam > 0))
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
	/* never gets scared by damage after recovering from fear by running out of options */
	if (!m_ptr->monfear && !m_ptr->bold && !(r_ptr->flags3 & (RF3_NO_FEAR)) && (dam > 0))
	{
		int percentage, fleechance;
		bool grabber = itgrabs(m_idx);

		/* Percentage of fully healthy */
		percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

		/* mindless undead should never run away (unless turn undead is used) */
		/* (paranoia: they should all have NO_FEAR) */
		if ((r_ptr->flags3 & (RF3_UNDEAD)) && (r_ptr->flags2 & (RF2_STUPID))) return (FALSE);
		
		if (r_ptr->flags7 & (RF7_NONMONSTER)) return (FALSE);

		/* grabber monsters rarely run away from low HP */
		if (grabber) fleechance = 33;
		/* other undead less likely to flee */
		else if (r_ptr->flags3 & (RF3_UNDEAD)) fleechance = 50;
		else fleechance = 96;
		if (6 > percentage) fleechance += (6-percentage) * 4;

		/*
		 * Run (sometimes) if at 10% or less of max hit points,
		 * or (usually) when hit for half its current hit points
		 */
		if (((randint(10) >= percentage) && (rand_int(100) < fleechance)) ||
		    ((dam >= m_ptr->hp) && (rand_int(100) < fleechance - 16)))
		{
			/* Hack -- note fear */
			(*fear) = TRUE;

			/* Hack -- Add some timed fear */
			m_ptr->monfear = (randint(10) +
			                  (((dam >= m_ptr->hp) && (percentage > 7)) ?
			                   20 : ((11 - percentage) * 5)));

			/* monster lets go of the player when it becomes afraid */
			if (p_ptr->timed[TMD_BEAR_HOLD])
			{
				p_ptr->held_m_idx = 0;
				msg_print("You pull free.");
				clear_timed(TMD_BEAR_HOLD);
			}
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
		p_ptr->window |= (PW_OVERHEAD | PW_MAP);

		/* Changed */
		return (TRUE);
	}

	/* No change */
	return (FALSE);
}


/*
 * Perform the minimum "whole panel" adjustment to ensure that the given
 * location is contained inside the current panel, and return TRUE if any
 * such adjustment was performed.
 */
bool adjust_panel(int y, int x)
{
	bool changed = FALSE;

	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		int wx, wy;
		int screen_hgt, screen_wid;

		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if ((j > 0) && !(op_ptr->window_flag[j] & PW_MAP)) continue;

		wy = t->offset_y;
		wx = t->offset_x;

		screen_hgt = (j == 0) ? (Term->hgt - ROW_MAP - 1) : t->hgt;
		screen_wid = (j == 0) ? (Term->wid - COL_MAP - 1) : t->wid;

		/* Bigtile panels only have half the width */
		if (use_bigtile) screen_wid = screen_wid / 2;

		/* Adjust as needed */
		while (y >= wy + screen_hgt) wy += screen_hgt / 2;
		while (y < wy) wy -= screen_hgt / 2;

		/* Adjust as needed */
		while (x >= wx + screen_wid) wx += screen_wid / 2;
		while (x < wx) wx -= screen_wid / 2;

		/* Use "modify_panel" */
		if (modify_panel(t, wy, wx)) changed = TRUE;
	}

	return (changed);
}


/*
 * Change the current panel to the panel lying in the given direction.
 *
 * Return TRUE if the panel was changed.
 */
bool change_panel(int dir)
{
	bool changed = FALSE;
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		int screen_hgt, screen_wid;
		int wx, wy;

		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if ((j > 0) && !(op_ptr->window_flag[j] & PW_MAP)) continue;

		screen_hgt = (j == 0) ? (Term->hgt - ROW_MAP - 1) : t->hgt;
		screen_wid = (j == 0) ? (Term->wid - COL_MAP - 1) : t->wid;

		/* Bigtile panels only have half the width */
		if (use_bigtile) screen_wid = screen_wid / 2;

		/* Shift by half a panel */
		wy = t->offset_y + ddy[dir] * screen_hgt / 2;
		wx = t->offset_x + ddx[dir] * screen_wid / 2;

		/* Use "modify_panel" */
		if (modify_panel(t, wy, wx)) changed = TRUE;
	}

	return (changed);
}


/*
 * Verify the current panel (relative to the player location).
 *
 * By default, when the player gets "too close" to the edge of the current
 * panel, the map scrolls one panel in that direction so that the player
 * is no longer so close to the edge.
 *
 * The "center_player" option allows the current panel to always be centered
 * around the player, which is very expensive, and also has some interesting
 * gameplay ramifications.
 */
void verify_panel(void)
{
	int wy, wx;
	int screen_hgt, screen_wid;

	int panel_wid, panel_hgt;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if ((j > 0) && !(op_ptr->window_flag[j] & (PW_MAP))) continue;

		wy = t->offset_y;
		wx = t->offset_x;

		screen_hgt = (j == 0) ? (Term->hgt - ROW_MAP - 1) : t->hgt;
		screen_wid = (j == 0) ? (Term->wid - COL_MAP - 1) : t->wid;

		/* Bigtile panels only have half the width */
		if (use_bigtile) screen_wid = screen_wid / 2;

		panel_wid = screen_wid / 2;
		panel_hgt = screen_hgt / 2;


		/* Scroll screen vertically when off-center */
		if (center_player && !p_ptr->running && (py != wy + panel_hgt))
			wy = py - panel_hgt;

		/* Scroll screen vertically when 3 grids from top/bottom edge */
		else if ((py < wy + 3) || (py >= wy + screen_hgt - 3))
			wy = py - panel_hgt;


		/* Scroll screen horizontally when off-center */
		if (center_player && !p_ptr->running && (px != wx + panel_wid))
			wx = px - panel_wid;

		/* Scroll screen horizontally when 3 grids from left/right edge */
		else if ((px < wx + 3) || (px >= wx + screen_wid - 3))
			wx = px - panel_wid;


		/* Scroll if needed -- disturb on term 0 */
		if (j == 0)
		{
			if (modify_panel(t, wy, wx) && disturb_panel && !center_player)
				disturb(0, 0);
		}
		else
		{
			modify_panel(t, wy, wx);
		}
	}
}



/*
 * Monster health description
 */
static void look_mon_desc(char *buf, size_t max, int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool living = TRUE;

	/* Determine if the monster is "living" (vs "undead") */
	if (r_ptr->flags3 & (RF3_NON_LIVING)) living = FALSE;
	if (r_ptr->flags3 & (RF3_UNDEAD)) living = FALSE;

	/* Healthy monsters */
	if (m_ptr->hp >= m_ptr->maxhp)
	{
		/* No damage */
		my_strcpy(buf, (living ? "unhurt" : "undamaged"), max);
	}
	else
	{
		/* Calculate a health "percentage" */
		int perc = 100L * m_ptr->hp / m_ptr->maxhp;

		if (perc >= 60)
			my_strcpy(buf, (living ? "somewhat wounded" : "somewhat damaged"), max);
		else if (perc >= 25)
			my_strcpy(buf, (living ? "wounded" : "damaged"), max);
		else if (perc >= 10)
			my_strcpy(buf, (living ? "badly wounded" : "badly damaged"), max);
		else
			my_strcpy(buf, (living ? "almost dead" : "almost destroyed"), max);
	}

	/* status stuff is irrevelent for nonmonsters (like ordinary trees) */
	if (r_ptr->flags7 & (RF7_NONMONSTER)) return;

	if (m_ptr->monfear) my_strcat(buf, ", afraid", max);
	if ((m_ptr->truce) && (!m_ptr->csleep)) my_strcat(buf, ", peaceful", max);
	if ((m_ptr->csleep) && (m_ptr->roaming) && (m_ptr->roaming < 20)) my_strcat(buf, ", awake but hasn't noticed you", max);
	else if ((m_ptr->roaming) && (cheat_hear)) my_strcat(buf, ", temporarily roaming", max);
	else if (m_ptr->csleep) my_strcat(buf, ", asleep", max);
	if ((p_ptr->timed[TMD_BEAR_HOLD]) && (p_ptr->held_m_idx == m_idx)) my_strcat(buf, ", holding you", max);
	if (m_ptr->confused) my_strcat(buf, ", confused", max);
	if (m_ptr->stunned) my_strcat(buf, ", stunned", max);
	if (m_ptr->charmed) my_strcat(buf, ", charmed", max);
	if (m_ptr->mspeed > r_ptr->speed + 5)  my_strcat(buf, ", hasted", max);
	if (m_ptr->mspeed < r_ptr->speed - 5)  my_strcat(buf, ", slowed", max);
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
	cptr act;
	cptr s;


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
		/* Roguelike */
		if (rogue_like_commands)
		{
			mode = KEYMAP_MODE_ROGUE;
		}

		/* Original */
		else
		{
			mode = KEYMAP_MODE_ORIG;
		}

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
 * Determine if a monster makes a reasonable target
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
	int py = p_ptr->py;
	int px = p_ptr->px;

	monster_type *m_ptr;
	monster_race *r_ptr;

	/* No monster */
	if (m_idx <= 0) return (FALSE);

	/* Get monster */
	m_ptr = &mon_list[m_idx];
	r_ptr = &r_info[m_ptr->r_idx];

	/* Monster must be alive */
	if (!m_ptr->r_idx) return (FALSE);

	/* Monster must be visible */
	if ((!m_ptr->ml) && (!m_ptr->heard)) return (FALSE);
	
	/* You must know that it is a monster (for CHAR_MULTI) */
	if (m_ptr->disguised) return (FALSE);

	/* Monster must be projectable */
	if (!projectable(py, px, m_ptr->fy, m_ptr->fx)) return (FALSE);

	/* Hack -- no targeting hallucinations */
	if (p_ptr->timed[TMD_IMAGE]) return (FALSE);

	/* Don't target nonmonsters (like ordinary trees) */
	if (r_ptr->flags7 & (RF7_NONMONSTER)) return (FALSE);

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
		int m_idx = p_ptr->target_who;

		/* Accept reasonable targets */
		if (target_able(m_idx))
		{
			monster_type *m_ptr = &mon_list[m_idx];

			/* Get the monster location */
			p_ptr->target_row = m_ptr->fy;
			p_ptr->target_col = m_ptr->fx;

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
		monster_type *m_ptr = &mon_list[m_idx];

		/* Save target info */
		p_ptr->target_set = TRUE;
		p_ptr->target_who = m_idx;
		p_ptr->target_row = m_ptr->fy;
		p_ptr->target_col = m_ptr->fx;
	}

	/* Clear target */
	else
	{
		/* Reset target info */
		p_ptr->target_set = FALSE;
		p_ptr->target_who = 0;
		p_ptr->target_row = 0;
		p_ptr->target_col = 0;
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
		p_ptr->target_row = y;
		p_ptr->target_col = x;
	}

	/* Clear target */
	else
	{
		/* Reset target info */
		p_ptr->target_set = FALSE;
		p_ptr->target_who = 0;
		p_ptr->target_row = 0;
		p_ptr->target_col = 0;
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
	int py = p_ptr->py;
	int px = p_ptr->px;

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
static bool target_set_interactive_accept(int y, int x)
{
	object_type *o_ptr;


	/* Player grids are always interesting */
	if (cave_m_idx[y][x] < 0) return (TRUE);

	/* Handle hallucination */
	if (p_ptr->timed[TMD_IMAGE]) return (FALSE);

	/* Visible monsters */
	if (cave_m_idx[y][x] > 0)
	{
		monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		
		/* don't know that it's a monster */
		if (m_ptr->disguised)
		{
			/* monster disguised as whatever is underneath it */
			if ((strchr(".", r_ptr->d_char)) && 
				(r_ptr->flags1 & (RF1_CHAR_CLEAR))) /* skip */;
			/* monster disguised as a wall or floor (uninteresting) */
			else if (strchr("#%.", r_ptr->d_char)) return (FALSE);
			/* Rubble counts as interesting */
			if (strchr(":", r_ptr->d_char)) return (TRUE);
			/* else monster disguised as object (objects are interesting) */
			if ((m_ptr->meet == 100) || (player_can_see_bold(y, x))) return (TRUE);
			/* (TRUE if visible) */
			if ((cave_info[y][x] & (CAVE_MARK)) && (strchr("!,-_~=?$", r_ptr->d_char)))
				return (TRUE);
		}

		/* Visible monsters */
		if (m_ptr->ml) return (TRUE);

		/* heard monsters */
		if (m_ptr->heard) return (TRUE);
	}

	/* Scan all objects in the grid */
	for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
	{
		/* Memorized object */
		if (o_ptr->marked && !squelch_hide_item(o_ptr)) return (TRUE);
	}

	/* Interesting memorized features */
	if (cave_info[y][x] & (CAVE_MARK))
	{
		/* Notice glyphs */
		if (cave_feat[y][x] == FEAT_GLYPH) return (TRUE);

		/* Notice doors */
		if (cave_feat[y][x] == FEAT_OPEN) return (TRUE);
		if (cave_feat[y][x] == FEAT_BROKEN) return (TRUE);

		/* Notice stairs */
		if (cave_feat[y][x] == FEAT_LESS) return (TRUE);
		if (cave_feat[y][x] == FEAT_MORE) return (TRUE);

		/* Notice shops */
		if ((cave_feat[y][x] >= FEAT_SHOP_HEAD) &&
		    (cave_feat[y][x] <= FEAT_SHOP_TAIL)) return (TRUE);

		/* Notice traps */
		if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
		    (cave_feat[y][x] <= FEAT_TRAP_TAIL)) return (TRUE);

		/* Notice doors */
		if ((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
		    (cave_feat[y][x] <= FEAT_DOOR_TAIL)) return (TRUE);

		/* Notice misc features */
		if (cave_feat[y][x] == FEAT_RUBBLE) return (TRUE);
		if (cave_feat[y][x] == FEAT_OPEN_PIT) return (TRUE);
		if (cave_feat[y][x] == FEAT_WATER) return (TRUE);

		/* Notice veins with treasure */
		if (cave_feat[y][x] == FEAT_MAGMA_K) return (TRUE);
		if (cave_feat[y][x] == FEAT_QUARTZ_K) return (TRUE);
	}

	/* Nope */
	return (FALSE);
}


/*
 * Prepare the "temp" array for "target_interactive_set"
 *
 * Return the number of target_able monsters in the set.
 */
static void target_set_interactive_prepare(int mode)
{
	int y, x;

	/* Reset "temp" array */
	temp_n = 0;

	/* Scan the current panel */
	for (y = Term->offset_y; y < Term->offset_y + SCREEN_HGT; y++)
	{
		for (x = Term->offset_x; x < Term->offset_x + SCREEN_WID; x++)
		{
			/* Check bounds */
			if (!in_bounds_fully(y, x)) continue;

			/* Require "interesting" contents */
			if (!target_set_interactive_accept(y, x)) continue;

			/* Special mode */
			if (mode & (TARGET_ITEM))
			{
				/* Must contain an item */
				if (cave_o_idx[y][x] == 0) continue;
			}

			/* Special mode */
			else if (mode & (TARGET_KILL))
			{
				/* Must contain a monster */
				if (!(cave_m_idx[y][x] > 0)) continue;

				/* Must be a targettable monster */
			 	if (!target_able(cave_m_idx[y][x])) continue;
			}

			/* Save the location */
			temp_x[temp_n] = x;
			temp_y[temp_n] = y;
			temp_n++;
		}
	}

	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_distance;
	ang_sort_swap = ang_sort_swap_distance;

	/* Sort the positions */
	ang_sort(temp_x, temp_y, temp_n);
}


/*** had to copy this stuff from object1.c ***/
/*
 * Efficient version of '(T) += sprintf((T), "%c", (C))'
 */
#define object_desc_chr_macro(T,C) do { \
 \
	/* Copy the char */ \
	*(T)++ = (C); \
 \
} while (0)



/*
 * Efficient version of '(T) += sprintf((T), "%s", (S))'
 */
#define object_desc_str_macro(T,S) do { \
 \
	cptr s = (S); \
 \
	/* Copy the string */ \
	while (*s) *(T)++ = *s++; \
 \
} while (0)



/*
 * Efficient version of '(T) += sprintf((T), "%u", (N))'
 */
#define object_desc_num_macro(T,N) do { \
 \
	int n = (N); \
 \
	int p; \
 \
	/* Find "size" of "n" */ \
	for (p = 1; n >= p * 10; p = p * 10) /* loop */; \
 \
	/* Dump each digit */ \
	while (p >= 1) \
	{ \
		/* Dump the digit */ \
		*(T)++ = I2D(n / p); \
 \
		/* Remove the digit */ \
		n = n % p; \
 \
		/* Process next digit */ \
		p = p / 10; \
	} \
 \
} while (0)



/*
 * Efficient version of '(T) += sprintf((T), "%+d", (I))'
 */
#define object_desc_int_macro(T,I) do { \
 \
	int i = (I); \
 \
	/* Negative */ \
	if (i < 0) \
	{ \
		/* Take the absolute value */ \
		i = 0 - i; \
 \
		/* Use a "minus" sign */ \
		*(T)++ = '-'; \
	} \
 \
	/* Positive (or zero) */ \
	else \
	{ \
		/* Use a "plus" sign */ \
		*(T)++ = '+'; \
	} \
 \
	/* Dump the number itself */ \
	object_desc_num_macro(T, i); \
 \
} while (0)

/* 
 * Abridged version of object_desc for monsters disguised as objects.
 * This version cannot use o_ptr since there is no real object.
 *
 *   I removed any code used for artifacts, egos or IDed items since the
 * fake object that a monster is disguised as is never any of those things.
 *   XXX A player will be able to recognise a mimmic which appears as
 * a squelched kind.  I can't think of a good way to prevent this.
 *   I left in some code which isn't currently used to allow for possible
 * added mimmic monsters (like a sword mimmic or spellbook mimmic)
 *
 *   The only use for this function is when player (l)ooks at a monster
 * while it is disguised, so there's no need for the pref and mode parameters.
 */
void disguise_object_desc(char *buf, size_t max, int fkidx)
{
	cptr basenm;
	cptr modstr;

	int power, ftval, fsval, fpval, fnumber, wvar;
	bool aware, flavor;
	bool append_name = FALSE;

	char *b;
	char *t;

	cptr s;
	cptr u;
	cptr v;

	char p1 = '(', p2 = ')';
	char b1 = '[', b2 = ']';
	char c1 = '{', c2 = '}';

	char tmp_buf[128];

	/* get the fake object */
    object_kind *k_ptr = &k_info[fkidx];
	ftval = k_ptr->tval;
	fsval = k_ptr->sval;
	/* a weird variable (because we can't use randint or the player will catch on) */
	if (p_ptr->depth > 20) wvar = ((p_ptr->depth+1)/2 - (((int)(p_ptr->depth/20)) * 10));
	else wvar = p_ptr->depth / 4;
	if ((wvar > 8) && (p_ptr->depth < 80)) wvar = p_ptr->depth/10;
	else if (wvar > 8) wvar = p_ptr->depth/17;
	/* fake pval */
	if ((ftval == TV_WAND) || (ftval == TV_STAFF)) fpval = 3 + wvar;
	else fpval = 2;
	/* fake amount */
	if ((ftval == TV_SHOT) || (ftval == TV_BOLT) || (ftval == TV_ARROW))
		fnumber = 21 + (wvar * 3) / 2;
	else if (ftval == TV_SPIKE) fnumber = 11 + (wvar * 2);
	else fnumber = 1;

	/* See if the object is "aware" (these fake objects are never known) */
	if (k_info[fkidx].aware) aware = TRUE;
	else aware = FALSE;

	/* See if the object is flavored */
	flavor = (k_ptr->flavor ? TRUE : FALSE);

	/* Allow flavors to be hidden when aware */
	if (aware && !show_flavors) flavor = FALSE;

	/* Extract default "base" string */
	basenm = (k_name + k_ptr->name);

	/* Assume no "modifier" string */
	modstr = "";

	/* Analyze the object */
	switch (ftval)
	{
		/* Some objects are easy to describe */
		case TV_SKELETON:
		case TV_BOTTLE:
		case TV_JUNK:
		case TV_SPIKE:
		case TV_FLASK:
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		case TV_BOW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_SHIELD:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_LITE:
		case TV_CHEST:
		{
			break;
		}

		/* Amulets */
		case TV_AMULET:
		{
			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Amulet" : "& Amulet");

			break;
		}

		/* Rings */
		case TV_RING:
		{
			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Ring" : "& Ring");

			break;
		}

		/* Staffs */
		case TV_STAFF:
		{
			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Staff" : "& Staff");

			break;
		}

		/* Wands */
		case TV_WAND:
		{
			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Wand" : "& Wand");

			break;
		}

		/* Rods */
		case TV_ROD:
		{
			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Rod" : "& Rod");

			break;
		}

		/* Scrolls */
		case TV_SCROLL:
		{
            /* Color the object */
		    modstr = scroll_adj[fsval];
		    if (aware) append_name = TRUE;
		    basenm = (flavor ? "& Scroll titled \"#\"" : "& Scroll");
			break;
		}

		/* Potions */
		case TV_POTION:
		{
			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Potion" : "& Potion");
			break;
		}

		/* Food */
		case TV_FOOD:
		{
			/* Ordinary food is "boring" */
			if (fsval >= SV_FOOD_MIN_FOOD) break;

			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Mushroom" : "& Mushroom");

			break;
		}

		/* Magic Books */
		case TV_MAGIC_BOOK:
		{
			modstr = basenm;
			basenm = "& Book of Magic Spells #";
			break;
		}
		/* Prayer Books */
		case TV_PRAYER_BOOK:
		{
			modstr = basenm;
			basenm = "& Holy Book of Prayers #";
			break;
		}
		/* New school */
		case TV_NEWM_BOOK:
		{
			modstr = basenm;
			basenm = "& Book of Nature Magic #";
			break;
		}
		/* Chance realm */
		case TV_LUCK_BOOK:
		{
			modstr = basenm;
			basenm = "& Book of Chance Magic #";
			break;
		}
		/* Alchemy realm */
		case TV_CHEM_BOOK:
		{
			modstr = basenm;
			basenm = "& Book of Alchemy #";
			break;
		}
		/* Black Magic realm */
		case TV_DARK_BOOK:
		{
			modstr = basenm;
			basenm = "& Book of Black Magic #";
			break;
		}
#if 0 /* for later */
		/* Mind Magic realm */
		case TV_MIND_BOOK:
		{
			modstr = basenm;
			basenm = "& Book of Mind Powers #";
			break;
		}
#endif

		/* Hack -- Gold/Gems */
		case TV_GOLD:
		{
			my_strcpy(buf, basenm, max);
			return;
		}

		/* Hack -- Default */
		default:
		{
			my_strcpy(buf, "(nothing)", max);
			return;
		}
	}

	/* Start dumping the result */
	t = b = tmp_buf;

	/* Begin */
	s = basenm;

	/* Handle objects which sometimes use "a" or "an" */
	if (*s == '&')
	{
		/* Skip the ampersand and the following space */
		s += 2;

		if (fnumber > 1)
		{
			object_desc_num_macro(t, fnumber);
			object_desc_chr_macro(t, ' ');
		}

		/* Hack -- A single one, and next character will be a vowel */
		else if ((*s == '#') ? is_a_vowel(modstr[0]) : is_a_vowel(*s))
		{
			object_desc_str_macro(t, "an ");
		}

		/* A single one, and next character will be a non-vowel */
		else
		{
			object_desc_str_macro(t, "a ");
		}
	}

	/* Handle objects which never use "a" or "an" */
	else
	{
		/* Prefix a number if required */
		if (fnumber > 1)
		{
			object_desc_num_macro(t, fnumber);
			object_desc_chr_macro(t, ' ');
		}

		/* Hack -- A single item, so no prefix needed */
		else
		{
			/* Nothing */
		}
	}

	/* Copy the string */
	for (; *s; s++)
	{
		if (*s == '~')
		{
			/* Add a plural if needed */
			if (fnumber != 1)
			{
				char k = t[-1];

				/* Hack -- "Cutlass-es" and "Torch-es" */
				if ((k == 's') || (k == 'h')) *t++ = 'e';

				/* Add an 's' */
				*t++ = 's';
			}
		}

		/* Pluralizer for irregular plurals */
		else if (*s == '|')
		{
			bool singular = (fnumber == 1);

			/* Process singular part */
			for (s++; *s != '|'; s++)
			{
				if (singular) *t++ = *s;
			}

			/* Process plural part */
			for (s++; *s != '|'; s++)
			{
				if (!singular) *t++ = *s;
			}
		}

		else if (*s == '#')
		{
			/* Append the modifier */
			cptr m = (modstr);

			for (; *m; m++)
			{
				/* Handle pluralization in the modifier */
				if (*m != '|')
				{
					/* Normal character - copy */
					*t++ = *m;
				}
				else
				{
					/* Pluralizer */
					bool singular = (fnumber == 1);

					/* Process singular part */
					for (m++; *m != '|'; m++)
					{
						if (singular) *t++ = *m;
					}

					/* Process plural part */
					for (m++; *m != '|'; m++)
					{
						if (!singular) *t++ = *m;
					}
				}
			}
		}

		/* Normal */
		else
		{
			/* Copy */
			*t++ = *s;
		}
	}

	/* Append the "kind name" to the "base name" */
	if (append_name)
	{
		object_desc_str_macro(t, " of ");
		object_desc_str_macro(t, (k_name + k_ptr->name));
	}

	/* Dump base weapon info */
	switch (ftval)
	{
		/* Missiles */
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		{
			/* Fall through */
		}

		/* Weapons */
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		case TV_SKELETON: /* tusks */
		case TV_STAFF: /* staffs are weapons now */
		{
			/* Append a "damage" string */
			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, p1);
			object_desc_num_macro(t, k_ptr->dd);
			object_desc_chr_macro(t, 'd');
			object_desc_num_macro(t, k_ptr->ds);

			if (k_ptr->sbdd)
			{
				object_desc_chr_macro(t, '/');
				object_desc_num_macro(t, k_ptr->sbdd);
				object_desc_chr_macro(t, 'd');
				object_desc_num_macro(t, k_ptr->sbds);
			}
			object_desc_chr_macro(t, p2);

			/* All done */
			break;
		}

		/* Bows */
		case TV_BOW:
		{
/* ML for 'multiplier level' because actual mutlplier is wierd now */
			/* Hack -- Extract the "base power" */
			power = (fsval % 10);

			/* Append a "power" string */
			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, p1);
			object_desc_chr_macro(t, 'M');
			object_desc_chr_macro(t, 'L');
			object_desc_num_macro(t, power);
			object_desc_chr_macro(t, p2);

			/* All done */
			break;
		}
	}

	/* Fuelled light sources get number of remaining turns appended */
	if (ftval == TV_LITE)
	{
		s32b minute, hour, sec, toturns;
		int blah = p_ptr->depth;
		if (blah < 15) blah = blah * 9;
		else if (blah < 21) blah = blah * 7;
		else blah = blah * 3;
        int ftimeout = 820 + (p_ptr->depth * wvar * ((wvar+3)/2));

		/* timeout decrements every 10 game turns */
		toturns = ftimeout * 10;
		/* translate turns to time */
		if (toturns >= 60) minute = toturns / 60;
		else minute = 0;
		if (minute >= 60) hour = minute / 60;
		else hour = 0;
		if (toturns >= 60) sec = toturns - (minute * 60);
		else sec = toturns;
		if (minute >= 60) minute -= hour * 60;

		object_desc_str_macro(t, " (");
#if old
		/* Turns of light for normal lites */
		object_desc_num_macro(t, ftimeout);
		object_desc_str_macro(t, " turns)");
#else
		if (hour) object_desc_num_macro(t, hour);
		if (hour) object_desc_str_macro(t, ":");
		if (minute < 10) object_desc_str_macro(t, "0");
		if (minute) object_desc_num_macro(t, minute);
		object_desc_str_macro(t, ":");
		if (sec < 10) object_desc_str_macro(t, "0");
		object_desc_num_macro(t, sec);
		object_desc_str_macro(t, ")");
#endif
	}

	/* Hack -- Wands and Staffs have charges */
	/* EFGchange show charges if aware without id */
	if (aware && ((ftval == TV_STAFF) || (ftval == TV_WAND)))
	{
		/* Dump " (N charges)" */
		object_desc_chr_macro(t, ' ');
		object_desc_chr_macro(t, p1);
		object_desc_num_macro(t, fpval);
		object_desc_str_macro(t, " charge");
		if (fpval != 1)
		{
			object_desc_chr_macro(t, 's');
		}
		object_desc_chr_macro(t, p2);
	}

	/* Terminate */
	*t = '\0';

	/* Copy the string over */
	my_strcpy(buf, tmp_buf, max);
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
static event_type target_set_interactive_aux(int y, int x, int mode, cptr info)
{
	s16b this_o_idx = 0, next_o_idx = 0;
	char monaswall;
	bool monasobj = FALSE;

	cptr s1, s2, s3;

	bool boring;
	bool floored;

	int feat;

	int floor_list[MAX_FLOOR_STACK];
	int floor_num;

	event_type query;

	char out_val[256];


	/* Repeat forever */
	while (1)
	{
		/* Paranoia */
		query.key = ' ';

		/* Assume boring */
		boring = TRUE;

		/* Default */
		s1 = "You see ";
		s2 = "";
		s3 = "";


		/* The player */
		if (cave_m_idx[y][x] < 0)
		{
			/* Description */
			s1 = "You are ";

			/* Preposition */
			s2 = "on ";
		}


		/* Hack -- hallucination */
		if (p_ptr->timed[TMD_IMAGE])
		{
			cptr name = "something strange";

			/* Display a message */
			if (p_ptr->wizard)
			{
				strnfmt(out_val, sizeof(out_val),
				        "%s%s%s%s [%s] (%d:%d)", s1, s2, s3, name, info, y, x);
			}
			else
			{
				strnfmt(out_val, sizeof(out_val),
				        "%s%s%s%s [%s]", s1, s2, s3, name, info);
			}

			prt(out_val, 0, 0);
			move_cursor_relative(y, x);
			query = inkey_ex();

			/* Stop on everything but "return" */
			if ((query.key != '\n') && (query.key != '\r')) break;

			/* Repeat forever */
			continue;
		}

		/* monster disguised as a wall */
        if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			if (m_ptr->disguised)
			{
				if (strchr("#%:.", r_ptr->d_char))
				{
					monaswall = r_ptr->d_char;
				}
			}
		}

		/* Actual monsters */
		if ((cave_m_idx[y][x] > 0) && (!monaswall))
		{
			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			bool seedisguise = FALSE;
			if ((m_ptr->disguised) && (m_ptr->meet == 100)) seedisguise = TRUE;
			if ((m_ptr->disguised) && (player_can_see_bold(y, x))) seedisguise = TRUE;

			/* Visible (or heard or detected disguised monster) */
			if ((m_ptr->ml) || (m_ptr->heard) || (seedisguise))
			{
				bool recall = FALSE;
				cptr heartype;

				char m_name[80];

				/* Not boring */
				boring = FALSE;

				/* monster disguised as an object */
                if (m_ptr->disguised)
				{
			        monasobj = TRUE;
				}
                /* Get the monster name ("a kobold") */
				else if (m_ptr->ml)
				{
					monster_desc(m_name, sizeof(m_name), m_ptr, 0x08);

				    /* Hack -- track this monster race */
				    monster_race_track(m_ptr->r_idx);

				    /* Hack -- health bar for this monster */
				    health_track(cave_m_idx[y][x]);
				}
				/* Get the type name (not the exact race for unseen monsters) */
				else
				{
					heartype = get_hear_race(r_ptr);
					
					/* Change the intro */
					s1 = "You hear ";
				}

				/* Hack -- handle stuff */
				handle_stuff();

				/* Interact */
				while (1)
				{
					/* Recall */
					if ((recall) && (m_ptr->ml) && (!monasobj))
					{
						/* Save screen */
						screen_save();

                        /* Recall on screen */
                        /* include individual monster stuff */
						screen_roff(m_ptr->r_idx, m_ptr);

						/* Hack -- Complete the prompt (again) */
						Term_addstr(-1, TERM_WHITE, format("  [r,%s]", info));

						/* Command */
						query = inkey_ex();

						/* Load screen */
						screen_load();
					}

					/* monster in disguise */
                    else if (monasobj)
					{
						char o_name[80];
						/* get the description of the object that */
						/* the monster is disguised as */
						disguise_object_desc(o_name, sizeof(o_name), m_ptr->disguised);

    	                /* objects can now be in the same space as a trap */
    		            if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
				         (cave_feat[y][x] <= FEAT_TRAP_TAIL)) s3 = ", and a trap.";

						strnfmt(out_val, sizeof(out_val),
						        "%s%s%s [%s]%s", s1, s2, o_name, info, s3);

						prt(out_val, 0, 0);

						/* Place cursor */
						move_cursor_relative(y, x);

						/* Command */
						query = inkey_ex();
					}
					/* Normal */
					else
					{
						char buf[80];

						/* Describe the monster */
						look_mon_desc(buf, sizeof(buf), cave_m_idx[y][x]);

						/* Describe, and prompt for recall */
						if (p_ptr->wizard)
						{
							strnfmt(out_val, sizeof(out_val),
							        "%s%s%s%s (%s) [r,%s] (%d:%d)",
						            s1, s2, s3, m_name, buf, info, y, x);
						}
						else
						{
							if (m_ptr->ml)
							{
								strnfmt(out_val, sizeof(out_val),
							        "%s%s%s%s (%s) [r,%s]",
							        s1, s2, s3, m_name, buf, info);
							}
							else /* (no recall for unseen monsters) */
							{
								strnfmt(out_val, sizeof(out_val),
									"%s%s%s%s [%s]", s1, s2, s3, heartype, info);
							}
						}

						prt(out_val, 0, 0);

						/* Place cursor */
						move_cursor_relative(y, x);

						/* Command */
						query = inkey_ex();
					}

					/* Normal commands */
					if (query.key != 'r') break;

					/* Toggle recall */
					recall = !recall;
				}

				/* Stop on everything but "return"/"space" */
				if ((query.key != '\n') && (query.key != '\r') && (query.key != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query.key == ' ') && !(mode & (TARGET_LOOK))) break;

				/* don't show carried stuff for unseen or disguised monsters */
				if ((!m_ptr->ml) || (monasobj)) break;

				/* Change the intro */
				s1 = "It is ";

				/* Hack -- take account of gender */
				if (r_ptr->flags1 & (RF1_FEMALE)) s1 = "She is ";
				else if (r_ptr->flags1 & (RF1_MALE)) s1 = "He is ";

				/* Use a preposition */
				s2 = "carrying ";

				/* Scan all objects being carried */
				for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
				{
					char o_name[80];

					object_type *o_ptr;

					/* Get the object */
					o_ptr = &o_list[this_o_idx];

					/* Get the next object */
					next_o_idx = o_ptr->next_o_idx;

					/* Obtain an object description */
					object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

					/* Describe the object */
					if (p_ptr->wizard)
					{
						strnfmt(out_val, sizeof(out_val),
						        "%s%s%s%s [%s] (%d:%d)",
						        s1, s2, s3, o_name, info, y, x);
					}
					else
					{
						strnfmt(out_val, sizeof(out_val),
						        "%s%s%s%s [%s]", s1, s2, s3, o_name, info);
					}

					prt(out_val, 0, 0);
					move_cursor_relative(y, x);
					query = inkey_ex();

					/* Stop on everything but "return"/"space" */
					if ((query.key != '\n') && (query.key != '\r') && (query.key != ' ')) break;

					/* Sometimes stop at "space" key */
					if ((query.key == ' ') && !(mode & (TARGET_LOOK))) break;

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

		/* Scan all marked objects in the grid */
		if ((!monasobj) && ((scan_floor(floor_list, &floor_num, y, x, 3)) &&
		    (!(p_ptr->timed[TMD_BLIND]) || (y == p_ptr->py && x == p_ptr->px))))
		{
			/* Not boring */
			boring = FALSE;

			/* If there is more than one item... */
			if (floor_num > 1) while (1)
			{
				floored = TRUE;
				
				/* objects can now be in the same space as a trap */
                if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
		         (cave_feat[y][x] <= FEAT_TRAP_TAIL)) s3 = ", and a trap.";
		        else if (cave_feat[y][x] == FEAT_RUBBLE) s3 = " (on a pile of rubble)";

				/* Describe the pile */
				if (p_ptr->wizard)
				{
					strnfmt(out_val, sizeof(out_val),
					        "%s%sa pile of %d objects [r,%s] (%d:%d)%s",
					        s1, s2, floor_num, info, y, x, s3);
				}
				else
				{
					strnfmt(out_val, sizeof(out_val),
					        "%s%sa pile of %d objects [r,%s]%s",
					        s1, s2, floor_num, info, s3);
				}

				prt(out_val, 0, 0);
				move_cursor_relative(y, x);
				query = inkey_ex();

				/* Display objects */
				if (query.key == 'r')
				{
					/* Save screen */
					screen_save();

					/* Display */
					show_floor(floor_list, floor_num, TRUE);

					/* Describe the pile */
					prt(out_val, 0, 0);
					query = inkey_ex();

					/* Load screen */
					screen_load();

					/* Continue on 'r' only */
					if (query.key == 'r') continue;
				}

				/* Done */
				break;
			}
			/* Only one object to display */
			else
			{
				char o_name[80];

				/* Get the single object in the list */
				object_type *o_ptr = &o_list[floor_list[0]];

				/* Not boring */
				boring = FALSE;

				/* Obtain an object description */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);
				
				/* objects can now be in the same space as a trap */
                if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
		         (cave_feat[y][x] <= FEAT_TRAP_TAIL)) s3 = ", and a trap.";
		        else if (cave_feat[y][x] == FEAT_RUBBLE) s3 = " (on a pile of rubble)";

				/* Describe the object */
				if (p_ptr->wizard)
				{
					strnfmt(out_val, sizeof(out_val),
					        "%s%s%s [%s] (%d:%d)%s",
					        s1, s2, o_name, info, y, x, s3);
				}
				else
				{
					strnfmt(out_val, sizeof(out_val),
					        "%s%s%s [%s]%s", s1, s2, o_name, info, s3);
				}

				prt(out_val, 0, 0);
				move_cursor_relative(y, x);
				query = inkey_ex();

				/* Stop on everything but "return"/"space" */
				if ((query.key != '\n') && (query.key != '\r') && (query.key != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query.key == ' ') && !(mode & (TARGET_LOOK))) break;

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
		feat = f_info[cave_feat[y][x]].mimic;

		/* monster disguising as a wall */
		if (monaswall)
        {
			if (strchr("%", monaswall)) feat = FEAT_QUARTZ;
			else if (strchr(":", monaswall)) feat = FEAT_RUBBLE;
			else if (strchr(".", monaswall)) feat = FEAT_FLOOR;
			else feat = FEAT_WALL_INNER;
		}

		/* Require knowledge about grid, or ability to see grid */
		if (!(cave_info[y][x] & (CAVE_MARK)) && !player_can_see_bold(y,x))
		{
			/* Forget feature */
			feat = FEAT_NONE;
		}

		/* Terrain feature if needed */
		if (boring || (feat > FEAT_INVIS))
		{
			cptr name = f_name + f_info[feat].name;

			/* Hack -- handle unknown grids */
			if (feat == FEAT_NONE) name = "unknown grid";

			/* handle earthquake trap (don't call it a pit) */
			if ((feat == FEAT_TRAP_HEAD + 0x01) && (p_ptr->depth > 65))
				name = "earthquake trap";
				
			/* rogues, alchemists, and kobolds can get more detail about traps */
            if ((p_ptr->pclass == 3) || (p_ptr->pclass == 7) ||
				(p_ptr->prace == 10) || ((p_ptr->pclass == 15) && 
				(p_ptr->luck > (p_ptr->depth / 10) + 1)))
			{
				if (feat == FEAT_TRAP_HEAD + 0x02) name = "spiked pit";
				if (feat == FEAT_TRAP_HEAD + 0x03) name = "poisoned spiked pit";
				if (feat == FEAT_TRAP_HEAD + 0x08) name = "dart trap (slowing)";
				if (feat == FEAT_TRAP_HEAD + 0x09) name = "dart trap (strength drain)";
				if ((feat == FEAT_TRAP_HEAD + 0x0A) && (p_ptr->depth > 42))
					name = "dart trap (dexterity drain or mana drain)";
				else if (feat == FEAT_TRAP_HEAD + 0x0A) name = "dart trap (dexterity drain)";
				if (feat == FEAT_TRAP_HEAD + 0x0B) name = "dart trap (constitution drain)";
			}
				
			/* differenciate locked or jammed doors from unlocked doors */
            if ((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
			   (cave_feat[y][x] <= FEAT_DOOR_TAIL))
			{
               if (cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x08) name = "jammed door";
               else if (cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x01) name = "locked door";
            }

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
				        "%s%s%s%s [%s] (%d:%d)", s1, s2, s3, name, info, y, x);
			}
			else
			{
				strnfmt(out_val, sizeof(out_val),
				        "%s%s%s%s [%s]", s1, s2, s3, name, info);
			}

			prt(out_val, 0, 0);
			move_cursor_relative(y, x);
			query = inkey_ex();

			/* Stop on everything but "return"/"space" */
			if ((query.key != '\n') && (query.key != '\r') && (query.key != ' ')) break;
		}

		/* Stop on everything but "return" */
		if ((query.key != '\n') && (query.key != '\r')) break;
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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, d, m, t, bd;

	int y = py;
	int x = px;

	bool done = FALSE;

	bool flag = TRUE;

	event_type query;

	char info[80];

	/* don't target monsters for telekinsis or teleport control */
	if (spellswitch == 13) flag = FALSE;

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
			y = temp_y[m];
			x = temp_x[m];

            /* Allow target */
			if (((cave_m_idx[y][x] > 0) && target_able(cave_m_idx[y][x])) ||
			   (mode & (TARGET_ITEM)))
			{
				my_strcpy(info, "g,q,t,p,o,+,-,<dir>, <click>", sizeof(info));
			}

			/* Dis-allow target */
			else
			{
				my_strcpy(info, "g,q,p,o,+,-,<dir>, <click>", sizeof(info));
			}

			/* Adjust panel if needed */
			if (adjust_panel(y, x))
			{
				/* Handle stuff */
				handle_stuff();
			}

			/* Describe and Prompt */
			query = target_set_interactive_aux(y, x, mode, info);

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no "direction" */
			d = 0;

			/* Analyze */
			switch (query.key)
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
					if (++m == temp_n)
						m = 0;

					break;
				}

				case '-':
				{
					if (m-- == 0)
						m = temp_n - 1;

					break;
				}

				case 'p':
				{
					/* Recenter around player */
					verify_panel();

					/* Handle stuff */
					handle_stuff();

					y = py;
					x = px;
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

				case '\xff':
				{
					x = query.mousex + Term->offset_x;
					y = query.mousey + Term->offset_y;
					target_set_location(y, x);
					done = TRUE;
					break;
				}
				case 't':
				case '5':
				case '0':
				case '.':
				{
					if (mode & (TARGET_ITEM))
					{
                        if (cave_o_idx[y][x] == 0) bell("Illegal target!");
                        else
                        {
                            target_set_location(y, x);
                            done = TRUE;
                        }
                    }
                    else
                    {
                        int m_idx = cave_m_idx[y][x];

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
                    }
					break;
				}

				case 'g':
				{
					do_cmd_pathfind(y, x);
					done = TRUE;
					break;
				}

				default:
				{
					/* Extract direction */
					d = target_dir(query.key);

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
			my_strcpy(info, "g,q,t,p,m,+,-,<dir>, <click>", sizeof(info));

			/* Describe and Prompt (enable "TARGET_LOOK") */
			query = target_set_interactive_aux(y, x, mode | TARGET_LOOK, info);

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no direction */
			d = 0;

			/* Analyze the keypress */
			switch (query.key)
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
					verify_panel();

					/* Handle stuff */
					handle_stuff();

					y = py;
					x = px;
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
						t = distance(y, x, temp_y[i], temp_x[i]);

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

				case '\xff':
				{
					x = query.mousex + Term->offset_x;
					y = query.mousey + Term->offset_y;
				}
				case 't':
				case '5':
				case '0':
				case '.':
				{
					target_set_location(y, x);
					done = TRUE;
					break;
				}

				case 'g':
				{
					do_cmd_pathfind(y,x);
					done = TRUE;
					break;
				}

				default:
				{
					/* Extract a direction */
					d = target_dir(query.key);

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
				x += ddx[d];
				y += ddy[d];

				/* Slide into legality */
				if (x >= dungeon_wid - 1) x--;
				else if (x <= 0) x++;

				/* Slide into legality */
				if (y >= dungeon_hgt - 1) y--;
				else if (y <= 0) y++;

				/* Adjust panel if needed */
				if (adjust_panel(y, x))
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
	verify_panel();

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
	bool spot_target = FALSE;

	event_type ke;

	cptr p;

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

	/* spellswitch 24 is for telekinesis */
	/* spellswitch 13 is for teleport control */
	if ((spellswitch == 24) || (spellswitch == 13))
	{
       /* never use old target */
       dir = 0;
       spot_target = TRUE;
    }
    /* never use old target when throwing a non-weapon */
    else if (spellswitch == 12) dir = 0;
	/* Hack -- auto-target if requested */
	else if (use_old_target && target_okay()) dir = 5;

	/* Ask until satisfied */
/* 	while (!dir) */
	while (spot_target ? dir != 5 : !dir)
	{
		/* Choose a prompt */
		if (!target_okay())
		{
			p = "Direction ('*' or <click> to choose a target, Escape to cancel)? ";
		}
		else
		{
			p = "Direction ('5' or <click>for target, '*' to re-target, Escape to cancel)? ";
		}
        
        if (!get_com_ex(p, &ke)) break;

		/* Analyze */
		switch (ke.key)
		{
			/* Mouse aiming */
			case '\xff':
			{
				target_set_location(ke.mousey + Term->offset_y, ke.mousex + Term->offset_x);
				dir = 5;
				break;
			}
			
			/* Set new target, use target if legal */
			case '*':
			{
				if (spot_target)
                {
                    if (target_set_interactive(TARGET_GRID)) dir = 5;
                }
                else if (target_set_interactive(TARGET_KILL)) dir = 5;
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
				if (spot_target)
				{
					bell("You must target a specific spot.");
				}
				else
				{
					dir = target_dir(ke.key);
					break;
				}
			}
		}
	    /* Error */
	    if ((spot_target) && (dir != 5)) bell("You must target a specific spot.");

		/* Error */
		else if (!dir) bell("Illegal aim direction!");
	}

	/* No direction */
	if (!dir) return (FALSE);

	/* Save the direction */
	p_ptr->command_dir = dir;

	/* Check for confusion */
	/* if you succeed in controlling teleport or casting telekinesis */
	/* while confused then you should be able to target */
	if ((p_ptr->timed[TMD_CONFUSED]) && (!spot_target))
	{
		/* Random direction */
		dir = ddd[rand_int(8)];
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

	event_type ke;

	cptr p;

	if (repeat_pull(dp))
	{
		return (TRUE);
	}

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = p_ptr->command_dir;

	/* Get a direction */
	while (!dir)
	{
		/* Choose a prompt */
		p = "Direction or <click> (Escape to cancel)? ";

		/* Get a command (or Cancel) */
		if (!get_com_ex(p, &ke)) break;

		/* Check mouse coordinates */
		if (ke.key == '\xff')
		{
			/*if (ke.button) */
			{
				int y = KEY_GRID_Y(ke);
				int x = KEY_GRID_X(ke);

				/* Calculate approximate angle */
				int angle = get_angle_to_target(p_ptr->py, p_ptr->px, y, x, 0);

				/* Convert angle to direction */
				if (angle < 15) dir = 6;
				else if (angle < 33) dir = 9;
				else if (angle < 59) dir = 8;
				else if (angle < 78) dir = 7;
				else if (angle < 104) dir = 4;
				else if (angle < 123) dir = 1;
				else if (angle < 149) dir = 2;
				else if (angle < 168) dir = 3;
				else dir = 6;
			}
			/* else continue; */
		}

		/* Convert keypress into a direction */
		else dir = target_dir(ke.key);

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
		if ((dir == 5) || (rand_int(100) < 75))
		{
			/* Random direction */
			dir = ddd[rand_int(8)];
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


