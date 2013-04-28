
/* File: xtra2.c */

/*
 * Set temporary conditions, print messages and update various displays as
 * needed.  Change shape, practice skills, gain and lose experience, drain and
 * recover skills.  Hurt the character.  Calculate map size and handle map
 * panels.  The looking and targeting code.  Get and confuse a direction,
 * precognition messages, music selection.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"



/*
 * Set a player condition.  -LM-
 *
 * Note that the condition must be stored in a s16b value, not an "int" or
 * a "bool".
 *
 * This function may (in future) be used to call updates using the "flg"
 * value, but only if that would actually make things easier.
 */
static bool set_condition(s16b *condition, int v, u16b flg,
	const char *msg_start, const char *msg_end)
{
	bool notice = FALSE;

	/* Hack -- no condition changes for dead characters  XXX */
	if (p_ptr->is_dead) return (FALSE);

	/* Unused parameter */
	(void)flg;

	/* Hack -- Force good values */
	if (v > 15000) v = 15000;
	if (v <     0) v =     0;

	/* Start of condition */
	if (v)
	{
		if (!(*condition))
		{
			if (strlen(msg_start)) msg_format("%s", msg_start);
			notice = TRUE;
		}
	}

	/* End of condition */
	else
	{
		if (*condition)
		{
			if (strlen(msg_end)) msg_format("%s", msg_end);
			notice = TRUE;
		}
	}

	/* Use the value */
	*condition = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->blind", notice observable changes
 *
 * Note the use of "PU_FORGET_VIEW" and "PU_UPDATE_VIEW", which are needed
 * because "p_ptr->blind" affects the "CAVE_SEEN" flag, and "PU_MONSTERS",
 * because "p_ptr->blind" affects monster visibility, and "PU_MAP", because
 * "p_ptr->blind" affects the display of map grids.
 */
bool set_blind(int v, cptr msg)
{
	bool notice;

	char *tmp = format("%s", msg);

	/* Set blindness, output messages */
	notice = set_condition(&p_ptr->blind, v, 0L,
	        (msg ? tmp : "You are blind!"),
	        (msg ? tmp : "You can see again."));

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Sounds */
	if (p_ptr->blind) sound(MSG_BLIND);
	else              sound(MSG_RECOVER);

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_M_LIST | PW_O_LIST);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Notice */
	return (TRUE);
}


/*
 * Set "p_ptr->confused", notice observable changes
 */
bool set_confused(int v)
{
	bool notice = FALSE;

	/* Set confusion, output messages */
	notice = set_condition(&p_ptr->confused, v, 0L,
	        "You are confused!",
	        "You feel less confused now.");

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Sounds */
	if (p_ptr->confused) sound(MSG_CONFUSED);
	else                 sound(MSG_RECOVER);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->image", notice observable changes
 *
 * Note the use of "PR_MAP", which is needed because "p_ptr->image" affects
 * the way in which monsters, objects, and some normal grids, are displayed.
 */
bool set_image(int v)
{
	bool notice = FALSE;

	/* Sounds */
	if      (!p_ptr->image && v) sound(MSG_DRUGGED);
	else if (p_ptr->image && !v) sound(MSG_RECOVER);

	/* Set image, output messages */
	notice = set_condition(&p_ptr->image, v, 0L,
	        "You feel drugged!",
	        "You can see clearly again.");

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_M_LIST | PW_O_LIST);

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

	/* Sounds */
	if      (!p_ptr->poisoned && v) sound(MSG_POISONED);
	else if (p_ptr->poisoned && !v) sound(MSG_RECOVER);

	/* Set poisoned, output messages */
	notice = set_condition(&p_ptr->poisoned, v, 0L,
	        "You are poisoned!",
	        "You are no longer poisoned.");

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (notice);
}


/*
 * Set "p_ptr->diseased", notice observable changes
 */
bool set_diseased(int v, cptr msg)
{
	bool notice = FALSE;

	char *tmp = format("%s", msg);

	/* Sounds */
	if      (!p_ptr->diseased && v) sound(MSG_DISEASED);
	else if (p_ptr->diseased && !v) sound(MSG_RECOVER);

	/* Set diseased, output messages */
	notice = set_condition(&p_ptr->diseased, v, 0L,
	        (msg ? tmp : "You feel ill."),
	        (msg ? tmp : "You no longer feel ill."));

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (notice);
}

/*
 * Set "p_ptr->afraid", notice observable changes
 */
bool set_afraid(int v)
{
	bool notice = FALSE;

	/* Sounds */
	if      (!p_ptr->afraid && v) sound(MSG_AFRAID);
	else if (p_ptr->afraid && !v) sound(MSG_RECOVER);

	/* Set afraid, output messages */
	notice = set_condition(&p_ptr->afraid, v, 0L,
	        "You are terrified!",
	        "You feel bolder now.");

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Print "fear/bold/hero" */
	left_panel_display(DISPLAY_FEAR, 0);

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

	/* Sounds */
	if      (!p_ptr->paralyzed && v) sound(MSG_PARALYZED);
	else if (p_ptr->paralyzed && !v) sound(MSG_RECOVER);

	/* Set paralyzed, output messages */
	notice = set_condition(&p_ptr->paralyzed, v, 0L,
	        "You are paralyzed!",
	        "You can move again.");

	/* Nothing to notice */
	if (!notice) return (FALSE);


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
bool set_fast(int v)
{
	bool notice = FALSE;

	/* Hack -- handle lich's temporary speed */
	if ((p_ptr->schange == SHAPE_LICH) && (v == 0)) return (TRUE);


	/* Sounds */
	if      (!p_ptr->fast && v) sound(MSG_SPEED);
	else if (p_ptr->fast && !v) sound(MSG_RECOVER);

	/* Set fast, output messages */
	notice = set_condition(&p_ptr->fast, v, 0L,
	        "You feel yourself moving faster!",
	        "You feel yourself slow down.");

	/* Nothing to notice */
	if (!notice) return (FALSE);


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

	/* Sounds */
	if      (!p_ptr->slow && v) sound(MSG_SLOW);
	else if (p_ptr->slow && !v) sound(MSG_RECOVER);

	/* Set slow, output messages */
	notice = set_condition(&p_ptr->slow, v, 0L,
	        "You feel yourself moving slower!",
	        "You feel yourself speed up.");

	/* Nothing to notice */
	if (!notice) return (FALSE);


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
bool set_shield(int v, cptr msg)
{
	bool notice = FALSE;

	char *tmp = format("%s", msg);

	/* Sounds */
	if      (!p_ptr->shield && v) sound(MSG_SHIELD);
	else if (p_ptr->shield && !v) sound(MSG_RECOVER);

	/* Set shield, output messages */
	notice = set_condition(&p_ptr->shield, v, 0L,
	        (msg ? tmp : "A mystic shield forms around your body!"),
	        (msg ? tmp : "You are no longer shielded."));

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Print "shield" */
	left_panel_display(DISPLAY_PROT_BLESS, 0);
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->steelskin", notice observable changes
 */
bool set_steelskin(int v, cptr msg)
{
	bool notice = FALSE;

	char *tmp = format("%s", msg);

	/* Sounds */
	if      (!p_ptr->steelskin && v) sound(MSG_STEELSKIN);
	else if (p_ptr->steelskin && !v) sound(MSG_RECOVER);

	/* Set steelskin, output messages */
	notice = set_condition(&p_ptr->steelskin, v, 0L,
	        (msg ? tmp : "Your skin turns to steel!"),
	        (msg ? tmp : "Your skin is no longer protected."));


	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Print "steelskin" */
	left_panel_display(DISPLAY_PROT_BLESS, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->blessed", notice observable changes
 */
bool set_blessed(int v, cptr msg)
{
	bool notice = FALSE;

	char *tmp = format("%s", msg);

	/* Sounds */
	if      (!p_ptr->blessed && v) sound(MSG_BLESSED);
	else if (p_ptr->blessed && !v) sound(MSG_RECOVER);

	/* Set blessed, output messages */
	notice = set_condition(&p_ptr->blessed, v, 0L,
	        (msg ? tmp : "You feel righteous!"),
	        (msg ? tmp : "The blessing has expired."));

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Print "blessed/holy" */
	left_panel_display(DISPLAY_PROT_BLESS, 0);
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->holy", notice observable changes
 */
bool set_holy(int v)
{
	bool notice = FALSE;

	/* Sounds */
	if      (!p_ptr->holy && v) sound(MSG_HOLY);
	else if (p_ptr->holy && !v) sound(MSG_RECOVER);

	/* Set holy, output messages */
	notice = set_condition(&p_ptr->holy, v, 0L,
	        "You feel a holy aura around you!",
	        "The holy aura vanishes.");

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate light radius */
	p_ptr->update |= (PU_TORCH);

	/* Print "blessed/holy" */
	left_panel_display(DISPLAY_PROT_BLESS, 0);
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->bold", notice observable changes
 */
bool set_bold(int v)
{
	bool notice = FALSE;

	/* Set bold, output messages */
	notice = set_condition(&p_ptr->bold, v, 0L,
	        "You feel yourself grow bolder.",
	        "Your courage returns to normal.");

	/* Remove any fear */
	if ((v) && (p_ptr->afraid))
	{
		/* No extraneous messages for removal of fear  XXX */
		p_ptr->afraid = 0;
		notice = TRUE;
	}

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Recalculate bonuses (especially p_ptr->resist_fear) */
	p_ptr->update |= (PU_BONUS);

	/* Print "fear/bold/hero" */
	left_panel_display(DISPLAY_FEAR, 0);
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

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

	/* Sounds */
	if      (!p_ptr->hero && v) sound(MSG_HERO);
	else if (p_ptr->hero && !v) sound(MSG_RECOVER);

	/* Special bonuses for heroism */
	if ((v) && (!p_ptr->hero))
	{
		if (extra_hp_player(10)) notice = TRUE;
	}

	/* Can still become afraid if hero, must do this regardless -JM */
	if (p_ptr->afraid)
	{
		/* No extraneous messages for removal of fear */
		p_ptr->afraid = 0;
		notice = TRUE;
	}

	/* Set hero, output messages */
	notice = set_condition(&p_ptr->hero, v, 0L,
	        "You feel heroic!",
	        "The heroism wears off.");

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate hitpoints */
	p_ptr->update |= (PU_HP);

	/* Print "fear/bold/hero" */
	left_panel_display(DISPLAY_FEAR, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->berserk", notice observable changes
 *
 * Special note:  Berserk rage is followed by a period of weakness, the
 * length of which is controlled by the define BERSERK_WEAKNESS_LENGTH.
 */
bool set_berserk(int v)
{
	bool notice = FALSE;

	/* Berserk rage */
	if (v > BERSERK_WEAKNESS_LENGTH)
	{
		if (!p_ptr->berserk)
		{
			(void)extra_hp_player(30);
			if (p_ptr->afraid)
			{
				/* No extraneous messages for removal of fear */
				p_ptr->afraid = 0;
			}

			message(MSG_BERSERK, 0, "Berserkergang!");
			notice = TRUE;
		}
	}

	/* Berserk weakness */
	else if (v == BERSERK_WEAKNESS_LENGTH)
	{
		if (p_ptr->berserk)
		{
			msg_print("Your battle-rage passes, and you stagger with weakness.");
			notice = TRUE;
		}
	}

	/* Berserker fit is over */
	else if (v == 0)
	{
		if (p_ptr->berserk)
		{
			message(MSG_RECOVER, 0, "Your berserker fit comes to an end.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->berserk = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Print "berserk" */
	left_panel_display(DISPLAY_FEAR, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Recalculate hitpoints */
	p_ptr->update |= (PU_HP);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->necro_rage", notice observable changes
 *
 * Special note:  Necromantic rage is followed by a period of weakness,
 * the length of which is controlled by the define NECRO_WEAKNESS_LENGTH.
 */
bool set_necro_rage(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Necro rage */
	if (v > NECRO_WEAKNESS_LENGTH)
	{
		if (!p_ptr->necro_rage)
		{
			message(MSG_NECRO_RAGE, 0, "Power!");
			notice = TRUE;
		}
	}

	/* Necro weakness */
	else if (v == NECRO_WEAKNESS_LENGTH)
	{
		if (p_ptr->necro_rage)
		{
			msg_print("Your necromantic rage passes; you feel utterly exhausted.");
			notice = TRUE;
		}
	}

	/* Berserker fit is over */
	else if (v == 0)
	{
		if (p_ptr->necro_rage)
		{
			message(MSG_RECOVER, 0, "You feel normal again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->necro_rage = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Print "necro_rage" */
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

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

	/* Sounds */
	if      (!p_ptr->protevil && v) sound(MSG_PROT_EVIL);
	else if (p_ptr->protevil && !v) sound(MSG_RECOVER);

	/* Set protevil, output messages */
	notice = set_condition(&p_ptr->protevil, v, 0L,
	        "You feel safe from evil!",
	        "You no longer feel safe from evil.");

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Print "protevil" */
	left_panel_display(DISPLAY_PROT_BLESS, 0);
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->wiz_prot", notice observable changes
 */
bool set_wiz_prot(int v)
{
	bool notice = FALSE;

	/* Sounds */
	if      (!p_ptr->wiz_prot && v) sound(MSG_WIZ_PROT);
	else if (p_ptr->wiz_prot && !v) sound(MSG_RECOVER);

	/* Set wiz_prot, output messages */
	notice = set_condition(&p_ptr->wiz_prot, v, 0L,
	        "You feel protected!",
	        "You are no longer protected.");

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Print "wiz_prot" */
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->detect_inv", notice observable changes
 *
 * Note the use of "PU_MONSTERS", which is needed because
 * "p_ptr->detect_inv" affects monster visibility.
 */
bool set_detect_inv(int v)
{
	bool notice = FALSE;

	/* Sounds */
	if      (!p_ptr->detect_inv && v) sound(MSG_SEE_INVIS);
	else if (p_ptr->detect_inv && !v) sound(MSG_RECOVER);

	/* Set detect_inv, output messages */
	notice = set_condition(&p_ptr->detect_inv, v, 0L,
	        "Your eyes feel very sensitive!",
	        "Your eyes feel less sensitive.");

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Update the monsters XXX */
	p_ptr->update |= (PU_MONSTERS);

	/* Print "detect_inv" */
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->esp_evil", notice observable changes
 */
bool set_esp_evil(int v)
{
	bool notice = FALSE;

	/* Set esp_evil, output messages */
	notice = set_condition(&p_ptr->esp_evil, v, 0L,
	        "You become aware of evil creatures.",
	        "You are no longer especially aware of evil creatures.");

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Update the monsters XXX */
	p_ptr->update |= (PU_MONSTERS);

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

	/* Set esp_evil, output messages */
	notice = set_condition(&p_ptr->tim_esp, v, 0L,
	        "Your mind expands!",
	        "Your mind contracts once more.");

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Update the monsters XXX */
	p_ptr->update |= (PU_MONSTERS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->tim_infra", notice observable changes
 *
 * Note the use of "PU_MONSTERS", which is needed because because
 * "p_ptr->tim_infra" affects monster visibility.
 */
bool set_tim_infra(int v)
{
	bool notice = FALSE;

	/* Sounds */
	if      (!p_ptr->tim_infra && v) sound(MSG_INFRARED);
	else if (p_ptr->tim_infra && !v) sound(MSG_RECOVER);

	/* Set tim_infra, output messages */
	notice = set_condition(&p_ptr->tim_infra, v, 0L,
	        "Your eyes begin to tingle!",
	        "Your eyes stop tingling.");

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Print "tim_infra" */
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Update the monsters XXX */
	p_ptr->update |= (PU_MONSTERS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->tim_invis", and "p_ptr->tim_inv_pow",
 * notice observable changes
 */
bool set_invis(int v, int p)
{
	bool notice = FALSE;

	/* Sounds */
	if      (!p_ptr->tim_invis && v) sound(MSG_INVIS);
	else if (p_ptr->tim_invis && !v) sound(MSG_RECOVER);

	/* Set tim_invis, output messages */
	notice = set_condition(&p_ptr->tim_invis, v, 0L,
			"You feel your body fade away.",
			(p_ptr->invisible > p_ptr->tim_inv_pow) ?
			"Your invisibility is no longer enhanced." :
			"You are no longer invisible.");


	/* Set strength of invisibility */
	if (p_ptr->tim_invis) p_ptr->tim_inv_pow = p;
	else                  p_ptr->tim_inv_pow = 0;

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Print "invisibility" */
	left_panel_display(DISPLAY_INVISIBILITY, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->hold_weath", do not notice observable changes
 */
bool set_hold_weather(int v)
{
	/* Set hold_weath, output messages (but not when the effects end) */
	(void)set_condition(&p_ptr->hold_weath, v, 0L,
	        "You lock the weather in place.", "");

	/* Never notice */
	return (FALSE);
}

/*
 * Set "p_ptr->regen_hp", notice observable changes
 *
 * Updates are handled in "process_player()"
 */
bool set_regen_hp(int v)
{
	bool notice = FALSE;

	/* Set regen_hp, output messages */
	notice = set_condition(&p_ptr->regen_hp, v, 0L,
	        "You feel unusually robust!",
	        "You no longer feel unusually robust.");

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "regen" */
	left_panel_display(DISPLAY_REGEN, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->regen_mana", notice observable changes
 *
 * Updates are handled in "process_player()"
 */
bool set_regen_mana(int v)
{
	bool notice = FALSE;

	/* Set regen_mana, output messages */
	notice = set_condition(&p_ptr->regen_mana, v, 0L,
	        "Your mind feels especially clear!",
	        "Your mind is no longer especially clear.");

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "regen" */
	left_panel_display(DISPLAY_REGEN, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->vitality", notice observable changes
 *
 * Updates are handled in "process_player()"
 */
bool set_vitality(int v)
{
	bool notice = FALSE;

	/* Set vitality, output messages */
	notice = set_condition(&p_ptr->vitality, v, 0L,
	        "Your vitality increases!",
	        "Your vitality returns to normal.");

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "regen" */
	left_panel_display(DISPLAY_REGEN, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->mania", notice observable changes
 */
bool set_mania(int v)
{
	bool notice = FALSE;

	/* Sounds */
	if      (!p_ptr->mania && v) sound(MSG_MANIA);
	else if (p_ptr->mania && !v) sound(MSG_RECOVER);

	/* Set mania, output messages */
	notice = set_condition(&p_ptr->mania, v, 0L,
	        "You become subject to manic-depressive fits!",
	        "You are no longer subject to manic-depressive fits.");

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->res_dam", notice observable changes
 */
bool set_res_dam(int v)
{
	bool notice = FALSE;

	/* Sounds */
	if      (!p_ptr->res_dam && v) sound(MSG_RES_DAM);
	else if (p_ptr->res_dam && !v) sound(MSG_RECOVER);

	/* Set mania, output messages */
	notice = set_condition(&p_ptr->res_dam, v, 0L,
	        "Your body becomes resistant to damage!",
	        "Your body is no longer especially tough.");

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Print "res_dam" */
	left_panel_display(DISPLAY_PROT_BLESS, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->oppose_acid", notice observable changes
 *
 * A special hack allows for "prettier" messages when all oppositions time
 * out simultaneously.
 */
bool set_oppose_acid(int v)
{
	bool notice = FALSE;

	/* Special notice when effects end */
	bool end = FALSE;
	if ((!v) && (p_ptr->oppose_acid)) end = TRUE;

	/* Sounds */
	if      (!p_ptr->oppose_acid && v) sound(MSG_RES_ACID);
	else if (p_ptr->oppose_acid && !v) sound(MSG_RECOVER);


	/* Set oppose_acid, output messages (hack -- except when ending) */
	notice = set_condition(&p_ptr->oppose_acid, v, 0L,
	        p_ptr->immune_acid ? "" : "You feel resistant to acid!", "");

	/* Shut */
	if (end)
	{
		/* Special case of all oppositions about to time out */
		if ((p_ptr->oppose_elec == 1) &&
			 (p_ptr->oppose_cold == 1) &&
			 (p_ptr->oppose_fire == 1))
		{
			msg_print("You are no longer protected from the ravages of the elements.");

			/* Hack -- Cancel all other oppositions */
			p_ptr->oppose_elec = 0;
			p_ptr->oppose_cold = 0;
			p_ptr->oppose_fire = 0;

			/* Hack -- Optional canceling of poison */
			if (p_ptr->oppose_pois == 1) p_ptr->oppose_pois = 0;
		}

		/* Usual case */
		else
		{
			if (!p_ptr->immune_acid)
				msg_print("You feel less resistant to acid.");
		}
	}

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Print "oppositions" */
	left_panel_display(DISPLAY_OPPOSE, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

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

	/* Sounds */
	if      (!p_ptr->oppose_elec && v) sound(MSG_RES_ELEC);
	else if (p_ptr->oppose_elec && !v) sound(MSG_RECOVER);

	/* Set oppose_elec, output messages */
	notice = set_condition(&p_ptr->oppose_elec, v, 0L,
	        p_ptr->immune_elec ? "" : "You feel resistant to electricity!",
	        p_ptr->immune_elec ? "" : "You feel less resistant to electricity.");

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "oppositions" */
	left_panel_display(DISPLAY_OPPOSE, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

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

	/* Sounds */
	if      (!p_ptr->oppose_fire && v) sound(MSG_RES_FIRE);
	else if (p_ptr->oppose_fire && !v) sound(MSG_RECOVER);

	/* Set oppose_fire, output messages */
	notice = set_condition(&p_ptr->oppose_fire, v, 0L,
	        p_ptr->immune_fire ? "" : "You feel resistant to fire!",
	        p_ptr->immune_fire ? "" : "You feel less resistant to fire.");

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "oppositions" */
	left_panel_display(DISPLAY_OPPOSE, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

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

	/* Hack -- handle lich's temporary resistance */
	if ((p_ptr->schange == SHAPE_LICH) && (v == 0)) return (TRUE);

	/* Sounds */
	if      (!p_ptr->oppose_cold && v) sound(MSG_RES_COLD);
	else if (p_ptr->oppose_cold && !v) sound(MSG_RECOVER);

	/* Set oppose_cold, output messages */
	notice = set_condition(&p_ptr->oppose_cold, v, 0L,
	        p_ptr->immune_cold ? "" : "You feel resistant to cold!",
	        p_ptr->immune_cold ? "" : "You feel less resistant to cold.");

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "oppositions" */
	left_panel_display(DISPLAY_OPPOSE, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

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

	/* Hack -- handle lich's temporary resistance */
	if ((p_ptr->schange == SHAPE_LICH) && (v == 0)) return (TRUE);

	/* Sounds */
	if      (!p_ptr->oppose_pois && v) sound(MSG_RES_POIS);
	else if (p_ptr->oppose_pois && !v) sound(MSG_RECOVER);

	/* Set oppose_pois, output messages */
	notice = set_condition(&p_ptr->oppose_pois, v, 0L,
	        "You feel resistant to poison!",
	        "You feel less resistant to poison.");

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "oppositions" */
	left_panel_display(DISPLAY_OPPOSE, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->oppose_ethereal", notice observable changes
 */
bool set_oppose_ethereal(int v)
{
	bool notice = FALSE;

	bool start = FALSE;
	if ((v) && (!p_ptr->oppose_ethereal)) start = TRUE;

	/* Sounds */
	if      (!p_ptr->oppose_ethereal && v) sound(MSG_RES_ETHEREAL);
	else if (p_ptr->oppose_ethereal && !v) sound(MSG_RECOVER);

	/* Set oppose_ethereal, output messages */
	notice = set_condition(&p_ptr->oppose_ethereal, v, 0L,
	        "You feel resistant to ethereal forces!",
	        "You feel less resistant to ethereal forces.");

	/* Hack -- Grant opposition to electricity */
	if ((start) && (p_ptr->oppose_elec < v)) p_ptr->oppose_elec = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "oppositions" */
	left_panel_display(DISPLAY_OPPOSE, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->acid_attack", notice observable changes
 */
bool set_acid_attack(int v)
{
	cptr desc = (p_ptr->barehand ? "hands" : "weapons");

	bool notice = FALSE;

	char desc_start[DESC_LEN];
	char desc_end[DESC_LEN];

	/* Build the messages */
	(void)strnfmt(desc_start, sizeof(desc_start), "Your %s drip with acid!", desc);
	(void)strnfmt(desc_end, sizeof(desc_end), "Your %s are no longer acidic.", desc);

	/* Set acid_attack, output messages */
	notice = set_condition(&p_ptr->acid_attack, v, 0L,
	        desc_start, desc_end);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "special attacks" */
	left_panel_display(DISPLAY_SPECIAL_ATTACK, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->elec_attack", notice observable changes
 */
bool set_elec_attack(int v)
{
	cptr desc = (p_ptr->barehand ? "hands" : "weapons");

	bool notice = FALSE;

	char desc_start[DESC_LEN];
	char desc_end[DESC_LEN];

	/* Build the messages */
	(void)strnfmt(desc_start, sizeof(desc_start), "Your %s crackle with electricity!", desc);
	(void)strnfmt(desc_end, sizeof(desc_end), "Your %s are no longer electric.", desc);

	/* Set elec_attack, output messages */
	notice = set_condition(&p_ptr->elec_attack, v, 0L,
	        desc_start, desc_end);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "special attacks" */
	left_panel_display(DISPLAY_SPECIAL_ATTACK, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->fire_attack", notice observable changes
 */
bool set_fire_attack(int v)
{
	cptr desc = (p_ptr->barehand ? "hands" : "weapons");

	bool notice = FALSE;

	char desc_start[DESC_LEN];
	char desc_end[DESC_LEN];

	/* Build the messages */
	(void)strnfmt(desc_start, sizeof(desc_start), "Your %s burn with fire!", desc);
	(void)strnfmt(desc_end, sizeof(desc_end), "Your %s are no longer fiery.", desc);

	/* Set fire_attack, output messages */
	notice = set_condition(&p_ptr->fire_attack, v, 0L,
	        desc_start, desc_end);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "special attacks" */
	left_panel_display(DISPLAY_SPECIAL_ATTACK, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->cold_attack", notice observable changes
 */
bool set_cold_attack(int v)
{
	cptr desc = (p_ptr->barehand ? "hands" : "weapons");

	bool notice = FALSE;

	char desc_start[DESC_LEN];
	char desc_end[DESC_LEN];

	/* Build the messages */
	(void)strnfmt(desc_start, sizeof(desc_start), "Your %s glitter with frost!", desc);
	(void)strnfmt(desc_end, sizeof(desc_end), "Your %s are no longer icy.", desc);

	/* Set cold_attack, output messages */
	notice = set_condition(&p_ptr->cold_attack, v, 0L,
	        desc_start, desc_end);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "special attacks" */
	left_panel_display(DISPLAY_SPECIAL_ATTACK, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->pois_attack", notice observable changes
 */
bool set_pois_attack(int v)
{
	cptr desc = (p_ptr->barehand ? "hands" : "weapons");

	bool notice = FALSE;

	char desc_start[DESC_LEN];
	char desc_end[DESC_LEN];

	/* Build the messages */
	(void)strnfmt(desc_start, sizeof(desc_start), "Your %s gleam with poison!", desc);
	(void)strnfmt(desc_end, sizeof(desc_end), "Your %s are no longer venomous.", desc);

	/* Set pois_attack, output messages */
	notice = set_condition(&p_ptr->pois_attack, v, 0L,
	        desc_start, desc_end);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "special attacks" */
	left_panel_display(DISPLAY_SPECIAL_ATTACK, 0);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

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
	if (p_ptr->stun >= KNOCKED_OUT)
	{
		/* No more stunning when knocked out */
		if (v > p_ptr->stun) v = p_ptr->stun;
		old_aux = 3;
	}
	/* Heavy stun */
	else if (p_ptr->stun >= HVY_STUN)
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
	if (v >= KNOCKED_OUT)
	{
		new_aux = 3;
	}

	/* Heavy stun */
	else if (v >= HVY_STUN)
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

	/* Increase stun */
	if (new_aux > old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* Stun */
			case 1:
			message(MSG_STUN, 0, "You have been stunned.");
			break;

			/* Heavy stun */
			case 2:
			message(MSG_STUN, 50, "You have been heavily stunned.");
			break;

			/* Knocked out */
			case 3:
			message(MSG_STUN, 200, "You have been knocked out!");
			break;
		}

		/* Notice */
		notice = TRUE;
	}

	/* Decrease stun */
	else if (!new_aux && old_aux)
	{
		/* Message */
		message(MSG_RECOVER, 0, "You are no longer stunned.");
		if (disturb_state) disturb(0, 0);

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

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

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
	if (p_ptr->cut > WOUND_MORTAL)
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
			message(MSG_CUT, 0, "You have been given a graze.");
			break;

			/* Light cut */
			case 2:
			message(MSG_CUT, 0, "You have been given a light cut.");
			break;

			/* Bad cut */
			case 3:
			message(MSG_CUT, 0, "You have been given a bad cut.");
			break;

			/* Nasty cut */
			case 4:
			message(MSG_CUT, 0, "You have been given a nasty cut.");
			break;

			/* Severe cut */
			case 5:
			message(MSG_CUT, 25, "You have been given a severe cut.");
			break;

			/* Deep gash */
			case 6:
			message(MSG_CUT, 50, "You have been given a deep gash.");
			break;

			/* Mortal wound */
			case 7:
			message(MSG_CUT, 200, "You have been given a mortal wound!");
			break;
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
			message(MSG_RECOVER, 0, "You are no longer bleeding.");
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

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->food", notice observable changes
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
bool set_food(s32b v)
{
	int old_aux, new_aux;
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 50000) ? 50000 : (v < 0) ? 0 : v;

	/* Fainting / Starving */
	if (p_ptr->food < p_ptr->food_fainting)
	{
		old_aux = 0;
	}

	/* Weak */
	else if (p_ptr->food < p_ptr->food_weak)
	{
		old_aux = 1;
	}

	/* Hungry */
	else if (p_ptr->food < p_ptr->food_hungry)
	{
		old_aux = 2;
	}

	/* Normal */
	else if (p_ptr->food < p_ptr->food_full)
	{
		old_aux = 3;
	}

	/* Full */
	else if (p_ptr->food < p_ptr->food_bloated)
	{
		old_aux = 4;
	}

	/* Gorged */
	else
	{
		old_aux = 5;
	}

	/* Fainting / Starving */
	if (v < p_ptr->food_fainting)
	{
		new_aux = 0;
	}
	/* Weak */
	else if (v < p_ptr->food_weak)
	{
		new_aux = 1;
	}

	/* Hungry */
	else if (v < p_ptr->food_hungry)
	{
		new_aux = 2;
	}

	/* Normal */
	else if (v < p_ptr->food_full)
	{
		new_aux = 3;
	}

	/* Full */
	else if (v < p_ptr->food_bloated)
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
			msg_print("You are still weak.");
			break;

			/* Hungry */
			case 2:
			msg_print("You are still hungry.");
			break;

			/* Normal */
			case 3:
			msg_print("You are no longer hungry.");
			break;

			/* Full */
			case 4:
			msg_print("You are full!");
			break;

			/* Bloated */
			case 5:
			msg_print("You have gorged yourself!");
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
			sound(MSG_NOTICE);
			message(MSG_RED,  500, "You are getting faint from hunger!");
			break;

			/* Weak */
			case 1:
			sound(MSG_NOTICE);
			message(MSG_ORANGE, 500, "You are getting weak from hunger!");
			break;

			/* Hungry */
			case 2:
			sound(MSG_NOTICE);
			message(MSG_YELLOW, 0, "You are getting hungry.");
			break;

			/* Normal */
			case 3:
			sound(MSG_NOTICE);
			msg_print("You are no longer full.");
			break;

			/* Full */
			case 4:
			sound(MSG_NOTICE);
			msg_print("You are no longer gorged.");
			break;
		}

		/* Change */
		notice = TRUE;
	}

	/* Use the value */
	p_ptr->food = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Optional disturb, except if going from full to normal */
	if ((disturb_state) && (new_aux >= 4)) disturb(0, 0);

	/* Always disturb when in trouble */
	else if (new_aux <= 2) disturb(0, 0);

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
 * Set "p_ptr->word_recall", notice observable changes
 */
bool set_recall(int v)
{
	bool notice = FALSE;

	/* Set word_recall, output messages */
	notice = set_condition(&p_ptr->word_recall, v, 0L,
	        "The air about you becomes charged...",
	        "A tension leaves the air around you...");

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->dancing_feet", notice observable changes
 */
bool set_dancing_feet(int v, cptr msg, bool safe)
{
	bool notice = FALSE;

	char *tmp = format("%s", msg);

	/* Set dancing_feet, output messages */
	notice = set_condition(&p_ptr->dancing_feet, v, 0L,
	        (msg ? tmp : "You start to blink around."),
	        (msg ? tmp : "You stop blinking around."));

	/* Use the value */
	p_ptr->dancing_feet_safe = safe;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "dancing_feet" */
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}



/*
 * Set "p_ptr->phasing_foes", notice observable changes
 */
bool set_phasing_foes(int v, cptr msg)
{
	bool notice = FALSE;

	char *tmp = format("%s", msg);

	/* Set phasing_foes, output messages */
	notice = set_condition(&p_ptr->phasing_foes, v, 0L,
	        (msg ? tmp : "You gesture, and your foes begin to jitter and dance..."),
	        (msg ? tmp : "Your enemies feel more steady."));

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "phasing_foes" */
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->blink_away", notice observable changes
 */
bool set_blink_away(int v)
{
	bool notice = FALSE;

	/* Set blink_away, output messages */
	notice = set_condition(&p_ptr->blink_away, v, 0L,
	        ("You prepare to blink away."),
	        ("You have used up all your blinks."));

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Print "blink_away" */
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->evasion", notice observable changes
 */
bool set_evasion(int v)
{
	bool notice = FALSE;

	/* Set evasion, output messages */
	notice = set_condition(&p_ptr->evasion, v, 0L,
	        ("You are surrounded by an evasion field."),
	        ("Your evasion field crumbles away."));

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Print "evasion" */
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->aura_cold", notice observable changes
 */
bool set_aura_cold(int v)
{
	bool notice = FALSE;

	bool start = FALSE;
	if ((v) && (!p_ptr->aura_cold)) start = TRUE;

	/* Set aura_cold, output messages */
	notice = set_condition(&p_ptr->aura_cold, v, 0L,
	        "A frosty aura surrounds you!",
	        "You are no longer surrounded by a frosty aura.");

	/* Hack -- Grant opposition to cold */
	if ((start) && (p_ptr->oppose_cold < v)) p_ptr->oppose_cold = v;

	/* Hack -- do not allow auras of fire and frost at the same time */
	if (v && p_ptr->aura_fire)
	{
		p_ptr->aura_fire = 0;
		notice = TRUE;
	}

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Print "aura" */
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->aura_fire", notice observable changes
 */
bool set_aura_fire(int v)
{
	bool notice = FALSE;

	bool start = FALSE;
	if ((v) && (!p_ptr->aura_fire)) start = TRUE;

	/* Set aura_fire, output messages */
	notice = set_condition(&p_ptr->aura_fire, v, 0L,
	        "A fiery aura surrounds you!",
	        "You are no longer surrounded by a fiery aura.");

	/* Hack -- Grant opposition to fire */
	if ((start) && (p_ptr->oppose_fire < v)) p_ptr->oppose_fire = v;

	/* Hack -- do not allow auras of fire and frost at the same time */
	if (v && p_ptr->aura_cold)
	{
		p_ptr->aura_cold = 0;
		notice = TRUE;
	}

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Print "aura" */
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->mental_barrier", notice observable changes
 */
bool set_mental_barrier(int v)
{
	bool notice = FALSE;

	/* Set mental_barrier, output messages */
	notice = set_condition(&p_ptr->mental_barrier, v, 0L,
	        "Your mind becomes stronger.",
	        "Your mind is no longer especially strong.");

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Print "mental_barrier" */
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->forbid_summoning", notice observable changes
 */
bool set_forbid_summoning(int v)
{
	bool notice = FALSE;

	/* Set forbid_summoning, output messages */
	notice = set_condition(&p_ptr->forbid_summoning, v, 0L,
	        "You cast an anti-summoning spell.",
	        "Your anti-summoning spell wears off.");

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Print "forbid_summoning" */
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->wraithform", notice observable changes
 */
bool set_wraithform(int v)
{
	bool notice = FALSE;

	/* Set wraithform, output messages */
	notice = set_condition(&p_ptr->wraithform, v, 0L,
	        "You become decorporeal!",
	        "You again become corporeal.");

	/* If the character is in an impassable grid without wraithform, he's in trouble */
	if ((!p_ptr->wraithform) && (!cave_passable_bold(p_ptr->py, p_ptr->px)))
	{
		if (!cave_any_door(p_ptr->py, p_ptr->px))
		{
			(void)take_hit(damroll(10 + p_ptr->power, 6), 0,
				"You emerge in a wall!", "becoming one with a wall");
			cave_set_feat(p_ptr->py, p_ptr->px, FEAT_RUBBLE);
		}
		else
		{
			(void)take_hit(damroll(5 + p_ptr->power / 2, 6), 0,
				"You emerge in a door!", "becoming one with a door");
			cave_set_feat(p_ptr->py, p_ptr->px, FEAT_BROKEN);
		}
	}

	/* Nothing to notice */
	if (!notice) return (FALSE);


	/* Print "wraithform" */
	left_panel_display(DISPLAY_REALM_COND, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->form_dur and p_ptr->schange", notice observable changes
 */
bool shapechange_temp(int v, s16b shape)
{
	bool notice = FALSE;

	/* Set form duration, no messages */
	notice = set_condition(&p_ptr->form_dur, v, 0L, "", "");

	/* Nothing to do */
	if (shape == p_ptr->schange && v > 0) return (TRUE);

	/* Turn into a something */
	if (p_ptr->form_dur) shapechange(shape);

	/* Change back to normal form */
	if ((!p_ptr->form_dur) && (p_ptr->schange))
	{
		do_cmd_unchange(FALSE);
	}

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->pois_power", do not notice
 */
bool set_pois_power(int v, int dur)
{
	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Use the value */
	p_ptr->pois_power_dur = dur;
	if (p_ptr->pois_power_dur < 0) p_ptr->pois_power_dur = 0;

	/* Poison enhancement has run out */
	if (p_ptr->pois_power_dur == 0) p_ptr->pois_power = 0;

	/* Adjust poison power */
	else p_ptr->pois_power = v;

	/* Never notice */
	return (FALSE);
}

/*
 * Set "p_ptr->chaos_power", do not notice
 */
bool set_chaos_power(int v, int dur)
{
	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Use the value */
	p_ptr->chaos_power_dur = dur;
	if (p_ptr->chaos_power_dur < 0) p_ptr->chaos_power_dur = 0;

	/* Chaos enhancement has run out */
	if (p_ptr->chaos_power_dur == 0) p_ptr->chaos_power = 0;

	/* Adjust chaos power */
	else p_ptr->chaos_power = v;

	/* Never notice */
	return (FALSE);
}

/*
 * Set "p_ptr->nexus_field", notice observable changes
 */
bool set_nexus_field(int v, int dam)
{
	bool notice = FALSE;

	/* Set nexus_field, output messages */
	notice = set_condition(&p_ptr->nexus_field, v, 0L,
	        "You are surrounded in a nexus field!",
	        "You are no longer surrounded in a nexus field.");

	/* Set strength of field */
	if (p_ptr->nexus_field) p_ptr->nexus_field_strength = dam;
	else                    p_ptr->nexus_field_strength = 0;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->luck", notice observable changes
 */
bool set_luck(int v, cptr msg)
{
	/* Nothing happened */
	if (v == p_ptr->luck) return (FALSE);

	/* Print messages  XXX */
	if (v < p_ptr->luck)
	{
		if (msg) msg_format("%s", msg);
		else     msg_print("You feel strangely unlucky...");
	}
	else if ((p_ptr->luck < 100) && (v >= 100))
	{
		if (msg) msg_format("%s", msg);
		else     msg_print("You feel less unlucky...");
	}

	/* Set luck */
	p_ptr->luck = v;

	/* Enforce limits */
	if (p_ptr->luck > 100) p_ptr->luck = 100;
	if (p_ptr->luck <   0) p_ptr->luck =   0;


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Print "luck" */
	left_panel_display(DISPLAY_LUCK, 0);

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->unsanctified", notice observable changes
 */
bool set_unsanctified(int v)
{
	bool notice = FALSE;

	/* Nothing happened */
	if (v == p_ptr->unsanctified) return (FALSE);

	/* Set unsanctified, output messages */
	notice = set_condition(&p_ptr->unsanctified, v, 0L,
	        "\"Angels fight for the Light; you must not be their enemy.\"\nYou have committed a sin, and you feel the Divine turn away from you.",
	        "You feel the Divine forgive you.  \"Sin no more, I ask.\"");

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->self_knowledge", notice observable changes
 */
bool set_self_knowledge(int v, cptr msg)
{
	bool notice = FALSE;

	char *tmp = format("%s", msg);

	/* Set self_knowledge, output messages */
	notice = set_condition(&p_ptr->self_knowledge, v, 0L,
	        (msg ? tmp : ""),
	        (msg ? tmp : "Your self knowledge returns to normal."));

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Redraw conditions status */
	p_ptr->redraw |= (PR_CONDITIONS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

void shapechange_perm(s16b shape)
{
	/* Clear form duration */
	p_ptr->form_dur = 0;
	shapechange(shape);
}


/*
 * Shapechange code. Most of the work is done by calc_bonuses().
 *
 * Mana now exclusively dealt with in calc_mana - JM
 */
void shapechange(s16b shape)
{
	cptr shapedesc = "(none)";
	int old_shape = p_ptr->schange;

	/* Wonder Twin powers -- Activate! */
	p_ptr->schange = shape;

	/* Get description of the shape */
	switch (shape)
	{
		case SHAPE_GOAT:    shapedesc = "goat";       break;
		case SHAPE_BEAR:    shapedesc = "bear";       break;
		case SHAPE_MOUSE:   shapedesc = "mouse";      break;
		case SHAPE_HOUND:   shapedesc = "hound";      break;
		case SHAPE_CHEETAH: shapedesc = "cheetah";    break;
		case SHAPE_LION:    shapedesc = "lion";       break;
		case SHAPE_DRAGON:  shapedesc = "dragon";     break;
		case SHAPE_ENT:     shapedesc = "ent";        break;
		case SHAPE_TROLL:   shapedesc = "troll";      break;
		case SHAPE_BAT:     shapedesc = "bat";        break;
		case SHAPE_LICH:    shapedesc = "lich";       break;
		case SHAPE_VAMPIRE: shapedesc = "vampire";    break;
		case SHAPE_WEREWOLF:shapedesc = "werewolf";   break;
		case SHAPE_SERPENT: shapedesc = "serpent";    break;
		case SHAPE_ANGEL:   shapedesc = "angel";      break;
		default:            shapedesc = "monster";    break;
	}

	/* Shapechange */
	if (shape != SHAPE_NORMAL)
	{
		/* Messages */
		msg_format("You assume the form of a %s.", shapedesc);
		msg_print("Your equipment merges into your body.");
	}

	/* Return to normal form */
	else
	{
		/* Message */
		msg_print("You return to your normal form.");
	}

	/* Hack -- handle lich's temporary stuff */
	if (shape == SHAPE_LICH)
	{
		if (!p_ptr->oppose_cold) set_oppose_cold(1);
		if (!p_ptr->oppose_pois) set_oppose_pois(1);
		if (!p_ptr->fast) set_fast(1);
	}
	else if (old_shape == SHAPE_LICH)
	{
		if (p_ptr->oppose_cold == 1) set_oppose_cold(0);
		if (p_ptr->oppose_pois == 1) set_oppose_pois(0);
		if (p_ptr->fast == 1) set_fast(0);
	}

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_TORCH | PU_HP | PU_MANA | PU_SPELLS);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw everything */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1 | PW_M_LIST | PW_O_LIST);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_MONSTER | PW_OBJECT);

	/* Handle stuff */
	handle_stuff();

	/* Disturb the character */
	disturb(0, 0);
}

/*
 * Stop doing a shapechange.
 */
void do_cmd_unchange(bool voluntary)
{
	if (!p_ptr->schange)
	{
		msg_print("You aren't in another form right now.");
		return;
	}

	/* Confirm -- if voluntary */
	if (voluntary)
	{
		if (!get_check("Really return to normal?")) return;
	}

	/* Return to normal form */
	shapechange_perm(SHAPE_NORMAL);

	/* Hack -- cancel temporary shapechanges */
	p_ptr->form_dur = 0;

	/* Use some energy  XXX */
	if (voluntary) p_ptr->energy_use = 100;
}

/*
 * Practice a skill.
 *
 * Most skills (disarming being the exception) are practiced both when
 * a monster takes damage and when it is killed.
 */
void practice_skill(s32b amount, s16b skill)
{
	/* Practice a skill */
	if ((skill >= 0) && (skill < NUM_SKILLS))
	{
		/* Normally multiply the exp gained by 2.5 (x10) */
		int mult = 25;
		s32b add, divisor;

		/* Some skills have different practice multipliers */
		if (skill == S_CROSSBOW) mult = 35;
		if (skill == S_BOW)      mult = 35;
		if (skill == S_SLING)    mult = 35;
		if (skill == S_THROWING) mult = 40;
		if (skill == S_DEVICE)   mult = 50;
		if (skill == S_DISARM)   mult = 3000;
		if (skill == S_BURGLARY) mult = 50;

		/* Speed of practice depends on race (varies from ~7 to ~15) */
		divisor = race_adj_cost_skill[skill][p_ptr->prace];

		/* Calculate amount of practice */
		add = amount * (long)mult / divisor;

		/* Practice the skill some */
		p_ptr->pskills[skill].practice_exp += add;
	}
}


/*
 * Advance experience levels and print experience
 */
void check_experience(void)
{
	/* Hack -- lower limit */
	if (p_ptr->exp < 0) p_ptr->exp = 0;

	/* Hack -- upper limit */
	if (p_ptr->exp > PY_MAX_EXP) p_ptr->exp = PY_MAX_EXP;

	/* Redraw experience */
	p_ptr->redraw |= (PR_EXP);

	/* Handle stuff */
	handle_stuff();
}



/*
 * Calculate the amount of experience the character's skills are "worth",
 * ignoring any racial or practice adjustments, and using maximum rather
 * than current values.
 */
s32b calc_spent_exp_max(void)
{
	int i, level;
	s32b cost;

	s32b tot_exp = 0L;

	/* Tally up all skills */
	for (i = 0; i < NUM_SK_USED; i++)
	{
		/* Skip non-existent skills */
		if (skill_info[i].cost_adj == 0) continue;

		/* Use current skills */
		level = p_ptr->pskills[i].max;

		/* Paranoia -- level must be between 0 and 100 */
		if (level <   0) level =   0;
		if (level > 100) level = 100;

		/* Get the cost to get to the current skill level */
		cost = player_exp[level];

		/* Apply level difficulty factor (10x inflation) */
		cost *= skill_info[i].cost_adj;

		/* Apply cost reduction for similar skills */
		adv_cost_reduce_similar(i, &cost, 2);

		/* Sum up adjusted values */
		tot_exp += cost;
	}

	/* Deflate */
	tot_exp /= 10L;

	/* Apply experience cost divisor */
	tot_exp /= EXP_ADJ;

	/* Return the value */
	return (tot_exp);
}

/*
 * Calculate the amount of experience the character's skills are "worth",
 * ignoring any racial or practice adjustments, and using current rather
 * than maximum values.
 *
 * This function is used to calculate both character power and earned exp
 * divisor.
 */
s32b calc_spent_exp(void)
{
	int i, level;
	s32b cost;

	s32b tot_exp = 0L;

	/* Tally up all skills */
	for (i = 0; i < NUM_SK_USED; i++)
	{
		/* Skip non-existent skills */
		if (skill_info[i].cost_adj == 0) continue;

		/* Use current skills */
		level = p_ptr->pskills[i].cur;

		/* Paranoia -- level must be between 0 and 100 */
		if (level <   0) level =   0;
		if (level > 100) level = 100;

		/* Get the cost to get to the current skill level */
		cost = player_exp[level];

		/* Apply level difficulty factor (10x inflation) */
		cost *= skill_info[i].cost_adj;

		/* Apply cost reduction for similar skills */
		adv_cost_reduce_similar(i, &cost, 2);

		/* Sum up adjusted values */
		tot_exp += cost;
	}

	/* Deflate */
	tot_exp /= 10L;

	/* Apply experience cost divisor */
	tot_exp /= EXP_ADJ;

	/* Return the value */
	return (tot_exp);
}

/*
 * Gain experience, practice skills
 */
void gain_exp(s32b amount, s16b skill)
{
	/* Gain some experience */
	p_ptr->exp += amount;

	/* Practice a skill */
	practice_skill(amount, skill);

	/* Check Experience */
	check_experience();
}

/*
 * Lose experience (and skills)
 */
void lose_exp(s32b amount, bool perm)
{
	int i, j;
	int count = 0;

	s32b cost;
	s16b *old_skills;


	/* Hack -- Restrict exp loss */
	if (amount > calc_spent_exp() / 3) amount = calc_spent_exp() / 3;

	/* If exp loss is small, attack unspent exp (the One Ring does this) */
	if ((amount == 1L) && (p_ptr->exp > 0L))
	{
		p_ptr->exp--;
	}

	/* Otherwise, attack only skills */
	else if (amount)
	{
		/* Allocate some space */
		C_MAKE(old_skills, NUM_SK_USED, s16b);

		/* Remember the skill levels before draining */
		for (i = 0; i < NUM_SK_USED; i++)
		{
			old_skills[i] = p_ptr->pskills[i].cur;
		}

		/* Hunt around for skills to suck away */
		for (i = 0; i < 50; i++)
		{
			int skill = S_NOSKILL;

			/* Power all gone */
			if (amount <= 0L) break;

			/* Pick a raised skill at random */
			for (j = 0; j < 50; j++)
			{
				skill = rand_int(NUM_SK_USED);
				if (p_ptr->pskills[skill].cur >= 1) break;
			}

			/* We have a valid skill */
			if (skill >= 0)
			{
				/* Current skill level */
				int cur = p_ptr->pskills[skill].cur;

				/* Get the base exp needed to restore this skill */
				cost = player_exp[cur] - player_exp[cur - 1];

				/* Multiply by skill cost adjustment (10x inflation) */
				cost *= (skill_info[skill].cost_adj);

				/* Get the actual cost (standard rounding) */
				cost = MAX(1L, (cost + (EXP_ADJ * 5)) / (EXP_ADJ * 10));

				/* Only lower this skill if attack is strong enough */
				if (rand_int(cost) < amount)
				{
					/* Do not charge for skills that can't be lowered */
					if (!alter_skill(skill, -1, perm)) cost = 0;
				}

				/* Reduce attack strength by cost */
				amount -= cost;
			}
		}

		/* Count decreased skills */
		for (i = 0; i < NUM_SK_USED; i++)
		{
			/* Skill has decreased - count it */
			if (p_ptr->pskills[i].cur < old_skills[i]) count++;
		}

		/* Display list of reduced skills */
		if (count)
		{
			/* Just to make sure we have plenty of space */
			char buf[2048];

			/* Clear the string */
			strcpy(buf, "");

			/* Scan through the skills */
			for (i = 0, j = 0; i < NUM_SK_USED; i++)
			{
				/* This skill has been reduced */
				if (p_ptr->pskills[i].cur < old_skills[i])
				{
					/* Listing another skill */
					j++;

					/* Add it to the list */
					strcat(buf, skill_info[i].desc);

					/* Commas separate members of a list of more than two. */
					if ((count > 2) && (j < count)) strcat(buf, ",");

					/* "and" before final member of a list of more than one. */
					if ((count > 1) && (j == count - 1))
					{
						strcat(buf, " and");
					}

					/* Insert a space */
					strcat(buf, " ");
				}
			}

			/* Display a list of skills reduced. */
			msg_format("You feel your %sdiminishing.", buf);
		}
	}

	/* Check experience */
	check_experience();
}


/*
 * Restores any drained skills
 */
bool restore_level(void)
{
	bool restore = FALSE;
	int i;

	/* Restore skills */
	for (i = 0; i < NUM_SKILLS; i++)
	{
		/* This skill is drained */
		if (p_ptr->pskills[i].cur < p_ptr->pskills[i].max)
		{
			/* Restore skill */
			p_ptr->pskills[i].cur = p_ptr->pskills[i].max;

			/* Notice */
			restore = TRUE;
		}
	}

	/* Note if something happened */
	if (restore)
	{
		msg_print("You feel your skills return to normal.");

		/* Recalculate character power */
		calc_power();

		/* Redraw and recalc whatever may have changed */
		p_ptr->redraw |= (PR_EXP | PR_HP | PR_TITLE);
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);
	}
	return (restore);
}



/*
 * Color damage messages according to percentage of current HPs lost.
 */
static int danger_color(int base, int hurt)
{
	/* Require that the "colored warning messages" option be on */
	if (!colored_hit_msg) return (MSG_GENERIC);

	/* Color message according to severity of hit */
	if (hurt < base / 4) return (MSG_HIT_SOFT);
	if (hurt < base / 3) return (MSG_HIT_MEDIUM);
	if (hurt < base / 2) return (MSG_HIT_HARD);
	else                 return (MSG_HIT_DEADLY);
}


/*
 * Decreases players hit points and sets death flag if necessary
 *
 * Hack -- this function allows the user to save (or quit) the game
 * when he dies, since the "You die." message is shown before setting
 * the player to "dead".
 *
 * Note: this function can heal the player with a negative argument -JM
 *
 * Return TRUE if the character dies or is leaving.
 */
bool take_hit(int dam, int msg_type, cptr hit_str, cptr kb_str)
{
	int old_chp = p_ptr->chp;

	int warning = (p_ptr->mhp * op_ptr->hitpoint_warn / 10);

	int delay = 0;
	char buf[80];


#ifdef ALLOW_BORG
	/* Mega-Hack -- Borg can't be hurt. */
	if (count_stop) return (FALSE);
#endif


	/* Character is dead or leaving */
	if ((p_ptr->is_dead) || (p_ptr->leaving)) return (TRUE);

	/* Disturb (vigorously) */
	if (dam > 0) disturb(2, 0);
	else         disturb(1, 0);

	/* Handle damage resistance */
	if ((p_ptr->res_dam) && (dam))
	{
		dam = div_round(2 * dam, 3);
	}


	/* We need to display a hit message */
	if (hit_str)
	{
		/* Option to adjust color according to severity of hit */
		if (!msg_type) msg_type = danger_color(old_chp, dam);

		/* Delay when badly hurt */
		if (msg_type == MSG_HIT_MEDIUM)
			delay = MAX(20, op_ptr->delay_factor * op_ptr->delay_factor * 2);
		if (msg_type == MSG_HIT_HARD)
			delay = MAX(50, op_ptr->delay_factor * op_ptr->delay_factor * 4);
		if (msg_type == MSG_HIT_DEADLY)
			delay = MAX(100, op_ptr->delay_factor * op_ptr->delay_factor * 8);

		/* Output the hit message */
		message(msg_type, delay, hit_str);
	}

	/* Hurt the player */
	p_ptr->chp -= dam;

	/* Display the hitpoints */
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	/* Dead player */
	if (p_ptr->chp < 0)
	{
		/* Hack -- Note death */
		message(MSG_DEATH, 0, "You die.");
		message_flush();

		/* Request appropriate music */
		music(MUSIC_DEATH + 100);

		/* Note cause of death */
		(void)my_strcpy(p_ptr->died_from, kb_str, sizeof(p_ptr->died_from));

		/* Note death in history */
		(void) strnfmt(buf, 80, "Killed by %s", kb_str);
		history_add(buf, HISTORY_PLAYER_DEATH, 0);

		/* No longer a winner */
		p_ptr->total_winner = FALSE;

		/* Note death */
		p_ptr->is_dead = TRUE;

		/* Leaving */
		p_ptr->leaving = TRUE;

		/* Dead */
		return (TRUE);
	}

	/* Optional hitpoint warning (display later) */
	if (alert_hitpoint && p_ptr->chp < warning)
	{
		p_ptr->hitpoint_warning = TRUE;
	}

	/* If character color changes with damage taken, redraw */
	if (colored_hurt_char) lite_spot(p_ptr->py, p_ptr->px);

	/* Not dead */
	return (FALSE);
}




/*
 * Calculate the number of map rows and columns the game will actually display.
 * This function should be called by any interface that resizes a term window.
 *
 * We could easily just display all we have space for, but pretty much
 * everything else about dungeon creation and display works with 11x11 blocks,
 * and therefore we accomodate this (by default) so as to keep the game
 * interface clean and tidy.  If you just can't stand this feature, an option
 * is available.
 */
void calc_map_size(byte cols, byte rows)
{
	if (map_display_precise_fit)
	{
		map_cols = cols;
		map_rows = rows;
	}
	else
	{
		map_cols = (cols / BLOCK_WID) * BLOCK_WID;
		map_rows = (rows / BLOCK_HGT) * BLOCK_HGT;
	}
}


/*
 * Calculate panel width, which varies depending on number of screen columns.
 */
int get_panel_wid(void)
{
	term *t = use_special_map ? term_map : term_main;
	int blocks;

	if      (t->cols < 4 * BLOCK_WID) blocks = 1;
	else if (t->cols < 6 * BLOCK_WID) blocks = 2;
	else                              blocks = 3;

	return (BLOCK_WID * blocks);
}

/*
 * Calculate panel height, which varies depending on number of screen rows.
 */
int get_panel_hgt(void)
{
	term *t = use_special_map ? term_map : term_main;
	int blocks;

	if      (t->rows < 4 * BLOCK_HGT) blocks = 1;
	else if (t->rows < 6 * BLOCK_HGT) blocks = 2;
	else                              blocks = 3;

	return (BLOCK_HGT * blocks);
}


/*
 * Modify the current panel to the given coordinates, adjusting only to
 * ensure the coordinates are legal, and return TRUE if anything done.
 *
 * Hack -- The town should never be scrolled around.
 *
 * Note that monsters are no longer affected in any way by panel changes.
 *
 * As a total hack, whenever the current panel changes, we assume that
 * the "overhead view" window should be updated.
 */
bool modify_panel(int wy, int wx)
{
	/* Verify wy, adjust if needed */
	if (wy > dungeon_hgt - map_rows) wy = dungeon_hgt - map_rows;
	if (wy < 0) wy = 0;

	/* Try to avoid scrolling around the town */
	if ((p_ptr->depth == 0) && (dungeon_hgt <= map_rows)) wy = 0;

	/* Verify wx, adjust if needed */
	if (wx > dungeon_wid - map_cols) wx = dungeon_wid - map_cols;
	if (wx < 0) wx = 0;

	/* Try to avoid scrolling around the town */
	if ((p_ptr->depth == 0) && (dungeon_wid <= map_cols)) wx = 0;

	/* React to changes */
	if ((p_ptr->wy != wy) || (p_ptr->wx != wx))
	{
		/* Save wy, wx */
		p_ptr->wy = wy;
		p_ptr->wx = wx;

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Hack -- Window stuff */
		p_ptr->window |= (PW_OVERHEAD);

		/* Changed */
		return (TRUE);
	}

	/* No change */
	return (FALSE);
}


/*
 * Shift the panel to ensure that the given location is contained inside
 * it, and return TRUE if any such adjustment was performed.
 */
bool adjust_panel(int y, int x)
{
	int wy = p_ptr->wy;
	int wx = p_ptr->wx;

	/* Adjust as needed */
	while (y >= wy + map_rows) wy += get_panel_hgt();
	while (y < wy)             wy -= get_panel_hgt();

	/* Adjust as needed */
	while (x >= wx + map_cols) wx += get_panel_wid();
	while (x < wx)             wx -= get_panel_wid();

	/* Use "modify_panel" */
	return (modify_panel(wy, wx));
}


/*
 * Shift the current panel one panel (usually) in the given direction.
 *
 * Return TRUE if the panel was changed.
 */
bool change_panel(int dir)
{
	/* Normally, change by one panel length/width at a time */
	int wy = p_ptr->wy + ddy[dir] * get_panel_hgt();
	int wx = p_ptr->wx + ddx[dir] * get_panel_wid();

	/* Try to butt up against the opposite wall for neatness */
	if (map_rows > 2 * BLOCK_WID)
	{
		/* Avoid that irksome last tiny panel change if already close */
		if ((ddy[dir] > 0) && (wy >= dungeon_hgt - map_rows - 8))
			wy = dungeon_hgt - map_rows;
		else if ((ddy[dir] < 0) && (wy <= 8))
			wy = 0;
		if ((ddx[dir] > 0) && (wx >= dungeon_wid - map_cols - 8))
			wx = dungeon_wid - map_cols;
		else if ((ddx[dir] < 0) && (wx <= 8))
			wx = 0;
	}


	/* Use "modify_panel" */
	return (modify_panel(wy, wx));
}


/*
 * Verify the current panel (relative to the player location).
 *
 * By default, when the player gets "too close" to the edge of the current
 * panel, the map scrolls one panel in that direction so that the player
 * is no longer so close to the edge.
 *
 * Use player-set options to precisely control how much clearance we
 * maintain.  If player wants maximal clearance in either direction, we
 * always keep the player centered.
 *
 * When looking, we try to show more space in front (often we can get
 * MAX_SIGHT), and sacrifice the view to the rear if necessary.  -LM-
 *
 * The calculations in this function are a little ugly.  XXX XXX
 */
void verify_panel(int dir, bool look)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dist_y;
	int dist_x;


	/*
	 * If asked for the maximum possible clearance, we shift the view in
	 * one grid increments.  This is the equivalent of "center_player".
	 */
	if ((!look) && ((clear_y >= (map_rows - get_panel_hgt()) / 2) ||
	                (clear_x >= (map_cols - get_panel_wid()) / 2)))
	{
		/* Center panel on character */
		modify_panel(py - map_rows / 2, px - map_cols / 2);

		/* We're done */
		return;
	}


	/* No (valid) direction given, or player out of sight */
	if ((dir < 1) || (dir == 5) || (dir > 9) || (!panel_contains(py, px)))
	{
		/* Determine minimum clearance */
		dist_y = (map_rows - get_panel_hgt() - (look ? 7 : 3)) / 2;
		dist_x = (map_cols - get_panel_wid() - (look ? 7 : 3)) / 2;

		/* With a direction of zero, we cut a lot of slack */
		if (dir == 0)
		{
			dist_y = 2;
			dist_x = 2;
		}

		/* Reduce required distance if near edge */
		if      (!in_bounds(py + dist_y, px)) dist_y = dungeon_hgt - py;
		else if (!in_bounds(py - dist_y, px)) dist_y = py - 0;
		if      (!in_bounds(py, px + dist_x)) dist_x = dungeon_wid - px;
		else if (!in_bounds(py, px - dist_x)) dist_x = px - 0;

		/* Require enough clearance in all directions */
		if ((!panel_contains(py + dist_y, px + dist_x)) ||
		    (!panel_contains(py - dist_y, px - dist_x)))
		{
			/*
			 * Get space (in half-panels, ignoring the first two) between the
			 * top of the panel and the center.
			 */
			int space_y = map_rows / get_panel_hgt() - 1;
			int space_x = map_cols / get_panel_wid() - 1;

			/* Get correct screen panel */
			int panel_y = (py - space_y * get_panel_hgt()/2) / get_panel_hgt();
			int panel_x = (px - space_x * get_panel_wid()/2) / get_panel_wid();

			/* Convert panel to grids, center view on character */
			(void)modify_panel(panel_y * get_panel_hgt(), panel_x * get_panel_wid());
		}
	}

	/* Check the panel; move it if necessary */
	else
	{
		/* When looking, try to give the best view practicable */
		if (look)
		{
			/* We leave just barely enough grids to the rear */
			dist_y = map_rows - get_panel_hgt() - 1 - clear_y;

			/* We can always get MAX_SIGHT horizontally */
			dist_x = MAX_SIGHT;
		}

		/* Compromise view and screen jerking - allow player control */
		else
		{
			/* Use given distance */
			dist_y = clear_y;
			dist_x = clear_x;
		}

		/* We don't need any more than MAX_SIGHT */
		if (dist_y > MAX_SIGHT) dist_y = MAX_SIGHT;
		if (dist_x > MAX_SIGHT) dist_x = MAX_SIGHT;

		/* We're too close to the edge in the direction of travel */
		if (!panel_contains(py + ddy[dir] * dist_y, px + ddx[dir] * dist_x))
		{
			/* We're not too close to the edge vertically */
			if (panel_contains(py + ddy[dir] * dist_y, px))
			{
				/* Only shift panel horizontally */
				if ((dir == 1) || (dir == 7)) dir = 4;
				if ((dir == 3) || (dir == 9)) dir = 6;
			}

			/* We're not too close to the edge horizontally */
			else if (panel_contains(py, px + ddx[dir] * dist_x))
			{
				/* Only shift panel vertically */
				if ((dir == 1) || (dir == 3)) dir = 2;
				if ((dir == 7) || (dir == 9)) dir = 8;
			}

			/* Shift panel */
			if (change_panel(dir))
			{
				/* Optional disturb on "panel change" */
				if (disturb_panel) disturb(0, 0);
			}
		}
	}
}


/*
 * Monster health description
 */
static void look_mon_desc(char *buf, int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	bool flag = FALSE;

	/* Describe a few special conditions */
	if (m_ptr->mflag & (MFLAG_MADD))
	{
		strcpy(buf, "insane");
		flag = TRUE;
	}
	else if (m_ptr->mflag & (MFLAG_WARY))
	{
		strcpy(buf, "wary");
		flag = TRUE;
	}
	else if (m_ptr->mflag & (MFLAG_TOWN))
	{
		strcpy(buf, "town");
		flag = TRUE;
	}
	else
	{
		strcpy(buf, "");
	}

	/* Display mana sometimes */
	if (r_ptr->mana)
	{
		/* Determine accuracy of mana reading */
		int accur = get_skill(S_WIZARDRY, 0, 100) - (2 * r_ptr->level / 3) - 20;
		if (know_mana(m_ptr->r_idx, l_ptr)) accur += 30;

		/* Require a reasonable amount of accuracy */
		if (accur >= 30)
		{
			int spread = 10 * m_ptr->mana / accur;

			/* Use the quick RNG */
			Rand_quick = TRUE;

			/* Hack -- use the mana of the monster to seed the RNG */
			Rand_value = m_ptr->mana;

			if (flag) strcat(buf, ", ");

			/* Calculate and show approximate, rounded mana */
			strcat(buf, format("~%d mana",
				(int)round_it(rand_spread(m_ptr->mana, spread), 8)));

			/* Stop using the quick RNG */
			Rand_quick = FALSE;
		}
	}
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
		while (!(*ang_sort_comp) (u, v, b, z)) b--;

		/* Slide i1 */
		while (!(*ang_sort_comp) (u, v, z, a)) a++;

		/* Done partition */
		if (a >= b) break;

		/* Swap */
		(*ang_sort_swap) (u, v, a, b);

		/* Advance */
		a++, b--;
	}
	/* Recurse left side */
	ang_sort_aux(u, v, p, b);

	/* Recurse right side */
	ang_sort_aux(u, v, b + 1, q);
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
	ang_sort_aux(u, v, 0, n - 1);
}





/*** Targeting Code ***/

/*
 * Hack - function to get object name of mimic.
 *
 * From Zangband.
 */
static bool mimic_desc(char *m_name, const monster_race *r_ptr)
{
	/* Hack - look at default character */
	switch (r_ptr->d_char)
	{
		case '$':
		{
			cptr name = (r_name + r_ptr->name);

			/* Look for textual clues */
			if (strstr(name, " copper "))     strcpy(m_name, "copper");
			if (strstr(name, " silver "))     strcpy(m_name, "silver");
			if (strstr(name, " gold "))       strcpy(m_name, "gold");
			if (strstr(name, " mithril "))    strcpy(m_name, "mithril");
			if (strstr(name, " adamantite ")) strcpy(m_name, "adamantite");
			return (TRUE);
		}

		case '|':
		{
			/* Hack */
			strcpy(m_name, r_name + r_ptr->name);
			return (TRUE);
		}

		case '?':
		{
			strcpy(m_name, "a Scroll");
			return (TRUE);
		}

		case '!':
		{
			strcpy(m_name, "a Potion");
			return (TRUE);
		}

		case '=':
		{
			strcpy(m_name, "a Ring");
			return (TRUE);
		}

		case '+':
		{
			if (strstr(m_name, "Sapphire"))
				strcpy(m_name, "a large sapphire");
			else if (strstr(m_name, "Ruby"))
				strcpy(m_name, "a large ruby");
			else if (strstr(m_name, "Emerald"))
				strcpy(m_name, "a large emerald");
			else if (strstr(m_name, "Diamond"))
				strcpy(m_name, "a large diamond");

			else strcpy(m_name, "a door");
			return (TRUE);
		}

		case '~':
		{
			cptr name = (r_name + r_ptr->name);

			if (strstr(name, "torch")) strcpy(m_name, "a torch");
			else strcpy(m_name, "a Large Jeweled chest");
			return (TRUE);
		}

		case '(':
		{
			strcpy(m_name, "a Cloak");
			return (TRUE);
		}

		case '.':
		{
			/* Hack - do not notice lurkers etc. */
			return (FALSE);
		}

		case '#':
		{
			strcpy(m_name, "a granite wall");
			return (TRUE);
		}

		default:
		{
			return (TRUE);
		}
	}
}


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
		act = keymap_act[mode][(byte) (ch)];

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
 * The concept of "targeting" was stolen from "Morgul" (?)
 *
 * The player can target any location, or any "target-able" monster.
 *
 * There are now two different rules for targeting monsters:
 * If this function is called in "use_sight" mode, we allow monsters in
 * line of sight.  Otherwise, we require line of fire.
 * This (hopefully) will allow the player to both use "closest target"
 * macros and to keep an eye on viewable monsters with the monster health
 * bar.
 */

bool target_able(int m_idx, bool use_sight)
{
	monster_type *m_ptr;

	int mx, my;

	/* No monster */
	if (m_idx <= 0) return (FALSE);

	/* Get monster */
	m_ptr = &m_list[m_idx];

	/* Monster must be alive */
	if (!m_ptr->r_idx) return (FALSE);

	/* Monster must be visible */
	if (!m_ptr->ml) return (FALSE);

	/* Monster must not be hidden */
	if (m_ptr->mflag & (MFLAG_MIME)) return (FALSE);

	/* Hack -- no targeting hallucinations */
	if (p_ptr->image) return (FALSE);

	/* Check maximum distance if necessary */
	if (p_ptr->max_dist > 0)
	{
		mx = m_ptr->fx;
		my = m_ptr->fy;
		if (p_ptr->max_dist < project_path(MAX_RANGE, p_ptr->py, p_ptr->px, &my, &mx, 0)) return (FALSE);
	}

	/* In manual mode */
	if (use_sight)
	{
		/* Monster must be in line of sight */
		if (!player_has_los_bold(m_ptr->fy, m_ptr->fx)) return (FALSE);
	}

	/* In automatic mode */
	else
	{
		/* Monster must be in line of fire (which means also line of sight) */
		if (!player_can_fire_bold(m_ptr->fy, m_ptr->fx)) return (FALSE);
	}

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

		/* Accept targets in line of fire */
		if (target_able(m_idx, FALSE))
		{
			monster_type *m_ptr = &m_list[m_idx];

			/* Get monster location */
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
	/* Acceptable target (need not be in line of fire) */
	if ((m_idx > 0) && target_able(m_idx, TRUE))
	{
		monster_type *m_ptr = &m_list[m_idx];

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

	/* Update the target display */
	left_panel_display(DISPLAY_TARGET, 0);
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
 * Given a direction, choose a location (see below)
 *
 * Rewritten to choose more correct directions.
 */
static s16b target_pick(int y0, int x0, int dy, int dx)
{
	int i, v;

	int x, y, ax, ay;

	int b_i = -1, b_v = 9999;

	int central_angle, rotate, angle, tmp, diff;

	/* Paranoia -- ignore non-movement */
	if (!dy && !dx) return (0);

	/* Get angle corresponding to this direction (240-degree circle) */
	central_angle = get_angle_to_grid[20 + dy][20 + dx];

	/* Get the rotation factor */
	rotate = 120 - central_angle;

	/* Scan the locations */
	for (i = 0; i < temp_n; i++)
	{
		/* Get the grid */
		x = temp_x[i];
		y = temp_y[i];

		/* Get absolute distance between source and target */
		ax = ABS(x - x0);
		ay = ABS(y - y0);

		/* Verify quadrant */
		if (dx && ((x - x0) * dx <= 0)) continue;
		if (dy && ((y - y0) * dy <= 0)) continue;

		/* Verify quadrant */
		if (dy && !dx && (ax > ay)) continue;
		if (dx && !dy && (ay > ax)) continue;


		/* Get angle to this grid */
		angle = get_angle_to_target(y0, x0, y, x, 0);

		/* Rotate it (wrap at 240 degrees) */
		tmp = ABS(angle + rotate) % 240;

		/* Get angular difference */
		diff = ABS(120 - tmp);


		/* Approximate Double Distance */
		v = ((ax > ay) ? (ax + ax + ay) : (ay + ay + ax));

		/* Add angular difference if great */
		if (diff > 12) v += (diff - 12) * 2;

		/* Track best */
		if (b_v > v)
		{
			b_i = i;
			b_v = v;
		}
	}

	/* Result */
	return (b_i);
}


/*
 * Hack -- determine if a given location is "interesting".
 *
 * Mimics are "interesting", even if hidden.  Most of the time, however,
 * the player will not actually notice them.
 *
 * We assume (y, x) is a legal grid.
 */
static bool target_set_interactive_accept(int y, int x)
{
	object_type *o_ptr;

	/* Player grids are always interesting */
	if (cave_m_idx[y][x] < 0) return (TRUE);


	/* Handle hallucination */
	if (p_ptr->image) return (FALSE);


	/* Visible monsters */
	if (cave_m_idx[y][x] > 0)
	{
		monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

		/* Visible monsters */
		if (m_ptr->ml) return (TRUE);
	}

	/* Scan all objects in the grid */
	if ((!p_ptr->blind) || ((y == p_ptr->py) && (x == p_ptr->px)))
	{
		for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
		{
			/* Memorized object */
			if (o_ptr->marked) return (TRUE);
		}
	}

	/* Notice traps and glyphs */
	if (cave_visible_trap(y, x)) return (TRUE);

	/* Interesting memorized features */
	if ((cave_info[y][x] & (CAVE_MARK)) &&
	    (f_info[cave_feat[y][x]].flags & (TF_INTERESTING)))
	{
		return (TRUE);
	}

	/* Nope */
	return (FALSE);
}


/*
 * Prepare the "temp" array for "target_interactive_set"
 */
static void target_set_interactive_prepare(int mode)
{
	int y, x;

	/* Reset "temp" array */
	temp_n = 0;

	/* Scan the current panel */
	for (y = p_ptr->wy; y < p_ptr->wy + map_rows; y++)
	{
		for (x = p_ptr->wx; x < p_ptr->wx + map_cols; x++)
		{
			/* Check bounds */
			if (!(in_bounds(y, x))) continue;

			/* Special aiming mode */
			if (mode & (TARGET_KILL))
			{
				/* Must contain a targetable monster */
				if (!target_able(cave_m_idx[y][x], FALSE)) continue;
			}

			/* Normal targetting mode */
			else
			{
				/* Require "interesting" contents */
				if (!target_set_interactive_accept(y, x)) continue;
			}

			/* Save the location */
			if (temp_n < TEMP_MAX - 1)
			{
				temp_x[temp_n] = x;
				temp_y[temp_n] = y;
				temp_n++;
			}
		}
	}

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
 * and terrain features in the same grid.
 *
 * This function uses Tim Baker's code to look at objects on the floor.
 *
 * This function must handle blindness/hallucination.
 */
static int target_set_interactive_aux(int y, int x, int mode, cptr info)
{
	object_type *o_ptr;
	s16b this_o_idx = 0, next_o_idx = 0;

	cptr s1, s2, s3;

	bool boring;

	int feat;

	int query;
	int count;
	char out_val[DESC_LEN];

	int floor_list[MAX_FLOOR_STACK+1], floor_num;


	/* Repeat forever */
	while (TRUE)
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
		if (cave_m_idx[y][x] < 0)
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
			(void)strnfmt(out_val, sizeof(out_val), "%s%s%s%s [%s]", s1, s2, s3, name, info);
			prt(out_val, 0, 0);
			move_cursor_relative(y, x);
			query = inkey(ALLOW_ALL);

			/* Stop on everything but "return" */
			if ((query != '\r') && (query != '\n')) break;

			/* Repeat forever */
			continue;
		}


		/* Actual monsters */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Monster is visible */
			if (m_ptr->ml)
			{
				char m_name[DESC_LEN];

				/* Monster is hidden in plain sight */
				if (m_ptr->mflag & (MFLAG_MIME))
				{
					/* Perceptive characters sometimes notice nearby mimics */
					if ((!p_ptr->image) && (!p_ptr->confused) &&
					    (m_ptr->ml >= ML_FULL) &&
					    (m_ptr->cdis < randint(get_skill(S_PERCEPTION, 0, 8))))
					{
						/* Get the monster name ("a kobold") */
						monster_desc(m_name, m_ptr, 0x88);

						/* Notice */
						prt(format("You notice %s!", m_name), 0, 0);
						m_ptr->mflag &= ~(MFLAG_MIME);

						/* Place cursor */
						move_cursor_relative(y, x);

						/* Wait for (any) keypress */
						(void)inkey(ALLOW_CLICK);
					}

					/* Most mimics look like something.  Some are camouflaged. */
					else if (mimic_desc(m_name, r_ptr))
					{
						/* Hack -- handle limited visibility */
						if (m_ptr->ml < ML_FULL)
							strcpy(m_name, "a bright light");

						/* Describe the monster */
						if (p_ptr->wizard)
						{
							(void)strnfmt(out_val, sizeof(out_val),
									  "%s%s%s [%s] (%d:%d)", s1, s2,
									  m_name, info, y, x);
						}
						else
						{
							(void)strnfmt(out_val, sizeof(out_val),
									  "%s%s%s [%s]", s1, s2, m_name, info);
						}

						/* Describe the monster */
						prt(out_val, 0, 0);

						/* Place cursor */
						move_cursor_relative(y, x);

						/* Wait for a keypress */
						query = inkey(ALLOW_ALL);

						/* Always stop at "normal" keys */
						if ((query != '\r') && (query != '\n') && (query != ' ')) break;

						/* Sometimes stop at "space" key */
						if ((query == ' ') && !(mode & TARGET_LOOK)) break;

						/* Change the intro */
						s1 = "It is ";

						/* Preposition */
						s2 = "on ";
					}
				}

				/* Monster is not hidden */
				if (!(m_ptr->mflag & (MFLAG_MIME)))
				{
					/* Limited monster visibility */
					if (m_ptr->ml < ML_FULL)
					{
						/* Not boring */
						boring = FALSE;

						/* Describe the monster */
						if (p_ptr->wizard)
						{
							(void)strnfmt(out_val, sizeof(out_val),
									  "%s%sa monster of some kind [%s] (%d:%d)",
									  s1, s2, info, y, x);
						}
						else
						{
							(void)strnfmt(out_val, sizeof(out_val),
									  "%s%sa monster of some kind [%s]", s1, s2, info);
						}

						/* Describe the monster */
						prt(out_val, 0, 0);

						/* Place cursor */
						move_cursor_relative(y, x);

						/* Wait for (a keypress */
						query = inkey(ALLOW_ALL);

						/* Stop on everything but "return"/"space" */
						if ((query != '\r') && (query != '\n') && (query != ' ')) break;

						/* Sometimes stop at "space" key */
						if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

						/* Change the intro */
						s1 = "It is ";

						/* Use a preposition */
						s2 = "on ";
					}

					/* Full visibility */
					else if (m_ptr->ml >= ML_FULL)
					{
						bool recall = FALSE;

						/* Not boring */
						boring = FALSE;

						/* Get the monster name ("a kobold") */
						monster_desc(m_name, m_ptr, 0x08);

						/* Hack -- track this monster race */
						monster_race_track(m_ptr->r_idx);

						/* Hack -- health bar for this monster */
						health_track(cave_m_idx[y][x]);

						/* Hack -- handle stuff */
						handle_stuff();

						/* Interact */
						while (TRUE)
						{
							/* Recall */
							if (recall)
							{
								/* Save screen */
								screen_save(FALSE);

								/* Recall on screen */
								screen_roff(m_ptr->r_idx);

								/* Hack -- Complete the prompt (again) */
								(void)Term_addstr(-1, TERM_WHITE, format("  [r,%s]", info));

								/* Command */
								query = inkey(ALLOW_ALL);

								/* Load screen */
								screen_load();
							}

							/* Normal */
							else
							{
								char buf[DESC_LEN];

								int cx, cy;

								/* Print conjunction and monster name */
								prt(format("%s%s%s%s ", s1, s2, s3, m_name), 0, 0);

								/* Obtain the cursor */
								(void)Term_locate(&cx, &cy);

								/* Print the monster health bar */
								health_redraw_aux(m_ptr, cy, cx);

								/* Describe monster conditions */
								look_mon_desc(buf, cave_m_idx[y][x]);

								/* Print special monster condition text */
								if (strlen(buf)) add_str(format(" (%s)", buf));

								/* Print available commands */
								add_str(format(" [r,%s]", info));

								/* Print coordinates (for wizards) */
								if (p_ptr->wizard)
								{
									add_str(format(" (%d:%d)", y, x));
								}

								/* Place cursor */
								move_cursor_relative(y, x);

								/* Command */
								query = inkey(ALLOW_ALL);
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

						/* Count objects being carried */
						for (count = 0, this_o_idx = m_ptr->hold_o_idx;
						                this_o_idx; this_o_idx = next_o_idx)
						{
							count++;
						}

						/* Note objects, but only vaguely */
						if (count)
						{
							if (count >= rand_range(9, 9 + (cave_m_idx[y][x] % 4)))
								s2 = "carrying a massive horde of objects.";
							else if (count >= rand_range(3, 3 + (cave_m_idx[y][x] % 3)))
								s2 = "carrying a large number of objects.";
							else if (count > 1)
								s2 = "carrying several objects.";
							else
								s2 = "carrying an object.";

							/* Describe the object */
							(void)strnfmt(out_val, sizeof(out_val), "%s%s%s [%s]", s1, s2, s3, info);
							prt(out_val, 0, 0);
							move_cursor_relative(y, x);
							query = inkey(ALLOW_CLICK);

							break;
						}

						/* Use a preposition */
						s2 = "on ";
					}
				}
			}
		}

		/* Traps */
		if ((cave_info[y][x] & (CAVE_TRAP)) && (!p_ptr->blind))
		{
			int i;
			int idx = 0;
			bool save = FALSE;

			/* Scan the current trap list */
			for (count = 0, i = 0; i < t_max; i++)
			{
				/* Point to this trap */
				trap_type *t_ptr = &t_list[i];

				/* Find a trap in this position */
				if ((t_ptr->fy == y) && (t_ptr->fx == x))
				{
					/* Trap must be visible */
					if (!(t_ptr->flags & (TRAP_VISIBLE))) continue;

					/* Count all traps */
					count++;

					/* Remember the last trap */
					idx = i;
				}
			}

			/* Require visible traps */
			if (count)
			{
				/* We have one trap (usual case) */
				if (count == 1)
				{
					/* Point to this trap */
					trap_type *t_ptr = &t_list[idx];

					s3 = "a ";

					/* Describe the trap */
					(void)strnfmt(out_val, sizeof(out_val), "%s%s%s%s [%s]", s1, s2, s3,
						t_kind_info[t_ptr->t_idx].name, info);
					prt(out_val, 0, 0);

					/* Trap contains objects */
					if (t_ptr->hold_o_idx)
					{
						char o_name[DESC_LEN];

						/* Save screen */
						screen_save(FALSE);
						save = TRUE;

						/* Display objects */
						for (i = 1, this_o_idx = t_ptr->hold_o_idx; this_o_idx;
							this_o_idx = next_o_idx, i++)
						{
							/* Get the object */
							o_ptr = &o_list[this_o_idx];

							/* Describe the object */
							object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

							/* Display this trap */
							put_str(format(" %.30s ", o_name), i, Term->cols - 61);

							/* Get the next object */
							next_o_idx = o_ptr->next_o_idx;
						}

						/* Display this trap */
						put_str(format(" %.30s ", ""), i, Term->cols - 61);
					}
				}

				/* We have more than one trap */
				else
				{
					s3 = "the following traps:";

					/* Save screen */
					screen_save(FALSE);
					save = TRUE;

					/* Describe the traps */
					(void)strnfmt(out_val, sizeof(out_val), "%s%s%s%s [%s]", s1, s2, s3, " ", info);
					prt(out_val, 0, 0);

					/* Display the individual traps */
					for (count = 0, i = 0; i < t_max; i++)
					{
						/* Point to this trap */
						trap_type *t_ptr = &t_list[i];

						/* Find a trap in this position */
						if ((t_ptr->fy == y) && (t_ptr->fx == x))
						{
							/* Trap must be visible */
							if (!(t_ptr->flags & (TRAP_VISIBLE))) continue;

							/* Display this trap */
							put_str(format(" %.30s ", t_kind_info[t_ptr->t_idx].name),
								count + 1, Term->cols - 61);

							count++;
						}
					}

					/* Print a blank line below the list of traps */
					if (count) put_str(format(" %.30s ", ""), count + 1, Term->cols - 61);
				}

				s3 = "";

				/* Select grid, wait for command */
				move_cursor_relative(y, x);
				query = inkey(ALLOW_ALL);

				/* Load screen if we were showing any sort of list */
				if (screen_depth) screen_load();

				/* Stop on everything but "return"/"space" */
				if ((query != '\r') && (query != '\n') && (query != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

				/* We might have a trap recall key at some point */
			}
		}


		/* Scan all marked objects in the grid */
		if ((scan_floor(floor_list, &floor_num, y, x, 0x02)) &&
		    (!p_ptr->blind || (y == p_ptr->py && x == p_ptr->px)))
		{
			/* Not boring */
			boring = FALSE;

			if (floor_num == 1)
			{
				char o_name[DESC_LEN];

				/* Get object */
				o_ptr = &o_list[floor_list[0]];

				/* Describe the object */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

				/* Message */
				if (p_ptr->wizard)
				{
					(void)strnfmt(out_val, sizeof(out_val),
							  "%s%s%s%s [%s] (%d:%d)",
							  s1, s2, s3, o_name, info, y, x);
				}
				else
				{
					(void)strnfmt(out_val, sizeof(out_val),
							  "%s%s%s%s [%s]", s1, s2, s3, o_name, info);
				}
			}
			else
			{
				bool old_show_weights = (show_weights != 0);

				/* Message */
				if (p_ptr->wizard)
				{
					(void)strnfmt(out_val, sizeof(out_val),
							  "%s%s%sa pile of %d items [%s] (%d:%d)",
							  s1, s2, s3, floor_num, info, y, x);
				}
				else
				{
					(void)strnfmt(out_val, sizeof(out_val),
							  "%s%s%sa pile of %d items [%s]",
							  s1, s2, s3, floor_num, info);
				}

				/* Save screen */
				screen_save(FALSE);

				/* Hack -- do not show weights */
				show_weights = FALSE;

				/* Display the list of objects (including gold) */
				show_floor(floor_list, floor_num, TRUE,
					!((cave_info[y][x] & (CAVE_SEEN)) || (cave_info[y][x] & (CAVE_GLOW))));

				/* Restore option */
				show_weights = old_show_weights;
			}

			/* Print info, move cursor back to floor grid */
			prt(out_val, 0, 0);
			move_cursor_relative(y, x);

			/* Wait for it */
			query = inkey(ALLOW_ALL);

			/* We are showing any sort of list */
			if (screen_depth)
			{
				/* Load screen, but do NOT refresh yet */
				screen_load();
			}

			/* Stop on normal keys */
			if ((query != '\r') && (query != '\n') && (query != ' ')) break;
		}

		/* Feature (apply "mimic") */
		feat = f_info[cave_feat[y][x]].mimic;

		/* Require knowledge about grid, or ability to see grid */
		if (!(cave_info[y][x] & (CAVE_MARK)) && !player_can_see_or_infra_bold(y,x))
		{
			/* Forget feature */
			feat = FEAT_NONE;
		}

		/* Terrain feature (when appropriate) */
		if (boring || (!cave_floor_bold(y, x)))
		{
			cptr name = f_name + f_info[feat].name;

			bool recall = FALSE;
			bool can_recall;

			/* Hack -- handle unknown grids */
			if (feat == FEAT_NONE) name = "unexplored area";

			/* Pick a prefix */
			if (*s2 && (feat >= FEAT_DOOR_HEAD)) s2 = "in ";

			/* Pick proper indefinite article */
			s3 = (my_is_vowel(name[0])) ? "an " : "a ";

			/* Hack -- special treatment for certain terrain features. */
			if ((feat == FEAT_WATER) || (feat == FEAT_LAVA) || (feat == FEAT_TREE))
			{
				s3 = "";
			}

			/* Hack -- special introduction for store doors */
			if (cave_shop_bold(y, x))
			{
				/* Hack -- special name for the Inn */
				if (feat == FEAT_SHOP_INN)
				{
					name = format("%s", inn_names[p_ptr->inn_name]);
				}
				s3 = "the entrance to ";
			}

			/* If this terrain feature has a description, allow it to be shown */
			can_recall = (f_info[feat].text != 0);

			/* Interact */
			while (TRUE)
			{
				/* Recall */
				if (recall)
				{
					cptr desc = f_text + f_info[feat].text;

					/* Save screen */
					screen_save(FALSE);

					/* Begin recall */
					clear_space(1, (Term->cols - 80) / 2, 80);

					/* 1-space left margin */
					text_border_left = 1;

					/* Describe the terrain */
					roff(desc, (Term->cols - 80) / 2, text_out_indent + 80);

					/* Create a blank line */
					roff("\n", (Term->cols - 80) / 2, text_out_indent + 80);

					/* No left border */
					text_border_left = 0;
				}

				/* Display a message */
				(void)strnfmt(out_val, sizeof(out_val), "%s%s%s%s", s1, s2, s3, name);
				prt(out_val, 0, 0);

				/* List available commands */
				if (can_recall)
				{
					(void)Term_addstr(-1, TERM_WHITE, format(" [r,%s]", info));
				}
				else
				{
					(void)Term_addstr(-1, TERM_WHITE, format(" [%s]", info));
				}

				/* Print coordinates (for wizards) */
				if (p_ptr->wizard)
				{
					add_str(format(" (%d:%d)", y, x));
				}

				/* Place cursor */
				move_cursor_relative(y, x);

				/* Command */
				query = inkey(ALLOW_ALL);


				/* Load screen */
				if (recall) screen_load();


				/* No recall if forbidden */
				if (!can_recall) break;

				/* Normal commands */
				if (query != 'r') break;

				/* Toggle recall */
				recall = !recall;
			}

			/* Always stop at "normal" keys */
			if ((query != '\r') && (query != '\n') && (query != ' ')) break;

			/* Sometimes stop at "space" key */
			if ((query == ' ') && !(mode & (TARGET_LOOK))) break;
		}

		/* Stop on everything but "return" */
		if ((query != '\r') && (query != '\n')) break;
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
 * Hack -- targeting/observing an "outer border grid" may induce
 * problems, so this is not currently allowed.
 *
 * The player can use the direction keys to move among "interesting"
 * grids, or the "space", "+", and "-" keys to move through these grids
 * sequentially, or can enter "location" mode, and use the direction
 * keys to move one grid at a time in any direction.  The "t" (set
 * target) command will only target a monster (as opposed to a
 * location) if the monster is target_able and the "interesting" mode
 * is being used.
 *
 * The current grid is described using the "look" method above, and
 * a new command may be entered at any time, but note that if the
 * "TARGET_LOOK" bit flag is set (or if we are in "location" mode,
 * where "space" has no obvious meaning) then "space" will scan
 * through the description of the current grid until done, instead
 * of immediately jumping to the next "interesting" grid.  This
 * allows the "target" command to retain its old effects.
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
bool target_set_interactive(u16b mode)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, d, m;

	int y = py;
	int x = px;

	bool done = FALSE;
	bool shift_key = FALSE;

	/* Save old map cursor state XXX */
	char map_cursor_vis = inkey_cursor_hack[TERM_MAP];

	bool flag = TRUE;
	bool jump = TRUE;

	char query;

	char info[DESC_LEN];


	/* Cancel target  (always?) XXX */
	target_set_monster(0);

	/* Show the map cursor  XXX */
	inkey_cursor_hack[use_special_map ? TERM_MAP : TERM_MAIN] = 1;

	/*
	 * Hack -- Start out by selecting any grid by using the TARGET_GRID
	 * flag.  -TNB-
	 */
	if (mode & (TARGET_GRID))
	{
		if (!(mode & (TARGET_FIRE))) flag = FALSE;
		mode &= ~(TARGET_GRID);
	}

	/* Handle look requests using the mouse */
	if (mode & (TARGET_MOUS))
	{
		/* Jump to the mouse position */
		y = cur_mouse_action.y + p_ptr->wy;
		x = cur_mouse_action.x + p_ptr->wx;

		/* Hack -- start off looking at all grids */
		flag = FALSE;
	}


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
			if (jump)
			{
				y = temp_y[m];
				x = temp_x[m];
			}
			jump = TRUE;

			/* Allow target */
			if ((cave_m_idx[y][x] > 0) &&
			    (target_able(cave_m_idx[y][x], !(mode & (TARGET_FIRE)))))
			{
				strcpy(info, "t,p,o,+,-,?,<dir>");
			}
			else
			{
				strcpy(info, "p,o,+,-,?,<dir>");
			}

			/* Describe and Prompt */
			query = target_set_interactive_aux(y, x, mode, info);

			/* Allow all direction keys */
			get_ui_direction(&query, 0x00, &shift_key);

			/* Assume no "direction" */
			d = 0;

			/* Analyze */
			switch (query)
			{
				case ESCAPE:
				{
					done = TRUE;
					break;
				}

				case ' ':
				case '*':
				case '+':
				case '=':
				{
					if (++m == temp_n)
					{
						m = 0;
					}
					break;
				}

				case '-':
				{
					if (m-- == 0)
					{
						m = temp_n - 1;
					}
					break;
				}

				case 'p':
				{
					/* Recenter around player */
					verify_panel(0, FALSE);

					/* Handle stuff */
					handle_stuff();

					y = py;
					x = px;

					/* Do not jump anywhere */
					jump = FALSE;

					break;
				}

				case 'o':
				{
					flag = !flag;
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
					int m_idx = cave_m_idx[y][x];

					if ((m_idx > 0) && target_able(m_idx, !(mode & (TARGET_FIRE))))
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

				case '?':
				{
					p_ptr->get_help_index = HELP_TARGET;
					do_cmd_help();
					break;
				}

				case MOUSEKEY:
				{
					/* Don't move anywhere */
					jump = FALSE;

					/* Ignore if not in map */
					if (((use_special_map) && (cur_mouse_action.term != TERM_MAP)) ||
					    ((!use_special_map) && (cur_mouse_action.term != TERM_MAIN)))
					{
						break;
					}

					/* Jump to location if legal */
					if (in_bounds(cur_mouse_action.y, cur_mouse_action.x))
					{
						/* Save the current location */
						x = p_ptr->wx + cur_mouse_action.x;
						y = p_ptr->wy + cur_mouse_action.y;

						/*
						 * If we are not using the special map, we need
						 * to convert to map coordinates  XXX XXX
						 */
						if (!use_special_map)
						{
							x -= COL_MAP;
							y -= ROW_MAP;
						}

						/* Go there */
						move_cursor_relative(y, x);
					}
					else break;

					/* Target on left double-click */
					if (cur_mouse_action.button == MOUSE_L_DBLCLICK)
					{
						/* Require a legal monster */
						int m_idx = cave_m_idx[y][x];

						if ((m_idx > 0) && target_able(m_idx, !(mode & (TARGET_FIRE))))
						{
							health_track(m_idx);
							target_set_monster(m_idx);
							done = TRUE;
						}
						else
						{
							target_set_location(y, x);
							done = TRUE;
						}
					}

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
					int old_wy = p_ptr->wy;
					int old_wx = p_ptr->wx;

					/* Change if legal */
					if (change_panel(d))
					{
						/* Recalculate interesting grids */
						target_set_interactive_prepare(mode);

						/* Find a new monster */
						i = target_pick(old_y, old_x, ddy[d], ddx[d]);

						/* Restore panel if needed */
						if ((i < 0) && modify_panel(old_wy, old_wx))
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
			strcpy(info, "t,p,m,+,-,?,<dir>");

			/* Describe and Prompt (enable "TARGET_LOOK") */
			query = target_set_interactive_aux(y, x, mode | TARGET_LOOK, info);

			/* Allow all direction keys and shift-movement */
			get_ui_direction(&query, 0x00, &shift_key);

			/* Assume no direction */
			d = 0;

			/* Analyze the keypress */
			switch (query)
			{
				case ESCAPE:
				{
					done = TRUE;
					break;
				}

				case ' ':
				case '*':
				case '+':
				case '=':
				case '-':
				{
					break;
				}

				case 'p':
				{
					/* Scroll screen and move cursor to player */
					verify_panel(0, FALSE);

					/* Handle stuff */
					handle_stuff();

					y = py;
					x = px;

					break;
				}

				case 'o':
				{
					break;
				}

				case 'm':
				{
					flag = !flag;
					break;
				}

				case 't':
				case '5':
				case '0':
				{
					target_set_location(y, x);
					done = TRUE;
					break;
				}

				case '?':
				{
					p_ptr->get_help_index = HELP_TARGET;
					do_cmd_help();
					break;
				}

				case MOUSEKEY:
				{
					/* Don't move anywhere */
					jump = FALSE;

					/* Ignore if not in map */
					if (((use_special_map) && (cur_mouse_action.term != TERM_MAP)) ||
					    ((!use_special_map) && (cur_mouse_action.term != TERM_MAIN)))
					{
						break;
					}

					/* Jump to location if legal */
					if (in_bounds(cur_mouse_action.y, cur_mouse_action.x))
					{
						/* Save the current location */
						x = p_ptr->wx + cur_mouse_action.x;
						y = p_ptr->wy + cur_mouse_action.y;

						/*
						 * If we are not using the special map, we need
						 * to convert to map coordinates  XXX XXX
						 */
						if (!use_special_map)
						{
							x -= COL_MAP;
							y -= ROW_MAP;
						}

						/* Go there */
						move_cursor_relative(y, x);
					}
					else break;

					/* Target on left double-click */
					if (cur_mouse_action.button == MOUSE_L_DBLCLICK)
					{
						target_set_location(y, x);
						done = TRUE;
					}

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
				/* Move quickly */
				if (shift_key)
				{
					int mag = BLOCK_HGT;  /* Size of a dungeon block */

					x += ddx[d] * mag;
					y += ddy[d] * mag;
				}

				/* Move one grid */
				else
				{
					x += ddx[d];
					y += ddy[d];
				}

				/* Cannot leave the dungeon */
				if      (x > dungeon_wid - 1) x = dungeon_wid - 1;
				else if (x < 0) x = 0;
				if      (y > dungeon_hgt - 1) y = dungeon_hgt - 1;
				else if (y < 0) y = 0;

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

	/* Recenter the map around the player */
	verify_panel(0, FALSE);

	/* Restore previous map cursor visibility */
	inkey_cursor_hack[use_special_map ? TERM_MAP : TERM_MAIN] = map_cursor_vis;

	/* Update the target display */
	left_panel_display(DISPLAY_TARGET, 0);

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

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
 * Note that "Force Target", if set, will preempt user interaction,
 * if there is a usable target already set.
 *
 * Currently this function applies confusion directly.
 */
bool get_aim_dir(int *dp)
{
	int dir;

	char ch;

	cptr p;

	/* Handle repeated commands.  -TNB- */
	if (repeat_pull(dp))
	{
		/* Verify */
		if (!(*dp == 5 && !target_okay()))
		{
			p_ptr->max_dist = 0;
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
	if (use_old_target && target_okay()) dir = 5;

	/* Ask until satisfied */
	while (!dir)
	{
		/* Choose a prompt */
		if (!target_okay())
		{
			p = "Direction ('*' to choose a target, '.' for closest, Escape to cancel)?";
		}
		else
		{
			p = "Direction ('5' for target, '*' to re-target, Escape to cancel)?";
		}

		/* Get a command (or Cancel) */
		if (!get_com(p, &ch)) break;

		/* Analyze */
		switch (ch)
		{
			/* Set new target, use target if legal */
			case '*':
			case ' ':
			case '\r':
			case '\n':
			{
				if (target_set_interactive(TARGET_KILL | TARGET_FIRE)) dir = 5;
				break;
			}

			/* Target the closest visible monster in line of fire */
			case '.':
			{
				int n = 0;

				/* Check closest monsters */
				while (TRUE)
				{
					int y = 0, x = 0;

					/* Get next monster */
					n++;

					/* Find the 'n'th closest viewable, visible monster */
					get_closest_los_monster(n, p_ptr->py, p_ptr->px, &y, &x, TRUE);

					/* We have a valid target */
					if ((y) && (x) && (cave_m_idx[y][x] > 0))
					{
						/* Get monster index */
						int m_idx = cave_m_idx[y][x];

						/* Monster must be in line of fire */
						if (target_able(m_idx, FALSE))
						{
							health_track(m_idx);
							target_set_monster(m_idx);
							dir = 5;
							break;
						}
					}

					/* We've run out of targetable monsters */
					else
					{
						bell("No targetable monsters!");
						break;
					}
				}
				break;
			}

			/* Use current target, if set and legal */
			case 't':
			case '5':
			case '0':
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
	if (!dir)
	{
		p_ptr->max_dist = 0;
		return (FALSE);
	}

	/* Save the direction */
	p_ptr->command_dir = dir;

	/* Check for confusion */
	if (p_ptr->confused)
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

	/* Save command */
	repeat_push(dir);

	/* Clear max distance */
	p_ptr->max_dist = 0;

	/* A "valid" direction was entered */
	return (TRUE);
}



/*
 * Request a "movement" direction (1,2,3,4,6,7,8,9) from the user.
 *
 * Return TRUE if a direction was chosen, otherwise return FALSE.
 *
 * This function should be used for all "repeatable" commands, such as
 * run, walk, open, close, disarm, spike, tunnel, etc, as well
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

	cptr p;

	/* Handle repeated commands.  -TNB- */
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
		p = "Direction (Escape to cancel)?";

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

	/* Save command */
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
	if (p_ptr->confused)
	{
		/* Apply confusion XXX XXX XXX */
		if ((dir == 5) || (!one_in_(4)))
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
 * Handle messages generated by precognition.  -LM-
 *
 * Store messages in an array.  When requested, print them all at once.
 */
void precog_msg(int precog_msg_idx)
{
	int i;
	static s16b msg_store[PRECOG_MSG_INDEX_MAX];

	/* Store a new message (no messages in town) */
	if ((precog_msg_idx > 0) && (precog_msg_idx < PRECOG_MSG_INDEX_MAX))
	{
		if (p_ptr->depth) msg_store[precog_msg_idx]++;
	}

	/* Wipe the messages */
	else if (precog_msg_idx == PRECOG_WIPE)
	{
		for (i = 0; i < PRECOG_MSG_INDEX_MAX; i++)
		{
			msg_store[i] = 0;
		}
	}

	/* Print out the messages */
	else if (precog_msg_idx == PRECOG_DISPLAY)
	{
		char buf[DESC_LEN];
		cptr s;
		char *t;

		/* Scan through the possible precognition messages */
		for (i = 1; i < PRECOG_MSG_INDEX_MAX; i++)
		{
			/* We have at least one of this type of message */
			if (msg_store[i])
			{
				/* Scan the raw text */
				for (s = precog_msg_text[i], t = buf; *s; s++)
				{
					/* Look for a '&' */
					if (*s == '&')
					{
						/* Only one message of this type */
						if (msg_store[i] == 1)
						{
							/* Insert an 'a' */
							*t++ = 'a';

							/* Next non-space is a vowel - insert an 'n' */
							if (my_is_vowel(*(s+2))) *t++ = 'n';
						}

						/* Multiple messages of this type */
						else
						{
							/* Insert the digits */
							object_desc_num_macro(t, msg_store[i]);
						}
					}

					/* Look for a '*' */
					else if (*s == '*')
					{
						/* Insert a 's', if plural */
						if (msg_store[i] > 1) *t++ = 's';
					}

					/* Look for a '@' */
					else if (*s == '@')
					{
						/* More than one message */
						if (msg_store[i] > 1)
						{
							/* Insert the digits */
							object_desc_num_macro(t, msg_store[i]);

							/* Insert "time" or "times"  XXX */
							*t++ = ' ';     *t++ = 't';
							*t++ = 'i';     *t++ = 'm';
							*t++ = 'e';     *t++ = 's';
						}

						/* One message */
						else
						{
							/* Delete any preceding space */
							if (*(t-1) == ' ') t--;
						}
					}

					/* Handle ordinary characters */
					else
					{
						*t++ = *s;
					}
				}

				/* End the string */
				*t++ = '\0';

				/* Display the edited message */
				msg_format("%s", buf);
			}
		}

		/* Display all the messages */
		flush();
	}
}


/*
 * Rate the current level of danger and excitement on a scale of 0 to 5, with
 * 5 being the highest.  Output is intended for use in the jukebox code.  -LM-
 *
 * If "change_right_now" is TRUE, we insist on the music changing immediately
 * if danger level has.
 */
void danger_music_level(bool change_right_now)
{
#ifdef USE_SOUND
	int i;
	monster_type *m_ptr;
	monster_race *r_ptr;

	bool is_dungeon = (p_ptr->depth != 0);

	/* Danger ranges from 1 to 5 */
	int danger;
	int mcount = 0;


	/* Require correct options */
	if ((use_sound != MUSIC_ONLY) && (use_sound != SOUND_AND_MUSIC)) return;

	/* Efficiency -- skip most turns if running or resting */
	if ((!change_right_now) && ((p_ptr->running) || (p_ptr->resting)) &&
		(Rand_simple(5) != 0)) return;


	/* We're in town and not seriously wounded */
	if ((!is_dungeon) && (p_ptr->chp > p_ptr->mhp / 2))
	{
		/* Scan the monster list */
		for (i = 1; i < m_max; i++)
		{
			/* Get this monster */
			m_ptr = &m_list[i];
			r_ptr = &r_info[m_ptr->r_idx];

			/* Paranoia -- skip "dead" monsters */
			if (!m_ptr->r_idx) continue;

			/* Check for visibility */
			if (!m_ptr->ml || (m_ptr->mflag & (MFLAG_MIME))) continue;

			/* If monster's level is at least half our own, treat as a dungeon */
			if ((r_ptr->level) && (r_ptr->level >= p_ptr->power / 2))
			{
				is_dungeon = TRUE;
				break;
			}
		}

		/* If this is a safe town, */
		if (!is_dungeon)
		{
			/* Request (or insist upon) town music */
			music(change_right_now ? 100 : 0);
			return;
		}
	}

	/* This is the dungeon (or being treated as such for our purposes) */
	danger = 1;


	/* Check the monsters */
	for (i = 1; i < m_max; i++)
	{
		/* Get the monster */
		m_ptr = &m_list[i];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- skip "dead" monsters */
		if (!m_ptr->r_idx) continue;

		/* Check for visibility */
		if (!m_ptr->ml || (m_ptr->mflag & (MFLAG_MIME))) continue;

		/* Must be in field of view or nearby */
		if (!(cave_info[m_ptr->fy][m_ptr->fx] & (CAVE_VIEW)) && (m_ptr->cdis >= 4)) continue;

		/* Ignore monsters without ranged attacks unless close */
		if ((!r_ptr->freq_ranged) && (m_ptr->cdis > 6)) continue;

		/* Ignore fleeing monsters unless injured */
		if ((m_ptr->min_range == FLEE_RANGE) && (p_ptr->chp >= p_ptr->mhp)) continue;

		/*  Tally up total danger level */
		if (r_ptr->flags1 & (RF1_UNIQUE | RF2_PLAYER_GHOST)) mcount += 8;

		if      (r_ptr->level >= 2 + (4 * p_ptr->power / 3)) mcount += 8;
		else if (r_ptr->level >= p_ptr->power)               mcount += 4;
		else if (r_ptr->level >= 2 * p_ptr->power / 3)       mcount += 2;
		else                                                 mcount += 1;
	}

	/* There are nearby monsters */
	if (mcount)
	{
		/* Adjust danger level for injuries */
		if (p_ptr->chp < p_ptr->mhp) mcount = mcount * (p_ptr->mhp + 1) / (p_ptr->chp + 1);

		/* Extract a danger level (may need revision) */
		if (mcount) danger = MIN(5, rsqrt(mcount));
	}

	/* Adjust for level feeling (if we know it) */
	if (!no_feeling_yet)
	{
		if      (feeling == 1) danger = MAX(danger, 2);
		else if (feeling == 2) danger = MAX(danger, 4);
		else if (feeling == 3) danger = MAX(danger, 3);
		else if (feeling == 4) danger = MAX(danger, 3);
		else if (feeling == 5) danger = MAX(danger, 2);
		else if (feeling == 6) danger = MAX(danger, 2);
		else if (feeling == 7) danger = MAX(danger, 2);
	}

	/* Adjust for quest */
	if ((p_ptr->cur_quest) && (p_ptr->cur_quest == p_ptr->depth))
	{
		danger = MAX(danger, 2);
	}

	/* Adjust for being within a vault */
	if (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ICKY))
	{
		danger = MAX(danger, 2);
	}

	/* Adjust for injuries (unless resting) */
	if ((p_ptr->chp <= p_ptr->mhp / 2) && (!p_ptr->resting))
	{
		if (p_ptr->chp <= p_ptr->mhp / 8) danger = MAX(danger, 4);
		if (p_ptr->chp <= p_ptr->mhp / 4) danger = MAX(danger, 3);
		else                              danger = MAX(danger, 2);
	}

	/* Request (or insist upon) an appropriate song */
	music(change_right_now ? danger + 100 : danger);

#endif /* USE_SOUND */
}

