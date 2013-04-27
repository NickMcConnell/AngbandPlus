/* File: spells2.c */

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
#include "project.h"
#include "ego.h"
#include "option.h"
#include "raceflag.h"
#include "tvalsval.h"

#include "keypad.h"

/*
 * Bit flags for the "enchant()" function
 */
#define ENCH_TOHIT   0x01
#define ENCH_TODAM   0x02
#define ENCH_TOAC    0x04


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

		/* Heal 0-4 */
		if (num < 5)
		{
			msg_print("You feel a little better.");
		}

		/* Heal 5-14 */
		else if (num < 15)
		{
			msg_print("You feel better.");
		}

		/* Heal 15-34 */
		else if (num < 35)
		{
			msg_print("You feel much better.");
		}

		/* Heal 35+ */
		else
		{
			msg_print("You feel very good.");
		}

		/* Notice */
		return (TRUE);
	}

	/* Ignore */
	return (FALSE);
}



/*
 * Leave a "glyph of warding" which prevents monster movement
 */
void warding_glyph(void)
{
	int py = p_ptr->loc.y;
	int px = p_ptr->loc.x;

	/* XXX XXX XXX */
	if (!cave_clean_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return;
	}

	/* Create a glyph */
	cave_set_feat(py, px, FEAT_GLYPH);
}




/*
 * Array of stat "descriptions"
 */
static const char* const desc_stat_pos[] =
{
	"strong",
	"smart",
	"wise",
	"dextrous",
	"healthy",
	"cute"
};


/*
 * Array of stat "descriptions"
 */
static const char* const desc_stat_neg[] =
{
	"weak",
	"stupid",
	"naive",
	"clumsy",
	"sickly",
	"ugly"
};


/*
 * Lose a "point"
 */
bool do_dec_stat(stat_index stat)
{
	/* Sustain */
	if (p_ptr->sustain[stat])
	{
		/* Message */
		msg_format("You feel very %s for a moment, but the feeling passes.",
		           desc_stat_neg[stat]);

		/* Notice effect */
		return (TRUE);
	}

	/* Attempt to reduce the stat */
	if (dec_stat(stat, 10, FALSE))
	{
		/* Message */
		message_format(MSG_DRAIN_STAT, stat, "You feel very %s.", desc_stat_neg[stat]);

		/* Notice effect */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}


/*
 * Restore lost "points" in a stat
 */
bool do_res_stat(stat_index stat)
{
	/* Attempt to increase */
	if (res_stat(stat))
	{
		/* Message */
		msg_format("You feel less %s.", desc_stat_neg[stat]);

		/* Notice */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}


/*
 * Gain a "point" in a stat
 */
bool do_inc_stat(stat_index stat)
{
	bool res = res_stat(stat);	/* restore */

	/* Attempt to increase */
	if (inc_stat(stat))
	{
		/* Message */
		msg_format("You feel very %s!", desc_stat_pos[stat]);

		/* Notice */
		return (TRUE);
	}

	/* Restoration worked */
	if (res)
	{
		/* Message */
		msg_format("You feel less %s.", desc_stat_neg[stat]);

		/* Notice */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}



/*
 * Identify everything being carried.
 * Done by a potion of "self knowledge".
 */
void identify_pack(void)
{
	int i;

	assert(0 <= p_ptr->inven_cnt && INVEN_PACK >= p_ptr->inven_cnt && "precondition");
	assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");

	/* Simply identify and know every item */
	for (i = 0; i < p_ptr->inven_cnt; ++i)
	{
		object_type* const o_ptr = &p_ptr->inventory[i];

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP);
}






/*
 * Used by the "enchant" function (chance of failure)
 */
static const int enchant_table[16] =
{
	0, 10,  50, 100, 200,
	300, 400, 500, 700, 950,
	990, 992, 995, 997, 999,
	1000
};


/*
 * Hack -- Removes curse from an object.
 */
static void uncurse_object(object_type *o_ptr)
{
	/* Uncurse it */
	o_ptr->ident &= ~(IDENT_CURSED);

	o_ptr->sense(INSCRIP_UNCURSED);		/* note the uncursing */
}


/*
 * Removes curses from items in inventory.
 *
 * Note that Items which are "Perma-Cursed" (The One Ring,
 * The Crown of Morgoth) can NEVER be uncursed.
 *
 * Note that if "all" is FALSE, then Items which are
 * "Heavy-Cursed" (Mormegil, Calris, and Weapons of Morgul)
 * will not be uncursed.
 */
static int remove_curse_aux(bool all)
{
	int i, cnt = 0;

	/* Attempt to uncurse items being worn */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		u32b f[OBJECT_FLAG_STRICT_UB];

		object_type* const o_ptr = &p_ptr->inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Uncursed already */
		if (!o_ptr->is_cursed()) continue;

		/* Extract the flags */
		object_flags(o_ptr, f);

		/* Heavily Cursed Items need a special spell */
		if (!all && (f[2] & (TR3_HEAVY_CURSE))) continue;

		/* Perma-Cursed Items can NEVER be uncursed */
		if (f[2] & (TR3_PERMA_CURSE)) continue;

		/* Uncurse the object */
		uncurse_object(o_ptr);

		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->redraw |= (PR_EQUIP);

		/* Count the uncursings */
		cnt++;
	}

	/* Return "something uncursed" */
	return (cnt);
}


/*
 * Remove most curses
 */
bool remove_curse(void)
{
	return (remove_curse_aux(FALSE));
}

/*
 * Remove all curses
 */
bool remove_all_curse(void)
{
	return (remove_curse_aux(TRUE));
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
		msg_print("You feel your life energies returning.");

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
 * Hack -- acquire self knowledge
 *
 * List various information about the player and/or his current equipment.
 *
 * See also "identify_fully()".
 *
 * Use the "roff()" routines, perhaps.  XXX XXX XXX
 *
 * Use the "show_file()" method, perhaps.  XXX XXX XXX
 *
 * This function cannot display more than 20 lines.  XXX XXX XXX
 */
void self_knowledge(void)
{
	int i = 0, j, k;
	int max_x = Term->hgt - 2;

	u32b f[OBJECT_FLAG_STRICT_UB];

	const object_type* o_ptr;

	const char* info[128];


	/* Get item flags from equipment */
	for (k = INVEN_WIELD; k < INVEN_TOTAL; k++)
	{
		u32b t[OBJECT_FLAG_STRICT_UB];

		o_ptr = &p_ptr->inventory[k];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the flags */
		object_flags(o_ptr, t);

		/* Extract flags */
		for (i = 0; i < OBJECT_FLAG_STRICT_UB; ++i)
			f[i] |= t[i];
	}


	if (p_ptr->timed[TMD_BLIND])
	{
		info[i++] = "You cannot see.";
	}
	if (p_ptr->timed[TMD_CONFUSED])
	{
		info[i++] = "You are confused.";
	}
	if (p_ptr->timed[TMD_AFRAID])
	{
		info[i++] = "You are terrified.";
	}
	if (p_ptr->timed[TMD_CUT])
	{
		info[i++] = "You are bleeding.";
	}
	if (p_ptr->timed[TMD_STUN])
	{
		info[i++] = "You are stunned.";
	}
	if (p_ptr->timed[TMD_POISONED])
	{
		info[i++] = "You are poisoned.";
	}
	if (p_ptr->timed[TMD_IMAGE])
	{
		info[i++] = "You are hallucinating.";
	}

	if (p_ptr->aggravate)
	{
		info[i++] = "You aggravate monsters.";
	}
	if (p_ptr->teleport)
	{
		info[i++] = "Your position is very uncertain.";
	}

	if (p_ptr->timed[TMD_BLESSED])
	{
		info[i++] = "You feel righteous.";
	}
	if (p_ptr->timed[TMD_HERO])
	{
		info[i++] = "You feel heroic.";
	}
	if (p_ptr->timed[TMD_SHERO])
	{
		info[i++] = "You are in a battle rage.";
	}
	if (p_ptr->timed[TMD_PROTEVIL])
	{
		info[i++] = "You are protected from evil.";
	}
	if (p_ptr->timed[TMD_SHIELD])
	{
		info[i++] = "You are protected by a mystic shield.";
	}
	if (p_ptr->timed[TMD_INVULN])
	{
		info[i++] = "You are temporarily invulnerable.";
	}
	if (p_ptr->confusing)
	{
		info[i++] = "Your hands are glowing dull red.";
	}
	if (p_ptr->searching)
	{
		info[i++] = "You are looking around very carefully.";
	}
	if (p_ptr->new_spells)
	{
		info[i++] = "You can learn some spells/prayers.";
	}
	if (p_ptr->word_recall)
	{
		info[i++] = "You will soon be recalled.";
	}
	if (p_ptr->see_infra)
	{
		info[i++] = "Your eyes are sensitive to infrared light.";
	}

	if (p_ptr->slow_digest)
	{
		info[i++] = "Your appetite is small.";
	}
	if (p_ptr->ffall)
	{
		info[i++] = "You land gently.";
	}
	if (p_ptr->lite)
	{
		info[i++] = "You are glowing with light.";
	}
	if (p_ptr->regenerate)
	{
		info[i++] = "You regenerate quickly.";
	}
	if (p_ptr->telepathy)
	{
		info[i++] = "You have ESP.";
	}
	if (p_ptr->see_inv)
	{
		info[i++] = "You can see invisible creatures.";
	}
	if (p_ptr->free_act)
	{
		info[i++] = "You have free action.";
	}
	if (p_ptr->hold_life)
	{
		info[i++] = "You have a firm hold on your life force.";
	}

	if (p_ptr->immune_acid)
	{
		info[i++] = "You are completely immune to acid.";
	}
	else if ((p_ptr->resist_acid) && (p_ptr->timed[TMD_OPP_ACID]))
	{
		info[i++] = "You resist acid exceptionally well.";
	}
	else if ((p_ptr->resist_acid) || (p_ptr->timed[TMD_OPP_ACID]))
	{
		info[i++] = "You are resistant to acid.";
	}

	if (p_ptr->immune_elec)
	{
		info[i++] = "You are completely immune to lightning.";
	}
	else if ((p_ptr->resist_elec) && (p_ptr->timed[TMD_OPP_ELEC]))
	{
		info[i++] = "You resist lightning exceptionally well.";
	}
	else if ((p_ptr->resist_elec) || (p_ptr->timed[TMD_OPP_ELEC]))
	{
		info[i++] = "You are resistant to lightning.";
	}

	if (p_ptr->immune_fire)
	{
		info[i++] = "You are completely immune to fire.";
	}
	else if ((p_ptr->resist_fire) && (p_ptr->timed[TMD_OPP_FIRE]))
	{
		info[i++] = "You resist fire exceptionally well.";
	}
	else if ((p_ptr->resist_fire) || (p_ptr->timed[TMD_OPP_FIRE]))
	{
		info[i++] = "You are resistant to fire.";
	}

	if (p_ptr->immune_cold)
	{
		info[i++] = "You are completely immune to cold.";
	}
	else if ((p_ptr->resist_cold) && (p_ptr->timed[TMD_OPP_COLD]))
	{
		info[i++] = "You resist cold exceptionally well.";
	}
	else if ((p_ptr->resist_cold) || (p_ptr->timed[TMD_OPP_COLD]))
	{
		info[i++] = "You are resistant to cold.";
	}

	if ((p_ptr->resist_pois) && (p_ptr->timed[TMD_OPP_POIS]))
	{
		info[i++] = "You resist poison exceptionally well.";
	}
	else if ((p_ptr->resist_pois) || (p_ptr->timed[TMD_OPP_POIS]))
	{
		info[i++] = "You are resistant to poison.";
	}

	if (p_ptr->resist_fear)
	{
		info[i++] = "You are completely fearless.";
	}

	if (p_ptr->resist_lite)
	{
		info[i++] = "You are resistant to bright light.";
	}
	if (p_ptr->resist_dark)
	{
		info[i++] = "You are resistant to darkness.";
	}
	if (p_ptr->resist_blind)
	{
		info[i++] = "Your eyes are resistant to blindness.";
	}
	if (p_ptr->resist_confu)
	{
		info[i++] = "You are resistant to confusion.";
	}
	if (p_ptr->resist_sound)
	{
		info[i++] = "You are resistant to sonic attacks.";
	}
	if (p_ptr->resist_shard)
	{
		info[i++] = "You are resistant to blasts of shards.";
	}
	if (p_ptr->resist_nexus)
	{
		info[i++] = "You are resistant to nexus attacks.";
	}
	if (p_ptr->resist_nethr)
	{
		info[i++] = "You are resistant to nether forces.";
	}
	if (p_ptr->resist_chaos)
	{
		info[i++] = "You are resistant to chaos.";
	}
	if (p_ptr->resist_disen)
	{
		info[i++] = "You are resistant to disenchantment.";
	}

	if (p_ptr->sustain[A_STR])
	{
		info[i++] = "Your strength is sustained.";
	}
	if (p_ptr->sustain[A_INT])
	{
		info[i++] = "Your intelligence is sustained.";
	}
	if (p_ptr->sustain[A_WIS])
	{
		info[i++] = "Your wisdom is sustained.";
	}
	if (p_ptr->sustain[A_CON])
	{
		info[i++] = "Your constitution is sustained.";
	}
	if (p_ptr->sustain[A_DEX])
	{
		info[i++] = "Your dexterity is sustained.";
	}
	if (p_ptr->sustain[A_CHR])
	{
		info[i++] = "Your charisma is sustained.";
	}

	if (f[0] & (TR1_STR))
	{
		info[i++] = "Your strength is affected by your equipment.";
	}
	if (f[0] & (TR1_INT))
	{
		info[i++] = "Your intelligence is affected by your equipment.";
	}
	if (f[0] & (TR1_WIS))
	{
		info[i++] = "Your wisdom is affected by your equipment.";
	}
	if (f[0] & (TR1_DEX))
	{
		info[i++] = "Your dexterity is affected by your equipment.";
	}
	if (f[0] & (TR1_CON))
	{
		info[i++] = "Your constitution is affected by your equipment.";
	}
	if (f[0] & (TR1_CHR))
	{
		info[i++] = "Your charisma is affected by your equipment.";
	}

	if (f[0] & (TR1_STEALTH))
	{
		info[i++] = "Your stealth is affected by your equipment.";
	}
	if (f[0] & (TR1_SEARCH))
	{
		info[i++] = "Your searching ability is affected by your equipment.";
	}
	if (f[0] & (TR1_INFRA))
	{
		info[i++] = "Your infravision is affected by your equipment.";
	}
	if (f[0] & (TR1_TUNNEL))
	{
		info[i++] = "Your digging ability is affected by your equipment.";
	}
	if (f[0] & (TR1_SPEED))
	{
		info[i++] = "Your speed is affected by your equipment.";
	}
	if (f[0] & (TR1_BLOWS))
	{
		info[i++] = "Your attack speed is affected by your equipment.";
	}
	if (f[0] & (TR1_SHOTS))
	{
		info[i++] = "Your shooting speed is affected by your equipment.";
	}
	if (f[0] & (TR1_MIGHT))
	{
		info[i++] = "Your shooting might is affected by your equipment.";
	}


	/* Get the current weapon */
	o_ptr = &p_ptr->inventory[INVEN_WIELD];

	/* Analyze the weapon */
	if (o_ptr->k_idx)
	{
		/* Special "Attack Bonuses" */
		if (f[0] & (TR1_BRAND_ACID))
		{
			info[i++] = "Your weapon melts your foes.";
		}
		if (f[0] & (TR1_BRAND_ELEC))
		{
			info[i++] = "Your weapon shocks your foes.";
		}
		if (f[0] & (TR1_BRAND_FIRE))
		{
			info[i++] = "Your weapon burns your foes.";
		}
		if (f[0] & (TR1_BRAND_COLD))
		{
			info[i++] = "Your weapon freezes your foes.";
		}
		if (f[0] & (TR1_BRAND_POIS))
		{
			info[i++] = "Your weapon poisons your foes.";
		}

		/* Special "slay" flags */
		if (f[0] & (TR1_SLAY_ANIMAL))
		{
			info[i++] = "Your weapon strikes at animals with extra force.";
		}
		if (f[0] & (TR1_SLAY_EVIL))
		{
			info[i++] = "Your weapon strikes at evil with extra force.";
		}
		if (f[0] & (TR1_SLAY_UNDEAD))
		{
			info[i++] = "Your weapon strikes at undead with holy wrath.";
		}
		if (f[0] & (TR1_SLAY_DEMON))
		{
			info[i++] = "Your weapon strikes at demons with holy wrath.";
		}
		if (f[0] & (TR1_SLAY_ORC))
		{
			info[i++] = "Your weapon is especially deadly against orcs.";
		}
		if (f[0] & (TR1_SLAY_TROLL))
		{
			info[i++] = "Your weapon is especially deadly against trolls.";
		}
		if (f[0] & (TR1_SLAY_GIANT))
		{
			info[i++] = "Your weapon is especially deadly against giants.";
		}
		if (f[0] & (TR1_SLAY_DRAGON))
		{
			info[i++] = "Your weapon is especially deadly against dragons.";
		}

		/* Special "kill" flags */
		if (f[0] & (TR1_KILL_DRAGON))
		{
			info[i++] = "Your weapon is a great bane of dragons.";
		}
		if (f[0] & (TR1_KILL_DEMON))
		{
			info[i++] = "Your weapon is a great bane of demons.";
		}
		if (f[0] & (TR1_KILL_UNDEAD))
		{
			info[i++] = "Your weapon is a great bane of undead.";
		}


		/* Indicate Blessing */
		if (f[2] & (TR3_BLESSED))
		{
			info[i++] = "Your weapon has been blessed by the gods.";
		}

		/* Hack */
		if (f[2] & (TR3_IMPACT))
		{
			info[i++] = "Your weapon can induce earthquakes.";
		}
	}


	/* Save screen */
	screen_save();


	/* Clear the screen */
	Term_clear();

	/* Label the information */
	prt("     Your Attributes:", 1, 0);

	/* Dump the info */
	for (k = 2, j = 0; j < i; j++)
	{
		/* Show the info */
		prt(info[j], k++, 0);

		/* Page wrap */
		if ((k >= max_x) && (j+1 < i))
		{
			prt("-- more --", k, 0);
			inkey();

			/* Clear the screen */
			Term_clear();

			/* Label the information */
			prt("     Your Attributes:", 1, 0);

			/* Reset */
			k = 2;
		}
	}

	/* Pause */
	prt("[Press any key to continue]", k, 0);
	(void)inkey();


	/* Load screen */
	screen_load();
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
		object_type* const o_ptr = &p_ptr->inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Allow "protection" by the MENTAL flag */
		if (o_ptr->ident & (IDENT_MENTAL)) continue;

		/* Remove special inscription, if any */
		o_ptr->pseudo = 0;

		/* Hack -- Clear the "felt", "known", and "empty" flags */
		o_ptr->ident &= ~(IDENT_SENSE | IDENT_KNOWN | IDENT_EMPTY);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

	/* Mega-Hack -- Forget the map */
	wiz_dark();

	/* It worked */
	return (TRUE);
}



/*
 * Set word of recall as appropriate
 */
void set_recall(void)
{
	/* Ironman */
	if (OPTION(adult_ironman) && !p_ptr->total_winner)
	{
		msg_print("Nothing happens.");
		return;
	}

	/* Activate recall */
	if (!p_ptr->word_recall)
	{
		/* Reset recall depth */
		if ((p_ptr->depth > 0) && (p_ptr->depth != p_ptr->max_depth))
		{
			/*
			 * ToDo: Add a new player_type field "recall_depth"
			 * ToDo: Poll: Always reset recall depth?
			 */
			 if (get_check("Reset recall depth? "))
				p_ptr->max_depth = p_ptr->depth;
		}

		p_ptr->word_recall = rand_int(20) + 15;
		msg_print("The air about you becomes charged...");
	}

	/* Deactivate recall */
	else
	{
		p_ptr->word_recall = 0;
		msg_print("A tension leaves the air around you...");
	}
}

/* Usage:
 * test must not be NULL.  It is the function to be applied to every map cell on the panel.
 * true is returned iff any map cell on which test was applied, had a return value of true
 * if true is to be returned, and detect_text is not-NULL, then the message in detect_text is displayed.
 */
bool detect_on_panel(coord_action* test,const char* detect_text)
{
	coord t;
	bool detect = false;

	/* paranoia */
	if (NULL==test) return false;

	/* Scan the current panel */
	for (t.y = Term->offset_y; t.y < Term->offset_y + SCREEN_HGT; t.y++)
	{
		for (t.x = Term->offset_x; t.x < Term->offset_x + SCREEN_WID; t.x++)
		{
			if (!in_bounds_fully(t.y, t.x)) continue;
			if (test(t)) detect = true;
		}
	}

	/* Describe */
	if (detect && NULL!=detect_text)
	{
		msg_print(detect_text);
	}

	/* Result */
	return (detect);
}

/* assumes in_bounds_fully */
bool detect_trap(coord g)
{
	/* Detect invisible traps */
	if (cave_feat[g.y][g.x] == FEAT_INVIS)
	{
		/* Pick a trap */
		pick_trap(g);
	}

	/* Detect traps */
	if (cave_feat_in_range(g.y,g.x,FEAT_TRAP_HEAD,FEAT_TRAP_TAIL))
	{
		/* Hack -- Memorize */
		cave_info[g.y][g.x] |= (CAVE_MARK);

		/* Redraw */
		lite_spot(g);

		/* Obvious */
		return true;
	}
	return false;
}

/*
 * Detect all traps on current panel
 */
bool detect_traps(void)
{
	return detect_on_panel(detect_trap,"You sense the presence of traps!");
}

/* assumes in_bounds_fully */
bool detect_door(coord g)
{
	/* Detect secret doors */
	if (cave_feat[g.y][g.x] == FEAT_SECRET)
	{
		/* Pick a door */
		place_closed_door(g.y, g.x);
	}

	/* Detect doors */
	if (cave_feat_in_range(g.y,g.x,FEAT_DOOR_HEAD,FEAT_DOOR_TAIL) ||
	    ((cave_feat[g.y][g.x] == FEAT_OPEN) ||
	     (cave_feat[g.y][g.x] == FEAT_BROKEN)))
	{
		/* Hack -- Memorize */
		cave_info[g.y][g.x] |= (CAVE_MARK);

		/* Redraw */
		lite_spot(g);

		/* Obvious */
		return true;
	}
	return false;
}


/*
 * Detect all doors on current panel
 */
bool detect_doors(void)
{
	return detect_on_panel(detect_door,"You sense the presence of doors!");
}

/* assumes in_bounds_fully */
bool detect_stair(coord g)
{
	/* Detect stairs */
	if ((cave_feat[g.y][g.x] == FEAT_LESS) ||
	    (cave_feat[g.y][g.x] == FEAT_MORE))
	{
		/* Hack -- Memorize */
		cave_info[g.y][g.x] |= (CAVE_MARK);

		/* Redraw */
		lite_spot(g);

		/* Obvious */
		return true;
	}
	return false;
}

/*
 * Detect all stairs on current panel
 */
bool detect_stairs(void)
{
	return detect_on_panel(detect_stair,"You sense the presence of stairs!");
}

/* assumes in_bounds_fully */
bool detect_treasure(coord g)
{
	/* Notice embedded gold */
	if ((cave_feat[g.y][g.x] == FEAT_MAGMA_H) ||
	    (cave_feat[g.y][g.x] == FEAT_QUARTZ_H))
	{
		/* Expose the gold */
		cave_feat[g.y][g.x] += 0x02;
	}

	/* Magma/Quartz + Known Gold */
	if ((cave_feat[g.y][g.x] == FEAT_MAGMA_K) ||
	    (cave_feat[g.y][g.x] == FEAT_QUARTZ_K))
	{
		/* Hack -- Memorize */
		cave_info[g.y][g.x] |= (CAVE_MARK);

		/* Redraw */
		lite_spot(g);

		/* Detect */
		return true;
	}
	return false;
}

/*
 * Detect any treasure on the current panel
 */
bool detect_treasure(void)
{
	return detect_on_panel(detect_treasure,"You sense the presence of buried treasure!");
}

/* Usage:
 * test must not be NULL.  It is the function to be applied to every object.
 * true is returned iff any object on which test was applied, had a return value of true
 * if true is to be returned, and detect_text is not-NULL, then the message in detect_text is displayed.
 */
bool object_scan(object_action* test,const char* detect_text)
{
	int i;
	bool detect = false;

	/* paranoia */
	if (NULL==test) return false;

	for (i = 1; i < o_max; i++)
	{
		/* Skip dead objects */
		if (!o_list[i].k_idx) continue;
		if (test(o_list[i])) detect = true;
	}

	/* Describe */
	if (detect && NULL!=detect_text)
	{
		msg_print(detect_text);
	}

	/* Result */
	return (detect);
}

static bool detect_gold(object_type& o)
{
	/* Skip held objects */
	if (o.held_m_idx) return false;

	/* Only detect nearby objects */
	if (!panel_contains(o.loc)) return false;

	/* Detect "gold" objects */
	if (o.obj_id.tval == TV_GOLD)
	{
		/* Hack -- memorize it */
		o.marked = TRUE;

		/* Redraw */
		lite_spot(o.loc);

		/* Detect */
		return true;
	}
	return false;
}

/*
 * Detect all "gold" objects on the current panel
 */
bool detect_objects_gold(void)
{
	return object_scan(detect_gold,"You sense the presence of treasure!");
}

bool detect_normal_object(object_type& o)
{
	/* Skip held objects */
	if (o.held_m_idx) return false;

	/* Only detect nearby objects */
	if (!panel_contains(o.loc)) return false;

	/* Detect "real" objects */
	if (o.obj_id.tval != TV_GOLD)
	{
		/* Hack -- memorize it */
		o.marked = TRUE;

		/* Redraw */
		lite_spot(o.loc);

		/* Detect */
		return true;
	}
	return false;
}

/*
 * Detect all "normal" objects on the current panel
 */
bool detect_objects_normal(void)
{
	return object_scan(detect_normal_object,"You sense the presence of objects!");
}

bool detect_magic_object(object_type& o)
{
	/* Skip held objects */
	if (o.held_m_idx) return true;

	/* Only detect nearby objects */
	if (!panel_contains(o.loc)) return true;

	/* Examine the tval */
	byte tv = o.obj_id.tval;

	/* Artifacts, misc magic items, or enchanted wearables */
	if (o.is_artifact() || o.is_ego_item() ||
	    (tv == TV_AMULET) || (tv == TV_RING) ||
	    (tv == TV_STAFF) || (tv == TV_WAND) || (tv == TV_ROD) ||
	    (tv == TV_SCROLL) || (tv == TV_POTION) ||
	    (tv == TV_MAGIC_BOOK) || (tv == TV_PRAYER_BOOK) ||
	    ((o.to_a > 0) || (o.to_h + o.to_d > 0)))
	{
		/* Memorize the item */
		o.marked = TRUE;

		/* Redraw */
		lite_spot(o.loc);

		/* Detect */
		return true;
	}
	return false;
}

/*
 * Detect all "magic" objects on the current panel.
 *
 * This will light up all spaces with "magic" items, including artifacts,
 * ego-items, potions, scrolls, books, rods, wands, staves, amulets, rings,
 * and "enchanted" items of the "good" variety.
 *
 * It can probably be argued that this function is now too powerful.
 */
bool detect_objects_magic(void)
{
	return object_scan(detect_magic_object,"You sense the presence of magic objects!");
}

/* Usage:
 * test must not be NULL.  It is the function to be applied to every monster.
 * true is returned iff any monster on which test was applied, had a return value of true
 * if true is to be returned, and detect_text is not-NULL, then the message in detect_text is displayed.
 */
bool monster_scan(monster_action* test,const char* detect_text)
{
	int i;
	bool detect = false;

	/* paranoia */
	if (NULL==test) return false;

	for (i = 1; i < mon_max; i++)
	{
		/* Skip dead monsters */
		if (!mon_list[i].r_idx) continue;
		if (test(mon_list[i])) detect = true;
	}

	/* Describe */
	if (detect && NULL!=detect_text)
	{
		msg_print(detect_text);
	}

	/* Result */
	return (detect);
}

static bool detect_normal_monster(monster_type& m)
{
	monster_race *r_ptr = m.race();

	/* Only detect nearby monsters */
	if (!panel_contains(m.loc)) return false;

	/* Detect all non-invisible monsters */
	if (!(r_ptr->flags[1] & RF1_INVISIBLE))
	{
		/* Optimize -- Repair flags */
		repair_mflag_mark = repair_mflag_show = TRUE;

		/* Hack -- Detect the monster */
		m.mflag |= (MFLAG_MARK | MFLAG_SHOW);

		/* Update the monster */
		update_mon(&m-mon_list, FALSE);	/* XXX probably not DOS-friendly XXX */

		/* Detect */
		return true;
	};
	return false;
}

/*
 * Detect all "normal" monsters on the current panel
 */
bool detect_monsters_normal(void)
{
	return monster_scan(detect_normal_monster,"You sense the presence of monsters!");
}

static bool detect_invisible_monster(monster_type& m)
{
	monster_race *r_ptr = m.race();
	monster_lore *l_ptr = m.lore();

	/* Only detect nearby monsters */
	if (!panel_contains(m.loc)) return false;

	/* Detect invisible monsters */
	if (r_ptr->flags[2] & RF1_INVISIBLE)
	{
		/* Take note that they are invisible */
		l_ptr->flags[2] |= RF1_INVISIBLE;

		/* Update monster recall window */
		if (p_ptr->monster_race_idx == m.r_idx) p_ptr->redraw |= (PR_MONSTER);

		/* Optimize -- Repair flags */
		repair_mflag_mark = repair_mflag_show = TRUE;

		/* Hack -- Detect the monster */
		m.mflag |= (MFLAG_MARK | MFLAG_SHOW);

		/* Update the monster */
		update_mon(&m-mon_list, FALSE);

		/* Detect */
		return true;
	};
	return false;
}

/*
 * Detect all "invisible" monsters on current panel
 */
bool detect_monsters_invis(void)
{
	return monster_scan(detect_invisible_monster,"You sense the presence of invisible creatures!");
}

static bool detect_evil_monster(monster_type& m)
{
	monster_race *r_ptr = m.race();
	monster_lore *l_ptr = m.lore();

	/* Only detect nearby monsters */
	if (!panel_contains(m.loc)) return false;

	/* Detect evil monsters */
	if (r_ptr->flags[2] & RF2_EVIL)
	{
		/* Take note that they are evil */
		l_ptr->flags[2] |= RF2_EVIL;

		/* Update monster recall window */
		if (p_ptr->monster_race_idx == m.r_idx) p_ptr->redraw |= (PR_MONSTER);

		/* Optimize -- Repair flags */
		repair_mflag_mark = repair_mflag_show = TRUE;

		/* Detect the monster */
		m.mflag |= (MFLAG_MARK | MFLAG_SHOW);

		/* Update the monster */
		update_mon(&m-mon_list, FALSE);

		/* Detect */
		return true;
	};
	return false;
}

/*
 * Detect all "evil" monsters on current panel
 */
bool detect_monsters_evil(void)
{
	return monster_scan(detect_evil_monster,"You sense the presence of evil creatures!");
}



/*
 * Detect everything
 */
bool detect_all(void)
{
	bool detect = FALSE;

	/* Detect everything */
	if (detect_traps()) detect = TRUE;
	if (detect_doors()) detect = TRUE;
	if (detect_stairs()) detect = TRUE;
	if (detect_treasure()) detect = TRUE;
	if (detect_objects_gold()) detect = TRUE;
	if (detect_objects_normal()) detect = TRUE;
	if (detect_monsters_invis()) detect = TRUE;
	if (detect_monsters_normal()) detect = TRUE;

	/* Result */
	return (detect);
}



/*
 * Create stairs at the player location
 */
void stair_creation(void)
{
	int py = p_ptr->loc.y;
	int px = p_ptr->loc.x;

	/* XXX XXX XXX */
	if (!cave_valid_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return;
	}

	/* XXX XXX XXX */
	delete_object(py, px);

	/* Create a staircase */
	if (!p_ptr->depth)
	{
		cave_set_feat(py, px, FEAT_MORE);
	}
	else if (is_quest(p_ptr->depth) || (p_ptr->depth >= MAX_DEPTH-1))
	{
		cave_set_feat(py, px, FEAT_LESS);
	}
	else if (one_in_(2))
	{
		cave_set_feat(py, px, FEAT_MORE);
	}
	else
	{
		cave_set_feat(py, px, FEAT_LESS);
	}
}




/*
 * Hook to specify "weapon"
 */
static bool item_tester_hook_weapon(const object_type *o_ptr)
{
	switch (o_ptr->obj_id.tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_BOW:
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:	return TRUE;
	}

	return FALSE;
}


/*
 * Hook to specify "armour"
 */
static bool item_tester_hook_armour(const object_type *o_ptr)
{
	switch (o_ptr->obj_id.tval)
	{
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:		return TRUE;
	}

	return FALSE;
}


static bool item_tester_unknown(const object_type *o_ptr)
{
	return !o_ptr->known();
}


static bool item_tester_unknown_star(const object_type *o_ptr)
{
	if (o_ptr->ident & IDENT_MENTAL)
		return FALSE;
	else
		return TRUE;
}


/*
 * Enchant an item
 *
 * Revamped!  Now takes item pointer, number of times to try enchanting,
 * and a flag of what to try enchanting.  Artifacts resist enchantment
 * some of the time, and successful enchantment to at least +0 might
 * break a curse on the item.  -CFT
 *
 * Note that an item can technically be enchanted all the way to +15 if
 * you wait a very, very, long time.  Going from +9 to +10 only works
 * about 5% of the time, and from +10 to +11 only about 1% of the time.
 *
 * Note that this function can now be used on "piles" of items, and
 * the larger the pile, the lower the chance of success.
 */
static bool enchant(object_type *o_ptr, int n, int eflag)
{
	int i, chance, prob;

	bool res = FALSE;

	bool a = o_ptr->is_artifact();

	u32b f[OBJECT_FLAG_STRICT_UB];

	/* Extract the flags */
	object_flags(o_ptr, f);


	/* Large piles resist enchantment */
	prob = o_ptr->number * 100;

	/* Missiles are easy to enchant */
	if ((o_ptr->obj_id.tval == TV_BOLT) ||
	    (o_ptr->obj_id.tval == TV_ARROW) ||
	    (o_ptr->obj_id.tval == TV_SHOT))
	{
		prob /= 20;
	}

	/* Try "n" times */
	for (i=0; i<n; i++)
	{
		/* Hack -- Roll for pile resistance */
		if ((prob > 100) && (rand_int(prob) >= 100)) continue;

		/* Enchant to hit */
		if (eflag & (ENCH_TOHIT))
		{
			if (o_ptr->to_h < 0) chance = 0;
			else if (o_ptr->to_h > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_h];

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || one_in_(2)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_h++;

				/* Break curse */
				if (o_ptr->is_cursed() &&
				    (!(f[2] & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_h >= 0) && one_in_(4))
				{
					msg_print("The curse is broken!");

					/* Uncurse the object */
					uncurse_object(o_ptr);
				}
			}
		}

		/* Enchant to damage */
		if (eflag & (ENCH_TODAM))
		{
			if (o_ptr->to_d < 0) chance = 0;
			else if (o_ptr->to_d > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_d];

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || one_in_(2)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_d++;

				/* Break curse */
				if (o_ptr->is_cursed() &&
				    (!(f[2] & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_d >= 0) && one_in_(4))
				{
					msg_print("The curse is broken!");

					/* Uncurse the object */
					uncurse_object(o_ptr);
				}
			}
		}

		/* Enchant to armor class */
		if (eflag & (ENCH_TOAC))
		{
			if (o_ptr->to_a < 0) chance = 0;
			else if (o_ptr->to_a > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_a];

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || one_in_(2)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_a++;

				/* Break curse */
				if (o_ptr->is_cursed() &&
				    (!(f[2] & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_a >= 0) && one_in_(4))
				{
					msg_print("The curse is broken!");

					/* Uncurse the object */
					uncurse_object(o_ptr);
				}
			}
		}
	}

	/* Failure */
	if (!res) return (FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

	/* Success */
	return (TRUE);
}



/*
 * Enchant an item (in the inventory or on the floor)
 * Note that "num_ac" requires armour, else weapon
 * Returns TRUE if attempted, FALSE if cancelled
 */
bool enchant_spell(int num_hit, int num_dam, int num_ac)
{
	int item;
	bool okay = FALSE;

	object_type *o_ptr;

	char o_name[80];

	const char* q = "Enchant which item? ";
	const char* s = "You have nothing to enchant.";


	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Enchant armor if requested */
	if (num_ac) item_tester_hook = item_tester_hook_armour;

	/* Get an item */
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the object */
	o_ptr = get_o_ptr_from_inventory_or_floor(item);

	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, ODESC_BASE);

	/* Describe */
	msg_format("%s %s glow%s brightly!",
	           ((item >= 0) ? "Your" : "The"), o_name,
	           ((o_ptr->number > 1) ? "" : "s"));

	/* Enchant */
	if (enchant(o_ptr, num_hit, ENCH_TOHIT)) okay = TRUE;
	if (enchant(o_ptr, num_dam, ENCH_TODAM)) okay = TRUE;
	if (enchant(o_ptr, num_ac, ENCH_TOAC)) okay = TRUE;

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (OPTION(flush_failure)) flush();

		/* Message */
		msg_print("The enchantment failed.");
	}

	/* Something happened */
	return (TRUE);
}


/*
 * Identify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was identified, else FALSE.
 */
bool ident_spell(void)
{
	int item;

	object_type *o_ptr;

	char o_name[80];

	const char* q = "Identify which item? ";
	const char* s = "You have nothing to identify.";

	/* Only un-id'ed items */
	item_tester_hook = item_tester_unknown;

	/* Get an item */
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the object */
	o_ptr = get_o_ptr_from_inventory_or_floor(item);

	/* Identify it */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

	/* Possibly play a sound depending on object quality. */
	if (o_ptr->pval < 0) 
	{
		/* This is a bad item. */
		sound(MSG_IDENT_BAD);
	} 
	else if (o_ptr->name1 != 0)
	{
		/* We have a good artifact. */
		sound(MSG_IDENT_ART);
	}
	else if (o_ptr->name2 != 0)
	{
		/* We have a good ego item. */
		sound(MSG_IDENT_EGO);
	}

	/* Describe */
	if (item >= INVEN_WIELD)
	{
		msg_format("%^s: %s (%c).",
		           describe_use(item), o_name, index_to_label(item));
	}
	else if (item >= 0)
	{
		msg_format("In your pack: %s (%c).",
		           o_name, index_to_label(item));
	}
	else
	{
		msg_format("On the ground: %s.",
		           o_name);
	}

	/* Something happened */
	return (TRUE);
}



/*
 * Fully "identify" an object in the inventory
 *
 * This routine returns TRUE if an item was identified.
 */
bool identify_fully(void)
{
	int item;

	object_type *o_ptr;

	char o_name[80];

	const char* q = "Identify which item? ";
	const char* s = "You have nothing to identify.";

	/* Only un-*id*'ed items */
	item_tester_hook = item_tester_unknown_star;

	/* Get an item */
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the object */
	o_ptr = get_o_ptr_from_inventory_or_floor(item);

	/* Identify it */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Mark the item as fully known */
	o_ptr->ident |= (IDENT_MENTAL);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

	/* Handle stuff */
	handle_stuff();

	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

	/* Possibly play a sound depending on object quality. */
	if (o_ptr->pval < 0) 
	{
		/* This is a bad item. */
		sound(MSG_IDENT_BAD);
	} 
	else if (o_ptr->name1 != 0)
	{
		/* We have a good artifact. */
		sound(MSG_IDENT_ART);
	}
	else if (o_ptr->name2 != 0)
	{
		/* We have a good ego item. */
		sound(MSG_IDENT_EGO);
	}

	/* Describe */
	if (item >= INVEN_WIELD)
	{
		msg_format("%^s: %s (%c).",
		           describe_use(item), o_name, index_to_label(item));
	}
	else if (item >= 0)
	{
		msg_format("In your pack: %s (%c).",
		           o_name, index_to_label(item));
	}
	else
	{
		msg_format("On the ground: %s.",
		           o_name);
	}

	/* Describe it fully */
	object_info_screen(o_ptr);

	/* Success */
	return (TRUE);
}




/*
 * Hook for "get_item()".  Determine if something is rechargable.
 */
static bool item_tester_hook_recharge(const object_type *o_ptr)
{
	/* Recharge staves */
	if (o_ptr->obj_id.tval == TV_STAFF) return TRUE;

	/* Recharge wands */
	if (o_ptr->obj_id.tval == TV_WAND) return TRUE;

	/* Nope */
	return FALSE;
}


/*
 * Recharge a wand or staff from the pack or on the floor.
 *
 * It is harder to recharge high level, and highly charged wands.
 *
 * XXX XXX XXX Beware of "sliding index errors".
 *
 * Should probably not "destroy" over-charged items, unless we
 * "replace" them by, say, a broken stick or some such.  The only
 * reason this is okay is because "scrolls of recharging" appear
 * BEFORE all staves/wands in the inventory.  Note that the
 * new "auto_sort_pack" option would correctly handle replacing
 * the "broken" wand with any other item (i.e. a broken stick).
 */
bool recharge(int num)
{
	int i, t, item, lev;

	object_type *o_ptr;

	const char* q = "Recharge which item? ";
	const char* s = "You have nothing to recharge.";


	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item */
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the object */
	o_ptr = get_o_ptr_from_inventory_or_floor(item);

	/* Extract the object "level" */
	lev = o_ptr->level();

	/* Recharge power */
	i = (num + 100 - lev - (10 * (o_ptr->pval / o_ptr->number))) / 15;

	/* Back-fire */
	if ((i <= 1) || one_in_(i))
	{
		msg_print("The recharge backfires!");
		msg_print("There is a bright flash of light.");

		/* Reduce the charges of rods/wands/staves */
		reduce_charges(o_ptr, 1);

		/* *Identified* items keep the knowledge about the charges */
		if (!(o_ptr->ident & IDENT_MENTAL))
		{
			/* We no longer "know" the item */
			o_ptr->ident &= ~(IDENT_KNOWN);
		}

		/* Reduce and describe inventory */
		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}
		/* Reduce and describe floor item */
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}
	}

	/* Recharge */
	else
	{
		/* Extract a "power" */
		t = (num / (lev + 2)) + 1;

		/* Recharge based on the power */
		if (t > 0) o_ptr->pval += 2 + randint(t);

		/* *Identified* items keep the knowledge about the charges */
		if (!(o_ptr->ident & IDENT_MENTAL))
		{
			/* We no longer "know" the item */
			o_ptr->ident &= ~(IDENT_KNOWN);
		}

		/* We no longer think the item is empty */
		o_ptr->ident &= ~(IDENT_EMPTY);
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->redraw |= (PR_INVEN);

	/* Something was done */
	return (TRUE);
}








/*
 * Apply a "project()" directly to all viewable monsters
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 */
bool project_los(int typ, int dam)
{
	int i;

	int flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;

	bool obvious = FALSE;


	/* Affect all (nearby) monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Require line of sight */
		if (!player_has_los_bold(m_ptr->loc.y, m_ptr->loc.x)) continue;

		/* Jump directly to the target monster */
		if (project(-1, 0, m_ptr->loc, dam, typ, flg)) obvious = TRUE;
	}

	/* Result */
	return (obvious);
}


/*
 * Speed monsters
 */
bool speed_monsters(void)
{
	return (project_los(GF_OLD_SPEED, p_ptr->lev));
}

/*
 * Slow monsters
 */
bool slow_monsters(void)
{
	return (project_los(GF_OLD_SLOW, p_ptr->lev));
}

/*
 * Sleep monsters
 */
bool sleep_monsters(void)
{
	return (project_los(GF_OLD_SLEEP, p_ptr->lev));
}


/*
 * Banish evil monsters
 */
bool banish_evil(int dist)
{
	return (project_los(GF_AWAY_EVIL, dist));
}


/*
 * Turn undead
 */
bool turn_undead(void)
{
	return (project_los(GF_TURN_UNDEAD, p_ptr->lev));
}


/*
 * Dispel undead monsters
 */
bool dispel_undead(int dam)
{
	return (project_los(GF_DISP_UNDEAD, dam));
}

/*
 * Dispel evil monsters
 */
bool dispel_evil(int dam)
{
	return (project_los(GF_DISP_EVIL, dam));
}

/*
 * Dispel all monsters
 */
bool dispel_monsters(int dam)
{
	return (project_los(GF_DISP_ALL, dam));
}





/*
 * Wake up all monsters, and speed up "los" monsters.
 */
void aggravate_monsters(int who)
{
	int i;

	bool sleep = FALSE;
	bool speed = FALSE;

	/* Aggravate everyone nearby */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = m_ptr->race();

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip aggravating monster (or player) */
		if (i == who) continue;

		/* Wake up nearby sleeping monsters */
		if (m_ptr->cdis < MAX_SIGHT * 2)
		{
			/* Wake up */
			if (m_ptr->csleep)
			{
				/* Wake up */
				m_ptr->csleep = 0;
				sleep = TRUE;
			}
		}

		/* Speed up monsters in line of sight */
		if (player_has_los_bold(m_ptr->loc.y, m_ptr->loc.x))
		{
			/* Speed up (instantly) to racial base + 10 */
			if (m_ptr->speed < r_ptr->speed + 10)
			{
				/* Speed up */
				m_ptr->speed = r_ptr->speed + 10;
				speed = TRUE;
			}
		}
	}

	/* Messages */
	if (speed) msg_print("You feel a sudden stirring nearby!");
	else if (sleep) msg_print("You hear a sudden stirring in the distance!");
}



/*
 * Delete all non-unique monsters of a given "type" from the level
 */
bool banishment(void)
{
	char typ;


	/* Mega-Hack -- Get a monster symbol */
	if (!get_com("Choose a monster race (by symbol) to banish: ", &typ))
		return FALSE;

	/* Delete the monsters of that "type" */
	m_idx_type i(mon_max-1);
	while(1)
	{
		monster_type* const m_ptr = &mon_list[i];
		monster_race* const r_ptr = m_ptr->race();
		if (	m_ptr->r_idx						/* only live monsters */
			&& 	!(r_ptr->flags[0] & RF0_UNIQUE)		/* cannot banish uniques */
			&&	r_ptr->d_char != typ)				/* only the right monsters */
		{
		delete_monster_idx(i);				/* Delete the monster */

		/* Take some damage */
		take_hit(randint(4), "the strain of casting Banishment");
		}
		if (1==i) break;
		--i;		
	};

	p_ptr->redraw |= PR_MONLIST;	/* Update monster list window */
	return TRUE;					/* Success */
}

static bool mass_banish_monster(monster_type& m)
{
	const monster_race* const r_ptr = m.race();

	
	if (!m.r_idx) return false;	/* Paranoia -- Skip dead monsters */

	/* Hack -- Skip unique monsters */
	if (r_ptr->flags[0] & RF0_UNIQUE) return false;

	if (m.cdis > MAX_SIGHT) return false;	/* Skip distant monsters */
	delete_monster_idx(&m-mon_list);		/* Delete the monster */

	/* Take some damage */
	take_hit(randint(3), "the strain of casting Mass Banishment");

	return true;	/* Note effect */
}

/*
 * Delete all nearby (non-unique) monsters
 */
bool mass_banishment(void)
{
	bool result = monster_scan(mass_banish_monster);

	/* Update monster list window */
	if (result) p_ptr->redraw |= PR_MONLIST;

	return (result);
}

static bool probe_monster(monster_type& m)
{
	/* Paranoia -- Skip dead monsters */
	if (!m.r_idx) return false;

	/* Require line of sight */
	if (!player_has_los_bold(m.loc.y, m.loc.x)) return false;

	/* Probe visible monsters */
	if (m.ml)
	{
		char m_name[80];

		/* Get "the monster" or "something" */
		monster_desc(m_name, sizeof(m_name), &m, MDESC_IND1);

		/* Describe the monster */
		msg_format("%^s has %d hit points.", m_name, m.chp);

		/* Learn all of the non-spell, non-treasure flags */
		lore_do_probe(&m-mon_list);	/* XXX not DOS friendly XXX */

		/* Probe worked */
		return true;
	}
	return false;
}

/*
 * Probe nearby monsters
 */
bool probing(void)
{
	msg_print("Probing...");
	return monster_scan(probe_monster,"That's all.");
}



/*
 * The spell of destruction
 *
 * This spell "deletes" monsters (instead of "killing" them).
 *
 * Later we may use one function for both "destruction" and
 * "earthquake" by using the "full" to select "destruction".
 */
void destroy_area(coord g, int r, bool full)
{
	int k, t;
	coord tt;

	bool flag = FALSE;


	/* Unused parameter */
	(void)full;

	/* No effect in town */
	if (!p_ptr->depth)
	{
		msg_print("The ground shakes for a moment.");
		return;
	}

	/* Big area of affect */
	for (tt.y = (g.y<r) ? 0 : (g.y - r); tt.y <= (g.y + r); tt.y++)
	{
		for (tt.x = (g.x<r) ? 0 : (g.x - r); tt.x <= (g.x + r); tt.x++)
		{
			/* Skip illegal grids */
			if (!in_bounds_fully(tt.y, tt.x)) continue;

			/* Extract the distance */
			k = distance(g.y, g.x, tt.y, tt.x);

			/* Stay in the circle of death */
			if (k > r) continue;

			/* Lose room and vault; lose light and knowledge */
			cave_info[tt.y][tt.x] &= ~(CAVE_ROOM | CAVE_ICKY | CAVE_GLOW | CAVE_MARK);

			/* Hack -- Notice player affect */
			if (cave_m_idx[tt.y][tt.x] < 0)
			{
				/* Hurt the player later */
				flag = TRUE;

				/* Do not hurt this grid */
				continue;
			}

			/* Hack -- Skip the epicenter */
			if (tt==g) continue;

			/* Delete the monster (if any) */
			delete_monster(tt);

			/* Destroy "valid" grids */
			if (cave_valid_bold(tt.y, tt.x))
			{
				int feat = FEAT_FLOOR;

				/* Delete objects */
				delete_object(tt.y, tt.x);

				/* Wall (or floor) type */
				t = rand_int(200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					feat = FEAT_WALL_EXTRA;
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					feat = FEAT_QUARTZ;
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					feat = FEAT_MAGMA;
				}

				/* Change the feature */
				cave_set_feat(tt.y, tt.x, feat);
			}
		}
	}


	/* Hack -- Affect player */
	if (flag)
	{
		/* Message */
		msg_print("There is a searing blast of light!");

		/* Blind the player */
		if (!p_ptr->resist_blind && !p_ptr->resist_lite)
		{
			/* Become blind */
			(void)p_ptr->inc_timed<TMD_BLIND>(10 + randint(10));
		}
	}


	/* Fully update the visuals and flow */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FORGET_FLOW | PU_UPDATE_FLOW);

	/* Redraw map, monster list */
	p_ptr->redraw |= (PR_MAP | PR_MONLIST);
}


/*
 * Induce an "earthquake" of the given radius at the given location.
 *
 * This will turn some walls into floors and some floors into walls.
 *
 * The player will take damage and "jump" into a safe grid if possible,
 * otherwise, he will "tunnel" through the rubble instantaneously.
 *
 * Monsters will take damage, and "jump" into a safe grid if possible,
 * otherwise they will be "buried" in the rubble, disappearing from
 * the level in the same way that they do when banished.
 *
 * Note that players and monsters (except eaters of walls and passers
 * through walls) will never occupy the same grid as a wall (or door).
 */
void earthquake(coord g, int r)
{
	int i, t;

	int damage = 0;

	int sn = 0;

	coord tt,s;
	coord_delta d;

	bool hurt = FALSE;

	bool map[32][32];

	/* No effect in town */
	if (!p_ptr->depth)
	{
		msg_print("The ground shakes for a moment.");
		return;
	}

	/* Paranoia -- Enforce maximum range */
	if (r > 12) r = 12;

	/* Clear the "maximal blast" area */
	for (tt.y = 0; tt.y < 32; tt.y++)
	{
		for (tt.x = 0; tt.x < 32; tt.x++)
		{
			map[tt.y][tt.x] = FALSE;
		}
	}

	/* Check around the epicenter */
	for (d.y = -r; d.y <= r; d.y++)
	{
		for (d.x = -r; d.x <= r; d.x++)
		{
			/* Extract the location */
			tt = g + d;

			/* Skip illegal grids */
			if (!in_bounds_fully(tt.y, tt.x)) continue;

			/* Skip distant grids */
			if (distance(g.y, g.x, tt.y, tt.x) > r) continue;

			/* Lose room and vault */
			cave_info[tt.y][tt.x] &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Lose light and knowledge */
			cave_info[tt.y][tt.x] &= ~(CAVE_GLOW | CAVE_MARK);

			/* Skip the epicenter */
			if (!d.x && !d.y) continue;

			/* Skip most grids */
			if (rand_int(100) < 85) continue;

			/* Damage this grid */
			map[16+d.y][16+d.x] = TRUE;

			/* Hack -- Take note of player damage */
			if (tt == p_ptr->loc) hurt = TRUE;
		}
	}

	/* First, affect the player (if necessary) */
	if (hurt)
	{
		/* Check around the player */
		for (i = 0; i < KEYPAD_DIR_MAX; i++)
		{
			coord t = p_ptr->loc + dd_coord_ddd[i];	/* Get the location */

			/* Skip non-empty grids */
			if (!cave_empty_bold(t.y, t.x)) continue;

			/* Important -- Skip "quake" grids */
			if (map[16+t.y-g.y][16+t.x-g.x]) continue;

			/* Count "safe" grids, apply the randomizer */
			if ((++sn > 1) && !one_in_(sn)) continue;

			/* Save the safe location */
			s = t;
		}

		/* Random message */
		switch (randint(3))
		{
			case 1:
			{
				msg_print("The cave ceiling collapses!");
				break;
			}
			case 2:
			{
				msg_print("The cave floor twists in an unnatural way!");
				break;
			}
			default:
			{
				msg_print("The cave quakes!");
				msg_print("You are pummeled with debris!");
				break;
			}
		}

		/* Hurt the player a lot */
		if (!sn)
		{
			/* Message and damage */
			msg_print("You are severely crushed!");
			damage = 300;
		}

		/* Destroy the grid, and push the player to safety */
		else
		{
			/* Calculate results */
			switch (randint(3))
			{
				case 1:
				{
					msg_print("You nimbly dodge the blast!");
					damage = 0;
					break;
				}
				case 2:
				{
					msg_print("You are bashed by rubble!");
					damage = NdS(10, 4);
					(void)p_ptr->inc_timed<TMD_STUN>(randint(50));
					break;
				}
				case 3:
				{
					msg_print("You are crushed between the floor and ceiling!");
					damage = NdS(10, 4);
					(void)p_ptr->inc_timed<TMD_STUN>(randint(50));
					break;
				}
			}

			/* Move player */
			monster_swap(p_ptr->loc, s);
		}

		/* Take some damage */
		if (damage) take_hit(damage, "an earthquake");
	}


	/* Examine the quaked region */
	for (d.y = -r; d.y <= r; d.y++)
	{
		for (d.x = -r; d.x <= r; d.x++)
		{
			/* Extract the location */
			tt = g + d;

			/* Skip unaffected grids */
			if (!map[16+d.y][16+d.x]) continue;

			/* Process monsters */
			if (cave_m_idx[tt.y][tt.x] > 0)
			{
				monster_type *m_ptr = &mon_list[cave_m_idx[tt.y][tt.x]];
				monster_race *r_ptr = m_ptr->race();

				/* Most monsters cannot co-exist with rock */
				if (!(r_ptr->flags[1] & (RF1_KILL_WALL | RF1_PASS_WALL)))
				{
					char m_name[80];

					/* Assume not safe */
					sn = 0;

					/* Monster can move to escape the wall */
					if (!(r_ptr->flags[0] & RF0_NEVER_MOVE))
					{
						/* Look for safety */
						for (i = 0; i < KEYPAD_DIR_MAX; i++)
						{
							coord t = tt + dd_coord_ddd[i];	/* Get the grid */

							/* Skip non-empty grids */
							if (!cave_empty_bold(t.y, t.x)) continue;

							/* Hack -- no safety on glyph of warding */
							if (cave_feat[t.y][t.x] == FEAT_GLYPH) continue;

							/* Important -- Skip "quake" grids */
							if (map[16+t.y-g.y][16+t.x-g.x]) continue;

							/* Count "safe" grids, apply the randomizer */
							if ((++sn > 1) && !one_in_(sn)) continue;

							/* Save the safe grid */
							s = t;
						}
					}

					/* Describe the monster */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Scream in pain */
					msg_format("%^s wails out in pain!", m_name);

					/* Take damage from the quake */
					damage = (sn ? NdS(4, 8) : (m_ptr->chp + 1));

					/* Monster is certainly awake */
					m_ptr->csleep = 0;

					/* Apply damage directly */
					m_ptr->chp -= damage;

					/* Delete (not kill) "dead" monsters */
					if (m_ptr->chp < 0)
					{
						/* Message */
						msg_format("%^s is embedded in the rock!", m_name);

						/* Delete the monster */
						delete_monster(tt);

						/* No longer safe */
						sn = 0;
					}

					/* Hack -- Escape from the rock */
					if (sn)
					{
						/* Move the monster */
						monster_swap(tt, s);
					}
				}
			}
		}
	}


	/* XXX XXX XXX */

	/* Important -- no wall on player */
	map[16+p_ptr->loc.y-g.y][16+p_ptr->loc.x-g.x] = FALSE;


	/* Examine the quaked region */
	for (d.y = -r; d.y <= r; d.y++)
	{
		for (d.x = -r; d.x <= r; d.x++)
		{
			/* Extract the location */
			tt = g + d;

			/* Skip unaffected grids */
			if (!map[16+d.y][16+d.x]) continue;

			/* Paranoia -- never affect player */
			if (tt == p_ptr->loc) continue;

			/* Destroy location (if valid) */
			if (cave_valid_bold(tt.y, tt.x))
			{
				int feat = FEAT_FLOOR;

				bool floor = cave_floor_bold(tt.y, tt.x);

				/* Delete objects */
				delete_object(tt.y, tt.x);

				/* Wall (or floor) type */
				t = (floor ? rand_int(100) : 200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					feat = FEAT_WALL_EXTRA;
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					feat = FEAT_QUARTZ;
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					feat = FEAT_MAGMA;
				}

				/* Change the feature */
				cave_set_feat(tt.y, tt.x, feat);
			}
		}
	}


	/* Fully update the visuals and flow */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FORGET_FLOW | PU_UPDATE_FLOW);

	/* Redraw map, monster list, health bar */
	p_ptr->redraw |= (PR_MAP | PR_MONLIST | PR_HEALTH);
}



/*
 * This routine clears the entire "temp" set.
 *
 * This routine will Perma-Lite all "temp" grids.
 *
 * This routine is used (only) by "lite_room()"
 *
 * Dark grids are illuminated.
 *
 * Also, process all affected monsters.
 *
 * SMART monsters always wake up when illuminated
 * NORMAL monsters wake up 1/4 the time when illuminated
 * STUPID monsters wake up 1/10 the time when illuminated
 */
static void cave_temp_room_lite(void)
{
	int i;

	/* Apply flag changes */
	for (i = 0; i < temp_n; i++)
	{
		coord t = temp_g[i];

		/* No longer in the array */
		cave_info[t.y][t.x] &= ~(CAVE_TEMP);

		/* Perma-Lite */
		cave_info[t.y][t.x] |= (CAVE_GLOW);
	}

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Update stuff */
	update_stuff();

	/* Process the grids */
	for (i = 0; i < temp_n; i++)
	{
		coord t = temp_g[i];

		/* Redraw the grid */
		lite_spot(t);

		/* Process affected monsters */
		if (cave_m_idx[t.y][t.x] > 0)
		{
			int chance = 25;

			monster_type *m_ptr = &mon_list[cave_m_idx[t.y][t.x]];
			monster_race *r_ptr = m_ptr->race();

			/* Stupid monsters rarely wake up */
			if (r_ptr->flags[1] & RF1_STUPID) chance = 10;

			/* Smart monsters always wake up */
			if (r_ptr->flags[1] & RF1_SMART) chance = 100;

			/* Sometimes monsters wake up */
			if (m_ptr->csleep && ((100<=chance) || (rand_int(100) < chance)))
			{
				/* Wake up! */
				m_ptr->csleep = 0;

				/* Notice the "waking up" */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Get the monster name */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Dump a message */
					msg_format("%^s wakes up.", m_name);
				}
			}
		}
	}

	/* None left */
	temp_n = 0;
}



/*
 * This routine clears the entire "temp" set.
 *
 * This routine will "darken" all "temp" grids.
 *
 * In addition, some of these grids will be "unmarked".
 *
 * This routine is used (only) by "unlite_room()"
 */
static void cave_temp_room_unlite(void)
{
	int i;

	/* Apply flag changes */
	for (i = 0; i < temp_n; i++)
	{
		coord t = temp_g[i];

		/* No longer in the array */
		cave_info[t.y][t.x] &= ~(CAVE_TEMP);

		/* Darken the grid */
		cave_info[t.y][t.x] &= ~(CAVE_GLOW);

		/* Hack -- Forget "boring" grids */
		if (cave_feat[t.y][t.x] <= FEAT_INVIS)
		{
			/* Forget the grid */
			cave_info[t.y][t.x] &= ~(CAVE_MARK);
		}
	}

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Update stuff */
	update_stuff();

	/* Process the grids */
	for (i = 0; i < temp_n; i++)
	{
		/* Redraw the grid */
		lite_spot(temp_g[i]);
	}

	/* None left */
	temp_n = 0;
}




/*
 * Aux function -- see below
 */
static void cave_temp_room_aux(coord g)
{
	/* Avoid infinite recursion */
	if (cave_info[g.y][g.x] & (CAVE_TEMP)) return;

	/* Do not "leave" the current room */
	if (!(cave_info[g.y][g.x] & (CAVE_ROOM))) return;

	/* Paranoia -- verify space */
	if (temp_n == TEMP_MAX) return;

	/* Mark the grid as "seen" */
	cave_info[g.y][g.x] |= (CAVE_TEMP);

	/* Add it to the "seen" set */
	temp_g[temp_n++] = g;
}

/*
 * not sure if this should be public or not
 */
static void floodmark_room(coord g)
{
	int i;

	/* Add the initial grid */
	cave_temp_room_aux(g);

	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
		coord t = temp_g[i];

		/* Walls get lit, but stop light */
		if (!cave_floor_bold(t.y, t.x)) continue;

		/* Spread adjacent */
		cave_temp_room_aux(coord(t.x,t.y + 1));
		cave_temp_room_aux(coord(t.x,t.y - 1));
		cave_temp_room_aux(coord(t.x + 1,t.y));
		cave_temp_room_aux(coord(t.x - 1,t.y));

		/* Spread diagonal */
		cave_temp_room_aux(coord(t.x + 1,t.y + 1));
		cave_temp_room_aux(coord(t.x - 1,t.y - 1));
		cave_temp_room_aux(coord(t.x + 1,t.y - 1));
		cave_temp_room_aux(coord(t.x - 1,t.y + 1));
	}
}


/*
 * Illuminate any room containing the given location.
 */
void lite_room(coord g)
{
	floodmark_room(g);

	/* Now, lite them all up at once */
	cave_temp_room_lite();
}


/*
 * Darken all rooms containing the given location
 */
void unlite_room(coord g)
{
	floodmark_room(g);

	/* Now, darken them all at once */
	cave_temp_room_unlite();
}



/*
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
bool lite_area(int dam, int rad)
{
	/* Hack -- Message */
	if (!p_ptr->timed[TMD_BLIND])
	{
		msg_print("You are surrounded by a white light.");
	}

	/* Hook into the "project()" function */
	(void)project(-1, rad, p_ptr->loc, dam, GF_LITE_WEAK, PROJECT_GRID | PROJECT_KILL);

	/* Lite up the room */
	lite_room(p_ptr->loc);

	/* Assume seen */
	return (TRUE);
}


/*
 * Hack -- call darkness around the player
 * Affect all monsters in the projection radius
 */
bool unlite_area(int dam, int rad)
{
	/* Hack -- Message */
	if (!p_ptr->timed[TMD_BLIND])
	{
		msg_print("Darkness surrounds you.");
	}

	/* Hook into the "project()" function */
	(void)project(-1, rad, p_ptr->loc, dam, GF_DARK_WEAK, PROJECT_GRID | PROJECT_KILL);

	/* Lite up the room */
	unlite_room(p_ptr->loc);

	/* Assume seen */
	return (TRUE);
}

/* returns true iff using an actual target */
bool extrapolate_target(coord& g, int dir)
{
	/* Use the given direction */
	g = p_ptr->loc;
	if (0!=dir && 5!=dir)
		{		
		do	g += dd_coord[dir];
		while(in_bounds_fully(g.y,g.x));
		g -= dd_coord[dir];
		};

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		g = p_ptr->target;
		return true;
	}
	return false;
}

/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */
bool fire_ball(int typ, int dir, int dam, int rad)
{
	int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	coord t;

	if (extrapolate_target(t,dir)) flg &= ~(PROJECT_STOP);

	/* Analyze the "dir" and the "target".  Hurt items on floor. */
	return (project(-1, rad, t, dam, typ, flg));
}


/*
 * Cast multiple non-jumping ball spells at the same target.
 *
 * Targets absolute coordinates instead of a specific monster, so that
 * the death of the monster doesn't change the target's location.
 */
bool fire_swarm(int num, int typ, int dir, int dam, int rad)
{
	bool noticed = FALSE;
	coord t;

	extrapolate_target(t,dir);

	while (num--)
	{
		/* Analyze the "dir" and the "target".  Hurt items on floor. */
		if (project(-1, rad, t, dam, typ, PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL)) noticed = TRUE;
	}

	return noticed;
}


/*
 * Hack -- apply a "projection()" in a direction (or at the target)
 */
static bool project_hook(int typ, int dir, int dam, int flg)
{
	coord t(p_ptr->loc + dd_coord[dir]);

	/* Pass through the target if needed */
	flg |= (PROJECT_THRU);

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		t = p_ptr->target;
	}

	/* Analyze the "dir" and the "target", do NOT explode */
	return (project(-1, 0, t, dam, typ, flg));
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a "bolt"
 * Affect monsters (not grids or objects)
 */
bool fire_bolt(int typ, int dir, int dam)
{
	return (project_hook(typ, dir, dam, PROJECT_STOP | PROJECT_KILL));
}

/*
 * Cast a beam spell
 * Pass through monsters, as a "beam"
 * Affect monsters (not grids or objects)
 */
bool fire_beam(int typ, int dir, int dam)
{
	return (project_hook(typ, dir, dam, PROJECT_BEAM | PROJECT_KILL));
}

/*
 * Cast a bolt spell, or rarely, a beam spell
 */
bool fire_bolt_or_beam(int prob, int typ, int dir, int dam)
{
	if (rand_int(100) < prob)
	{
		return (fire_beam(typ, dir, dam));
	}
	else
	{
		return (fire_bolt(typ, dir, dam));
	}
}


/*
 * Some of the old functions
 */

bool lite_line(int dir)
{
	return (project_hook(GF_LITE_WEAK, dir, NdS(6, 8), PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL));
}

bool strong_lite_line(int dir)
{
	return (project_hook(GF_LITE, dir, NdS(10, 8), PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL));
}

bool drain_life(int dir, int dam)
{
	return (project_hook(GF_OLD_DRAIN, dir, dam, PROJECT_STOP | PROJECT_KILL));
}

bool wall_to_mud(int dir)
{
	return (project_hook(GF_KILL_WALL, dir, 20 + randint(30), PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL));
}

bool destroy_door(int dir)
{
	return (project_hook(GF_KILL_DOOR, dir, 0, PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM));
}

bool disarm_trap(int dir)
{
	return (project_hook(GF_KILL_TRAP, dir, 0, PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM));
}

bool heal_monster(int dir)
{
	return (project_hook(GF_OLD_HEAL, dir, NdS(4, 6), PROJECT_STOP | PROJECT_KILL));
}

bool speed_monster(int dir)
{
	return (project_hook(GF_OLD_SPEED, dir, p_ptr->lev, PROJECT_STOP | PROJECT_KILL));
}

bool slow_monster(int dir)
{
	return (project_hook(GF_OLD_SLOW, dir, p_ptr->lev, PROJECT_STOP | PROJECT_KILL));
}

bool sleep_monster(int dir)
{
	return (project_hook(GF_OLD_SLEEP, dir, p_ptr->lev, PROJECT_STOP | PROJECT_KILL));
}

bool confuse_monster(int dir, int plev)
{
	return (project_hook(GF_OLD_CONF, dir, plev, PROJECT_STOP | PROJECT_KILL));
}

bool poly_monster(int dir)
{
	return (project_hook(GF_OLD_POLY, dir, p_ptr->lev, PROJECT_STOP | PROJECT_KILL));
}

bool clone_monster(int dir)
{
	return (project_hook(GF_OLD_CLONE, dir, 0, PROJECT_STOP | PROJECT_KILL));
}

bool fear_monster(int dir, int plev)
{
	return (project_hook(GF_TURN_ALL, dir, plev, PROJECT_STOP | PROJECT_KILL));
}

bool teleport_monster(int dir)
{
	return (project_hook(GF_AWAY_ALL, dir, MAX_SIGHT * 5, PROJECT_BEAM | PROJECT_KILL));
}



/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */

bool door_creation(void)
{
	return (project(-1, 1, p_ptr->loc, 0, GF_MAKE_DOOR, PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE));
}

bool trap_creation(void)
{
	return (project(-1, 1, p_ptr->loc, 0, GF_MAKE_TRAP, PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE));
}

bool destroy_doors_touch(void)
{
	return (project(-1, 1, p_ptr->loc, 0, GF_KILL_DOOR, PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE));
}

bool sleep_monsters_touch(void)
{
	return (project(-1, 1, p_ptr->loc, p_ptr->lev, GF_OLD_SLEEP, PROJECT_KILL | PROJECT_HIDE));
}


/*
 * Curse the players armor
 */
bool curse_armor(void)
{
	object_type* const o_ptr = &p_ptr->inventory[INVEN_BODY];	/* Curse the body armor */

	char o_name[80];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, ODESC_FULL);

	/* Attempt a saving throw for artifacts */
	if (o_ptr->is_artifact() && one_in_(2))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your armor", o_name);
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		/* Blast the armor */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_BLASTED;
		o_ptr->to_a = 0 - randint(5) - randint(5);
		o_ptr->to_h = 0;
		o_ptr->to_d = 0;
		o_ptr->ac = 0;
		o_ptr->d.clear();

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP);
	}

	return (TRUE);
}


/*
 * Curse the players weapon
 */
bool curse_weapon(void)
{
	object_type *o_ptr = &p_ptr->inventory[INVEN_WIELD];	/* Curse the weapon */

	char o_name[80];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, ODESC_FULL);

	/* Attempt a saving throw */
	if (o_ptr->is_artifact() && one_in_(2))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your weapon", o_name);
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		/* Shatter the weapon */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_SHATTERED;
		o_ptr->to_h = 0 - randint(5) - randint(5);
		o_ptr->to_d = 0 - randint(5) - randint(5);
		o_ptr->to_a = 0;
		o_ptr->ac = 0;
		o_ptr->d.clear();

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP);
	}

	/* Notice */
	return (TRUE);
}


/*
 * Brand weapons (or ammo)
 *
 * Turns the (non-magical) object into an ego-item of 'brand_type'.
 */
void brand_object(object_type *o_ptr, byte brand_type)
{
	/* you can never modify artifacts / ego-items */
	/* you can never modify broken / cursed items */
	if (o_ptr->k_idx &&
	    !o_ptr->is_artifact() && !o_ptr->is_ego_item() &&
	    !o_ptr->is_broken_or_cursed())
	{
		const char* act = "magical";
		char o_name[80];

		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, ODESC_BASE);

		switch (brand_type)
		{
			case EGO_BRAND_FIRE:
			case EGO_FLAME:
				act = "fiery";
				break;
			case EGO_BRAND_COLD:
			case EGO_FROST:
				act = "frosty";
				break;
			case EGO_BRAND_POIS:
			case EGO_AMMO_VENOM:
				act = "sickly";
				break;
		}

		/* Describe */
		msg_format("A %s aura surrounds the %s.", act, o_name);

		/* Brand the object */
		o_ptr->name2 = brand_type;

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP);
	
		/* Enchant */
		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}
	else
	{
		if (OPTION(flush_failure)) flush();
		msg_print("The Branding failed.");
	}
}


/*
 * Brand the current weapon
 */
void brand_weapon(void)
{
	object_type* const o_ptr = &p_ptr->inventory[INVEN_WIELD];
	byte brand_type = one_in_(4) ? EGO_BRAND_FIRE : EGO_BRAND_COLD;	/* Select a brand */

	/* Brand the weapon */
	brand_object(o_ptr, brand_type);
}


/*
 * Hook to specify "ammo"
 */
static bool item_tester_hook_ammo(const object_type *o_ptr)
{
	switch (o_ptr->obj_id.tval)
	{
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:	return TRUE;
	}

	return FALSE;
}


/*
 * Brand some (non-magical) ammo
 */
bool brand_ammo(void)
{
	int item;
	object_type *o_ptr;
	const char* q = "Brand which kind of ammunition? ";
	const char* s = "You have nothing to brand.";
	int r;
	byte brand_type;

	/* Only accept ammo */
	item_tester_hook = item_tester_hook_ammo;

	/* Get an item */
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the object */
	o_ptr = get_o_ptr_from_inventory_or_floor(item);

	r = rand_int(100);

	/* Select the brand */
	if (r < 33)
		brand_type = EGO_FLAME;
	else if (r < 67)
		brand_type = EGO_FROST;
	else
		brand_type = EGO_AMMO_VENOM;

	/* Brand the ammo */
	brand_object(o_ptr, brand_type);

	/* Done */
	return (TRUE);
}


/*
 * Enchant some (non-magical) bolts
 */
bool brand_bolts(void)
{
	int item;
	object_type *o_ptr;
	const char* q = "Brand which bolts? ";
	const char* s = "You have no bolts to brand.";

	/* Restrict choices to bolts */
	item_tester_tval = TV_BOLT;

	/* Get an item */
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the object */
	o_ptr = get_o_ptr_from_inventory_or_floor(item);

	/* Brand the bolts */
	brand_object(o_ptr, EGO_FLAME);

	/* Done */
	return (TRUE);
}


/*
 * Hack -- activate the ring of power
 */
void ring_of_power(int dir)
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
			p_ptr->max_exp -= (p_ptr->max_exp / 4);
			check_experience();

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
