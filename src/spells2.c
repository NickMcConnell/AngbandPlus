/* File: spells2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/* 
 *
 *
 * Higher-level hooks for ``project()''.
 *
 *
 */

static bool project_hook(int typ, int dir, int dam, int rad, u32b flg)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int ty, tx;

	/* Use the given direction */
	ty = py + ddy[dir];
	tx = px + ddx[dir];

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Analyze the "dir" and the "target", do NOT explode */
	return (project(-1, rad, ty, tx, dam, typ, flg));
}


bool fire_bolt(int typ, int dir, int dam)
{
	u32b flg = PROJECT_STOP | PROJECT_GRID | PROJECT_KILL;
	return (project_hook(typ, dir, dam, 0, flg));
}


bool fire_beam(int typ, int dir, int dam)
{
	u32b flg = PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID;
	return (project_hook(typ, dir, dam, 0, flg));
}


bool fire_ball(int typ, int dir, int dam, int rad)
{
	u32b flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(typ, dir, dam, rad, flg));
}

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


bool fire_visible_monsters(int typ, int dam)
{
	u32b flg = PROJECT_KILL | PROJECT_GRID | PROJECT_VIEWABLE;

	return (project(-1, 0, 0, 0, dam, typ, flg));
}


bool fire_explosion(int y, int x, int typ, int rad, int dam)
{
	u32b flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	return (project(-100, rad, y, x, dam, typ, flg));
}


bool fire_godly_wrath(int y, int x, int typ, int rad, int dam, byte god)
{
	u32b flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	return (project(-99 + god, rad, y, x, dam, typ, flg));
}

bool fire_at_player(int typ, int dam)
{
	u32b flg = PROJECT_PLAYER | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	return (project(-100, 0, p_ptr->py, p_ptr->px, dam, typ, flg));
}

bool fire_mega_blast(int y, int x, int typ, int rad, int dam)
{
	u32b flg =
		PROJECT_BLAST | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL |
		PROJECT_ETHER;

	return (project(-100, rad, y, x, dam, typ, flg));
}

bool fire_meteor_shower(int typ, int dam)
{
	u32b flg =
		PROJECT_METEOR_SHOWER | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	return (project(-100, 0, 0, 0, dam, typ, flg));
}


/**************************************************************/



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
		p_ptr->window |= (PW_SPELL | PW_PLAYER);

		/* Heal 0-4 */
		if (num < 5)
		{
			mprint(MSG_BONUS, "You feel a little better.");
		}

		/* Heal 5-14 */
		else if (num < 15)
		{
			mprint(MSG_BONUS, "You feel better.");
		}

		/* Heal 15-34 */
		else if (num < 35)
		{
			mprint(MSG_BONUS, "You feel much better.");
		}

		/* Heal 35+ */
		else
		{
			mprint(MSG_BONUS, "You feel very good.");
		}

		/* Notice */
		return (TRUE);
	}

	/* Ignore */
	return (FALSE);
}

/* Heal insanity. */

bool heal_insanity(int val)
{
	if (p_ptr->csane < p_ptr->msane)
	{
		p_ptr->csane += val;

		if (p_ptr->csane >= p_ptr->msane)
		{
			p_ptr->csane = p_ptr->msane;
			p_ptr->csane_frac = 0;
		}

		p_ptr->redraw |= PR_SANITY;
		p_ptr->window |= (PW_SPELL | PW_PLAYER);

		if (val < 5)
		{
			mprint(MSG_BONUS, "You feel a little better.");
		}
		else if (val < 15)
		{
			mprint(MSG_BONUS, "You feel better.");
		}
		else if (val < 35)
		{
			mprint(MSG_BONUS, "You feel much better.");
		}
		else
		{
			mprint(MSG_BONUS, "You feel very good.");
		}

		return TRUE;
	}

	return FALSE;
}


/*
 * Leave a "glyph of warding" which prevents monster movement
 */
void warding_glyph(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* XXX XXX XXX */
	if (!cave_clean_bold(py, px))
	{
		mprint(MSG_TEMP, "The object resists the spell.");
		return;
	}

	/* Create a glyph */
	cave_set_feat(py, px, FEAT_GLYPH);
}




/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_pos[] = {
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
static cptr desc_stat_neg[] = {
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
bool do_dec_stat(int stat)
{
	bool sust = FALSE;

	/* Access the "sustain" */
	switch (stat)
	{
		case A_STR:
			if (p_ptr->sustain_str)
				sust = TRUE;
			break;
		case A_INT:
			if (p_ptr->sustain_int)
				sust = TRUE;
			break;
		case A_WIS:
			if (p_ptr->sustain_wis)
				sust = TRUE;
			break;
		case A_DEX:
			if (p_ptr->sustain_dex)
				sust = TRUE;
			break;
		case A_CON:
			if (p_ptr->sustain_con)
				sust = TRUE;
			break;
		case A_CHR:
			if (p_ptr->sustain_chr)
				sust = TRUE;
			break;
	}

	/* Sustain */
	if (sust)
	{
		/* Message */
		msg_format("You feel %s for a moment, but the feeling passes.",
			desc_stat_neg[stat]);

		/* Notice effect */
		return (TRUE);
	}

	/* Attempt to reduce the stat */
	if (dec_stat(stat, 10, FALSE))
	{
		/* Message */
		mformat(MSG_WARNING, "You feel very %s.", desc_stat_neg[stat]);

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
		mformat(MSG_BONUS, "You feel less %s.", desc_stat_neg[stat]);

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
		mformat(MSG_BONUS, "Wow!  You feel very %s!", desc_stat_pos[stat]);

		/* Notice */
		return (TRUE);
	}

	/* Restoration worked */
	if (res)
	{
		/* Message */
		mformat(MSG_BONUS, "You feel less %s.", desc_stat_neg[stat]);

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
	object_type *o_ptr;

	/* Simply identify and know every item */
	for (o_ptr = inventory; o_ptr != NULL; o_ptr = o_ptr->next)
	{
		/* Skip non-objects */
		if (!o_ptr->k_idx)
			continue;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);
	}
}






/*
 * Used by the "enchant" function (chance of failure)
 */
static int enchant_table[16] = {
	0, 10, 50, 100, 200,
	300, 400, 500, 700, 950,
	990, 992, 995, 997, 999,
	1000
};


/* Added by GJW -KMW- */
static int enchant_table_dam[20] = {
	0, 10, 50, 100, 100,
	100, 200, 300, 400, 500,
	600, 700, 700, 700, 800,
	800, 850, 900, 950, 975
};


/*
 * Uncurse an item.
 */

bool uncurse_item(object_type * o_ptr, bool full)
{
	u32b f1, f2, f3;

	/* Skip non-objects */
	if (!o_ptr->k_idx)
		return FALSE;

	/* Uncursed already */
	if (!cursed_p(o_ptr))
		return FALSE;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Heavily Cursed Items need a special spell */
	if (!full && (f3 & (TR3_HEAVY_CURSE)))
		return FALSE;

	/* Perma-Cursed Items can NEVER be uncursed */
	if (f3 & (TR3_PERMA_CURSE))
		return FALSE;

	/* Uncurse it */
	o_ptr->ident &= ~(IDENT_CURSED);

	/* Hack -- Assume felt */
	o_ptr->ident |= (IDENT_SENSE);

	/* Take note */
	o_ptr->note = quark_add("uncursed");

	return TRUE;
}

/*
 * Removes curses from items in inventory
 *
 * Note that Items which are "Perma-Cursed" (The One Ring,
 * The Crown of Morgoth) can NEVER be uncursed.
 *
 * Note that if "all" is FALSE, then Items which are
 * "Heavy-Cursed" (Mormegil, Calris, and Weapons of Morgul)
 * will not be uncursed.
 */
static int remove_curse_aux(int all)
{
	int i, cnt = 0;

	/* Attempt to uncurse items being worn */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		object_type *o_ptr = equipment[i];

		if (!o_ptr)
			continue;

		if (uncurse_item(o_ptr, all))
		{
			/* Count the uncursings */
			cnt++;

			/* Recalculate the bonuses */
			p_ptr->update |= (PU_BONUS);

			/* Window stuff */
			p_ptr->window |= (PW_EQUIP);
		}
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
		mprint(MSG_BONUS, "You feel your life energies returning.");

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
 * Use the "roff()" routines, perhaps.  XXX XXX
 *
 * Use the "show_file()" method, perhaps.  XXX XXX
 */
void self_knowledge(void)
{
	int i = 0, j, k;

	u32b f1 = 0L, f2 = 0L, f3 = 0L;

	object_type *o_ptr;

	cptr info[128];


	/* Acquire item flags from equipment */
	for (k = 0; k < EQUIP_MAX; k++)
	{
		u32b t1, t2, t3;

		o_ptr = equipment[k];

		/* Skip non-objects */
		if (!o_ptr || !o_ptr->k_idx)
			continue;

		/* Extract the flags */
		object_flags(o_ptr, &t1, &t2, &t3);

		/* Extract flags */
		f1 |= t1;
		f2 |= t2;
		f3 |= t3;
	}


	if (p_ptr->blind)
	{
		info[i++] = "You cannot see.";
	}
	if (p_ptr->confused)
	{
		info[i++] = "You are confused.";
	}
	if (p_ptr->afraid)
	{
		info[i++] = "You are terrified.";
	}
	if (p_ptr->cut)
	{
		info[i++] = "You are bleeding.";
	}
	if (p_ptr->stun)
	{
		info[i++] = "You are stunned.";
	}
	if (p_ptr->poisoned)
	{
		info[i++] = "You are poisoned.";
	}
	if (p_ptr->image)
	{
		info[i++] = "You are hallucinating.";
	}

	if (p_ptr->shape)
	{
		info[i++] =
			format("You are in the form of a%s %s.",
			(is_a_vowel(shape_info[p_ptr->shape - 1].name[0]) ? "n" : ""),
			shape_info[p_ptr->shape - 1].name);
	}

	if (p_ptr->mega_spells)
	{
		info[i++] = "You have phenomenal mental powers.";
	}

	if (p_ptr->flying)
	{
		info[i++] = "You are flying.";
	}

	if (p_ptr->vampiric)
	{
		info[i++] = "You have vampiric abilities.";
	}

	if (p_ptr->immovable)
	{
		info[i++] = "You move by magical means.";
	}

	if (p_ptr->aggravate)
	{
		info[i++] = "You aggravate monsters.";
	}
	if (p_ptr->teleport)
	{
		info[i++] = "Your position is very uncertain.";
	}

	if (p_ptr->blessed)
	{
		info[i++] = "You feel rightous.";
	}
	if (p_ptr->hero)
	{
		info[i++] = "You feel heroic.";
	}
	if (p_ptr->shero)
	{
		info[i++] = "You are in a battle rage.";
	}
	if (p_ptr->protevil)
	{
		info[i++] = "You are protected from evil.";
	}
	if (p_ptr->shield)
	{
		info[i++] = "You are protected by a mystic shield.";
	}
	if (p_ptr->invuln)
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
		info[i++] = "You can learn some new powers.";
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

	if (p_ptr->immaterial)
	{
		info[i++] = "You can pass through walls.";
	}

	if (p_ptr->allseeing)
	{
		info[i++] = "You can sense the rock beneath your feet.";
	}

	if (p_ptr->weird_attack)
	{
		info[i++] = "Your attacks cause strange effects.";
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
	else if ((p_ptr->resist_acid) && (p_ptr->oppose_acid))
	{
		info[i++] = "You resist acid exceptionally well.";
	}
	else if ((p_ptr->resist_acid) || (p_ptr->oppose_acid))
	{
		info[i++] = "You are resistant to acid.";
	}

	if (p_ptr->immune_elec)
	{
		info[i++] = "You are completely immune to lightning.";
	}
	else if ((p_ptr->resist_elec) && (p_ptr->oppose_elec))
	{
		info[i++] = "You resist lightning exceptionally well.";
	}
	else if ((p_ptr->resist_elec) || (p_ptr->oppose_elec))
	{
		info[i++] = "You are resistant to lightning.";
	}

	if (p_ptr->immune_fire)
	{
		info[i++] = "You are completely immune to fire.";
	}
	else if ((p_ptr->resist_fire) && (p_ptr->oppose_fire))
	{
		info[i++] = "You resist fire exceptionally well.";
	}
	else if ((p_ptr->resist_fire) || (p_ptr->oppose_fire))
	{
		info[i++] = "You are resistant to fire.";
	}

	if (p_ptr->immune_cold)
	{
		info[i++] = "You are completely immune to cold.";
	}
	else if ((p_ptr->resist_cold) && (p_ptr->oppose_cold))
	{
		info[i++] = "You resist cold exceptionally well.";
	}
	else if ((p_ptr->resist_cold) || (p_ptr->oppose_cold))
	{
		info[i++] = "You are resistant to cold.";
	}

	if ((p_ptr->resist_pois) && (p_ptr->oppose_pois))
	{
		info[i++] = "You resist poison exceptionally well.";
	}
	else if ((p_ptr->resist_pois) || (p_ptr->oppose_pois))
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

	if (p_ptr->sustain_str)
	{
		info[i++] = "Your strength is sustained.";
	}
	if (p_ptr->sustain_int)
	{
		info[i++] = "Your intelligence is sustained.";
	}
	if (p_ptr->sustain_wis)
	{
		info[i++] = "Your wisdom is sustained.";
	}
	if (p_ptr->sustain_con)
	{
		info[i++] = "Your constitution is sustained.";
	}
	if (p_ptr->sustain_dex)
	{
		info[i++] = "Your dexterity is sustained.";
	}
	if (p_ptr->sustain_chr)
	{
		info[i++] = "Your charisma is sustained.";
	}

	if (f1 & (TR1_STR))
	{
		info[i++] = "Your strength is affected by your equipment.";
	}
	if (f1 & (TR1_INT))
	{
		info[i++] = "Your intelligence is affected by your equipment.";
	}
	if (f1 & (TR1_WIS))
	{
		info[i++] = "Your wisdom is affected by your equipment.";
	}
	if (f1 & (TR1_DEX))
	{
		info[i++] = "Your dexterity is affected by your equipment.";
	}
	if (f1 & (TR1_CON))
	{
		info[i++] = "Your constitution is affected by your equipment.";
	}
	if (f1 & (TR1_CHR))
	{
		info[i++] = "Your charisma is affected by your equipment.";
	}

	if (f1 & (TR1_STEALTH))
	{
		info[i++] = "Your stealth is affected by your equipment.";
	}
	if (f1 & (TR1_SEARCH))
	{
		info[i++] =
			"Your searching ability is affected by your equipment.";
	}
	if (f1 & (TR1_INFRA))
	{
		info[i++] = "Your infravision is affected by your equipment.";
	}
	if (f1 & (TR1_TUNNEL))
	{
		info[i++] = "Your digging ability is affected by your equipment.";
	}
	if (f1 & (TR1_SPEED))
	{
		info[i++] = "Your speed is affected by your equipment.";
	}
	if (f1 & (TR1_BLOWS))
	{
		info[i++] = "Your attack speed is affected by your equipment.";
	}
	if (f1 & (TR1_SHOTS))
	{
		info[i++] = "Your shooting speed is affected by your equipment.";
	}
	if (f1 & (TR1_MIGHT))
	{
		info[i++] = "Your shooting might is affected by your equipment.";
	}


	/* Access the current weapon */
	o_ptr = equipment[EQUIP_WIELD];

	/* Analyze the weapon */
	if (o_ptr && o_ptr->k_idx)
	{
		/* Special "Attack Bonuses" */
		if (f1 & (TR1_BRAND_ACID))
		{
			info[i++] = "Your weapon melts your foes.";
		}
		if (f1 & (TR1_BRAND_ELEC))
		{
			info[i++] = "Your weapon shocks your foes.";
		}
		if (f1 & (TR1_BRAND_FIRE))
		{
			info[i++] = "Your weapon burns your foes.";
		}
		if (f1 & (TR1_BRAND_COLD))
		{
			info[i++] = "Your weapon freezes your foes.";
		}

		/* Added by GJW -KMW- */
		if (f1 & (TR1_BRAND_POIS))
		{
			info[i++] = "Your weapon poisons your foes.";
		}

		/* Special "slay" flags */
		if (f1 & (TR1_SLAY_ANIMAL))
		{
			info[i++] = "Your weapon strikes at animals with extra force.";
		}
		if (f1 & (TR1_SLAY_EVIL))
		{
			info[i++] = "Your weapon strikes at evil with extra force.";
		}
		if (f1 & (TR1_SLAY_UNDEAD))
		{
			info[i++] = "Your weapon strikes at undead with holy wrath.";
		}
		if (f1 & (TR1_SLAY_DEMON))
		{
			info[i++] = "Your weapon strikes at demons with holy wrath.";
		}
		if (f1 & (TR1_SLAY_ORC))
		{
			info[i++] = "Your weapon is especially deadly against orcs.";
		}
		if (f1 & (TR1_SLAY_TROLL))
		{
			info[i++] = "Your weapon is especially deadly against trolls.";
		}
		if (f1 & (TR1_SLAY_GIANT))
		{
			info[i++] = "Your weapon is especially deadly against giants.";
		}
		if (f1 & (TR1_SLAY_DRAGON))
		{
			info[i++] =
				"Your weapon is especially deadly against dragons.";
		}

		/* Special "kill" flags */
		if (f1 & (TR1_KILL_DRAGON))
		{
			info[i++] = "Your weapon is a great bane of dragons.";
		}


		/* Indicate Blessing */
		if (f3 & (TR3_BLESSED))
		{
			info[i++] = "Your weapon has been blessed by the gods.";
		}

		/* Hack */
		if (f3 & (TR3_IMPACT))
		{
			info[i++] = "Your weapon can induce earthquakes.";
		}
	}

	if (p_ptr->munchkin)
	{
		info[i++] = "You have ghostly powers.";
	}


	/* Save the screen */
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
		if ((k == 22) && (j + 1 < i))
		{
			prt("-- more --", k, 0);
			inkey();

			/* Clear the screen */
			Term_clear();

			/* Start at the top */
			k = 2;

			/* Label the information */
			prt("     Your Attributes:", 1, 0);
		}
	}

	/* Pause */
	prt("[Press any key to continue]", k, 0);
	inkey();

	/* Restore the screen */
	screen_load();
}






/*
 * Forget everything
 */
bool lose_all_info(void)
{
	object_type *o_ptr;

	/* Forget info about objects */
	for (o_ptr = inventory; o_ptr != NULL; o_ptr = o_ptr->next)
	{

		/* Skip non-objects */
		if (!o_ptr->k_idx)
			continue;

		/* Allow "protection" by the MENTAL flag */
		if (o_ptr->ident & (IDENT_MENTAL))
			continue;

		/* Remove "default inscriptions" */
		if (o_ptr->note && (o_ptr->ident & (IDENT_SENSE)))
		{
			/* Access the inscription */
			cptr q = quark_str(o_ptr->note);

			/* Hack -- Remove auto-inscriptions */
			if ((streq(q, "cursed")) || (streq(q, "broken")) ||
				(streq(q, "good")) || (streq(q, "average")) ||
				(streq(q, "excellent")) || (streq(q, "worthless")) ||
				(streq(q, "special")) || (streq(q, "terrible")))
			{
				/* Forget the inscription */
				o_ptr->note = 0;
			}
		}

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
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Mega-Hack -- Forget the map */
	wiz_dark();

	/* It worked */
	return (TRUE);
}



/*
 * Create stairs at the player location
 */
void stair_creation(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* XXX XXX XXX */
	if (!cave_valid_bold(py, px))
	{
		if (character_generated)
			mprint(MSG_TEMP, "The object resists the spell.");
		return;
	}

	/* Create a staircase */
	if (p_ptr->inside_special > 0)
	{ /* in arena or quest -KMW- */
		if (character_generated)
			mprint(MSG_TEMP, "There is no effect!");
	}
	else if (!p_ptr->depth)
	{
		cave_set_feat(py, px, FEAT_LESS);

	}
	else if (p_ptr->depth >= MAX_DEPTH - 1)
	{
		cave_set_feat(py, px, FEAT_MORE);
	}
	else if (rand_int(100) < 50)
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
static bool item_tester_hook_weapon(object_type * o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_BOW:
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "armour"
 */
static bool item_tester_hook_armour(object_type * o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			return (TRUE);
		}
	}

	return (FALSE);
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
bool enchant(object_type * o_ptr, int n, int eflag)
{
	int i, chance, prob;

	bool res = FALSE;

	bool a = artifact_p(o_ptr);

	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);


	/* Large piles resist enchantment */
	prob = o_ptr->number * 100;

	/* Missiles are easy to enchant */
	if ((o_ptr->tval == TV_BOLT) || (o_ptr->tval == TV_ARROW) ||
		(o_ptr->tval == TV_SHOT))
	{
		prob = prob / 20;
	}

	/* Try "n" times */
	for (i = 0; i < n; i++)
	{
		/* Hack -- Roll for pile resistance */
		if (rand_int(prob) >= 100)
			continue;

		/* Enchant to hit */
		if (eflag & (ENCH_TOHIT))
		{
			if (o_ptr->to_h < 0)
				chance = 0;
			else if (o_ptr->to_h > 15)
				chance = 1000;
			else
				chance = enchant_table[o_ptr->to_h];

			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				o_ptr->to_h++;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (cursed_p(o_ptr) && (!(f3 & (TR3_PERMA_CURSE))) &&
					(o_ptr->to_h >= 0) && (rand_int(100) < 25))
				{
					mprint(MSG_BONUS, "The curse is broken!");
					o_ptr->ident &= ~(IDENT_CURSED);
					o_ptr->ident |= (IDENT_SENSE);
					o_ptr->note = quark_add("uncursed");
				}
			}
		}

		/* Enchant to damage */
		if (eflag & (ENCH_TODAM))
		{
			if (o_ptr->to_d < 0)
				chance = 0;
			/* Generally limit +to-dam to weapon's natural damage
			 * limitation.  E.g., a tulwar (2d4) can go to +8.
			 * Note the effect upon missiles.  From GJW -KMW- */
			else if (o_ptr->to_d > 19)
				chance = 1000;
			else
			{
				if (o_ptr->tval == TV_BOW)
					chance = enchant_table[o_ptr->to_d];
				else
				{
					chance = enchant_table_dam[o_ptr->to_d];
					if ((o_ptr->dd * o_ptr->ds <= o_ptr->to_d) &&
						(chance < 995))
						chance = 995;
				}
			}

			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				o_ptr->to_d++;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (cursed_p(o_ptr) && (!(f3 & (TR3_PERMA_CURSE))) &&
					(o_ptr->to_d >= 0) && (rand_int(100) < 25))
				{
					mprint(MSG_BONUS, "The curse is broken!");
					o_ptr->ident &= ~(IDENT_CURSED);
					o_ptr->ident |= (IDENT_SENSE);
					o_ptr->note = quark_add("uncursed");
				}
			}
		}

		/* Enchant to armor class */
		if (eflag & (ENCH_TOAC))
		{
			if (o_ptr->to_a < 0)
				chance = 0;
			else if (o_ptr->to_a > 15)
				chance = 1000;
			else
				chance = enchant_table[o_ptr->to_a];

			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				o_ptr->to_a++;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (cursed_p(o_ptr) && (!(f3 & (TR3_PERMA_CURSE))) &&
					(o_ptr->to_a >= 0) && (rand_int(100) < 25))
				{
					mprint(MSG_BONUS, "The curse is broken!");
					o_ptr->ident &= ~(IDENT_CURSED);
					o_ptr->ident |= (IDENT_SENSE);
					o_ptr->note = quark_add("uncursed");
				}
			}
		}

		if (eflag & (ENCH_MAKE_EGO))
		{
			if (!o_ptr->name2)
			{
				res = make_ego_item(o_ptr, 100);
			}
		}

		if (eflag & (ENCH_MAKE_ART))
		{
			if (!o_ptr->name1)
			{
				res = make_artifact(o_ptr, 100);
			}
		}
	}

	/* Failure */
	if (!res)
		return (FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Success */
	return (TRUE);
}


/*
 * A more liberal enchantment spell.
 */

bool enchant_spell2(int num, int flags)
{
	char o_name[80];

	object_type *o_ptr;

	cptr q, s;

	q = "Enchant which item? ";
	s = "You have nothing to enchant.";

	o_ptr =
		get_item("Enchant which item", "You have nothing to enchant.",
		p_ptr->py, p_ptr->px, (USE_INVEN | USE_FLOOR));

	if (!o_ptr)
		return FALSE;

	/* Description */
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Describe */
	mformat(MSG_BONUS, "Your %s glow%s brightly!", o_name,
		((o_ptr->number > 1) ? "" : "s"));

	/* Enchant */
	if (!enchant(o_ptr, num, flags))
	{
		/* Flush */
		if (flush_failure)
			flush();

		/* Message */
		msg_print("The enchantment failed.");
	}

	/* Something happened */
	return (TRUE);

}

/*
 * Enchant an item (in the inventory or on the floor)
 * Note that "num_ac" requires armour, else weapon
 * Returns TRUE if attempted, FALSE if cancelled
 */
bool enchant_spell(int num_hit, int num_dam, int num_ac)
{
	bool okay = FALSE;

	object_type *o_ptr;

	char o_name[80];


	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Enchant armor if requested */
	if (num_ac)
		item_tester_hook = item_tester_hook_armour;

	/* Get an item */
	o_ptr =
		get_item("Enchant which item", "You have nothing to enchant.",
		p_ptr->py, p_ptr->px, (USE_INVEN | USE_FLOOR));

	if (!o_ptr)
		return (FALSE);

	/* Description */
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Describe */
	mformat(MSG_BONUS, "Your %s glow%s brightly!", o_name,
		((o_ptr->number > 1) ? "" : "s"));

	/* Enchant */
	if (enchant(o_ptr, num_hit, ENCH_TOHIT))
		okay = TRUE;
	if (enchant(o_ptr, num_dam, ENCH_TODAM))
		okay = TRUE;
	if (enchant(o_ptr, num_ac, ENCH_TOAC))
		okay = TRUE;

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure)
			flush();

		/* Message */
		msg_print("The enchantment failed.");
	}

	/* Something happened */
	return (TRUE);
}



/*
 * An interface to ``transmute''.
 */
bool transmute_spell(bool full)
{
	object_type *o_ptr;
	char o_name[80];

	bool ret = FALSE;

	/* Get an item */
	o_ptr =
		get_item("Transmute which item",
		"You have nothing you can transmute.", p_ptr->py, p_ptr->px,
		(USE_INVEN | USE_FLOOR));

	if (!o_ptr)
		return FALSE;

	object_desc(o_name, o_ptr, FALSE, 0);

	/* Randomly transmute the item. */
	if (!full)
	{
		if (transmute_random(o_ptr, p_ptr->depth))
		{
			ret = TRUE;
		}

	}
	else
	{
		object_type *j_ptr;

		j_ptr =
			get_item("Transmute with what",
			"You have nothing to transmute with.", p_ptr->py, p_ptr->px,
			(USE_INVEN | USE_FLOOR));

		if (!j_ptr)
			return FALSE;

		if (transmute(o_ptr, j_ptr->stuff))
		{
			ret = TRUE;
		}
	}

	if (ret)
	{
		if (o_ptr->number > 1)
		{
			msg_format("%s %d %s turned to %s!",
				(o_ptr->stack == STACK_INVEN ? "Your" : "The"),
				o_ptr->number, o_name,
				materials[o_ptr->stuff].made_of_name);
		}
		else
		{
			msg_format("%s %s turned to %s!",
				(o_ptr->stack == STACK_INVEN ? "Your" : "The"), o_name,
				materials[o_ptr->stuff].made_of_name);
		}
	}

	return ret;
}





/*
 * Brand some ammunition.  Used by Cubragol and a mage spell.  The spell was
 * moved here from cmd6.c where it used to be for Cubragol only.  I've also
 * expanded it to do either frost, fire or venom, at random. -GJW	-KMW-
 */
void brand_ammo(int brand_type, int bolts_only)
{
	object_type *o_ptr;

	int allowable;

	if (bolts_only)
		allowable = TV_BOLT;
	else
		allowable = TV_BOLT | TV_ARROW | TV_SHOT;

	for (o_ptr = inventory; o_ptr != NULL; o_ptr = o_ptr->next)
	{

		if ((bolts_only) && (o_ptr->tval != TV_BOLT))
			continue;

		if ((!bolts_only) && (o_ptr->tval != TV_BOLT) &&
			(o_ptr->tval != TV_ARROW) && (o_ptr->tval != TV_SHOT))
			continue;

		if ((!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
			(!cursed_p(o_ptr) && !broken_p(o_ptr)))
			break;
	}

	/* Enchant the ammo (or fail) */
	if (rand_int(100) < 50)
	{
		char *ammo_name, *aura_name, msg[48];
		int aura_type, r;

		if (brand_type == 1)
			r = 0; /* fire only */
		else if (brand_type == 2)
			r = 99;	/* poison only */
		else
			r = rand_int(100);

		if (r < 33)
		{
			aura_name = "fiery";
			aura_type = EGO_FLAME;
		}
		else if (r < 67)
		{
			aura_name = "frosty";
			aura_type = EGO_FROST;
		}
		else
		{
			aura_name = "sickly";
			aura_type = EGO_VENOM;
		}

		if (o_ptr->tval == TV_BOLT)
			ammo_name = "bolts";
		else if (o_ptr->tval == TV_ARROW)
			ammo_name = "arrows";
		else
			ammo_name = "shots";

		sprintf(msg, "Your %s are covered in a %s aura!", ammo_name,
			aura_name);
		mprint(MSG_BONUS, msg);
		o_ptr->name2 = aura_type;
		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);

	}
	else
	{
		if (flush_failure)
			flush();
		msg_print("The enchantment failed.");
	}
}


/*
 * Brand the current weapon
 * From GJW	-KMW-
 */
void brand_weapon(void)
{
	object_type *o_ptr;

	o_ptr = equipment[EQUIP_WIELD];

	/* you can never modify artifacts / ego-items */
	/* you can never modify broken / cursed items */
	if (o_ptr && o_ptr->k_idx && (!artifact_p(o_ptr)) &&
		(!ego_item_p(o_ptr)) && (!broken_p(o_ptr)) && (!cursed_p(o_ptr)))
	{
		char *act = NULL;
		char o_name[80];

		if (rand_int(100) < 25)
		{
			act = "is covered in a fiery shield!";
			o_ptr->name2 = EGO_BRAND_FIRE;
		}
		else
		{
			act = "glows deep, icy blue!";
			o_ptr->name2 = EGO_BRAND_COLD;
		}

		object_desc(o_name, o_ptr, FALSE, 0);
		mformat(MSG_BONUS, "Your %s %s", o_name, act);
		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}

	else
	{
		if (flush_failure)
			flush();
		msg_print("The Branding failed.");
	}
}


/*
 * Identify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was identified, else FALSE.
 */
bool ident_spell(void)
{
	object_type *o_ptr;

	char o_name[80];

	/* Get an item */
	o_ptr =
		get_item("Identify which item", "You have nothing to identify.",
		p_ptr->py, p_ptr->px, (USE_INVEN | USE_FLOOR));

	if (!o_ptr)
		return (FALSE);

	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Describe */
	msg_format("You have: %s.", o_name);

	/* Something happened */
	return (TRUE);
}


/*
 * Repair an item in your inventory.
 */
bool repair_spell(int dam)
{
	object_type *o_ptr;

	char o_name[80];

	/* Get an item */
	o_ptr =
		get_item("Repair which item", "You have nothing to repair.",
		p_ptr->py, p_ptr->px, (USE_INVEN | USE_FLOOR));

	if (!o_ptr)
		return (FALSE);

	/* Description */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Repair it. */
	if (repair_object(o_ptr, dam))
	{
		mformat(MSG_BONUS, "The %s looks less worn.", o_name);
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
	object_type *o_ptr;

	char o_name[80];

	/* Get an item */
	o_ptr =
		get_item("Identify which item", "You have nothing to identify.",
		p_ptr->py, p_ptr->px, (USE_INVEN | USE_FLOOR));

	if (!o_ptr)
		return FALSE;

	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Mark the item as fully known */
	o_ptr->ident |= (IDENT_MENTAL);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Handle stuff */
	handle_stuff();

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Describe */
	msg_format("You have: %s.", o_name);

	/* Describe it fully */
	identify_fully_aux(o_ptr);

	/* Success */
	return (TRUE);
}




/*
 * Hook for "get_item()".  Determine if something is rechargable.
 */
static bool item_tester_hook_recharge(object_type * o_ptr)
{
	/* Recharge staffs */
	if (o_ptr->tval == TV_STAFF)
		return (TRUE);

	/* Recharge wands */
	if (o_ptr->tval == TV_WAND)
		return (TRUE);

	/* Hack -- Recharge rods */
	if (o_ptr->tval == TV_ROD)
		return (TRUE);

	/* Nope */
	return (FALSE);
}


/*
 * Recharge a wand/staff/rod from the pack or on the floor.
 *
 * Mage -- Recharge I --> recharge(5)
 * Mage -- Recharge II --> recharge(40)
 * Mage -- Recharge III --> recharge(100)
 *
 * Priest -- Recharge --> recharge(15)
 *
 * Scroll of recharging --> recharge(60)
 *
 * recharge(20) = 1/6 failure for empty 10th level wand
 * recharge(60) = 1/10 failure for empty 10th level wand
 *
 * It is harder to recharge high level, and highly charged wands.
 *
 * XXX XXX XXX Beware of "sliding index errors".
 *
 * Should probably not "destroy" over-charged items, unless we
 * "replace" them by, say, a broken stick or some such.  The only
 * reason this is okay is because "scrolls of recharging" appear
 * BEFORE all staffs/wands/rods in the inventory.  Note that the
 * new "auto_sort_pack" option would correctly handle replacing
 * the "broken" wand with any other item (i.e. a broken stick).
 *
 * XXX XXX XXX Perhaps we should auto-unstack recharging stacks.
 */
bool recharge(int num)
{
	object_type *o_ptr;

	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item */
	o_ptr =
		get_item("Recharge which item", "You have nothing to recharge.",
		p_ptr->py, p_ptr->px, (USE_INVEN | USE_FLOOR));

	if (!o_ptr)
		return FALSE;

	recharge_item(num, o_ptr);


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);

	/* Something was done */
	return (TRUE);
}

/*
 * Recharge something on the floor.
 */
bool recharge_item(int num, object_type * o_ptr)
{
	int lev, t, i;

	/* Extract the object "level" */
	lev = k_info[o_ptr->k_idx].level;

	/* Recharge a rod */
	if (o_ptr->tval == TV_ROD)
	{
		/* Extract a recharge power */
		i = (100 - lev + num) / 5;

		/* Paranoia -- prevent crashes */
		if (i < 1)
			i = 1;

		/* Back-fire */
		if (rand_int(i) == 0)
		{
			/* Hack -- backfire */
			if (o_ptr->marked)
				mprint(MSG_WARNING,
					"The recharge backfires, draining the rod further!");

			/* Hack -- decharge the rod */
			if (o_ptr->pval < 10000)
				o_ptr->pval = (o_ptr->pval + 100) * 2;
		}

		/* Recharge */
		else
		{
			/* Rechange amount */
			t = (num * damroll(2, 4));

			/* Recharge by that amount */
			if (o_ptr->pval > t)
			{
				o_ptr->pval -= t;
			}

			/* Fully recharged */
			else
			{
				o_ptr->pval = 0;
			}
		}
	}

	/* Recharge wand/staff */
	else
	{
		/* Recharge power */
		i = (num + 100 - lev - (10 * o_ptr->pval)) / 15;

		/* Paranoia -- prevent crashes */
		if (i < 1)
			i = 1;

		/* Back-fire XXX XXX XXX */
		if (rand_int(i) == 0)
		{
			byte ix = o_ptr->ix;
			byte iy = o_ptr->iy;
			byte stack = o_ptr->stack;
			monster_type *owner = o_ptr->owner;

			/* Show a message if the object is visible. */
			if (o_ptr->marked || stack == STACK_INVEN)
				mprint(MSG_WARNING, "There is a bright flash of light.");

			/* Delete the object. */
			remove_object(o_ptr);

			/* Make a new one. */
			o_ptr = new_object();

			object_prep(o_ptr, lookup_kind(TV_JUNK, SV_BROKEN_STICK));

			/* Insert it into the world. */
			switch (stack)
			{
				case STACK_INVEN:
					inven_carry(o_ptr);
					break;

				case STACK_MON_INVEN:
					monster_inven_carry(owner, o_ptr);
					break;

				case STACK_FLOOR:
					floor_carry(iy, ix, o_ptr);
					break;
			}

			/* Redraw the object. */
			if (stack == STACK_FLOOR)
				lite_spot(iy, ix);

			/* Recharge */
		}
		else
		{
			/* Extract a "power" */
			t = (num / (lev + 2)) + 1;

			/* Recharge based on the power */
			if (t > 0)
				o_ptr->pval += 2 + randint(t);

			/* Hack -- we no longer "know" the item */
			o_ptr->ident &= ~(IDENT_KNOWN);

			/* Hack -- we no longer think the item is empty */
			o_ptr->ident &= ~(IDENT_EMPTY);
		}
	}

	return TRUE;
}



/*
 * Multiple Monster Fear -KMW-
 */
bool fear_monsters(void)
{
	return (fire_visible_monsters(GF_TURN_ALL, p_ptr->lev));
}

/*
 * Speed monsters
 */
bool speed_monsters(void)
{
	return (fire_visible_monsters(GF_SPEED, p_ptr->lev));
}

/*
 * Slow monsters
 */
bool slow_monsters(void)
{
	return (fire_visible_monsters(GF_SLOW, p_ptr->lev));
}

/*
 * Sleep monsters
 */
bool sleep_monsters(void)
{
	return (fire_visible_monsters(GF_SLEEP, p_ptr->lev));
}


/*
 * Banish evil monsters
 */
bool banish_evil(int dist)
{
	return (fire_visible_monsters(GF_AWAY_EVIL, dist));
}


/*
 * Turn undead
 */
bool turn_undead(void)
{
	return (fire_visible_monsters(GF_TURN_UNDEAD, p_ptr->lev));
}


/*
 * Dispel undead monsters
 */
bool dispel_undead(int dam)
{
	return (fire_visible_monsters(GF_DISP_UNDEAD, dam));
}

/*
 * Dispel evil monsters
 */
bool dispel_evil(int dam)
{
	return (fire_visible_monsters(GF_DISP_EVIL, dam));
}

/*
 * Dispel all monsters
 */
bool dispel_monsters(int dam)
{
	return (fire_visible_monsters(GF_DISP_ALL, dam));
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
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx)
			continue;

		/* Skip aggravating monster (or player) */
		if (i == who)
			continue;

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
		if (player_has_los_bold(m_ptr->fy, m_ptr->fx))
		{
			/* Speed up (instantly) to racial base + 10 */
			if (m_ptr->mspeed < r_ptr->speed + 10)
			{
				/* Speed up */
				m_ptr->mspeed = r_ptr->speed + 10;
				speed = TRUE;
			}
		}
	}

	/* Messages */
	if (speed)
		mprint(MSG_WARNING, "You feel a sudden stirring nearby!");
	else if (sleep)
		mprint(MSG_WARNING, "You hear a sudden stirring in the distance!");
}


/*
 * Wake up monsters in LOS
 */
void awake_monsters(int who)
{
	int i;

	bool sleep = FALSE;

	/* Aggravate everyone nearby */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx)
			continue;

		/* Skip aggravating monster (or player) */
		if (i == who)
			continue;

		/* Wake up monsters in line of sight */
		if (player_has_los_bold(m_ptr->fy, m_ptr->fx))
		{
			if (m_ptr->csleep)
			{
				m_ptr->csleep = 0;
				sleep = TRUE;
			}
		}

	}

	/* Messages */
	if (sleep)
		mprint(MSG_WARNING, "You hear a stirring nearby!");
}


/*
 * Make monsters in LOS hostile.
 */
void hostile_monsters(int who)
{
	monster_type *m_ptr = NULL;
	char m_name[80];
	int i;

	if (who > 0)
	{
		m_ptr = &m_list[who];

		if (m_ptr->is_pet)
		{
			monster_desc(m_name, m_ptr, 0x80);
			mformat(MSG_WARNING, "%^s howls in rebellion!", m_name);

			m_ptr->is_pet = FALSE;
		}
	}

	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx)
			continue;

		/* Skip aggravating monster (or player) */
		if (i == who)
			continue;

		if (player_has_los_bold(m_ptr->fy, m_ptr->fx) && m_ptr->is_pet)
		{

			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			int tmp = r_ptr->level / 2 - randnor(p_ptr->lev, 1);
			int tmp2;
			int tmp3 = rand_int(11);

			if (tmp < 0)
			{
				tmp = 0;
			}

			tmp2 = adj_chr_pet_summon[p_ptr->stat_ind[A_CHR]] + tmp;

			/* Make the monster hostile. */
			if (tmp3 <= tmp2)
			{
				monster_desc(m_name, m_ptr, 0x80);
				mformat(MSG_WARNING, "%^s howls in rebellion!", m_name);

				m_ptr->is_pet = FALSE;
			}
		}

	}
}



/*
 * Hack -- call pets to player.
 */

void call_pets_toggle(void)
{
	/* Take a turn */
	p_ptr->energy_use = 100;

	if (p_ptr->pets_notice == 2)
	{
		p_ptr->pets_notice = 0;
		msg_print("You stop attracting your pet's attention.");

	}
	else if (p_ptr->pets_notice == 1)
	{

		if (!target_set(TARGET_GRID))
		{
			p_ptr->pets_notice = 0;
			msg_print("You stop attracting your pet's attention.");

		}
		else
		{
			p_ptr->pets_notice = 2;
			msg_print("You point your pet at your target.");
			awake_monsters(1);
		}

	}
	else
	{
		p_ptr->pets_notice = 1;
		msg_print("You shout for some help.");
		awake_monsters(1);
	}

	p_ptr->redraw |= (PR_SHOUT);
}

/*
 * Delete all non-unique monsters of a given "type" from the level
 */
bool genocide(void)
{
	int i;

	char typ;

	bool result = FALSE;


	/* Mega-Hack -- Get a monster symbol */
	(void) (get_com("Choose a monster race (by symbol) to genocide: ",
			&typ));

	/* Delete the monsters of that "type" */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx)
			continue;

		/* Hack -- Skip Unique Monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE))
			continue;

		/* Skip "wrong" monsters */
		if (r_ptr->d_char != typ)
			continue;

		/* Delete the monster */
		delete_monster_idx(i);

		/* Take some damage */
		take_hit(randint(4), "the strain of casting Genocide");

		/* Take note */
		result = TRUE;
	}

	return (result);
}


/*
 * Delete all nearby (non-unique) monsters
 */
bool mass_genocide(void)
{
	int i;

	bool result = FALSE;


	/* Delete the (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx)
			continue;

		/* Hack -- Skip unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE))
			continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > MAX_SIGHT)
			continue;

		/* Delete the monster */
		delete_monster_idx(i);

		/* Take some damage */
		take_hit(randint(3), "the strain of casting Mass Genocide");

		/* Note effect */
		result = TRUE;
	}

	return (result);
}



/*
 * Probe nearby monsters
 */
bool probing(void)
{
	int i;

	bool probe = FALSE;


	/* Probe all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx)
			continue;

		/* Require line of sight */
		if (!player_has_los_bold(m_ptr->fy, m_ptr->fx))
			continue;

		/* Probe visible monsters */
		if (m_ptr->ml)
		{
			char m_name[80];

			/* Start the message */
			if (!probe)
				mprint(MSG_TEMP, "Probing...");

			/* Get "the monster" or "something" */
			monster_desc(m_name, m_ptr, 0x04);

			/* Describe the monster */
			msg_format("%^s has %d hit points.", m_name, m_ptr->hp);

			/* Learn all of the non-spell, non-treasure flags */
			lore_do_probe(i);

			/* Probe worked */
			probe = TRUE;
		}
	}

	/* Done */
	if (probe)
	{
		mprint(MSG_TEMP, "That's all.");
	}

	/* Result */
	return (probe);
}



/*
 * The spell of destruction
 *
 */
void destroy_area(int y1, int x1, int r, bool full)
{
	fire_mega_blast(y1, x1, GF_WORD_OF_DESTRUCTION, r, 0);
}



/* Create lots of chaos fog. */

void chaos_destroy_area(int y1, int x1, int r)
{
	fire_mega_blast(y1, x1, GF_CHAOS_DESTRUCTION, r, 0);
}


/*
 * Fire a meteor shower of earthquakes. 
 */
void earthquake(void)
{
	fire_meteor_shower(GF_EARTHQUAKE, damroll(10, 4));
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

	/* Clear them all */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		/* No longer in the array */
		cave_info[y][x] &= ~(CAVE_TEMP);

		/* Update only non-CAVE_GLOW grids */
		/* if (cave_info[y][x] & (CAVE_GLOW)) continue; */

		/* Perma-Lite */
		cave_info[y][x] |= (CAVE_GLOW);

		/* Process affected monsters */
		if (cave_m_idx[y][x] > 0)
		{
			int chance = 25;

			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Update the monster */
			update_mon(cave_m_idx[y][x], FALSE);

			/* Stupid monsters rarely wake up */
			if (r_ptr->flags2 & (RF2_STUPID))
				chance = 10;

			/* Smart monsters always wake up */
			if (r_ptr->flags2 & (RF2_SMART))
				chance = 100;

			/* Sometimes monsters wake up */
			if (m_ptr->csleep && (rand_int(100) < chance))
			{
				/* Wake up! */
				m_ptr->csleep = 0;

				/* Notice the "waking up" */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Acquire the monster name */
					monster_desc(m_name, m_ptr, 0);

					/* Dump a message */
					mformat(MSG_WARNING, "%^s wakes up.", m_name);
				}
			}
		}

		/* Note */
		note_spot(y, x);

		/* Redraw */
		lite_spot(y, x);
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
 *
 * Also, process all affected monsters
 */
static void cave_temp_room_unlite(void)
{
	int i;

	/* Clear them all */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		/* No longer in the array */
		cave_info[y][x] &= ~(CAVE_TEMP);

		/* Darken the grid */
		cave_info[y][x] &= ~(CAVE_GLOW);

		/* Hack -- Forget "boring" grids */
		if (cave_feat[y][x] <= FEAT_INVIS)
		{
			/* Forget the grid */
			cave_info[y][x] &= ~(CAVE_MARK);

			/* Notice */
			note_spot(y, x);
		}

		/* Process affected monsters */
		if (cave_m_idx[y][x] > 0)
		{
			/* Update the monster */
			update_mon(cave_m_idx[y][x], FALSE);
		}

		/* Redraw */
		lite_spot(y, x);
	}

	/* None left */
	temp_n = 0;
}




/*
 * Aux function -- see below
 */
static void cave_temp_room_aux(int y, int x)
{
	/* Avoid infinite recursion */
	if (cave_info[y][x] & (CAVE_TEMP))
		return;

	/* Do not "leave" the current room */
	if (!(cave_info[y][x] & (CAVE_ROOM)))
		return;

	/* Paranoia -- verify space */
	if (temp_n == TEMP_MAX)
		return;

	/* Mark the grid as "seen" */
	cave_info[y][x] |= (CAVE_TEMP);

	/* Add it to the "seen" set */
	temp_y[temp_n] = y;
	temp_x[temp_n] = x;
	temp_n++;
}




/*
 * Illuminate any room containing the given location.
 */
void lite_room(int y1, int x1)
{
	int i, x, y;

	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);

	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get lit, but stop light */
		if (!cave_floor_bold(y, x))
			continue;

		/* Spread adjacent */
		cave_temp_room_aux(y + 1, x);
		cave_temp_room_aux(y - 1, x);
		cave_temp_room_aux(y, x + 1);
		cave_temp_room_aux(y, x - 1);

		/* Spread diagonal */
		cave_temp_room_aux(y + 1, x + 1);
		cave_temp_room_aux(y - 1, x - 1);
		cave_temp_room_aux(y - 1, x + 1);
		cave_temp_room_aux(y + 1, x - 1);
	}

	/* Now, lite them all up at once */
	cave_temp_room_lite();
}


/*
 * Darken all rooms containing the given location
 */
void unlite_room(int y1, int x1)
{
	int i, x, y;

	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);

	/* Spread, breadth first */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get dark, but stop darkness */
		if (!cave_floor_bold(y, x))
			continue;

		/* Spread adjacent */
		cave_temp_room_aux(y + 1, x);
		cave_temp_room_aux(y - 1, x);
		cave_temp_room_aux(y, x + 1);
		cave_temp_room_aux(y, x - 1);

		/* Spread diagonal */
		cave_temp_room_aux(y + 1, x + 1);
		cave_temp_room_aux(y - 1, x - 1);
		cave_temp_room_aux(y - 1, x + 1);
		cave_temp_room_aux(y + 1, x - 1);
	}

	/* Now, darken them all at once */
	cave_temp_room_unlite();
}



/*
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
bool lite_area(int dam, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("You are surrounded by a white light.");
	}

	/* Hook into the "project()" function */
	(void) project(-1, rad, py, px, dam, GF_LITE_WEAK, flg);

	/* Lite up the room */
	lite_room(py, px);

	/* Assume seen */
	return (TRUE);
}


/*
 * Hack -- call darkness around the player
 * Affect all monsters in the projection radius
 */
bool unlite_area(int dam, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("Darkness surrounds you.");
	}

	/* Hook into the "project()" function */
	(void) project(-1, rad, py, px, dam, GF_DARK_WEAK, flg);

	/* Lite up the room */
	unlite_room(py, px);

	/* Assume seen */
	return (TRUE);
}



/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */

/*
 * Hack -- apply a "projection()" in a direction (or at the target)
 */


/*
 * Some of the old functions
 */

bool lite_line(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
	return (project_hook(GF_LITE_WEAK, dir, damroll(6, 8), 0, flg));
}

bool drain_life(int dir, int dam)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_DRAIN, dir, dam, 0, flg));
}

bool wall_to_mud(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(GF_KILL_WALL, dir, 20 + randint(30), 0, flg));
}

bool wall_to_chaos(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return project_hook(GF_WALL_TO_CHAOS, dir, 20 + randint(30), 0, flg);
}

bool destroy_door(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_DOOR, dir, 0, 0, flg));
}

bool disarm_trap(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_TRAP, dir, 0, 0, flg));
}

bool heal_monster(int dir)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_HEAL, dir, damroll(4, 6), 0, flg));
}

bool speed_monster(int dir)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_SPEED, dir, p_ptr->lev, 0, flg));
}

bool slow_monster(int dir)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_SLOW, dir, p_ptr->lev, 0, flg));
}

bool sleep_monster(int dir)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_SLEEP, dir, p_ptr->lev, 0, flg));
}

bool confuse_monster(int dir, int plev)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CONF, dir, plev, 0, flg));
}

bool poly_monster(int dir)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_POLY, dir, p_ptr->lev, 0, flg));
}

bool clone_monster(int dir)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CLONE, dir, 0, 0, flg));
}

bool fear_monster(int dir, int plev)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_TURN_ALL, dir, plev, 0, flg));
}


bool fear_monsters_touch(void)
{
	u32b flg = PROJECT_KILL | PROJECT_STOP;
	return (project(-1, 1, p_ptr->py, p_ptr->px, p_ptr->lev, GF_TURN_ALL,
			flg));
}

bool teleport_monster(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_KILL;
	return (project_hook(GF_AWAY_ALL, dir, MAX_SIGHT * 5, 0, flg));
}



/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */

bool door_creation(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_GRID | PROJECT_ITEM;
	return (project(-1, 1, py, px, 0, GF_MAKE_DOOR, flg));
}

bool trap_creation(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_GRID | PROJECT_ITEM;
	return (project(-1, 1, py, px, 0, GF_MAKE_TRAP, flg));
}

bool destroy_doors_touch(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_GRID | PROJECT_ITEM;
	return (project(-1, 1, py, px, 0, GF_KILL_DOOR, flg));
}

bool sleep_monsters_touch(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_KILL;
	return (project(-1, 1, py, px, p_ptr->lev, GF_SLEEP, flg));
}


void summon_pet_monster(void)
{
	p_ptr->energy_use = 100;

	if (p_ptr->inside_special == SPECIAL_ARENA)
	{
		msg_print("This place seems devoid of life.");
		msg_print(NULL);
		return;
	}

	if (p_ptr->number_pets <= adj_chr_pet_summon[p_ptr->stat_ind[A_CHR]] &&
		summon_specific_friendly(p_ptr->py, p_ptr->px, p_ptr->depth, 0))
	{
		mprint(MSG_BONUS, "You summon some help.");
		p_ptr->number_pets++;
	}
	else
	{
		msg_print("You called, but no help came.");
	}
}


static s16b __fetch_wgt = 0;

static bool item_tester_hook_fetch(object_type * o_ptr)
{
	/* Too heavy to 'fetch' */
	if (o_ptr->weight > __fetch_wgt)
		return FALSE;

	return TRUE;
}

/*
 * Teleport an item to a player.
 *
 * Simply ask for an item using the standard ``get_item'' function, and
 * add it to the inventory.
 */
bool fetch_item(int wgt, int y, int x)
{

	int ty = 0, tx = 0;

	int py = p_ptr->py;
	int px = p_ptr->px;

	object_type *o_ptr;

	/* Use a target */
	if (y < 0 && x < 0)
	{
		if (target_okay() &&
			projectable(py, px, p_ptr->target_row, p_ptr->target_col) &&
			cave_o_idx[p_ptr->target_row][p_ptr->target_col])
		{
			tx = p_ptr->target_col;
			ty = p_ptr->target_row;

			if (distance(py, px, ty, tx) > MAX_RANGE)
			{
				msg_print("You can't fetch something that far away!");
				return FALSE;
			}
		}
		else
		{
			mprint(MSG_TEMP, "No target selected.");
			return FALSE;
		}

		/* Fetch something from a fixed spot. */
	}
	else
	{
		if (projectable(py, px, y, x) && cave_o_idx[y][x] &&
			distance(py, px, y, x) <= MAX_RANGE)
		{
			tx = x;
			ty = y;
		}
		else
		{
			return FALSE;
		}
	}

	/* Paranoia. */
	if (ty == py && tx == px)
	{
		mprint(MSG_TEMP, "You can't fetch something you're standing on!");
		return FALSE;
	}


	/* Mega-hack, pass an argument to hook fn. */
	__fetch_wgt = wgt;

	/* Stay within weight allowance. */
	item_tester_hook = item_tester_hook_fetch;

	o_ptr =
		get_item("Fetch what", "You can't fetch anything there.", ty, tx,
		(USE_INVEN | USE_FLOOR | USE_REMOVE | USE_BY_PARTS));

	if (!o_ptr)
		return FALSE;

	/* Give it to the player. */
	inven_carry(o_ptr);

	p_ptr->redraw |= PR_MAP;
	return TRUE;
}
