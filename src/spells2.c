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
 * Increase player's hit points by the given percentage of maximum, notice effects
 */
bool hp_player(int x, bool percent, bool message)
{
	int points;
	
	if (percent)	points = (p_ptr->mhp * x) / 100;
	else			points = x;
		
	/* Healing needed */
	if ((p_ptr->chp < p_ptr->mhp) && (points > 0))
	{
		/* Gain hitpoints */
		p_ptr->chp += points;

		/* Enforce maximum */
		if (p_ptr->chp >= p_ptr->mhp)
		{
			p_ptr->chp = p_ptr->mhp;
			p_ptr->chp_frac = 0;
		}

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0);

		if (message)
		{
			/* Heal 0-4 */
			if (points < 5)
			{
				msg_print("You feel a little better.");
			}

			/* Heal 5-10 */
			else if (points < 10)
			{
				msg_print("You feel better.");
			}

			/* Heal 10-25 */
			else if (points < 25)
			{
				msg_print("You feel much better.");
			}

			/* Heal 35+ */
			else
			{
				msg_print("You feel very good.");
			}
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
	int py = p_ptr->py;
	int px = p_ptr->px;

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
static cptr desc_stat_pos[] =
{
	"strong",
	"dextrous",
	"healthy",
	"attuned"
};


/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_neg[] =
{
	"weak",
	"awkward",
	"sickly",
	"drained"
};


/*
 * Lose a "point"
 */
bool do_dec_stat(int stat, monster_type *m_ptr)
{
	bool resistance = FALSE;
	
	/* Get the "sustain" */
	switch (stat)
	{
		case A_STR: if (p_ptr->sustain_str || p_ptr->tmp_str) resistance = TRUE; break;
		case A_DEX: if (p_ptr->sustain_dex || p_ptr->tmp_dex) resistance = TRUE; break;
		case A_CON: if (p_ptr->sustain_con || p_ptr->tmp_con) resistance = TRUE; break;
		case A_GRA: if (p_ptr->sustain_gra || p_ptr->tmp_gra) resistance = TRUE; break;
	}

	/* Saving throw */
	if (saving_throw(m_ptr, resistance))
	{
		/* Message */
		msg_format("You feel %s for a moment, but it passes.",
		           desc_stat_neg[stat]);

		// possibly identify relevant items
		switch (stat)
		{
			case A_STR: ident_resist(TR2_SUST_STR); break;
			case A_DEX: ident_resist(TR2_SUST_DEX); break;
			case A_CON: ident_resist(TR2_SUST_CON); break;
			case A_GRA: ident_resist(TR2_SUST_GRA); break;
		}

		/* Notice effect */
		return (TRUE);
	}
	
	/* Attempt to reduce the stat */
	if (dec_stat(stat, 1, FALSE))
	{
		/* Message */
		msg_format("You feel %s.", desc_stat_neg[stat]);

		/* Notice effect */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}


/*
 * Restore lost "points" in a stat
 */
bool do_res_stat(int stat, int points)
{
	/* Attempt to increase */
	if (res_stat(stat, points))
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
bool do_inc_stat(int stat)
{
	bool res;

	/* Restore stat */
	res = res_stat(stat, 20);

	/* Attempt to increase */
	if (inc_stat(stat))
	{
		/* Message */
		msg_format("You feel %s!", desc_stat_pos[stat]);

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
 */
void identify_pack(void)
{
	int i;

	/* Simply identify and know every item */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0);
}



#define ENCHANT_MAX 15


/*
 * Used by the "enchant" function (chance of failure)
 */
static const int enchant_table[ENCHANT_MAX+1] =
{
	0, 10,  50, 100, 200,
	300, 400, 500, 700, 950,
	990, 992, 995, 997, 999,
	1000
};


/*
 * Hack -- Removes curse from an object.
 */
void uncurse_object(object_type *o_ptr)
{
	/* Uncurse it */
	o_ptr->ident &= ~(IDENT_CURSED);

	/* Remove special inscription, if any */
	if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

	/* Take note if allowed */
	if (o_ptr->discount == 0) o_ptr->discount = INSCRIP_UNCURSED;

	/* The object has been "sensed" */
	o_ptr->ident |= (IDENT_SENSE);
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
static int remove_curse_aux(bool star_curse)
{
	int i, cnt = 0;

	/* Attempt to uncurse items being worn */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		u32b f1, f2, f3;

		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Uncursed already */
		if (!cursed_p(o_ptr)) continue;

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Heavily Cursed Items need a special spell */
		if (!star_curse && (f3 & (TR3_HEAVY_CURSE))) continue;

		/* Perma-Cursed Items can NEVER be uncursed */
		if (f3 & (TR3_PERMA_CURSE)) continue;

		/* Uncurse the object */
		uncurse_object(o_ptr);

		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);

		/* Count the uncursings */
		cnt++;
	}

	/* Return "something uncursed" */
	return (cnt);
}


/*
 * Remove most curses
 */
bool remove_curse(bool star_curse)
{
	return (remove_curse_aux(star_curse));
}


/*
 * Hack -- acquire self knowledge
 *
 * List various information about the player and/or his current equipment.
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
	int abilities = 0;
	
	u32b f1 = 0L, f2 = 0L, f3 = 0L;

	object_type *o_ptr;

	char buf[10][80];
	cptr info[128];
	char tmp1[80];
	char tmp2[80];
		
	int resistance;


	/* Get item flags from equipment */
	for (k = INVEN_WIELD; k < INVEN_TOTAL; k++)
	{
		u32b t1, t2, t3;

		o_ptr = &inventory[k];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the flags */
		object_flags(o_ptr, &t1, &t2, &t3);

		/* Extract flags */
		f1 |= t1;
		f2 |= t2;
		f3 |= t3;
	}


	if (p_ptr->pspeed < 2)
	{
		info[i++] = "You are moving slowly.";
	}
	if (p_ptr->pspeed > 2)
	{
		info[i++] = "You are moving quickly.";
	}
	
	// This is a hack, but I couldn't find a neater way...
	switch (p_ptr->hunger)
	{
		case -5: info[i++] = "You grow hungry at only 1/243 the normal rate."; break;
		case -4: info[i++] = "You grow hungry at only 1/81 the normal rate."; break;
		case -3: info[i++] = "You grow hungry at only 1/27 the normal rate."; break;
		case -2: info[i++] = "You grow hungry at only 1/9 the normal rate."; break;
		case -1: info[i++] = "You grow hungry at only 1/3 the normal rate."; break;
		// case 0:
		case  1: info[i++] = "You burn with an unnatural hunger (3 times the normal rate)."; break;
		case  2: info[i++] = "You burn with an unnatural hunger (9 times the normal rate)."; break;
		case  3: info[i++] = "You burn with an unnatural hunger (27 times the normal rate)."; break;
		case  4: info[i++] = "You burn with an unnatural hunger (81 times the normal rate)."; break;
		case  5: info[i++] = "You burn with an unnatural hunger (243 times the normal rate)."; break;
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

	if (p_ptr->cowardice > 0)
	{
		info[i++] = "You occasionally become terrified in combat.";
	}

	if (p_ptr->haunted > 0)
	{
		info[i++] = "You are haunted by wraiths.";
	}
	
	// This is a hack, but I couldn't find a neater way...
	if (p_ptr->danger)
	{
		sprintf(tmp1, "You encounter more dangerous creatures (+%d ft).", p_ptr->danger * 50);
		info[i++] = tmp1;
	}
	if (p_ptr->aggravate)
	{		
		if (p_ptr->aggravate == 1)
		{
			info[i++] = "You enrage nearby creatures.";
		}
		else
		{
			sprintf(tmp2, "You enrage nearby creatures (x%d).", p_ptr->aggravate);
			info[i++] = tmp2;
		}
	}
	if (p_ptr->rage)
	{
		info[i++] = "You are in a dark rage.";
	}
	if (p_ptr->tmp_str)
	{
		info[i++] = "You feel stronger.";
	}
	if (p_ptr->tmp_dex)
	{
		info[i++] = "You feel more agile.";
	}
	if (p_ptr->tmp_con)
	{
		info[i++] = "You feel more resilient.";
	}
	if (p_ptr->tmp_gra)
	{
		info[i++] = "You feel more attuned to the world.";
	}
	if (p_ptr->stealth_mode)
	{
		info[i++] = "You are creeping around very carefully.";
	}
	if (p_ptr->word_recall)
	{
		info[i++] = "You will soon be recalled.";
	}
	if (p_ptr->regenerate)
	{
		info[i++] = "You regenerate quickly.";
	}
	if (p_ptr->telepathy)
	{
		info[i++] = "You have telepathy.";
	}
	if (p_ptr->see_inv)
	{
		info[i++] = "You can see invisible creatures.";
	}
	if (p_ptr->free_act)
	{
		info[i++] = "Your movement is rarely hindered.";
	}

	resistance = resist_cold();
	if (resistance <= -4)
	{
		info[i++] = "You are exceptionally vulnerable to cold.";
	}
	else if (resistance == -3)
	{
		info[i++] = "You are very vulnerable to cold.";
	}
	else if (resistance == -2)
	{
		info[i++] = "You are vulnerable to cold.";
	}
	
	else if (resistance == 2)
	{
		info[i++] = "You are resistant to cold.";
	}
	else if (resistance == 3)
	{
		info[i++] = "You are very resistant to cold.";
	}
	else if (resistance >= 4)
	{
		info[i++] = "You are exceptionally resistant to cold.";
	}

	resistance = resist_fire();
	if (resistance <= -4)
	{
		info[i++] = "You are exceptionally vulnerable to fire.";
	}
	else if (resistance == -3)
	{
		info[i++] = "You are very vulnerable to fire.";
	}
	else if (resistance == -2)
	{
		info[i++] = "You are vulnerable to fire.";
	}
	
	else if (resistance == 2)
	{
		info[i++] = "You are resistant to fire.";
	}
	else if (resistance == 3)
	{
		info[i++] = "You are very resistant to fire.";
	}
	else if (resistance >= 4)
	{
		info[i++] = "You are exceptionally resistant to fire.";
	}

	resistance = resist_pois();
	if (resistance <= -4)
	{
		info[i++] = "You are exceptionally vulnerable to poison.";
	}
	else if (resistance == -3)
	{
		info[i++] = "You are very vulnerable to poison.";
	}
	else if (resistance == -2)
	{
		info[i++] = "You are vulnerable to poison.";
	}
	
	else if (resistance == 2)
	{
		info[i++] = "You are resistant to poison.";
	}
	else if (resistance == 3)
	{
		info[i++] = "You are very resistant to poison.";
	}
	else if (resistance >= 4)
	{
		info[i++] = "You are exceptionally resistant to poison.";
	}
	
	if (p_ptr->resist_fear)
	{
		info[i++] = "You are fearless.";
	}
	if (p_ptr->resist_blind)
	{
		info[i++] = "Your eyes are resistant to blindness.";
	}
	if (p_ptr->resist_confu)
	{
		info[i++] = "You are resistant to confusion attacks.";
	}
	if (p_ptr->resist_stun)
	{
		info[i++] = "You are resistant to stunning blows.";
	}
	if (p_ptr->resist_hallu)
	{
		info[i++] = "You are resistant to hallucination.";
	}
	
	if (p_ptr->sustain_str)
	{
		info[i++] = "Your strength is sustained.";
	}
	if (p_ptr->sustain_con)
	{
		info[i++] = "Your constitution is sustained.";
	}
	if (p_ptr->sustain_dex)
	{
		info[i++] = "Your dexterity is sustained.";
	}
	if (p_ptr->sustain_gra)
	{
		info[i++] = "Your grace is sustained.";
	}

	if ((f1 & (TR1_STR)) || (f1 & (TR1_NEG_STR)))
	{
		info[i++] = "Your strength is affected by your equipment.";
	}
	if ((f1 & (TR1_DEX)) || (f1 & (TR1_NEG_DEX)))
	{
		info[i++] = "Your dexterity is affected by your equipment.";
	}
	if ((f1 & (TR1_CON)) || (f1 & (TR1_NEG_CON)))
	{
		info[i++] = "Your constitution is affected by your equipment.";
	}
	if ((f1 & (TR1_GRA)) || (f1 & (TR1_NEG_GRA)))
	{
		info[i++] = "Your grace is affected by your equipment.";
	}
	if (f1 & (TR1_DAMAGE_SIDES))
	{
		info[i++] = "Your melee/archery damage sides are specially affected by your equipment.";
	}
	if (f1 & (TR1_MEL))
	{
		info[i++] = "Your melee ability is specially affected by your equipment.";
	}
	if (f1 & (TR1_ARC))
	{
		info[i++] = "Your archery ability is specially affected by your equipment.";
	}
	if (f1 & (TR1_STL))
	{
		info[i++] = "Your stealth is affected by your equipment.";
	}
	if (f1 & (TR1_PER))
	{
		info[i++] = "Your perception is affected by your equipment.";
	}
	if (f1 & (TR1_WIL))
	{
		info[i++] = "Your will is affected by your equipment.";
	}
	if (f1 & (TR1_SMT))
	{
		info[i++] = "Your smithing is affected by your equipment.";
	}
	if (f1 & (TR1_SNG))
	{
		info[i++] = "Your singing is affected by your equipment.";
	}
	if (f1 & (TR1_TUNNEL))
	{
		info[i++] = "Your digging ability is affected by your equipment.";
	}
	if (f2 & (TR2_LIGHT))
	{
		info[i++] = "Some of your equipment glows with an inner light.";
	}
	if (f2 & (TR2_RADIANCE))
	{
		info[i++] = "Your arrows illuminate the dungeon.";
	}
	if (f2 & (TR2_DARKNESS))
	{
		info[i++] = "Some of your equipment radiates an unnatural darkness.";
	}

	// *******************************************
	// Do some analysis just on the wielded weapon

	/* Get the current weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Analyze the weapon */
	if (o_ptr->k_idx)
	{
		// get the flags just for the wielded weapon
		object_flags(o_ptr, &f1, &f2, &f3);
		
		/* Special "Attack Bonuses" */
		if (f1 & (TR1_SHARPNESS))
		{
			info[i++] = "Your weapon cuts easily through armour.";
		}
		if (f1 & (TR1_SHARPNESS2))
		{
			info[i++] = "Your weapon cuts exceptionally easily through armour.";
		}
		if (f1 & (TR1_VAMPIRIC))
		{
			info[i++] = "Your weapon drains life from your enemies.";
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
		if (f1 & (TR1_BRAND_POIS))
		{
			info[i++] = "Your weapon poisons your foes.";
		}

		/* Special "slay" flags */
		if (f1 & (TR1_SLAY_WOLF))
		{
			info[i++] = "Your weapon is especially deadly against wolves.";
		}
		if (f1 & (TR1_SLAY_SPIDER))
		{
			info[i++] = "Your weapon is especially deadly against spiders.";
		}
		if (f1 & (TR1_SLAY_UNDEAD))
		{
			info[i++] = "Your weapon is especially effective against the undead.";
		}
		if (f1 & (TR1_SLAY_RAUKO))
		{
			info[i++] = "Your weapon is especially deadly against raukar.";
		}
		if (f1 & (TR1_SLAY_ORC))
		{
			info[i++] = "Your weapon is especially deadly against orcs.";
		}
		if (f1 & (TR1_SLAY_TROLL))
		{
			info[i++] = "Your weapon is especially deadly against trolls.";
		}
		if (f1 & (TR1_SLAY_DRAGON))
		{
			info[i++] = "Your weapon is especially deadly against dragons.";
		}
	}

	// *******************************************
	// Do some analysis just on the off-hand weapon (if any)
	
	/* Get the current weapon */
	o_ptr = &inventory[INVEN_ARM];
	
	/* Analyze the weapon */
	if (o_ptr->k_idx && (p_ptr->mds2 > 0))
	{
		// get the flags just for the wielded weapon
		object_flags(o_ptr, &f1, &f2, &f3);
		
		/* Special "Attack Bonuses" */
		if (f1 & (TR1_SHARPNESS))
		{
			info[i++] = "Your off-hand weapon cuts easily through armour.";
		}
		if (f1 & (TR1_SHARPNESS2))
		{
			info[i++] = "Your off-hand weapon cuts exceptionally easily through armour.";
		}
		if (f1 & (TR1_VAMPIRIC))
		{
			info[i++] = "Your off-hand weapon drains life from your enemies.";
		}
		
		if (f1 & (TR1_BRAND_ELEC))
		{
			info[i++] = "Your off-hand weapon shocks your foes.";
		}
		if (f1 & (TR1_BRAND_FIRE))
		{
			info[i++] = "Your off-hand weapon burns your foes.";
		}
		if (f1 & (TR1_BRAND_COLD))
		{
			info[i++] = "Your off-hand weapon freezes your foes.";
		}
		if (f1 & (TR1_BRAND_POIS))
		{
			info[i++] = "Your off-hand weapon poisons your foes.";
		}
		
		/* Special "slay" flags */
		if (f1 & (TR1_SLAY_WOLF))
		{
			info[i++] = "Your off-hand weapon is especially deadly against wolves.";
		}
		if (f1 & (TR1_SLAY_SPIDER))
		{
			info[i++] = "Your off-hand weapon is especially deadly against spiders.";
		}
		if (f1 & (TR1_SLAY_UNDEAD))
		{
			info[i++] = "Your off-hand weapon is especially effective against the undead.";
		}
		if (f1 & (TR1_SLAY_RAUKO))
		{
			info[i++] = "Your off-hand weapon is especially deadly against raukar.";
		}
		if (f1 & (TR1_SLAY_ORC))
		{
			info[i++] = "Your off-hand weapon is especially deadly against orcs.";
		}
		if (f1 & (TR1_SLAY_TROLL))
		{
			info[i++] = "Your off-hand weapon is especially deadly against trolls.";
		}
		if (f1 & (TR1_SLAY_DRAGON))
		{
			info[i++] = "Your off-hand weapon is especially deadly against dragons.";
		}
	}
	
	// *******************************************
	// Do some analysis just on the wielded bow

	/* Get the current bow */
	o_ptr = &inventory[INVEN_BOW];
	
	/* Analyze the weapon */
	if (o_ptr->k_idx)
	{
		// get the flags just for the wielded weapon
		object_flags(o_ptr, &f1, &f2, &f3);
		
		/* Special "Attack Bonuses" */		
		if (f1 & (TR1_BRAND_ELEC))
		{
			info[i++] = "Your bow shocks your foes.";
		}
		if (f1 & (TR1_BRAND_FIRE))
		{
			info[i++] = "Your bow burns your foes.";
		}
		if (f1 & (TR1_BRAND_COLD))
		{
			info[i++] = "Your bow freezes your foes.";
		}
		if (f1 & (TR1_BRAND_POIS))
		{
			info[i++] = "Your bow poisons your foes.";
		}
		
		/* Special "slay" flags */
		if (f1 & (TR1_SLAY_WOLF))
		{
			info[i++] = "Your bow is especially deadly against wolves.";
		}
		if (f1 & (TR1_SLAY_SPIDER))
		{
			info[i++] = "Your bow is especially deadly against spiders.";
		}
		if (f1 & (TR1_SLAY_UNDEAD))
		{
			info[i++] = "Your bow is especially effective against the undead.";
		}
		if (f1 & (TR1_SLAY_RAUKO))
		{
			info[i++] = "Your bow is especially deadly against raukar.";
		}
		if (f1 & (TR1_SLAY_ORC))
		{
			info[i++] = "Your bow is especially deadly against orcs.";
		}
		if (f1 & (TR1_SLAY_TROLL))
		{
			info[i++] = "Your bow is especially deadly against trolls.";
		}
		if (f1 & (TR1_SLAY_DRAGON))
		{
			info[i++] = "Your bow is especially deadly against dragons.";
		}
	}
	
	// *******************************************
	// Back to generic analysis
	
	/* Read the abilities info */
	for (j = 0; j < S_MAX; j++)
	{
		for (k = 0; k < ABILITIES_MAX; k++)
		{
			if (p_ptr->have_ability[j][k] && !p_ptr->innate_ability[j][k])
			{
				strnfmt(buf[abilities], 80, "You have been granted the ability: %s.", b_name + (&b_info[ability_index(j, k)])->name);
				info[i++] = buf[abilities];
				abilities++;
			}
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
		if ((k == 22) && (j+1 < i))
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
	prt("(press any key)", k, 0);
	(void)inkey();


	/* Load screen */
	screen_load();
}



/*
 * Detect all doors and traps on the whole level.
 * Used to show map at end of the game.
 */
void detect_all_doors_traps()
{
	int y, x;
	
	bool detect_trap = FALSE;
	bool detect_door = FALSE;

	/* Scan the visible area */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			if (!in_bounds_fully(y, x)) continue;
			
			/* Detect invisible traps */
			if (cave_trap_bold(y, x) && (cave_info[y][x] & (CAVE_HIDDEN)))
			{
				/* Reveal the trap */
				reveal_trap(y, x);
				
				detect_trap = TRUE;
			}

			/* Detect secret doors */
			if (cave_feat[y][x] == FEAT_SECRET)
			{
				/* Pick a door */
				place_closed_door(y, x);

				/* Hack -- Memorize */
				cave_info[y][x] |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				detect_door = TRUE;
			}
		}
	}
	
}


/*
 * Detect all traps in sight
 */
bool detect_traps(void)
{
	int y, x;

	bool detect = FALSE;


	/* Scan the visible area */
	for (y = p_ptr->py - MAX_SIGHT; y < p_ptr->py + MAX_SIGHT; y++)
	{
		for (x = p_ptr->px - MAX_SIGHT; x < p_ptr->px + MAX_SIGHT; x++)
		{
			if (!in_bounds_fully(y, x)) continue;
			
			if (!player_can_see_bold(y, x)) continue;

			/* Detect invisible traps */
			if (cave_trap_bold(y, x) && (cave_info[y][x] & (CAVE_HIDDEN)))
			{
				/* Reveal the trap */
				reveal_trap(y, x);

				/* Obvious */
				detect = TRUE;
			}

		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of traps!");
	}

	/* Result */
	return (detect);
}



/*
 * Detect all doors in sight
 */
bool detect_doors(void)
{
	int y, x;

	bool detect = FALSE;

	/* Scan the visible area */
	for (y = p_ptr->py - MAX_SIGHT; y < p_ptr->py + MAX_SIGHT; y++)
	{
		for (x = p_ptr->px - MAX_SIGHT; x < p_ptr->px + MAX_SIGHT; x++)
		{
			if (!in_bounds_fully(y, x)) continue;
			
			if (!player_can_see_bold(y, x)) continue;

			/* Detect secret doors */
			if (cave_feat[y][x] == FEAT_SECRET)
			{
				/* Pick a door */
				place_closed_door(y, x);

				/* Hack -- Memorize */
				cave_info[y][x] |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				detect = TRUE;
			}
		}
	}

	/* Result */
	return (detect);
}


/*
 * Detect all stairs within 20 squares
 */
bool detect_stairs(void)
{
	int y, x;

	bool detect = FALSE;


	/* Scan the visible area */
	for (y = p_ptr->py - 20; y < p_ptr->py + 20; y++)
	{
		for (x = p_ptr->px - 20; x < p_ptr->px + 20; x++)
		{
			if (!in_bounds_fully(y, x)) continue;
			
			/* Detect stairs */
			if (cave_stair_bold (y,x))
			{
				/* Hack -- Memorize */
				cave_info[y][x] |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Obvious */
				detect = TRUE;
			}
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of stairs!");
	}

	/* Result */
	return (detect);
}


/*
 * Detect all "normal" objects on the current panel
 */
bool detect_objects_normal(void)
{
	int i, y, x;

	bool detect = FALSE;


	/* Scan objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		// Skip staffs of treasures (cute, and helps prevent run-away detection spiral)
		if ((o_ptr->tval == TV_STAFF) && (o_ptr->sval == SV_STAFF_TREASURES)) continue;
		
		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		//if (!panel_contains(y, x)) continue;
		
		/* Hack -- memorize it */
		o_ptr->marked = TRUE;

		/* Redraw */
		lite_spot(y, x);

		/* Detect */
		detect = TRUE;
	}

	/* Scan monsters, looking for mimics */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/*we want to detect mimics of objects,
		 *note I did not include all possible magic items
		 */
		if (!(strchr("|!?-_=~", r_ptr->d_char))) continue;

		/* XXX XXX - Mimics aren't detected */
		if (!m_ptr->mimic_k_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/*mark them as a mimic*/
		m_ptr->mflag |= (MFLAG_MIMIC | MFLAG_MARK | MFLAG_SHOW);

		/* Optimize -- Repair flags */
		repair_mflag_mark = TRUE;
		repair_mflag_show = TRUE;

		/* Update the monster */
		update_mon(i, FALSE);

		/* Detect */
		detect = TRUE;

	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of objects!");
	}

	/* Result */
	return (detect);
}


/*
 * Detect all "magic" objects on the current panel.
 *
 * This will light up all spaces with "magic" items, including artefacts,
 * special items, potions, staves, amulets, rings.
 */
bool detect_objects_magic(void)
{
	int i, y, x, tv;

	bool detect = FALSE;

	/* Scan all objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		if (!panel_contains(y, x)) continue;

		/* Examine the tval */
		tv = o_ptr->tval;

		/* Artefacts, misc magic items, or ego items */
		if (artefact_p(o_ptr) || ego_item_p(o_ptr) ||
		    (tv == TV_AMULET) || (tv == TV_RING) ||
		    (tv == TV_STAFF) || (tv == TV_TRUMPET) ||
		    (tv == TV_POTION) ||
		    o_ptr->name2)
		{
			/* Memorize the item */
			o_ptr->marked = TRUE;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			detect = TRUE;
		}
	}

	/* Scan monsters, looking for mimics */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/*
		 *we want to detect magical appearing mimics,
		 *note I did not include all possible magic items
		 */
		if (!(strchr("!?-_=|", r_ptr->d_char))) continue;

		/* XXX XXX - Mimics aren't detected */
		if (!m_ptr->mimic_k_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/*mark them as a mimic*/
		m_ptr->mflag |= (MFLAG_MIMIC | MFLAG_MARK | MFLAG_SHOW);

		/* Optimize -- Repair flags */
		repair_mflag_mark = TRUE;
		repair_mflag_show = TRUE;

		/* Update the monster */
		update_mon(i, FALSE);

		/* Detect */
		detect = TRUE;

	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of enchantments!");
	}

	/* Return result */
	return (detect);
}


/*
 * Detect all "normal" monsters on the current panel
 */
bool detect_monsters(void)
{
	int i, y, x;

	bool flag = FALSE;

	/* Scan monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* XXX XXX - Unknown Mimics stay hidden */
		if (m_ptr->mimic_k_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Optimize -- Repair flags */
		repair_mflag_mark = TRUE;
		repair_mflag_show = TRUE;

		/* Hack -- Detect the monster */
		m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

		/* Update the monster */
		update_mon(i, FALSE);

		/* Detect */
		flag = TRUE;
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
		msg_print("You sense the presence of your enemies!");
	}

	/* Result */
	return (flag);
}


/*
 * Detect all "invisible" monsters in sight
 */
bool detect_monsters_invis(void)
{
	int i, y, x;

	bool flag = FALSE;

	/* Scan monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only monsters in sight range */
		//if (!player_can_see_bold(y, x)) continue;

		/* Detect invisible monsters */
		if (r_ptr->flags2 & (RF2_INVISIBLE))
		{
			/* XXX XXX - Mimics stay hidden */
			if (m_ptr->mimic_k_idx) continue;

			/* Take note that they are invisible */
			l_ptr->flags2 |= (RF2_INVISIBLE);

			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Optimize -- Repair flags */
			repair_mflag_mark = TRUE;
			repair_mflag_show = TRUE;

			/* Hack -- Detect the monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(i, FALSE);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
		msg_print("You sense the presence of invisible creatures!");
	}

	/* Result */
	return (flag);
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
	if (detect_objects_normal()) detect = TRUE;
	if (detect_monsters_invis()) detect = TRUE;
	if (detect_monsters()) detect = TRUE;

	/* Result */
	return (detect);
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
		msg_print("The object resists the spell.");
		return;
	}

	/* XXX XXX XXX */
	delete_object(py, px);

	place_random_stairs(py, px);
}

/*
 * Hook to specify "weapon"
 */
bool item_tester_hook_wieldable_ided_weapon(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		{
			if (object_known_p(o_ptr)) return (TRUE);
			else return (FALSE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "weapon"
 */
bool item_tester_hook_wieldable_weapon(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "weapon"
 */
bool item_tester_hook_weapon(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_BOW:
		case TV_ARROW:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "weapon"
 */
bool item_tester_hook_ided_weapon(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_BOW:
		case TV_ARROW:
		{
			if (object_known_p(o_ptr)) return (TRUE);
			else return (FALSE);
		}
	}

	return (FALSE);
}



/*
 * Hook to specify "armour"
 */
bool item_tester_hook_ided_armour(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_MAIL:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			if (object_known_p(o_ptr)) return (TRUE);
			else return (FALSE);
		}
	}

	return (FALSE);
}



/*
 * Hook to specify "armour"
 */
bool item_tester_hook_armour(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_MAIL:
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
 * Hook to specify "armour that can be elvenkind"
 */
bool item_tester_hook_elvenkindable_armour(const object_type *o_ptr)
{
	if ((o_ptr->tval == TV_SHIELD) || (o_ptr->tval == TV_MAIL) ||
		(o_ptr->tval == TV_SOFT_ARMOR) || (o_ptr->tval == TV_BOOTS)) return (TRUE);

	return (FALSE);
}

/*
 * Hook to specify "enchantable ring"
 */
bool item_tester_hook_enchantable_ring(const object_type *o_ptr)
{
	if ((o_ptr->tval == TV_RING) && (o_ptr->pval > 0)) return (TRUE);

	return (FALSE);
}

/*
 * Hook to specify "enchantable amulet"
 */
bool item_tester_hook_enchantable_amulet(const object_type *o_ptr)
{
	if ((o_ptr->tval == TV_AMULET) && (o_ptr->pval > 0)) return (TRUE);

	return (FALSE);
}


static bool item_tester_unknown(const object_type *o_ptr)
{
	if (object_known_p(o_ptr))
		return FALSE;
	else
		return TRUE;
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

	cptr q, s;

	// Sil-y: assignments to soothe compilation warnings
	num_hit = num_hit;
	num_dam = num_dam;

	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Enchant armor if requested */
	if (num_ac) item_tester_hook = item_tester_hook_armour;

	/* Get an item */
	q = "Enchant which item? ";
	s = "You have nothing to enchant.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

	/* Describe */
	msg_format("%s %s glow%s brightly!",
	           ((item >= 0) ? "Your" : "The"), o_name,
	           ((o_ptr->number > 1) ? "" : "s"));

	/* Enchant */
	/* Sil-y: currently no enchantment occurs */

	/* Failure */
	if (!okay)
	{
		/* Flush */
		flush();

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

	int squelch;

	object_type *o_ptr;

	cptr q, s;

	/* Only un-id'ed items */
	item_tester_hook = item_tester_unknown;

	/* Get an item */
	q = "Identify which item? ";
	s = "You have nothing to identify.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Identify the object and get squelch setting */
	squelch = do_ident_item(item, o_ptr);

	/* Now squelch it if needed */
	do_squelch_item(squelch, item, o_ptr);

	/* Something happened */
	return (TRUE);
}


/*
 * Hook for "get_item()".  Determine if something is rechargable.
 */
bool item_tester_hook_recharge(const object_type *o_ptr)
{
	/* Recharge staffs */
	if (o_ptr->tval == TV_STAFF) return (TRUE);

	/* Nope */
	return (FALSE);
}



/*
 * Recharge a staff from the pack or on the floor.
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
 * BEFORE all staves/wands/rods in the inventory.  Note that the
 * new "auto_sort_pack" option would correctly handle replacing
 * the "broken" wand with any other item (i.e. a broken stick).
 *
 */
bool recharge(int num)
{
	int item;

	object_type *o_ptr;

	cptr q, s;

	/* Only accept legal items, which are wands and staffs */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item */
	q = "Recharge which staff? ";
	s = "You have nothing to recharge.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0) o_ptr = &inventory[item];

	/* Get the item (on the floor) */
	else o_ptr = &o_list[0 - item];

	/* Attempt to Recharge a staff, or handle failure to recharge . */
	if (o_ptr->tval == TV_STAFF)
	{
		/* Recharge the staff. */
		o_ptr->pval += num;
				
		if (object_aware_p(o_ptr) && (o_ptr->ident & (IDENT_EMPTY)))
		{
			object_aware(o_ptr);
			object_known(o_ptr);
		}
		
		/* Hack -- we no longer think the item is empty */
		o_ptr->ident &= ~(IDENT_EMPTY);
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);

	/* Something was done */
	return (TRUE);
}

/************************************************************************
 *                                                                      *
 *                           Projection types                           *
 *                                                                      *
 ************************************************************************/


/*
 * Handle bolt spells.
 *
 * Bolts stop as soon as they hit a monster, whiz past missed targets, and
 * (almost) never affect items on the floor.
 */
bool project_bolt(int who, int rad, int y0, int x0, int y1, int x1, int dd, int ds, int dif,
                  int typ, u32b flg)
{
	/* Add the bolt bitflags */
	flg |= PROJECT_STOP | PROJECT_KILL | PROJECT_THRU;

	/* Hurt the character unless he controls the spell */
	if (who != -1) flg |= PROJECT_PLAY;

	/* Limit range */
	if ((rad > MAX_RANGE) || (rad <= 0)) rad = MAX_RANGE;

	/* Cast a bolt */
	return (project(who, rad, y0, x0, y1, x1, dd, ds, dif, typ, flg, 0, FALSE));
}

/*
 * Handle beam spells.
 *
 * Beams affect every grid they touch, go right through monsters, and
 * (almost) never affect items on the floor.
 */
bool project_beam(int who, int rad, int y0, int x0, int y1, int x1, int dd, int ds, int dif,
                  int typ, u32b flg)
{
	/* Add the beam bitflags */
	flg |= PROJECT_BEAM | PROJECT_KILL | PROJECT_THRU;

	/* Hurt the character unless he controls the spell */
	if (who != -1) flg |= (PROJECT_PLAY);

	/* Limit range */
	if ((rad > MAX_RANGE) || (rad <= 0)) rad = MAX_RANGE;

	/* Cast a beam */
	return (project(who, rad, y0, x0, y1, x1, dd, ds, dif, typ, flg, 0, FALSE));
}


/*
 * Handle ball spells.
 *
 * Balls act like bolt spells, except that they do not pass their target,
 * and explode when they hit a monster, a wall, their target, or the edge
 * of sight.  Within the explosion radius, they affect items on the floor.
 *
 * Balls may jump to the target, and have any source diameter (which affects
 * how quickly their damage falls off with distance from the center of the
 * explosion).
 */
bool project_ball(int who, int rad, int y0, int x0, int y1, int x1, int dd, int ds, int dif,
                  int typ, u32b flg, bool uniform)
{
	/* Add the ball bitflags */
	flg |= PROJECT_BOOM | PROJECT_GRID |
	       PROJECT_ITEM | PROJECT_KILL;

	/* Add the STOP flag if appropriate */
	if ((who < 0) &&
	    (!target_okay() || y1 != p_ptr->target_row || x1 != p_ptr->target_col))
	{
		flg |= (PROJECT_STOP);
	}

	/* Hurt the character unless he controls the spell */
	if (who != -1) flg |= (PROJECT_PLAY);

	/* Limit radius to nine (up to 256 grids affected) */
	if (rad > 9) rad = 9;

	/* Cast a ball */
	return (project(who, rad, y0, x0, y1, x1, dd, ds, dif, typ, flg,
	                0, uniform));
}

/*
 * Handle ball spells that explode immediately on the target and
 * hurt everything.
 */
bool explosion(int who, int rad, int y0, int x0, int dd, int ds, int dif, int typ)
{
	/* Add the explosion bitflags */
	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_JUMP |
	           PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;

	/* Explode */
	return (project_ball(who, rad, y0, x0, y0, x0, dd, ds, dif, typ, flg, FALSE));
}


/*
 * Handle arc spells.
 *
 * Arcs are a pie-shaped segment (with a width determined by "degrees")
 * of a explosion outwards from the source grid.  They are centered
 * along a line extending from the source towards the target.  -LM-
 *
 * Because all arcs start out as being one grid wide, arc spells with a
 * value for degrees of arc less than (roughly) 60 do not dissipate as
 * quickly.  In the extreme case where degrees of arc is 0, the arc is
 * actually a defined length beam, and loses no strength at all over the
 * ranges found in the game.
 *
 * Arcs affect items on the floor.
 */
bool project_arc(int who, int rad, int y0, int x0, int y1, int x1, int dd, int ds, int dif,
                 int typ, u32b flg, int degrees)
{

	/* Radius of zero means no fixed limit. */
	if (rad == 0) rad = MAX_SIGHT;

	/* If the arc has no spread, it's actually a beam */
	if (degrees <= 0)
	{
		/* Add the beam bitflags */
		flg |= (PROJECT_BEAM | PROJECT_KILL);
	}

	/* If a full circle is asked for, we cast a ball spell. */
	else if (degrees >= 360)
	{
		/* Add the ball bitflags */
		flg |= PROJECT_STOP | PROJECT_BOOM | PROJECT_GRID |
		       PROJECT_ITEM | PROJECT_KILL;
	}

	/* Otherwise, we fire an arc */
	else
	{
		/* Add the arc bitflags */
		flg |= PROJECT_ARC  | PROJECT_BOOM | PROJECT_GRID |
		       PROJECT_ITEM | PROJECT_KILL;
	}

	/* Hurt the character unless he controls the spell */
	if (who != -1) flg |= (PROJECT_PLAY);

	/* Cast an arc (or a ball) */
	return (project(who, rad, y0, x0, y1, x1, dd, ds, dif, typ, flg, degrees, FALSE));
}


/*
 * Handle target grids for projections under the control of
 * the character.  - Chris Wilde, Morgul
 */
static void adjust_target(int dir, int y0, int x0, int *y1, int *x1)
{
	/* If no direction is given, and a target is, use the target. */
	if ((dir == 5) && target_okay())
	{
		*y1 = p_ptr->target_row;
		*x1 = p_ptr->target_col;
	}
	else if ((dir == DIRECTION_UP) || (dir == DIRECTION_DOWN))
	{
		*y1 = y0;
		*x1 = x0;
	}
	
	/* Otherwise, use the given direction */
	else
	{
		*y1 = y0 + MAX_RANGE * ddy[dir];
		*x1 = x0 + MAX_RANGE * ddx[dir];
	}
}

/*
 * Apply a "project()" directly to all monsters in view of a certain spot.
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 *
 * This function is not optimized for efficieny.  It should only be used
 * in non-bottleneck functions such as spells. It should not be used in functions
 * that are major code bottlenecks such as process monster or update_view. -JG
 */
bool project_los_not_player(int y1, int x1, int dd, int ds, int dif, int typ)
{
	int i, x, y;

	u32b flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;

	bool obvious = FALSE;

	/* Affect all (nearby) monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/*The LOS function doesn't do well with long distances*/
		if (distance(y1, x1, y, x) > MAX_RANGE) continue;

		/* Require line of sight or the monster being right on the square */
		if ((y != y1) || (x != x1))
		{

			if (!los(y1, x1, y, x)) continue;

		}

		/* Jump directly to the target monster */
		if (project(-1, 0, y, x, y, x, dd, ds, dif, typ, flg, 0, FALSE)) obvious = TRUE;
	}

	/* Result */
	return (obvious);
}


/*
 * Apply a "project()" directly to all viewable monsters
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 */
bool project_los(int typ, int dd, int ds, int dif)
{
	int i, x, y;

	u32b flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;

	bool obvious = FALSE;

	/* Affect all (nearby) monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Require line of fire */
		if (!player_can_fire_bold(y, x)) continue;
		if (!player_has_los_bold(y, x)) continue;

		/* Jump directly to the target monster */
		if (project(-1, 0, y, x, y, x, dd, ds, dif, typ, flg, 0, FALSE)) obvious = TRUE;
	}

	/* Result */
	return (obvious);
}


/*
 * Apply a "project()" directly to all viewable grids
 */
bool project_los_grids(int typ, int dd, int ds, int dif)
{
	int x, y;
	u32b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE | PROJECT_JUMP;

	bool obvious = FALSE;

	for (y = p_ptr->py - MAX_SIGHT; y < p_ptr->py + MAX_SIGHT; y++)
	{
		for (x = p_ptr->px - MAX_SIGHT; x < p_ptr->px + MAX_SIGHT; x++)
		{
			if (!in_bounds_fully(y, x)) continue;
			
			if (!player_has_los_bold(y, x)) continue;

			if (project(-1, 0, y, x, y, x, dd, ds, dif, typ, flg, 0, FALSE))
			{
				obvious = TRUE;
			}
		}
	}
	/* Result */
	return (obvious);
}


/*
 * This routine clears the entire "temp" set.
 */
void clear_temp_array(void)
{
	int i;

	/* Apply flag changes */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		/* No longer in the array */
		cave_info[y][x] &= ~(CAVE_TEMP);
	}

	/* None left */
	temp_n = 0;
}


/*
 * Aux function -- see below
 */
void cave_temp_mark(int y, int x, bool room)
{
	/* Avoid infinite recursion */
	if (cave_info[y][x] & (CAVE_TEMP)) return;

	/* Option -- do not leave the current room */
	if ((room) && (!(cave_info[y][x] & (CAVE_ROOM)))) return;

	/* Verify space */
	if (temp_n == TEMP_MAX) return;

	/* Mark the grid */
	cave_info[y][x] |= (CAVE_TEMP);

	/* Add it to the marked set */
	temp_y[temp_n] = y;
	temp_x[temp_n] = x;
	temp_n++;
}

/*
 * Mark the nearby area with CAVE_TEMP flags.  Allow limited range.
 */
void spread_cave_temp(int y1, int x1, int range, bool room)
{
	int i, y, x;

	/* Add the initial grid */
	cave_temp_mark(y1, x1, room);

	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get marked, but stop further spread */
		if (!cave_floor_bold(y, x)) continue;

		/* Note limited range (note:  we spread out one grid further) */
		if ((range) && (distance(y1, x1, y, x) >= range)) continue;

		/* Spread adjacent */
		cave_temp_mark(y + 1, x, room);
		cave_temp_mark(y - 1, x, room);
		cave_temp_mark(y, x + 1, room);
		cave_temp_mark(y, x - 1, room);

		/* Spread diagonal */
		cave_temp_mark(y + 1, x + 1, room);
		cave_temp_mark(y - 1, x - 1, room);
		cave_temp_mark(y - 1, x + 1, room);
		cave_temp_mark(y + 1, x - 1, room);
	}
}


/*
 * Slow monsters
 */
bool slow_monsters(int power)
{
	return (project_los(GF_SLOW, 0, 0, power));
}

/*
 * Sleep monsters
 */
bool sleep_monsters(int power)
{
	return (project_los(GF_SLEEP, 0, 0, power));
}


/*
 * Destroy traps
 */
bool destroy_traps(int power)
{
	return (project_los_grids(GF_KILL_TRAP, 0, 0, power));
}


/*
 * Open doors
 */
bool open_doors(int power)
{
	return (project_los_grids(GF_KILL_DOOR, 0, 0, power));
}


/*
 * Close and lock doors
 */
bool lock_doors(int power)
{
	return (project_los_grids(GF_LOCK_DOOR, 0, 0, power));
}


/*
 * Wake up all monsters, and speed up "los" monsters.
 */
void wake_all_monsters(int who)
{
	int i;

	/* Aggravate everyone */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip aggravating monster (or player) */
		if (i == who) continue;

		// Alert it
		set_alertness(m_ptr, MAX(m_ptr->alertness, ALERTNESS_VERY_ALERT));

		/*possibly update the monster health bar*/
		if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTHBAR);
	}
}

/*
 * Set the aggressive flag on nearby monsters (using the sound metric).
 */
void make_aggressive(void)
{
	int i;
	
	for (i = 1; i < mon_max; i++)
	{
		/* Check the i'th monster */
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		
		if ((m_ptr->alertness >= ALERTNESS_ALERT) && (get_noise_dist(FLOW_REAL_NOISE,m_ptr->fy,m_ptr->fx) <= 10))
		{
			m_ptr->mflag |= (MFLAG_AGGRESSIVE);
			
			if ((r_ptr->flags2 & (RF2_SMART)) &&
			    ((r_ptr->flags1 & (RF1_FRIENDS)) ||
				 (r_ptr->flags1 & (RF1_FRIEND)) ||
				 (r_ptr->flags1 & (RF1_UNIQUE_FRIEND)) ||
				 (r_ptr->flags1 & (RF1_ESCORT)) ||
				 (r_ptr->flags1 & (RF1_ESCORTS)) ||
				 (r_ptr->flags4 & (RF4_SHRIEK))))
			{
				tell_allies(m_ptr->fy, m_ptr->fx, MFLAG_AGGRESSIVE);
			}
		}
	}
}


/*
 * Delete all non-unique monsters of a given "type" from the level
 */
bool banishment(void)
{
	int i;

	char typ;

	/* Mega-Hack -- Get a monster symbol */
	if (!get_com("Choose a monster race (by symbol) to banish: ", &typ))
		return FALSE;

	/* Delete the monsters of that "type" */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip Unique Monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Skip "wrong" monsters */
		if (r_ptr->d_char != typ) continue;

		/* Delete the monster */
		delete_monster_idx(i);

		/* Take some damage */
		take_hit(dieroll(4), "the strain of casting Banishment");

	}

	/* Success */
	return TRUE;

}


/*
 * Delete all nearby (non-unique) monsters
 */
bool mass_banishment(void)
{
	int i;

	bool result = FALSE;


	/* Delete the (nearby) monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > MAX_SIGHT) continue;

		/* Delete the monster */
		delete_monster_idx(i);

		/* Take some damage */
		take_hit(dieroll(3), "the strain of casting Mass Banishment");

		/* Note effect */
		result = TRUE;
	}

	return (result);
}



/*
 * The spell of destruction
 *
 * This spell "deletes" monsters (instead of "killing" them).
 *
 * Later we may use one function for both "destruction" and
 * "earthquake" by using the "full" to select "destruction".
 */
void destroy_area(int y1, int x1, int r, bool full)
{
	int y, x, k, t;

	bool flag = FALSE;

	/* Unused parameter */
	(void)full;

	/* No effect on the surface */
	if (!p_ptr->depth)
	{
		msg_print("The ground shakes for a moment.");
		return;
	}

	/* Big area of affect */
	for (y = (y1 - r); y <= (y1 + r); y++)
	{
		for (x = (x1 - r); x <= (x1 + r); x++)
		{
			/* Skip illegal grids */
			if (!in_bounds_fully(y, x)) continue;

			/* Extract the distance */
			k = distance(y1, x1, y, x);

			/* Stay in the circle of death */
			if (k > r) continue;

			/* Lose room and vault */
			cave_info[y][x] &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Lose light and knowledge */
			cave_info[y][x] &= ~(CAVE_GLOW | CAVE_MARK);

			/* Hack -- Notice player affect */
			if (cave_m_idx[y][x] < 0)
			{
				/* Hurt the player later */
				flag = TRUE;

				/* Do not hurt this grid */
				continue;
			}

			/* Hack -- Skip the epicenter */
			if ((y == y1) && (x == x1)) continue;

			/* Delete the monster (if any) */
			delete_monster(y, x);

			/* Destroy "valid" grids */
			if (cave_valid_bold(y, x))
			{
				int feat = FEAT_FLOOR;

				/* Delete objects */
				delete_object(y, x);

				/* Wall (or floor) type */
				t = rand_int(200);

				/* Granite */
				if (t < 60)
				{
					/* Create granite wall */
					feat = FEAT_WALL_EXTRA;
				}

				/* Quartz */
				else if (t < 100)
				{
					/* Create quartz vein */
					feat = FEAT_QUARTZ;
				}

				/* Change the feature */
				cave_set_feat(y, x, feat);
			}
		}
	}


	/* Hack -- Affect player */
	if (flag)
	{
		/* Message */
		msg_print("There is a searing blast of light!");

		/* Blind the player */
		if (allow_player_blind(NULL))
		{
			/* Become blind */
			(void)set_blind(p_ptr->blind + 10 + dieroll(10));
		}
	}

	/* Make a lot of noise */
	monster_perception(TRUE, FALSE, -30);
	
	
	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}


/*
 * Creates an earthquake effect centered around (cy,cx) with radius r.
 *
 * Does rd8 damage at the centre, and one less die each square out 
 * from there. If a square doesn't have a monster in it after the damage
 * it might be transformed to a different terrain (eg floor to rubble,
 * rubble to wall, wall to rubble), with a damage% chance. Note that
 * no damage is done to the square at the epicentre.
 * 
 * If 'pit_y' and 'pit_x' are not zero, then a pit will be created at the specified
 * location. If the player is in this location, they get a chance to move to another
 * square. If there are no squares to jump to, they fall into the pit and take some
 * more damage.
 *
 * Sil-y: Theoretically the non-pit stuff could be moved to the project functions
 *        assuming it can pass through walls properly and that it can deal with
 *        creating terrain in a square with a monster iff it first kills the monster
 *        and it can decay appropriately with distance (this last might be hardest).
 */
void earthquake(int cy, int cx, int pit_y, int pit_x, int r, int who)
{
	int i, t;
	int y, x, dy, dx;
	
	int dd, ds, damage, net_dam, prt;

	int dist;
	
	int sn = 0, sy = 0, sx = 0;
	int feat;
	
	monster_type *creator_m_ptr;

	bool creator_vis = FALSE;
	bool fall_into_pit = FALSE;
	bool already_in_pit = FALSE;
	
	/* No effect on the surface */
	if (!p_ptr->depth)
	{
		msg_print("The ground shakes for a moment.");
		return;
	}
	
	// Set the earthquake creator
	if (who < 0)
	{
		creator_m_ptr = PLAYER;
		creator_vis = TRUE;
	}
	else
	{
		creator_m_ptr = &mon_list[who];
		creator_vis = creator_m_ptr->ml;
	}
	

	/* Paranoia -- Enforce maximum range */
	if (r > 10) r = 10;

	// Step 1: 
	// deal with pit creation (if a valid location was passed to this function)
	if (in_bounds_fully(pit_y, pit_x))
	{
		// can't dodge out of a pit
		if (cave_pit(p_ptr->py,p_ptr->px))
		{
			already_in_pit = TRUE;
		}
		
		// deal with the possibility that the player is there
		if ((p_ptr->py == pit_y) && (p_ptr->px == pit_x))
		{
			if (!already_in_pit)
			{
				/* Check around the player for safe locations to dodge to */
				for (i = 0; i < 8; i++)
				{
					/* Get the location */
					y = p_ptr->py + ddy_ddd[i];
					x = p_ptr->px + ddx_ddd[i];
					
					/* Skip non-empty grids */
					if (!cave_empty_bold(y, x)) continue;
					
					/* Count "safe" grids, apply the randomizer */
					if ((++sn > 1) && (rand_int(sn) != 0)) continue;
					
					/* Save the safe location */
					sy = y; sx = x;
				}
			}
			
			if (sn > 0)
			{
				monster_swap(p_ptr->py, p_ptr->px, sy, sx);
			}
			
			else
			{
				// remember to make the player fall into the pit later
				
				fall_into_pit = TRUE;
			}
		}

		if (cave_valid_bold(pit_y, pit_x))
		{
			/* Delete objects */
			delete_object(pit_y, pit_x);
			
			/* Change the feature */
			cave_set_feat(pit_y, pit_x, FEAT_TRAP_PIT);
		}
		
	}

	// Step 2:
	// Earthquake damage
	
	// flash the area (using project)
	project_ball(-1, r, p_ptr->py, p_ptr->px, p_ptr->py, p_ptr->px, 0, 0, -1, GF_EARTHQUAKE, PROJECT_PASS, FALSE);

	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			y = cy + dy;
			x = cx + dx;

			/* Skip illegal grids */
			if (!in_bounds_fully(y, x)) continue;
			
			dist = distance(cy, cx, y, x);

			/* Skip distant grids */
			if (dist > r) continue;

			// Sil-y: previously lost knowledge of the squares
			// cave_info[y][x] &= ~(CAVE_MARK);

			/* Skip the epicentre */
			if ((y == cy) && (x == cx)) continue;
			
			// Roll the damage for this square
			dd = r+1-dist;
			ds = 8;
			damage = damroll(dd, ds);

			// If the player is on the square...
			if (cave_m_idx[y][x] < 0)
			{
				// appropriate message
				msg_print("You are pummeled with debris!");

				// apply protection
				prt = protection_roll(GF_HURT, FALSE);
				net_dam = damage - prt;
				
				// take the damage
				if (net_dam > 0)	take_hit(net_dam, "an earthquake");
				
				// do stunning
				if (allow_player_stun(NULL))
				{ 
					set_stun(p_ptr->stun + net_dam * 4);
				}
				
				// update the combat rolls to display this later
				update_combat_rolls1b(creator_m_ptr, PLAYER, creator_vis);
				update_combat_rolls2(dd, ds, damage, -1, -1, prt, 100, GF_HURT, FALSE);
			}
			
			// If a monster is on the square...
			else if (cave_m_idx[y][x] > 0)
			{
				monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];
				
				char m_name[80];
				
				/* Describe the monster */
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				// Apply monster protection
				prt = damroll(r_ptr->pd, r_ptr->ps);
				net_dam = damage - prt;
				
				// apply damage after protection
				if (net_dam > 0)
				{
					bool killed = FALSE;
					
					// message for each visible monster
					if (m_ptr->ml)	msg_format("%^s is hit by falling debris.",m_name);
					
					// if visible and caused by the player, update the combat rolls to display this later
					// Sil-y: does seem to work so turned off temporarily
					if (m_ptr->ml)
					{
						update_combat_rolls1b(creator_m_ptr, m_ptr, creator_vis);
						update_combat_rolls2(dd, ds, damage, r_ptr->pd, r_ptr->ps, prt, 100, GF_HURT, FALSE);
					}
					
					// do the damage and check for death
					killed = mon_take_hit(cave_m_idx[y][x], damage, NULL, who);
					
					// special effects for survivors
					if (!killed)
					{
						/*some creatures are resistant to stunning*/
						if (r_ptr->flags3 & RF3_NO_STUN)
						{
							monster_lore *l_ptr = &l_list[m_ptr->r_idx];
							
							/*mark the lore*/
							if (m_ptr->ml) l_ptr->flags3 |= (RF3_NO_STUN);
						}
						
						else
						{
							m_ptr->stunned = m_ptr->stunned + net_dam * 4;
						}
						
						// Alert it
						set_alertness(m_ptr, MAX(m_ptr->alertness + 10, ALERTNESS_VERY_ALERT));
						
						// message for non-visible monsters
						if (!m_ptr->ml)	message_pain(cave_m_idx[y][x], damage);
					}
					
				}
			}
			
			// squares without monsters/player will sometimes get transformed
			// (note that the monster may have been there but got killed by now)
			if ((cave_m_idx[y][x] == 0) && percent_chance(damage) && !((y == pit_y) && (x == pit_x)))
			{
				/* Destroy location (if valid) */
				if (cave_valid_bold(y, x))
				{					
					/* Delete objects */
					delete_object(y, x);
					
					/* Wall (or floor) type */
					t = rand_int(100);
					
					// if we started with open floor
					if (cave_floor_bold(y, x))
					{
						if (t < 40)			feat = FEAT_RUBBLE;
						else if (t < 80)	feat = FEAT_WALL_EXTRA;
						else				feat = FEAT_QUARTZ;
					}
					
					// if we started with rubble
					else if (cave_feat[y][x] == FEAT_RUBBLE)
					{
						if (t < 40)			feat = FEAT_FLOOR;
						else if (t < 70)	feat = FEAT_WALL_EXTRA;
						else				feat = FEAT_QUARTZ;
					}
					
					// if we started with a wall of some sort
					else
					{
						if (t < 80)			feat = FEAT_RUBBLE;
						else				feat = FEAT_FLOOR;
					}
					
					/* Change the feature */
					cave_info[y][x] &= ~(CAVE_MARK);
					cave_set_feat(y, x, feat);
				}
			}
		}
	}

	// Step 3:
	// Miscellaneous stuff
	
	// Fall into the pit if there were no safe squares to jump to
	if (fall_into_pit && cave_pit(p_ptr->py,p_ptr->px))
	{
		// Store information for the combat rolls window
		combat_roll_special_char = (&f_info[FEAT_TRAP_PIT])->d_char;
		combat_roll_special_attr = (&f_info[FEAT_TRAP_PIT])->d_attr;

		msg_print("You fall back into the newly made pit!");
		
		/* Falling damage */
		damage = damroll(2, 4);

		update_combat_rolls1b(NULL, PLAYER, TRUE);
		update_combat_rolls2(2, 4, damage, -1, -1, 0, 0, GF_HURT, FALSE);
		
		/* Take the damage */
		take_hit(damage, "falling into a pit");
		
	}


	/* Make a lot of noise */
	monster_perception(TRUE, FALSE, -30);
		
	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Update the health bar */
	p_ptr->redraw |= (PR_HEALTHBAR);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}



/*
 * This routine clears the entire "temp" set.
 *
 * This routine will Perma-Lite all "temp" grids.
 *
 * This routine is used (only) by "light_room()"
 *
 * Dark grids are illuminated.
 *
 * Also, process all affected monsters.
 *
 * SMART monsters always wake up when illuminated
 * NORMAL monsters wake up 1/4 the time when illuminated
 * MINDLESS monsters wake up 1/10 the time when illuminated
 */
static void cave_temp_room_light(void)
{
	int i;

	/* Apply flag changes */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		/* No longer in the array */
		cave_info[y][x] &= ~(CAVE_TEMP);

		/* Perma-Lite */
		cave_info[y][x] |= (CAVE_GLOW);
	}

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Update stuff */
	update_stuff();

	/* Process the grids */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		/* Redraw the grid */
		lite_spot(y, x);

		/* Process affected monsters */
		if (cave_m_idx[y][x] > 0)
		{
			int alerting_power = damroll(2,10);

			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Mindless monsters rarely wake up */
			if (r_ptr->flags2 & (RF2_MINDLESS)) alerting_power /= 2;

			/* Smart monsters mostly wake up */
			if (r_ptr->flags2 & (RF2_SMART)) alerting_power *= 2;

			/* Alert unwary/sleeping monsters to a degree */
			if (m_ptr->alertness < ALERTNESS_UNWARY)
			{
				set_alertness(m_ptr, MIN(m_ptr->alertness + alerting_power, ALERTNESS_ALERT));
				
				/*possibly update the monster health bar*/
				if (p_ptr->health_who == cave_m_idx[m_ptr->fy][m_ptr->fx])
					p_ptr->redraw |= (PR_HEALTHBAR);
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
 * This routine is used (only) by "darken_room()"
 */
static void cave_temp_room_darken(void)
{
	int i;

	/* Apply flag changes */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		/* No longer in the array */
		cave_info[y][x] &= ~(CAVE_TEMP);

		/* Darken the grid */
		cave_info[y][x] &= ~(CAVE_GLOW);

		/* Hack -- Forget "boring" grids */
		if (cave_floorlike_bold(y,x))
		{
			/* Forget the grid */
			cave_info[y][x] &= ~(CAVE_MARK);
		}
	}

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Update stuff */
	update_stuff();

	/* Process the grids */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		/* Redraw the grid */
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
	if (cave_info[y][x] & (CAVE_TEMP)) return;

	/* Do not "leave" the current room */
	if (!(cave_info[y][x] & (CAVE_ROOM))) return;

	/* Paranoia -- verify space */
	if (temp_n == TEMP_MAX) return;

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
void light_room(int y1, int x1)
{
	int i, x, y;

	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);

	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get lit, but stop light */
		if (!cave_floor_bold(y, x)) continue;

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
	cave_temp_room_light();
}


/*
 * Darken all rooms containing the given location
 */
void darken_room(int y1, int x1)
{
	int i, x, y;

	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);

	/* Spread, breadth first */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get dark, but stop darkness */
		if (!cave_floor_bold(y, x)) continue;

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
	cave_temp_room_darken();
}



/*
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
bool light_area(int dd, int ds, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_KILL | PROJECT_PASS;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("You are surrounded by a white light.");
	}

	/* Hook into the "project()" function */
	(void)project(-1, rad, py, px, py, px, dd, ds, -1, GF_LIGHT, flg, 0, FALSE);

	/* Assume seen */
	return (TRUE);
}


/*
 * Hack -- call darkness around the player
 * Affect all monsters in the projection radius
 */
bool darken_area(int dd, int ds, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("Darkness surrounds you.");
	}

	/* Hook into the "project()" function */
	(void)project(-1, rad, py, px, py, px, dd, ds, -1, GF_DARK_WEAK, flg, 0, FALSE);

	/* Darken the room */
	darken_room(py, px);

	/* Assume seen */
	return (TRUE);
}

/*
 * Character casts a special-purpose bolt or beam spell.
 */
bool fire_bolt_beam_special(int typ, int dir, int dd, int ds, int dif, int rad, u32b flg)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* This is a beam spell */
	if (flg & (PROJECT_BEAM))
	{
		/* Cast a beam */
		return (project_beam(-1, rad, p_ptr->py, p_ptr->px, y1, x1, dd, ds, dif,
	                        typ, flg));
	}

	/* This is a bolt spell */
	else
	{
		/* Cast a bolt */
		return (project_bolt(-1, rad, p_ptr->py, p_ptr->px, y1, x1, dd, ds, dif,
									typ, flg));
	}
}


/*
 * Character casts a (simple) ball spell.
 */
bool fire_ball(int typ, int dir, int dd, int ds, int dif, int rad)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast a (simple) ball */
	return (project_ball(-1, rad, p_ptr->py, p_ptr->px, y1, x1, dd, ds, dif, typ,
	                     0L, FALSE));
}

/*
 * Character casts an arc spell.
 */
bool fire_arc(int typ, int dir, int dd, int ds, int dif, int rad, int degrees)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast an arc */
	return (project_arc(-1, rad, p_ptr->py, p_ptr->px, y1, x1, dd, ds, dif, typ,
	                    0L, degrees));
}


/*
 * Character casts a bolt spell.
 */
bool fire_bolt(int typ, int dir, int dd, int ds, int dif)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast a bolt */
	return (project_bolt(-1, MAX_RANGE, p_ptr->py, p_ptr->px, y1, x1, dd, ds, dif,
	                     typ, 0L));
}

/*
 * Character casts a beam spell.
 */
bool fire_beam(int typ, int dir, int dd, int ds, int dif)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast a beam */
	return (project_beam(-1, MAX_RANGE, p_ptr->py, p_ptr->px, y1, x1, dd, ds, dif,
	                     typ, 0L));
}


/*
 * Cast a bolt or a beam spell
 */
bool fire_bolt_or_beam(int prob, int typ, int dir, int dd, int ds, int dif)
{
	if (percent_chance(prob))
	{
		return (fire_beam(typ, dir, dd, ds, dif));
	}
	else
	{
		return (fire_bolt(typ, dir, dd, ds, dif));
	}
}

/*
 * Some of the old functions
 */

bool light_line(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID;
	return (fire_bolt_beam_special(GF_LIGHT, dir, 6, 4, -1, MAX_RANGE, flg));
}

bool blast(int dir, int dd, int ds)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (fire_bolt_beam_special(GF_KILL_WALL, dir, dd, ds, -1, MAX_RANGE, flg));
}

bool destroy_door(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (fire_bolt_beam_special(GF_KILL_DOOR, dir, 0, 0, -1, MAX_RANGE, flg));
}

bool disarm_trap(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (fire_bolt_beam_special(GF_KILL_TRAP, dir, 0, 0, -1, MAX_RANGE, flg));
}


/*
 * Curse the player's armor
 */
bool curse_armor(void)
{
	object_type *o_ptr;

	char o_name[80];


	/* Curse the body armor */
	o_ptr = &inventory[INVEN_BODY];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

	/* Attempt a saving throw for artefacts */
	if (artefact_p(o_ptr) && percent_chance(50))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your armor", o_name);
	}

	/* not artefact or failed save... */
	else
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		/* Blast the armor */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_BLASTED;
		o_ptr->evn = 0 - dieroll(5) - dieroll(5);
		o_ptr->att = 0;
		o_ptr->pd = 1;
		o_ptr->ps = 1;
		o_ptr->dd = 1;
		o_ptr->ds = 1;

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0);
	}

	return (TRUE);
}


/*
 * Curse the player's weapon
 */
bool curse_weapon(void)
{
	object_type *o_ptr;

	char o_name[80];


	/* Curse the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

	/* Attempt a saving throw */
	if (artefact_p(o_ptr) && percent_chance(50))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your weapon", o_name);
	}

	/* not artefact or failed save... */
	else
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		/* Shatter the weapon */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_SHATTERED;
		o_ptr->att = 0 - dieroll(5) - dieroll(5);
		o_ptr->evn = 0;
		o_ptr->dd = 1;
		o_ptr->ds = 1;

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0);
	}

	/* Notice */
	return (TRUE);
}


/*
 * Hook to specify "arrows"
 */
bool item_tester_hook_ided_ammo(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_ARROW:
		{
			if (object_known_p(o_ptr)) return (TRUE);
			else return FALSE;
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "arrows"
 */
bool item_tester_hook_ammo(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_ARROW:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}



/*
 * Identifies all objects in the equipment and inventory.
 * Applies quality/special item squelch in the inventory.
 */
void identify_and_squelch_pack(void)
{
  	int item, squelch;
	object_type *o_ptr;

	/* Identify equipment */
	for (item = INVEN_WIELD; item < INVEN_TOTAL; item++)
	{
		/* Get the object */
		o_ptr = &inventory[item];

		/* Ignore empty objects */
		if (!o_ptr->k_idx) continue;

		/* Ignore known objects */
		if (object_known_p(o_ptr)) continue;

		/* Identify it */
		(void)do_ident_item(item, o_ptr);
	}

	/* Identify inventory */
	for (item = 0; item < INVEN_WIELD; item++)
	{
	  	while (TRUE)
		{
	  		/* Get the object */
			o_ptr = &inventory[item];

			/* Ignore empty objects */
			if (!o_ptr->k_idx) break;

			/* Ignore known objects */
			if (object_known_p(o_ptr)) break;

			/* Identify it and get the squelch setting */
			squelch = do_ident_item(item, o_ptr);

			/*
			 * If the object was squelched, keep analyzing
			 * the same slot (the inventory was displaced). -DG-
			 */
			if (squelch != SQUELCH_YES) break;

			/* Now squelch the object */
			do_squelch_item(squelch, item, o_ptr);
		}
	}
}

/* Mass-identify handler */
bool mass_identify (int rad)
{
	/* Direct the ball to the player */
  	target_set_location(p_ptr->py, p_ptr->px);

	/* Cast the ball spell */
	fire_ball(GF_IDENTIFY, 5, 0, 0, -1, rad);

  	/* Identify equipment and inventory, apply quality squelch */
  	identify_and_squelch_pack();

	/* This spell always works */
	return (TRUE);
}


/*
 * Execute some common code of the identify spells.
 * "item" is used to print the slot occupied by an object in equip/inven.
 * ANY negative value assigned to "item" can be used for specifying an object
 * on the floor (they don't have a slot, example: the code used to handle
 * GF_IDENTIFY in project_o).
 * It returns the value returned by squelch_itemp.
 * The object is NOT squelched here.
 */
int do_ident_item(int item, object_type *o_ptr)
{
	char o_name[80];
	int squelch = SQUELCH_NO;

	/* Identify it */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Apply an autoinscription, if necessary */
	apply_autoinscription(o_ptr);

	/* Squelch it? */
	if (item < INVEN_WIELD) squelch = squelch_itemp(o_ptr, 0, TRUE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0);

	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Describe */
	if (item >= INVEN_WIELD)
	{
		msg_format("%^s: %s (%c).",
			describe_use(item), o_name, index_to_label(item));
	}
	else if (item >= 0)
	{
		msg_format("In your pack: %s (%c).  %s",
			o_name, index_to_label(item),
			squelch_to_label(squelch));
 	}
	else
	{
		msg_format("On the ground: %s.  %s", o_name,
			squelch_to_label(squelch));
	}

	return (squelch);
}

