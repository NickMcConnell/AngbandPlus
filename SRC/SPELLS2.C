/* File: spells2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"





/*
 * Increase players hit points, notice effects
 */
int hp_player(int num)
{
	byte guess=0;

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
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		/* Heal 0-4 */
		if (num < 5)
		{
			msg_print("You feel a little better.");
			guess = SV_POTION_CURE_LIGHT;

		}

		/* Heal 5-14 */
		else if (num < 15)
		{
			msg_print("You feel better.");
			guess = SV_POTION_CURE_LIGHT;
		}

		/* Heal 15-34 */
		else if (num < 35)
		{
			msg_print("You feel much better.");
			guess = SV_POTION_CURE_SERIOUS;
		}

		/* Heal 35+ */
		else if (num < 50)
		{
			msg_print("You feel very good.");
			guess = SV_POTION_CURE_CRITICAL;
		}

		/* Heal 50+ */
		else if (num < 300)
		{
			msg_print("You feel very good.");
			guess = SV_POTION_HEALING;
		}

		/* Heal 300+ */
		else
		{
			msg_print("You feel very good.");
			guess = SV_POTION_STAR_HEALING;
		}

		/* Notice */
		return (guess);
	}

	/* Ignore */
	return (0);
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
 * Leave a "trap" which affects those moving on it
 */
void warding_trap(int feat, int dir)
{
	int ty = p_ptr->py + ddy[dir];
	int tx = p_ptr->px + ddx[dir];

	/* XXX XXX XXX */
	if (!cave_clean_bold(ty, tx))
	{
		msg_print("The object resists the spell.");
		return;
	}

	/* Create a trap */
	cave_set_feat(ty, tx, feat);
}


/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_pos[] =
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
static cptr desc_stat_neg[] =
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
bool do_dec_stat(int stat)
{
	bool sust = FALSE;

	/* Get the "sustain" */
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
	if (sust)
	{
		/* Message */
		msg_format("You feel very %s for a moment, but the feeling passes.",
		           desc_stat_neg[stat]);

#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
                equip_can_flags(0x0L,(1L<<stat),0x0L);
#endif

		/* Notice effect */
		return (TRUE);
	}

	/* Attempt to reduce the stat */
	if (dec_stat(stat, 10, FALSE))
	{
		/* Message */
		msg_format("You feel very %s.", desc_stat_neg[stat]);

#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,(1L<<stat),0x0L);
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

	/* Restore strength */
	res = res_stat(stat);

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
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
}






/*
 * Used by the "enchant" function (chance of failure)
 */
static int enchant_table[16] =
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
static int remove_curse_aux(int all)
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
		if (!all && (f3 & (TR3_HEAVY_CURSE)))
		{
			/* Learn about the object */
			object_can_flags(o_ptr,0x0L,0x0L,TR3_HEAVY_CURSE);

			continue;
		}

		/* Perma-Cursed Items can NEVER be uncursed */
		if (f3 & (TR3_PERMA_CURSE))
		{
			/* Learn about the object */
			if (all) object_can_flags(o_ptr,0x0L,0x0L,TR3_PERMA_CURSE);

			continue;
		}

		/* Learn about the object */
		if (!all) object_not_flags(o_ptr,0x0L,0x0L,TR3_HEAVY_CURSE);

		/* Learn about the object */
		object_not_flags(o_ptr,0x0L,0x0L,TR3_PERMA_CURSE);

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

	u32b f1 = 0L, f2 = 0L, f3 = 0L;

	object_type *o_ptr;

	cptr info[128];


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
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,0x0L,TR3_SLOW_DIGEST);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,0x0L,TR3_SLOW_DIGEST);
#endif
	}
	if (p_ptr->ffall)
	{
		info[i++] = "You land gently.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,0x0L,TR3_FEATHER);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,0x0L,TR3_FEATHER);
#endif
	}
	if (p_ptr->lite)
	{
		info[i++] = "You are glowing with light.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,0x0L,TR3_LITE);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,0x0L,TR3_LITE);
#endif
	}
	if (p_ptr->regenerate)
	{
		info[i++] = "You regenerate quickly.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,0x0L,TR3_REGEN);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,0x0L,TR3_REGEN);
#endif
	}
	if (p_ptr->telepathy)
	{
		info[i++] = "You have ESP.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,0x0L,TR3_TELEPATHY);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,0x0L,TR3_TELEPATHY);
#endif
	}
	if (p_ptr->see_inv)
	{
		info[i++] = "You can see invisible creatures.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,0x0L,TR3_SEE_INVIS);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,0x0L,TR3_SEE_INVIS);
#endif
	}
	if (p_ptr->free_act)
	{
		info[i++] = "You have free action.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,0x0L,TR3_FREE_ACT);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,0x0L,TR3_FREE_ACT);
#endif
	}
	if (p_ptr->hold_life)
	{
		info[i++] = "You have a firm hold on your life force.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,0x0L,TR3_HOLD_LIFE);
#endif
	}
	else if (p_ptr->blessed)
	{
		info[i++] = "You have a temporary hold on your life force.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,0x0L,TR3_HOLD_LIFE);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,0x0L,TR3_HOLD_LIFE);
#endif
	}


	if (p_ptr->immune_acid)
	{

		info[i++] = "You are completely immune to acid.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_IM_ACID,0x0L);
#endif
	}
	else if ((p_ptr->resist_acid) && (p_ptr->oppose_acid))
	{
		info[i++] = "You resist acid exceptionally well.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_IM_ACID,0x0L);
		equip_can_flags(0x0L,TR2_RES_ACID,0x0L);
#endif

	}
	else if ((p_ptr->resist_acid) || (p_ptr->oppose_acid))
	{
		info[i++] = "You are resistant to acid.";

#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_IM_ACID,0x0L);
		if(!p_ptr->oppose_acid) equip_can_flags(0x0L,TR2_RES_ACID,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_IM_ACID,0x0L);
		equip_not_flags(0x0L,TR2_RES_ACID,0x0L);
#endif
	}

	if (p_ptr->immune_elec)
	{
		info[i++] = "You are completely immune to lightning.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_IM_ELEC,0x0L);
#endif
	}
	else if ((p_ptr->resist_elec) && (p_ptr->oppose_elec))
	{
		info[i++] = "You resist lightning exceptionally well.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_IM_ELEC,0x0L);
		equip_can_flags(0x0L,TR2_RES_ELEC,0x0L);
#endif
	}
	else if ((p_ptr->resist_elec) || (p_ptr->oppose_elec))
	{
		info[i++] = "You are resistant to lightning.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_IM_ELEC,0x0L);
		if(!p_ptr->oppose_elec) equip_can_flags(0x0L,TR2_RES_ELEC,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_IM_ELEC,0x0L);
		equip_not_flags(0x0L,TR2_RES_ELEC,0x0L);
#endif
	}

	if (p_ptr->immune_fire)
	{
		info[i++] = "You are completely immune to fire.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_IM_FIRE,0x0L);
#endif
	}
	else if ((p_ptr->resist_fire) && (p_ptr->oppose_fire))
	{
		info[i++] = "You resist fire exceptionally well.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_IM_FIRE,0x0L);
		equip_can_flags(0x0L,TR2_RES_FIRE,0x0L);
#endif
	}
	else if ((p_ptr->resist_fire) || (p_ptr->oppose_fire))
	{
		info[i++] = "You are resistant to fire.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_IM_FIRE,0x0L);
		if(!p_ptr->oppose_cold) equip_can_flags(0x0L,TR2_RES_FIRE,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_IM_FIRE,0x0L);
		equip_not_flags(0x0L,TR2_RES_FIRE,0x0L);
#endif
	}

	if (p_ptr->immune_cold)
	{
		info[i++] = "You are completely immune to cold.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_IM_COLD,0x0L);
#endif
	}
	else if ((p_ptr->resist_cold) && (p_ptr->oppose_cold))
	{
		info[i++] = "You resist cold exceptionally well.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_IM_COLD,0x0L);
		equip_can_flags(0x0L,TR2_RES_COLD,0x0L);
#endif
	}
	else if ((p_ptr->resist_cold) || (p_ptr->oppose_cold))
	{
		info[i++] = "You are resistant to cold.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_IM_COLD,0x0L);
		if(!p_ptr->oppose_cold) equip_can_flags(0x0L,TR2_RES_COLD,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_IM_COLD,0x0L);
		equip_not_flags(0x0L,TR2_RES_COLD,0x0L);
#endif
	}

	if ((p_ptr->resist_pois) && (p_ptr->oppose_pois))
	{
		info[i++] = "You resist poison exceptionally well.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_RES_POIS,0x0L);
#endif
	}
	else if ((p_ptr->resist_pois) || (p_ptr->oppose_pois))
	{
		info[i++] = "You are resistant to poison.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		if(!p_ptr->oppose_cold) equip_can_flags(0x0L,TR2_RES_POIS,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_RES_POIS,0x0L);
#endif
	}

	if (p_ptr->resist_fear)
	{
		info[i++] = "You are completely fearless.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_RES_FEAR,0x0L);
#endif
	}
	else if ((p_ptr->hero)||(p_ptr->shero))
	{
		info[i++] = "You are temporarily fearless.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_RES_FEAR,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_RES_FEAR,0x0L);
#endif
	}

	if (p_ptr->resist_lite)
	{
		info[i++] = "You are resistant to bright light.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_RES_LITE,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_RES_LITE,0x0L);
#endif
	}
	if (p_ptr->resist_dark)
	{
		info[i++] = "You are resistant to darkness.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_RES_DARK,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_RES_DARK,0x0L);
#endif
	}
	if (p_ptr->resist_blind)
	{
		info[i++] = "Your eyes are resistant to blindness.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_RES_BLIND,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_RES_BLIND,0x0L);
#endif
	}
	if (p_ptr->resist_confu)
	{
		info[i++] = "You are resistant to confusion.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_RES_CONFU,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_RES_CONFU,0x0L);
#endif
	}
	if (p_ptr->resist_sound)
	{
		info[i++] = "You are resistant to sonic attacks.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_RES_SOUND,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_RES_SOUND,0x0L);
#endif
	}
	if (p_ptr->resist_shard)
	{
		info[i++] = "You are resistant to blasts of shards.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_RES_SHARD,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_RES_SHARD,0x0L);
#endif
	}
	if (p_ptr->resist_nexus)
	{
		info[i++] = "You are resistant to nexus attacks.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_RES_NEXUS,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_RES_NEXUS,0x0L);
#endif
	}
	if (p_ptr->resist_nethr)
	{
		info[i++] = "You are resistant to nether forces.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_RES_NETHR,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_RES_NETHR,0x0L);
#endif
	}
	if (p_ptr->resist_chaos)
	{
		info[i++] = "You are resistant to chaos.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_RES_CHAOS,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_RES_CHAOS,0x0L);
#endif
	}
	if (p_ptr->resist_disen)
	{
		info[i++] = "You are resistant to disenchantment.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_RES_DISEN,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_RES_DISEN,0x0L);
#endif
	}

	if (p_ptr->sustain_str)
	{
		info[i++] = "Your strength is sustained.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_SUST_STR,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_SUST_STR,0x0L);
#endif
	}
	if (p_ptr->sustain_int)
	{
		info[i++] = "Your intelligence is sustained.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_SUST_INT,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_SUST_INT,0x0L);
#endif
	}
	if (p_ptr->sustain_wis)
	{
		info[i++] = "Your wisdom is sustained.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_SUST_WIS,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_SUST_WIS,0x0L);
#endif
	}
	if (p_ptr->sustain_con)
	{
		info[i++] = "Your constitution is sustained.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_SUST_CON,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_SUST_CON,0x0L);
#endif
	}
	if (p_ptr->sustain_dex)
	{
		info[i++] = "Your dexterity is sustained.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_SUST_DEX,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_SUST_DEX,0x0L);
#endif
	}
	if (p_ptr->sustain_chr)
	{
		info[i++] = "Your charisma is sustained.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(0x0L,TR2_SUST_CHR,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(0x0L,TR2_SUST_CHR,0x0L);
#endif
	}

	if (f1 & (TR1_STR))
	{
		info[i++] = "Your strength is affected by your equipment.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(TR1_STR,0x0L,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(TR1_STR,0x0L,0x0L);
#endif
	}
	if (f1 & (TR1_INT))
	{
		info[i++] = "Your intelligence is affected by your equipment.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(TR1_INT,0x0L,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(TR1_INT,0x0L,0x0L);
#endif
	}
	if (f1 & (TR1_WIS))
	{
		info[i++] = "Your wisdom is affected by your equipment.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(TR1_WIS,0x0L,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(TR1_WIS,0x0L,0x0L);
#endif
	}
	if (f1 & (TR1_DEX))
	{
		info[i++] = "Your dexterity is affected by your equipment.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(TR1_DEX,0x0L,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(TR1_DEX,0x0L,0x0L);
#endif
	}
	if (f1 & (TR1_CON))
	{
		info[i++] = "Your constitution is affected by your equipment.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(TR1_CON,0x0L,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(TR1_CON,0x0L,0x0L);
#endif
	}
	if (f1 & (TR1_CHR))
	{
		info[i++] = "Your charisma is affected by your equipment.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
                equip_can_flags(TR1_CHR,0x0L,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
                equip_not_flags(TR1_CHR,0x0L,0x0L);
#endif
	}

	if (f1 & (TR1_STEALTH))
	{
		info[i++] = "Your stealth is affected by your equipment.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(TR1_STEALTH,0x0L,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(TR1_STEALTH,0x0L,0x0L);
#endif
	}
	if (f1 & (TR1_SEARCH))
	{
		info[i++] = "Your searching ability is affected by your equipment.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(TR1_SEARCH,0x0L,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(TR1_SEARCH,0x0L,0x0L);
#endif
	}
	if (f1 & (TR1_INFRA))
	{
		info[i++] = "Your infravision is affected by your equipment.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(TR1_INFRA,0x0L,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(TR1_INFRA,0x0L,0x0L);
#endif
	}
	if (f1 & (TR1_TUNNEL))
	{
		info[i++] = "Your digging ability is affected by your equipment.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(TR1_TUNNEL,0x0L,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(TR1_TUNNEL,0x0L,0x0L);
#endif
	}
	if (f1 & (TR1_SPEED))
	{
		info[i++] = "Your speed is affected by your equipment.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(TR1_SPEED,0x0L,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(TR1_SPEED,0x0L,0x0L);
#endif
	}
	if (f1 & (TR1_BLOWS))
	{
		info[i++] = "Your attack speed is affected by your equipment.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(TR1_BLOWS,0x0L,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(TR1_BLOWS,0x0L,0x0L);
#endif
	}
	if (f1 & (TR1_SHOTS))
	{
		info[i++] = "Your shooting speed is affected by your equipment.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(TR1_SHOTS,0x0L,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(TR1_SHOTS,0x0L,0x0L);
#endif
	}
	if (f1 & (TR1_MIGHT))
	{
		info[i++] = "Your shooting might is affected by your equipment.";
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_can_flags(TR1_MIGHT,0x0L,0x0L);
#endif
	}
	else
	{
#ifdef ALLOW_OBJECT_INFO
		/* Always notice */
		equip_not_flags(TR1_MIGHT,0x0L,0x0L);
#endif
	}


	/* Get the current weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Analyze the weapon */
	if (o_ptr->k_idx)
	{
		/* Special "Attack Bonuses" */
		if (f1 & (TR1_BRAND_POIS))
		{
			info[i++] = "Your weapon poisons your foes.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,TR1_BRAND_POIS,0x0L,0x0L);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_not_flags(o_ptr,TR1_BRAND_POIS,0x0L,0x0L);
#endif
		}
		/* Special "Attack Bonuses" */
		if (f1 & (TR1_BRAND_ACID))
		{
			info[i++] = "Your weapon melts your foes.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,TR1_BRAND_ACID,0x0L,0x0L);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_not_flags(o_ptr,TR1_BRAND_ACID,0x0L,0x0L);
#endif
		}
		if (f1 & (TR1_BRAND_ELEC))
		{
			info[i++] = "Your weapon shocks your foes.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,TR1_BRAND_POIS,0x0L,0x0L);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_not_flags(o_ptr,TR1_BRAND_POIS,0x0L,0x0L);
#endif
		}
		if (f1 & (TR1_BRAND_FIRE))
		{
			info[i++] = "Your weapon burns your foes.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,TR1_BRAND_FIRE,0x0L,0x0L);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_not_flags(o_ptr,TR1_BRAND_FIRE,0x0L,0x0L);
#endif
		}
		if (f1 & (TR1_BRAND_COLD))
		{
			info[i++] = "Your weapon freezes your foes.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,TR1_BRAND_COLD,0x0L,0x0L);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_not_flags(o_ptr,TR1_BRAND_COLD,0x0L,0x0L);
#endif
		}

		/* Special "slay" flags */
		if (f1 & (TR1_SLAY_ANIMAL))
		{
			info[i++] = "Your weapon strikes at animals with extra force.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,TR1_SLAY_ANIMAL,0x0L,0x0L);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_not_flags(o_ptr,TR1_SLAY_ANIMAL,0x0L,0x0L);
#endif
		}
		if (f1 & (TR1_SLAY_EVIL))
		{
			info[i++] = "Your weapon strikes at evil with extra force.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,TR1_SLAY_EVIL,0x0L,0x0L);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_not_flags(o_ptr,TR1_SLAY_EVIL,0x0L,0x0L);
#endif
		}
		if (f1 & (TR1_SLAY_UNDEAD))
		{
			info[i++] = "Your weapon strikes at undead with holy wrath.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,TR1_SLAY_UNDEAD,0x0L,0x0L);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_not_flags(o_ptr,TR1_SLAY_UNDEAD,0x0L,0x0L);
#endif
		}
		if (f1 & (TR1_SLAY_DEMON))
		{
			info[i++] = "Your weapon strikes at demons with holy wrath.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,TR1_SLAY_DEMON,0x0L,0x0L);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_not_flags(o_ptr,TR1_SLAY_DEMON,0x0L,0x0L);
#endif
		}
		if (f1 & (TR1_SLAY_ORC))
		{
			info[i++] = "Your weapon is especially deadly against orcs.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,TR1_SLAY_ORC,0x0L,0x0L);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_not_flags(o_ptr,TR1_SLAY_ORC,0x0L,0x0L);
#endif
		}
		if (f1 & (TR1_SLAY_TROLL))
		{
			info[i++] = "Your weapon is especially deadly against trolls.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,TR1_SLAY_TROLL,0x0L,0x0L);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_not_flags(o_ptr,TR1_SLAY_TROLL,0x0L,0x0L);
#endif
		}
		if (f1 & (TR1_SLAY_GIANT))
		{
			info[i++] = "Your weapon is especially deadly against giants.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,TR1_SLAY_GIANT,0x0L,0x0L);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_not_flags(o_ptr,TR1_SLAY_GIANT,0x0L,0x0L);
#endif
		}
		if (f1 & (TR1_SLAY_DRAGON))
		{
			info[i++] = "Your weapon is especially deadly against dragons.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,TR1_SLAY_DRAGON,0x0L,0x0L);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_not_flags(o_ptr,TR1_SLAY_DRAGON,0x0L,0x0L);
#endif
		}

		/* Special "kill" flags */
		if (f1 & (TR1_KILL_DRAGON))
		{
			info[i++] = "Your weapon is a great bane of dragons.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,TR1_KILL_DRAGON,0x0L,0x0L);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_not_flags(o_ptr,TR1_KILL_DRAGON,0x0L,0x0L);
#endif
		}


		/* Indicate Blessing */
		if (f3 & (TR3_BLESSED))
		{
			info[i++] = "Your weapon has been blessed by the gods.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,0x0L,0x0L,TR3_BLESSED);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_not_flags(o_ptr,0x0L,0x0L,TR3_BLESSED);
#endif
		}

		/* Hack */
		if (f3 & (TR3_IMPACT))
		{
			info[i++] = "Your weapon can induce earthquakes.";
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			object_can_flags(o_ptr,0x0L,0x0L,TR3_IMPACT);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
                        object_not_flags(o_ptr,0x0L,0x0L,TR3_IMPACT);
#endif
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
	prt("[Press any key to continue]", k, 0);
	(void)inkey();


	/* Load screen */
	screen_load();
}






/*
 * Forget everything. This really sucks now.
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

		/* Remove special inscription, if any */
		if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

		/* Hack -- Clear the "felt" flag */
		o_ptr->ident &= ~(IDENT_SENSE);

		/* Hack -- Clear the "bonus" flag */
		o_ptr->ident &= ~(IDENT_BONUS);

		/* Hack -- Clear the "known" flag */
		o_ptr->ident &= ~(IDENT_KNOWN);

		/* Hack -- Clear the "empty" flag */
		o_ptr->ident &= ~(IDENT_EMPTY);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Mega-Hack -- Forget the map */
	wiz_dark();

	/* It worked */
	return (TRUE);
}



/*
 *  Set word of recall as appropriate
 */
void set_recall(void)
{
	/* Ironman */
	if (adult_ironman && !p_ptr->total_winner)
	{
		msg_print("Nothing happens.");
		return;
	}

	/* Activate recall */
	if (!p_ptr->word_recall)
	{
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


/*
 * Detect all traps on current panel
 */
bool detect_feat_flags(u32b flags1, u32b flags2)
{
	int y, x;

	bool detect = FALSE;

	/* Scan the current panel */
	for (y = p_ptr->wy; y < p_ptr->wy+SCREEN_HGT; y++)
	{
		for (x = p_ptr->wx; x < p_ptr->wx+SCREEN_WID; x++)
		{
			/* Hack -- Safe */
                        if (flags1 & (FF1_TRAP))
                        {
                                cave_info[y][x] |= (CAVE_SAFE);
                                if (view_safe_grids) lite_spot(y,x);
                        }

			/* Detect flags */
			if ((f_info[cave_feat[y][x]].flags1 & (flags1)) ||
				(f_info[cave_feat[y][x]].flags2 & (flags2)))
			{
				/* Detect secrets */
				if (f_info[cave_feat[y][x]].flags1 & (FF1_SECRET))
				{

					/*Find secrets*/
                                        cave_alter_feat(y,x,FS_SECRET);
				}

				/* Hack -- Memorize */
				cave_info[y][x] |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Obvious */
				detect = TRUE;
			}

		}
	}

	/* Result */
	return (detect);
}


/*
 * Detect all traps on current panel
 */
bool detect_traps(void)
{

	bool detect = FALSE;

	/* Describe */
	if (detect_feat_flags(FF1_TRAP,0x00))
	{
		msg_print("You sense the presence of traps!");

		detect = TRUE;
	}

	/* Result */
	return (detect);
}



/*
 * Detect all doors on current panel
 */
bool detect_doors(void)
{

	bool detect = FALSE;


	/* Describe */
	if (detect_feat_flags(FF1_DOOR,0x00))
	{
		msg_print("You sense the presence of doors!");

		detect = TRUE;
	}

	/* Result */
	return (detect);
}


/*
 * Detect all stairs on current panel
 */
bool detect_stairs(void)
{

	bool detect = FALSE;

	/* Describe */
	if (detect_feat_flags(FF1_STAIRS,0x00))
	{
		msg_print("You sense the presence of stairs!");

		detect = TRUE;
	}

	/* Result */
	return (detect);
}

/*
 * Detect all (running) water on current panel
 */
bool detect_water(void)
{

	bool detect = FALSE;

	/* Describe */
	if (detect_feat_flags(0x00,FF2_WATER))
	{
		msg_print("You sense the presence of running water!");

		detect = TRUE;
	}

	/* Result */
	return (detect);
}



/*
 * Detect any treasure on the current panel
 */
bool detect_treasure(void)
{

	bool detect = FALSE;


	/* Describe */
	if (detect_feat_flags(FF1_HAS_GOLD,0x00))
	{
		msg_print("You sense the presence of buried treasure!");

		detect = TRUE;

	}

	/* Result */
	return (detect);
}

/*
 * Detect any treasure on the current panel
 */
bool detect_objects_buried(void)
{

	bool detect = FALSE;

	/* Describe */
	if (detect_feat_flags(FF1_HAS_ITEM,0x00))
	{
		msg_print("You sense the presence of buried objects!");

		detect = TRUE;

	}

	/* Result */
	return (detect);
}


/*
 * Detect all "gold" objects on the current panel
 */
bool detect_objects_gold(void)
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

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		if (!panel_contains(y, x)) continue;

		/* Detect "gold" objects */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Hack -- memorize it */
			o_ptr->marked = TRUE;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			detect = TRUE;
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of treasure!");
	}

	/* Result */
	return (detect);
}

/*
 * Determine if the object has "=i" in its inscription.
 */
static bool auto_pickup_ignore(object_type *o_ptr)
{
	cptr s;

	/* No inscription */
	if (!o_ptr->note) return (FALSE);

	/* Find a '=' */
	s = strchr(quark_str(o_ptr->note), '=');

	/* Process inscription */
	while (s)
	{
                /* Auto-ignore on "=i" */
                if (s[1] == 'i') return (TRUE);

		/* Find another '=' */
		s = strchr(s + 1, '=');
	}

        /* Don't auto destroy */
	return (FALSE);
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

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		if (!panel_contains(y, x)) continue;

		/* Detect "real" objects */
		if (o_ptr->tval != TV_GOLD)
		{
			/* Hack -- memorize it */
                        if (!auto_pickup_ignore) o_ptr->marked = TRUE;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			detect = TRUE;
		}
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
 * Return a "feeling" (or NULL) about an item.  Method 3 (Magic).
 */
int value_check_aux3(object_type *o_ptr)
{
	/* Artifacts */
	if (artifact_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (0);

		/* Normal */
		return (INSCRIP_SPECIAL);
	}

	/* Ego-Items */
	if (ego_item_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (0);

                /* Superb */
                if ((variant_great_id) && (o_ptr->xtra1)) return (INSCRIP_SUPERB);

		/* Normal */
		return (INSCRIP_EXCELLENT);
	}

	/* Cursed items */
	if (cursed_p(o_ptr)) return (0);

	/* Broken items */
	if (broken_p(o_ptr)) return (0);

        /* Great "armor" bonus */
        if ((variant_great_id) && (o_ptr->to_a > 8)) return (INSCRIP_GREAT);

        /* Great "weapon" bonus */
        if ((variant_great_id) && (o_ptr->to_h + o_ptr->to_d > 14)) return (INSCRIP_GREAT);

        /* Very good "armor" bonus */
        if ((variant_great_id) && (o_ptr->to_a > 4)) return (INSCRIP_VERY_GOOD);

	/* Good "weapon" bonus */
        if ((variant_great_id) && (o_ptr->to_h + o_ptr->to_d > 7)) return (INSCRIP_VERY_GOOD);

	/* Good "armor" bonus */
	if (o_ptr->to_a > 0) return (INSCRIP_GOOD);

	/* Good "weapon" bonus */
	if (o_ptr->to_h + o_ptr->to_d > 0) return (INSCRIP_GOOD);

	/* Default to nothing */
	return (0);
}


/*
 * Return a "feeling" (or NULL) about an item.  Method 4 (Cursed).
 */
int value_check_aux4(object_type *o_ptr)
{
	/* Artifacts */
	if (artifact_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (INSCRIP_TERRIBLE);

		/* Normal */
		return (0);
	}

	/* Ego-Items */
	if (ego_item_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (INSCRIP_WORTHLESS);

                /* Superb */
                if (o_ptr->xtra1) return (0);

		/* Normal */
		return (0);
	}

	/* Cursed items */
        if (cursed_p(o_ptr)) return (INSCRIP_CURSED);

	/* Broken items */
	if (broken_p(o_ptr)) return (INSCRIP_BROKEN);

	/* Default to nothing */
	return (0);
}





/*
 * Detect all "magic" objects on the current panel.
 *
 * This will light up all spaces with "magic" items, including artifacts,
 * ego-items, potions, scrolls, books, rods, wands, staves, amulets, rings,
 * and "enchanted" items of the "good" variety.
 *
 * It can probably be argued that this function is now too powerful.
 *
 * Now also senses all magical objects in the inventory.
 */
bool detect_objects_magic(void)
{
	int i, y, x, tv;

	bool detect = FALSE;


	/* Scan all objects */
	for (i = 1; i < o_max; i++)
	{
		int feel = INSCRIP_AVERAGE;

		bool okay = FALSE;

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

		/* Artifacts, misc magic items, or enchanted wearables */
		if (artifact_p(o_ptr) || ego_item_p(o_ptr) ||
		    (tv == TV_AMULET) || (tv == TV_RING) ||
		    (tv == TV_STAFF) || (tv == TV_WAND) || (tv == TV_ROD) ||
		    (tv == TV_SCROLL) || (tv == TV_POTION) ||
		    (tv == TV_MAGIC_BOOK) || (tv == TV_PRAYER_BOOK) ||
		    ((o_ptr->to_a > 0) || (o_ptr->to_h + o_ptr->to_d > 0)))
		{
			/* Memorize the item */
			o_ptr->marked = TRUE;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			detect = TRUE;
		}

		/* Valid "tval" codes */
		switch (o_ptr->tval)
		{
			case TV_SHOT:
			case TV_ARROW:
			case TV_BOLT:
			case TV_BOW:
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			case TV_BOOTS:
			case TV_GLOVES:
			case TV_HELM:
			case TV_CROWN:
			case TV_SHIELD:
			case TV_CLOAK:
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			{
				okay = TRUE;
				break;
			}
		}

		/* Skip objects */
		if (!okay) continue;

		/* It already has a discount */
		if ((o_ptr->discount > 0)&&(o_ptr->discount<=INSCRIP_NULL)) continue;
#if 0
		/* It has already been sensed, do not sense it again */
		if (o_ptr->ident & (IDENT_SENSE)) continue;
#endif
		/* It is fully known, no information needed */
		if (object_known_p(o_ptr)) continue;

		/* Get the inscription */
		feel = value_check_aux3(o_ptr);

		/* Sense something? */
		if (!feel) continue;

		/* Sense the object */
		o_ptr->discount = feel;

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);

	}


	/* Sense inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		int feel = INSCRIP_AVERAGE;

		bool okay = FALSE;

                object_type *o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Valid "tval" codes */
		switch (o_ptr->tval)
		{
			case TV_SHOT:
			case TV_ARROW:
			case TV_BOLT:
			case TV_BOW:
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			case TV_BOOTS:
			case TV_GLOVES:
			case TV_HELM:
			case TV_CROWN:
			case TV_SHIELD:
			case TV_CLOAK:
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			{
				okay = TRUE;
				break;
			}
		}

		/* Skip objects */
		if (!okay) continue;

		/* It already has a discount or special inscription */
		if ((o_ptr->discount > 0)&&(o_ptr->discount<=INSCRIP_NULL)) continue;
#if 0
		/* It has already been sensed, do not sense it again */
		if (o_ptr->ident & (IDENT_SENSE)) continue;
#endif
		/* It is fully known, no information needed */
		if (object_known_p(o_ptr)) continue;

		/* Get the inscription */
		feel = value_check_aux4(o_ptr);

		/* Sense something */
		if (!feel) continue;

		/* Detected */
		detect = TRUE;

		/* Sense the object */
		o_ptr->discount = feel;

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of magic objects!");
	}

	/* Return result */
	return (detect);
}



/*
 * Detect all "cursed" objects on the current panel.
 *
 * This will light up all spaces with "cursed" items, including artifacts,
 * ego-items, and "enchanted" items of the cursed or broken variety.
 *
 * Should probably marked objects detected as cursed when appropriate.
 *
 * Now also senses all cursed objects in the inventory.
 */
bool detect_objects_cursed(void)
{
	int i, y, x;

	bool detect = FALSE;

	/* Scan all objects */
	for (i = 1; i < o_max; i++)
	{
                int feel;

		bool okay = FALSE;

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

		/* Cursed items */
                if (cursed_p(o_ptr) || broken_p(o_ptr))
		{
			/* Memorize the item */
			o_ptr->marked = TRUE;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			detect = TRUE;
		}

		/* Valid "tval" codes */
		switch (o_ptr->tval)
		{
			case TV_SHOT:
			case TV_ARROW:
			case TV_BOLT:
			case TV_BOW:
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			case TV_BOOTS:
			case TV_GLOVES:
			case TV_HELM:
			case TV_CROWN:
			case TV_SHIELD:
			case TV_CLOAK:
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			{
				okay = TRUE;
				break;
			}
		}

		/* Skip objects */
		if (!okay) continue;

		/* It already has a discount or special inscription */
		if (o_ptr->discount > 0) continue;

		/* It has already been sensed, do not sense it again */
		if (o_ptr->ident & (IDENT_SENSE)) continue;

		/* It is fully known, no information needed */
		if (object_known_p(o_ptr)) continue;

		/* Get the inscription */
		feel = value_check_aux3(o_ptr);

		/* Sense something? */
		if (!feel) continue;

		/* Sense the object */
		o_ptr->discount = feel;

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);

	}

	/* Sense inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		int feel;

		bool okay = FALSE;

                object_type *o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;


		/* Valid "tval" codes */
		switch (o_ptr->tval)
		{
			case TV_SHOT:
			case TV_ARROW:
			case TV_BOLT:
			case TV_BOW:
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			case TV_BOOTS:
			case TV_GLOVES:
			case TV_HELM:
			case TV_CROWN:
			case TV_SHIELD:
			case TV_CLOAK:
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			{
				okay = TRUE;
				break;
			}
		}

		/* Skip objects */
		if (!okay) continue;

		/* It already has a discount or special inscription */
		if (o_ptr->discount > 0) continue;

		/* It has already been sensed, do not sense it again */
		if (o_ptr->ident & (IDENT_SENSE)) continue;

		/* It is fully known, no information needed */
		if (object_known_p(o_ptr)) continue;

		/* Get the inscription */
		feel = value_check_aux3(o_ptr);

		/* Sense something? */
		if (!feel) continue;

		/* Detected */
		detect = TRUE;

		/* Sense the object */
		o_ptr->discount = feel;

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of cursed objects!");
	}

	/* Return result */
	return (detect);
}



/*
 * Detect all "normal" monsters on the current panel
 */
bool detect_monsters_normal(void)
{
	int i, y, x;

	bool flag = FALSE;


	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/* Detect all non-invisible monsters */
		if (!(r_ptr->flags2 & (RF2_INVISIBLE)))
		{
			/* Optimize -- Repair flags */
			repair_mflag_mark = repair_mflag_show = TRUE;

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
		msg_print("You sense the presence of monsters!");
	}

	/* Result */
	return (flag);
}


/*
 * Detect all "invisible" monsters on current panel
 */
bool detect_monsters_invis(void)
{
	int i, y, x;

	bool flag = FALSE;


	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/* Detect invisible monsters */
		if (r_ptr->flags2 & (RF2_INVISIBLE))
		{
			/* Take note that they are invisible */
			l_ptr->r_flags2 |= (RF2_INVISIBLE);

			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Optimize -- Repair flags */
			repair_mflag_mark = repair_mflag_show = TRUE;

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
 * Detect all "evil" monsters on current panel
 */
bool detect_monsters_evil(void)
{
	int i, y, x;

	bool flag = FALSE;


	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/* Detect evil monsters */
		if (r_ptr->flags3 & (RF3_EVIL))
		{
			/* Take note that they are evil */
			l_ptr->r_flags3 |= (RF3_EVIL);

			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Optimize -- Repair flags */
			repair_mflag_mark = repair_mflag_show = TRUE;

			/* Detect the monster */
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
		msg_print("You sense the presence of evil creatures!");
	}

	/* Result */
	return (flag);
}

/*
 * Detect all "undead" monsters on current panel
 */
bool detect_monsters_undead(void)
{
	int i, y, x;

	bool flag = FALSE;


	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/* Detect evil monsters */
		if (r_ptr->flags3 & (RF3_UNDEAD))
		{
			/* Take note that they are evil */
			l_ptr->r_flags3 |= (RF3_UNDEAD);

			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Optimize -- Repair flags */
			repair_mflag_mark = repair_mflag_show = TRUE;

			/* Detect the monster */
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
		msg_print("You sense the presence of undead creatures!");
	}

	/* Result */
	return (flag);
}

/*
 * Detect all "animal" monsters on current panel
 */
bool detect_monsters_animal(void)
{
	int i, y, x;

	bool flag = FALSE;


	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/* Detect evil monsters */
		if (r_ptr->flags3 & (RF3_ANIMAL))
		{
			/* Take note that they are evil */
			l_ptr->r_flags3 |= (RF3_ANIMAL);

			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Optimize -- Repair flags */
			repair_mflag_mark = repair_mflag_show = TRUE;

			/* Detect the monster */
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
		msg_print("You sense the presence of natural creatures!");
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
	if (detect_doors()) detect = TRUE;
	if (detect_traps()) detect = TRUE;
	if (detect_stairs()) detect = TRUE;
	if (detect_treasure()) detect = TRUE;
	if (detect_objects_buried()) detect = TRUE;
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

	/* Create a staircase */
	if (p_ptr->depth == min_depth(p_ptr->dungeon))
	{
		cave_set_feat(py, px, FEAT_MORE);
	}
	else if (is_quest(p_ptr->depth) || (p_ptr->depth >= max_depth(p_ptr->dungeon)))
	{
		cave_set_feat(py, px, FEAT_LESS);
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
static bool item_tester_hook_weapon(object_type *o_ptr)
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
static bool item_tester_hook_armour(object_type *o_ptr)
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


static bool item_tester_unknown(object_type *o_ptr)
{
	if (object_known_p(o_ptr))
		return FALSE;
	else
		return TRUE;
}

static int unknown_tval;

static bool item_tester_unknown_tval(object_type *o_ptr)
{
	if(o_ptr->tval != unknown_tval) return FALSE;

	if (object_known_p(o_ptr))
		return FALSE;
	else
		return TRUE;
}


static bool item_tester_unknown_bonus(object_type *o_ptr)
{
	if (object_known_p(o_ptr))
		return FALSE;
	else if ((o_ptr->ident & (IDENT_BONUS)) && (o_ptr->ident & (IDENT_SENSE)))
		return FALSE;
	else
		return TRUE;
}


static bool item_tester_unknown_star(object_type *o_ptr)
{
	if (o_ptr->ident & IDENT_MENTAL)
		return FALSE;
	else
		return TRUE;
}

/*
 * Note for the following function, the player must know it is an artifact
 * or ego item, either by sensing or identifying it.
 */
static bool item_tester_known_name(object_type *o_ptr)
{
	if (o_ptr->ident & IDENT_MENTAL)
		return FALSE;
        else if ((!object_known_p(o_ptr))
                && !(o_ptr->discount == INSCRIP_SPECIAL)
                && !(o_ptr->discount == INSCRIP_EXCELLENT)
                && !(o_ptr->discount == INSCRIP_SUPERB)
                && !(o_ptr->discount == INSCRIP_WORTHLESS)
                && !(o_ptr->discount == INSCRIP_TERRIBLE))
		return FALSE;
	else if (!(o_ptr->name1) && !(o_ptr->name2))
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
bool enchant(object_type *o_ptr, int n, int eflag)
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
	if ((o_ptr->tval == TV_BOLT) ||
	    (o_ptr->tval == TV_ARROW) ||
	    (o_ptr->tval == TV_SHOT))
	{
		prob = prob / 20;
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
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_h++;

				/* Break curse */
				if (cursed_p(o_ptr) &&
				    (!(f3 & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_h >= 0) && (rand_int(100) < 25))
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
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_d++;

				/* Break curse */
				if (cursed_p(o_ptr) &&
				    (!(f3 & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_d >= 0) && (rand_int(100) < 25))
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
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_a++;

				/* Break curse */
				if (cursed_p(o_ptr) &&
				    (!(f3 & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_a >= 0) && (rand_int(100) < 25))
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

        /* Hack --- unsense the item */
        o_ptr->ident &= ~(IDENT_SENSE);        

	/* Remove special inscription, if any */
	if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

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

	cptr q, s;


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
	object_desc(o_name, o_ptr, FALSE, 0);

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
		if (flush_failure) flush();

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


	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

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
 * Identify the bonus/charges of an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was identified, else FALSE.
 */
bool ident_spell_bonus(void)
{
	int item;

	object_type *o_ptr;

	char o_name[80];

	cptr q, s;


	/* Only un-id'ed items */
	item_tester_hook = item_tester_unknown_bonus;

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

	/* Identify it's bonuses */
	object_bonus(o_ptr);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

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
 * Reveal some powers of a known artifact or ego item.
 * Returns TRUE if something was attempted, else FALSE.
 */
bool ident_spell_name(void)
{
	int item;

	object_type *o_ptr;

	cptr p, q, r, s;

	int i;

	bool done;

        u32b f1,f2,f3;

	/* Only un-id'ed items */
	item_tester_hook = item_tester_known_name;

	/* Get an item */
	q = "Learn legends about which item? ";
	s = "You have nothing legendary to examine.";
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

	/* Pick an interesting phrase */
	switch (randint(6))
	{
		case 1:
			p="Hmm... these runes look interesting..";
                        r=". but they tell you nothing more.";
			break;
		case 2:
			p="You recall tales fitting an item of this description";
                        r=", and they are bawdy and dull.";
			break;
		case 3:
			p="Ancient visions fill your mind..";
			r=". and give you a headache.";
			break;
		case 4:
			p="The maker's mark is strangely familiar";
			r="... oh, it's just a smudge of dirt.";
			break;
		case 5:
			p="The item glows with faint enchantments";
                        r=", snaps, crackles and pops.";
			break;
                default:
			p="Ah... the memories..";
			r=". things were always worse than you remember them.";
			break;
	}

        /* Examine the item */
        object_flags(o_ptr, &f1, &f2, &f3);

	/* Remove known flags */
	f1 &= ~(o_ptr->i_object.can_flags1);
	f2 &= ~(o_ptr->i_object.can_flags2);
	f3 &= ~(o_ptr->i_object.can_flags3);	

	/* We know everything? */
	done = ((f1 | f2 | f3) ? FALSE : TRUE);

	/* Clear some flags1 */
	if (f1)	for (i = 0;i<32;i++)
	{
		if ((f1 & (1L<<i)) && (rand_int(100)<25)) f1 &= ~(1L<<i);
	}

	/* Clear some flags2 */
	if (f2)	for (i = 0;i<32;i++)
	{
		if ((f2 & (1L<<i)) && (rand_int(100)<25)) f2 &= ~(1L<<i);
	}

	/* Clear some flags3 */
	if (f3)	for (i = 0;i<32;i++)
	{
                if ((f3 & (1L<<i)) && (rand_int(100)<25)) f3 &= ~(1L<<i);
	}

	
	if (done || (f1 | f2 | f3))
	{
		char o_name[80];

		/* Tell the player the good news */
		msg_format("%s.",p);

		if (done)
		{
			/* Do we know absolutely everything? */
			if (object_known_p(o_ptr)) object_mental(o_ptr);
			else
			{
				object_known(o_ptr);
				object_aware(o_ptr);
			}

		}

		/* Learn more about the item */
		object_can_flags(o_ptr,f1,f2,f3);

		/* Description */
		object_desc(o_name, o_ptr, TRUE, 3);

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
	}
	else
	{
		/* Tell the player the bad news */
                msg_format("%s%s.",p,r);
	}

	if (done)
	{
		/* Tell the player to stop trying */
		msg_format("You feel you know all %s secrets.",(o_ptr->number>0?"their":"its"));

	}
	else if (f1 | f2 | f3)
	{
		cptr info[128];

		int j,k;

		/* Describe the new stuff learnt */
                k= identify_fully_desc(info,f1,f2,f3);

		for (j = 0; j < k; j++)
		{
			msg_format("%s",info[j]);
		}

	}

	/* Something happened */
	return (TRUE);
}



/*
 * Identify an object in the inventory (or on the floor)
 * of a specified tval only.
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was identified, else FALSE.
 */
bool ident_spell_tval(int tval)
{
	int item;

	object_type *o_ptr;

	char o_name[80];

	cptr q, s;


	/* Restrict items */
	unknown_tval = tval;

	/* Only un-id'ed items */
	item_tester_hook = item_tester_unknown_tval;

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


	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

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

	cptr q, s;


	/* Only un-*id*'ed items */
	item_tester_hook = item_tester_unknown_star;

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


	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);
	object_mental(o_ptr);

#ifdef ALLOW_OBJECT_INFO

	/* Clear may flags */
	clear_may_flags();

#endif

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Handle stuff */
	handle_stuff();

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

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
	identify_fully_aux(o_ptr);

	/* Success */
	return (TRUE);
}




/*
 * Hook for "get_item()".  Determine if something is rechargable.
 */
static bool item_tester_hook_recharge(object_type *o_ptr)
{
	/* Recharge staffs */
	if (o_ptr->tval == TV_STAFF) return (TRUE);

	/* Recharge wands */
	if (o_ptr->tval == TV_WAND) return (TRUE);

	/* Hack -- Recharge rods */
	if (o_ptr->tval == TV_ROD) return (TRUE);

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
	int i, t, item, lev;

	object_type *o_ptr;

	cptr q, s;


	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item */
	q = "Recharge which item? ";
	s = "You have nothing to recharge.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

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


	/* Extract the object "level" */
	lev = k_info[o_ptr->k_idx].level;

	/* Recharge a rod */
	if (o_ptr->tval == TV_ROD)
	{
		/* Extract a recharge power */
		i = (100 - lev + num) / 5;

		/* Back-fire */
		if ((i <= 1) || (rand_int(i) == 0))
		{
			/* Hack -- backfire */
			msg_print("The recharge backfires, draining the rod further!");

			/* Hack -- decharge the rod */
			if (o_ptr->pval < 10000) o_ptr->pval = (o_ptr->pval + 100) * 2;
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

                /* Hack -- round up */
                o_ptr->stackc = 0;

	}

	/* Recharge wand/staff */
	else
	{
		/* Recharge power */
		i = (num + 100 - lev - (10 * o_ptr->pval)) / 15;

		/* Back-fire XXX XXX XXX */
		if ((i <= 1) || (rand_int(i) == 0))
		{
			/* Dangerous Hack -- Destroy the item */
			msg_print("There is a bright flash of light.");

			/* Reduce and describe inventory */
			if (item >= 0)
			{
				inven_item_increase(item, -999);
				inven_item_describe(item);
				inven_item_optimize(item);
			}

			/* Reduce and describe floor item */
			else
			{
				floor_item_increase(0 - item, -999);
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

			/* Hack -- we no longer "know" the item */
			o_ptr->ident &= ~(IDENT_KNOWN);

			/* Hack -- we no longer think the item is empty */
			o_ptr->ident &= ~(IDENT_EMPTY);

                        /* Hack -- round up */
                        o_ptr->stackc = 0;

		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);

	/* Something was done */
	return (TRUE);
}








/*
 * Apply a "project()" directly to all viewable monsters
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 */
static bool project_hack(int typ, int dam)
{
	int i, x, y;

	int flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;

	bool obvious = FALSE;


	/* Affect all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Require line of sight */
		if (!player_has_los_bold(y, x)) continue;

		/* Jump directly to the target monster */
		if (project(-1, 0, y, x, dam, typ, flg)) obvious = TRUE;
	}

	/* Result */
	return (obvious);
}


/*
 * Speed monsters
 */
bool speed_monsters(void)
{
	return (project_hack(GF_OLD_SPEED, p_ptr->lev));
}

/*
 * Slow monsters
 */
bool slow_monsters(void)
{
	return (project_hack(GF_OLD_SLOW, p_ptr->lev));
}

/*
 * Sleep monsters
 */
bool sleep_monsters(void)
{
	return (project_hack(GF_OLD_SLEEP, p_ptr->lev));
}

/*
 * Confuse monsters
 */
bool confuse_monsters(void)
{
	return (project_hack(GF_OLD_CONF, p_ptr->lev));
}

/*
 * Fear monsters
 */
bool fear_monsters(void)
{
	return (project_hack(GF_TURN_ALL, p_ptr->lev));
}


/*
 * Banish evil monsters
 */
bool banish_evil(int dist)
{
	return (project_hack(GF_AWAY_EVIL, dist));
}


/*
 * Turn undead
 */
bool turn_undead(void)
{
	return (project_hack(GF_TURN_UNDEAD, p_ptr->lev));
}


/*
 * Dispel undead monsters
 */
bool dispel_undead(int dam)
{
	return (project_hack(GF_DISP_UNDEAD, dam));
}

/*
 * Dispel evil monsters
 */
bool dispel_evil(int dam)
{
	return (project_hack(GF_DISP_EVIL, dam));
}

/*
 * Dispel all monsters
 */
bool dispel_monsters(int dam)
{
	return (project_hack(GF_DISP_ALL, dam));
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
	if (speed) msg_print("You feel a sudden stirring nearby!");
	else if (sleep) msg_print("You hear a sudden stirring in the distance!");
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
	(void)(get_com("Choose a monster race (by symbol) to genocide: ", &typ));

	/* Delete the monsters of that "type" */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
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
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > MAX_SIGHT) continue;

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
		if (!m_ptr->r_idx) continue;

		/* Require line of sight */
		if (!player_has_los_bold(m_ptr->fy, m_ptr->fx)) continue;

		/* Probe visible monsters */
		if (m_ptr->ml)
		{
			char m_name[80];

			/* Start the message */
			if (!probe) msg_print("Probing...");

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
		msg_print("That's all.");
	}

	/* Result */
	return (probe);
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
			cave_info[y][x] &= ~(CAVE_ROOM);

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
		if (!p_ptr->resist_blind && !p_ptr->resist_lite)
		{
			/* Become blind */
			(void)set_blind(p_ptr->blind + 10 + randint(10));
#ifdef ALLOW_OBJECT_INFO
			/* Always notice */
			equip_not_flags(0x0L,TR2_RES_BLIND,0x0L);

			/* Always notice */
			equip_not_flags(0x0L,TR2_RES_LITE,0x0L);
#endif

		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
			/* Sometimes notice */
			if ((p_ptr->resist_blind) && (rand_int(100)<30)) equip_can_flags(0x0L,TR2_RES_BLIND,0x0L);

			/* Sometimes notice */
			if ((p_ptr->resist_lite) && (rand_int(100)<30)) equip_can_flags(0x0L,TR2_RES_LITE,0x0L);
#endif
		}
	}


	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Fully update the flow */
	p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
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
 * the level in the same way that they do when genocided.
 *
 * Note that players and monsters (except eaters of walls and passers
 * through walls) will never occupy the same grid as a wall (or door).
 */
void earthquake(int cy, int cx, int r)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, t, y, x, yy, xx, dy, dx;

	int damage = 0;

	int sn = 0, sy = 0, sx = 0;

	bool hurt = FALSE;

	bool map[32][32];


	/* Paranoia -- Enforce maximum range */
	if (r > 12) r = 12;

	/* Clear the "maximal blast" area */
	for (y = 0; y < 32; y++)
	{
		for (x = 0; x < 32; x++)
		{
			map[y][x] = FALSE;
		}
	}

	/* Check around the epicenter */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;

			/* Skip illegal grids */
			if (!in_bounds_fully(yy, xx)) continue;

			/* Skip distant grids */
			if (distance(cy, cx, yy, xx) > r) continue;

			/* Lose room and vault */
			cave_info[yy][xx] &= ~(CAVE_ROOM);

			/* Lose light and knowledge */
			cave_info[yy][xx] &= ~(CAVE_GLOW | CAVE_MARK);

			/* Skip the epicenter */
			if (!dx && !dy) continue;

			/* Skip most grids */
			if (rand_int(100) < 85) continue;

			/* Damage this grid */
			map[16+yy-cy][16+xx-cx] = TRUE;

			/* Hack -- Take note of player damage */
			if ((yy == py) && (xx == px)) hurt = TRUE;
		}
	}

	/* First, affect the player (if necessary) */
	if (hurt)
	{
		/* Check around the player */
		for (i = 0; i < 8; i++)
		{
			/* Get the location */
			y = py + ddy_ddd[i];
			x = px + ddx_ddd[i];

			/* Skip non-empty grids */
			if (!cave_empty_bold(y, x)) continue;

			/* Important -- Skip "quake" grids */
			if (map[16+y-cy][16+x-cx]) continue;

			/* Count "safe" grids, apply the randomizer */
			if ((++sn > 1) && (rand_int(sn) != 0)) continue;

			/* Save the safe location */
			sy = y; sx = x;
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
					damage = damroll(10, 4);
					(void)set_stun(p_ptr->stun + randint(50));
					break;
				}
				case 3:
				{
					msg_print("You are crushed between the floor and ceiling!");
					damage = damroll(10, 4);
					(void)set_stun(p_ptr->stun + randint(50));
					break;
				}
			}

			/* Move player */
			monster_swap(py, px, sy, sx);
		}

		/* Take some damage */
		if (damage) take_hit(damage, "an earthquake");
	}


	/* Examine the quaked region */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;

			/* Skip unaffected grids */
			if (!map[16+yy-cy][16+xx-cx]) continue;

			/* Process monsters */
			if (cave_m_idx[yy][xx] > 0)
			{
				monster_type *m_ptr = &m_list[cave_m_idx[yy][xx]];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Most monsters cannot co-exist with rock */
				if (!(r_ptr->flags2 & (RF2_KILL_WALL)) &&
				    !(r_ptr->flags2 & (RF2_PASS_WALL)))
				{
					char m_name[80];

					/* Assume not safe */
					sn = 0;

					/* Monster can move to escape the wall */
					if (!(r_ptr->flags1 & (RF1_NEVER_MOVE)))
					{
						/* Look for safety */
						for (i = 0; i < 8; i++)
						{
							/* Get the grid */
							y = yy + ddy_ddd[i];
							x = xx + ddx_ddd[i];

							/* Skip non-empty grids */
							if (!cave_empty_bold(y, x)) continue;

							/* Hack -- no safety on glyph of warding */
							if (cave_feat[y][x] == FEAT_GLYPH) continue;

							/* Important -- Skip "quake" grids */
							if (map[16+y-cy][16+x-cx]) continue;

							/* Count "safe" grids, apply the randomizer */
							if ((++sn > 1) && (rand_int(sn) != 0)) continue;

							/* Save the safe grid */
							sy = y;
							sx = x;
						}
					}

					/* Describe the monster */
					monster_desc(m_name, m_ptr, 0);

					/* Scream in pain */
					msg_format("%^s wails out in pain!", m_name);

					/* Take damage from the quake */
					damage = (sn ? damroll(4, 8) : (m_ptr->hp + 1));

					/* Monster is certainly awake */
					m_ptr->csleep = 0;

					/* Apply damage directly */
					m_ptr->hp -= damage;

					/* Delete (not kill) "dead" monsters */
					if (m_ptr->hp < 0)
					{
						/* Message */
						msg_format("%^s is embedded in the rock!", m_name);

						/* Delete the monster */
						delete_monster(yy, xx);

						/* No longer safe */
						sn = 0;
					}

					/* Hack -- Escape from the rock */
					if (sn)
					{
						/* Move the monster */
						monster_swap(yy, xx, sy, sx);
					}
				}
			}
		}
	}


	/* XXX XXX XXX */

	/* New location */
	py = p_ptr->py;
	px = p_ptr->px;

	/* Important -- no wall on player */
	map[16+py-cy][16+px-cx] = FALSE;


	/* Examine the quaked region */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;

			/* Skip unaffected grids */
			if (!map[16+yy-cy][16+xx-cx]) continue;

			/* Paranoia -- never affect player */
			if ((yy == py) && (xx == px)) continue;

			/* Destroy location (if valid) */
			if (cave_valid_bold(yy, xx))
			{
				int feat = FEAT_FLOOR;

				bool floor = cave_floor_bold(yy, xx);

				/* Delete objects */
				delete_object(yy, xx);

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
				cave_set_feat(yy, xx, feat);
			}
		}
	}


	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Fully update the flow */
	p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Update the health bar */
	p_ptr->redraw |= (PR_HEALTH);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
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
 * NORMAL monsters wake up 3/4 the time when illuminated
 * STUPID monsters wake up 3/10 the time when illuminated
 *
 * Percentages were adjusted up, to make a greater change
 * of waking up monsters.
 */
static void cave_temp_room_lite(void)
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
			int chance = 75;

			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Stupid monsters rarely wake up */
			if (r_ptr->flags2 & (RF2_STUPID)) chance = 30;

			/* Smart monsters always wake up */
			if (r_ptr->flags2 & (RF2_SMART)) chance = 100;

			/* Sometimes monsters wake up */
			if (m_ptr->csleep && (rand_int(100) < chance))
			{
				/* Wake up! */
				m_ptr->csleep = 0;

				/* Notice the "waking up" */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Get the monster name */
					monster_desc(m_name, m_ptr, 0);

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
        int i,ii;

	/* Apply flag changes */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		/* No longer in the array */
		cave_info[y][x] &= ~(CAVE_TEMP);

		/* Darken the grid */
                if (!(f_info[cave_feat[y][x]].flags2 & (FF2_GLOW)))
		{
			cave_info[y][x] &= ~(CAVE_GLOW);
		}

		/* Check to see if not illuminated by innately glowing grids */
                for (ii = 0; ii < 8; ii++)
		{
                        int yy = y + ddy_ddd[ii];
                        int xx = x + ddx_ddd[ii];

			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

                        if (f_info[cave_feat[yy][xx]].flags2 & (FF2_GLOW))
                        {
				/* Illuminate the grid */
				cave_info[y][x] |= (CAVE_GLOW);

			}

		}

		/* Hack -- Forget "boring" grids */
		if (!(cave_info[y][x] & (CAVE_GLOW)) && 
			!(f_info[cave_feat[y][x]].flags1 & (FF1_REMEMBER)))
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
void lite_room(int y1, int x1)
{
	int i, x, y;

	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);

	/* While grids are in the queue, add their neighbors */
        for (i = 0; ((i < temp_n) && (i<TEMP_MAX)); i++)
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
	cave_temp_room_lite();

        /* Hack --- Have we seen this room before? */
        if (!(room_info[dun_room[y1/BLOCK_HGT][x1/BLOCK_WID]].flags & (ROOM_SEEN)))
        {
                p_ptr->update |= (PU_ROOM_INFO);
                p_ptr->window |= (PW_ROOM_INFO);
        }

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
        for (i = 0; ((i < temp_n) && (i<TEMP_MAX)); i++)
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
	cave_temp_room_unlite();
}


/*
 * Hack -- lower water around the player to none (earth/geothermal floor)
 * Affect all water monsters in the projection radius
 */
bool lower_water(int dam, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_GRID | PROJECT_KILL | PROJECT_HIDE;

	/* Hook into the "project()" function */
	(void)project(-1, rad, py, px, dam, GF_SHALLOW, flg);

	/* Assume seen */
	return (TRUE);
}

/*
 * Hack -- raise water around the player to deep
 * Affect all water monsters in the projection radius
 */
bool raise_water(int dam, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_GRID | PROJECT_KILL | PROJECT_HIDE;

	/* Hook into the "project()" function */
	(void)project(-1, rad, py, px, dam, GF_DEEP, flg);

	/* Assume seen */
	return (TRUE);
}


/*
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
bool lite_area(int dam, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("You are surrounded by a white light.");
	}

	/* Hook into the "project()" function */
	(void)project(-1, rad, py, px, dam, GF_LITE_WEAK, flg);

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
	(void)project(-1, rad, py, px, dam, GF_DARK_WEAK, flg);

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
bool fire_ball(int typ, int dir, int dam, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int ty, tx;

	int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	/* Use the given direction */
	ty = py + 99 * ddy[dir];
	tx = px + 99 * ddx[dir];

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		flg &= ~(PROJECT_STOP);

		ty = p_ptr->target_row;
		tx = p_ptr->target_col;
	}

	/* Analyze the "dir" and the "target".  Hurt items on floor. */
	return (project(-1, rad, ty, tx, dam, typ, flg));
}


/*
 * Hack -- apply a "projection()" in a direction (or at the target)
 */
static bool project_hook(int typ, int dir, int dam, int flg)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int ty, tx;

	/* Pass through the target if needed */
	flg |= (PROJECT_THRU);

	/* Use the given direction */
	ty = py + ddy[dir];
	tx = px + ddx[dir];

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		ty = p_ptr->target_row;
		tx = p_ptr->target_col;
	}

	/* Analyze the "dir" and the "target", do NOT explode */
	return (project(-1, 0, ty, tx, dam, typ, flg));
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a "bolt"
 * Affect monsters (not grids or objects)
 */
bool fire_bolt(int typ, int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(typ, dir, dam, flg));
}

/*
 * Cast a beam spell
 * Pass through monsters, as a "beam"
 * Affect monsters (not grids or objects)
 */
bool fire_beam(int typ, int dir, int dam)
{
	int flg = PROJECT_BEAM | PROJECT_KILL;
	return (project_hook(typ, dir, dam, flg));
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
 * Cast a blast spell
 * A blast spell is a radius 1 ball spell that only fires to adjacent
 * squares. Used for a couple of alchemy spells.
 */
bool fire_blast(int typ, int dir, int dam)
{
	int ty = p_ptr->py+ddy[dir];
	int tx = p_ptr->px+ddx[dir];

	int flg = PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM;
	return (project(-1, 1, ty, tx, dam, typ, flg));

}

/*
 * Affect an adjacent monster with an attack 
 */
bool fire_touch(int typ, int dir, int dam)
{
	int ty = p_ptr->py +ddy[dir];
	int tx = p_ptr->px +ddx[dir];

	return (project_m(-1, 0, ty, tx, dam, typ));
}

/*
 * Some of the old functions
 */

bool lite_line(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
	return (project_hook(GF_LITE_WEAK, dir, damroll(6, 8), flg));
}

bool raise_bridge(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
        return (project_hook(GF_BRIDGE, dir, 1, flg));
}


bool drain_life(int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_DRAIN, dir, dam, flg));
}

bool water_to_air(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
        return (project_hook(GF_SHALLOW, dir, 20 + randint(30), flg));
}


bool wall_to_mud(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(GF_KILL_WALL, dir, 20 + randint(30), flg));
}

bool destroy_door(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_DOOR, dir, 0, flg));
}

bool disarm_trap(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_TRAP, dir, 0, flg));
}

bool heal_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_HEAL, dir, damroll(4, 6), flg));
}

bool speed_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_SPEED, dir, p_ptr->lev, flg));
}

bool slow_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_SLOW, dir, p_ptr->lev, flg));
}

bool sleep_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_SLEEP, dir, p_ptr->lev, flg));
}

bool confuse_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_CONF, dir, plev, flg));
}

bool blind_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
        return (project_hook(GF_BLIND, dir, damroll(3,8), flg));
}


bool poly_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_POLY, dir, p_ptr->lev, flg));
}

bool clone_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_CLONE, dir, 0, flg));
}

bool fear_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_TURN_ALL, dir, plev, flg));
}

bool teleport_monster(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_KILL;
	return (project_hook(GF_AWAY_ALL, dir, MAX_SIGHT * 5, flg));
}



/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */

bool door_creation(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 1, py, px, 0, GF_MAKE_DOOR, flg));
}

bool trap_creation(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 1, py, px, 0, GF_MAKE_TRAP, flg));
}

bool destroy_doors_touch(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 1, py, px, 0, GF_KILL_DOOR, flg));
}

bool sleep_monsters_touch(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_KILL | PROJECT_HIDE;
	return (project(-1, 1, py, px, p_ptr->lev, GF_OLD_SLEEP, flg));
}
