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
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		/* Heal 0-4 */
		if (num < 5)
		{
			msg_print("You feel a little better.");
		}

		/* Heal 5-14 */
		else if (num < 20)
		{
			msg_print("You feel better.");
		}

		/* Heal 15-34 */
		else if (num < 45)
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
	bool usedup;
	object_type *o_ptr;
	int py = p_ptr->py;
	int px = p_ptr->px;

	if (cave_feat[py][px] != FEAT_FLOOR)
	{
		msg_print("There is no clear floor on which to cast the spell.");
		return;
	}

	/* Create a glyph */
	cave_set_feat(py, px, FEAT_GLYPH);

   	usedup = TRUE;
   	/* reading rune of protection from the floor */
   	if (spellswitch == 3) usedup = FALSE;
   	
	/* Shift any objects to further away */
	for (o_ptr = get_first_object(py, px); o_ptr; o_ptr = get_next_object(o_ptr))
	{
       	/* don't move the scroll of rune of protection because it should be used up */
        if ((o_ptr->tval == TV_SCROLL) && (o_ptr->sval == SV_SCROLL_RUNE_OF_PROTECTION) && (!usedup))
       	{
		   usedup = TRUE;
        }
        else drop_near(o_ptr, 0, py, px);
	}

	/* Delete the "moved" objects from their original position */
	delete_object(py, px);
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
bool do_dec_stat(int stat, int loses)
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
	
	/* monsters with powerful flag have a chance to get past sustains */
	if ((loses > 0) && (goodluck < 21))
	{
       if (randint(100) < loses) sust = FALSE;
    }

	/* Sustain */
	if (sust)
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
 * Done by a potion of "self knowledge".
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
		if (object_known_p(o_ptr)) continue;

		/* Identify it */
		do_ident_item(i, o_ptr);

		/* repeat with same slot */
		/* i--; */
	}
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

	/* Mark as uncursed */
	o_ptr->pseudo = INSCRIP_UNCURSED;

	/* The object has been "sensed" */
	o_ptr->ident |= (IDENT_SENSE);
}


/*
 * Removes curses from items in inventory.
 *
 * Note that Items which are "Perma-Cursed" (The One Ring,
 * The Crown of Morgoth) can NEVER be uncursed.
 * ..that's no longer true, instead they re-curse themselves.
 *
 * Note that if "all" is FALSE, then Items which are
 * "Heavy-Cursed" (Mormegil, Calris, and Weapons of Morgul)
 * will not be uncursed.
 */
static int remove_curse_aux(int all)
{
	int i, cnt = 0;
	int fail = 0;
	int failstop, failgo;

	/* Attempt to uncurse items being worn */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		u32b f1, f2, f3, f4;

		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Uncursed already */
		if (!cursed_p(o_ptr)) continue;

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3, &f4);

		/* Heavily Cursed Items need a special spell */
		/* DJA: Perma-Cursed can now be uncursed */
        /* but they recurse themselves automatically */
		if (!all && (f3 & (TR3_HEAVY_CURSE))) continue;
		if (!all && (f3 & (TR3_PERMA_CURSE))) continue;

        /* corruption makes it harder to remove curses */
        failstop = 70 + (goodluck * 3);
        if (all) failstop += 30;
        if ((f3 & (TR3_PERMA_CURSE)) && (p_ptr->corrupt < 20)) failgo = 25;
        else if (f3 & (TR3_PERMA_CURSE)) failgo = p_ptr->corrupt + 6;
        else failgo = p_ptr->corrupt + 1;
        if (((p_ptr->corrupt > 0) || (f3 & (TR3_PERMA_CURSE))) &&
           (randint(failstop) < failgo))
        {
            if (f3 & (TR3_PERMA_CURSE)) fail = 2; /* failed because of PERMA_CURSE */
		    else fail = 1; /* failed because of corruption */
        }
        else
        {
		    /* Uncurse the object */
		    uncurse_object(o_ptr);
        }

		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);

		/* Count the uncursings */
		cnt++;
	}
	
	if (fail == 2)
	{
      msg_print("You feel there's a curse that won't be lifted that easily.");
    }
	else if (fail == 1) msg_print("You feel curses clinging to your corruption.");

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
 * See also "identify_fully()".
 *
 * This tests the flags of the equipment being carried and the innate player
 * flags, so any changes made in calc_bonuses need to be shadowed here.
 *
 * Use the "roff()" routines, perhaps.  XXX XXX XXX
 *
 * Use the "show_file()" method, perhaps.  XXX XXX XXX
 *
 * This function cannot display more than 20 lines.  XXX XXX XXX
 */
void self_knowledge(bool spoil)
{
	int i = 0, j, k;

	u32b t1, t2, t3, t4;

	u32b f1 = 0L, f2 = 0L, f3 = 0L, f4 = 0L;

	object_type *o_ptr;

	cptr info[128];


	/* Get item flags from equipment */
	for (k = INVEN_WIELD; k < INVEN_TOTAL; k++)
	{
		o_ptr = &inventory[k];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

#ifdef EFG
		/* EFGchange !selfknowledge will *ID everything being wielded */
		if (spoil)
		{
			if (!object_aware_p(o_ptr))
				object_aware(o_ptr);
			if (!object_known_p(o_ptr))
				object_known(o_ptr);
			o_ptr->ident |= IDENT_MENTAL;
		}
#endif
		/* Extract the flags */
		if (spoil)
			object_flags(o_ptr, &t1, &t2, &t3, &t4);
		else 
			object_flags_known(o_ptr, &t1, &t2, &t3, &t4);

		/* Extract flags */
		f1 |= t1;
		f2 |= t2;
		f3 |= t3;
		f4 |= t4;
	}

	/* And flags from the player */
	player_flags(&t1, &t2, &t3, &t4);

	/* Extract flags */
	f1 |= t1;
	f2 |= t2;
	f3 |= t3;
	f4 |= t4;


	if (p_ptr->timed[TMD_BLIND])
	{
		info[i++] = "You cannot see.";
	}
	if (p_ptr->timed[TMD_BRAIL])
	{
		info[i++] = "You can spellcasting bonuses (and can read while blind).";
	}
	if (p_ptr->timed[TMD_CONFUSED])
	{
		info[i++] = "You are confused.";
	}
	if (p_ptr->timed[TMD_AFRAID])
	{
		info[i++] = "You are terrified.";
	}
	if (p_ptr->timed[TMD_CHARM])
	{
		info[i++] = "You are in too good a mood to fight.";
	}
	if (p_ptr->timed[TMD_FRENZY])
	{
		info[i++] = "You are in a mad careless frenzy.";
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
	if (p_ptr->timed[TMD_STONESKIN])
	{
		info[i++] = "Your skin is hard as stone.";
	}
	if (p_ptr->timed[TMD_TERROR])
	{
		info[i++] = "You are desperate to escape!!!";
	}
	if (p_ptr->timed[TMD_IMAGE])
	{
		info[i++] = "You are hallucinating.";
	}
	if (p_ptr->aggravate)
	{
		info[i++] = "You aggravate monsters.";
	}
	else if (f3 & TR3_AGGRAVATE)
	{
		info[i++] = "Your stealth is severely drained.";
	}
	if ((p_ptr->telecontrol) && (f3 & TR3_TELEPORT))
	{
		info[i++] = "You have teleport control and random teleportation.";
	}
	else if (p_ptr->telecontrol)
	{
		info[i++] = "You have teleport control.";
	}
	else if (f3 & TR3_TELEPORT)
	{
		info[i++] = "Your position is very uncertain.";
	}
	if (p_ptr->timed[TMD_SKILLFUL])
	{
		info[i++] = "Your skills are boosted.";
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
	if (p_ptr->timed[TMD_SANCTIFY])
	{
		info[i++] = "You strike with holy wrath.";
	}
	if (p_ptr->timed[TMD_BALROG])
	{
		info[i++] = "You stike with a balrog's shadow and flame.";
	}
	if (p_ptr->timed[TMD_BECOME_LICH])
	{
		info[i++] = "You are a semi-lich (temporarily undead).";
    }
	if (p_ptr->timed[TMD_PROTDEAD])
	{
		info[i++] = "You are protected from lifeless monsters.";
	}
	if (p_ptr->timed[TMD_PROTEVIL2])
	{
		info[i++] = "You are protected from powerful evil.";
	}
	else if (p_ptr->timed[TMD_PROTEVIL])
	{
		info[i++] = "You are protected from evil.";
	}
	if (p_ptr->timed[TMD_WSHIELD])
	{
		info[i++] = "You are protected by a mystic shield.";
	}
	if (p_ptr->timed[TMD_SHADOW])
	{
		info[i++] = "You are wrapped in shadow.";
	}
	if (p_ptr->timed[TMD_SHIELD])
	{
		info[i++] = "You are protected by a powerful mystic shield.";
	}
	if (p_ptr->timed[TMD_INVULN])
	{
		info[i++] = "You are temporarily invulnerable.";
	}
	if (p_ptr->timed[TMD_CURSE])
	{
		info[i++] = "You have a temporary curse on your combat.";
	}
	if (p_ptr->timed[TMD_WITCH])
	{
		info[i++] = "Your black magic is aggravating demons.";
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

	if (p_ptr->timed[TMD_DAYLIGHT])
	{
		info[i++] = "Daylight surrounds you even in the dungeon.";
	}
	if (p_ptr->timed[TMD_TSIGHT])
	{
		info[i++] = "You see everything that is really there.";
	}
	else if (p_ptr->timed[TMD_SINFRA])
	{
		info[i++] = "You have enhanced alertness.";
	}
	else if (p_ptr->timed[TMD_WSINFRA])
	{
		info[i++] = "Your alertness is slightly enhanced.";
	}

	if (f3 & TR3_SLOW_DIGEST)
	{
		info[i++] = "Your appetite is small.";
	}
	if (f3 & TR3_FEATHER)
	{
		info[i++] = "You land gently.";
	}
	if (f3 & TR3_REGEN)
	{
		info[i++] = "You regenerate quickly.";
	}
	if (f3 & (TR3_STOPREGEN))
	{
		info[i++] = "Your wounds do not heal naturally.";
	}
    if (p_ptr->timed[TMD_2ND_THOUGHT])
	{
		info[i++] = "You have first sight and second thoughts.";
	}
    if ((f3 & TR3_TELEPATHY) && (p_ptr->timed[TMD_2ND_THOUGHT]))
	{
		info[i++] = "Your second sight is being supressed by your first sight.";
	}
	else if (f3 & TR3_TELEPATHY)
	{
		info[i++] = "You have ESP.";
	}
	if (p_ptr->darkvis || (p_ptr->timed[TMD_DARKVIS]))
	{
		info[i++] = "You have darkvision.";
	}
	if (p_ptr->timed[TMD_SUPER_ROGUE])
	{
		info[i++] = "You have enhanced roguish skills.";
    }
	if ((p_ptr->breath_shield) && (p_ptr->timed[TMD_BR_SHIELD]))
	{
		info[i++] = "You have extra damage reduction against monster breath.";
	}
	else if ((p_ptr->breath_shield) || (p_ptr->timed[TMD_BR_SHIELD]))
	{
		info[i++] = "You have damage reduction against monster breath.";
	}
	
	if (f3 & TR3_SEE_INVIS)
	{
		info[i++] = "You can see invisible creatures.";
	}
	if (f3 & TR3_FREE_ACT)
	{
		info[i++] = "You have free action.";
	}
	if ((f3 & TR3_HOLD_LIFE) || (p_ptr->timed[TMD_HOLDLIFE]))
	{
		info[i++] = "You have a firm hold on your life force.";
	}

	if (p_ptr->timed[TMD_XATTACK])
	{
		info[i++] = "You have temporarily enhanced attack speed.";
	}
	if (p_ptr->timed[TMD_MIGHTY_HURL])
	{
		info[i++] = "You have temporary mighty throwing ability.";
	}
	if (p_ptr->timed[TMD_SUST_SPEED])
	{
		info[i++] = "Your speed is sustained.";
	}
	if (p_ptr->timed[TMD_SPHERE_CHARM])
	{
		info[i++] = "You have a sphere of charm animals around you.";
	}
	if (p_ptr->timed[TMD_ZAPPING])
	{
		info[i++] = "You have an electrical field around you, zapping monsters.";
	}

	if (f2 & TR2_IM_ACID)
	{
		info[i++] = "You are completely immune to acid.";
	}
	else if ((f2 & TR2_RES_ACID) && (p_ptr->timed[TMD_OPP_ACID]))
	{
		info[i++] = "You resist acid exceptionally well.";
	}
	else if ((f2 & TR2_RES_ACID) || (p_ptr->timed[TMD_OPP_ACID]))
	{
		info[i++] = "You are resistant to acid.";
	}

	if (f2 & TR2_IM_ELEC)
	{
		info[i++] = "You are completely immune to lightning.";
	}
	else if ((f2 & TR2_RES_ELEC) && (p_ptr->timed[TMD_OPP_ELEC]))
	{
		info[i++] = "You resist lightning exceptionally well.";
	}
	else if ((f2 & TR2_RES_ELEC) || (p_ptr->timed[TMD_OPP_ELEC]))
	{
		info[i++] = "You are resistant to lightning.";
	}

	if ((f2 & TR2_IM_FIRE) || (p_ptr->timed[TMD_IMM_FIRE]))
	{
		info[i++] = "You are completely immune to fire.";
	}
	else if ((f2 & TR2_RES_FIRE) && (p_ptr->timed[TMD_OPP_FIRE]))
	{
		info[i++] = "You resist fire exceptionally well.";
	}
	else if ((f2 & TR2_RES_FIRE) || (p_ptr->timed[TMD_OPP_FIRE]))
	{
		info[i++] = "You are resistant to fire.";
	}

	if ((f2 & TR2_IM_COLD) || (p_ptr->timed[TMD_BECOME_LICH]))
	{
		info[i++] = "You are completely immune to cold.";
	}
	else if ((f2 & TR2_RES_COLD) && (p_ptr->timed[TMD_OPP_COLD]))
	{
		info[i++] = "You resist cold exceptionally well.";
	}
	else if ((f2 & TR2_RES_COLD) || (p_ptr->timed[TMD_OPP_COLD]))
	{
		info[i++] = "You are resistant to cold.";
	}

	if ((f4 & TR4_RES_POIS) && (p_ptr->timed[TMD_OPP_POIS]))
	{
		info[i++] = "You resist poison exceptionally well.";
	}
	else if ((f4 & TR4_RES_POIS) || (p_ptr->timed[TMD_OPP_POIS]))
	{
		info[i++] = "You are resistant to poison.";
	}
	else if ((f2 & TR2_PR_POIS) || (p_ptr->timed[TMD_WOPP_POIS]))
	{
		info[i++] = "You are somewhat resistant to poison.";
	}

	if (p_ptr->accident)
	{
		info[i++] = "Your weapon is easy to hurt yourself with.";
	}

	if (f4 & TR4_RES_FEAR)
	{
		info[i++] = "You are completely fearless.";
	}
	
	if (f4 & TR4_RES_CHARM)
	{
		info[i++] = "You are resistant to charm.";
	}	

	if (f2 & TR2_PEACE)
	{
		info[i++] = "You are not agressive in combat.";
	}

	if (f2 & TR2_NICE)
	{
		info[i++] = "Animals and light fairies are less aggresive towards you.";
	}

	if (f4 & TR4_RES_LITE)
	{
		info[i++] = "You are resistant to bright light.";
	}
	if ((f4 & TR4_RES_DARK) || (p_ptr->timed[TMD_OPP_DARK]))
	{
		info[i++] = "You are resistant to darkness.";
	}
	if (f4 & TR4_RES_BLIND)
	{
		info[i++] = "Your eyes are resistant to blindness.";
	}
	if ((f4 & TR4_RES_CONFU) || (p_ptr->timed[TMD_CLEAR_MIND]))
	{
		info[i++] = "You are resistant to confusion.";
	}
	if (f4 & TR4_RES_SOUND)
	{
		info[i++] = "You are resistant to sonic attacks.";
	}
	if (f4 & TR4_RES_SHARD)
	{
		info[i++] = "You are resistant to blasts of shards.";
	}
	if (f4 & TR4_RES_NEXUS)
	{
		info[i++] = "You are resistant to nexus attacks.";
	}
	if ((f4 & TR4_RES_NETHR) || (p_ptr->timed[TMD_OPP_NETHR]))
	{
		info[i++] = "You are resistant to nether forces.";
	}
	if (f4 & TR4_RES_CHAOS)
	{
		info[i++] = "You are resistant to chaos.";
	}
	if (f4 & TR4_RES_DISEN)
	{
		info[i++] = "You are resistant to disenchantment.";
	}

	if (f2 & TR2_SUST_STR)
	{
		info[i++] = "Your strength is sustained.";
	}
	if (f2 & TR2_SUST_INT)
	{
		info[i++] = "Your intelligence is sustained.";
	}
	if (f2 & TR2_SUST_WIS)
	{
		info[i++] = "Your wisdom is sustained.";
	}
	if (f2 & TR2_SUST_DEX)
	{
		info[i++] = "Your dexterity is sustained.";
	}
	if (f2 & TR2_SUST_CON)
	{
		info[i++] = "Your constitution is sustained.";
	}
	if (f2 & TR2_SUST_CHR)
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
	if (f1 & (TR1_INFRA))
	{
		info[i++] = "Your alertness is affected by your equipment.";
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

	/* reset flags so current weapon part won't include stuff from other equipment */
	f1 = 0L, f2 = 0L, f3 = 0L, f4 = 0L;

	/* Get the current weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Extract the flags */
	if (spoil)
		object_flags(o_ptr, &t1, &t2, &t3, &t4);
	else 
		object_flags_known(o_ptr, &t1, &t2, &t3, &t4);

	/* Analyze the weapon */
	if (o_ptr->k_idx)
	{
		/* Special "Attack Bonuses" */
		if (f1 & (TR1_BRAND_ACID))
		{
			info[i++] = "Your weapon melts your foes.";
		}
		else if (p_ptr->brand_acid)
		{
			info[i++] = "Your ring adds caustic damage to your blows.";
		}
		/* For some reason it was always saying this so I turned it off */
/* 		if (f2 & (TR2_COAT_ACID))
		{
			info[i++] = "Your weapon corrodes your foes.";
		} */
		if (f1 & (TR1_BRAND_ELEC))
		{
			info[i++] = "Your weapon shocks your foes.";
		}
		else if (p_ptr->brand_elec)
		{
			info[i++] = "Your ring adds lightning to your blows.";
		}
		if (f1 & (TR1_BRAND_FIRE))
		{
			info[i++] = "Your weapon burns your foes.";
		}
		else if (p_ptr->brand_fire)
		{
			info[i++] = "Your ring adds fire to your blows.";
		}
		if (f1 & (TR1_BRAND_COLD))
		{
			info[i++] = "Your weapon freezes your foes.";
		}
		else if (p_ptr->brand_cold)
		{
			info[i++] = "Your ring adds a freezing chill to your blows.";
		}
		if (f1 & (TR1_BRAND_POIS))
		{
			info[i++] = "Your weapon poisons your foes.";
		}
		else if (p_ptr->brand_pois) /* may add a ring of poison */
		{
			info[i++] = "Your ring adds poison to your blows.";
		}

		if (f2 & (TR2_EXTRA_CRIT))
		{
			info[i++] = "You inflict critical hits extra often.";
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
			info[i++] = "Your weapon is especially deadly against undead.";
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
		if (f2 & (TR2_SLAY_BUG))
		{
			info[i++] = "Your weapon is especially deadly against bugs.";
		}
		if (f2 & (TR2_SLAY_LITE))
		{
			info[i++] = "Your weapon is especially deadly against creatures of light.";
		}
		if (f2 & (TR2_SLAY_SILVER))
		{
			info[i++] = "Your weapon strikes at silver monsters with holy wrath.";
		}
		if (f1 & (TR1_SLAY_GIANT))
		{
			info[i++] = "Your weapon is especially deadly against giants.";
		}
		if (f1 & (TR1_SLAY_DRAGON))
		{
			info[i++] = "Your weapon is especially deadly against dragons.";
		}

		/* Special "kill" flags */
		if (f1 & (TR1_KILL_DRAGON))
		{
			info[i++] = "Your weapon is a great bane of dragons.";
		}
		if (f1 & (TR1_KILL_DEMON))
		{
			info[i++] = "Your weapon is a great bane of demons.";
		}
		if (f1 & (TR1_KILL_UNDEAD))
		{
			info[i++] = "Your weapon is a great bane of undead.";
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

	/* luck level */
    if (goodluck > 16)
	{
		info[i++] = "You are extremely lucky.";
	}
	else if (goodluck > 10)
	{
		info[i++] = "You are very lucky.";
	}
	else if (goodluck > 4)
	{
		info[i++] = "You are lucky.";
	}
	else if (goodluck > 0)
	{
		info[i++] = "You are somewhat lucky.";
	}
	if (badluck > 15)
	{
		info[i++] = "You are extremely unlucky.";
	}
	else if (badluck > 9)
	{
		info[i++] = "You are very unlucky.";
	}
	else if (badluck > 4)
	{
		info[i++] = "You are unlucky.";
	}
	else if (badluck > 0)
	{
		info[i++] = "You are somewhat unlucky.";
	}

	/* slime / silver poison levels */
    if (p_ptr->silver >= PY_SILVER_VERYBAD)
	{
		info[i++] = "Your mind is almost fully corrupted by silver poison!";
	}
    else if (p_ptr->silver >= PY_SILVER_LEVELTWO)
	{
		info[i++] = "You have a dangerous level of silver poison.";
	}
    else if (p_ptr->silver >= PY_SILVER_LEVELONE)
	{
		info[i++] = "You have an unhealthy level of silver poison.";
	}
    if (p_ptr->slime >= PY_SLIME_VERYBAD)
	{
		info[i++] = "Any more sliming and your body will become a jelly!";
	}
    else if (p_ptr->slime >= PY_SLIME_LEVELTWO)
	{
		info[i++] = "You have a dangerous level of sliming.";
	}
    else if (p_ptr->slime >= PY_SLIME_LEVELONE)
	{
		info[i++] = "You have an unhealthy level of sliming.";
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
 * Set word of recall as appropriate
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

        if (spellswitch == 8) p_ptr->word_recall = rand_int(5) + 3;
		else p_ptr->word_recall = rand_int(20) + 15;
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
bool detect_traps(void)
{
	int y, x;

	bool detect = FALSE;


	/* Scan the current panel */
	for (y = Term->offset_y; y < Term->offset_y + SCREEN_HGT; y++)
	{
		for (x = Term->offset_x; x < Term->offset_x + SCREEN_WID; x++)
		{
			if (!in_bounds_fully(y, x)) continue;

			/* Detect invisible traps */
			if (cave_feat[y][x] == FEAT_INVIS)
			{
				/* Pick a trap */
				pick_trap(y, x);
			}

			/* Detect traps */
			if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
			    (cave_feat[y][x] <= FEAT_TRAP_TAIL))
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
		msg_print("You sense the presence of traps!");
	}

	/* Result */
	return (detect);
}



/*
 * Detect all doors on current panel
 */
bool detect_doors(void)
{
	int y, x;

	bool detect = FALSE;


	/* Scan the panel */
	for (y = Term->offset_y; y < Term->offset_y + SCREEN_HGT; y++)
	{
		for (x = Term->offset_x; x < Term->offset_x + SCREEN_WID; x++)
		{
			if (!in_bounds_fully(y, x)) continue;

			/* Detect secret doors */
			if (cave_feat[y][x] == FEAT_SECRET)
			{
				/* Pick a door */
				place_closed_door(y, x);
			}

			/* Detect doors */
			if (((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
			     (cave_feat[y][x] <= FEAT_DOOR_TAIL)) ||
			    ((cave_feat[y][x] == FEAT_OPEN) ||
			     (cave_feat[y][x] == FEAT_BROKEN)))
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
		msg_print("You sense the presence of doors!");
	}

	/* Result */
	return (detect);
}


/*
 * Detect all stairs on current panel
 */
bool detect_stairs(void)
{
	int y, x;

	bool detect = FALSE;


	/* Scan the panel */
	for (y = Term->offset_y; y < Term->offset_y + SCREEN_HGT; y++)
	{
		for (x = Term->offset_x; x < Term->offset_x + SCREEN_WID; x++)
		{
			if (!in_bounds_fully(y, x)) continue;

			/* Detect stairs */
			if ((cave_feat[y][x] == FEAT_LESS) ||
			    (cave_feat[y][x] == FEAT_MORE))
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
 * Detect any treasure on the current panel
 */
bool detect_treasure(void)
{
	int y, x;

	bool detect = FALSE;


	/* Scan the current panel */
	for (y = Term->offset_y; y < Term->offset_y + SCREEN_HGT; y++)
	{
		for (x = Term->offset_x; x < Term->offset_x + SCREEN_WID; x++)
		{
			if (!in_bounds_fully(y, x)) continue;

			/* Notice embedded gold */
			if ((cave_feat[y][x] == FEAT_MAGMA_H) ||
			    (cave_feat[y][x] == FEAT_QUARTZ_H))
			{
				/* Expose the gold */
				cave_feat[y][x] += 0x02;
			}

			/* Magma/Quartz + Known Gold */
			if ((cave_feat[y][x] == FEAT_MAGMA_K) ||
			    (cave_feat[y][x] == FEAT_QUARTZ_K))
			{
				/* Hack -- Memorize */
				cave_info[y][x] |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Detect */
				detect = TRUE;
			}
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of buried treasure!");
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
			o_ptr->marked = TRUE;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			if (!squelch_hide_item(o_ptr))
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

		/* Artifacts, misc magic items, or enchanted wearables */
		/* spellbooks of foreign realms no longer count */
		if (artifact_p(o_ptr) || ego_item_p(o_ptr) ||
		    (tv == TV_AMULET) || (tv == TV_RING) ||
		    (tv == TV_STAFF) || (tv == TV_WAND) || (tv == TV_ROD) ||
		    (tv == TV_SCROLL) || (tv == TV_POTION) ||
		    (tv == cp_ptr->spell_book) ||
		    ((o_ptr->to_a > 3) || (o_ptr->to_h + o_ptr->to_d > 4)))
		{
			/* Memorize the item */
			o_ptr->marked = TRUE;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			if (!squelch_hide_item(o_ptr))
				detect = TRUE;
		}
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
 * Detect all "normal" monsters on the current panel
 */
bool detect_monsters_normal(void)
{
	int i, y, x;

	bool flag = FALSE;


	/* Scan monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
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

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/* Detect invisible monsters */
		if ((r_ptr->flags2 & (RF2_INVISIBLE)) || (m_ptr->tinvis))
		{
			/* Take note that they are invisible */
			if (r_ptr->flags2 & (RF2_INVISIBLE)) l_ptr->flags2 |= (RF2_INVISIBLE);
			else if (m_ptr->tinvis) l_ptr->flags6 |= (RF6_INVIS);

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

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/* Detect evil monsters */
		if (m_ptr->evil)
		{
			/* Take note about evil monster race flags  */
			if (r_ptr->flags3 & (RF3_EVIL))
            {
                l_ptr->flags3 |= (RF3_EVIL);
            }
			else 
			{
			    if (r_ptr->flags2 & (RF2_S_EVIL2)) l_ptr->flags2 |= (RF2_S_EVIL2);
			    else if (r_ptr->flags2 & (RF2_S_EVIL1)) l_ptr->flags2 |= (RF2_S_EVIL1);

			    /* remember that this individual monster is evil (but the race isn't always) */
			    m_ptr->meet = 4;
            }

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
 * Detect all living monsters on current panel
 */
bool detect_monsters_life(void)
{
	int i, y, x;

	bool flag = FALSE;

	/* Scan monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/* Detect living monsters */
		if (!(r_ptr->flags3 & (RF3_NON_LIVING)) &&
		   !(r_ptr->flags3 & (RF3_UNDEAD)))
		{
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
		msg_print("You sense the presence of living creatures.");
	}

	/* Result */
	return (flag);
}

/*
 * Detect all animals (and bugs) on current panel
 */
bool detect_monsters_animal(void)
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

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/* Detect animals */
		if ((r_ptr->flags3 & (RF3_ANIMAL)) || (r_ptr->flags3 & (RF3_BUG)))
		{
			/* Take note that they are animals or bugs */
			l_ptr->flags3 |= (RF3_ANIMAL);
			l_ptr->flags3 |= (RF3_BUG);

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
		msg_print("You sense the presence of animals.");
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
	if (detect_treasure()) detect = TRUE;
	if (detect_objects_gold()) detect = TRUE;
	if (detect_objects_normal()) detect = TRUE;
	if (detect_monsters_invis()) detect = TRUE;
	if (detect_monsters_normal()) detect = TRUE;

	/* Result */
	return (detect);
}



/*
 * Create stairs (usually) at the player location
 * (dis is 0 except for find vault spell and chance version of stair creation)
 */
void stair_creation(int dis)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int i, x, y, d, min;
	bool look;
	
	/* XXX XXX XXX */
	if (!cave_valid_bold(py, px) && (!dis))
	{
		dis = 4;
	}

	/* XXX XXX XXX */
	/* delete_object(py, px); */

	/* Look until done */
	if (dis)
	{
	  look = TRUE;

	  /* Initialize */
	  y = py;
	  x = px;

	  /* Minimum distance */
	  min = dis / 2;

	  while (look)
	  {
		/* Verify max distance */
		if (dis > 200) dis = 200;

		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
			/* Pick a (possibly illegal) location */
			while (1)
			{
				y = rand_spread(py, dis);
				x = rand_spread(px, dis);
				d = distance(py, px, y, x);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds_fully(y, x)) continue;

			/* Require "naked" floor space */
			if (!cave_naked_bold(y, x)) continue;

			/* No creating stairs in vaults */
			if (cave_info[y][x] & (CAVE_ICKY)) continue;

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* Increase the maximum distance */
		dis = dis * 2;

		/* Decrease the minimum distance */
		min = min / 2;
      }

	  /* create the stairs somewhere else */
      py = y;
	  px = x;
	}


	/* Create a staircase */
	if (!p_ptr->depth)
	{
		cave_set_feat(py, px, FEAT_MORE);
	}
	else if (is_quest(p_ptr->depth) || (p_ptr->depth >= MAX_DEPTH-1))
	{
		cave_set_feat(py, px, FEAT_LESS);
	}
	else if ((rand_int(100) < 50) || ((dis) && (p_ptr->depth < 11)))
	{
		cave_set_feat(py, px, FEAT_MORE);
	}
	else
	{
		cave_set_feat(py, px, FEAT_LESS);
	}
}


/*
 * Hook to specify an item which can be blessed
 */
static bool item_tester_hook_bless(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		/* anything which can be wielded as a weapon */
        case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_STAFF:
		case TV_SKELETON:

		/* bows and lights */
		/* (but can't use bless object spell to enchant ammo) */
		case TV_BOW:
        case TV_LITE:

		/* any armor */
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
			return (TRUE);
		}
	}

	return (FALSE);
}



/*
 * Hook to specify "weapon"
 */
static bool item_tester_hook_weapon(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_BOW:
		case TV_ARROW:
		case TV_BOLT:
		case TV_SHOT:
		case TV_STAFF:
		case TV_SKELETON:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify bow or arrows
 */
static bool item_tester_hook_archer(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_BOW:
        {
            if ((o_ptr->sval == SV_LONG_BOW) || (o_ptr->sval == SV_SHORT_BOW) ||
               (o_ptr->sval == SV_GREAT_BOW))
            {
               return (TRUE);
            }
        }
		case TV_ARROW:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "armour"
 */
static bool item_tester_hook_armour(const object_type *o_ptr)
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
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		{
			u32b f1, f2, f3, f4;
			/* check for WIELD_SHIELD flag (mainly for main gauche) */
			object_flags(o_ptr, &f1, &f2, &f3, &f4);
			if (f4 & TR4_WIELD_SHIELD) return (TRUE);
		}
	}

	return (FALSE);
}


static bool item_tester_unknown(const object_type *o_ptr)
{
        /* able to identify weapon stats on a staff */
#ifdef EFG
	/* EFGchange show charges on aware unknown */
	if ((o_ptr->tval == TV_WAND) && (object_aware_p(o_ptr)))
		return FALSE;
#endif
	if (object_known_p(o_ptr))
		return FALSE;
	else
		return TRUE;
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
bool enchant(object_type *o_ptr, int n, int eflag)
{
	int i, chance, prob, lift;

	bool res = FALSE;

	bool a = artifact_p(o_ptr);

	u32b f1, f2, f3, f4;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);


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

        /* PERMA_CURSEs can now be lifted but not easily */
        lift = 25;
        if (f3 & (TR3_PERMA_CURSE)) lift -= 16;
        if (f3 & (TR3_HEAVY_CURSE)) lift -= badluck/3;

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
				    (o_ptr->to_h >= 0) && (rand_int(100) < lift))
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
				    (o_ptr->to_d >= 0) && (rand_int(100) < lift))
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
				    (o_ptr->to_a >= 0) && (rand_int(100) < lift))
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
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Success */
	return (TRUE);
}


/*
 * Bless an object (not always a weapon)
 * has a chance of nullifing the BAD_WEAP flag
 */
bool bless_weapon(int power)
{
	int item;
	cptr q, s;
	u32b f1, f2, f3, f4;
	int something, resistb, plus;
	bool weapon, bow, lite;

	object_type *o_ptr;
	char o_name[80];

	item_tester_hook = item_tester_hook_bless;

	/* Get an item */
	q = "Bless which item? ";
	s = "You have nothing to bless.";  
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
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 1);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Describe */
	msg_format("A white light touches %s %s",
	           ((item >= 0) ? "Your" : "The"), o_name);
	           
	something = 0;
	resistb = 1;
	if (!o_ptr->blessed)
	{
	   if (f3 & (TR3_BAD_WEAP)) resistb = 20 + badluck/2 + o_ptr->pval*2;
	   if (f2 & (TR2_CORRUPT)) resistb += 15;
    }
    /* this spell doesn't directly remove curses */
    /* but can uncurse by enchanting the item */
    if (cursed_p(o_ptr))
    {
	   if ((f3 & (TR3_HEAVY_CURSE)) && (resistb < 15)) resistb = 15;
	   else if (f3 & (TR3_HEAVY_CURSE)) resistb += 10;
	   else /* light curse */ resistb += 5;
    }
	
	weapon = (wield_slot(o_ptr) == INVEN_WIELD);
	bow = (wield_slot(o_ptr) == INVEN_BOW);
	lite = (wield_slot(o_ptr) == INVEN_LITE);
	/* if none of these then it's armor */

	/* Narsil is only thing with both CORRUPT and BLESSED */
	/* and it's easier to un-CORRUPT */
	if ((f2 & (TR2_CORRUPT)) && (f2 & (TR3_BLESSED))) resistb = 1;

	/* fail on artifacts with heavy curses or BAD_WEAP flag */
	/* can work on other artifacts */
    if ((broken_p(o_ptr)) || ((artifact_p(o_ptr)) && (resistb > 9) && (power + (goodluck/2) < 47)))
    {
        if (artifact_p(o_ptr)) msg_print("The powerful evil resists enchantment");
        else msg_print("The blessing fails");
        /* blessing failed but the attempt still uses mana */
	    return TRUE;
    }

    /* sometimes some trace of the curse remains */
    if ((f3 & (TR3_HEAVY_CURSE)) && (!cursed_p(o_ptr)) && (randint(100) < 19)) resistb += (randint(2) * 5);

    /* can bless egos or artifacts */
    /* but they have a chance to resist if they don't have GOOD_WEAP */
    if ((artifact_p(o_ptr)) && (!f3 & (TR3_GOOD_WEAP)) && (resistb < 30)) resistb = 30;
    else if ((ego_item_p(o_ptr)) && (!f3 & (TR3_GOOD_WEAP)) && (resistb < 15)) resistb = 15;
    else if ((artifact_p(o_ptr)) && (resistb < 20)) resistb = 20;
    else if ((ego_item_p(o_ptr)) && (resistb < 10)) resistb = 10;

	/* Narsil is only thing with CORRUPT and BLESSED and is easier to un-CORRUPT */
	if ((f2 & (TR2_CORRUPT)) && (f2 & (TR3_BLESSED))) resistb = (resistb*3)/5;
    
    /* paladins not as good at removing curses and evil alignment */
    if ((!cp_ptr->flags & CF_BLESS_WEAPON) && (resistb + 5 < 20)) resistb = 20;
    else if (!cp_ptr->flags & CF_BLESS_WEAPON) resistb += 5;
    
    /* blessing doesn't always work on lites (but lasts longer) */
    if ((lite) && (resistb < 2)) resistb += (randint(5) * 5);
    else if ((lite) && (resistb < 15)) resistb = 15;
    
    /* everything should have some small chance to be blessed (max power = plev) */
    if (resistb > 49) resistb = 49;

	/* Attempt to overcome BAD_WEAP and curses */
    if (resistb > power)
    {
        if (artifact_p(o_ptr)) msg_print("The artifact remains unaffected");
        else if (f3 & (TR3_BAD_WEAP)) msg_print("The evil enchantment resists blessing");
        else if (lite) msg_print("You fail to make the light brighter");
        else /* ego */ msg_print("The object's magic resists blessing");
        something = 2;
    }
	/* bless */
	else if (o_ptr->blessed <= 1)
    {
       if ((f3 & (TR3_BAD_WEAP)) && (!o_ptr->blessed))
       {
          msg_format("The evil enchantment on %s %s is lifted!",
           ((item >= 0) ? "Your" : "The"), o_name);
       }
       if (lite) o_ptr->blessed = 500 + (power * 100);
       else o_ptr->blessed = 500 + power * 50;
       something = 1;

       /* remove evil egos ("The evil enchantment is lifted") */
       /* (but not MORGUL or NAZGUL) */
       if ((o_ptr->name2 == EGO_WITCHCRAFT) || (o_ptr->name2 == EGO_DEMON_MIGHT) ||
          (o_ptr->name2 == EGO_BLOODLUST))
       {
           o_ptr->name2 = 0;
           /* "of witchcraft" has random sustain, so remove it also */
           o_ptr->xtra1 = 0;
           o_ptr->xtra2 = 0;
       }
    }
    else /* extend if already blessed */
    {
       if (lite) o_ptr->blessed += (power * (35 + goodluck));
       else o_ptr->blessed += (power * (25 + (goodluck/2)));
    }

    /* usually only enchants if you are at least L35 */
    if (p_ptr->lev > 34 - goodluck/4)
    {
       /* priests don't get as much combat enchantment as paladins do */
       if ((cp_ptr->flags & CF_BLESS_WEAPON) && (goodluck < 19))
           power -= (5 - goodluck/4);

       plus = 1;
       if (power > 19) plus = power / 10;
       if ((plus > 3) && (goodluck < 8)) plus = 2 + randint(plus - 2);

       if ((o_ptr->blessed) && ((weapon) || (bow)))
       {
	      /* Enchant */
	      if (enchant(o_ptr, plus, ENCH_TOHIT)) something = 1;
	      if (enchant(o_ptr, plus, ENCH_TODAM)) something = 1;
       }
       else if ((!lite) && (o_ptr->blessed)) /* armor */
       {
	      if (enchant(o_ptr, plus, ENCH_TOAC)) something = 1;
       }
    }

	/* nothing happened */
	if (something != 1)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message (if didn't already get a message) */
		if (something == 0) msg_format("%s %s seems unchanged.",
	           ((item >= 0) ? "Your" : "The"), o_name);
	           
	    return TRUE;
	}
	
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


	/* The archer spell can enchant only bows or arrows */
	if (spellswitch == 18)
    {
        item_tester_hook = item_tester_hook_archer;
    }
	else
	{
	    /* Assume enchant weapon */
	    item_tester_hook = item_tester_hook_weapon;
    }

    /* Enchant armor if requested */
    if (num_ac) item_tester_hook = item_tester_hook_armour;

	/* Get an item */
	q = "Enchant which item? ";
	if (spellswitch == 18) s = "You have no bow or arrows to enchant.";
	else s = "You have nothing to enchant.";
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
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 1);

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

    /* reset spellswitch */
    spellswitch = 0;

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


	/* Identify the object */
	do_ident_item(item, o_ptr);


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

	/* Identify the object */
	do_ident_item(item, o_ptr);

	/* Mark the item as fully known */
	o_ptr->ident |= (IDENT_MENTAL);

	/* Handle stuff */
	handle_stuff();

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
	if (o_ptr->tval == TV_STAFF) return (TRUE);

	/* Recharge wands */
	if (o_ptr->tval == TV_WAND) return (TRUE);

	/* Nope */
	return (FALSE);
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

	cptr q, s;


	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item */
	q = "Recharge which item? ";
	s = "You have nothing to recharge.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | USE_EQUIP))) return (FALSE);

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

	/* Recharge power */
	i = (num + 100 - lev - (10 * (o_ptr->pval / o_ptr->number))) / 15;

	/* Back-fire */
	if ((i <= 1) || (rand_int(i) == 0))
	{
		msg_print("The recharge backfires!");
		msg_print("There is a bright flash of light.");

		/* Reduce the charges of rods/wands/staves */
		reduce_charges(o_ptr, 1);

#ifdef removed_piece
        /* I don't want this to happen */
		/* *Identified* items keep the knowledge about the charges */
		if (!(o_ptr->ident & IDENT_MENTAL))
		{
			/* We no longer "know" the item */
			o_ptr->ident &= ~(IDENT_KNOWN);
		}
#endif

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

#ifdef removed_piece
        /* I don't want this to happen */
		/* *Identified* items keep the knowledge about the charges */
		if (!(o_ptr->ident & IDENT_MENTAL))
		{
			/* We no longer "know" the item */
			o_ptr->ident &= ~(IDENT_KNOWN);
		}
#endif

		/* We no longer think the item is empty */
		o_ptr->ident &= ~(IDENT_EMPTY);
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
bool project_los(int typ, int dam)
{
	int i, x, y;

	int flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;

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
	return (project_los(GF_OLD_SPEED, p_ptr->lev));
}

/*
 * Slow monsters
 */
bool slow_monsters(int plev)
{
	return (project_los(GF_OLD_SLOW, plev));
}

/*
 * Scare monsters
 */
bool scare_monsters(int plev)
{
	return (project_los(GF_TURN_ALL, plev));
}

/*
 * Sleep monsters
 */
bool sleep_monsters(int plev)
{
	return (project_los(GF_OLD_SLEEP, plev));
}

/*
 * Hold monsters
 */
bool hold_monsters(void)
{
    spellswitch = 29;
	return (project_los(GF_OLD_SLEEP, p_ptr->lev * 2));
}

/*
 * Mass Amnesia
 */
bool mass_amnesia(int power)
{
	return (project_los(GF_AMNESIA, power));
}


/*
 * Banish evil monsters
 */
bool banish_evil(int dist)
{
	return (project_los(GF_AWAY_EVIL, dist));
}

/*
 * Banish unnatural monsters
 */
bool banish_unnatural(int dist)
{
	return (project_los(GF_AWAY_UNDEAD, dist));
}

/*
 * Turn undead
 */
bool turn_undead(void)
{
    if (spellswitch == 15) return (project_los(GF_TURN_ALL, p_ptr->lev));
	else return (project_los(GF_TURN_UNDEAD, p_ptr->lev));
}


/*
 * Thunderclap
 */
bool dispel_ears(int dam)
{
	return (project_los(GF_SOUND, dam));
}

/*
 * Dispel undead monsters
 */
bool dispel_undead(int dam)
{
	return (project_los(GF_DISP_UNDEAD, dam));
}

/*
 * Dispel demons
 */
bool dispel_demon(int dam)
{
    spellswitch = 23; /* changes GF_DISP_UNDEAD to affect demons */
	return (project_los(GF_DISP_UNDEAD, dam));
}

/*
 * Bug Spray
 */
bool dispel_bug(int dam)
{
	return (project_los(GF_BUG_SPRAY, dam));
}

/*
 * Dispel silver monsters
 */
bool dispel_silver(int dam)
{
	return (project_los(GF_DISP_SILVER, dam));
}

/*
 * Dispel unnatural monsters
 */
bool dispel_unnatural(int dam)
{
	return (project_los(GF_DISP_UNN, dam));
}

/*
 * Dispel Life
 */
bool dispel_life(int dam)
{
    spellswitch = 27; /* makes OLD_DRAIN not affect silver monsters */
	return (project_los(GF_OLD_DRAIN, dam));
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
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip aggravating monster (or player) */
		if (i == who) continue;

		/* Wake up nearby sleeping monsters */
        if (cp_ptr->flags & CF_CLASS_SPEED)
        {
            /* thieves resist aggravation */
		    if (m_ptr->cdis < (MAX_SIGHT * 3) / 2)
		    {
			   /* Wake up */
			   if ((m_ptr->csleep) && (randint(100) < 95 - (m_ptr->cdis/2)))
			   {
				  /* Wake up */
				  m_ptr->csleep = 0;
				  m_ptr->roaming = 0;
				  sleep = TRUE;
			   }
		    }
        }
		/* Wake up nearby sleeping monsters */
        else
        {
		    if (m_ptr->cdis < MAX_SIGHT * 2)
		    {
			   /* Wake up */
			   if (m_ptr->csleep)
			   {
				  /* Wake up */
				  m_ptr->csleep = 0;
				  m_ptr->roaming = 0;
				  sleep = TRUE;
			   }
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
bool banishment(void)
{
	int i;
	unsigned dam = 0;

	char typ;

	/* Mega-Hack -- Get a monster symbol */
    if (spellswitch == 14) 
    {
       if (!get_com("Choose a monster type (by symbol) to summon: ", &typ))
		return FALSE;
    }
    else
    {
	   if (!get_com("Choose a monster race (by symbol) to banish: ", &typ))
		return FALSE;
    }

    if (spellswitch == 14) 
    {
           int py = p_ptr->py;
	       int px = p_ptr->px;
           int ny, nx, die, die2;
           die = randint(100);
           die2 = randint(100);
           if (die < 50) ny = py + randint(4);
           else ny = py - randint(4);
           if (die2 < 50) nx = px + randint(4);
           else nx = px - randint(4);
           
           summon_kin_type = typ;

           summon_specific(ny, nx, p_ptr->depth, SUMMON_KIN);
        
	       /* Update monster list window */
	       p_ptr->window |= PW_MONLIST;

	       /* Success- exits function */
	       return TRUE;
    }

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
		dam += randint(4);
	}

	/* Hurt the player */
	take_hit(dam, "the strain of casting Banishment");

	/* Update monster list window */
	p_ptr->window |= PW_MONLIST;

	/* Success */
	return TRUE;
}


/*
 * Delete all nearby (non-unique) monsters
 */
bool mass_banishment(void)
{
	int i;
	unsigned dam = 0;

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
		dam += randint(3);
	}

	/* Hurt the player */
	take_hit(dam, "the strain of casting Mass Banishment");

	/* Calculate result */
	result = (dam > 0) ? TRUE : FALSE;

	/* Update monster list window */
	if (result) p_ptr->window |= PW_MONLIST;

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
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

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
			monster_desc(m_name, sizeof(m_name), m_ptr, 0x04);

			/* Describe the monster */
			msg_format("%^s has %d hit points.", m_name, m_ptr->hp);

			/* Learn all of the non-spell, non-treasure flags */
			/* DJA: now includes spells */
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

	/* Unused parameter */
	(void)full;

	/* No effect in town */
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

			/* do not affect vaults */
			if (cave_info[y][x] & (CAVE_ICKY)) continue;
			if (cave_feat[y][x] >= FEAT_PERM_EXTRA) continue;

			/* Extract the distance */
			k = distance(y1, x1, y, x);

			/* Stay in the circle of death */
			if (k > r) continue;

			/* Lose room (not vault) */
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
			if (spellswitch == 11) (void)inc_timed(TMD_BLIND, 3 + randint(5));
			else if (spellswitch == 26) (void)inc_timed(TMD_BLIND, 1 + randint(2));
			else (void)inc_timed(TMD_BLIND, 10 + randint(10));
		}
	}


	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Fully update the flow */
	p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_MAP | PW_MONLIST);
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
void earthquake(int cy, int cx, int r)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, t, y, x, yy, xx, dy, dx;

	int damage = 0;
	
	int sn = 0, sy = 0, sx = 0;

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
			cave_info[yy][xx] &= ~(CAVE_ROOM | CAVE_ICKY);

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
		/* but not if spell is cast by the player */
		if ((!sn) && (spellswitch != 11) && (spellswitch != 26))
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
					(void)inc_timed(TMD_STUN, randint(50));
					break;
				}
				case 3:
				{
					msg_print("You are crushed between the floor and ceiling!");
					damage = damroll(10, 4);
					(void)inc_timed(TMD_STUN, randint(50));
					break;
				}
			}

			/* Move player */
			monster_swap(py, px, sy, sx);
		}

        /* extra damage reduction from surrounding magic */
		if (cp_ptr->flags & CF_POWER_SHIELD)
        {
           damage -= (damage * ((p_ptr->lev + 5) / 250));
        }
		else if (goodluck > 16)
        {
           damage -= (damage * (goodluck + 3 / 250));
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
				monster_type *m_ptr = &mon_list[cave_m_idx[yy][xx]];
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
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Scream in pain */
					msg_format("%^s wails out in pain!", m_name);

					/* Take damage from the quake */
					damage = (sn ? damroll(4, 8) : (m_ptr->hp + 1));

					/* Monster is certainly awake */
					m_ptr->csleep = 0;
					m_ptr->roaming = 0;

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
	p_ptr->window |= (PW_OVERHEAD | PW_MAP | PW_MONLIST);
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
	int i, dimness;

    /* how dim is the room's light? */
    int lmod = randint(50 - goodluck/2 + p_ptr->depth + p_ptr->corrupt);
    if (lmod < 30) dimness = 1;
    else if (lmod < 60) dimness = 2;
    else if (lmod > 99) dimness = 4;
    else dimness = 3;

	/* Apply flag changes */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		/* No longer in the array */
		cave_info[y][x] &= ~(CAVE_TEMP);

		/* Perma-Lite */
		cave_info[y][x] |= (CAVE_GLOW);
#if EXPM
        if (dimness == 0) cave_info[y][x] |= (DLIT_FULL);
        else if (dimness == 1) cave_info[y][x] |= (DLIT_DIMA);
        else if (dimness == 2) cave_info[y][x] |= (DLIT_DIMB);
        else if (dimness == 3) cave_info[y][x] |= (DLIT_DIMC);
        else /* dimness 4 */ cave_info[y][x] |= (DLIT_DIMD);
#endif
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
			int chance = 25;

			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Stupid monsters rarely wake up */
			if (r_ptr->flags2 & (RF2_STUPID)) chance = 10;

			/* Smart monsters always wake up */
			if (r_ptr->flags2 & (RF2_SMART)) chance = 100;
			
			if (m_ptr->roaming) chance += 25;

			/* Sometimes monsters wake up */
			if (m_ptr->csleep && (rand_int(100) < chance))
			{
				/* Notice the "waking up" */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Get the monster name */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Dump a message */
				    if (m_ptr->roaming) msg_format("%^s notices you.", m_name);
				    else msg_format("%^s wakes up.", m_name);
				}

				/* Wake up! */
				m_ptr->csleep = 0;
				m_ptr->roaming = 0;
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
 *
 * DJA: also to damage to any creatures of light in the room.
 */
static void cave_temp_room_unlite(void)
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
		if (cave_feat[y][x] <= FEAT_INVIS)
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
		
        /* is there a monster in the grid? */
        if (cave_m_idx[y][x] > 0)
        {
		   int dam = randint(11 + (p_ptr->depth/5));
		   int flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;

           /* Jump directly to the target monster */
		   (void)project(-1, 0, y, x, dam, GF_DARK_WEAK, flg);
        }
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
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
bool lite_area(int dam, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->timed[TMD_BLIND])
	{
		msg_print("You are surrounded by a white light.");
	}

	/* Lite up the room */
	if (spellswitch != 4) lite_room(py, px);

	/* Hook into the "project()" function */
	(void)project(-1, rad, py, px, dam, GF_LITE_WEAK, flg);

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
	if (!p_ptr->timed[TMD_BLIND])
	{
		msg_print("Darkness surrounds you.");
	}

	/* Hook into the "project()" function */
	if (spellswitch == 21) (void)project(-1, rad, py, px, dam, GF_DARK, flg);
	else (void)project(-1, rad, py, px, dam, GF_DARK_WEAK, flg);

	/* Darken the room */
	/* necromancer's 'call dark' only sometimes darkens the whole room */
	if (spellswitch == 21)
	{
       if (randint(100) < 15 + goodluck) unlite_room(py, px);
    }
    else unlite_room(py, px);

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

    /* spellswitch 28 used to center spell on the caster */
    if (spellswitch == 28)
    {
	   ty = py;
	   tx = px;
    }

	/* Analyze the "dir" and the "target".  Hurt items on floor. */
	return (project(-1, rad, ty, tx, dam, typ, flg));
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

	int py = p_ptr->py;
	int px = p_ptr->px;

	int ty, tx;

	int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	/* Use the given direction */
	ty = py + 99 * ddy[dir];
	tx = px + 99 * ddx[dir];

	/* Hack -- Use an actual "target" (early detonation) */
	if ((dir == 5) && target_okay())
	{
		ty = p_ptr->target_row;
		tx = p_ptr->target_col;
	}

	while (num--)
	{
		/* Analyze the "dir" and the "target".  Hurt items on floor. */
		if (project(-1, rad, ty, tx, dam, typ, flg)) noticed = TRUE;
	}

	return noticed;
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
 * Some of the old functions
 */

bool lite_line(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL | PROJECT_THRU;
	return (project_hook(GF_LITE_WEAK, dir, damroll(6, 8), flg));
}

bool strong_lite_line(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL | PROJECT_THRU;
	return (project_hook(GF_LITE, dir, damroll(10, 8), flg));
}

bool drain_life(int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_DRAIN, dir, dam, flg));
}

bool wall_to_mud(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_THRU;
	int dam = 25 + randint(25);
	if (spellswitch == 31) dam = 40 + randint(40);
	return (project_hook(GF_KILL_WALL, dir, dam, flg));
}

bool destroy_door(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_DOOR, dir, 0, flg));
}

bool disarm_trap(int dir, int mode)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_TRAP, dir, mode, flg));
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

bool slow_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_SLOW, dir, plev, flg));
}

bool sleep_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_SLEEP, dir, plev, flg));
}

bool confuse_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_CONF, dir, plev, flg));
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

bool teleport_monster(int dir, int dis)
{
	int flg;
	if (dis < 25) dis = 25;
	if ((dis > 100) && (goodluck < 11)) dis = 100;
	else if (dis > 125) dis = 125;
	flg = PROJECT_BEAM | PROJECT_KILL | PROJECT_THRU;
    return (project_hook(GF_AWAY_ALL, dir, dis, flg)); 
    /* return (project_hook(GF_AWAY_ALL, dir, MAX_SIGHT * 5, flg)); */
}

/*
 * For the monster spells: HEAL_OTHR and HEAL_KIN
 * (simulation of monster-cast project_los using GF_OLD_HEAL)
 * (doesn't actually use project() at all)
 *
 * if healmon is 0, then it only tests to see 
 * if there are nearby monsters to heal.
 */
bool heal_monsters(int healmon, const monster_type *m_ptr, bool kinonly)
{
	int i, x, y, d, cy, cx;
	char ally;
	int kinkind = 0;
	char n_name[80];
	char n_poss[80];
	monster_type *n_ptr;
	monster_race *r_ptr;

	int flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;

	bool healed = FALSE;
	bool endfear = FALSE;
	bool iskin = FALSE;
	bool onlygood = FALSE;

	/* what race is the caster? */
	r_ptr = &r_info[m_ptr->r_idx];
	ally = r_ptr->d_char;
	if (r_ptr->flags3 & (RF3_TROLL)) kinkind = 1;
	if (strchr("pKt", r_ptr->d_char)) kinkind = 2; /* all humans */
	if (strchr("oO", r_ptr->d_char)) kinkind = 3; /* orcs and ogres considered kin */
	if ((strchr("y", r_ptr->d_char)) || (r_ptr->flags3 & (RF3_HURT_DARK))) 
	{
		kinkind = 4; /* creatures of light */
	}
	/* hack: paladins and templar knights don't heal evil monsters */
	if ((m_ptr->r_idx == 619) || (m_ptr->r_idx == 319) || (m_ptr->r_idx == 561)) onlygood = TRUE;

	/* location of caster */
	cy = m_ptr->fy;
	cx = m_ptr->fx;

	/* Affect all (nearby) monsters */
	for (i = 1; i < mon_max; i++)
	{
		/* get target monster info */
		n_ptr = &mon_list[i];
		r_ptr = &r_info[n_ptr->r_idx];
		/* get target monster name (and possesive) */
		monster_desc(n_name, sizeof(n_name), n_ptr, 0);
		monster_desc(n_poss, sizeof(n_poss), n_ptr, 0x22);

		/* Paranoia -- Skip dead monsters */
		if (!n_ptr->r_idx) continue;

		/* Location of target */
		y = n_ptr->fy;
		x = n_ptr->fx;

		/* Require projectable from caster */
		d = distance(cy, cx, y, x);
		if ((d > MAX_RANGE - 1) || (!projectable(cy, cx, y, x))) continue;

		/* hurts undead */
		if (r_ptr->flags3 & (RF3_UNDEAD))
		{
			/* Hurt the monster */
			n_ptr->hp -= (healmon/2);

			/* Dead monster */
			if (n_ptr->hp < 0)
			{
				/* Generate treasure, etc */
				monster_death(cave_m_idx[y][x]);

				/* Delete the monster */
				delete_monster_idx(cave_m_idx[y][x]);

				/* Give detailed messages if destroyed */
				msg_format("%^s is destroyed", n_name);
			}

			/* Damaged monster */
			else
			{
				/* Give detailed messages if visible */
				if (n_ptr->ml) msg_format("%^s is damaged", n_name);
			}
			continue;
		}

		/* don't bother if monster doesn't need healing */
		if (n_ptr->hp == n_ptr->maxhp) continue;

		/* Heal_kin spell only heals similar monsters */
		if (kinonly)
		{
			int kinkindt = 0;
			if ((strchr("y", r_ptr->d_char)) || (r_ptr->flags3 & (RF3_HURT_DARK))) kinkindt = 4;
			/* light fairies also heal non-evil animals with HEAL_KIN */
			if ((!n_ptr->evil) && (r_ptr->flags3 & (RF3_ANIMAL))) kinkindt = 4;
			if (kinkind == kinkindt) iskin = TRUE;
			if (r_ptr->flags3 & (RF3_TROLL)) kinkindt = 1;
			if (strchr("pKt", r_ptr->d_char)) kinkindt = 2; /* all humans */
			if (strchr("oO", r_ptr->d_char)) kinkindt = 3; /* orcs and ogres considered kin */
			/* is the target monster kin? */
			if (kinkind == kinkindt) iskin = TRUE;
			/* same symbol is always kin */
			if (r_ptr->d_char == ally) iskin = TRUE;
			if (!iskin) continue;
		}

		/* hack: templar knights don't heal evil monsters */
		if ((n_ptr->evil) && (onlygood)) continue;

		/* !healmon == not actually casting the spell yet */
		/* TRUE == there is at least one monster nearby to heal */
		/* (but not if the only monster to heal is itself) */
		if ((!healmon) && (n_ptr != m_ptr)) return TRUE;
		else if (!healmon) continue;

		/* Wake up (usually) */
		if ((p_ptr->nice) && (!n_ptr->evil) &&
           (goodluck) && (randint(100) < 50) &&
	       ((r_ptr->flags3 & (RF3_HURT_DARK)) ||
	       (r_ptr->flags3 & (RF3_ANIMAL))))
	    {
            /* don't wake up */
        }   
		else if ((goodluck < 14) && (!n_ptr->charmed))
        {
			n_ptr->csleep = 0;
			n_ptr->roaming = 0;
		}

		/* Heal */
		healed = TRUE;
		n_ptr->hp += healmon;

		/* Message */
		if (n_ptr->hp < n_ptr->maxhp) msg_format("%^s looks healthier", n_name);

		/* No overflow */
		if (n_ptr->hp >= n_ptr->maxhp)
		{
			n_ptr->hp = n_ptr->maxhp;
			/* alternate message */
			msg_format("%^s looks fully healthy", n_name);
		}

		/* Redraw (later) if needed */
		if (p_ptr->health_who == cave_m_idx[y][x]) p_ptr->redraw |= (PR_HEALTH);

		/* always end fear if healed by an ally or "kin" */
		if ((kinonly) || (r_ptr->d_char == ally)) endfear = TRUE;
		/* Cancel fear */
		if (randint(100) < 60 + badluck - goodluck) endfear = TRUE;
		if ((n_ptr->monfear) && (endfear))
		{
			/* Cancel fear */
			n_ptr->monfear = 0;

			/* Message */
			msg_format("%^s recovers %s courage.", n_name, n_poss);
		}
	}

	/* Result */
	return (healed);
}


/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */

/* door creation mode (10=locked, 0=unlocked) */
bool door_creation(int mode)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 1, py, px, mode, GF_MAKE_DOOR, flg));
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

	if (spellswitch == 6) return (project(-1, 9, py, px, 0, GF_KILL_TRAP, flg));
	else return (project(-1, 1, py, px, 0, GF_KILL_DOOR, flg));
}

bool sleep_monsters_touch(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_KILL | PROJECT_HIDE;
	return (project(-1, 1, py, px, p_ptr->lev, GF_OLD_SLEEP, flg));
}


/*
 * Curse the players armor
 */
bool curse_armor(void)
{
	object_type *o_ptr;

	char o_name[80];

	int die = randint(101);

	/* Curse the body armor (DJA: added chance to affect other armor) */
	if (die < 7) o_ptr = &inventory[INVEN_OUTER];
	else if (die < 14) o_ptr = &inventory[INVEN_ARM];
	else if (die < 21) o_ptr = &inventory[INVEN_HEAD];
	else if (die < 28) o_ptr = &inventory[INVEN_HANDS];
	else if (die < 35) o_ptr = &inventory[INVEN_FEET];
	else o_ptr = &inventory[INVEN_BODY];
	

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

	/* Attempt a saving throw for artifacts */
	if (artifact_p(o_ptr) && (rand_int(100) < 45 + goodluck))
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

		/* Blast the armor (no more removing egos or destroying artifacts) */
		o_ptr->to_a -= randint(5) + randint(5);
		if ((die > 45) || (o_ptr->to_h > 0))
		{
           o_ptr->to_h -= 1 + randint(1 + badluck/6);
           if ((goodluck > 0) && (die < 41) && (o_ptr->to_h < 0)) o_ptr->to_h = 0;
        }
		else if (randint(6) == 1) o_ptr->to_h -= randint(1 + badluck/6);
		if ((o_ptr->to_d > 0) && (badluck > 1)) o_ptr->to_d -= randint(2 + badluck/4);
		o_ptr->ac -= randint(2 + badluck/3);
		
        /* remove blessing from bless obejct spell */
        if (o_ptr->blessed) o_ptr->blessed = 0;

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
	}

	return (TRUE);
}


/*
 * Curse the players weapon (no longer shatters weapons)
 * added an option to adjust the seriousness of the curse
 *  (..replacing use of the global spellswitch hack)
 */
bool curse_weapon(int badness)
{
	object_type *o_ptr;

	char o_name[80];


	/* Curse the weapon */
	o_ptr = &inventory[INVEN_WIELD];
	
	/* DJA: 1 in 6 chance to curse range weapon instead */
	if ((badness > 1) && (goodluck < 6) && (randint(6) == 1))
	{
	   o_ptr = &inventory[INVEN_BOW];
    }

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

	/* Attempt a saving throw */
	if (artifact_p(o_ptr) && (rand_int(100) < 50 + goodluck/2))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your weapon", o_name);
	}
	
	/* morgul curse or weakest curse never works on artifacts */
	else if (artifact_p(o_ptr) && ((badness == 5) || (badness == 1)))
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

        /* badness 5 is with bad luck from adjust curse spell only */
        if ((badness == 5) && (o_ptr == &inventory[INVEN_WIELD]))
        {
		   o_ptr->name1 = 0;
		   o_ptr->name2 = EGO_MORGUL;
		   o_ptr->to_h -= randint(8) + 1;
		   o_ptr->to_d -= randint(8) + 1;
		   o_ptr->to_a = 0;
		   o_ptr->ac = 0;
        }
        /* range weapon with bad luck from adjust curse spell only */
        else if (badness == 5)
        {
		   o_ptr->name1 = 0;
		   o_ptr->name2 = EGO_NAZGUL;
		   o_ptr->to_h -= randint(7) + 1;
		   o_ptr->to_d -= randint(7) + 1;
		   o_ptr->to_a = 0;
		   o_ptr->ac = 0;
        }
        /* badness 1 to 4 (usually 2 or 3) */
        else if (o_ptr == &inventory[INVEN_WIELD])
        {
		   o_ptr->to_h -= randint(badness * 2) + 1;
		   o_ptr->to_d -= randint(badness * 2) + 1;
		   if ((o_ptr->to_a > 0) && (badness > 1))
           {
              o_ptr->to_a -= randint(2);
		      if (o_ptr->to_a < 0) o_ptr->to_a = 0;
           }
		   if ((o_ptr->ac > 0) && (badness > 2))
           {
              o_ptr->ac -= randint(2);
		      if (o_ptr->ac < 0) o_ptr->ac = 0;
           }
           /* remove blessing from bless obejct spell */
		   if (o_ptr->blessed) o_ptr->blessed = 0;
        }
        else /* range weapon (don't touch ac) */
        {
		   o_ptr->to_h -= randint(badness * 2 - 1) + 1;
		   o_ptr->to_d -= randint(badness * 2 - 1) + 1;

           /* remove blessing from bless obejct spell */
		   if (o_ptr->blessed) o_ptr->blessed = 0;
        }
        /* else
        {
		/ never Shatter the weapon anymore /
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_SHATTERED;
		o_ptr->to_h = 0 - randint(5) - randint(5);
		o_ptr->to_d = 0 - randint(5) - randint(5);
		o_ptr->to_a = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;
        } */

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
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
	/* you can never modify broken (completely worthless) items */
	if ((o_ptr->k_idx) &&
	    (!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
	    (!broken_p(o_ptr)))
	{
		cptr act = "magical";
		char o_name[80];
		
		/* small chance to succeed branding cursed weapon */
		if ((cursed_p(o_ptr)) && (randint(100) > 10 + goodluck/2))
	    {
		     if (flush_failure) flush();
		     msg_print("The branding failed.");
		     return;
	    }

		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

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
			case EGO_BRAND_ELEC:
			case EGO_AMMO_ELEC:
				act = "electric";
				break;
		}

		/* Describe */
		msg_format("A %s aura surrounds the %s.", act, o_name);
		if (brand_type == EGO_ACID_COAT) msg_format("An acidic coating covers the %s.", o_name);

		/* Brand the object */
		o_ptr->name2 = brand_type;

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	
		/* Enchant (enchant less for acid coating or poison brand spells) */
		if (spellswitch == 17) enchant(o_ptr, rand_int(3) + 1, ENCH_TOHIT | ENCH_TODAM);
		else if (spellswitch == 19) enchant(o_ptr, rand_int(3) + 2, ENCH_TOHIT | ENCH_TODAM);
		else enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}
	else
	{
		if (flush_failure) flush();
		msg_print("The branding failed.");
	}
}


/*
 * Brand the current melee weapon
 */
void brand_weapon(void)
{
	object_type *o_ptr;
	byte brand_type;

	o_ptr = &inventory[INVEN_WIELD];

	/* Select a brand */
	if ((rand_int(100) < 50) || (spellswitch == 7))
		brand_type = EGO_BRAND_COLD;
	else
		brand_type = EGO_BRAND_FIRE;
		
	if (spellswitch == 20) brand_type = EGO_BRAND_POIS;
		
	/* Brand the weapon */
	brand_object(o_ptr, brand_type);
}


/*
 * Hook to specify "ammo"
 */
static bool item_tester_hook_ammo(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
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
 * Brand some (non-magical) ammo
 */
bool brand_ammo(void)
{
	int item;
	object_type *o_ptr;
	cptr q, s;
	int r;
	byte brand_type;

	/* Only accept ammo */
	item_tester_hook = item_tester_hook_ammo;

	/* Get an item */
	if (spellswitch == 17)
    {
        q = "Coat which ammunition? ";
	    s = "You have no ammo to coat.";
    }
	else
    {
        q = "Brand which kind of ammunition? ";
	    s = "You have no ammo to brand.";
    }
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

	r = rand_int(100);

	/* Select the brand */
	if (r < 33)
		brand_type = EGO_FLAME;
	else if (r < 67)
		brand_type = EGO_FROST;
	else
		brand_type = EGO_AMMO_ELEC;
		
	if (spellswitch == 17) brand_type = EGO_ACID_COAT;
	if (spellswitch == 19) brand_type = EGO_AMMO_VENOM;

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
	cptr q, s;


	/* Restrict choices to bolts */
	item_tester_tval = TV_BOLT;

	/* Get an item */
	q = "Brand which bolts? ";
	s = "You have no bolts to brand.";
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

	/* Brand the bolts */
	brand_object(o_ptr, EGO_FLAME);

	/* Done */
	return (TRUE);
}


/*
 * Enchant some (non-magical) sling shots
 */
bool snowball_shot(void)
{
	int item;
	object_type *o_ptr;
	cptr q, s;
	char o_name[80];

	/* Restrict choices to shots */
	item_tester_tval = TV_SHOT;

	/* Get an item */
	q = "Brand which shots or pebbles? ";
	s = "You have no shots to brand.";
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

	/* Brand the bolts */
	brand_object(o_ptr, EGO_FROST);

	/* Get the basic name of the object */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);
	
	msg_format("Your %s turn into magical snowballs!", o_name);

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
			if (randint(50) < goodluck)
			{
               msg_print("You feel you are out of luck.");
               p_ptr->luck -= randint(p_ptr->luck - 4) + 5;
			   (void)dec_stat(A_CHR, 2, TRUE);
			   if (p_ptr->hold_life) p_ptr->exp -= (p_ptr->exp / 20);
			   else p_ptr->exp -= (p_ptr->exp / 9);
               break;
            }
			
            /* Message */
			msg_print("You are surrounded by a malignant aura.");

			/* Decrease all stats (permanently) */
			(void)dec_stat(A_STR, 10, TRUE);
			(void)dec_stat(A_INT, 10, TRUE);
			(void)dec_stat(A_WIS, 10, TRUE);
			(void)dec_stat(A_DEX, 10, TRUE);
			(void)dec_stat(A_CON, 10, TRUE);
			(void)dec_stat(A_CHR, 10, TRUE);

			/* Lose some experience (permanently) */
			p_ptr->exp -= (p_ptr->exp / 5);
			p_ptr->max_exp -= (p_ptr->max_exp / 5);
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


/*
 * Identify an item.
 *
 * `item` is used to print the slot occupied by an object in equip/inven.
 * Any negative value assigned to "item" can be used for specifying an object
 * on the floor.
 */
void do_ident_item(int item, object_type *o_ptr)
{
	char o_name[80];

	/* Identify it */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Apply an autoinscription, if necessary */
	apply_autoinscription(o_ptr);

	/* Set squelch flag */
	p_ptr->notice |= PN_SQUELCH;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

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
		msg_format("On the ground: %s.", o_name);
	}
}

/*
 * Activate Experience drain on an item
 * (activated by attacking, shooting, or casting)
 * (was 55 instead of 10 + odd)
 * (odd is 45 except in melee when it is 40 + (damage/5))
 */
void rxp_drain(int odd)
{
     /* sentient equipment can make exp drain kick in more or less often */
     if (goodweap > badweap) odd -= 10;
     else if (badweap > goodweap) odd -= 5;

     if ((randint(100) < odd + badluck - goodluck) && (p_ptr->exp > 0))
     {
        int drainmuch = randint(11) + randint(p_ptr->lev) + randint(badluck*2);
        if (cp_ptr->spell_book == TV_DARK_BOOK) drainmuch += randint(6);
		p_ptr->exp -= drainmuch;
		if (goodluck > 16) p_ptr->max_exp -= drainmuch/10;
		else if (goodluck > 10) p_ptr->max_exp -= drainmuch/4;
		else if (goodluck > 4) p_ptr->max_exp -= drainmuch/3;
		else if (badluck > 6) p_ptr->max_exp -= (drainmuch * 2) / 3;
		else p_ptr->max_exp -= drainmuch/2;
	    check_experience();
     }
     else if ((badluck > 0) && (p_ptr->exp > 0))
     {
        int drainmuch = randint(badluck) + 1;
		p_ptr->exp -= drainmuch;
		p_ptr->max_exp -= 1;
	    check_experience();
     }
}

/*
 * Create a treasure map for the tourist to find.
 * Copied from wiz_create_item(void)
 */
void treasure_map(void)
{
	int i, x, y, d, dis, min;
	bool look = TRUE;

	int py = p_ptr->py;
	int px = p_ptr->px;

	object_type *i_ptr;
	object_type object_type_body;

	int k_idx;

	/* Save screen */
	screen_save();

	/* Get object base type (tval 4, sval 2) */
	k_idx = lookup_kind(TV_SPECIAL, SV_TREASURE);

	/* Load screen */
	screen_load();

	/* Return if failed */
	if (!k_idx) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Create the item */
	object_prep(i_ptr, k_idx);
	
	/* no need to apply magic to a treasure map */
	
	/* Look for a place to "hide" the map */
	/* coped from teleportation code */
	look = TRUE;
	dis = (76 - randint(p_ptr->lev + goodluck));

	/* Initialize */
	y = py;
	x = px;

	/* Minimum distance */
	min = dis / 2;

	while (look)
	{
		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
			/* Pick a (possibly illegal) location */
			while (1)
			{
				y = rand_spread(py, dis);
				x = rand_spread(px, dis);
				d = distance(py, px, y, x);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds_fully(y, x)) continue;

			/* Require "naked" floor space */
			if (!cave_naked_bold(y, x)) continue;

			/* Might create the map inside a vault */
			/* if (cave_info[y][x] & (CAVE_ICKY)) continue; */

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* Increase the maximum distance */
		dis = dis * 2;

		/* Decrease the minimum distance */
		min = min / 2;
    }

	/* how far away is it? */
    d = distance(py, px, y, x);
	
    /* location to create the map */
    py = y;
	px = x;

	/* Drop the object from heaven */
	drop_near(i_ptr, -1, py, px);

	/* All done */
	if (d < 8) msg_print("X marks the spot. (actually it's a '?')");
	else msg_print("The map is hidden, the hunt is on!");
}
