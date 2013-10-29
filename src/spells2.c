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
	int maxhp = p_ptr->mhp;
	if (p_ptr->timed[TMD_FALSE_LIFE]) maxhp += 2 * (p_ptr->lev + 10);
	
	/* Healing needed */
	if (p_ptr->chp < maxhp)
	{
		/* Gain hitpoints */
		p_ptr->chp += num;

		/* Enforce maximum */
		if (p_ptr->chp >= maxhp)
		{
			p_ptr->chp = maxhp;
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
 * DJAXXX Why can't objects be in the space with a glyph?
 */
bool warding_glyph(void)
{
	bool usedup;
	object_type *o_ptr;
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* (can't make a glyph while standing on rubble) */
	if (cave_feat[py][px] != FEAT_FLOOR)
	{
		msg_print("There is no clear floor on which to cast the spell.");
		/* don't use up the scroll */
		return FALSE;
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
	
	return TRUE;
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
	if (inc_stat(stat, 0))
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
void uncurse_object(object_type *o_ptr)
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
static int remove_curse_aux(int all, bool quiveronly)
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

		/* the archer spell can only uncurse ammo in the quiver */
		if ((quiveronly) && (!IS_QUIVER_SLOT(i))) continue;

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
 * Remove curses in quiver (ammo never has heavy or perma curses (I think))
 */
bool remove_cursed_quiver(void)
{
	return (remove_curse_aux(FALSE, TRUE));
}

/*
 * Remove most curses (all light curses)
 */
bool remove_curse(void)
{
	return (remove_curse_aux(FALSE, FALSE));
}

/*
 * Remove all curses (includes heavy curses, and sometimes perma curses)
 */
bool remove_all_curse(void)
{
	return (remove_curse_aux(TRUE, FALSE));
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
	/* this is kindof a pain, but I'm no good at messing with strings in fancy ways */
	bool senseorc = FALSE, sensetroll = FALSE, sensegiant = FALSE;
    bool senseundead = FALSE, sensedemon = FALSE, sensedragon = FALSE;
	bool sensewere = FALSE, sensesilver = FALSE, sensefairy = FALSE;
    bool sensegolem = FALSE, sensebug = FALSE, senseanimal = FALSE;
	bool sensedwarf = FALSE, senseelf = FALSE, thranduil = FALSE;
    bool ratagast = FALSE, sensesting = FALSE, senseall = FALSE;
	int sensemany;

	/* Get item flags from equipment */
	for (k = INVEN_WIELD; k < END_EQUIPMENT; k++)
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
		
		if (o_ptr->esprace)
		{
			switch (o_ptr->esprace)
			{
				case 1: {senseorc = TRUE; sensemany += 1; break;}
				case 2: {sensetroll = TRUE; sensemany += 1; break;}
				case 3: {sensegiant = TRUE; sensemany += 1; break;}
				case 4: {senseundead = TRUE; sensemany += 1; break;}
				case 5: {sensedragon = TRUE; sensemany += 1; break;}
				case 6: {sensedemon = TRUE; sensemany += 1; break;}
				case 7: {sensefairy = TRUE; sensemany += 1; break;}
				case 8: {sensebug = TRUE; sensemany += 1; break;}
				case 9: {sensesilver = TRUE; sensemany += 1; break;}
				case 10: {senseanimal = TRUE; sensemany += 1; break;}
				case 11: {sensegolem = TRUE; sensemany += 1; break;}
				case 12: {sensedwarf = TRUE; sensemany += 1; break;}
				case 13: {senseelf = TRUE; sensemany += 1; break;}
				case 15: {sensesting = TRUE; sensemany += 1; break;}
				case 71: {ratagast = TRUE; sensemany += 1; break;}
				case 75: {thranduil = TRUE; sensemany += 1; break;}
				case 70: {senseall = TRUE; break;}
			}
		}
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
		if (p_ptr->timed[TMD_FRENZY] > 19) info[i++] = "You are in an extreme frenzy.";
		else info[i++] = "You are in a mad careless frenzy.";
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
	else if (f2 & TR2_AGGRAVATE)
	{
		info[i++] = "Your stealth is severely drained.";
	}
	if ((p_ptr->telecontrol) && (f2 & TR2_TELEPORT))
	{
		info[i++] = "You have teleport control and random teleportation.";
	}
	else if (p_ptr->telecontrol)
	{
		info[i++] = "You have teleport control.";
	}
	else if (f2 & TR2_TELEPORT)
	{
		info[i++] = "Your position is very uncertain.";
	}
	if (p_ptr->timed[TMD_SKILLFUL])
	{
		info[i++] = "Your skills are boosted.";
	}
	if (p_ptr->timed[TMD_SNIPER])
	{
		info[i++] = "Your ranged weapon aim is enhanced.";
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
	if (p_ptr->timed[TMD_SHADOW])
	{
		info[i++] = "You are wrapped in shadow.";
	}
	if (p_ptr->timed[TMD_SHIELD])
	{
		info[i++] = "You are protected by a powerful mystic shield.";
	}
	else if (p_ptr->timed[TMD_WSHIELD])
	{
		info[i++] = "You are protected by a mystic shield.";
	}
	if (p_ptr->timed[TMD_FALSE_LIFE])
	{
		info[i++] = "You have unnatural toughness.";
	}
	if (p_ptr->timed[TMD_INVULN])
	{
		info[i++] = "You are temporarily invulnerable.";
	}
	if (p_ptr->timed[TMD_CURSE])
	{
		info[i++] = "You are temporarily cursed.";
	}
	if (p_ptr->timed[TMD_MIND_CONTROL])
	{
		info[i++] = "An outside force is attempting to control you.";
	}
	if (p_ptr->timed[TMD_WITCH])
	{
		info[i++] = "Your black magic is aggravating demons.";
	}
	if (p_ptr->timed[TMD_STINKY]) /* accidently had 'you spell disgusting'.. */
	{
		info[i++] = "You smell disgusting.";
	}
	if (p_ptr->timed[TMD_BEAR_HOLD])
	{
		info[i++] = "You are being held by a monster.";
	}
	if (p_ptr->confusing)
	{
		info[i++] = "Your hands are glowing dull red.";
	}
	if (p_ptr->searching)
	{
		info[i++] = "You are looking around very carefully.";
	}
	if (p_ptr->new_spells > 0)
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
	else if (p_ptr->timed[TMD_MINDLIGHT])
	{
		info[i++] = "You are giving off light.";
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
	if (f2 & (TR2_STOPREGEN))
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
	else if (senseall)
	{
		info[i++] = "You sense all monsters (even mindless ones).";
	}
	else if (f3 & TR3_TELEPATHY)
	{
		info[i++] = "You have ESP.";
	}
	else
	{
		/* I hate messing with strings. */
		/* I know there's a better way to do this, I just don't know how to do it. */
		if (thranduil)
		{
			info[i++] = "You sense the presense of orcs and various forest creatures.";
		}
		else if (sensesting)
		{
			info[i++] = "You sense the presense of orcs and spiders.";
		}
		else if (senseorc)
		{
			info[i++] = "You sense the presense of orcs.";
		}
		if (sensetroll)
		{
			info[i++] = "You sense the presense of trolls.";
		}
		if (sensegiant)
		{
			info[i++] = "You sense the presense of giants.";
		}
		if (sensedemon)
		{
			info[i++] = "You sense the presense of demons.";
		}
		if (sensedragon)
		{
			info[i++] = "You sense the presense of dragons.";
		}
		if (senseundead)
		{
			info[i++] = "You sense the presense of undead.";
		}
		if (senseanimal)
		{
			info[i++] = "You sense the presense of most types of animals.";
		}
		if (sensebug)
		{
			info[i++] = "You sense the presense of bugs.";
		}
		if (sensewere)
		{
			info[i++] = "You sense the presense of creatures vulnerable to silver.";
		}
		if (sensesilver)
		{
			info[i++] = "You sense the presense of grepse (silver monsters).";
		}
		if (sensefairy)
		{
			info[i++] = "You sense the presense of light fairies.";
		}
		if (sensedwarf)
		{
			info[i++] = "You sense the presense of dwarves.";
		}
		if (senseelf)
		{
			info[i++] = "You sense the presense of dark elves.";
		}
		/* This doesn't work but the others do. that doesn't make sense. */
        if (ratagast)
		{
			info[i++] = "You sense the presense of evil wizards.";
		}
		if (sensegolem)
		{
			info[i++] = "You sense the presense of contructs.";
		}
	}
	
	if (p_ptr->darkvis || (p_ptr->timed[TMD_DARKVIS]))
	{
		info[i++] = "You have darkvision.";
	}
	if (p_ptr->timed[TMD_TMPBOOST])
	{
		if (p_ptr->see_infra == A_STR) info[i++] = "Your strength is temporarily boosted.";
		if (p_ptr->see_infra == A_DEX) info[i++] = "Your dexterity is temporarily boosted.";
		if (p_ptr->see_infra == A_CON) info[i++] = "Your constitution is temporarily boosted.";
		if (p_ptr->see_infra == A_INT) info[i++] = "Your intelligence is temporarily boosted.";
		if (p_ptr->see_infra == A_WIS) info[i++] = "Your wisdom is temporarily boosted.";
		if (p_ptr->see_infra == A_CHR) info[i++] = "Your charisma is temporarily boosted.";
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

	if (p_ptr->cursed_quiver)
	{
		info[i++] = "Your quiver is cursed.";
	}
	if ((p_ptr->cursed_quiver) && (p_ptr->timed[TMD_QUIVERGUARD]))
	{
		info[i++] = "Your quiver is protected, but the curse weakens the protection.";
	}
	else if (p_ptr->timed[TMD_QUIVERGUARD])
	{
		info[i++] = "The ammunition in your quiver is protected.";
	}
	if (p_ptr->timed[TMD_ACID_BLOCK])
	{
		info[i++] = "Your inventory is protected from acid.";
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
	if (p_ptr->throwmult >= 2)
	{
		info[i++] = "Your equipment enhances your throwing weapon damage.";
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

	if (f4 & TR4_RES_FEAR)
	{
		info[i++] = "You are completely fearless.";
	}

	if (f2 & TR2_PEACE)
	{
		info[i++] = "You are not agressive in combat.";
	}

	if (p_ptr->nice)
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

	if (p_ptr->timed[TMD_OPP_SILV]) /* includes Rcharm and Rsilver */
	{
		info[i++] = "You are resistant to charm, silver magic, ";
		info[i++] = "  amnesia, and melee hallucenation.";
	}
	else if ((p_ptr->resist_silver) && (p_ptr->resist_charm))
	{
		info[i++] = "You are resistant to charm, silver magic and amnesia.";
	}
	else if (p_ptr->resist_silver)
	{
		info[i++] = "You are resistant to silver magic and amnesia.";
	}
	else if (p_ptr->resist_charm)
	{
		info[i++] = "You are resistant to charm.";
	}	
	if (p_ptr->resist_slime)
	{
		info[i++] = "You are resistant to sliming.";
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
	if (p_ptr->resist_static > 3)
	{
		info[i++] = "You resist static exceptionally well.";
	}
	else if (p_ptr->resist_static)
	{
		info[i++] = "You are resistant to static.";
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

	if (f1 & (TR1_EQLUCK))
	{
		info[i++] = "Your luck is affected by your equipment.";
	}

	/* sentient equipment */
	if ((goodweap) && (cp_ptr->spell_book == TV_PRAYER_BOOK))
	{
		info[i++] = "You are wearing equipment which aids your prayers.";
	}
	if ((badweap) && (cp_ptr->spell_book == TV_PRAYER_BOOK))
	{
		info[i++] = "You are wearing equipment which hinders your prayers.";
	}
	if ((goodweap) && (cp_ptr->spell_book == TV_DARK_BOOK))
	{
		info[i++] = "You are wearing equipment which hinders your black magic.";
	}
	if ((badweap) && (cp_ptr->spell_book == TV_DARK_BOOK))
	{
		info[i++] = "You are wearing equipment which aids your black magic.";
	}
	if ((goodweap) && (badweap))
	{
		info[i++] = "You are wielding pieces of aligned equipment which are in conflict.";
	}

	if (p_ptr->accident)
	{
		info[i++] = "You sometimes hit yourself with your own weapon.";
	}

	/* reset flags so current weapon part won't include stuff from other equipment */
	f1 = 0L, f2 = 0L, f3 = 0L, f4 = 0L;

	/* Get the current weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Extract the flags */
	if (spoil)
		object_flags(o_ptr, &f1, &f2, &f3, &f4);
	else 
		object_flags_known(o_ptr, &f1, &f2, &f3, &f4);

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
		if (f2 & (TR2_SLAY_ANIMAL))
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
		if (f1 & (TR1_SLAY_BUG))
		{
			info[i++] = "Your weapon is especially deadly against bugs.";
		}
		if (f1 & (TR1_SLAY_LITE))
		{
			info[i++] = "Your weapon is especially deadly against creatures of light.";
		}
		if (f1 & (TR1_SLAY_SILVER))
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
		if (f2 & (TR2_KILL_DRAGON))
		{
			info[i++] = "Your weapon is a great bane of dragons.";
		}
		if (f2 & (TR2_KILL_DEMON))
		{
			info[i++] = "Your weapon is a great bane of demons.";
		}
		if (f2 & (TR2_KILL_UNDEAD))
		{
			info[i++] = "Your weapon is a great bane of undead.";
		}


		/* Indicate Blessing */
		if (f3 & (TR3_BLESSED))
		{
			info[i++] = "Your weapon has been blessed by the gods.";
		}

		/* Hack */
		if (f2 & (TR2_IMPACT))
		{
			info[i++] = "Your weapon can induce earthquakes.";
		}		
	}

	if (p_ptr->theme == 1)
	{
		info[i++] = "You are currently in a cold forest.";
	}		
	if (p_ptr->theme == 2)
	{
		info[i++] = "You are currently in a fairy forest.";
	}		
	if (p_ptr->theme == 3)
	{
		info[i++] = "You are currently in an icky place.";
	}		
	if (p_ptr->theme == 4)
	{
		info[i++] = "You are currently in a volcano cave.";
	}		
	if (p_ptr->theme == 5)
	{
		info[i++] = "You are currently in a earthy cave.";
	}		
	if (p_ptr->theme == 6)
	{
		info[i++] = "You are currently in a windy cave.";
	}		
	if (p_ptr->theme == 7)
	{
		info[i++] = "You are currently under a full moon.";
	}		
	if (p_ptr->theme == 8)
	{
		info[i++] = "You are currently in the haunted ruins of a castle.";
	}		
	if (p_ptr->theme == 9)
	{
		info[i++] = "You are currently in a swamp.";
	}		
	if (p_ptr->theme == 10)
	{
		info[i++] = "You are currently in a dwarf mine.";
	}		
	if (p_ptr->theme == 11)
	{
		info[i++] = "You are currently in a bug cave.";
	}		
	if (p_ptr->theme == 12)
	{
		info[i++] = "You are currently in the domain of the grepse.";
	}		
	if (p_ptr->theme == 13)
	{
		info[i++] = "You are currently on the nightmare plane.";
	}		
	if (p_ptr->theme == 14)
	{
		info[i++] = "You are currently in a hell hall.";
	}		
	if (p_ptr->theme == 15)
	{
		info[i++] = "You are currently in an army barracks.";
	}		
	if (p_ptr->theme == 15)
	{
		info[i++] = "You are currently in a city of hobs (dark fairies).";
	}		


	/* luck level (see end of function) */
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
	
	/* corruption (see end of function) */
	if (p_ptr->corrupt >= 40)
	{
		info[i++] = "You have been almost completely corrupted.";
	}
	else if (p_ptr->corrupt > 20)
	{
		info[i++] = "You have advanced corruption from use of a corrupting item.";
	}
	else if (p_ptr->corrupt > 10)
	{
		info[i++] = "You are being corrupted from use of a corrupting item.";
	}
	else if (p_ptr->corrupt)
	{
		info[i++] = "You are beginning to be corrupted from use of a corrupting item.";
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

	/* Exact amounts for luck and corruption with potion of self knowledge */
	/* (Don't know how to put it with the rest of the information when displaying a variable) */
	if (spoil)
	{
		if ((goodluck) && (badluck)) msg_format("Your good luck rating is %d and your bad luck rating is %d.", goodluck, badluck);
		else if (goodluck) msg_format("Your good luck rating is %d, and bad luck is 0. (0-20 range) ", goodluck);
		else if (badluck) msg_format("Your bad luck rating is %d, and good luck is 0. (0-20 range) ", badluck);
		else msg_print("You have no good luck and no bad luck.");
		if (p_ptr->corrupt) msg_format("Your level of corruption is %d (max 50).", p_ptr->corrupt);
	}
}



/*
 * Set word of recall as appropriate
 */
void set_recall(s16b time)
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

		p_ptr->word_recall = time; /* rand_int(20) + 15; */
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
 * Useful constants for the area around the player to detect.
 * This is instead of using circular detection spells.
 */
#define DETECT_DIST_X	45	/* left & right (was 52) */
#define DETECT_DIST_Y	22	/* top & bottom (was 23) */
#define REVEAL_DIST_X	32	/* left & right */
#define REVEAL_DIST_Y	16	/* top & bottom */

/*
 * Detect all traps in the area (now fixed size)
 * (except for non-adjacent chest traps)
 */
bool detect_traps(void)
{
	int y, x;
	int x1, x2, y1, y2;
	int py = p_ptr->py;
	int px = p_ptr->px;

	bool detect = FALSE;

	/* Pick an area to detect */
	y1 = py - DETECT_DIST_Y;
	y2 = py + DETECT_DIST_Y;
	x1 = px - DETECT_DIST_X;
	x2 = px + DETECT_DIST_X;

	if (y1 < 0) y1 = 0;
	if (x1 < 0) x1 = 0;

	/* Scan the area */
	for (y = y1; y < y2; y++)
	{
		for (x = x1; x < x2; x++)
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

	/* check for chest traps */
	/* Search the nearby grids, which are always in bounds */
	for (y = (py - 1); y <= (py + 1); y++)
	{
		for (x = (px - 1); x <= (px + 1); x++)
		{
			object_type *o_ptr;
			/* look for a chest */
			for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
			{
				/* detect chest traps */
				if ((o_ptr->tval == TV_CHEST) && (o_ptr->pval > 0))
				{
					object_known(o_ptr);
					detect = TRUE;
				}
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
 * Detect doors and stairs around the player.
 */
bool detect_doorstairs(bool staironly)
{
	int y, x;
	int x1, x2, y1, y2;

	bool doors = FALSE, stairs = FALSE;

	/* Pick an area to map */
	y1 = p_ptr->py - DETECT_DIST_Y;
	y2 = p_ptr->py + DETECT_DIST_Y;
	x1 = p_ptr->px - DETECT_DIST_X;
	x2 = p_ptr->px + DETECT_DIST_X;

	if (y1 < 0) y1 = 0;
	if (x1 < 0) x1 = 0;


	/* Scan the dungeon */
	for (y = y1; y < y2; y++)
	{
		for (x = x1; x < x2; x++)
		{
			if (!in_bounds_fully(y, x)) continue;

			if (!staironly)
			{
				/* Detect secret doors */
				if (cave_feat[y][x] == FEAT_SECRET)
					place_closed_door(y, x);
	
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
					doors = TRUE;
				}
			}

			/* Detect stairs */
			if ((cave_feat[y][x] == FEAT_LESS) ||
			    (cave_feat[y][x] == FEAT_MORE))
			{
				/* Hack -- Memorize */
				cave_info[y][x] |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Obvious */
				stairs = TRUE;
			}

		}
	}

	/* Describe */
	if (doors && !stairs)      msg_print("You sense the presence of doors!");
	else if (!doors && stairs) msg_print("You sense the presence of stairs!");
	else if (doors && stairs)  msg_print("You sense the presence of doors and stairs!");
	/* else if (aware && !doors && !stairs) msg_print("You sense no doors or stairs."); */

	/* Result */
	return (doors || stairs);
}


/*
 * Detect any treasure on the current panel
 */
bool detect_treasure(void)
{
	int y, x, i;
	int x1, x2, y1, y2;

	bool gold_buried = FALSE;
	bool gold_object = FALSE;

	/* Pick an area to detect */
	y1 = p_ptr->py - DETECT_DIST_Y;
	y2 = p_ptr->py + DETECT_DIST_Y;
	x1 = p_ptr->px - DETECT_DIST_X;
	x2 = p_ptr->px + DETECT_DIST_X;

	if (y1 < 0) y1 = 0;
	if (x1 < 0) x1 = 0;

	/* Scan the area */
	for (y = y1; y < y2; y++)
	{
		for (x = x1; x < x2; x++)
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
				gold_buried = TRUE;
			}
		}
	}

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
		if (x < x1 || y < y1 || x > x2 || y > y2) continue;
		/* if (!panel_contains(y, x)) continue; */

		/* Detect "gold" objects */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Hack -- memorize it */
			o_ptr->marked = TRUE;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			gold_object = TRUE;
		}
	}
	
	/* Check for mimmics */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		bool detectme = FALSE;

		/* Paranoia -- skip "dead" monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby objects */
		/* if (x < x1 || y < y1 || x > x2 || y > y2) continue; */

		/* detect mimmics as objects */
        if ((r_ptr->flags1 & (RF1_CHAR_MULTI)) && (m_ptr->disguised))
		{
			if (strchr("$", r_ptr->d_char)) detectme = TRUE;
		}
		/* chance of detecting non-CHAR_MUTLI creeping coins as well */
		else if ((strchr("$", r_ptr->d_char)) && (rand_int(100) < 15 + goodluck))
			detectme = TRUE;
		
		if (detectme)
		{
			/* mark as detected as object */
			m_ptr->meet = 100;
			/* Redraw */
			lite_spot(y, x);
			gold_object = TRUE;
		}
	}

	/* Describe */
	if (gold_object)
		msg_print("You sense the presence of treasure!");

	if (gold_buried)
		msg_print("You sense the presence of buried treasure!");

	/* Result */
	return (gold_object || gold_buried);
}


/*
 * Detect all "normal" objects on the current panel
 */
bool detect_objects_normal(bool full)
{
	int i, y, x;
	int x1, x2, y1, y2;

	bool detect = FALSE;

	/* Pick an area to detect */
	y1 = p_ptr->py - DETECT_DIST_Y;
	y2 = p_ptr->py + DETECT_DIST_Y;
	x1 = p_ptr->px - DETECT_DIST_X;
	x2 = p_ptr->px + DETECT_DIST_X;

	if (y1 < 0) y1 = 0;
	if (x1 < 0) x1 = 0;


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
		if (x < x1 || y < y1 || x > x2 || y > y2) continue;
		/* if (!panel_contains(y, x)) continue; */

		/* Detect "real" objects */
		if (o_ptr->tval != TV_GOLD)
		{
			/* Hack -- memorize it */
			o_ptr->marked = TRUE;

			/* (never show objects buried in granite, quartz, or magma) */
			if ((o_ptr->hidden) && ((cave_feat[y][x] == FEAT_RUBBLE) || (cave_feat[y][x] == FEAT_FLOOR)))
			{
				/* always unhide rubble objects when using powerful detection spell */
				if (full) o_ptr->hidden = 0;
				/* normally unhide rubble objects only if in LOS */
				else if (player_can_see_bold(y, x)) o_ptr->hidden = 0;
				/* chance to unhide other rubble objects with luck */
				else if (randint(100) < 15 + goodluck - badluck) o_ptr->hidden = 0;
			}

			/* Redraw */
			lite_spot(y, x);

			/* Detect (ignore hidden objects) */
			if (!squelch_hide_item(o_ptr))
				detect = TRUE;
		}
	}
	
	/* Check for mimmics */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- skip "dead" monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby object mimmics */
		if (m_ptr->fx < x1 || m_ptr->fy < y1 || m_ptr->fx > x2 || m_ptr->fy > y2) continue;

		/* detect mimmics as objects */
        if ((r_ptr->flags1 & (RF1_CHAR_MULTI)) && (m_ptr->disguised))
		{
			if (strchr("!,-_~=?", r_ptr->d_char))
			{
				/* mark as detected as object */
				m_ptr->meet = 100;
				/* Redraw */
				lite_spot(y, x);
				detect = TRUE;
			}
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of objects!");
		p_ptr->window |= PW_OBJLIST;
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
	int x1, x2, y1, y2;

	bool detect = FALSE;

	/* Pick an area to detect */
	y1 = p_ptr->py - DETECT_DIST_Y;
	y2 = p_ptr->py + DETECT_DIST_Y;
	x1 = p_ptr->px - DETECT_DIST_X;
	x2 = p_ptr->px + DETECT_DIST_X;

	if (y1 < 0) y1 = 0;
	if (x1 < 0) x1 = 0;


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
		if (x < x1 || y < y1 || x > x2 || y > y2) continue;
		/* if (!panel_contains(y, x)) continue; */

		/* Examine the tval */
		tv = o_ptr->tval;

		/* Artifacts, misc magic items, or enchanted wearables. */
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

			/* (never show objects buried in granite, quartz, or magma) */
			if ((o_ptr->hidden) && ((cave_feat[y][x] == FEAT_RUBBLE) || (cave_feat[y][x] == FEAT_FLOOR)))
			{
				/* normally unhide rubble objects only if in LOS */
				if (player_can_see_bold(y, x)) o_ptr->hidden = 0;
				/* chance to unhide other rubble objects with luck */
				else if (randint(100) < 20 + goodluck - badluck) o_ptr->hidden = 0;
			}

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			if (!squelch_hide_item(o_ptr))
				detect = TRUE;
		}
	}
	
	/* Check for mimmics */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- skip "dead" monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby objects */
		if (x < x1 || y < y1 || x > x2 || y > y2) continue;

		/* detect mimmics as objects */
        if ((r_ptr->flags1 & (RF1_CHAR_MULTI)) && (m_ptr->disguised))
		{
			if (strchr("!,-_~=?", r_ptr->d_char))
			{
				/* mark as detected as object */
				m_ptr->meet = 100;
				/* Redraw */
				lite_spot(y, x);
				detect = TRUE;
			}
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of magic objects!");
		p_ptr->window |= PW_OBJLIST;
	}

	/* Return result */
	return (detect);
}


/*
 * Detect all "normal" monsters on the current panel
 *
 * Since this spell doesn't detect invisible monsters and there are
 * less invisible monsters than in V, maybe it shouldn't always
 * detect stealthy monsters or WATER_HIDE monsters in water.
 * ..But then you might need a specific "detect stealthy monsters" spell.
 *
 * A better idea: we make this function take a bool full parameter,
 * if FALSE, it will not always detect hiding monsters and this will be
 * explained in the spell description. Starting at a certain plev,
 * (early for full casters, later for other classes) full will always be TRUE.
 * Or using certain spells like True sight or the black-realm 'see all foes' spell,
 * full will always be TRUE.
 */
bool detect_monsters_normal(bool strong)
{
	int i, y, x;
	int x1, x2, y1, y2;

	bool flag = FALSE;

	/* Pick an area to detect */
	y1 = p_ptr->py - DETECT_DIST_Y;
	y2 = p_ptr->py + DETECT_DIST_Y;
	x1 = p_ptr->px - DETECT_DIST_X;
	x2 = p_ptr->px + DETECT_DIST_X;

	if (y1 < 0) y1 = 0;
	if (x1 < 0) x1 = 0;


	/* Scan monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (x < x1 || y < y1 || x > x2 || y > y2) continue;
		/* if (!panel_contains(y, x)) continue; */

		/* ordinary trees aren't detected as monsters */
		if (r_ptr->flags7 & (RF7_NONMONSTER)) continue;
		
		/* (Usually) don't detect disguised monsters */
		if ((m_ptr->disguised) && (!strong) &&
			(rand_int(100) > goodluck/2 + 1)) continue;

		/* stealthy monsters don't always get detected */
        if ((!m_ptr->ml) && (m_ptr->monseen < 2) && (m_ptr->cdis > 4) &&
			(r_ptr->stealth > 1) && (!strong))
		{
            if (!alertness_check(m_ptr, 1, FALSE)) continue;
		}

		/* Detect all non-invisible monsters */
		if (!(r_ptr->flags2 & (RF2_INVISIBLE)))
		{
			/* Optimize -- Repair flags */
			repair_mflag_mark = repair_mflag_show = TRUE;

			/* Hack -- Detect the monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(i, 0);

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
 * Reveal all "hidden" monsters on current panel, but
 * detect them only if they are in line of sight.
 * (Detect invisible spell renamed to detect hidden)
 */
bool reveal_monsters(bool showmes)
{
	int i, y, x;
	int x1, x2, y1, y2;
	bool revealed = FALSE;

	/* Pick an area to detect (shorter than usual detection distance) */
	y1 = p_ptr->py - REVEAL_DIST_Y;
	y2 = p_ptr->py + REVEAL_DIST_Y;
	x1 = p_ptr->px - REVEAL_DIST_X;
	x2 = p_ptr->px + REVEAL_DIST_X;

	if (y1 < 0) y1 = 0;
	if (x1 < 0) x1 = 0;

	/* Scan monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];
		bool showme = FALSE;

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if ((x < x1 || y < y1 || x > x2 || y > y2) && 
			(!player_can_see_bold(y, x))) continue;

		/* Reveal disguised monsters (but only show them if they're in LOS) */
        if (m_ptr->disguised)
		{
			m_ptr->disguised = 0;

            /* Update the monster */
			if (player_can_see_bold(y, x))
			{
				showme = TRUE;
			}
		}
		/* reveal stealthy monsters which are hiding, but not invisible */
		/* (possible for them to still escape notice) */
		else if (((player_can_see_bold(y, x)) && (!m_ptr->ml)) &&
			(!((r_ptr->flags2 & (RF2_INVISIBLE)) || (m_ptr->tinvis))))
		{
            /* Hack -- Detect the monster (update_mon() will set monseen to 2) */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			showme = TRUE;
		}
                                       
		if (showme)
		{
			update_mon(i, 0);
			
			/* Detect */
			revealed = TRUE;

			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}
		}
	}

	/* Describe */
	if ((revealed) && (showmes))
	{
		/* Describe result */
		msg_print("You sense the presence of hidden creatures!");
	}

	/* Result */
	return (revealed);
}


/*
 * Detect all "invisible" monsters on current panel
 */
bool detect_monsters_invis(void)
{
	int i, y, x;
	int x1, x2, y1, y2;
	bool revealed, flag = FALSE;

	/* Pick an area to detect */
	y1 = p_ptr->py - DETECT_DIST_Y;
	y2 = p_ptr->py + DETECT_DIST_Y;
	x1 = p_ptr->px - DETECT_DIST_X;
	x2 = p_ptr->px + DETECT_DIST_X;

	if (y1 < 0) y1 = 0;
	if (x1 < 0) x1 = 0;

	/* Scan monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (x < x1 || y < y1 || x > x2 || y > y2) continue;
		/* if (!panel_contains(y, x)) continue; */

		/* statues use m_ptr->tinvis for something else */
		if ((r_ptr->flags7 & (RF7_NONMONSTER)) && (r_ptr->flags3 & (RF3_NON_LIVING))) 
			continue;
		
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
			update_mon(i, 0);

			/* Detect */
			flag = TRUE;
		}
	}
	
	/* reveal disguised monsters in separate function */
    revealed = reveal_monsters(FALSE);

	/* Describe */
	if ((revealed) && (flag))
	{
		/* Describe result */
		msg_print("You sense the presence of hidden and invisible creatures!");
	}
	else if (revealed)
	{
		/* Describe result */
		msg_print("You sense the presence of hidden creatures!");
	}
	else if (flag)
	{
		/* Describe result */
		msg_print("You sense the presence of invisible creatures!");
	}
	/* for return value */
	if (revealed) flag = TRUE;

	/* Result */
	return (flag);
}



/*
 * Detect all "evil" monsters on current panel
 * This should also detect evil-aligned objects.
 */
bool detect_monsters_evil(void)
{
	int i, y, x;
	int x1, x2, y1, y2;

	bool flag = FALSE;

	/* Pick an area to detect */
	y1 = p_ptr->py - DETECT_DIST_Y;
	y2 = p_ptr->py + DETECT_DIST_Y;
	x1 = p_ptr->px - DETECT_DIST_X;
	x2 = p_ptr->px + DETECT_DIST_X;

	if (y1 < 0) y1 = 0;
	if (x1 < 0) x1 = 0;

	/* Scan monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;
		
		/* (Usually) don't detect disguised monsters */
		if ((m_ptr->disguised) && (rand_int(100) > goodluck/2 + 1)) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (x < x1 || y < y1 || x > x2 || y > y2) continue;
		/* if (!panel_contains(y, x)) continue; */

		/* stealthy monsters don't always get detected */
        if ((!m_ptr->ml) && (m_ptr->monseen < 2) && (m_ptr->cdis > 4) &&
			(r_ptr->stealth > 1))
		{
			if (!alertness_check(m_ptr, 1, FALSE)) continue;
		}

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
			update_mon(i, 0);

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
 *  This should be the only detect monsters spell which can detect
 * nonmonsters (like trees).
 */
bool detect_monsters_life(void)
{
	int i, y, x;
	int x1, x2, y1, y2;

	bool flag = FALSE;

	/* Pick an area to detect */
	y1 = p_ptr->py - DETECT_DIST_Y;
	y2 = p_ptr->py + DETECT_DIST_Y;
	x1 = p_ptr->px - DETECT_DIST_X;
	x2 = p_ptr->px + DETECT_DIST_X;

	if (y1 < 0) y1 = 0;
	if (x1 < 0) x1 = 0;

	/* Scan monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (x < x1 || y < y1 || x > x2 || y > y2) continue;
		/* if (!panel_contains(y, x)) continue; */
		
		/* (Usually) don't detect disguised monsters */
		if ((m_ptr->disguised) && (rand_int(100) > goodluck/2 + 1)) continue;

		/* stealthy monsters don't always get detected */
        if ((!m_ptr->ml) && (m_ptr->monseen < 2) && (m_ptr->cdis > 4) &&
			(r_ptr->stealth > 1))
		{
			if (!alertness_check(m_ptr, 1, FALSE)) continue;
		}

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
			update_mon(i, 0);

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
	int x1, x2, y1, y2;

	bool flag = FALSE;

	/* Pick an area to detect */
	y1 = p_ptr->py - DETECT_DIST_Y;
	y2 = p_ptr->py + DETECT_DIST_Y;
	x1 = p_ptr->px - DETECT_DIST_X;
	x2 = p_ptr->px + DETECT_DIST_X;

	if (y1 < 0) y1 = 0;
	if (x1 < 0) x1 = 0;

	/* Scan monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (x < x1 || y < y1 || x > x2 || y > y2) continue;
		/* if (!panel_contains(y, x)) continue; */

		/* (There are no animal mimmics) */
        /* stealthy monsters don't always get detected */
        if ((!m_ptr->ml) && (m_ptr->monseen < 2) && (m_ptr->cdis > 4) &&
			(r_ptr->stealth > 1))
		{
			if (!alertness_check(m_ptr, 1, FALSE)) continue;
		}

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
			update_mon(i, 0);

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
bool detect_all(bool strong)
{
	bool detect = FALSE;

	/* Detect everything */
	if (detect_traps()) detect = TRUE;
	if (detect_doorstairs(FALSE)) detect = TRUE;
	/* if (detect_stairs()) detect = TRUE; */
	if (detect_treasure()) detect = TRUE;
	if (detect_objects_normal(strong)) detect = TRUE;
	if (detect_monsters_invis()) detect = TRUE;
	if (detect_monsters_normal(strong)) detect = TRUE;

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
	/* this potion is disguised (pval is set to 0 when identified) */
	if ((o_ptr->tval == TV_POTION) && (o_ptr->sval == SV_POTION_MULTIHUED_POISON) && 
		(object_aware_p(o_ptr)) && (o_ptr->pval))
	{
		int spot;
		/* if disguised as an unaware potion, it appears unaware */
		if (!k_info[o_ptr->pval].aware) return TRUE;

		/* chance based on wisdom to see that there's something unknown about it */
		spot = (adj_mag_study[p_ptr->stat_ind[A_WIS]] / 10) + ((goodluck+1)/2);
		/* you think you recognise it.. */
		if (randint(100) < 100 - spot + ((badluck+1)/2)) return FALSE;
	}

	if (object_known_p(o_ptr))
		return FALSE;
	else
		return TRUE;
}


static bool item_tester_unknown_star(const object_type *o_ptr)
{
	if (o_ptr->ident & IDENT_MENTAL) return FALSE;
	else return TRUE;
}


/*
 * Enchant an item
 *
 * Takes item pointer, number of times to try enchanting,
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
bool enchant(object_type *o_ptr, int n, int eflag, bool big)
{
	int i, chance, prob, lift, minlift;

	bool res = FALSE;

	bool a = artifact_p(o_ptr);

	u32b f1, f2, f3, f4;
	artifact_type *a_ptr = &a_info[o_ptr->name1];

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Large piles resist enchantment */
	prob = o_ptr->number * 100;

	/* Missiles are easy to enchant */
	if ((o_ptr->tval == TV_BOLT) ||
	    (o_ptr->tval == TV_ARROW) ||
	    (o_ptr->tval == TV_SHOT))
	{
		prob = (prob / 20);
	}
	/* Throwing weapons also easier */
	else if (f3 & (TR3_THROWN))
	{
		prob = prob / (5 + randint(4) + ((goodluck+2)/3));
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

			/* has it been disenchanted? */
			if ((a) && (big))
			{
				/* much more likely to work */
				if (o_ptr->to_h < a_ptr->to_h)
				{
					chance = chance/5;
					if (chance > 200) chance = 200;
					/* still subject to 50% autofail on artifacts */
				}
			}

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_h++;
			}
		}

		/* Enchant to damage */
		if (eflag & (ENCH_TODAM))
		{
			if (o_ptr->to_d < 0) chance = 0;
			else if (o_ptr->to_d > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_d];

			/* has it been disenchanted? */
			if ((a) && (big))
			{
				/* much more likely to work */
				if (o_ptr->to_d < a_ptr->to_d)
				{
					chance = chance/5;
					if (chance > 200) chance = 200;
					/* still subject to 50% autofail on artifacts */
				}
			}

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_d++;
			}
		}

		/* Enchant to armor class */
		if (eflag & (ENCH_TOAC))
		{
			if (o_ptr->to_a < 0) chance = 0;
			else if (o_ptr->to_a > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_a];

			/* has it been disenchanted? */
			if ((a) && (big))
			{
				/* much more likely to work */
				if (o_ptr->to_a < a_ptr->to_a)
				{
					chance = chance/5;
					if (chance > 200) chance = 200;
					/* still subject to 50% autofail on artifacts */
				}
			}

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_a++;
			}
		}
	}

    if (a) lift = 20; /* chance to lift a curse */
    else lift = 25;
    if (big) lift += 5; /* *enchant* scrolls */
    /* PERMA_CURSEs can now be lifted but not easily */
    if ((f3 & (TR3_PERMA_CURSE)) || 
       ((f3 & (TR3_HEAVY_CURSE)) && (!big))) lift = lift / 2 - (badluck/2 + 1);
    else if (f3 & (TR3_HEAVY_CURSE)) lift -= (badluck/2 + 1);
    lift += ((goodluck + 1) / 3) * 2;
    if (o_ptr->to_h < 0-3) lift -= 4;
    if (o_ptr->to_a < 0-2) lift -= 4;
    if (o_ptr->to_d < 0-1) lift -= 4;
    if (!res) lift = (lift + 1) / 2;
    /* minimum chance */
    if (goodluck > 4) minlift = 5;
    else if (badluck < 8) minlift = 4;
    else minlift = 0;
    if (f3 & (TR3_PERMA_CURSE)) minlift = 0;
    else if ((f3 & (TR3_HEAVY_CURSE)) && (goodluck < 2)) minlift = 0;
    else if (f3 & (TR3_HEAVY_CURSE)) minlift -= 2;
    if (lift < minlift) lift = minlift;

	/* Break curse (now mostly separated from enchantment) */
	if ((cursed_p(o_ptr)) && (rand_int(100) < lift))
	{
		msg_print("The curse is broken!");

		/* Uncurse the object */
		uncurse_object(o_ptr);
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
 * Enchant an item (in the inventory or on the floor)
 * Note that "num_ac" requires armour, else weapon
 * Returns TRUE if attempted, FALSE if cancelled
 *
 * The 'big' parameter allows *enchant* scrolls to re-enchant
 * disenchanted artifacts.
 */
bool enchant_spell(int num_hit, int num_dam, int num_ac, bool archer, bool big)
{
	int item;
	bool okay = FALSE;

	object_type *o_ptr;

	char o_name[80];

	cptr q, s;


	/* The archer spell can enchant only bows or arrows */
	if (archer)
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
	if (archer) s = "You have no bow or arrows to enchant.";
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
	if (enchant(o_ptr, num_hit, ENCH_TOHIT, big)) okay = TRUE;
	if (enchant(o_ptr, num_dam, ENCH_TODAM, big)) okay = TRUE;
	if (enchant(o_ptr, num_ac, ENCH_TOAC, big)) okay = TRUE;

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

#if removedthis
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

		/* chance to remove curses */
		if ((o_ptr->tval == TV_STAFF) && (cursed_p(o_ptr)))
		{
           u32b f1, f2, f3, f4;
		    int lift = 22;
           /* Extract the flags */
           object_flags(o_ptr, &f1, &f2, &f3, &f4);
           
	        /* PERMA_CURSEs can now be lifted but not easily */
			if (f3 & (TR3_PERMA_CURSE)) lift -= 16;
			if (f3 & (TR3_HEAVY_CURSE)) lift -= (5 + (badluck+2)/3);

			/* chance to Break the curse */
			if (rand_int(100) < lift)
			{
				msg_print("The curse is broken!");

				/* Uncurse the object */
				uncurse_object(o_ptr);
			}
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
bool project_los(int typ, int dam)
{
	int i, x, y;
	int pflg = 0;
	int flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;

	bool obvious = FALSE;


	/* Affect all (nearby) monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Require line of sight */
		if (!player_has_los_bold(y, x)) continue;

		/* Jump directly to the target monster */
		if (project(-1, 0, y, x, dam, typ, flg, pflg)) obvious = TRUE;
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
 * Dispel demons
 */
bool dispel_demon(int dam)
{
    spellswitch = 23; /* changes GF_DISP_UNDEAD to affect demons */
	return (project_los(GF_DISP_UNDEAD, dam));
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
		bool nowake = FALSE;

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;

		/* Skip aggravating monster (or player) */
		if (i == who) continue;
		
		/* some monsters are non-aggressive */
        if (r_ptr->sleep == 255) nowake = TRUE;
		
		/* Wake up nearby sleeping monsters */
        if ((cp_ptr->flags & CF_CLASS_SPEED) && (!nowake))
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
        else if (!nowake)
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
        else /* non-aggressive monsters */
        {
		    if (m_ptr->cdis < MAX_SIGHT * 2)
		    {
			   /* Wake up */
			   if ((m_ptr->csleep) && (!m_ptr->roaming))
			   {
				  /* start roaming */
				  m_ptr->roaming = 1;
				  sleep = TRUE;
			   }
		    }
        }

		/* Speed up monsters in line of sight */
		if ((player_has_los_bold(m_ptr->fy, m_ptr->fx)) && (!m_ptr->roaming))
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

/* alternate effect of the chance-realm banish/summon spell */
bool summon_chosen(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int ny, nx, die, die2;

	char typ;

	/* Get a monster symbol */
	if (!get_com("Choose a monster type (by symbol) to summon: ", &typ))
		return FALSE;

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

	/* Done */
	return TRUE;
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
	if (!get_com("Choose a monster race (by symbol) to banish: ", &typ))
		return FALSE;

	/* Delete the monsters of that "type" */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		bool damage = TRUE;

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;
		
		/* banishes even monsters which are temporarily dead */
		/* if (m_ptr->temp_death) continue; */

		/* Hack -- Skip Unique Monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Skip "wrong" monsters */
		if (r_ptr->d_char != typ) continue;

		/* Nonmonsters can be banished but do not add to damage */
        if (r_ptr->flags7 & (RF7_NONMONSTER)) damage = FALSE;

		/* Delete the monster */
		delete_monster_idx(i, FALSE);

		/* Take some damage */
		if (damage) dam += randint(4);
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
		
		/* banishes even monsters which are temporarily dead */
		/* if (m_ptr->temp_death) continue; */

		/* Skip distant monsters */
		if (m_ptr->cdis > MAX_SIGHT) continue;

		/* Nonmonsters (ordinary trees) aren't affected by mass banishment */
		if (r_ptr->flags7 & (RF7_NONMONSTER)) continue;

		/* Delete the monster */
		delete_monster_idx(i, FALSE);

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
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;

		/* Require line of sight (usually) */
		if (!player_has_los_bold(m_ptr->fy, m_ptr->fx))
		{
			/* occationally probe detected monsters which aren't in LOS */
			if ((!m_ptr->ml) || (m_ptr->cdis > 14) || (randint(100) > 15 + (goodluck*2))) continue;
		}

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
			delete_monster(y, x, TRUE);

			/* Destroy "valid" grids */
			if (cave_valid_bold(y, x))
			{
				int feat = FEAT_FLOOR;

				/* Delete objects */
				delete_object(y, x);

				/* Wall (or floor) type */
				t = rand_int(200);

				/* Granite */
				if (t < 22)
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

				/* Rubble */
				else if (t < 110)
				{
					/* Create rubble */
					feat = FEAT_RUBBLE;
					big_rocks(y, x);
				}

				/* Open pit */
				else if (t < 113)
				{
					/* Create rubble */
					feat = FEAT_OPEN_PIT;
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
			if ((spellswitch == 26) && (randint(100) < 50 - (goodluck/2))) (void)inc_timed(TMD_BLIND, 2 + randint(3));
			else (void)inc_timed(TMD_BLIND, 9 + randint(10));
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
 * Explode a grenade (spread effect) 
 * If caused by an earthquake, it may damage the PC as well as monsters.
 */
bool explode_grenade(int y, int x, const object_type *o_ptr, bool accident, int dam)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	int pflg = PROJO_SPRED;
	u32b f1, f2, f3, f4;
	int rad, bigrad, firstdam;
	int kaboom = GF_SHARD;
	
	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);
		
	/* damage is already figured if not an accident */
    if (accident)
	{
		dam = damroll(o_ptr->dd, o_ptr->ds) + o_ptr->to_d;
		firstdam = dam;
	
		/* apply brand (against monsters and the PC alike) */
	    if ((f1 & TR1_BRAND_POIS) && (randint(100) < 40))
	    {
			kaboom = GF_POIS;
			if (accident) dam *= 2;
			else dam *= 3;
	    }
		if ((f1 & TR1_BRAND_FIRE) && (randint(100) < 70))
			{kaboom = GF_FIRE; dam *= 3;}
		if ((f1 & TR1_BRAND_ELEC) && (randint(100) < 55))
			{kaboom = GF_ELEC; dam *= 3;}
		if ((f1 & TR1_BRAND_ACID) && (randint(100) < 55))
			{kaboom = GF_ACID; dam *= 3;}
		/* minimum multiplier  */
		if (firstdam == dam)
		{
			dam = (dam * 7) / 5;
		}
	}
							
	bigrad = 25;
    if ((f1 & TR1_BRAND_ACID) || (f1 & TR1_BRAND_ELEC) ||
		(f1 & TR1_BRAND_FIRE)) bigrad+= 30;
		
	if ((dam > 300 - (bigrad/4)) && (randint(100) < bigrad)) rad = 4;
	else if ((dam > 112 - (bigrad/2)) && (randint(100) < bigrad)) rad = 3;
	else rad = 2;

	/* exploded in an earthquake */
    if (accident)
	{
		/* Analyze the "dir" and the "target".  Sometimes hurt items on floor. */
		/* project from object itself */
		return (project(0, rad, y, x, dam, kaboom, flg, pflg));
	}
	/* PC threw the grenade */
	else 
	{
		/* Analyze the "dir" and the "target".  Sometimes hurt items on floor. */
		/* project from PC */
		return (project(-1, rad, y, x, dam, kaboom, flg, pflg));
	}
}


/*
 * Determine odds for object to break (rather than be buried) in an earthquake.
 * mild = true  means an undestroyed grid in an earthquake
 * (where potions are still likely to break)
 */
static int quake_break(const object_type *o_ptr, bool mild)
{
	/* int qbreak; */
	switch (o_ptr->tval)
	{
		/* Always break */
		case TV_POTION:
		case TV_FLASK:
		case TV_BOTTLE:
		{
			if (mild) return (75);
			else return (100);
		}
		/* almost always breaks */
		case TV_FOOD:
		case TV_BOW:
		{
			if (mild) return (50);
			else return (91);
		}
		/* Usually breaks */
		case TV_LITE:
		case TV_JUNK:
		case TV_ARROW:
        case TV_SKELETON:
		{
			if (mild) return (25);
			else return (75);
		}
		/* often break */
		case TV_SCROLL:
		case TV_SPECIAL:
		case TV_WAND:
		case TV_BOLT:
		case TV_SHOT:
		case TV_HAFTED:
		case TV_SWORD:
		case TV_AMULET:
		case TV_CROWN:
		case TV_STAFF:
		case TV_ROD:
		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_NEWM_BOOK:
		case TV_LUCK_BOOK:
		case TV_CHEM_BOOK:
		case TV_DARK_BOOK:
		/* case TV_MIND_BOOK: */
		{
			if (mild) return (0);
			else return (60);
		}
		/* less often breaks */
		case TV_POLEARM:
		case TV_SHIELD:
		case TV_HELM:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_RING:
		case TV_CLOAK:
		case TV_CHEST:
		{
			if (mild) return (0);
			else return (40);
		}
		/* sometimes breaks */
		case TV_DRAG_ARMOR:
		case TV_DIGGING:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			if (mild) return (0);
			else return (26);
		}
	}
	
	/* default */
	if (mild) return (0);
	else return (100);

	/* return (qbreak); */
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
 *
 * Strength of 0 uses default strength (85), which is what it always used
 * before I added that parameter. The lower the strength number, the stronger
 * the quake.
 *
 * Allowcrush = 0 means don't allow the PC to take 300pts damage because the PC
 * caused the earthquake.
 * Allowcrush = 1 means allow partial crush damage, not 300, but something smaller
 * -used for the earthquake trap because it would be too nasty to do 300pts damage
 * to the PC without warning as a trap.
 * Allowcrush = 2 is normal behavior (allowing the 300pts damage if the PC gets crushed).
 * Allowcrush = 4 will always destroy the PC's square, causing damage to the PC.
 * Allowcrush is irrevelent when the quake is centered on the PC (unless it's 4).
 *
 * Allowxp = TRUE means the PC gets experience for the damage done to monsters
 * from the quake because the PC cast an earthquake or ball/beam of destruction spell.
 * The PC never gets experience for crushed monsters who are automatically killed
 * because they have nowhere to escape to.
 */
void earthquake(int cy, int cx, int r, int strength, int allowcrush, bool allowxp)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int i, t, y, x, yy, xx, dy, dx, edist;
	int damage = 0;
	
	int sn = 0, sy = 0, sx = 0;

	bool hurt = FALSE;

	bool map[32][32];
		
	if (((allowcrush == 1) || (allowcrush == 4)) && (cave_info[cy][cx] & (CAVE_ICKY)))
		/* allow quakes in a vault if caused by a trap or the monster EXPLODE spell */;

	/* (usually) No effect in town or in a vault */
	else if ((!p_ptr->depth) || (cave_info[cy][cx] & (CAVE_ICKY)))
	{
		msg_print("The ground shakes for a moment.");
		return;
	}

	/* default strength */
	if (strength == 0) strength = 85;
	/* more powerful (lower strength#) makes it more likely that the player can't */
	/* find a safe spot to dodge into.  Don't make it too likely to cause big damage. */
	if ((strength < 55) && (allowcrush == 2) && 
		(randint(100) < (90-strength + goodluck))) allowcrush = 1;

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

			/* get distance */
			edist = distance(cy, cx, yy, xx);

			/* allow quakes in a vault if caused by a trap or the monster EXPLODE spell */
			/* (but makes it a less likely for any space to be affected) */
			if ((allowcrush == 4) && (yy == py) && (xx == px)) /* always blow up */;
			else if (((allowcrush == 1) || (allowcrush == 4)) && (randint(100) < 80)) /* */;
            /* should be able to use earthquake to break into a vault */
			/* (if near the centre of the quake) */
			else if ((cave_info[yy][xx] & (CAVE_ICKY)) && (randint(100) < 45) &&
				(cave_feat[yy][xx] < FEAT_PERM_EXTRA) && /* (walls only) */
				(cave_feat[yy][xx] > FEAT_RUBBLE)) edist *= 2;
			/* otherwise do not affect vaults */
			else if ((cave_info[yy][xx] & (CAVE_ICKY)) || 
				(cave_feat[yy][xx] >= FEAT_PERM_EXTRA)) continue;

			/* Skip distant grids */
			if (edist > r) continue;

			/* Lose room (not vault) */
			cave_info[yy][xx] &= ~(CAVE_ROOM);

			/* Lose light and knowledge */
			cave_info[yy][xx] &= ~(CAVE_GLOW | CAVE_MARK);
			
			/* allowcrush == 4 means always damage PC's grid */
			if ((allowcrush == 4) && (yy == py) && (xx == px))
			{
				/* Damage this grid */
				map[16+yy-cy][16+xx-cx] = TRUE;

				/* Hack -- Take note of player damage */
				if ((yy == py) && (xx == px)) hurt = TRUE;
			}

			/* Skip the epicenter */
			/* (never affects the PC if cast by (and centered on) the PC) */
			if (!dx && !dy) continue;

			/* Skip most grids (usually strength = 85) */
			/* rogue's blast area spell is short-radius but more powerful (strength = 35) */
			if (rand_int(100) < strength) continue;

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

			/* Skip non-empty grids (now allows rubble) */
			/* if (!cave_empty_bold(y, x)) continue; */
			if (!cave_can_occupy_bold(y, x)) continue;

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
		if ((!sn) && (allowcrush == 2))
		{
			/* Message and damage */
			msg_print("You are severely crushed!");
			damage = 300;
		}
		/* monster EXPLODE spell */
		else if ((!sn) && (allowcrush == 4))
		{
			/* Message and damage */
			msg_print("You are severely crushed!");
			damage = damroll(26, 9); /* max 234 */
		}
		/* earthquake trap */
		else if ((!sn) && (allowcrush == 1))
		{
			/* Message and damage */
			msg_print("You are nearly severely crushed!");
			damage = damroll(10, 5);
		}

		/* Destroy the grid, and push the player to safety */
		else
		{
			bool dodgeblast = FALSE;
			int howhard = randint(2) + 1;
			int blastthis = 14;
			if (allowcrush == 4) blastthis *= 2;
			/* reflex save */
			if (rand_int(blastthis + badluck/2) < adj_dex_dis[p_ptr->stat_ind[A_DEX]])
				howhard = 1;
			
            /* Calculate results */
			switch (howhard)
			{
				case 1:
				{
					/* now determined by DEX */
                    msg_print("You nimbly dodge the blast!");
					damage = 0;
					hurt = FALSE;
					break;
				}
				case 2:
				{
					msg_print("You are bashed by rubble!");
					damage = damroll(8, 4);
					(void)inc_timed(TMD_STUN, randint(40));
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
			if (sn) monster_swap(py, px, sy, sx);
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
			s16b this_o_idx, next_o_idx = 0;
			int qbreak;
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;

			/* Skip unaffected grids */
			if (!map[16+yy-cy][16+xx-cx])
			{
/* #if thiscrashes */
				/* some fragile objects get destroyed even in unaffected grids */
				for (this_o_idx = cave_o_idx[yy][xx]; this_o_idx; this_o_idx = next_o_idx)
				{
					/* get the object */
					object_type *o_ptr = &o_list[this_o_idx];

					/* this fixes the crash bug (thanks to Zaimoni) */
					if (!((0 < this_o_idx) && (this_o_idx < z_info->o_max))) break;

					/* Get the next object */
					next_o_idx = o_ptr->next_o_idx;

					/* get odds for object to break (TRUE makes things much less likely to break) */
					qbreak = quake_break(o_ptr, TRUE);
					if (artifact_p(o_ptr)) qbreak = 0;

					/* roll for destruction */
					if (rand_int(100) < qbreak)
					{
						/* Extract the flags (to check for EXPLODE_A) */
						u32b f1, f2, f3, f4;
						object_flags(o_ptr, &f1, &f2, &f3, &f4);
						char o_name[80];
						bool exploded = FALSE;
						int boomchance = 12 + badluck;
						object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);
						/* grenades much more likely to explode than exploding arrows */
						if ((f3 & TR3_THROWN) || (f3 & TR3_PTHROW)) boomchance += 63;

						/* possible explosion */
						if ((f4 & TR4_EXPLODE_A) && (rand_int(100) < boomchance))
						{
							explode_grenade(yy, xx, o_ptr, TRUE, 2);
							exploded = TRUE;
						}
						
						/* message (only if you can see the object) */
						if ((player_can_see_bold(yy, xx)) && (!squelch_hide_item(o_ptr)))
						{
							if (exploded) msg_format("The %s explodes!", o_name);
							else msg_format("The %s breaks!", o_name);
						}

						delete_object_idx(this_o_idx);
					}
				}
/* #endif */
				continue;
			}

			/* Paranoia -- never affect player */
			if ((yy == py) && (xx == px)) continue;

			/* Destroy location (if valid) */
			if (cave_valid_bold(yy, xx))
			{
				int feat = FEAT_FLOOR;
				bool floor = cave_floor_bold(yy, xx);

				/* Delete most objects, but leave some durable ones buried */
				/* delete_object(yy, xx); */
				for (this_o_idx = cave_o_idx[yy][xx]; this_o_idx; this_o_idx = next_o_idx)
				{
					/* get the object */
					object_type *o_ptr = &o_list[this_o_idx];

					/* this fixes the crash bug (thanks to Zaimoni) */
					if (!((0 < this_o_idx) && (this_o_idx < z_info->o_max))) break;

					/* Get the next object */
					next_o_idx = o_ptr->next_o_idx;

					/* get odds for object to break */
					qbreak = quake_break(o_ptr, FALSE);
					if (artifact_p(o_ptr)) qbreak = 0;

					/* roll for destruction */
					if (rand_int(100) < qbreak)
					{
						/* msg_format("qbreak %d ", qbreak); (testing) */
						delete_object_idx(this_o_idx);
					}
					/* if it isn't destroyed then it's buried (and hidden) */
					else
					{
						o_ptr->hidden = 1;
						o_ptr->marked = FALSE;
						lite_spot(yy, xx);
					}
				}

				/* Wall (or floor) type */
				t = rand_int(105);
				if (!floor) t += 95;
				/* t = (floor ? rand_int(105) : 200); */

				/* Granite */
				if (t < 30)
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
				else if (t < 91)
				{
					/* Create magma vein */
					feat = FEAT_MAGMA;
				}

				/* Rubble */
				else if ((t < 100) || ((t >= 105) && (t < 110)))
				{
					/* Create rubble */
					feat = FEAT_RUBBLE;
					big_rocks(yy, xx);
				}

				/* earthquakes should have a chance to create pits */
				else if (t < 105)
				{
					/* a pit that isn't a trap (and cannot be disarmed) */
					feat = FEAT_OPEN_PIT;
				}
				
				/* Change the feature */
				cave_set_feat(yy, xx, feat);
			}
		}
	}


	/* Examine the quaked region (monsters) */
	/* do monsters after feature changes so that monsters can flee into */
	/* newly created empty spaces or rubble */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			bool noflee = FALSE; /* no need to flee */
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;

			/* Skip unaffected grids */
			if (!map[16+yy-cy][16+xx-cx]) continue;

			/* Skip okay grids */
			if (cave_feat[yy][xx] == FEAT_FLOOR) continue;

			/* Process monsters */
			if (cave_m_idx[yy][xx] > 0)
			{
				monster_type *m_ptr = &mon_list[cave_m_idx[yy][xx]];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];
				bool xpkill = FALSE;

				/* Most monsters cannot co-exist with rock */
				if (!(r_ptr->flags2 & (RF2_KILL_WALL)) &&
				    !(r_ptr->flags2 & (RF2_PASS_WALL)))
				{
					char m_name[80];

					/* Assume not safe */
					sn = 0;

					/* no need to move if on rubble (but still takes some damage) */
					if (cave_feat[yy][xx] == FEAT_RUBBLE) noflee = TRUE;

					/* Monster can move to escape the wall */
					if (!(r_ptr->flags1 & (RF1_NEVER_MOVE)))
					{
						/* Look for safety */
						for (i = 0; i < 8; i++)
						{
							/* Get the grid */
							y = yy + ddy_ddd[i];
							x = xx + ddx_ddd[i];

							/* Skip non-empty grids (now allows rubble) */
							/* if (!cave_empty_bold(y, x)) continue; */
							if (!cave_can_occupy_bold(y, x)) continue;

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

					/* Scream in pain (this happens in mon_take_hit if allowxp) */
					if (!allowxp) msg_format("%^s wails out in pain!", m_name);

					/* Take damage from the quake */
					if (noflee) damage = damroll(3, 8);
					else if ((!sn) && (randint(100) < 35)) damage = damroll(6, 8);
					else if (!sn) damage = m_ptr->hp + 1;
					else damage = damroll(4, 8);

					/* hurt the monster & give xp */
					if ((allowxp) && (sn))
					{
						bool fear = FALSE;
						cptr note_dies;
						note_dies = " is crushed in the earthquake.";
						if (mon_take_hit(cave_m_idx[yy][xx], damage, &fear, note_dies))
						{
							/* player killed monster with earthquake */
							xpkill = TRUE;
						}
					}
					/* hurt the monster directly without giving xp */
					else
					{
						if ((r_ptr->sleep == 255) && (m_ptr->csleep))
						{
							/* non-aggressive monsters start roaming */
                            if (!m_ptr->roaming) m_ptr->roaming = 1;
						}
						else
						{
							/* Monster is certainly awake */
							m_ptr->csleep = 0;
							m_ptr->roaming = 0;
						}

						/* Apply damage directly */
						m_ptr->hp -= damage;
					}

					/* xpkill means monster is dead & PC got XP */
					if (!xpkill)
					{
						/* Hack -- Escape from the rock */
						if ((sn) && (m_ptr->hp >= 0) && (!noflee))
						{
							/* Move the monster */
							monster_swap(yy, xx, sy, sx);
						}
						/* no safe place, but monster can't stay where there's going to be a wall */
						/* (not automatic kill) */
						else if ((!sn) && (m_ptr->hp >= 0) && (!noflee))
						{
							teleport_away(cave_m_idx[yy][xx], 3, 0);
						}

						/* Delete (not kill) "dead" monsters */
						if (m_ptr->hp < 0)
						{
							/* Message */
							msg_format("%^s is embedded in the rock!", m_name);
	
							/* Delete the monster */
							delete_monster(yy, xx, TRUE);
						}

						/* No longer safe */
						sn = 0;
					}
				}
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
	
	return;
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
			int chance = 25;
			bool nowake = FALSE;

			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			
			/* some monsters are non-aggressive */
			if ((r_ptr->sleep == 255) ||
				(r_ptr->flags7 & (RF7_NONMONSTER))) nowake = TRUE;

			/* Smart monsters always wake up */
			if (r_ptr->flags2 & (RF2_SMART)) chance = 100;

			/* Stupid monsters rarely wake up */
			if ((r_ptr->flags2 & (RF2_STUPID)) && (m_ptr->roaming)) chance = 20;
			else if (r_ptr->flags2 & (RF2_STUPID)) chance = 10;
			else if (m_ptr->roaming) chance += 25;

			/* Sometimes monsters wake up */
			if (m_ptr->csleep && (rand_int(100) < chance) && (!nowake))
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
			/* non-agressive monsters just start roaming */
			else if (m_ptr->csleep && (rand_int(100) < chance) && (nowake))
			{
				if ((!m_ptr->roaming) && (!(r_ptr->flags7 & (RF7_NONMONSTER))))
				{
					char m_name[80];

					/* Get the monster name */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					m_ptr->roaming = 1;
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
		   int pflg = 0;
			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			if (r_ptr->flags3 & (RF3_HURT_DARK))
			{
				/* Jump directly to the target monster */
				(void)project(-1, 0, y, x, dam, GF_DARK_WEAK, flg, pflg);
			}
        }
	}

	/* None left */
	temp_n = 0;
}


/*
 * return TRUE if next to a room grid
 * (This makes it so the walls don't have to have CAVE_ROOM)
 */
static int temp_aux_aux(int y1, int x1)
{
	int i, y, x;
	bool nextto = FALSE;

	/* Scan adjacent grids */
	for (i = 0; i < 8; i++)
	{
		/* Extract the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		/* Next to a room */
		if (cave_info[y][x] & (CAVE_ROOM)) nextto = TRUE;
	}

	/* return TRUE if next to a room grid */
	return (nextto);
}


/*
 * Aux function -- see below
 */
static void cave_temp_room_aux(int y, int x)
{
	/* Avoid infinite recursion */
	if (cave_info[y][x] & (CAVE_TEMP)) return;

	/* Do not "leave" the current room */
	/* if (!(cave_info[y][x] & (CAVE_ROOM))) return; */
	if (!temp_aux_aux(y, x)) return;

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
	bool second = FALSE;

	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);

	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get lit, but stop light */
		if (!cave_floor_bold(y, x)) continue;
		
		/* doors mark the end of a room, */
		/* so they should stop room light even if they're open */
		if (((cave_feat[y][x] == FEAT_OPEN) || (cave_feat[y][x] == FEAT_BROKEN)) &&
			(second)) continue;
		
		/* don't light distant grids */
        /* in case of a huge vault counted as one room (like minetown) */
        if (distance(y1, x1, y, x) >= MAX_SIGHT+4) continue;

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
		
		second = TRUE;
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
		
		/* doors mark the end of a room, so they should stop room light */
        if ((cave_feat[y][x] == FEAT_OPEN) || (cave_feat[y][x] == FEAT_BROKEN))
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
	int pflg = 0;

	int flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->timed[TMD_BLIND])
	{
		msg_print("You are surrounded by a white light.");
	}

	/* Lite up the room */
	if (spellswitch != 4) lite_room(py, px);

	/* Hook into the "project()" function */
	(void)project(-1, rad, py, px, dam, GF_LITE_WEAK, flg, pflg);

	/* Assume seen */
	return (TRUE);
}


/*
 * Hack -- call darkness around the player
 * Affect all monsters in the projection radius
 */
bool unlite_area(int dam, int rad, bool strong)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int pflg = 0;

	int flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->timed[TMD_BLIND])
	{
		msg_print("Darkness surrounds you.");
	}

	/* Hook into the "project()" function */
	if (strong) (void)project(-1, rad, py, px, dam, GF_DARK, flg, pflg);
	else (void)project(-1, rad, py, px, dam, GF_DARK_WEAK, flg, pflg);

	/* Darken the room */
	/* necromancer's 'call dark' only sometimes darkens the whole room */
	if (strong)
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

	int ty, tx, pflg = 0;

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
	return (project(-1, rad, ty, tx, dam, typ, flg, pflg));
}

/*
 * spread effect: ball centered on the PC
 * usually wide radius
 * PROJO_SPRED makes damage decrease with radius much slower than with a ball spell.
 * Spreads are also 1/4 as likely to destroy objects on the floor as ball spells.
 */
bool fire_spread(int typ, int dam, int rad)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	int pflg = PROJO_SPRED;

	/* Analyze the "dir" and the "target".  Sometimes hurt items on floor. */
	return (project(-1, rad, p_ptr->py, p_ptr->px, dam, typ, flg, pflg));
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

	int ty, tx, pflg = 0;

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
		if (project(-1, rad, ty, tx, dam, typ, flg, pflg)) noticed = TRUE;
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

	int ty, tx, pflg = 0;

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
	return (project(-1, 0, ty, tx, dam, typ, flg, pflg));
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

/* stone to mud / tunneldigging */
/* change to take a damage parameter, so Orome's stone to mud can do more damage */
bool wall_to_mud(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_THRU;
	int dam = 25 + randint(25);
	if (spellswitch == 31) dam = 40 + randint(40);
	return (project_hook(GF_KILL_WALL, dir, dam, flg));
}
/* for cannonball activation (lots of damage to HURT_ROCK monsters) */
bool golem_to_mud(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_THRU;
	int dam = 80 + randint(50 + goodluck);
	spellswitch = 31;
	return (project_hook(GF_KILL_WALL, dir, dam, flg));
}

/* calls stone to mud only if the spell is targetting a wall and there is nothing in between.
 * This is a side effect of certain spells: ball of destruction and rocket blast
 * (I'm currently not using the return bool value, but I thought I might later)
 */
bool blast_a_wall(int dir)
{
	int tx, ty;
	bool do_blast = FALSE;
	ty = p_ptr->target_row;
	tx = p_ptr->target_col;
	/* doors */
	if ((cave_feat[ty][tx] >= FEAT_DOOR_HEAD) && (cave_feat[ty][tx] <= FEAT_DOOR_TAIL))
	    do_blast = TRUE;
	/* non-permanent walls */
	if ((cave_feat[ty][tx] >= FEAT_SECRET) && (cave_feat[ty][tx] <= FEAT_WALL_SOLID))
	    do_blast = TRUE;
	
	/* this makes sure there's nothing in the way */
    if ((clean_shot(p_ptr->py, p_ptr->px, ty, tx, TRUE)) && (do_blast))
	{
	    return wall_to_mud(dir);
	}
	else return FALSE;
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

bool heal_monster(int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_HEAL, dir, dam, flg));
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

bool poly_monster(int dir, int pwr)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_POLY, dir, pwr, flg));
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
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */

/* door creation mode (10=locked, 0=unlocked) */
bool door_creation(int mode)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int pflg = 0;

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 1, py, px, mode, GF_MAKE_DOOR, flg, pflg));
}

bool trap_creation(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int pflg = 0;

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 1, py, px, 0, GF_MAKE_TRAP, flg, pflg));
}

bool destroy_doors_touch(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int pflg = 0;

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;

	if (spellswitch == 6) return (project(-1, 9, py, px, 0, GF_KILL_TRAP, flg, pflg));
	else return (project(-1, 1, py, px, 0, GF_KILL_DOOR, flg, pflg));
}

bool sleep_monsters_touch(int power)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int pflg = 0;

	int flg = PROJECT_KILL | PROJECT_HIDE;
	return (project(-1, 1, py, px, power, GF_OLD_SLEEP, flg, pflg));
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
		
        /* remove blessing from bless object spell */
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
 * It now only uses the old method of turning the item into a brand
 * ego for ammo.  For any other weapon, it uses a new method
 * which allows branding ego items and even artifacts.
 */
void brand_object(object_type *o_ptr, int brand_type, int duration, bool enchantit)
{
	bool autofail = FALSE;
	bool ammo = FALSE;
	int failchance = 0;
	cptr act = "magical";
	char o_name[80];

	/* Extract the flags */
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);
	
	/* only ammo uses the old method */
    if ((o_ptr->tval >= TV_SHOT) && (o_ptr->tval <= TV_BOLT)) ammo = TRUE;

	/* allow branding on ammo of wounding */
	if ((ego_item_p(o_ptr)) && (o_ptr->name2 == EGO_WOUNDING)) autofail = FALSE;
	/* automatically fails on ego ammo */
	/* (there is no artifact ammo, but allow for it just in case) */
	else if ((ammo) && ((ego_item_p(o_ptr)) || (artifact_p(o_ptr)))) 
		autofail = TRUE;
		
	/* already has an active temporary branding enchantment */
	if ((o_ptr->timedbrand) && (o_ptr->thisbrand))
	{
		msg_print("That item already has a brand enchantment! ");
		autofail = TRUE;
	}

    if (ego_item_p(o_ptr))
	{
		/* always fail if it already has the same brand you're trying to add */
		if ((o_ptr->name2 == brand_type) ||
			(o_ptr->name2 == EGO_RAGING_ELEMENTS)) autofail = TRUE;
		else if ((f1 & (TR1_BRAND_POIS)) && (brand_type == EGO_BRAND_POIS))
			autofail = TRUE;
		else if ((f1 & (TR1_BRAND_FIRE)) && (brand_type == EGO_BRAND_FIRE))
			autofail = TRUE;
		else if ((f1 & (TR1_BRAND_COLD)) && (brand_type == EGO_BRAND_COLD))
			autofail = TRUE;
		else if ((f1 & (TR1_BRAND_ELEC)) && (brand_type == EGO_BRAND_ELEC))
			autofail = TRUE;
		/* (spells which acid brand do not and should not exist) */
		
		/* already-branded items resist enchantment */
		else if ((f1 & (TR1_BRAND_ACID)) || (f1 & (TR1_BRAND_ELEC)) ||
			(f1 & (TR1_BRAND_FIRE)) || (f1 & (TR1_BRAND_COLD)) ||
			(f1 & (TR1_BRAND_POIS))) failchance = 150 - (3 * p_ptr->lev);
	}
	/* artifacts and cursed items resist enchantment */
	if (artifact_p(o_ptr)) failchance = 150 - (2 * p_ptr->lev);
	if (cursed_p(o_ptr)) failchance += 16 + badluck;
	
	if ((randint(100) < failchance) || (autofail) || (!o_ptr->k_idx))
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
	if (brand_type == EGO_ACID_COAT) msg_format("An acidic coating covers the %s.", o_name);
	else msg_format("A %s aura surrounds the %s.", act, o_name);

	/* new method (melee always, and sometimes ammo) */
	if (duration)
	{
		/* set brand type */
        switch (brand_type)
		{
			case EGO_BRAND_FIRE:
			case EGO_FLAME:
				o_ptr->thisbrand = 2;
				break;
			case EGO_BRAND_COLD:
			case EGO_FROST:
				o_ptr->thisbrand = 1;
				break;
			case EGO_BRAND_POIS:
			case EGO_AMMO_VENOM:
				o_ptr->thisbrand = 5;
				break;
			case EGO_BRAND_ELEC:
			case EGO_AMMO_ELEC:
				o_ptr->thisbrand = 3;
				break;
			case EGO_ACID_COAT:
				o_ptr->thisbrand = 6;
				break;
			/* o_ptr->thisbrand == 4 is for acid brand which is unused */
		}
		/* set duration */
		o_ptr->timedbrand = duration;
	}
	/* (usually) do ammo the old way */
	else
	{
		/* Brand the object */
		o_ptr->name2 = brand_type;
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);
	
	/* not all branding spells also enchant the weapon */
    if (!enchantit) return;
	
	/* Enchant (enchant less for acid coating or poison brand spells) */
	if ((brand_type == EGO_ACID_COAT) || (brand_type == EGO_AMMO_VENOM)) 
		enchant(o_ptr, randint(3), ENCH_TOHIT | ENCH_TODAM, FALSE);
	else enchant(o_ptr, randint(3) + 3, ENCH_TOHIT | ENCH_TODAM, FALSE);
}


/*
 * Brand the current melee weapon
 * mode = 0: normal
 * mode = 1: force frost brand
 * mode = 2: poison brand
 * mode = 3: branding scroll (chance of lightning brand)
 * (the mode parameter replaces the use of the ugly spellswitch hack)
 */
void brand_weapon(int mode)
{
	int brand_type;
	object_type *o_ptr = &inventory[INVEN_WIELD];
	int die = rand_int(100);
	bool enchantit = TRUE;

	/* Extract the flags */
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Select a brand */
	if ((mode == 3) && (die < 16 + goodluck) && (!(f1 & (TR1_BRAND_ELEC))))
		brand_type = EGO_BRAND_ELEC;
	else if (((die < 67) || (f1 & (TR1_BRAND_FIRE))) &&
		(!(f1 & (TR1_BRAND_COLD))))
		brand_type = EGO_BRAND_COLD;
	else
		brand_type = EGO_BRAND_FIRE;

	if (mode == 1) brand_type = EGO_BRAND_COLD;
	if (mode == 2) brand_type = EGO_BRAND_POIS;

	/* branding scrolls don't enchant the weapon */
    if (mode == 3) enchantit = FALSE;
		
	/* Brand the weapon (fix duration later) */
	brand_object(o_ptr, brand_type, 180 + randint(p_ptr->lev*2 + 20), enchantit);
}


/*
 * Hook to specify ammo
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

/* Hook to specify arrows (quickie) */
static bool item_tester_hook_arrow(const object_type *o_ptr)
{
	if (o_ptr->tval == TV_ARROW) return (TRUE);
	else return (FALSE);
}

/*
 * Hook to specify ammo or thrown weapons
 */
static bool item_tester_hook_projectile(const object_type *o_ptr)
{
	/* Get flags to check for THROWN flag */
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);
	if (f3 & TR3_THROWN) return TRUE;

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
 * mode = 0: normal (frost or fire)
 * mode = 1: acid coating alchemy spell
 * mode = 2: assassin's poisoning spell
 * mode = 3: archer's poisoning spell (arrows only)
 * (the mode parameter replaces the use of the ugly spellswitch hack)
 */
bool brand_ammo(int mode)
{
	int item, r;
	object_type *o_ptr;
	cptr q, s;
	int brand_type;
	bool enchantit = TRUE;

	/* Only accept ammo */
	item_tester_hook = item_tester_hook_ammo;
	/* assassins can also poison thrown weapons */
	if (mode == 2) item_tester_hook = item_tester_hook_projectile;
	/* archers can only poison arrows */
	if (mode == 3) item_tester_hook = item_tester_hook_arrow;

	/* Get an item */
	if (mode == 1)
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

	if (mode == 2) brand_type = EGO_AMMO_VENOM;

    if (mode == 1)
	{
		brand_type = EGO_ACID_COAT;
		/* acid coat alchemy spell doesn't always enchant also */
		if (rand_int(100) > p_ptr->lev/2 + 26 + goodluck) enchantit = FALSE;
	}

	/* Brand the ammo */
	/* mode 1-2 are temporary, other modes are permanent */
	if ((mode == 1) || (mode == 2))
		brand_object(o_ptr, brand_type, 180 + randint(p_ptr->lev*2 + 20), enchantit);
	else brand_object(o_ptr, brand_type, 0, TRUE);

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
	brand_object(o_ptr, EGO_FLAME, 360, TRUE);

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
	brand_object(o_ptr, EGO_FROST, 360, TRUE);

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
	/* (score and note when you find an artifact */
	/* is now included in object_known() ) */
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
