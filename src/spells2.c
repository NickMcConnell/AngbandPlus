/* File: spells2.c */

/* Purpose: Spell code (part 2) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 *
 * James E. Wilson and Robert A. Koeneke have released all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2 or any later version),
 * or under the terms of the traditional Angband license.
 *
 * All changes in Hellband are Copyright (c) 2005-2007 Konijn
 * I Konijn  release all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2),
 * or under the terms of the traditional Angband license.
 */

#include "angband.h"

/* Chance of using syllables to form the name instead of the "template" files */
/* TABLE_NAME is kept out of fear of breaking something */
#define TABLE_NAME      45
#define A_CURSED        13
#define WEIRD_LUCK      12
#define BIAS_LUCK       20
/* Bias luck needs to be higher than weird luck, since it is usually tested
several times... */
#define ACTIVATION_CHANCE 3

extern int artefact_bias;

extern s32b flag_cost(object_type * o_ptr, int plusses);


/*
* Increase players hit points, notice effects
*/
bool hp_player(int num)
{
	/* Healing needed */
	if (p_ptr->chp < p_ptr->mhp || num < 0)
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

        if( num < 0 )
        {
            msg_print("You feel drained.");
        }
		/* Heal 0-4 */
		else if (num < 5)
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
 * Increase players hit points, notice effects
 */
bool sp_player(int num)
{
	/* Healing needed */
	if (p_ptr->csp < p_ptr->msp || num < 0)
	{
		/* Gain hitpoints */
		p_ptr->csp += num;

		/* Enforce maximum */
		if (p_ptr->csp >= p_ptr->msp)
		{
			p_ptr->csp = p_ptr->msp;
			p_ptr->csp_frac = 0;
		}

		/* Enforce minimum */
		if (p_ptr->csp < 0)
		{
			p_ptr->csp = 0;
			p_ptr->csp_frac = 0;
		}

		/* Redraw */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

        if( num < 0 )
        {
            msg_print("You mind feels drained.");
        }
		/* Heal 0-34 */
		else if (num < 35)
		{
			msg_print("You feel a glow inside.");
		}

		/* Heal 35+ */
		else
		{
			msg_print("Your mind radiates.");
		}

		/* Notice */
		return (TRUE);
	}

	/* Ignore */
	return (FALSE);
}

/*Heals the body with mana points, not more than needed, not more than available*/
void mind_leech(void)
{
    /* Calculate how much we need to leech */
    int dif = p_ptr->mhp - p_ptr->chp;
    /* Make sure we will not get out mana in negative */
    dif = (dif > p_ptr->csp )?p_ptr->csp:dif;
    /* Gain health */
    (void)hp_player(dif);
    /* Loose mana */
    (void)sp_player(-dif);
}

/*Heals the mind with hit points, not more than needed, making sure we dont die ( yet )*/
void body_leech(void)
{
    /* Calculate how much we need to leech */
    int dif = p_ptr->msp - p_ptr->csp;
    /* Make sure we will not get out mana in negative */
    dif = (dif >= p_ptr->chp )?p_ptr->chp-1:dif;
    /* Gain mana */
    (void)sp_player(dif);
    /* Gain health */
    (void)hp_player(-dif);
}

/*
* Leave a "glyph of warding" which prevents monster movement
*/
void warding_glyph(void)
{
	/* XXX XXX XXX */
	if (!cave_clean_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return;
	}

	/* Create a glyph */
	cave_set_feat(py, px, FEAT_GLYPH);
}

void explosive_rune(void)
{
	/* XXX XXX XXX */
	if (!cave_clean_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return;
	}

	/* Create a glyph */
	cave_set_feat(py, px, FEAT_MINOR_GLYPH);
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
	case A_CHA: if (p_ptr->sustain_cha) sust = TRUE; break;
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
		msg_format("You feel very %s.", desc_stat_neg[stat]);

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
		msg_format("Wow!  You feel very %s!", desc_stat_pos[stat]);

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
	int                 i;

	/* Simply identify and know every item */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr,TRUE);
	}
	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
}






/*
* Used by the "enchant" function (chance of failure)
*/
static int enchant_table[16] =
{
	0, 10,  50, 100, 200,
		300, 400, 500, 650, 800,
		950, 987, 993, 995, 998,
		1000
};


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
	int             i, cnt = 0;

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
		if (!all && (f3 & (TR3_HEAVY_CURSE))) continue;

		/* Perma-Cursed Items can NEVER be uncursed */
		if (f3 & (TR3_PERMA_CURSE)) continue;

		/* Uncurse it */
		o_ptr->ident &= ~(IDENT_CURSED);

		/* Hack -- Assume felt */
		o_ptr->ident |= (IDENT_SENSE);

		if (o_ptr->art_flags3 & (TR3_CURSED))
			o_ptr->art_flags3 &= ~(TR3_CURSED);

		if (o_ptr->art_flags3 & (TR3_HEAVY_CURSE))
			o_ptr->art_flags3 &= ~(TR3_HEAVY_CURSE);

		/* Take note */
		o_ptr->note = quark_add("uncursed");

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


bool alchemy(void) /* Turns an object into gold, gain some of its value in a shop */
{
	int                     item, amt = 1;
	int                     old_number;
	long        price;

	bool            force = FALSE;

	object_type             *o_ptr;

	char            o_name[80];

	char            out_val[160];


	/* Hack -- force destruction */
	if (command_arg > 0) force = TRUE;


	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Turn which item to gold? ", "You have nothing to turn to gold.", USE_INVEN | USE_FLOOR))
	{
		return FALSE;
	}

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


	/* See how many items */
	if (o_ptr->number > 1)
	{
		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number,TRUE);

		/* Allow user abort */
		if (amt <= 0) return FALSE;
	}


	/* Describe the object */
	old_number = o_ptr->number;
	o_ptr->number = amt;
	object_desc(o_name, o_ptr, TRUE, 3);
	o_ptr->number = old_number;

	/* Verify unless quantity given */
	if (!force)
	{
		if (!((auto_destroy) && (object_value(o_ptr)<1)))
		{
			/* Make a verification */
			sprintf(out_val, "Really turn %s to gold? ", o_name);
			if (!get_check(out_val)) return FALSE;
		}
	}

	/* Artifacts cannot be destroyed */
	if (artefact_p(o_ptr) || o_ptr->art_name)
	{
		cptr feel = "special";

		/* Message */
		msg_format("You fail to turn %s to gold!", o_name);

		/* Hack -- Handle icky artefacts */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) feel = "terrible";

		/* Hack -- inscribe the artefact */
		o_ptr->note = quark_add(feel);

		/* We have "felt" it (again) */
		o_ptr->ident |= (IDENT_SENSE);

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return FALSE;
	}

	price = object_value_real(o_ptr);

	if (price <= 0)
		/* Message */
		msg_format("You turn %s to fool's gold.", o_name);
	else
	{
		price /= 3;

		if (amt > 1) price *= amt;

		if (price > 30000) price = 30000;

        /*To add a touch of cruelty, the object gets id'd, so that we see what we actually destroyed, muhahah */
        /* Identify it fully */
        object_aware(o_ptr);
        object_known(o_ptr,FALSE);
        /* We have "felt" it in every way */
        o_ptr->ident |= (IDENT_MENTAL);
        /* Redescribe it with possibly new knowledge */
       	object_desc(o_name, o_ptr, TRUE, 3);

		msg_format("You turn %s to %ld coins worth of gold.", o_name, price);
		p_ptr->au += price;

		/* Redraw gold */
		p_ptr->redraw |= (PR_GOLD);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

	}

	/* Eliminate the item (from the pack) */
	if (item >= 0)
	{
		inven_item_increase(item, -amt);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Eliminate the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -amt);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	return TRUE;
}




/*
* self-knowledge... idea from nethack.  Useful for determining powers and
* resistences of items.  It saves the screen, clears it, then starts listing
* attributes, a screenful at a time.  (There are a LOT of attributes to
* list.  It will probably take 2 or 3 screens for a powerful character whose
* using several artefacts...) -CFT
*
* It is now a lot more efficient. -BEN-
*
* See also "identify_fully()".
*
* XXX XXX XXX Use the "show_file()" method, perhaps.
*/
void self_knowledge(void)
{
	int             i = 0, j, k;

	int corruption_counter;
	int timed_counter;

	u32b *muta_class = 0;

	u32b    f1 = 0L, f2 = 0L, f3 = 0L;

	object_type     *o_ptr;

	char dummy[80];
	char sign_ability[80];
	char race_ability[80];

	cptr    info[128];
	cptr    timed_info[TIMED_COUNT];

	int plev = p_ptr->lev;

	/*Paranoid*/
	strcpy (dummy, "");
	strcpy (sign_ability, "");
	strcpy (race_ability, "");

	/* Acquire item flags from equipment */
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

	/* Birth sign powers ... */
	if(p_ptr->psign)
	{
		for( j = 0 ; sign_powers[j].description != NULL ; j++ )
		{
			if( sign_powers[j].idx == p_ptr->psign && plev >= sign_powers[j].level )
			{
				/*Show power and start description with cost*/
				sprintf(sign_ability,"You can %s (cost %d", sign_powers[j].description , sign_powers[j].cost );
				/*Show cost that rises with level if there is one*/
				if( racial_powers[j].cost_level > 0 )
					sprintf(sign_ability,"%s + lvl/%d", sign_ability, sign_powers[j].cost_level );
				/*Show extra info if there is*/
				if( racial_powers[j].info != NULL )
					sprintf(sign_ability,"%s, %s", sign_ability, sign_powers[j].info );
				/*End with stat used*/
				sprintf(sign_ability,"%s, %s based)", sign_ability, stats_short[sign_powers[j].stat] );
				info[i++] = sign_ability;
			}
		}
	}


	for( j = 0 ; racial_powers[j].description != NULL ; j++ )
	{
		if( racial_powers[j].idx == p_ptr->prace && plev >= racial_powers[j].level )
		{
			/*Show power and start description with cost*/
			sprintf(race_ability,"You can %s (cost %d", racial_powers[j].description , racial_powers[j].cost );
			/*Show cost that rises with level if there is one*/
			if( racial_powers[j].cost_level > 0 )
				sprintf(race_ability,"%s + lvl/%d", race_ability, racial_powers[j].cost_level );
			/*Show extra info if there is*/
			if( racial_powers[j].info != NULL )
				sprintf(race_ability,"%s, %s", race_ability, racial_powers[j].info );
			/*End with stat used*/
			sprintf(race_ability,"%s, %s based)", race_ability, stats_short[racial_powers[j].stat] );
			info[i++] = race_ability;
		}
	}

	/* Handle corruptions */
	/* Because of ancient history, the other corruption descriptions start with a space, we skip that space with the +1*/
	/* Also because of ancient history we are not directly accessing the corruptions flag, TODO */

	for( corruption_counter = 0 ; corruption_counter < COUNT_CORRUPTIONS ; corruption_counter++ )
	{
		muta_class = corruption_idx_to_u32b( corruptions[corruption_counter].idx );
		if (*(muta_class) & corruptions[corruption_counter].bitflag )
		{
			info[i++] = (corruptions[corruption_counter].description)+1;
		}
	}

	/* Handle timed effects */

	for( timed_counter = 0 ; timed_counter < TIMED_COUNT ; timed_counter++)
	{
		if(  *(timed[timed_counter].timer) > 0   )
		{
			info[i] = NULL;
			timed_info[i++] = timed[timed_counter].status;
		}
	}

	if (p_ptr->aggravate)   info[i++] = "You aggravate monsters.";
	if (p_ptr->teleport)    info[i++] = "Your position is very uncertain.";
	if (p_ptr->confusing)   info[i++] = "Your hands are glowing dull red.";
	if (p_ptr->searching)   info[i++] = "You are looking around very carefully.";
	if (p_ptr->new_spells)  info[i++] = "You can learn some spells/prayers.";
	if (p_ptr->word_recall) info[i++] = "You will soon be recalled.";
	if (p_ptr->see_infra)   info[i++] = "Your eyes are sensitive to infrared light.";
	if (p_ptr->see_inv)     info[i++] = "You can see invisible creatures.";
	if (p_ptr->ffall)       info[i++] = "You can fly.";
	if (p_ptr->free_act)    info[i++] = "You have free action.";
	if (p_ptr->regenerate)  info[i++] = "You regenerate quickly.";
	if (p_ptr->slow_digest) info[i++] = "Your appetite is small.";
	if (p_ptr->telepathy)   info[i++] = "You have ESP.";
	if (p_ptr->hold_life)   info[i++] = "You have a firm hold on your life force.";
	if (p_ptr->reflect)     info[i++] = "You reflect arrows and bolts.";
	if (p_ptr->sh_fire)     info[i++] = "You are surrounded with a fiery aura.";
	if (p_ptr->sh_elec)     info[i++] = "You are surrounded with electricity.";
	if (p_ptr->anti_magic)  info[i++] = "You are surrounded by an anti-magic shell.";
	if (p_ptr->anti_tele)   info[i++] = "You cannot teleport.";
	if (p_ptr->lite)        info[i++] = "You are carrying a permanent light.";

	if (p_ptr->immune_acid)		   info[i++] = "You are completely immune to acid.";
	else if ((p_ptr->resist_acid)
		  && (p_ptr->oppose_acid)) info[i++] = "You resist acid exceptionally well.";
	else if ((p_ptr->resist_acid)) info[i++] = "You are resistant to acid.";

	if (p_ptr->immune_elec)        info[i++] = "You are completely immune to lightning.";
	else if ((p_ptr->resist_elec)
		 &&  (p_ptr->oppose_elec)) info[i++] = "You resist lightning exceptionally well.";
	else if ((p_ptr->resist_elec)) info[i++] = "You are resistant to lightning.";

	if (p_ptr->immune_fire)        info[i++] = "You are completely immune to fire.";
	else if ((p_ptr->resist_fire)
		  && (p_ptr->oppose_fire)) info[i++] = "You resist fire exceptionally well.";
	else if ((p_ptr->resist_fire)) info[i++] = "You are resistant to fire.";

	if (p_ptr->immune_cold)        info[i++] = "You are completely immune to cold.";
	else if ((p_ptr->resist_cold)
		  && (p_ptr->oppose_cold)) info[i++] = "You resist cold exceptionally well.";
	else if ((p_ptr->resist_cold)) info[i++] = "You are resistant to cold.";

	if ((p_ptr->resist_pois) && (p_ptr->oppose_pois)) info[i++] = "You resist poison exceptionally well.";
	else if ((p_ptr->resist_pois))	info[i++] = "You are resistant to poison.";

	/* Higher resistances */
	if (p_ptr->resist_lite)  info[i++] = "You are resistant to bright light.";
	if (p_ptr->resist_dark)  info[i++] = "You are resistant to darkness.";
	if (p_ptr->resist_conf)  info[i++] = "You are resistant to confusion.";
	if (p_ptr->resist_sound) info[i++] = "You are resistant to sonic attacks.";
	if (p_ptr->resist_disen) info[i++] = "You are resistant to disenchantment.";
	if (p_ptr->resist_chaos) info[i++] = "You are resistant to chaos.";
	if (p_ptr->resist_shard) info[i++] = "You are resistant to blasts of shards.";
	if (p_ptr->resist_nexus) info[i++] = "You are resistant to nexus attacks.";
	if (p_ptr->resist_neth)  info[i++] = "You are resistant to nether forces.";
	if (p_ptr->resist_fear)  info[i++] = "You are completely fearless.";
	if (p_ptr->resist_blind) info[i++] = "Your eyes are resistant to blindness.";
	/* Sustains */
	if (p_ptr->sustain_str) info[i++] = "Your strength is sustained.";
	if (p_ptr->sustain_int) info[i++] = "Your intelligence is sustained.";
	if (p_ptr->sustain_wis) info[i++] = "Your wisdom is sustained.";
	if (p_ptr->sustain_con) info[i++] = "Your constitution is sustained.";
	if (p_ptr->sustain_dex) info[i++] = "Your dexterity is sustained.";
	if (p_ptr->sustain_cha) info[i++] = "Your charisma is sustained.";
	/* Stat boosters */
	if (f1 & (TR1_STR)) info[i++] = "Your strength is affected by your equipment.";
	if (f1 & (TR1_INT)) info[i++] = "Your intelligence is affected by your equipment.";
	if (f1 & (TR1_WIS)) info[i++] = "Your wisdom is affected by your equipment.";
	if (f1 & (TR1_DEX)) info[i++] = "Your dexterity is affected by your equipment.";
	if (f1 & (TR1_CON)) info[i++] = "Your constitution is affected by your equipment.";
	if (f1 & (TR1_CHA)) info[i++] = "Your charisma is affected by your equipment.";
	/* Skill boosters */
	if (f1 & (TR1_STEALTH)) info[i++] = "Your stealth is affected by your equipment.";
	if (f1 & (TR1_SEARCH))  info[i++] = "Your searching ability is affected by your equipment.";
	if (f1 & (TR1_INFRA))   info[i++] = "Your infravision is affected by your equipment.";
	if (f1 & (TR1_TUNNEL))  info[i++] = "Your digging ability is affected by your equipment.";
	if (f1 & (TR1_SPEED))   info[i++] = "Your speed is affected by your equipment.";
	if (f1 & (TR1_BLOWS))   info[i++] = "Your attack speed is affected by your equipment.";

	/* Access the current weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Analyze the weapon */
	if (o_ptr->k_idx)
	{
		/* Specials */
		if (f3 & (TR3_BLESSED))		info[i++] = "Your weapon has been blessed by the gods.";
		if (f1 & (TR1_CHAOTIC))		info[i++] = "Your weapon is branded with Abyssal runes.";
		if (f1 & (TR1_IMPACT))		info[i++] = "The impact of your weapon can cause earthquakes.";
		if (f1 & (TR1_VORPAL))		info[i++] = "Your weapon is very sharp.";
		if (f1 & (TR1_VAMPIRIC))	info[i++] = "Your weapon drains life from your foes.";
		/* Brands */
		if (f1 & (TR1_BRAND_ACID))	info[i++] = "Your weapon melts your foes.";
		if (f1 & (TR1_BRAND_ELEC))	info[i++] = "Your weapon shocks your foes.";
		if (f1 & (TR1_BRAND_FIRE))	info[i++] = "Your weapon burns your foes.";
		if (f1 & (TR1_BRAND_COLD))	info[i++] = "Your weapon freezes your foes.";
		if (f1 & (TR1_BRAND_POIS))	info[i++] = "Your weapon poisons your foes.";
		/* Special slays */
		if (f1 & (TR1_SLAY_ANIMAL))	info[i++] = "Your weapon strikes at animals with extra force.";
		if (f1 & (TR1_SLAY_EVIL))	info[i++] = "Your weapon strikes at evil with extra force.";
		if (f1 & (TR1_SLAY_UNDEAD))	info[i++] = "Your weapon strikes at undead with holy wrath.";
		if (f1 & (TR1_SLAY_DEMON))	info[i++] = "Your weapon strikes at demons with holy wrath.";
		if (f1 & (TR1_SLAY_ANGEL))	info[i++] = "Your weapon is especially deadly against fallen angels.";
		if (f1 & (TR1_SLAY_GIANT))	info[i++] = "Your weapon is especially deadly against giants.";
		if (f1 & (TR1_SLAY_DRAGON))	info[i++] = "Your weapon is especially deadly against dragons.";
		/* Special banes */
		if (f1 & (TR1_KILL_DRAGON))	info[i++] = "Your weapon is a great bane of dragons.";
		if (f1 & (TR1_KILL_ANGEL))	info[i++] = "Your weapon is a great bane of fallen angels";
		if (f1 & (TR1_KILL_ANGEL))	info[i++] = "Your weapon protects you from Lucifers' curse.";
	}

	/* Save the screen */
	Term_save();

	/* Erase the screen */
	for (k = 1; k < 24; k++) prt("", k, 13);

	/* Label the information */
	prt("     Your Attributes:", 1, 15);

	/* We will print on top of the map (column 13) */
	for (k = 2, j = 0; j < i; j++)
	{

		if( info[j] != NULL )
		{
			/* Show the info */
			prt(info[j], k++, 15);
		}
		else
		{
			/*Prepare and dump the info */
			sprintf(dummy, "You are temporarily %s.", timed_info[j]);
			prt(dummy, k++, 15);

		}

		/* Every 20 entries (lines 2 to 21), start over */
		if ((k == 22) && (j+1 < i))
		{
			prt("-- more --", k, 15);
			inkey();
			for (; k > 2; k--) prt("", k, 15);
		}
	}

	/* Pause */
	prt("[Press any key to continue]", k, 13);
	inkey();

	/* Restore the screen */
	Term_load();
}


/*
* Forget everything
*/
bool lose_all_info(void)
{
	int                 i;

	/* Forget info about objects */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Allow "protection" by the MENTAL flag */
		if (o_ptr->ident & (IDENT_MENTAL)) continue;

		/* Remove "default inscriptions" */
		if (o_ptr->note && (o_ptr->ident & (IDENT_SENSE)))
		{
			/* Access the inscription */
			cptr q = quark_str(o_ptr->note);

			/* Hack -- Remove auto-inscriptions */
			if ((streq(q, "cursed")) ||
				(streq(q, "broken")) ||
				(streq(q, "good")) ||
				(streq(q, "average")) ||
				(streq(q, "excellent")) ||
				(streq(q, "worthless")) ||
				(streq(q, "special")) ||
				(streq(q, "terrible")))
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
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	/* Mega-Hack -- Forget the map */
	wiz_dark();

	/* It worked */
	return (TRUE);
}




/*
* Detect all traps on current panel
*/
bool detect_traps(void)
{
	int y, x;

	bool detect = FALSE;

	cave_type *c_ptr;


	/* Scan the current panel */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			/* Access the grid */
			c_ptr = &cave[y][x];

			/* Detect invisible traps */
			if (c_ptr->feat == FEAT_INVIS)
			{
				/* Pick a trap */
				pick_trap(y, x);
			}

			/* Detect traps */
			if ((c_ptr->feat >= FEAT_TRAP_HEAD) &&
				(c_ptr->feat <= FEAT_TRAP_TAIL))
			{
				/* Hack -- Memorize */
				c_ptr->info |= (CAVE_MARK);

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

	cave_type *c_ptr;


	/* Scan the panel */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			c_ptr = &cave[y][x];

			/* Detect secret doors */
			if (c_ptr->feat == FEAT_SECRET)
			{
				/* Pick a door XXX XXX XXX */
				replace_secret_door(y,x);
			}

			/* Detect doors */
			if (((c_ptr->feat >= FEAT_DOOR_HEAD) &&
				(c_ptr->feat <= FEAT_DOOR_HEAD)) ||
				((c_ptr->feat == FEAT_OPEN) ||
				(c_ptr->feat == FEAT_BROKEN)))
			{
				/* Hack -- Memorize */
				c_ptr->info |= (CAVE_MARK);

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

	cave_type *c_ptr;


	/* Scan the panel */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			c_ptr = &cave[y][x];

			/* Detect stairs */
			if ((c_ptr->feat == FEAT_LESS) ||
				(c_ptr->feat == FEAT_MORE))
			{
				/* Hack -- Memorize */
				c_ptr->info |= (CAVE_MARK);

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

	cave_type *c_ptr;


	/* Scan the current panel */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			c_ptr = &cave[y][x];

			/* Notice embedded gold */
			if ((c_ptr->feat == FEAT_MAGMA_H) ||
				(c_ptr->feat == FEAT_QUARTZ_H))
			{
				/* Expose the gold */
				c_ptr->feat += 0x02;
			}

			/* Magma/Quartz + Known Gold */
			if ((c_ptr->feat == FEAT_MAGMA_K) ||
				(c_ptr->feat == FEAT_QUARTZ_K))
			{
				/* Hack -- Memorize */
				c_ptr->info |= (CAVE_MARK);

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

	if (detect_monsters_string("$*"))
	{
		detect = TRUE;
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
			detect = TRUE;
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of objects!");
	}

	if (detect_monsters_string("!=?|"))
	{
		detect = TRUE;
	}

	/* Result */
	return (detect);
}


/*
* Detect all "magic" objects on the current panel.
*
* This will light up all spaces with "magic" items, including artefacts,
* ego-items, potions, scrolls, books, rods, wands, staves, amulets, rings,
* and "enchanted" items of the "good" variety.
*
* It can probably be argued that this function is now too powerful.
*/
bool detect_objects_magic(bool detect_entire_floor, bool do_instant_pseudo_id)
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

		/* Only detect nearby objects unless we really do want the entire floor */
		if (!detect_entire_floor && !panel_contains(y,x)) continue;

		/* Examine the tval */
		tv = o_ptr->tval;

		/* Artifacts, misc magic items, or enchanted wearables */
		if (artefact_p(o_ptr) || ego_item_p(o_ptr) || o_ptr->art_name ||
			(tv == TV_AMULET) || (tv == TV_RING) ||
			(tv == TV_STAFF) || (tv == TV_WAND) || (tv == TV_ROD) ||
			(tv == TV_SCROLL) || (tv == TV_POTION) ||
			(tv == TV_MIRACLES_BOOK) || (tv == TV_SORCERY_BOOK) ||
			(tv == TV_NATURE_BOOK) || (tv == TV_CHAOS_BOOK) ||
			(tv == TV_DEATH_BOOK) || (tv == TV_SOMATIC_BOOK) ||
			(tv == TV_TAROT_BOOK) || (tv == TV_CHARMS_BOOK) ||  (tv == TV_DEMONIC_BOOK) ||
			((o_ptr->to_a > 0) || (o_ptr->to_h + o_ptr->to_d > 0)))
		{
			/* Memorize the item */
			o_ptr->marked = TRUE;

            /* Pseudo-id it */
            if(do_instant_pseudo_id)
            {
                cptr feel;
                /* Check for a feeling */
                feel = value_check_aux1(o_ptr);
                if(feel)
                {
                    /* We have "felt" it */
                    o_ptr->ident |= (IDENT_SENSE);
                    /* Inscribe it textually */
                    if (!o_ptr->note) o_ptr->note = quark_add(feel);
                    /* Describe the object again with the added quark */
                }
            }

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
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
	int             i, y, x;

	bool    flag = FALSE;


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
		if ((!(r_ptr->flags2 & (RF2_INVISIBLE)))
			|| p_ptr->see_inv || p_ptr->tim_invis)
		{
			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Hack -- See monster */
			m_ptr->ml = TRUE;

			/* Redraw */
			lite_spot(y, x);

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
	int             i, y, x;

	bool    flag = FALSE;


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

		/* Detect invisible monsters */
		if (r_ptr->flags2 & (RF2_INVISIBLE))
		{
			/* Take note that they are invisible */
			r_ptr->r_flags2 |= (RF2_INVISIBLE);

			/* Update monster recall window */
			if (monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Hack -- See monster */
			m_ptr->ml = TRUE;

			/* Redraw */
			lite_spot(y, x);

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
	int             i, y, x;

	bool    flag = FALSE;


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

		/* Detect evil monsters */
		if (r_ptr->flags3 & (RF3_EVIL))
		{
			/* Take note that they are evil */
			r_ptr->r_flags3 |= (RF3_EVIL);

			/* Update monster recall window */
			if (monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Hack -- See monster */
			m_ptr->ml = TRUE;

			/* Redraw */
			lite_spot(y, x);

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
* Detect all (string) monsters on current panel
*/
bool detect_monsters_string(cptr Match)
{
	int             i, y, x;

	bool    flag = FALSE;


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

		/* Detect evil monsters */
		if (strchr(Match, r_ptr->d_char))

		{

			/* Update monster recall window */
			if (monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Hack -- See monster */
			m_ptr->ml = TRUE;

			/* Redraw */
			lite_spot(y, x);

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
* A "generic" detect monsters routine, tagged to flags3
*/
bool detect_monsters_xxx(u32b match_flag)
{
	int             i, y, x;

	bool    flag = FALSE;
	cptr desc_monsters = "weird monsters";


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

		/* Detect evil monsters */
		if (r_ptr->flags3 & (match_flag))
		{
			/* Take note that they are something */
			r_ptr->r_flags3 |= (match_flag);

			/* Update monster recall window */
			if (monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Hack -- See monster */
			m_ptr->ml = TRUE;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		switch (match_flag)
		{ case RF3_DEMON:
		desc_monsters = "demons";
		break;
		case RF3_UNDEAD:
			desc_monsters = "the undead";
			break;
		}
		/* Describe result */
		msg_format("You sense the presence of %s!", desc_monsters);
		msg_print(NULL);
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
* Create stairs at the player location
*/
void stair_creation(void)
{
	/* XXX XXX XXX */
	if (!cave_valid_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return;
	}

	/* XXX XXX XXX */
	delete_object(py, px);

	/* Create a staircase */
	if (dun_level <= 0)
	{
		return;
	}
	else if (is_quest(dun_level) || (dun_level >= MAX_DEPTH))
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
bool item_tester_hook_armour(object_type *o_ptr)
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
    if( o_ptr->to_a != 0 )
	{
			return (TRUE);
	}

	return (FALSE);
}



/*
* Enchants a plus onto an item.                        -RAK-
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

	bool a = (artefact_p(o_ptr) || o_ptr->art_name);

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
		if (rand_int(prob) >= 100) continue;

		/* Flag, to see whether either 0 or 999 kicks in */
		chance = -1;

		/* Auto success if we enchant something broken / cursed */
		if( ((eflag & ENCH_TOHIT) && o_ptr->to_h < 0 ) ||
			((eflag & ENCH_TODAM) && o_ptr->to_d < 0 ) ||
			((eflag & ENCH_TOAC ) && o_ptr->to_a < 0 ) ) chance = 0;

		/* We dont like higher than 15 at all.. */
		if( ((eflag & ENCH_TOHIT) && o_ptr->to_h > 15 ) ||
		    ((eflag & ENCH_TODAM) && o_ptr->to_d > 15 ) ||
		    ((eflag & ENCH_TOAC ) && o_ptr->to_a > 15 ) ) chance = 999;

		if( chance == -1 )
		{
			int divider = 0;
			int total = 0;
			if (eflag & ENCH_TOHIT){ divider++; total += o_ptr->to_h; }
			if (eflag & ENCH_TODAM){ divider++; total += o_ptr->to_d; }
			if (eflag & ENCH_TOAC ){ divider++; total += o_ptr->to_a; }
			if(!divider)
				return FALSE; /* Odd, no enchant flag was passed */
			if(total<0)
				total = 0; /* Paranoia */
			total = total / divider;
			chance = enchant_table[ total ];
		}

		if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
		{
			/* No more enchanting beyond level 15 */
			if( (eflag & ENCH_TOHIT) && o_ptr->to_h < 15 ) o_ptr->to_h++;
			if( (eflag & ENCH_TODAM) && o_ptr->to_d < 15 ) o_ptr->to_d++;
			if( (eflag & ENCH_TOAC ) && o_ptr->to_a < 15 ) o_ptr->to_a++;
			res = TRUE;

			/* only when you get it above -1 -CFT , non-perma-cursed and appropriate enchantment type */
			if ( cursed_p(o_ptr) &&
				 (!(f3 & (TR3_PERMA_CURSE))) &&
				 (
					((o_ptr->to_h >= 0) && (eflag & ENCH_TOHIT) && (rand_int(100) < 25)) ||
					((o_ptr->to_d >= 0) && (eflag & ENCH_TODAM) && (rand_int(100) < 25)) ||
					((o_ptr->to_a >= 0) && (eflag & ENCH_TOAC)  && (rand_int(100) < 25))
				 )
			   )
			{
				msg_print("The curse is broken!");
				o_ptr->ident &= ~(IDENT_CURSED);
				o_ptr->ident |= (IDENT_SENSE);
				if (o_ptr->art_flags3 & (TR3_CURSED)){
					o_ptr->art_flags3 &= ~(TR3_CURSED);
				}
				if (o_ptr->art_flags3 & (TR3_HEAVY_CURSE)){
					o_ptr->art_flags3 &= ~(TR3_HEAVY_CURSE);
				}
				o_ptr->note = quark_add("uncursed");
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
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

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
	int                     item;
	bool            okay = FALSE;

	object_type             *o_ptr;

	char            o_name[80];

	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Enchant armour if requested */
	if (num_ac) item_tester_hook = item_tester_hook_armour;

	/* Get an item (from equip or inven or floor) */
	if (!get_item(&item, "Enchant which item? ", "You have nothing to enchant.", USE_FLOOR | USE_EQUIP | USE_INVEN))
	{
		return (FALSE);
	}

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
	if (enchant(o_ptr, num_ac , ENCH_TOAC )) okay = TRUE;

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


void curse_artefact (object_type * o_ptr)
{
	if (o_ptr->pval) o_ptr->pval = 0 - ((o_ptr->pval) + rand_s16b(4));
	if (o_ptr->to_a) o_ptr->to_a = 0 - ((o_ptr->to_a) + rand_s16b(4));
	if (o_ptr->to_h) o_ptr->to_h = 0 - ((o_ptr->to_h) + rand_s16b(4));
	if (o_ptr->to_d) o_ptr->to_d = 0 - ((o_ptr->to_d) + rand_s16b(4));
	o_ptr->art_flags3 |= ( TR3_HEAVY_CURSE | TR3_CURSED );
	if (randint(4)==1) o_ptr-> art_flags3 |= TR3_PERMA_CURSE;
	if (randint(3)==1) o_ptr-> art_flags3 |= TR3_TY_CURSE;
	if (randint(2)==1) o_ptr-> art_flags3 |= TR3_AGGRAVATE;
	if (randint(3)==1) o_ptr-> art_flags3 |= TR3_DRAIN_EXP;
	if (randint(2)==1) o_ptr-> art_flags3 |= TR3_TELEPORT;
	else if (randint(3)==1) o_ptr->art_flags3 |= TR3_NO_TELE;
	if( randint(3)==1)	o_ptr->art_flags3 |= TR3_NO_MAGIC;
	o_ptr->ident |= IDENT_CURSED;

}

/* Helper function for the art_resistance_table*/
static int can_have_sheath(object_type * o_ptr)
{
	return (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR);
}

/* Helper function for the art_resistance_table*/
static int can_reflect(object_type * o_ptr)
{
	return (o_ptr->tval == TV_SHIELD || o_ptr->tval == TV_CLOAK || o_ptr->tval == TV_HELM || o_ptr->tval == TV_HARD_ARMOR);
}

/* Helper function for the art_misc_table*/
static int do_weapon_ac(object_type * o_ptr)
{
	if (o_ptr->tval<TV_BOOTS)
	{
		o_ptr->art_flags3 |= TR3_SHOW_MODS;
		o_ptr->to_a = 4 + (rand_s16b(11));
		return TRUE;
	}
	return FALSE;
}

/* Helper function for the art_slay_table*/
static int is_sword(object_type * o_ptr)
{
	return (o_ptr->tval == TV_SWORD);
}

static int not_bow(object_type * o_ptr)
{
	return (o_ptr->tval != TV_BOW);
}


/* Helper function for the art_misc_table*/
static int do_fight_plusses(object_type * o_ptr)
{
	o_ptr->art_flags3 |= TR3_SHOW_MODS;
	o_ptr->to_h += 4 + (rand_s16b(11));
	o_ptr->to_d += 4 + (rand_s16b(11));
	return TRUE;
}

/* Local defines for the resistance_biases_table */
#define ALWAYS   1
#define ONE_IN_2 2
#define ONE_IN_3 3
#define LUCKY    20
#define NO_BIAS 0

static void apply_bias_table( art_bias_entry table[] , object_type * o_ptr, bool is_scroll, int specific)
{
	int i , idx;
	int total_odds = 0;
	art_bias_entry entry;

	/* Assign a resistance based on perceived bias */
	if( !specific && artefact_bias )
	{
		/* Minor hack : trailer record has 0 as flag_odds */
		for( i = 0 ; table[i].bias ; i++ )
		{
			/* Sugar the entry */
			entry = table[i];
			/* Does the bias match and do we feel lucky and do we have the proper object ? */
			if( artefact_bias == entry.bias && randint( entry.flag_odds ) && ( entry.func == NULL || entry.func( o_ptr ) ) )
			{
				/* Konijn promised only 1 of those will ever be true ;] */
				if( entry.flag1 && !(o_ptr->art_flags1 & entry.flag1) )	o_ptr->art_flags1 |= entry.flag1;
				if( entry.flag2 && !(o_ptr->art_flags2 & entry.flag2) )	o_ptr->art_flags2 |= entry.flag2;
				if( entry.flag3 && !(o_ptr->art_flags3 & entry.flag3) )	o_ptr->art_flags3 |= entry.flag3;
				/* There is 50% odds that we dont add anything else */
				if (randint(2)==1) return;
			}
		}
	}

	/* Calculate the addbias odds */
	for( i = 0 ; table[i].flag_odds ; i++ )
	{
		total_odds += table[i].random_odds;
		table[i].calculated_odds = total_odds;
	}

	/* decided what resistance we go for */
	idx = (specific?specific:randint(43));

	/* Find an assign the resistance*/
	for( i = 0 ; table[i].flag_odds ; i++ )
	{
		if( table[i].calculated_odds >= idx )
		{
			/* Sugar the entry */
			entry = table[i];
			/* Are we lucky enough and do we have the right equipment ? */
			if( randint( entry.flag_odds ) && ( entry.func == NULL || entry.func( o_ptr ) ) )
			{
				/* Konijn promised only 1 of those will ever be true ;] */
				if( entry.flag1 && !(o_ptr->art_flags1 & entry.flag1) )	o_ptr->art_flags1 |= entry.flag1;
				if( entry.flag2 && !(o_ptr->art_flags2 & entry.flag2) )	o_ptr->art_flags2 |= entry.flag2;
				if( entry.flag3 && !(o_ptr->art_flags3 & entry.flag3) )	o_ptr->art_flags3 |= entry.flag3;
				if( !artefact_bias && randint( entry.addbias_odds ) == 1 )
					artefact_bias = entry.bias;
			}
			else
			{	/* If we are unlucky, or unfit, we try again, but something else ( infinite loop alert ) */
				apply_bias_table( table , o_ptr, is_scroll, randint(43));
			}
			/* Avoid infinite loops ;] */
			break;
		}
	}
}

art_bias_entry art_resistance_table[] =
{
  { 3     , ALWAYS    , BIAS_ACID        , ALWAYS   , 0 , TR2_RES_ACID   , 0            , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_ACID        , LUCKY    , 0 , TR2_IM_ACID    , 0            , NULL            , 0 },
  { 3     , ALWAYS    , BIAS_ELEC        , ALWAYS   , 0 , TR2_RES_ELEC   , 0            , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_ELEC        , ALWAYS   , 0 , 0              , TR3_SH_ELEC  , can_have_sheath , 0 },
  { 1     , ALWAYS    , BIAS_ELEC        , LUCKY    , 0 , TR2_IM_ELEC    , 0            , NULL            , 0 },
  { 3     , ALWAYS    , BIAS_FIRE        , ALWAYS   , 0 , TR2_RES_FIRE   , 0            , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_FIRE        , ALWAYS   , 0 , 0              , TR3_SH_FIRE  , can_have_sheath , 0 },
  { 1     , ALWAYS    , BIAS_FIRE        , LUCKY    , 0 , TR2_IM_FIRE    , 0            , NULL            , 0 },
  { 3     , ALWAYS    , BIAS_COLD        , ALWAYS   , 0 , TR2_RES_COLD   , 0            , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_COLD        , LUCKY    , 0 , TR2_IM_COLD    , 0            , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_POIS        , ALWAYS   , 0 , TR2_RES_POIS   , 0            , NULL            , 0 }, /* counterparts rogue & necromantic */
  { 1     , ALWAYS    , BIAS_WARRIOR     , ONE_IN_2 , 0 , TR2_RES_FEAR   , 0            , NULL            , 0 }, /* counterpart under priestly */
  { 0     , ALWAYS    , BIAS_WARRIOR     , ONE_IN_3 , 0 , 0              , TR3_NO_MAGIC , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_PRIESTLY    , ONE_IN_2 , 0 , TR2_RES_FEAR   , 0            , NULL            , 0 }, /* counterpart under warrior */
  { 1     , ALWAYS    , BIAS_PRIESTLY    , ALWAYS   , 0 , TR2_RES_LITE   , 0            , NULL            , 0 },
  { 2     , ONE_IN_3  , BIAS_NECROMANTIC , ALWAYS   , 0 , TR2_RES_NETHER , 0            , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_NECROMANTIC , ALWAYS   , 0 , TR2_RES_POIS   , 0            , NULL            , 0 }, /* counterparts rogue & poison */
  { 0     , ALWAYS    , BIAS_NECROMANTIC , ALWAYS   , 0 , TR2_RES_DARK   , 0            , NULL            , 0 },
  { 2     , ONE_IN_2  , BIAS_CHAOS       , ALWAYS   , 0 , TR2_RES_CHAOS  , 0            , NULL            , 0 },
  { 1     , ONE_IN_2  , BIAS_CHAOS       , ALWAYS   , 0 , TR2_RES_CONF   , 0            , NULL            , 0 }, /* counterpart under mage */
  { 0     , ALWAYS    , BIAS_CHAOS       , ALWAYS   , 0 , TR2_RES_DISEN  , 0            , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_MAGE        , ONE_IN_2 , 0 , TR2_RES_CONF   , 0            , NULL            , 0 }, /* counterpart under chaos */
  { 2     , ALWAYS    , BIAS_MAGE        , ONE_IN_2 , 0 , TR2_RES_BLIND  , 0            , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_ROGUE       , ONE_IN_2 , 0 , TR2_RES_CONF   , 0            , NULL            , 0 },
  { 0     , ALWAYS    , BIAS_ROGUE       , ONE_IN_2 , 0 , TR2_FREE_ACT   , 0            , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_ROGUE       , ONE_IN_2 , 0 , TR2_RES_POIS   , 0            , NULL            , 0 }, /* counterparts poison & necromantic */
  { 2     , ALWAYS    , NO_BIAS          , ALWAYS   , 0 , TR2_RES_SOUND  , 0            , NULL            , 0 },
  { 2     , ALWAYS    , NO_BIAS          , ALWAYS   , 0 , TR2_RES_SHARDS , 0            , NULL            , 0 },
  { 2     , ALWAYS    , NO_BIAS          , ALWAYS   , 0 , TR2_RES_NEXUS  , 0            , NULL            , 0 },
  { 2     , ALWAYS    , NO_BIAS          , ALWAYS   , 0 , TR2_RES_DISEN  , 0            , NULL            , 0 },
  { 1     , ALWAYS    , NO_BIAS          , ALWAYS   , 0 , TR2_REFLECT    , 0            , can_reflect     , 0 },
  { 1     , ALWAYS    , NO_BIAS          , ALWAYS   , 0 , TR2_RES_DARK   , 0            , NULL            , 0 },
  { 0     , 0         , 0                , 0        , 0 , 0              , 0            , NULL            , 0 },
};

/* 42 in total , used to be 41 */
void random_resistance (object_type * o_ptr, bool is_scroll, int specific)
{
	(void)apply_bias_table( art_resistance_table , o_ptr , is_scroll , specific );
}

art_bias_entry art_misc_table[] =
{
  { 1     , ALWAYS    , BIAS_RANGER        , ALWAYS   , 0 , TR2_SUST_CON   , 0            , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_STR           , ALWAYS   , 0 , TR2_SUST_STR   , 0            , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_DEX           , ALWAYS   , 0 , TR2_SUST_DEX   , 0            , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_CON           , ALWAYS   , 0 , TR2_SUST_CON   , 0            , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_INT           , ALWAYS   , 0 , TR2_SUST_INT   , 0            , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_WIS           , ALWAYS   , 0 , TR2_SUST_WIS   , 0            , NULL            , 0 },
  { 1     , ALWAYS    , BIAS_CHA           , ALWAYS   , 0 , TR2_SUST_CHA   , 0            , NULL            , 0 },
  { 0     , ALWAYS    , BIAS_CHAOS         , ALWAYS   , 0 , 0              , TR3_TELEPORT , NULL            , 0 },
  { 0     , ALWAYS    , BIAS_FIRE          , ALWAYS   , 0 , 0              , TR3_LITE     , NULL            , 0 },
  { 3     , ALWAYS    , NO_BIAS            , ALWAYS   , 0 , TR2_FREE_ACT   , 0            , NULL            , 0 },
  { 2     , ALWAYS    , NO_BIAS            , ALWAYS   , 0 , 0              , TR3_LITE     , NULL            , 0 },
  { 2     , ALWAYS    , NO_BIAS            , ALWAYS   , 0 , 0              , TR3_FEATHER  , NULL            , 0 },
  { 3     , ALWAYS    , NO_BIAS            , ALWAYS   , 0 , 0              , TR3_SEE_INVIS, NULL            , 0 },
  { 2     , ALWAYS    , NO_BIAS            , ALWAYS   , 0 , 0              , TR3_SLOW_DIGEST, NULL          , 0 },
  { 2     , ALWAYS    , NO_BIAS            , ALWAYS   , 0 , 0              , TR3_REGEN    , NULL            , 0 },
  { 1     , ALWAYS    , NO_BIAS            , ALWAYS   , 0 , 0              , TR3_TELEPORT , NULL            , 0 },
  { 1     , ALWAYS    , NO_BIAS            , ALWAYS   , 0 , 0              , TR3_NO_MAGIC , NULL            , 0 },
  { 1     , ALWAYS    , NO_BIAS            , ALWAYS   , 0 , 0              , TR3_NO_TELE  , NULL            , 0 },
  { 3     , ALWAYS    , NO_BIAS            , ALWAYS   , 0 , 0              , 0            , do_weapon_ac    , 0 },
  { 3     , ALWAYS    , NO_BIAS            , ALWAYS   , 0 , 0              , 0            , do_fight_plusses, 0 },
  { 1     , 5         , BIAS_PRIESTLY      , ALWAYS   , 0 , TR2_HOLD_LIFE  , 0            , NULL            , 0 },
  { 1     , 6         , BIAS_NECROMANTIC   , ALWAYS   , 0 , TR2_HOLD_LIFE  , 0            , NULL            , 0 },
  { 1     , 9         , BIAS_MAGE          , ALWAYS   , 0 , 0              , TR3_TELEPATHY, NULL            , 0 },
  { 0     , 0         , 0                  , 0        , 0 , 0              , 0            , NULL            , 0 },
};


void random_misc (object_type * o_ptr, bool is_scroll)
{
	(void)apply_bias_table( art_misc_table , o_ptr , is_scroll , 0 );
}

art_bias_entry art_slay_table[] =
{
  { 1 , ALWAYS , BIAS_CHAOS      , ALWAYS , TR1_CHAOTIC     , 0 , 0           , NULL , 0 },
  { 0 , ALWAYS , BIAS_PRIESTLY   , ALWAYS , 0               , 0 , TR3_BLESSED , NULL , 0 },
  { 1 , ALWAYS , BIAS_NECROMANTIC, ALWAYS , TR1_VAMPIRIC    , 0 , 0           , NULL , 0 },
  { 1 , ALWAYS , BIAS_NECROMANTIC, ALWAYS , TR1_BRAND_POIS  , 0 , 0           , NULL , 0 },
  { 0 , ALWAYS , BIAS_RANGER     , ALWAYS , TR1_SLAY_ANIMAL , 0 , 0           , NULL , 0 },
  { 1 , ALWAYS , BIAS_ROGUE      , ALWAYS , TR1_BRAND_POIS  , 0 , 0           , NULL , 0 },
  { 1 , ALWAYS , BIAS_POIS       , ALWAYS , TR1_BRAND_POIS  , 0 , 0           , NULL , 0 },
  { 2 , ALWAYS , BIAS_FIRE       , ALWAYS , TR1_BRAND_FIRE  , 0 , 0           , NULL , 0 },
  { 2 , ALWAYS , BIAS_COLD       , ALWAYS , TR1_BRAND_COLD  , 0 , 0           , NULL , 0 },
  { 2 , ALWAYS , BIAS_ELEC       , ALWAYS , TR1_BRAND_ELEC  , 0 , 0           , NULL , 0 },
  { 2 , ALWAYS , BIAS_ACID       , ALWAYS , TR1_BRAND_ACID  , 0 , 0           , NULL , 0 },
  { 0 , ALWAYS , BIAS_LAW        , ALWAYS , TR1_SLAY_EVIL   , 0 , 0           , NULL , 0 },
  { 0 , ALWAYS , BIAS_LAW        , ALWAYS , TR1_SLAY_UNDEAD , 0 , 0           , NULL , 0 },
  { 0 , ALWAYS , BIAS_LAW        , ALWAYS , TR1_SLAY_DEMON  , 0 , 0           , NULL , 0 },
  { 2 , ALWAYS , NO_BIAS         , ALWAYS , TR1_SLAY_ANIMAL , 0 , 0           , NULL , 0 },
  { 2 , ALWAYS , NO_BIAS         , ALWAYS , TR1_SLAY_ANGEL  , 0 , 0           , NULL , 0 },
  { 2 , ALWAYS , NO_BIAS         , ALWAYS , TR1_SLAY_GIANT  , 0 , 0           , NULL , 0 },
  { 2 , ALWAYS , NO_BIAS         , ALWAYS , TR1_SLAY_DRAGON , 0 , 0           , NULL , 0 },
  { 2 , ALWAYS , NO_BIAS         , ALWAYS , TR1_KILL_ANGEL  , 0 , 0           , NULL , 0 },
  { 1 , ALWAYS , NO_BIAS         , ALWAYS , TR1_KILL_DRAGON , 0 , 0           , NULL , 0 },
  { 1 , ALWAYS , NO_BIAS         , ALWAYS , TR1_IMPACT      , 0 , 0           , NULL , 0 },
  { 1 , 2      , BIAS_LAW        , ALWAYS , TR1_SLAY_EVIL   , 0 , 0           , NULL , 0 },
  { 1 , 9      , BIAS_PRIESTLY   , ALWAYS , TR1_SLAY_EVIL   , 0 , 0           , NULL , 0 },
  { 2 , 9      , BIAS_PRIESTLY   , ALWAYS , TR1_SLAY_UNDEAD , 0 , 0           , NULL , 0 },
  { 2 , 9      , BIAS_PRIESTLY   , ALWAYS , TR1_SLAY_DEMON  , 0 , 0           , NULL , 0 },
  { 2 , 9      , BIAS_WARRIOR    , ALWAYS , TR1_VORPAL      , 0 , 0           , is_sword , 0 },
  { 0 , 0      , 0               , 0      , 0               , 0 , 0           , NULL    , 0 },
};

void random_slay (object_type * o_ptr, bool is_scroll)
{
	if( !(o_ptr->tval == TV_BOW) )
		(void)apply_bias_table( art_slay_table , o_ptr , is_scroll , 0 );
	else
	{
		if(randint(2)==1)
			o_ptr->art_flags3 |= TR3_XTRA_MIGHT;
		else
			o_ptr->art_flags3 |= TR3_XTRA_SHOTS;
		if (!artefact_bias && randint(9)==1) artefact_bias = BIAS_RANGER;
	}
}

art_bias_entry art_random_table[] =
{
  { 1 , 7      , BIAS_WARRIOR  , ALWAYS  , TR1_STR     , 0  , 0 , NULL    , 0 } ,
  { 0 , ALWAYS , BIAS_WARRIOR  , ALWAYS  , TR1_CON     , 0  , 0 , NULL    , 0 } ,
  { 0 , ALWAYS , BIAS_WARRIOR  , ALWAYS  , TR1_DEX     , 0  , 0 , NULL    , 0 } ,
  { 1 , 7      , BIAS_MAGE     , ALWAYS  , TR1_INT     , 0  , 0 , NULL    , 0 } ,
  { 1 , 7      , BIAS_PRIESTLY , ALWAYS  , TR1_WIS     , 0  , 0 , NULL    , 0 } ,
  { 0 , ALWAYS , BIAS_RANGER   , ALWAYS  , TR1_DEX     , 0  , 0 , NULL    , 0 } ,
  { 1 , 7      , BIAS_RANGER   , ALWAYS  , TR1_CON     , 0  , 0 , NULL    , 0 } ,
  { 0 , ALWAYS , BIAS_RANGER   , ALWAYS  , TR1_STR     , 0  , 0 , NULL    , 0 } ,
  { 2 , 3      , BIAS_ROGUE    , ALWAYS  , TR1_STEALTH , 0  , 0 , NULL    , 0 } ,
  { 0 , ALWAYS , BIAS_ROGUE    , ALWAYS  , TR1_SEARCH  , 0  , 0 , NULL    , 0 } ,
  { 1 , 7      , BIAS_ROGUE    , ALWAYS  , TR1_DEX     , 0  , 0 , NULL    , 0 } ,
  { 1 , 13     , BIAS_STR      , ALWAYS  , TR1_STR     , 0  , 0 , NULL    , 0 } ,
  { 1 , 13     , BIAS_DEX      , ALWAYS  , TR1_DEX     , 0  , 0 , NULL    , 0 } ,
  { 1 , 13     , BIAS_CON      , ALWAYS  , TR1_CON     , 0  , 0 , NULL    , 0 } ,
  { 1 , 13     , BIAS_INT      , ALWAYS  , TR1_INT     , 0  , 0 , NULL    , 0 } ,
  { 1 , 13     , BIAS_WIS      , ALWAYS  , TR1_WIS     , 0  , 0 , NULL    , 0 } ,
  { 2 , 13     , BIAS_CHA      , ALWAYS  , TR1_CHA     , 0  , 0 , NULL    , 0 } ,
  { 2 , ALWAYS , NO_BIAS       , ALWAYS  , TR1_INFRA   , 0  , 0 , NULL    , 0 } ,
  { 2 , ALWAYS , NO_BIAS       , ALWAYS  , TR1_TUNNEL  , 0  , 0 , NULL    , 0 } ,
  { 2 , 9      , BIAS_RANGER   , ALWAYS  , TR1_SEARCH  , 0  , 0 , NULL    , 0 } ,
  { 1 , 11     , BIAS_ROGUE    , ALWAYS  , TR1_SPEED   , 0  , 0 , NULL    , 0 } ,
  { 2 , 11     , BIAS_WARRIOR  , ALWAYS  , TR1_BLOWS   , 0  , 0 , not_bow , 0 } ,
  { 0 , 0      , 0             , 0       , 0           , 0  , 0 , NULL    , 0 } ,
};

void random_plus (object_type * o_ptr, bool is_scroll)
{
	(void)apply_bias_table( art_random_table , o_ptr , is_scroll , 0 );
}

void give_activation_power (object_type * o_ptr)
{

	int type = 0, chance = 0;

	if (artefact_bias)
	{
		if (artefact_bias == BIAS_ELEC)
		{
			if (randint(3)!=1)
			{
				type = ACT_BO_ELEC_1;
			}
			else if (randint(5)!=1)
			{
				type = ACT_BA_ELEC_2;
			}
			else
			{
				type = ACT_BA_ELEC_3;
			}
			chance = 101;
		}
		else if (artefact_bias == BIAS_POIS)
		{
			type = ACT_BA_POIS_1;
			chance = 101;
		}
		else if (artefact_bias == BIAS_FIRE)
		{
			if (randint(3)!=1)
			{
				type = ACT_BO_FIRE_1;
			}
			else if (randint(5)!=1)
			{
				type = ACT_BA_FIRE_1;
			}
			else
			{
				type = ACT_BA_FIRE_2;
			}
			chance = 101;
		}
		else if (artefact_bias == BIAS_COLD)
		{
			chance = 101;
			if (randint(3)!=1)
				type = ACT_BO_COLD_1;
			else if (randint(3)!=1)
				type = ACT_BA_COLD_1;
			else if (randint(3)!=1)
				type = ACT_BA_COLD_2;
			else
				type = ACT_BA_COLD_3;
		}
		else if (artefact_bias == BIAS_CHAOS)
		{
			chance = 50;
			if (randint(6)==1)
				type = ACT_SUMMON_DEMON;
			else
				type = ACT_CALL_CHAOS;
		}
		else if (artefact_bias == BIAS_PRIESTLY)
		{
			chance = 101;

			if (randint(13)==1)
				type = ACT_CHARM_UNDEAD;
			else if (randint(12)==1)
				type = ACT_BANISH_EVIL;
			else if (randint(11)==1)
				type = ACT_DISP_EVIL;
			else if (randint(10)==1)
				type = ACT_PROT_EVIL;
			else if (randint(9)==1)
				type = ACT_CURE_1000;
			else if (randint(8)==1)
				type = ACT_CURE_700;
			else if (randint(7)==1)
				type = ACT_REST_ALL;
			else if (randint(6)==1)
				type = ACT_REST_LIFE;
			else
				type = ACT_CURE_MW;
		}
		else if (artefact_bias == BIAS_NECROMANTIC)
		{
			chance = 101;
			if (randint(66)==1)
				type = ACT_WRAITH;
			else if (randint(13)==1)
				type = ACT_DISP_GOOD;
			else if (randint(9)==1)
				type = ACT_MASS_GENO;
			else if (randint(8)==1)
				type = ACT_GENOCIDE;
			else if (randint(13)==1)
				type = ACT_SUMMON_UNDEAD;
			else if (randint(9)==1)
				type = ACT_VAMPIRE_2;
			else if (randint(6)==1)
				type = ACT_CHARM_UNDEAD;
			else
				type = ACT_VAMPIRE_1;
		}
		else if (artefact_bias == BIAS_LAW)
		{
			chance = 101;
			if (randint(8)==1)
				type = ACT_BANISH_EVIL;
			else if (randint(4)==1)
				type = ACT_DISP_EVIL;
			else
				type = ACT_PROT_EVIL;
		}
		else if (artefact_bias == BIAS_ROGUE)
		{
			chance = 101;
			if (randint(50)==1)
				type = ACT_SPEED;
			else if (randint(4)==1)
				type = ACT_SLEEP;
			else if (randint(3)==1)
				type = ACT_DETECT_ALL;
			else if (randint(8)==1)
				type = ACT_ID_FULL;
			else
				type = ACT_ID_PLAIN;
		}
		else if (artefact_bias == BIAS_MAGE)
		{
			chance = 66;
			if (randint(20)==1)
				type = FILTER_ELEMENTAL;
			else if (randint(10)==1)
				type = FILTER_PHANTOM;
			else if (randint(5)==1)
				type = ACT_RUNE_EXPLO;
			else
				type = ACT_ESP;
		}
		else if (artefact_bias == BIAS_WARRIOR)
		{
			chance = 80;
			if (randint(100)==1)
				type = ACT_INVULN;
			else
				type = ACT_BERSERK;
		}
		else if (artefact_bias == BIAS_RANGER)
		{
			chance = 101;
			if (randint(20)==1)
				type = ACT_CHARM_ANIMALS;
			else if (randint(7)==1)
				type = ACT_SUMMON_ANIMAL;
			else if (randint(6)==1)
				type = ACT_CHARM_ANIMAL;
			else if (randint(4)==1)
				type = ACT_RESIST_ALL;
			else if (randint(3)==1)
				type = ACT_SATIATE;
			else
				type = ACT_CURE_POISON;
		}
	}

	while (!(type) || (randint(100)>=chance))
	{
		type = randint(255);
		switch (type)
		{
		case ACT_SUNLIGHT: case ACT_BO_MISS_1:
		case ACT_BA_POIS_1: case ACT_BO_ELEC_1:
		case ACT_BO_ACID_1: case ACT_BO_COLD_1: case ACT_BO_FIRE_1:
		case ACT_CONFUSE: case ACT_SLEEP: case ACT_QUAKE:
		case ACT_CURE_LW: case ACT_CURE_MW: case ACT_CURE_POISON:
		case ACT_BERSERK: case ACT_LIGHT: case ACT_MAP_LIGHT:
		case ACT_DEST_DOOR: case ACT_STONE_MUD: case ACT_TELEPORT:
			chance = 101;
			break;
		case ACT_BA_COLD_1: case ACT_BA_FIRE_1: case ACT_DRAIN_1:
		case ACT_TELE_AWAY: case ACT_ESP: case ACT_RESIST_ALL:
		case ACT_DETECT_ALL: case ACT_RECALL:
		case ACT_SATIATE: case ACT_RECHARGE:
			chance = 85;
			break;
		case ACT_TERROR: case ACT_PROT_EVIL: case ACT_ID_PLAIN:
			chance = 75;
			break;
		case ACT_DRAIN_2: case ACT_VAMPIRE_1: case ACT_BO_MISS_2:
		case ACT_BA_FIRE_2: case ACT_REST_LIFE:
			chance = 66;
			break;
		case ACT_BA_COLD_3: case ACT_BA_ELEC_3: case ACT_WHIRLWIND:
		case ACT_VAMPIRE_2: case ACT_CHARM_ANIMAL:
			chance = 50;
			break;
		case ACT_SUMMON_ANIMAL:
			chance = 40;
			break;
		case ACT_DISP_EVIL: case ACT_BA_MISS_3: case ACT_DISP_GOOD:
		case ACT_BANISH_EVIL: case ACT_GENOCIDE: case ACT_MASS_GENO:
		case ACT_CHARM_UNDEAD: case ACT_CHARM_OTHER: case ACT_SUMMON_PHANTOM:
		case ACT_REST_ALL:
		case ACT_RUNE_EXPLO:
			chance = 33;
			break;
		case ACT_CALL_CHAOS: case ACT_SHARD:
		case ACT_CHARM_ANIMALS: case ACT_CHARM_OTHERS:
		case ACT_SUMMON_ELEMENTAL: case ACT_CURE_700:
		case ACT_SPEED: case ACT_ID_FULL: case ACT_RUNE_PROT:
			chance = 25;
			break;
		case ACT_CURE_1000: case ACT_XTRA_SPEED:
		case ACT_DETECT_XTRA: case ACT_DIM_DOOR:
			chance = 10;
			break;
		case ACT_SUMMON_UNDEAD: case ACT_SUMMON_DEMON:
		case ACT_WRAITH: case ACT_INVULN: case ACT_ALCHEMY:
			chance = 5;
			break;
		default:
			chance = 0;
		}
	}

	/* A type was chosen... */
	o_ptr->xtra2 = type;
	o_ptr->art_flags3 |= TR3_ACTIVATE;
	o_ptr->timeout = 0;
}


/* Build filenames starting with a_ or w_, ending with cursed/low/med/high,
   these files are found under the vnmj'file' subdir */
void get_random_name(char * return_name, bool armour, int power)
{
	char NameFile[16];
	strcpy( NameFile , (armour==1)?"a_":"w_" );
	switch(power)
	{
		case 0:	strcpy(NameFile+2, "cursed.txt"); break;
		case 1:	strcpy(NameFile+2, "low.txt"   ); break;
		case 2:	strcpy(NameFile+2, "med.txt"   ); break;
		default:strcpy(NameFile+2, "high.txt"  );
	}
	get_rnd_line(NameFile, return_name);
}

bool create_artefact(object_type *o_ptr, bool a_scroll)
{

	char new_name[80];
	int has_pval = 0;
	int powers = randint(5) + 1;
	int max_type = (o_ptr->tval<TV_BOOTS?7:5);
	int power_level;
	s32b total_flags;
	bool a_cursed = FALSE;

	int warrior_artefact_bias = 0;

	artefact_bias = 0;

	if (a_scroll && (randint(4)==1))
	{
		switch (p_ptr->pclass)
		{
		case CLASS_WARRIOR:
			artefact_bias = BIAS_WARRIOR;
			break;
		case CLASS_MAGE: case CLASS_HIGH_MAGE: case CLASS_WARLOCK: case CLASS_BLOOD_MAGE:
			artefact_bias = BIAS_MAGE;
			break;
		case CLASS_PRIEST: case CLASS_DRUID:
			artefact_bias = BIAS_PRIESTLY;
			break;
		case CLASS_ROGUE:
			artefact_bias = BIAS_ROGUE;
			warrior_artefact_bias = 25;
			break;
		case CLASS_RANGER:
			artefact_bias = BIAS_RANGER;
			warrior_artefact_bias = 30;
			break;
		case CLASS_PALADIN: case CLASS_BLACK_KNIGHT:
			artefact_bias = BIAS_PRIESTLY;
			warrior_artefact_bias = 40;
			break;
		case CLASS_WARRIOR_MAGE:
			artefact_bias = BIAS_MAGE;
			warrior_artefact_bias = 40;
			break;
		case CLASS_HELL_KNIGHT: case CLASS_CHAOS_KNIGHT:
			artefact_bias = BIAS_CHAOS;
			warrior_artefact_bias = 40;
			break;
		case CLASS_MYSTIC:
			artefact_bias = BIAS_PRIESTLY;
			break;
		case CLASS_ORPHIC:
			if (randint(5)>2) artefact_bias = BIAS_PRIESTLY;
			break;
		}
	}

	if ((randint(100) <= warrior_artefact_bias) && a_scroll) artefact_bias = BIAS_WARRIOR;

	strcpy(new_name,"");

	if ((!a_scroll) && (randint(A_CURSED)==1)) a_cursed = TRUE;

	while ((randint(powers) == 1) || (randint(7)==1) || randint(10)==1) powers++;

	if ((!a_cursed) && (randint(WEIRD_LUCK)==1)) powers *= 2;

	if (a_cursed) powers /= 2;

	/* Main loop */

	while(powers--)
	{
		switch (randint(max_type))
		{
		case 1: case 2:
			random_plus(o_ptr, a_scroll);
			has_pval = TRUE;
			break;
		case 3: case 4:
			random_resistance(o_ptr, a_scroll, FALSE);
			break;
		case 5:
			random_misc(o_ptr, a_scroll);
			break;
		case 6: case 7:
			random_slay(o_ptr, a_scroll);
			break;
		default:
			if(debug_mode) msg_print ("Switch error in create_artefact!");
			powers++;
		}
	};

	if (has_pval)
	{
		if (o_ptr->art_flags1 & TR1_BLOWS)
			o_ptr->pval = rand_s16b(2) + 1;
		else
		{
			do
			{ o_ptr->pval++; }
			while (o_ptr->pval<randint(5) || randint(o_ptr->pval)==1);
		}
		if (o_ptr->pval > 4 && (randint(WEIRD_LUCK)!=1))
			o_ptr->pval = 4;
	}

	/* give it some plusses... */
	if (o_ptr->tval>=TV_BOOTS)
		o_ptr->to_a += rand_s16b(o_ptr->to_a>19?1:20-o_ptr->to_a);
	else
	{
		o_ptr->to_h += rand_s16b(o_ptr->to_h>19?1:20-o_ptr->to_h);
		o_ptr->to_d += rand_s16b(o_ptr->to_d>19?1:20-o_ptr->to_d);
	}
	o_ptr->art_flags3 |= ( TR3_IGNORE_ACID | TR3_IGNORE_ELEC |
		TR3_IGNORE_FIRE | TR3_IGNORE_COLD); /* Just to be sure */
	total_flags = flag_cost(o_ptr, o_ptr->pval);
	if (debug_peek) msg_format("%ld", total_flags);

	if (a_cursed) curse_artefact(o_ptr);

	if ( /* No activations for cursed weapons */
		 (!a_cursed) &&
		 /* Are we lucky enough for an activation */
		 (randint((o_ptr->tval>=TV_BOOTS)?ACTIVATION_CHANCE * 2:ACTIVATION_CHANCE)==1) &&
		 /* We cant use an activation with sentient weapons.. */
		 (!( o_ptr->art_flags3 & TR3_XP ))
	   )
	{
		o_ptr->xtra2 = 0;
		give_activation_power(o_ptr);
	}


	if(o_ptr->tval>=TV_BOOTS)
	{
		if (a_cursed) power_level = 0;
		else if (total_flags<10000) power_level = 1;
		else if (total_flags<20000) power_level = 2;
		else power_level = 3;
	}

	else
	{
		if (a_cursed) power_level = 0;
		else if (total_flags<15000) power_level = 1;
		else if (total_flags<30000) power_level = 2;
		else power_level = 3;
	}

	if (a_scroll)
	{
		char dummy_name[80];
		strcpy(dummy_name, "");
		identify_fully_aux(o_ptr);
		o_ptr->ident |= IDENT_STOREB; /* This will be used later on... */
		if (!(get_string("What do you want to call the artefact? ", dummy_name, 80)))
			strcpy(new_name,"(a DIY Artifact)");
		else
		{
			strcpy(new_name,"called '");
			strcat(new_name,dummy_name);
			strcat(new_name,"'");
		}
		/* Identify it fully */
		object_full_id( o_ptr );

	}

	else

		get_random_name(new_name, (bool)(o_ptr->tval >= TV_BOOTS), power_level);

	if (debug_xtra)
	{
		if (artefact_bias)
			msg_format("Biased artefact: %d.", artefact_bias);
		else
			msg_print("No bias in artefact.");
	}

	/* Save the inscription */
	o_ptr->art_name = quark_add(new_name);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);
	return TRUE;

}


bool artefact_scroll()
{
	int  item;
	bool okay = FALSE;
	char o_name[80];
	object_type       *o_ptr;

	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Get an item (from equip or inven or floor) */
	if (!get_item(&item, "Enchant which item? ", "You have nothing to enchant.", USE_FLOOR | USE_EQUIP | USE_INVEN))
	{
		return (FALSE);
	}

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

	/* Describe */
	msg_format("%s %s radiate%s a blinding light!",
		((item >= 0) ? "Your" : "The"), o_name,
		((o_ptr->number > 1) ? "" : "s"));

	if (o_ptr->name1 || o_ptr->art_name)
	{
		msg_format("The %s %s already %s!",
			o_name, ((o_ptr->number > 1) ? "are" : "is"),
			((o_ptr->number > 1) ? "artefacts" : "an artefact"));
		okay = FALSE;
	}
	else
	{
		if (o_ptr->number > 1)
		{
			msg_print("Not enough enough energy to enchant more than one object!");
			msg_format("%d of your %s %s destroyed!",(o_ptr->number)-1, o_name, (o_ptr->number>2?"were":"was"));
			o_ptr->number = 1;
		}
		/* Muhahah, artefact creation should be the awesomest ;] */
		if( o_ptr->name2 )
		{
			/* Obtain the ego-item info */
			ego_item_type *e_ptr;
			e_ptr = &e_info[o_ptr->name2];
			o_ptr->art_flags1 |= e_ptr->flags1;
			o_ptr->art_flags2 |= e_ptr->flags2;
			o_ptr->art_flags3 |= e_ptr->flags3;

			o_ptr->name2 = 0;
			/* Keep the realms for sentient weapons */
			if(!( e_ptr->flags3 & TR3_XP ))
			{
				o_ptr->xtra2 = 0;
			}
		}
		okay = create_artefact(o_ptr, TRUE);
	}

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message */
		msg_print("The enchantment failed.");
	}

	/* Show what happened */
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Something happened */
	return (TRUE);
}

static bool item_tester_hook_id(object_type *o_ptr)
{
	if( o_ptr->ident & (IDENT_MENTAL | IDENT_STOREB | IDENT_KNOWN ) )
		return FALSE;

	return TRUE;
}

static bool item_tester_hook_full_id(object_type *o_ptr)
{
	/* If we *id*'d it, dont bother */
	if( o_ptr->ident & IDENT_MENTAL )
		return FALSE;

	/* If we found the alchemic combination by ourselves, dont bother either */
	if( o_ptr->tval == TV_POTION && potion_alch[o_ptr->sval].known1 && potion_alch[o_ptr->sval].known2 )
		return FALSE;

	return TRUE;
}


/*
* Identify an object in the inventory (or on the floor)
* This routine does *not* automatically combine objects.
* Returns TRUE if something was identified, else FALSE.
*/
bool ident_spell(void)
{
	int           item;
	object_type  *o_ptr;
	char          o_name[80];

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_id;

	/* Get an item (from equip or inven or floor) */
	if (!get_item(&item, "Identify which item? ", "You have nothing to identify.", USE_FLOOR | USE_EQUIP | USE_INVEN))
	{
		return (FALSE);
	}

	/* Clear the hook */
	item_tester_hook = 0;

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
	object_known(o_ptr,TRUE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

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
* Fully "identify" an object in the inventory  -BEN-
* This routine returns TRUE if an item was identified.
*/
bool identify_fully(void)
{
	int                     item;

	object_type             *o_ptr;

	char            o_name[80];

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_full_id;

	/* Get an item (from equip or inven or floor) */
	if (!get_item(&item, "Identify which item? ", "You have nothing to identify.", USE_FLOOR | USE_EQUIP | USE_INVEN))
	{
		return (FALSE);
	}

	/* Clear the hook */
	item_tester_hook = 0;

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
	object_full_id( o_ptr );

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

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
bool item_tester_hook_recharge(object_type *o_ptr)
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
	int                 i, t, item, lev;

	object_type             *o_ptr;


	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Recharge which item? ", "You have nothing to recharge.", USE_FLOOR | USE_EQUIP | USE_INVEN))
	{
		return (FALSE);
	}

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

		/* Paranoia -- prevent crashes */
		if (i < 1) i = 1;

		/* Back-fire */
		if (rand_int(i) == 0)
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
	}

	/* Recharge wand/staff */
	else
	{
		/* Recharge power */
		i = (num + 100 - lev - (10 * o_ptr->pval)) / 15;

		/* Paranoia -- prevent crashes */
		if (i < 1) i = 1;

		/* Back-fire XXX XXX XXX */
		if (rand_int(i) == 0)
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
			if (t > 0) o_ptr->pval += 2 + rand_s16b(t);

			/* Hack -- we no longer "know" the item */
			o_ptr->ident &= ~(IDENT_KNOWN);

			/* Hack -- we no longer think the item is empty */
			o_ptr->ident &= ~(IDENT_EMPTY);
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
	int             i, x, y;

	int             flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;

	bool    obvious = FALSE;


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
		if (project(0, 0, y, x, dam, typ, flg)) obvious = TRUE;
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
* Dispel good monsters
*/
bool dispel_good(int dam)
{
	return (project_hack(GF_DISP_GOOD, dam));
}

/*
* Dispel all monsters
*/
bool dispel_monsters(int dam)
{
	return (project_hack(GF_DISP_ALL, dam));
}

/*
* Dispel 'living' monsters
*/
bool dispel_living(int dam)
{
	return (project_hack(GF_DISP_LIVING, dam));
}

/*
* Dispel demons
*/
bool dispel_demons(int dam)
{
	return (project_hack(GF_DISP_DEMON, dam));
}

/*
 * Dispel devils
 */
bool dispel_devils(int dam)
{
	return (project_hack(GF_DISP_DEVIL, dam));
}

/*
 * Dispel Fallen Angels
 */
bool dispel_fallen_angels(int dam)
{
	return (project_hack(GF_DISP_FALLEN_ANGEL, dam));
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
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];

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
			if ( is_potential_hater( m_ptr ) )
			{
				if (randint(2)==1)
				{
					set_hate_player( m_ptr );
				}
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
bool genocide(bool player_cast)
{
	int             i;

	char    typ;

	bool    result = FALSE;

	int             msec = delay_factor * delay_factor * delay_factor;

	/* Mega-Hack -- Get a monster symbol */
	(void)(get_com("Choose a monster race (by symbol) to genocide: ", &typ));

	/* Delete the monsters of that "type" */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip Unique Monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Skip "wrong" monsters */
		if (r_ptr->d_char != typ) continue;

		/* Skip Quest Monsters - Dean Anderson */
		if ((r_ptr->flags1 & RF1_GUARDIAN) || (r_ptr->flags1 & RF1_ALWAYS_GUARD)) continue;

		/* Delete the monster */
		delete_monster_idx(i,TRUE);

		if (player_cast)
		{
			/* Take damage */
			take_hit(randint(4), "the strain of casting Genocide");
		}

		/* Visual feedback */
		move_cursor_relative(py, px);

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

		/* Handle */
		handle_stuff();

		/* Fresh */
		Term_fresh();

		/* Delay */
		Term_xtra(TERM_XTRA_DELAY, msec);

		/* Take note */
		result = TRUE;
	}

	return (result);
}


/*
* Delete all nearby (non-unique) monsters
*/
bool mass_genocide(bool player_cast)
{
	int             i;

	bool    result = FALSE;

	int             msec = delay_factor * delay_factor * delay_factor;


	/* Delete the (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Skip Quest Monsters - Dean Anderson */
		if ((r_ptr->flags1 & RF1_GUARDIAN) || (r_ptr->flags1 & RF1_ALWAYS_GUARD)) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > MAX_SIGHT) continue;

		if (player_cast)
		{
			/* Delete the monster */
			delete_monster_idx(i,TRUE);
			/* Hack -- visual feedback */
			take_hit(randint(3), "the strain of casting Mass Genocide");
		}
		else
		{
			/* Keep the treasure, thank you patron ;) */
			int flg = PROJECT_STOP | PROJECT_JUMP | PROJECT_KILL;
			project(0, 0, m_ptr->fy, m_ptr->fx, 1000000, GF_MISSILE, flg);
		}

		move_cursor_relative(py, px);

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

		/* Handle */
		handle_stuff();

		/* Fresh */
		Term_fresh();

		/* Delay */
		Term_xtra(TERM_XTRA_DELAY, msec);

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
	int            i;

	bool    probe = FALSE;


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

	cave_type *c_ptr;

	bool flag = FALSE;


	/* XXX XXX */
	full = full ? full : 0;

	/* Big area of affect */
	for (y = (y1 - r); y <= (y1 + r); y++)
	{
		for (x = (x1 - r); x <= (x1 + r); x++)
		{
			/* Skip illegal grids */
			if (!in_bounds(y, x)) continue;

			/* Extract the distance */
			k = distance(y1, x1, y, x);

			/* Stay in the circle of death */
			if (k > r) continue;

			/* Access the grid */
			c_ptr = &cave[y][x];

			/* Lose room and vault */
			c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Lose light and knowledge */
			c_ptr->info &= ~(CAVE_MARK | CAVE_GLOW);

			/* Hack -- Notice player affect */
			if ((x == px) && (y == py))
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
				/* Delete objects */
				delete_object(y, x);

				/* Wall (or floor) type */
				t = rand_int(200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					c_ptr->feat = FEAT_WALL_EXTRA;
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					c_ptr->feat = FEAT_QUARTZ;
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					c_ptr->feat = FEAT_MAGMA;
				}

				/* Floor */
				else
				{
					/* Create floor */
					c_ptr->feat = FEAT_FLOOR;
				}
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
			(void)set_timed_effect( TIMED_BLIND , p_ptr->blind + 10 + randint(10));
		}
	}


	/* Mega-Hack -- Forget the view and lite */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

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
* Note that thus the player and monsters (except eaters of walls and
* passers through walls) will never occupy the same grid as a wall.
* Note that as of now (2.7.8) no monster may occupy a "wall" grid, even
* for a single turn, unless that monster can pass_walls or kill_walls.
* This has allowed massive simplification of the "monster" code.
*/
void earthquake(int cy, int cx, int r)
{
	int             i, t, y, x, yy, xx, dy, dx, oy, ox;

	int             damage = 0;

	int             sn = 0, sy = 0, sx = 0;

	bool    hurt = FALSE;

	cave_type       *c_ptr;

	bool    map[32][32];


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
			if (!in_bounds(yy, xx)) continue;

			/* Skip distant grids */
			if (distance(cy, cx, yy, xx) > r) continue;

			/* Access the grid */
			c_ptr = &cave[yy][xx];

			/* Lose room and vault */
			c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Lose light and knowledge */
			c_ptr->info &= ~(CAVE_GLOW | CAVE_MARK);

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
			/* Access the location */
			y = py + ddy[i];
			x = px + ddx[i];

			/* Skip non-empty grids */
			if (!cave_empty_bold(y, x)) continue;

			/* Important -- Skip "quake" grids */
			if (map[16+y-cy][16+x-cx]) continue;

			/* Count "safe" grids */
			sn++;

			/* Randomize choice */
			if (rand_int(sn) > 0) continue;

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
				msg_print("The cave quakes!  You are pummeled with debris!");
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
					(void)set_timed_effect( TIMED_STUN, p_ptr->stun + randint(50));
					break;
				}
			case 3:
				{
					msg_print("You are crushed between the floor and ceiling!");
					damage = damroll(10, 4);
					(void)set_timed_effect( TIMED_STUN, p_ptr->stun + randint(50));
					break;
				}
			}

			/* Save the old location */
			oy = py;
			ox = px;

			/* Move the player to the safe location */
			py = sy;
			px = sx;

			/* Redraw the old spot */
			lite_spot(oy, ox);

			/* Redraw the new spot */
			lite_spot(py, px);

			/* Check for new panel */
			verify_panel();
		}

		/* Important -- no wall on player */
		map[16+py-cy][16+px-cx] = FALSE;

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

			/* Access the grid */
			c_ptr = &cave[yy][xx];

			/* Process monsters */
			if (c_ptr->m_idx)
			{
				monster_type *m_ptr = &m_list[c_ptr->m_idx];
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
							/* Access the grid */
							y = yy + ddy[i];
							x = xx + ddx[i];

							/* Skip non-empty grids */
							/*if (!cave_empty_bold(y, x) || (cave[y][x].feat == FEAT_WATER)) continue;*/
							if (!can_place_monster(y,x, m_ptr->r_idx)) continue;

							/* Hack -- no safety on glyph of warding */
							if (cave[y][x].feat == FEAT_GLYPH) continue;
							if (cave[y][x].feat == FEAT_MINOR_GLYPH) continue;

							/* Important -- Skip "quake" grids */
							if (map[16+y-cy][16+x-cx]) continue;

							/* Count "safe" grids */
							sn++;

							/* Randomize choice */
							if (rand_int(sn) > 0) continue;

							/* Save the safe grid */
							sy = y; sx = x;
						}
					}

					/* Describe the monster */
					monster_desc(m_name, m_ptr, 0);

					/* Scream in pain */
					msg_format("%^s wails out in pain!", m_name);

					/* Take damage from the quake */
					damage = (sn ? damroll(4, 8) : 200);

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
						int m_idx = cave[yy][xx].m_idx;

						/* Update the new location */
						cave[sy][sx].m_idx = m_idx;

						/* Update the old location */
						cave[yy][xx].m_idx = 0;

						/* Move the monster */
						m_ptr->fy = sy;
						m_ptr->fx = sx;

						/* Update the monster (new location) */
						update_mon(m_idx, TRUE);

						/* Redraw the old grid */
						lite_spot(yy, xx);

						/* Redraw the new grid */
						lite_spot(sy, sx);
					}
				}
			}
		}
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

			/* Access the cave grid */
			c_ptr = &cave[yy][xx];

			/* Paranoia -- never affect player */
			if ((yy == py) && (xx == px)) continue;

			/* Destroy location (if valid) */
			if (cave_valid_bold(yy, xx))
			{
				bool floor = cave_floor_bold(yy, xx);

				/* Delete objects */
				delete_object(yy, xx);

				/* Wall (or floor) type */
				t = (floor ? rand_int(100) : 200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					c_ptr->feat = FEAT_WALL_EXTRA;
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					c_ptr->feat = FEAT_QUARTZ;
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					c_ptr->feat = FEAT_MAGMA;
				}

				/* Floor */
				else
				{
					/* Create floor */
					c_ptr->feat = FEAT_FLOOR;
				}
			}
		}
	}


	/* Mega-Hack -- Forget the view and lite */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

	/* Update the monsters */
	p_ptr->update |= (PU_DISTANCE);

	/* Update the health bar */
	p_ptr->redraw |= (PR_HEALTH);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

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

		cave_type *c_ptr = &cave[y][x];

		/* No longer in the array */
		c_ptr->info &= ~(CAVE_TEMP);

		/* Update only non-CAVE_GLOW grids */
		/* if (c_ptr->info & (CAVE_GLOW)) continue; */

		/* Perma-Lite */
		c_ptr->info |= (CAVE_GLOW);

		/* Process affected monsters */
		if (c_ptr->m_idx)
		{
			int chance = 25;

			monster_type    *m_ptr = &m_list[c_ptr->m_idx];

			monster_race    *r_ptr = &r_info[m_ptr->r_idx];

			/* Update the monster */
			update_mon(c_ptr->m_idx, FALSE);

			/* Stupid monsters rarely wake up */
			if (r_ptr->flags2 & (RF2_STUPID)) chance = 10;

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

					/* Acquire the monster name */
					monster_desc(m_name, m_ptr, 0);

					/* Dump a message */
					msg_format("%^s wakes up.", m_name);
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

		cave_type *c_ptr = &cave[y][x];

		/* No longer in the array */
		c_ptr->info &= ~(CAVE_TEMP);

		/* Darken the grid */
		c_ptr->info &= ~(CAVE_GLOW);

		/* Hack -- Forget "boring" grids */
		if (c_ptr->feat <= FEAT_INVIS)
		{
			/* Forget the grid */
			c_ptr->info &= ~(CAVE_MARK);

			/* Notice */
			note_spot(y, x);
		}

		/* Process affected monsters */
		if (c_ptr->m_idx)
		{
			/* Update the monster */
			update_mon(c_ptr->m_idx, FALSE);
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
	cave_type *c_ptr = &cave[y][x];

	/* Avoid infinite recursion */
	if (c_ptr->info & (CAVE_TEMP)) return;

	/* Do not "leave" the current room */
	if (!(c_ptr->info & (CAVE_ROOM))) return;

	/* Paranoia -- verify space */
	if (temp_n == TEMP_MAX) return;

	/* Mark the grid as "seen" */
	c_ptr->info |= (CAVE_TEMP);

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
	int flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("You are surrounded by a white light.");
	}

	/* Hook into the "project()" function */
	(void)project(0, rad, py, px, dam, GF_LITE_WEAK, flg);

	/* Lite up the room */
	lite_room(py, px);

	/* Assume seen */
	return (TRUE);
}

/*
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
bool lite_area_hecate(int dam, int rad)
{
	int flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("You are surrounded by Hecate's glow.");
	}
	/* Hook into the "project()" function */
	(void)project(0, rad, py, px, dam, GF_HECATE , flg);

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
	int flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("Darkness surrounds you.");
	}

	/* Hook into the "project()" function */
	(void)project(0, rad, py, px, dam, GF_DARK_WEAK, flg);

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
	int tx, ty;

	int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	/* Use the given direction */
	tx = px + 99 * ddx[dir];
	ty = py + 99 * ddy[dir];

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		flg &= ~(PROJECT_STOP);
		tx = target_col;
		ty = target_row;
	}

	/* Analyze the "dir" and the "target".  Hurt items on floor. */
	return (project(0, rad, ty, tx, dam, typ, flg));
}


/*
* Hack -- apply a "projection()" in a direction (or at the target)
*/
static bool project_hook(int typ, int dir, int dam, int flg)
{
	int tx, ty;

	/* Pass through the target if needed */
	flg |= (PROJECT_THRU);

	/* Use the given direction */
	tx = px + ddx[dir];
	ty = py + ddy[dir];

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}

	/* Analyze the "dir" and the "target", do NOT explode */
	return (project(0, 0, ty, tx, dam, typ, flg));
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
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
	return (project_hook(GF_LITE_WEAK, dir, damroll(6, 8), flg));
}

bool drain_life(int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_DRAIN, dir, dam, flg));
}

bool wall_to_mud(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(GF_KILL_WALL, dir, 20 + randint(30), flg));
}

bool wizard_lock(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(GF_JAM_DOOR, dir, 20 + randint(30), flg));
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

bool stasis_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_STASIS, dir, p_ptr->lev, flg));
}

bool confuse_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_CONF, dir, plev, flg));
}

bool stun_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_STUN, dir, plev, flg));
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

bool death_ray(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_DEATH_RAY, dir, plev, flg));
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
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, py, px, 0, GF_MAKE_DOOR, flg));
}

bool trap_creation(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, py, px, 0, GF_MAKE_TRAP, flg));
}

bool glyph_creation(void)
{   int flg = PROJECT_GRID | PROJECT_ITEM;
return (project(0, 1, py, px, 0, GF_MAKE_GLYPH, flg));
}

bool wall_stone(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM;

	bool dummy = (project(0, 1, py, px, 0, GF_STONE_WALL, flg));

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	return dummy;
}


bool destroy_doors_touch(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, py, px, 0, GF_KILL_DOOR, flg));
}

bool sleep_monsters_touch(void)
{
	int flg = PROJECT_KILL | PROJECT_HIDE;
	return (project(0, 1, py, px, p_ptr->lev, GF_OLD_SLEEP, flg));
}

void call_chaos(void)
{
	int Chaos_type, dummy, dir;
	int plev = p_ptr->lev;
	bool line_chaos = FALSE;

	int hurt_types[30] =
	{ GF_ELEC,      GF_POIS,    GF_ACID,    GF_COLD,
	GF_FIRE,      GF_MISSILE, GF_ARROW,   GF_PLASMA,
	GF_HOLY_FIRE, GF_WATER,   GF_LITE,    GF_DARK,
	GF_FORCE,     GF_INERTIA, GF_MANA,    GF_METEOR,
	GF_ICE,       GF_CHAOS,   GF_NETHER,  GF_DISENCHANT,
	GF_SHARDS,    GF_SOUND,   GF_NEXUS,   GF_CONFUSION,
	GF_TIME,      GF_GRAVITY, GF_SHARD,  GF_HELLSLIME,
	GF_HELL_FIRE, GF_DISINTEGRATE };

	Chaos_type = hurt_types[((randint (30))-1)];
	if (randint(4)==1) line_chaos = TRUE;

	if (randint(6)==1)
	{
		for (dummy = 1; dummy<10; dummy++)
		{
			if (dummy-5)
			{
				if (line_chaos)
					fire_beam(Chaos_type, dummy, 75);
				else
					fire_ball(Chaos_type, dummy, 75, 2);
			}
		}
	}
	else if (randint(3)==1)
	{
		fire_ball(Chaos_type, 0, 300, 8);
	}
	else
	{
		dir = randint( 9 );
		if (line_chaos)
			fire_beam(Chaos_type, dir, 150);
		else
			fire_ball(Chaos_type, dir, 150, 3 + (plev/35));
	}
}

void activate_ty_curse()
{
	int i = 0;
	do
	{
		switch(randint(27))
		{
		case 1: case 2: case 3: case 16: case 17:
			aggravate_monsters(1);
			if (randint(6)!=1) break;
		case 4: case 5: case 6:
			activate_hi_summon();
			if (randint(6)!=1) break;
		case 7: case 8: case 9: case 18:
			(void) summon_specific(py, px, dun_level, 0);
			if (randint(6)!=1) break;
		case 10: case 11: case 12:
			msg_print("You feel your life draining away...");
			lose_exp(p_ptr->exp / 16);
			if (randint(6)!=1) break;
		case 13: case 14: case 15: case 19: case 20:
			if (p_ptr->free_act && (randint(100) < p_ptr->skill_sav))
			{
				/* Do nothing */ ;
			}
			else
			{
				msg_print("You feel like a statue!");
				if (p_ptr->free_act)
					set_timed_effect( TIMED_PARALYZED , p_ptr->paralyzed + randint(3));
				else
					set_timed_effect( TIMED_PARALYZED , p_ptr->paralyzed + randint(13));
			}
			if (randint(6)!=1) break;
		case 21: case 22: case 23:
			(void)do_dec_stat((randint(6))-1);
			if (randint(6)!=1) break;
		case 24:
			msg_print("Huh? Who am I? What am I doing here?");
			lose_all_info();
			break;
		case 25:
			summon_reaver();
			break;
		default:
			while (i<6)
			{
				do { (void)do_dec_stat(i); } while (randint(2)==1);
				i++;
			}
		}
	}   while (randint(3)==1);

}

/* Modified the code so that it looks more table like,
   removed filter that are out of theme */
void activate_hi_summon(void)
{
	int i;
	for (i = 0; i < (randint(9) + (dun_level / 40)); i++)
	{
		switch(randint(26) + (dun_level / 20) )
		{
			case 3:  case 4:  (void) summon_specific(py, px, dun_level, FILTER_SPIDER);        break;
			case 5:  case 6:  (void) summon_specific(py, px, dun_level, FILTER_HOUND);         break;
			case 9:  case 10: (void) summon_specific(py, px, dun_level, FILTER_DEVIL);         break;
			case 11: case 12: (void) summon_specific(py, px, dun_level, FILTER_UNDEAD);        break;
			case 13: case 14: (void) summon_specific(py, px, dun_level, FILTER_DRAGON);        break;
			case 15: case 16: (void) summon_specific(py, px, dun_level, FILTER_DEMON);         break;
			case 17:          (void) summon_specific(py, px, dun_level, FILTER_FALLEN_ANGELS); break;
			case 18: case 19: (void) summon_specific(py, px, dun_level, FILTER_UNIQUE);        break;
			case 20: case 21: (void) summon_specific(py, px, dun_level, FILTER_HI_UNDEAD);     break;
			case 22: case 23: (void) summon_specific(py, px, dun_level, FILTER_HI_DRAGON);     break;
			case 24: case 25: (void) summon_specific(py, px, 100, FILTER_REAVER);              break;
			default:          (void) summon_specific(py, px, ( ( ( dun_level * 3) / 2 ) + 5 ), 0);
		}
	}
}

void summon_reaver(void)
{
	int i = 0;
	int max_reaver = (dun_level / 50) + randint(6);

	for (i = 0; i < max_reaver; i++)
	{
		(void)summon_specific(py, px, 100, FILTER_REAVER);
	}
}


void wall_breaker(void)
{

	int dummy = 5;

	if (randint(80+(p_ptr->lev))<70)
	{
		do { dummy = randint(9); }
		while ((dummy == 5) || (dummy == 0));
		wall_to_mud (dummy);
	}
	else if (randint(100)>30)
	{
		earthquake(py,px,1);
	}
	else
	{           for (dummy = 1; dummy<10; dummy++)

	{
		if (dummy-5)
		{
			wall_to_mud(dummy);

		}
	}
	}


}


void bless_weapon(void)
{
	int                     item;
	object_type             *o_ptr;
	u32b f1, f2, f3;

	char            o_name[80];


	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;


	/* Get an item (from equip or inven or floor) */
	if (!get_item(&item, "Bless which weapon? ", "You have no weapon to bless." , USE_FLOOR | USE_EQUIP | USE_INVEN))
	{
		return;
	}

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

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	if (o_ptr->ident & (IDENT_CURSED))
	{

		if ( ( (f3 &  (TR3_HEAVY_CURSE)) && ( randint (100) < 33 ) )
			|| (f3 & (TR3_PERMA_CURSE)) )
		{

			msg_format("The black aura on %s %s disrupts the blessing!",
				((item >= 0) ? "your" : "the"), o_name);
			return;
		}

		msg_format("A malignant aura leaves %s %s.",
			((item>=0)? "your" : "the"), o_name);

		/* Uncurse it */
		o_ptr->ident &= ~(IDENT_CURSED);

		/* Hack -- Assume felt */
		o_ptr->ident |= (IDENT_SENSE);

		/* Take note */
		o_ptr->note = quark_add("uncursed");

		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
	}

	/* Next, we try to bless it. Artifacts have a 1/3 chance of being blessed,
	otherwise, the operation simply disenchants them, godly power negating the
	magic. Ok, the explanation is silly, but otherwise priests would always
	bless every artefact weapon they find.
	Ego weapons and normal weapons can be blessed automatically. */

	if (f3 & TR3_BLESSED)
	{
		msg_format("%s %s %s blessed already.",
			((item >= 0) ? "Your" : "The"), o_name,
			((o_ptr->number > 1) ? "were" : "was"));
		return;
	}

	if (!(o_ptr->art_name || o_ptr->name1) || (randint(3)==1))
	{
		/* Describe */
		msg_format("%s %s shine%s!",
			((item >= 0) ? "Your" : "The"), o_name,
			((o_ptr->number > 1) ? "" : "s"));
		o_ptr->art_flags3 |= TR3_BLESSED;
	}
	else
	{

		bool dis_happened = FALSE;

		msg_print("The artefact resists your blessing!");

		/* Disenchant tohit */
		if (o_ptr->to_h > 0)
		{
			o_ptr->to_h--;
			dis_happened = TRUE;
		}

		if ((o_ptr->to_h > 5) && (rand_int(100) < 33)) o_ptr->to_h--;

		/* Disenchant todam */
		if (o_ptr->to_d > 0)
		{
			o_ptr->to_d--;
			dis_happened = TRUE;
		}
		if ((o_ptr->to_d > 5) && (rand_int(100) < 33)) o_ptr->to_d--;

		/* Disenchant toac */
		if (o_ptr->to_a > 0)
		{
			o_ptr->to_a--;
			dis_happened = TRUE;
		}
		if ((o_ptr->to_a > 5) && (rand_int(100) < 33)) o_ptr->to_a--;

		if (dis_happened)
		{
			msg_print("There is a static feeling in the air...");
			msg_format("%s %s %s disenchanted!",
				((item >= 0) ? "Your" : "The"), o_name,
				((o_ptr->number > 1) ? "were" : "was"));
		}

	}
	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_PLAYER);
}

/*
* Detect all "nonliving", "undead" or "demonic" monsters on current panel
*/
bool detect_monsters_nonliving(void)
{
	int             i, y, x;

	bool    flag = FALSE;


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

		/* Detect evil monsters */
		if ((r_ptr->flags3 & (RF3_NONLIVING)) ||
			(r_ptr->flags3 & (RF3_UNDEAD)) ||
			(r_ptr->flags3 & (RF3_DEVIL)) ||
			(r_ptr->flags3 & (RF3_DEMON)))
		{
			/* Update monster recall window */
			if (monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Hack -- See monster */
			m_ptr->ml = TRUE;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
		msg_print("You sense the presence of unnatural beings!");
	}

	/* Result */
	return (flag);
}

/*
 * Detect all "nonliving", "undead" or "demonic" monsters on current panel
 */
bool charm_all_goats(void)
{
	int             i;
	bool    flag = FALSE;

	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

        if(r_ptr->d_char == 'q')
        {
			/* Update monster recall window */
			if (monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Hack -- See monster */
			m_ptr->ml = TRUE;

			/* Redraw */
			lite_spot(m_ptr->fy, m_ptr->fx);

			/* Detect */
			flag = TRUE;

            /* Super Charm */
            set_ally( m_ptr, ALLY_COMPANION);
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
		msg_print("Your rule has been established!");
	}

	/* Result */
	return (flag);
}

/*
* Confuse monsters
*/
bool confuse_monsters(int dam)
{
	return (project_hack(GF_OLD_CONF, dam));
}


/*
* Charm monsters
*/
bool charm_monsters(int dam)
{
	return (project_hack(GF_CHARM, dam));
}


/*
* Charm animals
*/
bool charm_animals(int dam)
{
	return (project_hack(GF_CONTROL_ANIMAL, dam));
}


/*
* Stun monsters
*/
bool stun_monsters(int dam)
{
	return (project_hack(GF_STUN, dam));
}


/*
* Stasis monsters
*/
bool stasis_monsters(int dam)
{
	return (project_hack(GF_STASIS, dam));
}

/*
* Mindblast monsters
*/
bool mindblast_monsters(int dam)
{
	return (project_hack(GF_PSI, dam));
}


/*
* Banish all monsters
*/
bool banish_monsters(int dist)
{
	return (project_hack(GF_AWAY_ALL, dist));
}

/*
* Turn evil
*/
bool turn_evil(int dam)
{
	return (project_hack(GF_TURN_EVIL, dam));
}

/*
* Turn everyone
*/
bool turn_monsters(int dam)
{
	return (project_hack(GF_TURN_ALL, dam));
}


/*

* Death-ray all monsters (note: OBSCENELY powerful)
*/
bool deathray_monsters(void)
{
	return (project_hack(GF_DEATH_RAY, p_ptr->lev));
}

/* Charm only a specific type of monster ) */
bool charm_monster_type( int dir , int plev , int type )
{
	int flg = PROJECT_STOP | PROJECT_KILL;

	/*Set up the filter*/
	monster_filter_type = type;
	monster_filter_hook = monster_filter_okay;

	/*Do the the charming*/
	return (project_hook(GF_CHARM, dir, plev, flg));

	/*Remove the filter*/
	monster_filter_type = 0;
	monster_filter_hook = NULL;
}



bool charm_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CHARM, dir, plev, flg));
}

bool control_one_undead(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CONTROL_UNDEAD, dir, plev, flg));
}

bool charm_animal(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CONTROL_ANIMAL, dir, plev, flg));
}


static int report_magics_aux(int dur)
{
	if (dur <= 5)
	{
		return 0;
	}
	else if (dur <= 10)
	{
		return 1;
	}
	else if (dur <= 20)
	{
		return 2;
	}
	else if (dur <= 50)
	{
		return 3;
	}
	else if (dur <= 100)
	{
		return 4;
	}
	else if (dur <= 200)
	{
		return 5;
	}
	else
	{
		return 6;
	}
}

static cptr report_magic_durations[] =
{
	"for a short time",
		"for a little while",
		"for a while",
		"for a long while",
		"for a long time",
		"for a very long time",
		"for an incredibly long time",
		"until you hit a monster"
};


void report_magics(void)
{
	int             i = 0, j, k;
	int             timed_counter;

	char Dummy[80];

	cptr    info[128];
	int     info2[128];

	/* Handle timed effects */

	for( timed_counter = 0 ; timed_counter < TIMED_COUNT ; timed_counter++)
	{
		if(  *(timed[timed_counter].timer) > 0   )
		{
			info2[i]  = report_magics_aux( *(timed[timed_counter].timer) );
			info[i++] = timed[timed_counter].status;
		}
	}

	if (p_ptr->confusing)
	{
		info2[i]  = 7;
		info[i++] = "covered with confusing magics";
	}
	if (p_ptr->word_recall)
	{
		info2[i]  = report_magics_aux(p_ptr->word_recall);
		info[i++] = "waiting to be recalled";
	}

	/* Save the screen */
	Term_save();

	/* Erase the screen */
	for (k = 1; k < 24; k++) prt("", k, 13);

	/* Label the information */
	prt("     Your Current Magic:", 1, 15);

	/* We will print on top of the map (column 13) */
	for (k = 2, j = 0; j < i; j++)
	{
		/* Show the info */
		sprintf( Dummy, "You are %s %s.", info[j],
			report_magic_durations[info2[j]] );
		prt(Dummy, k++, 15);

		/* Every 20 entries (lines 2 to 21), start over */
		if ((k == 22) && (j + 1 < i))
		{
			prt("-- more --", k, 15);
			inkey();
			for (; k > 2; k--) prt("", k, 15);
		}
	}

	/* Pause */
	prt("[Press any key to continue]", k, 13);
	inkey();

	/* Restore the screen */
	Term_load();
}

void teleport_swap(int dir)
{
	int tx, ty;
	cave_type * c_ptr;
	monster_type * m_ptr;
	monster_race * r_ptr;

	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}
	else
	{
		tx = px + ddx[dir];
		ty = py + ddy[dir];
	}
	c_ptr = &cave[ty][tx];

	if (!c_ptr->m_idx)
	{
		msg_print("You can't trade places with that!");
	}
	else
	{
		m_ptr = &m_list[c_ptr->m_idx];
		r_ptr = &r_info[m_ptr->r_idx];

		if (r_ptr->flags3 & RF3_RES_TELE)
		{
			msg_print("Your teleportation is blocked!");
		}
		else
		{
			sound(SOUND_TELEPORT);

			cave[py][px].m_idx = c_ptr->m_idx;

			/* Update the old location */
			c_ptr->m_idx = 0;

			/* Move the monster */
			m_ptr->fy = (byte)py;
			m_ptr->fx = (byte)px;

			/* Move the player */
			px = tx;
			py = ty;

			tx = m_ptr->fx;
			ty = m_ptr->fy;

			/* Update the monster (new location) */
			update_mon(cave[ty][tx].m_idx, TRUE);

			/* Redraw the old grid */
			lite_spot(ty, tx);

			/* Redraw the new grid */
			lite_spot(py, px);

			/* Check for new panel (redraw map) */
			verify_panel();

			/* Update stuff */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

			/* Update the monsters */
			p_ptr->update |= (PU_DISTANCE);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD);

			/* Handle stuff XXX XXX XXX */
			handle_stuff();
		}
	}
}

void alter_reality(void)
{
	msg_print("The world changes!");

	if (autosave_l)
	{
		is_autosave = TRUE;
		msg_print("Autosaving the game...");
		do_cmd_save_game();
		is_autosave = FALSE;
	}

	/* Leaving */
	new_level_flag = TRUE;
	came_from=START_RANDOM;
}

/* Take a weapon, add some flags and give it back ;), some weapons Malphas might decided to keep ;)*/
void malphas_gift(void)
{

}

void force_lite_spot( int y , int x )
{
	cave_type		*c_ptr;
	if (!in_bounds(y, x)) return;
	/* Access the grid */
	c_ptr = &cave[y][x];
	/* Illuminated and known */
	c_ptr->info |= (CAVE_MARK | CAVE_GLOW);
	lite_spot(y,x);
}

void blow_monster(int y , int x)
{
	cave_type *c_ptr;
	char m_name[80];

	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Check the grid */
	c_ptr = &cave[y][x];

	/* Delete the monster (if any) */
	if (c_ptr->m_idx)
	{
		monster_type *m_ptr = &m_list[c_ptr->m_idx];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		/* Get "the monster" or "something" */
		monster_desc(m_name, m_ptr, 0x04);
		/* If they are flying or home in the water, they remain in place*/
		if(water_ok(m_ptr->r_idx))
		{
			msg_format( "%^s struggles to remain in place." , m_name);
			/*He oughta be awake by now*/
			m_ptr->csleep = 0;
		}
		else if( r_ptr->level < dun_level && !((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags1 & RF1_GUARDIAN) || (r_ptr->flags1 & RF1_ALWAYS_GUARD)))
		{
			msg_format( "%^s drowns in the deluge." , m_name );
			delete_monster_idx(c_ptr->m_idx,TRUE);
		}
		else
		{
			msg_format( "%^s is flung away!" , m_name);
			/*He oughta be awake by now*/
			m_ptr->csleep = 0;
			/*and flung away!*/
			teleport_away(c_ptr->m_idx,100);
		}
	}
}

void find_open_spot_at_distance( int x , int y , int rad , int *spotx , int *spoty  )
{
	int ly,lx,k;
	/* Big area of affect */
	for (ly = (y - (rad+1)); ly <= (y + (rad+1)); ly++)
	{
		for (lx = (x - (rad+1)); lx <= (x + (rad+1)); lx++)
		{
			/* Skip illegal grids */
			if (!in_bounds(ly, lx)) continue;

			/* Extract the distance */
			k = distance(y, x, ly, lx);

			/* If we are at the right distance , set the spot and exit */
			if ( k==rad && cave_valid_bold(ly, lx) && cave_empty_bold(ly, lx) )
			{
				*spotx = lx;
				*spoty = ly;
				return;
			}
		}
	}
	/* If we didnt find a good place, we place them at the foot of the player, lucky bastard */
	*spotx = px;
	*spoty = py;
}



void drown_object(int y , int x , int spoty , int spotx)
{
	cave_type *c_ptr;
	char o_name[80];
	s16b this_o_idx, next_o_idx = 0;

	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Check the grid */
	c_ptr = &cave[y][x];

	/* Scan all objects in the grid */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Wipe the object */
		if( (o_ptr->art_name || artefact_p(o_ptr)) )
		{
			/*Drop it at the players' feet*/
		    o_ptr->ix = spotx;
			o_ptr->iy = spoty;
			c_ptr = &cave[spoty][spotx];
			/* No stacking (allow combining) */
			if (!testing_stack)
			{
				delete_object(spoty, spotx);
				c_ptr->o_idx = this_o_idx;
				o_ptr->next_o_idx = 0;
			}
			else
			{
				/* Build a stack */
				o_ptr->next_o_idx = c_ptr->o_idx;
				/* Place the object */
				c_ptr->o_idx = this_o_idx;
				/* Describe the object */
				if(px==spotx && py==spoty)
					msg_format("%^s floats to your feet." , o_name);
				else
					msg_format("%^s drifts away." , o_name);
			}
		}
		else
		{
			object_wipe(o_ptr);
			/* Count objects */
			o_cnt--;
		}
	}

	/* Reread the original spot */
	c_ptr = &cave[y][x];
	/* Objects are gone unless if we were trying to remove the original spot, hmmmm*/
	if(x!=spotx||y!=spoty)
		c_ptr->o_idx = 0;

	/* Visual update, probably superfluous */
	lite_spot(y, x);
}

/* Change the dungeon into a local lake/sea with some fitting monsters ;) */
void behemoth_call(void)
{
	int lx,ly,x1,y1,k;
	int spotx =px;
	int spoty =py;

	cave_type		*c_ptr;

	x1 = px;
	y1 = py;

	find_open_spot_at_distance(  x1,y1,17,&spotx,&spoty);
/*
	spotx = px;
	spoty = py;
 */

	/* Big area of affect */
	for (ly = (y1 - 15); ly <= (y1 + 15); ly++)
	{
		for (lx = (x1 - 15); lx <= (x1 + 15); lx++)
		{
			/* Skip illegal grids */
			if (!in_bounds(ly, lx)) continue;

			/* Extract the distance */
			k = distance(y1, x1, ly, lx);


			/* Stay in the circle of death */
			if (k > 16) continue;

			/* If we couldnt find a good spot for artefacts then the player should stay on land*/
			if( lx == px && ly == py && spotx == px && spoty == py ) continue;

			/* Access the grid */
			c_ptr = &cave[ly][lx];

			/* Destroy valid grids */
			if (/*cave_valid_bold(ly, lx)*/!cave_perma_grid(c_ptr) && (  ( k<6 ) || ( k>=6 && cave_floor_bold(ly, lx)  )  ) )
			{
				/* Delete objects */
				/*delete_object(ly, lx);*/
				drown_object(ly,lx, spoty,spotx);

				/* Try to blow away the monster */
				blow_monster(ly, lx);

				/* Create water */
				c_ptr->feat = FEAT_WATER;

				/* No longer part of a room or vault */
				c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY);

				/* Illuminated and known */
				c_ptr->info |= (CAVE_MARK | CAVE_GLOW);
			}/* End of fixing cave*/
		}/* End of x */
	}/* End of y */

	/* Light up around the water damage */
	for (ly = (y1 - 15); ly <= (y1 + 15); ly++)
	{
		for (lx = (x1 - 15); lx <= (x1 + 15); lx++)
		{
			/* Skip illegal grids */
			if (!in_bounds(ly, lx)) continue;

			/* Access the grid */
			c_ptr = &cave[ly][lx];

			if( c_ptr->feat == FEAT_WATER )
			{
				/*vertical*/
				force_lite_spot(ly+1,lx);
				force_lite_spot(ly-1,lx);
				/*Horizontal*/
				force_lite_spot(ly,lx+1);
				force_lite_spot(ly,lx-1);
				/*Diagonal*/
				force_lite_spot(ly-1,lx-1);
				force_lite_spot(ly+1,lx-1);
				force_lite_spot(ly-1,lx+1);
				force_lite_spot(ly+1,lx+1);
			}
		}/* End of x */
	}/* End of y */

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

}

/*
 * Spell : Dimensional Gate, returns false if the player presses escape during target selection
 */
int spell_dimensional_gate( int	plev )
{
	int ii = 0, ij = 0;
	msg_print("Choose a destination.");
	if (!tgt_pt(&ii,&ij)) return FALSE;
	p_ptr->energy -= 60 - plev;
	if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) || (cave[ij][ii].feat == FEAT_WATER) ||
		(distance(ij,ii,py,px) > plev + 2) ||
		(!rand_int(plev * plev / 2)))
	{
		msg_print("You fail to exit the astral plane correctly!");
		p_ptr->energy -= 100;
		teleport_player(10);
		return FALSE;
	}
	teleport_player_to(ij,ii);
	return TRUE;
}

/*
 * Spell : Basic Resistance, increase temporary protection for the 5 basic elements
 */
bool spell_basic_resistance( int duration )
{
	int total = set_timed_effect( TIMED_OPPOSE_ACID, p_ptr->oppose_acid + randint(duration) + duration) +
                set_timed_effect( TIMED_OPPOSE_ELEC, p_ptr->oppose_elec + randint(duration) + duration) +
                set_timed_effect( TIMED_OPPOSE_FIRE, p_ptr->oppose_fire + randint(duration) + duration) +
                set_timed_effect( TIMED_OPPOSE_COLD, p_ptr->oppose_cold + randint(duration) + duration) +
                set_timed_effect( TIMED_OPPOSE_POIS, p_ptr->oppose_pois + randint(duration) + duration);

	if(total) return TRUE;
	return FALSE;
}
