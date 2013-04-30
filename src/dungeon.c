/* File: dungeon.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-6 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"


/*
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 * TODO: a lot of code is here duplicated with value_check_aux3 in spells2.c
 */
int value_check_aux1(const object_type *o_ptr)
{
	/* Artifacts */
	if (artifact_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (INSCRIP_TERRIBLE);

		/* Normal */
		return (INSCRIP_SPECIAL);
	}

	/* Ego-Items */
	if (ego_item_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (INSCRIP_WORTHLESS);

		/* Superb */
		if (o_ptr->xtra1) return (INSCRIP_SUPERB);

		/* Normal */
		return (INSCRIP_EXCELLENT);
	}

	/* Cursed items */
	if (cursed_p(o_ptr)) return (INSCRIP_CURSED);

	/* Broken items */
	if (broken_p(o_ptr)) return (INSCRIP_BROKEN);

	/* Coated items */
	if (coated_p(o_ptr)) return (INSCRIP_COATED);

	/* Magic item */
	if ((o_ptr->xtra1) && (object_power(o_ptr) > 0)) return (INSCRIP_EXCELLENT);

	/* Great "armor" bonus */
	if (o_ptr->to_a > 9) return (INSCRIP_GREAT);

	/* Great to_h bonus */
	if (o_ptr->to_h > 9) return (INSCRIP_GREAT);

	/* Great to_d bonus */
	if (o_ptr->to_d >
	    MAX(7, k_info[o_ptr->k_idx].dd * k_info[o_ptr->k_idx].ds))
	  return (INSCRIP_GREAT);

	/* Great "weapon" dice */
	if (o_ptr->dd > k_info[o_ptr->k_idx].dd) return (INSCRIP_GREAT);

	/* Great "weapon" sides */
	if (o_ptr->ds > k_info[o_ptr->k_idx].ds) return (INSCRIP_GREAT);

	/* Very good "armor" bonus */
	if (o_ptr->to_a > 5) return (INSCRIP_VERY_GOOD);

	/* Good "weapon" bonus */
	if (o_ptr->to_h + o_ptr->to_d > 7) return (INSCRIP_VERY_GOOD);

	/* Good "armor" bonus */
	if (o_ptr->to_a > 0) return (INSCRIP_GOOD);

	/* Good "weapon" bonus */
	if (o_ptr->to_h + o_ptr->to_d > 0) return (INSCRIP_GOOD);

	/* Default to "average" */
	return (INSCRIP_AVERAGE);
}


/*
 * Return a "feeling" (or NULL) about an item.  Method 2 (Light).
 */
int value_check_aux2(const object_type *o_ptr)
{
	/* If sensed magical, have no more value to add */
	if ((o_ptr->feeling == INSCRIP_GOOD) || (o_ptr->feeling == INSCRIP_VERY_GOOD)
		|| (o_ptr->feeling == INSCRIP_GREAT) || (o_ptr->feeling == INSCRIP_EXCELLENT)
		|| (o_ptr->feeling == INSCRIP_SUPERB) || (o_ptr->feeling == INSCRIP_SPECIAL)
		|| (o_ptr->feeling == INSCRIP_MAGICAL)) return (0);

	/* Cursed items (all of them) */
	if (cursed_p(o_ptr))
	{
		if (o_ptr->feeling == INSCRIP_ARTIFACT) return (INSCRIP_TERRIBLE);
		else if (o_ptr->feeling == INSCRIP_HIGH_EGO_ITEM) return (INSCRIP_WORTHLESS);
		else if (o_ptr->feeling == INSCRIP_EGO_ITEM) return (INSCRIP_WORTHLESS);
		return (INSCRIP_CURSED);
	}

	/* Broken items (all of them) */
	if (broken_p(o_ptr))
	{
		if (o_ptr->feeling == INSCRIP_ARTIFACT) return (INSCRIP_TERRIBLE);
		else if (o_ptr->feeling == INSCRIP_HIGH_EGO_ITEM) return (INSCRIP_WORTHLESS);
		else if (o_ptr->feeling == INSCRIP_EGO_ITEM) return (INSCRIP_WORTHLESS);
		return (INSCRIP_BROKEN);
	}

	/* Coated items */
	if (coated_p(o_ptr)) return (INSCRIP_COATED);

	/* Artifacts -- except cursed/broken ones */
	if (artifact_p(o_ptr))
	{
		/* Known to be artifact strength */
		if ((o_ptr->feeling == INSCRIP_UNBREAKABLE)
			|| (o_ptr->feeling == INSCRIP_ARTIFACT)) return (INSCRIP_SPECIAL);

		return (INSCRIP_UNCURSED);
	}

	/* Ego-Items -- except cursed/broken ones */
	if (ego_item_p(o_ptr))
	{
		if (o_ptr->feeling == INSCRIP_HIGH_EGO_ITEM) return (INSCRIP_SUPERB);
		else if (o_ptr->feeling == INSCRIP_EGO_ITEM) return (INSCRIP_EXCELLENT);
		return (INSCRIP_UNCURSED);
	}

	/* Magic-Items */
	if (o_ptr->xtra1)
	{
		if (o_ptr->feeling == INSCRIP_EGO_ITEM) return (INSCRIP_EXCELLENT);
		return (INSCRIP_UNCURSED);
	}

	/* Good armor bonus */
	if (o_ptr->to_a > 0)
	{
		if (o_ptr->feeling == INSCRIP_UNUSUAL) return (INSCRIP_MAGICAL);
		return (INSCRIP_UNCURSED);
	}

	/* Good weapon bonuses */
	if (o_ptr->to_h + o_ptr->to_d > 0)
	{
		if (o_ptr->feeling == INSCRIP_UNUSUAL) return (INSCRIP_MAGICAL);
		return (INSCRIP_UNCURSED);
	}

	/* No feeling */
	return (INSCRIP_AVERAGE);
}



/*
 * Sense the inventory
 *
 * Now instead of class-based sensing, this notices various
 * abilities on items that we don't notice any other way.
 */
static void sense_inventory(void)
{
	int i;

	int plev = p_ptr->lev;

	u32b f1=0x0L;
	u32b f2=0x0L;
	u32b f3=0x0L;
	u32b f4=0x0L;

	u32b af1=0x0L;
	u32b af2=0x0L;
	u32b af3=0x0L;
	u32b af4=0x0L;

	object_type *o_ptr;

	/*** Check for "sensing" ***/

	/* No sensing when confused */
	if (p_ptr->confused) return;

	/* No sensing when paralyzed */
	if (p_ptr->paralyzed) return;

	/* No sensing when in stastis */
	if (p_ptr->stastis) return;

	/* No sensing when asleep */
	if (p_ptr->psleep > PY_SLEEP_ASLEEP) return;

	/* No sensing when knocked out */
	if (p_ptr->stun > 100) return;

	if (cp_ptr->sense_squared)
	{
		if (0 != rand_int(cp_ptr->sense_base / (plev * plev + cp_ptr->sense_div)))
			return;
	}
	else
	{
		if (0 != rand_int(cp_ptr->sense_base / (plev + cp_ptr->sense_div)))
			return;
	}


	/*** Sense everything ***/

	/* Check everything */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Hack -- we seem to get a source of corrupt objects that crash this routine. Putting this warning in. */
		if ((o_ptr->k_idx >= z_info->k_max) || (o_ptr->k_idx < 0))
		{
			msg_format("BUG: Object corruption detected (kind %d, held by %s). Please report.",o_ptr->k_idx, o_ptr->held_m_idx ? r_name + r_info[o_ptr->held_m_idx].name : "floor");

			o_ptr->k_idx = 0;
			continue;
		}

		/* Sense flags to see if we have ability */
		if ((i >= INVEN_WIELD) && !(IS_QUIVER_SLOT(i)))
		{
			u32b if1,if2,if3,if4;

			object_flags(o_ptr,&if1,&if2,&if3,&if4);

			af1 |= if1;
			af2 |= if2;
			af3 |= if3;
			af4 |= if4;

		}

		/* Sense flags to see if we gain ability */
		if (!(o_ptr->ident & (IDENT_MENTAL)) && (i >= INVEN_WIELD))
		{
			u32b if1,if2,if3,if4;

			object_flags(o_ptr,&if1,&if2,&if3,&if4);

			f1 |= (if1 & ~(o_ptr->may_flags1)) & ~(o_ptr->can_flags1);
			f2 |= (if2 & ~(o_ptr->may_flags2)) & ~(o_ptr->can_flags2);
			f3 |= (if3 & ~(o_ptr->may_flags3)) & ~(o_ptr->can_flags3);
			f4 |= (if4 & ~(o_ptr->may_flags4)) & ~(o_ptr->can_flags4);
		}
	}

	/* Hack --- silently notice stuff */
	if (f1 & (TR1_STEALTH)) equip_can_flags(TR1_STEALTH,0x0L,0x0L,0x0L);
	else if (!(af1 & (TR1_STEALTH)) ) equip_not_flags(TR1_STEALTH,0x0L,0x0L,0x0L);

	if (f1 & (TR1_SEARCH)) equip_can_flags(TR1_SEARCH,0x0L,0x0L,0x0L);
	else if (!(af1 & (TR1_SEARCH)) ) equip_not_flags(TR1_SEARCH,0x0L,0x0L,0x0L);

	if (f3 & (TR3_SLOW_DIGEST)) equip_can_flags(0x0L,0x0L,TR3_SLOW_DIGEST,0x0L);
	else if (!(af3 & (TR3_SLOW_DIGEST))) equip_not_flags(0x0L,0x0L,TR3_SLOW_DIGEST,0x0L);

	if (f3 & (TR3_REGEN_HP)) equip_can_flags(0x0L,0x0L,TR3_REGEN_HP,0x0L);
	else if (!(af3 & (TR3_REGEN_HP))) equip_not_flags(0x0L,0x0L,TR3_REGEN_HP,0x0L);

	if (f3 & (TR3_REGEN_MANA)) equip_can_flags(0x0L,0x0L,TR3_REGEN_MANA,0x0L);
	else if (!(af3 & (TR3_REGEN_MANA))) equip_not_flags(0x0L,0x0L,TR3_REGEN_MANA,0x0L);

	if (f3 & (TR3_HUNGER)) equip_can_flags(0x0L,0x0L,0x0L,TR3_HUNGER);
	else if (!(af3 & (TR3_HUNGER))) equip_not_flags(0x0L,0x0L,0x0L,TR3_HUNGER);
}



/*
 * Regenerate hit points
 */
static void regenhp(int percent)
{
	s32b new_chp, new_chp_frac;
	int old_chp;

	/* Save the old hitpoints */
	old_chp = p_ptr->chp;

	/* Extract the new hitpoints */
	new_chp = ((long)p_ptr->mhp) * percent + PY_REGEN_HPBASE;
	p_ptr->chp += (s16b)(new_chp >> 16);   /* div 65536 */

	/* check for overflow */
	if ((p_ptr->chp < 0) && (old_chp > 0)) p_ptr->chp = MAX_SHORT;
	new_chp_frac = (new_chp & 0xFFFF) + p_ptr->chp_frac;	/* mod 65536 */
	if (new_chp_frac >= 0x10000L)
	{
		p_ptr->chp_frac = (u16b)(new_chp_frac - 0x10000L);
		p_ptr->chp++;
	}
	else
	{
		p_ptr->chp_frac = (u16b)new_chp_frac;
	}

	/* Fully healed */
	if (p_ptr->chp >= p_ptr->mhp)
	{
		p_ptr->chp = p_ptr->mhp;
		p_ptr->chp_frac = 0;
	}

	/* Notice changes */
	if (old_chp != p_ptr->chp)
	{
		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}
}


/*
 * Regenerate mana points
 */
static void regenmana(int percent)
{
	s32b new_mana, new_mana_frac;
	int old_csp;

	old_csp = p_ptr->csp;
	new_mana = ((long)p_ptr->msp) * percent + PY_REGEN_MNBASE;
	p_ptr->csp += (s16b)(new_mana >> 16);	/* div 65536 */
	/* check for overflow */
	if ((p_ptr->csp < 0) && (old_csp > 0))
	{
		p_ptr->csp = MAX_SHORT;
	}
	new_mana_frac = (new_mana & 0xFFFF) + p_ptr->csp_frac;	/* mod 65536 */
	if (new_mana_frac >= 0x10000L)
	{
		p_ptr->csp_frac = (u16b)(new_mana_frac - 0x10000L);
		p_ptr->csp++;
	}
	else
	{
		p_ptr->csp_frac = (u16b)new_mana_frac;
	}

	/* Must set frac to zero even if equal */
	if (p_ptr->csp >= p_ptr->msp)
	{
		p_ptr->csp = p_ptr->msp;
		p_ptr->csp_frac = 0;
	}

	/* Redraw mana */
	if (old_csp != p_ptr->csp)
	{
		/* Update mana */
		p_ptr->update |= (PU_MANA);				

		/* Redraw */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}
}




/*
 * Monster hook to use for 'wandering' monsters.
 */
bool dun_level_mon(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* If no restriction, accept anything */
	if (!(t_info[p_ptr->dungeon].r_char) && !(t_info[p_ptr->dungeon].r_flag)) return (TRUE);

	/* Hack -- Accept monsters with graphic */
	if ((t_info[p_ptr->dungeon].r_char) && (r_ptr->d_char == t_info[p_ptr->dungeon].r_char)) return (TRUE);

	/* Hack -- Accept monsters with flag */
	if (t_info[p_ptr->dungeon].r_flag)
	{
		int mon_flag = t_info[p_ptr->dungeon].r_flag-1;

		if ((mon_flag < 32) && 
			(r_ptr->flags1 & (1L << mon_flag))) return (TRUE);

		if ((mon_flag >= 32) && 
			(mon_flag < 64) && 
			(r_ptr->flags2 & (1L << (mon_flag -32)))) return (TRUE);

		if ((mon_flag >= 64) && 
			(mon_flag < 96) && 
			(r_ptr->flags3 & (1L << (mon_flag -64)))) return (TRUE);

		if ((mon_flag >= 96) && 
			(mon_flag < 128) && 
			(r_ptr->flags4 & (1L << (mon_flag -96)))) return (TRUE);
	}

	return (FALSE);

}


/*
 * Suffer from a disease.
 * 
 * This now occurs whilst changing levels, and while in dungeon for
 * quick diseases only.
 */
void suffer_disease(void)
{
	u32b old_disease = p_ptr->disease;

	/* Get hit by disease */
	if (p_ptr->disease & ((1 << DISEASE_BLOWS) - 1))
	{
		int i, n, effect = 0;

		msg_print("You feel an illness eat away at you.");

		disturb(0,0);

		n = 0;

		/* Select one of the possible effects that the player can suffer */
		for (i = 1; i < (1 << DISEASE_BLOWS); i <<=1)
		{
			if (!(p_ptr->disease & i)) continue;
		
			if (!rand_int(++n)) effect = i;
		}

		switch (effect)
		{
			case DISEASE_LOSE_STR:
			{
				dec_stat(A_STR, p_ptr->disease & (DISEASE_POWER) ? randint(6) : 1, 0);
				break;
			}

			case DISEASE_LOSE_INT:
			{
				dec_stat(A_INT, p_ptr->disease & (DISEASE_POWER) ? randint(6) : 1, 0);
				break;
			}

			case DISEASE_LOSE_WIS:
			{
				dec_stat(A_WIS, p_ptr->disease & (DISEASE_POWER) ? randint(6) : 1, 0);
				break;
			}

			case DISEASE_LOSE_DEX:
			{
				dec_stat(A_DEX, p_ptr->disease & (DISEASE_POWER) ? randint(6) : 1, 0);
				/* An exception --- disease does not glue DEX and AGI.
				   See DISEASE_SLOW below, however. */
				break;
			}

			case DISEASE_LOSE_CON:
			{
				dec_stat(A_CON, p_ptr->disease & (DISEASE_POWER) ? randint(6) : 1, 0);
				break;
			}

			case DISEASE_LOSE_CHR:
			{
				dec_stat(A_CHR, p_ptr->disease & (DISEASE_POWER) ? randint(6) : 1, 0);
				break;
			}

			case DISEASE_HUNGER:
			{
				msg_print("You vomit up your food!");
				(void)set_food(PY_FOOD_STARVE - 1);
				disturb(0, 0);
				break;
			}

			case DISEASE_THIRST:
			{
				(void)set_food(p_ptr->food - randint(p_ptr->disease & (DISEASE_POWER) ? 100 : 30) + 10);
				break;
			}

			case DISEASE_CUT:
			{
				(void)set_cut(p_ptr->cut + randint(p_ptr->disease & (DISEASE_POWER) ? 100 : 30) + 10);
				break;
			}

			case DISEASE_STUN:
			{
				(void)set_stun(p_ptr->stun + randint(p_ptr->disease & (DISEASE_POWER) ? 40 : 10) + 2);
				break;
			}

			case DISEASE_POISON:
			{
				(void)set_poisoned(p_ptr->poisoned + randint(p_ptr->disease & (DISEASE_POWER) ? 100 : 30) + 10);
				break;
			}

			case DISEASE_BLIND:
			{
				(void)set_blind(p_ptr->blind + randint(p_ptr->disease & (DISEASE_POWER) ? 40 : 10) + 2);
				break;
			}

			case DISEASE_FEAR:
			{
				(void)set_afraid(p_ptr->afraid + randint(p_ptr->disease & (DISEASE_POWER) ? 100 : 30) + 10);
				break;
			}

			case DISEASE_CONFUSE:
			{
				(void)set_confused(p_ptr->confused + randint(p_ptr->disease & (DISEASE_POWER) ? 10 : 3) + 1);
				break;
			}

			case DISEASE_HALLUC:
			{
				(void)set_image(p_ptr->image + randint(p_ptr->disease & (DISEASE_POWER) ? 100 : 30) + 10);
				break;
			}

			case DISEASE_AMNESIA:
			{
				(void)set_amnesia(p_ptr->amnesia + randint(p_ptr->disease & (DISEASE_POWER) ? 100 : 30) + 10);
				break;
			}

			case DISEASE_CURSE:
			{
				(void)set_cursed(p_ptr->cursed + randint(p_ptr->disease & (DISEASE_POWER) ? 100 : 30) + 10);
				break;
			}

			case DISEASE_SLOW:
			{
				(void)set_slow(p_ptr->slow + randint(p_ptr->disease & (DISEASE_POWER) ? 100 : 30) + 10);
				/* Also slightly reduce agility. */
				dec_stat(A_AGI, p_ptr->disease & (DISEASE_POWER) ? randint(3) : 1, 0);
				break;
			}

			case DISEASE_DISPEL:
			{
				/* Powerful diseases also drain mana */
				if (p_ptr->msp && (p_ptr->disease & (DISEASE_POWER)))
				{
					p_ptr->csp -= randint(30);
					if (p_ptr->csp < 0) p_ptr->csp = 0;

					/* Update mana */
					p_ptr->update |= (PU_MANA);				

					/* Redraw */
					p_ptr->redraw |= (PR_MANA);

					/* Window stuff */
					p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
				}

				(void)project_p(SOURCE_DISEASE, effect, p_ptr->py, p_ptr->px, 0, GF_DISPEL);
				break;
			}

			case DISEASE_SLEEP:
			{
				(void)set_msleep(p_ptr->msleep + randint(p_ptr->disease & (DISEASE_POWER) ? 40 : 10) + 2);
				break;
			}

			case DISEASE_PETRIFY:
			{
				(void)set_petrify(p_ptr->petrify + randint(p_ptr->disease & (DISEASE_POWER) ? 10 : 3) + 1);
				break;
			}

			case DISEASE_PARALYZE:
			{
				(void)set_paralyzed(p_ptr->paralyzed + randint(p_ptr->disease & (DISEASE_POWER) ? 10 : 3) + 1);
				disturb(0,0);
				break;
			}

			case DISEASE_STASTIS:
			{
				(void)set_stastis(p_ptr->stastis + randint(p_ptr->disease & (DISEASE_POWER) ? 10 : 3) + 1);
				disturb(0,0);
				break;
			}
		}

		/* The player is going to suffer further */
		if ((p_ptr->disease & (DISEASE_QUICK)) && !(rand_int(3)))
		{
			/* Breakfast time... */
			msg_print("Something pushes through your skin.");
			msg_print("Its... hatching...");

			/* How many eggs? */
			n = randint(p_ptr->depth / 5) + 1;

			/* A nasty chest wound */
			take_hit(damroll(n, 8),"the birth of a parasite");
		
			/* Set parasite race */
			summon_race_type = parasite_hack[effect];

			/* Drop lots of parasites */
			for (i = 0; i < n; i++) (void)summon_specific(p_ptr->py, p_ptr->py, 99, SUMMON_FRIEND, FALSE, 0L);

			/* Aggravate if not light */
			if (!(p_ptr->disease & (DISEASE_LIGHT))) aggravate_monsters(-1);

			/* Paralyze if heavy */
			if (p_ptr->disease & (DISEASE_HEAVY)) (void)set_paralyzed(p_ptr->paralyzed + randint(3) + 1);

			/* Not a pleasant cure but nonetheless */
			p_ptr->disease &= (DISEASE_HEAVY | DISEASE_PERMANENT);
		}

		/* Mutate plague */
		if ((p_ptr->disease & (DISEASE_DISEASE)) && !(rand_int(3)))
		{
			if ((n < 3) && (p_ptr->disease & (DISEASE_HEAVY)))
				p_ptr->disease |= (1 << rand_int(DISEASE_TYPES_HEAVY));
			else if (n < 3)
				p_ptr->disease |= (1 << rand_int(DISEASE_TYPES));
		
			if (n > 1) p_ptr->disease &= ~(1 << rand_int(DISEASE_TYPES));
			if (!rand_int(20)) p_ptr->disease |= (DISEASE_LIGHT);
		}
	}
	
	/* All diseases mutate to get blows if they have no effect currently*/
	else if ((!rand_int(3)) && ((p_ptr->disease & ((1 << DISEASE_TYPES_HEAVY) -1 )) == 0))
	{
		if (p_ptr->disease & (DISEASE_HEAVY))
		{
			p_ptr->disease |= (1 << rand_int(DISEASE_TYPES_HEAVY));
		}
		else
		{
			p_ptr->disease |= (1 << rand_int(DISEASE_BLOWS));
		}
		if (!rand_int(20)) p_ptr->disease |= (DISEASE_LIGHT);
	}

	/* Worsen black breath */
	if ((p_ptr->disease & (1 << DISEASE_SPECIAL)) && !(rand_int(10)))
	{
		if (p_ptr->disease & (DISEASE_HEAVY))
			p_ptr->disease |= (1 << rand_int(DISEASE_TYPES_HEAVY));
		else
			p_ptr->disease |= (1 << rand_int(DISEASE_TYPES));
	}

	/* The player is on the mend */
	if ((p_ptr->disease & (DISEASE_LIGHT)) && !(rand_int(6)))
	{
		msg_print("The illness has subsided.");
		p_ptr->disease &= (DISEASE_HEAVY | DISEASE_PERMANENT);

		p_ptr->redraw |= (PR_DISEASE);
		
		if (disturb_state) disturb(0, 0);
	}

	/* Diseases? */
	else if (old_disease != p_ptr->disease)
	{
		char output[1024];

		disease_desc(output, sizeof(output), old_disease, p_ptr->disease);

		msg_print(output);

		p_ptr->redraw |= (PR_DISEASE);
	}
}


/*
 * Handle certain things once every 10 game turns
 */
static void process_world(void)
{
	int i, j, k;

	feature_type *f_ptr;

	int mimic;

	int regen_amount;

	object_type *o_ptr;

	cptr name;

	/* Every 10 game turns */
	if (turn % 10) return;

	/*** Check the Time and Load ***/

	if (!(turn % 1000))
	{
		/* Check time and load */
		if ((0 != check_time()) || (0 != check_load()))
		{
			/* Warning */
			if (closing_flag <= 2)
			{
				/* Disturb */
				disturb(0, 0);

				/* Count warnings */
				closing_flag++;

				/* Message */
				msg_print("The gates to ANGBAND are closing...");
				msg_print("Please finish up and/or save your game.");
			}

			/* Slam the gate */
			else
			{
				/* Message */
				msg_print("The gates to ANGBAND are now closed.");

				/* Stop playing */
				p_ptr->playing = FALSE;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}
	}

	/*** Handle the "town" (stores and sunshine) ***/

	/* While in town */
	if (p_ptr->depth == min_depth(p_ptr->dungeon))
	{
		/* Hack -- Daybreak/Nighfall in town */
		if (!(turn % ((10L * TOWN_DAWN) / 2)))
		{
			bool dawn;

			/* Check for dawn */
			dawn = (!(turn % (10L * TOWN_DAWN)));

			/* Day breaks */
			if (dawn)
			{
				/* Message */
				msg_print("The sun has risen.");

			}

			/* Night falls */
			else
			{
				/* Message */
				msg_print("The sun has fallen.");
			}

			/* Illuminate */
			town_illuminate(dawn);

			/* Update runes */
			p_ptr->update |= (PU_RUNES);
		}
	}


	/* While in the dungeon */
	else
	{
		/*** Update the Stores ***/

		/* Update the stores once a day (while in dungeon) */
		if (!(turn % (10L * STORE_TURNS)))
		{
		  int n, i;

			/* Message */
			if (cheat_xtra) msg_print("Updating Shops...");

			/* Maintain each shop (except home, special locations) */
			for (n = 0; n < total_store_count; n++)
			{

			  /* no restocking for alien towns */
			  town_type *t_ptr = &t_info[p_ptr->town];
			  for (i = 0; i < MAX_STORES; i++)
			    {
			      if (t_ptr->store_index[i] == n)
				break;
			    }
			  if (i < MAX_STORES)
			    /* Maintain */
			    store_maint(n);
			}

			/* Sometimes, shuffle the shop-keepers */
			if ((total_store_count) && (rand_int(STORE_SHUFFLE) < 3))
			{
				/* Message */
				if (cheat_xtra) msg_print("Shuffling a Shopkeeper...");

				/* Pick a random shop (except home) */
				n = randint(total_store_count - 1);

			  /* no shuffling for alien towns */
			  town_type *t_ptr = &t_info[p_ptr->town];
			  for (i = 0; i < MAX_STORES; i++)
			    {
			      if (t_ptr->store_index[i] == n)
				break;
			    }
			  if (i < MAX_STORES)
			    /* Shuffle it */
			    store_shuffle(n);
			}

			/* Message */
			if (cheat_xtra) msg_print("Done.");
		}
	}


	/*** Process the monsters ***/

	/* Create new monsters */
	if (p_ptr->depth)
	{
		/* Base odds against a new monster (200 to 1) */
		int odds = MAX_M_ALLOC_CHANCE;

		int max_m_cnt = (2 * p_ptr->depth / 3) + 20;

		/* Do not overpopulate the dungeon (important) */
		if (m_cnt > max_m_cnt) odds += 4 * (m_cnt - max_m_cnt);

		/* Fewer monsters if stealthy, more if deep */
		odds += p_ptr->skill_stl * 2;
		odds -= p_ptr->depth / 2;

		/* Check for creature generation */
		if (!rand_int(odds))
		{
			bool slp = FALSE;

			/* Sneaky characters make monsters sleepy */
			if (p_ptr->skill_stl > rand_int(100)) slp = TRUE;

			/* Make a new monster */
			(void)alloc_monster(MAX_SIGHT + 5, slp);
		}
	}


	/*** Stastis ***/
	if (p_ptr->stastis)
	{
		(void)set_stastis(p_ptr->stastis - 1);

		/* Update dynamic terrain */
		update_dyna();

		return;
	}




	/*** Damage over Time ***/

	/* Get the feature */
	f_ptr = &f_info[cave_feat[p_ptr->py][p_ptr->px]];

	/* Get the mimiced feature */
	mimic = f_ptr->mimic;

	/* Use covered or bridged if necessary */
	if ((f_ptr->flags2 & (FF2_COVERED)) || (f_ptr->flags2 & (FF2_BRIDGED)))
	{
		f_ptr = &(f_info[f_ptr->mimic]);
	}

	/* Take damage from features */
	if (!(f_ptr->flags1 & (FF1_HIT_TRAP)) &&
	    ((f_ptr->blow.method) || (f_ptr->spell)))
	{
		/* Damage from terrain */
		hit_trap(p_ptr->py,p_ptr->px);
	}

	/* If paralyzed, we drown in shallow, deep or filled */
	if ((p_ptr->paralyzed || (p_ptr->stun >=100) || (p_ptr->psleep >= PY_SLEEP_ASLEEP)) &&
		(f_ptr->flags2 & (FF2_DEEP | FF2_SHALLOW | FF2_FILLED)))
	{
		/* Get the mimiced feature */
		mimic = f_ptr->mimic;

		/* Get the feature name */
		name = (f_name + f_info[mimic].name);

		msg_format("You are drowning %s%s!",(f_ptr->flags2 & (FF2_FILLED)?"":"in the "),name);

		/* Apply the blow */
		project_p(SOURCE_FEATURE, mimic, p_ptr->py, p_ptr->px, damroll(4,6), GF_WATER);

		/* Apply the blow */
		project_t(SOURCE_FEATURE, mimic, p_ptr->py, p_ptr->px, damroll(4,6), GF_WATER);
	}

	/* Take damage from poison */
	if (p_ptr->poisoned)
	{
		/* Take damage */
		take_hit(1, "poison");
	}

	/* Take damage from cuts */
	if (p_ptr->cut)
	{
		/* Mortal wound or Deep Gash */
		if (p_ptr->cut > 200)
		{
			i = 3;
		}

		/* Severe cut */
		else if (p_ptr->cut > 100)
		{
			i = 2;
		}

		/* Other cuts */
		else
		{
			i = 1;
		}

		/* Take damage */
		take_hit(i, "a fatal wound");
	}


	/*** Check the Food, Rest, and Regenerate ***/

	/* Tire normally */
	/* XXX We exclude situations where we adjust this counter elsewhere */
	if (!(p_ptr->resting || p_ptr->searching || p_ptr->running || p_ptr->paralyzed || p_ptr->stastis || (p_ptr->stun >= 100)) ||
			(f_ptr->flags2 & (FF2_FILLED)))
	{
		(void)set_rest(p_ptr->rest - p_ptr->tiring);
	}

	/* Digest normally */
	if (p_ptr->food < PY_FOOD_MAX)
	{
		/* Every 100 game turns */
		if (!(turn % 100))
		{
			/* Basic digestion rate based on speed */
			i = extract_energy[p_ptr->pspeed] * 2;

			/* Hunger takes more food */
			if ((p_ptr->cur_flags3 & (TR3_HUNGER)) != 0) i += 100;

			/* Regeneration takes more food */
			if (p_ptr->regen_hp > 0) i += 30 * p_ptr->regen_hp;

			/* Regeneration takes more food */
			if (p_ptr->regen_mana > 0) i += 30 * p_ptr->regen_mana;

			/* Slow digestion takes less food */
			if ((p_ptr->cur_flags3 & (TR3_SLOW_DIGEST)) != 0) i -= 10;

			/* Minimal digestion */
			if (i < 1) i = 1;

			/* Digest some food */
			(void)set_food(p_ptr->food - i);
		}
	}

	/* Digest quickly when gorged */
	else
	{
		/* Digest a lot of food */
		(void)set_food(p_ptr->food - 100);
	}

	/* Starve to death (slowly) */
	if (p_ptr->food < PY_FOOD_STARVE)
	{
		/* Calculate damage */
		i = (PY_FOOD_STARVE - p_ptr->food) / 10;

		/* Take damage */
		take_hit(i, "starvation");
	}

	/* Default regeneration */
	regen_amount = PY_REGEN_NORMAL;

	/* Getting Weak */
	if ((p_ptr->food < PY_FOOD_WEAK) || (p_ptr->rest < PY_REST_WEAK))
	{
		/* Lower regeneration */
		if (p_ptr->food < PY_FOOD_STARVE)
		{
			regen_amount = 0;
		}
		else if (p_ptr->food < PY_FOOD_FAINT)
		{
			regen_amount = PY_REGEN_FAINT;
		}
		else
		{
			regen_amount = PY_REGEN_WEAK;
		}

		/* Getting Faint */
		if (p_ptr->food < PY_FOOD_FAINT) 
		{
			/* Faint occasionally */
			if (!p_ptr->paralyzed && (rand_int(100) < 10))
			{
				/* Message */
				msg_print("You faint from the lack of food.");
				disturb(1, 0);

				/* Hack -- faint (bypass free action) */
				(void)set_paralyzed(p_ptr->paralyzed + 1 + rand_int(5));
			}
		}

		/* Getting Faint - lack of rest */
		if (p_ptr->rest < PY_REST_FAINT) 
		{
			/* Faint occasionally */
			if (!p_ptr->paralyzed && (rand_int(100) < 10))
			{
				/* Message */
				msg_print("You faint from exhaustion.");
				disturb(1, 0);

				/* Hack -- faint (bypass free action) */
				(void)set_paralyzed(p_ptr->paralyzed + 1 + rand_int(5));
			}
		}
	}	

	/* Searching or Resting */
	if (p_ptr->searching || p_ptr->resting)
	{
		regen_amount = regen_amount * 2;
	}

	/* Regenerate the mana */
	if (p_ptr->csp < p_ptr->msp)
	{
		if (!(p_ptr->cur_flags3 & (TR3_DRAIN_MANA)) &&
			!(p_ptr->disease & (DISEASE_DRAIN_MANA))
			&& (p_ptr->regen_mana >= 0)) regenmana(regen_amount * (p_ptr->regen_mana + 1));
	}

	/* Various things interfere with healing */
	if (p_ptr->paralyzed) regen_amount = 0;
	if (p_ptr->poisoned) regen_amount = 0;
	if (p_ptr->stun) regen_amount = 0;
	if (p_ptr->cut) regen_amount = 0;
	if (p_ptr->disease & (DISEASE_DRAIN_HP)) regen_amount = 0;
	if ((p_ptr->cur_flags3 & (TR3_DRAIN_HP)) != 0) regen_amount = 0;
	if (room_has_flag(p_ptr->py, p_ptr->px, ROOM_BLOODY)) regen_amount = 0;

	/* Regenerate Hit Points if needed */
	if ((p_ptr->chp < p_ptr->mhp) && (p_ptr->regen_hp >= 0))
	{
		regenhp(regen_amount * (p_ptr->regen_hp + 1));
	}


	/*** Timeout Various Things ***/

	/* Hack -- Hallucinating */
	if (p_ptr->image)
	{
		(void)set_image(p_ptr->image - 1);
	}

	/* Blindness */
	if (p_ptr->blind)
	{
		(void)set_blind(p_ptr->blind - 1);
	}

	/* Amnesia */
	if (p_ptr->amnesia)
	{
		(void)set_amnesia(p_ptr->amnesia - 1);
	}

	/* Monster curses */
	if (p_ptr->cursed)
	{
		(void)set_cursed(p_ptr->cursed - 1);
	}

	/* Petrification */
	if (p_ptr->petrify)
	{
		(void)set_petrify(p_ptr->petrify - 1);
	}

	/* Monster induced sleep */
	if (p_ptr->msleep)
	{
		(void)set_msleep(p_ptr->msleep - 1);

		if (p_ptr->psleep < PY_SLEEP_DROWSY) (void)set_psleep(PY_SLEEP_DROWSY);
	}

	/* Player induced sleep -- hack: goes in other direction */
	if (p_ptr->psleep)
	{
		(void)set_psleep(p_ptr->psleep + 1);
	}

	/* Times see-invisible */
	if (p_ptr->tim_invis)
	{
		(void)set_tim_invis(p_ptr->tim_invis - 1);
	}

	/* Timed infra-vision */
	if (p_ptr->tim_infra)
	{
		(void)set_tim_infra(p_ptr->tim_infra - 1);
	}

	/* Paralysis */
	if (p_ptr->paralyzed)
	{
		(void)set_paralyzed(p_ptr->paralyzed - 1);
	}

	/* Confusion */
	if (p_ptr->confused)
	{
		(void)set_confused(p_ptr->confused - 1);
	}

	/* Afraid */
	if (p_ptr->afraid)
	{
		(void)set_afraid(p_ptr->afraid - 1);
	}

	/* Temporary stat increase */
	for (i = 0; i < A_MAX; i++)
	{
		if (p_ptr->stat_inc_tim[i])
		{
			(void)set_stat_inc_tim(p_ptr->stat_inc_tim[i] - 1, i);
		}
	}

	/* Temporary stat decrease */
	for (i = 0; i < A_MAX; i++)
	{
		if (p_ptr->stat_dec_tim[i])
		{
			(void)set_stat_dec_tim(p_ptr->stat_dec_tim[i] - 1, i);
		}
	}

	/* Fast */
	if (p_ptr->fast)
	{
		(void)set_fast(p_ptr->fast - 1);
	}

	/* Fast */
	if (p_ptr->fast)
	{
		(void)set_fast(p_ptr->fast - 1);
	}

	/* Slow */
	if (p_ptr->slow)
	{
		(void)set_slow(p_ptr->slow - 1);
	}

	/* Protection from evil */
	if (p_ptr->protevil)
	{
		(void)set_protevil(p_ptr->protevil - 1);
	}

	/* Invulnerability */
	if (p_ptr->invuln)
	{
		(void)set_invuln(p_ptr->invuln - 1);
	}

	/* Heroism */
	if (p_ptr->hero)
	{
		(void)set_hero(p_ptr->hero - 1);
	}

	/* Super Heroism */
	if (p_ptr->shero)
	{
		(void)set_shero(p_ptr->shero - 1);
	}

	/* Blessed */
	if (p_ptr->blessed)
	{
		(void)set_blessed(p_ptr->blessed - 1);
	}

	/* Shield */
	if (p_ptr->shield)
	{
		(void)set_shield(p_ptr->shield - 1);
	}

	/* Oppose Acid */
	if (p_ptr->oppose_acid)
	{
		(void)set_oppose_acid(p_ptr->oppose_acid - 1);
	}

	/* Oppose Lightning */
	if (p_ptr->oppose_elec)
	{
		(void)set_oppose_elec(p_ptr->oppose_elec - 1);
	}

	/* Oppose Fire */
	if (p_ptr->oppose_fire)
	{
		(void)set_oppose_fire(p_ptr->oppose_fire - 1);
	}

	/* Oppose Cold */
	if (p_ptr->oppose_cold)
	{
		(void)set_oppose_cold(p_ptr->oppose_cold - 1);
	}

	/* Oppose Poison */
	if (p_ptr->oppose_pois)
	{
		(void)set_oppose_pois(p_ptr->oppose_pois - 1);
	}

	/* Free action */
	if (p_ptr->free_act)
	{
		(void)set_free_act(p_ptr->free_act - 1);
	}


	/*** Poison and Stun and Cut ***/

	/* Poison */
	if (p_ptr->poisoned)
	{
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);

		/* Some rooms make wounds magically worse */
		if (room_has_flag(p_ptr->py, p_ptr->px, ROOM_BLOODY)) adjust = -1;

		/* Apply some healing */
		(void)set_poisoned(p_ptr->poisoned - adjust);
	}

	/* Stun */
	if (p_ptr->stun)
	{
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);

		/* Apply some healing */
		(void)set_stun(p_ptr->stun - adjust);
	}

	/* Cut */
	if (p_ptr->cut)
	{
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);

		/* Hack -- Truly "mortal" wound */
		if (p_ptr->cut > 1000) adjust = 0;

		/* Some rooms make wounds magically worse */
		if (room_has_flag(p_ptr->py, p_ptr->px, ROOM_BLOODY)) adjust = -1;
		
		/* Apply some healing */
		(void)set_cut(p_ptr->cut - adjust);
	}

	/*** Process Light ***/

	/* Check for light being wielded */
	o_ptr = &inventory[INVEN_LITE];

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);

	/*** Process Inventory ***/

	/* Handle experience draining */
	if (((p_ptr->cur_flags3 & (TR3_DRAIN_EXP)) != 0) || (p_ptr->disease & (DISEASE_DRAIN_EXP)))
	{
		if ((rand_int(100) < 10) && (p_ptr->exp > 0))
		{
			/* Always notice */
			if (!(p_ptr->disease & (DISEASE_DRAIN_EXP))) equip_can_flags(0x0L,0x0L,TR3_DRAIN_EXP,0x0L);

			p_ptr->exp--;
			p_ptr->max_exp--;
			if (p_ptr->exp <= 0) p_ptr->exp = 0;

			check_experience();
		}
	}

	/* Handle hit point draining */
	if (((p_ptr->cur_flags3 & (TR3_DRAIN_HP)) != 0) || (p_ptr->disease & (DISEASE_DRAIN_HP)))
	{
		if ((rand_int(100) < 10) && (p_ptr->chp > 0))
		{
			/* Always notice */
			if (!(p_ptr->disease & (DISEASE_DRAIN_HP))) equip_can_flags(0x0L,0x0L,TR3_DRAIN_HP,0x0L);

			if (p_ptr->disease & (DISEASE_DRAIN_HP))
				take_hit(1, "disease");
			else
				take_hit(1, "curse");

		}
	}

	/* Handle mana draining */
	if (((p_ptr->cur_flags3 & (TR3_DRAIN_MANA)) != 0) || (p_ptr->disease & (DISEASE_DRAIN_MANA)))
	{
		if ((rand_int(100) < 10) && (p_ptr->csp > 0))
		{
			/* Always notice */
			if (!(p_ptr->disease & (DISEASE_DRAIN_MANA))) equip_can_flags(0x0L,0x0L,TR3_DRAIN_MANA,0x0L);

			p_ptr->csp--;
			p_ptr->csp_frac = 0;
			if (p_ptr->exp <= 0) p_ptr->csp = 0;

			/* Update mana */
			p_ptr->update |= (PU_MANA);				

			/* Redraw */
			p_ptr->redraw |= (PR_MANA);		
		}
	}

	/* Process timeouts */
	for (k = 0, j = 0, i = 0; i < INVEN_TOTAL; i++)
	{
		/* Get the object */
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Recharge activateable objects/rods */
		/* Also check for mimics/regenerating bodies */
		if (o_ptr->timeout > 0)
		{
			/* Recharge */
			o_ptr->timeout--;

			/* Notice changes */
			/* Now if 3 rods charging and timeout == 2 => 1 rod has recharged */
			if (!(o_ptr->timeout) || ((o_ptr->stackc) ? (o_ptr->timeout < o_ptr->stackc) :
					(o_ptr->timeout < o_ptr->number) ))
			{
				char o_name[80];

				u32b f1, f2, f3, f4;

				/* Get the flags */
				object_flags(o_ptr,&f1, &f2, &f3, &f4);

				/* Get a description */
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

				/* Hack -- lites */
				if (o_ptr->tval == TV_LITE) my_strcpy(o_name,"light",sizeof(o_name));

				/* Hack -- update torch radius */
				if (i == INVEN_LITE) p_ptr->update |= (PU_TORCH);

				/* Lanterns run dry */
				if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_LANTERN))
				{
					disturb(0, 0);
					msg_print("Your light has gone out!");
				}

				/* Torches / Spells run out */
				else if ((o_ptr->tval == TV_SPELL) || ((o_ptr->tval == TV_LITE) && !(artifact_p(o_ptr))))
				{
					/* Disturb */
					disturb(0, 0);

					/* Notice things */
					if (i < INVEN_PACK) j++;
					else k++;

					/* Message */
					if (o_ptr->timeout) msg_format("One of your %s has run out.",o_name);
					else msg_format("Your %s %s run out.", o_name,
					   (o_ptr->number == 1) ? "has" : "have");

					/* Destroy a spell if discharged */
					if (o_ptr->timeout)
					{
						if (o_ptr->number == 1) inven_drop_flags(o_ptr);

						inven_item_increase(i, -1);
					}
					else if (o_ptr->stackc)
					{
						if (o_ptr->number == o_ptr->stackc) inven_drop_flags(o_ptr);

						inven_item_increase(i, -(o_ptr->stackc));
					}
					else
					{
						inven_drop_flags(o_ptr);
						inven_item_increase(i,-(o_ptr->number));
					}

					inven_item_optimize(i);

				}

				/* Rods and activatible items charge */
				else if ((o_ptr->tval == TV_ROD) || (f3 & (TR3_ACTIVATE)))
				{
					/* Notice things */
					if (i < INVEN_PACK) j++;
					else k++;
	
					/* Message */
					if (o_ptr->timeout) msg_format("One of your %s has charged.",o_name);
					else msg_format("Your %s %s charged.", o_name,
					   (o_ptr->number == 1) ? "has" : "have");

					/* Reset stack */
					if ((o_ptr->timeout) && !(o_ptr->stackc)) o_ptr->stackc = o_ptr->number -1;
					else if (o_ptr->timeout) o_ptr->stackc--;
					else o_ptr->stackc = 0;

				}

				/* Bodies/mimics become a monster */
				else
				{
					/* Notice things */
					if (animate_object(i))
					{
						if (i < INVEN_PACK) j++;
						else k++;
					}
				}
			}

			/* The lantern / torch / spell is running low */
			else if ((o_ptr->timeout < 100) && (!(o_ptr->timeout % 10)) &&
				((o_ptr->tval == TV_SPELL) || (o_ptr->tval == TV_LITE)) &&
				!(artifact_p(o_ptr)))
			{
				char o_name[80];

				/* Get a description */
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

				if (disturb_minor) disturb(0, 0);

				if (o_ptr->tval == TV_SPELL) msg_format("Your %s is running out.", o_name);
				else if (o_ptr->sval == SV_LITE_LANTERN) msg_print("Your light is growing faint.");
				else msg_format("Your %s flame dims.", o_name);

				/* Hack -- update torch radius */
				if (i == INVEN_LITE) p_ptr->update |= (PU_TORCH);
			}
		}
	}

	/* Notice changes -- equip */
	if (k)
	{
		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);

		/* Disturb */
		if (disturb_minor) disturb(1, 0);
	}

	/* Notice changes - inventory */
	if (j)
	{
		/* Combine pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN);

		/* Disturb */
		if (disturb_minor) disturb(1,0);
	}

	/* Feel the inventory */
	sense_inventory();

	/* Show tips */
	if  (!(p_ptr->command_rep) && ((p_ptr->searching && !(turn % 1000)) ||
			(((level_flag & (LF1_TOWN)) != 0) && !(turn % 100))))
	{
		/* Show a tip */
		show_tip();
	}

	/*** Process Objects ***/

	/* Process objects */
	for (i = 1; i < o_max; i++)
	{
		/* Get the object */
		o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Timing out? */
		if (o_ptr->timeout > 0)
		{
			/* Check for light extinguishing */
			bool extinguish = ((o_ptr->timeout == 1) && check_object_lite(o_ptr));
			
			/* Recharge */
			o_ptr->timeout--;
			
			/* Extinguish lite */
			if ((extinguish) && (o_ptr->iy) && (o_ptr->ix))
			{
				/* Check for loss of light */
				check_attribute_lost(o_ptr->iy, o_ptr->ix, 2, CAVE_XLOS, require_halo, has_halo, redraw_halo_loss, remove_halo, reapply_halo);
			}

			/* Notice changes */
			if (!(o_ptr->timeout) || ((o_ptr->stackc) ? (o_ptr->timeout < o_ptr->stackc) :
				(o_ptr->timeout < o_ptr->number) ))
			{
				u32b f1, f2, f3, f4;

				/* Get the flags */
				object_flags(o_ptr,&f1, &f2, &f3, &f4);

				/* Spells run out */
				if (o_ptr->tval == TV_SPELL)
				{
					/* Destroy a spell if discharged */
					if (o_ptr->timeout) floor_item_increase(i, -1);
					else if (o_ptr->stackc) floor_item_increase(i, -(o_ptr->stackc));
					else
					{
						/* Was a terrain counter - note paranoia */
						if ((o_ptr->ident & (IDENT_STORE)) && (o_ptr->pval > 0) && !(o_ptr->held_m_idx))
						{
							/* Message */
							if (play_info[o_ptr->iy][o_ptr->ix] & (PLAY_MARK))
							{
								msg_format("The %s fades.", f_name + f_info[cave_feat[o_ptr->iy][o_ptr->ix]].name);
							}

							/* Revert to old feature */
							cave_set_feat(o_ptr->iy, o_ptr->ix, o_ptr->pval);
						}

						/* Destroy remaining spells */
						floor_item_increase(i,-(o_ptr->number));
					}

					floor_item_optimize(i);
				}
		
				/* Rods and activatible items charge */
				else if ((o_ptr->tval == TV_ROD) || (f3 & (TR3_ACTIVATE)))
				{
					/* Reset stack */
					if ((o_ptr->timeout) && !(o_ptr->stackc)) o_ptr->stackc = o_ptr->number -1;
					else if (o_ptr->timeout) o_ptr->stackc--;
					else o_ptr->stackc = 0;
		
				}

				/* Bodies/mimics become a monster */
				else
				{
					/* Notice count */
					j++;
				}
			}
		}

	}

	/*** Handle disease ***/
	if ((p_ptr->disease) && (p_ptr->disease & (DISEASE_QUICK)) && !(rand_int(100)))
	{
		suffer_disease();
	}


	/*** Involuntary Movement/Activations ***/

	/* Uncontrolled items */
	if ((p_ptr->uncontrolled) && (rand_int(100) < 1))
	{
		int j = 0, k = 0;
		
		/* Scan inventory and pick uncontrolled item */
		for (i = INVEN_WIELD; i < END_EQUIPMENT; i++)
		{
			u32b f1, f2, f3, f4;

			o_ptr = &inventory[i];
			
			/* Skip non-objects */
			if (!o_ptr->k_idx) continue;
			
			object_flags(o_ptr, &f1, &f2, &f3, &f4);
			
			/* Pick item */
			if (((f3 & (TR3_UNCONTROLLED)) != 0) && (cursed_p(o_ptr)) && !(rand_int(k++)))
			{
				j = i;
			}
		}
		
		/* Apply uncontrolled power */
		if (j)
		{
			bool dummy = FALSE;
			
			/* Get power */
			get_spell(&k, "", &inventory[j], FALSE);
			
			/* Process spell - involuntary effects */
			process_spell_eaten(SOURCE_OBJECT, inventory[j].k_idx, k, 25, &dummy);
		}

		/* Always notice */
		equip_can_flags(0x0L,0x0L,TR3_UNCONTROLLED,0x0L);
	}
	
	
	/* Mega-Hack -- Portal room */
	if ((room_has_flag(p_ptr->py, p_ptr->px, ROOM_PORTAL))
			&& ((p_ptr->cur_flags4 & (TR4_ANCHOR)) == 0) && (rand_int(100)<1))
	{
		/* Warn the player */
		msg_print("There is a brilliant flash of light.");

		/* Teleport player */
		teleport_player(40);
	}

	/* Delayed Word-of-Return */
	if (p_ptr->word_return)
	{
		/* Count down towards return */
		p_ptr->word_return--;

		/* Activate the return */
		if (!p_ptr->word_return)
		{
			/* Disturbing! */
			disturb(0, 0);

			msg_print("You feel yourself yanked sideways!");

			/* Teleport the player back to their original location */
			teleport_player_to(p_ptr->return_y, p_ptr->return_x);
			
			/* Clear the return coordinates */
			p_ptr->return_y = 0;
			p_ptr->return_x = 0;
		}
	}

	/* Delayed Word-of-Recall */
	if (p_ptr->word_recall)
	{
		/* Count down towards recall */
		p_ptr->word_recall--;

		/* Activate the recall */
		if (!p_ptr->word_recall)
		{
			/* Disturbing! */
			disturb(0, 0);

			/* Sound */
			sound(MSG_TPLEVEL);

			/* Determine the level */
			if (p_ptr->depth > min_depth(p_ptr->dungeon))
			{
				msg_print("You feel yourself yanked upwards!");

				/* New depth */
				p_ptr->depth = min_depth(p_ptr->dungeon);

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
			else if (min_depth(p_ptr->dungeon) < max_depth(p_ptr->dungeon))
			{
				msg_print("You feel yourself yanked downwards!");

				/* New depth */
				p_ptr->depth = p_ptr->max_depth;
				if (p_ptr->depth <= min_depth(p_ptr->dungeon)) p_ptr->depth = min_depth(p_ptr->dungeon)+1;
				if (p_ptr->depth > max_depth(p_ptr->dungeon)) p_ptr->depth = max_depth(p_ptr->dungeon);

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
			else 
			{
				msg_print("A tension leaves the air around you...");
			}
		}
	}	
	
	/* Update dynamic terrain */
	update_dyna();

#ifdef ALLOW_BORG
	if (count_stop)
	{
		/* The borg is always in perfect health */

		/* Restore stats */
		(void)res_stat(A_STR);
		(void)res_stat(A_INT);
		(void)res_stat(A_WIS);
		(void)res_stat(A_DEX);
		(void)res_stat(A_CON);
		(void)res_stat(A_CHR);
		(void)res_stat(A_AGI);
		(void)res_stat(A_SIZ);

		/* Restore experience. */
		p_ptr->exp = p_ptr->max_exp;

		/* No maladies */
		p_ptr->blind = 0;
		p_ptr->confused = 0;
		p_ptr->poisoned = 0;
		p_ptr->afraid = 0;
		p_ptr->paralyzed = 0;
		p_ptr->image = 0;
		p_ptr->slow = 0;
		p_ptr->stun = 0;
		p_ptr->paralyzed = 0;
		p_ptr->cut = 0;
		p_ptr->psleep = 0;
		p_ptr->msleep = 0;
		p_ptr->petrify = 0;
		p_ptr->stastis = 0;
		p_ptr->cursed = 0;
		p_ptr->amnesia = 0;
		p_ptr->disease = 0;

		/* Fully healed */
		p_ptr->chp = p_ptr->mhp;
		p_ptr->chp_frac = 0;

		/* No longer hungry */
		p_ptr->food = PY_FOOD_MAX - 1;

		/* No longer tired */
		p_ptr->rest = PY_REST_MAX - 1;
	}
#endif
}


/*
 * Verify use of "wizard" mode
 */
static bool enter_wizard_mode(void)
{
	/* Ask first time */
	if (verify_special || !(p_ptr->noscore & 0x0002))
	{
		/* Mention effects */
		msg_print("You are about to enter 'wizard' mode for the very first time!");
		msg_print("This is a form of cheating, and your game will not be scored!");
		msg_print(NULL);

		/* Verify request */
		if (!get_check("Are you sure you want to enter wizard mode? "))
		{
			return (FALSE);
		}
	}

	/* Mark savefile */
	p_ptr->noscore |= 0x0002;

	/* Success */
	return (TRUE);
}



#ifdef ALLOW_DEBUG

/*
 * Verify use of "debug" mode
 */
static bool verify_debug_mode(void)
{
	/* Ask first time */
	if (verify_special && !(p_ptr->noscore & 0x0008))
	{
		/* Mention effects */
		msg_print("You are about to use the dangerous, unsupported, debug commands!");
		msg_print("Your machine may crash, and your savefile may become corrupted!");
		msg_print(NULL);

		/* Verify request */
		if (!get_check("Are you sure you want to use the debug commands? "))
		{
			return (FALSE);
		}
	}

	/* Mark savefile */
	p_ptr->noscore |= 0x0008;

	/* Okay */
	return (TRUE);
}


/*
 * Hack -- Declare the Debug Routines
 */
extern void do_cmd_debug(void);

#endif



#ifdef ALLOW_BORG

/*
 * Verify use of "borg" mode
 */
static bool verify_borg_mode(void)
{
	/* Ask first time */
	if (verify_special && !(p_ptr->noscore & 0x0010))
	{
		/* Mention effects */
		msg_print("You are about to use the dangerous, unsupported, borg commands!");
		msg_print("Your machine may crash, and your savefile may become corrupted!");
		msg_print(NULL);

		/* Verify request */
		if (!get_check("Are you sure you want to use the borg commands? "))
		{
			return (FALSE);
		}
	}

	/* Mark savefile */
	p_ptr->noscore |= 0x0010;

	/* Okay */
	return (TRUE);
}


/*
 * Hack -- Declare the Borg Routines
 */
extern void do_cmd_borg(void);

#endif




/*
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 */
static void process_command(void)
{

#ifdef ALLOW_REPEAT

	/* Handle repeating the last command */
	repeat_check();

#endif /* ALLOW_REPEAT */

	/* Parse the command */
	switch (p_ptr->command_cmd)
	{
		/* Ignore */
		case ESCAPE:
		case ' ':
		{
			break;
		}


		/*** Cheating Commands ***/

		/* Toggle Wizard Mode */
		case KTRL('W'):
		{
			if (p_ptr->wizard)
			{
				p_ptr->wizard = FALSE;
				msg_print("Wizard mode off.");
			}
			else if (enter_wizard_mode())
			{
				p_ptr->wizard = TRUE;
				msg_print("Wizard mode on.");
			}

			/* Update monsters */
			p_ptr->update |= (PU_MONSTERS);

			/* Redraw "title" */
			p_ptr->redraw |= (PR_TITLE);

			break;
		}


#ifdef ALLOW_DEBUG

		/* Special "debug" commands */
		case KTRL('A'):
		{
			if (verify_debug_mode()) do_cmd_debug();
			break;
		}

#endif


#ifdef ALLOW_BORG

		/* Special "borg" commands */
		case KTRL('Z'):
		{
			if (verify_borg_mode()) do_cmd_borg();
			break;
		}

#endif



		/*** Inventory Commands ***/

		/* Wear/wield equipment */
		case 'w':
		{
			do_cmd_wield();
			break;
		}

		/* Take off equipment */
		case 't':
		{
			do_cmd_takeoff();
			break;
		}

		/* Drop an item */
		case 'd':
		{
			do_cmd_drop();
			break;
		}

		/* Destroy an item */
		case 'k':
		{
			do_cmd_destroy();
			break;
		}

		/* Equipment list */
		case 'e':
		{
			do_cmd_equip();
			break;
		}

		/* Inventory list */
		case 'i':
		{
			do_cmd_inven();
			break;
		}


		/*** Various commands ***/

		/* Identify an object */
		case 'I':
		{
			do_cmd_observe();
			break;
		}

		/* Hack -- toggle windows */
		case KTRL('E'):
		{
			toggle_inven_equip();
			break;
		}


		/*** Standard "Movement" Commands ***/

		/* Alter a grid */
		case '+':
		{
			do_cmd_alter();
			break;
		}

		/* Dig a tunnel */
		case 'T':
		{
			do_cmd_tunnel();
			break;
		}

		/* Walk */
		case ';':
		{
			do_cmd_walk();
			break;
		}

		/* Jump */
		case '-':
		{
			do_cmd_jump();
			break;
		}


		/*** Running, Resting, Searching, Staying */

		/* Begin Running -- Arg is Max Distance */
		case '.':
		{
			do_cmd_run();
			break;
		}

		/* Hold still */
		case ',':
		{
			do_cmd_hold();
			break;
		}

		/* Stay still */
		case 'g':
		{
			do_cmd_stay();
			break;
		}

		/* Rest -- Arg is time */
		case 'R':
		{
			do_cmd_rest();
			break;
		}

		/* Search for traps/doors */
		case 's':
		{
			do_cmd_search();
			break;
		}

		/* Toggle search mode */
		case 'S':
		{
			do_cmd_toggle_search();
			break;
		}


		/*** Stairs and Doors and Chests and Traps ***/

		/* Enter store */
		case '_':
		{
			do_cmd_store();
			break;
		}

		/* Go up staircase */
		case '<':
		{
			do_cmd_go_up();
			break;
		}

		/* Go down staircase */
		case '>':
		{
			do_cmd_go_down();
			break;
		}

		/* Open a door or chest */
		case 'o':
		{
			do_cmd_open();
			break;
		}

		/* Close a door */
		case 'c':
		{
			do_cmd_close();
			break;
		}

		/* Jam a door with spikes */
		case 'j':
		{
			do_cmd_set_trap_or_spike();
			break;
		}

		/* Bash a door */
		case 'B':
		{
			do_cmd_bash();
			break;
		}

		/* Disarm a trap or chest */
		case 'D':
		{
			do_cmd_disarm();
			break;
		}


		/*** Magic and Prayers ***/

		/* Gain new spells/prayers */
		case 'G':
		{
			do_cmd_study();
			break;
		}

		/* Browse a book */
		case 'b':
		{
			do_cmd_browse();
			break;
		}

		/* Cast a spell */
		case 'm':
		{
			do_cmd_cast();
			break;
		}

		/*** Use various objects ***/

		/* Inscribe an object */
		case '{':
		{
			do_cmd_inscribe();
			break;
		}

		/* Uninscribe an object */
		case '}':
		{
			do_cmd_uninscribe();
			break;
		}

		/* Activate an artifact */
		case 'A':
		{
			do_cmd_activate();
			break;
		}

		/* Eat some food */
		case 'E':
		{
			do_cmd_eat_food();
			break;
		}

		/* Fuel your lantern/torch */
		case 'F':
		{
			do_cmd_refill();
			break;
		}

		/* Fire an item */
		case 'f':
		{
			do_cmd_fire();
			break;
		}

		/* Throw an item */
		case 'v':
		{
			do_cmd_throw();
			break;
		}

		/* Aim a wand */
		case 'a':
		{
			do_cmd_aim_wand();
			break;
		}

		/* Zap a rod */
		case 'z':
		{
			do_cmd_zap_rod();
			break;
		}

		/* Quaff a potion */
		case 'q':
		{
			do_cmd_quaff_potion();
			break;
		}

		/* Read a scroll */
		case 'r':
		{
			do_cmd_read_scroll();
			break;
		}

		/* Use a staff */
		case 'u':
		{
			do_cmd_use_staff();
			break;
		}

		/* Apply a rune */
		case 'y':
		{
			do_cmd_apply_rune_or_coating();
			break;
		}

		/* Assemble a mechanism */
		case 'Y':
		{
			do_cmd_assemble();
			break;
		}

		/* Light or douse a light source */
		case '|':
		{
			do_cmd_light_and_douse();
			break;
		}

		/*** Looking at Things (nearby or on map) ***/

		/* Full dungeon map */
		case 'M':
		{
			do_cmd_view_map();
			break;
		}

		/* Locate player on map */
		case 'L':
		{
			do_cmd_locate();
			break;
		}

		/* Look around */
		case 'l':
		{
			do_cmd_look();
			break;
		}

		/* Target monster or location */
		case '*':
		{
			do_cmd_target();
			break;
		}



		/*** Help and Such ***/

		/* Help */
		case '\r':
		case '\n':
		{
			do_cmd_quick_help();
			break;
		}
		
		case '?':
		{
			do_cmd_help();
			break;
		}

		/* Identify symbol */
		case '/':
		{
			do_cmd_query_symbol();
			break;
		}

		/* Character description */
		case 'C':
		{
			do_cmd_change_name();
			break;
		}


		/*** System Commands ***/

		/* Hack -- User interface */
		case '!':
		{
			(void)Term_user(0);
			break;
		}

		/* Single line from a pref file */
		case '"':
		{
			do_cmd_pref();
			break;
		}

		/* Interact with macros */
		case '@':
		{
			do_cmd_macros();
			break;
		}

		/* Interact with visuals */
		case '%':
		{
			do_cmd_visuals();
			break;
		}

		/* Interact with colors */
		case '&':
		{
			do_cmd_colors();
			break;
		}

		/* Interact with options */
		case '=':
		{
			do_cmd_menu(MENU_OPTIONS, "options");
			do_cmd_redraw();
			break;
		}

		/*** Misc Commands ***/

		/* Take notes */
		case ':':
		{
			do_cmd_note();
			break;
		}

		/* Version info */
		case 'V':
		{
			do_cmd_version();
			break;
		}

		/* List quests */
		case KTRL('Q'):
		{
			do_cmd_quest();
			break;
		}

		/* Repeat level feeling */
		case KTRL('F'):
		{
			do_cmd_feeling();
			break;
		}

		/* Repeat time of day */
		case KTRL('T'):
		{
			do_cmd_timeofday();
			break;
		}

		/* Show previous message */
		case KTRL('O'):
		{
			do_cmd_message_one();
			break;
		}

		/* Show previous messages */
		case KTRL('P'):
		{
			do_cmd_messages();
			break;
		}

		/* Redraw the screen */
		case KTRL('R'):
		{
			do_cmd_redraw();
			break;
		}

#ifndef VERIFY_SAVEFILE

		/* Hack -- Save and don't quit */
		case KTRL('S'):
		{
			do_cmd_save_game();
			break;
		}

#endif

		/* Save and quit */
		case KTRL('X'):
		{
			/* Stop playing */
			p_ptr->playing = FALSE;

			/* Leaving */
			p_ptr->leaving = TRUE;

			break;
		}

		/* Quit (commit suicide) */
		case 'Q':
		{
			do_cmd_suicide();
			break;
		}

		/* Check knowledge */
		case '~':
		{
			do_cmd_menu(MENU_KNOWLEDGE, "knowledge");
			break;
		}

		/* Load "screen dump" */
		case '(':
		{
			do_cmd_load_screen();
			break;
		}

		/* Save "screen dump" */
		case ')':
		{
			do_cmd_save_screen();
			break;
		}

		/* Save "html screen dump" */
		case ']':
		{
			do_cmd_save_screen_html();
			break;
		}

		/* Mouse interaction */
		case '\xff':
		{
			int y = KEY_GRID_Y(p_ptr->command_cmd_ex);
			int x = KEY_GRID_X(p_ptr->command_cmd_ex);
			int room;
			
			/* Paranoia */
			if (!in_bounds_fully(y, x)) break;

			/* Hack -- we could try various things here like travelling or going up/down stairs */
			if ((p_ptr->py == y) && (p_ptr->px == x) && (p_ptr->command_cmd_ex.mousebutton))
			{
				do_cmd_rest();
			}
			else if (p_ptr->command_cmd_ex.mousebutton == BUTTON_MOVE)
			{
				if (p_ptr->confused)
				{
					do_cmd_walk();
				}
				else
				{
					do_cmd_pathfind(y, x);
				}
			}
			else if (p_ptr->command_cmd_ex.mousebutton == BUTTON_AIM)
			{
				target_set_location(y, x, 0);
				msg_print("Target set.");
			}
			else if (use_trackmouse && (easy_more || auto_more))
			{				
				target_set_interactive_aux(y, x, &room, TARGET_PEEK, (use_mouse ? "*,left-click to target, right-click to go to" : "*"));
			}
			break;
		}

		/* Hack -- Unknown command */
		default:
		{
			prt("Type '?' or press ENTER for help.", 0, 0);
			break;
		}
	}
}



/*
 * Hack -- helper function for "process_player()"
 *
 * Check for changes in the "monster memory"
 */
static void process_player_aux(void)
{
	static int old_monster_race_idx = 0;

	static u32b     old_flags1 = 0L;
	static u32b     old_flags2 = 0L;
	static u32b     old_flags3 = 0L;
	static u32b     old_flags4 = 0L;
	static u32b     old_flags5 = 0L;
	static u32b     old_flags6 = 0L;
	static u32b     old_flags7 = 0L;
	static u32b     old_flags8 = 0L;
	static u32b     old_flags9 = 0L;

	static byte	old_r_blows0 = 0;
	static byte	old_r_blows1 = 0;
	static byte	old_r_blows2 = 0;
	static byte	old_r_blows3 = 0;

	static byte     old_r_cast_innate = 0;
	static byte	old_r_cast_spell = 0;


	/* Tracking a monster */
	if (p_ptr->monster_race_idx)
	{
		/* Get the monster lore */
		monster_lore *l_ptr = &l_list[p_ptr->monster_race_idx];

		/* Check for change of any kind */
		if ((old_monster_race_idx != p_ptr->monster_race_idx) ||
		    (old_flags1 != l_ptr->flags1) ||
		    (old_flags2 != l_ptr->flags2) ||
		    (old_flags3 != l_ptr->flags3) ||
		    (old_flags4 != l_ptr->flags4) ||
		    (old_flags5 != l_ptr->flags5) ||
		    (old_flags6 != l_ptr->flags6) ||
		    (old_flags7 != l_ptr->flags7) ||
		    (old_flags8 != l_ptr->flags8) ||
		    (old_flags9 != l_ptr->flags9) ||
		    (old_r_blows0 != l_ptr->blows[0]) ||
		    (old_r_blows1 != l_ptr->blows[1]) ||
		    (old_r_blows2 != l_ptr->blows[2]) ||
		    (old_r_blows3 != l_ptr->blows[3]) ||
		    (old_r_cast_innate != l_ptr->cast_innate) ||
		    (old_r_cast_spell != l_ptr->cast_spell))
		{
			/* Memorize old race */
			old_monster_race_idx = p_ptr->monster_race_idx;

			/* Memorize flags */
			old_flags1 = l_ptr->flags1;
			old_flags2 = l_ptr->flags2;
			old_flags3 = l_ptr->flags3;
			old_flags4 = l_ptr->flags4;
			old_flags5 = l_ptr->flags5;
			old_flags6 = l_ptr->flags6;
			old_flags6 = l_ptr->flags7;
			old_flags6 = l_ptr->flags8;
			old_flags6 = l_ptr->flags9;

			/* Memorize blows */
			old_r_blows0 = l_ptr->blows[0];
			old_r_blows1 = l_ptr->blows[1];
			old_r_blows2 = l_ptr->blows[2];
			old_r_blows3 = l_ptr->blows[3];

			/* Memorize castings */
			old_r_cast_innate = l_ptr->cast_innate;
			old_r_cast_spell = l_ptr->cast_spell;

			/* Window stuff */
			p_ptr->window |= (PW_MONSTER);

			/* Window stuff */
			window_stuff();
		}
	}
}





/*
 * Process the player
 *
 * Notice the annoying code to handle "pack overflow", which
 * must come first just in case somebody manages to corrupt
 * the savefiles by clever use of menu commands or something.
 *
 * Notice the annoying code to handle "monster memory" changes,
 * which allows us to avoid having to update the window flags
 * every time we change any internal monster memory field, and
 * also reduces the number of times that the recall window must
 * be redrawn.
 *
 * Note that the code to check for user abort during repeated commands
 * and running and resting can be disabled entirely with an option, and
 * even if not disabled, it will only check during every 128th game turn
 * while resting, for efficiency.
 */
static void process_player(void)
{
	int i;


	/*** Check for interrupts ***/

	/* Complete resting */
	if (p_ptr->resting < 0)
	{
		/* Check to see if on damaging terrain */
		/* XXX This may cause a big slow down, but is needed for 'correctness' */

		/*Get the feature */
		feature_type *f_ptr = &f_info[cave_feat[p_ptr->py][p_ptr->px]];

		/* Use covered or bridged if necessary */
		if ((f_ptr->flags2 & (FF2_COVERED)) || (f_ptr->flags2 & (FF2_BRIDGED)))
		{
			f_ptr = &(f_info[f_ptr->mimic]);
		}

		/* Stop resting if would take damage */
		if (!(f_ptr->flags1 & (FF1_HIT_TRAP)) &&
		    ((f_ptr->blow.method) || (f_ptr->spell)))
		{

			disturb(0, 0);
		}

		/* Stop resting if tiring too fast */
		if ((p_ptr->tiring + 10) > PY_REST_RATE)
		{
			disturb(0, 0);
		}


		/* Basic resting */
		if (p_ptr->resting == -1)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) &&
			    (p_ptr->csp == p_ptr->msp) &&
			    (p_ptr->rest > PY_REST_FULL))
			{
				disturb(0, 0);
			}
		}

		/* Complete resting */
		else if (p_ptr->resting == -2)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) &&
			    (p_ptr->csp == p_ptr->msp) &&
			    !p_ptr->blind && !p_ptr->confused &&
			    !p_ptr->poisoned && !p_ptr->afraid &&
			    !p_ptr->stun && !p_ptr->cut &&
			    !p_ptr->slow && !p_ptr->paralyzed &&
			    !p_ptr->image && !p_ptr->word_recall &&
			    (p_ptr->rest > PY_REST_FULL))
			{
				disturb(0, 0);
			}
		}
	}

	/* Handle "abort" */
	if (!avoid_abort)
	{
		/* Check for "player abort" */
		if (p_ptr->running ||
		    p_ptr->command_rep ||
		    (p_ptr->resting && !(turn & 0x7F)))
		{
			/* Do not wait */
			inkey_scan = TRUE;

			/* Check for a key */
			if (anykey().key)
			{
				/* Flush input */
				flush();

				/* Disturb */
				disturb(0, 0);

				/* Hack -- Show a Message */
				msg_print("Cancelled.");
			}
		}
	}

	/*** Start searching ***/
	if ((easy_search) && (!p_ptr->searching) && (p_ptr->last_disturb < (turn-20)))
	{
		/* Start searching */
		p_ptr->searching = TRUE;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);
	}

	/*** Clear dodging ***/
	if (p_ptr->dodging)
	{
		/* Set charging -- reverse direction 180 degrees */
		p_ptr->charging = 10 - p_ptr->dodging;

		/* Clear dodging */
		p_ptr->dodging = 0;
	}

	/*** Clear blocking ***/
	if (p_ptr->blocking)
	{
		/* Reduce blocking */
		p_ptr->blocking--;
		
		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);
	}

	/*** Handle actual user input ***/

	/* Repeat until energy is reduced */
	do
	{
		/* Notice stuff (if needed) */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff (if needed) */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff (if needed) */
		if (p_ptr->redraw) redraw_stuff();

		/* Redraw stuff (if needed) */
		if (p_ptr->window) window_stuff();


		/* Place the cursor on the player */
		move_cursor_relative(p_ptr->py, p_ptr->px);

		/* Refresh (optional) */
		if (fresh_before) Term_fresh();

		/* Check for pack overflow */
		overflow_pack();

		/* Hack -- cancel "lurking browse mode" */
		if (!p_ptr->command_new.key) p_ptr->command_see = FALSE;


		/* Assume free turn */
		p_ptr->energy_use = 0;

#ifdef ALLOW_BORG
		/* Using the borg. */
		if (count_stop) do_cmd_borg();

		/* Paralyzed or Knocked Out */
		else 
#endif
		if ((p_ptr->paralyzed) || (p_ptr->stun >= 100) || (p_ptr->psleep >= PY_SLEEP_ASLEEP))
		{
			/* Get the feature */
			feature_type *f_ptr = &f_info[cave_feat[p_ptr->py][p_ptr->px]];

			/* Take a turn */
			p_ptr->energy_use = 100;

			/* Catch breath */
			if (!(f_ptr->flags2 & (FF2_FILLED)))
			{
				/* Rest the player */
				set_rest(p_ptr->rest + PY_REST_RATE - p_ptr->tiring);
			}
		}

		/* Resting */
		else if (p_ptr->resting)
		{
			/* Get the feature */
			feature_type *f_ptr = &f_info[cave_feat[p_ptr->py][p_ptr->px]];

			/* Timed rest */
			if (p_ptr->resting > 0)
			{
				/* Reduce rest count */
				p_ptr->resting--;

				/* Redraw the state */
				p_ptr->redraw |= (PR_STATE);
			}

			/* Catch breath */
			if (!(f_ptr->flags2 & (FF2_FILLED)))
			{
				/* Rest the player */
				set_rest(p_ptr->rest + PY_REST_RATE * 2 - p_ptr->tiring);
			}

			/* Take a turn */
			p_ptr->energy_use = 100;
		}

		/* Running */
		else if (p_ptr->running)
		{
			/* Take a step */
			run_step(0);
		}

		/* Repeated command */
		else if (p_ptr->command_rep)
		{
			/* Hack -- Assume messages were seen */
			msg_flag = FALSE;

			/* Clear the top line */
			prt("", 0, 0);

			/* Process the command */
			process_command();

			/* Count this execution */
			if (p_ptr->command_rep)
			{
				/* Count this execution */
				p_ptr->command_rep--;

				/* Redraw the state */
				p_ptr->redraw |= (PR_STATE);

				/* Redraw stuff */
				/* redraw_stuff(); */
			}
		}

		/* Normal command */
		else
		{
			/* Check monster recall */
			process_player_aux();

			/* Place the cursor on the player */
			move_cursor_relative(p_ptr->py, p_ptr->px);

			/* Get a command (normal) */
			request_command(FALSE);

			/* Process the command */
			process_command();
		}

		/*** Clean up ***/

		/* Action is or was resting */
		if (p_ptr->resting)
		{
			/* Increment the resting counter */
			p_ptr->resting_turn++;
		}

		/* Significant */
		if (p_ptr->energy_use)
		{
			/* Hack -- sing song */
			if (p_ptr->held_song)
			{
				/* Cast the spell */
			    if (do_cmd_cast_aux(p_ptr->held_song, spell_power(p_ptr->held_song), ((p_ptr->pstyle == WS_INSTRUMENT)?"play":"sing"), "song"))
					/* Hack -- if not aborted, always use a full turn */
					p_ptr->energy_use = 100;
			}

			/* Use some energy */
			p_ptr->energy -= p_ptr->energy_use;

			/* Increment the player turn counter */
			p_ptr->player_turn++;

			/* Hack -- constant hallucination */
			if (p_ptr->image) p_ptr->redraw |= (PR_MAP);

			/* Shimmer monsters if needed */
			if (!avoid_other && shimmer_monsters)
			{
				/* Clear the flag */
				shimmer_monsters = FALSE;

				/* Shimmer multi-hued monsters */
				for (i = 1; i < m_max; i++)
				{
					monster_type *m_ptr;
					monster_race *r_ptr;

					/* Get the monster */
					m_ptr = &m_list[i];

					/* Skip dead monsters */
					if (!m_ptr->r_idx) continue;

					/* Get the monster race */
					r_ptr = &r_info[m_ptr->r_idx];

					/* Skip non-multi-hued monsters */
					if (!(r_ptr->flags1 & (RF1_ATTR_MULTI)) && !(r_ptr->flags9 & (RF9_ATTR_METAL))) continue;

					/* Reset the flag */
					shimmer_monsters = TRUE;

					/* Redraw regardless */
					lite_spot(m_ptr->fy, m_ptr->fx);
				}
			}

			/* Repair "mark" flags */
			if (repair_mflag_mark)
			{
				/* Reset the flag */
				repair_mflag_mark = FALSE;

				/* Process the monsters */
				for (i = 1; i < m_max; i++)
				{
					monster_type *m_ptr;

					/* Get the monster */
					m_ptr = &m_list[i];

					/* Skip dead monsters */
					/* if (!m_ptr->r_idx) continue; */

					/* Repair "mark" flag */
					if (m_ptr->mflag & (MFLAG_MARK))
					{
						/* Skip "show" monsters */
						if (m_ptr->mflag & (MFLAG_SHOW))
						{
							/* Repair "mark" flag */
							repair_mflag_mark = TRUE;

							/* Skip */
							continue;
						}

						/* Forget flag */
						m_ptr->mflag &= ~(MFLAG_MARK);

						/* Update the monster */
						update_mon(i, FALSE);
					}
				}
			}
		}

		/* Repair "show" flags */
		if (repair_mflag_show)
		{
			/* Reset the flag */
			repair_mflag_show = FALSE;

			/* Process the monsters */
			for (i = 1; i < m_max; i++)
			{
				monster_type *m_ptr;

				/* Get the monster */
				m_ptr = &m_list[i];

				/* Skip dead monsters */
				/* if (!m_ptr->r_idx) continue; */

				/* Clear "show" flag */
				m_ptr->mflag &= ~(MFLAG_SHOW);
			}
		}
	}
	while (!p_ptr->energy_use && !p_ptr->leaving);

	/*** Clear charging ***/
	if (p_ptr->charging)
	{
		/* Set dodging */
		p_ptr->charging = 0;
	}

	/* Update noise flow information */
	update_noise();

	/* Update scent trail */
	update_smell();


	/* 
	 * Reset character vulnerability.  Will be calculated by 
	 * the first member of an animal pack that has a use for it.
	 */
	p_ptr->vulnerability = 0;

}



/*
 * Interact with the current dungeon level.
 *
 * This function will not exit until the level is completed,
 * the user dies, or the game is terminated.
 */
static void dungeon(void)
{
	/* Hack -- enforce illegal panel */
	p_ptr->wy = DUNGEON_HGT;
	p_ptr->wx = DUNGEON_WID;


	/* Not leaving */
	p_ptr->leaving = FALSE;

	/* Reset the "command" vars */
	p_ptr->command_cmd = 0;
	p_ptr->command_new.key = 0;
	p_ptr->command_rep = 0;
	p_ptr->command_arg = 0;
	p_ptr->command_dir = 0;


	/* Cancel the target */
	target_set_monster(0, 0);

	/* Cancel the health bar */
	health_track(0);


	/* Reset shimmer flags */
	shimmer_monsters = TRUE;
	shimmer_objects = TRUE;

	/* Reset repair flags */
	repair_mflag_show = TRUE;
	repair_mflag_mark = TRUE;


	/* Disturb */
	disturb(1, 0);


	/* Track maximum player level */
	if (p_ptr->max_lev < p_ptr->lev)
	{
		p_ptr->max_lev = p_ptr->lev;
	}


	/* Track maximum dungeon level */
	if (p_ptr->max_depth < p_ptr->depth)
	{
		p_ptr->max_depth = p_ptr->depth;
	}


	/* Choose panel */
	verify_panel();


	/* Flush messages */
	msg_print(NULL);


	/* Hack -- Increase "xtra" depth */
	character_xtra++;


	/* Clear */
	Term_clear();

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_RUNES);

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);

	/* Update stuff */
	update_stuff();

	/* Fully update the visuals (and monster distances) */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_DISTANCE);

	/* Redraw dungeon */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP | PR_ITEM_LIST);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER | PW_MONLIST);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_MAP);

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Redraw stuff */
	window_stuff();

	/* Hack -- Decrease "xtra" depth */
	character_xtra--;

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_ROOM_INFO);

	/* Combine / Reorder the pack */
/*	p_ptr->notice |= (PN_COMBINE | PN_REORDER);*/

	/* Window stuff */
/*	p_ptr->window |= (PW_ROOM_INFO);*/

	/* Notice stuff */
	notice_stuff();

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Window stuff */
	window_stuff();

	/* Refresh */
	Term_fresh();


	/* Handle delayed death */
	if (p_ptr->is_dead) return;

	/* Announce (or repeat) the feeling */
	if (p_ptr->depth>min_depth(p_ptr->dungeon)) do_cmd_feeling();

	/*** Process this dungeon level ***/

	/* Reset the monster generation level */
	monster_level = p_ptr->depth;

	/* Reset the object generation level */
	object_level = p_ptr->depth;

	/* Beginners get a tip */
	if (birth_beginner) show_tip();
	
	/* Main loop */
	while (TRUE)
	{
		int i;
		
		/* Hack -- Compact the monster list occasionally */
		if (m_cnt + 32 > z_info->m_max) compact_monsters(64);

		/* Hack -- Compress the monster list occasionally */
		if (m_cnt + 32 < m_max) compact_monsters(0);


		/* Hack -- Compact the object list occasionally */
		if (o_cnt + 32 > z_info->o_max) compact_objects(64);

		/* Hack -- Compress the object list occasionally */
		if (o_cnt + 32 < o_max) compact_objects(0);

		/*** Verify the object list ***/
		/*
		 * This is required until we identify the source of object corruption.
		 */
		for (i = 1; i < z_info->o_max; i++)
		{
			/* Check for straightforward corruption */
			if (o_list[i].next_o_idx == i)
			{
				msg_format("Object %d (%s) corrupted. Fixing.", i, k_name + k_info[o_list[i].k_idx].name);
				o_list[i].next_o_idx = 0;
				
				if (o_list[i].held_m_idx)
				{
					msg_format("Was held by %d.", o_list[i].held_m_idx);
				}
				else
				{
					msg_format("Was held at (%d, %d).", o_list[i].iy, o_list[i].ix);					
				}
			}
		}

		/*** Apply energy ***/

		/* Give the player some energy */
		p_ptr->energy += extract_energy[p_ptr->pspeed];

		/* Can the player move? */
		while ((p_ptr->energy >= 100) && !p_ptr->leaving)
		{

			/* process monster with even more energy first */
			process_monsters((byte)(p_ptr->energy + 1));

			/* if still alive */
			if (!p_ptr->leaving)
			{
				/* Process the player */
				process_player();
			}
		}

		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Redraw stuff */
		if (p_ptr->window) window_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->py, p_ptr->px);

		/* Optional fresh */
		if (fresh_after) Term_fresh();

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Process monsters */
		process_monsters(0);

		/* Reset Monsters */
		reset_monsters();

		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Redraw stuff */
		if (p_ptr->window) window_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->py, p_ptr->px);

		/* Optional fresh */
		if (fresh_after) Term_fresh();

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Process the world */
		process_world();



		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Window stuff */
		if (p_ptr->window) window_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->py, p_ptr->px);

		/* Optional fresh */
		if (fresh_after) Term_fresh();

		/* Handle "leaving" */
		if (p_ptr->leaving) break;


		/* Count game turns */
		turn++;
	}
}



/*
 * Process some user pref files
 *
 * Hack -- Allow players on UNIX systems to keep a ".angband.prf" user
 * pref file in their home directory.  Perhaps it should be loaded with
 * the "basic" user pref files instead of here.  This may allow bypassing
 * of some of the "security" compilation options.  XXX XXX XXX XXX XXX
 */
static void process_some_user_pref_files(void)
{
	char buf[1024];

#ifdef ALLOW_PREF_IN_HOME
#ifdef SET_UID

	char *homedir;

#endif /* SET_UID */
#endif /* ALLOW_PREF_IN_HOME */

	/* Process the "user.prf" file */
	(void)process_pref_file("user.prf");

	/* Get the "PLAYER.prf" filename */
	sprintf(buf, "%s.prf", op_ptr->base_name);

	/* Process the "PLAYER.prf" file */
	(void)process_pref_file(buf);

#ifdef ALLOW_PREF_IN_HOME
#ifdef SET_UID

	/* Process the "~/.angband.prf" file */
	if ((homedir = getenv("HOME")))
	{
		/* Get the ".angband.prf" filename */
		path_build(buf, 1024, homedir, ".angband.prf");

		/* Process the ".angband.prf" file */
		(void)process_pref_file(buf);
	}

#endif /* SET_UID */
#endif /* ALLOW_PREF_IN_HOME */
}


/*
 * Actually play a game.
 *
 * This function is called from a variety of entry points, since both
 * the standard "main.c" file, as well as several platform-specific
 * "main-xxx.c" files, call this function to start a new game with a
 * new savefile, start a new game with an existing savefile, or resume
 * a saved game with an existing savefile.
 *
 * If the "new_game" parameter is true, and the savefile contains a
 * living character, then that character will be killed, so that the
 * player may start a new game with that savefile.  This is only used
 * by the "-n" option in "main.c".
 *
 * If the savefile does not exist, cannot be loaded, or contains a dead
 * (non-wizard-mode) character, then a new game will be started.
 *
 * Several platforms (Windows, Macintosh, Amiga) start brand new games
 * with "savefile" and "op_ptr->base_name" both empty, and initialize
 * them later based on the player name.  To prevent weirdness, we must
 * initialize "op_ptr->base_name" to "PLAYER" if it is empty.
 *
 * Note that we load the RNG state from savefiles (2.8.0 or later) and
 * so we only initialize it if we were unable to load it.  The loading
 * code marks successful loading of the RNG state using the "Rand_quick"
 * flag, which is a hack, but which optimizes loading of savefiles.
 */
void play_game(bool new_game)
{
	/* Hack -- Increase "icky" depth */
	character_icky++;

	/* Verify main term */
	if (!angband_term[0])
	{
		quit("main window does not exist");
	}

	/* Make sure main term is active */
	Term_activate(angband_term[0]);

	/* Verify minimum size */
	if ((Term->hgt < 24) || (Term->wid < 80))
	{
		quit("main window is too small");
	}

	/* Hack -- Turn off the cursor */
	(void)Term_set_cursor(0);

	/* Attempt to load */
	if (!load_player())
	{
		/* Oops */
		quit("broken savefile");
	}

	/* Nothing loaded */
	if (!character_loaded)
	{
		/* Make new player */
		new_game = TRUE;

		/* The dungeon is not ready */
		character_dungeon = FALSE;
	}

	/* Hack -- Default base_name */
	if (!op_ptr->base_name[0])
	{
		my_strcpy(op_ptr->base_name, "PLAYER", sizeof(op_ptr->base_name));
	}

	/* Init RNG */
	if (Rand_quick)
	{
		u32b seed;

		/* Basic seed */
		seed = (time(NULL));

#ifdef SET_UID

		/* Mutate the seed on Unix machines */
		seed = ((seed >> 3) * (getpid() << 1));

#endif

		/* Use the complex RNG */
		Rand_quick = FALSE;

		/* Seed the "complex" RNG */
		Rand_state_init(seed);
	}

	/* Roll new character */
	if (new_game)
	{
		/* The dungeon is not ready */
		character_dungeon = FALSE;

		/* Start in town */
		p_ptr->depth = 0;

		/* Hack -- seed for flavors */
		seed_flavor = rand_int(0x10000000);

		/* Hack -- seed for town layout */
		seed_town = rand_int(0x10000000);

		/* Hack -- seed for random artifacts */
		if (reseed_artifacts) seed_randart = rand_int(0x10000000);

		/* Hack -- clear artifact memory */
		if (reseed_artifacts)
		{
			int i;

			for (i = 0;i<z_info->a_max;i++)
			{
				object_info *n_ptr = &a_list[i];

				n_ptr->can_flags1 = 0x0L;
				n_ptr->can_flags2 = 0x0L;
				n_ptr->can_flags3 = 0x0L;
				n_ptr->can_flags4 = 0x0L;

				n_ptr->not_flags1 = 0x0L;
				n_ptr->not_flags2 = 0x0L;
				n_ptr->not_flags3 = 0x0L;
				n_ptr->not_flags4 = 0x0L;
			}

		}

		/* Roll up a new character */
		player_birth();

		/* Randomize the artifact names */
		do_randart(seed_randart, TRUE);

		/* Hack -- enter the world */
		turn = 1;
		p_ptr->player_turn = 0;
		p_ptr->resting_turn = 0;
	}

	/* Normal machine (process player name) */
	if (savefile[0])
	{
		process_player_name(FALSE);
	}

	/* Weird machine (process player name, pick savefile name) */
	else
	{
		process_player_name(TRUE);
	}

	/* Flash a message */
	prt("Please wait...", 0, 0);

	/* Flush the message */
	Term_fresh();


	/* Hack -- Enter wizard mode */
	if (arg_wizard && enter_wizard_mode()) p_ptr->wizard = TRUE;


	/* Flavor the objects */
	flavor_init();

	/* Mark the fixed monsters as quests */
	if (adult_campaign)
	{
		int i;

		for (i = 0; i < z_info->t_max; i++)
		{
			int guard, ii;
			
			guard = t_info[i].quest_monster;

			/* Mark map quest monsters as 'unique guardians'. This allows
			 * the map quests to be completed successfully.
			 */
			if (guard)
			{
				r_info[guard].flags1 |= (RF1_GUARDIAN | RF1_UNIQUE);
				if (r_info[guard].max_num > 1) r_info[guard].max_num = 1;
			}
			
			/* However, we must ensure that town lockup monsters are
			 * unique to allow the town to be unlocked later.
			 */
			guard = t_info[i].town_lockup_monster;
			if (guard)
			{
				r_info[guard].flags1 |= (RF1_UNIQUE);
				if (r_info[guard].max_num > 1) r_info[guard].max_num = 1;
			}
			
			for (ii = 0; ii < MAX_DUNGEON_ZONES; ii++)
			{
				/*
				 * And we ensure dungeon mini-bosses are marked as
				 * guardians, so that they do not appear elsewhere
				 */
				guard = t_info[i].zone[ii].guard;
				if (guard)
				{
					r_info[guard].flags1 |= (RF1_GUARDIAN);
				}
			}

			/*
			 * And this includes 'replacement' mini-bosses.
			 */
			guard = t_info[i].replace_guardian;
			if (guard)
			{
				r_info[guard].flags1 |= (RF1_GUARDIAN);
			}
		}
	}

	/* Reset visuals */
	reset_visuals(TRUE);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1 | PW_PLAYER_2 | PW_PLAYER_3);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER | PW_MESSAGE);

	/* Window stuff */
	window_stuff();

	/* Process some user pref files */
	process_some_user_pref_files();

	/* Set or clear "rogue_like_commands" if requested */
	if (arg_force_original) rogue_like_commands = FALSE;
	if (arg_force_roguelike) rogue_like_commands = TRUE;


	/* React to changes */
	Term_xtra(TERM_XTRA_REACT, 0);


	/* Generate a dungeon level if needed */
	if (!character_dungeon) generate_cave();

	/* Character is now "complete" */
	character_generated = TRUE;

	/* Hack -- Decrease "icky" depth */
	character_icky--;

	/* Start playing */
	p_ptr->playing = TRUE;

	/* Hack -- Enforce "delayed death" */
	if (p_ptr->chp < 0) p_ptr->is_dead = TRUE;

	/* Process */
	while (TRUE)
	{

		/* Process the level */
		dungeon();

		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Window stuff */
		if (p_ptr->window) window_stuff();


		/* Cancel the target */
		target_set_monster(0, 0);

		/* Cancel the health bar */
		health_track(0);


		/* Forget the view */
		forget_view();


		/* Handle "quit and save" */
		if (!p_ptr->playing && !p_ptr->is_dead) break;


		/* Erase the old cave */
		wipe_o_list();
		wipe_m_list();


		/* XXX XXX XXX */
		msg_print(NULL);

		/* Accidental Death */
		if (p_ptr->playing && p_ptr->is_dead)
		{
			/* Mega-Hack -- Allow player to cheat death */
			if ((p_ptr->wizard || cheat_live) && !get_check("Die? "))
			{
				/* Mark social class, reset age, if needed */
				if (p_ptr->sc) p_ptr->sc = p_ptr->age = 0;

				/* Increase age */
				p_ptr->age++;

				/* Mark savefile */
				p_ptr->noscore |= 0x0001;

				/* Message */
				msg_print("You invoke wizard mode and cheat death.");
				msg_print(NULL);

				/* Cheat death */
				p_ptr->is_dead = FALSE;

				/* Restore hit points */
				p_ptr->chp = p_ptr->mhp;
				p_ptr->chp_frac = 0;

				/* Restore spell points */
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;

				/* Hack -- Healing */
				(void)set_blind(0);
				(void)set_confused(0);
				(void)set_poisoned(0);
				(void)set_afraid(0);
				(void)set_paralyzed(0);
				(void)set_amnesia(0);
				(void)set_cursed(0);
				(void)set_image(0);
				(void)set_stun(0);
				(void)set_cut(0);

				(void)set_msleep(0);
				(void)set_psleep(0);

				/* Hack -- Prevent starvation */
				(void)set_food(PY_FOOD_MAX - 1);

				/* Hack -- cancel recall */
				if (p_ptr->word_recall)
				{
					/* Message */
					msg_print("A tension leaves the air around you...");
					msg_print(NULL);

					/* Hack -- Prevent recall */
					p_ptr->word_recall = 0;
				}

				/* Note cause of death XXX XXX XXX */
				my_strcpy(p_ptr->died_from, "Cheating death", sizeof(p_ptr->died_from));

				/* New depth */
				p_ptr->depth = min_depth(p_ptr->dungeon);

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}

		/* Handle "death" */
		if (p_ptr->is_dead) break;

		/* Make a new level */
		generate_cave();
	}

	/* Close stuff */
	close_game();

	/* Quit */
	quit(NULL);
}
