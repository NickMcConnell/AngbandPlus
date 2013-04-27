/* File: dungeon.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "cmds.h"
#include "game-event.h"
#include "z-quark.h"
#include "option.h"
#include "raceflag.h"
#include "store.h"
#include "tvalsval.h"

/**
 * There is a 1/MAX_M_ALLOC_CHANCE chance per round of creating a new monster
 */
#define MAX_M_ALLOC_CHANCE	160

/*
 * Player regeneration constants
 */
#define PY_REGEN_NORMAL		197		/**< Regen factor*2^16 when full */
#define PY_REGEN_WEAK		98		/**< Regen factor*2^16 when weak */
#define PY_REGEN_FAINT		33		/**< Regen factor*2^16 when fainting */
#define PY_REGEN_HPBASE		1442	/**< Min amount hp regen*2^16 */
#define PY_REGEN_MNBASE		524		/**< Min amount mana regen*2^16 */

/*
 * Store constants.
 */
#define STORE_TURNS		1000	/**< Number of turns between turnovers */

/**
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
static int value_check_aux1(const object_type *o_ptr)
{
	/* Artifacts */
	if (o_ptr->is_artifact())
	{
		/* Cursed/Broken */
		if (o_ptr->is_broken_or_cursed()) return (INSCRIP_TERRIBLE);

		/* Normal */
		return (INSCRIP_SPECIAL);
	}

	/* Ego-Items */
	if (o_ptr->is_ego_item())
	{
		/* Cursed/Broken */
		if (o_ptr->is_broken_or_cursed()) return (INSCRIP_WORTHLESS);

		/* Normal */
		return (INSCRIP_EXCELLENT);
	}

	/* Cursed items */
	if (o_ptr->is_cursed()) return (INSCRIP_CURSED);

	/* Broken items */
	if (o_ptr->is_broken()) return (INSCRIP_BROKEN);

	/* Good "armor" bonus */
	if (o_ptr->to_a > 0) return (INSCRIP_GOOD);

	/* Good "weapon" bonus */
	if (o_ptr->to_h + o_ptr->to_d > 0) return (INSCRIP_GOOD);

	/* Default to "average" */
	return (INSCRIP_AVERAGE);
}


/**
 * Return a "feeling" (or NULL) about an item.  Method 2 (Light).
 */
static int value_check_aux2(const object_type *o_ptr)
{
	/* Cursed items (all of them) */
	if (o_ptr->is_cursed()) return (INSCRIP_CURSED);

	/* Broken items (all of them) */
	if (o_ptr->is_broken()) return (INSCRIP_BROKEN);

	/* Artifacts -- except cursed/broken ones */
	if (o_ptr->is_artifact()) return (INSCRIP_GOOD);

	/* Ego-Items -- except cursed/broken ones */
	if (o_ptr->is_ego_item()) return (INSCRIP_GOOD);

	/* Good armor bonus */
	if (o_ptr->to_a > 0) return (INSCRIP_GOOD);

	/* Good weapon bonuses */
	if (o_ptr->to_h + o_ptr->to_d > 0) return (INSCRIP_GOOD);

	/* No feeling */
	return (0);
}

static bool sensable(object_type* o_ptr)
{
	/* Skip empty slots */
	if (!o_ptr->k_idx) return FALSE;

	/* Valid "tval" codes */
	switch (o_ptr->obj_id.tval)
	{
		case TV_SHOT:		/* intentional fallthrough */
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
		case TV_DRAG_ARMOR:	break;
		default:			return FALSE;
	}

	/* It already has psuedoid */
	if ((o_ptr->pseudo > 0) &&
	    (o_ptr->pseudo != INSCRIP_INDESTRUCTIBLE)) return FALSE;

	/* It has already been sensed, do not sense it again */
	if (o_ptr->ident & (IDENT_SENSE)) return FALSE;

	/* It is known, no information needed */
	if (o_ptr->known()) return FALSE;

	return TRUE;
}

/** returns whether feel was INSCRIP_AVERAGE */
static bool full_sense(object_type* o_ptr,int feel)
{
	bool identified_average = FALSE;
	size_t i = o_ptr-p_ptr->inventory;
	char o_name[80];

	/* Stop everything */
	disturb(0, 0);

	/* Get an object description */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, ODESC_FULL);

	/* Message (equipment) */
	sound(MSG_PSEUDOID);

	if (i >= INVEN_WIELD)
	{
		msg_format("You feel the %s (%c) you are %s %s %s...",
		           o_name, index_to_label(i), describe_use(i),
		           ((o_ptr->number == 1) ? "is" : "are"),
		           inscrip_text[feel - INSCRIP_NULL]);
	}

	/* Message (inventory) */
	else
	{
		msg_format("You feel the %s (%c) in your pack %s %s...",
		           o_name, index_to_label(i),
		           ((o_ptr->number == 1) ? "is" : "are"),
		           inscrip_text[feel - INSCRIP_NULL]);
	}

		
	if (feel==INSCRIP_AVERAGE)
	{	/* average is pretty much fully known anyway.  Give a free identify. */
		identified_average = TRUE;

		object_aware(o_ptr);
		object_known(o_ptr);
	}
	else
	{	/* Sense the object */
		o_ptr->sense(feel);		
	}

	return identified_average;
}

/**
 * Sense the inventory
 */
static void sense_inventory(void)
{
	int i;
	int plev = p_ptr->lev;
	int feel;
	bool noticed_anything = FALSE;
	bool identified_average = FALSE;

	/*** Check for "sensing" ***/

	/* No sensing when confused */
	if (p_ptr->timed[TMD_CONFUSED]) return;

	if (p_ptr->cp_ptr->flags & CF_PSEUDO_ID_IMPROV)
	{
		if (!one_in_(p_ptr->cp_ptr->sense_base / (plev * plev + p_ptr->cp_ptr->sense_div)))
			return;
	}
	else
	{
		if (!one_in_(p_ptr->cp_ptr->sense_base / (plev + p_ptr->cp_ptr->sense_div)))
			return;
	}


	/*** Sense everything ***/

	/* Check everything */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		bool heavy = (p_ptr->cp_ptr->flags & CF_PSEUDO_ID_HEAVY);
		object_type* o_ptr = &p_ptr->inventory[i];

		if (!sensable(o_ptr)) continue;

		/* Occasional failure on inventory items */
		if ((i < INVEN_WIELD) && !one_in_(5)) continue;

		/* Indestructible objects are either excellent or terrible */
		if (o_ptr->pseudo == INSCRIP_INDESTRUCTIBLE)
			heavy = TRUE;

		/* Check for a feeling */
		feel = (heavy ? value_check_aux1(o_ptr) : value_check_aux2(o_ptr));

		/* Skip non-feelings */
		if (!feel) continue;

		noticed_anything = TRUE;		
		identified_average = full_sense(o_ptr,feel);
	}

	if (noticed_anything)
	{
		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

		/* if player has weak psuedo-id, do a second pass to catch wielded average items */
		/* start at INVEN_WIELD nonstrict lower bound because of the 1-in-5 failure on inventory items */
		if (!p_ptr->cp_ptr->flags & CF_PSEUDO_ID_IMPROV)
		{
			for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
			{
				object_type* o_ptr = &p_ptr->inventory[i];

				if (!sensable(o_ptr)) continue;

				/* Check for a feeling */
				feel = value_check_aux1(o_ptr);

				/* Skip non-feelings */
				if (!feel) continue;

				identified_average = full_sense(o_ptr,feel);
			}
		}

		if (identified_average)
		{
			/* Recalculate bonuses */
			p_ptr->update |= (PU_BONUS);

			/* Combine / Reorder the pack (later) */
/*			p_ptr->notice |= (PN_COMBINE | PN_REORDER);	*/

			/* Window stuff */
			p_ptr->redraw |= (PR_INVEN | PR_EQUIP);
		}
	}
}



/**
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
	if ((p_ptr->chp < 0) && (old_chp > 0)) p_ptr->chp = MAX_S16B;
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
	}
}


/**
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
		p_ptr->csp = MAX_S16B;
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
		/* Redraw */
		p_ptr->redraw |= (PR_MANA);
	}
}



static bool regenerate_monster(monster_type& m)
{
	/* Allow regeneration (if needed) */
	if (m.chp < m.mhp)
	{
		monster_race *r_ptr = m.race();

		/* Hack -- Base regeneration */
		int frac = m.mhp / 100;

		/* Hack -- Minimal regeneration rate */
		if (!frac) frac = 1;

		/* Hack -- Some monsters regenerate quickly */
		if (r_ptr->flags[1] & RF1_REGENERATE) frac *= 2;

		/* Hack -- Regenerate */
		if (m.mhp-m.chp>=frac) 
			m.chp += frac;
		else
			m.chp = m.mhp;

		/* Redraw (later) if needed */
		if (p_ptr->health_who == (&m-mon_list)) p_ptr->redraw |= (PR_HEALTH);
	};
	return false;
}

static bool monster_condition_timeout(monster_type& m)
{
	/* Handle "stun" */
	if (m.stunned)
	{
		int d = 1;

		/* Make a "saving throw" against stun */
		if (rand_int(5000) <= m.race()->level * m.race()->level)
		{
			/* Recover fully */
			d = m.stunned;
		}

		/* Hack -- Recover from stun */
		if (m.stunned > d)
		{
			/* Recover somewhat */
			m.stunned -= d;
		}

		/* Fully recover */
		else
		{
			/* Recover fully */
			m.stunned = 0;

			/* Message if visible */
			if (m.ml)
			{
				char m_name[80];

				/* Get the monster name */
				monster_desc(m_name, sizeof(m_name), &m, 0);

				/* Dump a message */
				msg_format("%^s is no longer stunned.", m_name);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == &m-mon_list) p_ptr->redraw |= (PR_HEALTH);
			}
		}

		/* Still stunned */
		if (m.stunned) return false;
	}


	/* Handle confusion */
	if (m.confused)
	{
		int d = randint(m.race()->level / 10 + 1);

		/* Still confused */
		if (m.confused > d)
		{
			/* Reduce the confusion */
			m.confused -= d;
		}

		/* Recovered */
		else
		{
			/* No longer confused */
			m.confused = 0;

			/* Message if visible */
			if (m.ml)
			{
				char m_name[80];

				/* Get the monster name */
				monster_desc(m_name, sizeof(m_name), &m, 0);

				/* Dump a message */
				msg_format("%^s is no longer confused.", m_name);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == &m-mon_list) p_ptr->redraw |= (PR_HEALTH);
			}
		}
	}


	/* Handle "fear" */
	if (m.monfear)
	{
		/* Amount of "boldness" */
		int d = randint(m.race()->level / 10 + 1);

		/* Still afraid */
		if (m.monfear > d)
		{
			/* Reduce the fear */
			m.monfear -= d;
		}

		/* Recover from fear, take note if seen */
		else
		{
			/* No longer afraid */
			m.monfear = 0;

			/* Visual note */
			if (m.ml)
			{
				char m_name[80];
				char m_poss[80];

				/* Get the monster name/poss */
				monster_desc(m_name, sizeof(m_name), &m, 0);
				monster_desc(m_poss, sizeof(m_poss), &m, 0x22);

				/* Dump a message */
				msg_format("%^s recovers %s courage.", m_name, m_poss);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == &m-mon_list) p_ptr->redraw |= (PR_HEALTH);
			}
		}
	}
	return false;
}

/**
 * Regenerate the monsters (once per 100 game turns)
 *
 * XXX XXX XXX Should probably be done during monster turns.
 */
static void regen_monsters(void)
{
	monster_scan(regenerate_monster);
}


/**
 * If player has inscribed the object with "!!", let him know when it's
 * recharged. -LM-
 * Also inform player when first item of a stack has recharged. -HK-
 */
static void recharged_notice(const object_type *o_ptr, bool all)
{
	char o_name[120];

	if (check_for_inscrip(o_ptr,"!!"))
		{
		/* Describe (briefly) */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, ODESC_BASE);

		/* Disturb the player */
		disturb(0, 0);

		/* Notify the player */
		if (o_ptr->number > 1)
		{
			if (all) msg_format("Your %s have recharged.", o_name);
			else msg_format("One of your %s has recharged.", o_name);
		}

		/* Artifacts */
		else if (o_ptr->name1)
		{
			msg_format("The %s has recharged.", o_name);
		}

		/* Single, non-artifact items */
		else msg_format("Your %s has recharged.", o_name);
		}
}

static bool recharge_rod_on_ground(object_type& o)
{
	if ((o.obj_id.tval == TV_ROD) && (o.timeout))
	{
		if (o.timeout<o.number)
			o.timeout = 0;
		else
			o.timeout -= o.number;
	}
	return false;
}

/**
 * Recharge activatable objects in the player's equipment
 * and rods in the inventory and on the ground.
 */
static void recharge_objects(void)
{
	int i;

	int charged = 0;

	assert(0 <= p_ptr->inven_cnt && INVEN_PACK >= p_ptr->inven_cnt && "precondition");
	assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");

	/* Process equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Get the object */
		object_type* const o_ptr = &p_ptr->inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Recharge activatable objects */
		if (o_ptr->timeout > 0)
		{
			/* Recharge */
			o_ptr->timeout--;

			/* Notice changes */
			if (!(o_ptr->timeout))
			{
				++charged;

				/* Message if item is recharged, if inscribed with "!!" */
				recharged_notice(o_ptr, TRUE);
			}
		}
	}

	/* Notice changes */
	if (charged) p_ptr->redraw |= (PR_EQUIP);

	charged = 0;

	/* Recharge rods */
	for (i = 0; i < p_ptr->inven_cnt; i++)
	{
		object_type* const o_ptr = &p_ptr->inventory[i];
		const object_kind* const k_ptr = &object_type::k_info[o_ptr->k_idx];

		/* Examine all charging rods */
		if ((o_ptr->obj_id.tval == TV_ROD) && (o_ptr->timeout))
		{
			/* Determine how many rods are charging */
			int temp = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;

			if (temp > o_ptr->number) temp = o_ptr->number;

			/* Decrease timeout by that number */
			o_ptr->timeout -= temp;

			/* Boundary control */
			if (o_ptr->timeout < 0) o_ptr->timeout = 0;

			/* Update if any rods are recharged */
			if (temp > (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval)
			{
				/* Update window */
				++charged;

				/* Message if whole stack is recharged, if inscribed with "!!" */
				if (!(o_ptr->timeout)) recharged_notice(o_ptr, TRUE);
				/* Message if first in a stack is recharged, if inscribed with "!!" -HK- */
				else if (temp == o_ptr->number) recharged_notice(o_ptr, FALSE);
			}
		}
	}

	/* Notice changes */
	if (charged)
	{
		/* Combine pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->redraw |= (PR_INVEN);
	}


	/*** Process Objects ***/
	object_scan(recharge_rod_on_ground);
}


static void play_ambient_sound(void)
{
	/* Town sound */
	if (p_ptr->depth == 0) 
	{
		/* Hack - is it daytime or nighttime? */
		if (turn % (10L * TOWN_DAWN) < TOWN_DAWN / 2)
		{
			/* It's day. */
			sound(MSG_AMBIENT_DAY);
		} 
		else 
		{
			/* It's night. */
			sound(MSG_AMBIENT_NITE);
		}
		
	}

	/* Dungeon level 1-20 */
	else if (p_ptr->depth <= 20) 
	{
		sound(MSG_AMBIENT_DNG1);
	}

	/* Dungeon level 21-40 */
	else if (p_ptr->depth <= 40) 
	{
		sound(MSG_AMBIENT_DNG2);
	}

	/* Dungeon level 41-60 */
	else if (p_ptr->depth <= 60) 
	{
		sound(MSG_AMBIENT_DNG3);
	}

	/* Dungeon level 61-80 */
	else if (p_ptr->depth <= 80) 
	{
		sound(MSG_AMBIENT_DNG4);
	}

	/* Dungeon level 80- */
	else  
	{
		sound(MSG_AMBIENT_DNG5);
	}
}


/**
 * Handle certain things once every 10 game turns
 */
static void process_world(void)
{
	int i;

	int regen_amount;

	object_type* o_ptr;

	/* Every 10 game turns */
	if (turn % 10) return;


	/*** Check the Time and Load ***/

	if (!(turn % 1000))
	{
		/* Check time and load */
		if (0 != check_time())
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

	/* Play an ambient sound at regular intervals. */
	if (!(turn % ((10L * TOWN_DAWN) / 4)))
	{
		play_ambient_sound();
	}

	/*** Handle the "town" (stores and sunshine) ***/

	/* While in town */
	if (!p_ptr->depth)
	{
		/* Hack -- Daybreak/Nighfall in town */
		if (!(turn % ((10L * TOWN_DAWN) / 2)))
		{
			bool dawn = (!(turn % (10L * TOWN_DAWN)));	/* Check for dawn */

			msg_print(dawn	? "The sun has risen."		/* Day breaks */
							: "The sun has fallen.");	/* Night falls */
			
			/* Illuminate */
			town_illuminate(dawn);
		}
	}


	/* While in the dungeon */
	else
	{
		/*** Update the Stores ***/

		/* Update the stores once a day (while in dungeon) */
		if (!(turn % (10L * STORE_TURNS)))
		{
			int n;

			/* Message */
			if (OPTION(cheat_xtra)) msg_print("Updating Shops...");

			/* Maintain each shop (except home) */
			ZAIBAND_STATIC_ASSERT(STORE_HOME+1==MAX_STORES);
			for (n = 0; n < MAX_STORES-1; n++)
			{
/*				if (STORE_HOME == n) continue; */

				/* Maintain */
				store_maint((store_indexes)(n));
			}

			/* Sometimes, shuffle the shop-keepers */
			if (one_in_(STORE_SHUFFLE))
			{
				/* Message */
				if (OPTION(cheat_xtra)) msg_print("Shuffling a Shopkeeper...");

				/* Pick a random shop (except home), then shuffle it */
/*				ZAIBAND_STATIC_ASSERT(STORE_HOME+1==MAX_STORES);	*/
				store_shuffle((store_indexes)(rand_int(MAX_STORES-1)));
/*
				int n = rand_int(MAX_STORES-1);
				if (STORE_HOME <= n) ++n;
				store_shuffle((store_indexes)(n));
 */
			}

			/* Message */
			if (OPTION(cheat_xtra)) msg_print("Done.");
		}
	}


	/*** Process the monsters ***/

	/* Check for creature generation */
	if (one_in_(MAX_M_ALLOC_CHANCE))
	{
		/* Make a new monster */
		(void)alloc_monster(MAX_SIGHT + 5, FALSE);
	}

	/* Hack -- Check for creature regeneration */
	if (!(turn % 100)) regen_monsters();


	/*** Damage over Time ***/

	/* Take damage from poison */
	if (p_ptr->timed[TMD_POISONED])
	{
		/* Take damage */
		take_hit(1, "poison");
	}

	/* Take damage from cuts */
	if (p_ptr->timed[TMD_CUT])
	{
		unsigned int bleeding = cut_level(p_ptr->timed[TMD_CUT]);
		
		if (6 <= bleeding)
			i = 3;	/* Mortal wound or Deep Gash */
		else if (5 == bleeding)
			i = 2;	/* Severe cut */
		else
			i = 1;	/* Other cuts */

		/* Take damage */
		take_hit(i, "a fatal wound");
	}


	/*** Check the Food, and Regenerate ***/

	/* Digest normally */
	if (p_ptr->food < PY_FOOD_MAX)
	{
		/* Every 100 game turns */
		if (!(turn % 100))
		{
			/* Basic digestion rate based on speed */
			i = extract_energy[p_ptr->speed] * 2;

			/* Regeneration takes more food */
			if (p_ptr->regenerate) i += 30;

			/* Slow digestion takes less food */
			if (p_ptr->slow_digest) i -= 10;

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
	if (p_ptr->food < PY_FOOD_WEAK)
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
			if (!p_ptr->timed[TMD_PARALYZED] && one_in_(10))
			{
				/* Message */
				msg_print("You faint from the lack of food.");
				disturb(1, 0);

				/* Hack -- faint (bypass free action) */
				(void)p_ptr->inc_timed<TMD_PARALYZED>(1 + rand_int(5));
			}
		}
	}

	/* Regeneration ability */
	if (p_ptr->regenerate)
	{
		regen_amount *= 2;
	}

	/* Searching or Resting */
	if (p_ptr->searching || p_ptr->resting)
	{
		regen_amount *= 2;
	}

	/* Regenerate the mana */
	if (p_ptr->csp < p_ptr->msp)
	{
		regenmana(regen_amount);
	}

	/* Various things interfere with healing */
	if (p_ptr->timed[TMD_PARALYZED]) regen_amount = 0;
	if (p_ptr->timed[TMD_POISONED]) regen_amount = 0;
	if (p_ptr->timed[TMD_STUN]) regen_amount = 0;
	if (p_ptr->timed[TMD_CUT]) regen_amount = 0;

	/* Regenerate Hit Points if needed */
	if (p_ptr->chp < p_ptr->mhp)
	{
		regenhp(regen_amount);
	}


	/*** Timeout Various Things ***/
	{
	const int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);
	for(i=0; i<TMD_MAX; ++i)
		switch(i)
		{
		case TMD_CUT:		{
							unsigned int bleeding = cut_level(p_ptr->timed[TMD_CUT]);

							/* mortal wounds do not heal */
							if (7 > bleeding) (void)p_ptr->dec_timed(i, adjust);
							break;
							};
		case TMD_POISONED:	/* fall-through intentional */
		case TMD_STUN:		{	
							(void)p_ptr->dec_timed(i, adjust);
							break;
							};
		default :	(void)p_ptr->dec_timed(i, 1);
		};
	}

	/* Paralyzed or Knocked Out: reset energy to 0 */
	if ((p_ptr->timed[TMD_PARALYZED]) || (p_ptr->timed[TMD_STUN] >= 100))
		p_ptr->energy = 0;

	monster_scan(monster_condition_timeout);

	/*** Process Light ***/

	/* Check for light being wielded */
	o_ptr = &p_ptr->inventory[INVEN_LITE];

	/* Burn some fuel in the current lite (unless it is daytime in the town) */
	if ((TV_LITE == o_ptr->obj_id.tval) && ((p_ptr->depth) || ((5L * TOWN_DAWN) <= turn % (10L * TOWN_DAWN))))
	{
		/* Hack -- Use some fuel (except on artifacts) */
		if (!o_ptr->is_artifact() && (o_ptr->pval > 0))
		{
			/* Decrease life-span */
			o_ptr->pval--;

			/* Hack -- notice interesting fuel steps */
			if ((o_ptr->pval < 100) || (!(o_ptr->pval % 100)))
			{
				/* Window stuff */
				p_ptr->redraw |= (PR_EQUIP);
			}

			/* Hack -- Special treatment when blind */
			if (p_ptr->timed[TMD_BLIND] )
			{
				/* Hack -- save some light for later */
				if (o_ptr->pval == 0) o_ptr->pval++;
			}

			/* The light is now out */
			else if (o_ptr->pval == 0)
			{
				disturb(0, 0);
				msg_print("Your light has gone out!");
			}

			/* The light is getting dim */
			else if ((o_ptr->pval < 100) && (!(o_ptr->pval % 10)))
			{
				disturb(0, 0);
				msg_print("Your light is growing faint.");
			}
		}
	}

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);


	/*** Process Inventory ***/

	/* Handle experience draining */
	if (p_ptr->exp_drain)
	{
		if (one_in_(10) && (p_ptr->exp > 0))
		{
			p_ptr->exp--;
			p_ptr->max_exp--;
			check_experience();
		}
	}

	/* Recharge activatable objects and rods */
	recharge_objects();

	/* Feel the inventory */
	sense_inventory();


	/*** Involuntary Movement ***/

	/* Mega-Hack -- Random teleportation XXX XXX XXX */
	if ((p_ptr->teleport) && one_in_(100))
	{
		/* Teleport player */
		teleport_player(40);
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
			if (p_ptr->depth)
			{
				msg_print("You feel yourself yanked upwards!");

				/* New depth */
				p_ptr->depth = 0;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
			else
			{
				msg_print("You feel yourself yanked downwards!");

				/* New depth */
				p_ptr->depth = p_ptr->max_depth;
				if (p_ptr->depth < 1) p_ptr->depth = 1;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}
	}
}



/**
 * Verify use of "wizard" mode
 */
static bool enter_wizard_mode(void)
{
	/* Ask first time - unless resurrecting a dead character */
	if (!(p_ptr->noscore & NOSCORE_WIZARD) && !(p_ptr->is_dead))
	{
		/* Mention effects */
		msg_print("You are about to enter 'wizard' mode for the very first time!");
		msg_print("This is a form of cheating, and your game will not be scored!");
		message_flush();

		/* Verify request */
		if (!get_check("Are you sure you want to enter wizard mode? ")) return FALSE;
	}

	/* Mark savefile */
	p_ptr->noscore |= NOSCORE_WIZARD;

	/* Success */
	return TRUE;
}



#ifdef ALLOW_DEBUG

/**
 * Verify use of "debug" mode
 */
static bool verify_debug_mode(void)
{
	/* Ask first time */
	if (!(p_ptr->noscore & NOSCORE_DEBUG))
	{
		/* Mention effects */
		msg_print("You are about to use the dangerous, unsupported, debug commands!");
		msg_print("Your machine may crash, and your savefile may become corrupted!");
		message_flush();

		/* Verify request */
		if (!get_check("Are you sure you want to use the debug commands? ")) return FALSE;
	}

	/* Mark savefile */
	p_ptr->noscore |= NOSCORE_DEBUG;

	/* Okay */
	return TRUE;
}

#endif /* ALLOW_DEBUG */



#ifdef ALLOW_BORG

/**
 * Verify use of "borg" mode
 */
static bool verify_borg_mode(void)
{
	/* Ask first time */
	if (!(p_ptr->noscore & NOSCORE_BORG))
	{
		/* Mention effects */
		msg_print("You are about to use the dangerous, unsupported, borg commands!");
		msg_print("Your machine may crash, and your savefile may become corrupted!");
		message_flush();

		/* Verify request */
		if (!get_check("Are you sure you want to use the borg commands? ")) return FALSE;
	}

	/* Mark savefile */
	p_ptr->noscore |= NOSCORE_BORG;

	/* Okay */
	return TRUE;
}

#endif /* ALLOW_BORG */

#if 0
		/*** Standard "Movement" Commands ***/

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

		/*** Looking at Things (nearby or on map) ***/

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

#endif

/** \page commands Command Reference for Zaiband
 * A terse summary of original-set keyboard commands for Zaiband
 *
 * Inventory commands:
 * - \subpage wear_wield Wear/Wield equipment (w)
 * - \subpage take_off Take off equipment (t)
 * - \subpage drop_item Drop an item (d)
 * - \subpage destroy_item Destroy an item (k)
 * - \subpage equipment_list Equipment list (e)
 * - \subpage inventory_list Inventory list (i)
 * - \subpage toggle_inven__equip Toggle inventory and equipment windows (CTRL-E)
 *
 * Movement commands:
 * - \subpage walking Walking and direction keys (12346789)
 * - \subpage stay_still Holding still (5)
 * - \subpage running Running (.)
 * - \subpage resting Rest (R)
 * - \subpage enter_store Stores and shopping
 *
 * Magic/Prayer:
 * - \subpage gain_spell Gain a spell (G)
 * - \subpage browse_book Browse a book (b)
 * - \subpage cast_pray Cast a spell/invoke a prayer (m,p)
 *
 * Terrain commands:
 * - \subpage open_door_chest Open a door or chest (o) 
 * - \subpage close_door Close a door (c) 
 * - \subpage jam_door Jam a door with spikes (j) 
 * - \subpage bash_door Bash a door (B) 
 * - \subpage disarm_trap_chest Disarm a trap or chest (o) 
 * - \subpage dig_tunnel Dig a tunnel (T)
 * - \subpage alter_grid Alter a grid (+)
 * - \subpage search_trap_door Search for traps and doors (s)
 * - \subpage toggle_search Toggle search mode (S)
 * - \subpage go_up Go up a level (<)
 * - \subpage go_down Go down a level (>)
 *
 * Object manipulation commands:
 * - \subpage eat__food Eat some food (E)
 * - \subpage fuel_light Fuel a lantern or torch (F)
 * - \subpage fire_item Fire an item (f)
 * - \subpage throw_item Throw an item (v)
 * - \subpage quaff__potion Quaff a potion (q)
 * - \subpage read__scroll Read a scroll (r)
 * - \subpage aim__wand Aim a wand (a)
 * - \subpage use__staff Use a staff (u)
 * - \subpage zap__rod Zap a rod (z)
 * - \subpage activate_artifact Activate an artifact (A)
 * - \subpage inscribe_uninscribe Inscribing ({) and uninscribing (}) an object
 *
 * Information commands:
 * - \subpage help_cmd Inline help (?)
 * - \subpage look_around Look around (l)
 * - \subpage target_location Targeting a monster or location (*)
 * - \subpage character_desc Character description (C)
 * - \subpage observe_object Observe an object (I)
 * - \subpage identify_symbol Identify a symbol (/)
 * - \subpage whole_map Full dungeon map (M)
 * - \subpage locate_player Locate player on map (L)
 * - \subpage check_knowledge Check knowledge (~) or (|)
 * - \subpage take_note Take notes (:)
 * - \subpage check_previous_message Check previous message (CTRL-O)
 * - \subpage check_messages Check messages (CTRL-P)
 * - \subpage check_feeling Check level feeling (CTRL-F)
 * - \subpage redraw_screen Redraw the screen (CTRL-R)
 *
 * Meta commands
 * - \subpage save Save game (CTRL-S)
 * - \subpage save_quit Save game and quit (CTRL-X)
 * - \subpage suicide Commit suicide (Q)
 * - \subpage interact_options Interact with options (=)
 * - \subpage interact_colors Interact with colors (&)
 * - \subpage interact_visuals Interact with visuals (%)
 * - \subpage interact_macros Interact with macros (@)
 * - \subpage one_line_pref Single line from a pref file (")
 * - \subpage user_interface User interface (!)
 * - \subpage load_screen Load screen dump (()
 * - \subpage save_screen Save screen dump ())
 * - \subpage version Show verson info (V)
 *
 * Cheating commands:
 * - \subpage wizard_mode Wizard Mode (CTRL-W)
 * - \subpage debug_command Debug Commands (CTRL-A)
 * - \subpage borg_command Borg Commands (CTRL-Z)
 */

/**
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 */
static void process_command(void)
{

	/* Handle repeating the last command */
	repeat_check();

	/* Parse the command */
	switch (p_ptr->command_cmd)
	{
		/* Ignore */
		case ESCAPE:
		case ' ':
		case '\n':
		case '\r':
		case '\a':
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
			do_cmd_spike();
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
			if (p_ptr->spell_book() == TV_PRAYER_BOOK)
				do_cmd_pray();
			else
				do_cmd_cast();

			break;
		}

		/* Pray a prayer */
		case 'p':
		{
			if (p_ptr->spell_book() == TV_MAGIC_BOOK)
				do_cmd_cast();
			else
				do_cmd_pray();

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
			do_cmd_options();
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

		/* Repeat level feeling */
		case KTRL('F'):
		{
			do_cmd_feeling();
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
		case '|':
		{
			do_cmd_knowledge();
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

		/* Hack -- Unknown command */
		default:
		{
			prt("Type '?' for help.", 0, 0);
			break;
		}
	}
}



/**
 * Hack -- helper function for "process_player()"
 *
 * Check for changes in the "monster memory"
 */
static void process_player_aux(void)
{
	static int old_monster_race_idx = 0;

	static u32b	old_flags[RACE_FLAG_STRICT_UB] = {0, 0, 0};
	static u32b	old_spell_flags[RACE_FLAG_SPELL_STRICT_UB] = {0, 0, 0};
	static byte old_blows[MONSTER_BLOW_MAX];

	static byte	old_cast_innate = 0;
	static byte	old_cast_spell = 0;


	/* Tracking a monster */
	if (p_ptr->monster_race_idx)
	{
		/* Get the monster lore */
		monster_lore *l_ptr = &monster_type::l_list[p_ptr->monster_race_idx];
		
		/* Check for change of any kind */
		if (0!=memcmp(old_blows,l_ptr->blows,MONSTER_BLOW_MAX) ||
		    (old_monster_race_idx != p_ptr->monster_race_idx) ||
		    (0!=memcmp(old_flags, l_ptr->flags, sizeof(u32b)*RACE_FLAG_STRICT_UB)) ||
		    (0!=memcmp(old_spell_flags, l_ptr->spell_flags, sizeof(u32b)*RACE_FLAG_SPELL_STRICT_UB)) ||
		    (old_cast_innate != l_ptr->cast_innate) ||
		    (old_cast_spell != l_ptr->cast_spell))
		{
			/* Memorize old race */
			old_monster_race_idx = p_ptr->monster_race_idx;

			/* Memorize flags */
			C_COPY(old_flags, l_ptr->flags, RACE_FLAG_STRICT_UB);

			/* Memorize spell flags */
			C_COPY(old_spell_flags, l_ptr->spell_flags, RACE_FLAG_SPELL_STRICT_UB);

			/* Memorize blows */
			C_COPY(old_blows, l_ptr->blows, MONSTER_BLOW_MAX);

			/* Memorize castings */
			old_cast_innate = l_ptr->cast_innate;
			old_cast_spell = l_ptr->cast_spell;

			/* Update the display */
			p_ptr->redraw |= (PR_MONSTER);
			redraw_stuff();
		}
	}
}

/**
 * Zaiband changes when noise is applied to when it's generated.
 */
void apply_noise(coord src,int intensity)
{
	int i;

	/* Handle "leaving" */
	if (p_ptr->leaving) return;

	/* todo: player, once sound flow is truly working */

	/* Process the monsters (backwards) */
	for (i = mon_max - 1; i >= 1; i--)
	{
		{ /* C-ish blocking brace */
		/* Get the monster */
		monster_type* m_ptr = &mon_list[i];

		if (!m_ptr->r_idx) continue;	/* Ignore "dead" monsters */
		if (!m_ptr->csleep) continue;	/* monster is already awake */

		{	/* C-ish blocking brace */
		/* XXX technically correct only for pass_wall/kill_wall, others should use sound flow XXX */
		int dist = distance(src.y,src.x,m_ptr->loc.y,m_ptr->loc.x);
		monster_race *r_ptr = m_ptr->race();

		if (src==p_ptr->loc)
		{	/* player is source, we have cheap reverse-visibility check */
			if (dist > r_ptr->aaf && !player_has_los_bold(m_ptr->loc.y, m_ptr->loc.x)) continue;	/* too far away to hear */
		}
		else
		{	/* XXX should use reverse-visibility check as well */
			if (dist > r_ptr->aaf) continue;	/* too far away to hear */
		}


		/* Wake up faster near the player (or other noise source) */
		m_ptr->disturb(100*intensity/dist);
		}	/* end blocking brace */
		}	/* end blocking brace */
	}
}


/**
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

	/* should actually be able to move */
	assert(!p_ptr->timed[TMD_PARALYZED] && (100 > p_ptr->timed[TMD_STUN]) && (150 <= p_ptr->energy));

	/*** Check for interrupts ***/

	/* Complete resting */
	if (p_ptr->resting < 0)
	{
		/* Basic resting */
		if (p_ptr->resting == -1)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) &&
			    (p_ptr->csp == p_ptr->msp))
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
			    !p_ptr->timed[TMD_BLIND]  && !p_ptr->timed[TMD_CONFUSED] &&
			    !p_ptr->timed[TMD_POISONED] && !p_ptr->timed[TMD_AFRAID] &&
			    !p_ptr->timed[TMD_STUN] && !p_ptr->timed[TMD_CUT] &&
			    !p_ptr->timed[TMD_SLOW] && !p_ptr->timed[TMD_PARALYZED] &&
			    !p_ptr->timed[TMD_IMAGE] && !p_ptr->word_recall)
			{
				disturb(0, 0);
			}
		}
	}

	/* Handle "abort" */
	if (!OPTION(avoid_abort))
	{
		/* Check for "player abort" */
		if (p_ptr->running ||
		    p_ptr->command_rep ||
		    (p_ptr->resting && !(turn & 0x7F)))
		{
			/* Do not wait */
			inkey_scan = TRUE;

			/* Check for a key */
			if (inkey())
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


	/*** Handle actual user input ***/
	p_ptr->redraw |= (PR_MONSTER);	/* Update the monster tracking */

	/* Repeat until energy is reduced */
	do
	{
		/* Notice stuff (if needed) */
		if (p_ptr->notice) notice_stuff();

		/* Handle stuff */
		handle_stuff();

		/* Place the cursor on the player */
		move_cursor_relative(p_ptr->loc);

		/* Refresh */
		Term_fresh();


		/* Hack -- Pack Overflow */
		if (p_ptr->inventory[INVEN_PACK].k_idx)
		{
			char o_name[80];

			/* Get the slot to be dropped */
			object_type* const o_ptr = &p_ptr->inventory[INVEN_PACK];

			/* Disturbing */
			disturb(0, 0);

			/* Warning */
			msg_print("Your pack overflows!");

			/* Describe */
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

			/* Message */
			msg_format("You drop %s (%c).", o_name, index_to_label(INVEN_PACK));

			/* Drop it (carefully) near the player */
			drop_near(o_ptr, 0, p_ptr->loc);

			/* Modify, Describe, Optimize */
			inven_item_increase(INVEN_PACK, -255);
			inven_item_describe(INVEN_PACK);
			inven_item_optimize(INVEN_PACK);

			/* Notice stuff (if needed) */
			if (p_ptr->notice) notice_stuff();

			/* Handle stuff */
			handle_stuff();
		}


		/* Hack -- cancel "lurking browse mode" */
		if (!p_ptr->command_new) p_ptr->command_see = FALSE;

		/* Assume free turn */
		p_ptr->energy_use = 0;


		/* Resting */
		if (p_ptr->resting)
		{
			/* Timed rest */
			if (p_ptr->resting > 0)
			{
				/* Reduce rest count */
				p_ptr->resting--;

				/* Redraw the state */
				p_ptr->redraw |= (PR_STATE);
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
			}
		}

		/* Normal command */
		else
		{
			/* Check monster recall */
			process_player_aux();

			/* Place the cursor on the player */
			move_cursor_relative(p_ptr->loc);

			/* Get a command (normal) */
			request_command(FALSE);

			/* Process the command */
			process_command();
		}


		/*** Clean up ***/

		/* Significant */
		if (p_ptr->energy_use)
		{
			/* Use some energy */
			p_ptr->energy -= p_ptr->energy_use;


			/* Hack -- constant hallucination */
			if (p_ptr->timed[TMD_IMAGE]) p_ptr->redraw |= (PR_MAP);

			/* Shimmer monsters if needed */
			if (!OPTION(avoid_other) && shimmer_monsters)
			{
				/* Clear the flag */
				shimmer_monsters = FALSE;

				/* Shimmer multi-hued monsters */
				for (i = 1; i < mon_max; i++)
				{
					const monster_type* const m_ptr = &mon_list[i];	/* Get the monster */

					/* Skip dead monsters */
					if (!m_ptr->r_idx) continue;

					/* Skip non-multi-hued monsters */
					if (!(m_ptr->race()->flags[0] & RF0_ATTR_MULTI)) continue;

					/* Reset the flag */
					shimmer_monsters = TRUE;

					/* Redraw regardless */
					lite_spot(m_ptr->loc);
				}
			}

			/* Repair "nice" flags */
			if (repair_mflag_nice)
			{
				/* Clear flag */
				repair_mflag_nice = FALSE;

				/* Process monsters */
				for (i = 1; i < mon_max; i++)
				{	/* loop intentionally does not skip dead monsters */
					mon_list[i].mflag &= ~(MFLAG_NICE);	/* Clear "nice" flag */
				}
			}

			/* Repair "mark" flags */
			if (repair_mflag_mark)
			{
				/* Reset the flag */
				repair_mflag_mark = FALSE;

				/* Process the monsters */
				for (i = 1; i < mon_max; i++)
				{
					monster_type* const m_ptr = &mon_list[i];	/* Get the monster */

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

			/* Zaiband: player makes noise here */
			{	/* C-ish blocking brace */
			u32b notice = rand_int(1024);
			if ((notice * notice * notice) <= p_ptr->noise)
				apply_noise(p_ptr->loc,1);	/* player made noise */
			}	/* end C-ish blocking brace */
		}

		/* Repair "show" flags */
		if (repair_mflag_show)
		{
			/* Reset the flag */
			repair_mflag_show = FALSE;

			/* Process the monsters */
			for (i = 1; i < mon_max; i++)
			{	/* loop intentionally does not skip dead monsters */
				mon_list[i].mflag &= ~(MFLAG_SHOW);	/* Clear "show" flag */
			}
		}
	}
	while (!p_ptr->energy_use && !p_ptr->leaving);
}



/**
 * Interact with the current dungeon level.
 *
 * This function will not exit until the level is completed,
 * the user dies, or the game is terminated.
 */
static void dungeon(void)
{
	int i;

	int py = p_ptr->loc.y;
	int px = p_ptr->loc.x;


	/* Hack -- enforce illegal panel */
	Term->offset_y = DUNGEON_HGT;
	Term->offset_x = DUNGEON_WID;


	/* Not leaving */
	p_ptr->leaving = FALSE;


	/* Reset the "command" vars */
	p_ptr->command_cmd = 0;
	p_ptr->command_new = 0;
	p_ptr->command_rep = 0;
	p_ptr->command_arg = 0;
	p_ptr->command_dir = 0;

	/* Cancel the target */
	target_set_monster(0);

	/* Cancel the health bar */
	health_track(0);


	/* Reset shimmer flags */
	shimmer_monsters = TRUE;
	shimmer_objects = TRUE;

	/* Reset repair flags */
	repair_mflag_nice = TRUE;
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


	/* No stairs down from Quest */
	if (is_quest(p_ptr->depth))
	{
		p_ptr->create_down_stair = FALSE;
	}

	/* No stairs from town or if not allowed */
	if (!p_ptr->depth || !OPTION(adult_dungeon_stair))
	{
		p_ptr->create_down_stair = p_ptr->create_up_stair = FALSE;
	}

	/* Make a staircase */
	if (p_ptr->create_down_stair || p_ptr->create_up_stair)
	{
		/* Place a staircase */
		if (cave_valid_bold(py, px))
		{
			/* XXX XXX XXX */
			delete_object(py, px);

			/* Make stairs */
			if (p_ptr->create_down_stair)
			{
				cave_set_feat(py, px, FEAT_MORE);
			}
			else
			{
				cave_set_feat(py, px, FEAT_LESS);
			}

			/* Mark the stairs as known */
			cave_info[py][px] |= (CAVE_MARK);
		}

		/* Cancel the stair request */
		p_ptr->create_down_stair = p_ptr->create_up_stair = FALSE;
	}


	/* Choose panel */
	event_signal(EVENT_PLAYERMOVED);


	/* Flush messages */
	message_flush();


	/* Hack -- Increase "xtra" depth */
	character_xtra++;


	/* Clear */
	Term_clear();


	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);

	/* Update stuff */
	update_stuff();


	/* Fully update the visuals (and monster distances) */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_DISTANCE);

	/* Fully update the flow */
	p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);

	/* Redraw dungeon */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP | PR_INVEN | PR_EQUIP | PR_MONSTER | PR_MONLIST);

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");

	/* Hack -- Decrease "xtra" depth */
	character_xtra--;


	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Notice stuff */
	notice_stuff();

	assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");

	/* Handle stuff */
	handle_stuff();

	/* Refresh */
	Term_fresh();


	/* Handle delayed death */
	if (p_ptr->is_dead) return;


	/* Announce (or repeat) the feeling */
	if (p_ptr->depth) do_cmd_feeling();


	/*** Process this dungeon level ***/

	/* Reset the monster generation level */
	monster_level = p_ptr->depth;

	/* Reset the object generation level */
	object_level = p_ptr->depth;

	/* Main loop */
	while (TRUE)
	{
		/* Hack -- Compact the monster list occasionally */
		if (mon_cnt + 32 > z_info->m_max) compact_monsters(64);

		/* Hack -- Compress the monster list occasionally */
		if (mon_cnt + 32 < mon_max) compact_monsters(0);


		/* Hack -- Compact the object list occasionally */
		if (o_cnt + 32 > z_info->o_max) compact_objects(64);

		/* Hack -- Compress the object list occasionally */
		if (o_cnt + 32 < o_max) compact_objects(0);


		/* Can the player move? */
		/* Zaiband: origin adjusted to 150 to handle monster energy being a byte, and diagonal move cost 150 */
		while ((p_ptr->energy >= 150) && !p_ptr->leaving)
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

		/* Handle stuff */
		handle_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->loc);

		/* Handle "leaving" */
		if (p_ptr->leaving) break;


		/* Process all of the monsters */
		process_monsters(150);

		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Handle stuff */
		handle_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->loc);

		/* Handle "leaving" */
		if (p_ptr->leaving) break;


		/* Process the world */
		process_world();

		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Handle stuff */
		handle_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->loc);

		/* Handle "leaving" */
		if (p_ptr->leaving) break;


		/*** Apply energy ***/

		/* Give the player some energy */
		p_ptr->energy += extract_energy[p_ptr->speed];

		/* Give energy to all monsters */
		for (i = mon_max - 1; i >= 1; i--)
		{
			/* Access the monster */
			monster_type* const m_ptr = &mon_list[i];

			/* Ignore "dead" monsters */
			if (!m_ptr->r_idx) continue;

			/* Give this monster some energy */
			m_ptr->energy += extract_energy[m_ptr->speed];
		}

		/* Count game turns */
		turn++;
	}
}



/**
 * Process some user pref files
 */
static void process_some_user_pref_files(void)
{
	char buf[1024];


	/* Process the "user.prf" file */
	(void)process_pref_file("user.prf");

	/* Get the "PLAYER.prf" filename */
	(void)strnfmt(buf, sizeof(buf), "%s.prf", op_ptr->base_name);

	/* Process the "PLAYER.prf" file */
	(void)process_pref_file(buf);
}


/**
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
#ifdef ZAIBAND_NEW_COMMAND_LOOP
void play_game(void)
#else
void play_game(bool new_game)
#endif
{
#ifdef ZAIBAND_NEW_COMMAND_LOOP
	/* Initialize */ 
 	bool new_game = init_angband(); 
#endif

	/* Hack -- Increase "icky" depth */
	character_icky++;


	/* Verify main term */
	if (!term_screen)
	{
		quit("main window does not exist");
	}

	/* Make sure main term is active */
	Term_activate(term_screen);

	/* Verify minimum size */
	if ((Term->hgt < 24) || (Term->wid < 80))
	{
		quit("main window is too small");
	}

	/* Hack -- Turn off the cursor */
	(void)Term_set_cursor(FALSE);

	/* Set screen resize hook */
	Term_set_resize_hook(do_cmd_redraw);

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
		strcpy(op_ptr->base_name, "PLAYER");
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
		seed_randart = rand_int(0x10000000);

		/* Roll up a new character */
		player_birth();

		/* Randomize the artifacts */
		if (OPTION(adult_rand_artifacts))
		{
			do_randart(seed_randart, TRUE);
		}

		/* Hack -- enter the world */
		turn = 1;
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

	/* Reset visuals */
	reset_visuals(TRUE);


	/* Window stuff */
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_MESSAGE | PR_MONSTER);

	/* Redraw stuff */
	redraw_stuff();


	/* Process some user pref files */
	process_some_user_pref_files();


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
		/* Play ambient sound on change of level. */
		play_ambient_sound();

		/* Process the level */
		dungeon();

		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Handle stuff */
		handle_stuff();

		/* Cancel the target */
		target_set_monster(0);

		/* Cancel the health bar */
		health_track(0);


		/* Forget the view */
		forget_view();


		/* Handle "quit and save" */
		if (!p_ptr->playing && !p_ptr->is_dead) break;


		/* Erase the old cave */
		wipe_o_list();
		wipe_mon_list();


		/* XXX XXX XXX */
		message_flush();

		/* Accidental Death */
		if (p_ptr->playing && p_ptr->is_dead)
		{
			/* Mega-Hack -- Allow player to cheat death */
			if ((p_ptr->wizard || OPTION(cheat_live)) && !get_check("Die? "))
			{
				/* Mark social class, reset age, if needed */
				if (p_ptr->sc) p_ptr->sc = p_ptr->age = 0;

				/* Increase age */
				p_ptr->age++;

				/* Mark savefile */
				p_ptr->noscore |= NOSCORE_WIZARD;

				/* Message */
				msg_print("You invoke wizard mode and cheat death.");
				message_flush();

				/* Cheat death */
				p_ptr->is_dead = FALSE;

				/* Restore hit points */
				p_ptr->chp = p_ptr->mhp;
				p_ptr->chp_frac = 0;

				/* Restore spell points */
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;

				/* Hack -- Healing */
				(void)p_ptr->clear_timed<TMD_BLIND>(); 
				(void)p_ptr->clear_timed<TMD_CONFUSED>(); 
				(void)p_ptr->clear_timed<TMD_POISONED>(); 
				(void)p_ptr->clear_timed<TMD_AFRAID>(); 
				(void)p_ptr->clear_timed<TMD_PARALYZED>(); 
				(void)p_ptr->clear_timed<TMD_IMAGE>(); 
				(void)p_ptr->clear_timed<TMD_STUN>(); 
				(void)p_ptr->clear_timed<TMD_CUT>(); 

				/* Hack -- Prevent starvation */
				(void)set_food(PY_FOOD_MAX - 1);

				/* Hack -- cancel recall */
				if (p_ptr->word_recall)
				{
					/* Message */
					msg_print("A tension leaves the air around you...");
					message_flush();

					/* Hack -- Prevent recall */
					p_ptr->word_recall = 0;
				}

				/* Note cause of death XXX XXX XXX */
				strcpy(p_ptr->died_from, "Cheating death");

				/* New depth */
				p_ptr->depth = 0;

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
}
