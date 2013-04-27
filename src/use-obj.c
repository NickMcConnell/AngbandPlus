/* File: use-obj.c */

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
#include "cmds.h"
#include "option.h"
#include "project.h"
#include "summon.h"
#include "tvalsval.h"

#include "keypad.h"
/*
 * Check to see if the player can use a rod/wand/staff/activatable object.
 */
static bool check_devices(object_type *o_ptr)
{
	int lev = (o_ptr->is_artifact()) ? object_type::a_info[o_ptr->name1].level : object_type::k_info[o_ptr->k_idx].level; /* Extract the item level */
	const char *msg;
	const char *what = NULL;

	/* Get the right string */
	switch (o_ptr->obj_id.tval)
	{
		case TV_ROD:   msg = "zap the rod";   break;
		case TV_WAND:  msg = "use the wand";  what = "wand";  break;
		case TV_STAFF: msg = "use the staff"; what = "staff"; break;
		default:       msg = "activate it";
	}

	/* Base chance of success */
	/* staves always work */
	if (TV_STAFF != o_ptr->obj_id.tval)
	{
		int chance = p_ptr->item_chance(lev);

		/* Roll for usage */
		if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
		{
			if (OPTION(flush_failure)) flush();
			msg_format("You failed to %s properly.", msg);
			return FALSE;
		}
	}

	/* Notice empty staffs */
	if (what && o_ptr->pval <= 0)
	{
		if (OPTION(flush_failure)) flush();
		msg_format("The %s has no charges left.", what);
		o_ptr->ident |= (IDENT_EMPTY);
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);
		p_ptr->redraw |= (PR_INVEN);
		return (FALSE);
	}

	return TRUE;
}

static bool eat_food(object_type *o_ptr, bool *ident)
{
	/* Analyze the food */
	switch (o_ptr->obj_id.sval)
	{
		case SV_FOOD_POISON:
		{
			if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
			{
				if (p_ptr->inc_timed<TMD_POISONED>(rand_int(10) + 10))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_BLINDNESS:
		{
			if (!p_ptr->resist_blind)
			{
				if (p_ptr->inc_timed<TMD_BLIND>(rand_int(200) + 200))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_PARANOIA:
		{
			if (!p_ptr->resist_fear)
			{
				if (p_ptr->inc_timed<TMD_AFRAID>(rand_int(10) + 10))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_CONFUSION:
		{
			if (!p_ptr->resist_confu)
			{
				if (p_ptr->inc_timed<TMD_CONFUSED>(rand_int(10) + 10))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_HALLUCINATION:
		{
			if (!p_ptr->resist_chaos)
			{
				if (p_ptr->inc_timed<TMD_IMAGE>(rand_int(250) + 250))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_PARALYSIS:
		{
			if (!p_ptr->free_act)
			{
				if (p_ptr->inc_timed<TMD_PARALYZED>(rand_int(10) + 10))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_WEAKNESS:
		{
			take_hit(NdS(6, 6), "poisonous food");
			(void)do_dec_stat(A_STR);
			*ident = TRUE;
			break;
		}

		case SV_FOOD_SICKNESS:
		{
			take_hit(NdS(6, 6), "poisonous food");
			(void)do_dec_stat(A_CON);
			*ident = TRUE;
			break;
		}

		case SV_FOOD_STUPIDITY:
		{
			take_hit(NdS(8, 8), "poisonous food");
			(void)do_dec_stat(A_INT);
			*ident = TRUE;
			break;
		}

		case SV_FOOD_NAIVETY:
		{
			take_hit(NdS(8, 8), "poisonous food");
			(void)do_dec_stat(A_WIS);
			*ident = TRUE;
			break;
		}

		case SV_FOOD_UNHEALTH:
		{
			take_hit(NdS(10, 10), "poisonous food");
			(void)do_dec_stat(A_CON);
			*ident = TRUE;
			break;
		}

		case SV_FOOD_DISEASE:
		{
			take_hit(NdS(10, 10), "poisonous food");
			(void)do_dec_stat(A_STR);
			*ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_POISON:
		{
			if (p_ptr->clear_timed<TMD_POISONED>()) *ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_BLINDNESS:
		{
			if (p_ptr->clear_timed<TMD_BLIND>()) *ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_PARANOIA:
		{
			if (p_ptr->clear_timed<TMD_AFRAID>()) *ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_CONFUSION:
		{
			if (p_ptr->clear_timed<TMD_CONFUSED>()) *ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_SERIOUS:
		{
			if (hp_player(NdS(4, 8))) *ident = TRUE;
			break;
		}

		case SV_FOOD_RESTORE_STR:
		{
			if (do_res_stat(A_STR)) *ident = TRUE;
			break;
		}

		case SV_FOOD_RESTORE_CON:
		{
			if (do_res_stat(A_CON)) *ident = TRUE;
			break;
		}

		case SV_FOOD_RESTORING:
		{
			if (do_res_stat(A_STR)) *ident = TRUE;
			if (do_res_stat(A_INT)) *ident = TRUE;
			if (do_res_stat(A_WIS)) *ident = TRUE;
			if (do_res_stat(A_DEX)) *ident = TRUE;
			if (do_res_stat(A_CON)) *ident = TRUE;
			if (do_res_stat(A_CHR)) *ident = TRUE;
			break;
		}


		case SV_FOOD_RATION:
		case SV_FOOD_BISCUIT:
		case SV_FOOD_JERKY:
		case SV_FOOD_SLIME_MOLD:
		{
			msg_print("That tastes good.");
			*ident = TRUE;
			break;
		}

		case SV_FOOD_WAYBREAD:
		{
			msg_print("That tastes good.");
			(void)p_ptr->clear_timed<TMD_POISONED>();
			(void)hp_player(NdS(4, 8));
			*ident = TRUE;
			break;
		}

		case SV_FOOD_PINT_OF_ALE:
		case SV_FOOD_PINT_OF_WINE:
		{
			msg_print("That tastes good.");
			*ident = TRUE;
			break;
		}
	}

	/* Food can feed the player */
	(void)set_food(p_ptr->food + o_ptr->pval);

	return (TRUE);
}


static bool quaff_potion(object_type *o_ptr, bool *ident)
{
	/* Analyze the potion */
	switch (o_ptr->obj_id.sval)
	{
		case SV_POTION_WATER:
		case SV_POTION_APPLE_JUICE:
		case SV_POTION_SLIME_MOLD:
		{
			msg_print("You feel less thirsty.");
			*ident = TRUE;
			break;
		}

		case SV_POTION_SLOWNESS:
		{
			if (p_ptr->inc_timed<TMD_SLOW>(randint(25) + 15)) *ident = TRUE;
			break;
		}

		case SV_POTION_SALT_WATER:
		{
			msg_print("The potion makes you vomit!");
			(void)set_food(PY_FOOD_STARVE - 1);
			(void)p_ptr->clear_timed<TMD_POISONED>();
			(void)p_ptr->inc_timed<TMD_PARALYZED>(4);
			*ident = TRUE;
			break;
		}

		case SV_POTION_POISON:
		{
			if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
			{
				if (p_ptr->inc_timed<TMD_POISONED>(rand_int(15) + 10))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_BLINDNESS:
		{
			if (!p_ptr->resist_blind)
			{
				if (p_ptr->inc_timed<TMD_BLIND>(rand_int(100) + 100))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_CONFUSION:
		{
			if (!p_ptr->resist_confu)
			{
				if (p_ptr->inc_timed<TMD_CONFUSED>(rand_int(20) + 15))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_SLEEP:
		{
			if (!p_ptr->free_act)
			{
				if (p_ptr->inc_timed<TMD_PARALYZED>(rand_int(4) + 4))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_LOSE_MEMORIES:
		{
			if (!p_ptr->hold_life && (p_ptr->exp > 0))
			{
				msg_print("You feel your memories fade.");
				lose_exp(p_ptr->exp / 4);
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_RUINATION:
		{
			msg_print("Your nerves and muscles feel weak and lifeless!");
			take_hit(NdS(10, 10), "a potion of Ruination");
			(void)dec_stat(A_DEX, 25, TRUE);
			(void)dec_stat(A_WIS, 25, TRUE);
			(void)dec_stat(A_CON, 25, TRUE);
			(void)dec_stat(A_STR, 25, TRUE);
			(void)dec_stat(A_CHR, 25, TRUE);
			(void)dec_stat(A_INT, 25, TRUE);
			*ident = TRUE;
			break;
		}

		case SV_POTION_DEC_STR:
		{
			if (do_dec_stat(A_STR)) *ident = TRUE;
			break;
		}

		case SV_POTION_DEC_INT:
		{
			if (do_dec_stat(A_INT)) *ident = TRUE;
			break;
		}

		case SV_POTION_DEC_WIS:
		{
			if (do_dec_stat(A_WIS)) *ident = TRUE;
			break;
		}

		case SV_POTION_DEC_DEX:
		{
			if (do_dec_stat(A_DEX)) *ident = TRUE;
			break;
		}

		case SV_POTION_DEC_CON:
		{
			if (do_dec_stat(A_CON)) *ident = TRUE;
			break;
		}

		case SV_POTION_DEC_CHR:
		{
			if (do_dec_stat(A_CHR)) *ident = TRUE;
			break;
		}

		case SV_POTION_DETONATIONS:
		{
			msg_print("Massive explosions rupture your body!");
			take_hit(NdS(50, 20), "a potion of Detonation");
			(void)p_ptr->inc_timed<TMD_STUN>(75);
			(void)p_ptr->inc_timed<TMD_CUT>(5000);
			*ident = TRUE;
			break;
		}

		case SV_POTION_DEATH:
		{
			msg_print("A feeling of Death flows through your body.");
			take_hit(5000, "a potion of Death");
			*ident = TRUE;
			break;
		}

		case SV_POTION_INFRAVISION:
		{
			if (p_ptr->inc_timed<TMD_SINFRA>(100 + randint(100))) *ident = TRUE;
			break;
		}

		case SV_POTION_DETECT_INVIS:
		{
			if (p_ptr->inc_timed<TMD_SINVIS>(12 + randint(12))) *ident = TRUE;
			break;
		}

		case SV_POTION_SLOW_POISON:
		{
			if (p_ptr->set_timed<TMD_POISONED>(p_ptr->timed[TMD_POISONED] / 2)) *ident = TRUE;
			break;
		}

		case SV_POTION_CURE_POISON:
		{
			if (p_ptr->clear_timed<TMD_POISONED>()) *ident = TRUE;
			break;
		}

		case SV_POTION_BOLDNESS:
		{
			if (p_ptr->clear_timed<TMD_AFRAID>()) *ident = TRUE;
			break;
		}

		case SV_POTION_SPEED:
		{
			if (!p_ptr->timed[TMD_FAST])
			{
				if (p_ptr->inc_timed<TMD_FAST>(randint(25) + 15)) *ident = TRUE;
			}
			else
			{
				(void)p_ptr->inc_timed<TMD_FAST>(5);
			}
			break;
		}

		case SV_POTION_RESIST_HEAT:
		{
			if (p_ptr->inc_timed<TMD_OPP_FIRE>(randint(10) + 10)) *ident = TRUE;
			break;
		}

		case SV_POTION_RESIST_COLD:
		{
			if (p_ptr->inc_timed<TMD_OPP_COLD>(randint(10) + 10)) *ident = TRUE;
			break;
		}

		case SV_POTION_HEROISM:
		{
			if (hp_player(10)) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_AFRAID>()) *ident = TRUE;
			if (p_ptr->inc_timed<TMD_HERO>(randint(25) + 25)) *ident = TRUE;
			break;
		}

		case SV_POTION_BERSERK_STRENGTH:
		{
			if (hp_player(30)) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_AFRAID>()) *ident = TRUE;
			if (p_ptr->inc_timed<TMD_SHERO>(randint(25) + 25)) *ident = TRUE;
			break;
		}

		case SV_POTION_CURE_LIGHT:
		{
			if (hp_player(NdS(2, 8))) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_BLIND>()) *ident = TRUE;
			if (p_ptr->dec_timed<TMD_CUT>(10)) *ident = TRUE;
			break;
		}

		case SV_POTION_CURE_SERIOUS:
		{
			if (hp_player(NdS(4, 8))) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_BLIND>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_CONFUSED>()) *ident = TRUE;
			if (p_ptr->set_timed<TMD_CUT>((p_ptr->timed[TMD_CUT] / 2) - 50)) *ident = TRUE;
			break;
		}

		case SV_POTION_CURE_CRITICAL:
		{
			if (hp_player(NdS(6, 8))) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_BLIND>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_CONFUSED>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_POISONED>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_STUN>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_CUT>()) *ident = TRUE;
			break;
		}

		case SV_POTION_HEALING:
		{
			if (hp_player(300)) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_BLIND>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_CONFUSED>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_POISONED>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_STUN>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_CUT>()) *ident = TRUE;
			break;
		}

		case SV_POTION_STAR_HEALING:
		{
			if (hp_player(1200)) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_BLIND>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_CONFUSED>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_POISONED>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_STUN>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_CUT>()) *ident = TRUE;
			break;
		}

		case SV_POTION_LIFE:
		{
			msg_print("You feel life flow through your body!");
			restore_level();
			(void)p_ptr->clear_timed<TMD_POISONED>();
			(void)p_ptr->clear_timed<TMD_BLIND>();
			(void)p_ptr->clear_timed<TMD_CONFUSED>();
			(void)p_ptr->clear_timed<TMD_IMAGE>();
			(void)p_ptr->clear_timed<TMD_STUN>();
			(void)p_ptr->clear_timed<TMD_CUT>();
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_CHR);

			/* Recalculate max. hitpoints */
			update_stuff();

			hp_player(5000);

			*ident = TRUE;
			break;
		}

		case SV_POTION_RESTORE_MANA:
		{
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				msg_print("Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESTORE_EXP:
		{
			if (restore_level()) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_STR:
		{
			if (do_res_stat(A_STR)) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_INT:
		{
			if (do_res_stat(A_INT)) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_WIS:
		{
			if (do_res_stat(A_WIS)) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_DEX:
		{
			if (do_res_stat(A_DEX)) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_CON:
		{
			if (do_res_stat(A_CON)) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_CHR:
		{
			if (do_res_stat(A_CHR)) *ident = TRUE;
			break;
		}

		case SV_POTION_INC_STR:
		{
			if (do_inc_stat(A_STR)) *ident = TRUE;
			break;
		}

		case SV_POTION_INC_INT:
		{
			if (do_inc_stat(A_INT)) *ident = TRUE;
			break;
		}

		case SV_POTION_INC_WIS:
		{
			if (do_inc_stat(A_WIS)) *ident = TRUE;
			break;
		}

		case SV_POTION_INC_DEX:
		{
			if (do_inc_stat(A_DEX)) *ident = TRUE;
			break;
		}

		case SV_POTION_INC_CON:
		{
			if (do_inc_stat(A_CON)) *ident = TRUE;
			break;
		}

		case SV_POTION_INC_CHR:
		{
			if (do_inc_stat(A_CHR)) *ident = TRUE;
			break;
		}

		case SV_POTION_AUGMENTATION:
		{
			if (do_inc_stat(A_STR)) *ident = TRUE;
			if (do_inc_stat(A_INT)) *ident = TRUE;
			if (do_inc_stat(A_WIS)) *ident = TRUE;
			if (do_inc_stat(A_DEX)) *ident = TRUE;
			if (do_inc_stat(A_CON)) *ident = TRUE;
			if (do_inc_stat(A_CHR)) *ident = TRUE;
			break;
		}

		case SV_POTION_ENLIGHTENMENT:
		{
			msg_print("An image of your surroundings forms in your mind...");
			wiz_lite();
			*ident = TRUE;
			break;
		}

		case SV_POTION_STAR_ENLIGHTENMENT:
		{
			msg_print("You begin to feel more enlightened...");
			message_flush();
			wiz_lite();
			(void)do_inc_stat(A_INT);
			(void)do_inc_stat(A_WIS);
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			(void)detect_treasure();
			(void)detect_objects_gold();
			(void)detect_objects_normal();
			identify_pack();
			self_knowledge();
			*ident = TRUE;
			break;
		}

		case SV_POTION_SELF_KNOWLEDGE:
		{
			msg_print("You begin to know yourself a little better...");
			message_flush();
			self_knowledge();
			*ident = TRUE;
			break;
		}

		case SV_POTION_EXPERIENCE:
		{
			restore_level();
			if (p_ptr->exp < PY_MAX_EXP)
			{
				s32b ee = (p_ptr->exp / 2) + 10;
				if (ee > 100000L) ee = 100000L;
				msg_print("You feel more experienced.");
				gain_exp(ee);
				*ident = TRUE;
			}
			break;
		}
	}

	return (TRUE);
}


static bool read_scroll(object_type *o_ptr, bool *ident)
{
	int k;

	bool used_up = TRUE;


	/* Analyze the scroll */
	switch (o_ptr->obj_id.sval)
	{
		case SV_SCROLL_DARKNESS:
		{
			if (!p_ptr->resist_blind)
			{
				(void)p_ptr->inc_timed<TMD_BLIND>(3 + randint(5));
			}
			if (unlite_area(10, 3)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_AGGRAVATE_MONSTER:
		{
			msg_print("There is a high pitched humming noise.");
			aggravate_monsters(0);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_CURSE_ARMOR:
		{
			if (curse_armor()) *ident = TRUE;
			break;
		}

		case SV_SCROLL_CURSE_WEAPON:
		{
			if (curse_weapon()) *ident = TRUE;
			break;
		}

		case SV_SCROLL_SUMMON_MONSTER:
		{
			const int rand_strict_UB = randint(3);
			sound(MSG_SUM_MONSTER);
			for (k = 0; k < rand_strict_UB; k++)
			{
				if (summon_specific(p_ptr->loc, p_ptr->depth, 0))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_SCROLL_SUMMON_UNDEAD:
		{
			const int rand_strict_UB = randint(3);
			sound(MSG_SUM_UNDEAD);
			for (k = 0; k < rand_strict_UB; k++)
			{
				if (summon_specific(p_ptr->loc, p_ptr->depth, SUMMON_UNDEAD))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_SCROLL_TRAP_CREATION:
		{
			if (trap_creation()) *ident = TRUE;
			break;
		}

		case SV_SCROLL_PHASE_DOOR:
		{
			teleport_player(10);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_TELEPORT:
		{
			teleport_player(100);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_TELEPORT_LEVEL:
		{
			(void)teleport_player_level();
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_WORD_OF_RECALL:
		{
			set_recall();
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_IDENTIFY:
		{
			*ident = TRUE;
			if (!ident_spell()) used_up = FALSE;
			break;
		}

		case SV_SCROLL_STAR_IDENTIFY:
		{
			*ident = TRUE;
			if (!identify_fully()) used_up = FALSE;
			break;
		}

		case SV_SCROLL_REMOVE_CURSE:
		{
			if (remove_curse())
			{
				msg_print("You feel as if someone is watching over you.");
				*ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_STAR_REMOVE_CURSE:
		{
			remove_all_curse();
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_ENCHANT_ARMOR:
		{
			*ident = TRUE;
			if (!enchant_spell(0, 0, 1)) used_up = FALSE;
			break;
		}

		case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
		{
			if (!enchant_spell(1, 0, 0)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
		{
			if (!enchant_spell(0, 1, 0)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_ARMOR:
		{
			if (!enchant_spell(0, 0, randint(3) + 2)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_WEAPON:
		{
			if (!enchant_spell(randint(3), randint(3), 0)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_RECHARGING:
		{
			if (!recharge(60)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_LIGHT:
		{
			if (lite_area(NdS(2, 8), 2)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_MAPPING:
		{
			map_area();
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_GOLD:
		{
			if (detect_treasure()) *ident = TRUE;
			if (detect_objects_gold()) *ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_ITEM:
		{
			if (detect_objects_normal()) *ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_TRAP:
		{
			if (detect_traps()) *ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_DOOR:
		{
			if (detect_doors()) *ident = TRUE;
			if (detect_stairs()) *ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_INVIS:
		{
			if (detect_monsters_invis()) *ident = TRUE;
			break;
		}

		case SV_SCROLL_SATISFY_HUNGER:
		{
			if (set_food(PY_FOOD_MAX - 1)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_BLESSING:
		{
			if (p_ptr->inc_timed<TMD_BLESSED>(randint(12) + 6)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_CHANT:
		{
			if (p_ptr->inc_timed<TMD_BLESSED>(randint(24) + 12)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_PRAYER:
		{
			if (p_ptr->inc_timed<TMD_BLESSED>(randint(48) + 24)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_MONSTER_CONFUSION:
		{
			if (p_ptr->confusing == 0)
			{
				msg_print("Your hands begin to glow.");
				p_ptr->confusing = TRUE;
				*ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_PROTECTION_FROM_EVIL:
		{
			k = 3 * p_ptr->lev;
			if (p_ptr->inc_timed<TMD_PROTEVIL>(randint(25) + k)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_RUNE_OF_PROTECTION:
		{
			warding_glyph();
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_TRAP_DOOR_DESTRUCTION:
		{
			if (destroy_doors_touch()) *ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_DESTRUCTION:
		{
			destroy_area(p_ptr->loc, 15, TRUE);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_DISPEL_UNDEAD:
		{
			if (dispel_undead(60)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_BANISHMENT:
		{
			if (!banishment()) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_MASS_BANISHMENT:
		{
			(void)mass_banishment();
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_ACQUIREMENT:
		{
			acquirement(p_ptr->loc, 1, TRUE);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ACQUIREMENT:
		{
			acquirement(p_ptr->loc, randint(2) + 1, TRUE);
			*ident = TRUE;
			break;
		}
	}

	return (used_up);
}


static bool use_staff(object_type *o_ptr, bool *ident)
{
	int k;

	bool use_charge = TRUE;

	/* Analyze the staff */
	switch (o_ptr->obj_id.sval)
	{
		case SV_STAFF_DARKNESS:
		{
			if (!p_ptr->resist_blind)
			{
				if (p_ptr->inc_timed<TMD_BLIND>(3 + randint(5))) *ident = TRUE;
			}
			if (unlite_area(10, 3)) *ident = TRUE;
			break;
		}

		case SV_STAFF_SLOWNESS:
		{
			if (p_ptr->inc_timed<TMD_SLOW>(randint(30) + 15)) *ident = TRUE;
			break;
		}

		case SV_STAFF_HASTE_MONSTERS:
		{
			if (speed_monsters()) *ident = TRUE;
			break;
		}

		case SV_STAFF_SUMMONING:
		{
			sound(MSG_SUM_MONSTER);
			for (k = 0; k < randint(4); k++)
			{
				if (summon_specific(p_ptr->loc, p_ptr->depth, 0))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_STAFF_TELEPORTATION:
		{
			teleport_player(100);
			*ident = TRUE;
			break;
		}

		case SV_STAFF_IDENTIFY:
		{
			if (!ident_spell()) use_charge = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_STAFF_REMOVE_CURSE:
		{
			if (remove_curse())
			{
				if (!p_ptr->timed[TMD_BLIND])
				{
					msg_print("The staff glows blue for a moment...");
				}
				*ident = TRUE;
			}
			break;
		}

		case SV_STAFF_STARLITE:
		{
			if (!p_ptr->timed[TMD_BLIND])
			{
				msg_print("The end of the staff glows brightly...");
			}
			for (k = 0; k < KEYPAD_DIR_MAX; k++) lite_line(ddd[k]);
			*ident = TRUE;
			break;
		}

		case SV_STAFF_LITE:
		{
			if (lite_area(NdS(2, 8), 2)) *ident = TRUE;
			break;
		}

		case SV_STAFF_MAPPING:
		{
			map_area();
			*ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_GOLD:
		{
			if (detect_treasure()) *ident = TRUE;
			if (detect_objects_gold()) *ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_ITEM:
		{
			if (detect_objects_normal()) *ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_TRAP:
		{
			if (detect_traps()) *ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_DOOR:
		{
			if (detect_doors()) *ident = TRUE;
			if (detect_stairs()) *ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_INVIS:
		{
			if (detect_monsters_invis()) *ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_EVIL:
		{
			if (detect_monsters_evil()) *ident = TRUE;
			break;
		}

		case SV_STAFF_CURE_LIGHT:
		{
			if (hp_player(randint(8))) *ident = TRUE;
			break;
		}

		case SV_STAFF_CURING:
		{
			if (p_ptr->clear_timed<TMD_BLIND>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_POISONED>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_CONFUSED>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_STUN>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_CUT>()) *ident = TRUE;
			break;
		}

		case SV_STAFF_HEALING:
		{
			if (hp_player(300)) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_STUN>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_CUT>()) *ident = TRUE;
			break;
		}

		case SV_STAFF_THE_MAGI:
		{
			if (do_res_stat(A_INT)) *ident = TRUE;
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				*ident = TRUE;
				msg_print("Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
			}
			break;
		}

		case SV_STAFF_SLEEP_MONSTERS:
		{
			if (sleep_monsters()) *ident = TRUE;
			break;
		}

		case SV_STAFF_SLOW_MONSTERS:
		{
			if (slow_monsters()) *ident = TRUE;
			break;
		}

		case SV_STAFF_SPEED:
		{
			if (!p_ptr->timed[TMD_FAST])
			{
				if (p_ptr->inc_timed<TMD_FAST>(randint(30) + 15)) *ident = TRUE;
			}
			else
			{
				(void)p_ptr->inc_timed<TMD_FAST>(5);
			}
			break;
		}

		case SV_STAFF_PROBING:
		{
			probing();
			*ident = TRUE;
			break;
		}

		case SV_STAFF_DISPEL_EVIL:
		{
			if (dispel_evil(60)) *ident = TRUE;
			break;
		}

		case SV_STAFF_POWER:
		{
			if (dispel_monsters(120)) *ident = TRUE;
			break;
		}

		case SV_STAFF_HOLINESS:
		{
			if (dispel_evil(120)) *ident = TRUE;
			k = 3 * p_ptr->lev;
			if (p_ptr->inc_timed<TMD_PROTEVIL>(randint(25) + k)) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_POISONED>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_AFRAID>()) *ident = TRUE;
			if (hp_player(50)) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_STUN>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_CUT>()) *ident = TRUE;
			break;
		}

		case SV_STAFF_BANISHMENT:
		{
			if (!banishment()) use_charge = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_STAFF_EARTHQUAKES:
		{
			earthquake(p_ptr->loc, 10);
			*ident = TRUE;
			break;
		}

		case SV_STAFF_DESTRUCTION:
		{
			destroy_area(p_ptr->loc, 15, TRUE);
			*ident = TRUE;
			break;
		}
	}

	return (use_charge);
}


static bool aim_wand(object_type *o_ptr, bool *ident, int dir)
{
	int sval = o_ptr->obj_id.sval;

	/* XXX Hack -- Wand of wonder can do anything before it */
	if (sval == SV_WAND_WONDER) sval = rand_int(SV_WAND_WONDER);

	/* Analyze the wand */
	switch (sval)
	{
		case SV_WAND_HEAL_MONSTER:
		{
			if (heal_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_HASTE_MONSTER:
		{
			if (speed_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_CLONE_MONSTER:
		{
			if (clone_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_TELEPORT_AWAY:
		{
			if (teleport_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_DISARMING:
		{
			if (disarm_trap(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_TRAP_DOOR_DEST:
		{
			if (destroy_door(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_STONE_TO_MUD:
		{
			if (wall_to_mud(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_LITE:
		{
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir);
			*ident = TRUE;
			break;
		}

		case SV_WAND_SLEEP_MONSTER:
		{
			if (sleep_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_SLOW_MONSTER:
		{
			if (slow_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_CONFUSE_MONSTER:
		{
			if (confuse_monster(dir, 10)) *ident = TRUE;
			break;
		}

		case SV_WAND_FEAR_MONSTER:
		{
			if (fear_monster(dir, 10)) *ident = TRUE;
			break;
		}

		case SV_WAND_DRAIN_LIFE:
		{
			if (drain_life(dir, 150)) *ident = TRUE;
			break;
		}

		case SV_WAND_POLYMORPH:
		{
			if (poly_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_STINKING_CLOUD:
		{
			fire_ball(GF_POIS, dir, 12, 2);
			*ident = TRUE;
			break;
		}

		case SV_WAND_MAGIC_MISSILE:
		{
			fire_bolt_or_beam(20, GF_MISSILE, dir, NdS(3, 4));
			*ident = TRUE;
			break;
		}

		case SV_WAND_ACID_BOLT:
		{
			fire_bolt_or_beam(20, GF_ACID, dir, NdS(10, 8));
			*ident = TRUE;
			break;
		}

		case SV_WAND_ELEC_BOLT:
		{
			fire_bolt_or_beam(20, GF_ELEC, dir, NdS(6, 6));
			*ident = TRUE;
			break;
		}

		case SV_WAND_FIRE_BOLT:
		{
			fire_bolt_or_beam(20, GF_FIRE, dir, NdS(12, 8));
			*ident = TRUE;
			break;
		}

		case SV_WAND_COLD_BOLT:
		{
			fire_bolt_or_beam(20, GF_COLD, dir, NdS(6, 8));
			*ident = TRUE;
			break;
		}

		case SV_WAND_ACID_BALL:
		{
			fire_ball(GF_ACID, dir, 120, 2);
			*ident = TRUE;
			break;
		}

		case SV_WAND_ELEC_BALL:
		{
			fire_ball(GF_ELEC, dir, 64, 2);
			*ident = TRUE;
			break;
		}

		case SV_WAND_FIRE_BALL:
		{
			fire_ball(GF_FIRE, dir, 144, 2);
			*ident = TRUE;
			break;
		}

		case SV_WAND_COLD_BALL:
		{
			fire_ball(GF_COLD, dir, 96, 2);
			*ident = TRUE;
			break;
		}

		case SV_WAND_WONDER:
		{
			msg_print("Oops.  Wand of wonder activated.");
			break;
		}

		case SV_WAND_DRAGON_FIRE:
		{
			fire_ball(GF_FIRE, dir, 200, 3);
			*ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_COLD:
		{
			fire_ball(GF_COLD, dir, 160, 3);
			*ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_BREATH:
		{
			static const byte wand_dragon_breath_damage[5] = {200,160,200,160,120};
			static const byte wand_dragon_breath_type[5] = {GF_ACID,GF_ELEC,GF_FIRE,GF_COLD,GF_POIS};
			const int i = rand_int(5);

			fire_ball(wand_dragon_breath_type[i],dir,wand_dragon_breath_damage[i],3);

			*ident = TRUE;
			break;
		}

		case SV_WAND_ANNIHILATION:
		{
			if (drain_life(dir, 250)) *ident = TRUE;
			break;
		}
	}

	return (TRUE);
}


static bool zap_rod(object_type *o_ptr, bool *ident, int dir)
{
	bool used_charge = TRUE;
	object_kind *k_ptr = &object_type::k_info[o_ptr->k_idx];

	/* Analyze the rod */
	switch (o_ptr->obj_id.sval)
	{
		case SV_ROD_DETECT_TRAP:
		{
			if (detect_traps()) *ident = TRUE;
			break;
		}

		case SV_ROD_DETECT_DOOR:
		{
			if (detect_doors()) *ident = TRUE;
			if (detect_stairs()) *ident = TRUE;
			break;
		}

		case SV_ROD_IDENTIFY:
		{
			*ident = TRUE;
			if (!ident_spell()) used_charge = FALSE;
			break;
		}

		case SV_ROD_RECALL:
		{
			set_recall();
			*ident = TRUE;
			break;
		}

		case SV_ROD_ILLUMINATION:
		{
			if (lite_area(NdS(2, 8), 2)) *ident = TRUE;
			break;
		}

		case SV_ROD_MAPPING:
		{
			map_area();
			*ident = TRUE;
			break;
		}

		case SV_ROD_DETECTION:
		{
			detect_all();
			*ident = TRUE;
			break;
		}

		case SV_ROD_PROBING:
		{
			probing();
			*ident = TRUE;
			break;
		}

		case SV_ROD_CURING:
		{
			if (p_ptr->clear_timed<TMD_BLIND>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_POISONED>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_CONFUSED>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_STUN>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_CUT>()) *ident = TRUE;
			break;
		}

		case SV_ROD_HEALING:
		{
			if (hp_player(500)) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_STUN>()) *ident = TRUE;
			if (p_ptr->clear_timed<TMD_CUT>()) *ident = TRUE;
			break;
		}

		case SV_ROD_RESTORATION:
		{
			if (restore_level()) *ident = TRUE;
			if (do_res_stat(A_STR)) *ident = TRUE;
			if (do_res_stat(A_INT)) *ident = TRUE;
			if (do_res_stat(A_WIS)) *ident = TRUE;
			if (do_res_stat(A_DEX)) *ident = TRUE;
			if (do_res_stat(A_CON)) *ident = TRUE;
			if (do_res_stat(A_CHR)) *ident = TRUE;
			break;
		}

		case SV_ROD_SPEED:
		{
			if (!p_ptr->timed[TMD_FAST])
			{
				if (p_ptr->inc_timed<TMD_FAST>(randint(30) + 15)) *ident = TRUE;
			}
			else
			{
				(void)p_ptr->inc_timed<TMD_FAST>(5);
			}
			break;
		}

		case SV_ROD_TELEPORT_AWAY:
		{
			if (teleport_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_ROD_DISARMING:
		{
			if (disarm_trap(dir)) *ident = TRUE;
			break;
		}

		case SV_ROD_LITE:
		{
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir);
			*ident = TRUE;
			break;
		}

		case SV_ROD_SLEEP_MONSTER:
		{
			if (sleep_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_ROD_SLOW_MONSTER:
		{
			if (slow_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_ROD_DRAIN_LIFE:
		{
			if (drain_life(dir, 150)) *ident = TRUE;
			break;
		}

		case SV_ROD_POLYMORPH:
		{
			if (poly_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_ROD_ACID_BOLT:
		{
			fire_bolt_or_beam(10, GF_ACID, dir, NdS(12, 8));
			*ident = TRUE;
			break;
		}

		case SV_ROD_ELEC_BOLT:
		{
			fire_bolt_or_beam(10, GF_ELEC, dir, NdS(6, 6));
			*ident = TRUE;
			break;
		}

		case SV_ROD_FIRE_BOLT:
		{
			fire_bolt_or_beam(10, GF_FIRE, dir, NdS(16, 8));
			*ident = TRUE;
			break;
		}

		case SV_ROD_COLD_BOLT:
		{
			fire_bolt_or_beam(10, GF_COLD, dir, NdS(10, 8));
			*ident = TRUE;
			break;
		}

		case SV_ROD_ACID_BALL:
		{
			fire_ball(GF_ACID, dir, 120, 2);
			*ident = TRUE;
			break;
		}

		case SV_ROD_ELEC_BALL:
		{
			fire_ball(GF_ELEC, dir, 64, 2);
			*ident = TRUE;
			break;
		}

		case SV_ROD_FIRE_BALL:
		{
			fire_ball(GF_FIRE, dir, 144, 2);
			*ident = TRUE;
			break;
		}

		case SV_ROD_COLD_BALL:
		{
			fire_ball(GF_COLD, dir, 96, 2);
			*ident = TRUE;
			break;
		}
	}

	/* XXX initialize key info at this time XXX */
	/* should be done in init1.c */
	k_ptr->time.base = k_ptr->pval;
	k_ptr->time.range.clear();

	return TRUE;
}


/*
 * Activate a wielded object.  Wielded objects never stack.
 * And even if they did, activatable objects never stack.
 *
 * Currently, only (some) artifacts, and Dragon Scale Mail, can be activated.
 * But one could, for example, easily make an activatable "Ring of Plasma".
 *
 * Note that it always takes a turn to activate an artifact, even if
 * the user hits "escape" at the "direction" prompt.
 */
static bool activate_object(object_type *o_ptr, int dir)
{
	int k, i, chance;

	/* Artifacts */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &object_type::a_info[o_ptr->name1];
		char o_name[80];

		/* Get the basic name of the object */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, ODESC_BASE);

		switch (a_ptr->activation)
		{
			case ACT_ILLUMINATION:
			{
				msg_format("The %s wells with clear light...", o_name);
				lite_area(NdS(2, 15), 3);
				break;
			}

			case ACT_MAGIC_MAP:
			{
				msg_format("The %s shines brightly...", o_name);
				map_area();
				break;
			}

			case ACT_CLAIRVOYANCE:
			{
				msg_format("The %s glows a deep green...", o_name);
				wiz_lite();
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case ACT_PROT_EVIL:
			{
				msg_format("The %s lets out a shrill wail...", o_name);
				k = 3 * p_ptr->lev;
				(void)p_ptr->inc_timed<TMD_PROTEVIL>(randint(25) + k);
				break;
			}

			case ACT_DISP_EVIL:
			{
				msg_format("The %s floods the area with goodness...", o_name);
				dispel_evil(p_ptr->lev * 5);
				break;
			}

			case ACT_HASTE2:
			{
				msg_format("The %s glows brightly...", o_name);
				if (!p_ptr->timed[TMD_FAST])
				{
					(void)p_ptr->inc_timed<TMD_FAST>(randint(75) + 75);
				}
				else
				{
					(void)p_ptr->inc_timed<TMD_FAST>(5);
				}
				break;
			}

			case ACT_FIRE3:
			{
				msg_format("The %s glows deep red...", o_name);
				fire_ball(GF_FIRE, dir, 120, 3);
				break;
			}

			case ACT_FROST5:
			{
				msg_format("The %s glows bright white...", o_name);
				fire_ball(GF_COLD, dir, 200, 3);
				break;
			}

			case ACT_ELEC2:
			{
				msg_format("The %s glows deep blue...", o_name);
				fire_ball(GF_ELEC, dir, 250, 3);
				break;
			}

			case ACT_BIZZARE:
			{
				msg_format("The %s glows intensely black...", o_name);
				ring_of_power(dir);
				break;
			}


			case ACT_STAR_BALL:
			{
				msg_format("Your %s is surrounded by lightning...", o_name);
				for (i = 0; i < KEYPAD_DIR_MAX; i++) fire_ball(GF_ELEC, ddd[i], 150, 3);
				break;
			}

			case ACT_RAGE_BLESS_RESIST:
			{
				msg_format("Your %s glows many colours...", o_name);
				(void)hp_player(30);
				(void)p_ptr->clear_timed<TMD_AFRAID>();
				(void)p_ptr->inc_timed<TMD_SHERO>(randint(50) + 50);
				(void)p_ptr->inc_timed<TMD_BLESSED>(randint(50) + 50);
				(void)p_ptr->inc_timed<TMD_OPP_ACID>(randint(50) + 50);
				(void)p_ptr->inc_timed<TMD_OPP_ELEC>(randint(50) + 50);
				(void)p_ptr->inc_timed<TMD_OPP_FIRE>(randint(50) + 50);
				(void)p_ptr->inc_timed<TMD_OPP_COLD>(randint(50) + 50);
				(void)p_ptr->inc_timed<TMD_OPP_POIS>(randint(50) + 50);
				break;
			}

			case ACT_HEAL2:
			{
				msg_format("Your %s glows a bright white...", o_name);
				msg_print("You feel much better...");
				(void)hp_player(1000);
				(void)p_ptr->clear_timed<TMD_CUT>();
				break;
			}

			case ACT_PHASE:
			{
				msg_format("Your %s twists space around you...", o_name);
				teleport_player(10);
				break;
			}

			case ACT_BANISHMENT:
			{
				msg_format("Your %s glows deep blue...", o_name);
				if (!banishment()) return FALSE;
				break;
			}

			case ACT_TRAP_DOOR_DEST:
			{
				msg_format("Your %s glows bright red...", o_name);
				destroy_doors_touch();
				break;
			}

			case ACT_DETECT:
			{
				msg_format("Your %s glows bright white...", o_name);
				msg_print("An image forms in your mind...");
				detect_all();
				break;
			}

			case ACT_HEAL1:
			{
				msg_format("Your %s glows deep blue...", o_name);
				msg_print("You feel a warm tingling inside...");
				(void)hp_player(500);
				(void)p_ptr->clear_timed<TMD_CUT>();
				break;
			}

			case ACT_RESIST:
			{
				msg_format("Your %s glows many colours...", o_name);
				(void)p_ptr->inc_timed<TMD_OPP_ACID>(randint(20) + 20);
				(void)p_ptr->inc_timed<TMD_OPP_ELEC>(randint(20) + 20);
				(void)p_ptr->inc_timed<TMD_OPP_FIRE>(randint(20) + 20);
				(void)p_ptr->inc_timed<TMD_OPP_COLD>(randint(20) + 20);
				(void)p_ptr->inc_timed<TMD_OPP_POIS>(randint(20) + 20);
				break;
			}

			case ACT_SLEEP:
			{
				msg_format("Your %s glows deep blue...", o_name);
				sleep_monsters_touch();
				break;
			}

			case ACT_RECHARGE1:
			{
				msg_format("Your %s glows bright yellow...", o_name);
				if (!recharge(60)) return FALSE;
				break;
			}

			case ACT_TELEPORT:
			{
				msg_format("Your %s twists space around you...", o_name);
				teleport_player(100);
				break;
			}

			case ACT_RESTORE_LIFE:
			{
				msg_format("Your %s glows a deep red...", o_name);
				restore_level();
				break;
			}

			case ACT_MISSILE:
			{
				msg_format("Your %s glows extremely brightly...", o_name);
				fire_bolt(GF_MISSILE, dir, NdS(2, 6));
				break;
			}

			case ACT_FIRE1:
			{
				msg_format("Your %s is covered in fire...", o_name);
				fire_bolt(GF_FIRE, dir, NdS(9, 8));
				break;
			}

			case ACT_FROST1:
			{
				msg_format("Your %s is covered in frost...", o_name);
				fire_bolt(GF_COLD, dir, NdS(6, 8));
				break;
			}

			case ACT_LIGHTNING_BOLT:
			{
				msg_format("Your %s is covered in sparks...", o_name);
				fire_bolt(GF_ELEC, dir, NdS(4, 8));
				break;
			}

			case ACT_ACID1:
			{
				msg_format("Your %s is covered in acid...", o_name);
				fire_bolt(GF_ACID, dir, NdS(5, 8));
				break;
			}

			case ACT_ARROW:
			{
				msg_format("Your %s grows magical spikes...", o_name);
				fire_bolt(GF_ARROW, dir, 150);
				break;
			}

			case ACT_HASTE1:
			{
				msg_format("Your %s glows bright green...", o_name);
				if (!p_ptr->timed[TMD_FAST])
				{
					(void)p_ptr->inc_timed<TMD_FAST>(randint(20) + 20);
				}
				else
				{
					(void)p_ptr->inc_timed<TMD_FAST>(5);
				}
				break;
			}

			case ACT_REM_FEAR_POIS:
			{
				msg_format("Your %s glows deep blue...", o_name);
				(void)p_ptr->clear_timed<TMD_AFRAID>();
				(void)p_ptr->clear_timed<TMD_POISONED>();
				break;
			}

			case ACT_STINKING_CLOUD:
			{
				msg_format("Your %s throbs deep green...", o_name);
				fire_ball(GF_POIS, dir, 12, 3);
				break;
			}

			case ACT_FROST2:
			{
				msg_format("Your %s is covered in frost...", o_name);
				fire_ball(GF_COLD, dir, 48, 2);
				break;
			}

			case ACT_FROST4:
			{
				msg_format("Your %s glows a pale blue...", o_name);
				fire_bolt(GF_COLD, dir, NdS(12, 8));
				break;
			}

			case ACT_FROST3:
			{
				msg_format("Your %s glows a intense blue...", o_name);
				fire_ball(GF_COLD, dir, 100, 2);
				break;
			}

			case ACT_FIRE2:
			{
				msg_format("Your %s rages in fire...", o_name);
				fire_ball(GF_FIRE, dir, 72, 2);
				break;
			}

			case ACT_DRAIN_LIFE2:
			{
				msg_format("Your %s glows black...", o_name);
				drain_life(dir, 120);
				break;
			}

			case ACT_STONE_TO_MUD:
			{
				msg_format("Your %s pulsates...", o_name);
				wall_to_mud(dir);
				break;
			}

			case ACT_MASS_BANISHMENT:
			{
				msg_format("Your %s lets out a long, shrill note...", o_name);
				(void)mass_banishment();
				break;
			}

			case ACT_CURE_WOUNDS:
			{
				msg_format("Your %s radiates deep purple...", o_name);
				hp_player(NdS(4, 8));
				(void)p_ptr->set_timed<TMD_CUT>((p_ptr->timed[TMD_CUT] / 2) - 50);
				break;
			}

			case ACT_TELE_AWAY:
			{
				msg_format("Your %s glows deep red...", o_name);
				teleport_monster(dir);
				break;
			}

			case ACT_WOR:
			{
				msg_format("Your %s glows soft white...", o_name);
				set_recall();
				break;
			}

			case ACT_CONFUSE:
			{
				msg_format("Your %s glows in scintillating colours...", o_name);
				confuse_monster(dir, 20);
				break;
			}

			case ACT_IDENTIFY:
			{
				msg_format("Your %s glows yellow...", o_name);
				if (!ident_spell()) return FALSE;
				break;
			}

			case ACT_PROBE:
			{
				msg_format("Your %s glows brightly...", o_name);
				probing();
				break;
			}

			case ACT_DRAIN_LIFE1:
			{
				msg_format("Your %s glows white...", o_name);
				drain_life(dir, 90);
				break;
			}

			case ACT_FIREBRAND:
			{
				msg_format("Your %s glows deep red...", o_name);
				if (!brand_bolts()) return FALSE;
				break;
			}

			case ACT_STARLIGHT:
			{
				msg_format("Your %s glows with the light of a thousand stars...", o_name);
				for (k = 0; k < KEYPAD_DIR_MAX; k++) strong_lite_line(ddd[k]);
				break;
			}

			case ACT_MANA_BOLT:
			{
				msg_format("Your %s glows white...", o_name);
				fire_bolt(GF_MANA, dir, NdS(12, 8));
				break;
			}

			case ACT_BERSERKER:
			{
				msg_format("Your %s glows in anger...", o_name);
				p_ptr->inc_timed<TMD_SHERO>(randint(50) + 50);
				break;
			}
		}

		/* Window stuff */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

		/* Done */
		return TRUE;
	}


	/* Hack -- Dragon Scale Mail can be activated as well */
	if (o_ptr->obj_id.tval == TV_DRAG_ARMOR)
	{
		object_kind* k_ptr = &object_type::k_info[o_ptr->k_idx];	/* XXX prepare to set timeout info XXX */

		/* Branch on the sub-type */
		switch (o_ptr->obj_id.sval)
		{
			case SV_DRAGON_BLUE:
			{
				sound(MSG_BR_ELEC);
				msg_print("You breathe lightning.");
				fire_ball(GF_ELEC, dir, 100, 2);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(450,1,450);
				break;
			}

			case SV_DRAGON_WHITE:
			{
				sound(MSG_BR_FROST);
				msg_print("You breathe frost.");
				fire_ball(GF_COLD, dir, 110, 2);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(450,1,450);
				break;
			}

			case SV_DRAGON_BLACK:
			{
				sound(MSG_BR_ACID);
				msg_print("You breathe acid.");
				fire_ball(GF_ACID, dir, 130, 2);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(450,1,450);
				break;
			}

			case SV_DRAGON_GREEN:
			{
				sound(MSG_BR_GAS);
				msg_print("You breathe poison gas.");
				fire_ball(GF_POIS, dir, 150, 2);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(450,1,450);
				break;
			}

			case SV_DRAGON_RED:
			{
				sound(MSG_BR_FIRE);
				msg_print("You breathe fire.");
				fire_ball(GF_FIRE, dir, 200, 2);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(450,1,450);
				break;
			}

			case SV_DRAGON_MULTIHUED:
			{
				chance = rand_int(5);
				sound(     ((chance == 1) ? MSG_BR_ELEC :
				            ((chance == 2) ? MSG_BR_FROST :
				             ((chance == 3) ? MSG_BR_ACID :
				              ((chance == 4) ? MSG_BR_GAS : MSG_BR_FIRE)))));
				msg_format("You breathe %s.",
				           ((chance == 1) ? "lightning" :
				            ((chance == 2) ? "frost" :
				             ((chance == 3) ? "acid" :
				              ((chance == 4) ? "poison gas" : "fire")))));
				fire_ball(((chance == 1) ? GF_ELEC :
				           ((chance == 2) ? GF_COLD :
				            ((chance == 3) ? GF_ACID :
				             ((chance == 4) ? GF_POIS : GF_FIRE)))),
				          dir, 250, 2);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(225,1,225);
				break;
			}

			case SV_DRAGON_BRONZE:
			{
				sound(MSG_BR_CONF);
				msg_print("You breathe confusion.");
				fire_ball(GF_CONFUSION, dir, 120, 2);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(450,1,450);
				break;
			}

			case SV_DRAGON_GOLD:
			{
				sound(MSG_BR_SOUND);
				msg_print("You breathe sound.");
				fire_ball(GF_SOUND, dir, 130, 2);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(450,1,450);
				break;
			}

			case SV_DRAGON_CHAOS:
			{
				chance = rand_int(2);
				sound(((chance == 1 ? MSG_BR_CHAOS : MSG_BR_DISENCHANT)));
				msg_format("You breathe %s.",
				           ((chance == 1 ? "chaos" : "disenchantment")));
				fire_ball((chance == 1 ? GF_CHAOS : GF_DISENCHANT),
				          dir, 220, 2);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(300,1,300);
				break;
			}

			case SV_DRAGON_LAW:
			{
				chance = rand_int(2);
				sound(((chance == 1 ? MSG_BR_SOUND : MSG_BR_SHARDS)));
				msg_format("You breathe %s.",
				           ((chance == 1 ? "sound" : "shards")));
				fire_ball((chance == 1 ? GF_SOUND : GF_SHARD),
				          dir, 230, 2);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(300,1,300);
				break;
			}

			case SV_DRAGON_BALANCE:
			{
				chance = rand_int(4);
				msg_format("You breathe %s.",
				           ((chance == 1) ? "chaos" :
				            ((chance == 2) ? "disenchantment" :
				             ((chance == 3) ? "sound" : "shards"))));
				fire_ball(((chance == 1) ? GF_CHAOS :
				           ((chance == 2) ? GF_DISENCHANT :
				            ((chance == 3) ? GF_SOUND : GF_SHARD))),
				          dir, 250, 2);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(300,1,300);
				break;
			}

			case SV_DRAGON_SHINING:
			{
				chance = rand_int(2);
				sound(((chance == 0 ? MSG_BR_LIGHT : MSG_BR_DARK)));
				msg_format("You breathe %s.",
				           ((chance == 0 ? "light" : "darkness")));
				fire_ball((chance == 0 ? GF_LITE : GF_DARK), dir, 200, 2);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(300,1,300);
				break;
			}

			case SV_DRAGON_POWER:
			{
				sound(MSG_BR_ELEMENTS);
				msg_print("You breathe the elements.");
				fire_ball(GF_MISSILE, dir, 300, 2);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(300,1,300);
				break;
			}
		}

		/* Window stuff */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

		/* Success */
		return TRUE;
	}

	/* Hack -- some Rings can be activated for double resist and element ball */
	if (o_ptr->obj_id.tval == TV_RING)
	{
		object_kind* k_ptr = &object_type::k_info[o_ptr->k_idx];	/* XXX prepare to set timeout info XXX */

		/* Branch on the sub-type */
		switch (o_ptr->obj_id.sval)
		{
			case SV_RING_ACID:
			{
				fire_ball(GF_ACID, dir, 70, 2);
				p_ptr->inc_timed<TMD_OPP_ACID>(randint(20) + 20);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(50,1,50);
				break;
			}

			case SV_RING_FLAMES:
			{
				fire_ball(GF_FIRE, dir, 80, 2);
				p_ptr->inc_timed<TMD_OPP_FIRE>(randint(20) + 20);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(50,1,50);
				break;
			}

			case SV_RING_ICE:
			{
				fire_ball(GF_COLD, dir, 75, 2);
				p_ptr->inc_timed<TMD_OPP_COLD>(randint(20) + 20);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(50,1,50);
				break;
			}

			case SV_RING_LIGHTNING:
			{
				fire_ball(GF_ELEC, dir, 85, 2);
				p_ptr->inc_timed<TMD_OPP_ELEC>(randint(20) + 20);
				/* XXX set timeout info here XXX */
				k_ptr->time.set(50,1,50);
				break;
			}
		}

		/* Window stuff */
		p_ptr->redraw |= (PR_EQUIP);

		/* Success */
		return TRUE;
	}

	/* Mistake */
	msg_print("Oops.  That object cannot be activated.");

	/* Not used up */
	return (FALSE);
}


static bool want_aim(object_type *o_ptr)
{
	if (o_ptr->obj_id.tval == TV_WAND) return TRUE;
	if (o_ptr->obj_id.tval == TV_ROD && ((o_ptr->obj_id.sval >= SV_ROD_MIN_DIRECTION) || !o_ptr->aware())) return TRUE;
	if (o_ptr->name1)
	{
		switch (object_type::a_info[o_ptr->name1].activation)
		{
			case ACT_FIRE3:
			case ACT_FROST5:
			case ACT_ELEC2:
			case ACT_BIZZARE:
			case ACT_MISSILE:
			case ACT_FIRE1:
			case ACT_FROST1:
			case ACT_LIGHTNING_BOLT:
			case ACT_ACID1:
			case ACT_ARROW:
			case ACT_STINKING_CLOUD:
			case ACT_FROST2:
			case ACT_FROST4:
			case ACT_FROST3:
			case ACT_FIRE2:
			case ACT_DRAIN_LIFE2:
			case ACT_STONE_TO_MUD:
			case ACT_TELE_AWAY:
			case ACT_CONFUSE:
			case ACT_DRAIN_LIFE1:
			case ACT_MANA_BOLT:	return TRUE;
			default: return FALSE;
		}
	}
	
	if (o_ptr->obj_id.tval == TV_DRAG_ARMOR) return TRUE;
	if (o_ptr->obj_id.tval == TV_RING) return TRUE;

	return FALSE;
}

/*
 * Use an object the right way.
 *
 * There may be a BIG problem with any "effect" that can cause "changes"
 * to the inventory.  For example, a "scroll of recharging" can cause
 * a wand/staff to "disappear", moving the inventory up.  Luckily, the
 * scrolls all appear BEFORE the staffs/wands, so this is not a problem.
 * But, for example, a "staff of recharging" could cause MAJOR problems.
 * In such a case, it will be best to either (1) "postpone" the effect
 * until the end of the function, or (2) "change" the effect, say, into
 * giving a staff "negative" charges, or "turning a staff into a stick".
 * It seems as though a "rod of recharging" might in fact cause problems.
 * The basic problem is that the act of recharging (and destroying) an
 * item causes the inducer of that action to "move", causing "o_ptr" to
 * no longer point at the correct item, with horrifying results.
 */
void do_cmd_use(object_type *o_ptr, int item, int snd, use_type _use)
{	/* mock up old environment for this */
	int dir = 5;
	bool used;
	bool ident = FALSE;

	if (want_aim(o_ptr))
	{
		/* Get a direction, allow cancel */
		if (!get_aim_dir(&dir))
			return;
	}

	/* Use energy regardless of failure */	
	p_ptr->energy_use = 100;

	/* Check for use */
	if (_use == USE_CHARGE || _use == USE_TIMEOUT)
	{
		if (!check_devices(o_ptr))
			return;
	}

	/* Check for timeout */
	if (_use == USE_TIMEOUT)
	{
		if (TV_ROD == o_ptr->obj_id.tval)
		{
			if (o_ptr->timeout > (o_ptr->pval - object_type::k_info[o_ptr->k_idx].pval))
			{
				if (OPTION(flush_failure)) flush();

				if (o_ptr->number == 1)
					msg_print("The rod is still charging");
				else
					msg_print("The rods are all still charging");

				return;
			}
		}
		else
		{
			if (o_ptr->timeout)
			{
				msg_print("It whines, glows and fades...");
				return;
			}
		}
	}

	/* Special message for artifacts */
	if (o_ptr->is_artifact())
	{
		message(snd, 0, "You activate it.");
	}
	else
	{
		/* Make a noise! */
		sound(snd);
	}

	/* Analyze the object */
	switch (o_ptr->obj_id.tval)
	{
		case TV_FOOD:
		{
			used = eat_food(o_ptr, &ident);
			break;
		}

		case TV_POTION:
		{
			used = quaff_potion(o_ptr, &ident);
			break;
		}

		case TV_SCROLL:
		{
			used = read_scroll(o_ptr, &ident);
			break;
		}

		case TV_STAFF:
		{
			used = use_staff(o_ptr, &ident);
			break;
		}

		case TV_WAND:
		{
			used = aim_wand(o_ptr, &ident, dir);
			break;
		}

		case TV_ROD:
		{
			used = zap_rod(o_ptr, &ident, dir);
			break;
		}

		default:
		{
			used = activate_object(o_ptr, dir);
			break;
		}
	}

#if 0
	/* Do effect */
	used = do_effect(effect, &ident, dir, beam_chance(o_ptr->tval));

	/* Food feeds the player */
	if (o_ptr->tval == TV_FOOD || o_ptr->tval == TV_POTION)
		(void)set_food(p_ptr->food + o_ptr->pval);
#endif

	if (!used && !ident) return;

	/* Mark as tried and redisplay */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

	/*
	 * If the player becomes aware of the item's function, then mark it as
	 * aware and reward the player with some experience.  Otherwise, mark
	 * it as "tried".
	 */
	if (ident && !o_ptr->aware())
	{
		object_type::k_info[o_ptr->k_idx].aware = TRUE;
		gain_exp((object_type::k_info[o_ptr->k_idx].level + (p_ptr->lev / 2)) / p_ptr->lev);
//		p_ptr->notice |= PN_SQUELCH;
	}
	else
	{
		object_tried(o_ptr);
	}

	
	/* Some uses are "free" */
	if (!used) return;

	/* Chargeables act differently to single-used items when not used up */
	if (_use == USE_CHARGE)
	{
		/* Use a single charge */
		o_ptr->pval--;

		/* Describe charges */
		if (item >= 0)
			inven_item_charges(item);
		else
			floor_item_charges(0 - item);
	}
	else if (_use == USE_TIMEOUT)
	{
		/* Artifacts use their own special field */
		if (o_ptr->name1)
		{
			const artifact_type *a_ptr = &object_type::a_info[o_ptr->name1];
			o_ptr->timeout = a_ptr->time.damroll();
		}
		else
		{
			const object_kind *k_ptr = &object_type::k_info[o_ptr->k_idx];
			o_ptr->timeout += k_ptr->time.damroll();
		}
	}
	else if (_use == USE_SINGLE)
	{
		/* Destroy a potion in the pack */
		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}

		/* Destroy a potion on the floor */
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}
	}
}

static const char* const act_description[ACT_MAX] =
{
	"illumination",
	"magic mapping",
	"clairvoyance",
	"protection from evil",
	"dispel evil (x5)",
	"heal (500)",
	"heal (1000)",
	"cure wounds (4d8)",
	"haste self (20+d20 turns)",
	"haste self (75+d75 turns)",
	"fire bolt (9d8)",
	"fire ball (72)",
	"large fire ball (120)",
	"frost bolt (6d8)",
	"frost ball (48)",
	"frost ball (100)",
	"frost bolt (12d8)",
	"large frost ball (200)",
	"acid bolt (5d8)",
	"recharge item I",
	"sleep II",
	"lightning bolt (4d8)",
	"large lightning ball (250)",
	"banishment",
	"mass banishment",
	"identify",
	"drain life (90)",
	"drain life (120)",
	"bizarre things",
	"star ball (150)",
	"berserk rage, bless, and resistance",
	"phase door",
	"door and trap destruction",
	"detection",
	"resistance (20+d20 turns)",
	"teleport",
	"restore life levels",
	"magic missile (2d6)",
	"a magical arrow (150)",
	"remove fear and cure poison",
	"stinking cloud (12)",
	"stone to mud",
	"teleport away",
	"word of recall",
	"confuse monster",
	"probing",
	"fire branding of bolts",
	"starlight (10d8)",
	"mana bolt (12d8)",
	"berserk rage (50+d50 turns)"
};



/*
 * Determine the "Activation" (if any) for an artifact
 */
void describe_item_activation(const object_type *o_ptr)
{
	/* Require activation ability */
	if (!obj_has_activation(o_ptr)) return;

	/* Artifact activations */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &object_type::a_info[o_ptr->name1];

		/* Paranoia */
		if (a_ptr->activation >= ACT_MAX) return;

		/* Some artifacts can be activated */
		text_out(act_description[a_ptr->activation]);

		/* Output the number of turns */
		if (a_ptr->time.base)
		{
			if (0<a_ptr->time.range.maxroll())
			{
				if (1==a_ptr->time.range.dice)
				{
					text_out(format(" every %d+d%d turns", (int)(a_ptr->time.base), (int)(a_ptr->time.range.sides)));
				}
				else
				{
					text_out(format(" every %d+%dd%d turns", (int)(a_ptr->time.base), (int)(a_ptr->time.range.dice), (int)(a_ptr->time.range.sides)));
				}
			}
			else
			{
				text_out(format(" every %d turns", (int)(a_ptr->time.base)));
			}
		}
		else if (0<a_ptr->time.range.maxroll())
		{
			if (1==a_ptr->time.range.dice)
			{
				text_out(format(" every d%d turns", (int)(a_ptr->time.range.sides)));
			}
			else
			{
				text_out(format(" every %dd%d turns", (int)(a_ptr->time.range.dice), (int)(a_ptr->time.range.sides)));
			}
		}

		return;
	}

	/* Ring activations */
	if (TV_RING == o_ptr->obj_id.tval)
	{
		/* Branch on the sub-type */
		switch (o_ptr->obj_id.sval)
		{
			case SV_RING_ACID:
			{
				text_out("acid resistance (20+d20 turns) and acid ball (70) every 50+d50 turns");
				break;
			}
			case SV_RING_FLAMES:
			{
				text_out("fire resistance (20+d20 turns) and fire ball (80) every 50+d50 turns");
				break;
			}
			case SV_RING_ICE:
			{
				text_out("cold resistance (20+d20 turns) and cold ball (75) every 50+d50 turns");
				break;
			}

			case SV_RING_LIGHTNING:
			{
				text_out("electricity resistance (20+d20 turns) and electricity ball (85) every 50+d50 turns");
				break;
			}
		}

		return;
	}

	/* Require dragon scale mail */
	if (TV_DRAG_ARMOR != o_ptr->obj_id.tval) return;

	/* Branch on the sub-type */
	switch (o_ptr->obj_id.sval)
	{
		case SV_DRAGON_BLUE:
		{
			text_out("breathe lightning (100) every 450+d450 turns");
			break;
		}
		case SV_DRAGON_WHITE:
		{
			text_out("breathe frost (110) every 450+d450 turns");
			break;
		}
		case SV_DRAGON_BLACK:
		{
			text_out("breathe acid (130) every 450+d450 turns");
			break;
		}
		case SV_DRAGON_GREEN:
		{
			text_out("breathe poison gas (150) every 450+d450 turns");
			break;
		}
		case SV_DRAGON_RED:
		{
			text_out("breathe fire (200) every 450+d450 turns");
			break;
		}
		case SV_DRAGON_MULTIHUED:
		{
			text_out("breathe multi-hued (250) every 225+d225 turns");
			break;
		}
		case SV_DRAGON_BRONZE:
		{
			text_out("breathe confusion (120) every 450+d450 turns");
			break;
		}
		case SV_DRAGON_GOLD:
		{
			text_out("breathe sound (130) every 450+d450 turns");
			break;
		}
		case SV_DRAGON_CHAOS:
		{
			text_out("breathe chaos/disenchant (220) every 300+d300 turns");
			break;
		}
		case SV_DRAGON_LAW:
		{
			text_out("breathe sound/shards (230) every 300+d300 turns");
			break;
		}
		case SV_DRAGON_BALANCE:
		{
			text_out("breathe balance (250) every 300+d300 turns");
			break;
		}
		case SV_DRAGON_SHINING:
		{
			text_out("breathe light/darkness (200) every 300+d300 turns");
			break;
		}
		case SV_DRAGON_POWER:
		{
			text_out("breathe the elements (300) every 300+d300 turns");
			break;
		}
	}
}

