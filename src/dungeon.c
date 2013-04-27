/* File: dungeon.c */

/* Purpose: Angband game engine */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#define TY_CURSE_CHANCE 100
#define CHAINSWORD_NOISE 100


/*
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
static byte value_check_aux1(object_type *o_ptr)
{
	/* Artifacts */
	if (artifact_p(o_ptr) || o_ptr->art_name)
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return FEEL_TERRIBLE;

		/* Normal */
		return FEEL_SPECIAL;
	}

	/* Ego-Items */
	if (ego_item_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return FEEL_WORTHLESS;

		/* Normal */
		return FEEL_EXCELLENT;
	}

	/* Cursed items */
	if (cursed_p(o_ptr)) return FEEL_CURSED;

	/* Broken items */
	if (broken_p(o_ptr)) return FEEL_BROKEN;

	/* Good "armor" bonus */
	if (o_ptr->to_a > 0) return FEEL_GOOD;

	/* Good "weapon" bonus */
	if (o_ptr->to_h + o_ptr->to_d > 0) return FEEL_GOOD;

	/* Default to "average" */
	return FEEL_AVERAGE;
}


/*
 * Return a "feeling" (or NULL) about an item.  Method 2 (Light).
 */
static byte value_check_aux2(object_type *o_ptr)
{
	/* Cursed items (all of them) */
	if (cursed_p(o_ptr)) return FEEL_CURSED;

	/* Broken items (all of them) */
	if (broken_p(o_ptr)) return FEEL_BROKEN;

	/* Artifacts -- except cursed/broken ones */
	if (artifact_p(o_ptr) || o_ptr->art_name) return FEEL_UNCURSED;

	/* Ego-Items -- except cursed/broken ones */
	if (ego_item_p(o_ptr)) return FEEL_UNCURSED;

	/* Good armor bonus */
	if (o_ptr->to_a > 0) return FEEL_UNCURSED;

	/* Good weapon bonuses */
	if (o_ptr->to_h + o_ptr->to_d > 0) return FEEL_UNCURSED;

	/* No feeling */
	return FEEL_NONE;
}





/*
 * Sense the inventory
 *
 *   Class 0 = Warrior --> fast and heavy
 *   Class 1 = Mage    --> slow and light
 *   Class 2 = Priest  --> fast but light
 *   Class 3 = Rogue   --> okay and heavy
 *   Class 4 = Ranger  --> slow but heavy  (changed!)
 *   Class 5 = Paladin --> slow but heavy
 */
static void sense_inventory(void)
{
	int         i;
	int         plev = p_ptr->lev;
	bool        heavy = cp_ptr->s_inv_h;
	byte        feel;
	object_type *o_ptr;
	char        o_name[MAX_NLEN];

	/*** Check for "sensing" ***/

	/* No sensing when confused */
	if (p_ptr->confused) return;

	/* Analyze the class */
	switch (cp_ptr->s_inv)
	{
		case SENSE_CANT:
		{
			/* cannot sense */
			return;
		}

		case SENSE_VFST:
		{
			/* Good sensing */
			if (!one_in_(4000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case SENSE_FAST:
		{
			/* Good sensing */
			if (0 != randint0(9000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case SENSE_GOOD:
		{
			/* Okay sensing */
			if (!one_in_(20000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case SENSE_NORM:
		{
			/* Bad sensing */
			if (!one_in_(55000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case SENSE_SLOW:
		{
			/* Bad sensing */
			if (!one_in_(75000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case SENSE_BAD:
		{
			/* Bad sensing */
			if (!one_in_(95000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case SENSE_POOR:
		{
			/* Very bad (light) sensing */
			if (0 != randint0(240000L / (plev + 5))) return;

			/* Done */
			break;
		}
	}


	/*** Sense everything ***/

	/* Check everything */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		bool okay = FALSE;

		o_ptr = &inventory[i];

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

		/* Skip non-sense machines */
		if (!okay) continue;

		/* We know about it already, do not tell us again */
		if (o_ptr->ident & (IDENT_SENSE)) continue;

		/* It is fully known, no information needed */
		if (object_known_p(o_ptr)) continue;

		/* Occasional failure on inventory items */
		if ((i < INVEN_WIELD) && (0 != randint0(5))) continue;

		/* Check for a feeling */
		feel = (heavy ? value_check_aux1(o_ptr) : value_check_aux2(o_ptr));

		/* Skip non-feelings */
		if (!feel) continue;

		/* Stop everything */
		if (disturb_minor) disturb(0, 0);

		/* Get an object description */
		object_desc(o_name, o_ptr, OD_OMIT_PREFIX | OD_NAME_ONLY);

		/* Message (equipment) */
		if (i >= INVEN_WIELD)
		{
#ifdef JP
			msg_format("%s%s(%c)は%sという感じがする...",
					describe_use(i),o_name, index_to_label(i),game_inscriptions[feel]);
#else
			msg_format("You feel the %s (%c) you are %s %s %s...",
					o_name, index_to_label(i), describe_use(i),
					((o_ptr->number == 1) ? "is" : "are"),
					game_inscriptions[feel]);
#endif
            sound(SOUND_PSEUDOID);
		}

		/* Message (inventory) */
		else
		{
#ifdef JP
			msg_format("ザックの中の%s(%c)は%sという感じがする...",
					o_name, index_to_label(i),game_inscriptions[feel]);
#else
			msg_format("You feel the %s (%c) in your pack %s %s...",
				   o_name, index_to_label(i),
				   ((o_ptr->number == 1) ? "is" : "are"),
					   game_inscriptions[feel]);
#endif
            sound(SOUND_PSEUDOID);
		}

		/* We have "felt" it */
		o_ptr->ident |= (IDENT_SENSE);

		/* Set the "inscription" */
		o_ptr->feeling = feel;

		/* Auto-inscription/destroy */
		autopick_alter_item(i, destroy_feeling);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}
}

static void sense_magic(void)
{
	int         i;
	int         plev = p_ptr->lev;
	bool        heavy = TRUE;
	byte        feel;
	object_type *o_ptr;
	char        o_name[MAX_NLEN];

	/*** Check for "sensing" ***/

	/* No sensing when confused */
	if (p_ptr->confused) return;

	/* Analyze the class */
	switch (cp_ptr->s_mag)
	{
		case SENSE_CANT:
		{
			/* cannot sense */
			return;
		}

		case SENSE_VFST:
		{
			/* Good sensing */
			if (!one_in_(4000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case SENSE_FAST:
		{
			/* Very bad sensing */
			if (!one_in_(9000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case SENSE_GOOD:
		{
			/* Good sensing */
			if (!one_in_(20000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case SENSE_NORM:
		{
			/* Good sensing */
			if (!one_in_(55000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case SENSE_SLOW:
		{
			/* Good sensing */
			if (!one_in_(75000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case SENSE_BAD:
		{
			/* Bad sensing */
			if (!one_in_(95000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case SENSE_POOR:
		{
			/* Bad sensing */
			if (!one_in_(240000L / (plev + 5))) return;

			/* Done */
			break;
		}
	}


	/*** Sense everything ***/

	/* Check everything */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		bool okay = FALSE;

		o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Valid "tval" codes */
		switch (o_ptr->tval)
		{
			case TV_RING:
			case TV_AMULET:
			case TV_FIGURINE:
			{
				okay = TRUE;
				break;
			}
		}

		/* Skip non-sense machines */
		if (!okay) continue;

		/* We know about it already, do not tell us again */
		if (o_ptr->ident & (IDENT_SENSE)) continue;

		/* It is fully known, no information needed */
		if (object_known_p(o_ptr)) continue;

		/* Occasional failure on inventory items */
		if ((i < INVEN_WIELD) && (0 != randint0(5))) continue;

		/* Check for a feeling */
		feel = (heavy ? value_check_aux1(o_ptr) : value_check_aux2(o_ptr));

		/* Skip non-feelings */
		if (!feel) continue;

		/* Stop everything */
		if (disturb_minor) disturb(0, 0);

		/* Get an object description */
		object_desc(o_name, o_ptr, OD_OMIT_PREFIX | OD_NAME_ONLY);

		/* Message (equipment) */
		if (i >= INVEN_WIELD)
		{
#ifdef JP
			msg_format("%s%s(%c)は%sという感じがする...",
					describe_use(i),o_name, index_to_label(i),game_inscriptions[feel]);
#else
			msg_format("You feel the %s (%c) you are %s %s %s...",
					o_name, index_to_label(i), describe_use(i),
					((o_ptr->number == 1) ? "is" : "are"),
					game_inscriptions[feel]);
#endif
            sound(SOUND_PSEUDOID);
		}

		/* Message (inventory) */
		else
		{
#ifdef JP
			msg_format("ザックの中の%s(%c)は%sという感じがする...",
					o_name, index_to_label(i),game_inscriptions[feel]);
#else
			msg_format("You feel the %s (%c) in your pack %s %s...",
				   o_name, index_to_label(i),
				   ((o_ptr->number == 1) ? "is" : "are"),
					   game_inscriptions[feel]);
#endif
            sound(SOUND_PSEUDOID);
		}

		/* We have "felt" it */
		o_ptr->ident |= (IDENT_SENSE);

		/* Set the "inscription" */
		o_ptr->feeling = feel;

		/* Auto-inscription/destroy */
		autopick_alter_item(i, destroy_feeling);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}
}

/*
 * Go to any level (ripped off from wiz_jump)
 */

static void pattern_teleport(void)
{
	int min_level = 0;
	int max_level = TINY_MAX_DEPTH - 1;

	/* Ask for level */
#ifdef JP
	if (get_check("レベル・テレポートしますか？"))
#else
	if (get_check("Teleport level? "))
#endif

	{
		char	ppp[80];
		char	tmp_val[160];

		/* Only downward in ironman mode */
		if (ironman_downward)
			min_level = dun_level;

		/* Maximum level */
		if (dun_level > MORGOTH_DEPTH)
			max_level = MORGOTH_DEPTH - 1;
		else if (dun_level == MORGOTH_DEPTH)
			max_level = MORGOTH_DEPTH;

		/* Prompt */
#ifdef JP
		sprintf(ppp, "テレポート先:(%d-%d)", min_level, max_level);
#else
		sprintf(ppp, "Teleport to level (%d-%d): ", min_level, max_level);
#endif

		/* Default */
		sprintf(tmp_val, "%d", dun_level);

		/* Ask for a level */
		if (!get_string(ppp, tmp_val, 10)) return;

		/* Extract request */
		command_arg = atoi(tmp_val);
	}
#ifdef JP
	else if (get_check("通常テレポート？"))
#else
	else if (get_check("Normal teleport? "))
#endif
	{
		teleport_player(200);
		return;
	}
	else
	{
		return;
	}

	/* Paranoia */
	if (command_arg < min_level) command_arg = min_level;

	/* Paranoia */
	if (command_arg > max_level) command_arg = max_level;

	/* Accept request */
#ifdef JP
	msg_format("%d 階にテレポートしました。", command_arg);
#else
	msg_format("You teleport to dungeon level %d.", command_arg);
#endif

	if (autosave_l) do_cmd_save_game(TRUE);

	/* Change level */
	dun_level = command_arg;

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/* Returns TRUE if we are on the Pattern... */
static bool pattern_effect(void)
{
	if ((cave[py][px].feat < FEAT_PATTERN_START) ||
	    (cave[py][px].feat > FEAT_PATTERN_XTRA2))
		return FALSE;

	if (cave[py][px].feat == FEAT_PATTERN_END)
	{
		(void)set_poisoned(0);
		(void)set_image(0);
		(void)set_stun(0);
		(void)set_cut(0);
		(void)set_blind(0);
		(void)set_afraid(0);
		(void)do_res_stat(A_STR);
		(void)do_res_stat(A_INT);
		(void)do_res_stat(A_WIS);
		(void)do_res_stat(A_DEX);
		(void)do_res_stat(A_CON);
		(void)do_res_stat(A_CHR);
		(void)restore_level();
		(void)hp_player(1000);
		cave_set_feat(py, px, FEAT_PATTERN_OLD);
#ifdef JP
		msg_print("「パターン」のこの部分は他の部分より強力でないようだ。");
#else
		msg_print("This section of the Pattern looks less powerful.");
#endif
	}


	/*
	 * We could make the healing effect of the
	 * Pattern center one-time only to avoid various kinds
	 * of abuse, like luring the win monster into fighting you
	 * in the middle of the pattern...
	 */

	else if (cave[py][px].feat == FEAT_PATTERN_OLD)
	{
		/* No effect */
	}
	else if (cave[py][px].feat == FEAT_PATTERN_XTRA1)
	{
		pattern_teleport();
	}
	else if (cave[py][px].feat == FEAT_PATTERN_XTRA2)
	{
		if (!p_ptr->invuln)
#ifdef JP
		take_hit(200, "壊れた「パターン」を歩いたダメージ");
#else
		take_hit(200, "walking the corrupted Pattern");
#endif
	}
	else
	{
		if (!p_ptr->invuln)
#ifdef JP
			take_hit(damroll(1,3), "「パターン」を歩いたダメージ");
#else
			take_hit(damroll(1, 3), "walking the Pattern");
#endif
	}

	return TRUE;
}





/*
 * Regenerate hit points				-RAK-
 */
static void regenhp(int percent)
{
	s32b    new_chp, new_chp_frac;
	int     old_chp;

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
		p_ptr->window |= (PW_PLAYER);
	}
}


/*
 * Regenerate mana points				-RAK-
 */
static void regenmana(int percent)
{
	s32b new_mana, new_mana_frac;
	int old_csp = p_ptr->csp;

	if (percent > 0)
	{
		s32b add_mana  = ((long)p_ptr->msp) * percent + PY_REGEN_MNBASE + p_ptr->csp_frac;

		new_mana = (add_mana >> 16);	/* div 65536 */
		new_mana_frac = (add_mana & 0xFFFF);	/* mod 65536 */

		p_ptr->csp += (s16b)(new_mana);
		p_ptr->csp_frac = (u16b)(new_mana_frac);

		/* check for overflow */
		if ((p_ptr->csp < 0) && (old_csp > 0))
		{
			p_ptr->csp = MAX_SHORT;
		}

		/* Must set frac to zero even if equal */
		if (p_ptr->csp >= p_ptr->msp)
		{
			p_ptr->csp = p_ptr->msp;
			p_ptr->csp_frac = 0;
		}
	}
	else if (percent < 0)
	{
		s32b dec_mana = ((long)p_ptr->msp) * (0 - percent) + PY_REGEN_MNBASE;

		new_mana = (dec_mana >> 16);	/* div 65536 */
		new_mana_frac = (dec_mana & 0xFFFF);	/* mod 65536 */

		/* Check underflow */
		if (new_mana_frac > p_ptr->csp_frac)
		{
			new_mana++;
			p_ptr->csp_frac = 0;
		}
		else
			p_ptr->csp_frac -= (u16b)(new_mana_frac);

		p_ptr->csp -= (s16b)(new_mana);

		/* Check underflow */
		if (p_ptr->csp < 0)
		{
			p_ptr->csp = 0;
			p_ptr->csp_frac = 0;
		}
	}

	/* Redraw mana */
	if (old_csp != p_ptr->csp)
	{
		/* Redraw */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		p_ptr->window |= (PW_SPELL);
	}
}






/*
 * Regenerate the monsters (once per 100 game turns)
 *
 * XXX XXX XXX Should probably be done during monster turns.
 */
static void regen_monsters(void)
{
	int i, frac;


	/* Regenerate everyone */
	for (i = 1; i < m_max; i++)
	{
		/* Check the i'th monster */
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];


		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Allow regeneration (if needed) */
		if (m_ptr->hp < m_ptr->maxhp)
		{
			/* Hack -- Base regeneration */
			frac = m_ptr->maxhp / 100;

			/* Hack -- Minimal regeneration rate */
			if (!frac) frac = 1;

			/* Hack -- Some monsters regenerate quickly */
			if (r_ptr->flags2 & RF2_REGENERATE) frac *= 2;

			/* Hack -- Regenerate */
			m_ptr->hp += frac;

			/* Do not over-regenerate */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
		}
	}
}


static void notice_lite_change(object_type *o_ptr)
{
	/* Hack -- notice interesting fuel steps */
	if ((o_ptr->xtra3 < 100) || (!(o_ptr->xtra3 % 100)))
	{
		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
	}

	/* Hack -- Special treatment when blind */
	if (p_ptr->blind)
	{
		/* Hack -- save some light for later */
		if (o_ptr->xtra3 == 0) o_ptr->xtra3++;
	}

	/* The light is now out */
	else if (o_ptr->xtra3 == 0)
	{
		disturb(0, 0);
#ifdef JP
	msg_print("明かりが消えてしまった！");
#else
		msg_print("Your light has gone out!");
#endif

	}

	/* The light is getting dim */
	else if ((o_ptr->xtra3 < 100) && (!(o_ptr->xtra3 % 10)))
	{
		if (disturb_minor) disturb(0, 0);
#ifdef JP
	msg_print("明かりが微かになってきている。");
#else
		msg_print("Your light is growing faint.");
#endif

	}
}


void leave_quest_check(void)
{
	/* Save quset number for dungeon pref file ($LEAVING_QUEST) */
	leaving_quest = p_ptr->inside_quest;

	/* Leaving an 'only once' quest marks it as failed */
	if (leaving_quest &&
	    ((quest[leaving_quest].flags & QUEST_FLAG_ONCE)  || (quest[leaving_quest].type == QUEST_TYPE_RANDOM)) &&
	    (quest[leaving_quest].status == QUEST_STATUS_TAKEN))
	{
		quest[leaving_quest].status = QUEST_STATUS_FAILED;
		quest[leaving_quest].complev = (byte)p_ptr->lev;
		if (quest[leaving_quest].type == QUEST_TYPE_RANDOM)
			r_info[quest[leaving_quest].r_idx].flags1 &= ~(RF1_QUESTOR);
	}
}


static bool item_tester_hook_psychometry(const object_type *o_ptr)
{
	return (!object_known_p(o_ptr) && 
		((o_ptr->feeling == FEEL_NONE) || (o_ptr->feeling == FEEL_UNCURSED)));
}

/*
 * Forcibly pseudo-identify an object in the inventory
 * (or on the floor)
 *
 * note: currently this function allows pseudo-id of any object,
 * including silly ones like potions & scrolls, which always
 * get '{average}'. This should be changed, either to stop such
 * items from being pseudo-id'd, or to allow psychometry to
 * detect whether the unidentified potion/scroll/etc is
 * good (Cure Light Wounds, Restore Strength, etc) or
 * bad (Poison, Weakness etc) or 'useless' (Slime Mold Juice, etc).
 */
bool psychometry(void)
{
	int             item;
	object_type     *o_ptr;
	char            o_name[MAX_NLEN];
	byte            feel;
	cptr            q, s;
	bool            okay = FALSE;

	item_tester_hook = item_tester_hook_psychometry;

	/* Get an item */
#ifdef JP
	q = "どのアイテムを霊視しますか？";
	s = "調べるアイテムがありません。";
#else
	q = "Meditate on which item? ";
	s = "You have nothing appropriate.";
#endif

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

	/* It is fully known, no information needed */
	if (object_known_p(o_ptr))
	{
#ifdef JP
		msg_print("何も新しいことは判らなかった。");
#else
		msg_print("You cannot find out anything more about that.");
#endif
		return TRUE;
	}

	/* Check for a feeling */
	feel = value_check_aux1(o_ptr);

	/* Get an object description */
	object_desc(o_name, o_ptr, OD_OMIT_PREFIX | OD_NAME_ONLY);

	/* Skip non-feelings */
	if (!feel)
	{
#ifdef JP
		msg_format("%sからは特に変わった事は感じとれなかった。", o_name);
#else
		msg_format("You do not perceive anything unusual about the %s.", o_name);
#endif
		return TRUE;
	}

#ifdef JP
	msg_format("%sは%sという感じがする...",
			o_name,  game_inscriptions[feel]);
#else
	msg_format("You feel that the %s %s %s...",
			o_name, ((o_ptr->number == 1) ? "is" : "are"),
			game_inscriptions[feel]);
#endif

	/* We have "felt" it */
	o_ptr->ident |= (IDENT_SENSE);

	/* "Inscribe" it */
	o_ptr->feeling = feel;

	/* Player touches it */
	o_ptr->marked |= OM_TOUCHED;

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER | PW_STATS);

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
		case TV_RING:
		case TV_AMULET:
		case TV_LITE:
		case TV_FIGURINE:
			okay = TRUE;
			break;
	}

	/* Auto-inscription/destroy */
	autopick_alter_item(item, (bool)(okay && destroy_feeling));

	/* Something happened */
	return (TRUE);
}


/*
 * If player has inscribed the object with "!!", let him know when it's
 * recharged. -LM-
 */
static void recharged_notice_aux(object_type *o_ptr)
{
	char o_name[MAX_NLEN];

	/* Describe (briefly) */
	object_desc(o_name, o_ptr, OD_OMIT_PREFIX | OD_NAME_ONLY);

	/* Notify the player */
#ifdef JP
	msg_format("%sは再充填された。", o_name);
#else
	if (o_ptr->number > 1)
		msg_format("Your %s are recharged.", o_name);
	else msg_format("Your %s is recharged.", o_name);
#endif

	/* Disturb */
	disturb(0, 0);
}


static void recharged_notice(object_type *o_ptr)
{
	cptr s;

	/* No inscription */
	if (!o_ptr->inscription) return;

	/* Find a '%' */
	s = my_strchr(quark_str(o_ptr->inscription), '%');

	if (s)
	{
		/* Notify the player */
		recharged_notice_aux(o_ptr);

		/* Done. */
		return;
	}

	/* Find a '!' */
	s = my_strchr(quark_str(o_ptr->inscription), '!');

	/* Process notification request. */
	while (s)
	{
		/* Find another '!' */
		if (s[1] == '!')
		{
			/* Notify the player */
			recharged_notice_aux(o_ptr);

			/* Done. */
			return;
		}

		/* Keep looking for '!'s */
		s = my_strchr(s + 1, '!');
	}
}

static void play_ambient_sound(void)
{
        /* Town sound */
        if (dun_level == 0) 
        {
                /* Hack - is it daytime or nighttime? */
                if (turn % (10L * TOWN_DAWN) < (10L * TOWN_DAWN) / 2)
                {
                        /* It's day. */
                        sound(SOUND_AMBIENT_DAY);
                } 
                else 
                {
                        /* It's night. */
                        sound(SOUND_AMBIENT_NITE);
                }
                
        }

        /* Dungeon level 1-5 */
        else if (dun_level <= 5) 
        {
                sound(SOUND_AMBIENT_DNG1);
        }

        /* Dungeon level 6-10 */
        else if (dun_level <= 10) 
        {
                sound(SOUND_AMBIENT_DNG2);
        }

        /* Dungeon level 11-15 */
        else if (dun_level <= 15) 
        {
                sound(SOUND_AMBIENT_DNG3);
        }

        /* Dungeon level 16-20 */
        else if (dun_level <= 20) 
        {
                sound(SOUND_AMBIENT_DNG4);
        }

        /* Dungeon level 21- */
        else  
        {
                sound(SOUND_AMBIENT_DNG5);
        }
}

/*
 * Count number of adjacent monsters
 */
static int get_monster_crowd_number(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	int my = m_ptr->fy;
	int mx = m_ptr->fx;
	int i;
	int count = 0;

	for (i = 0; i < 7; i++)
	{
		int ay = my + ddy_ddd[i];
		int ax = mx + ddx_ddd[i];

		if (!in_bounds(ay, ax)) continue;

		/* Count number of monsters */
		if (cave[ay][ax].m_idx > 0) count++;
 	}

	return count;
}



/*
 * Dungeon rating is no longer linear
 */
#define RATING_BOOST(delta) (delta * delta + 50 * delta)

/*
 * Examine all monsters and unidentified objects,
 * and get the feeling of current dungeon floor
 */
byte get_dungeon_feeling(void)
{
	const int base = 10;
	int rating = 0;
	int i;

	/* Hack -- no feeling in the town */
	if (!dun_level) return 0;

	/* Examine each monster */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr;
		int delta = 0;

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Ignore pet */
		if (is_pet(m_ptr)) continue;

		r_ptr = &r_info[m_ptr->r_idx];

		/* Unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Nearly out-of-depth unique monsters */
			if (r_ptr->level + 10 > dun_level)
			{
				/* Boost rating by twice delta-depth */
				delta += (r_ptr->level + 10 - dun_level) * 2 * base;
			}
		}
		else
		{
			/* Out-of-depth monsters */
			if (r_ptr->level > dun_level)
			{
				/* Boost rating by delta-depth */
				delta += (r_ptr->level - dun_level) * base;
			}
		}

		/* Unusually crowded monsters get a little bit of rating boost */
		if (r_ptr->flags1 & RF1_FRIENDS)
		{
			if (5 <= get_monster_crowd_number(i)) delta += 1;
		}
		else
		{
			if (2 <= get_monster_crowd_number(i)) delta += 1;
		}


		rating += RATING_BOOST(delta);
	}

	/* Examine each unidentified object */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];
		object_kind *k_ptr = &k_info[o_ptr->k_idx];
		int delta = 0;

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip known objects */
		if (object_known_p(o_ptr))
		{
			/* Touched? */
			if (o_ptr->marked & OM_TOUCHED) continue;
		}

		/* Skip pseudo-known objects */
		if (o_ptr->ident & IDENT_SENSE) continue;

		/* Ego objects */
		if (ego_item_p(o_ptr))
		{
			ego_item_type *e_ptr = &e_info[o_ptr->name2];

			delta += e_ptr->rating * base;
		}

		/* Fixed artifacts */
		if (artifact_p(o_ptr))
		{
			s32b cost;

			/* Special feeling */
			if (!preserve_mode) return 1;

			cost = object_value_real(o_ptr);

			delta += 10 * base;
			if (cost > 50000L) delta += 10 * base;
		}

		/* Random artifacts */
		if (o_ptr->art_name) delta += 40 * base;

		if (o_ptr->tval == TV_DRAG_ARMOR) delta += 30 * base;
		if (o_ptr->tval == TV_SHIELD &&
		    o_ptr->sval == SV_DRAGON_SHIELD) delta += 5 * base;
		if (o_ptr->tval == TV_GLOVES &&
		    o_ptr->sval == SV_SET_OF_DRAGON_GLOVES) delta += 5 * base;
		if (o_ptr->tval == TV_BOOTS &&
		    o_ptr->sval == SV_PAIR_OF_DRAGON_BOOTS) delta += 5 * base;
		if (o_ptr->tval == TV_HELM &&
		    o_ptr->sval == SV_DRAGON_HELM) delta += 5 * base;
		if (o_ptr->tval == TV_HARD_ARMOR &&
		    o_ptr->sval == SV_DRAGON_ARMOR) delta += 5 * base;
		if (o_ptr->tval == TV_RING &&
		    o_ptr->sval == SV_RING_SPEED &&
		    !cursed_p(o_ptr)) delta += 25 * base;
		if (o_ptr->tval == TV_RING &&
		    o_ptr->sval == SV_RING_LORDLY) delta += 5 * base;
		if (o_ptr->tval == TV_AMULET &&
		    o_ptr->sval == SV_AMULET_THE_MAGI) delta += 25 * base;
		if (o_ptr->tval == TV_AMULET &&
		    o_ptr->sval == SV_AMULET_THE_HERO) delta += 25 * base;

		/* Out-of-depth objects */
		if (!cursed_p(o_ptr) && !broken_p(o_ptr) &&
		    k_ptr->level > dun_level)
		{
			/* Rating increase */
			delta += (k_ptr->level - dun_level) * base;
		}

		rating += RATING_BOOST(delta);
	}


	if (rating > RATING_BOOST(1000)) return 2;
	if (rating > RATING_BOOST(800)) return 3;
	if (rating > RATING_BOOST(600)) return 4;
	if (rating > RATING_BOOST(400)) return 5;
	if (rating > RATING_BOOST(300)) return 6;
	if (rating > RATING_BOOST(200)) return 7;
	if (rating > RATING_BOOST(100)) return 8;
	if (rating > RATING_BOOST(0)) return 9;

	return 10;
}


/*
 * Update dungeon feeling, and announce it if changed
 */
static void update_dungeon_feeling(void)
{
	byte new_feeling;
	int quest_num;
	int delay;

	/* No feeling on the surface */
	if (!dun_level) return;

	/* Extract delay time */
	delay = MAX(10, 150 - p_ptr->skill_fos) * (150 - dun_level) * 10 / 100;

 	/* Not yet felt anything */
	if (turn < p_ptr->feeling_turn + delay && !cheat_xtra) return;

	/* Extract quest number (if any) */
	quest_num = quest_number(dun_level);

	/* No feeling in a quest */
	if (quest_num &&
	    (((quest_num < MIN_RANDOM_QUEST) || (quest_num > MAX_RANDOM_QUEST)) &&
	     !((quest_num == QUEST_SAURON) || (quest_num == QUEST_MORGOTH) ||
	       !(quest[quest_num].flags & QUEST_FLAG_PRESET)))) return;


	/* Get new dungeon feeling */
	new_feeling = get_dungeon_feeling();

	/* Remember last time updated */
	p_ptr->feeling_turn = turn;

	/* No change */
	if (p_ptr->feeling == new_feeling) return;

	/* Dungeon feeling is changed */
	p_ptr->feeling = new_feeling;

	/* Announce feeling */
	do_cmd_feeling();

	/* Update the level indicator */
	p_ptr->redraw |= (PR_DEPTH);

	/* Disturb */
	if (disturb_minor) disturb(0, 0);
}


/*
 * Handle certain things once every 10 game turns
 */
static void process_world(void)
{
	int x, y, i, j;
	s32b regen_amount;
	bool cave_no_regen = FALSE;
	int upkeep_factor = 0;
	cave_type *c_ptr;
	object_type *o_ptr;
	u32b f1 = 0 , f2 = 0 , f3 = 0;
	int temp;
	object_kind *k_ptr;

	int day, hour, min, prev_min;

#define __LEN (10L * TOWN_DAWN)
	s32b tick = (turn % __LEN) + (__LEN / 4);

	extract_day_hour_min(&day, &hour, &min);
	prev_min = (1440 * (tick - 10L) / __LEN) % 60;

	/* Update dungeon feeling, and announce it if changed */
	update_dungeon_feeling();

	/* Every 10 game turns */
	if (turn % 10) return;


	/*** Check the Time and Load ***/

    /* Play an ambient sound at regular intervals. */
    if (!(turn % (TOWN_DAWN / 4)))
    {
          play_ambient_sound();
    }

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
#ifdef JP
				msg_print("アングバンドへの門が閉じかかっています...");
				msg_print("ゲームを終了するかセーブするかして下さい。");
#else
				msg_print("The gates to ANGBAND are closing...");
				msg_print("Please finish up and/or save your game.");
#endif

			}

			/* Slam the gate */
			else
			{
				/* Message */
#ifdef JP
				msg_print("今、アングバンドへの門が閉ざされました。");
#else
				msg_print("The gates to ANGBAND are now closed.");
#endif


				/* Stop playing */
				alive = FALSE;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}
	}

	/*** Attempt timed autosave ***/
	if (autosave_t && autosave_freq)
	{
		if (!(turn % ((s32b)autosave_freq * 10)))
			do_cmd_save_game(TRUE);
	}

	if (mon_fight)
	{
#ifdef JP
		msg_print("何かが聞こえた。");
#else
		msg_print("You hear noise.");
#endif

	}

	/*** Handle the wilderness/town (sunshine) ***/

	/* While in town/wilderness */
	if (!dun_level && !p_ptr->inside_quest)
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
#ifdef JP
				msg_print("夜が明けた。");
#else
				msg_print("The sun has risen.");
#endif


				/* Hack -- Scan the town */
				for (y = 0; y < cur_hgt; y++)
				{
					for (x = 0; x < cur_wid; x++)
					{
						/* Get the cave grid */
						c_ptr = &cave[y][x];

						/* Assume lit */
						c_ptr->info |= (CAVE_GLOW);

						/* Hack -- Memorize lit grids if allowed */
						if (view_perma_grids) c_ptr->info |= (CAVE_MARK);

						/* Hack -- Notice spot */
						note_spot(y, x);
					}
				}
			}

			/* Night falls */
			else
			{
				/* Message */
#ifdef JP
				msg_print("日が沈んだ。");
#else
				msg_print("The sun has fallen.");
#endif


				/* Hack -- Scan the town */
				for (y = 0; y < cur_hgt; y++)
				{
					for (x = 0; x < cur_wid; x++)
					{
						/* Get the cave grid */
						c_ptr = &cave[y][x];

						/* Darken "boring" features */
						if ((c_ptr->feat <= FEAT_INVIS) ||
						    ((c_ptr->feat >= FEAT_DEEP_WATER) &&
						    (c_ptr->feat <= FEAT_MOUNTAIN)) ||
						    (x == 0) || (x == cur_wid - 1) ||
						    (y == 0) || (y == cur_hgt - 1))
						{
							/* Forget the grid */
							c_ptr->info &= ~(CAVE_GLOW | CAVE_MARK);

							/* Hack -- Notice spot */
							note_spot(y, x);
						}
					}
				}

				/* Glow lava in floor */
				glow_lava_floor();
			}

			/* Update the monsters */
			p_ptr->update |= (PU_MONSTERS | PU_MON_LITE);

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
		}
	}

	/* Set back the rewards once a day */
	if (!(turn % (10L * STORE_TURNS)))
	{
		int n;

		/* Reset the rewards */
		for (n = 0; n < MAX_BACT; n++)
		{
			p_ptr->rewards[n] = FALSE;
		}

		/* Message */
#ifdef JP
		if (cheat_xtra) msg_print("報酬をリセット");
#else
		if (cheat_xtra) msg_print("Rewards reset.");
#endif

	}


	/*** Process the monsters ***/

	/* Check for creature generation. */
	if ((randint0(MAX_M_ALLOC_CHANCE) == 0) &&
	    !p_ptr->inside_arena && !p_ptr->inside_quest)
	{
		/* Make a new monster */
		(void)alloc_monster(MAX_SIGHT + 5, FALSE);
	}

	/* Hack -- Check for creature regeneration */
	if (!(turn % 100)) regen_monsters();


	/*** Damage over Time ***/

	/* Take damage from poison */
	if (p_ptr->poisoned && !p_ptr->invuln)
	{
		/* Take damage */
#ifdef JP
		take_hit(1, "毒");
#else
		take_hit(1, "poison");
#endif

	}

#if 0
	/* (Vampires) Take damage from sunlight */
	if (p_ptr->prace == RACE_VAMPIRE)
	{
		if (!dun_level && !p_ptr->resist_lite && !p_ptr->invuln &&
		    (!((turn / ((10L * TOWN_DAWN) / 2)) % 2)))
		{
			if (cave[py][px].info & CAVE_GLOW)
			{
				/* Take damage */
#ifdef JP
				msg_print("日光があなたのアンデッドの肉体を焼き焦がした！");
				take_hit(1, "日光");
#else
				msg_print("The sun's rays scorch your undead flesh!");
				take_hit(1, "sunlight");
#endif

				cave_no_regen = TRUE;
			}
		}

		if (inventory[INVEN_LITE].tval &&
		    (inventory[INVEN_LITE].sval >= SV_LITE_GALADRIEL) &&
		    (inventory[INVEN_LITE].sval < SV_LITE_THRAIN) &&
		    !p_ptr->resist_lite)
		{
			object_type * o_ptr = &inventory[INVEN_LITE];
			char o_name[MAX_NLEN];
			char ouch[80];

			/* Get an object description */
			object_desc(o_name, o_ptr, OD_OMIT_PREFIX | OD_NAME_ONLY);

#ifdef JP
			msg_format("%sがあなたのアンデッドの肉体を焼き焦がした！", o_name);
#else
			msg_format("The %s scorches your undead flesh!", o_name);
#endif


			cave_no_regen = TRUE;

			/* Get an object description */
			object_desc(o_name, o_ptr, OD_NAME_ONLY);

#ifdef JP
			sprintf(ouch, "%sを装備したダメージ", o_name);
#else
			sprintf(ouch, "wielding %s", o_name);
#endif

			if (!p_ptr->invuln) take_hit(1, ouch);
		}
	}
#endif

	if ((cave[py][px].feat == FEAT_SHAL_LAVA) &&
		!p_ptr->invuln && !p_ptr->immune_fire && !p_ptr->ffall)
	{
		int damage = p_ptr->lev;

		if (p_ptr->resist_fire) damage = damage / 3;
		if (p_ptr->oppose_fire) damage = damage / 3;

		if (damage)
		{
			/* Take damage */
#ifdef JP
			msg_print("溶岩で火傷した！");
			take_hit(damage, "浅い溶岩流");
#else
			msg_print("The lava burns you!");
			take_hit(damage, "shallow lava");
#endif

			cave_no_regen = TRUE;
		}
	}

	else if ((cave[py][px].feat == FEAT_DEEP_LAVA) &&
		!p_ptr->invuln && !p_ptr->immune_fire)
	{
		int damage = p_ptr->lev * 2;
		cptr message;
		cptr hit_from;

		if (p_ptr->resist_fire) damage = damage / 3;
		if (p_ptr->oppose_fire) damage = damage / 3;

		if (p_ptr->ffall)
		{
			damage = damage / 5;

#ifdef JP
			message = "熱で火傷した！";
			hit_from = "深い溶岩流の上に浮遊したダメージ";
#else
			message = "The heat burns you!";
			hit_from = "flying over deep lava";
#endif

		}
		else
		{
#ifdef JP
			message = "溶岩で火傷した！";
			hit_from = "深い溶岩流";
#else
			message = "The lava burns you!";
			hit_from = "deep lava";
#endif

		}

		if (damage)
		{
			/* Take damage */
			msg_print(message);
			take_hit(damage, hit_from);

			cave_no_regen = TRUE;
		}
	}

	else if ((cave[py][px].feat == FEAT_DEEP_WATER) && !p_ptr->ffall)
	{
		if (p_ptr->total_weight > (u32b)((adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100) / 2))
		{
			/* Take damage */
#ifdef JP
			msg_print("溺れている！");
			take_hit(randint1(p_ptr->lev), "溺れ");
#else
			msg_print("You are drowning!");
			take_hit(randint1(p_ptr->lev), "drowning");
#endif
			cave_no_regen = TRUE;
		}
	}

	/* Spectres -- take damage when moving through walls */
	/*
	 * Added: ANYBODY takes damage if inside through walls
	 * without wraith form -- NOTE: Spectres will never be
	 * reduced below 0 hp by being inside a stone wall; others
	 * WILL BE!
	 */
	if (!cave_floor_bold(py, px))
	{
		/* Player can walk through trees */
		if ((cave[py][px].feat == FEAT_TREES) || ((cave[py][px].feat == FEAT_MOUNTAIN) && !dun_level && p_ptr->ffall))
		{
			/* Do nothing */
		}
		else if (!p_ptr->invuln && !p_ptr->wraith_form &&
		    ((p_ptr->chp > (p_ptr->lev / 5)) || !p_ptr->pass_wall))
		{
			cptr dam_desc;

			cave_no_regen = TRUE;

			if (p_ptr->pass_wall)
			{
#ifdef JP
				msg_print("体の分子が分解した気がする！");
				dam_desc = "密度";
#else
				msg_print("Your molecules feel disrupted!");
				dam_desc = "density";
#endif

			}
			else
			{
#ifdef JP
				msg_print("崩れた岩に押し潰された！");
				dam_desc = "硬い岩";
#else
				msg_print("You are being crushed!");
				dam_desc = "solid rock";
#endif

			}

			take_hit(1 + (p_ptr->lev / 5), dam_desc);
		}
	}

	if (take_notes)
	{
		if (!hour && !min && (min != prev_min))
			add_note(" ",'D');
	}

	/* Nightmare mode activates the TY_CURSE at midnight */
	if (ironman_nightmare)
	{
		/* Require exact minute */
		if (min != prev_min)
		{
			/* Every 15 minutes after 11:00 pm */
			if ((hour == 23) && !(min % 15))
			{
				/* Disturbing */
				disturb(0, 0);

				switch (min / 15)
				{
					case 0:
					{
#ifdef JP
						msg_print("遠くで不気味な鐘の音が鳴った。");
#else
						msg_print("You hear a distant bell toll ominously.");
#endif

						break;
					}
					case 1:
					{
#ifdef JP
						msg_print("遠くで鐘が二回鳴った。");
#else
						msg_print("A distant bell sounds twice.");
#endif

						break;
					}
					case 2:
	{
#ifdef JP
						msg_print("遠くで鐘が三回鳴った。");
#else
						msg_print("A distant bell sounds three times.");
#endif

						break;
					}
					case 3:
					{
#ifdef JP
						msg_print("遠くで鐘が四回鳴った。");
#else
						msg_print("A distant bell tolls four times.");
#endif

						break;
					}
				}
			}

			/* TY_CURSE activates at mignight! */
			if (!hour && !min)
			{
				int count = 0;

				disturb(1, 0);
#ifdef JP
				msg_print("遠くで鐘が何回も鳴り、死んだような静けさの中へ消えていった。");
#else
				msg_print("A distant bell tolls many times, fading into an deathly silence.");
#endif

				activate_ty_curse(FALSE, &count);
			}
		}
	}

	/* Take damage from cuts */
	if (p_ptr->cut && !p_ptr->invuln)
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
#ifdef JP
		take_hit(i, "致命傷");
#else
		take_hit(i, "a fatal wound");
#endif

	}


	/*** Check the Food, and Regenerate ***/

	/* Digest normally */
	if (p_ptr->food < PY_FOOD_MAX)
	{
		/* Every 100 game turns */
		if (!(turn % 100))
		{
			/* Basic digestion rate based on speed */
			if (p_ptr->pspeed > 199) i = 49;
			else if (p_ptr->pspeed < 0) i = 1;
			else i = extract_energy[p_ptr->pspeed];

			i *= 2;

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
#ifdef JP
		if (!p_ptr->invuln) take_hit(i, "空腹");
#else
		if (!p_ptr->invuln) take_hit(i, "starvation");
#endif

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
			if (!p_ptr->paralyzed && (randint0(100) < 10))
			{
				/* Message */
#ifdef JP
				msg_print("あまりにも空腹で気絶してしまった。");
#else
				msg_print("You faint from the lack of food.");
#endif

				disturb(1, 0);

				/* Hack -- faint (bypass free action) */
				(void)set_paralyzed(p_ptr->paralyzed + 1 + randint0(5));
			}
		}
	}


	/* Are we walking the pattern? */
	if (pattern_effect())
	{
		cave_no_regen = TRUE;
	}
	else
	{
		/* Regeneration ability */
		if (p_ptr->regenerate)
		{
			regen_amount = regen_amount * 2;
		}
	}


	/* Searching or Resting */
	if (p_ptr->searching || resting)
	{
		regen_amount = regen_amount * 2;
	}

	upkeep_factor = calculate_upkeep();

	/* Regenerate the mana */
	if ((p_ptr->csp < p_ptr->msp) || (upkeep_factor != 0))
	{
		s32b upkeep_regen = (((100 - upkeep_factor) * regen_amount) / 100);
		regenmana(upkeep_regen);

#ifdef TRACK_FRIENDS
		if (wizard)
		{
#ifdef JP
			msg_format("ＭＰ回復: %d/%d", upkeep_regen, regen_amount);
#else
			msg_format("Regen: %d/%d", upkeep_regen, regen_amount);
#endif
		}
#endif /* TRACK_FRIENDS */
	}

	/* Empty mana becaused of controling too many pets */
	if ((p_ptr->csp == 0) && (p_ptr->csp_frac == 0))
	{
		while (upkeep_factor > 100)
		{
#ifdef JP
			msg_print("こんなに多くのペットを制御できない！");
#else
			msg_print("Too many pets to control at once!");
#endif
			msg_print(NULL);
			do_cmd_pet_dismiss();

			upkeep_factor = calculate_upkeep();

#ifdef JP
			msg_format("維持ＭＰは %d%%", upkeep_factor);
#else
			msg_format("Upkeep: %d%% mana.", upkeep_factor);
#endif
			msg_print(NULL);
		}
	}

	/* Poisoned or cut yields no healing */
	if (p_ptr->poisoned) regen_amount = 0;
	if (p_ptr->cut) regen_amount = 0;

	/* Special floor -- Pattern, in a wall -- yields no healing */
	if (cave_no_regen) regen_amount = 0;

	regen_amount = (regen_amount * mutant_regenerate_mod) / 100;

	/* Regenerate Hit Points if needed */
	if (p_ptr->chp < p_ptr->mhp)
	{
		regenhp(regen_amount);
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

	/* Times see-invisible */
	if (p_ptr->tim_invis)
	{
		(void)set_tim_invis(p_ptr->tim_invis - 1);
	}

	if (multi_rew)
	{
		multi_rew = FALSE;
	}

	/* Timed esp */
	if (p_ptr->tim_esp)
	{
		(void)set_tim_esp(p_ptr->tim_esp - 1);
	}

	/* Timed infra-vision */
	if (p_ptr->tim_infra)
	{
		(void)set_tim_infra(p_ptr->tim_infra - 1);
	}

	/* Timed Regeneration */
	if (p_ptr->tim_regen)
	{
		(void)set_tim_regen(p_ptr->tim_regen - 1);
	}

	/* Timed fire aura */
	if (p_ptr->tim_sh_fire)
	{
		(void)set_tim_sh_fire(p_ptr->tim_sh_fire - 1);
	}

	/* Timed elec aura */
	if (p_ptr->tim_sh_elec)
	{
		(void)set_tim_sh_elec(p_ptr->tim_sh_elec - 1);
	}

	/* Timed cold aura */
	if (p_ptr->tim_sh_cold)
	{
		(void)set_tim_sh_cold(p_ptr->tim_sh_cold - 1);
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

	/* Wraith form */
	if (p_ptr->tim_wraith)
	{
		(void)set_wraith_form(p_ptr->tim_wraith - 1);
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

	/* Magicdef */
	if (p_ptr->magicdef)
	{
		(void)set_magicdef(p_ptr->magicdef - 1);
	}

	/* Musou */
	if (p_ptr->musou)
	{
		(void)set_musou(p_ptr->musou - 1);
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


	/* Radar Eye */
	if (p_ptr->tim_radar)
	{
		(void)set_tim_radar(p_ptr->tim_radar - 1);
	}

	/* Extra Might */
	if (p_ptr->tim_might)
	{
		(void)set_tim_might(p_ptr->tim_might - 1);
	}

	if (p_ptr->tim_brand)
	{
		(void)set_tim_brand(p_ptr->tim_brand - 1, 0);
	}

	/*** Poison and Stun and Cut ***/

	/* Poison */
	if (p_ptr->poisoned)
	{
		int adjust = adj_con_fix[p_ptr->stat_ind[A_CON]] + 1;

		/* Apply some healing */
		(void)set_poisoned(p_ptr->poisoned - adjust);
	}

	/* Stun */
	if (p_ptr->stun)
	{
		int adjust = adj_con_fix[p_ptr->stat_ind[A_CON]] + 1;

		/* Apply some healing */
		(void)set_stun(p_ptr->stun - adjust);
	}

	/* Cut */
	if (p_ptr->cut)
	{
		int adjust = adj_con_fix[p_ptr->stat_ind[A_CON]] + 1;

		/* Hack -- Truly "mortal" wound */
		if (p_ptr->cut > 1000) adjust = 0;

		/* Apply some healing */
		(void)set_cut(p_ptr->cut - adjust);
	}


	/*** Process Light ***/

	/* Check for light being wielded */
	o_ptr = &inventory[INVEN_LITE];

	/* Burn some fuel in the current lite */
	if (o_ptr->tval == TV_LITE)
	{
		/* Hack -- Use some fuel (except on artifacts) */
		if (!artifact_p(o_ptr) && (o_ptr->xtra3 > 0))
		{
			/* Decrease life-span */
			o_ptr->xtra3--;

			/* Notice interesting fuel steps */
			notice_lite_change(o_ptr);
		}
	}

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);


	/*** Process mutation effects ***/
	if (p_ptr->muta)
	{
		if ((p_ptr->muta & MUT_BERS_RAGE) && one_in_(3000))
		{
			disturb(0, 0);
#ifdef JP
			msg_print("ウガァァア！");
			msg_print("激怒の発作に襲われた！");
#else
			msg_print("RAAAAGHH!");
			msg_print("You feel a fit of rage coming over you!");
#endif
			(void)set_shero(p_ptr->shero + 10 + randint1(p_ptr->lev));
		}

		if ((p_ptr->muta & MUT_COWARDICE) && one_in_(3000))
		{
			if (!(p_ptr->resist_fear || p_ptr->hero || p_ptr->shero))
			{
				disturb(0, 0);
#ifdef JP
				msg_print("とても暗い... とても恐い！");
#else
				msg_print("It's so dark... so scary!");
#endif
				set_afraid(p_ptr->afraid + 13 + randint1(26));
			}
		}

		if ((p_ptr->muta & MUT_HALLU) && one_in_(6400))
		{
			if (!p_ptr->resist_chaos)
			{
				disturb(0, 0);
				p_ptr->redraw |= PR_EXTRA;
				(void)set_image(p_ptr->image + randint0(50) + 20);
			}
		}

		if ((p_ptr->muta & MUT_NORMALITY) && one_in_(5000))
		{
			if (!lose_mutation(0))
#ifdef JP
				msg_print("奇妙なくらい普通になった気がする。");
#else
				msg_print("You feel oddly normal.");
#endif
		}

		if ((p_ptr->muta & MUT_WASTING) && one_in_(3000))
		{
			int which_stat = randint0(6);
			int sustained = FALSE;

			switch (which_stat)
			{
			case A_STR:
				if (p_ptr->sustain_str) sustained = TRUE;
				break;
			case A_INT:
				if (p_ptr->sustain_int) sustained = TRUE;
				break;
			case A_WIS:
				if (p_ptr->sustain_wis) sustained = TRUE;
				break;
			case A_DEX:
				if (p_ptr->sustain_dex) sustained = TRUE;
				break;
			case A_CON:
				if (p_ptr->sustain_con) sustained = TRUE;
				break;
			case A_CHR:
				if (p_ptr->sustain_chr) sustained = TRUE;
				break;
			default:
#ifdef JP
				msg_print("不正な状態！");
#else
				msg_print("Invalid stat chosen!");
#endif
				sustained = TRUE;
			}

			if (!sustained)
			{
				disturb(0, 0);
#ifdef JP
				msg_print("自分が衰弱していくのが分かる！");
#else
				msg_print("You can feel yourself wasting away!");
#endif
				msg_print(NULL);
				(void)dec_stat(which_stat, randint1(6) + 6, randint1(3) == 1);
			}
		}

		if ((p_ptr->muta & MUT_WARNING) && one_in_(1000))
		{
			int danger_amount = 0;
			int monster;

			for (monster = 0; monster < m_max; monster++)
			{
				monster_type    *m_ptr = &m_list[monster];
				monster_race    *r_ptr = &r_info[m_ptr->r_idx];

				/* Paranoia -- Skip dead monsters */
				if (!m_ptr->r_idx) continue;

				if (r_ptr->level >= p_ptr->lev)
				{
					danger_amount += r_ptr->level - p_ptr->lev + 1;
				}
			}

			if (danger_amount > 100)
#ifdef JP
				msg_print("非常に恐ろしい気がする！");
#else
				msg_print("You feel utterly terrified!");
#endif
			else if (danger_amount > 50)
#ifdef JP
				msg_print("恐ろしい気がする！");
#else
				msg_print("You feel terrified!");
#endif
			else if (danger_amount > 20)
#ifdef JP
				msg_print("非常に心配な気がする！");
#else
				msg_print("You feel very worried!");
#endif
			else if (danger_amount > 10)
#ifdef JP
				msg_print("心配な気がする！");
#else
				msg_print("You feel paranoid!");
#endif
			else if (danger_amount > 5)
#ifdef JP
				msg_print("ほとんど安全な気がする。");
#else
				msg_print("You feel almost safe.");
#endif
			else
#ifdef JP
				msg_print("寂しい気がする。");
#else
				msg_print("You feel lonely.");
#endif
		}
	}


	/*** Process Inventory ***/

	/* Handle experience draining */
	if (p_ptr->exp_drain)
	{
		if ((randint0(100) < 10) && (p_ptr->exp > 0))
		{
			p_ptr->exp--;
			p_ptr->max_exp--;
			check_experience();
		}
	}

	/* Process equipment */
	for (j = 0, i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Get the object */
		o_ptr = &inventory[i];
		
		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		object_flags(o_ptr, &f1, &f2, &f3);

		/* TY Curse */
		if ((f3 & TR3_TY_CURSE) && one_in_(TY_CURSE_CHANCE))
		{
			int count = 0;

			(void)activate_ty_curse(FALSE, &count);
		}

		/*
		 * Hack: Uncursed teleporting items (e.g. Trump Weapons)
		 * can actually be useful!
		 */
		if ((f3 & TR3_TELEPORT) && (randint0(100) < 1))
		{
			if ((o_ptr->ident & IDENT_CURSED) && !p_ptr->anti_tele)
			{
				disturb(0, 0);

				/* Teleport player */
				teleport_player(40);
			}
			else
			{
				if (!disturb_other || (o_ptr->inscription &&
				    (my_strchr(quark_str(o_ptr->inscription), '.') ||
					 my_strchr(quark_str(o_ptr->inscription), '%'))))
				{
					/* Do nothing */
					/* msg_print("Teleport aborted.") */ ;
				}
#ifdef JP
				else if (get_check("テレポートしますか？"))
#else
				else if (get_check("Teleport? "))
#endif

				{
					disturb(0, 0);
					teleport_player(50);
				}
			}
		}

		/* Recharge activatable objects */
		if (o_ptr->timeout > 0)
		{
			/* Recharge */
			o_ptr->timeout--;

			/* Notice changes */
			if (!o_ptr->timeout)
			{
				recharged_notice(o_ptr);
				j++;
			}
		}
	}

	/* Notice changes */
	if (j)
	{
		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
	}

	/*
	 * Recharge rods.  Rods now use timeout to control charging status,
	 * and each charging rod in a stack decreases the stack's timeout by
	 * one per turn. -LM-
	 */
	for (j = 0, i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];
		k_ptr = &k_info[o_ptr->k_idx];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Examine all charging rods or stacks of charging rods. */
		if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
		{
			/* Determine how many rods are charging. */
			temp = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;
			if (temp > o_ptr->number) temp = o_ptr->number;

			/* Decrease timeout by that number. */
			o_ptr->timeout -= temp;

			/* Boundary control. */
			if (o_ptr->timeout < 0) o_ptr->timeout = 0;

			/* Notice changes, provide message if object is inscribed. */
			if (!(o_ptr->timeout))
			{
				recharged_notice(o_ptr);
				j++;
			}
		}
	}

	/* Notice changes */
	if (j)
	{
		/* Combine pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN);
	}

	/* Feel the inventory */
	sense_inventory();
	sense_magic();


	/*** Process Objects ***/

	/* Process objects */
	for (i = 1; i < o_max; i++)
	{
		/* Access object */
		o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Recharge rods on the ground.  No messages. */
		if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
		{
			/* Charge it */
			o_ptr->timeout -= o_ptr->number;

			/* Boundary control. */
			if (o_ptr->timeout < 0) o_ptr->timeout = 0;
		}
	}

	/*
	* Cycle ultra-quick R"bool"G to prevent periodic patterns
	* in the illumination in a forest after dark.
	*/

	quick_rand_add();


	/*** Involuntary Movement ***/

	/* Delayed Word-of-Recall */
	if (p_ptr->word_recall)
	{
		/*
		 * HACK: Autosave BEFORE resetting the recall counter (rr9)
		 * The player is yanked up/down as soon as
		 * he loads the autosaved game.
		 */
		if (autosave_l && (p_ptr->word_recall == 1))
			do_cmd_save_game(TRUE);

		/* Count down towards recall */
		p_ptr->word_recall--;

		p_ptr->redraw |= (PR_STATUS);

		/* Activate the recall */
		if (!p_ptr->word_recall)
		{
			/* Disturbing! */
			disturb(0, 0);

			/* Determine the level */
			if (dun_level || p_ptr->inside_quest)
			{
#ifdef JP
				msg_print("上に引っ張りあげられる感じがする！");
#else
				msg_print("You feel yourself yanked upwards!");
#endif

				dun_level = 0;

				leave_quest_check();

				p_ptr->inside_quest = 0;
				p_ptr->leaving = TRUE;
			}
			else
			{
#ifdef JP
				msg_print("下に引きずり降ろされる感じがする！");
#else
				msg_print("You feel yourself yanked downwards!");
#endif


				/* New depth */
				dun_level = p_ptr->max_dlv;

				if (dun_level < 1) dun_level = 1;

				/* Nightmare mode makes recall more dangerous */
				if (ironman_nightmare && !randint0(666))
				{
					if (dun_level < 50)
					{
						dun_level *= 2;
					}
					else if (dun_level < 99)
					{
						dun_level = (dun_level + 99) / 2;
					}
					else if (dun_level > 100)
					{
						dun_level = MAX_DEPTH - 1;
					}
				}

				/* Save player position */
				p_ptr->oldpx = px;
				p_ptr->oldpy = py;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}

			/* Sound */
			sound(SOUND_TPLEVEL);
		}
	}
}



/*
 * Verify use of "wizard" mode
 */
static bool enter_wizard_mode(void)
{
	/* Ask first time */
	if (!noscore)
	{
		/* Wizard mode is not permitted */
		if (!allow_debug_opts)
		{
#ifdef JP
			msg_print("ウィザードモードは許可されていません。 ");
#else
			msg_print("Wizard mode is not permitted.");
#endif
			return FALSE;
		}

		/* Mention effects */
#ifdef JP
		msg_print("ウィザードモードはデバッグと実験のためのモードです。 ");
		msg_print("一度ウィザードモードに入るとスコアは記録されません。");
#else
		msg_print("Wizard mode is for debugging and experimenting.");
		msg_print("The game will not be scored if you enter wizard mode.");
#endif
		msg_print(NULL);

		/* Verify request */
#ifdef JP
		if (!get_check("本当にウィザードモードに入りたいのですか? "))
#else
		if (!get_check("Are you sure you want to enter wizard mode? "))
#endif
		{
			return (FALSE);
		}

		/* Mark savefile */
		noscore |= 0x0002;

		if (take_notes)
		{
#ifdef JP
			add_note("ウィザードモード", 'W');
#else
			add_note("Wizard mode", 'W');
#endif
		}
	}

	/* Success */
	return (TRUE);
}


#ifdef ALLOW_WIZARD

/*
 * Verify use of "debug" commands
 */
static bool enter_debug_mode(void)
{
	/* Ask first time */
	if (!noscore)
	{
		/* Debug mode is not permitted */
		if (!allow_debug_opts)
		{
#ifdef JP
			msg_print("デバッグコマンドは許可されていません。 ");
#else
			msg_print("Use of debug command is not permitted.");
#endif
			return FALSE;
		}

		/* Mention effects */
#ifdef JP
		msg_print("デバッグ・コマンドはデバッグと実験のためのコマンドです。 ");
		msg_print("デバッグ・コマンドを使うとスコアは記録されません。");
#else
		msg_print("The debug commands are for debugging and experimenting.");
		msg_print("The game will not be scored if you use debug commands.");
#endif
		msg_print(NULL);

		/* Verify request */
#ifdef JP
		if (!get_check("本当にデバッグ・コマンドを使いますか? "))
#else
		if (!get_check("Are you sure you want to use debug commands? "))
#endif
		{
			return (FALSE);
		}

		/* Mark savefile */
		noscore |= 0x0008;

		if (take_notes)
		{
#ifdef JP
			add_note("デバッグモード", 'W');
#else
			add_note("Debug mode", 'W');
#endif
		}
	}

	/* Success */
	return (TRUE);
}

/*
 * Hack -- Declare the Debug Routines
 */
extern void do_cmd_debug(void);

#endif /* ALLOW_WIZARD */


#ifdef ALLOW_BORG

/*
 * Verify use of "borg" commands
 */
static bool enter_borg_mode(void)
{
	/* Ask first time */
	if (!(noscore & 0x0010))
	{
		/* Mention effects */
#ifdef JP
		msg_print("ボーグ・コマンドはデバッグと実験のためのコマンドです。 ");
		msg_print("ボーグ・コマンドを使うとスコアは記録されません。");
#else
		msg_print("The borg commands are for debugging and experimenting.");
		msg_print("The game will not be scored if you use borg commands.");
#endif

		msg_print(NULL);

		/* Verify request */
#ifdef JP
		if (!get_check("本当にボーグ・コマンドを使いますか? "))
#else
		if (!get_check("Are you sure you want to use borg commands? "))
#endif
		{
			return (FALSE);
		}

		/* Mark savefile */
		noscore |= 0x0010;
	}

	/* Success */
	return (TRUE);
}

/*
 * Hack -- Declare the Ben Borg
 */
extern void do_cmd_borg(void);

#endif /* ALLOW_BORG */



/*
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 *
 * XXX XXX XXX Make some "blocks"
 */
static void process_command(void)
{
	int old_now_message = p_ptr->now_message;

	/* Handle repeating the last command */
	repeat_check();

	/* Reset current message line */
	p_ptr->now_message = 0;

	/* Parse the command */
	switch (command_cmd)
	{
		/* Ignore */
		case ESCAPE:
		case ' ':
		{
			break;
		}

		/* Ignore return */
		case '\r':
		case '\n':
		{
			break;
		}

		/*** Wizard Commands ***/

		/* Toggle Wizard Mode */
		case KTRL('W'):
		{
			if (wizard)
			{
				wizard = FALSE;
#ifdef JP
				msg_print("ウィザードモード解除。");
#else
				msg_print("Wizard mode off.");
#endif

			}
			else if (enter_wizard_mode())
			{
				wizard = TRUE;
#ifdef JP
				msg_print("ウィザードモード突入。");
#else
				msg_print("Wizard mode on.");
#endif

			}

			/* Update monsters */
			p_ptr->update |= (PU_MONSTERS);

			/* Redraw "title" */
			p_ptr->redraw |= (PR_TITLE);

			break;
		}


#ifdef ALLOW_WIZARD

		/* Special "debug" commands */
		case KTRL('A'):
		{
			/* Enter debug mode */
			if (enter_debug_mode())
			{
				do_cmd_debug();
			}
			break;
		}

#endif /* ALLOW_WIZARD */


#ifdef ALLOW_BORG

		/* Special "borg" commands */
		case KTRL('Z'):
		{
			/* Enter borg mode */
			if (enter_borg_mode())
			{
				do_cmd_borg();
			}

			break;
		}

#endif /* ALLOW_BORG */



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
		case KTRL('I'):
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

		/* Move (usually pick up things) */
		case ';':
		{
			do_cmd_walk(FALSE);
			break;
		}

		/* Move (usually do not pick up) */
		case '-':
		{
			do_cmd_walk(TRUE);
			break;
		}


		/*** Running, Resting, Searching, Staying */

		/* Begin Running -- Arg is Max Distance */
		case '.':
		{
			do_cmd_run();
			break;
		}

		/* Stay still (usually pick things up) */
		case ',':
		{
			do_cmd_stay(always_pickup);
			break;
		}

		/* Stay still (usually do not pick up) */
		case 'g':
		{
			do_cmd_stay(!always_pickup);
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
		case SPECIAL_KEY_STORE:
		{
			do_cmd_store();
			break;
		}

		/* Enter building -KMW- */
		case SPECIAL_KEY_BUILDING:
		{
			do_cmd_bldg();
			break;
		}

		/* Enter quest level -KMW- */
		case SPECIAL_KEY_QUEST:
		{
			do_cmd_quest();
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
			if (!p_ptr->inside_arena)
			{
				/* -KMW- */
				if (p_ptr->anti_magic)
				{
#ifdef JP
					cptr which_power = "魔法";
#else
					cptr which_power = "magic";
#endif
					if (mp_ptr->spell_type == ST_PRAYER)
#ifdef JP
						which_power = "祈り";
#else
						which_power = "prayer";
#endif

#ifdef JP
					msg_format("反魔法バリアが%sを邪魔した！", which_power);
#else
					msg_format("An anti-magic shell disrupts your %s!", which_power);
#endif
					energy_use = 0;
				}
				else
				{
					do_cmd_cast();
				}
			}
			else
			{
#ifdef JP
				msg_print("アリーナが魔法を吸収した！");
#else
				msg_print("The arena absorbs all attempted magic!");
#endif
				msg_print(NULL);
			}
			break;
		}

		/* Issue a pet command */
		case 'p':
		{
			do_cmd_pet();
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
			if (!p_ptr->inside_arena)
				do_cmd_activate();
			else
			{
#ifdef JP
				msg_print("アリーナが魔法を吸収した！");
#else
				msg_print("The arena absorbs all attempted magic!");
#endif

				msg_print(NULL);
			}
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
			if (!p_ptr->inside_arena)
				(void)do_cmd_fire();
			else
			{
#ifdef JP
				msg_print("アリーナでは真っ向勝負だ！");
#else
				msg_print("You're in the arena now. This is hand-to-hand!");
#endif

				msg_print(NULL);
			}
			break;
		}

		/* Throw an item */
		case 'v':
		{
			if (!p_ptr->inside_arena)
				do_cmd_throw(1);
			else
			{
#ifdef JP
				msg_print("アリーナでは真っ向勝負だ！");
#else
				msg_print("You're in the arena now. This is hand-to-hand!");
#endif

				msg_print(NULL);
			}
			break;
		}

		/* Aim a wand */
		case 'a':
		{
			if (!p_ptr->inside_arena)
				do_cmd_aim_wand();
			else
			{
#ifdef JP
				msg_print("アリーナが魔法を吸収した！");
#else
				msg_print("The arena absorbs all attempted magic!");
#endif

				msg_print(NULL);
			}
			break;
		}

		/* Zap a rod */
		case 'z':
		{
			if (use_command && rogue_like_commands)
			{
				do_cmd_use();
			}
			else if (!p_ptr->inside_arena)
			{
				do_cmd_zap_rod();
			}
			else
			{
#ifdef JP
				msg_print("アリーナが魔法を吸収した！");
#else
				msg_print("The arena absorbs all attempted magic!");
#endif

				msg_print(NULL);
			}
			break;
		}

		/* Quaff a potion */
		case 'q':
		{
			if (!p_ptr->inside_arena)
				do_cmd_quaff_potion();
			else
			{
#ifdef JP
				msg_print("アリーナが魔法を吸収した！");
#else
				msg_print("The arena absorbs all attempted magic!");
#endif

				msg_print(NULL);
			}
			break;
		}

		/* Read a scroll */
		case 'r':
		{
			if (!p_ptr->inside_arena)
				do_cmd_read_scroll();
			else
			{
#ifdef JP
				msg_print("アリーナが魔法を吸収した！");
#else
				msg_print("The arena absorbs all attempted magic!");
#endif

				msg_print(NULL);
			}
			break;
		}

		/* Use a staff */
		case 'u':
		{
			if (use_command && !rogue_like_commands)
			{
				do_cmd_use();
			}
			else if (!p_ptr->inside_arena)
				do_cmd_use_staff();
			else
			{
#ifdef JP
				msg_print("アリーナが魔法を吸収した！");
#else
				msg_print("The arena absorbs all attempted magic!");
#endif

				msg_print(NULL);
			}
			break;
		}

		/* Use racial power */
		case 'U':
		{
			if (!p_ptr->inside_arena)
				do_cmd_racial_power();
			else
			{
#ifdef JP
				msg_print("アリーナが魔法を吸収した！");
#else
				msg_print("The arena absorbs all attempted magic!");
#endif

				msg_print(NULL);
			}
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
			do_cmd_character();
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

		/* XTRA HACK AUTOPICK*/
		case '$':
		{
			do_cmd_reload_autopick();
			break;
		}

		case '_':
		{
			do_cmd_edit_autopick();
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
			(void)combine_and_reorder_home();
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
#ifdef TRAVEL
			do_cmd_travel();
#else
			do_cmd_message_one();
#endif
			break;
		}

		/* Show previous messages */
		case KTRL('P'):
		{
			do_cmd_messages(old_now_message);
			break;
		}

		/* Show quest status -KMW- */
		case KTRL('Q'):
		{
			do_cmd_checkquest();
			break;
		}

		/* Redraw the screen */
		case KTRL('R'):
		{
			p_ptr->now_message = old_now_message;
			do_cmd_redraw();
			break;
		}

#ifndef VERIFY_SAVEFILE

		/* Hack -- Save and don't quit */
		case KTRL('S'):
		{
			do_cmd_save_game(FALSE);
			break;
		}

#endif /* VERIFY_SAVEFILE */

		case KTRL('T'):
		{
			do_cmd_time();
			break;
		}

		/* Save and quit */
		case KTRL('X'):
		case SPECIAL_KEY_QUIT:
		{
			do_cmd_save_and_exit();
			break;
		}

		/* Quit (commit suicide) */
		case 'Q':
		{
			do_cmd_suicide();
			break;
		}
		
		/* Check artifacts, uniques, objects */
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

		case ']':
		{
			prepare_movie_hooks();
			break;
		}

		/* Hack -- Unknown command */
		default:
		{
			if (randint1(2) == 1)
			{
				char error_m[1024];
 				sound(SOUND_BELL);  /* bell is the default system error sound */
#ifdef JP
				if (!get_rnd_line("error_j.txt", 0, error_m))
#else
				if (!get_rnd_line("error.txt", 0, error_m))
#endif

					msg_print(error_m);
			}
			else
#ifdef JP
				prt(" '?' でヘルプが表示されます。", 0, 0);
#else
				prt("Type '?' for help.", 0, 0);
#endif

			break;
		}
	}

	/* Restore current message line */
	if (!energy_use && !p_ptr->now_message)
		p_ptr->now_message = old_now_message;
}




/* Hack -- Pack Overflow */
static void pack_overflow(void)
{
	/* Hack -- Pack Overflow */
	if (inventory[INVEN_PACK].k_idx)
	{
		char o_name[MAX_NLEN];
		object_type *o_ptr;

		/* Is auto-destroy done? */
		notice_stuff();
		if (!inventory[INVEN_PACK].k_idx) return;

		/* Access the slot to be dropped */
		o_ptr = &inventory[INVEN_PACK];

		/* Disturbing */
		disturb(0, 0);

		/* Warning */
#ifdef JP
		msg_print("ザックからアイテムがあふれた！");
#else
		msg_print("Your pack overflows!");
#endif

		/* Describe */
		object_desc(o_name, o_ptr, 0);

		/* Message */
#ifdef JP
		msg_format("%s(%c)を落とした。", o_name, index_to_label(INVEN_PACK));
#else
		msg_format("You drop %s (%c).", o_name, index_to_label(INVEN_PACK));
#endif

		/* Drop it (carefully) near the player */
		(void)drop_near(o_ptr, 0, py, px);

		/* Modify, Describe, Optimize */
		inven_item_increase(INVEN_PACK, -255);
		inven_item_describe(INVEN_PACK);
		inven_item_optimize(INVEN_PACK);

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();
	}
}


/*
 * Process the player
 *
 * Notice the annoying code to handle "pack overflow", which
 * must come first just in case somebody manages to corrupt
 * the savefiles by clever use of menu commands or something.
 */
static void process_player(void)
{
	int i;

	if (hack_mutation)
	{
#ifdef JP
		msg_print("何か変わった気がする！");
#else
		msg_print("You feel different!");
#endif
		(void)gain_random_mutation(0);
		hack_mutation = FALSE;
	}

	if (!command_rep) prt_time();

	/*** Check for interupts ***/

	/* Complete resting */
	if (resting < 0)
	{
		/* Basic resting */
		if (resting == -1)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) &&
			    (p_ptr->csp >= p_ptr->msp))
			{
				disturb(0, 0);
			}
		}

		/* Complete resting */
		else if (resting == -2)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) &&
			    (p_ptr->csp == p_ptr->msp) &&
			    !p_ptr->blind && !p_ptr->confused &&
			    !p_ptr->poisoned && !p_ptr->afraid &&
			    !p_ptr->stun && !p_ptr->cut &&
			    !p_ptr->slow && !p_ptr->paralyzed &&
			    !p_ptr->image && !p_ptr->word_recall)
			{
				disturb(0, 0);
			}
		}
	}

	/* Handle "abort" */
	if (!avoid_abort)
	{
		/* Check for "player abort" (semi-efficiently for resting) */
		if (running || command_rep || (resting && !(resting & 0x0F)))
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
#ifdef JP
				msg_print("中断しました。");
#else
				msg_print("Cancelled.");
#endif
			}
		}
	}


	/*** Handle actual user input ***/

	/* Repeat until energy is reduced */
	while (TRUE)
	{
		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();


		/* Place the cursor on the player */
		move_cursor_relative(py, px);

		/* Refresh (optional) */
		if (fresh_before) Term_fresh();

		/* Hack -- Pack Overflow */
		pack_overflow();


		/* Hack -- cancel "lurking browse mode" */
		if (!command_new) command_see = FALSE;


		/* Assume free turn */
		energy_use = 0;


		/* Paralyzed or Knocked Out */
		if (p_ptr->paralyzed || (p_ptr->stun >= 100))
		{
			/* Take a turn */
			energy_use = 100;
		}

		/* Resting */
		else if (resting)
		{
			/* Timed rest */
			if (resting > 0)
			{
				/* Reduce rest count */
				resting--;

				/* Redraw the state */
				p_ptr->redraw |= (PR_STATE);
			}

			/* Take a turn */
			energy_use = 100;
		}

		/* Running */
		else if (running)
		{
			/* Take a step */
			run_step(0);
		}

#ifdef TRAVEL
		/* Traveling */
		else if (travel.run)
		{
			/* Take a step */
			travel_step();
		}
#endif

		/* Repeated command */
		else if (command_rep)
		{
			/* Count this execution */
			command_rep--;

			/* Redraw the state */
			p_ptr->redraw |= (PR_STATE);

			/* Redraw stuff */
			redraw_stuff();

			/* Hack -- Assume messages were seen */
			msg_flag = FALSE;

			/* Clear the top line */
			prt("", 0, 0);

			/* Process the command */
			process_command();
		}

		/* Normal command */
		else
		{
			/* Place the cursor on the player */
			move_cursor_relative(py, px);

			can_save = TRUE;
			/* Get a command (normal) */
			request_command(FALSE);
			can_save = FALSE;

			/* Process the command */
			process_command();
		}


		/* Hack -- Pack Overflow */
		pack_overflow();


		/*** Clean up ***/

		/* Significant */
		if (energy_use)
		{
			/* Use some energy */
			if (ironman_hengband)
				p_ptr->energy_need += (s16b)((s32b)energy_use * ENERGY_NEED() / 100L);
			else
				p_ptr->energy_need += energy_use;

			/* Show objects on floor in subwinodw*/
			look_y = py;
			look_x = px;
			p_ptr->window |= (PW_FLOOR);

			/* Hack -- constant hallucination */
			if (p_ptr->image) p_ptr->redraw |= (PR_MAP);


			/* Shimmer monsters if needed */
			if (shimmer_monsters)
			{
				/* Clear the flag */
				shimmer_monsters = FALSE;

				/* Shimmer multi-hued monsters */
				for (i = 1; i < m_max; i++)
				{
					monster_type *m_ptr;
					monster_race *r_ptr;

					/* Access monster */
					m_ptr = &m_list[i];

					/* Skip dead monsters */
					if (!m_ptr->r_idx) continue;

					/* Access the monster race */
					r_ptr = &r_info[m_ptr->r_idx];

					/* Skip non-multi-hued monsters */
					if (!(r_ptr->flags1 & RF1_ATTR_MULTI)) continue;

					/* Reset the flag */
					shimmer_monsters = TRUE;

					/* Redraw regardless */
					lite_spot(m_ptr->fy, m_ptr->fx);
				}
			}


			/* Handle monster detection */
			if (repair_monsters)
			{
				/* Reset the flag */
				repair_monsters = FALSE;

				/* Rotate detection flags */
				for (i = 1; i < m_max; i++)
				{
					monster_type *m_ptr;

					/* Access monster */
					m_ptr = &m_list[i];

					/* Skip dead monsters */
					if (!m_ptr->r_idx) continue;

					/* Nice monsters get mean */
					if (m_ptr->mflag & MFLAG_NICE)
					{
						/* Nice monsters get mean */
						m_ptr->mflag &= ~(MFLAG_NICE);
					}

					/* Handle memorized monsters */
					if (m_ptr->mflag & MFLAG_MARK)
					{
						/* Maintain detection */
						if (m_ptr->mflag & MFLAG_SHOW)
						{
							/* Forget flag */
							m_ptr->mflag &= ~(MFLAG_SHOW);

							/* Still need repairs */
							repair_monsters = TRUE;
						}

						/* Remove detection */
						else
						{
							/* Forget flag */
							m_ptr->mflag &= ~(MFLAG_MARK);

							/* Assume invisible */
							m_ptr->ml = FALSE;

							/* Update the monster */
							update_mon(i, FALSE);

							if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);

							/* Redraw regardless */
							lite_spot(m_ptr->fy, m_ptr->fx);
						}
					}
				}
			}
		}


		/* Hack -- notice death */
		if (!alive || death) break;

		/* Handle "leaving" */
		if (p_ptr->leaving) break;
		
		/* Used up energy for this turn */
		if (energy_use) 
		{
			break;
		}
	}
}

/*
 * Add energy to player and monsters.
 * Those with the most energy move first.
 * (This prevents monsters like Morgoth getting double moves
 * when he is at a lower speed than the player.)
 */
static void process_energy(void)
{
	int i, speed, e;
	monster_type *m_ptr;

	if (not_gain_energy) not_gain_energy = FALSE;
	else
	{
		/*** Apply energy to player ***/
		if (p_ptr->pspeed > 199) i = 49;
		else if (p_ptr->pspeed < 0) i = 1;
		else i = extract_energy[p_ptr->pspeed];

		p_ptr->energy_need -= i;

		/* Give energy to all monsters */
		for (i = m_max - 1; i >= 1; i--)
		{
			/* Access the monster */
			m_ptr = &m_list[i];

			/* Ignore "dead" monsters */
			if (!m_ptr->r_idx) continue;

			speed = m_ptr->mspeed;

			/* Monsters move quickly in Nightmare mode */
			if (ironman_nightmare)
			{
				speed = MIN(199, m_ptr->mspeed + 5);
			}

			if (m_ptr->hasted) speed = MIN(199, speed + 10);
			if (m_ptr->slowed) speed = MAX(0, speed - 10);

			e = extract_energy[speed];

			/* Give this monster some energy */
			m_ptr->energy_need -= e;
		}
	}

	/* Can the player move? */
	while (p_ptr->energy_need <= 0 && !p_ptr->leaving)
	{
		/* process monster with even more energy first */
		process_monsters(p_ptr->energy_need - 1);

		/* Process the player while still alive */
		if (!p_ptr->leaving) process_player();
	}
}


/*
 * Interact with the current dungeon level.
 *
 * This function will not exit until the level is completed,
 * the user dies, or the game is terminated.
 */
static void dungeon(void)
{
	int quest_num;

	/* Set the base level */
	base_level = dun_level;

	/* Reset various flags */
	hack_mind = FALSE;

	/* Not leaving */
	p_ptr->leaving = FALSE;

	/* Reset the "command" vars */
	command_cmd = 0;
	command_new = 0;
	command_rep = 0;
	command_arg = 0;
	command_dir = 0;


	/* Cancel the target */
	target_who = 0;

	/* Cancel the health bar */
	health_track(0);


	/* Check visual effects */
	shimmer_monsters = TRUE;
	shimmer_objects = TRUE;
	repair_monsters = TRUE;
	repair_objects = TRUE;


	/* Disturb */
	disturb(1, 0);

	/* Hack -- Handle Failed Random Quest */
	check_failed_random_quest();

	/* Get index of current quest (if any) */
	quest_num = quest_number(dun_level);

	/* Inside a quest? */
	if (quest_num)
	{
		/* Mark the quest monster */
		r_info[quest[quest_num].r_idx].flags1 |= RF1_QUESTOR;
	}

	/* Track maximum player level */
	if (p_ptr->max_plv < p_ptr->lev)
	{
		p_ptr->max_plv = p_ptr->lev;
	}


	/* Track maximum dungeon level (if not in quest -KMW-) */
	if ((p_ptr->max_dlv < dun_level) && !p_ptr->inside_quest)
	{
		p_ptr->max_dlv = dun_level;
	}

	/* No stairs down from Quest */
	if (quest_number(dun_level))
	{
		create_down_stair = FALSE;
	}

	/* Paranoia -- no stairs from town or wilderness */
	if (!dun_level) create_down_stair = create_up_stair = FALSE;

	/* Option -- no connected stairs */
	if (!dungeon_stair) create_down_stair = create_up_stair = FALSE;

	/* Option -- no up stairs */
	if (ironman_downward) create_down_stair = create_up_stair = FALSE;

	/* Make a stairway. */
	if (create_up_stair || create_down_stair)
	{
		/* Place a stairway */
		if (cave_valid_bold(py, px))
		{
			/* XXX XXX XXX */
			delete_object(py, px);

			/* Make stairs */
			if (create_down_stair)
			{
				if (create_down_stair == 2) cave_set_feat(py, px, FEAT_MORE_MORE);
				else cave_set_feat(py, px, FEAT_MORE);
			}
			else
			{
				if (create_up_stair == 2) cave_set_feat(py, px, FEAT_LESS_LESS);
				else cave_set_feat(py, px, FEAT_LESS);
			}
		}

		/* Cancel the stair request */
		create_down_stair = create_up_stair = FALSE;
	}

	/* Validate the panel */
	panel_bounds_center();

	/* Verify the panel */
	verify_panel();

	/* Flush messages */
	msg_print(NULL);


	/* Enter "xtra" mode */
	character_xtra = TRUE;

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);

	/* Redraw dungeon */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_EQUIPPY);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER | PW_STATS);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER | PW_MESSAGE);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	/* Handle stuff */
	handle_stuff();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_DISTANCE | PU_MONSTERS | PU_MON_LITE);

	/* Handle stuff */
	handle_stuff();

	/* Leave "xtra" mode */
	character_xtra = FALSE;

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Notice stuff */
	notice_stuff();

	/* Handle stuff */
	handle_stuff();

	/* Refresh */
	Term_fresh();

	/* Hack -- notice death or departure */
	if (!alive || death) return;

	/* Print quest message if appropriate */
	if (!p_ptr->inside_quest)
	{
		quest_discovery(random_quest_number(dun_level));
		p_ptr->inside_quest = random_quest_number(dun_level);
	}

	/*** Process this dungeon level ***/

	/* Reset the monster generation level */
	monster_level = base_level;

	/* Reset the object generation level */
	object_level = base_level;

	hack_mind = TRUE;

	/* Prevent insta-death */
	if (p_ptr->energy_need > 0 && dun_level) p_ptr->energy_need = 0;

	/* Main loop */
	while (TRUE)
	{
		int i;

		/* Hack -- Compact the monster list occasionally */
		if (m_cnt + 32 > max_m_idx) compact_monsters(64);

		/* Hack -- Compress the monster list occasionally */
		if (m_cnt + 32 < m_max) compact_monsters(0);


		/* Hack -- Compact the object list occasionally */
		if (o_cnt + 32 > max_o_idx) compact_objects(64);

		/* Hack -- Compress the object list occasionally */
		if (o_cnt + 32 < o_max) compact_objects(0);


		/*
		 * Add energy to player and monsters.
		 * Those with the most energy move first.
		 */
		process_energy();

		/* Notice stuff */
		notice_stuff();

		/* Similar slot? */
		for (i = 0; i < INVEN_PACK; i++)
		{
			object_type *j_ptr = &inventory[i];

			/* Skip non-objects */
			if (!j_ptr->k_idx) continue;
		}

		/* Handle stuff */
		handle_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(py, px);

		/* Optional fresh */
		if (fresh_after) Term_fresh();

		/* Hack -- Notice death or departure */
		if (!alive || death) break;

		/* Process all of the monsters */
		process_monsters(0);

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(py, px);

		/* Optional fresh */
		if (fresh_after) Term_fresh();

		/* Hack -- Notice death or departure */
		if (!alive || death) break;

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Process the world */
		process_world();

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(py, px);

		/* Optional fresh */
		if (fresh_after) Term_fresh();

		/* Hack -- Notice death or departure */
		if (!alive || death) break;

		/* Handle "leaving" */
		if (p_ptr->leaving) break;


		/* Count game turns */
		turn++;
	}

	/* Inside a quest and non-unique questor? */
	if (quest_num && !(r_info[quest[quest_num].r_idx].flags1 & RF1_UNIQUE))
	{
		/* Un-mark the quest monster */
		r_info[quest[quest_num].r_idx].flags1 &= ~RF1_QUESTOR;
	}
}


/*
 * Load some "user pref files"
 *
 * Modified by Arcum Dagsson to support
 * separate macro files for different realms.
 */
static void load_all_pref_files(void)
{
	char buf[1024];

	/* Access the "race" pref file */
	sprintf(buf, "%s.prf", rp_ptr->title);

	/* Process that file */
	process_pref_file(buf);

	/* Access the "class" pref file */
	sprintf(buf, "%s.prf", cp_ptr->title);

	/* Process that file */
	process_pref_file(buf);

	/* Access the "character" pref file */
	sprintf(buf, "%s.prf", player_base);

	/* Process that file */
	process_pref_file(buf);

	/* Access the "realm 1" pref file */
	if ((realm_choices1[p_ptr->pclass]) && (p_ptr->realm1 != REALM_NONE))
	{
		sprintf(buf, "%s.prf", realm_names[p_ptr->realm1]);

		/* Process that file */
		process_pref_file(buf);
	}

	/* Access the "realm 2" pref file */
	if (p_ptr->realm2 != REALM_NONE)
	{
		sprintf(buf, "%s.prf", realm_names[p_ptr->realm2]);

		/* Process that file */
		process_pref_file(buf);
	}


	/* Load an autopick preference file */
	autopick_load_pref(FALSE);
}


/*
 * Actually play a game
 *
 * If the "new_game" parameter is true, then, after loading the
 * savefile, we will commit suicide, if necessary, to allow the
 * player to start a new game.
 */
void play_game(bool new_game)
{
	int i;

	hack_mutation = FALSE;

	/* Hack -- Character is "icky" */
	character_icky = TRUE;


	/* Verify main term */
	if (!angband_term[0])
	{
#ifdef JP
		quit("メイン・ウィンドウが存在しません");
#else
		quit("main window does not exist");
#endif
	}

	/* Make sure main term is active */
	Term_activate(angband_term[0]);

	/* Verify minimum size */
	if ((Term->hgt < 24) || (Term->wid < 80))
	{
#ifdef JP
		quit("メイン・ウィンドウが小さすぎます");
#else
		quit("main window is too small");
#endif
	}

	/* Hack -- Turn off the cursor */
	(void)Term_set_cursor(0);

	/* for chuukei */
	if (chuukei_client)
	{
		reset_visuals();
		browse_chuukei();
		return;
	}
	else if (chuukei_server)
	{
		prepare_chuukei_hooks();
	}

	if (browsing_movie)
	{
		reset_visuals();
		browse_movie();
		return;
	}

	/* Make sure main term is active */
	Term_activate(angband_term[0]);

	/* Initialise the resize hooks */
	angband_term[0]->resize_hook = resize_map;
	
	for (i = 1; i < 8; i++)
	{
		/* Does the term exist? */
		if (angband_term[i])
		{
			/* Add the redraw on resize hook */
			angband_term[i]->resize_hook = redraw_window;
		}
	}

	/* Attempt to load */
	if (!load_player())
	{
		/* Oops */
#ifdef JP
		quit("セーブファイルが壊れています");
#else
		quit("broken savefile");
#endif
	}

	/* Nothing loaded */
	if (!character_loaded)
	{
		/* Make new player */
		new_game = TRUE;

		/* The dungeon is not ready */
		character_dungeon = FALSE;

		/* Prepare to init the RNG */
		Rand_quick = TRUE;
	}
	/* Prevent gain energy */
	else not_gain_energy = TRUE;

	/* Process old character */
	if (!new_game)
	{
		/* Process the player name */
		process_player_name(FALSE);
	}

	/* Init the RNG */
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

	/* Extract the options */
	for (i = 0; option_info[i].o_desc; i++)
	{
		int os = option_info[i].o_set;
		int ob = option_info[i].o_bit;

		/* Set the "default" options */
		if (option_info[i].o_var)
		{
			/* Set */
			if (option_flag[os] & (1L << ob))
			{
				/* Set */
				(*option_info[i].o_var) = TRUE;
			}

			/* Clear */
			else
			{
				/* Clear */
				(*option_info[i].o_var) = FALSE;
			}
		}
	}

	/* Roll new character */
	if (new_game)
	{
		/* The dungeon is not ready */
		character_dungeon = FALSE;

		/* Start in town */
		dun_level = 0;
		p_ptr->inside_quest = 0;
		p_ptr->inside_arena = 0;

		/* Hack -- seed for flavors */
		seed_flavor = randint0(0x10000000);

		/* Hack -- seed for town layout */
		seed_town = randint0(0x10000000);

		/* Roll up a new character */
		player_birth();
	}

	/* Hack - if note file exists, load it */
	if (!new_game && take_notes)
	{
		add_note_type(NOTE_ENTER_DUNGEON);
	}

	/* Flash a message */
#ifdef JP
	prt("お待ち下さい...", 0, 0);
#else
	prt("Please wait...", 0, 0);
#endif

	/* Flush the message */
	Term_fresh();


	/* Hack -- Enter wizard mode */
	if (arg_wizard && enter_wizard_mode()) wizard = TRUE;

	/* Flavor the objects */
	flavor_init();

	/* Set start time */
	start_time = time(NULL);

	/* Reset map panel */
	panel_row_min = cur_hgt;
	panel_col_min = cur_wid;

	/* Initialize the town-buildings if necessary */
	if (!dun_level && !p_ptr->inside_quest)
	{
		/* Init the wilderness */
		process_dungeon_file("w_info.txt", 0, 0, max_wild_y, max_wild_x);

		/* Init the town */
		init_flags = INIT_ONLY_BUILDINGS;
		process_dungeon_file("t_info.txt", 0, 0, MAX_HGT, MAX_WID);
	}

	/* Initialize vault info */
#ifdef JP
	if (init_v_info()) quit("建築物初期化不能");
#else
	if (init_v_info()) quit("Cannot initialize vaults");
#endif

	/* Generate a dungeon level if needed */
	if (!character_dungeon) generate_cave();


	/* Character is now "complete" */
	character_generated = TRUE;


	/* Hack -- Character is no longer "icky" */
	character_icky = FALSE;


	/* Start game */
	alive = TRUE;

	/* Reset the visual mappings */
	reset_visuals();

	/* Load the "pref" files */
	load_all_pref_files();

	/* Give startup outfit (after loading pref files) */
	if (new_game) player_outfit();

	/* React to changes */
	Term_xtra(TERM_XTRA_REACT, 0);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER | PW_STATS);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_DUNGEON | PW_MONSTER | PW_OBJECT);

	/* Window stuff */
	window_stuff();


	/* Set or clear "rogue_like_commands" if requested */
	if (arg_force_original) rogue_like_commands = FALSE;
	if (arg_force_roguelike) rogue_like_commands = TRUE;

	/* Hack -- Enforce "delayed death" */
	if (p_ptr->chp < 0) death = TRUE;

	(void)combine_and_reorder_home();

	/* Process */
	while (TRUE)
	{
		/* Process the level */
		dungeon();

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();


		/* Cancel the target */
		target_who = 0;

		/* Cancel the health bar */
		health_track(0);


		/* Forget the lite */
		forget_lite();

		/* Forget the view */
		forget_view();

		/* Forget the view */
		clear_mon_lite();

		/* Handle "quit and save" */
		if (!alive && !death) break;

		/* Erase the old cave */
		wipe_o_list();
		wipe_m_list();


		/* XXX XXX XXX */
		msg_print(NULL);

		/* Accidental Death */
		if (alive && death)
		{
			/* Mega-Hack -- Allow player to cheat death */
#ifdef JP
			if ((wizard || cheat_live) && !get_check("死にますか? "))
#else
			if ((wizard || cheat_live) && !get_check("Die? "))
#endif
			{
				/* Mark social class, reset age, if needed */
				if (p_ptr->sc) p_ptr->sc = p_ptr->age = 0;

				/* Increase age */
				p_ptr->age++;

				/* Mark savefile */
				noscore |= 0x0001;

				/* Message */
#ifdef JP
				msg_print("ウィザードモードを発動し、死をごまかした。");
#else
				msg_print("You invoke wizard mode and cheat death.");
#endif
				msg_print(NULL);

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
				(void)set_image(0);
				(void)set_stun(0);
				(void)set_cut(0);

				/* Hack -- Prevent starvation */
				(void)set_food(PY_FOOD_MAX - 1);

				/* Hack -- cancel recall */
				if (p_ptr->word_recall)
				{
					/* Message */
#ifdef JP
					msg_print("張りつめた大気が流れ去った...");
#else
					msg_print("A tension leaves the air around you...");
#endif
					msg_print(NULL);

					/* Hack -- Prevent recall */
					p_ptr->word_recall = 0;
					p_ptr->redraw |= (PR_STATUS);
				}

				/* Note cause of death XXX XXX XXX */
#ifdef JP
				(void)strcpy(died_from, "死の欺き");
#else
				(void)strcpy(died_from, "Cheating death");
#endif

				/* Do not die */
				death = FALSE;

				dun_level = 0;
				p_ptr->inside_arena = 0;
				leaving_quest = 0;
				p_ptr->inside_quest = 0;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}

		/* Handle "death" */
		if (death) break;

		/* Make a new level */
		generate_cave();
	}

	/* Close stuff */
	close_game();

	/* Quit */
	quit(NULL);
}
