/* File: dungeonc */

/* Purpose: Angband game engine */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#define TY_CURSE_CHANCE 200
#define CHAINSWORD_NOISE 100

static bool load = TRUE;
static int wild_regen = 20;

/*
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
static byte value_check_aux1(object_type *o_ptr)
{
	/* Artifacts */
	if (object_is_artifact(o_ptr))
	{
		/* Cursed/Broken */
		if (object_is_cursed(o_ptr) || object_is_broken(o_ptr)) return FEEL_TERRIBLE;

		/* Normal */
		return FEEL_SPECIAL;
	}

	/* Ego-Items */
	if (object_is_ego(o_ptr))
	{
		/* Cursed/Broken */
		if (object_is_cursed(o_ptr) || object_is_broken(o_ptr)) return FEEL_WORTHLESS;

		/* Normal */
		return FEEL_EXCELLENT;
	}

	/* Cursed items */
	if (object_is_cursed(o_ptr)) return FEEL_CURSED;

	/* Broken items */
	if (object_is_broken(o_ptr)) return FEEL_BROKEN;

	if ((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET)) return FEEL_AVERAGE;

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
	if (object_is_cursed(o_ptr)) return FEEL_CURSED;

	/* Broken items (all of them) */
	if (object_is_broken(o_ptr)) return FEEL_BROKEN;

	/* Artifacts -- except cursed/broken ones */
	if (object_is_artifact(o_ptr)) return FEEL_UNCURSED;

	/* Ego-Items -- except cursed/broken ones */
	if (object_is_ego(o_ptr)) return FEEL_UNCURSED;

	/* Good armor bonus */
	if (o_ptr->to_a > 0) return FEEL_UNCURSED;

	/* Good weapon bonuses */
	if (o_ptr->to_h + o_ptr->to_d > 0) return FEEL_UNCURSED;

	/* No feeling */
	return FEEL_NONE;
}


#define SENSE_TYPE_NONE   0
#define SENSE_TYPE_LIGHT  1
#define SENSE_TYPE_HEAVY  2
#define SENSE_TYPE_KNOWN  3
#define SENSE_TYPE_MENTAL 4


static void sense_object_aux(int item, int sense_type)
{
	byte        feel;
	bool        old_known;
	object_type *o_ptr;
	char        o_name[MAX_NLEN];
	int idx;

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

	if (o_ptr->ident & IDENT_TRIED) return;

	/* We know about it already, do not tell us again */
	if (o_ptr->ident & (IDENT_SENSE)) return;

	/* It is fully known, no information needed */
	if ((sense_type != SENSE_TYPE_MENTAL) && object_is_known(o_ptr)) return;

	if (o_ptr->ident & IDENT_MENTAL) return;

	switch (sense_type)
	{
	case SENSE_TYPE_NONE:
	case SENSE_TYPE_LIGHT:
	case SENSE_TYPE_HEAVY:
		/* Check for a feeling */
		switch (sense_type)
		{
		case SENSE_TYPE_NONE:
			feel = FEEL_NONE;
			break;
		case SENSE_TYPE_LIGHT:
			feel = value_check_aux2(o_ptr);
			break;
		case SENSE_TYPE_HEAVY:
			feel = value_check_aux1(o_ptr);
			break;
		}

		if (feel)
		{
			/* Bad luck */
			if ((p_ptr->muta3 & MUT3_BAD_LUCK) && !randint0(13))
			{
				switch (feel)
				{
					case FEEL_TERRIBLE:
					{
						feel = FEEL_SPECIAL;
						break;
					}
					case FEEL_WORTHLESS:
					{
						feel = FEEL_EXCELLENT;
						break;
					}
					case FEEL_CURSED:
					{
						if (sense_type == SENSE_TYPE_HEAVY)
							feel = randint0(3) ? FEEL_GOOD : FEEL_AVERAGE;
						else
							feel = FEEL_UNCURSED;
						break;
					}
					case FEEL_AVERAGE:
					{
						feel = randint0(2) ? FEEL_CURSED : FEEL_GOOD;
						break;
					}
					case FEEL_GOOD:
					{
						if (sense_type == SENSE_TYPE_HEAVY)
							feel = randint0(3) ? FEEL_CURSED : FEEL_AVERAGE;
						else
							feel = FEEL_CURSED;
						break;
					}
					case FEEL_EXCELLENT:
					{
						feel = FEEL_WORTHLESS;
						break;
					}
					case FEEL_SPECIAL:
					{
						feel = FEEL_TERRIBLE;
						break;
					}
				}
			}

			/* Stop everything */
			if (disturb_minor) disturb(0, 0);

			/* Get an object description */
			object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

			/* Message (equipment) */
			if (item >= INVEN_RARM)
			{
#ifdef JP
				msg_format("%s%s(%c)は%sという感じがする...",
				           describe_use(item),o_name, index_to_label(item),game_inscriptions[feel]);
#else
				msg_format("You feel the %s (%c) you are %s %s %s...",
					   o_name, index_to_label(item), describe_use(item),
					   ((o_ptr->number == 1) ? "is" : "are"),
						   game_inscriptions[feel]);
#endif

			}

			/* Message (inventory) */
			else if (item >= 0)
			{
#ifdef JP
				msg_format("ザックの中の%s(%c)は%sという感じがする...",
				           o_name, index_to_label(item),game_inscriptions[feel]);
#else
				msg_format("You feel the %s (%c) in your pack %s %s...",
					   o_name, index_to_label(item),
					   ((o_ptr->number == 1) ? "is" : "are"),
						   game_inscriptions[feel]);
#endif

			}

			/* We have "felt" it */
			o_ptr->ident |= (IDENT_SENSE);
		}
		else feel = FEEL_TRIED;

		/* Set the "inscription" */
		o_ptr->feeling = feel;

		/* Player touches it */
		o_ptr->marked |= OM_TOUCHED;

		/* Auto-inscription/destroy */
		idx = is_autopick(o_ptr);
		auto_inscribe_item(item, idx);
		if (destroy_feeling)
			auto_destroy_item(item, idx);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		break;

	case SENSE_TYPE_KNOWN:
	case SENSE_TYPE_MENTAL:
		/* Identify it */
		old_known = identify_item(o_ptr);

		/* Stop everything */
		if (disturb_minor) disturb(0, 0);

		/* Description */
		object_desc(o_name, o_ptr, 0);

		if (sense_type == SENSE_TYPE_MENTAL)
		{
			/* Mark the item as fully known */
			o_ptr->ident |= (IDENT_MENTAL);

			/* Handle stuff */
			handle_stuff();
		}

		/* Describe */
		if (item >= INVEN_RARM)
		{
#ifdef JP
			msg_format("%^s%s: %s(%c)。", (sense_type == SENSE_TYPE_MENTAL) ? "(*鑑定*)" : "", describe_use(item), o_name, index_to_label(item));
#else
			msg_format("%^s%s: %s (%c).", (sense_type == SENSE_TYPE_MENTAL) ? " (*Identify*)" : "", describe_use(item), o_name, index_to_label(item));
#endif
		}
		else if (item >= 0)
		{
#ifdef JP
			msg_format("ザック中%s: %s(%c)。", (sense_type == SENSE_TYPE_MENTAL) ? "(*鑑定*)" : "", o_name, index_to_label(item));
#else
			msg_format("In your pack%s: %s (%c).", (sense_type == SENSE_TYPE_MENTAL) ? " (*Identify*)" : "", o_name, index_to_label(item));
#endif
		}

		/* Auto-inscription/destroy */
		idx = is_autopick(o_ptr);
		auto_inscribe_item(item, idx);
		if (destroy_identify && !old_known)
			auto_destroy_item(item, idx);

		break;
	}

	/* We have "felt" it */
	o_ptr->ident |= (IDENT_TRIED);
}


static int get_sense_type_arms(void)
{
	int pstat = p_ptr->stat_use[A_WIS];

	/* Analyze the skill level */
	switch (skill_exp_level(p_ptr->skill_exp[SKILL_SENSE_ARMS]/10))
	{
	case SKILL_LEVEL_BEGINNER:
		if (randint0(200) < pstat) return SENSE_TYPE_LIGHT;
		else return SENSE_TYPE_NONE;

	case SKILL_LEVEL_NOVICE:
		if (randint0(200) < pstat) return SENSE_TYPE_HEAVY;
		else return SENSE_TYPE_LIGHT;

	case SKILL_LEVEL_AVERAGE:
		return SENSE_TYPE_HEAVY;

	case SKILL_LEVEL_SKILLED:
		if (randint0(200) < pstat) return SENSE_TYPE_KNOWN;
		else return SENSE_TYPE_HEAVY;

	case SKILL_LEVEL_EXPERT:
		return SENSE_TYPE_KNOWN;

	case SKILL_LEVEL_MASTER:
		return SENSE_TYPE_MENTAL;
	}

	return SENSE_TYPE_NONE;
}


static int get_sense_type_acc(void)
{
	int pstat = p_ptr->stat_use[A_WIS];

	/* Analyze the skill level */
	switch (skill_exp_level(p_ptr->skill_exp[SKILL_SENSE_ACC]/10))
	{
	case SKILL_LEVEL_BEGINNER:
		return SENSE_TYPE_NONE;

	case SKILL_LEVEL_NOVICE:
		if (randint0(200) < pstat) return SENSE_TYPE_HEAVY;
		else return SENSE_TYPE_NONE;

	case SKILL_LEVEL_AVERAGE:
		return SENSE_TYPE_HEAVY;

	case SKILL_LEVEL_SKILLED:
		if (randint0(200) < pstat) return SENSE_TYPE_KNOWN;
		else return SENSE_TYPE_HEAVY;

	case SKILL_LEVEL_EXPERT:
		return SENSE_TYPE_KNOWN;

	case SKILL_LEVEL_MASTER:
		return SENSE_TYPE_MENTAL;
	}

	return SENSE_TYPE_NONE;
}


/*
 * Sense the inventory
 */
static void sense_inventory1(void)
{
	int         i;
	int         sense_type;
	object_type *o_ptr;


	/*** Check for "sensing" ***/

	/* No sensing when confused */
	if (p_ptr->confused) return;

	sense_type = get_sense_type_arms();

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
			case TV_BULLET:
			case TV_ROUND:
			case TV_SHELL:
			case TV_ROCKET:
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
			case TV_CARD:
			{
				okay = TRUE;
				break;
			}
		}

		/* Skip non-sense machines */
		if (!okay) continue;

		/* Occasional failure on inventory items */
		if ((i < INVEN_RARM) && (0 != randint0(5))) continue;

		sense_object_aux(i, sense_type);
	}
}


static void sense_inventory2(void)
{
	int         i;
	int         sense_type;
	object_type *o_ptr;


	/*** Check for "sensing" ***/

	/* No sensing when confused */
	if (p_ptr->confused) return;

	sense_type = get_sense_type_acc();

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
			case TV_LITE:
			case TV_FIGURINE:
			{
				okay = TRUE;
				break;
			}
		}

		/* Skip non-sense machines */
		if (!okay) continue;

		/* Occasional failure on inventory items */
		if ((i < INVEN_RARM) && (0 != randint0(5))) continue;

		sense_object_aux(i, sense_type);
	}
}


void sense_floor_object(int o_idx)
{
	object_type *o_ptr = &o_list[o_idx];

	/* No sensing when confused */
	if (p_ptr->confused) return;

	switch (o_ptr->tval)
	{
	case TV_BULLET:
	case TV_ROUND:
	case TV_SHELL:
	case TV_ROCKET:
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
	case TV_CARD:
		sense_object_aux(0 - o_idx, get_sense_type_arms());
		break;

	case TV_RING:
	case TV_AMULET:
	case TV_LITE:
	case TV_FIGURINE:
		sense_object_aux(0 - o_idx, get_sense_type_acc());
		break;
	}
}


/*
 * Regenerate hit points				-RAK-
 */
static void regenhp(int percent)
{
	s32b new_chp;
	u32b new_chp_frac;
	s32b old_chp;

	/* Save the old hitpoints */
	old_chp = p_ptr->chp;

	/* Extract the new hitpoints */
	new_chp = ((long)p_ptr->mhp) * percent + PY_REGEN_HPBASE;
	p_ptr->chp += (new_chp >> 16);   /* div 65536 */

	/* check for overflow */
	if ((p_ptr->chp < 0) && (old_chp > 0)) p_ptr->chp = MAX_SHORT;
	new_chp_frac = (new_chp & 0xFFFF) + p_ptr->chp_frac;	/* mod 65536 */
	if (new_chp_frac >= 0x10000L)
	{
		p_ptr->chp_frac = new_chp_frac - 0x10000L;
		p_ptr->chp++;
	}
	else
	{
		p_ptr->chp_frac = new_chp_frac;
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

		wild_regen = 20;
	}
}


/*
 * Regenerate mana points				-RAK-
 */
static void regenmana(int percent)
{
	s32b new_mana;
	u32b new_mana_frac;
	s32b old_csp;
	bool    old_csp_msp = (p_ptr->csp > p_ptr->msp);

	if (p_ptr->regenerate_mana) percent *= 2;
	old_csp = p_ptr->csp;
	new_mana = ((long)p_ptr->msp) * percent + PY_REGEN_MNBASE;
	if (old_csp_msp && (new_mana > 0))
	{
		new_mana *= 32;
		p_ptr->csp--;
		p_ptr->csp -= (new_mana >> 16);	/* div 65536 */
		new_mana_frac = p_ptr->csp_frac + 0x10000L - (new_mana & 0xFFFF);
	}
	else
	{
		if (old_csp_msp) new_mana += ((((long)p_ptr->msp) * percent + PY_REGEN_MNBASE) * 32);
		p_ptr->csp += (new_mana >> 16);	/* div 65536 */

		new_mana_frac = (new_mana & 0xFFFF) + p_ptr->csp_frac;	/* mod 65536 */
	}
	if (new_mana_frac >= 0x10000L)
	{
		p_ptr->csp_frac = new_mana_frac - 0x10000L;
		p_ptr->csp++;
	}
	else
	{
		p_ptr->csp_frac = new_mana_frac;
	}

	/* check for overflow */
	if (p_ptr->csp < 0)
	{
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;
	}

	/* Must set frac to zero even if equal */
	if ((old_csp_msp && p_ptr->csp < p_ptr->msp) || (!old_csp_msp && p_ptr->csp >= p_ptr->msp))
	{
		p_ptr->csp = p_ptr->msp;
		p_ptr->csp_frac = 0;
	}

	/* Redraw mana */
	if (old_csp != p_ptr->csp)
	{
		/* Redraw */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		p_ptr->window |= (PW_SPELL);

		wild_regen = 20;
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
			if (!frac) if (one_in_(2)) frac = 1;

			/* Hack -- Some monsters regenerate quickly */
			if (r_ptr->flags2 & RF2_REGENERATE) frac *= 2;

			/* Hack -- Regenerate */
			m_ptr->hp += frac;

			/* Do not over-regenerate */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
			if (p_ptr->riding == i) p_ptr->redraw |= (PR_UHEALTH);
		}
	}
}


/*
 * Regenerate the captured monsters (once per 30 game turns)
 *
 * XXX XXX XXX Should probably be done during monster turns.
 */
static void regen_captured_monsters(void)
{
	int i, frac;
	bool heal = FALSE;

	/* Regenerate everyone */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		monster_race *r_ptr;
		object_type *o_ptr = &inventory[i];

		if (!o_ptr->k_idx) continue;
		if ((o_ptr->tval != TV_CARD) && (o_ptr->tval != SV_MONSTER_CARD)) continue;
		if (!o_ptr->pval) continue;

		heal = TRUE;

		r_ptr = &r_info[o_ptr->pval];

		/* Allow regeneration (if needed) */
		if (o_ptr->xtra4 < o_ptr->xtra5)
		{
			/* Hack -- Base regeneration */
			frac = o_ptr->xtra5 / 100;

			/* Hack -- Minimal regeneration rate */
			if (!frac) if (one_in_(2)) frac = 1;

			/* Hack -- Some monsters regenerate quickly */
			if (r_ptr->flags2 & RF2_REGENERATE) frac *= 2;

			/* Hack -- Regenerate */
			o_ptr->xtra4 += frac;

			/* Do not over-regenerate */
			if (o_ptr->xtra4 > o_ptr->xtra5) o_ptr->xtra4 = o_ptr->xtra5;
		}
	}

	if (heal)
	{
		/* Combine pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN);
		p_ptr->window |= (PW_EQUIP);
		wild_regen = 20;
	}
}


static void notice_lite_change(object_type *o_ptr)
{
	/* Hack -- notice interesting fuel steps */
	if ((o_ptr->xtra4 < 100) || (!(o_ptr->xtra4 % 100)))
	{
		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
	}

	/* Hack -- Special treatment when blind */
	if (p_ptr->blind)
	{
		/* Hack -- save some light for later */
		if (o_ptr->xtra4 == 0) o_ptr->xtra4++;
	}

	/* The light is now out */
	else if (o_ptr->xtra4 == 0)
	{
		disturb(0, 0);
#ifdef JP
		msg_print("明かりが消えてしまった！");
#else
		msg_print("Your light has gone out!");
#endif

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);

		/* Some ego light lose its effects without fuel */
		p_ptr->update |= (PU_BONUS);
	}

	/* The light is getting dim */
	else if (o_ptr->name2 == EGO_LITE_LONG)
	{
		if ((o_ptr->xtra4 < 50) && (!(o_ptr->xtra4 % 5))
		    && (turn % (TURNS_PER_TICK*2)))
		{
			if (disturb_minor) disturb(0, 0);
#ifdef JP
			msg_print("明かりが微かになってきている。");
#else
			msg_print("Your light is growing faint.");
#endif

		}
	}

	/* The light is getting dim */
	else if ((o_ptr->xtra4 < 100) && (!(o_ptr->xtra4 % 10)))
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
	if (leaving_quest && !astral_mode &&
	    ((quest[leaving_quest].flags & QUEST_FLAG_ONCE) || (quest[leaving_quest].type == QUEST_TYPE_RANDOM)) &&
	    (quest[leaving_quest].status == QUEST_STATUS_TAKEN))
	{
		quest[leaving_quest].status = QUEST_STATUS_FAILED;
		quest[leaving_quest].complev = (byte)p_ptr->lev;
		if (quest[leaving_quest].type == QUEST_TYPE_RANDOM)
		{
			r_info[quest[leaving_quest].r_idx].flags1 &= ~(RF1_QUESTOR);
			if (record_rand_quest)
				do_cmd_write_nikki(NIKKI_RAND_QUEST_F, leaving_quest, NULL);

			/* Floor of random quest will be blocked */
			prepare_change_floor_mode(CFM_NO_RETURN);
		}
		else
		{
			if (record_fix_quest) do_cmd_write_nikki(NIKKI_FIX_QUEST_F, leaving_quest, NULL);
			if (quest_is_fixed(leaving_quest)) change_your_alignment(ALI_LNC, -10);
		}

		if (leaving_quest == QUEST_DEATH) p_ptr->death_regen = 999;
	}
}


static void gere_music(s32b music)
{
	int plev = p_ptr->cexp_info[p_ptr->pclass].clev;

	switch (music)
	{
	case MUSIC_SAD:
		project_hack_undead(GF_SOUND, damroll(10 + plev / 5, 7));
		break;
	case MUSIC_SILENT:
		song_of_silence(20 + plev * 2);
		break;
	case MUSIC_TEMPTATION:
		song_of_temptation();
		break;
	}
}

/*
 * If player has inscribed the object with "!!", let him know when it's
 * recharged. -LM-
 */
static void recharged_notice(object_type *o_ptr)
{
	char o_name[MAX_NLEN];

	cptr s;

	/* No inscription */
	if (!o_ptr->inscription) return;

	/* Find a '!' */
	s = my_strchr(quark_str(o_ptr->inscription), '!');

	/* Process notification request. */
	while (s)
	{
		/* Find another '!' */
		if (s[1] == '!')
		{
			/* Describe (briefly) */
			object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

			/* Notify the player */
#ifdef JP
			msg_format("%sは再充填された。", o_name);
#else
			if (o_ptr->number > 1)
				msg_format("Your %s are recharged.", o_name);
			else
				msg_format("Your %s is recharged.", o_name);
#endif

			disturb(0, 0);

			/* Done. */
			return;
		}

		/* Keep looking for '!'s */
		s = my_strchr(s + 1, '!');
	}
}


static void check_music(void)
{
	s32b use_mana;

	/* Music singed by player */
	if (!p_ptr->singing && !p_ptr->restart_singing) return;

	switch (p_ptr->song_start)
	{
	case MUSIC_SAD:
		use_mana = 10;
		break;
	case MUSIC_SILENT:
		use_mana = 15;
		break;
	case MUSIC_TEMPTATION:
		use_mana = 14;
		break;
	default:
		use_mana = 0;
		break;
	}

	if (p_ptr->dec_mana) use_mana = use_mana * 3 / 4;
	if (use_mana < 1) use_mana = 1;
	use_mana *= 0x8000;
	if (((u16b)(p_ptr->csp) < (use_mana / 0x10000)) || p_ptr->anti_magic || is_anti_magic_grid(-1, py, px))
	{
		stop_singing();
		return;
	}
	else
	{
		p_ptr->csp -= (u16b) (use_mana / 0x10000);
		use_mana = (use_mana & 0x0000ffff);
		if (p_ptr->csp_frac < (u32b)use_mana)
		{
			p_ptr->csp--;
			p_ptr->csp_frac += (u16b)(0x10000L - use_mana);
		}
		else
		{
			p_ptr->csp_frac -= (u16b)use_mana;
		}

		p_ptr->redraw |= PR_MANA;
		if (p_ptr->restart_singing)
		{
			p_ptr->singing = p_ptr->restart_singing;
			p_ptr->restart_singing = 0;
#ifdef JP
			msg_print("歌を再開した。");
#else
			msg_print("You restart singing.");
#endif
			p_ptr->action = ACTION_SING;

			/* Recalculate bonuses */
			p_ptr->update |= (PU_BONUS | PU_HP);

			/* Redraw status bar */
			p_ptr->redraw |= (PR_STATUS);
		}
	}

	gere_music(p_ptr->singing);
}

/* Choose one of items that have cursed flag */
static object_type *choose_cursed_obj_name(u32b flag)
{
	int i;
	int choices[INVEN_TOTAL-INVEN_RARM];
	int number = 0;

	/* Paranoia -- Player has no warning-item */
	if (!(p_ptr->cursed & flag)) return NULL;

	/* Search Inventry */
	for (i = INVEN_RARM; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		if (o_ptr->curse_flags & flag)
		{
			choices[number] = i;
			number++;
		}
	}

	/* Choice one of them */
	return (&inventory[choices[randint0(number)]]);
}


/*
 * Handle timed damage and regeneration every 10 game turns
 */
static void process_world_aux_hp_and_sp(void)
{
	bool cave_no_regen = FALSE;
	int upkeep_factor = 0;
	int i;

	/* Default regeneration */
	int regen_amount = PY_REGEN_NORMAL;

	/*** Damage over Time ***/

	/* Take damage from poison */
	if (p_ptr->poisoned && !p_ptr->invuln)
	{
		/* Take damage */
#ifdef JP
		take_hit(DAMAGE_NOESCAPE, 1, "毒");
#else
		take_hit(DAMAGE_NOESCAPE, 1, "poison");
#endif

	}


	/* (Vampires) Take damage from sunlight */
	if (p_ptr->pclass == CLASS_VAMPIRE)
	{
		if (inventory[INVEN_LITE].tval &&
		    (((inventory[INVEN_LITE].sval != SV_LITE_EMPTY) && (inventory[INVEN_LITE].name2 != EGO_LITE_DARKNESS)) ||
		    ((inventory[INVEN_LITE].sval == SV_LITE_EMPTY) && (have_flag(inventory[INVEN_LITE].art_flags, TR_LITE)))) &&
		    !p_ptr->resist_lite)
		{
			object_type * o_ptr = &inventory[INVEN_LITE];
			char o_name [MAX_NLEN];
			char ouch [MAX_NLEN+40];

			/* Get an object description */
			object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

#ifdef JP
			msg_format("%sがあなたのアンデッドの肉体を焼き焦がした！", o_name);
#else
			msg_format("The %s scorches your undead flesh!", o_name);
#endif


			cave_no_regen = TRUE;

			/* Get an object description */
			object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

#ifdef JP
			sprintf(ouch, "%sを装備したダメージ", o_name);
#else
			sprintf(ouch, "wielding %s", o_name);
#endif

			if (!p_ptr->invuln) take_hit(DAMAGE_NOESCAPE, randint1(10), ouch);
		}

		for (i = INVEN_RARM; i <= INVEN_FEET; i++)
		{
			object_type * o_ptr = &inventory[i];
			char o_name [MAX_NLEN];
			char ouch [MAX_NLEN+40];
			u32b flgs[TR_FLAG_SIZE];

			/* Get an object description */
			object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
			object_flags(o_ptr, flgs);

			if (have_flag(flgs, TR_BLESSED))
			{
#ifdef JP
				msg_format("%sがあなたの不浄な肉体を焼き焦がした！", o_name);
#else
				msg_format("The %s scorches your undead flesh!", o_name);
#endif


				cave_no_regen = TRUE;

				/* Get an object description */
				object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

#ifdef JP
				sprintf(ouch, "%sを装備したダメージ", o_name);
#else
				sprintf(ouch, "wielding %s", o_name);
#endif

				if (!p_ptr->invuln) take_hit(DAMAGE_NOESCAPE, 25 + randint1(25), ouch);
			}
		}
	}

	if ((cave[py][px].feat == FEAT_SHAL_LAVA) &&
		!p_ptr->invuln && !p_ptr->immune_fire && !p_ptr->ffall)
	{
		int damage = 3000 + randint0(2000);

		if (p_ptr->resist_fire) damage = damage / 3;
		if (p_ptr->oppose_fire) damage = damage / 3;
		damage = damage / 100 + (randint0(100) < (damage % 100));

		if (damage)
		{
			/* Take damage */
#ifdef JP
			msg_print("溶岩で火傷した！");
			take_hit(DAMAGE_NOESCAPE, damage, "浅い溶岩流");
#else
			msg_print("The lava burns you!");
			take_hit(DAMAGE_NOESCAPE, damage, "shallow lava");
#endif

			cave_no_regen = TRUE;
		}
	}

	else if ((cave[py][px].feat == FEAT_DEEP_LAVA) &&
		!p_ptr->invuln && !p_ptr->immune_fire)
	{
		int damage = 6000 + randint0(4000);

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

		damage = damage / 100 + (randint0(100) < (damage % 100));
		if (damage)
		{
			/* Take damage */
			msg_print(message);
			take_hit(DAMAGE_NOESCAPE, damage, hit_from);

			cave_no_regen = TRUE;
		}
	}

	else if ((cave[py][px].feat == FEAT_DEEP_WATER) && !p_ptr->ffall && !p_ptr->can_swim)
	{
		if (p_ptr->total_weight > (((u32b)adj_str_wgt[p_ptr->stat_ind[A_STR]] * (p_ptr->pclass == CLASS_TERRORKNIGHT ? 150 : 100)) / 2))
		{
			/* Take damage */
#ifdef JP
			msg_print("溺れている！");
			take_hit(DAMAGE_NOESCAPE, randint1(p_ptr->lev), "溺れ");
#else
			msg_print("You are drowning!");
			take_hit(DAMAGE_NOESCAPE, randint1(p_ptr->lev), "drowning");
#endif

			cave_no_regen = TRUE;
		}
	}

	if (p_ptr->riding)
	{
		int riding_level = p_ptr->cexp_info[CLASS_BEASTTAMER].clev + p_ptr->cexp_info[CLASS_DRAGONTAMER].clev;
		if ((p_ptr->pclass == CLASS_BEASTTAMER) || (p_ptr->pclass == CLASS_DRAGONTAMER)) riding_level += 15;

		if (riding_level < 49)
		{
			int damage;
			if ((r_info[m_list[p_ptr->riding].r_idx].flags2 & RF2_AURA_FIRE) && !p_ptr->immune_fire)
			{
				damage = r_info[m_list[p_ptr->riding].r_idx].level / 2;
				if (p_ptr->resist_fire) damage = damage / 3;
				if (p_ptr->oppose_fire) damage = damage / 3;
#ifdef JP
				msg_print("熱い！");
				take_hit(DAMAGE_NOESCAPE, damage, "炎のオーラ");
#else
				msg_print("It's hot!");
				take_hit(DAMAGE_NOESCAPE, damage, "Fire aura");
#endif
			}
			if ((r_info[m_list[p_ptr->riding].r_idx].flags2 & RF2_AURA_ELEC) && !p_ptr->immune_elec)
			{
				damage = r_info[m_list[p_ptr->riding].r_idx].level / 2;
				if (p_ptr->resist_elec) damage = damage / 3;
				if (p_ptr->oppose_elec) damage = damage / 3;
#ifdef JP
				msg_print("痛い！");
				take_hit(DAMAGE_NOESCAPE, damage, "電気のオーラ");
#else
				msg_print("It hurts!");
				take_hit(DAMAGE_NOESCAPE, damage, "Elec aura");
#endif
			}
			if ((r_info[m_list[p_ptr->riding].r_idx].flags3 & RF3_AURA_COLD) && !p_ptr->immune_cold)
			{
				damage = r_info[m_list[p_ptr->riding].r_idx].level / 2;
				if (p_ptr->resist_cold) damage = damage / 3;
				if (p_ptr->oppose_cold) damage = damage / 3;
#ifdef JP
				msg_print("冷たい！");
				take_hit(DAMAGE_NOESCAPE, damage, "冷気のオーラ");
#else
				msg_print("It's cold!");
				take_hit(DAMAGE_NOESCAPE, damage, "Cold aura");
#endif
			}
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
		else if (!p_ptr->invuln && !WRAITH_FORM() &&
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

			take_hit(DAMAGE_NOESCAPE, 1 + (p_ptr->lev / 5), dam_desc);
		}
	}

	/* Take damage from cuts */
	if (p_ptr->cut && !p_ptr->invuln)
	{
		switch (cut_level(p_ptr->cut))
		{
		case 1:
			i = 1;
			break;

		case 2:
			i = 3;
			break;

		case 3:
			i = 7;
			break;

		case 4:
			i = 16;
			break;

		case 5:
			i = 32;
			break;

		case 6:
			i = 80;
			break;

		default:
			i = 200;
			break;
		}

		/* Take damage */
#ifdef JP
		take_hit(DAMAGE_NOESCAPE, i, "致命傷");
#else
		take_hit(DAMAGE_NOESCAPE, i, "a fatal wound");
#endif
	}


	/*** handle regeneration ***/


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
	}


	/* Regeneration ability */
	if (p_ptr->regenerate)
	{
		regen_amount = regen_amount * 2;
	}
	if (p_ptr->cursed & TRC_SLOW_REGEN)
	{
		regen_amount /= 5;
	}

	/* Searching or Resting */
	if ((p_ptr->action == ACTION_SEARCH) || (p_ptr->action == ACTION_REST))
	{
		regen_amount = regen_amount * 2;
	}

	upkeep_factor = calculate_upkeep();

	/* No regeneration while special action */
	if (p_ptr->action == ACTION_AURA) upkeep_factor += 100;

	/* Regenerate the mana */
	if (upkeep_factor)
	{
		s32b upkeep_regen = ((100 - upkeep_factor) * regen_amount);
		regenmana(upkeep_regen/100);

#ifdef TRACK_FRIENDS
		if (p_ptr->wizard)
		{
#ifdef JP
			msg_format("ＭＰ回復: %d/%d", upkeep_regen, regen_amount);
#else
			msg_format("Regen: %d/%d", upkeep_regen, regen_amount);
#endif
		}
#endif /* TRACK_FRIENDS */

	}
	else regenmana(regen_amount);

	if ((p_ptr->csp == 0) && (p_ptr->csp_frac == 0))
	{
		while (upkeep_factor > 100)
		{
#ifdef JP
			msg_print("こんなに多くのペットを制御できない！");
#else
			msg_print("Such much pets cannot be controled at once!");
#endif
			msg_print(NULL);
			do_cmd_pet_dismiss();

			upkeep_factor = calculate_upkeep();

#ifdef JP
			msg_format("維持MPは %d%%", upkeep_factor);
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
	if ((p_ptr->chp < p_ptr->mhp) && !cave_no_regen)
	{
		regenhp(regen_amount);
	}
}


/*
 * Handle timeout every 10 game turns
 */
static void process_world_aux_timeout(void)
{
	const int dec_count = (easy_band ? 2 : 1);

	/*** Timeout Various Things ***/

	/* Hack -- Hallucinating */
	if (p_ptr->image)
	{
		(void)set_image(p_ptr->image - dec_count);
	}

	/* Blindness */
	if (p_ptr->blind)
	{
		(void)set_blind(p_ptr->blind - dec_count);
	}

	/* Times see-invisible */
	if (p_ptr->tim_invis)
	{
		(void)set_tim_invis(p_ptr->tim_invis - 1, TRUE);
	}

	if (multi_rew)
	{
		multi_rew = FALSE;
	}

	/* Timed esp */
	if (p_ptr->tim_esp)
	{
		(void)set_tim_esp(p_ptr->tim_esp - 1, TRUE);
	}

	/* Timed temporary elemental brands. -LM- */
	if (p_ptr->magical_weapon)
	{
		p_ptr->magical_weapon--;

		/* Clear all temporary elemental brands. */
		if (!p_ptr->magical_weapon) set_magical_weapon(0, 0, INVEN_RARM, FALSE);
	}

	/* Timed evil weapon */
	if (p_ptr->evil_weapon)
	{
		(void)set_evil_weapon(p_ptr->evil_weapon - 1, TRUE, INVEN_RARM, FALSE);
	}

	/* Timed infra-vision */
	if (p_ptr->tim_infra)
	{
		(void)set_tim_infra(p_ptr->tim_infra - 1, TRUE);
	}

	/* Timed sh_fire */
	if (p_ptr->tim_sh_fire)
	{
		(void)set_tim_sh_fire(p_ptr->tim_sh_fire - 1, TRUE);
	}

	/* Timed sh_elec */
	if (p_ptr->tim_sh_elec)
	{
		(void)set_tim_sh_elec(p_ptr->tim_sh_elec - 1, TRUE);
	}

	/* Timed sh_cold */
	if (p_ptr->tim_sh_cold)
	{
		(void)set_tim_sh_cold(p_ptr->tim_sh_cold - 1, TRUE);
	}

	/* Timed sh_holy */
	if (p_ptr->tim_sh_holy)
	{
		(void)set_tim_sh_holy(p_ptr->tim_sh_holy - 1, TRUE);
	}

	/* Timed eyeeye */
	if (p_ptr->tim_eyeeye)
	{
		(void)set_tim_eyeeye(p_ptr->tim_eyeeye - 1, TRUE);
	}

	/* Timed inc_blow */
	if (p_ptr->tim_inc_blow)
	{
		(void)set_tim_inc_blow(p_ptr->tim_inc_blow - 1, TRUE);
	}

	/* Timed dec_blow */
	if (p_ptr->tim_dec_blow)
	{
		(void)set_tim_dec_blow(p_ptr->tim_dec_blow - 1, TRUE);
	}

	/* Timed protection of Zoshonel */
	if (p_ptr->zoshonel_protect)
	{
		(void)set_zoshonel_protect(p_ptr->zoshonel_protect - 1, TRUE);
	}

	/* Timed Chargespell */
	if (p_ptr->chargespell)
	{
		(void)set_chargespell(p_ptr->chargespell - 1, TRUE);
	}

	/* Timed resist time */
	if (p_ptr->tim_res_time)
	{
		(void)set_tim_res_time(p_ptr->tim_res_time - 1, TRUE);
	}

	/* Multi-shadow */
	if (p_ptr->multishadow)
	{
		(void)set_multishadow(p_ptr->multishadow - 1, TRUE);
	}

	/* Timed Robe of dust */
	if (p_ptr->dustrobe)
	{
		(void)set_dustrobe(p_ptr->dustrobe - 1, TRUE);
	}

	/* Timed immunity to teleport by others */
	if (p_ptr->earth_spike)
	{
		(void)set_earth_spike(p_ptr->earth_spike - 1, TRUE);
	}

	/* Timed avoidance to arrows */
	if (p_ptr->wind_guard)
	{
		(void)set_wind_guard(p_ptr->wind_guard - 1, TRUE);
	}

	/* Timed avoidance to death (!!) */
	if (p_ptr->tim_resurrection)
	{
		(void)set_tim_resurrection(p_ptr->tim_resurrection - 1, TRUE);
	}

	/* Paralysis */
	if (p_ptr->paralyzed)
	{
		(void)set_paralyzed(p_ptr->paralyzed - dec_count);
	}

	/* Confusion */
	if (p_ptr->confused)
	{
		(void)set_confused(p_ptr->confused - dec_count);
	}

	/* Afraid */
	if (p_ptr->afraid)
	{
		(void)set_afraid(p_ptr->afraid - dec_count);
	}

	/* Stoning */
	if (p_ptr->stoning)
	{
		if ((p_ptr->stoning % 5) == 4)
		{
#ifdef JP
			msg_print("あなたはさらにのろくなった。");
#else
			msg_print("You feel slow down yourself.");
#endif
		}
		(void)set_stoning(p_ptr->stoning + 1);
	}

	/* Forced to opposite element */
	if (p_ptr->opposite_pelem)
	{
		(void)set_opposite_pelem(p_ptr->opposite_pelem - 1);
	}

	/* Forced to no element */
	if (p_ptr->no_elem)
	{
		(void)set_no_elem(p_ptr->no_elem - 1);
	}

	/* Fast */
	if (p_ptr->fast)
	{
		(void)set_fast(p_ptr->fast - 1, TRUE);
	}

	/* Slow */
	if (p_ptr->slow)
	{
		(void)set_slow(p_ptr->slow - dec_count, TRUE);
	}

	/* Protection from evil */
	if (p_ptr->protevil)
	{
		(void)set_protevil(p_ptr->protevil - 1, TRUE);
	}

	/* Invulnerability */
	if (p_ptr->invuln)
	{
		(void)set_invuln(p_ptr->invuln - 1, TRUE);
	}

	/* Wraith form */
	if (p_ptr->wraith_form)
	{
		(void)set_wraith_form(p_ptr->wraith_form - 1, TRUE);
	}

	/* Heroism */
	if (p_ptr->hero)
	{
		(void)set_hero(p_ptr->hero - 1, TRUE);
	}

	/* Super Heroism */
	if (p_ptr->shero)
	{
		(void)set_shero(p_ptr->shero - 1, TRUE);
	}

	/* Blessed */
	if (p_ptr->blessed)
	{
		(void)set_blessed(p_ptr->blessed - 1, TRUE);
	}

	/* Shield */
	if (p_ptr->shield)
	{
		(void)set_shield(p_ptr->shield - 1, TRUE);
	}

	/* Magicdef */
	if (p_ptr->magicdef)
	{
		(void)set_magicdef(p_ptr->magicdef - 1, TRUE);
	}

	/* Oppose Acid */
	if (p_ptr->oppose_acid)
	{
		(void)set_oppose_acid(p_ptr->oppose_acid - 1, TRUE);
	}

	/* Oppose Lightning */
	if (p_ptr->oppose_elec)
	{
		(void)set_oppose_elec(p_ptr->oppose_elec - 1, TRUE);
	}

	/* Oppose Fire */
	if (p_ptr->oppose_fire)
	{
		(void)set_oppose_fire(p_ptr->oppose_fire - 1, TRUE);
	}

	/* Oppose Cold */
	if (p_ptr->oppose_cold)
	{
		(void)set_oppose_cold(p_ptr->oppose_cold - 1, TRUE);
	}

	/* Oppose Poison */
	if (p_ptr->oppose_pois)
	{
		(void)set_oppose_pois(p_ptr->oppose_pois - 1, TRUE);
	}

	/* Inhibit flood */
	if (p_ptr->inhibit_flood)
	{
		p_ptr->inhibit_flood--;

		if (!p_ptr->inhibit_flood)
		{
#ifdef JP
			msg_print("大洪水が使えるようになった。");
#else
			msg_print("You can use great flood now.");
#endif

			p_ptr->redraw |= (PR_STATUS);
		}
	}

	/* Death regenerate */
	if (p_ptr->death_regen)
	{
		int old_quest = p_ptr->inside_quest;

		p_ptr->death_regen--;

		if (!p_ptr->death_regen)
		{
			r_info[MON_DEATH].max_num = 1;
			if (!astral_mode)
			{
				init_flags = INIT_ASSIGN;
				p_ptr->inside_quest = QUEST_DEATH;
				process_dungeon_file("q_info.txt", 0, 0, 0, 0);

				quest[QUEST_DEATH].status = QUEST_STATUS_TAKEN;
			}
			p_ptr->inside_quest = old_quest;
		}
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
		if (cut_level(p_ptr->cut) >= 7) adjust = 0;

		/* Apply some healing */
		(void)set_cut(p_ptr->cut - adjust);
	}
}


/*
 * Handle burning fuel every 10 game turns
 */
static void process_world_aux_light(void)
{
	/*** Process Light ***/

	/* Check for light being wielded */
	object_type *o_ptr = &inventory[INVEN_LITE];

	/* Burn some fuel in the current lite */
	if (o_ptr->tval == TV_LITE)
	{
		/* Hack -- Use some fuel (except on artifacts) */
		if (!(object_is_fixed_artifact(o_ptr) || (o_ptr->sval == SV_LITE_FEANOR) || (o_ptr->sval == SV_LITE_MAGICAL_LAMP) || (o_ptr->sval == SV_LITE_EMPTY)) && (o_ptr->xtra4 > 0))
		{
			/* Decrease life-span */
			if (o_ptr->name2 == EGO_LITE_LONG)
			{
				if (turn % (TURNS_PER_TICK*2)) o_ptr->xtra4--;
			}
			else o_ptr->xtra4--;

			/* Notice interesting fuel steps */
			notice_lite_change(o_ptr);
		}
	}
}


/*
 * Handle mutation effects once every 10 game turns
 */
static void process_world_aux_mutation(void)
{
	/*** Process mutation effects ***/
	if (p_ptr->muta2 && !p_ptr->wild_mode)
	{
		if ((p_ptr->muta2 & MUT2_BERS_RAGE) && one_in_(3000))
		{
			disturb(0, 0);
#ifdef JP
			msg_print("ウガァァア！");
			msg_print("激怒の発作に襲われた！");
#else
			msg_print("RAAAAGHH!");
			msg_print("You feel a fit of rage coming over you!");
#endif

			(void)set_shero(10 + randint1(p_ptr->lev), FALSE);
		}

		if ((p_ptr->muta2 & MUT2_COWARDICE) && (randint1(3000) == 13))
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

		if ((p_ptr->muta2 & MUT2_RTELEPORT) && (randint1(5000) == 88))
		{
			if (!(p_ptr->muta1 & MUT1_VTELEPORT) && !(p_ptr->anti_tele || p_ptr->earth_spike))
			{
				disturb(0, 0);

				/* Teleport player */
#ifdef JP
				msg_print("あなたの位置は突然ひじょうに不確定になった...");
#else
				msg_print("Your position suddenly seems very uncertain...");
#endif

				msg_print(NULL);
				teleport_player(40);
			}
		}

		if ((p_ptr->muta2 & MUT2_ALCOHOL) && (randint1(6400) == 321))
		{
			if (!p_ptr->resist_conf && !p_ptr->resist_chaos)
			{
				disturb(0, 0);
				p_ptr->redraw |= PR_EXTRA;
#ifdef JP
				msg_print("いひきがもーろーとひてきたきがふる...ヒック！");
#else
				msg_print("You feel a SSSCHtupor cOmINg over yOu... *HIC*!");
#endif

			}

			if (!p_ptr->resist_conf)
			{
				(void)set_confused(p_ptr->confused + randint0(20) + 15);
			}

			if (!p_ptr->resist_chaos)
			{
				if (one_in_(20))
				{
					msg_print(NULL);
					if (one_in_(3)) lose_all_info();
					else wiz_dark(FALSE);
					teleport_player(100);
					wiz_dark(FALSE);
#ifdef JP
					msg_print("あなたは見知らぬ場所で目が醒めた...頭が痛い。");
					msg_print("何も覚えていない。どうやってここに来たかも分からない！");
#else
					msg_print("You wake up somewhere with a sore head...");
					msg_print("You can't remember a thing, or how you got here!");
#endif

				}
				else
				{
					if (one_in_(3))
					{
#ifdef JP
						msg_print("き〜れいなちょおちょらとんれいる〜");
#else
						msg_print("Thishcischs GooDSChtuff!");
#endif

						(void)set_image(p_ptr->image + randint0(150) + 150);
					}
				}
			}
		}

		if ((p_ptr->muta2 & MUT2_HALLU) && (randint1(6400) == 42))
		{
			if (!p_ptr->resist_chaos)
			{
				disturb(0, 0);
				p_ptr->redraw |= PR_EXTRA;
				(void)set_image(p_ptr->image + randint0(50) + 20);
			}
		}

		if ((p_ptr->muta2 & MUT2_ELEM_MULTI) && (randint1(666) == 1))
		{
			p_ptr->pelem = randint0(ELEM_NUM);
			init_realm_table();
			p_ptr->update |= (PU_BONUS);
			p_ptr->notice |= (PN_REORDER);
			load_all_pref_files();
			if (p_ptr->action == ACTION_ELEMSCOPE) lite_spot(py, px);
		}

		if ((p_ptr->muta2 & MUT2_PROD_MANA) &&
		    !p_ptr->anti_magic && one_in_(9000))
		{
			int dire = 0;
			disturb(0, 0);
#ifdef JP
			msg_print("魔法のエネルギーが突然あなたの中に流れ込んできた！エネルギーを解放しなければならない！");
#else
			msg_print("Magical energy flows through you! You must release it!");
#endif

			flush();
			msg_print(NULL);
			(void)get_hack_dir(&dire);
			fire_ball(GF_MANA, dire, p_ptr->lev * 2, 3, FALSE);
		}

		if ((p_ptr->muta2 & MUT2_ATT_DEMON) &&
		    !p_ptr->anti_magic && (randint1(6666) == 666))
		{
			bool pet = one_in_(6);
			u32b mode = PM_ALLOW_GROUP;

			if (pet) mode |= PM_FORCE_PET;
			else mode |= (PM_ALLOW_UNIQUE | PM_NO_PET | PM_IGNORE_AMGRID);

			if (summon_specific((pet ? -1 : 0), py, px,
				    dun_level, SUMMON_DEMON, mode))
			{
#ifdef JP
				msg_print("あなたはデーモンを引き寄せた！");
#else
				msg_print("You have attracted a demon!");
#endif

				disturb(0, 0);
			}
		}

		if ((p_ptr->muta2 & MUT2_SPEED_FLUX) && one_in_(6000))
		{
			disturb(0, 0);
			if (one_in_(2))
			{
#ifdef JP
				msg_print("精力的でなくなった気がする。");
#else
				msg_print("You feel less energetic.");
#endif

				if (p_ptr->fast > 0)
				{
					set_fast(0, TRUE);
				}
				else
				{
					set_slow(randint1(30) + 10, FALSE);
				}
			}
			else
			{
#ifdef JP
				msg_print("精力的になった気がする。");
#else
				msg_print("You feel more energetic.");
#endif

				if (p_ptr->slow > 0)
				{
					set_slow(0, TRUE);
				}
				else
				{
					set_fast(randint1(30) + 10, FALSE);
				}
			}
			msg_print(NULL);
		}
		if ((p_ptr->muta2 & MUT2_BANISH_ALL) && one_in_(9000))
		{
			disturb(0, 0);
#ifdef JP
			msg_print("突然ほとんど孤独になった気がする。");
#else
			msg_print("You suddenly feel almost lonely.");
#endif

			banish_monsters(100);
			if (!dun_level && p_ptr->town_num)
			{
#ifdef JP
				msg_print("店の主人が丘に向かって走っている！");
#else
				msg_print("You see one of the shopkeepers running for the hills!");
#endif

				store_shuffle(randint0(MAX_STORES));
			}
			msg_print(NULL);
		}

		if ((p_ptr->muta2 & MUT2_EAT_LIGHT) && one_in_(3000))
		{
			object_type *o_ptr;

#ifdef JP
			msg_print("影につつまれた。");
#else
			msg_print("A shadow passes over you.");
#endif

			msg_print(NULL);

			/* Absorb light from the current possition */
			if (cave[py][px].info & CAVE_GLOW)
			{
				hp_player(10);
			}

			o_ptr = &inventory[INVEN_LITE];

			/* Absorb some fuel in the current lite */
			if (o_ptr->tval == TV_LITE)
			{
				/* Use some fuel (except on artifacts) */
				if (!object_is_fixed_artifact(o_ptr) && (o_ptr->xtra4 > 0))
				{
					/* Heal the player a bit */
					hp_player(o_ptr->xtra4 / 20);

					/* Decrease life-span of lite */
					o_ptr->xtra4 /= 2;

#ifdef JP
					msg_print("光源からエネルギーを吸収した！");
#else
					msg_print("You absorb energy from your light!");
#endif


					/* Notice interesting fuel steps */
					notice_lite_change(o_ptr);
				}
			}

			/*
			 * Unlite the area (radius 10) around player and
			 * do 50 points damage to every affected monster
			 */
			unlite_area(50, 10);
		}

		if ((p_ptr->muta2 & MUT2_ATT_ANIMAL) &&
		   !p_ptr->anti_magic && one_in_(7000))
		{
			bool pet = one_in_(3);
			u32b mode = PM_ALLOW_GROUP;

			if (pet) mode |= PM_FORCE_PET;
			else mode |= (PM_ALLOW_UNIQUE | PM_NO_PET | PM_IGNORE_AMGRID);

			if (summon_specific((pet ? -1 : 0), py, px, dun_level, SUMMON_ANIMAL, mode))
			{
#ifdef JP
				msg_print("動物を引き寄せた！");
#else
				msg_print("You have attracted an animal!");
#endif

				disturb(0, 0);
			}
		}

		if ((p_ptr->muta2 & MUT2_RAW_CHAOS) &&
		    !p_ptr->anti_magic && one_in_(8000))
		{
			disturb(0, 0);
#ifdef JP
			msg_print("周りの空間が歪んでいる気がする！");
#else
			msg_print("You feel the world warping around you!");
#endif

			msg_print(NULL);
			fire_ball(GF_CHAOS, 0, p_ptr->lev, 8, FALSE);
		}
		if ((p_ptr->muta2 & MUT2_NORMALITY) && one_in_(5000))
		{
			if (!lose_mutation(0))
#ifdef JP
				msg_print("奇妙なくらい普通になった気がする。");
#else
				msg_print("You feel oddly normal.");
#endif

		}
		if ((p_ptr->muta2 & MUT2_WRAITH) && !p_ptr->anti_magic && one_in_(3000))
		{
			disturb(0, 0);
#ifdef JP
			msg_print("非物質化した！");
#else
			msg_print("You feel insubstantial!");
#endif

			msg_print(NULL);
			set_wraith_form(randint1(p_ptr->lev / 2) + (p_ptr->lev / 2), FALSE);
		}
		if ((p_ptr->muta2 & MUT2_POLY_WOUND) && one_in_(3000))
		{
			do_poly_wounds();
		}
		if ((p_ptr->muta2 & MUT2_WASTING) && one_in_(3000))
		{
			int which_stat = randint0(A_MAX);
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
				(void)dec_stat(which_stat, randint1(6) + 6, one_in_(3));
			}
		}
		if ((p_ptr->muta2 & MUT2_ATT_DRAGON) &&
		   !p_ptr->anti_magic && one_in_(3000))
		{
			bool pet = one_in_(5);
			u32b mode = PM_ALLOW_GROUP;

			if (pet) mode |= PM_FORCE_PET;
			else mode |= (PM_ALLOW_UNIQUE | PM_NO_PET | PM_IGNORE_AMGRID);

			if (summon_specific((pet ? -1 : 0), py, px, dun_level, SUMMON_DRAGON, mode))
			{
#ifdef JP
				msg_print("ドラゴンを引き寄せた！");
#else
				msg_print("You have attracted a dragon!");
#endif

				disturb(0, 0);
			}
		}
		if ((p_ptr->muta2 & MUT2_WEIRD_MIND) && !p_ptr->anti_magic &&
			one_in_(3000))
		{
			if (p_ptr->tim_esp > 0)
			{
#ifdef JP
				msg_print("精神にもやがかかった！");
#else
				msg_print("Your mind feels cloudy!");
#endif

				set_tim_esp(0, TRUE);
			}
			else
			{
#ifdef JP
				msg_print("精神が広がった！");
#else
				msg_print("Your mind expands!");
#endif

				set_tim_esp(p_ptr->lev, FALSE);
			}
		}
		if ((p_ptr->muta2 & MUT2_NAUSEA) && !p_ptr->slow_digest && !p_ptr->no_digest &&
			one_in_(9000))
		{
			disturb(0, 0);
#ifdef JP
			msg_print("胃が痙攣し、食事を失った！");
#else
			msg_print("Your stomach roils, and you lose your lunch!");
#endif

			msg_print(NULL);
			set_food(PY_FOOD_WEAK);
		}

		if ((p_ptr->muta2 & MUT2_ALTER_REALITY) &&
		   !p_ptr->anti_magic && one_in_(12000) && !p_ptr->inside_arena)
		{
			alter_reality();
		}

		if ((p_ptr->muta2 & MUT2_WARNING) && one_in_(1000))
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
		if ((p_ptr->muta2 & MUT2_INVULN) && !p_ptr->anti_magic &&
			one_in_(5000))
		{
			disturb(0, 0);
#ifdef JP
			msg_print("無敵な気がする！");
#else
			msg_print("You feel invincible!");
#endif

			msg_print(NULL);
			(void)set_invuln(randint1(8) + 8, FALSE);
		}
		if ((p_ptr->muta2 & MUT2_SP_TO_HP) && one_in_(2000))
		{
			int wounds = p_ptr->mhp - p_ptr->chp;

			if (wounds > 0)
			{
				int healing = p_ptr->csp;

				if (healing > wounds)
				{
					healing = wounds;
				}

				hp_player(healing);
				p_ptr->csp -= healing;
			}
		}
		if ((p_ptr->muta2 & MUT2_HP_TO_SP) && !p_ptr->anti_magic &&
			one_in_(4000))
		{
			int wounds = p_ptr->msp - p_ptr->csp;

			if (wounds > 0)
			{
				int healing = p_ptr->chp;

				if (healing > wounds)
				{
					healing = wounds;
				}

				p_ptr->csp += healing;
#ifdef JP
				take_hit(DAMAGE_LOSELIFE, healing, "頭に昇った血");
#else
				take_hit(DAMAGE_LOSELIFE, healing, "blood rushing to the head");
#endif

			}
		}
		if ((p_ptr->muta2 & MUT2_DISARM) && one_in_(10000))
		{
			object_type *o_ptr;

			disturb(0, 0);
#ifdef JP
			msg_print("足がもつれて転んだ！");
			take_hit(DAMAGE_NOESCAPE, randint1(p_ptr->wt / 6), "転倒");
#else
			msg_print("You trip over your own feet!");
			take_hit(DAMAGE_NOESCAPE, randint1(p_ptr->wt / 6), "tripping");
#endif


			msg_print(NULL);
			if (buki_motteruka(INVEN_RARM))
			{
				int slot = INVEN_RARM;
				o_ptr = &inventory[INVEN_RARM];
				if (buki_motteruka(INVEN_LARM) && one_in_(2))
				{
					o_ptr = &inventory[INVEN_LARM];
					slot = INVEN_LARM;
				}
				if (!object_is_cursed(o_ptr))
				{
#ifdef JP
					msg_print("武器を落してしまった！");
#else
					msg_print("You drop your weapon!");
#endif

					inven_drop(slot, 1);
				}
			}
		}
	}
}


/*
 * Handle curse effects once every 10 game turns
 */
static void process_world_aux_curse(void)
{
	/*** Process Inventory ***/

	if ((p_ptr->cursed & TRC_P_FLAG_MASK) && !p_ptr->wild_mode)
	{
		/*
		 * Hack: Uncursed teleporting items (e.g. Trump Weapons)
		 * can actually be useful!
		 */
		if ((p_ptr->cursed & TRC_TELEPORT_SELF) && one_in_(200))
		{
#ifdef JP
			if (get_check_strict("テレポートしますか？", CHECK_OKAY_CANCEL))
#else
			if (get_check_strict("Teleport? ", CHECK_OKAY_CANCEL))
#endif
			{
				disturb(0, 0);
				teleport_player(50);
			}
		}
		/* TY Curse */
		if ((p_ptr->cursed & TRC_TY_CURSE) && one_in_(TY_CURSE_CHANCE + TY_CURSE_CHANCE * skill_exp_level(p_ptr->cexp_info[CLASS_LICH].clev)))
		{
			int count = 0;
			(void)activate_ty_curse(FALSE, &count);
		}
		/* Handle experience draining */
		if ((p_ptr->cursed & TRC_DRAIN_EXP) && one_in_(4))
		{
			cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];

			/* Racial */
			p_ptr->exp -= (p_ptr->lev + 1) / 2;
			if (p_ptr->exp < 0) p_ptr->exp = 0;
			p_ptr->max_exp -= (p_ptr->lev + 1) / 2;
			if (p_ptr->max_exp < 0) p_ptr->max_exp = 0;

			/* Class */
			cexp_ptr->cexp -= (cexp_ptr->clev + 1) / 2;
			if (cexp_ptr->cexp < 0) cexp_ptr->cexp = 0;
			cexp_ptr->max_cexp -= (cexp_ptr->clev + 1) / 2;
			if (cexp_ptr->max_cexp < 0) cexp_ptr->max_cexp = 0;

			check_experience();
		}
		/* Add light curse (Later) */
		if ((p_ptr->cursed & TRC_ADD_L_CURSE) && one_in_(2000))
		{
			u32b new_curse;
			object_type *o_ptr;

			o_ptr = choose_cursed_obj_name(TRC_ADD_L_CURSE);

			new_curse = get_curse(0, o_ptr);
			if (!(o_ptr->curse_flags & new_curse))
			{
				char o_name[MAX_NLEN];

				object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

				o_ptr->curse_flags |= new_curse;
#ifdef JP
				msg_format("悪意に満ちた黒いオーラが%sをとりまいた...", o_name);
#else
				msg_format("There is a malignant black aura surrounding %s...", o_name);
#endif

				o_ptr->feeling = FEEL_NONE;

				p_ptr->update |= (PU_BONUS);
			}
		}
		/* Add heavy curse (Later) */
		if ((p_ptr->cursed & TRC_ADD_H_CURSE) && one_in_(2000))
		{
			u32b new_curse;
			object_type *o_ptr;

			o_ptr = choose_cursed_obj_name(TRC_ADD_H_CURSE);

			new_curse = get_curse(1, o_ptr);
			if (!(o_ptr->curse_flags & new_curse))
			{
				char o_name[MAX_NLEN];

				object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

				o_ptr->curse_flags |= new_curse;
#ifdef JP
				msg_format("悪意に満ちた黒いオーラが%sをとりまいた...", o_name);
#else
				msg_format("There is a malignant black aura surrounding %s...", o_name);
#endif

				o_ptr->feeling = FEEL_NONE;

				p_ptr->update |= (PU_BONUS);
			}
		}
		/* Call animal */
		if ((p_ptr->cursed & TRC_CALL_ANIMAL) && one_in_(2500))
		{
			if (summon_specific(0, py, px, dun_level, SUMMON_ANIMAL,
			    (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET)))
			{
				char o_name[MAX_NLEN];

				object_desc(o_name, choose_cursed_obj_name(TRC_CALL_ANIMAL), (OD_OMIT_PREFIX | OD_NAME_ONLY));
#ifdef JP
				msg_format("%sが動物を引き寄せた！", o_name);
#else
				msg_format("%s have attracted an animal!", o_name);
#endif

				disturb(0, 0);
			}
		}
		/* Call demon */
		if ((p_ptr->cursed & TRC_CALL_DEMON) && one_in_(1111))
		{
			if (summon_specific(0, py, px, dun_level, SUMMON_DEMON, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET)))
			{
				char o_name[MAX_NLEN];

				object_desc(o_name, choose_cursed_obj_name(TRC_CALL_DEMON), (OD_OMIT_PREFIX | OD_NAME_ONLY));
#ifdef JP
				msg_format("%sが悪魔を引き寄せた！", o_name);
#else
				msg_format("%s have attracted a demon!", o_name);
#endif

				disturb(0, 0);
			}
		}
		/* Call dragon */
		if ((p_ptr->cursed & TRC_CALL_DRAGON) && one_in_(800))
		{
			if (summon_specific(0, py, px, dun_level, SUMMON_DRAGON,
			    (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET)))
			{
				char o_name[MAX_NLEN];

				object_desc(o_name, choose_cursed_obj_name(TRC_CALL_DRAGON), (OD_OMIT_PREFIX | OD_NAME_ONLY));
#ifdef JP
				msg_format("%sがドラゴンを引き寄せた！", o_name);
#else
				msg_format("%s have attracted an animal!", o_name);
#endif

				disturb(0, 0);
			}
		}
		if ((p_ptr->cursed & TRC_COWARDICE) && one_in_(1500))
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
		/* Teleport player */
		if ((p_ptr->cursed & TRC_TELEPORT) && one_in_(200) && !(p_ptr->anti_tele || p_ptr->earth_spike))
		{
			disturb(0, 0);

			/* Teleport player */
			teleport_player(40);
		}
		/* Handle HP draining */
		if ((p_ptr->cursed & TRC_DRAIN_HP) && one_in_(666))
		{
			char o_name[MAX_NLEN];

			object_desc(o_name, choose_cursed_obj_name(TRC_DRAIN_HP), (OD_OMIT_PREFIX | OD_NAME_ONLY));
#ifdef JP
			msg_format("%sはあなたの体力を吸収した！", o_name);
#else
			msg_format("%s drains HP from you!", o_name);
#endif
			take_hit(DAMAGE_LOSELIFE, MIN(p_ptr->lev*2, 100), o_name);
		}
		/* Handle mana draining */
		if ((p_ptr->cursed & TRC_DRAIN_MANA) && p_ptr->csp && one_in_(666))
		{
			char o_name[MAX_NLEN];

			object_desc(o_name, choose_cursed_obj_name(TRC_DRAIN_MANA), (OD_OMIT_PREFIX | OD_NAME_ONLY));
#ifdef JP
			msg_format("%sはあなたの魔力を吸収した！", o_name);
#else
			msg_format("%s drains mana from you!", o_name);
#endif
			p_ptr->csp -= MIN(p_ptr->lev, 50);
			if (p_ptr->csp < 0)
			{
				p_ptr->csp = 0;
				p_ptr->csp_frac = 0;
			}
			p_ptr->redraw |= PR_MANA;
		}
	}

	/* Rarely, take damage from the Lost Eye */
	if (one_in_(999) && !p_ptr->anti_magic)
	{
		object_type *o_ptr = &inventory[INVEN_LITE];

		if (o_ptr->name1 == ART_HABORYM_EYE)
		{
#ifdef JP
			if (object_is_known(o_ptr))
				msg_print("その剥製はあなたの体力を吸収した！");
			else
				msg_print("なにかがあなたの体力を吸収した！");
			take_hit(DAMAGE_LOSELIFE, MIN(p_ptr->lev, 50), "剣聖ハボリムの失われた眼");
#else
			if (object_is_known(o_ptr))
				msg_print("The Lost Eye of of Haborym drains life from you!");
			else
				msg_print("Something drains life from you!");
			take_hit(DAMAGE_LOSELIFE, MIN(p_ptr->lev, 50), "The Lost Eye of of Haborym");
#endif
		}
		else if (o_ptr->name1 == ART_FAKE)
		{
#ifdef JP
			if (object_is_known(o_ptr))
				msg_print("その剥製はあなたの体力を吸収した！");
			else
				msg_print("なにかがあなたの体力を吸収した！");
			take_hit(DAMAGE_LOSELIFE, MIN(p_ptr->lev, 50), "暗黒騎士ランスロットの失われた眼");
#else
			if (object_is_known(o_ptr))
				msg_print("The Lost Eye of Lancelot the Dark Knight drains life from you!");
			else
				msg_print("Something drains life from you!");
			take_hit(DAMAGE_LOSELIFE, MIN(p_ptr->lev, 50), "The Lost Eye of Lancelot the Dark Knight");
#endif
		}
	}


	/* Take damage from Youtou */
	if ((((inventory[INVEN_RARM].tval == TV_SWORD) && (inventory[INVEN_RARM].sval == SV_YOUTOU)) ||
		((inventory[INVEN_LARM].tval == TV_SWORD) && (inventory[INVEN_LARM].sval == SV_YOUTOU))) &&
			(p_ptr->cexp_info[CLASS_SWORDMASTER].clev < 50))
	{
		int lev = skill_exp_level(p_ptr->cexp_info[CLASS_SWORDMASTER].clev);
		take_hit(DAMAGE_LOSELIFE, (4 - lev) * 10 + randint1(10), "妖刀");

	}

	if (one_in_(600)) p_ptr->smithy_town_num = randint1(7);
}


/*
 * Handle recharging objects once every 10 game turns
 */
static void process_world_aux_recharge(void)
{
	int i;
	bool changed;

	/* Process equipment */
	for (changed = FALSE, i = INVEN_RARM; i < INVEN_TOTAL; i++)
	{
		/* Get the object */
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Recharge activatable objects */
		if (o_ptr->timeout > 0)
		{
			/* Recharge */
			o_ptr->timeout--;

			/* Notice changes */
			if (!o_ptr->timeout)
			{
				recharged_notice(o_ptr);
				changed = TRUE;
			}
		}
	}

	/* Notice changes */
	if (changed)
	{
		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
		wild_regen = 20;
	}

	/*
	 * Recharge rods.  Rods now use timeout to control charging status,
	 * and each charging rod in a stack decreases the stack's timeout by
	 * one per turn. -LM-
	 */
	for (changed = FALSE, i = 0; i < INVEN_PACK; i++)
	{
		object_type *o_ptr = &inventory[i];
		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Examine all charging rods or stacks of charging rods. */
		if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
		{
			/* Determine how many rods are charging. */
			int temp = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;
			if (temp > o_ptr->number) temp = o_ptr->number;

			/* Decrease timeout by that number. */
			o_ptr->timeout -= temp;

			/* Boundary control. */
			if (o_ptr->timeout < 0) o_ptr->timeout = 0;

			/* Notice changes, provide message if object is inscribed. */
			if (!(o_ptr->timeout))
			{
				recharged_notice(o_ptr);
				changed = TRUE;
			}

			/* One of the stack of rod is charged */
			else if (o_ptr->timeout % k_ptr->pval)
			{
				changed = TRUE;
			}
		}
	}

	/* Notice changes */
	if (changed)
	{
		/* Combine pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN);
		wild_regen = 20;
	}

	/* Process objects on floor */
	for (i = 1; i < o_max; i++)
	{
		/* Access object */
		object_type *o_ptr = &o_list[i];

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
}


/*
 * Handle involuntary movement once every 10 game turns
 */
static void process_world_aux_movement(void)
{
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
				if (d_info[dungeon_type].flags1 & DF1_UPWARD)
#ifdef JP
					msg_print("下に引きずり降ろされる感じがする！");
#else
					msg_print("You feel yourself yanked downwards!");
#endif
				else
#ifdef JP
					msg_print("上に引っ張りあげられる感じがする！");
#else
					msg_print("You feel yourself yanked upwards!");
#endif

				if (dungeon_type) p_ptr->recall_dungeon = dungeon_type;
				if (record_stair)
					do_cmd_write_nikki(NIKKI_RECALL, dun_level, NULL);

				dun_level = 0;
				dungeon_type = 0;

				leave_quest_check();

				p_ptr->inside_quest = 0;

				p_ptr->leaving = TRUE;
			}
			else
			{
				dungeon_type = p_ptr->recall_dungeon;

				if (d_info[dungeon_type].flags1 & DF1_UPWARD)
#ifdef JP
					msg_print("上に引っ張りあげられる感じがする！");
#else
					msg_print("You feel yourself yanked upwards!");
#endif
				else
#ifdef JP
					msg_print("下に引きずり降ろされる感じがする！");
#else
					msg_print("You feel yourself yanked downwards!");
#endif

				if (record_stair)
				{
					int old_max_dlv = max_dlv[dungeon_type];

					if (dungeon_type == DUNGEON_HEAVEN_WAY)
					{
						if (old_max_dlv >= d_info[dungeon_type].maxdepth)
							max_dlv[dungeon_type] = d_info[dungeon_type].maxdepth - 1;
					}
					do_cmd_write_nikki(NIKKI_RECALL, dun_level, NULL);
					if (dungeon_type == DUNGEON_HEAVEN_WAY) max_dlv[dungeon_type] = old_max_dlv;
				}

				/* New depth */
				dun_level = max_dlv[dungeon_type];
				if (dun_level < 1) dun_level = 1;
				if (dungeon_type == DUNGEON_HEAVEN_WAY)
				{
					if (dun_level >= d_info[dungeon_type].maxdepth) dun_level = d_info[dungeon_type].maxdepth - 1;
				}

				if (p_ptr->wild_mode)
				{
					p_ptr->wilderness_y = py;
					p_ptr->wilderness_x = px;
				}
				else
				{
					/* Save player position */
					p_ptr->oldpx = px;
					p_ptr->oldpy = py;
				}
				p_ptr->wild_mode = FALSE;

				/*
				 * Clear all saved floors
				 * and create a first saved floor
				 */
				prepare_change_floor_mode(CFM_FIRST_FLOOR);

				/* Leaving */
				p_ptr->leaving = TRUE;

				if (dungeon_type == DUNGEON_PALACE)
				{
					int i;

					for (i = MIN_RANDOM_QUEST; i <= MAX_RANDOM_QUEST_ASTRAL; i++)
					{
						if ((quest[i].type == QUEST_TYPE_RANDOM) &&
						    (quest[i].status == QUEST_STATUS_TAKEN) &&
						    (quest[i].level < dun_level))
						{
							quest[i].status = QUEST_STATUS_FAILED;
							quest[i].complev = (byte)p_ptr->lev;
							r_info[quest[i].r_idx].flags1 &= ~(RF1_QUESTOR);
						}
					}
				}
			}

			/* Sound */
			sound(SOUND_TPLEVEL);
		}
	}

	/* Delayed Alter reality */
	if (p_ptr->alter_reality)
	{
		if (autosave_l && (p_ptr->alter_reality == 1))
			do_cmd_save_game(TRUE);

		/* Count down towards alter */
		p_ptr->alter_reality--;

		p_ptr->redraw |= (PR_STATUS);

		/* Activate the alter reality */
		if (!p_ptr->alter_reality)
		{
			/* Disturbing! */
			disturb(0, 0);

			/* Determine the level */
			if (!quest_number(dun_level) && dun_level)
			{
#ifdef JP
				msg_print("世界が変わった！");
#else
				msg_print("The world changes!");
#endif

				/*
				 * Clear all saved floors
				 * and create a first saved floor
				 */
				prepare_change_floor_mode(CFM_FIRST_FLOOR);

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
			else
			{
#ifdef JP
				msg_print("世界が少しの間変化したようだ。");
#else
				msg_print("The world seems to change for a moment!");
#endif
			}

			/* Sound */
			sound(SOUND_TPLEVEL);
		}
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
static byte get_dungeon_feeling(void)
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
		if (object_is_known(o_ptr))
		{
			/* Touched? */
			if (o_ptr->marked & OM_TOUCHED) continue;
		}

		/* Skip pseudo-known objects */
		if (o_ptr->ident & IDENT_SENSE) continue;

		/* Ego objects */
		if (object_is_ego(o_ptr))
		{
			ego_item_type *e_ptr = &e_info[o_ptr->name2];

			delta += e_ptr->rating * base;
		}

		/* Artifacts */
		if (object_is_artifact(o_ptr))
		{
			s32b cost = object_value_real(o_ptr);

			delta += 10 * base;
			if (cost > 10000L) delta += 10 * base;
			if (cost > 50000L) delta += 10 * base;
			if (cost > 100000L) delta += 10 * base;

			/* Special feeling */
			if (!preserve_mode) return 1;
		}

		if (o_ptr->tval == TV_SOFT_ARMOR &&
		    o_ptr->sval == SV_DRAGON_LEATHER_ARMOR) delta += 5 * base;
		if (o_ptr->tval == TV_HARD_ARMOR &&
		    o_ptr->sval == SV_DRAGON_SCALE_MAIL) delta += 5 * base;
		if (o_ptr->tval == TV_SHIELD &&
		    o_ptr->sval == SV_DRAGON_SHIELD) delta += 5 * base;
		if (o_ptr->tval == TV_GLOVES &&
		    o_ptr->sval == SV_SET_OF_DRAGON_GLOVES) delta += 5 * base;
		if (o_ptr->tval == TV_BOOTS &&
		    o_ptr->sval == SV_PAIR_OF_DRAGON_GREAVE) delta += 5 * base;
		if (o_ptr->tval == TV_HELM &&
		    o_ptr->sval == SV_DRAGON_HELM) delta += 5 * base;
		if (o_ptr->tval == TV_RING &&
		    o_ptr->sval == SV_RING_SPEED &&
		    !object_is_cursed(o_ptr)) delta += 25 * base;
		if (o_ptr->tval == TV_RING &&
		    o_ptr->sval == SV_RING_LORDLY &&
		    !object_is_cursed(o_ptr)) delta += 15 * base;
		if (o_ptr->tval == TV_AMULET &&
		    o_ptr->sval == SV_AMULET_THE_MAGI &&
		    !object_is_cursed(o_ptr)) delta += 15 * base;

		/* Out-of-depth objects */
		if (!object_is_cursed(o_ptr) && !object_is_broken(o_ptr) &&
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
	delay = MAX(10, 150 - p_ptr->skill_fos) * (150 - dun_level) * TURNS_PER_TICK / 100;

 	/* Not yet felt anything */
	if (turn < p_ptr->feeling_turn + delay && !cheat_xtra) return;

	/* Extract quest number (if any) */
	quest_num = quest_number(dun_level);

	/* No feeling in a quest */
	if (quest_num &&
	    (!quest_is_fixed(quest_num) &&
	     !((quest[quest_num].flags & QUEST_FLAG_GUARDIAN) ||
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
	int day, hour, min, prev_min;
	int x, y, i;

	cave_type *c_ptr;

	s32b len = TURNS_PER_TICK * TOWN_DAWN;
	s32b tick = turn % len + len / 4;

	extract_day_hour_min(&day, &hour, &min);
	prev_min = (1440 * (tick - TURNS_PER_TICK) / len) % 60;

	/* Update dungeon feeling, and announce it if changed */
	update_dungeon_feeling();

	/* Every 10 game turns */
	if (turn % TURNS_PER_TICK) return;

	/*** Check the Time and Load ***/

	if (!(turn % (50*TURNS_PER_TICK)))
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
				p_ptr->playing = FALSE;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}
	}

	/*** Attempt timed autosave ***/
	if (autosave_t && autosave_freq)
	{
		if (!(turn % ((s32b)autosave_freq * TURNS_PER_TICK)))
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
	if ((!dun_level && !p_ptr->inside_quest && !p_ptr->inside_arena) ||
		(d_info[dungeon_type].flags1 & DF1_WILD_LITE))
	{
		/* Hack -- Daybreak/Nighfall in town */
		if (!(turn % ((TURNS_PER_TICK * TOWN_DAWN) / 2)))
		{
			bool dawn;
			bool in_dungeon = FALSE;

			/* Check for dawn */
			dawn = (!(turn % (TURNS_PER_TICK * TOWN_DAWN)));

			if (d_info[dungeon_type].flags1 & DF1_WILD_LITE) in_dungeon = TRUE;

			/* Day breaks */
			if (dawn)
			{
				/* Message */
#ifdef JP
				msg_print("夜が明けた。");
#else
				msg_print("The sun has risen.");
#endif

				if (!p_ptr->wild_mode)
				{
					/* Hack -- Scan the town */
					for (y = 0; y < cur_hgt; y++)
					{
						for (x = 0; x < cur_wid; x++)
						{
							/* Get the cave grid */
							c_ptr = &cave[y][x];

							/* Assume lit */
							c_ptr->info |= (CAVE_GLOW);

							if ((!in_dungeon || player_has_los_bold(y, x)) && !p_ptr->blind)
							{
								/* Hack -- Memorize lit grids if allowed */
								if (view_perma_grids) c_ptr->info |= (CAVE_MARK);

								/* Hack -- Notice spot */
								note_spot(y, x);
							}
						}
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


				if (!p_ptr->wild_mode)
				{
					/* Hack -- Scan the town */
					for (y = 0; y < cur_hgt; y++)
					{
						for (x = 0; x < cur_wid; x++)
						{
							/* Get the cave grid */
							c_ptr = &cave[y][x];

							/* Assume dark */
							c_ptr->info &= ~(CAVE_GLOW);

							/* Darken "boring" features */
							if (in_dungeon ||
							    (c_ptr->feat <= FEAT_INVIS) ||
							    ((c_ptr->feat >= FEAT_DEEP_WATER) &&
							    (c_ptr->feat <= FEAT_MOUNTAIN) &&
							     (c_ptr->feat != FEAT_MUSEUM)) ||
							    (x == 0) || (x == cur_wid-1) ||
							    (y == 0) || (y == cur_hgt-1))
							{
								byte feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

								if ((feat <= FEAT_INVIS) ||
								    (feat == FEAT_DIRT) ||
								    (feat == FEAT_GRASS) ||
								    (feat == FEAT_SWAMP) ||
								    (feat == FEAT_TUNDRA))
								{
									/* Forget the grid */
									c_ptr->info &= ~(CAVE_MARK);
								}

								/* Hack -- Notice spot */
								note_spot(y, x);
							}
						}
					}

					/* Glow some feature */
					glow_floor_feature();
				}
			}

			/* Update the monsters */
			p_ptr->update |= (PU_MONSTERS | PU_MON_LITE);

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP | PR_TITLE);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
		}
	}


	/*** Process the monsters ***/

	/* Check for creature generation. */
	if (one_in_(d_info[dungeon_type].max_m_alloc_chance) &&
	    !p_ptr->inside_arena && !p_ptr->inside_quest)
	{
		/* Make a new monster */
		(void)alloc_monster(MAX_SIGHT + 5, 0);
	}

	/* Hack -- Check for creature regeneration */
	if (!(turn % (TURNS_PER_TICK*10))) regen_monsters();
	if (!(turn % (TURNS_PER_TICK*3))) regen_captured_monsters();

	if (!p_ptr->leaving)
	{
		int i;

		/* Hack -- Process the counters of monsters if needed */
		for (i = 0; i < MAX_MTIMED; i++)
		{
			if (mproc_max[i] > 0) process_monsters_mtimed(i);
		}
	}

	/* Date changes */
	if (!hour && !min)
	{
		monster_race *r_ptr;

		if (min != prev_min)
		{
			int max_dl = 3;
			byte old_dungeon_type = dungeon_type;

			do_cmd_write_nikki(NIKKI_HIGAWARI, 0, NULL);

			/* Hack -- Player is temporarily in main dungeon */
			dungeon_type = DUNGEON_PALACE;
			get_mon_num_prep(NULL, NULL);

			for (i = 0; i < max_d_idx; i++)
			{
				if (max_dlv[i] < d_info[i].mindepth) continue;
				if (max_dl < max_dlv[i]) max_dl = max_dlv[i];
			}
			while (1)
			{
				today_mon = get_mon_num(max_dl);
				r_ptr = &r_info[today_mon];

				if (r_ptr->flags1 & RF1_UNIQUE) continue;
				if (r_ptr->flags7 & (RF7_NAZGUL | RF7_UNIQUE2)) continue;
				if (r_ptr->flags2 & (RF2_MULTIPLY)) continue;
				if (!(r_ptr->flags9 & RF9_DROP_CORPSE) || !(r_ptr->flags9 & RF9_DROP_SKELETON)) continue;
				if (r_ptr->level < MIN(max_dl/2, 40)) continue;
				if (r_ptr->rarity > 10) continue;
				if (r_ptr->level == 0) continue;
				break;
			}
			p_ptr->today_mon = 0;

			/* Hack -- Restore the player to current dungeon */
			dungeon_type = old_dungeon_type;
			get_mon_num_prep(NULL, NULL);
		}
	}


	/*** Check the Food, and Regenerate ***/

	if (!p_ptr->no_digest)
	{
		/* Digest normally */
		if (p_ptr->food < PY_FOOD_MAX)
		{
			/* Every 100 game turns */
			if (!(turn % (TURNS_PER_TICK*5)))
			{
				/* Basic digestion rate based on speed */
				i = SPEED_TO_ENERGY(p_ptr->pspeed);

				/* Regeneration takes more food */
				if (p_ptr->regenerate) i += 20;
				if (p_ptr->regenerate_mana) i += 20;
				if (p_ptr->cursed & TRC_FAST_DIGEST) i += 30;

				/* Slow digestion takes less food */
				if (p_ptr->slow_digest) i -= 5;

				/* Minimal digestion */
				if (i < 1) i = 1;
				/* Maximal digestion */
				if (i > 100) i = 100;

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

		/* Starve to death (slowly) */
		if (p_ptr->food < PY_FOOD_STARVE)
		{
			/* Calculate damage */
			i = (PY_FOOD_STARVE - p_ptr->food) / 10;

			/* Take damage */
#ifdef JP
			if (!p_ptr->invuln) take_hit(DAMAGE_LOSELIFE, i, "空腹");
#else
			if (!p_ptr->invuln) take_hit(DAMAGE_LOSELIFE, i, "starvation");
#endif

		}
	}

	/* Process timed damage and regeneration */
	process_world_aux_hp_and_sp();

	/* Process timeout */
	process_world_aux_timeout();

	/* Process light */
	process_world_aux_light();

	/* Process mutation effects */
	process_world_aux_mutation();

	/* Process curse effects */
	process_world_aux_curse();

	/* Process recharging */
	process_world_aux_recharge();

	/* Feel the inventory */
	sense_inventory1();
	sense_inventory2();

	/* Involuntary Movement */
	process_world_aux_movement();

	/*** Weather ***/
	weather_time_to_change--;
	if (!weather_time_to_change)
	{
		int tmp_temp;

		switch (randint1(15))
		{
		case 1: case 2: case 3: case 4:
			tmp_temp = is_daytime() ? 1 : -1;
			break;
		case 5: case 6: case 7: case 8: case 9: case 10:
			tmp_temp = is_daytime() ? -1 : 1;
			break;
		default:
			tmp_temp = 0;
			break;
		}

		set_weather(randint1(3) - 2, randint1(3) - 2, tmp_temp);
		weather_time_to_change = 100 + randint1(100);
	}
}



/*
 * Verify use of "wizard" mode
 */
static bool enter_wizard_mode(void)
{
	/* Ask first time */
	if (!p_ptr->noscore)
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

#ifdef JP
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "ウィザードモードに突入してスコアを残せなくなった。");
#else
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "give up recording score to enter wizard mode.");
#endif
		/* Mark savefile */
		p_ptr->noscore |= 0x0002;
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
	if (!p_ptr->noscore)
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

#ifdef JP
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "デバッグモードに突入してスコアを残せなくなった。");
#else
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "give up recording score to use debug commands.");
#endif
		/* Mark savefile */
		p_ptr->noscore |= 0x0008;
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
	if (!(p_ptr->noscore & 0x0010))
	{
		/* Borg mode is not permitted */
		if (!allow_debug_opts)
		{
#ifdef JP
			msg_print("ボーグコマンドは許可されていません。 ");
#else
			msg_print("Use of borg command is not permitted.");
#endif
			return FALSE;
		}

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

#ifdef JP
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "ボーグ・コマンドを使用してスコアを残せなくなった。");
#else
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "give up recording score to use borg commands.");
#endif
		/* Mark savefile */
		p_ptr->noscore |= 0x0010;
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
	int old_now_message = now_message;

	/* Handle repeating the last command */
	repeat_check();

	now_message = 0;

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
			if (p_ptr->wizard)
			{
				p_ptr->wizard = FALSE;
#ifdef JP
				msg_print("ウィザードモード解除。");
#else
				msg_print("Wizard mode off.");
#endif

				verify_runeweapon();
			}
			else if (enter_wizard_mode())
			{
				p_ptr->wizard = TRUE;
#ifdef JP
				msg_print("ウィザードモード突入。");
#else
				msg_print("Wizard mode on.");
#endif

				verify_runeweapon();
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
				if (!p_ptr->wild_mode) do_cmd_borg();
			}

			break;
		}

#endif /* ALLOW_BORG */



		/*** Inventory Commands ***/

		/* Wear/wield equipment */
		case 'w':
		{
			if (!p_ptr->wild_mode) do_cmd_wield();
			break;
		}

		/* Take off equipment */
		case 't':
		{
			if (!p_ptr->wild_mode) do_cmd_takeoff();
			break;
		}

		/* Drop an item */
		case 'd':
		{
			if (!p_ptr->wild_mode) do_cmd_drop();
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
			if (!p_ptr->wild_mode) do_cmd_alter();
			break;
		}

		/* Dig a tunnel */
		case 'T':
		{
			if (!p_ptr->wild_mode) do_cmd_tunnel();
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
			if (!p_ptr->wild_mode) do_cmd_run();
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

		/* Switch search mode / element scope mode */
		case 'S':
		{
			if (p_ptr->action == ACTION_SEARCH) set_action(ACTION_ELEMSCOPE);
			else if (p_ptr->action == ACTION_ELEMSCOPE) set_action(ACTION_NONE);
			else set_action(ACTION_SEARCH);
			break;
		}


		/*** Stairs and Doors and Chests and Traps ***/

		/* Enter store */
		case SPECIAL_KEY_STORE:
		{
			if (!p_ptr->wild_mode) do_cmd_store();
			break;
		}

		/* Enter building -KMW- */
		case SPECIAL_KEY_BUILDING:
		{
			if (!p_ptr->wild_mode) do_cmd_bldg();
			break;
		}

		/* Enter quest level -KMW- */
		case SPECIAL_KEY_QUEST:
		{
			if (!p_ptr->wild_mode) do_cmd_quest();
			break;
		}

		/* Go up staircase */
		case '<':
		{
			if(!p_ptr->wild_mode && !dun_level && !p_ptr->inside_arena && !p_ptr->inside_quest)
			{
				if (cave[py][px].feat == FEAT_ENTRANCE_UPWARD) do_cmd_go_up();
#if 0
				else if (ambush_flag)
				{
#ifdef JP
					msg_print("襲撃から逃げるにはマップの端まで移動しなければならない。");
#else
					msg_print("To flee the ambush you have to reach the edge of the map.");
#endif
				}
#endif
				else if (!p_ptr->no_digest && (p_ptr->food < PY_FOOD_WEAK))
				{
#ifdef JP
					msg_print("その前に食事をとらないと。");
#else
					msg_print("You must eat something here.");
#endif
				}
				else
				{
					if (change_wild_mode())
					{
						p_ptr->oldpx = px;
						p_ptr->oldpy = py;
					}
				}
			}
			else if (p_ptr->wild_mode)
			{
#ifdef JP
				msg_print("そのコマンドは現在は使えません。");
#else
				msg_print("That command does not work now.");
#endif
			}
			else
				do_cmd_go_up();
			break;
		}

		/* Go down staircase */
		case '>':
		{
			if(!p_ptr->wild_mode) do_cmd_go_down();
			else
			{
				p_ptr->wilderness_x = px;
				p_ptr->wilderness_y = py;
				change_wild_mode();
			}
			break;
		}

		/* Open a door or chest */
		case 'o':
		{
			if (!p_ptr->wild_mode) do_cmd_open();
			break;
		}

		/* Close a door */
		case 'c':
		{
			if (!p_ptr->wild_mode) do_cmd_close();
			break;
		}

		/* Jam a door with spikes */
		case 'j':
		{
			if (!p_ptr->wild_mode) do_cmd_spike();
			break;
		}

		/* Bash a door */
		case 'B':
		{
			if (!p_ptr->wild_mode) do_cmd_bash();
			break;
		}

		/* Disarm a trap or chest */
		case 'D':
		{
			if (!p_ptr->wild_mode) do_cmd_disarm();
			break;
		}


		/*** Magic and Prayers ***/

		/* Browse a book */
		case 'b':
		{
			if (p_ptr->pclass == CLASS_GUNNER) do_cmd_gunner(TRUE);
			else do_cmd_browse();
			break;
		}

		/* Cast a spell */
		case 'm':
		{
			/* -KMW- */
			if (!p_ptr->wild_mode)
			{
				if (!class_info[p_ptr->pclass].realm_choices && (p_ptr->pclass != CLASS_GUNNER))
				{
#ifdef JP
					msg_print("呪文を唱えられない！");
#else
					msg_print("You cannot cast spells!");
#endif
				}
				else if (p_ptr->anti_magic && (p_ptr->pclass != CLASS_GUNNER))
				{
#ifdef JP
					cptr which_power = "魔法";
#else
					cptr which_power = "magic";
#endif
					if (mp_ptr->spell_book == TV_HOLY_BOOK)
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
				else if (is_anti_magic_grid(-1, py, px) && (p_ptr->pclass != CLASS_GUNNER))
				{
#ifdef JP
					cptr which_power = "魔法";
#else
					cptr which_power = "magic";
#endif
					if (mp_ptr->spell_book == TV_HOLY_BOOK)
#ifdef JP
						which_power = "祈り";
#else
						which_power = "prayer";
#endif

#ifdef JP
					msg_format("反魔法フィールドが%sを邪魔した！", which_power);
#else
					msg_format("An anti-magic field disrupts your %s!", which_power);
#endif
					energy_use = 100;
				}
				else if (p_ptr->shero)
				{
#ifdef JP
					msg_format("狂戦士化していて頭が回らない！");
#else
					msg_format("You cannot think directly!");
#endif
					energy_use = 0;
				}
				else if (p_ptr->pclass == CLASS_GUNNER) do_cmd_gunner(FALSE);
				else do_cmd_cast();
			}
			break;
		}

		/* Pray a prayer */
		case 'P':
		{
			do_cmd_pray();
			break;
		}

		/* Issue a pet command */
		case 'p':
		{
			if (!p_ptr->wild_mode) do_cmd_pet();
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
			if (!p_ptr->wild_mode)
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
			if (!p_ptr->wild_mode) (void)do_cmd_fire(DCFA_NONE, 0, 0, 0, FALSE);
			break;
		}

		/* Throw an item */
		case 'v':
		{
			if (!p_ptr->wild_mode)
			{
				do_cmd_throw();
			}
			break;
		}

		/* Aim a wand */
		case 'a':
		{
			if (!p_ptr->wild_mode)
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
			}
			break;
		}

		/* Zap a rod */
		case 'z':
		{
			if (!p_ptr->wild_mode)
			{
			if (p_ptr->inside_arena)
			{
#ifdef JP
				msg_print("アリーナが魔法を吸収した！");
#else
				msg_print("The arena absorbs all attempted magic!");
#endif

				msg_print(NULL);
			}
			else if (use_command && rogue_like_commands)
			{
				do_cmd_use();
			}
			else
			{
				do_cmd_zap_rod();
			}
			}
			break;
		}

		/* Quaff a potion */
		case 'q':
		{
			if (!p_ptr->wild_mode)
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
			}
			break;
		}

		/* Read a scroll */
		case 'r':
		{
			if (!p_ptr->wild_mode)
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
			}
			break;
		}

		/* Use a staff */
		case 'u':
		{
			if (!p_ptr->wild_mode)
			{
			if (p_ptr->inside_arena)
			{
#ifdef JP
				msg_print("アリーナが魔法を吸収した！");
#else
				msg_print("The arena absorbs all attempted magic!");
#endif

				msg_print(NULL);
			}
			else if (use_command && !rogue_like_commands)
			{
				do_cmd_use();
			}
			else
				do_cmd_use_staff();
			}
			break;
		}

		/* Use racial power */
		case 'U':
		{
			if (!p_ptr->wild_mode) do_cmd_racial_power();
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
			if (!p_ptr->wild_mode) do_cmd_target();
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

		case '$':
		{
			do_cmd_pickpref();
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
			do_cmd_redraw();
			break;
		}

		/* Interact with colors */
		case '&':
		{
			do_cmd_colors();
			do_cmd_redraw();
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
			if (!p_ptr->wild_mode) do_cmd_feeling();
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
			now_message = old_now_message;
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

		case '|':
		{
			do_cmd_nikki();
			break;
		}

		/* Check artifacts, uniques, objects */
		case '~':
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

		/* Make random artifact list */
		case KTRL('V'):
		{
			spoil_random_artifact("randifact.txt");
			break;
		}

		/* Give some money townspeople */
		case 'G':
		{
			do_cmd_give_money();
			break;
		}

		/* Hack -- Unknown command */
		default:
		{
			if (flush_failure) flush();
			if (one_in_(2))
			{
				char error_m[1024];
				sound(SOUND_ILLEGAL);
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
	if (!energy_use && !now_message)
		now_message = old_now_message;
}




/* Hack -- Pack Overflow */
static void pack_overflow(void)
{
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

		/* Notice stuff (if needed) */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff (if needed) */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff (if needed) */
		if (p_ptr->redraw) redraw_stuff();

		/* Redraw stuff (if needed) */
		if (p_ptr->window) window_stuff();
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

	/*** Apply energy ***/

	/* Give the player some energy */
	if (!(load && p_ptr->energy_need <= 0))
	{
		p_ptr->energy_need -= (p_ptr->pspeed > 199 ? 49 : (p_ptr->pspeed < 0 ? 1 : extract_energy[p_ptr->pspeed]));
	}

	/* No turn yet */
	if (p_ptr->energy_need > 0) return;
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
				set_action(ACTION_NONE);
			}
		}

		/* Complete resting */
		else if (resting == -2)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) &&
			    (p_ptr->csp >= p_ptr->msp) &&
			    !p_ptr->blind && !p_ptr->confused &&
			    !p_ptr->poisoned && !p_ptr->afraid &&
			    !p_ptr->stun && !p_ptr->cut &&
			    !p_ptr->slow && !p_ptr->paralyzed &&
			    !p_ptr->image && !p_ptr->word_recall &&
			    !p_ptr->alter_reality && !p_ptr->inhibit_flood &&
			    !p_ptr->tim_dec_blow)
			{
				set_action(ACTION_NONE);
			}
		}
	}

	/* Handle "abort" */
	if (check_abort)
	{
		/* Check for "player abort" (semi-efficiently for resting) */
		if (running || command_rep || (p_ptr->action == ACTION_REST))
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
				msg_print("Canceled.");
#endif

			}
		}
	}

	if (p_ptr->riding && !p_ptr->confused && !p_ptr->blind)
	{
		monster_type *m_ptr = &m_list[p_ptr->riding];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		if (MON_CSLEEP(m_ptr))
		{
			char m_name[80];

			/* Recover fully */
			(void)set_monster_csleep(p_ptr->riding, 0);

			/* Acquire the monster name */
			monster_desc(m_name, m_ptr, 0);
#ifdef JP
			msg_format("%^sを起こした。", m_name);
#else
			msg_format("You have waked %s up.", m_name);
#endif
			if (p_ptr->health_who == p_ptr->riding) p_ptr->redraw |= (PR_HEALTH);
			p_ptr->redraw |= (PR_UHEALTH);
		}

		if (MON_STUNNED(m_ptr))
		{
			/* Hack -- Recover from stun */
			if (set_monster_stunned(p_ptr->riding,
				(randint0(r_ptr->level) < (skill_lev_var[p_ptr->skill_exp[SKILL_RIDING]/10] * 1000)) ? 0 : (MON_STUNNED(m_ptr) - 1)))
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
#ifdef JP
				msg_format("%^sを朦朧状態から立ち直らせた。", m_name);
#else
				msg_format("%^s is no longer stunned.", m_name);
#endif
			}
		}

		if (MON_CONFUSED(m_ptr))
		{
			/* Hack -- Recover from confusion */
			if (set_monster_confused(p_ptr->riding,
				(randint0(r_ptr->level) < (skill_lev_var[p_ptr->skill_exp[SKILL_RIDING]/10] * 1000)) ? 0 : (MON_CONFUSED(m_ptr) - 1)))
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
#ifdef JP
				msg_format("%^sを混乱状態から立ち直らせた。", m_name);
#else
				msg_format("%^s is no longer confused.", m_name);
#endif
			}
		}

		if (MON_MONFEAR(m_ptr))
		{
			/* Hack -- Recover from fear */
			if (set_monster_monfear(p_ptr->riding,
				(randint0(r_ptr->level) < (skill_lev_var[p_ptr->skill_exp[SKILL_RIDING]/10] * 1000)) ? 0 : (MON_MONFEAR(m_ptr) - 1)))
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
#ifdef JP
				msg_format("%^sを恐怖から立ち直らせた。", m_name);
#else
				msg_format("%^s is no longer fear.", m_name);
#endif
			}
		}

		/* Handle "p_ptr->update" and "p_ptr->redraw" and "p_ptr->window" */
		handle_stuff();
	}

	/* Handle the player song */
	if (!load) check_music();

	load = FALSE;

	/* Aura of Lord */
	if (p_ptr->action == ACTION_AURA)
	{
		s32b use_mana = 10;

		use_mana *= 0x8000;
		if ((u16b)(p_ptr->csp) < (use_mana / 0x10000))
		{
			/* Mana run out */
			p_ptr->csp = 0;
			p_ptr->csp_frac = 0;
			set_action(ACTION_NONE);
		}
		else
		{
			p_ptr->csp -= (u16b) (use_mana / 0x10000);
			use_mana = (use_mana & 0x0000ffff);
			if (p_ptr->csp_frac < (u32b)use_mana)
			{
				p_ptr->csp--;
				p_ptr->csp_frac += (u16b)(0x10000L - use_mana);
			}
			else
			{
				p_ptr->csp_frac -= (u16b)use_mana;
			}
		}
		p_ptr->redraw |= PR_MANA;
	}

	/*** Handle actual user input ***/

	/* Repeat until out of energy */
	while (p_ptr->energy_need <= 0)
	{
		p_ptr->window |= PW_PLAYER;
		now_damaged = FALSE;

		/* Notice stuff (if needed) */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff (if needed) */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff (if needed) */
		if (p_ptr->redraw) redraw_stuff();

		/* Redraw stuff (if needed) */
		if (p_ptr->window) window_stuff();


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
		if (p_ptr->paralyzed || (stun_level(p_ptr->stun) >= 4))
		{
			/* Take a turn */
			energy_use = 100;
		}

		/* Resting */
		else if (p_ptr->action == ACTION_REST)
		{
			/* Timed rest */
			if (resting > 0)
			{
				/* Reduce rest count */
				resting--;

				if (!resting) set_action(ACTION_NONE);

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
			if (stop_the_time_player || (energy_use > 400))
			{
				/* The Randomness is irrelevant */
				p_ptr->energy_need += energy_use * TURNS_PER_TICK / 10;
			}
			else
			{
				/* There is some randomness of needed energy */
				p_ptr->energy_need += (s16b)((s32b)energy_use * ENERGY_NEED() / 100L);
			}

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
					if (m_ptr->mflag2 & MFLAG2_MARK)
					{
						/* Maintain detection */
						if (m_ptr->mflag2 & MFLAG2_SHOW)
						{
							/* Forget flag */
							m_ptr->mflag2 &= ~(MFLAG2_SHOW);

							/* Still need repairs */
							repair_monsters = TRUE;
						}

						/* Remove detection */
						else
						{
							/* Forget flag */
							m_ptr->mflag2 &= ~(MFLAG2_MARK);

							/* Assume invisible */
							m_ptr->ml = FALSE;

							/* Update the monster */
							update_mon(i, FALSE);

							if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
							if (p_ptr->riding == i) p_ptr->redraw |= (PR_UHEALTH);

							/* Redraw regardless */
							lite_spot(m_ptr->fy, m_ptr->fx);
						}
					}
				}
			}

			if (stop_the_time_player && (p_ptr->energy_need > - 1000))
			{
				/* Redraw map */
				p_ptr->redraw |= (PR_MAP);

				/* Update monsters */
				p_ptr->update |= (PU_MONSTERS);

				/* Window stuff */
				p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

#ifdef JP
				msg_print("止まっていた時が動き出した。");
#else
				msg_print("You feel time flowing around you once more.");
#endif
				msg_print(NULL);
				stop_the_time_player = FALSE;
				p_ptr->energy_need = ENERGY_NEED();

				handle_stuff();
			}
		}

		/* Hack -- notice death */
		if (!p_ptr->playing || p_ptr->is_dead)
		{
			stop_the_time_player = FALSE;
			break;
		}

		/* Handle "leaving" */
		if (p_ptr->leaving) break;
	}

	/* Update scent trail */
	update_smell();
}


/*
 * Interact with the current dungeon level.
 *
 * This function will not exit until the level is completed,
 * the user dies, or the game is terminated.
 */
static void dungeon(void)
{
	int quest_num = 0;
	int i;

	/* Set the base level */
	base_level = dun_level;

	/* Reset various flags */
	hack_mind = FALSE;

	/* Not leaving */
	p_ptr->leaving = FALSE;

	/* Reset the "command" vars */
	command_cmd = 0;

#if 0 /* Don't reset here --- It's used for Arena */
	command_new = 0;
#endif

	command_rep = 0;
	command_arg = 0;
	command_dir = 0;


	/* Cancel the target */
	target_who = 0;
	pet_t_m_idx = 0;
	riding_t_m_idx = 0;

	/* Cancel the health bar */
	health_track(0);

	/* Check visual effects */
	shimmer_monsters = TRUE;
	shimmer_objects = TRUE;
	repair_monsters = TRUE;
	repair_objects = TRUE;


	/* Disturb */
	disturb(1, 0);

	/* Get index of current quest (if any) */
	quest_num = quest_number(dun_level);

	/* Inside a quest? */
	if (quest_num && !(r_info[quest[quest_num].r_idx].flags1 & RF1_UNIQUE))
	{
		/* Mark the quest monster */
		r_info[quest[quest_num].r_idx].flags1 |= RF1_QUESTOR;
	}

	/* Track maximum player level */
	if (p_ptr->max_plv < p_ptr->lev) p_ptr->max_plv = p_ptr->lev;
	if (p_ptr->max_max_plv < p_ptr->max_plv) p_ptr->max_max_plv = p_ptr->max_plv;
	for (i = 0; i < max_c_idx; i++)
	{
		cexp_info_type *cexp_ptr = &p_ptr->cexp_info[i];
		if (cexp_ptr->max_clev < cexp_ptr->clev) cexp_ptr->max_clev = cexp_ptr->clev;
		if (cexp_ptr->max_max_clev < cexp_ptr->max_clev) cexp_ptr->max_max_clev = cexp_ptr->max_clev;
	}


	/* Track maximum dungeon level (if not in quest -KMW-) */
	if ((max_dlv[dungeon_type] < dun_level) && !p_ptr->inside_quest)
	{
		max_dlv[dungeon_type] = dun_level;
		if (record_maxdeapth) do_cmd_write_nikki(NIKKI_MAXDEAPTH, dun_level, NULL);
	}

	(void)calculate_upkeep();

	/* Validate the panel */
	panel_bounds_center();

	/* Verify the panel */
	verify_panel();

	/* Flush messages */
	msg_print(NULL);


	/* Enter "xtra" mode */
	character_xtra = TRUE;

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER | PW_MONSTER | PW_OVERHEAD | PW_DUNGEON);

	/* Redraw dungeon */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_EQUIPPY);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_GOLD);

	/* Update lite/view */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_MON_LITE | PU_TORCH);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS | PU_DISTANCE | PU_FLOW);

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Redraw stuff */
	window_stuff();

	/* Leave "xtra" mode */
	character_xtra = FALSE;

	if (p_ptr->prace == RACE_MERMAID) set_mermaid_in_water();

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

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

	if ((quest_num && (quest_is_fixed(quest_num) &&
	   !((quest[quest_num].flags & QUEST_FLAG_GUARDIAN) ||
	    !(quest[quest_num].flags & QUEST_FLAG_PRESET))))
	   || (!dun_level && p_ptr->town_num && (p_ptr->town_num != TOWN_LOST_ISLAND) && !p_ptr->inside_arena)) do_cmd_feeling();

	/* Hack -- notice death or departure */
	if (!p_ptr->playing || p_ptr->is_dead) return;

	/* Print quest message if appropriate */
	if (!p_ptr->inside_quest && (dungeon_type == DUNGEON_PALACE))
	{
		quest_discovery(random_quest_number(dun_level));
		p_ptr->inside_quest = random_quest_number(dun_level);
	}
	if ((dun_level == d_info[dungeon_type].maxdepth) && d_info[dungeon_type].final_guardian)
	{
		if (r_info[d_info[dungeon_type].final_guardian].max_num)
#ifdef JP
			msg_format("この階には%sの主である%sが棲んでいる。",
				   d_name+d_info[dungeon_type].name, 
				   r_name+r_info[d_info[dungeon_type].final_guardian].name);
#else
		msg_format("%^s lives in this level as the keeper of %s.",
				   r_name+r_info[d_info[dungeon_type].final_guardian].name, 
				   d_name+d_info[dungeon_type].name);
#endif
	}

	/*** Process this dungeon level ***/

	/* Reset the monster generation level */
	monster_level = base_level;

	/* Reset the object generation level */
	object_level = base_level;

	hack_mind = TRUE;

	if (p_ptr->energy_need > 0 &&
	    (dun_level || p_ptr->leaving_dungeon || p_ptr->inside_arena))
		p_ptr->energy_need = 0;

	/* Not leaving dungeon */
	p_ptr->leaving_dungeon = FALSE;

	/* Initialize monster process */
	mproc_init();

	/* Main loop */
	while (TRUE)
	{
		/* Hack -- Compact the monster list occasionally */
		if (m_cnt + 32 > max_m_idx) compact_monsters(64);

		/* Hack -- Compress the monster list occasionally */
		if (m_cnt + 32 < m_max) compact_monsters(0);


		/* Hack -- Compact the object list occasionally */
		if (o_cnt + 32 > max_o_idx) compact_objects(64);

		/* Hack -- Compress the object list occasionally */
		if (o_cnt + 32 < o_max) compact_objects(0);


		/* Process the player */
		process_player();

		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Redraw stuff */
		if (p_ptr->window) window_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(py, px);

		/* Optional fresh */
		if (fresh_after) Term_fresh();

		/* Hack -- Notice death or departure */
		if (!p_ptr->playing || p_ptr->is_dead) break;

		/* Process all of the monsters */
		process_monsters();

		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Redraw stuff */
		if (p_ptr->window) window_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(py, px);

		/* Optional fresh */
		if (fresh_after) Term_fresh();

		/* Hack -- Notice death or departure */
		if (!p_ptr->playing || p_ptr->is_dead) break;


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
		move_cursor_relative(py, px);

		/* Optional fresh */
		if (fresh_after) Term_fresh();

		/* Hack -- Notice death or departure */
		if (!p_ptr->playing || p_ptr->is_dead) break;

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Count game turns */
		turn++;

		if (dungeon_turn < dungeon_turn_limit)
		{
			if (!p_ptr->wild_mode || wild_regen) dungeon_turn++;
			else if (p_ptr->wild_mode && !(turn % ((MAX_HGT + MAX_WID) / 2))) dungeon_turn++;
		}

		prevent_turn_overflow();

		if (wild_regen) wild_regen--;
	}

	ambush_flag = FALSE;

	/* Inside a quest and non-unique questor? */
	if (quest_num && !(r_info[quest[quest_num].r_idx].flags1 & RF1_UNIQUE))
	{
		/* Un-mark the quest monster */
		r_info[quest[quest_num].r_idx].flags1 &= ~RF1_QUESTOR;
	}

	/* Not save-and-quit and not dead? */
	if (p_ptr->playing && !p_ptr->is_dead)
	{
		/*
		 * Maintain Unique monsters and artifact, save current
		 * floor, then prepare next floor
		 */
		leave_floor();

		/* Forget the flag */
		reinit_wilderness = FALSE;
	}

	/* Write about current level on the play record once per level */
	write_level = TRUE;
}


/*
 * Load some "user pref files"
 *
 * Modified by Arcum Dagsson to support
 * separate macro files for different realms.
 */
void load_all_pref_files(void)
{
	char buf[1024];
	errr err;
	int i;

	/* Access the "user" pref file */
	sprintf(buf, "user.prf");

	/* Process that file */
	process_pref_file(buf);

	/* Access the "user" system pref file */
	sprintf(buf, "user-%s.prf", ANGBAND_SYS);

	/* Process that file */
	process_pref_file(buf);

	/* Access the "race" pref file */
#ifdef JP
	sprintf(buf, "%s.prf", p_name + rp_ptr->E_name);
#else
	sprintf(buf, "%s.prf", p_name + rp_ptr->name);
#endif

	/* Process that file */
	process_pref_file(buf);

	/* Access the "class" pref file */
#ifdef JP
	sprintf(buf, "%s.prf", c_name + cp_ptr->E_name);
#else
	sprintf(buf, "%s.prf", c_name + cp_ptr->name);
#endif

	/* Process that file */
	process_pref_file(buf);

	/* Access the "character" pref file */
	sprintf(buf, "%s.prf", player_base);

	/* Process that file */
	process_pref_file(buf);

	/* Free old entries */
	init_autopicker();

#ifdef JP
	sprintf(buf, "picktype-%s.prf", player_base);
#else
	sprintf(buf, "pickpref-%s.prf", player_base);
#endif

	err = process_pickpref_file(buf);

	/* Process 'pick????.prf' if 'pick????-<name>.prf' doesn't exist */
	if (0 > err)
	{
#ifdef JP
		process_pickpref_file("picktype.prf");
#else
		process_pickpref_file("pickpref.prf");
#endif
	}

	/* Access the player's realm pref file */
	for (i = 1; i <= MAX_REALM; i++)
	{
		if (can_use_realm(i))
		{
			sprintf(buf, "%s.prf", realm_names[i]);

			/* Process that file */
			process_pref_file(buf);
		}
	}
}


/*
 * Initialize the magic realm table
 */
void init_realm_table(void)
{
	s32b mask = (CH_FIRE | CH_AQUA | CH_EARTH | CH_WIND);


	if (p_ptr->pclass == CLASS_MEDIUM)
	{
		cp_ptr->realm_choices = p_ptr->realm_medium;
		mask &= ~(CH_FIRE << get_cur_pelem());
		cp_ptr->realm_choices &= ~mask;
	}

	else if (cp_ptr->c_flags & PCF_REALM_ELEM_1)
	{
		cp_ptr->realm_choices |= mask;
		mask &= ~(CH_FIRE << get_cur_pelem());
		cp_ptr->realm_choices &= ~mask;
	}
}


/*
 * Extract option variables from bit sets
 */
void extract_option_vars(void)
{
	int i;

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

	/* Hack -- Character is "icky" */
	character_icky = TRUE;

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

	/* Hack -- turn off the cursor */
	(void)Term_set_cursor(0);


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

	/* Extract the options */
	extract_option_vars();

	creating_savefile = new_game;

	/* Nothing loaded */
	if (!character_loaded)
	{
		/* Make new player */
		new_game = TRUE;

		/* The dungeon is not ready */
		character_dungeon = FALSE;

		/* Prepare to init the RNG */
		Rand_quick = TRUE;

		/* Initialize the saved floors data */
		init_saved_floors();
	}

	/* Old game is loaded.  But new game is requested. */
	else if (new_game)
	{
		/* Delete expanded temporal files */
		clear_saved_floor_files();
	}

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

	/* Roll new character */
	if (new_game)
	{
		monster_race *r_ptr;
		u32b old_seed_flavor = seed_flavor;

		/* The dungeon is not ready */
		character_dungeon = FALSE;

		/* Start in town */
		dun_level = 0;
		p_ptr->inside_quest = 0;
		p_ptr->inside_arena = FALSE;

		write_level = TRUE;

		/* Hack -- seed for flavors */
		seed_flavor = randint0(0x10000000);

		/* Hack -- seed for town layout */
		seed_town = randint0(0x10000000);

		/* Roll up a new character */
		player_birth();
		if (astral_mode) seed_flavor = old_seed_flavor;

		load = FALSE;
		get_mon_num_prep(NULL, NULL);
		for (i = 0; i < MAX_KUBI; i++)
		{
			while (1)
			{
				int j;

				kubi_r_idx[i] = get_mon_num(MAX_DEPTH - 1);
				r_ptr = &r_info[kubi_r_idx[i]];

				if (!(r_ptr->flags1 & RF1_UNIQUE)) continue;

				if (!(r_ptr->flags9 & (RF9_DROP_CORPSE | RF9_DROP_SKELETON))) continue;
				if (r_ptr->rarity > 100) continue;

				if (monster_is_runeweapon(kubi_r_idx[i])) continue;

				for (j = 0; j < i; j++)
					if (kubi_r_idx[i] == kubi_r_idx[j])break;

				if (j == i) break;
			}
		}
		for (i = 0; i < MAX_KUBI -1; i++)
		{
			int j,tmp;
			for (j = i; j < MAX_KUBI; j++)
			{
				if (r_info[kubi_r_idx[i]].level > r_info[kubi_r_idx[j]].level)
				{
					tmp = kubi_r_idx[i];
					kubi_r_idx[i] = kubi_r_idx[j];
					kubi_r_idx[j] = tmp;
				}
			}
		}

		while (1)
		{
			today_mon = get_mon_num(3);
			r_ptr = &r_info[today_mon];

			if (r_ptr->flags1 & RF1_UNIQUE) continue;
			if (r_ptr->flags2 & (RF2_MULTIPLY)) continue;
			if (!(r_ptr->flags9 & RF9_DROP_CORPSE) || !(r_ptr->flags9 & RF9_DROP_SKELETON)) continue;
			if (r_ptr->rarity > 10) continue;
			if (r_ptr->level == 0) continue;
			break;
		}

		for (i = MIN_WEATHER_TYPE; i < WEATHER_TYPE_NUM; i++)
		{
			weather[i] = randint0(MAX_WEATHER_VAL);
			prev_weather[i] = weather[i];
		}
		weather_time_to_change = 100 + randint1(100);
	}
	else
	{
		write_level = FALSE;

#ifdef JP
		do_cmd_write_nikki(NIKKI_GAMESTART, 1, "                            ----ゲーム再開----");
#else
		do_cmd_write_nikki(NIKKI_GAMESTART, 1, "                            ---- Restart Game ----");
#endif

	}

	init_realm_table();

	creating_savefile = FALSE;

	p_ptr->teleport_town = FALSE;
	stop_the_time_monster = FALSE;
	now_damaged = FALSE;
	now_message = 0;
	start_time = time(NULL) - 1;
	record_o_name[0] = '\0';

	/* Reset map panel */
	panel_row_min = cur_hgt;
	panel_col_min = cur_wid;

	/* Fill the arrays of floors and walls in the good proportions */
	set_floor_and_wall(dungeon_type);

	/* Flavor the objects */
	flavor_init();
	if (new_game)
	{
		wipe_o_list();
		player_outfit();
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
	if (arg_wizard)
	{
		if (enter_wizard_mode()) p_ptr->wizard = TRUE;
		else if (p_ptr->is_dead) quit("Already dead.");
	}

	/* Initialize the town-buildings if necessary */
	if (!dun_level && !p_ptr->inside_quest)
	{
		/* Init the wilderness */
		process_dungeon_file("w_info.txt", 0, 0, max_wild_y, max_wild_x);

		/* Init the town */
		init_flags = INIT_ONLY_BUILDINGS;

		process_dungeon_file("t_info.txt", 0, 0, MAX_HGT, MAX_WID);

	}


	verify_runeweapon();

	/* Generate a dungeon level if needed */
	if (!character_dungeon) change_floor();


	/* Character is now "complete" */
	character_generated = TRUE;


	/* Hack -- Character is no longer "icky" */
	character_icky = FALSE;


	if (new_game)
	{
		char buf[80];

#ifdef JP
		sprintf(buf, "%sに降り立った。", map_name());
#else
		sprintf(buf, "You are standing in the %s.", map_name());
#endif
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, buf);

		if (astral_mode && ancestor_inventory)
		{
			int y, x;
			object_type *o_ptr;
			bool flag = FALSE;

			for (y = 0; y < cur_hgt; y++)
			{
				for (x = 0; x < cur_wid; x++)
				{
					if (cave[y][x].feat == (FEAT_SHOP_HEAD + STORE_HOME))
					{
						for (i = 0; i < ancestor_inven_cnt; i++)
						{
							o_ptr = &ancestor_inventory[i];
							if (o_ptr->k_idx) drop_near(o_ptr, 0, y, x);
						}
						flag = TRUE;
						break;
					}
				}
				if (flag) break; /* Double break */
			}

			/* Free the array */
			C_KILL(ancestor_inventory, INVEN_TOTAL, object_type);
			ancestor_inven_cnt = 0;
		}
	}


	/* Start game */
	p_ptr->playing = TRUE;

	/* Reset the visual mappings */
	reset_visuals();

	/* Load the "pref" files */
	load_all_pref_files();

	/* React to changes */
	Term_xtra(TERM_XTRA_REACT, 0);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_DUNGEON | PW_MONSTER | PW_OBJECT);

	/* Window stuff */
	window_stuff();


	/* Set or clear "rogue_like_commands" if requested */
	if (arg_force_original) rogue_like_commands = FALSE;
	if (arg_force_roguelike) rogue_like_commands = TRUE;

	/* Hack -- Enforce "delayed death" */
	if (p_ptr->stoning >= 250) p_ptr->is_dead |= DEATH_STONED;
	if ((p_ptr->chp < 0) || p_ptr->is_dead) p_ptr->is_dead |= DEATH_DEAD;

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
		if (!p_ptr->playing && !p_ptr->is_dead) break;

		/* Erase the old cave */
		wipe_o_list();
		if (!p_ptr->is_dead) wipe_m_list();


		/* XXX XXX XXX */
		msg_print(NULL);

		/* Accidental Death */
		if (p_ptr->playing && p_ptr->is_dead)
		{
			if (p_ptr->inside_arena && !(p_ptr->is_dead & DEATH_SNAP_DRAGON))
			{
				p_ptr->inside_arena = FALSE;
				if(p_ptr->arena_number > MAX_ARENA_MONS)
					p_ptr->arena_number++;
				else
					p_ptr->arena_number = -1 - p_ptr->arena_number;
				p_ptr->is_dead = 0L;
				p_ptr->chp = 0;
				p_ptr->chp_frac = 0;
				p_ptr->exit_bldg = TRUE;
				reset_tim_flags();

				/* Leave through the exit */
				prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_RAND_CONNECT);

				/* prepare next floor */
				leave_floor();
			}
			else
			{
				/* Mega-Hack -- Allow player to cheat death */
#ifdef JP
				if ((p_ptr->wizard || cheat_live) && !get_check("死にますか? "))
#else
				if ((p_ptr->wizard || cheat_live) && !get_check("Die? "))
#endif
				{
					/* Mark social class, reset age, if needed */
					if (p_ptr->sc) p_ptr->sc = p_ptr->age = 0;

					/* Increase age */
					p_ptr->age++;

					/* Mark savefile */
					p_ptr->noscore |= 0x0001;

					/* Message */
#ifdef JP
					msg_print("ウィザードモードに念を送り、死を欺いた。");
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
					(void)set_stoning(0);
					(void)set_opposite_pelem(0);
					(void)set_no_elem(0);

					/* Hack -- Prevent starvation */
					if (p_ptr->no_digest) p_ptr->food = PY_FOOD_FULL - 1;
					else (void)set_food(PY_FOOD_MAX - 1);

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

					/* Hack -- cancel alter */
					if (p_ptr->alter_reality)
					{
						/* Hack -- Prevent alter */
						p_ptr->alter_reality = 0;
						p_ptr->redraw |= (PR_STATUS);
					}

					/* Note cause of death XXX XXX XXX */
#ifdef JP
					(void)strcpy(p_ptr->died_from, "死の欺き");
#else
					(void)strcpy(p_ptr->died_from, "Cheating death");
#endif


	if (new_game)
	{
		char buf[80];

#ifdef JP
		sprintf(buf, "%sに降り立った。", map_name());
#else
		sprintf(buf, "You are standing in the %s.", map_name());
#endif
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, buf);
	}


					/* Do not die */
					p_ptr->is_dead = 0L;

					/* Wipe the temporary "Runeweapon" */
					(void)WIPE(&runeweapon_list[0], runeweapon_type);

					dun_level = 0;
					p_ptr->inside_arena = FALSE;
					leaving_quest = 0;
					p_ptr->inside_quest = 0;
					if (dungeon_type) p_ptr->recall_dungeon = dungeon_type;
					dungeon_type = 0;
					if (astral_mode)
					{
						p_ptr->wilderness_y = 1;
						p_ptr->wilderness_x = 1;
						if (astral_mode)
						{
							p_ptr->oldpy = 10;
							p_ptr->oldpx = 34;
						}
						else
						{
							p_ptr->oldpy = 33;
							p_ptr->oldpx = 131;
						}
					}
					else
					{
						int x = 0, y;

						process_dungeon_file("w_info.txt", 0, 0, max_wild_y, max_wild_x);

						for (y = 0; y < max_wild_y; y++)
						{
							for (x = 0; x < max_wild_x; x++)
							{
								if (wilderness[y][x].town == TOWN_ARMORICA) break;
							}
							if (wilderness[y][x].town == TOWN_ARMORICA) break;
						}

						p_ptr->wilderness_y = y;
						p_ptr->wilderness_x = x;
						p_ptr->oldpy = 33;
						p_ptr->oldpx = 131;
					}

					/* Leaving */
					p_ptr->wild_mode = FALSE;
					p_ptr->leaving = TRUE;

#ifdef JP
					do_cmd_write_nikki(NIKKI_BUNSHOU, 1, "                            しかし、不正に生き返った。");
#else
					do_cmd_write_nikki(NIKKI_BUNSHOU, 1, "                            but revived illegally.");
#endif

					/* Prepare next floor */
					leave_floor();
					wipe_m_list();
				}
			}
		}

		/* Handle "death" */
		if (p_ptr->is_dead) break;

		/* Reset decoy */
		if (p_ptr->use_decoy)
		{
			p_ptr->decoy_y = 0;
			p_ptr->decoy_x = 0;
			p_ptr->use_decoy = FALSE;
			p_ptr->update |= (PU_BONUS);
		}

		/* Reset effect of "The Fool" */
		fool_effect_status = FOOL_STATUS_NONE;

		/* Make a new level */
		change_floor();
	}

	/* Close stuff */
	close_game();

	/* Quit */
	quit(NULL);
}

s32b turn_real(s32b hoge)
{
	return hoge;
}

/*
 * ターンのオーバーフローに対する対処
 * ターン及びターンを記録する変数をターンの限界の1日前まで巻き戻す.
 */
void prevent_turn_overflow(void)
{
	int rollback_days, i, j;
	s32b rollback_turns;

	if (turn < turn_limit) return;

	rollback_days = 1 + (turn - turn_limit) / (TURNS_PER_TICK * TOWN_DAWN);
	rollback_turns = TURNS_PER_TICK * TOWN_DAWN * rollback_days;

	if (turn > rollback_turns) turn -= rollback_turns;
	else turn = 1; /* Paranoia */
	if (old_turn > rollback_turns) old_turn -= rollback_turns;
	else old_turn = 1;
	if (old_battle > rollback_turns) old_battle -= rollback_turns;
	else old_battle = 1;
	if (p_ptr->feeling_turn > rollback_turns) p_ptr->feeling_turn -= rollback_turns;
	else p_ptr->feeling_turn = 1;

	for (i = 1; i < max_towns; i++)
	{
		for (j = 0; j < MAX_STORES; j++)
		{
			store_type *st_ptr = &town[i].store[j];

			if (st_ptr->last_visit > -200L * STORE_TURNS)
			{
				st_ptr->last_visit -= rollback_turns;
				if (st_ptr->last_visit < -200L * STORE_TURNS) st_ptr->last_visit = -200L * STORE_TURNS;
			}

			if (st_ptr->store_open)
			{
				st_ptr->store_open -= rollback_turns;
				if (st_ptr->store_open < 1) st_ptr->store_open = 1;
			}
		}
	}
}
