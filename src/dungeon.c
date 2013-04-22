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
#define DG_CURSE_CHANCE 50
#define AUTO_CURSE_CHANCE 15
#define CHAINSWORD_NOISE 100

/*
 * I created this when a bug misplaced my character and the game wasn't able
 * to load it again.. very frustrating.
 * So this hack will generate a new level without calling dungeon(), and
 * then the normal behavior will apply.
 */
/* #define SAVE_HACK */
#ifdef SAVE_HACK
bool save_hack = TRUE;
#endif

/*
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
static byte value_check_aux1(object_type *o_ptr)
{
	/* Artifacts */
	if (artifact_p(o_ptr) || o_ptr->art_name)
	{
		/* Cursed/Broken */
                if (cursed_p(o_ptr)) return SENSE_TERRIBLE;

		/* Normal */
		return SENSE_SPECIAL;
	}

	/* Ego-Items */
	if (ego_item_p(o_ptr))
	{
		/* Cursed/Broken */
                if (cursed_p(o_ptr)) return SENSE_WORTHLESS;

		/* Normal */
		return SENSE_EXCELLENT;
	}

	/* Cursed items */
	if (cursed_p(o_ptr)) return SENSE_CURSED;

	/* Good "armor" bonus */
	if (o_ptr->to_a > 0) return SENSE_GOOD_HEAVY;

	/* Good "weapon" bonus */
	if (o_ptr->to_h + o_ptr->to_d > 0) return SENSE_GOOD_HEAVY;

	/* Default to "average" */
	return SENSE_AVERAGE;
}

static byte value_check_aux1_magic(object_type *o_ptr)
{
	/* Potions and Scrolls */
	if ((o_ptr->tval == TV_SCROLL) || (o_ptr->tval == TV_POTION) ||
            (o_ptr->tval == TV_POTION2) || (o_ptr->tval == TV_WAND) ||
            (o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_ROD) ||
            (o_ptr->tval == TV_ROD_MAIN))
	{
		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		/* "Cursed" scrolls/potions have a cost of 0 */
		if (k_ptr->cost == 0) return SENSE_TERRIBLE;

                /* Artifacts */
                if (artifact_p(o_ptr)) return SENSE_SPECIAL;

		/* Scroll of Nothing, Apple Juice, etc. */
		if (k_ptr->cost < 3) return SENSE_WORTHLESS;

		/* Identify, Phase Door, Cure Light Wounds, etc. are just
		 * average*/
		if (k_ptr->cost < 100) return SENSE_AVERAGE;

		/* Enchant Armor, *Identify*, Restore Stat, etc. */
		if (k_ptr->cost < 10000) return SENSE_GOOD_HEAVY;

		/* Acquirement, Deincarnation, Strength, Blood of Life, ... */
		if (k_ptr->cost >= 10000) return SENSE_EXCELLENT;
	}

	/* Food */
	if (o_ptr->tval == TV_FOOD)
	{
		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		/* "Cursed" food */
		if (k_ptr->cost == 0) return SENSE_TERRIBLE;

                /* Artifacts */
                if (artifact_p(o_ptr)) return SENSE_SPECIAL;

		/* Normal food (no magical properties) */
		if (k_ptr->cost <= 10) return SENSE_AVERAGE;

		/* Everything else is good */
		if (k_ptr->cost > 10) return SENSE_GOOD_HEAVY;
	}

        return SENSE_NONE;
}

/*
 * Return a "feeling" (or NULL) about an item.  Method 2 (Light).
 */
static byte value_check_aux2(object_type *o_ptr)
{
	/* Cursed items (all of them) */
	if (cursed_p(o_ptr)) return SENSE_CURSED;

	/* Artifacts -- except cursed/broken ones */
	if (artifact_p(o_ptr) || o_ptr->art_name) return SENSE_GOOD_LIGHT;

	/* Ego-Items -- except cursed/broken ones */
	if (ego_item_p(o_ptr)) return SENSE_GOOD_LIGHT;

	/* Good armor bonus */
	if (o_ptr->to_a > 0) return SENSE_GOOD_LIGHT;

	/* Good weapon bonuses */
	if (o_ptr->to_h + o_ptr->to_d > 0) return SENSE_GOOD_LIGHT;

	/* No feeling */
	return (SENSE_NONE);
}

static byte value_check_aux2_magic(object_type *o_ptr)
{
	/* Potions and Scrolls */
	if ((o_ptr->tval == TV_SCROLL) || (o_ptr->tval == TV_POTION) ||
            (o_ptr->tval == TV_POTION2) || (o_ptr->tval == TV_WAND) ||
            (o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_ROD) ||
            (o_ptr->tval == TV_ROD_MAIN))
	{
		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		/* "Cursed" scrolls/potions have a cost of 0 */
                if (k_ptr->cost == 0) return SENSE_CURSED;

                /* Artifacts */
                if (artifact_p(o_ptr)) return SENSE_GOOD_LIGHT;

		/* Scroll of Nothing, Apple Juice, etc. */
                if (k_ptr->cost < 3) return SENSE_AVERAGE;

		/* Identify, Phase Door, Cure Light Wounds, etc. are just
		 * average*/
		if (k_ptr->cost < 100) return SENSE_AVERAGE;

		/* Enchant Armor, *Identify*, Restore Stat, etc. */
                if (k_ptr->cost < 10000) return SENSE_GOOD_LIGHT;

		/* Acquirement, Deincarnation, Strength, Blood of Life, ... */
                if (k_ptr->cost >= 10000) return SENSE_GOOD_LIGHT;
	}

	/* Food */
	if (o_ptr->tval == TV_FOOD)
	{
		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		/* "Cursed" food */
                if (k_ptr->cost == 0) return SENSE_CURSED;

                /* Artifacts */
                if (artifact_p(o_ptr)) return SENSE_GOOD_LIGHT;

		/* Normal food (no magical properties) */
		if (k_ptr->cost <= 10) return SENSE_AVERAGE;

		/* Everything else is good */
                if (k_ptr->cost > 10) return SENSE_GOOD_LIGHT;
	}

        return SENSE_NONE;
}

/*
 * Can a player be resurrected?
 */
static bool granted_resurrection(void)
{
        if (p_ptr->pgod && p_ptr->grace > 300000)
        {
                if (magik(70)) return TRUE;
                else return FALSE;
        }
        else
        {
                return FALSE;
        }
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
	int		i;

	int		plev = p_ptr->lev;

        bool    heavy = FALSE, heavy_magic = FALSE;

        byte    feel;

	object_type *o_ptr;

	char o_name[80];


	/*** Check for "sensing" ***/

	/* No sensing when confused */
	if (p_ptr->confused) return;

	/* Analyze the class */
	switch (p_ptr->pclass)
	{
		case CLASS_MERCHANT:
		{
			/* Good sensing */
                        if (0 != rand_int(8000L / (plev * plev + 40))) return;

			/* Heavy sensing */
			heavy = TRUE;

			/* Done */
			break;
		}

		case CLASS_WARRIOR:
                case CLASS_WEAPONMASTER:
                case CLASS_UNBELIEVER:
                case CLASS_POSSESSOR:
                case CLASS_MIMIC:
                case CLASS_BEASTMASTER:
                case CLASS_POWERMAGE:
                case CLASS_ARCHER:
		{
			/* Good sensing */
			if (0 != rand_int(9000L / (plev * plev + 40))) return;

			/* Heavy sensing */
			heavy = TRUE;

			/* Done */
			break;
		}

                case CLASS_MAGE:
                case CLASS_RUNECRAFTER:
                case CLASS_HIGH_MAGE:
                case CLASS_SORCERER:
                case CLASS_ALCHEMIST:
		{
			/* Very bad (light) sensing */
			if (0 != rand_int(240000L / (plev + 5))) return;

                        heavy_magic = TRUE;

			/* Done */
			break;
		}

                case CLASS_HARPER:
		case CLASS_PRIEST:
		{
			/* Good (light) sensing */
			if (0 != rand_int(10000L / (plev * plev + 40))) return;

                        heavy_magic = TRUE;

			/* Done */
			break;
		}

		case CLASS_ROGUE:
                case CLASS_SYMBIANT:
                case CLASS_NECRO:
		{
			/* Okay sensing */
			if (0 != rand_int(20000L / (plev * plev + 40))) return;

			/* Heavy sensing */
			heavy = TRUE;
                        heavy_magic = TRUE;

			/* Done */
			break;
		}

		case CLASS_RANGER:
		{

			/* Bad sensing */
			if (0 != rand_int(95000L / (plev * plev + 40))) return;

			/* Changed! */
			heavy = TRUE;

			/* Done */
			break;
		}

		case CLASS_PALADIN:
		{
			/* Bad sensing */
			if (0 != rand_int(77777L / (plev * plev + 40))) return;

			/* Heavy sensing */
			heavy = TRUE;

			/* Done */
			break;
		}

                case CLASS_DRUID:
		case CLASS_WARLOCK:
		{

			/* Bad sensing */
			if (0 != rand_int(75000L / (plev * plev + 40))) return;

                        heavy_magic = TRUE;

			/* Done */
			break;
		}

		case CLASS_MINDCRAFTER:
		{

			/* Bad sensing */
			if (0 != rand_int(55000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case CLASS_CHAOS_WARRIOR:
		case CLASS_DAEMONOLOGIST:
		{

			/* Bad sensing */
			if (0 != rand_int(80000L / (plev * plev + 40))) return;

			/* Changed! */
			heavy = TRUE;

			/* Done */
			break;
		}

		case CLASS_MONK:
		{
			/* Okay sensing */
			if (0 != rand_int(20000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case CLASS_ILLUSIONIST: /* Added -KMW- */
		{
			/* Very bad (light) sensing */
			if (0 != rand_int(240000L / (plev + 5))) return;

                        heavy_magic = TRUE;

			/* Done */
			break;
		}
	}


	/*** Sense everything ***/

	/* Check everything */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
                byte okay = 0;

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
                        case TV_MSTAFF:
                        case TV_AXE:
			case TV_BOOTS:
			case TV_GLOVES:
			case TV_HELM:
			case TV_CROWN:
			case TV_SHIELD:
			case TV_CLOAK:
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
                        case TV_BOOMERANG:
                        case TV_TRAPKIT:
			{
                                okay = 1;
				break;
			}

                        case TV_POTION:
                        case TV_POTION2:
                        case TV_SCROLL:
                        case TV_WAND:
                        case TV_STAFF:
                        case TV_ROD:
                        case TV_ROD_MAIN:
                        case TV_FOOD:
			{
                                okay = 2;
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
		if ((i < INVEN_WIELD) && (0 != rand_int(5))) continue;

		/* Check for a feeling */
                if (okay == 1)
                {
                        feel = (heavy ? value_check_aux1(o_ptr) : value_check_aux2(o_ptr));
                }
                else
                {
                        feel = (heavy_magic ? value_check_aux1_magic(o_ptr) : value_check_aux2_magic(o_ptr));
                }

		/* Skip non-feelings */
                if (feel == SENSE_NONE) continue;

		/* Stop everything */
		if (disturb_minor) disturb(0, 0);

		/* Get an object description */
		object_desc(o_name, o_ptr, FALSE, 0);

		/* Message (equipment) */
		if (i >= INVEN_WIELD)
		{
			msg_format("You feel the %s (%c) you are %s %s %s...",
			           o_name, index_to_label(i), describe_use(i),
			           ((o_ptr->number == 1) ? "is" : "are"), sense_desc[feel]);
		}

		/* Message (inventory) */
		else
		{
			msg_format("You feel the %s (%c) in your pack %s %s...",
			           o_name, index_to_label(i),
			           ((o_ptr->number == 1) ? "is" : "are"), sense_desc[feel]);
		}

		/* We have "felt" it */
		o_ptr->ident |= (IDENT_SENSE);

		/* Set sense property */
		o_ptr->sense = feel;

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
	/* Ask for level */
	if (get_check("Teleport level? "))
	{
		char	ppp[80];

		char	tmp_val[160];

		/* Prompt */
		sprintf(ppp, "Teleport to level (0-%d): ", 99);

		/* Default */
		sprintf(tmp_val, "%d", dun_level);

		/* Ask for a level */
		if (!get_string(ppp, tmp_val, 10)) return;

		/* Extract request */
		command_arg = atoi(tmp_val);
	}
	else if (get_check("Normal teleport? "))
	{
		teleport_player(200);
	        return;
	}
	else
	{
		return;
	}

	/* Paranoia */
	if (command_arg < 0) command_arg = 0;

	/* Paranoia */
	if (command_arg > 99) command_arg = 99;

	/* Accept request */
	msg_format("You teleport to dungeon level %d.", command_arg);

	if (autosave_l)
	{
		is_autosave = TRUE;
		msg_print("Autosaving the game...");
		do_cmd_save_game();
		is_autosave = FALSE;
	}

	/* Change level */
	dun_level = command_arg;

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/* Returns TRUE if we are on the Straight Road... */
static bool pattern_effect(void)
{
	if ((cave[py][px].feat < FEAT_PATTERN_START)
	    || (cave[py][px].feat > FEAT_PATTERN_XTRA2))
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
                msg_print("This section of the Straight Road looks less powerful.");
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
		if (!(p_ptr->invuln))
                take_hit(200, "walking the corrupted Straight Road");
	}

	else
	{
                if (!(p_ptr->invuln))
                        take_hit(damroll(1,3), "walking the Straight Road");
	}

	return TRUE;
}

/*
 * If player has inscribed the object with "!!", let him know when it's
 * recharged. -LM-
 */
static void recharged_notice(object_type *o_ptr)
{
	char o_name[80];

	cptr s;

	/* No inscription */
	if (!o_ptr->note) return;

	/* Find a '!' */
	s = strchr(quark_str(o_ptr->note), '!');

	/* Process notification request. */
	while (s)
	{
		/* Find another '!' */
		if (s[1] == '!')
		{
			/* Describe (briefly) */
			object_desc(o_name, o_ptr, FALSE, 0);

			/* Notify the player */
			if (o_ptr->number > 1)
				msg_format("Your %s are recharged.", o_name);
			else msg_format("Your %s is recharged.", o_name);

			/* Done. */
			return;
		}

		/* Keep looking for '!'s */
		s = strchr(s + 1, '!');
	}
}



/*
 * Regenerate hit points				-RAK-
 */
static void regenhp(int percent)
{
	s32b        new_chp, new_chp_frac;
	int                   old_chp;

        /* Only if alive */
        if(!(p_ptr->class_extra3 & CLASS_UNDEAD))
        {
                /* Save the old hitpoints */
                old_chp = p_ptr->chp;

                /* Extract the new hitpoints */
                new_chp = ((long)p_ptr->mhp) * percent + PY_REGEN_HPBASE;
                p_ptr->chp += new_chp >> 16;   /* div 65536 */

                /* check for overflow */
                if ((p_ptr->chp < 0) && (old_chp > 0)) p_ptr->chp = MAX_SHORT;
                new_chp_frac = (new_chp & 0xFFFF) + p_ptr->chp_frac;    /* mod 65536 */
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
                }
        }
}


/*
 * Regenerate mana points				-RAK-
 */
static void regenmana(int percent)
{
	s32b        new_mana, new_mana_frac;
	int                   old_csp;

	old_csp = p_ptr->csp;
	new_mana = ((long)p_ptr->msp) * percent + PY_REGEN_MNBASE;
	p_ptr->csp += new_mana >> 16;	/* div 65536 */
	/* check for overflow */
	if ((p_ptr->csp < 0) && (old_csp > 0))
	{
		p_ptr->csp = MAX_SHORT;
	}
	new_mana_frac = (new_mana & 0xFFFF) + p_ptr->csp_frac;	/* mod 65536 */
	if (new_mana_frac >= 0x10000L)
	{
		p_ptr->csp_frac = new_mana_frac - 0x10000L;
		p_ptr->csp++;
	}
	else
	{
		p_ptr->csp_frac = new_mana_frac;
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
        object_type *o_ptr = &inventory[INVEN_CARRY];

        if(o_ptr->k_idx)
        {
                monster_race *r_ptr = &r_info[o_ptr->pval];
                int max = maxroll(r_ptr->hdice, r_ptr->hside);

		/* Allow regeneration (if needed) */
                if (o_ptr->pval2 < max)
		{
			/* Hack -- Base regeneration */
                        frac = max / 100;

			/* Hack -- Minimal regeneration rate */
			if (!frac) frac = 1;

			/* Hack -- Some monsters regenerate quickly */
			if (r_ptr->flags2 & (RF2_REGENERATE)) frac *= 2;


			/* Hack -- Regenerate */
                        o_ptr->pval2 += frac;

			/* Do not over-regenerate */
                        if (o_ptr->pval2 > max) o_ptr->pval2 = max;

                        /* Redraw (later) */
                        p_ptr->redraw |= (PR_MH);
		}
        }

	/* Regenerate everyone */
	for (i = 1; i < m_max; i++)
	{
		/* Check the i'th monster */
		monster_type *m_ptr = &m_list[i];
                monster_race *r_ptr = race_inf(m_ptr);

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

                /* Dont regen bleeding/poisonned monsters */
                if (m_ptr->bleeding || m_ptr->poisoned) continue;

		/* Allow regeneration (if needed) */
		if (m_ptr->hp < m_ptr->maxhp)
		{
			/* Hack -- Base regeneration */
			frac = m_ptr->maxhp / 100;

			/* Hack -- Minimal regeneration rate */
			if (!frac) frac = 1;

			/* Hack -- Some monsters regenerate quickly */
			if (r_ptr->flags2 & (RF2_REGENERATE)) frac *= 2;


			/* Hack -- Regenerate */
			m_ptr->hp += frac;

			/* Do not over-regenerate */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (health_who == i) p_ptr->redraw |= (PR_HEALTH);
		}
	}
}


/*
 * Forcibly pseudo-identify an object in the inventory
 * (or on the floor)
 */
bool psychometry(void)
{
	int                     item;

	object_type             *o_ptr;

	char            o_name[80];
	byte            feel;

	cptr q, s;


	/* Get an item */
	q = "Meditate on which item? ";
	s = "You have nothing appropriate.";
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
	if ((object_known_p(o_ptr)) || (o_ptr->ident & IDENT_SENSE))
	{
		msg_print("You cannot find out anything more about that.");
		return TRUE;
	}

	/* Check for a feeling */
        feel = value_check_aux1_magic(o_ptr);
        if (feel == SENSE_NONE) feel = value_check_aux1(o_ptr);

	/* Get an object description */
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Skip non-feelings */
	if (!feel)
	{
		msg_format("You do not perceive anything unusual about the %s.", o_name);
		return TRUE;
	}

	msg_format("You feel that the %s %s %s...",
	    o_name, ((o_ptr->number == 1) ? "is" : "are"), sense_desc[feel]);

	/* We have "felt" it */
	o_ptr->ident |= (IDENT_SENSE);

	/* Set sense property */
	o_ptr->sense = feel;

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	/* Something happened */
	return (TRUE);
}

/*
 * Does an object decay?
 */
bool decays(object_type *o_ptr)
{
        u32b f1, f2, f3, f4, f5, esp;

	/* Extract some flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

	if (f3 & TR3_DECAY) return TRUE;

	return FALSE;
}

static void gere_music(s16b music)
{
        switch(music)
        {
                case MUSIC_SLOW:
                        slow_monsters();
                        break;
                case MUSIC_STUN:
                        stun_monsters(damroll(p_ptr->lev/10,2));
                        break;
                case MUSIC_LIFE:
                        hp_player(damroll(2,8));
                        break;
                case MUSIC_MIND:
                        set_tim_esp(2);
                        break;
                case MUSIC_LITE:
                        set_lite(2);
                        break;
                case MUSIC_FURY:
                        set_hero(2);
                        break;
                case MUSIC_AWARE:
                        detect_objects_magic();
                        break;
                case MUSIC_ID:
                        fire_explosion(py, px, GF_IDENTIFY, 1, 1);
                        break;
                case MUSIC_ILLUSION:
                        conf_monsters();
                        break;
                case MUSIC_WALL:
                        fire_explosion(py, px, GF_KILL_WALL, 1, 1);
                        break;
                case MUSIC_RESIST:
                        (void)set_oppose_acid(2);
                        (void)set_oppose_elec(2);
                        (void)set_oppose_fire(2);
                        (void)set_oppose_cold(2);
                        (void)set_oppose_pois(2);
                        break;
                case MUSIC_TIME:
                        set_fast(2);
                        break;
                case MUSIC_BETWEEN:
                        teleport_player(20);
                        break;
                case MUSIC_CHARME:
                        project_hack(GF_CHARM, damroll(10 + p_ptr->lev/15,6));
                        break;
                case MUSIC_VIBRA:
                        project_hack(GF_SOUND, damroll(10 + p_ptr->lev/15,6));
                        break;
                case MUSIC_HOLY:
                        set_mimic(2,MIMIC_VALAR);
                        break;
                case MUSIC_HIDE:
                        set_invis(2, 40);
                        break;
                case MUSIC_LIBERTY:
                        set_invuln(2);
                        break;
                case MUSIC_BEAUTY:
                        project_hack(GF_DISP_EVIL, damroll(p_ptr->lev/3, 3));
                        break;
                case MUSIC_SHADOW:
                        set_shadow(2);
                        break;
                case MUSIC_STAR_ID:
                        fire_explosion(py, px, GF_STAR_IDENTIFY, 1, 1);
                        break;
        }
}

static void gere_class_special()
{
        switch(p_ptr->class_extra7)
        {
                /* Lay a path of mana on the floor */
                case CLASS_MANA_PATH:
                        /* Does the player have enought mana ? */
                        if(p_ptr->csp < (p_ptr->class_extra6 & 255))
                        {
                                p_ptr->class_extra6 = 0;
                                p_ptr->class_extra7 = CLASS_NONE;
                                msg_print("You stop laying a mana path.");
                                p_ptr->update |= PU_BONUS;
                        }
                        else
                        {
                                /* Use some mana */
                                p_ptr->csp -= (p_ptr->class_extra6 & 255);

                                if((p_ptr->class_extra6 >> 8) & CLASS_MANA_PATH_ERASE)
                                {
                                        /* Absorb some of the mana of the grid */
                                        p_ptr->csp += cave[py][px].mana / 50;
                                        if(p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;

                                        /* Set the new grid mana */
                                        cave[py][px].mana = p_ptr->class_extra6 & 255;
                                }
                                else
                                {
                                        int m = cave[py][px].mana;

                                        if(m + (p_ptr->class_extra6 & 255) > 255)
                                        {
                                                cave[py][px].mana = 255;
                                        }
                                        else
                                        {
                                                cave[py][px].mana += p_ptr->class_extra6 & 255;
                                        }
                                }

                                /* Redraw mana */
                                p_ptr->redraw |= (PR_MANA);

                                /* Window stuff */
                                p_ptr->window |= (PW_PLAYER);
                        }
                        break;

                /* Lay a path of mana on the floor */
                case CLASS_WINDS_MANA:
                        /* Does the player have enought mana ? */
                        if(p_ptr->csp < (p_ptr->class_extra6 & 255))
                        {
                                p_ptr->class_extra7 = CLASS_NONE;
                                msg_print("You stop expulsing mana winds.");
                        }
                        else
                        {
                                int dam = 0;

                                /* Use some mana */
                                p_ptr->csp -= (p_ptr->class_extra6 & 255);

                                if((p_ptr->class_extra6 >> 8) & CLASS_MANA_PATH_ERASE)
                                {
                                        dam = (p_ptr->class_extra6 & 255) + 256;
                                }
                                else
                                {
                                        dam = (p_ptr->class_extra6 & 255);
                                }

                                fire_explosion(py, px, GF_WINDS_MANA, 2, dam);

                                /* Redraw mana */
                                p_ptr->redraw |= (PR_MANA);

                                /* Window stuff */
                                p_ptr->window |= (PW_PLAYER);
                        }
                        break;

                case CLASS_CANALIZE_MANA:
                        if(p_ptr->class_extra6 & CLASS_CANALIZE_MANA_EXTRA)
                                p_ptr->csp += cave[py][px].mana / 10;
                        else
                                p_ptr->csp += cave[py][px].mana / 20;

                        if(p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;

                        cave[py][px].mana = 0;

                        /* Redraw mana */
                        p_ptr->redraw |= (PR_MANA);

                        /* Window stuff */
                        p_ptr->window |= (PW_PLAYER);
                        break;
        }
}

static void check_music()
{
        magic_type *s_ptr;
        object_type *o_ptr = &inventory[INVEN_BOW];

        /* Music of the instrument, if any */
        if(p_ptr->music<255)
        {
        if(o_ptr->tval == TV_INSTRUMENT)
        {
                if(o_ptr->timeout < music_info[p_ptr->music].init_recharge +
                                    music_info[p_ptr->music].dur * music_info[p_ptr->music].turn_recharge)
                {
                        o_ptr->timeout += music_info[p_ptr->music].turn_recharge;
                        gere_music(music_info[p_ptr->music].music);
                }else
                {
                        msg_print("Your instrument stops singing.");
                        p_ptr->music = 255;
                }
        }
        else
        {
                msg_print("Your instrument stop singing because you don't wield it anymore.");
                p_ptr->music = 255;
        }
        }

        /* Music sung by player */
        if(p_ptr->pclass != CLASS_HARPER) return;
        if(!p_ptr->class_extra1) return;

        s_ptr = &realm_info[REALM_MUSIC][p_ptr->class_extra2];

        if(p_ptr->csp < s_ptr->smana)
        {
                msg_print("You stop singing.");
                p_ptr->class_extra1 = MUSIC_NONE;
        }
        else
        {
                p_ptr->csp -= s_ptr->smana;

                p_ptr->redraw |= PR_MANA;
        }

        gere_music(p_ptr->class_extra1);
}


/* Generate the feature effect */
void apply_effect(int y, int x)
{
        cave_type *c_ptr = &cave[y][x];
        feature_type *f_ptr = &f_info[c_ptr->feat];

        if (f_ptr->d_frequency[0] != 0)
        {
                int i;

                for (i = 0; i < 4; i++)
                {
                        /* Check the frequency */
                        if (f_ptr->d_frequency[i] == 0) continue;

                        if (((turn % f_ptr->d_frequency[i]) == 0) &&
                            ((f_ptr->d_side[i] != 0) || (f_ptr->d_dice[i] != 0)))
                        {
                                int l, dam = 0;
                                int d = f_ptr->d_dice[i], s = f_ptr->d_side[i];

                                if (d == -1) d = p_ptr->lev;
                                if (s == -1) s = p_ptr->lev;

                                /* Roll damage */
                                for (l = 0; l < d; l++)
                                {
                                        dam += randint(s);
                                }

                                /* Apply damage */
                                project(-100, 0, y, x, dam, f_ptr->d_type[i], PROJECT_KILL | PROJECT_HIDE);
                        }
                }
        }
        
}

bool is_recall = FALSE;

/*
 * Handle certain things once every 10 game turns
 */
static void process_world(void)
{
        int x, y, i, j;

	int regen_amount;
	bool cave_no_regen = FALSE;
	int upkeep_factor = 0;

        dungeon_info_type *d_ptr = &d_info[dungeon_type];

	cave_type *c_ptr;

	object_type *o_ptr;
        object_kind *k_ptr;
        u32b f1 = 0 , f2 = 0 , f3 = 0, f4 = 0, f5 = 0, esp = 0;


	/* Every 10 game turns */
	if (turn % 10) return;

        /* Handle the player song */
        check_music();

        /* Handle class special actions */
        gere_class_special();

        /* Check the fate */
        if(fate_option && (randint(50000)==666) && (p_ptr->lev > 10)) gain_fate(0);

        /*** Is the wielded monsters still hypnotised ***/
        o_ptr = &inventory[INVEN_CARRY];
        if(o_ptr->k_idx)
        {
                if((randint(1000)<60-p_ptr->lev) && (p_ptr->pclass!=CLASS_SYMBIANT))
                {
                        msg_print("Your monster thinks you are not enough in symbiosis.");
                        carried_make_attack_normal(o_ptr->pval);
                }
        }

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
				alive = FALSE;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}
	}

	/*** Attempt timed autosave ***/
	if (autosave_t && autosave_freq)
	{
		if (!(turn % ((s32b) autosave_freq * 10 )))
		{
			is_autosave = TRUE;
			msg_print("Autosaving the game...");
			do_cmd_save_game();
			is_autosave = FALSE;
		}
	}


	/*** Handle the wilderness/town (sunshine) ***/

	/* While in town/wilderness */
	if (!dun_level)
	{
		/* Hack -- Daybreak/Nighfall in town */
                if (!(turn % ((10L * DAY) / 2)))
		{
			bool dawn;

			/* Check for dawn */
                        dawn = (!(turn % (10L * DAY)));

			/* Day breaks */
                        if (dawn && (!p_ptr->wild_mode))
			{
				/* Message */
				msg_print("The sun has risen.");

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
                        else if (!p_ptr->wild_mode)
			{
				/* Message */
				msg_print("The sun has fallen.");

				/* Hack -- Scan the town */
				for (y = 0; y < cur_hgt; y++)
				{
					for (x = 0; x < cur_wid; x++)
					{
						/* Get the cave grid */
						c_ptr = &cave[y][x];

						/* Darken "boring" features */
                                                if ((f_info[c_ptr->feat].flags1 & FF1_FLOOR) && !(f_info[c_ptr->feat].flags1 & FF1_REMEMBER))
						{
							/* Forget the grid */
							c_ptr->info &= ~(CAVE_GLOW | CAVE_MARK);

							/* Hack -- Notice spot */
							note_spot(y, x);
						}
					}
				}
			}

			/* Update the monsters */
			p_ptr->update |= (PU_MONSTERS);

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD);
		}
	}

        /* Tell a day passed */
        if (!((turn + (DAY_START * 10L)) % (10L * DAY)))
        {
                char buf[20];

                sprintf(buf, get_day(bst(YEAR, turn) + START_YEAR));
                cmsg_format(TERM_L_GREEN, "Today it is %s of the %s year of the third age.", get_month_name(bst(DAY, turn), wizard, FALSE), buf);
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
		if (cheat_xtra) msg_print("Rewards reset.");

                /* Select new bounties. */
                if (magik(20)) select_bounties();
	}

	/* Modify loan */
	if (p_ptr->loan)
	{
		if (p_ptr->loan_time) p_ptr->loan_time--;
	}

        if ((!(turn % 5000)) && p_ptr->loan && (!p_ptr->loan_time))
        {
                cmsg_print(TERM_RED, "You should pay your loan...");

                p_ptr->loan += p_ptr->loan / 12;
                if (p_ptr->loan > PY_MAX_GOLD) p_ptr->loan = PY_MAX_GOLD;

                /* Do a nasty stuff */
                if (rand_int(2) || (p_ptr->wild_mode))
                {
                        /* Discount player items */
                        int z, tries = 200;
                        object_type *o_ptr;

                        while (tries--)
                        {
                                z = rand_int(INVEN_TOTAL);
                                o_ptr = &inventory[z];

                                if (!o_ptr->k_idx) continue;

                                if (o_ptr->discount >= 100) continue;

                                break;
                        }
                        if (tries)
                        {
                                o_ptr->discount += 70;
                                if (o_ptr->discount >= 100) o_ptr->discount = 100;

                                inven_item_optimize(z);
                                inven_item_describe(z);
                                p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
                        }
                }
                else
                {
                        int merc = test_monster_name("Mean-looking mercenary"), agent = test_monster_name("Agent of the black market");
                        int num = 5 + (p_ptr->lev / 3), z;

                        for (z = 0; z < num; z++)
                        {
                                int yy, xx, attempts = 200, m_idx;

                                /* Summon */
                                do
                                {
                                        scatter(&yy, &xx, py, px, 6, 0);
                                }
                                while (!(in_bounds(yy, xx) && cave_floor_bold(yy, xx)) && --attempts);

                                m_idx = place_monster_one(yy, xx, (magik(80))?merc:agent, 0, FALSE, MSTATUS_ENEMY);

                                /* Level it */
                                if (m_idx)
                                {
                                        monster_type *m_ptr = &m_list[m_idx];

                                        m_ptr->exp = MONSTER_EXP(p_ptr->lev * 2);
                                        monster_check_experience(m_idx, TRUE);
                                }
                        }
                }
        }

	/*** Process the monsters ***/

	/* Check for creature generation. */
        if ((rand_int(d_info[dungeon_type].max_m_alloc_chance) == 0) && !(p_ptr->inside_arena) && !(p_ptr->inside_quest))
	{
		/* Make a new monster */
                if (!(dungeon_flags1 & LF1_NO_NEW_MONSTER)) (void)alloc_monster(MAX_SIGHT + 5, FALSE);
	}

	/* Hack -- Check for creature regeneration */
	if (!(turn % 100)) regen_monsters();


	/*** Damage over Time ***/

	/* Take damage from poison */
	if ((p_ptr->poisoned) && !(p_ptr->invuln))
	{
		/* Take damage */
		take_hit(1, "poison");
	}


	/* (Vampires) Take damage from sunlight */
        if ((p_ptr->pracem == RMOD_VAMPIRE)||(p_ptr->mimic_form == MIMIC_VAMPIRE))
	{
		if ((!dun_level)
		    && (!(p_ptr->resist_lite)) && !(p_ptr->invuln)
                    && (!((turn / ((10L * DAY)/2)) % 2)))
		{
			if (cave[py][px].info & (CAVE_GLOW))
			{
				/* Take damage */
				msg_print("The sun's rays scorch your undead flesh!");
				take_hit(1, "sunlight");
				cave_no_regen = TRUE;
			}
		}

		if ((inventory[INVEN_LITE].tval)
		    && (inventory[INVEN_LITE].sval >= SV_LITE_GALADRIEL)
                    && (inventory[INVEN_LITE].sval <= SV_STONE_LORE)
                    && (inventory[INVEN_LITE].sval != SV_LITE_UNDEATH)
		    && !(p_ptr->resist_lite))
		{
			object_type * o_ptr = &inventory[INVEN_LITE];
			char o_name [80];
			char ouch [80];

			/* Get an object description */
			object_desc(o_name, o_ptr, FALSE, 0);

			msg_format("The %s scorches your undead flesh!", o_name);

			cave_no_regen = TRUE;

			/* Get an object description */
			object_desc(o_name, o_ptr, TRUE, 0);

			sprintf(ouch, "wielding %s", o_name);
			if (!(p_ptr->invuln)) take_hit(1, ouch);
		}
	}

        /* Drown in deep water unless the player have levitation or water walking */
        else if ((cave[py][px].feat == FEAT_DEEP_WATER) && !p_ptr->ffall && !p_ptr->    walk_water)
	{
		if (calc_total_weight() > ((adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100) / 2))
		{
			/* Take damage */
			msg_print("You are drowning!");
			take_hit(randint(p_ptr->lev), "drowning");
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
                if ((cave[py][px].feat == FEAT_TREES) &&
                    ((p_ptr->pclass==CLASS_DRUID) || (p_ptr->pclass==CLASS_RANGER) || (p_ptr->prace==RACE_ENT)))
		{
			/* Do nothing */
		}
                else if (!player_can_enter(cave[py][px].feat))
		{
			cptr dam_desc;

			cave_no_regen = TRUE;

                        if (p_ptr->pracem == RMOD_SPECTRE)
			{
				msg_print("Your molecules feel disrupted!");
				dam_desc = "density";
			}
			else
			{
				msg_print("You are being crushed!");
				dam_desc = "solid rock";
			}

                        if (((p_ptr->chp > (p_ptr->lev / 5)) && (p_ptr->pracem == RMOD_SPECTRE)) || (p_ptr->pracem != RMOD_SPECTRE))
                        {
                                take_hit(1 + ((p_ptr->lev)/5), dam_desc);
                        }
                }
	}


	/* Take damage from cuts */
	if ((p_ptr->cut) && !(p_ptr->invuln))
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


	/*** Check the Food, and Regenerate ***/

	/* Digest normally */
	if (p_ptr->food < PY_FOOD_MAX)
	{
		/* Every 100 game turns */
		if (!(turn % 100))
		{
			/* Basic digestion rate based on speed */
                        i = extract_energy[(p_ptr->pspeed > 199)?199:(p_ptr->pspeed < 0)?0:p_ptr->pspeed] * 2;

			/* Regeneration takes more food */
			if (p_ptr->regenerate) i += 30;

                        /* DragonRider takes more food */
                        if (p_ptr->prace==RACE_DRAGONRIDER) i += 10;

                        /* Invisibility consume a lot of food */
                        i += p_ptr->invis / 2;

                        /* Invulnerability consume a lot of food */
                        if (p_ptr->invuln) i += 40;

                        /* Wraith Form consume a lot of food */
                        if (p_ptr->wraith_form) i += 30;

                        /* Get the weapon */
                        o_ptr = &inventory[INVEN_WIELD];

                        /* Examine the sword */
                        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

                        /* Hitpoints multiplier consume a lot of food */
                        if (o_ptr->k_idx && (f2 & (TR2_LIFE))) i += o_ptr->pval * 5;

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
		if (!(p_ptr->invuln)) take_hit(i, "starvation");
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
			if (!p_ptr->paralyzed && (rand_int(100) < 10))
			{
				/* Message */
				msg_print("You faint from the lack of food.");
				disturb(1, 0);

				/* Hack -- faint (bypass free action) */
				(void)set_paralyzed(p_ptr->paralyzed + 1 + rand_int(5));
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

	if (total_friends)
	{
		int upkeep_divider = 20;

		if (p_ptr->pclass == CLASS_MAGE)
			upkeep_divider = 15;
		else if (p_ptr->pclass == CLASS_HIGH_MAGE)
			upkeep_divider = 12;

#ifdef TRACK_FRIENDS
		if (wizard)
			msg_format("Total friends: %d.", total_friends);
#endif /* TRACK_FRIENDS */

		if (total_friends > 1 + (p_ptr->lev / (upkeep_divider)))
		{
			upkeep_factor = (total_friend_levels);

			if (upkeep_factor > 100) upkeep_factor = 100;
			else if (upkeep_factor < 10) upkeep_factor = 10;

#ifdef TRACK_FRIENDS
			if (wizard)
			msg_format("Levels %d, upkeep %d", total_friend_levels, upkeep_factor);
#endif /* TRACK_FRIENDS */

		}
	}

	/* Regenerate the mana */
	if (p_ptr->csp < p_ptr->msp)
	{
		if (upkeep_factor)
		{
			s16b upkeep_regen = (((100 - upkeep_factor) * regen_amount) / 100);
			regenmana(upkeep_regen);

#ifdef TRACK_FRIENDS
			if (wizard)
			{
				msg_format("Regen: %d/%d", upkeep_regen, regen_amount);
			}
#endif /* TRACK_FRIENDS */

		}
		else
		{
			regenmana(regen_amount);
		}
	}

        if (p_ptr->pgod > 0)
        {
                if ((turn % 50) == 0)
                {
                        set_grace(p_ptr->grace - (deity_info[p_ptr->pgod-1].grace_deduction * 3 + 1));
                }

                if (p_ptr->god_favor > -100000)
                {
                        int mns = 10 - deity_info[p_ptr->pgod-1].grace_deduction;

                        mns = (mns / 4) + 1;

                        if (p_ptr->pclass == CLASS_PRIEST) mns *= 2;
                        else if (p_ptr->pclass == CLASS_PALADIN) mns = (mns * 3) / 2;

                        p_ptr->god_favor -= mns;
                }
        }


	/* Poisoned or cut yields no healing */
	if (p_ptr->poisoned) regen_amount = 0;
	if (p_ptr->cut) regen_amount = 0;

	/* Special floor -- Pattern, in a wall -- yields no healing */
	if (cave_no_regen) regen_amount = 0;

	/* Regenerate Hit Points if needed */
	if ((p_ptr->chp < p_ptr->mhp) && !(cave_no_regen))
	{
		if ((cave[py][px].feat < FEAT_PATTERN_END) &&
		    (cave[py][px].feat >= FEAT_PATTERN_START))
		{
			regenhp(regen_amount / 5); /* Hmmm. this should never happen? */
		}
		else
		{
			regenhp(regen_amount);
		}
	}


	/*** Timeout Various Things ***/

	/* Handle temporary stat drains */
	for (i = 0; i < 6; i++)
	{
		if (p_ptr->stat_cnt[i] > 0)
		{
			p_ptr->stat_cnt[i]--;
                        if (p_ptr->stat_cnt[i] == 0)
			{
                                do_res_stat(i);
			}
		}
	}

	/* Hack -- Hallucinating */
	if (p_ptr->image)
	{
		(void)set_image(p_ptr->image - 1);
	}

        /* Holy Aura */
        if (p_ptr->holy)
	{
                (void)set_holy(p_ptr->holy - 1);
	}

        /* Undead loose Death Points */
        if(p_ptr->class_extra3 & CLASS_UNDEAD)
        {
                int old_chp = p_ptr->chp;
                int warning = (p_ptr->mhp * hitpoint_warn / 10);

                /* Bypass invulnerability and wraithform */
                p_ptr->chp--;

                /* Display the hitpoints */
                p_ptr->redraw |= (PR_HP);

                /* Window stuff */
                p_ptr->window |= (PW_PLAYER);

                /* Dead player */
                if (p_ptr->chp < 0)
                {
                        /* Sound */
                        sound(SOUND_DEATH);

                        /* Hack -- Note death */
                        if (!last_words)
                        {
                                msg_print("You die.");
                                msg_print(NULL);
                        }
                        else
                        {
                                char death_message[80];

                                (void)get_rnd_line("death.txt", death_message);
                                msg_print(death_message);
                        }

                        /* Note cause of death */
                        (void)strcpy(died_from, "being undead too long");

                        if (p_ptr->image) strcat(died_from,"(?)");

                        /* No longer a winner */
                        total_winner = FALSE;

                        /* Leaving */
                        p_ptr->leaving = TRUE;

                        /* Note death */
                        death = TRUE;

                        if (get_check("Dump the screen? "))
                        {
                                do_cmd_save_screen();
                        }

                        /* Dead */
                        return;
                }

                /* Hitpoint warning */
                if (p_ptr->chp < warning)
                {
                        /* Hack -- bell on first notice */
                        if (alert_hitpoint && (old_chp > warning)) bell();

                        sound(SOUND_WARN);

                        /* Message */
                        msg_print("*** LOW DEATHPOINT WARNING! ***");
                        msg_print(NULL);
                }
        }

        /* Walk water */
        if (p_ptr->walk_water)
	{
                (void)set_walk_water(p_ptr->walk_water - 1);
	}

        /* True Strike */
        if (p_ptr->strike)
	{
                (void)set_strike(p_ptr->strike - 1);
	}

        /* Meditation */
        if (p_ptr->meditation)
	{
                (void)set_meditation(p_ptr->meditation - 1);
	}

        /* Timed Disrupt shield */
        if (p_ptr->disrupt_shield)
	{
                (void)set_disrupt_shield(p_ptr->disrupt_shield - 1);
	}

        /* Timed Parasite */
        if (p_ptr->parasite)
	{
                (void)set_parasite(p_ptr->parasite - 1, p_ptr->parasite_r_idx);
	}

        /* Timed Reflection */
        if (p_ptr->tim_reflect)
	{
                (void)set_tim_reflect(p_ptr->tim_reflect - 1);
	}

        /* Timed Prob Travel */
        if (p_ptr->prob_travel)
	{
                (void)set_prob_travel(p_ptr->prob_travel - 1);
	}

        /* Timed Time Resistance */
        if (p_ptr->tim_res_time)
	{
                (void)set_tim_res_time(p_ptr->tim_res_time - 1);
	}

        /* Timed Levitation */
        if (p_ptr->tim_ffall)
	{
                (void)set_tim_ffall(p_ptr->tim_ffall - 1);
	}

        /* Timed Fire Aura */
        if (p_ptr->tim_fire_aura)
	{
                (void)set_tim_fire_aura(p_ptr->tim_fire_aura - 1);
	}

        /* Brightness */
        if (p_ptr->tim_lite)
	{
                (void)set_lite(p_ptr->tim_lite - 1);
	}

	/* Blindness */
	if (p_ptr->blind)
	{
		(void)set_blind(p_ptr->blind - 1);
	}

        /* Timed no_breeds */
        if (no_breeds)
	{
                (void)set_no_breeders(no_breeds - 1);
        }

        /* Timed mimic */
        if (p_ptr->tim_mimic)
	{
                (void)set_mimic(p_ptr->tim_mimic - 1, p_ptr->mimic_form);
        }

        /* Timed special move commands */
        if (p_ptr->immov_cntr) {
	  p_ptr->immov_cntr--;
	}

	/* Timed invisibility */
        if (p_ptr->tim_invisible)
	{
                (void)set_invis(p_ptr->tim_invisible - 1, p_ptr->tim_inv_pow);
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

        /* Light speed */
        if (p_ptr->lightspeed)
	{
                (void)set_light_speed(p_ptr->lightspeed - 1);
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

	/* Protection from good */
	if (p_ptr->protgood)
	{
		(void)set_protgood(p_ptr->protgood - 1);
	}

        /* Protection from undead */
        if (p_ptr->protundead)
	{
                (void)set_protundead(p_ptr->protundead - 1);
	}

	/* Invulnerability */
	if (p_ptr->invuln)
	{
		(void)set_invuln(p_ptr->invuln - 1);
	}

	/* Wraith form */
        if (p_ptr->tim_wraith)
	{
                (void)set_shadow(p_ptr->tim_wraith - 1);
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
                (void)set_shield(p_ptr->shield - 1, p_ptr->shield_power, p_ptr->shield_opt);
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

        /* Oppose Light & Dark */
        if (p_ptr->oppose_ld)
	{
                (void)set_oppose_ld(p_ptr->oppose_ld - 1);
	}

        /* Oppose Chaos & Confusion */
        if (p_ptr->oppose_cc)
	{
                (void)set_oppose_cc(p_ptr->oppose_cc - 1);
	}

        /* Oppose Sound & Shards */
        if (p_ptr->oppose_ss)
	{
                (void)set_oppose_ss(p_ptr->oppose_ss - 1);
	}

        /* Oppose Nexus */
        if (p_ptr->oppose_nex)
	{
                (void)set_oppose_nex(p_ptr->oppose_nex - 1);
	}

        /* Mental Barrier */
        if (p_ptr->tim_mental_barrier)
	{
                (void)set_mental_barrier(p_ptr->tim_mental_barrier - 1);
	}


        /* Timed mimicry */
        if (p_ptr->pclass == CLASS_MIMIC)
	{
                /* Extract the value and the flags */
                u32b value = p_ptr->class_extra6 >> 16,
                     att = p_ptr->class_extra6 & 0xFFFF;

                if ((att & CLASS_LEGS) || (att & CLASS_WALL) || (att & CLASS_ARMS))
                {
                        value--;

                        if (!value)
                        {
                                if (att & CLASS_LEGS) msg_print("You lose your extra pair of legs.");
                                if (att & CLASS_ARMS) msg_print("You lose your extra pair of arms.");
                                if (att & CLASS_WALL) msg_print("You lose your affinity for walls.");

                                att &= ~CLASS_ARMS;
                                att &= ~CLASS_LEGS;
                                att &= ~CLASS_WALL;

                                if (disturb_state)
                                        disturb(0, 0);
                        }

                        p_ptr->update |= PU_BODY;
                        p_ptr->class_extra6 = att + (value << 16);
                }
        }


	/*** Poison and Stun and Cut ***/

	/* Poison */
	if (p_ptr->poisoned)
	{
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);

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

		/* Apply some healing */
		(void)set_cut(p_ptr->cut - adjust);
	}

        /* Hack - damage done by the dungeon -SC- */
        if ((dun_level != 0) && (d_ptr->d_frequency[0] != 0))
        {
                int i, j, k;

                /* Apply damage to every grid in the dungeon */
                for (i = 0; i < 4; i++)
                {
                        /* Check the frequency */
                        if (d_ptr->d_frequency[i] == 0) continue;

                        if (((turn % d_ptr->d_frequency[i]) == 0) &&
                            ((d_ptr->d_side[i] != 0) || (d_ptr->d_dice[i] != 0)))
                                for (j = 0; j < cur_hgt - 1; j++)
                                        for (k = 0; k < cur_wid - 1; k++)
                                        {
                                                int l, dam = 0;

                                                if (!(d_ptr->flags1 & DF1_DAMAGE_FEAT))
                                                /* If the grid is empty, skip it */
                                                        if ((cave[j][k].o_idx == 0) &&
                                                            ((j != py) && (i != px)))
                                                                continue;

                                                /* Let's not hurt poor monsters */
                                                if (cave[j][k].m_idx) continue;

                                                /* Roll damage */
                                                for (l = 0; l < d_ptr->d_dice[i]; l++)
                                                        dam += randint(d_ptr->d_side[i]);

                                                /* Apply damage */
                                                project(-100, 0, j, k, dam, d_ptr->d_type[i], PROJECT_KILL | PROJECT_ITEM | PROJECT_HIDE);
                                        }
                }
        }

        apply_effect(py, px);

        /* Every 1500 turns, warn about any Black Breath not gotten from an equipped
         * object, and stop any resting. -LM-
         */
        if (!(turn % 3000) && (p_ptr->black_breath))
        {
                u32b f1, f2, f3, f4, f5;

                bool be_silent = FALSE;

                /* check all equipment for the Black Breath flag. */
                for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
                {
                        o_ptr = &inventory[i];

                        /* Skip non-objects */
                        if (!o_ptr->k_idx) continue;

                        /* Extract the item flags */
                        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

                        /* No messages if object has the flag, to avoid annoyance. */
                        if (f4 & (TR4_BLACK_BREATH)) be_silent = TRUE;

                }
                /* If we are allowed to speak, warn and disturb. */

                if (!be_silent)
                {
                        cmsg_print(TERM_L_DARK, "The Black Breath saps your soul!");
                        disturb(0, 0);
                }
        }


	/*** Process Light ***/

	/* Check for light being wielded */
	o_ptr = &inventory[INVEN_LITE];

	/* Burn some fuel in the current lite */
	if (o_ptr->tval == TV_LITE)
	{
                /* Extract the item flags */
                object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

                /* Hack -- Use some fuel */
                if ((f4 & TR4_FUEL_LITE) && (o_ptr->timeout > 0))
		{
			/* Decrease life-span */
                        o_ptr->timeout--;

			/* Hack -- notice interesting fuel steps */
                        if ((o_ptr->timeout < 100) || (!(o_ptr->timeout % 100)))
			{
				/* Window stuff */
				p_ptr->window |= (PW_EQUIP);
			}

			/* Hack -- Special treatment when blind */
			if (p_ptr->blind)
			{
				/* Hack -- save some light for later */
                                if (o_ptr->timeout == 0) o_ptr->timeout++;
			}

			/* The light is now out */
                        else if (o_ptr->timeout < 1)
			{
				disturb(0, 0);
                                cmsg_print(TERM_YELLOW, "Your light has gone out!");
			}

			/* The light is getting dim */
                        else if ((o_ptr->timeout < 100))
			{
				if (disturb_minor) disturb(0, 0);
                                cmsg_print(TERM_YELLOW, "Your light is growing faint.");
			}
		}
	}

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);


        /*** Process corruption effects ***/
        if (p_ptr->muta2 && (!p_ptr->wild_mode) && (dun_level))
	{
		if ((p_ptr->muta2 & MUT2_BERS_RAGE) && (randint(3000)==1))
		{
			disturb(0,0);
                        cmsg_print(TERM_L_RED, "RAAAAGHH!");
                        cmsg_print(TERM_L_RED, "You feel a fit of rage coming over you!");
			(void) set_shero(p_ptr->shero + 10 + randint(p_ptr->lev));
		}

		if ((p_ptr->muta2 & MUT2_COWARDICE) && (randint(3000)==13))
		{
			if (!(p_ptr->resist_fear || p_ptr->hero || p_ptr->shero))
			{
				disturb(0,0);
				msg_print("It's so dark... so scary!");
				p_ptr->redraw |= PR_AFRAID;
				p_ptr->afraid = (p_ptr->afraid) + 13 + randint(26);
			}
		}

		if ((p_ptr->muta2 & MUT2_RTELEPORT) && (randint(5000)==88))
		{
			if (!(p_ptr->resist_nexus) && !(p_ptr->muta1 & MUT1_VTELEPORT)
			    && !(p_ptr->anti_tele))
			{
				disturb(0,0);

				/* Teleport player */
				msg_print("Your position suddenly seems very uncertain...");
				msg_print(NULL);
				teleport_player(40);
			}
		}

		if ((p_ptr->muta2 & MUT2_ALCOHOL) && (randint(6400)==321))
		{
			if (!(p_ptr->resist_chaos || p_ptr->resist_conf))
			{
				disturb(0,0);
				p_ptr->redraw |= PR_EXTRA;
				msg_print("You feel a SSSCHtupor cOmINg over yOu... *HIC*!");

				if (randint(20)==1)
				{
					msg_print(NULL);
					if (randint(3)==1) lose_all_info();
					else wiz_dark();
					teleport_player(100);
					wiz_dark();
					msg_print("You wake up somewhere with a sore head...");
					msg_print("You can't remember a thing, or how you got here!");
				}
				else
				{
					if (!(p_ptr->resist_conf))
					{
						(void)set_confused(p_ptr->confused + rand_int(20) + 15);
					}

					if ((randint(3)==1) && !(p_ptr->resist_chaos))
					{
						msg_print("Thishcischs GooDSChtuff!");
						(void)set_image(p_ptr->image + rand_int(150) + 150);
					}
				}
			}
		}

		if ((p_ptr->muta2 & MUT2_HALLU) && (randint(6400)==42))
		{
			if (!(p_ptr->resist_chaos))
			{
				disturb(0,0);
				p_ptr->redraw |= PR_EXTRA;
				(void)set_image(p_ptr->image + rand_int(50) + 20);
			}
		}

		if ((p_ptr->muta2 & MUT2_FLATULENT) && (randint(3000)==13))
		{
			disturb(0,0);

			msg_print("BRRAAAP! Oops.");
			msg_print(NULL);
			fire_ball(GF_POIS, 0, p_ptr->lev,3);
		}

		if ((p_ptr->muta2 & MUT2_PROD_MANA) &&
		    (!(p_ptr->anti_magic)) && (randint(9000)==1))
		{
			int dire = 0;
			disturb(0,0);
			msg_print("Magical energy flows through you! You must release it!");
			flush();
			msg_print(NULL);
			(void)get_hack_dir(&dire);
			fire_ball(GF_MANA, dire, p_ptr->lev * 2, 3);
		}

		if ((p_ptr->muta2 & MUT2_ATT_DEMON) &&
		    (!(p_ptr->anti_magic)) && (randint(6666)==666))
		{
			bool d_summon = FALSE;
			if (randint(6)==1)
			{
				d_summon = summon_specific_friendly(py, px,
				    dun_level, SUMMON_DEMON, TRUE);
			}
			else
			{
				d_summon = summon_specific(py, px,
				    dun_level, SUMMON_DEMON);
			}

			if (d_summon)
			{
                                cmsg_print(TERM_VIOLET, "You have attracted a demon!");
				disturb(0,0);
			}
		}

		if ((p_ptr->muta2 & MUT2_SPEED_FLUX) && (randint(6000)==1))
		{

			disturb(0,0);
			if (randint(2)==1)
			{
                                cmsg_print(TERM_L_RED, "You feel less energetic.");
				if (p_ptr->fast > 0)
				{
					set_fast(0);
				}
				else
				{
					set_slow(p_ptr->slow + randint(30) + 10);
				}
			}
			else
			{
                                cmsg_print(TERM_L_GREEN, "You feel more energetic.");
				if (p_ptr->slow > 0)
				{
					set_slow(0);
				}
				else
				{
					set_fast(p_ptr->fast + randint(30) + 10);
				}
			}
			msg_print(NULL);
		}
		if ((p_ptr->muta2 & MUT2_BANISH_ALL) &&
			(randint(9000)==1))
		{

			disturb(0,0);
			msg_print("You suddenly feel almost lonely.");
			banish_monsters(100);
			if (!dun_level)
			{
				msg_print("You see one of the shopkeepers running for the hills!");
                                store_shuffle(rand_int(max_st_idx));
			}
			msg_print(NULL);
		}
		if ((p_ptr->muta2 & MUT2_EAT_LIGHT) &&
			(randint(3000)==1))
		{
			object_type *o_ptr;

			msg_print("A shadow passes over you.");
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
                                u32b f1, f2, f3, f4, f5, esp;

                                /* Extract the item flags */
                                object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

                                /* Use some fuel (except on artifacts) */
                                if ((f4 & TR4_FUEL_LITE) && (o_ptr->timeout > 0))
				{
					/* Heal the player a bit */
                                        hp_player(o_ptr->timeout/20);

					/* Decrease life-span of lite */
                                        o_ptr->timeout /= 2;

					msg_print("You absorb energy from your light!");

					/*
					 * ToDo: Implement a function to handle
					 * changes of the fuel
					 */

					/* Hack -- notice interesting fuel steps */
                                        if ((o_ptr->timeout < 100) || (!(o_ptr->timeout % 100)))
					{
						/* Window stuff */
						p_ptr->window |= (PW_EQUIP);
					}

					/* Hack -- Special treatment when blind */
					if (p_ptr->blind)
					{
						/* Hack -- save some light for later */
                                                if (o_ptr->timeout == 0) o_ptr->timeout++;
					}

					/* The light is now out */
                                        else if (o_ptr->timeout == 0)
					{
						disturb(0, 0);
                                                cmsg_print(TERM_YELLOW, "Your light has gone out!");
					}

					/* The light is getting dim */
                                        else if ((o_ptr->timeout < 100) && (!(o_ptr->timeout % 10)))
					{
						if (disturb_minor) disturb(0, 0);
                                                cmsg_print(TERM_YELLOW, "Your light is growing faint.");
					}
				}
			}

			/*
			 * Unlite the area (radius 10) around player and
			 * do 50 points damage to every affected monster
			 */
			unlite_area(50, 10);
		}
		if ((p_ptr->muta2 & MUT2_ATT_ANIMAL) && !(p_ptr->anti_magic) &&
			(randint(7000)==1))
		{

			bool a_summon = FALSE;
			if (randint(3)==1)
				a_summon = summon_specific_friendly(py,
				px, dun_level, SUMMON_ANIMAL, TRUE);
			else
				a_summon = summon_specific(py,
				px, dun_level, SUMMON_ANIMAL);
			if (a_summon)
			{
				msg_print("You have attracted an animal!");
				disturb(0,0);
			}
		}
		if ((p_ptr->muta2 & MUT2_RAW_CHAOS) && !(p_ptr->anti_magic) &&
			(randint(8000)==1))
		{

			disturb(0,0);
			msg_print("You feel the world warping around you!");
			msg_print(NULL);
			fire_ball(GF_CHAOS, 0, p_ptr->lev,8);
		}
		if ((p_ptr->muta2 & MUT2_NORMALITY) &&
			(randint(5000)==1))
		{
                        lose_corruption(0);
		}
		if ((p_ptr->muta2 & MUT2_WRAITH) && !(p_ptr->anti_magic) &&
			(randint(3000)==13))
		{

			disturb(0,0);
			msg_print("You feel insubstantial!");
			msg_print(NULL);
                        set_shadow(p_ptr->tim_wraith + randint(p_ptr->lev/2) + (p_ptr->lev/2));
		}
		if ((p_ptr->muta2 & MUT2_POLY_WOUND) &&
			(randint(3000)==1))
		{
			do_poly_wounds();
		}
		if ((p_ptr->muta2 & MUT2_WASTING) &&
			(randint(3000)==13))
		{
			int which_stat = rand_int(6);
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
				msg_print("Invalid stat chosen!");
				sustained = TRUE;
			}

			if (!sustained)
			{
				disturb(0,0);
				msg_print("You can feel yourself wasting away!");
				msg_print(NULL);
				(void)dec_stat(which_stat, randint(6)+6, randint(3)==1);
			}
		}
		if ((p_ptr->muta2 & MUT2_ATT_DRAGON) && !(p_ptr->anti_magic) &&
			(randint(3000)==13))
		{

			bool d_summon = FALSE;
			if (randint(5)==1)
				d_summon = summon_specific_friendly(py,
				px, dun_level, SUMMON_DRAGON, TRUE);
			else
				d_summon = summon_specific(py,
				px, dun_level, SUMMON_DRAGON);
			if (d_summon)
			{
				msg_print("You have attracted a dragon!");
				disturb(0,0);
			}
		}
		if ((p_ptr->muta2 & MUT2_WEIRD_MIND) && !(p_ptr->anti_magic) &&
			(randint(3000)==1))
		{
			if (p_ptr->tim_esp > 0)
			{
				msg_print("Your mind feels cloudy!");
				set_tim_esp(0);
			}
			else
			{
				msg_print("Your mind expands!");
				set_tim_esp(p_ptr->lev);
			}
		}
		if ((p_ptr->muta2 & MUT2_NAUSEA) && !(p_ptr->slow_digest) &&
			(randint(9000)==1))
		{

			disturb(0,0);
			msg_print("Your stomach rolls, and you lose your lunch!");
			msg_print(NULL);
			set_food(PY_FOOD_WEAK);
		}

		if ((p_ptr->muta2 & MUT2_WALK_SHAD) &&
		   !(p_ptr->anti_magic) &&
		    (randint(12000) == 1))
		{
			alter_reality();
		}

		if ((p_ptr->muta2 & MUT2_WARNING) &&
			(randint(1000)==1))
		{
			int danger_amount = 0;
			int monster;

			for (monster = 0; monster < m_max; monster++)
			{
				monster_type    *m_ptr = &m_list[monster];

				/* Paranoia -- Skip dead monsters */
				if (!m_ptr->r_idx) continue;

                                if (m_ptr->level >= p_ptr->lev)
				{
                                        danger_amount += m_ptr->level - p_ptr->lev + 1;
				}
			}

			if (danger_amount > 100)
				msg_print("You feel utterly terrified!");
			else if (danger_amount > 50)
				msg_print("You feel terrified!");
			else if (danger_amount > 20)
				msg_print("You feel very worried!");
			else if (danger_amount > 10)
				msg_print("You feel paranoid!");
			else if (danger_amount > 5)
				msg_print("You feel almost safe.");
			else
				msg_print("You feel lonely.");
		}
		if ((p_ptr->muta2 & MUT2_INVULN) && !(p_ptr->anti_magic) &&
			(randint(5000)==1))
		{

			disturb(0,0);
                        cmsg_print(TERM_L_GREEN, "You feel invincible!");
			msg_print(NULL);
			(void)set_invuln(p_ptr->invuln + randint(8) + 8);
		}
		if ((p_ptr->muta2 & MUT2_SP_TO_HP) &&
			(randint(2000)==1))
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
		if ((p_ptr->muta2 & MUT2_HP_TO_SP) && !(p_ptr->anti_magic) &&
			(randint(4000)==1))
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
				take_hit(healing, "blood rushing to the head");
			}
		}
		if ((p_ptr->muta2 & MUT2_DISARM) &&
			(randint(10000)==1))
		{
			object_type *o_ptr;

			disturb(0,0);
			msg_print("You trip over your own feet!");
			take_hit(randint(p_ptr->wt/6), "tripping");

			msg_print(NULL);
			o_ptr = &inventory[INVEN_WIELD];
			if (o_ptr->k_idx)
			{
				msg_print("You drop your weapon!");
                                inven_drop(INVEN_WIELD, 1, py, px, FALSE);
			}
        }
	}


	/*** Process Inventory ***/

        /* Handle experience draining.  In Oangband, the effect is worse,
         * especially for high-level characters.  As per Tolkien, hobbits
         * are resistant.
         */
        if (p_ptr->black_breath)
        {
                byte chance = 0;
                int plev = p_ptr->lev;

                if (p_ptr->prace == RACE_HOBBIT) chance = 2;
                else chance = 5;

                if ((rand_int(100) < chance) && (p_ptr->exp > 0))
                {
                        p_ptr->exp -= 1 + plev / 5;
                        p_ptr->max_exp -= 1 + plev / 5;
                        (void)do_dec_stat(randint(6)+1, STAT_DEC_NORMAL);
                        check_experience();
                }
        }

	/* Drain Mana */
	if (p_ptr->drain_mana && p_ptr->csp)
	{
                p_ptr->csp -= p_ptr->drain_mana;
                if (magik(30)) p_ptr->csp -= p_ptr->drain_mana;

                if (p_ptr->csp < 0) p_ptr->csp = 0;

		/* Redraw */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
                p_ptr->window |= (PW_PLAYER);
	}

	/* Drain Hitpoints */
        if (p_ptr->drain_life)
	{
                int drain = p_ptr->drain_life + rand_int(p_ptr->mhp / 100);

		p_ptr->chp -= (drain < p_ptr->chp ? drain : p_ptr->chp);

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
                p_ptr->window |= (PW_PLAYER);

	}

	/* Handle experience draining */
	if (p_ptr->exp_drain)
	{
		if ((rand_int(100) < 10) && (p_ptr->exp > 0))
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

                object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

		/* TY Curse */
		if ((f3 & TR3_TY_CURSE) && (randint(TY_CURSE_CHANCE)==1))
		{
			activate_ty_curse();
		}

                /* DG Curse */
                if ((f4 & TR4_DG_CURSE) && (randint(DG_CURSE_CHANCE)==1))
		{
                        activate_dg_curse();

                        /* The object recurse itself ! */
                        o_ptr->ident |= IDENT_CURSED;
		}

                /* Auto Curse */
                if ((f3 & TR3_AUTO_CURSE) && (!rand_int(AUTO_CURSE_CHANCE)))
		{
                        /* The object recurse itself ! */
                        o_ptr->ident |= IDENT_CURSED;
		}

		/* Make a chainsword noise */
		if ((o_ptr->name1 == ART_ELVAGIL) && randint(CHAINSWORD_NOISE) == 1)
		{
			char noise[80];
			get_rnd_line("chainswd.txt", noise);
			msg_print(noise);
			disturb(FALSE, FALSE);
		}

		/*
		 * Hack: Uncursed teleporting items (e.g. Trump Weapons)
		 * can actually be useful!
		 */

		if ((f3 & TR3_TELEPORT) && (rand_int(100)<1))
		{
			if ((o_ptr->ident & IDENT_CURSED) && !(p_ptr->anti_tele))
			{
				disturb(0,0);

				/* Teleport player */
				teleport_player(40);
			}
			else
			{
                                if ((p_ptr->wild_mode) || (!disturb_other) ||
				    (o_ptr->note && (strchr(quark_str(o_ptr->note),'.'))))
				{
					/* Do nothing */
					/* msg_print("Teleport aborted.") */ ;
				}
				else if (get_check("Teleport? "))
				{
					disturb(0,0);
					teleport_player(50);
				}
			}
		}


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
				recharged_notice(o_ptr);
                                j++;
                        }
		}

                /* Recharge activatable mage staffs */
                if ((o_ptr->xtra2 > 0) && (is_ego_p(o_ptr, EGO_MSTAFF_SPELL)))
		{
			/* Recharge */
                        o_ptr->xtra2--;

			/* Notice changes */
                        if (!(o_ptr->xtra2)) j++;
		}
	}

	/* Notice changes */
	if (j)
	{
		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
	}

	/* Recharge rods */
        for (j = 0, i = 0; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];
		k_ptr = &k_info[o_ptr->k_idx];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

                /* Examine the rod */
                object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

                /* Temporary items are destroyed */
                if (f5 & TR5_TEMPORARY)
                {
                        o_ptr->timeout--;

                        if (o_ptr->timeout <= 0)
                        {
                                inven_item_increase(i, -99);
                                inven_item_describe(i);
                                inven_item_optimize(i);

                                /* Combine and Reorder pack */
                                p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                        }
                }

		/* Examine all charging rods or stacks of charging rods. */
                if ((o_ptr->tval == TV_ROD_MAIN) && (o_ptr->timeout < o_ptr->pval2))
		{
                        /* Increase the rod's mana. */
                        o_ptr->timeout += (f4 & TR4_CHARGING)?2:1;

                        /* Always notice */
                        j++;

			/* Notice changes, provide message if object is inscribed. */
                        if (o_ptr->timeout >= o_ptr->pval2)
			{
                                o_ptr->timeout = o_ptr->pval2;
				recharged_notice(o_ptr);
			}
		}

                /* Examine all charging random artifacts */
                if ((o_ptr->tval == TV_RANDART) && (o_ptr->timeout))
		{
			/* Charge it */
                        o_ptr->timeout--;

			/* Notice changes */
                        if (!(o_ptr->timeout)) j++;
		}

		/* Decay objects in pack */
                if (decays(o_ptr))
		{
			/* Decay it */
                        if (o_ptr->pval != 0)
                        {
                                if (!o_ptr->timeout)
                                {
                                        if(d_info[dungeon_type].flags1 & DF1_HOT)
                                        {
                                               o_ptr->pval -= 2;
                                        }
                                        else if((d_info[dungeon_type].flags1 & DF1_COLD) && rand_int(2))
                                        {
                                               o_ptr->pval--;
                                        }
                                        else
                                        {
                                               o_ptr->pval--;
                                        }
                                }

                                if ((o_ptr->timeout > 0) && o_ptr->timeout < o_ptr->weight) o_ptr->timeout--;

                                /* Notice changes */
                                if (o_ptr->pval <= 0)
                                {
                                        pack_decay(i);
                                        j++;
                                }
                        }
		}

                /* Hatch eggs */
                if(o_ptr->tval == TV_EGG)
                {
                        int mx,my;
			if (!o_ptr->timeout) o_ptr->pval--;

			/* Notice changes */
			if (o_ptr->pval <= 0)
			{
                                monster_type *m_ptr;
                                monster_race *r_ptr;

                                mx=px;
                                my=py+1;
                                get_pos_player(5, &my, &mx);
                                msg_print("Your egg hatchs!");
                                place_monster_aux(my, mx, o_ptr->pval2, FALSE, FALSE, MSTATUS_PET);

                                m_ptr = &m_list[cave[my][mx].m_idx];
                                r_ptr = race_inf(m_ptr);
                                if ((r_ptr->flags9 & RF9_IMPRESED) && can_create_companion())
                                {
                                        msg_print("And you have given the imprint to your monster!");
                                        m_ptr->status = MSTATUS_COMPANION;
                                }

                                inven_item_increase(i, -1);
                                inven_item_describe(i);
                                inven_item_optimize(i);
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


	/*** Process Objects ***/

	/* Process objects */
	for (i = 1; i < o_max; i++)
	{
		/* Access object */
		o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

                /* Examine the rod */
                object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

                /* Temporary items are destroyed */
                if (f5 & TR5_TEMPORARY)
                {
                        o_ptr->timeout--;

                        if (o_ptr->timeout <= 0)
                        {
                                floor_item_increase(i, -99);
                                floor_item_optimize(i);

                                /* Combine and Reorder pack */
                                p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                        }
                }

		/* Recharge rods on the ground.  No messages. */
                if ((o_ptr->tval == TV_ROD_MAIN) && (o_ptr->timeout < o_ptr->pval2))
		{
                        /* Increase the rod's mana. */
                        o_ptr->timeout += (f4 & TR4_CHARGING)?2:1;

                        /* Do not overflow */
                        if (o_ptr->timeout >= o_ptr->pval2)
			{
                                o_ptr->timeout = o_ptr->pval2;
			}
		}

		/* Decay objects on the ground*/
		if (decays(o_ptr))
		{
			/* Decay it */
                        if (o_ptr->pval != 0)
                        {
                                if (!o_ptr->timeout)
                                {
                                        if(d_info[dungeon_type].flags1 & DF1_HOT)
                                        {
                                               o_ptr->pval -= 2;
                                        }
                                        else if((d_info[dungeon_type].flags1 & DF1_COLD) && rand_int(2))
                                        {
                                               o_ptr->pval--;
                                        }
                                        else
                                        {
                                               o_ptr->pval--;
                                        }
                                }

                                if ((o_ptr->timeout > 0) && o_ptr->timeout < o_ptr->weight) o_ptr->timeout--;

                                /* Turn it into a skeleton */
                                if (o_ptr->pval <= 0)
                                {
                                        floor_decay(i);
                                }
                        }
                }

                /* Hatch eggs */
                if(o_ptr->tval == TV_EGG)
                {
                        int mx,my;
			if (!o_ptr->timeout) o_ptr->pval--;

			/* Notice changes */
			if (o_ptr->pval <= 0)
			{
                                mx=o_ptr->ix;
                                my=o_ptr->iy;
                                get_pos_player(5, &my, &mx);
                                msg_print("An egg hatchs!");
                                place_monster_one(my, mx, o_ptr->pval2, 0, FALSE, MSTATUS_ENEMY);
                                floor_item_increase(i, -1);
                                floor_item_describe(i);
                                floor_item_optimize(i);
			}
                }
	}


	/*** Involuntary Movement ***/

	/* Delayed Word-of-Recall */
        if (p_ptr->word_recall && (!p_ptr->astral) && (dungeon_type != DUNGEON_DEATH))
	{
		/*
		 * HACK: Autosave BEFORE resetting the recall counter (rr9)
		 * The player is yanked up/down as soon as
		 * he loads the autosaved game.
		 */
		if (autosave_l && (p_ptr->word_recall == 1))
		{
			is_autosave = TRUE;
			msg_print("Autosaving the game...");
			do_cmd_save_game();
			is_autosave = FALSE;
		}

		/* Count down towards recall */
		p_ptr->word_recall--;

		/* Activate the recall */
		if (!p_ptr->word_recall)
		{
			/* Disturbing! */
  			disturb(0, 0);

			/* Determine the level */
                        if (p_ptr->inside_quest)
                        {
                                msg_print("The recall is cancelled by a powerful magic force!");
                        }
                        else if (dun_level)
			{
				msg_print("You feel yourself yanked upwards!");

                                p_ptr->recall_dungeon = dungeon_type;
				dun_level = 0;

                                is_recall = TRUE;

				p_ptr->inside_quest = 0;
				p_ptr->leaving = TRUE;
			}
			else
			{
				msg_print("You feel yourself yanked downwards!");

				/* New depth */
                                dungeon_type = p_ptr->recall_dungeon;
                                dun_level = max_dlv[dungeon_type];
				if (dun_level < 1) dun_level = 1;

				/* Reset player position */
                                p_ptr->oldpx = px;
                                p_ptr->oldpy = py;

				/* Leaving */
                                is_recall = TRUE;

				p_ptr->leaving = TRUE;
                                p_ptr->wild_mode = FALSE;
			}

			/* Sound */
			sound(SOUND_TPLEVEL);
		}
	}
        else if (p_ptr->word_recall && p_ptr->astral)
        {
                msg_print("As an astral being you can't recall.");
                p_ptr->word_recall = 0;
        }
        else if (p_ptr->word_recall && (dungeon_type == DUNGEON_DEATH))
        {
                cmsg_print(TERM_L_DARK, "You are fated to die here, FIGHT for your life!");
                p_ptr->word_recall = 0;
        }
}



/*
 * Verify use of "wizard" mode
 */
static bool enter_wizard_mode(void)
{
	/* Ask first time */
#if 0
	if (!(noscore & 0x0002))
#else
	if (!noscore)
#endif
	{
		/* Mention effects */
		msg_print("Wizard mode is for debugging and experimenting.");
		msg_print("The game will not be scored if you enter wizard mode.");
		msg_print(NULL);

		/* Verify request */
		if (!get_check("Are you sure you want to enter wizard mode? "))
		{
			return (FALSE);
		}

		/* Mark savefile */
                noscore |= 0x0002;
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
#if 0
	if (!(noscore & 0x0008))
#else
	if (!noscore)
#endif
	{
		/* Mention effects */
		msg_print("The debug commands are for debugging and experimenting.");
		msg_print("The game will not be scored if you use debug commands.");
		msg_print(NULL);

		/* Verify request */
		if (!get_check("Are you sure you want to use debug commands? "))
		{
			return (FALSE);
		}

		/* Mark savefile */
                noscore |= 0x0008;
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
		msg_print("The borg commands are for debugging and experimenting.");
		msg_print("The game will not be scored if you use borg commands.");
		msg_print(NULL);

		/* Verify request */
		if (!get_check("Are you sure you want to use borg commands? "))
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
	char error_m[80];

#ifdef ALLOW_REPEAT /* TNB */

    /* Handle repeating the last command */
    repeat_check();

#endif /* ALLOW_REPEAT -- TNB */

	/* Parse the command */
	switch (command_cmd)
	{
		/* Ignore */
		case ESCAPE:
		case ' ':
                case 0:
		{
			break;
		}

		/* Ignore return */
		case '\r':
		{
			break;
		}
#ifdef ALLOW_QUITING
                case KTRL('L'):
		{
                        quit("CHEATER");
                        break;
                }
#endif
		/*** Wizard Commands ***/

		/* Toggle Wizard Mode */
		case KTRL('W'):
		{
			if (wizard)
			{
				wizard = FALSE;
				msg_print("Wizard mode off.");
			}
			else if (enter_wizard_mode())
			{
				wizard = TRUE;
				msg_print("Wizard mode on.");
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
                                if(!p_ptr->wild_mode) do_cmd_borg();
			}

			break;
		}

#endif /* ALLOW_BORG */



		/*** Inventory Commands ***/

		/* Wear/wield equipment */
		case 'w':
		{
                        if(!p_ptr->wild_mode) do_cmd_wield();
			break;
		}

		/* Take off equipment */
		case 't':
		{
                        if(!p_ptr->wild_mode) do_cmd_takeoff();
                        p_ptr->redraw |= (PR_MH);
			break;
		}

		/* Drop an item */
		case 'd':
		{
                        if(!p_ptr->wild_mode) do_cmd_drop();
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
                        if(!p_ptr->wild_mode) do_cmd_alter();
			break;
		}

		/* Dig a tunnel */
		case 'T':
		{
                        if(!p_ptr->wild_mode) do_cmd_tunnel();
			break;
		}

		/* Move (usually pick up things) */
		case ';':
		{
#ifdef ALLOW_EASY_DISARM /* TNB */

			do_cmd_walk(FALSE);

#else /* ALLOW_EASY_DISARM -- TNB */

			do_cmd_walk(always_pickup);

#endif /* ALLOW_EASY_DISARM -- TNB */

			break;
		}

		/* Move (usually do not pick up) */
		case '-':
		{
#ifdef ALLOW_EASY_DISARM /* TNB */

			do_cmd_walk(TRUE);

#else /* ALLOW_EASY_DISARM -- TNB */

			do_cmd_walk(!always_pickup);

#endif /* ALLOW_EASY_DISARM -- TNB */

			break;
		}


		/*** Running, Resting, Searching, Staying */

		/* Begin Running -- Arg is Max Distance */
		case '.':
		{
                        if(!p_ptr->wild_mode) do_cmd_run();
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
		case '_':
		{
                        if(!p_ptr->wild_mode) do_cmd_store();
			break;
		}

		/* Enter quest level -KMW- */
		case '[':
		{
                        if(!p_ptr->wild_mode) do_cmd_quest();
			break;
		}

		/* Go up staircase */
		case '<':
		{
                        if(!p_ptr->wild_mode && !dun_level && !is_quest(dun_level))
                        {
                                if (!vanilla_town)
                                {
                                        if(!ambush_flag)
                                        {
                                                p_ptr->oldpx = px;
                                                p_ptr->oldpy = py;
                                                change_wild_mode();

                                                /* Update the known wilderness */
                                                reveal_wilderness_around_player(p_ptr->wilderness_y,
                                                                                p_ptr->wilderness_x,
                                                                                0, WILDERNESS_SEE_RADIUS);
                                        }
                                        else
                                                msg_print("To flee the ambush you have to reach the edge of the map.");
                                }
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
                                if(wf_info[wild_map[py][px].feat].entrance >= 1000)
                                {
                                        p_ptr->wilderness_x = px;
                                        p_ptr->wilderness_y = py;
                                        p_ptr->wild_mode = !p_ptr->wild_mode;
                                        do_cmd_go_down();
                                        if (dun_level == 0) p_ptr->wild_mode = !p_ptr->wild_mode;
                                }else{
                                        p_ptr->wilderness_x = px;
                                        p_ptr->wilderness_y = py;
                                        change_wild_mode();
                                }
                        }
                        break;
		}

		/* Open a door or chest */
		case 'o':
		{
                        if(!p_ptr->wild_mode) do_cmd_open();
			break;
		}

		/* Close a door */
		case 'c':
		{
                        if(!p_ptr->wild_mode) do_cmd_close();
			break;
		}

                /* Give an item */
                case 'y':
		{
                        if(!p_ptr->wild_mode) do_cmd_give();
			break;
		}

		/* Jam a door with spikes */
		case 'j':
		{
                        if(!p_ptr->wild_mode) do_cmd_spike();
			break;
		}

		/* Bash a door */
		case 'B':
		{
                        if(!p_ptr->wild_mode) do_cmd_bash();
			break;
		}

		/* Disarm a trap or chest */
		case 'D':
		{
                        if(!p_ptr->wild_mode) do_cmd_disarm();
			break;
		}


		/*** Magic and Prayers ***/

		/* Gain new spells/prayers */
		case 'G':
		{
                        if (p_ptr->pclass == CLASS_SORCERER)
                                msg_print("You don't have to learn spells!");
                        else
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
                        if(!p_ptr->wild_mode)
                        {
			if (!p_ptr->inside_arena)
			{
                                if (p_ptr->pclass == CLASS_UNBELIEVER)
                                {
                                        do_cmd_unbeliever();
                                }
                                else if (p_ptr->anti_magic)
				{
					cptr which_power = "magic";
					if (p_ptr->pclass == CLASS_MINDCRAFTER)
						which_power = "psionic powers";
                                        else if (p_ptr->pclass == CLASS_BEASTMASTER)
                                                which_power = "summoning powers";
                                        else if (p_ptr->pclass == CLASS_ALCHEMIST)
                                                which_power = "alchemist powers";
                                        else if (p_ptr->pclass == CLASS_MIMIC)
                                                which_power = "mimic powers";
                                        else if (p_ptr->pclass == CLASS_HARPER)
                                                which_power = "singing abilities";
                                        else if (mp_ptr->spell_book == TV_VALARIN_BOOK)
						which_power = "prayer";
                                        else if (p_ptr->pclass == CLASS_RUNECRAFTER)
                                                which_power = "combining abilities";
                                        else if (p_ptr->pclass == CLASS_ARCHER)
                                                which_power = "forging abilities";
                                        else if (p_ptr->pclass == CLASS_POSSESSOR)
                                                which_power = "transfering abilities";
                                        else if (p_ptr->pclass == CLASS_NECRO)
                                                which_power = "necromantic powers";
                                        else if (p_ptr->pclass == CLASS_UNBELIEVER)
                                                which_power = "sensing powers";
                                        else if (p_ptr->pclass == CLASS_MERCHANT)
                                                which_power = "telekinetic powers";

					msg_format("An anti-magic shell disrupts your %s!", which_power);

					energy_use = 0;
				}
				else
				{
					if (p_ptr->pclass == CLASS_MINDCRAFTER)
						do_cmd_mindcraft();
                                        else if (p_ptr->pclass == CLASS_ALCHEMIST)
                                                do_cmd_alchemist();
                                        else if (p_ptr->pclass == CLASS_MIMIC)
                                                do_cmd_mimic();
                                        else if (p_ptr->pclass == CLASS_POWERMAGE)
                                                do_cmd_powermage();
                                        else if (p_ptr->pclass == CLASS_RUNECRAFTER)
                                                do_cmd_runecrafter();
                                        else if (p_ptr->pclass == CLASS_ARCHER)
                                                do_cmd_archer();
                                        else if (p_ptr->pclass == CLASS_POSSESSOR)
                                                do_cmd_possessor();
                                        else if (p_ptr->pclass == CLASS_UNBELIEVER)
                                                do_cmd_unbeliever();
                                        else if (p_ptr->pclass == CLASS_MERCHANT)
                                                do_cmd_portable_hole();
					else
						do_cmd_cast();
				}
			}
			else
			{
				msg_print("The arena absorbs all attempted magic!");
				msg_print(NULL);
			}
                        }
			break;
		}

		/* Pray a prayer */
		case 'p':
		{
                        if(!p_ptr->wild_mode) do_cmd_pray();
			break;
		}

                /* Issue commands to pets */
                case 'P':
		{
                        if(!p_ptr->wild_mode) do_cmd_pet();
			break;
		}

		/* Cut up a corpse */
		case 'h':
		{
                        if(!p_ptr->wild_mode) do_cmd_cut_corpse();
			break;
		}

		/* Cure some meat */
		case 'K':
                {
			do_cmd_cure_meat();
                        break;
                }

                /* Steal an item form a monster */
                case 'Z':
                {
                        if(!p_ptr->wild_mode) do_cmd_steal();
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
                        if(!p_ptr->wild_mode)
                        {
			if (!p_ptr->inside_arena)
				do_cmd_activate();
			else
			{
				msg_print("The arena absorbs all attempted magic!");
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
                        if(!p_ptr->wild_mode)
                        {
			if (!p_ptr->inside_arena)
                        {
                                object_type *j_ptr;
                                j_ptr = &inventory[INVEN_BOW];
                                if(j_ptr->tval==TV_BOOMERANG)
                                        do_cmd_boomerang();
                                else
                                        do_cmd_fire();
                        }
                        else
			{
				msg_print("You're in the arena now. This is hand-to-hand!");
				msg_print(NULL);
			}
                        }
			break;
		}

		/* Throw an item */
		case 'v':
		{
                        if(!p_ptr->wild_mode)
                        {
			if (!p_ptr->inside_arena)
                                do_cmd_throw();
			else
			{
				msg_print("You're in the arena now. This is hand-to-hand!");
				msg_print(NULL);
			}
                        }
			break;
		}

		/* Aim a wand */
		case 'a':
		{
                        if(!p_ptr->wild_mode)
                        {
                        if (!p_ptr->inside_arena)
				do_cmd_aim_wand();
			else
			{
				msg_print("The arena absorbs all attempted magic!");
				msg_print(NULL);
			}
                        }
			break;
		}

		/* Zap a rod */
		case 'z':
		{
                        if(!p_ptr->wild_mode)
                        {
			if (!p_ptr->inside_arena)
				do_cmd_zap_rod();
			else
			{
				msg_print("The arena absorbs all attempted magic!");
				msg_print(NULL);
			}
                        }
			break;
		}

		/* Quaff a potion */
		case 'q':
		{
                        if(!p_ptr->wild_mode)
                        {
			if (!p_ptr->inside_arena)
				do_cmd_quaff_potion();
			else
			{
				msg_print("The arena absorbs all attempted magic!");
				msg_print(NULL);
			}
                        }
			break;
		}

		/* Drink from a fountain -SC- */
		case 'H':
		{
			cave_type *c_ptr = &cave[py][px];

			if ((c_ptr->feat == FEAT_FOUNTAIN) ||
			    (c_ptr->feat == FEAT_EMPTY_FOUNTAIN))
			{
				do_cmd_drink_fountain();
			}
			else
			{
                              msg_print("You see no fountain here.");
			}

			break;
		}

		/* Read a scroll */
		case 'r':
		{
                        if(!p_ptr->wild_mode)
                        {
			if (!p_ptr->inside_arena)
				do_cmd_read_scroll();
			else
			{
				msg_print("The arena absorbs all attempted magic!");
				msg_print(NULL);
			}
                        }
			break;
		}

		/* Use a staff */
		case 'u':
		{
                        if(!p_ptr->wild_mode)
                        {
			if (!p_ptr->inside_arena)
				do_cmd_use_staff();
			else
			{
				msg_print("The arena absorbs all attempted magic!");
				msg_print(NULL);
			}
                        }
                        break;
		}

		/* Use racial power */
		case 'U':
		{
                        if(!p_ptr->wild_mode)
                        {
			if (!p_ptr->inside_arena)
                                do_cmd_power();
			else
			{
				msg_print("The arena absorbs all attempted magic!");
				msg_print(NULL);
			}
                        }
                        break;
		}

                /* Sacrifice at an altar */
                case 'O':
                {
                        if(!p_ptr->wild_mode)
                        {
                         if (p_ptr->pclass == CLASS_CHAOS_WARRIOR)
                         {
                                 msg_print("Chaos Warriors cannot sacrifice to other gods");
                         }
                         else
                         {
                                 do_cmd_sacrifice();
                         }
                        }
                         break;
                }

		/*** Looking at Things (nearby or on map) ***/

		/* Full dungeon map */
		case 'M':
		{
                        if(!p_ptr->wild_mode) do_cmd_view_map();
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
                        if(!p_ptr->wild_mode) do_cmd_target();
			break;
		}

                /* Engrave the floor */
                case 'x':
                {
                        if(!p_ptr->wild_mode) do_cmd_sense_grid_mana();
                        if(!p_ptr->wild_mode) do_cmd_engrave();
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
                        if(!p_ptr->wild_mode) do_cmd_feeling();
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

		/* Show quest status -KMW- */
		case KTRL('Q'):
		{
			do_cmd_checkquest();
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
			is_autosave = FALSE;
			do_cmd_save_game();
			break;
		}

#endif /* VERIFY_SAVEFILE */

		case KTRL('T'):
		{
			do_cmd_time();
		}
		break;

		/* Save and quit */
		case KTRL('X'):
		{
			alive = FALSE;

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

                /* Activate cmovie */
		case '|':
		{
                        /* Stop ? */
                        if (do_movies == 1)
                        {
                                do_stop_cmovie();
                                msg_print("Cmovie recording stopped.");
                        }
                        else
                        {
                                do_start_cmovie();
                        }
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

		/* Hack -- Unknown command */
		default:
		{
                        if (randint(2)==1)
                        {
                                get_rnd_line("error.txt", error_m);
                                sound(SOUND_ILLEGAL);
                                msg_print(error_m);
                        }
                        else
                                prt("Type '?' for help.", 0, 0);
                        break;
		}
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
        int i, j;

	extern bool ate;
	/*** Apply energy ***/

        if (hack_corruption)
	{
		msg_print("You feel different!");
                (void)gain_random_corruption(0);
                hack_corruption = FALSE;
	}

	/* Give the player some energy */
        p_ptr->energy += extract_energy[(p_ptr->pspeed > 199)?199:(p_ptr->pspeed < 0)?0:p_ptr->pspeed];

	/* No turn yet */
	if (p_ptr->energy < 100) return;

	/* We've eaten */
	ate = FALSE;
	
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
			bool stop = TRUE;
                        object_type *o_ptr;
                        monster_race *r_ptr;
                        int max;

                        /* Get the carried monster */
                        o_ptr = &inventory[INVEN_CARRY];
                        r_ptr = &r_info[o_ptr->pval];
                        max = maxroll(r_ptr->hdice, r_ptr->hside);

			/* Stop resting */
			if (p_ptr->chp != p_ptr->mhp) stop = FALSE;
			if (p_ptr->csp != p_ptr->msp) stop = FALSE;
                        if (o_ptr->pval2 != max) stop = FALSE;
			if (p_ptr->blind || p_ptr->confused) stop = FALSE;
			if (p_ptr->poisoned || p_ptr->afraid) stop = FALSE;
			if (p_ptr->stun || p_ptr->cut) stop = FALSE;
			if (p_ptr->slow || p_ptr->paralyzed) stop = FALSE;
                        if (p_ptr->image || p_ptr->word_recall) stop = FALSE;
			if (p_ptr->immov_cntr != 0) stop = FALSE;
			for (i = 0; i < 6; i++)
				if (p_ptr->stat_cnt[i] > 0) stop = FALSE;

			if (stop)
			{
				disturb(0, 0);
			}
			p_ptr->redraw |= (PR_STATE);
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
				msg_print("Cancelled.");
			}
		}
	}


	/*** Handle actual user input ***/

	/* Repeat until out of energy */
	while (p_ptr->energy >= 100)
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
		move_cursor_relative(py, px);

		/* Refresh (optional) */
		if (fresh_before) Term_fresh();


		/* Hack -- Pack Overflow */
		if (inventory[INVEN_PACK].k_idx)
		{
			int item = INVEN_PACK;

			char o_name[80];

			object_type *o_ptr;

			/* Access the slot to be dropped */
			o_ptr = &inventory[item];

			/* Disturbing */
			disturb(0, 0);

			/* Warning */
			msg_print("Your pack overflows!");

			/* Describe */
			object_desc(o_name, o_ptr, TRUE, 3);

			/* Message */
			msg_format("You drop %s (%c).", o_name, index_to_label(item));

			/* Drop it (carefully) near the player */
			drop_near(o_ptr, 0, py, px);

			/* Modify, Describe, Optimize */
			inven_item_increase(item, -255);
			inven_item_describe(item);
			inven_item_optimize(item);

			/* Notice stuff (if needed) */
			if (p_ptr->notice) notice_stuff();

			/* Update stuff (if needed) */
			if (p_ptr->update) update_stuff();

			/* Redraw stuff (if needed) */
			if (p_ptr->redraw) redraw_stuff();

			/* Redraw stuff (if needed) */
			if (p_ptr->window) window_stuff();
		}


		/* Hack -- cancel "lurking browse mode" */
		if (!command_new) command_see = FALSE;


		/* Assume free turn */
		energy_use = 0;


		/* Paralyzed or Knocked Out */
		if ((p_ptr->paralyzed) || (p_ptr->stun >= 100))
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

			/* Get a command (normal) */
                        request_command(FALSE);

			/* Process the command */
                        process_command();
		}

                /* Squektch'em up ! */
                squeltch_grid();
                squeltch_inventory();


		/*** Clean up ***/

		/* Significant */
		if (energy_use)
		{
			/* Use some energy */
			p_ptr->energy -= energy_use;


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

					/* Access monster */
					m_ptr = &m_list[i];

					/* Skip dead monsters */
					if (!m_ptr->r_idx) continue;

					/* Access the monster race */
                                        r_ptr = race_inf(m_ptr);

					/* Skip non-multi-hued monsters */
					if (!(r_ptr->flags1 & (RF1_ATTR_MULTI))) continue;

					/* Reset the flag */
					shimmer_monsters = TRUE;

					/* Redraw regardless */
					lite_spot(m_ptr->fy, m_ptr->fx);
				}
			}

                        /* Shimmer objects if needed */
                        if (!avoid_other && shimmer_objects)
			{
				/* Clear the flag */
                                shimmer_objects = FALSE;

                                /* Shimmer multi-hued objects */
                                for (i = 1; i < o_max; i++)
                                {
                                        /* Acquire object -- for speed only base items are allowed to shimmer */
                                        object_type *o_ptr = &o_list[i];
                                        object_kind *k_ptr = &k_info[o_ptr->k_idx];
		
                                        /* Skip dead or carried objects */
                                        if ((!o_ptr->k_idx) || (!o_ptr->ix)) continue;

					/* Skip non-multi-hued monsters */
                                        if (!(k_ptr->flags5 & (TR5_ATTR_MULTI))) continue;

					/* Reset the flag */
                                        shimmer_objects = TRUE;

					/* Redraw regardless */
                                        lite_spot(o_ptr->iy, o_ptr->ix);
				}
			}

                        /* Shimmer features if needed */
                        if (!avoid_other)
			{
                                /* Shimmer multi-hued features */
                                for (j = panel_row_min; j <= panel_row_max; j++)
                                {
                                        for (i = panel_col_min; i <= panel_col_max; i++)
                                        {
                                                /* Acquire object -- for speed only base items are allowed to shimmer */
                                                feature_type *f_ptr = &f_info[(cave[j][i].mimic)?cave[j][i].mimic:f_info[cave[j][i].feat].mimic];
		
                                                /* Skip non-multi-hued monsters */
                                                if (!(f_ptr->flags1 & (FF1_ATTR_MULTI))) continue;

                                                /* Redraw regardless */
                                                lite_spot(j, i);
                                        }
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
					if (m_ptr->mflag & (MFLAG_NICE))
					{
						/* Nice monsters get mean */
						m_ptr->mflag &= ~(MFLAG_NICE);
					}

					/* Handle memorized monsters */
					if (m_ptr->mflag & (MFLAG_MARK))
					{
						/* Maintain detection */
						if (m_ptr->mflag & (MFLAG_SHOW))
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

	/* Track maximum player level */
	if (p_ptr->max_plv < p_ptr->lev)
	{
		p_ptr->max_plv = p_ptr->lev;
	}

	/* Track maximum dungeon level (if not in quest -KMW-) */
        if ((max_dlv[dungeon_type] < dun_level) && (!p_ptr->inside_quest))
	{
                max_dlv[dungeon_type] = dun_level;
	}

	/* No stairs down from Quest */
        if (is_quest(dun_level) && (!p_ptr->astral))
	{
		create_down_stair = FALSE;
                create_down_shaft = FALSE;
	}

	/* Paranoia -- no stairs from town or wilderness */
	if (!dun_level) create_down_stair = create_up_stair = FALSE;
        if (!dun_level) create_down_shaft = create_up_shaft = FALSE;

	/* Option -- no connected stairs */
	if (!dungeon_stair) create_down_stair = create_up_stair = FALSE;
        if (!dungeon_stair) create_down_shaft = create_up_shaft = FALSE;

	/* no connecting stairs on special levels */
        if (!(dungeon_flags1 & LF1_NO_STAIR)) create_down_stair = create_up_stair = FALSE;
        if (!(dungeon_flags1 & LF1_NO_STAIR)) create_down_shaft = create_up_shaft = FALSE;

	/* Make a stairway. */
        if ((create_up_stair || create_down_stair || create_up_shaft ||
            create_down_shaft) && !get_fbranch())
	{
		/* Place a stairway */
		if (cave_valid_bold(py, px))
		{
			/* XXX XXX XXX */
			delete_object(py, px);

			/* Make stairs */
			if (create_down_stair)
			{
				cave_set_feat(py, px, FEAT_MORE);
			}
                        else if (create_down_shaft)
                        {
                                cave_set_feat(py, px, FEAT_SHAFT_DOWN);
                        }
                        else if (create_up_shaft)
                        {
                                cave_set_feat(py, px, FEAT_SHAFT_UP);
                        }
			else
			{
				cave_set_feat(py, px, FEAT_LESS);
			}
		}

		/* Cancel the stair request */
		create_down_stair = create_up_stair = FALSE;
                create_down_shaft = create_up_shaft = FALSE;
	}

	/* Center on player */
        if (center_player)
	{
		/* Center vertically */
                panel_row_min = py - (PANEL_HGT);
                if (panel_row_min < 0) panel_row_min = 0;
                else if (panel_row_min > max_panel_rows * (SCREEN_HGT / 2)) panel_row_min = max_panel_rows * (SCREEN_HGT / 2);

		/* Center horizontally */
                panel_col_min = px - (PANEL_WID);
                if (panel_col_min < 0) panel_col_min = 0;
                else if (panel_col_min > max_panel_cols * (SCREEN_WID / 2)) panel_col_min = max_panel_cols * (SCREEN_WID / 2);
	}
	else
	{
                /* Choose a panel row */
                panel_row_min = (py / SCREEN_HGT) * SCREEN_HGT;
                if (panel_row_min > max_panel_rows * PANEL_HGT) panel_row_min = max_panel_rows * PANEL_HGT;
                else if (panel_row_min < 0) panel_row_min = 0;

                /* Choose a panel col */
                panel_col_min = (px / SCREEN_WID) * SCREEN_WID;
                if (panel_col_min > max_panel_cols * PANEL_WID) panel_col_min = max_panel_cols * PANEL_WID;
                else if (panel_col_min < 0) panel_col_min = 0;
        }

	/* Recalculate the boundaries */
	panel_bounds();
        verify_panel();

	/* Flush messages */
	msg_print(NULL);


	/* Enter "xtra" mode */
	character_xtra = TRUE;

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER);

	/* Redraw dungeon */
        p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Update stuff */
        p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_POWERS | PU_SANITY | PU_BODY);

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Redraw stuff */
	window_stuff();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_DISTANCE);

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Leave "xtra" mode */
	character_xtra = FALSE;

	/* Update stuff */
        p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_POWERS | PU_BODY);

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


	/* Announce (or repeat) the feeling */
	if (dun_level) do_cmd_feeling();


	/* Hack -- notice death or departure */
	if (!alive || death) return;

	/*** Process this dungeon level ***/

	/* Reset the monster generation level */
	monster_level = dun_level;

	/* Reset the object generation level */
	object_level = dun_level;

	hack_mind = TRUE;

        /* Mega Hack, if needed wipe all stairs */
        if (dungeon_type == DUNGEON_DEATH)
        {
                int i, j;

                for (i = 0; i < cur_wid; i++)
                for (j = 0; j < cur_hgt; j++)
                {
                        if ((cave[j][i].feat == FEAT_MORE) || (cave[j][i].feat == FEAT_LESS) ||
                            (cave[j][i].feat == FEAT_SHAFT_UP) || (cave[j][i].feat == FEAT_SHAFT_DOWN))
                        {
                                cave[j][i].feat = FEAT_FLOOR;
                        }
                }

                /* Reset the monster generation level */
                monster_level = 127;

                /* Reset the object generation level */
                object_level = 0;
        }

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
		if (!alive || death) break;

		total_friends = 0;
		total_friend_levels = 0;

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
		if (!alive || death) break;


		/* Process the world */
                process_world();

                /* Process the appropriate hooks */
                process_hooks(HOOK_END_TURN, is_quest(dun_level));

                /* Make it pulsate and live !!!! */
                if ((d_info[dungeon_type].flags1 & DF1_EVOLVE) && dun_level)
                {
                        if (!(turn % 10)) evolve_level(TRUE);
                }

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
		if (!alive || death) break;

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Count game turns */
                turn++;
	}

        /* Did we leave a dungeon ? */
        if ((dun_level < d_info[dungeon_type].mindepth) && (!is_recall))
        {
                dun_level = 0;

                if (d_info[dungeon_type].ix > -1)
                {
                        p_ptr->wilderness_x = d_info[dungeon_type].ix;
                        p_ptr->wilderness_y = d_info[dungeon_type].iy;
                }

                dungeon_type = DUNGEON_WILDERNESS;
        }
        if (dun_level > d_info[dungeon_type].maxdepth)
        {
                dun_level = 0;

                if (d_info[dungeon_type].ox > -1)
                {
                        p_ptr->wilderness_x = d_info[dungeon_type].ox;
                        p_ptr->wilderness_y = d_info[dungeon_type].oy;
                }

                dungeon_type = DUNGEON_WILDERNESS;
        }
        is_recall = FALSE;
}




#if 0

/* Old routine to load pref files */

/*
 * Load some "user pref files"
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
}

#else

/* Arcum Dagsson's code to support separate macro files for different realms */

/*
 * Load some "user pref files"
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
        sprintf(buf, "%s.prf", player_name);

	/* Process that file */
	process_pref_file(buf);

	/* Access the "realm 1" pref file */
	if (realm_names[p_ptr->realm1] != "no magic")
		if (realm_names[p_ptr->realm1] != "unknown")
		{
			sprintf(buf, "%s.prf",realm_names[p_ptr->realm1]);

			/* Process that file */
			process_pref_file(buf);
		}

		/* Access the "realm 2" pref file */
		if (realm_names[p_ptr->realm2] != "no magic")
		if (realm_names[p_ptr->realm2] != "unknown")
		{
			sprintf(buf, "%s.prf",realm_names[p_ptr->realm2]);

			/* Process that file */
			process_pref_file(buf);
		}
}


#endif

/*
 * Modify the realm_info array using the class modificator
 */
void calc_magic()
{
        int i, j;

        for(i = 0; i < MAX_REALM; i++)
                for(j = 0; j < 64; j++)
                {
                        int tmp;

                        tmp = realm_info_base[i][j].slevel;
                        if (tmp < 99)
                        {
                                tmp += magic_info[p_ptr->pclass].spell_lev;
                                tmp = (tmp < 1)?1:(tmp > 50)?50:tmp;
                                realm_info_base[i][j].slevel = tmp;
                        }

                        tmp = realm_info_base[i][j].smana;
                        tmp += (tmp * magic_info[p_ptr->pclass].spell_mana) / 100;
                        tmp = (tmp < 1)?1:(tmp > 255)?255:tmp;
                        realm_info_base[i][j].smana = tmp;

                        tmp = realm_info_base[i][j].sfail;
                        tmp += (tmp * magic_info[p_ptr->pclass].spell_fail) / 100;
                        tmp = (tmp < 5)?5:(tmp > 95)?95:tmp;
                        realm_info_base[i][j].sfail = tmp;
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
        int i, tmp_dun;
        bool cheat_death=FALSE;

        hack_corruption = FALSE;

	/* Hack -- Character is "icky" */
	character_icky = TRUE;

	/* Hack -- turn off the cursor */
	(void)Term_set_cursor(0);

        /* Character list */
        if (!new_game && !no_begin_screen) new_game = begin_screen();
        no_begin_screen = FALSE;

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
#if 1
	/* Process old character */
	if (!new_game)
	{
		/* Process the player name */
                process_player_name(FALSE);
	}
#endif

	/* Extract the options */
	for (i = 0; option_info[i].o_desc; i++)
	{
                int os = option_info[i].o_page;
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

		/* Hack -- seed for flavors */
		seed_flavor = rand_int(0x10000000);

		/* Hack -- seed for town layout */
		seed_town = rand_int(0x10000000);

		/* Roll up a new character */
		player_birth();

                /* Start in town, or not */
                if (p_ptr->astral) dun_level = 98;
                else dun_level = 0;
		p_ptr->inside_quest = 0;
		p_ptr->inside_arena = 0;

		/* Hack -- enter the world */
                /* Mega-hack Vampires and Spectres start in the dungeon */
                if ((p_ptr->pracem == RMOD_VAMPIRE) || (p_ptr->pracem == RMOD_SPECTRE) || (p_ptr->pracem == RMOD_SKELETON) || (p_ptr->pracem == RMOD_ZOMBIE))
                        turn = (10L * DAY / 2) + (START_DAY * 10);
                else
                        turn = START_DAY * 10;
	}

        /*** Calculate the magic table for the player class ***/
        calc_magic();

        /* Calculate the magic table based on the number of spell learned */
        calc_magic_bonus();

	/* Flash a message */
	prt("Please wait...", 0, 0);

	/* Flush the message */
	Term_fresh();

        /* Be sure to not bother the player */
        calc_powers_silent = TRUE;

	/* Hack -- Enter wizard mode */
	if (arg_wizard && enter_wizard_mode()) wizard = TRUE;

	/* Flavor the objects */
	flavor_init();

	/* Reset the visual mappings */
	reset_visuals();

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER);

	/* Window stuff */
	window_stuff();

        /* load user file */
        process_pref_file("user.prf");

	/* Load the "pref" files */
	load_all_pref_files();

	/* Set or clear "rogue_like_commands" if requested */
	if (arg_force_original) rogue_like_commands = FALSE;
	if (arg_force_roguelike) rogue_like_commands = TRUE;

	/* Initialize vault info */
	if (init_v_info()) quit("Cannot initialize vaults");

        /* Initialize hooks */
        init_hooks();
        ingame_help(p_ptr->help.enabled);

	/* React to changes */
	Term_xtra(TERM_XTRA_REACT, 0);

        /* Mega hack, prevent lots of bugs */
        if ((px == 0) || (py == 0))
        {
                px = 1;
                py = 1;
        };

        /* Hack - if note file exists, load it */
        if (!new_game && take_notes)
        {
		add_note_type(NOTE_ENTER_DUNGEON);
        }

	/* Generate a dungeon level if needed */
	if (!character_dungeon) generate_cave();


	/* Character is now "complete" */
	character_generated = TRUE;


	/* Hack -- Character is no longer "icky" */
	character_icky = FALSE;


	/* Start game */
	alive = TRUE;

	/* Hack -- Enforce "delayed death" */
	if (p_ptr->chp < 0) death = TRUE;

        /* Should we use old colors */
        if (autoload_old_colors)
        {
                user_process_pref_file = FALSE;
                process_pref_file("422colors.prf");
                user_process_pref_file = TRUE;
        }


	/* Process */
	while (TRUE)
	{
                /* Save the level */
                old_dun_level = dun_level;
                p_ptr->old_wild_mode = p_ptr->wild_mode;

                /* We reached surface ? good, lets go down again !! */
                if (p_ptr->astral && (!dun_level))
                {
                        p_ptr->astral = FALSE;
                        cmsg_print(TERM_L_GREEN, "Well done ! You reached the town ! You can now go down again.");
                }

                /* Update monster list window */
                p_ptr->window |= (PW_M_LIST);

#ifdef SAVE_HACK
		/* Process the level */
                if (!save_hack)
                {
                        dungeon();
                }else{
                        generate_cave();
                }

                save_hack=FALSE;

                p_ptr->leaving=TRUE;
#else
		/* Process the level */
                dungeon();
#endif

                /* Save the current level if in a persistent level */
                tmp_dun = dun_level;
                dun_level = old_dun_level;
                save_dungeon();
                dun_level = tmp_dun;

                for(i = 0; i < MAX_FATES; i++)
                {
                        if (((!fates[i].serious) && (randint(2)==1)) || (fates[i].serious))
                        if ((fates[i].fate) && (fates[i].level == dun_level))
                        {
                                switch(fates[i].fate)
                                {
                                        case FATE_DIE:
                                                cmsg_print(TERM_L_DARK, "You were fated to die there, DIE!");

                                                /* You shall perish there */
                                                dungeon_type = DUNGEON_DEATH;
                                                dun_level = 1;

                                                fates[i].fate = FATE_NONE;
                                                break;
                                }
                        }
                }

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


		/* Handle "quit and save" */
		if (!alive && !death) break;


		/* Erase the old cave */
		wipe_o_list();


		/* XXX XXX XXX */
		msg_print(NULL);

		/* Accidental Death */
		if (alive && death)
		{
                        cheat_death = FALSE;
                        if (granted_resurrection())
                        {
                                cheat_death = TRUE;
                                p_ptr->grace = -100000;
                                cmsg_format(TERM_L_GREEN, "The power of %s raises you back from the grave!", deity_info[p_ptr->pgod-1].name);
                                msg_print(NULL);
                        }
                        else if (p_ptr->allow_one_death>0)
                        {
                                cheat_death = TRUE;
                                if (p_ptr->allow_one_death > 1) p_ptr->allow_one_death--; else p_ptr->allow_one_death=0;
                                cmsg_print(TERM_L_GREEN, "You have been saved by the Blood of Life!");
                                msg_print(NULL);
                        }
                        else
                        if ((wizard || cheat_live) && !get_check("Die? "))
                        {
                                cheat_death = TRUE;

				/* Mark social class, reset age, if needed */
				if (p_ptr->sc) p_ptr->sc = p_ptr->age = 0;

				/* Increase age */
				p_ptr->age++;

				/* Mark savefile */
				noscore |= 0x0001;
                                msg_print("You invoke wizard mode and cheat death.");
                                msg_print(NULL);
                        }

                        if (cheat_death)
                        {
				/* Restore hit points */
				p_ptr->chp = p_ptr->mhp;
				p_ptr->chp_frac = 0;

                                /* Heal sanity */
                                p_ptr->csane = p_ptr->msane;
                                p_ptr->csane_frac = 0;

        			/* Restore spell points */
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;

                                /* Restore tank points */
                                p_ptr->ctp = p_ptr->mtp;

				/* Hack -- Healing */
				(void)set_blind(0);
				(void)set_confused(0);
				(void)set_poisoned(0);
				(void)set_afraid(0);
				(void)set_paralyzed(0);
				(void)set_image(0);
				(void)set_stun(0);
				(void)set_cut(0);
                                p_ptr->black_breath = FALSE;    /* accounting for a new ailment. -LM- */

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
				(void)strcpy(died_from, "Cheating death");

				/* Do not die */
				death = FALSE;

				/* New depth -KMW- */
				/* dun_level = 0; */
				p_ptr->inside_arena = 0;
				leaving_quest = 0;
				p_ptr->inside_quest = 0;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}

		/* Handle "death" */
		if (death) break;

                /* Mega hack */
                if (dun_level) p_ptr->wild_mode = FALSE;

		/* Make a new level */
                process_hooks(HOOK_NEW_LEVEL, is_quest(dun_level));
		generate_cave();
	}

	/* Close stuff */
	close_game();

	/* Quit */
	quit(NULL);
}
