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
		if (!all && (f3 & (TR3_HEAVY_CURSE))) continue;

		/* Perma-Cursed Items can NEVER be uncursed */
		if (f3 & (TR3_PERMA_CURSE)) continue;

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
 * Curse the players armor
 */
bool curse_armor(void)
{
	object_type *o_ptr;

	char o_name[80];

	/* Curse the body armor */
	o_ptr = &inventory[INVEN_BODY];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);

	/* Already cursed */
	if (cursed_p(o_ptr)) return (FALSE);

	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Attempt a saving throw for artifacts */
	if (artifact_p(o_ptr) && (rand_int(100) < 50))
	{
		/* Cool */
		message_format(MSG_ITEM_RESIST, o_ptr->k_idx, 
			"A terrible balck aura tries to surround %s, but it resists the effects!", o_name);
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		message_format(MSG_ITEM_DAMAGE, o_ptr->k_idx, 
			"A terrible black aura blasts your %s!", o_name);

		/* Blast the armor */
		o_ptr->a_idx = 0;
		o_ptr->e_idx = EGO_BLASTED;
		o_ptr->to_a = 0 - randint(5) - randint(5);
		o_ptr->to_h = 0;
		o_ptr->to_d = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
	}

	return (TRUE);
}

/*
 * Curse the players weapon
 */
bool curse_weapon(void)
{
	object_type *o_ptr;

	char o_name[80];

	/* Curse the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);

	/* Already cursed */
	if (cursed_p(o_ptr)) return (FALSE);

	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Attempt a saving throw */
	if (artifact_p(o_ptr) && (rand_int(100) < 50))
	{
		/* Cool */
		message_format(MSG_ITEM_RESIST, o_ptr->k_idx, 
			"A terrible balck aura tries to surround %s, but it resists the effects!", o_name);
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		message_format(MSG_ITEM_DAMAGE, o_ptr->k_idx, 
			"A terrible black aura blasts your %s!", o_name);

		/* Shatter the weapon */
		o_ptr->a_idx = 0;
		o_ptr->e_idx = EGO_SHATTERED;
		o_ptr->to_h = 0 - randint(5) - randint(5);
		o_ptr->to_d = 0 - randint(5) - randint(5);
		o_ptr->to_a = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
	}

	/* Notice */
	return (TRUE);
}

/*
 * Curse the players equipment (minor)
 */
bool curse_minor(void)
{
	int k;
	int count = 0;
	object_type *o_ptr;

	for (k = INVEN_WIELD; k < INVEN_MUSIC; k++)
	{
		/* Curse the weapon */
		o_ptr = &inventory[k];

		/* Not rings, lites or amulets */
		if ((k == INVEN_LEFT) || (k == INVEN_RIGHT) || (k == INVEN_LITE) || (k == INVEN_NECK))
			continue;

		/* Nothing to curse */
		if (!o_ptr->k_idx) continue;

		/* Already cursed */
		if (cursed_p(o_ptr)) continue;

		/* Artifacts resist */
		if (artifact_p(o_ptr)) continue;

		/* Ego items save */
		if (ego_item_p(o_ptr) && rand_int(100) < 50) continue;

		/* Curse the object */
		if ((k == INVEN_WIELD) || (k == INVEN_BOW))
		{
			o_ptr->to_h -= randint(4);
			o_ptr->to_d -= randint(4);
		}
		else o_ptr->to_a -= randint(4);

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Count it */
		count++;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
	}

	if (count) 
	{
		message(MSG_ITEM_DAMAGE, 0, "A dark aura surrounds your equipment!");
		return (TRUE);
	}

	/* Notice */
	return (FALSE);
}

/*
 * Forget everything
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

		/* Hack -- Clear the "known" flag */
		o_ptr->ident &= ~(IDENT_KNOWN);

		/* Hack -- Clear the "empty" flag */
		o_ptr->ident &= ~(IDENT_EMPTY);
	}

	/* Forget all traps */
	for (i = 0; i < t_max; i++)
	{
		if (!w_info[t_list[i].w_idx].flags & WGF_GLYPH) 
			t_list[i].visible = FALSE;
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
		message(MSG_FAIL, 0, "Nothing happens.");

		/* something happened */
		return;
	}

	/* Activate recall */
	if (!p_ptr->word_recall)
	{
		p_ptr->word_recall = rand_int(20) + 15;
		message(MSG_EFFECT, 0, "The air about you becomes charged...");
	}

	/* Deactivate recall */
	else
	{
		p_ptr->word_recall = 0;
		message(MSG_EFFECT, 0, "A tension leaves the air around you...");
	}

	return;
}

/*
 * Detect all traps on current panel
 */
bool detect_traps(void)
{
	int y, x;
	int i;

	bool detect = FALSE;

	for (i = 1; i < t_max; i++)
	{
		trap_type *t_ptr = &t_list[i];

		/* Skip dead traps */
		if (!t_ptr->w_idx) continue;

		/* Location */
		y = t_ptr->fy;
		x = t_ptr->fx;

		/* Only detect legal traps */
		if (!trap_detectable(y, x)) continue;

		/* Only detect nearby traps */
		if (!panel_contains(y, x)) continue;

		/* Set to visible */
		t_list[i].visible = TRUE;

		detect = TRUE;

		/* Hack -- Memorize */
		cave_info[y][x] |= (CAVE_MARK);

		/* Redraw */
		lite_spot(y, x);
	}
		
	/* Describe */
	if (detect)
	{
		message(MSG_DETECT, 0, "You sense the presence of traps!");
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
	for (y = p_ptr->wy; y < p_ptr->wy+SCREEN_HGT; y++)
	{
		for (x = p_ptr->wx; x < p_ptr->wx+SCREEN_WID; x++)
		{
			/* Detect secret doors */
			if (cave_feat[y][x] == FEAT_SECRET)
			{
				/* Create closed door */
				cave_set_feat(y, x, FEAT_CLOSED);

				if (trap_lock(y, x)) t_list[cave_t_idx[y][x]].visible = TRUE;
			}

			/* Detect doors */
			if ((cave_feat[y][x] == FEAT_OPEN) ||
			    (cave_feat[y][x] == FEAT_CLOSED) ||
			    (cave_feat[y][x] == FEAT_BROKEN))
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
		message(MSG_DETECT, 0, "You sense the presence of doors!");
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
	for (y = p_ptr->wy; y < p_ptr->wy+SCREEN_HGT; y++)
	{
		for (x = p_ptr->wx; x < p_ptr->wx+SCREEN_WID; x++)
		{
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
		message(MSG_DETECT, 0, "You sense the presence of stairs!");
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
	for (y = p_ptr->wy; y < p_ptr->wy+SCREEN_HGT; y++)
	{
		for (x = p_ptr->wx; x < p_ptr->wx+SCREEN_WID; x++)
		{
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
		message(MSG_DETECT, 0, "You sense the presence of buried treasure!");
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
		message(MSG_DETECT, 0, "You sense the presence of treasure!");
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
		message(MSG_DETECT, 0, "You sense the presence of objects!");
	}

	/* Result */
	return (detect);
}

/*
 * Detect all "magic" objects on the current panel.
 *
 * This will light up all spaces with "magic" items, including artifacts,
 * ego-items, potions, scrolls, books, rods, wands, staves, amulets, rings,
 * and "enchanted" items.
 *
 * It can probably be argued that this function is now too powerful.
 */
bool detect_objects_magic(void)
{
	int i, y, x, tv, sv;

	bool found;
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

		found = FALSE;

		/* Examine the object */
		tv = o_ptr->tval;
		sv = o_ptr->sval;

		/* Artifacts, misc magic items, or enchanted wearables */
		switch (tv)
		{
			case TV_AMULET:
			case TV_RING:
			case TV_STAFF:
			case TV_WAND:
			case TV_ROD:
			case TV_TALISMAN:
			case TV_SCROLL:
			case TV_POTION:
			case TV_MAGIC_BOOK:
			case TV_POWDER:
			case TV_LITE_SPECIAL:
			case TV_DRAG_ARMOR:
			{
				found = TRUE;
				break;
			}
			case TV_LITE:
			{
				if (sv >= SV_LANTERN_FIRST_MAGIC) found = TRUE;
				break;
			}
			default:
			{
				if (artifact_p(o_ptr) || ego_item_p(o_ptr)) found = TRUE;
				if ((o_ptr->to_a > 0) || (o_ptr->to_h > 0) || (o_ptr->to_d > 0)) 
					found = TRUE;
				/* Also, cursed items */
				if (cursed_p(o_ptr)) found = TRUE;
				break;
			}
		}

		if (found)
		{
			/* Memorize the item */
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
		message(MSG_DETECT, 0, "You sense the presence of magic objects!");
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
		monster_race *r_ptr;

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		r_ptr = get_monster_real(m_ptr);

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
		message(MSG_DETECT, 0, "You sense the presence of monsters!");
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
		monster_race *r_ptr;

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		r_ptr = get_monster_real(m_ptr);

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/* Detect invisible monsters */
		if (r_ptr->flags2 & (RF2_INVISIBLE))
		{
			/* Update monster recall window */
			if (term_mon_race_idx == m_ptr->r_idx)
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

			/* Take note that they are invisible */
			lore_learn(m_ptr, LRN_FLAG2, RF2_INVISIBLE, FALSE);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
		message(MSG_DETECT, 0, "You sense the presence of invisible creatures!");
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
		monster_race *r_ptr;

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		r_ptr = get_monster_real(m_ptr);

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/* Detect evil monsters */
		if (r_ptr->flags4 & (RF4_EVIL))
		{
			/* Update monster recall window */
			if (term_mon_race_idx == m_ptr->r_idx)
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

			/* Take note */
			lore_learn(m_ptr, LRN_FLAG2, RF2_INVISIBLE, FALSE);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
		message(MSG_DETECT, 0, "You sense the presence of evil creatures!");
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
	if (!cave_valid_bold(p_ptr->py, p_ptr->px))
	{
		message(MSG_FAIL, 0, "The object resists the spell.");
		return;
	}

	/* XXX XXX XXX */
	delete_object(p_ptr->py, p_ptr->px);

	/* Create a staircase */
	if (!p_ptr->depth)
	{
		cave_set_feat(p_ptr->py, p_ptr->px, FEAT_MORE);
	}
	else if ((quest_check(p_ptr->depth) == QUEST_FIXED) || 
			 (quest_check(p_ptr->depth) == QUEST_FIXED_U) || 
			 (p_ptr->depth >= MAX_DEPTH-1))
	{
		cave_set_feat(p_ptr->py, p_ptr->px, FEAT_LESS);
	}
	else if (rand_int(100) < 50)
	{
		cave_set_feat(p_ptr->py, p_ptr->px, FEAT_MORE);
	}
	else
	{
		cave_set_feat(p_ptr->py, p_ptr->px, FEAT_LESS);
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
 * Hook to brand "weapon" (same as before but no bows or digging weapons)
 */
static bool item_tester_hook_brand(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
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
 * Hook to choose spellbooks
 */
bool item_tester_hook_spellbooks(object_type *o_ptr)
{
	return ((o_ptr->tval == TV_MAGIC_BOOK) && (cp_ptr->spell_book[o_ptr->sval])) ;
}

/*
 * Hook to choose spellbooks
 */
bool item_tester_hook_bookmusic(object_type *o_ptr)
{
	if (p_ptr->confused > PY_CONF_CONFUSE) 
	{
		return ((o_ptr->tval == TV_MAGIC_BOOK) && (cp_ptr->spell_book[o_ptr->sval])
		&& (books[o_ptr->sval].flags & SBF_MYSTIC));
	}
	else if (p_ptr->blind || no_lite())
	{
		return (((o_ptr->tval == TV_MUSIC) && (cp_ptr->flags & CF_MUSIC)) ||
			((o_ptr->tval == TV_MAGIC_BOOK) && (cp_ptr->spell_book[o_ptr->sval]) &&
			(books[o_ptr->sval].flags & SBF_MYSTIC)));
	}
	else
	{
		return (((o_ptr->tval == TV_MUSIC) && (cp_ptr->flags & CF_MUSIC)) ||
			((o_ptr->tval == TV_MAGIC_BOOK) && (cp_ptr->spell_book[o_ptr->sval])));
	}
}

/*
 * Hook to specify "armour"
 */
static bool item_tester_hook_armour(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		case TV_BODY_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_HEADGEAR:
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

static bool item_tester_unknown_star(object_type *o_ptr)
{
	if (o_ptr->ident & IDENT_MENTAL)
		return FALSE;
	else
		return TRUE;
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
 * Enchant an item
 *
 * Revamped!  Now takes item pointer, number of times to try enchanting,
 * and a flag of what to try enchanting.  Artifacts resist enchantment
 * some of the time, and successful enchantment to at least +0 might
 * break a curse on the item.  -CFT
 *
 * Max to enchant hit/damage now dependant on item weight. Items weighing 
 * between 100-200 get normal range, lighter items get half range, heavier
 * items get twice the range. Ego items can always be enchanted higher than 
 * non-ego items of the same weight (including artifacts!!). -EZ
 *
 * Note that wfactor is 2 normally, 1 for light weapons, and 4 for heavy
 * weapons.
 *
 * Note that an item can technically be enchanted all the way to +15 if
 * you wait a very, very, long time.  Going from +9 to +10 only works
 * about 5% of the time, and from +10 to +11 only about 1% of the time.
 *
 * Note that this function can now be used on "piles" of items, and
 * the larger the pile, the lower the chance of success.
 */
static bool enchant(object_type *o_ptr, int n, int eflag)
{
	int i, chance, prob;

	bool res = FALSE;

	bool a = artifact_p(o_ptr);

	int calc_d = o_ptr->to_d;

	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Large piles resist enchantment */
	prob = o_ptr->number * 100;

	/* Missiles are easy to enchant */
	if (ammo_p(o_ptr)) prob = prob / 20;
	
	/* Weight affects damage enchantment, except for bows or ammo */
	if (!(ammo_p(o_ptr) || (o_ptr->tval == TV_BOW))) calc_d = (calc_d * 10) / wgt_factor(o_ptr);

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
					(!((f3 & (TR3_PERMA_CURSE)) || (f3 & (TR3_HEAVY_CURSE)))) &&
				    (o_ptr->to_h >= 0) && (rand_int(100) < 25))
				{
					message(MSG_ITEM_BONUS, o_ptr->k_idx, "The curse is broken!");

					/* Uncurse the object */
					uncurse_object(o_ptr);
				}
			}
		}

		/* Enchant to damage */
		if (eflag & (ENCH_TODAM))
		{
			if (calc_d < 0) chance = 0;
			else if (calc_d > 15) chance = 1000;
			else chance = enchant_table[calc_d];

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_d++;

				/* Break curse */
				if (cursed_p(o_ptr) &&
					(!((f3 & (TR3_PERMA_CURSE)) || (f3 & (TR3_HEAVY_CURSE)))) &&
				    (o_ptr->to_d >= 0) && (rand_int(100) < 25))
				{
					message(MSG_ITEM_BONUS, o_ptr->k_idx, "The curse is broken!");

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
					(!((f3 & (TR3_PERMA_CURSE)) || (f3 & (TR3_HEAVY_CURSE)))) &&
				    (o_ptr->to_a >= 0) && (rand_int(100) < 25))
				{
					message(MSG_ITEM_BONUS, o_ptr->k_idx, "The curse is broken!");

					/* Uncurse the object */
					uncurse_object(o_ptr);
				}
			}
		}
	}

	/* Failure */
	if (!res) return (FALSE);

	/* Mark object */
	o_ptr->ident |= IDENT_MODIFIED;

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
	message_format(MSG_ITEM_BONUS, o_ptr->k_idx, "%s %s glow%s brightly!",
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
		message(MSG_FAIL, 0, "The enchantment failed.");
	}

	/* Something happened */
	return (TRUE);
}

/*
 * Brand the current weapon
 */
bool brand_weapon(byte weapon_type, int brand_type, bool add_plus)
{
	int item;
	object_type *o_ptr;
	cptr act, s, q;
	char o_name[80];
	bool ammo;

	if (weapon_type) item_tester_tval = weapon_type;
	else item_tester_hook = item_tester_hook_brand;

	/* Get an item */
	q = "Brand which weapon? ";
	s = "You have no weapon to brand.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return (FALSE);
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

	ammo = (ammo_p(o_ptr));

    /*
	 * Don't enchant artifacts, ego-items, cursed or broken items, or piles of anything except
	 * arrows, bolts, and shots
	 */
	if (artifact_p(o_ptr) || ego_item_p(o_ptr) || cursed_p(o_ptr) || broken_p(o_ptr) ||
		((o_ptr->number > 1) && !ammo))
	{
		/* Flush */
		if (flush_failure) flush();
	
		/* Fail */
		message(MSG_FAIL, 0, "The branding failed.");

		/* Notice */
		return (TRUE);
	}

	object_desc(o_name, o_ptr, FALSE, 0);

	switch (brand_type)
	{
		case EGO_BRAND_POIS:
		case EGO_POISON:
		{
			/* Make sure you don't give an inappropriate brand */
			if (ammo) brand_type = EGO_POISON;
			else brand_type = EGO_BRAND_POIS;
			if (o_ptr->number > 1) act = "are covered in a noxious coating!";
			else act = "is covered in a noxious coating!";
			break;
		}
		case EGO_BRAND_LITE:
		case EGO_AMMO_LITE:
		{
			/* Make sure you don't give an inappropriate brand */
			if (ammo) brand_type = EGO_AMMO_LITE;
			else brand_type = EGO_BRAND_LITE;
			if (o_ptr->number > 1) act = "glow with the strength of the sun!";
			else act = "glows with the strength of the sun!";
			break;
		}
		case EGO_BRAND_DARK:
		case EGO_AMMO_DARK:
		{
			/* Make sure you don't give an inappropriate brand */
			if (ammo) brand_type = EGO_AMMO_DARK;
			else brand_type = EGO_BRAND_DARK;
			if (o_ptr->number > 1) act = "are covered in an aura of darkness!";
			else act = "is covered in an aura of darkness!";
			break;
		}
		case EGO_BRAND_FIRE:
		case EGO_AMMO_FIRE:
		{
			/* Make sure you don't give an inappropriate brand */
			if (ammo) brand_type = EGO_AMMO_FIRE;
			else brand_type = EGO_BRAND_FIRE;
			if (o_ptr->number > 1) act = "are covered in a fiery aura!";
			else act = "is covered in a fiery aura!";
			break;
		}
		case EGO_BRAND_COLD:
		case EGO_AMMO_COLD:
		{
			/* Make sure you don't give an inappropriate brand */
			if (ammo) brand_type = EGO_AMMO_COLD;
			else brand_type = EGO_BRAND_COLD;
			if (o_ptr->number > 1) act = "glow deep, icy blue!";
			else act = "glows deep, icy blue!";
			break;
		}
		case EGO_BRAND_ACID:
		case EGO_AMMO_ACID:
		{
			/* Make sure you don't give an inappropriate brand */
			if (ammo) brand_type = EGO_AMMO_ACID;
			else brand_type = EGO_BRAND_ACID;
			if (o_ptr->number > 1) act = "are covered in an acidic sheen!";
			else act = "is covered in an acidic sheen!";
			break;
		}
		case EGO_BRAND_ELEC:
		case EGO_AMMO_ELEC:
		{
			/* Make sure you don't give an inappropriate brand */
			if (ammo) brand_type = EGO_AMMO_ELEC;
			else brand_type = EGO_BRAND_ELEC;
			if (o_ptr->number > 1) act = "emit a halo of electrical sparks!";
			else act = "emits a halo of electrical sparks!";
			break;
		}
		case EGO_SLAY_EVIL:
		case EGO_HURT_EVIL:
		{
			/* Make sure you don't give an inappropriate brand */
			if (ammo) brand_type = EGO_HURT_EVIL;
			else brand_type = EGO_SLAY_EVIL;
			if (o_ptr->number > 1) act = "are covered in a holy aura!";
			else (act = "is covered in a holy aura!"); 
			break;
		}
		case EGO_SLAY_ANIMAL:
		case EGO_HURT_ANIMAL:
		{
			/* Make sure you don't give an inappropriate brand */
			if (ammo) brand_type = EGO_HURT_ANIMAL;
			else brand_type = EGO_SLAY_ANIMAL;
			if (o_ptr->number > 1) act = "are prepared for the hunt!";
			else (act = "is prepared for the hunt!"); 
			break;
		}
		case EGO_WOUNDING:
		case EGO_SHARPNESS:
		{
			/* Make sure you don't give an inappropriate brand */
			if (ammo) brand_type = EGO_WOUNDING;
			else brand_type = EGO_SHARPNESS;
			if (o_ptr->number > 1) act = "grow sharper and deadlier!";
			else (act = "grows sharper and deadlier!"); 
			break;
		}
		default:
		{
			/* Paranoia */
			brand_type = 0;
			if (o_ptr->number > 1) act = "is unaffected!";
			else (act = "are unaffected!"); 
			break;
		}
	}

	o_ptr->e_idx = brand_type;
	message_format(MSG_ITEM_BONUS, o_ptr->k_idx, "Your %s %s", o_name, act);

	if (add_plus) enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);

	/* Mark object */
	o_ptr->ident |= IDENT_MODIFIED;

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Something happened */
	return (TRUE);
}

/* 
 * Actually identify an item 
 */
static void ident_aux(int item)
{
	int squelch=0;

	object_type *o_ptr;

	char o_name[80];

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

	/* Not an object */
	if (!o_ptr->k_idx) return;

	/* Already identified */
	if (object_aware_p(o_ptr) && object_known_p(o_ptr)) return;

	/* Identify it */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Mark the item as fully known */
	if ((cp_ptr->flags & CF_LORE) && artifact_p(o_ptr)) artifact_known(&a_info[o_ptr->a_idx]);

	/* Mark the artifact as "aware" */
	if (artifact_p(o_ptr)) artifact_aware(&a_info[o_ptr->a_idx]);

	/* Squelch it? */
	if (item < INVEN_WIELD) squelch = squelch_itemp(o_ptr);

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Describe */
	if (item >= INVEN_WIELD)
	{
		message_format(MSG_DESCRIBE, 0, "%^s: %s (%c).",
		           describe_use(item), o_name, index_to_label(item));
	}
	else if (item >= 0)
	{
		message_format(MSG_DESCRIBE, 0, "In your pack: %s (%c).", o_name, index_to_label(item));
	}
	else
	{
		message_format(MSG_DESCRIBE, 0, "On the ground: %s.", o_name);
	}

	/* Now squelch it if needed */
	if (squelch) 
	{
		do_squelch_item(o_ptr);
	} 
	else if (artifact_p(o_ptr))
	{
		artifact_type *a_ptr = &a_info[o_ptr->a_idx];

		/* Describe it fully */
		if artifact_known_p(a_ptr) 
		{
			/* Track the object */
			object_actual_track(o_ptr);

			/* Hack -- Handle stuff */
			handle_stuff();

			screen_object(o_ptr, TRUE);
		}
	}
}

/*
 * Identify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was identified, else FALSE.
 */
bool ident_spell(void)
{
	int item;

	cptr q, s;

	/* Only un-id'ed items */
	item_tester_hook = item_tester_unknown;

	/* Get an item */
	q = "Identify which item? ";
	s = "You have nothing to identify.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Identify it */
	ident_aux(item);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Something happened */
	return (TRUE);
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
		ident_aux(i);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
}

/*
 * Fully "identify" an object in the inventory
 *
 * This routine returns TRUE if an item was identified.
 */
bool identify_fully(void)
{
	int i;
	int item;
	int squelch=0;

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

	/* Learn all alchemical info if potion */
	if (o_ptr->tval == TV_POTION)
	{
		potion_alch[o_ptr->sval].known1 = TRUE;
		potion_alch[o_ptr->sval].known2 = TRUE;

		for (i = 0; i < SV_POTION_MAX; i++)
		{
			if (potion_alch[i].sval1 == o_ptr->sval) potion_alch[i].known1 = TRUE;
			if (potion_alch[i].sval2 == o_ptr->sval) potion_alch[i].known2 = TRUE;
		}
	}

	/* Squelch it? */
	if (item < INVEN_WIELD) squelch = squelch_itemp(o_ptr);

	/* Mark the item as fully known */
	o_ptr->ident |= (IDENT_MENTAL);

	/* Mark the artifact as "aware" */
	if (artifact_p(o_ptr)) 
	{
		artifact_type *a_ptr = &a_info[o_ptr->a_idx];

		/* Aware of the artifact and its abilities */
		artifact_aware(a_ptr);
		artifact_known(a_ptr);
	}

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
		message_format(MSG_DESCRIBE, 0, "%^s: %s (%c).",
		           describe_use(item), o_name, index_to_label(item));
	}
	else if (item >= 0)
	{
		message_format(MSG_DESCRIBE, 0, "In your pack: %s (%c).", o_name, 
			index_to_label(item));
 	}
 	else
 	{
		message_format(MSG_DESCRIBE, 0, "On the ground: %s.", o_name);	
 	}
 
	/* Now squelch it if needed */
	if (squelch == 1) 
	{
		do_squelch_item(o_ptr);
	} 
	else 
	{
		/* Track the object */
		object_actual_track(o_ptr);

		/* Hack -- Handle stuff */
		handle_stuff();

		/* Describe it fully */
		screen_object(o_ptr, TRUE);
	}

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

	/* Hack -- Recharge talismans */
	if (o_ptr->tval == TV_TALISMAN) return (TRUE);

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
	int recharge_strength, recharge_amount;

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
	if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_TALISMAN))
	{
		/* Extract a recharge strength by comparing object level to power. */
		recharge_strength = ((num > lev) ? (num - lev) : 0) / 5;

		/* Back-fire */
		if (rand_int(recharge_strength) == 0)
		{
			/* Hack -- backfire */
			message(MSG_ITEM_DAMAGE, o_ptr->k_idx, "The recharge backfires, draining the rod further!");

			/* Hack -- decharge the rod */
			if (o_ptr->timeout < 10000) o_ptr->timeout = (o_ptr->timeout + 100) * 2;
		}

		/* Recharge */
		else
		{
			/* Recharge amount */
			recharge_amount = (num * damroll(3, 2));

			/* Recharge by that amount */
			if (o_ptr->timeout > recharge_amount)
				o_ptr->timeout -= recharge_amount;
			else
				o_ptr->timeout = 0;
		}
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
			message(MSG_ITEM_DAMAGE, o_ptr->k_idx, "There is a bright flash of light.");

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
		monster_race *r_ptr;

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		r_ptr = get_monster_real(m_ptr);

		/* Skip aggravating monster (or player) */
		if (i == who) continue;

		/* Wake up nearby sleeping monsters, anger calmed monsters */
		if (m_ptr->cdis < MAX_SIGHT * 2)
		{
			/* Wake up */
			if (m_ptr->csleep)
			{
				/* Wake up */
				m_ptr->csleep = 0;
				sleep = TRUE;
			}
			/* Anger */
			if (m_ptr->calmed)
			{
				/* Wake up */
				m_ptr->calmed = 0;
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
	if (speed) message(MSG_EFFECT, 0, "You feel a sudden stirring nearby!");
	else if (sleep) message(MSG_EFFECT, 0, "You hear a sudden stirring in the distance!");
}

/*
 * Delete all non-unique monsters of a given "type" from the level
 */
void genocide(void)
{
	int i;

	char typ;

	/* Mega-Hack -- Get a monster symbol */
	(void)(get_com("Choose a monster race (by symbol) to genocide: ", &typ));

	/* Delete the monsters of that "type" */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr;

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		r_ptr = get_monster_real(m_ptr);

		/* Hack -- Skip Unique Monsters */
		if (m_ptr->u_idx) continue;

		/* Skip "wrong" monsters */
		if (r_ptr->d_char != typ) continue;

		/* Delete the monster */
		delete_monster_idx(i);

		/* Take some damage */
		damage_player(randint(4), "the strain of casting Genocide");
	}
}

/*
 * Delete all nearby (non-unique) monsters
 */
void mass_genocide(void)
{
	int i;

	/* Delete the (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip unique monsters */
		if (m_ptr->u_idx) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > MAX_SIGHT) continue;

		/* Delete the monster */
		delete_monster_idx(i);

		/* Take some damage */
		damage_player(randint(3), "the strain of casting Mass Genocide");
	}	
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
			cave_info[y][x] &= ~(CAVE_ROOM | CAVE_ICKY);

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

				/* Delete traps */
				delete_trap(y, x);

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
		message(MSG_EFFECT, 0, "There is a searing blast of light!");

		/* Blind the player */
		if (!p_ptr->no_blind && !resist_effect(RS_LIT))
		{
			/* Become blind */
			(void)set_blind(p_ptr->blind + 10 + randint(10));
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
			if ((yy == p_ptr->py) && (xx == p_ptr->px)) hurt = TRUE;
		}
	}

	/* First, affect the player (if necessary) */
	if (hurt)
	{
		/* Check around the player */
		for (i = 0; i < 8; i++)
		{
			/* Get the location */
			y = p_ptr->py + ddy_ddd[i];
			x = p_ptr->px + ddx_ddd[i];

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
				message(MSG_EFFECT, 0, "The cave ceiling collapses!");
				break;
			}
			case 2:
			{
				message(MSG_EFFECT, 0, "The cave floor twists in an unnatural way!");
				break;
			}
			default:
			{
				message(MSG_EFFECT, 0, "The cave quakes!");
				message(MSG_EFFECT, 0, "You are pummeled with debris!");
				break;
			}
		}

		/* Hurt the player a lot */
		if (!sn)
		{
			/* Message and damage */
			message(MSG_EFFECT, 0, "You are severely crushed!");
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
					message(MSG_RESIST, 0, "You nimbly dodge the blast!");
					damage = 0;
					break;
				}
				case 2:
				{
					message(MSG_EFFECT, 0, "You are bashed by rubble!");
					damage = damroll(10, 4);
					if (!p_ptr->no_stun) set_stun(p_ptr->stun + randint(50));
					break;
				}
				case 3:
				{
					message(MSG_EFFECT, 0, "You are crushed between the floor and ceiling!");
					damage = damroll(10, 4);
					if (!p_ptr->no_stun) set_stun(p_ptr->stun + randint(50));
					break;
				}
			}

			/* Move player */
			monster_swap(p_ptr->py, p_ptr->px, sy, sx);
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
				monster_race *r_ptr = get_monster_real(m_ptr);

				/* Most monsters cannot co-exist with rock */
				if (!(r_ptr->flags2 & (RF2_KILL_WALL)) &&
				    !(r_ptr->flags2 & (RF2_PASS_WALL)))
				{
					char m_name[80];

					/* Assume not safe */
					sn = 0;

					/* Monster can move to escape the wall */
					if (!(r_ptr->flags2 & (RF2_NEVER_MOVE)))
					{
						/* Look for safety */
						for (i = 0; i < 8; i++)
						{
							/* Get the grid */
							y = yy + ddy_ddd[i];
							x = xx + ddx_ddd[i];

							/* Skip non-empty grids */
							if (!cave_empty_bold(y, x)) continue;

							/* no teleport onto glyph of warding */
							if (trap_monster(y, x) && trap_glyph(y, x)) 
							{
								if (mon_glyph_check(cave_m_idx[yy][xx], y, x)) continue;
							}		

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
					message_format(MSG_MONSTER, m_ptr->r_idx, "%^s wails out in pain!", m_name);

					/* Take damage from the quake */
					damage = (sn ? damroll(4, 8) : (m_ptr->hp + 1));

					/* Monster is certainly awake and upset*/
					m_ptr->csleep = 0;
					m_ptr->calmed = 0;

					/* Apply damage directly */
					m_ptr->hp -= damage;

					/* Delete (not kill) "dead" monsters */
					if (m_ptr->hp < 0)
					{
						/* Message */
						message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is embedded in the rock!", m_name);

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

	/* Important -- no wall on player */
	map[16+p_ptr->py-cy][16+p_ptr->px-cx] = FALSE;

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
			if ((yy == p_ptr->py) && (xx == p_ptr->px)) continue;

			/* Destroy location (if valid) */
			if (cave_valid_bold(yy, xx))
			{
				int feat = FEAT_FLOOR;

				bool floor = cave_floor_bold(yy, xx);

				/* Delete objects */
				delete_object(yy, xx);

				/* Delete traps */
				delete_trap(yy, xx);

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

			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = get_monster_real(m_ptr);

			/* Stupid monsters rarely wake up */
			if (r_ptr->flags1 & (RF1_STUPID)) chance = 10;

			/* Smart monsters always wake up */
			if (r_ptr->flags1 & (RF1_SMART)) chance = 100;

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
					message_format(MSG_MONSTER, m_ptr->r_idx, "%^s wakes up.", m_name);
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
		if (cave_feat[y][x] <= FEAT_FLOOR)
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
static void lite_room(int y1, int x1)
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

	/* Hack --- Have we seen this room before? */
	if (!(room_info[dun_room[y1/BLOCK_HGT][x1/BLOCK_WID]].seen))
	{
		p_ptr->update |= (PU_ROOM_INFO);
		p_ptr->window |= (PW_ROOM_INFO);
	}
}

/*
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
void lite_area(int dam, int rad)
{
	int flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		message(MSG_EFFECT, 0, "You are surrounded by a white light.");
	}

	/* Hook into the "project()" function */
	(void)project(-1, rad, p_ptr->py, p_ptr->px, dam, GF_LITE_WEAK, flg);

	/* Lite up the room */
	lite_room(p_ptr->py, p_ptr->px);

	/* Now, lite them all up at once */
	cave_temp_room_lite();
}

/*
 * Hack -- call darkness around the player
 * Affect all monsters in the projection radius
 */
void unlite_area(int dam, int rad)
{
	int flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		message(MSG_EFFECT, 0, "Darkness surrounds you.");
	}

	/* Hook into the "project()" function */
	(void)project(-1, rad, p_ptr->py, p_ptr->px, dam, GF_DARK_WEAK, flg);

	/* Lite up the room */
	lite_room(p_ptr->py, p_ptr->px);

	/* Now, darken them all at once */
	cave_temp_room_unlite();
}

/*
 * Affect a monster in a specific location (not grids or objects)
 */
bool strike(int typ, int y, int x, int dam, int rad)
{
	int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_JUMP;
	return (project(-1, rad, y, x, dam, typ, flg));
}

/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */
bool fire_ball(int typ, int dir, int dam, int rad)
{
	int ty, tx;

	int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	/* Use the given direction */
	ty = p_ptr->py + 99 * ddy[dir];
	tx = p_ptr->px + 99 * ddx[dir];

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
 * Cast a ball spell which combines several attack types
 * Mega hack - Use same target for all types
 * Another mega hack guaruntees that only one "missile" will be drawn
 */
void fire_ball_combo(int t1, int t2, int t3, int t4, int dir, int dam, int rad)
{
	int i;
	int ty, tx;

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		ty = p_ptr->target_row;
		tx = p_ptr->target_col;
	}
	/* 
	 * No "target" - HACK - find the stopping point here, and not in project(),
	 * to ensure all spells end up at the same place.
	 */
	else
	{
		for (i = 1; i < 99; i++)
		{
			/* Use the given direction */
			ty = p_ptr->py + i * ddy[dir];
			tx = p_ptr->px + i * ddx[dir];

			/* Always stop at non-initial wall grids */
			if (!cave_floor_bold(ty, tx)) break;

			/* Otherwise, stop on a monster */
			if (cave_m_idx[ty][tx] != 0) break;
		}
	}

	/* Analyze the "dir" and the "target".  Hurt items on floor. */
	if (t1)
	{
		(void)project(-1, rad, ty, tx, dam, t1, flg);
		/* Hack - now you can jump there */
		flg |= (PROJECT_JUMP);
	}
	if (t2)
	{
		(void)project(-1, rad, ty, tx, dam, t2, flg);
		/* Hack - now you can jump there */
		flg |= (PROJECT_JUMP);
	}
	if (t3)
	{
		(void)project(-1, rad, ty, tx, dam, t3, flg);
		/* Hack - now you can jump there */
		flg |= (PROJECT_JUMP);
	}
	if (t4)
	{
		(void)project(-1, rad, ty, tx, dam, t4, flg);
	}
}

/*
 * Hack -- apply a "projection()" in a direction (or at the target)
 */
static bool project_hook(int typ, int dir, int dam, int flg)
{
	int ty, tx;

	/* Pass through the target if needed */
	flg |= (PROJECT_THRU);

	/* Use the given direction */
	ty = p_ptr->py + ddy[dir];
	tx = p_ptr->px + ddx[dir];

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

bool lite_line(int dir, int dam)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
	return (project_hook(GF_LITE_WEAK, dir, dam, flg));
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

/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */
bool door_creation(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 1, p_ptr->py, p_ptr->px, 0, GF_MAKE_DOOR, flg));
}

bool wall_creation(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 1, p_ptr->py, p_ptr->px, 0, GF_MAKE_WALL, flg));
}

bool trap_creation(int power)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 1, p_ptr->py, p_ptr->px, power, GF_MAKE_TRAP, flg));
}

bool magic_lock(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 3, p_ptr->py, p_ptr->px, 0, GF_MAGIC_LOCK, flg));
}

bool destroy_doors_touch(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 1, p_ptr->py, p_ptr->px, 0, GF_KILL_DOOR, flg));
}

bool sleep_monsters_touch(int power)
{
	int flg = PROJECT_KILL | PROJECT_HIDE;
	return (project(-1, 1, p_ptr->py, p_ptr->px, power, GF_OLD_SLEEP, flg));
}
