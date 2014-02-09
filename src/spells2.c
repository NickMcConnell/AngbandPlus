/* File: spells2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
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

		/* Heal 0-4 */
		if (num < 5)
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
 * Leave a "glyph of warding" which prevents monster movement.
 * Return TRUE on success
 */
bool warding_glyph(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* XXX XXX XXX */
	if (!cave_clean_bold(py, px))
	{
		char name[80];

		feature_desc(name, sizeof(name), cave_feat[py][px], FALSE, TRUE);

		if (cave_o_idx[py][px]) msg_print("The object resists the spell.");
		else if (cave_any_trap_bold(py,px))
		{
			if (x_list[cave_x_idx[py][px]].x_flags & EF1_GLYPH)
			{
				msg_print("There is already a glyph where you are standing.");
			}
			/* Trap */
			else msg_print("The trap resists the spell.");
		}

		/* Unsuitable terrain */
		else msg_format("The %s resists the spell.", name);

		/* Failure */
		return (FALSE);
	}

	/* Require suitable grid. Ignore monsters */
 	if (!cave_trappable_bold(py, px))
	{
 		char name[80];

		feature_desc(name, sizeof(name), cave_feat[py][px], FALSE, TRUE);

		msg_format("The %s resists the spell.", name);

		/* Failure */
		return (FALSE);
	}

	/* Create a glyph */
	set_effect_glyph(py, px);

	/* Remember this square */
	cave_info[py][px] |= (CAVE_MARK);

	/* Success */
	return (TRUE);
}

/*
 * Place elemental features around the given grid and range
 * Most elemental features are forest grids.
 * Sometimes we can place other elemental grids
 * Return TRUE if it succeeds
 * -DiegoGonzalez-
 */
bool create_elements(int cy, int cx, int range)
{
    int y, x, k;
	u16b feat, feat2 = FEAT_TREE;
    u16b elements[1024];
	u16b n = 0;
	/* Trees are rare */
	bool trees_enabled = one_in_(30);

	/* Process chance to place other features than forest */
	if (one_in_(2))
	{
		/* Traverse dungeon */
		for (y = 1; y <= p_ptr->cur_map_hgt; y++)
		{
			for (x = 1; x <= p_ptr->cur_map_wid; x++)
			{
				/* Must be passable */
				if (!cave_passable_bold(y, x)) continue;

				/* Must be an elemental feature */
				if (!cave_ff3_match(y, x, TERRAIN_MASK)) continue;

				/* Ignore forest */
				if (cave_ff3_match(y, x, ELEMENT_FOREST)) continue;

				/* Ignore non-lake features */
				if (!cave_ff2_match(y, x, (FF2_LAKE | FF2_RIVER))) continue;

				/* Player must be native */
				/* if (!is_player_native(y, x)) continue; */

				/* Array is full? */
				if (n >= N_ELEMENTS(elements))
				{
					/* Replace features sometimes */
					if (one_in_(2)) elements[rand_int(n)] = cave_feat[y][x];
				}
				else
				{
					/* Save the feature */
					elements[n++] = cave_feat[y][x];
				}
			}
		}
		/* Pick a random feature */
		if (n > 0) feat2 = elements[rand_int(n)];
	}



	/* Traverse grids */
    for (y = cy - range; y <= cy + range; y++)
    {
        for (x = cx - range; x <= cx + range; x++)
        {
        	/* Ignore out of range grids */
            if (distance(y, x, cy, cx) > range) continue;

  			/* Ignore invalid grids */
            if (!in_bounds(y, x)) continue;

			/* Ignore perma-walls, stairs, etc. */
            if (cave_ff1_match(y, x, FF1_PERMANENT)) continue;

			/* Ignore vaults/pits */
			if (cave_info[y][x] & (CAVE_ICKY)) continue;

			/* Ignore walls, doors, etc. */
			if (!cave_passable_bold(y, x)) continue;

			/* Ignore grids with monsters */
			if (cave_m_idx[y][x] > 0) continue;

			/* Ignore grids with objects */
			if (cave_o_idx[y][x] != 0) continue;

			/* Reset feature */
			feat = feat2;

			/* Forest grids */
			if (feat == FEAT_TREE)
			{
				/* Roll */
				k = rand_int(100);

				/* Choose forest features. Trees are rare */
				if (trees_enabled && (k < 2)) feat = FEAT_TREE;
				else if (k < 20) feat = FEAT_BUSH;
				else if (k < 30) feat = FEAT_BRAMBLES;
				else if (k < 35) feat = FEAT_FSOIL_D;
				else feat = FEAT_FSOIL;
			}

			/* Put the feature */
            cave_set_feat(y, x, feat);
        }
    }

    /* Success */
	return (TRUE);
}


/*
 * Create a "glacier" that allows LOS but prevents movement and projections
 * Return TRUE on success
 */
bool create_glacier(void)
{
	int y, x;
	int status = FALSE;

	/* Select a grid */
	if (!target_set_interactive(TARGET_GRID, -1, -1)) return (FALSE);

	/* Paranoia */
	if (!p_ptr->target_set) return (FALSE);

	/* Get coordinates */
	y = p_ptr->target_row;
	x = p_ptr->target_col;

	/* Must be in the line of fire */
	if (!player_can_fire_bold(y, x))
	{
		msg_print("That grid is out of your line of fire!");
	}
	/* Must be a passable grid free of monsters */
	else if (!cave_empty_bold(y, x))
	{
		msg_print("The grid resists your spell!");
	}
	/* Valid grid */
	else
	{
		/* Get the index of the first effect of that grid */
		s16b x_idx = cave_x_idx[y][x];

		/* Remove some effects */
		while (x_idx)
		{
			/* Get the effect */
			effect_type *x_ptr = &x_list[x_idx];

			/* Point to the next effect */
			x_idx = x_ptr->next_x_idx;

			/* Remove only the cloud effects */
			if ((x_ptr->x_type == EFFECT_SHIMMERING_CLOUD) ||
				(x_ptr->x_type == EFFECT_LINGERING_CLOUD) ||
				(x_ptr->x_type == EFFECT_PERMANENT_CLOUD))
			{
				delete_effect_idx((s16b)(x_ptr - x_list));
			}
		}

		/* Create the glacier */
		set_effect_glacier(FEAT_GLACIER, y, x, SOURCE_EFFECT, 0);

		/* Show that grid */
		cave_info[y][x] |= (CAVE_MARK);
		light_spot(y, x);

		/* Success */
		status = TRUE;
	}

	/* Reset the target info */
	target_set_monster(0);

	return (status);
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
 * Array of stat "descriptions"
 */
static cptr moria_desc_stat_sus_pos[] =
{
	"You feel weaker for a moment, but it passes.",
	"You have trouble thinking clearly.  But your mind quickly clears.",
	"Your wisdom is sustained.",
	"You feel clumsy for a moment, but it passes.",
	"Your body resists the effects of the disease.",
	"Your skin starts to itch, but instantly feels better."
};

/*
 * Array of stat "descriptions"
 */
static cptr moria_desc_stat_dec_pos[] =
{
	"You feel weaker.",
	"You have trouble thinking clearly.",
	"Your wisdom is drained.",
	"You feel more clumsy.",
	"Your health is damaged!",
	"Your skin starts to itch."
};


/*
 * Lose a "point"
 */
bool do_dec_stat(int stat)
{
	bool sust = FALSE;

	/* Get the "sustain" */
	switch (stat)
	{
		case A_STR: if (p_ptr->state.sustain_str) sust = TRUE; break;
		case A_INT: if (p_ptr->state.sustain_int) sust = TRUE; break;
		case A_WIS: if (p_ptr->state.sustain_wis) sust = TRUE; break;
		case A_DEX: if (p_ptr->state.sustain_dex) sust = TRUE; break;
		case A_CON: if (p_ptr->state.sustain_con) sust = TRUE; break;
		case A_CHR: if (p_ptr->state.sustain_chr) sust = TRUE; break;
	}

	/* Sustain */
	if (sust)
	{
		/* Message */
		if (game_mode == GAME_NPPMORIA) msg_format("%s", moria_desc_stat_sus_pos[stat]);
		else msg_format("You feel very %s for a moment, but the feeling passes.", desc_stat_neg[stat]);

		/* Notice effect */
		return (TRUE);
	}

	/* Attempt to reduce the stat */
	if (dec_stat(stat, 10, FALSE))
	{
		/* Message */
		if (game_mode == GAME_NPPMORIA) msg_format("%s", moria_desc_stat_dec_pos[stat]);
		else message_format(MSG_DRAIN_STAT, stat, "You feel very %s.", desc_stat_neg[stat]);

		/* Notice effect */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}

/*
 * Array of stat "descriptions"
 */
static cptr moria_desc_stat_res_pos[] =
{
	"You feel your strength returning.",
	"Your head spins a moment.",
	"You feel your wisdom returning.",
	"You feel more dextrous.",
	"You feel your health returning.",
	"You feel your looks returning."
};



/*
 * Restore lost "points" in a stat
 */
bool do_res_stat(int stat)
{
	/* Attempt to increase */
	if (res_stat(stat))
	{
		/* Message */
		if (game_mode == GAME_NPPMORIA) msg_format("%s", moria_desc_stat_res_pos[stat]);
		else msg_format("You feel less %s.", desc_stat_neg[stat]);

		/* Notice */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}

/*
 * Array of stat "descriptions"
 */
static cptr moria_desc_stat_inc_pos[] =
{
	"Wow!  What bulging muscles!",
	"Aren't you brilliant!",
	"You suddenly have a profound thought!",
	"You feel more limber!",
	"You feel tingly for a moment.",
	"Gee, ain't you cute!"
};



/*
 * Gain a "point" in a stat
 */
bool do_inc_stat(int stat)
{
	bool res;

	/* Restore stat first */
	res = res_stat(stat);

	/* Attempt to increase */
	if (inc_stat(stat))
	{
		/* Message */
		if (game_mode == GAME_NPPMORIA) msg_format("%s", moria_desc_stat_inc_pos[stat]);
		else msg_format("You feel very %s!", desc_stat_pos[stat]);

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
 * Permanently gain a "point" in a stat
 */
void do_perm_stat_boost(int stat)
{
	/* Restore stat first */
	(void)res_stat(stat);

	/* Increase stat*/
	p_ptr->stat_quest_add[stat]++;

	/* Message */
	msg_print(format("You feel very %s!", desc_stat_pos[stat]));

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redisplay the stats later */
	p_ptr->redraw |= (PR_STATS);
}

/*
 * Identify everything being carried.
 * Done by a potion of "self knowledge".
 */
void identify_pack(void)
{
	int i;

	/* Simply identify and know every item */
	for (i = 0; i < ALL_INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		bool aware = FALSE;

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Remember awareness */
		if (object_aware_p(o_ptr)) aware = TRUE;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);

		if (!aware) apply_autoinscription(o_ptr);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS | PU_NATIVE);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

	/* Redraw stuff */
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

}



#define ENCHANT_MAX 9


/*
 * Used by the "enchant" function (chance of failure)
 */
static const int enchant_table[ENCHANT_MAX + 1] =
{
	0, 20,  50, 150, 300,
	450, 600, 750,
	900, 990
};


/*
 * Hack -- Removes curse from an object.
 */
void uncurse_object(object_type *o_ptr)
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
 * \param heavy removes heavy curses if true
 *
 * \returns number of items uncursed
 */
static int remove_curse_aux(bool heavy)
{
	int i, cnt = 0;

	/* Attempt to uncurse items being worn */
	for (i = INVEN_WIELD; i < ALL_INVEN_TOTAL; i++)
	{
		u32b f1, f2, f3, fn;

		object_type *o_ptr = &inventory[i];
		char o_name[80];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Uncursed already */
		if (!cursed_p(o_ptr)) continue;

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3, &fn);

		/* Heavily Cursed Items need a special spell */
		if (!heavy && (f3 & (TR3_HEAVY_CURSE))) continue;

		/* Perma-Cursed Items can NEVER be uncursed */
		if (f3 & (TR3_PERMA_CURSE)) continue;

		/* Uncurse the object */
		uncurse_object(o_ptr);

		object_desc(o_name, sizeof(o_name), o_ptr, ODESC_BASE);

		msg_format("The curse on your %s is broken!", o_name);

		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Count the uncursings */
		cnt++;
	}
	/* Combine and re-order the pack - and redraw stuff */
	if (cnt)
	{
		p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_ITEMLIST);
	}

	else msg_print("Nothing happens.");

	/* Return "something uncursed" */
	return (cnt);
}


/*
 * Remove most curses
 */
bool remove_curse(bool heavy)
{
	return (remove_curse_aux(heavy));
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
 *
 * See also "identify_fully()".
 *
 * Use the "roff()" routines, perhaps.  XXX XXX XXX
 *
 * Use the "show_file()" method, perhaps.  XXX XXX XXX
 *
 * This function cannot display more than 20 lines.  XXX XXX XXX
 */
void self_knowledge(void)
{
	int i = 0, j, k;

	u32b f1 = 0L, f2 = 0L, f3 = 0L, fn = 0L;

	object_type *o_ptr;

	cptr info[135];

	/* Get item flags from equipment */
	for (k = INVEN_WIELD; k < INVEN_TOTAL; k++)
	{
		u32b t1, t2, t3, tn;

		o_ptr = &inventory[k];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		if ((adult_swap_weapons) && (k == INVEN_SWAP_WEAPON)) continue;

		/* Extract the flags */
		object_flags(o_ptr, &t1, &t2, &t3, &tn);

		/* Extract flags */
		f1 |= t1;
		f2 |= t2;
		f3 |= t3;
		fn |= tn;
	}

	if (cp_ptr->flags & CF_BLESS_WEAPON)
	{
		info[i++] = "You are only comfortable wielding blunt weapons or blessed weapons.";
	}

	if (cp_ptr->flags & CF_CUMBER_GLOVE)
	{
		info[i++] = "You are only comfortable wearing gloves that aid your ability to move freely or increase your dexterity.";
	}

	if (cp_ptr->flags & CF_ROGUE_COMBAT)
	{
		info[i++] = "You can sometimes steal objects and gold from monsters.";
		info[i++] = "You are extraordinally precise with throwing weapons.";
		info[i++] = "You do an extraordinary amount of damage when attacking sleeping monsters.";
		info[i++] = "You are extraordinally precise and deadly when using a sling.";
	}

	if (cp_ptr->flags & CF_SET_TRAPS)
	{
		info[i++] = "You can set traps.";
	}

	if (cp_ptr->flags & CF_EXTRA_ATTACK)
	{
		if (p_ptr->lev >= LEV_EXTRA_COMBAT) info[i++] = "Your attacking speed is naturally increased.";
		else info[i++] = "After you gain more experience, your attacking speed will be naturally increased.";
	}

	if (cp_ptr->flags & CF_EXTRA_SHOT)
	{
		if (p_ptr->lev >= LEV_EXTRA_COMBAT) info[i++] = "Your shooting speed is increased when using a sling.";
		else info[i++] = "After you gain more experience, your shooting speed will be increased when using a sling.";
	}

	if (cp_ptr->flags & CF_EXTRA_ARROW)
	{
		if (p_ptr->lev >= LEV_EXTRA_COMBAT) info[i++] = "Your shooting speed is increased when using a bow.";
		else info[i++] = "After you gain more experience, your shooting speed will be increased when using a bow.";
	}

	if (cp_ptr->flags & CF_BRAVERY_30)
	{
		if (p_ptr->lev >= LEV_BRAVERY) info[i++] = "You are naturally resistant to fear.";
		else info[i++] = "After you gain more experience, you will be naturally resistant to fear.";
	}

	if (cp_ptr->flags & CF_BRIGAND_COMBAT)
	{
		if (p_ptr->lev >= LEV_RES_POIS) info[i++] = "You are naturally resistant to poison.";
		else info[i++] = "After you gain more experience, you will be naturally resistant to poison.";
	}

	if (p_ptr->timed[TMD_BLIND])
	{
		info[i++] = "You cannot see.";
	}
	if (p_ptr->timed[TMD_CONFUSED])
	{
		info[i++] = "You are confused.";
	}
	if (p_ptr->timed[TMD_AFRAID])
	{
		info[i++] = "You are terrified.";
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
	if (p_ptr->timed[TMD_IMAGE])
	{
		info[i++] = "You are hallucinating.";
	}

	if (p_ptr->state.aggravate)
	{
		info[i++] = "You aggravate monsters.";
	}
	if (p_ptr->state.teleport)
	{
		info[i++] = "Your position is very uncertain.";
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
	if (p_ptr->timed[TMD_PROTEVIL])
	{
		info[i++] = "You are protected from evil.";
	}
	if (p_ptr->timed[TMD_SHIELD])
	{
		info[i++] = "You are protected by a mystic shield.";
	}
	if (p_ptr->timed[TMD_INVULN])
	{
		info[i++] = "You are temporarily invulnerable.";
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
	if (p_ptr->state.see_infra)
	{
		info[i++] = "Your eyes are sensitive to infrared light.";
	}

	if (p_ptr->state.slow_digest)
	{
		info[i++] = "Your appetite is small.";
	}
	if (p_ptr->state.ffall)
	{
		info[i++] = "You land gently.";
	}
	if (p_ptr->timed[TMD_FLYING])
	{
		info[i++] = "You are flying.";
	}
	if (p_ptr->state.light)
	{
		info[i++] = "You are glowing with light.";
	}
	if (p_ptr->state.regenerate)
	{
		info[i++] = "You regenerate quickly.";
	}
	if (p_ptr->state.telepathy)
	{
		info[i++] = "You have ESP.";
	}
	if (p_ptr->state.see_inv)
	{
		info[i++] = "You can see invisible creatures.";
	}
	if (p_ptr->state.free_act)
	{
		info[i++] = "You have free action.";
	}
	if (p_ptr->state.hold_life)
	{
		info[i++] = "You have a firm hold on your life force.";
	}

	if (p_ptr->state.immune_acid)
	{
		info[i++] = "You are completely immune to acid.";
	}
	else if ((p_ptr->state.resist_acid) && (p_ptr->timed[TMD_OPP_ACID]))
	{
		info[i++] = "You resist acid exceptionally well.";
	}
	else if ((p_ptr->state.resist_acid) || (p_ptr->timed[TMD_OPP_ACID]))
	{
		info[i++] = "You are resistant to acid.";
	}

	if (p_ptr->state.immune_elec)
	{
		info[i++] = "You are completely immune to lightning.";
	}
	else if ((p_ptr->state.resist_elec) && (p_ptr->timed[TMD_OPP_ELEC]))
	{
		info[i++] = "You resist lightning exceptionally well.";
	}
	else if ((p_ptr->state.resist_elec) || (p_ptr->timed[TMD_OPP_ELEC]))
	{
		info[i++] = "You are resistant to lightning.";
	}

	if (p_ptr->state.immune_fire)
	{
		info[i++] = "You are completely immune to fire.";
	}
	else if ((p_ptr->state.resist_fire) && (p_ptr->timed[TMD_OPP_FIRE]))
	{
		info[i++] = "You resist fire exceptionally well.";
	}
	else if ((p_ptr->state.resist_fire) || (p_ptr->timed[TMD_OPP_FIRE]))
	{
		info[i++] = "You are resistant to fire.";
	}

	if (p_ptr->state.immune_cold)
	{
		info[i++] = "You are completely immune to cold.";
	}
	else if ((p_ptr->state.resist_cold) && (p_ptr->timed[TMD_OPP_COLD]))
	{
		info[i++] = "You resist cold exceptionally well.";
	}
	else if ((p_ptr->state.resist_cold) || (p_ptr->timed[TMD_OPP_COLD]))
	{
		info[i++] = "You are resistant to cold.";
	}

	if (p_ptr->state.immune_pois)
	{
		info[i++] = "You are completely immune to poison.";
	}
	else if ((p_ptr->state.resist_pois) && (p_ptr->timed[TMD_OPP_POIS]))
	{
		info[i++] = "You resist poison exceptionally well.";
	}
	else if ((p_ptr->state.resist_pois) || (p_ptr->timed[TMD_OPP_POIS]))
	{
		info[i++] = "You are resistant to poison.";
	}


	if (p_ptr->state.resist_fear)
	{
		info[i++] = "You are completely fearless.";
	}

	if (p_ptr->state.resist_light)
	{
		info[i++] = "You are resistant to bright light.";
	}
	if (p_ptr->state.resist_dark)
	{
		info[i++] = "You are resistant to darkness.";
	}
	if (p_ptr->state.resist_blind)
	{
		info[i++] = "Your eyes are resistant to blindness.";
	}
	if (p_ptr->state.resist_confu)
	{
		info[i++] = "You are resistant to confusion attacks.";
	}
	if (p_ptr->state.resist_sound)
	{
		info[i++] = "You are resistant to sonic attacks.";
	}
	if (p_ptr->state.resist_shard)
	{
		info[i++] = "You are resistant to blasts of shards.";
	}
	if (p_ptr->state.resist_nexus)
	{
		info[i++] = "You are resistant to nexus attacks.";
	}
	if (p_ptr->state.resist_nethr)
	{
		info[i++] = "You are resistant to nether forces.";
	}
	if (p_ptr->state.resist_chaos)
	{
		info[i++] = "You are resistant to chaos.";
	}
	if (((p_ptr->state.resist_confu) && (!p_ptr->state.resist_chaos)) ||
		((!p_ptr->state.resist_confu) && (p_ptr->state.resist_chaos)))
	{
		info[i++] = "You are resistant to being confused.";
	}
	if (p_ptr->state.resist_disen)
	{
		info[i++] = "You are resistant to disenchantment.";
	}
	if (p_ptr->p_native_known & P_NATIVE_LAVA)
	{
		info[i++] = "You are native to lava.";
	}
	if (p_ptr->p_native_known & P_NATIVE_ICE)
	{
		info[i++] = "You are native to ice.";
	}
	if (p_ptr->p_native_known & P_NATIVE_OIL)
	{
		info[i++] = "You are native to oil.";
	}
	if (p_ptr->p_native_known & P_NATIVE_FIRE)
	{
		info[i++] = "You are native to fire.";
	}
	if (p_ptr->p_native_known & P_NATIVE_SAND)
	{
		info[i++] = "You are native to sand.";
	}
	if (p_ptr->p_native_known & P_NATIVE_FOREST)
	{
		info[i++] = "You are native to forests.";
	}
	if (p_ptr->p_native_known & P_NATIVE_WATER)
	{
		info[i++] = "You are native to water.";
	}
	if (p_ptr->p_native_known & P_NATIVE_ACID)
	{
		info[i++] = "You are native to acid.";
	}
	if (p_ptr->p_native_known & P_NATIVE_MUD)
	{
		info[i++] = "You are native to mud.";
	}
	if ((p_ptr->p_native_known & ELEMENT_BWATER) == ELEMENT_BWATER)
	{
		info[i++] = "You are native to boiling water.";
	}
	if ((p_ptr->p_native_known & ELEMENT_BMUD) == ELEMENT_BMUD)
	{
		info[i++] = "You are native to boiling mud.";
	}
	if (p_ptr->state.sustain_str)
	{
		info[i++] = "Your strength is sustained.";
	}
	if (p_ptr->state.sustain_int)
	{
		info[i++] = "Your intelligence is sustained.";
	}
	if (p_ptr->state.sustain_wis)
	{
		info[i++] = "Your wisdom is sustained.";
	}
	if (p_ptr->state.sustain_con)
	{
		info[i++] = "Your constitution is sustained.";
	}
	if (p_ptr->state.sustain_dex)
	{
		info[i++] = "Your dexterity is sustained.";
	}
	if (p_ptr->state.sustain_chr)
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
	if (f1 & (TR1_SEARCH))
	{
		info[i++] = "Your searching ability is affected by your equipment.";
	}
	if (f1 & (TR1_INFRA))
	{
		info[i++] = "Your infravision is affected by your equipment.";
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

	/* Get the current weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Analyze the weapon */
	if (obj_is_weapon(o_ptr))
	{
		/* Special "Attack Bonuses" */
		if (f1 & (TR1_BRAND_ACID))
		{
			info[i++] = "Your weapon melts your foes.";
		}
		if (f1 & (TR1_BRAND_ELEC))
		{
			info[i++] = "Your weapon shocks your foes.";
		}
		if (f1 & (TR1_BRAND_FIRE))
		{
			info[i++] = "Your weapon burns your foes.";
		}
		if (f1 & (TR1_BRAND_COLD))
		{
			info[i++] = "Your weapon freezes your foes.";
		}
		if (f1 & (TR1_BRAND_POIS))
		{
			info[i++] = "Your weapon poisons your foes.";
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
			info[i++] = "Your weapon strikes at undead with holy wrath.";
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
bool set_recall(void)
{
	/* Ironman */
	if (adult_ironman && !p_ptr->total_winner)
	{
		msg_print("Nothing happens.");
		return (FALSE);
	}

	/* Verify leaving normal quest level */
	if (verify_leave_quest)
	{
		char out_val[160];

		if (quest_might_fail_if_leave_level())
		{
			sprintf(out_val, "Really risk failing your quest? ");
			if (!get_check(out_val)) return (FALSE);
		}

		/* Verify leaving normal quest level */
		else if (quest_shall_fail_if_leave_level())
		{
			sprintf(out_val, "Really fail your quest? ");
			if (!get_check(out_val)) return (FALSE);
		}
	}

	/* Activate recall */
	if (!p_ptr->word_recall)
	{
		/* Reset recall depth */
		if ((p_ptr->depth > 0) && (p_ptr->depth != p_ptr->recall_depth) && (game_mode != GAME_NPPMORIA))
		{
			/*
			 * ToDo: Add a new player_type field "recall_depth"
			 * ToDo: Poll: Always reset recall depth?
			 */
			 if (get_check("Reset recall depth? "))
				p_ptr->recall_depth = p_ptr->depth;
		}

		p_ptr->word_recall = rand_int(20) + 15;
		msg_print("The air about you becomes charged...");
	}

	/* Deactivate recall */
	else
	{
		p_ptr->word_recall = 0;
		msg_print("A tension leaves the air around you...");
	}

	/* Redraw status line */
	p_ptr->redraw = PR_STATUS;
	handle_stuff();

	return (TRUE);
}


/*
 * Hack - displays areas effected by detection spells.
 *
 */
static void animate_detect(int rad)
{
	int x, y;

	byte a = TERM_YELLOW;
	char c = '*';

	int dist_squared = rad * rad;

	/* Special tile for DVG graphics */
	if ((use_graphics) && (arg_graphics == GRAPHICS_DAVID_GERVAIS))
	{
		a = (byte)0x81;
		c = (char)0xE3;
	}

	/* Scan the map */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			int py = p_ptr->py - y;
			int px = p_ptr->px - x;

			/*
			 * Ensure we are inside the detection radius (using pythagorean's theorum)
			 * Also ensure we are onscreen
			 */
			if (((px * px + py * py) <= dist_squared) && (panel_contains(y, x)))
			{
               	/* Hack -- Visual effects -- Display a yellow star */
               	print_rel(c, a, y, x);
			}
		}
	}

	(void)Term_fresh();

	/* Delay (briefly) */
	Term_xtra(TERM_XTRA_DELAY, 150);

	/* Now erase the effect */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			if (panel_contains(y, x)) light_spot(y, x);
		}
	}

	(void)Term_fresh();
 }



/*
 * Detect any trap on a square
 */
static bool detect_traps(int y, int x)
{
	/* Mark as trap-detected */
	cave_info[y][x] |= (CAVE_DTRAP);

	/* Detect traps. Avoid glyphs */
	if (cave_player_trap_bold(y, x) ||
		cave_monster_trap_bold(y, x))
	{
		effect_type *x_ptr = &x_list[cave_x_idx[y][x]];

		/* Hack -- Memorize */
		x_ptr->x_flags &= ~(EF1_HIDDEN);

		/* Hack -- Memorize */
		cave_info[y][x] |= (CAVE_MARK);

		return (TRUE);
	}

	/* Result */
	return (FALSE);
}



/*
 * Detect any door on a square
 */
static bool detect_doors(int y, int x)
{

	/* Detect doors */
	if (cave_door_bold(y, x))
	{

		/* Detect secret doors */
		if (cave_ff1_match(y, x, FF1_SECRET))
		{
			/* Place a door */
			cave_alter_feat(y, x, FS_SECRET);
		}

		/* Hack -- Memorize */
		cave_info[y][x] |= (CAVE_MARK);

		return (TRUE);
	}

	/* Result */
	return (FALSE);
}


/*
 * Detect any stair on a square
 */
static bool detect_stairs(int y, int x)
{

	/* Detect stairs */
	if (cave_stair_bold(y, x))
	{
		/* Find secrets */
		if (cave_ff1_match(y, x, FF1_SECRET))
		{
			cave_alter_feat(y, x, FS_SECRET);
		}

		/* Hack -- Memorize */
		cave_info[y][x] |= (CAVE_MARK);

		return (TRUE);
	}

	/* Result */
	return (FALSE);
}






/*
 * Detect if there is treasure on a square
 */
static bool detect_treasure(int y, int x)
{
	if (cave_ff1_match(y, x, FF1_HAS_GOLD))
	{
		/* Detect secrets */
		if (cave_ff1_match(y, x, FF1_SECRET))
		{

			/*Find secrets*/
			cave_alter_feat(y, x, FS_SECRET);
		}

		/* Hack -- Memorize */
		cave_info[y][x] |= (CAVE_MARK);

		return (TRUE);
	}

	/* Result */
	return (FALSE);
}



/*
 * Detect if there are "gold" objects on a square
 */
static bool detect_objects_gold(int y, int x)
{
	s16b this_o_idx, next_o_idx = 0;

	bool detect = FALSE;

	/* Nothing there */
	if ((!cave_o_idx[y][x]) && (cave_m_idx[y][x]) < 1) return (FALSE);

	/* Check all objects on the screen */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Detect "gold" objects */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Hack -- memorize it */
			o_ptr->marked = TRUE;

			/* Detect */
			detect = TRUE;
		}
	}

	/* Result */
	return (detect);
}

/*
 * Detect if there are "normal" objects on a square
 */
static bool detect_objects_normal(int y, int x)
{
	s16b this_o_idx, next_o_idx = 0;

	bool detect = FALSE;

	/* Nothing there */
	if ((!cave_o_idx[y][x]) && (cave_m_idx[y][x] < 1)) return (FALSE);

	/* Check all objects on the screen */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Detect "real" objects */
		if (o_ptr->tval != TV_GOLD)
		{
			/* Hack -- memorize it */
			o_ptr->marked = TRUE;

			/* Detect */
			detect = TRUE;

			p_ptr->redraw |= (PR_ITEMLIST);
		}
	}

	return (detect);
}



/*
 * Detect all "magic" objects or mimics on a square.
 */
static bool detect_objects_magic(int y, int x)
{
	s16b this_o_idx, next_o_idx = 0;
	int tv;

	bool detect = FALSE;

	/* Nothing there */
	if ((!cave_o_idx[y][x]) && (cave_m_idx[y][x]) < 1) return (FALSE);

	/* Check all objects on the screen */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Examine the tval */
		tv = o_ptr->tval;

		/* Artifacts, misc magic items, or enchanted wearables */
		if (artifact_p(o_ptr) || ego_item_p(o_ptr) ||
		    (tv == TV_AMULET) || (tv == TV_RING) ||
		    (tv == TV_STAFF) || (tv == TV_WAND) || (tv == TV_ROD) ||
		    (tv == TV_SCROLL) || (tv == TV_POTION) ||
		    (tv == TV_MAGIC_BOOK) || (tv == TV_PRAYER_BOOK) || (tv == TV_DRUID_BOOK) ||
		    ((o_ptr->to_a > 0) || (o_ptr->to_h + o_ptr->to_d > 0)))
		{
			/* Hack -- memorize it */
			o_ptr->marked = TRUE;

			p_ptr->redraw |= (PR_ITEMLIST);

			detect = TRUE;
		}
	}

	/* Return result */
	return (detect);
}


/*
 * Detect a "normal" monsters on a specific square
 */
static bool detect_monsters_normal(int y, int x)
{
	/* No monster on this square */
	if (cave_m_idx[y][x] > 0)
	{

		monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) return (FALSE);

		/* Detect all non-invisible monsters */
		if (r_ptr->flags2 & (RF2_INVISIBLE)) return (FALSE);

		/* Optimize -- Repair flags */
		repair_mflag_mark = TRUE;
		repair_mflag_show = TRUE;

		/* Hack -- Detect the monster */
		m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

		/* Update the monster */
		update_mon(cave_m_idx[y][x], FALSE);

		return (TRUE);
	}

	/* Result */
	return (FALSE);
}

/*
 * Detect all "living" monsters on the current panel, visible and invisible.
 */
static bool detect_monsters_living(int y, int x)
{
	/* No monster on this square */
	if (cave_m_idx[y][x] > 0)
	{

		monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) return (FALSE);

		/*Only detect living monsters*/
		if (monster_nonliving(r_ptr)) return (FALSE);

		/* Optimize -- Repair flags */
		repair_mflag_mark = TRUE;
		repair_mflag_show = TRUE;

		/* Hack -- Detect the monster */
		m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

		/* Update the monster */
		update_mon(cave_m_idx[y][x], FALSE);

		return (TRUE);

	}

	/* Result */
	return (FALSE);
}


/*
 * Detect all "invisible" monsters on current panel
 */
static bool detect_monsters_invis(int y, int x)
{
	/* No monster on this square */
	if (cave_m_idx[y][x] > 0)
	{

		monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) return (FALSE);

		/* Detect invisible monsters */
		if (r_ptr->flags2 & (RF2_INVISIBLE))
		{

			/* Take note that they are invisible */
			l_ptr->r_l_flags2 |= (RF2_INVISIBLE);

			/* Optimize -- Repair flags */
			repair_mflag_mark = TRUE;
			repair_mflag_show = TRUE;

			/* Hack -- Detect the monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(cave_m_idx[y][x], FALSE);

			return (TRUE);
		}
	}

	/* Result */
	return (FALSE);
}



/*
 * Detect all "evil" monsters on current panel
 */
static bool detect_monsters_evil(int y, int x)
{
	/* No monster on this square */
	if (cave_m_idx[y][x] > 0)
	{

		monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) return (FALSE);

		/* Detect evil monsters */
		if (r_ptr->flags3 & (RF3_EVIL))
		{
			/* Take note that they are evil */
			l_ptr->r_l_flags3 |= (RF3_EVIL);

			/* Optimize -- Repair flags */
			repair_mflag_mark = TRUE;
			repair_mflag_show = TRUE;

			/* Detect the monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(cave_m_idx[y][x], FALSE);

			return (TRUE);

		}
	}

	/* Result */
	return (FALSE);
}

/*
 * Detect Terrain
 */
static bool detect_terrain(int y, int x)
{

	/* Check the terrain*/
	if (feat_ff3_match(cave_feat[y][x], TERRAIN_MASK))
	{
		/* Memorize the grid */
		cave_info[y][x] |= (CAVE_MARK | CAVE_GLOW);

		/* We have seen the feature */
		f_info[cave_feat[y][x]].f_everseen = TRUE;
	}

	else return FALSE;

	return (TRUE);
}

/*
 * Detect Terrain
 */
static bool detect_map(int y, int x)
{
	int i;

	/* All non-walls are "checked"*/
	if (!(f_info[cave_feat[y][x]].f_flags1 & (FF1_WALL)))
	{
		/* Memorize normal features */
		if (f_info[cave_feat[y][x]].f_flags1 & (FF1_REMEMBER))
		{
			/* Memorize the object */
			cave_info[y][x] |= (CAVE_MARK);
		}

		/* Memorize known walls */
		for (i = 0; i < 8; i++)
		{
			int yy = y + ddy_ddd[i];
			int xx = x + ddx_ddd[i];

			/* Memorize walls (etc) */
			if (f_info[cave_feat[yy][xx]].f_flags1 & (FF1_REMEMBER))
			{
				/* Memorize the walls */
				cave_info[yy][xx] |= (CAVE_MARK);
			}
		}
	}

	else return (FALSE);

	return (TRUE);
}

/*
 * Struct of sidebar handlers.
 */
static const struct detect_handler_t
{
	u16b detect_type;
	bool (*hook)(int, int);	 /* int y, int x */
	const char *detect_message;
} detect_handlers[] =
{
	{DETECT_INVISIBLE, 	detect_monsters_invis, 	"You sense the presence of invisible creatures!"},
	{DETECT_EVIL, 		detect_monsters_evil, 	"You sense the presence of evil creatures!"},
	{DETECT_LIFE, 		detect_monsters_living, "You sense the presence of living creatures!"},
	{DETECT_MONSTERS, 	detect_monsters_normal, "You sense the presence of monsters!"},
	{DETECT_GOLD, 		detect_objects_gold, 	"You sense the presence of treasure!"},
	{DETECT_TREASURE, 	detect_treasure, 		"You sense the presence of buried treasure!"},
	{DETECT_ENCHANTMENT,detect_objects_magic, 	"You sense the presence of magic objects!"},
	{DETECT_OBJECTS, 	detect_objects_normal, 	"You sense the presence of objects!"},
	{DETECT_DOORS, 		detect_doors, 			"You sense the presence of doors!"},
	{DETECT_STAIRS, 	detect_stairs, 			"You sense the presence of stairs!"},
	{DETECT_TERRAIN, 	detect_terrain, 		"You sense the presence of unusual terrain!"},
	{DETECT_TRAPS, 		detect_traps, 			"You sense the presence of traps!"},
	{DETECT_MAP, 		detect_map, 			"You sense the dungeon around you!"},

};

bool detect(int dist, u16b detect_checks)
{
	u16b detect_type_found = 0L;
	int y, x;
	u16b i;
	bool refresh = FALSE;

	/* Square the distance for later use */
	int dist_squared = dist * dist;

	/* Show the player the highlighted region */
	animate_detect(dist);

	/* Hack - always refresh map and statusline if detect_traps is called */
	if (detect_checks & (DETECT_TRAPS))
	{
		refresh = TRUE;

		/* Update the detect statusline */
		p_ptr->redraw |= (PR_DTRAP);
	}

	/* Some detects need the map re-drawn. */
	if (detect_checks & (DETECT_MAP | DETECT_TERRAIN)) p_ptr->redraw |= (PR_MAP);

	/* Go through and check all of the applicable detection functions */
	for (i = 0; i < N_ELEMENTS(detect_handlers); i++)
	{
		const struct detect_handler_t *dtc = &detect_handlers[i];

		/* We aren't trying to detect this one, continue */
		if (!(detect_checks & (dtc->detect_type))) continue;

		/* Scan the map */
		for (y = 0; y < p_ptr->cur_map_hgt; y++)
		{
			for (x = 0; x < p_ptr->cur_map_wid; x++)
			{
				int py = p_ptr->py - y;
				int px = p_ptr->px - x;

				/* Ensure we are inside the detection radius (using pythagorean's theorum) */
				if ((px * px + py * py) > dist_squared) continue;

				/* We detected something */
				if (dtc->hook(y, x))
				{
					/* Mark it so we can print out the message when we are done */
					detect_type_found |= (dtc->detect_type);
				}
			}
		}
	}

	/* Nothing found */
	if ((!detect_type_found) && (!refresh)) return (FALSE);

	handle_stuff();

	/* Re-draw the map */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			if (panel_contains(y, x)) light_spot(y, x);
		}
	}

	/* Print out the messages */
	for (i = 0; i < N_ELEMENTS(detect_handlers); i++)
	{
		const struct detect_handler_t *dtc = &detect_handlers[i];

		/* We aren't trying to detect this one, continue */
		if (!(detect_type_found & (dtc->detect_type))) continue;

		msg_format("%s", dtc->detect_message);
	}

	return (TRUE);
}

/*
 * Create stairs at the player location
 */
void stair_creation(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* XXX XXX XXX */
	if (!cave_valid_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return;
	}

	/* XXX XXX XXX */
	delete_object(py, px);

	place_random_stairs(py, px);
}

/*
 * Hook to specify "weapon"
 */
bool item_tester_hook_wieldable_ided_weapon(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		{
			if (object_known_p(o_ptr)) return (TRUE);
			else return (FALSE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "weapon"
 */
bool item_tester_hook_wieldable_weapon(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "weapon"
 */
bool item_tester_hook_weapon(const object_type *o_ptr)
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
 * Hook to specify "weapon"
 */
bool item_tester_hook_ided_weapon(const object_type *o_ptr)
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
			if (object_known_p(o_ptr)) return (TRUE);
			else return (FALSE);
		}
	}

	return (FALSE);
}



/*
 * Hook to specify "armour"
 */
bool item_tester_hook_ided_armour(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		case TV_DRAG_SHIELD:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			if (object_known_p(o_ptr)) return (TRUE);
			else return (FALSE);
		}
	}

	return (FALSE);
}



/*
 * Hook to specify "armour"
 */
bool item_tester_hook_armour(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		case TV_DRAG_SHIELD:
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

	return (FALSE);
}


static bool item_tester_unknown(const object_type *o_ptr)
{
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
	int i, chance, prob;

	bool res = FALSE;

	bool a = artifact_p(o_ptr);

	u32b f1, f2, f3, fn;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

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

		/* Enchant to hit */
		if (eflag & (ENCH_TOHIT))
		{
			if (o_ptr->to_h < 0) chance = 0;
			else if (o_ptr->to_h > ENCHANT_MAX) chance = 1000;
			else chance = enchant_table[o_ptr->to_h];

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_h++;

				/* Break curse */
				if (cursed_p(o_ptr) &&
				    (!(f3 & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_h >= 0) && (rand_int(100) < 25))
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
			else if (o_ptr->to_d > ENCHANT_MAX) chance = 1000;
			else chance = enchant_table[o_ptr->to_d];

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_d++;

				/* Break curse */
				if (cursed_p(o_ptr) &&
				    (!(f3 & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_d >= 0) && (rand_int(100) < 25))
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
			else if (o_ptr->to_a > ENCHANT_MAX) chance = 1000;
			else chance = enchant_table[o_ptr->to_a];

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_a++;

				/* Break curse */
				if (cursed_p(o_ptr) &&
				    (!(f3 & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_a >= 0) && (rand_int(100) < 25))
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
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

	/* Redraw stuff */
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP  | PR_ITEMLIST);

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
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_QUIVER))) return (FALSE);

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
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_FULL);

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

	int squelch;

	object_type *o_ptr;

	cptr q, s;

	/* Only un-id'ed items */
	item_tester_hook = item_tester_unknown;

	/* Get an item */
	q = "Identify which item? ";
	s = "You have nothing to identify.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_QUIVER))) return (FALSE);

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

	/* Identify the object and get squelch setting */
	squelch = do_ident_item(item, o_ptr);

	/* Now squelch it if needed */
	do_squelch_item(squelch, item, o_ptr);

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

	int squelch;

	object_type *o_ptr;

	cptr q, s;

	/* Only un-*id*'ed items */
	item_tester_hook = item_tester_unknown_star;

	/* Get an item */
	q = "Identify which item? ";
	s = "You have nothing to identify.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_QUIVER))) return (FALSE);

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

	/* Mark the item as fully known */
	o_ptr->ident |= (IDENT_MENTAL);

	/* Identify the object and get the squelch setting */
	squelch = do_ident_item(item, o_ptr);

	/* Now squelch it if needed */
	if (squelch == SQUELCH_YES)
	{
		do_squelch_item(squelch, item, o_ptr);
	}

	else
	{
		/* Describe it fully */
		object_info_screen(o_ptr);
	}

	/* Check for easy mental feature (artifacts) */
	if (ARTIFACT_EASY_MENTAL(o_ptr))
	{
		artifact_lore *a_l_ptr = &a_l_list[o_ptr->art_num];

		/* Message, keep commented out for now */
#if 0
		if (FALSE && !a_l_ptr->was_fully_identified)
		{
			msg_print("You will always remember this artifact.");
		}
#endif

		/* Remember that we *identified* this artifact */
		a_l_ptr->was_fully_identified = TRUE;
	}

	/* Success */
	return (TRUE);
}




/*
 * Hook for "get_item()".  Determine if something is rechargable.
 */
bool item_tester_hook_recharge(const object_type *o_ptr)
{
	/* Recharge staffs */
	if (o_ptr->tval == TV_STAFF) return (TRUE);

	/* Recharge wands */
	if (o_ptr->tval == TV_WAND) return (TRUE);

	/* Recharge rods */
	if (o_ptr->tval == TV_ROD) return (TRUE);

	/* Nope */
	return (FALSE);
}

/*
 * Re-charge a staff or wand, and remove the identification
 * If there is a chance for the re-charge to fail, that
 * should be checked before this function is called.
 */
void recharge_staff_wand(object_type *o_ptr, int percent)
{
	int recharge_amount;

	if (o_ptr->tval == TV_WAND) recharge_amount = charge_wand(o_ptr, percent);
	else if (o_ptr->tval == TV_STAFF) recharge_amount = charge_staff(o_ptr, percent);
	/* Paranoia */
	else return;

	/* Handle stacks of wands/staves, with diminishing returns */
	if (o_ptr->number > 1)
	{
		if (o_ptr->tval == TV_WAND) recharge_amount += charge_wand(o_ptr, (percent * 4 / 10));
		else if (o_ptr->tval == TV_STAFF) recharge_amount += charge_staff(o_ptr, (percent * 4 / 10));
	}

	/* Little increase for a stack greater than two */
	if (o_ptr->number > 2) recharge_amount += (o_ptr->number - 2);

	/* Recharge the wand or staff. */
	o_ptr->pval += recharge_amount;

	/* *Identified* items keep the knowledge about the charges */
	if (!(o_ptr->ident & IDENT_MENTAL))
	{
		/* We no longer "know" the item */
		o_ptr->ident &= ~(IDENT_KNOWN);
	}

	/* Hack -- we no longer think the item is empty */
	o_ptr->ident &= ~(IDENT_EMPTY);

}


/*
 * Recharge a wand/staff/rod from the pack or on the floor.
 *
 *
 * XXX XXX XXX Beware of "sliding index errors".
 *
 * Should probably not "destroy" over-charged items, unless we
 * "replace" them by, say, a broken stick or some such.  The only
 * reason this is okay is because "scrolls of recharging" appear
 * BEFORE all staves/wands/rods in the inventory.  Note that the
 * new "auto_sort_pack" option would correctly handle replacing
 * the "broken" wand with any other item (i.e. a broken stick).
 *
 */
bool recharge(int num, bool cannot_fail, int percent)
{
	int i, item, lev, chance;

	object_type *o_ptr;

	int fail_type = 0;

    int recharge_amount = 0;

	int recharge_strength;

	cptr q, s;

	/* Only accept legal items, which are wands and staffs */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item */
	q = "Recharge which item? ";
	s = "You have nothing to recharge.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0) o_ptr = &inventory[item];

	/* Get the item (on the floor) */
	else o_ptr = &o_list[0 - item];

	/* Extract the object "level" */
	lev = k_info[o_ptr->k_idx].k_level;

	/* Recharge a rod, or handle failure to recharge */
	if (o_ptr->tval == TV_ROD)
	{
		/* Extract a recharge strength by comparing object level to power. */
		recharge_strength = (((35 + num) > lev) ? ((35 + num) - lev) : 0) / 5;

		/* Back-fire */
		if ((one_in_(recharge_strength)) && (!cannot_fail))
		{
			/*rods in a larger stack are more likely to be destroyed*/
			chance = 20 + (o_ptr->number * 5);

			/*destroy one rod sometimes*/
			if (randint(100) <= chance) fail_type = 2;

			/* If attempting to charge when timeout is within 5% of
			 * max timeout, destroy 1 rod half of the time */
			else if (((o_ptr->pval * 95 / 100) <= o_ptr->timeout) &&
				(one_in_(2))) fail_type = 2;

			/*else completely drain rod or stack of rods*/
			else fail_type = 1;

			/*hack - single rods only get drained*/
			if (o_ptr->number == 1) fail_type = 1;

		}

		/* Drain the rod or stack of rods. */
		if (fail_type == 1)
		{

			/*stack of rods*/
			if (o_ptr->number > 1) msg_print("The stack of rods is completely drained!");

			/*one rod*/
			else msg_print("The recharge backfires, completely draining the rod!");

			/*drain the rod or rods to max timeout*/
			o_ptr->timeout = o_ptr->pval;

		}

		/* Destroy one of the rods. */
		else if (fail_type == 2)
		{

			msg_format("There is a bright flash of light");

			/* Reduce the charges rods */
			reduce_charges(o_ptr, 1);

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
			/* Recharge amount */
			recharge_amount = MAX(num, percent);

			recharge_amount *= randint(3);

			/* Recharge by that amount */
			if (o_ptr->timeout > recharge_amount) o_ptr->timeout -= recharge_amount;
			else o_ptr->timeout = 0;
		}
	}

	/* Attempt to Recharge wand/staff, or handle failure to recharge . */
	else if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
	{

		/* Extract a recharge strength by comparing object level to power.
		 * Divide up a stack of wands/staffs' charges to calculate charge penalty.
	 	 */
		if (o_ptr->number > 1)
			i = (135 + num - lev -
			(10 * o_ptr->pval / o_ptr->number)) / 15;

		/* All unstacked wands and staffs. */
		else i = (135 + num - lev - (10 * o_ptr->pval)) / 15;

		/* Back-fire XXX XXX XXX */
		if (((i <= 1) || (one_in_(i))) && (!cannot_fail))
		{
			/* 25% chance of the entire stack, 5% more for each wand/staff in the stack
			 * else destroy one wand/staff. */

			chance = 20 + (o_ptr->number * 5);
			if (randint(100) <= chance) fail_type = 2;
			else fail_type = 1;

			/*hack - single wands don't need failtype2*/
			if (o_ptr->number == 1) fail_type = 1;

		}

		/* Destroy an object or one in a stack of objects. */
		if (fail_type == 1)
		{
			msg_format("There is a bright flash of light");

			/* Drain wands and staffs. */
			o_ptr->pval = 0;

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

		/* Potentially destroy all members of a stack of wands/staffs one-by-one. */
		else if (fail_type == 2)
		{
			int counter = o_ptr->number;

			/*get the number of wands/staffs to be destroyed*/
			while ((counter > 1) && (!one_in_(counter)))
			{
				/*reduce by one*/
				counter --;

			}

			/* Drain wands and staffs. */
			o_ptr->pval = 0;

			if ((o_ptr->number - counter) > 1)
				msg_format("There are several bright flashes of light");
			else
				msg_format("There is a bright flash of light");

			/* Reduce and describe inventory */
			if (item >= 0)
			{
				inven_item_increase(item, -counter);
				inven_item_describe(item);
				inven_item_optimize(item);
			}

			/* Reduce and describe floor item */
			else
			{
				floor_item_increase(0 - item, -counter);
				floor_item_describe(0 - item);
				floor_item_optimize(0 - item);
			}
		}

		/* Recharge */
		else
		{
			recharge_staff_wand(o_ptr, percent);
		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

	/* Redraw stuff */
	p_ptr->redraw |= (PR_INVEN);

	/* Something was done */
	return (TRUE);
}

/************************************************************************
 *                                                                      *
 *                           Projection types                           *
 *                                                                      *
 ************************************************************************/


/*
 * Handle bolt spells.
 *
 * Bolts stop as soon as they hit a monster, whiz past missed targets, and
 * (almost) never affect items on the floor.
 */
bool project_bolt(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                  int typ, u32b flg)
{
	/* Add the bolt bitflags */
	flg |= PROJECT_STOP | PROJECT_KILL | PROJECT_THRU | PROJECT_EFCT;

	/* Hurt the character unless he controls the spell */
	if (who != SOURCE_PLAYER) flg |= PROJECT_PLAY;

	/* Limit range */
	if ((rad > MAX_RANGE) || (rad <= 0)) rad = MAX_RANGE;

	/* Cast a bolt */
	return (project(who, rad, y0, x0, y1, x1, dam, typ, flg, 0, 0));
}

/*
 * Handle beam spells.
 *
 * Beams affect every grid they touch, go right through monsters, and
 * (almost) never affect items on the floor.
 */
bool project_beam(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                  int typ, u32b flg)
{
	/* Add the beam bitflags */
	flg |= PROJECT_BEAM | PROJECT_KILL | PROJECT_THRU | PROJECT_EFCT;

	/* Hurt the character unless he controls the spell */
	if (who != SOURCE_PLAYER) flg |= (PROJECT_PLAY);

	/* Limit range */
	if ((rad > MAX_RANGE) || (rad <= 0)) rad = MAX_RANGE;

	/* Cast a beam */
	return (project(who, rad, y0, x0, y1, x1, dam, typ, flg, 0, 0));
}


/*
 * Handle ball spells.
 *
 * Balls act like bolt spells, except that they do not pass their target,
 * and explode when they hit a monster, a wall, their target, or the edge
 * of sight.  Within the explosion radius, they affect items on the floor.
 *
 * Balls may jump to the target, and have any source diameter (which affects
 * how quickly their damage falls off with distance from the center of the
 * explosion).
 */
bool project_ball(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                  int typ, u32b flg, int source_diameter)
{
	/* Add the ball bitflags */
	flg |= PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL |
		PROJECT_WALL | PROJECT_EFCT;

	/* Add the STOP flag if appropriate */
	if ((who == SOURCE_PLAYER) &&
	    (!target_okay() || y1 != p_ptr->target_row || x1 != p_ptr->target_col))
	{
		flg |= (PROJECT_STOP);
	}

	/* Hurt the character unless he controls the spell */
	if (who != SOURCE_PLAYER) flg |= (PROJECT_PLAY);
	/*Hack - poison cloud poison spells have a lingering cloud */
	else if (typ == GF_POIS)
	{
		if (game_mode != GAME_NPPMORIA) flg |= (PROJECT_CLOUD);
	}

	/* Limit radius to nine (up to 256 grids affected) */
	if (rad > 9) rad = 9;

	/* Cast a ball */
	return (project(who, rad, y0, x0, y1, x1, dam, typ, flg,
	                0, source_diameter));
}

/*
 * Handle ball spells that explode immediately on the target.
 * Whether monsters or the player is hurt must be determined
 * by the code that calls this function.
 */
bool explosion(int who, int rad, int y0, int x0, int dam, int typ, u32b flg)
{
	/* Add the explosion bitflags */
	flg |= 	PROJECT_BOOM | PROJECT_GRID | PROJECT_JUMP | PROJECT_ITEM;

	/* Explode */
	return (project_ball(who, rad, y0, x0, y0, x0, dam, typ, flg, 0));
}

/*
 * Handle monster-centered explosions.
 */
bool mon_explode(int who, int rad, int y0, int x0, int dam, int typ)
{
	return (project_ball(who, rad, y0, x0, y0, x0, dam, typ, 0L, 20));
}


/*
 * Handle arc spells.
 *
 * Arcs are a pie-shaped segment (with a width determined by "degrees")
 * of a explosion outwards from the source grid.  They are centered
 * along a line extending from the source towards the target.  -LM-
 *
 * Because all arcs start out as being one grid wide, arc spells with a
 * value for degrees of arc less than (roughly) 60 do not dissipate as
 * quickly.  In the extreme case where degrees of arc is 0, the arc is
 * actually a defined length beam, and loses no strength at all over the
 * ranges found in the game.
 *
 * Arcs affect items on the floor.
 */
bool project_arc(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                 int typ, u32b flg, int degrees)
{
	/* Diameter of source of energy is normally, but not always, 20. */
	int source_diameter = 20;

	/* Radius of zero means no fixed limit. */
	if (rad == 0) rad = MAX_SIGHT;

	/* Calculate the effective diameter of the energy source, if necessary. */
	if (degrees < ARC_STANDARD_WIDTH)
	{
		if (degrees <= 9) source_diameter = rad * 10;
		else source_diameter = source_diameter * ARC_STANDARD_WIDTH / degrees;
	}

	/* If the arc has no spread, it's actually a beam */
	if (degrees <= 0)
	{
		/* Add the beam bitflags */
		flg |= (PROJECT_BEAM | PROJECT_KILL | PROJECT_THRU | PROJECT_WALL);

		source_diameter = 0;
	}

	/* If a full circle is asked for, we cast a ball spell. */
	else if (degrees >= 360)
	{
		/* Add the ball bitflags */
		flg |= PROJECT_STOP | PROJECT_BOOM | PROJECT_GRID |
		       PROJECT_ITEM | PROJECT_KILL | PROJECT_WALL;

		source_diameter = 0;
	}

	/* Otherwise, we fire an arc */
	else
	{
		/* Add the arc bitflags */
		flg |= PROJECT_ARC  | PROJECT_BOOM | PROJECT_GRID |
		       PROJECT_ITEM | PROJECT_KILL | PROJECT_THRU | PROJECT_EFCT;
	}

	/* Hurt the character unless he controls the spell */
	if (who != SOURCE_PLAYER) flg |= (PROJECT_PLAY);

	/* Cast an arc (or a ball) */
	return (project(who, rad, y0, x0, y1, x1, dam, typ, flg, degrees,
	                (byte)source_diameter));
}

/*
 * Handle starburst spells.
 *
 * Starbursts are randomized balls that use the same sort of code that
 * governs the shape of starburst rooms in the dungeon.  -LM-
 *
 * Starbursts always do full damage to every grid they affect:  however,
 * the chances of affecting grids drop off significantly towards the
 * edge of the starburst.  They always "jump" to their target and affect
 * items on the floor.
 */
bool project_star(int who, int rad, int y0, int x0, int dam, int typ, u32b flg)
{
	/* Add the star bitflags */
	flg |= PROJECT_STAR | PROJECT_BOOM | PROJECT_GRID | PROJECT_JUMP |
	       PROJECT_ITEM | PROJECT_KILL | PROJECT_EFCT;

	/* Hurt the character unless he controls the spell */
	if (who != SOURCE_PLAYER) flg |= (PROJECT_PLAY);

	/* Cast a star */
	return (project(who, rad, y0, x0, y0, x0, dam, typ, flg, 0, 0));
}

/*
 * Handle target grids for projections under the control of
 * the character.  - Chris Wilde, Morgul
 */
static void adjust_target(int dir, int y0, int x0, int *y1, int *x1)
{
	/* If no direction is given, and a target is, use the target. */
	if ((dir == 5) && target_okay())
	{
		*y1 = p_ptr->target_row;
		*x1 = p_ptr->target_col;
	}

	/* Otherwise, use the given direction */
	else
	{
		*y1 = y0 + MAX_RANGE * ddy[dir];
		*x1 = x0 + MAX_RANGE * ddx[dir];
	}
}



/*
 * Apply a "project()" directly to all monsters in view of a certain spot.
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 *
 * We are able to check LOS from either the character (in which case we
 * use line of fire for speed and accuracy), or from any given grid.
 *
 * To avoid misbehavior when monster deaths have side-effects,
 * this is done in two passes. -- JDL
 */
bool project_los(int y0, int x0, int dam, int typ)
{
	int i, d, x, y;

	u32b flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;

	bool obvious = FALSE;

	int who;

	/* Determine whether we are using LOF or LOS */
	bool line_of_fire = FALSE;

	if ((y0 == p_ptr->py) && (x0 == p_ptr->px))
	{
		line_of_fire = TRUE;
		who = SOURCE_PLAYER;
	}
	else if (cave_monster_trap_bold(y0, x0)) who = SOURCE_TRAP;
 	else if (cave_player_trap_bold(y0, x0))  who = SOURCE_EFFECT;
	else who = SOURCE_OTHER;

	/* Mark monsters in LOS */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Apply character-centered test */
		if (line_of_fire)
		{
			/* Require line of fire */
			if (!player_can_fire_bold(y, x)) continue;
		}

		/* Apply generic grid test */
		else
		{
			/* Get distance between source and monster */
			d = distance(y0, x0, y, x);

			/* LOS extends only to max sight range */
			if (d > MAX_RANGE) continue;

			/* Check LOS if not at grid or adjacent */
			if (d > 1)
			{
				/* Ignore if not in LOS */
				if (!los(y0, x0, y, x)) continue;
			}
		}

		/* Mark the monster */
		m_ptr->mflag |= (MFLAG_TEMP);
	}

	/* Affect all (nearby) monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

		/* Skip unmarked monsters */
		if (!(m_ptr->mflag & (MFLAG_TEMP))) continue;

		/* Remove mark */
		m_ptr->mflag &= ~(MFLAG_TEMP);

		/* Jump directly to the monster */
		if (project_m(who, m_ptr->fy, m_ptr->fx, dam, typ, flg))
		{
			obvious = TRUE;
		}
	}

	/* Result */
	return (obvious);
}


/*
 * This routine clears the entire "temp" set.
 */
void clear_temp_array(void)
{
	int i;

	/* Apply flag changes */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		/* No longer in the array */
		cave_info[y][x] &= ~(CAVE_TEMP);
	}

	/* None left */
	temp_n = 0;
}


/*
 * Aux function -- see below
 */
void cave_temp_mark(int y, int x, bool room)
{
	if(!in_bounds_fully(y, x)) return;

	/* Avoid infinite recursion */
	if (cave_info[y][x] & (CAVE_TEMP)) return;

	/* Option -- do not leave the current room */
	if ((room) && (!(cave_info[y][x] & (CAVE_ROOM)))) return;

	/* Verify space */
	if (temp_n == TEMP_MAX) return;

	/* Mark the grid */
	cave_info[y][x] |= (CAVE_TEMP);

	/* Add it to the marked set */
	temp_y[temp_n] = y;
	temp_x[temp_n] = x;
	temp_n++;
}

/*
 * Mark the nearby area with CAVE_TEMP flags.  Allow limited range.
 */
void spread_cave_temp(int y1, int x1, int range, bool room, bool pass_walls)
{
	int i, y, x;

	/* Add the initial grid */
	cave_temp_mark(y1, x1, room);

	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get marked, but stop further spread, unless pass_walls is TRUE */
		if (!pass_walls && !cave_project_bold(y, x)) continue;

		/* Note limited range (note:  we spread out one grid further) */
		if ((range) && (distance(y1, x1, y, x) >= range)) continue;

		/* Spread adjacent */
		cave_temp_mark(y + 1, x, room);
		cave_temp_mark(y - 1, x, room);
		cave_temp_mark(y, x + 1, room);
		cave_temp_mark(y, x - 1, room);

		/* Spread diagonal */
		cave_temp_mark(y + 1, x + 1, room);
		cave_temp_mark(y - 1, x - 1, room);
		cave_temp_mark(y - 1, x + 1, room);
		cave_temp_mark(y + 1, x - 1, room);
	}
}


/*
 * Speed monsters
 */
bool speed_monsters(void)
{
	return (project_los(p_ptr->py, p_ptr->px, p_ptr->lev, GF_OLD_SPEED));
}

/*
 * Slow monsters
 */
bool slow_monsters(int power)
{
	return (project_los(p_ptr->py, p_ptr->px, power, GF_OLD_SLOW));
}

/*
 * Sleep monsters
 */
bool sleep_monsters(int power)
{
	return (project_los(p_ptr->py, p_ptr->px, power, GF_OLD_SLEEP));
}


/*
 * Banish evil monsters
 */
bool banish_evil(int dist)
{
	return (project_los(p_ptr->py, p_ptr->px, dist, GF_AWAY_EVIL));
}


/*
 * Turn undead
 */
bool turn_undead(int power)
{
	return (project_los(p_ptr->py, p_ptr->px, power, GF_AWAY_UNDEAD));
}


/*
 * Dispel undead monsters
 */
bool dispel_undead(int dam)
{
	return (project_los(p_ptr->py, p_ptr->px, dam, GF_DISP_UNDEAD));
}

/*
 * Dispel evil monsters
 */
bool dispel_evil(int dam)
{
	return (project_los(p_ptr->py, p_ptr->px, dam, GF_DISP_EVIL));
}

/*
 * Dispel all monsters
 */
bool dispel_monsters(int dam)
{
	return (project_los(p_ptr->py, p_ptr->px, dam, GF_DISP_ALL));
}

/*
 * Polymorph all monsters in LOS
 */
bool mass_polymorph(void)
{
	/* damage figure of 100 isn't used */
	return (project_los(p_ptr->py, p_ptr->px, 100, GF_OLD_POLY));
}


/*
 * Hit the player's entire LOS with damage of given type -AR
 */
bool fire_player_los(int type, int dam)
{
	return (project_los(p_ptr->py, p_ptr->px, dam, type));
}

/*
 * Wake up all monsters, and speed up "los" monsters.
 */
void aggravate_monsters(int who)
{
	int y, x;

	/*if a monster, use monster coordinates*/
	if(who > SOURCE_MONSTER_START)
	{
		monster_type *m_ptr = &mon_list[who];

		x = m_ptr->fx;
		y = m_ptr->fy;
	}

	/*use the player coordinates*/
	else
	{
		x = p_ptr->px;
		y = p_ptr->py;
	}

	/* Messages */
	(void)project_los(y, x, (250 + rand_int(250)), GF_OLD_SPEED);

}


/*
 * Wake up all monsters, and speed up "los" monsters.
 */
void mass_aggravate_monsters(int who)
{
	int i;

	/* Aggravate everyone */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip aggravating monster (or player) */
		if (i == who) continue;

		/* Wake up all monsters */
		m_ptr->m_timed[MON_TMD_SLEEP] = 0;

		/* Speed up monsters in line of sight */
		if (player_has_los_bold(m_ptr->fy, m_ptr->fx))
		{
			int flag = MON_TMD_FLG_NOTIFY;

			if (who != SOURCE_PLAYER) flag |= MON_TMD_MON_SOURCE;

			mon_inc_timed(i, MON_TMD_FAST, (50 + rand_int(50)),	flag);
		}

		/*possibly update the monster health bar*/
		if ((p_ptr->health_who == i)  || (m_ptr->sidebar)) p_ptr->redraw |= (PR_HEALTH);
	}

	/* If it just woke up, update the monster list */
	p_ptr->redraw |= PR_MONLIST;
}

/*
 * Delete all non-unique monsters of a given "type" from the level
 */
bool banishment(void)
{
	int i;

	char typ;

	/* Mega-Hack -- Get a monster symbol */
	if (!get_com("Choose a monster race (by symbol) to banish: ", &typ))
		return FALSE;

	/* Delete the monsters of that "type" */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip Unique Monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Quest monsters can only be "killed" by the player */
		if (m_ptr->mflag & (MFLAG_QUEST)) continue;

		/* Skip "wrong" monsters */
		if (r_ptr->d_char != typ) continue;

		/* Delete the monster */
		delete_monster_idx(i);

		/* Take some damage */
		take_hit(randint(4), "the strain of casting Banishment");

	}

	/* Update monster list window */
	p_ptr->redraw |= PR_MONLIST;

	/* Success */
	return TRUE;

}


/*
 * Delete all nearby (non-unique) monsters
 */
bool mass_banishment(void)
{
	int i;

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

		/* Quest monsters can only be "killed" by the player */
		if (m_ptr->mflag & (MFLAG_QUEST)) continue;

		/* Delete the monster */
		delete_monster_idx(i);

		/* Take some damage */
		take_hit(randint(3), "the strain of casting Mass Banishment");

		/* Note effect */
		result = TRUE;
	}

	/* Update monster list window */
	p_ptr->redraw |= PR_MONLIST;

	return (result);
}



/*
 * Learn a great deal about a given monster
 */
bool probing(void)
{

	/*let the player select one monster or feature*/
	if (!(target_set_interactive(TARGET_PROBE, -1, -1)))
	{
		return(FALSE);
	}

	/*Nothing set - paranoia*/
	if (!p_ptr->target_set) return (FALSE);

	/*We selected a terrain*/
	if (!p_ptr->target_who)
	{
		int f_idx = cave_feat[p_ptr->target_row][p_ptr->target_col];

		/*Learn about the feature*/
   		lore_do_probe_feature(f_idx);

		/* Save screen */
		screen_save();

		/*display the monster info*/
		display_feature_roff(f_idx);

		/*give the player a look at the updated feature info*/
		put_str("Press any key to continue.  ", 0, 40);

		inkey();

		/* Load screen */
		screen_load();
	}

	/*We selected a monster*/
	else
	{
		int m_idx = p_ptr->target_who;
		monster_type *m_ptr = &mon_list[m_idx];
		char m_name[80];

		/* Learn about the monsters */
		lore_do_probe_monster(m_idx);

		/* Save screen */
		screen_save();

		/*display the monster info*/
		display_roff(m_ptr->r_idx);

		/*give the player a look at the updated monster info*/
		put_str("Press any key to continue.  ", 0, 40);

		inkey();

		/* Load screen */
		screen_load();

		/* Get "the monster" or "something" */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0x04);

		/* Describe the monster */
		msg_format("%^s has %d hit points.", m_name, m_ptr->hp);
	}

	/* Result */
	return (TRUE);
}



/*
 * The spell of destruction
 *
 * This spell "deletes" monsters (instead of "killing" them).
 *
 * Later we may use one function for both "destruction" and
 * "earthquake" by using the "full" to select "destruction".
 */
void destroy_area(int y1, int x1, int r)
{
	int y, x, k, t;

	bool flag = FALSE;

	/* No effect in town */
	if ((*dun_cap->prevent_destruction)())
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

			/* Extract the distance */
			k = distance(y1, x1, y, x);

			/* Stay in the circle of death */
			if (k > r) continue;

			if (cave_info[y][x] & (CAVE_ICKY)) continue;

			/* Lose room and vault */
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

			/* Hack -- Quest monsters are unaffected */
			else if (cave_m_idx[y][x] > 0)
			{
				monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

				if (m_ptr->mflag & (MFLAG_QUEST | MFLAG_QUEST_SUMMON)) continue;
			}


			/* Hack -- Skip the epicenter */
			if ((y == y1) && (x == x1)) continue;

			/* Delete the monster (if any) */
			delete_monster(y, x);

			if (cave_valid_bold(y, x))
			{
				int feat = FEAT_FLOOR;

				/* Delete objects */
				delete_object(y, x);

				/* Wall (or floor) type */
				t = rand_int(200);

				/* Burn stuff */
				if (cave_ff2_match(y, x, FF2_HURT_FIRE))
				{
					feat = feat_state(cave_feat[y][x],
						FS_HURT_FIRE);
				}

				/* Granite */
				else if (t < 20)
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
				else if (t < 130)
				{
					/* Create rubble */
					feat = FEAT_RUBBLE;
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
		if (!p_ptr->state.resist_blind && !p_ptr->state.resist_light)
		{
			/* Become blind */
			(void)inc_timed(TMD_BLIND, 10 + randint(10), TRUE);
		}
	}

	/* Hard not to notice */
	add_wakeup_chance = 10000;

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_DOORS | PU_FLOW_NO_DOORS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);

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
void earthquake(int cy, int cx, int r, bool kill_vault)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, t, y, x, yy, xx, dy, dx;

	int damage = 0;

	int sn = 0, sy = 0, sx = 0;

	bool hurt = FALSE;

	bool map[32][32];

	/* No effect in town */
	if ((*dun_cap->prevent_destruction)())
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

			/* Hack - no earthquakes inside vaults. */
			if ((cave_info[yy][xx] & (CAVE_ICKY)) && (!kill_vault)) continue;

			/* Lose room and vault */
			cave_info[yy][xx] &= ~(CAVE_ROOM);

			/* Lose light and knowledge */
			cave_info[yy][xx] &= ~(CAVE_GLOW | CAVE_MARK);

			/* Skip the epicenter */
			if (!dx && !dy) continue;

			/* Skip most grids */
			if (rand_int(100) < 85) continue;

			/* Hack - make quest monsters immune*/
			if (cave_m_idx[yy][xx] > 0)
			{
				monster_type *m_ptr = &mon_list[cave_m_idx[yy][xx]];
				if (m_ptr->mflag & (MFLAG_QUEST | MFLAG_QUEST_SUMMON))  continue;
			}

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
					(void)set_stun(p_ptr->timed[TMD_STUN] + randint(50));
					break;
				}
				case 3:
				{
					msg_print("You are crushed between the floor and ceiling!");
					damage = damroll(10, 4);
					(void)set_stun(p_ptr->timed[TMD_STUN] + randint(50));
					break;
				}
			}

			/* Move player */
			monster_swap(py, px, sy, sx);
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
				int m_idx = cave_m_idx[yy][xx];
				monster_type *m_ptr = &mon_list[m_idx];
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
							if (cave_player_glyph_bold(y, x)) continue;

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
					wake_monster_attack(m_ptr, MON_TMD_FLG_NOMESSAGE);

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

					/* If it just woke up, update the monster list */
					p_ptr->redraw |= PR_MONLIST;
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

				bool floor = (cave_ff1_match(yy, xx, FF1_MOVE) != 0);

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

	/* Hard not to notice */
	add_wakeup_chance = MAX(add_wakeup_chance, 8000);

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_DOORS | PU_FLOW_NO_DOORS);

	/* Update the health bar */
	p_ptr->redraw |= (PR_HEALTH | PR_MON_MANA);

	/* Redraw map and Window Stuff */
	p_ptr->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);
}



/*
 * This routine clears the entire "temp" set.
 *
 * This routine will Perma-Lite all "temp" grids.
 *
 * This routine is used (only) by "light_room()"
 *
 * Dark grids are illuminated.
 *
 * Also, process all affected monsters.
 *
 * SMART monsters always wake up when illuminated
 * NORMAL monsters wake up 1/4 the time when illuminated
 * STUPID monsters wake up 1/10 the time when illuminated
 */
static void cave_temp_room_light(void)
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
		light_spot(y, x);

		/* Process affected monsters */
		if (cave_m_idx[y][x] > 0)
		{
			int chance = 25;

			int m_idx = cave_m_idx[y][x];
			monster_type *m_ptr = &mon_list[m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Stupid monsters rarely wake up */
			if (r_ptr->flags2 & (RF2_STUPID)) chance = 10;

			/* Smart monsters always wake up */
			if (r_ptr->flags2 & (RF2_SMART)) chance = 100;

			/* Sometimes monsters wake up */
			if ((m_ptr->m_timed[MON_TMD_SLEEP]) && (rand_int(100) < chance))
			{
				/* Wake up! */
				wake_monster_attack(m_ptr, MON_TMD_FLG_NOTIFY);

				/*possibly update the monster health bar*/
				if ((p_ptr->health_who == m_idx)  || (m_ptr->sidebar))
					p_ptr->redraw |= (PR_HEALTH);
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
 * This routine is used (only) by "unlight_room()"
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

		/* Turn off the light */
		cave_info[y][x] &= ~(CAVE_GLOW);

		/* Hack -- Forget "boring" grids */
		if (!(cave_info[y][x] & (CAVE_HALO)) &&
			!cave_ff1_match(y, x, FF1_REMEMBER) &&
			(!cave_any_trap_bold(y, x) ||
				(x_list[cave_x_idx[y][x]].x_flags &
				(EF1_HIDDEN))))
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
		light_spot(y, x);


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
void light_room(int y1, int x1)
{
	int i, x, y;

	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);

	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get lit, but stop light */
		if (!cave_project_bold(y, x)) continue;

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
	cave_temp_room_light();

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);

}


/*
 * Darken all rooms containing the given location
 */
void unlight_room(int y1, int x1)
{
	int i, x, y;

	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);

	/* Spread, breadth first */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get dark, but stop darkness */
		if (!cave_project_bold(y, x)) continue;

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
bool light_area(int dam, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->timed[TMD_BLIND])
	{
		msg_print("You are surrounded by a white light.");
	}

	/* Hook into the "project()" function */
	(void)project(SOURCE_PLAYER, rad, py, px, py, px, dam, GF_LIGHT_WEAK, flg, 0, 0);

	/* Lite up the room */
	light_room(py, px);

	/* Assume seen */
	return (TRUE);
}


/*
 * Hack -- call darkness around the player
 * Affect all monsters in the projection radius
 */
bool unlight_area(int dam, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->timed[TMD_BLIND])
	{
		msg_print("Darkness surrounds you.");
	}

	/* Hook into the "project()" function */
	(void)project(SOURCE_PLAYER, rad, py, px, py, px, dam, GF_DARK_WEAK, flg, 0, 0);

	/* Lite up the room */
	unlight_room(py, px);

	/* Assume seen */
	return (TRUE);
}

/*
 * Character casts a special-purpose bolt or beam spell.
 */
bool fire_bolt_beam_special(int typ, int dir, int dam, int rad, u32b flg)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* This is a beam spell */
	if (flg & (PROJECT_BEAM))
	{
		/* Cast a beam */
		return (project_beam(SOURCE_PLAYER, rad, p_ptr->py, p_ptr->px, y1, x1, dam,
	                        typ, flg));
	}

	/* This is a bolt spell */
	else
	{
		/* Cast a bolt */
		return (project_bolt(SOURCE_PLAYER, rad, p_ptr->py, p_ptr->px, y1, x1, dam,
									typ, flg));
	}
}

/*
 * Player casts a orb spell that creates an effect, but does not affect anything else.
 */
bool fire_effect_orb(int typ, int dir, int dam, int rad)
{
	int y1, x1;
	u32b flg = 0L;

	/* Add the ball bitflags */
	flg |= (PROJECT_BOOM | PROJECT_WALL | PROJECT_EFCT | PROJECT_CLOUD);

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Add the STOP flag if appropriate */
	if (!target_okay() || y1 != p_ptr->target_row || x1 != p_ptr->target_col)
	{
		flg |= (PROJECT_STOP);
	}

	/* Limit radius to nine (up to 256 grids affected) */
	if (rad > 9) rad = 9;

	/* Cast a ball */
	return (project(SOURCE_PLAYER, rad, p_ptr->py, p_ptr->px, y1, x1, dam, typ, flg, 0L, 10 + rad * 10));
}

/*
 * Character casts a (simple) ball spell.
 */
bool fire_ball(int typ, int dir, int dam, int rad)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast a (simple) ball */
	return (project_ball(SOURCE_PLAYER, rad, p_ptr->py, p_ptr->px, y1, x1, dam, typ,
	                     0L, 0));
}

/*
 * Character casts an orb spell (a ball that loses no strength out
 * from the origin).
 */
bool fire_orb(int typ, int dir, int dam, int rad)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast an orb */
	return (project_ball(SOURCE_PLAYER, rad, p_ptr->py, p_ptr->px, y1, x1, dam, typ,
	                     0L, 10 + rad * 10));
}

/*
 * Character casts a ball spell with a specified source diameter, that
 * jumps to the target, or does various other special things.
 */
bool fire_ball_special(int typ, int dir, int dam, int rad, u32b flg,
                       int source_diameter)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast a ball with specified source diameter */
	return (project_ball(SOURCE_PLAYER, rad, p_ptr->py, p_ptr->px, y1, x1, dam, typ,
	                     flg, source_diameter));
}

/*
 * Character casts an arc spell.
 */
bool fire_arc(int typ, int dir, int dam, int rad, int degrees)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast an arc */
	return (project_arc(SOURCE_PLAYER, rad, p_ptr->py, p_ptr->px, y1, x1, dam, typ,
	                    0L, degrees));
}

/*
 * Character casts an arc spell.
 */
static bool fire_arc_special(int typ, int dir, int dam, int rad, int degrees, u32b flg)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast an arc */
	return (project_arc(SOURCE_PLAYER, rad, p_ptr->py, p_ptr->px, y1, x1, dam, typ,
	                    flg, degrees));
}

/*
 * Character casts a star-shaped spell.
 */
bool fire_star(int typ, int dam, int rad, u32b flg)
{
	/* Cast a star */
	return (project_star(SOURCE_PLAYER, rad, p_ptr->py, p_ptr->px, dam, typ, flg));
}


/*
 * Fire a number of bolts, beams, or arcs that start in semi-random grids
 * near the character, and head in totally random directions.  The larger
 * the number of grids in the area of fire, and the more monsters inhabit
 * those grids, the more effective this spell is.
 * -LM-
 */
void fire_storm(int who, int typ0, int y0, int x0, int dam, int rad, int len,
	byte projection, bool lingering)
{
	int i, j;
	int y, x = 0, y1, x1, last_y, last_x;
	int dir;
	int typ;
	long num_missiles;
	int choice;
	monster_type *m_ptr;

	/* Save standard delay */
	int std_delay = op_ptr->delay_factor;

	/* Array of grids (max radius is 20) */
	u16b grid[1681];

	/* Grid count */
	int grid_count = 0;

	/* Array of monsters to hurt (indexes, initial HPs) */
	s16b mon_array[100][2];

	/* Monster count */
	int mon_count = 0;

	/* Allow spell graphics to accumulate */
	u32b flg = (lingering ? PROJECT_NO_REDRAW : 0L);


	/* We can't handle a radius of more than 20 */
	if (rad > 20) rad = 20;

	/* Very little delay while projecting each missile */
	op_ptr->delay_factor = (std_delay + 1) / 2;


	/* Build up an array of all nearby projectable grids */
	for (y = y0 - rad; y <= y0 + rad; y++)
	{
		for (x = x0 - rad; x <= x0 + rad; x++)
		{
			/* Stay legal */
			if (!in_bounds(y, x)) continue;

			/* Require that grid be projectable */
			if (projectable(y0, x0, y, x, PROJECT_NONE))
			{
				/* Convert location to a grid, save and count it */
				grid[grid_count++] = GRID(y, x);
			}
		}
	}


	/* Scan the monster list */
	for (i = 0; i < mon_max; i++)
	{
		/* Get this monster */
		m_ptr = &mon_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip monsters not in LOF (or LOS), and if too far away */
		if ((y0 == p_ptr->py) && (x0 == p_ptr->px))
		{
			if (!player_can_fire_bold(m_ptr->fy, m_ptr->fx)) continue;
			if (m_ptr->cdis > rad) continue;
		}
		else
		{
			if (!los(y0, x0, m_ptr->fy, m_ptr->fx)) continue;
			if (distance(y0, x0, m_ptr->fy, m_ptr->fx) > rad) continue;
		}

		/* Store this monster and its current HPs */
		if (mon_count < 100)
		{
			mon_array[mon_count][0] = i;
			mon_array[mon_count][1] = m_ptr->hp;
			mon_count++;
		}
	}


	/* Calculate the minimum number of missiles */
	num_missiles = MAX(1L, grid_count / 8);

	/* Handle each missile in turn */
	for (i = 0;; i++)
	{
		/* Limit -- never fire more than num_missiles * 8 */
		if (i > num_missiles * 8) break;

		/* We've used up our guaranteed missiles */
		if (i >= num_missiles)
		{
			/* Assume we stop */
			bool stop = TRUE;

			/* Keep firing until all monsters have been hurt */
			for (j = 0; j < mon_count; j++)
			{
				/* Get this monster */
				m_ptr = &mon_list[mon_array[j][0]];

				/* Skip dead monsters */
				if (!m_ptr->r_idx) continue;

				/* Skip monsters with HPs < initial value */
				if (m_ptr->hp < mon_array[j][1]) continue;

				/* This monster hasn't been hurt - keep firing */
				stop = FALSE;
				break;
			}

			/* Stop if all monsters have been hurt */
			if (stop) break;
		}


		/* Start with a very far away location */
		last_y = -255;
		last_x = -255;

		/* Bias for closer grids */
		for (j = 0; j < 3; j++)
		{
			/* Choose a grid at random */
			choice = rand_int(grid_count);

			/* Get the coordinates */
			y = GRID_Y(grid[choice]);
			x = GRID_X(grid[choice]);

			/* Save if less than previous distance */
			if (distance(y, x, y0, x0) < distance(last_x, last_x, y0, x0))
			{
				/* Save these coordinates */
				last_y = y;
				last_x = x;
			}
		}

		/* No movement */
		dir = 5;

		/* Get any direction other than 5 */
		while (dir == 5) dir = randint(9);

		/* Get target grid */
		y1 = last_y + ddy[dir];
		x1 = last_x + ddx[dir];


		/* Allow wizardly projection types */
		if (typ0 == -1)
		{
			choice = rand_int(12);

			if      (choice ==  1) typ = GF_FIRE;
			else if (choice ==  2) typ = GF_COLD;
			else if (choice ==  3) typ = GF_ACID;
			else if (choice ==  4) typ = GF_ELEC;
			else if (choice ==  5) typ = GF_POIS;
			else if (choice ==  6) typ = GF_LIGHT;
			else if (choice ==  7) typ = GF_DARK;
			else if (choice ==  8) typ = GF_NEXUS;
			else if (choice ==  9) typ = GF_CONFUSION;
			else if (choice == 10) typ = GF_SOUND;
			else if (choice == 11) typ = GF_SHARD;
			else                   typ = GF_CHAOS;
		}

		/* Allow light, darkness, and confusion */
		else if (typ0 == -2)
		{
			choice = rand_int(3);

			if      (choice == 1) typ = GF_LIGHT;
			else if (choice == 2) typ = GF_DARK;
			else                  typ = GF_CONFUSION;
		}

		/* Use given projection */
		else
		{
			typ = typ0;
		}

		/* Fire a projection using the calculated data */
		if (projection == 0)
		{
			(void)project_bolt(who, len, last_y, last_x, y1, x1, dam, typ, flg);
		}
		else if (projection == 1)
		{
			(void)project_beam(who, len, last_y, last_x, y1, x1, dam, typ, flg);
		}
		else if (projection == 2)
		{
			/* Used for the "Prismatic Armageddon" spell */
			(void)project_arc(who, rand_range(len - 1, len + 1), last_y, last_x,
				y1, x1, dam, typ, flg, rand_range(40, 55));
		}
		else if (projection == 3)
		{
			(void)project_ball(who, rad, y1, x1, y1, x1, dam, typ, flg, 0);
		}
	}

	/* We allowed spell graphics to accumulate */
	if (lingering)
	{

		int ty = Term->offset_y + SCREEN_HGT;
		int tx = Term->offset_x + SCREEN_WID;

		/* Clear all lingering spell effects on screen XXX */
		for (y = Term->offset_y; y < ty; y++)
		{
			for (y = Term->offset_x; x < tx; x++)
			{
				light_spot(y, x);
			}
		}
	}

	/* Restore standard delay */
	op_ptr->delay_factor = std_delay;
}


/*
 * Fire beams in random directions.
 */
bool beam_burst(int y, int x, int typ, int num, int dam)
{
	int i, yy, xx;

	bool notice = FALSE;

	int old_delay = op_ptr->delay_factor;

	/* Require legal centerpoint */
	if (!in_bounds_fully(y, x)) return (FALSE);


	/* Hack -- lower delay factor */
	if (op_ptr->delay_factor)
	{
		op_ptr->delay_factor = (op_ptr->delay_factor + 1) / 2;
	}

	/* Fire beams in all directions */
	for (i = 0; i < num; i++)
	{
		/* Get a totally random grid within six grids from current position */
		yy = rand_spread(y, 6);
		xx = rand_spread(x, 6);

		/* Fire a beam of (strong) light towards it */
		if (project(-1, 0, y, x, yy, xx, dam, typ,
			PROJECT_BEAM | PROJECT_KILL | PROJECT_EFCT, 0, 0)) notice = TRUE;
	}

	/* Restore standard delay */
	op_ptr->delay_factor = old_delay;

	/* Return "anything noticed" */
	return (notice);
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

	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	while (num--)
	{
		/* Analyze the "dir" and the "target".  Hurt items on floor. */
		if (project_ball(SOURCE_PLAYER, rad, p_ptr->py, p_ptr->px, y1, x1, dam,
				typ, 0L, 0)) noticed = TRUE;
	}

	return noticed;
}


/*
 * Character casts a bolt spell.
 */
bool fire_bolt(int typ, int dir, int dam)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast a bolt */
	return (project_bolt(SOURCE_PLAYER, MAX_RANGE, p_ptr->py, p_ptr->px, y1, x1, dam,
	                     typ, 0L));
}

/*
 * Character casts a beam spell.
 */
bool fire_beam(int typ, int dir, int dam, u32b flg)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast a beam */
	return (project_beam(-1, MAX_RANGE, p_ptr->py, p_ptr->px, y1, x1, dam,
	                     typ, flg));
}


/*
 * Cast a bolt or a beam spell
 */
bool fire_bolt_or_beam(int prob, int typ, int dir, int dam)
{
	if (rand_int(100) < prob)
	{
		return (fire_beam(typ, dir, dam, 0L));
	}
	else
	{
		return (fire_bolt(typ, dir, dam));
	}
}

/*
 * Some of the old functions
 */

bool light_line(int dir, int dam)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID;
	return (fire_bolt_beam_special(GF_LIGHT_WEAK, dir, dam, MAX_RANGE, flg));
}

bool strong_light_line(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_LIGHT, dir, damroll(10, 8), MAX_RANGE, flg));
}

bool drain_life(int dir, int dam)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_LIFE_DRAIN, dir, dam, MAX_RANGE, flg));
}

bool build_wall(int dir, int dam)
{
	u32b flg = (PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_WALL);
	return (fire_beam(GF_MAKE_WALL, dir, dam, flg));
}


bool wall_to_mud(int dir, int dam)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (fire_bolt_beam_special(GF_KILL_WALL, dir, dam, MAX_RANGE, flg));
}

bool destroy_door(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (fire_bolt_beam_special(GF_KILL_DOOR, dir, 0, MAX_RANGE, flg));
}

bool disarm_trap(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (fire_bolt_beam_special(GF_KILL_TRAP, dir, 0, MAX_RANGE, flg));
}

bool heal_monster(int dir, int dam)
{
	u32b flg = PROJECT_STOP;
	return (fire_bolt_beam_special(GF_OLD_HEAL, dir, dam, MAX_RANGE, flg));
}

bool speed_monster(int dir)
{
	u32b flg = PROJECT_STOP;
	return (fire_bolt_beam_special(GF_OLD_SPEED, dir, p_ptr->lev, MAX_RANGE, flg));
}

bool slow_monster(int dir)
{
	u32b flg = PROJECT_STOP;
	return (fire_bolt_beam_special(GF_OLD_SLOW, dir, p_ptr->lev, MAX_RANGE, flg));
}

bool sleep_monster(int dir)
{
	u32b flg = PROJECT_STOP;
	return (fire_bolt_beam_special(GF_OLD_SLEEP, dir, damroll (3, p_ptr->lev), MAX_RANGE, flg));
}

bool confuse_monster(int dir, int plev)
{
	u32b flg = PROJECT_STOP;
	return (fire_bolt_beam_special(GF_OLD_CONF, dir, p_ptr->lev, MAX_RANGE, flg));
}

bool poly_monster(int dir)
{
	u32b flg = PROJECT_STOP;
	return (fire_bolt_beam_special(GF_OLD_POLY, dir, p_ptr->lev, MAX_RANGE, flg));
}

bool clone_monster(int dir)
{
	u32b flg = PROJECT_STOP;
	return (fire_bolt_beam_special(GF_OLD_CLONE, dir, 0, MAX_RANGE, flg));
}

bool fear_monster(int dir, int plev)
{
	u32b flg = PROJECT_STOP;
	return (fire_bolt_beam_special(GF_TURN_ALL, dir, p_ptr->lev, MAX_RANGE, flg));
}

bool teleport_monster(int dir)
{
	return (fire_beam(GF_AWAY_ALL, dir, MAX_SIGHT * 5, 0L));
}



/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */

bool door_creation(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(SOURCE_PLAYER, 1, py, px, py, px, 0, GF_MAKE_DOOR, flg, 0,0));
}

bool trap_creation(int who)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE | PROJECT_EFCT;
	return (project(who, 1, py, px, py, px, 0, GF_MAKE_TRAP, flg, 0, 0));
}

bool destroy_doors_touch(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;

	return (project(SOURCE_PLAYER, 1, py, px, py, px, 0, GF_KILL_DOOR, flg, 0, 0));
}

bool sleep_monsters_touch(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int dam = damroll(3, p_ptr->lev);

	u32b flg = PROJECT_BOOM | PROJECT_KILL | PROJECT_HIDE;

	if (game_mode == GAME_NPPMORIA) dam = 500;

	return (project(SOURCE_PLAYER, 1, py, px, py, px, dam, GF_OLD_SLEEP, flg, 0, 20));
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


	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_BASE);

	/* Attempt a saving throw for artifacts */
	if (artifact_p(o_ptr) && (rand_int(100) < 50))
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

		/* Blast the armor */
		o_ptr->art_num = 0;
		o_ptr->ego_num = EGO_BLASTED;
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

		/* Recalculate bonuses and mana*/
		p_ptr->update |= (PU_BONUS | PU_NATIVE | PU_MANA);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

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

	/* Handle swap weapons */
	if (!obj_is_weapon(o_ptr))
	{
		o_ptr = &inventory[INVEN_SWAP_WEAPON];

		if (!obj_is_weapon(o_ptr)) return (FALSE);
	}

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_BASE);

	/* Attempt a saving throw */
	if (artifact_p(o_ptr) && (rand_int(100) < 50))
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

		/* Shatter the weapon */
		o_ptr->art_num = 0;
		o_ptr->ego_num = EGO_SHATTERED;
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

		/* Recalculate bonuses and mana*/
		p_ptr->update |= (PU_BONUS | PU_NATIVE | PU_MANA);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

	}

	/* Notice */
	return (TRUE);
}


/*
 * Brand weapons (or ammo)
 *
 * Turns the (non-magical) object into an ego-item of 'brand_type'.
 */
bool brand_object(object_type *o_ptr, byte brand_type, bool do_enchant)
{
	/* you can never modify artifacts / ego-items */
	/* you can never modify broken / cursed items */
	if ((o_ptr->k_idx) &&
	    (!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
	    (!broken_p(o_ptr)) && (!cursed_p(o_ptr)))
	{
		cptr act = "magical";
		char o_name[80];

		object_desc(o_name, sizeof(o_name), o_ptr, ODESC_BASE);

		/*Handle weapons differently than ammo*/
		if (wield_slot(o_ptr) == INVEN_WIELD)
		{
			/* Brand the object */
			o_ptr->ego_num = EGO_BRAND_ELEMENTS;

			o_ptr->xtra1 = OBJECT_XTRA_TYPE_BRAND;
			o_ptr->xtra2 = 1 << brand_type;
		}
		else
		{

			/* Brand the object */
			o_ptr->ego_num = brand_type;
		}

		switch (brand_type)
		{
			case BRAND_OFFSET_FLAME:
			case EGO_AMMO_FLAME:
			{
				act = "fiery";
				break;
			}
			case BRAND_OFFSET_FROST:
			case EGO_AMMO_FROST:
			{
				act = "frosty";
				break;
			}
			case BRAND_OFFSET_VENOM:
			case EGO_AMMO_VENOM:
			{
				act = "sickly";
				break;
			}
		}

		/* Describe */
		msg_format("A %s aura surrounds the %s.", act, o_name);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

		/* Window stuff */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP  | PR_ITEMLIST);

		/* Enchant */
		if (do_enchant) enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);

		/* Hack - Identify it */
		object_aware(o_ptr);
		object_known(o_ptr);

		return (TRUE);
	}
	else
	{
		if (flush_failure) flush();
		msg_print("The Branding failed.");
	}

	return (FALSE);
}


/*
 * Brand the current weapon
 */
bool brand_weapon(bool enchant)
{
	object_type *o_ptr;
	byte brand_type;

	o_ptr = &inventory[INVEN_WIELD];

	/* Handle swap weapons */
	if (!obj_is_weapon(o_ptr))
	{
		if (!obj_is_weapon(o_ptr)) return (FALSE);
	}

	/* Select a brand */
	if (one_in_(3))
		brand_type = BRAND_OFFSET_FLAME;
	else
		brand_type = BRAND_OFFSET_FROST;

	/* Brand the weapon */
	return (brand_object(o_ptr, brand_type, enchant));
}

/*
 * Hook to specify "ammo"
 */
bool item_tester_hook_ided_ammo(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			if (object_known_p(o_ptr)) return (TRUE);
			else return FALSE;
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "ammo"
 */
bool item_tester_hook_ammo(const object_type *o_ptr)
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
bool brand_ammo(bool enchant)
{
	int item;
	object_type *o_ptr;
	cptr q, s;
	byte brand_type;

	/* Only accept ammo */
	item_tester_hook = item_tester_hook_ammo;

	/* Get an item */
	q = "Brand which kind of ammunition? ";
	s = "You have nothing to brand.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_QUIVER | QUIVER_FIRST))) return (FALSE);

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

	/* Select the brand */
	if (one_in_(3))
		brand_type = EGO_AMMO_FLAME;
	else if (one_in_(2))
		brand_type = EGO_AMMO_FROST;
	else brand_type = EGO_AMMO_VENOM;

	/* Brand the ammo */
	return (brand_object(o_ptr, brand_type, enchant));

}


/*
 * Enchant some (non-magical) bolts
 */
bool brand_bolts(bool enchant)
{
	int item;
	object_type *o_ptr;
	cptr q, s;


	/* Restrict choices to bolts */
	item_tester_tval = TV_BOLT;

	/* Get an item */
	q = "Brand which bolts? ";
	s = "You have no bolts to brand.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_QUIVER | QUIVER_FIRST))) return (FALSE);

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
	return (brand_object(o_ptr, EGO_AMMO_FLAME, enchant));

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
			/* Message */
			msg_print("You are surrounded by a malignant aura.");

			/* Decrease all stats (permanently) */
			(void)dec_stat(A_STR, 50, TRUE);
			(void)dec_stat(A_INT, 50, TRUE);
			(void)dec_stat(A_WIS, 50, TRUE);
			(void)dec_stat(A_DEX, 50, TRUE);
			(void)dec_stat(A_CON, 50, TRUE);
			(void)dec_stat(A_CHR, 50, TRUE);

			/* Lose some experience (permanently) */
			p_ptr->exp -= (p_ptr->exp / 4);
			p_ptr->max_exp -= (p_ptr->max_exp / 4);
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
 * Identifies all objects in the equipment and inventory.
 * Applies quality/ego-item squelch in the inventory.
 */
void identify_and_squelch_pack(void)
{
  	int item, squelch;
	object_type *o_ptr;

	/* Identify equipment */
	for (item = INVEN_WIELD; item < ALL_INVEN_TOTAL; item++)
	{
		/* Get the object */
		o_ptr = &inventory[item];

		/* Ignore empty objects */
		if (!o_ptr->k_idx) continue;

		/* Ignore known objects */
		if (object_known_p(o_ptr)) continue;

		/* Identify it */
		(void)do_ident_item(item, o_ptr);
	}

	/* Identify inventory */
	for (item = 0; item < INVEN_WIELD; item++)
	{
	  	while (TRUE)
		{
	  		/* Get the object */
			o_ptr = &inventory[item];

			/* Ignore empty objects */
			if (!o_ptr->k_idx) break;

			/* Ignore known objects */
			if (object_known_p(o_ptr)) break;

			/* Identify it and get the squelch setting */
			squelch = do_ident_item(item, o_ptr);

			/*
			 * If the object was squelched, keep analyzing
			 * the same slot (the inventory was displaced). -DG-
			 */
			if (squelch != SQUELCH_YES) break;

			/* Now squelch the object */
			do_squelch_item(squelch, item, o_ptr);
		}
	}
}

/* Mass-identify handler */
bool mass_identify (int rad)
{
	/*record the old target*/
	s16b old_target_set = p_ptr->target_set;
	s16b old_target_who = p_ptr->target_who;
	s16b old_target_row = p_ptr->target_row;
	s16b old_target_col	= p_ptr->target_col;

	/* Direct the ball to the player */
  	target_set_location(p_ptr->py, p_ptr->px);

	/* Cast the ball spell */
	fire_ball(GF_MASS_IDENTIFY, 5, 0, rad);

  	/* Identify equipment and inventory, apply quality squelch */
  	identify_and_squelch_pack();

	/*re-set to old target*/
	p_ptr->target_set = old_target_set;
	p_ptr->target_who = old_target_who;
	p_ptr->target_row = old_target_row;
	p_ptr->target_col = old_target_col;

	/* This spell always works */
	return (TRUE);
}

void identify_object(object_type *o_ptr, bool star_ident)
{
	/* Identify it */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Apply an autoinscription, if necessary */
	apply_autoinscription(o_ptr);

	/*   *identify the item if called for*/
	if (star_ident)
	{
		/* Mark the item as fully known */
		o_ptr->ident |= (IDENT_MENTAL);
	}

	p_ptr->redraw |= (PR_ITEMLIST);
}

/*
 * Execute some common code of the identify spells.
 * "item" is used to print the slot occupied by an object in equip/inven.
 * ANY negative value assigned to "item" can be used for specifying an object
 * on the floor (they don't have a slot, example: the code used to handle
 * GF_MASS_IDENTIFY in project_o).
 * It returns the value returned by squelch_itemp.
 * The object is NOT squelched here.
 */
int do_ident_item(int item, object_type *o_ptr)
{
	char o_name[80];
	u16b msgt = MSG_GENERIC;
	int squelch = SQUELCH_NO;

	/* In Moria, mark the item as fully known */
	if (game_mode == GAME_NPPMORIA) o_ptr->ident |= (IDENT_MENTAL);

	/* Identify it */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Apply an autoinscription, if necessary */
	apply_autoinscription(o_ptr);

	/* Squelch it? */
	if (item < INVEN_WIELD) squelch = squelch_itemp(o_ptr, 0, TRUE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS | PU_NATIVE);

	p_ptr->redraw |= (PR_EXP | PR_STATS | PR_INVEN | PR_EQUIP | PR_ITEMLIST);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Possibly play a sound depending on object quality. */
	if (o_ptr->pval < 0)
	{
		/* This is a bad item. */
		sound(MSG_IDENT_BAD);
	}
	else if (o_ptr->art_num != 0)
	{
		/* We have a good artifact. */
		sound(MSG_IDENT_ART);
	}
	else if (o_ptr->ego_num != 0)
	{
		/* We have a good ego item. */
		sound(MSG_IDENT_EGO);
	}

	/* Display independent of cursedness */
	if(o_ptr->art_num)
		msgt = MSG_IDENT_ART;
	else if (o_ptr->ego_num)
		msgt = MSG_IDENT_EGO;

	/* Describe */
	if (item >= INVEN_WIELD)
	{
		msg_c_format(msgt, "%^s: %s (%c).",
			describe_use(item), o_name, index_to_label(item));
	}
	else if (item >= 0)
	{
		msg_c_format(msgt, "In your pack: %s (%c).  %s",
			o_name, index_to_label(item),
			squelch_to_label(squelch));
 	}
	else
	{
		msg_c_format(msgt, "On the ground: %s.  %s", o_name,
			squelch_to_label(squelch));
	}

	/*
	 * If the item was an artifact, and if the auto-note is selected,
	 * write a message.
	 */
    	if ((adult_take_notes) && artifact_p(o_ptr) && (o_ptr->xtra1 >= 1))
	{
		int artifact_depth;
        	char note[120];
		char shorter_desc[120];

		/* Get a shorter description to fit the notes file */
		object_desc(shorter_desc, sizeof(shorter_desc), o_ptr, ODESC_PREFIX | ODESC_BASE);

		/* Build note and write */
        	sprintf(note, "Found %s", shorter_desc);

		/* Record the depth where the artifact was created */
		artifact_depth = o_ptr->xtra1;

        	do_cmd_note(note, artifact_depth);

		/*
		 * Mark item creation depth 0, which will indicate the artifact
		 * has been previously identified.  This prevents an artifact
		 * from showing up on the notes list twice ifthe artifact had
		 * been previously identified.  JG
		 */
		o_ptr->xtra1 = 0 ;
	}

	/* Check if the object is an artifact known from a previous game */
	if (!(o_ptr->ident & (IDENT_MENTAL)) && ARTIFACT_EASY_MENTAL(o_ptr)
		&& a_l_list[o_ptr->art_num].was_fully_identified)
	{
		/* Message */
		msg_c_format(MSG_NOTICE, "You are already familiar with this artifact.");

		/* Fully identify the artifact for free */
		o_ptr->ident |= (IDENT_MENTAL);
	}

	return (squelch);
}

/*
 * Get a spell type (GF_*) for the given terrain feature.
 * The spell type is stored in gf_type.
 * A description of the effect on an object is stored in action.
 * Both gf_type and action can be NULL.
 */
void get_spell_type_from_feature(int f_idx, int *gf_type, cptr *action)
{
	/* Get the element flags */
	u32b element = feat_ff3_match(f_idx, TERRAIN_MASK);
	u16b i;

	/*
	 * Spell information for each element type.
	 */
	static struct
	{
		u32b element;
		int gf_type;
		const char *action;
	} spell_info[] =
	{
		{ELEMENT_BWATER, GF_BWATER, "burns"},
		{ELEMENT_BMUD, GF_BMUD, "burns"},
		{ELEMENT_LAVA, GF_LAVA, "burns"},
		{ELEMENT_FIRE, GF_FIRE, "burns"},
		{ELEMENT_ICE, GF_ICE, "freezes"},
		{ELEMENT_WATER, GF_WATER, "hurts"},
		{ELEMENT_SAND, GF_SAND, "hurts"},
		{ELEMENT_ACID, GF_ACID, "hurts"}
	};

	/* Save default spell type */
	if (gf_type) *gf_type = GF_MANA;

	/* Save default action */
	if (action) *action = "hurts";

	/* Find the element in the list */
	for (i = 0; i < N_ELEMENTS(spell_info); i++)
	{
		/* Found the element? */
		if (spell_info[i].element == element)
		{
			/* Save the spell type */
			if (gf_type) *gf_type = spell_info[i].gf_type;

			/* Save the action */
			if (action) *action = spell_info[i].action;

			/* Done */
			return;
		}
	}
}


/*
 * Return TRUE if the player is immune to the effects of the given
 * spell type
 */
bool is_player_immune(int gf_type)
{
	/* Check immunities */
	switch (gf_type)
	{
		case GF_ACID:
		{
			if (p_ptr->state.immune_acid) return (TRUE);

			break;
		}
		case GF_ICE:
		case GF_COLD:
		{
			if (p_ptr->state.immune_cold) return (TRUE);

			break;
		}
		case GF_FIRE:
		{
			if (p_ptr->state.immune_fire) return (TRUE);

			break;
		}
		case GF_ELEC:
		{
			if (p_ptr->state.immune_elec) return (TRUE);

			break;
		}
		case GF_POIS:
		{
			if (p_ptr->state.immune_pois) return (TRUE);

			break;
		}
	}

	return (FALSE);
}


/*
 * Lite the part of the dungeon that natural beings can see. Objects are also displayed.
 * Return TRUE if we found one of such creatures.
 */
bool read_minds(void)
{
	int y, x, yy, xx;
	int rad = MAX_SIGHT;
	bool flag = FALSE;
	int count = 0;

	/* Scan the dungeon for animals */
	for (y = 1; y < (p_ptr->cur_map_hgt - 1); y++)
	{
		for (x = 1; x < (p_ptr->cur_map_wid - 1); x++)
		{
			int m_idx = cave_m_idx[y][x];
			monster_type *m_ptr;
			monster_race *r_ptr;

			/* Empty grid */
			if (m_idx <= 0) continue;

			/* Get the monster */
			m_ptr = &mon_list[m_idx];

			/* Get the race */
			r_ptr = &r_info[m_ptr->r_idx];

			/* Avoid non-animals */
			if (!(r_ptr->flags3 & (RF3_ANIMAL))) continue;

			/* Wake up monsters (sometimes) */
			if ((m_ptr->m_timed[MON_TMD_SLEEP]) && (rand_int(100) < p_ptr->lev))
			{
				/* No more sleeping */
				wake_monster_attack(m_ptr, MON_TMD_FLG_NOMESSAGE);

				/* Remember this */
				++count;
			}

			/* Lite the surrounding area */
			for (yy = (y - rad); yy <= (y + rad); yy++)
			{
				for (xx = (x - rad); xx <= (x + rad); xx++)
				{
					int o_idx;

					/* Ignore annoying grids */
					if (!in_bounds(yy, xx)) continue;

					/* Ignore distant grids */
					if (distance(yy, xx, y, x) > rad) continue;

					/* Ignore grids outside of los */
					if (!los(y, x, yy, xx)) continue;

					/* Found! */
					flag = TRUE;

					/* Lite */
					cave_info[yy][xx] |= (CAVE_MARK | CAVE_GLOW);

					/* Remember the feature */
					f_info[cave_feat[yy][xx]].f_everseen = TRUE;

					/* Get the first object on the floor */
					o_idx = cave_o_idx[yy][xx];

					/* Mark all the objects in the grid */
					while (o_idx)
					{
						object_type *o_ptr = &o_list[o_idx];

						o_ptr->marked = TRUE;

						o_idx = o_ptr->next_o_idx;
					}
				}
			}
		}
	}

	/* Update the monster list */
	p_ptr->redraw |= PR_MONLIST;

	/* Show a message if some monsters wake up */
	if (count > 0)
	{
		if (count == 1) msg_format("A monster wakes up!");
		else msg_format("%d monsters wake up!", count);
	}

	/* Show a message if some monsters wake up */
	if (count > 0)
	{
		if (count == 1) msg_format("A monster wakes up!");
		else msg_format("%d monsters wake up!", count);
	}

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	return (flag);
}


/*
 * Return a string containing the printable name of the given spell type.
 */
static cptr find_spell_type_name(int gf_type)
{
	/* List of names for each gf_* constant */
	static struct
	{
		int gf_type;
		cptr name;
	} info[] =
	{
		{GF_ACID, "acid"},
		{GF_ELEC, "lightning"},
		{GF_FIRE, "fire"},
		{GF_POIS, "poison"},
		{GF_COLD, "cold"},
		{GF_ICE, "ice"},
		{GF_LAVA, "lava"},
		{GF_SAND, "sand"},
		{GF_WATER, "water"},
		{GF_BWATER, "boiling water"},
		{GF_BMUD, "boiling mud"},
		{GF_PLASMA, "plasma"},
		{GF_LIGHT, "light"},
		{GF_DARK, "darkness"},
		{GF_TIME, "time"},
		{GF_DISENCHANT, "disenchantment"},
		{GF_NETHER, "nether"},
		{GF_HOLY_ORB, "holy energy"},
		{GF_FORCE, "force"},
		{GF_SOUND, "sound"},
		{GF_INERTIA, "inertia"},
		{GF_GRAVITY, "gravity"},
		{GF_SHARD, "shards"},
		{GF_CONFUSION, "confusion"},
		{GF_MANA, "mana"},
		{GF_NEXUS, "nexus"},
		{GF_CHAOS, "chaos"},
	};
	u16b i;

	/* Find the spell type */
	for (i = 0; i < N_ELEMENTS(info); i++)
	{
		/* Found */
		if (info[i].gf_type == gf_type) return (info[i].name);
	}

	/* Paranoia */
	return ("?");
}

/*
 * Place terrain features related to the given spell type at random locations.
 * x, y and rad define the action area.
 */
static void misc_place_elements(int y, int x, int gf_type, int rad)
{
	/* List of features for each spell type */
	static struct
	{
		int gf_type;
		u16b feat;
	} info[] =
	{
		{GF_ACID, FEAT_ACID},
		{GF_ICE, FEAT_ICE},
		{GF_COLD, FEAT_ICE},
		{GF_FIRE, FEAT_FIRE},
		{0, 0}
	};
	int yy, xx;
	int i;
	u16b feat = 0;

	/* Find the spell type in the list and retrieve the feature */
	for (i = 0; info[i].gf_type; i++)
	{
		/* Found */
		if (info[i].gf_type == gf_type)
		{
			feat = info[i].feat;

			break;
		}
	}

	/* Not found? Done */
	if (!feat) return;

	/* Place features around the given point */
	for (yy = y - rad; yy <= y + rad; yy++)
	{
		for (xx = x - rad; xx <= x + rad; xx++)
		{
			/* Check bounds */
			if (!in_bounds(yy, xx)) continue;

			/* Too far away */
			if (distance(yy, xx, y, x) > rad) continue;

			/* It must be free of objects and monsters */
			if (!cave_empty_bold(yy, xx)) continue;

			if (!cave_clean_bold(yy, xx)) continue;

			/* Reject elemental grids */
			if (cave_ff3_match(yy, xx, TERRAIN_MASK)) continue;

			/* Flavor */
			if (one_in_(4))
			{
				/* Put the feature */
				cave_set_feat(yy, xx, feat);
			}
		}
	}
}

/*
 * Cast a random elemental ball (for druid-like characters). Elements included are fire,
 * cold, acid, lightning and poison.
 * Return TRUE if it succeeds.
 */
bool master_elements(int dam, int dir)
{

	/* Information for each element type */
	static struct
	{
		int gf_type;		/* Element type */
		/*
		 * LF1_* flags. If some of these are present in the grid occupied by the player
		 * the probability and damage of the element are increased
		 */
		u32b level_flags;
		int prob1;		/* Default probability */
		int prob2;		/* Working probability */
	} spell_info[] =
	{
		{GF_ACID, LF1_ACID, 100, 0},
		{GF_ELEC, LF1_WATER | LF1_BWATER,  100, 0},
		{GF_FIRE, LF1_FIRE | LF1_BMUD | LF1_LAVA,  100, 0},
		{GF_POIS, LF1_ACID,  100, 0},
		{GF_COLD, LF1_ICE,  50, 0},
		{GF_ICE, LF1_ICE,  50, 0},
	}, *info;
	u16b i, total = 0;
	u32b flags;
	cptr name;

	/* Accumulate the chance values */
	for (i = 0; i < N_ELEMENTS(spell_info); i++)
	{
		/* Get the spell */
		info = &spell_info[i];

		/* Get the LF1_* flag of the grid ocuppied by the player */
		flags = get_level_flag(cave_feat[p_ptr->py][p_ptr->px]);

		/* Boost chance? */
		if (flags & info->level_flags)
		{
			/*msg_format("More chance for %s.", info->name);*/
			info->prob2 = 3 * info->prob1 / 2;
		}
		/* Or just copy the default chance value */
		else
		{
			info->prob2 = info->prob1;
		}

		/* Accumulate */
		total += info->prob2;
	}

	/* Paranoia */
	if (total == 0) return (FALSE);

	/* Get a random element */
	total = rand_int(total);

	/* Find the element */
	for (i = 0; i < N_ELEMENTS(spell_info); i++)
	{
		/* Get the element */
		info = &spell_info[i];

		/* Discard forbidden features (unused) */
		if (info->prob2 == 0) continue;

		/* Found. Done. */
		if (total < info->prob2) break;

		/* Prepare for the next element */
		total -= info->prob2;
	}

	/* Get the name of the element */
	name = find_spell_type_name(info->gf_type);

	/* Message */
	msg_format("You cast a ball of %s!", name);

	/*msg_format("dam: %d.", dam);*/

	/* Cast the spell. Do not create effects (smoke, sparks, etc.) */
	fire_ball_special(info->gf_type, dir, dam, 5, PROJECT_NO_EFCT, 30);

	/* Modify the dungeon layout sometimes */
	if (one_in_(10)) misc_place_elements(p_ptr->py, p_ptr->px, info->gf_type, 2);

	return (TRUE);
}

/*
 * Cast attack spells like nearby animals and vortices (bolts, beams, balls and breaths)
 * Return TRUE if it succeeds.
 */
bool steal_powers(int dir)
{
	/* The type of attacks */
	#define BREATH 1
	#define BALL 2
	#define BOLT 3
	#define BEAM 4

	/* List of possible ranged attacks */
	struct attack_info_type
	{
		byte set;	/* Which set of flags is used (4 for flags4, etc.) */
		u32b flag;	/* Monster flag to check */
		u16b spell_type; /* The spell type. One of the GF_* constants */
		byte attack_type; /* Shape of the attack */
	};

	/* Ranged attacks */
	static struct attack_info_type attack_info[] =
	{
		{4, RF4_BRTH_ACID, GF_ACID, BREATH},
		{4, RF4_BRTH_ELEC, GF_ELEC, BREATH},
		{4, RF4_BRTH_FIRE, GF_FIRE, BREATH},
		{4, RF4_BRTH_COLD, GF_COLD, BREATH},
		{4, RF4_BRTH_POIS, GF_POIS, BREATH},
		{4, RF4_BRTH_PLAS, GF_PLASMA, BREATH},
		{4, RF4_BRTH_LIGHT, GF_LIGHT, BREATH},
		{4, RF4_BRTH_DARK, GF_DARK, BREATH},
		{4, RF4_BRTH_CONFU, GF_CONFUSION, BREATH},
		{4, RF4_BRTH_SOUND, GF_SOUND, BREATH},
		{4, RF4_BRTH_SHARD, GF_SHARD, BREATH},
		{4, RF4_BRTH_INER, GF_INERTIA, BREATH},
		{4, RF4_BRTH_GRAV, GF_GRAVITY, BREATH},
		{4, RF4_BRTH_FORCE, GF_FORCE, BREATH},
		{4, RF4_BRTH_NEXUS, GF_NEXUS, BREATH},
		{4, RF4_BRTH_NETHR, GF_NETHER, BREATH},
		{4, RF4_BRTH_CHAOS, GF_CHAOS, BREATH},
		{4, RF4_BRTH_DISEN, GF_DISENCHANT, BREATH},
		{4, RF4_BRTH_TIME, GF_TIME, BREATH},
		{4, RF4_BRTH_MANA, GF_MANA, BREATH},
		{5, RF5_BALL_ACID, GF_ACID, BALL},
		{5, RF5_BALL_ELEC, GF_ELEC, BALL},
		{5, RF5_BALL_FIRE, GF_FIRE, BALL},
		{5, RF5_BALL_COLD, GF_COLD, BALL},
		{5, RF5_BALL_POIS, GF_POIS, BALL},
		{5, RF5_BALL_LIGHT, GF_LIGHT, BALL},
		{5, RF5_BALL_DARK, GF_DARK, BALL},
		{5, RF5_BALL_CONFU, GF_CONFUSION, BALL},
		{5, RF5_BALL_SOUND, GF_SOUND, BALL},
		{5, RF5_BALL_SHARD, GF_SHARD, BALL},
		{5, RF5_BALL_METEOR, GF_METEOR, BALL},
		{5, RF5_BALL_STORM, GF_WATER, BALL},
		{5, RF5_BALL_NETHR, GF_NETHER, BALL},
		{5, RF5_BALL_CHAOS, GF_CHAOS, BALL},
		{5, RF5_BALL_MANA, GF_MANA, BALL},
		{5, RF5_BALL_WATER, GF_WATER, BALL},
		{5, RF5_BOLT_ACID, GF_ACID, BOLT},
		{5, RF5_BOLT_ELEC, GF_ELEC, BOLT},
		{5, RF5_BOLT_FIRE, GF_FIRE, BOLT},
		{5, RF5_BOLT_COLD, GF_COLD, BOLT},
		{5, RF5_BOLT_POIS, GF_POIS, BOLT},
		{5, RF5_BOLT_PLAS, GF_PLASMA, BOLT},
		{5, RF5_BOLT_ICE, GF_ICE, BOLT},
		{5, RF5_BOLT_WATER, GF_WATER, BOLT},
		{5, RF5_BOLT_NETHR, GF_NETHER, BOLT},
		{5, RF5_BOLT_MANA, GF_MANA, BOLT},
		{5, RF5_BOLT_GRAV, GF_GRAVITY, BOLT},
		{5, RF5_BEAM_ELEC, GF_ELEC, BEAM},
		{5, RF5_BEAM_ICE, GF_ICE, BEAM},
		{5, RF5_BEAM_NETHR, GF_NETHER, BEAM},
		{5, RF5_BEAM_LAVA, GF_LAVA, BEAM},
		{5, RF5_HOLY_ORB, GF_HOLY_ORB, BALL},
	};

	/* This array will contain the ranged attacks of nearby monsters */
	int attack[N_ELEMENTS(attack_info)];
	struct attack_info_type *a_ptr;
	int n = 0;
	monster_type *m_ptr;
	monster_race *r_ptr;
	u16b i;

	int gf_type;

	int y, x;
	int range = MAX_RANGE;

	int monsters[100];
	u16b m = 0;

	int dam;

	cptr gf_name;
	char mon_name[80];

	/* Clear the masks */
	u32b set4 = 0, set5 = 0;

	/* Collect the flags in mask variables */
	for (i = 0; i < N_ELEMENTS(attack_info); i++)
	{
		u32b *set;
		/* Get the attack info */
		a_ptr = &attack_info[i];

		/* Get the proper set */
		if (a_ptr->set == 5) set = &set5;
		else set = &set4;

		/* Set the flag */
		*set |= a_ptr->flag;
	}

	/* Find nearby monsters */
	for (y = p_ptr->py - range; y <= p_ptr->py + range; y++)
	{
		for (x = p_ptr->px - range; x <= p_ptr->px + range; x++)
		{
			/* Check bounds */
			if (!in_bounds(y, x)) continue;

			/* Not too far away */
			if (distance(y, x, p_ptr->py, p_ptr->px) > range) continue;

			/* Ignore grids not occupied by monsters  */
			if (cave_m_idx[y][x] <= SOURCE_MONSTER_START) continue;

			/* Get the monster */
			m_ptr = &mon_list[cave_m_idx[y][x]];

			/* Ignore invisible ones */
			if (!m_ptr->ml) continue;

			/* It must be in LOF */
			if (!player_can_fire_bold(y, x)) continue;

			/* Get race info */
			r_ptr = &r_info[m_ptr->r_idx];

			/* Only animals and vortices are allowed */
			if (!(r_ptr->flags3 & (RF3_ANIMAL)) && (r_ptr->d_char != 'v')) continue;

			/* Ignore monsters that cannot cast these spells */
			if (!(r_ptr->flags4 & set4) && !(r_ptr->flags5 & set5)) continue;

			/* No more place for monsters */
			if (m >= N_ELEMENTS(monsters)) continue;

			/* Add the monster */
			monsters[m++] = cave_m_idx[y][x];
		}
	}

	/* No nearby monsters */
	if (m == 0)
	{
		msg_print("There are not animals nor vortices around you!");
		return (FALSE);
	}

	/* Select a random monster */
	m_ptr = &mon_list[monsters[rand_int(m)]];

	/* Get its race */
	r_ptr = &r_info[m_ptr->r_idx];

	/* Get its attacks */
	for (i = 0; i < N_ELEMENTS(attack_info); i++)
	{
		u32b *flags;
		a_ptr = &attack_info[i];

		/* Check if the monster has the current attack */
		if (a_ptr->set == 5) flags = &r_ptr->flags5;
		else flags = &r_ptr->flags4;

		/* Append it to the list */
		if (*flags & a_ptr->flag) attack[n++] = i;
	}

	/* Paranoia. No attacks */
	if (n == 0)
	{
		msg_print("No powers!");
		return (FALSE);
	}

	/* Select a random attack */
	a_ptr = &attack_info[attack[rand_int(n)]];

	/* Get the spell type */
	gf_type = a_ptr->spell_type;

	/* Get the spell name */
	gf_name = find_spell_type_name(gf_type);

	/* Get the monster name */
	monster_desc(mon_name, sizeof(mon_name), m_ptr, 0x08);

	/* Calculate damage */
	dam = (p_ptr->chp + m_ptr->hp) / 3;
	/* Reduce damage if monster is asleep to minimize abuses */
	if (m_ptr->m_timed[MON_TMD_SLEEP]) dam /= 3;
	/* Check bounds */
	if (dam < 1) dam = 1;
	if (dam > 1000) dam = 1000;

	/* Process beam spells  */
	if (a_ptr->attack_type == BEAM)
	{
		msg_format("You cast a beam of %s like %s for %d hp damage!", gf_name, mon_name, dam);
		fire_bolt_beam_special(gf_type, dir, dam, MAX_RANGE, PROJECT_BEAM | PROJECT_NO_EFCT);
	}
	/* Process bolt spells */
	else if (a_ptr->attack_type == BOLT)
	{
		msg_format("You cast a bolt of %s like %s for %d hp damage!", gf_name, mon_name, dam);
		fire_bolt_beam_special(gf_type, dir, dam, MAX_RANGE, PROJECT_NO_EFCT);
	}
	/* Process ball spells */
	else if (a_ptr->attack_type == BALL)
	{
		msg_format("You cast a ball of %s like %s for %d hp damage!", gf_name, mon_name, dam);
		fire_ball_special(gf_type, dir, dam, 3, PROJECT_NO_EFCT, 0);
	}
	/* Process breath spells */
	else if (a_ptr->attack_type == BREATH)
	{
		msg_format("You breath %s like %s for %d hp damage!", gf_name, mon_name, dam);
		fire_arc_special(gf_type, dir, dam, 0, 60, PROJECT_NO_EFCT);
	}
	/* Paranoia */
	else
	{
		msg_print("Unknown attack type!");
		return (FALSE);
	}

	/* Update the monster list */
	p_ptr->redraw |= PR_MONLIST;

	/* Success */
	return (TRUE);
}


/*
 * Cast a chain of beams of the given type, jumping through nearby monsters. Monsters
 * must be inside the player's LOF.
 * max_hits is the maximum number of beams that will be casted.
 * decrement is a percentage of damage that is decreased with every beam. Use 0
 * to keep the damage constant.
 * Return TRUE if at least one monster was hit.
 */
bool beam_chain(int gf_type, int dam, int max_hits, int decrement)
{
	int yy, xx;

	/* Must be from from player's location */
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y = py;
	int x = px;

	int i, k;

	/* The indexes in mon_list of the reached monsters */
	u16b *monsters;
	u16b *hits;
	u16b n = 0;

	bool flag = FALSE;

	hits = C_ZNEW(mon_max, u16b);
	monsters = C_ZNEW(mon_max, u16b);

	/* Cast max_hits beams */
	for (i = 0; i < max_hits; i++)
	{
		int m_idx;
		u16b m = 0;

		/* Scan monsters as potential targets */

		for (m_idx = 0; m_idx < mon_max; m_idx++)
		{
			monster_type *m_ptr = &mon_list[m_idx];

			/* Paranoia -- Skip dead monsters */
			if (!m_ptr->r_idx) continue;

			/* It must be visible */
			if (!m_ptr->ml) continue;

			/* Get monster coordinates */
			yy = m_ptr->fy;
			xx = m_ptr->fx;

			/* It must be in the line of fire of the player */
			if(!m_ptr->project) continue;

			/* It must be in the line of fire of the previous location */
			if(!projectable(y, x, yy, xx, PROJECT_NONE)) continue;

			/* It must be close enough to the previous location */
			if (distance(y, x, yy, xx) > MAX_RANGE) continue;

			/* Find the monster in the list */
			for (k = 0; k < n; k++)
			{
				/* Found. Stop */
				if (hits[k] == m_idx) break;
			}

			/* If the monster was found in the list we just ignore it */
			if (k < n) continue;

			/* Mark the monster as a possible candidate */
			monsters[m++] = m_idx;
		}

		/* No monsters. Done */
		if (!m) break;

		/* Select a random monster from the list */
		m_idx = monsters[rand_int(m)];

		/* Get its location */
		yy = mon_list[m_idx].fy;
		xx = mon_list[m_idx].fx;

		/* Remember the monster */
		hits[n++] = m_idx;

		/* Cast the beam */
		project(SOURCE_PLAYER, 0, y, x, yy, xx, dam, gf_type,
			(PROJECT_KILL | PROJECT_BEAM), 0, 0);

		/* Success */
		flag = TRUE;

		/* Make the next beam weaker */
		dam -= (decrement * dam) / 100;

		/* No damage. Done */
		if (dam < 1) break;

		/* Remember the last location */
		y = yy;
		x = xx;

	}

	FREE(hits);
	FREE(monsters);

	return (flag);
}

/*
 * Animate nearby tress.
 * Return TRUE if it succeeds.
 */
bool call_huorns(void)
{
	/* List of locations of trees */
	u16b trees[200];
	u16b num_trees = 0;
	u16b i, n;

	/* List of attacked monsters */
	s16b hits[200];
	u16b nh = 0;

	bool flag = FALSE;

	int y, x, y2, x2;
	int range = MAX_SIGHT;

	char mon_name[80];

	y = p_ptr->py;
	x = p_ptr->px;

	/* Find nearby trees */
	for (y2 = y - range; y2 <= y + range; y2++)
	{
		for (x2 = x - range; x2 <= x + range; x2++)
		{
			/* No more space to hold the trees */
			if (num_trees >= N_ELEMENTS(trees)) continue;

			/* Ignore annyoing locations */
			if (!in_bounds(y2, x2)) continue;

			/* Ignore other features */
			if (cave_feat[y2][x2] != FEAT_TREE) continue;

			/*if (!player_can_fire_bold(y2, x2)) continue;*/

			/* Add the tree's location */
			trees[num_trees++] = GRID(y2, x2);
		}
	}

	/* Process every tree */
	for (n = 0; n < num_trees; n++)
	{

		s16b m_idx;
		int dis, best_dis = 10000;
		u16b best_grid = 0;
		bool do_destroy = FALSE;

		/* Update cave flags */
		handle_stuff();

		/* Get its location */
		y = GRID_Y(trees[n]);
		x = GRID_X(trees[n]);

		/* Scan adjacent grids */
		for (y2 = y - 1; y2 <= y + 1; y2++)
		{
			for (x2 = x - 1; x2 <= x + 1; x2++)
			{
				int dam;
				bool fear = FALSE;

				/* No more places to store monsters */
				if (nh >= N_ELEMENTS(hits)) continue;

				/* Check bounds */
				if (!in_bounds(y2, x2)) continue;

				/* Get the monster in that location */
				m_idx = cave_m_idx[y2][x2];

				/* Ignore non-monsters */
				if (m_idx <= 0) continue;

				/* It must be visible */
				if (!mon_list[m_idx].ml) continue;

				/* It must be in LOF */
				if (!player_can_fire_bold(y2, x2)) continue;

				/* Don't attack hidden monsters */
				if (mon_list[m_idx].mflag & (MFLAG_HIDE)) continue;

				/* Ignore attacked monsters */
				for (i = 0; i < nh; i++)
				{
					if (hits[i] == m_idx) break;
				}

				/* Monster was found? Done */
				if (i < nh) continue;

				/* Calculate damage */
				dam = 50 + rand_int(p_ptr->lev * 3);

				/* The monster is about to be killed? */
				if (dam > mon_list[m_idx].hp)
				{
					int y3, x3;
					bool found = FALSE;

					/* Find an adjacent spot for drops */
					for (y3 = y2 - 1; y3 <= y2 + 1; y3++)
					{
						for (x3 = x2 - 1; x3 <= x2 + 1; x3++)
						{
							/* Check bounds */
							if (!in_bounds(y3, x3)) continue;

							/* It must allow drops */
							if (!cave_ff1_match(y3, x3, FF1_DROP)) continue;


							/* Found empty place! */
							found = TRUE;

							/* Break nested loops */
							goto end_outer_for;
					       	}
					}
					end_outer_for:

					/* No place for drops */
					if (!found) continue;
				}

				/* Remember that the monster was hit */
				hits[nh++] = m_idx;

				/* Get the monster name */
				monster_desc(mon_name, sizeof(mon_name), &mon_list[m_idx], 0x08);

				msg_format("The huorn attacks %s for %d HP damage!", mon_name, dam);

				/* Take hit */
				mon_take_hit(m_idx, dam, &fear, NULL, SOURCE_PLAYER);

				/* Flavor. Enable tree destruction */
				do_destroy = TRUE;

				/* Remember this */
				flag = TRUE;
			}
		}

		/* Hack -- Avoid cloned messages */
		if (size_mon_msg > 0) flush_monster_messages();

		/* Sometimes destroy the tree */
		if (do_destroy && one_in_(10))
		{
			/* Set new feature */
			cave_set_feat(y, x, (rand_int(100) < 30) ? FEAT_BURNING_TREE: FEAT_FSOIL_DYNAMIC);
			/* Message */
			msg_print("A huorn was destroyed!");
			/* Done */
			continue;
		}

		/* Move the tree. Find target */
		for (y2 = y - range; y2 <= y + range; y2++)
		{
			for (x2 = x - range; x2 <= x + range; x2++)
			{
				int y3 = y;
				int x3 = x;
				s16b o_idx;

				/* Check bounds */
				if (!in_bounds(y2, x2)) continue;

				/* Get the monster */
				m_idx = cave_m_idx[y2][x2];

				/* Ignore non-monsters */
				if (m_idx <= 0) continue;

				/* It must be visible */
				if (!mon_list[m_idx].ml) continue;

				/* It must be in LOF */
				if (!player_can_fire_bold(y2, x2)) continue;

				/* Don't attack hidden monsters */
				if (mon_list[m_idx].mflag & (MFLAG_HIDE)) continue;

				/* Not too far away (from the player) */
				dis = distance(p_ptr->py, p_ptr->px, y2, x2);

				if (dis > best_dis) continue;

				/* We already have a target with this distance */
				if ((dis == best_dis) && best_grid) continue;

				/* Found a closest target */
				best_dis = dis;
				/* Forget previous target */
				best_grid = 0;

				/* Advance vertically */
				if (y2 > y) ++y3;
				else if (y2 < y) --y3;

				/* Advance horizontally */
				if (x2 > x) ++x3;
				else if (x2 < x) --x3;

				/* Don't move in the same grid */
				if ((x3 == x) && (y3 == y)) continue;

				/* Don't move inside vaults/pits */
				if (cave_info[y3][x3] & (CAVE_ICKY)) continue;

				/* Don't move into an occupied grid */
				if (cave_m_idx[y3][x3]) continue;

				/* Ignore other trees */
				if (cave_feat[y3][x3] == FEAT_TREE) continue;

				/* Ignore permanent features */
				if (cave_ff1_match(y3, x3, FF1_PERMANENT)) continue;

				/* Ignore dangerous grids for trees  */
				if (cave_ff3_match(y3, x3, ELEMENT_FIRE | ELEMENT_LAVA)) continue;

				/* Check presence of objects */
				for (o_idx = cave_o_idx[y3][x3]; o_idx; o_idx = o_list[o_idx].next_o_idx)
				{
					/* Get the object */
					object_type *o_ptr = &o_list[o_idx];

					/* This object must be kept intact */
					if (artifact_p(o_ptr) ||
						(k_info[o_ptr->k_idx].squelch != SQUELCH_ALWAYS) ||
						!object_aware_p(o_ptr))
					{

						break;
					}
				}

				/* Don't move over this object */
				if (o_idx) continue;

				/* Remember the best location */
				best_grid = GRID(y3, x3);
			}
		}

		/* Can we move? */
		if (best_grid)
		{
			u16b feat = FEAT_FSOIL_DYNAMIC;
			int k = rand_int(100);

			/* Add flavor */
			if (k < 10) feat = FEAT_FSOIL_D;
			else if (k < 20) feat = FEAT_BRAMBLES;
			else if (k < 25) feat = FEAT_THORNS;
			else if (k < 30) feat = FEAT_VINES;
			else if (k < 40) feat = FEAT_BUSH;
			else if (k < 50) feat = FEAT_THICKET;

			/* Restore the current grid to a passable feature */
			cave_set_feat(y, x, feat);

			/* Get the new location */
			y2 = GRID_Y(best_grid);
			x2 = GRID_X(best_grid);
			/* Destroy its objects */
			delete_object(y2, x2);
			/* Advance */
			cave_set_feat(y2, x2, FEAT_TREE);

			/* Restore the current grid to a passable feature, AGAIN!!! */
			/* This is to remove branches created by the new tree */
			cave_set_feat(y, x, feat);

			/* Remember this */
			flag = TRUE;
			/*msg_print("The huorn moves!");*/
		}
	}

	/* Update things */
	if (flag) handle_stuff();

	return (flag);
}


