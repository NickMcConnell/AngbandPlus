/* File: xtra1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
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
#include "game-event.h"




/*
 * Calculate number of spells player should have, and forget,
 * or remember, spells until that number is properly reflected.
 *
 * Note that this function induces various "status" messages,
 * which must be bypasses until the character is created.
 */
static void calc_spells(void)
{
	int i, j, k, levels;
	int num_allowed, num_known;
	int percent_spells;

	const magic_type *s_ptr;

	s16b old_spells;

	cptr p = ((cp_ptr->spell_book == TV_PRAYER_BOOK) ? "prayer" : "spell" );

	/* Hack -- must be literate */
	if (!cp_ptr->spell_book) return;

	/* Hack -- wait for creation */
	if (!character_generated) return;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Save the new_spells value */
	old_spells = p_ptr->new_spells;

	/* Determine the number of spells allowed */
	levels = p_ptr->lev - cp_ptr->spell_first + 1;

	/* Hack -- no negative spells */
	if (levels < 0) levels = 0;

	/* Number of 1/100 spells per level */
	percent_spells = adj_mag_study[SPELL_STAT_SLOT];

	/* Extract total allowed spells (rounded up) */
	num_allowed = (((percent_spells * levels) + 50) / 100);

	/* Assume none known */
	num_known = 0;

	/* Count the number of spells we know */
	for (j = 0; j < PY_MAX_SPELLS; j++)
	{
		/* Count known spells */
		if (p_ptr->spell_flags[j] & PY_SPELL_LEARNED)
		{
			num_known++;
		}
	}

	/* See how many spells we must forget or may learn */
	p_ptr->new_spells = num_allowed - num_known;

	/* Forget spells which are too hard */
	for (i = PY_MAX_SPELLS - 1; i >= 0; i--)
	{
		/* Get the spell */
		j = p_ptr->spell_order[i];

		/* Skip non-spells */
		if (j >= 99) continue;

		/* Get the spell */
		s_ptr = &mp_ptr->info[j];

		/* Skip spells we are allowed to know */
		if (s_ptr->slevel <= p_ptr->lev) continue;

		/* Is it known? */
		if (p_ptr->spell_flags[j] & PY_SPELL_LEARNED)
		{
			/* Mark as forgotten */
			p_ptr->spell_flags[j] |= PY_SPELL_FORGOTTEN;

			/* No longer known */
			p_ptr->spell_flags[j] &= ~PY_SPELL_LEARNED;

			/* Message */
			msg_format("You have forgotten the %s of %s.", p,
			           get_spell_name(cp_ptr->spell_book, j));

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}

	/* Forget spells if we know too many spells */
	for (i = PY_MAX_SPELLS - 1; i >= 0; i--)
	{
		/* Stop when possible */
		if (p_ptr->new_spells >= 0) break;

		/* Get the (i+1)th spell learned */
		j = p_ptr->spell_order[i];

		/* Skip unknown spells */
		if (j >= 99) continue;

		/* Forget it (if learned) */
		if (p_ptr->spell_flags[j] & PY_SPELL_LEARNED)
		{
			/* Mark as forgotten */
			p_ptr->spell_flags[j] |= PY_SPELL_FORGOTTEN;

			/* No longer known */
			p_ptr->spell_flags[j] &= ~PY_SPELL_LEARNED;

			/* Message */
			msg_format("You have forgotten the %s of %s.", p,
			           get_spell_name(cp_ptr->spell_book, j));

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}

	/* Check for spells to remember */
	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
		/* None left to remember */
		if (p_ptr->new_spells <= 0) break;

		/* Get the next spell we learned */
		j = p_ptr->spell_order[i];

		/* Skip unknown spells */
		if (j >= 99) break;

		/* Get the spell */
		s_ptr = &mp_ptr->info[j];

		/* Skip spells we cannot remember */
		if (s_ptr->slevel > p_ptr->lev) continue;

		/* First set of spells */
		if (p_ptr->spell_flags[j] & PY_SPELL_FORGOTTEN)
		{
			/* No longer forgotten */
			p_ptr->spell_flags[j] &= ~PY_SPELL_FORGOTTEN;

			/* Known once more */
			p_ptr->spell_flags[j] |= PY_SPELL_LEARNED;

			/* Message */
			msg_format("You have remembered the %s of %s.",
			           p, get_spell_name(cp_ptr->spell_book, j));

			/* One less can be learned */
			p_ptr->new_spells--;
		}
	}

	/* Assume no spells available */
	k = 0;

	/* Count spells that can be learned */
	for (j = 0; j < PY_MAX_SPELLS; j++)
	{

		/* Get the spell */
		s_ptr = &mp_ptr->info[j];

		/* Skip spells we cannot remember */
		if (s_ptr->slevel > p_ptr->lev) continue;

		/* Don't count Ironman Spells. */
		if (p_ptr->spell_flags[j] & PY_SPELL_IRONMAN) continue;

		/* Skip spells we already know */
		if (p_ptr->spell_flags[j] & PY_SPELL_LEARNED)
		{
			continue;
		}

		/* Count it */
		k++;
	}

	/* Cannot learn more spells than exist */
	if (p_ptr->new_spells > k) p_ptr->new_spells = k;

	/* Spell count changed */
	if (old_spells != p_ptr->new_spells)
	{
		/* Message if needed */
		if (p_ptr->new_spells)
		{
			/* Message */
			msg_format("You can learn %d more %s%s.",
			           p_ptr->new_spells, p,
			           (p_ptr->new_spells != 1) ? "s" : "");
		}

		/* Redraw Study Status */
		p_ptr->redraw |= (PR_STUDY | PR_OBJECT);
	}

}


/*
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor.
 *
 * This function induces status messages.
 */
static void calc_mana(void)
{
	int msp, levels, cur_wgt, max_wgt;

	object_type *o_ptr;

	bool old_cumber_glove = p_ptr->cumber_glove;
	bool old_cumber_armor = p_ptr->cumber_armor;

	/* Hack -- Must be literate */
	if (!cp_ptr->spell_book) return;

	/* Extract "effective" player level */
	levels = (p_ptr->lev - cp_ptr->spell_first) + 1;

	/* Hack -- no negative mana */
	if (levels < 0) levels = 0;

	msp = (long)adj_mag_mana[SPELL_STAT_SLOT] * levels / 100;

	/* Hack -- usually add one mana */
	if (msp) msp++;

	/* Process gloves for those disturbed by them */
	if (cp_ptr->flags & CF_CUMBER_GLOVE)
	{
		u32b f1, f2, f3, native;

		/* Assume player is not encumbered by gloves */
		p_ptr->cumber_glove = FALSE;

		/* Get the gloves */
		o_ptr = &inventory[INVEN_HANDS];

		/* Examine the gloves */
		object_flags(o_ptr, &f1, &f2, &f3, &native);

		/* Normal gloves hurt mage-type spells */
		if (o_ptr->k_idx &&
		    !(f3 & (TR3_FREE_ACT)) &&
		    !((f1 & (TR1_DEX)) && (o_ptr->pval > 0)))
		{
			/* Encumbered */
			p_ptr->cumber_glove = TRUE;

			/* Reduce mana */
			msp = (3 * msp) / 4;
		}
	}


	/* Assume player not encumbered by armor */
	p_ptr->cumber_armor = FALSE;

	/* Weigh the armor */
	cur_wgt = 0;
	cur_wgt += inventory[INVEN_BODY].weight;
	cur_wgt += inventory[INVEN_HEAD].weight;
	cur_wgt += inventory[INVEN_ARM].weight;
	cur_wgt += inventory[INVEN_OUTER].weight;
	cur_wgt += inventory[INVEN_HANDS].weight;
	cur_wgt += inventory[INVEN_FEET].weight;

	/* Determine the weight allowance */
	max_wgt = cp_ptr->spell_weight;

	/* Heavy armor penalizes mana */
	if (((cur_wgt - max_wgt) / 10) > 0)
	{
		/* Encumbered */
		p_ptr->cumber_armor = TRUE;

		/* Reduce mana */
		msp -= ((cur_wgt - max_wgt) / 10);
	}


	/* Mana can never be negative */
	if (msp < 0) msp = 0;


	/* Maximum mana has changed */
	if (p_ptr->msp != msp)
	{
		/* Save new limit */
		p_ptr->msp = msp;

		/* Enforce new limit */
		if (p_ptr->csp >= msp)
		{
			p_ptr->csp = msp;
			p_ptr->csp_frac = 0;
		}

		/* Display mana later */
		p_ptr->redraw |= (PR_MANA);

	}


	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Take note when "glove state" changes */
	if (old_cumber_glove != p_ptr->cumber_glove)
	{
		/* Message */
		if (p_ptr->cumber_glove)
		{
			msg_print("Your covered hands feel unsuitable for spellcasting.");
		}
		else
		{
			msg_print("Your hands feel more suitable for spellcasting.");
		}
	}


	/* Take note when "armor state" changes */
	if (old_cumber_armor != p_ptr->cumber_armor)
	{
		/* Message */
		if (p_ptr->cumber_armor)
		{
			msg_print("The weight of your armor encumbers your movement.");
		}
		else
		{
			msg_print("You feel able to move more freely.");
		}
	}
}



/*
 * Calculate the players (maximal) hit points
 *
 * Adjust current hitpoints if necessary
 */
static void calc_hitpoints(void)
{
	long bonus;
	int mhp;

	/* Get "1/100th hitpoint bonus per level" value */
	bonus = adj_con_mhp[p_ptr->state.stat_ind[A_CON]];

	/* Calculate hitpoints */
	mhp = p_ptr->player_hp[p_ptr->lev-1] + (bonus * p_ptr->lev / 100);

	/* Always have at least one hitpoint per level */
	if (mhp < p_ptr->lev + 1) mhp = p_ptr->lev + 1;

	/* New maximum hitpoints */
	if (p_ptr->mhp != mhp)
	{
		int i = 100;

		/* Get percentage of maximum hp */
		if (p_ptr->mhp) i = ((100 * p_ptr->chp) / p_ptr->mhp);

		/* Save new limit */
		p_ptr->mhp = mhp;

		/* Update current maximum hp */
		p_ptr->chp = ((i * p_ptr->mhp) / 100) + (((i * p_ptr->mhp) % 100 >= 50)	? 1 : 0);

		/* Hack - any change in max hitpoint resets frac */
		p_ptr->chp_frac = 0;

		/* Dihplay hp later */
		p_ptr->redraw |= (PR_HP);

	}
}



/*
 * Extract and set the current "lite radius"
 */
static void calc_torch(void)
{
	int i;
	object_type *o_ptr;
	u32b f1, f2, f3, native;

	s16b old_light = p_ptr->cur_light;

	/* Assume no light */
	p_ptr->cur_light = 0;

	/* Loop through all wielded items */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Examine actual lites */
		if (o_ptr->tval == TV_LIGHT)
		{
			/* Artifact Lites provide permanent, bright, lite */
			if (artifact_p(o_ptr))
			{
				p_ptr->cur_light += 3;
				continue;
			}

			/* Lanterns (with fuel) provide more lite */
			if ((o_ptr->sval == SV_LIGHT_LANTERN) && (o_ptr->timeout > 0))
			{
				object_flags(o_ptr, &f1, &f2, &f3, &native);

				/* Resist Dark means light radius of 3 */
				if ((f2 & TR2_RES_DARK) && (o_ptr->pval >= 0))
				{
				  p_ptr->cur_light += 3;
				}
				/* Resist Light means light radius of 1 */
				/* The same with cursed lanterns */
				else if ((f2 & TR2_RES_LIGHT) || (o_ptr->pval < 0))
				{
				  p_ptr->cur_light += 1;
				}
				else
				{
				  p_ptr->cur_light += 2;
				}

				continue;
			}

			/* Torches (with fuel) provide some lite */
			if ((o_ptr->sval == SV_LIGHT_TORCH) && (o_ptr->timeout > 0))
			{
				p_ptr->cur_light += 1;
				continue;
			}
		}
		else
		{
			/* Extract the flags */
			object_flags(o_ptr, &f1, &f2, &f3, &native);

			/* does this item glow? */
			if (f3 & TR3_LIGHT) p_ptr->cur_light++;
		}
	}


	/* Player is glowing */
	if (p_ptr->state.light) p_ptr->cur_light++;


	/* Notice changes in the "lite radius" */

	/* Update the visuals */
	p_ptr->update |= (PU_UPDATE_VIEW);
	p_ptr->update |= (PU_MONSTERS);

	/* Notice changes in the "lite radius" */
	 if (old_light != p_ptr->cur_light)
	{
		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}
}

/*
 * Extract and set the current nativity status
 */
static void calc_nativity(void)
{
	int i;
	object_type *o_ptr;
	u32b f1, f2, f3, fn;

	/* Extract the player flags */
	player_flags(&f1, &f2, &f3, &fn);

	/* Assume nothing */
	p_ptr->p_native = 0L;

	/* Assume nothing known */
	p_ptr->p_native_known = 0L;

	/*Class and Race Native flags*/
	p_ptr->p_native |= fn;
	p_ptr->p_native_known |= fn;

	/* Loop through all wielded items */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3, &fn);

		p_ptr->p_native |= fn;

		/* Don't know all the flags */
		if (!object_known_p(o_ptr)) continue;

		/* Extract the flags */
		object_flags_known(o_ptr, &f1, &f2, &f3, &fn);

		p_ptr->p_native_known |= fn;

	}

	/*Manually add the temporary native flags.  Assume known*/
	if (p_ptr->timed[TMD_NAT_LAVA])
	{
		p_ptr->p_native |= P_NATIVE_LAVA;
		p_ptr->p_native_known |= P_NATIVE_LAVA;
	}
	if (p_ptr->timed[TMD_NAT_OIL])
	{
		p_ptr->p_native |= P_NATIVE_OIL;
		p_ptr->p_native_known |= P_NATIVE_OIL;
	}
	if (p_ptr->timed[TMD_NAT_SAND])
	{
		p_ptr->p_native |= P_NATIVE_SAND;
		p_ptr->p_native_known |= P_NATIVE_SAND;
	}
	if (p_ptr->timed[TMD_NAT_TREE])
	{
		p_ptr->p_native |= P_NATIVE_FOREST;
		p_ptr->p_native_known |= P_NATIVE_FOREST;
	}
	if (p_ptr->timed[TMD_NAT_WATER])
	{
		p_ptr->p_native |= P_NATIVE_WATER;
		p_ptr->p_native_known |= P_NATIVE_WATER;
	}
	if (p_ptr->timed[TMD_NAT_MUD])
	{
		p_ptr->p_native |= P_NATIVE_MUD;
		p_ptr->p_native_known |= P_NATIVE_MUD;
	}

}

/*
 * Computes current weight limit.
 */
static int weight_limit(void)
{
	int i;

	/* Weight limit based only on strength */
	i = adj_str_wgt[p_ptr->state.stat_ind[A_STR]] * 100;

	/* Return the result */
	return (i);
}


/*Re-calculate the player stealth*/
static void calc_stealth(void)
{
	int old_skill_stl, i;

	/* Save the old stealth */
	old_skill_stl = p_ptr->state.skills[SKILL_STEALTH];

	/* Base skill -- stealth */
	p_ptr->state.skills[SKILL_STEALTH] = rp_ptr->r_stl + cp_ptr->c_stl;

	/* Very simple if flying */
	if (p_ptr->timed[TMD_FLYING])
	{
		/*Very quiet*/
		p_ptr->state.skills[SKILL_STEALTH] += 3;
	}

	/* Scan the equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		u32b f1, f2, f3, native;

		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the item flags */
		object_flags(o_ptr, &f1, &f2, &f3, &native);

		/* Affect stealth */
		if (f1 & (TR1_STEALTH)) p_ptr->state.skills[SKILL_STEALTH] += o_ptr->pval;
	}

	/* Affect Skill -- stealth (bonus one) */
	p_ptr->state.skills[SKILL_STEALTH] += 1;

	/* Affect Skill -- stealth (Level, by Class) */
	p_ptr->state.skills[SKILL_STEALTH] += (cp_ptr->x_stl * p_ptr->lev / 10);

	/*Feature affects skill*/
	p_ptr->state.skills[SKILL_STEALTH] += f_info[cave_feat[p_ptr->py][p_ptr->px]].f_stealth_adj;

	/* Limit Skill -- stealth from 0 to 30 */
	if (p_ptr->state.skills[SKILL_STEALTH] > 30) p_ptr->state.skills[SKILL_STEALTH] = 30;
	if (p_ptr->state.skills[SKILL_STEALTH] < 0) p_ptr->state.skills[SKILL_STEALTH] = 0;

	/* Recalculate stealth when needed */
	if ((p_ptr->state.skills[SKILL_STEALTH] != old_skill_stl) || (!p_ptr->state.skills[SKILL_STEALTH]))
	{
		/* Assume character is extremely noisy. */
		p_ptr->base_wakeup_chance = 100 * WAKEUP_ADJ;

		/* For every increase in stealth past 0, multiply wakeup chance by 0.8. */
		for (i = 0; i < p_ptr->state.skills[SKILL_STEALTH]; i++)
		{
			p_ptr->base_wakeup_chance = 4 * p_ptr->base_wakeup_chance / 5;

			/* Always make at least some innate noise */
			if (p_ptr->base_wakeup_chance < 50)
			{
				p_ptr->base_wakeup_chance = 50;
				break;
			}
		}
	}
}

/*
 * Go through player inventory, equipment, and quiver to calculate the player weight.
 */
static void calc_player_weight(void)
{
	int i;
	object_type *o_ptr;

	p_ptr->total_weight = 0;

	/*
	 * First, scan the backpack 0 to INVEN_PACK,
	 * then the equipment INVEN_WIELD to INVEN_MAX_PACK,
	 * then the quiver QUIVER_START to QUIVER_END/ALL_INVEN_TOTAL
	 */
	for (i = 0; i < ALL_INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		p_ptr->total_weight += o_ptr->weight * o_ptr->number;
	}
}

/*
 * Go through player inventory, equipment, and quiver to calculate the player weight.
 */
static void calc_inven_cnt(void)
{
	int i;
	object_type *o_ptr;

	p_ptr->inven_cnt = 0;

	/*
	 * First, scan the backpack 0 to INVEN_PACK,
	 * then the equipment INVEN_WIELD to INVEN_MAX_PACK,
	 * then the quiver QUIVER_START to QUIVER_END/ALL_INVEN_TOTAL
	 */
	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		p_ptr->inven_cnt++;
	}
}

/*
 * Calculate the players current "state", taking into account
 * not only race/class intrinsics, but also objects being worn
 * and temporary spell effects.
 *
 * See also calc_mana() and calc_hitpoints().
 *
 * Take note of the new "speed code", in particular, a very strong
 * player will start slowing down as soon as he reaches 150 pounds,
 * but not until he reaches 450 pounds will he be half as fast as
 * a normal kobold.  This both hurts and helps the player, hurts
 * because in the old days a player could just avoid 300 pounds,
 * and helps because now carrying 300 pounds is not very painful.
 *
 * The "weapon" and "bow" do *not* add to the bonuses to hit or to
 * damage, since that would affect non-combat things.  These values
 * are actually added in later, at the appropriate place.
 *
 * This function induces various "status" messages.
 */
static void calc_bonuses(void)
{

	int i, j, hold;

	int old_speed;

	int old_telepathy;
	int old_see_inv;


	int old_dis_ac;
	int old_dis_to_a;

	int extra_blows;
	int extra_shots;
	int extra_might;

	int old_stat_top[A_MAX];
	int old_stat_use[A_MAX];
	int old_stat_ind[A_MAX];

	bool old_heavy_shoot;
	bool old_heavy_wield;
	bool old_icky_wield;

	object_type *o_ptr;

	u32b f1, f2, f3, fn;


	/*** Memorize ***/

	/* Save the old speed */
	old_speed = p_ptr->state.p_speed;

	/* confirm equipment weight and backpack quantity */
	calc_player_weight();
	calc_inven_cnt();

	/* Save the old vision stuff */
	old_telepathy = p_ptr->state.telepathy;
	old_see_inv = p_ptr->state.see_inv;

	/* Save the old armor class */
	old_dis_ac = p_ptr->state.dis_ac;
	old_dis_to_a = p_ptr->state.dis_to_a;

	/* Save the old stats */
	for (i = 0; i < A_MAX; i++)
	{
		old_stat_top[i] = p_ptr->state.stat_top[i];
		old_stat_use[i] = p_ptr->state.stat_use[i];
		old_stat_ind[i] = p_ptr->state.stat_ind[i];
	}

	old_heavy_shoot = p_ptr->heavy_shoot;
	old_heavy_wield = p_ptr->heavy_wield;
	old_icky_wield = p_ptr->icky_wield;


	/*** Reset ***/

	/* Reset player speed */
	p_ptr->state.p_speed = 110;

	/* Reset "blow" info */
	p_ptr->state.num_blow = 1;
	extra_blows = 0;

	/* Reset "fire" info */
	p_ptr->state.num_fire = 0;
	p_ptr->state.ammo_mult = 0;
	p_ptr->state.ammo_tval = 0;
	extra_shots = 0;
	extra_might = 0;

	/* Clear the stat modifiers */
	for (i = 0; i < A_MAX; i++) p_ptr->state.stat_add[i] = 0;

	/* Clear the Displayed/Real armor class */
	p_ptr->state.dis_ac = p_ptr->state.ac = 0;

	/* Clear the Displayed/Real Bonuses */
	p_ptr->state.dis_to_h = p_ptr->state.to_h = 0;
	p_ptr->state.dis_to_d = p_ptr->state.to_d = 0;
	p_ptr->state.dis_to_a = p_ptr->state.to_a = 0;

	/* Clear all the flags */
	p_ptr->state.aggravate = FALSE;
	p_ptr->state.teleport = FALSE;
	p_ptr->state.exp_drain = FALSE;
	p_ptr->state.bless_blade = FALSE;
	p_ptr->state.impact = FALSE;
	p_ptr->state.see_inv = FALSE;
	p_ptr->state.free_act = FALSE;
	p_ptr->state.slow_digest = FALSE;
	p_ptr->state.regenerate = FALSE;
	p_ptr->state.ffall = FALSE;
	p_ptr->state.hold_life = FALSE;
	p_ptr->state.telepathy = FALSE;
	p_ptr->state.light = FALSE;
	p_ptr->state.sustain_str = FALSE;
	p_ptr->state.sustain_int = FALSE;
	p_ptr->state.sustain_wis = FALSE;
	p_ptr->state.sustain_con = FALSE;
	p_ptr->state.sustain_dex = FALSE;
	p_ptr->state.sustain_chr = FALSE;
	p_ptr->state.resist_acid = FALSE;
	p_ptr->state.resist_elec = FALSE;
	p_ptr->state.resist_fire = FALSE;
	p_ptr->state.resist_cold = FALSE;
	p_ptr->state.resist_pois = FALSE;
	p_ptr->state.resist_fear = FALSE;
	p_ptr->state.resist_light = FALSE;
	p_ptr->state.resist_dark = FALSE;
	p_ptr->state.resist_blind = FALSE;
	p_ptr->state.resist_confu = FALSE;
	p_ptr->state.resist_sound = FALSE;
	p_ptr->state.resist_chaos = FALSE;
	p_ptr->state.resist_disen = FALSE;
	p_ptr->state.resist_shard = FALSE;
	p_ptr->state.resist_nexus = FALSE;
	p_ptr->state.resist_nethr = FALSE;
	p_ptr->state.immune_acid = FALSE;
	p_ptr->state.immune_elec = FALSE;
	p_ptr->state.immune_fire = FALSE;
	p_ptr->state.immune_cold = FALSE;
	p_ptr->state.immune_pois = FALSE;

	/*** Extract race/class info ***/

	/* Base infravision plus current lite radius */
	p_ptr->state.see_infra = rp_ptr->infra;

	/* Base skill -- disarming */
	p_ptr->state.skills[SKILL_DISARM] = rp_ptr->r_dis + cp_ptr->c_dis;

	/* Base skill -- magic devices */
	p_ptr->state.skills[SKILL_DEVICE] = rp_ptr->r_dev + cp_ptr->c_dev;

	/* Base skill -- saving throw */
	p_ptr->state.skills[SKILL_SAVE] = rp_ptr->r_sav + cp_ptr->c_sav;

	/* Base skill -- searching ability */
	p_ptr->state.skills[SKILL_SEARCH] = rp_ptr->r_srh + cp_ptr->c_srh;

	/* Base skill -- searching frequency */
	p_ptr->state.skills[SKILL_SEARCH_FREQUENCY] = rp_ptr->r_fos + cp_ptr->c_fos;

	/* Base skill -- combat (normal) */
	p_ptr->state.skills[SKILL_TO_HIT_MELEE] = rp_ptr->r_thn + cp_ptr->c_thn;

	/* Base skill -- combat (shooting) */
	p_ptr->state.skills[SKILL_TO_HIT_BOW] = rp_ptr->r_thb + cp_ptr->c_thb;

	/* Base skill -- combat (throwing) */
	p_ptr->state.skills[SKILL_TO_HIT_THROW] = rp_ptr->r_thb + cp_ptr->c_thb;

	/* Base skill -- digging */
	p_ptr->state.skills[SKILL_DIGGING] = 0;

	/*** Analyze player ***/

	/* Extract the player flags */
	player_flags(&f1, &f2, &f3, &fn);

	/* Good flags */
	if (f3 & (TR3_SLOW_DIGEST)) p_ptr->state.slow_digest = TRUE;
	if (f3 & (TR3_FEATHER)) p_ptr->state.ffall = TRUE;
	if (f3 & (TR3_LIGHT)) p_ptr->state.light = TRUE;
	if (f3 & (TR3_REGEN)) p_ptr->state.regenerate = TRUE;
	if (f3 & (TR3_TELEPATHY)) p_ptr->state.telepathy = TRUE;
	if (f3 & (TR3_SEE_INVIS)) p_ptr->state.see_inv = TRUE;
	if (f3 & (TR3_FREE_ACT)) p_ptr->state.free_act = TRUE;
	if (f3 & (TR3_HOLD_LIFE)) p_ptr->state.hold_life = TRUE;

	/* Weird flags */
	if (f3 & (TR3_BLESSED)) p_ptr->state.bless_blade = TRUE;

	/* Bad flags */
	if (f3 & (TR3_IMPACT)) p_ptr->state.impact = TRUE;
	if (f3 & (TR3_AGGRAVATE)) p_ptr->state.aggravate = TRUE;
	if (f3 & (TR3_TELEPORT)) p_ptr->state.teleport = TRUE;
	if (f3 & (TR3_DRAIN_EXP)) p_ptr->state.exp_drain = TRUE;

	/* Immunity flags */
	if (f2 & (TR2_IM_FIRE)) p_ptr->state.immune_fire = TRUE;
	if (f2 & (TR2_IM_ACID)) p_ptr->state.immune_acid = TRUE;
	if (f2 & (TR2_IM_COLD)) p_ptr->state.immune_cold = TRUE;
	if (f2 & (TR2_IM_ELEC)) p_ptr->state.immune_elec = TRUE;
	if (f2 & (TR2_IM_POIS)) p_ptr->state.immune_pois = TRUE;

	/* Resistance flags */
	if (f2 & (TR2_RES_ACID)) p_ptr->state.resist_acid = TRUE;
	if (f2 & (TR2_RES_ELEC)) p_ptr->state.resist_elec = TRUE;
	if (f2 & (TR2_RES_FIRE)) p_ptr->state.resist_fire = TRUE;
	if (f2 & (TR2_RES_COLD)) p_ptr->state.resist_cold = TRUE;
	if (f2 & (TR2_RES_POIS)) p_ptr->state.resist_pois = TRUE;
	if (f2 & (TR2_RES_FEAR)) p_ptr->state.resist_fear = TRUE;
	if (f2 & (TR2_RES_LIGHT)) p_ptr->state.resist_light = TRUE;
	if (f2 & (TR2_RES_DARK)) p_ptr->state.resist_dark = TRUE;
	if (f2 & (TR2_RES_BLIND)) p_ptr->state.resist_blind = TRUE;
	if (f2 & (TR2_RES_CONFU)) p_ptr->state.resist_confu = TRUE;
	if (f2 & (TR2_RES_SOUND)) p_ptr->state.resist_sound = TRUE;
	if (f2 & (TR2_RES_SHARD)) p_ptr->state.resist_shard = TRUE;
	if (f2 & (TR2_RES_NEXUS)) p_ptr->state.resist_nexus = TRUE;
	if (f2 & (TR2_RES_NETHR)) p_ptr->state.resist_nethr = TRUE;
	if (f2 & (TR2_RES_CHAOS)) p_ptr->state.resist_chaos = TRUE;
	if (f2 & (TR2_RES_DISEN)) p_ptr->state.resist_disen = TRUE;

	/* Sustain flags */
	if (f2 & (TR2_SUST_STR)) p_ptr->state.sustain_str = TRUE;
	if (f2 & (TR2_SUST_INT)) p_ptr->state.sustain_int = TRUE;
	if (f2 & (TR2_SUST_WIS)) p_ptr->state.sustain_wis = TRUE;
	if (f2 & (TR2_SUST_DEX)) p_ptr->state.sustain_dex = TRUE;
	if (f2 & (TR2_SUST_CON)) p_ptr->state.sustain_con = TRUE;
	if (f2 & (TR2_SUST_CHR)) p_ptr->state.sustain_chr = TRUE;


	/*** Analyze equipment ***/

	/* Scan the equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the item flags */
		object_flags(o_ptr, &f1, &f2, &f3, &fn);

		/* Affect stats */
		if (f1 & (TR1_STR)) p_ptr->state.stat_add[A_STR] += o_ptr->pval;
		if (f1 & (TR1_INT)) p_ptr->state.stat_add[A_INT] += o_ptr->pval;
		if (f1 & (TR1_WIS)) p_ptr->state.stat_add[A_WIS] += o_ptr->pval;
		if (f1 & (TR1_DEX)) p_ptr->state.stat_add[A_DEX] += o_ptr->pval;
		if (f1 & (TR1_CON)) p_ptr->state.stat_add[A_CON] += o_ptr->pval;
		if (f1 & (TR1_CHR)) p_ptr->state.stat_add[A_CHR] += o_ptr->pval;

		/* Affect searching ability (factor of five) */
		if (f1 & (TR1_SEARCH)) p_ptr->state.skills[SKILL_SEARCH] += (o_ptr->pval * 5);

		/* Affect searching frequency (factor of five) */
		if (f1 & (TR1_SEARCH)) p_ptr->state.skills[SKILL_SEARCH_FREQUENCY] += (o_ptr->pval * 5);

		/* Affect infravision */
		if (f1 & (TR1_INFRA)) p_ptr->state.see_infra += o_ptr->pval;

		/* Affect digging (factor of 20) */
		if (f1 & (TR1_TUNNEL)) p_ptr->state.skills[SKILL_DIGGING] += (o_ptr->pval * 20);

		/* Affect speed */
		if (f1 & (TR1_SPEED)) p_ptr->state.p_speed += o_ptr->pval;

		/* Affect blows */
		if (f1 & (TR1_BLOWS)) extra_blows += o_ptr->pval;

		/* Affect shots */
		if (f1 & (TR1_SHOTS)) extra_shots += o_ptr->pval;

		/* Affect Might */
		if (f1 & (TR1_MIGHT)) extra_might += o_ptr->pval;

		/* Good flags */
		if (f3 & (TR3_SLOW_DIGEST)) p_ptr->state.slow_digest = TRUE;
		if (f3 & (TR3_FEATHER)) p_ptr->state.ffall = TRUE;
		if (f3 & (TR3_REGEN)) p_ptr->state.regenerate = TRUE;
		if (f3 & (TR3_TELEPATHY)) p_ptr->state.telepathy = TRUE;
		if (f3 & (TR3_SEE_INVIS)) p_ptr->state.see_inv = TRUE;
		if (f3 & (TR3_FREE_ACT)) p_ptr->state.free_act = TRUE;
		if (f3 & (TR3_HOLD_LIFE)) p_ptr->state.hold_life = TRUE;

		/* Weird flags */
		if (f3 & (TR3_BLESSED)) p_ptr->state.bless_blade = TRUE;

		/* Bad flags */
		if (f3 & (TR3_IMPACT)) p_ptr->state.impact = TRUE;
		if (f3 & (TR3_AGGRAVATE)) p_ptr->state.aggravate = TRUE;
		if (f3 & (TR3_TELEPORT)) p_ptr->state.teleport = TRUE;
		if (f3 & (TR3_DRAIN_EXP)) p_ptr->state.exp_drain = TRUE;

		/* Immunity flags */
		if (f2 & (TR2_IM_FIRE)) p_ptr->state.immune_fire = TRUE;
		if (f2 & (TR2_IM_ACID)) p_ptr->state.immune_acid = TRUE;
		if (f2 & (TR2_IM_COLD)) p_ptr->state.immune_cold = TRUE;
		if (f2 & (TR2_IM_ELEC)) p_ptr->state.immune_elec = TRUE;
		if (f2 & (TR2_IM_POIS)) p_ptr->state.immune_pois = TRUE;

		/* Resistance flags */
		if (f2 & (TR2_RES_ACID)) p_ptr->state.resist_acid = TRUE;
		if (f2 & (TR2_RES_ELEC)) p_ptr->state.resist_elec = TRUE;
		if (f2 & (TR2_RES_FIRE)) p_ptr->state.resist_fire = TRUE;
		if (f2 & (TR2_RES_COLD)) p_ptr->state.resist_cold = TRUE;
		if (f2 & (TR2_RES_POIS)) p_ptr->state.resist_pois = TRUE;
		if (f2 & (TR2_RES_FEAR)) p_ptr->state.resist_fear = TRUE;
		if (f2 & (TR2_RES_LIGHT)) p_ptr->state.resist_light = TRUE;
		if (f2 & (TR2_RES_DARK)) p_ptr->state.resist_dark = TRUE;
		if (f2 & (TR2_RES_BLIND)) p_ptr->state.resist_blind = TRUE;
		if (f2 & (TR2_RES_CONFU)) p_ptr->state.resist_confu = TRUE;
		if (f2 & (TR2_RES_SOUND)) p_ptr->state.resist_sound = TRUE;
		if (f2 & (TR2_RES_SHARD)) p_ptr->state.resist_shard = TRUE;
		if (f2 & (TR2_RES_NEXUS)) p_ptr->state.resist_nexus = TRUE;
		if (f2 & (TR2_RES_NETHR)) p_ptr->state.resist_nethr = TRUE;
		if (f2 & (TR2_RES_CHAOS)) p_ptr->state.resist_chaos = TRUE;
		if (f2 & (TR2_RES_DISEN)) p_ptr->state.resist_disen = TRUE;

		/* Sustain flags */
		if (f2 & (TR2_SUST_STR)) p_ptr->state.sustain_str = TRUE;
		if (f2 & (TR2_SUST_INT)) p_ptr->state.sustain_int = TRUE;
		if (f2 & (TR2_SUST_WIS)) p_ptr->state.sustain_wis = TRUE;
		if (f2 & (TR2_SUST_DEX)) p_ptr->state.sustain_dex = TRUE;
		if (f2 & (TR2_SUST_CON)) p_ptr->state.sustain_con = TRUE;
		if (f2 & (TR2_SUST_CHR)) p_ptr->state.sustain_chr = TRUE;

		/* Modify the base armor class */
		p_ptr->state.ac += o_ptr->ac;

		/* The base armor class is always known */
		p_ptr->state.dis_ac += o_ptr->ac;

		/* Apply the bonuses to armor class */
		p_ptr->state.to_a += o_ptr->to_a;

		/* Apply the mental bonuses to armor class, if known */
		if (object_known_p(o_ptr)) p_ptr->state.dis_to_a += o_ptr->to_a;

		/* Hack -- do not apply "weapon" bonuses */
		if (i == INVEN_WIELD) continue;

		/* Hack -- do not apply "bow" bonuses */
		if (i == INVEN_BOW) continue;

		/* Apply the bonuses to hit/damage */
		p_ptr->state.to_h += o_ptr->to_h;
		p_ptr->state.to_d += o_ptr->to_d;

		/* Apply the mental bonuses tp hit/damage, if known */
		if (object_known_p(o_ptr)) p_ptr->state.dis_to_h += o_ptr->to_h;
		if (object_known_p(o_ptr)) p_ptr->state.dis_to_d += o_ptr->to_d;
	}

	/* Find cursed ammo in the quiver */
	p_ptr->cursed_quiver = FALSE;

	/* Scan the quiver */
	for (i = QUIVER_START; i < QUIVER_END; i++)
	{
		/* Get the object */
		o_ptr = &inventory[i];

		/* Ignore empty objects */
		if (!o_ptr->k_idx) continue;

		/* Found cursed ammo */
		if (cursed_p(o_ptr))
		{
			/* Remember it */
			p_ptr->cursed_quiver = TRUE;

			/* Done */
			break;
		}
	}

	/*finally, add infravision to lite radius*/
	if (p_ptr->state.see_infra) p_ptr->state.see_infra += p_ptr->cur_light;

	/*** Handle stats ***/

	/* Calculate stats */
	for (i = 0; i < A_MAX; i++)
	{
		int add, top, use, ind;

		/* Extract modifier */
		add = p_ptr->state.stat_add[i];

		/* Maximize mode */
		if (adult_maximize)
		{
			/* Modify the stats for race/class */
			add += (rp_ptr->r_adj[i] + cp_ptr->c_adj[i]);

		}

		/* Extract the new "stat_top" value for the stat */
		top = modify_stat_value(p_ptr->stat_max[i], add);

		/* Save the new value */
		p_ptr->state.stat_top[i] = top;

		/* Extract the new "stat_use" value for the stat */
		use = modify_stat_value(p_ptr->stat_cur[i], add);

		/* Save the new value */
		p_ptr->state.stat_use[i] = use;

		/* Values: 3, 4, ..., 17 */
		if (use <= 18) ind = (use - 3);

		/* Ranges: 18/00-18/09, ..., 18/210-18/219 */
		else if (use <= 18+219) ind = (15 + (use - 18) / 10);

		/* Range: 18/220+ */
		else ind = (37);

		/* Save the new index */
		p_ptr->state.stat_ind[i] = ind;
	}


	/*** Temporary flags ***/

	/* Apply temporary "stun" */
	if (p_ptr->timed[TMD_STUN] > 50)
	{
		p_ptr->state.to_h -= 20;
		p_ptr->state.dis_to_h -= 20;
		p_ptr->state.to_d -= 20;
		p_ptr->state.dis_to_d -= 20;
	}
	else if (p_ptr->timed[TMD_STUN])
	{
		p_ptr->state.to_h -= 5;
		p_ptr->state.dis_to_h -= 5;
		p_ptr->state.to_d -= 5;
		p_ptr->state.dis_to_d -= 5;
	}

	/* Invulnerability */
	if (p_ptr->timed[TMD_INVULN])
	{
		p_ptr->state.to_a += 100;
		p_ptr->state.dis_to_a += 100;
	}

	/* Temporary blessing */
	if (p_ptr->timed[TMD_BLESSED])
	{
		p_ptr->state.to_a += 5;
		p_ptr->state.dis_to_a += 5;
		p_ptr->state.to_h += 10;
		p_ptr->state.dis_to_h += 10;
	}

	/* Temporary shield */
	if (p_ptr->timed[TMD_SHIELD])
	{
		p_ptr->state.to_a += 50;
		p_ptr->state.dis_to_a += 50;
	}

	/* Temporary "Hero" */
	if (p_ptr->timed[TMD_HERO])
	{
		p_ptr->state.to_h += 12;
		p_ptr->state.dis_to_h += 12;
	}

	/* Temporary "Berserk" */
	if (p_ptr->timed[TMD_SHERO])
	{
		p_ptr->state.to_h += 24;
		p_ptr->state.dis_to_h += 24;
		p_ptr->state.to_a -= 10;
		p_ptr->state.dis_to_a -= 10;
	}

	/* Temporary "fast" */
	if (p_ptr->timed[TMD_FAST])
	{
		p_ptr->state.p_speed += 10;
	}

	/* Temporary "slow" */
	if (p_ptr->timed[TMD_SLOW])
	{
		p_ptr->state.p_speed -= 10;
	}

	/* Temporary see invisible */
	if (p_ptr->timed[TMD_SINVIS])
	{
		p_ptr->state.see_inv = TRUE;
	}

	/* Temporary infravision boost */
	if (p_ptr->timed[TMD_SINFRA])
	{
		p_ptr->state.see_infra += 5;
	}


	/*** Special flags ***/

	/* Hack -- Hero/Shero -> Res fear */
	if (p_ptr->timed[TMD_HERO] || p_ptr->timed[TMD_SHERO])
	{
		p_ptr->state.resist_fear = TRUE;
	}


	/*** Analyze weight ***/

	/* Extract the current weight (in tenth pounds) */
	j = p_ptr->total_weight;

	/* Extract the "weight limit" (in tenth pounds) */
	i = weight_limit();

	/* Apply "encumbrance" from weight */
	if (j > i / 2) p_ptr->state.p_speed -= ((j - (i / 2)) / (i / 10));

	/* Bloating slows the player down (a little) */
	if (p_ptr->food >= PY_FOOD_MAX) p_ptr->state.p_speed -= 10;

	/* Searching slows the player down */
	if (p_ptr->searching) p_ptr->state.p_speed -= 10;

	/* Sanity check on extreme speeds */
	if (p_ptr->state.p_speed < 0) p_ptr->state.p_speed = 0;
	if (p_ptr->state.p_speed > 199) p_ptr->state.p_speed = 199;

	/*** Apply modifier bonuses ***/

	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->state.to_a += ((int)(adj_dex_ta[p_ptr->state.stat_ind[A_DEX]]) - 128);
	p_ptr->state.to_d += ((int)(adj_str_td[p_ptr->state.stat_ind[A_STR]]) - 128);
	p_ptr->state.to_h += ((int)(adj_dex_th[p_ptr->state.stat_ind[A_DEX]]) - 128);
	p_ptr->state.to_h += ((int)(adj_str_th[p_ptr->state.stat_ind[A_STR]]) - 128);

	/* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->state.dis_to_a += ((int)(adj_dex_ta[p_ptr->state.stat_ind[A_DEX]]) - 128);
	p_ptr->state.dis_to_d += ((int)(adj_str_td[p_ptr->state.stat_ind[A_STR]]) - 128);
	p_ptr->state.dis_to_h += ((int)(adj_dex_th[p_ptr->state.stat_ind[A_DEX]]) - 128);
	p_ptr->state.dis_to_h += ((int)(adj_str_th[p_ptr->state.stat_ind[A_STR]]) - 128);


	/*** Modify skills ***/

	/* Affect Skill -- disarming (DEX and INT) */
	p_ptr->state.skills[SKILL_DISARM] += adj_dex_dis[p_ptr->state.stat_ind[A_DEX]];
	p_ptr->state.skills[SKILL_DISARM] += adj_int_dis[p_ptr->state.stat_ind[A_INT]];

	/* Affect Skill -- magic devices (INT) */
	p_ptr->state.skills[SKILL_DEVICE] += adj_int_dev[p_ptr->state.stat_ind[A_INT]];

	/* Affect Skill -- saving throw (WIS) */
	p_ptr->state.skills[SKILL_SAVE] += adj_wis_sav[p_ptr->state.stat_ind[A_WIS]];

	/* Affect Skill -- digging (STR) */
	p_ptr->state.skills[SKILL_DIGGING] += adj_str_dig[p_ptr->state.stat_ind[A_STR]];

	/* Affect Skill -- disarming (Level, by Class) */
	p_ptr->state.skills[SKILL_DISARM] += (cp_ptr->x_dis * p_ptr->lev / 10);

	/* Affect Skill -- magic devices (Level, by Class) */
	p_ptr->state.skills[SKILL_DEVICE] += (cp_ptr->x_dev * p_ptr->lev / 10);

	/* Affect Skill -- saving throw (Level, by Class) */
	p_ptr->state.skills[SKILL_SAVE] += (cp_ptr->x_sav * p_ptr->lev / 10);

	/* Affect Skill -- search ability (Level, by Class) */
	p_ptr->state.skills[SKILL_SEARCH] += (cp_ptr->x_srh * p_ptr->lev / 10);

	/* Affect Skill -- search frequency (Level, by Class) */
	p_ptr->state.skills[SKILL_SEARCH_FREQUENCY] += (cp_ptr->x_fos * p_ptr->lev / 10);

	/* Affect Skill -- combat (normal) (Level, by Class) */
	p_ptr->state.skills[SKILL_TO_HIT_MELEE] += (cp_ptr->x_thn * p_ptr->lev / 10);

	/* Affect Skill -- combat (shooting) (Level, by Class) */
	p_ptr->state.skills[SKILL_TO_HIT_BOW] += (cp_ptr->x_thb * p_ptr->lev / 10);

	/* Affect Skill -- combat (throwing) (Level, by Class) */
	p_ptr->state.skills[SKILL_TO_HIT_THROW] += (cp_ptr->x_thb * p_ptr->lev / 10);

	/* Limit Skill -- digging from 1 up */
	if (p_ptr->state.skills[SKILL_DIGGING] < 1) p_ptr->state.skills[SKILL_DIGGING] = 1;

	/* Obtain the "hold" value */
	hold = adj_str_hold[p_ptr->state.stat_ind[A_STR]];


	/*** Analyze current bow ***/

	/* Examine the "current bow" */
	o_ptr = &inventory[INVEN_BOW];

	/* Assume not heavy */
	p_ptr->heavy_shoot = FALSE;

	/* It is hard to hold a heavy bow */
	if (hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy bow */
		p_ptr->state.to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->state.dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy Bow */
		p_ptr->heavy_shoot = TRUE;
	}

	/* Analyze launcher */
	if (o_ptr->k_idx)
	{
		/* Get to shoot */
		p_ptr->state.num_fire = 1;

		/* Analyze the launcher */
		switch (o_ptr->sval)
		{
			/* Sling and ammo */
			case SV_SLING:
			{
				p_ptr->state.ammo_tval = TV_SHOT;
				p_ptr->state.ammo_mult = 2;

				/*Hack - Rogues get increased skill with slings*/
				if (cp_ptr->flags & CF_ROGUE_COMBAT)
				{
					p_ptr->state.skills[SKILL_TO_HIT_BOW] += 3 + p_ptr->lev / 4;
				}
				break;
			}

			/* Short Bow and Arrow */
			case SV_SHORT_BOW:
			{
				p_ptr->state.ammo_tval = TV_ARROW;
				p_ptr->state.ammo_mult = 2;
				break;
			}

			/* Long Bow and Arrow */
			case SV_LONG_BOW:
			{
				p_ptr->state.ammo_tval = TV_ARROW;
				p_ptr->state.ammo_mult = 3;
				break;
			}

			/* Light Crossbow and Bolt */
			case SV_LIGHT_XBOW:
			{
				p_ptr->state.ammo_tval = TV_BOLT;
				p_ptr->state.ammo_mult = 3;
				break;
			}

			/* Heavy Crossbow and Bolt */
			case SV_HEAVY_XBOW:
			{
				p_ptr->state.ammo_tval = TV_BOLT;
				p_ptr->state.ammo_mult = 4;
				break;
			}
		}

		/* Apply special flags */
		if (o_ptr->k_idx && !p_ptr->heavy_shoot)
		{
			/* Extra shots */
			p_ptr->state.num_fire += extra_shots;

			/* Extra might */
			p_ptr->state.ammo_mult += extra_might;

			/* Hack -- Rangers love Bows, rogues love slings */
			if (((cp_ptr->flags & CF_EXTRA_SHOT) && (p_ptr->state.ammo_tval == TV_SHOT)) ||
				((cp_ptr->flags & CF_EXTRA_ARROW) && (p_ptr->state.ammo_tval == TV_ARROW)))
			{
				if (p_ptr->lev >= LEV_EXTRA_COMBAT) p_ptr->state.num_fire++;
			}
		}

		/* Require at least one shot */
		if (p_ptr->state.num_fire < 1) p_ptr->state.num_fire = 1;
	}

	/* Brigands get poison resistance */
	if ((p_ptr->lev >= LEV_RES_POIS) && (cp_ptr->flags & (CF_BRIGAND_COMBAT)))
	{
		p_ptr->state.resist_pois = TRUE;
	}

	/*** Analyze weapon ***/

	/* Examine the "current weapon" */
	o_ptr = &inventory[INVEN_WIELD];

	/* Assume not heavy */
	p_ptr->heavy_wield = FALSE;

	/* It is hard to hold a heavy weapon */
	if (hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy weapon */
		p_ptr->state.to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->state.dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy weapon */
		p_ptr->heavy_wield = TRUE;
	}

	/* Normal weapons */
	if (o_ptr->k_idx && !p_ptr->heavy_wield)
	{
		int str_index, dex_index;

		int divide_by;

		/* Enforce a minimum "weight" (tenth pounds) */
		divide_by = ((o_ptr->weight < cp_ptr->min_weight) ? cp_ptr->min_weight : o_ptr->weight);

		/* Get the strength vs weight */
		str_index = (adj_str_blow[p_ptr->state.stat_ind[A_STR]] * cp_ptr->att_multiply / divide_by);

		/* Maximal value */
		if (str_index > 11) str_index = 11;

		/* Index by dexterity */
		dex_index = (adj_dex_blow[p_ptr->state.stat_ind[A_DEX]]);

		/* Maximal value */
		if (dex_index > 11) dex_index = 11;

		/* Use the blows table */
		p_ptr->state.num_blow = blows_table[str_index][dex_index];

		/* Maximal value */
		if (p_ptr->state.num_blow > cp_ptr->max_attacks) p_ptr->state.num_blow = cp_ptr->max_attacks;

		/* Add in the "bonus blows" */
		p_ptr->state.num_blow += extra_blows;

		/* Require at least one blow */
		if (p_ptr->state.num_blow < 1) p_ptr->state.num_blow = 1;

		/* Boost digging skill by weapon weight */
		p_ptr->state.skills[SKILL_DIGGING] += (o_ptr->weight / 10);

		/*add extra attack for those who have the flag*/
		if ((p_ptr->lev >= LEV_EXTRA_COMBAT) && (cp_ptr->flags & CF_EXTRA_ATTACK))
			p_ptr->state.num_blow += 1;
	}

	/* Assume okay */
	p_ptr->icky_wield = FALSE;

	/* Priest weapon penalty for non-blessed edged weapons */
	if ((cp_ptr->flags & CF_BLESS_WEAPON) && (!p_ptr->state.bless_blade) &&
	    ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
	{
		/* Reduce the real bonuses */
		p_ptr->state.to_h -= 2;
		p_ptr->state.to_d -= 2;

		/* Reduce the mental bonuses */
		p_ptr->state.dis_to_h -= 2;
		p_ptr->state.dis_to_d -= 2;

		/* Icky weapon */
		p_ptr->icky_wield = TRUE;
	}


	/*** Notice changes ***/

	/* Analyze stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Notice changes */
		if (p_ptr->state.stat_top[i] != old_stat_top[i])
		{
			/* Redisplay the stats later */
			p_ptr->redraw |= (PR_STATS);

		}

		/* Notice changes */
		if (p_ptr->state.stat_use[i] != old_stat_use[i])
		{
			/* Redisplay the stats later */
			p_ptr->redraw |= (PR_STATS);

		}

		/* Notice changes */
		if (p_ptr->state.stat_ind[i] != old_stat_ind[i])
		{
			/* Change in CON affects Hitpoints */
			if (i == A_CON)
			{
				p_ptr->update |= (PU_HP);
			}

			/* Change in INT may affect Mana/Spells */
			else if (i == A_INT)
			{

				if ((cp_ptr->spell_book == TV_MAGIC_BOOK) ||
					(cp_ptr->spell_book == TV_DRUID_BOOK))
				{
					p_ptr->update |= (PU_MANA | PU_SPELLS);
				}
			}

			/* Change in WIS may affect Mana/Spells */
			else if (i == A_WIS)
			{
				if ((cp_ptr->spell_book == TV_PRAYER_BOOK) ||
					(cp_ptr->spell_book == TV_DRUID_BOOK))
				{
					p_ptr->update |= (PU_MANA | PU_SPELLS);
				}
			}
		}
	}

	/* Hack -- Telepathy Change */
	if (p_ptr->state.telepathy != old_telepathy)
	{
		/* Update monster visibility */
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- See Invis Change */
	if (p_ptr->state.see_inv != old_see_inv)
	{
		/* Update monster visibility */
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Redraw speed (if needed) */
	if (p_ptr->state.p_speed != old_speed)
	{
		/* Redraw speed */
		p_ptr->redraw |= (PR_SPEED | PR_STATUS);
	}

	/* Redraw armor (if needed) */
	if ((p_ptr->state.dis_ac != old_dis_ac) || (p_ptr->state.dis_to_a != old_dis_to_a))
	{
		/* Redraw */
		p_ptr->redraw |= (PR_ARMOR);

	}

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Take note when "heavy bow" changes */
	if (old_heavy_shoot != p_ptr->heavy_shoot)
	{
		/* Message */
		if (p_ptr->heavy_shoot)
		{
			msg_print("You have trouble wielding such a heavy bow.");
		}
		else if (inventory[INVEN_BOW].k_idx)
		{
			msg_print("You have no trouble wielding your bow.");
		}
		else
		{
			msg_print("You feel relieved to put down your heavy bow.");
		}
	}

	/* Take note when "heavy weapon" changes */
	if (old_heavy_wield != p_ptr->heavy_wield)
	{
		/* Message */
		if (p_ptr->heavy_wield)
		{
			msg_print("You have trouble wielding such a heavy weapon.");
		}
		else if (inventory[INVEN_WIELD].k_idx)
		{
			msg_print("You have no trouble wielding your weapon.");
		}
		else
		{
			msg_print("You feel relieved to put down your heavy weapon.");
		}
	}

	/* Take note when "illegal weapon" changes */
	if (old_icky_wield != p_ptr->icky_wield)
	{
		/* Message */
		if (p_ptr->icky_wield)
		{
			msg_print("You do not feel comfortable with your weapon.");
		}
		else if (inventory[INVEN_WIELD].k_idx)
		{
			msg_print("You feel comfortable with your weapon.");
		}
		else
		{
			msg_print("You feel more comfortable after removing your weapon.");
		}
	}

}

/*
 * Handle "p_ptr->notice"
 */
void notice_stuff(void)
{
	/* Notice stuff */
	if (!p_ptr->notice) return;

	/* Combine the pack */
	if (p_ptr->notice & (PN_COMBINE))
	{
		p_ptr->notice &= ~(PN_COMBINE);
		combine_pack();
		combine_quiver();
		calc_inven_cnt();
	}

	/* Reorder the pack */
	if (p_ptr->notice & (PN_REORDER))
	{
		p_ptr->notice &= ~(PN_REORDER);
		reorder_pack();
	}

	if(p_ptr->notice & PN_AUTOINSCRIBE)
	{
		p_ptr->notice &= ~(PN_AUTOINSCRIBE);
		autoinscribe_pack();
		autoinscribe_ground();
	}

	/* Sort the quiver */
	if (p_ptr->notice & PN_SORT_QUIVER)
	{
		p_ptr->notice &= ~(PN_SORT_QUIVER);
		sort_quiver(0);
		save_quiver_size();
	}

	/* Dump the monster messages */
	if (p_ptr->notice & PN_MON_MESSAGE)
	{
		p_ptr->notice &= ~(PN_MON_MESSAGE);

		/* Make sure this comes after all of the monster messages */
		if (size_mon_msg > 0) flush_monster_messages();
	}

	/*
	 * Let the player know how many quest monsters remain.
	 * Make sure this comes after all of the monster messages
	 */
	if (p_ptr->notice & PN_QUEST_REMAIN)
	{
		p_ptr->notice &= ~(PN_QUEST_REMAIN);

		quest_monster_update();
	}

	/* Clear all flags */
	p_ptr->notice = 0;

}


/*
 * Handle "p_ptr->update"
 */
void update_stuff(void)
{
	/* Update stuff */
	if (!p_ptr->update) return;

	if (p_ptr->update & (PU_TORCH))	calc_torch();
	if (p_ptr->update & (PU_BONUS))
	{
		calc_bonuses();

		/*hack = always re-check stealth & nativity*/
		p_ptr->update |= (PU_STEALTH | PU_NATIVE);
	}

	if (p_ptr->update & (PU_NATIVE))
	{
		calc_nativity();
		p_ptr->redraw |= PR_RESIST;
	}

	if (p_ptr->update & (PU_STEALTH))	calc_stealth();
	if (p_ptr->update & (PU_HP))		calc_hitpoints();
	if (p_ptr->update & (PU_MANA))		calc_mana();
	if (p_ptr->update & (PU_SPELLS)) 	calc_spells();

	/* Character is not ready yet or in icky mode, no screen updates */
	if ((!character_generated) || (character_icky))
	{
		/* Clear the flags */
		p_ptr->update &= ~(PU_TORCH | PU_BONUS | PU_STEALTH | PU_NATIVE | \
							PU_STEALTH | PU_HP | PU_MANA | PU_SPELLS);
		return;
	}

	if (p_ptr->update & (PU_FORGET_VIEW))	forget_view();
	if (p_ptr->update & (PU_UPDATE_VIEW))	update_view();
	if (p_ptr->update & (PU_DISTANCE))		update_monsters(TRUE);
	/* No need to do this twice */
	else if (p_ptr->update & (PU_MONSTERS))	update_monsters(FALSE);
	if (p_ptr->update & (PU_PANEL))
	{
		event_signal(EVENT_PLAYERMOVED);
	}
	if (p_ptr->update & (PU_FLOW_DOORS | PU_FLOW_NO_DOORS))	update_flows(TRUE);

	/* Clear all flags */
	p_ptr->update = 0;

}



struct flag_event_trigger
{
	u32b flag;
	game_event_type event;
};


/*
 * Events triggered by the various flags.
 */
static const struct flag_event_trigger redraw_events[] =
{
	{ PR_MISC,    EVENT_RACE_CLASS },
	{ PR_TITLE,   EVENT_PLAYERTITLE },
	{ PR_LEV,     EVENT_PLAYERLEVEL },
	{ PR_EXP,     EVENT_EXPERIENCE },
	{ PR_STATS,   EVENT_STATS },
	{ PR_ARMOR,   EVENT_AC },
	{ PR_HP,      EVENT_HP },
	{ PR_MANA,    EVENT_MANA },
	{ PR_GOLD,    EVENT_GOLD },
	{ PR_HEALTH,  EVENT_MONSTERHEALTH },
	{ PR_HEALTH,  EVENT_MONSTERMANA },
	{ PR_DEPTH,   EVENT_DUNGEONLEVEL },
	{ PR_SPEED,   EVENT_PLAYERSPEED },
	{ PR_STATE,   EVENT_STATE },
	{ PR_STATUS,  EVENT_STATUS },
	{ PR_STUDY,   EVENT_STUDYSTATUS },
	{ PR_DTRAP,   EVENT_DETECTIONSTATUS },
	{ PR_BUTTONS, EVENT_MOUSEBUTTONS },

	{ PR_INVEN,   EVENT_INVENTORY },
	{ PR_EQUIP,   EVENT_EQUIPMENT },
	{ PR_MONLIST, EVENT_MONSTERLIST },
	{ PR_ITEMLIST, EVENT_ITEMLIST },
	{ PR_MONSTER, EVENT_MONSTERTARGET },
	{ PR_OBJECT, EVENT_OBJECTTARGET },
	{ PR_MESSAGE, EVENT_MESSAGE },
	{ PR_RESIST, EVENT_RESISTANCES },
	{ PR_QUEST_ST, EVENT_QUEST_TICKER },
	{ PR_FEELING, EVENT_FEELING },
	{ PR_FEATURE, EVENT_FEATURE},

};


/*
 * Handle "p_ptr->redraw"
 */
void redraw_stuff(void)
{
	size_t i;

	/* Redraw stuff */
	if (!p_ptr->redraw) return;

	/* Character is not ready yet, no screen updates */
	if (!character_generated) return;

	/* Character is in "icky" mode, no screen updates */
	if (character_icky) return;

	/* For each listed flag, send the appropriate signal to the UI */
	for (i = 0; i < N_ELEMENTS(redraw_events); i++)
	{
		const struct flag_event_trigger *hnd = &redraw_events[i];

		if (p_ptr->redraw & hnd->flag)
			event_signal(hnd->event);
	}

	/* Then the ones that require parameters to be supplied. */
	if (p_ptr->redraw & PR_MAP)
	{
		/* Mark the whole map to be redrawn */
		event_signal_point(EVENT_MAP, -1, -1);
	}

	p_ptr->redraw = 0;

	/*
	 * Do any plotting, etc. delayed from earlier - this set of updates
	 * is over.
	 */
	event_signal(EVENT_END);

}


/*
 * Handle "p_ptr->update" and "p_ptr->redraw" and "p_ptr->window"
 */
void handle_stuff(void)
{

	/* Update stuff */
	if (p_ptr->update) update_stuff();

	/* Redraw stuff */
	if (p_ptr->redraw) redraw_stuff();

}



