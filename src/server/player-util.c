/*
 * File: player-util.c
 * Purpose: Player utility functions
 *
 * Copyright (c) 2011 The Angband Developers. See COPYING.
 * Copyright (c) 2020 MAngband and PWMAngband Developers
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


#include "s-angband.h"


/*
 * Increment to the next or decrement to the preceeding level
 * accounting for the stair skip value in constants.
 * Keep in mind to check all intermediate level for unskippable
 * quests.
 *
 * PWMAngband: we also check for special levels when going down
 * to ensure ironman players have the opportunity to visit every
 * dungeon town.
 */
int dungeon_get_next_level(struct player *p, int dlev, int added)
{
    int target_level, i;
    struct wild_type *w_ptr = get_wt_info_at(&p->wpos.grid);

    /* Get target level */
    target_level = dlev + added * z_info->stair_skip;

    /* Don't allow levels below max */
    if (target_level > z_info->max_depth - 1) target_level = z_info->max_depth - 1;
    if (target_level > w_ptr->max_depth - 1) target_level = w_ptr->max_depth - 1;

    /* Don't allow levels above the surface */
    /* PWMAngband: check minimum depth of current dungeon */
    if (target_level < w_ptr->min_depth)
    {
        /* Going down */
        if (added > 0) target_level = w_ptr->min_depth;

        /* Going up */
        else target_level = 0;
    }

    /* Check intermediate levels for quests */
    for (i = dlev; i <= target_level; i++)
    {
        struct worldpos wpos;

        if (is_quest_active(p, i)) return i;

        wpos_init(&wpos, &p->wpos.grid, i);

        /* Hack -- stop on special levels */
        if ((i > dlev) && special_level(&wpos)) return i;
    }

    return target_level;
}


/*
 * Change dungeon level - e.g. by going up stairs or with WoR
 */
void dungeon_change_level(struct player *p, struct chunk *c, struct worldpos *new_wpos,
    byte new_level_method)
{
    /* Paranoia */
    if (!c)
    {
        Destroy_connection(p->conn, "Leaving an unallocated level, please report this bug!");
        return;
    }

    /* Paranoia: exit manual design */
    if (chunk_inhibit_players(&p->wpos))
        chunk_set_player_count(&p->wpos, 1);

    /* Remove the player */
    square_set_mon(c, &p->grid, 0);

    /* Redraw */
    square_light_spot(c, &p->grid);

    /* One less player here */
    leave_depth(p, c);

    /* Adjust player energy */
    set_energy(p, new_wpos);

    /* Set coordinates */
    memcpy(&p->wpos, new_wpos, sizeof(struct worldpos));

    /* One more player here */
    chunk_increase_player_count(new_wpos);

    /* Generate a new level (later) */
    p->upkeep->new_level_method = new_level_method;
    p->upkeep->redraw |= (PR_DTRAP);

    /* Hack -- deactivate recall for force_descend players */
    if (((cfg_limit_stairs == 3) || OPT(p, birth_force_descend)) && p->word_recall)
    {
        p->word_recall = 0;
        msg(p, "A tension leaves the air around you...");
        msg_misc(p, "'s charged aura disappears...");
        p->upkeep->redraw |= (PR_STATE);
    }
}


/*
 * Decreases players hit points and sets death flag if necessary
 */
bool take_hit(struct player *p, int damage, const char *hit_from, bool non_physical,
    const char *died_flavor)
{
    int old_chp = p->chp;
    int warning = (p->mhp * p->opts.hitpoint_warn / 10);
    int old_num = get_player_num(p);

    /* Undisturbed rest */
    bool nodisturb = ((p->upkeep->resting == REST_COMPLETE_NODISTURB) && (p->chp > warning));

    /* Paranoia */
    if (p->is_dead) return true;

    /* Become aware of player's presence */
    if (p->k_idx) aware_player(p, p);

    /* Hack -- apply "invulnerability" */
    if ((p->timed[TMD_INVULN] == -1) || p->timed[TMD_SAFELOGIN])
    {
        /* Permanent invulnerability */
        damage = 0;
    }
    else if (p->timed[TMD_INVULN] && non_physical)
    {
        /* Globe of invulnerability protects against non-physical attacks only */
        damage -= damage * p->lev / 100;
    }

    /* Apply damage reduction */
    damage -= p->state.dam_red;
    if (damage <= 0)
    {
        p->died_flavor[0] = '\0';
        return false;
    }

    /* Disturb */
    if (strcmp(hit_from, "fading") && strcmp(hit_from, "hypoxia") && !nodisturb) disturb(p);

    /* Disruption shield: damage is subtracted from mana first */
    if (p->timed[TMD_MANASHIELD] && (p->csp > 0))
    {
        /* Disruption shield fully absorbed the damage */
        if (p->csp > damage)
        {
            /* Subtract from mana and set to zero */
            p->csp -= damage;
            damage = 0;
        }

        /* Disruption shield partially absorbed the damage */
        else
        {
            damage -= p->csp;
            p->csp = 0;
            p->csp_frac = 0;

            /* No more mana shield... */
            player_clear_timed(p, TMD_MANASHIELD, true);
        }

        /* Display the spellpoints */
        p->upkeep->redraw |= (PR_MANA);
    }

    /* Hurt the player */
    p->chp -= damage;

    /* Hack -- revive */
    if (p->timed[TMD_REVIVE] && (p->chp < 0))
    {
        /* Avoid death once */
        p->timed[TMD_REVIVE] = 0;

        /* Heal the player */
        p->chp = p->mhp;
        p->chp_frac = 0;
    }

    /* Reward COMBAT_REGEN characters with mana for their lost hitpoints */
    if (player_has(p, PF_COMBAT_REGEN) && strcmp(hit_from, "poison") &&
        strcmp(hit_from, "a fatal wound") && strcmp(hit_from, "starvation"))
    {
        /* lose X% of hitpoints get X% of spell points */
        s32b sp_gain = (MAX((s32b)p->msp, 10) << 16) / (s32b)p->mhp * damage;

        player_adjust_mana_precise(p, sp_gain);
    }

    /* Hack -- redraw picture */
    redraw_picture(p, old_num);

    /* Display the hitpoints */
    p->upkeep->redraw |= (PR_HP);

    /* Dead player */
    if (p->chp < 0)
    {
        /* From hell's heart I stab at thee */
        if (p->timed[TMD_BLOODLUST] && (p->chp + p->timed[TMD_BLOODLUST] + p->lev >= 0))
        {
            if (randint0(10))
                msg(p, "Your lust for blood keeps you alive!");
            else
            {
                msg(p, "So great was his prowess and skill in warfare, the Elves said: ");
                msg(p, "'The Mormegil cannot be slain, save by mischance.'");
            }
        }
        else
        {
            /* Note cause of death */
            my_strcpy(p->died_from, hit_from, sizeof(p->died_from));
            my_strcpy(p->died_flavor, died_flavor, sizeof(p->died_flavor));

            /* Record the original (pre-ghost) cause of death */
            if (p->ghost != 1) player_death_info(p, hit_from);

            /* No longer a winner */
            p->total_winner = 0;
            p->upkeep->redraw |= (PR_TITLE);

            /* Note death */
            p->is_dead = true;

            /* Dead */
            return true;
        }
    }

    /* Hitpoint warning */
    if (warning && (p->chp <= warning))
    {
        /* Message (only the first time) */
        if (old_chp > warning)
        {
            msgt(p, MSG_HITPOINT_WARN, "*** LOW HITPOINT WARNING! ***");
            message_flush(p);
        }
    }

    /* Alive */
    p->died_flavor[0] = '\0';
    return false;
}


/*
 * Energy per move, taking extra moves into account
 */
int energy_per_move(struct player *p)
{
    int num = p->state.num_moves;
    int energy = move_energy(p->wpos.depth);

    return (energy * (1 + ABS(num) - num)) / (1 + ABS(num));
}


/*
 * Regenerate one turn's worth of hit points
 */
void player_regen_hp(struct player *p, struct chunk *c)
{
    s32b hp_gain;
    int percent = 0;    /* max 32k -> 50% of mhp; more accurately "per two bytes" */
    int fed_pct, old_chp = p->chp;
    int old_num = get_player_num(p);

    /* Default regeneration */
    if (p->timed[TMD_FOOD] >= PY_FOOD_WEAK) percent = PY_REGEN_NORMAL;
    else if (p->timed[TMD_FOOD] >= PY_FOOD_FAINT) percent = PY_REGEN_WEAK;
    else if (p->timed[TMD_FOOD] >= PY_FOOD_STARVE) percent = PY_REGEN_FAINT;

    /* Food bonus - better fed players regenerate up to 1/3 faster */
    fed_pct = p->timed[TMD_FOOD] / z_info->food_value;
    percent *= 100 + fed_pct / 3;
    percent /= 100;

    /* Various things speed up regeneration */
    if (player_of_has(p, OF_REGEN)) percent *= 2;
    if (player_resting_can_regenerate(p)) percent *= 2;
    if (p->timed[TMD_REGEN]) percent *= 3;

    /* Some things slow it down */
    if (player_of_has(p, OF_IMPAIR_HP) || player_has(p, PF_COMBAT_REGEN)) percent /= 2;

    /* Various things interfere with physical healing */
    else
    {
        if (p->timed[TMD_PARALYZED]) percent = 0;
        if (p->timed[TMD_POISONED]) percent = 0;
        if (p->timed[TMD_STUN]) percent = 0;
        if (p->timed[TMD_CUT]) percent = 0;
    }
    if (player_undead(p)) percent = 0;
    if ((p->timed[TMD_WRAITHFORM] == -1) && !square_ispassable(c, &p->grid)) percent = 0;

    /* But Biofeedback always helps */
    if (p->timed[TMD_BIOFEEDBACK])
        percent += randint1(0x400) + percent;

    /* Extract the new hitpoints */
    hp_gain = (s32b)(p->mhp * percent) + PY_REGEN_HPBASE;
    player_adjust_hp_precise(p, hp_gain);

    /* Notice changes */
    if (old_chp != p->chp)
    {
        /* Hack -- redraw picture */
        redraw_picture(p, old_num);

        /* Redraw */
        equip_learn_flag(p, OF_REGEN);
        equip_learn_flag(p, OF_IMPAIR_HP);
    }
}


/*
 * Regenerate one turn's worth of mana
 */
void player_regen_mana(struct player *p)
{
    s32b sp_gain;
    int percent, old_csp = p->csp;
    int old_num = get_player_num(p);

    /* Default regeneration */
    percent = PY_REGEN_NORMAL;

    /* Various things speed up regeneration, but shouldn't punish healthy blackguards */
    if (!(player_has(p, PF_COMBAT_REGEN) && (p->chp > p->mhp / 2)))
    {
        if (player_of_has(p, OF_REGEN)) percent *= 2;
        if (player_resting_can_regenerate(p)) percent *= 2;
    }

    /* Some things slow it down */
    if (player_has(p, PF_COMBAT_REGEN)) percent /= -2;
    else if (player_of_has(p, OF_IMPAIR_MANA)) percent /= 2;

    /* Regenerate mana */
    sp_gain = (s32b)(p->msp * percent);
    if (percent >= 0) sp_gain += PY_REGEN_MNBASE;
    sp_gain = player_adjust_mana_precise(p, sp_gain);

    /* SP degen heals blackguards at double efficiency vs casting */
    if (sp_gain < 0 && player_has(p, PF_COMBAT_REGEN)) convert_mana_to_hp(p, -sp_gain << 2);

    /* Notice changes */
    if (old_csp != p->csp)
    {
        /* Hack -- redraw picture */
        redraw_picture(p, old_num);

        /* Redraw */
        p->upkeep->redraw |= (PR_MANA);
        equip_learn_flag(p, OF_REGEN);
        equip_learn_flag(p, OF_IMPAIR_MANA);
    }
}


void player_adjust_hp_precise(struct player *p, s32b hp_gain)
{
	s32b new_chp;
	int num, old_chp = p->chp;

	/* Load it all into 4 byte format*/
	new_chp = (s32b)((p->chp << 16) + p->chp_frac) + hp_gain;

	/* Check for overflow */
	if ((new_chp < 0) && (old_chp > 0) && (hp_gain > 0))
        new_chp = LONG_MAX;
	else if ((new_chp > 0) && (old_chp < 0) && (hp_gain < 0))
		new_chp = LONG_MIN;

	/* Break it back down */
	p->chp = (s16b)(new_chp >> 16);   /* div 65536 */
	p->chp_frac = (u16b)(new_chp & 0xFFFF); /* mod 65536 */

	/* Fully healed */
	if (p->chp >= p->mhp)
    {
		p->chp = p->mhp;
		p->chp_frac = 0;
	}

	num = p->chp - old_chp;
	if (num == 0) return;

	p->upkeep->redraw |= (PR_HP);
}


/*
 * Accept a 4 byte signed int, divide it by 65k, and add
 * to current spell points. p->csp and csp_frac are 2 bytes each.
 */
s32b player_adjust_mana_precise(struct player *p, s32b sp_gain)
{
	s32b old_csp_long, new_csp_long;
	int old_csp_short = p->csp;

	if (sp_gain == 0) return 0;

	/* Load it all into 4 byte format*/
	old_csp_long = (s32b)((p->csp << 16) + p->csp_frac);
	new_csp_long = old_csp_long + sp_gain;

	/* Check for overflow */
	if ((new_csp_long < 0) && (old_csp_long > 0) && (sp_gain > 0))
    {
		new_csp_long = LONG_MAX;
		sp_gain = 0;
	}
    else if ((new_csp_long > 0) && (old_csp_long < 0) && (sp_gain < 0))
    {
		new_csp_long = LONG_MIN;
		sp_gain = 0;
	}

	/* Break it back down */
	p->csp = (s16b)(new_csp_long >> 16);   /* div 65536 */
	p->csp_frac = (u16b)(new_csp_long & 0xFFFF);    /* mod 65536 */

	/* Max/min SP */
	if (p->csp >= p->msp)
    {
		p->csp = p->msp;
		p->csp_frac = 0;
		sp_gain = 0;
	}
    else if (p->csp < 0)
    {
		p->csp = 0;
		p->csp_frac = 0;
		sp_gain = 0;
	}

	/* Notice changes */
	if (old_csp_short != p->csp) p->upkeep->redraw |= (PR_MANA);

	if (sp_gain == 0)
    {
		/* Recalculate */
		new_csp_long = (s32b)((p->csp << 16) + p->csp_frac);
		sp_gain = new_csp_long - old_csp_long;
	}

	return sp_gain;
}


void convert_mana_to_hp(struct player *p, s32b sp_long)
{
	s32b hp_gain, sp_ratio;

	if (sp_long <= 0 || p->msp == 0 || p->mhp == p->chp) return;

	/* Total HP from max */
	hp_gain = (s32b)((p->mhp - p->chp) << 16);
	hp_gain -= (s32b)p->chp_frac;

	/* Spend X% of SP get X/2% of lost HP. E.g., at 50% HP get X/4% */
	/* Gain stays low at msp < 10 because MP gains are generous at msp < 10 */
	/* sp_ratio is max sp to spent sp, doubled to suit target rate. */
	sp_ratio = (MAX(10, (s32b)p->msp) << 16) * 2 / sp_long;

	/* Limit max healing to 25% of damage; ergo spending > 50% msp is inefficient */
	if (sp_ratio < 4) sp_ratio = 4;
	hp_gain /= sp_ratio;

	player_adjust_hp_precise(p, hp_gain);
}


/*
 * Update the player's light fuel
 */
void player_update_light(struct player *p)
{
    /* Check for light being wielded */
    struct object *obj = equipped_item_by_slot_name(p, "light");

    /* Burn some fuel in the current light */
    if (obj && tval_is_light(obj))
    {
        bool burn_fuel = true;

        /* Turn off the wanton burning of light during the day outside of the dungeon */
        if ((p->wpos.depth == 0) && is_daytime())
            burn_fuel = false;

        /* If the light has the NO_FUEL flag, well... */
        if (of_has(obj->flags, OF_NO_FUEL))
            burn_fuel = false;

        /* Use some fuel (except on artifacts, or during the day) */
        if (burn_fuel && (obj->timeout > 0))
        {
            /* Decrease life-span */
            obj->timeout--;

            /* Hack -- notice interesting fuel steps */
            if ((obj->timeout < 100) || (!(obj->timeout % 100)))
            {
                /* Redraw */
                p->upkeep->redraw |= (PR_EQUIP);
            }

            /* Hack -- special treatment when blind */
            if (p->timed[TMD_BLIND])
            {
                /* Hack -- save some light for later */
                if (obj->timeout == 0) obj->timeout++;
            }

            /* The light is now out */
            else if (obj->timeout == 0)
            {
                disturb(p);
                msg(p, "Your light has gone out!");

                /* If it's a torch, now is the time to delete it */
                if (of_has(obj->flags, OF_BURNS_OUT))
                {
                    gear_excise_object(p, obj);
                    object_delete(&obj);
                }
            }

            /* The light is getting dim */
            else if ((obj->timeout < 50) && (!(obj->timeout % 20)))
            {
                disturb(p);
                msg(p, "Your light is growing faint.");
            }
        }
    }

    /* Calculate torch radius */
    p->upkeep->update |= (PU_BONUS);
}


/*
 * Have random bad stuff happen to the player from over-exertion
 *
 * This function uses the PY_EXERT_* flags
 */
void player_over_exert(struct player *p, int flag, int chance, int amount)
{
    if (chance <= 0) return;

    /* CON damage */
    if ((flag & PY_EXERT_CON) && (randint0(100) < chance))
    {
        /* Hack - only permanent with high chance (no-mana casting) */
        bool perm = ((randint0(100) < chance / 2) && (chance >= 50));

        msg(p, "You have damaged your health!");
        player_stat_dec(p, STAT_CON, perm);
    }

    /* Fainting */
    if ((flag & PY_EXERT_FAINT) && (randint0(100) < chance))
    {
        msg(p, "You faint from the effort!");

        /* Bypass free action */
        player_inc_timed(p, TMD_PARALYZED, randint1(amount), true, false);
    }

    /* Scrambled stats */
    if ((flag & PY_EXERT_SCRAMBLE) && (randint0(100) < chance))
        player_inc_timed(p, TMD_SCRAMBLE, randint1(amount), true, true);

    /* Cut damage */
    if ((flag & PY_EXERT_CUT) && (randint0(100) < chance))
    {
        msg(p, "Wounds appear on your body!");
        player_inc_timed(p, TMD_CUT, randint1(amount), true, false);
    }

    /* Confusion */
    if ((flag & PY_EXERT_CONF) && (randint0(100) < chance))
        player_inc_timed(p, TMD_CONFUSED, randint1(amount), true, true);

    /* Hallucination */
    if ((flag & PY_EXERT_HALLU) && (randint0(100) < chance))
        player_inc_timed(p, TMD_IMAGE, randint1(amount), true, true);

    /* Slowing */
    if ((flag & PY_EXERT_SLOW) && (randint0(100) < chance))
    {
        msg(p, "You feel suddenly lethargic.");
        player_inc_timed(p, TMD_SLOW, randint1(amount), true, false);
    }

    /* HP */
    if ((flag & PY_EXERT_HP) && (randint0(100) < chance))
    {
        const char *pself = player_self(p);
        char df[160];

        msg(p, "You cry out in sudden pain!");
        strnfmt(df, sizeof(df), "over-exerted %s", pself);
        take_hit(p, randint1(amount), "over-exertion", false, df);
    }
}


/*
 * Use mana
 */
void use_mana(struct player *p)
{
    int old_num = get_player_num(p);

    /* Sufficient mana? */
    if (p->spell_cost <= p->csp)
    {
        /* Use some mana */
        p->csp -= p->spell_cost;
    }
    else
    {
        int oops = p->spell_cost - p->csp;

        /* No mana left */
        p->csp = 0;
        p->csp_frac = 0;

        /* Over-exert the player */
        player_over_exert(p, PY_EXERT_FAINT, 100, 5 * oops + 1);
        player_over_exert(p, PY_EXERT_CON, 50, 0);
    }

    /* Hack -- redraw picture */
    redraw_picture(p, old_num);

    /* Redraw mana */
    p->upkeep->redraw |= (PR_MANA);
}


/*
 * See how much damage the player will take from damaging terrain
 */
int player_check_terrain_damage(struct player *p, struct chunk *c)
{
    int dam_taken = 0;

    if (player_passwall(p)) return 0;

    if (square_isfiery(c, &p->grid))
    {
        int base_dam = 100 + randint1(100);
        int res = p->state.el_info[ELEM_FIRE].res_level;

        /* Fire damage */
        dam_taken = adjust_dam(p, ELEM_FIRE, base_dam, RANDOMISE, res);

        /* Levitation makes one lightfooted. */
        if (player_of_has(p, OF_FEATHER)) dam_taken /= 2;
    }
    else if (square_islava(c, &p->grid))
    {
        int damage = p->mhp / 100 + randint1(3);

        /* Fire damage */
        dam_taken = adjust_dam(p, PROJ_FIRE, damage, RANDOMISE, 0);
    }
    else if (square_iswater(c, &p->grid) && !can_swim(p))
    {
        /* Drowning damage */
        dam_taken = p->mhp / 100 + randint1(3);

        /* Levitation prevents drowning */
        if (player_of_has(p, OF_FEATHER)) dam_taken = 0;
    }
    else if (square_isnether(c, &p->grid))
    {
        /* Draining damage */
        dam_taken = p->mhp / 100 + randint1(3);
    }

    return dam_taken;
}


/*
 * Terrain damages the player
 */
void player_take_terrain_damage(struct player *p, struct chunk *c)
{
    int dam_taken = player_check_terrain_damage(p, c);
    struct feature *feat = square_feat(c, &p->grid);

    if (!dam_taken) return;

    msg(p, feat->hurt_msg);

    /* Damage the player */
    if (!take_hit(p, dam_taken, feat->die_msg, false, feat->died_flavor))
    {
        /* Damage the inventory */
        if (square_isfiery(c, &p->grid)) inven_damage(p, PROJ_FIRE, dam_taken);
        else if (square_islava(c, &p->grid)) inven_damage(p, PROJ_FIRE, MIN(dam_taken * 5, 300));
    }
}


/*
 * Apply confusion, if needed, to a direction
 *
 * Display a message and return true if direction changes.
 */
bool player_confuse_dir(struct player *p, int *dp)
{
    int dir = *dp;

    /* Random direction */
    if (p->timed[TMD_CONFUSED] && ((dir == DIR_TARGET) || magik(75)))
        dir = ddd[randint0(8)];

    if (*dp != dir)
    {
        msg(p, "You are confused.");
        *dp = dir;
        return true;
    }

    return false;
}


/*
 * Return true if the provided count is one of the conditional REST_ flags.
 */
bool player_resting_is_special(s16b count)
{
    switch (count)
    {
        case REST_COMPLETE:
        case REST_ALL_POINTS:
        case REST_SOME_POINTS:
        case REST_MORNING:
        case REST_COMPLETE_NODISTURB:
            return true;
    }

    return false;
}


/*
 * Return true if the player is resting.
 */
bool player_is_resting(struct player *p)
{
    return ((p->upkeep->resting > 0) || player_resting_is_special(p->upkeep->resting));
}


/*
 * Return the remaining number of resting turns.
 */
s16b player_resting_count(struct player *p)
{
    return p->upkeep->resting;
}


/*
 * Set the number of resting turns.
 *
 * count is the number of turns to rest or one of the REST_ constants.
 */
void player_resting_set_count(struct player *p, s16b count)
{
    /* Cancel if player is disturbed */
    if (p->player_rest_disturb)
    {
        p->upkeep->resting = 0;
        p->player_rest_disturb = false;
        return;
    }

    /* Ignore if the rest count is negative. */
    if ((count < 0) && !player_resting_is_special(count))
    {
        p->upkeep->resting = 0;
        return;
    }

    /* Save the rest code */
    p->upkeep->resting = count;

    /* Truncate overlarge values */
    if (p->upkeep->resting > 9999) p->upkeep->resting = 9999;
}


/*
 * Cancel current rest.
 */
void player_resting_cancel(struct player *p, bool disturb)
{
    player_resting_set_count(p, 0);
    p->player_turns_rested = 0;
    p->player_rest_disturb = disturb;
}


/*
 * Return true if the player should get a regeneration bonus for the current rest.
 */
bool player_resting_can_regenerate(struct player *p)
{
    return ((p->player_turns_rested >= REST_REQUIRED_FOR_REGEN) ||
        player_resting_is_special(p->upkeep->resting));
}


/*
 * Perform one turn of resting. This only handles the bookkeeping of resting itself,
 * and does not calculate any possible other effects of resting (see process_world()
 * for regeneration).
 */
void player_resting_step_turn(struct player *p)
{
    /* Timed rest */
    if (p->upkeep->resting > 0)
    {
        /* Reduce rest count */
        p->upkeep->resting--;

        /* Redraw the state */
        if (p->upkeep->resting == 0) p->upkeep->redraw |= (PR_STATE);
    }

    /* Take a turn */
    use_energy(p);

    /* Increment the resting counter */
    p->player_turns_rested++;
}


/*
 * Handle the conditions for conditional resting (resting with the REST_ constants).
 */
void player_resting_complete_special(struct player *p)
{
    bool done = false;

    if (!player_resting_is_special(p->upkeep->resting)) return;

    /* Complete resting */
    switch (p->upkeep->resting)
    {
        case REST_ALL_POINTS:
        {
            if ((p->chp == p->mhp) && (p->csp == p->msp))
                done = true;
            break;
        }
        case REST_COMPLETE:
        case REST_COMPLETE_NODISTURB:
        {
            if ((p->chp == p->mhp) && (p->csp == p->msp) && !p->timed[TMD_BLIND] &&
                !p->timed[TMD_CONFUSED] && !p->timed[TMD_POISONED] && !p->timed[TMD_AFRAID] &&
                !p->timed[TMD_TERROR] && !p->timed[TMD_STUN] && !p->timed[TMD_CUT] &&
                !p->timed[TMD_SLOW] && !p->timed[TMD_PARALYZED] && !p->timed[TMD_IMAGE] &&
                !p->word_recall && !p->deep_descent)
            {
                done = true;
            }
            break;
        }
        case REST_SOME_POINTS:
        {
            if ((p->chp == p->mhp) || (p->csp == p->msp))
                done = true;
            break;
        }
        case REST_MORNING: 
        {
            /* We need to be careful: this is only called every ten "scaled" turns... */
            int time = move_energy(p->wpos.depth) / (10 * time_factor(p, chunk_get(&p->wpos)));

            int after_dawn = (turn.turn % (10L * z_info->day_length));

            if ((after_dawn >= 0) && (after_dawn < time))
                done = true;
            break;
        }
    }

    /* Stop resting */
    if (done) disturb(p);
}


/*
 * Check if the player state has the given OF_ flag.
 */
bool player_of_has(struct player *p, int flag)
{
    my_assert(p);

    return of_has(p->state.flags, flag);
}


/*
 * Check if the player resists (or better) an element
 */
bool player_resists(struct player *p, int element)
{
    return (p->state.el_info[element].res_level > 0);
}


/*
 * Check if the player resists (or better) an element
 */
bool player_is_immune(struct player *p, int element)
{
    return (p->state.el_info[element].res_level == 3);
}


/*
 * Return true if the player can cast a spell.
 *
 * show_msg should be set to true if a failure message should be displayed.
 */
bool player_can_cast(struct player *p, bool show_msg)
{
    if (!p->clazz->magic.total_spells)
    {
        if (show_msg) msg(p, "You cannot pray or produce magics.");
        return false;
    }

    if (p->timed[TMD_BLIND] || no_light(p))
    {
        if (show_msg) msg(p, "You cannot see!");
        return false;
    }

    if (p->timed[TMD_CONFUSED])
    {
        if (show_msg) msg(p, "You are too confused!");
        return false;
    }

    return true;
}


/*
 * Get a list of "valid" objects.
 *
 * Fills item_list[] with items that are "okay" as defined by the
 * provided tester function, etc.
 *
 * PWMAngband: uses pack + floor -- alter if needed.
 *
 * Returns the number of items placed into the list.
 */
static int scan_items(struct player *p, struct object **item_list, size_t item_max,
    item_tester tester)
{
    int floor_max = z_info->floor_size;
    struct object **floor_list = mem_zalloc(floor_max * sizeof(struct object *));
    int floor_num;
    int i;
    size_t item_num = 0;
    struct chunk *c = chunk_get(&p->wpos);

    for (i = 0; ((i < z_info->pack_size) && (item_num < item_max)); i++)
    {
        if (object_test(p, tester, p->upkeep->inven[i]))
            item_list[item_num++] = p->upkeep->inven[i];
    }

    /* Scan all non-gold objects in the grid */
    floor_num = scan_floor(p, c, floor_list, floor_max, OFLOOR_TEST | OFLOOR_SENSE | OFLOOR_VISIBLE,
        tester);
    for (i = 0; ((i < floor_num) && (item_num < item_max)); i++)
        item_list[item_num++] = floor_list[i];

    mem_free(floor_list);
    return item_num;
}


static bool spell_okay_to_study(struct player *p, int spell_index)
{
    const struct class_spell *spell = spell_by_index(&p->clazz->magic, spell_index);

    if (!spell) return false;

    /* Skip illegible spells */
    if (spell->slevel >= 99) return false;

    /* Analyze the spell */
    if (p->spell_flags[spell_index] & PY_SPELL_FORGOTTEN) return false;
    if (!(p->spell_flags[spell_index] & PY_SPELL_LEARNED)) return (spell->slevel <= p->lev);
    if (!(p->spell_flags[spell_index] & PY_SPELL_WORKED)) return false;
    return (streq(spell->realm->name, "elemental"));
}


/*
 * Return true if the player has access to a book that has unlearned spells.
 *
 * p is the player
 */
bool player_book_has_unlearned_spells(struct player *p)
{
    int i, j;
    int item_max = z_info->pack_size + z_info->floor_size;
    struct object **item_list;
    int item_num;

    /* Check if the player can cast spells */
    if (!player_can_cast(p, false)) return false;

    /* Check if the player can learn new spells */
    if (!p->upkeep->new_spells) return false;

    item_list = mem_zalloc(item_max * sizeof(struct object *));

    /* Check through all available books */
    item_num = scan_items(p, item_list, item_max, obj_can_browse);
    for (i = 0; i < item_num; i++)
    {
        const struct class_book *book = player_object_to_book(p, item_list[i]);

        if (!book) continue;

        /* Extract spells */
        for (j = 0; j < book->num_spells; j++)
        {
            /* Check if the player can study it */
            if (spell_okay_to_study(p, book->spells[j].sidx))
            {
                /* There is a spell the player can study */
                mem_free(item_list);
                return true;
            }
        }
    }

    mem_free(item_list);
    return false;
}


void cancel_running(struct player *p)
{
    p->upkeep->running = false;

    /* Check for new panel if appropriate */
    verify_panel(p);
    p->upkeep->update |= (PU_BONUS);

    /* Mark the whole map to be redrawn */
    p->upkeep->redraw |= (PR_MAP);
}


/*
 * Something has happened to disturb the player.
 *
 * The first arg indicates a major disturbance, which affects search.
 *
 * All disturbance cancels repeated commands, resting, and running.
 */
void disturb(struct player *p)
{
    /* Dungeon Master is never disturbed */
    /*if (p->dm_flags & DM_NEVER_DISTURB) return;*/

    /* Cancel repeated commands */
    p->digging_request = 0;
    if (p->cancel_firing) p->firing_request = 0;
    else p->cancel_firing = true;

    /* Cancel Resting */
    if (player_is_resting(p))
    {
        player_resting_cancel(p, true);
        p->upkeep->redraw |= (PR_STATE);
    }

    /* Cancel running */
    if (p->upkeep->running) cancel_running(p);

    /* Cancel stealth mode */
    if (p->stealthy)
    {
        p->stealthy = false;
        p->upkeep->update |= (PU_BONUS);
        p->upkeep->redraw |= (PR_STATE);
    }

    /* Get out of icky screen */
    if (p->screen_save_depth && OPT(p, disturb_icky) && !p->no_disturb_icky)
        Send_term_info(p, NTERM_HOLD, 1);

    /* Cancel looking around */
    if (((p->offset_grid.y != p->old_offset_grid.y) && (p->old_offset_grid.y != -1)) ||
        ((p->offset_grid.x != p->old_offset_grid.x) && (p->old_offset_grid.x != -1)))
    {
        /* Cancel input */
        Send_term_info(p, NTERM_HOLD, 0);

        /* Stop locating */
        do_cmd_locate(p, 0);
    }
}


/*
 * Search for traps or secret doors
 */
void search(struct player *p, struct chunk *c)
{
    struct loc begin, end;
    struct loc_iterator iter;

    /* Various conditions mean no searching */
    if (p->timed[TMD_BLIND] || no_light(p) || p->timed[TMD_CONFUSED] || p->timed[TMD_IMAGE])
        return;

    /* Paranoia */
    if (loc_is_zero(&p->grid)) return;

    loc_init(&begin, p->grid.x - 1, p->grid.y - 1);
    loc_init(&end, p->grid.x + 1, p->grid.y + 1);
    loc_iterator_first(&iter, &begin, &end);

    /* Search the nearby grids, which are always in bounds */
    do
    {
        struct object *obj;

        /* Secret doors */
        if (square_issecretdoor(c, &iter.cur))
        {
            msg(p, "You have found a secret door.");
            place_closed_door(c, &iter.cur);
            disturb(p);
        }

        /* Traps on chests */
        for (obj = square_object(c, &iter.cur); obj; obj = obj->next)
        {
            if (object_is_known(p, obj) || !is_trapped_chest(obj)) continue;

            object_notice_everything_aux(p, obj, true, false);
            if (!ignore_item_ok(p, obj))
            {
                msg(p, "You have discovered a trap on the chest!");
                disturb(p);
            }
        }
    }
    while (loc_iterator_next(&iter));
}


bool has_bowbrand(struct player *p, bitflag type, bool blast)
{
    return (p->timed[TMD_BOWBRAND] && (p->brand.type == type) &&
        (p->brand.blast == blast));
}


bool can_swim(struct player *p)
{
    return (p->poly_race && rf_has(p->poly_race->flags, RF_IM_WATER));
}


/*
 * Increase players hit points, notice effects
 */
bool hp_player_safe(struct player *p, int num)
{
    /* Paranoia */
    if (!p) return false;

    /* Healing needed */
    if (p->chp < p->mhp)
    {
        int old_num = get_player_num(p);

        /* Gain hitpoints */
        p->chp += num;

        /* Enforce maximum */
        if (p->chp >= p->mhp)
        {
            p->chp = p->mhp;
            p->chp_frac = 0;
        }

        /* Hack -- redraw picture */
        redraw_picture(p, old_num);

        /* Redraw */
        p->upkeep->redraw |= (PR_HP);

        /* Print a nice message */
        if (num < 5) msg(p, "You feel a little better.");
        else if (num < 15) msg(p, "You feel better.");
        else if (num < 35) msg(p, "You feel much better.");
        else msg(p, "You feel very good.");

        /* Notice */
        return true;
    }

    /* Ignore */
    return false;
}


bool hp_player(struct player *p, int num)
{
    /* Paranoia */
    if (!p) return false;

    if (player_undead(p))
    {
        take_hit(p, num, "a bad healing medicine", false, "was killed by a bad healing medicine");
        return true;
    }

    return hp_player_safe(p, num);
}


/*
 * Get player "number"
 */
int get_player_num(struct player *p)
{
    int num;

    num = (p->chp * 95) / (p->mhp * 10);
    if (p->timed[TMD_MANASHIELD])
        num = (p->csp * 95) / (p->msp * 10);
    if (num >= 8) num = 10;

    return num;
}


/*
 * Update player picture after HP/SP change
 */
void redraw_picture(struct player *p, int old_num)
{
    int new_num;
    struct source who_body;
    struct source *who = &who_body;

    /* Figure out of if the player's "number" has changed */
    new_num = get_player_num(p);

    /* If so then refresh everyone's view of this player */
    if (new_num != old_num)
        square_light_spot(chunk_get(&p->wpos), &p->grid);

    /* Update health bars */
    source_player(who, 0, p);
    update_health(who);
}


void current_clear(struct player *p)
{
    p->current_spell = -1;
    p->current_item = ITEM_REQUEST;
    p->current_action = 0;
    p->current_value = ITEM_REQUEST;
}


/* Space/Time Anchor radius */
#define ANCHOR_RADIUS   12


bool check_st_anchor(struct worldpos *wpos, struct loc *grid)
{
    int i;

    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        /* Skip players not on this level */
        if (!wpos_eq(&q->wpos, wpos)) continue;

        /* Skip players too far */
        if (distance(&q->grid, grid) > ANCHOR_RADIUS) continue;

        if (!q->timed[TMD_ANCHOR]) continue;

        return true;
    }

    /* Assume no st_anchor */
    return false;
}


static const char *dragon_format[6][2] =
{
    {"baby %s dragon", "baby %s drake"},
    {"young %s dragon", "young %s drake"},
    {"mature %s dragon", "mature %s drake"},
    {"ancient %s dragon", "great %s drake"},
    {"great %s wyrm", "Great Wyrm of %s"},
    {"ancient %s wyrm", "Ancient Wyrm of %s"}
};


static void get_dragon_name(int lvl_idx, struct dragon_breed *dn, char *name, size_t len)
{
    /* Dragon */
    if (lvl_idx < 4)
        strnfmt(name, len, dragon_format[lvl_idx][dn->d_fmt], dn->d_name);

    /* Wyrm */
    else
        strnfmt(name, len, dragon_format[lvl_idx][dn->w_fmt], dn->w_name);
}


static struct monster_race *get_dragon_race(int lvl_idx, struct dragon_breed *dn)
{
    char name[NORMAL_WID];

    /* Name */
    get_dragon_name(lvl_idx, dn, name, sizeof(name));

    return get_race(name);
}


struct dragon_breed *get_dragon_form(struct monster_race *race)
{
    int lvl_idx;
    char name[NORMAL_WID];
    struct dragon_breed *dn;

    for (lvl_idx = 0; lvl_idx < 6; lvl_idx++)
    {
        for (dn = breeds; dn; dn = dn->next)
        {
            /* Name */
            get_dragon_name(lvl_idx, dn, name, sizeof(name));

            if (streq(race->name, name)) return dn;
        }
    }

    return NULL;
}


static struct monster_race *get_dragon_random(void)
{
    int i, options = 0;
    struct dragon_breed *dn, *choice;

    for (dn = breeds; dn; dn = dn->next)
    {
        for (i = 0; i < dn->commonness; i++)
        {
            if (one_in_(++options)) choice = dn;
        }
    }

    return get_dragon_race(0, choice);
}


void poly_dragon(struct player *p, bool msg)
{
    struct monster_race *race = NULL;
    struct monster_race *race_newborn = get_race("newborn dragon");

    /* Character birth */
    if (!p->poly_race) race = race_newborn;

    /* Keep current form at low level */
    else if (p->lev < 5) race = p->poly_race;

    /* Random choice of race at level 5 */
    else if ((p->lev == 5) && (p->poly_race == race_newborn))
    {
        struct dragon_breed *dn;

        race = get_dragon_random();

        /* Dragon breed */
        dn = get_dragon_form(race);

        /* Apply experience penalty */
        p->expfact = p->expfact * dn->r_exp / 100;
    }

    /* New form */
    else
    {
        int lvl_idx;
        struct dragon_breed *dn;

        /* Level index */
        if (p->lev == PY_MAX_LEVEL) lvl_idx = 5;
        else lvl_idx = (p->lev - 5) / 10;

        /* Dragon breed */
        dn = get_dragon_form(p->poly_race);

        /* New form */
        race = get_dragon_race(lvl_idx, dn);
    }

    /* Polymorph into that dragon */
    if (race && (race != p->poly_race)) do_cmd_poly(p, race, false, msg);
}


static int hydra_progression[15] = {0, 1, 10, 15, 20, 24, 28, 32, 36, 40, 42, 44, 46, 48, 50};


void poly_hydra(struct player *p, bool msg)
{
    struct monster_race *race;
    char nheads[20];
    int i;

    /* Get number of heads depending on level */
    my_strcpy(nheads, "hydra", sizeof(nheads));
    for (i = 14; i >= 2; i--)
    {
        if (p->lev >= hydra_progression[i])
        {
            strnfmt(nheads, sizeof(nheads), "%d-headed hydra", i);
            break;
        }
    }

    /* Polymorph into that hydra */
    race = get_race(nheads);
    if (race && (race != p->poly_race)) do_cmd_poly(p, race, false, msg);
}


void poly_bat(struct player *p, int chance, char *killer)
{
    char buf[MSG_LEN];
    struct monster_race *race_fruit_bat = get_race("fruit bat");

    /* Not in fruit bat mode! */
    if (OPT(p, birth_fruit_bat))
    {
        msg(p, "Nothing happens.");
        return;
    }

    if (p->poly_race != race_fruit_bat)
    {
        /* Attempt a saving throw */
        if (p->ghost || player_has(p, PF_DRAGON) || player_has(p, PF_HYDRA) ||
            CHANCE(p->state.skills[SKILL_SAVE], chance))
        {
            msg(p, "You resist the effects!");
        }
        else
        {
            char desc[NORMAL_WID];

            my_strcpy(desc, p->name, sizeof(desc));
            my_strcap(desc);

            /* Turned into a fruit bat */
            if (killer)
                strnfmt(buf, sizeof(buf), "%s was turned into a fruit bat by %s!", desc, killer);
            else
                strnfmt(buf, sizeof(buf), "%s was turned into a fruit bat!", desc);
            msg_broadcast(p, buf, MSG_BROADCAST_FRUITBAT);
            do_cmd_poly(p, race_fruit_bat, false, true);
        }
    }
    else
    {
        /* No saving throw for being restored... */
        do_cmd_poly(p, NULL, false, true);
    }
}


void drain_mana(struct player *p, struct source *who, int drain, bool seen)
{
    char m_name[NORMAL_WID];
    int old_num = get_player_num(p);

    /* Get the monster name (or "it") */
    if (who->monster)
        monster_desc(p, m_name, sizeof(m_name), who->monster, MDESC_STANDARD);

    /* Get the player name (or "it") */
    else if (who->player && !who->trap)
        player_desc(p, m_name, sizeof(m_name), who->player, true);

    if (!p->csp)
    {
        msg(p, "The draining fails.");
        if (who->monster) update_smart_learn(who->monster, p, 0, PF_NO_MANA, -1);
        return;
    }

    /* Drain the given amount if the player has that much, or all of it */
    if (drain >= p->csp)
    {
        drain = p->csp;
        p->csp = 0;
        p->csp_frac = 0;
        player_clear_timed(p, TMD_MANASHIELD, true);
    }
    else
        p->csp -= drain;

    /* Hack -- redraw picture */
    redraw_picture(p, old_num);

    /* Heal the monster */
    if (who->monster && (who->monster->hp < who->monster->maxhp))
    {
        who->monster->hp += (6 * drain);
        if (who->monster->hp > who->monster->maxhp) who->monster->hp = who->monster->maxhp;

        /* Redraw (later) if needed */
        update_health(who);

        /* Special message */
        if (seen) msg(p, "%s appears healthier.", m_name);
    }

    /* Heal the player */
    else if (who->player && !who->trap && hp_player(who->player, drain * 6))
    {
        /* Special message */
        if (seen) msg(p, "%s appears healthier.", m_name);
    }

    /* Redraw mana */
    p->upkeep->redraw |= (PR_MANA);
}


/*
 * Recall a player.
 */
void recall_player(struct player *p, struct chunk *c)
{
    byte new_level_method;
    const char *msg_self, *msg_others;

    /* From dungeon to surface */
    if (p->wpos.depth > 0)
    {
        /* Messages */
        msg_self = "You feel yourself yanked upwards!";
        msg_others = " is yanked upwards!";

        /* New location */
        wpos_init(&p->recall_wpos, &p->wpos.grid, 0);
        new_level_method = LEVEL_RAND;
    }

    /* Nowhere to go */
    else if (wpos_eq(&p->recall_wpos, &p->wpos))
    {
        msg(p, "A tension leaves the air around you...");
        msg_misc(p, "'s charged aura disappears...");
        p->upkeep->redraw |= (PR_STATE);
        return;
    }

    /* From surface to dungeon */
    else if (p->recall_wpos.depth > 0)
    {
        /* Winner-only/shallow dungeons */
        if (forbid_entrance_weak(p) || forbid_entrance_strong(p))
        {
            msg(p, "A tension leaves the air around you...");
            msg_misc(p, "'s charged aura disappears...");
            p->upkeep->redraw |= (PR_STATE);
            return;
        }

        /* Messages */
        msg_self = "You feel yourself yanked downwards!";
        msg_others = " is yanked downwards!";

        /* New location */
        new_level_method = LEVEL_RAND;
    }

    /* From wilderness to wilderness */
    else
    {
        /* Messages */
        msg_self = "You feel yourself yanked sideways!";
        msg_others = " is yanked sideways!";

        /* New location */
        new_level_method = LEVEL_OUTSIDE_RAND;
    }

    /* Hack -- DM redesigning the level */
    if (chunk_inhibit_players(&p->recall_wpos))
    {
        msg(p, "A tension leaves the air around you...");
        msg_misc(p, "'s charged aura disappears...");
        p->upkeep->redraw |= (PR_STATE);
        return;
    }

    /* Disturbing! */
    disturb(p);

    /* Messages */
    msgt(p, MSG_TPLEVEL, msg_self);
    msg_misc(p, msg_others);

    /* Change location */
    dungeon_change_level(p, c, &p->recall_wpos, new_level_method);

    /* Hack -- replace the player */
    p->arena_num = -1;

    /* Redraw the state (later) */
    p->upkeep->redraw |= (PR_STATE);
}


int player_digest(struct player *p)
{
    int i;
    int speed = p->state.speed;
    int excess = p->timed[TMD_FOOD] - PY_FOOD_FULL;

    /* Basic digestion rate based on speed */
    /* PWMAngband: remove speed penalty from being Full to avoid double penalty */
    if ((excess > 0) && !p->timed[TMD_ATT_VAMP])
    {
        excess = (excess * 10) / (PY_FOOD_MAX - PY_FOOD_FULL);
        speed += excess;
    }
    i = turn_energy(speed);

    /* Some effects require more food */
    if (p->timed[TMD_ADRENALINE]) i *= 2;
    if (p->timed[TMD_HARMONY]) i *= 2;
    if (p->timed[TMD_BIOFEEDBACK]) i *= 2;
    if (p->timed[TMD_INVIS]) i *= 2;
    if (p->timed[TMD_WRAITHFORM]) i *= 2;
    if (p->timed[TMD_REGEN]) i *= 2;

    /* Adjust for food value */
    i = (i * 100) / z_info->food_value;

    /* Regeneration takes more food */
    if (player_of_has(p, OF_REGEN)) i *= 2;

    /* Slow digestion takes less food */
    if (player_of_has(p, OF_SLOW_DIGEST)) i /= 2;

    /* Minimal digestion */
    if (i < 1) i = 1;

    return i;
}


void use_energy(struct player *p)
{
    /* Take a turn */
    p->energy -= move_energy(p->wpos.depth);

    /* Paranoia */
    if (p->energy < 0) p->energy = 0;
}


/*
 * Check for nearby players/monsters and attack the current target.
 */
bool auto_retaliate(struct player *p, struct chunk *c, int mode)
{
    int i, n = 0;
    bool found = false;
    struct source *health_who = &p->upkeep->health_who;
    struct source who_body;
    struct source *who = &who_body;
    struct loc target, targets[8];
    s16b target_dir, targets_dir[8];
    struct object *weapon = equipped_item_by_slot_name(p, "weapon");
    struct object *launcher = ((mode == AR_BLOODLUST)? NULL:
        equipped_item_by_slot_name(p, "shooting"));

    /* Hack -- shoppers don't auto-retaliate */
    if (in_store(p)) return false;

    /* The dungeon master does not auto-retaliate */
    if (p->dm_flags & DM_MONSTER_FRIEND) return false;

    /* Not while confused */
    if (p->timed[TMD_CONFUSED]) return false;

    /* Don't auto-retalitate with commands queued */
    if (get_connection(p->conn)->q.len > 0) return false;

    /* Check preventive inscription '^O' */
    if (check_prevent_inscription(p, INSCRIPTION_RETALIATE) && (mode == AR_NORMAL)) return false;

    /* Check melee weapon inscription '!O' */
    if (weapon && object_prevent_inscription(p, weapon, INSCRIPTION_RETALIATE, false) &&
        (mode == AR_NORMAL))
    {
        return false;
    }

    /* Try to find valid targets around us */
    for (i = 0; i < 8; i++)
    {
        bool hostile, visible, mimicking;

        /* Current location */
        loc_sum(&target, &p->grid, &ddgrid_ddd[i]);
        target_dir = ddd[i];

        /* Paranoia */
        if (!square_in_bounds_fully(c, &target)) continue;

        /* Nobody here */
        if (!square(c, &target)->mon) continue;

        square_actor(c, &target, who);

        /* Target info */
        if (who->player)
        {
            hostile = pvp_check(p, who->player, PVP_CHECK_BOTH, true, square(c, &target)->feat);
            visible = player_is_visible(p, who->idx);
            mimicking = (who->player->k_idx != 0);
        }
        else
        {
            hostile = pvm_check(p, who->monster);
            visible = monster_is_visible(p, who->idx);
            mimicking = monster_is_camouflaged(who->monster);
        }

        /* If hostile and visible, it's a fair target (except hidden mimics) */
        if (hostile && visible && !mimicking)
        {
            loc_copy(&targets[n], &target);
            targets_dir[n] = target_dir;
            n++;
        }
    }

    /* No valid target around */
    if (!n) return false;

    /* If there's a current target, attack it (always) */
    if (!source_null(health_who))
    {
        for (i = 0; i < n; i++)
        {
            /* Current location */
            loc_copy(&target, &targets[i]);
            target_dir = targets_dir[i];

            /* Not the current target */
            square_actor(c, &target, who);
            if (!source_equal(health_who, who)) continue;

            /* Current target found */
            found = true;
            break;
        }
    }

    /* If there's at least one valid target around, attack one (active auto-retaliator only) */
    if ((OPT(p, active_auto_retaliator) || (mode != AR_NORMAL)) && !found)
    {
        /* Choose randomly */
        i = randint0(n);
        loc_copy(&target, &targets[i]);
        target_dir = targets_dir[i];
        square_actor(c, &target, who);
        found = true;
    }

    /* No current target */
    if (!found) return false;

    /* Check if we can retaliate with launcher */
    if (launcher && object_match_inscription(p, launcher, INSCRIPTION_RETALIATE))
    {
        struct object *ammo = NULL;

        /* Find first eligible ammo in the quiver */
        for (i = 0; i < z_info->quiver_size; i++)
        {
            if (!p->upkeep->quiver[i]) continue;
            if (p->upkeep->quiver[i]->tval != p->state.ammo_tval) continue;
            ammo = p->upkeep->quiver[i];
            break;
        }

        /* Require usable ammo */
        if (ammo)
            do_cmd_fire(p, target_dir, ammo->oidx);
        else
            msg(p, "You have no ammunition in the quiver to fire.");
    }

    /* Attack the current target */
    else
    {
        /* Not while afraid */
        if (player_of_has(p, OF_AFRAID)) return false;

        py_attack(p, c, &target);

        /* Take a turn */
        use_energy(p);
    }

    return true;
}


/*
 * Check if player has enough energy to act
 *
 * real_command: true if checking for a real command, false if just checking for idle players
 */
bool has_energy(struct player *p, bool real_command)
{
    /* Check if we have enough energy */
    if (p->energy < move_energy(p->wpos.depth)) return false;

    /* Occasional attack instead for bloodlust-affected characters */
    if (real_command && (randint0(200) < p->timed[TMD_BLOODLUST]))
    {
        struct chunk *c = chunk_get(&p->wpos);

        if (auto_retaliate(p, c, AR_BLOODLUST)) return false;
    }

    return true;
}


void set_energy(struct player *p, struct worldpos *wpos)
{
    /* Set player energy */
    if (wpos_eq(&p->wpos, wpos))
        p->energy = move_energy(p->wpos.depth);

    /* Adjust player energy to new depth */
    else
    {
        p->energy = p->energy * move_energy(wpos->depth) / move_energy(p->wpos.depth);

        /* Paranoia */
        if (p->energy < 0) p->energy = 0;
        if (p->energy > move_energy(wpos->depth)) p->energy = move_energy(wpos->depth);
    }
}


bool player_is_at(struct player *p, struct loc *grid)
{
    return loc_eq(grid, &p->grid);
}


struct player_race *lookup_player_race(const char *name)
{
    struct player_race *r;

    for (r = races; r; r = r->next)
    {
        if (streq(r->name, name)) break;
    }

    return r;
}


bool forbid_entrance_weak(struct player *p)
{
    struct location *dungeon = get_dungeon(&p->wpos);

    return (dungeon && df_has(dungeon->flags, DF_WINNERS_ONLY) && !(p->total_winner || is_dm_p(p)));
}


bool forbid_entrance_strong(struct player *p)
{
    struct location *dungeon = get_dungeon(&p->wpos);

    return (dungeon && dungeon->max_level && (p->lev > dungeon->max_level) && !is_dm_p(p));
}


/*
 * Player is in the player's field of view
 */
bool player_is_in_view(struct player *p, int p_idx)
{
    return mflag_has(p->pflag[p_idx], MFLAG_VIEW);
}


/*
 * Player is visible to the player
 */
bool player_is_visible(struct player *p, int p_idx)
{
    return mflag_has(p->pflag[p_idx], MFLAG_VISIBLE);
}


/*
 * Player is invisible
 */
bool player_is_invisible(struct player *q)
{
    return (q->timed[TMD_INVIS] != 0);
}


/*
 * Player is not invisible
 */
bool player_is_not_invisible(struct player *q)
{
    return (!q->timed[TMD_INVIS] && !q->k_idx);
}


/*
 * Player lives
 */
bool player_is_living(struct player *q)
{
    return (!q->is_dead);
}


/*
 * Check if the player is immune from traps
 */
bool player_is_trapsafe(struct player *p)
{
    if (p->timed[TMD_TRAPSAFE]) return true;
    if (player_of_has(p, OF_TRAP_IMMUNE)) return true;
    return false;
}
