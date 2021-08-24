/*
 * File: mon-blows.c
 * Purpose: Monster melee module.
 *
 * Copyright (c) 1997 Ben Harrison, David Reeve Sward, Keldon Jones.
 * Copyright (c) 2013 Ben Semmler
 * Copyright (c) 2016 Nick McConnell
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
 * Monster blow methods
 */


/*
 * Return a randomly chosen string to append to an INSULT message.
 */
static const char *monster_blow_random_insult(void)
{
    #define MAX_DESC_INSULT 8
    static const char *desc_insult[MAX_DESC_INSULT] =
    {
        "insults %s!",
        "insults your mother!",
        "gives %s the finger!",
        "humiliates %s!",
        "defiles %s!",
        "dances around %s!",
        "makes obscene gestures!",
        "moons %s!!!"
    };

    return desc_insult[randint0(MAX_DESC_INSULT)];
    #undef MAX_DESC_INSULT
}


/*
 * Return a randomly chosen string to append to a MOAN message.
 */
static const char *monster_blow_random_moan(void)
{
    #define MAX_DESC_MOAN 8
    static const char *desc_moan[MAX_DESC_MOAN] =
    {
        "wants his mushrooms back",
        "tells you to get off his land",
        "looks for his dogs",
        "says 'Did you kill my Fang?'",
        "asks 'Do you want to buy any mushrooms?'",
        "seems sad about something",
        "asks if you have seen his dogs",
        "mumbles something about mushrooms"
    };

    return desc_moan[randint0(MAX_DESC_MOAN)];
    #undef MAX_DESC_MOAN
}


/*
 * Return an action string to be appended on the attack message.
 *
 * method is the blow method.
 */
const char *monster_blow_method_action(struct blow_method *method)
{
	const char *action = NULL;

	if (method->act_msg)
        action = method->act_msg;
	else if (streq(method->name, "INSULT"))
		action = monster_blow_random_insult();
	else if (streq(method->name, "MOAN"))
		action = monster_blow_random_moan();

	return action;
}


/*
 * Monster blow effect helper functions
 */


static void eat_gold(struct player *p, struct source *who)
{
    s32b gold = (p->au / 10) + randint1(25);

    if (gold < 2) gold = 2;
    if (gold > 5000) gold = (p->au / 20) + randint1(3000);
    if (gold > p->au) gold = p->au;
    if (gold <= 0)
    {
        msg(p, "Nothing was stolen.");
        return;
    }

    p->au -= gold;

    /* Let the player know they were robbed */
    msg(p, "Your purse feels lighter.");
    if (p->au)
        msg(p, "%d coins were stolen!", gold);
    else
        msg(p, "All of your coins were stolen!");

    /* Redraw gold */
    p->upkeep->redraw |= (PR_GOLD);

    /* Give the gold to the monster */
    if (who->monster)
    {
        struct object *obj = object_new();

        /* Create a new temporary object */
        object_prep(p, obj, money_kind("gold", gold), 0, MINIMISE);

        /* Amount of gold to put in this object */
        obj->pval = gold;

        /* Set origin to stolen, so it is not confused with dropped treasure in monster_death */
        obj->origin = ORIGIN_STOLEN;

        /* Give the gold to the monster */
        if (!monster_carry(who->monster, obj, false))
            object_delete(&obj);
    }

    /* PWMAngband: give the gold to the offending player in PvP! */
    else if (who->player)
    {
        who->player->au += gold;
        msg(who->player, "Your purse feels heavier.");
        who->player->upkeep->redraw |= (PR_GOLD);
    }
}


/*
 * Monster steals an item from the player
 */
static void steal_player_item(struct player *p, struct source *who, bool* obvious, int* blinked)
{
    int tries;

    /* Find an item */
    for (tries = 0; tries < 10; tries++)
    {
        struct object *obj, *stolen;
        char o_name[NORMAL_WID];
        bool split = false;
        bool none_left = false;

        /* Pick an item */
        int index = randint0(z_info->pack_size);

        /* Obtain the item */
        obj = p->upkeep->inven[index];

        /* Skip non-objects */
        if (obj == NULL) continue;

        /* Skip artifacts */
        if (obj->artifact) continue;

        /* Skip deeds of property */
        if (tval_is_deed(obj)) continue;

        /* PvP: can only steal items if they can be carried */
        if (who->player)
        {
            struct object *test = object_new();
            bool ok = true;

            /* Get a copy with the right "amt" */
            object_copy_amt(test, obj, 1);

            /* Note that the pack is too full */
            if (!inven_carry_okay(who->player, test)) ok = false;

            /* Note that the pack is too heavy */
            else if (!weight_okay(who->player, test)) ok = false;

            /* Must meet level requirement */
            else if (!has_level_req(who->player, test)) ok = false;

            object_delete(&test);
            if (!ok) continue;
        }

        /* Get a description */
        object_desc(p, o_name, sizeof(o_name), obj, ODESC_FULL);

        /* Is it one of a stack being stolen? */
        if (obj->number > 1) split = true;

        /* Message */
        msg(p, "%s %s (%c) was stolen!", (split? "One of your": "Your"), o_name, I2A(index));

        /* Steal and carry */
        stolen = gear_object_for_use(p, obj, 1, false, &none_left);
        if (who->monster)
        {
            if (!monster_carry(who->monster, stolen, false))
                object_delete(&stolen);
        }
        else if (who->player)
            inven_carry(who->player, stolen, true, false);

        /* Obvious */
        *obvious = true;

        /* Blink away */
        *blinked = 2;

        /* Done */
        break;
    }
}


/*
 * Get the elemental damage taken by a monster from another monster's melee
 */
static void monster_elemental_damage(melee_effect_handler_context_t *context, int imm_flag,
    int suscept_flag)
{
    int mult = 3;

    /* Obvious */
    context->obvious = true;

    /* Notice immunity */
    if (rf_has(context->target->monster->race->flags, imm_flag))
    {
        mult = 1;
        if (context->visible) rf_on(context->target_l_ptr->flags, imm_flag);
    }

    /* Notice susceptibility */
    else if (suscept_flag && rf_has(context->target->monster->race->flags, suscept_flag))
    {
        mult = 6;
        if (context->visible) rf_on(context->target_l_ptr->flags, suscept_flag);
    }

    /* Take some damage */
    context->dead = project_m_monster_attack_aux(context->mon, chunk_get(&context->mon->wpos),
        context->target->monster, context->damage * mult, context->note_dies);
}


/*
 * Deal the actual melee damage from a monster to a target player or monster
 */
static bool monster_damage_target(melee_effect_handler_context_t *context)
{
    /* MvM */
    if (context->style == TYPE_MVM)
    {
        /* Take some damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(&context->mon->wpos), context->target->monster, context->damage,
            context->note_dies);
        return true;
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false, context->flav)) return true;

    return false;
}


/*
 * Monster blow multi-effect handlers
 * These are each called by several individual effect handlers
 */


/*
 * Do damage as the result of a melee attack that has an elemental aspect.
 *
 * context is information for the current attack.
 * type is the PROJ_ constant for the element.
 * pure_element should be true if no side effects (mostly a hack for poison).
 */
static bool melee_effect_elemental(melee_effect_handler_context_t *context, int type,
    bool pure_element, const char *flav_msg)
{
    int physical_dam, elemental_dam;
    bool ret = false;

    /* Obvious */
    if (pure_element) context->obvious = true;

    switch (type)
    {
        case PROJ_ACID: msg(context->p, "You are covered in acid!"); break;
        case PROJ_ELEC: msg(context->p, "You are struck by electricity!"); break;
        case PROJ_FIRE: msg(context->p, "You are enveloped in flames!"); break;
        case PROJ_COLD: msg(context->p, "You are covered with frost!"); break;
    }

    /* Give the player a small bonus to ac for elemental attacks */
    physical_dam = adjust_dam_armor(context->damage, context->ac + 50);

    /* Some attacks do no physical damage */
    if (!context->method->phys)
        physical_dam = 0;

    elemental_dam = adjust_dam(context->p, type, context->damage, RANDOMISE, 0);

    /* Take the larger of physical or elemental damage */
    context->damage = MAX(physical_dam, elemental_dam);

    if (elemental_dam > 0)
    {
        if (pure_element) inven_damage(context->p, type, MIN(elemental_dam * 5, 300));
        ret = true;
    }
    if (context->damage > 0)
    {
        char df[160];

        my_strcpy(df, context->flav, sizeof(df));
        if (pure_element) strnfmt(df, sizeof(df), "was %s by %s", flav_msg, context->ddesc);
        take_hit(context->p, context->damage, context->ddesc, false, df);
    }

    /* Learn about the player */
    if (pure_element) update_smart_learn(context->mon, context->p, 0, 0, type);

    return ret;
}


/*
 * Do damage as the result of a melee attack that has a status effect.
 *
 * context is the information for the current attack.
 * type is the TMD_ constant for the effect.
 * amount is the amount that the timer should be increased by.
 * of_flag is the OF_ flag that is passed on to monster learning for this effect.
 * save indicates if a saving throw should be attempted for this effect.
 * save_msg is the message that is displayed if the saving throw is successful.
 */
static void melee_effect_timed(melee_effect_handler_context_t *context, int type, int amount,
    int of_flag, bool save, const char *save_msg, bool paralyze)
{
    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false, context->flav)) return;

	/* Perform a saving throw if desired. */
	if ((save && magik(context->p->state.skills[SKILL_SAVE])) ||
        resist_undead_attacks(context->p, context->mon->race))
    {
		if (save_msg != NULL)
			msg(context->p, "%s", save_msg);

		context->obvious = true;
	}

    /* Increase timer for type. */
	else
    {
        if (player_inc_timed(context->p, type, amount, true, true))
        {
            context->obvious = true;

            /* Hack -- make level 1 monsters who paralyze also blink */
            if (paralyze && context->mon->race->level == 1) context->blinked = 1;
        }
    }

	/* Learn about the player */
	update_smart_learn(context->mon, context->p, of_flag, 0, -1);
}


/*
 * Do damage as the result of a melee attack that drains a stat.
 *
 * context is the information for the current attack.
 * stat is the STAT_ constant for the desired stat.
 */
static void melee_effect_stat(melee_effect_handler_context_t *context, int stat, int of_flag)
{
	/* PvX */
    if (context->style == TYPE_PVX)
    {
        /* PvP: lose-stat attack */
        if (context->target->player)
        {
            struct source who_body;
            struct source *who = &who_body;

            source_player(who, get_player_index(get_connection(context->target->player->conn)),
                context->target->player);
            effect_simple(EF_DRAIN_STAT, who, "0", stat, 0, 0, 0, 0, NULL);
        }
        return;
    }

    /* Take damage */
	if (monster_damage_target(context)) return;

	/* Damage (stat) */
    if (resist_undead_attacks(context->p, context->mon->race))
    {
        msg(context->p, "You feel %s for a moment, but the feeling passes.",
            desc_stat(stat, false));
        equip_learn_flag(context->p, of_flag);
        context->obvious = true;
    }
    else
    {
        struct source who_body;
        struct source *who = &who_body;

        source_player(who, get_player_index(get_connection(context->p->conn)), context->p);
        effect_simple(EF_DRAIN_STAT, who, "0", stat, 0, 0, 0, 0, &context->obvious);
    }
}


/*
 * Do damage as the result of an experience draining melee attack.
 *
 * context is the information for the current attack.
 * drain_amount is the base amount of experience to drain.
 */
static void melee_effect_experience(melee_effect_handler_context_t *context, int drain_amount)
{
    /* PvX */
    if (context->style == TYPE_PVX)
    {
        if (context->target->monster) return;

        /* PvP: lose-exp attack */
        drain_xp(context->target->player, drain_amount);
        equip_learn_flag(context->target->player, OF_HOLD_LIFE);
        return;
    }

	/* Obvious */
	context->obvious = true;

    /* Take damage */
	if (monster_damage_target(context)) return;
	update_smart_learn(context->mon, context->p, OF_HOLD_LIFE, 0, -1);

    if (resist_undead_attacks(context->p, context->mon->race))
        msg(context->p, "You keep hold of your life force!");
    else
        drain_xp(context->p, drain_amount);
}


/*
 * Monster blow effect handlers
 */


/*
 * Melee effect handler: Hit the player, but don't do any damage.
 */
static void melee_effect_handler_NONE(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == TYPE_PVX) return;

	context->obvious = true;
	context->damage = 0;
}


/*
 * Melee effect handler: Hurt the player with no side effects.
 */
static void melee_effect_handler_HURT(melee_effect_handler_context_t *context)
{
	/* PvX */
    if (context->style == TYPE_PVX) return;

	/* Obvious */
	context->obvious = true;

	/* Armor reduces total damage */
	context->damage = adjust_dam_armor(context->damage, context->ac);

    /* Take damage */
    monster_damage_target(context);
}


/*
 * Melee effect handler: Poison the player.
 *
 * We can't use melee_effect_timed(), because this is both an elemental attack and a
 * status attack. Note the false value for pure_element for melee_effect_elemental().
 */
static void melee_effect_handler_POISON(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == TYPE_PVX) return;

	/* MvM */
    if (context->style == TYPE_MVM)
    {
        int mult = 3;

        /* Obvious */
        context->obvious = true;

        /* Notice immunity */
        if (rf_has(context->target->monster->race->flags, RF_IM_POIS)) mult = 1;

        /* Take some damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(&context->mon->wpos), context->target->monster, context->damage * mult,
            context->note_dies);
        if (context->dead) return;

        /* Take "poison" effect */
        mon_inc_timed(context->p, context->target->monster, MON_TMD_POIS, 5 + randint1(5),
            MON_TMD_FLG_NOTIFY);
        return;
    }

	/* Take "poison" effect */
    if (melee_effect_elemental(context, PROJ_POIS, false, "killed"))
    {
        context->obvious = player_inc_timed(context->p, TMD_POISONED, 5 + randint1(context->rlev),
            true, true);
    }

	/* Learn about the player */
	update_smart_learn(context->mon, context->p, 0, 0, ELEM_POIS);
}


/*
 * Melee effect handler: Disenchant the player.
 */
static void melee_effect_handler_DISENCHANT(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == TYPE_PVX)
    {
        if (context->target->monster) return;

        /* PvP: un-bonus attack */
        equip_learn_element(context->target->player, ELEM_DISEN);
        if (player_resists(context->target->player, ELEM_DISEN))
            msg(context->p, "%s is unaffected.", context->ddesc);
        else
        {
            struct source who_body;
            struct source *who = &who_body;

            source_player(who, get_player_index(get_connection(context->target->player->conn)),
                context->target->player);
            effect_simple(EF_DISENCHANT, who, "0", 0, 0, 0, 0, 0, NULL);
        }
        return;
    }

    /* Take damage */
	if (monster_damage_target(context)) return;

	/* Apply disenchantment if no resist */
	if (!player_resists(context->p, ELEM_DISEN))
    {
		struct source who_body;
        struct source *who = &who_body;

        source_player(who, get_player_index(get_connection(context->p->conn)), context->p);
        effect_simple(EF_DISENCHANT, who, "0", 0, 0, 0, 0, 0, &context->obvious);
    }

	/* Learn about the player */
	update_smart_learn(context->mon, context->p, 0, 0, ELEM_DISEN);
}


/*
 * Melee effect handler: Drain charges from the player's inventory.
 */
static void melee_effect_handler_DRAIN_CHARGES(melee_effect_handler_context_t *context)
{
    struct source who_body;
    struct source *who = &who_body;

    /* PvX */
    if (context->style == TYPE_PVX)
    {
        /* PvP: un-power attack */
        if (context->target->player)
        {
            bool dummy;

            source_player(who, 0, context->p);
            un_power(context->target->player, who, &dummy);
        }
        return;
    }

    /* Take damage */
	if (monster_damage_target(context)) return;

    source_monster(who, context->mon);
    un_power(context->p, who, &context->obvious);
}


/*
 * Melee effect handler: Take the player's gold.
 */
static void melee_effect_handler_EAT_GOLD(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == TYPE_PVX)
    {
        if (context->target->monster) return;

        /* PvP: eat-gold attack */
        if (!context->target->player->timed[TMD_PARALYZED] &&
            magik(adj_dex_safe[context->target->player->state.stat_ind[STAT_DEX]] +
            context->target->player->lev))
        {
            /* Saving throw message */
            msg(context->target->player, "You quickly protect your money pouch!");
        }
        else
        {
            struct source who_body;
            struct source *who = &who_body;

            source_player(who, 0, context->p);
            eat_gold(context->target->player, who);
        }
        return;
    }

    /* Take damage */
	if (monster_damage_target(context)) return;

    /* Obvious */
    context->obvious = true;

    /* Attempt saving throw (unless paralyzed) based on dex and level */
    if (!context->p->timed[TMD_PARALYZED] &&
        magik(adj_dex_safe[context->p->state.stat_ind[STAT_DEX]] + context->p->lev))
    {
        /* Saving throw message */
        msg(context->p, "You quickly protect your money pouch!");

        /* Occasional blink anyway */
        if (randint0(3)) context->blinked = 2;
    }
    else
    {
        struct source who_body;
        struct source *who = &who_body;

        source_monster(who, context->mon);
        eat_gold(context->p, who);

        /* Blink away */
        context->blinked = 2;
    }
}


/*
 * Melee effect handler: Take something from the player's inventory.
 */
static void melee_effect_handler_EAT_ITEM(melee_effect_handler_context_t *context)
{
    struct source who_body;
    struct source *who = &who_body;

    /* PvX */
    if (context->style == TYPE_PVX)
    {
        if (context->target->monster) return;

        /* PvP: eat-item attack */
        if (!context->target->player->timed[TMD_PARALYZED] &&
            magik(adj_dex_safe[context->target->player->state.stat_ind[STAT_DEX]] +
            context->target->player->lev))
        {
            /* Saving throw message */
            msg(context->target->player, "You grab hold of your backpack!");
        }
        else
        {
            bool dummy;
            int dummy2;

            source_player(who, 0, context->p);
            steal_player_item(context->target->player, who, &dummy, &dummy2);
        }
        return;
    }

    /* Take damage */
	if (monster_damage_target(context)) return;

    /* Saving throw (unless paralyzed) based on dex and level */
    if (!context->p->timed[TMD_PARALYZED] &&
        magik(adj_dex_safe[context->p->state.stat_ind[STAT_DEX]] + context->p->lev))
    {
        /* Saving throw message */
        msg(context->p, "You grab hold of your backpack!");

        /* Occasional "blink" anyway */
        context->blinked = 2;

        /* Obvious */
        context->obvious = true;

        /* Done */
        return;
    }

    source_monster(who, context->mon);
    steal_player_item(context->p, who, &context->obvious, &context->blinked);
}


/*
 * Melee effect handler: Eat the player's food.
 */
static void melee_effect_handler_EAT_FOOD(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == TYPE_PVX)
    {
        /* PvP: eat-food attack */
        if (context->target->player)
        {
            bool dummy;

            eat_fud(context->target->player, context->p, &dummy);
        }
        return;
    }

    /* Take damage */
	if (monster_damage_target(context)) return;

    eat_fud(context->p, NULL, &context->obvious);
}


/*
 * Melee effect handler: Absorb the player's light.
 */
static void melee_effect_handler_EAT_LIGHT(melee_effect_handler_context_t *context)
{
    struct source who_body;
    struct source *who = &who_body;

    /* PvX */
    if (context->style == TYPE_PVX)
    {
        /* PvP: eat-light attack */
        if (context->target->player)
        {
            bool dummy;

            source_player(who, get_player_index(get_connection(context->target->player->conn)),
                context->target->player);
            effect_simple(EF_DRAIN_LIGHT, who, "250+1d250", 0, 0, 0, 0, 0, &dummy);
        }
        return;
    }

    /* Take damage */
	if (monster_damage_target(context)) return;

    /* Drain the light source */
    source_player(who, get_player_index(get_connection(context->p->conn)), context->p);
    effect_simple(EF_DRAIN_LIGHT, who, "250+1d250", 0, 0, 0, 0, 0, &context->obvious);
}


/*
 * Melee effect handler: Attack the player with acid.
 */
static void melee_effect_handler_ACID(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == TYPE_PVX) return;

	if (context->style == TYPE_MVM)
        monster_elemental_damage(context, RF_IM_ACID, 0);
    else
	    melee_effect_elemental(context, PROJ_ACID, true, "dissolved");
}


/*
 * Melee effect handler: Attack the player with electricity.
 */
static void melee_effect_handler_ELEC(melee_effect_handler_context_t *context)
{
	/* PvX */
    if (context->style == TYPE_PVX) return;

	if (context->style == TYPE_MVM)
        monster_elemental_damage(context, RF_IM_ELEC, 0);
	else
        melee_effect_elemental(context, PROJ_ELEC, true, "electrocuted");
}


/*
 * Melee effect handler: Attack the player with fire.
 */
static void melee_effect_handler_FIRE(melee_effect_handler_context_t *context)
{
	/* PvX */
    if (context->style == TYPE_PVX) return;

	if (context->style == TYPE_MVM)
        monster_elemental_damage(context, RF_IM_FIRE, RF_HURT_FIRE);
    else
	    melee_effect_elemental(context, PROJ_FIRE, true, "fried");
}


/*
 * Melee effect handler: Attack the player with cold.
 */
static void melee_effect_handler_COLD(melee_effect_handler_context_t *context)
{
	/* PvX */
    if (context->style == TYPE_PVX) return;

	if (context->style == TYPE_MVM)
        monster_elemental_damage(context, RF_IM_COLD, RF_HURT_COLD);
    else
	    melee_effect_elemental(context, PROJ_COLD, true, "frozen");
}


/*
 * Melee effect handler: Blind the player.
 */
static void melee_effect_handler_BLIND(melee_effect_handler_context_t *context)
{
	/* PvX */
    if (context->style == TYPE_PVX)
    {
        /* Blinding attack */
        context->do_blind = true;
        return;
    }

    /* MvM */
    if (context->style == TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(&context->mon->wpos), context->target->monster, context->damage,
            context->note_dies);
        if (context->dead) return;

        /* Obvious */
        context->obvious = true;

        /* Blinding attack */
        context->do_blind = true;
        return;
    }

    melee_effect_timed(context, TMD_BLIND, 10 + randint1(context->rlev), OF_PROT_BLIND, false, NULL,
        false);
}


/*
 * Melee effect handler: Confuse the player.
 */
static void melee_effect_handler_CONFUSE(melee_effect_handler_context_t *context)
{
	/* PvX */
    if (context->style == TYPE_PVX)
    {
        /* Confusing attack */
        context->do_conf = true;
        return;
    }

    /* MvM */
    if (context->style == TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(&context->mon->wpos), context->target->monster, context->damage,
            context->note_dies);
        if (context->dead) return;

        /* Obvious */
        context->obvious = true;

        /* Confusing attack */
        context->do_conf = true;
        return;
    }

    melee_effect_timed(context, TMD_CONFUSED, 3 + randint1(context->rlev), OF_PROT_CONF, false,
        NULL, false);
}


/*
 * Melee effect handler: Terrify the player.
 */
static void melee_effect_handler_TERRIFY(melee_effect_handler_context_t *context)
{
	/* PvX */
    if (context->style == TYPE_PVX)
    {
        /* Fear attack */
        context->do_fear = true;
        return;
    }

    /* MvM */
    if (context->style == TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(&context->mon->wpos), context->target->monster, context->damage,
            context->note_dies);
        if (context->dead) return;

        /* Obvious */
        context->obvious = true;

        /* Fear attack */
        context->do_fear = true;
        return;
    }

    melee_effect_timed(context, TMD_AFRAID, 3 + randint1(context->rlev), OF_PROT_FEAR, true,
        "You stand your ground!", false);
}


/*
 * Melee effect handler: Paralyze the player.
 */
static void melee_effect_handler_PARALYZE(melee_effect_handler_context_t *context)
{
	/* PvX */
    if (context->style == TYPE_PVX)
    {
        if (context->target->monster)
        {
            /* Paralyzing attack */
            context->do_para = true;
            return;
        }

        /* PvP: paralyzing attack */
        if (magik(context->target->player->state.skills[SKILL_SAVE]))
            msg(context->p, "%s resists the effect.", context->ddesc);
        else
        {
            player_inc_timed(context->target->player, TMD_PARALYZED, 3 + randint1(context->p->lev),
                true, true);
        }
        return;
    }

    /* MvM */
    if (context->style == TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(&context->mon->wpos), context->target->monster, context->damage,
            context->note_dies);
        if (context->dead) return;

        /* Obvious */
        context->obvious = true;

        /* Paralyzing attack */
        context->do_para = true;
        return;
    }

    melee_effect_timed(context, TMD_PARALYZED, 3 + randint1(context->rlev), OF_FREE_ACT, true,
        "You resist the effects!", true);
}


/*
 * Melee effect handler: Drain the player's strength.
 */
static void melee_effect_handler_LOSE_STR(melee_effect_handler_context_t *context)
{
	melee_effect_stat(context, STAT_STR, OF_SUST_STR);
}


/*
 * Melee effect handler: Drain the player's intelligence.
 */
static void melee_effect_handler_LOSE_INT(melee_effect_handler_context_t *context)
{
	melee_effect_stat(context, STAT_INT, OF_SUST_INT);
}


/*
 * Melee effect handler: Drain the player's wisdom.
 */
static void melee_effect_handler_LOSE_WIS(melee_effect_handler_context_t *context)
{
	melee_effect_stat(context, STAT_WIS, OF_SUST_WIS);
}


/*
 * Melee effect handler: Drain the player's dexterity.
 */
static void melee_effect_handler_LOSE_DEX(melee_effect_handler_context_t *context)
{
	melee_effect_stat(context, STAT_DEX, OF_SUST_DEX);
}


/*
 * Melee effect handler: Drain the player's constitution.
 */
static void melee_effect_handler_LOSE_CON(melee_effect_handler_context_t *context)
{
	melee_effect_stat(context, STAT_CON, OF_SUST_CON);
}


/*
 * Melee effect handler: Drain all of the player's stats.
 */
static void melee_effect_handler_LOSE_ALL(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == TYPE_PVX)
    {
        /* PvP: lose-all attack */
        if (context->target->player)
        {
            struct source who_body;
            struct source *who = &who_body;

            source_player(who, get_player_index(get_connection(context->target->player->conn)),
                context->target->player);
            effect_simple(EF_DRAIN_STAT, who, "0", STAT_STR, 0, 0, 0, 0, NULL);
            effect_simple(EF_DRAIN_STAT, who, "0", STAT_DEX, 0, 0, 0, 0, NULL);
            effect_simple(EF_DRAIN_STAT, who, "0", STAT_CON, 0, 0, 0, 0, NULL);
            effect_simple(EF_DRAIN_STAT, who, "0", STAT_INT, 0, 0, 0, 0, NULL);
            effect_simple(EF_DRAIN_STAT, who, "0", STAT_WIS, 0, 0, 0, 0, NULL);
        }
        return;
    }

    /* Take damage */
	if (monster_damage_target(context)) return;

	/* Damage (stats) */
    if (resist_undead_attacks(context->p, context->mon->race))
    {
        msg(context->p, "You feel %s for a moment, but the feeling passes.",
            desc_stat(STAT_STR, false));
        msg(context->p, "You feel %s for a moment, but the feeling passes.",
            desc_stat(STAT_DEX, false));
        msg(context->p, "You feel %s for a moment, but the feeling passes.",
            desc_stat(STAT_CON, false));
        msg(context->p, "You feel %s for a moment, but the feeling passes.",
            desc_stat(STAT_INT, false));
        msg(context->p, "You feel %s for a moment, but the feeling passes.",
            desc_stat(STAT_WIS, false));
        equip_learn_flag(context->p, OF_SUST_STR);
        equip_learn_flag(context->p, OF_SUST_INT);
        equip_learn_flag(context->p, OF_SUST_WIS);
        equip_learn_flag(context->p, OF_SUST_DEX);
        equip_learn_flag(context->p, OF_SUST_CON);
        context->obvious = true;
    }
    else
    {
        struct source who_body;
        struct source *who = &who_body;

        source_player(who, get_player_index(get_connection(context->p->conn)), context->p);
        effect_simple(EF_DRAIN_STAT, who, "0", STAT_STR, 0, 0, 0, 0, &context->obvious);
        effect_simple(EF_DRAIN_STAT, who, "0", STAT_DEX, 0, 0, 0, 0, &context->obvious);
        effect_simple(EF_DRAIN_STAT, who, "0", STAT_CON, 0, 0, 0, 0, &context->obvious);
        effect_simple(EF_DRAIN_STAT, who, "0", STAT_INT, 0, 0, 0, 0, &context->obvious);
        effect_simple(EF_DRAIN_STAT, who, "0", STAT_WIS, 0, 0, 0, 0, &context->obvious);
    }
}


/*
 * Melee effect handler: Cause an earthquake around the player.
 */
static void melee_effect_handler_SHATTER(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == TYPE_PVX)
    {
        /* Quaking attack */
        if (context->damage > 50) context->do_quake = true;
        return;
    }

    /* Obvious */
	context->obvious = true;

    /* Hack -- reduce damage based on the player armor class */
    context->damage = adjust_dam_armor(context->damage, context->ac);

    /* MvM */
    if (context->style == TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon, chunk_get(&context->mon->wpos),
            context->target->monster, context->damage, context->note_dies);
        if (context->dead) return;
    }
    else
    {
        char df[160];

        strnfmt(df, sizeof(df), "was splattered by %s", context->ddesc);

        /* Take damage */
        if (take_hit(context->p, context->damage, context->ddesc, false, df)) return;
    }

    /* Earthquake centered at the monster, radius damage-determined */
    if (context->damage > 23)
    {
        int radius = context->damage / 12;
        struct source who_body;
        struct source *who = &who_body;

        source_player(who, get_player_index(get_connection(context->p->conn)), context->p);
        who->monster = context->mon;
        effect_simple(EF_EARTHQUAKE, who, "0", 0, radius, 0, 0, 0, NULL);
    }

    /* Chance of knockback */
    if (context->damage > 100)
    {
        int value = context->damage - 100;

        if (randint1(value) > 40)
        {
            int dist = 1 + value / 40;
            struct source who_body;
            struct source *who = &who_body;
            struct loc centre;

            if (context->style == TYPE_MVM)
            {
                source_monster(who, context->mon);
                who->player = context->p;
                loc_copy(&centre, &context->mon->grid);
            }
            else
            {
                source_player(who, get_player_index(get_connection(context->p->conn)), context->p);
                loc_copy(&centre, &context->p->grid);
            }

            thrust_away(chunk_get(&context->mon->wpos), who, &centre, dist);
        }
    }
}


/*
 * Melee effect handler: Drain the player's experience.
 */
static void melee_effect_handler_EXP_10(melee_effect_handler_context_t *context)
{
	melee_effect_experience(context, 10);
}


/*
 * Melee effect handler: Drain the player's experience.
 */
static void melee_effect_handler_EXP_20(melee_effect_handler_context_t *context)
{
	melee_effect_experience(context, 20);
}


/*
 * Melee effect handler: Drain the player's experience.
 */
static void melee_effect_handler_EXP_40(melee_effect_handler_context_t *context)
{
	melee_effect_experience(context, 40);
}


/*
 * Melee effect handler: Drain the player's experience.
 */
static void melee_effect_handler_EXP_80(melee_effect_handler_context_t *context)
{
	melee_effect_experience(context, 80);
}


/*
 * Melee effect handler: Make the player hallucinate.
 *
 * Note that we don't use melee_effect_timed(), due to the different monster
 * learning function.
 */
static void melee_effect_handler_HALLU(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == TYPE_PVX)
    {
        if (context->target->monster)
        {
            /* Confusing attack */
            context->do_conf = true;

            return;
        }

        /* PvP: hallucinatory attack */
        player_inc_timed(context->target->player, TMD_IMAGE, 3 + randint1(context->p->lev / 2), true,
            true);
        return;
    }

    /* MvM */
    if (context->style == TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(&context->mon->wpos), context->target->monster, context->damage,
            context->note_dies);
        if (context->dead) return;

        /* Obvious */
        context->obvious = true;

        /* Confusing attack */
        context->do_conf = true;
        return;
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false, context->flav)) return;

    /* Increase "image" */
    context->obvious = player_inc_timed(context->p, TMD_IMAGE, 3 + randint1(context->rlev / 2),
        true, true);

    /* Learn about the player */
    update_smart_learn(context->mon, context->p, 0, 0, ELEM_CHAOS);
}


/*
 * Melee effect handler: Give the player Black Breath.
 *
 * Note that we don't use melee_effect_timed(), as this is unresistable.
 */
static void melee_effect_handler_BLACK_BREATH(melee_effect_handler_context_t *context)
{
    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false, context->flav)) return;

    /* Increase Black Breath counter a *small* amount, maybe */
    if (one_in_(5) && player_inc_timed(context->p, TMD_BLACKBREATH, context->damage / 10, true, false))
        context->obvious = true;
}


static void melee_effect_handler_FORGET(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == TYPE_PVX)
    {
        if (context->target->monster) return;

        /* PvP: forget attack */
        if (magik(context->target->player->state.skills[SKILL_SAVE]))
            msg(context->p, "%s is unaffected.", context->ddesc);
        else
            player_inc_timed(context->target->player, TMD_AMNESIA, 4, true, true);
        return;
    }

    /* Take damage */
	if (monster_damage_target(context)) return;

    /* Increase "amnesia" */
    if (magik(context->p->state.skills[SKILL_SAVE]) ||
        resist_undead_attacks(context->p, context->mon->race))
    {
        msg(context->p, "You resist the effects!");
        context->obvious = true;
    }
    else
        context->obvious = player_inc_timed(context->p, TMD_AMNESIA, 4, true, true);
}


static void melee_effect_handler_DISEASE(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == TYPE_PVX)
    {
        /* PvP: lose-con attack */
        if (context->target->player)
        {
            struct source who_body;
            struct source *who = &who_body;

            source_player(who, get_player_index(get_connection(context->target->player->conn)),
                context->target->player);
            effect_simple(EF_DRAIN_STAT, who, "0", STAT_CON, 0, 0, 0, 0, NULL);
        }
        return;
    }

    melee_effect_handler_POISON(context);

    /* MvM */
    if (context->style == TYPE_MVM) return;

    /* Damage (stat) */
    if (resist_undead_attacks(context->p, context->mon->race))
    {
        msg(context->p, "You feel %s for a moment, but the feeling passes.",
            desc_stat(STAT_CON, false));
        equip_learn_flag(context->p, OF_SUST_CON);
        context->obvious = true;
    }
    else
    {
        struct source who_body;
        struct source *who = &who_body;

        source_player(who, get_player_index(get_connection(context->p->conn)), context->p);
        effect_simple(EF_DRAIN_STAT, who, "0", STAT_CON, 0, 0, 0, 0, &context->obvious);
    }
}


static void melee_effect_handler_TIME(melee_effect_handler_context_t *context)
{
    struct source who_body;
    struct source *who = &who_body;

    /* PvX */
    if (context->style == TYPE_PVX)
    {
        /* PvP: time attack */
        if (context->target->player)
        {
            source_player(who, 0, context->p);
            project_player_time_effects(context->target->player, who);
        }
        return;
    }

    /* Take damage */
	if (monster_damage_target(context)) return;

    /* Take "time" effect */
    source_monster(who, context->mon);
    project_player_time_effects(context->p, who);
    context->obvious = true;

    /* Learn about the player */
    update_smart_learn(context->mon, context->p, 0, 0, ELEM_TIME);
}


static void melee_effect_handler_DISARM(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == TYPE_PVX)
    {
        /* PvP: disarming attack */
        if (context->target->player) drop_weapon(context->target->player, context->damage);
        return;
    }

    /* Take damage */
	if (monster_damage_target(context)) return;

    drop_weapon(context->p, context->damage);
    context->obvious = true;
}


static void melee_effect_handler_FAMINE(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == TYPE_PVX)
    {
        /* PvP: hunger attack */
        if (context->target->player && !context->target->player->ghost)
        {
            msg(context->target->player, "You have a sudden attack of hunger!");
            player_set_timed(context->target->player, TMD_FOOD,
                context->target->player->timed[TMD_FOOD] / 2, false);
        }
        return;
    }

    /* Take damage */
	if (monster_damage_target(context)) return;

    /* Take "hunger" effect */
    if (!context->p->ghost)
    {
        msg(context->p, "You have a sudden attack of hunger!");
        player_set_timed(context->p, TMD_FOOD, context->p->timed[TMD_FOOD] / 2, false);
        context->obvious = true;
    }
}


static void undress(struct player *p)
{
    int i, count = 0;
    struct object *obj;
    char o_name[NORMAL_WID];

    /* Count slots */
    for (i = 0; i < p->body.count; i++)
    {
        /* Ignore non armor parts */
        if (slot_type_is(p, i, EQUIP_WEAPON)) continue;
        if (slot_type_is(p, i, EQUIP_BOW)) continue;
        if (slot_type_is(p, i, EQUIP_RING)) continue;
        if (slot_type_is(p, i, EQUIP_AMULET)) continue;
        if (slot_type_is(p, i, EQUIP_LIGHT)) continue;
        if (slot_type_is(p, i, EQUIP_TOOL)) continue;

        obj = slot_object(p, i);

        /* No item */
        if (!obj) continue;

        /* Item is stuck */
        if (!obj_can_takeoff(obj)) continue;

        /* Count usable slots */
        count++;
    }

    /* Already naked */
    if (!count) return;

    /* Pick one at random */
    for (i = p->body.count - 1; i >= 0; i--)
    {
        /* Ignore non armor parts */
        if (slot_type_is(p, i, EQUIP_WEAPON)) continue;
        if (slot_type_is(p, i, EQUIP_BOW)) continue;
        if (slot_type_is(p, i, EQUIP_RING)) continue;
        if (slot_type_is(p, i, EQUIP_AMULET)) continue;
        if (slot_type_is(p, i, EQUIP_LIGHT)) continue;
        if (slot_type_is(p, i, EQUIP_TOOL)) continue;

        obj = slot_object(p, i);

        /* No item */
        if (!obj) continue;

        /* Item is stuck */
        if (!obj_can_takeoff(obj)) continue;

        if (one_in_(count--)) break;
    }

    /* Describe the object */
    object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);

    /* Message */
    msg(p, "You remove your %s...", o_name);

    /* De-equip the object */
    p->body.slots[i].obj = NULL;
    p->upkeep->equip_cnt--;

    p->upkeep->update |= (PU_BONUS | PU_INVEN | PU_UPDATE_VIEW);
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP | PR_PLUSSES);
    p->upkeep->notice |= (PN_IGNORE);
    update_stuff(p, chunk_get(&p->wpos));

    combine_pack(p);
}


/*
 * Melee effect handler: Seduce the player.
 */
static void melee_effect_handler_SEDUCE(melee_effect_handler_context_t *context)
{
	bool opposite;

    /* PvX */
    if (context->style == TYPE_PVX)
    {
        if (context->target->monster)
        {
            opposite = (((context->p->psex == SEX_MALE) &&
                rf_has(context->target->monster->race->flags, RF_FEMALE)) ||
                ((context->p->psex == SEX_FEMALE) &&
                rf_has(context->target->monster->race->flags, RF_MALE)));

            /* Must be of opposite sex */
            if (!opposite) return;

            /* Confusing attack */
            context->do_conf = true;

            return;
        }

        opposite = (((context->p->psex == SEX_MALE) &&
            (context->target->player->psex == SEX_FEMALE)) ||
            ((context->p->psex == SEX_FEMALE) &&
            (context->target->player->psex == SEX_MALE)));

        /* Must be of opposite sex */
        if (!opposite) return;

        /* PvP: seduce attack */
        undress(context->target->player);
    }

    /* MvM */
    if (context->style == TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(&context->mon->wpos), context->target->monster, context->damage,
            context->note_dies);
        if (context->dead) return;

        /* Obvious */
        context->obvious = true;

        opposite = ((rf_has(context->mon->race->flags, RF_MALE) &&
                rf_has(context->target->monster->race->flags, RF_FEMALE)) ||
                (rf_has(context->mon->race->flags, RF_FEMALE) &&
                rf_has(context->target->monster->race->flags, RF_MALE)));

        /* Must be of opposite sex */
        if (!opposite) return;

        /* Confusing attack */
        context->do_conf = true;
        return;
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false, context->flav)) return;

    /* Obvious */
    context->obvious = true;

    opposite = ((rf_has(context->mon->race->flags, RF_MALE) &&
        (context->p->psex == SEX_FEMALE)) ||
        (rf_has(context->mon->race->flags, RF_FEMALE) &&
        (context->p->psex == SEX_MALE)));

    /* Must be of opposite sex */
    if (!opposite) return;

    undress(context->p);
}


/*
 * Monster blow melee handler selection
 */


/*
 * Return a handler for the given effect.
 */
melee_effect_handler_f melee_handler_for_blow_effect(const char *name)
{
	static const struct effect_handler_s
    {
        const char *name;
        melee_effect_handler_f function;
    } effect_handlers[] =
    {
        {"NONE", melee_effect_handler_NONE},
        {"HURT", melee_effect_handler_HURT},
        {"POISON", melee_effect_handler_POISON},
        {"DISENCHANT", melee_effect_handler_DISENCHANT},
        {"DRAIN_CHARGES", melee_effect_handler_DRAIN_CHARGES},
        {"EAT_GOLD", melee_effect_handler_EAT_GOLD},
        {"EAT_ITEM", melee_effect_handler_EAT_ITEM},
        {"EAT_FOOD", melee_effect_handler_EAT_FOOD},
        {"EAT_LIGHT", melee_effect_handler_EAT_LIGHT},
        {"ACID", melee_effect_handler_ACID},
        {"ELEC", melee_effect_handler_ELEC},
        {"FIRE", melee_effect_handler_FIRE},
        {"COLD", melee_effect_handler_COLD},
        {"BLIND", melee_effect_handler_BLIND},
        {"CONFUSE", melee_effect_handler_CONFUSE},
        {"TERRIFY", melee_effect_handler_TERRIFY},
        {"PARALYZE", melee_effect_handler_PARALYZE},
        {"LOSE_STR", melee_effect_handler_LOSE_STR},
        {"LOSE_INT", melee_effect_handler_LOSE_INT},
        {"LOSE_WIS", melee_effect_handler_LOSE_WIS},
        {"LOSE_DEX", melee_effect_handler_LOSE_DEX},
        {"LOSE_CON", melee_effect_handler_LOSE_CON},
        {"LOSE_ALL", melee_effect_handler_LOSE_ALL},
        {"SHATTER", melee_effect_handler_SHATTER},
        {"EXP_10", melee_effect_handler_EXP_10},
        {"EXP_20", melee_effect_handler_EXP_20},
        {"EXP_40", melee_effect_handler_EXP_40},
        {"EXP_80", melee_effect_handler_EXP_80},
        {"HALLU", melee_effect_handler_HALLU},
        {"BLACK_BREATH", melee_effect_handler_BLACK_BREATH},
        {"FORGET", melee_effect_handler_FORGET},
        {"DISEASE", melee_effect_handler_DISEASE},
        {"TIME", melee_effect_handler_TIME},
        {"DISARM", melee_effect_handler_DISARM},
        {"FAMINE", melee_effect_handler_FAMINE},
        {"SEDUCE", melee_effect_handler_SEDUCE},
        {NULL, NULL}
    };

    int i;

    for (i = 0; effect_handlers[i].name; i++)
    {
        if (streq(name, effect_handlers[i].name))
            return effect_handlers[i].function;
    }

	return NULL;
}


byte blow_method_index(const char *name)
{
    byte i;

    for (i = 0; i < (byte)z_info->blow_methods_max; i++)
    {
        struct blow_method *meth = &blow_methods[i];

        if (streq(name, meth->name)) return i;
    }

    /* We should never come here... */
    return (byte)z_info->blow_methods_max;
}


byte blow_effect_index(const char *name)
{
    byte i;

    for (i = 0; i < (byte)z_info->blow_effects_max; i++)
    {
        struct blow_effect *effect = &blow_effects[i];

        if (streq(name, effect->name)) return i;
    }

    /* We should never come here... */
    return (byte)z_info->blow_effects_max;
}
