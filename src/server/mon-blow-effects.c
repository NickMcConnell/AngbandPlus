/*
 * File: mon-blow-effects.c
 * Purpose: Monster melee effects module.
 *
 * Copyright (c) 1997 Ben Harrison, David Reeve Sward, Keldon Jones.
 * Copyright (c) 2013 Ben Semmler
 * Copyright (c) 2016 MAngband and PWMAngband Developers
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


static void eat_gold(struct player *p, struct actor *who)
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
    if (who->mon)
    {
        struct object *obj = object_new();

        /* Create a new temporary object */
        object_prep(p, obj, money_kind("gold", gold), 0, MINIMISE);

        /* Amount of gold to put in this object */
        obj->pval = gold;

        /* Set origin to stolen, so it is not confused with dropped treasure in monster_death */
        obj->origin = ORIGIN_STOLEN;

        /* Give the gold to the monster */
        if (!monster_carry(who->mon, obj, false))
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
 * Do damage as the result of a melee attack that has an elemental aspect.
 *
 * context is information for the current attack.
 * type is the GF_ constant for the element.
 * pure_element should be true if no side effects (mostly a hack for poison).
 */
static bool melee_effect_elemental(melee_effect_handler_context_t *context, int type,
    bool pure_element, const char *flav_msg)
{
    int physical_dam, elemental_dam;
    bool ret = false;

    /* Obvious */
    if (pure_element) context->obvious = true;

    if (pure_element || !context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was %s by %s", flav_msg,
            context->ddesc);
    }

    switch (type)
    {
        case GF_ACID: msg(context->p, "You are covered in acid!"); break;
        case GF_ELEC: msg(context->p, "You are struck by electricity!"); break;
        case GF_FIRE: msg(context->p, "You are enveloped in flames!"); break;
        case GF_COLD: msg(context->p, "You are covered with frost!"); break;
    }

    /* Give the player a small bonus to ac for elemental attacks */
    physical_dam = adjust_dam_armor(context->damage, context->ac + 50);

    /* Some attacks do no physical damage */
    if (!monster_blow_method_physical(context->method))
        physical_dam = 0;

    elemental_dam = adjust_dam(context->p, type, context->damage, RANDOMISE, 0);

    /* Take the larger of physical or elemental damage */
    context->damage = MAX(physical_dam, elemental_dam);

    if (elemental_dam > 0)
    {
        if (pure_element) inven_damage(context->p, type, MIN(elemental_dam * 5, 300));
        ret = true;
    }
    if (context->damage > 0) take_hit(context->p, context->damage, context->ddesc, false);

    /* Learn about the player */
    if (pure_element) update_smart_learn(context->mon, context->p, 0, 0, type);

    return ret;
}


static void melee_effect_elemental_MvM(melee_effect_handler_context_t *context, int imm_flag,
    int suscept_flag)
{
    int mult = 3;

    /* Obvious */
    context->obvious = true;

    /* Notice immunity */
    if (rf_has(context->target->mon->race->flags, imm_flag))
    {
        mult = 1;
        if (context->visible) rf_on(context->target_l_ptr->flags, imm_flag);
    }

    /* Notice susceptibility */
    else if (suscept_flag && rf_has(context->target->mon->race->flags, suscept_flag))
    {
        mult = 6;
        if (context->visible) rf_on(context->target_l_ptr->flags, suscept_flag);
    }

    /* Take some damage */
    context->dead = project_m_monster_attack_aux(context->mon, chunk_get(context->mon->depth),
        context->target->mon, context->damage * mult, context->note_dies);
}


/*
 * Do damage as the result of a melee attack that has a status effect.
 *
 * context is the information for the current attack.
 * type is the TMD_ constant for the effect.
 * amount is the amount that the timer should be increased by.
 * of_flag is the OF_ flag that is passed on to monster learning for this effect.
 * attempt_save indicates if a saving throw should be attempted for this effect.
 * save_msg is the message that is displayed if the saving throw is successful.
 */
static void melee_effect_timed(melee_effect_handler_context_t *context, int type, int amount,
    int of_flag, bool attempt_save, const char *save_msg, bool paralyze)
{
	if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false)) return;

	/* Perform a saving throw if desired. */
	if ((attempt_save && magik(context->p->state.skills[SKILL_SAVE])) ||
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
    if (context->style == RBE_TYPE_PVX)
    {
        /* PvP: lose-stat attack */
        if (context->target->player)
            effect_simple(context->target->player, EF_DRAIN_STAT, "0", stat, 0, 0, NULL, NULL);
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take some damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        return;
    }

    if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false)) return;

	/* Damage (stat) */
    if (resist_undead_attacks(context->p, context->mon->race))
    {
        msg(context->p, "You feel %s for a moment, but the feeling passes.", desc_stat_neg[stat]);
        equip_notice_flag(context->p, of_flag);
        context->obvious = true;
    }
    else
        effect_simple(context->p, EF_DRAIN_STAT, "0", stat, 0, 0, &context->obvious, NULL);
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
    if (context->style == RBE_TYPE_PVX)
    {
        if (context->target->mon) return;

        /* PvP: lose-exp attack */
        drain_xp(context->target->player, drain_amount);
        equip_notice_flag(context->target->player, OF_HOLD_LIFE);
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take some damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        return;
    }

	/* Obvious */
	context->obvious = true;

	if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false)) return;
	update_smart_learn(context->mon, context->p, OF_HOLD_LIFE, 0, -1);

    if (resist_undead_attacks(context->p, context->mon->race))
        msg(context->p, "You keep hold of your life force!");
    else
        drain_xp(context->p, drain_amount);
}


/*
 * Melee effect handler: Hit the player, but don't do any damage.
 */
static void melee_effect_handler_NONE(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == RBE_TYPE_PVX) return;

	/* Hack -- assume obvious */
	context->obvious = true;

	/* Hack -- no damage */
	context->damage = 0;
}


/*
 * Melee effect handler: Hurt the player with no side effects.
 */
static void melee_effect_handler_HURT(melee_effect_handler_context_t *context)
{
	/* PvX */
    if (context->style == RBE_TYPE_PVX) return;

	/* Obvious */
	context->obvious = true;

	/* Hack -- player/monster armor reduces total damage */
	context->damage = adjust_dam_armor(context->damage, context->ac);

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        return;
    }

	if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	take_hit(context->p, context->damage, context->ddesc, false);
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
    if (context->style == RBE_TYPE_PVX) return;

	/* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        int mult = 3;

        /* Obvious */
        context->obvious = true;

        /* Notice immunity */
        if (rf_has(context->target->mon->race->flags, RF_IM_POIS)) mult = 1;

        /* Take some damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage * mult,
            context->note_dies);
        if (context->dead) return;

        /* Take "poison" effect */
        mon_inc_timed(context->p, context->target->mon, MON_TMD_POIS, randint1(context->rlev) + 5,
            MON_TMD_FLG_NOTIFY | MON_TMD_MON_SOURCE, false);
        return;
    }

	/* Take "poison" effect */
    if (melee_effect_elemental(context, GF_POIS, false, "killed"))
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
    if (context->style == RBE_TYPE_PVX)
    {
        if (context->target->mon) return;

        /* PvP: un-bonus attack */
        equip_notice_element(context->target->player, ELEM_DISEN);
        if (player_resists(context->target->player, ELEM_DISEN))
            msg(context->p, "%s is unaffected.", context->ddesc);
        else
            effect_simple(context->target->player, EF_DISENCHANT, "0", 0, 0, 0, NULL, NULL);
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take some damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        return;
    }

	if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false)) return;

	/* Apply disenchantment if no resist */
	if (!player_resists(context->p, ELEM_DISEN))
		effect_simple(context->p, EF_DISENCHANT, "0", 0, 0, 0, &context->obvious, NULL);

	/* Learn about the player */
	update_smart_learn(context->mon, context->p, 0, 0, ELEM_DISEN);
}


/*
 * Melee effect handler: Drain charges from the player's inventory.
 */
static void melee_effect_handler_DRAIN_CHARGES(melee_effect_handler_context_t *context)
{
    struct actor who_body;
    struct actor *who = &who_body;

    /* PvX */
    if (context->style == RBE_TYPE_PVX)
    {
        /* PvP: un-power attack */
        if (context->target->player)
        {
            bool dummy;

            ACTOR_PLAYER(who, 0, context->p);
            un_power(context->target->player, who, &dummy);
        }
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take some damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        return;
    }

    if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false)) return;

    ACTOR_MONSTER(who, context->mon);
    un_power(context->p, who, &context->obvious);
}


/*
 * Melee effect handler: Take the player's gold.
 */
static void melee_effect_handler_EAT_GOLD(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == RBE_TYPE_PVX)
    {
        if (context->target->mon) return;

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
            struct actor who_body;
            struct actor *who = &who_body;

            ACTOR_PLAYER(who, 0, context->p);
            eat_gold(context->target->player, who);
        }
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take some damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        return;
    }

    if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false)) return;

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
        struct actor who_body;
        struct actor *who = &who_body;

        ACTOR_MONSTER(who, context->mon);
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
    struct actor who_body;
    struct actor *who = &who_body;

    /* PvX */
    if (context->style == RBE_TYPE_PVX)
    {
        if (context->target->mon) return;

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

            ACTOR_PLAYER(who, 0, context->p);
            eat_item(context->target->player, who, &dummy, &dummy2);
        }
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take some damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        return;
    }

    if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false)) return;

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

    ACTOR_MONSTER(who, context->mon);
    eat_item(context->p, who, &context->obvious, &context->blinked);
}


/*
 * Melee effect handler: Eat the player's food.
 */
static void melee_effect_handler_EAT_FOOD(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == RBE_TYPE_PVX)
    {
        /* PvP: eat-food attack */
        if (context->target->player)
        {
            bool dummy;

            eat_fud(context->target->player, context->p, &dummy);
        }
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take some damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        return;
    }

    if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false)) return;

    eat_fud(context->p, NULL, &context->obvious);
}


/*
 * Melee effect handler: Absorb the player's light.
 */
static void melee_effect_handler_EAT_LIGHT(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == RBE_TYPE_PVX)
    {
        /* PvP: eat-light attack */
        if (context->target->player)
        {
            bool dummy;

            eat_light(context->target->player, &dummy);
        }
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take some damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        return;
    }

    if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false)) return;

    eat_light(context->p, &context->obvious);
}


/*
 * Melee effect handler: Attack the player with acid.
 */
static void melee_effect_handler_ACID(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == RBE_TYPE_PVX) return;

	/* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        melee_effect_elemental_MvM(context, RF_IM_ACID, 0);
        return;
    }

	melee_effect_elemental(context, GF_ACID, true, "dissolved");
}


/*
 * Melee effect handler: Attack the player with electricity.
 */
static void melee_effect_handler_ELEC(melee_effect_handler_context_t *context)
{
	/* PvX */
    if (context->style == RBE_TYPE_PVX) return;

	/* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        melee_effect_elemental_MvM(context, RF_IM_ELEC, 0);
        return;
    }

	melee_effect_elemental(context, GF_ELEC, true, "electrocuted");
}


/*
 * Melee effect handler: Attack the player with fire.
 */
static void melee_effect_handler_FIRE(melee_effect_handler_context_t *context)
{
	/* PvX */
    if (context->style == RBE_TYPE_PVX) return;

	/* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        melee_effect_elemental_MvM(context, RF_IM_FIRE, RF_HURT_FIRE);
        return;
    }

	melee_effect_elemental(context, GF_FIRE, true, "fried");
}


/*
 * Melee effect handler: Attack the player with cold.
 */
static void melee_effect_handler_COLD(melee_effect_handler_context_t *context)
{
	/* PvX */
    if (context->style == RBE_TYPE_PVX) return;

	/* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        melee_effect_elemental_MvM(context, RF_IM_COLD, RF_HURT_COLD);
        return;
    }

	melee_effect_elemental(context, GF_COLD, true, "frozen");
}


/*
 * Melee effect handler: Blind the player.
 */
static void melee_effect_handler_BLIND(melee_effect_handler_context_t *context)
{
	/* PvX */
    if (context->style == RBE_TYPE_PVX)
    {
        /* Blinding attack */
        context->do_blind = true;
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
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
    if (context->style == RBE_TYPE_PVX)
    {
        /* Confusing attack */
        context->do_conf = true;
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
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
    if (context->style == RBE_TYPE_PVX)
    {
        /* Fear attack */
        context->do_fear = true;
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
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
    if (context->style == RBE_TYPE_PVX)
    {
        if (context->target->mon)
        {
            /* Stunning attack */
            context->do_stun = 1;

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
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
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
    if (context->style == RBE_TYPE_PVX)
    {
        /* PvP: lose-all attack */
        if (context->target->player)
        {
            effect_simple(context->target->player, EF_DRAIN_STAT, "0", STAT_STR, 0, 0, NULL, NULL);
            effect_simple(context->target->player, EF_DRAIN_STAT, "0", STAT_DEX, 0, 0, NULL, NULL);
            effect_simple(context->target->player, EF_DRAIN_STAT, "0", STAT_CON, 0, 0, NULL, NULL);
            effect_simple(context->target->player, EF_DRAIN_STAT, "0", STAT_INT, 0, 0, NULL, NULL);
            effect_simple(context->target->player, EF_DRAIN_STAT, "0", STAT_WIS, 0, 0, NULL, NULL);
        }
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take some damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        return;
    }

    if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false)) return;

	/* Damage (stats) */
    if (resist_undead_attacks(context->p, context->mon->race))
    {
        msg(context->p, "You feel %s for a moment, but the feeling passes.", desc_stat_neg[STAT_STR]);
        msg(context->p, "You feel %s for a moment, but the feeling passes.", desc_stat_neg[STAT_DEX]);
        msg(context->p, "You feel %s for a moment, but the feeling passes.", desc_stat_neg[STAT_CON]);
        msg(context->p, "You feel %s for a moment, but the feeling passes.", desc_stat_neg[STAT_INT]);
        msg(context->p, "You feel %s for a moment, but the feeling passes.", desc_stat_neg[STAT_WIS]);
        equip_notice_flag(context->p, OF_SUST_STR);
        equip_notice_flag(context->p, OF_SUST_INT);
        equip_notice_flag(context->p, OF_SUST_WIS);
        equip_notice_flag(context->p, OF_SUST_DEX);
        equip_notice_flag(context->p, OF_SUST_CON);
        context->obvious = true;
    }
    else
    {
        effect_simple(context->p, EF_DRAIN_STAT, "0", STAT_STR, 0, 0, &context->obvious, NULL);
        effect_simple(context->p, EF_DRAIN_STAT, "0", STAT_DEX, 0, 0, &context->obvious, NULL);
        effect_simple(context->p, EF_DRAIN_STAT, "0", STAT_CON, 0, 0, &context->obvious, NULL);
        effect_simple(context->p, EF_DRAIN_STAT, "0", STAT_INT, 0, 0, &context->obvious, NULL);
        effect_simple(context->p, EF_DRAIN_STAT, "0", STAT_WIS, 0, 0, &context->obvious, NULL);
    }
}


/*
 * Melee effect handler: Cause an earthquake around the player.
 */
static void melee_effect_handler_SHATTER(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == RBE_TYPE_PVX)
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
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        if (context->dead) return;
    }
    else
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was splattered by %s",
            context->ddesc);

        /* Take damage */
        if (take_hit(context->p, context->damage, context->ddesc, false)) return;
    }

    /* Radius 8 earthquake centered at the monster */
    if (context->damage > 23)
    {
        int px_old = context->p->px;
        int py_old = context->p->py;

        effect_simple(context->p, EF_EARTHQUAKE, "0", 0, 8, 0, NULL, context->mon);

        /* Stop the blows if the player is pushed away */
        if ((px_old != context->p->px) || (py_old != context->p->py))
            context->do_break = true;
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
    if (context->style == RBE_TYPE_PVX)
    {
        if (context->target->mon)
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
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        if (context->dead) return;

        /* Obvious */
        context->obvious = true;

        /* Confusing attack */
        context->do_conf = true;
        return;
    }

    if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false)) return;

    /* Increase "image" */
    context->obvious = player_inc_timed(context->p, TMD_IMAGE, 3 + randint1(context->rlev / 2),
        true, true);

    /* Learn about the player */
    update_smart_learn(context->mon, context->p, 0, 0, ELEM_CHAOS);
}


static void melee_effect_handler_FORGET(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == RBE_TYPE_PVX)
    {
        if (context->target->mon) return;

        /* PvP: forget attack */
        if (magik(context->target->player->state.skills[SKILL_SAVE]))
            msg(context->p, "%s is unaffected.", context->ddesc);
        else
            player_inc_timed(context->target->player, TMD_AMNESIA, 4, true, true);
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        return;
    }

    if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false)) return;

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
    if (context->style == RBE_TYPE_PVX)
    {
        /* PvP: lose-con attack */
        if (context->target->player)
            effect_simple(context->target->player, EF_DRAIN_STAT, "0", STAT_CON, 0, 0, NULL, NULL);
        return;
    }

    melee_effect_handler_POISON(context);

    /* MvM */
    if (context->style == RBE_TYPE_MVM) return;

    /* Damage (stat) */
    if (resist_undead_attacks(context->p, context->mon->race))
    {
        msg(context->p, "You feel %s for a moment, but the feeling passes.", desc_stat_neg[STAT_CON]);
        equip_notice_flag(context->p, OF_SUST_CON);
        context->obvious = true;
    }
    else
        effect_simple(context->p, EF_DRAIN_STAT, "0", STAT_CON, 0, 0, &context->obvious, NULL);
}


static void melee_effect_handler_TIME(melee_effect_handler_context_t *context)
{
    struct actor who_body;
    struct actor *who = &who_body;

    /* PvX */
    if (context->style == RBE_TYPE_PVX)
    {
        /* PvP: time attack */
        if (context->target->player)
        {
            ACTOR_PLAYER(who, 0, context->p);
            project_player_time_effects(context->target->player, who);
        }
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        return;
    }

    if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false)) return;

    /* Take "time" effect */
    ACTOR_MONSTER(who, context->mon);
    project_player_time_effects(context->p, who);
    context->obvious = true;

    /* Learn about the player */
    update_smart_learn(context->mon, context->p, 0, 0, ELEM_TIME);
}


static void melee_effect_handler_DISARM(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == RBE_TYPE_PVX)
    {
        /* PvP: disarming attack */
        if (context->target->player) drop_weapon(context->target->player, context->damage);
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        return;
    }

    if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false)) return;

    drop_weapon(context->p, context->damage);
    context->obvious = true;
}


static void melee_effect_handler_FAMINE(melee_effect_handler_context_t *context)
{
    /* PvX */
    if (context->style == RBE_TYPE_PVX)
    {
        /* PvP: hunger attack */
        if (context->target->player && !context->target->player->ghost)
        {
            msg(context->target->player, "You have a sudden attack of hunger!");
            player_set_food(context->target->player, context->target->player->food / 2);
        }
        return;
    }

    /* MvM */
    if (context->style == RBE_TYPE_MVM)
    {
        /* Take damage */
        context->dead = project_m_monster_attack_aux(context->mon,
            chunk_get(context->mon->depth), context->target->mon, context->damage,
            context->note_dies);
        return;
    }

    if (!context->flav)
    {
        strnfmt(context->p->died_flavor, sizeof(context->p->died_flavor), "was killed by %s",
            context->ddesc);
    }

    /* Take damage */
	if (take_hit(context->p, context->damage, context->ddesc, false)) return;

    /* Take "hunger" effect */
    if (!context->p->ghost)
    {
        msg(context->p, "You have a sudden attack of hunger!");
        player_set_food(context->p, context->p->food / 2);
        context->obvious = true;
    }
}


/*
 * Return a handler for the given effect.
 *
 * Handlers are named after RBE_ constants.
 *
 * effect is the RBE_ constant for the effect.
 * returns a function pointer to handle the effect, or NULL if not found.
 */
melee_effect_handler_f melee_handler_for_blow_effect(monster_blow_effect_t effect)
{
	static const melee_effect_handler_f blow_handlers[] =
    {
        #define RBE(x, p, e, d) melee_effect_handler_##x,
        #include "list-blow-effects.h"
        #undef RBE
        NULL
    };

	if (effect >= RBE_MAX)
		return NULL;

	return blow_handlers[effect];
}


/*
 * Return a power modifier for the given effect.
 *
 * Values are in list-blow-effects.h.
 *
 * effect is the RBE_ constant for the effect.
 */
int monster_blow_effect_power(monster_blow_effect_t effect)
{
	static const int effect_powers[] =
    {
		#define RBE(x, p, e, d) p,
		#include "list-blow-effects.h"
		#undef RBE
        0
	};

	if (effect >= RBE_MAX)
		return 0;

	return effect_powers[effect];
}


/*
 * Return a description for the given monster blow effect flags.
 *
 * Returns an sensible placeholder string for an out-of-range flag.
 * Descriptions are in list-blow-effects.h
 *
 * effect is one of the RBE_ flags.
 */
const char *monster_blow_effect_description(monster_blow_effect_t effect)
{
    static const char *r_blow_effect_description[] =
    {
        #define RBE(x, p, e, d) d,
        #include "list-blow-effects.h"
        #undef RBE
        NULL
    };

    /* Some blows have no effects, so we do want to return whatever is in the table for RBE_NONE */
    if (effect >= RBE_MAX) return "do weird things";

    return r_blow_effect_description[effect];
}


/*
 * Return a power factor for the given effect to evaluate its power.
 *
 * Values are in list-blow-effects.h.
 *
 * effect is the RBE_ constant for the effect.
 */
int monster_blow_effect_eval(monster_blow_effect_t effect)
{
    static const int effect_evals[] =
    {
        #define RBE(x, p, e, d) e,
        #include "list-blow-effects.h"
        #undef RBE
        0
    };

    if (effect >= RBE_MAX)
        return 0;

    return effect_evals[effect];
}


/*
 * Return the RBE_ constant matching the given string.
 *
 * Values are stringified RBE_ constants.
 *
 * string contains a value to search for.
 */
monster_blow_effect_t blow_effect_name_to_idx(const char *string)
{
    int i;
    static const char *r_info_blow_effect[] =
    {
        #define RBE(x, p, e, d) #x,
        #include "list-blow-effects.h"
        #undef RBE
        NULL
    };

    for (i = 0; r_info_blow_effect[i]; i++)
    {
        if (streq(string, r_info_blow_effect[i])) break;
    }

    return i;
}


/*
 * Return whether the given effect is valid.
 *
 * effect is the RBE_ constant for the effect.
 */
bool monster_blow_effect_is_valid(monster_blow_effect_t effect)
{
    return (effect < RBE_MAX);
}
