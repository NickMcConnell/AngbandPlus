/*
 * File: mon-attack.c
 * Purpose: Monster attacks
 *
 * Copyright (c) 1997 Ben Harrison
 * Copyright (c) 2019 MAngband and PWMAngband Developers
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
 * This file deals with monster attacks (including spells) as follows:
 *
 * Give monsters more intelligent attack/spell selection based on
 * observations of previous attacks on the player, and/or by allowing
 * the monster to "cheat" and know the player status.
 *
 * Maintain an idea of the player status, and use that information
 * to occasionally eliminate "ineffective" spell attacks.  We could
 * also eliminate ineffective normal attacks, but there is no reason
 * for the monster to do this, since he gains no benefit.
 * Note that MINDLESS monsters are not allowed to use this code.
 * And non-INTELLIGENT monsters only use it partially effectively.
 *
 * Actually learn what the player resists, and use that information
 * to remove attacks or spells before using them.
 */


/*
 * Check if a monster has a chance of casting a spell this turn
 */
static bool monster_can_cast(struct chunk *c, struct monster *mon, int target_m_dis,
    struct loc *grid)
{
    int chance = mon->race->freq_spell;

    /* Cannot cast spells when blind */
    if (mon->m_timed[MON_TMD_BLIND]) return false;

    /* Not allowed to cast spells */
    if (!chance) return false;

    /* Only do spells occasionally */
    if (!magik(chance)) return false;

    /* Check range */
    if (target_m_dis > z_info->max_range) return false;

    /* Check path (destination could be standing on a wall) */
    if (!projectable(c, &mon->grid, grid, PROJECT_NONE, false)) return false;

    return true;
}


/*
 * Remove the "bad" spells from a spell list
 */
static void remove_bad_spells(struct player *p, struct monster *mon, bitflag f[RSF_SIZE])
{
    bitflag f2[RSF_SIZE];

    /* Hack -- MvM */
    if (!p) return;

    /* Take working copy of spell flags */
    rsf_copy(f2, f);

    /* Don't heal if full */
    if (mon->hp >= mon->maxhp) rsf_off(f2, RSF_HEAL);

    /* Don't heal others if no injuries */
    if (rsf_has(f2, RSF_HEAL_KIN) && !find_any_nearby_injured_kin(chunk_get(&p->wpos), mon))
        rsf_off(f2, RSF_HEAL_KIN);

    /* Don't haste if hasted with time remaining */
    if (mon->m_timed[MON_TMD_FAST] > 10) rsf_off(f2, RSF_HASTE);

    /* Don't teleport to if the player is already next to us */
    if (mon->cdis == 1) rsf_off(f2, RSF_TELE_TO);

    /* Update acquired knowledge */
    if (cfg_ai_learn)
    {
        size_t i;
        bitflag ai_flags[OF_SIZE], ai_pflags[PF_SIZE];
        struct element_info el[ELEM_MAX];
        bool know_something = false;

        /* Occasionally forget player status */
        if (one_in_(100))
        {
            of_wipe(mon->known_pstate.flags);
            pf_wipe(mon->known_pstate.pflags);
            for (i = 0; i < ELEM_MAX; i++)
                mon->known_pstate.el_info[i].res_level = 0;
        }

        /* Use the memorized info */
        of_wipe(ai_flags);
        pf_wipe(ai_pflags);
        of_copy(ai_flags, mon->known_pstate.flags);
        pf_copy(ai_pflags, mon->known_pstate.pflags);
        if (!of_is_empty(ai_flags) || !pf_is_empty(ai_pflags)) know_something = true;

        memset(el, 0, ELEM_MAX * sizeof(struct element_info));
        for (i = 0; i < ELEM_MAX; i++)
        {
            el[i].res_level = mon->known_pstate.el_info[i].res_level;
            if (el[i].res_level != 0) know_something = true;
        }

        /* Cancel out certain flags based on knowledge */
        if (know_something) unset_spells(p, f2, ai_flags, ai_pflags, el, mon->race);
    }

    /* Use working copy of spell flags */
    rsf_copy(f, f2);
}


/*
 * Determine if there is a space near the selected spot in which
 * a summoned creature can appear
 */
static bool summon_possible(struct chunk *c, struct loc *grid)
{
    struct loc begin, end;
    struct loc_iterator iter;

    loc_init(&begin, grid->x - 2, grid->y - 2);
    loc_init(&end, grid->x + 2, grid->y + 2);
    loc_iterator_first(&iter, &begin, &end);

    /* Start at the location, and check 2 grids in each dir */
    do
    {
        /* Ignore illegal locations */
        if (!square_in_bounds(c, &iter.cur)) continue;

        /* Only check a circular area */
        if (distance(grid, &iter.cur) > 2) continue;

        /* No summon on glyph of warding */
        if (square_iswarded(c, &iter.cur)) continue;

        /* If it's empty floor grid in line of sight, we're good */
        if (square_isemptyfloor(c, &iter.cur) && los(c, grid, &iter.cur))
            return true;
    }
    while (loc_iterator_next(&iter));

    return false;
}


/*
 * Have a monster choose a spell to cast.
 *
 * Note that the monster's spell list has already had "useless" spells
 * (bolts that won't hit the player, summons without room, etc.) removed.
 * Perhaps that should be done by this function.
 *
 * Stupid monsters will just pick a spell randomly.  Smart monsters
 * will choose more "intelligently".
 *
 * This function could be an efficiency bottleneck.
 */
static int choose_attack_spell(bitflag *f)
{
    int num = 0;
    byte spells[RSF_MAX];
    int i;

    /* Extract all spells: "innate", "normal", "bizarre" */
    for (i = FLAG_START; i < RSF_MAX; i++)
    {
        if (rsf_has(f, i)) spells[num++] = i;
    }

    /* Paranoia */
    if (num == 0) return 0;

    /* Pick at random */
    return (spells[randint0(num)]);
}


/*
 * Failure rate of a monster's spell, based on spell power and current status
 */
static int monster_spell_failrate(struct monster *mon, int thrown_spell)
{
    int power = MIN(mon->race->spell_power, 1);
    int failrate = 0;

    /* Stupid monsters will never fail (for jellies and such) */
    if (!monster_is_stupid(mon->race))
    {
        /* Base failrate */
        failrate = 25 - (power + 3) / 4;

        /* Fear adds 20% */
        if (mon->m_timed[MON_TMD_FEAR]) failrate += 20;

        /* Confusion and disenchantment add 50% */
        if (mon->m_timed[MON_TMD_CONF] || mon->m_timed[MON_TMD_DISEN]) failrate += 50;
    }

    if (failrate < 0) failrate = 0;

    /* Hack -- pets/slaves will be unlikely to summon */
    if (mon->master && is_spell_summon(thrown_spell)) failrate = 95;

    return failrate;
}


/*
 * Have a monster choose a spell to cast (remove all "useless" spells).
 */
static int get_thrown_spell(struct player *p, struct player *who, struct chunk *c,
    struct monster *mon, int target_m_dis, struct loc *grid)
{
    int thrown_spell, failrate;
    bitflag f[RSF_SIZE];

    /* Check prerequisites */
    if (!monster_can_cast(c, mon, target_m_dis, grid)) return -1;

    /* Extract the racial spell flags */
    rsf_copy(f, mon->race->spell_flags);

    /* Smart monsters can use "desperate" spells */
    if (monster_is_smart(mon->race) && (mon->hp < mon->maxhp / 10) && magik(50))
        ignore_spells(f, RST_DAMAGE | RST_INNATE | RST_MISSILE);

    /* Non-stupid monsters do some filtering */
    if (!monster_is_stupid(mon->race))
    {
        /* Remove the "ineffective" spells */
        remove_bad_spells(who, mon, f);

        /* Check for a clean bolt shot */
        if (test_spells(f, RST_BOLT) && !projectable(c, &mon->grid, grid, PROJECT_STOP, false))
            ignore_spells(f, RST_BOLT);

        /* Check for a possible summon */
        if (!summon_possible(c, &mon->grid))
            ignore_spells(f, RST_SUMMON);
    }

    /* No spells left */
    if (rsf_is_empty(f)) return -1;

    /* Choose a spell to cast */
    thrown_spell = choose_attack_spell(f);

    /* Abort if no spell was chosen */
    if (!thrown_spell) return -1;

    /* Check for spell failure (innate attacks never fail) */
    failrate = monster_spell_failrate(mon, thrown_spell);
    if (!mon_spell_is_innate(thrown_spell) && magik(failrate))
    {
        char m_name[NORMAL_WID];

        /* Get the monster name (or "it") */
        monster_desc(p, m_name, sizeof(m_name), mon, MDESC_CAPITAL);

        /* Message */
        msg(p, "%s tries to cast a spell, but fails.", m_name);

        return -2;
    }

    return thrown_spell;
}


/*
 * Creatures can cast spells, shoot missiles, and breathe.
 *
 * Returns "true" if a spell (or whatever) was (successfully) cast.
 *
 * Perhaps monsters should breathe at locations *near* the player,
 * since this would allow them to inflict "partial" damage.
 *
 * It will not be possible to "correctly" handle the case in which a
 * monster attempts to attack a location which is thought to contain
 * the player, but which in fact is nowhere near the player, since this
 * might induce all sorts of messages about the attack itself, and about
 * the effects of the attack, which the player might or might not be in
 * a position to observe.  Thus, for simplicity, it is probably best to
 * only allow "faulty" attacks by a monster if one of the important grids
 * (probably the initial or final grid) is in fact in view of the player.
 * It may be necessary to actually prevent spell attacks except when the
 * monster actually has line of sight to the player.  Note that a monster
 * could be left in a bizarre situation after the player ducked behind a
 * pillar and then teleported away, for example.
 *
 * Note that this function attempts to optimize the use of spells for the
 * cases in which the monster has no spells, or has spells but cannot use
 * them, or has spells but they will have no "useful" effect.  Note that
 * this function has been an efficiency bottleneck in the past.
 */
bool make_attack_spell(struct source *who, struct chunk *c, struct monster *mon, int target_m_dis)
{
    struct monster_lore *lore = get_lore(who->player, mon->race);
    int thrown_spell;
    bool seen = ((who->player->timed[TMD_BLIND] == 0) && monster_is_visible(who->player, mon->midx));

    /* Stop if player is dead or gone */
    if (!who->player->alive || who->player->is_dead || who->player->upkeep->new_level_method)
        return false;

    /* Choose a spell to cast */
    thrown_spell = get_thrown_spell(who->player, (who->monster? NULL: who->player), c, mon,
        target_m_dis, (who->monster? &who->monster->grid: &who->player->grid));

    /* Abort if no spell was chosen */
    if (thrown_spell < 0) return ((thrown_spell == -1)? false: true);

    /* If we see a hidden monster try to cast a spell, become aware of it */
    if (monster_is_camouflaged(mon)) become_aware(who->player, c, mon);

    /* Cast the spell. */
    disturb(who->player, 1);
    do_mon_spell(who->player, c, who->monster, thrown_spell, mon, seen);

    /* Remember what the monster did */
    if (seen)
    {
        rsf_on(lore->spell_flags, thrown_spell);

        /* Innate spell */
        if (mon_spell_is_innate(thrown_spell))
        {
            if (lore->cast_innate < UCHAR_MAX) lore->cast_innate++;
        }

        /* Bolt or Ball, or Special spell */
        else
        {
            if (lore->cast_spell < UCHAR_MAX) lore->cast_spell++;
        }
    }

    /* Always take note of monsters that kill you */
    if (who->player->is_dead && (lore->pdeaths < SHRT_MAX))
        lore->pdeaths++;
    if (who->player->is_dead && (mon->race->lore.tdeaths < SHRT_MAX))
        mon->race->lore.tdeaths++;

    /* Record any new info */
    lore_update(mon->race, lore);

    /* A spell was cast */
    return true;
}


/*
 * Critical blow.  All hits that do 95% of total possible damage,
 * and which also do at least 20 damage, or, sometimes, N damage.
 * This is used only to determine "cuts" and "stuns".
 */
static int monster_critical(random_value dice, int dam)
{
    int max = 0;
    int total = randcalc(dice, 0, MAXIMISE);

    /* Must do at least 95% of perfect */
    if (dam < total * 19 / 20) return (0);

    /* Weak blows rarely work */
    if ((dam < 20) && !magik(dam)) return (0);

    /* Perfect damage */
    if (dam == total) max++;

    /* Super-charge */
    if (dam >= 20)
    {
        while (magik(2)) max++;
    }

    /* Critical damage */
    if (dam > 45) return (6 + max);
    if (dam > 33) return (5 + max);
    if (dam > 25) return (4 + max);
    if (dam > 18) return (3 + max);
    if (dam > 11) return (2 + max);
    return (1 + max);
}


/*
 * Determine if a monster attack against the player succeeds.
 * Always miss 5% of the time, always hit 12% of the time.
 * Otherwise, match monster power against player armor.
 */
bool check_hit(struct source *who, int power, int level, int accuracy)
{
    int chance, ac;

    /* Calculate the "attack quality" */
    chance = (power + (level * 3));

    /* Total armor */
    if (who->monster)
        ac = who->monster->ac;
    else
    {
        ac = who->player->state.ac + who->player->state.to_a;

        /* If the monster checks vs ac, the player learns ac bonuses */
        equip_learn_on_defend(who->player);
    }

    /* Apply accuracy */
    chance *= accuracy;
    chance /= 100;

    /* Check if the target was hit */
    return test_hit(chance, ac, true);
}


/*
 * Calculate how much damage remains after armor is taken into account
 * (does for a physical attack what adjust_dam does for an elemental attack).
 */
int adjust_dam_armor(int damage, int ac)
{
    return damage - (damage * ((ac < 240)? ac: 240) / 400);
}


/*
 * Attack a target via physical attacks.
 */
bool make_attack_normal(struct monster *mon, struct source *who)
{
    struct monster_lore *lore = get_lore(who->player, mon->race);
    struct monster_lore *target_l_ptr = (who->monster? get_lore(who->player, who->monster->race): NULL);
    int rlev = ((mon->level >= 1)? mon->level: 1);
    int ap_cnt;
    char m_name[NORMAL_WID];
    char target_m_name[NORMAL_WID];
    char ddesc[NORMAL_WID];
    int blinked = 0;

    /* Assume a default death */
    byte note_dies = MON_MSG_DIE;

    /* Some monsters get "destroyed" */
    if (monster_is_destroyed(mon->race))
    {
        /* Special note at death */
        note_dies = MON_MSG_DESTROYED;
    }

    /* Hack -- don't attack shoppers */
    if (in_store(who->player)) return false;

    /* Not allowed to attack */
    if (rf_has(mon->race->flags, RF_NEVER_BLOW)) return false;

    /* Get the monster name (or "it") */
    monster_desc(who->player, m_name, sizeof(m_name), mon, MDESC_STANDARD);
    if (who->monster)
        monster_desc(who->player, target_m_name, sizeof(target_m_name), who->monster, MDESC_DEFAULT);
    else
        my_strcpy(target_m_name, "you", sizeof(target_m_name));

    /* Get the "died from" information (i.e. "a kobold") */
    monster_desc(who->player, ddesc, sizeof(ddesc), mon, MDESC_DIED_FROM);

    /* Scan through all blows */
    for (ap_cnt = 0; ap_cnt < z_info->mon_blows_max; ap_cnt++)
    {
        struct loc grid;
        bool visible = (monster_is_visible(who->player, mon->midx) ||
            rf_has(mon->race->flags, RF_HAS_LIGHT));
        bool obvious = false;
        int damage = 0;
        int do_cut = 0;
        int do_stun = 0;
        int sound_msg = MSG_GENERIC;
        const char *act = NULL;
        bool do_conf = false, do_fear = false, do_blind = false, do_para = false;
        bool dead = false;
        int accuracy = 100 - (mon->m_timed[MON_TMD_STUN]? STUN_HIT_REDUCTION: 0);

        /* Extract the attack infomation */
        struct blow_effect *effect = mon->blow[ap_cnt].effect;
        struct blow_method *method = mon->blow[ap_cnt].method;
        random_value dice = mon->blow[ap_cnt].dice;

        loc_copy(&grid, &who->player->grid);

        /* No more attacks */
        if (!method) break;

        /* Stop if player is dead or gone */
        if (!who->player->alive || who->player->is_dead || who->player->upkeep->new_level_method)
            break;

        /* Monster hits target */
        my_assert(effect);
        if (streq(effect->name, "NONE") || check_hit(who, effect->power, rlev, accuracy))
        {
            melee_effect_handler_f effect_handler;
            const char* flav = NULL;

            /* Always disturbing */
            disturb(who->player, 1);

            /* Hack -- apply "protection from evil" */
            if ((who->player->timed[TMD_PROTEVIL] > 0) && !who->monster)
            {
                /* Learn about the evil flag */
                if (visible) rf_on(lore->flags, RF_EVIL);

                if (monster_is_evil(mon) && (who->player->lev >= rlev) &&
                    !magik(PY_MAX_LEVEL - who->player->lev))
                {
                    /* Message */
                    msg(who->player, "%s is repelled.", m_name);

                    /* Hack -- next attack */
                    continue;
                }
            }

            /* Describe the attack method */
            act = monster_blow_method_action(method);
            do_cut = method->cut;
            do_stun = method->stun;
            sound_msg = method->msgt;
            if (!who->monster) flav = method->flavor;
            if (!flav) flav = "killed";

            /* Hack -- assume all attacks are obvious */
            obvious = true;

            /* Roll dice */
            damage = randcalc(dice, 0, RANDOMISE);

            /* Reduce damage when stunned */
            if (mon->m_timed[MON_TMD_STUN]) damage = (damage * (100 - STUN_DAM_REDUCTION)) / 100;

            /* Message */
            if (act)
            {
                const char *fullstop = ".";
                const char *act_text = "";
                const char *dmg_text = "";

                if (suffix(act, "'") || suffix(act, "!")) fullstop = "";

                if (method->act_msg)
                {
                    act_text = format(act, target_m_name);
                    if (OPT(who->player, show_damage)) dmg_text = format(" (%d)", damage);
                }
                else if (strstr(act, "%s"))
                    act_text = format(act, target_m_name);
                else if (who->monster)
                    act_text = format("insults %s!", target_m_name);
                else
                    act_text = act;

                msgt(who->player, sound_msg, "%s %s%s%s", m_name, act_text, dmg_text, fullstop);
            }

            /* Perform the actual effect. */
            effect_handler = melee_handler_for_blow_effect(effect->name);

            if (effect_handler != NULL)
            {
                melee_effect_handler_context_t context;

                /* Initialize */
                context.p = who->player;
                context.mon = mon;
                context.target = who;
                context.target_l_ptr = target_l_ptr;
                context.rlev = rlev;
                context.method = method;
                context.ac = (who->monster? who->monster->ac:
                    (who->player->state.ac + who->player->state.to_a));
                context.ddesc = ddesc;
                context.obvious = obvious;
                context.visible = visible;
                context.dead = dead;
                context.do_blind = do_blind;
                context.do_para = do_para;
                context.do_conf = do_conf;
                context.do_fear = do_fear;
                strnfmt(context.flav, sizeof(context.flav), "was %s by %s", flav, ddesc);
                context.blinked = blinked;
                context.damage = damage;
                context.note_dies = note_dies;
                context.style = (who->monster? TYPE_MVM: TYPE_MVP);

                effect_handler(&context);

                /* Save any changes made in the handler for later use. */
                obvious = context.obvious;
                if (who->monster)
                    dead = context.dead;
                else
                    dead = who->player->is_dead;
                do_blind = context.do_blind;
                do_para = context.do_para;
                do_conf = context.do_conf;
                do_fear = context.do_fear;
                blinked = context.blinked;
                damage = context.damage;
            }
            else
                plog_fmt("Effect handler not found for %s.", effect->name);

            /* Handle effects (only if not dead) */
            if (!dead)
            {
                /* Hack -- only one of cut or stun */
                if (do_cut && do_stun)
                {
                    /* Cancel cut */
                    if (magik(50))
                        do_cut = 0;

                    /* Cancel stun */
                    else
                        do_stun = 0;
                }

                /* Handle cut */
                if (do_cut)
                {
                    if (who->monster)
                    {
                        mon_inc_timed(who->player, who->monster, MON_TMD_CUT, 5 + randint1(5),
                            MON_TMD_FLG_NOTIFY);
                    }
                    else
                    {
                        do_cut = get_cut(dice, damage);
                        if (do_cut) player_inc_timed(who->player, TMD_CUT, do_cut, true, true);
                    }
                }

                /* Handle stun */
                if (do_stun)
                {
                    if (who->monster)
                    {
                        mon_inc_timed(who->player, who->monster, MON_TMD_STUN, 5 + randint1(5),
                            MON_TMD_FLG_NOTIFY);
                    }
                    else
                    {
                        do_stun = get_stun(dice, damage);

                        /* Apply the stun */
                        if (do_stun) player_inc_timed(who->player, TMD_STUN, do_stun, true, true);
                    }
                }

                /* Apply fear */
                if (do_fear)
                {
                    mon_inc_timed(who->player, who->monster, MON_TMD_FEAR, 10 + randint1(10),
                        MON_TMD_FLG_NOTIFY);
                }

                /* Apply confusion */
                if (do_conf)
                {
                    mon_inc_timed(who->player, who->monster, MON_TMD_CONF, 5 + randint1(5),
                        MON_TMD_FLG_NOTIFY);
                }

                /* Apply blindness */
                if (do_blind)
                {
                    mon_inc_timed(who->player, who->monster, MON_TMD_BLIND, 5 + randint1(5),
                        MON_TMD_FLG_NOTIFY);
                }

                /* Handle paralysis */
                if (do_para)
                {
                    mon_inc_timed(who->player, who->monster, MON_TMD_HOLD, 3 + randint1(5),
                        MON_TMD_FLG_NOTIFY);
                }
            }
        }

        /* Visible monster missed target, so notify if appropriate. */
        else if (visible && method->miss)
        {
            /* Disturbing */
            disturb(who->player, 1);

            /* Message */
            msgt(who->player, MSG_MISS, "%s misses %s.", m_name, target_m_name);
        }

        /* Analyze "visible" monsters only */
        if (visible)
        {
            /* Count "obvious" attacks (and ones that cause damage) */
            if (obvious || damage || (lore->blows[ap_cnt] > 10))
            {
                /* Count attacks of this type */
                if (lore->blows[ap_cnt] < UCHAR_MAX)
                    lore->blows[ap_cnt]++;
            }
        }

        /* Handle freezing aura */
        if (who->player->timed[TMD_ICY_AURA] && damage && !who->monster)
        {
            if (magik(50))
                 fire_ball(who->player, PROJ_ICE, 0, 1, 1, false);
            else
                 fire_ball(who->player, PROJ_COLD, 0, 1 + who->player->lev / 5, 1, false);

            /* Stop if monster is dead */
            if (mon->hp < 0) break;
        }

        /* Skip the other blows if the player has moved */
        if (dead || !loc_eq(&grid, &who->player->grid)) break;
    }

    /* Blink away */
    if (blinked == 2)
    {
        char dice[5];
        struct loc grid;
        struct source origin_body;
        struct source *origin = &origin_body;

        loc_copy(&grid, &mon->grid);

        source_player(origin, get_player_index(get_connection(who->player->conn)), who->player);
        origin->monster = mon;

        strnfmt(dice, sizeof(dice), "%d", z_info->max_sight * 2 + 5);
        effect_simple(EF_TELEPORT, origin, dice, 0, 0, 0, 0, 0, NULL);
        if (!loc_eq(&grid, &mon->grid))
            msg(who->player, "There is a puff of smoke!");
    }
    else if (blinked == 1)
    {
        struct loc grid;
        struct source origin_body;
        struct source *origin = &origin_body;

        loc_copy(&grid, &mon->grid);

        source_player(origin, get_player_index(get_connection(who->player->conn)), who->player);
        origin->monster = mon;

        effect_simple(EF_TELEPORT, origin, "10", 0, 0, 0, 0, 0, NULL);
        if (!loc_eq(&grid, &mon->grid))
            msg(who->player, "%s blinks away.", m_name);
    }

    /* Always notice cause of death */
    if (who->player->is_dead && (lore->pdeaths < SHRT_MAX))
        lore->pdeaths++;
    if (who->player->is_dead && (mon->race->lore.tdeaths < SHRT_MAX))
        mon->race->lore.tdeaths++;

    /* Learn lore */
    lore_update(mon->race, lore);

    /* Assume we attacked */
    return true;
}


int get_cut(random_value dice, int d_dam)
{
    /* Critical hit (zero if non-critical) */
    int amt, tmp = monster_critical(dice, d_dam);

    /* Roll for damage */
    switch (tmp)
    {
        case 0: amt = 0; break;
        case 1: amt = randint1(5); break;
        case 2: amt = randint1(5) + 5; break;
        case 3: amt = randint1(20) + 20; break;
        case 4: amt = randint1(50) + 50; break;
        case 5: amt = randint1(100) + 100; break;
        case 6: amt = 300; break;
        default: amt = 500; break;
    }

    return amt;
}


int get_stun(random_value dice, int d_dam)
{
    /* Critical hit (zero if non-critical) */
    int amt, tmp = monster_critical(dice, d_dam);

    /* Roll for damage */
    switch (tmp)
    {
        case 0: amt = 0; break;
        case 1: amt = randint1(5); break;
        case 2: amt = randint1(10) + 10; break;
        case 3: amt = randint1(20) + 20; break;
        case 4: amt = randint1(30) + 30; break;
        case 5: amt = randint1(40) + 40; break;
        case 6: amt = 100; break;
        default: amt = 200; break;
    }

    return amt;
}
