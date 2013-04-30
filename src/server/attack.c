/*
 * File: attack.c
 * Purpose: Attacking (both throwing and melee) code
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2012 MAngband and PWMAngband Developers
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
#include "../common/tvalsval.h"
#include "attack.h"
#include "cmds.h"
#include "monster/mon-make.h"
#include "monster/mon-msg.h"
#include "monster/mon-timed.h"
#include "monster/mon-util.h"
#include "netserver.h"
#include "object/inventory.h"
#include "object/slays.h"
#include "s-spells.h"
#include "target.h"


/*
 * Returns percent chance of an object breaking after throwing or shooting.
 *
 * Artifacts will never break.
 *
 * Beyond that, each item kind has a percent chance to break (0-100). When the
 * object hits its target this chance is used.
 *
 * When an object misses it also has a chance to break. This is determined by
 * squaring the normaly breakage probability. So an item that breaks 100% of
 * the time on hit will also break 100% of the time on a miss, whereas a 50%
 * hit-breakage chance gives a 25% miss-breakage chance, and a 10% hit breakage
 * chance gives a 1% miss-breakage chance.
 */
int breakage_chance(const object_type *o_ptr, bool hit_target)
{
    int perc = o_ptr->kind->base->break_perc;

    if (o_ptr->artifact) return 0;
    if (!hit_target) return (perc * perc) / 100;

    return perc;
}


/*
 * Determine if the player "hits" a monster.
 */
bool test_hit(int chance, int ac, int vis)
{
    int k = randint0(100);

    /* There is an automatic 12% chance to hit and 5% chance to miss */
    if (k < 17) return (k < 12);

    /* Penalize invisible targets */
    if (!vis) chance = chance / 2;

    /* Starting a bit higher up on the scale */
    if (chance < 9) chance = 9;

    /* Power competes against armor */
    return (randint0(chance) >= (ac * 2 / 3));
}


/*
 * Determine damage for critical hits from shooting.
 *
 * Factor in item weight, total plusses, and player level.
 */
static int critical_shot(int Ind, int weight, int plus, int dam, u32b *msg_type)
{
    player_type *p_ptr = player_get(Ind);
    int chance = weight + (p_ptr->state.to_h + plus) * 4 + p_ptr->lev * 2;
    int power = weight + randint1(500);

    if (randint1(5000) > chance)
    {
        *msg_type = MSG_SHOOT_HIT;
        return dam;
    }
    if (power < 500)
    {
        *msg_type = MSG_HIT_GOOD;
        return 2 * dam + 5;
    }
    if (power < 1000)
    {
        *msg_type = MSG_HIT_GREAT;
        return 2 * dam + 10;
    }
    *msg_type = MSG_HIT_SUPERB;
    return 3 * dam + 15;
}


/*
 * Determine damage for critical hits from melee.
 *
 * Factor in weapon weight, total plusses, player level.
 */
static int critical_norm(int Ind, int weight, int plus, int dam, u32b *msg_type)
{
    player_type *p_ptr = player_get(Ind);
    int chance = weight + (p_ptr->state.to_h + plus) * 5 + p_ptr->lev * 3;
    int power = weight + randint1(650);

    /* Apply Touch of Death */
    if (p_ptr->timed[TMD_DEADLY] && magik(25))
    {
        *msg_type = MSG_HIT_HI_CRITICAL;
        return 4 * dam + 30;
    }

    if (randint1(5000) > chance)
    {
        *msg_type = MSG_HIT;
        return dam;
    }
    if (power < 400)
    {
        *msg_type = MSG_HIT_GOOD;
        return dam * 2 + 5;
    }
    if (power < 700)
    {
        *msg_type = MSG_HIT_GREAT;
        return dam * 2 + 10;
    }
    if (power < 900)
    {
        *msg_type = MSG_HIT_SUPERB;
        return dam * 3 + 15;
    }
    if (power < 1300)
    {
        *msg_type = MSG_HIT_HI_GREAT;
        return dam * 3 + 20;
    }
    *msg_type = MSG_HIT_HI_SUPERB;
    return 4 * dam + 20;
}


struct delayed_effects
{
    bool fear;
    bool poison;
    bool cut;
    bool stun;
    bool slow;
    bool conf;
    bool blind;
    bool stab_sleep;
    bool stab_flee;
};


/*
 * Attack the monster at the given location with a single blow.
 */
static bool py_attack_real(int Ind, int y, int x, struct delayed_effects *effects)
{
    /* Information about the attacker */
    player_type *p_ptr = player_get(Ind);
    char killer_name[NORMAL_WID];

    /* Information about the target of the attack */
    int m_idx = cave_get(p_ptr->depth)->m_idx[y][x];
    player_type *q_ptr = player_get(0 - m_idx);
    monster_type *m_ptr = NULL;
    monster_race *r_ptr = NULL;
    char m_name[NORMAL_WID], target_name[NORMAL_WID];
    bool stop = FALSE;
    bool visible;
    int ac;

    /* The weapon used */
    object_type *o_ptr = &p_ptr->inventory[INVEN_WIELD];

    /* Information about the attack */
    int bonus = p_ptr->state.to_h + o_ptr->to_h;
    int chance = p_ptr->state.skills[SKILL_TO_HIT_MELEE] + bonus * BTH_PLUS_ADJ;
    bool do_quake = FALSE;
    bool success = FALSE;
    int do_stun = 0, do_cut = 0;
    bool do_slow = FALSE, do_fear = FALSE, do_conf = FALSE, do_blind = FALSE;
    int drain = 1, d_dice = 1, d_side = 1, d_dam = 1;

    /* Default to punching for one damage */
    const char *hit_verb = "punch";
    int dmg = 1;
    u32b msg_type = MSG_HIT;
    const char *hit_extra = "";
    bool do_poison = FALSE;

    /* Information about the target of the attack */
    if (m_idx > 0)
    {
        m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
        r_ptr = &r_info[m_ptr->r_idx];
        visible = p_ptr->mon_vis[m_idx];
        ac = m_ptr->ac;
    }
    else
    {
        visible = p_ptr->play_vis[0 - m_idx];
        ac = q_ptr->state.ac + q_ptr->state.to_a;

        /* Extract killer name */
        player_desc(q_ptr, killer_name, sizeof(killer_name), p_ptr, TRUE);
    }

    /* Extract target name */
    if (m_idx > 0)
        monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, 0);
    else
    {
        player_desc(p_ptr, m_name, sizeof(m_name), q_ptr, FALSE);
        player_desc(p_ptr, target_name, sizeof(target_name), q_ptr, TRUE);
    }

    /* Auto-Recall if possible and visible */
    if ((m_idx > 0) && visible)
        monster_race_track(Ind, m_ptr->r_idx);

    /* Track a new monster */
    if (visible) health_track(p_ptr, m_idx);

    /* Handle player fear */
    if (check_state(p_ptr, OF_AFRAID))
    {
        msgt(p_ptr, MSG_AFRAID, "You are too afraid to attack %s!", m_name);
        return FALSE;
    }

    /* Disturb the target */
    if (m_idx > 0)
        mon_clear_timed(p_ptr, m_ptr, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE, FALSE);
    else
        disturb(q_ptr, 0, 0);

    /* See if the player hit */
    success = test_hit(chance, ac, visible);

    /* If a miss, skip this hit */
    if (!success)
    {
        effects->stab_sleep = FALSE;
        msgt(p_ptr, MSG_MISS, "You miss %s.", m_name);
        if (m_idx < 0) msg(q_ptr, "%s misses you.", killer_name);
        return FALSE;
    }

    /* Ghosts get damage relative to level */
    if (p_ptr->ghost && !player_can_undead(p_ptr)) dmg = p_ptr->lev;

    /* Dragons do barehanded damage: claw/bite attacks */
    if (player_has(p_ptr, PF_DRAGON))
    {
        if (magik(50))
        {
            /* Attack: claw */
            d_dice = 1 + p_ptr->lev / 16;
            d_side = 3 + p_ptr->lev / 5;
            hit_verb = "claw";
        }
        else
        {
            /* Attack: bite */
            d_dice = 3 + p_ptr->lev / 8;
            d_side = 1 + p_ptr->lev / 10;
            hit_verb = "bite";
        }
        dmg = d_dam = damroll(d_dice, d_side);
        dmg = critical_norm(Ind, p_ptr->lev * randint1(10), p_ptr->lev, dmg, &msg_type);
    }

    /* Monks do barehanded damage */
    /* Dragon monks do tail attacks 1/3 of the time */
    if (player_has(p_ptr, PF_MARTIAL_ARTS) && !(player_has(p_ptr, PF_DRAGON) && magik(66)))
    {
        int times;
        martial_arts *ma_ptr = &ma_blows[0], *old_ptr = &ma_blows[0];
        bool male = FALSE;
        int hp = ((m_idx > 0)? m_ptr->hp: q_ptr->chp);

        /* Male target */
        if (m_idx > 0)
            male = rf_has(r_ptr->flags, RF_MALE);
        else
            male = (q_ptr->psex == SEX_MALE);

        /* Get an attack */
        for (times = 0; times < 1 + p_ptr->lev / 7; times++)
        {
            do
            {
                ma_ptr = &ma_blows[randint0(MAX_MA)];
            }
            while ((ma_ptr->min_level > p_ptr->lev) || CHANCE(ma_ptr->chance, p_ptr->lev));

            /* Keep the highest level attack available we found */
            if ((ma_ptr->min_level > old_ptr->min_level) &&
                !(p_ptr->timed[TMD_STUN] || p_ptr->timed[TMD_CONFUSED]))
            {
                old_ptr = ma_ptr;
            }
            else
                ma_ptr = old_ptr;
        }

        hit_verb = ma_ptr->hit_verb;
        hit_extra = ma_ptr->hit_extra;

        /* Compute the damage */
        d_dice = ma_ptr->dd;
        d_side = ma_ptr->ds;
        dmg = d_dam = damroll(d_dice, d_side);
        dmg = critical_norm(Ind, p_ptr->lev * randint1(10), ma_ptr->min_level, dmg, &msg_type);

        /* Special effect: knee attack */
        if (ma_ptr->effect == MA_KNEE)
        {
            /* Stuns male targets */
            if (male)
            {
                hit_verb = "hit";
                hit_extra = " in the groin with your knee";
                do_stun = 1;
            }
        }

        /* Special effect: slowing attack */
        else if (ma_ptr->effect == MA_SLOW)
        {
            /* Slows some targets */
            if ((m_idx > 0) && !rf_has(r_ptr->flags, RF_NEVER_MOVE) &&
                !rf_has(r_ptr->flags, RF_UNIQUE) &&
                (is_humanoid(r_ptr) || rf_has(r_ptr->flags, RF_HAS_LEGS)) &&
                !CHANCE(m_ptr->level - 10, (dmg < 11)? 1: (dmg - 10)))
            {
                hit_verb = "kick";
                hit_extra = " in the ankle";
                do_slow = TRUE;
            }
        }

        /* Special effect: stunning attack */
        else if (ma_ptr->effect)
            do_stun = 1;

        /* Dragon monks do tail attacks */
        if (player_has(p_ptr, PF_DRAGON))
        {
            hit_verb = "hit";
            hit_extra = " with your tail";
        }
    }

    /* Handle normal weapon */
    if (o_ptr->kind)
    {
        int i;
        const struct slay *best_s_ptr = NULL;

        hit_verb = "hit";

        /* Handle polymorphed players */
        improve_attack_modifier(p_ptr, NULL, m_idx, FALSE, 0, &best_s_ptr, FALSE);

        /*
         * Get the best attack from all slays or
         * brands on all non-launcher equipment
         */
        for (i = INVEN_LEFT; i < INVEN_TOTAL; i++)
        {
            struct object *obj = &p_ptr->inventory[i];

            if (obj->kind)
                improve_attack_modifier(p_ptr, obj, m_idx, FALSE, 0, &best_s_ptr, FALSE);
        }

        /* Handle the weapon itself */
        improve_attack_modifier(p_ptr, o_ptr, m_idx, FALSE, 0, &best_s_ptr, FALSE);

        /* Set the hit verb appropriately */
        if (best_s_ptr != NULL) hit_verb = best_s_ptr->melee_verb;

        /* Hack -- poison */
        if ((best_s_ptr != NULL) && (best_s_ptr->object_flag == OF_BRAND_POIS))
            do_poison = TRUE;

        d_dice = o_ptr->dd;
        d_side = o_ptr->ds;

        dmg = drain = d_dam = damroll(d_dice, d_side);
        dmg *= ((best_s_ptr == NULL)? 1: best_s_ptr->mult);

        if (m_idx > 0)
        {
            /* Stabbing attacks */
            if (effects->stab_sleep) dmg *= (3 + p_ptr->lev / 40);
            if (effects->stab_flee) dmg = dmg * 3 / 2;
        }

        dmg += o_ptr->to_d;
        dmg = critical_norm(Ind, o_ptr->weight, o_ptr->to_h, dmg, &msg_type);

        /* Learn by use for the weapon */
        object_notice_attack_plusses(Ind, o_ptr);

        if (check_state(p_ptr, OF_IMPACT) && (dmg > 50))
        {
            do_quake = TRUE;
            wieldeds_notice_flag(p_ptr, OF_IMPACT);
        }
    }

    /* Learn by use for other equipped items */
    wieldeds_notice_on_attack(Ind);

    /* Apply the player damage bonuses */
    dmg += p_ptr->state.to_d;

    /* No negative damage */
    if (dmg <= 0) dmg = 0;

    /* Special messages */
    if (m_idx > 0)
    {
        /* Stabbing attacks */
        if (effects->stab_sleep) hit_verb = "cruelly stab";
        if (effects->stab_flee) hit_verb = "backstab";
    }
    else
    {
        /* Tell the target what happened */
        if (!dmg)
            msg(q_ptr, "%s fails to harm you.", killer_name);
        else
            msg(q_ptr, "%s hits you.", killer_name);
    }

    /* Tell the player what happened */
    if (!dmg)
        msgt(p_ptr, MSG_MISS, "You fail to harm %s.", m_name);
    else if (msg_type == MSG_HIT)
        msgt(p_ptr, MSG_HIT, "You %s %s%s.", hit_verb, m_name, hit_extra);
    else
    {
        const char *msg_hit = NULL;

        switch (msg_type)
        {
            case MSG_HIT_GOOD: msg_hit = "It was a good hit!"; break;
            case MSG_HIT_GREAT: msg_hit = "It was a great hit!"; break;
            case MSG_HIT_SUPERB: msg_hit = "It was a superb hit!"; break;
            case MSG_HIT_HI_GREAT: msg_hit = "It was a *GREAT* hit!"; break;
            case MSG_HIT_HI_SUPERB: msg_hit = "It was a *SUPERB* hit!"; break;
            case MSG_HIT_HI_CRITICAL: msg_hit = "It was a *CRITICAL* hit!"; break;
        }

        if (msg_hit)
            msgt(p_ptr, msg_type, "You %s %s%s. %s", hit_verb, m_name, hit_extra, msg_hit);
    }

    effects->stab_sleep = FALSE;

    /* Apply poison */
    if (do_poison)
    {
        int dur = randint1(p_ptr->lev) + 5;

        if (m_idx > 0)
        {
            if (mon_inc_timed(p_ptr, m_ptr, MON_TMD_POIS, dur, MON_TMD_FLG_NOMESSAGE, FALSE))
                effects->poison = TRUE;
        }
        else
            player_inc_timed(q_ptr, TMD_POISONED, dur, TRUE, TRUE);
    }

    /* Apply Shadow Touch */
    if (p_ptr->timed[TMD_TOUCH] && (m_idx > 0) && !monster_is_nonliving(r_ptr))
    {
        if (drain > m_ptr->hp) drain = m_ptr->hp;
        hp_player_safe(p_ptr, 1 + drain / 2);
    }

    /* Confusion attack */
    if (p_ptr->confusing)
    {
        p_ptr->confusing = FALSE;
        msg(p_ptr, "Your hands stop glowing.");
        do_conf = TRUE;
    }

    /* Handle polymorphed players */
    if (p_ptr->r_idx && o_ptr->kind)
    {
        monster_race *rp_ptr = &r_info[p_ptr->r_idx];
        int m = randint0(MONSTER_BLOW_MAX);

        /* Extract the attack infomation */
        int effect = rp_ptr->blow[m].effect;
        int method = rp_ptr->blow[m].method;

        /* There must be an attack */
        if (method)
        {
            /* Describe the attack method */
            switch (method)
            {
                case RBM_HIT:
                    do_cut = do_stun = 1;
                    break;
                case RBM_PUNCH:
                case RBM_KICK:
                case RBM_BUTT:
                case RBM_CRUSH:
                    do_stun = 1;
                    break;
                case RBM_CLAW:
                case RBM_BITE:
                    do_cut = 1;
                    break;
            }

            /* Apply appropriate damage */
            switch (effect)
            {
                case RBE_UN_BONUS:
                {
                    if (m_idx > 0) break;

                    /* PvP: un-bonus attack */
                    wieldeds_notice_flag(q_ptr, OF_RES_DISEN);
                    if (check_state(q_ptr, OF_RES_DISEN))
                        msg(p_ptr, "%s is unaffected.", target_name);
                    else
                        apply_disenchant(q_ptr, 0);

                    break;
                }

                case RBE_UN_POWER:
                {
                    /* PvP: un-power attack */
                    if (m_idx < 0)
                    {
                        bool dummy;

                        un_power(q_ptr, 0 - Ind, &dummy);
                    }

                    break;
                }

                case RBE_EAT_GOLD:
                {
                    if (m_idx > 0) break;

                    /* PvP: eat-gold attack */
                    if (!q_ptr->timed[TMD_PARALYZED] &&
                        magik(adj_dex_safe[q_ptr->state.stat_ind[A_DEX]] + q_ptr->lev))
                    {
                        /* Saving throw message */
                        msg(q_ptr, "You quickly protect your money pouch!");
                    }
                    else
                        eat_gold(q_ptr, 0 - Ind);

                    break;
                }

                case RBE_EAT_ITEM:
                {
                    if (m_idx > 0) break;

                    /* PvP: eat-item attack */
                    if (!q_ptr->timed[TMD_PARALYZED] &&
                        magik(adj_dex_safe[q_ptr->state.stat_ind[A_DEX]] + q_ptr->lev))
                    {
                        /* Saving throw message */
                        msg(q_ptr, "You grab hold of your backpack!");
                    }
                    else
                    {
                        bool dummy;
                        int dummy2;

                        eat_item(q_ptr, 0 - Ind, &dummy, &dummy2);
                    }

                    break;
                }

                case RBE_EAT_FOOD:
                {
                    /* PvP: eat-food attack */
                    if (m_idx < 0)
                    {
                        bool dummy;

                        eat_fud(q_ptr, Ind, &dummy);
                    }

                    break;
                }   

                case RBE_EAT_LIGHT:
                {
                    /* PvP: eat-light attack */
                    if (m_idx < 0)
                    {
                        bool dummy;

                        eat_light(q_ptr, &dummy);
                    }

                    break;
                }

                case RBE_BLIND:
                {
                    /* Blinding attack */
                    do_blind = TRUE;

                    break;
                }

                case RBE_HALLU:
                {
                    if (m_idx > 0)
                    {
                        /* Confusing attack */
                        do_conf = TRUE;

                        break;
                    }

                    /* PvP: hallucinatory attack */
                    player_inc_timed(q_ptr, TMD_IMAGE, 3 + randint1(p_ptr->lev / 2), TRUE, TRUE);

                    break;
                }

                case RBE_CONFUSE:
                {
                    /* Confusing attack */
                    do_conf = TRUE;

                    break;
                }

                case RBE_TERRIFY:
                {
                    /* Fear attack */
                    do_fear = TRUE;

                    break;
                }

                case RBE_PARALYZE:
                {
                    if (m_idx > 0)
                    {
                        /* Stunning attack */
                        do_stun = 1;

                        break;
                    }

                    /* PvP: paralyzing attack */
                    if (magik(q_ptr->state.skills[SKILL_SAVE]))
                        msg(p_ptr, "%s resists the effect.", target_name);
                    else
                        player_inc_timed(q_ptr, TMD_PARALYZED, 3 + randint1(p_ptr->lev), TRUE, TRUE);

                    break;
                }

                case RBE_LOSE_STR:
                {
                    /* PvP: lose-str attack */
                    if (m_idx < 0) do_dec_stat(q_ptr, A_STR, FALSE);

                    break;
                }

                case RBE_LOSE_INT:
                {
                    /* PvP: lose-int attack */
                    if (m_idx < 0) do_dec_stat(q_ptr, A_INT, FALSE);

                    break;
                }

                case RBE_LOSE_WIS:
                {
                    /* PvP: lose-wis attack */
                    if (m_idx < 0) do_dec_stat(q_ptr, A_WIS, FALSE);

                    break;
                }

                case RBE_LOSE_DEX:
                {
                    /* PvP: lose-dex attack */
                    if (m_idx < 0) do_dec_stat(q_ptr, A_DEX, FALSE);

                    break;
                }

                case RBE_LOSE_CON:
                case RBE_DISEASE:
                {
                    /* PvP: lose-con attack */
                    if (m_idx < 0) do_dec_stat(q_ptr, A_CON, FALSE);

                    break;
                }

                case RBE_LOSE_CHR:
                {
                    /* PvP: lose-chr attack */
                    if (m_idx < 0) do_dec_stat(q_ptr, A_CHR, FALSE);

                    break;
                }

                case RBE_LOSE_ALL:
                {
                    /* PvP: lose-all attack */
                    if (m_idx < 0)
                    {
                        do_dec_stat(q_ptr, A_STR, FALSE);
                        do_dec_stat(q_ptr, A_DEX, FALSE);
                        do_dec_stat(q_ptr, A_CON, FALSE);
                        do_dec_stat(q_ptr, A_INT, FALSE);
                        do_dec_stat(q_ptr, A_WIS, FALSE);
                        do_dec_stat(q_ptr, A_CHR, FALSE);
                    }

                    break;
                }

                case RBE_SHATTER:
                {
                    /* Quaking attack */
                    if (dmg > 50) do_quake = TRUE;

                    break;
                }

                case RBE_EXP_10:
                {
                    if (m_idx > 0) break;

                    /* PvP: lose-exp attack */
                    drain_xp(q_ptr, 10);
                    wieldeds_notice_flag(q_ptr, OF_HOLD_LIFE);

                    break;
                }  

                case RBE_EXP_20:
                {
                    if (m_idx > 0) break;

                    /* PvP: lose-exp attack */
                    drain_xp(q_ptr, 20);
                    wieldeds_notice_flag(q_ptr, OF_HOLD_LIFE);

                    break;
                }

                case RBE_EXP_40:
                {
                    if (m_idx > 0) break;

                    /* PvP: lose-exp attack */
                    drain_xp(q_ptr, 40);
                    wieldeds_notice_flag(q_ptr, OF_HOLD_LIFE);

                    break;
                }

                case RBE_EXP_80:
                {
                    if (m_idx > 0) break;

                    /* PvP: lose-exp attack */
                    drain_xp(q_ptr, 80);
                    wieldeds_notice_flag(q_ptr, OF_HOLD_LIFE);

                    break;
                }

                case RBE_FORGET:
                {
                    if (m_idx > 0) break;

                    /* PvP: forget attack */
                    if (magik(q_ptr->state.skills[SKILL_SAVE]))
                        msg(p_ptr, "%s is unaffected.", target_name);
                    else
                        player_inc_timed(q_ptr, TMD_AMNESIA, 4, TRUE, TRUE);

                    break;
                }

                case RBE_TIME:
                {
                    /* PvP: time attack */
                    if (m_idx < 0) do_side_effects(q_ptr, RSF_BR_TIME, 0, Ind, FALSE, TRUE);

                    break;
                } 

                case RBE_DISARM:
                {
                    /* PvP: disarming attack */
                    if (m_idx < 0) drop_weapon(q_ptr, dmg);

                    break;
                }

                case RBE_FAMINE:
                {
                    /* PvP: hunger attack */
                    if ((m_idx < 0) && !q_ptr->ghost)
                    {
                        msg(q_ptr, "You have a sudden attack of hunger!");
                        player_set_food(q_ptr, q_ptr->food / 2);
                    }

                    break;
                }
            }
        }
    }

    /* Ghosts get fear attacks */
    if (p_ptr->ghost && !player_can_undead(p_ptr)) do_fear = TRUE;

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

    /* Handle cut (Shard breathers resist) */
    if (do_cut) do_cut = get_cut(d_dice, d_side, d_dam);
    if (do_cut)
    {
        /* PvP */
        if (m_idx < 0)
            player_inc_timed(q_ptr, TMD_CUT, do_cut, TRUE, TRUE);
        else if (mon_inc_timed(p_ptr, m_ptr, MON_TMD_CUT, do_cut, MON_TMD_FLG_NOMESSAGE, FALSE))
            effects->cut = TRUE;
    }

    /* Apply stunning */
    if (do_stun) do_stun = get_stun(d_dice, d_side, d_dam);
    if (do_stun)
    {
        /* PvP: stunning attack */
        if (m_idx < 0)
            player_inc_timed(q_ptr, TMD_STUN, do_stun, TRUE, TRUE);
        else if (mon_inc_timed(p_ptr, m_ptr, MON_TMD_STUN, do_stun, MON_TMD_FLG_NOMESSAGE, FALSE))
            effects->stun = TRUE;
    }

    /* Apply slowing */
    if (do_slow)
    {
        /* PvP: slowing attack */
        if (m_idx < 0)
            player_inc_timed(q_ptr, TMD_SLOW, randint0(4) + 4, TRUE, TRUE);
        else if (dmg && mon_inc_timed(p_ptr, m_ptr, MON_TMD_SLOW, dmg, MON_TMD_FLG_NOMESSAGE, FALSE))
            effects->slow = TRUE;
    }

    /* Apply fear */
    if (do_fear)
    {
        int dur = 3 + randint1(p_ptr->lev);

        /* PvP: fear attack */
        if (m_idx < 0)
        {
            /* Player is terrified */
            if (magik(q_ptr->state.skills[SKILL_SAVE]))
                msg(p_ptr, "%s is unaffected.", target_name);
            else
                player_inc_timed(q_ptr, TMD_AFRAID, dur, TRUE, TRUE);
        }
        else if (mon_inc_timed(p_ptr, m_ptr, MON_TMD_FEAR, dur, MON_TMD_FLG_NOMESSAGE, FALSE))
            effects->fear = TRUE;
    }

    /* Apply confusion */
    if (do_conf)
    {
        int dur = 10 + randint0(p_ptr->lev) / 10;

        /* PvP: confusing attack */
        if (m_idx < 0)
        {
            /* Player is confused */
            player_inc_timed(q_ptr, TMD_CONFUSED, dur, TRUE, TRUE);
        }
        else if (mon_inc_timed(p_ptr, m_ptr, MON_TMD_CONF, dur, MON_TMD_FLG_NOMESSAGE, FALSE))
            effects->conf = TRUE;
    }

    /* Apply blindness */
    if (do_blind)
    {
        int dur = 10 + randint1(p_ptr->lev);

        /* PvP: blinding attack */
        if (m_idx < 0)
        {
            /* Player is blinded */
            player_inc_timed(q_ptr, TMD_BLIND, dur, TRUE, TRUE);
        }
        else if (mon_inc_timed(p_ptr, m_ptr, MON_TMD_BLIND, dur, MON_TMD_FLG_NOMESSAGE, FALSE))
            effects->blind = TRUE;
    }

    /* Damage, check for fear and death */
    if (m_idx > 0)
        stop = mon_take_hit(Ind, m_ptr, dmg, &effects->fear, NULL);
    else
    {
        strnfmt(q_ptr->died_flavor, sizeof(q_ptr->died_flavor), "was brutally murdered by %s",
            p_ptr->name);
        stop = take_hit(q_ptr, dmg, p_ptr->name, FALSE);

        /* Handle freezing aura */
        if (!stop && q_ptr->timed[TMD_ICY_AURA] && dmg)
        {
            if (magik(50))
                fire_ball(q_ptr, GF_ICE, 0, 1, 1);
            else
                fire_ball(q_ptr, GF_COLD, 0, 1 + q_ptr->lev / 5, 1);

            /* Stop if player is dead */
            if (p_ptr->is_dead) stop = TRUE;

            /* Stop if player is stunned */
            if (p_ptr->timed[TMD_STUN]) stop = TRUE;
        }
    }

    if (stop) WIPE(effects, struct delayed_effects);

    /* Apply earthquake brand */
    if (do_quake)
    {
        earthquake(p_ptr, p_ptr->depth, p_ptr->py, p_ptr->px, 10);

        /* Get the monster index again (it may have been killed or pushed away) */
        m_idx = cave_get(p_ptr->depth)->m_idx[y][x];

        if (!m_idx) stop = TRUE;
    }

    return stop;
}


/*
 * Attack the monster at the given location
 *
 * We get blows until energy drops below that required for another blow, or
 * until the target monster dies. Each blow is handled by py_attack_real().
 * We don't allow @ to spend more than 100 energy in one go, to avoid slower
 * monsters getting double moves.
 */
void py_attack(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);
    int num_blows;
    bool stop = FALSE;
    int blows = 0;
    struct delayed_effects effects;
    int m_idx = cave_get(p_ptr->depth)->m_idx[y][x];
    bool visible = ((m_idx > 0) && p_ptr->mon_vis[m_idx]);

    WIPE(&effects, struct delayed_effects);

    /* Handle polymorphed players */
    if (p_ptr->r_idx && rf_has(r_info[p_ptr->r_idx].flags, RF_NEVER_BLOW)) return;

    /* Unaware players attacking something reveal themselves */
    if (p_ptr->k_idx) aware_player(p_ptr, p_ptr);

    /* Rogues get stabbing attacks against sleeping and fleeing (visible) monsters */
    if (visible && player_has(p_ptr, PF_BACK_STAB))
    {
        monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);

        if (m_ptr->m_timed[MON_TMD_SLEEP]) effects.stab_sleep = TRUE;
        else if (m_ptr->m_timed[MON_TMD_FEAR]) effects.stab_flee = TRUE;
    }

    /* Disturb the player */
    disturb(p_ptr, 0, 0);

    /* Calculate number of blows */
    num_blows = (p_ptr->state.num_blows + p_ptr->state.frac_blow) / 100;

    /* Calculate remainder */
    p_ptr->state.frac_blow += (p_ptr->state.num_blows - num_blows * 100);

    /* Take blows until energy runs out or monster dies */
    while ((blows < num_blows) && !stop)
    {
        stop = py_attack_real(Ind, y, x, &effects);
        blows++;
    }

    /* Hack -- Delay messages */
    if (visible)
    {
        char m_name[NORMAL_WID];
        monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);

        monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, 0);
        if (effects.fear)
            add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_FLEE_IN_TERROR, TRUE);
        if (effects.poison)
            add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_POISONED, TRUE);
        if (effects.cut) add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_BLEED, TRUE);
        if (effects.stun) add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_DAZED, TRUE);
        if (effects.slow) add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_SLOWED, TRUE);
        if (effects.conf) add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_CONFUSED, TRUE);
        if (effects.blind) add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_BLIND, TRUE);
    }

    /* Carry over the remaining energy to the next turn */
    p_ptr->state.frac_blow += (num_blows - blows) * 100;

    /* Hack -- Limit to ONE turn */
    if (p_ptr->state.frac_blow > p_ptr->state.num_blows)
        p_ptr->state.frac_blow = p_ptr->state.num_blows;
}


void un_power(struct player *p, int m_idx, bool* obvious)
{
    int unpower = 0, newcharge;
    int k, i, rlev;
    object_type *o_ptr;
    monster_type *m_ptr = NULL;
    player_type *q_ptr = NULL;

    /* Get target */
    if (m_idx > 0)
    {
        m_ptr = cave_monster(cave_get(p->depth), m_idx);
        rlev = ((m_ptr->level >= 1) ? m_ptr->level : 1);
    }
    else
    {
        q_ptr = player_get(0 - m_idx);
        rlev = q_ptr->lev;
    }

    /* Find an item */
    for (k = 0; k < 10; k++)
    {
        /* Pick an item */
        i = randint0(INVEN_PACK);

        /* Obtain the item */
        o_ptr = &p->inventory[i];

        /* Skip non-objects */
        if (!o_ptr->kind) continue;

        /* Drain charged wands/staves */
        if ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND))
        {
            /* Charged? */
            if (o_ptr->pval[DEFAULT_PVAL])
            {
                /* Get number of charge to drain */
                unpower = (rlev / (o_ptr->kind->level + 2)) + 1;

                /* Get new charge value, don't allow negative */
                newcharge = MAX((o_ptr->pval[DEFAULT_PVAL] - unpower), 0);

                /* Remove the charges */
                o_ptr->pval[DEFAULT_PVAL] = newcharge;
            }
        }

        if (unpower)
        {
            int heal = rlev * unpower;

            /* Message */
            msg(p, "Energy drains from your pack!");

            /* Obvious */
            *obvious = TRUE;

            /* Heal */
            if (m_ptr)
            {
                /* Don't heal more than max hp */
                heal = MIN(heal, m_ptr->maxhp - m_ptr->hp);

                /* Heal */
                m_ptr->hp += heal;

                /* Redraw (later) if needed */
                update_health(m_idx);
            }
            else if (q_ptr)
                hp_player_safe(q_ptr, heal);

            /* Combine / Reorder the pack */
            p->notice |= (PN_COMBINE | PN_REORDER);

            /* Redraw stuff */
            p->redraw |= (PR_INVEN);

            /* Affect only a single inventory slot */
            break;
        }
    }
}


void eat_gold(struct player *p, int m_idx)
{
    s32b gold;

    gold = (p->au / 10) + randint1(25);
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
        msg(p, "%ld coins were stolen!", (long)gold);
    else
        msg(p, "All of your coins were stolen!");

    /* Redraw gold */
    p->redraw |= (PR_GOLD);

    /* Give the gold to the monster */
    if (m_idx > 0)
    {
        object_type o;

        /* Create a new temporary object */
        object_prep(&o, lookup_kind(TV_GOLD, SV_GOLD), 0, MINIMISE);

        /* Amount of gold to put in this object */
        o.pval[DEFAULT_PVAL] = gold;

        /* Set origin to stolen, so it is not confused with dropped treasure in monster_death */
        o.origin = ORIGIN_STOLEN;

        /* Give the gold to the monster */
        monster_carry(cave_monster(cave_get(p->depth), m_idx), &o, FALSE);
    }

    /* PW: give the gold to the offending player in PvP! */
    else if (m_idx < 0)
    {
        player_type *q_ptr = player_get(0 - m_idx);

        q_ptr->au += gold;
        msg(q_ptr, "Your purse feels heavier.");
        q_ptr->redraw |= (PR_GOLD);
    }
}


void eat_item(struct player *p, int m_idx, bool* obvious, int* blinked)
{
    int k, i;
    object_type *o_ptr;
    char o_name[NORMAL_WID];
    player_type *q_ptr = player_get(0 - m_idx);

    /* Find an item */
    for (k = 0; k < 10; k++)
    {
        object_type object_type_body;
        object_type *i_ptr = &object_type_body;

        /* Pick an item */
        i = randint0(INVEN_PACK);

        /* Obtain the item */
        o_ptr = &p->inventory[i];

        /* Accept real items */
        if (!o_ptr->kind) continue;

        /* Don't steal artifacts */
        if (o_ptr->artifact) continue;

        /* Get a copy with the right "amt" */
        object_distribute_amt(i_ptr, o_ptr, 1);

        /* PvP: can only steal items if they can be carried */
        if (m_idx < 0)
        {
            /* Note that the pack is too full */
            if (!inven_carry_okay(q_ptr, i_ptr)) continue;

            /* Note that the pack is too heavy */
            if (!weight_okay(q_ptr, i_ptr)) continue;
        }

        /* Get a description */
        object_desc(p, o_name, sizeof(o_name), i_ptr, ODESC_FULL);

        /* Message */
        msg(p, "%sour %s (%c) was stolen!", ((o_ptr->number > 1) ? "One of y" : "Y"), o_name,
            index_to_label(i));

        /* Carry the object */
        if (m_idx > 0)
            monster_carry(cave_monster(cave_get(p->depth), m_idx), i_ptr, FALSE);
        else if (m_idx < 0)
            inven_carry(q_ptr, i_ptr, TRUE);

        /* Steal the items */
        inven_item_increase(p, i, -1);
        inven_item_optimize(p, i);

        /* Obvious */
        *obvious = TRUE;

        /* Blink away */
        *blinked = 2;

        /* Done */
        break;
    }
}


void eat_fud(struct player *p, int p_idx, bool* obvious)
{
    int k, i;
    object_type *o_ptr;
    char o_name[NORMAL_WID];

    /* Steal some food */
    for (k = 0; k < 10; k++)
    {
        /* Pick an item from the pack */
        i = randint0(INVEN_PACK);

        /* Get the item */
        o_ptr = &p->inventory[i];

        /* Accept real items */
        if (!o_ptr->kind) continue;

        /* Only eat food */
        if (!is_food(o_ptr)) continue;

        /* Get a description */
        object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_BASE);

        /* Message */
        msg(p, "%sour %s (%c) was eaten!", ((o_ptr->number > 1) ? "One of y" : "Y"), o_name,
            index_to_label(i));

        /* PW: feed the offending player in PvP! */
        if (p_idx > 0)
        {
            player_type *p_ptr = player_get(p_idx);
            bool ident = FALSE, used = FALSE;

            sound(p_ptr, MSG_EAT);
            p_ptr->was_aware = object_flavor_is_aware(p_ptr, o_ptr);
            used = use_object(p_idx, o_ptr, &ident, 0);
            object_aware_tried(p_idx, o_ptr, ident, used);
        }

        /* Steal the items */
        inven_item_increase(p, i, -1);
        inven_item_optimize(p, i);

        /* Obvious */
        *obvious = TRUE;

        /* Done */
        break;
    }
}


void eat_light(struct player *p, bool* obvious)
{
    object_type *o_ptr;
    bitflag f[OF_SIZE];

    /* Access the light */
    o_ptr = &p->inventory[INVEN_LIGHT];
    object_flags(o_ptr, f);

    /* Drain fuel where applicable */
    if (!of_has(f, OF_NO_FUEL) && (o_ptr->timeout > 0))
    {
        /* Reduce fuel */
        o_ptr->timeout -= (250 + randint1(250));
        if (o_ptr->timeout < 1) o_ptr->timeout = 1;

        /* Notice */
        if (!p->timed[TMD_BLIND])
        {
            msg(p, "Your light dims.");
            *obvious = TRUE;
        }

        /* Redraw */
        p->redraw |= (PR_EQUIP);
    }
}


void drain_xp(struct player *p, int amt)
{
    int chance = 100 - (amt / 2) - (amt / 40) * 5;

    if (check_state(p, OF_HOLD_LIFE) && magik(chance))
        msg(p, "You keep hold of your life force!");
    else
    {
        s32b d = damroll(amt, 6) + (p->exp / 100) * MON_DRAIN_LIFE;
        if (check_state(p, OF_HOLD_LIFE))
        {
            msg(p, "You feel your life slipping away!");
            player_exp_lose(p, d / 10, FALSE);
        }
        else
        {
            msg(p, "You feel your life draining away!");
            player_exp_lose(p, d, FALSE);
        }
    }
}


void drop_weapon(struct player *p, int damage)
{
    bitflag f[OF_SIZE];
    int tmp;
    object_type *o_ptr;

    /* This effect is *very* nasty - it should be very rare */
    /* So give a chance to avoid it to everyone */
    if (magik(50)) return;

    /* A high DEX will help a lot */
    tmp = adj_dex_safe[p->state.stat_ind[A_DEX]];
    if (tmp > 95) tmp = 95;
    if (magik(tmp)) return;

    /* Access the weapon */
    o_ptr = &p->inventory[INVEN_WIELD];

    /* No weapon used - bleeding effect instead */
    if (!o_ptr->kind)
    {
        player_inc_timed(p, TMD_CUT, 100, TRUE, TRUE);
        return;
    }

    /* Artifacts are safe */
    if (o_ptr->artifact && magik(90)) return;

    /* Permacursed weapons can't be removed */
    object_flags(o_ptr, f);
    if (of_has(f, OF_PERMA_CURSE)) return;

    /* Two-handed weapons are safe */
    if (of_has(f, OF_TWO_HANDED) && magik(90)) return;

    /* Give an extra chance for comfortable weapons */
    if (magik(50) && !(p->state.heavy_wield || p->state.icky_wield || p->state.cumber_shield))
        return;

    /* Finally give an extra chance for weak blows */
    if (!magik(damage)) return;

    /* Really unlucky or really lousy fighters get disarmed */
    msg(p, "You lose grip of your weapon!");
    if (!inven_drop(p, INVEN_WIELD, 1, TRUE))
    {
        /* Protect true artifacts at shallow depths */
        msg(p, "You manage to catch your weapon before it falls to the ground.");
    }
    p->update |= (PU_BONUS);
}


static byte door_color(int missile_attr)
{
    return get_color(missile_attr, ATTR_DOOR, 1);
}


static bool pvx_check(int Ind, int target, byte feat)
{
    player_type *p_ptr = player_get(Ind);

    /* Player here */
    if (target < 0)
    {
        player_type *q_ptr = player_get(0 - target);
        int mode = ((target_get_monster(Ind) == target)? PVP_DIRECT: PVP_INDIRECT);

        return pvp_check(p_ptr, q_ptr, mode, TRUE, feat);
    }

    /* Monster here */
    return pvm_check(Ind, target);
}


static void colorize_door(int Ind, object_kind *k_ptr, int depth, int y, int x)
{
    player_type *p_ptr = player_get(Ind);
    byte m_attr;
    int i, j;

    /* Find the color of the missile */
    m_attr = (k_ptr->flavor? k_ptr->flavor->x_attr: k_ptr->x_attr);

    /* Find suitable color */
    for (i = FEAT_HOME_HEAD; i <= FEAT_HOME_TAIL; i++)
    {
        if (f_info[i].x_attr[FEAT_LIGHTING_LIT] == door_color(m_attr))
        {
            char store_name[NORMAL_WID];

            /* Pick a house */
            if ((j = pick_house(depth, y, x)) == -1) break;

            /* Must own the house */
            if (!house_owned_by(p_ptr, j)) break;

            /* Check house contents for non custom stores */
            if (!get_player_store_name(j, store_name, sizeof(store_name)))
            {
                int hy, hx;
                bool invalid = FALSE;

                /* Scan house */
                for (hy = houses[j].y_1; (hy <= houses[j].y_2 && !invalid); hy++)
                {
                    for (hx = houses[j].x_1; (hx <= houses[j].x_2 && !invalid); hx++)
                    {
                        s16b this_o_idx, next_o_idx = 0;

                        /* Objects */
                        for (this_o_idx = cave_get(houses[j].depth)->o_idx[hy][hx];
                            this_o_idx && !invalid; this_o_idx = next_o_idx)
                        {
                            object_type *ho_ptr = object_byid(this_o_idx);

                            next_o_idx = ho_ptr->next_o_idx;
                            invalid = !check_store_drop_color(p_ptr, ho_ptr, i - FEAT_HOME_HEAD);
                        }
                    }
                }
                if (invalid) break;
            }

            /* Perform colorization */
            houses[j].color = i - FEAT_HOME_HEAD;
            cave_set_feat(cave_get(depth), y, x, i);

            /* Done */
            break;
        }
    }
}


static int hostile_monster(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);
    int m_idx = cave_get(p_ptr->depth)->m_idx[y][x];

    /* Don't allow if not hostile */
    if (m_idx && pvx_check(Ind, m_idx, cave_get(p_ptr->depth)->feat[y][x])) return m_idx;

    return 0;
}


typedef struct delayed_ranged_effects
{
    int m_idx;
    int dmg;
    bool fear;
    bool poison;
    bool conf;
    struct delayed_ranged_effects *next;
} ranged_effects;


/*
 * This is a helper function to manage a linked list of delayed range effects.
 */
static ranged_effects *get_delayed_ranged_effects(ranged_effects **effects, int m_idx)
{
    ranged_effects *current = *effects;

    /* Walk through the list to get the corresponding monster */
    while (current)
    {
        /* Found a match */
        if (current->m_idx == m_idx) return current;

        current = current->next;
    }

    /* No match: create, assign and return */
    current = mem_zalloc(sizeof(ranged_effects));
    current->m_idx = m_idx;
    current->next = *effects;
    *effects = current;
    return current;
}


/*
 * Wipes a dead monster from the linked list of delayed range effects.
 */
static void wipe_delayed_ranged_effects(ranged_effects **effects, int m_idx)
{
    ranged_effects *current = *effects, *next;

    /* Empty list */
    if (!current) return;

    /* First element */
    if ((*effects)->m_idx == m_idx)
    {
        /* Wipe the dead monster */
        next = (*effects)->next;
        mem_free(*effects);
        *effects = next;
        return;
    }

    /* Walk through the list to get the corresponding monster */
    while (current)
    {
        next = current->next;

        /* End of list */
        if (!next) return;

        /* Found a match */
        if (next->m_idx == m_idx)
        {
            /* Wipe the dead monster */
            current->next = next->next;
            mem_free(next);
            return;
        }

        current = next;
    }
}


/*
 * Find the attr/char pair to use for a missile.
 *
 * It is moving (or has moved) from (x, y) to (nx, ny).
 */
static void missile_pict(int Ind, const object_type *o_ptr, int y, int x, int ny, int nx, byte *a,
    char *c)
{
    player_type *p_ptr = player_get(Ind);

    /* Get a nice missile picture for arrows and bolts */
    if (o_ptr->tval == TV_ARROW)
        bolt_pict(Ind, y, x, ny, nx, ((o_ptr->sval == SV_AMMO_NORMAL)? GF_ARROW_2: GF_ARROW_X), a, c);
    else if (o_ptr->tval == TV_BOLT)
        bolt_pict(Ind, y, x, ny, nx, ((o_ptr->sval == SV_AMMO_NORMAL)? GF_ARROW_3: GF_ARROW_4), a, c);
    else
    {
        /* Default to object picture */
        *a = object_attr(p_ptr, o_ptr);
        *c = object_char(p_ptr, o_ptr);
    }
}


/*
 * This is a helper function used by do_cmd_throw and do_cmd_fire.
 *
 * It abstracts out the projectile path, display code, identify and clean up
 * logic, while using the 'attack' parameter to do work particular to each
 * kind of attack.
 */
static void ranged_helper(int Ind, int item, int dir, int range, int shots,
    ranged_attack attack, bool magic, bool pierce, bool ranged_effect)
{
    player_type *p_ptr = player_get(Ind);

    /* Get the ammo */
    object_type *o_ptr = object_from_item_idx(p_ptr, item, 0, TRUE);

    int i, j, k;
    byte missile_attr;
    char missile_char;
    object_type object_type_body;
    object_type *i_ptr = &object_type_body;
    char o_name[NORMAL_WID];
    int path_n;
    u16b path_g[256];

    /* Start at the player */
    int x = p_ptr->px;
    int y = p_ptr->py;

    /* Predict the "target" location */
    s16b ty = y + 99 * ddy[dir];
    s16b tx = x + 99 * ddx[dir];

    bool hit_target = FALSE;
    int num = 0;
    bool dead = FALSE;
    ranged_effects *effects = NULL, *current;

    /* Check for target validity */
    if ((dir == 5) && target_okay(p_ptr))
    {
        int taim;

        target_get(p_ptr, &tx, &ty);

        /* Check distance */
        taim = distance(y, x, ty, tx);
        if (taim > range)
        {
            msg(p_ptr, "Target out of range by %d squares.", taim - range);
            return;
        }
    }

    /* Sound */
    sound(p_ptr, MSG_SHOOT);

    object_notice_on_firing(Ind, o_ptr);

    /* Describe the object */
    object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_SINGULAR);

    /* Take a turn */
    use_energy(Ind);

    /* Attack once for each legal shot */
    while (num++ < shots)
    {
        int m_idx = 0;
        int by = -1, bx = -1;

        /* Start at the player */
        y = p_ptr->py;
        x = p_ptr->px;

        /* Calculate the path */
        path_n = project_path(path_g, range, p_ptr->depth, y, x, ty, tx, (pierce? PROJECT_THRU: 0));

        /* Hack -- Handle stuff */
        handle_stuff(p_ptr);

        /* Project along the path */
        for (i = 0; i < path_n; ++i)
        {
            int ny = GRID_Y(path_g[i]);
            int nx = GRID_X(path_g[i]);

            /* Hack -- Disable throwing through open house door */
            if (cave_get(p_ptr->depth)->feat[ny][nx] == FEAT_HOME_OPEN) break;

            /* Hack -- Stop before hitting walls */
            if (!cave_floor_bold(p_ptr->depth, ny, nx))
            {
                /* Special case: potion VS house door */
                if ((o_ptr->tval == TV_POTION) && cave_ishomedoor(cave_get(p_ptr->depth), ny, nx))
                {
                    /* Break it */
                    hit_target = TRUE;

                    /* Find suitable color */
                    colorize_door(Ind, o_ptr->kind, p_ptr->depth, ny, nx);
                }

                /* Done */
                break;
            }

            /* Get missile picture */
            missile_pict(Ind, o_ptr, y, x, ny, nx, &missile_attr, &missile_char);

            /* Advance */
            x = nx;
            y = ny;

            /* Display the missile for each player */
            for (k = 1; k < NumPlayers + 1; k++)
            {
                player_type *q_ptr = player_get(k);

                /* Skip irrelevant players */
                if (q_ptr->depth != p_ptr->depth) continue;
                if (!panel_contains(q_ptr, y, x)) continue;

                /* Only do visuals if the player can "see" the missile */
                if (player_can_see_bold(q_ptr, y, x))
                {
                    /* Draw, Highlight, Fresh, Pause, Erase */
                    flush_path_grid(q_ptr, q_ptr->depth, y, x, missile_attr, missile_char);
                }
                else
                {
                    /* Delay anyway for consistency */
                    Send_flush(q_ptr, FALSE, TRUE);
                }
            }

            /* Handle monster/player */
            m_idx = hostile_monster(Ind, y, x);

            /* Try the attack on the monster/player at (x, y) if any */
            if (m_idx)
            {
                player_type *q_ptr = NULL;
                monster_type *m_ptr = NULL;
                monster_race *r_ptr = NULL;
                bool visible;
                bool fear = FALSE;
                char m_name[NORMAL_WID];
                const char *note_dies = " dies.";
                struct attack_result result = attack(Ind, o_ptr, y, x);
                int dmg = result.dmg;
                u32b msg_type = result.msg_type;
                const char *hit_verb = result.hit_verb;
                bool do_poison = result.do_poison;

                /* Target info */
                if (m_idx > 0)
                {
                    m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
                    visible = p_ptr->mon_vis[m_idx];
                    monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, 0);
                }
                else
                {
                    q_ptr = player_get(0 - m_idx);
                    visible = p_ptr->play_vis[0 - m_idx];
                    my_strcpy(m_name, q_ptr->name, sizeof(m_name));
                }

                if (result.success)
                {
                    hit_target = TRUE;

                    /* Target info */
                    if (m_idx > 0)
                    {
                        r_ptr = &r_info[m_ptr->r_idx];
                        if (monster_is_unusual(r_ptr)) note_dies = " is destroyed.";
                    }

                    object_notice_attack_plusses(Ind, o_ptr);

                    /* Learn by use for other equipped items */
                    wieldeds_notice_to_hit_on_attack(Ind);

                    /* No negative damage */
                    if (dmg <= 0)
                    {
                        dmg = 0;
                        hit_verb = "fail to harm";
                    }

                    if (!visible)
                    {
                        /* Invisible monster/player */
                        msgt(p_ptr, MSG_SHOOT_HIT, "The %s finds a mark.", o_name);
                    }
                    else
                    {
                        /* Handle visible monster/player */
                        if (msg_type == MSG_SHOOT_HIT)
                            msgt(p_ptr, MSG_SHOOT_HIT, "The %s %s %s.", o_name, hit_verb, m_name);
                        else
                        {
                            const char *msg_hit = NULL;

                            switch (msg_type)
                            {
                                case MSG_HIT_GOOD: msg_hit = "It was a good hit!";
                                case MSG_HIT_GREAT: msg_hit = "It was a great hit!";
                                case MSG_HIT_SUPERB: msg_hit = "It was a superb hit!";
                            }

                            if (msg_hit)
                            {
                                msgt(p_ptr, msg_type, "The %s %s %s. %s", o_name, hit_verb, m_name,
                                    msg_hit);
                            }
                        }

                        /* Track this target */
                        if (m_idx > 0) monster_race_track(Ind, m_ptr->r_idx);
                        health_track(p_ptr, m_idx);
                    }

                    /* Message */
                    if (m_idx < 0)
                    {
                        char killer_name[NORMAL_WID];

                        /* Killer name */
                        player_desc(q_ptr, killer_name, sizeof(killer_name), p_ptr, TRUE);

                        msg(q_ptr, "%s hits you with a %s.", killer_name, o_name);
                    }

                    /* Hit the target, check for death */
                    if (m_idx > 0)
                    {
                        /* Hit the monster, check for death */
                        dead = mon_take_hit(Ind, m_ptr, dmg, &fear, note_dies);
                    }
                    else
                    {
                        strnfmt(q_ptr->died_flavor, sizeof(q_ptr->died_flavor),
                            "was shot to death with a %s by %s", o_name, p_ptr->name);

                        /* Hit the player, check for death */
                        dead = take_hit(q_ptr, dmg, p_ptr->name, FALSE);
                    }

                    /* Message */
                    if (!dead)
                    {
                        if (m_idx > 0)
                        {
                            current = get_delayed_ranged_effects(&effects, m_idx);
                            current->dmg += dmg;
                        }
                        else
                            player_pain(Ind, 0 - m_idx, dmg);
                    }
                    else if (m_idx > 0)
                        wipe_delayed_ranged_effects(&effects, m_idx);

                    /* Apply poison */
                    if (do_poison && !dead)
                    {
                        int dur = randint1(p_ptr->lev) + 5;

                        if (m_idx < 0)
                            player_inc_timed(q_ptr, TMD_POISONED, dur, TRUE, TRUE);
                        else if (mon_inc_timed(p_ptr, m_ptr, MON_TMD_POIS, dur,
                            MON_TMD_FLG_NOMESSAGE, FALSE))
                        {
                            current = get_delayed_ranged_effects(&effects, m_idx);
                            current->poison = TRUE;
                        }
                    }

                    /* Apply archer confusion brand */
                    if (ranged_effect && has_bowbrand(p_ptr, BOW_BRAND_CONF) && !dead)
                    {
                        int dur = 3 + randint1(10 + randint0(p_ptr->lev) / 10);

                        if (m_idx < 0)
                            player_inc_timed(q_ptr, TMD_CONFUSED, dur, TRUE, TRUE);
                        else if (mon_inc_timed(p_ptr, m_ptr, MON_TMD_CONF, dur,
                            MON_TMD_FLG_NOMESSAGE, FALSE))
                        {
                            current = get_delayed_ranged_effects(&effects, m_idx);
                            current->conf = TRUE;
                        }
                    }

                    /* Add a nice ball if needed */
                    if (ranged_effect && p_ptr->timed[TMD_BOWBRAND])
                    {
                        bx = x;
                        by = y;
                    }

                    /* Take note */
                    if (!dead && fear)
                    {
                        current = get_delayed_ranged_effects(&effects, m_idx);
                        current->fear = TRUE;
                    }
                }
                else if (visible)
                {
                    /* Handle visible monster/player */
                    msgt(p_ptr, MSG_MISS, "The %s misses %s.", o_name, m_name);
                }

                /* Stop looking */
                if (!pierce) break;
            }
        }

        /* Ball effect */
        if ((by >= 0) && (bx >= 0))
        {
            int p_flag = PROJECT_JUMP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

            p_ptr->current_sound = -2;
            switch (p_ptr->bow_brand_t)
            {
                case BOW_BRAND_THUNDER:
                    project(0 - Ind, 2, p_ptr->depth, by, bx, 20, GF_ELEC, p_flag, "killed");
                    break;
                case BOW_BRAND_SHARDS:
                    project(0 - Ind, 2, p_ptr->depth, by, bx, 10, GF_SHARD, p_flag, "killed");
                    break;
                case BOW_BRAND_ICE:
                    project(0 - Ind, 2, p_ptr->depth, by, bx, 30, GF_COLD, p_flag, "killed");
                    break;
                case BOW_BRAND_FLAME:
                    project(0 - Ind, 2, p_ptr->depth, by, bx, 35, GF_FIRE, p_flag, "killed");
                    break;
                case BOW_BRAND_WATER:
                    project(0 - Ind, 2, p_ptr->depth, by, bx, 40, GF_ACID, p_flag, "killed");
                    break;
                case BOW_BRAND_SONIC:
                    project(0 - Ind, 2, p_ptr->depth, by, bx, 25, GF_SOUND, p_flag, "killed");
                    break;
            }
            p_ptr->current_sound = -1;
        }

        /* Get a copy with the right "amt" */
        object_distribute_amt(i_ptr, o_ptr, 1);

        /* Chance of breakage (during attacks) */
        j = breakage_chance(i_ptr, hit_target);

        /* Handle the newbies_cannot_drop option */
        if (newbies_cannot_drop(p_ptr)) j = 100;

        /* Drop (or break) near that location */
        if (!magic) drop_near(p_ptr, cave_get(p_ptr->depth), i_ptr, j, y, x, TRUE);

        /* Reduce and describe inventory/floor item */
        if (!magic) item_decrease(p_ptr, item, 1, TRUE);

        /* Stop if dead */
        if (dead && !pierce) break;
    }

    /* Hack -- Delay messages */
    while (effects)
    {
        char m_name[NORMAL_WID];
        monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), effects->m_idx);

        monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, 0);

        if (p_ptr->mon_vis[effects->m_idx])
        {
            if (effects->dmg)
                message_pain(Ind, m_ptr, effects->dmg);
            if (effects->poison)
                add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_POISONED, TRUE);
            if (effects->conf)
                add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_CONFUSED, TRUE);
            if (effects->fear)
                add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_FLEE_IN_TERROR, TRUE);
        }

        current = effects->next;
        mem_free(effects);
        effects = current;
    }
}


/*
 * Helper function used with ranged_helper by do_cmd_fire.
 */
static struct attack_result make_ranged_shot(int Ind, object_type *o_ptr, int y, int x)
{
    player_type *p_ptr = player_get(Ind);
    struct attack_result result = {FALSE, 0, 0, "hits", FALSE};
    object_type *j_ptr = &p_ptr->inventory[INVEN_BOW];
    int bonus = p_ptr->state.to_h + o_ptr->to_h + j_ptr->to_h;
    int chance = p_ptr->state.skills[SKILL_TO_HIT_BOW] + bonus * BTH_PLUS_ADJ;
    int chance2 = chance - distance(p_ptr->py, p_ptr->px, y, x);
    int multiplier = p_ptr->state.ammo_mult;
    const struct slay *best_s_ptr = NULL;
    bool magic = ((o_ptr->sval == SV_AMMO_MAGIC) || o_ptr->artifact);
    int m_idx = cave_get(p_ptr->depth)->m_idx[y][x];
    bool visible;
    int ac;

    /* Target info */
    if (m_idx > 0)
    {
        monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);

        visible = p_ptr->mon_vis[m_idx];
        ac = m_ptr->ac;
    }
    else
    {
        player_type *q_ptr = player_get(0 - m_idx);

        visible = p_ptr->play_vis[0 - m_idx];
        ac = q_ptr->state.ac + q_ptr->state.to_a;
    }

    /* Did we hit it (penalize distance travelled) */
    if (!test_hit(chance2, ac, visible)) return result;

    result.success = TRUE;

    improve_attack_modifier(p_ptr, NULL, m_idx, TRUE, 0, &best_s_ptr, FALSE);
    improve_attack_modifier(p_ptr, o_ptr, m_idx, TRUE, 0, &best_s_ptr, FALSE);
    if (j_ptr->kind)
        improve_attack_modifier(p_ptr, j_ptr, m_idx, TRUE, 0, &best_s_ptr, FALSE);

    /* If we have a slay, modify the multiplier appropriately */
    if (best_s_ptr != NULL)
    {
        result.hit_verb = best_s_ptr->range_verb;
        if (multiplier > 1) multiplier += best_s_ptr->mult;
        else multiplier = best_s_ptr->mult;

        /* Hack -- Poison */
        if (best_s_ptr->object_flag == OF_BRAND_POIS) result.do_poison = TRUE;
    }

    /* Apply damage: multiplier, slays, criticals, bonuses */
    result.dmg = damroll(o_ptr->dd, o_ptr->ds);
    result.dmg += o_ptr->to_d + j_ptr->to_d;
    result.dmg *= multiplier;
    if (p_ptr->timed[TMD_BOWBRAND]) result.dmg += p_ptr->bow_brand_d;
    result.dmg = critical_shot(Ind, o_ptr->weight, o_ptr->to_h, result.dmg, &result.msg_type);

    object_notice_attack_plusses(Ind, j_ptr);

    return result;
}


/*
 * Helper function used with ranged_helper by do_cmd_throw.
 */
static struct attack_result make_ranged_throw(int Ind, object_type *o_ptr, int y, int x)
{
    player_type *p_ptr = player_get(Ind);
    struct attack_result result = {FALSE, 0, 0, "hits", FALSE};
    int bonus = p_ptr->state.to_h + o_ptr->to_h;
    int chance = p_ptr->state.skills[SKILL_TO_HIT_THROW] + bonus * BTH_PLUS_ADJ;
    int chance2 = chance - distance(p_ptr->py, p_ptr->px, y, x);
    int multiplier = 1;
    const struct slay *best_s_ptr = NULL;
    int m_idx = cave_get(p_ptr->depth)->m_idx[y][x];
    bool visible;
    int ac;

    /* Target info */
    if (m_idx > 0)
    {
        monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);

        visible = p_ptr->mon_vis[m_idx];
        ac = m_ptr->ac;
    }
    else
    {
        player_type *q_ptr = player_get(0 - m_idx);

        visible = p_ptr->play_vis[0 - m_idx];
        ac = q_ptr->state.ac + q_ptr->state.to_a;
    }

    /* Did we hit it (penalize distance travelled) */
    if (!test_hit(chance2, ac, visible)) return result;

    result.success = TRUE;

    improve_attack_modifier(p_ptr, NULL, m_idx, TRUE, 0, &best_s_ptr, FALSE);
    improve_attack_modifier(p_ptr, o_ptr, m_idx, TRUE, 0, &best_s_ptr, FALSE);

    /* If we have a slay, modify the multiplier appropriately */
    if (best_s_ptr != NULL)
    {
        result.hit_verb = best_s_ptr->range_verb;
        multiplier = best_s_ptr->mult;

        /* Hack -- poison */
        if (best_s_ptr->object_flag == OF_BRAND_POIS) result.do_poison = TRUE;
    }

    /* Apply damage: multiplier, slays, criticals, bonuses */
    result.dmg = damroll(o_ptr->dd, o_ptr->ds);
    result.dmg += o_ptr->to_d;
    result.dmg *= multiplier;
    result.dmg = critical_shot(Ind, o_ptr->weight, o_ptr->to_h, result.dmg, &result.msg_type);

    return result;
}


/*
 * Fire an object from the quiver, pack or floor at a target.
 */
void do_cmd_fire(int Ind, int dir, int item)
{
    player_type *p_ptr = player_get(Ind);
    int range = MIN(6 + 2 * p_ptr->state.ammo_mult, MAX_RANGE);
    int shots = p_ptr->state.num_shots;
    ranged_attack attack = make_ranged_shot;
    object_type *o_ptr = object_from_item_idx(p_ptr, item, 0, TRUE);
    bool magic = ((o_ptr->sval == SV_AMMO_MAGIC) || o_ptr->artifact);
    bool pierce = has_bowbrand(p_ptr, BOW_BRAND_PIERCE);

    /* Restrict ghosts */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_HANDS))
    {
        msg(p_ptr, "You cannot fire missiles!");
        return;
    }

    /* Check preventive inscription '^f' */
    __trap(p_ptr, CPI(p_ptr, 'f'))

    /* Make sure the player isn't firing wielded items */
    if ((item >= INVEN_WIELD) && (item < QUIVER_START))
    {
        msg(p_ptr, "You cannot fire wielded items.");
        return;
    }

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return;

    /* Restricted by choice */
    if ((item < 0) && !is_owner(p_ptr, o_ptr))
    {
        msg(p_ptr, "This item belongs to someone else!");
        return;
    }

    /* Paranoia: requires a proper missile */
    if (o_ptr->tval != p_ptr->state.ammo_tval) return;

    /* The inscription prevents it */
    __trap(p_ptr, CGI(o_ptr, 'f', FALSE))

    /* Restrict artifacts */
    if (o_ptr->artifact && newbies_cannot_drop(p_ptr))
    {
        msg(p_ptr, "You cannot fire that!");
        return;
    }

    /* Never in wrong house */
    if (!check_store_drop(p_ptr, o_ptr))
    {
        msg(p_ptr, "You cannot fire this here.");
        return;
    }

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return;

    /* Apply confusion */
    player_confuse_dir(p_ptr, &dir);

    /* Only fire in direction 5 if we have a target */
    if ((dir == 5) && !target_okay(p_ptr)) return;

    /* Temporary "Farsight" */
    if (p_ptr->timed[TMD_FARSIGHT]) range += (p_ptr->lev - 7) / 10;

    /* Check if we have enough missiles */
    if (!magic && (shots > o_ptr->number)) shots = o_ptr->number;

    ranged_helper(Ind, item, dir, range, shots, attack, magic, pierce, TRUE);
}


/*
 * Fire at nearest target
 */
void do_cmd_fire_at_nearest(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* The direction '5' means 'use the target' */
    int i, dir = 5, item = -1;

    /* Require a usable launcher */
    if (!p_ptr->inventory[INVEN_BOW].tval && (p_ptr->state.ammo_tval != TV_ROCK))
    {
        msg(p_ptr, "You have nothing to fire with.");
        return;
    }

    /* Find first eligible ammo in the quiver */
    for (i = QUIVER_START; i < QUIVER_END; i++)
    {
        if (p_ptr->inventory[i].tval != p_ptr->state.ammo_tval) continue;
        item = i;
        break;
    }

    /* Require usable ammo */
    if (item < 0)
    {
        msg(p_ptr, "You have no ammunition in the quiver to fire.");
        return;
    }

    /* Require foe */
    if (!target_set_closest(Ind, TARGET_KILL | TARGET_QUIET)) return;

    /* Fire! */
    do_cmd_fire(Ind, dir, item);
}


/*
 * Throw an object from the quiver, pack or floor.
 */
void do_cmd_throw(int Ind, int dir, int item)
{
    player_type *p_ptr = player_get(Ind);
    int shots = 1;
    object_type *o_ptr = object_from_item_idx(p_ptr, item, 0, TRUE);
    int weight = MAX(o_ptr->weight, 10);
    int str = adj_str_blow[p_ptr->state.stat_ind[A_STR]];
    int range = MIN(((str + 20) * 10) / weight, 10);
    ranged_attack attack = make_ranged_throw;
    bool magic = FALSE;
    bool pierce = FALSE;

    /* Restrict ghosts */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_HANDS))
    {
        msg(p_ptr, "You cannot throw items!");
        return;
    }

    /* Check preventive inscription '^v' */
    __trap(p_ptr, CPI(p_ptr, 'v'))

    /* Make sure the player isn't throwing wielded items */
    if ((item >= INVEN_WIELD) && (item < QUIVER_START))
    {
        msg(p_ptr, "You cannot throw wielded items.");
        return;
    }

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return;

    /* Restricted by choice */
    if ((item < 0) && !is_owner(p_ptr, o_ptr))
    {
        msg(p_ptr, "This item belongs to someone else!");
        return;
    }

    /* The inscription prevents it */
    __trap(p_ptr, CGI(o_ptr, 'v', FALSE))

    /* Restrict artifacts */
    if (o_ptr->artifact)
    {
        msg(p_ptr, "You cannot throw that!");
        return;
    }

    /* Never in wrong house */
    if (!check_store_drop(p_ptr, o_ptr))
    {
        msg(p_ptr, "You cannot throw this here.");
        return;
    }

    /* Apply confusion */
    player_confuse_dir(p_ptr, &dir);

    ranged_helper(Ind, item, dir, range, shots, attack, magic, pierce, FALSE);
}


void apply_poly_brand(int r_idx, int m, bitflag f[OF_SIZE], bitflag known_f[OF_SIZE])
{
    monster_race *rp_ptr = &r_info[r_idx];

    /* No special attacks */
    if (!rp_ptr->blow[m].method) return;

    /* Branded attacks give similar brand */
    switch (rp_ptr->blow[m].effect)
    {
        case RBE_POISON:
        case RBE_DISEASE:
        {
            of_on(f, OF_BRAND_POIS);
            of_on(known_f, OF_BRAND_POIS);
            break;
        }
        case RBE_ACID:
        {
            of_on(f, OF_BRAND_ACID);
            of_on(known_f, OF_BRAND_ACID);
            break;
        }
        case RBE_ELEC:
        {
            of_on(f, OF_BRAND_ELEC);
            of_on(known_f, OF_BRAND_ELEC);
            break;
        }
        case RBE_FIRE:
        {
            of_on(f, OF_BRAND_FIRE);
            of_on(known_f, OF_BRAND_FIRE);
            break;
        }
        case RBE_COLD:
        {
            of_on(f, OF_BRAND_COLD);
            of_on(known_f, OF_BRAND_COLD);
            break;
        }
    }
}


void apply_bow_brand(int bow_brand_t, bitflag f[OF_SIZE], bitflag known_f[OF_SIZE])
{
    switch (bow_brand_t)
    {
        case BOW_BRAND_ELEC:
        {
            of_on(f, OF_BRAND_ELEC);
            of_on(known_f, OF_BRAND_ELEC);
            break;
        }
        case BOW_BRAND_COLD:
        {
            of_on(f, OF_BRAND_COLD);
            of_on(known_f, OF_BRAND_COLD);
            break;
        }
        case BOW_BRAND_FIRE:
        {
            of_on(f, OF_BRAND_FIRE);
            of_on(known_f, OF_BRAND_FIRE);
            break;
        }
        case BOW_BRAND_ACID:
        {
            of_on(f, OF_BRAND_ACID);
            of_on(known_f, OF_BRAND_ACID);
            break;
        }
        case BOW_BRAND_POISON:
        {
            of_on(f, OF_BRAND_POIS);
            of_on(known_f, OF_BRAND_POIS);
            break;
        }
    }
}
