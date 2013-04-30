/*
 * File: mon-spell.c
 * Purpose: Functions to deal with spell attacks and effects
 *
 * Copyright (c) 2010-11 Chris Carr
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


#include "../s-angband.h"
#include "../../common/tvalsval.h"
#include "../effects.h"
#include "mon-make.h"
#include "mon-spell.h"
#include "mon-timed.h"
#include "mon-util.h"
#include "../s-spells.h"


/*
 * Details of the different monster spells in the game.
 * See src/monster/monster.h for structure
 */
static const struct mon_spell mon_spell_table[] =
{
    #define RSF(a, b, c, d, e, f, g, h, i, j, k, l, m, n) \
        {RSF_##a, b, c, d, e, f, g, h, i, j, k, l, m, n},
    #define RV(b, x, y, m) {b, x, y, m}
    #include "../../common/list-mon-spells.h"
    #undef RSF
    #undef RV
    {RSF_MAX, 0, NULL, 0, 0, 0, 0, FALSE, 0, NULL, {0, 0, 0, 0}, {0, 0, 0, 0}, NULL, NULL}
};


/*
 * Details of the different side effects of spells.
 * See src/monster/monster.h for structure
 */
static const struct spell_effect spell_effect_table[] =
{
    #define RSE(a, b, c, d, e, f, g, h, i, j, k) \
        {RSE_##a, b, c, d, e, f, g, h, i, j, k},
    #define RV(b, x, y, m) {b, x, y, m}
    #include "list-spell-effects.h"
    #undef RSE
    #undef RV
    {0}
};


/*
 * Determine the damage of a spell attack which ignores monster hp
 * (i.e. bolts and balls, including arrows/boulders/storms/etc.)
 *
 * spell is the attack type
 * rlev is the monster level of the attacker
 * aspect is the damage calc required (min, avg, max, random)
 */
static int nonhp_dam(int spell, int rlev, aspect dam_aspect)
{
    const struct mon_spell *rs_ptr = &mon_spell_table[spell];
    int dam;

    /* Base damage is X + YdZ (m_bonus is not used) */
    dam = randcalc(rs_ptr->base_dam, 0, dam_aspect);

    /* rlev-dependent damage (m_bonus is used as a switch) */
    dam += (rlev * rs_ptr->rlev_dam.base / 100);

    /* rlev affects dice */
    if (rs_ptr->rlev_dam.m_bonus == 1)
        dam += damcalc(MIN(1, rs_ptr->rlev_dam.dice * rlev / 100), rs_ptr->rlev_dam.sides, dam_aspect);

    /* rlev affects sides */
    else
        dam += damcalc(rs_ptr->rlev_dam.dice, rs_ptr->rlev_dam.sides * rlev / 100, dam_aspect);

    return dam;
}


/*
 * Determine the damage of a monster attack which depends on its hp
 *
 * spell is the attack type
 * hp is the monster's hp
 */
static int hp_dam(int spell, int hp)
{
    const struct mon_spell *rs_ptr = &mon_spell_table[spell];
    int dam;

    /* Damage is based on monster's current hp */
    dam = hp / rs_ptr->div;

    /* Check for maximum damage */
    if (dam > rs_ptr->cap) dam = rs_ptr->cap;

    return dam;
}


/*
 * Drain stats at random
 *
 * num is the number of points to drain
 * sustain is whether sustains will prevent draining
 * perma is whether the drains are permanent
 */
static void drain_stats(struct player *p, int num, bool sustain, bool perma)
{
    int i, k = 0;
    const char *act = NULL;

    for (i = 0; i < num; i++)
    {
        switch (randint1(A_MAX))
        {
            case 1: k = A_STR; act = "strong"; break;
            case 2: k = A_INT; act = "bright"; break;
            case 3: k = A_WIS; act = "wise"; break;
            case 4: k = A_DEX; act = "agile"; break;
            case 5: k = A_CON; act = "hale"; break;
            case 6: k = A_CHR; act = "beautiful"; break;
        }

        if (sustain)
            do_dec_stat(p, k, perma);
        else
        {
            msg(p, "You're not as %s as you used to be...", act);
            player_stat_dec(p, k, perma);
        }
    }
}


/*
 * Swap a random pair of stats
 */
static void swap_stats(struct player *p)
{
    int max1, cur1, max2, cur2, ii, jj;

    msg(p, "Your body starts to scramble...");

    /* Pick a pair of stats */
    ii = randint0(A_MAX);
    for (jj = ii; jj == ii; jj = randint0(A_MAX)) /* loop */;

    max1 = p->stat_max[ii];
    cur1 = p->stat_cur[ii];
    max2 = p->stat_max[jj];
    cur2 = p->stat_cur[jj];

    p->stat_max[ii] = max2;
    p->stat_cur[ii] = cur2;
    p->stat_max[jj] = max1;
    p->stat_cur[jj] = cur1;

    p->update |= (PU_BONUS);
}


/*
 * Drain mana from the player, healing the caster.
 *
 * m_idx is the monster/player casting
 * rlev is its level
 * seen is whether @ can see it
 */
static void drain_mana(struct player *p, int m_idx, int rlev, bool seen)
{
    monster_type *m_ptr = NULL;
    int r1;
    char m_name[NORMAL_WID];
    int old_num = get_player_num(p);
    player_type *q_ptr = NULL;

    if (m_idx > 0)
    {
        m_ptr = cave_monster(cave_get(p->depth), m_idx);

        /* Get the monster name (or "it") */
        monster_desc(p, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);
    }
    else
    {
        q_ptr = player_get(0 - m_idx);
        player_desc(p, m_name, sizeof(m_name), q_ptr, TRUE);
    }

    if (!p->csp)
    {
        msg(p, "The draining fails.");
        if (m_ptr && cfg_ai_learn && !(m_ptr->smart & SM_IMM_MANA))
        {
            msg(p, "%s notes that you have no mana!", m_name);
            m_ptr->smart |= SM_IMM_MANA;
        }
        return;
    }

    /* Attack power */
    r1 = (randint1(rlev) / 2) + 1;

    /* Full drain */
    if (r1 >= p->csp)
    {
        r1 = p->csp;
        p->csp = 0;
        p->csp_frac = 0;
        player_clear_timed(p, TMD_MANASHIELD, TRUE);
    }

    /* Partial drain */
    else
        p->csp -= r1;

    /* Hack -- Redraw picture */
    redraw_picture(p, old_num);

    /* Redraw mana */
    p->redraw |= (PR_MANA);

    /* Heal the monster */
    if (m_ptr && (m_ptr->hp < m_ptr->maxhp))
    {
        m_ptr->hp += (6 * r1);
        if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

        /* Redraw (later) if needed */
        update_health(m_idx);

        /* Special message */
        if (seen) msg(p, "%s appears healthier.", m_name);
    }

    /* Heal the player */
    else if (q_ptr && hp_player(q_ptr, r1 * 6))
    {
        /* Special message */
        if (seen) msg(p, "%s appears healthier.", m_name);
    }
}


/*
 * Monster self-healing.
 *
 * m_idx is the monster casting
 * rlev is its level
 * seen is whether @ can see it
 */
static void heal_self(struct player *p, int m_idx, int rlev, bool seen)
{
    monster_type *m_ptr = cave_monster(cave_get(p->depth), m_idx);
    char m_name[NORMAL_WID], m_poss[NORMAL_WID];

    /* No stupid message when at full health */
    if (m_ptr->hp == m_ptr->maxhp) return;

    /* Get the monster name (or "it") */
    monster_desc(p, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);

    /* Get the monster possessive ("his"/"her"/"its") */
    monster_desc(p, m_poss, sizeof(m_poss), m_ptr, MDESC_PRO2 | MDESC_POSS);

    /* Heal some */
    m_ptr->hp += (rlev * 6);

    /* Fully healed */
    if (m_ptr->hp >= m_ptr->maxhp)
    {
        m_ptr->hp = m_ptr->maxhp;

        if (seen)
            msg(p, "%s looks REALLY healthy!", m_name);
        else
            msg(p, "%s sounds REALLY healthy!", m_name);
    }

    /* Partially healed */
    else if (seen)
        msg(p, "%s looks healthier.", m_name);
    else
        msg(p, "%s sounds healthier.", m_name);

    /* Redraw (later) if needed */
    update_health(m_idx);

    /* Cancel fear */
    if (m_ptr->m_timed[MON_TMD_FEAR])
    {
        mon_clear_timed(p, m_ptr, MON_TMD_FEAR, MON_TMD_FLG_NOMESSAGE, FALSE);
        msg(p, "%s recovers %s courage.", m_name, m_poss);
    }

    /* Cancel poison */
    if (m_ptr->m_timed[MON_TMD_POIS])
    {
        mon_clear_timed(p, m_ptr, MON_TMD_POIS, MON_TMD_FLG_NOMESSAGE, FALSE);
        msg(p, "%s is no longer poisoned.", m_name);
    }  

    /* Cancel bleeding */
    if (m_ptr->m_timed[MON_TMD_CUT])
    {
        mon_clear_timed(p, m_ptr, MON_TMD_CUT, MON_TMD_FLG_NOMESSAGE, FALSE);
        msg(p, "%s is no longer bleeding.", m_name);
    }
}


/*
 * This function is used when a group of monsters is summoned.
 */
int summon_monster_aux(struct player *p, int y, int x, int flag, int rlev, int max, int chance)
{
    int count = 0, val = 0, attempts = 0;
    int temp;

    /* Continue adding summoned monsters until we reach the current dungeon level */
    while ((val < p->depth * rlev) && (attempts < max))
    {
        /* Get a monster */
        temp = summon_specific(p, y, x, rlev, flag, 0, chance);

        val += temp * temp;

        /* Increase the attempt, needed in case no monsters were available. */
        attempts++;

        /* Increase count of summoned monsters */
        if (val > 0) count++;
    }

    return count;
}


/*
 * Apply side effects from a spell attack to the player
 *
 * spell is the attack type
 * dam is the amount of damage caused by the attack
 * m_idx is the attacking monster/player
 * seen is whether @ can see it
 */
void do_side_effects(struct player *p, int spell, int dam, int m_idx, bool seen, bool check)
{
    monster_type *m_ptr = NULL;
    const struct spell_effect *re_ptr;
    const struct mon_spell *rs_ptr = &mon_spell_table[spell];
    int i, choice[99], dur = 0, j = 0, count = 0;
    s32b d = 0;
    bool sustain = FALSE, perma = FALSE, chosen[RSE_MAX] = {0};
    int rlev;
    char d_char;
    int y, x;
    bool resist;
    char killer[NORMAL_WID];

    if (m_idx > 0)
    {
        monster_race *r_ptr;

        m_ptr = cave_monster(cave_get(p->depth), m_idx);
        r_ptr = &r_info[m_ptr->r_idx];

        rlev = ((r_ptr->level >= 1)? r_ptr->level: 1);
        d_char = r_ptr->d_char;
        y = m_ptr->fy;
        x = m_ptr->fx;
        resist = resist_undead_attacks(p, r_ptr);
    }
    else
    {
        player_type *q_ptr = player_get(0 - m_idx);

        rlev = dam;
        d_char = 0;
        y = q_ptr->py;
        x = q_ptr->px;
        resist = FALSE;
        my_strcpy(killer, q_ptr->name, sizeof(killer));
    }

    /* First we note all the effects we'll be doing. */
    for (re_ptr = spell_effect_table; re_ptr->index < RSE_MAX; re_ptr++)
    {
        if ((re_ptr->method && (re_ptr->method == rs_ptr->index)) ||
            (re_ptr->gf && (re_ptr->gf == rs_ptr->gf)))
        {
            /* If we have a choice of effects, we create a cum freq table */
            if (re_ptr->chance)
            {
                for (i = j; i < (j + re_ptr->chance); i++) choice[i] = re_ptr->index;
                j = i;
            }
            else
                chosen[re_ptr->index] = TRUE;
        }
    }

    /* If we have built a cum freq table, choose an effect from it */
    if (j) chosen[choice[randint0(j)]] = TRUE;

    /* Now we cycle through again to activate the chosen effects */
    for (re_ptr = spell_effect_table; re_ptr->index < RSE_MAX; re_ptr++)
    {
        if (chosen[re_ptr->index])
        {
            /*
             * Check for resistance - there are three possibilities:
             * 1. Immunity to the attack type if side_immune is TRUE
             * 2. Resistance to the attack type if it affords no immunity
             * 3. Resistance to the specific side-effect
             */
            if (m_ptr && re_ptr->res_flag)
                update_smart_learn(m_ptr, p, re_ptr->res_flag);
            if ((rs_ptr->gf && check_side_immune(p, rs_ptr->gf)) || check_state(p, re_ptr->res_flag))
            {
                msg(p, "You resist the effect!");
                continue;
            }

            /* Allow saving throw if available */
            if (re_ptr->save && (randint0(100) < p->state.skills[SKILL_SAVE]))
            {
                msg(p, "You avoid the effect!");
                continue;
            }

            /* Implement the effect */
            if (re_ptr->timed)
            {
                /* Calculate base duration (m_bonus is not used) */
                dur = randcalc(re_ptr->base, 0, RANDOMISE);

                /* Calculate the damage-dependent duration (m_bonus is used as a cap) */
                dur += damcalc(re_ptr->dam.dice, re_ptr->dam.sides * dam / 100, RANDOMISE);

                if (re_ptr->dam.m_bonus && (dur > re_ptr->dam.m_bonus))
                    dur = re_ptr->dam.m_bonus;

                /* Apply the effect */
                player_inc_timed(p, re_ptr->flag, dur, TRUE, check);
            }
            else
            {
                int flag = re_ptr->flag;

                /* PWMAngband hack: space/time anchor prevents nastiest time effect */
                if (p->timed[TMD_ANCHOR] && (flag == S_DRAIN_ALL))
                    flag = ((randint1(9) < 6)? S_DRAIN_LIFE: S_DRAIN_STAT);

                /* Apply the effect */
                switch (flag)
                {
                    case S_INV_DAM:
                    {
                        if (dam > 0)
                        {
                            /* Calculate the damage (outside the MIN macro!) */
                            int calcdam = dam * randcalc(re_ptr->dam, 0, RANDOMISE);

                            inven_damage(p, re_ptr->gf, MIN(calcdam, 300));
                        }
                        break;
                    }

                    case S_TELEPORT: /* m_bonus is used as a clev filter */
                    {
                        if (!re_ptr->dam.m_bonus || (randint1(re_ptr->dam.m_bonus) > p->lev))
                            teleport_player(p, randcalc(re_ptr->base, 0, RANDOMISE));
                        break;
                    }

                    case S_TELE_TO:
                    {
                        teleport_player_to(p, y, x);
                        break;
                    }

                    case S_TELE_LEV:
                    {
                        teleport_player_level(p);
                        break;
                    }

                    case S_TELE_SELF:
                    {
                        if (m_ptr && !teleport_away(m_ptr, randcalc(re_ptr->base, 0, RANDOMISE)))
                            msg(p, "The attempt fails.");
                        break;
                    }

                    case S_DRAIN_LIFE:
                    {
                        d = re_ptr->base.base + (p->exp * re_ptr->base.sides / 100) * MON_DRAIN_LIFE;

                        msg(p, "You feel your life force draining away!");
                        player_exp_lose(p, d, FALSE);
                        break;
                    }

                    case S_DRAIN_STAT: /* m_bonus is used as a flag */
                    {
                        int num = 1;

                        if (re_ptr->dam.m_bonus > 0) sustain = TRUE;

                        if (abs(re_ptr->dam.m_bonus) > 1) perma = TRUE;

                        /* PWMAngband hack: space/time anchor prevents nastiest time effect */
                        if (!check_state(p, OF_RES_TIME) && !p->timed[TMD_ANCHOR])
                            num = randcalc(re_ptr->base, 0, RANDOMISE);

                        drain_stats(p, num, sustain, perma);
                        break;
                    }

                    case S_SWAP_STAT:
                    {
                        swap_stats(p);
                        break;
                    }

                    case S_DRAIN_ALL:
                    {
                        msg(p, "You're not as powerful as you used to be...");

                        for (i = 0; i < A_MAX; i++) player_stat_dec(p, i, FALSE);
                        break;
                    }

                    case S_DISEN:
                    {
                        apply_disenchant(p, 0);
                        break;
                    }

                    case S_KIN:
                        summon_kin_type = d_char;
                    case S_ANIMAL:
                    case S_SPIDER:
                    case S_HOUND:
                    case S_HYDRA:
                    case S_AINU:
                    case S_DEMON:
                    case S_UNDEAD:
                    case S_DRAGON:
                    case S_HI_DEMON:
                    case S_HI_UNDEAD:
                    case S_HI_DRAGON:
                    case S_WRAITH:
                    case S_UNIQUE:
                    case S_MONSTER:
                    case S_MONSTERS:
                    {
                        count = summon_monster_aux(p, y, x, flag, rlev, re_ptr->base.base, 0);

                        /*
                         * In the special case that uniques or wraiths were summoned but all were
                         * dead, S_HI_UNDEAD is used instead
                         */
                        if (!count && ((re_ptr->flag == S_WRAITH) || (re_ptr->flag == S_UNIQUE)))
                        {
                            count = summon_monster_aux(p, y, x, S_HI_UNDEAD, rlev,
                                re_ptr->base.base, 0);
                        }

                        if (count && p->timed[TMD_BLIND])
                        {
                            msgt(p, rs_ptr->msgt, "You hear %s appear nearby.",
                                ((count > 1)? "many things": "something"));
                        }

                        break;
                    }

                    case S_DRAIN_MANA:
                    {
                        if (resist)
                        {
                            msg(p, "You resist the effects!");
                            break;
                        }

                        drain_mana(p, m_idx, rlev, seen);
                        break;
                    }

                    case S_HEAL:
                    {
                        heal_self(p, m_idx, rlev, seen);
                        break;
                    }

                    case S_DARKEN:
                    {
                        unlight_area(p, 0, 3);
                        break;
                    }

                    case S_TRAPS:
                    {
                        trap_creation(p, TRUE);
                        break;
                    }

                    case S_AGGRAVATE:
                    {
                        aggravate_monsters(p, m_idx);
                        break;
                    }

                    case S_MAGE_PROJECT:
                    {
                        cast_spell_proj(p, TV_MAGIC_BOOK, dam);
                        break;
                    }

                    case S_PRIEST_PROJECT:
                    {
                        cast_spell_proj(p, TV_PRAYER_BOOK, dam);
                        break;
                    }

                    case S_SORC_PROJECT:
                    {
                        cast_spell_proj(p, TV_SORCERY_BOOK, dam);
                        break;
                    }

                    case S_SHAD_PROJECT:
                    {
                        cast_spell_proj(p, TV_SHADOW_BOOK, dam);
                        break;
                    }

                    case S_HUNT_PROJECT:
                    {
                        cast_spell_proj(p, TV_HUNT_BOOK, dam);
                        break;
                    }

                    case S_PSI_PROJECT:
                    {
                        cast_spell_proj(p, TV_PSI_BOOK, dam);
                        break;
                    }

                    case S_DEATH_PROJECT:
                    {
                        cast_spell_proj(p, TV_DEATH_BOOK, dam);
                        break;
                    }

                    case S_GHOST_PROJECT:
                    {
                        cast_spell_proj(p, 1, dam);
                        break;
                    }

                    case S_MIMIC_PROJECT:
                    {
                        cast_spell_proj(p, 2, dam);
                        break;
                    }

                    case S_ELEM_PROJECT:
                    {
                        cast_spell_proj(p, TV_ELEM_BOOK, dam);
                        break;
                    }

                    case S_SUMMON_PROJECT:
                    {
                        cast_spell_proj(p, TV_SUMMON_BOOK, dam);
                        break;
                    }

                    case S_POLY_BAT:
                    {
                        poly_bat(p, 10 + dam * 4, killer);
                        break;
                    }

                    default: break;
                }
            }
        }
    }
}


/*
 * Calculate the damage of a monster spell.
 *
 * spell is the spell in question.
 * hp is the hp of the casting monster.
 * rlev is the level of the casting monster.
 dam_aspect is the damage calc we want (min, max, avg, random).
 */
static int mon_spell_dam(int spell, int hp, int rlev, aspect dam_aspect)
{
    const struct mon_spell *rs_ptr = &mon_spell_table[spell];

    if (rs_ptr->div) return hp_dam(spell, hp);
    return nonhp_dam(spell, rlev, dam_aspect);
}


/*
 * Process a monster spell
 *
 * spell is the monster spell flag (RSF_FOO)
 * m_idx is the attacking monster
 * seen is whether the player can see the monster at this moment
 */
void do_mon_spell(int Ind, int spell, int m_idx, bool seen)
{
    player_type *p_ptr = player_get(Ind);
    const struct mon_spell *rs_ptr = &mon_spell_table[spell];
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    char m_name[NORMAL_WID], ddesc[NORMAL_WID], buf[NORMAL_WID];
    bool hits = FALSE;
    int dam = 0, flag = 0, rad = 0;
    int y = p_ptr->py, x = p_ptr->px;

    /* Extract the monster level */
    int rlev = ((r_ptr->level >= 1)? r_ptr->level: 1);

    /* Antimagic field prevents magical spells from working */
    if (!(rs_ptr->type & (RST_BREATH | RST_ATTACK | RST_MISSILE)) && check_antimagic(p_ptr, m_ptr))
        return;

    /* Antisummon field prevents summoning spells from working */
    if ((rs_ptr->type & RST_SUMMON) && check_antisummon(p_ptr, m_ptr)) return;

    /* Get the monster name (or "it") */
    monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);

    /* See if it hits */
    if (rs_ptr->hit == 100)
        hits = TRUE;
    else if (rs_ptr->hit == 0)
        hits = FALSE;
    else
        hits = check_hit(p_ptr, rs_ptr->hit, rlev);

    /* Tell the player what's going on */
    disturb(p_ptr, 1, 0);

    if (!seen)
        strnfmt(buf, sizeof(buf), "Something %s.", rs_ptr->blind_verb);
    else if (!hits)
        strnfmt(buf, sizeof(buf), "%s %s %s, but misses.", m_name, rs_ptr->verb, rs_ptr->desc);
    else if (spell == RSF_S_KIN)
    {
        char m_poss[NORMAL_WID];

        monster_desc(p_ptr, m_poss, sizeof(m_poss), m_ptr, MDESC_PRO2 | MDESC_POSS);
        strnfmt(buf, sizeof(buf), "%s %s %s %s.", m_name, rs_ptr->verb, m_poss,
            (rf_has(r_ptr->flags, RF_UNIQUE)? "minions": "kin"));
    }
    else
        strnfmt(buf, sizeof(buf), "%s %s %s.", m_name, rs_ptr->verb, rs_ptr->desc);

    if (rs_ptr->msgt) msgt(p_ptr, rs_ptr->msgt, buf);
    else msg(p_ptr, buf);

    if (!hits) return;

    /* Try a saving throw if available */
    if (((rs_ptr->save & RSV_SKILL) && (randint0(100) < p_ptr->state.skills[SKILL_SAVE])) ||
        ((rs_ptr->save & RSV_UNDEAD) && resist_undead_attacks(p_ptr, r_ptr)))
    {
        msg(p_ptr, "You avoid the effects!");
        return;
    }

    /* Calculate the damage */
    dam = mon_spell_dam(spell, m_ptr->hp, rlev, RANDOMISE);

    /* Get the "died from" name in case this attack kills @ */
    monster_desc(p_ptr, ddesc, sizeof(ddesc), m_ptr, MDESC_SHOW | MDESC_IND2);

    /* Display the attack, adjust for resists and apply effects */
    if (rs_ptr->type & RST_BOLT)
        flag = PROJECT_STOP | PROJECT_KILL;
    else if (rs_ptr->type & (RST_BALL | RST_BREATH))
    {
        flag = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
        rad = (rf_has(r_ptr->flags, RF_POWERFUL)? 3: 2);
    }
    else if ((rs_ptr->type & RST_ANNOY) && (rs_ptr->type & RST_SUMMON))
    {
        flag = PROJECT_ITEM | PROJECT_HIDE;
        rad = 1 + rlev / 10;
        y = m_ptr->fy;
        x = m_ptr->fx;
    }

    if (rs_ptr->gf && !(rs_ptr->type & RST_SPECIAL))
    {
        project(m_idx, rad, p_ptr->depth, y, x, dam, rs_ptr->gf, flag, rs_ptr->what);
        monster_learn_resists(m_ptr, p_ptr, rs_ptr->gf);
    }
    else
    {
        /* Note that non-projectable attacks are unresistable */
        strnfmt(p_ptr->died_flavor, sizeof(p_ptr->died_flavor), "was %s by %s", rs_ptr->what, ddesc);
        if (take_hit(p_ptr, dam, ddesc, TRUE)) return;
    }

    do_side_effects(p_ptr, spell, dam, m_idx, seen, FALSE);
}


/*
 * Test a spell bitflag for a type of spell.
 * Returns TRUE if any desired type is among the flagset
 *
 * f is the set of spell flags we're testing
 * type is the spell type(s) we're looking for
 */
bool test_spells(bitflag *f, enum mon_spell_type type)
{
    const struct mon_spell *rs_ptr;

    for (rs_ptr = mon_spell_table; rs_ptr->index < RSF_MAX; rs_ptr++)
    {
        if (rsf_has(f, rs_ptr->index) && (rs_ptr->type & type))
            return TRUE;
    }

    return FALSE;
}


/*
 * Set a spell bitflag to allow only a specific set of spell types.
 *
 * f is the set of spell flags we're pruning
 * type is the spell type(s) we're allowing
 */
void set_spells(bitflag *f, enum mon_spell_type type)
{
    const struct mon_spell *rs_ptr;

    for (rs_ptr = mon_spell_table; rs_ptr->index < RSF_MAX; rs_ptr++)
    {
        if (rsf_has(f, rs_ptr->index) && !(rs_ptr->type & type))
            rsf_off(f, rs_ptr->index);
    }
}


/*
 * Turn off spells with a side effect or a gf_type that is resisted by
 * something in flags, subject to intelligence and chance.
 *
 * spells is the set of spells we're pruning
 * flags is the set of flags we're testing
 * r_ptr is the monster type we're operating on
 */
void unset_spells(int Ind, bitflag *spells, bitflag *flags, const monster_race *r_ptr)
{
    player_type *p_ptr = player_get(Ind);
    const struct mon_spell *rs_ptr;
    const struct spell_effect *re_ptr;

    /* First we test the gf (projectable) spells */
    for (rs_ptr = mon_spell_table; rs_ptr->index < RSF_MAX; rs_ptr++)
    {
        if (rs_ptr->gf && !(rs_ptr->type & RST_SPECIAL) &&
            (randint0(100) < check_for_resist_aux(p_ptr, rs_ptr->gf, flags, FALSE) *
            (rf_has(r_ptr->flags, RF_SMART)? 2: 1) * 25))
        {
            rsf_off(spells, rs_ptr->index);
        }
    }

    /* ... then we test the non-gf side effects */
    for (re_ptr = spell_effect_table; re_ptr->index < RSE_MAX; re_ptr++)
    {
        if (re_ptr->method && re_ptr->res_flag && (rf_has(r_ptr->flags, RF_SMART) || !one_in_(3)) &&
            of_has(flags, re_ptr->res_flag))
        {
            rsf_off(spells, re_ptr->method);
        }
    }
}


/*
 * Calculate a monster's maximum spell power.
 *
 * r_ptr is the monster we're studying
 * resist is the degree of resistance we're assuming to any
 *   attack type (-1 = vulnerable ... 3 = immune)
 */
int best_spell_power(const monster_race *r_ptr, int resist)
{
    const struct mon_spell *rs_ptr;
    const struct spell_effect *re_ptr;
    int dam = 0, best_dam = 0;

    /* Extract the monster level */
    int rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

    for (rs_ptr = mon_spell_table; rs_ptr->index < RSF_MAX; rs_ptr++)
    {
        if (rsf_has(r_ptr->spell_flags, rs_ptr->index))
        {
            /* Get the maximum basic damage output of the spell (could be 0) */
            dam = mon_spell_dam(rs_ptr->index, mon_hp(r_ptr, MAXIMISE), rlev, MAXIMISE);

            /* For all attack forms the player can save against, damage is halved */
            if (rs_ptr->save) dam /= 2;

            /* Adjust the real damage by the assumed resistance (if it is a resistable type) */
            if (rs_ptr->gf) dam = adjust_dam(NULL, rs_ptr->gf, dam, MAXIMISE, resist);

            /*
             * Add the power ratings assigned to the various possible spell
             * effects (which is crucial for non-damaging spells)
             */
            for (re_ptr = spell_effect_table; re_ptr->index < RSE_MAX; re_ptr++)
            {
                if ((re_ptr->method && (re_ptr->method == rs_ptr->index)) ||
                    (re_ptr->gf && (re_ptr->gf == rs_ptr->gf)))
                {
                    /* First we adjust the real damage if necessary */
                    if (re_ptr->power.dice)
                        dam = (dam * re_ptr->power.dice) / 100;

                    /* Then we add any flat rating for this effect */
                    dam += re_ptr->power.base;

                    /* Then we add any rlev-dependent rating */
                    if (re_ptr->power.m_bonus < 0)
                        dam += re_ptr->power.sides / (rlev + 1);
                    else if (re_ptr->power.m_bonus > 0)
                        dam += (re_ptr->power.sides * rlev) / 100;
                }
            }

            /* Update the best_dam tracker */
            if (dam > best_dam) best_dam = dam;
        }
    }

    return best_dam;
}


static void strrep(char *dest, size_t len, const char *src, const char *search, const char *replace)
{
    char *ptr = strstr(src, search);

    if (ptr)
    {
        my_strcpy(dest, src, 1 + ptr - src);
        my_strcat(dest, replace, len);
        my_strcat(dest, ptr + strlen(search), len);
    }
    else
        my_strcpy(dest, src, len);
}


/*
 * Process a monster spell (MvM)
 *
 * target_m_idx is the target monster
 * spell is the monster attack flag (RSF_FOO)
 * m_idx is the attacking monster
 * seen is whether the player can see the monster at this moment
 */
void do_mon_spell_MvM(int Ind, int target_m_idx, int spell, int m_idx, bool seen)
{
    player_type *p_ptr = player_get(Ind);
    const struct mon_spell *rs_ptr = &mon_spell_table[spell];
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    char m_name[NORMAL_WID], buf[NORMAL_WID], tmp[NORMAL_WID];
    bool hits = FALSE;
    int dam = 0, flag = 0, rad = 0;
    monster_type *target_m_ptr = cave_monster(cave_get(p_ptr->depth), target_m_idx);
    const struct spell_effect *re_ptr;
    int count = 0;
    int y = target_m_ptr->fy, x = target_m_ptr->fx;

    /* Extract the monster level */
    int rlev = ((r_ptr->level >= 1)? r_ptr->level: 1);

    /* Antimagic field prevents magical spells from working */
    if (!(rs_ptr->type & (RST_BREATH | RST_ATTACK | RST_MISSILE)) && check_antimagic(p_ptr, m_ptr))
        return;

    /* Antisummon field prevents summoning spells from working */
    if ((rs_ptr->type & RST_SUMMON) && check_antisummon(p_ptr, m_ptr)) return;

    /* Get the monster name (or "it") */
    monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);

    /* See if it hits */
    if (rs_ptr->hit == 100)
        hits = TRUE;
    else if (rs_ptr->hit == 0)
        hits = FALSE;
    else
        hits = check_hit_MvM(target_m_ptr->ac, rs_ptr->hit, rlev);

    /* Tell the player what's going on */
    disturb(p_ptr, 1, 0);

    if (!seen)
        strnfmt(buf, sizeof(buf), "Something %s.", rs_ptr->blind_verb);
    else if (!hits)
        strnfmt(buf, sizeof(buf), "%s %s %s, but misses.", m_name, rs_ptr->verb, rs_ptr->desc);
    else if (spell == RSF_S_KIN)
    {
        char m_poss[NORMAL_WID];

        monster_desc(p_ptr, m_poss, sizeof(m_poss), m_ptr, MDESC_PRO2 | MDESC_POSS);
        strnfmt(buf, sizeof(buf), "%s %s %s %s.", m_name, rs_ptr->verb, m_poss,
            (rf_has(r_ptr->flags, RF_UNIQUE)? "minions": "kin"));
    }
    else
        strnfmt(buf, sizeof(buf), "%s %s %s.", m_name, rs_ptr->verb, rs_ptr->desc);

    /* Hack -- Replace "you"/"your" by "something"/"some" */
    strrep(tmp, sizeof(tmp), buf, "your", "some");
    strrep(buf, sizeof(buf), tmp, "you", "something");

    if (rs_ptr->msgt) msgt(p_ptr, rs_ptr->msgt, buf);
    else msg(p_ptr, buf);

    if (!hits) return;

    /* Calculate the damage */
    if (rs_ptr->div)
        dam = hp_dam(spell, m_ptr->hp);
    else if (rs_ptr->type & (RST_ANNOY | RST_SPECIAL))
        dam = rlev;
    else if (rs_ptr->index == RSF_TELE_AWAY)
        dam = MAX_SIGHT_LGE * 5;
    else if (rs_ptr->index == RSF_TELE_LEVEL)
        dam = 0;
    else
        dam = nonhp_dam(spell, rlev, RANDOMISE);

    /* Display the attack, adjust for resists and apply effects */
    if (rs_ptr->type & (RST_BOLT | RST_SPECIAL))
        flag = PROJECT_STOP | PROJECT_KILL | PROJECT_AWARE;
    else if (rs_ptr->type & (RST_BALL | RST_BREATH))
    {
        flag = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
        rad = (rf_has(r_ptr->flags, RF_POWERFUL)? 3: 2);
    }
    else if ((rs_ptr->type & RST_ANNOY) && (rs_ptr->type & RST_SUMMON))
    {
        flag = PROJECT_ITEM | PROJECT_HIDE;
        rad = 1 + rlev / 10;
        y = m_ptr->fy;
        x = m_ptr->fx;
    }

    if (rs_ptr->gf)
        project(m_idx, rad, target_m_ptr->depth, y, x, dam, rs_ptr->gf, flag, rs_ptr->what);

    for (re_ptr = spell_effect_table; re_ptr->index < RSE_MAX; re_ptr++)
    {
        if ((re_ptr->method && (re_ptr->method == rs_ptr->index)) ||
            (re_ptr->gf && (re_ptr->gf == rs_ptr->gf)))
        {
            /* Apply the effect */
            switch (re_ptr->flag)
            {
                case S_TELE_TO:
                {
                    if (!teleport_to(p_ptr->depth, target_m_idx, m_ptr->fy, m_ptr->fx))
                        msg(p_ptr, "The attempt fails.");
                    break;
                }

                case S_TELE_SELF:
                {
                    if (!teleport_away(m_ptr, randcalc(re_ptr->base, 0, RANDOMISE)))
                        msg(p_ptr, "The attempt fails.");
                    break;
                }

                case S_KIN:
                    summon_kin_type = r_ptr->d_char;
                case S_ANIMAL:
                case S_SPIDER:
                case S_HOUND:
                case S_HYDRA:
                case S_AINU:
                case S_DEMON:
                case S_UNDEAD:
                case S_DRAGON:
                case S_HI_DEMON:
                case S_HI_UNDEAD:
                case S_HI_DRAGON:
                case S_WRAITH:
                case S_UNIQUE:
                case S_MONSTER:
                case S_MONSTERS:
                {
                    count = summon_monster_aux(p_ptr, m_ptr->fy, m_ptr->fx, re_ptr->flag, rlev,
                        re_ptr->base.base, 0);

                    if (count && p_ptr->timed[TMD_BLIND])
                    {
                        msgt(p_ptr, rs_ptr->msgt, "You hear %s appear nearby.",
                            ((count > 1)? "many things": "something"));
                    }

                    break;
                }

                case S_DRAIN_MANA:
                {
                    monster_race *target_r_ptr = &r_info[target_m_ptr->r_idx];
                    int r1;

                    /* Affects only casters */
                    if (!target_r_ptr->freq_spell) break;

                    /* Attack power, capped vs monster level */
                    r1 = (randint1(rlev) / 2) + 1;
                    if (r1 > (target_m_ptr->level / 6) + 1)
                        r1 = (target_m_ptr->level / 6) + 1;

                    /* Heal the monster */
                    if (m_ptr->hp < m_ptr->maxhp)
                    {
                        /* Heal */
                        m_ptr->hp += (6 * r1);
                        if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

                        /* Redraw (later) if needed */
                        update_health(m_idx);

                        /* Special message */
                        if (seen) msg(p_ptr, "%s appears healthier.", m_name);
                    }

                    break;
                }

                case S_HEAL:
                {
                    heal_self(p_ptr, m_idx, rlev, seen);
                    break;
                }

                case S_DARKEN:
                {
                    unlight_area_MvM(target_m_idx, p_ptr->depth, m_idx);
                    break;
                }

                case S_TRAPS:
                {
                    trap_creation_MvM(target_m_idx, p_ptr->depth, m_idx);
                    break;
                }

                default: break;
            }

            break;
        }
    }
}


bool is_spell_summon(int spell)
{
    const struct mon_spell *rs_ptr = &mon_spell_table[spell];

    if (rs_ptr->type & RST_SUMMON) return TRUE;
    return FALSE;
}


int spell_effect(int spell)
{
    const struct mon_spell *rs_ptr = &mon_spell_table[spell];

    return rs_ptr->gf;
}
