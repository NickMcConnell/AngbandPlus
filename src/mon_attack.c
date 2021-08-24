#include "angband.h"

#include <assert.h>

#define _ATTACK_PLR 0x0001
#define _UNVIEW     0x0002
#define _RETALIATE  0x0004

/*************************************************************************
 * Private Helpers
 *************************************************************************/
static void _init_race(mon_attack_ptr context)
{
    assert(context->mon);
    context->race = context->mon->race;
    monster_desc(context->mon_full_name, context->mon, 0);
    monster_desc(context->mon_name, context->mon, MD_PRON_VISIBLE);
    monster_desc(context->mon_name_obj, context->mon, MD_PRON_VISIBLE | MD_OBJECTIVE);
}
static void _init_race2(mon_attack_ptr context)
{
    assert(context->mon2);
    context->race2 = context->mon2->race;
    monster_desc(context->mon2_full_name, context->mon2, 0);
    monster_desc(context->mon2_name, context->mon2, MD_PRON_VISIBLE);
    monster_desc(context->mon2_name_obj, context->mon2, MD_PRON_VISIBLE | MD_OBJECTIVE);
}
static void _check_race(mon_attack_ptr context) /* GF_CHAOS might polymorph */
{
    if (context->race->id != context->mon->race->id)
        _init_race(context);
    if (context->race2 && context->race2->id != context->mon2->race->id)
        _init_race2(context);
}
static bool _stop_attack(mon_attack_ptr context)
{
    if (context->stop) return TRUE;
    if (!mon_is_valid(context->mon))
        context->stop = STOP_MON_DEAD; /* .. or deleted */
    else if (mon_tim_find(context->mon, T_PARALYZED) || mon_tim_find(context->mon, MT_SLEEP))
        context->stop = STOP_MON_PARALYZED;
    /* XXX Confused monsters should be able to stumble into the player for melee attacks ...
    else if (mon_tim_find(context->mon, T_CONFUSED))
        context->stop = STOP_MON_CONFUSED; */
    else if (mon_tim_find(context->mon, T_FEAR))
        context->stop = STOP_MON_FEAR;
    else if (!point_equals(context->mon->pos, context->mon_pos))
        context->stop = STOP_MON_MOVED;
    else if (context->flags & _ATTACK_PLR)
    {
        if (plr->is_dead)
            context->stop = STOP_PLR_DEAD;
        else if (plr->leaving)
            context->stop = STOP_PLR_LEAVING;
        else if (!mon_is_hostile(context->mon))
            context->stop = STOP_MON_FRIENDLY;
        else if (!point_equals(plr->pos, context->tgt_pos))
            context->stop = STOP_PLR_MOVED;
    }
    else
    {
        if (!mon_is_valid(context->mon2))
            context->stop = STOP_MON_DEAD;
        if (!point_equals(context->mon2->pos, context->tgt_pos))
            context->stop = STOP_MON_MOVED;
    }
    return context->stop != STOP_CONTINUE;
}
static void _plr_custom_init(mon_attack_ptr context)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.mon_attack_init) r->hooks.mon_attack_init(context);
    if (c->hooks.mon_attack_init) c->hooks.mon_attack_init(context);
}

/*************************************************************************
 * Context
 *************************************************************************/
static mon_attack_ptr _current = NULL;
mon_attack_ptr mon_attack_current(void) { return _current; }

bool mon_attack_begin(mon_attack_ptr context, mon_ptr mon, point_t pos)
{
    if (cave->flags & DF_NO_MELEE)
        return FALSE;

    context->stop = STOP_CONTINUE;
    context->mon = mon;
    _init_race(context);
    context->mon_pos = mon->pos;
    context->tgt_pos = pos;

    if (mon_race_never_blow(context->race)) return FALSE;

    assert(cave == mon->dun);
    if (dun_plr_at(cave, pos))
    {
        if (plr->riding && (!mon_is_hostile(mon) || one_in_(2)))
        {
            context->mon2 = plr_riding_mon();
            _init_race2(context);
            if (!mon_tim_find(mon, T_CONFUSED) && !are_enemies(mon, context->mon2)) return FALSE;
        }
        else
        {
            context->mon2 = NULL;
            context->flags |= _ATTACK_PLR;
            if (!mon_tim_find(mon, T_CONFUSED) && !mon_is_hostile(mon)) return FALSE;
        }
    }
    else
    {
        mon_ptr tgt_mon = dun_mon_at(mon->dun, pos);
        if (!tgt_mon) return FALSE;
        context->mon2 = tgt_mon;
        _init_race2(context);
        if (!mon_show_msg(context->mon) && !mon_show_msg(context->mon2))
            context->flags |= _UNVIEW;
        if (!mon_tim_find(mon, T_CONFUSED) && !are_enemies(mon, context->mon2)) return FALSE;
    }

    if (context->flags & _ATTACK_PLR)
    {
        _plr_custom_init(context); /* give race/class a chance to install a begin_f! */
        if (context->begin_f)
        {
            context->begin_f(context);
            if (context->stop) return FALSE; /* allow custom aborts */
        }
    }

    _current = context;
    return TRUE;
}

void mon_attack_end(mon_attack_ptr context)
{
    if (context->stop != STOP_MON_DEAD)
    {
        /* fixup ai targetting */
        if (context->flags & _ATTACK_PLR) /* "lock on" to player */
            mon_clear_target(context->mon);
        else if (context->mon2)
        {
            if (context->hits || (context->mon->mflag2 & MFLAG2_ILLUSION))
                mon_set_target(context->mon2, context->mon->pos);
            if (mon_race_is_(context->mon2->race, "@.player")) /* lock on to "player" */
                mon_set_target(context->mon, context->mon2->pos);
        }
        /* remember last enemy */
        context->mon->last_enemy_pos = context->tgt_pos;
    }
    _current = NULL;
}

/*************************************************************************
 * Hit or Miss?
 *************************************************************************/
static int _scale(mon_ptr mon, int amt)
{
    int stun = mon_tim_amount(mon, T_STUN);
    amt = amt * mon->mpower / 1000;
    if (mon_tim_find(mon, T_BERSERK))
        amt = amt * 125 / 100;
    if (stun)
        amt -= amt*MIN(100, stun) / 150;
    return amt;
}
static int _check_hit(int skill, int ac)
{
    int k = randint0(100);
    if (k < 10) return (k < 5);
    if (skill > 0 && randint1(skill) > ac*3/4) return TRUE;
    return FALSE;
}
bool mon_check_hit(mon_attack_ptr context)
{
    int skill;

    if (context->flags & _ATTACK_PLR)
        context->ac = plr_ac(context->mon);
    else
        context->ac = mon_ac(context->mon2);

    if (context->mon->mflag2 & MFLAG2_ILLUSION) return FALSE;
    if (!context->blow->effect_ct) return TRUE;  /* B:(DROOL|MOAN|BEG|INSULT)$ always hit */

    skill = mon_skill_thn(context->mon);
    skill += context->blow->power;
    skill += context->to_h * BTH_PLUS_ADJ;
    skill = _scale(context->mon, skill);

    return _check_hit(skill, context->ac);
}

/*************************************************************************
* Attack Helpers
*************************************************************************/
static void _disturb(mon_attack_ptr context)
{
    if (context->flags & _ATTACK_PLR)
        disturb(1, 0);
}
static void _miss_msg(mon_attack_ptr context)
{
    if (context->flags & _UNVIEW) return;
    switch (context->blow->method)
    {
    case RBM_DROOL:
        if (context->mon->ml)
        {
            _disturb(context);
            msg_format("%^s slobbers ineffectually.", context->mon_name);
        }
        break;
    case RBM_WAIL:
        if (context->mon->ml)
        {
            _disturb(context);
            msg_format("%^s wails ineffectually.", context->mon_name);
        }
        break;
    case RBM_GAZE:
        if (context->mon->ml)
        {
            char tmp[MAX_NLEN];
            monster_desc(tmp, context->mon, MD_PRON_VISIBLE | MD_POSSESSIVE);
            _disturb(context);
            if (context->flags & _ATTACK_PLR)
                msg_format("You avoid %s gaze.", tmp);
            else
                msg_format("<color:o>%^s</color> avoids <color:B>%s</color> gaze.", context->mon2_name, tmp);
        }
        break;
    default:
        if (context->mon->ml)
        {
            _disturb(context);
            msg_format("%^s misses.", context->mon_name);
        }
    }
}
static void _hit_msg(mon_attack_ptr context)
{
    mon_blow_info_ptr info = mon_blow_info_lookup(context->blow->method);
    if (!info)
    {
        msg_format("Bug: Unknown blow method = %d", context->blow->method);
        return;
    }
    if (context->flags & _ATTACK_PLR)
    {
        if (plr_tim_find(T_HALLUCINATE) && one_in_(2))
        {
            context->blow_lore |= MON_BLOW_SILLY;
            msg_format("%^s %s you.", context->mon_name, silly_attacks[randint0(MAX_SILLY_ATTACK)]);
        }
        else
        {
            /* XXX flavorful moans, insults and songs */
            msg_format(info->mon_hit_msg, context->mon_name, "you");
        }
    }
    else if (!(context->flags & _UNVIEW))
    {
        if (plr_tim_find(T_HALLUCINATE) && one_in_(2))
        {
            msg_format("<color:B>%^s</color> %s <color:o>%s</color>.",
                context->mon_name, silly_attacks[randint0(MAX_SILLY_ATTACK)], context->mon2_name_obj);
        }
        else
        {
            msg_format(info->mon_hit_msg, context->mon_name, context->mon2_name_obj);
        }
    }
}

static bool _explode(mon_attack_ptr context)
{
    if (context->blow->method != RBM_EXPLODE) return FALSE;
    mon_tim_delete(context->mon, T_INVULN); /* otherwise mon_take_hit will fail */
    if (mon_take_hit(context->mon, context->mon->hp + 1, &context->fear, NULL))
    {
        context->stop = STOP_MON_DEAD;
        return TRUE;
    }
    return FALSE; /* ??? */
}

/*************************************************************************
* Attack Player
*************************************************************************/
static obj_ptr _get_drain_device(void)
{
    /* For RBE_DRAIN_CHARGES ... Mage quivers do not get a free ride.
     * We also pick any slot, even empty or non-device slots for
     * historical reasons. */
    if (equip_find_obj(TV_QUIVER, SV_QUIVER_MAGE) && one_in_(2))
    {
        slot_t slot = quiver_random_slot(NULL);
        if (!slot) return NULL;
        return quiver_obj(slot);
    }
    else
    {
        slot_t slot = pack_random_slot(NULL);
        if (!slot) return NULL;
        return pack_obj(slot);
    }
}

static void _effect_plr(mon_attack_ptr context, mon_effect_ptr effect, int dam)
{
    int k;
    switch (effect->type)
    {
    case RBE_HURT: {
        int pct = ac_melee_pct(context->ac);
        dam = dam * pct / 100;
        context->dam += take_hit(DAMAGE_ATTACK, dam, context->mon_full_name);
        break; }
    case RBE_DRAIN_CHARGES: {
        char buf[MAX_NLEN];
        bool drained = FALSE;
        int  drain_amt = mon_lvl(context->mon);

        context->dam += take_hit(DAMAGE_ATTACK, dam, context->mon_full_name);
        if (plr->is_dead || CHECK_MULTISHADOW()) break;

        /* Find an item */
        for (k = 0; k < 10; k++)
        {
            obj_ptr obj = _get_drain_device();

            if (!obj) continue;
            if (!obj_is_device(obj)) continue;

            if (obj_has_flag(obj, OF_HOLD_LIFE))
            {
                drained = TRUE; /* No food drain! */
                break;
            }

            if (obj->tval == TV_ROD)
                drain_amt /= 3;

            if (drain_amt > device_sp(obj))
                drain_amt = device_sp(obj);

            if (plr->no_charge_drain)
                break;

            if (plr->pclass == CLASS_DEVICEMASTER)
            {
                int pl = plr->lev;
                int dl = obj->activation.difficulty;

                if (devicemaster_is_speciality(obj))
                    pl *= 2;

                if (pl >= randint1(dl))
                {
                    msg_print("Energy begins to drain from your pack ... But you pull it back!");
                    drained = TRUE; /* No food drain! */
                    break;
                }
            }

            object_desc(buf, obj, OD_OMIT_PREFIX | OD_COLOR_CODED);
            msg_format("Energy drains from your %s!", buf);
            device_decrease_sp(obj, drain_amt);
            drained = TRUE;

            /* Heal the monster */
            context->mon->hp += drain_amt;
            if (context->mon->hp > context->mon->maxhp)
                context->mon->hp = context->mon->maxhp;

            check_mon_health_redraw(context->mon);
            plr->window |= (PW_INVEN);
            break;
        }

        if ( !drained
          && !(get_race()->flags & RACE_IS_NONLIVING)
          && !prace_is_(RACE_MON_JELLY) )
        {
            msg_print("Food drains from your belly!");
            set_food(MAX(0, MIN(plr->food - 1000, plr->food*2/3)));
        }
        break; }
    case RBE_EAT_GOLD:
        if (mon_tim_find(context->mon, T_CONFUSED)) break;
        if (plr->is_dead || CHECK_MULTISHADOW()) break;

        if (mon_is_thief(context->mon))
            mon_lore_thief(context->mon);

        if (plr_block_steal(context->mon))
        {
            msg_format("%^s decides not to steal from you.", context->mon_name);
        }
        else if ( !plr_tim_find(T_PARALYZED)
               && randint0(100) < adj_dex_safe[plr->stat_ind[A_DEX]] + plr->lev )
        {
            msg_print("You quickly protect your money pouch!");
            if (randint0(3)) context->do_blink = TRUE;
        }
        else
        {
            int gold = (plr->au / 10) + randint1(25);
            if (gold < 2) gold = 2;
            if (gold > 5000) gold = (plr->au / 20) + randint1(3000);
            if (gold > plr->au) gold = plr->au;
            plr->au -= gold;
            stats_on_gold_stolen(gold);
            if (gold <= 0)
                msg_print("Nothing was stolen.");
            else if (plr->au)
            {
                msg_print("Your purse feels lighter.");
                msg_format("%d coins were stolen!", gold);
                virtue_add(VIRTUE_SACRIFICE, 1);
            }
            else
            {
                msg_print("Your purse feels lighter.");
                msg_print("All of your coins were stolen!");
                virtue_add(VIRTUE_SACRIFICE, 2);
            }

            plr->redraw |= PR_GOLD;
            if (prace_is_(RACE_MON_LEPRECHAUN))
                plr->update |= PU_BONUS | PU_HP | PU_MANA;

            context->do_blink = TRUE;
        }
        break;
    case RBE_EAT_ITEM:
        if (mon_tim_find(context->mon, T_CONFUSED)) break;
        if (plr->is_dead || CHECK_MULTISHADOW()) break;

        if (mon_is_thief(context->mon))
            mon_lore_thief(context->mon);

        if (plr_block_steal(context->mon))
        {
            msg_format("%^s decides not to steal from you.", context->mon_name);
            break;
        }
        else if ( !plr_tim_find(T_PARALYZED)
               && randint0(100) < adj_dex_safe[plr->stat_ind[A_DEX]] + plr->lev )
        {
            msg_print("You grab hold of your backpack!");
            context->do_blink = TRUE;
            break;
        }

        for (k = 0; k < 10; k++)
        {
            slot_t  slot = pack_random_slot(NULL);
            obj_ptr obj;
            obj_t   loot;
            char    o_name[MAX_NLEN];

            if (!slot) continue;
            obj = pack_obj(slot);

            if (!obj) continue;
            if (obj_is_art(obj)) continue;

            object_desc(o_name, obj, OD_OMIT_PREFIX | OD_COLOR_CODED);

            msg_format("%sour %s was stolen!",
                   ((obj->number > 1) ? "One of y" : "Y"),
                   o_name);

            virtue_add(VIRTUE_SACRIFICE, 1);

            loot = *obj;
            loot.number = 1;
            loot.marked = OM_TOUCHED;
            dun_mon_steal_plr(cave, context->mon, &loot);

            obj->number--;
            obj_release(obj, OBJ_RELEASE_QUIET);

            context->do_blink = TRUE;
            break;
        }
        break;
    case RBE_EAT_FOOD:
        if (plr->is_dead || CHECK_MULTISHADOW()) break;

        if (mon_is_thief(context->mon))
            mon_lore_thief(context->mon);

        if (plr_block_steal(context->mon))
        {
            msg_format("%^s decides not to steal from you.", context->mon_name);
            break;
        }

        for (k = 0; k < 10; k++)
        {
            slot_t  slot = pack_random_slot(NULL);
            obj_ptr obj;
            char    o_name[MAX_NLEN];

            if (!slot) continue;
            obj = pack_obj(slot);
            if (!obj) continue;
            if (obj->tval != TV_FOOD && !(obj->tval == TV_CORPSE && obj->sval)) continue;
            if (obj_is_art(obj)) continue;

            object_desc(o_name, obj, OD_OMIT_PREFIX | OD_NAME_ONLY | OD_COLOR_CODED);
            msg_format("%sour %s was eaten!",
                   ((obj->number > 1) ? "One of y" : "Y"),
                   o_name);

            obj->number--;
            obj_release(obj, OBJ_RELEASE_QUIET);
            break;
        }
        break;
    case RBE_EAT_LIGHT: {
        int slot;
        if (plr->is_dead || CHECK_MULTISHADOW()) break;
        slot = equip_find_obj(TV_LIGHT, SV_ANY);
        if (slot)
        {
            obj_ptr o_ptr = equip_obj(slot);
            if (o_ptr->xtra4 > 0 && !obj_is_std_art(o_ptr))
            {
                o_ptr->xtra4 -= (250 + randint1(250));
                if (o_ptr->xtra4 < 1) o_ptr->xtra4 = 1;

                if (!plr_tim_find(T_BLIND))
                    msg_print("Your light dims.");
                plr->window |= PW_EQUIP;
            }
        }
        break; }
    case RBE_LOSE_STR:
        do_dec_stat(A_STR);
        break;
    case RBE_LOSE_INT:
        do_dec_stat(A_INT);
        break;
    case RBE_LOSE_WIS:
        do_dec_stat(A_WIS);
        break;
    case RBE_LOSE_DEX:
        do_dec_stat(A_DEX);
        break;
    case RBE_LOSE_CON:
        do_dec_stat(A_CON);
        break;
    case RBE_LOSE_CHR:
        do_dec_stat(A_CHR);
        break;
    case RBE_LOSE_ALL:
        do_dec_stat(A_STR);
        do_dec_stat(A_DEX);
        do_dec_stat(A_CON);
        do_dec_stat(A_INT);
        do_dec_stat(A_WIS);
        do_dec_stat(A_CHR);
        break;
    case RBE_SHATTER: {
        int pct = ac_melee_pct(context->ac);

        dam = dam * pct / 100;
        context->dam += take_hit(DAMAGE_ATTACK, dam, context->mon_full_name);

        if (dam > 23)
            earthquake_aux(context->mon->pos, 8, context->mon->id);
        break; }
    case RBE_DRAIN_EXP:
        if (CHECK_MULTISHADOW()) break;
        dam += MON_DRAIN_LIFE * plr->exp / 100;
        if (dam > 25000) dam = 25000;
        dam = drain_exp(dam, dam/10, 95 - dam/15);
        if (dam)
            mon_gain_exp(context->mon, dam);
        break;
    case RBE_DISEASE:
        if (plr->is_dead || CHECK_MULTISHADOW()) break;
        context->dam += take_hit(DAMAGE_ATTACK, dam, context->mon_full_name);

        /* XXX should we do both immediate and delayed damage? */
        {
            int d = res_calc_dam(GF_POIS, dam);
            if (d) plr_tim_add(T_POISON, d);
        }

        if (randint1(100) < 11 && plr->prace != RACE_ANDROID)
        {
            bool perm = one_in_(10) && one_in_(100/MAX(1, mon_lvl(context->mon)));
            if (dec_stat(A_CON, randint1(10), perm))
                msg_print("You feel strange sickness.");
        }
        break;
    case RBE_VAMP:
        /* Interpretation: BITE:VAMP(3d10) is still a physically
         * damaging blow, even if the player is non-living. This
         * effect is pure vamp, no longer combining exp drain. Do
         * B:BITE:VAMP(3d10):DRAIN_EXP(40d6) if you want both. */
        dam = take_hit(DAMAGE_ATTACK, dam, context->mon_full_name);
        context->dam += dam;

        if (plr->is_dead || CHECK_MULTISHADOW()) break;
        if (!(get_race()->flags & RACE_IS_NONLIVING))
        {
            if (context->mon->hp < context->mon->maxhp)
            {
                context->mon->hp += dam;
                if (context->mon->hp > context->mon->maxhp)
                    context->mon->hp = context->mon->maxhp;
                check_mon_health_redraw(context->mon);
                if (context->mon->ml)
                    msg_format("%^s appears healthier.", context->mon_name);
            }
        }
        break;
    case RBE_CUT:
        if (!plr->no_cut) plr_tim_add(T_CUT, dam);
        break;
    default: /* using GF_* ... damage 0 is OK: B:BITE:HURT(4d4):DISENCHANT */
        gf_affect_p(who_create_mon(context->mon), effect->type, dam, GF_AFFECT_ATTACK);
        _check_race(context); /* GF_CHAOS and GF_TIME */
    }
}

static bool _is_touch(mon_blow_ptr blow)
{
    return BOOL(blow->flags & MBF_TOUCH);
}

static bool _repelled(mon_attack_ptr context)
{
    if ( plr->repel_evil
      && _1d(200) <= -context->race->align
      && !mon_save_p(context->mon, A_CHR) )
    {
        cmsg_format(TERM_L_BLUE, "%^s is repelled.", context->mon_name);
        plr_tim_subtract(T_PROT_EVIL, _1d(1 + mon_lvl(context->mon)/10));
        return TRUE;
    }
    if ( plr->repel_good
      && _1d(200) <= context->race->align
      && !mon_save_p(context->mon, A_CHR) )
    {
        cmsg_format(TERM_L_BLUE, "%^s is repelled.", context->mon_name);
        plr_tim_subtract(T_PROT_GOOD, _1d(1 + mon_lvl(context->mon)/10));
        return TRUE;
    }
    if ( plr->repel_monsters
      && _1d(200) > mon_lvl(context->mon)
      && !mon_save_p(context->mon, A_CHR) )
    {
        cmsg_format(TERM_L_BLUE, "%^s is repelled.", context->mon_name);
        return TRUE;
    }
    return FALSE;
}

bool mon_hit_plr(mon_attack_ptr context)
{
    int i;
    context->dam = 0;
    context->blow_lore = MON_BLOW_OBVIOUS;

    /* Early exit if monster misses */
    if (!mon_check_hit(context))
    {
        context->misses++;
        _miss_msg(context);
        return FALSE;
    }

    disturb(1, 0);
    context->hits++;

    /* Early exit if repelled (T_PROT_EVIL|GOOD) */
    if (_repelled(context))
    {
        mon_lore_align(context->mon);
        return FALSE;
    }

    _hit_msg(context);

    /* Early exit if monster explodes */
    if (_explode(context)) return TRUE;

    /* Apply Effects */
    for (i = 0; i < context->blow->effect_ct; i++)
    {
        mon_effect_ptr effect = &context->blow->effects[i];
        int            dam = 0;

        if (!effect->type) break;
        if (_stop_attack(context)) break;
        if (effect->pct && randint1(100) > effect->pct) continue;

        /* roll and scale the effect damage */
        dam = damroll(effect->dice.dd, effect->dice.ds);
        if (i == 0)
        {
            slay_t crit = mon_attack_crit(context->race, context->blow);
            if (crit.msg)
            {
                msg_print(crit.msg);
                dam = dam * crit.mul / 100 + crit.add;
            }
        }
        dam += effect->dice.base;
        dam = _scale(context->mon, dam);
        if (context->mon == who_mon(plr->duelist_target))
            dam -= dam/3;

        /* apply the effect */
        _effect_plr(context, effect, dam); 
        mon_lore_effect(context->mon, effect);
    }
    hex_on_dam(context->dam); /* store after each blow in case we stop */
    context->dam_total += context->dam;

    /* lore */
    if (context->dam)
        context->blow_lore |= MON_BLOW_DAMAGE;
    mon_lore_blow(context->mon, context->blow, context->blow_lore);

    if (context->after_hit_f)
        context->after_hit_f(context);
    if (_stop_attack(context)) return TRUE;

    /* Player auras if this blow touches the player */
    if (_is_touch(context->blow))
    {
        mon_on_hit_plr(context);
        if (_stop_attack(context)) return TRUE;
    }

    /* XXX Hacks processed on every hit */
    if (plr->riding && context->dam)
    {
        char m_name[MAX_NLEN];
        monster_desc(m_name, plr_riding_mon(), 0);
        if (rakuba(MIN(200, context->dam), FALSE)) /* XXX might clear plr->riding */
            msg_format("You have fallen from %s.", m_name);
    }
    if (plr->special_defense & NINJA_KAWARIMI)
    {
        if (kawarimi(FALSE))
        {
            context->stop = STOP_PLR_MOVED;
            return TRUE;
        }
    }
    return TRUE;
}

static int _aura_dam_p(void)
{
    return 2 + damroll(1 + (plr->lev / 10), 2 + (plr->lev / 10));
}

static void _check_plr_retaliate(mon_attack_ptr context)
{
    plr_attack_t ctx = {0};

    ctx.mode = PLR_HIT_RETALIATE;

    /* check for retaliatory cycles */
    if (plr_attack_current()) return;
    if (context->flags & _RETALIATE) return;

    /* check for ability */
    if (weaponmaster_get_toggle() == TOGGLE_TRADE_BLOWS)
        ctx.flags |= PAC_NO_INNATE;
    else if (mystic_get_toggle() == MYSTIC_TOGGLE_RETALIATE && plr->csp >= 7)
    {
        if (plr->csp < 7) return;
        ctx.flags |= PAC_NO_INNATE;
    }
    else if (plr->sh_retaliation)
    {
        if (plr->prace == RACE_MON_SWORD) /* death scythe */
        {
            if (!one_in_(3)) return;
            ctx.flags |= PAC_NO_INNATE;
        }
        else if (plr->monk_lvl) /* monk */
        {
            if (mon_save_p(context->mon, A_DEX)) return;
            if (plr->prace != RACE_MON_POSSESSOR && plr->prace != RACE_MON_MIMIC)
                ctx.flags |= PAC_NO_INNATE;
        }
        else if (weaponmaster_is_(WEAPONMASTER_STAVES))
            ctx.flags |= PAC_NO_INNATE;
        /* otherwise, a cloak of retaliation */
    }
    else return;

    /* check player status */
    if (plr_tim_find(T_CONFUSED) || plr_tim_find(T_PARALYZED)) return;
    if (plr->pclass != CLASS_FORCETRAINER && (plr_tim_find(T_BLIND) || !context->mon->ml)) return;
    if (plr_tim_find(T_STUN) && randint0(35) < plr_tim_amount(T_STUN)) return;

    /* retaliate! */
    if (plr_retaliate(&ctx, context->mon_pos))
    {
        equip_learn_flag(OF_AURA_REVENGE);
        if (mystic_get_toggle() == MYSTIC_TOGGLE_RETALIATE)
            sp_player(-7);
        if (_stop_attack(context)) return;
        _check_race(context);
    }
}

void mon_on_hit_plr(mon_attack_ptr context)
{
    if (context->stop) return; /* paranoia */

    if (weaponmaster_get_toggle() == TOGGLE_READIED_SHOT)
    {
        weaponmaster_do_readied_shot(context->mon);
        if (_stop_attack(context)) return;
    }

    if (plr->prace == RACE_MON_POSSESSOR || plr->prace == RACE_MON_MIMIC)
    {
        possessor_do_auras(context->mon);
        if (_stop_attack(context)) return;
        _check_race(context);
    }

    _check_plr_retaliate(context);
    if (context->stop) return;

    if (plr->sh_fire)
    {
        int dam = _aura_dam_p();
        dam = mon_res_calc_dam(context->mon, GF_FIRE, dam);
        if (dam)
        {
            dam = mon_damage_mod(context->mon, dam, FALSE);
            msg_format("%^s is <color:r>burned</color>!", context->mon_name);

            if (mon_take_hit(context->mon, dam, &context->fear, " turns into a pile of ash."))
            {
                context->stop = STOP_MON_DEAD;
                return;
            }
        }
    }

    if (plr->sh_elec)
    {
        int dam = _aura_dam_p();
        dam = mon_res_calc_dam(context->mon, GF_ELEC, dam);
        if (dam)
        {
            dam = mon_damage_mod(context->mon, dam, FALSE);
            msg_format("%^s is <color:b>zapped</color>!", context->mon_name);

            if (mon_take_hit(context->mon, dam, &context->fear, " turns into a pile of cinder."))
            {
                context->stop = STOP_MON_DEAD;
                return;
            }
        }
    }

    if (plr->sh_cold)
    {
        int dam = _aura_dam_p();
        dam = mon_res_calc_dam(context->mon, GF_COLD, dam);
        if (dam)
        {
            dam = mon_damage_mod(context->mon, dam, FALSE);
            msg_format("%^s is <color:w>frozen</color>!", context->mon_name);

            if (mon_take_hit(context->mon, dam, &context->fear, " was frozen."))
            {
                context->stop = STOP_MON_DEAD;
                return;
            }
        }
    }

    if (plr->sh_shards)
    {
        int dam = _aura_dam_p();
        dam = mon_res_calc_dam(context->mon, GF_SHARDS, dam);
        if (dam)
        {
            dam = mon_damage_mod(context->mon, dam, FALSE);
            msg_format("%^s is <color:u>shredded</color>!", context->mon_name);
            if (mon_take_hit(context->mon, dam, &context->fear," was torn to pieces."))
            {
                context->stop = STOP_MON_DEAD;
                return;
            }
        }
        if ( plr->pclass == CLASS_MIRROR_MASTER
          && plr_tim_find(T_AURA_SHARDS)
          && floor_has_mirror(dun_cell_at(cave, plr->pos)) )
        {
            teleport_player(10, 0L);
        }
    }

    if (plr->sh_fear)
    {
        gf_affect_m(who_create_plr(), context->mon, GF_FEAR, 2*plr->lev, GF_AFFECT_AURA);
        if (_stop_attack(context)) return;
    }

    if (plr->sh_holy)
    {
        int align = context->mon->race->align;
        if (align < 0)
        {
            int dam = _aura_dam_p();

            dam = dam * holy_align_dam_pct(align) / 100;
            dam = mon_damage_mod(context->mon, dam, FALSE);
            msg_format("%^s is injured by holy power!", context->mon_name);

            if (mon_take_hit(context->mon, dam, &context->fear, " is destroyed."))
            {
                context->stop = STOP_MON_DEAD;
                return;
            }
            mon_lore_align(context->mon);
        }
    }

    /* XXX move to hex.c */
    if (plr_tim_find(T_HEX_SHADOW_CLOAK))
    {
        int dam = 1;
        int slot, hand;

        for (hand = 0; hand < MAX_HANDS; hand++)
        {
            object_type *o_ptr = NULL;
            if (plr->attack_info[hand].type != PAT_NONE)
                o_ptr = equip_obj(plr->attack_info[hand].slot);
            if (o_ptr)
            {
                int dd = o_ptr->dd + plr->attack_info[hand].to_dd;
                int ds = o_ptr->ds + plr->attack_info[hand].to_ds;
                dam = dd * (ds + 1) / 2 + o_ptr->to_d + plr->attack_info[hand].to_d;
                break;
            }
        }
        slot = equip_find_first(obj_is_body_armor);
        if (slot)
        {
            object_type *o_ptr = equip_obj(slot);
            if (obj_is_cursed(o_ptr))
                dam *= 2;
        }
        dam = mon_res_calc_dam(context->mon, GF_DARK, dam);
        if (dam)
        {
            dam = mon_damage_mod(context->mon, dam, FALSE);
            msg_format("Enveloped shadows attack %^s.", context->mon_name);

            if (mon_take_hit(context->mon, dam, &context->fear, " is destroyed."))
            {
                context->stop = STOP_MON_DEAD;
                return;
            }
            else
            {
                for (slot = equip_find_first(obj_is_cursed);
                        slot;
                        slot = equip_find_next(obj_is_cursed, slot))
                {
                    object_type *o_ptr = equip_obj(slot);
                    int          effect = 0;

                    switch (equip_slot_type(slot))
                    {
                    case EQUIP_SLOT_HELMET: effect = GF_OLD_CONF; break;
                    case EQUIP_SLOT_GLOVES: effect = GF_FEAR; break;
                    case EQUIP_SLOT_BOOTS: effect = GF_SLOW; break;
                    default: if (obj_is_shield(o_ptr)) effect = GF_SLEEP;
                    }
                    if (effect)
                    {
                        gf_affect_m(who_create_plr(), context->mon, effect, plr->lev*2, GF_AFFECT_AURA);
                        if (_stop_attack(context)) return;
                    }
                }
            }
        }
    }
}

/*************************************************************************
 * Attack Monster
 *************************************************************************/
static bool _mon_gf_mon(mon_ptr who, mon_ptr tgt, int type, int dam)
{
    return gf_affect_m(who_create_mon(who), tgt, type, dam, GF_AFFECT_ATTACK);
}
static void _effect_mon(mon_attack_ptr context, mon_effect_ptr effect, int dam)
{
    int gf = GF_MISSILE;
    switch (effect->type)
    {
    case 0:
    case RBE_HURT:
        dam = dam * ac_melee_pct(context->ac) / 100;
        break;

    case RBE_DISEASE:
        gf = GF_POIS;
        break;

    case RBE_DRAIN_CHARGES:
        gf = GF_DISENCHANT;
        break;

    /* non-damaging attacks */
    case RBE_EAT_ITEM:
    case RBE_EAT_GOLD:
        if ((plr->riding != context->mon->id) && one_in_(2)) context->do_blink = TRUE;
    case RBE_EAT_FOOD:
    case RBE_EAT_LIGHT:
    case RBE_LOSE_STR:
    case RBE_LOSE_INT:
    case RBE_LOSE_WIS:
    case RBE_LOSE_DEX:
    case RBE_LOSE_CON:
    case RBE_LOSE_CHR:
    case RBE_LOSE_ALL:
    case RBE_DRAIN_EXP:
        gf = 0;
        break;

    case RBE_SHATTER:
        dam = dam * ac_melee_pct(context->ac) / 100;
        if (dam > 23)
        {
            earthquake_aux(context->mon->pos, 8, context->mon->id);
            if (_stop_attack(context)) return;
        }
        break;

    case RBE_VAMP:
        gf = GF_OLD_DRAIN;
        break;

    default:
        gf = effect->type;
        break;
    }
    if (!gf) return;

    context->dam += dam;
    _mon_gf_mon(context->mon, context->mon2, gf, dam);
    if (_stop_attack(context)) return;
    _check_race(context);
    if (effect->type == RBE_VAMP && mon_is_living(context->mon2) && dam > 2)
    {
        bool did_heal = FALSE;

        if (context->mon->hp < context->mon->maxhp) did_heal = TRUE;

        context->mon->hp += damroll(4, dam / 6);
        if (context->mon->hp > context->mon->maxhp) context->mon->hp = context->mon->maxhp;

        check_mon_health_redraw(context->mon);
        if (!(context->flags & _UNVIEW) && did_heal)
            msg_format("%^s appears healthier.", context->mon_name);
    }
}
bool mon_hit_mon(mon_attack_ptr context)
{
    int i;
    context->dam = 0;
    context->blow_lore = MON_BLOW_OBVIOUS;

    mon_tim_delete(context->mon2, MT_SLEEP); /* Even missing wakes him up */

    /* Early exit if monster misses */
    if (!mon_check_hit(context))
    {
        context->misses++;
        _miss_msg(context);
        return FALSE;
    }

    context->hits++;

    _hit_msg(context);

    /* Early exit if monster explodes */
    if (_explode(context)) return TRUE;

    /* Apply Effects */
    for (i = 0; i < context->blow->effect_ct; i++)
    {
        mon_effect_ptr effect = &context->blow->effects[i];
        int            dam = 0;

        if (!effect->type) break;
        if (_stop_attack(context)) break;
        if (effect->pct && randint1(100) > effect->pct) continue;

        /* roll and scale the effect damage */
        if (i == 0)
            dam = damroll(effect->dice.dd + context->to_dd, effect->dice.ds);
        else
            dam = damroll(effect->dice.dd, effect->dice.ds);
        if (i == 0)
        {
            slay_t crit = mon_attack_crit(context->race, context->blow);
            if (crit.msg)
            {
                if (!(context->flags & _UNVIEW))
                    msg_print(crit.msg);
                dam = dam * crit.mul / 100 + crit.add;
            }
        }
        dam += effect->dice.base;
        dam = _scale(context->mon, dam);

        /* apply the effect */
        _effect_mon(context, effect, dam); 
    }
    context->dam_total += context->dam;

    if (_stop_attack(context)) return TRUE;

    /* Monster auras */
    if (_is_touch(context->blow))
    {
        mon_on_hit_mon(context);
        if (_stop_attack(context)) return TRUE;
    }

    return TRUE;
}

void mon_on_hit_mon(mon_attack_ptr context)
{
    mon_aura_ptr aura;

    if ( mon_can_retaliate(context->mon2)
      && !(context->flags & _RETALIATE)
      && randint0(150) < context->race2->alloc.lvl )
    {
        mon_retaliate_mon(context->mon2, context->mon);
        if (_stop_attack(context)) return;
    }

    for (aura = context->race2->auras; aura; aura = aura->next)
    {
        int dam;
        if (aura->pct && randint1(100) > aura->pct) continue;
        /* XXX skip aura if resist? */
        dam = dice_roll(aura->dam);
        _mon_gf_mon(context->mon2, context->mon, aura->gf, dam);
        if (_stop_attack(context)) return;
        _check_race(context);
    }
}

/*************************************************************************
 * Normal Melee
 *************************************************************************/
static void _delayed_effects(mon_attack_ptr context)
{
    if (context->stop) return;
    if (context->do_blink)
    {
        if (plr_block_teleport(context->mon))
        {
            if (!(context->flags & _UNVIEW))
                msg_print("The thief flees laughing...? But a magic barrier obstructs it.");
        }
        else
        {
            if (!(context->flags & _UNVIEW))
                msg_print("The thief flees laughing!");
            teleport_away(context->mon, MAX_SIGHT * 2 + 5, 0);
            context->stop = STOP_MON_MOVED;
        }
    }
}
static int _get_num_blows(int blows)
{
    int result = blows / 100;
    if (randint0(100) < (blows % 100))
        result++;
    return result;
}
static void _mon_hit(mon_attack_ptr context)
{
    /* allow monsters to attack with martial arts! */
    if (context->blow->method == RBM_MONK)
    {
        mon_blow_ptr old = context->blow;
        context->blow = monk_choose_attack_mon(context->blow->name, context->mon);
        assert(context->blow);
        assert(context->blow->method != RBM_MONK);
        _mon_hit(context);
        context->blow = old;
        mon_lore_blow(context->mon, context->blow, MON_BLOW_OBVIOUS);
        return;
    }

    if (context->flags & _ATTACK_PLR)
        mon_hit_plr(context);
    else
        mon_hit_mon(context);
}
static bool _skip_blow(mon_attack_ptr context)
{
    /* a Blind Beholder cannot gaze */
    if ((context->blow->flags & MBF_MASK_BLIND) && mon_tim_find(context->mon, T_BLIND))
        return TRUE;
    /* a Bound Dragon cannot claw (but can still Bite) (Illusionist Bind Monster) */
    if ((context->blow->flags & MBF_MASK_HAND) && mon_tim_find(context->mon, MT_BOUND))
        return TRUE;
    /* XXX limit melee at a distance. Currently, only beholders have Gaze spells, and
     * all of their attacks are Gaze, so no problem. Consider a Dracolisk that could
     * project its Gaze blow at a distance ... but not its claws or bites! */
    return FALSE;
}
static void _loop(mon_attack_ptr context)
{
    int i, j, ct;
    context->hits = 0;
    context->misses = 0;
    context->dam_total = 0;
    for (i = 0; i < vec_length(context->race->blows); i++)
    {
        if (_stop_attack(context)) return;
        context->which = i;
        context->blow = vec_get(context->race->blows, i);
        if (_skip_blow(context)) continue;
        ct = _get_num_blows(context->blow->blows); /* fractional system */
        for (j = 0; j < ct; j++)
        {
            if (_stop_attack(context)) return;
            _mon_hit(context);
        }
    }
}
static bool _kawarimi(void)
{
    int odds = plr->lev*3/5 + 20;
    if (randint0(55) < odds && kawarimi(TRUE))
        return TRUE;
    return FALSE;
}
static void _pre_attack(mon_attack_ptr context) /* XXX remove these if possible, using hooks */
{
    /* Apply Dragon Songs to the player's mount */
    if (plr->riding == context->mon->id && warlock_is_(WARLOCK_DRAGONS))
    {
        switch (warlock_get_toggle())
        {
        case WARLOCK_DRAGON_TOGGLE_BLESS:
            context->to_h += 5;
            break;
        case WARLOCK_DRAGON_TOGGLE_HEROIC_CHARGE:
            context->to_h += 12;
            context->to_dd += 2;
            break;
        }
    }

    if (!(context->flags & _ATTACK_PLR)) return;

    if (plr->prace == RACE_MON_RING && ring_dominate_m(context->mon))
        context->stop = STOP_MON_FRIENDLY;
    else if (plr->special_defense & KATA_IAI) /* XXX move to samurai.c and make HISSATSU_IAI private */
    {
        plr_attack_t ctx = {0};
        context->stop = STOP_PLR_SPECIAL;
        _current = NULL;  /* <=== Allow monsters to retaliate since we pre-empted their attacks (cf plr_on_hit_mon) */
        ctx.mode = HISSATSU_IAI;
        ctx.to_h = 20;
        ctx.flags = PAC_NO_START_MSG | PAC_NO_INNATE;
        msg_format("You took sen, draw and cut in one motion before %s can move.", context->mon_name);
        plr_retaliate(&ctx, context->mon_pos);
    }
    else if ((plr->special_defense & NINJA_KAWARIMI) && _kawarimi())
        context->stop = STOP_PLR_MOVED;
}
static void _post_attack(mon_attack_ptr context)
{
    if (!(context->flags & _ATTACK_PLR)) return;

    if (plr->is_dead && context->race->lore.deaths < MAX_SHORT)
        context->race->lore.deaths++;

    if (context->stop) return;

    if (plr->revenge && context->dam_total > 0)
    {
        char m_name_self[80];
        monster_desc(m_name_self, context->mon, MD_PRON_VISIBLE | MD_POSSESSIVE | MD_OBJECTIVE);
        msg_format("The attack of %s has wounded %s!", context->mon_full_name, m_name_self);
        gf_affect_m(who_create_plr(), context->mon, GF_MISSILE, psion_backlash_dam(context->dam_total), 0);
        plr_tim_subtract(T_REVENGE, 5);
    }
    if ( (plr->counter || (plr->special_defense & KATA_MUSOU))
      && !(context->flags & _RETALIATE)
      && context->mon->ml
      && plr->csp > 7 )
    {
        plr_attack_t ctx = {0};
        sp_player(-7);
        ctx.mode = PLR_HIT_RETALIATE;
        ctx.flags = PAC_NO_START_MSG;
        msg_format("<color:v>You counterattack %s:</color>", context->mon_full_name);
        plr_attack(&ctx, context->mon_pos);
    }
    if (!context->hits && context->misses && allow_ticked_off(context->race) && one_in_(2))
    {
        context->mon->anger = MIN(100, context->mon->anger + 5 + context->mon->anger/3); 
    }
    /*if (context->fear && context->mon->ml)
        msg_format("%^s flees in terror!", context->mon_name);*/
}
bool mon_attack(mon_ptr mon, point_t pos)
{
    mon_attack_t context = {0};
    if (!mon_attack_begin(&context, mon, pos)) return FALSE;
    _pre_attack(&context);
    if (!context.stop)
    {
        if (context.flags & _ATTACK_PLR)
            cmsg_format(TERM_GREEN, "%^s attacks you:", context.mon_full_name);
        else if (!(context.flags & _UNVIEW))
            cmsg_format(TERM_GREEN, "<color:B>%^s</color> attacks <color:o>%s</color>:", context.mon_full_name, context.mon2_full_name);
        _loop(&context);
        _post_attack(&context);
        _delayed_effects(&context);
    }
    mon_attack_end(&context);
    #if 0
    if (context.flags & _ATTACK_PLR)
    {
        rect_t r = ui_char_info_rect();
        int    a = TERM_WHITE;
        if (context.dam_total > 1000) a = TERM_VIOLET;
        else if (context.dam_total > 750) a = TERM_RED;
        else if (context.dam_total > 500) a = TERM_L_RED;
        else if (context.dam_total > 250) a = TERM_YELLOW;
        else if (context.dam_total > 100) a = TERM_L_UMBER;
        c_put_str(a, format("Mon: %4d", context.dam_total), r.y + r.cy - 4, r.x);
    }
    #endif
    return TRUE;
}

/*************************************************************************
 * Retaliatory Strikes
 *************************************************************************/
static int _random_blow(mon_race_ptr race)
{
    int i, tot = 0, n;
    for (i = 0; i < vec_length(race->blows); i++)
    {
        mon_blow_ptr blow = vec_get(race->blows, i);
        tot += blow->blows;
    }
    n = randint0(tot);
    for (i = 0; i < vec_length(race->blows); i++)
    {
        mon_blow_ptr blow = vec_get(race->blows, i);
        n -= blow->blows;
        if (n < 0) return i;
    }
    return 0;
}
bool mon_retaliate_plr(mon_ptr mon)
{
    mon_attack_t context = {0};

    context.flags |= _RETALIATE | _ATTACK_PLR;
    context.mon = mon;
    _init_race(&context);
    context.mon_pos = mon->pos;
    context.tgt_pos = plr->pos;

    context.which = _random_blow(context.race);
    context.blow = vec_get(context.race->blows, context.which);

    cmsg_format(TERM_GREEN, "(%^s retaliates:", context.mon_name);
    _mon_hit(&context);
    _post_attack(&context);
    _delayed_effects(&context);
    cmsg_print(TERM_GREEN, ")");
    mon_lore_can_retaliate(mon);
    return TRUE;
}

bool mon_retaliate_mon(mon_ptr mon, mon_ptr mon2)
{
    mon_attack_t context = {0};

    context.flags |= _RETALIATE;
    context.mon = mon;
    _init_race(&context);
    context.mon_pos = mon->pos;

    context.mon2 = mon2;
    _init_race2(&context);
    context.tgt_pos = mon2->pos;
    if (!mon_show_msg(context.mon) && !mon_show_msg(context.mon2))
        context.flags |= _UNVIEW;

    context.which = _random_blow(context.race);
    context.blow = vec_get(context.race->blows, context.which);

    if (!(context.flags & _UNVIEW))
        cmsg_format(TERM_GREEN, "(%^s retaliates:", context.mon_full_name);
    _mon_hit(&context);
    _post_attack(&context);
    _delayed_effects(&context);
    if (!(context.flags & _UNVIEW))
    {
        cmsg_print(TERM_GREEN, ")");
        mon_lore_can_retaliate(mon);
    }
    return TRUE;
}

