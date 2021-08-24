#include "angband.h"

#include <assert.h>
#include "str_map.h"

static bool _gf_innate(mon_ptr mon, int effect, int dam);

static vec_ptr _stack(void)
{
    static vec_ptr _vec = NULL;
    if (!_vec) _vec = vec_alloc(NULL);
    return _vec;
}
static void _push(plr_attack_ptr context)
{
    vec_push(_stack(), context);
}
static void _pop(void)
{
    vec_pop(_stack());
}
plr_attack_ptr plr_attack_current(void)
{
    int n = vec_length(_stack());
    if (n) return vec_get(_stack(), n - 1);
    return NULL;
}

/*************************************************************************
 * Player Attack Info
 *************************************************************************/
void plr_attack_info_wipe(plr_attack_info_ptr info)
{
    memset(info, 0, sizeof(plr_attack_info_t));
    info->skill_mul = 1000;
    info->info_attr = TERM_L_GREEN;
    info->crit.qual_mul = 100;
    info->crit.freq_mul = 100;
}
bool plr_attack_info_two_hand(plr_attack_info_ptr info)
{
    if (info->type != PAT_WEAPON && info->type != PAT_MONK) return FALSE;
    if (!have_flag(info->paf_flags, PAF_TWO_HANDS)) return FALSE;
    return TRUE;
}
bool plr_attack_info_two_hand_bonus(plr_attack_info_ptr info)
{
    if (plr->pclass == CLASS_DUELIST) return FALSE;
    if (!plr_attack_info_two_hand(info)) return FALSE;
    if (have_flag(info->paf_flags, PAF_NEEDS_TWO_HANDS)) return FALSE;
    return TRUE;
}
/*************************************************************************
 * Hit Testing
 *************************************************************************/
static int _hit_per_mil(int skill, int ac, bool vis)
{
    int odds;
    if (!vis) skill = (skill + 1)/2;
    if (plr_tim_find(T_STUN))
        skill -= skill * MIN(100, plr_tim_amount(T_STUN)) / 150;
    if (skill <= 0) return 0;

    odds = 95*(skill - ac*3/4)*1000/(skill*100);
    if (odds < 50) odds = 50;
    return odds;
}
static int _hit_pct(int skill, int ac, bool vis)
{
    int per_mil = _hit_per_mil(skill, ac, vis);
    return (per_mil + 5)/10;
}
/* XXX test_hit_norm */
/*************************************************************************
 * Private helpers
 *************************************************************************/
static int _get_num_blows(plr_attack_ptr context)
{
    int result;
    int num_blow;
   
    if (context->info.type == PAT_INNATE)
    {
        num_blow = context->blow->blows;
        if (context->which_blow == 0)
            num_blow += context->info.xtra_blow;
        if (context->blow->method == RBM_MONK && heavy_armor())
            num_blow /= 2;
    }
    else
        num_blow = context->info.base_blow + context->info.xtra_blow;

    result = num_blow / 100;

    if (randint0(100) < (num_blow % 100))
        result++;

    return result;
}
static int _max_vampiric_drain(void)
{
    if (prace_is_(RACE_MON_VAMPIRE) || prace_is_(MIMIC_BAT))
        return 100;
    /* XXX note that dragon forms attack with claws and bites, and
     * the vampiric counter gets reset for each form on innate attack (plr_attack_mon) */
    if (prace_is_(RACE_MON_DRAGON) || prace_is_(RACE_MON_HOUND))
        return 30;
    if (mut_present(MUT_DRACONIAN_METAMORPHOSIS))
        return 30;
    if (plr->pclass == CLASS_HIGH_PRIEST && plr->realm1 == REALM_DEATH)
        return 100;
    return 50;
}
static bool _penetrate_invuln(plr_attack_ptr context)
{
    if (context->obj)
    {
        if (object_is_(context->obj, TV_POLEARM, SV_DEATH_SCYTHE))
            return TRUE;
        /* XXX if (context->mode == WEAPONMASTER_CRUSADERS_STRIKE)
            return TRUE; */
    }
    return FALSE;
}
static void _mon_damage_mod(plr_attack_ptr context)
{
    if (context->obj)
    {
        if ( obj_is_specified_art(context->obj, "|.Zantetsuken")
          && mon_race_is_char(context->race, 'j'))
        {
            msg_print("You cannot cut such a elastic thing!");
            context->dam = 0;
        }
        if ( obj_is_specified_art(context->obj, "|.Excalibur Jr")
          && mon_race_is_char(context->race, 'S'))
        {
            msg_print("Spiders are difficult for you to deal with!");
            context->dam /= 2;
        }
    }
    context->dam = mon_damage_mod(context->mon, context->dam, _penetrate_invuln(context));
}
static bool _is_fatal_blow(plr_attack_ptr context)
{
    int dam = context->dam + context->info.to_d;
    return dam > context->mon->hp;
}
static bool _stop_fear(plr_attack_ptr context)
{
    if (plr->afraid && !fear_allow_melee(context->mon))
    {
        if (context->mon->ml)
            msg_format("You are too afraid to attack %s!", context->mon_name_obj);
        else
            msg_format("There is something scary in your way!");

        context->stop = STOP_PLR_FEAR;
        return TRUE;
    }
    return FALSE;
}
static bool _stop_attack(plr_attack_ptr context)
{
    if (context->stop) return TRUE;
    if (plr->is_dead)
        context->stop = STOP_PLR_DEAD;
    else if (plr->leaving)
        context->stop = STOP_PLR_LEAVING;
    else if (!mon_is_valid(context->mon))
        context->stop = STOP_MON_DEAD;
    else if (plr_tim_amount(T_STUN) >= STUN_KNOCKED_OUT || plr_tim_find(T_PARALYZED))
        context->stop = STOP_PLR_PARALYZED;
    /* XXX else if (plr_tim_find(T_CONFUSED))
        context->stop = STOP_PLR_CONFUSED; */
    else if (!point_equals(context->mon->pos, context->mon_pos))
        context->stop = STOP_MON_MOVED;
    else if (!point_equals(plr->pos, context->plr_pos))
        context->stop = STOP_PLR_MOVED;
    return context->stop != STOP_CONTINUE;
}
static bool _icky_weapon(plr_attack_ptr context)
{
    return have_flag(context->info.paf_flags, PAF_ICKY);
}
static void _plr_custom_init(plr_attack_ptr context)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.attack_init) r->hooks.attack_init(context);
    if (c->hooks.attack_init) c->hooks.attack_init(context);
}
static void _init_race(plr_attack_ptr context)
{
    context->race = context->mon->race;
    monster_desc(context->mon_full_name, context->mon, 0);
    monster_desc(context->mon_name, context->mon, MD_PRON_VISIBLE);
    monster_desc(context->mon_name_obj, context->mon, MD_PRON_VISIBLE | MD_OBJECTIVE);
}
static void _check_race(plr_attack_ptr context) /* GF_CHAOS might polymorph */
{
    if (context->race->id != context->mon->race->id)
        _init_race(context);
}
static bool _is_two_hand_bonus(plr_attack_ptr context)
{
    if (!plr_attack_info_two_hand_bonus(&context->info)) return FALSE;
    return TRUE;
}
static void _set_target(mon_ptr mon)
{
    plr->target = who_create_mon(mon);
    plr->redraw |= PR_HEALTH_BARS;
}
static void _animate(plr_attack_ptr context)
{
    if (!context->obj) return;
    if (cave_pt_is_visible(context->mon_pos) && plr_can_see(context->mon_pos))
    {
        char c = object_char(context->obj);
        byte a = object_attr(context->obj);
        int x = context->mon_pos.x;
        int y = context->mon_pos.y;

        print_rel(c, a, y, x);
        move_cursor_relative(context->mon_pos);
        Term_fresh();
        Term_xtra(TERM_XTRA_DELAY, delay_animation);
        draw_pos(context->mon_pos);
        Term_fresh();
    }
    else
    {
        /* Pause anyway, for consistancy */
        Term_xtra(TERM_XTRA_DELAY, delay_animation);
    }
}
static int _fractional_energy_use(plr_attack_ptr context)
{
    int i, blows = 0;
    if (!(context->flags & PAC_NO_WEAPON))
    {
        for (i = 0; i < MAX_HANDS; i++)
        {
            if (plr->attack_info[i].type != PAT_NONE)
            {
                blows += plr->attack_info[i].base_blow;
                blows += plr->attack_info[i].xtra_blow;
            }
        }
    }
    if (!(context->flags & PAC_NO_INNATE))
    {
        for (i = 0; i < vec_length(plr->innate_blows); i++)
        {
            mon_blow_ptr blow = vec_get(plr->innate_blows, i);
            blows += blow->blows;
        }
    }
    if (context->counter * 100 >= blows) return context->energy;
    return context->counter * 100 * context->energy / blows;
}
/*************************************************************************
 * Poison Needle: A pretty big hack that attempts to kill a monster
 * in a single blow, doing just 1hp damage otherwise. Usually, the
 * player wields a poison needle, but there are samurai and mystic
 * abilities to do this as well. Only works for normal combat (weapon
 * or bare handed).
 *************************************************************************/
static bool _is_poison_needle(plr_attack_ptr context)
{
    if (context->info.type == PAT_INNATE) return FALSE;
    if (object_is_(context->obj, TV_SWORD, SV_POISON_NEEDLE)) return TRUE;
    if (context->mode == PLR_HIT_KILL) return TRUE;
    return FALSE;
}
static bool _poison_needle_hit(plr_attack_ptr context)
{
    return one_in_(plr->weapon_ct);
}
static bool _poison_needle_kill(plr_attack_ptr context)
{
    int n;
    if (mon_race_is_unique(context->race)) return FALSE;
    n = randint1(context->race->alloc.lvl/7) + 5;
    return one_in_(n);
}
static bool _do_poison_needle(plr_attack_ptr context)
{
    if (!_is_poison_needle(context)) return FALSE;
    if (_stop_attack(context)) return TRUE;
    if (_stop_fear(context)) return TRUE;
    context->counter++;
    if (!_poison_needle_hit(context))
    {
        context->misses++;
        msg_format("You miss.");
        return TRUE;
    }
    if (_poison_needle_kill(context))
    {
        context->dam = context->mon->hp + 1;
        msg_format("You hit %s on a fatal spot!", context->mon_name_obj);
    }
    else
    {
        context->dam = 1;
        msg_print("You hit.");
    }
    context->hits++;
    context->dam_total += context->dam;
    if (mon_take_hit(context->mon, context->dam, &context->fear, NULL))
        context->stop = STOP_MON_DEAD;
    else
        plr_on_hit_mon(context);
    return TRUE;
}

/*************************************************************************
 * Criticals
 * W + S <= 1dR means a critical hit.
 * W + 1dQ gives crit quality via table lookup
 *   where W is "weight", S is "skill", R is constant (CRIT_FREQ_ROLL)
 *   and so is Q (CRIT_QUAL_ROLL).
 *************************************************************************/
#define _CRIT_TBL_SIZE   5
typedef struct {
    int low;
    int high;
    slay_t slay;
} _crit_info_t, *_crit_info_ptr;
static _crit_info_t _crit_tbl[_CRIT_TBL_SIZE] = {
    {    0,  399, {OF_CRIT, 200,  5, "It was a <color:y>good</color> hit!"} },
    {  400,  699, {OF_CRIT, 200, 10, "It was a <color:R>great</color> hit!"} },
    {  700,  899, {OF_CRIT, 300, 15, "It was a <color:r>superb</color> hit!"} },
    {  900, 1299, {OF_CRIT, 300, 20, "It was a <color:v>*GREAT*</color> hit!"} },
    { 1300,99999, {OF_CRIT, 350, 25, "It was a <color:v>*SUPERB*</color> hit!"} },
};
static _crit_info_ptr _crit_find(int roll)
{
    int i;
    for (i = 0; i < _CRIT_TBL_SIZE; i++)
    {
        _crit_info_ptr info = &_crit_tbl[i];
        if (info->low <= roll && roll <= info->high) return info;
    }
    return NULL;
}
int crit_chance_aux(int roll, int weight, int skill)
{
    int threshold = weight + skill;
    if (roll <= threshold) return 1000; /* handles roll == 0 */
    return  threshold * 1000 / roll;
}
slay_t crit_aux(int roll, int weight, int skill)
{
    slay_t crit = {0};
    if (randint0(1000) < crit_chance_aux(roll, weight, skill))
    {
        int k = weight + randint1(CRIT_QUAL_ROLL);
        _crit_info_ptr info = _crit_find(k);
        assert(info);
        crit = info->slay;
    }
    return crit;
}
/*************************************************************************
 * Criticals: Monster and Player Innate Attacks (and Possessors)
 *************************************************************************/
/* we use body weight to calculate a monster attack weight. for monsters,
 * this is set in r_info, but for players, we'll just fudge one unless the
 * player is possessing/mimicking or actually is a monster himself! */
static int _plr_weight(void)
{
    int wgt = 170;
    if (plr->current_r_idx)
        wgt = plr_mon_race()->weight;
    return wgt;
}
static int _mon_blow_weight(mon_blow_ptr blow, int wgt)
{
    if (blow->weight) return blow->weight; /* specify in r_info: HIT(50lbs) */
    switch (blow->method)
    {
    case RBM_HIT:
        if (mon_blow_base_effect(blow) == RBE_SHATTER) /* make an exception for SHATTER effects */
            return 300 + 300*MIN(wgt, 2000)/2000;
    case RBM_PUNCH:
    case RBM_CLAW:
    case RBM_SLASH:
    case RBM_PECK:
        return 100 + 100*MIN(wgt, 5000)/2000;
    case RBM_KICK:
    case RBM_BITE:
    case RBM_BUTT:
        return 200 + 200*MIN(wgt, 3000)/2000;
    case RBM_CRUSH:
        return 300 + 300*MIN(wgt, 2000)/2000;
    }
    return 50;
}
slay_t mon_attack_crit(mon_race_ptr race, mon_blow_ptr blow)
{
    slay_t crit = {0};
    if (mon_blow_allow_crit(blow))
    {
        int skill = mon_race_skill_thn(race) + blow->power;
        int weight = _mon_blow_weight(blow, race->weight);
        crit = crit_aux(CRIT_FREQ_ROLL, weight, skill);
    }
    return crit;
}
/*************************************************************************
 * Criticals: Player
 *************************************************************************/
slay_t plr_crit(int roll, int weight, int skill, plr_crit_ptr scale)
{
    slay_t result = {0};
    if (randint0(1000) < plr_crit_chance(roll, weight, skill, scale))
    {
        int k;
        _crit_info_ptr info;

        if (scale)
        {
            k = weight * scale->qual_mul / 100;
            k += _1d(CRIT_QUAL_ROLL + scale->qual_add);
        }
        else
        {
            k = weight + _1d(CRIT_QUAL_ROLL);
        }
        info = _crit_find(k);
        assert(info);
        result = info->slay;
    }
    return result;
}
int plr_crit_chance(int roll, int weight, int skill, plr_crit_ptr scale)
{
    int threshold = weight + skill;
    if (scale)
    {
        threshold = threshold * scale->freq_mul / 100;
        if (scale->freq_add)
            threshold += scale->freq_add * roll / 1000;
    }
    if (roll <= threshold) return 1000; /* handles roll == 0 */
    return  threshold * 1000 / roll;
}
slay_t plr_crit_avg(int roll, int weight, plr_crit_ptr scale, int crit_chance)
{
    slay_t crit;
    if (scale)
    {
        weight = weight * scale->qual_mul / 100;
        roll = roll + scale->qual_add;
    }
    crit = crit_avg(roll, weight);
    crit.mul = (1000 - crit_chance)*100/1000 + (crit_chance*crit.mul)/1000;
    crit.add = crit_chance*crit.add/1000;
    return crit;
}
static int _plr_crit_freq_roll(plr_attack_ptr context)
{
    int roll = CRIT_FREQ_ROLL;
    if (context->mode == PLR_HIT_CRIT)
        roll = 0;
    else if (_is_two_hand_bonus(context))
        roll = roll*4/5;
    return roll;
}
static int _plr_crit_weight(plr_attack_ptr context)
{
    int weight = 100;
    if (context->obj)
        weight = context->obj->weight;
    else if (context->blow)
        weight = _mon_blow_weight(context->blow, _plr_weight());
    return weight;
}
static bool _plr_allow_crit(plr_attack_ptr context)
{
    if (context->blow) return mon_blow_allow_crit(context->blow);
    return TRUE;
}
static int _plr_crit_chance(plr_attack_ptr context)
{
    if (!_plr_allow_crit(context)) return 0;
    return plr_crit_chance(_plr_crit_freq_roll(context), _plr_crit_weight(context), context->skill, &context->info.crit);
}
static slay_t _apply_crits(plr_attack_ptr context)
{
    slay_t crit = {0};
    if (_plr_allow_crit(context))
    {
        int roll = _plr_crit_freq_roll(context);
        int weight = _plr_crit_weight(context);

        crit = plr_crit(roll, weight, context->skill, &context->info.crit);
        if (crit.msg)
        {
            context->dam = context->dam * crit.mul/100;
            context->dam_xtra += crit.add;
            msg_print(crit.msg);
        }
    }
    return crit;
}
slay_t crit_avg(int roll, int weight)
{
    int i;
    slay_t crit = {0};
    /* this only works for a "weight + randint1(roll)" quality roll */
    for (i = 0; i < _CRIT_TBL_SIZE; i++)
    {
        _crit_info_t info = _crit_tbl[i]; /* copy */
        info.low -= weight; /* shift */
        info.high -= weight;
        if (info.high < 0) continue; /* too weak for this roll */
        if (roll < info.low) break; /* too strong for this roll */
        if (info.low < 0) info.low = 0; /* low end is off range */
        if (roll < info.high) /* weight this multiplier by # of ways */
        {
            crit.mul += info.slay.mul * (roll - info.low + 1);
            crit.add += info.slay.add * (roll - info.low + 1);
        }
        else
        {
            crit.mul += info.slay.mul * (info.high - info.low + 1);
            crit.add += info.slay.add * (info.high - info.low + 1);
        }
    }
    crit.mul /= roll;
    crit.add = crit.add * 100 / roll;
    return crit;
}
int mon_crit_chance(mon_race_ptr race, mon_blow_ptr blow)
{
    if (mon_blow_allow_crit(blow))
    {
        int skill = mon_race_skill_thn(race) + blow->power;
        int weight = _mon_blow_weight(blow, race->weight);
        return crit_chance_aux(CRIT_FREQ_ROLL, weight, skill);
    }
    return 0;
}
int mon_crit_avg_mul(mon_race_ptr race, mon_blow_ptr blow)
{
    if (mon_blow_allow_crit(blow))
    {
        int skill = mon_race_skill_thn(race) + blow->power;
        int weight = _mon_blow_weight(blow, race->weight);
        int chance = crit_chance_aux(CRIT_FREQ_ROLL, weight, skill);
        slay_t crit = crit_avg(CRIT_QUAL_ROLL, weight);
        return (1000 - chance)*100/1000 + (chance*crit.mul)/1000;
    }
    return 100;
}
static slay_t _plr_crit_avg(plr_attack_ptr context)
{
    int chance = _plr_crit_chance(context);
    int roll = CRIT_QUAL_ROLL + context->info.crit.qual_add;
    int weight = _plr_crit_weight(context) * context->info.crit.qual_mul / 100;
    slay_t crit = crit_avg(roll, weight);
    crit.mul = (1000 - chance)*100/1000 + chance*crit.mul/1000;
    crit.add = chance*crit.add/1000;
    return crit;
}

/*************************************************************************
 * Vorpals
 *************************************************************************/
static int _innate_vorpal_pct(mon_blow_ptr blow)
{
    int i;
    /* Assume CUT is never the first effect ... */
    for (i = 1; i < blow->effect_ct; i++)
    {
        if (blow->effects[i].type == RBE_CUT)
        {
            int chance = blow->effects[i].pct;
            if (!chance) chance = 100;
            return chance;
        }
    }
    return 0;
}
static int _vorpal_chance(plr_attack_ptr context)
{
    if (context->blow) return 4; /* innate or monk */
    if (!context->obj) return 0; /* paranoia */
    if ( obj_is_specified_art(context->obj, "|.Zantetsuken")
      && mon_race_is_char(context->race, 'j') )
    {
        return 0;
    }
    if (have_flag(context->obj_flags, OF_VORPAL2)) return 2;
    if (have_flag(context->obj_flags, OF_VORPAL) && plr->vorpal) return 3;
    if (have_flag(context->obj_flags, OF_VORPAL) || plr->vorpal) return 4;
    return 0;
}
static bool _is_vorpal_hit(plr_attack_ptr context)
{
    int chance = _vorpal_chance(context);
    if (context->blow) /* monks? */
        return randint0(100) < _innate_vorpal_pct(context->blow);
    if (!chance) return FALSE;
    return one_in_(chance*3/2);
}

static void _apply_vorpal(plr_attack_ptr context)
{
    if (_is_vorpal_hit(context))
    {
        int chance = _vorpal_chance(context);
        int mult = 2;

        /* boost the damage */
        while (one_in_(chance))
            mult++;
        context->dam *= mult;

        /* describe the hit */
        if (_is_fatal_blow(context))
        {
            if (context->blow && (context->blow->flags & MBF_MONK) && mon_is_living(context->mon))
            {
                char buf[MAX_NLEN];
                monster_desc(buf, context->mon, MD_PRON_VISIBLE | MD_POSSESSIVE);
                cmsg_format(TERM_VIOLET, "You rip %s heart out!", buf);
            }
            else if (context->blow && (context->blow->flags & MBF_MONK))
                cmsg_format(TERM_VIOLET, "You tear %s to pieces!", context->mon_name_obj);
            else
                cmsg_format(TERM_VIOLET, "You cut %s in half!", context->mon_name_obj);
        }
        else
        {
            switch (mult)
            {
            case 2: msg_format("You <color:U>gouge</color> %s!", context->mon_name_obj); break;
            case 3: msg_format("You <color:y>maim</color> %s!", context->mon_name_obj); break;
            case 4: msg_format("You <color:R>carve</color> %s!", context->mon_name_obj); break;
            case 5: msg_format("You <color:r>cleave</color> %s!", context->mon_name_obj); break;
            case 6: msg_format("You <color:v>smite</color> %s!", context->mon_name_obj); break;
            case 7: msg_format("You <color:v>eviscerate</color> %s!", context->mon_name_obj); break;
            default: msg_format("You <color:v>shred</color> %s!", context->mon_name_obj); break;
            }
        }

        /* lore */
        if (context->obj)
        {
            if (have_flag(context->obj_flags, OF_VORPAL2))
                obj_learn_slay(context->obj, OF_VORPAL2, "is <color:v>*Sharp*</color>");
            else
                obj_learn_slay(context->obj, OF_VORPAL, "is <color:R>Sharp</color>");
        }
    }
}

/*************************************************************************
 * Vampiric
 *************************************************************************/
static bool _is_vampiric(plr_attack_ptr context)
{
    if (!mon_is_living(context->mon)) return FALSE;
    if (_is_fatal_blow(context)) return FALSE;
    if (have_flag(context->obj_flags, OF_BRAND_VAMP)) return TRUE;
    return FALSE;
}

static bool _muramasa_drain(plr_attack_ptr context)
{
    if (context->obj && obj_is_specified_art(context->obj, "|.Muramasa")) 
    {
        if (mon_race_is_char(context->race, 'p'))
        {
            int to_h = context->obj->to_h;
            int to_d = context->obj->to_d;
            int i, flag;

            flag = 1;
            for (i = 0; i < to_h + 3; i++) if (one_in_(4)) flag = 0;
            if (flag) to_h++;

            flag = 1;
            for (i = 0; i < to_d + 3; i++) if (one_in_(4)) flag = 0;
            if (flag) to_d++;

            if (context->obj->to_h != to_h || context->obj->to_d != to_d)
            {
                msg_print("Muramasa sucked blood, and became more powerful!");
                context->obj->to_h = to_h;
                context->obj->to_d = to_d;
            }
        }
        return TRUE;
    }
    return FALSE;
}

static void _apply_vampiric(plr_attack_ptr context)
{
    if (!_is_vampiric(context)) return;
    if (_muramasa_drain(context)) return;

    if (context->dam_drain > 5) /* Did we really hurt it? */
    {
        int drain_heal = damroll(2, context->dam_drain / 6);

        if (plr_tim_find(T_HEX_VAMP_BLADE)) drain_heal *= 2;
        if (prace_is_(RACE_MON_VAMPIRE)) drain_heal *= 2;

        context->drain_hits++;
        if (context->drain_left)
        {
            if (drain_heal < context->drain_left)
                context->drain_left -= drain_heal;
            else
            {
                drain_heal = context->drain_left;
                context->drain_left = 0;
            }

            /* XXX Not sure why, but old code only messaged the first drain life strike. */
            if (context->drain_hits == 1)
            {
                if (context->obj)
                    msg_format("<color:D>Your weapon drains life from %s!</color>", context->mon_name_obj);
                else
                    msg_format("<color:D>You drain life from %s!</color>", context->mon_name_obj);
            }
            drain_heal = (drain_heal * mutant_regenerate_mod) / 100;
            vamp_player(drain_heal);
            if (context->obj)
                obj_learn_slay(context->obj, OF_BRAND_VAMP, "is <color:D>Vampiric</color>");
            else
                equip_learn_slay(OF_BRAND_VAMP, "is <color:D>Vampiric</color>");
        }
    }
}

/*************************************************************************
 * Slay and Brand Tables
 *************************************************************************/
static bool _mon_is_evil(mon_ptr mon) { return mon->race->align < 0; }
static bool _mon_is_good(mon_ptr mon) { return mon->race->align > 0; }

slay_info_t slay_tbl[] = {
    /* slays: lore_f applies when check_p succeeds */
    {OF_KILL_ANIMAL, mon_is_animal, mon_lore_animal, 300, 10, "Animals", "slays <color:g>*Animals*</color>"},
    {OF_SLAY_ANIMAL, mon_is_animal, mon_lore_animal, 200,  5, "Animals", "slays <color:g>Animals</color>", 0, OF_KILL_ANIMAL},
    {OF_KILL_DEMON,  mon_is_demon,  mon_lore_demon,  400, 12, "Demons",  "slays <color:R>*Demons*</color>"},
    {OF_SLAY_DEMON,  mon_is_demon,  mon_lore_demon,  250,  6, "Demons",  "slays <color:R>Demons</color>", 0, OF_KILL_DEMON},
    {OF_KILL_DRAGON, mon_is_dragon, mon_lore_dragon, 400, 10, "Dragons", "slays <color:r>*Dragons*</color>"},
    {OF_SLAY_DRAGON, mon_is_dragon, mon_lore_dragon, 250,  5, "Dragons", "slays <color:r>Dragons</color>", 0, OF_KILL_DRAGON},
    {OF_KILL_EVIL,   _mon_is_evil,  mon_lore_align,  200, 14, "Evil",    "slays <color:y>*Evil*</color>"},
    {OF_SLAY_EVIL,   _mon_is_evil,  mon_lore_align,  150,  7, "Evil",    "slays <color:y>Evil</color>", 0, OF_KILL_EVIL},
    {OF_KILL_GIANT,  mon_is_giant,  mon_lore_giant,  400, 10, "Giants",  "slays <color:u>*Giants*</color>"},
    {OF_SLAY_GIANT,  mon_is_giant,  mon_lore_giant,  250,  5, "Giants",  "slays <color:u>Giants</color>", 0, OF_KILL_GIANT},
    {OF_SLAY_GOOD,   _mon_is_good,  mon_lore_align,  150,  6, "Good",    "slays <color:W>Good</color>"},
    {OF_KILL_HUMAN,  mon_is_human,  mon_lore_human,  300, 10, "Humans",  "slays <color:s>*Humans*</color>"},
    {OF_SLAY_HUMAN,  mon_is_human,  mon_lore_human,  200,  5, "Humans",  "slays <color:s>Humans</color>", 0, OF_KILL_HUMAN},
    {OF_SLAY_LIVING, mon_is_living, mon_lore_living, 150,  4, "Living",  "slays <color:o>Living</color>"},
    {OF_KILL_ORC,    mon_is_orc,    mon_lore_orc,    400, 16, "Orcs",    "slays <color:U>*Orcs*</color>"},
    {OF_SLAY_ORC,    mon_is_orc,    mon_lore_orc,    250,  8, "Orcs",    "slays <color:U>Orcs</color>", 0, OF_KILL_ORC},
    {OF_KILL_TROLL,  mon_is_troll,  mon_lore_troll,  400, 16, "Trolls",  "slays <color:g>*Trolls*</color>"},
    {OF_SLAY_TROLL,  mon_is_troll,  mon_lore_troll,  250,  8, "Trolls",  "slays <color:g>Trolls</color>", 0, OF_KILL_TROLL},
    {OF_KILL_UNDEAD, mon_is_undead, mon_lore_undead, 400, 12, "Undead",  "slays <color:D>*Undead*</color>"},
    {OF_SLAY_UNDEAD, mon_is_undead, mon_lore_undead, 250,  6, "Undead",  "slays <color:D>Undead</color>", 0, OF_KILL_UNDEAD},
    {0}
};

slay_info_t brand_tbl[] = {
    /* XXX brands are easier if we use a gf code rather than predicates XXX */
    {OF_BRAND_ACID,  NULL, NULL, 200,  7, "Acid", "is <color:g>Acid Branded</color>", GF_ACID},
    {OF_BRAND_ELEC,  NULL, NULL, 200, 10, "Elec", "is <color:b>Lightning Branded</color>", GF_ELEC},
    {OF_BRAND_FIRE,  NULL, NULL, 200,  5, "Fire", "has <color:r>Flame Tongue</color>", GF_FIRE},
    {OF_BRAND_COLD,  NULL, NULL, 200,  5, "Cold", "is <color:W>Frost Branded</color>", GF_COLD},
    {OF_BRAND_POIS,  NULL, NULL, 200,  5, "Poison", "has <color:G>Viper's Fang</color>", GF_POIS},
    {OF_BRAND_LIGHT,  NULL, NULL, 150,  7, "Light", "is <color:y>Light Branded</color>", GF_LIGHT},
    {OF_BRAND_DARK,  NULL, NULL, 150,  6, "Dark", "is <color:D>Dark Branded</color>", GF_DARK},
    {OF_BRAND_PLASMA, NULL, NULL, 150, 10, "Plasma", "is <color:R>Plasma Branded</color>", GF_PLASMA},
    {0}
};
slay_t slay_info_apply(slay_info_ptr info, mon_ptr mon, obj_ptr obj)
{
    slay_t result = {0};
    int res_pct = 0;
    assert(info);

    /* check if slay or brand applies */
    if (mon)
    {
        if (info->gf) /* brands lore when they fail (resistance) */
        {
            res_pct = mon_res_pct(mon, info->gf);
            if (res_pct) mon_lore_resist(mon, info->gf);
            if (res_pct > 0) /* any amount of resistance negates the brand */
                return result;
        }
        else if (info->check_p && !info->check_p(mon)) /* slay */
        {
            return result;
        }
    }

    /* successful slay */
    result.id = info->id;
    result.name = info->name;
    result.mul = info->mul;
    result.add = info->add;

    /* apply slay lore and check for vulnerabilities */
    if (mon)
    {
        if (info->lore_f) /* slays lore when they work */
        {
            assert(info->gf == GF_NONE);
            switch (info->id)
            {
            case OF_SLAY_EVIL:
            case OF_KILL_EVIL:
                slay_scale(&result, holy_align_dam_pct(mon->race->align) - 100);
                break;
            case OF_SLAY_GOOD:
                slay_scale(&result, hell_align_dam_pct(mon->race->align) - 100);
                break;
            }
            info->lore_f(mon);
        }
        obj_learn_slay(obj, info->id, info->lore_msg);
        if (res_pct < 0)
            slay_scale(&result, 100 - res_pct); /* -100% vuln gives 200% scale */
    }
    return result;
}
static slay_info_ptr _slay_lookup(int id)
{
    static int_map_ptr _map = NULL;
    if (!_map)
    {
        int i;
        _map = int_map_alloc(NULL);
        for (i = 0; ; i++)
        {
            slay_info_ptr info = &slay_tbl[i];
            if (!info->id) break;
            int_map_add(_map, info->id, info);
        }
        for (i = 0; ; i++)
        {
            slay_info_ptr info = &brand_tbl[i];
            if (!info->id) break;
            int_map_add(_map, info->id, info);
        }
    }
    return int_map_find(_map, id);
}
slay_t slay_lookup(int id)
{
    slay_t slay = {0};
    slay_info_ptr info = _slay_lookup(id);
    if (info)
        slay = slay_info_apply(info, NULL, NULL);
    return slay;
}
int slay_compare(slay_ptr left, slay_ptr right)
{
    if (left->mul < right->mul) return -1;
    if (left->mul > right->mul) return 1;
    if (left->add < right->add) return -1;
    if (left->add > right->add) return 1;
    return 0;
}
void slay_scale(slay_ptr slay, int pct)
{
    assert(pct >= 0);
    if (pct == 100) return;
    if (pct <= 0) /* paranoia */
    {
        slay->mul = 100;
        slay->add = 0;
    }
    else
    {
        /* e.g. scale(250, 200) -> 400 slay */
        slay->mul -= 100;
        slay->mul = slay->mul * pct / 100;
        slay->mul += 100;
        slay->add = slay->add * pct / 100;
    }
}
/*************************************************************************
 * Slays: Check them all and call custom hooks. Allow code to be
 * used by shooting and throwing in addition to melee. This complicates
 * the interface a bit.
 *************************************************************************/
slay_t slay_apply_force(slay_t slay, obj_ptr obj, int cost, bool display)
{
    caster_info *caster = get_caster_info();
    bool         ok = FALSE;

    if (caster && (caster->options & CASTER_USE_AU))
    {
        cost *= 10;
        if (plr->au >= cost)
        {
            if (!display)
            {
                plr->au -= cost;
                stats_on_gold_services(cost); /* ? */
                plr->update |= PU_BONUS | PU_HP | PU_MANA;
                plr->redraw |= PR_GOLD;
                obj_learn_slay(obj, OF_BRAND_MANA, "is <color:B>Mana Branded</color>");
            }
            ok = TRUE;
        }
    }
    else if (plr->csp >= cost)
    {
        if (!display)
        {
            plr->csp -= cost;
            plr->redraw |= PR_MANA;
            obj_learn_slay(obj, OF_BRAND_MANA, "is <color:B>Mana Branded</color>");
        }
        ok = TRUE;
    }
    if (ok) slay.mul = slay.mul*3/2 + 150;
    return slay;
}
slay_t slay_find_best(u32b flags[OF_ARRAY_SIZE], mon_ptr mon, obj_ptr obj, slay_info_ptr tbl)
{
    slay_t best = {0};
    int i;
    for (i = 0; ; i++)
    {
        slay_info_ptr info = &tbl[i];
        if (!info->id) break;
        if (have_flag(flags, info->id))
        {
            slay_t tmp = slay_info_apply(info, mon, obj);
            if (!tmp.id) continue;
            if (slay_compare(&tmp, &best) > 0)
                best = tmp;
        }
    }
    return best;
}
bool obj_slays_mon(obj_ptr obj, mon_ptr mon)
{
    /* used by dun_mon_pickup and dun_mon_destroy to skip objects
     * that slay the monster in question w/o spoiling lore */
    u32b flags[OF_ARRAY_SIZE];
    int i;
    obj_flags(obj, flags);
    for (i = 0; ; i++)
    {
        slay_info_ptr info = &slay_tbl[i];
        if (!info->id) break;
        if (!have_flag(flags, info->id)) continue;
        if (info->gf && mon_res_pct(mon, info->gf) <= 0) return TRUE;
        if (info->check_p && info->check_p(mon)) return TRUE;
    }
    for (i = 0; ; i++)
    {
        slay_info_ptr info = &brand_tbl[i];
        if (!info->id) break;
        if (!have_flag(flags, info->id)) continue;
        if (info->gf && mon_res_pct(mon, info->gf) <= 0) return TRUE;
        if (info->check_p && info->check_p(mon)) return TRUE;
    }
    return FALSE;
}
slay_t slay_mon(u32b flags[OF_ARRAY_SIZE], mon_ptr mon, obj_ptr obj)
{
    slay_t slay = {0};
    slay_t best_slay = slay_find_best(flags, mon, obj, slay_tbl);
    slay_t best_brand = slay_find_best(flags, mon, obj, brand_tbl);
    if (best_slay.id)
    {
        slay = best_slay;
        if (best_brand.id)
        {
            slay.mul += best_brand.mul - 100;
            slay.add += best_brand.add;
        }
    }
    else if (best_brand.id)
        slay = best_brand;

    if (!slay.id) slay.mul = 100;
    if (have_flag(flags, OF_BRAND_MANA))
    {
        int cost = 5;
        if (obj) cost = 1 + obj->dd*obj->ds/2;

        slay = slay_apply_force(slay, obj, cost, BOOL(mon == NULL));
    }
    return slay;
}
static int _slay_dam(slay_ptr slay, int dam)
{
    return dam * slay->mul / 100 + slay->add;
}
static slay_t _slay_find_best(plr_attack_ptr context, slay_info_ptr tbl)
{
    slay_t best = {0};
    int i, dam, best_dam = context->dam;
    for (i = 0; ; i++)
    {
        slay_info_ptr info = &tbl[i];
        if (!info->id) break;
        if (have_flag(context->obj_flags, info->id))
        {
            slay_t tmp = slay_info_apply(info, context->mon, context->obj);
            if (!tmp.id) continue;
            dam = _slay_dam(&tmp, context->dam);
            if (dam > best_dam)
            {
                best = tmp;
                best_dam = dam;
            }
        }
    }
    return best;
}
static bool _plr_allow_slay(plr_attack_ptr context)
{
    if (context->blow) return mon_blow_allow_slay(context->blow);
    return TRUE;
}
static void _apply_slays(plr_attack_ptr context)
{
    slay_t best_slay, best_brand, slay = {0};

    if (!_plr_allow_slay(context)) return;

    /* find the best slay */
    best_slay = _slay_find_best(context, slay_tbl);
    if (context->hooks.calc_slay_f)
    {
        slay_t tmp = context->hooks.calc_slay_f(context, &best_slay);
        int    best_dam = _slay_dam(&best_slay, context->dam);
        int    dam = _slay_dam(&tmp, context->dam);
        if (dam > best_dam)
            best_slay = tmp;
    }
    if (best_slay.msg)
        msg_format(best_slay.msg, context->mon_name);

    /* find the best brand */
    best_brand = _slay_find_best(context, brand_tbl);
    if (context->hooks.calc_brand_f)
    {
        slay_t tmp = context->hooks.calc_brand_f(context, &best_brand);
        int    best_dam = _slay_dam(&best_brand, context->dam);
        int    dam = _slay_dam(&tmp, context->dam);
        if (dam > best_dam)
            best_brand = tmp;
    }
    if (best_brand.msg)
        msg_format(best_brand.msg, context->mon_name);

    /* compute the total slay */
    if (best_slay.id)
    {
        slay = best_slay;
        if (best_brand.id)
        {
            slay.mul += best_brand.mul - 100;
            slay.add += best_brand.add;
        }
    }
    else if (best_brand.id)
        slay = best_brand;

    if (!slay.id) slay.mul = 100;

    /* use the force */
    if (have_flag(context->obj_flags, OF_BRAND_MANA))
    {
        int cost = 0, dd = 0, ds = 0;

        if (context->obj)
        {
            dd = context->obj->dd + context->info.to_dd;
            ds = context->obj->ds + context->info.to_ds;
        }
        else if (context->blow)
        {
            dice_t d = mon_blow_base_dice(context->blow);
            dd = d.dd + context->info.to_dd;
            ds = d.ds + context->info.to_ds;
        }

        if (plr->pclass == CLASS_SAMURAI)
            cost = (1 + (dd * ds * 2 / 7));
        else
            cost = (1 + (dd * ds / 7));

        slay = slay_apply_force(slay, context->obj, cost, FALSE);
    }

    #if DEVELOPER
    if (0 && slay.mul != 100)
    {
        str_ptr s = str_alloc();
        
        str_printf(s, "<color:D>%d x %d.%02d", context->dam, slay.mul/100, slay.mul%100);
        if (slay.add)
            str_printf(s, " + %d", slay.add);
        str_printf(s, " = %d</color>", context->dam*slay.mul/100 + slay.add);
        msg_print(str_buffer(s));
        str_free(s);
    }
    #endif

    /* boost the damage */
    if (slay.mul > 100)
        context->dam = context->dam * slay.mul / 100;
    if (slay.add > 0)
        context->dam_xtra += slay.add;
}
/*************************************************************************
 * Initialize a Context
 *************************************************************************/
bool plr_attack_begin(plr_attack_ptr context, point_t pos)
{
    mon_ptr mon = dun_mon_at(cave, pos);
    bool    attack_ct = 0;

    disturb(0, 0);

    if (!(context->flags & PAC_NO_WEAPON))
        attack_ct += plr->weapon_ct;
    if (!(context->flags & PAC_NO_INNATE))
        attack_ct += vec_length(plr->innate_blows);
    if (!attack_ct)
    {
        if (context->flags & PAC_NO_WEAPON)
            msg_print("You have no innate melee attacks.");
        else if (context->flags & PAC_NO_INNATE)
            msg_print("You have no normal melee attacks.");
        else
            msg_print("You have no melee attacks.");
        context->energy = 0;
        return FALSE;
    }
    if (prace_is_(MIMIC_MIST))
    {
        msg_print("You cannot attack while incorporeal.");
        context->energy = 0;
        return FALSE;
    }
    if (!mon)
    {
        msg_print("There is no monster there!");
        context->energy = 0;
        return FALSE;
    }
    /* XXX We assume it is OK to attack pos even if not adjacent. Many classes have
     * techniques that allow attacking monsters at a distance (e.g. beholders, samurai, etc.)
     * See plr_attack_special_aux() for proper treatment of ranged techniques that avoid
     * relying on an actual call to project. */

    context->mon = mon;
    if (point_fast_distance(plr->pos, mon->pos) > 1)
        context->flags |= PAC_NO_AURA;
    _init_race(context);
    context->mon_pos = pos;
    context->plr_pos = plr->pos;
    context->counter = 0;
    context->hits = 0;
    context->misses = 0;
    context->dam_total = 0;
    context->stop = STOP_CONTINUE;
    context->drain_left = 0;
    context->drain_hits = 0;
    context->retaliation_ct = 0;
    context->fear = FALSE;
    context->do_quake = FALSE;
    context->do_blink = FALSE;
    context->do_knockback = 0;
    context->do_sleep = FALSE;
    
    if (context->mon->ml)
    {
        if (!plr_tim_find(T_HALLUCINATE)) mon_track(context->mon);
        health_track(context->mon);
    }
    if (!context->energy)
        context->energy = 100;

    /* The following checks waste player energy */
    if ( mon_is_female(context->mon)
      && !(plr_tim_find(T_STUN) || plr_tim_find(T_CONFUSED) || plr_tim_find(T_HALLUCINATE) || !context->mon->ml)
      && equip_find_art("|.Zantetsuken") )
    {
        msg_print("I can not attack women!");
        return FALSE;
    }
    if (cave->flags & DF_NO_MELEE)
    {
        msg_print("Something prevents you from attacking.");
        return FALSE;
    }
    if ( !mon_is_hostile(context->mon)
      && !(plr_tim_find(T_STUN) || plr_tim_find(T_CONFUSED) || plr_tim_find(T_HALLUCINATE) || plr_tim_find(T_BERSERK) || !context->mon->ml) )
    {
        if (equip_find_art("|.Stormbringer"))
        {
            msg_format("Your black blade greedily attacks %s!", context->mon_name_obj);
            virtue_add(VIRTUE_INDIVIDUALISM, 1);
            virtue_add(VIRTUE_HONOUR, -1);
            virtue_add(VIRTUE_JUSTICE, -1);
            virtue_add(VIRTUE_COMPASSION, -1);
        }
        else
        {
            if (get_check("Really hit it? "))
            {
                virtue_add(VIRTUE_INDIVIDUALISM, 1);
                virtue_add(VIRTUE_HONOUR, -1);
                virtue_add(VIRTUE_JUSTICE, -1);
                virtue_add(VIRTUE_COMPASSION, -1);
            }
            else
            {
                msg_format("You stop to avoid hitting %s.", context->mon_name_obj);
                return FALSE;
            }
        }
    }

    /* custom checks */
    _plr_custom_init(context); /* give race/class a chance to install a begin_f! */
    if (context->hooks.begin_f)
    {
        context->hooks.begin_f(context);
        if (context->stop) return FALSE; /* allow custom aborts */
    }

    if (mon_tim_find(context->mon, MT_SLEEP)) /* It is not honorable etc to attack helpless victims */
    {
        if (!mon_is_evil(mon) || one_in_(5)) virtue_add(VIRTUE_COMPASSION, -1);
        if (!mon_is_evil(mon) || one_in_(5)) virtue_add(VIRTUE_HONOUR, -1);
    }

    if (plr->riding)
    {
        skills_riding_gain_melee(context->race);
        plr->riding_target = who_create_mon(context->mon);
    }
    _push(context);
    return TRUE;
}

void plr_attack_end(plr_attack_ptr context)
{
    if ( (context->do_blink || context->mode == PLR_HIT_TELEPORT) 
      && (!context->stop || context->stop == STOP_MON_DEAD) )
    {
        if (randint0(plr->skills.dis) < 7)
            msg_print("You fail to run away!");
        else
            teleport_player(25 + plr->lev/2, 0);
    }
    if (context->stop != STOP_MON_DEAD)
    {
        if (context->hits)
            mon_set_target(context->mon, plr->pos);
        if (context->do_sleep)
            _gf_innate(context->mon, GF_SLEEP, 5 + plr_prorata_level(95));
        if (context->mode == PLR_HIT_KNOCKBACK && context->hits)
        {
            int dist = 4 + context->hits;
            if (plr->pclass == CLASS_MAULER) dist += 3;
            if (dist) do_monster_knockback(context->mon, dist);
        }
        else if (context->do_knockback) /* set the distance to knockback */
            do_monster_knockback(context->mon, context->do_knockback);
        /*if (context->fear && context->mon->ml)
            msg_format("%^s flees in terror!", context->mon_name);*/
    }
    if (context->hooks.end_f)
        context->hooks.end_f(context);
    _pop();
}
static void _add_extra_obj_flags(plr_attack_ptr context)
{
    if (context->mode == PLR_HIT_ACID)
        add_flag(context->obj_flags, OF_BRAND_ACID);
    if (context->mode == PLR_HIT_ELEC)
        add_flag(context->obj_flags, OF_BRAND_ELEC);
    if (context->mode == PLR_HIT_FIRE)
        add_flag(context->obj_flags, OF_BRAND_FIRE);
    if (context->mode == PLR_HIT_COLD)
        add_flag(context->obj_flags, OF_BRAND_COLD);
    if (context->mode == PLR_HIT_POISON)
        add_flag(context->obj_flags, OF_BRAND_POIS);
    if (context->mode == PLR_HIT_MANA)
        add_flag(context->obj_flags, OF_BRAND_MANA);
    if (context->mode == PLR_HIT_VAMP)
        add_flag(context->obj_flags, OF_BRAND_VAMP);
    if (context->mode == PLR_HIT_VORPAL)
        add_flag(context->obj_flags, OF_VORPAL);
}
bool plr_attack_init_hand(plr_attack_ptr context, int hand)
{
    int to_h;

    /* turn off innate attacks */
    context->which_blow = BLOW_NONE;
    context->blow = NULL;

    /* turn on normal attacks */
    context->hand = hand;
    context->info = plr->attack_info[hand];
    if (context->info.type == PAT_WEAPON)
        context->obj = equip_obj(context->info.slot);
    else
        context->obj = NULL;

    if (context->flags & PAC_DISPLAY)
        weapon_flags_known(hand, context->obj_flags);
    else
        weapon_flags(hand, context->obj_flags); /* N.B. weapon_flags handles bare handed fighting */
    _add_extra_obj_flags(context);

    /* cache the base skill amount. we need this for plr_check_hit and _apply_crits */
    to_h = context->to_h + context->info.to_h;
    if (context->obj)
        to_h += context->obj->to_h;

    context->skill = plr->skills.thn + to_h*BTH_PLUS_ADJ;
    if (context->skill > 0 && context->obj)
        context->skill = context->skill * context->info.skill_mul / 1000;

    context->skill += virtue_current(VIRTUE_VALOUR) / 10;

    if (have_flag(context->info.paf_flags, PAF_DUAL_WIELDING) && one_in_(2) && !(context->flags & PAC_DISPLAY))
    {
        assert(context->race);
        skills_dual_wielding_gain(context->race);
    }

    if (context->obj)
        object_desc(context->obj_name, context->obj, OD_NAME_ONLY | OD_OMIT_PREFIX | OD_COLOR_CODED);
    else
        strcpy(context->obj_name, "Your fists");

    /* skip this weapon? */
    if (context->hooks.begin_weapon_f)
        return context->hooks.begin_weapon_f(context);
    return TRUE;
}

static bool _blow_is_masked(mon_blow_ptr blow)
{
    if ((blow->flags & MBF_MASK_BLIND) && plr_tim_find(T_BLIND))
        return TRUE;

    /* mask hand based attacks only if the player's body actually has hands */
    if ((blow->flags & MBF_MASK_HAND) && equip_is_valid_hand(0))
    {
        if (plr->weapon_ct > 0) /* Wielding a weapon blocks hand based attacks */
            return TRUE;

        if (!equip_find_empty_hand()) /* dual wielding shields also blocks */
            return TRUE;
    }
    return FALSE;
}

bool plr_attack_init_innate(plr_attack_ptr context, int which)
{
    int i;
    int to_h;

    assert(0 <= which && which < vec_length(plr->innate_blows));

    /* turn off normal attacks */
    context->hand = HAND_NONE;
    context->obj = NULL;

    /* turn on innate attacks */
    context->which_blow = which;
    context->info = plr->innate_attack_info;
    context->info.type = PAT_INNATE;
    context->blow = vec_get(plr->innate_blows, which);

    if (context->flags & PAC_DISPLAY)
    {
        for (i = 0; i < OF_ARRAY_SIZE; i++)
            context->obj_flags[i] = plr->innate_attack_info.obj_known_flags[i];
    }
    else
    {
        for (i = 0; i < OF_ARRAY_SIZE; i++)
            context->obj_flags[i] = plr->innate_attack_info.obj_flags[i];
    }

    /* cache the base skill amount. we need this for plr_check_hit and _apply_crits */
    to_h = context->to_h + plr->to_h_m;
    to_h += skills_innate_calc_bonus(skills_innate_calc_name(context->blow));

    context->skill = plr->skills.thn + to_h*BTH_PLUS_ADJ;
    context->skill += virtue_current(VIRTUE_VALOUR) / 10;
    context->skill += context->blow->power;

    /* skip this blow? */
    if (_blow_is_masked(context->blow))
        return FALSE;
    if (context->hooks.begin_weapon_f)
        return context->hooks.begin_weapon_f(context);
    return TRUE;
}

/*************************************************************************
 * A Single Strike for Innate Attacks (PAT_INNATE and PAT_MONK)
 *************************************************************************/
static void _hit_effects2(plr_attack_ptr context);

static void _innate_hit_msg(plr_attack_ptr context)
{
    if (context->blow->msg)
        msg_format(context->blow->msg, context->mon_name_obj);
    else
    {
        mon_blow_info_ptr info = mon_blow_info_lookup(context->blow->method);
        msg_format(info->plr_hit_msg, context->mon_name_obj);
    }
}
static bool _skip_effect(int which)
{
    switch (which)
    {
    case RBE_CUT:
    case RBE_DRAIN_EXP:
    case RBE_LOSE_STR: case RBE_LOSE_INT: case RBE_LOSE_WIS:
    case RBE_LOSE_DEX: case RBE_LOSE_CON: case RBE_LOSE_CHR:
    case RBE_LOSE_ALL:
    case RBE_EAT_LIGHT:
        return TRUE;
    }
    return FALSE;
}
static bool _has_ankle(mon_race_ptr race)
{
    if (mon_race_never_move(race)) return FALSE;
    if (mon_race_is_char_ex(race, "~#{}.UjmeEv$,DdsbBFIJQSXclnw!=?")) return FALSE;
    return TRUE;
}
static bool _gf_innate(mon_ptr mon, int effect, int dam)
{
    return gf_affect_m(who_create_plr(), mon, effect, dam, GF_AFFECT_ATTACK | GF_AFFECT_QUIET);
}
static void _innate_hit_mon(plr_attack_ptr context)
{
    int    j;
    slay_t crit = {0}; /* Monks: goose stun effects with criticals */

    if (!context->technique || context->info.type == PAT_MONK)
        _innate_hit_msg(context);
    for (j = 0; j < context->blow->effect_ct; j++)
    {
        mon_effect_t effect = context->blow->effects[j]; /* copy since we'll adjust values as needed */

        if (_stop_attack(context)) break;
        if (!effect.type) break;
        if (_skip_effect(effect.type)) continue;
        if (effect.type == GF_STUN && effect.pct)
        {
            if (context->mode == PLR_HIT_STUN) effect.pct *= 2;
            else if (crit.id) effect.pct += crit.mul / 2000;
        }
        if (effect.pct && randint1(100) > effect.pct) continue;

        /* The first effect is the base damage, and gets boosted by combat boosting
         * magic (e.g. rings of combat, weaponmaster, et. al.). Also apply criticals,
         * vorpal hits and slays. Apply bonuses from special combat techniques. */
        if (j == 0)
        {
            effect.dice.dd += context->info.to_dd;
            effect.dice.ds += context->info.to_ds;
            context->dam = damroll(effect.dice.dd, effect.dice.ds);
            if (context->blow->flags & MBF_TOUCH) /* XXX seems like we should skip these for non-touch blows */
            {
                crit = _apply_crits(context);
                _apply_vorpal(context);
                context->dam_base = context->dam; /* XXX _unlife_strike_spell. before slays! */
                _apply_slays(context);
            }
            if (context->technique)
                context->dam = context->dam * context->technique / 100;
            if (plr->clp > 1000) /* RACE_MON_LICH ... scaling base dice only seems weak */
            {
                context->dam = context->dam * plr->clp / 1000;
                effect.dice.base = effect.dice.base * plr->clp / 1000;
            }
            context->dam += context->dam_xtra; /* crit.add and slay.add */
            context->dam += effect.dice.base;
            if (context->dam)
            {
                context->dam += context->info.to_d;
                if (effect.type != RBE_VAMP)
                    context->dam_drain = context->dam;
                if (context->hooks.mod_damage_f)
                    context->hooks.mod_damage_f(context);
            }
        }
        /* Subsequent effects are add-ons, and should not be damage boosted */
        else
            context->dam = dice_roll(effect.dice);

        if (plr_tim_find(T_STUN) && context->dam > 0)
        {
            context->dam -= context->dam * MIN(100, plr_tim_amount(T_STUN)) / 150;
            context->dam_base -= context->dam_base * MIN(100, plr_tim_amount(T_STUN)) / 150;
        }
        if (context->dam < 0) context->dam = 0;
        if (context->blow->method == RBM_EXPLODE)
        {
            possessor_explode(context->dam);
            context->stop = STOP_PLR_SPECIAL;
            return;
        }
        switch (effect.type)
        {
        case RBE_SHATTER:
            if (context->dam > 23) context->do_quake = TRUE;
        case RBE_HURT:
        case RBE_VAMP: /* cf discussion in _effect_plr (mon_attack.c) */
            context->dam = mon_damage_mod(context->mon, context->dam, FALSE);
            if (context->dam > 0)
                anger_monster(context->mon);
            context->dam_total += context->dam;
            if (mon_take_hit(context->mon, context->dam, &context->fear, NULL))
            {
                context->stop = STOP_MON_DEAD;
                break;
            }
            if (effect.type == RBE_VAMP && mon_race_is_living(context->race) && context->drain_left)
            {
                int amt = MIN(context->dam, context->drain_left);
                msg_format("You <color:D>drain life</color> from %s!", context->mon_name_obj);
                if (prace_is_(MIMIC_BAT))
                    vampire_feed(amt);
                else
                    vamp_player(amt);
                context->drain_left -= amt;
                /* we bypassed _apply_vampiric() so don't set dam_drain for this effect. */
            }
            if (j == 0) _hit_effects2(context);
            break;
        case RBE_EAT_GOLD:
        case RBE_EAT_ITEM:
        case RBE_EAT_FOOD:
            if (leprechaun_steal(context->mon->id))
                context->do_blink = TRUE;
            break;
        case RBE_DISEASE:
            if (context->dam)
                _gf_innate(context->mon, GF_POIS, context->dam);
            break;
        case RBE_DRAIN_CHARGES:
            _gf_innate(context->mon, GF_DRAIN_MANA, context->dam ? context->dam : (plr->lev + 1)/2);
            break;
        case GF_PARALYSIS:
            _gf_innate(context->mon, GF_PARALYSIS, context->dam ? context->dam : plr_prorata_level(75));
            break;
        case GF_SLEEP:
            context->do_sleep = TRUE; /* plr_attack_end ... otherwise plr_hit_mon will immediately undo */
            break;
        case GF_OLD_CONF:
            _gf_innate(context->mon, GF_OLD_CONF, context->dam ? context->dam : 5 + plr_prorata_level(95));
            break;
        case RBE_STUN_MALE:
            if (mon_is_male(context->mon))
            {
                if (context->mode == PLR_HIT_STUN) context->dam *= 2;
                _gf_innate(context->mon, GF_STUN, context->dam);
                mon_lore_male(context->mon);
            }
            break;
        case GF_STUN:
            if (context->mode == PLR_HIT_STUN) context->dam *= 2;
            else if (crit.id) context->dam += context->dam * crit.mul / 1000;
            _gf_innate(context->mon, GF_STUN, context->dam);
            break;
        case RBE_SLOW_ANKLE:
            if (_has_ankle(context->race))
            {
                if ( !mon_race_is_unique(context->race)
                  && randint1(plr->lev) > context->race->alloc.lvl
                  && context->mon->mspeed > -40 )
                {
                    msg_format("%^s starts limping slower.", context->mon_name);
                    context->mon->mspeed -= 10;
                }
            }
            break;
        default:
            _gf_innate(context->mon, effect.type, context->dam);
            _check_race(context);
        }
    }
    /* XXX Need a call to _stop_attack rather than just checking context->stop.
     * For example, _gf_innate might kill the monster (or teleport it) on the *last*
     * effect for this blow, at which point context->stop == 0 causing us to
     * mis-apply auras (or crash if the monster is dead). An example is a Death monk
     * killing an Ice Elemental with a Zombie Claw (GF_NETHER). XXX */
    if ((context->blow->flags & MBF_TOUCH) && !_stop_attack(context))
        plr_on_hit_mon(context);
}

/*************************************************************************
 * A Single Strike
 *************************************************************************/
static bool _do_quake(plr_attack_ptr context)
{
    if (context->mode == PLR_HIT_QUAKE) return TRUE;
    if (have_flag(context->obj_flags, OF_IMPACT))
        return context->dam_base > 50 || one_in_(7);
    return FALSE;
}
static void _hit_effects1(plr_attack_ptr context) /* before mon_take_hit */
{
    if (context->info.type == PAT_INNATE) return;
    if (context->dam > 0) anger_monster(context->mon);
    if (_do_quake(context))
    {
        context->do_quake = TRUE; /* Later */
        if ( _1d(100) <= mon_res_pct(context->mon, GF_STUN)
          || mon_save_stun(context->race->alloc.lvl, context->dam) )
        {
        }
        else
        {
            mon_stun(context->mon, mon_stun_amount(context->dam));
        }
    }
}
static void _hit_effects2(plr_attack_ptr context) /* after mon_take_hit ... but not if dead */
{
    if (context->info.type != PAT_INNATE)
    {
        if (have_flag(context->obj_flags, OF_BRAND_TIME))
        {
            gf_affect_m(who_create_plr(), context->mon, GF_TIME, context->dam_base, GF_AFFECT_ATTACK);
            obj_learn_slay(context->obj, OF_BRAND_TIME, "is <color:B>Time Branded</color>");
            if (!mon_is_valid(context->mon))
            {
                context->stop = STOP_MON_DEAD;
                return;
            }
            _check_race(context); /* evolution and devolution */
        }
        if (have_flag(context->obj_flags, OF_BRAND_CHAOS) && one_in_(7))
        {
            gf_affect_m(who_create_plr(), context->mon, GF_CHAOS, context->dam_base, GF_AFFECT_ATTACK);
            obj_learn_slay(context->obj, OF_BRAND_CHAOS, "is <color:v>Marked by Chaos</color>");
            if (one_in_(10)) virtue_add(VIRTUE_CHANCE, 1);
            if (!mon_is_valid(context->mon))
            {
                context->stop = STOP_MON_DEAD;
                return;
            }
            _check_race(context); /* polymorph */
        }
    }
    /* Confusion attack */
    if ( (plr->special_attack & ATTACK_CONFUSE)
      || context->mode == PLR_HIT_CONFUSE
      || plr_tim_find(T_HEX_CONFUSION)
      || (giant_is_(GIANT_TITAN) && plr->lev >= 30 && one_in_(5)) )
    {
        /* Cancel glowing hands */
        if (plr->special_attack & ATTACK_CONFUSE)
        {
            plr->special_attack &= ~(ATTACK_CONFUSE);
            msg_print("Your hands stop glowing.");
            plr->redraw |= PR_STATUS;
        }

        /* Confuse the monster */
        if (mon_res_pct(context->mon, GF_CONFUSION) > 0) /* XXX any resist => old RF3_NO_CONF */
        {
            mon_lore_resist(context->mon, GF_CONFUSION);
            msg_format("%^s is unaffected.", context->mon_name);
        }
        else if (randint0(100) < context->race->alloc.lvl)
            msg_format("%^s is unaffected.", context->mon_name);
        else
            mon_tim_add(context->mon, T_CONFUSED, 10 + randint0(plr->lev)/5);

        /* Only try to confuse once */
        if (context->mode == PLR_HIT_CONFUSE)
            context->mode = PLR_HIT_NORMAL;
    }
    if (have_flag(context->obj_flags, OF_STUN) || context->mode == PLR_HIT_STUN)
    {
        #if 0
        msg_format("<color:D>1d%d <= %d</color>", context->race->level, plr->lev/5 + context->dam_base);
        #endif
        if ( _1d(100) <= mon_res_pct(context->mon, GF_STUN)
          || _1d(context->race->alloc.lvl) > plr->lev/5 + context->dam_base /* XXX */
          || mon_save_stun(context->race->alloc.lvl, context->dam) )
        {
        }
        else
        {
            mon_stun(context->mon, mon_stun_amount(context->dam));
            obj_learn_slay(context->obj, OF_STUN, "<color:o>Stuns</color> your enemies");
        }
    }
}
static void _miss_effects(plr_attack_ptr context)
{
    if (object_is_(context->obj, TV_POLEARM, SV_DEATH_SCYTHE))
    {
        death_scythe_miss(context->obj, context->hand, context->skill);
        if (plr->is_dead)
            context->stop = STOP_PLR_DEAD;
    }
}
static void _weapon_hit_mon(plr_attack_ptr context)
{
    int dd = context->obj->dd + context->info.to_dd;
    int ds = context->obj->ds + context->info.to_ds;
    if (!context->technique) msg_print("You hit.");
    context->dam = damroll(dd, ds);
    _apply_crits(context);
    _apply_vorpal(context);
    context->dam_base = context->dam;
    _apply_slays(context);
    /* Scale the base damage roll for player techniques */
    if (context->technique)
    {
        context->dam = context->dam * context->technique / 100;
        context->dam_base = context->dam_base * context->technique / 100;
    }
    context->dam += context->dam_xtra; /* crit.add and slay.add */

    /* Calculate Total Damage */
    if (context->obj)
        context->dam += context->obj->to_d;
    context->dam += context->info.to_d;
    context->dam += context->to_d;
    if (plr_tim_find(T_STUN) && context->dam > 0)
    {
        context->dam -= context->dam * MIN(100, plr_tim_amount(T_STUN)) / 150;
        context->dam_base -= context->dam_base * MIN(100, plr_tim_amount(T_STUN)) / 150;
    }
    if (context->dam < 0) context->dam = 0;
    _mon_damage_mod(context);
    context->dam_drain = context->dam;
    if (context->hooks.mod_damage_f)
        context->hooks.mod_damage_f(context);

    /* Apply Damage */
    context->dam_total += context->dam;
    _hit_effects1(context);
    if (mon_take_hit(context->mon, context->dam, &context->fear, NULL))
        context->stop = STOP_MON_DEAD;
    else
    {
        _hit_effects2(context);
        if (!context->stop) /* e.g. OF_BRAND_CHAOS */
            plr_on_hit_mon(context);
    }
}
static bool _repelled(plr_attack_ptr context)
{
    if ( mon_tim_find(context->mon, T_PROT_EVIL)
      && _1d(200) <= -plr->align
      && mon_save_p(context->mon, A_CHR) ) /* ie !p_save_mon... */
    {
        cmsg_print(TERM_L_BLUE, "You are repelled.");
        mon_tim_subtract(context->mon, T_PROT_EVIL, _1d(1 + plr->lev/5));
        return TRUE;
    }
    if ( mon_tim_find(context->mon, T_PROT_GOOD)
      && _1d(200) <= plr->align
      && mon_save_p(context->mon, A_CHR) ) /* ie !p_save_mon... */
    {
        cmsg_print(TERM_L_BLUE, "You are repelled.");
        mon_tim_subtract(context->mon, T_PROT_GOOD, _1d(1 + plr->lev/5));
        return TRUE;
    }
    return FALSE;
}
bool plr_hit_mon(plr_attack_ptr context)
{
    bool hit = FALSE;

    context->dam_base = 0;
    context->dam_drain = 0;
    context->dam = 0;
    context->dam_xtra = 0;
    context->technique = 0;

    /* Check if the battle may continue */
    if (_stop_attack(context)) return FALSE;
    if (_stop_fear(context)) return FALSE;

    if (context->flags & PAC_ANIMATE)
        _animate(context);

    /* Attempt a new blow with current weapon */
    context->counter++;
    if (context->hooks.check_hit_f)
    {
        hit = context->hooks.check_hit_f(context);
        if (context->stop) return FALSE; /* paranoia */
    }
    else
        hit = plr_check_hit(context);

    mon_tim_delete(context->mon, MT_SLEEP);

    if (_repelled(context))
        return FALSE;

    if (hit)
    {
        context->hits++;
        if (context->hooks.before_hit_f)
            context->hooks.before_hit_f(context);

        switch (context->info.type)
        {
        case PAT_WEAPON:
            _weapon_hit_mon(context);
            break;
        case PAT_MONK:
            context->blow = monk_choose_attack_plr(plr->monk_tbl);
            _innate_hit_mon(context);
            break;
        case PAT_INNATE:
            if (context->blow->method == RBM_MONK)
            {
                mon_blow_ptr old = context->blow;
                context->blow = monk_choose_attack_plr(old->name);
                _innate_hit_mon(context);
                context->blow = old; /* restore so next strike knows to choose a new attack */
            }
            else
                _innate_hit_mon(context);
            break;
        }

        if (!_stop_attack(context))
            _apply_vampiric(context);
        if (context->hooks.after_hit_f)
            context->hooks.after_hit_f(context);
        if (context->info.type == PAT_MONK)
            context->blow = NULL; /* paranoia: non-null blow used to imply PAT_INNATE */
    }
    else
    {
        context->misses++;
        msg_print("You miss.");
        _miss_effects(context);
    }
    return hit;
}

/*************************************************************************
 * Special Techniques to Enhance Normal Melee
 *
 * Typically the player is casting a spell and we need to prompt them
 * for the target to attack. For the sake of ergonomics, we'll try to
 * automatically choose a target when possible (e.g. first adjacent monster
 * for normal range=1 techniques; or current target if still in range, etc)
 *
 * Distance techniques no longer project a bolt of GF_ATTACK since "int dam"
 * cannot hold the rich "plr_attack_ptr context". Instead, we'll manually
 * traverse the projection looking for the first hit monster.
 *************************************************************************/
bool plr_attack_special(int type, int flags)
{
    return plr_attack_ranged(type, flags, 1);
}
bool plr_attack_ranged(int type, int flags, int range)
{
    plr_attack_t context = {0};
    context.mode = type;
    context.flags = flags;
    return plr_attack_special_aux(&context, range);
}
static bool _target_ok(void)
{
    if (who_is_pos(plr->target))
        return plr_project(who_pos(plr->target));
    if (who_is_mon(plr->target))
        return target_okay();
    return FALSE;
}
static mon_ptr _get_adjacent_target(void)
{
    int i;
    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(plr->pos, cdd[i]);
        mon_ptr mon = dun_mon_at(cave, p);
        if (!mon || !mon->ml || !mon_is_hostile(mon)) continue;
        _set_target(mon);
        return mon;
    }
    return NULL;
}
static mon_ptr _get_ranged_target(point_t pos, int range)
{
    point_t path[32];
    int  path_n, i;

    /* fire a bolt from plr to pos ... */
    path_n = project_path(path, range, plr->pos, pos, PROJECT_STOP);
    if (!path_n) return NULL;

    /* ... and return the first hit monster */
    for (i = 0; i < path_n; i++)
    {
        point_t p = path[i];
        mon_ptr mon = dun_mon_at(cave, p);
        if (mon) return mon;
    }
    return NULL;
}
bool plr_attack_special_aux(plr_attack_ptr context, int range)
{
    mon_ptr mon = NULL;

    /* Ergonomics: Use current monster target if in range. Ignore position targets. */
    if (!mon && use_old_target && who_is_mon(plr->target) && _target_ok())
    {
        mon = who_mon(plr->target);
        if (mon->cdis > range)
            mon = NULL;
        else if (range > 1) /* find first monster between plr and mon */
            mon = _get_ranged_target(mon->pos, range);
    }

    /* Ergonomics: Automatically choose and target first adjacent hostile monster */
    if (!mon && auto_target && range == 1 && !plr_tim_find(T_CONFUSED) && !plr_tim_find(T_HALLUCINATE))
        mon = _get_adjacent_target();

    /* Prompt user for target or direction */
    if (!mon)
    {
        point_t tgt = {0};
        int dir;
        if (range == 1)
        {
            if (!get_rep_dir2(&dir)) return FALSE;
            if (dir == 5) return FALSE;

            tgt = point_step(plr->pos, dir);
            mon = dun_mon_at(cave, tgt);
        }
        else
        {

            project_length = range;
            if (!get_fire_dir(&dir)) return FALSE;
            tgt = point_jump(plr->pos, dir, project_length);
            if (dir == 5 && target_okay())
                tgt = who_pos(plr->target);

            /* find first monster between plr and tgt */
            mon = _get_ranged_target(tgt, range);
            project_length = 0;
        }
    }

    if (!mon)
    {
        msg_print("There is no monster there.");
        return FALSE;
    }

    return plr_attack(context, mon->pos);
}

/*************************************************************************
 * Attack with all weapons + innate attacks
 *************************************************************************/
bool plr_attack_normal(point_t pos) 
{
    plr_attack_t context = {0};
    return plr_attack(&context, pos);
}
void plr_attack_start_msg(plr_attack_ptr context)
{
    if (!(context->flags & PAC_NO_START_MSG))
    {
        cptr desc = context->attack_desc;
        if (!desc) desc = "attack";
        cmsg_format(TERM_L_UMBER, "You %s %s:", desc, context->mon_full_name);
    }
}
bool plr_attack(plr_attack_ptr context, point_t pos)
{
    int  i;
    bool result = FALSE;

    if (plr_attack_begin(context, pos))
    {
        plr_attack_start_msg(context);
        /* Attack with weapons or martial arts. Stop if the monster
         * is slain or teleported. Stop if the player gets scared or
         * is killed. Also, clients can stop the attacks for certain
         * class specific techniques. */
        if (!(context->flags & PAC_NO_WEAPON))
        {
            for (i = 0; i < MAX_HANDS; i++)
            {
                if (context->stop) break;
                if (plr->attack_info[i].type != PAT_NONE)
                {
                    if (plr_attack_init_hand(context, i))
                        plr_attack_mon(context);
                }
            }
        }
        /* Attack with Innate Attacks */
        if (!(context->flags & PAC_NO_INNATE))
        {
            for (i = 0; i < vec_length(plr->innate_blows); i++)
            {
                if (context->stop) break;
                if (plr_attack_init_innate(context, i))
                    plr_attack_mon(context);
            }
        }
        plr_attack_end(context);
        result = TRUE;
    }
    energy_use = context->energy;
    if (context->stop == STOP_MON_DEAD && mut_present(MUT_FANTASTIC_FRENZY) && !(context->flags & PAC_ONE_BLOW))
        energy_use = _fractional_energy_use(context);
    #ifdef DEVELOPER
    if (1 || plr->wizard)
    {
        rect_t r = ui_char_info_rect();
        int    a = TERM_WHITE;
        if (context->dam_total > 1000) a = TERM_VIOLET;
        else if (context->dam_total > 750) a = TERM_RED;
        else if (context->dam_total > 500) a = TERM_L_RED;
        else if (context->dam_total > 250) a = TERM_YELLOW;
        else if (context->dam_total > 100) a = TERM_L_UMBER;
        c_put_str(a, format("Dam:%5d", context->dam_total), 0, r.x);
        /*c_put_str(a, format("Dam:%5d", context->dam_total), r.y + r.cy - 3, r.x);*/
        /*cmsg_format(a, "You did %d total damage.", context->dam_total);*/
    }
    #endif

    return result;
}

/*************************************************************************
 * Retaliate with a single strike
 *************************************************************************/
static int _random_hand(void)
{
    int i, ct = 0, n;
    for (i = 0; i < MAX_HANDS; i++)
    {
        if (plr->attack_info[i].type == PAT_NONE) continue;
        if (plr->pclass == CLASS_SAMURAI && !equip_obj(plr->attack_info[i].slot)) continue;
        ct++;
    }
    if (!ct) return HAND_NONE;
    n = randint0(ct);
    for (i = 0; i < MAX_HANDS; i++)
    {
        if (plr->attack_info[i].type == PAT_NONE) continue;
        if (plr->pclass == CLASS_SAMURAI && !equip_obj(plr->attack_info[i].slot)) continue;
        n--;
        if (n < 0)
            return i;
    }
    return HAND_NONE;
}
static int _random_blow(void)
{
    int ct = vec_length(plr->innate_blows);
    if (!ct) return BLOW_NONE;
    return randint0(ct); /* XXX weight by blows */
}
static bool _random_attack(plr_attack_ptr context)
{
    int which = _random_hand();
    if (which != HAND_NONE)
        return plr_attack_init_hand(context, which);
    which = _random_blow();
    if (which != BLOW_NONE)
        return plr_attack_init_innate(context, which);
    return FALSE;
}
bool plr_retaliate(plr_attack_ptr context, point_t pos)
{
    context->flags |= PAC_RETALIATE;
    if (plr_attack_begin(context, pos))
    {
        if (!_random_attack(context)) return FALSE;
        if (!(context->flags & PAC_NO_START_MSG))
        {
            cmsg_print(TERM_L_UMBER, "(You retaliate:");
        }
        plr_hit_mon(context);
        if (!(context->flags & PAC_NO_START_MSG))
            cmsg_print(TERM_L_UMBER, ")");
        plr_attack_end(context);
        return TRUE;
    }
    return FALSE;
}

/*************************************************************************
 * Attack with given weapon (mutliple blows per round)
 *************************************************************************/
bool plr_attack_mon(plr_attack_ptr context)
{
    int i;

    if ((context->flags & PAC_ONE_BLOW) && context->counter == 1) return FALSE;

    /* Initialize and gain Proficiency */
    context->drain_left = _max_vampiric_drain();
    context->drain_hits = 0;
    context->do_quake = FALSE;
    if (context->blow)
    {
        skills_innate_gain(skills_innate_calc_name(context->blow), context->race->alloc.lvl);
    }
    else
    {
        context->retaliation_ct = 0; /* don't reset for innate attacks */
        if (context->obj)
            skills_weapon_gain(context->obj->tval, context->obj->sval, context->race->alloc.lvl);
        else if (context->race->alloc.lvl + 10 > plr->lev)
            skills_martial_arts_gain();
    }

    /* Hack for the poison needle: single blow that always hits */
    if (_do_poison_needle(context))
    {
        if (context->hooks.end_weapon_f) context->hooks.end_weapon_f(context);
        return TRUE;
    }

    /* Calc Blows */
    if (context->flags & PAC_ONE_BLOW)
        context->blow_ct = 1;
    else
    {
        context->blow_ct = _get_num_blows(context);
        if (context->hooks.mod_blows_f)
            context->hooks.mod_blows_f(context);
    }

    /* Perform Blows */
    for (i = 0; i < context->blow_ct; i++)
    {
        if (context->stop) break;
        plr_hit_mon(context);
    }

    /* After Effects */
    if (context->drain_hits && one_in_(4))
        virtue_add(VIRTUE_UNLIFE, 1);
    if (!context->stop && context->hits)
        fear_p_touch_m(context->mon);
    if (context->do_quake)
    {
        earthquake(plr->pos, 10);
        if (context->obj)
            obj_learn_slay(context->obj, OF_IMPACT, "causes <color:U>Earthquakes</color>");
    }
    if (context->hooks.end_weapon_f) context->hooks.end_weapon_f(context);
    return TRUE;
}

/*************************************************************************
 * Test for a single hit
 *************************************************************************/
bool plr_check_hit(plr_attack_ptr context)
{
    int skill;

    /* stealth techniques on the first blow only ... monster must be visible */
    if (context->counter == 1 && context->mon->ml && !_icky_weapon(context))
    {
        if (plr->ambush)
        {
            /* always works against sleeping monsters (and never misses!) */
            bool do_it = mon_tim_find(context->mon, MT_SLEEP)
                      || mon_tim_find(context->mon, T_PARALYZED)
                      || plr->innocence; /* You brute! */
            /* sometimes works if invisible */
            if (!do_it && (plr->special_defense & DEFENSE_INVISIBLE))
            {
                int tmp = plr->lev * 6 + (plr->skills.stl + 10) * 4;
                if (plr->cursed & OFC_AGGRAVATE) tmp /= 2;
                if (randint0(tmp) > context->race->alloc.lvl + 20)
                    do_it = TRUE;
            }
            /* sometimes works if monster not aware of plr */
            if (!do_it && (context->mon->mflag & MFLAG_IGNORE_PLR))
            {
                int tmp = plr->lev * 2 + (plr->skills.stl + 10) * 4/3;
                if (plr->cursed & OFC_AGGRAVATE) tmp /= 2;
                if (randint0(tmp) > context->race->alloc.lvl + 20)
                {
                    context->mon->mflag &= ~MFLAG_IGNORE_PLR; /* he's aware of you now! */
                    do_it = TRUE;
                }
            }
            /* sometimes works if monster stuck in a web, confused, or blinded ...
             * note that plr aggravation does not biff the odds here */
            if ( !do_it
              && ( (context->mon->mflag2 & MFLAG2_WEB)
                || mon_tim_find(context->mon, T_CONFUSED)
                || mon_tim_find(context->mon, T_BLIND) ) )
            {
                int tmp = plr->lev * 2 + (plr->skills.stl + 10) * 4/3;
                if (randint0(tmp) > context->race->alloc.lvl + 20)
                    do_it = TRUE;
            }
            if (do_it)
            {
                context->technique = plr->ambush;
                cmsg_format(TERM_L_GREEN, "You cruelly attack %s!", context->mon_name_obj);
                return TRUE;
            }
        }
        if ((plr->special_defense & NINJA_S_STEALTH) && plr->pclass != CLASS_NECROMANCER)
        {
            int tmp = plr->lev * 6 + (plr->skills.stl + 10) * 4;
            if (plr->monlite && context->mode != PLR_HIT_RUSH_ATTACK) tmp /= 3;
            if (plr->cursed & OFC_AGGRAVATE) tmp /= 2;
            if (context->race->alloc.lvl > (plr->lev * plr->lev / 20 + 10)) tmp /= 3;
            if (randint0(tmp) > context->race->alloc.lvl + 20)
            {
                context->technique = 250 + plr->lev*4;
                cmsg_format(TERM_L_GREEN, "You make a surprise attack, and hit %s with a powerful blow!", context->mon_name_obj);
                return TRUE;
            }
        }
    }

    skill = context->skill;
    if (plr_tim_find(T_STUN))
        skill -= skill * MIN(100, plr_tim_amount(T_STUN)) / 150;

    if (test_hit_norm(skill, mon_ac(context->mon), context->mon->ml))
    {
        if (plr->backstab && mon_tim_find(context->mon, T_FEAR) && context->mon->ml)
        {
            context->technique = plr->backstab;
            cmsg_format(TERM_L_GREEN, "You backstab %s!",  context->mon_name_obj);
        }
        return TRUE;
    }
    return FALSE;
}

/*************************************************************************
 * Auras
 *************************************************************************/
static bool _skip_aura(plr_attack_ptr context, int gf)
{
    gf_info_ptr info = gf_lookup(gf);
    if (info)
    {
        if (plr->riding == context->mon->id)
        {
            if (!(info->flags & GFF_RIDING))
                return TRUE;
            if (!(info->flags & (GFF_RESIST | GFF_RESIST_HI))) return FALSE;
            if (plr->resist[gf] > 2)
                return TRUE;
        }
        else
        {
            if (!(info->flags & (GFF_RESIST | GFF_RESIST_HI))) return FALSE;
            if (res_pct(gf) >= 100)
                return TRUE;
        }
    }
    return FALSE;
}
static void _apply_auras(plr_attack_ptr context)
{
    mon_aura_ptr aura;
    for (aura = context->race->auras; aura; aura = aura->next)
    {
        int dam;
        if (aura->pct && randint1(100) > aura->pct) continue;
        if (_skip_aura(context, aura->gf)) continue;
        dam = dice_roll(aura->dam);
        if (!dam) continue;
        gf_affect_p(who_create_mon(context->mon), aura->gf, dam, GF_AFFECT_AURA);
        mon_lore_aura(context->mon, aura);
        if (_stop_attack(context)) return;
    }
}
void plr_on_hit_mon(plr_attack_ptr context)
{
    /* beholders gaze on their enemies without touching (even as a ranged attack())
     * staffmasters gain a quick strike that avoids monster auras */
    if (plr->prace == RACE_MON_BEHOLDER || plr->lightning_reflexes)
        return;

    /* Some classes have speciality attacks that are ranged: Samurai, Archaeologist, ...
     * These now skip all auras, not just retaliation. This makes more sense. For example,
     * the "Extended Crack" ability is a more distant melee attack, and the player never
     * gets close enough to the monster to get burned, etc. */
    if (context->flags & PAC_NO_AURA)
        return;

    if ( !mon_attack_current() /* avoid retaliatory cycles */
      && mon_can_retaliate(context->mon)
      && context->retaliation_ct < 1 + context->blow_ct/3
      && !mon_tim_find(context->mon, T_CONFUSED)
      && !mon_tim_find(context->mon, T_PARALYZED)
      && randint0(150) < context->race->alloc.lvl )
    {
        mon_retaliate_plr(context->mon);
        context->retaliation_ct++;
        if (_stop_attack(context)) return;
    }
    _apply_auras(context);
}
void plr_on_touch_mon(mon_ptr mon)
{
    plr_attack_t context = {0};

    /* fake a context */
    context.mon = mon;
    _init_race(&context);
    context.mon_pos = mon->pos;
    context.plr_pos = plr->pos;

    /* apply auras */
    plr_on_hit_mon(&context);
}
void plr_process_riding(void)
{
    plr_attack_t context = {0};
    mon_ptr mount = plr_riding_mon();

    if (!mount) return;

    /* fake a context */
    context.mon = mount;
    _init_race(&context);
    context.mon_pos = context.mon->pos;
    context.plr_pos = plr->pos;

    /* apply auras */
    _apply_auras(&context);
}

/*************************************************************************
 * Calculate Number of Attacks per Round
 *************************************************************************/
/* DEX gives the range of blows allowed, and STR is used to calculate
 * a factor for straight line interpolation between min and max.
 * The slope of the interpolation depends on the weapon weight, 
 * the player's strength, whether they are wielding with two hands, etc */
typedef struct { int min; int max; } _range_t;
#define _MAX_STAT_IND 40 - 3 + 1
static _range_t _blows_range[_MAX_STAT_IND] =
{
    {   0, 200}       /* 3 */,
    {   0, 200}       /* 4 */,
    {  10, 200}       /* 5 */,
    {  20, 210}       /* 6 */,
    {  30, 220}       /* 7 */,
    {  40, 230}       /* 8 */,
    {  50, 240}       /* 9 */,
    {  60, 250}       /* 10 */,
    {  70, 260}       /* 11 */,
    {  80, 270}       /* 12 */,
    {  90, 280}       /* 13 */,
    { 100, 290}       /* 14 */,
    { 110, 300}       /* 15 */,
    { 120, 350}       /* 16 */,
    { 130, 400}       /* 17 */,
    { 140, 450}       /* 18/00-18/09 */,
    { 150, 460}       /* 18/10-18/19 */,
    { 160, 470}       /* 18/20-18/29 */,
    { 170, 480}       /* 18/30-18/39 */,
    { 180, 490}       /* 18/40-18/49 */,
    { 190, 500}       /* 18/50-18/59 */,
    { 200, 520}       /* 18/60-18/69 */,
    { 210, 540}       /* 18/70-18/79 */,
    { 220, 560}       /* 18/80-18/89 */,
    { 230, 580}       /* 18/90-18/99 */,
    { 240, 600}       /* 18/100-18/109 */,
    { 250, 610}       /* 18/110-18/119 */,
    { 260, 620}       /* 18/120-18/129 */,
    { 280, 630}       /* 18/130-18/139 */,
    { 300, 640}       /* 18/140-18/149 */,
    { 320, 650}       /* 18/150-18/159 */,
    { 340, 660}       /* 18/160-18/169 */,
    { 350, 670}       /* 18/170-18/179 */,
    { 360, 680}       /* 18/180-18/189 */,
    { 370, 690}       /* 18/190-18/199 */,
    { 380, 700}       /* 18/200-18/209 */,
    { 390, 725}       /* 18/210-18/219 */,
    { 400, 750}       /* 18/220+ */
};
static void _calc_blows_normal(plr_attack_info_ptr info, int str_idx, int dex_idx)
{
    obj_ptr  obj;
    int      blow_str_idx, div, mul, wgt;
    _range_t rng = _blows_range[dex_idx];

    obj = equip_obj(info->slot);
    if (!obj) return; /* paranoia */

    wgt = obj->weight;
    div = wgt < info->blows_calc.wgt ? info->blows_calc.wgt : wgt;
    mul = info->blows_calc.mul + info->giant_wield * 10;

    blow_str_idx = adj_str_blow[str_idx] * mul / div; /* Scaled by 10 */
    if (have_flag(info->paf_flags, PAF_TWO_HANDS))
    {
        if (!have_flag(info->paf_flags, PAF_NEEDS_TWO_HANDS))
            blow_str_idx += 10;
        if (prace_is_(RACE_MON_GIANT) && giant_is_favorite(obj))
            blow_str_idx += 10;
    }
    if (plr->pclass == CLASS_NINJA) blow_str_idx = MAX(0, blow_str_idx-10);
    if (blow_str_idx > 110) blow_str_idx = 110;

    /* straight line interpolation */
    info->base_blow = rng.min + (rng.max - rng.min) * blow_str_idx / 110;

    if (plr->pclass == CLASS_MAULER)
        info->base_blow = 100 + (info->base_blow - 100)*2/5;

    if (info->base_blow > info->blows_calc.max)
        info->base_blow = info->blows_calc.max;

    if (info->base_blow < 100)
        info->base_blow = 100;

    if (have_flag(info->paf_flags, PAF_HEAVY)) info->base_blow = 100;
    if (object_is_(obj, TV_SWORD, SV_POISON_NEEDLE)) info->base_blow = 100;
}
static void _calc_blows_doc(plr_attack_info_ptr info, doc_ptr doc)
{
    #if DEVELOPER
    if (0)
    {
        int old_blows = info->base_blow;
        int str_idx = plr->stat_ind[A_STR];
        int dex_idx = plr->stat_ind[A_DEX];
        int i, j;

        doc_insert(doc, "     <color:G>STR</color>\n");
        doc_insert(doc, " <color:U>DEX</color> <color:G>");
        for (i = 0; i < 10; i++)
        {
            if (str_idx + 3 + i > 40) break;
            doc_printf(doc, "%3d ", str_idx + 3 + i);
        }
        doc_insert(doc, "</color>\n");

        for (i = 0; i < 10; i++)
        {
            if (dex_idx + 3 + i > 40) break;
            doc_printf(doc, " <color:U>%3d</color> ", dex_idx + 3 + i);
            for (j = 0; j < 10; j++)
            {
                char color = 'w';
                if (str_idx + 3 + j > 40) break;
                _calc_blows_normal(info, str_idx + j, dex_idx + i);
                if (i == 0 && j == 0) color = 'B';
                doc_printf(doc, "<color:%c>%3d </color>", color, info->base_blow);
            }
            doc_newline(doc);
        }
        doc_newline(doc);

        info->base_blow = old_blows;
    }
    #endif
}
static void _calc_blows_monk(plr_attack_info_ptr info, int lvl, int dex_idx)
{
    int blow_base = lvl + adj_dex_blow[dex_idx];

    info->base_blow = 100;
    if (plr->pclass == CLASS_FORCETRAINER)
        info->base_blow += MIN(400, 400 * blow_base / 57);
    else if (plr->pclass == CLASS_MYSTIC)
        info->base_blow += MIN(450, 450 * blow_base / 60);
    else if (plr->pclass == CLASS_MONK)
        info->base_blow += MIN(600, 600 * blow_base / 60);
    else /* Skillmaster, Monastic Lich, etc */
        info->base_blow += MIN(500, 500 * blow_base / 60);

    if (heavy_armor())
    {
        info->base_blow /= 2;
        if (info->base_blow < 100) info->base_blow = 100;
    }
}
void plr_calc_blows_hand(int hand)
{
    plr_attack_info_ptr info = &plr->attack_info[hand];

    if (info->type == PAT_NONE) return; /* paranoia */
    if (info->type == PAT_MONK) _calc_blows_monk(info, plr->monk_lvl, plr->stat_ind[A_DEX]);
    _calc_blows_normal(info, plr->stat_ind[A_STR], plr->stat_ind[A_DEX]);
}
static int _calc_blows_innate(mon_blow_ptr blow, blows_calc_t info, int str_idx, int dex_idx)
{
    int      blow_str_idx, wgt, div, blows;
    _range_t rng;

    /* paranoia */
    if (!info.wgt) info.wgt = 70;
    if (!info.mul) info.mul = 55;
    if (!info.max) info.max = 100;

    wgt = _mon_blow_weight(blow, _plr_weight());
    div = MAX(info.wgt, wgt);
    rng = _blows_range[dex_idx];

    blow_str_idx = adj_str_blow[str_idx] * info.mul / div;
    if (blow_str_idx > 110) blow_str_idx = 110;

    /* straight line interpolation */
    blows = rng.min + (rng.max - rng.min) * blow_str_idx / 110;

    /* XXX monster mode races with multiple attack types gain extra blows
     * far too quickly: hounds and dragons. Also, some races have low max
     * values, such as Leprechauns and Liches. XXX */
    if (info.max < 500)
        blows = blows * info.max / 500;
    else if (info.max > 600) /* RACE_MON_HYDRA */
        blows = blows * info.max / 600;

    #if 0
    if (prace_is_(RACE_MON_HYDRA)) blows = blows * info.max / 600; /* linear re-scale */
    #endif

    if (blows < 100) blows = 100; /* XXX info.min */
    if (blows > info.max) blows = info.max;
    return blows;
}
static void _calc_blows_innate_doc(mon_blow_ptr blow, doc_ptr doc)
{
    #if DEVELOPER
    if (0)
    {
        int str_idx = plr->stat_ind[A_STR];
        int dex_idx = plr->stat_ind[A_DEX];
        int i, j;
        const int ct = 15;
        plr_race_ptr race = plr_race();
        blows_calc_t info;

        /* XXX a single plr_attack_info covers *all* innate attacks. many monster
         * forms only get a single class of attacks, but some have multiple types:
         * dragon claws vs dragon bites. we are after accurate "blow_calc" info for
         * this type of innate attack. this hook is generally called by calc_bonuses,
         * and its sole purpose is to recalc mon_blow->blows, so an extra call here
         * should be ok ... this is DEVELOPER mode, after all :) XXX */
        if (race->hooks.calc_innate_bonuses)
            race->hooks.calc_innate_bonuses(blow);
        info = plr->innate_attack_info.blows_calc;

        if (info.max <= 100) return;

        doc_insert(doc, "     <color:G>STR</color>\n");
        doc_insert(doc, " <color:U>DEX</color> <color:G>");
        for (i = 0; i < ct; i++)
        {
            if (str_idx + 3 + i > 40) break;
            doc_printf(doc, "%3d ", str_idx + 3 + i);
        }
        doc_insert(doc, "</color>\n");

        for (i = 0; i < ct; i++)
        {
            if (dex_idx + 3 + i > 40) break;
            doc_printf(doc, " <color:U>%3d</color> ", dex_idx + 3 + i);
            for (j = 0; j < ct; j++)
            {
                char color = 'w';
                int blows;
                if (str_idx + 3 + j > 40) break;
                blows = _calc_blows_innate(blow, info, str_idx + j, dex_idx + i);
                if (i == 0 && j == 0) color = 'B';
                doc_printf(doc, "<color:%c>%3d </color>", color, blows);
            }
            doc_newline(doc);
        }
        doc_printf(doc, " <color:D>wgt=%d; mul=%d; max=%d</color>\n", info.wgt, info.mul, info.max);
        doc_newline(doc);
    }
    #endif
}
void plr_calc_blows_innate(mon_blow_ptr blow)
{
    blow->blows = _calc_blows_innate(blow,
        plr->innate_attack_info.blows_calc,
        plr->stat_ind[A_STR],
        plr->stat_ind[A_DEX]
    );
}

/*************************************************************************
 * Display
 *************************************************************************/
void plr_attack_display(void)
{
    plr_attack_display_special(0, 0);
}
void plr_attack_display_special(int type, int flags)
{
    plr_attack_t context = {0};
    context.mode = type;
    context.flags = flags;
    plr_attack_display_aux(&context);
}
extern void plr_attack_display_aux(plr_attack_ptr context)
{
    doc_ptr doc = doc_alloc(80);

    plr_attack_display_doc(context, doc);
    if (doc_line_count(doc))
    {
        /* screen_load|save do nothing if already saved. I'll fix this someday, but
         * for now we need ugly hacks to prevent garbage when browsing spells.
         * these hacks only work if screen_depth is 1 and if clients know to redraw
         * their menus inside their menu loops */
        bool screen_hack = screen_is_saved();
        if (screen_hack) screen_load(); /* screen_depth back to 0 */
    
        screen_save(); /* <=== bug: this would not save our current state without the screen_hack */
        doc_display(doc, "Melee", 0);
        screen_load(); /* <=== bug: this would not erase our drawing without the screen_hack */

        if (screen_hack) screen_save(); /* screen_depth back to 1 with (hopefully) the original screen */
    }
    else
        msg_print("You have no melee attacks.");

    doc_free(doc);
}

static void _display(plr_attack_ptr context, doc_ptr doc);
static void _display_innate(plr_attack_ptr context, doc_ptr doc);
static void _display_weapon(plr_attack_ptr context, doc_ptr doc);
static void _display_monk(plr_attack_ptr context, doc_ptr doc);

void plr_attack_display_doc(plr_attack_ptr context, doc_ptr doc)
{
    int i, ct = 0;

    /* setup special bonuses, if any (e.g. _super_attack_spell in race_troll.c) */
    context->flags |= PAC_DISPLAY;
    _plr_custom_init(context);
    if (context->hooks.begin_f)
        context->hooks.begin_f(context);
    
    /* normal attacks */
    if (!(context->flags & PAC_NO_WEAPON))
    {
        for (i = 0; i < MAX_HANDS; i++)
        {
            if (plr->attack_info[i].type == PAT_NONE) continue;
            if (plr_attack_init_hand(context, i))
            {
                _display(context, doc);
                if (context->hooks.end_weapon_f) context->hooks.end_weapon_f(context);
                ct++;
            }
        }
    }

    /* innate attacks */
    if (!(context->flags & PAC_NO_INNATE))
    {
        for (i = 0; i < vec_length(plr->innate_blows); i++)
        {
            if (plr_attack_init_innate(context, i))
            {
                if (context->blow->method == RBM_MONK) _display_monk(context, doc);
                else _display_innate(context, doc);
                if (context->hooks.end_weapon_f) context->hooks.end_weapon_f(context);
                ct++;
            }
        }
    }
    if (ct > 1)
        doc_printf(doc, " <color:G>Total Damage = <color:R>%d</color></color>\n", context->dam_total);
    if (context->hooks.end_f)
        context->hooks.end_f(context);
}

/*************************************************************************
 * Display: Normal Attacks
 *************************************************************************/
static void _display(plr_attack_ptr context, doc_ptr doc)
{
    if (context->info.type == PAT_MONK)
        _display_monk(context, doc);
    else if (context->obj)  /* check is paranoia */
        _display_weapon(context, doc);
}
static int _display_weapon_slay(slay_t crit, slay_t slay, bool force, int blows,
                                 dice_t d, cptr name, int color, doc_ptr doc)
{
    int roll, base1, base2, base3, dam1, dam2, dam3;
    const int scale = 100;

    roll = scale*d.dd*(d.ds + 1)/2;
    base1 = crit.mul*roll/100;
    base2 = base1 + scale*d.base + crit.add;
    base3 = blows*base2/100;

    if (force)
        slay.mul = slay.mul * 3/2 + 150;

    dam1 = slay.mul*(base1)/100 + scale*slay.add + crit.add;
    dam2 = dam1 + scale*d.base;
    dam3 = blows*dam2/100;

    if (plr_tim_find(T_STUN))
    {
        base3 -= base3 * MIN(100, plr_tim_amount(T_STUN)) / 150;
        dam3 -= dam3 * MIN(100, plr_tim_amount(T_STUN)) / 150;
    }
    doc_printf(doc, "<color:%c> %s</color><tab:8>", attr_to_attr_char(color), name);
    if (slay.mul == 100)
        doc_printf(doc, ": %4d\n", dam3/scale);
    else
        doc_printf(doc, ": %4d  (+%d)\n", dam3/scale, (dam3 - base3)/scale);
    return dam3/scale;
}
void crit_doc(doc_ptr doc, slay_t crit, int crit_chance)
{
    if (crit.add && 0) /* XXX add is usually insignificant (e.g. +1 or less) ... */
    {
        double mul = (double)crit.mul / 100.;
        double add = (double)crit.add / 100.;
        double chance = (double)crit_chance / 10.;
        doc_printf(doc, " %-7.7s: %.2fx+%.1f (%.1f%%)\n", "Crits", mul, add, chance); 
    }
    else if (crit.mul > 100)
    {
        double mul = (double)crit.mul / 100.;
        double chance = (double)crit_chance / 10.;
        doc_printf(doc, " %-7.7s: %.2fx (%.1f%%)\n", "Crits", mul, chance);
    }
}
static void _display_weapon(plr_attack_ptr context, doc_ptr doc)
{
    dice_t dice = {0};
    slay_t slay = {0};
    int crit_chance;
    slay_t crit;
    int to_d = context->to_d;
    int to_h = context->to_h;
    int i;
    int num_blow;
    bool force = FALSE, custom = FALSE;
    doc_ptr cols[2] = {0};

    assert(context->obj);
    assert(context->hand != HAND_NONE);

    num_blow = context->info.base_blow + context->info.xtra_blow;
    dice.dd = context->obj->dd + context->info.to_dd;
    dice.ds = context->obj->ds + context->info.to_ds;
    if (obj_is_known(context->obj))
    {
        to_d += context->obj->to_d;
        to_h += context->obj->to_h;
    }
    if (have_flag(context->obj_flags, OF_BRAND_MANA))
    {
        caster_info *caster = get_caster_info();
        int          cost = 0;

        if (plr->pclass == CLASS_SAMURAI)
            cost = (1 + (dice.dd * dice.ds * 2 / 7));
        else
            cost = (1 + (dice.dd * dice.ds / 7));

        if (caster && (caster->options & CASTER_USE_AU))
        {
            cost *= 10;
            if (plr->au >= cost)
                force = TRUE;
        }
        else if (plr->csp >= cost)
            force = TRUE;
    }

    /* Display in 2 columns, side by side */
    cols[0] = doc_alloc(60);
    cols[1] = doc_alloc(10);

    /* Column #1 */
    object_desc(context->obj_name, context->obj, OD_COLOR_CODED | OD_NAME_AND_ENCHANT);
    if (prace_is_(RACE_MON_SWORD))
        doc_printf(cols[0], "<color:y> You    :</color> <indent><style:indent>%s</style></indent>\n", context->obj_name);
    else
        doc_printf(cols[0], "<color:y> Hand #%d:</color> <indent><style:indent>%s</style></indent>\n", context->hand+1, context->obj_name);

    doc_printf(cols[0], " %-7.7s: %d.%d lbs\n", "Weight", context->obj->weight/10, context->obj->weight%10);

    if (weaponmaster_get_toggle() == TOGGLE_SHIELD_BASH)
    {
        assert(context->obj->tval == TV_SHIELD);
        doc_printf(cols[0], " %-7.7s: %dd%d (%+d,%+d)\n", "Bash", dice.dd, dice.ds, to_h, to_d);
        doc_printf(cols[0], " %-7.7s: %s (%+d To Hit)\n",
                    "Profic",
                    skills_shield_describe_current(context->obj->sval),
                    skills_shield_calc_bonus(context->obj->sval));
    }
    else
    {
        doc_printf(cols[0], " %-7.7s: %s (%+d To Hit)\n",
                    "Profic",
                    skills_weapon_describe_current(context->obj->tval, context->obj->sval),
                    skills_weapon_calc_bonus(context->obj->tval, context->obj->sval));
    }
    doc_printf(cols[0], " %-7.7s: %d + %d = %d\n", "To Hit", to_h, context->info.dis_to_h, to_h + context->info.dis_to_h);
    doc_printf(cols[0], " %-7.7s: %d + %d = %d\n", "To Dam", to_d, context->info.dis_to_d, to_d + context->info.dis_to_d);
    doc_printf(cols[0], " %-7.7s: %d.%2.2d\n", "Blows", num_blow/100, num_blow%100);

    if (context->info.skill_mul < 1000)
    {
        doc_printf(cols[0], " %-7.7s: %d.%d%%\n", "Skill",
            context->info.skill_mul / 10,
            context->info.skill_mul % 10);
    }

    doc_printf(cols[0], "<color:G> %-7.7s</color>\n", "Damage");

    dice.base = to_d + context->info.dis_to_d;

    /* criticals and vorpals give a "base slay" */
    crit_chance = _plr_crit_chance(context);
    crit = _plr_crit_avg(context);
    crit_doc(cols[0], crit, crit_chance);

    if (have_flag(context->obj_flags, OF_VORPAL2))
    {
        crit.mul = crit.mul * 5 / 3;  /* 1 + 1/3(1 + 1/2 + ...) = 1.667x */
        crit.add = crit.add * 5 / 3;
    }
    else if (have_flag(context->obj_flags, OF_VORPAL) && plr->vorpal)
    {
        crit.mul = crit.mul * 11 / 8; /* 1 + 1/4(1 + 1/3 + ...) = 1.375x */
        crit.add = crit.add * 11 / 8;
    }
    else if (have_flag(context->obj_flags, OF_VORPAL) || plr->vorpal)
    {
        crit.mul = crit.mul * 11 / 9; /* 1 + 1/6(1 + 1/4 + ...) = 1.222x */
        crit.add = crit.add * 11 / 9;
    }

    slay.mul = 100;
    context->dam_total += _display_weapon_slay(crit, slay, FALSE, num_blow, dice, "Normal", TERM_WHITE, cols[0]);
    if (force)
        _display_weapon_slay(crit, slay, force, num_blow, dice, "Force", TERM_L_BLUE, cols[0]);

    /* slays */
    for (i = 0; ; i++)
    {
        slay_info_ptr info = &slay_tbl[i];
        slay_t slay = {0};
        if (!info->id) break;
        if (!have_flag(context->obj_flags, info->id)) continue;
        if (info->mask_id > 0 && have_flag(context->obj_flags, info->mask_id)) continue; /* Skip SLAY_ORC if KILL_ORC ... */
        slay = slay_info_apply(info, NULL, NULL);
        if (context->hooks.calc_slay_f)
        {
            slay_t tmp = context->hooks.calc_slay_f(context, &slay);
            if (tmp.id == slay.id) /* e.g. "Keiun-Kininken" increases power of existing slays */
            {
                slay = tmp;
                custom = TRUE;
            }
        }
        _display_weapon_slay(crit, slay, force, num_blow, dice, slay.name, TERM_YELLOW, cols[0]);
    }
    if (!custom && context->hooks.calc_slay_f) /* skip custom slay if hook improved an existing one */
    {
        slay_t dummy = {0};
        slay_t slay = context->hooks.calc_slay_f(context, &dummy);
        if (slay.name)
            _display_weapon_slay(crit, slay, force, num_blow, dice, slay.name, TERM_YELLOW, cols[0]);
    }
    /* brands */
    custom = FALSE;
    for (i = 0; ; i++)
    {
        slay_info_ptr info = &brand_tbl[i];
        slay_t brand = {0};
        if (!info->id) break;
        if (!have_flag(context->obj_flags, info->id)) continue;
        brand = slay_info_apply(info, NULL, NULL);
        if (context->hooks.calc_brand_f)
        {
            slay_t tmp = context->hooks.calc_brand_f(context, &brand);
            if (tmp.id == brand.id) /* e.g. Lightning Eagle improves existing OF_BRAND_ELEC to 7x */
            {
                brand = tmp;
                custom = TRUE;
            }
        }
        _display_weapon_slay(crit, brand, force, num_blow, dice, brand.name, TERM_RED, cols[0]);
    }
    if (!custom && context->hooks.calc_brand_f) /* e.g. Lightning Eagle is a mere 5x if no exising OF_BRAND_ELEC */
    {
        slay_t dummy = {0};
        slay_t brand = context->hooks.calc_brand_f(context, &dummy);
        if (brand.name)
            _display_weapon_slay(crit, brand, force, num_blow, dice, brand.name, TERM_RED, cols[0]);
    }

    /* info */
    if (have_flag(context->info.paf_flags, PAF_TWO_HANDS))
    {
        if (have_flag(context->info.paf_flags, PAF_NEEDS_TWO_HANDS))
            doc_insert(cols[0], " Your weapon requires two hands to wield properly.\n");
    }
    if (context->info.info)
    {
        byte a = context->info.info_attr;
        doc_printf(cols[0], "\n <indent><color:%c>%s</color></indent>\n", attr_to_attr_char(a), context->info.info);
    }

    /* Column #2 */
    doc_insert(cols[1], "<color:G>Accuracy</color>\n");
    doc_insert(cols[1], " AC Hit\n");

    doc_printf(cols[1], "%3d %2d%%\n", 25, _hit_pct(context->skill, 25, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 50, _hit_pct(context->skill, 50, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 75, _hit_pct(context->skill, 75, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 100, _hit_pct(context->skill, 100, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 125, _hit_pct(context->skill, 125, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 150, _hit_pct(context->skill, 150, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 175, _hit_pct(context->skill, 175, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 200, _hit_pct(context->skill, 200, TRUE));

    /* Assemble the result */
    doc_insert_cols(doc, cols, 2, 1);
    doc_free(cols[0]);
    doc_free(cols[1]);

    _calc_blows_doc(&context->info, doc);
}

/*************************************************************************
 * Display: Martial Arts (Hacked to work for possessor)
 *************************************************************************/
typedef struct {
    int dam_base;
    int dam_add;
    int crit_freq;
    slay_t crit;
} _monk_stats_t, *_monk_stats_ptr;
static _monk_stats_t _sample_monk(plr_attack_ptr context, int count)
{
    _monk_stats_t stats = {0};
    mon_blow_ptr  old = context->blow;
    cptr          tbl_name = plr->monk_tbl;
    int           i;
    int           to_d = context->to_d + context->info.dis_to_d;
    int           to_dd = context->info.to_dd;
    int           to_ds = context->info.to_ds;

    if (context->blow)
        tbl_name = context->blow->name;
    for (i = 0; i < count; i++)
    {
        int base, dd, ds, add;
        slay_t crit;
        context->blow = monk_choose_attack_plr(tbl_name);
        if (!context->blow->effect_ct) continue;
        crit = _plr_crit_avg(context);
        dd = context->blow->effects[0].dice.dd + to_dd;
        ds = context->blow->effects[0].dice.ds + to_ds;
        add = 100*context->blow->effects[0].dice.base + crit.add;
        base = 100*dd*crit.mul*(ds + 1)/200;
        if (plr->clp > 1000) /* RACE_MON_LICH ... scaling base dice only seems weak. */
        {
            base = base * plr->clp / 1000;
            add = add * plr->clp / 1000;
        }
        add += 100*to_d;
        stats.dam_base += base;
        stats.dam_add += add;
        stats.crit.mul += crit.mul;
        stats.crit.add += crit.add;
        stats.crit_freq += _plr_crit_chance(context);
    }
    stats.dam_base /= 100*count;
    stats.dam_add /= 100*count;
    stats.crit_freq /= count;
    stats.crit.mul /= count;
    stats.crit.add /= count;
    context->blow = old;
    return stats;
}
static int _display_monk_slay(_monk_stats_ptr stats, slay_t slay, bool force, int blows, cptr name, int color, doc_ptr doc)
{
    int dam, base;

    base = blows * (stats->dam_base + stats->dam_add) / 100;

    /* calculate dice multiplier from slays, boosted by mana brand, and scaled
     * by vorpal and crit average multipliers */
    if (force)
        slay.mul = slay.mul * 3/2 + 150;

    /* compute average damage. both blows and mult are scaled by 100 */
    dam = blows * (slay.mul*stats->dam_base/100 + slay.add + stats->dam_add) / 100;
    if (plr_tim_find(T_STUN))
    {
        base -= base * MIN(100, plr_tim_amount(T_STUN)) / 150;
        dam -= dam * MIN(100, plr_tim_amount(T_STUN)) / 150;
    }

    doc_printf(doc, "<color:%c> %s</color><tab:8>", attr_to_attr_char(color), name);
    if (slay.mul == 100)
        doc_printf(doc, ": %4d  (%d+%d)\n", dam, blows*stats->dam_base/100, blows*stats->dam_add/100);
    else
        doc_printf(doc, ": %4d  (+%d)\n", dam, (dam-base));
    return dam;
}
static void _display_monk(plr_attack_ptr context, doc_ptr doc)
{
    _monk_stats_t stats = _sample_monk(context, 1000);
    int           blows = 100, i;
    int           to_h = context->to_h + context->info.dis_to_h;
    int           to_d = context->to_d + context->info.dis_to_d;
    bool          force = have_flag(context->obj_flags, OF_BRAND_MANA), custom = FALSE;
    doc_ptr       cols[2] = {0};

    if (context->blow) /* possessor */
    {
        blows = context->blow->blows;
        blows += context->info.xtra_blow;
        if (context->blow->method == RBM_MONK && heavy_armor())
            blows /= 2;
    }
    else
    {
        blows = context->info.base_blow + context->info.xtra_blow;
    }
    cols[0] = doc_alloc(60);
    cols[1] = doc_alloc(10);

    /* First Column */
    doc_printf(cols[0], "<color:y> %-7.7s</color>: Your Fists\n", "Attack");
    if (context->blow)
    {
        cptr name = skills_innate_calc_name(context->blow);
        int  bonus = skills_innate_calc_bonus(name);
        to_h += bonus;
        doc_printf(cols[0], " %-7.7s: %s (%+d To Hit)\n", "Profic",
                    skills_innate_describe_current(name), bonus);
    }
    else
    {
        doc_printf(cols[0], " %-7.7s: %s (%+d To Hit)\n", "Profic",
            skills_martial_arts_describe_current(), skills_martial_arts_calc_bonus());
    }
    doc_printf(cols[0], " %-7.7s: %d\n", "To Hit", to_h);
    doc_printf(cols[0], " %-7.7s: %d\n", "To Dam", to_d);

    doc_printf(cols[0], " %-7.7s: %d.%2.2d\n", "Blows", blows/100, blows%100);
    crit_doc(cols[0], stats.crit, stats.crit_freq);
    {
        slay_t none = {0};
        none.mul = 100;
        context->dam_total += _display_monk_slay(&stats, none, FALSE, blows, "Normal", TERM_WHITE, cols[0]);
        if (force)
            _display_monk_slay(&stats, none, force, blows, "Force", TERM_L_BLUE, cols[0]);
    }
    /* slays */
    for (i = 0; ; i++)
    {
        slay_info_ptr info = &slay_tbl[i];
        slay_t slay = {0};
        if (!info->id) break;
        if (!have_flag(context->obj_flags, info->id)) continue;
        slay = slay_info_apply(info, NULL, NULL);
        if (context->hooks.calc_slay_f)
        {
            slay_t tmp = context->hooks.calc_slay_f(context, &slay);
            if (tmp.id == slay.id)
            {
                slay = tmp;
                custom = TRUE;
            }
        }
        _display_monk_slay(&stats, slay, force, blows, slay.name, TERM_YELLOW, cols[0]);
    }
    if (!custom && context->hooks.calc_slay_f)
    {
        slay_t dummy = {0};
        slay_t slay = context->hooks.calc_slay_f(context, &dummy);
        if (slay.name)
            _display_monk_slay(&stats, slay, force, blows, slay.name, TERM_YELLOW, cols[0]);
    }
    /* brands */
    custom = FALSE;
    for (i = 0; ; i++)
    {
        slay_info_ptr info = &brand_tbl[i];
        slay_t brand = {0};
        if (!info->id) break;
        if (!have_flag(context->obj_flags, info->id)) continue;
        brand = slay_info_apply(info, NULL, NULL);
        if (context->hooks.calc_brand_f)
        {
            slay_t tmp = context->hooks.calc_brand_f(context, &brand);
            if (tmp.id == brand.id)
            {
                brand = tmp;
                custom = TRUE;
            }
        }
        _display_monk_slay(&stats, brand, force, blows, brand.name, TERM_RED, cols[0]);
    }
    if (!custom && context->hooks.calc_brand_f)
    {
        slay_t dummy = {0};
        slay_t brand = context->hooks.calc_brand_f(context, &dummy);
        if (brand.name)
            _display_monk_slay(&stats, brand, force, blows, brand.name, TERM_RED, cols[0]);
    }

    if (context->info.info)
    {
        byte a = context->info.info_attr;
        doc_printf(cols[0], " <color:%c>%s</color>\n", attr_to_attr_char(a), context->info.info);
    }

    /* Second Column */
    doc_insert(cols[1], "<color:G>Accuracy</color>\n");
    doc_insert(cols[1], " AC Hit\n");

    doc_printf(cols[1], "%3d %2d%%\n", 25, _hit_pct(context->skill, 25, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 50, _hit_pct(context->skill, 50, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 75, _hit_pct(context->skill, 75, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 100, _hit_pct(context->skill, 100, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 125, _hit_pct(context->skill, 125, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 150, _hit_pct(context->skill, 150, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 175, _hit_pct(context->skill, 175, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 200, _hit_pct(context->skill, 200, TRUE));

    doc_insert_cols(doc, cols, 2, 1);
    doc_free(cols[0]);
    doc_free(cols[1]);
}

/*************************************************************************
 * Display: Innate Attacks
 *************************************************************************/
static cptr _effect_name(int effect)
{
    if (plr_mon_race_is_("v.aether")) return "Random"; /* XXX */
    switch (effect)
    {
    case RBE_HURT:        return "Hurt";
    case RBE_DRAIN_CHARGES: return "Drain Charges";
    case RBE_EAT_GOLD:
    case RBE_EAT_ITEM:
    case RBE_EAT_FOOD:    return "Steal";
    case RBE_SHATTER:     return "Shatter";
    case RBE_DRAIN_EXP:   return "<color:D>Lower Experience</color>";
    case RBE_DISEASE:     return "Disease";
    case RBE_VAMP:        return "<color:D>Vampiric</color>";
    case GF_MISSILE:      return "Damage";
    case GF_FEAR:     return "<color:r>Terrify</color>";
    }
    return gf_name(effect);
}
static bool _effect_show_dam(int effect)
{
    if (_skip_effect(effect)) return FALSE;
    switch (effect)
    {
    case RBE_DRAIN_CHARGES:
    case RBE_EAT_GOLD:
    case RBE_EAT_ITEM:
    case RBE_EAT_FOOD:
        return FALSE;
    }
    if (effect < GF_COUNT)
    {
        gf_info_ptr info = gf_lookup(effect);
        if (effect && !(info->flags & GFF_DAMAGE))
            return FALSE;
    }
    return TRUE;
}
static void _display_dice(dice_t dice, doc_ptr doc)
{
    if (dice_avg_roll(dice))
        doc_printf(doc, "(%s)", dice_format(dice));
}
static void _display_innate(plr_attack_ptr context, doc_ptr doc)
{
    int blows, base_dam = 0;
    dice_t d = {0};
    slay_t slay = {0};
    bool force = FALSE;
    int i;
    int to_h = context->to_h + context->blow->power/BTH_PLUS_ADJ + context->info.dis_to_h;
    int to_d = context->to_d + context->info.dis_to_d;
    mon_blow_info_ptr info = mon_blow_info_lookup(context->blow->method);
    cptr name = context->blow->name;
    doc_ptr cols[2] = {0};

    blows = context->blow->blows;
    if (context->which_blow == 0)
        blows += plr->innate_attack_info.xtra_blow;

    /* no display if this attack is disabled (cf race_golem.c's RBM_PUNCH) */
    if (!blows) return;

    cols[0] = doc_alloc(60);
    cols[1] = doc_alloc(10);

    /* First Column */
    if (!name) name = info->name;
    if (context->blow->effect_ct)
    {
        d = mon_blow_base_dice(context->blow);
        if (d.dd) d.dd += plr->innate_attack_info.to_dd;
    }
    doc_printf(cols[0], "<color:y> %-7.7s</color>: Your %s ", "Attack", name);
    _display_dice(d, cols[0]);
    doc_newline(cols[0]);

    {
        cptr name = skills_innate_calc_name(context->blow);
        int  bonus = skills_innate_calc_bonus(name);
        to_h += bonus;
        doc_printf(cols[0], " %-7.7s: %s (%+d To Hit)\n", "Profic",
                    skills_innate_describe_current(name), bonus);
    }

    doc_printf(cols[0], " %-7.7s: %d\n", "To Hit", to_h);
    doc_printf(cols[0], " %-7.7s: %d\n", "To Dam", to_d);

    doc_printf(cols[0], " %-7.7s: %d.%2.2d\n", "Blows", blows/100, blows%100);
    if (context->blow->weight)
        doc_printf(cols[0], " %-7.7s: %d.%d lbs\n", "Weight", context->blow->weight/10, context->blow->weight%10);

    doc_printf(cols[0], "<color:G> %-7.7s</color>\n", "Damage");

    for (i = 0; i < context->blow->effect_ct; i++)
    {
        mon_effect_ptr e = &context->blow->effects[i];
        if (!e->type) continue;
        if (_skip_effect(e->type)) continue;
        if (i == 0 && dice_avg_roll(d)) /* first effect behaves like normal weapon display */
        {
            int crit_chance = _plr_crit_chance(context);
            slay_t crit = _plr_crit_avg(context);
            int  vorpal_pct = _innate_vorpal_pct(context->blow);
            cptr name = "Normal";
            crit_doc(cols[0], crit, crit_chance);
            if (vorpal_pct)
            {
                /* 1 + p(1 + 1/4 + ...) = 1 + p(4/3) */
                crit.mul = crit.mul * (100 + 4*vorpal_pct/3)/100;
                crit.add = crit.add * (100 + 4*vorpal_pct/3)/100;
                doc_printf(cols[0], " %-7.7s: %d.%02dx\n", "Vorpal", crit.mul/100, crit.mul%100);
            }

            if (e->type != RBE_HURT)
                name = _effect_name(e->type);
            d.base += to_d;
            slay.mul = 100;
            base_dam = _display_weapon_slay(crit, slay, FALSE, blows, d, name, TERM_WHITE, cols[0]);
            context->dam_total += base_dam;
            if (force)
                base_dam = _display_weapon_slay(crit, slay, force, blows, d, "Force", TERM_L_BLUE, cols[0]);
            if ((context->blow->flags & MBF_TOUCH) && mon_blow_allow_slay(context->blow))
            {
                int j;
                for (j = 0; ; j++)
                {
                    slay_info_ptr info = &slay_tbl[j];
                    slay_t slay = {0};
                    if (!info->id) break;
                    if (!have_flag(context->obj_flags, info->id)) continue;
                    slay = slay_info_apply(info, NULL, NULL);
                    _display_weapon_slay(crit, slay, force, blows, d, slay.name, TERM_YELLOW, cols[0]);
                }
                for (j = 0; ; j++)
                {
                    slay_info_ptr info = &brand_tbl[j];
                    slay_t brand = {0};
                    if (!info->id) break;
                    if (!have_flag(context->obj_flags, info->id)) continue;
                    brand = slay_info_apply(info, NULL, NULL);
                    _display_weapon_slay(crit, brand, force, blows, d, brand.name, TERM_RED, cols[0]);
                }
            }
        }
        else
        {
            doc_printf(cols[0], " %s", _effect_name(e->type));
            if (_effect_show_dam(e->type))
            {
                int dam = dice_avg_roll(e->dice) * blows / 100;
                if (e->pct) dam = dam * e->pct / 100;
                doc_printf(cols[0], "<tab:8>: %4d  (+%d)", base_dam + dam, dam);
                if (e->pct)
                    doc_printf(cols[0], " %d%%", e->pct);
            }
            else if (e->pct)
                doc_printf(cols[0], "<tab:11> %d%%", e->pct);
            doc_newline(cols[0]);
        }
    }

    /* Second Column */
    doc_insert(cols[1], "<color:G>Accuracy</color>\n");
    doc_insert(cols[1], " AC Hit\n");

    doc_printf(cols[1], "%3d %2d%%\n", 25, _hit_pct(context->skill, 25, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 50, _hit_pct(context->skill, 50, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 75, _hit_pct(context->skill, 75, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 100, _hit_pct(context->skill, 100, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 125, _hit_pct(context->skill, 125, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 150, _hit_pct(context->skill, 150, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 175, _hit_pct(context->skill, 175, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 200, _hit_pct(context->skill, 200, TRUE));

    doc_insert_cols(doc, cols, 2, 1);
    doc_free(cols[0]);
    doc_free(cols[1]);

    _calc_blows_innate_doc(context->blow, doc);
}

