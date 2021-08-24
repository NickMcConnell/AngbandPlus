#include "angband.h"

#include <assert.h>

static cptr _desc =
    "Giants are humanoids of immense stature. There are several types of giants. "
    "Fire, Frost and Storm giants are elemental giants and gain extra resistance, "
    "elementals slays and even elemental attacks of their respective element. Titans "
    "are powerful immortal beings of legend. Their attacks often confuse their foes and they "
    "rarely fight alone.\n \n"
    "Giants are monsters so cannot choose a normal class. Instead, they must rely on their "
    "superior physical stature to pummel their opponents with mighty blows. Against a distant "
    "foe, giants are capable of hurling large boulders with devastating effect.\n \n"
    "Giants use the same equipment slots as normal player races and have no innate attacks.";

static void _birth(void)
{
    object_type    forge;

    plr_mon_race_set("P.hill");

    object_prep(&forge, lookup_kind(TV_HARD_ARMOR, SV_CHAIN_MAIL));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SWORD, SV_EXECUTIONERS_SWORD));
    plr_birth_obj(&forge);

    plr_birth_food();
    plr_birth_light();
}

/**********************************************************************
 * Giant Weapon Restrictions
 * N.B. We could use weapon weight for this, but weight and size aren't
 *      always the same thing.
 **********************************************************************/
static bool _weapon_is_small(int tval, int sval)
{
    if (tval == TV_SWORD)
    {
        switch (sval)
        {
        case SV_BROKEN_DAGGER:
        case SV_BROKEN_SWORD:
        case SV_DAGGER:
        case SV_MAIN_GAUCHE:
        case SV_TANTO:
        case SV_RAPIER:
        case SV_SMALL_SWORD:
        case SV_BASILLARD:
        case SV_SHORT_SWORD:
        case SV_SABRE:
        case SV_DOKUBARI:
        case SV_HAYABUSA:
        case SV_DRAGON_FANG:
            return TRUE;
        }
    }
    if (tval == TV_POLEARM)
    {
        switch (sval)
        {
        case SV_HATCHET:
        case SV_SICKLE:
            return TRUE;
        }
    }
    if (tval == TV_HAFTED)
    {
        switch (sval)
        {
        case SV_WHIP:
        case SV_NUNCHAKU:
        case SV_JO_STAFF:
        case SV_THREE_PIECE_ROD:
            return TRUE;
        }
    }
    if (tval == TV_DIGGING)
    {
        switch (sval)
        {
        case SV_SHOVEL:
        case SV_GNOMISH_SHOVEL:
            return TRUE;
        }
    }
    return FALSE;
}
static bool _weapon_is_giant(int tval, int sval)
{
    if (tval == TV_SWORD)
    {
        switch (sval)
        {
        case SV_GREAT_SCIMITAR:
        case SV_FLAMBERGE:
        case SV_TWO_HANDED_SWORD:
        case SV_NO_DACHI:
        case SV_EXECUTIONERS_SWORD:
        case SV_ZWEIHANDER:
            return TRUE;
        }
    }
    if (tval == TV_POLEARM)
    {
        switch (sval)
        {
        case SV_LANCE:
        case SV_GREAT_AXE:
        case SV_TRIFURCATE_SPEAR:
        case SV_LOCHABER_AXE:
        case SV_HEAVY_LANCE:
        case SV_SCYTHE_OF_SLICING:
        case SV_DEATH_SCYTHE:
            return TRUE;
        }
    }
    if (tval == TV_HAFTED)
    {
        switch (sval)
        {
        case SV_TWO_HANDED_FLAIL:
        case SV_GREAT_HAMMER:
        case SV_MACE_OF_DISRUPTION:
        case SV_GROND:
            return TRUE;
        }
    }
    if (tval == TV_DIGGING)
    {
        switch (sval)
        {
        case SV_MATTOCK:
            return TRUE;
        }
    }
    return FALSE;
}
static void _calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    if (_weapon_is_small(obj->tval, obj->sval))
    {
        info->to_h -= 20;
        info->dis_to_h -= 20;
        info->to_d -= 5;
        info->dis_to_d -= 5;
        info->info_attr = TERM_RED;
        info->info = "Your weapon is too small.";
    }
    if (_weapon_is_giant(obj->tval, obj->sval))
    {
        info->to_h += 5 + plr->lev/5;
        info->dis_to_h += 5 + plr->lev/5;
        info->to_d += 3 + plr->lev/7;
        info->dis_to_d += 3 + plr->lev/7;
        info->info_attr = TERM_L_GREEN;
        info->info = "You love your weapon.";
    }
}

/**********************************************************************
 * Throw Monster :)
 **********************************************************************/
typedef struct {
    int m_idx;
    int wgt;
    int mult;
    int tdis;
    int tx;
    int ty;
} _monster_toss_info;
static void _monster_toss_imp(_monster_toss_info *info);

bool monster_toss(int m_idx)
{
    int dir, chance;
    _monster_toss_info info = {0};
    monster_type *m_ptr = dun_mon(cave, m_idx);
    monster_race *r_ptr = m_ptr->race;
    char m_name[MAX_NLEN];

    monster_desc(m_name, m_ptr, 0);

    if (mon_can_passwall(m_ptr))
    {
        msg_format("Failed! %^s is incoporeal!", m_name);
        return TRUE;
    }
    if (mon_race_is_unique(r_ptr) && r_ptr->alloc.lvl > plr->lev)
    {
        msg_format("Failed! %^s is too powerful!", m_name);
        return TRUE;
    }
    chance = plr->skills.thn + ((plr->lev + plr->to_h_m) * BTH_PLUS_ADJ);
    if (!test_hit_norm(chance, mon_ac(m_ptr), TRUE))
    {
        msg_format("You lose hold of %s.", m_name);
        return TRUE;
    }

    info.m_idx = m_idx;
    info.wgt = r_ptr->weight;

    /* Pick a target */
    info.mult = 1 + plr->lev / 10;
    if (plr->mighty_throw) info.mult++;
    {
        int mul, div;
        int wgt = info.wgt * 10;
        mul = 10 + 2 * (info.mult - 1);
        div = (wgt > 10) ? wgt : 10;
        div /= 2;
        info.tdis = (adj_str_blow[plr->stat_ind[A_STR]] + 250) * mul / div;

        if (info.tdis <= 1) /* Can the giant lift the monster? */
        {
            msg_format("Failed! %^s is too heavy!", m_name);
            return TRUE;
        }

        info.tdis += info.mult; /* Tossing only 2 squares is pretty lame! */
        if (info.tdis > mul) info.tdis = mul;

        project_length = info.tdis;
        command_dir = 0; /* Code is buggy asking for a direction 2x in a single player action! */
        plr->target = who_create_null();  /* TODO: Repeat command is busted ... */
        if (!get_aim_dir(&dir)) return FALSE;

        info.tx = plr->pos.x + 99 * ddx[dir];
        info.ty = plr->pos.y + 99 * ddy[dir];

        if ((dir == 5) && target_okay())
        {
            point_t pos = who_pos(plr->target);
            info.tx = pos.x;
            info.ty = pos.y;
        }

        project_length = 0;

        if (info.tx == plr->pos.x && info.ty == plr->pos.y) return FALSE;
    }

    /* Toss */
    _monster_toss_imp(&info);

    return TRUE;
}

static void _monster_toss_imp(_monster_toss_info *info)
{
    char m_name[MAX_NLEN];
    point_t path[32];
    int dam = 0;
    int cur_dis, ct;
    bool do_stun = FALSE;
    int chance;
    point_t tgt = point_create(info->tx, info->ty);
    point_t cur;
    mon_ptr tossed_mon = dun_mon(cave, info->m_idx);
    mon_race_ptr tossed_race = tossed_mon->race;

    chance = plr->skill_tht + (plr->lev * BTH_PLUS_ADJ);
    chance *= 2;

    monster_desc(m_name, tossed_mon, 0);
    msg_format("You toss %s.", m_name);

    dun_detach_mon(cave, tossed_mon->id);

    ct = project_path(path, info->tdis, plr->pos, tgt, 0);
    cur = plr->pos;

    for (cur_dis = 0; cur_dis < ct; )
    {
        /* Peek ahead at the next square in the path */
        point_t next = path[cur_dis];
        mon_ptr hit_mon = dun_mon_at(cave, next);
        dun_cell_ptr cell = dun_cell_at(cave, next);

        /* Always draw the visual effect ... Its nice to see
           monsters bouncing off nearby walls :) */
        if (cave_pt_is_visible(next) && plr_can_see(next))
        {
            term_char_t gc = mon_race_visual(tossed_race);

            print_rel(gc.c, gc.a, next.y, next.x);
            move_cursor_relative(next);
            Term_fresh();
            Term_xtra(TERM_XTRA_DELAY, delay_animation);
            draw_pos(next);
            Term_fresh();
        }
        else
        {
            Term_xtra(TERM_XTRA_DELAY, delay_animation);
        }

        /* Stopped by walls/doors/forest ... but allow hitting your target, please! */
        if (!cell_project(cell) && !hit_mon)
        {
            if (cell_is_wall(cell))
            {
                dam = plr->lev * info->mult;
                do_stun = one_in_(2);
            }
            else if (!mon_can_fly(tossed_mon))
                dam = plr->lev / 5 * info->mult;

            break;
        }

        /* Advance the distance */
        cur_dis++;

        /* Monster here, Try to hit it */
        if (hit_mon)
        {
            char m_name2[80];
            bool visible = hit_mon->ml;

            monster_desc(m_name2, hit_mon, 0);
            if (test_hit_fire(chance - cur_dis, mon_ac(hit_mon), visible))
            {
                bool fear = FALSE;
                slay_t crit = {0};

                if (!visible)
                    msg_format("%^s finds a mark.", m_name);
                else
                {
                    msg_format("%^s hits %s.", m_name, m_name2);
                    if (visible)
                    {
                        if (!plr_tim_find(T_HALLUCINATE)) mon_track(hit_mon);
                        health_track(hit_mon);
                    }
                }

                /***** The Damage Calculation!!! *****/
                dam = damroll(1 + MIN(10 + plr->lev/3, info->wgt / 25), 5);
                crit = crit_aux(CRIT_FREQ_ROLL, info->wgt * 10, chance - cur_dis);
                if (crit.id)
                {
                    dam = dam * crit.mul/100 + crit.add;
                    if (crit.msg) msg_print(crit.msg);
                }
                dam *= info->mult;
                if (dam < 0) dam = 0;
                dam = mon_damage_mod(hit_mon, dam, FALSE);

                if (mon_take_hit(hit_mon, dam, &fear, extract_note_dies(real_r_ptr(hit_mon))))
                {
                    /* Dead monster */
                    cur = next;
                }
                else
                {
                    message_pain(hit_mon->id, dam);
                    if (dam > 0)
                        anger_monster(hit_mon);

                    if (dam > 0 && hit_mon->cdis > 1)
                        mon_anger_spell(hit_mon, dam);

                    if (fear && visible)
                    {
                        sound(SOUND_FLEE);
                        msg_format("%^s flees in terror!", m_name2);
                    }
                }
            }
            else
            {
                msg_format("%^s misses %s.", m_name, m_name2);
            }

            /* Stop looking */
            break;
        }

        /* Save the new location */
        cur = next;

        if (cur_dis >= ct)
        {
            if (!mon_can_fly(tossed_mon))
                dam = plr->lev / 5 * info->mult;
            break;
        }
    }

    while (!dun_allow_mon_at(cave, cur))
        cur = scatter(cur, 1);
    dun_place_mon(cave, tossed_mon, cur);

    if (dam)
    {
        bool fear = FALSE;
        if (plr_attack_current()) /* _snatch_spell */
            plr_attack_current()->dam_total += dam;
        if (mon_take_hit(tossed_mon, dam, &fear, extract_note_dies(real_r_ptr(tossed_mon))))
        {
            /* Dead monster */
        }
        else
        {
            if ( !do_stun
              || _1d(100) <= mon_res_pct(tossed_mon, GF_STUN)
              || (mon_race_is_unique(tossed_race) && mon_save_stun(tossed_race->alloc.lvl, dam)) )
            {
            }
            else
            {
                msg_format("%^s is stunned.", m_name);
                mon_stun(tossed_mon, mon_stun_amount(dam));
            }

            message_pain(tossed_mon->id, dam);
            if (dam > 0)
                anger_monster(tossed_mon);

            if (fear)
                msg_format("%^s flees in terror!", m_name);
        }
    }
}

void monster_toss_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Throw Monster");
        break;
    case SPELL_DESC:
        var_set_string(res, "Throw an adjacent monster at chosen target, provided it is light enough, of course!");
        break;
    case SPELL_CAST: {
        mon_ptr mon = plr_target_adjacent_mon();
        var_set_bool(res, FALSE);
        if (!mon) break;
        var_set_bool(res, monster_toss(mon->id));
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}


/******************************************************************************
 *                 20             30            40
 * Hru: Hill Giant -> Stone Giant -> Rock Giant -> Hru
 ******************************************************************************/
static power_info _hru_powers[] = {
    { A_STR, {  5,  0, 50, throw_boulder_spell} },
    { A_STR, {  7,  0,  0, monster_toss_spell} },
    { A_STR, { 30,  5, 25, stone_to_mud_spell} },
    { A_STR, { 35, 10, 45, earthquake_spell} },
    { A_STR, { 40, 15, 50, stone_skin_spell} },
    {    -1, { -1, -1, -1, NULL}}
};
static int _hru_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _hru_powers);
}

static void _hru_calc_bonuses(void) {
    plr->sustain_str = TRUE;
    if (plr->lev >= 30)
    {
        plr->kill_wall = TRUE;
        plr->to_a += 10;
        plr->dis_to_a += 10;
    }
    if (plr->lev >= 40)
    {
        res_add(GF_SHARDS);
        plr->to_a += 15;
        plr->dis_to_a += 15;
    }
    plr->skill_tht += 2*plr->lev;
}
static void _hru_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_SUST_STR);
    if (plr->lev >= 40)
        add_flag(flgs, OF_RES_(GF_SHARDS));
}
static void _hru_gain_level(int new_level) {
    if (plr_mon_race_is_("P.hill") && new_level >= 20)
        plr_mon_race_evolve("P.stone");
    if (plr_mon_race_is_("P.stone") && new_level >= 30)
        plr_mon_race_evolve("P.rock");
    if (plr_mon_race_is_("P.rock") && new_level >= 40)
        plr_mon_race_evolve("P.hru");
}
static plr_race_ptr _hru_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[4] =  {"Hill Giant", "Stone Giant", "Rock Giant", "Hru"};
    int           rank = 0;

    if (plr->lev >= 20) rank++;
    if (plr->lev >= 30) rank++;
    if (plr->lev >= 40) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  30,   0,  13,   7,  75,  30};
    skills_t xs = { 50,  35,  50,   0,   0,   0, 170,  75};

        me = plr_race_alloc_aux(RACE_MON_GIANT, GIANT_HRU);

        me->skills = bs;
        me->extra_skills = xs;
        me->infra = 5;
        me->exp = 225;
        me->boss_r_idx = mon_race_parse("P.Atlas")->id;

        me->hooks.birth = _birth;
        me->hooks.get_powers = _hru_get_powers;
        me->hooks.calc_bonuses = _hru_calc_bonuses;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.get_flags = _hru_get_flags;
        me->hooks.gain_level = _hru_gain_level;
    }

    me->subname = titles[rank];
    me->stats[A_STR] =  7 + rank;
    me->stats[A_INT] = -5;
    me->stats[A_WIS] = -5;
    me->stats[A_DEX] = -3;
    me->stats[A_CON] =  4 + rank;
    me->stats[A_CHR] =  0 + rank/2;
    me->life = 110 + 5*rank;

    return me;
}

/******************************************************************************
 *                        20             30            40
 * Fire Giant: Hill Giant -> Stone Giant -> Fire Giant -> Elder Fire Giant
 ******************************************************************************/
static void _breathe_plasma_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Plasma");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes Plasma at your opponent.");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, plr->lev);
        break;
    default:
        breath_spell_innate(cmd, res, 3, GF_PLASMA, 3*plr->chp/10);
    }
}
static power_info _fire_powers[] = {
    { A_STR, {  5,  0, 50, throw_boulder_spell} },
    { A_STR, {  7,  0,  0, monster_toss_spell} },
    { A_STR, { 30,  5, 25, fire_bolt_spell} },
    { A_STR, { 32, 10, 50, fire_ball_spell} },
    { A_STR, { 35, 15, 60, plasma_bolt_spell} },
    { A_CON, { 40,  0, 65, breathe_fire_II_spell} },
    { A_CON, { 42, 10, 70, _breathe_plasma_spell} },
    {    -1, { -1, -1, -1, NULL}}
};
static int _fire_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _fire_powers);
}
static void _fire_calc_bonuses(void) {
    plr->sustain_str = TRUE;
    if (plr->lev >= 30)
    {
        res_add(GF_FIRE);
        plr->sh_fire = TRUE;
    }
    if (plr->lev >= 40)
        res_add(GF_FIRE);
    plr->skill_tht += 2*plr->lev;
}
static void _fire_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_SUST_STR);
    if (plr->lev >= 30)
    {
        add_flag(flgs, OF_RES_(GF_FIRE));
        add_flag(flgs, OF_AURA_FIRE);
    }
    if (plr->lev >= 40)
        add_flag(flgs, OF_BRAND_FIRE);
}
static void _fire_calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    _calc_weapon_bonuses(obj, info);
    if (plr->lev >= 40)
    {
        add_flag(info->obj_flags, OF_BRAND_FIRE);
        add_flag(info->obj_known_flags, OF_BRAND_FIRE);
    }
}
static void _fire_gain_level(int new_level) {
    if (plr_mon_race_is_("P.hill") && new_level >= 20)
        plr_mon_race_evolve("P.stone");
    if (plr_mon_race_is_("P.stone") && new_level >= 30)
        plr_mon_race_evolve("P.fire");
    if (plr_mon_race_is_("P.fire") && new_level >= 40)
        plr_mon_race_evolve("P.fire.elder");
}
static plr_race_ptr _fire_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[4] =  {"Hill Giant", "Stone Giant", "Fire Giant", "Elder Fire Giant"};
    int           rank = 0;

    if (plr->lev >= 20) rank++;
    if (plr->lev >= 30) rank++;
    if (plr->lev >= 40) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  30,   0,  13,   7,  75,  30};
    skills_t xs = { 50,  35,  50,   0,   0,   0, 170,  75};

        me = plr_race_alloc_aux(RACE_MON_GIANT, GIANT_FIRE);

        me->skills = bs;
        me->extra_skills = xs;
        me->infra = 5;
        me->exp = 200;
        me->boss_r_idx = mon_race_parse("P.Surtur")->id;

        me->hooks.birth = _birth;
        me->hooks.get_powers = _fire_get_powers;
        me->hooks.calc_bonuses = _fire_calc_bonuses;
        me->hooks.calc_weapon_bonuses = _fire_calc_weapon_bonuses;
        me->hooks.get_flags = _fire_get_flags;
        me->hooks.gain_level = _fire_gain_level;
    }

    me->subname = titles[rank];
    me->stats[A_STR] =  6 + rank;
    me->stats[A_INT] = -3;
    me->stats[A_WIS] = -3;
    me->stats[A_DEX] = -2;
    me->stats[A_CON] =  3 + rank;
    me->stats[A_CHR] =  0 + rank/2;
    me->life = 107 + 5*rank;

    return me;
}

/******************************************************************************
 *                         20             30             40
 * Frost Giant: Hill Giant -> Stone Giant -> Frost Giant -> Ice Giant
 ******************************************************************************/
static void _ice_storm_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Ice Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generate a huge ball of ice on chosen target.");
        break;
    default:
        ball_spell_aux(cmd, res, 5, GF_ICE, innate_dice(0, 0, 6*plr->lev));
    }
}
static power_info _frost_powers[] = {
    { A_STR, {  5,  0, 50, throw_boulder_spell} },
    { A_STR, {  7,  0,  0, monster_toss_spell} },
    { A_STR, { 30,  3, 25, frost_bolt_spell} },
    { A_STR, { 32,  9, 50, frost_ball_spell} },
    { A_STR, { 35, 15, 60, ice_bolt_spell} },
    { A_STR, { 40, 40, 60, _ice_storm_spell} },
    {    -1, { -1, -1, -1, NULL}}
};
static int _frost_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _frost_powers);
}
static void _frost_calc_bonuses(void) {
    plr->sustain_str = TRUE;
    if (plr->lev >= 30)
    {
        res_add(GF_COLD);
        plr->sh_cold = TRUE;
    }
    if (plr->lev >= 40)
        res_add(GF_COLD);
    plr->skill_tht += 2*plr->lev;
}
static void _frost_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_SUST_STR);
    if (plr->lev >= 30)
    {
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_AURA_COLD);
    }
    if (plr->lev >= 40)
        add_flag(flgs, OF_BRAND_COLD);
}
static void _frost_calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    _calc_weapon_bonuses(obj, info);
    if (plr->lev >= 40)
    {
        add_flag(info->obj_flags, OF_BRAND_COLD);
        add_flag(info->obj_known_flags, OF_BRAND_COLD);
    }
}
static void _frost_gain_level(int new_level) {
    if (plr_mon_race_is_("P.hill") && new_level >= 20)
        plr_mon_race_evolve("P.stone");
    if (plr_mon_race_is_("P.stone") && new_level >= 30)
        plr_mon_race_evolve("P.frost");
    if (plr_mon_race_is_("P.frost") && new_level >= 40)
        plr_mon_race_evolve("P.ice");
}
static plr_race_ptr _frost_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[4] =  {"Hill Giant", "Stone Giant", "Frost Giant", "Ice Giant"};
    int           rank = 0;

    if (plr->lev >= 20) rank++;
    if (plr->lev >= 30) rank++;
    if (plr->lev >= 40) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  30,   0,  13,   7,  75,  30};
    skills_t xs = { 50,  35,  50,   0,   0,   0, 170,  75};

        me = plr_race_alloc_aux(RACE_MON_GIANT, GIANT_FROST);

        me->skills = bs;
        me->extra_skills = xs;
        me->infra = 5;
        me->exp = 200;
        me->boss_r_idx = mon_race_parse("P.Ymir")->id;

        me->hooks.birth = _birth;
        me->hooks.get_powers = _frost_get_powers;
        me->hooks.calc_bonuses = _frost_calc_bonuses;
        me->hooks.calc_weapon_bonuses = _frost_calc_weapon_bonuses;
        me->hooks.get_flags = _frost_get_flags;
        me->hooks.gain_level = _frost_gain_level;
    }

    me->subname = titles[rank];
    me->stats[A_STR] =  6 + rank;
    me->stats[A_INT] = -3;
    me->stats[A_WIS] = -3;
    me->stats[A_DEX] = -2;
    me->stats[A_CON] =  3 + rank;
    me->stats[A_CHR] =  0 + rank/2;
    me->life = 107 + 5*rank;

    return me;
}

/*******************************************************************************************
 *                         20             30             40             45
 * Storm Giant: Hill Giant -> Stone Giant -> Cloud Giant -> Storm Giant -> Elder Storm Giant
 *******************************************************************************************/
static void _breathe_storm_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes storm winds at your opponent.");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, plr->lev);
        break;
    default:
        breath_spell_innate(cmd, res, 3, GF_STORM, 3*plr->chp/10);
    }
}
static void _lightning_storm_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Lightning Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generate a huge ball of lightning on chosen target.");
        break;
    default:
        ball_spell_aux(cmd, res, 5, GF_ELEC, innate_dice(0, 0, 7*plr->lev));
    }
}
static power_info _storm_powers[] = {
    { A_STR, {  5,  0, 50, throw_boulder_spell} },
    { A_STR, {  7,  0,  0, monster_toss_spell} },
    { A_STR, { 30,  3, 25, lightning_bolt_spell} },
    { A_STR, { 35, 10, 50, lightning_ball_spell} },
    { A_DEX, { 40,  2, 10, phase_door_spell} },
    { A_DEX, { 40, 10, 50, teleport_to_spell} },
    { A_STR, { 40, 40, 60, _lightning_storm_spell} },
    { A_CON, { 45, 10, 70, _breathe_storm_spell} },
    {    -1, { -1, -1, -1, NULL} }
};
static int _storm_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _storm_powers);
}
static void _storm_calc_bonuses(void) {
    plr->sustain_str = TRUE;
    if (plr->lev >= 30)
        res_add(GF_ELEC);

    if (plr->lev >= 40)
    {
        plr->sh_elec = TRUE;
        res_add(GF_ELEC);
    }
    plr->skill_tht += 2*plr->lev;
}
static void _storm_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_SUST_STR);
    if (plr->lev >= 30)
        add_flag(flgs, OF_RES_(GF_ELEC));
    if (plr->lev >= 40)
    {
        add_flag(flgs, OF_AURA_ELEC);
        add_flag(flgs, OF_BRAND_ELEC);
    }
}
static void _storm_calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    _calc_weapon_bonuses(obj, info);
    if (plr->lev >= 40)
    {
        add_flag(info->obj_flags, OF_BRAND_ELEC);
        add_flag(info->obj_known_flags, OF_BRAND_ELEC);
    }
}
static void _storm_gain_level(int new_level) {
    if (plr_mon_race_is_("P.hill") && new_level >= 20)
        plr_mon_race_evolve("P.stone");
    if (plr_mon_race_is_("P.stone") && new_level >= 30)
        plr_mon_race_evolve("P.cloud");
    if (plr_mon_race_is_("P.cloud") && new_level >= 40)
        plr_mon_race_evolve("P.storm");
    if (plr_mon_race_is_("P.storm") && new_level >= 45)
        plr_mon_race_evolve("P.storm.elder");
}
static plr_race_ptr _storm_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[5] =  {"Hill Giant", "Stone Giant", "Cloud Giant", "Storm Giant", "Elder Storm Giant"};
    int           rank = 0;

    if (plr->lev >= 20) rank++;
    if (plr->lev >= 30) rank++;
    if (plr->lev >= 40) rank++;
    if (plr->lev >= 45) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  30,   0,  13,   7,  70,  30};
    skills_t xs = { 50,  35,  50,   0,   0,   0, 150,  75};

        me = plr_race_alloc_aux(RACE_MON_GIANT, GIANT_STORM);

        me->skills = bs;
        me->extra_skills = xs;
        me->infra = 5;
        me->exp = 250;
        me->boss_r_idx = mon_race_parse("P.Typhoeus")->id;

        me->hooks.birth = _birth;
        me->hooks.get_powers = _storm_get_powers;
        me->hooks.calc_bonuses = _storm_calc_bonuses;
        me->hooks.calc_weapon_bonuses = _storm_calc_weapon_bonuses;
        me->hooks.get_flags = _storm_get_flags;
        me->hooks.gain_level = _storm_gain_level;
    }

    me->subname = titles[rank];
    me->stats[A_STR] =  5 + (rank + 1)/2;
    me->stats[A_INT] = -3;
    me->stats[A_WIS] = -3;
    me->stats[A_DEX] = -2;
    me->stats[A_CON] =  3 + (rank + 1)/2;
    me->stats[A_CHR] =  0 + rank/2;
    me->life = 105 + 3*rank;

    return me;
}

/******************************************************************************
 *                   20             30              40
 * Titan: Hill Giant -> Stone Giant -> Lesser Titan -> Greater Titan
 ******************************************************************************/
static power_info _titan_powers[] = {
    { A_STR, {  5,   0, 50, throw_boulder_spell} },
    { A_STR, {  7,   0,  0, monster_toss_spell} },
    { A_CHR, { 30,  30, 25, summon_monsters_spell} },
    { A_DEX, { 35,  10, 50, teleport_to_spell} },
    { A_CHR, { 40,  30, 50, summon_kin_spell} },
    { A_CON, { 45, 100, 95, healing_I_spell} }, /* N.B. Casting costs are paid with hp! */
    {    -1, { -1,  -1, -1, NULL}}
};
static int _titan_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _titan_powers);
}
static void _titan_calc_bonuses(void) {
    plr->sustain_str = TRUE;
    if (plr->lev >= 30)
    {
        res_add(GF_CHAOS);
        plr->pspeed += 2;
    }
    if (plr->lev >= 40)
        plr->pspeed += 3;
    plr->skill_tht += 2*plr->lev;
}
static void _titan_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_SUST_STR);
    if (plr->lev >= 30)
    {
        add_flag(flgs, OF_RES_(GF_CHAOS));
        add_flag(flgs, OF_SPEED);
    }
}
static void _titan_gain_level(int new_level) {
    if (plr_mon_race_is_("P.hill") && new_level >= 20)
        plr_mon_race_evolve("P.stone");
    if (plr_mon_race_is_("P.stone") && new_level >= 30)
        plr_mon_race_evolve("P.titan.lesser");
    if (plr_mon_race_is_("P.titan.lesser") && new_level >= 40)
        plr_mon_race_evolve("P.titan");
}
static plr_race_ptr _titan_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[4] =  {"Hill Giant", "Stone Giant", "Lesser Titan", "Greater Titan"};
    int           rank = 0;

    if (plr->lev >= 20) rank++;
    if (plr->lev >= 30) rank++;
    if (plr->lev >= 40) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  20,  32,   0,  15,  10,  75,  30};
    skills_t xs = { 55,  40,  50,   0,   0,   0, 175,  75};

        me = plr_race_alloc_aux(RACE_MON_GIANT, GIANT_TITAN);

        me->skills = bs;
        me->extra_skills = xs;
        me->infra = 5;
        me->exp = 300;
        me->boss_r_idx = mon_race_parse("P.Kronos")->id;

        me->hooks.birth = _birth;
        me->hooks.get_powers = _titan_get_powers;
        me->hooks.calc_bonuses = _titan_calc_bonuses;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.get_flags = _titan_get_flags;
        me->hooks.gain_level = _titan_gain_level;
    }

    me->subname = titles[rank];
    me->stats[A_STR] =  5 + rank;
    me->stats[A_INT] =  0 + rank;
    me->stats[A_WIS] =  0 + rank;
    me->stats[A_DEX] = -2;
    me->stats[A_CON] =  3 + rank;
    me->stats[A_CHR] =  3 + rank;
    me->life = 102 + 5*rank;

    return me;
}

static name_desc_t _info[GIANT_MAX] = {
    { "Fire Giant", "Fire Giants are massive giants of flame-> At high levels they become "
                        "wreathed in flames and even their weapons will burn their foes. Like "
                        "all giants, they may toss loose rubble at their foes. In addition, "
                        "they have a few fire based distance attacks up their sleeves." },
    { "Frost Giant", "Frost Giants are massive giants of ice. At high levels they become "
                        "wreathed in cold and even their weapons will freeze their foes. Like "
                        "all giants, they may toss loose rubble at their foes. In addition, "
                        "they have a few cold based distance attacks up their sleeves." },
    { "Storm Giant", "Storm Giants are massive giants of lightning. At high levels they become "
                        "wreathed in electricity and even their weapons will shock their foes. Like "
                        "all giants, they may toss loose rubble at their foes. In addition, "
                        "they have a few lightning based distance attacks up their sleeves." },
    { "Titan", "Titans are huge immortal beings of incredible strength and awesome power. "
                "Descended from Gaia and Uranus, they ruled during the legendary Golden Age, "
                "but were overthrown by the Olympians during the War of the Titans." },
    { "Hru", "Hrus are rock giants, made of stone. Their hides are tough and they are able "
                "to break through walls effortlessly. Hrus are incredibly strong, but lack "
                "much in the way of magical powers." },
};
/**********************************************************************
 * Public
 **********************************************************************/
bool giant_is_favorite(object_type *o_ptr)
{
    if (prace_is_(RACE_MON_GIANT))
        return _weapon_is_giant(o_ptr->tval, o_ptr->sval);
    return FALSE;
}

plr_race_ptr mon_giant_get_race(int psubrace)
{
    plr_race_ptr result = NULL;

    if (birth_hack && psubrace >= GIANT_MAX)
        psubrace = 0;

    assert(0 <= psubrace && psubrace < GIANT_MAX);

    switch (psubrace)
    {
    case GIANT_FIRE:
        result = _fire_get_race_t();
        break;
    case GIANT_FROST:
        result = _frost_get_race_t();
        break;
    case GIANT_STORM:
        result = _storm_get_race_t();
        break;
    case GIANT_TITAN:
        result = _titan_get_race_t();
        break;
    case GIANT_HRU:
        result = _hru_get_race_t();
        break;
    default: /* Birth Menus */
        result = _fire_get_race_t();
    }

    result->name = "Giant";
    result->desc = _desc;
    result->flags = RACE_IS_MONSTER;
    result->base_hp = 46;
    result->pseudo_class_id = CLASS_WARRIOR;
    result->shop_adjust = 130;

    if (birth_hack || spoiler_hack)
    {
        result->subname = _info[psubrace].name;
        result->subdesc = _info[psubrace].desc;
    }

    return result;
}

