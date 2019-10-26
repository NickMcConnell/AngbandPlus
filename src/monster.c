#include "angband.h"

bool player_is_monster_king(void)
{
    plr_race_ptr race_ptr;
    if (p_ptr->pclass != CLASS_MONSTER) return FALSE;
    race_ptr = plr_race();
    if (!race_ptr->boss_r_idx) return FALSE;
    return unique_is_dead(race_ptr->boss_r_idx);
}

static void _calc_bonuses(void)
{
    if (player_is_monster_king())
        plr_tim_lock(T_HERO);
}

equip_template_ptr mon_get_equip_template(void)
{
    monster_race *r_ptr = mon_race_lookup(p_ptr->current_r_idx);
    return &b_info[r_ptr->body.body_idx];
}

cptr mon_name(int r_idx)
{
    if (r_idx)
        return r_name + mon_race_lookup(r_idx)->name;
    return ""; /* Birth Menu */
}

plr_class_ptr monster_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {
        me = plr_class_alloc(CLASS_MONSTER);
        me->name = "Monster";
        me->desc = "";
        me->life = 100;
        me->exp = 100;
        me->pets = 25;
        me->hooks.calc_bonuses = _calc_bonuses;
    }

    return me;
}

