#include "angband.h"

bool player_is_monster_king(void)
{
    mon_race_ptr boss = NULL;
    if (plr->pclass != CLASS_MONSTER) return FALSE;
    boss = plr_boss_race();
    if (!boss) return FALSE;
    return mon_race_is_dead_unique(boss);
}

static void _calc_bonuses(void)
{
    if (player_is_monster_king())
        plr_tim_lock(T_HERO);
}

cptr mon_name(int r_idx)
{
    if (r_idx)
        return mon_race_lookup(r_idx)->name;
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

