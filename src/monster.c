#include "angband.h"

bool player_is_monster_king(void)
{
    race_t *race_ptr;
    if (p_ptr->pclass != CLASS_MONSTER) return FALSE;
    race_ptr = get_race();
    if (!race_ptr->boss_r_idx) return FALSE;
    if (r_info[race_ptr->boss_r_idx].max_num) return FALSE;
    return TRUE;
}

equip_template_ptr mon_get_equip_template(void)
{
    monster_race *r_ptr = &r_info[p_ptr->current_r_idx];
    return &b_info[r_ptr->body.body_idx];
}

cptr mon_name(int r_idx)
{
    if (r_idx)
        return r_name + r_info[r_idx].name;
    return ""; /* Birth Menu */
}

class_t *monster_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Monster";
        me.desc = "";
        me.life = 100;
        me.exp = 100;
        me.pets = 25;
        init = TRUE;
    }

    return &me;
}

