/*
 *  Hooks and Callbacks for various classes
 */

#include "angband.h"

#include <assert.h>

bool plr_allow_martial_arts(void)
{
    if (get_class()->flags & CLASS_MARTIAL_ARTS) return TRUE;
    if (get_race()->flags & RACE_MARTIAL_ARTS) return TRUE;
    return FALSE;
}

/* In general, the class id is given by player_type.pclass. However, for various
 * monster races, the "class" behavior is actually determined by the race, and we
 * encode this desire with race_t.pseudo_class_id. As another twist, consider the
 * possessor. Here, the "class" behavior depends on the current body (see r_info.text).
 */
int plr_pseudo_class_id(void)
{
    int result = plr->pclass;

    if (result == CLASS_MONSTER)
    {
        switch (plr->prace)
        {
        case RACE_MON_POSSESSOR:
        case RACE_MON_MIMIC:
            if (plr->current_r_idx)
            {
                mon_race_ptr race = plr_mon_race();
                result = race->body.class_id;
                break;
            }
            /* FALL THROUGH */
        default:
        {
            race_t *race_ptr = get_race();
            if (race_ptr->pseudo_class_id != CLASS_NONE)
                result = race_ptr->pseudo_class_id;
        }
        }
    }
    return result;
}

class_t *get_class_aux(int pclass, int psubclass)
{
    return plr_class_aux(pclass, psubclass);
}

class_t *get_class(void)
{
    return plr_class();
}

caster_info *get_caster_info(void)
{
    caster_info *result = NULL;
    class_t *class_ptr = get_class();
    race_t *race_ptr = get_race();

    if (race_ptr->hooks.caster_info) /* Monster Races: Lich, Angel, Demon */
        result = (race_ptr->hooks.caster_info)();
    else if (class_ptr->hooks.caster_info)
        result = (class_ptr->hooks.caster_info)();
    return result;
}

int get_spell_stat(void)
{
    int          result = A_NONE;
    caster_info *caster_ptr = get_caster_info();

    if (caster_ptr)
        result = caster_ptr->which_stat;

    return result;
}

