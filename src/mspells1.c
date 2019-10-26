/* File: mspells1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Monster spells (attack player) */

#include "angband.h"


/*
 * Determine if there is a space near the player in which
 * a summoned creature can appear
 */
bool summon_possible(int y1, int x1)
{
    int y, x;

    /* Start at the player's location, and check 2 grids in each dir */
    for (y = y1 - 2; y <= y1 + 2; y++)
    {
        for (x = x1 - 2; x <= x1 + 2; x++)
        {
            /* Ignore illegal locations */
            if (!in_bounds(y, x)) continue;

            /* Only check a circular area */
            if (distance(y1, x1, y, x)>2) continue;

            /* ...nor on the Pattern */
            if (pattern_tile(y, x)) continue;

            /* Require empty floor grid in line of projection */
            if (cave_empty_bold(y, x) && projectable(y1, x1, y, x) && projectable(y, x, y1, x1)) return (TRUE);
        }
    }

    return FALSE;
}


bool raise_possible(monster_type *m_ptr)
{
    int xx, yy;
    int y = m_ptr->pos.y;
    int x = m_ptr->pos.x;

    for (xx = x - 5; xx <= x + 5; xx++)
    {
        for (yy = y - 5; yy <= y + 5; yy++)
        {
            obj_ptr obj;
            if (distance(y, x, yy, xx) > 5) continue;
            if (!los(y, x, yy, xx)) continue;
            if (!projectable(y, x, yy, xx)) continue;

            for (obj = obj_at_xy(xx, yy); obj; obj = obj->next)
            {
                if (obj->tval != TV_CORPSE) continue;
                if (!monster_has_hostile_align(m_ptr, 0, 0, mon_race_lookup(obj->pval)))
                    return TRUE;
            }
        }
    }
    return FALSE;
}


/*
 * Originally, it was possible for a friendly to shoot another friendly.
 * Change it so a "clean shot" means no equally friendly monster is
 * between the attacker and target.
 */
/*
 * Determine if a bolt spell will hit the player.
 *
 * This is exactly like "projectable", but it will
 * return FALSE if a monster is in the way.
 * no equally friendly monster is
 * between the attacker and target.
 */
bool clean_shot(int y1, int x1, int y2, int x2, bool friend)
{
    /* Must be the same as projectable() */

    int i;
    point_t p1 = point_create(x1, y1);
    point_t p2 = point_create(x2, y2);
    point_t p;

    int path_n = 0;
    point_t path[36];

    /* Check the projection path */
    path_n = project_path(path, MAX_RANGE, p1, p2, 0);

    /* No grid is ever projectable from itself */
    if (!path_n) return FALSE;

    /* Hit target? */
    if (!point_equals(path[path_n - 1], p2)) return FALSE;

    for (i = 0; i < path_n; i++)
    {
        mon_ptr mon;
        p = path[i];

        mon = mon_at(p);
        if (mon && !point_equals(p, p2))
        {
            if (friend == is_pet(mon)) return FALSE;
        }
        /* Pets may not shoot through the character - TNB */
        if (friend && plr_at(p)) return FALSE;
    }

    return TRUE;
}

u32b get_curse(int power, object_type *o_ptr)
{
    u32b new_curse;

    while(1)
    {
        new_curse = (1 << (randint0(MAX_CURSE)+4));
        if (power == 2)
        {
            if (!(new_curse & TRC_HEAVY_MASK)) continue;
        }
        else if (power == 1)
        {
            if (new_curse & TRC_SPECIAL_MASK) continue;
        }
        else if (power == 0)
        {
            if (new_curse & TRC_HEAVY_MASK) continue;
        }
        if (new_curse == OFC_LOW_MELEE && !obj_is_weapon(o_ptr)) continue;
        if (new_curse == OFC_LOW_AC && !obj_is_armor(o_ptr)) continue;
        break;
    }
    return new_curse;
}

static bool _object_is_any(object_type *o_ptr) { return TRUE; }
void curse_equipment(int chance, int heavy_chance)
{
    int slot = equip_random_slot(_object_is_any);
    if (slot)
    {
        bool         changed = FALSE;
        int          curse_power = 0;
        u32b         new_curse;
        object_type *o_ptr = equip_obj(slot);
        char         o_name[MAX_NLEN];

        if (!o_ptr) return;
        if (randint1(100) > chance) return;

        object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

        if (obj_has_flag(o_ptr, OF_BLESSED) && (randint1(888) > chance))
        {
            msg_format("Your %s resists cursing!", o_name);
            return;
        }

        if ((randint1(100) <= heavy_chance) &&
            (obj_is_art(o_ptr) || obj_is_ego(o_ptr)))
        {
            if (!(o_ptr->curse_flags & OFC_HEAVY_CURSE))
                changed = TRUE;
            o_ptr->curse_flags |= OFC_HEAVY_CURSE;
            o_ptr->curse_flags |= OFC_CURSED;
            curse_power++;
        }
        else
        {
            if (!obj_is_cursed(o_ptr))
                changed = TRUE;
            o_ptr->curse_flags |= OFC_CURSED;
        }
        if (heavy_chance >= 50) curse_power++;

        new_curse = get_curse(curse_power, o_ptr);
        if (!(o_ptr->curse_flags & new_curse))
        {
            changed = TRUE;
            o_ptr->curse_flags |= new_curse;
        }

        if (changed)
        {
            msg_format("There is a malignant black aura surrounding %s...", o_name);
            o_ptr->feeling = FEEL_NONE;
        }
        p_ptr->update |= PU_BONUS;
    }
}

/*
 * Check should monster cast dispel spell.
 */
bool dispel_check(int m_idx)
{
    monster_type *m_ptr = dun_mon(cave, m_idx);

    /* TODO: Monsters should have to learn this! */
    if (psion_mental_fortress() && !one_in_(12)) return FALSE;

    if (plr_tim_dispel_check(m_ptr)) return TRUE;

    /* Powerful Mimickry: Note Colossus and Demon-Lord have insane XP requirements,
       so will always trigger a dispel. */
    if (p_ptr->mimic_form != MIMIC_NONE)
    {
        if (randint1(500) < get_race()->exp) return TRUE;
    }

    if (p_ptr->riding && (dun_mon(cave, p_ptr->riding)->mspeed < 135))
    {
        if (mon_tim_find(dun_mon(cave, p_ptr->riding), T_FAST)) return TRUE;
    }

    if ( p_ptr->prace == RACE_MON_MIMIC
      && p_ptr->current_r_idx != MON_MIMIC )
    {
        int lvl = mon_race_lookup(p_ptr->current_r_idx)->level;
        if (lvl >= 50 && randint1(100) < lvl)
            return TRUE;
    }

    /* No need to cast dispel spell */
    return (FALSE);
}


