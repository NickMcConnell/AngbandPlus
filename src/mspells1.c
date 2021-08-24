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
bool summon_possible(point_t pos)
{
    point_t p;
    for (p.y = pos.y - 2; p.y <= pos.y + 2; p.y++)
    {
        for (p.x = pos.x - 2; p.x <= pos.x + 2; p.x++)
        {
            if (!dun_pos_interior(cave, p)) continue;
            if (point_fast_distance(pos, p) > 2) continue;
            if (!point_project(pos, p)) continue;
            if (!dun_allow_mon_at(cave, p)) continue;
            return TRUE;
        }
    }
    return FALSE;
}


bool raise_possible(mon_ptr mon)
{
    point_t p;
    for (p.y = mon->pos.y - 5; p.y <= mon->pos.y + 5; p.y++)
    {
        for (p.x = mon->pos.x - 5; p.x <= mon->pos.x + 5; p.x++)
        {
            obj_ptr obj;
            if (!dun_pos_interior(cave, p)) continue;
            if (point_fast_distance(mon->pos, p) > 5) continue;
            if (!point_los(mon->pos, p)) continue;
            if (!point_project(mon->pos, p)) continue;
            for (obj = dun_obj_at(cave, p); obj; obj = obj->next)
            {
                mon_race_ptr race;
                if (obj->tval != TV_CORPSE) continue;
                race = corpse_race(obj);
                if (race && !align_hostile(mon->align, race->align))
                    return TRUE;
            }
        }
    }
    return FALSE;
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
        plr->update |= PU_BONUS;
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
    if (plr->mimic_form != MIMIC_NONE)
    {
        if (randint1(500) < get_race()->exp) return TRUE;
    }

    if (plr->riding && (dun_mon(cave, plr->riding)->mspeed < 25))
    {
        if (mon_tim_find(dun_mon(cave, plr->riding), T_FAST)) return TRUE;
    }

    if ( plr->prace == RACE_MON_MIMIC
      && !sym_equals(plr->current_r_idx, "@.mimic") )
    {
        int lvl = plr_mon_race()->alloc.lvl;
        if (lvl >= 50 && randint1(100) < lvl)
            return TRUE;
    }

    /* No need to cast dispel spell */
    return (FALSE);
}


