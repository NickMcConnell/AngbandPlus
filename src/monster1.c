/* File: monster1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: describe monsters (using monster memory)
   Note: The (beloved?) roff_aux() has moved to mon_display.c
*/

#include "angband.h"


void mon_lore_1(monster_type *m_ptr, u32b mask)
{
    if (is_original_ap_and_seen(m_ptr))
        mon_lore_aux_1(mon_race(m_ptr), mask);
}

void mon_lore_2(monster_type *m_ptr, u32b mask)
{
    if (is_original_ap_and_seen(m_ptr))
        mon_lore_aux_2(mon_race(m_ptr), mask);
}

void mon_lore_3(monster_type *m_ptr, u32b mask)
{
    if (is_original_ap_and_seen(m_ptr))
        mon_lore_aux_3(mon_race(m_ptr), mask);
}

void mon_lore_r(monster_type *m_ptr, u32b mask)
{
    if (is_original_ap_and_seen(m_ptr))
        mon_lore_aux_r(mon_race(m_ptr), mask);
}

void mon_lore_blow(monster_type *m_ptr, mon_blow_ptr blow, int options)
{
    if (is_original_ap_and_seen(m_ptr))
        mon_lore_aux_blow(mon_race(m_ptr), blow, options);
}

void mon_lore_aux_blow(monster_race *r_ptr, mon_blow_ptr blow, int options)
{
    if (!(options & MON_BLOW_SILLY))
    {
        if ( (options & MON_BLOW_OBVIOUS)
          || (options & MON_BLOW_DAMAGE)
          || blow->lore > 10 )
        {
            if (blow->lore < MAX_SHORT)
            {
                blow->lore++;
                if (r_ptr->id == p_ptr->monster_race_idx)
                    p_ptr->window |= PW_MONSTER;
            }
        }
    }
}

void mon_lore_effect(monster_type *m_ptr, mon_effect_ptr effect)
{
    if (is_original_ap_and_seen(m_ptr))
        mon_lore_aux_effect(mon_race(m_ptr), effect);
}

void mon_lore_aux_effect(monster_race *r_ptr, mon_effect_ptr effect)
{
    if (effect->lore < MAX_SHORT)
    {
        effect->lore++;
        if (r_ptr->id == p_ptr->monster_race_idx)
            p_ptr->window |= PW_MONSTER;
    }
}

void mon_lore_spell(mon_ptr mon, mon_spell_ptr spell)
{
    if (is_original_ap_and_seen(mon))
        mon_lore_aux_spell(mon_race(mon), spell);
}

void mon_lore_aux_spell(mon_race_ptr race, mon_spell_ptr spell)
{
    if (spell->lore < MAX_SHORT)
    {
        spell->lore++;
        if (race->id == p_ptr->monster_race_idx)
            p_ptr->window |= PW_MONSTER;
    }
    mon_lore_aux_spell_turns(race);
}

void mon_lore_aux_spell_turns(mon_race_ptr race)
{
    u32b old = race->r_spell_turns;
    race->r_spell_turns++;
    if (race->r_spell_turns < old) /* wrap? */
        race->r_spell_turns = old;

    if (race->r_spell_turns != old && race->id == p_ptr->monster_race_idx)
        p_ptr->window |= PW_MONSTER;
}

void mon_lore_aux_1(monster_race *r_ptr, u32b mask)
{
    u32b old = r_ptr->r_flags1;

    r_ptr->r_flags1 |= (r_ptr->flags1 & mask);
    if (r_ptr->r_flags1 != old && r_ptr->id == p_ptr->monster_race_idx)
        p_ptr->window |= PW_MONSTER;
}

void mon_lore_aux_2(monster_race *r_ptr, u32b mask)
{
    u32b old = r_ptr->r_flags2;

    r_ptr->r_flags2 |= (r_ptr->flags2 & mask);
    if (r_ptr->r_flags2 != old && r_ptr->id == p_ptr->monster_race_idx)
        p_ptr->window |= PW_MONSTER;
}

void mon_lore_aux_3(monster_race *r_ptr, u32b mask)
{
    u32b old = r_ptr->r_flags3;

    r_ptr->r_flags3 |= (r_ptr->flags3 & mask);
    if (r_ptr->r_flags3 != old && r_ptr->id == p_ptr->monster_race_idx)
        p_ptr->window |= PW_MONSTER;
}

static void _mon_lore_aux_move(monster_race *r_ptr)
{
    u32b old = r_ptr->r_move_turns;
    r_ptr->r_move_turns++;
    if (r_ptr->r_move_turns < old) /* wrap? */
        r_ptr->r_move_turns = old;
    if (r_ptr->r_move_turns != old && r_ptr->id == p_ptr->monster_race_idx)
        p_ptr->window |= PW_MONSTER;
}

void mon_lore_move(monster_type *m_ptr)
{
    if (is_original_ap_and_seen(m_ptr))
        _mon_lore_aux_move(mon_race(m_ptr));
}

void mon_lore_aux_r(monster_race *r_ptr, u32b mask)
{
    u32b old = r_ptr->r_flagsr;

    r_ptr->r_flagsr |= (r_ptr->flagsr & mask);
    if (r_ptr->r_flagsr != old && r_ptr->id == p_ptr->monster_race_idx)
        p_ptr->window |= PW_MONSTER;
}

/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
void roff_top(int r_idx)
{
    monster_race    *r_ptr = mon_race_lookup(r_idx);

    byte        a1, a2;
    char        c1, c2;


    /* Access the chars */
    c1 = r_ptr->d_char;
    c2 = r_ptr->x_char;

    /* Access the attrs */
    a1 = r_ptr->d_attr;
    a2 = r_ptr->x_attr;


    /* Clear the top line */
    Term_erase(0, 0, 255);

    /* Reset the cursor */
    Term_gotoxy(0, 0);

    /* A title (use "The" for non-uniques) */
    if (!(r_ptr->flags1 & RF1_UNIQUE))
    {
        Term_addstr(-1, TERM_WHITE, "The ");
    }

    /* Dump the name */
    Term_addstr(-1, TERM_WHITE, (r_name + r_ptr->name));

    /* Append the "standard" attr/char info */
    Term_addstr(-1, TERM_WHITE, " ('");
    Term_add_bigch(a1, c1);
    Term_addstr(-1, TERM_WHITE, "')");

    /* Append the "optional" attr/char info */
    Term_addstr(-1, TERM_WHITE, "/('");
    Term_add_bigch(a2, c2);
    Term_addstr(-1, TERM_WHITE, "'):");

    /* Wizards get extra info */
    if (p_ptr->wizard)
    {
        char buf[6];

        sprintf(buf, "%d", r_idx);

        Term_addstr(-1, TERM_WHITE, " (");
        Term_addstr(-1, TERM_L_BLUE, buf);
        Term_addch(TERM_WHITE, ')');
    }
}

bool mon_hook_dungeon(int r_idx)
{
    monster_race *r_ptr = mon_race_lookup(r_idx);

    if (cave->dun_type_id == D_SURFACE) return TRUE; /* XXX ignore hook on surface for S_EAGLE */
    if (r_ptr->flags8 & RF8_WILD_ONLY)
    {
        /* XXX s/b using mon_alloc_dungeon() instead of mon_hook_dungeon() */
        if (cave->dun_type_id == D_MOUNTAIN && (r_ptr->flags8 & RF8_WILD_MOUNTAIN)) return TRUE;
        return FALSE;
    }
    else
        return TRUE;
}

void set_friendly(monster_type *m_ptr)
{
    m_ptr->smart |= (1U << SM_FRIENDLY);
}

void set_pet(monster_type *m_ptr)
{
    if (!is_pet(m_ptr)) check_pets_num_and_align(m_ptr, TRUE);

    quests_on_kill_mon(m_ptr);

    m_ptr->smart |= (1U << SM_PET);
    if (!(mon_race(m_ptr)->flags3 & (RF3_EVIL | RF3_GOOD)))
        m_ptr->sub_align = SUB_ALIGN_NEUTRAL;
}

/*
 * Makes the monster hostile towards the player
 */
void set_hostile(monster_type *m_ptr)
{
    if (is_pet(m_ptr)) check_pets_num_and_align(m_ptr, FALSE);

    m_ptr->smart &= ~(1U << SM_PET);
    m_ptr->smart &= ~(1U << SM_FRIENDLY);
}


/*
 * Anger the monster
 */
void anger_monster(monster_type *m_ptr)
{
    if (is_friendly(m_ptr))
    {
        char m_name[80];

        monster_desc(m_name, m_ptr, 0);
        msg_format("%^s gets angry!", m_name);

        set_hostile(m_ptr);

        virtue_add(VIRTUE_INDIVIDUALISM, 1);
        virtue_add(VIRTUE_HONOUR, -1);
        virtue_add(VIRTUE_JUSTICE, -1);
        virtue_add(VIRTUE_COMPASSION, -1);
    }
}


/*
 * Check if monster can cross terrain
 */
bool monster_can_cross_terrain(s16b feat, monster_race *r_ptr, u16b mode)
{
    feature_type *f_ptr = &f_info[feat];
    bool          lev = FALSE;

    if ((mode & CEM_RIDING) && p_ptr->prace == RACE_MON_RING && p_ptr->levitation)
        lev = TRUE;
    if ((mode & CEM_MIMIC) && p_ptr->levitation)
        lev = TRUE;

    /* Pattern */
    if (have_flag(f_ptr->flags, FF_PATTERN))
    {
        if (!(mode & CEM_RIDING))
        {
            if (!(r_ptr->flags7 & RF7_CAN_FLY)) return FALSE;
        }
        else
        {
            if (!(mode & CEM_P_CAN_ENTER_PATTERN)) return FALSE;
        }
    }

    /* "CAN" flags */
    if (have_flag(f_ptr->flags, FF_CAN_FLY) && ((r_ptr->flags7 & RF7_CAN_FLY) || lev)) return TRUE;
    if (have_flag(f_ptr->flags, FF_CAN_CLIMB) && (r_ptr->flags7 & RF7_CAN_CLIMB)) return TRUE;
    if (have_flag(f_ptr->flags, FF_CAN_SWIM) && ((r_ptr->flags7 & RF7_CAN_SWIM) || lev)) return TRUE;
    if (have_flag(f_ptr->flags, FF_CAN_PASS))
    {
        if ((r_ptr->flags2 & RF2_PASS_WALL) && (!(mode & CEM_RIDING) || p_ptr->pass_wall)) return TRUE;
    }

    if (!have_flag(f_ptr->flags, FF_MOVE)) return FALSE;

    /* Some monsters can walk on mountains */
    if (have_flag(f_ptr->flags, FF_MOUNTAIN) && (r_ptr->flags8 & RF8_WILD_MOUNTAIN)) return TRUE;

    /* Water */
    if (have_flag(f_ptr->flags, FF_WATER))
    {
        if (!(r_ptr->flags7 & RF7_AQUATIC))
        {
            /* Deep water */
            if (have_flag(f_ptr->flags, FF_DEEP)) return FALSE;

            /* Shallow water */
            else if (mon_auras_find(r_ptr, GF_FIRE)) return FALSE;
        }
    }

    /* Aquatic monster into non-water? */
    else if (r_ptr->flags7 & RF7_AQUATIC) return FALSE;

    /* Lava */
    if (have_flag(f_ptr->flags, FF_LAVA))
    {
        if (!(r_ptr->flagsr & RFR_EFF_IM_FIRE_MASK)) return FALSE;
    }

    return TRUE;
}


/*
 * Strictly check if monster can enter the grid
 */
bool monster_can_enter(int y, int x, monster_race *r_ptr, u16b mode)
{
    point_t pos = point_create(x, y);
    if (plr_at(pos)) return FALSE;
    if (mon_at(pos)) return FALSE;
    return monster_can_cross_terrain(cave_at(pos)->feat, r_ptr, mode);
}


/*
 * Check if this monster has "hostile" alignment (aux)
 */
static bool check_hostile_align(byte sub_align1, byte sub_align2)
{
    if (sub_align1 != sub_align2)
    {
        if (((sub_align1 & SUB_ALIGN_EVIL) && (sub_align2 & SUB_ALIGN_GOOD)) ||
            ((sub_align1 & SUB_ALIGN_GOOD) && (sub_align2 & SUB_ALIGN_EVIL)))
            return TRUE;
    }

    /* Non-hostile alignment */
    return FALSE;
}


/*
 * Check if two monsters are enemies
 */
bool are_enemies(monster_type *m_ptr, monster_type *n_ptr)
{
    monster_race *r_ptr = mon_race(m_ptr);
    monster_race *s_ptr = mon_race(n_ptr);

    if ((r_ptr->flags8 & (RF8_WILD_TOWN | RF8_WILD_ALL))
        && (s_ptr->flags8 & (RF8_WILD_TOWN | RF8_WILD_ALL)))
    {
        if (!is_pet(m_ptr) && !is_pet(n_ptr)) return FALSE;
    }

    /* Friendly vs. opposite aligned normal or pet */
    if (check_hostile_align(m_ptr->sub_align, n_ptr->sub_align))
    {
        /* No monster fighting (option) except involving pets */
        if (!allow_hostile_monster && !is_pet(m_ptr) && !is_pet(n_ptr)) return FALSE;

        if (!(m_ptr->mflag2 & MFLAG2_CHAMELEON) || !(n_ptr->mflag2 & MFLAG2_CHAMELEON)) return TRUE;
    }

    /* Hostile vs. non-hostile */
    if (is_hostile(m_ptr) != is_hostile(n_ptr))
    {
        return TRUE;
    }

    /* Default */
    return FALSE;
}


/*
 * Check if this monster race has "hostile" alignment
 * If user is player, m_ptr == NULL.
 */
bool monster_has_hostile_align(monster_type *m_ptr, int pa_good, int pa_evil, monster_race *r_ptr)
{
    byte sub_align1 = SUB_ALIGN_NEUTRAL;
    byte sub_align2 = SUB_ALIGN_NEUTRAL;

    if (m_ptr) /* For a monster */
    {
        sub_align1 = m_ptr->sub_align;
    }
    else /* For player */
    {
        if (p_ptr->align >= pa_good) sub_align1 |= SUB_ALIGN_GOOD;
        if (p_ptr->align <= pa_evil) sub_align1 |= SUB_ALIGN_EVIL;
    }

    /* Racial alignment flags */
    if (r_ptr->flags3 & RF3_EVIL) sub_align2 |= SUB_ALIGN_EVIL;
    if (r_ptr->flags3 & RF3_GOOD) sub_align2 |= SUB_ALIGN_GOOD;

    if (check_hostile_align(sub_align1, sub_align2)) return TRUE;

    /* Non-hostile alignment */
    return FALSE;
}


/*
 * Is the monster "alive"?
 *
 * Used to determine the message to print for a killed monster.
 * ("dies", "destroyed")
 */
bool monster_living(monster_race *r_ptr)
{
    /* Non-living, undead, or demon */
    if (r_ptr->flags3 & (RF3_DEMON | RF3_UNDEAD | RF3_NONLIVING))
        return FALSE;
    else
        return TRUE;
}

bool monster_magical(monster_race *r_ptr)
{
    if (r_ptr->spells && r_ptr->spells->freq >= 16)
        return TRUE;
    else
        return FALSE;
}

