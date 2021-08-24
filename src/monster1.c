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
#include <assert.h>

void mon_lore_blow(monster_type *m_ptr, mon_blow_ptr blow, int options)
{
    if (is_original_ap_and_seen(m_ptr))
        mon_lore_aux_blow(m_ptr->race, blow, options);
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
                if (r_ptr->id == plr->monster_race_idx)
                    plr->window |= PW_MONSTER;
            }
        }
    }
}

void mon_lore_effect(monster_type *m_ptr, mon_effect_ptr effect)
{
    if (is_original_ap_and_seen(m_ptr))
        mon_lore_aux_effect(m_ptr->race, effect);
}

void mon_lore_aux_effect(monster_race *r_ptr, mon_effect_ptr effect)
{
    if (effect->lore < MAX_SHORT)
    {
        effect->lore++;
        if (r_ptr->id == plr->monster_race_idx)
            plr->window |= PW_MONSTER;
    }
}

/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
void roff_top(int r_idx)
{
    monster_race    *r_ptr = mon_race_lookup(r_idx);
    term_char_t      r_tc = mon_race_visual(r_ptr);
    term_char_t      r_atc = mon_race_visual_ascii(r_ptr);
    byte             a1 = r_atc.a;
    char             c1 = r_atc.c;
    byte             a2 = r_tc.a;
    char             c2 = r_tc.c;

    /* Clear the top line */
    Term_erase(0, 0, 255);

    /* Reset the cursor */
    Term_gotoxy(0, 0);

    /* A title (use "The" for non-uniques) */
    if (!mon_race_is_unique(r_ptr))
    {
        Term_addstr(-1, TERM_WHITE, "The ");
    }

    /* Dump the name */
    Term_addstr(-1, TERM_WHITE, r_ptr->name);

    /* Append the "standard" attr/char info */
    Term_addstr(-1, TERM_WHITE, " ('");
    Term_add_bigch(a1, c1);
    Term_addstr(-1, TERM_WHITE, "')");

    /* Append the "optional" attr/char info */
    Term_addstr(-1, TERM_WHITE, "/('");
    Term_add_bigch(a2, c2);
    Term_addstr(-1, TERM_WHITE, "'):");

    /* Wizards get extra info */
    if (plr->wizard)
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

    if (cave->type->id == D_SURFACE) return TRUE; /* XXX ignore hook on surface for S_EAGLE */
    if (r_ptr->alloc.flags & RFA_WILD_ONLY)
    {
        /* XXX s/b using mon_alloc_dungeon() instead of mon_hook_dungeon() */
        if (cave->type->id == D_MOUNTAIN && (r_ptr->alloc.flags & RFA_WILD_MOUNTAIN)) return TRUE;
        return FALSE;
    }
    else
        return TRUE;
}

void set_temp_friendly(monster_type *m_ptr)
{
    if (!have_flag(m_ptr->smart, SM_FRIENDLY))
    {
        add_flag(m_ptr->smart, SM_FRIENDLY);
        add_flag(m_ptr->smart, SM_TEMP_FRIENDLY);
    }
}

void set_friendly(monster_type *m_ptr)
{
    add_flag(m_ptr->smart, SM_FRIENDLY);
}

static void set_pet_aux(monster_type *m_ptr, bool temp)
{
    if (!mon_is_pet(m_ptr))
    {
        if (m_ptr->pack)  /* leave my current pack in order to join the plr (plr_pack) */
            mon_pack_remove(m_ptr->pack, m_ptr);
    }

    if (!temp)
        quests_on_kill_mon(m_ptr);

    add_flag(m_ptr->smart, SM_PET);
    if (temp)
        add_flag(m_ptr->smart, SM_TEMP_PET);

    /* forget old allegiances */
    m_ptr->align = m_ptr->race->align;
    /* XXX align with plr is probably a bad idea, since plr->align is so changeable */

    mon_pack_add(plr_pack(), m_ptr);
    check_pets_num_and_align();
    if (plr->pet_extra_flags & PF_HILITE)
        draw_pos(m_ptr->pos);
}
void set_temp_pet(monster_type *m_ptr)
{
    set_pet_aux(m_ptr, TRUE);
}
void set_pet(monster_type *m_ptr)
{
    assert(!mon_is_pet(m_ptr));
    if (mon_is_pet(m_ptr)) return;
    set_pet_aux(m_ptr, FALSE);
}

/*
 * Makes the monster hostile towards the player
 */
void set_hostile(monster_type *m_ptr)
{
    bool was_pet = FALSE;
    if (mon_is_pet(m_ptr))
    {
        was_pet = TRUE;
        mon_pack_remove(plr_pack(), m_ptr);
        check_pets_num_and_align();
    }

    remove_flag(m_ptr->smart, SM_PET);
    remove_flag(m_ptr->smart, SM_TEMP_PET);
    remove_flag(m_ptr->smart, SM_FRIENDLY);
    remove_flag(m_ptr->smart, SM_TEMP_FRIENDLY);

    /* friendly uniques now wander the dungeon, but if attacked,
     * they should seek the plr instead */
    if (m_ptr->pack)
    {
        if (m_ptr->pack->ai == AI_WANDER || m_ptr->pack->ai == AI_HUNT)
            m_ptr->pack->ai = AI_SEEK;
    }
    if (was_pet && (plr->pet_extra_flags & PF_HILITE))
        draw_pos(m_ptr->pos);
}


/*
 * Anger the monster
 */
void anger_monster(monster_type *m_ptr)
{
    if (mon_is_friendly(m_ptr) || mon_is_temp_pet(m_ptr))
    {
        if (mon_show_msg(m_ptr))
        {
            char m_name[80];

            monster_desc(m_name, m_ptr, 0);
            msg_format("%^s gets angry!", m_name);
        }
        set_hostile(m_ptr);

        virtue_add(VIRTUE_INDIVIDUALISM, 1);
        if (plr_tim_find(T_BLESS_FRIENDSHIP))
        {
            plr_tim_remove(T_BLESS_FRIENDSHIP);
            virtue_add(VIRTUE_HONOUR, -5);
            virtue_add(VIRTUE_JUSTICE, -5);
            virtue_add(VIRTUE_COMPASSION, -5);
        }
        else if (plr_tim_find(T_BLESS_OBEDIENCE))
        {
            plr_tim_remove(T_BLESS_OBEDIENCE);
            virtue_add(VIRTUE_HONOUR, -5);
            virtue_add(VIRTUE_JUSTICE, -5);
            virtue_add(VIRTUE_COMPASSION, -5);
        }
        else
        {
            virtue_add(VIRTUE_HONOUR, -1);
            virtue_add(VIRTUE_JUSTICE, -1);
            virtue_add(VIRTUE_COMPASSION, -1);
        }
    }
}


/*
 * Check if two monsters are enemies
 */
bool are_enemies(monster_type *m_ptr, monster_type *n_ptr)
{
    if (mon_tim_find(m_ptr, MT_DISCORD) || mon_tim_find(n_ptr, MT_DISCORD))
        return TRUE;
    if ((m_ptr->mflag2 & MFLAG2_ILLUSION) && (n_ptr->mflag2 & MFLAG2_ILLUSION)) /* override alignment check */
        return FALSE;

    if (align_hostile(m_ptr->align, n_ptr->align))
    {
        /* No monster fighting (option) except involving pets */
        if (!allow_hostile_monster && !mon_is_pet(m_ptr) && !mon_is_pet(n_ptr)) return FALSE;

        if (!(m_ptr->mflag2 & MFLAG2_CHAMELEON) || !(n_ptr->mflag2 & MFLAG2_CHAMELEON)) return TRUE;
    }

    /* Hostile vs. non-hostile */
    if (mon_is_hostile(m_ptr) != mon_is_hostile(n_ptr))
    {
        return TRUE;
    }

    /* Default */
    return FALSE;
}

bool monster_magical(monster_race *r_ptr)
{
    if (r_ptr->spells && r_ptr->spells->freq >= 16)
        return TRUE;
    else
        return FALSE;
}

