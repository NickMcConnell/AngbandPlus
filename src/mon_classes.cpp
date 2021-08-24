/* File: was monster2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 3, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "src/npp.h"


/*
 * Take note that the given monster just dropped some treasure
 *
 * Note that learning the "CHEST/GOOD"/"GREAT" flags gives information
 * about the treasure (even when the monster is killed for the first
 * time, such as uniques, and the treasure has not been examined yet).
 *
 * This "indirect" method is used to prevent the player from learning
 * exactly how much treasure a monster can drop from observing only
 * a single example of a drop.  This method actually observes how much
 * gold and items are dropped, and remembers that information to be
 * described later by the monster recall code.
 */
void lore_treasure(int m_idx, int num_item, int num_gold)
{
    monster_type *m_ptr = &mon_list[m_idx];
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &l_list[m_ptr->r_idx];

    /* Note the number of things dropped */
    if (num_item > l_ptr->drop_item) l_ptr->drop_item = num_item;
    if (num_gold > l_ptr->drop_gold) l_ptr->drop_gold = num_gold;


    /* Hack -- memorize the good/great flags */
    if (r_ptr->flags1 & (RF1_DROP_CHEST)) l_ptr->r_l_flags1 |= (RF1_DROP_CHEST);
    if (r_ptr->flags1 & (RF1_DROP_GOOD)) l_ptr->r_l_flags1 |= (RF1_DROP_GOOD);
    if (r_ptr->flags1 & (RF1_DROP_GREAT)) l_ptr->r_l_flags1 |= (RF1_DROP_GREAT);


    /* Update monster recall window */
    if (p_ptr->monster_race_idx == m_ptr->r_idx)
    {
        /* Window stuff */
        p_ptr->redraw |= (PR_WIN_MON_RECALL);
    }
}


monster_type::monster_type()
{
    monster_wipe();
}

/*
 * Wipe the monster_type class.
 * This function shoudld be used instead of WIPE command.
 * All variables in dungeon_type should be re-set in this function.
 * This function does not clear the monster pointer from dungeon_type.
 * It should be called directly only under unusual circumstances.
 */
void monster_type::monster_wipe()
{
    r_idx = 0;
    fy = fx = 0;
    hp = maxhp = 0;
    m_speed = 0;
    m_energy = 0;
    for (int i = 0; i < MON_TMD_MAX; i++) m_timed[i] = 0;
    cdis = 0;
    mflag = 0;
    m_color.setRgb(0,0,0,0);
    ml = project = sidebar = FALSE;
    hold_o_idx = 0;
    smart = 0;
    target_x = target_y = 0;
    min_range = best_range = mana = using_flow;

}

/*
return the monster idx based on the position in the dungeon
 */
s16b monster_type::get_mon_idx()
{
    return (dungeon_info[fy][fx].monster_idx);
}

bool monster_type::mon_fully_healthy()
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Monster is wounded */
    if (hp < maxhp) return (FALSE);
    if (mana < r_ptr->mana) return (FALSE);
    if (m_timed[MON_TMD_STUN]) return (FALSE);
    if (m_timed[MON_TMD_FEAR]) return (FALSE);
    if (m_timed[MON_TMD_CONF]) return (FALSE);

    /* Fully healthy */
    return (TRUE);
}


monster_lore::monster_lore()
{
    monster_lore_wipe();
}


//  Wipe the monster_lore class. Should only be called at the beginning of a game
void monster_lore::monster_lore_wipe()
{
    sights = deaths = pkills = tkills = 0;
    wake = ignore = xtra1 = xtra2 = 0;
    drop_gold = drop_item = 0;
    ranged = 0;
    for (int i = 0; i < MONSTER_BLOW_MAX; i++) blows[i] = 0;
    r_l_flags1 = r_l_flags2 = r_l_flags3 = r_l_flags4 = 0;
    r_l_flags5 = r_l_flags6 = r_l_flags7 = r_l_native = 0;
}



monster_race::monster_race()
{
    monster_race_wipe();
}


// Wipe the monster_race class.  This could be used on any entry, but is intended only for player ghosts.
void monster_race::monster_race_wipe()
{
    r_name_full.clear();
    r_name_short.clear();
    r_text.clear();
    hdice = hside = 0;
    ac = sleep = 0;
    aaf = r_speed = 0;
    mexp = extra = 0;
    freq_ranged = mana = spell_power = mon_power = 0;
    flags1 = flags2 = flags3 = flags4 = flags5 = 0;
    flags6 = flags7 = r_native = 0;
    for (int i = 0; i < MONSTER_BLOW_MAX; i++)
    {
        blow[i].d_dice = blow[i].d_side = blow[i].effect = blow[i].method = 0;
    }
    level = rarity = color_num = 0;
    d_color.setRgb(0,0,0,0);
    d_char = '\0';
    max_num = cur_num = 0;
    tile_id.clear();
    double_height_tile = FALSE;
}

bool monster_race::is_unique()
{
    if (flags1 & (RF1_UNIQUE)) return (TRUE);
    return (FALSE);
}

bool monster_race::is_player_ghost()
{
    if (flags2 & (RF2_PLAYER_GHOST)) return (TRUE);
    return (FALSE);
}

bool monster_race::is_mimic()
{
    if (flags1 & (RF1_CHAR_MIMIC)) return (TRUE);
    return (FALSE);
}


ghost_template::ghost_template()
{
    ghost_template_wipe();
}

/*
 * Information about ghost "templates".
 */
void ghost_template::ghost_template_wipe()
{
    t_name.clear();
    t_gender = t_race = t_class = t_depth = 0;
};
