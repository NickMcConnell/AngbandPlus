/* File: spells2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include <src/spells.h>
#include <QVBoxLayout>
#include <QLabel>
#include <QRadioButton>
#include <qmath.h>
#include <QKeyEvent>
#include <QApplication>
#include <QTextEdit>


/*
 * Increase players hit points, notice effects
 */
bool hp_player(int num)
{
    /* Healing needed */
    if (p_ptr->chp < p_ptr->mhp)
    {
        /* Gain hitpoints */
        p_ptr->chp += num;

        /* Enforce maximum */
        if (p_ptr->chp >= p_ptr->mhp)
        {
            p_ptr->chp = p_ptr->mhp;
            p_ptr->chp_frac = 0;
        }

        /* Redraw */
        p_ptr->redraw |= (PR_SIDEBAR_PL);

        /* Heal 0-4 */
        if (num < 5)
        {
            message(QString("You feel a little better."));
        }

        /* Heal 5-14 */
        else if (num < 15)
        {
            message(QString("You feel better."));
        }

        /* Heal 15-34 */
        else if (num < 35)
        {
            message(QString("You feel much better."));
        }

        /* Heal 35+ */
        else
        {
            message(QString("You feel very good."));
        }

        /* Notice */
        return (TRUE);
    }

    /* Ignore */
    return (FALSE);
}


/*
 * Leave a "glyph of warding" which prevents monster movement.
 * Return TRUE on success
 */
bool warding_glyph(void)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    /* XXX XXX XXX */
    if (!cave_clean_bold(py, px))
    {
        QString name;

        name = feature_desc(dungeon_info[py][px].feature_idx, FALSE, TRUE);

        if (dungeon_info[py][px].object_idx) message(QString("The object resists the spell."));
        else if (cave_any_trap_bold(py,px))
        {
            if (x_list[dungeon_info[py][px].effect_idx].x_flags & EF1_GLYPH)
            {
                message(QString("There is already a glyph where you are standing."));
            }
            /* Trap */
            else message(QString("The trap resists the spell."));
        }
        else if (cave_hidden_object_bold(py, px))
        {
            message(QString("This location resists the spell."));
        }

        /* Unsuitable terrain */
        else message(QString("The %1 resists the spell.") .arg(name));

        /* Failure */
        return (FALSE);
    }

    /* Require suitable grid. Ignore monsters */
    if (!cave_trappable_bold(py, px))
    {
        QString name;

        name = feature_desc(dungeon_info[py][px].feature_idx, FALSE, TRUE);

        message(QString("The %1 resists the spell.") .arg(name));

        /* Failure */
        return (FALSE);
    }

    /* Create a glyph */
    set_effect_glyph(py, px);

    /* Remember this square */
    dungeon_info[py][px].cave_info |= (CAVE_MARK);

    /* Success */
    return (TRUE);
}

/*
 * Place elemental features around the given grid and range
 * Most elemental features are forest grids.
 * Sometimes we can place other elemental grids
 * Return TRUE if it succeeds
 * -DiegoGonzalez-
 */
bool create_elements(int cy, int cx, int range)
{
    int y, x, k;
    u16b feat, feat2 = FEAT_TREE;
    u16b elements[1024];
    u16b n = 0;
    /* Trees are rare */
    bool trees_enabled = one_in_(30);

    /* Process chance to place other features than forest */
    if (one_in_(2))
    {
        /* Traverse dungeon */
        for (y = 1; y <= p_ptr->cur_map_hgt; y++)
        {
            for (x = 1; x <= p_ptr->cur_map_wid; x++)
            {
                /* Must be passable */
                if (!cave_passable_bold(y, x)) continue;

                /* Must be an elemental feature */
                if (!cave_ff3_match(y, x, TERRAIN_MASK)) continue;

                /* Ignore forest */
                if (cave_ff3_match(y, x, ELEMENT_FOREST)) continue;

                /* Ignore non-lake features */
                if (!cave_ff2_match(y, x, (FF2_LAKE | FF2_RIVER))) continue;

                /* Player must be native */
                /* if (!is_player_native(y, x)) continue; */

                /* Array is full? */
                if (n >= N_ELEMENTS(elements))
                {
                    /* Replace features sometimes */
                    if (one_in_(2)) elements[rand_int(n)] = dungeon_info[y][x].feature_idx;
                }
                else
                {
                    /* Save the feature */
                    elements[n++] = dungeon_info[y][x].feature_idx;
                }
            }
        }
        /* Pick a random feature */
        if (n > 0) feat2 = elements[rand_int(n)];
    }



    /* Traverse grids */
    for (y = cy - range; y <= cy + range; y++)
    {
        for (x = cx - range; x <= cx + range; x++)
        {
            /* Ignore out of range grids */
            if (distance(y, x, cy, cx) > range) continue;

            /* Ignore invalid grids */
            if (!in_bounds(y, x)) continue;

            /* Ignore perma-walls, stairs, etc. */
            if (cave_ff1_match(y, x, FF1_PERMANENT)) continue;

            /* Ignore vaults/pits */
            if (dungeon_info[y][x].cave_info & (CAVE_ICKY)) continue;

            /* Ignore walls, doors, etc. */
            if (!cave_passable_bold(y, x)) continue;

            /* Ignore grids with monsters */
            if (dungeon_info[y][x].monster_idx > 0) continue;

            /* Ignore grids with objects */
            if (dungeon_info[y][x].object_idx != 0) continue;

            /* Reset feature */
            feat = feat2;

            /* Forest grids */
            if (feat == FEAT_TREE)
            {
                /* Roll */
                k = rand_int(100);

                /* Choose forest features. Trees are rare */
                if (trees_enabled && (k < 2)) feat = FEAT_TREE;
                else if (k < 20) feat = FEAT_BUSH;
                else if (k < 30) feat = FEAT_BRAMBLES;
                else if (k < 35) feat = FEAT_FOREST_SOIL_DYNAMIC;
                else if (k < 45) feat = FEAT_GRASS;
                else if (k < 55) feat = FEAT_GRASS_DYNAMIC;
                else feat = FEAT_FOREST_SOIL;
            }

            /* Put the feature */
            cave_set_feat(y, x, feat);
        }
    }

    /* Success */
    return (TRUE);
}


/*
 * Create a "glacier" that allows LOS but prevents movement and projections
 * Return TRUE on success
 */
bool create_glacier(void)
{
    int y, x;
    int status = FALSE;

    /* Select a grid */
    if (!target_set_interactive(TARGET_GRID, -1, -1)) return (FALSE);

    /* Paranoia */
    if (!p_ptr->target_set) return (FALSE);

    /* Get coordinates */
    y = p_ptr->target_row;
    x = p_ptr->target_col;

    /* Must be in the line of fire */
    if (!player_can_fire_bold(y, x))
    {
        message(QString("That grid is out of your line of fire!"));
    }
    /* Must be a passable grid free of monsters */
    else if (!cave_empty_bold(y, x))
    {
        message(QString("The grid resists your spell!"));
    }
    /* Valid grid */
    else
    {
        /* Get the index of the first effect of that grid */
        s16b x_idx = dungeon_info[y][x].effect_idx;

        /* Remove some effects */
        while (x_idx)
        {
            /* Get the effect */
            effect_type *x_ptr = &x_list[x_idx];

            /* Point to the next effect */
            x_idx = x_ptr->next_x_idx;

            /* Remove only the cloud effects */
            if ((x_ptr->x_type == EFFECT_SHIMMERING_CLOUD) ||
                (x_ptr->x_type == EFFECT_LINGERING_CLOUD) ||
                (x_ptr->x_type == EFFECT_PERMANENT_CLOUD))
            {
                delete_effect_idx((s16b)(x_ptr - x_list));
            }
        }

        /* Create the glacier */
        set_effect_glacier(FEAT_WALL_GLACIER, y, x, SOURCE_EFFECT, 0);

        /* Show that grid */
        dungeon_info[y][x].cave_info |= (CAVE_MARK);
        light_spot(y, x);

        /* Success */
        status = TRUE;
    }

    /* Reset the target info */
    target_set_monster(0);

    return (status);
}


/*
 * Array of stat "descriptions"
 */
static QString desc_stat_pos[A_MAX] =
{
    "strong",
    "smart",
    "wise",
    "dextrous",
    "healthy",
    "cute"
};


/*
 * Array of stat "descriptions"
 */
static QString desc_stat_neg[A_MAX] =
{
    "weak",
    "stupid",
    "naive",
    "clumsy",
    "sickly",
    "ugly"
};

/*
 * Array of stat "descriptions"
 */
static QString moria_desc_stat_sus_pos[A_MAX] =
{
    "You feel weaker for a moment, but it passes.",
    "You have trouble thinking clearly.  But your mind quickly clears.",
    "Your wisdom is sustained.",
    "You feel clumsy for a moment, but it passes.",
    "Your body resists the effects of the disease.",
    "Your skin starts to itch, but instantly feels better."
};

/*
 * Array of stat "descriptions"
 */
static QString moria_desc_stat_dec_pos[A_MAX] =
{
    "You feel weaker.",
    "You have trouble thinking clearly.",
    "Your wisdom is drained.",
    "You feel more clumsy.",
    "Your health is damaged!",
    "Your skin starts to itch."
};

/*
 * Increase a stat by one randomized level
 *
 * Most code will "restore" a stat before calling this function,
 * in particular, stat potions will always restore the stat and
 * then increase the fully restored value.
 */
bool inc_stat(int stat)
{
    int value, gain;

    /* Then augment the current/max stat */
    value = p_ptr->stat_base_cur[stat];

    /* Cannot go above 18/100 */
    if (value < 18+100)
    {
        /* Gain one (sometimes two) points */
        if (value < 18)
        {
            gain = ((rand_int(100) < 75) ? 1 : 2);
            value += gain;
        }

        /* Gain 1/6 to 1/3 of distance to 18/100 */
        else if (value < 18+98)
        {
            /* Approximate gain value */
            gain = (((18+100) - value) / 2 + 3) / 2;

            /* Paranoia */
            if (gain < 1) gain = 1;

            /* Apply the bonus */
            value += randint(gain) + gain / 2;

            /* Maximal value */
            if (value > 18+99) value = 18 + 99;
        }

        /* Gain one point at a time */
        else
        {
            value++;
        }

        /* Save the new value */
        p_ptr->stat_base_cur[stat] = value;

        /* Bring up the maximum too */
        if (value > p_ptr->stat_base_max[stat])
        {
            p_ptr->stat_base_max[stat] = value;
        }

        /* Recalculate bonuses */
        p_ptr->update |= (PU_BONUS);

        /* Redisplay the stats later */
        p_ptr->redraw |= (PR_SIDEBAR_PL);

        /* Success */
        return (TRUE);
    }

    /* Nothing to gain */
    return (FALSE);
}



/*
 * Decreases a stat by an amount indended to vary from 0 to 100 percent.
 *
 * Note that "permanent" means that the *given* amount is permanent,
 * not that the new value becomes permanent.  This may not work exactly
 * as expected, due to "weirdness" in the algorithm, but in general,
 * if your stat is already drained, the "max" value will not drop all
 * the way down to the "cur" value.
 */
bool dec_stat(int stat, int amount,bool permanent)
{
    int cur, max, loss, same, res = FALSE;


    /* Get the current value */
    cur = p_ptr->stat_base_cur[stat];
    max = p_ptr->stat_base_max[stat];

    /* Note when the values are identical */
    same = (cur == max);

    /* Damage "current" value */
    if (cur > 3)
    {
        /* Handle "low" values */
        if (cur <= 18)
        {
            if (amount > 90) cur--;
            if (amount > 50) cur--;
            if (amount > 20) cur--;
            cur--;
        }

        /* Handle "high" values */
        else
        {
            /* Hack -- Decrement by a random amount between one-quarter */
            /* and one-half of the stat bonus times the percentage, with a */
            /* minimum damage of half the percentage. -CWS */
            loss = (((cur-18) / 2 + 1) / 2 + 1);

            /* Paranoia */
            if (loss < 1) loss = 1;

            /* Randomize the loss */
            loss = ((randint(loss) + loss) * amount) / 100;

            /* Maximal loss */
            if (loss < amount/2) loss = amount/2;

            /* Lose some points */
            cur = cur - loss;

            /* Hack -- Only reduce stat to 17 sometimes */
            if (cur < 18) cur = (amount <= 20) ? 18 : 17;
        }

        /* Prevent illegal values */
        if (cur < 3) cur = 3;

        /* Something happened */
        if (cur != p_ptr->stat_base_cur[stat]) res = TRUE;
    }

    /* Damage "max" value */
    if (permanent && (max > 3))
    {
        /* Handle "low" values */
        if (max <= 18)
        {
            if (amount > 90) max--;
            if (amount > 50) max--;
            if (amount > 20) max--;
            max--;
        }

        /* Handle "high" values */
        else
        {
            /* Hack -- Decrement by a random amount between one-quarter */
            /* and one-half of the stat bonus times the percentage, with a */
            /* minimum damage of half the percentage. -CWS */
            loss = (((max-18) / 2 + 1) / 2 + 1);
            if (loss < 1) loss = 1;
            loss = ((randint(loss) + loss) * amount) / 100;
            if (loss < amount/2) loss = amount/2;

            /* Lose some points */
            max = max - loss;

            /* Hack -- Only reduce stat to 17 sometimes */
            if (max < 18) max = (amount <= 20) ? 18 : 17;
        }

        /* Hack -- keep it clean */
        if (same || (max < cur)) max = cur;

        /* Something happened */
        if (max != p_ptr->stat_base_max[stat]) res = TRUE;
    }

    /* Apply changes */
    if (res)
    {
        /* Actually set the stat to its new value. */
        p_ptr->stat_base_cur[stat] = cur;
        p_ptr->stat_base_max[stat] = max;

        /* Recalculate bonuses */
        p_ptr->update |= (PU_BONUS);

        /* Redisplay the stats later */
        p_ptr->redraw |= (PR_SIDEBAR_PL);
    }

    /* Done */
    return (res);
}


/*
 * Restore a stat.  Return TRUE only if this actually makes a difference.
 */
bool res_stat(int stat)
{
    /* Restore if needed */
    if (p_ptr->stat_base_cur[stat] != p_ptr->stat_base_max[stat])
    {
        /* Restore */
        p_ptr->stat_base_cur[stat] = p_ptr->stat_base_max[stat];

        /* Recalculate bonuses */
        p_ptr->update |= (PU_BONUS);

        /* Redisplay the stats later */
        p_ptr->redraw |= (PR_SIDEBAR_PL);

        /* Success */
        return (TRUE);
    }

    /* Nothing to restore */
    return (FALSE);
}


/*
 * Lose a "point"
 */
bool do_dec_stat(int stat)
{
    bool sust = FALSE;

    /* Get the "sustain" */
    switch (stat)
    {
        case A_STR: if (p_ptr->state.sustain_str) sust = TRUE; break;
        case A_INT: if (p_ptr->state.sustain_int) sust = TRUE; break;
        case A_WIS: if (p_ptr->state.sustain_wis) sust = TRUE; break;
        case A_DEX: if (p_ptr->state.sustain_dex) sust = TRUE; break;
        case A_CON: if (p_ptr->state.sustain_con) sust = TRUE; break;
        case A_CHR: if (p_ptr->state.sustain_chr) sust = TRUE; break;
    }

    /* Sustain */
    if (sust)
    {
        /* Message */
        if (game_mode == GAME_NPPMORIA) message(QString("%1") .arg(moria_desc_stat_sus_pos[stat]));
        else message(QString("You feel very %1 for a moment, but the feeling passes.") .arg(desc_stat_neg[stat]));

        /* Notice effect */
        return (TRUE);
    }

    /* Attempt to reduce the stat */
    if (dec_stat(stat, 10, FALSE))
    {
        /* Message */
        if (game_mode == GAME_NPPMORIA) message(QString("%1") .arg(moria_desc_stat_dec_pos[stat]));
                else message(QString("You feel very %1.") .arg(desc_stat_neg[stat]));

        /* Notice effect */
        return (TRUE);
    }

    /* Nothing obvious */
    return (FALSE);
}

/*
 * Array of stat "descriptions"
 */
static QString moria_desc_stat_res_pos[A_MAX] =
{
    "You feel your strength returning.",
    "Your head spins a moment.",
    "You feel your wisdom returning.",
    "You feel more dextrous.",
    "You feel your health returning.",
    "You feel your looks returning."
};



/*
 * Restore lost "points" in a stat
 */
bool do_res_stat(int stat)
{
    /* Attempt to increase */
    if (res_stat(stat))
    {
        /* Message */
        if (game_mode == GAME_NPPMORIA) message(QString("%1") .arg(moria_desc_stat_res_pos[stat]));
        else message(QString("You feel less %1.") .arg(desc_stat_neg[stat]));

        /* Notice */
        return (TRUE);
    }

    /* Nothing obvious */
    return (FALSE);
}

/*
 * Array of stat "descriptions"
 */
static QString moria_desc_stat_inc_pos[A_MAX] =
{
    "Wow!  What bulging muscles!",
    "Aren't you brilliant!",
    "You suddenly have a profound thought!",
    "You feel more limber!",
    "You feel tingly for a moment.",
    "Gee, ain't you cute!"
};



/*
 * Gain a "point" in a stat
 */
bool do_inc_stat(int stat)
{
    bool res;

    /* Restore stat first */
    res = res_stat(stat);

    /* Attempt to increase */
    if (inc_stat(stat))
    {
        /* Message */
        if (game_mode == GAME_NPPMORIA) message(QString("%1") .arg(moria_desc_stat_inc_pos[stat]));
        else message(QString("You feel very %1!") .arg(desc_stat_pos[stat]));

        /* Notice */
        return (TRUE);
    }

    /* Restoration worked */
    if (res)
    {
        /* Message */
        message(QString("You feel less %1.") .arg(desc_stat_neg[stat]));

        /* Notice */
        return (TRUE);
    }

    /* Nothing obvious */
    return (FALSE);
}

/*
 * Permanently gain a "point" in a stat
 */
void do_perm_stat_boost(int stat)
{
    /* Restore stat first */
    (void)res_stat(stat);

    /* Increase stat*/
    p_ptr->stat_quest_add[stat]++;

    /* Message */
    message(QString("You feel very %1!") .arg(desc_stat_pos[stat]));

    /* Recalculate bonuses */
    p_ptr->update |= (PU_BONUS);

    /* Redisplay the stats later */
    p_ptr->redraw |= (PR_SIDEBAR_PL);
}

/*
 * Identify everything being carried.
 * Done by a potion of "self knowledge".
 */
void identify_pack(void)
{
    int i;

    /* Simply identify and know every item */
    for (i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Aware and Known */
        o_ptr->mark_known(TRUE);

        apply_autoinscription(o_ptr);
    }

    /* Recalculate bonuses */
    p_ptr->update |= (PU_BONUS);

    /* Combine / Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Redraw stuff */
    p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);

}

/*
 * This routine clears the entire "temp" set.
 *
 * This routine will Perma-Lite all "temp" grids.
 *
 * This routine is used (only) by "light_room()"
 *
 * Dark grids are illuminated.
 *
 * Also, process all affected monsters.
 *
 * SMART monsters always wake up when illuminated
 * NORMAL monsters wake up 1/4 the time when illuminated
 * STUPID monsters wake up 1/10 the time when illuminated
 */
static void cave_temp_room_light(void)
{
    int i;

    /* Apply flag changes */
    for (i = 0; i < room_grids.size(); i++)
    {
        int y = room_grids[i].y;
        int x = room_grids[i].x;

        /* No longer in the array */
        dungeon_info[y][x].cave_info &= ~(CAVE_TEMP);

        /* Perma-Lite */
        dungeon_info[y][x].cave_info |= (CAVE_GLOW);
    }

    /* Fully update the visuals */
    p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Update stuff */
    update_stuff();

    /* Process the grids */
    for (i = 0; i < room_grids.size(); i++)
    {
        int y = room_grids[i].y;
        int x = room_grids[i].x;

        /* Redraw the grid */
        light_spot(y, x);

        /* Process affected monsters */
        if (dungeon_info[y][x].monster_idx > 0)
        {
            int chance = 25;

            int m_idx = dungeon_info[y][x].monster_idx;
            monster_type *m_ptr = &mon_list[m_idx];
            monster_race *r_ptr = &r_info[m_ptr->r_idx];

            /* Stupid monsters rarely wake up */
            if (r_ptr->flags2 & (RF2_STUPID)) chance = 10;

            /* Smart monsters always wake up */
            if (r_ptr->flags2 & (RF2_SMART)) chance = 100;

            /* Sometimes monsters wake up */
            if ((m_ptr->m_timed[MON_TMD_SLEEP]) && (rand_int(100) < chance))
            {
                /* Wake up! */
                wake_monster_attack(m_ptr, MON_TMD_FLG_NOTIFY);

                /*possibly update the monster health bar*/
                if (m_ptr->sidebar) p_ptr->redraw |= (PR_SIDEBAR_MON);
            }
        }
    }

    /* None left */
    room_grids.clear();
}



/*
 * This routine clears the entire "temp" set.
 *
 * This routine will "darken" all "temp" grids.
 *
 * In addition, some of these grids will be "unmarked".
 *
 * This routine is used (only) by "unlight_room()"
 */
static void cave_temp_room_unlite(void)
{
    /* Apply flag changes */
    for (int i = 0; i < room_grids.size(); i++)
    {
        int y = room_grids[i].y;
        int x = room_grids[i].x;

        /* No longer in the array */
        dungeon_info[y][x].cave_info &= ~(CAVE_TEMP);

        /* Turn off the light */
        dungeon_info[y][x].cave_info &= ~(CAVE_GLOW);

        /* Hack -- Forget "boring" grids */
        if (!(dungeon_info[y][x].cave_info & (CAVE_HALO)) &&
            !cave_ff1_match(y, x, FF1_REMEMBER) &&
            (!cave_any_trap_bold(y, x) ||
                (x_list[dungeon_info[y][x].effect_idx].x_flags &
                (EF1_HIDDEN))))
        {
            /* Forget the grid */
            dungeon_info[y][x].cave_info &= ~(CAVE_MARK | CAVE_EXPLORED);
        }
    }

    /* Fully update the visuals */
    p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Update stuff */
    update_stuff();

    /* Apply flag changes */
    for (int i = 0; i < room_grids.size(); i++)
    {
        int y = room_grids[i].y;
        int x = room_grids[i].x;

        /* Redraw the grid */
        light_spot(y, x);
    }

    /* None left */
    room_grids.clear();
}




/*
 * Aux function -- see below
 */
static void cave_temp_room_aux(int y, int x)
{
    /* Avoid infinite recursion */
    if (dungeon_info[y][x].cave_info & (CAVE_TEMP)) return;

    /* Do not "leave" the current room */
    if (!(dungeon_info[y][x].cave_info & (CAVE_ROOM))) return;

    /* Mark the grid as "seen" */
    dungeon_info[y][x].cave_info |= (CAVE_TEMP);

    /* Add it to the "seen" set */
    room_grids.append(make_coords(y, x));
}


/*
 * Speed monsters
 */
bool speed_monsters(void)
{
    return (project_los(p_ptr->py, p_ptr->px, p_ptr->lev, GF_OLD_SPEED));
}

/*
 * Slow monsters
 */
bool slow_monsters(int power)
{
    return (project_los(p_ptr->py, p_ptr->px, power, GF_OLD_SLOW));
}

/*
 * Sleep monsters
 */
bool sleep_monsters(int power)
{
    return (project_los(p_ptr->py, p_ptr->px, power, GF_OLD_SLEEP));
}


/*
 * Banish evil monsters
 */
bool banish_evil(int dist)
{
    return (project_los(p_ptr->py, p_ptr->px, dist, GF_AWAY_EVIL));
}


/*
 * Turn undead
 */
bool turn_undead(int power)
{
    return (project_los(p_ptr->py, p_ptr->px, power, GF_AWAY_UNDEAD));
}


/*
 * Dispel undead monsters
 */
bool dispel_undead(int dam)
{
    return (project_los(p_ptr->py, p_ptr->px, dam, GF_DISP_UNDEAD));
}

/*
 * Dispel evil monsters
 */
bool dispel_evil(int dam)
{
    return (project_los(p_ptr->py, p_ptr->px, dam, GF_DISP_EVIL));
}

/*
 * Dispel all monsters
 */
bool dispel_monsters(int dam)
{
    return (project_los(p_ptr->py, p_ptr->px, dam, GF_DISP_ALL));
}

/*
 * Polymorph all monsters in LOS
 */
bool mass_polymorph(void)
{
    /* damage figure of 100 isn't used */
    return (project_los(p_ptr->py, p_ptr->px, 100, GF_OLD_POLY));
}


/*
 * Hit the player's entire LOS with damage of given type -AR
 */
bool fire_player_los(int type, int dam)
{
    return (project_los(p_ptr->py, p_ptr->px, dam, type));
}

/*
 * Wake up all monsters, and speed up "los" monsters.
 */
void aggravate_monsters(int who)
{
    int y, x;

    /*if a monster, use monster coordinates*/
    if(who > SOURCE_MONSTER_START)
    {
        monster_type *m_ptr = &mon_list[who];

        x = m_ptr->fx;
        y = m_ptr->fy;
    }

    /*use the player coordinates*/
    else
    {
        x = p_ptr->px;
        y = p_ptr->py;
    }

    /* Messages */
    (void)project_los(y, x, (250 + rand_int(250)), GF_OLD_SPEED);

}


/*
 * Wake up all monsters, and speed up "los" monsters.
 */
void mass_aggravate_monsters(int who)
{
    int i;

    /* Aggravate everyone */
    for (i = 1; i < mon_max; i++)
    {
        monster_type *m_ptr = &mon_list[i];
        /* Paranoia -- Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Skip aggravating monster (or player) */
        if (i == who) continue;

        /* Wake up all monsters */
        m_ptr->m_timed[MON_TMD_SLEEP] = 0;

        /* Speed up monsters in line of sight */
        if (player_has_los_bold(m_ptr->fy, m_ptr->fx))
        {
            int flag = MON_TMD_FLG_NOTIFY;

            if (who != SOURCE_PLAYER) flag |= MON_TMD_MON_SOURCE;

            mon_inc_timed(i, MON_TMD_FAST, (50 + rand_int(50)),	flag);
        }

        /*possibly update the monster health bar*/
        if (m_ptr->sidebar) p_ptr->redraw |= (PR_SIDEBAR_MON);
    }

    /* If it just woke up, update the monster list */
    p_ptr->redraw |= PR_WIN_MONLIST;
}

/*
 * The table of "symbol info" -- each entry is a string of the form
 * "X:desc" where "X" is the trigger, and "desc" is the "info".
 */
static monster_banish_choices banish_info_nppangband[] =
{
    {"A", "Ainu/Maia"},
    {"a", "Ant"},
    {"B", "Bird"},
    {"b", "Bat"},
    {"C", "Canine"},
    {"c", "Centipede"},
    {"D", "Ancient Dragon/Wyrm"},
    {"d", "Dragon"},
    {"E", "Elemental"},
    {"e", "Floating Eye"},
    {"F", "Dragon Fly"},
    {"f", "Feline"},
    {"G", "Ghost"},
    {"g", "Golem"},
    {"H", "Hybrid"},
    {"h", "Hobbit/Elf/Dwarf"},
    {"I", "Insect"},
    {"i", "Icky Thing"},
    {"J", "Snake"},
    {"j", "Jelly"},
    {"K", "Killer Beetle"},
    {"k", "Kobold"},
    {"L", "Lich"},
    {"l", "Louse"},
    {"M", "Multi-Headed Reptile"},
    {"m", "Mold"},
    {"n", "Naga"},
    {"O", "Ogre"},
    {"o", "Orc"},
    {"P", "Giant Humanoid"},
    {"p", "Person/Human"},
    {"Q", "Quylthulg (Pulsing Flesh Mound)"},
    {"q", "Qudruped"},
    {"R", "Reptile/Amphibian"},
    {"r", "Rodent"},
    {"S", "Spider/Scorpion/Tick"},
    {"s", "Skeleton"},
    {"T", "Troll"},
    {"t", "Townsperson"},
    {"U", "Major Demon"},
    {"u", "Minor Demon"},
    {"V", "Vampire"},
    {"v", "vortex"},
    {"W", "Wight/Wraith/etc"},
    {"w", "Worm/Worm-Mass"},
    {"X", "Xorn/Xaren/etc"},
    {"Y", "Yeti"},
    {"y", "Yeek"},
    {"Z", "Zephyr Hound"},
    {"z", "Zombie/Yeek"},
    {"$", "Mimics"},
    // Must have NULL at the end to end WHILE loop below
    {NULL, NULL},
};


/*
 * The table of "symbol info" -- each entry is a string of the form
 * "X:desc" where "X" is the trigger, and "desc" is the "info".
 */
static monster_banish_choices banish_info_nppmoria[] =
{
    {"A", "Ant Lion"},
    {"a", "Ant"},
    {"B", "Balrog"},
    {"b", "Bat"},
    {"C", "Gelatinous Cube"},
    {"c", "Centipede"},
    {"D", "Ancient Dragon (Beware)"},
    {"d", "Dragon"},
    {"E", "Elemental/Spirit"},
    {"e", "Floating Eye"},
    {"F", "Fly/Dragon Fly/Insect"},
    {"f", "Frogs"},
    {"G", "Ghost"},
    {"g", "Golem"},
    {"H", "Hobgoblin"},
    {"h", "harpy"},
    {"i", "Icky Thing"},
    {"J", "Jelly"},
    {"j", "Jackal"},
    {"K", "Killer Beetle"},
    {"k", "Kobold"},
    {"L", "Lich"},
    {"l", "Louse"},
    {"M", "Mummy"},
    {"m", "Mold"},
    {"n", "Naga"},
    {"O", "Ooze"},
    {"o", "Orc"},
    {"P", "Giant"},
    {"p", "Person/Humanoid"},
    {"Q", "Quylthulg (Pulsing Flesh Mound)"},
    {"q", "Quasit"},
    {"R", "Snake"},
    {"r", "Rodent"},
    {"S", "Scorpions"},
    {"s", "Skeleton"},
    {"T", "Troll"},
    {"t", "Tick"},
    {"U", "Umber Hulk"},
    {"V", "Vampire"},
    {"W", "Wight/Wraith"},
    {"w", "Worm/Worm-Mass"},
    {"X", "Xorn"},
    {"Y", "Yeti"},
    {"y", "Yeek"},
    {"z", "Zombie"},
    {"$", "Mimics"},
    // Must have NULL at the end to end WHILE loop below
    {NULL, NULL},
};


// Record the new selected item
void BanishSelectDialog::update_banish_choice(int choice)
{
    chosen_type = choice;
}

// See if the user selected a button bia a keypress.
void BanishSelectDialog::keyPressEvent(QKeyEvent* which_key)
{
    // Get the keypress and make sure we have the case correct
    QString key_pressed = which_key->text();

    // Just a modifier key pressed
    if (!key_pressed.length()) return;

    //make sure we know the right case
    if (QApplication::queryKeyboardModifiers() & (Qt::ShiftModifier))
    {
        key_pressed[0] = key_pressed.at(0).toTitleCase();
    }

    // Search for a radio button that starts with the keypress.
    QList<QRadioButton *> buttons = this->findChildren<QRadioButton *>();
    for (int i = 0; i < buttons.size(); i++)
    {
        QString this_text = buttons.at(i)->text();

        this_text.truncate(1);

        if (this_text.contains(key_pressed, Qt::CaseSensitive))
        {
            buttons.at(i)->click();
            this->accept();
        }
    }
}

void BanishSelectDialog::add_monster_types(QGridLayout *return_layout)
{
    int count = 0;
    int i = 0;

    monster_banish_choices *banish_ptr;

    // First count the number of monster groups
    while (TRUE)
    {
        if (game_mode == GAME_NPPANGBAND) banish_ptr = &banish_info_nppangband[i];
        else banish_ptr = &banish_info_nppmoria[i];

        if (banish_ptr->mon_race.isNull()) break;
        count++;
        i++;
    }

    banish_choice_group = new QButtonGroup(this);
    banish_choice_group->setExclusive(TRUE);

    count = qSqrt(count);

    int row = 0;
    int col = 0;
    i = 0;

    // Now put the radio buttons into the grid
    while (TRUE)
    {
        if (game_mode == GAME_NPPANGBAND) banish_ptr = &banish_info_nppangband[i];
        else banish_ptr = &banish_info_nppmoria[i];

        if (banish_ptr->mon_race.isNull()) break;

        QString mon_name = (QString("%1 - %2") .arg(banish_ptr->mon_symbol) .arg(banish_ptr->mon_race));

        QRadioButton *this_radiobutton = new QRadioButton(mon_name);
        if (!i) this_radiobutton->setChecked(TRUE);
        banish_choice_group->addButton(this_radiobutton, i);
        return_layout->addWidget(this_radiobutton, row++, col, Qt::AlignLeft);

        if (row >= count)
        {
            col++;
            row = 0;
        }

        i++;
    }

    connect(banish_choice_group, SIGNAL(buttonClicked(int)), this, SLOT(update_banish_choice(int)));
}


BanishSelectDialog::BanishSelectDialog(void)
{
    int i = 0;
    QString selected_char;
    return_value = FALSE;
    bool banishing_mimics = FALSE;

    monster_banish_choices *banish_ptr;

    QVBoxLayout *vlay = new QVBoxLayout;

    QLabel *obj_label = new QLabel(QString("<b><big>Please select a monster type to banish:</big></b>"));
    obj_label->setAlignment(Qt::AlignCenter);
    vlay->addWidget(obj_label);
    vlay->addStretch();

    QGridLayout *banish_choices = new QGridLayout;
    vlay->addLayout(banish_choices);
    add_monster_types(banish_choices);


    button_boxes = new QDialogButtonBox(QDialogButtonBox::Ok
                                      | QDialogButtonBox::Cancel);
    connect(button_boxes, SIGNAL(accepted()), this, SLOT(accept()));
    connect(button_boxes, SIGNAL(rejected()), this, SLOT(reject()));

    vlay->addWidget(button_boxes);

    setLayout(vlay);

    setWindowTitle(tr("Banishment Selection Screen"));

    if (!this->exec()) return;

    i = 0;

    while (TRUE)
    {
        if (game_mode == GAME_NPPANGBAND) banish_ptr = &banish_info_nppangband[i];
        else banish_ptr = &banish_info_nppmoria[i];

        // Paranoia - shouldn't happen
        if (banish_ptr->mon_race.isNull()) return;

        if (i == chosen_type)
        {
            selected_char = banish_ptr->mon_symbol;
            break;
        }
        i++;
    }

    return_value = TRUE;

    QString compare = "$";

    if (operator==(selected_char, compare)) banishing_mimics = TRUE;

    /* Delete the monsters of that "type" */
    for (int i = 1; i < mon_max; i++)
    {
        monster_type *m_ptr = &mon_list[i];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Paranoia -- Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Hack -- Skip Unique Monsters */
        if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

        /* Quest monsters can only be "killed" by the player */
        if (m_ptr->mflag & (MFLAG_QUEST)) continue;

        /* Skip "wrong" monsters */
        if (banishing_mimics)
        {
            if (!r_ptr->is_mimic()) continue;
        }
        else if (!selected_char.contains(r_ptr->d_char)) continue;

        /* Delete the monster */
        delete_monster_idx(i);

        /* Take some damage */
        take_hit(randint(4), "the strain of casting Banishment");
    }

    /* Update monster list window and sidebar*/
    p_ptr->redraw |= (PR_SIDEBAR_MON | PR_WIN_MONLIST);
}

/*
 * Delete all non-unique monsters of a given "type" from the level
 */
bool banishment(void)
{
    BanishSelectDialog dlg;
    return (dlg.return_value);
}

/*
 * Delete all nearby (non-unique) monsters
 */
bool mass_banishment(void)
{
    int i;

    bool result = FALSE;


    /* Delete the (nearby) monsters */
    for (i = 1; i < mon_max; i++)
    {
        monster_type *m_ptr = &mon_list[i];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Paranoia -- Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Hack -- Skip unique monsters */
        if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

        /* Skip distant monsters */
        if (m_ptr->cdis > MAX_SIGHT) continue;

        /* Quest monsters can only be "killed" by the player */
        if (m_ptr->mflag & (MFLAG_QUEST)) continue;

        /* Delete the monster */
        delete_monster_idx(i);

        /* Take some damage */
        take_hit(randint(3), "the strain of casting Mass Banishment");

        /* Note effect */
        result = TRUE;
    }

    /* Update monster list window */
    p_ptr->redraw |= PR_WIN_MONLIST;

    return (result);
}



/*
 * Learn a great deal about a given monster
 */
bool probing(void)
{

    /*let the player select one monster or feature*/
    if (!(target_set_interactive(TARGET_PROBE, -1, -1)))
    {
        return(FALSE);
    }

    /*Nothing set - paranoia*/
    if (!p_ptr->target_set) return (FALSE);

    /*We selected a terrain*/
    if (!p_ptr->target_who)
    {
        int f_idx = dungeon_info[p_ptr->target_row][p_ptr->target_col].feature_idx;

        /*Learn about the feature*/
        lore_do_probe_feature(f_idx);
        describe_feature(f_idx, FALSE);
    }

    /*We selected a monster*/
    else
    {
        int m_idx = p_ptr->target_who;
        monster_type *m_ptr = &mon_list[m_idx];
        QString m_name, extra_message;

        /* Learn about the monsters */
        lore_do_probe_monster(m_idx);

        /* Get "the monster" or "something" */
        m_name = capitalize_first(monster_desc(m_ptr, 0x04));

        /* Describe the monster */
        extra_message = (QString("%1 has %2 hit point%3.") .arg(m_name) .arg(m_ptr->hp) .arg((m_ptr->hp != 1) ? "s" : ""));

        // Get the monster info and display it
        describe_monster(m_ptr->r_idx, FALSE, extra_message);
    }

    /* Result */
    return (TRUE);
}



/*
 * The spell of destruction
 *
 * This spell "deletes" monsters (instead of "killing" them).
 *
 * Later we may use one function for both "destruction" and
 * "earthquake" by using the "full" to select "destruction".
 */
void destroy_area(int y1, int x1, int r)
{
    int y, x, k, t;

    bool flag = FALSE;

    /* No effect in town */
    if ((*dun_cap->prevent_destruction)())
    {
        message(QString("The ground shakes for a moment."));
        return;
    }

    /* Big area of affect */
    for (y = (y1 - r); y <= (y1 + r); y++)
    {
        for (x = (x1 - r); x <= (x1 + r); x++)
        {
            /* Skip illegal grids */
            if (!in_bounds_fully(y, x)) continue;

            /* Extract the distance */
            k = distance(y1, x1, y, x);

            /* Stay in the circle of death */
            if (k > r) continue;

            if (dungeon_info[y][x].cave_info & (CAVE_ICKY)) continue;

            /* Lose room and vault */
            dungeon_info[y][x].cave_info &= ~(CAVE_ROOM);

            /* Lose light and knowledge */
            dungeon_info[y][x].cave_info &= ~(CAVE_GLOW | CAVE_MARK | CAVE_EXPLORED);

            /* Hack -- Notice player affect */
            if (dungeon_info[y][x].monster_idx < 0)
            {
                /* Hurt the player later */
                flag = TRUE;

                /* Do not hurt this grid */
                continue;
            }

            /* Hack -- Quest monsters are unaffected */
            else if (dungeon_info[y][x].monster_idx > 0)
            {
                monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];

                if (m_ptr->mflag & (MFLAG_QUEST | MFLAG_QUEST_SUMMON)) continue;
            }


            /* Hack -- Skip the epicenter */
            if ((y == y1) && (x == x1)) continue;

            /* Delete the monster (if any) */
            delete_monster(y, x);

            if (cave_valid_bold(y, x))
            {
                int feat = FEAT_FLOOR;

                /* Delete objects */
                delete_object(y, x);

                /* Wall (or floor) type */
                t = rand_int(200);

                /* Burn stuff */
                if (cave_ff2_match(y, x, FF2_HURT_FIRE))
                {
                    feat = feat_state(dungeon_info[y][x].feature_idx,
                        FS_HURT_FIRE);
                }

                /* Granite */
                else if (t < 20)
                {
                    /* Create granite wall */
                    feat = FEAT_WALL_GRANITE;
                }

                /* Quartz */
                else if (t < 70)
                {
                    /* Create quartz vein */
                    feat = FEAT_QUARTZ_VEIN;
                }

                /* Magma */
                else if (t < 100)
                {
                    /* Create magma vein */
                    feat = FEAT_MAGMA_VEIN;
                }

                /* Rubble */
                else if (t < 130)
                {
                    /* Create rubble */
                    feat = FEAT_RUBBLE;
                }

                /* Change the feature */
                cave_set_feat(y, x, feat);
            }
        }
    }

    /* Hack -- Affect player */
    if (flag)
    {
        /* Message */
        message(QString("There is a searing blast of light!"));

        /* Blind the player */
        if (!p_ptr->state.resist_blind && !p_ptr->state.resist_light)
        {
            /* Become blind */
            (void)inc_timed(TMD_BLIND, 10 + randint(10), TRUE);
        }
    }

    /* Hard not to notice */
    add_wakeup_chance = 10000;

    /* Fully update the visuals */
    p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_DOORS | PU_FLOW_NO_DOORS);

    /* Redraw map */
    p_ptr->redraw |= (PR_MAP | PR_WIN_MONLIST | PR_SIDEBAR_MON);

}


/*
 * Induce an "earthquake" of the given radius at the given location.
 *
 * This will turn some walls into floors and some floors into walls.
 *
 * The player will take damage and "jump" into a safe grid if possible,
 * otherwise, he will "tunnel" through the rubble instantaneously.
 *
 * Monsters will take damage, and "jump" into a safe grid if possible,
 * otherwise they will be "buried" in the rubble, disappearing from
 * the level in the same way that they do when banished.
 *
 * Note that players and monsters (except eaters of walls and passers
 * through walls) will never occupy the same grid as a wall (or door).
 */
void earthquake(int cy, int cx, int r, bool kill_vault)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    int i, t, y, x, yy, xx, dy, dx;

    int damage = 0;

    int sn = 0, sy = 0, sx = 0;

    bool hurt = FALSE;

    bool map[32][32];

    /* No effect in town */
    if ((*dun_cap->prevent_destruction)())
    {
        message(QString("The ground shakes for a moment."));
        return;
    }

    /* Paranoia -- Enforce maximum range */
    if (r > 12) r = 12;

    /* Clear the "maximal blast" area */
    for (y = 0; y < 32; y++)
    {
        for (x = 0; x < 32; x++)
        {
            map[y][x] = FALSE;
        }
    }

    /* Check around the epicenter */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip illegal grids */
            if (!in_bounds_fully(yy, xx)) continue;

            /* Skip distant grids */
            if (distance(cy, cx, yy, xx) > r) continue;

            /* Hack - no earthquakes inside vaults. */
            if ((dungeon_info[yy][xx].cave_info & (CAVE_ICKY)) && (!kill_vault)) continue;

            /* Lose room and vault */
            dungeon_info[yy][xx].cave_info &= ~(CAVE_ROOM);

            /* Lose light and knowledge */
            dungeon_info[yy][xx].cave_info &= ~(CAVE_GLOW | CAVE_MARK | CAVE_EXPLORED);

            /* Skip the epicenter */
            if (!dx && !dy) continue;

            /* Skip most grids */
            if (rand_int(100) < 85) continue;

            /* Hack - make quest monsters immune*/
            if (dungeon_info[yy][xx].monster_idx > 0)
            {
                monster_type *m_ptr = &mon_list[dungeon_info[yy][xx].monster_idx];
                if (m_ptr->mflag & (MFLAG_QUEST | MFLAG_QUEST_SUMMON))  continue;
            }

            /* Damage this grid */
            map[16+yy-cy][16+xx-cx] = TRUE;

            /* Hack -- Take note of player damage */
            if ((yy == py) && (xx == px)) hurt = TRUE;
        }
    }

    /* First, affect the player (if necessary) */
    if (hurt)
    {
        /* Check around the player */
        for (i = 0; i < 8; i++)
        {
            /* Get the location */
            y = py + ddy_ddd[i];
            x = px + ddx_ddd[i];

            /* Skip non-empty grids */
            if (!cave_empty_bold(y, x)) continue;

            /* Important -- Skip "quake" grids */
            if (map[16+y-cy][16+x-cx]) continue;

            /* Count "safe" grids, apply the randomizer */
            if ((++sn > 1) && (rand_int(sn) != 0)) continue;

            /* Save the safe location */
            sy = y; sx = x;
        }

        /* Random message */
        switch (randint(3))
        {
            case 1:
            {
                message(QString("The cave ceiling collapses!"));
                break;
            }
            case 2:
            {
                message(QString("The cave floor twists in an unnatural way!"));
                break;
            }
            default:
            {
                message(QString("The cave quakes!"));
                message(QString("You are pummeled with debris!"));
                break;
            }
        }

        /* Hurt the player a lot */
        if (!sn)
        {
            /* Message and damage */
            message(QString("You are severely crushed!"));
            damage = 300;
        }

        /* Destroy the grid, and push the player to safety */
        else
        {
            /* Calculate results */
            switch (randint(3))
            {
                case 1:
                {
                    message(QString("You nimbly dodge the blast!"));
                    damage = 0;
                    break;
                }
                case 2:
                {
                    message(QString("You are bashed by rubble!"));
                    damage = damroll(10, 4);
                    (void)set_stun(p_ptr->timed[TMD_STUN] + randint(50));
                    break;
                }
                case 3:
                {
                    message(QString("You are crushed between the floor and ceiling!"));
                    damage = damroll(10, 4);
                    (void)set_stun(p_ptr->timed[TMD_STUN] + randint(50));
                    break;
                }
            }

            /* Move player */
            monster_swap(py, px, sy, sx);
        }

        /* Take some damage */
        if (damage) take_hit(damage, "an earthquake");
    }


    /* Examine the quaked region */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip unaffected grids */
            if (!map[16+yy-cy][16+xx-cx]) continue;

            /* Process monsters */
            if (dungeon_info[yy][xx].monster_idx > 0)
            {
                int m_idx = dungeon_info[yy][xx].monster_idx;
                monster_type *m_ptr = &mon_list[m_idx];
                monster_race *r_ptr = &r_info[m_ptr->r_idx];

                /* Most monsters cannot co-exist with rock */
                if (!(r_ptr->flags2 & (RF2_KILL_WALL)) &&
                    !(r_ptr->flags2 & (RF2_PASS_WALL)))
                {
                    QString m_name;;

                    /* Assume not safe */
                    sn = 0;

                    /* Monster can move to escape the wall */
                    if (!(r_ptr->flags1 & (RF1_NEVER_MOVE)))
                    {
                        /* Look for safety */
                        for (i = 0; i < 8; i++)
                        {
                            /* Get the grid */
                            y = yy + ddy_ddd[i];
                            x = xx + ddx_ddd[i];

                            /* Skip non-empty grids */
                            if (!cave_empty_bold(y, x)) continue;

                            /* Hack -- no safety on glyph of warding */
                            if (cave_player_glyph_bold(y, x)) continue;

                            /* Important -- Skip "quake" grids */
                            if (map[16+y-cy][16+x-cx]) continue;

                            /* Count "safe" grids, apply the randomizer */
                            if ((++sn > 1) && (rand_int(sn) != 0)) continue;

                            /* Save the safe grid */
                            sy = y;
                            sx = x;
                        }
                    }

                    /* Describe the monster */
                    m_name = monster_desc(m_ptr, 0);

                    /* Scream in pain */
                    message(QString("%1 wails out in pain!") .arg(capitalize_first(m_name)));

                    /* Take damage from the quake */
                    damage = (sn ? damroll(4, 8) : (m_ptr->hp + 1));

                    /* Monster is certainly awake */
                    wake_monster_attack(m_ptr, MON_TMD_FLG_NOMESSAGE);

                    /* Apply damage directly */
                    m_ptr->hp -= damage;

                    /* Delete (not kill) "dead" monsters */
                    if (m_ptr->hp < 0)
                    {
                        /* Message */
                        message(QString("%1 is embedded in the rock!") .arg(capitalize_first(m_name)));

                        /* Delete the monster */
                        delete_monster(yy, xx);

                        /* No longer safe */
                        sn = 0;
                    }

                    /* Hack -- Escape from the rock */
                    if (sn)
                    {
                        /* Move the monster */
                        monster_swap(yy, xx, sy, sx);
                    }
                }
            }
        }
    }


    /* XXX XXX XXX */

    /* New location */
    py = p_ptr->py;
    px = p_ptr->px;

    /* Important -- no wall on player */
    map[16+py-cy][16+px-cx] = FALSE;


    /* Examine the quaked region */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip unaffected grids */
            if (!map[16+yy-cy][16+xx-cx]) continue;

            /* Paranoia -- never affect player */
            if ((yy == py) && (xx == px)) continue;

            /* Destroy location (if valid) */
            if (cave_valid_bold(yy, xx))
            {
                int feat = FEAT_FLOOR;

                bool floor = (cave_ff1_match(yy, xx, FF1_MOVE) != 0);

                /* Delete objects */
                delete_object(yy, xx);

                /* Wall (or floor) type */
                t = (floor ? rand_int(100) : 200);

                /* Granite */
                if (t < 20)
                {
                    /* Create granite wall */
                    feat = FEAT_WALL_GRANITE;
                }

                /* Quartz */
                else if (t < 70)
                {
                    /* Create quartz vein */
                    feat = FEAT_QUARTZ_VEIN;
                }

                /* Magma */
                else if (t < 100)
                {
                    /* Create magma vein */
                    feat = FEAT_MAGMA_VEIN;
                }

                /* Change the feature */
                cave_set_feat(yy, xx, feat);
            }
        }
    }

    /* Hard not to notice */
    add_wakeup_chance = MAX(add_wakeup_chance, 8000);

    /* Fully update the visuals */
    p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_DOORS | PU_FLOW_NO_DOORS);

    /* Update the health bar */
    p_ptr->redraw |= (PR_SIDEBAR_MON | PR_WIN_MONLIST | PR_MAP);
}





/*
 * Illuminate any room containing the given location.
 */
void light_room(int y1, int x1)
{
    int i, x, y;

    /* Add the initial grid */
    cave_temp_room_aux(y1, x1);

    /* While grids are in the queue, add their neighbors */
    for (i = 0; i < room_grids.size(); i++)
    {
        x = room_grids[i].x, y = room_grids[i].y;

        /* Walls get lit, but stop light */
        if (!cave_project_bold(y, x)) continue;

        /* Spread adjacent */
        cave_temp_room_aux(y + 1, x);
        cave_temp_room_aux(y - 1, x);
        cave_temp_room_aux(y, x + 1);
        cave_temp_room_aux(y, x - 1);

        /* Spread diagonal */
        cave_temp_room_aux(y + 1, x + 1);
        cave_temp_room_aux(y - 1, x - 1);
        cave_temp_room_aux(y - 1, x + 1);
        cave_temp_room_aux(y + 1, x - 1);
    }

    /* Now, lite them all up at once */
    cave_temp_room_light();

    /* Redraw map */
    p_ptr->redraw |= (PR_MAP);

}


/*
 * Darken all rooms containing the given location
 */
void unlight_room(int y1, int x1)
{
    int i, x, y;

    /* Add the initial grid */
    cave_temp_room_aux(y1, x1);

    /* Spread, breadth first */
    for (i = 0; i < room_grids.size(); i++)
    {
        x = room_grids[i].x, y = room_grids[i].y;

        /* Walls get dark, but stop darkness */
        if (!cave_project_bold(y, x)) continue;

        /* Spread adjacent */
        cave_temp_room_aux(y + 1, x);
        cave_temp_room_aux(y - 1, x);
        cave_temp_room_aux(y, x + 1);
        cave_temp_room_aux(y, x - 1);

        /* Spread diagonal */
        cave_temp_room_aux(y + 1, x + 1);
        cave_temp_room_aux(y - 1, x - 1);
        cave_temp_room_aux(y - 1, x + 1);
        cave_temp_room_aux(y + 1, x - 1);
    }

    /* Now, darken them all at once */
    cave_temp_room_unlite();

}



/*
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
bool light_area(int dam, int rad)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_KILL;

    /* Hack -- Message */
    if (!p_ptr->timed[TMD_BLIND])
    {
        message(QString("You are surrounded by a white light."));
    }

    /* Hook into the "project()" function */
    (void)project(SOURCE_PLAYER, rad, py, px, py, px, dam, GF_LIGHT_WEAK, flg, 0, 0);

    /* Lite up the room */
    light_room(py, px);

    /* Assume seen */
    return (TRUE);
}


/*
 * Hack -- call darkness around the player
 * Affect all monsters in the projection radius
 */
bool unlight_area(int dam, int rad)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_KILL;

    /* Hack -- Message */
    if (!p_ptr->timed[TMD_BLIND])
    {
        message(QString("Darkness surrounds you."));
    }

    /* Hook into the "project()" function */
    (void)project(SOURCE_PLAYER, rad, py, px, py, px, dam, GF_DARK_WEAK, flg, 0, 0);

    /* Lite up the room */
    unlight_room(py, px);

    /* Assume seen */
    return (TRUE);
}

/*
 * Get a spell type (GF_*) for the given terrain feature.
 * The spell type is stored in gf_type.
 * A description of the effect on an object is stored in action.
 * Both gf_type and action can be NULL.
 */
QString get_spell_type_from_feature(int f_idx, int *gf_type)
{
    /* Get the element flags */
    u32b element = feat_ff3_match(f_idx, TERRAIN_MASK);
    u16b i;
    QString desc;


    /*
     * Spell information for each element type.
     */
    static struct
    {
        u32b element;
        int gf_type;
        QString action;
    } spell_info[] =
    {
        {ELEMENT_BWATER, GF_BWATER, "burns"},
        {ELEMENT_BMUD, GF_BMUD, "burns"},
        {ELEMENT_LAVA, GF_LAVA, "burns"},
        {ELEMENT_FIRE, GF_FIRE, "burns"},
        {ELEMENT_ICE, GF_ICE, "freezes"},
        {ELEMENT_WATER, GF_WATER, "hurts"},
        {ELEMENT_SAND, GF_SAND, "hurts"},
        {ELEMENT_ACID, GF_ACID, "hurts"}
    };

    /* Save default spell type */
    *gf_type = GF_MANA;

    /* Save default action */
    desc = ("hurts");

    /* Find the element in the list */
    for (i = 0; i < N_ELEMENTS(spell_info); i++)
    {
        /* Found the element? */
        if (spell_info[i].element == element)
        {
            /* Save the spell type */
            *gf_type = spell_info[i].gf_type;

            /* Save the action */
            desc = (spell_info[i].action);
        }
    }

    return (desc);
}

/*
 * Return TRUE if the player is immune to the effects of the given
 * spell type
 */
bool is_player_immune(int gf_type)
{
    /* Check immunities */
    switch (gf_type)
    {
        case GF_ACID:
        {
            if (p_ptr->state.immune_acid) return (TRUE);

            break;
        }
        case GF_ICE:
        case GF_COLD:
        {
            if (p_ptr->state.immune_cold) return (TRUE);

            break;
        }
        case GF_FIRE:
        {
            if (p_ptr->state.immune_fire) return (TRUE);

            break;
        }
        case GF_ELEC:
        {
            if (p_ptr->state.immune_elec) return (TRUE);

            break;
        }
        case GF_POIS:
        {
            if (p_ptr->state.immune_pois) return (TRUE);

            break;
        }
    }

    return (FALSE);
}

/*
 * Hack -- Removes curse from an object.
 */
void uncurse_object(object_type *o_ptr)
{
    /* Uncurse it */
    o_ptr->ident &= ~(IDENT_CURSED);

    /* Remove special inscription, if any */
    if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

    /* Take note if allowed */
    if (o_ptr->discount == 0) o_ptr->discount = INSCRIP_UNCURSED;

    /* The object has been "sensed" */
    o_ptr->ident |= (IDENT_SENSE);
}


/*
 * Removes curses from items in inventory.
 *
 * \param heavy removes heavy curses if true
 *
 * \returns number of items uncursed
 */
static int remove_curse_aux(bool heavy)
{
    int i, cnt = 0;

    /* Attempt to uncurse items being worn */
    for (i = INVEN_WIELD; i < ALL_INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &inventory[i];
        QString o_name;

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Uncursed already */
        if (!o_ptr->is_cursed()) continue;

        /* Heavily Cursed Items need a special spell */
        if (!heavy && (o_ptr->obj_flags_3 & (TR3_HEAVY_CURSE))) continue;

        /* Perma-Cursed Items can NEVER be uncursed */
        if (o_ptr->obj_flags_3 & (TR3_PERMA_CURSE)) continue;

        /* Uncurse the object */
        uncurse_object(o_ptr);

        o_name = object_desc(o_ptr, ODESC_BASE);

        message(QString("The curse on your %1 is broken!") .arg(o_name));

        o_ptr->update_object_flags();

        /* Recalculate the bonuses */
        p_ptr->update |= (PU_BONUS);

        /* Count the uncursings */
        cnt++;
    }
    /* Combine and re-order the pack - and redraw stuff */
    if (cnt)
    {
        p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);
        p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);
    }

    else message(QString("Nothing happens."));

    /* Return "something uncursed" */
    return (cnt);
}


/*
 * Remove most curses
 */
bool remove_curse(bool heavy)
{
    return (remove_curse_aux(heavy));
}

/*
 * Remove all curses
 */
bool remove_all_curse(void)
{
    return (remove_curse_aux(TRUE));
}



/*
 * Restores any drained experience
 */
bool restore_level(void)
{
    /* Restore experience */
    if (p_ptr->exp < p_ptr->max_exp)
    {
        /* Message */
        message(QString("You feel your life energies returning."));

        /* Restore the experience */
        p_ptr->exp = p_ptr->max_exp;

        /* Check the experience */

        check_experience();

        /* Did something */
        return (TRUE);
    }

    /* No effect */
    return (FALSE);
}

void identify_object(object_type *o_ptr, bool star_ident)
{
    /* Identify it */
    if (star_ident) o_ptr->mark_fully_known(TRUE);
    else o_ptr->mark_known(TRUE);

    /* Apply an autoinscription, if necessary */
    apply_autoinscription(o_ptr);

    p_ptr->redraw |= (PR_WIN_OBJLIST);
}


/*
 * Execute some common code of the identify spells.
 * "item" is used to print the slot occupied by an object in equip/inven.
 * ANY negative value assigned to "item" can be used for specifying an object
 * on the floor (they don't have a slot, example: the code used to handle
 * GF_MASS_IDENTIFY in project_o).
 * It returns the value returned by squelch_itemp.
 * The object is NOT squelched here.
 */
int do_ident_item(int item, object_type *o_ptr)
{
    QString o_name;
    int squelch = SQUELCH_NO;
    byte color = TERM_WHITE;

    /* In Moria, mark the item as fully known, else identify it */
    if (game_mode == GAME_NPPMORIA) o_ptr->mark_fully_known(TRUE);
    else o_ptr->mark_known(TRUE);

    /* Apply an autoinscription, if necessary */
    apply_autoinscription(o_ptr);

    /* Squelch it? */
    if (item < INVEN_WIELD) squelch = squelch_itemp(o_ptr, 0, TRUE);

    /* Recalculate bonuses */
    p_ptr->update |= (PU_BONUS | PU_PLAYER_SCORE);

    p_ptr->redraw |= (PR_SIDEBAR_PL | PR_WIN_INVENTORY | PR_WIN_EQUIPMENT | PR_WIN_OBJLIST);

    /* Combine / Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Description */
    o_name  = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Display independent of cursedness */
    if(o_ptr->art_num)
        color = TERM_BLUE;
    else if (o_ptr->ego_num)
        color = TERM_GREEN;

    /* Describe */
    if (item >= INVEN_WIELD)
    {
        color_message(QString("%1: %2 (%3).") .arg(capitalize_first(describe_use(item))) .arg(o_name) .arg(index_to_label(item)), color);
    }
    else if (item >= 0)
    {
        color_message(QString("In your pack: %1 (%2).%3") .arg(o_name) .arg(index_to_label(item)) .arg(squelch_to_label(squelch)), color);
    }
    else
    {
        color_message(QString("On the ground: %1.%2") .arg(o_name)  .arg(squelch_to_label(squelch)), color);
    }

    /*
     * If the item was an artifact, and if the auto-note is selected,
     * write a message.
     */
    if (o_ptr->is_artifact() && (o_ptr->xtra1 >= 1))
    {
        int artifact_depth;
        QString note;
        QString shorter_desc;

        /* Get a shorter description to fit the notes file */
        shorter_desc = object_desc(o_ptr, ODESC_PREFIX | ODESC_BASE);

        /* Build note and write */
        note = (QString("Found %1") .arg(shorter_desc));

        /* Record the depth where the artifact was created */
        artifact_depth = o_ptr->xtra1;

        write_note(note, artifact_depth);

        /*
         * Mark item creation depth 0, which will indicate the artifact
         * has been previously identified.  This prevents an artifact
         * from showing up on the notes list twice ifthe artifact had
         * been previously identified.  JG
         */
        o_ptr->xtra1 = 0 ;
    }

    /* Check if the object is an artifact known from a previous game */
    if (!(o_ptr->ident & (IDENT_MENTAL)) && ARTIFACT_EASY_MENTAL(o_ptr)
        && a_l_list[o_ptr->art_num].was_fully_identified)
    {
        /* Message */
        color_message(QString("You are already familiar with this artifact."), TERM_BLUE);

        /* Fully identify the artifact for free */
        o_ptr->ident |= (IDENT_MENTAL);
    }

    return (squelch);
}





static bool item_tester_unknown(object_type *o_ptr)
{
    if (o_ptr->is_known())
        return FALSE;
    else
        return TRUE;
}


static bool item_tester_unknown_star(object_type *o_ptr)
{
    if (o_ptr->ident & IDENT_MENTAL)
        return FALSE;
    else
        return TRUE;
}

/*
 * Identify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was identified, else FALSE.
 */
bool ident_spell(void)
{
    int item;

    int squelch;

    object_type *o_ptr;

    QString q, s;

    /* Only un-id'ed items */
    item_tester_hook = item_tester_unknown;

    /* Get an item */
    q = "Identify which item? ";
    s = "You have nothing to identify.";
    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_QUIVER))) return (FALSE);

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Identify the object and get squelch setting */
    squelch = do_ident_item(item, o_ptr);

    /* Now squelch it if needed */
    do_squelch_item(squelch, item, o_ptr);

    /* Something happened */
    return (TRUE);
}



/*
 * Fully "identify" an object in the inventory
 *
 * This routine returns TRUE if an item was identified.
 */
bool identify_fully(void)
{
    int item;

    int squelch;

    object_type *o_ptr;

    QString q, s;

    /* Only un-*id*'ed items */
    item_tester_hook = item_tester_unknown_star;

    /* Get an item */
    q = "Identify which item? ";
    s = "You have nothing to identify.";
    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_QUIVER))) return (FALSE);

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Mark the item as fully known */
    o_ptr->mark_fully_known(TRUE);

     o_ptr->update_object_flags();

    /* Identify the object and get the squelch setting */
    squelch = do_ident_item(item, o_ptr);

    /* Now squelch it if needed */
    if (squelch == SQUELCH_YES)
    {
        do_squelch_item(squelch, item, o_ptr);
    }

    else
    {
        /* Describe it fully */
        object_info_screen(o_ptr);
    }

    /* Check for easy mental feature (artifacts) */
    if (ARTIFACT_EASY_MENTAL(o_ptr))
    {
        artifact_lore *a_l_ptr = &a_l_list[o_ptr->art_num];

        /* Message, keep commented out for now */

        /* Remember that we *identified* this artifact */
        a_l_ptr->was_fully_identified = TRUE;
    }

    /* Success */
    return (TRUE);
}


/*
 * Re-charge a staff or wand, and remove the identification
 * If there is a chance for the re-charge to fail, that
 * should be checked before this function is called.
 */
void recharge_staff_wand(object_type *o_ptr, int percent)
{
    int recharge_amount;

    if (o_ptr->tval == TV_WAND) recharge_amount = charge_wand(o_ptr, percent);
    else if (o_ptr->tval == TV_STAFF) recharge_amount = charge_staff(o_ptr, percent);
    /* Paranoia */
    else return;

    /* Handle stacks of wands/staves, with diminishing returns */
    if (o_ptr->number > 1)
    {
        if (o_ptr->tval == TV_WAND) recharge_amount += charge_wand(o_ptr, (percent * 4 / 10));
        else if (o_ptr->tval == TV_STAFF) recharge_amount += charge_staff(o_ptr, (percent * 4 / 10));
    }

    /* Little increase for a stack greater than two */
    if (o_ptr->number > 2) recharge_amount += (o_ptr->number - 2);

    /* Recharge the wand or staff. */
    o_ptr->pval += recharge_amount;

    /* *Identified* items keep the knowledge about the charges */
    if (!(o_ptr->ident & IDENT_MENTAL))
    {
        /* We no longer "know" the item */
        o_ptr->ident &= ~(IDENT_KNOWN);
    }

    /* Hack -- we no longer think the item is empty */
    o_ptr->ident &= ~(IDENT_EMPTY);

}

/*
 * Recharge a wand/staff/rod from the pack or on the floor.
 *
 *
 * XXX XXX XXX Beware of "sliding index errors".
 *
 * Should probably not "destroy" over-charged items, unless we
 * "replace" them by, say, a broken stick or some such.  The only
 * reason this is okay is because "scrolls of recharging" appear
 * BEFORE all staves/wands/rods in the inventory.  Note that the
 * new "auto_sort_pack" option would correctly handle replacing
 * the "broken" wand with any other item (i.e. a broken stick).
 *
 */
bool recharge(int num, bool cannot_fail, int percent)
{
    int i, item, lev, chance;

    object_type *o_ptr;

    int fail_type = 0;

    int recharge_amount = 0;

    int recharge_strength;

    QString q, s;

    /* Only accept legal items, which are wands and staffs */
    item_tester_hook = item_tester_hook_recharge;

    /* Get an item */
    q = "Recharge which item? ";
    s = "You have nothing to recharge.";
    if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

    /* Get the item (in the pack) */
    if (item >= 0) o_ptr = &inventory[item];

    /* Get the item (on the floor) */
    else o_ptr = &o_list[0 - item];

    /* Extract the object "level" */
    lev = k_info[o_ptr->k_idx].k_level;

    /* Recharge a rod, or handle failure to recharge */
    if (o_ptr->tval == TV_ROD)
    {
        /* Extract a recharge strength by comparing object level to power. */
        recharge_strength = (((35 + num) > lev) ? ((35 + num) - lev) : 0) / 5;

        /* Back-fire */
        if ((one_in_(recharge_strength)) && (!cannot_fail))
        {
            /*rods in a larger stack are more likely to be destroyed*/
            chance = 20 + (o_ptr->number * 5);

            /*destroy one rod sometimes*/
            if (randint(100) <= chance) fail_type = 2;

            /* If attempting to charge when timeout is within 5% of
             * max timeout, destroy 1 rod half of the time */
            else if (((o_ptr->pval * 95 / 100) <= o_ptr->timeout) &&
                (one_in_(2))) fail_type = 2;

            /*else completely drain rod or stack of rods*/
            else fail_type = 1;

            /*hack - single rods only get drained*/
            if (o_ptr->number == 1) fail_type = 1;

        }

        /* Drain the rod or stack of rods. */
        if (fail_type == 1)
        {

            /*stack of rods*/
            if (o_ptr->number > 1) message(QString("The stack of rods is completely drained!"));

            /*one rod*/
            else message(QString("The recharge backfires, completely draining the rod!"));

            /*drain the rod or rods to max timeout*/
            o_ptr->timeout = o_ptr->pval;

        }

        /* Destroy one of the rods. */
        else if (fail_type == 2)
        {

            message(QString("There is a bright flash of light"));

            /* Reduce the charges rods */
            reduce_charges(o_ptr, 1);

            /* Reduce and describe inventory */
            if (item >= 0)
            {

                inven_item_increase(item, -1);
                inven_item_describe(item);
                inven_item_optimize(item);

            }

            /* Reduce and describe floor item */
            else
            {
                floor_item_increase(0 - item, -1);
                floor_item_describe(0 - item);
                floor_item_optimize(0 - item);
            }

        }

        /* Recharge */
        else
        {
            /* Recharge amount */
            recharge_amount = MAX(num, percent);

            recharge_amount *= randint(3);

            /* Recharge by that amount */
            if (o_ptr->timeout > recharge_amount) o_ptr->timeout -= recharge_amount;
            else o_ptr->timeout = 0;
        }
    }

    /* Attempt to Recharge wand/staff, or handle failure to recharge . */
    else if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
    {

        /* Extract a recharge strength by comparing object level to power.
         * Divide up a stack of wands/staffs' charges to calculate charge penalty.
         */
        if (o_ptr->number > 1)
            i = (135 + num - lev -
            (10 * o_ptr->pval / o_ptr->number)) / 15;

        /* All unstacked wands and staffs. */
        else i = (135 + num - lev - (10 * o_ptr->pval)) / 15;

        /* Back-fire XXX XXX XXX */
        if (((i <= 1) || (one_in_(i))) && (!cannot_fail))
        {
            /* 25% chance of the entire stack, 5% more for each wand/staff in the stack
             * else destroy one wand/staff. */

            chance = 20 + (o_ptr->number * 5);
            if (randint(100) <= chance) fail_type = 2;
            else fail_type = 1;

            /*hack - single wands don't need failtype2*/
            if (o_ptr->number == 1) fail_type = 1;

        }

        /* Destroy an object or one in a stack of objects. */
        if (fail_type == 1)
        {
            message(QString("There is a bright flash of light"));

            /* Drain wands and staffs. */
            o_ptr->pval = 0;

            /* Reduce and describe inventory */
            if (item >= 0)
            {
                inven_item_increase(item, -1);
                inven_item_describe(item);
                inven_item_optimize(item);
            }

            /* Reduce and describe floor item */
            else
            {
                floor_item_increase(0 - item, -1);
                floor_item_describe(0 - item);
                floor_item_optimize(0 - item);
            }
        }

        /* Potentially destroy all members of a stack of wands/staffs one-by-one. */
        else if (fail_type == 2)
        {
            int counter = o_ptr->number;

            /*get the number of wands/staffs to be destroyed*/
            while ((counter > 1) && (!one_in_(counter)))
            {
                /*reduce by one*/
                counter --;

            }

            /* Drain wands and staffs. */
            o_ptr->pval = 0;

            if ((o_ptr->number - counter) > 1)
                message(QString("There are several bright flashes of light"));
            else
                message(QString("There is a bright flash of light"));

            /* Reduce and describe inventory */
            if (item >= 0)
            {
                inven_item_increase(item, -counter);
                inven_item_describe(item);
                inven_item_optimize(item);
            }

            /* Reduce and describe floor item */
            else
            {
                floor_item_increase(0 - item, -counter);
                floor_item_describe(0 - item);
                floor_item_optimize(0 - item);
            }
        }

        /* Recharge */
        else
        {
            recharge_staff_wand(o_ptr, percent);
        }
    }

    /* Combine / Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Redraw stuff */
    p_ptr->redraw |= (PR_WIN_INVENTORY);

    /* Something was done */
    return (TRUE);
}

/*
 * Create stairs at the player location
 */
void stair_creation(void)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    /* XXX XXX XXX */
    if (!cave_valid_bold(py, px))
    {
        message(QString("The object resists the spell."));
        return;
    }

    /* XXX XXX XXX */
    delete_object(py, px);

    place_random_stairs(py, px);
}

/*
 * Used by the "enchant" function (chance of failure)
 */
static const int enchant_table[ENCHANT_MAX + 1] =
{
    0, 20,  50, 150, 300,
    450, 600, 750,
    900, 990
};

/*
 * Enchant an item
 *
 * Revamped!  Now takes item pointer, number of times to try enchanting,
 * and a flag of what to try enchanting.  Artifacts resist enchantment
 * some of the time, and successful enchantment to at least +0 might
 * break a curse on the item.  -CFT
 *
 * Note that an item can technically be enchanted all the way to +15 if
 * you wait a very, very, long time.  Going from +9 to +10 only works
 * about 5% of the time, and from +10 to +11 only about 1% of the time.
 *
 * Note that this function can now be used on "piles" of items, and
 * the larger the pile, the lower the chance of success.
 */
bool enchant(object_type *o_ptr, int n, int eflag)
{
    int i, chance, prob;

    bool res = FALSE;

    bool a = o_ptr->is_artifact();

    /* Large piles resist enchantment */
    prob = o_ptr->number * 100;

    /* Missiles are easy to enchant */
    if ((o_ptr->tval == TV_BOLT) ||
        (o_ptr->tval == TV_ARROW) ||
        (o_ptr->tval == TV_SHOT))
    {
        prob = prob / 20;
    }

    /* Try "n" times */
    for (i=0; i<n; i++)
    {
        /* Hack -- Roll for pile resistance */
        if ((prob > 100) && (rand_int(prob) >= 100)) continue;

        /* Enchant to hit */
        if (eflag & (ENCH_TOHIT))
        {
            if (o_ptr->to_h < 0) chance = 0;
            else if (o_ptr->to_h > ENCHANT_MAX) chance = 1000;
            else chance = enchant_table[o_ptr->to_h];

            /* Attempt to enchant */
            if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
            {
                res = TRUE;

                /* Enchant */
                o_ptr->to_h++;

                /* Break curse */
                if (o_ptr->is_cursed() &&
                    (!(o_ptr->obj_flags_3 & (TR3_PERMA_CURSE))) &&
                    (o_ptr->to_h >= 0) && (rand_int(100) < 25))
                {
                    message(QString("The curse is broken!"));

                    /* Uncurse the object */
                    o_ptr->uncurse();
                }
            }
        }

        /* Enchant to damage */
        if (eflag & (ENCH_TODAM))
        {
            if (o_ptr->to_d < 0) chance = 0;
            else if (o_ptr->to_d > ENCHANT_MAX) chance = 1000;
            else chance = enchant_table[o_ptr->to_d];

            /* Attempt to enchant */
            if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
            {
                res = TRUE;

                /* Enchant */
                o_ptr->to_d++;

                /* Break curse */
                if (o_ptr->is_cursed() &&
                    (!(o_ptr->obj_flags_3 & (TR3_PERMA_CURSE))) &&
                    (o_ptr->to_d >= 0) && (rand_int(100) < 25))
                {
                    message(QString("The curse is broken!"));

                    /* Uncurse the object */
                    o_ptr->uncurse();
                }
            }
        }

        /* Enchant to armor class */
        if (eflag & (ENCH_TOAC))
        {
            if (o_ptr->to_a < 0) chance = 0;
            else if (o_ptr->to_a > ENCHANT_MAX) chance = 1000;
            else chance = enchant_table[o_ptr->to_a];

            /* Attempt to enchant */
            if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
            {
                res = TRUE;

                /* Enchant */
                o_ptr->to_a++;

                /* Break curse */
                if (o_ptr->is_cursed() &&
                    (!(o_ptr->obj_flags_3 & (TR3_PERMA_CURSE))) &&
                    (o_ptr->to_a >= 0) && (rand_int(100) < 25))
                {
                    message(QString("The curse is broken!"));

                    /* Uncurse the object */
                    o_ptr->uncurse();
                }
            }
        }
    }

     o_ptr->update_object_flags();

    /* Failure */
    if (!res) return (FALSE);

    /* Recalculate bonuses */
    p_ptr->update |= (PU_BONUS);

    /* Combine / Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Redraw stuff */
    p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT  | PR_WIN_OBJLIST);

    /* Success */
    return (TRUE);
}


/*
 * Enchant an item (in the inventory or on the floor)
 * Note that "num_ac" requires armour, else weapon
 * Returns TRUE if attempted, FALSE if cancelled
 */
bool enchant_spell(int num_hit, int num_dam, int num_ac)
{
    int item;
    bool okay = FALSE;

    object_type *o_ptr;

    QString o_name;

    QString q, s;


    /* Assume enchant weapon */
    item_tester_hook = item_tester_hook_weapon;

    /* Enchant armor if requested */
    if (num_ac) item_tester_hook = item_tester_hook_armour;

    /* Get an item */
    q = "Enchant which item? ";
    s = "You have nothing to enchant.";
    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_QUIVER))) return (FALSE);

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }


    /* Description */
    o_name = object_desc(o_ptr, ODESC_FULL);

    /* Describe */
    message(QString("%1 %2 glow%3 brightly!") .arg((item >= 0) ? "Your" : "The") .arg(o_name) .arg((o_ptr->number > 1) ? "" : "s"));

    /* Enchant */
    if (enchant(o_ptr, num_hit, ENCH_TOHIT)) okay = TRUE;
    if (enchant(o_ptr, num_dam, ENCH_TODAM)) okay = TRUE;
    if (enchant(o_ptr, num_ac, ENCH_TOAC)) okay = TRUE;

    /* Failure */
    if (!okay)
    {
        /* Message */
        message(QString("The enchantment failed."));
    }

    /* Something happened */
    return (TRUE);
}


DisplaySelfKnowledge::DisplaySelfKnowledge()
{
    int i = 0, k;

    u32b f1 = 0L, f2 = 0L, f3 = 0L, fn = 0L;

    object_type *o_ptr;

    QString info[135];

    /* Get item flags from equipment */
    for (k = INVEN_WIELD; k < INVEN_TOTAL; k++)
    {
        o_ptr = &inventory[k];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        if ((birth_swap_weapons) && (k == INVEN_SWAP_WEAPON)) continue;

        o_ptr->update_object_flags();

        /* Extract flags */
        f1 |= o_ptr->obj_flags_1;
        f2 |= o_ptr->obj_flags_2;
        f3 |= o_ptr->obj_flags_3;
        fn |= o_ptr->obj_flags_native;
    }

    if (cp_ptr->flags & CF_BLESS_WEAPON)
    {
        info[i++] = "You are only comfortable wielding blunt weapons or blessed weapons.";
    }

    if (cp_ptr->flags & CF_CUMBER_GLOVE)
    {
        info[i++] = "You are only comfortable wearing gloves that aid your ability to move freely or increase your dexterity.";
    }

    if (cp_ptr->flags & CF_ROGUE_COMBAT)
    {
        info[i++] = "You can sometimes steal objects and gold from monsters.";
        info[i++] = "You are extraordinally precise with throwing weapons.";
        info[i++] = "You do an extraordinary amount of damage when attacking sleeping monsters.";
        info[i++] = "You are extraordinally precise and deadly when using a sling.";
    }

    if (cp_ptr->flags & CF_SET_TRAPS)
    {
        info[i++] = "You can set traps.";
    }

    if (cp_ptr->flags & CF_EXTRA_ATTACK)
    {
        if (p_ptr->lev >= LEV_EXTRA_COMBAT) info[i++] = "Your attacking speed is naturally increased.";
        else info[i++] = "After you gain more experience, your attacking speed will be naturally increased.";
    }

    if (cp_ptr->flags & CF_EXTRA_SHOT)
    {
        if (p_ptr->lev >= LEV_EXTRA_COMBAT) info[i++] = "Your shooting speed is increased when using a sling.";
        else info[i++] = "After you gain more experience, your shooting speed will be increased when using a sling.";
    }

    if (cp_ptr->flags & CF_EXTRA_ARROW)
    {
        if (p_ptr->lev >= LEV_EXTRA_COMBAT) info[i++] = "Your shooting speed is increased when using a bow.";
        else info[i++] = "After you gain more experience, your shooting speed will be increased when using a bow.";
    }

    if (cp_ptr->flags & CF_BRAVERY_30)
    {
        if (p_ptr->lev >= LEV_BRAVERY) info[i++] = "You are naturally resistant to fear.";
        else info[i++] = "After you gain more experience, you will be naturally resistant to fear.";
    }

    if (cp_ptr->flags & CF_BRIGAND_COMBAT)
    {
        if (p_ptr->lev >= LEV_RES_POIS) info[i++] = "You are naturally resistant to poison.";
        else info[i++] = "After you gain more experience, you will be naturally resistant to poison.";
    }

    if (p_ptr->timed[TMD_BLIND])
    {
        info[i++] = "You cannot see.";
    }
    if (p_ptr->timed[TMD_CONFUSED])
    {
        info[i++] = "You are confused.";
    }
    if (p_ptr->timed[TMD_AFRAID])
    {
        info[i++] = "You are terrified.";
    }
    if (p_ptr->timed[TMD_CUT])
    {
        info[i++] = "You are bleeding.";
    }
    if (p_ptr->timed[TMD_STUN])
    {
        info[i++] = "You are stunned.";
    }
    if (p_ptr->timed[TMD_POISONED])
    {
        info[i++] = "You are poisoned.";
    }
    if (p_ptr->timed[TMD_IMAGE])
    {
        info[i++] = "You are hallucinating.";
    }

    if (p_ptr->state.aggravate)
    {
        info[i++] = "You aggravate monsters.";
    }
    if (p_ptr->state.teleport)
    {
        info[i++] = "Your position is very uncertain.";
    }

    if (p_ptr->timed[TMD_BLESSED])
    {
        info[i++] = "You feel righteous.";
    }
    if (p_ptr->timed[TMD_HERO])
    {
        info[i++] = "You feel heroic.";
    }
    if (p_ptr->timed[TMD_BERSERK])
    {
        info[i++] = "You are in a battle rage.";
    }
    if (p_ptr->timed[TMD_PROTEVIL])
    {
        info[i++] = "You are protected from evil.";
    }
    if (p_ptr->timed[TMD_SHIELD])
    {
        info[i++] = "You are protected by a mystic shield.";
    }
    if (p_ptr->timed[TMD_INVULN])
    {
        info[i++] = "You are temporarily invulnerable.";
    }
    if (p_ptr->confusing)
    {
        info[i++] = "Your hands are glowing dull red.";
    }
    if (p_ptr->searching)
    {
        info[i++] = "You are looking around very carefully.";
    }
    if (p_ptr->new_spells)
    {
        info[i++] = "You can learn some spells/prayers.";
    }
    if (p_ptr->word_recall)
    {
        info[i++] = "You will soon be recalled.";
    }
    if (p_ptr->state.see_infra)
    {
        info[i++] = "Your eyes are sensitive to infrared light.";
    }

    if (p_ptr->state.slow_digest)
    {
        info[i++] = "Your appetite is small.";
    }
    if (p_ptr->state.ffall)
    {
        info[i++] = "You land gently.";
    }
    if (p_ptr->timed[TMD_FLYING])
    {
        info[i++] = "You are flying.";
    }
    if (p_ptr->state.light)
    {
        info[i++] = "You are glowing with light.";
    }
    if (p_ptr->state.regenerate)
    {
        info[i++] = "You regenerate quickly.";
    }
    if (p_ptr->state.telepathy)
    {
        info[i++] = "You have ESP.";
    }
    if (p_ptr->state.see_inv)
    {
        info[i++] = "You can see invisible creatures.";
    }
    if (p_ptr->state.free_act)
    {
        info[i++] = "You have free action.";
    }
    if (p_ptr->state.hold_life)
    {
        info[i++] = "You have a firm hold on your life force.";
    }

    if (p_ptr->state.immune_acid)
    {
        info[i++] = "You are completely immune to acid.";
    }
    else if ((p_ptr->state.resist_acid) && (p_ptr->timed[TMD_OPP_ACID]))
    {
        info[i++] = "You resist acid exceptionally well.";
    }
    else if ((p_ptr->state.resist_acid) || (p_ptr->timed[TMD_OPP_ACID]))
    {
        info[i++] = "You are resistant to acid.";
    }

    if (p_ptr->state.immune_elec)
    {
        info[i++] = "You are completely immune to lightning.";
    }
    else if ((p_ptr->state.resist_elec) && (p_ptr->timed[TMD_OPP_ELEC]))
    {
        info[i++] = "You resist lightning exceptionally well.";
    }
    else if ((p_ptr->state.resist_elec) || (p_ptr->timed[TMD_OPP_ELEC]))
    {
        info[i++] = "You are resistant to lightning.";
    }

    if (p_ptr->state.immune_fire)
    {
        info[i++] = "You are completely immune to fire.";
    }
    else if ((p_ptr->state.resist_fire) && (p_ptr->timed[TMD_OPP_FIRE]))
    {
        info[i++] = "You resist fire exceptionally well.";
    }
    else if ((p_ptr->state.resist_fire) || (p_ptr->timed[TMD_OPP_FIRE]))
    {
        info[i++] = "You are resistant to fire.";
    }

    if (p_ptr->state.immune_cold)
    {
        info[i++] = "You are completely immune to cold.";
    }
    else if ((p_ptr->state.resist_cold) && (p_ptr->timed[TMD_OPP_COLD]))
    {
        info[i++] = "You resist cold exceptionally well.";
    }
    else if ((p_ptr->state.resist_cold) || (p_ptr->timed[TMD_OPP_COLD]))
    {
        info[i++] = "You are resistant to cold.";
    }

    if (p_ptr->state.immune_pois)
    {
        info[i++] = "You are completely immune to poison.";
    }
    else if ((p_ptr->state.resist_pois) && (p_ptr->timed[TMD_OPP_POIS]))
    {
        info[i++] = "You resist poison exceptionally well.";
    }
    else if ((p_ptr->state.resist_pois) || (p_ptr->timed[TMD_OPP_POIS]))
    {
        info[i++] = "You are resistant to poison.";
    }


    if (p_ptr->state.resist_fear)
    {
        info[i++] = "You are completely fearless.";
    }

    if (p_ptr->state.resist_light)
    {
        info[i++] = "You are resistant to bright light.";
    }
    if (p_ptr->state.resist_dark)
    {
        info[i++] = "You are resistant to darkness.";
    }
    if (p_ptr->state.resist_blind)
    {
        info[i++] = "Your eyes are resistant to blindness.";
    }
    if (p_ptr->state.resist_confu)
    {
        info[i++] = "You are resistant to confusion attacks.";
    }
    if (p_ptr->state.resist_sound)
    {
        info[i++] = "You are resistant to sonic attacks.";
    }
    if (p_ptr->state.resist_shard)
    {
        info[i++] = "You are resistant to blasts of shards.";
    }
    if (p_ptr->state.resist_nexus)
    {
        info[i++] = "You are resistant to nexus attacks.";
    }
    if (p_ptr->state.resist_nethr)
    {
        info[i++] = "You are resistant to nether forces.";
    }
    if (p_ptr->state.resist_chaos)
    {
        info[i++] = "You are resistant to chaos.";
    }
    if (((p_ptr->state.resist_confu) && (!p_ptr->state.resist_chaos)) ||
        ((!p_ptr->state.resist_confu) && (p_ptr->state.resist_chaos)))
    {
        info[i++] = "You are resistant to being confused.";
    }
    if (p_ptr->state.resist_disen)
    {
        info[i++] = "You are resistant to disenchantment.";
    }
    if (p_ptr->state.native_lava)
    {
        info[i++] = "You are native to lava.";
    }
    if (p_ptr->state.native_ice)
    {
        info[i++] = "You are native to ice.";
    }
    if (p_ptr->state.native_oil)
    {
        info[i++] = "You are native to oil.";
    }
    if (p_ptr->state.native_fire)
    {
        info[i++] = "You are native to fire.";
    }
    if (p_ptr->state.native_sand)
    {
        info[i++] = "You are native to sand.";
    }
    if (p_ptr->state.native_forest)
    {
        info[i++] = "You are native to forests.";
    }
    if (p_ptr->state.native_water)
    {
        info[i++] = "You are native to water.";
    }
    if (p_ptr->state.native_acid)
    {
        info[i++] = "You are native to acid.";
    }
    if (p_ptr->state.native_mud)
    {
        info[i++] = "You are native to mud.";
    }
    if (p_ptr->state.native_boiling_water)
    {
        info[i++] = "You are native to boiling water.";
    }
    if (p_ptr->state.native_boiling_mud)
    {
        info[i++] = "You are native to boiling mud.";
    }
    if (p_ptr->state.sustain_str)
    {
        info[i++] = "Your strength is sustained.";
    }
    if (p_ptr->state.sustain_int)
    {
        info[i++] = "Your intelligence is sustained.";
    }
    if (p_ptr->state.sustain_wis)
    {
        info[i++] = "Your wisdom is sustained.";
    }
    if (p_ptr->state.sustain_con)
    {
        info[i++] = "Your constitution is sustained.";
    }
    if (p_ptr->state.sustain_dex)
    {
        info[i++] = "Your dexterity is sustained.";
    }
    if (p_ptr->state.sustain_chr)
    {
        info[i++] = "Your charisma is sustained.";
    }

    if (f1 & (TR1_STR))
    {
        info[i++] = "Your strength is affected by your equipment.";
    }
    if (f1 & (TR1_INT))
    {
        info[i++] = "Your intelligence is affected by your equipment.";
    }
    if (f1 & (TR1_WIS))
    {
        info[i++] = "Your wisdom is affected by your equipment.";
    }
    if (f1 & (TR1_DEX))
    {
        info[i++] = "Your dexterity is affected by your equipment.";
    }
    if (f1 & (TR1_CON))
    {
        info[i++] = "Your constitution is affected by your equipment.";
    }
    if (f1 & (TR1_CHR))
    {
        info[i++] = "Your charisma is affected by your equipment.";
    }

    if (f1 & (TR1_STEALTH))
    {
        info[i++] = "Your stealth is affected by your equipment.";
    }
    if (f1 & (TR1_SEARCH))
    {
        info[i++] = "Your searching ability is affected by your equipment.";
    }
    if (f1 & (TR1_INFRA))
    {
        info[i++] = "Your infravision is affected by your equipment.";
    }
    if (f1 & (TR1_TUNNEL))
    {
        info[i++] = "Your digging ability is affected by your equipment.";
    }
    if (f1 & (TR1_SPEED))
    {
        info[i++] = "Your speed is affected by your equipment.";
    }
    if (f1 & (TR1_BLOWS))
    {
        info[i++] = "Your attack speed is affected by your equipment.";
    }
    if (f1 & (TR1_SHOTS))
    {
        info[i++] = "Your shooting speed is affected by your equipment.";
    }
    if (f1 & (TR1_MIGHT))
    {
        info[i++] = "Your shooting might is affected by your equipment.";
    }

    /* Get the current weapon */
    o_ptr = &inventory[INVEN_WIELD];

    /* Analyze the weapon */
    if (o_ptr->is_weapon())
    {
        /* Special "Attack Bonuses" */
        if (f1 & (TR1_BRAND_ACID))
        {
            info[i++] = "Your weapon melts your foes.";
        }
        if (f1 & (TR1_BRAND_ELEC))
        {
            info[i++] = "Your weapon shocks your foes.";
        }
        if (f1 & (TR1_BRAND_FIRE))
        {
            info[i++] = "Your weapon burns your foes.";
        }
        if (f1 & (TR1_BRAND_COLD))
        {
            info[i++] = "Your weapon freezes your foes.";
        }
        if (f1 & (TR1_BRAND_POIS))
        {
            info[i++] = "Your weapon poisons your foes.";
        }

        /* Special "slay" flags */
        if (f1 & (TR1_SLAY_ANIMAL))
        {
            info[i++] = "Your weapon strikes at animals with extra force.";
        }
        if (f1 & (TR1_SLAY_EVIL))
        {
            info[i++] = "Your weapon strikes at evil with extra force.";
        }
        if (f1 & (TR1_SLAY_UNDEAD))
        {
            info[i++] = "Your weapon strikes at undead with holy wrath.";
        }
        if (f1 & (TR1_SLAY_DEMON))
        {
            info[i++] = "Your weapon strikes at demons with holy wrath.";
        }
        if (f1 & (TR1_SLAY_ORC))
        {
            info[i++] = "Your weapon is especially deadly against orcs.";
        }
        if (f1 & (TR1_SLAY_TROLL))
        {
            info[i++] = "Your weapon is especially deadly against trolls.";
        }
        if (f1 & (TR1_SLAY_GIANT))
        {
            info[i++] = "Your weapon is especially deadly against giants.";
        }
        if (f1 & (TR1_SLAY_DRAGON))
        {
            info[i++] = "Your weapon is especially deadly against dragons.";
        }

        /* Special "kill" flags */
        if (f1 & (TR1_KILL_DRAGON))
        {
            info[i++] = "Your weapon is a great bane of dragons.";
        }
        if (f1 & (TR1_KILL_DEMON))
        {
            info[i++] = "Your weapon is a great bane of demons.";
        }
        if (f1 & (TR1_KILL_UNDEAD))
        {
            info[i++] = "Your weapon is a great bane of undead.";
        }


        /* Indicate Blessing */
        if (f3 & (TR3_BLESSED))
        {
            info[i++] = "Your weapon has been blessed by the gods.";
        }

        /* Hack */
        if (f3 & (TR3_IMPACT))
        {
            info[i++] = "Your weapon can induce earthquakes.";
        }
    }


    // Display the info
    QVBoxLayout *main_layout = new QVBoxLayout;
    QTextEdit *message_area = new QTextEdit;

    main_layout->addWidget(message_area);
    message_area->setReadOnly(true);
    message_area->setStyleSheet("background-color: black;");
    message_area->setTextInteractionFlags(Qt::NoTextInteraction);

    message_area->clear();

    for (int x = 0; x < i; x++)
    {
        message_area->moveCursor(QTextCursor::End);
        message_area->setTextColor(defined_colors[TERM_WHITE]);
        message_area->insertPlainText(QString("%1<br>") .arg(info[x]));
    }

    QDialogButtonBox *buttons = new QDialogButtonBox(QDialogButtonBox::Close);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));
    main_layout->addWidget(buttons);

    setLayout(main_layout);
    setWindowTitle(tr("Player Self Knowldge"));

    resize(QSize(width(), height() * 3 / 2));

    updateGeometry();

    this->exec();

}

/*
 * Hack -- acquire self knowledge
 *
 * List various information about the player and/or his current equipment.
 *
 * See also "identify_fully()".
 *
 * Use the "roff()" routines, perhaps.  XXX XXX XXX
 *
 * Use the "show_file()" method, perhaps.  XXX XXX XXX
 *
 * This function cannot display more than 20 lines.  XXX XXX XXX
 */
void self_knowledge(void)
{
    DisplaySelfKnowledge();
}

/*
 * Set word of recall as appropriate
 */
bool set_recall(void)
{
    /* Ironman */
    if (birth_ironman && !p_ptr->total_winner)
    {
        message(QString("Nothing happens."));
        return (FALSE);
    }

    /* Verify leaving normal quest level */
    if (verify_leave_quest)
    {
        char out_val[160];

        if (quest_might_fail_if_leave_level())
        {
            sprintf(out_val, "Really risk failing your quest? ");
            if (!get_check(out_val)) return (FALSE);
        }

        /* Verify leaving normal quest level */
        else if (quest_shall_fail_if_leave_level())
        {
            sprintf(out_val, "Really fail your quest? ");
            if (!get_check(out_val)) return (FALSE);
        }
    }

    /* Activate recall */
    if (!p_ptr->word_recall)
    {
        /* Reset recall depth */
        if ((p_ptr->depth > 0) && (p_ptr->depth != p_ptr->recall_depth) && (game_mode != GAME_NPPMORIA))
        {
            /*
             * ToDo: Add a new player_type field "recall_depth"
             * ToDo: Poll: Always reset recall depth?
             */
             if (get_check("Reset recall depth? "))
                p_ptr->recall_depth = p_ptr->depth;
        }

        p_ptr->word_recall = rand_int(20) + 15;
        message(QString("The air about you becomes charged..."));
    }

    /* Deactivate recall */
    else
    {
        p_ptr->word_recall = 0;
        message(QString("A tension leaves the air around you..."));
    }

    /* Redraw status line */
    p_ptr->redraw = PR_STATUSBAR;
    handle_stuff();

    return (TRUE);
}


/*
 * Curse the players armor
 */
bool curse_armor(void)
{
    object_type *o_ptr;

    QString o_name;


    /* Curse the body armor */
    o_ptr = &inventory[INVEN_BODY];

    /* Nothing to curse */
    if (!o_ptr->k_idx) return (FALSE);


    /* Describe */
    o_name = object_desc(o_ptr, ODESC_BASE);

    /* Attempt a saving throw for artifacts */
    if (o_ptr->is_artifact() && one_in_(2))
    {
        /* Cool */
        message(QString("A terrible black aura tries to surround your armor, but your %1 resists the effects!") .arg(o_name));
    }

    /* not artifact or failed save... */
    else
    {
        /* Oops */
        message(QString("A terrible black aura blasts your %1!") .arg(o_name));

        /* Blast the armor */
        o_ptr->art_num = 0;
        o_ptr->ego_num = EGO_BLASTED;
        o_ptr->to_a = 0 - randint(5) - randint(5);
        o_ptr->to_h = 0;
        o_ptr->to_d = 0;
        o_ptr->ac = 0;
        o_ptr->dd = 0;
        o_ptr->ds = 0;

        /* Curse it */
        o_ptr->ident |= (IDENT_CURSED);

        /* Break it */
        o_ptr->ident |= (IDENT_BROKEN);

        /* Recalculate bonuses and mana*/
        p_ptr->update |= (PU_BONUS | PU_MANA);

        /* Redraw stuff */
        p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);

    }

    return (TRUE);
}


/*
 * Curse the players weapon
 */
bool curse_weapon(void)
{
    object_type *o_ptr;

    QString o_name;

    /* Curse the weapon */
    o_ptr = &inventory[INVEN_WIELD];

    /* Handle swap weapons */
    if (!o_ptr->is_weapon())
    {
        o_ptr = &inventory[INVEN_SWAP_WEAPON];

        if (!o_ptr->is_weapon()) return (FALSE);
    }

    /* Nothing to curse */
    if (!o_ptr->k_idx) return (FALSE);


    /* Describe */
    o_name = object_desc(o_ptr, ODESC_BASE);

    /* Attempt a saving throw */
    if (o_ptr->is_artifact() && one_in_(2))
    {
        /* Cool */
        message(QString("A terrible black aura tries to surround your weapon, but your %1 resists the effects!") .arg(o_name));
    }

    /* not artifact or failed save... */
    else
    {
        /* Oops */
        message(QString("A terrible black aura blasts your %1!") .arg(o_name));

        /* Shatter the weapon */
        o_ptr->art_num = 0;
        o_ptr->ego_num = EGO_SHATTERED;
        o_ptr->to_h = 0 - randint(5) - randint(5);
        o_ptr->to_d = 0 - randint(5) - randint(5);
        o_ptr->to_a = 0;
        o_ptr->ac = 0;
        o_ptr->dd = 0;
        o_ptr->ds = 0;

        /* Curse it */
        o_ptr->ident |= (IDENT_CURSED);

        /* Break it */
        o_ptr->ident |= (IDENT_BROKEN);

        /* Recalculate bonuses and mana*/
        p_ptr->update |= (PU_BONUS | PU_MANA);

        /* Redraw stuff */
        p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);

    }

    /* Notice */
    return (TRUE);
}


/*
 * Brand weapons (or ammo)
 *
 * Turns the (non-magical) object into an ego-item of 'brand_type'.
 */
bool brand_object(object_type *o_ptr, byte brand_type, bool do_enchant)
{
    /* you can never modify artifacts / ego-items */
    /* you can never modify broken / cursed items */
    if ((o_ptr->k_idx) && !o_ptr->is_artifact() && !o_ptr->is_ego_item() && !o_ptr->is_broken() && !o_ptr->is_cursed())
    {
        QString act = "magical";
        QString o_name = object_desc(o_ptr, ODESC_BASE);

        /*Handle weapons differently than ammo*/
        if (wield_slot(o_ptr) == INVEN_WIELD)
        {
            /* Brand the object */
            o_ptr->ego_num = EGO_BRAND_ELEMENTS;

            o_ptr->xtra1 = OBJECT_XTRA_TYPE_BRAND;
            o_ptr->xtra2 = 1 << brand_type;
        }
        else
        {

            /* Brand the object */
            o_ptr->ego_num = brand_type;
        }

        switch (brand_type)
        {
            case BRAND_OFFSET_FLAME:
            case EGO_AMMO_FLAME:
            {
                act = "fiery";
                break;
            }
            case BRAND_OFFSET_FROST:
            case EGO_AMMO_FROST:
            {
                act = "frosty";
                break;
            }
            case BRAND_OFFSET_VENOM:
            case EGO_AMMO_VENOM:
            {
                act = "sickly";
                break;
            }
        }

        /* Describe */
        message(QString("A %1 aura surrounds the %2.") .arg(act) .arg(o_name));

        /* Combine / Reorder the pack (later) */
        p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

        /* Window stuff */
        p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT  | PR_WIN_OBJLIST);

        /* Enchant */
        if (do_enchant) enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);

        /* Hack - Identify it */
        o_ptr->mark_known(TRUE);

        return (TRUE);
    }
    else
    {
        message(QString("The Branding failed."));
    }

    return (FALSE);
}

/*
 * Brand the current weapon
 */
bool brand_weapon(bool enchant)
{
    object_type *o_ptr;
    byte brand_type;

    o_ptr = &inventory[INVEN_WIELD];

    /* Handle swap weapons */
    if (!o_ptr->is_weapon())
    {
        return (FALSE);
    }

    /* Select a brand */
    if (one_in_(3))
        brand_type = BRAND_OFFSET_FLAME;
    else
        brand_type = BRAND_OFFSET_FROST;

    /* Brand the weapon */
    return (brand_object(o_ptr, brand_type, enchant));
}

/*
 * Brand some (non-magical) ammo
 */
bool brand_ammo(bool enchant)
{
    int item;
    object_type *o_ptr;
    QString q, s;
    byte brand_type;

    /* Only accept ammo */
    item_tester_hook = item_tester_hook_ammo;

    /* Get an item */
    q = "Brand which kind of ammunition? ";
    s = "You have nothing to brand.";
    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_QUIVER | QUIVER_FIRST))) return (FALSE);

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Select the brand */
    if (one_in_(3))
        brand_type = EGO_AMMO_FLAME;
    else if (one_in_(2))
        brand_type = EGO_AMMO_FROST;
    else brand_type = EGO_AMMO_VENOM;

    /* Brand the ammo */
    return (brand_object(o_ptr, brand_type, enchant));

}


/*
 * Enchant some (non-magical) bolts
 */
bool brand_bolts(bool enchant)
{
    int item;
    object_type *o_ptr;
    QString q, s;


    /* Restrict choices to bolts */
    item_tester_tval = TV_BOLT;

    /* Get an item */
    q = "Brand which bolts? ";
    s = "You have no bolts to brand.";
    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_QUIVER | QUIVER_FIRST))) return (FALSE);

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Brand the bolts */
    return (brand_object(o_ptr, EGO_AMMO_FLAME, enchant));

}


/*
 * Some of the old functions
 */

bool light_line(int dir, int dam)
{
    u32b flg = PROJECT_BEAM | PROJECT_GRID;
    return (fire_bolt_beam_special(GF_LIGHT_WEAK, dir, dam, MAX_RANGE, flg));
}

bool strong_light_line(int dir)
{
    u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
    return (fire_bolt_beam_special(GF_LIGHT, dir, damroll(10, 8), MAX_RANGE, flg));
}

bool drain_life(int dir, int dam)
{
    u32b flg = PROJECT_STOP | PROJECT_KILL;
    return (fire_bolt_beam_special(GF_LIFE_DRAIN, dir, dam, MAX_RANGE, flg));
}

bool build_wall(int dir, int dam)
{
    u32b flg = (PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_WALL);
    return (fire_beam(GF_MAKE_WALL, dir, dam, flg));
}


bool wall_to_mud(int dir, int dam)
{
    u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_EFCT;
    return (fire_bolt_beam_special(GF_KILL_WALL, dir, dam, MAX_RANGE, flg));
}

bool destroy_door(int dir)
{
    u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
    return (fire_bolt_beam_special(GF_KILL_DOOR, dir, 0, MAX_RANGE, flg));
}

bool disarm_trap(int dir)
{
    u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
    return (fire_bolt_beam_special(GF_KILL_TRAP, dir, 0, MAX_RANGE, flg));
}

bool heal_monster(int dir, int dam)
{
    u32b flg = PROJECT_STOP;
    return (fire_bolt_beam_special(GF_OLD_HEAL, dir, dam, MAX_RANGE, flg));
}

bool speed_monster(int dir)
{
    u32b flg = PROJECT_STOP;
    return (fire_bolt_beam_special(GF_OLD_SPEED, dir, p_ptr->lev, MAX_RANGE, flg));
}

bool slow_monster(int dir)
{
    u32b flg = PROJECT_STOP;
    return (fire_bolt_beam_special(GF_OLD_SLOW, dir, p_ptr->lev, MAX_RANGE, flg));
}

bool sleep_monster(int dir)
{
    u32b flg = PROJECT_STOP;
    return (fire_bolt_beam_special(GF_OLD_SLEEP, dir, damroll (3, p_ptr->lev), MAX_RANGE, flg));
}

bool confuse_monster(int dir, int plev)
{
    u32b flg = PROJECT_STOP;
    return (fire_bolt_beam_special(GF_OLD_CONF, dir, plev, MAX_RANGE, flg));
}

bool poly_monster(int dir)
{
    u32b flg = PROJECT_STOP;
    return (fire_bolt_beam_special(GF_OLD_POLY, dir, p_ptr->lev, MAX_RANGE, flg));
}

bool clone_monster(int dir)
{
    u32b flg = PROJECT_STOP;
    return (fire_bolt_beam_special(GF_OLD_CLONE, dir, 0, MAX_RANGE, flg));
}

bool fear_monster(int dir, int plev)
{
    u32b flg = PROJECT_STOP;
    return (fire_bolt_beam_special(GF_TURN_ALL, dir, plev, MAX_RANGE, flg));
}

bool teleport_monster(int dir)
{
    return (fire_beam(GF_AWAY_ALL, dir, MAX_SIGHT * 5, 0L));
}



/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */

bool door_creation(void)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
    return (project(SOURCE_PLAYER, 1, py, px, py, px, 0, GF_MAKE_DOOR, flg, 0,0));
}

bool trap_creation(int who)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE | PROJECT_EFCT;
    return (project(who, 1, py, px, py, px, 0, GF_MAKE_TRAP, flg, 0, 0));
}

bool destroy_doors_touch(void)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;

    return (project(SOURCE_PLAYER, 1, py, px, py, px, 0, GF_KILL_DOOR, flg, 0, 0));
}

bool sleep_monsters_touch(void)
{
    int py = p_ptr->py;
    int px = p_ptr->px;
    int dam = damroll(3, p_ptr->lev);

    u32b flg = PROJECT_BOOM | PROJECT_KILL | PROJECT_HIDE;

    if (game_mode == GAME_NPPMORIA) dam = 500;

    return (project(SOURCE_PLAYER, 1, py, px, py, px, dam, GF_OLD_SLEEP, flg, 0, 20));
}



/*
 * Hack -- activate the ring of power
 */
void ring_of_power(int dir)
{
    /* Pick a random effect */
    switch (randint(10))
    {
        case 1:
        case 2:
        {
            /* Message */
            message(QString("You are surrounded by a malignant aura."));

            /* Decrease all stats (permanently) */
            (void)dec_stat(A_STR, 50, TRUE);
            (void)dec_stat(A_INT, 50, TRUE);
            (void)dec_stat(A_WIS, 50, TRUE);
            (void)dec_stat(A_DEX, 50, TRUE);
            (void)dec_stat(A_CON, 50, TRUE);
            (void)dec_stat(A_CHR, 50, TRUE);

            /* Lose some experience (permanently) */
            p_ptr->exp -= (p_ptr->exp / 4);
            p_ptr->max_exp -= (p_ptr->max_exp / 4);
            check_experience();

            break;
        }

        case 3:
        {
            /* Message */
            message(QString("You are surrounded by a powerful aura."));

            /* Dispel monsters */
            dispel_monsters(1000);

            break;
        }

        case 4:
        case 5:
        case 6:
        {
            /* Mana Ball */
            fire_ball(GF_MANA, dir, 300, 3);

            break;
        }

        case 7:
        case 8:
        case 9:
        case 10:
        {
            /* Mana Bolt */
            fire_bolt(GF_MANA, dir, 250);

            break;
        }
    }
}

/*
 * Identifies all objects in the equipment and inventory.
 * Applies quality/ego-item squelch in the inventory.
 */
void identify_and_squelch_pack(void)
{
    int item, squelch;
    object_type *o_ptr;

    /* Identify equipment */
    for (item = INVEN_WIELD; item < ALL_INVEN_TOTAL; item++)
    {
        /* Get the object */
        o_ptr = &inventory[item];

        /* Ignore empty objects */
        if (!o_ptr->k_idx) continue;

        /* Ignore known objects */
        if (o_ptr->is_known()) continue;

        /* Identify it */
        (void)do_ident_item(item, o_ptr);
    }

    /* Identify inventory */
    for (item = 0; item < INVEN_WIELD; item++)
    {
        while (TRUE)
        {
            /* Get the object */
            o_ptr = &inventory[item];

            /* Ignore empty objects */
            if (!o_ptr->k_idx) break;

            /* Ignore known objects */
            if (o_ptr->is_known()) break;

            /* Identify it and get the squelch setting */
            squelch = do_ident_item(item, o_ptr);

            /*
             * If the object was squelched, keep analyzing
             * the same slot (the inventory was displaced). -DG-
             */
            if (squelch != SQUELCH_YES) break;

            /* Now squelch the object */
            do_squelch_item(squelch, item, o_ptr);
        }
    }
}

/* Mass-identify handler */
bool mass_identify (int rad)
{
    /*record the old target*/
    s16b old_target_set = p_ptr->target_set;
    s16b old_target_who = p_ptr->target_who;
    s16b old_target_row = p_ptr->target_row;
    s16b old_target_col	= p_ptr->target_col;

    /* Direct the ball to the player */
    target_set_location(p_ptr->py, p_ptr->px);

    /* Cast the ball spell */
    fire_ball(GF_MASS_IDENTIFY, 5, 0, rad);

    /* Identify equipment and inventory, apply quality squelch */
    identify_and_squelch_pack();

    /*re-set to old target*/
    p_ptr->target_set = old_target_set;
    p_ptr->target_who = old_target_who;
    p_ptr->target_row = old_target_row;
    p_ptr->target_col = old_target_col;

    /* This spell always works */
    return (TRUE);
}


/*
 * Lite the part of the dungeon that natural beings can see. Objects are also displayed.
 * Return TRUE if we found one of such creatures.
 */
bool read_minds(void)
{
    int y, x, yy, xx;
    int rad = MAX_SIGHT;
    bool flag = FALSE;
    int count = 0;

    /* Scan the dungeon for animals */
    for (y = 1; y < (p_ptr->cur_map_hgt - 1); y++)
    {
        for (x = 1; x < (p_ptr->cur_map_wid - 1); x++)
        {
            int m_idx = dungeon_info[y][x].monster_idx;
            monster_type *m_ptr;
            monster_race *r_ptr;

            /* Empty grid */
            if (m_idx <= 0) continue;

            /* Get the monster */
            m_ptr = &mon_list[m_idx];

            /* Get the race */
            r_ptr = &r_info[m_ptr->r_idx];

            /* Avoid non-animals */
            if (!(r_ptr->flags3 & (RF3_ANIMAL))) continue;

            /* Wake up monsters (sometimes) */
            if ((m_ptr->m_timed[MON_TMD_SLEEP]) && (rand_int(100) < p_ptr->lev))
            {
                /* No more sleeping */
                wake_monster_attack(m_ptr, MON_TMD_FLG_NOMESSAGE);

                /* Remember this */
                ++count;
            }

            /* Lite the surrounding area */
            for (yy = (y - rad); yy <= (y + rad); yy++)
            {
                for (xx = (x - rad); xx <= (x + rad); xx++)
                {
                    int o_idx;

                    /* Ignore annoying grids */
                    if (!in_bounds(yy, xx)) continue;

                    /* Ignore distant grids */
                    if (distance(yy, xx, y, x) > rad) continue;

                    /* Ignore grids outside of los */
                    if (!los(y, x, yy, xx)) continue;

                    /* Found! */
                    flag = TRUE;

                    /* Lite */
                    dungeon_info[yy][xx].cave_info |= (CAVE_MARK | CAVE_GLOW);

                    /* Remember the feature */
                    f_info[dungeon_info[yy][xx].feature_idx].f_everseen = TRUE;

                    /* Get the first object on the floor */
                    o_idx = dungeon_info[yy][xx].object_idx;

                    /* Mark all the objects in the grid */
                    while (o_idx)
                    {
                        object_type *o_ptr = &o_list[o_idx];

                        o_ptr->mark_object();

                        o_idx = o_ptr->next_o_idx;
                    }
                }
            }
        }
    }

    /* Show a message if some monsters wake up */
    if (count > 0)
    {
        if (count == 1) message(QString("A monster wakes up!"));
        else message(QString("%1 monsters wake up!") .arg(count));
    }

    /* Show a message if some monsters wake up */
    if (count > 0)
    {
        if (count == 1) message(QString("A monster wakes up!"));
        else message(QString("%1 monsters wake up!") .arg(count));
    }

    /* Redraw map */
    p_ptr->redraw |= (PR_MAP);

    return (flag);
}



/*
 * Return a string containing the printable name of the given spell type.
 */
static QString find_spell_type_name(int gf_type)
{
    /* List of names for each gf_* constant */
    static struct
    {
        int gf_type;
        QString name;
    } info[] =
    {
        {GF_ACID, "acid"},
        {GF_ELEC, "lightning"},
        {GF_FIRE, "fire"},
        {GF_POIS, "poison"},
        {GF_COLD, "cold"},
        {GF_ICE, "ice"},
        {GF_LAVA, "lava"},
        {GF_SAND, "sand"},
        {GF_WATER, "water"},
        {GF_BWATER, "boiling water"},
        {GF_BMUD, "boiling mud"},
        {GF_PLASMA, "plasma"},
        {GF_LIGHT, "light"},
        {GF_DARK, "darkness"},
        {GF_TIME, "time"},
        {GF_DISENCHANT, "disenchantment"},
        {GF_NETHER, "nether"},
        {GF_HOLY_ORB, "holy energy"},
        {GF_FORCE, "force"},
        {GF_SOUND, "sound"},
        {GF_INERTIA_NPP, "inertia"},
        {GF_GRAVITY, "gravity"},
        {GF_SHARD, "shards"},
        {GF_CONFUSION, "confusion"},
        {GF_MANA, "mana"},
        {GF_NEXUS, "nexus"},
        {GF_CHAOS, "chaos"},
    };
    u16b i;

    /* Find the spell type */
    for (i = 0; i < N_ELEMENTS(info); i++)
    {
        /* Found */
        if (info[i].gf_type == gf_type) return (info[i].name);
    }

    /* Paranoia */
    return ("?");
}

/*
 * Place terrain features related to the given spell type at random locations.
 * x, y and rad define the action area.
 */
static void misc_place_elements(int y, int x, int gf_type, int rad)
{
    /* List of features for each spell type */
    static struct
    {
        int gf_type;
        u16b feat;
    } info[] =
    {
        {GF_ACID, FEAT_FLOOR_ACID},
        {GF_ICE, FEAT_FLOOR_ICE},
        {GF_COLD, FEAT_FLOOR_ICE},
        {GF_FIRE, FEAT_FIRE},
        {0, 0}
    };
    int yy, xx;
    int i;
    u16b feat = 0;

    /* Find the spell type in the list and retrieve the feature */
    for (i = 0; info[i].gf_type; i++)
    {
        /* Found */
        if (info[i].gf_type == gf_type)
        {
            feat = info[i].feat;

            break;
        }
    }

    /* Not found? Done */
    if (!feat) return;

    /* Place features around the given point */
    for (yy = y - rad; yy <= y + rad; yy++)
    {
        for (xx = x - rad; xx <= x + rad; xx++)
        {
            /* Check bounds */
            if (!in_bounds(yy, xx)) continue;

            /* Too far away */
            if (distance(yy, xx, y, x) > rad) continue;

            /* It must be free of objects and monsters */
            if (!cave_empty_bold(yy, xx)) continue;

            if (!cave_clean_bold(yy, xx)) continue;

            /* Reject elemental grids */
            if (cave_ff3_match(yy, xx, TERRAIN_MASK)) continue;

            /* Flavor */
            if (one_in_(4))
            {
                /* Put the feature */
                cave_set_feat(yy, xx, feat);
            }
        }
    }
}

/*
 * Cast a random elemental ball (for druid-like characters). Elements included are fire,
 * cold, acid, lightning and poison.
 * Return TRUE if it succeeds.
 */
bool master_elements(int dam, int dir)
{

    /* Information for each element type */
    static struct
    {
        int gf_type;		/* Element type */
        /*
         * LF1_* flags. If some of these are present in the grid occupied by the player
         * the probability and damage of the element are increased
         */
        u32b level_flags;
        int prob1;		/* Default probability */
        int prob2;		/* Working probability */
    } spell_info[] =
    {
        {GF_ACID, LF1_ACID, 100, 0},
        {GF_ELEC, LF1_WATER | LF1_BWATER,  100, 0},
        {GF_FIRE, LF1_FIRE | LF1_BMUD | LF1_LAVA,  100, 0},
        {GF_POIS, LF1_ACID,  100, 0},
        {GF_COLD, LF1_ICE,  50, 0},
        {GF_ICE, LF1_ICE,  50, 0},
    }, *info;
    u16b i, total = 0;
    u32b flags;
    QString name;

    /* Accumulate the chance values */
    for (i = 0; i < N_ELEMENTS(spell_info); i++)
    {
        /* Get the spell */
        info = &spell_info[i];

        /* Get the LF1_* flag of the grid ocuppied by the player */
        flags = get_level_flag(dungeon_info[p_ptr->py][p_ptr->px].feature_idx);

        /* Boost chance? */
        if (flags & info->level_flags)
        {
            info->prob2 = 3 * info->prob1 / 2;
        }
        /* Or just copy the default chance value */
        else
        {
            info->prob2 = info->prob1;
        }

        /* Accumulate */
        total += info->prob2;
    }

    /* Paranoia */
    if (total == 0) return (FALSE);

    /* Get a random element */
    total = rand_int(total);

    /* Find the element */
    for (i = 0; i < N_ELEMENTS(spell_info); i++)
    {
        /* Get the element */
        info = &spell_info[i];

        /* Discard forbidden features (unused) */
        if (info->prob2 == 0) continue;

        /* Found. Done. */
        if (total < info->prob2) break;

        /* Prepare for the next element */
        total -= info->prob2;
    }

    /* Get the name of the element */
    name = find_spell_type_name(info->gf_type);

    /* Message */
    message(QString("You cast a ball of %1!") .arg(name));

    /*msg_format("dam: %1.", dam);*/

    /* Cast the spell. Do not create effects (smoke, sparks, etc.) */
    fire_ball_special(info->gf_type, dir, dam, 5, PROJECT_NO_EFCT, 30);

    /* Modify the dungeon layout sometimes */
    if (one_in_(10)) misc_place_elements(p_ptr->py, p_ptr->px, info->gf_type, 2);

    return (TRUE);
}

/*
 * Cast attack spells like nearby animals and vortices (bolts, beams, balls and breaths)
 * Return TRUE if it succeeds.
 */
bool steal_powers(int dir)
{
    /* The type of attacks */
    #define BREATH 1
    #define BALL 2
    #define BOLT 3
    #define BEAM 4

    /* List of possible ranged attacks */
    struct attack_info_type
    {
        byte set;	/* Which set of flags is used (4 for flags4, etc.) */
        u32b flag;	/* Monster flag to check */
        u16b spell_type; /* The spell type. One of the GF_* constants */
        byte attack_type; /* Shape of the attack */
    };

    /* Ranged attacks */
    static struct attack_info_type attack_info[] =
    {
        {4, RF4_BRTH_ACID, GF_ACID, BREATH},
        {4, RF4_BRTH_ELEC, GF_ELEC, BREATH},
        {4, RF4_BRTH_FIRE, GF_FIRE, BREATH},
        {4, RF4_BRTH_COLD, GF_COLD, BREATH},
        {4, RF4_BRTH_POIS, GF_POIS, BREATH},
        {4, RF4_BRTH_PLAS, GF_PLASMA, BREATH},
        {4, RF4_BRTH_LIGHT, GF_LIGHT, BREATH},
        {4, RF4_BRTH_DARK, GF_DARK, BREATH},
        {4, RF4_BRTH_CONFU, GF_CONFUSION, BREATH},
        {4, RF4_BRTH_SOUND, GF_SOUND, BREATH},
        {4, RF4_BRTH_SHARD, GF_SHARD, BREATH},
        {4, RF4_BRTH_INER, GF_INERTIA_NPP, BREATH},
        {4, RF4_BRTH_GRAV, GF_GRAVITY, BREATH},
        {4, RF4_BRTH_FORCE, GF_FORCE, BREATH},
        {4, RF4_BRTH_NEXUS, GF_NEXUS, BREATH},
        {4, RF4_BRTH_NETHR, GF_NETHER, BREATH},
        {4, RF4_BRTH_CHAOS, GF_CHAOS, BREATH},
        {4, RF4_BRTH_DISEN, GF_DISENCHANT, BREATH},
        {4, RF4_BRTH_TIME, GF_TIME, BREATH},
        {4, RF4_BRTH_MANA, GF_MANA, BREATH},
        {5, RF5_BALL_ACID, GF_ACID, BALL},
        {5, RF5_BALL_ELEC, GF_ELEC, BALL},
        {5, RF5_BALL_FIRE, GF_FIRE, BALL},
        {5, RF5_BALL_COLD, GF_COLD, BALL},
        {5, RF5_BALL_POIS, GF_POIS, BALL},
        {5, RF5_BALL_LIGHT, GF_LIGHT, BALL},
        {5, RF5_BALL_DARK, GF_DARK, BALL},
        {5, RF5_BALL_CONFU, GF_CONFUSION, BALL},
        {5, RF5_BALL_SOUND, GF_SOUND, BALL},
        {5, RF5_BALL_SHARD, GF_SHARD, BALL},
        {5, RF5_BALL_METEOR, GF_METEOR, BALL},
        {5, RF5_BALL_STORM, GF_WATER, BALL},
        {5, RF5_BALL_NETHR, GF_NETHER, BALL},
        {5, RF5_BALL_CHAOS, GF_CHAOS, BALL},
        {5, RF5_BALL_MANA, GF_MANA, BALL},
        {5, RF5_BALL_WATER, GF_WATER, BALL},
        {5, RF5_BOLT_ACID, GF_ACID, BOLT},
        {5, RF5_BOLT_ELEC, GF_ELEC, BOLT},
        {5, RF5_BOLT_FIRE, GF_FIRE, BOLT},
        {5, RF5_BOLT_COLD, GF_COLD, BOLT},
        {5, RF5_BOLT_POIS, GF_POIS, BOLT},
        {5, RF5_BOLT_PLAS, GF_PLASMA, BOLT},
        {5, RF5_BOLT_ICE, GF_ICE, BOLT},
        {5, RF5_BOLT_WATER, GF_WATER, BOLT},
        {5, RF5_BOLT_NETHR, GF_NETHER, BOLT},
        {5, RF5_BOLT_MANA, GF_MANA, BOLT},
        {5, RF5_BOLT_GRAV, GF_GRAVITY, BOLT},
        {5, RF5_BEAM_ELEC, GF_ELEC, BEAM},
        {5, RF5_BEAM_ICE, GF_ICE, BEAM},
        {5, RF5_BEAM_NETHR, GF_NETHER, BEAM},
        {5, RF5_BEAM_LAVA, GF_LAVA, BEAM},
        {5, RF5_HOLY_ORB, GF_HOLY_ORB, BALL},
    };

    /* This array will contain the ranged attacks of nearby monsters */
    int attack[N_ELEMENTS(attack_info)];
    struct attack_info_type *a_ptr;
    int n = 0;
    monster_type *m_ptr;
    monster_race *r_ptr;
    u16b i;

    int gf_type;

    int y, x;
    int range = MAX_RANGE;

    int monsters[100];
    u16b m = 0;

    int dam;

    QString gf_name;
    QString mon_name;

    /* Clear the masks */
    u32b set4 = 0, set5 = 0;

    /* Collect the flags in mask variables */
    for (i = 0; i < N_ELEMENTS(attack_info); i++)
    {
        u32b *set;
        /* Get the attack info */
        a_ptr = &attack_info[i];

        /* Get the proper set */
        if (a_ptr->set == 5) set = &set5;
        else set = &set4;

        /* Set the flag */
        *set |= a_ptr->flag;
    }

    /* Find nearby monsters */
    for (y = p_ptr->py - range; y <= p_ptr->py + range; y++)
    {
        for (x = p_ptr->px - range; x <= p_ptr->px + range; x++)
        {
            /* Check bounds */
            if (!in_bounds(y, x)) continue;

            /* Not too far away */
            if (distance(y, x, p_ptr->py, p_ptr->px) > range) continue;

            /* Ignore grids not occupied by monsters  */
            if (dungeon_info[y][x].monster_idx <= SOURCE_MONSTER_START) continue;

            /* Get the monster */
            m_ptr = &mon_list[dungeon_info[y][x].monster_idx];

            /* Ignore invisible ones */
            if (!m_ptr->ml) continue;

            /* It must be in LOF */
            if (!player_can_fire_bold(y, x)) continue;

            /* Get race info */
            r_ptr = &r_info[m_ptr->r_idx];

            /* Only animals and vortices are allowed */
            if (!(r_ptr->flags3 & (RF3_ANIMAL)) && (r_ptr->d_char != 'v')) continue;

            /* Ignore monsters that cannot cast these spells */
            if (!(r_ptr->flags4 & set4) && !(r_ptr->flags5 & set5)) continue;

            /* No more place for monsters */
            if (m >= N_ELEMENTS(monsters)) continue;

            /* Add the monster */
            monsters[m++] = dungeon_info[y][x].monster_idx;
        }
    }

    /* No nearby monsters */
    if (m == 0)
    {
        message(QString("There are not animals nor vortices around you!"));
        return (FALSE);
    }

    /* Select a random monster */
    m_ptr = &mon_list[monsters[rand_int(m)]];

    /* Get its race */
    r_ptr = &r_info[m_ptr->r_idx];

    /* Get its attacks */
    for (i = 0; i < N_ELEMENTS(attack_info); i++)
    {
        u32b *flags;
        a_ptr = &attack_info[i];

        /* Check if the monster has the current attack */
        if (a_ptr->set == 5) flags = &r_ptr->flags5;
        else flags = &r_ptr->flags4;

        /* Append it to the list */
        if (*flags & a_ptr->flag) attack[n++] = i;
    }

    /* Paranoia. No attacks */
    if (n == 0)
    {
        message(QString("No powers!"));
        return (FALSE);
    }

    /* Select a random attack */
    a_ptr = &attack_info[attack[rand_int(n)]];

    /* Get the spell type */
    gf_type = a_ptr->spell_type;

    /* Get the spell name */
    gf_name = find_spell_type_name(gf_type);

    /* Get the monster name */
    mon_name = monster_desc(m_ptr, 0x08);

    /* Calculate damage */
    dam = (p_ptr->chp + m_ptr->hp) / 3;
    /* Reduce damage if monster is asleep to minimize abuses */
    if (m_ptr->m_timed[MON_TMD_SLEEP]) dam /= 3;
    /* Check bounds */
    if (dam < 1) dam = 1;
    if (dam > 1000) dam = 1000;

    /* Process beam spells  */
    if (a_ptr->attack_type == BEAM)
    {
        message(QString("You cast a beam of %1 like %2 for %3 hp damage!") .arg(gf_name) .arg(mon_name) .arg(dam));
        fire_bolt_beam_special(gf_type, dir, dam, MAX_RANGE, PROJECT_BEAM | PROJECT_NO_EFCT);
    }
    /* Process bolt spells */
    else if (a_ptr->attack_type == BOLT)
    {
        message(QString("You cast a bolt of %1 like %2 for %3 hp damage!") .arg(gf_name) .arg(mon_name) .arg(dam));
        fire_bolt_beam_special(gf_type, dir, dam, MAX_RANGE, PROJECT_NO_EFCT);
    }
    /* Process ball spells */
    else if (a_ptr->attack_type == BALL)
    {
        message(QString("You cast a ball of %1 like %2 for %3 hp damage!") .arg(gf_name) .arg(mon_name) .arg(dam));
        fire_ball_special(gf_type, dir, dam, 3, PROJECT_NO_EFCT, 0);
    }
    /* Process breath spells */
    else if (a_ptr->attack_type == BREATH)
    {
        message(QString("You breath %1 like %2 for %3 hp damage!") .arg(gf_name) .arg(mon_name) .arg(dam));
        fire_arc_special(gf_type, dir, dam, 0, 60, PROJECT_NO_EFCT);
    }
    /* Paranoia */
    else
    {
        message(QString("Unknown attack type!"));
        return (FALSE);
    }

    /* Success */
    return (TRUE);
}



/*
 * Animate nearby tress.
 * Return TRUE if it succeeds.
 */
bool call_huorns(void)
{
    /* List of locations of trees */
    u16b trees[200];
    u16b num_trees = 0;
    u16b i, n;

    /* List of attacked monsters */
    s16b hits[200];
    u16b nh = 0;

    bool flag = FALSE;

    int y, x, y2, x2;
    int range = MAX_SIGHT;

    QString mon_name;

    y = p_ptr->py;
    x = p_ptr->px;

    /* Find nearby trees */
    for (y2 = y - range; y2 <= y + range; y2++)
    {
        for (x2 = x - range; x2 <= x + range; x2++)
        {
            /* No more space to hold the trees */
            if (num_trees >= N_ELEMENTS(trees)) continue;

            /* Ignore annyoing locations */
            if (!in_bounds(y2, x2)) continue;

            /* Ignore other features */
            if (dungeon_info[y2][x2].feature_idx != FEAT_TREE) continue;

            /*if (!player_can_fire_bold(y2, x2)) continue;*/

            /* Add the tree's location */
            trees[num_trees++] = GRID(y2, x2);
        }
    }

    /* Process every tree */
    for (n = 0; n < num_trees; n++)
    {

        s16b m_idx;
        int dis, best_dis = 10000;
        u16b best_grid = 0;
        bool do_destroy = FALSE;

        /* Update cave flags */
        handle_stuff();

        /* Get its location */
        y = GRID_Y(trees[n]);
        x = GRID_X(trees[n]);

        /* Scan adjacent grids */
        for (y2 = y - 1; y2 <= y + 1; y2++)
        {
            for (x2 = x - 1; x2 <= x + 1; x2++)
            {
                int dam;
                bool fear = FALSE;

                /* No more places to store monsters */
                if (nh >= N_ELEMENTS(hits)) continue;

                /* Check bounds */
                if (!in_bounds(y2, x2)) continue;

                /* Get the monster in that location */
                m_idx = dungeon_info[y2][x2].monster_idx;

                /* Ignore non-monsters */
                if (m_idx <= 0) continue;

                /* It must be visible */
                if (!mon_list[m_idx].ml) continue;

                /* It must be in LOF */
                if (!player_can_fire_bold(y2, x2)) continue;

                /* Don't attack hidden monsters */
                if (mon_list[m_idx].mflag & (MFLAG_HIDE)) continue;

                /* Ignore attacked monsters */
                for (i = 0; i < nh; i++)
                {
                    if (hits[i] == m_idx) break;
                }

                /* Monster was found? Done */
                if (i < nh) continue;

                /* Calculate damage */
                dam = 50 + rand_int(p_ptr->lev * 3);

                /* The monster is about to be killed? */
                if (dam > mon_list[m_idx].hp)
                {
                    int y3, x3;
                    bool found = FALSE;

                    /* Find an adjacent spot for drops */
                    for (y3 = y2 - 1; y3 <= y2 + 1; y3++)
                    {
                        for (x3 = x2 - 1; x3 <= x2 + 1; x3++)
                        {
                            /* Check bounds */
                            if (!in_bounds(y3, x3)) continue;

                            /* It must allow drops */
                            if (!cave_ff1_match(y3, x3, FF1_DROP)) continue;


                            /* Found empty place! */
                            found = TRUE;

                            /* Break nested loops */
                            goto end_outer_for;
                            }
                    }
                    end_outer_for:

                    /* No place for drops */
                    if (!found) continue;
                }

                /* Remember that the monster was hit */
                hits[nh++] = m_idx;

                /* Get the monster name */
                mon_name = monster_desc(&mon_list[m_idx], 0x08);

                message(QString("The huorn attacks %1") .arg(mon_name));

                /* Take hit */
                mon_take_hit(m_idx, dam, &fear, NULL, SOURCE_PLAYER, TRUE);

                /* Flavor. Enable tree destruction */
                do_destroy = TRUE;

                /* Remember this */
                flag = TRUE;
            }
        }

        /* Hack -- Avoid cloned messages */
        flush_monster_messages();

        /* Sometimes destroy the tree */
        if (do_destroy && one_in_(10))
        {
            /* Set new feature */
            cave_set_feat(y, x, (rand_int(100) < 30) ? FEAT_TREE_BURNING: FEAT_FOREST_SOIL_DYNAMIC);
            /* Message */
            message(QString("A huorn was destroyed!"));
            /* Done */
            continue;
        }

        /* Move the tree. Find target */
        for (y2 = y - range; y2 <= y + range; y2++)
        {
            for (x2 = x - range; x2 <= x + range; x2++)
            {
                int y3 = y;
                int x3 = x;
                s16b o_idx;

                /* Check bounds */
                if (!in_bounds(y2, x2)) continue;

                /* Get the monster */
                m_idx = dungeon_info[y2][x2].monster_idx;

                /* Ignore non-monsters */
                if (m_idx <= 0) continue;

                /* It must be visible */
                if (!mon_list[m_idx].ml) continue;

                /* It must be in LOF */
                if (!player_can_fire_bold(y2, x2)) continue;

                /* Don't attack hidden monsters */
                if (mon_list[m_idx].mflag & (MFLAG_HIDE)) continue;

                /* Not too far away (from the player) */
                dis = distance(p_ptr->py, p_ptr->px, y2, x2);

                if (dis > best_dis) continue;

                /* We already have a target with this distance */
                if ((dis == best_dis) && best_grid) continue;

                /* Found a closest target */
                best_dis = dis;
                /* Forget previous target */
                best_grid = 0;

                /* Advance vertically */
                if (y2 > y) ++y3;
                else if (y2 < y) --y3;

                /* Advance horizontally */
                if (x2 > x) ++x3;
                else if (x2 < x) --x3;

                /* Don't move in the same grid */
                if ((x3 == x) && (y3 == y)) continue;

                /* Don't move inside vaults/pits */
                if (dungeon_info[y3][x3].cave_info & (CAVE_ICKY)) continue;

                /* Don't move into an occupied grid */
                if (dungeon_info[y3][x3].monster_idx) continue;

                /* Ignore other trees */
                if (dungeon_info[y3][x3].feature_idx == FEAT_TREE) continue;

                /* Ignore permanent features */
                if (cave_ff1_match(y3, x3, FF1_PERMANENT)) continue;

                /* Ignore dangerous grids for trees  */
                if (cave_ff3_match(y3, x3, ELEMENT_FIRE | ELEMENT_LAVA)) continue;

                /* Check presence of objects */
                for (o_idx = dungeon_info[y3][x3].object_idx; o_idx; o_idx = o_list[o_idx].next_o_idx)
                {
                    /* Get the object */
                    object_type *o_ptr = &o_list[o_idx];

                    /* This object must be kept intact */
                    if (o_ptr->is_artifact() ||
                        (k_info[o_ptr->k_idx].squelch != SQUELCH_ALWAYS) ||
                        !o_ptr->is_aware())
                    {

                        break;
                    }
                }

                /* Don't move over this object */
                if (o_idx) continue;

                /* Remember the best location */
                best_grid = GRID(y3, x3);
            }
        }

        /* Can we move? */
        if (best_grid)
        {
            u16b feat = FEAT_FOREST_SOIL_DYNAMIC;
            int k = rand_int(100);

            /* Add flavor */
            if (k < 10) feat = FEAT_FOREST_SOIL;
            else if (k < 20) feat = FEAT_BRAMBLES;
            else if (k < 25) feat = FEAT_THORNS;
            else if (k < 30) feat = FEAT_WALL_VINES;
            else if (k < 40) feat = FEAT_BUSH;
            else if (k < 50) feat = FEAT_THICKET;
            else if (k < 60) feat = FEAT_GRASS;

            /* Restore the current grid to a passable feature */
            cave_set_feat(y, x, feat);

            /* Get the new location */
            y2 = GRID_Y(best_grid);
            x2 = GRID_X(best_grid);
            /* Destroy its objects */
            delete_object(y2, x2);
            /* Advance */
            cave_set_feat(y2, x2, FEAT_TREE);

            /* Restore the current grid to a passable feature, AGAIN!!! */
            /* This is to remove branches created by the new tree */
            cave_set_feat(y, x, feat);

            /* Remember this */
            flag = TRUE;
            /*msg_print("The huorn moves!");*/
        }
    }

    /* Update things */
    if (flag) handle_stuff();

    return (flag);
}


