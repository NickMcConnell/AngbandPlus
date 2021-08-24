/* File: was a part of spells2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"


/*
 * Detect any trap on a square
 */
static bool detect_traps(int y, int x)
{
    /* Mark as trap-detected */
    dungeon_info[y][x].cave_info |= (CAVE_DTRAP);

    /* Detect traps. Avoid glyphs */
    if (cave_player_trap_bold(y, x) ||
        cave_monster_trap_bold(y, x))
    {
        effect_type *x_ptr = &x_list[dungeon_info[y][x].effect_idx];

        /* Hack -- Memorize */
        x_ptr->x_flags &= ~(EF1_HIDDEN);

        /* Hack -- Memorize */
        dungeon_info[y][x].cave_info |= (CAVE_MARK);

        return (TRUE);
    }

    /* Result */
    return (FALSE);
}



/*
 * Detect any door on a square
 */
static bool detect_doors(int y, int x)
{
    /* Detect doors */
    if (dungeon_info[y][x].is_door())
    {

        /* Detect secret doors */
        if (cave_ff1_match(y, x, FF1_SECRET))
        {
            /* Place a door */
            cave_alter_feat(y, x, FS_SECRET);
        }

        /* Hack -- Memorize */
        dungeon_info[y][x].cave_info |= (CAVE_MARK);

        return (TRUE);
    }

    /* Result */
    return (FALSE);
}


/*
 * Detect any stair on a square
 */
static bool detect_stairs(int y, int x)
{
    /* Detect stairs */
    if (dungeon_info[y][x].is_stairs())
    {
        /* Find secrets */
        if (cave_ff1_match(y, x, FF1_SECRET))
        {
            cave_alter_feat(y, x, FS_SECRET);
        }

        /* Hack -- Memorize */
        dungeon_info[y][x].cave_info |= (CAVE_MARK);

        return (TRUE);
    }

    /* Result */
    return (FALSE);
}






/*
 * Detect if there is treasure on a square
 */
static bool detect_treasure(int y, int x)
{
    if (cave_ff1_match(y, x, FF1_HAS_GOLD))
    {
        /* Detect secrets */
        if (cave_ff1_match(y, x, FF1_SECRET))
        {

            /*Find secrets*/
            cave_alter_feat(y, x, FS_SECRET);
        }

        /* Hack -- Memorize */
        dungeon_info[y][x].cave_info |= (CAVE_MARK);

        return (TRUE);
    }

    /* Result */
    return (FALSE);
}



/*
 * Detect if there are "gold" objects on a square
 */
static bool detect_objects_gold(int y, int x)
{
    s16b this_o_idx, next_o_idx = 0;

    bool detect = FALSE;

    /* Nothing there */
    if ((!dungeon_info[y][x].object_idx) && (dungeon_info[y][x].monster_idx) < 1) return (FALSE);

    /* Check all objects on the screen */
    for (this_o_idx = dungeon_info[y][x].object_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr = &o_list[this_o_idx];

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Skip dead objects */
        if (!o_ptr->k_idx) continue;

        /* Detect "gold" objects */
        if (o_ptr->tval == TV_GOLD)
        {
            /* Hack -- memorize it */
            o_ptr->mark_object();

            /* Detect */
            detect = TRUE;
        }
    }

    /* Result */
    return (detect);
}

/*
 * Detect if there are "normal" objects on a square
 */
static bool detect_objects_normal(int y, int x)
{
    s16b this_o_idx, next_o_idx = 0;

    bool detect = FALSE;

    /* Nothing there */
    if ((!dungeon_info[y][x].object_idx) && (dungeon_info[y][x].monster_idx < 1)) return (FALSE);

    /* Check all objects on the screen */
    for (this_o_idx = dungeon_info[y][x].object_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr = &o_list[this_o_idx];

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Skip dead objects */
        if (!o_ptr->k_idx) continue;

        /* Detect "real" objects */
        if (o_ptr->tval != TV_GOLD)
        {
            /* Hack -- memorize it */
            o_ptr->mark_object();

            /* Detect */
            detect = TRUE;
        }
    }

    return (detect);
}



/*
 * Detect all "magic" objects or mimics on a square.
 */
static bool detect_objects_magic(int y, int x)
{
    s16b this_o_idx, next_o_idx = 0;
    int tv;

    bool detect = FALSE;

    /* Nothing there */
    if ((!dungeon_info[y][x].object_idx) && (dungeon_info[y][x].monster_idx) < 1) return (FALSE);

    /* Check all objects on the screen */
    for (this_o_idx = dungeon_info[y][x].object_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr = &o_list[this_o_idx];

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Skip dead objects */
        if (!o_ptr->k_idx) continue;

        /* Examine the tval */
        tv = o_ptr->tval;

        /* Artifacts, misc magic items, or enchanted wearables */
        if (o_ptr->is_artifact() || o_ptr->is_ego_item() ||
            (tv == TV_AMULET) || (tv == TV_RING) ||
            (tv == TV_STAFF) || (tv == TV_WAND) || (tv == TV_ROD) ||
            (tv == TV_SCROLL) || (tv == TV_POTION) ||
            (tv == TV_MAGIC_BOOK) || (tv == TV_PRAYER_BOOK) || (tv == TV_DRUID_BOOK) ||
            ((o_ptr->to_a > 0) || (o_ptr->to_h + o_ptr->to_d > 0)))
        {
            /* Hack -- memorize it */
            o_ptr->mark_object();

            detect = TRUE;
        }
    }

    /* Return result */
    return (detect);
}


/*
 * Detect a "normal" monsters on a specific square
 */
static bool detect_monsters_normal(int y, int x)
{
    /* No monster on this square */
    if (dungeon_info[y][x].monster_idx > 0)
    {

        monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Skip dead monsters */
        if (!m_ptr->r_idx) return (FALSE);

        /* Detect all non-invisible monsters */
        if (r_ptr->flags2 & (RF2_INVISIBLE)) return (FALSE);

        /* Optimize -- Repair flags */
        repair_mflag_mark = TRUE;
        repair_mflag_show = TRUE;

        /* Hack -- Detect the monster */
        m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

        /* Update the monster */
        update_mon(dungeon_info[y][x].monster_idx, FALSE);

        return (TRUE);
    }

    /* Result */
    return (FALSE);
}

/*
 * Detect all "living" monsters on the current panel, visible and invisible.
 */
static bool detect_monsters_living(int y, int x)
{
    /* No monster on this square */
    if (dungeon_info[y][x].monster_idx > 0)
    {

        monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Skip dead monsters */
        if (!m_ptr->r_idx) return (FALSE);

        /*Only detect living monsters*/
        if (monster_nonliving(r_ptr)) return (FALSE);

        /* Optimize -- Repair flags */
        repair_mflag_mark = TRUE;
        repair_mflag_show = TRUE;

        /* Hack -- Detect the monster */
        m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

        /* Update the monster */
        update_mon(dungeon_info[y][x].monster_idx, FALSE);

        return (TRUE);

    }

    /* Result */
    return (FALSE);
}


/*
 * Detect all "invisible" monsters on current panel
 */
static bool detect_monsters_invis(int y, int x)
{
    /* No monster on this square */
    if (dungeon_info[y][x].monster_idx > 0)
    {

        monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];
        monster_lore *l_ptr = &l_list[m_ptr->r_idx];

        /* Skip dead monsters */
        if (!m_ptr->r_idx) return (FALSE);

        /* Detect invisible monsters */
        if (r_ptr->flags2 & (RF2_INVISIBLE))
        {

            /* Take note that they are invisible */
            l_ptr->r_l_flags2 |= (RF2_INVISIBLE);

            /* Optimize -- Repair flags */
            repair_mflag_mark = TRUE;
            repair_mflag_show = TRUE;

            /* Hack -- Detect the monster */
            m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

            /* Update the monster */
            update_mon(dungeon_info[y][x].monster_idx, FALSE);

            return (TRUE);
        }
    }

    /* Result */
    return (FALSE);
}



/*
 * Detect all "evil" monsters on current panel
 */
static bool detect_monsters_evil(int y, int x)
{
    /* No monster on this square */
    if (dungeon_info[y][x].monster_idx > 0)
    {

        monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];
        monster_lore *l_ptr = &l_list[m_ptr->r_idx];

        /* Skip dead monsters */
        if (!m_ptr->r_idx) return (FALSE);

        /* Detect evil monsters */
        if (r_ptr->flags3 & (RF3_EVIL))
        {
            /* Take note that they are evil */
            l_ptr->r_l_flags3 |= (RF3_EVIL);

            /* Optimize -- Repair flags */
            repair_mflag_mark = TRUE;
            repair_mflag_show = TRUE;

            /* Detect the monster */
            m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

            /* Update the monster */
            update_mon(dungeon_info[y][x].monster_idx, FALSE);

            return (TRUE);

        }
    }

    /* Result */
    return (FALSE);
}

/*
 * Detect Terrain
 */
static bool detect_terrain(int y, int x)
{

    /* Check the terrain*/
    if (feat_ff3_match(dungeon_info[y][x].feature_idx, TERRAIN_MASK))
    {
        /* Memorize the grid */
        dungeon_info[y][x].cave_info |= (CAVE_MARK | CAVE_GLOW);

        /* We have seen the feature */
        f_info[dungeon_info[y][x].feature_idx].f_everseen = TRUE;
    }

    else return FALSE;

    return (TRUE);
}

/*
 * Detect Terrain
 */
static bool detect_map(int y, int x)
{
    int i;

    /* All non-walls are "checked"*/
    if (!(f_info[dungeon_info[y][x].feature_idx].f_flags1 & (FF1_WALL)))
    {
        /* Memorize normal features */
        if (f_info[dungeon_info[y][x].feature_idx].f_flags1 & (FF1_REMEMBER))
        {
            /* Memorize the object */
            dungeon_info[y][x].cave_info |= (CAVE_MARK);
        }

        /* Memorize known walls */
        for (i = 0; i < 8; i++)
        {
            int yy = y + ddy_ddd[i];
            int xx = x + ddx_ddd[i];

            /* Memorize walls (etc) */
            if (f_info[dungeon_info[yy][xx].feature_idx].f_flags1 & (FF1_REMEMBER))
            {
                /* Memorize the walls */
                dungeon_info[yy][xx].cave_info |= (CAVE_MARK);
            }
        }
    }

    else return (FALSE);

    return (TRUE);
}

/*
 * Struct of sidebar handlers.
 */
static const struct detect_handler_t
{
    u16b detect_type;
    bool (*hook)(int, int);	 /* int y, int x */
    QString detect_message;
} detect_handlers[] =
{
    {DETECT_INVISIBLE, 	detect_monsters_invis, 	"You sense the presence of invisible creatures!"},
    {DETECT_EVIL, 		detect_monsters_evil, 	"You sense the presence of evil creatures!"},
    {DETECT_LIFE, 		detect_monsters_living, "You sense the presence of living creatures!"},
    {DETECT_MONSTERS, 	detect_monsters_normal, "You sense the presence of monsters!"},
    {DETECT_GOLD, 		detect_objects_gold, 	"You sense the presence of treasure!"},
    {DETECT_TREASURE, 	detect_treasure, 		"You sense the presence of buried treasure!"},
    {DETECT_ENCHANTMENT,detect_objects_magic, 	"You sense the presence of magic objects!"},
    {DETECT_OBJECTS, 	detect_objects_normal, 	"You sense the presence of objects!"},
    {DETECT_DOORS, 		detect_doors, 			"You sense the presence of doors!"},
    {DETECT_STAIRS, 	detect_stairs, 			"You sense the presence of stairs!"},
    {DETECT_TERRAIN, 	detect_terrain, 		"You sense the presence of unusual terrain!"},
    {DETECT_TRAPS, 		detect_traps, 			"You sense the presence of traps!"},
    {DETECT_MAP, 		detect_map, 			"You sense the dungeon around you!"},

};

bool detect(int dist, u16b detect_checks)
{
    u16b detect_type_found = 0L;
    int y, x;
    u16b i;
    bool refresh = FALSE;

    /* Square the distance for later use */
    int dist_squared = dist * dist;

    /* Show the player the highlighted region */
    ui_animate_detection(p_ptr->py, p_ptr->px, dist);

    /* Hack - always refresh map and statusline if detect_traps is called */
    if (detect_checks & (DETECT_TRAPS))
    {
        refresh = TRUE;

        /* Update the detect statusline */
        p_ptr->redraw |= (PR_STATUSBAR);
    }

    /* Go through and check all of the applicable detection functions */
    for (i = 0; i < N_ELEMENTS(detect_handlers); i++)
    {
        const struct detect_handler_t *dtc = &detect_handlers[i];

        /* We aren't trying to detect this one, continue */
        if (!(detect_checks & (dtc->detect_type))) continue;

        /* Scan the map */
        for (y = 0; y < p_ptr->cur_map_hgt; y++)
        {
            for (x = 0; x < p_ptr->cur_map_wid; x++)
            {
                int py = p_ptr->py - y;
                int px = p_ptr->px - x;

                /* Ensure we are inside the detection radius (using pythagorean's theorum) */
                if ((px * px + py * py) > dist_squared) continue;

                /* We detected something */
                if (dtc->hook(y, x))
                {
                    /* Mark it so we can print out the message when we are done */
                    detect_type_found |= (dtc->detect_type);
                }

            }
        }
    }

    /* Nothing found */
    if ((!detect_type_found) && (!refresh)) return (FALSE);

    p_ptr->redraw |= PR_MAP;
    handle_stuff();


    /* Print out the messages */
    for (i = 0; i < N_ELEMENTS(detect_handlers); i++)
    {
        const struct detect_handler_t *dtc = &detect_handlers[i];

        /* We aren't trying to detect this one, continue */
        if (!(detect_type_found & (dtc->detect_type))) continue;

        message(dtc->detect_message);
    }

    return (TRUE);
}

