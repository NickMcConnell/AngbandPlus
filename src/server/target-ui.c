/*
 * File: target-ui.c
 * Purpose: UI for targetting code
 *
 * Copyright (c) 1997-2014 Angband contributors
 * Copyright (c) 2016 MAngband and PWMAngband Developers
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */


#include "s-angband.h"


/*
 * Display targeting help at the bottom of the screen
 */
void target_display_help(char *help, size_t len, bool monster, bool free)
{
    /* Display help */
    my_strcpy(help, "[Press <dir>, 'p', 'q', 'r'", len);
    if (free) my_strcat(help, ", 'm'", len);
    else my_strcat(help, ", '+', '-', 'o'", len);
    if (monster || free) my_strcat(help, ", 't'", len);
    my_strcat(help, ", Return, or Space]", len);
}


/*
 * Perform the minimum "whole panel" adjustment to ensure that the given
 * location is contained inside the current panel, and return true if any
 * such adjustment was performed.
 */
static bool adjust_panel_help(struct player *p, int y, int x)
{
    bool changed = false;
    int wx, wy;
    int screen_hgt, screen_wid;

    screen_hgt = p->screen_rows / p->tile_hgt;
    screen_wid = p->screen_cols / p->tile_wid;

    wy = p->offset_y;
    wx = p->offset_x;

    /* Adjust as needed */
    while (y >= wy + screen_hgt) wy += screen_hgt / 2;
    while (y < wy) wy -= screen_hgt / 2;

    /* Adjust as needed */
    while (x >= wx + screen_wid) wx += screen_wid / 2;
    while (x < wx) wx -= screen_wid / 2;

    /* Use "modify_panel" */
    if (modify_panel(p, wy, wx)) changed = true;

    return (changed);
}


/*
 * Do we need to inform client about target info?
 */
static bool need_target_info(struct player *p, u32b query, byte step)
{
    bool need_info = false;

    /* Acknowledge */
    if (query == '\0') need_info = true;

    /* Next step */
    if (p->tt_step < step) need_info = true;

    /* Print help */
    if ((query == KC_ENTER) && (p->tt_step == step) && p->tt_help)
        need_info = true;

    /* Advance step */
    if (need_info) p->tt_step = step;

    /* Clear help */
    else p->tt_help = false;

    return need_info;
}


/*
 * Inform client about target info
 */
static bool target_info(struct player *p, int y, int x, const char *info, const char *help,
    u32b query)
{
    int col = x - p->offset_x;
    int row = y - p->offset_y + 1;
    bool dble = true;

    /* Do nothing on quit */
    if ((query == 'q') || (query == ESCAPE)) return false;

    /* Hack -- is there something targetable above our position? */
    if (square_in_bounds_fully(chunk_get(p->depth), y - 1, x) && target_accept(p, y - 1, x))
        dble = false;

    /* Display help info */
    if (p->tt_help)
        Send_target_info(p, col, row, dble, help);

    /* Display target info */
    else
        Send_target_info(p, col, row, dble, info);

    /* Toggle help */
    p->tt_help = !p->tt_help;

    return true;
}


/*
 * Examine a grid, return a keypress.
 *
 * The "mode" argument contains the "TARGET_LOOK" bit flag, which
 * indicates that the "space" key should scan through the contents
 * of the grid, instead of simply returning immediately.  This lets
 * the "look" command get complete information, without making the
 * "target" command annoying.
 *
 * The "info" argument contains the "commands" which should be shown
 * inside the "[xxx]" text.  This string must never be empty, or grids
 * containing monsters will be displayed with an extra comma.
 *
 * Note that if a monster is in the grid, we update both the monster
 * recall info and the health bar info to track that monster.
 *
 * This function correctly handles multiple objects per grid, and objects
 * and terrain features in the same grid, though the latter never happens.
 *
 * This function must handle blindness/hallucination.
 */
static bool target_set_interactive_aux(struct player *p, int y, int x, int mode,
    const char *help, u32b query)
{
    const char *s1, *s2, *s3;
    bool boring;
    int feat;
    int floor_max = z_info->floor_size;
    struct object **floor_list = mem_zalloc(floor_max * sizeof(*floor_list));
    int floor_num;
    char out_val[256];
    char coords[20];
    int tries = 200;
    struct chunk *c = chunk_get(p->depth);
    struct actor who_body;
    struct actor *who = &who_body;

    square_actor(c, y, x, who);

    /* Describe the square location */
    coords_desc(p, coords, sizeof(coords), y, x);

    /* Repeat forever */
    while (tries--)
    {
        struct trap *trap;

        /* Assume boring */
        boring = true;

        /* Default */
        s1 = "You see ";
        s2 = "";
        s3 = "";

        /* The player */
        if (who->player && (who->player == p))
        {
            /* Description */
            s1 = "You are ";

            /* Preposition */
            s2 = "on ";
        }

        /* Hallucination messes things up */
        if (p->timed[TMD_IMAGE])
        {
            const char *name = "something strange";

            /* Display a message */
            strnfmt(out_val, sizeof(out_val), "%s%s%s%s, %s.", s1, s2, s3, name, coords);

            /* Inform client */
            if (need_target_info(p, query, TARGET_NONE))
            {
                mem_free(floor_list);
                return target_info(p, y, x, out_val, help, query);
            }

            /* Stop on everything but "return" */
            if (query == KC_ENTER)
            {
                query = '\0';
                continue;
            }

            mem_free(floor_list);
            return false;
        }

        /* Actual players */
        if (who->player && (who->player != p))
        {
            /* Visible */
            if (mflag_has(p->pflag[who->idx], MFLAG_VISIBLE))
            {
                bool recall = false;
                char player_name[NORMAL_WID];

                /* Not boring */
                boring = false;

                /* Unaware players get a pseudo description */
                if (who->player->k_idx)
                {
                    /* Acting as an object: get a pseudo object description */
                    if (who->player->k_idx > 0)
                    {
                        struct object_kind *kind = &k_info[who->player->k_idx];
                        struct object *fake = object_new();

                        object_prep(p, fake, kind, 0, MINIMISE);
                        if (tval_is_money_k(kind)) fake->pval = 1;
                        object_desc(p, player_name, sizeof(player_name), fake,
                            ODESC_PREFIX | ODESC_BASE);
                        object_delete(&fake);
                    }

                    /* Acting as a feature: get a pseudo feature description */
                    else
                    {
                        feat = feat_pseudo(who->player->poly_race->d_char);
                        my_strcpy(player_name, f_info[feat].name, sizeof(player_name));
                        s3 = (is_a_vowel(player_name[0])? "an ": "a ");
                    }

                    /* Describe the player */
                    strnfmt(out_val, sizeof(out_val), "%s%s%s%s, %s.", s1, s2, s3, player_name,
                        coords);

                    /* Inform client */
                    if (need_target_info(p, query, TARGET_MON))
                    {
                        mem_free(floor_list);
                        return target_info(p, y, x, out_val, help, query);
                    }

                    /* Stop on everything but "return" */
                    if (query != KC_ENTER) break;

                    /* Paranoia */
                    mem_free(floor_list);
                    return true;
                }

                /* Get the player name */
                strnfmt(player_name, sizeof(player_name), "%s the %s %s", who->player->name,
                    who->player->race->name, who->player->clazz->name);

                /* Hack -- track this player */
                monster_race_track(p->upkeep, who);

                /* Hack -- health bar for this player */
                health_track(p->upkeep, who);

                /* Hack -- track cursor for this player */
                cursor_track(p, who);

                /* Hack -- handle stuff */
                handle_stuff(p);

                /* Interact */
                if ((query == 'r') && (p->tt_step == TARGET_MON))
                    recall = true;

                /* Recall or target */
                if (recall)
                {
                    /* Recall on screen */
                    do_cmd_describe(p);
                    mem_free(floor_list);
                    return false;
                }
                else
                {
                    char buf[NORMAL_WID];

                    /* Describe the player */
                    look_player_desc(who->player, buf, sizeof(buf));

                    /* Describe, and prompt for recall */
                    strnfmt(out_val, sizeof(out_val), "%s%s%s%s (%s), %s.",
                        s1, s2, s3, player_name, buf, coords);

                    /* Inform client */
                    if (need_target_info(p, query, TARGET_MON))
                    {
                        mem_free(floor_list);
                        return target_info(p, y, x, out_val, help, query);
                    }
                }

                /* Stop on everything but "return"/"space" */
                if ((query != KC_ENTER) && (query != ' ')) break;

                /* Sometimes stop at "space" key */
                if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

                /* Take account of gender */
                if (who->player->psex == SEX_FEMALE) s1 = "She is ";
                else if (who->player->psex == SEX_MALE) s1 = "He is ";
                else s1 = "It is ";

                /* Use a preposition */
                s2 = "on ";
            }
        }

        /* Actual monsters */
        if (who->mon)
        {
            /* Visible */
            if (mflag_has(p->mflag[who->idx], MFLAG_VISIBLE) && !who->mon->unaware)
            {
                bool recall = false;
                char m_name[NORMAL_WID];

                /* Not boring */
                boring = false;

                /* Get the monster name ("a kobold") */
                monster_desc(p, m_name, sizeof(m_name), who->mon, MDESC_IND_VIS);

                /* Hack -- track this monster race */
                monster_race_track(p->upkeep, who);

                /* Hack -- health bar for this monster */
                health_track(p->upkeep, who);

                /* Hack -- track cursor for this monster */
                cursor_track(p, who);

                /* Hack -- handle stuff */
                handle_stuff(p);

                /* Interact */
                if ((query == 'r') && (p->tt_step == TARGET_MON))
                    recall = true;

                /* Recall */
                if (recall)
                {
                    /* Recall on screen */
                    do_cmd_describe(p);
                    mem_free(floor_list);
                    return false;
                }

                /* Normal */
                else
                {
                    char buf[NORMAL_WID];

                    /* Describe the monster */
                    look_mon_desc(who->mon, buf, sizeof(buf));

                    /* Describe, and prompt for recall */
                    strnfmt(out_val, sizeof(out_val), "%s%s%s%s (%s), %s.", s1, s2, s3, m_name, buf,
                        coords);

                    /* Inform client */
                    if (need_target_info(p, query, TARGET_MON))
                    {
                        mem_free(floor_list);
                        return target_info(p, y, x, out_val, help, query);
                    }
                }

                /* Stop on everything but "return"/"space" */
                if ((query != KC_ENTER) && (query != ' ')) break;

                /* Sometimes stop at "space" key */
                if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

                /* Take account of gender */
                if (rf_has(who->mon->race->flags, RF_FEMALE)) s1 = "She is ";
                else if (rf_has(who->mon->race->flags, RF_MALE)) s1 = "He is ";
                else s1 = "It is ";

                /* Disabled for non-DMs since monsters now carry their drops */
                if (is_dm_p(p))
                {
                    /* Use a verb */
                    s2 = "carrying ";

                    /* Change the intro */
                    if (p->tt_o) s2 = "also carrying ";

                    /* Scan all objects being carried */
                    if (!p->tt_o) p->tt_o = who->mon->held_obj;
                    else p->tt_o = p->tt_o->next;
                    if (p->tt_o)
                    {
                        char o_name[NORMAL_WID];

                        /* Obtain an object description */
                        object_desc(p, o_name, sizeof(o_name), p->tt_o, ODESC_PREFIX | ODESC_FULL);

                        /* Describe the object */
                        strnfmt(out_val, sizeof(out_val), "%s%s%s%s, %s.", s1, s2, s3, o_name,
                            coords);

                        /* Inform client */
                        mem_free(floor_list);
                        return target_info(p, y, x, out_val, help, query);
                    }
                }

                /* Use a preposition */
                s2 = "on ";
            }
        }

        /* A trap */
        trap = square_known_trap(p, c, y, x);
		if (trap)
		{
			bool recall = false;

			/* Not boring */
			boring = false;

            /* Pick proper indefinite article */
            s3 = (is_a_vowel(trap->kind->desc[0])? "an ": "a ");

            /* Interact */
            if ((query == 'r') && (p->tt_step == TARGET_TRAP))
                recall = true;

            /* Recall */
            if (recall)
            {
                /* Recall on screen */
                describe_trap(p, trap);
                mem_free(floor_list);
                return false;
            }

            /* Normal */
            else
            {
                /* Describe, and prompt for recall */
                strnfmt(out_val, sizeof(out_val), "%s%s%s%s, %s.", s1, s2, s3, trap->kind->desc,
                    coords);

                /* Inform client */
                if (need_target_info(p, query, TARGET_TRAP))
                {
                    mem_free(floor_list);
                    return target_info(p, y, x, out_val, help, query);
                }
            }

            /* Stop on everything but "return"/"space" */
            if ((query != KC_ENTER) && (query != ' ')) break;

            /* Sometimes stop at "space" key */
            if ((query == ' ') && !(mode & (TARGET_LOOK))) break;
		}

		/* Double break */
		if (square_known_trap(p, c, y, x)) break;

        /* Scan all sensed objects in the grid */
        floor_num = scan_distant_floor(p, c, floor_list, floor_max, y, x);
        if (floor_num > 0)
        {
            /* Not boring */
            boring = false;

            track_object(p->upkeep, floor_list[0]);
            handle_stuff(p);

            /* If there is more than one item... */
            if (floor_num > 1)
            {
                /* Describe the pile */
                strnfmt(out_val, sizeof(out_val), "%s%s%sa pile of %d objects, %s.",
                    s1, s2, s3, floor_num, coords);

                /* Inform client */
                if (need_target_info(p, query, TARGET_OBJ))
                {
                    mem_free(floor_list);
                    return target_info(p, y, x, out_val, help, query);
                }

                /* Display objects */
                if (query == 'r')
                {
                    msg(p, "You see:");
                    display_floor(p, c, floor_list, floor_num);
                    show_floor(p, OLIST_WEIGHT | OLIST_GOLD);
                    mem_free(floor_list);
                    return false;
                }

                /* Done */
                break;
            }

            /* Only one object to display */
            else
            {
                bool recall = false;
                char o_name[NORMAL_WID];

                /* Get the single object in the list */
                struct object *obj = floor_list[0];

                /* Not boring */
                boring = false;

                /* Obtain an object description */
                object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);

                /* Interact */
                if ((query == 'r') && (p->tt_step == TARGET_OBJ))
                    recall = true;

                /* Recall */
                if (recall)
                {
                    /* Recall on screen */
                    display_object_recall_interactive(p, obj, o_name);
                    mem_free(floor_list);
                    return false;
                }

                /* Normal */
                else
                {
                    /* Describe, and prompt for recall */
                    strnfmt(out_val, sizeof(out_val), "%s%s%s%s, %s.", s1, s2, s3, o_name, coords);

                    /* Inform client */
                    if (need_target_info(p, query, TARGET_OBJ))
                    {
                        mem_free(floor_list);
                        return target_info(p, y, x, out_val, help, query);
                    }
                }

                /* Stop on everything but "return"/"space" */
                if ((query != KC_ENTER) && (query != ' ')) break;

                /* Sometimes stop at "space" key */
                if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

                /* Plurals */
                s1 = VERB_AGREEMENT(obj->number, "It is ", "They are ");

                /* Preposition */
                s2 = "on ";
            }
        }

        feat = square_apparent_feat(p, c, y, x);

        /* Terrain feature if needed */
        if (boring || feat_isterrain(feat))
        {
            const char *name = square_apparent_name(p, c, y, x);
            bool recall = false;

            /* Pick a prefix */
            if (*s2 && feat_isprefixed(feat))
                s2 = "in ";

            /* Pick proper indefinite article */
            s3 = (is_a_vowel(name[0])? "an ": "a ");

            /* Hack -- special introduction for store doors */
            if (feat_is_shop(feat)) s3 = "the entrance to the ";

            /* Interact */
            if ((query == 'r') && (p->tt_step == TARGET_FEAT))
                recall = true;

            /* Recall */
            if (recall)
            {
                /* Recall on screen */
                describe_feat(p, &f_info[feat]);
                mem_free(floor_list);
                return false;
            }

            /* Normal */
            else
            {
                /* Message */
                strnfmt(out_val, sizeof(out_val), "%s%s%s%s, %s.", s1, s2, s3, name, coords);

                /* Inform client */
                if (need_target_info(p, query, TARGET_FEAT))
                {
                    mem_free(floor_list);
                    return target_info(p, y, x, out_val, help, query);
                }
            }

            /* Stop on everything but "return"/"space" */
            if ((query != KC_ENTER) && (query != ' ')) break;
        }

        /* Stop on everything but "return" */
        if (query != KC_ENTER) break;

        /* Paranoia */
        mem_free(floor_list);
        return true;
    }

    /* Paranoia */
    if (!tries)
        plog_fmt("Infinite loop in target_set_interactive_aux: %c", query);

    mem_free(floor_list);

    /* Keep going */
    return false;
}


/*
 * Draw a visible path over the squares between (x1,y1) and (x2,y2).
 *
 * The path consists of "*", which are white except where there is a
 * monster, object or feature in the grid.
 *
 * This routine has (at least) three weaknesses:
 * - remembered objects/walls which are no longer present are not shown,
 * - squares which (e.g.) the player has walked through in the dark are
 *   treated as unknown space.
 * - walls which appear strange due to hallucination aren't treated correctly.
 *
 * The first two result from information being lost from the dungeon arrays,
 * which requires changes elsewhere.
 */
int draw_path(struct player *p, u16b path_n, struct loc *path_g, int y1, int x1)
{
    int i;
    bool on_screen;
    struct chunk *c = chunk_get(p->depth);

    /* No path, so do nothing. */
    if (path_n < 1) return 0;

    /*
     * The starting square is never drawn, but notice if it is being
     * displayed. In theory, it could be the last such square.
     */
    on_screen = panel_contains(p, y1, x1);

    /* Draw the path. */
    for (i = 0; i < path_n; i++)
    {
        byte colour;

        /* Find the co-ordinates on the level. */
        int y = path_g[i].y;
        int x = path_g[i].x;

        struct object *obj = floor_pile_known(p, c, y, x);

        struct actor who_body;
        struct actor *who = &who_body;

        /*
         * As path[] is a straight line and the screen is oblong,
         * there is only section of path[] on-screen.
         * If the square being drawn is visible, this is part of it.
         * If none of it has been drawn, continue until some of it
         * is found or the last square is reached.
         * If some of it has been drawn, finish now as there are no
         * more visible squares to draw.
         */
        if (panel_contains(p, y, x)) on_screen = true;
        else if (on_screen) break;
        else continue;

        square_actor(c, y, x, who);

        /* Choose a colour (monsters). */
        if (who->mon && mflag_has(p->mflag[who->idx], MFLAG_VISIBLE))
        {
            /* Mimics act as objects */
            if (who->mon->unaware && who->mon->mimicked_obj)
                colour = COLOUR_YELLOW;

            /* Visible monsters are red. */
            else
                colour = COLOUR_L_RED;
        }

        /* Choose a colour (players). */
        else if (who->player && mflag_has(p->pflag[who->idx], MFLAG_VISIBLE))
        {
            /* Player mimics act as objects (or features) */
            if (who->player->k_idx > 0)
                colour = COLOUR_YELLOW;
            else if (who->player->k_idx < 0)
                colour = COLOUR_WHITE;

            /* Visible players are red. */
            else
                colour = COLOUR_L_RED;
        }

        /* Known objects are yellow. */
        else if (obj)
            colour = COLOUR_YELLOW;

        /* Known walls are blue. */
        else if (!square_isprojectable(c, y, x) &&
            (square_isknown(p, y, x) || square_isseen(p, y, x)))
        {
            colour = COLOUR_BLUE;
        }

        /* Unknown squares are grey. */
        else if (!square_isknown(p, y, x) && !square_isseen(p, y, x))
            colour = COLOUR_L_DARK;

        /* Unoccupied squares are white. */
        else
            colour = COLOUR_WHITE;

        /* Draw the path segment */
        draw_path_grid(p, y, x, colour, '*');
    }

    /* Flush and wait (delay for consistency) */
    if (i) Send_flush(p, true, true);
    else Send_flush(p, true, false);

    return i;
}


/*
 * Load the attr/char at each point along "path" which is on screen.
 */
void load_path(struct player *p, u16b path_n, struct loc *path_g)
{
    int i;

    for (i = 0; i < path_n; i++)
    {
        int y = path_g[i].y;
        int x = path_g[i].x;

        if (!panel_contains(p, y, x)) continue;
        square_light_spot_aux(p, chunk_get(p->depth), y, x);
    }

    Send_flush(p, true, false);
    p->path_drawn = false;
}


/*
 * Extract a direction (or zero) from a character
 */
static int target_dir(u32b ch)
{
    int d = 0;

    /* Already a direction? */
    if (isdigit((unsigned char)ch))
        d = D2I(ch);
    else if (isarrow(ch))
    {
        switch (ch)
        {
            case ARROW_DOWN:  d = 2; break;
            case ARROW_LEFT:  d = 4; break;
            case ARROW_RIGHT: d = 6; break;
            case ARROW_UP:    d = 8; break;
        }
    }

    /* Paranoia */
    if (d == 5) d = 0;

    /* Return direction */
    return (d);
}


static void set_tt_m(struct player *p, s16b m)
{
    p->tt_m = m;
    p->tt_o = NULL;
}


/*
 * Handle "target" and "look".
 *
 * Note that this code can be called from "get_aim_dir()".
 *
 * Currently, when "flag" is true, that is, when
 * "interesting" grids are being used, and a directional key is used, we
 * only scroll by a single panel, in the direction requested, and check
 * for any interesting grids on that panel.  The "correct" solution would
 * actually involve scanning a larger set of grids, including ones in
 * panels which are adjacent to the one currently scanned, but this is
 * overkill for this function.  XXX XXX
 *
 * Hack -- targeting/observing an "outer border grid" may induce
 * problems, so this is not currently allowed.
 *
 * The player can use the direction keys to move among "interesting"
 * grids in a heuristic manner, or the "space", "+", and "-" keys to
 * move through the "interesting" grids in a sequential manner, or
 * can enter "location" mode, and use the direction keys to move one
 * grid at a time in any direction.  The "t" (set target) command will
 * only target a monster (as opposed to a location) if the monster is
 * target_able and the "interesting" mode is being used.
 *
 * The current grid is described using the "look" method above, and
 * a new command may be entered at any time, but note that if the
 * "TARGET_LOOK" bit flag is set (or if we are in "location" mode,
 * where "space" has no obvious meaning) then "space" will scan
 * through the description of the current grid until done, instead
 * of immediately jumping to the next "interesting" grid.  This
 * allows the "target" command to retain its old semantics.
 *
 * The "*", "+", and "-" keys may always be used to jump immediately
 * to the next (or previous) interesting grid, in the proper mode.
 *
 * The "return" key may always be used to scan through a complete
 * grid description (forever).
 *
 * This command will cancel any old target, even if used from
 * inside the "look" command.
 *
 * 'mode' is one of TARGET_LOOK or TARGET_KILL.
 * 'x' and 'y' are the initial position of the target to be highlighted,
 * or -1 if no location is specified.
 * Returns true if a target has been successfully set, false otherwise.
 */
bool target_set_interactive(struct player *p, int mode, u32b query)
{
    int py = p->py;
    int px = p->px;
    int i, d, t, bd;
    bool done = false;
    struct point_set *targets;
    bool good_target;
    char help[NORMAL_WID];
    struct actor old_target_who_body;
    struct actor *old_target_who = &old_target_who_body;
    int old_target_x, old_target_y;
    bool auto_target = false;
    int tries = 200;
    struct chunk *c = chunk_get(p->depth);

    /* Paranoia */
    if (!c) return false;

    /* Remove old targeting path */
    if (p->path_drawn) load_path(p, p->path_n, p->path_g);

    /* Hack -- auto-target if requested */
    if ((mode & (TARGET_AIM)) && OPT_P(p, use_old_target) && target_okay(p))
    {
        memcpy(old_target_who, &p->target_who, sizeof(struct actor));
        old_target_x = p->target_x;
        old_target_y = p->target_y;
        auto_target = true;
    }

    if (query == '\0')
    {
        p->tt_flag = true;
        p->tt_step = TARGET_NONE;
        p->tt_help = false;
    }

    /* Start on the player */
    if (query == '\0')
    {
        p->tt_x = p->px;
        p->tt_y = p->py;

        /* Hack -- auto-target if requested */
        if (auto_target)
        {
            p->tt_x = old_target_x;
            p->tt_y = old_target_y;
        }
    }

    /* Cancel target */
	target_set_monster(p, NULL);

    /* Cancel tracking */
    cursor_track(p, NULL);

    /* Prepare the "temp" array */
    targets = target_get_monsters(p, mode);

    /* Start near the player */
    if (query == '\0')
    {
        set_tt_m(p, 0);

        /* Hack -- auto-target if requested */
        if (auto_target)
        {
            /* Find the old target */
            for (i = 0; i < point_set_size(targets); i++)
            {
                int temp_y = targets->pts[i].y;
                int temp_x = targets->pts[i].x;
                int temp_idx;
                struct actor temp_who_body;
                struct actor *temp_who = &temp_who_body;

                temp_idx = c->squares[temp_y][temp_x].mon;
                ACTOR_WHO(temp_who, temp_idx, player_get(0 - temp_idx), cave_monster(c, temp_idx));

                if (ACTOR_EQUAL(temp_who, old_target_who))
                {
                    set_tt_m(p, i);
                    break;
                }
            }
        }
    }

    /* Interact */
    while (tries-- && !done)
    {
        bool ret;

        /* Paranoia: grids could have changed! */
        if (p->tt_m >= point_set_size(targets)) set_tt_m(p, point_set_size(targets) - 1);
        if (p->tt_m < 0) set_tt_m(p, 0);

#ifdef NOTARGET_PROMPT
        /* No targets */
        if (p->tt_flag && !point_set_size(targets))
        {
            /* Analyze */
            switch (query)
            {
                case ESCAPE:
                case 'q': break;

                case 'p':
                {
                    p->tt_flag = false;
                    break;
                }

                default:
                {
                    int col = p->px - p->offset_x;
                    int row = p->py - p->offset_y + 1;
                    bool dble = true;

                    /* Hack -- is there something targetable above our position? */
                    if (square_in_bounds_fully(c, p->py - 1, p->px) &&
                        target_accept(p, p->py - 1, p->px))
                    {
                        dble = false;
                    }

                    Send_target_info(p, col, row, dble, "Nothing to target. [p,q]");
                    point_set_dispose(targets);
                    return false;
                }
            }
        }
#endif

        /* Interesting grids if chosen and there are any, otherwise arbitrary */
        if (p->tt_flag && point_set_size(targets))
        {
            struct actor who_body;
            struct actor *who = &who_body;

            p->tt_y = targets->pts[p->tt_m].y;
            p->tt_x = targets->pts[p->tt_m].x;

            /* Adjust panel if needed */
            if (adjust_panel_help(p, p->tt_y, p->tt_x)) handle_stuff(p);

            /* Update help */
            square_actor(c, p->tt_y, p->tt_x, who);
            good_target = target_able(p, who);
            target_display_help(help, sizeof(help), good_target, false);

            /* Find the path. */
            p->path_n = project_path(p->path_g, z_info->max_range, c, py, px, p->tt_y,
                p->tt_x, PROJECT_THRU);

            /* Draw the path in "target" mode. If there is one */
            if (mode & TARGET_KILL)
                p->path_drawn = draw_path(p, p->path_n, p->path_g, py, px);

            /* Describe and Prompt */
            ret = target_set_interactive_aux(p, p->tt_y, p->tt_x, mode, help, query);

            if (ret)
            {
                point_set_dispose(targets);
                return false;
            }

            /* Remove the path */
            if (p->path_drawn) load_path(p, p->path_n, p->path_g);

            /* Assume no "direction" */
            d = 0;

            /* Analyze */
            switch (query)
            {
                case ESCAPE:
                case 'q':
                case 'r':
                {
                    done = true;
                    break;
                }

                case ' ':
                case '(':
                case '*':
                case '+':
                {
                    set_tt_m(p, p->tt_m + 1);
                    if (p->tt_m == point_set_size(targets)) set_tt_m(p, 0);

                    /* Hack -- acknowledge */
                    query = '\0';

                    break;
                }

                case '-':
                {
                    set_tt_m(p, p->tt_m - 1);
                    if (p->tt_m == -1) set_tt_m(p, point_set_size(targets) - 1);

                    /* Hack -- acknowledge */
                    query = '\0';

                    break;
                }

                case 'p':
                {
                    /* Recenter around player */
                    verify_panel(p);

                    /* Handle stuff */
                    handle_stuff(p);

                    p->tt_y = p->py;
                    p->tt_x = p->px;
                }

                case 'o':
                {
                    p->tt_flag = false;

                    /* Hack -- acknowledge */
                    query = '\0';

                    break;
                }

                case 'm':
                {
                    /* Hack -- acknowledge */
                    query = '\0';

                    break;
                }

                case 't':
                case '5':
                case '0':
                case '.':
                {
                    square_actor(c, p->tt_y, p->tt_x, who);

                    if (target_able(p, who))
                    {
                        health_track(p->upkeep, who);
                        target_set_monster(p, who);
                    }

                    done = true;

                    break;
                }

                default:
                {
                    /* Extract direction */
                    d = target_dir(query);

                    /* Oops */
                    if (!d)
                    {
                        /* Hack -- acknowledge */
                        if (query != KC_ENTER) query = '\0';
                    }

                    break;
                }
            }

            /* Hack -- move around */
            if (d)
            {
                int old_y = targets->pts[p->tt_m].y;
                int old_x = targets->pts[p->tt_m].x;

                /* Find a new monster */
                i = target_pick(old_y, old_x, ddy[d], ddx[d], targets);

                /* Scroll to find interesting grid */
                if (i < 0)
                {
                    int old_wy = p->offset_y;
                    int old_wx = p->offset_x;

                    /* Change if legal */
                    if (change_panel(p, d))
                    {
                        /* Recalculate interesting grids */
                        point_set_dispose(targets);
                        targets = target_get_monsters(p, mode);

                        /* Find a new monster */
                        i = target_pick(old_y, old_x, ddy[d], ddx[d], targets);

                        /* Restore panel if needed */
                        if ((i < 0) && modify_panel(p, old_wy, old_wx))
                        {
                            /* Recalculate interesting grids */
                            point_set_dispose(targets);
                            targets = target_get_monsters(p, mode);
                        }

                        /* Handle stuff */
                        handle_stuff(p);
                    }
                }

                /* Use interesting grid if found */
                if (i >= 0) set_tt_m(p, i);

                /* Hack -- acknowledge */
                query = '\0';
            }
        }
        else
        {
            struct actor who_body;
            struct actor *who = &who_body;

            /* Update help */
            square_actor(c, p->tt_y, p->tt_x, who);
            good_target = target_able(p, who);
            target_display_help(help, sizeof(help), good_target, true);

            /* Find the path. */
            p->path_n = project_path(p->path_g, z_info->max_range, c, py, px, p->tt_y,
                p->tt_x, PROJECT_THRU);

            /* Draw the path in "target" mode. If there is one */
            if (mode & TARGET_KILL)
                p->path_drawn = draw_path(p, p->path_n, p->path_g, py, px);

            /* Describe and Prompt (enable "TARGET_LOOK") */
            ret = target_set_interactive_aux(p, p->tt_y, p->tt_x, mode | TARGET_LOOK,
                help, query);

            if (ret)
            {
                point_set_dispose(targets);
                return false;
            }

            /* Remove the path */
            if (p->path_drawn) load_path(p, p->path_n, p->path_g);

            /* Assume no "direction" */
            d = 0;

            /* Analyze */
            switch (query)
            {
                case ESCAPE:
                case 'q':
                case 'r':
                {
                    done = true;
                    break;
                }

                case ' ':
                case '(':
                case '*':
                case '+':
                case '-':
                {
                    /* Hack -- acknowledge */
                    query = '\0';

                    break;
                }

                case 'p':
                {
                    /* Recenter around player */
                    verify_panel(p);

                    /* Handle stuff */
                    handle_stuff(p);

                    p->tt_y = p->py;
                    p->tt_x = p->px;
                }

                case 'o':
                {
                    /* Hack -- acknowledge */
                    query = '\0';

                    break;
                }

                case 'm':
                {
                    p->tt_flag = true;

                    set_tt_m(p, 0);
                    bd = 999;

                    /* Pick a nearby monster */
                    for (i = 0; i < point_set_size(targets); i++)
                    {
                        t = distance(p->tt_y, p->tt_x, targets->pts[i].y, targets->pts[i].x);

                        /* Pick closest */
                        if (t < bd)
                        {
                            set_tt_m(p, i);
                            bd = t;
                        }
                    }

                    /* Nothing interesting */
                    if (bd == 999) p->tt_flag = false;

                    /* Hack -- acknowledge */
                    query = '\0';

                    break;
                }

                case 't':
                case '5':
                case '0':
                case '.':
                {
                    target_set_location(p, p->tt_y, p->tt_x);
                    done = true;

                    break;
                }

                default:
                {
                    /* Extract a direction */
                    d = target_dir(query);

                    /* Oops */
                    if (!d)
                    {
                        /* Hack -- acknowledge */
                        if (query != KC_ENTER) query = '\0';
                    }

                    break;
                }
            }

            /* Handle "direction" */
            if (d)
            {
                /* Move */
                p->tt_x += ddx[d];
                p->tt_y += ddy[d];

                /* Slide into legality */
                if (p->tt_x >= c->width - 1) p->tt_x--;
                else if (p->tt_x <= 0) p->tt_x++;

                /* Slide into legality */
                if (p->tt_y >= c->height - 1) p->tt_y--;
                else if (p->tt_y <= 0) p->tt_y++;

                /* Adjust panel if needed */
                if (adjust_panel_help(p, p->tt_y, p->tt_x))
                {
                    /* Handle stuff */
                    handle_stuff(p);

                    /* Recalculate interesting grids */
                    point_set_dispose(targets);
                    targets = target_get_monsters(p, mode);
                }

                /* Hack -- acknowledge */
                query = '\0';
            }
        }
    }

    /* Paranoia */
    if (!tries) plog_fmt("Infinite loop in target_set_interactive: %lu", query);

    /* Forget */
    point_set_dispose(targets);

    /* Recenter around player */
    verify_panel(p);

    /* Handle stuff */
    handle_stuff(p);

    /* Failure to set target */
    if (!p->target_set) return false;

    /* Success */
    return true;
}