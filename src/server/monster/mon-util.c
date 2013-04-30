/*
 * File: mon-util.c
 * Purpose: Monster manipulation utilities.
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2012 MAngband and PWMAngband Developers
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


#include "../s-angband.h"
#include "mon-make.h"
#include "mon-spell.h"
#include "mon-timed.h"
#include "mon-util.h"
#include "../s-spells.h"
#include "../squelch.h"


/* Hack -- See summon_specific() */
char summon_kin_type;


/*
 * Return the monster base matching the given name.
 */
monster_base *lookup_monster_base(const char *name)
{
    monster_base *base;

    /* Look for it */
    for (base = rb_info; base; base = base->next)
    {
        /* Found a match */
        if (base->name && streq(base->name, name)) return base;
    }

    return NULL;
}


/*
 * Return whether the given base matches any of the names given.
 *
 * Accepts a variable-length list of name strings. The list must end with NULL.
 */
bool match_monster_bases(const monster_base *base, ...)
{
    bool ok = FALSE;
    va_list vp;
    char *name;

    va_start(vp, base);
    while (!ok && ((name = va_arg(vp, char *)) != NULL))
        ok = (base == lookup_monster_base(name));
    va_end(vp);

    return ok;
}


static void plural_aider(char *name, size_t max, char *aider)
{
    char dummy[NORMAL_WID];
    int i = 0;
    char *ctr = name;

    while (ctr < aider)
    {
        dummy[i] = *ctr;
        ctr++;
        i++;
    }

    if (dummy[i - 1] == 's')
    {
        strcpy(&(dummy[i]), "es");
        i++;
    }
    else
        strcpy(&(dummy[i]), "s");

    strcpy(&(dummy[i + 1]), aider);
    my_strcpy(name, dummy, max);
}


/*
 * Hack - Fix plural names of monsters
 */
void plural_aux(char *name, size_t max)
{
    int name_len = strlen(name);

    if (strstr(name, " of "))
        plural_aider(name, max, strstr(name, " of "));
    else if (strstr(name, " to "))
        plural_aider(name, max, strstr(name, " to "));
    else if (strstr(name, "coins"))
    {
        char dummy[NORMAL_WID];

        strcpy(dummy, "Piles of c");
        my_strcat(dummy, &(name[1]), sizeof(dummy));
        my_strcpy(name, dummy, max);
    }
    else if (strstr(name, "Manes") || strstr(name, "Erinyes"))
        return;
    else if (name[name_len - 1] == 'y')
        strcpy(&(name[name_len - 1]), "ies");
    else if (streq(&(name[name_len - 4]), "ouse"))
        strcpy(&(name[name_len - 4]), "ice");
    else if (streq(&(name[name_len - 3]), "sus"))
        strcpy(&(name[name_len - 3]), "si");
    else if (streq(&(name[name_len - 5]), "culus"))
        strcpy(&(name[name_len - 5]), "culi");
    else if (streq(&(name[name_len - 3]), "bus"))
        strcpy(&(name[name_len - 3]), "bi");
    else if (streq(&(name[name_len - 4]), "sman"))
        strcpy(&(name[name_len - 4]), "smen");
    else if (streq(&(name[name_len - 4]), "lman"))
        strcpy(&(name[name_len - 4]), "lmen");
    else if (streq(&(name[name_len - 4]), " man"))
        strcpy(&(name[name_len - 4]), " men");
    else if (streq(&(name[name_len - 2]), "ex"))
        strcpy(&(name[name_len - 2]), "ices");
    else if ((name[name_len - 1] == 'f') && (!streq(&(name[name_len - 2]), "ff")))
        strcpy (&(name[name_len - 1]), "ves");
    else if (((streq(&(name[name_len - 2]), "ch")) || (name[name_len - 1] == 's')) &&
        (!streq(&(name[name_len - 5]), "iarch")))
            strcpy(&(name[name_len]), "es");
    else if (streq(&(name[name_len - 2]), "nx"))
        strcpy(&(name[name_len - 2]), "nxes");
    else
        strcpy(&(name[name_len]), "s");
}


/*
 * Helper function for display_monlist. Prints the number of creatures, followed
 * by either a singular or plural version of the race name as appropriate.
 */
static void get_mon_name(char *output_name, size_t max, const monster_race *r_ptr, int num)
{
    /* Get monster race and name */
    char race_name[NORMAL_WID];

    my_assert(r_ptr);

    my_strcpy(race_name, r_ptr->name, sizeof(race_name));

    /* Unique names don't have a number */
    if (rf_has(r_ptr->flags, RF_UNIQUE))
        my_strcpy(output_name, "[U] ", max);

    /* Normal races */
    else
    {
        my_strcpy(output_name, format("%3d ", num), max);

        /* Make it plural, if needed. */
        if (num > 1) plural_aux(race_name, sizeof(race_name));
    }

    /* Mix the quantity and the header. */
    my_strcat(output_name, race_name, max);
}


/*
 * Monster data for the visible monster list
 */
typedef struct
{
    u16b count;         /* Total number of this type visible */
    u16b asleep;        /* Number asleep (not in LOS) */
    u16b los;           /* Number in LOS */
    u16b los_asleep;    /* Number asleep and in LOS */
    byte attr;          /* "attr" to use for drawing */
} monster_vis;


/*
 * Display visible monsters in a window
 */
void display_monlist(struct player *p, bool do_cmd)
{
    int ii;
    size_t i, j, k;
    unsigned total_count = 0, disp_count = 0, type_count = 0, los_count = 0;
    byte attr;
    char m_name[NORMAL_WID];
    char buf[NORMAL_WID];
    monster_type *m_ptr;
    monster_race *r_ptr;
    monster_race *r2_ptr;
    monster_vis *list;
    u16b *order;
    int cur_line, max_line;

    /* Let the player scroll through this info */
    p->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p);

    /* Hallucination is weird */
    if (p->timed[TMD_IMAGE])
    {
        /* Clear display and print note */
        text_out_c(p, TERM_ORANGE, "Your hallucinations are too wild to see things clearly.");

        /* Restore height and width of current dungeon level */
        text_out_done(p);

        /* Done */
        return;
    }

    /* Allocate the primary array */
    list = C_ZNEW(z_info->r_max, monster_vis);

    /* Scan the list of monsters on the level */
    for (ii = 1; ii < cave_monster_max(cave_get(p->depth)); ii++)
    {
        monster_vis *v;

        m_ptr = cave_monster(cave_get(p->depth), ii);
        r_ptr = &r_info[m_ptr->r_idx];

        /* Only consider visible, known monsters */
        if (!p->mon_vis[ii] || m_ptr->unaware) continue;

        /* Take a pointer to this monster visibility entry */
        v = &list[m_ptr->r_idx];

        /* Note each monster type and save its display attr (color) */
        if (!v->count) type_count++;
        if (!v->attr)
        {
            if (p->tile_distorted) v->attr = r_ptr->d_attr;
            else if (m_ptr->attr) v->attr = m_ptr->attr;
            else v->attr = p->r_attr[m_ptr->r_idx];
        }

        /* Check for LOS using projectable() */
        if (projectable(p->depth, p->py, p->px, m_ptr->fy, m_ptr->fx, PROJECT_NONE) && p->mon_los[ii])
        {
            /* Increment the total number of in-LOS monsters */
            los_count++;

            /* Increment the LOS count for this monster type */
            v->los++;

            /* Check if asleep and increment accordingly */
            if (m_ptr->m_timed[MON_TMD_SLEEP]) v->los_asleep++;
        }

        /* Not in LOS so increment if asleep */
        else if (m_ptr->m_timed[MON_TMD_SLEEP]) v->asleep++;

        /* Bump the count for this race, and the total count */
        v->count++;
        total_count++;
    }

    /* Note no visible monsters at all */
    if (!total_count)
    {
        /* Clear display and print note */
        text_out_c(p, TERM_SLATE, "You see no monsters.");

        /* Free up memory */
        mem_free(list);

        /* Restore height and width of current dungeon level */
        text_out_done(p);

        /* Done */
        return;
    }

    /* Allocate the secondary array */
    order = C_ZNEW(type_count, u16b);

    /* Sort, because we cannot rely on monster.txt being ordered */

    /* Populate the ordered array, starting at 1 to ignore @ */
    for (i = 1; i < z_info->r_max; i++)
    {
        /* No monsters of this race are visible */
        if (!list[i].count) continue;

        /* Get the monster info */
        r_ptr = &r_info[i];

        /* Fit this monster into the sorted array */
        for (j = 0; j < type_count; j++)
        {
            /* If we get to the end of the list, put this one in */
            if (!order[j])
            {
                order[j] = i;
                break;
            }

            /* Get the monster info for comparison */
            r2_ptr = &r_info[order[j]];

            /* Monsters are sorted by depth */
            /* Monsters of same depth are sorted by power */
            if ((r_ptr->level > r2_ptr->level) ||
                ((r_ptr->level == r2_ptr->level) && (r_ptr->power > r2_ptr->power)))
            {
                /* Move weaker monsters down the array */
                for (k = type_count - 1; k > j; k--) order[k] = order[k - 1];

                /* Put current monster in the right place */
                order[j] = i;
                break;
            }
        }
    }

    /* Message for monsters in LOS - even if there are none */
    if (!los_count)
        text_out(p, "You can see no monsters.");
    else
        text_out(p, "You can see %d monster%s:", los_count, PLURAL(los_count));
    text_out(p, "\n\n");
    cur_line = 4;
    max_line = ((los_count == total_count)? p->max_hgt: p->max_hgt - 5);

    /* Print out in-LOS monsters in descending order */
    for (i = 0; i < type_count; i++)
    {
        /* Skip if there are none of these in LOS */
        if (!list[order[i]].los) continue;

        /* Page wrap */
        cur_line++;
        if (!do_cmd && (cur_line == max_line))
        {
            /* Print "and others" message if we've run out of space */
            text_out(p, "... and %d others.\n", total_count - disp_count);
            break;
        }

        /* Note that these have been displayed */
        disp_count += list[order[i]].los;

        /* Get monster race and name */
        r_ptr = &r_info[order[i]];
        get_mon_name(m_name, sizeof(m_name), r_ptr, list[order[i]].los);

        /* Display uniques in a special colour */
        if (rf_has(r_ptr->flags, RF_UNIQUE))
            attr = TERM_VIOLET;
        else if (r_ptr->level > monster_level(p->depth))
            attr = TERM_RED;
        else
            attr = TERM_WHITE;

        /* Build the monster name */
        if (list[order[i]].los == 1)
            strnfmt(buf, sizeof(buf), (list[order[i]].los_asleep? " %s (asleep)": " %s"), m_name);
        else if (list[order[i]].los_asleep)
            strnfmt(buf, sizeof(buf), " %s (%d asleep)", m_name, list[order[i]].los_asleep);
        else
            strnfmt(buf, sizeof(buf), " %s", m_name);

        /* Display the pict */
        if (p->tile_distorted)
            text_out_c(p, list[order[i]].attr, "%c", r_ptr->d_char);
        else
        {
            p->info[p->info_y][p->info_x].a = list[order[i]].attr;
            p->info[p->info_y][p->info_x].c = p->r_char[order[i]];
            p->info_x++;
        }

        /* Print and bump line counter */
        text_out_c(p, attr, buf);
        text_out(p, "\n");
    }

    /* Message for monsters outside LOS, if there are any */
    total_count -= los_count;
    if (total_count)
    {
        /* Leave a blank line */
        if (los_count)
        {
            text_out(p, "\n");
            cur_line++;
        }

        text_out(p, "You are aware of %d monster%s:", total_count, PLURAL(total_count));
        text_out(p, "\n\n");
        cur_line += 2;
    }

    /* Print out non-LOS monsters in descending order */
    for (i = 0; i < type_count; i++)
    {
        u16b out_of_los = list[order[i]].count - list[order[i]].los;

        /* Skip if there are none of these out of LOS */
        if (!out_of_los) continue;

        /* Page wrap */
        cur_line++;
        if (!do_cmd && (cur_line == p->max_hgt))
        {
            /* Print "and others" message if we've run out of space */
            text_out(p, "... and %d others.\n", total_count);
            break;
        }

        /* Note that these have been displayed */
        total_count -= out_of_los;

        /* Get monster race and name */
        r_ptr = &r_info[order[i]];
        get_mon_name(m_name, sizeof(m_name), r_ptr, out_of_los);

        /* Display uniques in a special colour */
        if (rf_has(r_ptr->flags, RF_UNIQUE))
            attr = TERM_VIOLET;
        else if (r_ptr->level > monster_level(p->depth))
            attr = TERM_RED;
        else
            attr = TERM_WHITE;

        /* Build the monster name */
        if (out_of_los == 1)
            strnfmt(buf, sizeof(buf), (list[order[i]].asleep? " %s (asleep)": " %s"), m_name);
        else if (list[order[i]].asleep)
            strnfmt(buf, sizeof(buf), " %s (%d asleep)", m_name, list[order[i]].asleep);
        else
            strnfmt(buf, sizeof(buf), " %s", m_name);

        /* Display the pict */
        if (p->tile_distorted)
            text_out_c(p, list[order[i]].attr, "%c", r_ptr->d_char);
        else
        {
            p->info[p->info_y][p->info_x].a = list[order[i]].attr;
            p->info[p->info_y][p->info_x].c = p->r_char[order[i]];
            p->info_x++;
        }

        /* Print and bump line counter */
        text_out_c(p, attr, buf);
        text_out(p, "\n");
    }

    /* Free the arrays */
    mem_free(list);
    mem_free(order);

    /* Restore height and width of current dungeon level */
    text_out_done(p);
}


void player_desc(struct player *p, char *desc, size_t max, struct player *q, bool capitalize)
{
    int who = get_player_index(get_connection(q->conn));

    if (p->play_vis[who])
        my_strcpy(desc, q->name, max);
    else
        my_strcpy(desc, "someone", max);
    if (capitalize) my_strcap(desc);
}


/*
 * Builds a string describing a monster in some way.
 *
 * We can correctly describe monsters based on their visibility.
 * We can force all monsters to be treated as visible or invisible.
 * We can build nominatives, objectives, possessives, or reflexives.
 * We can selectively pronominalize hidden, visible, or all monsters.
 * We can use definite or indefinite descriptions for hidden monsters.
 * We can use definite or indefinite descriptions for visible monsters.
 *
 * Pronominalization involves the gender whenever possible and allowed,
 * so that by cleverly requesting pronominalization / visibility, you
 * can get messages like "You hit someone.  She screams in agony!".
 *
 * Reflexives are acquired by requesting Objective plus Possessive.
 *
 * I am assuming that no monster name is more than 65 characters long,
 * so that "char desc[NORMAL_WID];" is sufficiently large for any result, even
 * when the "offscreen" notation is added.
 *
 * Note that the "possessive" for certain unique monsters will look
 * really silly, as in "Morgoth, King of Darkness's".  We should
 * perhaps add a flag to "remove" any "descriptives" in the name.
 *
 * Note that "offscreen" monsters will get a special "(offscreen)"
 * notation in their name if they are visible but offscreen.  This
 * may look silly with possessives, as in "the rat's (offscreen)".
 * Perhaps the "offscreen" descriptor should be abbreviated.
 *
 * Mode Flags:
 *   0x01 --> Objective (or Reflexive)
 *   0x02 --> Possessive (or Reflexive)
 *   0x04 --> Use indefinites for hidden monsters ("something")
 *   0x08 --> Use indefinites for visible monsters ("a kobold")
 *   0x10 --> Pronominalize hidden monsters
 *   0x20 --> Pronominalize visible monsters
 *   0x40 --> Assume the monster is hidden
 *   0x80 --> Assume the monster is visible
 *  0x100 --> Capitalise monster name
 *
 * Useful Modes:
 *   0x00 --> Full nominative name ("the kobold") or "it"
 *   0x04 --> Full nominative name ("the kobold") or "something"
 *   0x80 --> Banishment resistance name ("the kobold")
 *   0x88 --> Killing name ("a kobold")
 *   0x22 --> Possessive, genderized if visible ("his") or "its"
 *   0x23 --> Reflexive, genderized if visible ("himself") or "itself"
 */
void monster_desc(struct player *p, char *desc, size_t max, const monster_type *m_ptr, int mode)
{
    const char *res;
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    const char *name = r_ptr->name;
    bool seen, pron;
    bool offscreen;

    /* Can we "see" it (forced, or not hidden + visible) */
    if (p)
    {
        seen = ((mode & 0x80) || (!(mode & 0x40) && p->mon_vis[m_ptr->midx]));
        offscreen = !panel_contains(p, m_ptr->fy, m_ptr->fx);
    }
    else
    {
        seen = ((mode & 0x80) || !(mode & 0x40));
        offscreen = FALSE;
    }

    /* Sexed Pronouns (seen and allowed, or unseen and allowed) */
    pron = ((seen && (mode & 0x20)) || (!seen && (mode & 0x10)));

    /* First, try using pronouns, or describing hidden monsters */
    if (!seen || pron)
    {
        /* An encoding of the monster "sex" */
        int kind = 0x00;

        /* Extract the gender (if applicable) */
        if (rf_has(r_ptr->flags, RF_FEMALE)) kind = 0x20;
        else if (rf_has(r_ptr->flags, RF_MALE)) kind = 0x10;

        /* Ignore the gender (if desired) */
        if (!m_ptr->r_idx || !pron) kind = 0x00;

        /* Assume simple result */
        res = "it";

        /* Brute force: split on the possibilities */
        switch (kind + (mode & 0x07))
        {
            /* Neuter, or unknown */
            case 0x00: res = "it"; break;
            case 0x01: res = "it"; break;
            case 0x02: res = "its"; break;
            case 0x03: res = "itself"; break;
            case 0x04: res = "something"; break;
            case 0x05: res = "something"; break;
            case 0x06: res = "something's"; break;
            case 0x07: res = "itself"; break;

            /* Male (assume human if vague) */
            case 0x10: res = "he"; break;
            case 0x11: res = "him"; break;
            case 0x12: res = "his"; break;
            case 0x13: res = "himself"; break;
            case 0x14: res = "someone"; break;
            case 0x15: res = "someone"; break;
            case 0x16: res = "someone's"; break;
            case 0x17: res = "himself"; break;

            /* Female (assume human if vague) */
            case 0x20: res = "she"; break;
            case 0x21: res = "her"; break;
            case 0x22: res = "her"; break;
            case 0x23: res = "herself"; break;
            case 0x24: res = "someone"; break;
            case 0x25: res = "someone"; break;
            case 0x26: res = "someone's"; break;
            case 0x27: res = "herself"; break;
        }

        /* Copy the result */
        my_strcpy(desc, res, max);
    }

    /* Handle visible monsters, "reflexive" request */
    else if ((mode & 0x02) && (mode & 0x01))
    {
        /* The monster is visible, so use its gender */
        if (rf_has(r_ptr->flags, RF_FEMALE)) my_strcpy(desc, "herself", max);
        else if (rf_has(r_ptr->flags, RF_MALE)) my_strcpy(desc, "himself", max);
        else my_strcpy(desc, "itself", max);
    }

    /* Handle all other visible monster requests */
    else
    {
        /* It could be a Unique */
        if (rf_has(r_ptr->flags, RF_UNIQUE))
        {
            /* Start with the name (thus nominative and objective) */
            my_strcpy(desc, name, max);
        }

        /* It could be an indefinite monster */
        else if (mode & 0x08)
        {
            /* XXX Check plurality for "some" */

            /* Indefinite monsters need an indefinite article */
            my_strcpy(desc, is_a_vowel(name[0])? "an ": "a ", max);
            my_strcat(desc, name, max);
        }

        /* It could be a normal, definite, monster */
        else
        {
            /* Definite monsters need a definite article */
            my_strcpy(desc, "the ", max);
            my_strcat(desc, name, max);
        }

        /* Handle the Possessive as a special afterthought */
        if (mode & 0x02)
        {
            /* XXX Check for trailing "s" */

            /* Simply append "apostrophe" and "s" */
            my_strcat(desc, "'s", max);
        }

        /* Mention "offscreen" monsters XXX XXX */
        if (offscreen)
        {
            /* Append special notation */
            my_strcat(desc, " (offscreen)", max);
        }
    }

    if (mode & MDESC_CAPITAL) my_strcap(desc);
}


static bool is_detected_m(int Ind, const bitflag mflags[RF_SIZE], int d_esp)
{
    player_type *p_ptr = player_get(Ind);

    /* Full ESP */
    if (check_state(p_ptr, OF_ESP_ALL)) return TRUE;

    /* Partial ESP */
    if (rf_has(mflags, RF_ORC) && check_state(p_ptr, OF_ESP_ORC)) return TRUE;
    if (rf_has(mflags, RF_TROLL) && check_state(p_ptr, OF_ESP_TROLL)) return TRUE;
    if (rf_has(mflags, RF_GIANT) && check_state(p_ptr, OF_ESP_GIANT)) return TRUE;
    if (rf_has(mflags, RF_DRAGON) && check_state(p_ptr, OF_ESP_DRAGON)) return TRUE;
    if (rf_has(mflags, RF_DEMON) && check_state(p_ptr, OF_ESP_DEMON)) return TRUE;
    if (rf_has(mflags, RF_UNDEAD) && check_state(p_ptr, OF_ESP_UNDEAD)) return TRUE;
    if (rf_has(mflags, RF_EVIL) && check_state(p_ptr, OF_ESP_EVIL)) return TRUE;
    if (rf_has(mflags, RF_ANIMAL) && check_state(p_ptr, OF_ESP_ANIMAL)) return TRUE;

    /* Radius ESP */
    if (check_state(p_ptr, OF_ESP_RADIUS)) return (d_esp <= MAX_SIGHT);

    /* No ESP */
    return FALSE;
}


/*
 * Returns TRUE if the given monster is currently mimicking a squelched item.
 */
static bool is_mimicking_squelched(struct player *p, int m_idx)
{
    const monster_type *m_ptr;
    object_type *o_ptr;

    my_assert(m_idx > 0);
    m_ptr = cave_monster(cave_get(p->depth), m_idx);

    if (!(m_ptr->unaware && m_ptr->mimicked_o_idx)) return FALSE;

    o_ptr = object_byid(m_ptr->mimicked_o_idx);
    return squelch_item_ok(p, o_ptr);
}


/*
 * This function updates the monster record of the given monster
 *
 * This involves extracting the distance to the player (if requested),
 * and then checking for visibility (natural, infravision, see-invis,
 * telepathy), updating the monster visibility flag, redrawing (or
 * erasing) the monster when its visibility changes, and taking note
 * of any interesting monster flags (cold-blooded, invisible, etc).
 *
 * Note the new "mflag" field which encodes several monster state flags,
 * including "view" for when the monster is currently in line of sight,
 * and "mark" for when the monster is currently visible via detection.
 *
 * The only monster fields that are changed here are "cdis" (the
 * distance from the player), "ml" (visible to the player), and
 * "mflag" (to maintain the "MFLAG_VIEW" flag).
 *
 * Note the special "update_monsters()" function which can be used to
 * call this function once for every monster.
 *
 * Note the "full" flag which requests that the "cdis" field be updated;
 * this is only needed when the monster (or the player) has moved.
 *
 * Every time a monster moves, we must call this function for that
 * monster, and update the distance, and the visibility.  Every time
 * the player moves, we must call this function for every monster, and
 * update the distance, and the visibility.  Whenever the player "state"
 * changes in certain ways ("blindness", "infravision", "telepathy",
 * and "see invisible"), we must call this function for every monster,
 * and update the visibility.
 *
 * Routines that change the "illumination" of a grid must also call this
 * function for any monster in that grid, since the "visibility" of some
 * monsters may be based on the illumination of their grid.
 *
 * Note that this function is called once per monster every time the
 * player moves.  When the player is running, this function is one
 * of the primary bottlenecks, along with "update_view()" and the
 * "process_monsters()" code, so efficiency is important.
 *
 * Note the optimized "inline" version of the "distance()" function.
 *
 * A monster is "visible" to the player if (1) it has been detected
 * by the player, (2) it is close to the player and the player has
 * telepathy, or (3) it is close to the player, and in line of sight
 * of the player, and it is "illuminated" by some combination of
 * infravision, torch light, or permanent light (invisible monsters
 * are only affected by "light" if the player can see invisible).
 *
 * Monsters which are not on the current panel may be "visible" to
 * the player, and their descriptions will include an "offscreen"
 * reference.  Currently, offscreen monsters cannot be targeted
 * or viewed directly, but old targets will remain set.  XXX XXX
 *
 * The player can choose to be disturbed by several things, including
 * "disturb_move" (monster which is viewable moves in some way), and
 * "disturb_near" (monster which is "easily" viewable moves in some
 * way).  Note that "moves" includes "appears" and "disappears".
 */
void update_mon(int depth, int m_idx, bool full)
{
    monster_type *m_ptr;
    monster_race *r_ptr;
    int Ind;
    int closest = 0, dis_to_closest = 9999, lowhp = 9999;
    bool blos = FALSE, new_los;

    /* Current location */
    int fy;
    int fx;

    my_assert(m_idx > 0);
    m_ptr = cave_monster(cave_get(depth), m_idx);
    r_ptr = &r_info[m_ptr->r_idx];
    fy = m_ptr->fy;
    fx = m_ptr->fx;

    /* Check for each player */
    for (Ind = 1; Ind < NumPlayers + 1; Ind++)
    {
        player_type *p_ptr = player_get(Ind);
        monster_lore *l_ptr = &p_ptr->lore[m_ptr->r_idx];
        int d, d_esp;
        int py = p_ptr->py;
        int px = p_ptr->px;

        /* Seen at all */
        bool flag = FALSE;

        /* Seen by vision */
        bool easy = FALSE;

        /* Basic telepathy */
        bool basic = FALSE;

        /* Make sure he's on the same dungeon level */
        if (p_ptr->depth != depth) continue;

        /* Compute distance */
        d = distance(py, px, fy, fx);
        d_esp = distance(py, px / 3, fy, fx / 3);

        /* Restrict distance */
        if (d > 255) d = 255;
        if (d_esp > 255) d_esp = 255;

        /* Find the closest player */
        if (full)
        {
            /* Hack -- Skip him if he's shopping */
            /* Hack -- Make the dungeon master invisible to monsters */
            /* Skip player if dead or gone */
            if (!in_store(p_ptr) && !(p_ptr->dm_flags & DM_MONSTER_FRIEND) &&
                p_ptr->alive && !p_ptr->is_dead && !p_ptr->new_level_flag)
            {
                /* Check if monster has LOS to the player */
                new_los = los(p_ptr->depth, fy, fx, py, px);

                /* Remember this player if closest */
                if (is_closest(Ind, m_ptr, blos, new_los, d, dis_to_closest, lowhp))
                {
                    blos = new_los;
                    dis_to_closest = d;
                    closest = Ind;
                    lowhp = p_ptr->chp;
                }
            }
        }

        /* Detected */
        if (p_ptr->mon_det[m_idx]) flag = TRUE;

        /* Nearby */
        else if (panel_contains(p_ptr, fy, fx))
        {
            bool isDM = ((p_ptr->dm_flags & DM_SEE_MONSTERS)? TRUE: FALSE);
            bool hasESP = is_detected_m(Ind, r_ptr->flags, d_esp);
            bool isTL = (player_has(p_ptr, PF_THUNDERLORD) &&
                (d_esp <= (p_ptr->lev * MAX_SIGHT / PY_MAX_LEVEL)));

            /* Basic telepathy */
            if (isDM || hasESP || isTL)
            {
                basic = TRUE;

                /* Empty mind, no telepathy */
                if (rf_has(r_ptr->flags, RF_EMPTY_MIND))
                {
                    /* Nothing! */
                }

                /* Weird mind, occasional telepathy */
                else if (rf_has(r_ptr->flags, RF_WEIRD_MIND))
                {
                    /* One in ten individuals are detectable */
                    if ((m_idx % 10) == 5)
                    {
                        /* Detectable */
                        flag = TRUE;

                        /* Check for LOS so that mon_los is set later */
                        if (player_has_los_bold(p_ptr, fy, fx)) easy = TRUE;
                    }
                }

                /* Normal mind, allow telepathy */
                else
                {
                    /* Detectable */
                    flag = TRUE;

                    /* Check for LOS so that mon_los is set later */
                    if (player_has_los_bold(p_ptr, fy, fx)) easy = TRUE;
                }

                /* DM has perfect ESP */
                if (isDM)
                {
                    /* Detectable */
                    flag = TRUE;

                    /* Check for LOS so that mon_los is set later */
                    if (player_has_los_bold(p_ptr, fy, fx)) easy = TRUE;
                }
            }

            /* Normal line of sight and player is not blind */
            if (player_has_los_bold(p_ptr, fy, fx) && !p_ptr->timed[TMD_BLIND])
            {
                /* Use "infravision" */
                if (d <= p_ptr->state.see_infra)
                {
                    /* Learn about warm/cold blood */
                    rf_on(l_ptr->flags, RF_COLD_BLOOD);

                    /* Handle "warm blooded" monsters */
                    if (!rf_has(r_ptr->flags, RF_COLD_BLOOD))
                    {
                        /* Easy to see */
                        easy = flag = TRUE;
                    }
                }

                /* Use "illumination" */
                if (player_can_see_bold(p_ptr, fy, fx))
                {
                    /* Learn it emits light */
                    rf_on(l_ptr->flags, RF_HAS_LIGHT);

                    /* Learn about invisibility */
                    rf_on(l_ptr->flags, RF_INVISIBLE);

                    /* Handle "invisible" monsters */
                    if (rf_has(r_ptr->flags, RF_INVISIBLE))
                    {
                        /* See invisible */
                        if (check_state(p_ptr, OF_SEE_INVIS))
                        {
                            /* Easy to see */
                            easy = flag = TRUE;
                        }
                    }

                    /* Handle "normal" monsters */
                    else
                    {
                        /* Easy to see */
                        easy = flag = TRUE;
                    }
                }
            }

            /* Controlled monsters are always visible */
            if (master_in_party(m_ptr->master, p_ptr->id)) easy = flag = TRUE;
        }

        /* If a mimic looks like a squelched item, it's not seen */
        if (is_mimicking_squelched(p_ptr, m_idx))
            easy = flag = FALSE;

        /* The monster is now visible */
        if (flag)
        {
            /* Learn about the monster's mind */
            if (basic)
            {
                flags_set(l_ptr->flags, RF_SIZE, RF_EMPTY_MIND, RF_WEIRD_MIND, RF_SMART, RF_STUPID,
                    FLAG_END);
            }

            /* It was previously unseen */
            if (!p_ptr->mon_vis[m_idx])
            {
                /* Mark as visible */
                p_ptr->mon_vis[m_idx] = TRUE;

                /* Draw the monster */
                cave_light_spot_aux(p_ptr, cave_get(depth), fy, fx);

                /* Update health bar as needed */
                update_health(m_idx);

                /* Hack -- Count "fresh" sightings */
                r_ptr->lore.seen = 1;
                l_ptr->pseen = 1;

                /* Disturb on appearance (except townies, friendlies and unaware mimics) */
                if (OPT_P(p_ptr, disturb_move) && (m_ptr->level > 0) && pvm_check(Ind, m_idx) &&
                    !is_mimicking(m_ptr))
                {
                    disturb(p_ptr, 1, 0);
                }

                /* Redraw */
                p_ptr->redraw |= (PR_MONLIST);
            }

            /* Efficiency -- Notice multi-hued monsters */
            if (monster_shimmer(r_ptr) && allow_shimmer(p_ptr))
                cave_get(depth)->scan_monsters = TRUE;
        }

        /* The monster is not visible */
        else
        {
            /* It was previously seen */
            if (p_ptr->mon_vis[m_idx])
            {
                /* Treat mimics differently */
                if (!m_ptr->mimicked_o_idx ||
                    squelch_item_ok(p_ptr, object_byid(m_ptr->mimicked_o_idx)))
                {
                    /* Mark as not visible */
                    p_ptr->mon_vis[m_idx] = FALSE;

                    /* Erase the monster */
                    cave_light_spot_aux(p_ptr, cave_get(depth), fy, fx);

                    /* Update health bar as needed */
                    update_health(m_idx);

                    /* Redraw */
                    p_ptr->redraw |= (PR_MONLIST);
                }
            }
        }

        /* The monster is now easily visible */
        if (easy)
        {
            /* Change */
            if (!p_ptr->mon_los[m_idx])
            {
                /* Mark as easily visible */
                p_ptr->mon_los[m_idx] = TRUE;

                /* Disturb on appearance (except townies, friendlies and unaware mimics) */
                if (OPT_P(p_ptr, disturb_near) && (m_ptr->level > 0) && pvm_check(Ind, m_idx) &&
                    !is_mimicking(m_ptr))
                {
                    disturb(p_ptr, 1, 0);
                }

                /* Redraw */
                p_ptr->redraw |= (PR_MONLIST);
            }
        }

        /* The monster is not easily visible */
        else
        {
            /* Change */
            if (p_ptr->mon_los[m_idx])
            {
                /* Mark as not easily visible */
                p_ptr->mon_los[m_idx] = FALSE;

                /* Redraw */
                p_ptr->redraw |= (PR_MONLIST);
            }
        }
    }

    /* Track closest player */
    if (full)
    {
        /* Forget player status */
        if (closest != m_ptr->closest_player) m_ptr->smart = 0L;

        /* Always track closest player */
        m_ptr->closest_player = closest;

        /* Paranoia -- Make sure we found a closest player */
        if (closest) m_ptr->cdis = dis_to_closest;
    }

    /* Update the cursor */
    update_cursor(m_idx);
}


/*
 * Updates all the (non-dead) monsters via update_mon().
 */
void update_monsters(int depth, bool full)
{
    int i;

    /* Efficiency -- Clear multi-hued flag */
    cave_get(depth)->scan_monsters = FALSE;

    /* Update each (live) monster */
    for (i = 1; i < cave_monster_max(cave_get(depth)); i++)
    {
        monster_type *m_ptr = cave_monster(cave_get(depth), i);

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Update the monster */
        update_mon(depth, i, full);
    }
}


/*  
 * See if a monster can carry an object (it will pick up either way)
 */
static bool monster_can_carry(struct monster *m, object_type *j_ptr, bool force)
{
    int total_number = 0;
    s16b this_o_idx, next_o_idx = 0;
    monster_race *r_ptr = &r_info[m->r_idx];

    /* Always carry artifacts */
    if (j_ptr->artifact) return TRUE;

    /* Clones don't carry stuff */
    if (m->clone) return FALSE;

    /* Force to carry monster drops */
    if (force) return TRUE;

    /* Only carry stuff in the dungeon */
    if (m->depth <= 0) return FALSE;

#if !defined(MAX_MONSTER_BAG)
    return TRUE;
#else
    /* Scan objects already being held for combination */
    for (this_o_idx = m->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        /* Get the next object */
        next_o_idx = object_byid(this_o_idx)->next_o_idx;

        /* Count it */
        total_number++;
    }

    /*
     * Chance-based response. The closer monster to his limit, the less the chance is.
     * If he reached the limit, he will not pick up
     * XXX XXX XXX -- double chance && strict limit
     */
    if ((randint0(MAX_MONSTER_BAG) * 2 > total_number) && (total_number < MAX_MONSTER_BAG))
        return TRUE;

    return FALSE;
#endif
}


/*
 * Add the given object to the given monster's inventory.
 *
 * Returns the o_idx of the new object, or 0 if the object is
 * not successfully added.
 */
s16b monster_carry(struct monster *m, object_type *j_ptr, bool force)
{
    s16b o_idx;
    s16b this_o_idx, next_o_idx = 0;

    /* See if the monster can carry the object */
    if (!monster_can_carry(m, j_ptr, force)) return 0;

    /* Scan objects already being held for combination */
    for (this_o_idx = m->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = object_byid(this_o_idx);

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Check for combination */
        if (object_similar(o_ptr, j_ptr, OSTACK_MONSTER))
        {
            /* Combine the items */
            object_absorb(o_ptr, j_ptr);

            /* Result */
            return (this_o_idx);
        }
    }

    /* Make an object */
    o_idx = o_pop();

    /* Success */
    if (o_idx)
    {
        object_type *o_ptr;

        /* Get new object */
        o_ptr = object_byid(o_idx);

        /* Copy object */
        object_copy(o_ptr, j_ptr);

        /* Forget location */
        o_ptr->iy = o_ptr->ix = 0;

        /* Link the object to the monster */
        o_ptr->held_m_idx = m->midx;
        o_ptr->depth = m->depth;

        /* Link the object to the pile */
        o_ptr->next_o_idx = m->hold_o_idx;

        /* Link the monster to the object */
        m->hold_o_idx = o_idx;
    }

    /* Result */
    return (o_idx);
}


/*
 * Swap the players/monsters (if any) at two locations.
 */
void monster_swap(int depth, int y1, int x1, int y2, int x2)
{
    player_type *p_ptr;
    int m1, m2;
    monster_type *m_ptr;
    monster_race *r_ptr;

    /* Monsters */
    m1 = cave_get(depth)->m_idx[y1][x1];
    m2 = cave_get(depth)->m_idx[y2][x2];

    /* Update grids */
    cave_get(depth)->m_idx[y1][x1] = m2;
    cave_get(depth)->m_idx[y2][x2] = m1;

    /* Monster 1 */
    if (m1 > 0)
    {
        m_ptr = cave_monster(cave_get(depth), m1);

        /* Move monster */
        m_ptr->fy = y2;
        m_ptr->fx = x2;

        /* Update monster */
        update_mon(depth, m1, TRUE);

        /* Radiate light? */
        r_ptr = &r_info[m_ptr->r_idx];
        if (rf_has(r_ptr->flags, RF_HAS_LIGHT))
        {
            int i;

            for (i = 1; i < NumPlayers + 1; i++)
            {
                player_type *q_ptr = player_get(i);

                if (q_ptr->depth == depth) q_ptr->update |= PU_UPDATE_VIEW;
            }
        }
    }

    /* Player 1 */
    else if (m1 < 0)
    {
        p_ptr = player_get(0 - m1);

        /* Hack -- Save previous player location */
        p_ptr->old_py = p_ptr->py;
        p_ptr->old_px = p_ptr->px;

        /* Move player */
        p_ptr->py = y2;
        p_ptr->px = x2;

        /* Update the trap detection status */
        p_ptr->redraw |= (PR_DTRAP);

        /* Redraw */
        p_ptr->redraw |= (PR_FLOOR | PR_ITEMLIST);

        /* Update the panel */
        verify_panel(p_ptr);

        /* Update the visuals (and monster distances) */
        p_ptr->update |= (PU_UPDATE_VIEW | PU_DISTANCE);

        /* Update the flow */
        p_ptr->update |= (PU_UPDATE_FLOW);

        /* Radiate light? */
        if (p_ptr->cur_light)
        {
            int i;

            for (i = 1; i < NumPlayers + 1; i++)
            {
                player_type *q_ptr = player_get(i);

                if ((q_ptr->depth == depth) && (i != 0 - m1))
                    q_ptr->update |= PU_UPDATE_VIEW;
            }
        }
    }

    /* Monster 2 */
    if (m2 > 0)
    {
        m_ptr = cave_monster(cave_get(depth), m2);

        /* Move monster */
        m_ptr->fy = y1;
        m_ptr->fx = x1;

        /* Update monster */
        update_mon(depth, m2, TRUE);

        /* Radiate light? */
        r_ptr = &r_info[m_ptr->r_idx];
        if (rf_has(r_ptr->flags, RF_HAS_LIGHT))
        {
            int i;

            for (i = 1; i < NumPlayers + 1; i++)
            {
                player_type *q_ptr = player_get(i);

                if (q_ptr->depth == depth) q_ptr->update |= PU_UPDATE_VIEW;
            }
        }
    }

    /* Player 2 */
    else if (m2 < 0)
    {
        p_ptr = player_get(0 - m2);

        /* Hack -- Save previous player location */
        p_ptr->old_py = p_ptr->py;
        p_ptr->old_px = p_ptr->px;

        /* Move player */
        p_ptr->py = y1;
        p_ptr->px = x1;

        /* Update the trap detection status */
        p_ptr->redraw |= (PR_DTRAP);

        /* Redraw */
        p_ptr->redraw |= (PR_FLOOR | PR_ITEMLIST);

        /* Update the panel */
        verify_panel(p_ptr);

        /* Update the visuals (and monster distances) */
        p_ptr->update |= (PU_UPDATE_VIEW | PU_DISTANCE);

        /* Update the flow */
        p_ptr->update |= (PU_UPDATE_FLOW);

        /* Radiate light? */
        if (p_ptr->cur_light)
        {
            int i;

            for (i = 1; i < NumPlayers + 1; i++)
            {
                player_type *q_ptr = player_get(i);

                if ((q_ptr->depth == depth) && (i != 0 - m2))
                    q_ptr->update |= PU_UPDATE_VIEW;
            }
        }
    }

    /* Redraw */
    cave_light_spot(cave_get(depth), y1, x1);
    cave_light_spot(cave_get(depth), y2, x2);
}


/*
 * Hack -- The "type" of the current "summon specific"
 */
static int summon_specific_type = 0;


/*
 * Hack -- Help decide if a monster race is "okay" to summon.
 *
 * Compares the given monster to the monster type specified by
 * summon_specific_type. Returns TRUE if the monster is eligible to
 * be summoned, FALSE otherwise.
 */
static bool summon_specific_okay(int r_idx)
{
    const monster_race *r_ptr;
    const bitflag *flags;
    const struct monster_base *base;
    bool unique, scary;

    my_assert(r_idx > 0);
    r_ptr = &r_info[r_idx];
    flags = r_ptr->flags;
    base = r_ptr->base;
    unique = rf_has(r_ptr->flags, RF_UNIQUE);
    scary = flags_test(r_ptr->flags, RF_SIZE, RF_UNIQUE, RF_FRIEND, RF_FRIENDS, RF_ESCORT,
        RF_ESCORTS, FLAG_END);

    /* Check our requirements */
    switch (summon_specific_type)
    {
        case S_ANIMAL: return (!unique && rf_has(flags, RF_ANIMAL));
        case S_SPIDER: return (!unique && match_monster_bases(base, "spider", NULL));
        case S_HOUND:
            return (!unique && match_monster_bases(base, "canine", "zephyr hound", NULL));
        case S_HYDRA: return (!unique && match_monster_bases(base, "hydra", NULL));
        case S_AINU: return (!scary && match_monster_bases(base, "ainu", NULL));
        case S_DEMON: return (!scary && rf_has(flags, RF_DEMON));
        case S_UNDEAD: return (!scary && rf_has(flags, RF_UNDEAD));
        case S_DRAGON: return (!scary && rf_has(flags, RF_DRAGON));
        case S_KIN: return (!unique && (r_ptr->d_char == summon_kin_type));
        case S_HI_UNDEAD:
            return match_monster_bases(base, "lich", "vampire", "wraith", NULL);
        case S_HI_DRAGON: return match_monster_bases(base, "ancient dragon", NULL);
        case S_HI_DEMON: return match_monster_bases(base, "major demon", NULL);
        case S_WRAITH: return (unique && match_monster_bases(base, "wraith", NULL));
        case S_UNIQUE: return unique;
        case S_MONSTER: return !scary;
        case S_MONSTERS: return !unique;
        case S_JELLY: return (!unique && match_monster_bases(base, "jelly", "mold", NULL));
        case S_GOLEM: return (!unique && match_monster_bases(base, "golem", NULL));
        case S_VORTEX: return (!unique && match_monster_bases(base, "vortex", NULL));

        default: return TRUE;
    }
}


/*
 * Places a monster (of the specified "type") near the given
 * location.  Return TRUE if a monster was actually summoned.
 *
 * We will attempt to place the monster up to 60 times before giving up.
 *
 * Note: S_UNIQUE and S_WRAITH (XXX) will summon Uniques
 * Note: S_HI_UNDEAD/S_HI_DRAGON/S_HI_DEMON may summon Uniques
 * Note: None of the other summon codes will ever summon Uniques.
 *
 * This function has been changed.  We now take the "monster level"
 * of the summoning monster as a parameter, and use that, along with
 * the current dungeon level, to help determine the level of the
 * desired monster.  Note that this is an upper bound, and also
 * tends to "prefer" monsters of that level.  Currently, we use
 * the average of the dungeon and monster levels, and then add
 * five to allow slight increases in monster power.
 *
 * Note that we use the new "monster allocation table" creation code
 * to restrict the "get_mon_num()" function to the set of "legal"
 * monsters, making this function much faster and more reliable.
 *
 * Note that this function may not succeed, though this is very rare.
 */
int summon_specific(struct player *p, int y1, int x1, int lev, int type, int delay, int chance)
{
    int i, x = 0, y = 0, r_idx;
    monster_type *m_ptr;
    monster_race *r_ptr;
    int m_idx;
    byte status = MSTATUS_HOSTILE, status_player = MSTATUS_SUMMONED;
    int summon_level = (monster_level(p->depth) + monster_level(lev)) / 2 + 5;

    /* Forbid in the town or on special levels */
    if (forbid_special(p->depth)) return 0;

    /* Paranoia, make sure the level is allocated */
    if (!cave_get(p->depth)) return 0;

    /* Look for a location, allow up to 4 squares away */
    for (i = 0; i < 60; ++i)
    {
        /* Pick a distance */
        int d = (i / 15) + 1;

        /* Pick a location */
        scatter(p->depth, &y, &x, y1, x1, d, FALSE);

        /* Require "empty" floor grid */
        if (!cave_empty_bold(p->depth, y, x)) continue;

        /* No summon on glyph of warding */
        if (cave_get(p->depth)->feat[y][x] == FEAT_GLYPH) continue;

        /* Okay */
        break;
    }

    /* Failure */
    if (i == 60) return 0;

    /* Hack -- Monster summoned by the player */
    if (chance) status = MSTATUS_SUMMONED;

    /* Hack -- Try to "charm" the monster (friendly summon) */
    if (magik(chance))
    {
        /* A high level will help a lot */
        if (!CHANCE(MAX(summon_level - 5, 1), p->lev * 5)) status_player = 1 + randint1(2);

        /* A high charisma will help a lot, and will always yield useful summons */
        if (can_charm_monster(p)) status_player = MSTATUS_ATTACK;
    }

    /* Save the "summon" type */
    summon_specific_type = type;

    /* Require "okay" monsters */
    get_mon_num_hook = summon_specific_okay;

    /* Prepare allocation table */
    get_mon_num_prep();

    /* Try to get a proper summon */
    while (1)
    {
        /* Pick a monster, using the level calculation */
        r_idx = get_mon_num(p->depth, summon_level);

        /* Handle failure */
        if (!r_idx) break;

        /* Too enraged to be controlled */
        if (check_state(p, OF_AGGRAVATE))
        {
            status_player = MSTATUS_SUMMONED;
            break;
        }

        /* Uniques and breeders cannot be tamed */
        r_ptr = &r_info[r_idx];
        if ((status_player > MSTATUS_SUMMONED) &&
            (rf_has(r_ptr->flags, RF_UNIQUE) || rf_has(r_ptr->flags, RF_MULTIPLY)))
        {
            continue;
        }

        /* Useful summons should be "useful" (except specific summons) */
        if ((status_player == MSTATUS_ATTACK) && (type != S_JELLY) && (type != S_GOLEM) &&
            (type != S_VORTEX) && (type != S_HYDRA) &&
            (rf_has(r_ptr->flags, RF_NEVER_BLOW) || rf_has(r_ptr->flags, RF_NEVER_MOVE) ||
            rf_has(r_ptr->flags, RF_RAND_25) || rf_has(r_ptr->flags, RF_RAND_50)))
        {
            continue;
        }

        /* Done */
        break;
    }

    /* Remove restriction */
    get_mon_num_hook = NULL;

    /* Prepare allocation table */
    get_mon_num_prep();

    /* Handle failure */
    if (!r_idx) return 0;

    /* Attempt to place the monster (awake, don't allow groups) */
    if (!place_new_monster(p, cave_get(p->depth), y, x, r_idx, 0, ORIGIN_DROP_SUMMON))
        return 0;

    /*
     * If delay, try to let the player act before the summoned monsters,
     * including slowing down faster monsters for one turn
     */
    m_idx = cave_get(p->depth)->m_idx[y][x];
    m_ptr = cave_monster(cave_get(p->depth), m_idx);
    if (delay)
    {
        m_ptr->energy = 0;
        if (m_ptr->mspeed > p->state.speed)
            mon_inc_timed(p, m_ptr, MON_TMD_SLOW, 1, MON_TMD_FLG_NOMESSAGE, FALSE);
    }

    /* Hack -- Monster summoned by the player */
    if (status_player > MSTATUS_SUMMONED) monster_set_master(m_ptr, p, status_player);
    else m_ptr->status = status;

    /* Success, return the level of the monster */
    /* Monsters that normally come with FRIENDS are weaker */
    r_ptr = &r_info[m_ptr->r_idx];
    if (rf_has(r_ptr->flags, RF_FRIENDS)) return r_ptr->level / 5;
    return r_ptr->level;
}


/*
 * Lets the given monster attempt to reproduce.
 *
 * Note that "reproduction" REQUIRES empty space.
 *
 * Returns TRUE if the monster successfully reproduced.
 */
bool clone_mon(int Ind, int depth, int m_idx)
{
    player_type *p_ptr = player_get(Ind);
    const monster_type *m_ptr;
    const monster_race *r_ptr;
    int i, y, x;
    bool result = FALSE;

    my_assert(m_idx > 0);
    m_ptr = cave_monster(cave_get(depth), m_idx);
    r_ptr = &r_info[m_ptr->r_idx];

    /* Only on random levels */
    if (!random_level(depth)) return FALSE;

    /* No uniques */
    if (rf_has(r_ptr->flags, RF_UNIQUE)) return FALSE;

    /* Limit number of clones */
    if (cave_get(depth)->num_clones == MAX_REPRO) return FALSE;

    /* Try up to 18 times */
    for (i = 0; i < 18; i++)
    {
        int d = 1;

        /* Pick a location */
        scatter(depth, &y, &x, m_ptr->fy, m_ptr->fx, d, FALSE);

        /* Require an "empty" floor grid */
        if (!cave_empty_bold(depth, y, x)) continue;

        /* Create a new monster (awake, no groups) */
        result = place_new_monster(p_ptr, cave_get(depth), y, x, m_ptr->r_idx, MON_CLONE,
            ORIGIN_DROP_BREED);

        /* Done */
        break;
    }

    /* Result */
    return (result);
}


/*
 * Make player fully aware of the given player.
 */
void aware_player(struct player *p, struct player *q)
{
    if (!q->k_idx) return;

    q->k_idx = 0;
    p->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
}


/*
 * Make player fully aware of the given mimic.
 *
 * When a player becomes aware of a mimic, we update the monster memory
 * and delete the "fake item" that the monster was mimicking.
 */
void become_aware(struct player *p, struct monster *m_ptr)
{
    const monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &p->lore[m_ptr->r_idx];

    if (!m_ptr->unaware) return;

    m_ptr->unaware = FALSE;

    /* Learn about mimicry */
    if (rf_has(r_ptr->flags, RF_UNAWARE)) rf_on(l_ptr->flags, RF_UNAWARE);

    /* Delete any false items */
    if (m_ptr->mimicked_o_idx > 0)
    {
        object_type *o_ptr = object_byid(m_ptr->mimicked_o_idx);
        char o_name[NORMAL_WID];

        object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_FULL);

        /* Print a message */
        msg(p, "The %s was really a monster!", o_name);

        /* Clear the mimicry */
        o_ptr->mimicking_m_idx = 0;

        /* Give the object to the monster if appropriate */
        if (rf_has(r_ptr->flags, RF_MIMIC_INV))
        {
            object_type *i_ptr;
            object_type object_type_body;

            /* Get local object */
            i_ptr = &object_type_body;

            /* Obtain local object */
            object_copy(i_ptr, o_ptr);

            /* Carry the object */
            monster_carry(m_ptr, i_ptr, TRUE);
        }

        /* Delete the mimicked object */
        delete_object_idx(m_ptr->mimicked_o_idx);
        m_ptr->mimicked_o_idx = 0;
    }

    /* Delete any false features */
    if (r_ptr->base == lookup_monster_base("feature mimic"))
    {
        /* Print a message */
        msg(p, "The %s was really a monster!",
            f_info[cave_get(p->depth)->feat[m_ptr->fy][m_ptr->fx]].name);

        /* Clear the feature */
        cave_set_feat(cave_get(p->depth), m_ptr->fy, m_ptr->fx, FEAT_FLOOR);
    }

    /* Update monster and item lists */
    p->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
    p->redraw |= (PR_MONLIST | PR_ITEMLIST);
}


/*
 * Returns TRUE if the given monster is currently mimicking.
 */
bool is_mimicking(struct monster *m_ptr)
{
    return m_ptr->unaware;
}


/*
 * The given monster learns about an "observed" resistance or other player
 * state property, or lack of it.
 */
void update_smart_learn(struct monster *m, struct player *p, int flag)
{
    monster_race *r_ptr = &r_info[m->r_idx];

    /* Sanity check */
    if (!flag) return;

    /* Anything a monster might learn, the player should learn */
    wieldeds_notice_flag(p, flag);

    /* Not allowed to learn */
    if (!cfg_ai_learn) return;

    /* Too stupid to learn anything */
    if (rf_has(r_ptr->flags, RF_STUPID)) return;

    /* Not intelligent, only learn sometimes */
    if (!rf_has(r_ptr->flags, RF_SMART) && one_in_(2)) return;

    /* Analyze the knowledge; fail very rarely */
    if (check_state(p, flag) && !one_in_(100))
        of_on(m->known_pflags, flag);
    else
        of_off(m->known_pflags, flag);
}


static bool is_detected_p(int Ind, int who, int dis_esp)
{
    player_type *p_ptr = player_get(Ind), *q_ptr = player_get(who);

    /* Full ESP */
    if (check_state(p_ptr, OF_ESP_ALL)) return TRUE;

    /* Partial ESP */
    if (player_has(q_ptr, PF_ORC) && check_state(p_ptr, OF_ESP_ORC)) return TRUE;
    if (player_has(q_ptr, PF_TROLL) && check_state(p_ptr, OF_ESP_TROLL)) return TRUE;
    if (player_has(q_ptr, PF_GIANT) && check_state(p_ptr, OF_ESP_GIANT)) return TRUE;
    if (player_has(q_ptr, PF_THUNDERLORD) && check_state(p_ptr, OF_ESP_DRAGON)) return TRUE;
    if (player_has(q_ptr, PF_ANIMAL) && check_state(p_ptr, OF_ESP_ANIMAL)) return TRUE;
    if (player_has(q_ptr, PF_DRAGON) && check_state(p_ptr, OF_ESP_DRAGON)) return TRUE;

    /* Radius ESP */
    if (check_state(p_ptr, OF_ESP_RADIUS)) return (dis_esp <= MAX_SIGHT);

    /* No ESP */
    return FALSE;
}


/*
 * This function updates the visibility flags for everyone who may see
 * this player.
 */
void update_player(int Ind)
{
    player_type *p_ptr, *q_ptr = player_get(Ind);
    monster_race *r_ptr = (q_ptr->r_idx? &r_info[q_ptr->r_idx]: NULL);
    int i;

    /* Current player location */
    int py = q_ptr->py;
    int px = q_ptr->px;

    /* Distance */
    int dis, dis_esp;

    /* Seen at all */
    bool flag = FALSE;

    /* Seen by vision */
    bool easy = FALSE;

    /* Efficiency -- Clear "shimmer" flag */
    q_ptr->shimmer = FALSE;

    /* Check for every other player */
    for (i = 1; i <= NumPlayers; i++)
    {
        p_ptr = player_get(i);

        /* Reset the flags */
        flag = easy = FALSE;

        /* Skip players not on this depth */
        if (p_ptr->depth != q_ptr->depth)
        {
            p_ptr->play_vis[Ind] = FALSE;
            p_ptr->play_los[Ind] = FALSE;
            p_ptr->play_det[Ind] = 0;
            continue;
        }

        /* Player can always see himself */
        if (Ind == i) continue;

        /* Compute distance */
        dis = distance(py, px, p_ptr->py, p_ptr->px);
        dis_esp = distance(py, px / 3, p_ptr->py, p_ptr->px / 3);

        /* Hack -- Detected via magical means */
        if (p_ptr->play_det[Ind]) flag = TRUE;

        /* Process players on current panel */
        else if (panel_contains(p_ptr, py, px))
        {
            bool isDM = ((p_ptr->dm_flags & DM_SEE_PLAYERS)? TRUE: FALSE);
            bool hasESP = is_detected_p(i, Ind, dis_esp);
            bool isTL = (player_has(p_ptr, PF_THUNDERLORD) &&
                (dis_esp <= (p_ptr->lev * MAX_SIGHT / PY_MAX_LEVEL)));

            /* Basic telepathy */
            if (isDM || hasESP || isTL)
            {
                /* Empty mind, no telepathy */
                if (r_ptr && rf_has(r_ptr->flags, RF_EMPTY_MIND)) {}

                /* Weird mind, occasional telepathy */
                else if (r_ptr && rf_has(r_ptr->flags, RF_WEIRD_MIND))
                {
                    /* One in ten individuals are detectable */
                    if ((Ind % 10) == 5)
                    {
                        /* Detectable */
                        flag = TRUE;

                        /* Check for LOS so that play_los is set later */
                        if (player_has_los_bold(p_ptr, py, px)) easy = TRUE;
                    }
                }

                /* Normal mind, allow telepathy */
                else
                {
                    /* Detectable */
                    flag = TRUE;

                    /* Check for LOS so that play_los is set later */
                    if (player_has_los_bold(p_ptr, py, px)) easy = TRUE;
                }

                /* DM has perfect ESP */
                if (isDM)
                {
                    /* Detectable */
                    flag = TRUE;

                    /* Check for LOS so that play_los is set later */
                    if (player_has_los_bold(p_ptr, py, px)) easy = TRUE;
                }
            }

            /* Normal line of sight, and not blind */
            if (player_has_los_bold(p_ptr, py, px) && !p_ptr->timed[TMD_BLIND])
            {
                /* Use "infravision" */
                if (dis <= (byte)(p_ptr->state.see_infra))
                {
                    /* Handle "cold blooded" players */
                    if (r_ptr && rf_has(r_ptr->flags, RF_COLD_BLOOD)) {}

                    /* Handle "warm blooded" players */
                    else
                    {
                        /* Easy to see */
                        easy = flag = TRUE;
                    }
                }

                /* Use "illumination" */
                if (player_can_see_bold(p_ptr, py, px))
                {
                    /* Handle "invisible" players */
                    if ((r_ptr && rf_has(r_ptr->flags, RF_INVISIBLE)) || q_ptr->timed[TMD_INVIS])
                    {
                        /* See invisible */
                        if (check_state(p_ptr, OF_SEE_INVIS))
                        {
                            /* Easy to see */
                            easy = flag = TRUE;
                        }
                    }

                    /* Handle "normal" monsters */
                    else
                    {
                        /* Easy to see */
                        easy = flag = TRUE;
                    }
                }
            }

            /* Players in the same party are always visible */
            if (in_party(p_ptr, q_ptr->party)) easy = flag = TRUE;

            /* Hack -- Dungeon masters are invisible */
            if (q_ptr->dm_flags & DM_SECRET_PRESENCE) easy = flag = FALSE;
        }

        /* Player is now visible */
        if (flag)
        {
            /* It was previously unseen */
            if (!p_ptr->play_vis[Ind])
            {
                /* Mark as visible */
                p_ptr->play_vis[Ind] = TRUE;

                /* Draw the player */
                cave_light_spot_aux(p_ptr, cave_get(q_ptr->depth), py, px);

                /* Disturb on appearance (except friendlies and unaware mimics) */
                if (OPT_P(p_ptr, disturb_move) &&
                    pvp_check(p_ptr, q_ptr, PVP_CHECK_ONE, TRUE, FEAT_NONE) && !q_ptr->k_idx)
                {
                    /* Disturb */
                    disturb(p_ptr, 1, 0);
                }
            }
            else
            {
                /* Player color may have changed! */
                cave_light_spot_aux(p_ptr, cave_get(q_ptr->depth), py, px);
            }

            /* Efficiency -- Notice multi-hued players */
            if (r_ptr && monster_shimmer(r_ptr) && allow_shimmer(p_ptr))
                q_ptr->shimmer = TRUE;

            /* Efficiency -- Notice party leaders */
            if (is_party_owner(p_ptr, q_ptr) && OPT_P(p_ptr, highlight_leader))
                q_ptr->shimmer = TRUE;

            /* Efficiency -- Notice elementalists */
            if (player_has(q_ptr, PF_ELEMENTAL_SPELLS) && allow_shimmer(p_ptr))
                q_ptr->shimmer = TRUE;
        }

        /* The player is not visible */
        else
        {
            /* It was previously seen */
            if (p_ptr->play_vis[Ind])
            {
                /* Mark as not visible */
                p_ptr->play_vis[Ind] = FALSE;

                /* Erase the player */
                cave_light_spot_aux(p_ptr, cave_get(q_ptr->depth), py, px);
            }
        }

        /* The player is now easily visible */
        if (easy)
        {
            /* Change */
            if (!p_ptr->play_los[Ind])
            {
                /* Mark as easily visible */
                p_ptr->play_los[Ind] = TRUE;

                /* Disturb on appearance (except friendlies and unaware mimics) */
                if (OPT_P(p_ptr, disturb_near) &&
                    pvp_check(p_ptr, q_ptr, PVP_CHECK_ONE, TRUE, FEAT_NONE) && !q_ptr->k_idx)
                {
                    /* Disturb */
                    disturb(p_ptr, 1, 0);
                }
            }
        }

        /* The player is not easily visible */
        else
        {
            /* Change */
            if (p_ptr->play_los[Ind])
            {
                /* Mark as not easily visible */
                p_ptr->play_los[Ind] = FALSE;
            }
        }
    }

    update_cursor(0 - Ind);
}

/*
 * This function simply updates all the players (see above).
 */
void update_players(void)
{
    int i;

    /* Update each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        /* Update the player */
        update_player(i);
    }
}


/*
 * Summon a specific race near this location.
 * Summon until we can't find a location or we have summoned size...
 */
bool summon_specific_race(int Ind, int depth, int y1, int x1, int r_idx, unsigned char size)
{
    player_type *p_ptr = player_get(Ind);
    int c, i, x, y;

    /* Forbid in the town or on special levels */
    if (forbid_special(depth)) return (FALSE);

    /* Paranoia, make sure the level is allocated */
    if (!cave_get(depth)) return (FALSE);

    /* For each monster we are summoning */
    for (c = 0; c < size; c++)
    {
        /* Look for a location */
        for (i = 0; i < 200; ++i)
        {
            /* Pick a distance */
            int d = (i / 15) + 1;

            /* Pick a location */
            scatter(depth, &y, &x, y1, x1, d, FALSE);

            /* Require "empty" floor grid */
            if (!cave_empty_bold(depth, y, x)) continue;

            /* No summon on glyph of warding */
            if (cave_get(depth)->feat[y][x] == FEAT_GLYPH) continue;

            /* Okay */
            break;
        }

        /* Failure */
        if (i == 200) return (FALSE);

        /* Handle failure */
        if (!r_idx) return (FALSE);

        /* Attempt to place the monster (awake, don't allow groups) */
        if (!place_new_monster(p_ptr, cave_get(depth), y, x, r_idx, 0, ORIGIN_DROP_SUMMON))
            return (FALSE);
    }

    /* Success */
    return (TRUE);
}


/* Summon a specific race at a random location */
bool summon_specific_race_somewhere(int Ind, int depth, int r_idx, unsigned char size)
{
    int y, x;
    int tries = 50;

    /* Forbid in the town or on special levels */
    if (forbid_special(depth)) return (FALSE);

    /* Paranoia, make sure the level is allocated */
    if (!cave_get(depth)) return (FALSE);

    /* Find a legal, distant, unoccupied, space */
    while (--tries)
    {
        /* Pick a location */
        y = randint0(DUNGEON_HGT);
        x = randint0(DUNGEON_WID);

        /* Require "naked" floor grid */
        if (!cave_naked_bold(depth, y, x)) continue;

        /* We have a valid location */
        break;
    }

    /* Abort */
    if (!tries) return (FALSE);

    /* Attempt to place the monster */
    if (summon_specific_race(Ind, depth, y, x, r_idx, size)) return TRUE;
    return (FALSE);
}


/*
 * Takes a monster name and returns an index, or 0 if no such monster was found.
 */
int race_index(char *name)
{
    char monster[NORMAL_WID];
    char* str;
    char* dst;
    int i;

    /* Lowercase our search string */
    for (str = name; *str; str++) *str = tolower((unsigned char)*str);

    /* For each monster race */
    for (i = 1; i < z_info->r_max; i++)
    {
        /* Skip non-entries */
        if (!r_info[i].name) continue;

        /* Clean up monster name */
        dst = monster;
        for (str = r_info[i].name; *str; str++)
        {
            if (isalpha(*str) || (*str == 32)) *dst++ = tolower((unsigned char)*str);
        }
        *dst++ = '\0';

        /* If cleaned name matches our search string, return it */
        if (!strcmp(monster, name)) return i;
    }
    return 0;
}


/*
 * Takes a (partial) monster name and returns an index, or 0 if no match was found.
 */
int race_index_fuzzy(char *name)
{
    char monster[NORMAL_WID];
    char* str;
    char* dst;
    int i;

    /* Lowercase our search string */
    for (str = name; *str; str++) *str = tolower((unsigned char)*str);

    /* For each monster race */
    for (i = 1; i < z_info->r_max; i++)
    {
        /* Skip non-entries */
        if (!r_info[i].name) continue;

        /* Clean up monster name */
        dst = monster;
        for (str = r_info[i].name; *str; str++)
        {
            if (isalpha(*str) || (*str == 32)) *dst++ = tolower((unsigned char)*str);
        }
        *dst++ = '\0';

        /* If cleaned name matches our search string, return it */
        if (strstr(monster, name)) return i;
    }
    return 0;
}


bool is_humanoid(const monster_race *r_ptr)
{
    return rf_has(r_ptr->flags, RF_HUMANOID);
}


/*
 * Half humanoid monsters: nagas (half snake/half human), hybrids,
 * driders (half spider/half human), human metamorphs
 */
bool is_half_humanoid(const monster_race *r_ptr)
{
    if ((r_ptr->base == lookup_monster_base("naga")) || strstr(r_ptr->name, "harpy") ||
        strstr(r_ptr->name, "taur") || streq(r_ptr->name, "Sphinx") ||
        streq(r_ptr->name, "Gorgon") || streq(r_ptr->name, "Drider") ||
        strstr(r_ptr->name, "Were")) return TRUE;

    return FALSE;
}


void update_monlist(struct monster *m)
{
    int i;

    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        if (p_ptr->depth == m->depth)
            p_ptr->redraw |= PR_MONLIST;
    }
}
