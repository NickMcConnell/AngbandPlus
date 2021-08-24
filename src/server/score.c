/*
 * File: score.c
 * Purpose: Highscore handling
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2018 MAngband and PWMAngband Developers
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
 * Read in a highscore file
 */
size_t highscore_read(high_score scores[], size_t sz)
{
    char fname[MSG_LEN];
    ang_file *scorefile;
    size_t i;

    /* Wipe current scores */
    memset(scores, 0, sz * sizeof(high_score));

    path_build(fname, sizeof(fname), ANGBAND_DIR_SCORES, "scores.raw");
    scorefile = file_open(fname, MODE_READ, FTYPE_RAW);

    if (!scorefile) return true;

    for (i = 0; i < sz; i++)
    {
        if (file_read(scorefile, (char *)&scores[i], sizeof(high_score)) <= 0) break;
    }

    file_close(scorefile);

    return i;
}


/*
 * Just determine where a new score *would* be placed
 * Return the location (0 is best) or -1 on failure
 */
size_t highscore_where(const high_score *entry, const high_score scores[], size_t sz)
{
    size_t i;

    /* Read until we get to a higher score */
    for (i = 0; i < sz; i++)
    {
        long entry_pts = strtoul(entry->pts, NULL, 0);
        long score_pts = strtoul(scores[i].pts, NULL, 0);

        if (entry_pts >= score_pts) return i;
        if (scores[i].what[0] == '\0') return i;
    }

    /* The last entry is always usable */
    return sz - 1;
}


size_t highscore_add(const high_score *entry, high_score scores[], size_t sz)
{
    size_t slot = highscore_where(entry, scores, sz);

    memmove(&scores[slot + 1], &scores[slot], sizeof(high_score) * (sz - 1 - slot));
    memcpy(&scores[slot], entry, sizeof(high_score));

    return slot;
}


static size_t highscore_count(const high_score scores[], size_t sz)
{
    size_t i;

    for (i = 0; i < sz; i++)
    {
        if (scores[i].what[0] == '\0') break;
    }

    return i;
}


/*
 * Actually place an entry into the high score file
 * Return the location (0 is best) or -1 on "failure"
 */
static void highscore_write(const high_score scores[], size_t sz)
{
    size_t n;
    ang_file *lok;
    ang_file *scorefile;
    char old_name[MSG_LEN];
    char cur_name[MSG_LEN];
    char new_name[MSG_LEN];
    char lok_name[MSG_LEN];

    path_build(old_name, sizeof(old_name), ANGBAND_DIR_SCORES, "scores.old");
    path_build(cur_name, sizeof(cur_name), ANGBAND_DIR_SCORES, "scores.raw");
    path_build(new_name, sizeof(new_name), ANGBAND_DIR_SCORES, "scores.new");
    path_build(lok_name, sizeof(lok_name), ANGBAND_DIR_SCORES, "scores.lok");

    /* Read in and add new score */
    n = highscore_count(scores, sz);

    /* Lock scores */
    if (file_exists(lok_name))
    {
        plog("Lock file in place for scorefile; not writing.");
        return;
    }

    lok = file_open(lok_name, MODE_WRITE, FTYPE_RAW);
    file_lock(lok);

    if (!lok)
    {
        plog("Failed to create lock for scorefile; not writing.");
        return;
    }

    /* Open the new file for writing */
    scorefile = file_open(new_name, MODE_WRITE, FTYPE_RAW);

    if (!scorefile)
    {
        plog("Failed to open new scorefile for writing.");

        file_close(lok);
        file_delete(lok_name);
        return;
    }

    file_write(scorefile, (const char *)scores, sizeof(high_score) * n);
    file_close(scorefile);

    /* Now move things around */
    if (file_exists(old_name) && !file_delete(old_name))
        plog("Couldn't delete old scorefile");

    if (file_exists(cur_name) && !file_move(cur_name, old_name))
        plog("Couldn't move old scores.raw out of the way");

    if (!file_move(new_name, cur_name))
        plog("Couldn't rename new scorefile to scores.raw");

    /* Remove the lock */
    file_close(lok);
    file_delete(lok_name);
}


void build_score(struct player *p, high_score *entry, const char *died_from, time_t *death_time)
{
    char psex;
    struct player_death_info score_info;

    memset(entry, 0, sizeof(high_score));

    switch (p->psex)
    {
        case SEX_MALE: psex = 'm'; break;
        case SEX_FEMALE: psex = 'f'; break;
        default: psex = 'n'; break;
    }

    /* Score info */
    memset(&score_info, 0, sizeof(score_info));
    if (death_time)
    {
        /* Hack -- take the saved cause of death of the character, not the ghost */
        memcpy(&score_info, &p->death_info, sizeof(struct player_death_info));
    }
    else
    {
        /* Take the current info */
        score_info.max_lev = p->max_lev;
        score_info.lev = p->lev;
        score_info.max_exp = p->max_exp;
        score_info.au = p->au;
        score_info.max_depth = p->max_depth;
        memcpy(&score_info.wpos, &p->wpos, sizeof(struct worldpos));
    }

    /* Save the version */
    strnfmt(entry->what, sizeof(entry->what), "%s", version_build(VB_BASE));

    /* Calculate and save the points */
    strnfmt(entry->pts, sizeof(entry->pts), "%9u",
        total_points(p, score_info.max_exp, score_info.max_depth));

    /* Save the current gold */
    strnfmt(entry->gold, sizeof(entry->gold), "%9u", score_info.au);

    /* Save the current turn */
    my_strcpy(entry->turns, ht_show(&turn), sizeof(entry->turns));

    /* Time of death */
    if (death_time)
        strftime(entry->day, sizeof(entry->day), "@%Y%m%d", localtime(death_time));
    else
        my_strcpy(entry->day, "TODAY", sizeof(entry->day));

    /* Save the player name (15 chars) */
    strnfmt(entry->who, sizeof(entry->who), "%-.15s", p->name);

    /* Save the player info */
    strnfmt(entry->uid, sizeof(entry->uid), "%7u", 0);
    strnfmt(entry->sex, sizeof(entry->sex), "%c", psex);
    strnfmt(entry->p_r, sizeof(entry->p_r), "%2d", p->race->ridx);
    strnfmt(entry->p_c, sizeof(entry->p_c), "%2d", p->clazz->cidx);

    /* Save the level and such */
    strnfmt(entry->cur_lev, sizeof(entry->cur_lev), "%3d", score_info.lev);
    strnfmt(entry->cur_dun, sizeof(entry->cur_dun), "%3d", score_info.wpos.depth);
    strnfmt(entry->max_lev, sizeof(entry->max_lev), "%3d", score_info.max_lev);
    strnfmt(entry->max_dun, sizeof(entry->max_dun), "%3d", score_info.max_depth);

    /* Save the cause of death (31 chars) */
    strnfmt(entry->how, sizeof(entry->how), "%-.31s", died_from);
}


/*
 * Enters a players name on a hi-score table, if "legal".
 */
void enter_score(struct player *p, time_t *death_time)
{
    high_score entry;
    high_score scores[MAX_HISCORES];

    /* Add a new entry, if allowed */
    if (p->noscore)
    {
        msg(p, "Score not registered for wizards, quitters and cheaters.");
        return;
    }

    /* Add a new entry to the score list, see where it went */
    build_score(p, &entry, p->death_info.died_from, death_time);

    highscore_read(scores, N_ELEMENTS(scores));
    highscore_add(&entry, scores, N_ELEMENTS(scores));
    highscore_write(scores, N_ELEMENTS(scores));
}


/*
 * Calculates the total number of points earned
 */
long total_points(struct player *p, s32b max_exp, s16b max_depth)
{
    long base_score = max_exp + (100 * max_depth);
    long score = base_score;

    /* We award a 50% score bonus for bravery with no ghost characters */
    if (OPT(p, birth_no_ghost)) score += base_score / 2;

    /* We award a 50% score bonus for bravery with ironman characters */
    if (OPT(p, birth_no_recall)) score += base_score / 4;
    if (OPT(p, birth_force_descend) && (cfg_limit_stairs < 3)) score += base_score / 4;

    /* We award a 50% score bonus for bravery with fruit bat characters */
    if (OPT(p, birth_fruit_bat))
    {
        score += base_score / 2;

        /* Boost further on normal servers */
        if (!cfg_no_ghost) score += base_score / 2;
    }

    /* Standard scoring */
    return (score);
}
