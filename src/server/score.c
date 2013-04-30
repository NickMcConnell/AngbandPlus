/*
 * File: score.c
 * Purpose: Highscore handling
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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


#include "s-angband.h"
#include "../common/buildid.h"
#include "files.h"


/*
 * Maximum number of high scores in the high score file
 */
#define MAX_HISCORES    100


/*
 * Semi-Portable High Score List Entry (128 bytes)
 *
 * All fields listed below are null terminated ascii strings.
 *
 * In addition, the "number" fields are right justified, and
 * space padded, to the full available length (minus the "null").
 *
 * Note that "string comparisons" are thus valid on "pts".
 */
typedef struct
{
    char what[8];       /* Version info (string) */
    char pts[10];       /* Total Score (number) */
    char gold[10];      /* Total Gold (number) */
    char turns[20];     /* Turns Taken (number) */
    char day[10];       /* Time stamp (string) */
    char who[16];       /* Player Name (string) */
    char uid[8];        /* Player UID (number) */
    char sex[2];        /* Player Sex (string) */
    char p_r[3];        /* Player Race (number) */
    char p_c[3];        /* Player Class (number) */
    char cur_lev[4];    /* Current Player Level (number) */
    char cur_dun[4];    /* Current Dungeon Level (number) */
    char max_lev[4];    /* Max Player Level (number) */
    char max_dun[4];    /* Max Dungeon Level (number) */
    char how[32];       /* Method of death (string) */
} high_score;


/*
 * Read in a highscore file
 */
static size_t highscore_read(high_score scores[], size_t sz)
{
    char fname[MSG_LEN];
    ang_file *scorefile;
    size_t i;

    /* Wipe current scores */
    C_WIPE(scores, sz, high_score);

    path_build(fname, sizeof(fname), ANGBAND_DIR_APEX, "scores.raw");
    scorefile = file_open(fname, MODE_READ, FTYPE_RAW);

    if (!scorefile) return TRUE;

    for (i = 0;
        (i < sz) &&
            (file_read(scorefile, (char *)&scores[i], sizeof(high_score)) > 0);
        i++)
    {
    }

    file_close(scorefile);

    return i;
}


/*
 * Just determine where a new score *would* be placed
 * Return the location (0 is best) or -1 on failure
 */
static size_t highscore_where(const high_score *entry,
    const high_score scores[], size_t sz)
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


static size_t highscore_add(const high_score *entry, high_score scores[],
    size_t sz)
{
    size_t slot = highscore_where(entry, scores, sz);

    memmove(&scores[slot + 1], &scores[slot], sizeof(high_score) * (sz - 1 - slot));
    COPY(&scores[slot], entry, high_score);

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

    path_build(old_name, sizeof(old_name), ANGBAND_DIR_APEX, "scores.old");
    path_build(cur_name, sizeof(cur_name), ANGBAND_DIR_APEX, "scores.raw");
    path_build(new_name, sizeof(new_name), ANGBAND_DIR_APEX, "scores.new");
    path_build(lok_name, sizeof(lok_name), ANGBAND_DIR_APEX, "scores.lok");

    /* Read in and add new score */
    n = highscore_count(scores, sz);

    /*** Lock scores ***/

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

    /*** Open the new file for writing ***/

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

    /*** Now move things around ***/

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


/*
 * Display the scores in a given range.
 */
static void display_scores_aux(ang_file *fff, const high_score scores[], int from, int to,
    int highlight)
{
    int j, place;
    int count;

    /* Assume we will show the first 10 */
    if (from < 0) from = 0;
    if (to < 0) to = 10;
    if (to > MAX_HISCORES) to = MAX_HISCORES;

    /* Hack -- Count the high scores */
    for (count = 0; count < MAX_HISCORES; count++)
    {
        if (!scores[count].what[0]) break;
    }

    /* Forget about the last entries */
    if (count > to) count = to;

    /* Show 5 per page, until "done" */
    for (j = from, place = j + 1; j < count; j++, place++)
    {
        char out_val[160];
        char tmp_val[160];
        const high_score *score = &scores[j];
        char attr;
        int clev, mlev, cdun, mdun;
        const char *user, *gold, *when, *aged;
        struct player_class *c;
        struct player_race *r;

        /* Hack -- Indicate current in green */
        attr = ((j == highlight)? 'G': 'w');

        /* Hack -- Indicate winners in purple */
        if (streq(score->how, "winner")) attr = 'v';

        /* Extract the race/class */
        c = player_id2class(atoi(score->p_c));
        r = player_id2race(atoi(score->p_r));

        /* Extract the level info */
        clev = atoi(score->cur_lev);
        mlev = atoi(score->max_lev);
        cdun = atoi(score->cur_dun);
        mdun = atoi(score->max_dun);

        /* Hack -- Extract the gold and such */
        for (user = score->uid; isspace(*user); user++) /* loop */;
        for (when = score->day; isspace(*when); when++) /* loop */;
        for (gold = score->gold; isspace(*gold); gold++) /* loop */;
        for (aged = score->turns; isspace(*aged); aged++) /* loop */;

        /* Dump some info */
        strnfmt(out_val, sizeof(out_val), "%3d.%9s  %s the %s %s, Level %d",
            place, score->pts, score->who, r->name, c->name, clev);

        /* Append a "maximum level" */
        if (mlev > clev) my_strcat(out_val, format(" (Max %d)", mlev), sizeof(out_val));

        /* Dump the first line */
        file_putf(fff, "%c%s\n", attr, out_val);

        /* Died where? */
        if (streq(score->how, "winner"))
            strnfmt(out_val, sizeof(out_val), "Retired after a legendary career");
        else if (!cdun)
            strnfmt(out_val, sizeof(out_val), "Killed by %s in the town", score->how);
        else
            strnfmt(out_val, sizeof(out_val), "Killed by %s on dungeon level %d", score->how, cdun);

        /* Append a "maximum level" */
        if (mdun > cdun)
            my_strcat(out_val, format(" (Max %d)", mdun), sizeof(out_val));

        /* Dump the info */
        file_putf(fff, "%c               %s\n", attr, out_val);

        /* Clean up standard encoded form of "when" */
        if ((*when == '@') && strlen(when) == 9)
        {
            strnfmt(tmp_val, sizeof(tmp_val), "%.4s-%.2s-%.2s", when + 1, when + 5, when + 7);
            when = tmp_val;
        }

        /* And still another line of info */
        strnfmt(out_val, sizeof(out_val), "(User %s, Date %s, Gold %s, Turn %s).", user, when, gold,
            aged);
        file_putf(fff, "%c               %s\n", attr, out_val);

        /* Print newline if this isn't the last one */
        if (j < count - 1) file_put(fff, "d\n");
    }
}


static void build_score(int Ind, high_score *entry, const char *died_from, time_t *death_time)
{
    player_type *p_ptr = player_get(Ind);
    char sx;
    struct player_death_info score_info;

    WIPE(entry, high_score);

    /* Score info */
    WIPE(&score_info, struct player_death_info);
    if (death_time)
    {
        /* Hack -- Take the saved cause of death of the character, not the ghost */
        COPY(&score_info, &p_ptr->death_info, struct player_death_info);
    }
    else
    {
        /* Take the current info */
        score_info.max_lev = p_ptr->max_lev;
        score_info.lev = p_ptr->lev;
        score_info.max_exp = p_ptr->max_exp;
        score_info.au = p_ptr->au;
        score_info.max_depth = p_ptr->max_depth;
        score_info.depth = p_ptr->depth;
    }

    /* Save the version */
    strnfmt(entry->what, sizeof(entry->what), "%s", get_buildver());

    /* Calculate and save the points */
    strnfmt(entry->pts, sizeof(entry->pts), "%9lu",
        (long)total_points(p_ptr, score_info.max_exp, score_info.max_depth));

    /* Save the current gold */
    strnfmt(entry->gold, sizeof(entry->gold), "%9lu", (long)score_info.au);

    /* Save the current turn */
    my_strcpy(entry->turns, ht_show(&turn), sizeof(entry->turns));

    /* Time of death */
    if (death_time)
        strftime(entry->day, sizeof(entry->day), "@%Y%m%d", localtime(death_time));
    else
        my_strcpy(entry->day, "TODAY", sizeof(entry->day));

    /* Save the player name (15 chars) */
    strnfmt(entry->who, sizeof(entry->who), "%-.15s", p_ptr->name);

    switch (p_ptr->psex)
    {
        case SEX_FEMALE: sx = 'f'; break;
        case SEX_MALE: sx = 'm'; break;
        default: sx = 'n'; break;
    }

    /* Save the player info */
    strnfmt(entry->uid, sizeof(entry->uid), "%7u", 0);
    strnfmt(entry->sex, sizeof(entry->sex), "%c", sx);
    strnfmt(entry->p_r, sizeof(entry->p_r), "%2d", p_ptr->race->ridx);
    strnfmt(entry->p_c, sizeof(entry->p_c), "%2d", p_ptr->clazz->cidx);

    /* Save the level and such */
    strnfmt(entry->cur_lev, sizeof(entry->cur_lev), "%3d", score_info.lev);
    strnfmt(entry->cur_dun, sizeof(entry->cur_dun), "%3d", score_info.depth);
    strnfmt(entry->max_lev, sizeof(entry->max_lev), "%3d", score_info.max_lev);
    strnfmt(entry->max_dun, sizeof(entry->max_dun), "%3d", score_info.max_depth);

    /* Save the cause of death (31 chars) */
    strnfmt(entry->how, sizeof(entry->how), "%-.31s", died_from);
}


/*
 * Enters a players name on a hi-score table, if "legal".
 */
void enter_score(int Ind, time_t *death_time)
{
    player_type *p_ptr = player_get(Ind);
    high_score entry;
    high_score scores[MAX_HISCORES];

    /* Wizard-mode pre-empts scoring */
    if (p_ptr->noscore)
    {
        msg(p_ptr, "Score not registered for wizards, quitters and cheaters.");
        return;
    }

    /* Add a new entry to the score list, see where it went */
    build_score(Ind, &entry, p_ptr->death_info.died_from, death_time);

    highscore_read(scores, N_ELEMENTS(scores));
    highscore_add(&entry, scores, N_ELEMENTS(scores));
    highscore_write(scores, N_ELEMENTS(scores));
}


/*
 * Display hall of fame
 */
void do_cmd_knowledge_scores(int Ind, int line)
{
    player_type *p_ptr = player_get(Ind);
    int j;
    high_score the_score;
    high_score scores[MAX_HISCORES];
    ang_file *fff;
    char file_name[MSG_LEN];

    /* Read scores, place current score */
    highscore_read(scores, N_ELEMENTS(scores));
    if (p_ptr->ghost)
        build_score(Ind, &the_score, p_ptr->death_info.died_from, &p_ptr->death_info.time);
    else
        build_score(Ind, &the_score, "nobody (yet!)", NULL);
    if (p_ptr->is_dead)
        j = highscore_where(&the_score, scores, N_ELEMENTS(scores));
    else
        j = highscore_add(&the_score, scores, N_ELEMENTS(scores));

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    /* Hack -- Display the top 25 scores */
    if (j < 20)
        display_scores_aux(fff, scores, 0, 25, j);

    /* Display some "useful" scores */
    else
    {
        display_scores_aux(fff, scores, 0, 15, -1);
        file_put(fff, "d\n");
        display_scores_aux(fff, scores, j - 2, j + 7, j);
    }

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(Ind, file_name, "Hall of Fame", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


/*
 * Hack -- Calculates the total number of points earned
 */
long total_points(struct player *p, s32b max_exp, s16b max_depth)
{
    long base_score = max_exp + (100 * max_depth);
    long score = base_score;

    /* We award a 50% score bonus for bravery with no ghost characters */
    if ((OPT_P(p, birth_no_ghost) || OPT_P(p, birth_fruit_bat)) && !cfg_no_ghost)
        score += base_score / 2;

    /* We award a 50% score bonus for bravery with ironman characters */
    if (OPT_P(p, birth_ironman) && !cfg_no_recall) score += base_score / 4;
    if (OPT_P(p, birth_ironman) && (cfg_limit_stairs != 2)) score += base_score / 4;

    /* We award a 50% score bonus for bravery with fruit bat characters */
    if (OPT_P(p, birth_fruit_bat)) score += base_score / 2;

    /* Standard scoring */
    return (score);
}
