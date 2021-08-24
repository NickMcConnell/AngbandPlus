/*
 * File: score-ui.c
 * Purpose: Highscore display for Angband
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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

    /* Hack -- count the high scores */
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

        /* Hack -- indicate current in green */
        attr = ((j == highlight)? 'G': 'w');

        /* Hack -- indicate winners in purple */
        if (streq(score->how, "winner")) attr = 'v';

        /* Extract the race/class */
        c = player_id2class(atoi(score->p_c));
        r = player_id2race(atoi(score->p_r));

        /* Extract the level info */
        clev = atoi(score->cur_lev);
        mlev = atoi(score->max_lev);
        cdun = atoi(score->cur_dun);
        mdun = atoi(score->max_dun);

        /* Hack -- extract the gold and such */
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


/*
 * Display hall of fame
 */
void do_cmd_knowledge_scores(struct player *p, int line)
{
    int j;
    high_score the_score;
    high_score scores[MAX_HISCORES];
    ang_file *fff;
    char file_name[MSG_LEN];

    /* Read scores, place current score */
    highscore_read(scores, N_ELEMENTS(scores));
    if (p->ghost)
        build_score(p, &the_score, p->death_info.died_from, &p->death_info.time);
    else
        build_score(p, &the_score, "nobody (yet!)", NULL);
    if (p->is_dead)
        j = highscore_where(&the_score, scores, N_ELEMENTS(scores));
    else
        j = highscore_add(&the_score, scores, N_ELEMENTS(scores));

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    /* Hack -- display the top 25 scores */
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
    show_file(p, file_name, "Hall of Fame", line, 1);

    /* Remove the file */
    file_delete(file_name);
}