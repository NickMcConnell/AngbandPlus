/* File: player_scores.cpp */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/player_scores.h>
#include <src/npp.h>

bool scores_sort(high_score first, high_score second)
{
    if (first.score >= second.score) return (TRUE);

    return (FALSE);
}


/*
 * Hack -- Calculates the total number of points earned
 */
static u32b total_points(void)
{
    int i;
    u32b points = p_ptr->max_exp / 10;
    points += (100 * p_ptr->max_depth);
    points += (p_ptr->q_fame * 1000);

    // Factor in player ghosts and uniques
    for (i = 1; i < z_info->r_max - 1; i++)
    {
        monster_race *r_ptr = &r_info[i];

        // Require uniques
        if (!r_ptr->is_unique()) continue;

        //Not killed yet;
        if (r_ptr->max_num != 0) continue;

        // different scoring for player ghosts
        if (r_ptr->is_player_ghost())
        {
            points += r_ptr->level * 500;
        }
        else points += r_ptr->level * 250;
    }

    // Factor in normal artifacts found
    for (i = 1; i < z_info->art_norm_max; i++)
    {
        artifact_type *a_ptr = &a_info[i];

        /* Skip "empty" items */
        if (a_ptr->tval + a_ptr->sval == 0) continue;

        /* Ignore artifacts not found*/
        if (!a_ptr->a_cur_num) continue;

        points += (a_ptr->a_level * a_ptr->a_rarity) / ( i <= z_info->art_spec_max ? 3 : 10);
    }

    if (p_ptr->game_turn < 200000) return (points);

    // Factor in amount of time played, reduce points for longer games.
    int x = (p_ptr->game_turn / 100000) - 1;
    for (i = 0; i < x; i++)
    {
        if (i < 10) points = (points * 99) / 100;
        else if (i < 20) points = (points * 98) / 100;
        else if (i < 30) points = (points * 97) / 100;
        else points = (points * 96) / 100;
        if (points < 100) break;
    }

    return (points);
}

high_score build_score(QString date_death)
{

    high_score this_score;
    high_score *score_ptr = &this_score;

    score_ptr->version = VERSION_STRING;

    /* Calculate and save the points */
    score_ptr->score = total_points();

    /* Save the current turn */
    score_ptr->turns = p_ptr->game_turn;

    /* Time of death */
    score_ptr->date_time = date_death;

    /* Save the player name (15 chars) */
    score_ptr->p_name = op_ptr->full_name;

    /* Save the player info XXX XXX XXX */
    if (p_ptr->psex == SEX_MALE) score_ptr->p_sex = QString("Male");
    else score_ptr->p_sex = QString("Female");

    // Save race_class
    score_ptr->p_race = p_info[p_ptr->prace].pr_name;
    score_ptr->p_class = c_info[p_ptr->pclass].cl_name;

    /* Save the level and such */
    score_ptr->cur_level =  p_ptr->lev;
    score_ptr->cur_depth =  p_ptr->depth;
    score_ptr->cur_exp   =  p_ptr->exp;
    score_ptr->max_level =  p_ptr->max_lev;
    score_ptr->max_depth =  p_ptr->max_depth;
    score_ptr->max_exp =    p_ptr->max_exp;
    score_ptr->fame =       p_ptr->q_fame;

    /* No cause of death */
    score_ptr->death_how = (QString("%1") .arg(p_ptr->died_from));

    return (this_score);
}

/*
 * Enters a players name on a hi-score table, for non-wizards.
 */
void enter_score(QString date_death)
{
    /* Cheaters are not scored */
    for (int j = OPT_CHEAT_HEAD; j < OPT_CHEAT_TAIL; ++j)
    {
        if (op_ptr->opt[j]) return;
    }

        /* Wizard-mode pre-empts scoring */
    if (p_ptr->is_wizard) return;

    /* Game inpterruped or quie */
    if (!p_ptr->total_winner)
    {

        if ((operator==(p_ptr->died_from, "Interrupting")) ||
            (operator==(p_ptr->died_from, "Quitting"))) return;
    }

    high_score entry = build_score(date_death);

    player_scores_list.append(entry);

    // Sort the scores
    qSort(player_scores_list.begin(), player_scores_list.end(), scores_sort);

}

/*
 * Predict the players score based on current game.
 */
void update_player_score(void)
{
    p_ptr->current_score = total_points();
    p_ptr->redraw |= (PR_PLYR_SCORE);
}
