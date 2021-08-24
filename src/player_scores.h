#ifndef PLAYER_SCORES_H
#define PLAYER_SCORES_H

#include <src/player.h>
#include <src/object_classes.h>
#include <QString>
#include <QVector>


typedef struct high_score high_score;

struct high_score
{
    QString version;		/* Version info*/

    u32b score;           /* Total Score*/

    s32b turns;         /* Turns Taken*/

    QString date_time;  /* Time stamp*/

    QString p_name;     /* Player Name*/

    QString p_sex;        /* Player Sex*/
    QString p_race;       /* Player Race*/
    QString p_class;      /* Player Class*/

    s16b cur_level;		/* Current Player Level*/
    s16b cur_depth;		/* Current Dungeon Level*/
    s32b cur_exp;       // Experience
    s16b max_level;		/* Max Player Level*/
    s16b max_depth;		/* Max Dungeon Level*/
    s32b max_exp;       // Max Experience
    u16b fame;           // Final fame score


    QString death_how;		/* Method of death*/
};

extern QVector<high_score> player_scores_list;

extern bool scores_sort(high_score first, high_score second);
extern high_score build_score(QString date_death);
extern void enter_score(QString date_death);
extern void update_player_score(void);

#endif // PLAYER_SCORES_H
