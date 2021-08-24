#ifndef PLAYER_BIRTH_H
#define PLAYER_BIRTH_H

#include <src/player_screen.h>
#include <QButtonGroup>
#include <QLineEdit>



/*
 * A class to hold "rolled" information, and any
 * other useful state for the birth process.
 */
class Birther
{
public:


    byte mode;

    QString full_name;

    byte p_sex;
    byte p_race;
    byte p_class;

    s16b age;
    s16b wt;
    s16b ht;
    s16b sc;

    s32b au;

    s16b stat[A_MAX];

    QString history;

    void birther_wipe();
    void save();
    void load();

};



class PlayerBirth : public QDialog
{
    Q_OBJECT

public:
    explicit PlayerBirth(bool quickstart);

    bool quick_start;

private:
    void setup_character();
    void char_name_label(QGridLayout *return_layout);

    // try not to update the character more than once each action
    bool hold_update;

    QWidget *top_widget;
    QGridLayout *glay_char_basic;
    QGridLayout *glay_char_data;
    QGridLayout *glay_ability_info;


    //Option checkboxes
    QButtonGroup *group_options;
    void add_option_boxes(QVBoxLayout *return_layout);

    //Gender and buttons
    QButtonGroup *group_gender;
    QLineEdit *player_name;
    QString cur_name;
    int cur_gender;
    void add_genders(QVBoxLayout *return_layout);

    //Race
    QButtonGroup *group_race;
    int cur_race;
    void add_races(QVBoxLayout *return_layout);

    //Class
    QButtonGroup *group_class;
    int cur_class;
    void add_classes(QVBoxLayout *return_layout);

    //help boxes
    QLabel *race_info;
    QLabel *class_info;
    void add_info_boxes(QVBoxLayout *return_layout);

    //stats
    void add_stat_boxes(QVBoxLayout *return_layout);
    void add_stat_results(void);
    void update_stats_info();

    //Statroll options
    QVBoxLayout *vlay_stats_current;
    QGridLayout *grid_stat_results;
    QButtonGroup *group_stat_choice;
    void add_stat_choices(QVBoxLayout *return_layout);

    void update_screen(void);
    void update_character(bool new_player, bool needs_stat_update);


private slots:
    void name_changed(QString new_name);
    void gender_changed(int new_gender);
    void race_changed(int new_race);
    void class_changed(int new_class);
    void option_changed(int index);
    void call_options_dialog(void);
    void point_button_chosen(void);
    void random_button_chosen(void);
    void redo_stat_box(void);
    void stat_spin_changed(int new_value);
    void random_roll(void);
    void accept_char(void);
    void reject_char(void);



    // Random char slots
    void random_name(void);
    void random_gender(void);
    void random_race(void);
    void random_class(void);
    void random_all(void);

};

// birth.cpp
extern void update_hp_sp(void);
extern void init_birth();
extern void finish_birth();
extern void reset_stats(void);
extern bool buy_stat(int choice);
extern void sell_stat(int choice);
extern void generate_stats(void);
extern void generate_player(bool full);
extern void roll_player(void);
extern bool has_prev_character();
extern void save_prev_character();
extern void load_prev_character();


extern int points_spent;
extern int stats[A_MAX];

#define MAX_POINTS 24

#define POINTS_LEFT  (MAX_POINTS - points_spent)

#endif // PLAYER_BIRTH_H
