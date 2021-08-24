/*
 * Copyright (c) 2014 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */


#include <src/player_birth.h>
#include <src/optionsdialog.h>
#include <QDialogButtonBox>
#include <QRadioButton>
#include <src/help.h>
#include <QCheckBox>
#include <QList>
#include <QPushButton>
#include <QSpinBox>


int points_spent;
int stats[A_MAX];

void PlayerBirth::reject_char(void)
{
    this->reject();
}

void PlayerBirth::accept_char(void)
{
    finish_birth();
    this->accept();
}


//assumes layout is consistent with add_stat_boxes below
void PlayerBirth::update_stats_info()
{
    QList<QLabel *> lbl_list = this->findChildren<QLabel *>();
    {
        for (int x = 0; x < lbl_list.size(); x++)
        {
            QLabel *this_lbl = lbl_list.at(x);

            QString this_name = this_lbl->objectName();

            //Not a named label
            if (!this_name.length()) continue;

            // Update all of the stats
            for (int i = 0; i < A_MAX; i++)
            {
                int r = rp_ptr->r_adj[i];
                int c = cp_ptr->c_adj[i];
                int rc = r + c;

                if (this_name.contains(QString("base_%1") .arg(i)))
                {
                    QString base_num = (QString("<b>%1 </b>") .arg(cnv_stat(stats[i])));
                    base_num = color_string(base_num, TERM_BLUE);
                    this_lbl->setText(base_num);

                    continue;
                }

                if (this_name.contains(QString("race_%1") .arg(i)))
                {
                    QString race_num = format_stat(r);
                    race_num.append(" ");
                    race_num = color_string(race_num, (r < 0 ? TERM_RED : TERM_BLUE));
                    this_lbl->setText(race_num);
                    continue;
                }

                if (this_name.contains(QString("class_%1") .arg(i)))
                {
                    QString class_num = format_stat(c);
                    class_num.append(" ");
                    class_num = color_string(class_num, (c < 0 ? TERM_RED : TERM_BLUE));
                    this_lbl->setText(class_num);
                    continue;
                }


                if (this_name.contains(QString("combined_%1") .arg(i)))
                {
                    QString combined_num = format_stat(rc);
                    combined_num.append(" ");
                    combined_num = color_string(combined_num, (rc < 0 ? TERM_RED : TERM_BLUE));
                    this_lbl->setText(combined_num);
                    continue;
                }

                if (this_name.contains(QString("final_%1") .arg(i)))
                {
                    QString this_stat = (QString("<b>%1 </b>") .arg(cnv_stat(p_ptr->state.stat_loaded_max[i])));
                    this_stat = color_string(this_stat, TERM_BLUE);
                    this_lbl->setText(this_stat);
                    continue;
                }
            }

            // Update the race hitpoints
            if (this_name.operator ==("HD_Race"))
            {
                QString this_race = (QString("<b>%1 </b>") .arg(rp_ptr->r_mhp));
                this_lbl->setText(color_string(this_race, TERM_BLUE));
                continue;
            }

            // Update the class hitpoints
            if (this_name.operator ==("HD_Class"))
            {
                QString this_class = (QString("<b>%1 </b>") .arg(cp_ptr->c_mhp));
                this_lbl->setText(color_string(this_class, TERM_BLUE));
                continue;
            }

            // Update the combined hitpoints
            if (this_name.operator ==("HD_Both"))
            {
                QString this_both = (QString("<b>%1 </b>") .arg(rp_ptr->r_mhp + cp_ptr->c_mhp));
                this_lbl->setText(color_string(this_both, TERM_BLUE));
                continue;
            }

            // Update the race %
            if (this_name.operator ==("XP_Race"))
            {
                QString this_race = (QString("<b>%1%</b>") .arg(rp_ptr->r_exp));
                this_lbl->setText(color_string(this_race, TERM_BLUE));
                continue;
            }

            // Update the class %
            if (this_name.operator ==("XP_Class"))
            {
                QString this_class = (QString("<b>%1%</b>") .arg(cp_ptr->c_exp));
                this_lbl->setText(color_string(this_class, TERM_BLUE));
                continue;
            }

            // Update the combined %
            if (this_name.operator ==("XP_Both"))
            {
                QString this_both = (QString("<b>%1%</b>") .arg(rp_ptr->r_exp + cp_ptr->c_exp));
                this_lbl->setText(color_string(this_both, TERM_BLUE));
                continue;
            }

            // Update the points spent
            if (this_name.operator ==("points_spent"))
            {
                QString this_points = (QString("<b>Points Spent: %1/24</b>") .arg(points_spent));
                this_lbl->setText(color_string(this_points, TERM_BLUE));
                if (birth_point_based) this_lbl->show();
                else this_lbl->hide();
                continue;
            }

            // Update the points spent
            if (this_name.operator ==("points_left"))
            {
                QString this_points = (QString("<b>Points Left:  %1/24</b>") .arg(POINTS_LEFT));
                this_lbl->setText(color_string(this_points, (POINTS_LEFT ? TERM_BLUE : TERM_RED)));
                if (birth_point_based) this_lbl->show();
                else this_lbl->hide();
                continue;
            }
            // Update the player history
            if (this_name.operator ==("PLYR_Hist"))
            {
                this_lbl->setText(color_string(QString("<b>%1</b>") .arg(p_ptr->history), TERM_BLUE));
                continue;
            }
        }
    }

    QList<QPushButton *> push_list = this->findChildren<QPushButton *>();
    for (int x = 0; x < push_list.size(); x++)
    {
        QPushButton *this_push = push_list.at(x);

        QString this_name = this_push->objectName();

        //Not a named label
        if (!this_name.length()) continue;

        // Update the points spent
        if (this_name.operator ==("roll_char"))
        {
            if (!birth_point_based) this_push->show();
            else this_push->hide();
            continue;
        }
    }

    // This spin boxex need updating when a new race/class is selected
    QList<QSpinBox *> spin_list = this->findChildren<QSpinBox *>();

    for (int x = 0; x < spin_list.size(); x++)
    {
        QSpinBox *this_spinner = spin_list.at(x);

        QString this_name = this_spinner->objectName();

        //Not a named label
        if (!this_name.contains("spinner_")) continue;

        this_name.remove("spinner_");

        int this_stat = this_name.toInt();

        this_spinner->setValue(stats[this_stat]);
    }
}

/* Find out which spinner was selected, see in the stat can be increased
 * or decreased, change the spinner back if it cannot be changed.
 * update character if it can be channged
 */
void PlayerBirth::stat_spin_changed(int new_value)
{
    QWidget *button = dynamic_cast<QWidget *>(sender());

    QString obj_name = button->objectName();

    QList<QSpinBox *> spin_list = this->findChildren<QSpinBox *>();

    for (int x = 0; x < spin_list.size(); x++)
    {
        QSpinBox *this_spinner = spin_list.at(x);

        QString this_name = this_spinner->objectName();

        if (!obj_name.contains(this_name)) continue;

        this_name.remove("spinner_");

        int this_stat = this_name.toInt();

        int old_value = stats[this_stat];

        // Sell stat won't fail with the spiner paramaters.
        if (old_value > new_value)
        {
            sell_stat(this_stat);
            update_character(FALSE, FALSE);
            return;
        }
        else if (old_value < new_value)
        {
            if (buy_stat(this_stat))
            {
                update_character(FALSE, FALSE);
                return;
            }
            else
            {
                this_spinner->setValue(old_value);
                return;
            }
        }
    }
}

// The layout here needs to be consistent with update_vlay_stats_info above
void PlayerBirth::add_stat_boxes(QVBoxLayout *return_layout)
{
    QLabel *stat_box_label = new QLabel("<h2>Stat Adjustments</h2>");
    //stat_box_label->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Minimum);
    return_layout->addWidget(stat_box_label, 0, Qt::AlignCenter);

    QGridLayout *grid_stat_modifiers = new QGridLayout;
    return_layout->addLayout(grid_stat_modifiers);

    int row = 0;
    int col = 0;

    QLabel *stat_header = new QLabel;
    make_standard_label(stat_header, "STAT   ", TERM_DARK);
    grid_stat_modifiers->addWidget(stat_header, row, col++, Qt::AlignLeft);
    QLabel *race_adj_header = new QLabel;
    make_standard_label(race_adj_header, "   RA ", TERM_DARK);
    race_adj_header->setToolTip("Adjustments due to player race");
    grid_stat_modifiers->addWidget(race_adj_header, row, col++, Qt::AlignRight);
    QLabel *class_adj_header = new QLabel;
    make_standard_label(class_adj_header, "   CA ", TERM_DARK);
    class_adj_header->setToolTip("Adjustments due to player class");
    grid_stat_modifiers->addWidget(class_adj_header, row, col++, Qt::AlignRight);
    QLabel *all_adj_header = new QLabel;
    make_standard_label(all_adj_header, "   TA ", TERM_DARK);
    all_adj_header->setToolTip("Total Adjustments");
    grid_stat_modifiers->addWidget(all_adj_header, row, col++, Qt::AlignRight);

    row++;

    for (int i = 0; i < A_MAX; i++)
    {
        col = 0;

        // Stat label
        QLabel *stat_label = new QLabel();
        make_standard_label(stat_label, stat_names[i], TERM_DARK);
        stat_label->setToolTip(stat_entry(i));
        grid_stat_modifiers->addWidget(stat_label, row, col++, Qt::AlignLeft);

        int r = rp_ptr->r_adj[i];
        int c = cp_ptr->c_adj[i];
        int rc = r + c;

        QLabel *race_adj = new QLabel();
        make_standard_label(race_adj, (QString("   %1 ") .arg(format_stat(r))), (r < 0 ? TERM_RED : TERM_BLUE));
        race_adj->setObjectName(QString("race_%1") .arg(i));
        grid_stat_modifiers->addWidget(race_adj, row, col++, Qt::AlignRight);


        QLabel *class_adj = new QLabel();
        make_standard_label(class_adj, (QString("   %1 ") .arg(format_stat(c))), (c < 0 ? TERM_RED : TERM_BLUE));
        class_adj->setObjectName(QString("class_%1") .arg(i));
        grid_stat_modifiers->addWidget(class_adj, row, col++, Qt::AlignRight);

        QLabel *combined_adj = new QLabel();
        make_standard_label(combined_adj, (QString("   %1 ") .arg(format_stat(rc))), (rc < 0 ? TERM_RED : TERM_BLUE));
        combined_adj->setObjectName(QString("combined_%1") .arg(i));
        grid_stat_modifiers->addWidget(combined_adj, row++, col, Qt::AlignRight);
    }


    // Add a little space
    QLabel *dummy_label = new QLabel(" ");
    grid_stat_modifiers->addWidget(dummy_label, row++, 0, Qt::AlignLeft);
    QLabel *dummy_label2 = new QLabel(" ");
    grid_stat_modifiers->addWidget(dummy_label2, row++, 0, Qt::AlignLeft);

    col = 0;

    // Display character hit dice
    QLabel *hit_die_label = new QLabel();
    make_standard_label(hit_die_label, "Hit Die:", TERM_DARK);
    hit_die_label->setToolTip(get_help_topic("character_info", "Hit Die"));
    grid_stat_modifiers->addWidget(hit_die_label, row, col++, Qt::AlignLeft);

    QString this_race;
    QString this_class;
    QString this_both;

    this_race.setNum(rp_ptr->r_mhp);
    this_class.setNum(cp_ptr->c_mhp);
    this_both.setNum(rp_ptr->r_mhp + cp_ptr->c_mhp);
    this_race.append(" ");
    this_class.append(" ");
    this_both.append(" ");

    // HD Race
    QLabel *hit_die_race = new QLabel();
    make_standard_label(hit_die_race, this_race, TERM_BLUE);
    hit_die_race->setObjectName("HD_Race");
    grid_stat_modifiers->addWidget(hit_die_race, row, col++, Qt::AlignRight);

    // HD class
    QLabel *hit_die_class = new QLabel;
    make_standard_label(hit_die_class, this_class, TERM_BLUE);
    hit_die_class->setObjectName("HD_Class");
    grid_stat_modifiers->addWidget(hit_die_class, row, col++, Qt::AlignRight);

    // HD combined
    QLabel *hit_die_both = new QLabel;
    make_standard_label(hit_die_both, this_both, TERM_BLUE);
    hit_die_both->setObjectName("HD_Both");
    grid_stat_modifiers->addWidget(hit_die_both, row, col++, Qt::AlignRight);

    col = 0;
    row++;

    // Display experience percent
    QLabel *exp_pct_label = new QLabel();
    make_standard_label(exp_pct_label, "Experience:", TERM_DARK);
    exp_pct_label->setToolTip(get_help_topic("character_info", "Experience Factor"));
    grid_stat_modifiers->addWidget(exp_pct_label, row, col++, Qt::AlignLeft);

    this_race = (QString(" %1%") .arg(rp_ptr->r_exp));
    this_class = (QString(" %1%") .arg(cp_ptr->c_exp));
    this_both = (QString(" %1%") .arg(rp_ptr->r_exp + cp_ptr->c_exp));

    // HD Race
    QLabel *exp_pct_race = new QLabel();
    make_standard_label(exp_pct_race, this_race, TERM_BLUE);
    exp_pct_race->setObjectName("XP_Race");
    grid_stat_modifiers->addWidget(exp_pct_race, row, col++, Qt::AlignRight);

    // HD class
    QLabel *exp_pct_class = new QLabel;
    make_standard_label(exp_pct_class, this_class, TERM_BLUE);
    exp_pct_class->setObjectName("XP_Class");
    grid_stat_modifiers->addWidget(exp_pct_class, row, col++, Qt::AlignRight);

    // HD combined
    QLabel *exp_pct_both = new QLabel;
    make_standard_label(exp_pct_both, this_both, TERM_BLUE);
    exp_pct_both->setObjectName("XP_Both");
    grid_stat_modifiers->addWidget(exp_pct_both, row, col++, Qt::AlignRight);
}

// Note this really only works because we know there is only
// one layout inside the stat box.  A working universal function
// would use recursion to delete all widgets inside all layouts.
void PlayerBirth::redo_stat_box(void)
{
    QLayoutItem *child;
    while ((child = vlay_stats_current->takeAt(0)) != 0)
    {
        if(child->layout() != 0)
        {
            //Assumes only widgets inside second layout
            QLayoutItem *child2;
            while ((child2 = child->layout()->takeAt(0)) != 0)
            {
                child2->widget()->hide();
                delete child2->widget();

                delete child2;
            }
        }
        else if(child->widget() != 0)
        {
            child->widget()->hide();
            delete child->widget();
        }
        delete child;
    }

    grid_stat_results = NULL;

    add_stat_results();
}

// The layout here needs to be consistent with update_vlay_stats_info above
void PlayerBirth::add_stat_results(void)
{
    QLabel *stat_box_label = new QLabel("<h2>Current Stats</h2>");
    //stat_box_label->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Minimum);
    vlay_stats_current->addWidget(stat_box_label, 0, Qt::AlignCenter);

    grid_stat_results = new QGridLayout;
    vlay_stats_current->addLayout(grid_stat_results);

    int row = 0;
    int col = 0;

    QLabel *stat_header = new QLabel;
    make_standard_label(stat_header, "STAT   ", TERM_DARK);
    grid_stat_results->addWidget(stat_header, row, col++, Qt::AlignLeft);

    QLabel *base_header = new QLabel;
    make_standard_label(base_header, " BASE ", TERM_DARK);
    base_header->setToolTip("Base stat before all adjustments");
    grid_stat_results->addWidget(base_header, row, col++, Qt::AlignRight);

    if (birth_maximize)
    {
        QLabel *race_adj_header = new QLabel;
        make_standard_label(race_adj_header, "   RA ", TERM_DARK);
        race_adj_header->setToolTip("Adjustments due to player race");
        grid_stat_results->addWidget(race_adj_header, row, col++, Qt::AlignRight);
        QLabel *class_adj_header = new QLabel;
        make_standard_label(class_adj_header, " CA ", TERM_DARK);
        class_adj_header->setToolTip("Adjustments due to player class");
        grid_stat_results->addWidget(class_adj_header, row, col++, Qt::AlignRight);
    }
    QLabel *all_adj_header = new QLabel;
    make_standard_label(all_adj_header, " FINAL ", TERM_DARK);
    all_adj_header->setToolTip("Final stat after all adjustments");
    grid_stat_results->addWidget(all_adj_header, row, col++, Qt::AlignRight);

    row++;

    for (int i = 0; i < A_MAX; i++)
    {
        col = 0;

        // Stat label
        QLabel *stat_label = new QLabel();
        make_standard_label(stat_label, stat_names[i], TERM_DARK);
        stat_label->setToolTip(stat_entry(i));
        grid_stat_results->addWidget(stat_label, row, col++, Qt::AlignLeft);

        int r = rp_ptr->r_adj[i];
        int c = cp_ptr->c_adj[i];

        if (birth_point_based)
        {
            QSpinBox *this_spinner = new QSpinBox;
            int max_value = (birth_maximize ? 18 : 17);
            this_spinner->setRange(10, max_value);
            this_spinner->setValue(stats[i]);
            this_spinner->setObjectName(QString("spinner_%1") .arg(i));
            connect(this_spinner, SIGNAL(valueChanged(int)), this, SLOT(stat_spin_changed(int)));
            grid_stat_results->addWidget(this_spinner, row, col++, Qt::AlignRight);
        }
        else
        {
            QLabel *base_stat = new QLabel();
            make_standard_label(base_stat, (QString("<b>   %1 </b>") .arg(cnv_stat(stats[i]))), TERM_BLUE);
            base_stat->setObjectName(QString("base_%1") .arg(i));
            grid_stat_results->addWidget(base_stat, row, col++, Qt::AlignRight);
        }

        if (birth_maximize)
        {
            QLabel *race_adj = new QLabel();
            make_standard_label(race_adj, (QString("   %1 ") .arg(format_stat(r))), (r < 0 ? TERM_RED : TERM_BLUE));
            race_adj->setObjectName(QString("race_%1") .arg(i));
            grid_stat_results->addWidget(race_adj, row, col++, Qt::AlignRight);

            QLabel *class_adj = new QLabel();
            make_standard_label(class_adj, (QString("   %1 ") .arg(format_stat(c))), (c < 0 ? TERM_RED : TERM_BLUE));
            class_adj->setObjectName(QString("class_%1") .arg(i));
            grid_stat_results->addWidget(class_adj, row, col++, Qt::AlignRight);
        }

        QLabel *stat_max = new QLabel();
        make_standard_label(stat_max, (QString("   %1 ") .arg(cnv_stat(p_ptr->state.stat_loaded_max[i]))), TERM_BLUE);
        stat_max->setObjectName(QString("final_%1") .arg(i));
        grid_stat_results->addWidget(stat_max, row++, col, Qt::AlignRight);
    }

    vlay_stats_current->addStretch(1);
}


// Find the button changed, and update the option value
void PlayerBirth::option_changed(int index)
{

    QList<QAbstractButton *> ops  = group_options->buttons();
    for (int i = 0; i < ops.size(); i++)
    {
        QAbstractButton *chk = ops.at(i);

        int id = group_options->id(chk);
        if (id != index) continue;
        op_ptr->opt[index] = chk->isChecked();
        break;
    }

    //Reset the stats if birth_maximize has been selected
    if (index == OPT_birth_maximize)
    {
        if (birth_point_based)
        {
            //hack - pretend point based is being selected
            birth_point_based = FALSE;
            QRadioButton *btn = this->findChild<QRadioButton *>("point_radio");
            btn->click();
        }
        else
        {
            // hack - pretend random roller is being selected
            birth_point_based = TRUE;
            QRadioButton *btn = this->findChild<QRadioButton *>("roller_radio");
            btn->click();
        }
    }

    if (index == OPT_birth_money)
    {
        get_money();
        update_screen();
    }
}

void PlayerBirth::call_options_dialog()
{
    OptionsDialog *dlg = new OptionsDialog;
    dlg->exec();
    delete dlg;

    // now update the birth checkboxes
    QList<QAbstractButton *> ops  = group_options->buttons();
    for (int i = 0; i < ops.size(); i++)
    {
        QAbstractButton *chk = ops.at(i);

        int id = group_options->id(chk);
        chk->setChecked(op_ptr->opt[id]);
    }
}


// Add a box for all birth options
void PlayerBirth::add_option_boxes(QVBoxLayout *return_layout)
{
    group_options = new QButtonGroup(this);
    group_options->setExclusive(FALSE);

    QLabel *options_label = new QLabel("<h2>Birth Options</h2>");
    options_label->setToolTip("Birth Options must be changed before completing character<br>in order to apply to the current game.");
    //options_label->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Minimum);
    return_layout->addWidget(options_label, 0, Qt::AlignCenter);

    for (int i = 0; i < OPT_PAGE_PER; i++)
    {
        byte idx;
        if (game_mode == GAME_NPPANGBAND)
        {
            idx = option_page_nppangband[OPT_PAGE_BIRTH][i];
        }
        else //GAME_NPPMORIA
        {
            idx = option_page_nppmoria[OPT_PAGE_BIRTH][i];
        }
        if (idx == OPT_NONE) continue;

        option_entry *opt_ptr = &options[idx];
        if (opt_ptr->name.isEmpty()) continue;

        QCheckBox *this_checkbox = new QCheckBox(opt_ptr->description);
        this_checkbox->setChecked(op_ptr->opt[idx]);
        this_checkbox->setToolTip(get_help_topic(QString("option_info"), opt_ptr->name));
        group_options->addButton(this_checkbox, idx);

        return_layout->addWidget(this_checkbox);
    }

    connect(group_options, SIGNAL(buttonClicked(int)), this, SLOT(option_changed(int)));
}

void PlayerBirth::add_info_boxes(QVBoxLayout *return_layout)
{
    // add race description
    race_info = new QLabel();
    QString race_help = get_help_topic(QString("race_class_info"), p_info[cur_race].pr_name);
    race_info->setText(QString("%1<br><br>") .arg(race_help));
    race_info->setWordWrap(TRUE);
    race_info->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Minimum);
    return_layout->addWidget(race_info);

    // add class description
    class_info = new QLabel();
    QString class_help = get_help_topic(QString("race_class_info"), c_info[cur_class].cl_name);
    class_info->setText(QString("%1<br>") .arg(class_help));
    class_info->setWordWrap(TRUE);
    class_info->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Minimum);
    return_layout->addWidget(class_info);
}


void PlayerBirth::point_button_chosen()
{
    //Already chosen
    if (birth_point_based) return;

    birth_point_based = TRUE;
    generate_player(TRUE);
    reset_stats();
    generate_stats();
    update_character(TRUE, FALSE);
    redo_stat_box();
}

void PlayerBirth::random_button_chosen()
{
    // Already chosen
    if (!birth_point_based) return;

    birth_point_based = FALSE;
    reset_stats();
    random_roll();
    redo_stat_box();
}

void PlayerBirth::random_roll(void)
{
    roll_player();
    update_character(TRUE, FALSE);
}

void PlayerBirth::add_stat_choices(QVBoxLayout *return_layout)
{
    group_stat_choice = new QButtonGroup(this);
    group_stat_choice->setExclusive(TRUE);

    QLabel *stat_choice_label = new QLabel("<h2>Stat Gen Method</h2>");
    return_layout->addWidget(stat_choice_label, Qt::AlignLeft);

    QRadioButton *point_radiobutton = new QRadioButton("Point Based");
    if (birth_point_based) point_radiobutton->setChecked(TRUE);
    else point_radiobutton->setChecked(FALSE);
    point_radiobutton->setObjectName("point_radio");
    group_stat_choice->addButton(point_radiobutton, TRUE);
    return_layout->addWidget(point_radiobutton, Qt::AlignLeft);
    connect(point_radiobutton, SIGNAL(clicked()), this, SLOT(point_button_chosen()));

    QRadioButton *roller_radiobutton = new QRadioButton("Random Roller");
    if (birth_point_based) roller_radiobutton->setChecked(FALSE);
    else roller_radiobutton->setChecked(TRUE);
    roller_radiobutton->setObjectName("roller_radio");
    group_stat_choice->addButton(roller_radiobutton, FALSE);
    return_layout->addWidget(roller_radiobutton, Qt::AlignLeft);
    connect(roller_radiobutton, SIGNAL(clicked()), this, SLOT(random_button_chosen()));

    return_layout->addStretch(1);

    QLabel *lab_points_spent = new QLabel;
    make_standard_label(lab_points_spent, (QString("Points Spent: %1/24") .arg(points_spent)), TERM_BLUE);
    lab_points_spent->setObjectName("points_spent");
    lab_points_spent->setToolTip("Increasing a stat by 1 between 10 and 16 costs 1 point.<br>Increasing a stat from 16 to 17 costs 2 points.4<br>Increasing a stat from 17 to 18 costs 4 points.");
    return_layout->addWidget(lab_points_spent, Qt::AlignLeft);
    if (!birth_point_based) lab_points_spent->hide();

    QLabel *points_left = new QLabel;
    make_standard_label(points_left, (QString("Points Left:  %1/24") .arg(POINTS_LEFT)), TERM_BLUE);
    points_left->setObjectName("points_left");
    points_left->setToolTip("The player receives an extra 50 gold pieces at startup for each unused point.");
    return_layout->addWidget(points_left, Qt::AlignLeft);
    if (birth_point_based) points_left->hide();

    QPushButton *roll_char = new QPushButton("Roll Character");
    roll_char->setObjectName("roll_char");
    roll_char->setToolTip("Press to roll your player characteristics.");
    connect(roll_char, SIGNAL(clicked()), this, SLOT(random_roll()));
    return_layout->addWidget(roll_char, Qt::AlignLeft);
    if (birth_point_based) roll_char->hide();
}

void PlayerBirth::class_changed(int new_class)
{
    p_ptr->pclass = cur_class = new_class;
    QString class_help = get_help_topic(QString("race_class_info"), c_info[cur_class].cl_name);
    class_info->setText(QString("%1<br>") .arg(class_help));
    update_character(TRUE, TRUE);
}

void PlayerBirth::add_classes(QVBoxLayout *return_layout)
{
    group_class = new QButtonGroup(this);
    group_class->setExclusive(TRUE);

    QLabel *class_label = new QLabel("<h2>Player Class</h2>");
    class_label->setToolTip(get_help_topic("race_class_info", "Classes"));
    return_layout->addWidget(class_label, 0, Qt::AlignCenter);

    for (int i = 0; i < z_info->c_max; i++)
    {
        QRadioButton *this_radiobutton = new QRadioButton(c_info[i].cl_name);
        if (i == cur_class) this_radiobutton->setChecked(TRUE);
        else this_radiobutton->setChecked(FALSE);
        group_class->addButton(this_radiobutton, i);
        return_layout->addWidget(this_radiobutton);
    }

    connect(group_class, SIGNAL(buttonClicked(int)), this, SLOT(class_changed(int)));
}

void PlayerBirth::race_changed(int new_race)
{
    p_ptr->prace = cur_race = new_race;

    QString race_help = get_help_topic(QString("race_class_info"), p_info[cur_race].pr_name);
    race_info->setText(QString("%1<br><br>") .arg(race_help));
    update_character(TRUE, TRUE);
}

void PlayerBirth::add_races(QVBoxLayout *return_layout)
{
    group_race = new QButtonGroup(this);
    group_race->setExclusive(TRUE);

    QLabel *race_label = new QLabel("<h2>Player Race</h2>");
    race_label->setToolTip(get_help_topic("race_class_info", "races"));
    return_layout->addWidget(race_label, 0, Qt::AlignCenter);

    for (int i = 0; i < z_info->p_max; i++)
    {
        QRadioButton *this_radiobutton = new QRadioButton(p_info[i].pr_name);
        if (i == cur_race) this_radiobutton->setChecked(TRUE);
        else this_radiobutton->setChecked(FALSE);
        group_race->addButton(this_radiobutton, i);
        return_layout->addWidget(this_radiobutton);
    }

    connect(group_race, SIGNAL(buttonClicked(int)), this, SLOT(race_changed(int)));  
}

void PlayerBirth::gender_changed(int new_gender)
{
    p_ptr->psex = cur_gender = new_gender;
    update_character(TRUE, FALSE);
}

void PlayerBirth::name_changed(QString new_name)
{
    if (new_name.contains("'"))
    {
        pop_up_message_box("The character name can not contain an apostrophe.");
        new_name.remove("'");
    }

    if (!new_name.length()) return;

    op_ptr->full_name = cur_name = new_name;
    update_screen();
}

// Generate a name at random
void PlayerBirth::random_name(void)
{
    cur_name = make_random_name(4, 12);
    player_name->setText(cur_name);

};
//Select a random gender
void PlayerBirth::random_gender(void)
{
    int x = group_gender->buttons().count();
    group_gender->buttons().at(rand_int(x))->click();
};
//select a random race
void PlayerBirth::random_race(void)
{
    int x = group_race->buttons().count();
    group_race->buttons().at(rand_int(x))->click();

};
// select a random class
void PlayerBirth::random_class(void)
{
    int x = group_class->buttons().count();
    group_class->buttons().at(rand_int(x))->click();
};
// select everything randomly
void PlayerBirth::random_all(void)
{
    hold_update = TRUE;
    random_name();
    random_gender();
    random_race();
    random_class();
    hold_update = FALSE;
    update_character(TRUE, TRUE);
};

void PlayerBirth::char_name_label(QGridLayout *return_layout)
{
    QLabel *player_name = new QLabel;
    make_standard_label(player_name, "NAME:", TERM_DARK);
    return_layout->addWidget(player_name, 0, 0, Qt::AlignLeft);
}

// Add player names, buttons, and gender
void PlayerBirth::add_genders(QVBoxLayout *return_layout)
{
    QLabel *name_label = new QLabel("<h2>Charater Name</h2>");
    return_layout->addWidget(name_label, 0, Qt::AlignCenter);

    if (op_ptr->full_name.length()) cur_name = op_ptr->full_name;
    else op_ptr->full_name = cur_name = "Player";
    player_name = new QLineEdit(cur_name);
    player_name->setText(cur_name);
    connect(player_name, SIGNAL(textChanged(QString)), this, SLOT(name_changed(QString)));
    return_layout->addWidget(player_name, Qt::AlignLeft);

    group_gender = new QButtonGroup(this);
    group_gender->setExclusive(TRUE);

    QLabel *gender_label = new QLabel("<h2>Player Gender</h2>");
    return_layout->addWidget(gender_label, 0, Qt::AlignCenter);

    for (int i = 0; i < MAX_SEXES; i++)
    {
        QRadioButton *this_radiobutton = new QRadioButton(sex_info[i].title);
        if (i == cur_gender) this_radiobutton->setChecked(TRUE);
        else this_radiobutton->setChecked(FALSE);
        group_gender->addButton(this_radiobutton, i);
        return_layout->addWidget(this_radiobutton);
    }

    connect(group_gender, SIGNAL(buttonClicked(int)), this, SLOT(gender_changed(int)));

    QLabel *gender_expl = new QLabel(color_string(QString("<b>Gender does not have any significant gameplay effects.</b>"), TERM_BLUE));
    gender_expl->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::MinimumExpanding);
    gender_expl->setWordWrap(TRUE);
    return_layout->addWidget(gender_expl);

    QPushButton *rand_name = new QPushButton("Random Name");
    return_layout->addWidget(rand_name, 0, Qt::AlignLeft);
    connect(rand_name, SIGNAL(clicked()), this, SLOT(random_name()));

    QPushButton *rand_gender = new QPushButton("Random Gender");
    return_layout->addWidget(rand_gender, 0, Qt::AlignLeft);
    connect(rand_gender, SIGNAL(clicked()), this, SLOT(random_gender()));

    QPushButton *rand_race = new QPushButton("Random Race");
    return_layout->addWidget(rand_race, 0, Qt::AlignLeft);
    connect(rand_race, SIGNAL(clicked()), this, SLOT(random_race()));

    QPushButton *rand_class = new QPushButton("Random Class");
    return_layout->addWidget(rand_class, 0, Qt::AlignLeft);
    connect(rand_class, SIGNAL(clicked()), this, SLOT(random_class()));

    QPushButton *rand_all = new QPushButton("Random Character");
    rand_all->setToolTip("Randomly select the character's name, gender, race, and class.");
    return_layout->addWidget(rand_all, 0, Qt::AlignLeft);
    connect(rand_all, SIGNAL(clicked()), this, SLOT(random_all()));

    QPushButton *all_options = new QPushButton("All Options");
    return_layout->addWidget(all_options, 0, Qt::AlignLeft);
    connect(all_options, SIGNAL(clicked()), this, SLOT(call_options_dialog()));
}

// Get all new history,completely redo the character
void PlayerBirth::update_character(bool new_player, bool needs_stat_update)
{
    // prevent updating more than once per change, if necessary;
    if (hold_update) return;

    p_ptr->prace = cur_race;
    p_ptr->pclass = cur_class;
    p_ptr->psex = cur_gender;

    if (new_player) generate_player(TRUE);

    if (needs_stat_update)
    {
        if (birth_point_based)
        {
            reset_stats();
            generate_stats();
        }
        else roll_player();
    }

    calc_bonuses(inventory, &p_ptr->state, FALSE);
    calc_stealth();
    update_screen();
}

// Update everything onscreen
void PlayerBirth::update_screen(void)
{
    update_stats_info();
    update_char_screen(central, ui_message_window_font());
}


// Note this needs to be called after hlay_choices is finished.
void PlayerBirth::setup_character()
{
    init_birth();

    for (int i = 0; i < A_MAX; i++)
    {
        stats[i] = p_ptr->stat_birth[i];
    }
    if (quick_start && has_prev_character())
    {
        load_prev_character();
        cur_name = op_ptr->full_name;
        cur_gender = p_ptr->psex;
        cur_race = p_ptr->prace;
        cur_class = p_ptr->pclass;
        for (int i = 0; i < A_MAX; i++)
        {
            stats[i] = p_ptr->stat_birth[i];
        }
        generate_player(FALSE);

    }
    else
    {
        p_ptr->prace = cur_race = 0;
        p_ptr->pclass = cur_class = 0;
        p_ptr->psex = cur_gender = 0;
        generate_player(TRUE);
        if (birth_point_based)
        {
            reset_stats();
            generate_stats();
        }
        else roll_player();

    }

    calc_bonuses(inventory, &p_ptr->state, false);
    calc_stealth();


}


// Build the birth dialog
PlayerBirth::PlayerBirth(bool quickstart): NPPDialog()
{
    quick_start = quickstart;
    hold_update = FALSE;

    central = new QWidget;
    QVBoxLayout *main_layout = new QVBoxLayout;
    central->setLayout(main_layout);
    main_layout->setSpacing(10);
    // IMPORTANT: it must be called AFTER setting the layout
    this->setClient(central);

    // Setup the character, only after hlay choices are completed
    setup_character();

    // Title Box
    QLabel *main_prompt = new QLabel(QString("<h2>Character Creation %1 %2 </h2>") .arg(VERSION_MODE_NAME) .arg(VERSION_STRING));
    main_layout->addWidget(main_prompt, 0, Qt::AlignCenter);
    QHBoxLayout *hlay_choices = new QHBoxLayout;
    main_layout->addLayout(hlay_choices);

    // Add a box for options
    QVBoxLayout *vlay_options = new QVBoxLayout;
    hlay_choices->addLayout(vlay_options);
    add_option_boxes(vlay_options);
    vlay_options->addStretch(1);

    // Add gender column and buttons
    QVBoxLayout *vlay_gender = new QVBoxLayout;
    hlay_choices->addLayout(vlay_gender);
    add_genders(vlay_gender);
    vlay_gender->addStretch(1);

    // Add race column
    QVBoxLayout *vlay_race  = new QVBoxLayout;
    hlay_choices->addLayout(vlay_race);
    add_races(vlay_race);
    vlay_race->addStretch(1);

    //Add class column
    QVBoxLayout *vlay_class  = new QVBoxLayout;
    hlay_choices->addLayout(vlay_class);
    add_classes(vlay_class);
    vlay_class->addStretch(1);

    QVBoxLayout *vlay_help_area = new QVBoxLayout;
    hlay_choices->addLayout(vlay_help_area, 1);
    add_info_boxes(vlay_help_area);
    vlay_help_area->addStretch(1);

    QVBoxLayout *vlay_stats_info_area = new QVBoxLayout;
    hlay_choices->addLayout(vlay_stats_info_area);
    add_stat_boxes(vlay_stats_info_area);
    vlay_stats_info_area->addStretch(1);



    QHBoxLayout *hlay_info = new QHBoxLayout;
    main_layout->addLayout(hlay_info);

    QVBoxLayout *vlay_char_basic = new QVBoxLayout;
    glay_char_basic = new QGridLayout;
    char_name_label(glay_char_basic);
    char_basic_info(glay_char_basic);
    vlay_char_basic->addLayout(glay_char_basic);
    vlay_char_basic->addStretch(1);
    hlay_info->addLayout(vlay_char_basic);
    hlay_info->addStretch(1);

    QVBoxLayout *vlay_char_data = new QVBoxLayout;
    glay_char_data = new QGridLayout;
    char_basic_data(glay_char_data);
    vlay_char_data->addLayout(glay_char_data);
    vlay_char_data->addStretch(1);
    hlay_info->addLayout(vlay_char_data);
    hlay_info->addStretch(1);

    QVBoxLayout *vlay_ability_info = new QVBoxLayout;
    glay_ability_info = new QGridLayout;
    char_ability_info(glay_ability_info);
    vlay_ability_info->addLayout(glay_ability_info);
    vlay_ability_info->addStretch(1);
    hlay_info->addLayout(vlay_ability_info);
    hlay_info->addStretch(1);

    QVBoxLayout *vlay_stat_choice = new QVBoxLayout;
    add_stat_choices(vlay_stat_choice);
    vlay_stat_choice->addStretch(1);
    hlay_info->addLayout(vlay_stat_choice);
    hlay_info->addStretch(1);

    vlay_stats_current = new QVBoxLayout;
    hlay_info->addLayout(vlay_stats_current);
    add_stat_results();

    // Add player history
    QVBoxLayout *history_box = new QVBoxLayout;
    main_layout->addStretch(1);
    main_layout->addLayout(history_box);
    QLabel *history = new QLabel();
    make_standard_label(history, (QString("<b>%1</b>") .arg(p_ptr->history)), TERM_BLUE);
    history->setObjectName("PLYR_Hist");
    history_box->addWidget(history);
    main_layout->addStretch(1);

    update_char_screen(central, ui_message_window_font());

    //Add a close button on the right side
    QDialogButtonBox *buttons = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel);
    connect(buttons, SIGNAL(rejected()), this, SLOT(reject_char()));
    connect(buttons, SIGNAL(accepted()), this, SLOT(accept_char()));
    main_layout->addWidget(buttons);

    this->clientSizeUpdated();

    this->exec();
}
