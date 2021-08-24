/*
 * Copyright (c) 2015 Jeff Greene, Diego Gonzalez
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 3, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include <src/npp.h>
#include <src/qt_mainwindow.h>
#include <src/player_screen.h>
#include <QHeaderView>
#include <QFontDialog>
#include <QPushButton>



void MainWindow::name_change(void)
{
    QString new_name = get_string("Please enter a new name for your character.", "Enter new name", op_ptr->full_name);

    if (new_name.contains("'"))
    {
        pop_up_message_box("The character name can not contain an apostrophe.");
        new_name.remove("'");
    }

    if (!new_name.length()) return;

    op_ptr->full_name = new_name;
    win_char_info_basic_update();

}

void MainWindow::name_change_pushbutton(QGridLayout *return_layout)
{
    QPushButton *label_player_name = new QPushButton("NAME:");
    QPalette pushbutton_palette;
    pushbutton_palette.setColor(QPalette::ButtonText, defined_colors[TERM_DARK]);
    label_player_name->setPalette(pushbutton_palette);
    label_player_name->setToolTip("Press to change player name");
    connect(label_player_name, SIGNAL(clicked()), this, SLOT(name_change()));
    return_layout->addWidget(label_player_name, 0, 0, Qt::AlignLeft);
}

void MainWindow::update_label_basic_font()
{
    QList<QLabel *> lbl_list = window_char_info_basic->findChildren<QLabel *>();
    for (int i = 0; i < lbl_list.size(); i++)
    {
        QLabel *this_lbl = lbl_list.at(i);
        this_lbl->setFont(font_char_basic_info);
    }
}

void MainWindow::set_font_char_info_basic(QFont newFont)
{
    font_char_basic_info = newFont;
    update_label_basic_font();

}

void MainWindow::win_char_info_basic_font()
{
    bool selected;
    QFont font = QFontDialog::getFont( &selected, font_char_basic_info, this);

    if (selected)
    {
        set_font_char_info_basic(font);
    }
}

// For when savefiles close but the game doesn't.
void MainWindow::win_char_info_basic_wipe()
{
    if (!show_char_info_basic) return;
    if (!character_generated) return;
    clear_layout(main_vlay_char_basic);
}

// Just update the score
void MainWindow::win_char_info_score()
{
    if (!character_generated) return;
    if (!show_char_info_basic) return;
    QList<QLabel *> lbl_list = window_char_info_basic->findChildren<QLabel *>();
    for (int i = 0; i < lbl_list.size(); i++)
    {
        QLabel *this_lbl = lbl_list.at(i);

        QString this_name = this_lbl->objectName();

        if (this_name.operator ==("PLYR_Score"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(number_to_formatted_string(p_ptr->current_score)), TERM_BLUE));
            return;
        }
    }

}

// Just update the two turncount labels
void MainWindow::win_char_info_turncount()
{
    if (!character_generated) return;
    if (!show_char_info_basic) return;

    bool turn_game = FALSE;
    bool turn_player = FALSE;

    QList<QLabel *> lbl_list = window_char_info_basic->findChildren<QLabel *>();
    for (int i = 0; i < lbl_list.size(); i++)
    {
        QLabel *this_lbl = lbl_list.at(i);

        QString this_name = this_lbl->objectName();

        if (this_name.operator ==("TURN_Game"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(number_to_formatted_string(p_ptr->game_turn)), TERM_BLUE));
            turn_game = TRUE;
            if (turn_player) return;
        }
        else if (this_name.operator ==("TURN_Player"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(number_to_formatted_string(p_ptr->p_turn)), TERM_BLUE));
            turn_player = TRUE;
            if (turn_game) return;
        }
    }

}

void MainWindow::win_char_info_basic_update()
{
    if (!character_generated) return;
    if (!show_char_info_basic) return;
    update_char_screen(window_char_info_basic, font_char_basic_info);
}

void MainWindow::create_win_char_info()
{
    if (!character_generated) return;
    if (!show_char_info_basic) return;

    QHBoxLayout *char_info_basic_hlay = new QHBoxLayout;

    main_vlay_char_basic->addLayout(char_info_basic_hlay);

    QVBoxLayout *vlay_basic = new QVBoxLayout;
    char_info_basic_hlay->addLayout(vlay_basic);
    QGridLayout *basic_info = new QGridLayout;
    name_change_pushbutton(basic_info);
    char_basic_info(basic_info);
    vlay_basic->addLayout(basic_info);
    vlay_basic->addStretch(1);

    QVBoxLayout *vlay_data = new QVBoxLayout;
    char_info_basic_hlay->addLayout(vlay_data);
    QGridLayout *basic_data = new QGridLayout;
    char_basic_data(basic_data);
    vlay_data->addLayout(basic_data);
    vlay_data->addStretch(1);

    QVBoxLayout *vlay_game_info = new QVBoxLayout;
    char_info_basic_hlay->addLayout(vlay_game_info);
    QGridLayout *game_info = new QGridLayout;
    char_game_info(game_info);
    vlay_game_info->addLayout(game_info);
    vlay_game_info->addStretch(1);

    // Add player history
    // Title Box
    main_vlay_char_basic->addStretch(1);
    QLabel *history = new QLabel();
    make_standard_label(history, p_ptr->history, TERM_BLUE);
    main_vlay_char_basic->addWidget(history);
    main_vlay_char_basic->addStretch(1);

    QHBoxLayout *char_info_other_hlay = new QHBoxLayout;
    main_vlay_char_basic->addLayout(char_info_other_hlay);

    QVBoxLayout *vlay_combat_info = new QVBoxLayout;
    char_info_other_hlay->addLayout(vlay_combat_info);
    QGridLayout *combat_info = new QGridLayout;
    char_combat_info(combat_info);
    vlay_combat_info->addLayout(combat_info);
    vlay_combat_info->addStretch(1);

    QVBoxLayout *vlay_ability_info = new QVBoxLayout;
    char_info_other_hlay->addLayout(vlay_ability_info);
    QGridLayout *ability_info = new QGridLayout;
    char_ability_info(ability_info);
    vlay_ability_info->addLayout(ability_info);
    vlay_ability_info->addStretch(1);

    QVBoxLayout *vlay_stat_info = new QVBoxLayout;
    char_info_other_hlay->addLayout(vlay_stat_info);
    QGridLayout *stat_info = new QGridLayout;
    char_stat_info(stat_info);
    vlay_stat_info->addLayout(stat_info);
    vlay_stat_info->addStretch(1);

    update_char_screen(window_char_info_basic, font_char_basic_info);
    update_label_basic_font();
}

void MainWindow::close_win_char_info_frame(QObject *this_object)
{
    (void)this_object;
    window_char_info_basic = NULL;
    show_char_info_basic = FALSE;
    win_char_basic->setText("Show Basic Character Information");
}

/*
 *  Make the basic shell
 *  The game crashes if the labels are drawn before the character is created
 *  So that is filled after a character is created.
 */
void MainWindow::win_char_info_basic_create()
{
    window_char_info_basic = new QWidget();
    main_vlay_char_basic = new QVBoxLayout;
    window_char_info_basic->setLayout(main_vlay_char_basic);

    char_info_basic_menubar = new QMenuBar;
    main_vlay_char_basic->setMenuBar(char_info_basic_menubar);
    window_char_info_basic->setWindowTitle("Character Information - Basic");
    char_info_basic_settings = char_info_basic_menubar->addMenu(tr("&Settings"));
    char_info_basic_font = new QAction(tr("Set Basic Character Screen Font"), this);
    char_info_basic_font->setStatusTip(tr("Set the font for the Basic Character Information screen."));
    connect(char_info_basic_font, SIGNAL(triggered()), this, SLOT(win_char_info_basic_font()));
    char_info_basic_settings->addAction(char_info_basic_font);

    window_char_info_basic->setAttribute(Qt::WA_DeleteOnClose);
    connect(window_char_info_basic, SIGNAL(destroyed(QObject*)), this, SLOT(close_win_char_info_frame(QObject*)));
}

void MainWindow::win_char_info_basic_destroy()
{
    if (!show_char_info_basic) return;
    if (!window_char_info_basic) return;
    delete window_char_info_basic;
    window_char_info_basic = NULL;
}

void MainWindow::toggle_win_char_info_frame()
{
    if (!show_char_info_basic)
    {
        win_char_info_basic_create();
        show_char_info_basic = TRUE;
        create_win_char_info();
        win_char_basic->setText("Hide Basic Character Information");
        window_char_info_basic->show();
    }
    else
    {
        win_char_info_basic_destroy();
        show_char_info_basic = FALSE;
        win_char_basic->setText("Show Basic Character Information");
    }
}

