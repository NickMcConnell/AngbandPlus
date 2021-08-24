/*
 * Copyright (c) 2015 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
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

    p_ptr->redraw |= (PR_TITLEBAR);

}

void MainWindow::name_change_pushbutton(QGridLayout *return_layout)
{
    QPointer<QPushButton> label_player_name = new QPushButton("NAME:");
    QPalette pushbutton_palette;
    pushbutton_palette.setColor(QPalette::ButtonText, defined_colors[TERM_DARK]);
    label_player_name->setPalette(pushbutton_palette);
    label_player_name->setToolTip("Press to change player name");
    connect(label_player_name, SIGNAL(clicked()), this, SLOT(name_change()));
    return_layout->addWidget(label_player_name, 0, 0, Qt::AlignLeft);
}

void MainWindow::update_label_basic_font()
{
    QList<QLabel *> lbl_list = char_info_basic_settings.main_widget->findChildren<QLabel *>();
    for (int i = 0; i < lbl_list.size(); i++)
    {
        QLabel *this_lbl = lbl_list.at(i);
        this_lbl->setFont(char_info_basic_settings.win_font);
    }
}

void MainWindow::set_font_char_info_basic(QFont newFont)
{
    char_info_basic_settings.win_font = newFont;
    update_label_basic_font();

}

void MainWindow::win_char_info_basic_font()
{
    bool selected;
    QFont font = QFontDialog::getFont( &selected, char_info_basic_settings.win_font, this);

    if (selected)
    {
        set_font_char_info_basic(font);
    }
}

// For when savefiles close but the game doesn't.
void MainWindow::win_char_info_basic_wipe()
{
    if (!char_info_basic_settings.win_show) return;
    if (!character_generated) return;
    clear_layout(char_info_basic_settings.main_vlay);
}

// Just update the score
void MainWindow::win_char_info_score()
{
    if (!character_generated) return;
    if (!char_info_basic_settings.win_show) return;
    QList<QLabel *> lbl_list = char_info_basic_settings.main_widget->findChildren<QLabel *>();
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
    if (!char_info_basic_settings.win_show) return;

    bool turn_game = FALSE;
    bool turn_player = FALSE;

    QList<QLabel *> lbl_list = char_info_basic_settings.main_widget->findChildren<QLabel *>();
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
    if (!char_info_basic_settings.win_show) return;
    if (!char_info_basic_settings.main_vlay->count()) return;

    update_char_screen(char_info_basic_settings.main_widget, char_info_basic_settings.win_font);
}

void MainWindow::create_win_char_info()
{
    if (!character_generated) return;
    if (!char_info_basic_settings.win_show) return;

    QPointer<QHBoxLayout> char_info_basic_hlay = new QHBoxLayout;

    char_info_basic_settings.main_vlay->addLayout(char_info_basic_hlay);

    QVBoxLayout *vlay_basic = new QVBoxLayout;
    char_info_basic_hlay->addLayout(vlay_basic);
    QGridLayout *basic_info = new QGridLayout;
    name_change_pushbutton(basic_info);
    char_basic_info(basic_info);
    vlay_basic->addLayout(basic_info);
    vlay_basic->addStretch(1);

    QPointer<QVBoxLayout> vlay_data = new QVBoxLayout;
    char_info_basic_hlay->addLayout(vlay_data);
    QGridLayout *basic_data = new QGridLayout;
    char_basic_data(basic_data);
    vlay_data->addLayout(basic_data);
    vlay_data->addStretch(1);

    QPointer<QVBoxLayout> vlay_game_info = new QVBoxLayout;
    char_info_basic_hlay->addLayout(vlay_game_info);
    QGridLayout *game_info = new QGridLayout;
    char_game_info(game_info);
    vlay_game_info->addLayout(game_info);
    vlay_game_info->addStretch(1);

    // Add player history
    // Title Box
    char_info_basic_settings.main_vlay->addStretch(1);
    QPointer<QLabel> history = new QLabel();
    make_standard_label(history, p_ptr->history, TERM_BLUE);
    char_info_basic_settings.main_vlay->addWidget(history);
    char_info_basic_settings.main_vlay->addStretch(1);

    QPointer<QHBoxLayout> char_info_other_hlay = new QHBoxLayout;
    char_info_basic_settings.main_vlay->addLayout(char_info_other_hlay);

    QPointer<QVBoxLayout> vlay_combat_info = new QVBoxLayout;
    char_info_other_hlay->addLayout(vlay_combat_info);
    QGridLayout *combat_info = new QGridLayout;
    char_combat_info(combat_info);
    vlay_combat_info->addLayout(combat_info);
    vlay_combat_info->addStretch(1);

    QPointer<QVBoxLayout> vlay_ability_info = new QVBoxLayout;
    char_info_other_hlay->addLayout(vlay_ability_info);
    QGridLayout *ability_info = new QGridLayout;
    char_ability_info(ability_info);
    vlay_ability_info->addLayout(ability_info);
    vlay_ability_info->addStretch(1);

    QPointer<QVBoxLayout> vlay_stat_info = new QVBoxLayout;
    char_info_other_hlay->addLayout(vlay_stat_info);
    QGridLayout *stat_info = new QGridLayout;
    char_stat_info(stat_info);
    vlay_stat_info->addLayout(stat_info);
    vlay_stat_info->addStretch(1);

    update_char_screen(char_info_basic_settings.main_widget, char_info_basic_settings.win_font);
    update_label_basic_font();
}



/*
 *  Make the basic shell
 *  The game crashes if the labels are drawn before the character is created
 *  So that is filled after a character is created.
 */
void MainWindow::win_char_info_basic_create()
{
    char_info_basic_settings.make_extra_window();

    char_info_basic_settings.main_widget->setWindowTitle("Character Information - Basic");
    connect(char_info_basic_settings.win_font_act, SIGNAL(triggered()), this, SLOT(win_char_info_basic_font()));

    connect(char_info_basic_settings.main_widget, SIGNAL(destroyed(QObject*)), this, SLOT(win_char_info_basic_destroy(QObject*)));
}

void MainWindow::win_char_info_basic_destroy(QObject *this_object)
{
    (void)this_object;
    if (!char_info_basic_settings.win_show) return;
    if (!char_info_basic_settings.main_widget) return;
    char_info_basic_settings.get_widget_settings(char_info_basic_settings.main_widget);
    char_info_basic_settings.main_widget->deleteLater();
    char_info_basic_settings.win_show = FALSE;
    win_char_basic_act->setText("Show Basic Character Information");
}

/*
 * This version should only be used when the game is shutting down.
 * So it is remembered if the window was open or not.
 * For closing the window mid-game use win_char_info_basic_destroy directly
 */
void MainWindow::win_char_info_basic_close()
{
    bool was_open = char_info_basic_settings.win_show;
    win_char_info_basic_destroy(char_info_basic_settings.main_widget);
    char_info_basic_settings.win_show = was_open;
}

void MainWindow::toggle_win_char_basic_frame()
{
    if (!char_info_basic_settings.win_show)
    {
        win_char_info_basic_create();
        char_info_basic_settings.win_show = TRUE;
        create_win_char_info();
        char_info_basic_settings.main_widget->setGeometry(char_info_basic_settings.win_geometry);
        win_char_basic_act->setText("Hide Basic Character Information");
        if (char_info_basic_settings.win_maximized) char_info_basic_settings.main_widget->showMaximized();
        else char_info_basic_settings.main_widget->show();
    }
    else win_char_info_basic_destroy(char_info_basic_settings.main_widget);
}

