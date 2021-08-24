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



void MainWindow::update_label_equip_info_font()
{
    QList<QLabel *> lbl_list = char_info_equip_settings.main_widget->findChildren<QLabel *>();
    for (int i = 0; i < lbl_list.size(); i++)
    {
        QLabel *this_lbl = lbl_list.at(i);
        this_lbl->setFont(char_info_equip_settings.win_font);
    }
}

void MainWindow::set_font_char_info_equip(QFont newFont)
{
    char_info_equip_settings.win_font = newFont;
    update_label_equip_info_font();

}

void MainWindow::win_char_info_equip_font()
{
    bool selected;
    QFont font = QFontDialog::getFont( &selected, char_info_equip_settings.win_font, this);

    if (selected)
    {
        set_font_char_info_equip(font);
    }
}

// For when savefiles close but the game doesn't.
void MainWindow::win_char_info_equip_wipe()
{
    if (!char_info_equip_settings.win_show) return;
    if (!character_generated) return;
    clear_layout(char_info_equip_settings.main_vlay);
}


void MainWindow::win_char_info_equip_update()
{
    if (!character_generated) return;
    if (!char_info_equip_settings.win_show) return;
    update_equip_flags(list_resist_equippy, list_resist_flags, list_resist_labels);
    update_equip_flags(list_ability_equippy, list_ability_flags, list_ability_labels);
    update_equip_flags(list_nativity_equippy, list_nativity_flags, list_nativity_labels);
    update_equip_modifiers(list_equip_equippy, list_equip_flags, list_equip_labels);
}

// The update function above assumes these lists have been made correctly
// This is necessary for efficiency.  The game will likely crash if
// these lists are made incorrectly.
void MainWindow::update_win_char_equip_set_lists()
{
    list_resist_flags.clear();
    list_ability_flags.clear();
    list_equip_flags.clear();
    list_nativity_flags.clear();
    list_resist_labels.clear();
    list_ability_labels.clear();
    list_equip_labels.clear();
    list_nativity_labels.clear();
    list_resist_equippy.clear();
    list_ability_equippy.clear();
    list_equip_equippy.clear();
    list_nativity_equippy.clear();

    QList<QLabel *> lbl_list = resist_widget->findChildren<QLabel *>();
    for (int x = 0; x < lbl_list.size(); x++)
    {
        QLabel *this_lbl = lbl_list.at(x);

        QString this_name = this_lbl->objectName();

        if (this_name.contains("equippy"))      list_resist_equippy.append(this_lbl);
        if (this_name.contains("obj_flag_info"))list_resist_flags.append(this_lbl);
        if (this_name.contains("line_label"))   list_resist_labels.append(this_lbl);
    }

    lbl_list = ability_widget->findChildren<QLabel *>();
    for (int x = 0; x < lbl_list.size(); x++)
    {
        QLabel *this_lbl = lbl_list.at(x);

        QString this_name = this_lbl->objectName();

        if (this_name.contains("equippy"))      list_ability_equippy.append(this_lbl);
        if (this_name.contains("obj_flag_info"))list_ability_flags.append(this_lbl);
        if (this_name.contains("line_label"))   list_ability_labels.append(this_lbl);
    }

    lbl_list = equip_widget->findChildren<QLabel *>();
    for (int x = 0; x < lbl_list.size(); x++)
    {
        QLabel *this_lbl = lbl_list.at(x);

        QString this_name = this_lbl->objectName();

        if (this_name.contains("equippy"))      list_equip_equippy.append(this_lbl);
        if (this_name.contains("obj_mod_info")) list_equip_flags.append(this_lbl);
        if (this_name.contains("line_label"))   list_equip_labels.append(this_lbl);
    }

    lbl_list = nativity_widget->findChildren<QLabel *>();
    for (int x = 0; x < lbl_list.size(); x++)
    {
        QLabel *this_lbl = lbl_list.at(x);

        QString this_name = this_lbl->objectName();

        if (this_name.contains("equippy"))      list_nativity_equippy.append(this_lbl);
        if (this_name.contains("obj_flag_info"))list_nativity_flags.append(this_lbl);
        if (this_name.contains("line_label"))   list_nativity_labels.append(this_lbl);
    }
}

void MainWindow::create_win_char_equip_info()
{
    if (!character_generated) return;
    if (!char_info_equip_settings.win_show) return;

    // Object Info
    QPointer<QGridLayout> equip_info = new QGridLayout;
    char_info_equip_settings.main_vlay->addLayout(equip_info);

    QPointer<QVBoxLayout> resist_vlay = new QVBoxLayout;
    QPointer<QVBoxLayout> ability_vlay = new QVBoxLayout;
    QPointer<QVBoxLayout> equip_vlay = new QVBoxLayout;
    QPointer<QVBoxLayout> nativity_vlay = new QVBoxLayout;
    resist_flags = new QGridLayout;
    ability_flags = new QGridLayout;
    equip_mods = new QGridLayout;
    nativity_flags = new QGridLayout;
    resist_widget = new QWidget;
    ability_widget = new QWidget;
    equip_widget = new QWidget;
    nativity_widget = new QWidget;
    resist_vlay->addWidget(resist_widget);
    ability_vlay->addWidget(ability_widget);
    equip_vlay->addWidget(equip_widget);
    nativity_vlay->addWidget(nativity_widget);
    resist_widget->setLayout(resist_flags);
    ability_widget->setLayout(ability_flags);
    equip_widget->setLayout(equip_mods);
    nativity_widget->setLayout(nativity_flags);
    resist_vlay->addStretch(1);
    ability_vlay->addStretch(1);
    equip_vlay->addStretch(1);
    nativity_vlay->addStretch(1);

    QPointer<QLabel> resist_label = new QLabel("<h3>Resistance Information</h3>");
    resist_label->setToolTip(QString("Blue represents elemental immunity, green represents resistance, and purple represents double resistance."));
    equip_info->addWidget(resist_label, 0, 0, Qt::AlignCenter);
    equip_info->addLayout(resist_vlay, 1, 0);

    QPointer<QLabel> ability_label = new QLabel("<h3>Ability Information</h3>");
    equip_info->addWidget(ability_label, 0, 1, Qt::AlignCenter);
    equip_info->addLayout(ability_vlay,  1, 1);

    QPointer<QLabel> nativity_label = new QLabel("<h3>Nativity Information</h3>");
    equip_info->addWidget(nativity_label, 0, 2, Qt::AlignCenter);
    equip_info->addLayout(nativity_vlay, 1, 2);

    QPointer<QLabel> modifier_label = new QLabel("<h3>Equipment Modifiers</h3>");
    equip_info->addWidget(modifier_label, 0, 3, Qt::AlignCenter);
    equip_info->addLayout(equip_vlay, 1, 3);

    equip_flag_info(resist_widget, resist_flags, FLAGS_RESIST, char_info_equip_settings.win_font);
    equip_flag_info(ability_widget, ability_flags, FLAGS_ABILITY, char_info_equip_settings.win_font);
    equip_flag_info(nativity_widget, nativity_flags, FLAGS_NATIVITY, char_info_equip_settings.win_font);
    equip_modifier_info(equip_widget, equip_mods, char_info_equip_settings.win_font);
    update_win_char_equip_set_lists();
}

/*
 *  Make the equip shell
 *  The game crashes if the labels are drawn before the character is created
 *  So that is filled after a character is created.
 */
void MainWindow::win_char_info_equip_create()
{
    char_info_equip_settings.make_extra_window();

    char_info_equip_settings.main_widget->setWindowTitle("Character Equipment Information");

    connect(char_info_equip_settings.win_font_act, SIGNAL(triggered()), this, SLOT(win_char_info_equip_font()));

    connect(char_info_equip_settings.main_widget, SIGNAL(destroyed(QObject*)), this, SLOT(win_char_info_equip_destroy(QObject*)));
}

/*
 * win_char_equip_info_close should be used when the game is shutting down.
 * Use this function for closing the window mid-game
 */
void MainWindow::win_char_info_equip_destroy(QObject *this_object)
{
    (void)this_object;
    if (!char_info_equip_settings.win_show) return;
    if (!char_info_equip_settings.main_widget) return;
    char_info_equip_settings.get_widget_settings(char_info_equip_settings.main_widget);
    char_info_equip_settings.main_widget->deleteLater();
    char_info_equip_settings.win_show = FALSE;
    win_char_equip_info_act->setText("Show Character Equipment Information");

    list_resist_flags.clear();
    list_ability_flags.clear();
    list_equip_flags.clear();
    list_nativity_flags.clear();
    list_resist_labels.clear();
    list_ability_labels.clear();
    list_equip_labels.clear();
    list_nativity_labels.clear();
    list_resist_equippy.clear();
    list_ability_equippy.clear();
    list_equip_equippy.clear();
    list_nativity_equippy.clear();
}

/*
 * This version should only be used when the game is shutting down.
 * So it is remembered if the window was open or not.
 * For closing the window mid-game use win_char_info_equip_destroy directly
 */
void MainWindow::win_char_info_equip_close()
{
    bool was_open = char_info_equip_settings.win_show;
    win_char_info_equip_destroy(char_info_equip_settings.main_widget);
    char_info_equip_settings.win_show = was_open;
}

void MainWindow::toggle_win_char_equip_frame()
{
    if (!char_info_equip_settings.win_show)
    {
        win_char_info_equip_create();
        char_info_equip_settings.win_show = TRUE;
        create_win_char_equip_info();
        char_info_equip_settings.main_widget->setGeometry(char_info_equip_settings.win_geometry);
        win_char_equip_info_act->setText("Hide Character Equipment Information");
        if (char_info_equip_settings.win_maximized) char_info_equip_settings.main_widget->showMaximized();
        else char_info_equip_settings.main_widget->show();
    }
    else win_char_info_equip_destroy(char_info_equip_settings.main_widget);
}



