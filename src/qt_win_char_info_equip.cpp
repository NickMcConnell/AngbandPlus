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
    QList<QLabel *> lbl_list = window_char_info_equip->findChildren<QLabel *>();
    for (int i = 0; i < lbl_list.size(); i++)
    {
        QLabel *this_lbl = lbl_list.at(i);
        this_lbl->setFont(font_char_equip_info);
    }
}

void MainWindow::set_font_char_info_equip(QFont newFont)
{
    font_char_equip_info = newFont;
    update_label_equip_info_font();

}

void MainWindow::win_char_info_equip_font()
{
    bool selected;
    QFont font = QFontDialog::getFont( &selected, font_char_equip_info, this);

    if (selected)
    {
        set_font_char_info_equip(font);
    }
}

// For when savefiles close but the game doesn't.
void MainWindow::win_char_info_equip_wipe()
{
    if (!show_char_info_equip) return;
    if (!character_generated) return;
    clear_layout(main_vlay_char_equip_info);
}


void MainWindow::win_char_info_equip_update()
{
    if (!character_generated) return;
    if (!show_char_info_equip) return;
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
    if (!show_char_info_equip) return;

    // Object Info
    QGridLayout *equip_info = new QGridLayout;
    main_vlay_char_equip_info->addLayout(equip_info);

    QVBoxLayout *resist_vlay = new QVBoxLayout;
    QVBoxLayout *ability_vlay = new QVBoxLayout;
    QVBoxLayout *equip_vlay = new QVBoxLayout;
    QVBoxLayout *nativity_vlay = new QVBoxLayout;
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

    QLabel *resist_label = new QLabel("<h3>Resistance Information</h3>");
    resist_label->setToolTip(QString("Blue represents elemental immunity, green represents resistance, and purple represents double resistance."));
    equip_info->addWidget(resist_label, 0, 0, Qt::AlignCenter);
    equip_info->addLayout(resist_vlay, 1, 0);

    QLabel *ability_label = new QLabel("<h3>Ability Information</h3>");
    equip_info->addWidget(ability_label, 0, 1, Qt::AlignCenter);
    equip_info->addLayout(ability_vlay,  1, 1);

    QLabel *nativity_label = new QLabel("<h3>Nativity Information</h3>");
    equip_info->addWidget(nativity_label, 0, 2, Qt::AlignCenter);
    equip_info->addLayout(nativity_vlay, 1, 2);

    QLabel *modifier_label = new QLabel("<h3>Equipment Modifiers</h3>");
    equip_info->addWidget(modifier_label, 0, 3, Qt::AlignCenter);
    equip_info->addLayout(equip_vlay, 1, 3);

    equip_flag_info(resist_widget, resist_flags, FLAGS_RESIST, font_char_equip_info);
    equip_flag_info(ability_widget, ability_flags, FLAGS_ABILITY, font_char_equip_info);
    equip_flag_info(nativity_widget, nativity_flags, FLAGS_NATIVITY, font_char_equip_info);
    equip_modifier_info(equip_widget, equip_mods, font_char_equip_info);
    update_win_char_equip_set_lists();
}

void MainWindow::close_win_char_equip_frame(QObject *this_object)
{
    (void)this_object;
    window_char_info_equip = NULL;
    show_char_info_equip = FALSE;
    win_char_equip_info->setText("Show Character Equipment Information");
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
 *  Make the equip shell
 *  The game crashes if the labels are drawn before the character is created
 *  So that is filled after a character is created.
 */
void MainWindow::win_char_info_equip_create()
{
    window_char_info_equip = new QWidget();
    main_vlay_char_equip_info = new QVBoxLayout;
    window_char_info_equip->setLayout(main_vlay_char_equip_info);

    char_info_equip_menubar = new QMenuBar;
    main_vlay_char_equip_info->setMenuBar(char_info_equip_menubar);
    window_char_info_equip->setWindowTitle("Character Equipment Information");
    char_info_equip_settings = char_info_equip_menubar->addMenu(tr("&Settings"));
    char_info_equip_font = new QAction(tr("Set Basic Character Screen Font"), this);
    char_info_equip_font->setStatusTip(tr("Set the font for the Basic Character Information screen."));
    connect(char_info_equip_font, SIGNAL(triggered()), this, SLOT(win_char_info_equip_font()));
    char_info_equip_settings->addAction(char_info_equip_font);

    window_char_info_equip->setAttribute(Qt::WA_DeleteOnClose);
    connect(window_char_info_equip, SIGNAL(destroyed(QObject*)), this, SLOT(close_win_char_equip_frame(QObject*)));
}

void MainWindow::win_char_info_equip_destroy()
{
    if (!show_char_info_equip) return;
    if (!window_char_info_equip) return;
    delete window_char_info_equip;
    window_char_info_equip = NULL;
}

void MainWindow::toggle_win_char_equip_frame()
{
    if (!show_char_info_equip)
    {
        win_char_info_equip_create();
        show_char_info_equip = TRUE;
        create_win_char_equip_info();
        win_char_equip_info->setText("Hide Character Equipment Information");
        window_char_info_equip->show();
    }
    else

    {
        win_char_info_equip_destroy();
        show_char_info_equip = FALSE;
        win_char_equip_info->setText("Show Character Equipment Information");
    }
}



