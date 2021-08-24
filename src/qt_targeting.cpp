/*
 * File: qt_statusbar.cpp
 *
 * Copyright (c) 2015  Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/qt_mainwindow.h>
#include <src/npp.h>
#include <src/player_command.h>
#include <QPushButton>
#include <QLayoutItem>

void MainWindow::set_input_key(int this_key)
{
    input.key = this_key;
    input.text.clear();
    input.mode = INPUT_MODE_KEY;
    ev_loop.quit();
}


// Translates the targeting pushbuttons to target commands
void MainWindow::target_choice()
{
    QString this_text = QObject::sender()->objectName();

    if (strings_match(this_text, "TargetCancel"))
    {
        set_input_key(Qt::Key_Escape);
        return;
    }

    if (strings_match(this_text, "TargetClosest") && (targeting_mode != MODE_TARGETING_DIRECTION))
    {
        set_input_key(Qt::Key_C);
        return;
    }

    if (strings_match(this_text, "TargetPlayer"))
    {
        set_input_key(Qt::Key_Ampersand);
        return;
    }

    if (strings_match(this_text, "TargetInfo"))
    {
        set_input_key(Qt::Key_L);
        return;
    }

    if (strings_match(this_text, "TargetSelect"))
    {
        set_input_key(Qt::Key_H);
        return;
    }

    if (strings_match(this_text, "NorthWest"))
    {
        set_input_key(Qt::Key_7);
        return;
    }
    if (strings_match(this_text, "North"))
    {
        set_input_key(Qt::Key_8);
        return;
    }
    if (strings_match(this_text, "NorthEast"))
    {
        set_input_key(Qt::Key_9);
        return;
    }
    if (strings_match(this_text, "West"))
    {
        set_input_key(Qt::Key_4);
        return;
    }
    if (strings_match(this_text, "East"))
    {
        set_input_key(Qt::Key_6);
        return;
    }
    if (strings_match(this_text, "SouthWest"))
    {
        set_input_key(Qt::Key_1);
        return;
    }
    if (strings_match(this_text, "South"))
    {
        set_input_key(Qt::Key_2);
        return;
    }
    if (strings_match(this_text, "SouthEast"))
    {
        set_input_key(Qt::Key_3);
        return;
    }

    if (strings_match(this_text, "TargetToggle"))
    {
        set_input_key(Qt::Key_M);
        return;
    }

    if (strings_match(this_text, "TargetGridInfo"))
    {
        set_input_key(Qt::Key_L);
        return;
    }

    if (targeting_mode ==  MODE_TARGETING_INTERACTIVE)
    {
        if (strings_match(this_text, "TargetPrev"))
        {
            set_input_key(Qt::Key_Plus);
            return;
        }
        else if (strings_match(this_text, "TargetNext"))
        {
            set_input_key(Qt::Key_Minus);
            return;
        }
    }
}

// Hide the targeting toolbar
void MainWindow::hide_targeting_sidebar()
{
    QList<QPushButton *> pushbuttons = sidebar_widget->findChildren<QPushButton *>();

    QFontMetrics metrics(font_sidebar_window);

    for (int i = 0; i < pushbuttons.size(); i++)
    {
        QPointer<QPushButton> this_pushbutton = pushbuttons.at(i);

        QString this_text = this_pushbutton->objectName();
        if (!this_text.length()) continue;

        this_pushbutton->setMaximumWidth(metrics.width("M") * 4);

        if (strings_match(this_text, "NorthWest")) this_pushbutton->hide();
        else if (strings_match(this_text, "North")) this_pushbutton->hide();
        else if (strings_match(this_text, "NorthEast")) this_pushbutton->hide();
        else if (strings_match(this_text, "West")) this_pushbutton->hide();
        else if (strings_match(this_text, "TargetPlayer")) this_pushbutton->hide();
        else if (strings_match(this_text, "East")) this_pushbutton->hide();
        else if (strings_match(this_text, "SouthWest")) this_pushbutton->hide();
        else if (strings_match(this_text, "South")) this_pushbutton->hide();
        else if (strings_match(this_text, "SouthEast")) this_pushbutton->hide();
        else if (strings_match(this_text, "TargetClosest")) this_pushbutton->hide();
        else if (strings_match(this_text, "TargetToggle")) this_pushbutton->hide();
        else if (strings_match(this_text, "TargetGridInfo")) this_pushbutton->hide();
        else if (strings_match(this_text, "TargetCancel")) this_pushbutton->hide();
        else if (strings_match(this_text, "TargetSelect")) this_pushbutton->hide();
        else if (strings_match(this_text, "TargetHelp")) this_pushbutton->hide();
        else if (strings_match(this_text, "TargetNext")) this_pushbutton->hide();
        else if (strings_match(this_text, "TargetPrev")) this_pushbutton->hide();
        else if (strings_match(this_text, "TargetInfo")) this_pushbutton->hide();
    }
}

// Show the targeting toolbar
void MainWindow::show_targeting_sidebar()
{
    // Paranoia
    if ((targeting_mode == MODE_NO_TARGETING) || !show_targeting_buttons)
    {
        hide_targeting_sidebar();
        return;
    }

    QList<QPushButton *> pushbuttons = sidebar_widget->findChildren<QPushButton *>();
    if (targeting_mode ==  MODE_TARGETING_INTERACTIVE)for (int i = 0; i < pushbuttons.size(); i++)
    {
        QPushButton *this_pushbutton = pushbuttons.at(i);

        QString this_text = this_pushbutton->objectName();

        if (strings_match(this_text, "NorthWest"))
        {
            this_pushbutton->setToolTip("Target Direction NorthWest");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "North"))
        {
            this_pushbutton->setToolTip("Target Direction North");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "NorthEast"))
        {
            this_pushbutton->setToolTip("Target Direction NorthEast");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "West"))
        {
            this_pushbutton->setToolTip("Target Direction West");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetPlayer")) this_pushbutton->hide();
        else if (strings_match(this_text, "East"))
        {
            this_pushbutton->setToolTip("Target Direction East");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "SouthWest"))
        {
            this_pushbutton->setToolTip("Target Direction SouthWest");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "South"))
        {
            this_pushbutton->setToolTip("Target Direction South");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "SouthEast"))
        {
            this_pushbutton->setToolTip("Target Direction SouthEast");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetClosest")) this_pushbutton->show();
        else if (strings_match(this_text, "TargetToggle"))
        {
            this_pushbutton->setEnabled(TRUE);
            this_pushbutton->setToolTip("Select the Target Manually.  Currently in Interactive Targeting Mode.");
            this_pushbutton->setIcon(QIcon(":/icons/lib/icons/target-manual.png"));
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetGridInfo"))
        {
            this_pushbutton->setEnabled(TRUE);
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetCancel")) this_pushbutton->show();
        else if (strings_match(this_text, "TargetSelect")) this_pushbutton->show();
        else if (strings_match(this_text, "TargetHelp")) this_pushbutton->show();
        else if (strings_match(this_text, "TargetPrev"))
        {
            this_pushbutton->setEnabled(TRUE);
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetNext"))
        {
            this_pushbutton->setEnabled(TRUE);
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetInfo"))
        {
            this_pushbutton->setEnabled(FALSE);
            this_pushbutton->hide();
        }
    }
    else if (targeting_mode ==  MODE_TARGETING_MANUAL)for (int i = 0; i < pushbuttons.size(); i++)
    {
        QPushButton *this_pushbutton = pushbuttons.at(i);

        QString this_text = this_pushbutton->objectName();

        if (strings_match(this_text, "NorthWest"))
        {
            this_pushbutton->setToolTip("Move target 1 square to the NorthWest");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "North"))
        {
            this_pushbutton->setToolTip("Move target 1 square to the North");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "NorthEast"))
        {
            this_pushbutton->setToolTip("Move target 1 square to the NorthEast");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "West"))
        {
            this_pushbutton->setToolTip("Move target 1 square to the West");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetPlayer")) this_pushbutton->show();
        else if (strings_match(this_text, "East"))
        {
            this_pushbutton->setToolTip("Move target 1 square to the East");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "SouthWest"))
        {
            this_pushbutton->setToolTip("Move target 1 square to the SouthWest");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "South"))
        {
            this_pushbutton->setToolTip("Move target 1 square to the South");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "SouthEast"))
        {
            this_pushbutton->setToolTip("Move target 1 square to the SouthEast");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetClosest")) this_pushbutton->show();
        else if (strings_match(this_text, "TargetToggle"))
        {
            this_pushbutton->setEnabled(TRUE);
            this_pushbutton->setToolTip("Select the Target Interactively.  Currently in Manual Targeting Mode.");
            this_pushbutton->setIcon(QIcon(":/icons/lib/icons/target-interactive.png"));
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetGridInfo"))
        {
            this_pushbutton->setEnabled(TRUE);
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetCancel")) this_pushbutton->show();
        else if (strings_match(this_text, "TargetSelect")) this_pushbutton->show();
        else if (strings_match(this_text, "TargetHelp")) this_pushbutton->show();
        else if (strings_match(this_text, "TargetPrev"))
        {
            this_pushbutton->setEnabled(FALSE);
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetNext"))
        {
            this_pushbutton->setEnabled(FALSE);
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetInfo"))
        {
            this_pushbutton->setEnabled(TRUE);
            this_pushbutton->show();
        }
    }

    else if (targeting_mode ==  MODE_TARGETING_DIRECTION)for (int i = 0; i < pushbuttons.size(); i++)
    {
        QPushButton *this_pushbutton = pushbuttons.at(i);

        QString this_text = this_pushbutton->objectName();

        if (strings_match(this_text, "NorthWest"))
        {
            this_pushbutton->setToolTip("Select the direction NorthWest.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "North"))
        {
            this_pushbutton->setToolTip("Select the direction North.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "NorthEast"))
        {
            this_pushbutton->setToolTip("Select the direction NorthEast.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "West"))
        {
            this_pushbutton->setToolTip("Select the direction West.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetPlayer")) this_pushbutton->hide();
        else if (strings_match(this_text, "East"))
        {
            this_pushbutton->setToolTip("Select the direction East.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "SouthWest"))
        {
            this_pushbutton->setToolTip("Select the direction SouthWest.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "South"))
        {
            this_pushbutton->setToolTip("Select the direction South.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "SouthEast"))
        {
            this_pushbutton->setToolTip("Select the direction SouthEast.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetClosest")) this_pushbutton->hide();
        else if (strings_match(this_text, "TargetToggle"))
        {
            this_pushbutton->setEnabled(FALSE);
            this_pushbutton->hide();
        }
        else if (strings_match(this_text, "TargetGridInfo"))
        {
            this_pushbutton->setEnabled(FALSE);
            this_pushbutton->hide();
        }
        else if (strings_match(this_text, "TargetCancel")) this_pushbutton->show();
        else if (strings_match(this_text, "TargetSelect")) this_pushbutton->hide();
        else if (strings_match(this_text, "TargetHelp")) this_pushbutton->show();
        else if (strings_match(this_text, "TargetPrev"))
        {
            this_pushbutton->setEnabled(FALSE);
            this_pushbutton->hide();
        }
        else if (strings_match(this_text, "TargetNext"))
        {
            this_pushbutton->setEnabled(FALSE);
            this_pushbutton->hide();
        }
        else if (strings_match(this_text, "TargetInfo"))
        {
            this_pushbutton->setEnabled(FALSE);
            this_pushbutton->hide();
        }
    }
    else if (targeting_mode ==  MODE_TARGETING_AIMING)for (int i = 0; i < pushbuttons.size(); i++)
    {
        QPushButton *this_pushbutton = pushbuttons.at(i);

        QString this_text = this_pushbutton->objectName();

        if (strings_match(this_text, "NorthWest"))
        {
            this_pushbutton->setToolTip("Select the direction NorthWest.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "North"))
        {
            this_pushbutton->setToolTip("Select the direction North.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "NorthEast"))
        {
            this_pushbutton->setToolTip("Select the direction NorthEast.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "West"))
        {
            this_pushbutton->setToolTip("Select the direction West.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetPlayer")) this_pushbutton->hide();
        else if (strings_match(this_text, "East"))
        {
            this_pushbutton->setToolTip("Select the direction East.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "SouthWest"))
        {
            this_pushbutton->setToolTip("Select the direction SouthWest.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "South"))
        {
            this_pushbutton->setToolTip("Select the direction South.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "SouthEast"))
        {
            this_pushbutton->setToolTip("Select the direction SouthEast.");
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetClosest")) this_pushbutton->show();
        else if (strings_match(this_text, "TargetToggle"))
        {
            this_pushbutton->setToolTip("Select The Target Interactively");
            this_pushbutton->setEnabled(TRUE);
            this_pushbutton->show();
        }
        else if (strings_match(this_text, "TargetGridInfo"))
        {
            this_pushbutton->setEnabled(FALSE);
            this_pushbutton->hide();
        }
        else if (strings_match(this_text, "TargetCancel")) this_pushbutton->show();
        else if (strings_match(this_text, "TargetSelect")) this_pushbutton->hide();
        else if (strings_match(this_text, "TargetHelp")) this_pushbutton->show();
        else if (strings_match(this_text, "TargetPrev"))
        {
            this_pushbutton->setEnabled(FALSE);
            this_pushbutton->hide();
        }
        else if (strings_match(this_text, "TargetNext"))
        {
            this_pushbutton->setEnabled(FALSE);
            this_pushbutton->hide();
        }
        else if (strings_match(this_text, "TargetInfo"))
        {
            this_pushbutton->setEnabled(FALSE);
            this_pushbutton->hide();
        }
    }
}

void MainWindow::create_targeting_sidebar()
{
    QPointer<QPushButton> north_west = new QPushButton;
    north_west->setIcon(QIcon(":/icons/lib/icons/arrow-northwest.png"));
    north_west->setObjectName("NorthWest");
    connect(north_west, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(north_west, 0, 0);

    QPointer<QPushButton> north = new QPushButton;
    north->setIcon(QIcon(":/icons/lib/icons/arrow-north.png"));
    north->setObjectName("North");
    connect(north, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(north, 0, 1);

    QPointer<QPushButton> north_east = new QPushButton;
    north_east->setIcon(QIcon(":/icons/lib/icons/arrow-northeast.png"));
    north_east->setToolTip("Target Direction NorthEast");
    north_east->setObjectName("NorthEast");
    connect(north_east, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(north_east, 0, 2);

    QPointer<QPushButton> west = new QPushButton;
    west->setIcon(QIcon(":/icons/lib/icons/arrow-west.png"));
    west->setToolTip("Target Direction West");
    west->setObjectName("West");
    connect(west, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(west, 1, 0);

    QPointer<QPushButton> target_player = new QPushButton;
    target_player->setIcon(QIcon(":/icons/lib/icons/target-player.png"));
    target_player->setToolTip("Target Player");
    target_player->setObjectName("TargetPlayer");
    connect(target_player, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(target_player, 1, 1);

    QPointer<QPushButton> east = new QPushButton;
    east->setIcon(QIcon(":/icons/lib/icons/arrow-east.png"));
    east->setToolTip("Target Direction East");
    east->setObjectName("East");
    connect(east, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(east, 1, 2);

    QPointer<QPushButton> south_west = new QPushButton;
    south_west->setIcon(QIcon(":/icons/lib/icons/arrow-southwest.png"));
    south_west->setToolTip("Target Direction SouthWest");
    south_west->setObjectName("SouthWest");
    connect(south_west, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(south_west, 2, 0);

    QPointer<QPushButton> south = new QPushButton;
    south->setIcon(QIcon(":/icons/lib/icons/arrow-south.png"));
    south->setToolTip("Target Direction South");
    south->setObjectName("South");
    connect(south, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(south, 2, 1);

    QPointer<QPushButton> south_east = new QPushButton;
    south_east->setIcon(QIcon(":/icons/lib/icons/arrow-southeast.png"));
    south_east->setToolTip("Target Direction SouthEast");
    south_east->setObjectName("SouthEast");
    connect(south_east, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(south_east, 2, 2);

    QPointer<QPushButton> target_closest = new QPushButton;
    target_closest->setIcon(QIcon(":/icons/lib/icons/target-closest.png"));
    target_closest->setToolTip("Target Closest");
    target_closest->setObjectName("TargetClosest");
    connect(target_closest, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(target_closest, 3, 0);

    QPointer<QPushButton> target_toggle = new QPushButton;
    target_toggle->setIcon(QIcon(":/icons/lib/icons/target-interactive.png"));
    target_toggle->setObjectName("TargetToggle");
    connect(target_toggle, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(target_toggle, 3, 1);

    QPointer<QPushButton> target_grid_info = new QPushButton;
    target_grid_info->setIcon(QIcon(":/icons/lib/icons/target-information.png"));
    target_grid_info->setObjectName("TargetGridInfo");
    target_grid_info->setToolTip("Display Information on Current Targeted Grid");
    connect(target_grid_info, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(target_grid_info, 3, 2);

    QPointer<QPushButton> target_cancel = new QPushButton;
    target_cancel->setIcon(QIcon(":/icons/lib/icons/target-cancel.png"));
    target_cancel->setToolTip("Cancel Targeting");
    target_cancel->setObjectName("TargetCancel");
    connect(target_cancel, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(target_cancel, 4, 0);

    QPointer<QPushButton> target_select = new QPushButton;
    target_select->setIcon(QIcon(":/icons/lib/icons/select.png"));
    target_select->setToolTip("Select Current Target");
    target_select->setObjectName("TargetSelect");
    connect(target_select, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(target_select, 4, 1);

    QPointer<QPushButton> target_help = new QPushButton;
    target_help->setIcon(QIcon(":/icons/lib/icons/target-help.png"));
    target_help->setToolTip("View Targeting Help");
    target_help->setObjectName("TargetHelp");
    connect(target_help, SIGNAL(pressed()), this, SLOT(command_list_targeting()));
    targeting_glay->addWidget(target_help, 4, 2);

    QPointer<QPushButton> target_prev = new QPushButton;
    target_prev->setIcon(QIcon(":/icons/lib/icons/arrow-left-double.png"));
    target_prev->setToolTip("Select next target");
    target_prev->setObjectName("TargetPrev");
    connect(target_prev, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(target_prev, 5, 0);

    QPointer<QPushButton> target_next = new QPushButton;
    target_next->setIcon(QIcon(":/icons/lib/icons/arrow-right-double.png"));
    target_next->setToolTip("Select the next target.");
    target_next->setObjectName("TargetNext");
    connect(target_next, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(target_next, 5, 1);

    QPointer<QPushButton> target_info = new QPushButton;
    target_info->setIcon(QIcon(":/icons/lib/icons/target-information.png"));
    target_info->setToolTip("Receive information on the selected square.");
    target_info->setObjectName("TargetInfo");
    connect(target_info, SIGNAL(pressed()), this, SLOT(target_choice()));
    targeting_glay->addWidget(target_info, 5, 2);

    hide_targeting_sidebar();

}


void ui_targeting_show(int mode)
{
    main_window->targeting_mode = mode;

    main_window->show_targeting_sidebar();
}

void ui_targeting_hide(void)
{
    main_window->targeting_mode= MODE_NO_TARGETING;

    main_window->hide_targeting_sidebar();
}
