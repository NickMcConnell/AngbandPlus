/*
 * File: qt_statusbar.cpp
 *
 * Copyright (c) 2015  Jeff Greene, Diego Gonzalez
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

#include <src/qt_mainwindow.h>
#include <src/npp.h>
#include <src/player_command.h>
#include <QToolBar>

void MainWindow::slot_targeting_button()
{
    if (ui_mode != UI_MODE_INPUT || !ev_loop.isRunning()) return;

    QObject *snd = QObject::sender();
    input.text = snd->objectName();
    input.key = snd->property("key").toInt();
    input.mode = INPUT_MODE_KEY;
    ui_mode = UI_MODE_DEFAULT;
    ev_loop.quit();
}

void MainWindow::create_targetbar(void)
{
    target_toolbar = new QToolBar;
    target_toolbar->setObjectName("Target Bar");
    addToolBar(Qt::BottomToolBarArea, target_toolbar);
    target_toolbar->setVisible(false);

    QSize tool_size = target_toolbar->iconSize();
    tool_size.setHeight(tool_size.height()*3/2);
    tool_size.setWidth(tool_size.width()*3/2);
    target_toolbar->setIconSize(tool_size);

    escape = new QAction(tr("Cancel Targeting"), this);
    escape->setIcon(QIcon(":icons/lib/icons/target-cancel.png"));
    escape->setToolTip("ESC - Cancel Targeting.");
    escape->setProperty("key", Qt::Key_Escape);
    target_toolbar->addAction(escape);
    connect(escape, SIGNAL(triggered()), this, SLOT(slot_targeting_button()));

    use_current = new QAction(tr("Select Target"), this);
    use_current->setIcon(QIcon(":icons/lib/icons/target.png"));
    use_current->setToolTip("5 - Select This Target.");
    use_current->setProperty("key", Qt::Key_5);
    target_toolbar->addAction(use_current);
    connect(use_current, SIGNAL(triggered()), this, SLOT(slot_targeting_button()));

    target_closest = new QAction(tr("Target Closest"), this);
    target_closest->setIcon(QIcon(":icons/lib/icons/target-closest.png"));
    target_closest->setToolTip("C - Select the closest target.");
    target_closest->setProperty("key", Qt::Key_C);
    target_toolbar->addAction(target_closest);
    connect(target_closest, SIGNAL(triggered()), this, SLOT(slot_targeting_button()));

    target_interactive = new QAction(tr("Interactive targeting mode"), this);
    target_interactive->setIcon(QIcon(":icons/lib/icons/target-interactive.png"));
    target_interactive->setToolTip("* - Select the target interactively.");
    target_interactive->setProperty("key", Qt::Key_Asterisk);
    target_toolbar->addAction(target_interactive);
    connect(target_interactive, SIGNAL(triggered()), this, SLOT(slot_targeting_button()));

    target_manually = new QAction(tr("Manual targeting mode"), this);
    target_manually->setIcon(QIcon(":icons/lib/icons/target-manual.png"));
    target_manually->setToolTip("* m - Select the target manually.");
    target_manually->setProperty("key", Qt::Key_M);
    target_toolbar->addAction(target_manually);
    connect(target_manually, SIGNAL(triggered()), this, SLOT(slot_targeting_button()));

    target_player = new QAction(tr("Target the player"), this);
    target_player->setIcon(QIcon(":icons/lib/icons/target-player.png"));
    target_player->setToolTip("p - Target the player location.");
    target_player->setProperty("key", Qt::Key_P);
    target_toolbar->addAction(target_player);
    connect(target_player, SIGNAL(triggered()), this, SLOT(slot_targeting_button()));

    view_grid_contents = new QAction(tr("View Grid Contents"), this);
    view_grid_contents->setIcon(QIcon(":icons/lib/icons/help.png"));
    view_grid_contents->setToolTip("L = View the contents of the grid.");
    view_grid_contents->setProperty("key", Qt::Key_L);
    target_toolbar->addAction(view_grid_contents);
    connect(view_grid_contents, SIGNAL(triggered()), this, SLOT(slot_targeting_button()));

    target_help = new QAction(tr("View Grid Contents"), this);
    target_help->setIcon(QIcon(":icons/lib/icons/help.png"));
    target_help->setToolTip("L = View the contents of the grid.");
    target_help->setProperty("key", Qt::Key_Question);
    target_toolbar->addAction(target_help);
    connect(target_help, SIGNAL(triggered()), this, SLOT(slot_targeting_button()));

    ui_toolbar_hide(TOOLBAR_TARGETING);
}

void MainWindow::update_targetbar(int toolbar)
{
    target_toolbar->setVisible(TRUE);

    switch (toolbar)
    {
        case TOOLBAR_TARGETING:
        {
            target_closest->setVisible(TRUE);
            target_interactive->setVisible(TRUE);
            target_manually->setVisible(FALSE);
            target_player->setVisible(FALSE);
            view_grid_contents->setVisible(FALSE);
            break;
        }
        case TOOLBAR_TARGETING_INTERACTIVE:
        {
            target_closest->setVisible(FALSE);
            target_interactive->setVisible(FALSE);
            target_manually->setVisible(TRUE);
            target_player->setVisible(TRUE);
            view_grid_contents->setVisible(TRUE);
            break;
        }
        default: break;
    }
}



void ui_toolbar_show(int toolbar)
{
    main_window->update_targetbar(toolbar);
}

void ui_toolbar_hide(int toolbar)
{
    (void)toolbar;

    main_window->target_toolbar->setVisible(FALSE);

}
