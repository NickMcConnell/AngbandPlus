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
#include <QHeaderView>
#include <QFontDialog>


void MainWindow::set_font_win_obj_recall(QFont newFont)
{
    font_win_obj_recall = newFont;
    win_obj_recall_update();
}

void MainWindow::win_obj_recall_font()
{
    bool selected;
    QFont font = QFontDialog::getFont(&selected, font_win_obj_recall, this );

    if (selected)
    {
        set_font_win_obj_recall(font);
    }
}

// For when savefiles close but the game doesn't.
void MainWindow::win_obj_recall_wipe()
{
    if (!show_obj_recall) return;
    if (!character_generated) return;

    obj_recall_area->clear();

}

void MainWindow::win_obj_recall_update()
{
    win_obj_recall_wipe();
    if (!show_obj_recall) return;
    if (!character_generated) return;
    if (!p_ptr->object_kind_idx && !p_ptr->object_idx) return;

    obj_recall_area->setFont(font_win_obj_recall);
    obj_recall_area->moveCursor(QTextCursor::Start);

    QString obj_recall;

    if (p_ptr->object_idx)
    {
        object_type *o_ptr = object_from_item_idx(p_ptr->object_idx);
        obj_recall = get_object_description(o_ptr);
    }
    else // p_ptr->object_kind_idx
    {
        s16b k_idx = p_ptr->object_kind_idx;

        // Initialize and prepare a fake object
        object_type object;
        object_type *o_ptr = &object;
        o_ptr->object_wipe();
        object_prep(o_ptr, k_idx);

        if (k_info[k_idx].aware) o_ptr->ident |= (IDENT_STORE);

        /* draw it */
        obj_recall = get_object_description(o_ptr);
    }


    obj_recall_area->insertHtml(obj_recall);
}

void MainWindow::close_win_obj_recall(QObject *this_object)
{
    (void)this_object;
    window_obj_recall = NULL;
    show_obj_recall = FALSE;
    win_obj_recall->setText("Show Object Recall Window");
}

/*
 *  Show widget is called after this to allow
 * the settings to restore the save geometry.
 */
void MainWindow::win_obj_recall_create()
{
    window_obj_recall = new QWidget();
    obj_recall_vlay = new QVBoxLayout;
    window_obj_recall->setLayout(obj_recall_vlay);
    obj_recall_area = new QTextEdit;
    obj_recall_area->setReadOnly(TRUE);
    obj_recall_area->setStyleSheet("background-color: lightGray;");
    obj_recall_area->setTextInteractionFlags(Qt::NoTextInteraction);
    obj_recall_vlay->addWidget(obj_recall_area);
    obj_recall_menubar = new QMenuBar;
    obj_recall_vlay->setMenuBar(obj_recall_menubar);
    window_obj_recall->setWindowTitle("Object Recall Window");
    obj_recall_win_settings = obj_recall_menubar->addMenu(tr("&Settings"));
    obj_recall_set_font = new QAction(tr("Set Object Recall Font"), this);
    obj_recall_set_font->setStatusTip(tr("Set the font for the Object Recall Window."));
    connect(obj_recall_set_font, SIGNAL(triggered()), this, SLOT(win_obj_recall_font()));
    obj_recall_win_settings->addAction(obj_recall_set_font);

    window_obj_recall->setAttribute(Qt::WA_DeleteOnClose);
    connect(window_obj_recall, SIGNAL(destroyed(QObject*)), this, SLOT(close_win_obj_recall(QObject*)));
}


void MainWindow::win_obj_recall_destroy()
{
    if (!show_obj_recall) return;
    delete window_obj_recall;
    window_obj_recall = NULL;
}

void MainWindow::toggle_win_obj_recall()
{
    if (!show_obj_recall)
    {
        win_obj_recall_create();
        show_obj_recall = TRUE;
        win_obj_recall->setText("Hide Object Recall Window");
        window_obj_recall->show();
        win_obj_recall_update();
    }
    else
    {
        win_obj_recall_destroy();
        show_obj_recall = FALSE;
        win_obj_recall->setText("Show Object Recall Window");
    }
}

